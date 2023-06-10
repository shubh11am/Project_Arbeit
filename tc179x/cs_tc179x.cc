/*

   This source file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Compiler Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2005 - 2022

   Hamburg University of Technology (TUHH)
   Institute of Embedded Systems
   21071 Hamburg
   Germany

   http://www.tuhh.de/es/esd/research/wcc

*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wcc.h>
#endif

// Include standard headers
#include <cassert>
#include <stack>

// Include boost headers
#include <boost/current_function.hpp>

// Include WIR headers
#include <wir/wir.h>
#include <arch/tricore/tc131.h>

// Include ICD headers
#include <arch/PROC/archinfo.h>
#include <optimize/irtransform.h>

// Include flowfacts headers
#include <flowfacts/flowfactmanagericdctollir.h>

// Include back-annotation headers
#include <backannotation/backannotation.h>

// Include libuseful headers
#include <libuseful/debugmacros.h>
#include <libuseful/exceptions.h>
#include <libuseful/io.h>

// Include misc headers
#include <misc/misc.h>

// Include local headers
#include <codesel/codesel.h>
#include "registrar.h"
#include "cs_tc179x.h"
#include <tc179x/tpm.h>


//
// Preprocessor macros
//

#define DEBUG_CODESEL 0

#undef TRACE
#undef CHECK
#undef CHECKX
#if DEBUG_CODESEL
# include <iostream>
# include <iomanip>
# define TRACE(s) cout << "[Trace] " << s << flush
# define CHECK(v) TRACE(# v << ": " << dec << v << endl);
# define CHECKX(v) TRACE(# v << ": " << hex << v <<  endl);
#else
# define TRACE(s) do{}while(0)
# define CHECK(s) do{}while(0)
# define CHECKX(s) do{}while(0)
#endif


//
// Code section
//

using namespace std;
using namespace WIR;
using namespace IR_Debug;


//
// Global variable declaration section
//

TC13 *TC179x_wirProc = nullptr;
WIR_CompilationUnit *TC179x_wirUnit = nullptr;
WIR_Function *TC179x_wirFct = nullptr;
WIR_BasicBlock *TC179x_wirBB = nullptr;


//
// Public class methods
//

TC179x_CodeSelector::TC179x_CodeSelector( WIR_System &sys, TaskEntry &te,
                                          BackAnnotation *backannotation ) :
  CodeSelector( sys, te, backannotation ),
  mStack {},
  mInstructionFactory { *(mTE.getConfig()), *this }
{
  DSTART(
    "TC179x_CodeSelector::TC179x_CodeSelector(WIR_System&, TaskEntry&, BackAnnotation*)" );

  TC179x_wirProc = nullptr;
  TC179x_wirFct = nullptr;
  TC179x_wirBB = nullptr;

  // Preserve code selector of outer FSM.
  if ( Registrar::getCodeSelector() )
    mLastCodeSel = Registrar::getCodeSelector();
  else
    // If there are no outer FSMs, the codeselector must not be
    // preserved.
    mLastCodeSel = nullptr;
  Registrar::setCodeSelector( this );

  mTreePatternMatcher.reset( new TC179x_TreePatternMatcher );

  #ifdef HAVE_ALIAS_ANALYSIS
  mAlias = nullptr;
  #endif
};


TC179x_CodeSelector::~TC179x_CodeSelector()
{
  DSTART( "TC179x_CodeSelector::~TC179x_CodeSelector()" );

  mLLIRBB.erase( mLLIRBB.begin(), mLLIRBB.end() );

  mLLIRFunc.erase( mLLIRFunc.begin(), mLLIRFunc.end() );

  mLLIR.erase( mLLIR.begin(), mLLIR.end() );

  #ifdef HAVE_ALIAS_ANALYSIS
  if ( mAlias )
    delete mAlias;
  #endif

  // Restore code selector of outer FSM.
  Registrar::setCodeSelector( mLastCodeSel );
  mLastCodeSel = nullptr;

  TC179x_wirBB = nullptr;
  TC179x_wirFct = nullptr;
  TC179x_wirProc = nullptr;
};


void TC179x_CodeSelector::reset()
{
  mLLIRBB.erase( mLLIRBB.begin(), mLLIRBB.end() );

  mLLIRFunc.erase( mLLIRFunc.begin(), mLLIRFunc.end() );

  mLLIR.erase( mLLIR.begin(), mLLIR.end() );

  mStack.reset();

  #ifdef HAVE_ALIAS_ANALYSIS
  if ( mAlias ) {
    delete mAlias;
    mAlias = nullptr;
  }
  #endif

  mInstructionFactory.setCurrentInstruction( nullptr );

  // Reset label map.
  mBlockLabelMap.clear();

  // Reset WIR data structures.
  mFunctionMap.clear();
  mBBMap.clear();

  TC179x_wirBB = nullptr;
  TC179x_wirFct = nullptr;
  TC179x_wirProc = nullptr;
};


Configuration *TC179x_CodeSelector::getConfig()
{
  return mConfig;
}


InstructionFactory &TC179x_CodeSelector::getInstructionFactory( void )
{
  return( mInstructionFactory );
};


#ifdef HAVE_ALIAS_ANALYSIS
IR_AliasAnalysis *TC179x_CodeSelector::getAliasAnalysis() const
{
  return mAlias;
}
#endif


/*
  setCurrentInstruction sets the pointer to the LLIR instruction lastly
  generated during code selection.
*/
void TC179x_CodeSelector::setCurrentInstruction( LLIR_Instruction *i )
{
  mInstructionFactory.setCurrentInstruction( i );
};


/*
  getCurrentInstruction returns a pointer to the LLIR instruction lastly
  generated during code selection.
*/
LLIR_Instruction *TC179x_CodeSelector::getCurrentInstruction( void ) const
{
  return( mInstructionFactory.getCurrentInstruction() );
};


/*
  doCodeSelection is responsible for the actual task of code selection for the
  Infineon TriCore ISA.

  doCodeSelection pre-prcesses the IR of @a mTE and generates data flow trees on
  the fly, calls the tree pattern matcher which performs a tree pattern matching
  based on a tree grammar's rule set. Post-processing of the generated assembly
  code creates the missing control flow edges and the control flow graph.
*/
void TC179x_CodeSelector::doCodeSelection( void )
{
  DSTART( "virtual void TC179x_CodeSelector::doCodeSelection()" );

  WCCTimer codeselTimer( true, mConfig->getVerbosity() );

  TC179x_wirProc = nullptr;
  TC179x_wirFct = nullptr;
  TC179x_wirBB = nullptr;
  mStringConstants.clear();

  // Create instance of Flowfactmanager to collect and transform infos
  FlowFactManagerICDCtoLLIR ffmanager( *mConfig );

  assert( mTE.getIR() );
  mIR = mTE.getIR();

  assert( mTE.getConfig() );
  mConfig = mTE.getConfig();

  // Insert a '1'-condition into all for stmts without condition.
  CodeSelector::fixForStmtsWithoutCondition( mConfig, mIR );

  #ifdef DEBUG_WCC
  // If "--keepIR" is passed to wcc, generate ICD-C output.
  if ( mConfig->getKeepFile( Configuration::IR ) ) {
    ofstream out( "test.ir" );

    if ( !out.is_open() )
      throw ufFatalError( "Failed to open file 'test.ir'." );

    for ( auto c : mIR->getCompilationUnits() )
      c->writeC( out );
  }
  #endif

  // Create empty WIR structure.
  createWIRSkeleton();

  // Iterate functions and generate assembly code.
  auto irUnitIt = mIR->getCompilationUnits().begin();
  auto wirUnitIt = mSystem.begin();

  IR_SymbolTable *globSymbol = &(mIR->globalSymbolTable);
  IR_SymbolTable *fileSymbol = nullptr;


  #ifdef HAVE_ALIAS_ANALYSIS
  if ( !mAlias &&
       mConfig->getOptimizationFlag( Configuration::WCETILPDATASPM ) )
    mAlias = new IR_AliasAnalysis( *mIR );
  #endif

  // This variable is used to enforce the code selector to process the global
  // table exactly once.
  bool isProcessed = false;

  // This variable indicates if a function is static.
  bool isFunctionStatic = false;

  for ( auto file : mConfig->getFiles() ) {
    if ( file->hasIR() ) {
      mConfig->printVerboseMessage(
        VERBOSE_5, "Processing file '%s'.", file->getFileName().c_str() );

      IR_CompilationUnit *irUnit = *irUnitIt;
      TC179x_wirUnit = &(wirUnitIt->get());

      // The created LLIR objects have the same temporary name as their IR
      // compilation units which are constructed from preprocessed I-files.
      LLIR *llir = new LLIR( 1, irUnit->getFilename() );
      mLLIR.push_back( llir );
      file->setLLIR( true );
      setCurrentInstruction( nullptr );

      // Set up architecture specific object-sections.
      LLIR_ObjectSectionLayout *layout = llir->getObjectSectionLayout();
      LLIR_ObjectSection *section;

      ufAssert( layout );

      // Text mapped to cached flash memory.
      section = layout->createObjectSection();
      section->setName( ".text_cached" );
      section->setAlignment( 10 ); // Align 2^10 = 1K
      if ( !layout->registerObjectSection( section ) )
        ufAssertT( 0, "Unable to instantiate object-section .text_cached" );

      // Text mapped to SPM.
      section = layout->createObjectSection();
      section->setName( ".text_spm" );
      section->setAlignment( 10 );
      if ( !layout->registerObjectSection( section ) )
        ufAssertT( 0, "Unable to instantiate object-section .text_spm" );

      // Text mapped to INTTAB.
      section = layout->createObjectSection();
      section->setName( ".inttab" );
      section->setAlignment( 3 );
      if ( !layout->registerObjectSection( section ) )
        ufAssertT( 0, "Unable to instantiate object-section .inttab" );


      // Data mapped to SPM.
      section = layout->createObjectSection();
      section->setName( ".data_spm" );
      section->setAlignment( 10 );     // 1K aligned
      if ( !layout->registerObjectSection( section ) )
        ufAssertT( 0, "Unable to instantiate object-section .data_spm" );

      // Add some pragmas to every LLIR.
      string pragmaString = "Generated by " PACKAGE " " VERSION " for ";
      pragmaString += __BLO_ARCH_TYPE__ " " __BLO_ARCH_VERSION__;
      llir->AddPragma( new LLIR_Pragma( pragmaString.c_str(), true ) );
      pragmaString = "file      \"" + file->getFileName() + "\"";
      llir->AddPragma( new LLIR_Pragma( pragmaString.c_str(), false ) );

      // The global symbol table is processed exactly once. Here, just
      // non-static global symbols are considered.
      if ( !isProcessed )
        processSymbolTable( llir, *TC179x_wirUnit, globSymbol );
      // Prevent further processing of global symbol table.
      isProcessed = true;

      // Next, the file symbol table containing global static variables and
      // global functions is processed.
      fileSymbol = &( irUnit->fileScopeSymbolTable );
      processSymbolTable( llir, *TC179x_wirUnit, fileSymbol, false );

      for ( auto irFunc : irUnit->getFunctions() ) {
        // The function is pushed on the stack. All local and non-static
        // symbols are taken into accont.
        mStack.pushFunction( irFunc );

        // The last step is to traverse hierarchically through all function
        // symbol tables and handle function static symbols.
        irFunc->getTopCompound().localSymbolTable.iterateSymbolTables(
          (void (*)( IR_SymbolTable &, void * )) symtabIterator,
          (void *) this );
      }

      for ( auto irFunc : irUnit->getFunctions() ) {
        mConfig->printVerboseMessage(
          VERBOSE_5,
          "  Processing function '%s'.",
          irFunc->getSymbol().getName().c_str() );

        LLIR_Function *llirFunc = nullptr;
        list<LLIR_Instruction *> stackAdjustment;
        int stackAdjOffset = 0;

        string tmp_func_name = irFunc->getSymbol().getWrittenName();

        // Set current WIR function.
        TC179x_wirFct = &( getWIRFunction( irFunc ) );
        TC179x_wirFct->setFrameSize( mStack.getMaxArgOverflowSize( irFunc ) );

        // Determine current WIR processor core.
        const WIR_Section &sec =
          mSystem.findSymbol( *TC179x_wirFct ).getSection();
        TC179x_wirProc = dynamic_cast<TC13 *>( &(sec.getProcessor() ) );

        // Create tree pattern matcher instance.
        IR_TreePatternMatching tpm( *mTreePatternMatcher, irFunc, false, true );
        if ( mConfig->getVerbosity() >= VERBOSE_8 )
          tpm.setDumpCover();

        // Create WIR basic block per IR basic block.
        for ( auto irBB : irFunc->getBasicBlocks() ) {
          stringstream ss;
          ss << irBB;

          WIR_BasicBlock &b = TC179x_wirFct->pushBackBasicBlock( {} );
          mBBMap.insert( { ss.str(), b } );
        }

        for ( auto irBB : irFunc->getBasicBlocks() ) {
          // Generate code for each block separately, emit block labels first.
          setCurrentInstruction( nullptr );
          LLIR_BB *llirBB = nullptr;

          if ( llirFunc == nullptr ) {
            // Generate very first basic block of a function.
            string label = tmp_func_name;
            llirBB = new LLIR_BB( label.c_str() );

            // Add this information to the ffmanager (Top Compound of func)
            ffmanager.addTranslation( &(irFunc->getTopCompound()), llirBB );

            // Generate new function with its first basic block. This first BB
            // serves as container for stack allocation and function argument
            // saving commands.
            llirFunc = new LLIR_Function( tmp_func_name.c_str(), llirBB );
            llirFunc->SetFrameSize( mStack.getMaxArgOverflowSize( irFunc ) );
            mLLIRFunc.push_back( llirFunc );
            llir->InsertFunction( llirFunc );

            // Traverse all IR_LoopStmt of the current IR function and store
            // those loops for which the LOOP instruction is applicable. This
            // must be done for each LLIR function to be able to add virtual
            // registes that serve as counter.
            findLOOPCandidates( irFunc );

            mLLIRBB.push_back( llirBB );

            // BackAnnotation
            mBackAnnotation->addNewMapping( llirBB, irBB );

            // Adjust stack frame for the current function.
            int stackFrameSize = mStack.getStackFrameSize( irFunc );
            int paramFrameSize =
              mStack.getParameterStackFrameSize(
                irFunc->getSymbol().getType() );
            stackAdjOffset = stackFrameSize - paramFrameSize;

            if ( stackAdjOffset > 0 )
              stackAdjustment =
                mStack.adjustStackFrame( llirFunc, llirBB, stackAdjOffset );

            // Determine if function is static.
            isFunctionStatic =
              ( irFunc->getSymbol().getType().getStorageClass() ==
                IR_Type::STATIC );

            // Add directive "global" only if function is not static.
            llirFunc->setGlobal( !isFunctionStatic );

            pragmaString =
              "type      " + irFunc->getSymbol().getName() + ",@function";
            llirFunc->AddPragma(
              new LLIR_Pragma( pragmaString.c_str(), false ) );
          }

          // The following assertion is required for the clang static analyzer.
          ufAssert( llirFunc != nullptr );

          stringstream ss;
          ss << irBB;

          // Generate LLIR basic block for actual code generation.
          string label = getBlockLabel( ss.str() );
          llirBB = new LLIR_BB( label.c_str() );
          llirFunc->InsertBB( llirBB, nullptr );
          mLLIRBB.push_back( llirBB );

          // Obtain WIR basic block for actual code generation.
          WIR_BasicBlock &b = getWIRBlock( irBB );
          TC179x_wirBB = &b;

          // BackAnnotation
          mBackAnnotation->addNewMapping( llirBB, irBB );

          // Add this information to the ffmanager (further bb of func).
          ffmanager.addTranslation( irBB, llirBB );

          // Build trees and perform tree pattern matching block wise.
          if ( !tpm.generate( irBB ) ) {
            string location = file->getFileName();
            location += "::" + irFunc->getSymbol().getName();

            IR_Stmt *firstStmt = irBB->getStatements().front();
            unsigned int line = firstStmt->getFileContext().getLine();

            throw ufFatalError( location, line, "Uncovered Tree." );
          }
        }

        // The following assertion is required for the clang static analyzer.
        ufAssert( llirFunc != nullptr );

        // Move instruction for stack adjustment to the very beginning of the
        // function.
        mStack.adjustStackFrame(
          *TC179x_wirProc, *(TC179x_wirFct->begin()), stackAdjOffset );
        for ( auto itAdjust = stackAdjustment.rbegin();
              itAdjust != stackAdjustment.rend(); ++itAdjust )
          llirFunc->GetFirstBB()->MoveIns( *itAdjust );

        // This for loop generates the valid control flow graph of the LLIR.
        for ( auto llirBB = llirFunc->GetFirstBB(); llirBB;
              llirBB = llirFunc->GetNextBB( llirBB ) ) {
          // Generate valid control edges.
          LLIR_Instruction *lastIns = llirBB->GetLastIns();
          LLIR_Operation *lastOp = nullptr;

          if ( lastIns != nullptr ) {
            lastOp = lastIns->GetOp( 0 );

            if ( isJumpInstruction( lastOp->GetInstruction() ) &&
                 !isCALLInstruction( lastOp->GetInstruction() ) &&
                 !isRETInstruction( lastOp->GetInstruction() ) ) {
              // Search the label that this operation jumps to (is most often at
              // the back of the parameter list).
              string label = "";
              for ( int i = lastOp->GetNumberOfParameters() - 1; i >= 0; --i ) {
                LLIR_Parameter *theParam = lastOp->GetParam( i );
                if ( theParam->GetType() == PARAM_LABEL ) {
                  label = theParam->GetLabel();
                }
              }
              ufAssertT( label != "", "Failed to find label!" );

              // Register the newly discovered successor.
              LLIR_BB *child = llirFunc->FindBB( label.c_str() );
              ufAssert( child != nullptr );

              if ( llirBB->GetNumberOfSucc() == 0 )
                llirBB->AddSucc( child );
              else {
                LLIR_BB *first = llirBB->GetFirstSucc();
                if ( !( llirBB->GetNextSucc( first ) == child ||
                        first == child ) )
                  llirBB->AddSucc( child );
              }
            }
          }

          // Insert fall-through edges between succeeding basic blocks if the
          // current basic block does not end with an unconditional jump.
          if ( ( lastIns == nullptr ) ||
               ( ( lastOp != nullptr ) &&
                 ( lastOp->GetMnemonic() != INS_J_16 ) &&
                 ( lastOp->GetMnemonic() != INS_J_32 ) &&
                 ( lastOp->GetMnemonic() != INS_LOOPU_32 ) &&
                 ( lastOp->GetMnemonic() != INS_JA_32 ) &&
                 ( lastOp->GetMnemonic() != INS_JI_16 ) &&
                 ( lastOp->GetMnemonic() != INS_JI_32 ) &&
                 ( lastOp->GetMnemonic() != INS_RET_16 ) &&
                 ( lastOp->GetMnemonic() != INS_RET_32 ) &&
                 ( lastOp->GetMnemonic() != INS_RFE_16 ) &&
                 ( lastOp->GetMnemonic() != INS_RFE_32 ) ) ) {

            LLIR_BB *succBB = llirFunc->GetNextBB( llirBB );

            if ( succBB ) {
              bool edgeFound = false;
              LLIR_BB *firstSucc = llirBB->GetFirstSucc();

              if ( firstSucc ) {
                if ( firstSucc == succBB )
                  edgeFound = true;
                else {
                  LLIR_BB *nextSucc = llirBB->GetNextSucc( firstSucc );

                  if ( nextSucc )
                    if ( nextSucc == succBB )
                      edgeFound = true;
                }
              }

              if ( !edgeFound )
                llirBB->AddSucc( succBB );
            }
          }
        }

        if ( mConfig->getVerbosity() >= VERBOSE_8 )
          cout << endl;
      }

      writeROData( llir );

      ++irUnitIt;
      ++wirUnitIt;
    }
  }

  // Translate flow facts from ICD-C to LLIR for the entire IR over all
  // compilation units.
  ffmanager.doTranslation( mIR, mLLIR );

  // Map precolor information to new implicit parameters of operations.
  mapPrecolorsToDefsUses( mLLIR, mConfig );

  #ifdef HAVE_ALIAS_ANALYSIS
  if ( mAlias ) {
    delete mAlias;
    mAlias = nullptr;
  }
  #endif

  TC179x_wirBB = nullptr;
  TC179x_wirFct = nullptr;
  TC179x_wirProc = nullptr;
  mStringConstants.clear();
};


void TC179x_CodeSelector::addApplicableLoop( const IR_LoopStmt *loop,
                                      unsigned int iterations )
{
  mLOOPApplicable[ const_cast<IR_LoopStmt*>( loop ) ] = iterations;
}


bool TC179x_CodeSelector::hasApplicableLoop( const IR_LoopStmt *loop )
{
  return mLOOPApplicable.find( const_cast<IR_LoopStmt*>( loop ) ) !=
         mLOOPApplicable.end();
}


unsigned int TC179x_CodeSelector::getLOOPIterationCounts( const IR_LoopStmt *loop )
{
  ufAssertT( hasApplicableLoop( loop ), "No entry available." );
  return mLOOPApplicable[ const_cast<IR_LoopStmt*>( loop ) ];
}

unsigned int TC179x_CodeSelector::getNumberOfEnclosingLOOPs( const IR_Stmt &stmt )
{
  unsigned int surroundingLOOPs = 0;

  // Check for outer applicable loops
  for ( const IR_Stmt *s=&stmt; s; s=s->getParent() ){
    const IR_LoopStmt *loop = dynamic_cast<const IR_LoopStmt*>( s );
    if ( loop && hasApplicableLoop( loop ) ){
      surroundingLOOPs++;
    }
  }

  return surroundingLOOPs;
};


void TC179x_CodeSelector::findLOOPCandidates( IR_Function *ir_func )
{
  // The iterator traverses all statements and stores loop statements
  // together with their iteration counts in the map mLOOPApplicable.
  ir_func->getTopCompound().iterateStatements(
      ( void ( * )( IR_Stmt &, void * ) )stmtIteratorFindLoopIterations, this );

  // In a second run, the nest level of the loops is determined and outer loops
  // for which the LOOP instruction is not applicable are removed.

  for( map< IR_LoopStmt*, unsigned int >::iterator
       lIt  = mLOOPApplicable.begin();
       lIt != mLOOPApplicable.end(); lIt++ ) {

    // Check for outer applicable loops
    int currentAppNestLevel = 0;
    for (IR_Stmt *s=lIt->first->getParent(); s; s=s->getParent()){
      IR_LoopStmt *loop=dynamic_cast<IR_LoopStmt*>(s);
      if (loop && hasApplicableLoop(loop)) {
        currentAppNestLevel++;
        if (currentAppNestLevel >= 2) {
          mLOOPApplicable.erase(loop);
        }
      }
    }
  }
  return;
}


void TC179x_CodeSelector::stmtIteratorFindLoopIterations( IR_Stmt &s, void *p )
{
  TC179x_CodeSelector *This=reinterpret_cast< TC179x_CodeSelector* >(p);

  IR_LoopStmt *loop = dynamic_cast< IR_LoopStmt * >( &s );

  if ( loop ) {
    unsigned int iterations = isLoopInsApplicable( loop );

    if( iterations > 1 ) {
      This->addApplicableLoop( loop, iterations );
    }
  }
}

unsigned int TC179x_CodeSelector::isLoopInsApplicable( IR_LoopStmt *l )
{
  IR_DoWhileStmt *doWhileStmt = dynamic_cast< IR_DoWhileStmt * >( l );
  IR_ForStmt     *forStmt     = dynamic_cast< IR_ForStmt     * >( l );
  IR_WhileStmt   *whileStmt   = dynamic_cast< IR_WhileStmt   * >( l );

  if ( doWhileStmt ) {
    IR_LoopAnalyzer la( *doWhileStmt );

    if ( ! doWhileStmt->getContExp().hasEffect()
         && la.getNumberOfIterations()
         && la.getNumberOfIterations()->getIntValue() > 1 ) {
      return la.getNumberOfIterations()->getIntValue();
    }
  } else if ( forStmt ) {
    IR_LoopAnalyzer la( *forStmt );

    if ( la.isExecuted()){
      return 2;
    }

  } else if ( whileStmt ) {
    IR_LoopAnalyzer la( *whileStmt );

    if ( la.isExecuted()){
      return 2;
    }
  }
  return 0;
}


Stack *TC179x_CodeSelector::getStack( void )
{
  return( &mStack );
};


const string TC179x_CodeSelector::getStaticName( const IR_Symbol *irsym )
{
  IR_SymbolTable *key = &irsym->getSymbolTable();

  outerMapType::iterator itOuter = mStaticName.find( key );

  // If there's already an entry in the map with that symbol table
  if ( itOuter != mStaticName.end() ) {
    string symName = irsym->getWrittenName();

    coreMapType::const_iterator itInner = itOuter->second.find( symName );

    if ( itInner != itOuter->second.end() )
      return itInner->second;
    else {
      string staticName = symName;
      stringstream sStream;
      sStream << mStaticName.size();
      staticName += "." + sStream.str();

      coreMapType::value_type v( symName, staticName );

      // Insert successful?
      if ( !(itOuter->second.insert( v ).second) )
        ufAssertT( 0, "\n Error while inserting block label" );

      // If not in map, return newly generated static symbol name
      return staticName;
    }
  }
  else {
    string symName = irsym->getWrittenName();

    string staticName = symName;
    stringstream sStream;
    sStream << mStaticName.size();
    staticName += "." + sStream.str();

    coreMapType::value_type val1( symName, staticName );

    coreMapType coreMap;

    // Insert successful?
    if ( !coreMap.insert( val1 ).second )
      ufAssertT( 0, "\n Error while inserting block label" );

    outerMapType::value_type val2( key, coreMap );

    mStaticName.insert( val2 );
    itOuter = mStaticName.find( key );
    // Insert successful?
    if ( itOuter == mStaticName.end() )
      ufAssertT( 0, "\n Error while inserting block label" );

    return staticName;

  }

  // Will never reach
  ufAssert( 0 );
  return string( "" );
}


/*
  startNewBasicBlock starts a new WIR basic block immediately after that one
  currently processed by the code selector.

  Using the given IR basic block, startNewBasicBlock updates the back-annotation
  mapping (TODO: Missing!).
*/
WIR::WIR_BasicBlock &TC179x_CodeSelector::startNewBasicBlock( const IR_BasicBlock &b )
{
  DSTART(
    "virtual WIR_BasicBlock& TC179x_CodeSelector::startNewBasicBlock(const IR_BasicBlock&)" );

  TC179x_wirBB = &( CodeSelector::startNewBasicBlock( b, *TC179x_wirBB ) );
  return( *TC179x_wirBB );
};


/*
  startNewBasicBlock starts a new WIR basic block immediately after that one
  currently processed by the code selector.

  startNewBasicBlock updates the back-annotation mapping by inserting a join
  mapping that marks the current and the new basic block as joined (mapped to
  the same IR BB) (TODO: Missing!).
*/
WIR::WIR_BasicBlock &TC179x_CodeSelector::startNewBasicBlock( void )
{
  DSTART( "virtual WIR_BasicBlock& TC179x_CodeSelector::startNewBasicBlock()" );

  TC179x_wirBB = &( CodeSelector::startNewBasicBlock( *TC179x_wirBB ) );
  return( *TC179x_wirBB );
};


void TC179x_CodeSelector::addLoopboundToLLIR( LLIR_BB *bb , int min, int max,
                                       LLIR_Loopbound::LoopControlType type )
{
  CodeSelector::addLoopboundToLLIR(bb, min, max,type, mConfig, mIR);
}


/*
  getStringConstData returns a WIR data object that contains the specified
  string constant.

  If the WIR system for which code is currently generated does not yet contain a
  data object for the given string constant, a novel read-only data object is
  created and returned.
*/
WIR::WIR_Data &TC179x_CodeSelector::getStringConstData( const std::string &c )
{
  DSTART( "WIR_Data& TC179x_CodeSelector::getStringConstData(const string&)" );

  auto it = mStringConstants.find( c );

  if ( it != mStringConstants.end() )
    return( it->second.get() );

  stringstream ss;
  ss << mStringConstants.size();
  string label = string( "_LC" ) + ss.str();

  WIR_Data &data = TC179x_wirUnit->pushBackData( WIR_Data( label ) );
  data.pushBackInitData( { WIR_DataInitType::iascii, c } );
  data.setSize( c.size() );

  WIR_Symbol &dataSym = mSystem.findSymbol( data );
  dataSym.setConst();
  dataSym.setGlobal( false );

  mStringConstants.insert( { c, data } );

  return( data );
};


//
// Private class methods
//

void TC179x_CodeSelector::symtabIterator( IR_SymbolTable &symtab,
                                          TC179x_CodeSelector *p )
{
  // Get current LLIR and process all function local symbol tables by adding
  // static symbols as assembly directives.
  LLIR *templlir = p->getLLIR().back();
  ufAssert ( templlir );

  p->processSymbolTable( templlir, *TC179x_wirUnit, &symtab, false );
};


/*
  getSize computes the size in bytes of an IR_Type.
*/
unsigned int TC179x_CodeSelector::getSize( const IR_Type *t ) const
{
  DSTART(
    "virtual unsigned int TC179x_CodeSelector::getSize(const IR_Type*) const" );

  const int bitsPerByte = TCIR_CONFIGURATION->bitwidthAddressable;
  const enum IR_Type::Type symType = t->getType();

  switch ( symType ) {
    case IR_Type::BOOL:
      return( boolBytes );

    case IR_Type::CHAR:
    case IR_Type::UNSIGNED_CHAR:
      return( charBytes );

    case IR_Type::SHORT:
    case IR_Type::UNSIGNED_SHORT:
      return( shortBytes );

    case IR_Type::INT:
    case IR_Type::UNSIGNED_INT:
      return( intBytes );

    case IR_Type::LONG:
    case IR_Type::UNSIGNED_LONG:
      return( longBytes );

    case IR_Type::LONG_LONG:
    case IR_Type::UNSIGNED_LONG_LONG:
      return( longLongBytes );

    case IR_Type::LONG_DOUBLE:
      return( longDoubleBytes );

    case IR_Type::DOUBLE:
      return( doubleBytes );

    case IR_Type::FLOAT:
      return( floatBytes );

    case IR_Type::POINTER:
      return( pointerBytes );

    case IR_Type::ARRAY:
      // Round up the number of needed bits to the next byte border.
      return( t->bitSize() + bitsPerByte - 1 ) / bitsPerByte;

    case IR_Type::STRUCT:
    case IR_Type::UNION:
      // Round up the number of needed bits to the next byte border
      return( t->bitSize() + bitsPerByte - 1 ) / bitsPerByte;

    default: {
      throw ufFatalError( "Unsupported type of global variable (3)." );
      break;
    }
  }
};


/*
  getAlignment computes the alignment in bytes of an IR_Type.
*/
unsigned int TC179x_CodeSelector::getAlignment( const IR_Type *t ) const
{
  DSTART(
    "virtual unsigned int TC179x_CodeSelector::getAlignment(const IR_Type*) const" );

  enum IR_Type::Type symType = t->getType();

  switch ( symType ) {
    case IR_Type::BOOL:
      return( TCIR_CONFIGURATION->alignmentBool );

    case IR_Type::CHAR:
    case IR_Type::UNSIGNED_CHAR:
      return( TCIR_CONFIGURATION->alignmentChar );

    case IR_Type::SHORT:
    case IR_Type::UNSIGNED_SHORT:
      return( TCIR_CONFIGURATION->alignmentShort );

    case IR_Type::INT:
    case IR_Type::UNSIGNED_INT:
      return( TCIR_CONFIGURATION->alignmentInt );

    case IR_Type::LONG:
    case IR_Type::UNSIGNED_LONG:
      return( TCIR_CONFIGURATION->alignmentLong );

    case IR_Type::LONG_LONG:
    case IR_Type::UNSIGNED_LONG_LONG:
      return( TCIR_CONFIGURATION->alignmentLongLong );

    case IR_Type::LONG_DOUBLE:
      return( TCIR_CONFIGURATION->alignmentLongDouble );

    case IR_Type::DOUBLE:
      return( TCIR_CONFIGURATION->alignmentDouble );

    case IR_Type::FLOAT:
      return( TCIR_CONFIGURATION->alignmentFloat );

    case IR_Type::POINTER:
      return( TCIR_CONFIGURATION->alignmentPointer );

    case IR_Type::ARRAY: {
      auto *theArrayType = dynamic_cast<const IR_ArrayType *>( t );
      return( theArrayType->getBaseType().alignment() );
    }

    case IR_Type::STRUCT:
    case IR_Type::UNION:
      return( TCIR_CONFIGURATION->alignmentComposedType );

    default: {
      throw ufFatalError( "Unsupported type of global variable (3)." );
      break;
    }
  }
};


// Generates an expression string suitable for assembly.
std::pair<bool, std::string> TC179x_CodeSelector::irToAsmInitializer(IR_Exp *exp, IR_Type *targetType)
{
  DSTART(
    "virtual pair<bool, string> TC179x_CodeSelector::irToAsmInitializer(IR_Exp*, IR_Type*)" );

  IR_ConstExp           *constExp;
  IR_SymbolExp          *symbolExp;
  IR_StringConstExp     *stringConstExp;
  IR_UnaryExp           *unaryExp;
  IR_BinaryExp          *binaryExp;
  IR_SizeOfExp          *sizeOfExp;
  IR_ComponentAccessExp *compExp;
  std::ostringstream value;
  bool               numConst = true;

  ufAssert(exp);

  if ( ( sizeOfExp = dynamic_cast<IR_SizeOfExp *>( exp ) ) ) {

    value << sizeOfExp->getBaseType().sizeOf();

  } else if ( ( stringConstExp = dynamic_cast<IR_StringConstExp *>( exp ) ) ) {

    getRODataLabel( stringConstExp->getValue() );
    value <<
      TCCODESEL->getStringConstData( stringConstExp->getValue() ).getName();

    numConst = false;

  } else if ( ( unaryExp = dynamic_cast<IR_UnaryExp *> ( exp ) ) ) {

    std::pair<bool, std::string> res = irToAsmInitializer( &unaryExp->getOp(),
                                                           targetType);

    value << res.second;
    numConst = res.first;

  } else if ( ( binaryExp = dynamic_cast<IR_BinaryExp *> ( exp ) ) ) {

    std::pair<bool, std::string> resl = irToAsmInitializer(
      &binaryExp->getOp1(), targetType );

    std::pair<bool, std::string> resr = irToAsmInitializer(
      &binaryExp->getOp2(), targetType );

    numConst = resl.first | resr.first;

    switch ( binaryExp->getOperator() ) {
      case IR_BinaryExp::PLUS:

        // Apply pointer arithmetic
        if ( binaryExp->getType().getType() == IR_Type::POINTER ) {

          IR_IntConstExp *ie;
          if ( ( ie = dynamic_cast<IR_IntConstExp*>( &binaryExp->getOp1() ) ) ) {

            IR_PointerType *type = dynamic_cast<IR_PointerType*>(&binaryExp->getOp2().getType());
            ufAssert(type);
            value << resr.second  << " + " << (ie->getValue() * type->getBaseType().sizeOf());

          } else if ( ( ie = dynamic_cast<IR_IntConstExp*>( &binaryExp->getOp2() ) ) ) {

            IR_PointerType *type = dynamic_cast<IR_PointerType*>(&binaryExp->getOp1().getType());
            ufAssert(type);
            value << resl.second  << " + " << (ie->getValue() * type->getBaseType().sizeOf()) ;

          } else
            ufAssert(0);

        } else if ( binaryExp->getType().getType() == IR_Type::ARRAY ) {

          IR_IntConstExp *ie;
          if ( ( ie = dynamic_cast<IR_IntConstExp*>( &binaryExp->getOp1() ) ) ) {

            IR_FixArrayType *arrayType = dynamic_cast<IR_FixArrayType *>( &binaryExp->getOp2().getType() );
            ufAssertT (arrayType, "Unsupported initialization");
            value << resr.second  << " + " << ( ie->getValue() * arrayType->getBaseType().sizeOf() ) ;

          } else if ( ( ie = dynamic_cast<IR_IntConstExp*>( &binaryExp->getOp2() ) ) ) {

            IR_FixArrayType *arrayType = dynamic_cast<IR_FixArrayType *>( &binaryExp->getOp1().getType() );
            ufAssertT (arrayType, "Unsupported initialization");
            value << resl.second  << " + " << ( ie->getValue() * arrayType->getBaseType().sizeOf() ) ;

          } else
            ufAssert(0);

        } else {

          TRACE( "-- unhandled type: " << binaryExp->getType().getType() << "\n");

          value << resl.second << " + " << resr.second;

        }

        break;
      case IR_BinaryExp::MINUS:
        value << resl.second << " - " << resr.second;
        break;
      case IR_BinaryExp::MULT:
        value << resl.second << " * " << resr.second;
        break;
      case IR_BinaryExp::DIV:
        value << resl.second << " / " << resr.second;
        break;
      case IR_BinaryExp::AND:
        value << resl.second << " & " << resr.second;
        break;
      case IR_BinaryExp::OR:
        value << resl.second << " | " << resr.second;
        break;
      case IR_BinaryExp::XOR:
        value << resl.second << " ^ " << resr.second;
        break;
      default:
        TRACE( "-- unhandled binary expression: " << *binaryExp << "\n");
        ufAssert(0);
    }

  } else

  if ( dynamic_cast<IR_IndexExp *>( exp ) ) {

    ufAssertT(0, "If this triggers, the normalizing step wasn't exhaustive");

  } else if ( ( symbolExp = dynamic_cast<IR_SymbolExp*>( exp ) ) ) {

    IR_Symbol *sym = &symbolExp->getSymbol();
    IR_EnumType *enm;


    if ( ( enm = sym->getEnumType() ) ) {

      value << enm->getValue(sym);

    } else {

      if ( sym->getType().getStorageClass() == IR_Type::STATIC )
        value << getStaticName( sym );
      else
        value << sym->getWrittenName();

      if ( value.str()[0] == '_' )
        value.str(value.str() .substr(1));

    }

  } else if ( ( constExp = dynamic_cast<IR_ConstExp*>( exp ) ) ) {

    /*
     *
     * This branch assumes that the statement  has the form of n = m
     * where m is a numeric literal which is converted into the
     * appropriate target type encoding.
     *
     */

    IR_FloatConstExp *floatConstExp;

    if ( dynamic_cast<IR_IntConstExp *>( constExp ) ) {

      if ( (targetType->isIntegralType()
            && targetType->getType() != IR_Type::BOOL)
           || targetType->getType() == IR_Type::POINTER) {

        value << CodeSelector::getInitIntConst( exp, TCIR_CONFIGURATION->bitwidthInt ).getValue();

      } else if ( targetType->getType() == IR_Type::BOOL ) {

         value << (CodeSelector::getInitIntConst( exp, TCIR_CONFIGURATION->bitwidthInt) != 0);

      }else if ( targetType->isRealType() ) {

        IR_Type::Type t = targetType->getType();

        if ( t == IR_Type::LONG_DOUBLE ) {

          value << (IR_Float(CodeSelector::getInitIntConst( exp, TCIR_CONFIGURATION->bitwidthInt ), t).
                    getLongDoubleValue().getValue().getComposed());

        } else if ( t == IR_Type::DOUBLE ) {

          value << (IR_Float(CodeSelector::getInitIntConst( exp, TCIR_CONFIGURATION->bitwidthInt ), t).
                    getDoubleValue().getValue().getComposed());

        } else if ( t == IR_Type::FLOAT ) {

          value << (IR_Float(CodeSelector::getInitIntConst( exp, TCIR_CONFIGURATION->bitwidthInt ), t).
                    getSingleValue().getValue().getComposed());

        } else {
          throw
            ufFatalError(
              string( "Unsupported type " ) +
                to_string( targetType->getType() ) +
                " in initializer list (3)." );
        }

      } else {
        throw
          ufFatalError(
            string( "Unsupported type " ) + to_string( targetType->getType() ) +
            " as initialized type (4)." );
      }

    } else if ( ( floatConstExp = dynamic_cast<IR_FloatConstExp*>( constExp ) ) ) {

      if (targetType->isIntegralType()) {

        IR_Type::Type t = floatConstExp->getType().getType();

        if (t == IR_Type::DOUBLE) {

          value << IR_Integer(CodeSelector::getInitFloatConst( exp, t, TCIR_CONFIGURATION->bitwidthInt ).getDoubleValue(),
                              targetType->bitSize());

        } else if (t == IR_Type::LONG_DOUBLE) {

          value << IR_Integer(CodeSelector::getInitFloatConst( exp, t, TCIR_CONFIGURATION->bitwidthInt).getLongDoubleValue(),
                              targetType->bitSize());

        } else if (t == IR_Type::FLOAT) {

          value << IR_Integer(CodeSelector::getInitFloatConst( exp, t, TCIR_CONFIGURATION->bitwidthInt).getSingleValue(),
                              targetType->bitSize());

        } else
          throw ufFatalError( "Unsupported type in initializer list (0)." );

      } else if (targetType->isRealType()) {

        IR_Type::Type t = targetType->getType();

        if (t == IR_Type::LONG_DOUBLE) {

          value << (IR_Float(CodeSelector::getInitFloatConst( exp, t, TCIR_CONFIGURATION->bitwidthInt ), t).
                    getLongDoubleValue().getValue().getComposed());

        } else if (t == IR_Type::DOUBLE) {

          value << (IR_Float(CodeSelector::getInitFloatConst( exp, t, TCIR_CONFIGURATION->bitwidthInt ), t).
                    getDoubleValue().getValue().getComposed());

        } else if (t == IR_Type::FLOAT) {

          value << (IR_Float(CodeSelector::getInitFloatConst( exp, t, TCIR_CONFIGURATION->bitwidthInt ), t).
                    getSingleValue().getValue().getComposed());

        } else
          throw
            ufFatalError(
              string( "Unsupported type " ) +
                to_string( targetType->getType() ) +
                " in initializer list (1)." );

      } else
        throw
          ufFatalError(
            string( "Unsupported type " ) + to_string( targetType->getType() ) +
              " as initialized type (2)." );

    }

  } else if ( ( compExp = dynamic_cast<IR_ComponentAccessExp*>( exp ) ) ) {


    std::pair<bool, std::string> res = irToAsmInitializer( &compExp->getObjectExp(),
                                                           targetType);

    value << res.second;
    numConst = res.first;

  } else {

    ufAssert(0);

  }

  return std::make_pair( numConst, value.str() );
}
