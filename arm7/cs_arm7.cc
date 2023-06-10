/*

   This source file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Compiler Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2016 - 2022

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
#include <list>

// Include boost headers
#include <boost/current_function.hpp>

// Include WIR headers
#include <wir/wir.h>
#include <arch/arm/armv6.h>

// Include ICD headers
#include <arch/PROC/archinfo.h>
#include <optimize/irtransform.h>
#include <llir3/llir3.defs.h>
#include <arch/PROC/proc.h>

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
#include <misc/fileinfo.h>

// Include local headers
#include <codesel/codesel.h>
#include <arm7/cs_arm7.h>
#include <arm7/registrar.h>
#include <arm7/stack.h>
#include <arm7/tpm.h>

// Loopbound placement fixing.
#include "ICD-LLIR-ARM/objectives/llirflowfactref.h"
#include "llir3analyze/loopnesting/llirloopnestingforest.h"
#include "llir3analyze/loopnesting/llirloopnestingtree.h"
#include "llir3/llirloopbound.h"
#include "llir3optimize/flowfactupdater/llirflowfactupdater.h"


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
# include "codesel.conf"
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

ARM_Base *ARM7_wirProc = nullptr;
WIR_CompilationUnit *ARM7_wirUnit = nullptr;
WIR_Function *ARM7_wirFct = nullptr;
WIR_BasicBlock *ARM7_wirBB = nullptr;


//
// Public class methods
//

ARM7_CodeSelector::ARM7_CodeSelector( WIR::WIR_System &sys, TaskEntry &te,
                                      BackAnnotation *backannotation ) :
  CodeSelector( sys, te, backannotation ),
  mStack {},
  mInstructionFactory { *(mTE.getConfig()), *this }
{
  DSTART(
    "ARM7_CodeSelector::ARM7_CodeSelector(WIR_System&, TaskEntry&, BackAnnotation*)" );

  ARM7_wirProc = nullptr;
  ARM7_wirFct = nullptr;
  ARM7_wirBB = nullptr;

  // Preserve code selector of outer FSM.
  if ( Registrar::getCodeSelector() )
    mLastCodeSel = Registrar::getCodeSelector();
  else
    // If there are no outer FSMs, the codeselector must not be
    // preserved.
    mLastCodeSel = nullptr;
  Registrar::setCodeSelector( this );

  mTreePatternMatcher.reset( new ARM7_TreePatternMatcher );

  #ifdef HAVE_ALIAS_ANALYSIS
  mAlias = nullptr;
  #endif
};


ARM7_CodeSelector::~ARM7_CodeSelector()
{
  DSTART( "ARM7_CodeSelector::~ARM7_CodeSelector()" );

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

  ARM7_wirBB = nullptr;
  ARM7_wirFct = nullptr;
  ARM7_wirProc = nullptr;
};


//! reset deletes the private member objects to allow repetitive code selections
void ARM7_CodeSelector::reset()
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

  ARM7_wirBB = nullptr;
  ARM7_wirFct = nullptr;
  ARM7_wirProc = nullptr;
};


Configuration *ARM7_CodeSelector::getConfig()
{
  return mConfig;
};


InstructionFactory &ARM7_CodeSelector::getInstructionFactory( void )
{
  return( mInstructionFactory );
};


#ifdef HAVE_ALIAS_ANALYSIS
IR_AliasAnalysis *ARM7_CodeSelector::getAliasAnalysis() const
{
  return mAlias;
}
#endif


/*
  setCurrentInstruction sets the pointer to the LLIR instruction lastly
  generated during code selection.
*/
void ARM7_CodeSelector::setCurrentInstruction( LLIR_Instruction *i )
{
  mInstructionFactory.setCurrentInstruction( i );
};


/*
  getCurrentInstruction returns a pointer to the LLIR instruction lastly
  generated during code selection.
*/
LLIR_Instruction *ARM7_CodeSelector::getCurrentInstruction( void ) const
{
  return( mInstructionFactory.getCurrentInstruction() );
};


Stack *ARM7_CodeSelector::getStack( void )
{
  return( &mStack );
};


const string ARM7_CodeSelector::getStaticName( const IR_Symbol *irsym )
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
WIR::WIR_BasicBlock &ARM7_CodeSelector::startNewBasicBlock( const IR_BasicBlock &b )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  ARM7_wirBB = &( CodeSelector::startNewBasicBlock( b, *ARM7_wirBB ) );
  return( *ARM7_wirBB );
};


/*
  startNewBasicBlock starts a new WIR basic block immediately after that one
  currently processed by the code selector.

  startNewBasicBlock updates the back-annotation mapping by inserting a join
  mapping that marks the current and the new basic block as joined (mapped to
  the same IR BB) (TODO: Missing!).
*/
WIR::WIR_BasicBlock &ARM7_CodeSelector::startNewBasicBlock( void )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  ARM7_wirBB = &( CodeSelector::startNewBasicBlock( *ARM7_wirBB ) );
  return( *ARM7_wirBB );
};


void ARM7_CodeSelector::addLoopboundToLLIR( LLIR_BB *bb , int min, int max,
                                       LLIR_Loopbound::LoopControlType type )
{
  // Create instance of Flowfactmanager for addition
  FlowFactManagerICDCtoLLIR ffmanager( *mConfig );

  ffmanager.addLoopBoundToLLIR( min, max, bb, type, mIR );

  return;
}

void ARM7_CodeSelector::symtabIterator( IR_SymbolTable &symtab,
                                        ARM7_CodeSelector *p )
{
  // Get current LLIR and process all function local symbol tables by adding
  // static symbols as assembly directives.
  LLIR *templlir = p->getLLIR().back();
  ufAssert( templlir );

  p->processSymbolTable( templlir, *ARM7_wirUnit, &symtab, false );
};


/*
  doCodeSelection is responsible for the actual task of code selection for the
  ARM ISA.

  doCodeSelection pre-prcesses the IR of @a mTE and generates data flow trees on
  the fly, calls the tree pattern matcher which performs a tree pattern matching
  based on a tree grammar's rule set. Post-processing of the generated assembly
  code creates the missing control flow edges and the control flow graph.
*/
void ARM7_CodeSelector::doCodeSelection( void )
{
  DSTART( "virtual void ARM7_CodeSelector::doCodeSelection()" );

  WCCTimer codeselTimer( true, mConfig->getVerbosity() );

  ARM7_wirProc = nullptr;
  ARM7_wirFct = nullptr;
  ARM7_wirBB = nullptr;

  // Create instance of Flowfactmanager to collect and transform infos
  FlowFactManagerICDCtoLLIR ffmanager( *mConfig );

  assert( mTE.getIR() );
  mIR = mTE.getIR();

  assert( mTE.getConfig() );
  mConfig = mTE.getConfig();

  // Insert a '1'-condition into all for stmts without condition
  CodeSelector::fixForStmtsWithoutCondition( mConfig, mIR );

  #ifdef DEBUG_WCC
  // If "--keepIR" is passed to wcc, generate ICD-C output.
  if ( mConfig->getKeepFile( Configuration::IR ) ) {
    ofstream out( "test.ir" );

    if ( !out.is_open() )
      throw ufFatalError( "Failed to open file test.ir." );

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

  // This variable is used to enforce the codeselector to process the global
  // table exactly once.
  bool isProcessed = false;

  // This variable indicates if a function is static.
  bool isFunctionStatic = false;

  for ( auto file : mConfig->getFiles() ) {
    if ( file->hasIR() ) {
      mConfig->printVerboseMessage(
        VERBOSE_5, "Processing file '%s'.", file->getFileName().c_str() );

      IR_CompilationUnit *irUnit = *irUnitIt;
      ARM7_wirUnit = &(wirUnitIt->get());

      // The created LLIR objects have the same temporary name as their IR
      // compilation units which are constructed from preprocessed I-files.
      LLIR *llir = new LLIR( 1, file->getIFile() );
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

      // Add some pragmas to every LLIR.
      string pragmaString = "Generated by " PACKAGE " " VERSION " for ";
      pragmaString += __BLO_ARCH_TYPE__ " " __BLO_ARCH_VERSION__;
      llir->AddPragma( new LLIR_Pragma( pragmaString.c_str(), true ) );
      pragmaString = "file      \"" + file->getFileName() + "\"";
      llir->AddPragma( new LLIR_Pragma( pragmaString.c_str(), false ) );

      // The global symbol table is processed exactly once. Here, just
      // non-static global symbols are considered.
      if ( !isProcessed )
        processSymbolTable( llir, *ARM7_wirUnit, globSymbol );
      // Prevent further processing of global symbol table.
      isProcessed = true;

      // Next, the file symbol table containing global static variables and
      // global functions is processed.
      fileSymbol = &( irUnit->fileScopeSymbolTable );
      processSymbolTable( llir, *ARM7_wirUnit, fileSymbol, false );

      for ( auto irFunc : irUnit->getFunctions() ) {
        // The function is pushed on the stack. All local and non-static
        // symbols are taken into account.
        mStack.pushFunction( irFunc );

        // The last step is to traverse hierarchically through all function
        // symbol tables and handle function static symbols.
        irFunc->getTopCompound().localSymbolTable.iterateSymbolTables(
          (void (*)( IR_SymbolTable &, void * )) symtabIterator,
          (void *) this );
      }

      for ( auto irFunc : irUnit->getFunctions() ) {
        mConfig->printVerboseMessage(
          VERBOSE_5, "  Processing function '%s'.",
          irFunc->getSymbol().getName().c_str() );

        LLIR_Function *llirFunc = nullptr;
        list<LLIR_Instruction *> stackAdjustment;
        list<LLIR_Instruction *> stackInit;

        string tmp_func_name = irFunc->getSymbol().getWrittenName();

        // Set current WIR function.
        ARM7_wirFct = &( getWIRFunction ( irFunc ) );
        ARM7_wirFct->setFrameSize( mStack.getMaxArgOverflowSize( irFunc ) );

        // Determine current WIR processor core.
        const WIR_Section &sec =
          mSystem.findSymbol( *ARM7_wirFct ).getSection();
        ARM7_wirProc = dynamic_cast<ARM_Base *>( &(sec.getProcessor() ) );

        // Create tree pattern matcher instance.
        IR_TreePatternMatching tpm( *mTreePatternMatcher, irFunc, false, true );
        if ( mConfig->getVerbosity() >= VERBOSE_8 )
          tpm.setDumpCover();

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

            stackInit = mStack.InitStack( llirFunc, llirBB );

            mLLIRBB.push_back( llirBB );

            // BackAnnotation
            mBackAnnotation->addNewMapping( llirBB, irBB );

            // Adjust stack frame for the current function.
            int calleeSaveSize = mStack.getCalleeSavedRegs().size() * 4;
            int stackFrameSize = mStack.getStackFrameSize( irFunc );
            int paramFrameSize =
              mStack.getParameterStackFrameSize(
                irFunc->getSymbol().getType() );

            if ( stackFrameSize - paramFrameSize - calleeSaveSize > 0 )
              stackAdjustment =
                mStack.adjustStackFrame(
                  *ARM7_wirProc, *(ARM7_wirFct->begin() ), llirFunc,
                  llirBB, stackFrameSize - paramFrameSize - calleeSaveSize );

            // Determine if function is static.
            isFunctionStatic =
              ( irFunc->getSymbol().getType().getStorageClass() ==
                IR_Type::STATIC );

            // Add directive "global" only if function is not static.
            llirFunc->setGlobal( !isFunctionStatic );

            pragmaString =
              "type      " + irFunc->getSymbol().getName() + ",%function";
            llirFunc->AddPragma(
              new LLIR_Pragma( pragmaString.c_str(), false ) );
          }

          stringstream ss;
          ss << irBB;

          // Generate LLIR basic block for actual code generation.
          string label = getBlockLabel( ss.str() );
          llirBB = new LLIR_BB( label.c_str() );
          llirFunc->InsertBB( llirBB, nullptr );
          mLLIRBB.push_back( llirBB );

          // Generate WIR basic block for actual code generation.
          WIR_BasicBlock &b = ARM7_wirFct->pushBackBasicBlock( {} );
          mBBMap.insert( { ss.str(), b } );
          ARM7_wirBB = &b;

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
        // function. Therefore merge stack adjustment and stack init.
        stackAdjustment.insert( stackAdjustment.begin(), stackInit.begin(),
          stackInit.end() );
        for ( auto itAdjust = stackAdjustment.rbegin();
              itAdjust != stackAdjustment.rend(); ++itAdjust )
          llirFunc->GetFirstBB()->MoveIns( *itAdjust );

        // move all basic blocks which contain addresses to global data to the
        // end of the function
        for ( LLIR_BB* llirBB = llirFunc->GetFirstBB(); llirBB; ) {
          // Check if it is am ASM directive
          LLIR_BB* tmpBB = llirFunc->GetNextBB( llirBB );

          // Make sure the BB is not empty.
          if ( !llirBB->GetFirstIns() ) {
            // Iterate and continue.
            llirBB = tmpBB;
            continue;
          }

          // Check if it is am ASM directive
          if( isAsmDataDirective(llirBB->GetFirstIns() ) ) {
            // The block only needs to be moved, if..
            // * It is the first basic block of a function (...this should
            //   never be the case)
            // * It is not succeeding an unconditional jump (not a call) or
            //   another ASM directive.
            LLIR_BB* prevBB = llirFunc->GetPrevBB( llirBB );
            if ( !prevBB ||
                 !( ( isJumpInstruction( prevBB->GetLastIns() ) &&
                      !isCALLInstruction( prevBB->GetLastIns() ) &&
                      isAlwaysExecuted( prevBB->GetLastIns() ) ) ||
                    isAsmDataDirective( prevBB->GetLastIns() ) ) ) {
              // We have to move.
              llirFunc->MoveBB( llirBB, llirFunc->GetLastBB() );
            }
          }

          // Get the next BB.
          llirBB = tmpBB;
        }

        // Remove left over extended registers if existing in the current
        // function. We only use extended registers as a type for the code
        // selection, but they should never be part of an instruction.
        LLIR_Register* r = llirFunc->GetFirstRegister();
        while ( r ) {
          // Check if it is an extended reg.
          if ( isEReg( r ) ) {
            DOUT( "Found EReg " << r->GetName() <<", removing it." << endl );
            // Get the next reg.
            LLIR_Register* delReg = r;
            r = llirFunc->GetNextRegister( r );
            // Delete it. The children should survive.
            bool succ = llirFunc->DeleteRegister( delReg );
            ufAssert( succ );
          } else {
            r = llirFunc->GetNextRegister( r );
          }
        }

        // This for loop generates the valid control flow graph of the LLIR.
        for ( auto llirBB = llirFunc->GetFirstBB(); llirBB;
              llirBB = llirFunc->GetNextBB( llirBB ) ) {
          // Generate valid control edges.
          LLIR_Instruction *lastIns = llirBB->GetLastIns();
          LLIR_Operation *lastOp = nullptr;

          if ( lastIns != nullptr ) {
            lastOp = lastIns->GetOp( 0 );

            if ( isJumpInstruction( lastOp->GetInstruction() ) &&
                !isIndirectJumpInstruction( lastOp->GetInstruction() ) &&
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
          // Insert no fall-through edges if a basic block contains a literal
          // pool (only one .word per bb)
          if ( ( lastIns == nullptr ) ||
              ( ( lastOp != nullptr ) &&
                !( ( isJumpInstruction( lastIns ) &&
                     isAlwaysExecuted( lastIns ) &&
                     !isCALLInstruction( lastIns ) ) ||
                   isRETInstruction( lastIns ) ||
                   isAsmDataDirective( lastIns) ) ) ) {

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

  ARM7_wirBB = nullptr;
  ARM7_wirFct = nullptr;
  ARM7_wirProc = nullptr;
};


/*
  getSize computes the size in bytes of an IR_Type.
*/
unsigned int ARM7_CodeSelector::getSize( const IR_Type *t ) const
{
  DSTART( BOOST_CURRENT_FUNCTION );

  const int bitsPerByte = ARMIR_CONFIGURATION->bitwidthAddressable;
  const enum IR_Type::Type symType = t->getType();

  switch ( symType ) {
    case IR_Type::BOOL:
      return( 8 / 8 );          /*8 bit Bool*/

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
      return ( 64 / 8 );

    case IR_Type::DOUBLE:
      return( doubleBytes );

    case IR_Type::FLOAT:
      return( floatBytes );

    case IR_Type::POINTER:
      return( pointerBytes );

    case IR_Type::ARRAY:
      // Round up the number of needed bits to the next byte border
      return( t->bitSize() + bitsPerByte - 1 ) / bitsPerByte;

    case IR_Type::STRUCT:
    case IR_Type::UNION:
      // Round up the number of needed bits to the next byte border
      return( t->bitSize() + bitsPerByte - 1 ) / bitsPerByte;

    default: {
      throw ufFatalError( "Unsupported type of global variable (3)." );
    }
  }
};


// Generates an expression string suitable for assembly.
std::pair<bool, std::string> ARM7_CodeSelector::irToAsmInitializer(IR_Exp *exp, IR_Type *targetType)
{
  IR_ConstExp *constExp;
  IR_SymbolExp *symbolExp;
  IR_StringConstExp *stringConstExp;
  IR_UnaryExp *unaryExp;
  IR_BinaryExp *binaryExp;
  IR_SizeOfExp *sizeOfExp;
  IR_ComponentAccessExp *compExp;
  std::ostringstream value;
  bool numConst = true;

  ufAssert(exp);

  if ( ( sizeOfExp = dynamic_cast<IR_SizeOfExp *>( exp ) ) ) {

    value << sizeOfExp->getBaseType().sizeOf();

  } else if ( ( stringConstExp = dynamic_cast<IR_StringConstExp *>( exp ) ) ) {

    value << getRODataLabel( stringConstExp->getValue() );
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

            IR_PointerType *type = dynamic_cast<IR_PointerType*>(
              &binaryExp->getOp2().getType());
            ufAssert(type);
            value << resr.second  << " + " << ( ie->getValue() *
              type->getBaseType().sizeOf());

          } else if ( ( ie = dynamic_cast<IR_IntConstExp*>( &binaryExp->getOp2() ) ) ) {

            IR_PointerType *type = dynamic_cast<IR_PointerType*>(
              &binaryExp->getOp1().getType());
            ufAssert( type );
            value << resl.second  << " + " << (
              ie->getValue() * type->getBaseType().sizeOf()) ;

          } else
            ufAssert(0);

        } else if ( binaryExp->getType().getType() == IR_Type::ARRAY ) {

          IR_IntConstExp *ie;
          if ( ( ie = dynamic_cast<IR_IntConstExp*>( &binaryExp->getOp1() ) ) ) {

            IR_FixArrayType *arrayType = dynamic_cast<IR_FixArrayType *>(
              &binaryExp->getOp2().getType() );
            // Unsupported initialization
            ufAssert (arrayType );
            value << resr.second  << " + " << (
              ie->getValue() * arrayType->getBaseType().sizeOf() ) ;

          } else if ( ( ie = dynamic_cast<IR_IntConstExp*>( &binaryExp->getOp2() ) ) ) {

            IR_FixArrayType *arrayType = dynamic_cast<IR_FixArrayType *>(
              &binaryExp->getOp1().getType() );
            // Unsupported initialization
            ufAssert( arrayType );
            value << resl.second  << " + " << ( ie->getValue() *
              arrayType->getBaseType().sizeOf() ) ;

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
      default:
        TRACE( "-- unhandled binary expression: " << *binaryExp << "\n");
        ufAssert(0);
    }

  } else if ( dynamic_cast<IR_IndexExp*>( exp ) ) {

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

    if ( dynamic_cast<IR_IntConstExp*>( constExp ) ) {

      if ( (targetType->isIntegralType()
            && targetType->getType() != IR_Type::BOOL)
           || targetType->getType() == IR_Type::POINTER) {

        value << CodeSelector::getInitIntConst( exp,
          ARMIR_CONFIGURATION->bitwidthInt ).getValue();

      } else if ( targetType->getType() == IR_Type::BOOL ) {

         value << ( CodeSelector::getInitIntConst( exp,
           ARMIR_CONFIGURATION->bitwidthInt) != 0);

      }else if ( targetType->isRealType() ) {

        IR_Type::Type t = targetType->getType();

        if ( t == IR_Type::LONG_DOUBLE ) {

          value << ( IR_Float(CodeSelector::getInitIntConst( exp,
            ARMIR_CONFIGURATION->bitwidthInt ), t).getLongDoubleValue().getValue().getComposed());

        } else if ( t == IR_Type::DOUBLE ) {

          value << ( IR_Float(CodeSelector::getInitIntConst( exp,
            ARMIR_CONFIGURATION->bitwidthInt ), t).getDoubleValue().getValue().getComposed());

        } else if ( t == IR_Type::FLOAT ) {

          value << ( IR_Float(CodeSelector::getInitIntConst( exp,
            ARMIR_CONFIGURATION->bitwidthInt ), t).getSingleValue().getValue().getComposed());

        } else {
          throw ufFatalError( "Unsupported type in initializer list (3)." );
        }

      } else {

        throw ufFatalError( "Unsupported type %d as initialized type (4)." );
      }

    } else if ( ( floatConstExp = dynamic_cast<IR_FloatConstExp*>( constExp ) ) ) {

      if ( targetType->isIntegralType() ) {

        IR_Type::Type t = floatConstExp->getType().getType();

        if ( t == IR_Type::DOUBLE ) {

          value << IR_Integer( CodeSelector::getInitFloatConst( exp, t,
            ARMIR_CONFIGURATION->bitwidthInt ).getDoubleValue(),
            targetType->bitSize() );

        } else if ( t == IR_Type::LONG_DOUBLE ) {

          value << IR_Integer( CodeSelector::getInitFloatConst( exp, t,
            ARMIR_CONFIGURATION->bitwidthInt).getLongDoubleValue(),
            targetType->bitSize() );

        } else if ( t == IR_Type::FLOAT ) {

          value << IR_Integer( CodeSelector::getInitFloatConst( exp, t,
            ARMIR_CONFIGURATION->bitwidthInt).getSingleValue(),
            targetType->bitSize() );

        } else {
          throw ufFatalError( "Unsupported type in initializer list (0)." );
        }

      } else if ( targetType->isRealType() ) {

        IR_Type::Type t = targetType->getType();

        if (t == IR_Type::LONG_DOUBLE) {

          value << ( IR_Float( CodeSelector::getInitFloatConst( exp, t,
            ARMIR_CONFIGURATION->bitwidthInt ), t).getLongDoubleValue().getValue().getComposed());

        } else if (t == IR_Type::DOUBLE) {

          value << ( IR_Float( CodeSelector::getInitFloatConst( exp, t,
            ARMIR_CONFIGURATION->bitwidthInt ), t).getDoubleValue().getValue().getComposed());

        } else if (t == IR_Type::FLOAT) {

          value << ( IR_Float( CodeSelector::getInitFloatConst( exp, t,
            ARMIR_CONFIGURATION->bitwidthInt ), t).getSingleValue().getValue().getComposed());

        } else {
          throw ufFatalError( "Unsupported type in initializer list (1)." );
        }

      } else {
        throw ufFatalError( "Unsupported type as initialized type (2)." );
      }

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


/*
  getAlignment computes the alignment in bytes of an IR_Type.
*/
unsigned int ARM7_CodeSelector::getAlignment( const IR_Type *t ) const
{
  DSTART( BOOST_CURRENT_FUNCTION );

  enum IR_Type::Type symType = t->getType();

  switch ( symType ) {
    case IR_Type::BOOL:
      return( ARMIR_CONFIGURATION->alignmentBool );

    case IR_Type::CHAR:
    case IR_Type::UNSIGNED_CHAR:
      return( ARMIR_CONFIGURATION->alignmentChar );

    case IR_Type::SHORT:
    case IR_Type::UNSIGNED_SHORT:
      return( ARMIR_CONFIGURATION->alignmentShort );

    case IR_Type::INT:
    case IR_Type::UNSIGNED_INT:
      return( ARMIR_CONFIGURATION->alignmentInt );

    case IR_Type::LONG:
    case IR_Type::UNSIGNED_LONG:
      return( ARMIR_CONFIGURATION->alignmentLong );

    case IR_Type::LONG_LONG:
    case IR_Type::UNSIGNED_LONG_LONG:
      return( ARMIR_CONFIGURATION->alignmentLongLong );

    case IR_Type::LONG_DOUBLE:
      return( ARMIR_CONFIGURATION->alignmentLongDouble );

    case IR_Type::DOUBLE:
      return( ARMIR_CONFIGURATION->alignmentDouble );

    case IR_Type::FLOAT:
      return( ARMIR_CONFIGURATION->alignmentFloat );

    case IR_Type::POINTER:
      return( ARMIR_CONFIGURATION->alignmentPointer );

    case IR_Type::ARRAY: {
      auto *theArrayType = dynamic_cast<const IR_ArrayType *>( t );
      return( theArrayType->getBaseType().alignment() );
    }

    case IR_Type::STRUCT:
    case IR_Type::UNION:
      return( ARMIR_CONFIGURATION->alignmentComposedType );

    default: {
      throw ufFatalError( "Unsupported type of global variable (3)." );
    }
  }
};

void ARM7_CodeSelector::addImplParams( LLIR* l, IR* ir )
{
  DSTART( "ARM7_CodeSelector::addImplParams" );
  ufAssert( l );
  for ( LLIR_Function* f : l->GetFunctions() ) {
    for ( LLIR_BB* b : f->GetBasicBlocks() ) {
      LLIR_Instruction* ins = b->GetLastIns();
      if ( !ins )
        continue;

      // Check if we actually have a call to check
      if ( !isCALLInstruction( ins ) )
        continue;

      // A simple lambda to receive the label of a call target.
      auto getJumpTargetLabel = [] ( LLIR_Operation* op ) -> string {
        for ( LLIR_Parameter* p : op->GetParameters() ) {
          // We take the first label parameter.
          if ( p->GetType() == PARAM_LABEL )
            return p->GetLabel();

        }
        // Otherwise return an empty string.
        return "";
      };

      // Let's try to get the target function in case it was not passed
      string fun = getJumpTargetLabel( ins->GetFirstOp() );

      // Store the maximum number of registers needed for parameter passing.
      int regCntPar = 0;
      // ...and how many for the return value.
      int regCntRet = 0;
      if ( fun.size() ) {
        // We could retrieve a function name.
        // Now we get the corresponding IR compilation unit to the LLIR
        string filename = ins->GetBB()->GetFunction()->GetLLIR()->getFileName();

        IR_CompilationUnit* cu = nullptr;
        if ( ir ) {
          for ( IR_CompilationUnit* curCU : ir->getCompilationUnits() ) {
            if ( !curCU->getFilename().compare( filename ) ) {
              cu = curCU;
              break;
            }
          }
        }

        // Search for the correspondig function locally.
        IR_Function* irFun = nullptr;
        if ( cu ) {
          for ( IR_Function* f : cu->getFunctions() ) {
            if ( fun.compare( f->getSymbol().getName() ) == 0 ) {
              irFun = f;
              break;
            }
          }
        }

        // If the IR was set, but the function not found, we check the other compilation
        // units.
        if ( ir && !irFun) {
          for ( IR_CompilationUnit* otherCU : ir->getCompilationUnits() ) {
            // Skip the already checked CU
            if ( otherCU == cu )
              continue;
            // ...and search again for the correspondig function.
            for ( IR_Function* f : otherCU->getFunctions() ) {
              if ( fun.compare( f->getSymbol().getName() ) == 0 ) {
                irFun = f;
                break;
              }
            }

            if ( irFun )
              break;
          }
        }


        // Now count the number of registers which are needed to pass the arguments
        if ( !irFun ) {
          // We could not find the function inside the IRs.
          // We try to check if it is a an fp call from libgcc (quite common, since
          // the ARM7 does not have an FPU).
          // Or an integer divsion call.
          if ( fun.compare( "__addsf3" )     == 0 ||
               fun.compare( "__divsf3" )     == 0 ||
               fun.compare( "__mulsf3" )     == 0 ||
               fun.compare( "__subsf3" )     == 0 ||
               fun.compare( "__eqsf2" )      == 0 ||
               fun.compare( "__nesf2" )      == 0 ||
               fun.compare( "__ltsf2" )      == 0 ||
               fun.compare( "__lesf2" )      == 0 ||
               fun.compare( "__divsi3" )     == 0 ||
               fun.compare( "__truncdfsf2" ) == 0 ||
               fun.compare( "__fixdfsi" )    == 0 ||
               fun.compare( "__fixunsdfsi" ) == 0 ||
               fun.compare( "__floatdisf" )  == 0 ||
               fun.compare( "__fixunsdfdi" ) == 0 ||
               fun.compare( "__gtsf2" )      == 0 ) {
            regCntPar = 2;
            regCntRet = 1;

          } else if (
               fun.compare( "__adddf3" ) == 0 ||
               fun.compare( "__subdf3" ) == 0 ||
               fun.compare( "__muldf3" ) == 0 ||
               fun.compare( "__divdf3" ) == 0 ) {
            regCntPar = 4;
            regCntRet = 2;

          } else if (
               fun.compare( "__eqdf2" )  == 0 ||
               fun.compare( "__nedf2" )  == 0 ||
               fun.compare( "__ltdf2" )  == 0 ||
               fun.compare( "__ledf2" )  == 0 ||
               fun.compare( "__gtdf2" )  == 0 ||
               fun.compare( "__gedf2" )  == 0 ||
               fun.compare( "__gesf2" )  == 0 ) {
            regCntPar = 4;
            regCntRet = 1;

          } else if (
                      fun.compare( "__extendsfdf2" ) == 0 ||
                      fun.compare( "__floatsidf" )   == 0 ||
                      fun.compare( "__floatusidf" )  == 0 ||
                      fun.compare( "__floatusisf" )  == 0 ||
                      fun.compare( "__fixsfdi" )     == 0 ) {
            regCntPar = 1;
            regCntRet = 2;

          } else if ( fun.compare( "__fixsfsi" )    == 0 ||
                      fun.compare( "__floatsisf" )  == 0 ||
                      fun.compare( "__fixunssfdi" ) == 0 ||
                      fun.compare( "__fixunssfsi" ) == 0 ) {
            regCntPar = 1;
            regCntRet = 1;

          } else if ( fun.compare( "__floatdidf" ) == 0 ||
                      fun.compare( "__fixdfdi" )   == 0 ) {
            regCntPar = 2;
            regCntRet = 2;

          } else {
            // Assuming worst case.
            regCntPar = 4;
            // The "worst case" for parameter returning is a void return, as
            // then no register are newly defined, hence the the livetime is
            // not reset.
            regCntRet = 0;
          }

        } else {
          // We found a local function call.
          // Declaration of the lambda function so we can use it recursively
          std::function< int( IR_Type*, bool ) > getNeededRegs;
          getNeededRegs = [ &getNeededRegs ]( IR_Type* irt, bool arg )-> int {
            IR_Type::Type t = irt->getType();
            if ( ( t == IR_Type::BOOL ) || ( t == IR_Type::ARRAY ) ||
                ( t == IR_Type::CHAR ) || ( t == IR_Type::FLOAT ) ||
                ( t == IR_Type::FUNCTION ) || ( t == IR_Type::LONG ) ||
                ( t == IR_Type::POINTER ) || ( t == IR_Type::INT ) ||
                ( t == IR_Type::SHORT ) || ( t == IR_Type::UNSIGNED_CHAR ) ||
                ( t == IR_Type::UNSIGNED_INT ) || ( t == IR_Type::UNSIGNED_LONG ) ||
                ( t == IR_Type::UNSIGNED_SHORT ) )
              return 1;
            else if ( ( t == IR_Type::DOUBLE ) || ( t == IR_Type::LONG_DOUBLE ) ||
                      ( t == IR_Type::LONG_LONG ) || ( t == IR_Type::UNSIGNED_LONG_LONG ) )
              return 2;
            else if ( t == IR_Type::STRUCT ) {
              // We have to check what's inside the struct
              IR_ComposedType* c = dynamic_cast< IR_ComposedType* >( irt );
              if ( !c ) {
                // Failed to get the internal structure of the struct, assuming the
                // worst case.
                if ( arg )
                  return 4;
                else
                  return 0;
              }

              int tmpCnt = 0;
              for ( IR_Symbol* s : c->getComponents().getSymbols() ) {
                tmpCnt += getNeededRegs( &( s->getType() ), arg );
              }

              return tmpCnt;
            } else if ( t == IR_Type::VOID )
              return 0;
            else {
              //Retun worst case
              if ( arg )
                return 4;
              else
                return 0;
            }
          };

          for ( IR_Symbol* sym : irFun->functionArguments.getSymbols() ) {
            regCntPar += getNeededRegs( &( sym->getType() ), true );
          }

          // And also get the number of register needed for the return.
          regCntRet = getNeededRegs(
            &irFun->getSymbol().getType().getReturnType(), false );

        }

        // No registers needed.
        if ( regCntPar == 0 && regCntRet == 0 )
          continue;

      } else {
       // Worst case.
        regCntPar = 4;
        regCntRet = 0;
      }

      // Create a list of all potential passing registers
      std::vector< string > param_reg_names;
      param_reg_names.push_back( PHREG_R0 );
      param_reg_names.push_back( PHREG_R1 );
      param_reg_names.push_back( PHREG_R2 );
      param_reg_names.push_back( PHREG_R3 );

      // We can pass max. 4 parameters via registers
      std::list< pair< LLIR_Register*, LLIR_UsageType  > > functionRegs;
      LLIR_BB* bb = ins->GetBB();
      for ( unsigned int i = 0; i <  param_reg_names.size(); ++i ) {
        if ( i >= static_cast< unsigned int >( regCntPar ) &&
             i >= static_cast< unsigned int >( regCntRet ) )
          break;
        // Differentiate whether the register is only used, defined, or both.
        if ( i < static_cast< unsigned int >( regCntPar ) &&
             i < static_cast< unsigned int >( regCntRet ) ) {
          // It is defined and used.
          functionRegs.push_back( { LLIR_Register::Create( bb->GetFunction(),
            param_reg_names.at( i ).c_str(), false ), USAGE_DEFUSE } );
        } else if ( i < static_cast< unsigned int >( regCntPar ) &&
                    i >= static_cast< unsigned int >( regCntRet ) ) {
          // It is used, but not defined.
          functionRegs.push_back( { LLIR_Register::Create( bb->GetFunction(),
            param_reg_names.at( i ).c_str(), false ), USAGE_USE } );
        } else {
          // It is defined, but not used.
          functionRegs.push_back( { LLIR_Register::Create( bb->GetFunction(),
            param_reg_names.at( i ).c_str(), false ), USAGE_DEF } );
        }

      }

      // Iterate over the existing implicit registers of ins and remove the ones
      // already existing from the list.
      for ( LLIR_Parameter* p : ins->GetFirstOp()->GetParameters() ) {
        if ( !p->IsImplicit() || ( p->GetType() != PARAM_REGISTER ) )
          continue;
        // Erase the found reg from the list (if existing)
        auto r_it = functionRegs.begin();
        while( r_it != functionRegs.end() ) {
          if( string( p->GetRegister()->GetName() ).compare(
              string( ( *r_it ).first->GetName() ) ) == 0 ) {
            functionRegs.erase( r_it++ );
          } else
            ++r_it;
        }
      }

      // Now only the registers, which still should be added to ins as an implicit
      // parameter, are left in functionRegs
      for ( pair< LLIR_Register*, LLIR_UsageType > regUse : functionRegs ) {
        ins->GetFirstOp()->AddParameter(
          new LLIR_Parameter( regUse.first, regUse.second, true ) );
      }
    }
  }

  return;
}

void ARM7_CodeSelector::fixLoopBoundPlacement( LLIR* l ) {
  DSTART( "ARM7_CodeSelector::fixLoopBoundPlacement" );

  // Build up loop forest. This can go wrong since our current loop nesting
  // forest does not support irreducible loops.
  try {
    LLIR_LoopNestingForest::createLoopNestingForests( { l } );

  } catch ( IrreducibleLoopException& ) {
    // For irreducible loops we simply skip.
    DOUT( "Irreducible loop. Aborting loop bound fixing." << endl );
    return;
  }

  // Iterate over all loop trees.
  for ( auto fun : l->GetFunctions() ) {
    auto forest = LLIR_LoopNestingForest::getForest( *fun );
    for ( auto t :
      forest->getLoopTreesInPostorder() ) {

      // Check if we can get loopbound.
      DOUT( "Trying to get loopbound" << endl );
      LLIR_Loopbound* lb = t->getLoopbound();
      if ( lb ) {
        DOUT( "Could get loopbound!" << endl );
        // Check if it is attached the loop entry.
        LLIR_BB* lbBB = lb->getLoop();
        auto entrances = t->getEntrances();
        // We currently only support single entry loops.
        if ( entrances.size() == 1 ) {
          LLIR_BB* treeBB = entrances.front();
          if ( treeBB != lbBB ) {
            DOUT( "Loopbound not correct, trying to fix it." << endl );
            LLIR_Flowfactupdater ffu;
            ffu.flowfactsMoveLoopbounds( lbBB, treeBB );
          }
        }
      }
    }
  }
}
