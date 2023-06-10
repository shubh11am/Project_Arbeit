  /*

   This source file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Compiler Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2021 - 2022

   Hamburg University of Technology (TUHH)
   Institute of Embedded Systems
   21071 Hamburg
   Germany

   http://www.tuhh.de/es/esd/research/wcc

*/

/*!
  @file rv32codesel.cc
  @brief This file implements the RISC-V RV32 tree pattern matching-based code
         selector.
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wcc.h>
#endif

// Include standard headers
#include <iostream>
#include <sstream>
#include <stack>

// Include boost headers
#include <boost/current_function.hpp>

// Include WIR headers
#include <wir/wir.h>
#include <arch/riscv/rv32imc.h>

// Include ICD headers
#include <ICD-C/optimize/irtransform.h>
#ifdef HAVE_ALIAS_ANALYSIS
#include <aliasanalysis/iraliasanalysis.h>
#endif

// Include back-annotation headers
#include <backannotation/backannotation.h>

// Include libuseful headers
#include <libuseful/debugmacros.h>
#include <libuseful/exceptions.h>
#include <libuseful/io.h>

// Include misc headers
#include <misc/misc.h>

// Include local headers
#include <rv32/rv32codesel.h>
#include <rv32/rv32registrar.h>
#include <rv32/tpm.h>


//
// Preprocessor macros
//

#define DEBUG_CODESEL 0

#undef TRACE
#undef CHECK
#undef CHECKX
#if DEBUG_CODESEL
  #include <iostream>
  #include <iomanip>
  #define TRACE(s) cout << "[Trace] " << s << flush
  #define CHECK(v) TRACE(# v << ": " << dec << v << endl);
  #define CHECKX(v) TRACE(# v << ": " << hex << v <<  endl);
#else
  #define TRACE(s) do{}while(0)
  #define CHECK(s) do{}while(0)
  #define CHECKX(s) do{}while(0)
#endif


//
// Code section
//

using namespace std;
using namespace WIR;
using namespace IR_Debug;


namespace RV32 {

//
// Global variables
//

RV32IMC *RV32_wirProc = nullptr;
WIR_CompilationUnit *RV32_wirUnit = nullptr;
WIR_Function *RV32_wirFct = nullptr;
WIR_BasicBlock *RV32_wirBB = nullptr;


//
// Public class methods
//

/*
  Default constructor.
*/
RV32_CodeSelector::RV32_CodeSelector( WIR::WIR_System &sys, TaskEntry &te,
                                      BackAnnotation *backannotation ) :
  CodeSelector { sys, te, backannotation },
  mStack {},
  mInstructionFactory { *(mTE.getConfig()), *this }
{
  DSTART(
    "RV32_CodeSelector::RV32_CodeSelector(WIR_System&, TaskEntry&, "
    "BackAnnotation*)" );

  RV32_wirProc = nullptr;
  RV32_wirUnit = nullptr;
  RV32_wirFct = nullptr;
  RV32_wirBB = nullptr;

  // Preserve code selector of outer FSM.
  if ( RV32_Registrar::isCodeSelectorRegistered() )
    mLastCodeSel = &(RV32_Registrar::getCodeSelector());
  else
    // If there are no outer FSMs, the code selector must not be preserved.
    mLastCodeSel = nullptr;
  RV32_Registrar::setCodeSelector( *this );

  mTreePatternMatcher.reset( new RV32_TreePatternMatcher );

  #ifdef HAVE_ALIAS_ANALYSIS
  mAlias = nullptr;
  #endif
};


/*
  Destructor.
*/
RV32_CodeSelector::~RV32_CodeSelector( void )
{
  DSTART( "RV32_CodeSelector::~RV32_CodeSelector()" );

  #ifdef HAVE_ALIAS_ANALYSIS
  if ( mAlias )
    delete( mAlias );
  #endif

  // Restore code selector of outer FSM.
  RV32_Registrar::setCodeSelector( *mLastCodeSel );
  mLastCodeSel = nullptr;

  RV32_wirBB = nullptr;
  RV32_wirFct = nullptr;
  RV32_wirUnit = nullptr;
  RV32_wirProc = nullptr;
};


/*
  reset clears all private members of this class in order to allow repetitive
  code selection.
*/
void RV32_CodeSelector::reset( void )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  mStack.reset();

  #ifdef HAVE_ALIAS_ANALYSIS
  if ( mAlias ) {
    delete( mAlias );
    mAlias = nullptr;
  }
  #endif

  // Reset label map.
  mBlockLabelMap.clear();

  // Reset WIR data structures.
  mFunctionMap.clear();
  mBBMap.clear();

  RV32_wirBB = nullptr;
  RV32_wirFct = nullptr;
  RV32_wirUnit = nullptr;
  RV32_wirProc = nullptr;
};


/*
  getConfig returns the current compiler configuration.
*/
Configuration *RV32_CodeSelector::getConfig( void )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  return( mConfig );
};


/*
  getInstructionFactory returns the RV32 instruction factory used by this code
  selector.
*/
RV32_InstructionFactory &RV32_CodeSelector::getInstructionFactory( void )
{
  DSTART(
    "RV32_InstructionFactory& RV32_CodeSelector::getInstructionFactory()" );

  return( mInstructionFactory );
};


#ifdef HAVE_ALIAS_ANALYSIS
/*
  getAliasAnalysis returns the ICD-C alias analysis used by this code selector
  (if any).
*/
IR_AliasAnalysis *RV32_CodeSelector::getAliasAnalysis( void ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  return( mAlias );
};
#endif


/*
  doCodeSelection is responsible for the actual task of code selection for the
  RISC-V RV32 ISA.

  doCodeSelection pre-processes the IR of @a mTE and generates data flow trees
  on the fly, calls the tree pattern matcher which performs a tree pattern
  matching based on a tree grammar's rule set. Post-processing of the generated
  assembly code creates the missing control flow edges and the control flow
  graph.
*/
void RV32_CodeSelector::doCodeSelection( void )
{
  DSTART( "virtual void RV32_CodeSelector::doCodeSelection()" );

  WCCTimer codeselTimer( true, mConfig->getVerbosity() );

  RV32_wirProc = nullptr;
  RV32_wirUnit = nullptr;
  RV32_wirFct = nullptr;
  RV32_wirBB = nullptr;
  mStringConstants.clear();

  ufAssert( mTE.getIR() );
  mIR = mTE.getIR();

  ufAssert( mTE.getConfig() );
  mConfig = mTE.getConfig();

  // Insert a '1'-condition into all for stmts without condition.
  fixForStmtsWithoutCondition( mConfig, mIR );

  #ifdef DEBUG_WCC
  // If "--keepIR" is passed to WCC, generate ICD-C output.
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

  for ( auto file : mConfig->getFiles() ) {
    if ( file->hasIR() ) {
      mConfig->printVerboseMessage(
        VERBOSE_5, "Processing file '%s'.", file->getFileName().c_str() );

      IR_CompilationUnit *irUnit = *irUnitIt;
      RV32_wirUnit = &(wirUnitIt->get());

      // The global symbol table is processed exactly once. Here, just
      // non-static global symbols are considered.
      if ( !isProcessed )
        processSymbolTable( *RV32_wirUnit, globSymbol );
      // Prevent further processing of global symbol table.
      isProcessed = true;

      // Next, the file symbol table containing global static variables and
      // global functions is processed.
      fileSymbol = &( irUnit->fileScopeSymbolTable );
      processSymbolTable( *RV32_wirUnit, fileSymbol, false );

      for ( auto irFunc : irUnit->getFunctions() ) {
        // The function is pushed on the stack. All local and non-static
        // symbols are taken into account.
        mStack.pushFunction( *irFunc );

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

        // Set current WIR function.
        RV32_wirFct = &( getWIRFunction( irFunc ) );
        RV32_wirFct->setFrameSize( mStack.getMaxArgOverflowSize( *irFunc ) );

        // Determine current WIR processor core.
        const WIR_Section &sec =
          mSystem.findSymbol( *RV32_wirFct ).getSection();
        RV32_wirProc = dynamic_cast<RV32IMC *>( &(sec.getProcessor() ) );

        // Create tree pattern matcher instance.
        IR_TreePatternMatching tpm( *mTreePatternMatcher, irFunc, false, true );
        if ( mConfig->getVerbosity() >= VERBOSE_8 )
          tpm.setDumpCover();

        // Create WIR basic block per IR basic block.
        for ( auto irBB : irFunc->getBasicBlocks() ) {
          stringstream ss;
          ss << irBB;

          WIR_BasicBlock &b = RV32_wirFct->pushBackBasicBlock( {} );
          mBBMap.insert( { ss.str(), b } );
        }

        bool firstBB = true;

        for ( auto irBB : irFunc->getBasicBlocks() ) {

          // Obtain WIR basic block for actual code generation.
          WIR_BasicBlock &b = getWIRBlock( irBB );
          RV32_wirBB = &b;

          // Add function prologue
          if(firstBB){
            // Calculate stack frame size for the current function.
            int stackFrameSize = mStack.getStackFrameSize( *irFunc );
            int paramFrameSize = mStack.getParameterStackFrameSize(
                                   irFunc->getSymbol().getType() );
            int stackAdjOffset = stackFrameSize - paramFrameSize;

            mStack.initStack( *RV32_wirProc, *RV32_wirFct, *RV32_wirBB,
                              stackAdjOffset );
            firstBB = false;
          }

          // TODO: Add back-annotation and flow-fact manager calls for WIR when
          //       existing.

          // Build trees and perform tree pattern matching block-wise.
          if ( !tpm.generate( irBB ) ) {
            string location = file->getFileName();
            location += "::" + irFunc->getSymbol().getName();

            IR_Stmt *firstStmt = irBB->getStatements().front();
            unsigned int line = firstStmt->getFileContext().getLine();

            throw ufFatalError( location, line, "Uncovered Tree." );
          }
        }

        if ( mConfig->getVerbosity() >= VERBOSE_8 )
          cout << endl;
      }

      ++irUnitIt;
      ++wirUnitIt;
    }
  }

  // TODO: Add flow-fact manager calls for WIR when existing.

  #ifdef HAVE_ALIAS_ANALYSIS
  if ( mAlias ) {
    delete( mAlias );
    mAlias = nullptr;
  }
  #endif

  RV32_wirBB = nullptr;
  RV32_wirFct = nullptr;
  RV32_wirUnit = nullptr;
  RV32_wirProc = nullptr;
  mStringConstants.clear();
};


/*
  getStack returns the code selector's current stack object.
*/
RV32_Stack &RV32_CodeSelector::getStack( void )
{
  DSTART( "RV32_Stack& RV32_CodeSelector::getStack()" );

  return( mStack );
};


/*
  getStaticName computes a unique name for a given static symbol.

  According to C, it is allowed to have multiple static variables of same name
  in different symbol tables. However, the high-level IR just obtains the
  symbol name from the source code which might be not unambiguous. Thus, the
  symbol's symbol map is read out and used as the key for the nested map. The
  value of the key is another map that contains as key the symbol name and as
  value the unique static symbol name. It's not just sufficient to have the
  symbol table as key, since there might be more than one static symbol within
  a symbol table. If there is no entry for the requested value in the map, it
  is generated, added to the map and returned. Otherwise, the existing entry is
  read out from the map.
*/
const string RV32_CodeSelector::getStaticName( const IR_Symbol *irsym )
{
  DSTART(
    "virtual const string RV32_CodeSelector::getStaticName(const IR_Symbol*)" );

  IR_SymbolTable *key = &irsym->getSymbolTable();
  auto itOuter = mStaticName.find( key );

  if ( itOuter != mStaticName.end() ) {
    // If there's already an entry in the map for that symbol table, look it up.
    string symName = irsym->getWrittenName();

    coreMapType::const_iterator itInner = itOuter->second.find( symName );

    if ( itInner != itOuter->second.end() )
      // Some entry in the inner map exists, return it.
      return( itInner->second );
    else {
      // Create new unique symbol name.
      string staticName = symName;
      stringstream sStream;
      sStream << mStaticName.size();
      staticName += "." + sStream.str();

      // Add new entry to inner map.
      itOuter->second.insert( { symName, staticName } );

      // Return the newly generated static symbol name.
      return( staticName );
    }
  } else {
    string symName = irsym->getWrittenName();

    // Create new unique symbol name.
    string staticName = symName;
    stringstream sStream;
    sStream << mStaticName.size();
    staticName += "." + sStream.str();

    // Create new inner map with new entry.
    coreMapType coreMap;
    coreMap.insert( { symName, staticName } );

    // Add new entry to outer map.
    mStaticName.insert( { key, coreMap } );

    // Return the newly generated static symbol name.
    return( staticName );
  }
};


/*
  getStringConstData returns a WIR data object that contains the specified
  string constant.

  If the WIR system for which code is currently generated does not yet contain a
  data object for the given string constant, a novel read-only data object is
  created and returned.
*/
WIR::WIR_Data &RV32_CodeSelector::getStringConstData( const std::string &c )
{
  DSTART( "WIR_Data& RV32_CodeSelector::getStringConstData(const string&)" );

  auto it = mStringConstants.find( c );

  if ( it != mStringConstants.end() )
    return( it->second.get() );

  stringstream ss;
  ss << mStringConstants.size();
  string label = string( "_LC" ) + ss.str();

  WIR_Data &data = RV32_wirUnit->pushBackData( WIR_Data( label ) );
  data.pushBackInitData( { WIR_DataInitType::iascii, c } );
  data.setSize( c.size() );

  WIR_Symbol &dataSym = mSystem.findSymbol( data );
  dataSym.setConst();
  dataSym.setGlobal( false );

  mStringConstants.insert( { c, data } );

  return( data );
};


/*
  startNewBasicBlock starts a new WIR basic block immediately after that one
  currently processed by the code selector.

  Using the given IR basic block, startNewBasicBlock updates the back-annotation
  mapping (TODO: Missing!).
*/
WIR::WIR_BasicBlock &RV32_CodeSelector::startNewBasicBlock( const IR_BasicBlock &b )
{
  DSTART(
    "virtual WIR_BasicBlock& RV32_CodeSelector::startNewBasicBlock(const "
    "IR_BasicBlock&)" );

  RV32_wirBB = &( CodeSelector::startNewBasicBlock( b, *RV32_wirBB ) );
  return( *RV32_wirBB );
};


/*
  startNewBasicBlock starts a new WIR basic block immediately after that one
  currently processed by the code selector.

  startNewBasicBlock updates the back-annotation mapping by inserting a join
  mapping that marks the current and the new basic block as joined (mapped to
  the same IR BB) (TODO: Missing!).
*/
WIR::WIR_BasicBlock &RV32_CodeSelector::startNewBasicBlock( void )
{
  DSTART( "virtual WIR_BasicBlock& RV32_CodeSelector::startNewBasicBlock()" );

  RV32_wirBB = &( CodeSelector::startNewBasicBlock( *RV32_wirBB ) );
  return( *RV32_wirBB );
};


//
// Private class methods
//

/*
  symtabIterator pre-processes an IR symbol table for a given RISC-V RV32 code
  selector. This method serves as callback function for
  IR_SymbolTable::iterateSymbolTables.

  iterateSymbolTables calls this function whenever a new symbol table is found.
  Furthermore, symtabIterator creates assembly directives when a static variable
  is found.
*/
void RV32_CodeSelector::symtabIterator( IR_SymbolTable &symtab,
                                        RV32_CodeSelector *p )
{
  DSTART(
    "static void RV32_CodeSelector::symtabIterator(IR_SymbolTable&, "
    "RV32_CodeSelector*)" );

  p->processSymbolTable( *RV32_wirUnit, &symtab, false );
};


/*
  irToAsmInitializer generates a string to be used for assembly-level
  initialization of a given IR expression.
*/
std::pair<bool, std::string> RV32_CodeSelector::irToAsmInitializer( IR_Exp *exp,
                                                                    IR_Type *targetType )
{
  DSTART(
    "virtual pair<bool, string> RV32_CodeSelector::irToAsmInitializer(IR_Exp*, "
    "IR_Type*)" );

  ostringstream value;
  bool numConst = true;

  ufAssert( exp != nullptr );

  if ( auto *sizeOfExp = dynamic_cast<IR_SizeOfExp *>( exp ) )
    value << sizeOfExp->getBaseType().sizeOf();
  else

  if ( auto *stringConstExp = dynamic_cast<IR_StringConstExp *>( exp ) ) {
    getRODataLabel( stringConstExp->getValue() );
    value <<
      RVCODESEL.getStringConstData( stringConstExp->getValue() ).getName();
    numConst = false;
  } else

  if ( auto *unaryExp = dynamic_cast<IR_UnaryExp *> ( exp ) ) {
    auto res = irToAsmInitializer( &unaryExp->getOp(), targetType );
    value << res.second;
    numConst = res.first;
  } else

  if ( auto *binaryExp = dynamic_cast<IR_BinaryExp *> ( exp ) ) {
    auto resl = irToAsmInitializer( &binaryExp->getOp1(), targetType );
    auto resr = irToAsmInitializer( &binaryExp->getOp2(), targetType );

    numConst = resl.first | resr.first;

    switch ( binaryExp->getOperator() ) {
      case IR_BinaryExp::PLUS: {
        // Apply pointer arithmetic.
        if ( binaryExp->getType().getType() == IR_Type::POINTER ) {
          IR_IntConstExp *ie;
          if ( ie = dynamic_cast<IR_IntConstExp *>( &binaryExp->getOp1() ) ) {
            auto *type =
              dynamic_cast<IR_PointerType *>( &binaryExp->getOp2().getType() );
            ufAssert( type );

            value << resr.second << " + "
                  << (ie->getValue() * type->getBaseType().sizeOf());
          } else

          if ( ie = dynamic_cast<IR_IntConstExp *>( &binaryExp->getOp2() ) ) {
            auto *type =
              dynamic_cast<IR_PointerType *>( &binaryExp->getOp1().getType() );
            ufAssert( type );

            value << resl.second  << " + "
                  << (ie->getValue() * type->getBaseType().sizeOf()) ;
          } else
            ufAssert( false );
        } else

        if ( binaryExp->getType().getType() == IR_Type::ARRAY ) {
          IR_IntConstExp *ie;
          if ( ie = dynamic_cast<IR_IntConstExp *>( &binaryExp->getOp1() ) ) {
            auto *arrayType =
              dynamic_cast<IR_FixArrayType *>( &binaryExp->getOp2().getType() );
            ufAssert( arrayType );

            value << resr.second  << " + "
                  << ( ie->getValue() * arrayType->getBaseType().sizeOf() ) ;
          } else

          if ( ie = dynamic_cast<IR_IntConstExp *>( &binaryExp->getOp2() ) ) {
            auto *arrayType =
              dynamic_cast<IR_FixArrayType *>( &binaryExp->getOp1().getType() );
            ufAssert( arrayType );

            value << resl.second  << " + "
                  << ( ie->getValue() * arrayType->getBaseType().sizeOf() ) ;
          } else
            ufAssert( false );
        } else {
          TRACE(
            "-- unhandled type: " << binaryExp->getType().getType() << "\n" );

          value << resl.second << " + " << resr.second;
        }

        break;
      }

      case IR_BinaryExp::MINUS: {
        value << resl.second << " - " << resr.second;
        break;
      }

      case IR_BinaryExp::MULT: {
        value << resl.second << " * " << resr.second;
        break;
      }

      case IR_BinaryExp::DIV: {
        value << resl.second << " / " << resr.second;
        break;
      }

      case IR_BinaryExp::AND: {
        value << resl.second << " & " << resr.second;
        break;
      }

      case IR_BinaryExp::OR: {
        value << resl.second << " | " << resr.second;
        break;
      }

      case IR_BinaryExp::XOR: {
        value << resl.second << " ^ " << resr.second;
        break;
      }

      default: {
        TRACE( "-- unhandled binary expression: " << *binaryExp << "\n" );
        ufAssert( false );
      }
    }

  } else

  if ( dynamic_cast<IR_IndexExp *>( exp ) )
    ufAssertT(
      false, "If this triggers, the normalizing step wasn't exhaustive.");
  else

  if ( IR_SymbolExp *symbolExp = dynamic_cast<IR_SymbolExp *>( exp ) ) {
    IR_Symbol *sym = &symbolExp->getSymbol();
    IR_EnumType *enm;

    if ( enm = sym->getEnumType() )
      value << enm->getValue(sym);
    else {
      if ( sym->getType().getStorageClass() == IR_Type::STATIC )
        value << getStaticName( sym );
      else
        value << sym->getWrittenName();

      if ( value.str()[ 0 ] == '_' )
        value.str( value.str() .substr( 1 ) );
    }
  } else

  if ( IR_ConstExp *constExp = dynamic_cast<IR_ConstExp *>( exp ) ) {
    // In the following, we assume that the statement has the form of n = m
    // where m is a numeric literal which is converted into the appropriate
    // target type encoding.

    if ( dynamic_cast<IR_IntConstExp *>( constExp ) ) {
      if ( ( targetType->isIntegralType() &&
             ( targetType->getType() != IR_Type::BOOL ) ) ||
           ( targetType->getType() == IR_Type::POINTER ) )
        value <<
          CodeSelector::getInitIntConst(
            exp, RVIR_CONFIGURATION->bitwidthInt ).getValue();
      else

      if ( targetType->getType() == IR_Type::BOOL )
         value <<
           ( CodeSelector::getInitIntConst(
               exp, RVIR_CONFIGURATION->bitwidthInt) != 0 );
      else

      if ( targetType->isRealType() ) {
        auto t = targetType->getType();

        if ( t == IR_Type::LONG_DOUBLE )
          value <<
            IR_Float(
              CodeSelector::getInitIntConst(
                exp, RVIR_CONFIGURATION->bitwidthInt ), t ).
              getLongDoubleValue().getValue().getComposed();
        else

        if ( t == IR_Type::DOUBLE )
          value <<
            IR_Float(
              CodeSelector::getInitIntConst(
                exp, RVIR_CONFIGURATION->bitwidthInt ), t ).
              getDoubleValue().getValue().getComposed();
        else

        if ( t == IR_Type::FLOAT )
          value <<
            IR_Float(
              CodeSelector::getInitIntConst(
                exp, RVIR_CONFIGURATION->bitwidthInt ), t ).
              getSingleValue().getValue().getComposed();
        else
          throw
            ufFatalError(
              string( "Unsupported type '" ) +
              to_string( targetType->getType() ) +
              "' in initializer list (1)." );
      } else
        throw
          ufFatalError(
            string( "Unsupported type '" ) +
            to_string( targetType->getType() ) + "' as initialized type (2)." );
    } else

    if ( auto *floatConstExp = dynamic_cast<IR_FloatConstExp *>( constExp ) ) {
      if ( targetType->isIntegralType() ) {
        auto t = floatConstExp->getType().getType();

        if ( t == IR_Type::DOUBLE )
          value <<
            IR_Integer(
              CodeSelector::getInitFloatConst(
                exp, t, RVIR_CONFIGURATION->bitwidthInt ).getDoubleValue(),
              targetType->bitSize() );
        else

        if ( t == IR_Type::LONG_DOUBLE )
          value <<
            IR_Integer(
              CodeSelector::getInitFloatConst(
                exp, t, RVIR_CONFIGURATION->bitwidthInt ).getLongDoubleValue(),
              targetType->bitSize() );
        else

        if ( t == IR_Type::FLOAT )
          value <<
            IR_Integer(
              CodeSelector::getInitFloatConst(
                exp, t, RVIR_CONFIGURATION->bitwidthInt ).getSingleValue(),
              targetType->bitSize() );
        else
          throw ufFatalError( "Unsupported type in initializer list (0)." );
      } else

      if ( targetType->isRealType() ) {
        auto t = targetType->getType();

        if ( t == IR_Type::LONG_DOUBLE )
          value <<
            IR_Float(
              CodeSelector::getInitFloatConst(
                exp, t, RVIR_CONFIGURATION->bitwidthInt ), t ).
              getLongDoubleValue().getValue().getComposed();
        else

        if ( t == IR_Type::DOUBLE )
          value <<
            IR_Float(
              CodeSelector::getInitFloatConst(
                exp, t, RVIR_CONFIGURATION->bitwidthInt ), t ).
              getDoubleValue().getValue().getComposed();
        else

        if ( t == IR_Type::FLOAT )
          value <<
            IR_Float(
              CodeSelector::getInitFloatConst(
                exp, t, RVIR_CONFIGURATION->bitwidthInt ), t ).
              getSingleValue().getValue().getComposed();
        else
          throw
            ufFatalError(
              string( "Unsupported type '" ) +
              to_string( targetType->getType() ) +
              "' in initializer list (3)." );
      } else
        throw
          ufFatalError(
            string( "Unsupported type '" ) +
            to_string( targetType->getType() ) + "' as initialized type (4)." );
    }
  } else

  if ( auto *compExp = dynamic_cast<IR_ComponentAccessExp *>( exp ) )
    return( irToAsmInitializer( &compExp->getObjectExp(), targetType ) );
  else
    ufAssert( false );

  return( make_pair( numConst, value.str() ) );
};


/*
  getSize computes the byte size of an IR type.
*/
unsigned int RV32_CodeSelector::getSize( const IR_Type *t ) const
{
  DSTART(
    "virtual unsigned int RV32_CodeSelector::getSize(const IR_Type*) const" );

  const int bitsPerByte = RVIR_CONFIGURATION->bitwidthAddressable;

  switch ( t->getType() ) {
    case IR_Type::BOOL:
      return( RVIR_CONFIGURATION->bitwidthBool / bitsPerByte );

    case IR_Type::CHAR:
    case IR_Type::UNSIGNED_CHAR:
      return( RVIR_CONFIGURATION->bitwidthChar / bitsPerByte );

    case IR_Type::SHORT:
    case IR_Type::UNSIGNED_SHORT:
      return( RVIR_CONFIGURATION->bitwidthShort / bitsPerByte );

    case IR_Type::INT:
    case IR_Type::UNSIGNED_INT:
      return( RVIR_CONFIGURATION->bitwidthInt / bitsPerByte );

    case IR_Type::LONG:
    case IR_Type::UNSIGNED_LONG:
      return( RVIR_CONFIGURATION->bitwidthLong / bitsPerByte );

    case IR_Type::LONG_LONG:
    case IR_Type::UNSIGNED_LONG_LONG:
      return( RVIR_CONFIGURATION->bitwidthLongLong / bitsPerByte );

    case IR_Type::LONG_DOUBLE:
      return( RVIR_CONFIGURATION->bitwidthLongDouble / bitsPerByte );

    case IR_Type::DOUBLE:
      return( RVIR_CONFIGURATION->bitwidthDouble / bitsPerByte );

    case IR_Type::FLOAT:
      return( RVIR_CONFIGURATION->bitwidthFloat / bitsPerByte );

    case IR_Type::POINTER:
      return( RVIR_CONFIGURATION->bitwidthPointer / bitsPerByte );

    //TODO: Nothing to change here in the ARRAY and STRUCT cases, right?
    case IR_Type::ARRAY:
      // Round up the number of needed bits to the next byte border.
      return( t->bitSize() + bitsPerByte - 1 ) / bitsPerByte;

    case IR_Type::STRUCT:
    case IR_Type::UNION:
      // Round up the number of needed bits to the next byte border
      return( t->bitSize() + bitsPerByte - 1 ) / bitsPerByte;

    default: {
      throw ufFatalError( "Unsupported type of global variable." );
      break;
    }
  }
};


/*
  getAlignment computes the byte-alignment of an IR type.
*/
unsigned int RV32_CodeSelector::getAlignment( const IR_Type *t ) const
{
  DSTART(
    "virtual unsigned int RV32_CodeSelector::getAlignment(const IR_Type*) "
    "const" );

  switch ( t->getType() ) {
    case IR_Type::BOOL:
      return( RVIR_CONFIGURATION->alignmentBool );

    case IR_Type::CHAR:
    case IR_Type::UNSIGNED_CHAR:
      return( RVIR_CONFIGURATION->alignmentChar );

    case IR_Type::SHORT:
    case IR_Type::UNSIGNED_SHORT:
      return( RVIR_CONFIGURATION->alignmentShort );

    case IR_Type::INT:
    case IR_Type::UNSIGNED_INT:
      return( RVIR_CONFIGURATION->alignmentInt );

    case IR_Type::LONG:
    case IR_Type::UNSIGNED_LONG:
      return( RVIR_CONFIGURATION->alignmentLong );

    case IR_Type::LONG_LONG:
    case IR_Type::UNSIGNED_LONG_LONG:
      return( RVIR_CONFIGURATION->alignmentLongLong );

    case IR_Type::LONG_DOUBLE:
      return( RVIR_CONFIGURATION->alignmentLongDouble );

    case IR_Type::DOUBLE:
      return( RVIR_CONFIGURATION->alignmentDouble );

    case IR_Type::FLOAT:
      return( RVIR_CONFIGURATION->alignmentFloat );

    case IR_Type::POINTER:
      return( RVIR_CONFIGURATION->alignmentPointer );

    case IR_Type::ARRAY: {
      auto *theArrayType = dynamic_cast<const IR_ArrayType *>( t );
      return( theArrayType->getBaseType().alignment() );
    }

    case IR_Type::STRUCT:
    case IR_Type::UNION:
      return( RVIR_CONFIGURATION->alignmentComposedType );

    default: {
      throw ufFatalError( "Unsupported type of global variable." );
      break;
    }
  }
};

}       // namespace RV32
