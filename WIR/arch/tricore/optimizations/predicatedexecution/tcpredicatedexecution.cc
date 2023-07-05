/*

   This source file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Intermediate Representation Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2015 - 2022, Heiko Falk.

   Hamburg University of Technology (TUHH)
   Institute of Embedded Systems
   21071 Hamburg
   Germany

   http://www.tuhh.de/es/esd/research/wcc

*/

/*!
  @file tcpredicatedexecution.cc
  @brief This file implements a TriCore-specific optimization that replaces
         conditionally executed arithmetical operations by predicated
         operations.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include standard headers
#include <map>
#include <iterator>
#include <set>

// Include boost headers
#include <boost/current_function.hpp>

// Include libuseful headers
#include <libuseful/debugmacros.h>

// Include WIR headers
#include <wir/wir.h>
#include <arch/tricore/tc131.h>

// Include local headers
#include "tcpredicatedexecution.h"


//
// Code section
//

namespace WIR {


using namespace std;


//
// Public class methods
//

/*
  Default constructor for system-level optimization.
*/
TC_PredicatedExecution::TC_PredicatedExecution( WIR_System &s ) :
  WIR_Optimization { s },
  mConditionalJump { nullptr },
  mConditionalReg { nullptr },
  mThenPart { nullptr },
  mCreateNegatedForms { false }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Default constructor for compilation unit-level optimization.
*/
TC_PredicatedExecution::TC_PredicatedExecution( WIR_CompilationUnit &c ) :
  WIR_Optimization { c },
  mConditionalJump { nullptr },
  mConditionalReg { nullptr },
  mThenPart { nullptr },
  mCreateNegatedForms { false }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Default constructor for function-level optimization.
*/
TC_PredicatedExecution::TC_PredicatedExecution( WIR_Function &f ) :
  WIR_Optimization { f },
  mConditionalJump { nullptr },
  mConditionalReg { nullptr },
  mThenPart { nullptr },
  mCreateNegatedForms { false }
{
  DSTART( "TC_PredicatedExecution::TC_PredicatedExecution(WIR_Function&)" );
};


/*
  Destructor.
*/
TC_PredicatedExecution::~TC_PredicatedExecution( void )
{
  DSTART( "virtual TC_PredicatedExecution::~TC_PredicatedExecution()" );
};


//
// Protected class methods
//

/*
  runOptimization performs predicated execution in the given function.
*/
void TC_PredicatedExecution::runOptimization( WIR_Function &f )
{
  DSTART(
    "virtual void TC_PredicatedExecution::runOptimization(WIR_Function&)" );

  DOUT( "Processing function '" << f.getName() << "'." << endl );

  // Iterate all basic blocks of f.
  for ( WIR_BasicBlock &b : f )
    if ( checkIf( b ) )
      optimizeIf();
};


//
// Private class methods
//

/*
  checkIf checks whether a basic block is the head of an if-then control region
  that is suitable for predicated execution.

  An if-else control region is suitable for predicated execution iff:
    - b ends with a conditional jump that depends on a Boolean condition (in
      contrast to computed, indirect jumps).
    - The then-part of the if-else region consists of a single basic block only.
    - All operations inside the then-part are suitable for predicated execution,
      i.e., are either ADD, SUB or MOV operations of appropriate formats.
*/
bool TC_PredicatedExecution::checkIf( const WIR_BasicBlock &b )
{
  DSTART( "bool TC_PredicatedExecution::checkIf(const WIR_BasicBlock&)" );

  // Check whether b is head of an if-then control region.
  WIR_BasicBlock *thenPart = nullptr;
  if ( b.getSuccessors().size() == 2 ) {
    WIR_BasicBlock &succ1 = b.getSuccessors().begin()->get();
    WIR_BasicBlock &succ2 = b.getSuccessors().rbegin()->get();

    // A small lambda to check for the presence of an if-then control region.
    auto isIf = []( const WIR_BasicBlock &b, WIR_BasicBlock &b1,
                    WIR_BasicBlock &b2 ) -> WIR_BasicBlock * {
      if ( ( b1.getPredecessors().size() == 1 ) &&
           ( b1.getPredecessors().begin()->get() == b ) &&
           ( b1.getSuccessors().size() == 1 ) &&
           ( b1.getSuccessors().begin()->get() == b2 ) &&
           ( b2.getPredecessors().size() == 2 ) )
        return( &b1 );
      else
        return( nullptr );
    };

    thenPart = isIf( b, succ1, succ2 );
    if ( thenPart == nullptr )
      thenPart = isIf( b, succ2, succ1 );
  }

  if ( thenPart == nullptr )
    return( false );

  // Determine the last non-empty instruction in b which must be the if's
  // conditional jump.
  if ( !isConditionalJump( b ) )
    return( false );

  // Finally, check whether the then-part only contains operations that are
  // suitable for predicated execution.
  if ( !isSuitableBB( *thenPart ) )
    return( false );

  mThenPart = thenPart;
  return( true );
};


/*
  isConditionalJump checks whether the last WIR operation of a basic block
  realizes a conditional jump suitable for predicated execution.

  If a suitable conditional jump is identified, it is also stored in class
  member mConditionalJump.
*/
bool TC_PredicatedExecution::isConditionalJump( const WIR_BasicBlock &b )
{
  DSTART(
    "bool TC_PredicatedExecution::isConditionalJump(const WIR_BasicBlock&)" );

  static bool initDone = false;
  static set<WIR_BaseProcessor::OpCode> cJumps;

  if ( !initDone ) {
    cJumps = {
      TC13::OpCode::JEQ, TC13::OpCode::JEQ_A, TC13::OpCode::JGE,
      TC13::OpCode::JGE_U, TC13::OpCode::JGEZ, TC13::OpCode::JGTZ,
      TC13::OpCode::JLEZ, TC13::OpCode::JLT, TC13::OpCode::JLT_U,
      TC13::OpCode::JLTZ, TC13::OpCode::JNE, TC13::OpCode::JNE_A,
      TC13::OpCode::JNZ, TC13::OpCode::JNZ_A, TC13::OpCode::JZ,
      TC13::OpCode::JZ_A };

    initDone = true;
  }

  // Determine the last non-empty instruction in b which must be the if's
  // conditional jump.
  auto it = b.rbegin();
  for ( ; it != b.rend(); ++it )
    if ( !it->get().getOperations().empty() )
      break;

  if ( it == b.rend() )
    return( false );

  WIR_Instruction &i = it->get();
  WIR_Operation &cJmp = i.getOperations().front();

  if ( !cJumps.count( cJmp.getOpCode() ) )
    return( false );

  mConditionalJump = &cJmp;
  mCreateNegatedForms = true;
  if ( ( cJmp.getOpCode() == TC13::OpCode::JZ ) ||
       ( cJmp.getOpCode() == TC13::OpCode::JZ_A ) )
    mCreateNegatedForms = false;

  DOUT(
    tricore << *mConditionalJump << " is suitable (change to " <<
    string( !mCreateNegatedForms ? "non-" : "" ) << "negated form)." << endl );

  return( true );
};


/*
  isSuitableBB checks whether all operations in a basic block are suitable for
  predicated execution.

  Suitable operations are ADD, SUB and MOV of particular formats.
*/
bool TC_PredicatedExecution::isSuitableBB( const WIR_BasicBlock &b ) const
{
  DSTART(
    "bool TC_PredicatedExecution::isSuitableBB(const WIR_BasicBlock&) const" );

  static bool initDone = false;
  static set<WIR_BaseProcessor::OpCode> opCodes;

  if ( !initDone ) {
    opCodes = {
      TC13::OpCode::ADD, TC13::OpCode::SUB, TC13::OpCode::MOV,
      TC13::OpCode::MOV_RR };

    initDone = true;
  }

  for ( WIR_Instruction &i : b )
    for ( WIR_Operation &o : i ) {
      // Check the opcodes inside b first.
      if ( !opCodes.count( o.getOpCode() ) )
        return( false );

      // For MOV, additionally check whether the immediate fits into const9
      // signed.
      if ( o.getOpCode() == TC13::OpCode::MOV ) {
        long long imm;
        auto &iParam =
          dynamic_cast<WIR_BaseImmediateParameter &>(
            o.getExplicitParameter( 2 ) );

        if ( o.getOperationFormat() == TC13::OperationFormat::SIC8_1 )
          imm = iParam.getUnsignedValue();
        else
          imm = iParam.getSignedValue();

        if ( ( imm < TC_Const9_Signed::getMinValue( 9 ) ) ||
             ( imm > TC_Const9_Signed::getMaxValue( 9 ) ) )
          return( false );
      }
    }

  DOUT( tricore << b << " is suitable." << endl );

  return( true );
};


/*
  optimizeIf performs predicated execution for a basic block identified as head
  of a suitable if-then control region.

  The optimization removes the conditional jump from the basic block and
  transforms all operations in the identified then-part into their predicated
  variants.
*/
void TC_PredicatedExecution::optimizeIf( void )
{
  DSTART( "void TC_PredicatedExecution::optimizeIf()" );

  // First, we transform the branch condition.
  transformJump();

  // Next, all operations of the identified then-part are optimized.
  transformThen();

  // Finally, erase the conditional jump.
  mConditionalJump->getInstruction().getBasicBlock().eraseInstruction(
    mConditionalJump->getInstruction().getBasicBlock().findInstruction(
      mConditionalJump->getInstruction() ) );
};


/*
  transformJump inserts a comparison operation immediately before the identified
  conditional jump.
*/
void TC_PredicatedExecution::transformJump( void )
{
  DSTART( "void TC_PredicatedExecution::transformJump()" );

  static bool initDone = false;
  static map<WIR_BaseProcessor::OpCode,
             WIR_BaseProcessor::OpCode> newOpCode;

  if ( !initDone ) {
    newOpCode = {
      { TC13::OpCode::JEQ, TC13::OpCode::EQ },
      { TC13::OpCode::JEQ_A, TC13::OpCode::EQ_A },
      { TC13::OpCode::JGE, TC13::OpCode::GE },
      { TC13::OpCode::JGE_U, TC13::OpCode::GE_U },
      { TC13::OpCode::JGEZ, TC13::OpCode::GE },
      { TC13::OpCode::JGTZ, TC13::OpCode::GE },
      { TC13::OpCode::JLEZ, TC13::OpCode::LT },
      { TC13::OpCode::JLT, TC13::OpCode::LT },
      { TC13::OpCode::JLT_U, TC13::OpCode::LT_U },
      { TC13::OpCode::JLTZ, TC13::OpCode::LT },
      { TC13::OpCode::JNE, TC13::OpCode::NE },
      { TC13::OpCode::JNE_A, TC13::OpCode::NE_A },
      { TC13::OpCode::JNZ_A, TC13::OpCode::NEZ_A },
      { TC13::OpCode::JZ_A, TC13::OpCode::NEZ_A } };

    initDone = true;
  }

  WIR_Operation &cJmp = *mConditionalJump;
  WIR_BasicBlock &b = cJmp.getInstruction().getBasicBlock();
  auto it = b.findInstruction( cJmp.getInstruction() );
  WIR_Operation *newOp = nullptr;

  // Create a new virtual register that will hold the conditional jump's
  // predicate.
  mConditionalReg =
    &(b.getFunction().pushBackVirtualRegister( new TC_DRegV() ));

  if ( ( ( cJmp.getOpCode() == TC13::OpCode::JEQ ) ||
         ( cJmp.getOpCode() == TC13::OpCode::JGE ) ||
         ( cJmp.getOpCode() == TC13::OpCode::JGE_U ) ||
         ( cJmp.getOpCode() == TC13::OpCode::JLT ) ||
         ( cJmp.getOpCode() == TC13::OpCode::JLT_U ) ||
         ( cJmp.getOpCode() == TC13::OpCode::JNE ) ) &&
       ( ( cJmp.getOperationFormat() == TC13::OperationFormat::DDL_1 ) ||
         ( cJmp.getOperationFormat() == TC13::OperationFormat::SIDL ) ) )
    // JEQ Da, Db       -> EQ vReg, Da, Da
    // JGE Da, Db       -> GE vReg, Da, Da
    // JGE.U Da, Db     -> GE.U vReg, Da, Da
    // JLT Da, Db       -> LT vReg, Da, Da
    // JLT.U Da, Db     -> LT.U vReg, Da, Da
    // JNE Da, Db       -> NE vReg, Da, Da
    newOp =
      &b.insertInstruction(
        it,
        { { newOpCode.at( cJmp.getOpCode() ), TC13::OperationFormat::DDD_1,
            new WIR_RegisterParameter( *mConditionalReg, WIR_Usage::def ),
            new WIR_RegisterParameter(
              dynamic_cast<WIR_RegisterParameter &>(
                cJmp.getExplicitParameter( 1 ) ) ),
            new WIR_RegisterParameter(
              dynamic_cast<WIR_RegisterParameter &>(
                cJmp.getExplicitParameter( 2 ) ) ) } } )->get().begin()->get();
  else

  if ( ( ( cJmp.getOpCode() == TC13::OpCode::JEQ ) ||
         ( cJmp.getOpCode() == TC13::OpCode::JGE ) ||
         ( cJmp.getOpCode() == TC13::OpCode::JGEZ ) ||
         ( cJmp.getOpCode() == TC13::OpCode::JGTZ ) ||
         ( cJmp.getOpCode() == TC13::OpCode::JLEZ ) ||
         ( cJmp.getOpCode() == TC13::OpCode::JLT ) ||
         ( cJmp.getOpCode() == TC13::OpCode::JLTZ ) ||
         ( cJmp.getOpCode() == TC13::OpCode::JNE ) ) &&
       ( ( cJmp.getOperationFormat() == TC13::OperationFormat::DC4L_1 ) ||
         ( cJmp.getOperationFormat() == TC13::OperationFormat::SIC4L ) ||
         ( cJmp.getOperationFormat() == TC13::OperationFormat::SDL ) ) )
    // JEQ Da, #imm     -> EQ vReg, Da, #imm
    // JGE Da, #imm     -> GE vReg, Da, #imm
    // JGEZ Da          -> GE vReg, Da, 0
    // JGTZ Da          -> GE vReg, Da, 1
    // JLEZ Da          -> LT vReg, Da, 1
    // JLT Da, #imm     -> LT vReg, Da, #imm
    // JLTZ Da          -> LT vReg, Da, 0
    // JNE Da, #imm     -> NE vReg, Da, #imm
    newOp =
      &b.insertInstruction(
        it,
        { { newOpCode.at( cJmp.getOpCode() ), TC13::OperationFormat::DDC9_1,
            new WIR_RegisterParameter( *mConditionalReg, WIR_Usage::def ),
            new WIR_RegisterParameter(
              dynamic_cast<WIR_RegisterParameter &>(
                cJmp.getExplicitParameter( 1 ) ) ),
            ( ( cJmp.getOpCode() == TC13::OpCode::JGEZ ) ||
              ( cJmp.getOpCode() == TC13::OpCode::JLTZ ) ) ?
              new TC_Const9_Signed( 0 ) :
              ( ( cJmp.getOpCode() == TC13::OpCode::JGTZ ) ||
                ( cJmp.getOpCode() == TC13::OpCode::JLEZ ) ) ?
                new TC_Const9_Signed( 1 ) :
                new TC_Const9_Signed(
                  dynamic_cast<WIR_BaseImmediateParameter &>(
                    cJmp.getExplicitParameter( 2 ) ).getSignedValue() ) } } )->get().begin()->get();
  else

  if ( ( ( cJmp.getOpCode() == TC13::OpCode::JGE_U ) ||
         ( cJmp.getOpCode() == TC13::OpCode::JLT_U ) ) &&
       ( cJmp.getOperationFormat() == TC13::OperationFormat::DC4L_2 ) )
    // JGE.U Da, #imm   -> GE.U vReg, Da, #imm
    // JLT.U Da, #imm   -> LT.U vReg, Da, #imm
    newOp =
      &b.insertInstruction(
        it,
        { { newOpCode.at( cJmp.getOpCode() ), TC13::OperationFormat::DDC9_2,
            new WIR_RegisterParameter( *mConditionalReg, WIR_Usage::def ),
            new WIR_RegisterParameter(
              dynamic_cast<WIR_RegisterParameter &>(
                cJmp.getExplicitParameter( 1 ) ) ),
            new TC_Const9_Unsigned(
              dynamic_cast<WIR_BaseImmediateParameter &>(
                cJmp.getExplicitParameter( 2 ) ).getUnsignedValue() ) } } )->get().begin()->get();
  else

  if ( ( cJmp.getOpCode() == TC13::OpCode::JEQ_A ) ||
       ( cJmp.getOpCode() == TC13::OpCode::JNE_A ) )
    // JEQ.A Aa, Ab     -> EQ.A vReg, Aa, Aa
    // JNE.A Aa, Ab     -> NE.A vReg, Aa, Aa
    newOp =
      &b.insertInstruction(
        it,
        { { newOpCode.at( cJmp.getOpCode() ), TC13::OperationFormat::DAA,
            new WIR_RegisterParameter( *mConditionalReg, WIR_Usage::def ),
            new WIR_RegisterParameter(
              dynamic_cast<WIR_RegisterParameter &>(
                cJmp.getExplicitParameter( 1 ) ) ),
            new WIR_RegisterParameter(
              dynamic_cast<WIR_RegisterParameter &>(
                cJmp.getExplicitParameter( 2 ) ) ) } } )->get().begin()->get();
  else

  if ( ( cJmp.getOpCode() == TC13::OpCode::JNZ_A ) ||
       ( cJmp.getOpCode() == TC13::OpCode::JZ_A ) )
    // JNZ.A Aa         -> NEZ.A vReg, Aa
    // JZ.A Aa          -> NEZ.A vReg, Aa
    newOp =
      &b.insertInstruction(
        it,
        { { newOpCode.at( cJmp.getOpCode() ), TC13::OperationFormat::DA,
            new WIR_RegisterParameter( *mConditionalReg, WIR_Usage::def ),
            new WIR_RegisterParameter(
              dynamic_cast<WIR_RegisterParameter &>(
                cJmp.getExplicitParameter( 1 ) ) ) } } )->get().begin()->get();

  if ( newOp )
    copyContainers( newOp->getInstruction(), cJmp.getInstruction() );

  DACTION(
    if ( newOp )
      DOUT( "Inserting new comparison " << tricore << *newOp << endl ) );
};


/*
  transformThen takes all operations from basic block mThenPart and transforms
  them to predicated execution.
*/
void TC_PredicatedExecution::transformThen( void )
{
  DSTART( "void TC_PredicatedExecution::transformThen()" );

  WIR_Operation &cJmp = *mConditionalJump;
  WIR_BasicBlock &b = cJmp.getInstruction().getBasicBlock();
  auto it = b.findInstruction( cJmp.getInstruction() );

  for ( WIR_Instruction &i : *mThenPart ) {
    bool twoInstrs = false;

    for ( WIR_Operation &o : i ) {
      if ( o.getOpCode() == TC13::OpCode::MOV ) {
        // MOV Da, #imm     -> SEL Da, vReg, Da, #imm
        long long imm;
        auto &iParam =
          dynamic_cast<WIR_BaseImmediateParameter &>(
            o.getExplicitParameter( 2 ) );

        if ( o.getOperationFormat() == TC13::OperationFormat::SIC8_1 )
          imm = iParam.getUnsignedValue();
        else
          imm = iParam.getSignedValue();

        b.insertInstruction(
          it,
          { { mCreateNegatedForms ? TC13::OpCode::SEL : TC13::OpCode::SELN,
              TC13::OperationFormat::DDDC9_1,
              new WIR_RegisterParameter(
                dynamic_cast<WIR_RegisterParameter &>(
                  o.getExplicitParameter( 1 ) ).getRegister(),
                WIR_Usage::def ),
              new WIR_RegisterParameter( *mConditionalReg, WIR_Usage::use ),
              new WIR_RegisterParameter(
                dynamic_cast<WIR_RegisterParameter &>(
                  o.getExplicitParameter( 1 ) ).getRegister(),
                WIR_Usage::use ),
              new TC_Const9_Signed( imm ) } } );
      } else

      if ( o.getOpCode() == TC13::OpCode::MOV_RR )
        // MOV Da, Db       -> SELN Da, vReg, Db, Da
        b.insertInstruction(
          it,
          { { mCreateNegatedForms ? TC13::OpCode::SELN : TC13::OpCode::SEL,
              TC13::OperationFormat::DDDD,
              new WIR_RegisterParameter(
                dynamic_cast<WIR_RegisterParameter &>(
                  o.getExplicitParameter( 1 ) ).getRegister(),
                WIR_Usage::def ),
              new WIR_RegisterParameter( *mConditionalReg, WIR_Usage::use ),
              new WIR_RegisterParameter(
                dynamic_cast<WIR_RegisterParameter &>(
                  o.getExplicitParameter( 2 ) ).getRegister(),
                WIR_Usage::use ),
              new WIR_RegisterParameter(
                dynamic_cast<WIR_RegisterParameter &>(
                  o.getExplicitParameter( 1 ) ).getRegister(),
                WIR_Usage::use ) } } );
      else

      if ( o.getOpCode() == TC13::OpCode::ADD ) {
        auto &newOpCode =
          mCreateNegatedForms ? TC13::OpCode::CADDN : TC13::OpCode::CADD;

        if ( o.getOperationFormat() == TC13::OperationFormat::DDC9_1 ) {
          auto &reg1 =
            dynamic_cast<WIR_RegisterParameter &>(
              o.getExplicitParameter( 1 ) ).getRegister();
          auto &reg2 =
            dynamic_cast<WIR_RegisterParameter &>(
              o.getExplicitParameter( 2 ) ).getRegister();

          if ( reg1 == reg2 )
            // ADD Da, Da, #imm -> CADD Da, vReg, Da, #imm
            b.insertInstruction(
              it,
              { { newOpCode, TC13::OperationFormat::DDDC9_1,
                  new WIR_RegisterParameter( reg1, WIR_Usage::def ),
                  new WIR_RegisterParameter( *mConditionalReg, WIR_Usage::use ),
                  new WIR_RegisterParameter( reg2, WIR_Usage::use ),
                  new TC_Const9_Signed(
                    dynamic_cast<TC_Const9_Signed &>(
                      o.getExplicitParameter( 3 ) ) ) } } );
          else {
            // ADD Da, Db, #imm -> ADD tmp, Db, #imm
            //                     SEL Da, vReg, tmp, Da
            twoInstrs = true;
            auto &tmpReg =
              b.getFunction().pushBackVirtualRegister( new TC_DRegV() );
            b.insertInstruction(
              it,
              { { TC13::OpCode::ADD, TC13::OperationFormat::DDC9_1,
                  new WIR_RegisterParameter( tmpReg, WIR_Usage::def ),
                  new WIR_RegisterParameter( reg2, WIR_Usage::use ),
                  new TC_Const9_Signed(
                    dynamic_cast<TC_Const9_Signed &>(
                      o.getExplicitParameter( 3 ) ) ) } } );
            b.insertInstruction(
              it,
              { { mCreateNegatedForms ? TC13::OpCode::SELN : TC13::OpCode::SEL,
                  TC13::OperationFormat::DDDD,
                  new WIR_RegisterParameter( reg1, WIR_Usage::def ),
                  new WIR_RegisterParameter( *mConditionalReg, WIR_Usage::use ),
                  new WIR_RegisterParameter( tmpReg, WIR_Usage::use ),
                  new WIR_RegisterParameter( reg1, WIR_Usage::use ) } } );
          }
        } else

        if ( o.getOperationFormat() == TC13::OperationFormat::DDD_1 ) {
          auto &reg1 =
            dynamic_cast<WIR_RegisterParameter &>(
              o.getExplicitParameter( 1 ) ).getRegister();
          auto &reg2 =
            dynamic_cast<WIR_RegisterParameter &>(
              o.getExplicitParameter( 2 ) ).getRegister();
          auto &reg3 =
            dynamic_cast<WIR_RegisterParameter &>(
              o.getExplicitParameter( 3 ) ).getRegister();

          if ( reg1 == reg2 )
            // ADD Da, Da, Db   -> CADD Da, vReg, Da, Db
            b.insertInstruction(
              it,
              { { newOpCode, TC13::OperationFormat::DDDD,
                  new WIR_RegisterParameter( reg1, WIR_Usage::def ),
                  new WIR_RegisterParameter( *mConditionalReg, WIR_Usage::use ),
                  new WIR_RegisterParameter( reg2, WIR_Usage::use ),
                  new WIR_RegisterParameter( reg3, WIR_Usage::use ) } } );
          else

          if ( reg1 == reg3 )
            // ADD Da, Db, Da   -> CADD Da, vReg, Da, Db
            b.insertInstruction(
              it,
              { { newOpCode, TC13::OperationFormat::DDDD,
                  new WIR_RegisterParameter( reg1, WIR_Usage::def ),
                  new WIR_RegisterParameter( *mConditionalReg, WIR_Usage::use ),
                  new WIR_RegisterParameter( reg3, WIR_Usage::use ),
                  new WIR_RegisterParameter( reg2, WIR_Usage::use ) } } );
          else {
            // ADD Da, Db, Dc   -> ADD tmp, Db, Dc
            //                     SEL Da, vReg, tmp, Da
            twoInstrs = true;
            auto &tmpReg =
              b.getFunction().pushBackVirtualRegister( new TC_DRegV() );
            b.insertInstruction(
              it,
              { { TC13::OpCode::ADD, TC13::OperationFormat::DDD_1,
                  new WIR_RegisterParameter( tmpReg, WIR_Usage::def ),
                  new WIR_RegisterParameter( reg2, WIR_Usage::use ),
                  new WIR_RegisterParameter(
                    dynamic_cast<WIR_RegisterParameter &>(
                      o.getExplicitParameter( 3 ) ) ) } } );
            b.insertInstruction(
              it,
              { { mCreateNegatedForms ? TC13::OpCode::SELN : TC13::OpCode::SEL,
                  TC13::OperationFormat::DDDD,
                  new WIR_RegisterParameter( reg1, WIR_Usage::def ),
                  new WIR_RegisterParameter( *mConditionalReg, WIR_Usage::use ),
                  new WIR_RegisterParameter( tmpReg, WIR_Usage::use ),
                  new WIR_RegisterParameter( reg1, WIR_Usage::use ) } } );
          }
        } else

        if ( o.getOperationFormat() == TC13::OperationFormat::SDC4_2 ) {
          auto &reg1 =
            dynamic_cast<WIR_RegisterParameter &>(
              o.getExplicitParameter( 1 ) ).getRegister();

          // ADD Da, #imm     -> CADD Da, vReg, Da, #imm
          b.insertInstruction(
            it,
            { { newOpCode, TC13::OperationFormat::DDDC9_1,
                new WIR_RegisterParameter( reg1, WIR_Usage::def ),
                new WIR_RegisterParameter( *mConditionalReg, WIR_Usage::use ),
                new WIR_RegisterParameter( reg1, WIR_Usage::use ),
                new TC_Const9_Signed(
                  dynamic_cast<TC_Const4_Signed &>(
                    o.getExplicitParameter( 2 ) ).getSignedValue() ) } } );
        } else

        if ( o.getOperationFormat() == TC13::OperationFormat::SDIC4_2 ) {
          auto &reg1 =
            dynamic_cast<WIR_RegisterParameter &>(
              o.getExplicitParameter( 1 ) ).getRegister();

          if ( TC13::isD15( reg1 ) )
            // ADD D15, D15, #imm -> CADD D15, vReg, D15, #imm
            b.insertInstruction(
              it,
              { { newOpCode, TC13::OperationFormat::DDDC9_1,
                  new WIR_RegisterParameter( reg1, WIR_Usage::def ),
                  new WIR_RegisterParameter( *mConditionalReg, WIR_Usage::use ),
                  new WIR_RegisterParameter( reg1, WIR_Usage::use ),
                  new TC_Const9_Signed(
                    dynamic_cast<TC_Const4_Signed &>(
                      o.getExplicitParameter( 3 ) ).getSignedValue() ) } } );
          else {
            // ADD Da, D15, Dc   -> ADD tmp, D15, Dc
            //                      SEL Da, vReg, tmp, Da
            twoInstrs = true;
            auto &tmpReg =
              b.getFunction().pushBackVirtualRegister( new TC_DRegV() );
            b.insertInstruction(
              it,
              { { TC13::OpCode::ADD, TC13::OperationFormat::DDD_1,
                  new WIR_RegisterParameter( tmpReg, WIR_Usage::def ),
                  new WIR_RegisterParameter(
                    dynamic_cast<WIR_RegisterParameter &>(
                      o.getExplicitParameter( 2 ) ) ),
                  new WIR_RegisterParameter(
                    dynamic_cast<WIR_RegisterParameter &>(
                      o.getExplicitParameter( 3 ) ) ) } } );
            b.insertInstruction(
              it,
              { { mCreateNegatedForms ? TC13::OpCode::SELN : TC13::OpCode::SEL,
                  TC13::OperationFormat::DDDD,
                  new WIR_RegisterParameter( reg1, WIR_Usage::def ),
                  new WIR_RegisterParameter( *mConditionalReg, WIR_Usage::use ),
                  new WIR_RegisterParameter( tmpReg, WIR_Usage::use ),
                  new WIR_RegisterParameter( reg1, WIR_Usage::use ) } } );
          }
        } else

        if ( o.getOperationFormat() == TC13::OperationFormat::SIDC4 ) {
          auto &reg2 =
            dynamic_cast<WIR_RegisterParameter &>(
              o.getExplicitParameter( 2 ) ).getRegister();

          if ( TC13::isD15( reg2 ) )
            // ADD D15, D15, #imm -> CADD D15, vReg, D15, #imm
            b.insertInstruction(
              it,
              { { newOpCode, TC13::OperationFormat::DDDC9_1,
                  new WIR_RegisterParameter( reg2, WIR_Usage::def ),
                  new WIR_RegisterParameter( *mConditionalReg, WIR_Usage::use ),
                  new WIR_RegisterParameter( reg2, WIR_Usage::use ),
                  new TC_Const9_Signed(
                    dynamic_cast<TC_Const4_Signed &>(
                      o.getExplicitParameter( 3 ) ).getSignedValue() ) } } );
          else {
            // ADD D15, Da, Dc   -> ADD tmp, Da, Dc
            //                      SEL D15, vReg, tmp, D15
            twoInstrs = true;
            auto &tmpReg =
              b.getFunction().pushBackVirtualRegister( new TC_DRegV() );
            b.insertInstruction(
              it,
              { { TC13::OpCode::ADD, TC13::OperationFormat::DDD_1,
                  new WIR_RegisterParameter( tmpReg, WIR_Usage::def ),
                  new WIR_RegisterParameter(
                    dynamic_cast<WIR_RegisterParameter &>(
                      o.getExplicitParameter( 2 ) ) ),
                  new WIR_RegisterParameter(
                    dynamic_cast<WIR_RegisterParameter &>(
                      o.getExplicitParameter( 3 ) ) ) } } );
            b.insertInstruction(
              it,
              { { mCreateNegatedForms ? TC13::OpCode::SELN : TC13::OpCode::SEL,
                  TC13::OperationFormat::DDDD,
                  new WIR_RegisterParameter(
                    dynamic_cast<WIR_RegisterParameter &>(
                      o.getExplicitParameter( 1 ) ).getRegister(),
                    WIR_Usage::def ),
                  new WIR_RegisterParameter( *mConditionalReg, WIR_Usage::use ),
                  new WIR_RegisterParameter( tmpReg, WIR_Usage::use ),
                  new WIR_RegisterParameter(
                    dynamic_cast<WIR_RegisterParameter &>(
                      o.getExplicitParameter( 1 ) ).getRegister(),
                    WIR_Usage::use ) } } );
          }
        } else

        if ( o.getOperationFormat() == TC13::OperationFormat::SDD_2 ) {
          auto &reg1 =
            dynamic_cast<WIR_RegisterParameter &>(
              o.getExplicitParameter( 1 ) ).getRegister();
          auto &reg2 =
            dynamic_cast<WIR_RegisterParameter &>(
              o.getExplicitParameter( 2 ) ).getRegister();

          // ADD Da, Db       -> CADD Da, vReg, Da, Db
          b.insertInstruction(
            it,
            { { newOpCode, TC13::OperationFormat::DDDD,
                new WIR_RegisterParameter( reg1, WIR_Usage::def ),
                new WIR_RegisterParameter( *mConditionalReg, WIR_Usage::use ),
                new WIR_RegisterParameter( reg1, WIR_Usage::use ),
                new WIR_RegisterParameter( reg2, WIR_Usage::use ) } } );
        } else

        if ( o.getOperationFormat() == TC13::OperationFormat::SDID_1 ) {
          auto &reg1 =
            dynamic_cast<WIR_RegisterParameter &>(
              o.getExplicitParameter( 1 ) ).getRegister();
          auto &reg2 =
            dynamic_cast<WIR_RegisterParameter &>(
              o.getExplicitParameter( 2 ) ).getRegister();
          auto &reg3 =
            dynamic_cast<WIR_RegisterParameter &>(
              o.getExplicitParameter( 3 ) ).getRegister();

          if ( TC13::isD15( reg1 ) )
            // ADD D15, D15, Dc   -> CADD D15, vReg, D15, Dc
            b.insertInstruction(
              it,
              { { newOpCode, TC13::OperationFormat::DDDD,
                  new WIR_RegisterParameter( reg1, WIR_Usage::def ),
                  new WIR_RegisterParameter( *mConditionalReg, WIR_Usage::use ),
                  new WIR_RegisterParameter( reg2, WIR_Usage::use ),
                  new WIR_RegisterParameter( reg3, WIR_Usage::use ) } } );
          else

          if ( reg1 == reg3 )
            // ADD Da, D15, Da   -> CADD Da, vReg, Da, D15
            b.insertInstruction(
              it,
              { { newOpCode, TC13::OperationFormat::DDDD,
                  new WIR_RegisterParameter( reg1, WIR_Usage::def ),
                  new WIR_RegisterParameter( *mConditionalReg, WIR_Usage::use ),
                  new WIR_RegisterParameter( reg3, WIR_Usage::use ),
                  new WIR_RegisterParameter( reg2, WIR_Usage::use ) } } );
          else {
            // ADD Da, D15, Dc  -> ADD tmp, D15, Dc
            //                     SEL Da, vReg, tmp, Da
            twoInstrs = true;
            auto &tmpReg =
              b.getFunction().pushBackVirtualRegister( new TC_DRegV() );
            b.insertInstruction(
              it,
              { { TC13::OpCode::ADD, TC13::OperationFormat::DDD_1,
                  new WIR_RegisterParameter( tmpReg, WIR_Usage::def ),
                  new WIR_RegisterParameter(
                    dynamic_cast<WIR_RegisterParameter &>(
                      o.getExplicitParameter( 2 ) ) ),
                  new WIR_RegisterParameter(
                    dynamic_cast<WIR_RegisterParameter &>(
                      o.getExplicitParameter( 3 ) ) ) } } );
            b.insertInstruction(
              it,
              { { mCreateNegatedForms ? TC13::OpCode::SELN : TC13::OpCode::SEL,
                  TC13::OperationFormat::DDDD,
                  new WIR_RegisterParameter( reg1, WIR_Usage::def ),
                  new WIR_RegisterParameter( *mConditionalReg, WIR_Usage::use ),
                  new WIR_RegisterParameter( tmpReg, WIR_Usage::use ),
                  new WIR_RegisterParameter( reg1, WIR_Usage::use ) } } );
          }
        } else

        if ( o.getOperationFormat() == TC13::OperationFormat::SIDD ) {
          auto &reg1 =
            dynamic_cast<WIR_RegisterParameter &>(
              o.getExplicitParameter( 1 ) ).getRegister();
          auto &reg2 =
            dynamic_cast<WIR_RegisterParameter &>(
              o.getExplicitParameter( 2 ) ).getRegister();
          auto &reg3 =
            dynamic_cast<WIR_RegisterParameter &>(
              o.getExplicitParameter( 3 ) ).getRegister();

          if ( TC13::isD15( reg2 ) )
            // ADD D15, D15, Dc   -> CADD D15, vReg, D15, Dc
            b.insertInstruction(
              it,
              { { newOpCode, TC13::OperationFormat::DDDD,
                  new WIR_RegisterParameter( reg1, WIR_Usage::def ),
                  new WIR_RegisterParameter( *mConditionalReg, WIR_Usage::use ),
                  new WIR_RegisterParameter( reg2, WIR_Usage::use ),
                  new WIR_RegisterParameter( reg3, WIR_Usage::use ) } } );
          else

          if ( TC13::isD15( reg3 ) )
            // ADD D15, Da, D15   -> CADD D15, vReg, D15, Da
            b.insertInstruction(
              it,
              { { newOpCode, TC13::OperationFormat::DDDD,
                  new WIR_RegisterParameter( reg1, WIR_Usage::def ),
                  new WIR_RegisterParameter( *mConditionalReg, WIR_Usage::use ),
                  new WIR_RegisterParameter( reg3, WIR_Usage::use ),
                  new WIR_RegisterParameter( reg2, WIR_Usage::use ) } } );
          else {
            // ADD D15, Db, Dc  -> ADD tmp, Db, Dc
            //                     SEL D15, vReg, tmp, D15
            twoInstrs = true;
            auto &tmpReg =
              b.getFunction().pushBackVirtualRegister( new TC_DRegV() );
            b.insertInstruction(
              it,
              { { TC13::OpCode::ADD, TC13::OperationFormat::DDD_1,
                  new WIR_RegisterParameter( tmpReg, WIR_Usage::def ),
                  new WIR_RegisterParameter(
                    dynamic_cast<WIR_RegisterParameter &>(
                      o.getExplicitParameter( 2 ) ) ),
                  new WIR_RegisterParameter(
                    dynamic_cast<WIR_RegisterParameter &>(
                      o.getExplicitParameter( 3 ) ) ) } } );
            b.insertInstruction(
              it,
              { { mCreateNegatedForms ? TC13::OpCode::SELN : TC13::OpCode::SEL,
                  TC13::OperationFormat::DDDD,
                  new WIR_RegisterParameter( reg1, WIR_Usage::def ),
                  new WIR_RegisterParameter( *mConditionalReg, WIR_Usage::use ),
                  new WIR_RegisterParameter( tmpReg, WIR_Usage::use ),
                  new WIR_RegisterParameter( reg1, WIR_Usage::use ) } } );
          }
        }
      } else

      if ( o.getOpCode() == TC13::OpCode::SUB ) {
        auto &newOpCode =
          mCreateNegatedForms ? TC13::OpCode::CSUBN : TC13::OpCode::CSUB;

        if ( o.getOperationFormat() == TC13::OperationFormat::DDD_1 ) {
          auto &reg1 =
            dynamic_cast<WIR_RegisterParameter &>(
              o.getExplicitParameter( 1 ) ).getRegister();
          auto &reg2 =
            dynamic_cast<WIR_RegisterParameter &>(
              o.getExplicitParameter( 2 ) ).getRegister();
          auto &reg3 =
            dynamic_cast<WIR_RegisterParameter &>(
              o.getExplicitParameter( 3 ) ).getRegister();

          if ( reg1 == reg2 )
            // SUB Da, Da, Db   -> CSUB Da, vReg, Da, Db
            b.insertInstruction(
              it,
              { { newOpCode, TC13::OperationFormat::DDDD,
                  new WIR_RegisterParameter( reg1, WIR_Usage::def ),
                  new WIR_RegisterParameter( *mConditionalReg, WIR_Usage::use ),
                  new WIR_RegisterParameter( reg2, WIR_Usage::use ),
                  new WIR_RegisterParameter( reg3, WIR_Usage::use ) } } );
          else {
            // SUB Da, Db, Dc   -> SUB tmp, Db, Dc
            //                     SEL Da, vReg, tmp, Da
            twoInstrs = true;
            auto &tmpReg =
              b.getFunction().pushBackVirtualRegister( new TC_DRegV() );
            b.insertInstruction(
              it,
              { { TC13::OpCode::SUB, TC13::OperationFormat::DDD_1,
                  new WIR_RegisterParameter( tmpReg, WIR_Usage::def ),
                  new WIR_RegisterParameter( reg2, WIR_Usage::use ),
                  new WIR_RegisterParameter( reg3, WIR_Usage::use ) } } );
            b.insertInstruction(
              it,
              { { mCreateNegatedForms ? TC13::OpCode::SELN : TC13::OpCode::SEL,
                  TC13::OperationFormat::DDDD,
                  new WIR_RegisterParameter( reg1, WIR_Usage::def ),
                  new WIR_RegisterParameter( *mConditionalReg, WIR_Usage::use ),
                  new WIR_RegisterParameter( tmpReg, WIR_Usage::use ),
                  new WIR_RegisterParameter( reg1, WIR_Usage::use ) } } );
          }
        } else

        if ( o.getOperationFormat() == TC13::OperationFormat::SDD_2 ) {
          auto &reg1 =
            dynamic_cast<WIR_RegisterParameter &>(
              o.getExplicitParameter( 1 ) ).getRegister();
          auto &reg2 =
            dynamic_cast<WIR_RegisterParameter &>(
              o.getExplicitParameter( 2 ) ).getRegister();

          // SUB Da, Db       -> CSUB Da, vReg, Da, Db
          b.insertInstruction(
            it,
            { { newOpCode, TC13::OperationFormat::DDDD,
                new WIR_RegisterParameter( reg1, WIR_Usage::def ),
                new WIR_RegisterParameter( *mConditionalReg, WIR_Usage::use ),
                new WIR_RegisterParameter( reg1, WIR_Usage::use ),
                new WIR_RegisterParameter( reg2, WIR_Usage::use ) } } );
        } else

        if ( o.getOperationFormat() == TC13::OperationFormat::SDID_1 ) {
          auto &reg1 =
            dynamic_cast<WIR_RegisterParameter &>(
              o.getExplicitParameter( 1 ) ).getRegister();
          auto &reg2 =
            dynamic_cast<WIR_RegisterParameter &>(
              o.getExplicitParameter( 2 ) ).getRegister();
          auto &reg3 =
            dynamic_cast<WIR_RegisterParameter &>(
              o.getExplicitParameter( 3 ) ).getRegister();

          if ( TC13::isD15( reg1 ) )
            // SUB D15, D15, Dc   -> CSUB D15, vReg, D15, Dc
            b.insertInstruction(
              it,
              { { newOpCode, TC13::OperationFormat::DDDD,
                  new WIR_RegisterParameter( reg1, WIR_Usage::def ),
                  new WIR_RegisterParameter( *mConditionalReg, WIR_Usage::use ),
                  new WIR_RegisterParameter( reg2, WIR_Usage::use ),
                  new WIR_RegisterParameter( reg3, WIR_Usage::use ) } } );
          else {
            // SUB Da, D15, Dc  -> SUB tmp, D15, Dc
            //                     SEL Da, vReg, tmp, Da
            twoInstrs = true;
            auto &tmpReg =
              b.getFunction().pushBackVirtualRegister( new TC_DRegV() );
            b.insertInstruction(
              it,
              { { TC13::OpCode::SUB, TC13::OperationFormat::DDD_1,
                  new WIR_RegisterParameter( tmpReg, WIR_Usage::def ),
                  new WIR_RegisterParameter( reg2, WIR_Usage::use ),
                  new WIR_RegisterParameter( reg3, WIR_Usage::use ) } } );
            b.insertInstruction(
              it,
              { { mCreateNegatedForms ? TC13::OpCode::SELN : TC13::OpCode::SEL,
                  TC13::OperationFormat::DDDD,
                  new WIR_RegisterParameter( reg1, WIR_Usage::def ),
                  new WIR_RegisterParameter( *mConditionalReg, WIR_Usage::use ),
                  new WIR_RegisterParameter( tmpReg, WIR_Usage::use ),
                  new WIR_RegisterParameter( reg1, WIR_Usage::use ) } } );
          }
        } else

        if ( o.getOperationFormat() == TC13::OperationFormat::SIDD ) {
          auto &reg1 =
            dynamic_cast<WIR_RegisterParameter &>(
              o.getExplicitParameter( 1 ) ).getRegister();
          auto &reg2 =
            dynamic_cast<WIR_RegisterParameter &>(
              o.getExplicitParameter( 2 ) ).getRegister();
          auto &reg3 =
            dynamic_cast<WIR_RegisterParameter &>(
              o.getExplicitParameter( 3 ) ).getRegister();

          if ( TC13::isD15( reg2 ) )
            // SUB D15, D15, Dc   -> CSUB D15, vReg, D15, Dc
            b.insertInstruction(
              it,
              { { newOpCode, TC13::OperationFormat::DDDD,
                  new WIR_RegisterParameter( reg1, WIR_Usage::def ),
                  new WIR_RegisterParameter( *mConditionalReg, WIR_Usage::use ),
                  new WIR_RegisterParameter( reg2, WIR_Usage::use ),
                  new WIR_RegisterParameter( reg3, WIR_Usage::use ) } } );
          else {
            // SUB D15, Db, Dc  -> SUB tmp, Db, Dc
            //                     SEL D15, vReg, tmp, D15
            twoInstrs = true;
            auto &tmpReg =
              b.getFunction().pushBackVirtualRegister( new TC_DRegV() );
            b.insertInstruction(
              it,
              { { TC13::OpCode::SUB, TC13::OperationFormat::DDD_1,
                  new WIR_RegisterParameter( tmpReg, WIR_Usage::def ),
                  new WIR_RegisterParameter( reg2, WIR_Usage::use ),
                  new WIR_RegisterParameter( reg3, WIR_Usage::use ) } } );
            b.insertInstruction(
              it,
              { { mCreateNegatedForms ? TC13::OpCode::SELN : TC13::OpCode::SEL,
                  TC13::OperationFormat::DDDD,
                  new WIR_RegisterParameter( reg1, WIR_Usage::def ),
                  new WIR_RegisterParameter( *mConditionalReg, WIR_Usage::use ),
                  new WIR_RegisterParameter( tmpReg, WIR_Usage::use ),
                  new WIR_RegisterParameter( reg1, WIR_Usage::use ) } } );
          }
        }
      }
    }

    copyContainers( *prev( it ), i );
    if ( twoInstrs )
      copyContainers( *prev( prev( it ) ), i );
  }

  mThenPart->clearInstructions();
};

}       // namespace WIR
