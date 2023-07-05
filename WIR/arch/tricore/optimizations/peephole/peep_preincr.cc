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
  @file peep_preincr.cc
  @brief This file implements a peephole optimizer for
         <TT>ADD.A</TT>/<TT>LEA</TT>/<TT>SUB.A</TT> operations followed by a
         <TT>LD</TT>/<TT>ST</TT> using base+offset addressing with an offset of
         0.

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
#include <set>

// Include libuseful headers
#include <libuseful/debugmacros.h>

// Include WIR headers
#include <wir/wir.h>
#include <arch/tricore/tc131.h>
#include <arch/tricore/analyses/bit/tcbitdfa.h>

// Include local headers
#include "peep_preincr.h"


//
// Code section
//

namespace WIR {


using namespace std;


//
// Public class methods
//

/*
  Default constructor.
*/
TC_Peep_PreIncr::TC_Peep_PreIncr( WIR_Function &f ) :
  WIR_Optimization { f },
  WIR_BitOpt { f },
  WIR_Peephole { f },
  mFoundIncr { nullptr },
  mIncrement { 0 },
  mSingleAReg { false },
  mReachingDefsAnalyzed { false },
  mAReg { nullptr }
{
  DSTART( "TC_Peep_PreIncr::TC_Peep_PreIncr(WIR_Function&)" );

  addPeepholeSize( 1 );
};


/*
  Destructor.
*/
TC_Peep_PreIncr::~TC_Peep_PreIncr( void )
{
  DSTART( "virtual TC_Peep_PreIncr::~TC_Peep_PreIncr()" );
};


/*
  optimize performs the peephole optimization.
*/
void TC_Peep_PreIncr::optimize( void )
{
  DSTART( "void TC_Peep_PreIncr::optimize()" );

  WIR_Peephole::optimize();
};


//
// Protected class methods
//

/*
  runOptimization performs peephole optimization in the given function.
*/
void TC_Peep_PreIncr::runOptimization( WIR_Function &f )
{
  DSTART( "virtual void TC_Peep_PreIncr::runOptimization(WIR_Function&)" );

  TC_BitDFA analyzer { f };
  setDFA( analyzer );

  // Perform bit-true data and value flow analysis first.
  if ( mRunDFA && ( mBitDFA != nullptr ) )
    mBitDFA->analyze();

  WIR_Peephole::runOptimization( f );

  // Update collected location bits.
  updateLocations( f );
  verifyLocations( f );
};


/*
  matchPeephole determines whether the specified peephole matches with the
  operation pattern ADD.A/LEA/SUB.A, LD/ST.
*/
bool TC_Peep_PreIncr::matchPeephole( const WIR_Peephole::peephole &p )
{
  DSTART( "virtual bool TC_Peep_PreIncr::matchPeephole(const peephole&)" );

  static bool initDone = false;
  static set<WIR_BaseProcessor::OpCode> memAcc;
  static set<WIR_BaseProcessor::OpCode> incrOp;
  static map<WIR_BaseProcessor::OperationFormat, unsigned int> boaAddressing;
  static set<WIR_BaseProcessor::OperationFormat> incrFormats;

  if ( !initDone ) {
    memAcc = {
      TC13::OpCode::LD_A, TC13::OpCode::LD_B, TC13::OpCode::LD_BU,
      TC13::OpCode::LD_D, TC13::OpCode::LD_DA, TC13::OpCode::LD_H,
      TC13::OpCode::LD_HU, TC13::OpCode::LD_Q, TC13::OpCode::LD_W,
      TC13::OpCode::LEA, TC13::OpCode::ST_A, TC13::OpCode::ST_B,
      TC13::OpCode::ST_D, TC13::OpCode::ST_DA, TC13::OpCode::ST_H,
      TC13::OpCode::ST_Q, TC13::OpCode::ST_W, TC13::OpCode::SWAP_W };

    incrOp = {
      TC13::OpCode::ADD_A, TC13::OpCode::LEA, TC13::OpCode::SUB_A };

    boaAddressing = {
      { TC13::OperationFormat::AAC10BOA, 2 },
      { TC13::OperationFormat::AAC16BOA, 2 },
      { TC13::OperationFormat::DAC10BOA, 2 },
      { TC13::OperationFormat::EAC10BOA, 2 },
      { TC13::OperationFormat::PAC10BOA, 2 },
      { TC13::OperationFormat::DAC16BOA, 2 },
      { TC13::OperationFormat::AC10ABOA, 1 },
      { TC13::OperationFormat::AC10DBOA_1, 1 },
      { TC13::OperationFormat::AC10EBOA, 1 },
      { TC13::OperationFormat::AC10PBOA, 1 },
      { TC13::OperationFormat::AC16DBOA, 1 },
      { TC13::OperationFormat::AC10DBOA_2, 1 },
      { TC13::OperationFormat::SISPC10_1, 2 },
      { TC13::OperationFormat::SAA_2, 2 },
      { TC13::OperationFormat::SAIC4, 2 },
      { TC13::OperationFormat::SIAC4_1, 2 },
      { TC13::OperationFormat::SDA_2, 2 },
      { TC13::OperationFormat::SDIC4_1, 2 },
      { TC13::OperationFormat::SIAC4_2, 2 },
      { TC13::OperationFormat::SISPC10_2, 2 },
      { TC13::OperationFormat::SSPC10I_1, 1 },
      { TC13::OperationFormat::SAC4I_1, 1 },
      { TC13::OperationFormat::SAA_4, 1 },
      { TC13::OperationFormat::SIC4A, 1 },
      { TC13::OperationFormat::SAC4I_2, 1 },
      { TC13::OperationFormat::SAD_2, 1 },
      { TC13::OperationFormat::SIC4D, 1 },
      { TC13::OperationFormat::SSPC10I_2, 1 } };

    incrFormats = {
      TC13::OperationFormat::SAC4_2, TC13::OperationFormat::AAC10BOA,
      TC13::OperationFormat::AAC16BOA, TC13::OperationFormat::SSPC8 };

    initDone = true;
  }

  mFoundIncr = nullptr;
  WIR_Operation &o0 = p[ 0 ]->get().begin()->get();
  mAReg = nullptr;

  // Check whether the peephole is a memory access with appropriate format.
  auto itOpCode = memAcc.find( o0.getOpCode() );
  auto itOpFormat = boaAddressing.find( o0.getOperationFormat() );
  if ( ( itOpCode == memAcc.end() ) || ( itOpFormat == boaAddressing.end() ) )
    return( false );

  // Determine the address register and offset involved in the base+offset
  // addressing.
  auto &aRegParam =
    dynamic_cast<WIR_RegisterParameter &>(
      o0.getExplicitParameter( itOpFormat->second ) );
  long long offset = 0;
  if ( o0.getExplicitParameters().size() != 2 ) {
    if ( o0.getSize() != 2 )
      offset = dynamic_cast<WIR_BaseImmediateParameter &>(
          o0.getExplicitParameter( itOpFormat->second + 1 ) ).getSignedValue();
    else
      offset = dynamic_cast<WIR_BaseImmediateParameter &>(
          o0.getExplicitParameter(
              itOpFormat->second + 1 ) ).getUnsignedValue();
  }
  mAReg = &(aRegParam.getRegister());

  // Determine the bitValue container of the address register parameter.
  auto &cRegParam = aRegParam.getContainers<WIR_BitValues>().begin()->get();

  // There must be only 1 incoming edge for the address register parameter.
  if ( cRegParam.getInValues().size() != 1 )
    return( false );

  auto &incrParam = *(cRegParam.getInValues().begin()->rp);
  auto &incr = incrParam.getOperation();

  if ( incr == o0 )
    return( false );

  // Check whether the operation is an address increment of appropriate format.
  auto itIncr = incrOp.find( incr.getOpCode() );
  auto itIncrFormat = incrFormats.find( incr.getOperationFormat() );
  if ( ( itIncr == incrOp.end() ) || ( itIncrFormat == incrFormats.end() ) )
    return( false );

  // Check whether a single address register is involved in the increment.
  mSingleAReg =
    ( ( incr.getOpCode() == TC13::OpCode::LEA ) &&
      ( dynamic_cast<WIR_RegisterParameter &>(
          incr.getExplicitParameter( 1 ) ).getRegister() !=
        dynamic_cast<WIR_RegisterParameter &>(
          incr.getExplicitParameter( 2 ) ).getRegister() ) ) ? false : true;

  mIncrement =
    ( incr.getOpCode() == TC13::OpCode::LEA ) ?
      dynamic_cast<WIR_BaseImmediateParameter &>(
        incr.getExplicitParameter( 3 ) ).getSignedValue() :
      dynamic_cast<WIR_BaseImmediateParameter &>(
        incr.getExplicitParameter( 2 ) ).getSignedValue();

  if ( !mSingleAReg )
    mIncrement += offset;

  auto &newF = getNewOperationFormat( o0 );
  if ( ( newF == TC13::OperationFormat::AAC16BOA ) ||
       ( newF == TC13::OperationFormat::DAC16BOA ) ||
       ( newF == TC13::OperationFormat::AC16DBOA ) ) {
    if ( ( mIncrement < TC_Const16_Signed::getMinValue( 16 ) ) ||
         ( mIncrement > TC_Const16_Signed::getMaxValue( 16 ) ) )
      return( false );
  } else {
    if ( ( mIncrement < TC_Const10_Signed::getMinValue( 10 ) ) ||
         ( mIncrement > TC_Const10_Signed::getMaxValue( 10 ) ) )
      return( false );
  }

  if ( !mSingleAReg ||
       ( mSingleAReg && ( offset == 0 ) &&
         ( o0.getOpCode() != TC13::OpCode::LEA ) &&
         ( incrParam.getContainers<WIR_BitValues>().begin()->get().
             getOutValues().size() == 1 ) ) )
    mFoundIncr = &incr;

  // Finally, we need to ensure that if several address registers are involved
  // in an increment, the originally incremented address register is not
  // redefined between the increment and the memory access.
  if ( !mSingleAReg ) {
    // Do a reaching definitions analysis first, if necessary.
    if ( !mReachingDefsAnalyzed ) {
      WIR_ReachingDefinitionsAnalysis analyzer
        { o0.getInstruction().getBasicBlock().getFunction() };
      analyzer.analyze();
    }

    WIR_RegisterParameterSet defs;
    WIR_BaseRegister &aReg =
      dynamic_cast<WIR_RegisterParameter &>(
        incr.getExplicitParameter( 2 ) ).getRegister();
    auto &cont =
      incr.getExplicitParameter( 2 ).getContainers<WIR_BitValues>().
        begin()->get();
    for ( auto &inEdge : cont.getInValues() )
      defs.insert( dynamic_cast<WIR_RegisterParameter &>( *(inEdge.rp) ) );

    auto &reachingDefs =
      o0.getInstruction().getContainers<WIR_ReachingDefinitions>().
        begin()->get();
    for ( WIR_RegisterParameter &rDef : reachingDefs.getReachingDefinitions() )
      if ( defs.count( rDef ) )
        defs.erase( rDef );
      else
        for ( WIR_BaseRegister &leaf : rDef.getRegister().getLeafs() )
          if ( leaf == aReg )
            return( false );
  }

  return( mFoundIncr != nullptr );
};


/*
  transformPeephole optimizes the operation pattern ADD.A/LEA/SUB.A,
  LD/ST in the specified peephole.
*/
void TC_Peep_PreIncr::transformPeephole( const WIR_Peephole::peephole &p )
{
  DSTART( "virtual void TC_Peep_PreIncr::transformPeephole(const peephole&)" );

  // A small lambda to copy incoming bit value data.
  auto copyInEdges = [&]( WIR_RegisterParameter *from,
                          WIR_RegisterParameter *to ) {
    auto &cFrom = from->getContainers<WIR_BitValues>().begin()->get();
    if ( !to->containsContainers( WIR_BitValues::getContainerTypeID() ) )
      to->insertContainer( new WIR_BitValues() );
    else
      to->getContainers<WIR_BitValues>().begin()->get().clearInValues();
    auto &cTo = to->getContainers<WIR_BitValues>().begin()->get();

    for ( auto &inEdge : cFrom.getInValues() ) {
      cTo.insertInValues(
        *(inEdge.rp), WIR_UpDownValue { inEdge.downVal },
        WIR_UpDownValue { inEdge.upVal } );

      auto &srcContainer =
        inEdge.rp->getContainers<WIR_BitValues>().begin()->get();
      srcContainer.insertOutValues(
        *to, WIR_UpDownValue { inEdge.downVal },
        WIR_UpDownValue { inEdge.upVal } );
    }
  };

  // A small lambda to copy outgoing bit value data.
  auto copyOutEdges = [&]( WIR_RegisterParameter *from,
                           WIR_RegisterParameter *to ) {
    auto &cFrom = from->getContainers<WIR_BitValues>().begin()->get();
    if ( !to->containsContainers( WIR_BitValues::getContainerTypeID() ) )
      to->insertContainer( new WIR_BitValues() );
    else
      to->getContainers<WIR_BitValues>().begin()->get().clearOutValues();
    auto &cTo = to->getContainers<WIR_BitValues>().begin()->get();

    for ( auto &outEdge : cFrom.getOutValues() ) {
      cTo.insertOutValues(
        *(outEdge.rp), WIR_UpDownValue { outEdge.downVal },
        WIR_UpDownValue { outEdge.upVal } );

      auto &tgtContainer =
        outEdge.rp->getContainers<WIR_BitValues>().begin()->get();
      tgtContainer.insertInValues(
        *to, WIR_UpDownValue { outEdge.downVal },
        WIR_UpDownValue { outEdge.upVal } );
    }
  };

  // A small lambda to remove an operation from all its related bitValue
  // containers.
  auto removeOp = [&]( WIR_Operation &o ) {
    for ( WIR_Parameter &p : o )
      if ( p.containsContainers( WIR_BitValues::getContainerTypeID() ) ) {
        auto &c = p.getContainers<WIR_BitValues>().begin()->get();

        for ( auto &inEdge : c.getInValues() ) {
          auto &srcContainer =
            inEdge.rp->getContainers<WIR_BitValues>().begin()->get();
          srcContainer.eraseOutValues( p );
        }

        for ( auto &outEdge : c.getOutValues() ) {
          auto &tgtContainer =
            outEdge.rp->getContainers<WIR_BitValues>().begin()->get();
          tgtContainer.eraseInValues( p );
        }
      }
  };

  WIR_Operation &o0 = p[ 0 ]->get().begin()->get();
  WIR_Instruction &i = o0.getInstruction();

  auto &newF = getNewOperationFormat( o0 );

  if ( mSingleAReg ) {

    WIR_RegisterParameter *newOpAReg;
    WIR_RegisterParameter *newOpOtherReg;
    WIR_RegisterParameter *oldOpOtherReg;

    // Create the new LD/ST.
    if ( ( newF == TC13::OperationFormat::AC10APIA ) ||
         ( newF == TC13::OperationFormat::AC10DPIA_1 ) ||
         ( newF == TC13::OperationFormat::AC10EPIA ) ||
         ( newF == TC13::OperationFormat::AC10PPIA ) ||
         ( newF == TC13::OperationFormat::AC10DPIA_2 ) ) {
      auto &newOp = i.pushBackOperation(
        { o0.getOpCode(), newF,
          new WIR_AddressingModeParameter( TC13::AddressingMode::pre ),
          new WIR_RegisterParameter( *mAReg, WIR_Usage::defuse ),
          new TC_Const10_Signed( mIncrement ),
          new WIR_RegisterParameter(
            dynamic_cast<WIR_RegisterParameter &>(
              o0.getExplicitParameter(
                o0.getExplicitParameters().size() ) ) ) } );

      newOpAReg =
        &(dynamic_cast<WIR_RegisterParameter &>(
            newOp.getExplicitParameter( 2 ) ));
      newOpOtherReg =
        &(dynamic_cast<WIR_RegisterParameter &>(
            newOp.getExplicitParameter( 4 ) ));
      oldOpOtherReg =
        &(dynamic_cast<WIR_RegisterParameter &>(
            o0.getExplicitParameter( o0.getExplicitParameters().size() ) ));
    } else

    if ( ( newF == TC13::OperationFormat::AC10ABOA ) ||
         ( newF == TC13::OperationFormat::AC10DBOA_1 ) ||
         ( newF == TC13::OperationFormat::AC10DBOA_2 ) ||
         ( newF == TC13::OperationFormat::AC10EBOA ) ||
         ( newF == TC13::OperationFormat::AC10PBOA ) ) {
      auto &newOp = i.pushBackOperation(
        { o0.getOpCode(), newF,
          new WIR_RegisterParameter( *mAReg, WIR_Usage::use ),
          new TC_Const10_Signed( mIncrement ),
          new WIR_RegisterParameter(
            dynamic_cast<WIR_RegisterParameter &>(
              o0.getExplicitParameter(
                o0.getExplicitParameters().size() ) ) ) } );

      newOpAReg =
        &(dynamic_cast<WIR_RegisterParameter &>(
            newOp.getExplicitParameter( 1 ) ));
      newOpOtherReg =
        &(dynamic_cast<WIR_RegisterParameter &>(
            newOp.getExplicitParameter( 3 ) ));
      oldOpOtherReg =
        &(dynamic_cast<WIR_RegisterParameter &>(
            o0.getExplicitParameter( o0.getExplicitParameters().size() ) ));
    } else

    if ( ( newF == TC13::OperationFormat::AAC10BOA ) ||
         ( newF == TC13::OperationFormat::DAC10BOA ) ||
         ( newF == TC13::OperationFormat::EAC10BOA ) ||
         ( newF == TC13::OperationFormat::PAC10BOA ) ) {
      auto &newOp = i.pushBackOperation(
        { o0.getOpCode(), newF,
          new WIR_RegisterParameter(
            dynamic_cast<WIR_RegisterParameter &>(
              o0.getExplicitParameter( 1 ) ) ),
          new WIR_RegisterParameter( *mAReg, WIR_Usage::use ),
          new TC_Const10_Signed( mIncrement ) } );

      newOpAReg =
        &(dynamic_cast<WIR_RegisterParameter &>(
            newOp.getExplicitParameter( 2 ) ));
      newOpOtherReg =
        &(dynamic_cast<WIR_RegisterParameter &>(
            newOp.getExplicitParameter( 1 ) ));
      oldOpOtherReg =
        &(dynamic_cast<WIR_RegisterParameter &>(
            o0.getExplicitParameter( 1 ) ));
    } else {
      auto &newOp = i.pushBackOperation(
        { o0.getOpCode(), newF,
          new WIR_RegisterParameter(
            dynamic_cast<WIR_RegisterParameter &>(
              o0.getExplicitParameter( 1 ) ) ),
          new WIR_AddressingModeParameter( TC13::AddressingMode::pre ),
          new WIR_RegisterParameter( *mAReg, WIR_Usage::defuse ),
          new TC_Const10_Signed( mIncrement ) } );

      newOpAReg =
        &(dynamic_cast<WIR_RegisterParameter &>(
            newOp.getExplicitParameter( 3 ) ));
      newOpOtherReg =
        &(dynamic_cast<WIR_RegisterParameter &>(
            newOp.getExplicitParameter( 1 ) ));
      oldOpOtherReg =
        &(dynamic_cast<WIR_RegisterParameter &>(
            o0.getExplicitParameter( 1 ) ));
    }

    // Duplicate all bit value containers from the old operations to the new
    // one.
    WIR_RegisterParameter *incrARegUse =
      &(dynamic_cast<WIR_RegisterParameter &>(
          mFoundIncr->getExplicitParameter(
            ( mFoundIncr->getSize() == 2 ) ? 1 : 2 ) ) );

    copyInEdges( incrARegUse, newOpAReg );

    copyInEdges( oldOpOtherReg, newOpOtherReg );
    copyOutEdges( oldOpOtherReg, newOpOtherReg );
    mNewLocation.insert( { oldOpOtherReg->getID(), *newOpOtherReg } );

    // Remove o0 and the increment from all its related bitValue containers.
    removeOp( o0 );
    removeOp( *mFoundIncr );

    // Update location bits.
    updateLocations( i.getBasicBlock().getFunction() );

    // Erase original operations that became superfluous now.
    auto &incr = mFoundIncr->getInstruction();
    incr.getBasicBlock().eraseInstruction(
      incr.getBasicBlock().findInstruction( incr ) );
    i.eraseOperation( i.begin() );

  } else {

    WIR_RegisterParameter *newOpAReg;
    WIR_RegisterParameter *newOpOtherReg;
    WIR_RegisterParameter *oldOpOtherReg;
    WIR_BaseImmediateParameter *newOpImm =
      ( ( newF == TC13::OperationFormat::AAC16BOA ) ||
        ( newF == TC13::OperationFormat::DAC16BOA ) ||
        ( newF == TC13::OperationFormat::AC16DBOA ) ) ?
        static_cast<WIR_BaseImmediateParameter *>(
          new TC_Const16_Signed( mIncrement ) ) :
        static_cast<WIR_BaseImmediateParameter *>(
          new TC_Const10_Signed( mIncrement ) );
    WIR_RegisterParameter *incrARegUse =
      &(dynamic_cast<WIR_RegisterParameter &>(
          mFoundIncr->getExplicitParameter( 2 ) ));

    if ( ( newF == TC13::OperationFormat::AC10ABOA ) ||
         ( newF == TC13::OperationFormat::AC10EBOA ) ||
         ( newF == TC13::OperationFormat::AC10PBOA ) ||
         ( newF == TC13::OperationFormat::AC16DBOA ) ||
         ( newF == TC13::OperationFormat::AC10DBOA_1 ) ||
         ( newF == TC13::OperationFormat::AC10DBOA_2 ) ) {
      // ST
      auto &newOp = i.pushBackOperation(
        { o0.getOpCode(), newF,
          new WIR_RegisterParameter( *incrARegUse ), newOpImm,
          new WIR_RegisterParameter(
            dynamic_cast<WIR_RegisterParameter &>(
              o0.getExplicitParameter(
                o0.getExplicitParameters().size() ) ) ) } );

      newOpAReg =
        &(dynamic_cast<WIR_RegisterParameter &>(
            newOp.getExplicitParameter( 1 ) ));
      newOpOtherReg =
        &(dynamic_cast<WIR_RegisterParameter &>(
            newOp.getExplicitParameter( 3 ) ));
      oldOpOtherReg =
        &(dynamic_cast<WIR_RegisterParameter &>(
            o0.getExplicitParameter( o0.getExplicitParameters().size() ) ));
    } else {
      // LD
      auto &newOp = i.pushBackOperation(
        { o0.getOpCode(), newF,
          new WIR_RegisterParameter(
            dynamic_cast<WIR_RegisterParameter &>(
              o0.getExplicitParameter( 1 ) ) ),
          new WIR_RegisterParameter( *incrARegUse ), newOpImm } );

      newOpAReg =
        &(dynamic_cast<WIR_RegisterParameter &>(
            newOp.getExplicitParameter( 2 ) ));
      newOpOtherReg =
        &(dynamic_cast<WIR_RegisterParameter &>(
            newOp.getExplicitParameter( 1 ) ));
      oldOpOtherReg =
        &(dynamic_cast<WIR_RegisterParameter &>(
            o0.getExplicitParameter( 1 ) ));
    }

    // Duplicate all bit value containers from the old operations to the new
    // one.
    copyInEdges( incrARegUse, newOpAReg );

    copyInEdges( oldOpOtherReg, newOpOtherReg );
    copyOutEdges( oldOpOtherReg, newOpOtherReg );
    for ( auto it = mNewLocation.begin(); it != mNewLocation.end(); ) {
      auto oldParamID = it->first;
      auto &newParam = it->second.get();

      if ( newParam.getID() == oldOpOtherReg->getID() ) {
        mNewLocation.insert( { oldParamID, *newOpOtherReg } );
        it = mNewLocation.erase( it );
      } else
        ++it;
    }
    mNewLocation.insert( { oldOpOtherReg->getID(), *newOpOtherReg } );

    // Remove o0 from all its related bitValue containers.
    removeOp( o0 );

    // Erase original operation that became superfluous now. The increment will
    // be erased by a subsequent dead code elimination if data flow permits.
    i.eraseOperation( i.begin() );
    if ( mFoundIncr->getExplicitParameter( 1 ).getContainers<WIR_BitValues>().
           begin()->get().getOutValues().empty() ) {
      removeOp( *mFoundIncr );
      auto &incr = mFoundIncr->getInstruction();
      incr.getBasicBlock().eraseInstruction(
        incr.getBasicBlock().findInstruction( incr ) );
    }
  }
};


//
// Private class methods
//

/*
  getNewOperationFormat determines the new format of an optimized memory access
  depending on the original memory access's format.
*/
const WIR_BaseProcessor::OperationFormat &TC_Peep_PreIncr::getNewOperationFormat( const WIR_Operation &o ) const
{
  DSTART(
    "const WIR_BaseProcessor::OperationFormat& TC_Peep_PreIncr::getNewOperationFormat(const WIR_Operation&) const" );

  // A small lambda to perform a depth-first traversal from a basic block as
  // starting point.
  WIR_Instruction &i = o.getInstruction();
  set<WIR_id_t> visited;

  function<void( WIR_BasicBlock & )> dfs = [&]( WIR_BasicBlock &b ) {
    if ( b != i.getBasicBlock() )
      // Mark every bb different from the start block as visited.
      visited.insert( b.getID() );

    for ( WIR_BasicBlock &s : b.getSuccessors() )
      // If we encounter the start block again, mark it as visited now.
      if ( s == i.getBasicBlock() )
        visited.insert( s.getID() );
      else

      if ( !visited.count( s.getID() ) )
        dfs( s );
  };

  static bool initDone = false;
  static map<WIR_BaseProcessor::OperationFormat,
             WIR_BaseProcessor::OperationFormat> newFormatSingleAReg;
  static map<WIR_BaseProcessor::OperationFormat,
             WIR_BaseProcessor::OperationFormat> newBaseOffsetFormat;
  static map<WIR_BaseProcessor::OperationFormat,
             WIR_BaseProcessor::OperationFormat> newFormatMultiAReg;

  if ( !initDone ) {
    newFormatSingleAReg = {
      { TC13::OperationFormat::AAC10BOA, TC13::OperationFormat::AAC10PIA },
      { TC13::OperationFormat::AAC16BOA, TC13::OperationFormat::AAC10PIA },
      { TC13::OperationFormat::DAC10BOA, TC13::OperationFormat::DAC10PIA },
      { TC13::OperationFormat::EAC10BOA, TC13::OperationFormat::EAC10PIA },
      { TC13::OperationFormat::PAC10BOA, TC13::OperationFormat::PAC10PIA },
      { TC13::OperationFormat::DAC16BOA, TC13::OperationFormat::DAC10PIA },
      { TC13::OperationFormat::AC10ABOA, TC13::OperationFormat::AC10APIA },
      { TC13::OperationFormat::AC10DBOA_1, TC13::OperationFormat::AC10DPIA_1 },
      { TC13::OperationFormat::AC10EBOA, TC13::OperationFormat::AC10EPIA },
      { TC13::OperationFormat::AC10PBOA, TC13::OperationFormat::AC10PPIA },
      { TC13::OperationFormat::AC16DBOA, TC13::OperationFormat::AC10DPIA_1 },
      { TC13::OperationFormat::AC10DBOA_2, TC13::OperationFormat::AC10DPIA_2 },
      { TC13::OperationFormat::SISPC10_1, TC13::OperationFormat::AAC10PIA },
      { TC13::OperationFormat::SAA_2, TC13::OperationFormat::AAC10PIA },
      { TC13::OperationFormat::SAIC4, TC13::OperationFormat::AAC10PIA },
      { TC13::OperationFormat::SIAC4_1, TC13::OperationFormat::AAC10PIA },
      { TC13::OperationFormat::SDA_2, TC13::OperationFormat::DAC10PIA },
      { TC13::OperationFormat::SDIC4_1, TC13::OperationFormat::DAC10PIA },
      { TC13::OperationFormat::SIAC4_2, TC13::OperationFormat::DAC10PIA },
      { TC13::OperationFormat::SISPC10_2, TC13::OperationFormat::DAC10PIA },
      { TC13::OperationFormat::SSPC10I_1, TC13::OperationFormat::AC10APIA },
      { TC13::OperationFormat::SAC4I_1, TC13::OperationFormat::AC10APIA },
      { TC13::OperationFormat::SAA_4, TC13::OperationFormat::AC10APIA },
      { TC13::OperationFormat::SIC4A, TC13::OperationFormat::AC10APIA },
      { TC13::OperationFormat::SAC4I_2, TC13::OperationFormat::AC10DPIA_1 },
      { TC13::OperationFormat::SAD_2, TC13::OperationFormat::AC10DPIA_1 },
      { TC13::OperationFormat::SIC4D, TC13::OperationFormat::AC10DPIA_1 },
      { TC13::OperationFormat::SSPC10I_2, TC13::OperationFormat::AC10DPIA_1 } };

    newBaseOffsetFormat = {
      { TC13::OperationFormat::AC10APIA, TC13::OperationFormat::AC10ABOA },
      { TC13::OperationFormat::AC10DPIA_1, TC13::OperationFormat::AC10DBOA_1 },
      { TC13::OperationFormat::AC10DPIA_2, TC13::OperationFormat::AC10DBOA_2 },
      { TC13::OperationFormat::AC10EPIA, TC13::OperationFormat::AC10EBOA },
      { TC13::OperationFormat::AC10PPIA, TC13::OperationFormat::AC10PBOA },
      { TC13::OperationFormat::AAC10PIA, TC13::OperationFormat::AAC10BOA },
      { TC13::OperationFormat::DAC10PIA, TC13::OperationFormat::DAC10BOA },
      { TC13::OperationFormat::EAC10PIA, TC13::OperationFormat::EAC10BOA },
      { TC13::OperationFormat::PAC10PIA, TC13::OperationFormat::PAC10BOA } };

    newFormatMultiAReg = {
      { TC13::OperationFormat::AAC10BOA, TC13::OperationFormat::AAC16BOA },
      { TC13::OperationFormat::AAC16BOA, TC13::OperationFormat::AAC16BOA },
      { TC13::OperationFormat::DAC10BOA, TC13::OperationFormat::DAC10BOA },
      { TC13::OperationFormat::EAC10BOA, TC13::OperationFormat::EAC10BOA },
      { TC13::OperationFormat::PAC10BOA, TC13::OperationFormat::PAC10BOA },
      { TC13::OperationFormat::DAC16BOA, TC13::OperationFormat::DAC16BOA },
      { TC13::OperationFormat::AC10ABOA, TC13::OperationFormat::AC10ABOA },
      { TC13::OperationFormat::AC10DBOA_1, TC13::OperationFormat::AC10DBOA_1 },
      { TC13::OperationFormat::AC10EBOA, TC13::OperationFormat::AC10EBOA },
      { TC13::OperationFormat::AC10PBOA, TC13::OperationFormat::AC10PBOA },
      { TC13::OperationFormat::AC16DBOA, TC13::OperationFormat::AC16DBOA },
      { TC13::OperationFormat::AC10DBOA_2, TC13::OperationFormat::AC10DBOA_2 },
      { TC13::OperationFormat::SISPC10_1, TC13::OperationFormat::AAC16BOA },
      { TC13::OperationFormat::SAA_2, TC13::OperationFormat::AAC16BOA },
      { TC13::OperationFormat::SAIC4, TC13::OperationFormat::AAC16BOA },
      { TC13::OperationFormat::SIAC4_1, TC13::OperationFormat::AAC16BOA },
      { TC13::OperationFormat::SDA_2, TC13::OperationFormat::DAC10BOA },
      { TC13::OperationFormat::SDIC4_1, TC13::OperationFormat::DAC10BOA },
      { TC13::OperationFormat::SIAC4_2, TC13::OperationFormat::DAC10BOA },
      { TC13::OperationFormat::SISPC10_2, TC13::OperationFormat::DAC16BOA },
      { TC13::OperationFormat::SSPC10I_1, TC13::OperationFormat::AC10ABOA },
      { TC13::OperationFormat::SAC4I_1, TC13::OperationFormat::AC10ABOA },
      { TC13::OperationFormat::SAA_4, TC13::OperationFormat::AC10ABOA },
      { TC13::OperationFormat::SIC4A, TC13::OperationFormat::AC10ABOA },
      { TC13::OperationFormat::SAC4I_2, TC13::OperationFormat::AC10DBOA_1 },
      { TC13::OperationFormat::SAD_2, TC13::OperationFormat::AC10DBOA_1 },
      { TC13::OperationFormat::SIC4D, TC13::OperationFormat::AC10DBOA_1 },
      { TC13::OperationFormat::SSPC10I_2, TC13::OperationFormat::AC16DBOA } };

    initDone = true;
  }

  auto &res =
    mSingleAReg ?
      newFormatSingleAReg.at( o.getOperationFormat() ) :
      newFormatMultiAReg.at( o.getOperationFormat() );

  if ( mSingleAReg ) {
    // Check whether o is in a loop. If so, we change the operation format from
    // pre-decrement/-increment to base+offset addressing so that no loop-
    // carried dependence due to a de-/incremented address register exists.
    dfs( i.getBasicBlock() );

    if ( visited.count( i.getBasicBlock().getID() ) )
      return( newBaseOffsetFormat.at( res ) );
  }

  if ( ( res == TC13::OperationFormat::DAC10BOA ) &&
       ( o.getOpCode() == TC13::OpCode::LD_W ) )
    return( TC13::OperationFormat::DAC16BOA );
  if ( ( res == TC13::OperationFormat::AC10DBOA_1 ) &&
       ( o.getOpCode() == TC13::OpCode::ST_W ) )
    return( TC13::OperationFormat::AC16DBOA );

  return( res );
};

}       // namespace WIR
