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
  @file tcconstfold.cc
  @brief This file implements a TriCore-specific constant folding optimization.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include standard headers
#include <iterator>
#include <set>
#include <utility>

// Include boost headers
#include <boost/current_function.hpp>

// Include libuseful headers
#include <libuseful/debugmacros.h>

// Include WIR headers
#include <wir/wir.h>
#include <arch/tricore/tc131.h>
#include <arch/tricore/analyses/bit/tcbitdfa.h>

// Include local headers
#include "tcconstfold.h"


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
TC_ConstFold::TC_ConstFold( WIR_System &s ) :
  WIR_Optimization { s },
  WIR_ConstFold { s }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Default constructor for compilation unit-level optimization.
*/
TC_ConstFold::TC_ConstFold( WIR_CompilationUnit &c ) :
  WIR_Optimization { c },
  WIR_ConstFold { c }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Default constructor for function-level optimization.
*/
TC_ConstFold::TC_ConstFold( WIR_Function &f ) :
  WIR_Optimization { f },
  WIR_ConstFold { f }
{
  DSTART( "TC_ConstFold::TC_ConstFold(WIR_Function&)" );
};


/*
  Destructor.
*/
TC_ConstFold::~TC_ConstFold( void )
{
  DSTART( "virtual TC_ConstFold::~TC_ConstFold()" );
};


//
// Protected class methods
//

/*
  runOptimization folds constants in the given function.
*/
void TC_ConstFold::runOptimization( WIR_Function &f )
{
  DSTART( "virtual void TC_ConstFold::runOptimization(WIR_Function&)" );

  TC_BitDFA analyzer { f };
  setDFA( analyzer );

  WIR_ConstFold::runOptimization( f );
};


/*
  For an operation identified to be constant, doConstFolding does the actual
  TriCore-specific folding.

  doConstFolding does not actually modify the currently examined WIR operation
  o. Instead, new instructions realizing the constant folding of o are added to
  map mNewInstructions.
*/
bool TC_ConstFold::doConstFolding( const WIR_Operation &o,
                                   const std::map<WIR_id_t, WIR_UpDownValue> &outValue,
                                   const std::map<WIR_id_t, WIR_UpDownValue> &inValue )
{
  DSTART(
    "virtual bool TC_ConstFold::doConstFolding(const WIR_Operation&, const map<long long unsigned int, WIR_UpDownValue>&, const map<long long unsigned int, WIR_UpDownValue>&)" );

  // A small lambda to retrieve the ID of o's first explicit parameter.
  auto firstID = [&]( void ) -> WIR_id_t {
    auto &p = o.getExplicitParameters().front().get();
    return( p.getID() );
  };

  // A small lambda to retrieve the ID of o's last explicit parameter.
  auto lastID = [&]( void ) -> WIR_id_t {
    auto &p = o.getExplicitParameters().back().get();
    return( p.getID() );
  };

  // A small lambda to retrieve the ID of o's nth explicit parameter.
  auto nthID = [&]( unsigned int n ) -> WIR_id_t {
    auto it = o.getExplicitParameters().begin();
    std::advance( it, n );
    return( it->get().getID() );
  };

  // A small lambda to retrieve the register of o's first explicit parameter.
  auto firstReg = [&]( void ) -> WIR_BaseRegister & {
    auto &rp =
      dynamic_cast<WIR_RegisterParameter &>(
        o.getExplicitParameters().front().get() );
    return( rp.getRegister() );
  };

  // A small lambda to retrieve the register of o's last explicit parameter.
  auto lastReg = [&]( void ) -> WIR_BaseRegister & {
    auto &rp =
      dynamic_cast<WIR_RegisterParameter &>(
        o.getExplicitParameters().back().get() );
    return( rp.getRegister() );
  };

  // A small lambda to retrieve the register of o's nth explicit parameter.
  auto nthReg = [&]( unsigned int n ) -> WIR_BaseRegister & {
    auto it = o.getExplicitParameters().begin();
    std::advance( it, n );
    return( dynamic_cast<WIR_RegisterParameter &>( it->get() ).getRegister() );
  };

  // A small lambda to retrieve o's first explicit parameter.
  auto firstParam = [&]( void ) -> WIR_Parameter & {
    return( o.getExplicitParameters().front().get() );
  };

  // A small lambda to retrieve o's last explicit parameter.
  auto lastParam = [&]( void ) -> WIR_Parameter & {
    return( o.getExplicitParameters().back().get() );
  };

  // A small lambda to retrieve o's nth explicit parameter.
  auto nthParam = [&]( unsigned int n ) -> WIR_Parameter & {
    auto it = o.getExplicitParameters().begin();
    std::advance( it, n );
    return( it->get() );
  };

  static bool initDone = false;
  static set<pair<WIR_BaseProcessor::OpCode,
             WIR_BaseProcessor::OperationFormat>> getMOVOps;
  static set<pair<WIR_BaseProcessor::OpCode,
             WIR_BaseProcessor::OperationFormat>> getLEAOps;

  if ( !initDone ) {
    getMOVOps = {
      { TC131::OpCode::ADDI, TC131::OperationFormat::DDC16_1 },
      { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1 },
      { TC131::OpCode::MOV, TC131::OperationFormat::SDC4_1 },
      { TC131::OpCode::MOV_U, TC131::OperationFormat::DC16_2 },
      { TC131::OpCode::MOVH, TC131::OperationFormat::DC16_2 } };

    getLEAOps = {
      { TC13::OpCode::LEA, TC13::OperationFormat::AC18ABSA },
      { TC13::OpCode::LEA, TC13::OperationFormat::AAC16BOA },
      { TC13::OpCode::MOVH_A, TC13::OperationFormat::AC16 } };

    initDone = true;
  }

  // Check whether all outgoing bit-values are constant.
  bool allOutValuesConst = !outValue.empty();
  for ( auto &p : outValue )
    if ( !p.second.isInteger() )
      allOutValuesConst = false;

  // Handle operations that produce a constant value in an AREG as very first
  // parameter. Of course, those operations that getLEA produces must be
  // excluded here.
  if ( !getLEAOps.count( { o.getOpCode(), o.getOperationFormat() } ) &&
       ( firstParam().getType() == WIR_ParameterType::reg ) &&
       ( dynamic_cast<WIR_RegisterParameter &>( firstParam() ).isDefined() ||
         dynamic_cast<WIR_RegisterParameter &>( firstParam() ).isDefUsed() ) &&
       ( firstReg().getType() == TC13::RegisterType::aReg ) &&
       allOutValuesConst ) {
    mNewInstructions.emplace(
      const_cast<WIR_Operation &>( o ),
      getLEA( firstParam(), outValue.at( firstID() ) ) );
    return( true );
  }

  // Handle operations that produce a constant value in a DREG as very first
  // parameter. Of course, those operations that getMOV produces must be
  // excluded here, as well as ADDC, ADDX, JNED, JNEI, SHA, SUBC and SUBX which
  // have side-effects.
  if ( !getMOVOps.count( { o.getOpCode(), o.getOperationFormat() } ) &&
       ( o.getOpCode() != TC13::OpCode::ADDC ) &&
       ( o.getOpCode() != TC13::OpCode::ADDX ) &&
       ( o.getOpCode() != TC13::OpCode::JNED ) &&
       ( o.getOpCode() != TC13::OpCode::JNEI ) &&
       ( o.getOpCode() != TC13::OpCode::SHA ) &&
       ( o.getOpCode() != TC13::OpCode::SUBC ) &&
       ( o.getOpCode() != TC13::OpCode::SUBX ) &&
       ( firstParam().getType() == WIR_ParameterType::reg ) &&
       ( dynamic_cast<WIR_RegisterParameter &>( firstParam() ).isDefined() ||
         dynamic_cast<WIR_RegisterParameter &>( firstParam() ).isDefUsed() ) &&
       ( firstReg().getType() == TC13::RegisterType::dReg ) &&
       allOutValuesConst ) {
    mNewInstructions.emplace(
      const_cast<WIR_Operation &>( o ),
      getMOV( firstParam(), outValue.at( firstID() ) ) );
    return( true );
  }

  // Handle operations that produce a constant value in a DREG as very last
  // parameter.
  if ( ( o.getExplicitParameters().size() > 1 ) &&
       ( lastParam().getType() == WIR_ParameterType::reg ) &&
       ( dynamic_cast<WIR_RegisterParameter &>( lastParam() ).isDefined() ||
         dynamic_cast<WIR_RegisterParameter &>( lastParam() ).isDefUsed() ) &&
       ( lastReg().getType() == TC13::RegisterType::dReg ) &&
       allOutValuesConst ) {
    mNewInstructions.emplace(
      const_cast<WIR_Operation &>( o ),
      getMOV( lastParam(), outValue.at( lastID() ) ) );
    return( true );
  }

  // Handle operations that produce a constant value in an EREG as very first
  // parameter.
  if ( ( firstParam().getType() == WIR_ParameterType::reg ) &&
       ( dynamic_cast<WIR_RegisterParameter &>( firstParam() ).isDefined() ||
         dynamic_cast<WIR_RegisterParameter &>( firstParam() ).isDefUsed() ) &&
       ( firstReg().getType() == TC13::RegisterType::eReg ) &&
       allOutValuesConst ) {
    mNewInstructions.emplace(
      const_cast<WIR_Operation &>( o ),
      getMOV_E( firstParam(), outValue.at( firstID() ) ) );
    return( true );
  }

  // Handle operations that produce a constant value in a PREG as very first
  // parameter.
  if ( ( firstParam().getType() == WIR_ParameterType::reg ) &&
       ( dynamic_cast<WIR_RegisterParameter &>( firstParam() ).isDefined() ||
         dynamic_cast<WIR_RegisterParameter &>( firstParam() ).isDefUsed() ) &&
       ( firstReg().getType() == TC13::RegisterType::pReg ) &&
       allOutValuesConst ) {
    mNewInstructions.emplace(
      const_cast<WIR_Operation &>( o ),
      getLEA_P( firstParam(), outValue.at( firstID() ) ) );
    return( true );
  }

  // Handle operations that produce a constant value in a PREG as second
  // parameter.
  if ( ( o.getExplicitParameters().size() > 1 ) &&
       ( nthParam( 1 ).getType() == WIR_ParameterType::reg ) &&
       ( dynamic_cast<WIR_RegisterParameter &>( nthParam( 1 ) ).isDefined() ||
         dynamic_cast<WIR_RegisterParameter &>( nthParam( 1 ) ).isDefUsed() ) &&
       ( nthReg( 1 ).getType() == TC13::RegisterType::pReg ) &&
       allOutValuesConst ) {
    mNewInstructions.emplace(
      const_cast<WIR_Operation &>( o ),
      getLEA_P( nthParam( 1 ), outValue.at( nthID( 1 ) ) ) );
    return( true );
  }

  //
  // Handling of special cases.
  //

  // Folding of operations dealing with PSW.C.
  if ( ( ( o.getOpCode() == TC13::OpCode::ADDC ) ||
         ( o.getOpCode() == TC13::OpCode::SUBC ) ) &&
       ( ( outValue.count( lastID() ) &&
           ( outValue.at( lastID() ).containsOnlyBit( WIR_L4::bX ) ||
             ( outValue.at( lastID() ).at( 0 ) ==
                 inValue.at( lastID() ).at( 0 ) ) ) ) ||
          !outValue.count( lastID() ) ) &&
       allOutValuesConst ) {
    // ADDC Dc, Da, Db -> MOV
    // SUBC Dc, Da, Db -> MOV
    //   if outgoing PSW.C is X or if operation leaves PSW.C unchanged.
    mNewInstructions.emplace(
      const_cast<WIR_Operation &>( o ),
      getMOV( firstParam(), outValue.at( firstID() ) ) );
exit( 7 );
    return( true );
  }

  if ( ( ( o.getOpCode() == TC13::OpCode::ADDX ) ||
         ( o.getOpCode() == TC13::OpCode::SHA ) ||
         ( o.getOpCode() == TC13::OpCode::SUBX ) ) &&
       ( ( outValue.count( lastID() ) &&
           ( outValue.at( lastID() ).containsOnlyBit( WIR_L4::bX ) ) ) ||
         !outValue.count( lastID() ) ) &&
       allOutValuesConst ) {
    // ADDX Dc, Da, Db -> MOV
    // SUBX Dc, Da, Db -> MOV
    //   if outgoing PSW.C is X.
    mNewInstructions.emplace(
      const_cast<WIR_Operation &>( o ),
      getMOV( firstParam(), outValue.at( firstID() ) ) );
    return( true );
  }

  if ( ( ( o.getOpCode() == TC13::OpCode::ADDX ) ||
         ( o.getOpCode() == TC13::OpCode::SUBX ) ) &&
       ( ( outValue.count( lastID() ) &&
           ( outValue.at( lastID() ).containsOnlyBit( WIR_L4::bX ) ) ) ||
         !outValue.count( lastID() ) ) ) {
    // ADDX Dc, Da, Db -> ADD
    // SUBX Dc, Da, Db -> SUB
    //   if outgoing PSW.C is X but Dc is not const.
    mNewInstructions.emplace(
      const_cast<WIR_Operation &>( o ), getADDSUB( o ) );
    return( true );
  }

  // Folding of constant conditional jumps.
  if ( doBranchFolding( o, inValue ) )
    return( true );

  // Folding of operations that arithmetically have no effect into MOVs.
  if ( doMOVFolding( o, inValue ) )
    return( true );

  return( false );
};


//
// Private class methods
//

/*
  For an operation identified to be constant, doBranchFolding performs TriCore-
  specific folding of conditional branches.

  If the result of jump condition is statically known, the conditional branch
  gets folded either to an unconditional branch or gets removed completely.

  doBranchFolding does not actually modify the currently examined WIR operation
  o. Instead, new instructions realizing the constant folding of o are added to
  map mNewInstructions.
*/
bool TC_ConstFold::doBranchFolding( const WIR_Operation &o,
                                    const std::map<WIR_id_t, WIR_UpDownValue> &inValue )
{
  DSTART(
    "bool TC_ConstFold::doBranchFolding(const WIR_Operation&, const map<long long unsigned int, WIR_UpDownValue>&)" );

  // A small lambda to retrieve the register of o's first explicit parameter.
  auto firstReg = [&]( void ) -> WIR_BaseRegister & {
    auto &rp =
      dynamic_cast<WIR_RegisterParameter &>(
        o.getExplicitParameters().front().get() );
    return( rp.getRegister() );
  };

  // A small lambda to retrieve the register of o's nth explicit parameter.
  auto nthReg = [&]( unsigned int n ) -> WIR_BaseRegister & {
    auto it = o.getExplicitParameters().begin();
    std::advance( it, n );
    return( dynamic_cast<WIR_RegisterParameter &>( it->get() ).getRegister() );
  };

  // A small lambda to retrieve the immediate of o's nth explicit parameter.
  auto nthImm = [&]( unsigned int n ) -> WIR_UpDownValue {
    auto it = o.getExplicitParameters().begin();
    std::advance( it, n );
    return(
      WIR_UpDownValue(
        dynamic_cast<WIR_BaseImmediateParameter &>( it->get() ) ) );
  };

  // A small lambda to retrieve o's first explicit parameter.
  auto firstParam = [&]( void ) -> WIR_Parameter & {
    return( o.getExplicitParameters().front().get() );
  };

  // A small lambda to retrieve o's last explicit parameter.
  auto lastParam = [&]( void ) -> WIR_Parameter & {
    return( o.getExplicitParameters().back().get() );
  };

  // A small lambda to retrieve o's nth explicit parameter.
  auto nthParam = [&]( unsigned int n ) -> WIR_Parameter & {
    auto it = o.getExplicitParameters().begin();
    std::advance( it, n );
    return( it->get() );
  };

  // A small lambda to retrieve the in-value of o's first explicit parameter.
  auto firstOp = [&]( void ) -> const WIR_UpDownValue & {
    auto &p = o.getExplicitParameters().front().get();

    // Get up value from inValues map.
    return( inValue.at( p.getID() ) );
  };

  // A small lambda to retrieve the in-value of o's nth explicit parameter.
  auto nthOp = [&]( unsigned int n ) -> const WIR_UpDownValue & {
    auto it = o.getExplicitParameters().begin();
    std::advance( it, n );

    // Get up value from inValues map.
    return( inValue.at( it->get().getID() ) );
  };

  // JEQ Da, Da, disp15 -> J disp8
  // JEQ.A Aa, Aa, disp15 -> J disp8
  // JGE Da, Da, disp15 -> J disp8
  // JGE.U Da, Da, disp15 -> J disp8
  if ( ( ( o.getOpCode() == TC13::OpCode::JEQ ) ||
         ( o.getOpCode() == TC13::OpCode::JEQ_A ) ||
         ( o.getOpCode() == TC13::OpCode::JGE ) ||
         ( o.getOpCode() == TC13::OpCode::JGE_U ) ) &&
       ( ( o.getOperationFormat() == TC13::OperationFormat::DDL_1 ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::SIDL ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::AAL ) ) &&
       ( firstReg() == nthReg( 1 ) ) ) {
    mNewInstructions.emplace(
      const_cast<WIR_Operation &>( o ),
      getJ( dynamic_cast<WIR_LabelParameter &>( lastParam() ) ) );
    return( true );
  }

  // JEQ Da, Db, disp15 -> J disp8
  // JEQ Da, imm, disp15 -> J disp8
  // JEQ.A Aa, Ab, disp15 -> J disp8
  if ( ( ( o.getOpCode() == TC13::OpCode::JEQ ) ||
         ( o.getOpCode() == TC13::OpCode::JEQ_A ) ) &&
         ( ( ( nthParam( 1 ).getType() == WIR_ParameterType::reg ) &&
             ( ( firstOp() == nthOp( 1 ) ) == WIR_L4::b1 ) ) ||
           ( ( nthParam( 1 ).getType() == WIR_ParameterType::imm ) &&
             ( ( firstOp() == nthImm( 1 ).extend( 32 ) ) == WIR_L4::b1 ) ) ) ) {
    mNewInstructions.emplace(
      const_cast<WIR_Operation &>( o ),
      getJ( dynamic_cast<WIR_LabelParameter &>( lastParam() ) ) );
    return( true );
  }

  // JEQ Da, Db, disp15 -> <nothing>
  // JEQ Da, imm, disp15 -> <nothing>
  // JEQ.A Aa, Ab, disp15 -> <nothing>
  if ( ( ( o.getOpCode() == TC13::OpCode::JEQ ) ||
         ( o.getOpCode() == TC13::OpCode::JEQ_A ) ) &&
         ( ( ( nthParam( 1 ).getType() == WIR_ParameterType::reg ) &&
             ( ( firstOp() == nthOp( 1 ) ) == WIR_L4::b0 ) ) ||
           ( ( nthParam( 1 ).getType() == WIR_ParameterType::imm ) &&
             ( ( firstOp() == nthImm( 1 ).extend( 32 ) ) == WIR_L4::b0 ) ) ) ) {
    mNewInstructions.emplace(
      const_cast<WIR_Operation &>( o ), list<WIR_Instruction> {} );
    return( true );
  }

  // JGE Da, Db, disp15 -> J disp8
  // JGE Da, imm, disp15 -> J disp8
  // JGE.U Da, Db, disp15 -> J disp8
  // JGE.U Da, imm, disp15 -> J disp8
  if ( ( ( o.getOpCode() == TC13::OpCode::JGE ) ||
         ( o.getOpCode() == TC13::OpCode::JGE_U ) ) &&
         ( ( ( o.getOperationFormat() == TC13::OperationFormat::DDL_1 ) &&
             ( ( firstOp() >= nthOp( 1 ) ) == WIR_L4::b1 ) ) ||
           ( ( nthParam( 1 ).getType() == WIR_ParameterType::imm ) &&
             ( ( firstOp() >= nthImm( 1 ).extend( 32 ) ) == WIR_L4::b1 ) ) ) ) {
    mNewInstructions.emplace(
      const_cast<WIR_Operation &>( o ),
      getJ( dynamic_cast<WIR_LabelParameter &>( lastParam() ) ) );
    return( true );
  }

  // JGE Da, Db, disp15 -> <nothing>
  // JGE Da, imm, disp15 -> <nothing>
  // JGE.U Da, Db, disp15 -> <nothing>
  // JGE.U Da, imm, disp15 -> <nothing>
  if ( ( ( o.getOpCode() == TC13::OpCode::JGE ) ||
         ( o.getOpCode() == TC13::OpCode::JGE_U ) ) &&
         ( ( ( o.getOperationFormat() == TC13::OperationFormat::DDL_1 ) &&
             ( ( firstOp() >= nthOp( 1 ) ) == WIR_L4::b0 ) ) ||
           ( ( nthParam( 1 ).getType() == WIR_ParameterType::imm ) &&
             ( ( firstOp() >= nthImm( 1 ).extend( 32 ) ) == WIR_L4::b0 ) ) ) ) {
    mNewInstructions.emplace(
      const_cast<WIR_Operation &>( o ), list<WIR_Instruction> {} );
    return( true );
  }

  // JGEZ Da, disp4 -> J disp8
  if ( ( o.getOpCode() == TC13::OpCode::JGEZ ) &&
       ( ( firstOp() >=
             WIR_UpDownValue( WIR_L4::b0, 32, true ) ) == WIR_L4::b1 ) ) {
    mNewInstructions.emplace(
      const_cast<WIR_Operation &>( o ),
      getJ( dynamic_cast<WIR_LabelParameter &>( lastParam() ) ) );
    return( true );
  }

  // JGEZ Da, disp4 -> <nothing>
  if ( ( o.getOpCode() == TC13::OpCode::JGEZ ) &&
       ( ( firstOp() >=
             WIR_UpDownValue( WIR_L4::b0, 32, true ) ) == WIR_L4::b0 ) ) {
    mNewInstructions.emplace(
      const_cast<WIR_Operation &>( o ), list<WIR_Instruction> {} );
    return( true );
  }

  // JGTZ Da, disp4 -> J disp8
  if ( ( o.getOpCode() == TC13::OpCode::JGTZ ) &&
       ( ( WIR_UpDownValue( WIR_L4::b0, 32, true  ) <
             firstOp() ) == WIR_L4::b1 ) ) {
    mNewInstructions.emplace(
      const_cast<WIR_Operation &>( o ),
      getJ( dynamic_cast<WIR_LabelParameter &>( lastParam() ) ) );
    return( true );
  }

  // JGTZ Da, disp4 -> <nothing>
  if ( ( o.getOpCode() == TC13::OpCode::JGTZ ) &&
       ( ( WIR_UpDownValue( WIR_L4::b0, 32, true  ) <
             firstOp() ) == WIR_L4::b0 ) ) {
    mNewInstructions.emplace(
      const_cast<WIR_Operation &>( o ), list<WIR_Instruction> {} );
    return( true );
  }

  // JLEZ Da, disp4 -> J disp8
  if ( ( o.getOpCode() == TC13::OpCode::JLEZ ) &&
       ( ( WIR_UpDownValue( WIR_L4::b0, 32, true  ) >=
             firstOp() ) == WIR_L4::b1 ) ) {
    mNewInstructions.emplace(
      const_cast<WIR_Operation &>( o ),
      getJ( dynamic_cast<WIR_LabelParameter &>( lastParam() ) ) );
    return( true );
  }

  // JLEZ Da, disp4 -> <nothing>
  if ( ( o.getOpCode() == TC13::OpCode::JLEZ ) &&
       ( ( WIR_UpDownValue( WIR_L4::b0, 32, true  ) >=
             firstOp() ) == WIR_L4::b0 ) ) {
    mNewInstructions.emplace(
      const_cast<WIR_Operation &>( o ), list<WIR_Instruction> {} );
    return( true );
  }

  // JLT Da, Da, disp15 -> <nothing>
  // JLT.U Da, Da, disp15 -> <nothing>
  // JNE Da, Da, disp15 -> <nothing>
  // JNE.A Aa, Aa, disp15 -> <nothing>
  if ( ( ( o.getOpCode() == TC13::OpCode::JLT ) ||
         ( o.getOpCode() == TC13::OpCode::JLT_U ) ||
         ( o.getOpCode() == TC13::OpCode::JNE ) ||
         ( o.getOpCode() == TC13::OpCode::JNE_A ) ) &&
       ( ( o.getOperationFormat() == TC13::OperationFormat::DDL_1 ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::SIDL ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::AAL ) ) &&
       ( firstReg() == nthReg( 1 ) ) ) {
    mNewInstructions.emplace(
      const_cast<WIR_Operation &>( o ), list<WIR_Instruction> {} );
    return( true );
  }

  // JLT Da, Db, disp15 -> J disp8
  // JLT Da, imm, disp15 -> J disp8
  // JLT.U Da, Db, disp15 -> J disp8
  // JLT.U Da, imm, disp15 -> J disp8
  if ( ( ( o.getOpCode() == TC13::OpCode::JLT ) ||
         ( o.getOpCode() == TC13::OpCode::JLT_U ) ) &&
         ( ( ( o.getOperationFormat() == TC13::OperationFormat::DDL_1 ) &&
             ( ( firstOp() < nthOp( 1 ) ) == WIR_L4::b1 ) ) ||
           ( ( nthParam( 1 ).getType() == WIR_ParameterType::imm ) &&
             ( ( firstOp() < nthImm( 1 ).extend( 32 ) ) == WIR_L4::b1 ) ) ) ) {
    mNewInstructions.emplace(
      const_cast<WIR_Operation &>( o ),
      getJ( dynamic_cast<WIR_LabelParameter &>( lastParam() ) ) );
    return( true );
  }

  // JLT Da, Db, disp15 -> <nothing>
  // JLT Da, imm, disp15 -> <nothing>
  // JLT.U Da, Db, disp15 -> <nothing>
  // JLT.U Da, imm, disp15 -> <nothing>
  if ( ( ( o.getOpCode() == TC13::OpCode::JLT ) ||
         ( o.getOpCode() == TC13::OpCode::JLT_U ) ) &&
         ( ( ( o.getOperationFormat() == TC13::OperationFormat::DDL_1 ) &&
             ( ( firstOp() < nthOp( 1 ) ) == WIR_L4::b0 ) ) ||
           ( ( nthParam( 1 ).getType() == WIR_ParameterType::imm ) &&
             ( ( firstOp() < nthImm( 1 ).extend( 32 ) ) == WIR_L4::b0 ) ) ) ) {
    mNewInstructions.emplace(
      const_cast<WIR_Operation &>( o ), list<WIR_Instruction> {} );
    return( true );
  }

  // JLTZ Da, disp4 -> J disp8
  if ( ( o.getOpCode() == TC13::OpCode::JLTZ ) &&
       ( ( firstOp() <
             WIR_UpDownValue( WIR_L4::b0, 32, true ) ) == WIR_L4::b1 ) ) {
    mNewInstructions.emplace(
      const_cast<WIR_Operation &>( o ),
      getJ( dynamic_cast<WIR_LabelParameter &>( lastParam() ) ) );
    return( true );
  }

  // JLTZ Da, disp4 -> <nothing>
  if ( ( o.getOpCode() == TC13::OpCode::JLTZ ) &&
       ( ( firstOp() <
             WIR_UpDownValue( WIR_L4::b0, 32, true ) ) == WIR_L4::b0 ) ) {
    mNewInstructions.emplace(
      const_cast<WIR_Operation &>( o ), list<WIR_Instruction> {} );
    return( true );
  }

  // JNE Da, Db, disp15 -> J disp8
  // JNE Da, imm, disp15 -> J disp8
  // JNE.A Aa, Ab, disp15 -> J disp8
  if ( ( ( o.getOpCode() == TC13::OpCode::JNE ) ||
         ( o.getOpCode() == TC13::OpCode::JNE_A ) ) &&
         ( ( ( nthParam( 1 ).getType() == WIR_ParameterType::reg ) &&
             ( ( firstOp() != nthOp( 1 ) ) == WIR_L4::b1 ) ) ||
           ( ( nthParam( 1 ).getType() == WIR_ParameterType::imm ) &&
             ( ( firstOp() != nthImm( 1 ).extend( 32 ) ) == WIR_L4::b1 ) ) ) ) {
    mNewInstructions.emplace(
      const_cast<WIR_Operation &>( o ),
      getJ( dynamic_cast<WIR_LabelParameter &>( lastParam() ) ) );
    return( true );
  }

  // JNE Da, Db, disp15 -> <nothing>
  // JNE Da, imm, disp15 -> <nothing>
  // JNE.A Aa, Ab, disp15 -> <nothing>
  if ( ( ( o.getOpCode() == TC13::OpCode::JNE ) ||
         ( o.getOpCode() == TC13::OpCode::JNE_A ) ) &&
         ( ( ( nthParam( 1 ).getType() == WIR_ParameterType::reg ) &&
             ( ( firstOp() != nthOp( 1 ) ) == WIR_L4::b0 ) ) ||
           ( ( nthParam( 1 ).getType() == WIR_ParameterType::imm ) &&
             ( ( firstOp() != nthImm( 1 ).extend( 32 ) ) == WIR_L4::b0 ) ) ) ) {
    mNewInstructions.emplace(
      const_cast<WIR_Operation &>( o ), list<WIR_Instruction> {} );
    return( true );
  }

  // JNED Da, Da, disp15 -> ADD Da, -1
  if ( ( o.getOpCode() == TC13::OpCode::JNED ) &&
       ( o.getOperationFormat() == TC13::OperationFormat::DDL_2 ) &&
       ( firstReg() == nthReg( 1 ) ) ) {
    mNewInstructions.emplace(
      const_cast<WIR_Operation &>( o ), getADD( firstParam(), -1 ) );
    return( true );
  }

  // JNED Da, Db, disp15 -> ADD Da, -1
  // JNED Da, imm, disp15 -> ADD Da, -1
  if ( ( o.getOpCode() == TC13::OpCode::JNED ) &&
       ( ( ( nthParam( 1 ).getType() == WIR_ParameterType::reg ) &&
           ( ( firstOp() == nthOp( 1 ) ) == WIR_L4::b1 ) ) ||
         ( ( nthParam( 1 ).getType() == WIR_ParameterType::imm ) &&
           ( ( firstOp() == nthImm( 1 ).extend( 32 ) ) == WIR_L4::b1 ) ) ) ) {
    mNewInstructions.emplace(
      const_cast<WIR_Operation &>( o ), getADD( firstParam(), -1 ) );
    return( true );
  }

  // JNEI Da, Da, disp15 -> ADD Da, 1
  if ( ( o.getOpCode() == TC13::OpCode::JNEI ) &&
       ( o.getOperationFormat() == TC13::OperationFormat::DDL_2 ) &&
       ( firstReg() == nthReg( 1 ) ) ) {
    mNewInstructions.emplace(
      const_cast<WIR_Operation &>( o ), getADD( firstParam(), 1 ) );
    return( true );
  }

  // JNEI Da, Db, disp15 -> ADD Da, 1
  // JNEI Da, imm, disp15 -> ADD Da, 1
  if ( ( o.getOpCode() == TC13::OpCode::JNEI ) &&
       ( ( ( nthParam( 1 ).getType() == WIR_ParameterType::reg ) &&
           ( ( firstOp() == nthOp( 1 ) ) == WIR_L4::b1 ) ) ||
         ( ( nthParam( 1 ).getType() == WIR_ParameterType::imm ) &&
           ( ( firstOp() == nthImm( 1 ).extend( 32 ) ) == WIR_L4::b1 ) ) ) ) {
    mNewInstructions.emplace(
      const_cast<WIR_Operation &>( o ), getADD( firstParam(), 1 ) );
    return( true );
  }

  // JNZ Da, disp4 -> J disp8
  // JNZ.A Aa, disp4 -> J disp8
  if ( ( ( o.getOpCode() == TC13::OpCode::JNZ ) ||
         ( o.getOpCode() == TC13::OpCode::JNZ_A ) ) &&
       ( ( firstOp() !=
             WIR_UpDownValue( WIR_L4::b0, 32, true ) ) == WIR_L4::b1 ) ) {
    mNewInstructions.emplace(
      const_cast<WIR_Operation &>( o ),
      getJ( dynamic_cast<WIR_LabelParameter &>( lastParam() ) ) );
    return( true );
  }

  // JNZ Da, disp4 -> <nothing>
  // JNZ.A Aa, disp4 -> <nothing>
  if ( ( ( o.getOpCode() == TC13::OpCode::JNZ ) ||
         ( o.getOpCode() == TC13::OpCode::JNZ_A ) ) &&
       ( ( firstOp() !=
             WIR_UpDownValue( WIR_L4::b0, 32, true ) ) == WIR_L4::b0 ) ) {
    mNewInstructions.emplace(
      const_cast<WIR_Operation &>( o ), list<WIR_Instruction> {} );
    return( true );
  }

  // JNZ.T Da, n, disp15 -> J disp8
  if ( ( o.getOpCode() == TC13::OpCode::JNZ_T ) &&
       ( ( firstOp().extract( nthImm( 1 ).getSignedValue(), 1 ) !=
             WIR_UpDownValue( WIR_L4::b0, 1, true ) ) == WIR_L4::b1 ) ) {
    mNewInstructions.emplace(
      const_cast<WIR_Operation &>( o ),
      getJ( dynamic_cast<WIR_LabelParameter &>( lastParam() ) ) );
    return( true );
  }

  // JNZ.T Da, n, disp15 -> <nothing>
  if ( ( o.getOpCode() == TC13::OpCode::JNZ_T ) &&
       ( ( firstOp().extract( nthImm( 1 ).getSignedValue(), 1 ) !=
             WIR_UpDownValue( WIR_L4::b0, 1, true ) ) == WIR_L4::b0 ) ) {
    mNewInstructions.emplace(
      const_cast<WIR_Operation &>( o ), list<WIR_Instruction> {} );
    return( true );
  }

  // JZ Da, disp4 -> J disp8
  // JZ.A Aa, disp4 -> J disp8
  if ( ( ( o.getOpCode() == TC13::OpCode::JZ ) ||
         ( o.getOpCode() == TC13::OpCode::JZ_A ) ) &&
       ( ( firstOp() ==
             WIR_UpDownValue( WIR_L4::b0, 32, true ) ) == WIR_L4::b1 ) ) {
    mNewInstructions.emplace(
      const_cast<WIR_Operation &>( o ),
      getJ( dynamic_cast<WIR_LabelParameter &>( lastParam() ) ) );
    return( true );
  }

  // JZ Da, disp4 -> <nothing>
  // JZ.A Aa, disp4 -> <nothing>
  if ( ( ( o.getOpCode() == TC13::OpCode::JZ ) ||
         ( o.getOpCode() == TC13::OpCode::JZ_A ) ) &&
       ( ( firstOp() ==
             WIR_UpDownValue( WIR_L4::b0, 32, true ) ) == WIR_L4::b0 ) ) {
    mNewInstructions.emplace(
      const_cast<WIR_Operation &>( o ), list<WIR_Instruction> {} );
    return( true );
  }

  // JZ.T Da, n, disp15 -> J disp8
  if ( ( o.getOpCode() == TC13::OpCode::JZ_T ) &&
       ( ( firstOp().extract( nthImm( 1 ).getSignedValue(), 1 ) ==
             WIR_UpDownValue( WIR_L4::b0, 1, true ) ) == WIR_L4::b1 ) ) {
    mNewInstructions.emplace(
      const_cast<WIR_Operation &>( o ),
      getJ( dynamic_cast<WIR_LabelParameter &>( lastParam() ) ) );
    return( true );
  }

  // JZ.T Da, n, disp15 -> <nothing>
  if ( ( o.getOpCode() == TC13::OpCode::JZ_T ) &&
       ( ( firstOp().extract( nthImm( 1 ).getSignedValue(), 1 ) ==
             WIR_UpDownValue( WIR_L4::b0, 1, true ) ) == WIR_L4::b0 ) ) {
    mNewInstructions.emplace(
      const_cast<WIR_Operation &>( o ), list<WIR_Instruction> {} );
    return( true );
  }

  return( false );
};


/*
  For an operation identified to be constant, doMOVFolding performs TriCore-
  specific folding into register MOV operations.

  If the computations of an operation are statically known to have no
  arithmetical effect, doMOVFolding folds the operation into a register MOV.

  doMOVFolding does not actually modify the currently examined WIR operation o.
  Instead, new instructions realizing the constant folding of o are added to map
  mNewInstructions.
*/
bool TC_ConstFold::doMOVFolding( const WIR_Operation &o,
                                 const std::map<WIR_id_t, WIR_UpDownValue> &inValue )
{
  DSTART(
    "bool TC_ConstFold::doMOVFolding(const WIR_Operation&, const map<long long unsigned int, WIR_UpDownValue>&)" );

  // A small lambda to retrieve the in-value of o's first explicit parameter.
  auto firstOp = [&]( void ) -> const WIR_UpDownValue & {
    auto &p = o.getExplicitParameters().front().get();

    // Get up value from inValues map.
    return( inValue.at( p.getID() ) );
  };

  // A small lambda to retrieve the in-value of o's last explicit parameter.
  auto lastOp = [&]( void ) -> const WIR_UpDownValue & {
    auto &p = o.getExplicitParameters().back().get();

    // Get up value from inValues map.
    return( inValue.at( p.getID() ) );
  };

  // A small lambda to retrieve the in-value of o's nth explicit parameter.
  auto nthOp = [&]( unsigned int n ) -> const WIR_UpDownValue & {
    auto it = o.getExplicitParameters().begin();
    std::advance( it, n );

    // Get up value from inValues map.
    return( inValue.at( it->get().getID() ) );
  };

  // A small lambda to retrieve o's first explicit register parameter.
  auto firstReg = [&]( void ) -> WIR_RegisterParameter & {
    return(
      dynamic_cast<WIR_RegisterParameter &>(
        o.getExplicitParameters().front().get() ) );
  };

  // A small lambda to retrieve o's last explicit register parameter.
  auto lastReg = [&]( void ) -> WIR_RegisterParameter & {
    return(
      dynamic_cast<WIR_RegisterParameter &>(
        o.getExplicitParameters().back().get() ) );
  };

  // A small lambda to retrieve o's nth explicit register parameter.
  auto nthReg = [&]( unsigned int n ) -> WIR_RegisterParameter & {
    auto it = o.getExplicitParameters().begin();
    std::advance( it, n );
    return(
      dynamic_cast<WIR_RegisterParameter &>( it->get() ) );
  };

  // A small lambda to retrieve o's last explicit parameter.
  auto lastParam = [&]( void ) -> WIR_Parameter & {
    return( o.getExplicitParameters().back().get() );
  };

  // ADD Dc, Da, <0> -> MOV Dc, Da
  // ADD.A Ac, Aa, <0> -> MOV.AA Ac, Aa
  if ( ( ( o.getOpCode() == TC13::OpCode::ADD ) ||
         ( o.getOpCode() == TC13::OpCode::ADD_A ) ) &&
       ( ( ( lastParam().getType() == WIR_ParameterType::reg ) &&
           lastOp().isInteger() &&
           ( replace( lastOp(), WIR_L4::b0 ) == 0 ) ) ||
         ( ( lastParam().getType() == WIR_ParameterType::imm ) &&
           ( dynamic_cast<WIR_BaseImmediateParameter &>(
               lastParam() ).getSignedValue() == 0 ) ) ) ) {
    mNewInstructions.emplace(
      const_cast<WIR_Operation &>( o ),
      o.getExplicitParameters().size() != 2 ?
        getMOV( firstReg(), nthReg( 1 ) ) : getMOV( firstReg(), firstReg() ) );
    return( true );
  }

  // ADD Dc, <0>, Db -> MOV Dc, Db
  // ADD.A Ac, <0>, Ab -> MOV.AA Ac, Ab
  if ( ( ( o.getOpCode() == TC13::OpCode::ADD ) ||
         ( o.getOpCode() == TC13::OpCode::ADD_A ) ) &&
       ( lastParam().getType() == WIR_ParameterType::reg ) &&
       ( ( ( o.getExplicitParameters().size() == 3 ) &&
           nthOp( 1 ).isInteger() &&
           ( replace( nthOp( 1 ), WIR_L4::b0 ) == 0 ) ) ||
         ( ( o.getExplicitParameters().size() == 2 ) &&
           firstOp().isInteger() &&
           ( replace( firstOp(), WIR_L4::b0 ) == 0 ) ) ) ) {
    mNewInstructions.emplace(
      const_cast<WIR_Operation &>( o ), getMOV( firstReg(), lastReg() ) );
    return( true );
  }

  // AND Dc, Da, Da -> MOV Dc, Da
  // AND Da, Da -> MOV Da, Da
  if ( ( o.getOpCode() == TC13::OpCode::AND ) &&
       ( lastParam().getType() == WIR_ParameterType::reg ) &&
       ( ( ( o.getExplicitParameters().size() == 3 ) &&
           ( nthReg( 1 ).getRegister() == lastReg().getRegister() ) ) ||
         ( ( o.getExplicitParameters().size() == 2 ) &&
           ( firstReg().getRegister() == lastReg().getRegister() ) ) ) ) {
    mNewInstructions.emplace(
      const_cast<WIR_Operation &>( o ), getMOV( firstReg(), lastReg() ) );
    return( true );
  }

  // AND Dc, Da, <val of Da> -> MOV Dc, Da
  // AND Da, <val of Da> -> MOV Da, Da
  if ( ( o.getOpCode() == TC13::OpCode::AND ) &&
       ( lastParam().getType() == WIR_ParameterType::reg ) &&
       ( ( ( o.getExplicitParameters().size() == 3 ) &&
           ( ( nthOp( 1 ) == lastOp().extend( 32 ) ) == WIR_L4::b1 ) ) ||
         ( ( o.getExplicitParameters().size() == 2 ) &&
           ( ( firstOp() == lastOp().extend( 32 ) ) == WIR_L4::b1 ) ) ) ) {
    mNewInstructions.emplace(
      const_cast<WIR_Operation &>( o ),
      o.getExplicitParameters().size() != 2 ?
        getMOV( firstReg(), nthReg( 1 ) ) : getMOV( firstReg(), firstReg() ) );
exit( 4 );
    return( true );
  }

  // AND Dc, Da, <immediate val of Da> -> MOV Dc, Da
  // AND Da, <immediate val of Da> -> MOV Da, Da
  if ( ( o.getOpCode() == TC13::OpCode::AND ) &&
       ( lastParam().getType() == WIR_ParameterType::imm ) &&
       ( ( ( o.getExplicitParameters().size() == 3 ) &&
           ( replace( nthOp( 1 ), WIR_L4::b0 ) ==
               (long long) dynamic_cast<WIR_BaseImmediateParameter &>(
                 lastParam() ).getUnsignedValue() ) ) ||
         ( ( o.getExplicitParameters().size() == 2 ) &&
           ( replace( firstOp(), WIR_L4::b0 ) ==
               (long long) dynamic_cast<WIR_BaseImmediateParameter &>(
                 lastParam() ).getUnsignedValue() ) ) ) ) {
    mNewInstructions.emplace(
      const_cast<WIR_Operation &>( o ),
      o.getExplicitParameters().size() != 2 ?
        getMOV( firstReg(), nthReg( 1 ) ) : getMOV( firstReg(), firstReg() ) );
    return( true );
  }

  // LEA Aa, [Ab] 0 -> MOV.AA Aa, Ab
  if ( ( o.getOpCode() == TC13::OpCode::LEA ) &&
       ( ( o.getOperationFormat() == TC13::OperationFormat::AAC10BOA ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::AAC16BOA ) ) &&
       ( dynamic_cast<WIR_BaseImmediateParameter &>(
           lastParam() ).getSignedValue() == 0 ) ) {
    mNewInstructions.emplace(
      const_cast<WIR_Operation &>( o ), getMOV( firstReg(), nthReg( 1 ) ) );
    return( true );
  }

  // MAX Dc, Da, Da -> MOV Dc, Da
  // MAX.U Dc, Da, Da -> MOV Dc, Da
  // MAX.B Dc, Da, Da -> MOV Dc, Da
  // MAX.BU Dc, Da, Da -> MOV Dc, Da
  // MAX.H Dc, Da, Da -> MOV Dc, Da
  // MAX.HU Dc, Da, Da -> MOV Dc, Da
  if ( ( ( o.getOpCode() == TC13::OpCode::MAX ) ||
         ( o.getOpCode() == TC13::OpCode::MAX_U ) ||
         ( o.getOpCode() == TC13::OpCode::MAX_B ) ||
         ( o.getOpCode() == TC13::OpCode::MAX_BU ) ||
         ( o.getOpCode() == TC13::OpCode::MAX_H ) ||
         ( o.getOpCode() == TC13::OpCode::MAX_HU ) ) &&
       ( o.getOperationFormat() == TC13::OperationFormat::DDD_1 ) &&
       ( nthReg( 1 ).getRegister() == lastReg().getRegister() ) ) {
    mNewInstructions.emplace(
      const_cast<WIR_Operation &>( o ), getMOV( firstReg(), lastReg() ) );
exit( 2 );
    return( true );
  }

  // MAX Dc, Da, <val of Da> -> MOV Dc, Da
  // MAX.U Dc, Da, <val of Da> -> MOV Dc, Da
  // MAX.B Dc, Da, <val of Da> -> MOV Dc, Da
  // MAX.BU Dc, Da, <val of Da> -> MOV Dc, Da
  // MAX.H Dc, Da, <val of Da> -> MOV Dc, Da
  // MAX.HU Dc, Da, <val of Da> -> MOV Dc, Da
  if ( ( ( o.getOpCode() == TC13::OpCode::MAX ) ||
         ( o.getOpCode() == TC13::OpCode::MAX_U ) ||
         ( o.getOpCode() == TC13::OpCode::MAX_B ) ||
         ( o.getOpCode() == TC13::OpCode::MAX_BU ) ||
         ( o.getOpCode() == TC13::OpCode::MAX_H ) ||
         ( o.getOpCode() == TC13::OpCode::MAX_HU ) ) &&
       ( o.getOperationFormat() == TC13::OperationFormat::DDD_1 ) &&
       ( ( nthOp( 1 ) == lastOp().extend( 32 ) ) == WIR_L4::b1 ) ) {
    mNewInstructions.emplace(
      const_cast<WIR_Operation &>( o ), getMOV( firstReg(), nthReg( 1 ) ) );
exit( 3 );
    return( true );
  }

  // MIN Dc, Da, Da -> MOV Dc, Da
  // MIN.U Dc, Da, Da -> MOV Dc, Da
  // MIN.B Dc, Da, Da -> MOV Dc, Da
  // MIN.BU Dc, Da, Da -> MOV Dc, Da
  // MIN.H Dc, Da, Da -> MOV Dc, Da
  // MIN.HU Dc, Da, Da -> MOV Dc, Da
  if ( ( ( o.getOpCode() == TC13::OpCode::MIN ) ||
         ( o.getOpCode() == TC13::OpCode::MIN_U ) ||
         ( o.getOpCode() == TC13::OpCode::MIN_B ) ||
         ( o.getOpCode() == TC13::OpCode::MIN_BU ) ||
         ( o.getOpCode() == TC13::OpCode::MIN_H ) ||
         ( o.getOpCode() == TC13::OpCode::MIN_HU ) ) &&
       ( o.getOperationFormat() == TC13::OperationFormat::DDD_1 ) &&
       ( nthReg( 1 ).getRegister() == lastReg().getRegister() ) ) {
    mNewInstructions.emplace(
      const_cast<WIR_Operation &>( o ), getMOV( firstReg(), lastReg() ) );
exit( 6 );
    return( true );
  }

  // MIN Dc, Da, <val of Da> -> MOV Dc, Da
  // MIN.U Dc, Da, <val of Da> -> MOV Dc, Da
  // MIN.B Dc, Da, <val of Da> -> MOV Dc, Da
  // MIN.BU Dc, Da, <val of Da> -> MOV Dc, Da
  // MIN.H Dc, Da, <val of Da> -> MOV Dc, Da
  // MIN.HU Dc, Da, <val of Da> -> MOV Dc, Da
  if ( ( ( o.getOpCode() == TC13::OpCode::MIN ) ||
         ( o.getOpCode() == TC13::OpCode::MIN_U ) ||
         ( o.getOpCode() == TC13::OpCode::MIN_B ) ||
         ( o.getOpCode() == TC13::OpCode::MIN_BU ) ||
         ( o.getOpCode() == TC13::OpCode::MIN_H ) ||
         ( o.getOpCode() == TC13::OpCode::MIN_HU ) ) &&
       ( o.getOperationFormat() == TC13::OperationFormat::DDD_1 ) &&
       ( ( nthOp( 1 ) == lastOp().extend( 32 ) ) == WIR_L4::b1 ) ) {
    mNewInstructions.emplace(
      const_cast<WIR_Operation &>( o ), getMOV( firstReg(), nthReg( 1 ) ) );
exit( 7 );
    return( true );
  }

  // MUL Dc, Da, <1> -> MOV Dc, Da
  // MUL.U Dc, Da, <1> -> MOV Dc, Da
  if ( ( ( o.getOpCode() == TC13::OpCode::MUL ) ||
         ( o.getOpCode() == TC13::OpCode::MUL_U ) ) &&
       ( firstReg().getRegister().getBitWidth() == 32 ) &&
       ( ( ( lastParam().getType() == WIR_ParameterType::reg ) &&
           lastOp().isInteger() &&
           ( replace( lastOp(), WIR_L4::b0 ) == 1 ) ) ||
         ( ( lastParam().getType() == WIR_ParameterType::imm ) &&
           ( dynamic_cast<WIR_BaseImmediateParameter &>(
               lastParam() ).getSignedValue() == 1 ) ) ) ) {
    mNewInstructions.emplace(
      const_cast<WIR_Operation &>( o ),
      o.getExplicitParameters().size() != 2 ?
        getMOV( firstReg(), nthReg( 1 ) ) : getMOV( firstReg(), firstReg() ) );
exit( 8 );
    return( true );
  }

  // MUL Dc, <1>, Db -> MOV Dc, Db
  // MUL.U Dc, <1>, Db -> MOV Dc, Db
  if ( ( ( o.getOpCode() == TC13::OpCode::MUL ) ||
         ( o.getOpCode() == TC13::OpCode::MUL_U ) ) &&
       ( lastParam().getType() == WIR_ParameterType::reg ) &&
       ( ( ( o.getExplicitParameters().size() == 3 ) &&
           nthOp( 1 ).isInteger() &&
           ( replace( nthOp( 1 ), WIR_L4::b0 ) == 1 ) ) ||
         ( ( o.getExplicitParameters().size() == 2 ) &&
           firstOp().isInteger() &&
           ( replace( firstOp(), WIR_L4::b0 ) == 1 ) ) ) ) {
    mNewInstructions.emplace(
      const_cast<WIR_Operation &>( o ), getMOV( firstReg(), lastReg() ) );
exit( 9 );
    return( true );
  }

  // OR Dc, Da, Da -> MOV Dc, Da
  // OR Da, Da -> MOV Da, Da
  if ( ( o.getOpCode() == TC13::OpCode::OR ) &&
       ( lastParam().getType() == WIR_ParameterType::reg ) &&
       ( ( ( o.getExplicitParameters().size() == 3 ) &&
           ( nthReg( 1 ).getRegister() == lastReg().getRegister() ) ) ||
         ( ( o.getExplicitParameters().size() == 2 ) &&
           ( firstReg().getRegister() == lastReg().getRegister() ) ) ) ) {
    mNewInstructions.emplace(
      const_cast<WIR_Operation &>( o ), getMOV( firstReg(), lastReg() ) );
    return( true );
  }

  // OR Dc, Da, <val of Da> -> MOV Dc, Da
  // OR Da, <val of Da> -> MOV Da, Da
  if ( ( o.getOpCode() == TC13::OpCode::OR ) &&
       ( lastParam().getType() == WIR_ParameterType::reg ) &&
       ( ( ( o.getExplicitParameters().size() == 3 ) &&
           ( ( nthOp( 1 ) == lastOp().extend( 32 ) ) == WIR_L4::b1 ) ) ||
         ( ( o.getExplicitParameters().size() == 2 ) &&
           ( ( firstOp() == lastOp().extend( 32 ) ) == WIR_L4::b1 ) ) ) ) {
    mNewInstructions.emplace(
      const_cast<WIR_Operation &>( o ),
      o.getExplicitParameters().size() != 2 ?
        getMOV( firstReg(), nthReg( 1 ) ) : getMOV( firstReg(), firstReg() ) );
exit( 11 );
    return( true );
  }

  // OR Dc, Da, <immediate val of Da> -> MOV Dc, Da
  // OR Da, <immediate val of Da> -> MOV Da, Da
  if ( ( o.getOpCode() == TC13::OpCode::OR ) &&
       ( lastParam().getType() == WIR_ParameterType::imm ) &&
       ( ( ( o.getExplicitParameters().size() == 3 ) &&
           ( replace( nthOp( 1 ), WIR_L4::b0 ) ==
               (long long) dynamic_cast<WIR_BaseImmediateParameter &>(
                 lastParam() ).getUnsignedValue() ) ) ||
         ( ( o.getExplicitParameters().size() == 2 ) &&
           ( replace( firstOp(), WIR_L4::b0 ) ==
               (long long) dynamic_cast<WIR_BaseImmediateParameter &>(
                 lastParam() ).getUnsignedValue() ) ) ) ) {
    mNewInstructions.emplace(
      const_cast<WIR_Operation &>( o ),
      o.getExplicitParameters().size() != 2 ?
        getMOV( firstReg(), nthReg( 1 ) ) : getMOV( firstReg(), firstReg() ) );
exit( 12 );
    return( true );
  }

  // SEL Dc, <!0>, Da, Db -> MOV Dc, Da
  if ( ( o.getOpCode() == TC13::OpCode::SEL ) &&
       ( ( nthOp( 1 ) != WIR_UpDownValue( WIR_L4::b0, 32 ) ) == WIR_L4::b1 ) ) {
    mNewInstructions.emplace(
      const_cast<WIR_Operation &>( o ), getMOV( firstReg(), nthReg( 2 ) ) );
  return( true );
  }

  // SEL Dc, <0>, Da, Db -> MOV Dc, Db
  if ( ( o.getOpCode() == TC13::OpCode::SEL ) &&
       ( o.getOperationFormat() == TC13::OperationFormat::DDDD ) &&
       ( ( nthOp( 1 ) != WIR_UpDownValue( WIR_L4::b0, 32 ) ) == WIR_L4::b0 ) ) {
    mNewInstructions.emplace(
      const_cast<WIR_Operation &>( o ), getMOV( firstReg(), lastReg() ) );
exit( 14 );
  return( true );
  }

  // SELN Dc, <0>, Da, Db -> MOV Dc, Da
  if ( ( o.getOpCode() == TC13::OpCode::SELN ) &&
       ( ( nthOp( 1 ) == WIR_UpDownValue( WIR_L4::b0, 32 ) ) == WIR_L4::b1 ) ) {
    mNewInstructions.emplace(
      const_cast<WIR_Operation &>( o ), getMOV( firstReg(), nthReg( 2 ) ) );
exit( 15 );
  return( true );
  }

  // SELN Dc, <!0>, Da, Db -> MOV Dc, Db
  if ( ( o.getOpCode() == TC13::OpCode::SEL ) &&
       ( o.getOperationFormat() == TC13::OperationFormat::DDDD ) &&
       ( ( nthOp( 1 ) == WIR_UpDownValue( WIR_L4::b0, 32 ) ) == WIR_L4::b0 ) ) {
    mNewInstructions.emplace(
      const_cast<WIR_Operation &>( o ), getMOV( firstReg(), lastReg() ) );
exit( 16 );
  return( true );
  }

  return( false );
};


/*
  getLEA generates TriCore instructions loading an address register with a
  constant.
*/
list<WIR_Instruction> TC_ConstFold::getLEA( const WIR_Parameter &p,
                                            const WIR_UpDownValue &v ) const
{
  DSTART(
    "list<WIR_Instruction> TC_ConstFold::getLEA(const WIR_Parameter&, const WIR_UpDownValue&) const" );

  list<WIR_Instruction> res;
  auto &r = dynamic_cast<const WIR_RegisterParameter &>( p ).getRegister();
  auto &rp = insertLEA( r, v, res );

  // Copy bit-value container to the finally produced register parameter.
  auto *cont = new WIR_BitValues();
  rp.insertContainer( cont );

  for ( auto &outEdge :
          p.getContainers<WIR_BitValues>().begin()->get().getOutValues() ) {
    cont->insertOutValues(
      *(outEdge.rp), move( outEdge.downVal ), move( outEdge.upVal ) );

    // Patch target container so that it receives input data from rp.
    auto &tgtContainer =
      outEdge.rp->getContainers<WIR_BitValues>().begin()->get();
    auto inEdgeIt = tgtContainer.findInValues( p );
    inEdgeIt->rp = &rp;
  }

  for ( WIR_Instruction &i : res )
    for ( WIR_Operation &o : i )
      copyContainers( o, p.getOperation() );

  return( res );
};


/*
  getLEA_P generates TriCore instructions loading an extended address register
  with a constant.
*/
list<WIR_Instruction> TC_ConstFold::getLEA_P( const WIR_Parameter &p,
                                              const WIR_UpDownValue &v ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  #ifdef FAILSAFEMODE
  ufAssert( v.getBitWidth() == 64 );
  #endif

  list<WIR_Instruction> res;
  auto &r = dynamic_cast<const WIR_RegisterParameter &>( p ).getRegister();

  // Determine child registers of r.
  auto childs = getChilds( r );
  WIR_BaseRegister &child1 = *(childs.first);
  WIR_BaseRegister &child2 = *(childs.second);

  // Determine whether both childs of r are actually used according to the data
  // flow.
  auto childsUsed = areChildsUsed( p, child1, child2 );
  bool child1IsUsed = childsUsed.first;
  bool child2IsUsed = childsUsed.second;

  if ( child1IsUsed ) {
    // Load least-significant 32 bits into first child of r.
    auto &rp = insertLEA( child1, v.extract( 0, 32 ), res );

    // Copy bit-value container to the finally produced register parameter.
    auto *cont = new WIR_BitValues();
    rp.insertContainer( cont );

    for ( auto &outEdge :
            p.getContainers<WIR_BitValues>().begin()->get().getOutValues() ) {
      auto &tgtRegister =
        dynamic_cast<const WIR_RegisterParameter &>(
          *(outEdge.rp) ).getRegister();

      if ( ( tgtRegister == r ) || ( tgtRegister == child1 ) ) {
        cont->insertOutValues(
          *(outEdge.rp), outEdge.downVal.extract( 0, 32 ),
          outEdge.upVal.extract( 0, 32 ) );

        // Patch target container so that it receives input data from rp.
        auto &tgtContainer =
          outEdge.rp->getContainers<WIR_BitValues>().begin()->get();
        auto inEdgeIt = tgtContainer.findInValues( p );

        tgtContainer.insertInValues(
          rp, inEdgeIt->downVal.extract( 0, 32 ),
          inEdgeIt->upVal.extract( 0, 32 ) );

        if ( dynamic_cast<const WIR_RegisterParameter &>(
               *(outEdge.rp) ).getRegister() == child1 )
          tgtContainer.eraseInValues( inEdgeIt );
      }
    }
  }

  if ( child2IsUsed ) {
    // Load most-significant 32 bits into second child of r.
    auto &rp = insertLEA( child2, v.extract( 32, 32 ), res );

    // Copy bit-value container to the finally produced register parameter.
    auto *cont = new WIR_BitValues();
    rp.insertContainer( cont );

    for ( auto &outEdge :
            p.getContainers<WIR_BitValues>().begin()->get().getOutValues() ) {
      auto &tgtRegister =
        dynamic_cast<const WIR_RegisterParameter &>(
          *(outEdge.rp) ).getRegister();

      if ( ( tgtRegister == r ) || ( tgtRegister == child2 ) ) {
        cont->insertOutValues(
          *(outEdge.rp), outEdge.downVal.extract( 32, 32 ),
          outEdge.upVal.extract( 32, 32 ) );

        // Patch target container so that it receives input data from rp.
        auto &tgtContainer =
          outEdge.rp->getContainers<WIR_BitValues>().begin()->get();
        auto inEdgeIt = tgtContainer.findInValues( p );
        inEdgeIt->rp = &rp;
        inEdgeIt->downVal = inEdgeIt->downVal.extract( 32, 32 );
        inEdgeIt->upVal = inEdgeIt->upVal.extract( 32, 32 );
      }
    }
  }

  for ( WIR_Instruction &i : res )
    for ( WIR_Operation &o : i )
      copyContainers( o, p.getOperation() );

  return( res );
};


/*
  getMOV generates TriCore instructions loading a data register with a constant.
*/
list<WIR_Instruction> TC_ConstFold::getMOV( const WIR_Parameter &p,
                                            const WIR_UpDownValue &v ) const
{
  DSTART(
    "list<WIR_Instruction> TC_ConstFold::getMOV(const WIR_Parameter&, const WIR_UpDownValue&) const" );

  list<WIR_Instruction> res;
  auto &r = dynamic_cast<const WIR_RegisterParameter &>( p ).getRegister();
  auto &rp = insertMOV( r, v, res );

  // Copy bit-value container to the finally produced register parameter.
  auto *cont = new WIR_BitValues();
  rp.insertContainer( cont );

  for ( auto &outEdge :
          p.getContainers<WIR_BitValues>().begin()->get().getOutValues() ) {
    cont->insertOutValues(
      *(outEdge.rp), move( outEdge.downVal ), move( outEdge.upVal ) );

    // Patch target container so that it receives input data from rp.
    auto &tgtContainer =
      outEdge.rp->getContainers<WIR_BitValues>().begin()->get();
    auto inEdgeIt = tgtContainer.findInValues( p );
    inEdgeIt->rp = &rp;
  }

  for ( WIR_Instruction &i : res )
    for ( WIR_Operation &o : i )
      copyContainers( o, p.getOperation() );

  return( res );
};


/*
  getMOV generates TriCore instructions performing a register-register MOV.

  Depending on whether t is a data or an address register, getMOV generates
  either a MOV_RR or a MOV_AA operation.
*/
list<WIR_Instruction> TC_ConstFold::getMOV( const WIR_RegisterParameter &t,
                                            const WIR_RegisterParameter &s )
{
  DSTART(
    "list<WIR_Instruction> TC_ConstFold::getMOV(const WIR_RegisterParameter&, const WIR_RegisterParameter&)" );

  list<WIR_Instruction> res;

  auto *rp1 = new WIR_RegisterParameter( t.getRegister(), WIR_Usage::def );
  auto *rp2 = new WIR_RegisterParameter( s.getRegister(), WIR_Usage::use );

  if ( t.getRegister().getType() == TC13::RegisterType::dReg )
    res.push_back(
      { { TC13::OpCode::MOV_RR,
          m16BitOperations ?
            TC13::OperationFormat::SDD_1 : TC13::OperationFormat::DD,
          rp1, rp2 } } );
  else
    res.push_back(
      { { TC13::OpCode::MOV_AA,
          m16BitOperations ?
            TC13::OperationFormat::SAA_1 : TC13::OperationFormat::AA,
          rp1, rp2 } } );

  // Ensure that all locations in the bit-wise DFG refering to the old parameter
  // t will be updated for the new parameter rp1.
  mNewLocation.insert( make_pair( t.getID(), ref( *rp1 ) ) );

  // Copy bit-value container to the finally produced register parameter.
  auto *cont1 = new WIR_BitValues();
  rp1->insertContainer( cont1 );

  for ( auto &outEdge :
          t.getContainers<WIR_BitValues>().begin()->get().getOutValues() ) {
    cont1->insertOutValues(
      *(outEdge.rp), move( outEdge.downVal ), move( outEdge.upVal ) );

    // Patch target container so that it receives input data from rp1.
    auto &tgtContainer =
      outEdge.rp->getContainers<WIR_BitValues>().begin()->get();
    auto inEdgeIt = tgtContainer.findInValues( t );
    inEdgeIt->rp = rp1;
  }

  // Copy bit-value container to the finally used register parameter.
  auto *cont2 = new WIR_BitValues();
  rp2->insertContainer( cont2 );

  for ( auto &inEdge :
          s.getContainers<WIR_BitValues>().begin()->get().getInValues() ) {
    cont2->insertInValues(
      *(inEdge.rp), move( inEdge.downVal ), move( inEdge.upVal ) );

    // Patch source container so that it provides input data to rp2.
    auto &srcContainer =
      inEdge.rp->getContainers<WIR_BitValues>().begin()->get();
    auto outEdgeIt = srcContainer.findOutValues( s );
    outEdgeIt->rp = rp2;
  }

  for ( WIR_Instruction &i : res )
    for ( WIR_Operation &o : i )
      copyContainers( o, t.getOperation() );

  return( res );
};


/*
  getMOV_E generates TriCore instructions loading an extended data register with
  a constant.
*/
list<WIR_Instruction> TC_ConstFold::getMOV_E( const WIR_Parameter &p,
                                              const WIR_UpDownValue &v ) const
{
  DSTART(
    "list<WIR_Instruction> TC_ConstFold::getMOV_E(const WIR_Parameter&, const WIR_UpDownValue&) const" );

  #ifdef FAILSAFEMODE
  ufAssert( v.getBitWidth() == 64 );
  #endif

  list<WIR_Instruction> res;
  auto &r = dynamic_cast<const WIR_RegisterParameter &>( p ).getRegister();

  // Determine child registers of r.
  auto childs = getChilds( r );
  WIR_BaseRegister &child1 = *(childs.first);
  WIR_BaseRegister &child2 = *(childs.second);

  // Determine whether both childs of r are actually used according to the data
  // flow.
  auto childsUsed = areChildsUsed( p, child1, child2 );
  bool child1IsUsed = childsUsed.first;
  bool child2IsUsed = childsUsed.second;

  if ( child1IsUsed ) {
    // Load least-significant 32 bits into first child of r.
    auto &rp = insertMOV( child1, v.extract( 0, 32 ), res );

    // Copy bit-value container to the finally produced register parameter.
    auto *cont = new WIR_BitValues();
    rp.insertContainer( cont );

    for ( auto &outEdge :
            p.getContainers<WIR_BitValues>().begin()->get().getOutValues() ) {
      auto &tgtRegister =
        dynamic_cast<const WIR_RegisterParameter &>(
          *(outEdge.rp) ).getRegister();

      if ( ( tgtRegister == r ) || ( tgtRegister == child1 ) ) {
        cont->insertOutValues(
          *(outEdge.rp), outEdge.downVal.extract( 0, 32 ),
          outEdge.upVal.extract( 0, 32 ) );

        // Patch target container so that it receives input data from rp.
        auto &tgtContainer =
          outEdge.rp->getContainers<WIR_BitValues>().begin()->get();
        auto inEdgeIt = tgtContainer.findInValues( p );

        tgtContainer.insertInValues(
          rp, inEdgeIt->downVal.extract( 0, 32 ),
          inEdgeIt->upVal.extract( 0, 32 ) );

        if ( dynamic_cast<const WIR_RegisterParameter &>(
               *(outEdge.rp) ).getRegister() == child1 )
          tgtContainer.eraseInValues( inEdgeIt );
      }
    }
  }

  if ( child2IsUsed ) {
    // Load most-significant 32 bits into second child of r.
    auto &rp = insertMOV( child2, v.extract( 32, 32 ), res );

    // Copy bit-value container to the finally produced register parameter.
    auto *cont = new WIR_BitValues();
    rp.insertContainer( cont );

    for ( auto &outEdge :
            p.getContainers<WIR_BitValues>().begin()->get().getOutValues() ) {
      auto &tgtRegister =
        dynamic_cast<const WIR_RegisterParameter &>(
          *(outEdge.rp) ).getRegister();

      if ( ( tgtRegister == r ) || ( tgtRegister == child2 ) ) {
        cont->insertOutValues(
          *(outEdge.rp), outEdge.downVal.extract( 32, 32 ),
          outEdge.upVal.extract( 32, 32 ) );

        // Patch target container so that it receives input data from rp.
        auto &tgtContainer =
          outEdge.rp->getContainers<WIR_BitValues>().begin()->get();
        auto inEdgeIt = tgtContainer.findInValues( p );
        inEdgeIt->rp = &rp;
        inEdgeIt->downVal = inEdgeIt->downVal.extract( 32, 32 );
        inEdgeIt->upVal = inEdgeIt->upVal.extract( 32, 32 );
      }
    }
  }

  for ( WIR_Instruction &i : res )
    for ( WIR_Operation &o : i )
      copyContainers( o, p.getOperation() );

  return( res );
};


/*
  getADD generates TriCore instructions incrementing a register by the given
  constant.
*/
std::list<WIR_Instruction> TC_ConstFold::getADD( const WIR_Parameter &p,
                                                 int v )
{
  DSTART(
    "list<WIR_Instruction> TC_ConstFold::getADD(const WIR_Parameter&, int)" );

  list<WIR_Instruction> res;
  auto &r = dynamic_cast<const WIR_RegisterParameter &>( p ).getRegister();

  if ( r.isVirtual() ) {
    if ( m16BitOperations )
      res.push_back(
        { { TC13::OpCode::ADD, TC13::OperationFormat::SDC4_2,
            new WIR_RegisterParameter(
              dynamic_cast<const TC_DRegV &>( r ), WIR_Usage::defuse ),
            new TC_Const4_Signed( v ) } } );
    else
      res.push_back(
        { { TC13::OpCode::ADD, TC13::OperationFormat::DDC9_1,
            new WIR_RegisterParameter(
              dynamic_cast<const TC_DRegV &>( r ), WIR_Usage::def ),
            new WIR_RegisterParameter(
              dynamic_cast<const TC_DRegV &>( r ), WIR_Usage::use ),
            new TC_Const9_Signed( v ) } } );
  } else {
    if ( m16BitOperations )
      res.push_back(
        { { TC13::OpCode::ADD, TC13::OperationFormat::SDC4_2,
            new WIR_RegisterParameter(
              dynamic_cast<const TC_DRegP &>( r ), WIR_Usage::defuse ),
            new TC_Const4_Signed( v ) } } );
    else
      res.push_back(
        { { TC13::OpCode::ADD, TC13::OperationFormat::DDC9_1,
            new WIR_RegisterParameter(
              dynamic_cast<const TC_DRegP &>( r ), WIR_Usage::def ),
            new WIR_RegisterParameter(
              dynamic_cast<const TC_DRegP &>( r ), WIR_Usage::use ),
            new TC_Const9_Signed( v ) } } );
  }

  auto &rp = res.front().begin()->get().getExplicitParameters().front().get();

  // Ensure that all locations in the bit-wise DFG refering to the old parameter
  // p will be updated for the new parameter rp.
  mNewLocation.insert(
    make_pair(
      p.getID(), ref( dynamic_cast<WIR_RegisterParameter &>( rp ) ) ) );

  // Copy bit-value container to the finally produced register parameter.
  auto *cont = new WIR_BitValues();
  rp.insertContainer( cont );

  for ( auto &outEdge :
          p.getContainers<WIR_BitValues>().begin()->get().getOutValues() ) {
    if ( outEdge.rp->getOperation() == p.getOperation() )
      continue;

    cont->insertOutValues(
      *(outEdge.rp), move( outEdge.downVal ), move( outEdge.upVal ) );

    // Patch target container so that it receives input data from rp.
    auto &tgtContainer =
      outEdge.rp->getContainers<WIR_BitValues>().begin()->get();
    auto inEdgeIt = tgtContainer.findInValues( p );
    inEdgeIt->rp = &rp;
  }

  auto &rp1 =
    ( res.front().begin()->get().getSize() == 2 ) ?
      res.front().begin()->get().getExplicitParameters().front().get() :
      next( res.front().begin()->get().getExplicitParameters().begin() )->get();

  // If a 32-bit ADD was created above, attach another bit-value container to
  // its incoming register parameter.
  if ( rp1 != rp ) {
    cont = new WIR_BitValues();
    rp1.insertContainer( cont );
  }

  for ( auto &inEdge :
          p.getContainers<WIR_BitValues>().begin()->get().getInValues() ) {
    if ( inEdge.rp->getOperation() == p.getOperation() )
      continue;

    cont->insertInValues(
      *(inEdge.rp), move( inEdge.downVal ), move( inEdge.upVal ) );

    // Patch source container so that it produces output data for rp1.
    auto &srcContainer =
      inEdge.rp->getContainers<WIR_BitValues>().begin()->get();
    auto outEdgeIt = srcContainer.findOutValues( p );
    outEdgeIt->rp = &rp1;
  }

  for ( WIR_Instruction &i : res )
    for ( WIR_Operation &o : i )
      copyContainers( o, p.getOperation() );

  return( res );
};


/*
  getADDSUB generates TriCore ADD or SUB instructions for a given original
  ADDX/SUBX instruction.
*/
std::list<WIR_Instruction> TC_ConstFold::getADDSUB( const WIR_Operation &o )
{
  DSTART(
    "list<WIR_Instruction> TC_ConstFold::getADDSUB(const WIR_Operation&)" );

  list<WIR_Instruction> res;

  if ( o.getOperationFormat() == TC13::OperationFormat::DDDPSW_1 )
    res.push_back(
      { { ( o.getOpCode() == TC13::OpCode::ADDX ) ?
              TC13::OpCode::ADD : TC13::OpCode::SUB,
          TC13::OperationFormat::DDD_1,
          new WIR_RegisterParameter(
            dynamic_cast<WIR_RegisterParameter &>( o.begin()->get() ) ),
          new WIR_RegisterParameter(
            dynamic_cast<WIR_RegisterParameter &>(
              next( o.begin() )->get() ) ),
          new WIR_RegisterParameter(
            dynamic_cast<WIR_RegisterParameter &>(
              next( next( o.begin() ) )->get() ) ) } } );
  else
    res.push_back(
      { { ( o.getOpCode() == TC13::OpCode::ADDX ) ?
              TC13::OpCode::ADD : TC13::OpCode::SUB,
          TC13::OperationFormat::DDC9_1,
          new WIR_RegisterParameter(
            dynamic_cast<WIR_RegisterParameter &>( o.begin()->get() ) ),
          new WIR_RegisterParameter(
            dynamic_cast<WIR_RegisterParameter &>(
              next( o.begin() )->get() ) ),
          new TC_Const9_Signed(
            dynamic_cast<TC_Const9_Signed &>(
              next( next( o.begin() ) )->get() ) ) } } );

  auto &p = o.getExplicitParameters().front().get();
  auto &rp = res.front().begin()->get().getExplicitParameters().front().get();

  // Ensure that all locations in the bit-wise DFG refering to the old parameter
  // p will be updated for the new parameter rp.
  mNewLocation.insert(
    make_pair(
      p.getID(), ref( dynamic_cast<WIR_RegisterParameter &>( rp ) ) ) );

  // Copy bit-value container to the finally produced register parameter.
  auto *cont = new WIR_BitValues();
  rp.insertContainer( cont );

  for ( auto &outEdge :
          p.getContainers<WIR_BitValues>().begin()->get().getOutValues() ) {
    if ( outEdge.rp->getOperation() == o )
      continue;

    cont->insertOutValues(
      *(outEdge.rp), move( outEdge.downVal ), move( outEdge.upVal ) );

    // Patch target container so that it receives input data from rp.
    auto &tgtContainer =
      outEdge.rp->getContainers<WIR_BitValues>().begin()->get();
    auto inEdgeIt = tgtContainer.findInValues( p );
    inEdgeIt->rp = &rp;
  }

  auto &p1 = next( o.getExplicitParameters().begin() )->get();
  auto &rp1 =
    next( res.front().begin()->get().getExplicitParameters().begin() )->get();

  cont = new WIR_BitValues();
  rp1.insertContainer( cont );

  for ( auto &inEdge :
          p1.getContainers<WIR_BitValues>().begin()->get().getInValues() ) {
    if ( inEdge.rp->getOperation() == o )
      continue;

    cont->insertInValues(
      *(inEdge.rp), move( inEdge.downVal ), move( inEdge.upVal ) );

    // Patch source container so that it produces output data for rp1.
    auto &srcContainer =
      inEdge.rp->getContainers<WIR_BitValues>().begin()->get();
    auto outEdgeIt = srcContainer.findOutValues( p1 );
    outEdgeIt->rp = &rp1;
  }

  if ( o.getOperationFormat() == TC13::OperationFormat::DDDPSW_1 ) {
    auto &p2 = next( next( o.getExplicitParameters().begin() ) )->get();
    auto &rp2 =
      next(
        next(
          res.front().begin()->get().getExplicitParameters().begin() ) )->get();

    cont = new WIR_BitValues();
    rp2.insertContainer( cont );

    for ( auto &inEdge :
            p2.getContainers<WIR_BitValues>().begin()->get().getInValues() ) {
      if ( inEdge.rp->getOperation() == o )
        continue;

      cont->insertInValues(
        *(inEdge.rp), move( inEdge.downVal ), move( inEdge.upVal ) );

      // Patch source container so that it produces output data for rp2.
      auto &srcContainer =
        inEdge.rp->getContainers<WIR_BitValues>().begin()->get();
      auto outEdgeIt = srcContainer.findOutValues( p2 );
      outEdgeIt->rp = &rp2;
    }
  }

  return( res );
};


/*
  getJ generates TriCore instructions unconditionally jumping to the given
  label.
*/
std::list<WIR_Instruction> TC_ConstFold::getJ( const WIR_LabelParameter &p ) const
{
  DSTART(
    "list<WIR_Instruction> TC_ConstFold::getJ(const WIR_LabelParameter&) const" );

  list<WIR_Instruction> res;

  res.push_back(
    { { TC13::OpCode::J,
        m16BitOperations ?
          TC13::OperationFormat::SL : TC13::OperationFormat::L,
        new WIR_LabelParameter( p ) } } );

  for ( WIR_Instruction &i : res )
    for ( WIR_Operation &o : i )
      copyContainers( o, p.getOperation() );

  return( res );
};


/*
  insertLEA generates the actual TriCore instructions to load an address
  register with a constant.
*/
WIR_RegisterParameter &TC_ConstFold::insertLEA( const WIR_BaseRegister &r,
                                                const WIR_UpDownValue &v,
                                                std::list<WIR_Instruction> &l ) const
{
  DSTART(
    "WIR_RegisterParameter& TC_ConstFold::insertLEA(const WIR_BaseRegister&, const WIR_UpDownValue&, list<WIR_Instruction>&) const" );

  auto *rp = new WIR_RegisterParameter( r, WIR_Usage::def );
  auto c0 = replace( v, WIR_L4::b0 );
  auto c1 = replace( v, WIR_L4::b1 );

  if ( ( c0 >= 0 ) &&
       v.extract( 14, 14 ).containsOnlyBits( { WIR_L4::b0, WIR_L4::bX } ) ) {
    auto *imm = new TC_Const18_Unsigned( c0 );
    auto *cont = new WIR_BitValues();
    imm->insertContainer( cont );
    cont->insertInValues(
      *imm, WIR_UpDownValue( *imm ), WIR_UpDownValue( *imm ) );

    l.push_back(
      { { TC13::OpCode::LEA, TC13::OperationFormat::AC18ABSA, rp, imm } } );
  } else

  if ( ( c1 >= 0 ) &&
       v.extract( 14, 14 ).containsOnlyBits( { WIR_L4::b0, WIR_L4::bX } ) ) {
    auto *imm = new TC_Const18_Unsigned( c1 );
    auto *cont = new WIR_BitValues();
    imm->insertContainer( cont );
    cont->insertInValues(
      *imm, WIR_UpDownValue( *imm ), WIR_UpDownValue( *imm ) );

    l.push_back(
      { { TC13::OpCode::LEA, TC13::OperationFormat::AC18ABSA, rp, imm } } );
  } else {
    int low = c0 & 0xFFFF;
    int high = ( ( c0 + 0x8000 ) / (unsigned int) 0x10000 ) & 0xFFFF;

    if ( low > TC_Const16_Signed::getMaxValue( 16 ) )
      low =
        TC_Const16_Signed::getMinValue( 16 ) - 1 +
        ( low - TC_Const16_Signed::getMaxValue( 16 ) );
    else

    if ( low < TC_Const16_Signed::getMinValue( 16 ) )
      low =
        TC_Const16_Signed::getMaxValue( 16 ) +
        ( low - TC_Const16_Signed::getMinValue( 16 ) );

    // Generate MOVH.A instruction (cf. TriCore Architecture Manual, page 5-19).
    auto *imm = new TC_Const16_Unsigned( high );
    auto *cont = new WIR_BitValues();
    imm->insertContainer( cont );
    cont->insertInValues(
      *imm, WIR_UpDownValue( *imm ), WIR_UpDownValue( *imm ) );

    l.push_back(
      { { TC13::OpCode::MOVH_A, TC13::OperationFormat::AC16,
          low != 0 ? new WIR_RegisterParameter( r, WIR_Usage::def ) : rp,
          imm } } );
    auto &movh = l.back();

    // Generate LEA instruction.
    if ( low != 0 ) {
      auto *imm = new TC_Const16_Signed( low );
      auto *cont = new WIR_BitValues();
      imm->insertContainer( cont );
      cont->insertInValues(
        *imm, WIR_UpDownValue( *imm ), WIR_UpDownValue( *imm ) );

      l.push_back(
        { { TC13::OpCode::LEA, TC13::OperationFormat::AAC16BOA, rp,
            new WIR_RegisterParameter( r, WIR_Usage::use ), imm } } );
      auto &lea = l.back();

      // Add bit-true data flow from MOVH.A to LEA.
      WIR_UpDownValue v { WIR_L4::b0, 32 };
      insert( v, WIR_UpDownValue( TC_Const16_Unsigned( high ) ), 16 );

      WIR_Parameter &src = movh.begin()->get().begin()->get();
      WIR_Parameter &tgt = std::next( lea.begin()->get().begin() )->get();

      src.insertContainer( new WIR_BitValues() );
      tgt.insertContainer( new WIR_BitValues() );

      auto &srcContainer = src.getContainers<WIR_BitValues>().begin()->get();
      auto &tgtContainer = tgt.getContainers<WIR_BitValues>().begin()->get();

      srcContainer.insertOutValues(
        tgt, WIR_UpDownValue( v ), WIR_UpDownValue( v ) );
      tgtContainer.insertInValues(
        src, WIR_UpDownValue( v ), WIR_UpDownValue( v ) );
    }
  }

  return( *rp );
};


/*
  insertMOV generates the actual TriCore instructions to load a data register
  with a constant.
*/
WIR_RegisterParameter &TC_ConstFold::insertMOV( const WIR_BaseRegister &r,
                                                const WIR_UpDownValue &v,
                                                std::list<WIR_Instruction> &l ) const
{
  DSTART(
    "WIR_RegisterParameter& TC_ConstFold::insertMOV(const WIR_BaseRegister&, const WIR_UpDownValue&, list<WIR_Instruction>&) const" );

  auto *rp = new WIR_RegisterParameter( r, WIR_Usage::def );
  auto c0 = replace( v, WIR_L4::b0 );
  auto c1 = replace( v, WIR_L4::b1 );

  if ( ( c0 >= TC_Const4_Signed::getMinValue( 4 ) ) &&
       ( c0 <= TC_Const4_Signed::getMaxValue( 4 ) ) && m16BitOperations ) {
    auto *imm = new TC_Const4_Signed( c0 );
    auto *cont = new WIR_BitValues();
    imm->insertContainer( cont );
    cont->insertInValues(
      *imm, WIR_UpDownValue( *imm ), WIR_UpDownValue( *imm ) );

    l.push_back(
      { { TC13::OpCode::MOV, TC13::OperationFormat::SDC4_1, rp, imm } } );
  } else

  if ( ( c1 >= TC_Const4_Signed::getMinValue( 4 ) ) &&
       ( c1 <= TC_Const4_Signed::getMaxValue( 4 ) ) && m16BitOperations ) {
    auto *imm = new TC_Const4_Signed( c1 );
    auto *cont = new WIR_BitValues();
    imm->insertContainer( cont );
    cont->insertInValues(
      *imm, WIR_UpDownValue( *imm ), WIR_UpDownValue( *imm ) );

    l.push_back(
      { { TC13::OpCode::MOV, TC13::OperationFormat::SDC4_1, rp, imm } } );
  } else

  if ( ( c0 >= TC_Const16_Signed::getMinValue( 16 ) ) &&
       ( c0 <= TC_Const16_Signed::getMaxValue( 16 ) ) ) {
    auto *imm = new TC_Const16_Signed( c0 );
    auto *cont = new WIR_BitValues();
    imm->insertContainer( cont );
    cont->insertInValues(
      *imm, WIR_UpDownValue( *imm ), WIR_UpDownValue( *imm ) );

    l.push_back(
      { { TC13::OpCode::MOV, TC13::OperationFormat::DC16_1, rp, imm } } );
  } else

  if ( ( c1 >= TC_Const16_Signed::getMinValue( 16 ) ) &&
       ( c1 <= TC_Const16_Signed::getMaxValue( 16 ) ) ) {
    auto *imm = new TC_Const16_Signed( c1 );
    auto *cont = new WIR_BitValues();
    imm->insertContainer( cont );
    cont->insertInValues(
      *imm, WIR_UpDownValue( *imm ), WIR_UpDownValue( *imm ) );

    l.push_back(
      { { TC13::OpCode::MOV, TC13::OperationFormat::DC16_1, rp, imm } } );
  } else

  if ( ( c0 >= 0 ) && ( c0 <= (int) TC_Const16_Unsigned::getMaxValue( 16 ) ) ) {
    auto *imm = new TC_Const16_Unsigned( c0 );
    auto *cont = new WIR_BitValues();
    imm->insertContainer( cont );
    cont->insertInValues(
      *imm, WIR_UpDownValue( *imm ), WIR_UpDownValue( *imm ) );

    l.push_back(
      { { TC13::OpCode::MOV_U, TC13::OperationFormat::DC16_2, rp, imm } } );
  } else {
    int low, high;

    // Extract lowest 16 bits from constant, use 2-complement representation.
    low = c0 & 0xFFFF;

    if ( low > TC_Const16_Signed::getMaxValue( 16 ) )
      low =
        TC_Const16_Signed::getMinValue( 16 ) +
        ( low - TC_Const16_Signed::getMaxValue( 16 ) ) - 1;

    // Extract upper part of constant.
    high = c0 - low;

    // We don't use '>> 16' here, since C does not specify whether >> performs
    // shifting with or without sign extension.
    for ( int i = 0; i < 16; ++i )
      high /= 2;

    if ( high < 0 )
      high += TC_Const16_Unsigned::getMaxValue( 16 ) + 1;

    // Generate MOVH instruction.
    auto *imm = new TC_Const16_Unsigned( high );
    auto *cont = new WIR_BitValues();
    imm->insertContainer( cont );
    cont->insertInValues(
      *imm, WIR_UpDownValue( *imm ), WIR_UpDownValue( *imm ) );

    l.push_back(
      { { TC13::OpCode::MOVH, TC13::OperationFormat::DC16_2,
          low != 0 ? new WIR_RegisterParameter( r, WIR_Usage::def ) : rp,
          imm } } );
    auto &movh = l.back();

    // Generate ADDI instruction.
    if ( low != 0 ) {
      auto *imm = new TC_Const16_Signed( low );
      auto *cont = new WIR_BitValues();
      imm->insertContainer( cont );
      cont->insertInValues(
        *imm, WIR_UpDownValue( *imm ), WIR_UpDownValue( *imm ) );

      l.push_back(
        { { TC13::OpCode::ADDI, TC13::OperationFormat::DDC16_1, rp,
            new WIR_RegisterParameter( r, WIR_Usage::use ), imm } } );
      auto &addi = l.back();

      // Add bit-true data flow from MOVH to ADDI.
      WIR_UpDownValue v { WIR_L4::b0, 32 };
      insert( v, WIR_UpDownValue( TC_Const16_Unsigned( high ) ), 16 );

      WIR_Parameter &src = movh.begin()->get().begin()->get();
      WIR_Parameter &tgt = std::next( addi.begin()->get().begin() )->get();

      src.insertContainer( new WIR_BitValues() );
      tgt.insertContainer( new WIR_BitValues() );

      auto &srcContainer = src.getContainers<WIR_BitValues>().begin()->get();
      auto &tgtContainer = tgt.getContainers<WIR_BitValues>().begin()->get();

      srcContainer.insertOutValues(
        tgt, WIR_UpDownValue( v ), WIR_UpDownValue( v ) );
      tgtContainer.insertInValues(
        src, WIR_UpDownValue( v ), WIR_UpDownValue( v ) );
    }
  }

  return( *rp );
};


/*
  For a given extended register, getChilds returns the two child registers.
*/
std::pair<WIR_BaseRegister *, WIR_BaseRegister *> TC_ConstFold::getChilds( const WIR_BaseRegister &r ) const
{
  DSTART(
    "pair<WIR_BaseRegister*, WIR_BaseRegister*> TC_ConstFold::getChilds(const WIR_BaseRegister&) const" );

  WIR_BaseRegister *c1 = nullptr;
  WIR_BaseRegister *c2 = nullptr;

  if ( r.isVirtual() ) {
    auto &eReg = dynamic_cast<const WIR_VirtualRegister &>( r );
    c1 = &(eReg.getChilds().front().get());
    c2 = &(eReg.getChilds().back().get());
  } else {
    auto &eReg = dynamic_cast<const WIR_PhysicalRegister &>( r );
    c1 = &(eReg.getChilds().front().get());
    c2 = &(eReg.getChilds().back().get());
  }

  #ifdef FAILSAFEMODE
  ufAssert( c1 != nullptr );
  ufAssert( c2 != nullptr );
  #endif

  return( make_pair( c1, c2 ) );
};


/*
  areChildsUsed determines whether the two child registers of an extended
  register are actually used according to the data flow related to the given WIR
  parameter.
*/
std::pair<bool, bool> TC_ConstFold::areChildsUsed( const WIR_Parameter &p,
                                                   const WIR_BaseRegister &c1,
                                                   const WIR_BaseRegister &c2 ) const
{
  DSTART(
    "pair<bool, bool> TC_ConstFold::areChildsUsed(const WIR_Parameter&, const WIR_BaseRegister&, const WIR_BaseRegister&) const" );

  // Determine whether both childs of an extended register are actually used
  // according to the data flow. For this purpose, we iterate all outgoing edges
  // of p.
  bool c1IsUsed = false;
  bool c2IsUsed = false;

  for ( auto &outEdge :
          p.getContainers<WIR_BitValues>().begin()->get().getOutValues() ) {
    #ifdef FAILSAFEMODE
    ufAssert( outEdge.downVal.getBitWidth() == 64 );
    ufAssert( outEdge.upVal.getBitWidth() == 64 );
    auto &tgtContainer =
      outEdge.rp->getContainers<WIR_BitValues>().begin()->get();
    #endif

    auto &tgtRegister =
      dynamic_cast<const WIR_RegisterParameter &>(
        *(outEdge.rp) ).getRegister();

    if ( tgtRegister == c1 ) {
      // Partial data flow via least-significant child 1, 32 bits wide.
      c1IsUsed = true;

      #ifdef FAILSAFEMODE
      auto inEdgeIt = tgtContainer.findInValues( p );
      ufAssert( inEdgeIt != tgtContainer.getInValues().end() );
      ufAssert( inEdgeIt->downVal.getBitWidth() == 64 );
      ufAssert( inEdgeIt->upVal.getBitWidth() == 64 );
      #endif
    } else

    if ( tgtRegister == c2 ) {
      // Partial data flow via most-significant child 2, 32 bits wide.
      c2IsUsed = true;

      #ifdef FAILSAFEMODE
      auto inEdgeIt = tgtContainer.findInValues( p );
      ufAssert( inEdgeIt != tgtContainer.getInValues().end() );
      ufAssert( inEdgeIt->downVal.getBitWidth() == 64 );
      ufAssert( inEdgeIt->upVal.getBitWidth() == 64 );
      #endif
    } else {
      // Full data flow via the complete eReg, 64 bits wide.
      c1IsUsed = true;
      c2IsUsed = true;

      #ifdef FAILSAFEMODE
      auto inEdgeIt = tgtContainer.findInValues( p );
      ufAssert( inEdgeIt != tgtContainer.getInValues().end() );
      ufAssert( inEdgeIt->downVal.getBitWidth() == 64 );
      ufAssert( inEdgeIt->upVal.getBitWidth() == 64 );
      #endif
    }
  }

  return( make_pair( c1IsUsed, c2IsUsed ) );
};

}       // namespace WIR
