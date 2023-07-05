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
  @file tcconstprop.cc
  @brief This file implements a TriCore-specific constant propagation
         optimization.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include standard headers
#include <sstream>

// Include boost headers
#include <boost/current_function.hpp>

// Include libuseful headers
#include <libuseful/debugmacros.h>

// Include WIR headers
#include <wir/wir.h>
#include <arch/tricore/tc131.h>
#include <arch/tricore/analyses/bit/tcbitdfa.h>

// Include local headers
#include "tcconstprop.h"


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
TC_ConstProp::TC_ConstProp( WIR_System &s ) :
  WIR_Optimization { s },
  WIR_ConstProp { s }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Default constructor for compilation unit-level optimization.
*/
TC_ConstProp::TC_ConstProp( WIR_CompilationUnit &c ) :
  WIR_Optimization { c },
  WIR_ConstProp { c }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Default constructor for function-level optimization.
*/
TC_ConstProp::TC_ConstProp( WIR_Function &f ) :
  WIR_Optimization { f },
  WIR_ConstProp { f }
{
  DSTART( "TC_ConstProp::TC_ConstProp(WIR_Function&)" );
};


/*
  Destructor.
*/
TC_ConstProp::~TC_ConstProp( void )
{
  DSTART( "virtual TC_ConstProp::~TC_ConstProp()" );
};


//
// Protected class methods
//

/*
  runOptimization propagates constants in the given function.
*/
void TC_ConstProp::runOptimization( WIR_Function &f )
{
  DSTART( "virtual void TC_ConstProp::runOptimization(WIR_Function&)" );

  TC_BitDFA analyzer { f };
  setDFA( analyzer );

  WIR_ConstProp::runOptimization( f );
};


/*
  doConstProp does the actual TriCore-specific propagation of constants for a
  given operation.

  doConstProp does not actually modify the currently examined WIR operation o.
  Instead, new instructions realizing the constant folding of o are added to map
  mNewInstructions.
*/
bool TC_ConstProp::doConstProp( const WIR_Operation &o,
                                const std::map<WIR_id_t, WIR_UpDownValue> &inValue )
{
  DSTART(
    "virtual bool TC_ConstProp::doConstProp(const WIR_Operation&, const map<long long unsigned int, WIR_UpDownValue>&)" );

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

  // A small lambda to retrieve the register of o's first explicit parameter.
  auto firstReg = [&]( void ) -> WIR_BaseRegister & {
    auto &rp =
      dynamic_cast<WIR_RegisterParameter &>(
        o.getExplicitParameters().front().get() );
    return( rp.getRegister() );
  };

  //
  // Propagate operation formats DDD into DDC9.
  //

  // Do copy propagation for "classical" signed 3-address operations.
  if ( ( ( ( ( o.getOpCode() == TC13::OpCode::ABSDIF ) ||
             ( o.getOpCode() == TC13::OpCode::ABSDIFS ) ||
             ( o.getOpCode() == TC13::OpCode::ADD ) ||
             ( o.getOpCode() == TC13::OpCode::ADDS ) ||
             ( o.getOpCode() == TC13::OpCode::ADDS_U ) ||
             ( o.getOpCode() == TC13::OpCode::EQ ) ||
             ( o.getOpCode() == TC13::OpCode::EQANY_B ) ||
             ( o.getOpCode() == TC13::OpCode::EQANY_H ) ||
             ( o.getOpCode() == TC13::OpCode::GE ) ||
             ( o.getOpCode() == TC13::OpCode::LT ) ||
             ( o.getOpCode() == TC13::OpCode::MAX ) ||
             ( o.getOpCode() == TC13::OpCode::MIN ) ||
             ( o.getOpCode() == TC13::OpCode::MUL ) ||
             ( o.getOpCode() == TC13::OpCode::MULS ) ||
             ( o.getOpCode() == TC13::OpCode::NE ) ||
             ( o.getOpCode() == TC13::OpCode::SH ) ||
             ( o.getOpCode() == TC13::OpCode::SH_H ) ||
             ( o.getOpCode() == TC13::OpCode::SHA_H ) ||
             ( o.getOpCode() == TC13::OpCode::SHAS ) ) &&
           ( o.getOperationFormat() == TC13::OperationFormat::DDD_1 ) ) ||
         ( ( ( o.getOpCode() == TC13::OpCode::AND_EQ ) ||
             ( o.getOpCode() == TC13::OpCode::AND_GE ) ||
             ( o.getOpCode() == TC13::OpCode::AND_LT ) ||
             ( o.getOpCode() == TC13::OpCode::AND_NE ) ||
             ( o.getOpCode() == TC13::OpCode::OR_EQ ) ||
             ( o.getOpCode() == TC13::OpCode::OR_GE ) ||
             ( o.getOpCode() == TC13::OpCode::OR_LT ) ||
             ( o.getOpCode() == TC13::OpCode::OR_NE ) ||
             ( o.getOpCode() == TC13::OpCode::SH_EQ ) ||
             ( o.getOpCode() == TC13::OpCode::SH_GE ) ||
             ( o.getOpCode() == TC13::OpCode::SH_LT ) ||
             ( o.getOpCode() == TC13::OpCode::SH_NE ) ||
             ( o.getOpCode() == TC13::OpCode::XOR_EQ ) ||
             ( o.getOpCode() == TC13::OpCode::XOR_GE ) ||
             ( o.getOpCode() == TC13::OpCode::XOR_LT ) ||
             ( o.getOpCode() == TC13::OpCode::XOR_NE ) ) &&
           ( o.getOperationFormat() == TC13::OperationFormat::DDD_2 ) ) ) &&
       lastOp().isInteger() ) {
    auto instrs = prop_DDD_DDC9( o, lastOp() );
    if ( !instrs.empty() ) {
      mNewInstructions.emplace(
        const_cast<WIR_Operation &>( o ), std::move( instrs ) );
      return( true );
    }
  }

  // Exploit commutativity of some of the above operations.
  if ( ( ( ( ( o.getOpCode() == TC13::OpCode::ADD ) ||
             ( o.getOpCode() == TC13::OpCode::ADDS ) ||
             ( o.getOpCode() == TC13::OpCode::ADDS_U ) ||
             ( o.getOpCode() == TC13::OpCode::EQ ) ||
             ( o.getOpCode() == TC13::OpCode::EQANY_B ) ||
             ( o.getOpCode() == TC13::OpCode::EQANY_H ) ||
             ( o.getOpCode() == TC13::OpCode::MAX ) ||
             ( o.getOpCode() == TC13::OpCode::MIN ) ||
             ( o.getOpCode() == TC13::OpCode::MUL ) ||
             ( o.getOpCode() == TC13::OpCode::MULS ) ||
             ( o.getOpCode() == TC13::OpCode::NE ) ) &&
           ( o.getOperationFormat() == TC13::OperationFormat::DDD_1 ) ) ||
         ( ( ( o.getOpCode() == TC13::OpCode::AND_EQ ) ||
             ( o.getOpCode() == TC13::OpCode::AND_NE ) ||
             ( o.getOpCode() == TC13::OpCode::OR_EQ ) ||
             ( o.getOpCode() == TC13::OpCode::OR_NE ) ||
             ( o.getOpCode() == TC13::OpCode::SH_EQ ) ||
             ( o.getOpCode() == TC13::OpCode::SH_NE ) ||
             ( o.getOpCode() == TC13::OpCode::XOR_EQ ) ||
             ( o.getOpCode() == TC13::OpCode::XOR_NE ) ) &&
           ( o.getOperationFormat() == TC13::OperationFormat::DDD_2 ) ) ) &&
       nthOp( 1 ).isInteger() ) {
    auto instrs = prop_DDD_DDC9_2( o, nthOp( 1 ) );
    if ( !instrs.empty() ) {
      mNewInstructions.emplace(
        const_cast<WIR_Operation &>( o ), std::move( instrs ) );
      return( true );
    }
  }

  // Do copy propagation for "classical" signed 3-address operations with PSW.C.
  if ( ( ( ( o.getOpCode() == TC13::OpCode::ADDC ) ||
           ( o.getOpCode() == TC13::OpCode::ADDX ) ||
           ( o.getOpCode() == TC13::OpCode::SHA ) ) &&
         ( ( o.getOperationFormat() == TC13::OperationFormat::DDDPSW_2 ) ||
           ( o.getOperationFormat() == TC13::OperationFormat::DDDPSW_1 ) ) ) &&
       nthOp( 2 ).isInteger() ) {
    auto instrs = prop_DDD_DDC9_6( o, nthOp( 2 ) );
    if ( !instrs.empty() ) {
      mNewInstructions.emplace(
        const_cast<WIR_Operation &>( o ), std::move( instrs ) );
      return( true );
    }
  }

  // Exploit commutativity of some of the above operations.
  if ( ( ( ( o.getOpCode() == TC13::OpCode::ADDC ) ||
           ( o.getOpCode() == TC13::OpCode::ADDX ) ) &&
         ( ( o.getOperationFormat() == TC13::OperationFormat::DDDPSW_2 ) ||
           ( o.getOperationFormat() == TC13::OperationFormat::DDDPSW_1 ) ) ) &&
       nthOp( 1 ).isInteger() ) {
    auto instrs = prop_DDD_DDC9_7( o, nthOp( 1 ) );
    if ( !instrs.empty() ) {
      mNewInstructions.emplace(
        const_cast<WIR_Operation &>( o ), std::move( instrs ) );
      return( true );
    }
  }

  // ADD Dc, Da, const Db (DDD_1) -> ADDI Dc, Da, const16 (DDC16_1)
  if ( ( o.getOpCode() == TC13::OpCode::ADD ) &&
       ( o.getOperationFormat() == TC13::OperationFormat::DDD_1 ) &&
       lastOp().isInteger() ) {
    auto instrs = prop_DDD_DDC16( o, lastOp() );
    if ( !instrs.empty() ) {
      mNewInstructions.emplace(
        const_cast<WIR_Operation &>( o ), std::move( instrs ) );
      return( true );
    }
  }

  // Exploit commutativity of the above transformation.
  if ( ( o.getOpCode() == TC13::OpCode::ADD ) &&
       ( o.getOperationFormat() == TC13::OperationFormat::DDD_1 ) &&
       nthOp( 1 ).isInteger() ) {
    auto instrs = prop_DDD_DDC16_1( o, nthOp( 1 ) );
    if ( !instrs.empty() ) {
      mNewInstructions.emplace(
        const_cast<WIR_Operation &>( o ), std::move( instrs ) );
      return( true );
    }
  }

  // ADD Dc, Da, const Db (DDD_1) -> ADDIH Dc, Da, const16 (DDC16_2)
  // ADD.A Ac, Aa, const Ab (AAA_1) -> ADDIH.A Ac, Aa, const16 (AAC16)
  if ( ( ( ( o.getOpCode() == TC13::OpCode::ADD ) &&
           ( o.getOperationFormat() == TC13::OperationFormat::DDD_1 ) ) ||
         ( ( o.getOpCode() == TC13::OpCode::ADD_A ) &&
           ( o.getOperationFormat() == TC13::OperationFormat::AAA ) ) ) &&
       lastOp().isInteger() ) {
    auto instrs = prop_xxx_xxC16( o, lastOp() );
    if ( !instrs.empty() ) {
      mNewInstructions.emplace(
        const_cast<WIR_Operation &>( o ), std::move( instrs ) );
      return( true );
    }
  }

  // Exploit commutativity of the above transformation.
  if ( ( ( ( o.getOpCode() == TC13::OpCode::ADD ) &&
           ( o.getOperationFormat() == TC13::OperationFormat::DDD_1 ) ) ||
         ( ( o.getOpCode() == TC13::OpCode::ADD_A ) &&
           ( o.getOperationFormat() == TC13::OperationFormat::AAA ) ) ) &&
       nthOp( 1 ).isInteger() ) {
    auto instrs = prop_xxx_xxC16_1( o, nthOp( 1 ) );
    if ( !instrs.empty() ) {
      mNewInstructions.emplace(
        const_cast<WIR_Operation &>( o ), std::move( instrs ) );
      return( true );
    }
  }

  // Exploit commutativity of *GE or *LT operations.
  if ( ( ( ( ( o.getOpCode() == TC13::OpCode::GE ) ||
             ( o.getOpCode() == TC13::OpCode::LT ) ) &&
           ( o.getOperationFormat() == TC13::OperationFormat::DDD_1 ) ) ||
         ( ( ( o.getOpCode() == TC13::OpCode::AND_GE ) ||
             ( o.getOpCode() == TC13::OpCode::AND_LT ) ||
             ( o.getOpCode() == TC13::OpCode::OR_GE ) ||
             ( o.getOpCode() == TC13::OpCode::OR_LT ) ||
             ( o.getOpCode() == TC13::OpCode::SH_GE ) ||
             ( o.getOpCode() == TC13::OpCode::SH_LT ) ||
             ( o.getOpCode() == TC13::OpCode::XOR_GE ) ||
             ( o.getOpCode() == TC13::OpCode::XOR_LT ) ) &&
           ( o.getOperationFormat() == TC13::OperationFormat::DDD_2 ) ) ) &&
       nthOp( 1 ).isInteger() ) {
    auto instrs = prop_DDD_DDC9_3( o, nthOp( 1 ) );
    if ( !instrs.empty() ) {
      mNewInstructions.emplace(
        const_cast<WIR_Operation &>( o ), std::move( instrs ) );
      return( true );
    }
  }

  // Do copy propagation for signed 3-address SUB operations.
  if ( ( ( o.getOpCode() == TC13::OpCode::SUB ) ||
         ( o.getOpCode() == TC13::OpCode::SUBS ) ) &&
       ( o.getOperationFormat() == TC13::OperationFormat::DDD_1 ) &&
       lastOp().isInteger() ) {
    auto instrs = prop_DDD_DDC9_4( o, lastOp() );
    if ( !instrs.empty() ) {
      mNewInstructions.emplace(
        const_cast<WIR_Operation &>( o ), std::move( instrs ) );
      return( true );
    }
  }

  // Exploit commutativity of the above operations.
  if ( ( ( o.getOpCode() == TC13::OpCode::SUB ) ||
         ( o.getOpCode() == TC13::OpCode::SUBS ) ) &&
       ( o.getOperationFormat() == TC13::OperationFormat::DDD_1 ) &&
       nthOp( 1 ).isInteger() ) {
    auto instrs = prop_DDD_DDC9_5( o, nthOp( 1 ) );
    if ( !instrs.empty() ) {
      mNewInstructions.emplace(
        const_cast<WIR_Operation &>( o ), std::move( instrs ) );
      return( true );
    }
  }

  // Do copy propagation for "classical" unsigned 3-address operations.
  if ( ( ( ( ( o.getOpCode() == TC13::OpCode::AND ) ||
             ( o.getOpCode() == TC13::OpCode::ANDN ) ||
             ( o.getOpCode() == TC13::OpCode::GE_U ) ||
             ( o.getOpCode() == TC13::OpCode::LT_U ) ||
             ( o.getOpCode() == TC13::OpCode::MAX_U ) ||
             ( o.getOpCode() == TC13::OpCode::MIN_U ) ||
             ( o.getOpCode() == TC13::OpCode::MULS_U ) ||
             ( o.getOpCode() == TC13::OpCode::NAND ) ||
             ( o.getOpCode() == TC13::OpCode::NOR ) ||
             ( o.getOpCode() == TC13::OpCode::OR ) ||
             ( o.getOpCode() == TC13::OpCode::ORN ) ||
             ( o.getOpCode() == TC13::OpCode::XNOR ) ||
             ( o.getOpCode() == TC13::OpCode::XOR ) ) &&
           ( o.getOperationFormat() == TC13::OperationFormat::DDD_1 ) ) ||
         ( ( o.getOpCode() == TC13::OpCode::MUL_U ) &&
           ( o.getOperationFormat() == TC13::OperationFormat::EDD ) ) ) &&
       lastOp().isInteger() ) {
    auto instrs = prop_xDD_DDC9_u( o, lastOp() );
    if ( !instrs.empty() ) {
      mNewInstructions.emplace(
        const_cast<WIR_Operation &>( o ), std::move( instrs ) );
      return( true );
    }
  }

  // Exploit commutativity of some of the above operations.
  if ( ( ( ( ( o.getOpCode() == TC13::OpCode::AND ) ||
             ( o.getOpCode() == TC13::OpCode::ANDN ) ||
             ( o.getOpCode() == TC13::OpCode::MAX_U ) ||
             ( o.getOpCode() == TC13::OpCode::MIN_U ) ||
             ( o.getOpCode() == TC13::OpCode::MULS_U ) ||
             ( o.getOpCode() == TC13::OpCode::NAND ) ||
             ( o.getOpCode() == TC13::OpCode::NOR ) ||
             ( o.getOpCode() == TC13::OpCode::OR ) ||
             ( o.getOpCode() == TC13::OpCode::ORN ) ||
             ( o.getOpCode() == TC13::OpCode::XNOR ) ||
             ( o.getOpCode() == TC13::OpCode::XOR ) ) &&
           ( o.getOperationFormat() == TC13::OperationFormat::DDD_1 ) ) ||
         ( ( o.getOpCode() == TC13::OpCode::MUL_U ) &&
           ( o.getOperationFormat() == TC13::OperationFormat::EDD ) ) ) &&
       nthOp( 1 ).isInteger() ) {
    auto instrs = prop_xDD_DDC9_2u( o, nthOp( 1 ) );
    if ( !instrs.empty() ) {
      mNewInstructions.emplace(
        const_cast<WIR_Operation &>( o ), std::move( instrs ) );
      return( true );
    }
  }

  // Exploit commutativity of *GE.U or *LT.U operations.
  if ( ( ( ( ( o.getOpCode() == TC13::OpCode::GE_U ) ||
             ( o.getOpCode() == TC13::OpCode::LT_U ) ) &&
           ( o.getOperationFormat() == TC13::OperationFormat::DDD_1 ) ) ||
         ( ( ( o.getOpCode() == TC13::OpCode::AND_GE_U ) ||
             ( o.getOpCode() == TC13::OpCode::AND_LT_U ) ||
             ( o.getOpCode() == TC13::OpCode::OR_GE_U ) ||
             ( o.getOpCode() == TC13::OpCode::OR_LT_U ) ||
             ( o.getOpCode() == TC13::OpCode::SH_GE_U ) ||
             ( o.getOpCode() == TC13::OpCode::SH_LT_U ) ||
             ( o.getOpCode() == TC13::OpCode::XOR_GE_U ) ||
             ( o.getOpCode() == TC13::OpCode::XOR_LT_U ) ) &&
           ( o.getOperationFormat() == TC13::OperationFormat::DDD_2 ) ) ) &&
       nthOp( 1 ).isInteger() ) {
    auto instrs = prop_DDD_DDC9_3u( o, nthOp( 1 ) );
    if ( !instrs.empty() ) {
      mNewInstructions.emplace(
        const_cast<WIR_Operation &>( o ), std::move( instrs ) );
      return( true );
    }
  }

  //
  // Propagate operation formats xxDD into xxDC9.
  //

  // Do copy propagation for "classical" signed 4-address operations.
  if ( ( ( o.getOpCode() == TC13::OpCode::CADD ) ||
         ( o.getOpCode() == TC13::OpCode::CADDN ) ||
         ( o.getOpCode() == TC13::OpCode::MADD ) ||
         ( o.getOpCode() == TC13::OpCode::MADDS ) ||
         ( o.getOpCode() == TC13::OpCode::MSUB ) ||
         ( o.getOpCode() == TC13::OpCode::MSUBS ) ||
         ( o.getOpCode() == TC13::OpCode::SEL ) ||
         ( o.getOpCode() == TC13::OpCode::SELN ) ) &&
       ( ( o.getOperationFormat() == TC13::OperationFormat::DDDD ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::EEDD ) ) &&
       lastOp().isInteger() ) {
    auto instrs = prop_xxDD_xxDC9( o, lastOp() );
    if ( !instrs.empty() ) {
      mNewInstructions.emplace(
        const_cast<WIR_Operation &>( o ), std::move( instrs ) );
      return( true );
    }
  }

  // Exploit commutativity of some of the above operations.
  if ( ( ( o.getOpCode() == TC13::OpCode::MADD ) ||
         ( o.getOpCode() == TC13::OpCode::MADDS ) ||
         ( o.getOpCode() == TC13::OpCode::MSUB ) ||
         ( o.getOpCode() == TC13::OpCode::MSUBS ) ||
         ( o.getOpCode() == TC13::OpCode::SEL ) ||
         ( o.getOpCode() == TC13::OpCode::SELN ) ) &&
       ( ( o.getOperationFormat() == TC13::OperationFormat::DDDD ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::EEDD ) ) &&
       nthOp( 2 ).isInteger() ) {
    auto instrs = prop_xxDD_xxDC9_2( o, nthOp( 2 ) );
    if ( !instrs.empty() ) {
      mNewInstructions.emplace(
        const_cast<WIR_Operation &>( o ), std::move( instrs ) );
      return( true );
    }
  }

  // Do copy propagation for CSUB*.
  if ( ( ( o.getOpCode() == TC13::OpCode::CSUB ) ||
         ( o.getOpCode() == TC13::OpCode::CSUBN ) ) &&
       lastOp().isInteger() ) {
    auto instrs = prop_CSUB( o, lastOp() );
    if ( !instrs.empty() ) {
      mNewInstructions.emplace(
        const_cast<WIR_Operation &>( o ), std::move( instrs ) );
exit( 4 );
      return( true );
    }
  }

  // Do copy propagation for "classical" unsigned 4-address operations.
  if ( ( ( o.getOpCode() == TC13::OpCode::MADD_U ) ||
         ( o.getOpCode() == TC13::OpCode::MADDS_U ) ||
         ( o.getOpCode() == TC13::OpCode::MSUB_U ) ||
         ( o.getOpCode() == TC13::OpCode::MSUBS_U ) ) &&
       ( ( o.getOperationFormat() == TC13::OperationFormat::DDDD ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::EEDD ) ) &&
       lastOp().isInteger() ) {
    auto instrs = prop_xxDD_xxDC9_u( o, lastOp() );
    if ( !instrs.empty() ) {
      mNewInstructions.emplace(
        const_cast<WIR_Operation &>( o ), std::move( instrs ) );
      return( true );
    }
  }

  // Exploit commutativity of the above operations.
  if ( ( ( o.getOpCode() == TC13::OpCode::MADD_U ) ||
         ( o.getOpCode() == TC13::OpCode::MADDS_U ) ||
         ( o.getOpCode() == TC13::OpCode::MSUB_U ) ||
         ( o.getOpCode() == TC13::OpCode::MSUBS_U ) ) &&
       ( ( o.getOperationFormat() == TC13::OperationFormat::DDDD ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::EEDD ) ) &&
       nthOp( 2 ).isInteger() ) {
    auto instrs = prop_xxDD_xxDC9_2u( o, nthOp( 2 ) );
    if ( !instrs.empty() ) {
      mNewInstructions.emplace(
        const_cast<WIR_Operation &>( o ), std::move( instrs ) );
exit( 5 );
      return( true );
    }
  }

  //
  // Propagate operation formats SAA/SDD into SAC4/SDC4.
  //

  if ( ( ( ( o.getOpCode() == TC13::OpCode::ADD ) &&
           ( o.getOperationFormat() == TC13::OperationFormat::SDD_2 ) ) ||
         ( ( o.getOpCode() == TC13::OpCode::ADD_A ) &&
           ( o.getOperationFormat() == TC13::OperationFormat::SAA_5 ) ) ) &&
       lastOp().isInteger() ) {
    auto instrs = prop_Sxx_SxC4( o, lastOp() );
    if ( !instrs.empty() ) {
      mNewInstructions.emplace(
        const_cast<WIR_Operation &>( o ), std::move( instrs ) );
      return( true );
    }
  }

  //
  // Propagate operation format SDD into SIC8_2.
  //

  // Exploit special format SIC8_2 for AND or OR.
  if ( ( ( o.getOpCode() == TC13::OpCode::AND ) ||
         ( o.getOpCode() == TC13::OpCode::OR ) ) &&
       ( o.getOperationFormat() == TC13::OperationFormat::SDD_2 ) &&
       TC13::isD15( firstReg() ) && lastOp().isInteger() ) {
    auto instrs = prop_SDD_SIC8_2( o, lastOp() );
    if ( !instrs.empty() ) {
      mNewInstructions.emplace(
        const_cast<WIR_Operation &>( o ), std::move( instrs ) );
exit( 6 );
      return( true );
    }
  }

  //
  // Propagate operation formats SDID/SIDD into SDIC4/SIDC4.
  //

  // Exploit special formats SDIC4 or SIDC4.
  if ( ( ( ( ( o.getOpCode() == TC13::OpCode::CMOV ) ||
             ( o.getOpCode() == TC13::OpCode::CMOVN ) ) &&
           ( o.getOperationFormat() == TC13::OperationFormat::SDID_2 ) ) ||
         ( ( ( o.getOpCode() == TC13::OpCode::ADD ) ||
             ( o.getOpCode() == TC13::OpCode::EQ ) ||
             ( o.getOpCode() == TC13::OpCode::LT ) ) &&
           ( o.getOperationFormat() == TC13::OperationFormat::SIDD ) ) ||
         ( ( o.getOpCode() == TC13::OpCode::ADD ) &&
           ( o.getOperationFormat() == TC13::OperationFormat::SDID_1 ) ) ) &&
       lastOp().isInteger() ) {
    auto instrs = prop_SIxD_SIxC4( o, lastOp() );
    if ( !instrs.empty() ) {
      mNewInstructions.emplace(
        const_cast<WIR_Operation &>( o ), std::move( instrs ) );
exit( 1 );
      return( true );
    }
  }

  // Exploit commutativity of some of the above operations.
  if ( ( ( o.getOpCode() == TC13::OpCode::ADD ) ||
         ( o.getOpCode() == TC13::OpCode::EQ ) ) &&
       ( o.getOperationFormat() == TC13::OperationFormat::SIDD ) &&
       nthOp( 1 ).isInteger() ) {
    auto instrs = prop_SIxD_SIxC4_2( o, nthOp( 1 ) );
    if ( !instrs.empty() ) {
      mNewInstructions.emplace(
        const_cast<WIR_Operation &>( o ), std::move( instrs ) );
exit( 2 );
      return( true );
    }
  }

  // Do copy propagation for short SUB operations.
  if ( ( o.getOpCode() == TC13::OpCode::SUB ) &&
       ( ( o.getOperationFormat() == TC13::OperationFormat::SIDD ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::SDID_1 ) ) &&
       lastOp().isInteger() ) {
    auto instrs = prop_SIxD_SIDC4( o, lastOp() );
    if ( !instrs.empty() ) {
      mNewInstructions.emplace(
        const_cast<WIR_Operation &>( o ), std::move( instrs ) );
exit( 3 );
      return( true );
    }
  }

  //
  // Propagate operation formats SDD into DDC9.
  //

  // Do copy propagation for "classical" signed 2-address operations.
  if ( ( ( o.getOpCode() == TC13::OpCode::ADD ) ||
         ( o.getOpCode() == TC13::OpCode::ADDS ) ||
         ( o.getOpCode() == TC13::OpCode::MUL ) ) &&
       ( o.getOperationFormat() == TC13::OperationFormat::SDD_2 ) &&
       lastOp().isInteger() ) {
    auto instrs = prop_SDD_DDC9( o, lastOp() );
    if ( !instrs.empty() ) {
      mNewInstructions.emplace(
        const_cast<WIR_Operation &>( o ), std::move( instrs ) );
      return( true );
    }
  }

  // Exploit commutativity of the above operations.
  if ( ( ( o.getOpCode() == TC13::OpCode::ADD ) ||
         ( o.getOpCode() == TC13::OpCode::ADDS ) ||
         ( o.getOpCode() == TC13::OpCode::MUL ) ) &&
       ( o.getOperationFormat() == TC13::OperationFormat::SDD_2 ) &&
       firstOp().isInteger() ) {
    auto instrs = prop_SDD_DDC9_2( o, firstOp() );
    if ( !instrs.empty() ) {
      mNewInstructions.emplace(
        const_cast<WIR_Operation &>( o ), std::move( instrs ) );
      return( true );
    }
  }

  // Do copy propagation for signed 2-address SUB operations.
  if ( ( ( o.getOpCode() == TC13::OpCode::SUB ) ||
         ( o.getOpCode() == TC13::OpCode::SUBS ) ) &&
       ( o.getOperationFormat() == TC13::OperationFormat::SDD_2 ) &&
       lastOp().isInteger() ) {
    auto instrs = prop_SDD_DDC9_3( o, lastOp() );
    if ( !instrs.empty() ) {
      mNewInstructions.emplace(
        const_cast<WIR_Operation &>( o ), std::move( instrs ) );
      return( true );
    }
  }

  // Exploit commutativity of the above operations.
  if ( ( ( o.getOpCode() == TC13::OpCode::SUB ) ||
         ( o.getOpCode() == TC13::OpCode::SUBS ) ) &&
       ( o.getOperationFormat() == TC13::OperationFormat::SDD_2 ) &&
       firstOp().isInteger() ) {
    auto instrs = prop_SDD_DDC9_4( o, firstOp() );
    if ( !instrs.empty() ) {
      mNewInstructions.emplace(
        const_cast<WIR_Operation &>( o ), std::move( instrs ) );
      return( true );
    }
  }

  // Do copy propagation for "classical" unsigned 2-address operations.
  if ( ( ( o.getOpCode() == TC13::OpCode::AND ) ||
         ( o.getOpCode() == TC13::OpCode::OR ) ||
         ( o.getOpCode() == TC13::OpCode::XOR ) ) &&
       ( o.getOperationFormat() == TC13::OperationFormat::SDD_2 ) &&
       lastOp().isInteger() ) {
    auto instrs = prop_SDD_DDC9_u( o, lastOp() );
    if ( !instrs.empty() ) {
      mNewInstructions.emplace(
        const_cast<WIR_Operation &>( o ), std::move( instrs ) );
      return( true );
    }
  }

  // Exploit commutativity of the above unsigned operations.
  if ( ( ( o.getOpCode() == TC13::OpCode::AND ) ||
         ( o.getOpCode() == TC13::OpCode::OR ) ||
         ( o.getOpCode() == TC13::OpCode::XOR ) ) &&
       ( o.getOperationFormat() == TC13::OperationFormat::SDD_2 ) &&
       firstOp().isInteger() ) {
    auto instrs = prop_SDD_DDC9_2u( o, firstOp() );
    if ( !instrs.empty() ) {
      mNewInstructions.emplace(
        const_cast<WIR_Operation &>( o ), std::move( instrs ) );
      return( true );
    }
  }

  //
  // Propagate operation formats DDL into DC4L.
  //

  // Do copy propagation for "classical" conditional branches.
  if ( ( ( o.getOpCode() == TC13::OpCode::JEQ ) ||
         ( o.getOpCode() == TC13::OpCode::JGE ) ||
         ( o.getOpCode() == TC13::OpCode::JLT ) ||
         ( o.getOpCode() == TC13::OpCode::JNE ) ||
         ( o.getOpCode() == TC13::OpCode::JNED ) ||
         ( o.getOpCode() == TC13::OpCode::JNEI ) ) &&
       ( ( o.getOperationFormat() == TC13::OperationFormat::DDL_1 ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::DDL_2 ) ) &&
       nthOp( 1 ).isInteger() ) {
    auto instrs = prop_DDL_DC4L( o, nthOp( 1 ) );
    if ( !instrs.empty() ) {
      mNewInstructions.emplace(
        const_cast<WIR_Operation &>( o ), std::move( instrs ) );
      return( true );
    }
  }

  // Exploit commutativity of some of the above operations.
  if ( ( ( o.getOpCode() == TC13::OpCode::JEQ ) ||
         ( o.getOpCode() == TC13::OpCode::JNE ) ) &&
       ( o.getOperationFormat() == TC13::OperationFormat::DDL_1 ) &&
       firstOp().isInteger() ) {
    auto instrs = prop_DDL_DC4L_2( o, firstOp() );
    if ( !instrs.empty() ) {
      mNewInstructions.emplace(
        const_cast<WIR_Operation &>( o ), std::move( instrs ) );
      return( true );
    }
  }

  // Exploit commutativity of JGE or JLT operations.
  if ( ( ( o.getOpCode() == TC13::OpCode::JGE ) ||
         ( o.getOpCode() == TC13::OpCode::JLT ) ) &&
       ( o.getOperationFormat() == TC13::OperationFormat::DDL_1 ) &&
       firstOp().isInteger() ) {
    auto instrs = prop_DDL_DC4L_3( o, firstOp() );
    if ( !instrs.empty() ) {
      mNewInstructions.emplace(
        const_cast<WIR_Operation &>( o ), std::move( instrs ) );
      return( true );
    }
  }

  // Do copy propagation for "classical" unsigned conditional branches.
  if ( ( ( o.getOpCode() == TC13::OpCode::JGE_U ) ||
         ( o.getOpCode() == TC13::OpCode::JLT_U ) ) &&
       ( o.getOperationFormat() == TC13::OperationFormat::DDL_1 ) &&
       nthOp( 1 ).isInteger() ) {
    auto instrs = prop_DDL_DC4L_u( o, nthOp( 1 ) );
    if ( !instrs.empty() ) {
      mNewInstructions.emplace(
        const_cast<WIR_Operation &>( o ), std::move( instrs ) );
      return( true );
    }
  }

  // Exploit commutativity of JGE.U or JLT.U operations.
  if ( ( ( o.getOpCode() == TC13::OpCode::JGE_U ) ||
         ( o.getOpCode() == TC13::OpCode::JLT_U ) ) &&
       ( o.getOperationFormat() == TC13::OperationFormat::DDL_1 ) &&
       firstOp().isInteger() ) {
    auto instrs = prop_DDL_DC4L_2u( o, firstOp() );
    if ( !instrs.empty() ) {
      mNewInstructions.emplace(
        const_cast<WIR_Operation &>( o ), std::move( instrs ) );
      return( true );
    }
  }

  //
  // Propagate operation format SIDL into SIC4L.
  //

  if ( ( ( o.getOpCode() == TC13::OpCode::JEQ ) ||
         ( o.getOpCode() == TC13::OpCode::JNE ) ) &&
       ( o.getOperationFormat() == TC13::OperationFormat::SIDL ) &&
       nthOp( 1 ).isInteger() ) {
    auto instrs = prop_SIDL_SIC4L( o, nthOp( 1 ) );
    if ( !instrs.empty() ) {
      mNewInstructions.emplace(
        const_cast<WIR_Operation &>( o ), std::move( instrs ) );
exit( 10 );
      return( true );
    }
  }

  //
  // Propagate constants for MOV* operations.
  //

  // MOV.A Ac, const Db (AD/SAD_1) -> MOV.A Ac, const4 (SAC4_1)
  if ( ( o.getOpCode() == TC13::OpCode::MOV_A ) &&
       ( ( o.getOperationFormat() == TC13::OperationFormat::AD ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::SAD_1 ) ) &&
       lastOp().isInteger() ) {
    auto instrs = prop_AD_SAC4( o, lastOp() );
    if ( !instrs.empty() ) {
      mNewInstructions.emplace(
        const_cast<WIR_Operation &>( o ), std::move( instrs ) );
      return( true );
    }
  }

  // MOV_RR Da, const Db (SDD_1/DD) -> MOV Da, const4 (SDC4_1)
  if ( ( o.getOpCode() == TC13::OpCode::MOV_RR ) &&
       ( ( o.getOperationFormat() == TC13::OperationFormat::DD ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::SDD_1 ) ) &&
       lastOp().isInteger() ) {
    auto instrs = prop_DD_SDC4( o, lastOp() );
    if ( !instrs.empty() ) {
      mNewInstructions.emplace(
        const_cast<WIR_Operation &>( o ), std::move( instrs ) );
      return( true );
    }
  }

  // MOV_RR Da, const Db (SDD_1/DD) -> MOV D15, const8 (SIC8_1)
  if ( ( o.getOpCode() == TC13::OpCode::MOV_RR ) &&
       ( ( o.getOperationFormat() == TC13::OperationFormat::DD ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::SDD_1 ) ) &&
       TC13::isD15( firstReg() ) && lastOp().isInteger() ) {
    auto instrs = prop_DD_SIC8( o, lastOp() );
    if ( !instrs.empty() ) {
      mNewInstructions.emplace(
        const_cast<WIR_Operation &>( o ), std::move( instrs ) );
      return( true );
    }
  }

  // MOV_RR Da, const Db (SDD_1/DD) -> MOV Dc, const16 (DC16_1)
  if ( ( o.getOpCode() == TC13::OpCode::MOV_RR ) &&
       ( ( o.getOperationFormat() == TC13::OperationFormat::DD ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::SDD_1 ) ) &&
       lastOp().isInteger() ) {
    auto instrs = prop_DD_DC16( o, lastOp() );
    if ( !instrs.empty() ) {
      mNewInstructions.emplace(
        const_cast<WIR_Operation &>( o ), std::move( instrs ) );
      return( true );
    }
  }

  // MOV_RR Da, const Db (SDD_1/DD) -> MOV.U Dc, const16 (DC16_2)
  if ( ( o.getOpCode() == TC13::OpCode::MOV_RR ) &&
       ( ( o.getOperationFormat() == TC13::OperationFormat::DD ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::SDD_1 ) ) &&
       lastOp().isInteger() ) {
    auto instrs = prop_DD_DC16_1( o, lastOp() );
    if ( !instrs.empty() ) {
      mNewInstructions.emplace(
        const_cast<WIR_Operation &>( o ), std::move( instrs ) );
      return( true );
    }
  }

  // MOV_RR Dc, const Db (DD/SDD_1) -> MOVH Dc, const16 (DC16_2)
  // MOV.AA Ac, const Ab (AA/SAA_1) -> MOVH.A Ac, const16 (AC16)
  if ( ( ( ( o.getOpCode() == TC13::OpCode::MOV_RR ) &&
           ( ( o.getOperationFormat() == TC13::OperationFormat::DD ) ||
             ( o.getOperationFormat() == TC13::OperationFormat::SDD_1 ) ) ) ||
         ( ( o.getOpCode() == TC13::OpCode::MOV_AA ) &&
           ( ( o.getOperationFormat() == TC13::OperationFormat::AA ) ||
             ( o.getOperationFormat() == TC13::OperationFormat::SAA_1 ) ) ) ) &&
       lastOp().isInteger() ) {
    auto instrs = prop_xx_xC16( o, lastOp() );
    if ( !instrs.empty() ) {
      mNewInstructions.emplace(
        const_cast<WIR_Operation &>( o ), std::move( instrs ) );
      return( true );
    }
  }

  //
  // Propagate operation formats DDE/DDDC5 into DDC5C5/DDDC5.
  //

  if ( ( ( o.getOpCode() == TC13::OpCode::EXTR ) ||
         ( o.getOpCode() == TC13::OpCode::EXTR_U ) ) &&
       ( o.getOperationFormat() == TC13::OperationFormat::DDDC5 ) &&
       nthOp( 2 ).extract( 0, 5 ).isInteger() ) {
    auto instrs = prop_DDDC5_DDC5C5( o, nthOp( 2 ) );
    if ( !instrs.empty() ) {
      mNewInstructions.emplace(
        const_cast<WIR_Operation &>( o ), std::move( instrs ) );
      return( true );
    }
  }

  if ( ( ( o.getOpCode() == TC13::OpCode::EXTR ) ||
         ( o.getOpCode() == TC13::OpCode::EXTR_U ) ) &&
       ( o.getOperationFormat() == TC13::OperationFormat::DDE ) &&
       lastOp().extract( 32, 5 ).isInteger() &&
       lastOp().extract( 0, 5 ).isInteger() ) {
    auto instrs = prop_DDE_DDC5C5( o, lastOp() );
    if ( !instrs.empty() ) {
      mNewInstructions.emplace(
        const_cast<WIR_Operation &>( o ), std::move( instrs ) );
exit( 11 );
      return( true );
    }
  }

  if ( ( ( o.getOpCode() == TC13::OpCode::EXTR ) ||
         ( o.getOpCode() == TC13::OpCode::EXTR_U ) ) &&
       ( o.getOperationFormat() == TC13::OperationFormat::DDE ) &&
       lastOp().extract( 32, 5 ).isInteger() ) {
    auto instrs = prop_DDE_DDC5C5_2( o, lastOp() );
    if ( !instrs.empty() ) {
      mNewInstructions.emplace(
        const_cast<WIR_Operation &>( o ), std::move( instrs ) );
exit( 12 );
      return( true );
    }
  }

  //
  // Propagate operation format DDDD into DDDC5.
  //

  if ( ( o.getOpCode() == TC13::OpCode::DEXTR ) &&
       ( o.getOperationFormat() == TC13::OperationFormat::DDDD ) &&
       lastOp().extract( 0, 5 ).isInteger() ) {
    auto instrs = prop_DDDD_DDDC5( o, lastOp() );
    if ( !instrs.empty() ) {
      mNewInstructions.emplace(
        const_cast<WIR_Operation &>( o ), std::move( instrs ) );
      return( true );
    }
  }

  //
  // Propagate operation formats DD*E/DD*DC5 into DD*C5C5/DD*DC5.
  //

  if ( ( o.getOpCode() == TC13::OpCode::INSERT ) &&
       ( ( o.getOperationFormat() == TC13::OperationFormat::DDC4DC5 ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::DDDDC5 ) ) &&
       nthOp( 3 ).extract( 0, 5 ).isInteger() ) {
    auto instrs = prop_DDxDC5_DDxC5C5( o, nthOp( 3 ) );
    if ( !instrs.empty() ) {
      mNewInstructions.emplace(
        const_cast<WIR_Operation &>( o ), std::move( instrs ) );
      return( true );
    }
  }

  if ( ( o.getOpCode() == TC13::OpCode::INSERT ) &&
       ( ( o.getOperationFormat() == TC13::OperationFormat::DDC4E ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::DDDE ) ) &&
       lastOp().extract( 32, 5 ).isInteger() &&
       lastOp().extract( 0, 5 ).isInteger() ) {
    auto instrs = prop_DDxE_DDxC5C5( o, lastOp() );
    if ( !instrs.empty() ) {
      mNewInstructions.emplace(
        const_cast<WIR_Operation &>( o ), std::move( instrs ) );
      return( true );
    }
  }

  if ( ( o.getOpCode() == TC13::OpCode::INSERT ) &&
       ( ( o.getOperationFormat() == TC13::OperationFormat::DDC4E ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::DDDE ) ) &&
       lastOp().extract( 32, 5 ).isInteger() ) {
    auto instrs = prop_DDxE_DDxC5C5_2( o, lastOp() );
    if ( !instrs.empty() ) {
      mNewInstructions.emplace(
        const_cast<WIR_Operation &>( o ), std::move( instrs ) );
      return( true );
    }
  }

  //
  // Propagate operation formats EDDC5 and friends into EC4C5C5 etc.
  //

  if ( ( o.getOpCode() == TC13::OpCode::IMASK ) &&
       ( o.getOperationFormat() == TC13::OperationFormat::EDDC5 ) &&
       nthOp( 1 ).isInteger() && nthOp( 2 ).extract( 0, 5 ).isInteger() ) {
    auto instrs = prop_EDDC5_EC4C5C5( o, nthOp( 1 ), nthOp( 2 ) );
    if ( !instrs.empty() ) {
      mNewInstructions.emplace(
        const_cast<WIR_Operation &>( o ), std::move( instrs ) );
exit( 8 );
      return( true );
    }
  }

  // IMASK Ec, *, const Dd, width (ExDC5) -> IMASK Ec, *, const5, width (ExC5C5)
  if ( ( o.getOpCode() == TC13::OpCode::IMASK ) &&
       ( ( o.getOperationFormat() == TC13::OperationFormat::EC4DC5 ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::EDDC5 ) ) &&
       nthOp( 2 ).extract( 0, 5 ).isInteger() ) {
    auto instrs = prop_ExDC5_ExC5C5( o, nthOp( 2 ) );
    if ( !instrs.empty() ) {
      mNewInstructions.emplace(
        const_cast<WIR_Operation &>( o ), std::move( instrs ) );
      return( true );
    }
  }

  // IMASK Ec, const Db, *, width (EDxC5) -> IMASK Ec, const4, *, width (EC4xC5)
  if ( ( o.getOpCode() == TC13::OpCode::IMASK ) &&
       ( ( o.getOperationFormat() == TC13::OperationFormat::EDC5C5 ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::EDDC5 ) ) &&
       nthOp( 1 ).isInteger() ) {
    auto instrs = prop_EDxC5_EC4xC5( o, nthOp( 1 ) );
    if ( !instrs.empty() ) {
      mNewInstructions.emplace(
        const_cast<WIR_Operation &>( o ), std::move( instrs ) );
      return( true );
    }
  }

  //
  // Check LD.A, LD.W and ST.W featuring 16bit offsets.
  //

  if ( ( ( o.getOpCode() == TC13::OpCode::LD_A ) ||
         ( o.getOpCode() == TC13::OpCode::LD_W ) ) &&
       ( ( o.getOperationFormat() == TC13::OperationFormat::AAC10BOA ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::AAC16BOA ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::DAC10BOA ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::DAC16BOA ) ) ) {
    auto &aregValue = nthOp( 1 );

    if ( isSymbol( aregValue ) ) {
      auto &sym = aregValue.getLocation( 0 ).getSymbol();

      mNewInstructions.emplace(
        const_cast<WIR_Operation &>( o ), prop_xACBOA_xALBOA( o, sym ) );
      return( true );
    }
  }

  if ( ( o.getOpCode() == TC13::OpCode::ST_W ) &&
       ( ( o.getOperationFormat() == TC13::OperationFormat::AC10DBOA_1 ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::AC16DBOA ) ) ) {
    auto &aregValue = firstOp();

    if ( isSymbol( aregValue ) ) {
      auto &sym = aregValue.getLocation( 0 ).getSymbol();

      mNewInstructions.emplace(
        const_cast<WIR_Operation &>( o ), prop_ACDBOA_ALDBOA( o, sym ) );
      return( true );
    }
  }

  return( false );
};


//
// Private class methods
//

/*
  prop_DDD_DDC9 propagates constants for a TriCore operation of format DDD_1 or
  DDD_2.

  If the given up value does not fit into a 9 bits signed constant,
  prop_DDD_DDC9 returns an empty instruction list.
*/
list<WIR_Instruction> TC_ConstProp::prop_DDD_DDC9( const WIR_Operation &o,
                                                   const WIR_UpDownValue &v )
{
  DSTART(
    "list<WIR_Instruction> TC_ConstProp::prop_DDD_DDC9(const WIR_Operation&, const WIR_UpDownValue&)" );

  list<WIR_Instruction> res;

  auto c0 = replace( v, WIR_L4::b0, true );
  auto c1 = replace( v, WIR_L4::b1, true );

  // Determine whether c0 or c1 are 9 bits signed constants.
  bool c0fits =
    ( c0 >= TC_Const16_Signed::getMinValue( 9 ) ) &&
    ( c0 <= TC_Const16_Signed::getMaxValue( 9 ) );
  bool c1fits =
    ( c1 >= TC_Const16_Signed::getMinValue( 9 ) ) &&
    ( c1 <= TC_Const16_Signed::getMaxValue( 9 ) );

  if ( c0fits || c1fits ) {
    // Do the actual constant propagation of the last register parameter of
    // format DDD_1/DDD_2 to DDC9_1/DDC9_3, resp.
    auto it = o.getExplicitParameters().begin();
    auto &p1o = it->get();

    auto *p1 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( (it++)->get() ) );
    auto *p2 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( it->get() ) );
    auto &p2o = it->get();
    auto *p3 = new TC_Const9_Signed( c0fits ? c0 : c1 );

    // Patch incoming bit-values for all uses of p1.
    patchDefinedParameter( *p1, p1o );

    if ( p1->isDefUsed() )
      // Patch outgoing bit-values for all definitions of p1, if so.
      patchUsedParameter( *p1, p1o );

    // Patch outgoing bit-values for all definitions of p2.
    patchUsedParameter( *p2, p2o );

    // Add bit-value container for new immediate parameter p3.
    patchImmediateParameter( *p3 );

    res.push_back(
      { { o.getOpCode(),
          o.getOperationFormat() == TC13::OperationFormat::DDD_1 ?
            TC13::OperationFormat::DDC9_1 : TC13::OperationFormat::DDC9_3,
          p1, p2, p3 } } );

    checkSelfEdges( res.back().begin()->get(), o );
    copyContainers( res.back().begin()->get(), o );
  }

  return( res );
};


/*
  prop_DDD_DDC9_2 propagates constants for a commutative TriCore operation of
  format DDD_1 or DDD_2.

  If the given up value does not fit into a 9 bits signed constant,
  prop_DDD_DDC9_2 returns an empty instruction list.
*/
list<WIR_Instruction> TC_ConstProp::prop_DDD_DDC9_2( const WIR_Operation &o,
                                                     const WIR_UpDownValue &v )
{
  DSTART(
    "list<WIR_Instruction> TC_ConstProp::prop_DDD_DDC9_2(const WIR_Operation&, const WIR_UpDownValue&)" );

  list<WIR_Instruction> res;

  auto c0 = replace( v, WIR_L4::b0, true );
  auto c1 = replace( v, WIR_L4::b1, true );

  // Determine whether c0 or c1 are 9 bits signed constants.
  bool c0fits =
    ( c0 >= TC_Const16_Signed::getMinValue( 9 ) ) &&
    ( c0 <= TC_Const16_Signed::getMaxValue( 9 ) );
  bool c1fits =
    ( c1 >= TC_Const16_Signed::getMinValue( 9 ) ) &&
    ( c1 <= TC_Const16_Signed::getMaxValue( 9 ) );

  if ( c0fits || c1fits ) {
    // Do the actual constant propagation of the second register parameter of
    // format DDD_1/DDD_2 to DDC9_1/DDC9_3, resp.
    auto it = o.getExplicitParameters().begin();
    auto &p1o = it->get();

    auto *p1 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( (it++)->get() ) );
    ++it;
    auto *p2 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( it->get() ) );
    auto &p3o = it->get();
    auto *p3 = new TC_Const9_Signed( c0fits ? c0 : c1 );

    // Patch incoming bit-values for all uses of p1.
    patchDefinedParameter( *p1, p1o );

    if ( p1->isDefUsed() )
      // Patch outgoing bit-values for all definitions of p1, if so.
      patchUsedParameter( *p1, p1o );

    // Patch outgoing bit-values for all definitions of p2.
    patchUsedParameter( *p2, p3o );

    // Add bit-value container for new immediate parameter p3.
    patchImmediateParameter( *p3 );

    res.push_back(
      { { o.getOpCode(),
          o.getOperationFormat() == TC13::OperationFormat::DDD_1 ?
            TC13::OperationFormat::DDC9_1 : TC13::OperationFormat::DDC9_3,
          p1, p2, p3 } } );

    checkSelfEdges( res.back().begin()->get(), o );
    copyContainers( res.back().begin()->get(), o );
  }

  return( res );
};


/*
  prop_DDD_DDC9_3 propagates constants for the commutative TriCore operations
  *GE and *LT of format DDD_1 or DDD_2.

  For *GE Dc, "const Da", Db, prop_DDD_DDC9_3 returns *LT Dc, Db, "const Da+1".
  For *LT Dc, "const Da", Db, prop_DDD_DDC9_3 returns *GE Dc, Db, "const Da+1".
  If the given up value plus one does not fit into a 9 bits signed constant,
  prop_DDD_DDC9_3 returns an empty instruction list.
*/
list<WIR_Instruction> TC_ConstProp::prop_DDD_DDC9_3( const WIR_Operation & o,
                                                     const WIR_UpDownValue &v )
{
  DSTART(
    "list<WIR_Instruction> TC_ConstProp::prop_DDD_DDC9_3(const WIR_Operation&, const WIR_UpDownValue&)" );

  list<WIR_Instruction> res;

  auto c0 = replace( v, WIR_L4::b0, true ) + 1;
  auto c1 = replace( v, WIR_L4::b1, true ) + 1;

  // Determine whether c0 or c1 are 9 bits signed constants.
  bool c0fits =
    ( c0 >= TC_Const16_Signed::getMinValue( 9 ) ) &&
    ( c0 <= TC_Const16_Signed::getMaxValue( 9 ) );
  bool c1fits =
    ( c1 >= TC_Const16_Signed::getMinValue( 9 ) ) &&
    ( c1 <= TC_Const16_Signed::getMaxValue( 9 ) );

  if ( c0fits || c1fits ) {
    // Do the actual constant propagation of the second register parameter of
    // format DDD_1/DDD_2 to DDC9_1/DDC9_3, resp.
    auto it = o.getExplicitParameters().begin();
    auto &p1o = it->get();

    auto *p1 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( (it++)->get() ) );
    ++it;
    auto *p2 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( it->get() ) );
    auto &p3o = it->get();
    auto *p3 = new TC_Const9_Signed( c0fits ? c0 : c1 );

    // Patch incoming bit-values for all uses of p1.
    patchDefinedParameter( *p1, p1o );

    if ( p1->isDefUsed() )
      // Patch outgoing bit-values for all definitions of p1, if so.
      patchUsedParameter( *p1, p1o );

    // Patch outgoing bit-values for all definitions of p2.
    patchUsedParameter( *p2, p3o );

    // Add bit-value container for new immediate parameter p3.
    patchImmediateParameter( *p3 );

    auto &oc = o.getOpCode();
    res.push_back(
      { { oc == TC13::OpCode::GE ? TC13::OpCode::LT :
            oc == TC13::OpCode::LT ? TC13::OpCode::GE :
              oc == TC13::OpCode::AND_GE ? TC13::OpCode::AND_LT :
                oc == TC13::OpCode::AND_LT ? TC13::OpCode::AND_GE :
                  oc == TC13::OpCode::OR_GE ? TC13::OpCode::OR_LT :
                    oc == TC13::OpCode::OR_LT ? TC13::OpCode::OR_GE :
                      oc == TC13::OpCode::SH_GE ? TC13::OpCode::SH_LT :
                        oc == TC13::OpCode::SH_LT ? TC13::OpCode::SH_GE :
                          oc == TC13::OpCode::XOR_GE ? TC13::OpCode::XOR_LT :
                            TC13::OpCode::XOR_GE,
          o.getOperationFormat() == TC13::OperationFormat::DDD_1 ?
            TC13::OperationFormat::DDC9_1 : TC13::OperationFormat::DDC9_3,
          p1, p2, p3 } } );

    checkSelfEdges( res.back().begin()->get(), o );
    copyContainers( res.back().begin()->get(), o );
  }

  return( res );
};


/*
  prop_DDD_DDC9_4 propagates constants for a TriCore SUB or SUBS operation of
  format DDD_1.

  For SUB* Dc, Da, "const Db", prop_DDD_DDC9_4 returns
  ADD* Dc, Da, -1*"const Db".
  If the inverse of the given up value does not fit into a 9 bits signed
  constant, prop_DDD_DDC9_4 returns an empty instruction list.
*/
list<WIR_Instruction> TC_ConstProp::prop_DDD_DDC9_4( const WIR_Operation &o,
                                                     const WIR_UpDownValue &v )
{
  DSTART(
    "list<WIR_Instruction> TC_ConstProp::prop_DDD_DDC9_4(const WIR_Operation&, const WIR_UpDownValue&)" );

  list<WIR_Instruction> res;

  auto c0 = -1 * replace( v, WIR_L4::b0, true );
  auto c1 = -1 * replace( v, WIR_L4::b1, true );

  // Determine whether c0 or c1 are 9 bits signed constants.
  bool c0fits =
    ( c0 >= TC_Const16_Signed::getMinValue( 9 ) ) &&
    ( c0 <= TC_Const16_Signed::getMaxValue( 9 ) );
  bool c1fits =
    ( c1 >= TC_Const16_Signed::getMinValue( 9 ) ) &&
    ( c1 <= TC_Const16_Signed::getMaxValue( 9 ) );

  if ( c0fits || c1fits ) {
    // Do the actual constant propagation of the last register parameter of
    // format DDD_1 to DDC9_1.
    auto it = o.getExplicitParameters().begin();
    auto &p1o = it->get();

    auto *p1 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( (it++)->get() ) );
    auto *p2 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( it->get() ) );
    auto &p2o = it->get();
    auto *p3 = new TC_Const9_Signed( c0fits ? c0 : c1 );

    // Patch incoming bit-values for all uses of p1.
    patchDefinedParameter( *p1, p1o );

    // Patch outgoing bit-values for all definitions of p2.
    patchUsedParameter( *p2, p2o );

    // Add bit-value container for new immediate parameter p3.
    patchImmediateParameter( *p3 );

    res.push_back(
      { { o.getOpCode() == TC13::OpCode::SUB ?
            TC13::OpCode::ADD : TC13::OpCode::ADDS,
          TC13::OperationFormat::DDC9_1, p1, p2, p3 } } );

    checkSelfEdges( res.back().begin()->get(), o );
    copyContainers( res.back().begin()->get(), o );
  }

  return( res );
};


/*
  prop_DDD_DDC9_5 propagates constants for the commutative TriCore operations
  SUB or SUBS of format DDD_1.

  For SUB* Dc, "const Da", Db, prop_DDD_DDC9_5 returns RSUB* Dc, Db, "const Da".
  If the given up value does not fit into a 9 bits signed constant,
  prop_DDD_DDC9_5 returns an empty instruction list.
*/
list<WIR_Instruction> TC_ConstProp::prop_DDD_DDC9_5( const WIR_Operation &o,
                                                     const WIR_UpDownValue &v )
{
  DSTART(
    "list<WIR_Instruction> TC_ConstProp::prop_DDD_DDC9_5(const WIR_Operation&, const WIR_UpDownValue&)" );

  list<WIR_Instruction> res;

  auto c0 = replace( v, WIR_L4::b0, true );
  auto c1 = replace( v, WIR_L4::b1, true );

  // Determine whether c0 or c1 are 9 bits signed constants.
  bool c0fits =
    ( c0 >= TC_Const16_Signed::getMinValue( 9 ) ) &&
    ( c0 <= TC_Const16_Signed::getMaxValue( 9 ) );
  bool c1fits =
    ( c1 >= TC_Const16_Signed::getMinValue( 9 ) ) &&
    ( c1 <= TC_Const16_Signed::getMaxValue( 9 ) );

  if ( c0fits || c1fits ) {
    // Do the actual constant propagation of the second register parameter of
    // format DDD_1 to DDC9_1.
    auto it = o.getExplicitParameters().begin();
    auto &p1o = it->get();

    auto *p1 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( (it++)->get() ) );
    ++it;
    auto *p2 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( it->get() ) );
    auto &p3o = it->get();
    auto *p3 = new TC_Const9_Signed( c0fits ? c0 : c1 );

    // Patch incoming bit-values for all uses of p1.
    patchDefinedParameter( *p1, p1o );

    // Patch outgoing bit-values for all definitions of p2.
    patchUsedParameter( *p2, p3o );

    // Add bit-value container for new immediate parameter p3.
    patchImmediateParameter( *p3 );

    res.push_back(
      { { o.getOpCode() == TC13::OpCode::SUB ?
            TC13::OpCode::RSUB : TC13::OpCode::RSUBS,
          TC13::OperationFormat::DDC9_1, p1, p2, p3 } } );

    checkSelfEdges( res.back().begin()->get(), o );
    copyContainers( res.back().begin()->get(), o );
  }

  return( res );
};


/*
  prop_DDD_DDC9_6 propagates constants for a TriCore operation of format
  DDDPSW_1 or DDDPSW_2.

  If the given up value does not fit into a 9 bits signed constant,
  prop_DDD_DDC9_6 returns an empty instruction list.
*/
list<WIR_Instruction> TC_ConstProp::prop_DDD_DDC9_6( const WIR_Operation &o,
                                                     const WIR_UpDownValue &v )
{
  DSTART(
    "list<WIR_Instruction> TC_ConstProp::prop_DDD_DDC9_6(const WIR_Operation&, const WIR_UpDownValue&)" );

  list<WIR_Instruction> res;

  auto c0 = replace( v, WIR_L4::b0, true );
  auto c1 = replace( v, WIR_L4::b1, true );

  // Determine whether c0 or c1 are 9 bits signed constants.
  bool c0fits =
    ( c0 >= TC_Const16_Signed::getMinValue( 9 ) ) &&
    ( c0 <= TC_Const16_Signed::getMaxValue( 9 ) );
  bool c1fits =
    ( c1 >= TC_Const16_Signed::getMinValue( 9 ) ) &&
    ( c1 <= TC_Const16_Signed::getMaxValue( 9 ) );

  if ( c0fits || c1fits ) {
    // Do the actual constant propagation of the last data register parameter of
    // format DDDPSW_1/DDDPSW_2 to DDC9PSW_1/DDC9PSW_2, resp.
    auto it = o.getExplicitParameters().begin();
    auto &p1o = it->get();

    auto *p1 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( (it++)->get() ) );
    auto &p2o = it->get();
    auto *p2 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( (it++)->get() ) );
    auto *p3 = new TC_Const9_Signed( c0fits ? c0 : c1 );
    auto &p4o = (++it)->get();
    auto *p4 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( it->get() ) );

    // Patch incoming bit-values for all uses of p1.
    patchDefinedParameter( *p1, p1o );

    // Patch outgoing bit-values for all definitions of p2.
    patchUsedParameter( *p2, p2o );

    // Add bit-value container for new immediate parameter p3.
    patchImmediateParameter( *p3 );

    // Patch incoming bit-values for all uses of p4/PSW.C.
    patchDefinedParameter( *p4, p4o );

    if ( p4->isDefUsed() )
      // Patch outgoing bit-values for all definitions of p4/PSW.C, if so.
      patchUsedParameter( *p4, p4o );

    res.push_back(
      { { o.getOpCode(),
          o.getOperationFormat() == TC13::OperationFormat::DDDPSW_1 ?
            TC13::OperationFormat::DDC9PSW_1 : TC13::OperationFormat::DDC9PSW_2,
          p1, p2, p3, p4 } } );

    checkSelfEdges( res.back().begin()->get(), o );
    copyContainers( res.back().begin()->get(), o );
  }

  return( res );
};


/*
  prop_DDD_DDC9_7 propagates constants for a commutative TriCore operation of
  format DDDPSW_1 or DDDPSW_2.

  If the given up value does not fit into a 9 bits signed constant,
  prop_DDD_DDC9_7 returns an empty instruction list.
*/
list<WIR_Instruction> TC_ConstProp::prop_DDD_DDC9_7( const WIR_Operation &o,
                                                     const WIR_UpDownValue &v )
{
  DSTART(
    "list<WIR_Instruction> TC_ConstProp::prop_DDD_DDC9_7(const WIR_Operation&, const WIR_UpDownValue&)" );

  list<WIR_Instruction> res;

  auto c0 = replace( v, WIR_L4::b0, true );
  auto c1 = replace( v, WIR_L4::b1, true );

  // Determine whether c0 or c1 are 9 bits signed constants.
  bool c0fits =
    ( c0 >= TC_Const16_Signed::getMinValue( 9 ) ) &&
    ( c0 <= TC_Const16_Signed::getMaxValue( 9 ) );
  bool c1fits =
    ( c1 >= TC_Const16_Signed::getMinValue( 9 ) ) &&
    ( c1 <= TC_Const16_Signed::getMaxValue( 9 ) );

  if ( c0fits || c1fits ) {
    // Do the actual constant propagation of the second register parameter of
    // format DDDPSW_1/DDDPSW_2 to DDC9PSW_1/DDC9PSW_2, resp.
    auto it = o.getExplicitParameters().begin();
    auto &p1o = it->get();

    auto *p1 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( (it++)->get() ) );
    ++it;
    auto *p2 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( it->get() ) );
    auto &p3o = (it++)->get();
    auto *p3 = new TC_Const9_Signed( c0fits ? c0 : c1 );
    auto &p4o = it->get();
    auto *p4 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( it->get() ) );

    // Patch incoming bit-values for all uses of p1.
    patchDefinedParameter( *p1, p1o );

    // Patch outgoing bit-values for all definitions of p2.
    patchUsedParameter( *p2, p3o );

    // Add bit-value container for new immediate parameter p3.
    patchImmediateParameter( *p3 );

    // Patch incoming bit-values for all uses of p4/PSW.C.
    patchDefinedParameter( *p4, p4o );

    if ( p1->isDefUsed() )
      // Patch outgoing bit-values for all definitions of p4/PSW.C, if so.
      patchUsedParameter( *p4, p4o );

    res.push_back(
      { { o.getOpCode(),
          o.getOperationFormat() == TC13::OperationFormat::DDDPSW_1 ?
            TC13::OperationFormat::DDC9PSW_1 : TC13::OperationFormat::DDC9PSW_2,
          p1, p2, p3, p4 } } );

    checkSelfEdges( res.back().begin()->get(), o );
    copyContainers( res.back().begin()->get(), o );
  }

  return( res );
};


/*
  prop_DDD_DDC16 propagates constants for a TriCore ADD operation of format
  DDD_1.

  If the given up value does not fit into a 16 bits signed constant,
  prop_DDD_DDC16 returns an empty instruction list.
*/
list<WIR_Instruction> TC_ConstProp::prop_DDD_DDC16( const WIR_Operation &o,
                                                    const WIR_UpDownValue &v )
{
  DSTART(
    "list<WIR_Instruction> TC_ConstProp::prop_DDD_DDC16(const WIR_Operation&, const WIR_UpDownValue&)" );

  list<WIR_Instruction> res;

  auto c0 = replace( v, WIR_L4::b0, true );
  auto c1 = replace( v, WIR_L4::b1, true );

  // Determine whether c0 or c1 are 16 bits signed constants.
  bool c0fits =
    ( c0 >= TC_Const16_Signed::getMinValue( 16 ) ) &&
    ( c0 <= TC_Const16_Signed::getMaxValue( 16 ) );
  bool c1fits =
    ( c1 >= TC_Const16_Signed::getMinValue( 16 ) ) &&
    ( c1 <= TC_Const16_Signed::getMaxValue( 16 ) );

  if ( c0fits || c1fits ) {
    // Do the actual constant propagation of the last register parameter of
    // format DDD_1 to DDC16_1.
    auto it = o.getExplicitParameters().begin();
    auto &p1o = it->get();

    auto *p1 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( (it++)->get() ) );
    auto *p2 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( it->get() ) );
    auto &p2o = it->get();
    auto *p3 = new TC_Const16_Signed( c0fits ? c0 : c1 );

    // Patch incoming bit-values for all uses of p1.
    patchDefinedParameter( *p1, p1o );

    // Patch outgoing bit-values for all definitions of p2.
    patchUsedParameter( *p2, p2o );

    // Add bit-value container for new immediate parameter p3.
    patchImmediateParameter( *p3 );

    res.push_back(
      { { TC13::OpCode::ADDI, TC13::OperationFormat::DDC16_1, p1, p2, p3 } } );

    checkSelfEdges( res.back().begin()->get(), o );
    copyContainers( res.back().begin()->get(), o );
  }

  return( res );
};


/*
  prop_DDD_DDC16_1 propagates constants for the commutative TriCore ADD
  operation of format DDD_1.

  If the given up value does not fit into a 16 bits signed constant,
  prop_DDD_DDC16 returns an empty instruction list.
*/
list<WIR_Instruction> TC_ConstProp::prop_DDD_DDC16_1( const WIR_Operation &o,
                                                      const WIR_UpDownValue &v )
{
  DSTART(
    "list<WIR_Instruction> TC_ConstProp::prop_DDD_DDC16_1(const WIR_Operation&, const WIR_UpDownValue&)" );

  list<WIR_Instruction> res;

  auto c0 = replace( v, WIR_L4::b0, true );
  auto c1 = replace( v, WIR_L4::b1, true );

  // Determine whether c0 or c1 are 9 bits signed constants.
  bool c0fits =
    ( c0 >= TC_Const16_Signed::getMinValue( 16 ) ) &&
    ( c0 <= TC_Const16_Signed::getMaxValue( 16 ) );
  bool c1fits =
    ( c1 >= TC_Const16_Signed::getMinValue( 16 ) ) &&
    ( c1 <= TC_Const16_Signed::getMaxValue( 16 ) );

  if ( c0fits || c1fits ) {
    // Do the actual constant propagation of the second register parameter of
    // format DDD_1 to DDC16_1.
    auto it = o.getExplicitParameters().begin();
    auto &p1o = it->get();

    auto *p1 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( (it++)->get() ) );
    ++it;
    auto *p2 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( it->get() ) );
    auto &p3o = it->get();
    auto *p3 = new TC_Const16_Signed( c0fits ? c0 : c1 );

    // Patch incoming bit-values for all uses of p1.
    patchDefinedParameter( *p1, p1o );

    // Patch outgoing bit-values for all definitions of p2.
    patchUsedParameter( *p2, p3o );

    // Add bit-value container for new immediate parameter p3.
    patchImmediateParameter( *p3 );

    res.push_back(
      { { TC13::OpCode::ADDI, TC13::OperationFormat::DDC16_1, p1, p2, p3 } } );

    checkSelfEdges( res.back().begin()->get(), o );
    copyContainers( res.back().begin()->get(), o );
  }

  return( res );
};



/*
  prop_xxx_xxC16 propagates constants for a TriCore ADD/ADD.A operation of
  formats DDD_1/AAA.

  If the given up value does not fit into the shape expected by ADDIH/ADDIH.A,
  prop_xxx_xxC16 returns an empty instruction list.
*/
list<WIR_Instruction> TC_ConstProp::prop_xxx_xxC16( const WIR_Operation &o,
                                                    const WIR_UpDownValue &v )
{
  DSTART(
    "list<WIR_Instruction> TC_ConstProp::prop_xxx_xxC16(const WIR_Operation&, const WIR_UpDownValue&)" );

  list<WIR_Instruction> res;

  auto c0 = replace( v.extract( 16, 16 ), WIR_L4::b0, false );
  auto c1 = replace( v.extract( 0, 16 ), WIR_L4::b0, false );

  // Determine whether c0 is a 16 bits unsigned constant and c1 is 0.
  bool c0fits =
    ( c0 >= 0 ) && ( c0 <= (long long) TC_Const16_Unsigned::getMaxValue( 16 ) );
  bool c1fits = ( c1 == 0 );

  if ( c0fits && c1fits ) {
    // Do the actual constant propagation of the last register parameter of
    // format DDD_1/AAA to DDC16_2/AAC16.
    auto it = o.getExplicitParameters().begin();
    auto &p1o = it->get();

    auto *p1 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( (it++)->get() ) );
    auto *p2 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( it->get() ) );
    auto &p2o = it->get();
    auto *p3 = new TC_Const16_Unsigned( c0 );

    // Patch incoming bit-values for all uses of p1.
    patchDefinedParameter( *p1, p1o );

    // Patch outgoing bit-values for all definitions of p2.
    patchUsedParameter( *p2, p2o );

    // Add bit-value container for new immediate parameter p3.
    patchImmediateParameter( *p3 );

    res.push_back(
      { { o.getOpCode() == TC13::OpCode::ADD ?
            TC13::OpCode::ADDIH : TC13::OpCode::ADDIH_A,
          o.getOperationFormat() == TC13::OperationFormat::DDD_1 ?
            TC13::OperationFormat::DDC16_2 : TC13::OperationFormat::AAC16,
          p1, p2, p3 } } );

    checkSelfEdges( res.back().begin()->get(), o );
    copyContainers( res.back().begin()->get(), o );
  }

  return( res );
};


/*
  prop_xxx_xxC16_1 propagates constants for a commutative TriCore ADD/ADD.A
  operation of formats DDD_1/AAA.

  If the given up value does not fit into the shape expected by ADDIH/ADDIH.A,
  prop_xxx_xxC16_1 returns an empty instruction list.
*/
list<WIR_Instruction> TC_ConstProp::prop_xxx_xxC16_1( const WIR_Operation &o,
                                                      const WIR_UpDownValue &v )
{
  DSTART(
    "list<WIR_Instruction> TC_ConstProp::prop_xxx_xxC16_1(const WIR_Operation&, const WIR_UpDownValue&)" );

  list<WIR_Instruction> res;

  auto c0 = replace( v.extract( 16, 16 ), WIR_L4::b0, false );
  auto c1 = replace( v.extract( 0, 16 ), WIR_L4::b0, false );

  // Determine whether c0 is a 16 bits signed constant and c1 is 0.
  bool c0fits =
    ( c0 >= 0 ) && ( c0 <= (long long) TC_Const16_Unsigned::getMaxValue( 16 ) );
  bool c1fits = ( c1 == 0 );

  if ( c0fits && c1fits ) {
    // Do the actual constant propagation of the second register parameter of
    // format DDD_1/AAA to DDC16_2/AAC16.
    auto it = o.getExplicitParameters().begin();
    auto &p1o = it->get();

    auto *p1 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( (it++)->get() ) );
    ++it;
    auto *p2 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( it->get() ) );
    auto &p3o = it->get();
    auto *p3 = new TC_Const16_Unsigned( c0 );

    // Patch incoming bit-values for all uses of p1.
    patchDefinedParameter( *p1, p1o );

    // Patch outgoing bit-values for all definitions of p2.
    patchUsedParameter( *p2, p3o );

    // Add bit-value container for new immediate parameter p3.
    patchImmediateParameter( *p3 );

    res.push_back(
      { { o.getOpCode() == TC13::OpCode::ADD ?
            TC13::OpCode::ADDIH : TC13::OpCode::ADDIH_A,
          o.getOperationFormat() == TC13::OperationFormat::DDD_1 ?
            TC13::OperationFormat::DDC16_2 : TC13::OperationFormat::AAC16,
          p1, p2, p3 } } );

    checkSelfEdges( res.back().begin()->get(), o );
    copyContainers( res.back().begin()->get(), o );
  }

  return( res );
};


/*
  prop_xDD_DDC9_u propagates constants for an unsigned TriCore operation of
  format DDD_1 or EDD.

  If the given up value does not fit into a 9 bits unsigned constant,
  prop_xDD_DDC9_u returns an empty instruction list.
*/
list<WIR_Instruction> TC_ConstProp::prop_xDD_DDC9_u( const WIR_Operation &o,
                                                     const WIR_UpDownValue &v )
{
  DSTART(
    "list<WIR_Instruction> TC_ConstProp::prop_xDD_DDC9_u(const WIR_Operation&, const WIR_UpDownValue&)" );

  list<WIR_Instruction> res;

  auto c0 = replace( v, WIR_L4::b0, false );

  // Determine whether c0 is a 9 bits unsigned constant.
  bool c0fits =
    ( c0 >= 0 ) && ( c0 <= (long long) TC_Const16_Unsigned::getMaxValue( 9 ) );

  if ( c0fits ) {
    // Do the actual constant propagation of the last register parameter of
    // format DDD_1 to DDC9_2.
    auto it = o.getExplicitParameters().begin();
    auto &p1o = it->get();

    auto *p1 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( (it++)->get() ) );
    auto *p2 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( it->get() ) );
    auto &p2o = it->get();
    auto *p3 = new TC_Const9_Unsigned( c0 );

    // Patch incoming bit-values for all uses of p1.
    patchDefinedParameter( *p1, p1o );

    // Patch outgoing bit-values for all definitions of p2.
    patchUsedParameter( *p2, p2o );

    // Add bit-value container for new immediate parameter p3.
    patchImmediateParameter( *p3 );

    res.push_back(
      { { o.getOpCode(),
          o.getOpCode() == TC13::OpCode::MUL_U ?
            TC13::OperationFormat::EDC9_2 : TC13::OperationFormat::DDC9_2,
          p1, p2, p3 } } );

    checkSelfEdges( res.back().begin()->get(), o );
    copyContainers( res.back().begin()->get(), o );
  }

  return( res );
};


/*
  prop_xDD_DDC9_2u propagates constants for a commutative unsigned TriCore
  operation of format DDD_1 or EDD.

  If the given up value does not fit into a 9 bits unsigned constant,
  prop_xDD_DDC9_2u returns an empty instruction list.
*/
list<WIR_Instruction> TC_ConstProp::prop_xDD_DDC9_2u( const WIR_Operation &o,
                                                      const WIR_UpDownValue &v )
{
  DSTART(
    "list<WIR_Instruction> TC_ConstProp::prop_xDD_DDC9_2u(const WIR_Operation&, const WIR_UpDownValue&)" );

  list<WIR_Instruction> res;

  auto c0 = replace( v, WIR_L4::b0, false );

  // Determine whether c0 is a 9 bits unsigned constant.
  bool c0fits =
    ( c0 >= 0 ) && ( c0 <= (long long) TC_Const16_Unsigned::getMaxValue( 9 ) );

  if ( c0fits ) {
    // Do the actual constant propagation of the second register parameter of
    // format DDD_1 to DDC9_2.
    auto it = o.getExplicitParameters().begin();
    auto &p1o = it->get();

    auto *p1 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( (it++)->get() ) );
    ++it;
    auto *p2 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( it->get() ) );
    auto &p3o = it->get();
    auto *p3 = new TC_Const9_Unsigned( c0 );

    // Patch incoming bit-values for all uses of p1.
    patchDefinedParameter( *p1, p1o );

    // Patch outgoing bit-values for all definitions of p2.
    patchUsedParameter( *p2, p3o );

    // Add bit-value container for new immediate parameter p3.
    patchImmediateParameter( *p3 );

    res.push_back(
      { { o.getOpCode(),
          o.getOpCode() == TC13::OpCode::MUL_U ?
            TC13::OperationFormat::EDC9_2 : TC13::OperationFormat::DDC9_2,
          p1, p2, p3 } } );

    checkSelfEdges( res.back().begin()->get(), o );
    copyContainers( res.back().begin()->get(), o );
  }

  return( res );
};


/*
  prop_DDD_DDC9_3u propagates constants for the commutative unsigned TriCore
  operations *GE.U and *LT_U of format DDD_1 or DDD_2.

  For GE.U Dc, "const Da", Db, prop_DDD_DDC9_3u returns
  LT.U Dc, Db, "const Da+1".
  For LT.U Dc, "const Da", Db, prop_DDD_DDC9_3u returns
  GE.U Dc, Db, "const Da+1".
  If the given up value plus one does not fit into a 9 bits unsigned constant,
  prop_DDD_DDC9_3u returns an empty instruction list.
*/
list<WIR_Instruction> TC_ConstProp::prop_DDD_DDC9_3u( const WIR_Operation &o,
                                                      const WIR_UpDownValue &v )
{
  DSTART(
    "list<WIR_Instruction> TC_ConstProp::prop_DDD_DDC9_3u(const WIR_Operation&, const WIR_UpDownValue&)" );

  list<WIR_Instruction> res;

  auto c0 = replace( v, WIR_L4::b0, false ) + 1;

  // Determine whether c0 is a 9 bits unsigned constant.
  bool c0fits =
    ( c0 >= 0 ) && ( c0 <= (long long) TC_Const16_Unsigned::getMaxValue( 9 ) );

  if ( c0fits ) {
    // Do the actual constant propagation of the second register parameter of
    // format DDD_1/DDD_2 to DDC9_2/DDC9_4, resp.
    auto it = o.getExplicitParameters().begin();
    auto &p1o = it->get();

    auto *p1 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( (it++)->get() ) );
    ++it;
    auto *p2 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( it->get() ) );
    auto &p3o = it->get();
    auto *p3 = new TC_Const9_Unsigned( c0 );

    // Patch incoming bit-values for all uses of p1.
    patchDefinedParameter( *p1, p1o );

    if ( p1->isDefUsed() )
      // Patch outgoing bit-values for all definitions of p1, if so.
      patchUsedParameter( *p1, p1o );

    // Patch outgoing bit-values for all definitions of p2.
    patchUsedParameter( *p2, p3o );

    // Add bit-value container for new immediate parameter p3.
    patchImmediateParameter( *p3 );

    auto &oc = o.getOpCode();
    res.push_back(
      { { oc == TC13::OpCode::GE_U ? TC13::OpCode::LT_U :
            oc == TC13::OpCode::LT_U ? TC13::OpCode::GE_U :
              oc == TC13::OpCode::AND_GE_U ? TC13::OpCode::AND_LT_U :
                oc == TC13::OpCode::AND_LT_U ? TC13::OpCode::AND_GE_U :
                  oc == TC13::OpCode::OR_GE_U ? TC13::OpCode::OR_LT_U :
                    oc == TC13::OpCode::OR_LT_U ? TC13::OpCode::OR_GE_U :
                      oc == TC13::OpCode::SH_GE_U ? TC13::OpCode::SH_LT_U :
                        oc == TC13::OpCode::SH_LT_U ? TC13::OpCode::SH_GE_U :
                          oc == TC13::OpCode::XOR_GE_U ?
                            TC13::OpCode::XOR_LT_U : TC13::OpCode::XOR_GE_U,
          o.getOperationFormat() == TC13::OperationFormat::DDD_1 ?
            TC13::OperationFormat::DDC9_2 : TC13::OperationFormat::DDC9_4,
          p1, p2, p3 } } );

    checkSelfEdges( res.back().begin()->get(), o );
    copyContainers( res.back().begin()->get(), o );
  }

  return( res );
};


/*
  prop_xxDD_xxDC9 propagates constants for a TriCore operation of format DDDD or
  EEDD.

  If the given up value does not fit into a 9 bits signed constant,
  prop_xxDD_xxDC9 returns an empty instruction list.
*/
list<WIR_Instruction> TC_ConstProp::prop_xxDD_xxDC9( const WIR_Operation &o,
                                                     const WIR_UpDownValue &v )
{
  DSTART(
    "list<WIR_Instruction> TC_ConstProp::prop_xxDD_xxDC9(const WIR_Operation&, const WIR_UpDownValue&)" );

  list<WIR_Instruction> res;

  auto c0 = replace( v, WIR_L4::b0, true );
  auto c1 = replace( v, WIR_L4::b1, true );

  // Determine whether c0 or c1 are 9 bits signed constants.
  bool c0fits =
    ( c0 >= TC_Const16_Signed::getMinValue( 9 ) ) &&
    ( c0 <= TC_Const16_Signed::getMaxValue( 9 ) );
  bool c1fits =
    ( c1 >= TC_Const16_Signed::getMinValue( 9 ) ) &&
    ( c1 <= TC_Const16_Signed::getMaxValue( 9 ) );

  if ( c0fits || c1fits ) {
    // Do the actual constant propagation of the last register parameter of
    // format DDDD/EEDD to DDDC9_1/EEDC9_1, resp.
    auto it = o.getExplicitParameters().begin();
    auto &p1o = it->get();

    auto *p1 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( (it++)->get() ) );
    auto &p2o = it->get();
    auto *p2 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( (it++)->get() ) );
    auto &p3o = it->get();
    auto *p3 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( it->get() ) );
    auto *p4 = new TC_Const9_Signed( c0fits ? c0 : c1 );

    // Patch incoming bit-values for all uses of p1.
    patchDefinedParameter( *p1, p1o );

    // Patch outgoing bit-values for all definitions of p2.
    patchUsedParameter( *p2, p2o );

    // Patch outgoing bit-values for all definitions of p3.
    patchUsedParameter( *p3, p3o );

    // Add bit-value container for new immediate parameter p4.
    patchImmediateParameter( *p4 );

    res.push_back(
      { { o.getOpCode(),
          o.getOperationFormat() == TC13::OperationFormat::DDDD ?
            TC13::OperationFormat::DDDC9_1 : TC13::OperationFormat::EEDC9_1,
          p1, p2, p3, p4 } } );

    checkSelfEdges( res.back().begin()->get(), o );
    copyContainers( res.back().begin()->get(), o );
  }

  return( res );
};


/*
  prop_xxDD_xxDC9_2 propagates constants for a comutative TriCore operation of
  format DDDD or EEDD.

  If the given up value does not fit into a 9 bits signed constant,
  prop_xxDD_xxDC9 returns an empty instruction list.
*/
list<WIR_Instruction> TC_ConstProp::prop_xxDD_xxDC9_2( const WIR_Operation &o,
                                                       const WIR_UpDownValue &v )
{
  DSTART(
    "list<WIR_Instruction> TC_ConstProp::prop_xxDD_xxDC9_2(const WIR_Operation&, const WIR_UpDownValue&)" );

  list<WIR_Instruction> res;

  auto c0 = replace( v, WIR_L4::b0, true );
  auto c1 = replace( v, WIR_L4::b1, true );

  // Determine whether c0 or c1 are 9 bits signed constants.
  bool c0fits =
    ( c0 >= TC_Const16_Signed::getMinValue( 9 ) ) &&
    ( c0 <= TC_Const16_Signed::getMaxValue( 9 ) );
  bool c1fits =
    ( c1 >= TC_Const16_Signed::getMinValue( 9 ) ) &&
    ( c1 <= TC_Const16_Signed::getMaxValue( 9 ) );

  if ( c0fits || c1fits ) {
    // Do the actual constant propagation of the third register parameter of
    // format DDDD/EEDD to DDDC9_1/EEDC9_1, resp.
    auto it = o.getExplicitParameters().begin();
    auto &p1o = it->get();

    auto *p1 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( (it++)->get() ) );
    auto &p2o = it->get();
    auto *p2 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( (it++)->get() ) );
    ++it;
    auto *p3 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( it->get() ) );
    auto &p4o = it->get();
    auto *p4 = new TC_Const9_Signed( c0fits ? c0 : c1 );

    // Patch incoming bit-values for all uses of p1.
    patchDefinedParameter( *p1, p1o );

    // Patch outgoing bit-values for all definitions of p2.
    patchUsedParameter( *p2, p2o );

    // Patch outgoing bit-values for all definitions of p3.
    patchUsedParameter( *p3, p4o );

    // Add bit-value container for new immediate parameter p4.
    patchImmediateParameter( *p4 );

    auto &oc = o.getOpCode();
    res.push_back(
      { { oc == TC13::OpCode::SEL ? TC13::OpCode::SELN :
            oc == TC13::OpCode::SELN ? TC13::OpCode::SEL : oc,
          o.getOperationFormat() == TC13::OperationFormat::DDDD ?
            TC13::OperationFormat::DDDC9_1 : TC13::OperationFormat::EEDC9_1,
          p1, p2, p3, p4 } } );

    checkSelfEdges( res.back().begin()->get(), o );
    copyContainers( res.back().begin()->get(), o );
  }

  return( res );
};


/*
  prop_CSUB propagates constants for TriCore CSUB/CSUBN operation of format
  DDDD.

  For CSUB* Dc, Dd, Da, "const Db", prop_CSUB returns
  CADD* Dc, Dd, Da, -1*"const Db".
  If the inverse of the given up value does not fit into a 9 bits signed
  constant, prop_CSUB returns an empty instruction list.
*/
list<WIR_Instruction> TC_ConstProp::prop_CSUB( const WIR_Operation &o,
                                               const WIR_UpDownValue &v )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  list<WIR_Instruction> res;

  auto c0 = -1 * replace( v, WIR_L4::b0, true );
  auto c1 = -1 * replace( v, WIR_L4::b1, true );

  // Determine whether c0 or c1 are 9 bits signed constants.
  bool c0fits =
    ( c0 >= TC_Const16_Signed::getMinValue( 9 ) ) &&
    ( c0 <= TC_Const16_Signed::getMaxValue( 9 ) );
  bool c1fits =
    ( c1 >= TC_Const16_Signed::getMinValue( 9 ) ) &&
    ( c1 <= TC_Const16_Signed::getMaxValue( 9 ) );

  if ( c0fits || c1fits ) {
    // Do the actual constant propagation of the last register parameter of
    // format DDDD to DDDC9_1.
    auto it = o.getExplicitParameters().begin();
    auto &p1o = it->get();

    auto *p1 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( (it++)->get() ) );
    auto &p2o = it->get();
    auto *p2 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( (it++)->get() ) );
    auto &p3o = it->get();
    auto *p3 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( it->get() ) );
    auto *p4 = new TC_Const9_Signed( c0fits ? c0 : c1 );

    // Patch incoming bit-values for all uses of p1.
    patchDefinedParameter( *p1, p1o );

    // Patch outgoing bit-values for all definitions of p2.
    patchUsedParameter( *p2, p2o );

    // Patch outgoing bit-values for all definitions of p3.
    patchUsedParameter( *p3, p3o );

    // Add bit-value container for new immediate parameter p4.
    patchImmediateParameter( *p4 );

    res.push_back(
      { { o.getOpCode() == TC13::OpCode::CSUB ?
            TC13::OpCode::CADD : TC13::OpCode::CADDN,
          TC13::OperationFormat::DDDC9_1, p1, p2, p3, p4 } } );

    checkSelfEdges( res.back().begin()->get(), o );
    copyContainers( res.back().begin()->get(), o );
  }

  return( res );
};


/*
  prop_xxDD_xxDC9_u propagates constants for an unsigned TriCore operation of
  format DDDD or EEDD.

  If the given up value does not fit into a 9 bits unsigned constant,
  prop_xxDD_xxDC9_u returns an empty instruction list.
*/
list<WIR_Instruction> TC_ConstProp::prop_xxDD_xxDC9_u( const WIR_Operation &o,
                                                       const WIR_UpDownValue &v )
{
  DSTART(
    "list<WIR_Instruction> TC_ConstProp::prop_xxDD_xxDC9_u(const WIR_Operation&, const WIR_UpDownValue&)" );

  list<WIR_Instruction> res;

  auto c0 = replace( v, WIR_L4::b0, false );

  // Determine whether c0 is a 9 bits unsigned constant.
  bool c0fits =
    ( c0 >= 0 ) && ( c0 <= (long long) TC_Const16_Unsigned::getMaxValue( 9 ) );

  if ( c0fits ) {
    // Do the actual constant propagation of the last register parameter of
    // format DDDD/EEDD to DDDC9_2/EEDC9_2, resp.
    auto it = o.getExplicitParameters().begin();
    auto &p1o = it->get();

    auto *p1 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( (it++)->get() ) );
    auto &p2o = it->get();
    auto *p2 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( (it++)->get() ) );
    auto &p3o = it->get();
    auto *p3 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( it->get() ) );
    auto *p4 = new TC_Const9_Unsigned( c0 );

    // Patch incoming bit-values for all uses of p1.
    patchDefinedParameter( *p1, p1o );

    // Patch outgoing bit-values for all definitions of p2.
    patchUsedParameter( *p2, p2o );

    // Patch outgoing bit-values for all definitions of p3.
    patchUsedParameter( *p3, p3o );

    // Add bit-value container for new immediate parameter p4.
    patchImmediateParameter( *p4 );

    res.push_back(
      { { o.getOpCode(),
          o.getOperationFormat() == TC13::OperationFormat::DDDD ?
            TC13::OperationFormat::DDDC9_2 : TC13::OperationFormat::EEDC9_2,
          p1, p2, p3, p4 } } );

    checkSelfEdges( res.back().begin()->get(), o );
    copyContainers( res.back().begin()->get(), o );
  }

  return( res );
};


/*
  prop_xxDD_xxDC9_2u propagates constants for a comutative unsigned TriCore
  operation of format DDDD or EEDD.

  If the given up value does not fit into a 9 bits signed constant,
  prop_xxDD_xxDC9_2u returns an empty instruction list.
*/
list<WIR_Instruction> TC_ConstProp::prop_xxDD_xxDC9_2u( const WIR_Operation &o,
                                                        const WIR_UpDownValue &v )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  list<WIR_Instruction> res;

  auto c0 = replace( v, WIR_L4::b0, false );

  // Determine whether c0 is a 9 bits unsigned constant.
  bool c0fits =
    ( c0 >= 0 ) && ( c0 <= (long long) TC_Const16_Unsigned::getMaxValue( 9 ) );

  if ( c0fits ) {
    // Do the actual constant propagation of the third register parameter of
    // format DDDD/EEDD to DDDC9_2/EEDC9_2, resp.
    auto it = o.getExplicitParameters().begin();
    auto &p1o = it->get();

    auto *p1 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( (it++)->get() ) );
    auto &p2o = it->get();
    auto *p2 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( (it++)->get() ) );
    ++it;
    auto *p3 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( it->get() ) );
    auto &p4o = it->get();
    auto *p4 = new TC_Const9_Unsigned( c0 );

    // Patch incoming bit-values for all uses of p1.
    patchDefinedParameter( *p1, p1o );

    // Patch outgoing bit-values for all definitions of p2.
    patchUsedParameter( *p2, p2o );

    // Patch outgoing bit-values for all definitions of p3.
    patchUsedParameter( *p3, p4o );

    // Add bit-value container for new immediate parameter p4.
    patchImmediateParameter( *p4 );

    res.push_back(
      { { o.getOpCode(),
          o.getOperationFormat() == TC13::OperationFormat::DDDD ?
            TC13::OperationFormat::DDDC9_2 : TC13::OperationFormat::EEDC9_2,
          p1, p2, p3, p4 } } );

    checkSelfEdges( res.back().begin()->get(), o );
    copyContainers( res.back().begin()->get(), o );
  }

  return( res );
};


/*
  prop_Sxx_SxC4 propagates constants for a TriCore operation of format SAA_5 or
  SDD_2.

  If the given up value does not fit into a 4 bits signed constant,
  prop_Sxx_SxC4 returns an empty instruction list.
*/
list<WIR_Instruction> TC_ConstProp::prop_Sxx_SxC4( const WIR_Operation &o,
                                                   const WIR_UpDownValue &v )
{
  DSTART(
    "list<WIR_Instruction> TC_ConstProp::prop_Sxx_SxC4(const WIR_Operation&, const WIR_UpDownValue&)" );

  list<WIR_Instruction> res;

  auto c0 = replace( v, WIR_L4::b0, true );
  auto c1 = replace( v, WIR_L4::b1, true );

  // Determine whether c0 or c1 are 4 bits signed constants.
  bool c0fits =
    ( c0 >= TC_Const16_Signed::getMinValue( 4 ) ) &&
    ( c0 <= TC_Const16_Signed::getMaxValue( 4 ) );
  bool c1fits =
    ( c1 >= TC_Const16_Signed::getMinValue( 4 ) ) &&
    ( c1 <= TC_Const16_Signed::getMaxValue( 4 ) );

  if ( c0fits || c1fits ) {
    // Do the actual constant propagation of the last register parameter of
    // format SAA_5/SDD_2 to SAC4_2/SDC4_2, resp.
    auto it = o.getExplicitParameters().begin();
    auto &p1o = it->get();

    auto *p1 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( it->get() ) );
    auto *p2 = new TC_Const4_Signed( c0fits ? c0 : c1 );

    // Patch incoming bit-values for all uses of p1.
    patchDefinedParameter( *p1, p1o );

    // Patch outgoing bit-values for all definitions of p1.
    patchUsedParameter( *p1, p1o );

    // Add bit-value container for new immediate parameter p2.
    patchImmediateParameter( *p2 );

    res.push_back(
      { { o.getOpCode(),
          o.getOperationFormat() == TC13::OperationFormat::SAA_5 ?
            TC13::OperationFormat::SAC4_2 : TC13::OperationFormat::SDC4_2,
          p1, p2 } } );

    checkSelfEdges( res.back().begin()->get(), o );
    copyContainers( res.back().begin()->get(), o );
  }

  return( res );
};


/*
  prop_SDD_DDC9 propagates constants for a TriCore operation of format SDD_2.

  If the given up value does not fit into a 9 bits signed constant,
  prop_SDD_DDC9 returns an empty instruction list.
*/
list<WIR_Instruction> TC_ConstProp::prop_SDD_DDC9( const WIR_Operation &o,
                                                   const WIR_UpDownValue &v )
{
  DSTART(
    "list<WIR_Instruction> TC_ConstProp::prop_SDD_DDC9(const WIR_Operation&, const WIR_UpDownValue&)" );

  list<WIR_Instruction> res;

  auto c0 = replace( v, WIR_L4::b0, true );
  auto c1 = replace( v, WIR_L4::b1, true );

  // Determine whether c0 or c1 are 9 bits signed constants.
  bool c0fits =
    ( c0 >= TC_Const16_Signed::getMinValue( 9 ) ) &&
    ( c0 <= TC_Const16_Signed::getMaxValue( 9 ) );
  bool c1fits =
    ( c1 >= TC_Const16_Signed::getMinValue( 9 ) ) &&
    ( c1 <= TC_Const16_Signed::getMaxValue( 9 ) );

  if ( c0fits || c1fits ) {
    // Do the actual constant propagation of the last register parameter of
    // format SDD_2 to DDC9_1.
    auto it = o.getExplicitParameters().begin();
    auto &p1o = it->get();

    auto *p1 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( it->get() ) );
    p1->setUsage( WIR_Usage::def );
    p1->getContainers<WIR_BitValues>().begin()->get().clearInValues();

    auto *p2 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( it->get() ) );
    p2->setUsage( WIR_Usage::use );
    p2->getContainers<WIR_BitValues>().begin()->get().clearOutValues();

    auto *p3 = new TC_Const9_Signed( c0fits ? c0 : c1 );

    // Patch incoming bit-values for all uses of p1.
    patchDefinedParameter( *p1, p1o );

    // Patch outgoing bit-values for all definitions of p1.
    patchUsedParameter( *p2, p1o );

    // Add bit-value container for new immediate parameter p3.
    patchImmediateParameter( *p3 );

    res.push_back(
      { { o.getOpCode(), TC13::OperationFormat::DDC9_1, p1, p2, p3 } } );

    checkSelfEdges( res.back().begin()->get(), o );
    copyContainers( res.back().begin()->get(), o );
  }

  return( res );
};


/*
  prop_SDD_DDC9_2 propagates constants for a commutative TriCore operation of
  format SDD_2.

  If the given up value does not fit into a 9 bits signed constant,
  prop_SDD_DDC9_2 returns an empty instruction list.
*/
list<WIR_Instruction> TC_ConstProp::prop_SDD_DDC9_2( const WIR_Operation &o,
                                                     const WIR_UpDownValue &v )
{
  DSTART(
    "list<WIR_Instruction> TC_ConstProp::prop_SDD_DDC9_2(const WIR_Operation&, const WIR_UpDownValue&)" );

  list<WIR_Instruction> res;

  auto c0 = replace( v, WIR_L4::b0, true );
  auto c1 = replace( v, WIR_L4::b1, true );

  // Determine whether c0 or c1 are 9 bits signed constants.
  bool c0fits =
    ( c0 >= TC_Const16_Signed::getMinValue( 9 ) ) &&
    ( c0 <= TC_Const16_Signed::getMaxValue( 9 ) );
  bool c1fits =
    ( c1 >= TC_Const16_Signed::getMinValue( 9 ) ) &&
    ( c1 <= TC_Const16_Signed::getMaxValue( 9 ) );

  if ( c0fits || c1fits ) {
    // Do the actual constant propagation of the last register parameter of
    // format SDD_2 to DDC9_1.
    auto it = o.getExplicitParameters().begin();
    auto &p1o = it->get();

    auto *p1 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( (it++)->get() ) );
    p1->setUsage( WIR_Usage::def );
    p1->getContainers<WIR_BitValues>().begin()->get().clearInValues();

    auto *p2 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( it->get() ) );
    auto &p2o = it->get();

    auto *p3 = new TC_Const9_Signed( c0fits ? c0 : c1 );

    // Patch incoming bit-values for all uses of p1.
    patchDefinedParameter( *p1, p1o );

    // Patch outgoing bit-values for all definitions of p2.
    patchUsedParameter( *p2, p2o );

    // Add bit-value container for new immediate parameter p3.
    patchImmediateParameter( *p3 );

    res.push_back(
      { { o.getOpCode(), TC13::OperationFormat::DDC9_1, p1, p2, p3 } } );

    checkSelfEdges( res.back().begin()->get(), o );
    copyContainers( res.back().begin()->get(), o );
  }

  return( res );
};


/*
  prop_SDD_DDC9_3 propagates constants for a TriCore SUB/SUBS operation of
  format SDD_2.

  For SUB* Da, "const Db", prop_SDD_DDC9_3 returns ADD* Da, Da, -1*"const Db".
  If the inverse of the given up value does not fit into a 9 bits signed
  constant, prop_SDD_DDC9_3 returns an empty instruction list.
*/
list<WIR_Instruction> TC_ConstProp::prop_SDD_DDC9_3( const WIR_Operation &o,
                                                     const WIR_UpDownValue &v )
{
  DSTART(
    "list<WIR_Instruction> TC_ConstProp::prop_SDD_DDC9_3(const WIR_Operation&, const WIR_UpDownValue&)" );

  list<WIR_Instruction> res;

  auto c0 = -1 * replace( v, WIR_L4::b0, true );
  auto c1 = -1 * replace( v, WIR_L4::b1, true );

  // Determine whether c0 or c1 are 9 bits signed constants.
  bool c0fits =
    ( c0 >= TC_Const16_Signed::getMinValue( 9 ) ) &&
    ( c0 <= TC_Const16_Signed::getMaxValue( 9 ) );
  bool c1fits =
    ( c1 >= TC_Const16_Signed::getMinValue( 9 ) ) &&
    ( c1 <= TC_Const16_Signed::getMaxValue( 9 ) );

  if ( c0fits || c1fits ) {
    // Do the actual constant propagation of the last register parameter of
    // format SDD_2 to DDC9_1.
    auto it = o.getExplicitParameters().begin();
    auto &p1o = it->get();

    auto *p1 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( it->get() ) );
    p1->setUsage( WIR_Usage::def );
    p1->getContainers<WIR_BitValues>().begin()->get().clearInValues();

    auto *p2 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( it->get() ) );
    p2->setUsage( WIR_Usage::use );
    p2->getContainers<WIR_BitValues>().begin()->get().clearOutValues();

    auto *p3 = new TC_Const9_Signed( c0fits ? c0 : c1 );

    // Patch incoming bit-values for all uses of p1.
    patchDefinedParameter( *p1, p1o );

    // Patch outgoing bit-values for all definitions of p1.
    patchUsedParameter( *p2, p1o );

    // Add bit-value container for new immediate parameter p3.
    patchImmediateParameter( *p3 );

    res.push_back(
      { { o.getOpCode() == TC13::OpCode::SUB ?
            TC13::OpCode::ADD : TC13::OpCode::ADDS,
          TC13::OperationFormat::DDC9_1, p1, p2, p3 } } );

    checkSelfEdges( res.back().begin()->get(), o );
    copyContainers( res.back().begin()->get(), o );
  }

  return( res );
};


/*
  prop_SDD_DDC9_4 propagates constants for commutative TriCore SUB/SUBS
  operation of format SDD_2.

  For SUB* "const Da", Db, prop_SDD_DDC9_4 returns RSUB* Da, Db, "const Da".
  If the given up value does not fit into a 9 bits signed constant,
  prop_SDD_DDC9_4 returns an empty instruction list.
*/
list<WIR_Instruction> TC_ConstProp::prop_SDD_DDC9_4( const WIR_Operation &o,
                                                     const WIR_UpDownValue &v )
{
  DSTART(
    "list<WIR_Instruction> TC_ConstProp::prop_SDD_DDC9_4(const WIR_Operation&, const WIR_UpDownValue&)" );

  list<WIR_Instruction> res;

  auto c0 = replace( v, WIR_L4::b0, true );
  auto c1 = replace( v, WIR_L4::b1, true );

  // Determine whether c0 or c1 are 9 bits signed constants.
  bool c0fits =
    ( c0 >= TC_Const16_Signed::getMinValue( 9 ) ) &&
    ( c0 <= TC_Const16_Signed::getMaxValue( 9 ) );
  bool c1fits =
    ( c1 >= TC_Const16_Signed::getMinValue( 9 ) ) &&
    ( c1 <= TC_Const16_Signed::getMaxValue( 9 ) );

  if ( c0fits || c1fits ) {
    // Do the actual constant propagation of the first register parameter of
    // format SDD_2 to DDC9_1.
    auto it = o.getExplicitParameters().begin();
    auto &p1o = it->get();

    auto *p1 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( it->get() ) );
    p1->setUsage( WIR_Usage::def );
    p1->getContainers<WIR_BitValues>().begin()->get().clearInValues();

    auto *p2 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( it->get() ) );
    p2->setUsage( WIR_Usage::use );
    p2->getContainers<WIR_BitValues>().begin()->get().clearOutValues();

    auto *p3 = new TC_Const9_Signed( c0fits ? c0 : c1 );

    // Patch incoming bit-values for all uses of p1.
    patchDefinedParameter( *p1, p1o );

    // Patch outgoing bit-values for all definitions of p1.
    patchUsedParameter( *p2, p1o );

    // Add bit-value container for new immediate parameter p3.
    patchImmediateParameter( *p3 );

    res.push_back(
      { { o.getOpCode() == TC13::OpCode::SUB ?
            TC13::OpCode::RSUB : TC13::OpCode::RSUBS,
          TC13::OperationFormat::DDC9_1, p1, p2, p3 } } );

    checkSelfEdges( res.back().begin()->get(), o );
    copyContainers( res.back().begin()->get(), o );
  }

  return( res );
};


/*
  prop_SDD_SIC8_2 propagates constants for TriCore operations AND or OR into
  format SIC8_2.

  If the given up value does not fit into an 8 bits unsigned constant,
  prop_SDD_SIC8_2 returns an empty instruction list.
*/
list<WIR_Instruction> TC_ConstProp::prop_SDD_SIC8_2( const WIR_Operation &o,
                                                     const WIR_UpDownValue &v )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  list<WIR_Instruction> res;

  auto c0 = replace( v, WIR_L4::b0, false );

  // Determine whether c0 is an 8 bits unsigned constant.
  bool c0fits =
    ( c0 >= 0 ) && ( c0 <= (long long) TC_Const16_Unsigned::getMaxValue( 8 ) );

  if ( c0fits ) {
    // Do the actual constant propagation of the last register parameter to
    // SIC8_2.
    auto it = o.getExplicitParameters().begin();
    auto &p1o = it->get();

    auto *p1 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( (it++)->get() ) );

    auto *p2 = new TC_Const8_Unsigned( c0 );

    // Patch incoming bit-values for all uses of p1.
    patchDefinedParameter( *p1, p1o );

    // Patch outgoing bit-values for all definitions of p1.
    patchUsedParameter( *p1, p1o );

    // Add bit-value container for new immediate parameter p2.
    patchImmediateParameter( *p2 );

    res.push_back(
      { { o.getOpCode(), TC13::OperationFormat::SIC8_2, p1, p2 } } );

    checkSelfEdges( res.back().begin()->get(), o );
    copyContainers( res.back().begin()->get(), o );
  }

  return( res );
};


/*
  prop_SIxD_SIxC4 propagates constants for a TriCore operation of formats SDID
  or SIDD into formats SDIC4 or SIDC4, resp.

  If the given up value does not fit into a 4 bits signed constant,
  prop_SIxD_SIxC4 returns an empty instruction list.
*/
list<WIR_Instruction> TC_ConstProp::prop_SIxD_SIxC4( const WIR_Operation &o,
                                                     const WIR_UpDownValue &v )
{
  DSTART(
    "list<WIR_Instruction> TC_ConstProp::prop_SIxD_SIxC4(const WIR_Operation&, const WIR_UpDownValue&)" );

  list<WIR_Instruction> res;

  auto c0 = replace( v, WIR_L4::b0, true );
  auto c1 = replace( v, WIR_L4::b1, true );

  // Determine whether c0 or c1 are 4 bits signed constants.
  bool c0fits =
    ( c0 >= TC_Const16_Signed::getMinValue( 4 ) ) &&
    ( c0 <= TC_Const16_Signed::getMaxValue( 4 ) );
  bool c1fits =
    ( c1 >= TC_Const16_Signed::getMinValue( 4 ) ) &&
    ( c1 <= TC_Const16_Signed::getMaxValue( 4 ) );

  if ( c0fits || c1fits ) {
    // Do the actual constant propagation of the last register parameter of
    // format SDID/SIDD to SDIC4/SIDC4.
    auto it = o.getExplicitParameters().begin();
    auto &p1o = it->get();

    auto *p1 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( (it++)->get() ) );

    auto &p2o = it->get();
    auto *p2 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( it->get() ) );

    auto *p3 = new TC_Const4_Signed( c0fits ? c0 : c1 );

    // Patch incoming bit-values for all uses of p1.
    patchDefinedParameter( *p1, p1o );

    if ( p1->isDefUsed() )
      // Patch outgoing bit-values for all definitions of p1, if so.
      patchUsedParameter( *p1, p1o );

    // Patch outgoing bit-values for all definitions of p2.
    patchUsedParameter( *p2, p2o );

    // Add bit-value container for new immediate parameter p3.
    patchImmediateParameter( *p3 );

    res.push_back(
      { { o.getOpCode(),
          o.getOperationFormat() == TC13::OperationFormat::SDID_2 ?
            TC13::OperationFormat::SDIC4_3 :
            o.getOperationFormat() == TC13::OperationFormat::SIDD ?
              TC13::OperationFormat::SIDC4 : TC13::OperationFormat::SDIC4_2,
          p1, p2, p3 } } );

    checkSelfEdges( res.back().begin()->get(), o );
    copyContainers( res.back().begin()->get(), o );
  }

  return( res );
};


/*
  prop_SIxD_SIxC4_2 propagates constants for a commutative TriCore operation of
  format SIDD into format SIDC4.

  If the given up value does not fit into a 4 bits signed constant,
  prop_SIxD_SIxC4_2 returns an empty instruction list.
*/
list<WIR_Instruction> TC_ConstProp::prop_SIxD_SIxC4_2( const WIR_Operation &o,
                                                       const WIR_UpDownValue &v )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  list<WIR_Instruction> res;

  auto c0 = replace( v, WIR_L4::b0, true );
  auto c1 = replace( v, WIR_L4::b1, true );

  // Determine whether c0 or c1 are 4 bits signed constants.
  bool c0fits =
    ( c0 >= TC_Const16_Signed::getMinValue( 4 ) ) &&
    ( c0 <= TC_Const16_Signed::getMaxValue( 4 ) );
  bool c1fits =
    ( c1 >= TC_Const16_Signed::getMinValue( 4 ) ) &&
    ( c1 <= TC_Const16_Signed::getMaxValue( 4 ) );

  if ( c0fits || c1fits ) {
    // Do the actual constant propagation of the second register parameter of
    // format SIDD to SIDC4.
    auto it = o.getExplicitParameters().begin();
    auto &p1o = it->get();

    auto *p1 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( (it++)->get() ) );
    ++it;
    auto &p3o = it->get();
    auto *p2 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( it->get() ) );

    auto *p3 = new TC_Const4_Signed( c0fits ? c0 : c1 );

    // Patch incoming bit-values for all uses of p1.
    patchDefinedParameter( *p1, p1o );

    // Patch outgoing bit-values for all definitions of p2.
    patchUsedParameter( *p2, p3o );

    // Add bit-value container for new immediate parameter p3.
    patchImmediateParameter( *p3 );

    res.push_back(
      { { o.getOpCode(), TC13::OperationFormat::SIDC4, p1, p2, p3 } } );

    checkSelfEdges( res.back().begin()->get(), o );
    copyContainers( res.back().begin()->get(), o );
  }

  return( res );
};


/*
  prop_SIxD_SIDC4 propagates constants for a TriCore SUB operation of formats
  SIDD or SDID_1.

  For SUB D15, Da, "const Db", prop_SIxD_SIDC4 returns
  ADD D15, Da, -1*"const Db".
  If the inverse of the given up value does not fit into a 4 bits signed
  constant, prop_SIxD_SIDC4 returns an empty instruction list.
*/
list<WIR_Instruction> TC_ConstProp::prop_SIxD_SIDC4( const WIR_Operation &o,
                                                     const WIR_UpDownValue &v )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  list<WIR_Instruction> res;

  auto c0 = -1 * replace( v, WIR_L4::b0, true );
  auto c1 = -1 * replace( v, WIR_L4::b1, true );

  // Determine whether c0 or c1 are 9 bits signed constants.
  bool c0fits =
    ( c0 >= TC_Const16_Signed::getMinValue( 4 ) ) &&
    ( c0 <= TC_Const16_Signed::getMaxValue( 4 ) );
  bool c1fits =
    ( c1 >= TC_Const16_Signed::getMinValue( 4 ) ) &&
    ( c1 <= TC_Const16_Signed::getMaxValue( 4 ) );

  if ( c0fits || c1fits ) {
    // Do the actual constant propagation of the last register parameter of
    // formats SIDD/SDID_1 to SIDC4.
    auto it = o.getExplicitParameters().begin();
    auto &p1o = it->get();

    auto *p1 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( (it++)->get() ) );
    auto *p2 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( it->get() ) );
    auto &p2o = it->get();
    auto *p3 = new TC_Const4_Signed( c0fits ? c0 : c1 );

    // Patch incoming bit-values for all uses of p1.
    patchDefinedParameter( *p1, p1o );

    // Patch outgoing bit-values for all definitions of p2.
    patchUsedParameter( *p2, p2o );

    // Add bit-value container for new immediate parameter p3.
    patchImmediateParameter( *p3 );

    res.push_back(
      { { TC13::OpCode::ADD, TC13::OperationFormat::SIDC4, p1, p2, p3 } } );

    checkSelfEdges( res.back().begin()->get(), o );
    copyContainers( res.back().begin()->get(), o );
  }

  return( res );
};


/*
  prop_SDD_DDC9_u propagates constants for an unsigned TriCore operation of
  format SDD_2.

  If the given up value does not fit into a 9 bits unsigned constant,
  prop_SDD_DDC9_u returns an empty instruction list.
*/
list<WIR_Instruction> TC_ConstProp::prop_SDD_DDC9_u( const WIR_Operation &o,
                                                     const WIR_UpDownValue &v )
{
  DSTART(
    "list<WIR_Instruction> TC_ConstProp::prop_SDD_DDC9_u(const WIR_Operation&, const WIR_UpDownValue&)" );

  list<WIR_Instruction> res;

  auto c0 = replace( v, WIR_L4::b0, false );

  // Determine whether c0 is a 9 bits unsigned constant.
  bool c0fits =
    ( c0 >= 0 ) && ( c0 <= (long long) TC_Const16_Unsigned::getMaxValue( 9 ) );

  if ( c0fits ) {
    // Do the actual constant propagation of the last register parameter of
    // format SDD_2 to DDC9_2.
    auto it = o.getExplicitParameters().begin();
    auto &p1o = it->get();

    auto *p1 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( it->get() ) );
    p1->setUsage( WIR_Usage::def );
    p1->getContainers<WIR_BitValues>().begin()->get().clearInValues();

    auto *p2 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( it->get() ) );
    p2->setUsage( WIR_Usage::use );
    p2->getContainers<WIR_BitValues>().begin()->get().clearOutValues();

    auto *p3 = new TC_Const9_Unsigned( c0 );

    // Patch incoming bit-values for all uses of p1.
    patchDefinedParameter( *p1, p1o );

    // Patch outgoing bit-values for all definitions of p1.
    patchUsedParameter( *p2, p1o );

    // Add bit-value container for new immediate parameter p3.
    patchImmediateParameter( *p3 );

    res.push_back(
      { { o.getOpCode(), TC13::OperationFormat::DDC9_2, p1, p2, p3 } } );

    checkSelfEdges( res.back().begin()->get(), o );
    copyContainers( res.back().begin()->get(), o );
  }

  return( res );
};


/*
  prop_SDD_DDC9_2u propagates constants for a commutative unsigned TriCore
  operation of format SDD_2.

  If the given up value does not fit into a 9 bits unsigned constant,
  prop_SDD_DDC9_2u returns an empty instruction list.
*/
list<WIR_Instruction> TC_ConstProp::prop_SDD_DDC9_2u( const WIR_Operation &o,
                                                      const WIR_UpDownValue &v )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  list<WIR_Instruction> res;

  auto c0 = replace( v, WIR_L4::b0, false );

  // Determine whether c0 is a 9 bits unsigned constant.
  bool c0fits =
    ( c0 >= 0 ) && ( c0 <= (long long) TC_Const16_Unsigned::getMaxValue( 9 ) );

  if ( c0fits ) {
    // Do the actual constant propagation of the last register parameter of
    // format SDD_2 to DDC9_2.
    auto it = o.getExplicitParameters().begin();
    auto &p1o = it->get();

    auto *p1 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( (it++)->get() ) );
    p1->setUsage( WIR_Usage::def );
    p1->getContainers<WIR_BitValues>().begin()->get().clearInValues();

    auto *p2 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( it->get() ) );
    auto &p2o = it->get();

    auto *p3 = new TC_Const9_Unsigned( c0 );

    // Patch incoming bit-values for all uses of p1.
    patchDefinedParameter( *p1, p1o );

    // Patch outgoing bit-values for all definitions of p2.
    patchUsedParameter( *p2, p2o );

    // Add bit-value container for new immediate parameter p3.
    patchImmediateParameter( *p3 );

    res.push_back(
      { { o.getOpCode(), TC13::OperationFormat::DDC9_2, p1, p2, p3 } } );

    checkSelfEdges( res.back().begin()->get(), o );
    copyContainers( res.back().begin()->get(), o );
  }

  return( res );
};


/*
  prop_DDL_DC4L propagates constants for a TriCore jump operation of format
  DDL_1 or DDL_2.

  If the given up value does not fit into a 4 bits signed constant,
  prop_DDL_DC4L returns an empty instruction list.
*/
list<WIR_Instruction> TC_ConstProp::prop_DDL_DC4L( const WIR_Operation &o,
                                                   const WIR_UpDownValue &v )
{
  DSTART(
    "list<WIR_Instruction> TC_ConstProp::prop_DDL_DC4L(const WIR_Operation&, const WIR_UpDownValue&)" );

  list<WIR_Instruction> res;

  auto c0 = replace( v, WIR_L4::b0, true );
  auto c1 = replace( v, WIR_L4::b1, true );

  // Determine whether c0 or c1 are 4 bits signed constants.
  bool c0fits =
    ( c0 >= TC_Const16_Signed::getMinValue( 4 ) ) &&
    ( c0 <= TC_Const16_Signed::getMaxValue( 4 ) );
  bool c1fits =
    ( c1 >= TC_Const16_Signed::getMinValue( 4 ) ) &&
    ( c1 <= TC_Const16_Signed::getMaxValue( 4 ) );

  if ( c0fits || c1fits ) {
    // Do the actual constant propagation of the second register parameter of
    // format DDL_1/DDL_2 to DC4L_1/DC4L_3, resp.
    auto it = o.getExplicitParameters().begin();
    auto &p1o = it->get();

    auto *p1 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( (it++)->get() ) );
    ++it;

    auto *p2 = new TC_Const4_Signed( c0fits ? c0 : c1 );
    auto *p3 =
      new WIR_LabelParameter(
        dynamic_cast<const WIR_LabelParameter &>( it->get() ) );

    // Patch outgoing bit-values for all definitions of p1.
    patchUsedParameter( *p1, p1o );

    if ( p1->isDefUsed() )
      // Patch incoming bit-values for all uses of p1, if so.
      patchDefinedParameter( *p1, p1o );

    // Add bit-value container for new immediate parameter p2.
    patchImmediateParameter( *p2 );

    res.push_back(
      { { o.getOpCode(),
          o.getOperationFormat() == TC13::OperationFormat::DDL_1 ?
            TC13::OperationFormat::DC4L_1 : TC13::OperationFormat::DC4L_3,
          p1, p2, p3 } } );

    checkSelfEdges( res.back().begin()->get(), o );
    copyContainers( res.back().begin()->get(), o );
  }

  return( res );
};


/*
  prop_DDL_DC4L_2 propagates constants for a commutative TriCore jump operation
  of format DDL_1.

  If the given up value does not fit into a 4 bits signed constant,
  prop_DDL_DC4L_2 returns an empty instruction list.
*/
list<WIR_Instruction> TC_ConstProp::prop_DDL_DC4L_2( const WIR_Operation &o,
                                                     const WIR_UpDownValue &v )
{
  DSTART(
    "list<WIR_Instruction> TC_ConstProp::prop_DDL_DC4L_2(const WIR_Operation&, const WIR_UpDownValue&)" );

  list<WIR_Instruction> res;

  auto c0 = replace( v, WIR_L4::b0, true );
  auto c1 = replace( v, WIR_L4::b1, true );

  // Determine whether c0 or c1 are 4 bits signed constants.
  bool c0fits =
    ( c0 >= TC_Const16_Signed::getMinValue( 4 ) ) &&
    ( c0 <= TC_Const16_Signed::getMaxValue( 4 ) );
  bool c1fits =
    ( c1 >= TC_Const16_Signed::getMinValue( 4 ) ) &&
    ( c1 <= TC_Const16_Signed::getMaxValue( 4 ) );

  if ( c0fits || c1fits ) {
    // Do the actual constant propagation of the first register parameter of
    // format DDL_1 to DC4L_1.
    auto it = o.getExplicitParameters().begin();
    ++it;

    auto &p2o = it->get();
    auto *p1 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( (it++)->get() ) );

    auto *p2 = new TC_Const4_Signed( c0fits ? c0 : c1 );
    auto *p3 =
      new WIR_LabelParameter(
        dynamic_cast<const WIR_LabelParameter &>( it->get() ) );

    // Patch outgoing bit-values for all definitions of p1.
    patchUsedParameter( *p1, p2o );

    // Add bit-value container for new immediate parameter p2.
    patchImmediateParameter( *p2 );

    res.push_back(
      { { o.getOpCode(), TC13::OperationFormat::DC4L_1, p1, p2, p3 } } );

    checkSelfEdges( res.back().begin()->get(), o );
    copyContainers( res.back().begin()->get(), o );
  }

  return( res );
};


/*
  prop_DDL_DC4L_3 propagates constants for the commutative TriCore operations
  JGE and JLT of format DDL_1.

  For JGE "const Da", Db, disp15 prop_DDL_DC4L_3 returns
  JLT Db, "const Da+1", disp15.
  For JLT "const Da", Db, disp15 prop_DDL_DC4L_3 returns
  JGE Db, "const Da+1", disp15.
  If the given up value plus one does not fit into a 4 bits signed constant,
  prop_DDL_DC4L_3 returns an empty instruction list.
*/
list<WIR_Instruction> TC_ConstProp::prop_DDL_DC4L_3( const WIR_Operation & o,
                                                     const WIR_UpDownValue &v )
{
  DSTART(
    "list<WIR_Instruction> TC_ConstProp::prop_DDL_DC4L_3(const WIR_Operation&, const WIR_UpDownValue&)" );

  list<WIR_Instruction> res;

  auto c0 = replace( v, WIR_L4::b0, true ) + 1;
  auto c1 = replace( v, WIR_L4::b1, true ) + 1;

  // Determine whether c0 or c1 are 4 bits signed constants.
  bool c0fits =
    ( c0 >= TC_Const16_Signed::getMinValue( 4 ) ) &&
    ( c0 <= TC_Const16_Signed::getMaxValue( 4 ) );
  bool c1fits =
    ( c1 >= TC_Const16_Signed::getMinValue( 4 ) ) &&
    ( c1 <= TC_Const16_Signed::getMaxValue( 4 ) );

  if ( c0fits || c1fits ) {
    // Do the actual constant propagation of the first register parameter of
    // format DDL_1 to DC4L_1.
    auto it = o.getExplicitParameters().begin();
    ++it;

    auto &p2o = it->get();
    auto *p1 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( (it++)->get() ) );

    auto *p2 = new TC_Const4_Signed( c0fits ? c0 : c1 );
    auto *p3 =
      new WIR_LabelParameter(
        dynamic_cast<const WIR_LabelParameter &>( it->get() ) );

    // Patch outgoing bit-values for all definitions of p1.
    patchUsedParameter( *p1, p2o );

    // Add bit-value container for new immediate parameter p2.
    patchImmediateParameter( *p2 );

    auto &oc = o.getOpCode();
    res.push_back(
      { { oc == TC13::OpCode::JGE ? TC13::OpCode::JLT : TC13::OpCode::JGE,
          TC13::OperationFormat::DC4L_1, p1, p2, p3 } } );

    checkSelfEdges( res.back().begin()->get(), o );
    copyContainers( res.back().begin()->get(), o );
  }

  return( res );
};


/*
  prop_DDL_DC4L_u propagates constants for an unsigned TriCore jump operation
  of format DDL_1.

  If the given up value does not fit into a 4 bits unsigned constant,
  prop_DDL_DC4L_u returns an empty instruction list.
*/
list<WIR_Instruction> TC_ConstProp::prop_DDL_DC4L_u( const WIR_Operation &o,
                                                     const WIR_UpDownValue &v )
{
  DSTART(
    "list<WIR_Instruction> TC_ConstProp::prop_DDL_DC4L_u(const WIR_Operation&, const WIR_UpDownValue&)" );

  list<WIR_Instruction> res;

  auto c0 = replace( v, WIR_L4::b0, false );

  // Determine whether c0 is a 4 bits unsigned constant.
  bool c0fits =
    ( c0 >= 0 ) && ( c0 <= (long long) TC_Const16_Unsigned::getMaxValue( 4 ) );

  if ( c0fits ) {
    // Do the actual constant propagation of the second register parameter of
    // format DDL_1 to DC4L_2.
    auto it = o.getExplicitParameters().begin();
    auto &p1o = it->get();

    auto *p1 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( (it++)->get() ) );
    ++it;

    auto *p2 = new TC_Const4_Unsigned( c0 );
    auto *p3 =
      new WIR_LabelParameter(
        dynamic_cast<const WIR_LabelParameter &>( it->get() ) );

    // Patch outgoing bit-values for all definitions of p1.
    patchUsedParameter( *p1, p1o );

    // Add bit-value container for new immediate parameter p2.
    patchImmediateParameter( *p2 );

    res.push_back(
      { { o.getOpCode(), TC13::OperationFormat::DC4L_2, p1, p2, p3 } } );

    checkSelfEdges( res.back().begin()->get(), o );
    copyContainers( res.back().begin()->get(), o );
  }

  return( res );
};


/*
  prop_DDL_DC4L_2u propagates constants for the commutative unsigned TriCore
  operations JGE.U and JLT.U of format DDL_1.

  For JGE.U "const Da", Db, disp15 prop_DDL_DC4L_2u returns
  JLT.U Db, "const Da+1", disp15.
  For JLT.U "const Da", Db, disp15 prop_DDL_DC4L_2u returns
  JGE.U Db, "const Da+1", disp15.
  If the given up value plus one does not fit into a 4 bits unsigned constant,
  prop_DDL_DC4L_2u returns an empty instruction list.
*/
list<WIR_Instruction> TC_ConstProp::prop_DDL_DC4L_2u( const WIR_Operation &o,
                                                      const WIR_UpDownValue &v )
{
  DSTART(
    "list<WIR_Instruction> TC_ConstProp::prop_DDL_DC4L_2u(const WIR_Operation&, const WIR_UpDownValue&)" );

  list<WIR_Instruction> res;

  auto c0 = replace( v, WIR_L4::b0, false ) + 1;

  // Determine whether c0 is a 4 bits unsigned constant.
  bool c0fits =
    ( c0 >= 0 ) && ( c0 <= (long long) TC_Const16_Unsigned::getMaxValue( 4 ) );

  if ( c0fits ) {
    // Do the actual constant propagation of the first register parameter of
    // format DDL_1 to DC4L_2.
    auto it = o.getExplicitParameters().begin();
    ++it;

    auto &p2o = it->get();
    auto *p1 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( (it++)->get() ) );

    auto *p2 = new TC_Const4_Unsigned( c0 );
    auto *p3 =
      new WIR_LabelParameter(
        dynamic_cast<const WIR_LabelParameter &>( it->get() ) );

    // Patch outgoing bit-values for all definitions of p1.
    patchUsedParameter( *p1, p2o );

    // Add bit-value container for new immediate parameter p2.
    patchImmediateParameter( *p2 );

    auto &oc = o.getOpCode();
    res.push_back(
      { { oc == TC13::OpCode::JGE_U ? TC13::OpCode::JLT_U : TC13::OpCode::JGE_U,
          TC13::OperationFormat::DC4L_2, p1, p2, p3 } } );

    checkSelfEdges( res.back().begin()->get(), o );
    copyContainers( res.back().begin()->get(), o );
  }

  return( res );
};


/*
  prop_SIDL_SIC4L propagates constants for a TriCore jump operation of format
  SIDL.

  If the given up value does not fit into a 4 bits signed constant,
  prop_SIDL_SIC4L returns an empty instruction list.
*/
list<WIR_Instruction> TC_ConstProp::prop_SIDL_SIC4L( const WIR_Operation &o,
                                                     const WIR_UpDownValue &v )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  list<WIR_Instruction> res;

  auto c0 = replace( v, WIR_L4::b0, true );
  auto c1 = replace( v, WIR_L4::b1, true );

  // Determine whether c0 or c1 are 4 bits signed constants.
  bool c0fits =
    ( c0 >= TC_Const16_Signed::getMinValue( 4 ) ) &&
    ( c0 <= TC_Const16_Signed::getMaxValue( 4 ) );
  bool c1fits =
    ( c1 >= TC_Const16_Signed::getMinValue( 4 ) ) &&
    ( c1 <= TC_Const16_Signed::getMaxValue( 4 ) );

  if ( c0fits || c1fits ) {
    // Do the actual constant propagation of the second register parameter of
    // format SIDL to SIC4L.
    auto it = o.getExplicitParameters().begin();
    auto &p1o = it->get();

    auto *p1 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( (it++)->get() ) );
    ++it;

    auto *p2 = new TC_Const4_Signed( c0fits ? c0 : c1 );
    auto *p3 =
      new WIR_LabelParameter(
        dynamic_cast<const WIR_LabelParameter &>( it->get() ) );

    // Patch outgoing bit-values for all definitions of p1.
    patchUsedParameter( *p1, p1o );

    // Add bit-value container for new immediate parameter p2.
    patchImmediateParameter( *p2 );

    res.push_back(
      { { o.getOpCode(), TC13::OperationFormat::SIC4L, p1, p2, p3 } } );

    checkSelfEdges( res.back().begin()->get(), o );
    copyContainers( res.back().begin()->get(), o );
  }

  return( res );
};


/*
  prop_AD_SAC4 propagates constants for a TriCore MOV.A operation of formats AD
  or SAD_1.

  If the given up value does not fit into a 4 bits unsigned constant,
  prop_AD_SAC4 returns an empty instruction list.
*/
list<WIR_Instruction> TC_ConstProp::prop_AD_SAC4( const WIR_Operation &o,
                                                  const WIR_UpDownValue &v )
{
  DSTART(
    "list<WIR_Instruction> TC_ConstProp::prop_AD_SAC4(const WIR_Operation&, const WIR_UpDownValue&)" );

  list<WIR_Instruction> res;

  auto c0 = replace( v, WIR_L4::b0, false );

  // Determine whether c0 is a 4 bits unsigned constant.
  bool c0fits =
    ( c0 >= 0 ) && ( c0 <= (long long) TC_Const16_Unsigned::getMaxValue( 4 ) );

  if ( c0fits ) {
    // Do the actual constant propagation of the last register parameter of
    // format AD/SAD_1 to SAC4_1.
    auto it = o.getExplicitParameters().begin();

    auto &p1o = it->get();
    auto *p1 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( it->get() ) );

    // Patch incoming bit-values for all uses of p1.
    patchDefinedParameter( *p1, p1o );

    if ( m16BitOperations ) {
      auto *p2 = new TC_Const4_Unsigned( c0 );

      // Add bit-value container for new immediate parameter p2.
      patchImmediateParameter( *p2 );

      res.push_back(
        { { o.getOpCode(), TC13::OperationFormat::SAC4_1, p1, p2 } } );
    } else {
      auto *p2 = new TC_Const18_Unsigned( c0 );

      // Add bit-value container for new immediate parameter p2.
      patchImmediateParameter( *p2 );

      res.push_back(
        { { TC13::OpCode::LEA, TC13::OperationFormat::AC18ABSA, p1, p2 } } );
    }

    checkSelfEdges( res.back().begin()->get(), o );
    copyContainers( res.back().begin()->get(), o );
  }

  return( res );
};


/*
  prop_DD_SDC4 propagates constants for a TriCore MOV operation of formats DD or
  SDD_1.

  If the given up value does not fit into a 4 bits signed constant, prop_DD_SDC4
  returns an empty instruction list.
*/
list<WIR_Instruction> TC_ConstProp::prop_DD_SDC4( const WIR_Operation &o,
                                                  const WIR_UpDownValue &v )
{
  DSTART(
    "list<WIR_Instruction> TC_ConstProp::prop_DD_SDC4(const WIR_Operation&, const WIR_UpDownValue&)" );

  list<WIR_Instruction> res;

  auto c0 = replace( v, WIR_L4::b0, true );
  auto c1 = replace( v, WIR_L4::b1, true );

  // Determine whether c0 or c1 are 4 bits signed constants.
  bool c0fits =
    ( c0 >= TC_Const16_Signed::getMinValue( 4 ) ) &&
    ( c0 <= TC_Const16_Signed::getMaxValue( 4 ) );
  bool c1fits =
    ( c1 >= TC_Const16_Signed::getMinValue( 4 ) ) &&
    ( c1 <= TC_Const16_Signed::getMaxValue( 4 ) );

  if ( c0fits || c1fits ) {
    // Do the actual constant propagation of the last register parameter of
    // format DD/SDD_1 to SDC4_1.
    auto it = o.getExplicitParameters().begin();

    auto &p1o = it->get();
    auto *p1 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( it->get() ) );

    // Patch incoming bit-values for all uses of p1.
    patchDefinedParameter( *p1, p1o );

    if ( m16BitOperations ) {
      auto *p2 = new TC_Const4_Signed( c0fits ? c0 : c1 );

      // Add bit-value container for new immediate parameter p2.
      patchImmediateParameter( *p2 );

      res.push_back(
        { { TC13::OpCode::MOV, TC13::OperationFormat::SDC4_1, p1, p2 } } );
    } else {
      auto *p2 = new TC_Const16_Signed( c0fits ? c0 : c1 );

      // Add bit-value container for new immediate parameter p2.
      patchImmediateParameter( *p2 );

      res.push_back(
        { { TC13::OpCode::MOV, TC13::OperationFormat::DC16_1, p1, p2 } } );
    }

    checkSelfEdges( res.back().begin()->get(), o );
    copyContainers( res.back().begin()->get(), o );
  }

  return( res );
};


/*
  prop_DD_SIC8 propagates constants for a TriCore MOV operation of formats DD or
  SDD_1 whose first register parameter is D15.

  If the given up value does not fit into an 8 bits unsigned constant,
  prop_DD_SIC8 returns an empty instruction list.
*/
list<WIR_Instruction> TC_ConstProp::prop_DD_SIC8( const WIR_Operation &o,
                                                  const WIR_UpDownValue &v )
{
  DSTART(
    "list<WIR_Instruction> TC_ConstProp::prop_DD_SIC8(const WIR_Operation&, const WIR_UpDownValue&)" );

  list<WIR_Instruction> res;

  auto c0 = replace( v, WIR_L4::b0, false );

  // Determine whether c0 is an 8 bits unsigned constant.
  bool c0fits =
    ( c0 >= 0 ) && ( c0 <= (long long) TC_Const16_Unsigned::getMaxValue( 8 ) );

  if ( c0fits ) {
    // Do the actual constant propagation of the last register parameter of
    // format DD/SDD_1 to SIC8_1.
    auto it = o.getExplicitParameters().begin();

    auto &p1o = it->get();
    auto *p1 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( it->get() ) );

    // Patch incoming bit-values for all uses of p1.
    patchDefinedParameter( *p1, p1o );

    if ( m16BitOperations ) {
      auto *p2 = new TC_Const8_Unsigned( c0 );

      // Add bit-value container for new immediate parameter p2.
      patchImmediateParameter( *p2 );

      res.push_back(
        { { TC13::OpCode::MOV, TC13::OperationFormat::SIC8_1, p1, p2 } } );
    } else {
      auto *p2 = new TC_Const16_Signed( c0 );

      // Add bit-value container for new immediate parameter p2.
      patchImmediateParameter( *p2 );

      res.push_back(
        { { TC13::OpCode::MOV, TC13::OperationFormat::DC16_1, p1, p2 } } );
    }

    checkSelfEdges( res.back().begin()->get(), o );
    copyContainers( res.back().begin()->get(), o );
  }

  return( res );
};


/*
  prop_DD_DC16 propagates constants for a TriCore MOV operation of formats DD or
  SDD_1.

  If the given up value does not fit into a 16 bits signed constant,
  prop_DD_DC16 returns an empty instruction list.
*/
list<WIR_Instruction> TC_ConstProp::prop_DD_DC16( const WIR_Operation &o,
                                                  const WIR_UpDownValue &v )
{
  DSTART(
    "list<WIR_Instruction> TC_ConstProp::prop_DD_DC16(const WIR_Operation&, const WIR_UpDownValue&)" );

  list<WIR_Instruction> res;

  auto c0 = replace( v, WIR_L4::b0, true );
  auto c1 = replace( v, WIR_L4::b1, true );

  // Determine whether c0 or c1 are 16 bits signed constants.
  bool c0fits =
    ( c0 >= TC_Const16_Signed::getMinValue( 16 ) ) &&
    ( c0 <= TC_Const16_Signed::getMaxValue( 16 ) );
  bool c1fits =
    ( c1 >= TC_Const16_Signed::getMinValue( 16 ) ) &&
    ( c1 <= TC_Const16_Signed::getMaxValue( 16 ) );

  if ( c0fits || c1fits ) {
    // Do the actual constant propagation of the last register parameter of
    // format DD/SDD_1 to DC16_1.
    auto it = o.getExplicitParameters().begin();

    auto &p1o = it->get();
    auto *p1 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( it->get() ) );

    auto *p2 = new TC_Const16_Signed( c0fits ? c0 : c1 );

    // Patch incoming bit-values for all uses of p1.
    patchDefinedParameter( *p1, p1o );

    // Add bit-value container for new immediate parameter p2.
    patchImmediateParameter( *p2 );

    res.push_back(
      { { TC13::OpCode::MOV, TC13::OperationFormat::DC16_1, p1, p2 } } );

    checkSelfEdges( res.back().begin()->get(), o );
    copyContainers( res.back().begin()->get(), o );
  }

  return( res );
};


/*
  prop_DD_DC16_1 propagates constants for a TriCore MOV operation of formats DD
  or SDD_1.

  If the given up value does not fit into a 16 bits unsigned constant,
  prop_DD_DC16_1 returns an empty instruction list.
*/
list<WIR_Instruction> TC_ConstProp::prop_DD_DC16_1( const WIR_Operation &o,
                                                    const WIR_UpDownValue &v )
{
  DSTART(
    "list<WIR_Instruction> TC_ConstProp::prop_DD_DC16_1(const WIR_Operation&, const WIR_UpDownValue&)" );

  list<WIR_Instruction> res;

  auto c0 = replace( v, WIR_L4::b0, false );

  // Determine whether c0 is a 16 bits unsigned constant.
  bool c0fits =
    ( c0 >= 0 ) && ( c0 <= (long long) TC_Const16_Unsigned::getMaxValue( 16 ) );

  if ( c0fits ) {
    // Do the actual constant propagation of the last register parameter of
    // format DD/SDD_1 to DC16_2.
    auto it = o.getExplicitParameters().begin();

    auto &p1o = it->get();
    auto *p1 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( it->get() ) );

    auto *p2 = new TC_Const16_Unsigned( c0 );

    // Patch incoming bit-values for all uses of p1.
    patchDefinedParameter( *p1, p1o );

    // Add bit-value container for new immediate parameter p2.
    patchImmediateParameter( *p2 );

    res.push_back(
      { { TC13::OpCode::MOV_U, TC13::OperationFormat::DC16_2, p1, p2 } } );

    checkSelfEdges( res.back().begin()->get(), o );
    copyContainers( res.back().begin()->get(), o );
  }

  return( res );
};


/*
  prop_xx_xC16 propagates constants for a TriCore MOV/MOV.AA operation of
  formats DD/SDD_1/AA/SAA_1.

  If the given up value does not fit into the shape expected by MOVH/MOVH.A,
  prop_xx_xC16 returns an empty instruction list.
*/
list<WIR_Instruction> TC_ConstProp::prop_xx_xC16( const WIR_Operation &o,
                                                  const WIR_UpDownValue &v )
{
  DSTART(
    "list<WIR_Instruction> TC_ConstProp::prop_xx_xC16(const WIR_Operation&, const WIR_UpDownValue&)" );

  list<WIR_Instruction> res;

  auto c0 = replace( v.extract( 16, 16 ), WIR_L4::b0, false );
  auto c1 = replace( v.extract( 0, 16 ), WIR_L4::b0, false );

  // Determine whether c0 is a 16 bits unsigned constant and c1 is 0.
  bool c0fits =
    ( c0 >= 0 ) && ( c0 <= (long long) TC_Const16_Unsigned::getMaxValue( 16 ) );
  bool c1fits = ( c1 == 0 );

  if ( c0fits && c1fits ) {
    // Do the actual constant propagation of the last register parameter of
    // formats DD/SDD_1/AA/SAA_1 to DC16_2/AC16, resp.
    auto it = o.getExplicitParameters().begin();

    auto &p1o = it->get();
    auto *p1 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( it->get() ) );

    auto *p2 = new TC_Const16_Unsigned( c0 );

    // Patch incoming bit-values for all uses of p1.
    patchDefinedParameter( *p1, p1o );

    // Add bit-value container for new immediate parameter p2.
    patchImmediateParameter( *p2 );

    res.push_back(
      { { o.getOpCode() == TC13::OpCode::MOV_RR ?
            TC13::OpCode::MOVH : TC13::OpCode::MOVH_A,
          o.getOpCode() == TC13::OpCode::MOV_RR ?
            TC13::OperationFormat::DC16_2 : TC13::OperationFormat::AC16,
          p1, p2 } } );

    checkSelfEdges( res.back().begin()->get(), o );
    copyContainers( res.back().begin()->get(), o );
  }

  return( res );
};


/*
  prop_DDDC5_DDC5C5 propagates constants for TriCore EXTR* operations of format
  DDDC5.

  If the given up value does not fit into a 5 bits unsigned constant,
  prop_DDDC5_DDC5C5 returns an empty instruction list.
*/
list<WIR_Instruction> TC_ConstProp::prop_DDDC5_DDC5C5( const WIR_Operation &o,
                                                       const WIR_UpDownValue &v )
{
  DSTART(
    "list<WIR_Instruction> TC_ConstProp::prop_DDDC5_DDC5C5(const WIR_Operation&, const WIR_UpDownValue&)" );

  list<WIR_Instruction> res;

  auto c0 = replace( v.extract( 0, 5 ), WIR_L4::b0, false );

  // Determine whether c0 is a 5 bits unsigned constant.
  bool c0fits =
    ( c0 >= 0 ) && ( c0 <= (long long) TC_Const16_Unsigned::getMaxValue( 5 ) );

  if ( c0fits ) {
    // Do the actual constant propagation of the third register parameter of
    // format DDDC5 to DDC5C5.
    auto it = o.getExplicitParameters().begin();

    auto &p1o = it->get();
    auto *p1 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( (it++)->get() ) );

    auto &p2o = it->get();
    auto *p2 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( (it++)->get() ) );

    ++it;
    auto *p3 = new TC_Const5_Unsigned( c0 );

    auto *p4 =
      new TC_Const5_Unsigned(
        dynamic_cast<const TC_Const5_Unsigned &>( it->get() ) );

    // Patch incoming bit-values for all uses of p1.
    patchDefinedParameter( *p1, p1o );

    // Patch outgoing bit-values for all definitions of p2.
    patchUsedParameter( *p2, p2o );

    // Add bit-value container for new immediate parameter p3.
    patchImmediateParameter( *p3 );

    res.push_back(
      { { o.getOpCode(), TC13::OperationFormat::DDC5C5, p1, p2, p3, p4 } } );

    checkSelfEdges( res.back().begin()->get(), o );
    copyContainers( res.back().begin()->get(), o );
  }

  return( res );
};


/*
  prop_DDE_DDC5C5 propagates constants for TriCore EXTR* operations of format
  DDE.

  If bits [36:32] or [4:0] of the given up value do not fit into a 5 bits
  unsigned constant, prop_DDE_DDC5C5 returns an empty instruction list.
*/
list<WIR_Instruction> TC_ConstProp::prop_DDE_DDC5C5( const WIR_Operation &o,
                                                     const WIR_UpDownValue &v )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  list<WIR_Instruction> res;

  auto c0 = replace( v.extract( 0, 5 ), WIR_L4::b0, false );
  auto c1 = replace( v.extract( 32, 5 ), WIR_L4::b0, false );

  // Determine whether c0 and c1 are 5 bits unsigned constants.
  bool c0fits =
    ( c0 >= 0 ) && ( c0 <= (long long) TC_Const16_Unsigned::getMaxValue( 5 ) );
  bool c1fits =
    ( c1 >= 0 ) && ( c1 <= (long long) TC_Const16_Unsigned::getMaxValue( 5 ) );

  if ( c0fits && c1fits ) {
    // Do the actual constant propagation of the last register parameter of
    // format DDE to DDC5C5.
    auto it = o.getExplicitParameters().begin();

    auto &p1o = it->get();
    auto *p1 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( (it++)->get() ) );

    auto &p2o = it->get();
    auto *p2 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( it->get() ) );

    auto *p3 = new TC_Const5_Unsigned( c0 );
    auto *p4 = new TC_Const5_Unsigned( c1 );

    // Patch incoming bit-values for all uses of p1.
    patchDefinedParameter( *p1, p1o );

    // Patch outgoing bit-values for all definitions of p2.
    patchUsedParameter( *p2, p2o );

    // Add bit-value container for new immediate parameter p3.
    patchImmediateParameter( *p3 );

    // Add bit-value container for new immediate parameter p4.
    patchImmediateParameter( *p4 );

    res.push_back(
      { { o.getOpCode(), TC13::OperationFormat::DDC5C5, p1, p2, p3, p4 } } );

    checkSelfEdges( res.back().begin()->get(), o );
    copyContainers( res.back().begin()->get(), o );
  }

  return( res );
};


/*
  prop_DDE_DDC5C5_2 propagates constants for TriCore EXTR* operations of format
  DDE.

  If bits [36:32] of the given up value do not fit into a 5 bits unsigned
  constant, prop_DDE_DDC5C5_2 returns an empty instruction list.
*/
list<WIR_Instruction> TC_ConstProp::prop_DDE_DDC5C5_2( const WIR_Operation &o,
                                                       const WIR_UpDownValue &v )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  list<WIR_Instruction> res;

  auto c0 = replace( v.extract( 32, 5 ), WIR_L4::b0, false );

  // Determine whether c0 is a 5 bits unsigned constant.
  bool c0fits =
    ( c0 >= 0 ) && ( c0 <= (long long) TC_Const16_Unsigned::getMaxValue( 5 ) );

  if ( c0fits ) {
    // Do the actual constant propagation of the last register parameter of
    // format DDE to DDDC5.
    auto it = o.getExplicitParameters().begin();

    auto &p1o = it->get();
    auto *p1 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( (it++)->get() ) );

    auto &p2o = it->get();
    auto *p2 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( (it++)->get() ) );

    auto &p3o = it->get();
    auto &eReg =
      dynamic_cast<const WIR_RegisterParameter &>( it->get() ).getRegister();
    auto *p3 =
      new WIR_RegisterParameter(
        eReg.getLeafs().front().get(), WIR_Usage::use );

    auto *p4 = new TC_Const5_Unsigned( c0 );

    // Patch incoming bit-values for all uses of p1.
    patchDefinedParameter( *p1, p1o );

    // Patch outgoing bit-values for all definitions of p2.
    patchUsedParameter( *p2, p2o );

    // Patch outgoing bit-values for all definitions of p3.
    patchUsedERegParameter( *p3, p3o );

    // Add bit-value container for new immediate parameter p4.
    patchImmediateParameter( *p4 );

    res.push_back(
      { { o.getOpCode(), TC13::OperationFormat::DDDC5, p1, p2, p3, p4 } } );

    checkSelfEdges( res.back().begin()->get(), o );
    copyContainers( res.back().begin()->get(), o );
  }

  return( res );
};


/*
  prop_DDDD_DDDC5 propagates constants for TriCore DEXTR operations of format
  DDDD.

  If the given up value does not fit into a 5 bits unsigned constant,
  prop_DDDD_DDDC5 returns an empty instruction list.
*/
list<WIR_Instruction> TC_ConstProp::prop_DDDD_DDDC5( const WIR_Operation &o,
                                                     const WIR_UpDownValue &v )
{
  DSTART(
    "list<WIR_Instruction> TC_ConstProp::prop_DDDD_DDDC5(const WIR_Operation&, const WIR_UpDownValue&)" );

  list<WIR_Instruction> res;

  auto c0 = replace( v.extract( 0, 5 ), WIR_L4::b0, false );

  // Determine whether c0 is a 5 bits unsigned constant.
  bool c0fits =
    ( c0 >= 0 ) && ( c0 <= (long long) TC_Const16_Unsigned::getMaxValue( 5 ) );

  if ( c0fits ) {
    // Do the actual constant propagation of the last register parameter of
    // format DDDD to DDDC5.
    auto it = o.getExplicitParameters().begin();

    auto &p1o = it->get();
    auto *p1 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( (it++)->get() ) );

    auto &p2o = it->get();
    auto *p2 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( (it++)->get() ) );

    auto &p3o = it->get();
    auto *p3 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( it->get() ) );

    auto *p4 = new TC_Const5_Unsigned( c0 );

    // Patch incoming bit-values for all uses of p1.
    patchDefinedParameter( *p1, p1o );

    // Patch outgoing bit-values for all definitions of p2.
    patchUsedParameter( *p2, p2o );

    // Patch outgoing bit-values for all definitions of p3.
    patchUsedParameter( *p3, p3o );

    // Add bit-value container for new immediate parameter p4.
    patchImmediateParameter( *p4 );

    res.push_back(
      { { o.getOpCode(), TC13::OperationFormat::DDDC5, p1, p2, p3, p4 } } );

    checkSelfEdges( res.back().begin()->get(), o );
    copyContainers( res.back().begin()->get(), o );
  }

  return( res );
};


/*
  prop_DDxDC5_DDxC5C5 propagates constants for TriCore INSERT operations of
  formats DDC4DC5 or DDDDC5, resp.

  If the given up value does not fit into a 5 bits unsigned constant,
  prop_DDxDC5_DDxC5C5 returns an empty instruction list.
*/
list<WIR_Instruction> TC_ConstProp::prop_DDxDC5_DDxC5C5( const WIR_Operation &o,
                                                         const WIR_UpDownValue &v )
{
  DSTART(
    "list<WIR_Instruction> TC_ConstProp::prop_DDxDC5_DDxC5C5(const WIR_Operation&, const WIR_UpDownValue&)" );

  list<WIR_Instruction> res;

  auto c0 = replace( v.extract( 0, 5 ), WIR_L4::b0, false );

  // Determine whether c0 is a 5 bits unsigned constant.
  bool c0fits =
    ( c0 >= 0 ) && ( c0 <= (long long) TC_Const16_Unsigned::getMaxValue( 5 ) );

  if ( c0fits ) {
    // Do the actual constant propagation of the fourth register parameter of
    // formats DDC4DC5/DDDDC5 to DDC4C5C5/DDDC5C5.
    auto it = o.getExplicitParameters().begin();

    auto &p1o = it->get();
    auto *p1 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( (it++)->get() ) );

    auto &p2o = it->get();
    auto *p2 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( (it++)->get() ) );

    auto &p3o = it->get();
    WIR_Parameter *p3 =
      o.getOperationFormat() == TC13::OperationFormat::DDC4DC5 ?
        static_cast<WIR_Parameter *>( new TC_Const4_Unsigned(
          dynamic_cast<const TC_Const4_Unsigned &>( (it++)->get() ) ) ) :
        static_cast<WIR_Parameter *>( new WIR_RegisterParameter(
          dynamic_cast<const WIR_RegisterParameter &>( (it++)->get() ) ) );

    ++it;
    auto *p4 = new TC_Const5_Unsigned( c0 );

    auto *p5 =
      new TC_Const5_Unsigned(
        dynamic_cast<const TC_Const5_Unsigned &>( it->get() ) );

    // Patch incoming bit-values for all uses of p1.
    patchDefinedParameter( *p1, p1o );

    // Patch outgoing bit-values for all definitions of p2.
    patchUsedParameter( *p2, p2o );

    // Patch bit-values for p3.
    if ( o.getOperationFormat() == TC13::OperationFormat::DDC4DC5 )
      patchImmediateParameter(
        dynamic_cast<WIR_BaseImmediateParameter &>( *p3 ) );
    else
      patchUsedParameter( *p3, p3o );

    // Add bit-value container for new immediate parameter p3.
    patchImmediateParameter( *p4 );

    res.push_back(
      { { o.getOpCode(),
          o.getOperationFormat() == TC13::OperationFormat::DDC4DC5 ?
            TC13::OperationFormat::DDC4C5C5 : TC13::OperationFormat::DDDC5C5,
          p1, p2, p3, p4, p5 } } );

    checkSelfEdges( res.back().begin()->get(), o );
    copyContainers( res.back().begin()->get(), o );
  }

  return( res );
};


/*
  prop_DDxE_DDxC5C5 propagates constants for TriCore INSERT operations of
  formats DDC4E or DDDE, resp.

  If bits [36:32] or [4:0] of the given up value do not fit into a 5 bits
  unsigned constant, prop_DDxE_DDxC5C5 returns an empty instruction list.
*/
list<WIR_Instruction> TC_ConstProp::prop_DDxE_DDxC5C5( const WIR_Operation &o,
                                                       const WIR_UpDownValue &v )
{
  DSTART(
    "list<WIR_Instruction> TC_ConstProp::prop_DDxE_DDxC5C5(const WIR_Operation&, const WIR_UpDownValue&)" );

  list<WIR_Instruction> res;

  auto c0 = replace( v.extract( 0, 5 ), WIR_L4::b0, false );
  auto c1 = replace( v.extract( 32, 5 ), WIR_L4::b0, false );

  // Determine whether c0 and c1 are 5 bits unsigned constants.
  bool c0fits =
    ( c0 >= 0 ) && ( c0 <= (long long) TC_Const16_Unsigned::getMaxValue( 5 ) );
  bool c1fits =
    ( c1 >= 0 ) && ( c1 <= (long long) TC_Const16_Unsigned::getMaxValue( 5 ) );

  if ( c0fits && c1fits ) {
    // Do the actual constant propagation of the last register parameter of
    // format DD*E to DD*C5C5.
    auto it = o.getExplicitParameters().begin();

    auto &p1o = it->get();
    auto *p1 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( (it++)->get() ) );

    auto &p2o = it->get();
    auto *p2 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( (it++)->get() ) );

    auto &p3o = it->get();
    WIR_Parameter *p3 =
      o.getOperationFormat() == TC13::OperationFormat::DDC4E ?
        static_cast<WIR_Parameter *>( new TC_Const4_Unsigned(
          dynamic_cast<const TC_Const4_Unsigned &>( it->get() ) ) ) :
        static_cast<WIR_Parameter *>( new WIR_RegisterParameter(
          dynamic_cast<const WIR_RegisterParameter &>( it->get() ) ) );

    auto *p4 = new TC_Const5_Unsigned( c0 );
    auto *p5 = new TC_Const5_Unsigned( c1 );

    // Patch incoming bit-values for all uses of p1.
    patchDefinedParameter( *p1, p1o );

    // Patch outgoing bit-values for all definitions of p2.
    patchUsedParameter( *p2, p2o );

    // Patch bit-values for p3.
    if ( o.getOperationFormat() == TC13::OperationFormat::DDC4E )
      patchImmediateParameter(
        dynamic_cast<WIR_BaseImmediateParameter &>( *p3 ) );
    else
      patchUsedParameter( *p3, p3o );

    // Add bit-value container for new immediate parameter p4.
    patchImmediateParameter( *p4 );

    // Add bit-value container for new immediate parameter p5.
    patchImmediateParameter( *p5 );

    res.push_back(
      { { o.getOpCode(),
          o.getOperationFormat() == TC13::OperationFormat::DDC4E ?
            TC13::OperationFormat::DDC4C5C5 : TC13::OperationFormat::DDDC5C5,
          p1, p2, p3, p4, p5 } } );

    checkSelfEdges( res.back().begin()->get(), o );
    copyContainers( res.back().begin()->get(), o );
  }

  return( res );
};


/*
  prop_DDxE_DDxC5C5_2 propagates constants for TriCore INSERT operations of
  formats DDC4E or DDDE, resp.

  If bits [36:32] of the given up value do not fit into a 5 bits unsigned
  constant, prop_DDxE_DDxC5C5_2 returns an empty instruction list.
*/
list<WIR_Instruction> TC_ConstProp::prop_DDxE_DDxC5C5_2( const WIR_Operation &o,
                                                         const WIR_UpDownValue &v )
{
  DSTART(
    "list<WIR_Instruction> TC_ConstProp::prop_DDxE_DDxC5C5_2(const WIR_Operation&, const WIR_UpDownValue&)" );

  list<WIR_Instruction> res;

  auto c0 = replace( v.extract( 32, 5 ), WIR_L4::b0, false );

  // Determine whether c0 is a 5 bits unsigned constant.
  bool c0fits =
    ( c0 >= 0 ) && ( c0 <= (long long) TC_Const16_Unsigned::getMaxValue( 5 ) );

  if ( c0fits ) {
    // Do the actual constant propagation of the last register parameter of
    // format DD*E to DD*DC5.
    auto it = o.getExplicitParameters().begin();

    auto &p1o = it->get();
    auto *p1 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( (it++)->get() ) );

    auto &p2o = it->get();
    auto *p2 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( (it++)->get() ) );

    auto &p3o = it->get();
    WIR_Parameter *p3 =
      o.getOperationFormat() == TC13::OperationFormat::DDC4E ?
        static_cast<WIR_Parameter *>( new TC_Const4_Unsigned(
          dynamic_cast<const TC_Const4_Unsigned &>( (it++)->get() ) ) ) :
        static_cast<WIR_Parameter *>( new WIR_RegisterParameter(
          dynamic_cast<const WIR_RegisterParameter &>( (it++)->get() ) ) );

    auto &p4o = it->get();
    auto &eReg =
      dynamic_cast<const WIR_RegisterParameter &>( it->get() ).getRegister();
    auto *p4 =
      new WIR_RegisterParameter(
        eReg.getLeafs().front().get(), WIR_Usage::use );

    auto *p5 = new TC_Const5_Unsigned( c0 );

    // Patch incoming bit-values for all uses of p1.
    patchDefinedParameter( *p1, p1o );

    // Patch outgoing bit-values for all definitions of p2.
    patchUsedParameter( *p2, p2o );

    // Patch bit-values for p3.
    if ( o.getOperationFormat() == TC13::OperationFormat::DDC4E )
      patchImmediateParameter(
        dynamic_cast<WIR_BaseImmediateParameter &>( *p3 ) );
    else
      patchUsedParameter( *p3, p3o );

    // Patch outgoing bit-values for all definitions of p4.
    patchUsedERegParameter( *p4, p4o );

    // Add bit-value container for new immediate parameter p5.
    patchImmediateParameter( *p5 );

    res.push_back(
      { { o.getOpCode(),
          o.getOperationFormat() == TC13::OperationFormat::DDC4E ?
            TC13::OperationFormat::DDC4DC5 : TC13::OperationFormat::DDDDC5,
          p1, p2, p3, p4, p5 } } );

    checkSelfEdges( res.back().begin()->get(), o );
    copyContainers( res.back().begin()->get(), o );
  }

  return( res );
};


/*
  prop_EDDC5_EC4C5C5 propagates constants for TriCore IMASK operations of format
  EDDC5.

  If the given up values do not fit into a 4 or 5 bits unsigned constant, resp.,
  prop_EDDC5_EC4C5C5 returns an empty instruction list.
*/
list<WIR_Instruction> TC_ConstProp::prop_EDDC5_EC4C5C5( const WIR_Operation &o,
                                                        const WIR_UpDownValue &v1,
                                                        const WIR_UpDownValue &v2 )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  list<WIR_Instruction> res;

  auto c0 = replace( v1, WIR_L4::b0, false );
  auto c1 = replace( v2.extract( 0, 5 ), WIR_L4::b0, false );

  // Determine whether c0 is a 4 bits unsigned constant and c1 is 5 bits
  // unsigned.
  bool c0fits =
    ( c0 >= 0 ) && ( c0 <= (long long) TC_Const16_Unsigned::getMaxValue( 4 ) );
  bool c1fits =
    ( c1 >= 0 ) && ( c1 <= (long long) TC_Const16_Unsigned::getMaxValue( 5 ) );

  if ( c0fits && c1fits ) {
    // Do the actual constant propagation of the second and third register
    // parameter of format EDDC5 to EC4C5C5.
    auto it = o.getExplicitParameters().begin();

    auto &p1o = it->get();
    auto *p1 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( (it++)->get() ) );

    ++it;
    auto *p2 = new TC_Const4_Unsigned( c0 );

    ++it;
    auto *p3 = new TC_Const5_Unsigned( c1 );

    WIR_Parameter *p4 =
      static_cast<WIR_Parameter *>( new TC_Const5_Unsigned(
        dynamic_cast<const TC_Const5_Unsigned &>( it->get() ) ) );

    // Patch incoming bit-values for all uses of p1.
    patchDefinedParameter( *p1, p1o );

    // Add bit-value container for new immediate parameter p2.
    patchImmediateParameter( *p2 );

    // Add bit-value container for new immediate parameter p3.
    patchImmediateParameter( *p3 );

    res.push_back(
      { { o.getOpCode(), TC13::OperationFormat::EC4C5C5, p1, p2, p3, p4 } } );

    checkSelfEdges( res.back().begin()->get(), o );
    copyContainers( res.back().begin()->get(), o );
  }

  return( res );
};


/*
  prop_ExDC5_ExC5C5 propagates constants for TriCore IMASK operations of formats
  EC4DC5 or EDDC5.

  If bits [4:0] of the given up value do not fit into a 5 bits unsigned
  constant, prop_ExDC5_ExC5C5 returns an empty instruction list.
*/
list<WIR_Instruction> TC_ConstProp::prop_ExDC5_ExC5C5( const WIR_Operation &o,
                                                       const WIR_UpDownValue &v )
{
  DSTART(
    "list<WIR::WIR_Instruction> TC_ConstProp::prop_ExDC5_ExC5C5(const WIR_Operation&, const WIR_UpDownValue&)" );

  list<WIR_Instruction> res;

  auto c0 = replace( v.extract( 0, 5 ), WIR_L4::b0, false );

  // Determine whether c0 is a 5 bits unsigned constant.
  bool c0fits =
    ( c0 >= 0 ) && ( c0 <= (long long) TC_Const16_Unsigned::getMaxValue( 5 ) );

  if ( c0fits ) {
    // Do the actual constant propagation of the third register parameter of
    // formats EC4DC5/EDDC5 to EC4C5C5/EDC5C5, resp.
    auto it = o.getExplicitParameters().begin();

    auto &p1o = it->get();
    auto *p1 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( (it++)->get() ) );

    auto &p2o = it->get();
    WIR_Parameter *p2 =
      o.getOperationFormat() == TC13::OperationFormat::EC4DC5 ?
        static_cast<WIR_Parameter *>( new TC_Const4_Unsigned(
          dynamic_cast<const TC_Const4_Unsigned &>( (it++)->get() ) ) ) :
        static_cast<WIR_Parameter *>( new WIR_RegisterParameter(
          dynamic_cast<const WIR_RegisterParameter &>( (it++)->get() ) ) );

    ++it;
    auto *p3 = new TC_Const5_Unsigned( c0 );

    WIR_Parameter *p4 =
      static_cast<WIR_Parameter *>( new TC_Const5_Unsigned(
        dynamic_cast<const TC_Const5_Unsigned &>( it->get() ) ) );

    // Patch incoming bit-values for all uses of p1.
    patchDefinedParameter( *p1, p1o );

    // Patch bit-values for p2.
    if ( o.getOperationFormat() == TC13::OperationFormat::EC4DC5 )
      patchImmediateParameter(
        dynamic_cast<WIR_BaseImmediateParameter &>( *p2 ) );
    else
      patchUsedParameter( *p2, p2o );

    // Add bit-value container for new immediate parameter p3.
    patchImmediateParameter( *p3 );

    res.push_back(
      { { o.getOpCode(),
          o.getOperationFormat() == TC13::OperationFormat::EC4DC5 ?
            TC13::OperationFormat::EC4C5C5 : TC13::OperationFormat::EDC5C5,
          p1, p2, p3, p4 } } );

    checkSelfEdges( res.back().begin()->get(), o );
    copyContainers( res.back().begin()->get(), o );
  }

  return( res );
};


/*
  prop_EDxC5_EC4xC5 propagates constants for TriCore IMASK operations of formats
  EDC5C5 or EDDC5.

  If the given up value do not fit into a 4 bits unsigned constant,
  prop_EDxC5_EC4xC5 returns an empty instruction list.
*/
list<WIR_Instruction> TC_ConstProp::prop_EDxC5_EC4xC5( const WIR_Operation &o,
                                                       const WIR_UpDownValue &v )
{
  DSTART(
    "list<WIR_Instruction> TC_ConstProp::prop_EDxC5_EC4xC5(const WIR_Operation&, const WIR_UpDownValue&)" );

  list<WIR_Instruction> res;

  auto c0 = replace( v, WIR_L4::b0, false );

  // Determine whether c0 is a 4 bits unsigned constant.
  bool c0fits =
    ( c0 >= 0 ) && ( c0 <= (long long) TC_Const16_Unsigned::getMaxValue( 4 ) );

  if ( c0fits ) {
    // Do the actual constant propagation of the second register parameter of
    // formats EDC5C5/EDDC5 to EC4C5C5/EC4DC5, resp.
    auto it = o.getExplicitParameters().begin();

    auto &p1o = it->get();
    auto *p1 =
      new WIR_RegisterParameter(
        dynamic_cast<const WIR_RegisterParameter &>( (it++)->get() ) );

    ++it;
    auto *p2 = new TC_Const4_Unsigned( c0 );

    auto &p3o = it->get();
    WIR_Parameter *p3 =
      o.getOperationFormat() == TC13::OperationFormat::EDC5C5 ?
        static_cast<WIR_Parameter *>( new TC_Const5_Unsigned(
          dynamic_cast<const TC_Const5_Unsigned &>( (it++)->get() ) ) ) :
        static_cast<WIR_Parameter *>( new WIR_RegisterParameter(
          dynamic_cast<const WIR_RegisterParameter &>( (it++)->get() ) ) );

    WIR_Parameter *p4 =
      static_cast<WIR_Parameter *>( new TC_Const5_Unsigned(
        dynamic_cast<const TC_Const5_Unsigned &>( it->get() ) ) );

    // Patch incoming bit-values for all uses of p1.
    patchDefinedParameter( *p1, p1o );

    // Add bit-value container for new immediate parameter p2.
    patchImmediateParameter( *p2 );

    // Patch bit-values for p3.
    if ( o.getOperationFormat() == TC13::OperationFormat::EDC5C5 )
      patchImmediateParameter(
        dynamic_cast<WIR_BaseImmediateParameter &>( *p3 ) );
    else
      patchUsedParameter( *p3, p3o );

    res.push_back(
      { { o.getOpCode(),
          o.getOperationFormat() == TC13::OperationFormat::EDC5C5 ?
            TC13::OperationFormat::EC4C5C5 : TC13::OperationFormat::EC4DC5,
          p1, p2, p3, p4 } } );

    checkSelfEdges( res.back().begin()->get(), o );
    copyContainers( res.back().begin()->get(), o );
  }

  return( res );
};


/*
  prop_xACBOA_xALBOA propagates locations refering to symbol addresses into
  TriCore LD.A and LD.W operations of formats AAC10BOA, AAC16BOA DAC10BOA or
  DAC16BOA.

  This method produces an instruction sequence of format

    MOVH.A areg_0, HI:s
    LD.W   areg_1, [areg_0] LO:s
*/
list<WIR_Instruction> TC_ConstProp::prop_xACBOA_xALBOA( const WIR_Operation &o,
                                                        const WIR_Symbol &s )
{
  DSTART(
    "list<WIR_Instruction> TC_ConstProp::prop_xACBOA_xALBOA(const WIR_Operation&, const WIR_Symbol&)" );

  list<WIR_Instruction> res;

  // Create a new virtual address register.
  WIR_Function &f = o.getInstruction().getBasicBlock().getFunction();
  auto &vreg = f.pushBackVirtualRegister( TC_ARegV() );

  // Create an up/down value modeling the data flowing between MOVH.A and LD.W.
  WIR_UpDownValue v { WIR_L4::b0, 32 };
  for ( unsigned int i = 16; i < 32; ++i )
    v.setBit( i, WIR_L4::bL, { s, i } );

  // Generate the MOVH.A operation.
  auto *movhDef = new WIR_RegisterParameter( vreg, WIR_Usage::def );
  res.push_back(
    { { TC13::OpCode::MOVH_A, TC13::OperationFormat::AL_1, movhDef,
        s.getType() == WIR_SymbolType::data ?
          new WIR_LabelParameter( s.getData() ) :
          new WIR_LabelParameter( s.getFunction() ) } } );

  copyContainers( res.back().begin()->get(), o );

  // Generate the LD.A or LD.W operation, resp.
  auto &p1o = o.getExplicitParameters().front().get();
  auto *p1 =
    new WIR_RegisterParameter(
      dynamic_cast<const WIR_RegisterParameter &>( p1o ) );

  auto *p2 = new WIR_RegisterParameter( vreg, WIR_Usage::use );

  auto &offset =
    dynamic_cast<WIR_BaseImmediateParameter &>(
      o.getExplicitParameters().back().get() );
  auto *p3 = new TC_Const16_Signed( offset.getSignedValue() );

  res.push_back(
    { { o.getOpCode(),
        o.getOpCode() == TC13::OpCode::LD_A ?
          TC13::OperationFormat::AALC16BOA : TC13::OperationFormat::DALC16BOA,
        p1, p2,
        s.getType() == WIR_SymbolType::data ?
          new WIR_LabelParameter( s.getData() ) :
          new WIR_LabelParameter( s.getFunction() ),
        p3 } } );

  copyContainers( res.back().begin()->get(), o );

  // Properly model the data flow from MOVH.A to LD.A/LD.W.
  auto *cont = new WIR_BitValues();
  movhDef->insertContainer( cont );
  cont->insertOutValues( *p2, WIR_UpDownValue( v ), WIR_UpDownValue( v ) );

  cont = new WIR_BitValues();
  p2->insertContainer( cont );
  cont->insertInValues( *movhDef, WIR_UpDownValue( v ), WIR_UpDownValue( v ) );

  // Patch incoming bit-values for all uses of p1.
  patchDefinedParameter( *p1, p1o );

  // Add bit-value container for new immediate parameter p3.
  patchImmediateParameter( *p3 );

  return( res );
};


/*
  prop_ACDBOA_ALDBOA propagates locations refering to symbol addresses into
  TriCore ST.W operations of formats AC10DBOA or AC16DBOA.

  This method produces an instruction sequence of format

    MOVH.A areg_0, HI:s
    ST.W   [areg_0] LO:s, dreg
*/
list<WIR_Instruction> TC_ConstProp::prop_ACDBOA_ALDBOA( const WIR_Operation &o,
                                                        const WIR_Symbol &s )
{
  DSTART(
    "list<WIR_Instruction> TC_ConstProp::prop_ACDBOA_ALDBOA(const WIR_Operation&, const WIR_Symbol&)" );

  list<WIR_Instruction> res;

  // Create a new virtual address register.
  WIR_Function &f = o.getInstruction().getBasicBlock().getFunction();
  auto &vreg = f.pushBackVirtualRegister( TC_ARegV() );

  // Create an up/down value modeling the data flowing between MOVH.A and LD.W.
  WIR_UpDownValue v { WIR_L4::b0, 32 };
  for ( unsigned int i = 16; i < 32; ++i )
    v.setBit( i, WIR_L4::bL, { s, i } );

  // Generate the MOVH.A operation.
  auto *movhDef = new WIR_RegisterParameter( vreg, WIR_Usage::def );
  res.push_back(
    { { TC13::OpCode::MOVH_A, TC13::OperationFormat::AL_1, movhDef,
        s.getType() == WIR_SymbolType::data ?
          new WIR_LabelParameter( s.getData() ) :
          new WIR_LabelParameter( s.getFunction() ) } } );

  copyContainers( res.back().begin()->get(), o );

  // Generate the ST.W operation.
  auto *p1 = new WIR_RegisterParameter( vreg, WIR_Usage::use );

  auto &offset =
    dynamic_cast<WIR_BaseImmediateParameter &>(
      std::next( o.begin() )->get() );
  auto *p2 = new TC_Const16_Signed( offset.getSignedValue() );

  auto &p3o = o.getExplicitParameters().back().get();
  auto *p3 =
    new WIR_RegisterParameter(
      dynamic_cast<const WIR_RegisterParameter &>( p3o ) );

  res.push_back(
    { { o.getOpCode(), TC13::OperationFormat::ALC16DBOA, p1,
        s.getType() == WIR_SymbolType::data ?
          new WIR_LabelParameter( s.getData() ) :
          new WIR_LabelParameter( s.getFunction() ),
        p2, p3 } } );

  copyContainers( res.back().begin()->get(), o );

  // Properly model the data flow from MOVH.A to ST.W.
  auto *cont = new WIR_BitValues();
  movhDef->insertContainer( cont );
  cont->insertOutValues( *p1, WIR_UpDownValue( v ), WIR_UpDownValue( v ) );

  cont = new WIR_BitValues();
  p1->insertContainer( cont );
  cont->insertInValues( *movhDef, WIR_UpDownValue( v ), WIR_UpDownValue( v ) );

  // Add bit-value container for new immediate parameter p2.
  patchImmediateParameter( *p2 );

  // Patch outgoing bit-values for all definitions of p3.
  patchUsedParameter( *p3, p3o );

  return( res );
};


/*
  patchDefinedParameter updates the bit-values associated with a defined
  parameter of an operation subject to constant propagation.

  For the targets of all out-edges of p, patchDefinedParameter removes pOrig
  from the set of incoming bit-values and adds p instead.
*/
void TC_ConstProp::patchDefinedParameter( const WIR_RegisterParameter &p,
                                          const WIR_Parameter &pOrig )
{
  DSTART(
    "void TC_ConstProp::patchDefinedParameter(const WIR_RegisterParameter&, const WIR_Parameter&)" );

  // Patch incoming bit-values for all uses of p.
  auto &srcContainer = p.getContainers<WIR_BitValues>().begin()->get();

  // Iterate all outgoing edges of p.
  for ( auto &outEdge : srcContainer.getOutValues() ) {
    auto &tgtContainer =
      outEdge.rp->getContainers<WIR_BitValues>().begin()->get();
    auto inEdgeIt = tgtContainer.findInValues( pOrig );

    tgtContainer.insertInValues(
      const_cast<WIR_RegisterParameter &>( p ),
      WIR_UpDownValue( inEdgeIt->downVal ),
      WIR_UpDownValue( inEdgeIt->upVal ) );
  }

  // Ensure that all locations in the bit-wise DFG refering to the old
  // parameter pOrig will be updated for the new parameter p.
  mNewLocation.insert(
    make_pair(
      pOrig.getID(), ref( const_cast<WIR_RegisterParameter &>( p ) ) ) );
};


/*
  patchUsedParameter updates the bit-values associated with a used parameter of
  an operation subject to constant propagation.

  For the sources of all in-edges of p, patchUsedParameter adds p to the set of
  outgoing bit-values.
*/
void TC_ConstProp::patchUsedParameter( const WIR_Parameter &p,
                                       const WIR_Parameter &pOrig ) const
{
  DSTART(
    "void TC_ConstProp::patchUsedParameter(const WIR_Parameter&, const WIR_Parameter&) const" );

  // Patch outgoing bit-values for all definitions of p.
  auto &tgtContainer = p.getContainers<WIR_BitValues>().begin()->get();

  // Patch former self-inedges pOrig -> pOrig first.
  if ( dynamic_cast<const WIR_RegisterParameter &>( p ).isUsed() ) {
    auto it = tgtContainer.findInValues( pOrig );
    if ( it != tgtContainer.getInValues().end() )
      it->rp = &const_cast<WIR_Parameter &>( p );
  }

  // Iterate all incoming edges of p.
  for ( auto &inEdge : tgtContainer.getInValues() ) {
    // Skip self-inedges.
    if ( *(inEdge.rp) == p )
      continue;

    auto &srcContainer =
      inEdge.rp->getContainers<WIR_BitValues>().begin()->get();
    auto outEdgeIt = srcContainer.findOutValues( pOrig );

    srcContainer.insertOutValues(
      const_cast<WIR_Parameter &>( p ), WIR_UpDownValue( outEdgeIt->downVal ),
      WIR_UpDownValue( outEdgeIt->upVal ) );
  }
};


/*
  patchUsedERegParameter updates the bit-values associated with a used extended
  register parameter of an operation subject to constant propagation.

  For the sources of all in-edges of pOrig, patchUsedERegParameter checks
  whether the current edge relates to the extended register's first child. If
  so, p is added to the set of outgoing bit-values.
*/
void TC_ConstProp::patchUsedERegParameter( WIR_Parameter &p,
                                           const WIR_Parameter &pOrig ) const
{
  DSTART(
    "void TC_ConstProp::patchUsedERegParameter(WIR_Parameter&, const WIR_Parameter&) const" );

  auto *cont = new WIR_BitValues();
  p.insertContainer( cont );

  // Determine the involved extended register.
  auto &eReg =
    dynamic_cast<const WIR_RegisterParameter &>( pOrig ).getRegister();

  // Patch outgoing bit-values for all definitions of pOrig.
  auto &tgtContainer = pOrig.getContainers<WIR_BitValues>().begin()->get();

  // Iterate all incoming edges of pOrig.
  for ( auto &inEdge : tgtContainer.getInValues() ) {
    // Determine the current edge's  source register.
    auto &srcReg =
      dynamic_cast<const WIR_RegisterParameter &>( *(inEdge.rp) ).getRegister();

    // Check whether the current edge relates to eRegs first child.
    if ( ( srcReg == eReg ) || ( srcReg == eReg.getLeafs().front() ) ) {
      auto &srcContainer =
        inEdge.rp->getContainers<WIR_BitValues>().begin()->get();
      auto outEdgeIt = srcContainer.findOutValues( pOrig );

      srcContainer.insertOutValues(
        const_cast<WIR_Parameter &>( p ), WIR_UpDownValue( outEdgeIt->downVal ),
        WIR_UpDownValue( outEdgeIt->upVal ) );
      cont->insertInValues(
        *(inEdge.rp), WIR_UpDownValue( outEdgeIt->downVal ),
        WIR_UpDownValue( outEdgeIt->upVal ) );
    }
  }
};


/*
  patchImmediateParameter creates a bit-value container for an immediate
  parameter newly created during constant propagation.
*/
void TC_ConstProp::patchImmediateParameter( WIR_BaseImmediateParameter &p ) const
{
  DSTART(
    "void TC_ConstProp::patchImmediateParameter(WIR_BaseImmediateParameter&) const" );

  auto *cont = new WIR_BitValues();
  p.insertContainer( cont );
  cont->insertInValues( p, WIR_UpDownValue( p ), WIR_UpDownValue( p ) );
};


/*
  checkSelfEdges checkes whether an original operation contains DFG edges
  starting and ending both at some of the operation's parameters. Such
  self-edges are then taken over to a new constant-folded operation.
*/
void TC_ConstProp::checkSelfEdges( WIR_Operation &o,
                                   const WIR_Operation &oOrig ) const
{
  DSTART(
    "void TC_ConstProp::checkSelfEdges(WIR_Operation&, const WIR_Operation&) const"  );

  for ( WIR_Parameter &pOrig : oOrig )
    if ( pOrig.getType() == WIR_ParameterType::reg ) {
      auto &rpOrig = dynamic_cast<WIR_RegisterParameter &>( pOrig );

      if ( ( rpOrig.isDefined() || rpOrig.isDefUsed() ) &&
           rpOrig.containsContainers( WIR_BitValues::getContainerTypeID() ) ) {
        // Get the register parameter's bitValue container.
        auto &cont = rpOrig.getContainers<WIR_BitValues>().begin()->get();

        // Iterate all outgoing edges of the curent register parameter.
        for ( auto &e : cont.getOutValues() ) {
          auto &tgtOrig =
            dynamic_cast<const WIR_RegisterParameter &>( *(e.rp) );

          if ( tgtOrig.getOperation() == oOrig ) {
            DACTION(
              stringstream sstr;
              sstr << wir << oOrig;
              DOUT(
                "Found self-edge '" << rpOrig << "' (ID " << rpOrig.getID() <<
                ") -> '" << tgtOrig << "' (ID " << tgtOrig.getID() <<
                ") in operation '" << sstr.str().substr( 8 ) <<
                "', down-value = " << e.downVal << ", up-value = " << e.upVal <<
                endl ); );

            // Find a definition of the same register and of same usage type in
            // the new constant-folded operation.
            auto itDef = o.end();
            for ( auto it = o.begin(); it != o.end(); ++it )
              if ( it->get().getType() == WIR_ParameterType::reg ) {
                auto &rp = dynamic_cast<WIR_RegisterParameter &>( it->get() );

                if ( ( rp.getUsage() == rpOrig.getUsage() ) &&
                     ( rp.getRegister() == rpOrig.getRegister() ) ) {
                  itDef = it;
                  break;
                }
              }

            // Find a use of the same register and of same usage type in
            // the new constant-folded operation.
            auto itUse = o.end();
            for ( auto it = o.begin(); it != o.end(); ++it )
              if ( it->get().getType() == WIR_ParameterType::reg ) {
                auto &rp = dynamic_cast<WIR_RegisterParameter &>( it->get() );

                if ( ( rp.getUsage() == tgtOrig.getUsage() ) &&
                     ( rp.getRegister() == tgtOrig.getRegister() ) ) {
                  itUse = it;
                  break;
                }
              }

            if ( ( itDef != o.end() ) && ( itUse != o.end() ) ) {
              DACTION(
                stringstream sstr;
                sstr << wir << o;
                DOUT(
                  "Adding self-edge '" << itDef->get() << "' (ID " <<
                  itDef->get().getID() << ") -> '" << itUse->get() <<
                  "' (ID " << itUse->get().getID() << ") in operation '" <<
                  sstr.str().substr( 8 ) << "', up-value = " << e.upVal <<
                  ", down-value = " << e.downVal << endl ); );

              auto &cDef =
                itDef->get().getContainers<WIR_BitValues>().begin()->get();
              auto &cUse =
                itUse->get().getContainers<WIR_BitValues>().begin()->get();

              cDef.insertOutValues(
                itUse->get(), WIR_UpDownValue( e.downVal ),
                WIR_UpDownValue( e.upVal ) );
              cUse.insertInValues(
                itDef->get(), WIR_UpDownValue( e.downVal ),
                WIR_UpDownValue( e.upVal ) );
            }
          }
        }
      }
    }
};


/*
  isSymbol checks whether an up/down value entirely refers to one WIR symbol.

  In order to refer completely to a symbol, all bits of the up/down value must
  be L, all locations must refer to one and the same symbol, and bit i in the
  given up/down value must refer to bit i of the symbol's address.
*/
bool TC_ConstProp::isSymbol( const WIR_UpDownValue &v ) const
{
  DSTART( "bool TC_ConstProp::isSymbol(const WIR_UpDownValue&) const" );

  WIR_id_t symID = nullid;

  for ( unsigned int i = 0; i < v.getBitWidth(); ++i ) {
    if ( v.at( i ) != WIR_L4::bL )
      return( false );

    auto &loc = v.getLocation( i );
    if ( loc.isRegisterParameter() )
      return( false );

    auto &sym = loc.getSymbol();
    if ( i == 0 )
      symID = sym.getID();
    else

    if ( ( sym.getID() != symID ) || ( loc.getBitPosition() != i ) )
      return( false );
  }

  return( true );
};

}       // namespace WIR
