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
  @file tcbitdfa.cc
  @brief This file implements a TriCore-specific bit-true data flow analysis.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include standard headers
#include <functional>
#include <iterator>
#include <set>
#include <sstream>
#include <utility>
#include <vector>

// Include libuseful headers
#include <libuseful/debugmacros.h>

// Include WIR headers
#include <wir/wir.h>
#include <arch/tricore/tc13.h>

// Include local headers
#include "tcbitdfa.h"


//
// Code section
//

namespace WIR {


using namespace boost;
using namespace std;


//
// Private global variables
//

static const vector<unsigned int> bytePos { 0, 8, 16, 24 };
static const vector<unsigned int> halfwordPos { 0, 16 };
static const vector<unsigned int> wordPos { 0, 32 };


//
// Public class methods
//

/*
  Default constructor for function-level analysis.
*/
TC_BitDFA::TC_BitDFA( WIR_Function &f ) :
  WIR_BitDFA { f }
{
  DSTART( "TC_BitDFA::TC_BitDFA(WIR_Function&)" );
};


/*
  Destructor.
*/
TC_BitDFA::~TC_BitDFA( void )
{
  DSTART( "virtual TC_BitDFA::~TC_BitDFA()" );
};


//
// Protected class methods
//

/*
  simulateTopDown performs the TriCore-specific top-down simulation of the given
  WIR operation.

  For a documentation of the semantics of TriCore operations, please refer to
  the TriCore User's Manual Volume 2 (Instruction Set).
*/
void TC_BitDFA::simulateTopDown( const WIR_Operation &o,
                                 std::map<WIR_id_t, WIR_UpDownValue> &operands,
                                 std::map<WIR_id_t, WIR_UpDownValue> &results )
{
  DSTART(
    "virtual void TC_BitDFA::simulateTopDown(const WIR_Operation&, map<long long unsigned int, WIR_UpDownValue>&, map<long long unsigned int, WIR_UpDownValue>&)" );

  DACTION(
    stringstream str;
    str << o;
    DOUT(
      "Simulating '" << str.str().substr( 8 ) << "' (ID = " << o.getID() <<
      ")." << endl ); );

  // A small lambda to update a register operand's bit down value to the bit
  // width required by the operand's register.
  auto updateOp = []( const WIR_Parameter &p, WIR_UpDownValue &v ) {
    // Compare bit width of down value with that of the parameter's actual
    // register.
    if ( p.getType() == WIR_ParameterType::reg ) {
      auto &r = dynamic_cast<const WIR_RegisterParameter &>( p ).getRegister();

      if ( r.getBitWidth() < v.getBitWidth() ) {
        // A wide down value is used for a tall register which can only happen
        // for hierarchical registers. Thus, apply an appropriate extract
        // operation.
        unsigned int offset = 0;

        if ( r.isVirtual() ) {
          auto &vr = dynamic_cast<WIR_VirtualRegister &>( r );
          offset = ( vr.getParent().begin()->get() == r ) ? 0 : 32;
        } else {
          auto &pr = dynamic_cast<WIR_PhysicalRegister &>( r );
          offset = ( pr.getParent().begin()->get() == r ) ? 0 : 32;
        }

        v = v.extract( offset, r.getBitWidth() );
      }
    }
  };

  // A small lambda to retrieve the operand value of o's first explicit
  // parameter.
  auto firstOp = [&]( void ) -> WIR_UpDownValue & {
    auto &p = o.getExplicitParameters().front().get();

    // Get down value from operands map.
    auto &v = operands.at( p.getID() );
    updateOp( p, v );

    return( v );
  };

  // A small lambda to retrieve the operand value of o's last explicit
  // parameter.
  auto lastOp = [&]( void ) -> WIR_UpDownValue & {
    auto &p = o.getExplicitParameters().back().get();

    // Get down value from operands map.
    auto &v = operands.at( p.getID() );
    updateOp( p, v );

    return( v );
  };

  // A small lambda to retrieve the operand value of o's nth explicit parameter.
  auto nthOp = [&]( unsigned int n ) -> WIR_UpDownValue & {
    auto it = o.getExplicitParameters().begin();
    std::advance( it, n );

    // Get down value from operands map.
    auto &v = operands.at( it->get().getID() );
    updateOp( it->get(), v );

    return( v );
  };

  // A small lambda to retrieve the result value of o's nth explicit parameter.
  auto nthRes = [&]( unsigned int n ) -> WIR_UpDownValue & {
    auto it = o.getExplicitParameters().begin();
    std::advance( it, n );

    // Check presence of parameter in results map.
    auto rIt = results.find( it->get().getID() );
    if ( rIt == results.end() )
      rIt =
        results.insert(
          { it->get().getID(),
            { WIR_L4::bU,
              dynamic_cast<WIR_RegisterParameter &>(
                it->get() ).getRegister().getBitWidth() } } ).first;

    // Return reference to down value from results map.
    return( rIt->second );
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

  // A small lambda to compute the carry PSW.C status bit (cf. TriCore
  // Instruction Set Manual, page 2-11).
  auto carry = [&]( WIR_UpDownValue a, WIR_UpDownValue b,
                    WIR_UpDownValue c ) -> WIR_UpDownValue {
    // carry( a, b, c ) {
    //   result = a + b + c; // unsigned additions
    //   return( result[ 32 ]; }

    // Zero-extend a.
    a.setSignedness( false );
    auto aExt = a.extend( 33 );
//       a33.setSignedness( false );

    // Zero-extend b.
    b.setSignedness( false );
    auto bExt = b.extend( 33 );
//       b33.setSignedness( false );

    // Zero-extend c.
    c.setSignedness( false );
    auto cExt = c.extend( 33 );

    return( ( aExt + bExt + cExt ).extract( 32, 1 ) );
  };


  //
  // ABS & co.
  //

  if ( o.getOpCode() == TC13::OpCode::ABS ) {
    auto &b = lastOp();
    b.setSignedness();
    nthRes( 0 ) = abs( b );
  }

  if ( o.getOpCode() == TC13::OpCode::ABS_B )
    for ( auto bitPos : bytePos ) {
      auto b = lastOp().extract( bitPos, 8 );
      b.setSignedness();
      insert( nthRes( 0 ), abs( b ), bitPos );
    }

  if ( o.getOpCode() == TC13::OpCode::ABS_H )
    for ( auto bitPos : halfwordPos ) {
      auto b = lastOp().extract( bitPos, 16 );
      b.setSignedness();
      insert( nthRes( 0 ), abs( b ), bitPos );
    }

  if ( o.getOpCode() == TC13::OpCode::ABSDIF ) {
    auto &a = nthOp( 1 );
    auto &b = lastOp();
    a.setSignedness();
    b.setSignedness();

    if ( ( o.getOperationFormat() == TC13::OperationFormat::DDD_1 ) &&
         ( nthReg( 1 ) == lastReg() ) )
      // Special case: ABSDIF Dc, Da, Da = 0
      nthRes( 0 ) = { WIR_L4::b0, 32, true };
    else
      nthRes( 0 ) = abs( a - b.extend( 32 ) );
  }

  if ( o.getOpCode() == TC13::OpCode::ABSDIF_B ) {
    if ( nthReg( 1 ) == lastReg() )
      // Special case: ABSDIF.B Dc, Da, Da = 0
      nthRes( 0 ) = { WIR_L4::b0, 32, true };
    else
      for ( auto bitPos: bytePos ) {
        auto a = nthOp( 1 ).extract( bitPos, 8 );
        auto b = lastOp().extract( bitPos, 8 );
        a.setSignedness();
        b.setSignedness();

        insert( nthRes( 0 ), abs( a - b ), bitPos );
      }
  }

  if ( o.getOpCode() == TC13::OpCode::ABSDIF_H ) {
    if ( nthReg( 1 ) == lastReg() )
      // Special case: ABSDIF.H Dc, Da, Da = 0
      nthRes( 0 ) = { WIR_L4::b0, 32, true };
    else
      for ( auto bitPos: halfwordPos ) {
        auto a = nthOp( 1 ).extract( bitPos, 16 );
        auto b = lastOp().extract( bitPos, 16 );
        a.setSignedness();
        b.setSignedness();

        insert( nthRes( 0 ), abs( a - b ), bitPos );
      }
  }

  if ( o.getOpCode() == TC13::OpCode::ABSDIFS ) {
    auto &a = nthOp( 1 );
    auto &b = lastOp();
    a.setSignedness();
    b.setSignedness();

    if ( ( o.getOperationFormat() == TC13::OperationFormat::DDD_1 ) &&
         ( nthReg( 1 ) == lastReg() ) )
      // Special case: ABSDIFS Dc, Da, Da = 0
      nthRes( 0 ) = { WIR_L4::b0, 32, true };
    else {
      WIR_UpDownValue res { WIR_L4::b0, 33, true };
      insert( res, abs( a - b.extend( 32 ) ), 0 );
      nthRes( 0 ) = ssov( res, 32 );
    }
  }

  if ( o.getOpCode() == TC13::OpCode::ABSDIFS_H ) {
    if ( nthReg( 1 ) == lastReg() )
      // Special case: ABSDIFS.H Dc, Da, Da = 0
      nthRes( 0 ) = { WIR_L4::b0, 32, true };
    else
      for ( auto bitPos: halfwordPos ) {
        auto a = nthOp( 1 ).extract( bitPos, 16 );
        auto b = lastOp().extract( bitPos, 16 );
        a.setSignedness();
        b.setSignedness();

        WIR_UpDownValue res_h{ WIR_L4::b0, 17, true };
        insert( res_h, abs( a - b ), 0 );
        insert( nthRes( 0 ), ssov( res_h, 16 ), bitPos );
      }
  }

  if ( o.getOpCode() == TC13::OpCode::ABSS ) {
    auto &b = lastOp();
    b.setSignedness();

    WIR_UpDownValue res { WIR_L4::b0, 33, true };
    insert( res, abs( b ), 0 );
    nthRes( 0 ) = ssov( res, 32 );
  }

  if ( o.getOpCode() == TC13::OpCode::ABSS_H )
    for ( auto bitPos : halfwordPos ) {
      auto b = lastOp().extract( bitPos, 16 );
      b.setSignedness();

      WIR_UpDownValue res_h { WIR_L4::b0, 17, true };
      insert( res_h, abs( b ), 0 );
      insert( nthRes( 0 ), ssov( res_h, 16 ), bitPos );
    }


  //
  // ADD & co.
  //

  if ( ( o.getOpCode() == TC13::OpCode::ADD ) ||
       ( o.getOpCode() == TC13::OpCode::ADD_A ) ||
       ( o.getOpCode() == TC13::OpCode::ADDI ) ) {
    auto b = lastOp().extend( 32 );
    b.setSignedness();
    auto &r = nthRes( 0 );

    if ( ( o.getOperationFormat() == TC13::OperationFormat::SDC4_2 ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::SDD_2 ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::SAC4_2 ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::SAA_5 ) )
      r = firstOp() + b;
    else
      r = nthOp( 1 ) + b;

    r.setSignedness( o.getOpCode() != TC13::OpCode::ADD_A );
  }

  if ( o.getOpCode() == TC13::OpCode::ADDC ) {
    auto a = nthOp( 1 );
    a.setSignedness();
    auto b = nthOp( 2 ).extend( 32 );
    b.setSignedness();
    auto carryIn = nthOp( 3 ).extend( 32 );
    carryIn.setSignedness();

    auto &r = nthRes( 0 );
    auto &carryOut = nthRes( 3 );

    r = a + b + carryIn;
    r.setSignedness();

    // Compute PSW.C.
    carryOut = carry( a, b, carryIn );
  }

  if ( o.getOpCode() == TC13::OpCode::ADDX ) {
    auto a = nthOp( 1 );
    a.setSignedness();
    auto b = nthOp( 2 ).extend( 32 );
    b.setSignedness();

    auto &r = nthRes( 0 );
    auto &carryOut = nthRes( 3 );

    r = a + b;
    r.setSignedness();

    // Compute PSW.C.
    carryOut = carry( a, b, WIR_UpDownValue( TC_Const4_Unsigned( 0 ) ) );
  }

  if ( o.getOpCode() == TC13::OpCode::ADD_B )
    for ( auto bitPos : bytePos )
      insert(
        nthRes( 0 ),
        nthOp( 1 ).extract( bitPos, 8 ) + lastOp().extract( bitPos, 8 ),
        bitPos );

  if ( o.getOpCode() == TC13::OpCode::ADD_H )
    for ( auto bitPos : halfwordPos )
      insert(
        nthRes( 0 ),
        nthOp( 1 ).extract( bitPos, 16 ) + lastOp().extract( bitPos, 16 ),
        bitPos );

  if ( ( o.getOpCode() == TC13::OpCode::ADDIH ) ||
       ( o.getOpCode() == TC13::OpCode::ADDIH_A ) ) {
    WIR_UpDownValue shamt { WIR_L4::b0, 32, lastOp().isSigned() };
    insert( shamt, lastOp(), 16 );
    nthRes( 0 ) = nthOp( 1 ) + shamt;
  }

  if ( o.getOpCode() == TC13::OpCode::ADDS ) {
    auto &a =
      ( o.getOperationFormat() == TC13::OperationFormat::SDD_2 ) ?
        firstOp() : nthOp( 1 );
    auto &b = lastOp();
    a.setSignedness();
    b.setSignedness();

    nthRes( 0 ) = ssov( a.extend( 33 ) + b.extend( 33 ), 32 );
  }

  if ( o.getOpCode() == TC13::OpCode::ADDS_H )
    for ( auto bitPos : halfwordPos ) {
      WIR_UpDownValue a = nthOp( 1 ).extract( bitPos, 16 );
      WIR_UpDownValue b = lastOp().extract( bitPos, 16 );
      a.setSignedness();
      b.setSignedness();

      WIR_UpDownValue res_h = a.extend( 17 ) + b.extend( 17 );
      insert( nthRes( 0 ), ssov( res_h, 16 ), bitPos );
    }

  if ( o.getOpCode() == TC13::OpCode::ADDS_HU )
    for ( auto bitPos : halfwordPos ) {
      WIR_UpDownValue a = nthOp( 1 ).extract( bitPos, 16 );
      WIR_UpDownValue b = lastOp().extract( bitPos, 16 );
      a.setSignedness( false );
      b.setSignedness( false );

      WIR_UpDownValue res_h = a.extend( 17 ) + b.extend( 17 );
      insert( nthRes( 0 ), suov( res_h, 16 ), bitPos );
    }

  if ( o.getOpCode() == TC13::OpCode::ADDS_U ) {
    auto &a = nthOp( 1 );
    auto &b = lastOp();
    a.setSignedness( false );
    b.setSignedness();

    // ADDS.U is so crazy: The 2nd summand is sign-extended (!), followed by an
    // unsigned (!) addition with final saturation on unsigned (!) overflow.
    WIR_UpDownValue tmp = b.extend( 32 );
    tmp.setSignedness( false );

    WIR_UpDownValue res = a.extend( 33 ) + tmp.extend( 33 );
    res.setSignedness( false );
    nthRes( 0 ) = suov( res, 32 );
  }

  if ( o.getOpCode() == TC13::OpCode::ADDSC_A ) {
    nthRes( 0 ) =
      nthOp( 1 ) +
      ( nthOp( 2 ) <<
        dynamic_cast<TC_Const2_Unsigned &>(
          o.getExplicitParameters().back().get() ).getUnsignedValue() );

    nthRes( 0 ).setSignedness( false );
  }

  if ( o.getOpCode() == TC13::OpCode::ADDSC_AT ) {
    auto &r = nthRes( 0 );
    r = nthOp( 1 ) + ( lastOp() >> 3 );
    r[ 0 ] = WIR_L4::b0;
    r[ 1 ] = WIR_L4::b0;
    r.setSignedness( false );
  }


  //
  // AND & co.
  //

  if ( o.getOpCode() == TC13::OpCode::AND ) {
    auto &b = lastOp();
    b.setSignedness( false );
    auto &r = nthRes( 0 );

    if ( ( o.getOperationFormat() == TC13::OperationFormat::SIC8_2 ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::SDD_2 ) )
      r = firstOp() & b.extend( 32 );
    else
      r = nthOp( 1 ) & b.extend( 32 );
    r.setSignedness( false );
  }

  if ( o.getOpCode() == TC13::OpCode::AND_AND_T ) {
    auto b1 = nthOp( 1 ).extract( nthOp( 2 ).getSignedValue(), 1 );
    auto b2 = nthOp( 3 ).extract( lastOp().getSignedValue(), 1 );
    auto &r = nthRes( 0 );

    r = firstOp();
    insert( r, r.extract( 0, 1 ) & ( b1 & b2 ), 0 );
    r.setSignedness( false );
  }

  if ( o.getOpCode() == TC13::OpCode::AND_ANDN_T ) {
    auto b1 = nthOp( 1 ).extract( nthOp( 2 ).getSignedValue(), 1 );
    auto b2 = nthOp( 3 ).extract( lastOp().getSignedValue(), 1 );
    auto &r = nthRes( 0 );

    r = firstOp();
    insert( r, r.extract( 0, 1 ) & ( b1 & ~b2 ), 0 );
    r.setSignedness( false );
  }

  if ( o.getOpCode() == TC13::OpCode::AND_EQ ) {
    auto &b = lastOp();
    b.setSignedness();
    auto &r = nthRes( 0 );
    WIR_L4 cmp = ( nthOp( 1 ) == b.extend( 32 ) );

    if ( ( o.getOperationFormat() == TC13::OperationFormat::DDD_2 ) &&
         ( nthReg( 1 ) == lastReg() ) )
      // Special case: AND.EQ Dc, Da, Da = Dc
      cmp = WIR_L4::b1;

    r = firstOp();
    insert( r, r.extract( 0, 1 ) & WIR_UpDownValue { cmp, 1, false }, 0 );
    r.setSignedness( false );
  }

  if ( ( o.getOpCode() == TC13::OpCode::AND_GE ) ||
       ( o.getOpCode() == TC13::OpCode::AND_GE_U ) ) {
    auto &a = nthOp( 1 );
    auto &b = lastOp();
    a.setSignedness( o.getOpCode() == TC13::OpCode::AND_GE );
    b.setSignedness( o.getOpCode() == TC13::OpCode::AND_GE );
    auto &r = nthRes( 0 );
    WIR_L4 cmp = ( a >= b.extend( 32 ) );

    if ( ( o.getOperationFormat() == TC13::OperationFormat::DDD_2 ) &&
         ( nthReg( 1 ) == lastReg() ) )
      // Special case: AND.GE Dc, Da, Da = Dc
      cmp = WIR_L4::b1;

    r = firstOp();
    insert( r, r.extract( 0, 1 ) & WIR_UpDownValue { cmp, 1, false }, 0 );
    r.setSignedness( false );
  }

  if ( ( o.getOpCode() == TC13::OpCode::AND_LT ) ||
       ( o.getOpCode() == TC13::OpCode::AND_LT_U ) ) {
    auto &a = nthOp( 1 );
    auto &b = lastOp();
    a.setSignedness( o.getOpCode() == TC13::OpCode::AND_LT );
    b.setSignedness( o.getOpCode() == TC13::OpCode::AND_LT );
    auto &r = nthRes( 0 );
    WIR_L4 cmp = ( a < b.extend( 32 ) );

    if ( ( o.getOperationFormat() == TC13::OperationFormat::DDD_2 ) &&
         ( nthReg( 1 ) == lastReg() ) )
      // Special case: AND.LT Dc, Da, Da = Dc[31:1] 0
      cmp = WIR_L4::b0;

    r = firstOp();
    insert( r, r.extract( 0, 1 ) & WIR_UpDownValue { cmp, 1, false }, 0 );
    r.setSignedness( false );
  }

  if ( o.getOpCode() == TC13::OpCode::AND_NE ) {
    auto &b = lastOp();
    b.setSignedness();
    auto &r = nthRes( 0 );
    WIR_L4 cmp = ( nthOp( 1 ) != b.extend( 32 ) );

    if ( ( o.getOperationFormat() == TC13::OperationFormat::DDD_2 ) &&
         ( nthReg( 1 ) == lastReg() ) )
      // Special case: AND.NE Dc, Da, Da = Dc[31:1] 0
      cmp = WIR_L4::b0;

    r = firstOp();
    insert( r, r.extract( 0, 1 ) & WIR_UpDownValue { cmp, 1, false }, 0 );
    r.setSignedness( false );
  }

  if ( o.getOpCode() == TC13::OpCode::AND_NOR_T ) {
    auto b1 = nthOp( 1 ).extract( nthOp( 2 ).getSignedValue(), 1 );
    auto b2 = nthOp( 3 ).extract( lastOp().getSignedValue(), 1 );
    auto &r = nthRes( 0 );

    r = firstOp();
    insert( r, r.extract( 0, 1 ) & ~( b1 | b2 ), 0 );
    r.setSignedness( false );
  }

  if ( o.getOpCode() == TC13::OpCode::AND_OR_T ) {
    auto b1 = nthOp( 1 ).extract( nthOp( 2 ).getSignedValue(), 1 );
    auto b2 = nthOp( 3 ).extract( lastOp().getSignedValue(), 1 );
    auto &r = nthRes( 0 );

    r = firstOp();
    insert( r, r.extract( 0, 1 ) & ( b1 | b2 ), 0 );
    r.setSignedness( false );
  }

  if ( o.getOpCode() == TC13::OpCode::AND_T ) {
    auto b1 = nthOp( 1 ).extract( nthOp( 2 ).getSignedValue(), 1 );
    auto b2 = nthOp( 3 ).extract( lastOp().getSignedValue(), 1 );
    auto &r = nthRes( 0 );

    r.setAllBits( WIR_L4::b0 );
    insert( r, b1 & b2, 0 );
    r.setSignedness( false );
  }

  if ( o.getOpCode() == TC13::OpCode::ANDN ) {
    auto &b = lastOp();
    b.setSignedness( false );
    auto &r = nthRes( 0 );

    if ( ( o.getOperationFormat() == TC13::OperationFormat::DDD_1 ) &&
         ( nthReg( 1 ) == lastReg() ) )
      // Special case: ANDN Dc, Da, Da = 0
      r = { WIR_L4::b0, 32, true };
    else
      r = nthOp( 1 ) & ~( b.extend( 32 ) );

    r.setSignedness( false );
  }

  if ( o.getOpCode() == TC13::OpCode::ANDN_T ) {
    auto b1 = nthOp( 1 ).extract( nthOp( 2 ).getSignedValue(), 1 );
    auto b2 = nthOp( 3 ).extract( lastOp().getSignedValue(), 1 );
    auto &r = nthRes( 0 );

    r.setAllBits( WIR_L4::b0 );
    insert( r, b1 & ~b2, 0 );
    r.setSignedness( false );
  }


  //
  // BMERGE & co.
  //

  if ( o.getOpCode() == TC13::OpCode::BMERGE ) {
    auto &r = nthRes( 0 );

    for ( unsigned int i = 0; i < 16; ++i ) {
      insert( r, nthOp( 1 ).extract( i, 1 ), 2 * i + 1 );
      insert( r, lastOp().extract( i, 1 ), 2 * i );
    }
  }

  if ( o.getOpCode() == TC13::OpCode::BSPLIT ) {
    auto &r = nthRes( 0 );
    r.setAllBits( WIR_L4::b0 );

    unsigned int j = 32;
    for ( unsigned int i = 1; i < 32; i += 2, ++j )
      insert( r, lastOp().extract( i, 1 ), j );

    j = 0;
    for ( unsigned int i = 0; i < 32; i += 2, ++j )
      insert( r, lastOp().extract( i, 1 ), j );
  }


  //
  // CADD & co.
  //

  if ( o.getOpCode() == TC13::OpCode::CADD ) {
    auto &b = lastOp();
    b.setSignedness();
    auto &r = nthRes( 0 );

    auto &cond = nthOp( 1 );
    auto &a =
      ( o.getOperationFormat() == TC13::OperationFormat::SDIC4_3 ) ?
        firstOp() : nthOp( 2 );

    if ( cond.containsBits( { WIR_L4::b1, WIR_L4::bX } ) )
      r = a + b.extend( 32 );
    else

    if ( cond.containsOnlyBit( WIR_L4::b0 ) )
      r = a;
  }

  if ( o.getOpCode() == TC13::OpCode::CADDN ) {
    auto &b = lastOp();
    b.setSignedness();
    auto &r = nthRes( 0 );

    auto &cond = nthOp( 1 );
    auto &a =
      ( o.getOperationFormat() == TC13::OperationFormat::SDIC4_3 ) ?
        firstOp() : nthOp( 2 );

    if ( cond.containsOnlyBits( { WIR_L4::b0, WIR_L4::bX } ) )
      r = a + b.extend( 32 );
    else

    if ( cond.containsBit( WIR_L4::b1 ) )
      r = a;
  }


  //
  // CLO & co.
  //

  if ( o.getOpCode() == TC13::OpCode::CLO )
    nthRes( 0 ) = leading_ones( lastOp() ).extend( 32 );

  if ( o.getOpCode() == TC13::OpCode::CLO_H )
    for ( auto bitPos : halfwordPos )
      insert(
        nthRes( 0 ),
        leading_ones( lastOp().extract( bitPos, 16 ) ).extend( 16 ), bitPos );

  if ( o.getOpCode() == TC13::OpCode::CLS )
    nthRes( 0 ) = leading_signs( lastOp() ).extend( 32 );

  if ( o.getOpCode() == TC13::OpCode::CLS_H )
    for ( auto bitPos : halfwordPos )
      insert(
        nthRes( 0 ),
        leading_signs( lastOp().extract( bitPos, 16 ) ).extend( 16 ), bitPos );

  if ( o.getOpCode() == TC13::OpCode::CLZ )
    nthRes( 0 ) = leading_zeros( lastOp() ).extend( 32 );

  if ( o.getOpCode() == TC13::OpCode::CLZ_H )
    for ( auto bitPos : halfwordPos )
      insert(
        nthRes( 0 ),
        leading_zeros( lastOp().extract( bitPos, 16 ) ).extend( 16 ), bitPos );


  //
  // CMOV & co.
  //

  if ( o.getOpCode() == TC13::OpCode::CMOV ) {
    auto &b = lastOp();
    b.setSignedness();

    auto &cond = nthOp( 1 );

    if ( cond.containsBits( { WIR_L4::b1, WIR_L4::bX } ) )
      nthRes( 0 ) = b.extend( 32 );
    else

    if ( cond.containsOnlyBit( WIR_L4::b0 ) )
      nthRes( 0 ) = firstOp();
  }

  if ( o.getOpCode() == TC13::OpCode::CMOVN ) {
    auto &b = lastOp();
    b.setSignedness();

    auto &cond = nthOp( 1 );

    if ( cond.containsOnlyBits( { WIR_L4::b0, WIR_L4::bX } ) )
      nthRes( 0 ) = b.extend( 32 );
    else

    if ( cond.containsBit( WIR_L4::b1 ) )
      nthRes( 0 ) = firstOp();
  }


  //
  // CMP.F
  //

  if ( o.getOpCode() == TC13::OpCode::CMP_F ) {
    nthRes( 0 ).setAllBits( WIR_L4::b0 );
    insert( nthRes( 0 ), WIR_UpDownValue { WIR_L4::bU, 6, false }, 0 );
  }


  //
  // CSUB & co.
  //

  if ( o.getOpCode() == TC13::OpCode::CSUB ) {
    auto &b = lastOp();
    b.setSignedness();

    auto &cond = nthOp( 1 );

    if ( cond.containsBits( { WIR_L4::b1, WIR_L4::bX } ) )
      nthRes( 0 ) = nthOp( 2 ) - b;
    else

    if ( cond.containsOnlyBit( WIR_L4::b0 ) )
      nthRes( 0 ) = nthOp( 2 );
  }

  if ( o.getOpCode() == TC13::OpCode::CSUBN ) {
    auto &b = lastOp();
    b.setSignedness();

    auto &cond = nthOp( 1 );

    if ( cond.containsOnlyBits( { WIR_L4::b0, WIR_L4::bX } ) )
      nthRes( 0 ) = nthOp( 2 ) - b;
    else

    if ( cond.containsBit( WIR_L4::b1 ) )
      nthRes( 0 ) = nthOp( 2 );
  }


  //
  // DEXTR
  //

  if ( o.getOpCode() == TC13::OpCode::DEXTR ) {
    WIR_UpDownValue large { WIR_L4::bU, 64, false };
    insert( large, nthOp( 1 ), 32 );
    insert( large, nthOp( 2 ), 0 );
    auto &shamt = lastOp();

    if ( shamt.extract( 0, 5 ).isInteger() && ( shamt.getSignedValue() >= 0 ) &&
         ( shamt.getSignedValue() <= 31 ) )
      nthRes( 0 ) = ( large << shamt ).extract( 32, 32 );
  }


  //
  // DV* & co.
  //

  if ( o.getOpCode() == TC13::OpCode::DVADJ ) {
    auto &d = nthOp( 1 );
    auto &b = lastOp();
    b.setSignedness();
    auto &r = nthRes( 0 );

    WIR_UpDownValue dividend_quotient = d.extract( 0, 32 );
    WIR_UpDownValue remainder = d.extract( 32, 32 );
    remainder.setSignedness();

    auto abs_remainder = abs( remainder );
    auto abs_b = abs( b );

    if ( ( abs_remainder.containsOnlyBit( WIR_L4::bU ) ) ||
         ( abs_b.containsOnlyBit( WIR_L4::bU ) ) )
      return;

    auto cmp = ( abs_remainder == abs_b );
    if ( ( cmp == WIR_L4::b1 ) && ( d[ 63 ] == WIR_L4::b1 ) ) {
      insert( r, { WIR_L4::b0, 32, false }, 32 );

      if ( d[ 31 ] == WIR_L4::b1 )
        insert( r, dividend_quotient, 0 );
      else

      if ( d[ 31 ] == WIR_L4::b0 ) {
        WIR_UpDownValue one { WIR_L4::b0, 32, true };
        one[ 0 ] = WIR_L4::b1;
        insert( r, dividend_quotient + one, 0 );
      } else
        insert( r, { WIR_L4::bU, 32, false }, 0 );
    } else

    if ( ( cmp == WIR_L4::b0 ) || ( d[ 63 ] == WIR_L4::b0 ) ) {
      insert( r, remainder, 32 );

      if ( d[ 31 ] == WIR_L4::b1 ) {
        WIR_UpDownValue one { WIR_L4::b0, 32, true };
        one[ 0 ] = WIR_L4::b1;
        insert( r, dividend_quotient + one, 0 );
      } else

      if ( d[ 31 ] == WIR_L4::b0 )
        insert( r, dividend_quotient, 0 );
      else
        insert( r, { WIR_L4::bU, 32, false }, 0 );
    }
  }

  if ( ( o.getOpCode() == TC13::OpCode::DVINIT ) ||
       ( o.getOpCode() == TC13::OpCode::DVINIT_U ) ) {
    auto &a = nthOp( 1 );
    a.setSignedness( o.getOpCode() == TC13::OpCode::DVINIT );

    nthRes( 0 ) = a.extend( 64 );
  }

  if ( o.getOpCode() == TC13::OpCode::DVINIT_B ) {
    auto &a = nthOp( 1 );
    a.setSignedness();
    auto &b = lastOp();

    WIR_L4 quotient_bit = WIR_L4::bU;
    if ( ( ( getLevel( a.at( 31 ) ) == 2 ) &&
           ( getLevel( b.at( 31 ) ) == 2 ) ) ||
         ( ( getLevel( a.at( 31 ) ) == 1 ) && ( getLevel( b.at( 31 ) ) == 1 ) &&
           ( a.getLocation( 31 ) == b.getLocation( 31 ) ) ) )
      // The sign bits of both operands are either '0' or '1', or the sign bits
      // both refer to the same location. We can directly compare them in these
      // cases.
      quotient_bit = ( a.at( 31 ) == b.at( 31 ) ) ? WIR_L4::b0 : WIR_L4::b1;

    insert( nthRes( 0 ), a.extend( 40 ), 24 );
    insert( nthRes( 0 ), { quotient_bit, 24, false }, 0 );
  }

  if ( o.getOpCode() == TC13::OpCode::DVINIT_BU ) {
    auto &a = nthOp( 1 );
    a.setSignedness( false );

    insert( nthRes( 0 ), a.extend( 40 ), 24 );
    insert( nthRes( 0 ), { WIR_L4::b0, 24, false }, 0 );
  }

  if ( o.getOpCode() == TC13::OpCode::DVINIT_H ) {
    auto &a = nthOp( 1 );
    a.setSignedness();
    auto &b = lastOp();

    WIR_L4 quotient_bit = WIR_L4::bU;
    if ( ( ( getLevel( a.at( 31 ) ) == 2 ) &&
           ( getLevel( b.at( 31 ) ) == 2 ) ) ||
         ( ( getLevel( a.at( 31 ) ) == 1 ) && ( getLevel( b.at( 31 ) ) == 1 ) &&
           ( a.getLocation( 31 ) == b.getLocation( 31 ) ) ) )
      // The sign bits of both operands are either '0' or '1', or the sign bits
      // both refer to the same location. We can directly compare them in these
      // cases.
      quotient_bit = ( a.at( 31 ) == b.at( 31 ) ) ? WIR_L4::b0 : WIR_L4::b1;

    insert( nthRes( 0 ), a.extend( 48 ), 16 );
    insert( nthRes( 0 ), { quotient_bit, 16, false }, 0 );
  }

  if ( o.getOpCode() == TC13::OpCode::DVINIT_HU ) {
    auto &a = nthOp( 1 );
    a.setSignedness( false );

    insert( nthRes( 0 ), a.extend( 48 ), 16 );
    insert( nthRes( 0 ), { WIR_L4::b0, 16, false }, 0 );
  }

  if ( o.getOpCode() == TC13::OpCode::DVSTEP ) {
    auto &d = nthOp( 1 );
    auto &b = lastOp();
    b.setSignedness();

    WIR_L4 dividend_sign = d[ 63 ];
    WIR_L4 divisor_sign = b[ 31 ];

    WIR_L4 quotient_sign = WIR_L4::bU;
    if ( ( ( getLevel( dividend_sign ) == 2 ) &&
           ( getLevel( divisor_sign ) == 2 ) ) ||
         ( ( getLevel( dividend_sign ) == 1 ) &&
           ( getLevel( divisor_sign ) == 1 ) &&
           ( d.getLocation( 63 ) == b.getLocation( 31 ) ) ) )
      // The sign bits of both operands are either '0' or '1', or the sign bits
      // both refer to the same location. We can directly compare them in these
      // cases.
      quotient_sign =
        ( dividend_sign != divisor_sign ) ? WIR_L4::b1 : WIR_L4::b0;

    WIR_UpDownValue addend { WIR_L4::bU, 32, true };
    if ( quotient_sign == WIR_L4::b1 )
      addend = b;
    if ( quotient_sign == WIR_L4::b0 )
      addend = -b;

    WIR_UpDownValue dividend_quotient = d.extract( 0, 32 );
    WIR_UpDownValue remainder = d.extract( 32, 32 );

    for ( unsigned int i = 0; i < 8; ++i ) {
      WIR_UpDownValue bitMask { WIR_L4::b0, 32, false };
      if ( getLevel( dividend_quotient[ 31 ] ) == 1 )
        bitMask.setBit(
          0, dividend_quotient[ 31 ], dividend_quotient.getLocation( 31 ) );
      else
        bitMask.setBit( 0, dividend_quotient[ 31 ] );

      remainder = ( remainder << 1 ) | bitMask;
      dividend_quotient = dividend_quotient << 1;

      WIR_UpDownValue temp = remainder + addend;

      if ( ( !temp.isNegative() && !temp.isPositive() ) ||
           ( getLevel( dividend_sign ) != 2 ) )
        remainder.setAllBits( WIR_L4::bU );
      else
        remainder = ( temp[ 31 ] == dividend_sign ) ? temp : remainder;

      if ( ( !temp.isNegative() && !temp.isPositive() ) ||
           ( getLevel( dividend_sign ) != 2 ) )
        bitMask[ 0 ] = WIR_L4::bU;
      else
        bitMask[ 0 ] =
         ( temp[ 31 ] == dividend_sign ) ? ~quotient_sign : quotient_sign;

      dividend_quotient = dividend_quotient | bitMask;
    }

    insert( nthRes( 0 ), remainder, 32 );
    insert( nthRes( 0 ), dividend_quotient, 0 );
  }

  if ( o.getOpCode() == TC13::OpCode::DVSTEP_U ) {
    auto &divisor = lastOp();
    auto &d = nthOp( 1 );
    WIR_UpDownValue dividend_quotient = d.extract( 0, 32 );
    WIR_UpDownValue remainder = d.extract( 32, 32 );

    for ( unsigned int i = 0; i < 8; ++i ) {
      WIR_UpDownValue bitMask { WIR_L4::b0, 32, false };
      if ( getLevel( dividend_quotient[ 31 ] ) == 1 )
        bitMask.setBit(
          0, dividend_quotient[ 31 ], dividend_quotient.getLocation( 31 ) );
      else
        bitMask.setBit( 0, dividend_quotient[ 31 ] );

      remainder = ( remainder << 1 ) | bitMask;
      dividend_quotient = dividend_quotient << 1;

      WIR_UpDownValue temp = remainder - divisor;

      if ( temp.isPositive() )
        remainder = temp;
      if ( !temp.isPositive() && !temp.isNegative() )
        remainder.setAllBits( WIR_L4::bU );

      if ( temp.isPositive() )
        bitMask[ 0 ] = WIR_L4::b1;
      if ( temp.isNegative() )
        bitMask[ 0 ] = WIR_L4::b0;
      if ( !temp.isPositive() && !temp.isNegative() )
        bitMask[ 0 ] = WIR_L4::bU;

      dividend_quotient = dividend_quotient | bitMask;
    }

    insert( nthRes( 0 ), remainder, 32 );
    insert( nthRes( 0 ), dividend_quotient, 0 );
  }


  //
  // EQ & co.
  //

  if ( ( o.getOpCode() == TC13::OpCode::EQ ) ||
       ( o.getOpCode() == TC13::OpCode::EQ_A ) ) {
    auto &a = nthOp( 1 );
    auto &b = lastOp();
    b.setSignedness();

    nthRes( 0 ) = { WIR_L4::b0, 32, false };

    if ( ( ( o.getOperationFormat() == TC13::OperationFormat::DDD_1 ) ||
           ( o.getOperationFormat() == TC13::OperationFormat::DAA ) ) &&
         ( nthReg( 1 ) == lastReg() ) )
      // Special case: EQ Dc, Da, Da = 1
      nthRes( 0 )[ 0 ] = WIR_L4::b1;
    else
      nthRes( 0 )[ 0 ] = ( a == b.extend( 32 ) );
  }

  if ( o.getOpCode() == TC13::OpCode::EQ_B )
    if ( nthReg( 1 ) == lastReg() )
      // Special case: EQ.B Dc, Da, Da = 0xFFFFFFFF
      nthRes( 0 ) = { WIR_L4::b1, 32, false };
    else
      for ( auto bitPos : bytePos ) {
        auto a = nthOp( 1 ).extract( bitPos, 8 );
        auto b = lastOp().extract( bitPos, 8 );

        insert( nthRes( 0 ), { a == b, 8, false }, bitPos );
      }

  if ( o.getOpCode() == TC13::OpCode::EQ_H ) {
    if ( nthReg( 1 ) == lastReg() )
      // Special case: EQ.H Dc, Da, Da = 0xFFFFFFFF
      nthRes( 0 ) = { WIR_L4::b1, 32, false };
    else
      for ( auto bitPos: halfwordPos ) {
        auto a = nthOp( 1 ).extract( bitPos, 16 );
        auto b = lastOp().extract( bitPos, 16 );

        insert( nthRes( 0 ), { a == b, 16, false }, bitPos );
      }
  }

  if ( o.getOpCode() == TC13::OpCode::EQ_W ) {
    if ( nthReg( 1 ) == lastReg() )
      // Special case: EQ.W Dc, Da, Da = 0xFFFFFFFF
      nthRes( 0 ) = { WIR_L4::b1, 32, false };
    else
      nthRes( 0 ) = { nthOp( 1 ) == lastOp(), 32, false };
  }

  if ( o.getOpCode() == TC13::OpCode::EQANY_B ) {
    auto &a = nthOp( 1 );
    auto &b = lastOp();
    b.setSignedness();
    b = b.extend( 32 );
    WIR_L4 cmp = WIR_L4::b0;

    if ( ( o.getOperationFormat() == TC13::OperationFormat::DDD_1 ) &&
         ( nthReg( 1 ) == lastReg() ) )
      // Special case: EQANY.B Dc, Da, Da = 1
      cmp = WIR_L4::b1;
    else
      for ( auto bitPos : bytePos )
        cmp = cmp | ( a.extract( bitPos, 8 ) == b.extract( bitPos, 8 ) );

    nthRes( 0 ) = { WIR_L4::b0, 32, false };
    nthRes( 0 )[ 0 ] = cmp;
  }

  if ( o.getOpCode() == TC13::OpCode::EQANY_H ) {
    auto &a = nthOp( 1 );
    auto &b = lastOp();
    b.setSignedness();
    b = b.extend( 32 );
    WIR_L4 cmp = WIR_L4::b0;

    if ( ( o.getOperationFormat() == TC13::OperationFormat::DDD_1 ) &&
         ( nthReg( 1 ) == lastReg() ) )
      // Special case: EQANY.H Dc, Da, Da = 1
      cmp = WIR_L4::b1;
    else
      for ( auto bitPos : halfwordPos )
        cmp = cmp | ( a.extract( bitPos, 16 ) == b.extract( bitPos, 16 ) );

    nthRes( 0 ) = { WIR_L4::b0, 32, false };
    nthRes( 0 )[ 0 ] = cmp;
  }

  if ( o.getOpCode() == TC13::OpCode::EQZ_A ) {
    nthRes( 0 ) = { WIR_L4::b0, 32, false };
    nthRes( 0 )[ 0 ] =
      ( lastOp() == WIR_UpDownValue { WIR_L4::b0, 32, false } );
  }


  //
  // EXTR & co.
  //

  if ( ( o.getOpCode() == TC13::OpCode::EXTR ) ||
       ( o.getOpCode() == TC13::OpCode::EXTR_U ) ) {
    auto &a = nthOp( 1 );
    a.setSignedness( false );
    auto &r = nthRes( 0 );

    unsigned int pos = 0;
    unsigned int width = 0;

    if ( o.getOperationFormat() == TC13::OperationFormat::DDC5C5 ) {
      auto it = o.getExplicitParameters().begin();
      std::advance( it, 2 );
      pos = dynamic_cast<TC_Const5_Unsigned &>( it->get() ).getUnsignedValue();
      std::advance( it, 1 );
      width =
        dynamic_cast<TC_Const5_Unsigned &>( it->get() ).getUnsignedValue();
    } else

    if ( o.getOperationFormat() == TC13::OperationFormat::DDE ) {
      WIR_UpDownValue p = lastOp().extract( 0, 5 );
      WIR_UpDownValue w = lastOp().extract( 32, 5 );

      if ( !p.isBinaryInteger() || !w.isBinaryInteger() )
        return;

      pos = p.getSignedValue();
      width = w.getSignedValue();
    } else {
      WIR_UpDownValue p = nthOp( 2 ).extract( 0, 5 );

      if ( !p.isBinaryInteger() )
        return;

      pos = p.getSignedValue();
      width =
        dynamic_cast<TC_Const5_Unsigned &>(
          o.getExplicitParameters().back().get() ).getUnsignedValue();
    }

    r = a.extract( pos, width );
    r.setSignedness( o.getOpCode() == TC13::OpCode::EXTR );
    r = r.extend( 32 );
  }


  //
  // GE & co.
  //

  if ( ( o.getOpCode() == TC13::OpCode::GE ) ||
       ( o.getOpCode() == TC13::OpCode::GE_A ) ||
       ( o.getOpCode() == TC13::OpCode::GE_U ) ) {
    auto &a = nthOp( 1 );
    auto &b = lastOp();
    a.setSignedness( o.getOpCode() == TC13::OpCode::GE );
    b.setSignedness( o.getOpCode() == TC13::OpCode::GE );

    nthRes( 0 ) = { WIR_L4::b0, 32, false };

    if ( ( ( o.getOperationFormat() == TC13::OperationFormat::DDD_1 ) ||
           ( o.getOperationFormat() == TC13::OperationFormat::DAA ) ) &&
         ( nthReg( 1 ) == lastReg() ) )
      // Special case: GE Dc, Da, Da = 1
      nthRes( 0 )[ 0 ] = WIR_L4::b1;
    else
      nthRes( 0 )[ 0 ] = ( a >= b.extend( 32 ) );
  }


  //
  // IMASK
  //

  if ( o.getOpCode() == TC13::OpCode::IMASK ) {
    auto &b = nthOp( 1 );
    b.setSignedness( false );

    // Construct 2^width - 1.
    unsigned int width =
      dynamic_cast<TC_Const5_Unsigned &>(
        o.getExplicitParameters().back().get() ).getUnsignedValue();
    WIR_UpDownValue tmp { WIR_L4::b0, 32, false };
    for ( unsigned int i = 0; i < width; ++i )
      tmp[ i ] = WIR_L4::b1;

    if ( ( o.getOperationFormat() == TC13::OperationFormat::EC4C5C5 ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::EDC5C5 ) ) {
      auto it = o.getExplicitParameters().begin();
      std::advance( it, 2 );
      unsigned int pos =
        dynamic_cast<TC_Const5_Unsigned &>( it->get() ).getUnsignedValue();

      insert( nthRes( 0 ), tmp << pos, 32 );
      insert( nthRes( 0 ), b.extend( 32 ) << pos, 0 );
    } else {
      auto &pos = nthOp( 2 );

      insert( nthRes( 0 ), tmp << pos.extract( 0, 5 ), 32 );
      insert( nthRes( 0 ), b.extend( 32 ) << pos.extract( 0, 5 ), 0 );
    }
  }


  //
  // INS* & co.
  //

  if ( ( o.getOpCode() == TC13::OpCode::INS_T ) ||
       ( o.getOpCode() == TC13::OpCode::INSN_T ) ) {
    nthRes( 0 ) = nthOp( 1 );
    nthRes( 0 ).setSignedness( false );

    auto it = o.getExplicitParameters().begin();
    std::advance( it, 2 );
    unsigned int pos1 =
      dynamic_cast<TC_Const5_Unsigned &>( it->get() ).getUnsignedValue();
    std::advance( it, 2 );
    unsigned int pos2 =
      dynamic_cast<TC_Const5_Unsigned &>( it->get() ).getUnsignedValue();

    WIR_L4 bit =
      o.getOpCode() == TC13::OpCode::INS_T ?
        nthOp( 3 )[ pos2 ] : ~(nthOp( 3 )[ pos2 ]);

    if ( getLevel( bit ) == 1 )
      nthRes( 0 ).setBit( pos1, bit, nthOp( 3 ).getLocation( pos2 ) );
    else
      nthRes( 0 ).setBit( pos1, bit );
  }

  if ( o.getOpCode() == TC13::OpCode::INSERT ) {
    unsigned int pos = 0;
    unsigned int width = 0;

    if ( ( o.getOperationFormat() == TC13::OperationFormat::DDC4C5C5 ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::DDDC5C5 ) ) {
      auto it = o.getExplicitParameters().begin();
      std::advance( it, 3 );
      pos = dynamic_cast<TC_Const5_Unsigned &>( it->get() ).getUnsignedValue();
      std::advance( it, 1 );
      width =
        dynamic_cast<TC_Const5_Unsigned &>( it->get() ).getUnsignedValue();
    } else

    if ( ( o.getOperationFormat() == TC13::OperationFormat::DDC4E ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::DDDE ) ) {
      lastOp().setSignedness( false );
      WIR_UpDownValue p = lastOp().extract( 0, 5 );
      WIR_UpDownValue w = lastOp().extract( 32, 5 );

      if ( !p.isBinaryInteger() || !w.isBinaryInteger() )
        return;

      pos = p.getSignedValue();
      width = w.getSignedValue();
    } else {
      WIR_UpDownValue p = nthOp( 3 ).extract( 0, 5 );

      if ( !p.isBinaryInteger() )
        return;

      pos = p.getSignedValue();
      width =
        dynamic_cast<TC_Const5_Unsigned &>(
          o.getExplicitParameters().back().get() ).getUnsignedValue();
    }

    auto &b = nthOp( 1 );
    b.setSignedness( false );
    nthRes( 0 ) = b;

    auto &p = nthOp( 2 );
    p.setSignedness( false );
    auto packet = p.extend( 32 ).extract( 0, width );
    insert( nthRes( 0 ), packet, pos );
  }


  //
  // IXMAX* & co.
  //

  if ( ( o.getOpCode() == TC13::OpCode::IXMAX ) ||
       ( o.getOpCode() == TC13::OpCode::IXMAX_U ) ||
       ( o.getOpCode() == TC13::OpCode::IXMIN ) ||
       ( o.getOpCode() == TC13::OpCode::IXMIN_U ) ) {
    // Set most-significant 16 bits of c to 0.
    WIR_UpDownValue zero { WIR_L4::b0, 16, false };
    insert( nthRes( 0 ), zero, 48 );
  }


  //
  // JNE* & co.
  //

  if ( ( o.getOpCode() == TC13::OpCode::JNED ) ||
       ( o.getOpCode() == TC13::OpCode::JNEI ) ) {
    WIR_UpDownValue one = { WIR_L4::b0, 32, true };
    one[ 0 ] = WIR_L4::b1;
    nthRes( 0 ) =
      ( o.getOpCode() == TC13::OpCode::JNED ) ?
        firstOp() - one : firstOp() + one;
  }


  //
  // LD* & co.
  //

  if ( ( ( o.getOpCode() == TC13::OpCode::LD_A ) ||
         ( o.getOpCode() == TC13::OpCode::LD_B ) ||
         ( o.getOpCode() == TC13::OpCode::LD_BU ) ||
         ( o.getOpCode() == TC13::OpCode::LD_D ) ||
         ( o.getOpCode() == TC13::OpCode::LD_DA ) ||
         ( o.getOpCode() == TC13::OpCode::LD_H ) ||
         ( o.getOpCode() == TC13::OpCode::LD_HU ) ||
         ( o.getOpCode() == TC13::OpCode::LD_Q ) ||
         ( o.getOpCode() == TC13::OpCode::LD_W ) ) &&
       ( ( o.getOperationFormat() == TC13::OperationFormat::APBRA ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::DPBRA ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::EPBRA ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::PPBRA_1 ) ) ) {
    // Here, only the address arithmetics caused by bit-reverse addressing is
    // modeled. Post/pre-increment/decrement or circular addressing is not
    // considered here.
    auto &b = lastOp();
    b.setSignedness( false );

    auto index = b.extract( 32, 16 );
    auto incr = b.extract( 48, 16 );
    auto new_index = reverse( reverse( index ) + reverse( incr ) );

    nthRes( 1 ) = b;
    insert( nthRes( 1 ), new_index, 32 );
    insert( nthRes( 1 ), incr, 48 );
  }

  if ( ( ( o.getOpCode() == TC13::OpCode::LD_A ) ||
         ( o.getOpCode() == TC13::OpCode::LD_B ) ||
         ( o.getOpCode() == TC13::OpCode::LD_BU ) ||
         ( o.getOpCode() == TC13::OpCode::LD_D ) ||
         ( o.getOpCode() == TC13::OpCode::LD_DA ) ||
         ( o.getOpCode() == TC13::OpCode::LD_H ) ||
         ( o.getOpCode() == TC13::OpCode::LD_HU ) ||
         ( o.getOpCode() == TC13::OpCode::LD_Q ) ||
         ( o.getOpCode() == TC13::OpCode::LD_W ) ) &&
       ( ( o.getOperationFormat() == TC13::OperationFormat::AAC10PIA ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::SAA_3 ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::DAC10PIA ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::SDA_3 ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::EAC10PIA ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::PAC10PIA ) ) ) {
    // Here, only the address arithmetics caused by post/pre-increment/decrement
    // addressing modes is modeled. Bit-reverse addressing or circular
    // addressing is not considered here.
    unsigned int i = 2;
    if ( ( o.getOperationFormat() == TC13::OperationFormat::SAA_3 ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::SDA_3 ) )
      i = 1;
    auto &b = nthOp( i );
    auto &r = nthRes( i );
    r.setSignedness( false );

    i = 0;
    if ( o.getOperationFormat() == TC13::OperationFormat::SAA_3 )
      i = 4;
    if ( ( o.getOperationFormat() == TC13::OperationFormat::SDA_3 ) &&
         ( o.getOpCode() == TC13::OpCode::LD_BU ) )
      i = 1;
    if ( ( o.getOperationFormat() == TC13::OperationFormat::SDA_3 ) &&
         ( o.getOpCode() == TC13::OpCode::LD_H ) )
      i = 2;
    if ( ( o.getOperationFormat() == TC13::OperationFormat::SDA_3 ) &&
         ( o.getOpCode() == TC13::OpCode::LD_W ) )
      i = 4;

    WIR_UpDownValue incr =
      ( i != 0 ) ? WIR_UpDownValue { TC_Const10_Signed { i } } : lastOp();

    r = b + incr.extend( 32 );
  }

  if ( o.getOpCode() == TC13::OpCode::LD_BU ) {
    nthRes( 0 ) = { WIR_L4::b0, 32, false };
    WIR_UpDownValue u { WIR_L4::bU, 8, false };
    insert( nthRes( 0 ), u, 0 );
  }

  if ( o.getOpCode() == TC13::OpCode::LD_HU ) {
    nthRes( 0 ) = { WIR_L4::b0, 32, false };
    WIR_UpDownValue u { WIR_L4::bU, 16, false };
    insert( nthRes( 0 ), u, 0 );
  }

  if ( o.getOpCode() == TC13::OpCode::LD_Q ) {
    nthRes( 0 ) = { WIR_L4::b0, 32, true };
    WIR_UpDownValue u { WIR_L4::bU, 16, true };
    insert( nthRes( 0 ), u, 16 );
  }

  if ( ( o.getOpCode() == TC13::OpCode::LDMST ) &&
       ( o.getOperationFormat() == TC13::OperationFormat::PEBRA ) ) {
    // Here, only the address arithmetics caused by bit-reverse addressing is
    // modeled. Post/pre-increment/decrement or circular addressing is not
    // considered here.
    auto &b = firstOp();
    b.setSignedness( false );

    auto index = b.extract( 32, 16 );
    auto incr = b.extract( 48, 16 );
    auto new_index = reverse( reverse( index ) + reverse( incr ) );

    nthRes( 0 ) = b;
    insert( nthRes( 0 ), new_index, 32 );
    insert( nthRes( 0 ), incr, 48 );
  }

  if ( ( o.getOpCode() == TC13::OpCode::LDMST ) &&
       ( o.getOperationFormat() == TC13::OperationFormat::AC10EPIA ) )
    nthRes( 1 ) = nthOp( 1 ) + nthOp( 2 ).extend( 32 );

  if ( o.getOpCode() == TC13::OpCode::LEA ) {
    if ( o.getOperationFormat() == TC13::OperationFormat::AALC16BOA ) {
      // Check the particular constallation
      //   LEA    aReg2, [aReg1]LO:label

      auto it = o.getExplicitParameters().rbegin();
      auto &label =
        dynamic_cast<WIR_LabelParameter &>( std::next( it )->get() );

      if ( ( ( label.getLabelType() == WIR_SymbolType::data ) ||
             ( label.getLabelType() == WIR_SymbolType::function ) ) &&
           ( lastOp().getSignedValue() == 0 ) ) {
        auto &f = o.getInstruction().getBasicBlock().getFunction();
        auto &sys = f.getCompilationUnit().getSystem();
        auto &sym =
          label.getLabelType() == WIR_SymbolType::data ?
            sys.findSymbol( label.getData() ) :
            sys.findSymbol( label.getFunction() );

        WIR_UpDownValue tmp { WIR_L4::b0, 32 };

        for ( unsigned int i = 0; i < 16; ++i )
          tmp.setBit( i, WIR_L4::bL, { sym, i } );

        nthRes( 0 ) = nthOp( 1 ) + tmp;
      }
    } else

    if ( o.getOperationFormat() == TC13::OperationFormat::AC18ABSA )
      // Note the semantics of 18-bit absolute offsets as documented in class
      // TC_Const18_Unsigned!
      nthRes( 0 ) = lastOp();
    else

    if ( ( o.getOperationFormat() == TC13::OperationFormat::AAC10BOA ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::AAC16BOA ) )
      nthRes( 0 ) = nthOp( 1 ) + lastOp().extend( 32 );

    nthRes( 0 ).setSignedness( false );
  }


  //
  // LOOP
  //

  if ( o.getOpCode() == TC13::OpCode::LOOP )
    nthRes( 0 ) =
      firstOp() - WIR_UpDownValue( TC_Const4_Signed { 1 } ).extend( 32 );


  //
  // LT & co.
  //

  if ( ( o.getOpCode() == TC13::OpCode::LT ) ||
       ( o.getOpCode() == TC13::OpCode::LT_A ) ||
       ( o.getOpCode() == TC13::OpCode::LT_U ) ) {
    auto &a = nthOp( 1 );
    a.setSignedness( o.getOpCode() == TC13::OpCode::LT );
    auto &b = lastOp();
    b.setSignedness( o.getOpCode() == TC13::OpCode::LT );

    nthRes( 0 ) = { WIR_L4::b0, 32, false };

    if ( ( ( o.getOperationFormat() == TC13::OperationFormat::DDD_1 ) ||
           ( o.getOperationFormat() == TC13::OperationFormat::DAA ) ) &&
         ( nthReg( 1 ) == lastReg() ) )
      // Special case: LT Dc, Da, Da = 0
      nthRes( 0 )[ 0 ] = WIR_L4::b0;
    else
      nthRes( 0 )[ 0 ] = ( a < b.extend( 32 ) );
  }

  if ( ( o.getOpCode() == TC13::OpCode::LT_B ) ||
       ( o.getOpCode() == TC13::OpCode::LT_BU ) ) {
    if ( nthReg( 1 ) == lastReg() )
      // Special case: LT.B Dc, Da, Da = 0
      nthRes( 0 ) = { WIR_L4::b0, 32, false };
    else
      for ( auto bitPos: bytePos ) {
        auto a = nthOp( 1 ).extract( bitPos, 8 );
        a.setSignedness( o.getOpCode() == TC13::OpCode::LT_B );
        auto b = lastOp().extract( bitPos, 8 );
        b.setSignedness( o.getOpCode() == TC13::OpCode::LT_B );

        insert( nthRes( 0 ), WIR_UpDownValue( a < b, 1, true ).extend( 8 ),
                bitPos );
      }
  }

  if ( ( o.getOpCode() == TC13::OpCode::LT_H ) ||
       ( o.getOpCode() == TC13::OpCode::LT_HU ) ) {
    if ( nthReg( 1 ) == lastReg() )
      // Special case: LT.H Dc, Da, Da = 0
      nthRes( 0 ) = { WIR_L4::b0, 32, false };
    else
      for ( auto bitPos: halfwordPos ) {
        auto a = nthOp( 1 ).extract( bitPos, 16 );
        a.setSignedness( o.getOpCode() == TC13::OpCode::LT_H );
        auto b = lastOp().extract( bitPos, 16 );
        b.setSignedness( o.getOpCode() == TC13::OpCode::LT_H );

        insert( nthRes( 0 ), WIR_UpDownValue( a < b, 1, true ).extend( 16 ),
                bitPos );
      }
  }

  if ( ( o.getOpCode() == TC13::OpCode::LT_W ) ||
       ( o.getOpCode() == TC13::OpCode::LT_WU ) ) {
    auto &a = nthOp( 1 );
    a.setSignedness( o.getOpCode() == TC13::OpCode::LT_W );
    auto &b = lastOp();
    b.setSignedness( o.getOpCode() == TC13::OpCode::LT_W );

    if ( nthReg( 1 ) == lastReg() )
      // Special case: LT.W Dc, Da, Da = 0
      nthRes( 0 ) = { WIR_L4::b0, 32, false };
    else
      nthRes( 0 ) = WIR_UpDownValue( a < b, 1, true ).extend( 32 );
  }


  //
  // MADD*/MSUB* & co.
  //

  if ( ( o.getOpCode() == TC13::OpCode::MADD ) ||
       ( o.getOpCode() == TC13::OpCode::MADDS ) ||
       ( o.getOpCode() == TC13::OpCode::MADD_U ) ||
       ( o.getOpCode() == TC13::OpCode::MADDS_U ) ) {
    auto &d = nthOp( 1 );
    d.setSignedness(
      ( o.getOpCode() == TC13::OpCode::MADD ) ||
      ( o.getOpCode() == TC13::OpCode::MADDS ) );
    auto &a = nthOp( 2 );
    a.setSignedness( d.isSigned() );
    auto &b = lastOp();
    b.setSignedness( d.isSigned() );
    auto &r = nthRes( 0 );

    WIR_UpDownValue prod { WIR_L4::bU, d.getBitWidth(), d.isSigned() };

    // If one of the two factors is 0, the product is also 0.
    if ( a.containsOnlyBit( WIR_L4::b0 ) || b.containsOnlyBit( WIR_L4::b0 ) )
      prod.setAllBits( WIR_L4::b0 );
    else

    // If one of the two factors is 1, the product is equal to the other factor.
    if ( ( a[ 0 ] == WIR_L4::b1 ) &&
         a.extract( 1, a.getBitWidth() - 1 ).containsOnlyBit( WIR_L4::b0 ) )
      prod = b.extend( prod.getBitWidth() );
    else

    if ( ( b[ 0 ] == WIR_L4::b1 ) &&
         b.extract( 1, b.getBitWidth() - 1 ).containsOnlyBit( WIR_L4::b0 ) )
      prod = a.extend( prod.getBitWidth() );

    r = d + prod;
    r.setSignedness( d.isSigned() );
  }

  if ( ( o.getOpCode() == TC13::OpCode::MSUB ) ||
       ( o.getOpCode() == TC13::OpCode::MSUBS ) ||
       ( o.getOpCode() == TC13::OpCode::MSUB_U ) ||
       ( o.getOpCode() == TC13::OpCode::MSUBS_U ) ) {
    auto &d = nthOp( 1 );
    d.setSignedness(
      ( o.getOpCode() == TC13::OpCode::MSUB ) ||
      ( o.getOpCode() == TC13::OpCode::MSUBS ) );
    auto &a = nthOp( 2 );
    a.setSignedness( d.isSigned() );
    auto &b = lastOp();
    b.setSignedness( d.isSigned() );
    auto &r = nthRes( 0 );

    WIR_UpDownValue prod { WIR_L4::bU, d.getBitWidth(), d.isSigned() };

    // If one of the two factors is 0, the product is also 0.
    if ( a.containsOnlyBit( WIR_L4::b0 ) || b.containsOnlyBit( WIR_L4::b0 ) )
      prod.setAllBits( WIR_L4::b0 );
    else

    // If one of the two factors is 1, the product is equal to the other factor.
    if ( ( a[ 0 ] == WIR_L4::b1 ) &&
         a.extract( 1, a.getBitWidth() - 1 ).containsOnlyBit( WIR_L4::b0 ) )
      prod = b.extend( prod.getBitWidth() );
    else

    if ( ( b[ 0 ] == WIR_L4::b1 ) &&
         b.extract( 1, b.getBitWidth() - 1 ).containsOnlyBit( WIR_L4::b0 ) )
      prod = a.extend( prod.getBitWidth() );

    r = d - prod;
    r.setSignedness( d.isSigned() );
  }


  //
  // MAX/MIN & co.
  //

  if ( ( o.getOpCode() == TC13::OpCode::MAX ) ||
       ( o.getOpCode() == TC13::OpCode::MAX_U ) ) {
    auto &a = nthOp( 1 );
    a.setSignedness( o.getOpCode() == TC13::OpCode::MAX );
    auto &b = lastOp();
    b.setSignedness( o.getOpCode() == TC13::OpCode::MAX );

    auto res = ( b.extend( 32 ) < a );
    if ( res == WIR_L4::b1 )
      nthRes( 0 ) = a;
    else

    if ( res == WIR_L4::b0 )
      nthRes( 0 ) = b.extend( 32 );

    nthRes( 0 ).setSignedness( o.getOpCode() == TC13::OpCode::MAX );
  }

  if ( ( o.getOpCode() == TC13::OpCode::MAX_B ) ||
       ( o.getOpCode() == TC13::OpCode::MAX_BU ) ) {
    for ( auto bitPos : bytePos ) {
      auto a = nthOp( 1 ).extract( bitPos, 8 );
      a.setSignedness( o.getOpCode() == TC13::OpCode::MAX_B );
      auto b = lastOp().extract( bitPos, 8 );
      b.setSignedness( o.getOpCode() == TC13::OpCode::MAX_B );

      auto res = ( b < a );
      if ( res == WIR_L4::b1 )
        insert( nthRes( 0 ), a, bitPos );
      else

      if ( res == WIR_L4::b0 )
        insert( nthRes( 0 ), b, bitPos );
    }

    nthRes( 0 ).setSignedness( o.getOpCode() == TC13::OpCode::MAX_B );
  }

  if ( ( o.getOpCode() == TC13::OpCode::MAX_H ) ||
       ( o.getOpCode() == TC13::OpCode::MAX_HU ) ) {
    for ( auto bitPos : halfwordPos ) {
      auto a = nthOp( 1 ).extract( bitPos, 16 );
      a.setSignedness( o.getOpCode() == TC13::OpCode::MAX_H );
      auto b = lastOp().extract( bitPos, 16 );
      b.setSignedness( o.getOpCode() == TC13::OpCode::MAX_H );

      auto res = ( b < a );
      if ( res == WIR_L4::b1 )
        insert( nthRes( 0 ), a, bitPos );
      else

      if ( res == WIR_L4::b0 )
        insert( nthRes( 0 ), b, bitPos );
    }

    nthRes( 0 ).setSignedness( o.getOpCode() == TC13::OpCode::MAX_H );
  }

  if ( ( o.getOpCode() == TC13::OpCode::MIN ) ||
       ( o.getOpCode() == TC13::OpCode::MIN_U ) ) {
    auto &a = nthOp( 1 );
    a.setSignedness( o.getOpCode() == TC13::OpCode::MIN );
    auto &b = lastOp();
    b.setSignedness( o.getOpCode() == TC13::OpCode::MIN );

    auto res = ( a < b.extend( 32 ) );
    if ( res == WIR_L4::b1 )
      nthRes( 0 ) = a;
    else

    if ( res == WIR_L4::b0 )
      nthRes( 0 ) = b.extend( 32 );

    nthRes( 0 ).setSignedness( o.getOpCode() == TC13::OpCode::MIN );
  }

  if ( ( o.getOpCode() == TC13::OpCode::MIN_B ) ||
       ( o.getOpCode() == TC13::OpCode::MIN_BU ) ) {
    for ( auto bitPos : bytePos ) {
      auto a = nthOp( 1 ).extract( bitPos, 8 );
      a.setSignedness( o.getOpCode() == TC13::OpCode::MIN_B );
      auto b = lastOp().extract( bitPos, 8 );
      b.setSignedness( o.getOpCode() == TC13::OpCode::MIN_B );

      auto res = ( a < b );
      if ( res == WIR_L4::b1 )
        insert( nthRes( 0 ), a, bitPos );
      else

      if ( res == WIR_L4::b0 )
        insert( nthRes( 0 ), b, bitPos );
    }

    nthRes( 0 ).setSignedness( o.getOpCode() == TC13::OpCode::MIN_B );
  }

  if ( ( o.getOpCode() == TC13::OpCode::MIN_H ) ||
       ( o.getOpCode() == TC13::OpCode::MIN_HU ) ) {
    for ( auto bitPos : halfwordPos ) {
      auto a = nthOp( 1 ).extract( bitPos, 16 );
      a.setSignedness( o.getOpCode() == TC13::OpCode::MIN_H );
      auto b = lastOp().extract( bitPos, 16 );
      b.setSignedness( o.getOpCode() == TC13::OpCode::MIN_H );

      auto res = ( a < b );
      if ( res == WIR_L4::b1 )
        insert( nthRes( 0 ), a, bitPos );
      else

      if ( res == WIR_L4::b0 )
        insert( nthRes( 0 ), b, bitPos );
    }

    nthRes( 0 ).setSignedness( o.getOpCode() == TC13::OpCode::MIN_H );
  }


  //
  // MOV & co.
  //

  if ( ( o.getOpCode() == TC13::OpCode::MOV ) ||
       ( o.getOpCode() == TC13::OpCode::MOV_RR ) ||
       ( o.getOpCode() == TC13::OpCode::MOV_A ) ||
       ( o.getOpCode() == TC13::OpCode::MOV_AA ) ||
       ( o.getOpCode() == TC13::OpCode::MOV_D ) ||
       ( o.getOpCode() == TC13::OpCode::MOV_U ) ) {
    auto &b = lastOp();
    if ( ( o.getOperationFormat() == TC13::OperationFormat::SIC8_1 ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::SAC4_1 ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::DC16_2 ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::SAA_1 ) ||
         ( o.getOpCode() == TC13::OpCode::MOV_A ) ||
         ( o.getOpCode() == TC13::OpCode::MOV_AA ) ||
         ( o.getOpCode() == TC13::OpCode::MOV_D ) )
      b.setSignedness( false );
    else
      b.setSignedness( true );

    nthRes( 0 ) = b.extend( 32 );
  }

  if ( o.getOpCode() == TC13::OpCode::MOVH )
    nthRes( 0 ) = lastOp().extend( 32 ) << 16;

  if ( o.getOpCode() == TC13::OpCode::MOVH_A ) {
    auto &r = nthRes( 0 );

    if ( o.getOperationFormat() == TC13::OperationFormat::AL_1 ) {
      // Check the particular constallation
      //   MOVH.A aReg1, HI:label
      auto &label =
        dynamic_cast<WIR_LabelParameter &>(
          o.getExplicitParameters().back().get() );

      if ( ( label.getLabelType() == WIR_SymbolType::data ) ||
           ( label.getLabelType() == WIR_SymbolType::function ) ) {
        auto &f = o.getInstruction().getBasicBlock().getFunction();
        auto &sys = f.getCompilationUnit().getSystem();
        auto &sym =
          label.getLabelType() == WIR_SymbolType::data ?
            sys.findSymbol( label.getData() ) :
            sys.findSymbol( label.getFunction() );

        r.setAllBits( WIR_L4::b0 );
        for ( unsigned int i = 16; i < 32; ++i )
          r.setBit( i, WIR_L4::bL, { sym, i } );
      }
    } else
      r = lastOp().extend( 32 ) << 16;
  }


  //
  // MUL & co.
  //

  if ( ( o.getOpCode() == TC13::OpCode::MUL ) ||
       ( o.getOpCode() == TC13::OpCode::MULS ) ||
       ( o.getOpCode() == TC13::OpCode::MUL_U ) ||
       ( o.getOpCode() == TC13::OpCode::MULS_U ) ) {
    unsigned int i = 1;
    if ( o.getOperationFormat() == TC13::OperationFormat::SDD_2 )
      i = 0;
    auto &a = nthOp( i );
    a.setSignedness(
      ( o.getOpCode() == TC13::OpCode::MUL ) ||
      ( o.getOpCode() == TC13::OpCode::MULS ) );
    auto &b = lastOp();
    b.setSignedness( a.isSigned() );
    auto &r = nthRes( 0 );

    // If one of the two factors is 0, the result is also 0.
    if ( b.containsOnlyBit( WIR_L4::b0 ) || a.containsOnlyBit( WIR_L4::b0 ) )
      r.setAllBits( WIR_L4::b0 );
    else

    // If one of the two factors is 1, the result can be set to the other
    // factor.
    if ( ( a[ 0 ] == WIR_L4::b1 ) &&
         a.extract( 1, a.getBitWidth() - 1 ).containsOnlyBit( WIR_L4::b0 ) )
      r = b.extend( r.getBitWidth() );
    else

    if ( ( b[ 0 ] == WIR_L4::b1 ) &&
         b.extract( 1, b.getBitWidth() - 1 ).containsOnlyBit( WIR_L4::b0 ) )
      r = a.extend( r.getBitWidth() );
  }


  //
  // NAND & co.
  //

  if ( o.getOpCode() == TC13::OpCode::NAND ) {
    auto &b = lastOp();
    b.setSignedness( false );

    nthRes( 0 ) = ~( nthOp( 1 ) & b.extend( 32 ) );
    nthRes( 0 ).setSignedness( false );
  }

  if ( o.getOpCode() == TC13::OpCode::NAND_T ) {
    auto b1 = nthOp( 1 ).extract( nthOp( 2 ).getSignedValue(), 1 );
    auto b2 = nthOp( 3 ).extract( lastOp().getSignedValue(), 1 );

    nthRes( 0 ).setAllBits( WIR_L4::b0 );
    insert( nthRes( 0 ), ~( b1 & b2 ), 0 );
    nthRes( 0 ).setSignedness( false );
  }


  //
  // NE & co.
  //

  if ( ( o.getOpCode() == TC13::OpCode::NE ) ||
       ( o.getOpCode() == TC13::OpCode::NE_A ) ) {
    auto &a = nthOp( 1 );
    auto &b = lastOp();
    b.setSignedness();

    nthRes( 0 ) = { WIR_L4::b0, 32, false };

    if ( ( ( o.getOperationFormat() == TC13::OperationFormat::DDD_1 ) ||
           ( o.getOperationFormat() == TC13::OperationFormat::DAA ) ) &&
         ( nthReg( 1 ) == lastReg() ) )
      // Special case: NE Dc, Da, Da = 0
      nthRes( 0 )[ 0 ] = WIR_L4::b0;
    else
      nthRes( 0 )[ 0 ] = ( a != b.extend( 32 ) );
  }

  if ( o.getOpCode() == TC13::OpCode::NEZ_A ) {
    nthRes( 0 ) = { WIR_L4::b0, 32, false };
    nthRes( 0 )[ 0 ] =
      ( lastOp() != WIR_UpDownValue { WIR_L4::b0, 32, false } );
  }


  //
  // NOR & co.
  //

  if ( o.getOpCode() == TC13::OpCode::NOR ) {
    auto &b = lastOp();
    b.setSignedness( false );

    nthRes( 0 ) = ~( nthOp( 1 ) | b.extend( 32 ) );
    nthRes( 0 ).setSignedness( false );
  }

  if ( o.getOpCode() == TC13::OpCode::NOR_T ) {
    auto b1 = nthOp( 1 ).extract( nthOp( 2 ).getSignedValue(), 1 );
    auto b2 = nthOp( 3 ).extract( lastOp().getSignedValue(), 1 );

    nthRes( 0 ).setAllBits( WIR_L4::b0 );
    insert( nthRes( 0 ), ~( b1 | b2 ), 0 );
    nthRes( 0 ).setSignedness( false );
  }


  //
  // NOT
  //

  if ( o.getOpCode() == TC13::OpCode::NOT ) {
    nthRes( 0 ) = ~firstOp();
    nthRes( 0 ).setSignedness( false );
  }


  //
  // OR & co.
  //

  if ( o.getOpCode() == TC13::OpCode::OR ) {
    auto &b = lastOp();
    b.setSignedness( false );
    auto &r = nthRes( 0 );

    if ( ( o.getOperationFormat() == TC13::OperationFormat::SIC8_2 ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::SDD_2 ) )
      r = firstOp() | b.extend( 32 );
    else
      r = nthOp( 1 ) | b.extend( 32 );
    r.setSignedness( false );
  }

  if ( o.getOpCode() == TC13::OpCode::OR_AND_T ) {
    auto b1 = nthOp( 1 ).extract( nthOp( 2 ).getSignedValue(), 1 );
    auto b2 = nthOp( 3 ).extract( lastOp().getSignedValue(), 1 );
    auto &r = nthRes( 0 );

    r = firstOp();
    insert( r, r.extract( 0, 1 ) | ( b1 & b2 ), 0 );
    r.setSignedness( false );
  }

  if ( o.getOpCode() == TC13::OpCode::OR_ANDN_T ) {
    auto b1 = nthOp( 1 ).extract( nthOp( 2 ).getSignedValue(), 1 );
    auto b2 = nthOp( 3 ).extract( lastOp().getSignedValue(), 1 );
    auto &r = nthRes( 0 );

    r = firstOp();
    insert( r, r.extract( 0, 1 ) | ( b1 & ~b2 ), 0 );
    r.setSignedness( false );
  }

  if ( o.getOpCode() == TC13::OpCode::OR_EQ ) {
    auto &b = lastOp();
    b.setSignedness();
    auto &r = nthRes( 0 );

    WIR_L4 cmp = ( nthOp( 1 ) == b.extend( 32 ) );

    if ( ( o.getOperationFormat() == TC13::OperationFormat::DDD_2 ) &&
         ( nthReg( 1 ) == lastReg() ) )
      // Special case: OR.EQ Dc, Da, Da = Dc[31:1] 1
      cmp = WIR_L4::b1;

    r = firstOp();
    insert( r, r.extract( 0, 1 ) | WIR_UpDownValue { cmp, 1, false }, 0 );
    r.setSignedness( false );
  }

  if ( ( o.getOpCode() == TC13::OpCode::OR_GE ) ||
       ( o.getOpCode() == TC13::OpCode::OR_GE_U ) ) {
    auto &a = nthOp( 1 );
    auto &b = lastOp();
    a.setSignedness( o.getOpCode() == TC13::OpCode::OR_GE );
    b.setSignedness( o.getOpCode() == TC13::OpCode::OR_GE );
    auto &r = nthRes( 0 );

    WIR_L4 cmp = ( a >= b.extend( 32 ) );

    if ( ( o.getOperationFormat() == TC13::OperationFormat::DDD_2 ) &&
         ( nthReg( 1 ) == lastReg() ) )
      // Special case: OR.GE Dc, Da, Da = Dc[31:1] 1
      cmp = WIR_L4::b1;

    r = firstOp();
    insert( r, r.extract( 0, 1 ) | WIR_UpDownValue { cmp, 1, false }, 0 );
    r.setSignedness( false );
  }

  if ( ( o.getOpCode() == TC13::OpCode::OR_LT ) ||
       ( o.getOpCode() == TC13::OpCode::OR_LT_U ) ) {
    auto &a = nthOp( 1 );
    auto &b = lastOp();
    a.setSignedness( o.getOpCode() == TC13::OpCode::OR_LT );
    b.setSignedness( o.getOpCode() == TC13::OpCode::OR_LT );
    auto &r = nthRes( 0 );

    WIR_L4 cmp = ( a < b.extend( 32 ) );

    if ( ( o.getOperationFormat() == TC13::OperationFormat::DDD_2 ) &&
         ( nthReg( 1 ) == lastReg() ) )
      // Special case: OR.LT Dc, Da, Da = Dc
      cmp = WIR_L4::b0;

    r = firstOp();
    insert( r, r.extract( 0, 1 ) | WIR_UpDownValue { cmp, 1, false }, 0 );
    r.setSignedness( false );
  }

  if ( o.getOpCode() == TC13::OpCode::OR_NE ) {
    auto &b = lastOp();
    b.setSignedness();
    auto &r = nthRes( 0 );

    WIR_L4 cmp = ( nthOp( 1 ) != b.extend( 32 ) );

    if ( ( o.getOperationFormat() == TC13::OperationFormat::DDD_2 ) &&
         ( nthReg( 1 ) == lastReg() ) )
      // Special case: OR.NE Dc, Da, Da = Dc
      cmp = WIR_L4::b0;

    r = firstOp();
    insert( r, r.extract( 0, 1 ) | WIR_UpDownValue { cmp, 1, false }, 0 );
    r.setSignedness( false );
  }

  if ( o.getOpCode() == TC13::OpCode::OR_NOR_T ) {
    auto b1 = nthOp( 1 ).extract( nthOp( 2 ).getSignedValue(), 1 );
    auto b2 = nthOp( 3 ).extract( lastOp().getSignedValue(), 1 );
    auto &r = nthRes( 0 );

    r = firstOp();
    insert( r, r.extract( 0, 1 ) | ~( b1 | b2 ), 0 );
    r.setSignedness( false );
  }

  if ( o.getOpCode() == TC13::OpCode::OR_OR_T ) {
    auto b1 = nthOp( 1 ).extract( nthOp( 2 ).getSignedValue(), 1 );
    auto b2 = nthOp( 3 ).extract( lastOp().getSignedValue(), 1 );
    auto &r = nthRes( 0 );

    r = firstOp();
    insert( r, r.extract( 0, 1 ) | ( b1 | b2 ), 0 );
    r.setSignedness( false );
  }

  if ( o.getOpCode() == TC13::OpCode::OR_T ) {
    auto b1 = nthOp( 1 ).extract( nthOp( 2 ).getSignedValue(), 1 );
    auto b2 = nthOp( 3 ).extract( lastOp().getSignedValue(), 1 );

    nthRes( 0 ).setAllBits( WIR_L4::b0 );
    insert( nthRes( 0 ), b1 | b2, 0 );
  }

  if ( o.getOpCode() == TC13::OpCode::ORN ) {
    auto &b = lastOp();
    b.setSignedness( false );
    auto &r = nthRes( 0 );

    if ( ( o.getOperationFormat() == TC13::OperationFormat::DDD_1 ) &&
         ( nthReg( 1 ) == lastReg() ) )
      // Special case: ORN Dc, Da, Da = 0xFFFFFFFF
      r = { WIR_L4::b1, 32, true };
    else
      r = nthOp( 1 ) | ~( b.extend( 32 ) );

    r.setSignedness( false );
  }

  if ( o.getOpCode() == TC13::OpCode::ORN_T ) {
    auto b1 = nthOp( 1 ).extract( nthOp( 2 ).getSignedValue(), 1 );
    auto b2 = nthOp( 3 ).extract( lastOp().getSignedValue(), 1 );
    auto &r = nthRes( 0 );

    r.setAllBits( WIR_L4::b0 );
    insert( r, b1 | ~b2, 0 );
    r.setSignedness( false );
  }


  //
  // PARITY
  //

  if ( o.getOpCode() == TC13::OpCode::PARITY ) {
    auto &a = lastOp();
    nthRes( 0 ).setAllBits( WIR_L4::b0 );

    for ( auto bitPos : bytePos ) {
      auto res =
        a.extract( bitPos, 1 ) ^ a.extract( bitPos + 1, 1 ) ^
        a.extract( bitPos + 2, 1 ) ^ a.extract( bitPos + 3, 1 ) ^
        a.extract( bitPos + 4, 1 ) ^ a.extract( bitPos + 5, 1 ) ^
        a.extract( bitPos + 6, 1 ) ^ a.extract( bitPos + 7, 1 );

      insert( nthRes( 0 ), res, bitPos );
    }
  }


  //
  // RSUB & co.
  //

  if ( o.getOpCode() == TC13::OpCode::RSUB ) {
    if ( o.getOperationFormat() == TC13::OperationFormat::DDC9_1 ) {
      auto &b = lastOp();
      b.setSignedness();

      nthRes( 0 ) = b.extend( 32 ) - nthOp( 1 );
    } else
      nthRes( 0 ) =
        WIR_UpDownValue( TC_Const4_Signed { 0 } ).extend( 32 ) - firstOp();
  }

  if ( o.getOpCode() == TC13::OpCode::RSUBS ) {
    auto &a = nthOp( 1 );
    auto &b = lastOp();
    a.setSignedness();
    b.setSignedness();

    auto res = b.extend( 33 ) - a.extend( 33 );
    res.setSignedness();
    nthRes( 0 ) = ssov( res, 32 );
  }

  if ( o.getOpCode() == TC13::OpCode::RSUBS_U ) {
    auto &a = nthOp( 1 );
    auto &b = lastOp();
    a.setSignedness( false );
    b.setSignedness();

    // RSUBS.U is so crazy: The 2nd summand is sign-extended (!), followed by an
    // unsigned (!) subtraction with final saturation on unsigned (!) overflow.
    WIR_UpDownValue tmp = b.extend( 32 );
    tmp.setSignedness( false );

    WIR_UpDownValue res = tmp.extend( 33 ) - a.extend( 33 );
    res.setSignedness();
    nthRes( 0 ) = suov( res, 32 );
  }


  //
  // SAT & co.
  //

  if ( o.getOpCode() == TC13::OpCode::SAT_B ) {
    auto &a = lastOp();
    a.setSignedness();
    auto &r = nthRes( 0 );

    auto cmp1 = ( a < WIR_UpDownValue( TC_Const16_Signed { -0x80 } ) );
    auto cmp2 = ( WIR_UpDownValue( TC_Const16_Signed { 0x7F } ) < a );

    if ( cmp1 == WIR_L4::b1 )
      r = WIR_UpDownValue( TC_Const16_Signed { -0x80 } ).extend( 32 );
    else

    if ( cmp2 == WIR_L4::b1 )
      r = WIR_UpDownValue( TC_Const16_Signed { 0x7F } ).extend( 32 );
    else

    if ( ( cmp1 == WIR_L4::b0 ) && ( cmp2 == WIR_L4::b0 ) )
      r = a;
  }

  if ( o.getOpCode() == TC13::OpCode::SAT_BU ) {
    auto &a = lastOp();
    a.setSignedness( false );
    auto &r = nthRes( 0 );

    auto cmp1 = ( WIR_UpDownValue( TC_Const16_Unsigned { 0xFF } ) < a );

    if ( cmp1 == WIR_L4::b1 )
      r = WIR_UpDownValue( TC_Const16_Unsigned { 0xFF } ).extend( 32 );
    else

    if ( cmp1 == WIR_L4::b0 )
      r = a;
  }

  if ( o.getOpCode() == TC13::OpCode::SAT_H ) {
    auto &a = lastOp();
    a.setSignedness();
    auto &r = nthRes( 0 );

    auto cmp1 = ( a < WIR_UpDownValue( TC_Const16_Signed { -0x8000 } ) );
    auto cmp2 = ( WIR_UpDownValue( TC_Const16_Signed { 0x7FFF } ) < a );

    if ( cmp1 == WIR_L4::b1 )
      r = WIR_UpDownValue( TC_Const16_Signed { -0x8000 } ).extend( 32 );
    else

    if ( cmp2 == WIR_L4::b1 )
      r = WIR_UpDownValue( TC_Const16_Signed { 0x7FFF } ).extend( 32 );
    else

    if ( ( cmp1 == WIR_L4::b0 ) && ( cmp2 == WIR_L4::b0 ) )
      r = a;
  }

  if ( o.getOpCode() == TC13::OpCode::SAT_HU ) {
    auto &a = lastOp();
    a.setSignedness( false );
    auto &r = nthRes( 0 );

    auto cmp1 = ( WIR_UpDownValue( TC_Const16_Unsigned { 0xFFFF } ) < a );

    if ( cmp1 == WIR_L4::b1 )
      r = WIR_UpDownValue( TC_Const16_Unsigned { 0xFFFF } ).extend( 32 );
    else

    if ( cmp1 == WIR_L4::b0 )
      r = a;
  }


  //
  // SEL & co.
  //

  if ( o.getOpCode() == TC13::OpCode::SEL ) {
    auto &b = lastOp();
    b.setSignedness();

    auto &cond = nthOp( 1 );

    if ( cond.containsBits( { WIR_L4::b1, WIR_L4::bX } ) )
      nthRes( 0 ) = nthOp( 2 );
    else

    if ( cond.containsOnlyBit( WIR_L4::b0 ) )
      nthRes( 0 ) = b.extend( 32 );
  }

  if ( o.getOpCode() == TC13::OpCode::SELN ) {
    auto &b = lastOp();
    b.setSignedness();

    auto &cond = nthOp( 1 );

    if ( cond.containsOnlyBits( { WIR_L4::b0, WIR_L4::bX } ) )
      nthRes( 0 ) = nthOp( 2 );
    else

    if ( cond.containsBit( WIR_L4::b1 ) )
      nthRes( 0 ) = b.extend( 32 );
  }


  //
  // SH & co.
  //

  if ( o.getOpCode() == TC13::OpCode::SH ) {
    auto &b = lastOp();
    b.setSignedness();
    auto shamt = ( b.getBitWidth() == 4 ) ? b.extend( 6 ) : b.extract( 0, 6 );

    auto &a =
      ( o.getOperationFormat() == TC13::OperationFormat::SDC4_2 ) ?
        firstOp() : nthOp( 1 );
    a.setSignedness( false );

    nthRes( 0 ) = a << shamt;
  }

  if ( o.getOpCode() == TC13::OpCode::SH_AND_T ) {
    auto b1 = nthOp( 1 ).extract( nthOp( 2 ).getSignedValue(), 1 );
    auto b2 = nthOp( 3 ).extract( lastOp().getSignedValue(), 1 );

    nthRes( 0 ) = firstOp() << 1;
    insert( nthRes( 0 ), b1 & b2, 0 );
  }

  if ( o.getOpCode() == TC13::OpCode::SH_ANDN_T ) {
    auto b1 = nthOp( 1 ).extract( nthOp( 2 ).getSignedValue(), 1 );
    auto b2 = nthOp( 3 ).extract( lastOp().getSignedValue(), 1 );

    nthRes( 0 ) = firstOp() << 1;
    insert( nthRes( 0 ), b1 & ~b2, 0 );
  }

  if ( o.getOpCode() == TC13::OpCode::SH_EQ ) {
    nthRes( 0 ) = firstOp() << 1;

    if ( ( o.getOperationFormat() == TC13::OperationFormat::DDD_2 ) &&
         ( nthReg( 1 ) == lastReg() ) )
      // Special case: SH.EQ Dc, Da, Da
      nthRes( 0 )[ 0 ] = WIR_L4::b1;
    else
      nthRes( 0 )[ 0 ] = ( nthOp( 1 ) == lastOp().extend( 32 ) );
  }

  if ( ( o.getOpCode() == TC13::OpCode::SH_GE ) ||
       ( o.getOpCode() == TC13::OpCode::SH_GE_U ) ) {
    auto &a = nthOp( 1 );
    a.setSignedness( o.getOpCode() == TC13::OpCode::SH_GE );
    auto &b = lastOp();
    b.setSignedness( o.getOpCode() == TC13::OpCode::SH_GE );

    nthRes( 0 ) = firstOp() << 1;

    if ( ( o.getOperationFormat() == TC13::OperationFormat::DDD_2 ) &&
         ( nthReg( 1 ) == lastReg() ) )
      // Special case: SH.GE Dc, Da, Da
      nthRes( 0 )[ 0 ] = WIR_L4::b1;
    else
      nthRes( 0 )[ 0 ] = ( a >= b.extend( 32 ) );
  }

  if ( o.getOpCode() == TC13::OpCode::SH_H ) {
    auto &a = nthOp( 1 );
    a.setSignedness( false );
    auto &b = lastOp();
    b.setSignedness();
    auto shamt = b.extract( 0, 5 );

    for ( auto bitPos : halfwordPos )
      insert( nthRes( 0 ), a.extract( bitPos, 16 ) << shamt, bitPos );
  }

  if ( ( o.getOpCode() == TC13::OpCode::SH_LT ) ||
       ( o.getOpCode() == TC13::OpCode::SH_LT_U ) ) {
    auto &a = nthOp( 1 );
    a.setSignedness( o.getOpCode() == TC13::OpCode::SH_LT );
    auto &b = lastOp();
    b.setSignedness( o.getOpCode() == TC13::OpCode::SH_LT );

    nthRes( 0 ) = firstOp() << 1;

    if ( ( o.getOperationFormat() == TC13::OperationFormat::DDD_2 ) &&
         ( nthReg( 1 ) == lastReg() ) )
      // Special case: SH.LT Dc, Da, Da
      nthRes( 0 )[ 0 ] = WIR_L4::b0;
    else
      nthRes( 0 )[ 0 ] = ( a < b.extend( 32 ) );
  }

  if ( o.getOpCode() == TC13::OpCode::SH_NAND_T ) {
    auto b1 = nthOp( 1 ).extract( nthOp( 2 ).getSignedValue(), 1 );
    auto b2 = nthOp( 3 ).extract( lastOp().getSignedValue(), 1 );

    nthRes( 0 ) = firstOp() << 1;
    insert( nthRes( 0 ), ~( b1 & b2 ), 0 );
  }

  if ( o.getOpCode() == TC13::OpCode::SH_NE ) {
    nthRes( 0 ) = firstOp() << 1;

    if ( ( o.getOperationFormat() == TC13::OperationFormat::DDD_2 ) &&
         ( nthReg( 1 ) == lastReg() ) )
      // Special case: SH.NE Dc, Da, Da
      nthRes( 0 )[ 0 ] = WIR_L4::b0;
    else
      nthRes( 0 )[ 0 ] = ( nthOp( 1 ) != lastOp().extend( 32 ) );
  }

  if ( o.getOpCode() == TC13::OpCode::SH_NOR_T ) {
    auto b1 = nthOp( 1 ).extract( nthOp( 2 ).getSignedValue(), 1 );
    auto b2 = nthOp( 3 ).extract( lastOp().getSignedValue(), 1 );

    nthRes( 0 ) = firstOp() << 1;
    insert( nthRes( 0 ), ~( b1 | b2 ), 0 );
  }

  if ( o.getOpCode() == TC13::OpCode::SH_OR_T ) {
    auto b1 = nthOp( 1 ).extract( nthOp( 2 ).getSignedValue(), 1 );
    auto b2 = nthOp( 3 ).extract( lastOp().getSignedValue(), 1 );

    nthRes( 0 ) = firstOp() << 1;
    insert( nthRes( 0 ), b1 | b2, 0 );
  }

  if ( o.getOpCode() == TC13::OpCode::SH_ORN_T ) {
    auto b1 = nthOp( 1 ).extract( nthOp( 2 ).getSignedValue(), 1 );
    auto b2 = nthOp( 3 ).extract( lastOp().getSignedValue(), 1 );

    nthRes( 0 ) = firstOp() << 1;
    insert( nthRes( 0 ), b1 | ~b2, 0 );
  }

  if ( o.getOpCode() == TC13::OpCode::SH_XNOR_T ) {
    auto b1 = nthOp( 1 ).extract( nthOp( 2 ).getSignedValue(), 1 );
    auto b2 = nthOp( 3 ).extract( lastOp().getSignedValue(), 1 );

    nthRes( 0 ) = firstOp() << 1;
    insert( nthRes( 0 ), ~( b1 ^ b2 ), 0 );
  }

  if ( o.getOpCode() == TC13::OpCode::SH_XOR_T ) {
    auto b1 = nthOp( 1 ).extract( nthOp( 2 ).getSignedValue(), 1 );
    auto b2 = nthOp( 3 ).extract( lastOp().getSignedValue(), 1 );

    nthRes( 0 ) = firstOp() << 1;
    insert( nthRes( 0 ), b1 ^ b2, 0 );
  }

  if ( o.getOpCode() == TC13::OpCode::SHA ) {
    auto &a = ( o.getSize() == 2 ) ? firstOp() : nthOp( 1 );
    a.setSignedness();
    auto &b = nthOp( ( o.getSize() == 4 ) ? 2 : 1 );
    b.setSignedness();

    auto &r = nthRes( 0 );
    auto &carryOut = ( o.getSize() == 2 ) ? nthRes( 2 ) : nthRes( 3 );

    // Determine an integer shift amount if possible.
    auto shamt = ( b.getBitWidth() == 4 ) ? b.extend( 6 ) : b.extract( 0, 6 );

    // Determine whether shift amount is a positive or negative integer number.
    bool positiveShamt = ( shamt.at( shamt.getBitWidth() - 1 ) != WIR_L4::b1 );

    // Iterate the shift amount.
    bool skip = false;
    for ( unsigned int i = 0; i < shamt.getBitWidth(); ++i ) {
      // If we encounter a 'U', 'L' or 'N' bit in the shift amount, we skip.
      if ( getLevel( shamt.at( i ) ) <= 1 ) {
        skip = true;
        break;
      }

      // Replace 'X' either by '1' or '0', depending on whether the shift amount
      // is a positive number or not.
      if ( shamt.at( i ) == WIR_L4::bX ) {
        if ( i == shamt.getBitWidth() - 1 )
          // Special handling of the most-significant sign bit that is 'X' here.
          shamt[ i ] = WIR_L4::b0;
        else
          shamt[ i ] = positiveShamt ? WIR_L4::b1 : WIR_L4::b0;
      }
    }

    // Do the shift operation.
    r = a << shamt;

    // Finally determine the resulting PSW.C if possible.
    if ( !skip ) {
      signed long long shiftVal = shamt.getSignedValue();

      if ( shiftVal == 0 )
        carryOut = WIR_UpDownValue { WIR_L4::b0, 1 };
      else {
        auto shiftedOutBits =
          ( shiftVal > 0 ) ?
            a.extract( 32 - shiftVal, shiftVal ) : a.extract( 0, -shiftVal );

        // Check whether the shifted out bits provably are zero or non-zero.
        // They are provably zero if they only consist of '0' or 'X' bits. They
        // are provably non-zero if they contain a '1' bit, or two complementary
        // 'L' and 'N' bits refering to the very same location, or if they
        // contains some 'X' bit.
        bool isZero = false;
        bool isNonZero = false;

        if ( shiftedOutBits.containsOnlyBits( { WIR_L4::b0, WIR_L4::bX } ) )
          isZero = true;
        else

        if ( shiftedOutBits.containsBits( { WIR_L4::b1, WIR_L4::bX } ) )
          isNonZero = true;

        else {
          set<unsigned int> LLocations;
          set<unsigned int> NLocations;

          for ( unsigned int i = 0; i < shiftedOutBits.getBitWidth(); ++i )
            if ( shiftedOutBits.at( i ) == WIR_L4::bL )
              LLocations.insert( i );
            else

            if ( shiftedOutBits.at( i ) == WIR_L4::bN )
              NLocations.insert( i );

          for ( auto l : LLocations )
            for ( auto n : NLocations )
              if ( shiftedOutBits.getLocation( l ) ==
                     shiftedOutBits.getLocation( n ) ) {
                isNonZero = true;
                break;
              }
        }

        if ( isZero )
          carryOut = WIR_UpDownValue { WIR_L4::b0, 1 };
        else

        if ( isNonZero )
          carryOut = WIR_UpDownValue { WIR_L4::b1, 1 };
        else
          carryOut = WIR_UpDownValue { WIR_L4::bU, 1 };
      }
    }
  }

  if ( o.getOpCode() == TC13::OpCode::SHA_H ) {
    auto &a = nthOp( 1 );
    a.setSignedness( true );
    auto &b = lastOp();
    b.setSignedness();
    auto shamt = b.extract( 0, 5 );

    for ( auto bitPos : halfwordPos )
      insert( nthRes( 0 ), a.extract( bitPos, 16 ) << shamt, bitPos );
  }

  if ( o.getOpCode() == TC13::OpCode::SHAS ) {
    auto &b = lastOp();
    b.setSignedness();
    auto shamt = b.extract( 0, 6 );

    auto &a = nthOp( 1 );
    a.setSignedness( true );

    if ( shamt.isPositive() ) {
      WIR_UpDownValue res = a.extract( 31, 1 ).extend( 33 );
      insert( res, a << shamt, 0 );

      nthRes( 0 ) = ssov( res, 32 );
    }

    if ( shamt.isNegative() )
      nthRes( 0 ) = a << shamt;
  }


  //
  // ST* & co.
  //

  if ( ( ( o.getOpCode() == TC13::OpCode::ST_A ) ||
         ( o.getOpCode() == TC13::OpCode::ST_B ) ||
         ( o.getOpCode() == TC13::OpCode::ST_D ) ||
         ( o.getOpCode() == TC13::OpCode::ST_DA ) ||
         ( o.getOpCode() == TC13::OpCode::ST_H ) ||
         ( o.getOpCode() == TC13::OpCode::ST_Q ) ||
         ( o.getOpCode() == TC13::OpCode::ST_W ) ) &&
       ( ( o.getOperationFormat() == TC13::OperationFormat::PABRA ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::PDBRA_1 ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::PEBRA ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::PPBRA_2 ) ) ) {
    // Here, only the address arithmetics caused by bit-reverse addressing is
    // modeled. Post/pre-increment/decrement or circular addressing is not
    // considered here.
    auto &b = firstOp();
    b.setSignedness( false );

    auto index = b.extract( 32, 16 );
    auto incr = b.extract( 48, 16 );
    auto new_index = reverse( reverse( index ) + reverse( incr ) );

    nthRes( 0 ) = b;
    insert( nthRes( 0 ), new_index, 32 );
    insert( nthRes( 0 ), incr, 48 );
  }

  if ( ( ( o.getOpCode() == TC13::OpCode::ST_A ) ||
         ( o.getOpCode() == TC13::OpCode::ST_B ) ||
         ( o.getOpCode() == TC13::OpCode::ST_D ) ||
         ( o.getOpCode() == TC13::OpCode::ST_DA ) ||
         ( o.getOpCode() == TC13::OpCode::ST_H ) ||
         ( o.getOpCode() == TC13::OpCode::ST_Q ) ||
         ( o.getOpCode() == TC13::OpCode::ST_W ) ) &&
       ( ( o.getOperationFormat() == TC13::OperationFormat::AC10APIA ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::SAA_6 ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::AC10DPIA_1 ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::SAD_3 ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::AC10EPIA ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::AC10PPIA ) ) ) {
    // Here, only the address arithmetics caused by post/pre-increment/decrement
    // addressing modes is modeled. Bit-reverse addressing or circular
    // addressing is not considered here.
    unsigned int i = 1;
    if ( ( o.getOperationFormat() == TC13::OperationFormat::SAA_6 ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::SAD_3 ) )
      i = 0;
    auto &b = nthOp( i );
    auto &r = nthRes( i );

    i = 0;
    if ( o.getOperationFormat() == TC13::OperationFormat::SAA_6 )
      i = 4;
    if ( ( o.getOperationFormat() == TC13::OperationFormat::SAD_3 ) &&
         ( o.getOpCode() == TC13::OpCode::ST_B ) )
      i = 1;
    if ( ( o.getOperationFormat() == TC13::OperationFormat::SAD_3 ) &&
         ( o.getOpCode() == TC13::OpCode::ST_H ) )
      i = 2;
    if ( ( o.getOperationFormat() == TC13::OperationFormat::SAD_3 ) &&
         ( o.getOpCode() == TC13::OpCode::ST_W ) )
      i = 4;

    WIR_UpDownValue incr =
      ( i != 0 ) ? WIR_UpDownValue { TC_Const10_Signed { i } } : nthOp( 2 );

    r = b + incr.extend( 32 );
  }


  //
  // SUB & co.
  //

  if ( ( o.getOpCode() == TC13::OpCode::SUB ) ||
       ( o.getOpCode() == TC13::OpCode::SUB_A ) ) {
    auto &b = lastOp();
    b.setSignedness( o.getOperationFormat() != TC13::OperationFormat::SSPC8 );

    if ( ( ( o.getOperationFormat() == TC13::OperationFormat::DDD_1 ) ||
           ( o.getOperationFormat() == TC13::OperationFormat::AAA ) ) &&
         ( nthReg( 1 ) == nthReg( 2 ) ) )
      // Special case: SUB Dc, Da, Da = 0
      nthRes( 0 ) = { WIR_L4::b0, 32, true };
    else

    if ( o.getOperationFormat() == TC13::OperationFormat::SDD_2 )
      nthRes( 0 ) = firstOp() - b;
    else

    if ( o.getOperationFormat() == TC13::OperationFormat::SSPC8 )
      nthRes( 0 ) = firstOp() - b.extend( 32 );
    else
      nthRes( 0 ) = nthOp( 1 ) - b;
    nthRes( 0 ).setSignedness( o.getOpCode() != TC13::OpCode::SUB_A );
  }

  if ( o.getOpCode() == TC13::OpCode::SUBC ) {
    auto a = nthOp( 1 );
    a.setSignedness();
    auto b = nthOp( 2 );
    b.setSignedness();
    auto carryIn = nthOp( 3 ).extend( 32 );
    carryIn.setSignedness();

    auto &r = nthRes( 0 );
    auto &carryOut = nthRes( 3 );

    if ( nthReg( 1 ) == nthReg( 2 ) ) {
      // Special case: SUBC Dc, Da, Da = 0
      r = { WIR_L4::b0, 32, true };
      carryOut = { WIR_L4::b0, 1 };
    } else {
      r =
        a - b + carryIn - WIR_UpDownValue( TC_Const4_Signed( 1 ) ).extend( 32 );

      // Compute PSW.C.
      carryOut = carry( a, ~b, carryIn );
    }
    r.setSignedness();
  }

  if ( o.getOpCode() == TC13::OpCode::SUBX ) {
    auto a = nthOp( 1 );
    a.setSignedness();
    auto b = nthOp( 2 );
    b.setSignedness();

    auto &r = nthRes( 0 );
    auto &carryOut = nthRes( 3 );

    if ( nthReg( 1 ) == nthReg( 2 ) ) {
      // Special case: SUBX Dc, Da, Da = 0
      r = { WIR_L4::b0, 32, true };
      carryOut = { WIR_L4::b0, 1 };
    } else {
      r = a - b;

      // Compute PSW.C.
      carryOut = carry( a, ~b, WIR_UpDownValue( TC_Const4_Unsigned( 1 ) ) );
    }
    r.setSignedness();
  }

  if ( o.getOpCode() == TC13::OpCode::SUB_B ) {
    if ( nthReg( 1 ) == lastReg() )
      // Special case: SUB.B Dc, Da, Da = 0
      nthRes( 0 ) = { WIR_L4::b0, 32, true };
    else
      for ( auto bitPos: bytePos )
        insert( nthRes( 0 ),
                nthOp( 1 ).extract( bitPos, 8 ) - lastOp().extract( bitPos, 8 ),
                bitPos );
  }

  if ( o.getOpCode() == TC13::OpCode::SUB_H ) {
    if ( nthReg( 1 ) == lastReg() )
      // Special case: SUB.H Dc, Da, Da = 0
      nthRes( 0 ) = { WIR_L4::b0, 32, true };
    else
      for ( auto bitPos: halfwordPos )
        insert( nthRes( 0 ), nthOp( 1 ).extract( bitPos, 16 ) -
                             lastOp().extract( bitPos, 16 ), bitPos );
  }

  if ( o.getOpCode() == TC13::OpCode::SUBS ) {
    auto &a =
      ( o.getOperationFormat() == TC13::OperationFormat::SDD_2 ) ?
        firstOp() : nthOp( 1 );
    auto &b = lastOp();
    a.setSignedness();
    b.setSignedness();

    if ( ( o.getOperationFormat() == TC13::OperationFormat::DDD_1 ) &&
         ( nthReg( 1 ) == lastReg() ) )
      // Special case: SUBS Dc, Da, Da = 0
      nthRes( 0 ) = { WIR_L4::b0, 32, true };
    else
      nthRes( 0 ) = ssov( a.extend( 33 ) - b.extend( 33 ), 32 );
  }

  if ( o.getOpCode() == TC13::OpCode::SUBS_H )
    if ( nthReg( 1 ) == lastReg() )
      // Special case: SUBS.H Dc, Da, Da = 0
      nthRes( 0 ) = { WIR_L4::b0, 32, true };
    else
      for ( auto bitPos : halfwordPos ) {
        WIR_UpDownValue a = nthOp( 1 ).extract( bitPos, 16 );
        WIR_UpDownValue b = lastOp().extract( bitPos, 16 );
        a.setSignedness();
        b.setSignedness();

        WIR_UpDownValue res_h = a.extend( 17 ) - b.extend( 17 );
        insert( nthRes( 0 ), ssov( res_h, 16 ), bitPos );
      }

  if ( o.getOpCode() == TC13::OpCode::SUBS_HU )
    if ( nthReg( 1 ) == lastReg() )
      // Special case: SUBS.HU Dc, Da, Da = 0
      nthRes( 0 ) = { WIR_L4::b0, 32, true };
    else
      for ( auto bitPos : halfwordPos ) {
        WIR_UpDownValue a = nthOp( 1 ).extract( bitPos, 16 );
        WIR_UpDownValue b = lastOp().extract( bitPos, 16 );
        a.setSignedness( false );
        b.setSignedness( false );

        WIR_UpDownValue res_h = a.extend( 17 ) - b.extend( 17 );
        insert( nthRes( 0 ), suov( res_h, 16 ), bitPos );
      }

  if ( o.getOpCode() == TC13::OpCode::SUBS_U ) {
    auto &a = nthOp( 1 );
    a.setSignedness( false );
    auto &b = lastOp();
    b.setSignedness( false );

    if ( nthReg( 1 ) == lastReg() )
      // Special case: SUBS.U Dc, Da, Da = 0
      nthRes( 0 ) = { WIR_L4::b0, 32, true };
    else {
      WIR_UpDownValue res = a.extend( 33 ) - b.extend( 33 );
      res.setSignedness();
      nthRes( 0 ) = suov( res, 32 );
    }
  }


  //
  // SWAP.W
  //

  if ( ( o.getOpCode() == TC13::OpCode::SWAP_W ) &&
       ( o.getOperationFormat() == TC13::OperationFormat::PDBRA_2 ) ) {
    // Here, only the address arithmetics caused by bit-reverse addressing is
    // modeled. Post/pre-increment/decrement or circular addressing is not
    // considered here.
    auto &b = firstOp();
    b.setSignedness( false );

    auto index = b.extract( 32, 16 );
    auto incr = b.extract( 48, 16 );
    auto new_index = reverse( reverse( index ) + reverse( incr ) );

    nthRes( 0 ) = b;
    insert( nthRes( 0 ), new_index, 32 );
    insert( nthRes( 0 ), incr, 48 );
  }

  if ( ( o.getOpCode() == TC13::OpCode::SWAP_W ) &&
       ( o.getOperationFormat() == TC13::OperationFormat::AC10DPIA_2 ) ) {
    // Here, only the address arithmetics caused by post/pre-increment/decrement
    // addressing modes is modeled. Bit-reverse addressing or circular
    // addressing is not considered here.
    auto &b = nthOp( 1 );

    nthRes( 1 ) = b + nthOp( 2 ).extend( 32 );
  }


  //
  // X*OR & co.
  //

  if ( o.getOpCode() == TC13::OpCode::XNOR ) {
    auto &b = lastOp();
    b.setSignedness( false );
    auto &r = nthRes( 0 );

    if ( ( o.getOperationFormat() == TC13::OperationFormat::DDD_1 ) &&
         ( nthReg( 1 ) == lastReg() ) )
      // Special case: XNOR Dc, Da, Da = 0xFFFFFFFF
      r = { WIR_L4::b1, 32, false };
    else
      r = ~( nthOp( 1 ) ^ b.extend( 32 ) );

    r.setSignedness( false );
  }

  if ( o.getOpCode() == TC13::OpCode::XNOR_T ) {
    auto b1 = nthOp( 1 ).extract( nthOp( 2 ).getSignedValue(), 1 );
    auto b2 = nthOp( 3 ).extract( lastOp().getSignedValue(), 1 );
    auto &r = nthRes( 0 );

    r.setAllBits( WIR_L4::b0 );
    insert( r, ~( b1 ^ b2 ), 0 );
    r.setSignedness( false );
  }

  if ( o.getOpCode() == TC13::OpCode::XOR ) {
    auto &b = lastOp();
    b.setSignedness( false );
    auto &r = nthRes( 0 );

    if ( ( o.getOperationFormat() == TC13::OperationFormat::DDD_1 ) &&
         ( nthReg( 1 ) == lastReg() ) )
      // Special case: XOR Dc, Da, Da = 0
      r = { WIR_L4::b0, 32, false };
    else

    if ( o.getOperationFormat() == TC13::OperationFormat::SDD_2 )
      r = firstOp() ^ b.extend( 32 );
    else
      r = nthOp( 1 ) ^ b.extend( 32 );
    r.setSignedness( false );
  }

  if ( o.getOpCode() == TC13::OpCode::XOR_EQ ) {
    auto &b = lastOp();
    b.setSignedness();
    auto &r = nthRes( 0 );

    WIR_L4 cmp = ( nthOp( 1 ) == b.extend( 32 ) );

    if ( ( o.getOperationFormat() == TC13::OperationFormat::DDD_2 ) &&
         ( nthReg( 1 ) == lastReg() ) )
      // Special case: XOR.EQ Dc, Da, Da
      cmp = WIR_L4::b1;

    r = firstOp();
    insert( r, r.extract( 0, 1 ) ^ WIR_UpDownValue { cmp, 1, false }, 0 );
    r.setSignedness( false );
  }

  if ( ( o.getOpCode() == TC13::OpCode::XOR_GE ) ||
       ( o.getOpCode() == TC13::OpCode::XOR_GE_U ) ) {
    auto &a = nthOp( 1 );
    auto &b = lastOp();
    a.setSignedness( o.getOpCode() == TC13::OpCode::XOR_GE );
    b.setSignedness( o.getOpCode() == TC13::OpCode::XOR_GE );
    auto &r = nthRes( 0 );

    WIR_L4 cmp = ( a >= b.extend( 32 ) );

    if ( ( o.getOperationFormat() == TC13::OperationFormat::DDD_2 ) &&
         ( nthReg( 1 ) == lastReg() ) )
      // Special case: XOR.GE Dc, Da, Da
      cmp = WIR_L4::b1;

    r = firstOp();
    insert( r, r.extract( 0, 1 ) ^ WIR_UpDownValue { cmp, 1, false }, 0 );
    r.setSignedness( false );
  }

  if ( ( o.getOpCode() == TC13::OpCode::XOR_LT ) ||
       ( o.getOpCode() == TC13::OpCode::XOR_LT_U ) ) {
    auto &a = nthOp( 1 );
    auto &b = lastOp();
    a.setSignedness( o.getOpCode() == TC13::OpCode::XOR_LT );
    b.setSignedness( o.getOpCode() == TC13::OpCode::XOR_LT );
    auto &r = nthRes( 0 );

    WIR_L4 cmp = ( a < b.extend( 32 ) );

    if ( ( o.getOperationFormat() == TC13::OperationFormat::DDD_2 ) &&
         ( nthReg( 1 ) == lastReg() ) )
      // Special case: XOR.LT Dc, Da, Da
      cmp = WIR_L4::b0;

    r = firstOp();
    insert( r, r.extract( 0, 1 ) ^ WIR_UpDownValue { cmp, 1, false }, 0 );
    r.setSignedness( false );
  }

  if ( o.getOpCode() == TC13::OpCode::XOR_NE ) {
    auto &b = lastOp();
    b.setSignedness();
    auto &r = nthRes( 0 );

    WIR_L4 cmp = ( nthOp( 1 ) != b.extend( 32 ) );

    if ( ( o.getOperationFormat() == TC13::OperationFormat::DDD_2 ) &&
         ( nthReg( 1 ) == lastReg() ) )
      // Special case: XOR.NE Dc, Da, Da
      cmp = WIR_L4::b0;

    r = firstOp();
    insert( r, r.extract( 0, 1 ) ^ WIR_UpDownValue { cmp, 1, false }, 0 );
    r.setSignedness( false );
  }

  if ( o.getOpCode() == TC13::OpCode::XOR_T ) {
    auto b1 = nthOp( 1 ).extract( nthOp( 2 ).getSignedValue(), 1 );
    auto b2 = nthOp( 3 ).extract( lastOp().getSignedValue(), 1 );
    auto &r = nthRes( 0 );

    r.setAllBits( WIR_L4::b0 );
    insert( r, b1 ^ b2, 0 );
    r.setSignedness( false );
  }

};


/*
  simulateBottomUp performs the TriCore-specific bottom-up simulation of the
  given WIR operation.

  For a documentation of the semantics of TriCore operations, please refer to
  the TriCore User's Manual Volume 2 (Instruction Set).
*/
void TC_BitDFA::simulateBottomUp( const WIR_Operation &o,
                                  std::map<WIR_id_t, WIR_UpDownValue> &in,
                                  std::map<WIR_id_t, WIR_UpDownValue> &out,
                                  std::map<WIR_id_t, WIR_UpDownValue> &results )
{
  DSTART(
    "virtual void TC_BitDFA::simulateBottomUp(const WIR_Operation&, map<long long unsigned int, WIR_UpDownValue>&, map<long long unsigned int, WIR_UpDownValue>&, map<long long unsigned int, WIR_UpDownValue>&)" );

  DACTION(
    stringstream str;
    str << o;
    DOUT(
      "Simulating '" << str.str().substr( 8 ) << "' (ID = " << o.getID() <<
      ")." << endl ); );

  // A small lambda to update a register operand's bit up value to the bit
  // width required by the operand's register.
  auto updateOp = []( const WIR_Parameter &p, WIR_UpDownValue &v ) {
    // Compare bit width of up value with that of the parameter's actual
    // register.
    if ( p.getType() == WIR_ParameterType::reg ) {
      auto &r = dynamic_cast<const WIR_RegisterParameter &>( p ).getRegister();

      if ( r.getBitWidth() < v.getBitWidth() ) {
        // A wide up value is used for a tall register which can only happen for
        // hierarchical registers. Thus, apply an appropriate extract operation.
        unsigned int offset = 0;

        if ( r.isVirtual() ) {
          auto &vr = dynamic_cast<WIR_VirtualRegister &>( r );
          offset = ( vr.getParent().begin()->get() == r ) ? 0 : 32;
        } else {
          auto &pr = dynamic_cast<WIR_PhysicalRegister &>( r );
          offset = ( pr.getParent().begin()->get() == r ) ? 0 : 32;
        }

        v = v.extract( offset, r.getBitWidth() );
      }
    }
  };

  // A small lambda to retrieve the incoming value of o's first explicit
  // parameter.
  auto firstIn = [&]( void ) -> pair<WIR_id_t, WIR_UpDownValue *> {
    auto &p = o.getExplicitParameters().front().get();

    // Get up value from in map.
    auto &v = in.at( p.getID() );
    updateOp( p, v );

    return( make_pair( p.getID(), &v ) );
  };

  // A small lambda to retrieve the outgoing value of o's first explicit
  // parameter.
  auto firstOut = [&]( void ) -> pair<WIR_id_t, WIR_UpDownValue *> {
    auto &p = o.getExplicitParameters().front().get();

    // Get up value from out map.
    auto &v = out.at( p.getID() );
    updateOp( p, v );

    return( make_pair( p.getID(), &v ) );
  };

  // A small lambda to retrieve the incoming value of o's last explicit
  // parameter.
  auto lastIn = [&]( void ) -> pair<WIR_id_t, WIR_UpDownValue *> {
    auto &p = o.getExplicitParameters().back().get();

    // Get up value from in map.
    auto &v = in.at( p.getID() );
    updateOp( p, v );

    return( make_pair( p.getID(), &v ) );
  };

  // A small lambda to retrieve the incoming value of o's nth explicit
  // parameter.
  auto nthIn = [&]( unsigned int n ) -> pair<WIR_id_t, WIR_UpDownValue *> {
    auto it = o.getExplicitParameters().begin();
    std::advance( it, n );

    // Get up value from in map.
    auto &v = in.at( it->get().getID() );
    updateOp( it->get(), v );

    return( make_pair( it->get().getID(), &v ) );
  };

  // A small lambda to retrieve the outgoing value of o's nth explicit
  // parameter.
  auto nthOut = [&]( unsigned int n ) -> pair<WIR_id_t, WIR_UpDownValue *> {
    auto it = o.getExplicitParameters().begin();
    std::advance( it, n );

    // Get up value from out map.
    auto &v = out.at( it->get().getID() );
    updateOp( it->get(), v );

    return( make_pair( it->get().getID(), &v ) );
  };


  //
  // ABS & co.
  //

  if ( o.getOpCode() == TC13::OpCode::ABS ) {
    auto pb = lastIn();
    auto &b = *(pb.second);
    auto pc = firstOut();
    auto &c = *(pc.second);

    // If the incoming value is positive, then ABS does nothing. Thus, X bits
    // from c can be propagated bottom-up in this case.
    if ( b.isPositive() )
      for ( unsigned int i = 0; i < 32; ++i )
        if ( c.at( i ) == WIR_L4::bX )
          // If a bit in c is already X, then propagate it into b.
          b.setBit( i, WIR_L4::bX );

    results.insert( { pb.first, b } );
  }

  if ( o.getOpCode() == TC13::OpCode::ABS_B ) {
    auto pb = lastIn();
    auto &b = *(pb.second);
    auto pc = firstOut();
    auto &c = *(pc.second);

    for ( auto bitPos : bytePos ) {
      auto b1 = b.extract( bitPos, 8 );
      b1.setSignedness();

      if ( b1.isPositive() )
        for ( unsigned int i = 0; i < 8; ++i )
          if ( c.at( bitPos + i ) == WIR_L4::bX )
            b.setBit( bitPos + i, WIR_L4::bX );
    }

    results.insert( { pb.first, b } );
  }

  if ( o.getOpCode() == TC13::OpCode::ABS_H ) {
    auto pb = lastIn();
    auto &b = *(pb.second);
    auto pc = firstOut();
    auto &c = *(pc.second);

    for ( auto bitPos : halfwordPos ) {
      auto b1 = b.extract( bitPos, 16 );
      b1.setSignedness();

      if ( b1.isPositive() )
        for ( unsigned int i = 0; i < 16; ++i )
          if ( c.at( bitPos + i ) == WIR_L4::bX )
            b.setBit( bitPos + i, WIR_L4::bX );
    }

    results.insert( { pb.first, b } );
  }


  //
  // ADD & co.
  //

  if ( ( o.getOpCode() == TC13::OpCode::ADD ) ||
       ( o.getOpCode() == TC13::OpCode::ADD_A ) ||
       ( o.getOpCode() == TC13::OpCode::ADDI ) ||
       ( o.getOpCode() == TC13::OpCode::ADDX ) ) {
    auto pa =
      ( ( o.getOperationFormat() == TC13::OperationFormat::SDC4_2 ) ||
        ( o.getOperationFormat() == TC13::OperationFormat::SDD_2 ) ||
        ( o.getOperationFormat() == TC13::OperationFormat::SAC4_2 ) ||
        ( o.getOperationFormat() == TC13::OperationFormat::SAA_5 ) ) ?
      firstIn() : nthIn( 1 );
    auto &a = *(pa.second);
    auto pb = ( o.getOpCode() == TC13::OpCode::ADDX ) ? nthIn( 2 ) : lastIn();
    auto b = pb.second->extend( 32 );
    auto pc = firstOut();
    auto &c = *(pc.second);

    arithOp_ADD( c, a, b );

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b.extract( 0, pb.second->getBitWidth() ) } );
  }

  if ( o.getOpCode() == TC13::OpCode::ADDC ) {
    auto pa = nthIn( 1 );
    auto &a = *(pa.second);
    auto pb = nthIn( 2 );
    auto b = pb.second->extend( 32 );
    auto pc = firstOut();
    auto &c = *(pc.second);
    auto pcIn = lastIn();
    auto &cIn = *(pcIn.second);

    for ( int i = c.getBitWidth() - 1; i >= 0; --i )
      if ( c.at( i ) == WIR_L4::bX ) {
        a.setBit( i, WIR_L4::bX );
        b.setBit( i, WIR_L4::bX );

        if ( i == 0 )
          cIn.setBit( 0, WIR_L4::bX );
      } else
        break;

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b.extract( 0, pb.second->getBitWidth() ) } );
    results.insert( { pcIn.first, cIn } );
  }

  if ( o.getOpCode() == TC13::OpCode::ADD_B ) {
    auto pa = nthIn( 1 );
    auto &a = *(pa.second);
    auto pb = lastIn();
    auto &b = *(pb.second);
    auto pc = firstOut();
    auto &c = *(pc.second);

    for ( auto bitPos : bytePos ) {
      auto a1 = a.extract( bitPos, 8 );
      auto b1 = b.extract( bitPos, 8 );

      arithOp_ADD( c.extract( bitPos, 8 ), a1, b1 );

      insert( a, a1, bitPos );
      insert( b, b1, bitPos );
    }

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b } );
  }

  if ( o.getOpCode() == TC13::OpCode::ADD_H ) {
    auto pa = nthIn( 1 );
    auto &a = *(pa.second);
    auto pb = lastIn();
    auto &b = *(pb.second);
    auto pc = firstOut();
    auto &c = *(pc.second);

    for ( auto bitPos : halfwordPos ) {
      auto a1 = a.extract( bitPos, 16 );
      auto b1 = b.extract( bitPos, 16 );

      arithOp_ADD( c.extract( bitPos, 16 ), a1, b1 );

      insert( a, a1, bitPos );
      insert( b, b1, bitPos );
    }

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b } );
  }

  if ( ( o.getOpCode() == TC13::OpCode::ADDIH ) ||
       ( o.getOpCode() == TC13::OpCode::ADDIH_A ) ) {
    auto pa = nthIn( 1 );
    auto &a = *(pa.second);
    auto pb = lastIn();
    auto &b = *(pb.second);
    auto pc = firstOut();
    auto &c = *(pc.second);

    for ( int i = c.getBitWidth() - 1; i >= 0; --i )
      // Set the most-significant bits of a and b to X if they are X in c.
      if ( c.at( i ) == WIR_L4::bX ) {
        a.setBit( i, WIR_L4::bX );

        if ( i - 16 >= 0 )
          b.setBit( i - 16 , WIR_L4::bX );
      } else
        break;

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b } );
  }

  if ( o.getOpCode() == TC13::OpCode::ADDSC_A ) {
    auto pa = nthIn( 2 );
    auto &a = *(pa.second);
    auto pb = nthIn( 1 );
    auto &b = *(pb.second);
    auto pc = firstOut();
    auto &c = *(pc.second);
    unsigned int n =
      dynamic_cast<TC_Const2_Unsigned &>(
        o.getExplicitParameters().back().get() ).getUnsignedValue();

    for ( unsigned int i = 0; i < n; ++i )
      // Set a's most-significant bits that are shifted out by ADDSC.A to X.
      a.setBit( 31 - i, WIR_L4::bX );

    for ( int i = c.getBitWidth() - 1; i >= 0; --i )
      // Set the most-significant bits of a and b to X if they are X in c.
      if ( c.at( i ) == WIR_L4::bX ) {
        b.setBit( i, WIR_L4::bX );

        if ( (unsigned int) i >= n )
          a.setBit( i - n , WIR_L4::bX );
      } else
        break;

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b } );
  }

  if ( o.getOpCode() == TC13::OpCode::ADDSC_AT ) {
    auto pa = lastIn();
    auto &a = *(pa.second);
    auto pb = nthIn( 1 );
    auto &b = *(pb.second);
    auto pc = firstOut();
    auto &c = *(pc.second);
    auto signBit = a.at( 31 );

    // Due to D[a] >> 3 of ADDSC.AT, the 3 least-significant bits of 3 are X.
    a.setBit( 0, WIR_L4::bX );
    a.setBit( 1, WIR_L4::bX );
    a.setBit( 2, WIR_L4::bX );

    for ( int i = c.getBitWidth() - 1; i >= 0; --i )
      // Set the most-significant bits of a and b to X if they are X in c.
      if ( c.at( i ) == WIR_L4::bX ) {
        b.setBit( i, WIR_L4::bX );

        if ( i + 3 <= 31 )
          a.setBit( i + 3, WIR_L4::bX );
      } else
        break;

    // Mask a with X's only if the 4 most-significant bits of c are all X. This
    // is due to the arithmetic right-shift of a by 3 bits where the sign bit of
    // a is replicated 3 times. If any of these 4 occurrences of a's original
    // sign bits is still required, we must not mask it away.
    if ( c.extract( 28, 4 ).containsOnlyBit( WIR_L4::bX ) )
      a.setBit( 31, WIR_L4::bX );
    else
      a.setBit( 31, signBit );

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b } );
  }


  //
  // AND & co.
  //

  if ( o.getOpCode() == TC13::OpCode::AND ) {
    auto pa =
      ( ( o.getOperationFormat() == TC13::OperationFormat::SIC8_2 ) ||
        ( o.getOperationFormat() == TC13::OperationFormat::SDD_2 ) ) ?
      firstIn() : nthIn( 1 );
    auto &a = *(pa.second);
    auto pb = lastIn();
    auto b = pb.second->extend( 32 );
    auto pc = firstOut();
    auto &c = *(pc.second);

    for ( unsigned int i = 0; i < a.getBitWidth(); ++i ) {
      if ( b.at( i ) == WIR_L4::b0 )
        // If a bit in b is 0, the same bit in a is X.
        a.setBit( i, WIR_L4::bX );
      else

      if ( a.at( i ) == WIR_L4::b0 )
        // If a bit in a is 0, the same bit in b is X.
        b.setBit( i, WIR_L4::bX );

      if ( c.at( i ) == WIR_L4::bX ) {
        // If a bit in c is already X, then propagate it into both a and b.
        a.setBit( i, WIR_L4::bX );
        b.setBit( i, WIR_L4::bX );
      }
    }

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b.extract( 0, pb.second->getBitWidth() ) } );
  }

  if ( o.getOpCode() == TC13::OpCode::AND_AND_T ) {
    auto pa = nthIn( 1 );
    auto &a = *(pa.second);
    auto pb = nthIn( 3 );
    auto &b = *(pb.second);
    auto pcOut = firstOut();
    auto &cOut = *(pcOut.second);
    auto pcIn = firstIn();
    auto &cIn = *(pcIn.second);

    unsigned int pos1 = nthIn( 2 ).second->getSignedValue();
    unsigned int pos2 = lastIn().second->getSignedValue();

    bitOp_DDC5DC5_2( cOut, cIn, a, pos1, b, pos2 );
    bitOp_AND3( cOut, cIn, a, pos1, b, pos2 );

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b } );
    results.insert( { pcIn.first, cIn } );
  }

  if ( o.getOpCode() == TC13::OpCode::AND_ANDN_T ) {
    auto pa = nthIn( 1 );
    auto &a = *(pa.second);
    auto pb = nthIn( 3 );
    auto &b = *(pb.second);
    auto pcOut = firstOut();
    auto &cOut = *(pcOut.second);
    auto pcIn = firstIn();
    auto &cIn = *(pcIn.second);

    unsigned int pos1 = nthIn( 2 ).second->getSignedValue();
    unsigned int pos2 = lastIn().second->getSignedValue();

    bitOp_DDC5DC5_2( cOut, cIn, a, pos1, b, pos2 );
    bitOp_ANDN3( cOut, cIn, a, pos1, b, pos2 );

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b } );
    results.insert( { pcIn.first, cIn } );
  }

  if ( o.getOpCode() == TC13::OpCode::AND_EQ ) {
    auto pa = nthIn( 1 );
    auto &a = *(pa.second);
    auto pb = lastIn();
    auto &b = *(pb.second);
    b.setSignedness();
    auto pcOut = firstOut();
    auto &cOut = *(pcOut.second);
    auto pcIn = firstIn();
    auto &cIn = *(pcIn.second);

    WIR_L4 cmp = ( a == b.extend( 32 ) );

    bitOp_DDC9_3( cOut, cIn );
    bitOp_AND_CMP( cOut, cIn, a, b, cmp );

    if ( cmp == WIR_L4::b0 ) {
      // If a and b are inequal, we simply determine one bit position where a
      // and b provably differ and set all other bits to X.
      auto bExt = b.extend( 32 );
      unsigned int diffPos = 0;

      for ( unsigned int i = 0; i < a.getBitWidth(); ++i )
        if ( ( getLevel( a[ i ] ) == 2 ) && ( getLevel( bExt[ i ] ) == 2 ) &&
             ( a[ i ] != bExt[ i ] ) ) {
          diffPos = i;
          break;
        } else

        if ( ( getLevel( a[ i ] ) == 1 ) && ( getLevel( bExt[ i ] ) == 1 ) &&
             ( a[ i ] != bExt[ i ] ) &&
             ( a.getLocation( i ) == bExt.getLocation( i ) ) ) {
          diffPos = i;
          break;
        }

      a.setAllBits( WIR_L4::bX );
      a.setBit( diffPos, WIR_L4::b0 );

      b.setAllBits( WIR_L4::bX );
      if ( diffPos < b.getBitWidth() )
        b.setBit( diffPos, WIR_L4::b0 );
      else
        b.setBit( b.getBitWidth() - 1, WIR_L4::b0 );
    }

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b } );
    results.insert( { pcIn.first, cIn } );
  }

  if ( ( o.getOpCode() == TC13::OpCode::AND_GE ) ||
       ( o.getOpCode() == TC13::OpCode::AND_GE_U ) ) {
    auto pa = nthIn( 1 );
    auto &a = *(pa.second);
    a.setSignedness( o.getOpCode() == TC13::OpCode::AND_GE );
    auto pb = lastIn();
    auto &b = *(pb.second);
    b.setSignedness( o.getOpCode() == TC13::OpCode::AND_GE );
    auto pcOut = firstOut();
    auto &cOut = *(pcOut.second);
    auto pcIn = firstIn();
    auto &cIn = *(pcIn.second);

    WIR_L4 cmp = ( a >= b.extend( 32 ) );

    bitOp_DDC9_3( cOut, cIn );
    bitOp_AND_CMP( cOut, cIn, a, b, cmp );

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b } );
    results.insert( { pcIn.first, cIn } );
  }

  if ( ( o.getOpCode() == TC13::OpCode::AND_LT ) ||
       ( o.getOpCode() == TC13::OpCode::AND_LT_U ) ) {
    auto pa = nthIn( 1 );
    auto &a = *(pa.second);
    a.setSignedness( o.getOpCode() == TC13::OpCode::AND_LT );
    auto pb = lastIn();
    auto &b = *(pb.second);
    b.setSignedness( o.getOpCode() == TC13::OpCode::AND_LT );
    auto pcOut = firstOut();
    auto &cOut = *(pcOut.second);
    auto pcIn = firstIn();
    auto &cIn = *(pcIn.second);

    WIR_L4 cmp = ( a < b.extend( 32 ) );

    bitOp_DDC9_3( cOut, cIn );
    bitOp_AND_CMP( cOut, cIn, a, b, cmp );

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b } );
    results.insert( { pcIn.first, cIn } );
  }

  if ( o.getOpCode() == TC13::OpCode::AND_NE ) {
    auto pa = nthIn( 1 );
    auto &a = *(pa.second);
    auto pb = lastIn();
    auto &b = *(pb.second);
    b.setSignedness();
    auto pcOut = firstOut();
    auto &cOut = *(pcOut.second);
    auto pcIn = firstIn();
    auto &cIn = *(pcIn.second);

    WIR_L4 cmp = ( a != b.extend( 32 ) );

    bitOp_DDC9_3( cOut, cIn );
    bitOp_AND_CMP( cOut, cIn, a, b, cmp );

    if ( cmp == WIR_L4::b1 ) {
      // If a and b are inequal, we simply determine one bit position where a
      // and b provably differ and set all other bits to X.
      auto bExt = b.extend( 32 );
      unsigned int diffPos = 0;

      for ( unsigned int i = 0; i < a.getBitWidth(); ++i )
        if ( ( getLevel( a[ i ] ) == 2 ) && ( getLevel( bExt[ i ] ) == 2 ) &&
             ( a[ i ] != bExt[ i ] ) ) {
          diffPos = i;
          break;
        } else

        if ( ( getLevel( a[ i ] ) == 1 ) && ( getLevel( bExt[ i ] ) == 1 ) &&
             ( a[ i ] != bExt[ i ] ) &&
             ( a.getLocation( i ) == bExt.getLocation( i ) ) ) {
          diffPos = i;
          break;
        }

      a.setAllBits( WIR_L4::bX );
      a.setBit( diffPos, WIR_L4::b0 );

      b.setAllBits( WIR_L4::bX );
      if ( diffPos < b.getBitWidth() )
        b.setBit( diffPos, WIR_L4::b0 );
      else
        b.setBit( b.getBitWidth() - 1, WIR_L4::b0 );
    }

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b } );
    results.insert( { pcIn.first, cIn } );
  }

  if ( o.getOpCode() == TC13::OpCode::AND_NOR_T ) {
    auto pa = nthIn( 1 );
    auto &a = *(pa.second);
    auto pb = nthIn( 3 );
    auto &b = *(pb.second);
    auto pcOut = firstOut();
    auto &cOut = *(pcOut.second);
    auto pcIn = firstIn();
    auto &cIn = *(pcIn.second);

    unsigned int pos1 = nthIn( 2 ).second->getSignedValue();
    unsigned int pos2 = lastIn().second->getSignedValue();

    bitOp_DDC5DC5_2( cOut, cIn, a, pos1, b, pos2 );

    if ( b.at( pos2 ) == WIR_L4::b1 ) {
      // If b[ pos2 ] is 1, set cIn[ 0 ] and a[ pos1 ] to X.
      cIn.setBit( 0, WIR_L4::bX );
      a.setBit( pos1, WIR_L4::bX );
      DOUT( "Case 1: Setting cIn[ 0 ] and a[ " << pos1 << " ] to X." << endl );
    } else

    if ( a.at( pos1 ) == WIR_L4::b1 ) {
      // If a[ pos1 ] is 1, set cIn[ 0 ] and b[ pos2 ] to X.
      cIn.setBit( 0, WIR_L4::bX );
      b.setBit( pos2, WIR_L4::bX );
      DOUT( "Case 2: Setting cIn[ 0 ] and b[ " << pos2 << " ] to X." << endl );
    } else

    if ( cIn.at( 0 ) == WIR_L4::b0 ) {
      // If cIn[ 0 ] is 0, set a[ pos1 ] and b[ pos2 ] to X.
      a.setBit( pos1, WIR_L4::bX );
      b.setBit( pos2, WIR_L4::bX );
      DOUT(
        "Case 3: Setting a[ " << pos1 << " ] and b [ " << pos2 << " ] to X." <<
        endl );
    }

    if ( cOut.at( 0 ) == WIR_L4::bX ) {
      // If cOut[ 0 ] is X, set cIn[ 0 ], a[ pos1 ] and b[ pos2 ] to X.
      cIn.setBit( 0, WIR_L4::bX );
      a.setBit( pos1, WIR_L4::bX );
      b.setBit( pos2, WIR_L4::bX );
      DOUT(
        "Case 4: Setting cIn[ 0 ], a[ " << pos1 << " ] and b [ " << pos2 <<
        " ] to X." << endl );
    }

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b } );
    results.insert( { pcIn.first, cIn } );
  }

  if ( o.getOpCode() == TC13::OpCode::AND_OR_T ) {
    auto pa = nthIn( 1 );
    auto &a = *(pa.second);
    auto pb = nthIn( 3 );
    auto &b = *(pb.second);
    auto pcOut = firstOut();
    auto &cOut = *(pcOut.second);
    auto pcIn = firstIn();
    auto &cIn = *(pcIn.second);

    unsigned int pos1 = nthIn( 2 ).second->getSignedValue();
    unsigned int pos2 = lastIn().second->getSignedValue();

    bitOp_DDC5DC5_2( cOut, cIn, a, pos1, b, pos2 );

    if ( b.at( pos2 ) == WIR_L4::b1 ) {
      // If b[ pos2 ] is 1, set a[ pos1 ] to X.
      a.setBit( pos1, WIR_L4::bX );
      DOUT( "Case 1: Setting a[ " << pos1 << " ] to X." << endl );
    } else

    if ( a.at( pos1 ) == WIR_L4::b1 ) {
      // If a[ pos1 ] is 1, set b[ pos2 ] to X.
      b.setBit( pos2, WIR_L4::bX );
      DOUT( "Case 2: Setting b[ " << pos2 << " ] to X." << endl );
    } else

    if ( ( a.at( pos1 ) == WIR_L4::b0 ) && ( b.at( pos2 ) == WIR_L4::b0 ) ) {
      // If a[ pos1 ] and b[ pos2 ] are 0, set cIn[ 0 ] to X.
      cIn.setBit( 0, WIR_L4::bX );
      DOUT( "Case 3: Setting cIn[ 0 ] to X." << endl );
    } else

    if ( cIn.at( 0 ) == WIR_L4::b0 ) {
      // If cIn[ 0 ] is 0, set a[ pos1 ] and b[ pos2 ] to X.
      a.setBit( pos1, WIR_L4::bX );
      b.setBit( pos2, WIR_L4::bX );
      DOUT(
        "Case 4: Setting a[ " << pos1 << " ] and b [ " << pos2 << " ] to X." <<
        endl );
    }

    if ( cOut.at( 0 ) == WIR_L4::bX ) {
      // If cOut[ 0 ] is X, set cIn[ 0 ], a[ pos1 ] and b[ pos2 ] to X.
      cIn.setBit( 0, WIR_L4::bX );
      a.setBit( pos1, WIR_L4::bX );
      b.setBit( pos2, WIR_L4::bX );
      DOUT(
        "Case 5: Setting cIn[ 0 ], a[ " << pos1 << " ] and b [ " << pos2 <<
        " ] to X." << endl );
    }

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b } );
    results.insert( { pcIn.first, cIn } );
  }

  if ( o.getOpCode() == TC13::OpCode::AND_T ) {
    auto pa = nthIn( 1 );
    auto &a = *(pa.second);
    auto pb = nthIn( 3 );
    auto &b = *(pb.second);
    auto pc = firstOut();
    auto &c = *(pc.second);

    unsigned int pos1 = nthIn( 2 ).second->getSignedValue();
    unsigned int pos2 = lastIn().second->getSignedValue();

    bitOp_DDC5DC5_1( c, a, pos1, b, pos2 );

    if ( b.at( pos2 ) == WIR_L4::b0 )
      // If b[ pos2 ] is 0, a[ pos1 ] is X.
      a.setBit( pos1, WIR_L4::bX );
    else

    if ( a.at( pos1 ) == WIR_L4::b0 )
      // If a[ pos1 ] is 0, b[ pos2 ] is X.
      b.setBit( pos2, WIR_L4::bX );

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b } );
  }

  if ( o.getOpCode() == TC13::OpCode::ANDN ) {
    auto pa = nthIn( 1 );
    auto &a = *(pa.second);
    auto pb = lastIn();
    auto b = pb.second->extend( 32 );
    auto pc = firstOut();
    auto &c = *(pc.second);

    for ( unsigned int i = 0; i < a.getBitWidth(); ++i ) {
      if ( b.at( i ) == WIR_L4::b1 )
        // If a bit in b is 1, the same bit in a is X.
        a.setBit( i, WIR_L4::bX );
      else

      if ( a.at( i ) == WIR_L4::b0 )
        // If a bit in a is 0, the same bit in b is X.
        b.setBit( i, WIR_L4::bX );

      if ( c.at( i ) == WIR_L4::bX ) {
        // If a bit in c is already X, then propagate it into both a and b.
        a.setBit( i, WIR_L4::bX );
        b.setBit( i, WIR_L4::bX );
      }
    }

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b.extract( 0, pb.second->getBitWidth() ) } );
  }

  if ( o.getOpCode() == TC13::OpCode::ANDN_T ) {
    auto pa = nthIn( 1 );
    auto &a = *(pa.second);
    auto pb = nthIn( 3 );
    auto &b = *(pb.second);
    auto pc = firstOut();
    auto &c = *(pc.second);

    unsigned int pos1 = nthIn( 2 ).second->getSignedValue();
    unsigned int pos2 = lastIn().second->getSignedValue();

    bitOp_DDC5DC5_1( c, a, pos1, b, pos2 );

    if ( b.at( pos2 ) == WIR_L4::b1 )
      // If b[ pos2 ] is 1, a[ pos1 ] is X.
      a.setBit( pos1, WIR_L4::bX );
    else

    if ( a.at( pos1 ) == WIR_L4::b0 )
      // If a[ pos1 ] is 0, b[ pos2 ] is X.
      b.setBit( pos2, WIR_L4::bX );

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b } );
  }


  //
  // BMERGE & co.
  //

  if ( o.getOpCode() == TC13::OpCode::BMERGE ) {
    auto pa = nthIn( 1 );
    auto &a = *(pa.second);
    auto pb = lastIn();
    auto &b = *(pb.second);
    auto pc = firstOut();
    auto &c = *(pc.second);

    for ( unsigned int i = 16; i < 32; ++i ) {
      // BMERGE does not use the upper 16 bits of both a and b.
      a.setBit( i, WIR_L4::bX );
      b.setBit( i, WIR_L4::bX );
    }

    unsigned int j = 0;
    for ( unsigned int i = 1; i < 32; i += 2, ++j )
      // Check all odd bit positions produced by BMERGE whether they are X. If
      // so, set the corresponding bit of a to X.
      if ( c.at( i ) == WIR_L4::bX )
        a.setBit( j, WIR_L4::bX );

    j = 0;
    for ( unsigned int i = 0; i < 32; i += 2, ++j )
      // Check all even bit positions produced by BMERGE whether they are X. If
      // so, set the corresponding bit of b to X.
      if ( c.at( i ) == WIR_L4::bX )
        b.setBit( j, WIR_L4::bX );

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b } );
  }

  if ( o.getOpCode() == TC13::OpCode::BSPLIT ) {
    auto pa = lastIn();
    auto &a = *(pa.second);
    auto pc = firstOut();
    auto &c = *(pc.second);

    unsigned int j = 0;
    for ( unsigned int i = 0; i < 16; ++i, j += 2 )
      // Check bits 0 - 15 of c whether they are X. If so, set the corresponding
      // even bits of a to X.
      if ( c.at( i ) == WIR_L4::bX )
        a.setBit( j, WIR_L4::bX );

    j = 1;
    for ( unsigned int i = 32; i < 48; ++i, j += 2 )
      // Check bits 32 - 47 of c whether they are X. If so, set the
      // corresponding odd bits of a to X.
      if ( c.at( i ) == WIR_L4::bX )
        a.setBit( j, WIR_L4::bX );

    results.insert( { pa.first, a } );
  }


  //
  // CADD & co.
  //

  if ( o.getOpCode() == TC13::OpCode::CADD ) {
    auto pCond = nthIn( 1 );
    auto &cond = *(pCond.second);
    auto pa =
      ( o.getOperationFormat() == TC13::OperationFormat::SDIC4_3 ) ?
      firstIn() : nthIn( 2 );
    auto &a = *(pa.second);
    auto pb = lastIn();
    auto b = pb.second->extend( 32 );
    auto pc = firstOut();
    auto &c = *(pc.second);

    if ( cond.containsBit( WIR_L4::b1 ) )
      // The condition is definitely != 0 so that the addition is performed.
      arithOp_ADD( c, a, b );
    else

    if ( cond.containsOnlyBits( { WIR_L4::b0, WIR_L4::bX } ) )
      // The condition is == 0 so that only a move is performed.
      for ( unsigned int i = 0; i < c.getBitWidth(); ++i )
        if ( c.at( i ) == WIR_L4::bX )
          a.setBit( i, WIR_L4::bX );

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b.extract( 0, pb.second->getBitWidth() ) } );
  }

  if ( o.getOpCode() == TC13::OpCode::CADDN ) {
    auto pCond = nthIn( 1 );
    auto &cond = *(pCond.second);
    auto pa =
      ( o.getOperationFormat() == TC13::OperationFormat::SDIC4_3 ) ?
      firstIn() : nthIn( 2 );
    auto &a = *(pa.second);
    auto pb = lastIn();
    auto b = pb.second->extend( 32 );
    auto pc = firstOut();
    auto &c = *(pc.second);

    if ( cond.containsOnlyBit( WIR_L4::b0 ) )
      // The condition is definitely == 0 so that the addition is performed.
      arithOp_ADD( c, a, b );
    else

    if ( cond.containsBits( { WIR_L4::b1, WIR_L4::bX } ) )
      // The condition is != 0 so that only a move is performed.
      for ( unsigned int i = 0; i < c.getBitWidth(); ++i )
        if ( c.at( i ) == WIR_L4::bX )
          a.setBit( i, WIR_L4::bX );

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b.extract( 0, pb.second->getBitWidth() ) } );
  }


  //
  // CLO & co.
  //

  if ( ( o.getOpCode() == TC13::OpCode::CLO ) ||
       ( o.getOpCode() == TC13::OpCode::CLS ) ||
       ( o.getOpCode() == TC13::OpCode::CLZ ) ) {
    auto pa = lastIn();
    auto &a = *(pa.second);
    auto pc = firstOut();
    auto c = pc.second->extract( 0, 6 );

    if ( c.containsOnlyBit( WIR_L4::bX ) )
      a.setAllBits( WIR_L4::bX );
    else

    if ( c.isBinaryInteger() ) {
      auto leadingBits = c.getSignedValue();
      if ( o.getOpCode() == TC13::OpCode::CLS )
        ++leadingBits;

      // Set all least-significant bits of a to X up to those ones denoting the
      // leading '1/0/S' bits and the next following bit.
      for ( int i = 30 - leadingBits; i >= 0; --i )
        a.setBit( i, WIR_L4::bX );
    }

    results.insert( { pa.first, a } );
  }

  if ( ( o.getOpCode() == TC13::OpCode::CLO_H ) ||
       ( o.getOpCode() == TC13::OpCode::CLS_H ) ||
       ( o.getOpCode() == TC13::OpCode::CLZ_H ) ) {
    auto pa = lastIn();
    auto &a = *(pa.second);
    auto pc = firstOut();
    auto c = pc.second->extract( 0, 5 );

    for ( auto bitPos : halfwordPos ) {
      auto c = pc.second->extract( bitPos, 5 );

      if ( c.containsOnlyBit( WIR_L4::bX ) )
        for ( unsigned int i = bitPos; i < bitPos + 16; ++i )
          a.setBit( i, WIR_L4::bX );
      else

      if ( c.isBinaryInteger() ) {
        auto leadingBits = c.getSignedValue();
        if ( o.getOpCode() == TC13::OpCode::CLS_H )
          ++leadingBits;

        // Set all least-significant bits of a's current halfword to X up to
        // those ones denoting the leading '1/0/S' bits and the next following
        // bit.
        for ( int i = 14 + bitPos - leadingBits; i >= (int) bitPos; --i )
          a.setBit( i, WIR_L4::bX );
      }
    }

    results.insert( { pa.first, a } );
  }


  //
  // CMOV & co.
  //

  if ( o.getOpCode() == TC13::OpCode::CMOV ) {
    auto pCond = nthIn( 1 );
    auto &cond = *(pCond.second);
    auto paIn = firstIn();
    auto &aIn = *(paIn.second);
    auto pb = lastIn();
    auto b = pb.second->extend( 32 );
    auto paOut = firstOut();
    auto &aOut = *(paOut.second);

    if ( cond.containsBit( WIR_L4::b1 ) ) {
      // The condition is definitely != 0 so that a move of b is performed.
      for ( unsigned int i = 0; i < aOut.getBitWidth(); ++i )
        if ( aOut.at( i ) == WIR_L4::bX )
          b.setBit( i, WIR_L4::bX );
    } else

    if ( cond.containsOnlyBits( { WIR_L4::b0, WIR_L4::bX } ) )
      // The condition is == 0 so that a remains unchanged.
      for ( unsigned int i = 0; i < aOut.getBitWidth(); ++i )
        if ( aOut.at( i ) == WIR_L4::bX )
          aIn.setBit( i, WIR_L4::bX );

    results.insert( { paIn.first, aIn } );
    results.insert( { pb.first, b.extract( 0, pb.second->getBitWidth() ) } );
  }

  if ( o.getOpCode() == TC13::OpCode::CMOVN ) {
    auto pCond = nthIn( 1 );
    auto &cond = *(pCond.second);
    auto paIn = firstIn();
    auto &aIn = *(paIn.second);
    auto pb = lastIn();
    auto b = pb.second->extend( 32 );
    auto paOut = firstOut();
    auto &aOut = *(paOut.second);

    if ( cond.containsOnlyBit( WIR_L4::b0 ) ) {
      // The condition is definitely == 0 so that a move of b is performed.
      for ( unsigned int i = 0; i < aOut.getBitWidth(); ++i )
        if ( aOut.at( i ) == WIR_L4::bX )
          b.setBit( i, WIR_L4::bX );
    } else

    if ( cond.containsBits( { WIR_L4::b1, WIR_L4::bX } ) )
      // The condition is != 0 so that a remains unchanged.
      for ( unsigned int i = 0; i < aOut.getBitWidth(); ++i )
        if ( aOut.at( i ) == WIR_L4::bX )
          aIn.setBit( i, WIR_L4::bX );

    results.insert( { paIn.first, aIn } );
    results.insert( { pb.first, b.extract( 0, pb.second->getBitWidth() ) } );
  }


  //
  // CSUB & co.
  //

  if ( o.getOpCode() == TC13::OpCode::CSUB ) {
    auto pCond = nthIn( 1 );
    auto &cond = *(pCond.second);
    auto pa = nthIn( 2 );
    auto &a = *(pa.second);
    auto pb = lastIn();
    auto &b = *(pb.second);
    auto pc = firstOut();
    auto &c = *(pc.second);

    if ( cond.containsBit( WIR_L4::b1 ) )
      // The condition is definitely != 0 so that the subtraction is performed.
      // Since a - b is equivalent to a + (-b) and since building the
      // 2's-complement -b is equal to ~b + 1, it still holds that whenever a
      // most-significant bit of c is X, then the same most-significant bits of
      // both a and b can be set to X.
      arithOp_ADD( c, a, b );
    else

    if ( cond.containsOnlyBits( { WIR_L4::b0, WIR_L4::bX } ) )
      // The condition is == 0 so that only a move is performed.
      for ( unsigned int i = 0; i < c.getBitWidth(); ++i )
        if ( c.at( i ) == WIR_L4::bX )
          a.setBit( i, WIR_L4::bX );

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b } );
  }

  if ( o.getOpCode() == TC13::OpCode::CSUBN ) {
    auto pCond = nthIn( 1 );
    auto &cond = *(pCond.second);
    auto pa = nthIn( 2 );
    auto &a = *(pa.second);
    auto pb = lastIn();
    auto &b = *(pb.second);
    auto pc = firstOut();
    auto &c = *(pc.second);

    if ( cond.containsOnlyBit( WIR_L4::b0 ) )
      // The condition is definitely == 0 so that the addition is performed.
      // Since a - b is equivalent to a + (-b) and since building the
      // 2's-complement -b is equal to ~b + 1, it still holds that whenever a
      // most-significant bit of c is X, then the same most-significant bits of
      // both a and b can be set to X.
      arithOp_ADD( c, a, b );
    else

    if ( cond.containsBits( { WIR_L4::b1, WIR_L4::bX } ) )
      // The condition is != 0 so that only a move is performed.
      for ( unsigned int i = 0; i < c.getBitWidth(); ++i )
        if ( c.at( i ) == WIR_L4::bX )
          a.setBit( i, WIR_L4::bX );

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b } );
  }


  //
  // DEXTR
  //

  if ( o.getOpCode() == TC13::OpCode::DEXTR ) {
    auto pa = nthIn( 1 );
    auto &a = *(pa.second);
    auto pb = nthIn( 2 );
    auto &b = *(pb.second);
    auto pShamt = lastIn();
    auto &shamt = *(pShamt.second);
    auto pos = shamt.getSignedValue();
    auto pc = firstOut();
    auto &c = *(pc.second);

    if ( shamt.extract( 0, 5 ).isInteger() && ( pos >= 0 ) && ( pos <= 31 ) ) {
      // DEXTR performs a left shift by pos bits (0 <= pos <= 31). Thus, the
      // most-significant bits of a that get shifted out can be set to X.
      for ( unsigned int i = 1; i <= pos; ++i )
        a.setBit( 32 - i, WIR_L4::bX );

      // Likewise, the 32 - pos least-significant bits of b are X.
      for ( unsigned int i = 0; i < 32 - pos; ++i )
        b.setBit( i, WIR_L4::bX );

      // Finally, all irrelevant bits of c can be propagated back to a and b,
      // resp.
      for ( unsigned int i = 0; i < c.getBitWidth(); ++i )
        if ( c.at( i ) == WIR_L4::bX ) {
          if ( i < pos )
            b.setBit( 32 - pos + i, WIR_L4::bX );
          else
            a.setBit( i - pos, WIR_L4::bX );
        }
    }

    // Finally, the most-significant 27 bits of shamt are X.
    for ( unsigned int i = 5; i < shamt.getBitWidth(); ++i )
      shamt.setBit( i, WIR_L4::bX );

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b } );
    results.insert( { pShamt.first, shamt } );
  }


  //
  // EQ & co.
  //

  if ( ( o.getOpCode() == TC13::OpCode::EQ ) ||
       ( o.getOpCode() == TC13::OpCode::EQ_A ) ||
       ( o.getOpCode() == TC13::OpCode::EQANY_B ) ||
       ( o.getOpCode() == TC13::OpCode::EQANY_H ) ) {
    auto pa = nthIn( 1 );
    auto &a = *(pa.second);
    auto pb = lastIn();
    auto &b = *(pb.second);
    auto pc = firstOut();
    auto &c = *(pc.second);

    if ( c.at( 0 ) == WIR_L4::bX ) {
      // If the least-significant bit of c that stores the comparison result is
      // irrelevant, both operands a and b can be set to X.
      a.setAllBits( WIR_L4::bX );
      b.setAllBits( WIR_L4::bX );
    } else

    if ( c.at( 0 ) == WIR_L4::b0 ) {
      if ( ( o.getOpCode() == TC13::OpCode::EQ ) ||
           ( o.getOpCode() == TC13::OpCode::EQ_A ) ) {
        // If a and b are inequal, we simply determine one bit position where a
        // and b provably differ and set all other bits to X.
        auto bExt = b.extend( 32 );
        unsigned int diffPos = 0;

        for ( unsigned int i = 0; i < a.getBitWidth(); ++i )
          if ( ( getLevel( a[ i ] ) == 2 ) && ( getLevel( bExt[ i ] ) == 2 ) &&
               ( a[ i ] != bExt[ i ] ) ) {
            diffPos = i;
            break;
          } else

          if ( ( getLevel( a[ i ] ) == 1 ) && ( getLevel( bExt[ i ] ) == 1 ) &&
               ( a[ i ] != bExt[ i ] ) &&
               ( a.getLocation( i ) == bExt.getLocation( i ) ) ) {
            diffPos = i;
            break;
          }

        a.setAllBits( WIR_L4::bX );
        a.setBit( diffPos, WIR_L4::b0 );

        b.setAllBits( WIR_L4::bX );
        if ( diffPos < b.getBitWidth() )
          b.setBit( diffPos, WIR_L4::b0 );
        else
          b.setBit( b.getBitWidth() - 1, WIR_L4::b0 );
      } else

      if ( o.getOpCode() == TC13::OpCode::EQANY_B ) {
        // If all bytes of a and b are inequal, we simply determine one bit
        // position per byte where a and b provably differ and set all other
        // bits in these bytes to X.
        auto bExt = b.extend( 32 );

        for ( auto bitPos : bytePos ) {
          unsigned int diffPos = 0;

          for ( unsigned int i = bitPos; i < bitPos + 8; ++i ) {
            if ( ( getLevel( a[ i ] ) == 2 ) &&
                 ( getLevel( bExt[ i ] ) == 2 ) && ( a[ i ] != bExt[ i ] ) ) {
              diffPos = i;
              break;
            } else

            if ( ( getLevel( a[ i ] ) == 1 ) &&
                 ( getLevel( bExt[ i ] ) == 1 ) && ( a[ i ] != bExt[ i ] ) &&
                 ( a.getLocation( i ) == bExt.getLocation( i ) ) ) {
              diffPos = i;
              break;
            }
          }

          for ( unsigned int i = bitPos; i < bitPos + 8; ++i ) {
            a.setBit( i, i != diffPos ? WIR_L4::bX : WIR_L4::b0 );

            if ( i < b.getBitWidth() )
              b.setBit( i, i != diffPos ? WIR_L4::bX : WIR_L4::b0 );
            else
              // Keep the sign bit in case that b was sign-extended previously.
              b.setBit( b.getBitWidth() - 1, WIR_L4::b0 );
          }
        }
      } else

      if ( o.getOpCode() == TC13::OpCode::EQANY_H ) {
        // If all halfwords of a and b are inequal, we simply determine one bit
        // position per halfword where a and b provably differ and set all other
        // bits in these halfwords to X.
        auto bExt = b.extend( 32 );

        for ( auto bitPos : halfwordPos ) {
          unsigned int diffPos = 0;

          for ( unsigned int i = bitPos; i < bitPos + 16; ++i ) {
            if ( ( getLevel( a[ i ] ) == 2 ) &&
                 ( getLevel( bExt[ i ] ) == 2 ) && ( a[ i ] != bExt[ i ] ) ) {
              diffPos = i;
              break;
            } else

            if ( ( getLevel( a[ i ] ) == 1 ) &&
                 ( getLevel( bExt[ i ] ) == 1 ) && ( a[ i ] != bExt[ i ] ) &&
                 ( a.getLocation( i ) == bExt.getLocation( i ) ) ) {
              diffPos = i;
              break;
            }
          }

          for ( unsigned int i = bitPos; i < bitPos + 16; ++i ) {
            a.setBit( i, i != diffPos ? WIR_L4::bX : WIR_L4::b0 );

            if ( i < b.getBitWidth() )
              b.setBit( i, i != diffPos ? WIR_L4::bX : WIR_L4::b0 );
            else
              // Keep the sign bit in case that b was sign-extended previously.
              b.setBit( b.getBitWidth() - 1, WIR_L4::b0 );
          }
        }
      }
    }

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b } );
  }

  if ( o.getOpCode() == TC13::OpCode::EQ_B ) {
    auto pa = nthIn( 1 );
    auto &a = *(pa.second);
    auto pb = lastIn();
    auto &b = *(pb.second);
    auto pc = firstOut();
    auto &c = *(pc.second);

    for ( auto bitPos : bytePos ) {
      auto c1 = c.extract( bitPos, 8 );

      if ( c1.containsOnlyBit( WIR_L4::bX ) )
        for ( unsigned int i = 0; i < 8; ++i ) {
          // If a byte in c is irrelevant, the corresponding bytes of both a and
          // b can be set to X.
          a.setBit( bitPos + i, WIR_L4::bX );
          b.setBit( bitPos + i, WIR_L4::bX );
        }
      else

      if ( c1.containsOnlyBit( WIR_L4::b0 ) ) {
        // If the current bytes of a and b are inequal, we simply determine one
        // bit position where a and b provably differ and set all other bits to
        // X.
        unsigned int diffPos = 0;

        for ( unsigned int i = bitPos; i < bitPos + 8; ++i ) {
          if ( ( getLevel( a[ i ] ) == 2 ) && ( getLevel( b[ i ] ) == 2 ) &&
               ( a[ i ] != b[ i ] ) ) {
            diffPos = i;
            break;
          } else

          if ( ( getLevel( a[ i ] ) == 1 ) && ( getLevel( b[ i ] ) == 1 ) &&
               ( a[ i ] != b[ i ] ) &&
               ( a.getLocation( i ) == b.getLocation( i ) ) ) {
            diffPos = i;
            break;
          }
        }

        for ( unsigned int i = bitPos; i < bitPos + 8; ++i ) {
          a.setBit( i, i != diffPos ? WIR_L4::bX : WIR_L4::b0 );
          b.setBit( i, i != diffPos ? WIR_L4::bX : WIR_L4::b0 );
        }
      }
    }

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b } );
  }

  if ( o.getOpCode() == TC13::OpCode::EQ_H ) {
    auto pa = nthIn( 1 );
    auto &a = *(pa.second);
    auto pb = lastIn();
    auto &b = *(pb.second);
    auto pc = firstOut();
    auto &c = *(pc.second);

    for ( auto bitPos : halfwordPos ) {
      auto c1 = c.extract( bitPos, 16 );

      if ( c1.containsOnlyBit( WIR_L4::bX ) )
        for ( unsigned int i = 0; i < 16; ++i ) {
          // If a halfword in c is irrelevant, the corresponding halfwords of
          // both a and b can be set to X.
          a.setBit( bitPos + i, WIR_L4::bX );
          b.setBit( bitPos + i, WIR_L4::bX );
        }
      else

      if ( c1.containsOnlyBit( WIR_L4::b0 ) ) {
        // If the current halfwords of a and b are inequal, we simply determine
        // one bit position where a and b provably differ and set all other bits
        // to X.
        unsigned int diffPos = 0;

        for ( unsigned int i = bitPos; i < bitPos + 16; ++i ) {
          if ( ( getLevel( a[ i ] ) == 2 ) && ( getLevel( b[ i ] ) == 2 ) &&
               ( a[ i ] != b[ i ] ) ) {
            diffPos = i;
            break;
          } else

          if ( ( getLevel( a[ i ] ) == 1 ) && ( getLevel( b[ i ] ) == 1 ) &&
               ( a[ i ] != b[ i ] ) &&
               ( a.getLocation( i ) == b.getLocation( i ) ) ) {
            diffPos = i;
            break;
          }
        }

        for ( unsigned int i = bitPos; i < bitPos + 16; ++i ) {
          a.setBit( i, i != diffPos ? WIR_L4::bX : WIR_L4::b0 );
          b.setBit( i, i != diffPos ? WIR_L4::bX : WIR_L4::b0 );
        }
      }
    }

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b } );
  }

  if ( o.getOpCode() == TC13::OpCode::EQ_W ) {
    auto pa = nthIn( 1 );
    auto &a = *(pa.second);
    auto pb = lastIn();
    auto &b = *(pb.second);
    auto pc = firstOut();
    auto &c = *(pc.second);

    if ( c.containsOnlyBit( WIR_L4::b0 ) ) {
      // If a and b are inequal, we simply determine one bit position where a
      // and b provably differ and set all other bits to X.
      unsigned int diffPos = 0;

      for ( unsigned int i = 0; i < 32; ++i ) {
        if ( ( getLevel( a[ i ] ) == 2 ) && ( getLevel( b[ i ] ) == 2 ) &&
             ( a[ i ] != b[ i ] ) ) {
          diffPos = i;
          break;
        } else

        if ( ( getLevel( a[ i ] ) == 1 ) && ( getLevel( b[ i ] ) == 1 ) &&
             ( a[ i ] != b[ i ] ) &&
             ( a.getLocation( i ) == b.getLocation( i ) ) ) {
          diffPos = i;
          break;
        }
      }

      for ( unsigned int i = 0; i < 32; ++i ) {
        a.setBit( i, i != diffPos ? WIR_L4::bX : WIR_L4::b0 );
        b.setBit( i, i != diffPos ? WIR_L4::bX : WIR_L4::b0 );
      }
    }

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b } );
  }

  if ( o.getOpCode() == TC13::OpCode::EQZ_A ) {
    auto pa = lastIn();
    auto &a = *(pa.second);
    auto pc = firstOut();
    auto &c = *(pc.second);

    if ( c.at( 0 ) == WIR_L4::bX ) {
      // If the least-significant bit of c that stores the comparison result is
      // irrelevant, the operand a can be set to X.
      a.setAllBits( WIR_L4::bX );

      results.insert( { pa.first, a } );
    } else

    if ( c.at( 0 ) == WIR_L4::b0 ) {
      // If a is inequal to zero, we simply determine one bit position where a
      // is provably 1 and set all other bits to X.
      unsigned int diffPos = 0;

      for ( unsigned int i = 0; i < a.getBitWidth(); ++i )
        if ( a[ i ] == WIR_L4::b1 ) {
          diffPos = i;
          break;
        }

      a.setAllBits( WIR_L4::bX );
      a.setBit( diffPos, WIR_L4::b0 );
    }

    results.insert( { pa.first, a } );
  }


  //
  // EXTR & co.
  //

  if ( ( o.getOpCode() == TC13::OpCode::EXTR ) ||
       ( o.getOpCode() == TC13::OpCode::EXTR_U ) ) {
    unsigned int pos = 0;
    unsigned int width = 0;

    if ( o.getOperationFormat() == TC13::OperationFormat::DDC5C5 ) {
      auto it = o.getExplicitParameters().begin();
      std::advance( it, 2 );
      pos = dynamic_cast<TC_Const5_Unsigned &>( it->get() ).getUnsignedValue();
      std::advance( it, 1 );
      width =
        dynamic_cast<TC_Const5_Unsigned &>( it->get() ).getUnsignedValue();
    } else

    if ( o.getOperationFormat() == TC13::OperationFormat::DDE ) {
      auto pd = nthIn( 2 );
      auto &d = *(pd.second);

      // Set bits 5-31 and 37-63 of d to X.
      for ( unsigned int i = 5; i < 32; ++i )
        d.setBit( i, WIR_L4::bX );
      for ( unsigned int i = 37; i < 64; ++i )
        d.setBit( i, WIR_L4::bX );

      results.insert( { pd.first, d } );

      auto p = d.extract( 0, 5 );
      auto w = d.extract( 32, 5 );

      if ( !p.isBinaryInteger() || !w.isBinaryInteger() )
        return;

      pos = p.getSignedValue();
      width = w.getSignedValue();
    } else {
      auto pd = nthIn( 2 );
      auto &d = *(pd.second);

      // Set the most-significant 27 bits of d to X.
      for ( unsigned int i = 5; i < 32; ++i )
        d.setBit( i, WIR_L4::bX );

      results.insert( { pd.first, d } );

      auto d5 = d.extract( 0, 5 );
      if ( !d5.isBinaryInteger() )
        return;

      pos = d5.getSignedValue();
      width =
        dynamic_cast<TC_Const5_Unsigned &>(
          o.getExplicitParameters().back().get() ).getUnsignedValue();
    }

    auto pa = nthIn( 1 );
    auto &a = *(pa.second);
    auto pc = firstOut();
    auto &c = *(pc.second);

    // Set the pos least-significant bits of a to X.
    for ( unsigned int i = 0; i < pos; ++i )
      a.setBit( i, WIR_L4::bX );

    // Set the most-significant bits of a to X.
    for ( unsigned int i = pos + width; i < a.getBitWidth(); ++i )
      a.setBit( i, WIR_L4::bX );

    // Finally, all irrelevant bits of c can be propagated back to a.
    for ( unsigned int i = 0; i < width; ++i )
      if ( c[ i ] == WIR_L4::bX )
        a.setBit( i + pos, WIR_L4::bX );

    results.insert( { pa.first, a } );
  }


  //
  // GE & co.
  //

  if ( ( o.getOpCode() == TC13::OpCode::GE ) ||
       ( o.getOpCode() == TC13::OpCode::GE_A ) ||
       ( o.getOpCode() == TC13::OpCode::GE_U ) ) {
    auto pa = nthIn( 1 );
    auto &a = *(pa.second);
    auto pb = lastIn();
    auto &b = *(pb.second);
    auto pc = firstOut();
    auto &c = *(pc.second);

    if ( c.at( 0 ) == WIR_L4::bX ) {
      // If the least-significant bit of c that stores the comparison result is
      // irrelevant, both operands a and b can be set to X.
      a.setAllBits( WIR_L4::bX );
      b.setAllBits( WIR_L4::bX );
    }

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b } );
  }


  //
  // IMASK
  //

  if ( o.getOpCode() == TC13::OpCode::IMASK ) {
    auto pc = firstOut();
    auto &c = *(pc.second);

    unsigned int pos = 0;

    // Determine value of pos.
    if ( ( o.getOperationFormat() == TC13::OperationFormat::EC4DC5 ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::EDDC5 ) ) {
      auto pd = nthIn( 2 );
      auto &d = *(pd.second);

      // Set the most-significant 27 bits of d to X.
      for ( unsigned int i = 5; i < 32; ++i )
        d.setBit( i, WIR_L4::bX );

      results.insert( { pd.first, d } );

      auto d5 = d.extract( 0, 5 );
      if ( !d5.isBinaryInteger() )
        return;

      pos = d5.getSignedValue();
    } else {
      auto it = o.getExplicitParameters().begin();
      std::advance( it, 2 );
      pos = dynamic_cast<TC_Const5_Unsigned &>( it->get() ).getUnsignedValue();
    }

    if ( ( o.getOperationFormat() == TC13::OperationFormat::EDC5C5 ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::EDDC5 ) ) {
      auto pb = nthIn( 1 );
      auto &b = *(pb.second);

      // Set the most-significant pos bits of b to X.
      for ( unsigned int i = 32 - pos; i < 32; ++i )
        b.setBit( i, WIR_L4::bX );

      // Set those bits of b to X that are already X in the left-shifted c.
      for ( unsigned int i = pos; i < 32; ++i )
        if ( c[ i ] == WIR_L4::bX )
          b.setBit( i - pos, WIR_L4::bX );

      results.insert( { pb.first, b } );
    }
  }


  //
  // INS* & co.
  //

  if ( ( o.getOpCode() == TC13::OpCode::INS_T ) ||
       ( o.getOpCode() == TC13::OpCode::INSN_T ) ) {
    auto pa = nthIn( 1 );
    auto &a = *(pa.second);
    auto pb = nthIn( 3 );
    auto &b = *(pb.second);
    auto pc = firstOut();
    auto &c = *(pc.second);

    unsigned int pos1 = nthIn( 2 ).second->getSignedValue();
    unsigned int pos2 = lastIn().second->getSignedValue();

    // For a, bit a[ pos1 ] is X.
    a.setBit( pos1, WIR_L4::bX );

    for ( unsigned int i = 0; i < 32; ++i ) {
      if ( i != pos2 )
        // For b, all bits except b[ pos2 ] are X.
        b.setBit( i, WIR_L4::bX );

      // Back-propagate irrelevant bits from c up to a or b, resp.
      if ( c[ i ] == WIR_L4::bX ) {
        if ( i != pos1 )
          a.setBit( i, WIR_L4::bX );
        else
          b.setBit( pos2, WIR_L4::bX );
      }
    }

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b } );
  }

  if ( o.getOpCode() == TC13::OpCode::INSERT ) {
    unsigned int pos = 0;
    unsigned int width = 0;

    if ( ( o.getOperationFormat() == TC13::OperationFormat::DDC4C5C5 ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::DDDC5C5 ) ) {
      auto it = o.getExplicitParameters().begin();
      std::advance( it, 3 );
      pos = dynamic_cast<TC_Const5_Unsigned &>( it->get() ).getUnsignedValue();
      std::advance( it, 1 );
      width =
        dynamic_cast<TC_Const5_Unsigned &>( it->get() ).getUnsignedValue();
    } else

    if ( ( o.getOperationFormat() == TC13::OperationFormat::DDC4E ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::DDDE ) ) {
      auto pd = lastIn();
      auto &d = *(pd.second);
      d.setSignedness( false );

      // Set bits 37-63 and 5-31 of d to X.
      for ( unsigned int i = 5; i < 32; ++i )
        d.setBit( i, WIR_L4::bX );
      for ( unsigned int i = 37; i < 64; ++i )
        d.setBit( i, WIR_L4::bX );

      results.insert( { pd.first, d } );

      WIR_UpDownValue p = d.extract( 0, 5 );
      WIR_UpDownValue w = d.extract( 32, 5 );

      if ( !p.isBinaryInteger() || !w.isBinaryInteger() )
        return;

      pos = p.getSignedValue();
      width = w.getSignedValue();
    } else {
      auto pd = nthIn( 3 );
      auto &d = *(pd.second);
      d.setSignedness( false );

      // Set bits 5-31 of d to X.
      for ( unsigned int i = 5; i < 32; ++i )
        d.setBit( i, WIR_L4::bX );

      results.insert( { pd.first, d } );

      WIR_UpDownValue p = d.extract( 0, 5 );

      if ( !p.isBinaryInteger() )
        return;

      pos = p.getSignedValue();
      width =
        dynamic_cast<TC_Const5_Unsigned &>(
          o.getExplicitParameters().back().get() ).getUnsignedValue();
    }

    auto pa = nthIn( 1 );
    auto &a = *(pa.second);
    auto pb = nthIn( 2 );
    auto &b = *(pb.second);
    auto pc = firstOut();
    auto &c = *(pc.second);

    // Set bits pos to pos+width of a to X.
    for ( unsigned int i = pos; i < pos + width; ++i )
      a.setBit( i, WIR_L4::bX );

    // Set bits width to 31 of b to X.
    for ( unsigned int i = width; i < 32; ++i )
      if ( i < b.getBitWidth() )
        b.setBit( i, WIR_L4::bX );

    // Back-propagate irrelevant bits from c up to a or b, resp.
    for ( unsigned int i = 0; i < 32; ++i )
      if ( c[ i ] == WIR_L4::bX ) {
        if ( ( i < pos ) || ( i >= pos + width ) )
          a.setBit( i, WIR_L4::bX );
        else

        if ( i - pos < b.getBitWidth() )
          b.setBit( i - pos, WIR_L4::bX );
      }

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b } );
  }


  //
  // IXMAX* & co.
  //

  if ( ( o.getOpCode() == TC13::OpCode::IXMAX ) ||
       ( o.getOpCode() == TC13::OpCode::IXMAX_U ) ||
       ( o.getOpCode() == TC13::OpCode::IXMIN ) ||
       ( o.getOpCode() == TC13::OpCode::IXMIN_U ) ) {
    auto pd = nthIn( 1 );
    auto &d = *(pd.second);

    for ( unsigned int i = 48; i < 64; ++i )
      d.setBit( i, WIR_L4::bX );

    results.insert( { pd.first, d } );
  }


  //
  // JEQ* & co.
  //

  if ( ( o.getOpCode() == TC13::OpCode::JEQ ) ||
       ( o.getOpCode() == TC13::OpCode::JEQ_A ) ) {
    auto pa = firstIn();
    auto &a = *(pa.second);
    auto pb = nthIn( 1 );
    auto &b = *(pb.second);
    auto bExt = b.extend( 32 );

    auto cmp = ( a == bExt );

    if ( cmp == WIR_L4::b0 ) {
      // If a and b are inequal, we simply determine one bit position where a
      // and b provably differ and set all other bits to X.
      unsigned int diffPos = 0;

      for ( unsigned int i = 0; i < a.getBitWidth(); ++i )
        if ( ( getLevel( a[ i ] ) == 2 ) && ( getLevel( bExt[ i ] ) == 2 ) &&
             ( a[ i ] != bExt[ i ] ) ) {
          diffPos = i;
          break;
        } else

        if ( ( getLevel( a[ i ] ) == 1 ) && ( getLevel( bExt[ i ] ) == 1 ) &&
             ( a[ i ] != bExt[ i ] ) &&
             ( a.getLocation( i ) == bExt.getLocation( i ) ) ) {
          diffPos = i;
          break;
        }

      a.setAllBits( WIR_L4::bX );
      a.setBit( diffPos, WIR_L4::b0 );

      b.setAllBits( WIR_L4::bX );
      if ( diffPos < b.getBitWidth() )
        b.setBit( diffPos, WIR_L4::b0 );
      else
        b.setBit( b.getBitWidth() - 1, WIR_L4::b0 );

      results.insert( { pa.first, a } );
      results.insert( { pb.first, b } );
    }
  }

  if ( ( o.getOpCode() == TC13::OpCode::JGEZ ) ||
       ( o.getOpCode() == TC13::OpCode::JLTZ ) ) {
    // Greater-equal 0 and less-than 0 boil down to a simple check whether
    // register b is positive or not. Thus, only b's sign bit is relevant, all
    // other bits can be set to X.
    auto pb = firstIn();
    auto &b = *(pb.second);

    for ( unsigned int i = 0; i < 31; ++i )
      b.setBit( i, WIR_L4::bX );

    results.insert( { pb.first, b } );
  }

  if ( ( o.getOpCode() == TC13::OpCode::JI ) ||
       ( o.getOpCode() == TC13::OpCode::JLI ) ) {
    auto pa = firstIn();
    auto &a = *(pa.second);

    // The least-significant bit of a is always ignored by JI and JLI.
    a.setBit( 0, WIR_L4::bX );

    results.insert( { pa.first, a } );
  }

  if ( ( o.getOpCode() == TC13::OpCode::JNE ) ||
       ( o.getOpCode() == TC13::OpCode::JNE_A ) ) {
    auto pa = firstIn();
    auto &a = *(pa.second);
    auto pb = nthIn( 1 );
    auto &b = *(pb.second);
    auto bExt = b.extend( 32 );

    auto cmp = ( a != bExt );

    if ( cmp == WIR_L4::b1 ) {
      // If a and b are inequal, we simply determine one bit position where a
      // and b provably differ and set all other bits to X.
      unsigned int diffPos = 0;

      for ( unsigned int i = 0; i < a.getBitWidth(); ++i )
        if ( ( getLevel( a[ i ] ) == 2 ) && ( getLevel( bExt[ i ] ) == 2 ) &&
             ( a[ i ] != bExt[ i ] ) ) {
          diffPos = i;
          break;
        } else

        if ( ( getLevel( a[ i ] ) == 1 ) && ( getLevel( bExt[ i ] ) == 1 ) &&
             ( a[ i ] != bExt[ i ] ) &&
             ( a.getLocation( i ) == bExt.getLocation( i ) ) ) {
          diffPos = i;
          break;
        }

      a.setAllBits( WIR_L4::bX );
      a.setBit( diffPos, WIR_L4::b0 );

      b.setAllBits( WIR_L4::bX );
      if ( diffPos < b.getBitWidth() )
        b.setBit( diffPos, WIR_L4::b0 );
      else
        b.setBit( b.getBitWidth() - 1, WIR_L4::b0 );

      results.insert( { pa.first, a } );
      results.insert( { pb.first, b } );
    }
  }

  if ( ( o.getOpCode() == TC13::OpCode::JNED ) ||
       ( o.getOpCode() == TC13::OpCode::JNEI ) ) {
    auto paIn = firstIn();
    auto &aIn = *(paIn.second);
    auto paOut = firstOut();
    auto &aOut = *(paOut.second);
    auto pb = nthIn( 1 );
    auto &b = *(pb.second);
    auto bExt = b.extend( 32 );

    WIR_UpDownValue one = { WIR_L4::b0, 32, true };
    one.setBit( 0, WIR_L4::b1 );

    arithOp_ADD( aOut, aIn, one );

    auto cmp = ( aIn != bExt );

    if ( cmp == WIR_L4::b1 ) {
      // If a and b are inequal, we simply determine one bit position where a
      // and b provably differ and set all other bits to X.
      unsigned int diffPos = 0;

      for ( unsigned int i = 0; i < aIn.getBitWidth(); ++i )
        if ( ( getLevel( aIn[ i ] ) == 2 ) && ( getLevel( bExt[ i ] ) == 2 ) &&
             ( aIn[ i ] != bExt[ i ] ) ) {
          diffPos = i;
          break;
        } else

        if ( ( getLevel( aIn[ i ] ) == 1 ) && ( getLevel( bExt[ i ] ) == 1 ) &&
             ( aIn[ i ] != bExt[ i ] ) &&
             ( aIn.getLocation( i ) == bExt.getLocation( i ) ) ) {
          diffPos = i;
          break;
        }

      aIn.setAllBits( WIR_L4::bX );
      aIn.setBit( diffPos, WIR_L4::b0 );

      b.setAllBits( WIR_L4::bX );
      if ( diffPos < b.getBitWidth() )
        b.setBit( diffPos, WIR_L4::b0 );
      else
        b.setBit( b.getBitWidth() - 1, WIR_L4::b0 );
    }

    results.insert( { paIn.first, aIn } );
    results.insert( { pb.first, b } );
  }

  if ( ( o.getOpCode() == TC13::OpCode::JNZ ) ||
       ( o.getOpCode() == TC13::OpCode::JNZ_A ) ||
       ( o.getOpCode() == TC13::OpCode::JZ ) ||
       ( o.getOpCode() == TC13::OpCode::JZ_A ) ) {
    auto pb = firstIn();
    auto &b = *(pb.second);

    // If b is non-zero, then the first found non-zero bit is relevant and all
    // other bits of b can be set to X.
    int diffPos = -1;

    for ( unsigned int i = 0; i < b.getBitWidth(); ++i )
      if ( b[ i ] == WIR_L4::b1 ) {
        diffPos = i;
        break;
      }

    if ( diffPos != -1 ) {
      b.setAllBits( WIR_L4::bX );
      b.setBit( diffPos, WIR_L4::b0 );

      results.insert( { pb.first, b } );
    }
  }

  if ( ( o.getOpCode() == TC13::OpCode::JNZ_T ) ||
       ( o.getOpCode() == TC13::OpCode::JZ_T ) ) {
    auto pa = firstIn();
    auto &a = *(pa.second);
    unsigned int n = nthIn( 1 ).second->getSignedValue();

    // All bits of a can be set to X except bit n.
    a.setAllBits( WIR_L4::bX );
    a.setBit( n, WIR_L4::b0 );

    results.insert( { pa.first, a } );
  }


  //
  // LD* & co.
  //

  if ( ( ( o.getOpCode() == TC13::OpCode::LD_A ) ||
         ( o.getOpCode() == TC13::OpCode::LD_B ) ||
         ( o.getOpCode() == TC13::OpCode::LD_BU ) ||
         ( o.getOpCode() == TC13::OpCode::LD_D ) ||
         ( o.getOpCode() == TC13::OpCode::LD_DA ) ||
         ( o.getOpCode() == TC13::OpCode::LD_H ) ||
         ( o.getOpCode() == TC13::OpCode::LD_HU ) ||
         ( o.getOpCode() == TC13::OpCode::LD_Q ) ||
         ( o.getOpCode() == TC13::OpCode::LD_W ) ) &&
       ( ( o.getOperationFormat() == TC13::OperationFormat::AAC10PIA ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::SAA_3 ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::DAC10PIA ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::SDA_3 ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::EAC10PIA ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::PAC10PIA ) ) ) {
    // Here, only the address arithmetics caused by post/pre-increment/decrement
    // addressing modes is modeled.
    unsigned int i = 2;
    if ( ( o.getOperationFormat() == TC13::OperationFormat::SAA_3 ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::SDA_3 ) )
      i = 1;
    auto pbIn = nthIn( i );
    auto &bIn = *(pbIn.second);
    auto pbOut = nthOut( i );
    auto &bOut = *(pbOut.second);

    WIR_UpDownValue dummy { WIR_L4::b0, 32, true };

    arithOp_ADD( bOut, bIn, dummy );

    results.insert( { pbIn.first, bIn } );
  }

  if ( ( o.getOpCode() == TC13::OpCode::LDMST ) &&
       ( o.getOperationFormat() == TC13::OperationFormat::AC10EPIA ) ) {
    // Here, only the address arithmetics caused by post/pre-increment/decrement
    // addressing modes is modeled.
    auto pbIn = nthIn( 1 );
    auto &bIn = *(pbIn.second);
    auto pbOut = nthOut( 1 );
    auto &bOut = *(pbOut.second);

    WIR_UpDownValue dummy { WIR_L4::b0, 32, true };

    arithOp_ADD( bOut, bIn, dummy );

    results.insert( { pbIn.first, bIn } );
  }


  //
  // LOOP
  //

  if ( o.getOpCode() == TC13::OpCode::LOOP ) {
    auto pbIn = firstIn();
    auto &bIn = *(pbIn.second);
    auto pbOut = firstOut();
    auto &bOut = *(pbOut.second);

    WIR_UpDownValue dummy { WIR_L4::b0, 32, true };

    arithOp_ADD( bOut, bIn, dummy );

    results.insert( { pbIn.first, bIn } );
  }


  //
  // LT & co.
  //

  if ( ( o.getOpCode() == TC13::OpCode::LT ) ||
       ( o.getOpCode() == TC13::OpCode::LT_A ) ||
       ( o.getOpCode() == TC13::OpCode::LT_U ) ) {
    auto pa = nthIn( 1 );
    auto &a = *(pa.second);
    auto pb = lastIn();
    auto &b = *(pb.second);
    auto pc = firstOut();
    auto &c = *(pc.second);

    if ( c.at( 0 ) == WIR_L4::bX ) {
      // If the least-significant bit of c that stores the comparison result is
      // irrelevant, both operands a and b can be set to X.
      a.setAllBits( WIR_L4::bX );
      b.setAllBits( WIR_L4::bX );
    }

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b } );
  }

  if ( ( o.getOpCode() == TC13::OpCode::LT_B ) ||
       ( o.getOpCode() == TC13::OpCode::LT_BU ) ) {
    auto pa = nthIn( 1 );
    auto &a = *(pa.second);
    auto pb = lastIn();
    auto &b = *(pb.second);
    auto pc = firstOut();
    auto &c = *(pc.second);

    for ( auto bitPos : bytePos ) {
      auto c1 = c.extract( bitPos, 8 );

      if ( c1.containsOnlyBit( WIR_L4::bX ) )
        for ( unsigned int i = 0; i < 8; ++i ) {
          // If a byte in c is irrelevant, the corresponding bytes of both a and
          // b can be set to X.
          a.setBit( bitPos + i, WIR_L4::bX );
          b.setBit( bitPos + i, WIR_L4::bX );
        }
    }

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b } );
  }

  if ( ( o.getOpCode() == TC13::OpCode::LT_H ) ||
       ( o.getOpCode() == TC13::OpCode::LT_HU ) ) {
    auto pa = nthIn( 1 );
    auto &a = *(pa.second);
    auto pb = lastIn();
    auto &b = *(pb.second);
    auto pc = firstOut();
    auto &c = *(pc.second);

    for ( auto bitPos : halfwordPos ) {
      auto c1 = c.extract( bitPos, 16 );

      if ( c1.containsOnlyBit( WIR_L4::bX ) )
        for ( unsigned int i = 0; i < 16; ++i ) {
          // If a halfword in c is irrelevant, the corresponding bytes of both a
          // and b can be set to X.
          a.setBit( bitPos + i, WIR_L4::bX );
          b.setBit( bitPos + i, WIR_L4::bX );
        }
    }

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b } );
  }


  //
  // MADD*/MSUB* & co.
  //

  if ( ( o.getOpCode() == TC13::OpCode::MADD ) ||
       ( o.getOpCode() == TC13::OpCode::MADDS ) ||
       ( o.getOpCode() == TC13::OpCode::MADD_U ) ||
       ( o.getOpCode() == TC13::OpCode::MADDS_U ) ||
       ( o.getOpCode() == TC13::OpCode::MSUB ) ||
       ( o.getOpCode() == TC13::OpCode::MSUBS ) ||
       ( o.getOpCode() == TC13::OpCode::MSUB_U ) ||
       ( o.getOpCode() == TC13::OpCode::MSUBS_U ) ) {
    auto pa = nthIn( 2 );
    auto &a = *(pa.second);
    auto pb = lastIn();
    auto &b = *(pb.second);

    // If one of the two factors is 0, the other factor can be set to X.
    if ( b.containsOnlyBit( WIR_L4::b0 ) )
      a.setAllBits( WIR_L4::bX );
    else

    if ( a.containsOnlyBit( WIR_L4::b0 ) )
      b.setAllBits( WIR_L4::bX );

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b } );
  }

  if ( ( o.getOpCode() == TC13::OpCode::MADD_H ) ||
       ( o.getOpCode() == TC13::OpCode::MADDS_H ) ||
       ( o.getOpCode() == TC13::OpCode::MADDSU_H ) ||
       ( o.getOpCode() == TC13::OpCode::MADDSUS_H ) ||
       ( o.getOpCode() == TC13::OpCode::MSUB_H ) ||
       ( o.getOpCode() == TC13::OpCode::MSUBS_H ) ||
       ( o.getOpCode() == TC13::OpCode::MSUBAD_H ) ||
       ( o.getOpCode() == TC13::OpCode::MSUBADS_H ) ) {
    auto pc = firstOut();
    auto &c = *(pc.second);
    auto pd = nthIn( 1 );
    auto &d = *(pd.second);

    for ( auto bitPos : wordPos ) {
      auto c1 = c.extract( bitPos, 32 );

      if ( c1.containsOnlyBit( WIR_L4::bX ) )
        for ( unsigned int i = 0; i < 32; ++i )
          // If a word in c is irrelevant, the corresponding word in d can be
          // set to X.
          d.setBit( bitPos + i, WIR_L4::bX );
    }

    results.insert( { pd.first, d } );
  }

  if ( ( o.getOpCode() == TC13::OpCode::MADDR_H ) ||
       ( o.getOpCode() == TC13::OpCode::MADDRS_H ) ||
       ( o.getOpCode() == TC13::OpCode::MADDSUR_H ) ||
       ( o.getOpCode() == TC13::OpCode::MADDSURS_H ) ||
       ( o.getOpCode() == TC13::OpCode::MSUBADR_H ) ||
       ( o.getOpCode() == TC13::OpCode::MSUBADRS_H ) ||
       ( o.getOpCode() == TC13::OpCode::MSUBR_H ) ||
       ( o.getOpCode() == TC13::OpCode::MSUBRS_H ) ) {
    auto pc = firstOut();
    auto &c = *(pc.second);
    auto pd = nthIn( 1 );
    auto &d = *(pd.second);

    for ( auto bitPos : halfwordPos ) {
      auto c1 = c.extract( bitPos, 16 );

      if ( c1.containsOnlyBit( WIR_L4::bX ) ) {
        if ( o.getOperationFormat() == TC13::OperationFormat::DEDDC1 )
          for ( unsigned int i = 0; i < 32; ++i )
            // If a halfword in c is irrelevant, the corresponding word in d can
            // be set to X.
            d.setBit( ( bitPos * 2 ) + i, WIR_L4::bX );
        else
          for ( unsigned int i = 0; i < 16; ++i )
            // If a halfword in c is irrelevant, the corresponding halfword in d
            // can be set to X.
            d.setBit( bitPos + i, WIR_L4::bX );
      }
    }

    results.insert( { pd.first, d } );
  }


  //
  // MAX/MIN & co.
  //

  if ( ( o.getOpCode() == TC13::OpCode::MAX_B ) ||
       ( o.getOpCode() == TC13::OpCode::MAX_BU ) ||
       ( o.getOpCode() == TC13::OpCode::MIN_B ) ||
       ( o.getOpCode() == TC13::OpCode::MIN_BU ) ) {
    auto pa = nthIn( 1 );
    auto &a = *(pa.second);
    auto pb = lastIn();
    auto &b = *(pb.second);
    auto pc = firstOut();
    auto &c = *(pc.second);

    for ( auto bitPos : bytePos ) {
      auto c1 = c.extract( bitPos, 8 );

      if ( c1.containsOnlyBit( WIR_L4::bX ) )
        for ( unsigned int i = 0; i < 8; ++i ) {
          // If a byte in c is irrelevant, the corresponding bytes of both a and
          // b can be set to X.
          a.setBit( bitPos + i, WIR_L4::bX );
          b.setBit( bitPos + i, WIR_L4::bX );
        }
    }

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b } );
  }

  if ( ( o.getOpCode() == TC13::OpCode::MAX_H ) ||
       ( o.getOpCode() == TC13::OpCode::MAX_HU ) ||
       ( o.getOpCode() == TC13::OpCode::MIN_H ) ||
       ( o.getOpCode() == TC13::OpCode::MIN_HU ) ) {
    auto pa = nthIn( 1 );
    auto &a = *(pa.second);
    auto pb = lastIn();
    auto &b = *(pb.second);
    auto pc = firstOut();
    auto &c = *(pc.second);

    for ( auto bitPos : halfwordPos ) {
      auto c1 = c.extract( bitPos, 16 );

      if ( c1.containsOnlyBit( WIR_L4::bX ) )
        for ( unsigned int i = 0; i < 16; ++i ) {
          // If a halfword in c is irrelevant, the corresponding bytes of both a
          // and b can be set to X.
          a.setBit( bitPos + i, WIR_L4::bX );
          b.setBit( bitPos + i, WIR_L4::bX );
        }
    }

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b } );
  }


  //
  // MOV & co.
  //

  if ( ( o.getOpCode() == TC13::OpCode::MOV_RR ) ||
       ( o.getOpCode() == TC13::OpCode::MOV_A ) ||
       ( o.getOpCode() == TC13::OpCode::MOV_AA ) ||
       ( o.getOpCode() == TC13::OpCode::MOV_D ) ||
       ( o.getOpCode() == TC13::OpCode::MOV_U ) ) {
    auto pa = firstOut();
    auto &a = *(pa.second);
    auto pb = lastIn();
    auto b = pb.second->extend( 32 );

    for ( unsigned int i = 0; i < a.getBitWidth(); ++i )
      if ( a.at( i ) == WIR_L4::bX )
        b.setBit( i, WIR_L4::bX );

    results.insert( { pb.first, b.extract( 0, pb.second->getBitWidth() ) } );
  }


  //
  // MUL & co.
  //

  if ( ( o.getOpCode() == TC13::OpCode::MUL ) ||
       ( o.getOpCode() == TC13::OpCode::MULS ) ||
       ( o.getOpCode() == TC13::OpCode::MUL_U ) ||
       ( o.getOpCode() == TC13::OpCode::MULS_U ) ) {
    unsigned int i = 1;
    if ( o.getOperationFormat() == TC13::OperationFormat::SDD_2 )
      i = 0;
    auto pa = nthIn( i );
    auto &a = *(pa.second);
    auto pb = lastIn();
    auto &b = *(pb.second);
    auto pc = firstOut();
    auto &c = *(pc.second);

    // If one of the two factors is 0, the other factor can be set to X.
    if ( b.containsOnlyBit( WIR_L4::b0 ) )
      a.setAllBits( WIR_L4::bX );
    else

    if ( a.containsOnlyBit( WIR_L4::b0 ) )
      b.setAllBits( WIR_L4::bX );
    else

    // If one of the two factors is 1, the other factor can be set to c.
    if ( ( a[ 0 ] == WIR_L4::b1 ) &&
         a.extract( 1, a.getBitWidth() - 1 ).containsOnlyBit( WIR_L4::b0 ) )
      for ( unsigned int i = 0; i < b.getBitWidth(); ++i )
        if ( c.at( i ) == WIR_L4::bX )
          b.setBit( i, WIR_L4::bX );
    else

    if ( ( b[ 0 ] == WIR_L4::b1 ) &&
         b.extract( 1, b.getBitWidth() - 1 ).containsOnlyBit( WIR_L4::b0 ) )
      for ( unsigned int i = 0; i < a.getBitWidth(); ++i )
        if ( c.at( i ) == WIR_L4::bX )
          a.setBit( i, WIR_L4::bX );

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b } );
  }

  if ( o.getOpCode() == TC13::OpCode::MUL_H ) {
    auto pa = nthIn( 1 );
    auto &a = *(pa.second);
    auto pb = nthIn( 2 );
    auto &b = *(pb.second);
    auto pc = firstOut();
    auto &c = *(pc.second);

    for ( auto bitPos : wordPos ) {
      auto c1 = c.extract( bitPos, 32 );

      if ( c1.containsOnlyBit( WIR_L4::bX ) ) {
        for ( unsigned int i = 0; i < 16; ++i )
          if ( o.getOperationFormat() == TC13::OperationFormat::EDDC1_3 )
            // If a word in c is irrelevant, the corresponding halfword in a can
            // be set to X.
            a.setBit( ( bitPos == 0 ? 0 : 16 ) + i, WIR_L4::bX );
          else

          if ( o.getOperationFormat() == TC13::OperationFormat::EDDC1_4 ) {
            // If a word in c is irrelevant, the corresponding halfword in a and
            // the opposite halfword in b can be set to X.
            a.setBit( ( bitPos == 0 ? 0 : 16 ) + i, WIR_L4::bX );
            b.setBit( ( bitPos == 0 ? 16 : 0 ) + i, WIR_L4::bX );
          } else

          if ( o.getOperationFormat() == TC13::OperationFormat::EDDC1_6 ) {
            // If a word in c is irrelevant, the corresponding halfwords in a
            // and b can be set to X.
            a.setBit( ( bitPos == 0 ? 0 : 16 ) + i, WIR_L4::bX );
            b.setBit( ( bitPos == 0 ? 0 : 16 ) + i, WIR_L4::bX );
          } else
            // If a word in c is irrelevant, the opposite halfword in a can be
            // set to X.
            a.setBit( ( bitPos == 0 ? 16 : 0 ) + i, WIR_L4::bX );
      }
    }

    if ( o.getOperationFormat() == TC13::OperationFormat::EDDC1_3 )
      // The most-significant halfword of b is X.
      for ( unsigned int i = 16; i < 32; ++i )
        b.setBit( i, WIR_L4::bX );

    if ( o.getOperationFormat() == TC13::OperationFormat::EDDC1_6 )
      // The least-significant halfword of b is X.
      for ( unsigned int i = 0; i < 16; ++i )
        b.setBit( i, WIR_L4::bX );

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b } );
  }

  if ( ( o.getOpCode() == TC13::OpCode::MUL_Q ) ||
       ( o.getOpCode() == TC13::OpCode::MULR_Q ) ) {
    auto pa = nthIn( 1 );
    auto &a = *(pa.second);
    auto pb = nthIn( 2 );
    auto &b = *(pb.second);

    if ( ( o.getOperationFormat() == TC13::OperationFormat::DDDC1_2 ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::EDDC1_2 ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::DDDC1_8 ) )
      // The most-significant halfword of b is X.
      for ( unsigned int i = 16; i < 32; ++i )
        b.setBit( i, WIR_L4::bX );

    if ( ( o.getOperationFormat() == TC13::OperationFormat::DDDC1_5 ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::EDDC1_5 ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::DDDC1_9 ) )
      // The least-significant halfword of b is X.
      for ( unsigned int i = 0; i < 16; ++i )
        b.setBit( i, WIR_L4::bX );

    if ( o.getOperationFormat() == TC13::OperationFormat::DDDC1_8 )
      // The most-significant halfword of a is X.
      for ( unsigned int i = 16; i < 32; ++i )
        a.setBit( i, WIR_L4::bX );

    if ( o.getOperationFormat() == TC13::OperationFormat::DDDC1_9 )
      // The least-significant halfword of a is X.
      for ( unsigned int i = 0; i < 16; ++i )
        a.setBit( i, WIR_L4::bX );

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b } );
  }

  if ( o.getOpCode() == TC13::OpCode::MULM_H ) {
    auto pb = nthIn( 2 );
    auto &b = *(pb.second);

    if ( o.getOperationFormat() == TC13::OperationFormat::EDDC1_3 )
      // The most-significant halfword of b is X.
      for ( unsigned int i = 16; i < 32; ++i )
        b.setBit( i, WIR_L4::bX );

    if ( o.getOperationFormat() == TC13::OperationFormat::EDDC1_7 )
      // The least-significant halfword of b is X.
      for ( unsigned int i = 0; i < 16; ++i )
        b.setBit( i, WIR_L4::bX );

    results.insert( { pb.first, b } );
  }

  if ( o.getOpCode() == TC13::OpCode::MULR_H ) {
    auto pa = nthIn( 1 );
    auto &a = *(pa.second);
    auto pb = nthIn( 2 );
    auto &b = *(pb.second);
    auto pc = firstOut();
    auto &c = *(pc.second);

    for ( auto bitPos : halfwordPos ) {
      auto c1 = c.extract( bitPos, 16 );

      if ( c1.containsOnlyBit( WIR_L4::bX ) ) {
        for ( unsigned int i = 0; i < 16; ++i )
          if ( o.getOperationFormat() == TC13::OperationFormat::DDDC1_3 )
            // If a halfword in c is irrelevant, the corresponding halfword in a
            // can be set to X.
            a.setBit( bitPos + i, WIR_L4::bX );
          else

          if ( o.getOperationFormat() == TC13::OperationFormat::DDDC1_4 ) {
            // If a halfword in c is irrelevant, the corresponding halfword in a
            // and the opposite halfword in b can be set to X.
            a.setBit( bitPos + i, WIR_L4::bX );
            b.setBit( ( bitPos == 0 ? 16 : 0 ) + i, WIR_L4::bX );
          } else

          if ( o.getOperationFormat() == TC13::OperationFormat::DDDC1_6 ) {
            // If a halfword in c is irrelevant, the corresponding halfwords in
            // a and b can be set to X.
            a.setBit( bitPos + i, WIR_L4::bX );
            b.setBit( bitPos + i, WIR_L4::bX );
          } else
            // If a halfword in c is irrelevant, the opposite halfword in a can
            // be set to X.
            a.setBit( ( bitPos == 0 ? 16 : 0 ) + i, WIR_L4::bX );
      }
    }

    if ( o.getOperationFormat() == TC13::OperationFormat::DDDC1_3 )
      // The most-significant halfword of b is X.
      for ( unsigned int i = 16; i < 32; ++i )
        b.setBit( i, WIR_L4::bX );

    if ( o.getOperationFormat() == TC13::OperationFormat::DDDC1_7 )
      // The least-significant halfword of b is X.
      for ( unsigned int i = 0; i < 16; ++i )
        b.setBit( i, WIR_L4::bX );

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b } );
  }


  //
  // NAND & co.
  //

  if ( o.getOpCode() == TC13::OpCode::NAND ) {
    auto pa = nthIn( 1 );
    auto &a = *(pa.second);
    auto pb = lastIn();
    auto b = pb.second->extend( 32 );
    auto pc = firstOut();
    auto &c = *(pc.second);

    for ( unsigned int i = 0; i < a.getBitWidth(); ++i ) {
      if ( b.at( i ) == WIR_L4::b0 )
        // If a bit in b is 0, the same bit in a is X.
        a.setBit( i, WIR_L4::bX );
      else

      if ( a.at( i ) == WIR_L4::b0 )
        // If a bit in a is 0, the same bit in b is X.
        b.setBit( i, WIR_L4::bX );

      if ( c.at( i ) == WIR_L4::bX ) {
        // If a bit in c is already X, then propagate it into both a and b.
        a.setBit( i, WIR_L4::bX );
        b.setBit( i, WIR_L4::bX );
      }
    }

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b.extract( 0, pb.second->getBitWidth() ) } );
  }

  if ( o.getOpCode() == TC13::OpCode::NAND_T ) {
    auto pa = nthIn( 1 );
    auto &a = *(pa.second);
    auto pb = nthIn( 3 );
    auto &b = *(pb.second);
    auto pc = firstOut();
    auto &c = *(pc.second);

    unsigned int pos1 = nthIn( 2 ).second->getSignedValue();
    unsigned int pos2 = lastIn().second->getSignedValue();

    bitOp_DDC5DC5_1( c, a, pos1, b, pos2 );

    if ( b.at( pos2 ) == WIR_L4::b0 )
      // If b[ pos2 ] is 0, a[ pos1 ] is X.
      a.setBit( pos1, WIR_L4::bX );
    else

    if ( a.at( pos1 ) == WIR_L4::b0 )
      // If a[ pos1 ] is 0, b[ pos2 ] is X.
      b.setBit( pos2, WIR_L4::bX );

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b } );
  }


  //
  // NE & co.
  //

  if ( ( o.getOpCode() == TC13::OpCode::NE ) ||
       ( o.getOpCode() == TC13::OpCode::NE_A ) ) {
    auto pa = nthIn( 1 );
    auto &a = *(pa.second);
    auto pb = lastIn();
    auto &b = *(pb.second);
    auto pc = firstOut();
    auto &c = *(pc.second);

    if ( c.at( 0 ) == WIR_L4::bX ) {
      // If the least-significant bit of c that stores the comparison result is
      // irrelevant, both operands a and b can be set to X.
      a.setAllBits( WIR_L4::bX );
      b.setAllBits( WIR_L4::bX );
    } else

    if ( c.at( 0 ) == WIR_L4::b1 ) {
      // If a and b are inequal, we simply determine one bit position where a
      // and b provably differ and set all other bits to X.
      auto bExt = b.extend( 32 );
      unsigned int diffPos = 0;

      for ( unsigned int i = 0; i < a.getBitWidth(); ++i )
        if ( ( getLevel( a[ i ] ) == 2 ) && ( getLevel( bExt[ i ] ) == 2 ) &&
             ( a[ i ] != bExt[ i ] ) ) {
          diffPos = i;
          break;
        } else

        if ( ( getLevel( a[ i ] ) == 1 ) && ( getLevel( bExt[ i ] ) == 1 ) &&
             ( a[ i ] != bExt[ i ] ) &&
             ( a.getLocation( i ) == bExt.getLocation( i ) ) ) {
          diffPos = i;
          break;
        }

      a.setAllBits( WIR_L4::bX );
      a.setBit( diffPos, WIR_L4::b0 );

      b.setAllBits( WIR_L4::bX );
      if ( diffPos < b.getBitWidth() )
        b.setBit( diffPos, WIR_L4::b0 );
      else
        b.setBit( b.getBitWidth() - 1, WIR_L4::b0 );
    }

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b } );
  }

  if ( o.getOpCode() == TC13::OpCode::NEZ_A ) {
    auto pa = lastIn();
    auto &a = *(pa.second);
    auto pc = firstOut();
    auto &c = *(pc.second);

    if ( c.at( 0 ) == WIR_L4::bX ) {
      // If the least-significant bit of c that stores the comparison result is
      // irrelevant, the operand a can be set to X.
      a.setAllBits( WIR_L4::bX );

      results.insert( { pa.first, a } );
    } else

    if ( c.at( 0 ) == WIR_L4::b1 ) {
      // If a is inequal to zero, we simply determine one bit position where a
      // is provably 1 and set all other bits to X.
      unsigned int diffPos = 0;

      for ( unsigned int i = 0; i < a.getBitWidth(); ++i )
        if ( a[ i ] == WIR_L4::b1 ) {
          diffPos = i;
          break;
        }

      a.setAllBits( WIR_L4::bX );
      a.setBit( diffPos, WIR_L4::b0 );
    }

    results.insert( { pa.first, a } );
  }


  //
  // NOR & co.
  //

  if ( o.getOpCode() == TC13::OpCode::NOR ) {
    auto pa = nthIn( 1 );
    auto &a = *(pa.second);
    auto pb = lastIn();
    auto b = pb.second->extend( 32 );
    auto pc = firstOut();
    auto &c = *(pc.second);

    for ( unsigned int i = 0; i < a.getBitWidth(); ++i ) {
      if ( b.at( i ) == WIR_L4::b1 )
        // If a bit in b is 1, the same bit in a is X.
        a.setBit( i, WIR_L4::bX );
      else

      if ( a.at( i ) == WIR_L4::b1 )
        // If a bit in a is 1, the same bit in b is X.
        b.setBit( i, WIR_L4::bX );

      if ( c.at( i ) == WIR_L4::bX ) {
        // If a bit in c is already X, then propagate it into both a and b.
        a.setBit( i, WIR_L4::bX );
        b.setBit( i, WIR_L4::bX );
      }
    }

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b.extract( 0, pb.second->getBitWidth() ) } );
  }

  if ( o.getOpCode() == TC13::OpCode::NOR_T ) {
    auto pa = nthIn( 1 );
    auto &a = *(pa.second);
    auto pb = nthIn( 3 );
    auto &b = *(pb.second);
    auto pc = firstOut();
    auto &c = *(pc.second);

    unsigned int pos1 = nthIn( 2 ).second->getSignedValue();
    unsigned int pos2 = lastIn().second->getSignedValue();

    bitOp_DDC5DC5_1( c, a, pos1, b, pos2 );

    if ( b.at( pos2 ) == WIR_L4::b1 )
      // If b[ pos2 ] is 1, a[ pos1 ] is X.
      a.setBit( pos1, WIR_L4::bX );
    else

    if ( a.at( pos1 ) == WIR_L4::b1 )
      // If a[ pos1 ] is 1, b[ pos2 ] is X.
      b.setBit( pos2, WIR_L4::bX );

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b } );
  }


  //
  // NOT
  //

  if ( o.getOpCode() == TC13::OpCode::NOT ) {
    auto paIn = firstIn();
    auto &aIn = *(paIn.second);
    auto paOut = firstOut();
    auto &aOut = *(paOut.second);

    for ( unsigned int i = 0; i < aOut.getBitWidth(); ++i )
      if ( aOut[ i ] == WIR_L4::bX )
        aIn.setBit( i, WIR_L4::bX );

    results.insert( { paIn.first, aIn } );
  }


  //
  // OR & co.
  //

  if ( o.getOpCode() == TC13::OpCode::OR ) {
    auto pa =
      ( ( o.getOperationFormat() == TC13::OperationFormat::SIC8_2 ) ||
        ( o.getOperationFormat() == TC13::OperationFormat::SDD_2 ) ) ?
      firstIn() : nthIn( 1 );
    auto &a = *(pa.second);
    auto pb = lastIn();
    auto b = pb.second->extend( 32 );
    auto pc = firstOut();
    auto &c = *(pc.second);

    for ( unsigned int i = 0; i < a.getBitWidth(); ++i ) {
      if ( b.at( i ) == WIR_L4::b1 )
        // If a bit in b is 1, the same bit in a is X.
        a.setBit( i, WIR_L4::bX );
      else

      if ( a.at( i ) == WIR_L4::b1 )
        // If a bit in a is 1, the same bit in b is X.
        b.setBit( i, WIR_L4::bX );

      if ( c.at( i ) == WIR_L4::bX ) {
        // If a bit in c is already X, then propagate it into both a and b.
        a.setBit( i, WIR_L4::bX );
        b.setBit( i, WIR_L4::bX );
      }
    }

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b.extract( 0, pb.second->getBitWidth() ) } );
  }

  if ( o.getOpCode() == TC13::OpCode::OR_AND_T ) {
    auto pa = nthIn( 1 );
    auto &a = *(pa.second);
    auto pb = nthIn( 3 );
    auto &b = *(pb.second);
    auto pcOut = firstOut();
    auto &cOut = *(pcOut.second);
    auto pcIn = firstIn();
    auto &cIn = *(pcIn.second);

    unsigned int pos1 = nthIn( 2 ).second->getSignedValue();
    unsigned int pos2 = lastIn().second->getSignedValue();

    bitOp_DDC5DC5_2( cOut, cIn, a, pos1, b, pos2 );

    if ( b.at( pos2 ) == WIR_L4::b0 ) {
      // If b[ pos2 ] is 0, set a[ pos1 ] to X.
      a.setBit( pos1, WIR_L4::bX );
      DOUT( "Case 1: Setting a[ " << pos1 << " ] to X." << endl );
    } else

    if ( a.at( pos1 ) == WIR_L4::b0 ) {
      // If a[ pos1 ] is 0, set b[ pos2 ] to X.
      b.setBit( pos2, WIR_L4::bX );
      DOUT( "Case 2: Setting b[ " << pos2 << " ] to X." << endl );
    } else

    if ( ( a.at( pos1 ) == WIR_L4::b1 ) && ( b.at( pos2 ) == WIR_L4::b1 ) ) {
      // If a[ pos1 ] is 1 and b[ pos2 ] is 1, set cIn[ 0 ] to X.
      cIn.setBit( 0, WIR_L4::bX );
      DOUT( "Case 3: Setting cIn[ 0 ] to X." << endl );
    } else

    if ( cIn.at( 0 ) == WIR_L4::b1 ) {
      // If cIn[ 0 ] is 1, set a[ pos1 ] and b[ pos2 ] to X.
      a.setBit( pos1, WIR_L4::bX );
      b.setBit( pos2, WIR_L4::bX );
      DOUT(
        "Case 4: Setting a[ " << pos1 << " ] and b [ " << pos2 << " ] to X." <<
        endl );
    }

    if ( cOut.at( 0 ) == WIR_L4::bX ) {
      // If cOut[ 0 ] is X, set cIn[ 0 ], a[ pos1 ] and b[ pos2 ] to X.
      cIn.setBit( 0, WIR_L4::bX );
      a.setBit( pos1, WIR_L4::bX );
      b.setBit( pos2, WIR_L4::bX );
      DOUT(
        "Case 5: Setting cIn[ 0 ], a[ " << pos1 << " ] and b [ " << pos2 <<
        " ] to X." << endl );
    }

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b } );
    results.insert( { pcIn.first, cIn } );
  }

  if ( o.getOpCode() == TC13::OpCode::OR_ANDN_T ) {
    auto pa = nthIn( 1 );
    auto &a = *(pa.second);
    auto pb = nthIn( 3 );
    auto &b = *(pb.second);
    auto pcOut = firstOut();
    auto &cOut = *(pcOut.second);
    auto pcIn = firstIn();
    auto &cIn = *(pcIn.second);

    unsigned int pos1 = nthIn( 2 ).second->getSignedValue();
    unsigned int pos2 = lastIn().second->getSignedValue();

    bitOp_DDC5DC5_2( cOut, cIn, a, pos1, b, pos2 );

    if ( b.at( pos2 ) == WIR_L4::b1 ) {
      // If b[ pos2 ] is 1, set a[ pos1 ] to X.
      a.setBit( pos1, WIR_L4::bX );
      DOUT( "Case 1: Setting a[ " << pos1 << " ] to X." << endl );
    } else

    if ( a.at( pos1 ) == WIR_L4::b0 ) {
      // If a[ pos1 ] is 0, set b[ pos2 ] to X.
      b.setBit( pos2, WIR_L4::bX );
      DOUT( "Case 2: Setting b[ " << pos2 << " ] to X." << endl );
    } else

    if ( ( a.at( pos1 ) == WIR_L4::b1 ) && ( b.at( pos2 ) == WIR_L4::b0 ) ) {
      // If a[ pos1 ] is 1 and b[ pos2 ] is 1, set cIn[ 0 ] to X.
      cIn.setBit( 0, WIR_L4::bX );
      DOUT( "Case 3: Setting cIn[ 0 ] to X." << endl );
    } else

    if ( cIn.at( 0 ) == WIR_L4::b1 ) {
      // If cIn[ 0 ] is 1, set a[ pos1 ] and b[ pos2 ] to X.
      a.setBit( pos1, WIR_L4::bX );
      b.setBit( pos2, WIR_L4::bX );
      DOUT(
        "Case 4: Setting a[ " << pos1 << " ] and b [ " << pos2 << " ] to X." <<
        endl );
    }

    if ( cOut.at( 0 ) == WIR_L4::bX ) {
      // If cOut[ 0 ] is X, set cIn[ 0 ], a[ pos1 ] and b[ pos2 ] to X.
      cIn.setBit( 0, WIR_L4::bX );
      a.setBit( pos1, WIR_L4::bX );
      b.setBit( pos2, WIR_L4::bX );
      DOUT(
        "Case 5: Setting cIn[ 0 ], a[ " << pos1 << " ] and b [ " << pos2 <<
        " ] to X." << endl );
    }

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b } );
    results.insert( { pcIn.first, cIn } );
  }

  if ( o.getOpCode() == TC13::OpCode::OR_NOR_T ) {
    auto pa = nthIn( 1 );
    auto &a = *(pa.second);
    auto pb = nthIn( 3 );
    auto &b = *(pb.second);
    auto pcOut = firstOut();
    auto &cOut = *(pcOut.second);
    auto pcIn = firstIn();
    auto &cIn = *(pcIn.second);

    unsigned int pos1 = nthIn( 2 ).second->getSignedValue();
    unsigned int pos2 = lastIn().second->getSignedValue();

    bitOp_DDC5DC5_2( cOut, cIn, a, pos1, b, pos2 );

    if ( b.at( pos2 ) == WIR_L4::b1 ) {
      // If b[ pos2 ] is 1, set a[ pos1 ] to X.
      a.setBit( pos1, WIR_L4::bX );
      DOUT( "Case 1: Setting a[ " << pos1 << " ] to X." << endl );
    } else

    if ( a.at( pos1 ) == WIR_L4::b1 ) {
      // If a[ pos1 ] is 1, set b[ pos2 ] to X.
      b.setBit( pos2, WIR_L4::bX );
      DOUT( "Case 2: Setting b[ " << pos2 << " ] to X." << endl );
    } else

    if ( ( a.at( pos1 ) == WIR_L4::b0 ) && ( b.at( pos2 ) == WIR_L4::b0 ) ) {
      // If a[ pos1 ] is 0 and b[ pos2 ] is 0, set cIn[ 0 ] to X.
      cIn.setBit( 0, WIR_L4::bX );
      DOUT( "Case 3: Setting cIn[ 0 ] to X." << endl );
    } else

    if ( cIn.at( 0 ) == WIR_L4::b1 ) {
      // If cIn[ 0 ] is 1, set a[ pos1 ] and b[ pos2 ] to X.
      a.setBit( pos1, WIR_L4::bX );
      b.setBit( pos2, WIR_L4::bX );
      DOUT(
        "Case 4: Setting a[ " << pos1 << " ] and b [ " << pos2 << " ] to X." <<
        endl );
    }

    if ( cOut.at( 0 ) == WIR_L4::bX ) {
      // If cOut[ 0 ] is X, set cIn[ 0 ], a[ pos1 ] and b[ pos2 ] to X.
      cIn.setBit( 0, WIR_L4::bX );
      a.setBit( pos1, WIR_L4::bX );
      b.setBit( pos2, WIR_L4::bX );
      DOUT(
        "Case 5: Setting cIn[ 0 ], a[ " << pos1 << " ] and b [ " << pos2 <<
        " ] to X." << endl );
    }

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b } );
    results.insert( { pcIn.first, cIn } );
  }

  if ( o.getOpCode() == TC13::OpCode::OR_OR_T ) {
    auto pa = nthIn( 1 );
    auto &a = *(pa.second);
    auto pb = nthIn( 3 );
    auto &b = *(pb.second);
    auto pcOut = firstOut();
    auto &cOut = *(pcOut.second);
    auto pcIn = firstIn();
    auto &cIn = *(pcIn.second);

    unsigned int pos1 = nthIn( 2 ).second->getSignedValue();
    unsigned int pos2 = lastIn().second->getSignedValue();

    bitOp_DDC5DC5_2( cOut, cIn, a, pos1, b, pos2 );

    if ( b.at( pos2 ) == WIR_L4::b1 ) {
      // If b[ pos2 ] is 1, set cIn[ 0 ] and a[ pos1 ] to X.
      cIn.setBit( 0, WIR_L4::bX );
      a.setBit( pos1, WIR_L4::bX );
      DOUT( "Case 1: Setting cIn[ 0 ] and a[ " << pos1 << " ] to X." << endl );
    } else

    if ( a.at( pos1 ) == WIR_L4::b1 ) {
      // If a[ pos1 ] is 1, set cIn[ 0 ] and b[ pos2 ] to X.
      cIn.setBit( 0, WIR_L4::bX );
      b.setBit( pos2, WIR_L4::bX );
      DOUT( "Case 2: Setting cIn[ 0 ] and b[ " << pos2 << " ] to X." << endl );
    } else

    if ( cIn.at( 0 ) == WIR_L4::b1 ) {
      // If cIn[ 0 ] is 1, set a[ pos1 ] and b[ pos2 ] to X.
      a.setBit( pos1, WIR_L4::bX );
      b.setBit( pos2, WIR_L4::bX );
      DOUT(
        "Case 3: Setting a[ " << pos1 << " ] and b [ " << pos2 << " ] to X." <<
        endl );
    }

    if ( cOut.at( 0 ) == WIR_L4::bX ) {
      // If cOut[ 0 ] is X, set cIn[ 0 ], a[ pos1 ] and b[ pos2 ] to X.
      cIn.setBit( 0, WIR_L4::bX );
      a.setBit( pos1, WIR_L4::bX );
      b.setBit( pos2, WIR_L4::bX );
      DOUT(
        "Case 4: Setting cIn[ 0 ], a[ " << pos1 << " ] and b [ " << pos2 <<
        " ] to X." << endl );
    }

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b } );
    results.insert( { pcIn.first, cIn } );
  }

  if ( o.getOpCode() == TC13::OpCode::OR_EQ ) {
    auto pa = nthIn( 1 );
    auto &a = *(pa.second);
    auto pb = lastIn();
    auto &b = *(pb.second);
    b.setSignedness();
    auto pcOut = firstOut();
    auto &cOut = *(pcOut.second);
    auto pcIn = firstIn();
    auto &cIn = *(pcIn.second);

    WIR_L4 cmp = ( a == b.extend( 32 ) );

    bitOp_DDC9_3( cOut, cIn );
    bitOp_OR_CMP( cOut, cIn, a, b, cmp );

    if ( cmp == WIR_L4::b0 ) {
      // If a and b are inequal, we simply determine one bit position where a
      // and b provably differ and set all other bits to X.
      auto bExt = b.extend( 32 );
      unsigned int diffPos = 0;

      for ( unsigned int i = 0; i < a.getBitWidth(); ++i )
        if ( ( getLevel( a[ i ] ) == 2 ) && ( getLevel( bExt[ i ] ) == 2 ) &&
             ( a[ i ] != bExt[ i ] ) ) {
          diffPos = i;
          break;
        } else

        if ( ( getLevel( a[ i ] ) == 1 ) && ( getLevel( bExt[ i ] ) == 1 ) &&
             ( a[ i ] != bExt[ i ] ) &&
             ( a.getLocation( i ) == bExt.getLocation( i ) ) ) {
          diffPos = i;
          break;
        }

      a.setAllBits( WIR_L4::bX );
      a.setBit( diffPos, WIR_L4::b0 );

      b.setAllBits( WIR_L4::bX );
      if ( diffPos < b.getBitWidth() )
        b.setBit( diffPos, WIR_L4::b0 );
      else
        b.setBit( b.getBitWidth() - 1, WIR_L4::b0 );
    }

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b } );
    results.insert( { pcIn.first, cIn } );
  }

  if ( ( o.getOpCode() == TC13::OpCode::OR_GE ) ||
       ( o.getOpCode() == TC13::OpCode::OR_GE_U ) ) {
    auto pa = nthIn( 1 );
    auto &a = *(pa.second);
    a.setSignedness( o.getOpCode() == TC13::OpCode::OR_GE );
    auto pb = lastIn();
    auto &b = *(pb.second);
    b.setSignedness( o.getOpCode() == TC13::OpCode::OR_GE );
    auto pcOut = firstOut();
    auto &cOut = *(pcOut.second);
    auto pcIn = firstIn();
    auto &cIn = *(pcIn.second);

    WIR_L4 cmp = ( a >= b.extend( 32 ) );

    bitOp_DDC9_3( cOut, cIn );
    bitOp_OR_CMP( cOut, cIn, a, b, cmp );

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b } );
    results.insert( { pcIn.first, cIn } );
  }

  if ( ( o.getOpCode() == TC13::OpCode::OR_LT ) ||
       ( o.getOpCode() == TC13::OpCode::OR_LT_U ) ) {
    auto pa = nthIn( 1 );
    auto &a = *(pa.second);
    a.setSignedness( o.getOpCode() == TC13::OpCode::OR_LT );
    auto pb = lastIn();
    auto &b = *(pb.second);
    b.setSignedness( o.getOpCode() == TC13::OpCode::OR_LT );
    auto pcOut = firstOut();
    auto &cOut = *(pcOut.second);
    auto pcIn = firstIn();
    auto &cIn = *(pcIn.second);

    WIR_L4 cmp = ( a < b.extend( 32 ) );

    bitOp_DDC9_3( cOut, cIn );
    bitOp_OR_CMP( cOut, cIn, a, b, cmp );

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b } );
    results.insert( { pcIn.first, cIn } );
  }

  if ( o.getOpCode() == TC13::OpCode::OR_NE ) {
    auto pa = nthIn( 1 );
    auto &a = *(pa.second);
    auto pb = lastIn();
    auto &b = *(pb.second);
    b.setSignedness();
    auto pcOut = firstOut();
    auto &cOut = *(pcOut.second);
    auto pcIn = firstIn();
    auto &cIn = *(pcIn.second);

    WIR_L4 cmp = ( a != b.extend( 32 ) );

    bitOp_DDC9_3( cOut, cIn );
    bitOp_OR_CMP( cOut, cIn, a, b, cmp );

    if ( cmp == WIR_L4::b1 ) {
      // If a and b are inequal, we simply determine one bit position where a
      // and b provably differ and set all other bits to X.
      auto bExt = b.extend( 32 );
      unsigned int diffPos = 0;

      for ( unsigned int i = 0; i < a.getBitWidth(); ++i )
        if ( ( getLevel( a[ i ] ) == 2 ) && ( getLevel( bExt[ i ] ) == 2 ) &&
             ( a[ i ] != bExt[ i ] ) ) {
          diffPos = i;
          break;
        } else

        if ( ( getLevel( a[ i ] ) == 1 ) && ( getLevel( bExt[ i ] ) == 1 ) &&
             ( a[ i ] != bExt[ i ] ) &&
             ( a.getLocation( i ) == bExt.getLocation( i ) ) ) {
          diffPos = i;
          break;
        }

      a.setAllBits( WIR_L4::bX );
      a.setBit( diffPos, WIR_L4::b0 );

      b.setAllBits( WIR_L4::bX );
      if ( diffPos < b.getBitWidth() )
        b.setBit( diffPos, WIR_L4::b0 );
      else
        b.setBit( b.getBitWidth() - 1, WIR_L4::b0 );
    }

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b } );
    results.insert( { pcIn.first, cIn } );
  }

  if ( o.getOpCode() == TC13::OpCode::OR_T ) {
    auto pa = nthIn( 1 );
    auto &a = *(pa.second);
    auto pb = nthIn( 3 );
    auto &b = *(pb.second);
    auto pc = firstOut();
    auto &c = *(pc.second);

    unsigned int pos1 = nthIn( 2 ).second->getSignedValue();
    unsigned int pos2 = lastIn().second->getSignedValue();

    bitOp_DDC5DC5_1( c, a, pos1, b, pos2 );

    if ( b.at( pos2 ) == WIR_L4::b1 )
      // If b[ pos2 ] is 1, a[ pos1 ] is X.
      a.setBit( pos1, WIR_L4::bX );
    else

    if ( a.at( pos1 ) == WIR_L4::b1 )
      // If a[ pos1 ] is 1, b[ pos2 ] is X.
      b.setBit( pos2, WIR_L4::bX );

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b } );
  }

  if ( o.getOpCode() == TC13::OpCode::ORN ) {
    auto pa = nthIn( 1 );
    auto &a = *(pa.second);
    auto pb = lastIn();
    auto b = pb.second->extend( 32 );
    auto pc = firstOut();
    auto &c = *(pc.second);

    for ( unsigned int i = 0; i < a.getBitWidth(); ++i ) {
      if ( b.at( i ) == WIR_L4::b0 )
        // If a bit in b is 0, the same bit in a is X.
        a.setBit( i, WIR_L4::bX );
      else

      if ( a.at( i ) == WIR_L4::b1 )
        // If a bit in a is 1, the same bit in b is X.
        b.setBit( i, WIR_L4::bX );

      if ( c.at( i ) == WIR_L4::bX ) {
        // If a bit in c is already X, then propagate it into both a and b.
        a.setBit( i, WIR_L4::bX );
        b.setBit( i, WIR_L4::bX );
      }
    }

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b.extract( 0, pb.second->getBitWidth() ) } );
  }

  if ( o.getOpCode() == TC13::OpCode::ORN_T ) {
    auto pa = nthIn( 1 );
    auto &a = *(pa.second);
    auto pb = nthIn( 3 );
    auto &b = *(pb.second);
    auto pc = firstOut();
    auto &c = *(pc.second);

    unsigned int pos1 = nthIn( 2 ).second->getSignedValue();
    unsigned int pos2 = lastIn().second->getSignedValue();

    bitOp_DDC5DC5_1( c, a, pos1, b, pos2 );

    if ( b.at( pos2 ) == WIR_L4::b0 )
      // If b[ pos2 ] is 0, a[ pos1 ] is X.
      a.setBit( pos1, WIR_L4::bX );
    else

    if ( a.at( pos1 ) == WIR_L4::b1 )
      // If a[ pos1 ] is 1, b[ pos2 ] is X.
      b.setBit( pos2, WIR_L4::bX );

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b } );
  }


  //
  // PACK
  //

  if ( o.getOpCode() == TC13::OpCode::PACK ) {
    auto pa = nthIn( 2 );
    auto &a = *(pa.second);

    for ( unsigned int i = 0; i < 31; ++i )
      // The least-significant 31 bits of a are always X.
      a.setBit( i, WIR_L4::bX );

    results.insert( { pa.first, a } );
  }


  //
  // RSUB & co.
  //

  if ( o.getOpCode() == TC13::OpCode::RSUB ) {
    auto pa =
      ( o.getOperationFormat() == TC13::OperationFormat::SD ) ?
        firstIn() : nthIn( 1 );
    auto &a = *(pa.second);
    auto pb = lastIn();
    auto b =
      ( o.getOperationFormat() == TC13::OperationFormat::SD ) ?
        WIR_UpDownValue { WIR_L4::b0, 32, true } : pb.second->extend( 32 );
    auto pc = firstOut();
    auto &c = *(pc.second);

    arithOp_ADD( c, b, a );

    results.insert( { pa.first, a } );

    if ( o.getOperationFormat() == TC13::OperationFormat::DDC9_1 )
      results.insert( { pb.first, b.extract( 0, pb.second->getBitWidth() ) } );
  }


  //
  // SEL & co.
  //

  if ( o.getOpCode() == TC13::OpCode::SEL ) {
    auto pCond = nthIn( 1 );
    auto &cond = *(pCond.second);
    auto pa = nthIn( 2 );
    auto &a = *(pa.second);
    auto pb = lastIn();
    auto b = pb.second->extend( 32 );
    auto pc = firstOut();
    auto &c = *(pc.second);

    if ( cond.containsBit( WIR_L4::b1 ) ) {
      // The condition is definitely != 0 so that a is selected.
      for ( unsigned int i = 0; i < c.getBitWidth(); ++i )
        if ( c.at( i ) == WIR_L4::bX )
          a.setBit( i, WIR_L4::bX );

      b.setAllBits( WIR_L4::bX );
    } else

    if ( cond.containsOnlyBits( { WIR_L4::b0, WIR_L4::bX } ) ) {
      // The condition is == 0 so that b is selected.
      for ( unsigned int i = 0; i < c.getBitWidth(); ++i )
        if ( c.at( i ) == WIR_L4::bX )
          b.setBit( i, WIR_L4::bX );

      a.setAllBits( WIR_L4::bX );
    }

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b.extract( 0, pb.second->getBitWidth() ) } );
  }

  if ( o.getOpCode() == TC13::OpCode::SELN ) {
    auto pCond = nthIn( 1 );
    auto &cond = *(pCond.second);
    auto pa = nthIn( 2 );
    auto &a = *(pa.second);
    auto pb = lastIn();
    auto b = pb.second->extend( 32 );
    auto pc = firstOut();
    auto &c = *(pc.second);

    if ( cond.containsOnlyBits( { WIR_L4::b0, WIR_L4::bX } ) ) {
      // The condition is == 0 so that a is selected.
      for ( unsigned int i = 0; i < c.getBitWidth(); ++i )
        if ( c.at( i ) == WIR_L4::bX )
          a.setBit( i, WIR_L4::bX );

      b.setAllBits( WIR_L4::bX );
    } else

    if ( cond.containsBit( WIR_L4::b1 ) ) {
      // The condition is definitely != 0 so that b is selected.
      for ( unsigned int i = 0; i < c.getBitWidth(); ++i )
        if ( c.at( i ) == WIR_L4::bX )
          b.setBit( i, WIR_L4::bX );

      a.setAllBits( WIR_L4::bX );
    }

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b.extract( 0, pb.second->getBitWidth() ) } );
  }


  //
  // SH & co.
  //

  if ( ( o.getOpCode() == TC13::OpCode::SH ) ||
       ( o.getOpCode() == TC13::OpCode::SHAS ) ) {
    auto pa = ( o.getSize() == 2 ) ? firstIn() : nthIn( 1 );
    auto &a = *(pa.second);
    a.setSignedness( o.getOpCode() != TC13::OpCode::SH );
    auto pb = lastIn();
    auto &b = *(pb.second);
    b.setSignedness();
    auto shamt = ( b.getBitWidth() == 4 ) ? b.extend( 6 ) : b.extract( 0, 6 );
    auto pc = firstOut();
    auto &c = *(pc.second);

    // Bits 6 - 31 of the shift amount are always X.
    for ( unsigned int i = 6; i < b.getBitWidth(); ++i )
      b.setBit( i, WIR_L4::bX );

    arithOp_SH( c, a, shamt );

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b } );
  }

  if ( o.getOpCode() == TC13::OpCode::SHA ) {
    auto pa = ( o.getSize() == 2 ) ? firstIn() : nthIn( 1 );
    auto &a = *(pa.second);
    a.setSignedness();
    auto pb = ( o.getSize() == 4 ) ? nthIn( 2 ) : nthIn( 1 );
    auto &b = *(pb.second);
    b.setSignedness();
    auto shamt = ( b.getBitWidth() == 4 ) ? b.extend( 6 ) : b.extract( 0, 6 );
    auto pc = firstOut();
    auto &c = *(pc.second);
    auto pcOut = ( o.getSize() == 4 ) ? nthOut( 3 ) : nthOut( 2 );
    auto &cOut = *(pcOut.second);

    // Bits 6 - 31 of the shift amount are always X.
    for ( unsigned int i = 6; i < b.getBitWidth(); ++i )
      b.setBit( i, WIR_L4::bX );

    arithOp_SH( c, a, shamt, !cOut.containsOnlyBit( WIR_L4::bX ) );

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b } );
  }

  if ( ( o.getOpCode() == TC13::OpCode::SH_EQ ) ||
       ( o.getOpCode() == TC13::OpCode::SH_NE ) ) {
    auto pa = nthIn( 1 );
    auto &a = *(pa.second);
    auto pb = lastIn();
    auto &b = *(pb.second);
    b.setSignedness();
    auto pcOut = firstOut();
    auto &cOut = *(pcOut.second);
    auto pcIn = firstIn();
    auto &cIn = *(pcIn.second);

    WIR_UpDownValue one { WIR_L4::b0, 6, true };
    one[ 0 ] = WIR_L4::b1;
    arithOp_SH( cOut, cIn, one );

    WIR_L4 cmp = ( a == b.extend( 32 ) );

    if ( cmp == WIR_L4::b0 ) {
      // If a and b are inequal, we simply determine one bit position where a
      // and b provably differ and set all other bits to X.
      auto bExt = b.extend( 32 );
      unsigned int diffPos = 0;

      for ( unsigned int i = 0; i < a.getBitWidth(); ++i )
        if ( ( getLevel( a[ i ] ) == 2 ) && ( getLevel( bExt[ i ] ) == 2 ) &&
             ( a[ i ] != bExt[ i ] ) ) {
          diffPos = i;
          break;
        } else

        if ( ( getLevel( a[ i ] ) == 1 ) && ( getLevel( bExt[ i ] ) == 1 ) &&
             ( a[ i ] != bExt[ i ] ) &&
             ( a.getLocation( i ) == bExt.getLocation( i ) ) ) {
          diffPos = i;
          break;
        }

      a.setAllBits( WIR_L4::bX );
      a.setBit( diffPos, WIR_L4::b0 );

      b.setAllBits( WIR_L4::bX );
      if ( diffPos < b.getBitWidth() )
        b.setBit( diffPos, WIR_L4::b0 );
      else
        b.setBit( b.getBitWidth() - 1, WIR_L4::b0 );
    }

    // If cOut[0] is X, then the comparison result is not used and a and b can
    // be set to X.
    if ( cOut[ 0 ] == WIR_L4::bX ) {
      a.setAllBits( WIR_L4::bX );
      b.setAllBits( WIR_L4::bX );
    }

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b } );
    results.insert( { pcIn.first, cIn } );
  }

  if ( ( o.getOpCode() == TC13::OpCode::SH_GE ) ||
       ( o.getOpCode() == TC13::OpCode::SH_GE_U ) ||
       ( o.getOpCode() == TC13::OpCode::SH_LT ) ||
       ( o.getOpCode() == TC13::OpCode::SH_LT_U ) ) {
    auto pa = nthIn( 1 );
    auto &a = *(pa.second);
    auto pb = lastIn();
    auto &b = *(pb.second);
    b.setSignedness();
    auto pcOut = firstOut();
    auto &cOut = *(pcOut.second);
    auto pcIn = firstIn();
    auto &cIn = *(pcIn.second);

    WIR_UpDownValue one { WIR_L4::b0, 6, true };
    one[ 0 ] = WIR_L4::b1;
    arithOp_SH( cOut, cIn, one );

    // If cOut[0] is X, then the comparison result is not used and a and b can
    // be set to X.
    if ( cOut[ 0 ] == WIR_L4::bX ) {
      a.setAllBits( WIR_L4::bX );
      b.setAllBits( WIR_L4::bX );
    }

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b } );
    results.insert( { pcIn.first, cIn } );
  }

  if ( ( o.getOpCode() == TC13::OpCode::SH_H ) ||
       ( o.getOpCode() == TC13::OpCode::SHA_H ) ) {
    auto pa = nthIn( 1 );
    auto &a = *(pa.second);
    a.setSignedness( o.getOpCode() == TC13::OpCode::SHA_H );
    auto pb = lastIn();
    auto &b = *(pb.second);
    b.setSignedness();
    auto shamt = b.extract( 0, 5 ).extend( 6 );
    auto pc = firstOut();
    auto &c = *(pc.second);

    // Bits 5 - 31 of the shift amount are always X.
    for ( unsigned int i = 5; i < b.getBitWidth(); ++i )
      b.setBit( i, WIR_L4::bX );

    for ( auto bitPos : halfwordPos ) {
      auto c1 = c.extract( bitPos, 16 );
      auto a1 = a.extract( bitPos, 16 );

      arithOp_SH( c1, a1, shamt );

      insert( a, a1, bitPos );
    }

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b } );
  }

  if ( ( o.getOpCode() == TC13::OpCode::SH_AND_T ) ||
       ( o.getOpCode() == TC13::OpCode::SH_NAND_T ) ||
       ( o.getOpCode() == TC13::OpCode::SH_ANDN_T ) ) {
    auto pa = nthIn( 1 );
    auto &a = *(pa.second);
    auto pb = nthIn( 3 );
    auto &b = *(pb.second);
    auto pcOut = firstOut();
    auto &cOut = *(pcOut.second);
    auto pcIn = firstIn();
    auto &cIn = *(pcIn.second);

    unsigned int pos1 = nthIn( 2 ).second->getSignedValue();
    unsigned int pos2 = lastIn().second->getSignedValue();

    WIR_UpDownValue one { WIR_L4::b0, 6, true };
    one[ 0 ] = WIR_L4::b1;
    arithOp_SH( cOut, cIn, one );
    shiftOp_DDC5DC5_2( a, pos1, b, pos2 );

    if ( b.at( pos2 ) ==
           ( ( o.getOpCode() == TC13::OpCode::SH_ANDN_T ) ?
             WIR_L4::b1 : WIR_L4::b0 ) ) {
      // If b[ pos2 ] is 0 (or 1 for SH.ANDN.T, resp.), set a[ pos1 ] to X.
      a.setBit( pos1, WIR_L4::bX );
      DOUT( "Case 1: Setting a[ " << pos1 << " ] to X." << endl );
    } else

    if ( a.at( pos1 ) == WIR_L4::b0 ) {
      // If a[ pos1 ] is 0, set b[ pos2 ] to X.
      b.setBit( pos2, WIR_L4::bX );
      DOUT( "Case 2: Setting b[ " << pos2 << " ] to X." << endl );
    }

    if ( cOut.at( 0 ) == WIR_L4::bX ) {
      // If cOut[ 0 ] is X, set a[ pos1 ] and b[ pos2 ] to X.
      a.setBit( pos1, WIR_L4::bX );
      b.setBit( pos2, WIR_L4::bX );
      DOUT(
        "Case 3: Setting a[ " << pos1 << " ] and b [ " << pos2 << " ] to X." <<
        endl );
    }

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b } );
    results.insert( { pcIn.first, cIn } );
  }

  if ( ( o.getOpCode() == TC13::OpCode::SH_OR_T ) ||
       ( o.getOpCode() == TC13::OpCode::SH_NOR_T ) ||
       ( o.getOpCode() == TC13::OpCode::SH_ORN_T ) ) {
    auto pa = nthIn( 1 );
    auto &a = *(pa.second);
    auto pb = nthIn( 3 );
    auto &b = *(pb.second);
    auto pcOut = firstOut();
    auto &cOut = *(pcOut.second);
    auto pcIn = firstIn();
    auto &cIn = *(pcIn.second);

    unsigned int pos1 = nthIn( 2 ).second->getSignedValue();
    unsigned int pos2 = lastIn().second->getSignedValue();

    WIR_UpDownValue one { WIR_L4::b0, 6, true };
    one[ 0 ] = WIR_L4::b1;
    arithOp_SH( cOut, cIn, one );
    shiftOp_DDC5DC5_2( a, pos1, b, pos2 );

    if ( b.at( pos2 ) ==
           ( ( o.getOpCode() == TC13::OpCode::SH_ORN_T ) ?
             WIR_L4::b0 : WIR_L4::b1 ) ) {
      // If b[ pos2 ] is 1 (or 0 for SH.ORN.T, resp.), set a[ pos1 ] to X.
      a.setBit( pos1, WIR_L4::bX );
      DOUT( "Case 1: Setting a[ " << pos1 << " ] to X." << endl );
    } else

    if ( a.at( pos1 ) == WIR_L4::b1 ) {
      // If a[ pos1 ] is 1, set b[ pos2 ] to X.
      b.setBit( pos2, WIR_L4::bX );
      DOUT( "Case 2: Setting b[ " << pos2 << " ] to X." << endl );
    }

    if ( cOut.at( 0 ) == WIR_L4::bX ) {
      // If cOut[ 0 ] is X, set a[ pos1 ] and b[ pos2 ] to X.
      a.setBit( pos1, WIR_L4::bX );
      b.setBit( pos2, WIR_L4::bX );
      DOUT(
        "Case 3: Setting a[ " << pos1 << " ] and b [ " << pos2 << " ] to X." <<
        endl );
    }

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b } );
    results.insert( { pcIn.first, cIn } );
  }

  if ( ( o.getOpCode() == TC13::OpCode::SH_XOR_T ) ||
       ( o.getOpCode() == TC13::OpCode::SH_XNOR_T ) ) {
    auto pa = nthIn( 1 );
    auto &a = *(pa.second);
    auto pb = nthIn( 3 );
    auto &b = *(pb.second);
    auto pcOut = firstOut();
    auto &cOut = *(pcOut.second);
    auto pcIn = firstIn();
    auto &cIn = *(pcIn.second);

    unsigned int pos1 = nthIn( 2 ).second->getSignedValue();
    unsigned int pos2 = lastIn().second->getSignedValue();

    WIR_UpDownValue one { WIR_L4::b0, 6, true };
    one[ 0 ] = WIR_L4::b1;
    arithOp_SH( cOut, cIn, one );
    shiftOp_DDC5DC5_2( a, pos1, b, pos2 );

    if ( cOut.at( 0 ) == WIR_L4::bX ) {
      // If cOut[ 0 ] is X, set a[ pos1 ] and b[ pos2 ] to X.
      a.setBit( pos1, WIR_L4::bX );
      b.setBit( pos2, WIR_L4::bX );
      DOUT(
        "Case 1: Setting a[ " << pos1 << " ] and b [ " << pos2 << " ] to X." <<
        endl );
    }

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b } );
    results.insert( { pcIn.first, cIn } );
  }


  //
  // ST* & co.
  //

  if ( ( ( o.getOpCode() == TC13::OpCode::ST_A ) ||
         ( o.getOpCode() == TC13::OpCode::ST_B ) ||
         ( o.getOpCode() == TC13::OpCode::ST_D ) ||
         ( o.getOpCode() == TC13::OpCode::ST_DA ) ||
         ( o.getOpCode() == TC13::OpCode::ST_H ) ||
         ( o.getOpCode() == TC13::OpCode::ST_Q ) ||
         ( o.getOpCode() == TC13::OpCode::ST_W ) ) &&
       ( ( o.getOperationFormat() == TC13::OperationFormat::AC10APIA ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::SAA_6 ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::AC10DPIA_1 ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::SAD_3 ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::AC10EPIA ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::AC10PPIA ) ) ) {
    // Here, only the address arithmetics caused by post/pre-increment/decrement
    // addressing modes is modeled.
    unsigned int i = 1;
    if ( ( o.getOperationFormat() == TC13::OperationFormat::SAA_6 ) ||
         ( o.getOperationFormat() == TC13::OperationFormat::SAD_3 ) )
      i = 0;
    auto pbIn = nthIn( i );
    auto &bIn = *(pbIn.second);
    auto pbOut = nthOut( i );
    auto &bOut = *(pbOut.second);

    WIR_UpDownValue dummy { WIR_L4::b0, 32, true };

    arithOp_ADD( bOut, bIn, dummy );

    results.insert( { pbIn.first, bIn } );
  }

  if ( ( o.getOpCode() == TC13::OpCode::ST_B ) ||
       ( o.getOpCode() == TC13::OpCode::ST_H ) ) {
    auto pa = lastIn();
    auto &a = *(pa.second);

    // The most-significant 24 or 16 bits of a are X, resp.
    for ( unsigned int i = ( o.getOpCode() == TC13::OpCode::ST_B ? 8 : 16 );
          i < 32; ++i )
      a.setBit( i, WIR_L4::bX );

    results.insert( { pa.first, a } );
  }

  if ( o.getOpCode() == TC13::OpCode::ST_Q ) {
    auto pa = lastIn();
    auto &a = *(pa.second);

    // The least-significant 16 bits of a are X, resp.
    for ( unsigned int i = 0; i < 16; ++i )
      a.setBit( i, WIR_L4::bX );

    results.insert( { pa.first, a } );
  }


  //
  // SUB & co.
  //

  if ( ( o.getOpCode() == TC13::OpCode::SUB ) ||
       ( o.getOpCode() == TC13::OpCode::SUB_A ) ||
       ( o.getOpCode() == TC13::OpCode::SUBX ) ) {
    auto pa =
      ( ( o.getOperationFormat() == TC13::OperationFormat::SDD_2 ) ||
        ( o.getOperationFormat() == TC13::OperationFormat::SSPC8 ) ) ?
      firstIn() : nthIn( 1 );
    auto &a = *(pa.second);
    auto pb = ( o.getOpCode() == TC13::OpCode::SUBX ) ? nthIn( 2 ) : lastIn();
    pb.second->setSignedness(
      o.getOperationFormat() != TC13::OperationFormat::SSPC8 );
    auto b = pb.second->extend( 32 );
    auto pc = firstOut();
    auto &c = *(pc.second);

    arithOp_ADD( c, a, b );

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b.extract( 0, pb.second->getBitWidth() ) } );
  }

  if ( o.getOpCode() == TC13::OpCode::SUBC ) {
    auto pa = nthIn( 1 );
    auto &a = *(pa.second);
    auto pb = nthIn( 2 );
    auto b = *(pb.second);
    auto pc = firstOut();
    auto &c = *(pc.second);
    auto pcIn = lastIn();
    auto &cIn = *(pcIn.second);

    for ( int i = c.getBitWidth() - 1; i >= 0; --i )
      if ( c.at( i ) == WIR_L4::bX ) {
        a.setBit( i, WIR_L4::bX );
        b.setBit( i, WIR_L4::bX );

        if ( i == 0 )
          cIn.setBit( 0, WIR_L4::bX );
      } else
        break;

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b } );
    results.insert( { pcIn.first, cIn } );
  }

  if ( o.getOpCode() == TC13::OpCode::SUB_B ) {
    auto pa = nthIn( 1 );
    auto &a = *(pa.second);
    auto pb = lastIn();
    auto &b = *(pb.second);
    auto pc = firstOut();
    auto &c = *(pc.second);

    for ( auto bitPos : bytePos ) {
      auto a1 = a.extract( bitPos, 8 );
      auto b1 = b.extract( bitPos, 8 );

      arithOp_ADD( c.extract( bitPos, 8 ), a1, b1 );

      insert( a, a1, bitPos );
      insert( b, b1, bitPos );
    }

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b } );
  }

  if ( o.getOpCode() == TC13::OpCode::SUB_H ) {
    auto pa = nthIn( 1 );
    auto &a = *(pa.second);
    auto pb = lastIn();
    auto &b = *(pb.second);
    auto pc = firstOut();
    auto &c = *(pc.second);

    for ( auto bitPos : halfwordPos ) {
      auto a1 = a.extract( bitPos, 16 );
      auto b1 = b.extract( bitPos, 16 );

      arithOp_ADD( c.extract( bitPos, 16 ), a1, b1 );

      insert( a, a1, bitPos );
      insert( b, b1, bitPos );
    }

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b } );
  }


  //
  // SWAP.W
  //

  if ( ( o.getOpCode() == TC13::OpCode::SWAP_W ) &&
       ( o.getOperationFormat() == TC13::OperationFormat::AC10DPIA_2 ) ) {
    // Here, only the address arithmetics caused by post/pre-increment/decrement
    // addressing modes is modeled.
    auto pbIn = nthIn( 1 );
    auto &bIn = *(pbIn.second);
    auto pbOut = nthOut( 1 );
    auto &bOut = *(pbOut.second);

    WIR_UpDownValue dummy { WIR_L4::b0, 32, true };

    arithOp_ADD( bOut, bIn, dummy );

    results.insert( { pbIn.first, bIn } );
  }


  //
  // X*OR & co.
  //

  if ( ( o.getOpCode() == TC13::OpCode::XNOR ) ||
       ( o.getOpCode() == TC13::OpCode::XOR ) ) {
    auto pa =
      ( o.getOperationFormat() == TC13::OperationFormat::SDD_2 ) ?
      firstIn() : nthIn( 1 );
    auto &a = *(pa.second);
    auto pb = lastIn();
    auto b = pb.second->extend( 32 );
    auto pc = firstOut();
    auto &c = *(pc.second);

    for ( unsigned int i = 0; i < a.getBitWidth(); ++i )
      if ( c.at( i ) == WIR_L4::bX ) {
        // If a bit in c is already X, then propagate it into both a and b.
        a.setBit( i, WIR_L4::bX );
        b.setBit( i, WIR_L4::bX );
      }

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b.extract( 0, pb.second->getBitWidth() ) } );
  }

  if ( ( o.getOpCode() == TC13::OpCode::XNOR_T ) ||
       ( o.getOpCode() == TC13::OpCode::XOR_T ) ) {
    auto pa = nthIn( 1 );
    auto &a = *(pa.second);
    auto pb = nthIn( 3 );
    auto &b = *(pb.second);
    auto pc = firstOut();
    auto &c = *(pc.second);

    unsigned int pos1 = nthIn( 2 ).second->getSignedValue();
    unsigned int pos2 = lastIn().second->getSignedValue();

    bitOp_DDC5DC5_1( c, a, pos1, b, pos2 );

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b } );
  }

  if ( ( o.getOpCode() == TC13::OpCode::XOR_EQ ) ||
       ( o.getOpCode() == TC13::OpCode::XOR_NE ) ) {
    auto pa = nthIn( 1 );
    auto &a = *(pa.second);
    auto pb = lastIn();
    auto &b = *(pb.second);
    b.setSignedness();
    auto pcOut = firstOut();
    auto &cOut = *(pcOut.second);
    auto pcIn = firstIn();
    auto &cIn = *(pcIn.second);

    WIR_L4 cmp = ( a == b.extend( 32 ) );

    bitOp_DDC9_3( cOut, cIn );

    if ( cOut.at( 0 ) == WIR_L4::bX ) {
      // If cOut[ 0 ] is X, set cIn[ 0 ], a and b to X.
      cIn.setBit( 0, WIR_L4::bX );
      a.setAllBits( WIR_L4::bX );
      b.setAllBits( WIR_L4::bX );
    }

    if ( cmp == WIR_L4::b0 ) {
      // If a and b are inequal, we simply determine one bit position where a
      // and b provably differ and set all other bits to X.
      auto bExt = b.extend( 32 );
      unsigned int diffPos = 0;

      for ( unsigned int i = 0; i < a.getBitWidth(); ++i )
        if ( ( getLevel( a[ i ] ) == 2 ) && ( getLevel( bExt[ i ] ) == 2 ) &&
             ( a[ i ] != bExt[ i ] ) ) {
          diffPos = i;
          break;
        } else

        if ( ( getLevel( a[ i ] ) == 1 ) && ( getLevel( bExt[ i ] ) == 1 ) &&
             ( a[ i ] != bExt[ i ] ) &&
             ( a.getLocation( i ) == bExt.getLocation( i ) ) ) {
          diffPos = i;
          break;
        }

      a.setAllBits( WIR_L4::bX );
      a.setBit( diffPos, WIR_L4::b0 );

      b.setAllBits( WIR_L4::bX );
      if ( diffPos < b.getBitWidth() )
        b.setBit( diffPos, WIR_L4::b0 );
      else
        b.setBit( b.getBitWidth() - 1, WIR_L4::b0 );
    }

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b } );
    results.insert( { pcIn.first, cIn } );
  }

  if ( ( o.getOpCode() == TC13::OpCode::XOR_GE ) ||
       ( o.getOpCode() == TC13::OpCode::XOR_GE_U ) ||
       ( o.getOpCode() == TC13::OpCode::XOR_LT ) ||
       ( o.getOpCode() == TC13::OpCode::XOR_LT_U ) ) {
    auto pa = nthIn( 1 );
    auto &a = *(pa.second);
    auto pb = lastIn();
    auto &b = *(pb.second);
    auto pcOut = firstOut();
    auto &cOut = *(pcOut.second);
    auto pcIn = firstIn();
    auto &cIn = *(pcIn.second);

    bitOp_DDC9_3( cOut, cIn );

    if ( cOut.at( 0 ) == WIR_L4::bX ) {
      // If cOut[ 0 ] is X, set cIn[ 0 ], a and b to X.
      cIn.setBit( 0, WIR_L4::bX );
      a.setAllBits( WIR_L4::bX );
      b.setAllBits( WIR_L4::bX );
    }

    results.insert( { pa.first, a } );
    results.insert( { pb.first, b } );
    results.insert( { pcIn.first, cIn } );
  }

};


/*
  ssov realizes saturation of a given up/down value on signed overflow.

  According to the TriCore Architecture User's Manual Volume 2 (Instruction
  Set), page 2-11, ssov is defined as follows:

  ssov(x, y) = max_pos = (1 << (y - 1)) - 1;
               max_neg = -(1 << (y - 1));
               return(
                 (x > max_pos) ? max_pos :
                   ((x < max_neg) ? max_neg : x) );

  In order to properly detect whether the original up/down value v exceeds the
  given bit width's min/max bounds, v must be signed and one bit wider than the
  given bit width. I.e., if a signed saturation for 32 bits is desired, then v
  must be 33 (!) bits wide. ssov asserts if this is not the case.

  However, the result returned by ssov is ensured to have a width of w bits.
*/
WIR_UpDownValue TC_BitDFA::ssov( const WIR_UpDownValue &v,
                                 unsigned int w ) const
{
  DSTART(
    "WIR_UpDownValue TC_BitDFA::ssov(const WIR_UpDownValue&, unsigned int) const" );

  #ifdef FAILSAFEMODE
  ufAssert( v.isSigned() && ( v.getBitWidth() == w + 1 ) );
  #endif

  if ( v.isPositive() && ( v.at( w - 1 ) == WIR_L4::b1 ) ) {
    // v is meant to be a positive value, but the most-significant bit w-1 is 1,
    // indicating a negative value in 2's complement. Thus, a signed overflow
    // occured, saturate to max_pos.
    WIR_UpDownValue max_pos { WIR_L4::b1, w, true };
    max_pos[ w - 1 ] = WIR_L4::b0;
    DOUT( "Saturating '" << v << "' to max_pos (" << max_pos << ")." << endl );
    return( max_pos );
  }

  if ( v.isNegative() && ( v.at( w - 1 ) == WIR_L4::b0 ) ) {
    WIR_UpDownValue max_neg { WIR_L4::b0, w, true };
    max_neg[ w - 1 ] = WIR_L4::b1;
    DOUT( "Saturating '" << v << "' to max_neg (" << max_neg << ")." << endl );
    return( max_neg );
  }

  if ( ( v.isPositive() || v.isNegative() ) &&
       ( v.at( w ) == v.at( w - 1 ) ) ) {
    // v is meant to be positive and the most-significant bit w-1 is 0, or v is
    // meant to be negative and the most-significant bit w-1 is 1. So, intended
    // and effective signedness match, no saturation is necessary. Simply return
    // v with width w.
    DOUT( "No saturation applied to '" << v << "'." << endl );
    return( v.extract( 0, w ) );
  }

  DOUT( "Saturation unknown for '" << v << "'." << endl );
  return( WIR_UpDownValue { WIR_L4::bU, w, true } );
};


/*
  suov realizes saturation of a given up/down value on unsigned overflow.

  According to the TriCore Architecture User's Manual Volume 2 (Instruction
  Set), page 2-11, suov is defined as follows:

  suov(x, y) = max_pos = (1 << y) - 1;
               return( (x > max_pos) ? max_pos : ((x < 0) ? 0 : x) );

  In order to properly detect whether the original up/down value v exceeds the
  given bit width's min/max bounds, v must be one bit wider than the given bit
  width. I.e., if an unsigned saturation for 32 bits is desired, then v must be
  33 (!) bits wide. suov asserts if this is not the case.

  However, the result returned by suov is ensured to have a width of w bits.
*/
WIR_UpDownValue TC_BitDFA::suov( const WIR_UpDownValue &v,
                                 unsigned int w ) const
{
  DSTART(
    "WIR_UpDownValue TC_BitDFA::suov(const WIR_UpDownValue&, unsigned int) const" );

  #ifdef FAILSAFEMODE
  ufAssert( v.getBitWidth() == w + 1 );
  #endif

  WIR_UpDownValue max_pos { WIR_L4::b1, w, false };

  if ( ( getLevel( v.at( w ) ) == 0 ) || ( getLevel( v.at( w ) ) == 1 ) ) {
    DOUT( "Saturation unknown for '" << v << "'." << endl );
    // The signedness of v is unknown. We thus return U*.
    return( WIR_UpDownValue { WIR_L4::bU, w, false } );
  }

  if ( v.isUnsigned() &&
       ( ( v.at( w ) == WIR_L4::b1 ) || ( v.at( w ) == WIR_L4::bX ) ) ) {
    // v is meant to be unsigned and the most-significant bit can be interpreted
    // as 1. We thus saturate to max_pos.
    WIR_UpDownValue max_pos { WIR_L4::b1, w, false };
    DOUT( "Saturating '" << v << "' to max_pos (" << max_pos << ")." << endl );
    return( max_pos );
  }

  if ( ( v.at( w ) == WIR_L4::b1 ) || ( v.at( w ) == WIR_L4::bX ) ) {
    // v is negative or don't care. We thus saturate to 0.
    DOUT( "Saturating '" << v << "' to 0." << endl );
    return( WIR_UpDownValue { WIR_L4::b0, w, false } );
  }

  DOUT( "No saturation applied to '" << v << "'." << endl );
  auto res = v.extract( 0, w );
  res.setSignedness( false );
  return( res );
};


/*
  leading_ones returns the number of leading ones of a given up/down value.
*/
WIR_UpDownValue TC_BitDFA::leading_ones( const WIR_UpDownValue &v ) const
{
  DSTART(
    "WIR_UpDownValue TC_BitDFA::leading_ones(const WIR_UpDownValue&) const" );

  unsigned int ones = 0;

  for ( int i = v.getBitWidth() - 1; i >= 0; --i ) {
    if ( ( v.at( i ) == WIR_L4::b1 ) || ( v.at( i ) == WIR_L4::bX ) )
      ++ones;
    else

    if ( v.at( i ) == WIR_L4::b0 )
      break;
    else
      return( WIR_UpDownValue { WIR_L4::bU, 6, false } );
  }

  return( WIR_UpDownValue { TC_Const8_Unsigned( ones ) } );
};


/*
  leading_signs returns the number of leading sign bits of a given up/down
  value.

  The value returned by leading_signs is the number of leading sign bits minus
  one, giving the number of redundant sign bits in v.
*/
WIR_UpDownValue TC_BitDFA::leading_signs( const WIR_UpDownValue &v ) const
{
  DSTART(
    "WIR_UpDownValue TC_BitDFA::leading_signs(const WIR_UpDownValue&) const" );

  WIR_L4 signBit = v.at( v.getBitWidth() - 1 );
  if ( ( signBit != WIR_L4::b0 ) && ( signBit != WIR_L4::b1 ) )
    return( WIR_UpDownValue { WIR_L4::bU, 6, false } );

  unsigned int signs = 0;

  for ( int i = v.getBitWidth() - 1; i >= 0; --i ) {
    if ( ( v.at( i ) == signBit ) || ( v.at( i ) == WIR_L4::bX ) )
      ++signs;
    else

    if ( v.at( i ) == ~signBit )
      break;
    else
      return( WIR_UpDownValue { WIR_L4::bU, 6, false } );
  }

  return( WIR_UpDownValue { TC_Const8_Unsigned( signs - 1 ) } );
};


/*
  leading_zeros returns the number of leading zeros of a given up/down value.
*/
WIR_UpDownValue TC_BitDFA::leading_zeros( const WIR_UpDownValue &v ) const
{
  DSTART(
    "WIR_UpDownValue TC_BitDFA::leading_zeros(const WIR_UpDownValue&) const" );

  unsigned int zeros = 0;

  for ( int i = v.getBitWidth() - 1; i >= 0; --i ) {
    if ( ( v.at( i ) == WIR_L4::b0 ) || ( v.at( i ) == WIR_L4::bX ) )
      ++zeros;
    else

    if ( v.at( i ) == WIR_L4::b1 )
      break;
    else
      return( WIR_UpDownValue { WIR_L4::bU, 6, false } );
  }

  return( WIR_UpDownValue { TC_Const8_Unsigned( zeros ) } );
};


/*
  reverse returns the bit-reverse of a given up/down value.

  Bit width and signedness of the result are the same as of the argument value
  v.
*/
WIR_UpDownValue TC_BitDFA::reverse( const WIR_UpDownValue &v ) const
{
  DSTART( "WIR_UpDownValue TC_BitDFA::reverse(const WIR_UpDownValue&) const" );

  WIR_UpDownValue res { WIR_L4::bU, v.getBitWidth(), v.isSigned() };

  for ( unsigned int i = 0; i < v.getBitWidth(); ++i )
    if ( getLevel( v.at( i ) ) == 1 )
      res.setBit( v.getBitWidth() - i - 1, v.at( i ), v.getLocation( i ) );
    else
      res.setBit( v.getBitWidth() - i - 1, v.at( i ) );

  return( res );
};


/*
  arithOp_ADD identifies common don't care bits for arithmetical addition
  operations.

  During bottom-up analysis, arithOp_ADD performs the following steps:
  - Iterate all bits of cOut from the most-significant down to the least-
    significant bit.
  - If the current bit of cOut is X, set the same bits of a and b to X.
  - Otherwise exit.
*/
void TC_BitDFA::arithOp_ADD( const WIR_UpDownValue &cOut, WIR_UpDownValue &a,
                             WIR_UpDownValue &b ) const
{
  DSTART(
    "void TC_BitDFA::arithOp_ADD(const WIR_UpDownValue&, WIR_UpDownValue&, WIR_UpDownValue&) const" );

  for ( int i = cOut.getBitWidth() - 1; i >= 0; --i )
    if ( cOut.at( i ) == WIR_L4::bX ) {
      a.setBit( i, WIR_L4::bX );
      b.setBit( i, WIR_L4::bX );
    } else
      return;
};


/*
  arithOp_SH identifies common don't care bits for shift operations.

  During bottom-up analysis, arithOp_SH performs the following steps:
  - Nothing is done if the shift amount contains U, L or N bits.
  - X bits in the shift amount are interpreted either as 1 for positive shift
    amount values or as 0 for negative shift amount values.
  - If the shift amount is greater-equal 0 and keepShiftedOutBits is false, the
    most-significant shamt bits of a are set to X.
  - Otherwise, the least-significant shamt bits of a are set to X if
    keepShiftedOutBits is false.
  - If a bit of cOut is X, the corresponding bit of a at reverse-shifted
    position is set to X.
*/
void TC_BitDFA::arithOp_SH( const WIR_UpDownValue &cOut, WIR_UpDownValue &a,
                            WIR_UpDownValue &shamt,
                            bool keepShiftedOutBits ) const
{
  DSTART(
    "void TC_BitDFA::arithOp_SH(const WIR_UpDownValue&, WIR_UpDownValue&, WIR_UpDownValue&, bool) const" );

  // Determine whether shift amount is a positive or negative integer number.
  bool positiveShamt = ( shamt[ 5 ] != WIR_L4::b1 );

  // Iterate the shift amount.
  for ( unsigned int i = 0; i < 6; ++i ) {
    // If we encounter a 'U', 'L' or 'N' bit in the shift amount, we don't
    // know anything about this shift operation.
    if ( ( shamt[ i ] == WIR_L4::bU ) || ( shamt[ i ] == WIR_L4::bL ) ||
         ( shamt[ i ] == WIR_L4::bN ) )
      return;

    // Replace 'X' either by '1' or '0', depending on whether the shift amount
    // is a positive number or not.
    if ( shamt[ i ] == WIR_L4::bX ) {
      if ( i == 5 )
        // Special handling of the most-significant sign bit that is 'X' here.
        shamt[ i ] = WIR_L4::b0;
      else
        shamt[ i ] = positiveShamt ? WIR_L4::b1 : WIR_L4::b0;
    }
  }

  // Finally, get the shift amount's value.
  signed long long shiftVal = shamt.getSignedValue();

  auto signBit = a[ a.getBitWidth() - 1 ];
  const WIR_Location *l =
    ( getLevel( signBit ) == 1 ) ?
      &( a.getLocation( a.getBitWidth() - 1 ) ) : nullptr;

  if ( shiftVal >= 0 ) {
    if ( !keepShiftedOutBits )
      // Positive shift amount: Set most-significant bits of a to X.
      for ( unsigned int i = 1; i <= shiftVal; ++i )
        a.setBit( a.getBitWidth() - i, WIR_L4::bX );
  } else {
    if ( !keepShiftedOutBits )
      // Negative shift amount: Set least-significant bits of a to X.
      for ( unsigned int i = 0; i < -shiftVal; ++i )
        a.setBit( i, WIR_L4::bX );

    // Ensure that the sign bit is preserved if it's an arithmetical right shift
    // by 32 positions (or 16 for halfword shifts, resp.).
    if ( a.isSigned() && ( shiftVal == -((int) a.getBitWidth()) ) ) {
      if ( getLevel( signBit ) == 1 )
        a.setBit( a.getBitWidth() - 1, signBit, *l );
      else
        a.setBit( a.getBitWidth() - 1, signBit );
    }
  }

  // Propagate X bits from cOut to a.
  for ( unsigned int i = 0; i < cOut.getBitWidth(); ++i )
    if ( cOut.at( i ) == WIR_L4::bX ) {
      int aPos = i - shiftVal;
      bool isSHARight = ( a.isSigned() && ( shiftVal < 0 ) );

      if ( ( aPos >= 0 ) && ( (unsigned int) aPos < a.getBitWidth() ) &&
           !( isSHARight && ( (unsigned int) aPos == a.getBitWidth() - 1 ) ) )
        // Set the corresponding bit of a to X, except a's sign bit if this is
        // an arithmetical right-shift.
        a.setBit( aPos, WIR_L4::bX );
    }
};


/*
  bitOp_DDC5DC5_1 identifies common don't care bits for bitwise logical TriCore
  operations AND.T, ANDN.T, NAND.T, NOR.T, OR.T, ORN.T, XNOR.T and XOR.T having
  format DDC5DC5_1.

  During bottom-up analysis, bitOp_DDC5DC5_1 performs the following steps:
  - All bits of a are set to X, except bit a[ pos1 ].
  - All bits of b are set to X, except bit b[ pos2 ].
  - a[ pos1 ] and b[ pos2 ] are set to X if cOut[ 0 ] is X.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
void TC_BitDFA::bitOp_DDC5DC5_1( const WIR_UpDownValue &cOut,
                                 WIR_UpDownValue &a, unsigned int pos1,
                                 WIR_UpDownValue &b, unsigned int pos2 ) const
{
  DSTART(
    "void TC_BitDFA::bitOp_DDC5DC5_1(const WIR_UpDownValue&, WIR_UpDownValue&, unsigned int, WIR_UpDownValue&, unsigned int) const" );

  for ( unsigned int i = 0; i < 32; ++i ) {
    if ( i != pos1 )
      // For a, all bits except a[ pos1 ] are X.
      a.setBit( i, WIR_L4::bX );

    if ( i != pos2 )
      // For b, all bits except b[ pos2 ] are X.
      b.setBit( i, WIR_L4::bX );
  }

  if ( cOut.at( 0 ) == WIR_L4::bX ) {
    a.setBit( pos1, WIR_L4::bX );
    b.setBit( pos2, WIR_L4::bX );
  }
};


/*
  bitOp_DDC5DC5_2 identifies common don't care bits for bitwise logical TriCore
  operations of format DDC5DC5_2.

  During bottom-up analysis, bitOp_DDC5DC5_2 performs the following steps:
  - All bits of a are set to X, except bit a[ pos1 ].
  - All bits of b are set to X, except bit b[ pos2 ].
  - All those bits of cIn are set to X that are already X in cOut.
*/
void TC_BitDFA::bitOp_DDC5DC5_2( const WIR_UpDownValue &cOut,
                                 WIR_UpDownValue &cIn,
                                 WIR_UpDownValue &a, unsigned int pos1,
                                 WIR_UpDownValue &b, unsigned int pos2 ) const
{
  DSTART(
    "void TC_BitDFA::bitOp_DDC5DC5_2(const WIR_UpDownValue&, WIR_UpDownValue&, WIR_UpDownValue&, unsigned int, WIR_UpDownValue&, unsigned int) const" );

  for ( unsigned int i = 0; i < 32; ++i ) {
    if ( i != pos1 )
      // For a, all bits except a[ pos1 ] are X.
      a.setBit( i, WIR_L4::bX );

    if ( i != pos2 )
      // For b, all bits except b[ pos2 ] are X.
      b.setBit( i, WIR_L4::bX );

    if ( cOut.at( i ) == WIR_L4::bX )
      // If a bit in cOut is already X, then propagate it into cIn.
      cIn.setBit( i, WIR_L4::bX );
  }
};


/*
  bitOp_AND3 performs bottom-up simulation of the ternary AND operator found in
  some bitwise logical TriCore operations of format DDC5DC5_2.

  During bottom-up analysis, bitOp_AND3 performs the following steps:
  - If b[ pos2 ] is 0, set cIn[ 0 ] and a[ pos1 ] to X.
  - Otherwise, if a[ pos1 ] is 0, set cIn[ 0 ] and b[ pos2 ] to X.
  - Otherwise, if cIn[ 0 ] is 0, set a[ pos1 ] and b[ pos2 ] to X.
  - If cOut[ 0 ] is X, set cIn[ 0 ], a[ pos1 ] and b[ pos2 ] to X.
*/
void TC_BitDFA::bitOp_AND3( const WIR_UpDownValue &cOut, WIR_UpDownValue &cIn,
                            WIR_UpDownValue &a, unsigned int pos1,
                            WIR_UpDownValue &b, unsigned int pos2 ) const
{
  DSTART(
    "void TC_BitDFA::bitOp_AND3(const WIR_UpDownValue&, WIR_UpDownValue&, WIR_UpDownValue&, unsigned int, WIR_UpDownValue&, unsigned int) const" );

  DOUT(
    "cOut = " << cOut << endl <<
    "cIn = " << cIn << endl <<
    "a = " << a << " / " << pos1 << endl <<
    "b = " << b << " / " << pos2 << endl );

  if ( b.at( pos2 ) == WIR_L4::b0 ) {
    // If b[ pos2 ] is 0, set cIn[ 0 ] and a[ pos1 ] to X.
    cIn.setBit( 0, WIR_L4::bX );
    a.setBit( pos1, WIR_L4::bX );
    DOUT( "Case 1: Setting cIn[ 0 ] and a[ " << pos1 << " ] to X." << endl );
  } else

  if ( a.at( pos1 ) == WIR_L4::b0 ) {
    // If a[ pos1 ] is 0, set cIn[ 0 ] and b[ pos2 ] to X.
    cIn.setBit( 0, WIR_L4::bX );
    b.setBit( pos2, WIR_L4::bX );
    DOUT( "Case 2: Setting cIn[ 0 ] and b[ " << pos2 << " ] to X." << endl );
  } else

  if ( cIn.at( 0 ) == WIR_L4::b0 ) {
    // If cIn[ 0 ] is 0, set a[ pos1 ] and b[ pos2 ] to X.
    a.setBit( pos1, WIR_L4::bX );
    b.setBit( pos2, WIR_L4::bX );
    DOUT(
      "Case 3: Setting a[ " << pos1 << " ] and b [ " << pos2 << " ] to X." <<
      endl );
  }

  if ( cOut.at( 0 ) == WIR_L4::bX ) {
    // If cOut[ 0 ] is X, set cIn[ 0 ], a[ pos1 ] and b[ pos2 ] to X.
    cIn.setBit( 0, WIR_L4::bX );
    a.setBit( pos1, WIR_L4::bX );
    b.setBit( pos2, WIR_L4::bX );
    DOUT(
      "Case 4: Setting cIn[ 0 ], a[ " << pos1 << " ] and b [ " << pos2 <<
      " ] to X." << endl );
  }
};


/*
  bitOp_ANDN3 performs bottom-up simulation of the ternary AND operator found in
  some bitwise logical TriCore operations of format DDC5DC5_2.

  During bottom-up analysis, bitOp_AND3 performs the following steps:
  - If b[ pos2 ] is 1, set cIn[ 0 ] and a[ pos1 ] to X.
  - Otherwise, if a[ pos1 ] is 0, set cIn[ 0 ] and b[ pos2 ] to X.
  - Otherwise, if cIn[ 0 ] is 0, set a[ pos1 ] and b[ pos2 ] to X.
  - If cOut[ 0 ] is X, set cIn[ 0 ], a[ pos1 ] and b[ pos2 ] to X.
*/
void TC_BitDFA::bitOp_ANDN3( const WIR_UpDownValue &cOut, WIR_UpDownValue &cIn,
                             WIR_UpDownValue &a, unsigned int pos1,
                             WIR_UpDownValue &b, unsigned int pos2 ) const
{
  DSTART(
    "void TC_BitDFA::bitOp_ANDN3(const WIR_UpDownValue&, WIR_UpDownValue&, WIR_UpDownValue&, unsigned int, WIR_UpDownValue&, unsigned int) const" );

  DOUT(
    "cOut = " << cOut << endl <<
    "cIn = " << cIn << endl <<
    "a = " << a << " / " << pos1 << endl <<
    "b = " << b << " / " << pos2 << endl );

  if ( b.at( pos2 ) == WIR_L4::b1 ) {
    // If b[ pos2 ] is 1, set cIn[ 0 ] and a[ pos1 ] to X.
    cIn.setBit( 0, WIR_L4::bX );
    a.setBit( pos1, WIR_L4::bX );
    DOUT( "Case 1: Setting cIn[ 0 ] and a[ " << pos1 << " ] to X." << endl );
  } else

  if ( a.at( pos1 ) == WIR_L4::b0 ) {
    // If a[ pos1 ] is 0, set cIn[ 0 ] and b[ pos2 ] to X.
    cIn.setBit( 0, WIR_L4::bX );
    b.setBit( pos2, WIR_L4::bX );
    DOUT( "Case 2: Setting cIn[ 0 ] and b[ " << pos2 << " ] to X." << endl );
  } else

  if ( cIn.at( 0 ) == WIR_L4::b0 ) {
    // If cIn[ 0 ] is 0, set a[ pos1 ] and b[ pos2 ] to X.
    a.setBit( pos1, WIR_L4::bX );
    b.setBit( pos2, WIR_L4::bX );
    DOUT(
      "Case 3: Setting a[ " << pos1 << " ] and b [ " << pos2 << " ] to X." <<
      endl );
  }

  if ( cOut.at( 0 ) == WIR_L4::bX ) {
    // If cOut[ 0 ] is X, set cIn[ 0 ], a[ pos1 ] and b[ pos2 ] to X.
    cIn.setBit( 0, WIR_L4::bX );
    a.setBit( pos1, WIR_L4::bX );
    b.setBit( pos2, WIR_L4::bX );
    DOUT(
      "Case 4: Setting cIn[ 0 ], a[ " << pos1 << " ] and b [ " << pos2 <<
      " ] to X." << endl );
  }
};


/*
  bitOp_DDC9_3 identifies common don't care bits for bitwise logical TriCore
  operations of format DDC9_3.

  During bottom-up analysis, bitOp_DDC9_3 performs the following steps:
  - All those bits of cIn are set to X that are already X in cOut.
*/
void TC_BitDFA::bitOp_DDC9_3( const WIR_UpDownValue &cOut,
                              WIR_UpDownValue &cIn ) const
{
  DSTART(
    "void TC_BitDFA::bitOp_DDC9_3(const WIR_UpDownValue&, WIR_UpDownValue&) const" );

  for ( unsigned int i = 0; i < 32; ++i )
    if ( cOut.at( i ) == WIR_L4::bX )
      // If a bit in cOut is already X, then propagate it into cIn.
      cIn.setBit( i, WIR_L4::bX );
};


/*
  bitOp_AND_CMP performs bottom-up simulation of the AND.{EQ|GE|...} family of
  logical TriCore operations.

  During bottom-up analysis, bitOp_AND_CMP performs the following steps:
  - If cIn[ 0 ] is 0, set a and b to X.
  - Otherwise, if cmp is 0, set cIn[ 0 ] to X.
  - If cOut[ 0 ] is X, set cIn[ 0 ], a and b to X.
*/
void TC_BitDFA::bitOp_AND_CMP( const WIR_UpDownValue &cOut,
                               WIR_UpDownValue &cIn, WIR_UpDownValue &a,
                               WIR_UpDownValue &b, WIR_L4 cmp ) const
{
  DSTART(
    "void TC_BitDFA::bitOp_AND_CMP(const WIR_UpDownValue&, WIR_UpDownValue&, WIR_UpDownValue&, WIR_UpDownValue&, WIR_L4) const" );

  DOUT(
    "cOut = " << cOut << endl <<
    "cIn = " << cIn << endl <<
    "a = " << a << endl <<
    "b = " << b << endl <<
    "cmp = " << cmp << endl );

  if ( cIn.at( 0 ) == WIR_L4::b0 ) {
    // If cIn[ 0 ] is 0, set a and b to X.
    a.setAllBits( WIR_L4::bX );
    b.setAllBits( WIR_L4::bX );
    DOUT( "Case 1: Setting a and b to X." << endl );
  } else

  if ( cmp == WIR_L4::b0 ) {
    // If cmp is 0, set cIn[ 0 ] to X.
    cIn.setBit( 0, WIR_L4::bX );
    DOUT( "Case 2: Setting cIn[ 0 ] to X." << endl );
  }

  if ( cOut.at( 0 ) == WIR_L4::bX ) {
    // If cOut[ 0 ] is X, set cIn[ 0 ], a and b to X.
    cIn.setBit( 0, WIR_L4::bX );
    a.setAllBits( WIR_L4::bX );
    b.setAllBits( WIR_L4::bX );
    DOUT( "Case 3: Setting cIn[ 0 ], a and b to X." << endl );
  }
};

/*
  bitOp_OR_CMP performs bottom-up simulation of the OR.{EQ|GE|...} family of
  logical TriCore operations.

  During bottom-up analysis, bitOp_OR_CMP performs the following steps:
  - If cIn[ 0 ] is 1, set a and b to X.
  - Otherwise, if cmp is 1, set cIn[ 0 ] to X.
  - If cOut[ 0 ] is X, set cIn[ 0 ], a and b to X.
*/
void TC_BitDFA::bitOp_OR_CMP( const WIR_UpDownValue &cOut, WIR_UpDownValue &cIn,
                              WIR_UpDownValue &a, WIR_UpDownValue &b,
                              WIR_L4 cmp ) const
{
  DSTART(
    "void TC_BitDFA::bitOp_OR_CMP(const WIR_UpDownValue&, WIR_UpDownValue&, WIR_UpDownValue&, WIR_UpDownValue&, WIR_L4) const" );

  DOUT(
    "cOut = " << cOut << endl <<
    "cIn = " << cIn << endl <<
    "a = " << a << endl <<
    "b = " << b << endl <<
    "cmp = " << cmp << endl );

  if ( cIn.at( 0 ) == WIR_L4::b1 ) {
    // If cIn[ 0 ] is 1, set a and b to X.
    a.setAllBits( WIR_L4::bX );
    b.setAllBits( WIR_L4::bX );
    DOUT( "Case 1: Setting a and b to X." << endl );
  } else

  if ( cmp == WIR_L4::b1 ) {
    // If cmp is 1, set cIn[ 0 ] to X.
    cIn.setBit( 0, WIR_L4::bX );
    DOUT( "Case 2: Setting cIn[ 0 ] to X." << endl );
  }

  if ( cOut.at( 0 ) == WIR_L4::bX ) {
    // If cOut[ 0 ] is X, set cIn[ 0 ], a and b to X.
    cIn.setBit( 0, WIR_L4::bX );
    a.setAllBits( WIR_L4::bX );
    b.setAllBits( WIR_L4::bX );
    DOUT( "Case 3: Setting cIn[ 0 ], a and b to X." << endl );
  }
};


/*
  shiftOp_DDC5DC5_2 identifies common don't care bits for TriCore shift
  operations of format DDC5DC5_2.

  During bottom-up analysis, shiftOp_DDC5DC5_2 performs the following steps:
  - All bits of a are set to X, except bit a[ pos1 ].
  - All bits of b are set to X, except bit b[ pos2 ].
*/
void TC_BitDFA::shiftOp_DDC5DC5_2( WIR_UpDownValue &a, unsigned int pos1,
                                   WIR_UpDownValue &b, unsigned int pos2 ) const
{
  DSTART(
    "void TC_BitDFA::shiftOp_DDC5DC5_2(WIR_UpDownValue&, unsigned int, WIR_UpDownValue&, unsigned int) const" );

  for ( unsigned int i = 0; i < 32; ++i ) {
    if ( i != pos1 )
      // For a, all bits except a[ pos1 ] are X.
      a.setBit( i, WIR_L4::bX );

    if ( i != pos2 )
      // For b, all bits except b[ pos2 ] are X.
      b.setBit( i, WIR_L4::bX );
  }
};

}       // namespace WIR
