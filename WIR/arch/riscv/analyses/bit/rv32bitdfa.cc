/*

   This source file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Intermediate Representation Framework

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
  @file rv32bitdfa.cc
  @brief This file implements a RISC-V-specific bit-true data flow analysis.

  @author Maurice Hoffmann <Maurice.Hoffmann@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include standard headers
#include <iterator>
#include <sstream>
#include <utility>

// Include libuseful headers
#include <libuseful/debugmacros.h>

// Include WIR headers
#include <wir/wir.h>
#include <arch/riscv/rv32imc.h>

// Include local headers
#include "rv32bitdfa.h"


//
// Code section
//

namespace WIR {


using namespace boost;
using namespace std;


//
// Public class methods
//

/*
  Default constructor for function-level analysis.
*/
RV32_BitDFA::RV32_BitDFA( WIR_Function &f ) :
  WIR_BitDFA { f }
{
  DSTART( "RV32_BitDFA::RV32_BitDFA(WIR_Function&)" );
};


/*
  Destructor.
*/
RV32_BitDFA::~RV32_BitDFA( void )
{
  DSTART( "virtual RV32_BitDFA::~RV32_BitDFA()" );
};


//
// Protected class methods
//

/*
  simulateTopDown performs the RISC-V-specific top-down simulation of the given
  WIR operation.

  For a documentation of the semantics of RISC-V operations, please refer to
  the RISC-V Instruction Set Manual Volume I: User-Level ISA.
*/
void RV32_BitDFA::simulateTopDown( const WIR_Operation &o,
                                   std::map<WIR_id_t, WIR_UpDownValue> &operands,
                                   std::map<WIR_id_t, WIR_UpDownValue> &results )
{
  DSTART(
    "virtual void RV32_BitDFA::simulateTopDown(const WIR_Operation&, "
    "map<long long unsigned int, WIR_UpDownValue>&, "
    "map<long long unsigned int, WIR_UpDownValue>&)" );

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


  /*
    RV32I
  */

  //
  // ADD & co.
  //

  // ADD performs an addition. Overflows are ignored and the least-significant
  // XLEN bits of results are written to the destination.
  if ( o.getOpCode() == RV32I::OpCode::ADD ) {
    nthRes( 0 ) = nthOp( 1 ) + lastOp();
    nthRes( 0 ).setSignedness();
  }

  // ADDI adds the sign-extended 12-bit immediate to register rs1. Arithmetic
  // overflow is ignored and the result is simply the least-significant XLEN
  // bits of the result. ADDI rd, rs1, 0 is used to implement the MV rd, rs1
  // assembler pseudo-instruction.
  if ( o.getOpCode() == RV32I::OpCode::ADDI ) {
    // Get last operand (sign-extended 12-bit immediate).
    auto &x = lastOp();
    x.setSignedness();

    nthRes( 0 ) = nthOp( 1 ) + x.extend( 32 );
    nthRes( 0 ).setSignedness();
  }


  //
  // AND & co.
  //

  // AND performs bitwise logical operations.
  if ( o.getOpCode() == RV32I::OpCode::AND ) {
    nthRes( 0 ) = nthOp( 1 ) & lastOp();
    nthRes( 0 ).setSignedness( false );
  }

  // ANDI performs bitwise AND on register rs1 and the sign-extended 12-bit
  // immediate and places the result in rd.
  if ( o.getOpCode() == RV32I::OpCode::ANDI ) {
    // Get last operand (sign-extended 12-bit immediate).
    auto &x = lastOp();
    x.setSignedness();

    nthRes( 0 ) = nthOp( 1 ) & x.extend( 32 );
    nthRes( 0 ).setSignedness( false );
  }


  //
  // LBU / LHU
  //

  // LBU loads an 8-bit value from memory and zero-extends it to 32-bits before
  // storing in rd.
  if ( o.getOpCode() == RV32I::OpCode::LBU ) {
    // Least-significant 8 bits are unknown, most-significant bits are all 0.
    nthRes( 0 ) = { WIR_L4::b0, 32, false };
    WIR_UpDownValue u { WIR_L4::bU, 8, false };
    insert( nthRes( 0 ), u, 0 );
  }

  // LHU loads a 16-bit value from memory and zero-extends it to 32-bits before
  // storing in rd.
  if ( o.getOpCode() == RV32I::OpCode::LHU ) {
    // Least-significant 16 bits are unknown, most-significant bits are all 0.
    nthRes( 0 ) = { WIR_L4::b0, 32, false };
    WIR_UpDownValue u { WIR_L4::bU, 16, false };
    insert( nthRes( 0 ), u, 0 );
  }


  //
  // LUI
  //

  // LUI (load upper immediate) is used to build 32-bit constants and uses the
  // U-type format. LUI places the immediate value in the most-significant 20
  // bits of the destination register rd, filling in the least-significant 12
  // bits with zeros.
  if ( o.getOpCode() == RV32I::OpCode::LUI )
    nthRes( 0 ) = lastOp().extend( 32 ) << 12;


  //
  // MOV (pseudo-operation)
  //

  // MOV copies the value in register rs2 into register rd.
  if ( o.getOpCode() == RV32I::OpCode::MOV )
    nthRes( 0 ) = lastOp();


  //
  // OR & co.
  //

  // OR performs bitwise logical operations.
  if ( o.getOpCode() == RV32I::OpCode::OR ) {
    nthRes( 0 ) = nthOp( 1 ) | lastOp();
    nthRes( 0 ).setSignedness( false );
  }

  // ORI performs bitwise OR on register rs1 and the sign-extended 12-bit
  // immediate and places the result in rd.
  if ( o.getOpCode() == RV32I::OpCode::ORI ) {
    // Get last operand (sign-extended 12-bit immediate).
    auto &x = lastOp();
    x.setSignedness();

    nthRes( 0 ) = nthOp( 1 ) | x.extend( 32 );
    nthRes( 0 ).setSignedness( false );
  }


  //
  // SL & co.
  //

  // SLL performs logical left shift of the value in register rs1 by the shift
  // amount held in the least-significant 5 bits of register rs2.
  if ( o.getOpCode() == RV32I::OpCode::SLL ) {
    auto shamt = lastOp().extract( 0, 5 );
    shamt.setSignedness( false );

    // Enforce logical shifting.
    auto &a = nthOp( 1 );
    a.setSignedness( false );

    nthRes( 0 ) = a << shamt;
  }

  // SLLI performs logical left shift by a shift amount given as immediate.
  if ( o.getOpCode() == RV32I::OpCode::SLLI ) {
    auto shamt = lastOp();
    shamt.setSignedness( false );

    // Enforce logical shifting.
    auto &a = nthOp( 1 );
    a.setSignedness( false );

    nthRes( 0 ) = a << shamt;
  }


  //
  // SLT & co.
  //

  // SLT/SLTU performs signed/unsigned comparisons writing 1 to rd if rs1 < rs2,
  // 0 otherwise.
  if ( ( o.getOpCode() == RV32I::OpCode::SLT ) ||
       ( o.getOpCode() == RV32I::OpCode::SLTU ) ) {
    auto &a = nthOp( 1 );
    a.setSignedness( o.getOpCode() == RV32I::OpCode::SLT );
    auto &b = lastOp();
    b.setSignedness( o.getOpCode() == RV32I::OpCode::SLT );

    nthRes( 0 ) = { WIR_L4::b0, 32, false };

    if ( nthReg( 1 ) == lastReg() )
      // Special case: SLT Dc, Da, Da = 0
      nthRes( 0 )[ 0 ] = WIR_L4::b0;
    else
      nthRes( 0 )[ 0 ] = ( a < b );
  }

  // SLTI/SLTIU write a 1 in register rd if register rs1 is less than the sign-
  // or zero- extended immediate, respectively , or 0 otherwise.
  if ( ( o.getOpCode() == RV32I::OpCode::SLTI ) ||
       ( o.getOpCode() == RV32I::OpCode::SLTIU ) ) {
    auto &a = nthOp( 1 );
    a.setSignedness( o.getOpCode() == RV32I::OpCode::SLTI );
    auto &b = lastOp();
    b.setSignedness( o.getOpCode() == RV32I::OpCode::SLTI );

    nthRes( 0 ) = { WIR_L4::b0, 32, false };
    nthRes( 0 )[ 0 ] = ( a < b.extend( 32 ) );
  }


  //
  // SR & co.
  //

  // SRA/SRL perform arithmetical/logical right shifts of the value in register
  // rs1 by the shift amount held in the least-significant 5 bits of register
  // rs2.
  if ( ( o.getOpCode() == RV32I::OpCode::SRA ) ||
       ( o.getOpCode() == RV32I::OpCode::SRL ) ) {
    auto shamt = lastOp().extract( 0, 5 );
    shamt.setSignedness( false );

    // Enforce arithmetical/logical shifting.
    auto &a = nthOp( 1 );
    a.setSignedness( o.getOpCode() == RV32I::OpCode::SRA );

    nthRes( 0 ) = a >> shamt;
  }

  // SRAI/SRLI perform arithmetical/logical right shifts by a shift amount given
  // as immediate.
  if ( ( o.getOpCode() == RV32I::OpCode::SRAI ) ||
       ( o.getOpCode() == RV32I::OpCode::SRLI ) ) {
    auto shamt = lastOp();
    shamt.setSignedness( false );

    // Enforce arithmetical/logical shifting.
    auto &a = nthOp( 1 );
    a.setSignedness( o.getOpCode() == RV32I::OpCode::SRAI );

    nthRes( 0 ) = a >> shamt;
  }


  //
  // SUB
  //

  // SUB performs a subtraction. Overflows are ignored and the least-significant
  // XLEN bits of results are written to the destination.
  if ( o.getOpCode() == RV32I::OpCode::SUB ) {

    if ( nthReg( 1 ) == lastReg() )
      // Special case: SUB Dc, Da, Da = 0
      nthRes( 0 ) = { WIR_L4::b0, 32, true };
    else
      nthRes( 0 ) = nthOp( 1 ) - nthOp( 2 );

    nthRes( 0 ).setSignedness();
  }


  //
  // XOR & co.
  //

  // XOR performs bitwise logical operations.
  if ( o.getOpCode() == RV32I::OpCode::XOR ) {
    nthRes( 0 ) = nthOp( 1 ) ^ lastOp();
    nthRes( 0 ).setSignedness( false );
  }

  // XORI performs bitwise XOR on register rs1 and the sign-extended 12-bit
  // immediate and places the result in rd.
  if ( o.getOpCode() == RV32I::OpCode::XORI ) {
    auto &x = lastOp();
    x.setSignedness();

    nthRes( 0 ) = nthOp( 1 ) ^ x.extend( 32 );
    nthRes( 0 ).setSignedness( false );
  }


  /*
    RV32IC
  */

  //
  // C.ADD & co.
  //

  // C.ADD adds the values in registers rd and rs2 and writes the result to
  // register rd.
  if ( o.getOpCode() == RV32IC::OpCode::CADD ) {
    nthRes( 0 ) = firstOp() + lastOp();
    nthRes( 0 ).setSignedness();
  }

  // C.ADDI adds the non-zero sign-extended 6-bit immediate to the value in
  // register rd then writes the result to rd.
  if ( o.getOpCode() == RV32IC::OpCode::CADDI ) {
    auto &x = lastOp();
    x.setSignedness();

    nthRes( 0 ) = firstOp() + x.extend( 32 );
    nthRes( 0 ).setSignedness();
  }


  //
  // C.AND & co.
  //

  // C.AND computes the bitwise AND of the values in registers rd0 and rd2, then
  // writes the result to register rd0.
  if ( o.getOpCode() == RV32IC::OpCode::CAND ) {
    nthRes( 0 ) = firstOp() & lastOp();
    nthRes( 0 ).setSignedness( false );
  }

  // C.ANDI computes the bitwise AND of of the value in register rd0 and the
  // sign-extended 6-bit immediate, then writes the result to rd0.
  if ( o.getOpCode() == RV32IC::OpCode::CANDI ) {
    auto &x = lastOp();
    x.setSignedness();

    nthRes( 0 ) = firstOp() & x.extend( 32 );
    nthRes( 0 ).setSignedness( false );
  }


  //
  // C.LI & C.LUI
  //

  // C.LI loads the sign-extended 6-bit immediate into register rd.
  if ( o.getOpCode() == RV32IC::OpCode::CLI ) {
    auto &x = lastOp();
    x.setSignedness();

    nthRes( 0 ) = x.extend( 32 );
  }

  // C.LUI loads the non-zero 6-bit immediate field into bits 17–12 of the
  // destination register, clears the bottom 12 bits, and sign-extends bit 17
  // into all higher bits of the destination.
  if ( o.getOpCode() == RV32IC::OpCode::CLUI ) {
    auto &x = lastOp();
    x.setSignedness();

    nthRes( 0 ) = x.extend( 32 ) << 12;
  }


  //
  // C.MV
  //

  // C.MV copies the value in register rs2 into register rd.
  if ( o.getOpCode() == RV32IC::OpCode::CMV )
    nthRes( 0 ) = lastOp();


  //
  // C.OR
  //

  // C.OR computes the bitwise OR of the values in registers rd0 and rd2, then
  // writes the result to register rd0.
  if ( o.getOpCode() == RV32IC::OpCode::COR ) {
    nthRes( 0 ) = firstOp() | lastOp();
    nthRes( 0 ).setSignedness( false );
  }


  //
  // C.SLLI
  //

  // C.SLLI performs logical left shift of the value in register rd then writes
  // the result to rd.
  if ( o.getOpCode() == RV32IC::OpCode::CSLLI ) {
    auto shamt = lastOp();
    shamt.setSignedness( false );

    // Enforce logical shifting.
    auto &a = firstOp();
    a.setSignedness( false );

    nthRes( 0 ) = a << shamt;
  }


  //
  // C.SR & co.
  //

  // C.SRAI/C.SRLI perform arithmetical/logical right shift, resp., of the value
  // in register rd then writes the result to rd.
  if ( ( o.getOpCode() == RV32IC::OpCode::CSRAI ) ||
       ( o.getOpCode() == RV32IC::OpCode::CSRLI ) ) {
    auto &shamt = lastOp();
    shamt.setSignedness( false );

    // Enforce logical shifting.
    auto &a = firstOp();
    a.setSignedness( o.getOpCode() == RV32IC::OpCode::CSRAI );

    nthRes( 0 ) = a >> shamt;
  }


  //
  // C.SUB
  //

  // C.SUB subtracts the value in register rs2 from the value in register rd0,
  // then writes the result to register rd0.
  if ( o.getOpCode() == RV32IC::OpCode::CSUB ) {
    nthRes( 0 ) = firstOp() - lastOp();
    nthRes( 0 ).setSignedness();
  }


  //
  // C.XOR
  //

  // C.XOR computes the bitwise XOR of the values in registers rd0 and rs2, then
  // writes the result to register rd0.
  if ( o.getOpCode() == RV32IC::OpCode::CXOR ) {
    nthRes( 0 ) = firstOp() ^ lastOp();
    nthRes( 0 ).setSignedness( false );
  }
};


/*
  simulateBottomUp performs the RISC-V-specific bottom-up simulation of the
  given WIR operation.

  For a documentation of the semantics of RISC-V operations, please refer to
  the RISC-V Instruction Set Manual Volume I: User-Level ISA.
*/
void RV32_BitDFA::simulateBottomUp( const WIR_Operation &o,
                                    std::map<WIR_id_t, WIR_UpDownValue> &in,
                                    std::map<WIR_id_t, WIR_UpDownValue> &out,
                                    std::map<WIR_id_t, WIR_UpDownValue> &results )
{
  DSTART(
    "virtual void RV32_BitDFA::simulateBottomUp(const WIR_Operation&, "
    "map<long long unsigned int, WIR_UpDownValue>&, "
    "map<long long unsigned int, WIR_UpDownValue>&, "
    "map<long long unsigned int, WIR_UpDownValue>&)" );

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


  /*
    RV32I
  */

  //
  // ADD & co.
  //

  // ADD performs an addition. Overflows are ignored and the least-significant
  // XLEN bits of results are written to the destination.
  // ADDI adds the sign-extended 12-bit immediate to register rs1. Arithmetic
  // overflow is ignored and the result is simply the least-significant XLEN
  // bits of the result.
  if ( ( o.getOpCode() == RV32I::OpCode::ADD ) ||
       ( o.getOpCode() == RV32I::OpCode::ADDI ) ) {
    auto pcOut = firstOut();
    auto &cOut = *(pcOut.second);

    auto paIn = nthIn( 1 );
    auto &aIn = *(paIn.second);

    auto pbIn = lastIn();
    auto bIn = pbIn.second->extend( 32 );

    for ( int i = cOut.getBitWidth() - 1;
          ( i >= 0 ) && ( cOut[ i ] == WIR_L4::bX ); --i ) {
      aIn.setBit( i, WIR_L4::bX );
      bIn.setBit( i, WIR_L4::bX );
    }

    results.insert( { paIn.first, aIn } );
    results.insert(
      { pbIn.first, bIn.extract( 0, pbIn.second->getBitWidth() ) } );
  }


  //
  // AND & co.
  //

  // AND performs bitwise logical operations.
  // ANDI performs bitwise AND on register rs1 and the sign-extended 12-bit
  // immediate and places the result in rd.
  if ( ( o.getOpCode() == RV32I::OpCode::AND ) ||
       ( o.getOpCode() == RV32I::OpCode::ANDI ) ) {
    auto pcOut = firstOut();
    auto &cOut = *(pcOut.second);

    auto paIn = nthIn( 1 );
    auto &aIn = *(paIn.second);

    auto pbIn = lastIn();
    auto bIn = pbIn.second->extend( 32 );

    for ( unsigned int i = 0; i < aIn.getBitWidth(); ++i ) {
      if ( bIn.at( i ) == WIR_L4::b0 )
        // If a bit in bIn is 0, the same bit in aIn is X.
        aIn.setBit( i, WIR_L4::bX );
      else

      if ( aIn.at( i ) == WIR_L4::b0 )
        // If a bit in aIn is 0, the same bit in bIn is X.
        bIn.setBit( i, WIR_L4::bX );

      if ( cOut.at( i ) == WIR_L4::bX ) {
        // If a bit in cOut is already X, then propagate it into both aIn and
        // bIn.
        aIn.setBit( i, WIR_L4::bX );
        bIn.setBit( i, WIR_L4::bX );
      }
    }

    results.insert( { paIn.first, aIn } );
    results.insert(
      { pbIn.first, bIn.extract( 0, pbIn.second->getBitWidth() ) } );
  }


  //
  // B* & co.
  //

  // BEQ takes a branch if registers rs1 and rs2 are equal.
  if ( o.getOpCode() == RV32I::OpCode::BEQ ) {
    auto paIn = firstIn();
    auto &aIn = *(paIn.second);

    auto pbIn = nthIn( 1 );
    auto &bIn = *(pbIn.second);

    auto cmp = ( aIn == bIn );

    if ( cmp == WIR_L4::b0 ) {
      // If aIn and bIn are inequal, we simply determine one bit position where
      // aIn and bIn provably differ and set all other bits to X.
      unsigned int diffPos = 0;

      for ( unsigned int i = 0; i < aIn.getBitWidth(); ++i )
        if ( ( getLevel( aIn[ i ] ) == 2 ) && ( getLevel( bIn[ i ] ) == 2 ) &&
             ( aIn[ i ] != bIn[ i ] ) ) {
          diffPos = i;
          break;
        } else

        if ( ( getLevel( aIn[ i ] ) == 1 ) && ( getLevel( bIn[ i ] ) == 1 ) &&
             ( aIn[ i ] != bIn[ i ] ) &&
             ( aIn.getLocation( i ) == bIn.getLocation( i ) ) ) {
          diffPos = i;
          break;
        }

      aIn.setAllBits( WIR_L4::bX );
      aIn.setBit( diffPos, WIR_L4::b0 );

      bIn.setAllBits( WIR_L4::bX );
      bIn.setBit( diffPos, WIR_L4::b0 );

      results.insert( { paIn.first, aIn } );
      results.insert( { pbIn.first, bIn } );
    }
  }

  // BGE and BGEU take a branch if rs1 is greater than or equal to rs2, using
  // signed or unsigned comparison, resp.
  // BLT and BLTU take a branch if rs1 is less than rs2, using signed or
  // unsigned comparison, resp.
  if ( ( o.getOpCode() == RV32I::OpCode::BGE ) ||
       ( o.getOpCode() == RV32I::OpCode::BGEU ) ||
       ( o.getOpCode() == RV32I::OpCode::BLT ) ||
       ( o.getOpCode() == RV32I::OpCode::BLTU ) ) {
    auto paIn = firstIn();
    auto &aIn = *(paIn.second);

    auto pbIn = nthIn( 1 );
    auto &bIn = *(pbIn.second);

    unsigned int diffPos = 0;
    bool diffFound = false;

    // Try to find a bit position where aIn and bIn provably differ, starting
    // with the most-significant position.
    for ( int i = aIn.getBitWidth() - 1; i >= 0; --i )
      if ( ( getLevel( aIn[ i ] ) == 2 ) && ( getLevel( bIn[ i ] ) == 2 ) &&
           ( aIn[ i ] != bIn[ i ] ) ) {
        diffPos = i;
        diffFound = true;
        break;
      } else

      if ( ( getLevel( aIn[ i ] ) == 2 ) && ( getLevel( bIn[ i ] ) == 2 ) &&
           ( aIn[ i ] == bIn[ i ] ) )
        continue;
      else

      if ( ( getLevel( aIn[ i ] ) == 1 ) && ( getLevel( bIn[ i ] ) == 1 ) &&
           ( aIn[ i ] != bIn[ i ] ) &&
           ( aIn.getLocation( i ) == bIn.getLocation( i ) ) ) {
        diffPos = i;
        diffFound = true;
        break;
      } else

      if ( ( getLevel( aIn[ i ] ) == 1 ) && ( getLevel( bIn[ i ] ) == 1 ) &&
           ( aIn[ i ] == bIn[ i ] ) &&
           ( aIn.getLocation( i ) == bIn.getLocation( i ) ) )
        continue;
      else
        break;

    if ( diffFound ) {
      // We found a provably differing bit position starting from the
      // most-significant bits of aIn and bIn. We thus can set all lower bits to
      // X.
      for ( int i = diffPos - 1; i >= 0; --i ) {
        aIn.setBit( i, WIR_L4::bX );
        bIn.setBit( i, WIR_L4::bX );
      }
    }

    results.insert( { paIn.first, aIn } );
    results.insert( { pbIn.first, bIn } );
  }

  // BNE takes a branch if registers rs1 and rs2 are unequal.
  if ( o.getOpCode() == RV32I::OpCode::BNE ) {
    auto paIn = firstIn();
    auto &aIn = *(paIn.second);

    auto pbIn = nthIn( 1 );
    auto &bIn = *(pbIn.second);

    auto cmp = ( aIn != bIn );

    if ( cmp == WIR_L4::b1 ) {
      // If aIn and bIn are inequal, we simply determine one bit position where
      // aIn and bIn provably differ and set all other bits to X.
      unsigned int diffPos = 0;

      for ( unsigned int i = 0; i < aIn.getBitWidth(); ++i )
        if ( ( getLevel( aIn[ i ] ) == 2 ) && ( getLevel( bIn[ i ] ) == 2 ) &&
             ( aIn[ i ] != bIn[ i ] ) ) {
          diffPos = i;
          break;
        } else

        if ( ( getLevel( aIn[ i ] ) == 1 ) && ( getLevel( bIn[ i ] ) == 1 ) &&
             ( aIn[ i ] != bIn[ i ] ) &&
             ( aIn.getLocation( i ) == bIn.getLocation( i ) ) ) {
          diffPos = i;
          break;
        }

      aIn.setAllBits( WIR_L4::bX );
      aIn.setBit( diffPos, WIR_L4::b0 );

      bIn.setAllBits( WIR_L4::bX );
      bIn.setBit( diffPos, WIR_L4::b0 );

      results.insert( { paIn.first, aIn } );
      results.insert( { pbIn.first, bIn } );
    }
  }


  //
  // LUI
  //

  // LUI (load upper immediate) is used to build 32-bit constants and uses the
  // U-type format. LUI places the immediate value in the most-significant 20
  // bits of the destination register rd, filling in the least-significant 12
  // bits with zeros.
  if ( o.getOpCode() == RV32I::OpCode::LUI ) {
    auto paOut = firstOut();
    auto &aOut = *(paOut.second);

    auto paIn = lastIn();
    auto &aIn = *(paIn.second);

    // Propagate X bits bottom-up.
    for ( unsigned int i = 12; i < aOut.getBitWidth(); ++i )
      if ( aOut[ i ] == WIR_L4::bX )
        aIn.setBit( i - 12, WIR_L4::bX );

    results.insert( { paIn.first, aIn } );
  }


  //
  // MOV (pseudo-operation)
  //

  // MOV copies the value in register rs2 into register rd.
  if ( o.getOpCode() == RV32I::OpCode::MOV ) {
    auto paOut = firstOut();
    auto &aOut = *(paOut.second);

    auto pbIn = lastIn();
    auto &bIn = *(pbIn.second);

    for ( unsigned int i = 0; i < aOut.getBitWidth(); ++i )
      if ( aOut.at( i ) == WIR_L4::bX )
        bIn.setBit( i, WIR_L4::bX );

    results.insert( { pbIn.first, bIn } );
  }


  //
  // OR & co.
  //

  // OR performs bitwise logical operations.
  // ORI performs bitwise OR on register rs1 and the sign-extended 12-bit
  // immediate and places the result in rd.
  if ( ( o.getOpCode() == RV32I::OpCode::OR ) ||
       ( o.getOpCode() == RV32I::OpCode::ORI ) ) {
    auto pcOut = firstOut();
    auto &cOut = *(pcOut.second);

    auto paIn = nthIn( 1 );
    auto &aIn = *(paIn.second);

    auto pbIn = lastIn();
    auto bIn = pbIn.second->extend( 32 );

    for ( unsigned int i = 0; i < aIn.getBitWidth(); ++i ) {
      if ( bIn.at( i ) == WIR_L4::b1 )
        // If a bit in bIn is 1, the same bit in aIn is X.
        aIn.setBit( i, WIR_L4::bX );
      else

      if ( aIn.at( i ) == WIR_L4::b1 )
        // If a bit in aIn is 1, the same bit in bIn is X.
        bIn.setBit( i, WIR_L4::bX );

      if ( cOut.at( i ) == WIR_L4::bX ) {
        // If a bit in cOut is already X, then propagate it into both aIn and
        // bIn.
        aIn.setBit( i, WIR_L4::bX );
        bIn.setBit( i, WIR_L4::bX );
      }
    }

    results.insert( { paIn.first, aIn } );
    results.insert(
      { pbIn.first, bIn.extract( 0, pbIn.second->getBitWidth() ) } );
  }


  //
  // SL & co.
  //

  // SLL performs logical left shift of the value in register rs1 by the shift
  // amount held in the least-significant 5 bits of register rs2.
  // SLLI performs logical left shift by a shift amount given as immediate.
  if ( ( o.getOpCode() == RV32I::OpCode::SLL ) ||
       ( o.getOpCode() == RV32I::OpCode::SLLI ) ) {
    auto paOut = firstOut();
    auto &aOut = *(paOut.second);

    auto paIn = nthIn( 1 );
    auto &aIn = *(paIn.second);

    auto pbIn = lastIn();
    auto &bIn = *(pbIn.second);
    auto shamt = bIn.extract( 0, 5 );

    // Bits 5 - 31 of the shift amount are always X.
    for ( unsigned int i = 5; i < bIn.getBitWidth(); ++i )
      bIn.setBit( i, WIR_L4::bX );

    // Iterate the shift amount.
    for ( unsigned int i = 0; i < 5; ++i ) {
      // If we encounter a 'U', 'L' or 'N' bit in the shift amount, we don't
      // know anything about this shift operation.
      if ( ( shamt[ i ] == WIR_L4::bU ) || ( shamt[ i ] == WIR_L4::bL ) ||
          ( shamt[ i ] == WIR_L4::bN ) )
        return;

      // Replace 'X' by '1'.
      if ( shamt[ i ] == WIR_L4::bX )
        shamt[ i ] = WIR_L4::b1;
    }

    // Finally, get the shift amount's value.
    signed long long shiftVal = shamt.getSignedValue();

    // Set most-significant bits of a to X.
    for ( unsigned int i = 1; i <= shiftVal; ++i )
      aIn.setBit( aIn.getBitWidth() - i, WIR_L4::bX );

    // Propagate X bits from aOut to a.
    for ( unsigned int i = 0; i < aOut.getBitWidth(); ++i )
      if ( aOut.at( i ) == WIR_L4::bX ) {
        int aPos = i - shiftVal;

        if ( ( aPos >= 0 ) && ( (unsigned int) aPos < aOut.getBitWidth() ) )
          aIn.setBit( aPos, WIR_L4::bX );
      }

    results.insert( { paIn.first, aIn } );
    results.insert( { pbIn.first, bIn } );
  }


  //
  // SLT & co.
  //

  // SLT/SLTU performs signed/unsigned comparisons writing 1 to rd if rs1 < rs2,
  // 0 otherwise.
  // SLTI/SLTIU write a 1 in register rd if register rs1 is less than the sign-
  // or zero- extended immediate, respectively , or 0 otherwise.
  if ( ( o.getOpCode() == RV32I::OpCode::SLT ) ||
       ( o.getOpCode() == RV32I::OpCode::SLTI ) ||
       ( o.getOpCode() == RV32I::OpCode::SLTU ) ||
       ( o.getOpCode() == RV32I::OpCode::SLTIU ) ) {
    auto paOut = firstOut();
    auto &aOut = *(paOut.second);

    auto paIn = nthIn( 1 );
    auto &aIn = *(paIn.second);

    auto pbIn = lastIn();
    auto &bIn = *(pbIn.second);

    if ( aOut.at( 0 ) ==  WIR_L4::bX ) {
      // If the least-significant bit of aOut that stores the comparison result
      // is irrelevant, both operands  aIn and bIn can be set to X.
      aIn.setAllBits( WIR_L4::bX );
      bIn.setAllBits( WIR_L4::bX );
    }

    results.insert( { paIn.first, aIn } );
    results.insert( { pbIn.first, bIn } );
  }


  //
  // SR & co.
  //

  // SRA/SRL perform arithmetical/logical right shifts of the value in register
  // rs1 by the shift amount held in the least-significant 5 bits of register
  // rs2.
  // SRAI/SRLI perform arithmetical/logical right shifts by a shift amount given
  // as immediate.
  if ( ( o.getOpCode() == RV32I::OpCode::SRA ) ||
       ( o.getOpCode() == RV32I::OpCode::SRAI ) ||
       ( o.getOpCode() == RV32I::OpCode::SRL ) ||
       ( o.getOpCode() == RV32I::OpCode::SRLI ) ) {
    auto paOut = firstOut();
    auto &aOut = *(paOut.second);

    auto paIn = nthIn( 1 );
    auto &aIn = *(paIn.second);

    auto pbIn = lastIn();
    auto &bIn = *(pbIn.second);
    auto shamt = bIn.extract( 0, 5 );

    // Bits 5 - 31 of the shift amount are always X.
    for ( unsigned int i = 5; i < bIn.getBitWidth(); ++i )
      bIn.setBit( i, WIR_L4::bX );

    // Iterate the shift amount.
    for ( unsigned int i = 0; i < 5; ++i ) {
      // If we encounter a 'U', 'L' or 'N' bit in the shift amount, we don't
      // know anything about this shift operation.
      if ( ( shamt[ i ] == WIR_L4::bU ) || ( shamt[ i ] == WIR_L4::bL ) ||
           ( shamt[ i ] == WIR_L4::bN ) )
        return;

      // Replace 'X' by '1'.
      if ( shamt[ i ] == WIR_L4::bX )
        shamt[ i ] = WIR_L4::b1;
    }

    // Finally, get the shift amount's value.
    signed long long shiftVal = shamt.getSignedValue();

    // Set least-significant bits of a to X.
    for ( unsigned int i = 0; i < shiftVal; ++i )
      aIn.setBit( i, WIR_L4::bX );

    // Propagate X bits from aOut to a.
    for ( unsigned int i = 0; i < aOut.getBitWidth(); ++i )
      if ( aOut.at( i ) == WIR_L4::bX ) {
        int aPos = i + shiftVal;

        if ( ( aPos >= 0 ) && ( (unsigned int) aPos < aOut.getBitWidth() ) )
          aIn.setBit( aPos, WIR_L4::bX );
      }

    results.insert( { paIn.first, aIn } );
    results.insert( { pbIn.first, bIn } );
  }


  //
  // SUB
  //

  // SUB performs a subtraction. Overflows are ignored and the least-significant
  // XLEN bits of results are written to the destination.
  if ( o.getOpCode() == RV32I::OpCode::SUB ) {
    auto paOut = firstOut();
    auto &aOut = *(paOut.second);

    auto paIn = nthIn( 1 );
    auto &aIn = *(paIn.second);

    auto pbIn = lastIn();
    auto &bIn = *(pbIn.second);

    for ( int i = aOut.getBitWidth() - 1;
          ( i >= 0 ) && ( aOut[ i ] == WIR_L4::bX ); --i ) {
      aIn.setBit( i, WIR_L4::bX );
      bIn.setBit( i, WIR_L4::bX );
    }

    results.insert( { paIn.first, aIn } );
    results.insert( { pbIn.first, bIn } );
  }


  //
  // SB / SH
  //

  // The SH and SB instructions store 16-bit and 8-bit values from the low bits
  // of register rs2 to memory.
  if ( ( o.getOpCode() == RV32I::OpCode::SB ) ||
       ( o.getOpCode() == RV32I::OpCode::SH ) ) {
    auto pbIn = lastIn();
    auto &bIn = *(pbIn.second);

    // The most-significant 24 or 16 bits of a are X, resp.
    for ( unsigned int i = ( o.getOpCode() == RV32I::OpCode::SB ? 8 : 16 );
          i < 32; ++i )
      bIn.setBit( i, WIR_L4::bX );

    results.insert( { pbIn.first, bIn } );
  }


  //
  // XOR & co.
  //

  // XOR performs bitwise logical operations.
  // XORI performs bitwise XOR on register rs1 and the sign-extended 12-bit
  // immediate and places the result in rd.
  if ( ( o.getOpCode() == RV32I::OpCode::XOR ) ||
       ( o.getOpCode() == RV32I::OpCode::XORI ) ) {
    auto pcOut = firstOut();
    auto &cOut = *(pcOut.second);

    auto paIn = nthIn( 1 );
    auto &aIn = *(paIn.second);

    auto pbIn = lastIn();
    auto bIn = pbIn.second->extend( 32 );

    for ( unsigned int i = 0; i < aIn.getBitWidth(); ++i )
      if ( cOut.at( i ) == WIR_L4::bX ) {
        // If a bit in cOut is already X, then propagate it into both aIn and
        // bIn.
        aIn.setBit( i, WIR_L4::bX );
        bIn.setBit( i, WIR_L4::bX );
      }

    results.insert( { paIn.first, aIn } );
    results.insert(
      { pbIn.first, bIn.extract( 0, pbIn.second->getBitWidth() ) } );
  }


  /*
    RV32IC
  */

  //
  // C.ADD & co.
  //

  // C.ADD adds the values in registers rd and rs2 and writes the result to
  // register rd.
  // C.ADDI adds the non-zero sign-extended 6-bit immediate to the value in
  // register rd then writes the result to rd.
  if ( ( o.getOpCode() == RV32IC::OpCode::CADD ) ||
       ( o.getOpCode() == RV32IC::OpCode::CADDI ) ) {
    auto paOut = firstOut();
    auto &aOut = *(paOut.second);

    auto paIn = firstIn();
    auto &aIn = *(paIn.second);

    auto pbIn = lastIn();
    auto bIn = pbIn.second->extend( 32 );

    for ( int i = aOut.getBitWidth() - 1;
          ( i >= 0 ) && ( aOut[ i ] == WIR_L4::bX ); --i ) {
      aIn.setBit( i, WIR_L4::bX );
      bIn.setBit( i, WIR_L4::bX );
    }

    results.insert( { paIn.first, aIn } );
    results.insert(
      { pbIn.first, bIn.extract( 0, pbIn.second->getBitWidth() ) } );
  }


  //
  // C.AND & co.
  //

  // C.AND performs bitwise logical operations.
  // C.ANDI performs bitwise AND on register rs1 and the sign-extended 12-bit
  // immediate and places the result in rd.
  if ( ( o.getOpCode() == RV32IC::OpCode::CAND ) ||
       ( o.getOpCode() == RV32IC::OpCode::CANDI ) ) {
    auto paOut = firstOut();
    auto &aOut = *(paOut.second);

    auto paIn = firstIn();
    auto &aIn = *(paIn.second);

    auto pbIn = lastIn();
    auto bIn = pbIn.second->extend( 32 );

    for ( unsigned int i = 0; i < aIn.getBitWidth(); ++i ) {
      if ( bIn.at( i ) == WIR_L4::b0 )
        // If a bit in bIn is 0, the same bit in aIn is X.
        aIn.setBit( i, WIR_L4::bX );
      else

      if ( aIn.at( i ) == WIR_L4::b0 )
        // If a bit in aIn is 0, the same bit in bIn is X.
        bIn.setBit( i, WIR_L4::bX );

      if ( aOut.at( i ) == WIR_L4::bX ) {
        // If a bit in cOut is already X, then propagate it into both aIn and
        // bIn.
        aIn.setBit( i, WIR_L4::bX );
        bIn.setBit( i, WIR_L4::bX );
      }
    }

    results.insert( { paIn.first, aIn } );
    results.insert(
      { pbIn.first, bIn.extract( 0, pbIn.second->getBitWidth() ) } );
  }


  //
  // C.B* & co.
  //

  // C.BEQZ/C.BNEZ take a branch if register rs1 is zero or non-zero, resp.
  if ( ( o.getOpCode() == RV32IC::OpCode::CBEQZ ) ||
       ( o.getOpCode() == RV32IC::OpCode::CBNEZ ) ) {
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


  //
  // C.LI & C.LUI
  //

  // C.LI loads the sign-extended 6-bit immediate into register rd.
  if ( o.getOpCode() == RV32IC::OpCode::CLI ) {
    auto paOut = firstOut();
    auto &aOut = *(paOut.second);

    auto paIn = lastIn();
    auto &aIn = *(paIn.second);

    // Propagate X bits bottom-up.
    for ( unsigned int i = 0; i < aIn.getBitWidth(); ++i )
      if ( aOut[ i ] == WIR_L4::bX )
        aIn.setBit( i, WIR_L4::bX );

    results.insert( { paIn.first, aIn } );
  }

  // C.LUI loads the non-zero 6-bit immediate field into bits 17–12 of the
  // destination register, clears the bottom 12 bits, and sign-extends bit 17
  // into all higher bits of the destination.
  if ( o.getOpCode() == RV32IC::OpCode::CLUI ) {
    auto paOut = firstOut();
    auto &aOut = *(paOut.second);

    auto paIn = lastIn();
    auto &aIn = *(paIn.second);

    // Propagate X bits bottom-up.
    for ( unsigned int i = 0; i < aIn.getBitWidth(); ++i )
      if ( aOut[ i + 12 ] == WIR_L4::bX )
        aIn.setBit( i, WIR_L4::bX );

    results.insert( { paIn.first, aIn } );
  }


  //
  // C.MV
  //

  // C.MV copies the value in register rs2 into register rd.
  if ( o.getOpCode() == RV32IC::OpCode::CMV ) {
    auto paOut = firstOut();
    auto &aOut = *(paOut.second);

    auto pbIn = lastIn();
    auto &bIn = *(pbIn.second);

    for ( unsigned int i = 0; i < aOut.getBitWidth(); ++i )
      if ( aOut.at( i ) == WIR_L4::bX )
        bIn.setBit( i, WIR_L4::bX );

    results.insert( { pbIn.first, bIn } );
  }


  //
  // C.OR
  //

  // C.OR computes the bitwise OR of the values in registers rd0 and rd2, then
  // writes the result to register rd0.
  if ( o.getOpCode() == RV32IC::OpCode::COR ) {
    auto paOut = firstOut();
    auto &aOut = *(paOut.second);

    auto paIn = firstIn();
    auto &aIn = *(paIn.second);

    auto pbIn = lastIn();
    auto &bIn = *(pbIn.second);

    for ( unsigned int i = 0; i < aIn.getBitWidth(); ++i ) {
      if ( bIn.at( i ) == WIR_L4::b1 )
        // If a bit in bIn is 1, the same bit in aIn is X.
        aIn.setBit( i, WIR_L4::bX );
      else

      if ( aIn.at( i ) == WIR_L4::b1 )
        // If a bit in aIn is 1, the same bit in bIn is X.
        bIn.setBit( i, WIR_L4::bX );

      if ( aOut.at( i ) == WIR_L4::bX ) {
        // If a bit in aOut is already X, then propagate it into both aIn and
        // bIn.
        aIn.setBit( i, WIR_L4::bX );
        bIn.setBit( i, WIR_L4::bX );
      }
    }

    results.insert( { paIn.first, aIn } );
    results.insert( { pbIn.first, bIn } );
  }


  //
  // C.SLLI
  //

  // C.SLLI performs logical left shift of the value in register rd then writes
  // the result to rd.
  if ( o.getOpCode() == RV32IC::OpCode::CSLLI ) {
    auto paOut = firstOut();
    auto &aOut = *(paOut.second);

    auto paIn = firstIn();
    auto &aIn = *(paIn.second);

    auto pbIn = lastIn();
    auto &bIn = *(pbIn.second);
    auto shamt = bIn.extract( 0, 5 );

    // Iterate the shift amount.
    for ( unsigned int i = 0; i < 5; ++i ) {
      // If we encounter a 'U', 'L' or 'N' bit in the shift amount, we don't
      // know anything about this shift operation.
      if ( ( shamt[ i ] == WIR_L4::bU ) || ( shamt[ i ] == WIR_L4::bL ) ||
           ( shamt[ i ] == WIR_L4::bN ) )
        return;

      // Replace 'X' by '1'.
      if ( shamt[ i ] == WIR_L4::bX )
        shamt[ i ] = WIR_L4::b1;
    }

    // Finally, get the shift amount's value.
    signed long long shiftVal = shamt.getSignedValue();

    // Set most-significant bits of a to X.
    for ( unsigned int i = 1; i <= shiftVal; ++i )
      aIn.setBit( aIn.getBitWidth() - i, WIR_L4::bX );

    // Propagate X bits from aOut to a.
    for ( unsigned int i = 0; i < aOut.getBitWidth(); ++i )
      if ( aOut.at( i ) == WIR_L4::bX ) {
        int aPos = i - shiftVal;

        if ( ( aPos >= 0 ) && ( (unsigned int) aPos < aOut.getBitWidth() ) )
          aIn.setBit( aPos, WIR_L4::bX );
      }

    results.insert( { paIn.first, aIn } );
  }


  //
  // C.SR & co.
  //

  // C.SRAI/C.SRLI perform arithmetical/logical right shift, resp., of the value
  // in register rd then writes the result to rd.
  if ( ( o.getOpCode() == RV32IC::OpCode::CSRAI ) ||
       ( o.getOpCode() == RV32IC::OpCode::CSRLI ) ) {
    auto paOut = firstOut();
    auto &aOut = *(paOut.second);

    auto paIn = firstIn();
    auto &aIn = *(paIn.second);

    auto pbIn = lastIn();
    auto &bIn = *(pbIn.second);
    auto shamt = bIn.extract( 0, 5 );

    // Iterate the shift amount.
    for ( unsigned int i = 0; i < 5; ++i ) {
      // If we encounter a 'U', 'L' or 'N' bit in the shift amount, we don't
      // know anything about this shift operation.
      if ( ( shamt[ i ] == WIR_L4::bU ) || ( shamt[ i ] == WIR_L4::bL ) ||
           ( shamt[ i ] == WIR_L4::bN ) )
        return;

      // Replace 'X' by '1'.
      if ( shamt[ i ] == WIR_L4::bX )
        shamt[ i ] = WIR_L4::b1;
    }

    // Finally, get the shift amount's value.
    signed long long shiftVal = shamt.getSignedValue();

    // Set least-significant bits of a to X.
    for ( unsigned int i = 0; i < shiftVal; ++i )
      aIn.setBit( i, WIR_L4::bX );

    // Propagate X bits from aOut to a.
    for ( unsigned int i = 0; i < aOut.getBitWidth(); ++i )
      if ( aOut.at( i ) == WIR_L4::bX ) {
        int aPos = i + shiftVal;

        if ( ( aPos >= 0 ) && ( (unsigned int) aPos < aOut.getBitWidth() ) )
          aIn.setBit( aPos, WIR_L4::bX );
      }

    results.insert( { paIn.first, aIn } );
  }


  //
  // C.SUB
  //

  // C.SUB subtracts the value in register rs2 from the value in register rd0,
  // then writes the result to register rd0.
  if ( o.getOpCode() == RV32IC::OpCode::CSUB ) {
    auto paOut = firstOut();
    auto &aOut = *(paOut.second);

    auto paIn = firstIn();
    auto &aIn = *(paIn.second);

    auto pbIn = lastIn();
    auto &bIn = *(pbIn.second);

    for ( int i = aOut.getBitWidth() - 1;
          ( i >= 0 ) && ( aOut[ i ] == WIR_L4::bX ); --i ) {
      aIn.setBit( i, WIR_L4::bX );
      bIn.setBit( i, WIR_L4::bX );
    }

    results.insert( { paIn.first, aIn } );
    results.insert( { pbIn.first, bIn } );
  }


  //
  // C.XOR
  //

  // C.XOR computes the bitwise XOR of the values in registers rd0 and rs2, then
  // writes the result to register rd0.
  if ( o.getOpCode() == RV32IC::OpCode::CXOR ) {
    auto paOut = firstOut();
    auto &aOut = *(paOut.second);

    auto paIn = firstIn();
    auto &aIn = *(paIn.second);

    auto pbIn = lastIn();
    auto bIn = *(pbIn.second);

    for ( unsigned int i = 0; i < aIn.getBitWidth(); ++i )
      if ( aOut.at( i ) == WIR_L4::bX ) {
        // If a bit in aOut is already X, then propagate it into both aIn and
        // bIn.
        aIn.setBit( i, WIR_L4::bX );
        bIn.setBit( i, WIR_L4::bX );
      }

    results.insert( { paIn.first, aIn } );
    results.insert( { pbIn.first, bIn } );
  }

};


/*
  postProcessEdge can be used to set properties of newly created DFG edges to
  particular, processor-dependent values.

  postProcessEdge checks whether the edge refers to register x0 which always
  contains the value 0. Reading this register should result in up/down values
  only containing 0 bits, which is realized here.
*/
void RV32_BitDFA::postProcessEdge( WIR_DFGEdgeProperty &ep )
{
  DSTART( "virtual void RV32_BitDFA::postProcessEdge(WIR_DFGEdgeProperty&)" );

  // Check whether the current edge denotes an input from a function-external
  // register which is x0.
  if ( ( ep.getType() == WIR_DFGNodeType::reg ) &&
       RV32I::isX0( ep.getSourceRegisterParameter().getRegister() ) ) {
    ep.getDownValue().setAllBits( WIR_L4::b0 );
    ep.getUpValue().setAllBits( WIR_L4::b0 );
  }
};


/*
  addEdge returns whether an edge from a defined register parameter to a used
  register parameter shall be added to the DFG.

  addEdge checks whether the defined register is x0 which always contains the
  value 0, irrespective of what is written into it. For this register, a proper
  def-use relationship in the DFG does not make sense so that no edge will be
  added.
*/
bool RV32_BitDFA::addEdge( const WIR_RegisterParameter &def,
                           const WIR_RegisterParameter &use )
{
  DSTART(
    "virtual bool RV32_BitDFA::addEdge(const WIR_RegisterParameter&, const "
    "WIR_RegisterParameter&)" );

  (void) use;

  return( !RV32I::isX0( def.getRegister() ) );
};

}       // namespace WIR
