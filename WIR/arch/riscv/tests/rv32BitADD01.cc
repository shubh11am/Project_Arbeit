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


//
// Include section
//

// Include standard headers
#include <iterator>

// Include libuseful headers
#include <libuseful/io.h>

// Include WIR headers
#include <wir/wir.h>
#include <arch/riscv/rv32imc.h>
#include <arch/riscv/analyses/bit/rv32bitdfa.h>


using namespace std;
using namespace WIR;


int main( void )
{
  WIR_Init();

  // This test case tests the bit-true top-down data flow analysis for RISC-V
  // operation ADD.

  RV32I p;
  WIR_CompilationUnit c;
  c.setName( __FILE__ );
  WIR_Function &f = c.pushBackFunction( WIR_Function { "main" } );
  WIR_BasicBlock &b1 = f.pushBackBasicBlock( {} );
  auto &input1 = f.pushBackVirtualRegister( RV_RegV() );
  auto &input2 = f.pushBackVirtualRegister( RV_RegV() );
  auto &input3 = f.pushBackVirtualRegister( RV_RegV() );
  auto &input4 = f.pushBackVirtualRegister( RV_RegV() );
  auto &input5 = f.pushBackVirtualRegister( RV_RegV() );
  auto &tmp = f.pushBackVirtualRegister( RV_RegV() );
  auto &tmp2 = f.pushBackVirtualRegister( RV_RegV() );
  auto &tmp3 = f.pushBackVirtualRegister( RV_RegV() );

  // This lambda serves for generating and inserting a RISC-V operation.
  auto rv32op = [&]( WIR_Operation &&o ) -> WIR_Operation & {
    auto &i = b1.pushBackInstruction( WIR_Instruction { WIR_Operation { o } } );
    return( i.begin()->get() );
  };

  // This lambda serves to retrieve an operation's outgoing down value.
  auto dval = []( const WIR_Operation &o,
                  unsigned int pos ) -> const WIR_UpDownValue & {
    auto it = o.begin();
    std::advance( it, pos );
    auto &c = it->get().getContainers<WIR_BitValues>().begin()->get();
    return( c.getOutValues().begin()->downVal );
  };

  // This lambda serves to retrieve an operation's incoming up value.
  auto uval = []( const WIR_Operation &o,
                  unsigned int pos ) -> const WIR_UpDownValue & {
    auto it = o.begin();
    std::advance( it, pos );
    auto &c = it->get().getContainers<WIR_BitValues>().begin()->get();
    return( c.getInValues().begin()->upVal );
  };

  // Create WIR code.

  rv32op(
    { RV32I::OpCode::ADDI, RV32I::OperationFormat::RRC12_1,
      new WIR_RegisterParameter( input1, WIR_Usage::def ),
      new WIR_RegisterParameter( p.x0(), WIR_Usage::use ),
      new RV_Const12_Signed( 56 ) } );

  rv32op(
    { RV32I::OpCode::SLLI, RV32I::OperationFormat::RRC5_1,
      new WIR_RegisterParameter( input1, WIR_Usage::def ),
      new WIR_RegisterParameter( input1, WIR_Usage::use ),
      new RV_Const5_Unsigned( 8 ) } );

  rv32op(
    { RV32I::OpCode::ADDI, RV32I::OperationFormat::RRC12_1,
      new WIR_RegisterParameter( input2, WIR_Usage::def ),
      new WIR_RegisterParameter( p.x0(), WIR_Usage::use ),
      new RV_Const12_Signed( 123 ) } );

  rv32op(
    { RV32I::OpCode::OR, RV32I::OperationFormat::RRR_1,
      new WIR_RegisterParameter( input1, WIR_Usage::def ),
      new WIR_RegisterParameter( input1, WIR_Usage::use ),
      new WIR_RegisterParameter( input2, WIR_Usage::use ) } );

  rv32op(
    { RV32I::OpCode::SLLI, RV32I::OperationFormat::RRC5_1,
      new WIR_RegisterParameter( input1, WIR_Usage::def ),
      new WIR_RegisterParameter( input1, WIR_Usage::use ),
      new RV_Const5_Unsigned( 8 ) } );

  rv32op(
    { RV32I::OpCode::ADDI, RV32I::OperationFormat::RRC12_1,
      new WIR_RegisterParameter( input3, WIR_Usage::def ),
      new WIR_RegisterParameter( p.x0(), WIR_Usage::use ),
      new RV_Const12_Signed( 64 ) } );

  rv32op(
    { RV32I::OpCode::OR, RV32I::OperationFormat::RRR_1,
      new WIR_RegisterParameter( input1, WIR_Usage::def ),
      new WIR_RegisterParameter( input1, WIR_Usage::use ),
      new WIR_RegisterParameter( input3, WIR_Usage::use ) } );

  rv32op(
    { RV32I::OpCode::SLLI, RV32I::OperationFormat::RRC5_1,
      new WIR_RegisterParameter( input1, WIR_Usage::def ),
      new WIR_RegisterParameter( input1, WIR_Usage::use ),
      new RV_Const5_Unsigned( 8 ) } );

  rv32op(
    { RV32I::OpCode::ANDI, RV32I::OperationFormat::RRC12_1,
      new WIR_RegisterParameter( tmp, WIR_Usage::def ),
      new WIR_RegisterParameter( input4, WIR_Usage::use ),
      new RV_Const12_Signed( 255 ) } );

  rv32op(
    { RV32I::OpCode::OR, RV32I::OperationFormat::RRR_1,
      new WIR_RegisterParameter( tmp2, WIR_Usage::def ),
      new WIR_RegisterParameter( input1, WIR_Usage::use ),
      new WIR_RegisterParameter( tmp, WIR_Usage::use ) } );

  rv32op(
    { RV32I::OpCode::ADDI, RV32I::OperationFormat::RRC12_1,
      new WIR_RegisterParameter( input5, WIR_Usage::def ),
      new WIR_RegisterParameter( p.x0(), WIR_Usage::use ),
      new RV_Const12_Signed( 1 ) } );

  // ADD
  auto &add1 = rv32op(
    { RV32I::OpCode::ADD, RV32I::OperationFormat::RRR_1,
      new WIR_RegisterParameter( tmp3, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp2, WIR_Usage::use ),
      new WIR_RegisterParameter( input5, WIR_Usage::use ) } );

  auto &add2 = rv32op(
    { RV32I::OpCode::ADD, RV32I::OperationFormat::RRR_1,
      new WIR_RegisterParameter( input1, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp3, WIR_Usage::use ),
      new WIR_RegisterParameter( input5, WIR_Usage::use ) } );

  rv32op(
    { RV32I::OpCode::SLLI, RV32I::OperationFormat::RRC5_1,
      new WIR_RegisterParameter( input1, WIR_Usage::def ),
      new WIR_RegisterParameter( input1, WIR_Usage::use ),
      new RV_Const5_Unsigned( 8 ) } );

  // Do bit-true data flow analysis.
  RV32_BitDFA a { f };
  a.analyze();

  // Evaluate analysis results.
  ufAssert( uval( add1, 1 ).extract( 0, 8 ).containsOnlyBit( WIR_L4::bL ) );
  ufAssert(
    uval( add1, 1 ).getLocation( 0 ).getRegisterParameter().getRegister() ==
      input4 );
  ufAssert( uval( add1, 1 ).getLocation( 0 ).getBitPosition() == 0 );
  ufAssert(
    uval( add1, 1 ).getLocation( 1 ).getRegisterParameter().getRegister() ==
      input4 );
  ufAssert( uval( add1, 1 ).getLocation( 1 ).getBitPosition() == 1 );
  ufAssert(
    uval( add1, 1 ).getLocation( 2 ).getRegisterParameter().getRegister() ==
      input4 );
  ufAssert( uval( add1, 1 ).getLocation( 2 ).getBitPosition() == 2 );
  ufAssert(
    uval( add1, 1 ).getLocation( 3 ).getRegisterParameter().getRegister() ==
      input4 );
  ufAssert( uval( add1, 1 ).getLocation( 3 ).getBitPosition() == 3 );
  ufAssert(
    uval( add1, 1 ).getLocation( 4 ).getRegisterParameter().getRegister() ==
      input4 );
  ufAssert( uval( add1, 1 ).getLocation( 4 ).getBitPosition() == 4 );
  ufAssert(
    uval( add1, 1 ).getLocation( 5 ).getRegisterParameter().getRegister() ==
      input4 );
  ufAssert( uval( add1, 1 ).getLocation( 5 ).getBitPosition() == 5 );
  ufAssert(
    uval( add1, 1 ).getLocation( 6 ).getRegisterParameter().getRegister() ==
      input4 );
  ufAssert( uval( add1, 1 ).getLocation( 6 ).getBitPosition() == 6 );
  ufAssert(
    uval( add1, 1 ).getLocation( 7 ).getRegisterParameter().getRegister() ==
      input4 );
  ufAssert( uval( add1, 1 ).getLocation( 7 ).getBitPosition() == 7 );
  ufAssert( uval( add1, 1 ).extract( 8, 8 ).getSignedValue() == 64 );
  ufAssert( uval( add1, 1 ).extract( 16, 8 ).getSignedValue() == 123 );
  ufAssert( uval( add1, 1 ).extract( 24, 8 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( add1, 0 ).extract( 0, 1 ).containsOnlyBit( WIR_L4::bN ) );
  ufAssert(
    dval( add1, 0 ).getLocation( 0 ).getRegisterParameter().getRegister() ==
      input4 );
  ufAssert( dval( add1, 0 ).getLocation( 0 ).getBitPosition() == 0 );
  ufAssert( dval( add1, 0 ).extract( 1, 8 ).containsOnlyBit( WIR_L4::bL ) );
  ufAssert(
    dval( add1, 0 ).getLocation( 1 ).getRegisterParameter().getRegister() ==
      tmp3 );
  ufAssert( dval( add1, 0 ).getLocation( 1 ).getBitPosition() == 1 );
  ufAssert(
    dval( add1, 0 ).getLocation( 2 ).getRegisterParameter().getRegister() ==
      tmp3 );
  ufAssert( dval( add1, 0 ).getLocation( 2 ).getBitPosition() == 2 );
  ufAssert(
    dval( add1, 0 ).getLocation( 3 ).getRegisterParameter().getRegister() ==
      tmp3 );
  ufAssert( dval( add1, 0 ).getLocation( 3 ).getBitPosition() == 3 );
  ufAssert(
    dval( add1, 0 ).getLocation( 4 ).getRegisterParameter().getRegister() ==
      tmp3 );
  ufAssert( dval( add1, 0 ).getLocation( 4 ).getBitPosition() == 4 );
  ufAssert(
    dval( add1, 0 ).getLocation( 5 ).getRegisterParameter().getRegister() ==
      tmp3 );
  ufAssert( dval( add1, 0 ).getLocation( 5 ).getBitPosition() == 5 );
  ufAssert(
    dval( add1, 0 ).getLocation( 6 ).getRegisterParameter().getRegister() ==
      tmp3 );
  ufAssert( dval( add1, 0 ).getLocation( 6 ).getBitPosition() == 6 );
  ufAssert(
    dval( add1, 0 ).getLocation( 7 ).getRegisterParameter().getRegister() ==
      tmp3 );
  ufAssert( dval( add1, 0 ).getLocation( 7 ).getBitPosition() == 7 );
  ufAssert(
    dval( add1, 0 ).getLocation( 8 ).getRegisterParameter().getRegister() ==
      tmp3 );
  ufAssert( dval( add1, 0 ).getLocation( 8 ).getBitPosition() == 8 );
  ufAssert( dval( add1, 0 ).extract( 9, 7 ).getSignedValue() == 32 );
  ufAssert( dval( add1, 0 ).extract( 16, 8 ).getSignedValue() == 123 );
  ufAssert( dval( add1, 0 ).extract( 24, 8 ).getSignedValue() == 56 );

  ufAssert( uval( add2, 1 ).extract( 0, 1 ).containsOnlyBit( WIR_L4::bN ) );
  ufAssert(
    uval( add2, 1 ).getLocation( 0 ).getRegisterParameter().getRegister() ==
      input4 );
  ufAssert( uval( add2, 1 ).getLocation( 0 ).getBitPosition() == 0 );
  ufAssert( uval( add2, 1 ).extract( 1, 8 ).containsOnlyBit( WIR_L4::bL ) );
  ufAssert(
    uval( add2, 1 ).getLocation( 1 ).getRegisterParameter().getRegister() ==
      tmp3 );
  ufAssert( uval( add2, 1 ).getLocation( 1 ).getBitPosition() == 1 );
  ufAssert(
    uval( add2, 1 ).getLocation( 2 ).getRegisterParameter().getRegister() ==
      tmp3 );
  ufAssert( uval( add2, 1 ).getLocation( 2 ).getBitPosition() == 2 );
  ufAssert(
    uval( add2, 1 ).getLocation( 3 ).getRegisterParameter().getRegister() ==
      tmp3 );
  ufAssert( uval( add2, 1 ).getLocation( 3 ).getBitPosition() == 3 );
  ufAssert(
    uval( add2, 1 ).getLocation( 4 ).getRegisterParameter().getRegister() ==
      tmp3 );
  ufAssert( uval( add2, 1 ).getLocation( 4 ).getBitPosition() == 4 );
  ufAssert(
    uval( add2, 1 ).getLocation( 5 ).getRegisterParameter().getRegister() ==
      tmp3 );
  ufAssert( uval( add2, 1 ).getLocation( 5 ).getBitPosition() == 5 );
  ufAssert(
    uval( add2, 1 ).getLocation( 6 ).getRegisterParameter().getRegister() ==
      tmp3 );
  ufAssert( uval( add2, 1 ).getLocation( 6 ).getBitPosition() == 6 );
  ufAssert(
    uval( add2, 1 ).getLocation( 7 ).getRegisterParameter().getRegister() ==
      tmp3 );
  ufAssert( uval( add2, 1 ).getLocation( 7 ).getBitPosition() == 7 );
  ufAssert(
    uval( add2, 1 ).getLocation( 8 ).getRegisterParameter().getRegister() ==
      tmp3 );
  ufAssert( uval( add2, 1 ).getLocation( 8 ).getBitPosition() == 8 );
  ufAssert( uval( add2, 1 ).extract( 9, 7 ).getSignedValue() == 32 );
  ufAssert( uval( add2, 1 ).extract( 16, 8 ).getSignedValue() == 123 );
  ufAssert( uval( add2, 1 ).extract( 24, 8 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( add2, 0 ).extract( 0, 1 ).containsOnlyBit( WIR_L4::bL ) );
  ufAssert(
    dval( add2, 0 ).getLocation( 0 ).getRegisterParameter().getRegister() ==
      input4 );
  ufAssert( dval( add2, 0 ).getLocation( 0 ).getBitPosition() == 0 );
  ufAssert( dval( add2, 0 ).extract( 1, 9 ).containsOnlyBit( WIR_L4::bL ) );
  ufAssert(
    dval( add2, 0 ).getLocation( 1 ).getRegisterParameter().getRegister() ==
      input1 );
  ufAssert( dval( add2, 0 ).getLocation( 1 ).getBitPosition() == 1 );
  ufAssert(
    dval( add2, 0 ).getLocation( 2 ).getRegisterParameter().getRegister() ==
      input1 );
  ufAssert( dval( add2, 0 ).getLocation( 2 ).getBitPosition() == 2 );
  ufAssert(
    dval( add2, 0 ).getLocation( 3 ).getRegisterParameter().getRegister() ==
      input1 );
  ufAssert( dval( add2, 0 ).getLocation( 3 ).getBitPosition() == 3 );
  ufAssert(
    dval( add2, 0 ).getLocation( 4 ).getRegisterParameter().getRegister() ==
      input1 );
  ufAssert( dval( add2, 0 ).getLocation( 4 ).getBitPosition() == 4 );
  ufAssert(
    dval( add2, 0 ).getLocation( 5 ).getRegisterParameter().getRegister() ==
      input1 );
  ufAssert( dval( add2, 0 ).getLocation( 5 ).getBitPosition() == 5 );
  ufAssert(
    dval( add2, 0 ).getLocation( 6 ).getRegisterParameter().getRegister() ==
      input1 );
  ufAssert( dval( add2, 0 ).getLocation( 6 ).getBitPosition() == 6 );
  ufAssert(
    dval( add2, 0 ).getLocation( 7 ).getRegisterParameter().getRegister() ==
      input1 );
  ufAssert( dval( add2, 0 ).getLocation( 7 ).getBitPosition() == 7 );
  ufAssert(
    dval( add2, 0 ).getLocation( 8 ).getRegisterParameter().getRegister() ==
      input1 );
  ufAssert( dval( add2, 0 ).getLocation( 8 ).getBitPosition() == 8 );
  ufAssert(
    dval( add2, 0 ).getLocation( 9 ).getRegisterParameter().getRegister() ==
      input1 );
  ufAssert( dval( add2, 0 ).getLocation( 9 ).getBitPosition() == 9 );
  ufAssert( dval( add2, 0 ).extract( 10, 6 ).getSignedValue() == 16 );
  ufAssert( dval( add2, 0 ).extract( 16, 8 ).getSignedValue() == 123 );
  ufAssert( dval( add2, 0 ).extract( 24, 8 ).getSignedValue() == 56 );

  return( 0 );
}
