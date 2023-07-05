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
  // operation COR.

  RV32I p;
  WIR_CompilationUnit c;
  c.setName( __FILE__ );
  WIR_Function &f = c.pushBackFunction( WIR_Function { "main" } );
  WIR_BasicBlock &b1 = f.pushBackBasicBlock( {} );
  auto &input1 = f.pushBackVirtualRegister( RV_RegV() );
  auto &inputLeft1 = f.pushBackVirtualRegister( RV_RegV() );
  auto &input2 = f.pushBackVirtualRegister( RV_RegV() );
  auto &inputLeft2 = f.pushBackVirtualRegister( RV_RegV() );
  auto &input3 = f.pushBackVirtualRegister( RV_RegV() );
  auto &input4 = f.pushBackVirtualRegister( RV_RegV() );
  auto &tmp = f.pushBackVirtualRegister( RV_RegV() );

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
      new WIR_RegisterParameter( input1, WIR_Usage::def ),
      new WIR_RegisterParameter( input1, WIR_Usage::use ),
      new WIR_RegisterParameter( tmp, WIR_Usage::use ) } );

  // Create Left Vector (0000 1111 0000 1111 0000 1111 0000 1111).
  rv32op(
    { RV32I::OpCode::ADDI, RV32I::OperationFormat::RRC12_1,
      new WIR_RegisterParameter( inputLeft1, WIR_Usage::def ),
      new WIR_RegisterParameter( p.x0(), WIR_Usage::use ),
      new RV_Const12_Signed( 15 ) } );
  rv32op(
    { RV32I::OpCode::SLLI, RV32I::OperationFormat::RRC5_1,
      new WIR_RegisterParameter( inputLeft1, WIR_Usage::def ),
      new WIR_RegisterParameter( inputLeft1, WIR_Usage::use ),
      new RV_Const5_Unsigned( 8 ) } );

  rv32op(
    { RV32I::OpCode::ADDI, RV32I::OperationFormat::RRC12_1,
      new WIR_RegisterParameter( inputLeft2, WIR_Usage::def ),
      new WIR_RegisterParameter( p.x0(), WIR_Usage::use ),
      new RV_Const12_Signed( 15 ) } );

  rv32op(
    { RV32I::OpCode::OR, RV32I::OperationFormat::RRR_1,
      new WIR_RegisterParameter( inputLeft1, WIR_Usage::def ),
      new WIR_RegisterParameter( inputLeft1, WIR_Usage::use ),
      new WIR_RegisterParameter( inputLeft2, WIR_Usage::use ) } );
  rv32op(
    { RV32I::OpCode::SLLI, RV32I::OperationFormat::RRC5_1,
      new WIR_RegisterParameter( inputLeft1, WIR_Usage::def ),
      new WIR_RegisterParameter( inputLeft1, WIR_Usage::use ),
      new RV_Const5_Unsigned( 8 ) } );

  rv32op(
    { RV32I::OpCode::ADDI, RV32I::OperationFormat::RRC12_1,
      new WIR_RegisterParameter( inputLeft2, WIR_Usage::def ),
      new WIR_RegisterParameter( p.x0(), WIR_Usage::use ),
      new RV_Const12_Signed( 15 ) } );

  rv32op(
    { RV32I::OpCode::OR, RV32I::OperationFormat::RRR_1,
      new WIR_RegisterParameter( inputLeft1, WIR_Usage::def ),
      new WIR_RegisterParameter( inputLeft1, WIR_Usage::use ),
      new WIR_RegisterParameter( inputLeft2, WIR_Usage::use ) } );
  rv32op(
    { RV32I::OpCode::SLLI, RV32I::OperationFormat::RRC5_1,
      new WIR_RegisterParameter( inputLeft1, WIR_Usage::def ),
      new WIR_RegisterParameter( inputLeft1, WIR_Usage::use ),
      new RV_Const5_Unsigned( 8 ) } );

  rv32op(
    { RV32I::OpCode::ADDI, RV32I::OperationFormat::RRC12_1,
      new WIR_RegisterParameter( inputLeft2, WIR_Usage::def ),
      new WIR_RegisterParameter( p.x0(), WIR_Usage::use ),
      new RV_Const12_Signed( 15 ) } );

  rv32op(
    { RV32I::OpCode::OR, RV32I::OperationFormat::RRR_1,
      new WIR_RegisterParameter( inputLeft1, WIR_Usage::def ),
      new WIR_RegisterParameter( inputLeft1, WIR_Usage::use ),
      new WIR_RegisterParameter( inputLeft2, WIR_Usage::use ) } );

  // COR
  auto &cor1 = rv32op(
    { RV32IC::OpCode::COR, RV32IC::OperationFormat::SRR_2,
      new WIR_RegisterParameter( input1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( inputLeft1, WIR_Usage::use ) } );

  rv32op(
    { RV32I::OpCode::SLLI, RV32I::OperationFormat::RRC5_1,
      new WIR_RegisterParameter( input1, WIR_Usage::def ),
      new WIR_RegisterParameter( input1, WIR_Usage::use ),
      new RV_Const5_Unsigned( 8 ) } );

  // Do bit-true data flow analysis.
  RV32_BitDFA a { f };
  a.analyze();

  // Evaluate analysis results.
  ufAssert( uval( cor1, 0 ).extract( 0, 4 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( cor1, 0 ).extract( 4, 4 ).containsOnlyBit( WIR_L4::bL ) );
  ufAssert(
    uval( cor1, 0 ).getLocation( 4 ).getRegisterParameter().getRegister() ==
      input4 );
  ufAssert( uval( cor1, 0 ).getLocation( 4 ).getBitPosition() == 4 );
  ufAssert(
    uval( cor1, 0 ).getLocation( 5 ).getRegisterParameter().getRegister() ==
      input4 );
  ufAssert( uval( cor1, 0 ).getLocation( 5 ).getBitPosition() == 5 );
  ufAssert(
    uval( cor1, 0 ).getLocation( 6 ).getRegisterParameter().getRegister() ==
      input4 );
  ufAssert( uval( cor1, 0 ).getLocation( 6 ).getBitPosition() == 6 );
  ufAssert(
    uval( cor1, 0 ).getLocation( 7 ).getRegisterParameter().getRegister() ==
      input4 );
  ufAssert( uval( cor1, 0 ).getLocation( 7 ).getBitPosition() == 7 );
  ufAssert( uval( cor1, 0 ).extract( 8, 4 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( cor1, 0 ).extract( 12, 4 ).getSignedValue() == 4 );
  ufAssert( uval( cor1, 0 ).extract( 16, 4 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( cor1, 0 ).extract( 20, 4 ).getSignedValue() == 7 );
  ufAssert( uval( cor1, 0 ).extract( 24, 8 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( uval( cor1, 1 ).extract( 0, 14 ).getSignedValue() == 3855 );
  ufAssert( uval( cor1, 1 ).extract( 14, 1 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( cor1, 1 ).extract( 15, 5 ).getSignedValue() == 30 );
  ufAssert( uval( cor1, 1 ).extract( 20, 3 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( cor1, 1 ).extract( 23, 1 ).containsOnlyBit( WIR_L4::b0 ) );
  ufAssert( uval( cor1, 1 ).extract( 24, 8 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( cor1, 0 ).extract( 0, 4 ).containsOnlyBit( WIR_L4::b1 ) );
  ufAssert( dval( cor1, 0 ).extract( 4, 4 ).containsOnlyBit( WIR_L4::bL ) );
  ufAssert(
    dval( cor1, 0 ).getLocation( 4 ).getRegisterParameter().getRegister() ==
      input4 );
  ufAssert( dval( cor1, 0 ).getLocation( 4 ).getBitPosition() == 4 );
  ufAssert(
    dval( cor1, 0 ).getLocation( 5 ).getRegisterParameter().getRegister() ==
      input4 );
  ufAssert( dval( cor1, 0 ).getLocation( 5 ).getBitPosition() == 5 );
  ufAssert(
    dval( cor1, 0 ).getLocation( 6 ).getRegisterParameter().getRegister() ==
      input4 );
  ufAssert( dval( cor1, 0 ).getLocation( 6 ).getBitPosition() == 6 );
  ufAssert(
    dval( cor1, 0 ).getLocation( 7 ).getRegisterParameter().getRegister() ==
      input4 );
  ufAssert( dval( cor1, 0 ).getLocation( 7 ).getBitPosition() == 7 );
  ufAssert( dval( cor1, 0 ).extract( 8, 24 ).getSignedValue() == 4161359 );

  return( 0 );
}
