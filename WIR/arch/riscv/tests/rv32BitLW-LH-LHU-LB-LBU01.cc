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
  // operations LW, LH, LHU, LB and LBU.

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
  auto &input6 = f.pushBackVirtualRegister( RV_RegV() );
  auto &input7 = f.pushBackVirtualRegister( RV_RegV() );
  auto &input8 = f.pushBackVirtualRegister( RV_RegV() );
  auto &input9 = f.pushBackVirtualRegister( RV_RegV() );
  auto &input10 = f.pushBackVirtualRegister( RV_RegV() );

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

  // Create WIR code.

  auto &lw1 = rv32op(
    { RV32I::OpCode::LW, RV32I::OperationFormat::RC12R_1,
      new WIR_RegisterParameter( input2, WIR_Usage::def ),
      new RV_Const12_Signed( 16 ),
      new WIR_RegisterParameter( input1, WIR_Usage::use ) } );

  rv32op(
    { RV32I::OpCode::SLLI, RV32I::OperationFormat::RRC5_1,
      new WIR_RegisterParameter( input2, WIR_Usage::def ),
      new WIR_RegisterParameter( input2, WIR_Usage::use ),
      new RV_Const5_Unsigned( 0 ) } );

  auto &lh1 = rv32op(
    { RV32I::OpCode::LH, RV32I::OperationFormat::RC12R_1,
      new WIR_RegisterParameter( input4, WIR_Usage::def ),
      new RV_Const12_Signed( 16 ),
      new WIR_RegisterParameter( input3, WIR_Usage::use ) } );

  rv32op(
    { RV32I::OpCode::SLLI, RV32I::OperationFormat::RRC5_1,
      new WIR_RegisterParameter( input4, WIR_Usage::def ),
      new WIR_RegisterParameter( input4, WIR_Usage::use ),
      new RV_Const5_Unsigned( 0 ) } );

  auto &lhu1 = rv32op(
    { RV32I::OpCode::LHU, RV32I::OperationFormat::RC12R_1,
      new WIR_RegisterParameter( input6, WIR_Usage::def ),
      new RV_Const12_Signed( 16 ),
      new WIR_RegisterParameter( input5, WIR_Usage::use ) } );

  rv32op(
    { RV32I::OpCode::SLLI, RV32I::OperationFormat::RRC5_1,
      new WIR_RegisterParameter( input6, WIR_Usage::def ),
      new WIR_RegisterParameter( input6, WIR_Usage::use ),
      new RV_Const5_Unsigned( 0 ) } );

  auto &lb1 = rv32op(
    { RV32I::OpCode::LB, RV32I::OperationFormat::RC12R_1,
      new WIR_RegisterParameter( input8, WIR_Usage::def ),
      new RV_Const12_Signed( 16 ),
      new WIR_RegisterParameter( input7, WIR_Usage::use ) } );

  rv32op(
    { RV32I::OpCode::SLLI, RV32I::OperationFormat::RRC5_1,
      new WIR_RegisterParameter( input8, WIR_Usage::def ),
      new WIR_RegisterParameter( input8, WIR_Usage::use ),
      new RV_Const5_Unsigned( 0 ) } );

  auto &lbu1 = rv32op(
    { RV32I::OpCode::LBU, RV32I::OperationFormat::RC12R_1,
      new WIR_RegisterParameter( input10, WIR_Usage::def ),
      new RV_Const12_Signed( 16 ),
      new WIR_RegisterParameter( input9, WIR_Usage::use ) } );

  rv32op(
    { RV32I::OpCode::SLLI, RV32I::OperationFormat::RRC5_1,
      new WIR_RegisterParameter( input10, WIR_Usage::def ),
      new WIR_RegisterParameter( input10, WIR_Usage::use ),
      new RV_Const5_Unsigned( 0 ) } );

  // Do bit-true data flow analysis.
  RV32_BitDFA a { f };
  a.analyze();

  // Evaluate analysis results.
  ufAssert( dval( lw1, 0 ).extract( 0, 32 ).containsOnlyBit( WIR_L4::bL ) );

  ufAssert( dval( lh1, 0 ).extract( 0, 32 ).containsOnlyBit( WIR_L4::bL ) );

  ufAssert( dval( lb1, 0 ).extract( 0, 32 ).containsOnlyBit( WIR_L4::bL ) );

  ufAssert( dval( lhu1, 0 ).extract( 0, 16 ).containsOnlyBit( WIR_L4::bL ) );
  ufAssert( dval( lhu1, 0 ).extract( 16, 16 ).containsOnlyBit( WIR_L4::b0 ) );

  ufAssert( dval( lbu1, 0 ).extract( 0, 8 ).containsOnlyBit( WIR_L4::bL ) );
  ufAssert( dval( lbu1, 0 ).extract( 8, 24 ).containsOnlyBit( WIR_L4::b0 ) );

  return( 0 );
}
