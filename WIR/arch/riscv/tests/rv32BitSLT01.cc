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
  // operation SLT.

  RV32I p;
  WIR_CompilationUnit c;
  c.setName( __FILE__ );
  WIR_Function &f = c.pushBackFunction( WIR_Function { "main" } );
  WIR_BasicBlock &b1 = f.pushBackBasicBlock( {} );
  auto &input1 = f.pushBackVirtualRegister( RV_RegV() );
  auto &input2 = f.pushBackVirtualRegister( RV_RegV() );
  auto &inputV0 = f.pushBackVirtualRegister( RV_RegV() );
  auto &inputV32 = f.pushBackVirtualRegister( RV_RegV() );
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
      new WIR_RegisterParameter( inputV32, WIR_Usage::def ),
      new WIR_RegisterParameter( p.x0(), WIR_Usage::use ),
      new RV_Const12_Signed( 32 ) } );

  rv32op(
    { RV32I::OpCode::ADDI, RV32I::OperationFormat::RRC12_1,
      new WIR_RegisterParameter( inputV0, WIR_Usage::def ),
      new WIR_RegisterParameter( p.x0(), WIR_Usage::use ),
      new RV_Const12_Signed( 0 ) } );

  rv32op(
    { RV32I::OpCode::ADDI, RV32I::OperationFormat::RRC12_1,
      new WIR_RegisterParameter( input1, WIR_Usage::def ),
      new WIR_RegisterParameter( p.x0(), WIR_Usage::use ),
      new RV_Const12_Signed( 30 ) } );

  auto &slt1 = rv32op(
    { RV32I::OpCode::SLT, RV32I::OperationFormat::RRR_1,
      new WIR_RegisterParameter( input1, WIR_Usage::def ),
      new WIR_RegisterParameter( input1, WIR_Usage::use ),
      new WIR_RegisterParameter( inputV32, WIR_Usage::use ) } );

  auto &slt2 = rv32op(
    { RV32I::OpCode::SLT, RV32I::OperationFormat::RRR_1,
      new WIR_RegisterParameter( input1, WIR_Usage::def ),
      new WIR_RegisterParameter( input1, WIR_Usage::use ),
      new WIR_RegisterParameter( inputV0, WIR_Usage::use ) } );

  auto &slt3 = rv32op(
    { RV32I::OpCode::SLT, RV32I::OperationFormat::RRR_1,
      new WIR_RegisterParameter( input1, WIR_Usage::def ),
      new WIR_RegisterParameter( input1, WIR_Usage::use ),
      new WIR_RegisterParameter( inputV0, WIR_Usage::use ) } );

  rv32op(
    { RV32I::OpCode::SRLI, RV32I::OperationFormat::RRC5_1,
      new WIR_RegisterParameter( input1, WIR_Usage::def ),
      new WIR_RegisterParameter( input1, WIR_Usage::use ),
      new RV_Const5_Unsigned( 1 ) } );

  rv32op(
    { RV32I::OpCode::ADDI, RV32I::OperationFormat::RRC12_1,
      new WIR_RegisterParameter( input2, WIR_Usage::def ),
      new WIR_RegisterParameter( p.x0(), WIR_Usage::use ),
      new RV_Const12_Signed( 30 ) } );

  auto &slt4 = rv32op(
    { RV32I::OpCode::SLT, RV32I::OperationFormat::RRR_1,
      new WIR_RegisterParameter( input2, WIR_Usage::def ),
      new WIR_RegisterParameter( input2, WIR_Usage::use ),
      new WIR_RegisterParameter( inputV32, WIR_Usage::use ) } );

  auto &slt5 = rv32op(
    { RV32I::OpCode::SLT, RV32I::OperationFormat::RRR_1,
      new WIR_RegisterParameter( input2, WIR_Usage::def ),
      new WIR_RegisterParameter( input2, WIR_Usage::use ),
      new WIR_RegisterParameter( inputV0, WIR_Usage::use ) } );

  auto &slt6 = rv32op(
    { RV32I::OpCode::SLT, RV32I::OperationFormat::RRR_1,
      new WIR_RegisterParameter( input2, WIR_Usage::def ),
      new WIR_RegisterParameter( input2, WIR_Usage::use ),
      new WIR_RegisterParameter( inputV0, WIR_Usage::use ) } );

  rv32op(
    { RV32I::OpCode::OR, RV32I::OperationFormat::RRR_1,
      new WIR_RegisterParameter( tmp, WIR_Usage::def ),
      new WIR_RegisterParameter( input1, WIR_Usage::use ),
      new WIR_RegisterParameter( input2, WIR_Usage::use ) } );

  // Do bit-true data flow analysis.
  RV32_BitDFA a { f };
  a.analyze();

  // Evaluate analysis results.
  ufAssert( uval( slt1, 1 ).extract( 0, 32 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( dval( slt1, 0 ).extract( 0, 32 ).getSignedValue() == 1 );

  ufAssert( uval( slt2, 1 ).extract( 0, 32 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( dval( slt2, 0 ).extract( 0, 32 ).getSignedValue() == 0 );

  ufAssert( uval( slt3, 1 ).extract( 0, 32 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( dval( slt3, 0 ).extract( 0, 32 ).getSignedValue() == 0 );

  ufAssert( uval( slt4, 1 ).extract( 0, 32 ).getSignedValue() == 30 );
  ufAssert( dval( slt4, 0 ).extract( 0, 32 ).getSignedValue() == 1 );

  ufAssert( uval( slt5, 1 ).extract( 0, 32 ).getSignedValue() == 1 );
  ufAssert( dval( slt5, 0 ).extract( 0, 32 ).getSignedValue() == 0 );

  ufAssert( uval( slt6, 1 ).extract( 0, 32 ).getSignedValue() == 0 );
  ufAssert( dval( slt6, 0 ).extract( 0, 32 ).getSignedValue() == 0 );

  return( 0 );
}
