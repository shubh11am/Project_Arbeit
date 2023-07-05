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
#include <arch/riscv/rv32ic.h>
#include <arch/riscv/analyses/bit/rv32bitdfa.h>


using namespace std;
using namespace WIR;


int main( void )
{
  WIR_Init();

  // This test case tests the bit-true top-down data flow analysis for RISC-V
  // operation SW, SH and SB.

  RV32I p;
  WIR_CompilationUnit c;
  c.setName( __FILE__ );
  WIR_Function &f = c.pushBackFunction( WIR_Function { "main" } );
  WIR_BasicBlock &b1 = f.pushBackBasicBlock( {} );
  auto &input1 = f.pushBackVirtualRegister( RV_RegV() );
  auto &input2 = f.pushBackVirtualRegister( RV_RegV() );
  auto &input3 = f.pushBackVirtualRegister( RV_RegV() );
  auto &zero = f.pushBackVirtualRegister( RV_RegV() );
  f.insertPrecolor( zero, p.x0() );

  // This lambda serves for generating and inserting a RISC-V operation.
  auto rv32op = [&]( WIR_Operation &&o ) -> WIR_Operation & {
    auto &i = b1.pushBackInstruction( WIR_Instruction { WIR_Operation { o } } );
    return( i.begin()->get() );
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
      new RV_Const12_Signed( 64 ) } );

  auto &sw1 = rv32op(
    { RV32I::OpCode::SW, RV32I::OperationFormat::RC12R_2,
      new WIR_RegisterParameter( zero, WIR_Usage::use ),
      new RV_Const12_Signed( 0 ),
      new WIR_RegisterParameter( input1, WIR_Usage::use ) } );

  rv32op(
    { RV32I::OpCode::ADDI, RV32I::OperationFormat::RRC12_1,
      new WIR_RegisterParameter( input2, WIR_Usage::def ),
      new WIR_RegisterParameter( p.x0(), WIR_Usage::use ),
      new RV_Const12_Signed( 64 ) } );

  auto &sh1 = rv32op(
    { RV32I::OpCode::SH, RV32I::OperationFormat::RC12R_2,
      new WIR_RegisterParameter( zero, WIR_Usage::use ),
      new RV_Const12_Signed( 0 ),
      new WIR_RegisterParameter( input2, WIR_Usage::use ) } );

  rv32op(
    { RV32I::OpCode::ADDI, RV32I::OperationFormat::RRC12_1,
      new WIR_RegisterParameter( input3, WIR_Usage::def ),
      new WIR_RegisterParameter( p.x0(), WIR_Usage::use ),
      new RV_Const12_Signed( 64 ) } );

  auto &sb1 = rv32op(
    { RV32I::OpCode::SB, RV32I::OperationFormat::RC12R_2,
      new WIR_RegisterParameter( zero, WIR_Usage::use ),
      new RV_Const12_Signed( 0 ),
      new WIR_RegisterParameter( input3, WIR_Usage::use ) } );

  // Do bit-true data flow analysis.
  RV32_BitDFA a { f };
  a.analyze();

  // Evaluate analysis results.
  ufAssert( uval( sw1, 2 ).extract( 0, 32 ).getSignedValue() == 64 );

  ufAssert( uval( sh1, 2 ).extract( 0, 16 ).getSignedValue() == 64 );
  ufAssert( uval( sh1, 2 ).extract( 16, 16 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( uval( sb1, 2 ).extract( 0, 8 ).getSignedValue() == 64 );
  ufAssert( uval( sb1, 2 ).extract( 8, 24 ).containsOnlyBit( WIR_L4::bX ) );

  return( 0 );
}
