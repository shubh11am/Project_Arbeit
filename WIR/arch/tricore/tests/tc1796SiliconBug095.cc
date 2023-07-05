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


//
// Include section
//

// Include libuseful headers
#include <libuseful/io.h>

// Include WIR headers
#include <wir/wir.h>
#include <arch/tricore/tc131.h>
#include <arch/tricore/optimizations/siliconbugs/tc1796_cpu_tc_095.h>


using namespace std;
using namespace WIR;


int main( void )
{
  WIR_Init();
  WIR_TaskManager t;
  WIR_System sys( "tc1796.sys", t );

  auto &p = sys.getComponents<TC13>().begin()->get();

  auto &c = sys.pushBackCompilationUnit( {} );
  auto &f = c.pushBackFunction( WIR_Function( "main" ) );
  auto &b1 = f.pushBackBasicBlock( {} );

  b1.pushBackInstruction(
    { { TC13::OpCode::MOV_D, TC13::OperationFormat::DA,
        WIR_RegisterParameter( p.D2(), WIR_Usage::def ),
        WIR_RegisterParameter( p.A7(), WIR_Usage::use ) } } );
  b1.pushBackInstruction(
    { { TC13::OpCode::SAT_H, TC13::OperationFormat::DD,
        WIR_RegisterParameter( p.D15(), WIR_Usage::def ),
        WIR_RegisterParameter( p.D12(), WIR_Usage::use ) } } );
  b1.pushBackInstruction(
    { { TC13::OpCode::MOV_A, TC13::OperationFormat::SAD_1,
        WIR_RegisterParameter( p.A4(), WIR_Usage::def ),
        WIR_RegisterParameter( p.D2(), WIR_Usage::use ) } } );
  b1.pushBackInstruction(
    { { TC13::OpCode::LD_D, TC13::OperationFormat::EC18ABSA,
        WIR_RegisterParameter( p.E4(), WIR_Usage::def ),
        TC_Const18_Unsigned( 236 ) } } );
  b1.pushBackInstruction(
    { { TC13::OpCode::SAT_B, TC13::OperationFormat::SD,
        WIR_RegisterParameter( p.D15(), WIR_Usage::defuse ) } } );
  b1.pushBackInstruction(
    { { TC13::OpCode::MTCR, TC13::OperationFormat::C16DPSW,
        TC_Const16_Unsigned( 815 ),
        WIR_RegisterParameter( p.D5(), WIR_Usage::use ),
        WIR_RegisterParameter( p.PSW_C(), WIR_Usage::def ) } } );
  b1.pushBackInstruction(
    { { TC13::OpCode::LD_D, TC13::OperationFormat::EC18ABSA,
        WIR_RegisterParameter( p.E4(), WIR_Usage::def ),
        TC_Const18_Unsigned( 236 ) } } );
  b1.pushBackInstruction(
    { { TC13::OpCode::SAT_B, TC13::OperationFormat::SD,
        WIR_RegisterParameter( p.D15(), WIR_Usage::defuse ) } } );
  b1.pushBackInstruction(
    { { TC13::OpCode::MTCR, TC13::OperationFormat::C16DPSW,
        TC_Const16_Unsigned( 815 ),
        WIR_RegisterParameter( p.D4(), WIR_Usage::use ),
        WIR_RegisterParameter( p.PSW_C(), WIR_Usage::def ) } } );

  TC1796_CPU_TC_095 tc095( f );
  tc095.optimize();

  ufAssert( b1.getInstructions().size() == 11 );
  auto it = b1.begin();
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::MOV_D );
  ++it;
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::SAT_H );
  ++it;
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::NOP );
  ++it;
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::MOV_A );
  ++it;
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::LD_D );
  ++it;
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::SAT_B );
  ++it;
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::NOP );
  ++it;
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::MTCR );
  ++it;
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::LD_D );
  ++it;
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::SAT_B );
  ++it;
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::MTCR );

  for ( WIR_BasicBlock &b : f )
    for ( WIR_Instruction &i : b )
      i.setDontOptimize( false );

  return( 0 );
}
