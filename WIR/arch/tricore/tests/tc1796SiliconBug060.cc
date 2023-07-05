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
#include <arch/tricore/optimizations/siliconbugs/tc1796_cpu_tc_060.h>


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
  auto &b2 = f.pushBackBasicBlock( {} );

  b1.pushBackInstruction(
    { { TC13::OpCode::LD_A, TC13::OperationFormat::AC18ABSA,
        WIR_RegisterParameter( p.A7(), WIR_Usage::def ),
        TC_Const18_Unsigned( 42 ) } } );
  b1.pushBackInstruction(
    { { TC13::OpCode::LD_DA, TC13::OperationFormat::PAC10BOA,
        WIR_RegisterParameter( p.P2(), WIR_Usage::def ),
        WIR_RegisterParameter( p.A7(), WIR_Usage::use ),
        TC_Const10_Signed( 444 ) } } );
  b1.pushBackInstruction(
    { { TC13::OpCode::LD_DA, TC13::OperationFormat::PC18ABSA,
        WIR_RegisterParameter( p.P2(), WIR_Usage::def ),
        TC_Const18_Unsigned( 23 ) } } );
  b2.pushBackInstruction(
    { { TC13::OpCode::LD_W, TC13::OperationFormat::DAC10PIA,
        WIR_RegisterParameter( p.D9(), WIR_Usage::def ),
        WIR_AddressingModeParameter( TC13::AddressingMode::pre ),
        WIR_RegisterParameter( p.A2(), WIR_Usage::defuse ),
        TC_Const10_Signed( 236 ) } } );

  TC1796_CPU_TC_060 tc060( f );
  tc060.optimize();

  ufAssert( b1.getInstructions().size() == 4 );
  auto it = b1.begin();
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::LD_A );
  ++it;
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::NOP );
  ++it;
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::LD_DA );
  ++it;
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::LD_DA );

  ufAssert( b2.getInstructions().size() == 2 );
  it = b2.begin();
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::NOP );
  ++it;
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::LD_W );

  for ( WIR_BasicBlock &b : f )
    for ( WIR_Instruction &i : b )
      i.setDontOptimize( false );

  return( 0 );
}
