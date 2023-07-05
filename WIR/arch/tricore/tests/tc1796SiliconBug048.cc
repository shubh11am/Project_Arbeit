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
#include <arch/tricore/optimizations/siliconbugs/tc1796_cpu_tc_048.h>


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
  auto &b3 = f.pushBackBasicBlock( {} );
  auto &b4 = f.pushBackBasicBlock( {} );

  b1.pushBackInstruction(
    { { TC13::OpCode::LD_A, TC13::OperationFormat::AC18ABSA,
        WIR_RegisterParameter( p.A7(), WIR_Usage::def ),
        TC_Const18_Unsigned( 42 ) } } );
  b1.pushBackInstruction(
    { { TC13::OpCode::JLI, TC13::OperationFormat::A,
        WIR_RegisterParameter( p.A7(), WIR_Usage::use ) } } );

  b2.pushBackInstruction(
    { { TC13::OpCode::LD_DA, TC13::OperationFormat::PC18ABSA,
        WIR_RegisterParameter( p.P2(), WIR_Usage::def ),
        TC_Const18_Unsigned( 23 ) } } );
  b3.pushBackInstruction(
    { { TC13::OpCode::CALLI, TC13::OperationFormat::A,
        WIR_RegisterParameter( p.A3(), WIR_Usage::use ) } } );

  b4.pushBackInstruction(
    { { TC13::OpCode::LD_A, TC13::OperationFormat::AC18ABSA,
        WIR_RegisterParameter( p.A4(), WIR_Usage::def ),
        TC_Const18_Unsigned( 64 ) } } );
  b4.pushBackInstruction(
    { { TC13::OpCode::ADD, TC13::OperationFormat::DDD_1,
        WIR_RegisterParameter( p.D7(), WIR_Usage::def ),
        WIR_RegisterParameter( p.D8(), WIR_Usage::use ),
        WIR_RegisterParameter( p.D9(), WIR_Usage::use ) } } );
  b4.pushBackInstruction(
    { { TC13::OpCode::JI, TC13::OperationFormat::A,
        WIR_RegisterParameter( p.A4(), WIR_Usage::use ) } } );

  TC1796_CPU_TC_048 tc048( f );
  tc048.optimize();

  ufAssert( b1.getInstructions().size() == 3 );
  auto it = b1.begin();
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::LD_A );
  ++it;
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::NOP );
  ++it;
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::JLI );

  ufAssert( b2.getInstructions().size() == 1 );
  it = b2.begin();
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::LD_DA );

  ufAssert( b3.getInstructions().size() == 2 );
  it = b3.begin();
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::NOP );
  ++it;
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::CALLI );

  ufAssert( b4.getInstructions().size() == 4 );
  it = b4.begin();
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::LD_A );
  ++it;
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::NOP );
  ++it;
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::ADD );
  ++it;
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::JI );

  for ( WIR_BasicBlock &b : f )
    for ( WIR_Instruction &i : b )
      i.setDontOptimize( false );

  return( 0 );
}
