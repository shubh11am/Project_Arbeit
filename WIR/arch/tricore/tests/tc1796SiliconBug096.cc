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
#include <arch/tricore/optimizations/siliconbugs/tc1796_cpu_tc_096.h>


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

  b1.pushBackInstruction(
    { { TC13::OpCode::ADD, TC13::OperationFormat::DDD_1,
        WIR_RegisterParameter( p.D7(), WIR_Usage::def ),
        WIR_RegisterParameter( p.D8(), WIR_Usage::use ),
        WIR_RegisterParameter( p.D9(), WIR_Usage::use ) } } );
  b1.pushBackInstruction(
    { { TC13::OpCode::ADD_A, TC13::OperationFormat::AAA,
        WIR_RegisterParameter( p.A3(), WIR_Usage::def ),
        WIR_RegisterParameter( p.A14(), WIR_Usage::use ),
        WIR_RegisterParameter( p.A7(), WIR_Usage::use ) } } );
  b1.pushBackInstruction(
    { { TC13::OpCode::LOOP, TC13::OperationFormat::AL_3,
        WIR_RegisterParameter( p.A7(), WIR_Usage::defuse ),
        WIR_LabelParameter( b1 ) } } );

  b2.pushBackInstruction(
    { { TC13::OpCode::ADD, TC13::OperationFormat::DDD_1,
        WIR_RegisterParameter( p.D0(), WIR_Usage::def ),
        WIR_RegisterParameter( p.D1(), WIR_Usage::use ),
        WIR_RegisterParameter( p.D2(), WIR_Usage::use ) } } );
  b2.pushBackInstruction(
    { { TC13::OpCode::LOOP, TC13::OperationFormat::AL_3,
        WIR_RegisterParameter( p.A7(), WIR_Usage::defuse ),
        WIR_LabelParameter( b2 ) } } );

  b3.pushBackInstruction(
    { { TC13::OpCode::ADD, TC13::OperationFormat::DDD_1,
        WIR_RegisterParameter( p.D7(), WIR_Usage::def ),
        WIR_RegisterParameter( p.D8(), WIR_Usage::use ),
        WIR_RegisterParameter( p.D9(), WIR_Usage::use ) } } );
  b3.pushBackInstruction(
    { { TC13::OpCode::NOP, TC13::OperationFormat::SYS } } );
  b3.pushBackInstruction(
    { { TC13::OpCode::LOOP, TC13::OperationFormat::AL_3,
        WIR_RegisterParameter( p.A7(), WIR_Usage::defuse ),
        WIR_LabelParameter( b3 ) } } );

  TC1796_CPU_TC_096 tc096( f );
  tc096.optimize();

  ufAssert( b1.getInstructions().size() == 4 );
  auto it = b1.begin();
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::ADD );
  ++it;
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::ADD_A );
  ++it;
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::NOP );
  ++it;
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::LOOP );

  ufAssert( b2.getInstructions().size() == 3 );
  it = b2.begin();
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::ADD );
  ++it;
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::NOP );
  ufAssert( it->get().rbegin()->get().getOpCode() == TC13::OpCode::NOP );
  ++it;
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::LOOP );

  ufAssert( b3.getInstructions().size() == 4 );
  it = b3.begin();
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::ADD );
  ++it;
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::NOP );
  ++it;
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::NOP );
  ++it;
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::LOOP );

  for ( WIR_BasicBlock &b : f )
    for ( WIR_Instruction &i : b )
      i.setDontOptimize( false );

  return( 0 );
}
