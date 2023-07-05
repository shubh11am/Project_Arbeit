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
#include <arch/tricore/optimizations/siliconbugs/tc1796_cpu_tc_069.h>


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
    { { TC13::OpCode::RSLCX, TC13::OperationFormat::SYS } } );
  b1.pushBackInstruction(
    { { TC13::OpCode::LOOP, TC13::OperationFormat::AL_3,
        WIR_RegisterParameter( p.A1(), WIR_Usage::defuse ),
        WIR_LabelParameter( b1 ) } } );
  b2.pushBackInstruction(
    { { TC13::OpCode::RSLCX, TC13::OperationFormat::SYS } } );
  b2.pushBackInstruction(
    { { TC13::OpCode::ADD, TC13::OperationFormat::DDD_1,
        WIR_RegisterParameter( p.D7(), WIR_Usage::def ),
        WIR_RegisterParameter( p.D8(), WIR_Usage::use ),
        WIR_RegisterParameter( p.D9(), WIR_Usage::use ) } } );
  b2.pushBackInstruction(
    { { TC13::OpCode::LEA, TC13::OperationFormat::AAC16BOA,
        WIR_RegisterParameter( p.A0(), WIR_Usage::def ),
        WIR_RegisterParameter( p.A0(), WIR_Usage::use ),
        TC_Const16_Signed( 0x158c ) } } );
  b2.pushBackInstruction(
    { { TC13::OpCode::RSLCX, TC13::OperationFormat::SYS } } );
  b2.pushBackInstruction(
    { { TC13::OpCode::ADD, TC13::OperationFormat::DDD_1,
        WIR_RegisterParameter( p.D7(), WIR_Usage::def ),
        WIR_RegisterParameter( p.D8(), WIR_Usage::use ),
        WIR_RegisterParameter( p.D9(), WIR_Usage::use ) } } );
  b2.pushBackInstruction(
    { { TC13::OpCode::LEA, TC13::OperationFormat::AAC16BOA,
        WIR_RegisterParameter( p.A0(), WIR_Usage::def ),
        WIR_RegisterParameter( p.A8(), WIR_Usage::use ),
        TC_Const16_Signed( 0x158c ) } } );

  TC1796_CPU_TC_069 tc069( f );
  tc069.optimize();

  ufAssert( b1.getInstructions().size() == 3 );
  auto it = b1.begin();
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::RSLCX );
  ++it;
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::NOP );
  ++it;
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::LOOP );

  ufAssert( b2.getInstructions().size() == 7 );
  it = b2.begin();
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::RSLCX );
  ++it;
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::NOP );
  ++it;
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::ADD );
  ++it;
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::LEA );
  ++it;
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::RSLCX );
  ++it;
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::ADD );
  ++it;
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::LEA );

  for ( WIR_BasicBlock &b : f )
    for ( WIR_Instruction &i : b )
      i.setDontOptimize( false );

  return( 0 );
}
