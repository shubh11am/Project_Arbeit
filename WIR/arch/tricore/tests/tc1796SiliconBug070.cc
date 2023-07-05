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
#include <arch/tricore/optimizations/siliconbugs/tc1796_cpu_tc_070.h>


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
  auto &b5 = f.pushBackBasicBlock( {} );
  auto &b6 = f.pushBackBasicBlock( {} );

  b1.pushBackInstruction(
    { { TC13::OpCode::ADD, TC13::OperationFormat::DDD_1,
        WIR_RegisterParameter( p.D7(), WIR_Usage::def ),
        WIR_RegisterParameter( p.D8(), WIR_Usage::use ),
        WIR_RegisterParameter( p.D9(), WIR_Usage::use ) } } );
  b1.pushBackInstruction(
    { { TC13::OpCode::JNE_A, TC13::OperationFormat::AAL,
        WIR_RegisterParameter( p.A7(), WIR_Usage::use ),
        WIR_RegisterParameter( p.A15(), WIR_Usage::use ),
        WIR_LabelParameter( b2 ) } } );
  b1.pushBackInstruction(
    { { TC13::OpCode::LOOP, TC13::OperationFormat::SAL_2,
        WIR_RegisterParameter( p.A6(), WIR_Usage::defuse ),
        WIR_LabelParameter( b1 ) } } );

  b2.pushBackInstruction(
    { { TC13::OpCode::ADD, TC13::OperationFormat::DDD_1,
        WIR_RegisterParameter( p.D7(), WIR_Usage::def ),
        WIR_RegisterParameter( p.D8(), WIR_Usage::use ),
        WIR_RegisterParameter( p.D9(), WIR_Usage::use ) } } );
  b2.pushBackInstruction(
    { { TC13::OpCode::JNZ_A, TC13::OperationFormat::SAL_1,
        WIR_RegisterParameter( p.A7(), WIR_Usage::use ),
        WIR_LabelParameter( b3 ) } } );
  b2.pushBackInstruction(
    { { TC13::OpCode::LOOP, TC13::OperationFormat::SAL_2,
        WIR_RegisterParameter( p.A6(), WIR_Usage::defuse ),
        WIR_LabelParameter( b2 ) } } );

  b3.pushBackInstruction(
    { { TC13::OpCode::ADD, TC13::OperationFormat::DDD_1,
        WIR_RegisterParameter( p.D7(), WIR_Usage::def ),
        WIR_RegisterParameter( p.D8(), WIR_Usage::use ),
        WIR_RegisterParameter( p.D9(), WIR_Usage::use ) } } );
  b3.pushBackInstruction(
    { { TC13::OpCode::JNE_A, TC13::OperationFormat::AAL,
        WIR_RegisterParameter( p.A7(), WIR_Usage::use ),
        WIR_RegisterParameter( p.A15(), WIR_Usage::use ),
        WIR_LabelParameter( b1 ) } } );
  b3.pushBackInstruction(
    { { TC13::OpCode::LOOP, TC13::OperationFormat::SAL_2,
        WIR_RegisterParameter( p.A6(), WIR_Usage::defuse ),
        WIR_LabelParameter( b3 ) } } );

  b4.pushBackInstruction(
    { { TC13::OpCode::ADD, TC13::OperationFormat::DDD_1,
        WIR_RegisterParameter( p.D7(), WIR_Usage::def ),
        WIR_RegisterParameter( p.D8(), WIR_Usage::use ),
        WIR_RegisterParameter( p.D9(), WIR_Usage::use ) } } );
  b4.pushBackInstruction(
    { { TC13::OpCode::JGE_U, TC13::OperationFormat::DC4L_2,
        WIR_RegisterParameter( p.D13(), WIR_Usage::use ),
        TC_Const4_Unsigned( 4 ),
        WIR_LabelParameter( b5 ) } } );
  b4.pushBackInstruction(
    { { TC13::OpCode::LOOP, TC13::OperationFormat::SAL_2,
        WIR_RegisterParameter( p.A6(), WIR_Usage::defuse ),
        WIR_LabelParameter( b4 ) } } );

  b5.pushBackInstruction(
    { { TC13::OpCode::ADD, TC13::OperationFormat::DDD_1,
        WIR_RegisterParameter( p.D7(), WIR_Usage::def ),
        WIR_RegisterParameter( p.D8(), WIR_Usage::use ),
        WIR_RegisterParameter( p.D9(), WIR_Usage::use ) } } );
  b5.pushBackInstruction(
    { { TC13::OpCode::JGE_U, TC13::OperationFormat::DC4L_2,
        WIR_RegisterParameter( p.D13(), WIR_Usage::use ),
        TC_Const4_Unsigned( 4 ),
        WIR_LabelParameter( b6 ) } } );
  b5.pushBackInstruction(
    { { TC13::OpCode::NOP, TC13::OperationFormat::SYS } } );
  b5.pushBackInstruction(
    { { TC13::OpCode::LOOP, TC13::OperationFormat::SAL_2,
        WIR_RegisterParameter( p.A6(), WIR_Usage::defuse ),
        WIR_LabelParameter( b5 ) } } );

  TC1796_CPU_TC_070 tc070( f );
  tc070.optimize();

  ufAssert( b1.getInstructions().size() == 4 );
  auto it = b1.begin();
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::ADD );
  ++it;
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::JNE_A );
  ++it;
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::NOP );
  ++it;
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::LOOP );

  ufAssert( b2.getInstructions().size() == 3 );
  it = b2.begin();
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::ADD );
  ++it;
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::JNZ_A );
  ++it;
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::LOOP );

  ufAssert( b3.getInstructions().size() == 3 );
  it = b3.begin();
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::ADD );
  ++it;
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::JNE_A );
  ++it;
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::LOOP );

  ufAssert( b4.getInstructions().size() == 4 );
  it = b4.begin();
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::ADD );
  ++it;
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::JGE_U );
  ++it;
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::NOP );
  ufAssert( it->get().rbegin()->get().getOpCode() == TC13::OpCode::NOP );
  ++it;
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::LOOP );

  ufAssert( b5.getInstructions().size() == 5 );
  it = b5.begin();
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::ADD );
  ++it;
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::JGE_U );
  ++it;
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::NOP );
  ++it;
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::NOP );
  ++it;
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::LOOP );

  ufAssert( b6.getInstructions().size() == 0 );

  for ( WIR_BasicBlock &b : f )
    for ( WIR_Instruction &i : b )
      i.setDontOptimize( false );

  return( 0 );
}
