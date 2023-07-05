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
#include <arch/tricore/optimizations/siliconbugs/tc1796_cpu_tc_081.h>


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
  auto &sp = f.pushBackVirtualRegister( TC_ARegV() );
  auto &p10 = f.pushBackVirtualRegister( TC_PRegV() );
  f.insertPrecolor( sp, p.SP() );
  f.insertPrecolor( p10, p.P10() );
  auto &b1 = f.pushBackBasicBlock( {} );

  b1.pushBackInstruction(
    { { TC13::OpCode::LD_A, TC13::OperationFormat::AC18ABSA,
        WIR_RegisterParameter( sp, WIR_Usage::def ),
        TC_Const18_Unsigned( 42 ) } } );
  b1.pushBackInstruction(
    { { TC13::OpCode::LD_DA, TC13::OperationFormat::PC18ABSA,
        WIR_RegisterParameter( p10, WIR_Usage::def ),
        TC_Const18_Unsigned( 128 ) } } );

  TC1796_CPU_TC_081 tc081( f );
  tc081.optimize();

  ufAssert( b1.getInstructions().size() == 4 );
  auto it = b1.begin();
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::LD_A );
  ufAssert(
    !TC13::isSP(
      dynamic_cast<WIR_RegisterParameter &>(
        it->get().begin()->get().begin()->get() ).getRegister() ) );
  ++it;
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::MOV_AA );
  ufAssert(
    TC13::isSP(
      dynamic_cast<WIR_RegisterParameter &>(
        it->get().begin()->get().begin()->get() ).getRegister() ) );
  ++it;
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::LD_DA );
  ufAssert(
    ! dynamic_cast<WIR_VirtualRegister &>(
        dynamic_cast<WIR_RegisterParameter &>(
          it->get().begin()->get().begin()->get() ).getRegister() ).isPrecolored() );
  ++it;
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::MOV_AA );
  ufAssert(
    dynamic_cast<WIR_VirtualRegister &>(
      dynamic_cast<WIR_RegisterParameter &>(
        it->get().begin()->get().begin()->get() ).getRegister() ).getPrecolor().
        getName() == "a10" );
  ufAssert( it->get().rbegin()->get().getOpCode() == TC13::OpCode::MOV_AA );
  ufAssert(
    dynamic_cast<WIR_VirtualRegister &>(
      dynamic_cast<WIR_RegisterParameter &>(
        it->get().rbegin()->get().begin()->get() ).getRegister() ).getPrecolor().
        getName() == "a11" );

  for ( WIR_BasicBlock &b : f )
    for ( WIR_Instruction &i : b )
      i.setDontOptimize( false );

  return( 0 );
}
