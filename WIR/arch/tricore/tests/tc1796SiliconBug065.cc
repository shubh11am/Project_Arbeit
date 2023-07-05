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
#include <arch/tricore/optimizations/siliconbugs/tc1796_cpu_tc_065.h>


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
    { { TC13::OpCode::J, TC13::OperationFormat::SL,
        WIR_LabelParameter( b3 ) } } );
  b2.pushBackInstruction(
    { { TC13::OpCode::LOOPU, TC13::OperationFormat::L,
        WIR_LabelParameter( b1 ) } } );
  b3.pushBackInstruction(
    { { TC13::OpCode::JI, TC13::OperationFormat::A,
        WIR_RegisterParameter( p.A3(), WIR_Usage::use ) } } );

  TC1796_CPU_TC_065 tc065( f );
  tc065.optimize();

  ufAssert( b1.getInstructions().size() == 2 );
  auto it = b1.begin();
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::NOP );
  ++it;
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::J );

  ufAssert( b2.getInstructions().size() == 1 );
  it = b2.begin();
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::LOOPU );

  ufAssert( b3.getInstructions().size() == 1 );
  it = b3.begin();
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::JI );

  for ( WIR_BasicBlock &b : f )
    for ( WIR_Instruction &i : b )
      i.setDontOptimize( false );

  return( 0 );
}
