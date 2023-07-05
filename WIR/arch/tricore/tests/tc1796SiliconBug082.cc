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
#include <arch/tricore/optimizations/siliconbugs/tc1796_cpu_tc_082.h>


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
    { { TC13::OpCode::LD_A, TC13::OperationFormat::AC18ABSA,
        WIR_RegisterParameter( p.A7(), WIR_Usage::def ),
        TC_Const18_Unsigned( 42 ) } } );
  b1.pushBackInstruction(
    { { TC13::OpCode::STLCX, TC13::OperationFormat::AC10BOA,
        WIR_RegisterParameter( p.A7(), WIR_Usage::use ),
        TC_Const10_Signed( 444 ) } } );
  b1.pushBackInstruction(
    { { TC13::OpCode::LD_DA, TC13::OperationFormat::PC18ABSA,
        WIR_RegisterParameter( p.P2(), WIR_Usage::def ),
        TC_Const18_Unsigned( 23 ) } } );
  b1.pushBackInstruction(
    { { TC13::OpCode::STUCX, TC13::OperationFormat::AC10BOAPSW,
        WIR_RegisterParameter( p.A7(), WIR_Usage::use ),
        TC_Const10_Signed( 444 ),
        WIR_RegisterParameter( p.PSW_C(), WIR_Usage::use ) } } );
  b1.pushBackInstruction(
    { { TC13::OpCode::LD_B, TC13::OperationFormat::DC18ABSA,
        WIR_RegisterParameter( p.D7(), WIR_Usage::def ),
        TC_Const18_Unsigned( 42 ) } } );

  TC1796_CPU_TC_082 tc082( f );
  tc082.optimize();

  ufAssert( b1.getInstructions().size() == 7 );
  auto it = b1.begin();
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::LD_A );
  ++it;
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::STLCX );
  ++it;
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::NOP );
  ++it;
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::LD_DA );
  ++it;
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::STUCX );
  ++it;
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::NOP );
  ++it;
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::LD_B );

  for ( WIR_BasicBlock &b : f )
    for ( WIR_Instruction &i : b )
      i.setDontOptimize( false );

  return( 0 );
}
