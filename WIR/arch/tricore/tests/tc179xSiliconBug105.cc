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
#include <arch/tricore/optimizations/siliconbugs/tc179x_cpu_tc_105.h>


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
    { { TC13::OpCode::ST_W, TC13::OperationFormat::AC10DBOA_1,
        WIR_RegisterParameter( p.A7(), WIR_Usage::use ),
        TC_Const10_Signed( -42 ),
        WIR_RegisterParameter( p.D13(), WIR_Usage::use ) } } );
  b1.pushBackInstruction(
    { { TC13::OpCode::MTCR, TC13::OperationFormat::C16DPSW,
        TC_Const16_Unsigned( 0xFE04 ),
        WIR_RegisterParameter( p.D9(), WIR_Usage::use ),
        WIR_RegisterParameter( p.PSW_C(), WIR_Usage::def ) } } );

  TC179x_CPU_TC_105 tc105( f );
  tc105.optimize();

  ufAssert( b1.getInstructions().size() == 3 );
  auto it = b1.begin();
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::ST_W );
  ++it;
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::DSYNC );
  ++it;
  ufAssert( it->get().begin()->get().getOpCode() == TC13::OpCode::MTCR );

  for ( WIR_BasicBlock &b : f )
    for ( WIR_Instruction &i : b )
      i.setDontOptimize( false );

  return( 0 );
}
