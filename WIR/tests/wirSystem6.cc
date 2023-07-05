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

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include WIR headers
#include <wir/wir.h>
#include <arch/generic/mips.h>


using namespace std;
using namespace WIR;


int main( void )
{
  WIR_Init();

  WIR_TaskManager t;
  WIR_System s( "genericmips.sys", t );
  WIR_CompilationUnit &c = s.pushBackCompilationUnit( {} );
  WIR_Function &f = c.pushBackFunction( WIR_Function( "foo" ) );
  WIR_VirtualRegister &r1 =
    f.pushBackVirtualRegister( WIR_VirtualRegister( MIPS::RegisterType::reg ) );
  WIR_VirtualRegister &r2 =
    f.pushBackVirtualRegister( WIR_VirtualRegister( MIPS::RegisterType::reg ) );

  f.pushBackBasicBlock(
    { { { MIPS::OpCode::ADDI, MIPS::OperationFormat::RRI,
          WIR_RegisterParameter( r1, WIR_Usage::def ),
          WIR_RegisterParameter( r2, WIR_Usage::use ),
          MIPS_Immediate16_Signed( 42 ) } } } );

  s.pushBackCompilationUnit( {} );
  s.pushBackCompilationUnit( {} );
  s.pushBackCompilationUnit( {} );

  s.setDontOptimize();
  s.popBackCompilationUnit();

  return( 0 );
}