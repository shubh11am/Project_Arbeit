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
#include <arch/generic/mips.h>


using namespace std;
using namespace WIR;


int main( void )
{
  WIR_Init();

  // Check invalid operation format/opcode constellations.

  WIR_Function f( "main" );
  WIR_VirtualRegister &r1 =
    f.pushBackVirtualRegister( WIR_VirtualRegister( MIPS::RegisterType::reg ) );
  WIR_VirtualRegister &r2 =
    f.pushBackVirtualRegister( WIR_VirtualRegister( MIPS::RegisterType::reg ) );

  // Assertion (failsafe mode only): RIR_1 is not associated with SW.
  WIR_Operation o1(
    MIPS::OpCode::SW, MIPS::OperationFormat::RIR_1,
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    MIPS_Immediate16_Signed( 112 ),
    WIR_RegisterParameter( r2, WIR_Usage::use ) );

  // Fallback for disabled failsafe mode.
  ufAssertT(
    WIR_Registry::isLegalOperationFormat(
      MIPS::OpCode::SW, MIPS::OperationFormat::RIR_1 ),
    "Instruction format '" << MIPS::OperationFormat::RIR_1.getName() <<
    "' incompatible with opcode '" << MIPS::OpCode::SW.getName() << "'." );

  return( 0 );
}
