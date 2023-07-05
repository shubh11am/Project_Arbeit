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
#include <arch/arm/armv4t.h>


using namespace std;
using namespace WIR;


int main( void )
{
  WIR_Init();

  ARMv4T p;

  // The following must assert since TR1LR accepts the link register only.
  WIR_Operation o1(
    ARMv4T::OpCode::PUSH, ARMv4T::OperationFormat::TR1LR,
    WIR_RegisterParameter( p.R1(), WIR_Usage::use ),
    WIR_RegisterParameter( p.SP(), WIR_Usage::use ) );

  // Fallback for disabled failsafe mode.
  auto it = o1.end();
  --it;
  auto &regP = dynamic_cast<WIR_RegisterParameter &>( it->get() );
  ufAssertT(
    regP.getRegister().getName() == p.LR().getName(),
    "Parameters incompatible with operation format '" <<
    o1.getOperationFormat().getName() << "'." );

  return( 0 );
}
