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

  ARM_RegV r1;
  ARM_HiRegV r2;

  // The following must assert since THUMB operations only accept low registers.
  WIR_Operation o1(
    ARMv4T::OpCode::MOV, ARMv4T::OperationFormat::TRR_1,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::use ) );

  // Fallback for disabled failsafe mode.
  auto it = o1.end();
  --it;
  auto &regP = dynamic_cast<WIR_RegisterParameter &>( it->get() );
  ufAssertT(
    regP.getRegister().getType().isCompatible(
      WIR_RegisterParameter(
        ARM_LoRegV(), WIR_Usage::use ).getRegister().getType(),
      regP.getRegister(), ARM_LoRegV() ) ||
      ( regP.getUsage() != WIR_Usage::use ),
    "Parameters incompatible with operation format '" <<
    o1.getOperationFormat().getName() << "'." );

  return( 0 );
}
