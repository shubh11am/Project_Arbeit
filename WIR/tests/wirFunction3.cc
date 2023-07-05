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

  // Check precolors.

  MIPS m;
  WIR_Function f( "main" );

  MIPS_RegV r3;

  // This must assert since r3 is not inserted into f.
  f.insertPrecolor( r3, m.r4() );

  // Fallback for disabled failsafe mode.
  ufAssert( f.containsVirtualRegister( r3.getRoot() ) );
  ufAssert( r3.getType() == m.r4().getType() );

  return( 0 );
}
