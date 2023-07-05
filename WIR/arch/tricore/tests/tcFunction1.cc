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
#include <arch/generic/mips.h>


using namespace std;
using namespace WIR;


int main( void )
{
  WIR_Init();

  // Check precolors.

  MIPS m;
  TC131 t;
  WIR_Function f( "main" );
  auto &tr1 = f.pushBackVirtualRegister( MIPS_RegV() );
  f.pushBackVirtualRegister( MIPS_RegV() );

  MIPS_RegV &r1 = static_cast<MIPS_RegV &>( tr1 );

  // This must assert since a MIPS vreg shall be precolored with a TriCore
  // PHREG.
  f.insertPrecolor( r1, t.D7() );

  // Fallback for disabled failsafe mode.
  ufAssert( f.containsVirtualRegister( r1.getRoot() ) );
  ufAssert( r1.getType() == t.D7().getType() );

  return( 0 );
}
