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

  WIR_CompilationUnit c;

  // Insert functions in DESCENDING order of IDs.
  c.pushBackFunction( WIR_Function( "f1" ) );
  c.pushFrontFunction( WIR_Function( "f2" ) );
  c.pushFrontFunction( WIR_Function( "f3" ) );

  // Copy compilation unit. The functions in c1 should also appear in DESCENDING
  // ID order.
  WIR_CompilationUnit c1( c );

  ufAssert( c.getFunctions().size() == c1.getFunctions().size() );

  // Check descending order of function IDs.
  WIR_id_t prev = nullid;
  for ( WIR_Function &f : c1 ) {
    if ( prev == nullid )
      ufAssert( f.getID() > prev );
    else
      ufAssert( f.getID() < prev );
    prev = f.getID();
  }

  return( 0 );
}
