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


using namespace std;
using namespace WIR;


int main( void )
{
  WIR_Init();

  WIR_TaskManager t;
  WIR_System s( "genericmips.sys", t );

  // Insert compilation units in DESCENDING order of IDs.
  s.pushBackCompilationUnit( WIR_CompilationUnit() );
  s.pushFrontCompilationUnit( WIR_CompilationUnit() );
  s.pushFrontCompilationUnit( WIR_CompilationUnit() );

  // Copy system.
  WIR_System s1( s );

  ufAssert(
    s.getCompilationUnits().size() == s1.getCompilationUnits().size() );

  // Check descending order of compilation unit IDs.
  WIR_id_t prev = nullid;
  for ( WIR_CompilationUnit &c : s1 ) {
    if ( prev == nullid )
      ufAssert( c.getID() > prev );
    else
      ufAssert( c.getID() < prev );
    prev = c.getID();
  }

  return( 0 );
}
