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

  // Check name handling of WIR systems.
  WIR_TaskManager t;
  WIR_System sys1( "genericmips.sys", t );
  WIR_System sys2( "genericmips.sys", t );

  string name = "Dummy WIR system";
  sys1.setName( name );
  sys2.setName( "Test WIR system" );

  ufAssert( sys1.getName() == "Dummy WIR system" );
  ufAssert( sys2.getName() == "Test WIR system" );

  // Check copy constructors of WIR systems.
  WIR_System sys3( sys1 );
  ufAssert( sys3.getName() == "Dummy WIR system" );
  WIR_System sys4( move( sys2 ) );
  ufAssert( sys2.getName() == "" );
  ufAssert( sys4.getName() == "Test WIR system" );

  // Check assignment operators of WIR systems.
  WIR_System sys5( "genericmips.sys", t );
  sys5 = sys3;
  ufAssert( sys5.getName() == "Dummy WIR system" );
  WIR_System sys6( "genericmips.sys", t );
  sys6 = move( sys4 );
  ufAssert( sys4.getName() == "" );
  ufAssert( sys6.getName() == "Test WIR system" );

  return( 0 );
}
