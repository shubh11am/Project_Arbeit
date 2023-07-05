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

// Include standard headers
#include <set>

// Include libuseful headers
#include <libuseful/io.h>

// Include WIR headers
#include <wir/wir.h>
#include <analyses/bit/wirupdownvalue.h>


using namespace std;
using namespace WIR;


int main( void )
{
  WIR_Init();

  // Test basic properties of WIR_L4 operator ~.
  ufAssert( ~WIR_L4::bU == WIR_L4::bU );
  ufAssert( ~WIR_L4::bL == WIR_L4::bN );
  ufAssert( ~WIR_L4::bN == WIR_L4::bL );
  ufAssert( ~WIR_L4::b0 == WIR_L4::b1 );
  ufAssert( ~WIR_L4::b1 == WIR_L4::b0 );
  ufAssert( ~WIR_L4::bX == WIR_L4::bX );

  return( 0 );
}
