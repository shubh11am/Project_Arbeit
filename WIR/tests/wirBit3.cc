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

  // Complete test of WIR_L4 operator &.

  // 0 AND b = b AND 0 = 0.
  for ( auto b : WIR_L4Set { WIR_L4::bU, WIR_L4::bL, WIR_L4::bN, WIR_L4::b0,
                             WIR_L4::b1, WIR_L4::bX } ) {
    ufAssert( ( WIR_L4::b0 & b ) == WIR_L4::b0 );
    ufAssert( ( b & WIR_L4::b0 ) == WIR_L4::b0 );
  }

  // 1 AND b = b AND 1 = b.
  for ( auto b : WIR_L4Set { WIR_L4::bU, WIR_L4::bL, WIR_L4::bN, WIR_L4::b0,
                             WIR_L4::b1, WIR_L4::bX } ) {
    ufAssert( ( WIR_L4::b1 & b ) == b );
    ufAssert( ( b & WIR_L4::b1 ) == b );
  }

  // b AND b = b.
  for ( auto b : WIR_L4Set { WIR_L4::bU, WIR_L4::bL, WIR_L4::bN, WIR_L4::b0,
                             WIR_L4::b1, WIR_L4::bX } )
    ufAssert( ( b & b ) == b );

  // L AND N = N AND L = 0.
  ufAssert( ( WIR_L4::bL & WIR_L4::bN ) == WIR_L4::b0 );
  ufAssert( ( WIR_L4::bN & WIR_L4::bL ) == WIR_L4::b0 );

  // U AND b = b AND U = U (almost always, except for b = X and b = 0).
  for ( auto b : WIR_L4Set { WIR_L4::bU, WIR_L4::bL, WIR_L4::bN,
                             WIR_L4::b1 } ) {
    ufAssert( ( WIR_L4::bU & b ) == WIR_L4::bU );
    ufAssert( ( b & WIR_L4::bU ) == WIR_L4::bU );
  }

  // Special case: X AND b = b AND X = 0 (for b = L, N or U).
  for ( auto b : WIR_L4Set { WIR_L4::bU, WIR_L4::bL, WIR_L4::bN } ) {
    ufAssert( ( WIR_L4::bX & b ) == WIR_L4::b0 );
    ufAssert( ( b & WIR_L4::bX ) == WIR_L4::b0 );
  }

  return( 0 );
}
