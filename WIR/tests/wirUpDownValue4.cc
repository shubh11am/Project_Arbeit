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
#include <sstream>

// Include libuseful headers
#include <libuseful/io.h>

// Include WIR headers
#include <wir/wir.h>
#include <analyses/bit/wirupdownvalue.h>
#include <arch/generic/mips.h>


using namespace std;
using namespace WIR;


int main( void )
{
  WIR_Init();

  // Test arithmetic operators.

  // Test 0-extension for unsigned values.
  WIR_UpDownValue e1 { MIPS_Immediate5_Shamt( 21 ) };
  auto e2 = e1.extend( 13 );
  ufAssert( e2.getBitWidth() == 13 );
  ufAssert( e2.getSignedValue() == 21 );

  // Test 0-extension for signed value.
  WIR_UpDownValue e3 { MIPS_Immediate16_Signed( 26361 ) };
  e2 = e3.extend( 23 );
  ufAssert( e2.getBitWidth() == 23 );
  ufAssert( e2.getSignedValue() == 26361 );

  // Test 1-extension for signed value.
  WIR_UpDownValue e4 { MIPS_Immediate16_Signed( -26361 ) };
  e2 = e4.extend( 27 );
  ufAssert( e2.getBitWidth() == 27 );
  ufAssert( e2.getSignedValue() == -26361 );

  // Test binary + operator.
  e2 = e1 + e1;
  ufAssert( e2.isUnsigned() && ( e2.getBitWidth() == 5 ) );
  ufAssert( e2.getSignedValue() == 10 );                    // Overflow!

  e2 = e1.extend( 16 ) + e3;
  ufAssert( e2.isSigned() && ( e2.getBitWidth() == 16 ) );
  ufAssert( e2.getSignedValue() == 26382 );

  e2 = e4 + e1.extend( 16 );
  ufAssert( e2.isSigned() && ( e2.getBitWidth() == 16 ) );
  ufAssert( e2.getSignedValue() == -26340 );

  e2 = e3 + e3;
  ufAssert( e2.isSigned() && ( e2.getBitWidth() == 16 ) );
  ufAssert( e2.getSignedValue() == -12814 );              // Overflow!

  e2 = e4 + e4;
  ufAssert( e2.isSigned() && ( e2.getBitWidth() == 16 ) );
  ufAssert( e2.getSignedValue() == 12814 );               // Underflow!

  // Test unary - operator.
  e2 = -e4;
  ufAssert( e2.getSignedValue() == 26361 );
  e2 = -e3;
  ufAssert( e2.getSignedValue() == -26361 );

  // Test binary - operator.
  e2 = e1 - e1;
  ufAssert( e2.isSigned() && ( e2.getBitWidth() == 5 ) );
  ufAssert( e2.getSignedValue() == 0 );

  e2 = e1.extend( 16 ) - e3;
  ufAssert( e2.isSigned() && ( e2.getBitWidth() == 16 ) );
  ufAssert( e2.getSignedValue() == -26340 );

  e2 = e4 - e1.extend( 16 );
  ufAssert( e2.isSigned() && ( e2.getBitWidth() == 16 ) );
  ufAssert( e2.getSignedValue() == -26382 );

  e2 = e3 - e4;
  ufAssert( e2.isSigned() && ( e2.getBitWidth() == 16 ) );
  ufAssert( e2.getSignedValue() == -12814 );              // Overflow!

  e2 = e4 - e3;
  ufAssert( e2.isSigned() && ( e2.getBitWidth() == 16 ) );
  ufAssert( e2.getSignedValue() == 12814 );               // Underflow!

  e2 = e4 - -e3;
  ufAssert( e2.isSigned() && ( e2.getBitWidth() == 16 ) );
  ufAssert( e2.getSignedValue() == 0 );

  // Test *, / and % operators that all result in U*.
  e2 = e1;
  ufAssert( e2.isUnsigned() && ( e2.getBitWidth() == 5 ) );

  e2 = e1.extend( 16 ) * e4;
  ufAssert( e2.isSigned() && ( e2.getBitWidth() == 16 ) );
  ufAssert( e2.containsOnlyBit( WIR_L4::bU ) );

  e2 = e1;
  ufAssert( e2.isUnsigned() && ( e2.getBitWidth() == 5 ) );

  e2 = e4 / e1.extend( 16 );
  ufAssert( e2.isSigned() && ( e2.getBitWidth() == 16 ) );
  ufAssert( e2.containsOnlyBit( WIR_L4::bU ) );

  e2 = e1;
  ufAssert( e2.isUnsigned() && ( e2.getBitWidth() == 5 ) );

  e2 = e4 % e1.extend( 16 );
  ufAssert( e2.isSigned() && ( e2.getBitWidth() == 16 ) );
  ufAssert( e2.containsOnlyBit( WIR_L4::bU ) );

  return( 0 );
}
