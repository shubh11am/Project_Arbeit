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
#include <analyses/bit/wirupdownvalue.h>
#include <arch/generic/mips.h>


using namespace std;
using namespace WIR;


int main( void )
{
  WIR_Init();

  MIPS p;
  WIR_Function f( "foo" );
  MIPS_RegV &x =
    static_cast<MIPS_RegV &>( f.pushBackVirtualRegister( MIPS_RegV() ) );
  WIR_Operation o1
    { MIPS::OpCode::ADDI, MIPS::OperationFormat::RRI,
      WIR_RegisterParameter( x, WIR_Usage::def ),
      WIR_RegisterParameter( p.r0(), WIR_Usage::use ),
      MIPS_Immediate16_Signed( 2 ) };
  WIR_Operation o2
    { MIPS::OpCode::J, MIPS::OperationFormat::L, WIR_LabelParameter( f ) };

  // Test default constructors.
  WIR_UpDownValue v1 { 32 };
  WIR_UpDownValue v2 { WIR_L4::bX, 23, true };
  WIR_UpDownValue v3 { MIPS_Immediate16_Signed( 26361 ) };
  WIR_UpDownValue v4
    { dynamic_cast<WIR_RegisterParameter &>( o1.begin()->get() ), false, true };

  // Test == and != operators.
  ufAssert( ( v1 != v2 ) == WIR_L4::b0 );
  ufAssert( ( v1 == v2 ) == WIR_L4::b0 );
  ufAssert( ( v2 != v3 ) == WIR_L4::b0 );
  ufAssert( ( v2 == v3 ) == WIR_L4::b0 );
  ufAssert( ( v3 != v4 ) == WIR_L4::b0 );
  ufAssert( ( v3 == v4 ) == WIR_L4::b0 );

  // Test copy constructors.
  WIR_UpDownValue c1 { v1 };
  WIR_UpDownValue c2 { v2 };
  WIR_UpDownValue c3 { v3 };
  WIR_UpDownValue c4 { v4 };

  // Test == operator.
  ufAssert( ( v1 == c1 ) == WIR_L4::bU );
  ufAssert( ( v2 == c2 ) == WIR_L4::b1 );
  ufAssert( ( v3 == c3 ) == WIR_L4::b1 );
  ufAssert( ( v4 == c4 ) == WIR_L4::b1 );

  // Test predicates.
  ufAssert( !v1.isSigned() );
  ufAssert( v1.isUnsigned() );
  ufAssert( v2.isSigned() );
  ufAssert( !v2.isUnsigned() );
  ufAssert( v3.isSigned() );
  ufAssert( !v3.isUnsigned() );
  ufAssert( !v4.isSigned() );
  ufAssert( v4.isUnsigned() );

  ufAssert( !v1.isInteger() );
  ufAssert( v2.isInteger() );
  ufAssert( v3.isInteger() );
  ufAssert( !v4.isInteger() );

  ufAssert( !v1.isBinaryInteger() );
  ufAssert( !v2.isBinaryInteger() );
  ufAssert( v3.isBinaryInteger() );
  ufAssert( !v4.isBinaryInteger() );

  ufAssert( v1.isPositive() );
  ufAssert( !v1.isNegative() );
  ufAssert( !v2.isPositive() );
  ufAssert( !v2.isNegative() );
  ufAssert( v3.isPositive() );
  ufAssert( !v3.isNegative() );
  ufAssert( v4.isPositive() );
  ufAssert( !v4.isNegative() );

  ufAssert( v1.containsBit( WIR_L4::bU ) );
  ufAssert( !v1.containsBit( WIR_L4::bL ) );
  ufAssert( !v1.containsBit( WIR_L4::bN ) );
  ufAssert( !v1.containsBit( WIR_L4::b0 ) );
  ufAssert( !v1.containsBit( WIR_L4::b1 ) );
  ufAssert( !v1.containsBit( WIR_L4::bX ) );

  ufAssert( v2.containsBit( WIR_L4::bX ) );
  ufAssert( !v2.containsBit( WIR_L4::bU ) );
  ufAssert( !v2.containsBit( WIR_L4::bL ) );
  ufAssert( !v2.containsBit( WIR_L4::bN ) );
  ufAssert( !v2.containsBit( WIR_L4::b0 ) );
  ufAssert( !v2.containsBit( WIR_L4::b1 ) );

  ufAssert( v3.containsBit( WIR_L4::b0 ) );
  ufAssert( v3.containsBit( WIR_L4::b1 ) );
  ufAssert( !v3.containsBit( WIR_L4::bU ) );
  ufAssert( !v3.containsBit( WIR_L4::bL ) );
  ufAssert( !v3.containsBit( WIR_L4::bN ) );
  ufAssert( !v3.containsBit( WIR_L4::bX ) );

  ufAssert( v4.containsBit( WIR_L4::bN ) );
  ufAssert( !v4.containsBit( WIR_L4::bU ) );
  ufAssert( !v4.containsBit( WIR_L4::bL ) );
  ufAssert( !v4.containsBit( WIR_L4::b0 ) );
  ufAssert( !v4.containsBit( WIR_L4::b1 ) );
  ufAssert( !v4.containsBit( WIR_L4::bX ) );

  ufAssert( v1.containsBits( { WIR_L4::bU } ) );
  ufAssert(
    !v1.containsBits(
      { WIR_L4::bL, WIR_L4::bN, WIR_L4::b0, WIR_L4::b1, WIR_L4::bX } ) );

  ufAssert( v2.containsBits( { WIR_L4::bX } ) );
  ufAssert(
    !v2.containsBits(
      { WIR_L4::bU, WIR_L4::bL, WIR_L4::bN, WIR_L4::b0, WIR_L4::b1 } ) );

  ufAssert( v3.containsBits( { WIR_L4::b0, WIR_L4::b1 } ) );
  ufAssert( !v3.containsBits( { WIR_L4::bU, WIR_L4::bL, WIR_L4::bN } ) );

  ufAssert( v4.containsBits( { WIR_L4::bN } ) );
  ufAssert(
    !v4.containsBits(
      { WIR_L4::bU, WIR_L4::bL, WIR_L4::b0, WIR_L4::b1, WIR_L4::bX } ) );

  ufAssert( v1.containsOnlyBit( WIR_L4::bU ) );
  ufAssert( !v1.containsOnlyBit( WIR_L4::bL ) );
  ufAssert( !v1.containsOnlyBit( WIR_L4::bN ) );
  ufAssert( !v1.containsOnlyBit( WIR_L4::b0 ) );
  ufAssert( !v1.containsOnlyBit( WIR_L4::b1 ) );
  ufAssert( !v1.containsOnlyBit( WIR_L4::bX ) );

  ufAssert( v2.containsOnlyBit( WIR_L4::bX ) );
  ufAssert( !v2.containsOnlyBit( WIR_L4::bU ) );
  ufAssert( !v2.containsOnlyBit( WIR_L4::bL ) );
  ufAssert( !v2.containsOnlyBit( WIR_L4::bN ) );
  ufAssert( !v2.containsOnlyBit( WIR_L4::b0 ) );
  ufAssert( !v2.containsOnlyBit( WIR_L4::b1 ) );

  ufAssert( !v3.containsOnlyBit( WIR_L4::bU ) );
  ufAssert( !v3.containsOnlyBit( WIR_L4::bL ) );
  ufAssert( !v3.containsOnlyBit( WIR_L4::bN ) );
  ufAssert( !v3.containsOnlyBit( WIR_L4::b0 ) );
  ufAssert( !v3.containsOnlyBit( WIR_L4::b1 ) );
  ufAssert( !v3.containsOnlyBit( WIR_L4::bX ) );

  ufAssert( v4.containsOnlyBit( WIR_L4::bN ) );
  ufAssert( !v4.containsOnlyBit( WIR_L4::bU ) );
  ufAssert( !v4.containsOnlyBit( WIR_L4::bL ) );
  ufAssert( !v4.containsOnlyBit( WIR_L4::b0 ) );
  ufAssert( !v4.containsOnlyBit( WIR_L4::b1 ) );
  ufAssert( !v4.containsOnlyBit( WIR_L4::bX ) );

  ufAssert( v1.containsOnlyBits( { WIR_L4::bU } ) );
  ufAssert( v1.containsOnlyBits( { WIR_L4::bU, WIR_L4::bX } ) );
  ufAssert( !v1.containsOnlyBits( { WIR_L4::b0, WIR_L4::bX } ) );

  ufAssert( v3.containsOnlyBits( { WIR_L4::b0, WIR_L4::b1 } ) );
  ufAssert(
    !v3.containsOnlyBits(
      { WIR_L4::bU, WIR_L4::bL, WIR_L4::bN, WIR_L4::bX } ) );

  ufAssert( v4.isLocationBit( 7 ) );
  ufAssert( !v1.isLocationBit( 7 ) );

  ufAssert( v1.getBitWidth() == 32 );
  ufAssert( v2.getBitWidth() == 23 );
  ufAssert( v3.getBitWidth() == 16 );
  ufAssert( v4.getBitWidth() == 32 );

  ufAssert( v3.getSignedValue() == 26361 );
  WIR_UpDownValue v6 { MIPS_Immediate16_Unsigned( 26361 ) };
  ufAssert( v6.getSignedValue() == 26361 );
  WIR_UpDownValue v7 { MIPS_Immediate16_Signed( -26361 ) };
  ufAssert( v7.getSignedValue() == -26361 );

  ufAssert( v4.getLocation( 13 ).getRegisterParameter() == o1.begin()->get() );
  ufAssert( v4.getLocation( 27 ).getBitPosition() == 27 );

  ufAssert( v1[ 7 ] == WIR_L4::bU );
  ufAssert( v2[ 21 ] == WIR_L4::bX );
  ufAssert( v3[ 10 ] == WIR_L4::b1 );
  ufAssert( v3[ 11 ] == WIR_L4::b0 );
  ufAssert( v4[ 13 ] == WIR_L4::bN );

  v3.setBit( 0, WIR_L4::b0 );
  ufAssert( v3.getSignedValue() == 26360 );
  v2.setBit( 15, WIR_L4::b1 );
  ufAssert( !v2.containsOnlyBit( WIR_L4::bX ) );
  v7.setBit( 5, WIR_L4::b1 );
  ufAssert( v7.getSignedValue() == -26329 );
  v7.setBit( 15, WIR_L4::b0 );
  ufAssert( v7.getSignedValue() == 6439 );

  v4.setBit(
    13, WIR_L4::bL,
    { dynamic_cast<WIR_RegisterParameter &>( o1.begin()->get() ), 9 } );
  ufAssert( v4[ 13 ] == WIR_L4::bL );
  ufAssert( v4.getLocation( 13 ).getBitPosition() == 9 );

  v4.setAllBits(
    WIR_L4::bN,
    { dynamic_cast<WIR_RegisterParameter &>( o1.begin()->get() ), 27 } );
  ufAssert( v4[ 13 ] == WIR_L4::bN );
  ufAssert( v4.getLocation( 13 ).getBitPosition() == 27 );
  v4.setAllBits( WIR_L4::b1 );
  ufAssert( !v4.isLocationBit( 7 ) );
  ufAssert( v4.getSignedValue() == 4294967295 );

  return( 0 );
}
