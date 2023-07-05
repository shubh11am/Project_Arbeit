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

  MIPS p;
  WIR_Function f( "foo" );
  MIPS_RegV &x =
    static_cast<MIPS_RegV &>( f.pushBackVirtualRegister( MIPS_RegV() ) );
  WIR_Operation o1
    { MIPS::OpCode::ADDI, MIPS::OperationFormat::RRI,
      WIR_RegisterParameter( x, WIR_Usage::def ),
      WIR_RegisterParameter( p.r0(), WIR_Usage::use ),
      MIPS_Immediate16_Signed( 2 ) };
  auto &rp1 = dynamic_cast<WIR_RegisterParameter &>( o1.begin()->get() );
  WIR_Operation o2
    { MIPS::OpCode::ADDI, MIPS::OperationFormat::RRI,
      WIR_RegisterParameter( x, WIR_Usage::def ),
      WIR_RegisterParameter( p.r0(), WIR_Usage::use ),
      MIPS_Immediate16_Signed( 2 ) };
  auto &rp2 = dynamic_cast<WIR_RegisterParameter &>( o2.begin()->get() );

  WIR_UpDownValue v1 { 11 };
  v1.setBit( 0, WIR_L4::bL, { rp1, 9 } );
  v1.setBit( 1, WIR_L4::bL, { rp1, 9 } );
  v1.setBit( 2, WIR_L4::bL, { rp1, 9 } );
  v1.setBit( 3, WIR_L4::bL, { rp1, 9 } );
  v1.setBit( 4, WIR_L4::bN, { rp1, 9 } );
  v1.setBit( 5, WIR_L4::bL, { rp1, 9 } );
  v1.setBit( 6, WIR_L4::bL, { rp1, 9 } );
  v1.setBit( 7, WIR_L4::bN, { rp1, 9 } );
  v1.setBit( 8, WIR_L4::bL, { rp1, 9 } );
  v1.setBit( 9, WIR_L4::bL, { rp1, 9 } );
  v1.setBit( 10, WIR_L4::bN, { rp1, 9 } );

  WIR_UpDownValue v2 { 11 };
  v2.setBit( 0, WIR_L4::b1 );
  v2.setBit( 1, WIR_L4::b0 );
  v2.setBit( 2, WIR_L4::bN, { rp1, 9 } );
  v2.setBit( 3, WIR_L4::bL, { rp1, 9 } );
  v2.setBit( 4, WIR_L4::bN, { rp1, 9 } );
  v2.setBit( 5, WIR_L4::bN, { rp2, 9 } );
  v2.setBit( 6, WIR_L4::bL, { rp2, 9 } );
  v2.setBit( 7, WIR_L4::bN, { rp2, 9 } );
  v2.setBit( 8, WIR_L4::bN, { rp1, 13 } );
  v2.setBit( 9, WIR_L4::bL, { rp1, 13 } );
  v2.setBit( 10, WIR_L4::bN, { rp1, 13 } );

  // Test bitwise operators.
  auto r1 = v1 & v2;
  ufAssert(
    ( r1[ 0 ] == WIR_L4::bL ) &&
    ( r1.getLocation( 0 ) == WIR_Location( rp1, 9 ) ) );
  ufAssert( r1[ 1 ] == WIR_L4::b0 );
  ufAssert( r1[ 2 ] == WIR_L4::b0 );
  ufAssert(
    ( r1[ 3 ] == WIR_L4::bL ) &&
    ( r1.getLocation( 3 ) == WIR_Location( rp1, 9 ) ) );
  ufAssert(
    ( r1[ 4 ] == WIR_L4::bN ) &&
    ( r1.getLocation( 4 ) == WIR_Location( rp1, 9 ) ) );
  ufAssert( r1[ 5 ] == WIR_L4::bU );
  ufAssert( r1[ 6 ] == WIR_L4::bU );
  ufAssert( r1[ 7 ] == WIR_L4::bU );
  ufAssert( r1[ 8 ] == WIR_L4::bU );
  ufAssert( r1[ 9 ] == WIR_L4::bU );
  ufAssert( r1[ 10 ] == WIR_L4::bU );

  auto r2 = v1 | v2;
  ufAssert( r2[ 0 ] == WIR_L4::b1 );
  ufAssert(
    ( r2[ 1 ] == WIR_L4::bL ) &&
    ( r2.getLocation( 1 ) == WIR_Location( rp1, 9 ) ) );
  ufAssert( r2[ 2 ] == WIR_L4::b1 );
  ufAssert(
    ( r2[ 3 ] == WIR_L4::bL ) &&
    ( r2.getLocation( 3 ) == WIR_Location( rp1, 9 ) ) );
  ufAssert(
    ( r2[ 4 ] == WIR_L4::bN ) &&
    ( r2.getLocation( 4 ) == WIR_Location( rp1, 9 ) ) );
  ufAssert( r2[ 5 ] == WIR_L4::bU );
  ufAssert( r2[ 6 ] == WIR_L4::bU );
  ufAssert( r2[ 7 ] == WIR_L4::bU );
  ufAssert( r2[ 8 ] == WIR_L4::bU );
  ufAssert( r2[ 9 ] == WIR_L4::bU );
  ufAssert( r2[ 10 ] == WIR_L4::bU );

  auto r3 = v1 ^ v2;
  ufAssert(
    ( r3[ 0 ] == WIR_L4::bN ) &&
    ( r3.getLocation( 0 ) == WIR_Location( rp1, 9 ) ) );
  ufAssert(
    ( r3[ 1 ] == WIR_L4::bL ) &&
    ( r3.getLocation( 1 ) == WIR_Location( rp1, 9 ) ) );
  ufAssert( r3[ 2 ] == WIR_L4::b1 );
  ufAssert( r3[ 3 ] == WIR_L4::b0 );
  ufAssert( r3[ 4 ] == WIR_L4::b0 );
  ufAssert( r3[ 5 ] == WIR_L4::bU );
  ufAssert( r3[ 6 ] == WIR_L4::bU );
  ufAssert( r3[ 7 ] == WIR_L4::bU );
  ufAssert( r3[ 8 ] == WIR_L4::bU );
  ufAssert( r3[ 9 ] == WIR_L4::bU );
  ufAssert( r3[ 10 ] == WIR_L4::bU );

  auto r4 = ~v2;
  ufAssert( r4[ 0 ] == WIR_L4::b0 );
  ufAssert( r4[ 1 ] == WIR_L4::b1 );
  ufAssert(
    ( r4[ 2 ] == WIR_L4::bL ) &&
    ( r4.getLocation( 2 ) == WIR_Location( rp1, 9 ) ) );
  ufAssert(
    ( r4[ 3 ] == WIR_L4::bN ) &&
    ( r4.getLocation( 3 ) == WIR_Location( rp1, 9 ) ) );
  ufAssert(
    ( r4[ 4 ] == WIR_L4::bL ) &&
    ( r4.getLocation( 4 ) == WIR_Location( rp1, 9 ) ) );
  ufAssert(
    ( r4[ 5 ] == WIR_L4::bL ) &&
    ( r4.getLocation( 5 ) == WIR_Location( rp2, 9 ) ) );
  ufAssert(
    ( r4[ 6 ] == WIR_L4::bN ) &&
    ( r4.getLocation( 6 ) == WIR_Location( rp2, 9 ) ) );
  ufAssert(
    ( r4[ 7 ] == WIR_L4::bL ) &&
    ( r4.getLocation( 7 ) == WIR_Location( rp2, 9 ) ) );
  ufAssert(
    ( r4[ 8 ] == WIR_L4::bL ) &&
    ( r4.getLocation( 8 ) == WIR_Location( rp1, 13 ) ) );
  ufAssert(
    ( r4[ 9 ] == WIR_L4::bN ) &&
    ( r4.getLocation( 9 ) == WIR_Location( rp1, 13 ) ) );
  ufAssert(
    ( r4[ 10 ] == WIR_L4::bL ) &&
    ( r4.getLocation( 10 ) == WIR_Location( rp1, 13 ) ) );

  // Test << operator.
  // Shift amounts including U, L or N must result in U*.
  WIR_UpDownValue v3 { MIPS_Immediate16_Signed( 26361 ) };

  WIR_UpDownValue s1 { WIR_L4::bU, 1, true };
  WIR_UpDownValue s2 { s1 };
  s2.setBit(
    0, WIR_L4::bL,
    { dynamic_cast<WIR_RegisterParameter &>( o1.begin()->get() ), 9 } );
  WIR_UpDownValue s3 { 3 };
  s3 = s1;
  s3.setBit(
    0, WIR_L4::bN,
    { dynamic_cast<WIR_RegisterParameter &>( o2.begin()->get() ), 9 } );

  WIR_UpDownValue r5 { 5 };
  r5 = v3 << s1;
  ufAssert( r5.getBitWidth() == 16 );
  ufAssert( r5.containsOnlyBit( WIR_L4::bU ) );

  r5 = v3;
  ufAssert( r5.isBinaryInteger() );

  r5 = v3 << s2;
  ufAssert( r5.containsOnlyBit( WIR_L4::bU ) );

  r5 = v3;
  ufAssert( r5.isBinaryInteger() );

  r5 = v3 << s3;
  ufAssert( r5.containsOnlyBit( WIR_L4::bU ) );

  // Shifting by 0 changes nothing.
  WIR_UpDownValue s4 { WIR_L4::b0, 4, true };
  r5 = v3 << s4;
  ufAssert( ( r5 == v3 ) == WIR_L4::b1 );

  // Shifting by large values results in 0 (everything's shifted out).
  WIR_UpDownValue s5 { MIPS_Immediate16_Signed( 64 ) };
  r5 = v3 << s5;
  ufAssert( r5.getSignedValue() == 0 );

  r5 = v3;
  ufAssert( r5.getSignedValue() == 26361 );

  WIR_UpDownValue s6 { MIPS_Immediate16_Signed( -64 ) };
  r5 = v3 << s6;
  ufAssert( r5.getSignedValue() == 0 );

  // Left-shift by some binary integer value.
  WIR_UpDownValue s7 { MIPS_Immediate16_Signed( 5 ) };
  r5 = v3 << s7;
  ufAssert( r5.getSignedValue() == -8416 );

  // Right-shift by some binary integer value.
  WIR_UpDownValue s8 { MIPS_Immediate16_Signed( -5 ) };
  r5 = v3 << s8;
  ufAssert( r5.getSignedValue() == 823 );

  // Left-shift by some integer value containing X.
  WIR_UpDownValue s9 { MIPS_Immediate16_Signed( 5 ) };
  s9.setBit( 1, WIR_L4::bX );
  r5 = v3 << s9;
  ufAssert( r5.getSignedValue() == 31872 );

  // Right-shift by some integer value containing X.
  WIR_UpDownValue s10 { MIPS_Immediate16_Signed( -5 ) };
  s10.setBit( 3, WIR_L4::bX );
  r5 = v3 << s10;
  ufAssert( r5.getSignedValue() == 3 );

  // Right-shift a negative value to test proper sign extension.
  WIR_UpDownValue v4 { MIPS_Immediate16_Signed( -4711 ) };
  r5 = v4 << s10;
  ufAssert( r5.getSignedValue() == -1 );

  // Test >> operator.
  // Shift amounts including U, L or N must result in U*.
  r5 = v3 >> s1;
  ufAssert( r5.getBitWidth() == 16 );
  ufAssert( r5.containsOnlyBit( WIR_L4::bU ) );

  r5 = v3;
  ufAssert( r5.isBinaryInteger() );

  r5 = v3 >> s2;
  ufAssert( r5.containsOnlyBit( WIR_L4::bU ) );

  r5 = v3;
  ufAssert( r5.isBinaryInteger() );

  r5 = v3 >> s3;
  ufAssert( r5.containsOnlyBit( WIR_L4::bU ) );

  // Shifting by 0 changes nothing.
  r5 = v3 >> s4;
  ufAssert( ( r5 == v3 ) == WIR_L4::b1 );

  // Shifting by large values results in 0 (everything's shifted out).
  r5 = v3 >> s5;
  ufAssert( r5.getSignedValue() == 0 );

  r5 = v3;
  ufAssert( r5.getSignedValue() == 26361 );

  r5 = v3 >> s6;
  ufAssert( r5.getSignedValue() == 0 );

  // Right-shift by some binary integer value.
  r5 = v3 >> s7;
  ufAssert( r5.getSignedValue() == 823 );

  // Left-shift by some binary integer value.
  r5 = v3 >> s8;
  ufAssert( r5.getSignedValue() == -8416 );

  // Right-shift by some integer value containing X.
  r5 = v3 >> s9;
  ufAssert( r5.getSignedValue() == 205 );

  // Left-shift by some integer value containing X.
  r5 = v3 >> s10;
  ufAssert( r5.getSignedValue() == 8192 );

  // Right-shift a negative value to test proper sign extension.
  r5 = v4 >> s9;
  ufAssert( r5.getSignedValue() == -37 );

  return( 0 );
}
