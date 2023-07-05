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
#include <sstream>

// Include libuseful headers
#include <libuseful/io.h>

// Include WIR headers
#include <wir/wir.h>
#include <arch/generic/mips.h>
#include <analyses/bit/wirupdownvalue.h>


using namespace std;
using namespace WIR;


int main( void )
{
  WIR_Init();

  // Test basic properties of class WIR_L4.
  ufAssert( getLevel( WIR_L4::bU ) == 0 );
  ufAssert( getLevel( WIR_L4::bL ) == 1 );
  ufAssert( getLevel( WIR_L4::bN ) == 1 );
  ufAssert( getLevel( WIR_L4::b0 ) == 2 );
  ufAssert( getLevel( WIR_L4::b1 ) == 2 );
  ufAssert( getLevel( WIR_L4::bX ) == 3 );

  for ( auto b : WIR_L4Set { WIR_L4::bL, WIR_L4::bN, WIR_L4::b0, WIR_L4::b1,
                             WIR_L4::bX } ) {
    ufAssert( WIR_L4::bU < b );
    ufAssert( b > WIR_L4::bU );
  }
  for ( auto b : WIR_L4Set { WIR_L4::b0, WIR_L4::b1, WIR_L4::bX } ) {
    ufAssert( WIR_L4::bL < b );
    ufAssert( b > WIR_L4::bL );
    ufAssert( WIR_L4::bN < b );
    ufAssert( b > WIR_L4::bN );
  }
  for ( auto b : WIR_L4Set { WIR_L4::bX } ) {
    ufAssert( WIR_L4::b0 < b );
    ufAssert( b > WIR_L4::b0 );
    ufAssert( WIR_L4::b1 < b );
    ufAssert( b > WIR_L4::b1 );
  }

  // cppcheck-suppress duplicateExpression
  ufAssert( !( WIR_L4::bU < WIR_L4::bU ) );
  // cppcheck-suppress duplicateExpression
  ufAssert( !( WIR_L4::bU > WIR_L4::bU ) );

  // cppcheck-suppress duplicateExpression
  ufAssert( !( WIR_L4::bL < WIR_L4::bL ) );
  // cppcheck-suppress duplicateExpression
  ufAssert( !( WIR_L4::bL > WIR_L4::bL ) );

  // cppcheck-suppress duplicateExpression
  ufAssert( !( WIR_L4::bN < WIR_L4::bN ) );
  // cppcheck-suppress duplicateExpression
  ufAssert( !( WIR_L4::bN > WIR_L4::bN ) );

  ufAssert( !( WIR_L4::bL < WIR_L4::bN ) );
  ufAssert( !( WIR_L4::bL > WIR_L4::bN ) );

  ufAssert( !( WIR_L4::bN < WIR_L4::bL ) );
  ufAssert( !( WIR_L4::bN > WIR_L4::bL ) );

  // cppcheck-suppress duplicateExpression
  ufAssert( !( WIR_L4::b0 < WIR_L4::b0 ) );
  // cppcheck-suppress duplicateExpression
  ufAssert( !( WIR_L4::b0 > WIR_L4::b0 ) );

  // cppcheck-suppress duplicateExpression
  ufAssert( !( WIR_L4::b1 < WIR_L4::b1 ) );
  // cppcheck-suppress duplicateExpression
  ufAssert( !( WIR_L4::b1 > WIR_L4::b1 ) );

  ufAssert( !( WIR_L4::b0 < WIR_L4::b1 ) );
  ufAssert( !( WIR_L4::b0 > WIR_L4::b1 ) );

  ufAssert( !( WIR_L4::b1 < WIR_L4::b0 ) );
  ufAssert( !( WIR_L4::b1 > WIR_L4::b0 ) );

  // cppcheck-suppress duplicateExpression
  ufAssert( !( WIR_L4::bX < WIR_L4::bX ) );
  // cppcheck-suppress duplicateExpression
  ufAssert( !( WIR_L4::bX > WIR_L4::bX ) );

  stringstream str;
  str << WIR_L4::bN << WIR_L4::bU << WIR_L4::bL << WIR_L4::bL << " "
      << WIR_L4::b0 << WIR_L4::bX << WIR_L4::b1;
  ufAssert( str.str() == string( "NULL 0X1" ) );

  // Check bit locations.
  WIR_Function f( "foo" );
  auto &r1 = f.pushBackVirtualRegister( MIPS_RegV() );
  auto &r2 = f.pushBackVirtualRegister( MIPS_RegV() );

  WIR_Operation o
    { MIPS::OpCode::LW, MIPS::OperationFormat::RIR_1,
      WIR_RegisterParameter( r1, WIR_Usage::def ),
      MIPS_Immediate16_Signed( -138 ),
      WIR_RegisterParameter( r2, WIR_Usage::use ) };

  WIR_Location l1(
    dynamic_cast<WIR_RegisterParameter &>( o.begin()->get() ), 17 );
  WIR_Location l2(
    dynamic_cast<WIR_RegisterParameter &>( o.rbegin()->get() ), 17 );
  WIR_Location l3(
    dynamic_cast<WIR_RegisterParameter &>( o.begin()->get() ), 23 );

  // l1 and l2 refer to different register parameters.
  ufAssert( !( l1 == l2 ) );
  ufAssert( l1 != l2 );

  // l1 and l3 refer to different bits of the same register parameter.
  ufAssert( !( l1 == l3 ) );
  ufAssert( l1 != l3 );

  // l2 and l3 refer to different register parameters and different bit
  // positions.
  ufAssert( !( l2 == l3 ) );
  ufAssert( l2 != l3 );

  // l1 should be equal to l1, of course.
  ufAssert( l1 == l1 );
  ufAssert( !( l1 != l1 ) );

  return( 0 );
}
