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

  WIR_StringParameter p1( "foo" );
  string s { "bar" };
  WIR_StringParameter p6( s );

  // Check properties of the created parameters.
  ufAssert( p1.getType() == WIR_ParameterType::str );
  ufAssert( p6.getType() == WIR_ParameterType::str );

  ufAssert( p1.getString() == "foo" );
  ufAssert( p6.getString() == "bar" );

  p1.setString( "bar" );
  string name = p1.getString();
  ufAssert( p1.getString() == "bar" );

  s = "foo";
  p6.setString( s );
  ufAssert( p6.getString() == "foo" );

  // Test the copy constructors.
  WIR_StringParameter p2( p1 );

  ufAssert( p2.getType() == WIR_ParameterType::str );

  ufAssert( p2.getString() == name );

  // Test the move constructors.
  WIR_StringParameter p3( move( p2 ) );

  ufAssert( p3.getType() == WIR_ParameterType::str );

  ufAssert( p3.getString() == name );
  ufAssert( p2.getString() == "" );

  // Test the copy assignment operator.
  WIR_StringParameter p4( "bla" );
  p4 = p1;

  ufAssert( p4.getType() == WIR_ParameterType::str );

  ufAssert( p4.getString() == name );

  // Test the move assignment operator.
  WIR_StringParameter p5( "xyz" );
  p5 = move( p3 );

  ufAssert( p5.getType() == WIR_ParameterType::str );

  ufAssert( p5.getString() == name );
  ufAssert( p3.getString() == "" );

  return( 0 );
}
