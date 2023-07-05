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

  WIR_BasicBlock b;
  WIR_Data d( "input" );
  WIR_Function f( "foo" );

  WIR_LabelParameter p1( b );
  WIR_LabelParameter p2( d );
  WIR_LabelParameter p3( f );

  // Check properties of the created parameters.
  ufAssert( p1.getType() == WIR_ParameterType::label );
  ufAssert( p1.getType() == p2.getType() );
  ufAssert( p1.getType() == p3.getType() );

  ufAssert( p2.getName() == "input" );
  ufAssert( p3.getName() == "foo" );

  f.setName( "bar" );
  string name = p1.getName();

  ufAssert( p3.getName() == "bar" );

  ufAssert( p1.getLabelType() == WIR_SymbolType::block );
  ufAssert( p2.getLabelType() == WIR_SymbolType::data );
  ufAssert( p3.getLabelType() == WIR_SymbolType::function );

  ufAssert( p1.getBasicBlock().getID() == b.getID() );
  ufAssert( p2.getData().getID() == d.getID() );
  ufAssert( p3.getFunction().getID() == f.getID() );

  // Test the copy constructors.
  WIR_LabelParameter p4( p1 );
  WIR_LabelParameter p5( p2 );
  WIR_LabelParameter p6( p3 );

  ufAssert( p4.getType() == WIR_ParameterType::label );
  ufAssert( p4.getType() == p5.getType() );
  ufAssert( p4.getType() == p6.getType() );

  ufAssert( p4.getName() == name );
  ufAssert( p5.getName() == "input" );
  ufAssert( p6.getName() == "bar" );

  ufAssert( p4.getLabelType() == WIR_SymbolType::block );
  ufAssert( p5.getLabelType() == WIR_SymbolType::data );
  ufAssert( p6.getLabelType() == WIR_SymbolType::function );

  ufAssert( p4.getBasicBlock().getID() == b.getID() );
  ufAssert( p5.getData().getID() == d.getID() );
  ufAssert( p6.getFunction().getID() == f.getID() );

  // Test the move constructors.
  WIR_LabelParameter p7( move( p4 ) );
  WIR_LabelParameter p8( move( p5 ) );
  WIR_LabelParameter p9( move( p6 ) );

  ufAssert( p7.getType() == WIR_ParameterType::label );
  ufAssert( p7.getType() == p8.getType() );
  ufAssert( p7.getType() == p9.getType() );

  ufAssert( p7.getName() == name );
  ufAssert( p8.getName() == "input" );
  ufAssert( p9.getName() == "bar" );

  ufAssert( p7.getLabelType() == WIR_SymbolType::block );
  ufAssert( p8.getLabelType() == WIR_SymbolType::data );
  ufAssert( p9.getLabelType() == WIR_SymbolType::function );

  ufAssert( p7.getBasicBlock().getID() == b.getID() );
  ufAssert( p8.getData().getID() == d.getID() );
  ufAssert( p9.getFunction().getID() == f.getID() );

  // Test the copy assignment operator.
  WIR_LabelParameter p10( f );
  p10 = p1;
  WIR_LabelParameter p11( b );
  p11 = p2;
  WIR_LabelParameter p12( d );
  p12 = p3;

  ufAssert( p10.getType() == WIR_ParameterType::label );
  ufAssert( p10.getType() == p11.getType() );
  ufAssert( p10.getType() == p12.getType() );

  ufAssert( p10.getName() == name );
  ufAssert( p11.getName() == "input" );
  ufAssert( p12.getName() == "bar" );

  ufAssert( p10.getLabelType() == WIR_SymbolType::block );
  ufAssert( p11.getLabelType() == WIR_SymbolType::data );
  ufAssert( p12.getLabelType() == WIR_SymbolType::function );

  // Test the move assignment operator.
  WIR_LabelParameter p13( f );
  p13 = move( p10 );
  WIR_LabelParameter p14( b );
  p14 = move( p11 );
  WIR_LabelParameter p15( d );
  p15 = move( p12 );

  ufAssert( p13.getType() == WIR_ParameterType::label );
  ufAssert( p13.getType() == p14.getType() );
  ufAssert( p13.getType() == p15.getType() );

  ufAssert( p13.getName() == name );
  ufAssert( p14.getName() == "input" );
  ufAssert( p15.getName() == "bar" );

  ufAssert( p13.getLabelType() == WIR_SymbolType::block );
  ufAssert( p14.getLabelType() == WIR_SymbolType::data );
  ufAssert( p15.getLabelType() == WIR_SymbolType::function );

  return( 0 );
}
