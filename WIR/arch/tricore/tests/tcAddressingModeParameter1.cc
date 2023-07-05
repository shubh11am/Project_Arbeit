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
#include <arch/tricore/tc131.h>


using namespace std;
using namespace WIR;


int main( void )
{
  WIR_Init();

  WIR_AddressingModeParameter p1( TC131::AddressingMode::post );
  WIR_AddressingModeParameter p2( TC131::AddressingMode::pre );

  // Check properties of the created parameters.
  ufAssert( p1.getType() == WIR_ParameterType::addr );
  ufAssert( p1.getType() == p2.getType() );

  ufAssert( p1.getAddressingMode() == TC131::AddressingMode::post );
  ufAssert( p2.getAddressingMode() == TC131::AddressingMode::pre );

  p1.setAddressingMode( TC131::AddressingMode::pre );
  p2.setAddressingMode( TC131::AddressingMode::post );

  ufAssert( p1.getAddressingMode() == TC131::AddressingMode::pre );
  ufAssert( p2.getAddressingMode() == TC131::AddressingMode::post );

  WIR_Parameter &ref1 = p1;
  auto ref2 = dynamic_cast<WIR_AddressingModeParameter &>( ref1 );
  ufAssert( ref2.getAddressingMode() == p1.getAddressingMode() );

  // Test the copy constructors.
  WIR_AddressingModeParameter p4( p1 );
  WIR_AddressingModeParameter p5( p2 );

  ufAssert( p4.getType() == WIR_ParameterType::addr );
  ufAssert( p4.getType() == p5.getType() );

  ufAssert( p4.getAddressingMode() == TC131::AddressingMode::pre );
  ufAssert( p5.getAddressingMode() == TC131::AddressingMode::post );

  WIR_Parameter &ref3 = p5;
  auto ref4 = dynamic_cast<WIR_AddressingModeParameter &>( ref3 );
  ufAssert( ref4.getAddressingMode() == p5.getAddressingMode() );

  // Test the move constructors.
  WIR_AddressingModeParameter p7( move( p4 ) );
  WIR_AddressingModeParameter p8( move( p5 ) );

  ufAssert( p7.getType() == WIR_ParameterType::addr );
  ufAssert( p7.getType() == p8.getType() );

  ufAssert( p7.getAddressingMode() == TC131::AddressingMode::pre );
  ufAssert( p8.getAddressingMode() == TC131::AddressingMode::post );

  WIR_Parameter &ref5 = p8;
  auto ref6 = dynamic_cast<WIR_AddressingModeParameter &>( ref5 );
  ufAssert( ref6.getAddressingMode() == p8.getAddressingMode() );

  // Test the copy assignment operator.
  WIR_AddressingModeParameter p10( TC131::AddressingMode::post );
  p10 = p1;
  WIR_AddressingModeParameter p11( TC131::AddressingMode::pre );
  p11 = p2;

  ufAssert( p10.getType() == WIR_ParameterType::addr );
  ufAssert( p10.getType() == p11.getType() );

  ufAssert( p10.getAddressingMode() == TC131::AddressingMode::pre );
  ufAssert( p11.getAddressingMode() == TC131::AddressingMode::post );

  WIR_Parameter &ref7 = p10;
  auto ref8 = dynamic_cast<WIR_AddressingModeParameter &>( ref7 );
  ufAssert( ref8.getAddressingMode() == p10.getAddressingMode() );

  // Test the move assignment operator.
  WIR_AddressingModeParameter p13( TC131::AddressingMode::post );
  p13 = move( p10 );
  WIR_AddressingModeParameter p14( TC131::AddressingMode::pre );
  p14 = move( p11 );

  ufAssert( p13.getType() == WIR_ParameterType::addr );
  ufAssert( p13.getType() == p14.getType() );

  ufAssert( p13.getAddressingMode() == TC131::AddressingMode::pre );
  ufAssert( p14.getAddressingMode() == TC131::AddressingMode::post );

  WIR_Parameter &ref9 = p14;
  auto ref10 = dynamic_cast<WIR_AddressingModeParameter &>( ref9 );
  ufAssert( ref10.getAddressingMode() == p14.getAddressingMode() );

  return( 0 );
}
