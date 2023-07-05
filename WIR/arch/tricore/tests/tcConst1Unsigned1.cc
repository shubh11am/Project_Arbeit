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

  TC_Const1_Unsigned p1( 0 );
  TC_Const1_Unsigned p2( 1 );

  // Check properties of the created parameters.
  ufAssert( p1.getType() == WIR_ParameterType::imm );
  ufAssert( p1.getType() == p2.getType() );

  ufAssert( p1.getValue() == 0 );
  ufAssert( p2.getValue() == 1 );

  p1.setValue( 1 );
  p2.setValue( 0 );

  ufAssert( p1.getValue() == 1 );
  ufAssert( p2.getValue() == 0 );

  ufAssert( !p1.isSigned() );
  ufAssert( p1.isUnsigned() );
  ufAssert( p1.getBitWidth() == 1 );
  ufAssert( p1.isSigned() == p2.isSigned() );
  ufAssert( p1.isUnsigned() == p2.isUnsigned() );
  ufAssert( p1.getBitWidth() == p2.getBitWidth() );

  WIR_Parameter &ref1 = p1;
  auto ref2 = dynamic_cast<TC_Const1_Unsigned &>( ref1 );
  ufAssert( ref2.getValue() == p1.getValue() );

  // Test the copy constructors.
  TC_Const1_Unsigned p4( p1 );
  TC_Const1_Unsigned p5( p2 );

  ufAssert( p4.getType() == WIR_ParameterType::imm );
  ufAssert( p4.getType() == p5.getType() );

  ufAssert( p4.getValue() == 1 );
  ufAssert( p5.getValue() == 0 );

  ufAssert( !p4.isSigned() );
  ufAssert( p4.isUnsigned() );
  ufAssert( p4.getBitWidth() == 1 );
  ufAssert( p4.isSigned() == p5.isSigned() );
  ufAssert( p4.isUnsigned() == p5.isUnsigned() );
  ufAssert( p4.getBitWidth() == p5.getBitWidth() );

  WIR_Parameter &ref3 = p5;
  auto ref4 = dynamic_cast<TC_Const1_Unsigned &>( ref3 );
  ufAssert( ref4.getValue() == p5.getValue() );

  // Test the move constructors.
  TC_Const1_Unsigned p7( move( p4 ) );
  TC_Const1_Unsigned p8( move( p5 ) );

  ufAssert( p7.getType() == WIR_ParameterType::imm );
  ufAssert( p7.getType() == p8.getType() );

  ufAssert( p7.getValue() == 1 );
  ufAssert( p8.getValue() == 0 );
  ufAssert( p4.getValue() == 0 );
  ufAssert( p5.getValue() == 0 );

  ufAssert( !p7.isSigned() );
  ufAssert( p7.isUnsigned() );
  ufAssert( p7.getBitWidth() == 1 );
  ufAssert( p7.isSigned() == p8.isSigned() );
  ufAssert( p7.isUnsigned() == p8.isUnsigned() );
  ufAssert( p7.getBitWidth() == p8.getBitWidth() );
  ufAssert( p4.getBitWidth() == 0 );
  ufAssert( p5.getBitWidth() == 0 );

  WIR_Parameter &ref5 = p8;
  auto ref6 = dynamic_cast<TC_Const1_Unsigned &>( ref5 );
  ufAssert( ref6.getValue() == p8.getValue() );

  // Test the copy assignment operator.
  TC_Const1_Unsigned p10( 0 );
  p10 = p1;
  TC_Const1_Unsigned p11( 1 );
  p11 = p2;

  ufAssert( p10.getType() == WIR_ParameterType::imm );
  ufAssert( p10.getType() == p11.getType() );

  ufAssert( p10.getValue() == 1 );
  ufAssert( p11.getValue() == 0 );

  ufAssert( !p10.isSigned() );
  ufAssert( p10.isUnsigned() );
  ufAssert( p10.getBitWidth() == 1 );
  ufAssert( p10.isSigned() == p11.isSigned() );
  ufAssert( p10.isUnsigned() == p11.isUnsigned() );
  ufAssert( p10.getBitWidth() == p11.getBitWidth() );

  WIR_Parameter &ref7 = p10;
  auto ref8 = dynamic_cast<TC_Const1_Unsigned &>( ref7 );
  ufAssert( ref8.getValue() == p10.getValue() );

  // Test the move assignment operator.
  TC_Const1_Unsigned p13( 0 );
  p13 = move( p10 );
  TC_Const1_Unsigned p14( 1 );
  p14 = move( p11 );

  ufAssert( p13.getType() == WIR_ParameterType::imm );
  ufAssert( p13.getType() == p14.getType() );

  ufAssert( p13.getValue() == 1 );
  ufAssert( p14.getValue() == 0 );
  ufAssert( p10.getValue() == 0 );
  ufAssert( p11.getValue() == 0 );

  ufAssert( !p13.isSigned() );
  ufAssert( p13.isUnsigned() );
  ufAssert( p13.getBitWidth() == 1 );
  ufAssert( p13.isSigned() == p14.isSigned() );
  ufAssert( p13.isUnsigned() == p14.isUnsigned() );
  ufAssert( p13.getBitWidth() == p14.getBitWidth() );
  ufAssert( p10.getBitWidth() == 0 );
  ufAssert( p11.getBitWidth() == 0 );

  WIR_Parameter &ref9 = p14;
  auto ref10 = dynamic_cast<TC_Const1_Unsigned &>( ref9 );
  ufAssert( ref10.getValue() == p14.getValue() );

  // Check min/max unsigned value for 1 bit.
  TC_Const1_Unsigned p16( 0 );
  TC_Const1_Unsigned p17( 1 );

  return( 0 );
}
