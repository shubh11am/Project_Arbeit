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

  TC_Const2_Unsigned p1( 0 );
  TC_Const2_Unsigned p2( 1 );
  TC_Const2_Unsigned p3( 2 );

  // Check properties of the created parameters.
  ufAssert( p1.getType() == WIR_ParameterType::imm );
  ufAssert( p1.getType() == p2.getType() );
  ufAssert( p2.getType() == p3.getType() );

  ufAssert( p1.getValue() == 0 );
  ufAssert( p2.getValue() == 1 );
  ufAssert( p3.getValue() == 2 );

  p1.setValue( 3 );
  p2.setValue( 2 );
  p3.setValue( 1 );

  ufAssert( p1.getValue() == 3 );
  ufAssert( p2.getValue() == 2 );
  ufAssert( p3.getValue() == 1 );

  ufAssert( !p1.isSigned() );
  ufAssert( p1.isUnsigned() );
  ufAssert( p1.getBitWidth() == 2 );
  ufAssert( p1.isSigned() == p2.isSigned() );
  ufAssert( p2.isSigned() == p3.isSigned() );
  ufAssert( p1.isUnsigned() == p2.isUnsigned() );
  ufAssert( p2.isUnsigned() == p3.isUnsigned() );
  ufAssert( p1.getBitWidth() == p2.getBitWidth() );
  ufAssert( p2.getBitWidth() == p3.getBitWidth() );

  WIR_Parameter &ref1 = p1;
  auto ref2 = dynamic_cast<TC_Const2_Unsigned &>( ref1 );
  ufAssert( ref2.getValue() == p1.getValue() );

  // Test the copy constructors.
  TC_Const2_Unsigned p4( p1 );
  TC_Const2_Unsigned p5( p2 );
  TC_Const2_Unsigned p6( p3 );

  ufAssert( p4.getType() == WIR_ParameterType::imm );
  ufAssert( p4.getType() == p5.getType() );
  ufAssert( p5.getType() == p6.getType() );

  ufAssert( p4.getValue() == 3 );
  ufAssert( p5.getValue() == 2 );
  ufAssert( p6.getValue() == 1 );

  ufAssert( !p4.isSigned() );
  ufAssert( p4.isUnsigned() );
  ufAssert( p4.getBitWidth() == 2 );
  ufAssert( p4.isSigned() == p5.isSigned() );
  ufAssert( p5.isSigned() == p6.isSigned() );
  ufAssert( p4.isUnsigned() == p5.isUnsigned() );
  ufAssert( p5.isUnsigned() == p6.isUnsigned() );
  ufAssert( p4.getBitWidth() == p5.getBitWidth() );
  ufAssert( p5.getBitWidth() == p6.getBitWidth() );

  WIR_Parameter &ref3 = p5;
  auto ref4 = dynamic_cast<TC_Const2_Unsigned &>( ref3 );
  ufAssert( ref4.getValue() == p5.getValue() );

  // Test the move constructors.
  TC_Const2_Unsigned p7( move( p4 ) );
  TC_Const2_Unsigned p8( move( p5 ) );
  TC_Const2_Unsigned p9( move( p6 ) );

  ufAssert( p7.getType() == WIR_ParameterType::imm );
  ufAssert( p7.getType() == p8.getType() );
  ufAssert( p8.getType() == p9.getType() );

  ufAssert( p7.getValue() == 3 );
  ufAssert( p8.getValue() == 2 );
  ufAssert( p9.getValue() == 1 );
  ufAssert( p4.getValue() == 0 );
  ufAssert( p5.getValue() == 0 );
  ufAssert( p6.getValue() == 0 );

  ufAssert( !p7.isSigned() );
  ufAssert( p7.isUnsigned() );
  ufAssert( p7.getBitWidth() == 2 );
  ufAssert( p7.isSigned() == p8.isSigned() );
  ufAssert( p8.isSigned() == p9.isSigned() );
  ufAssert( p7.isUnsigned() == p8.isUnsigned() );
  ufAssert( p8.isUnsigned() == p9.isUnsigned() );
  ufAssert( p7.getBitWidth() == p8.getBitWidth() );
  ufAssert( p8.getBitWidth() == p9.getBitWidth() );
  ufAssert( p4.getBitWidth() == 0 );
  ufAssert( p5.getBitWidth() == 0 );
  ufAssert( p6.getBitWidth() == 0 );

  WIR_Parameter &ref5 = p8;
  auto ref6 = dynamic_cast<TC_Const2_Unsigned &>( ref5 );
  ufAssert( ref6.getValue() == p8.getValue() );

  // Test the copy assignment operator.
  TC_Const2_Unsigned p10( 0 );
  p10 = p1;
  TC_Const2_Unsigned p11( 1 );
  p11 = p2;
  TC_Const2_Unsigned p12( 2 );
  p12 = p3;

  ufAssert( p10.getType() == WIR_ParameterType::imm );
  ufAssert( p10.getType() == p11.getType() );
  ufAssert( p11.getType() == p12.getType() );

  ufAssert( p10.getValue() == 3 );
  ufAssert( p11.getValue() == 2 );
  ufAssert( p12.getValue() == 1 );

  ufAssert( !p10.isSigned() );
  ufAssert( p10.isUnsigned() );
  ufAssert( p10.getBitWidth() == 2 );
  ufAssert( p10.isSigned() == p11.isSigned() );
  ufAssert( p11.isSigned() == p12.isSigned() );
  ufAssert( p10.isUnsigned() == p11.isUnsigned() );
  ufAssert( p11.isUnsigned() == p12.isUnsigned() );
  ufAssert( p10.getBitWidth() == p11.getBitWidth() );
  ufAssert( p11.getBitWidth() == p12.getBitWidth() );

  WIR_Parameter &ref7 = p10;
  auto ref8 = dynamic_cast<TC_Const2_Unsigned &>( ref7 );
  ufAssert( ref8.getValue() == p10.getValue() );

  // Test the move assignment operator.
  TC_Const2_Unsigned p13( 1 );
  p13 = move( p10 );
  TC_Const2_Unsigned p14( 2 );
  p14 = move( p11 );
  TC_Const2_Unsigned p15( 3 );
  p15 = move( p12 );

  ufAssert( p13.getType() == WIR_ParameterType::imm );
  ufAssert( p13.getType() == p14.getType() );
  ufAssert( p14.getType() == p15.getType() );

  ufAssert( p13.getValue() == 3 );
  ufAssert( p14.getValue() == 2 );
  ufAssert( p15.getValue() == 1 );
  ufAssert( p10.getValue() == 0 );
  ufAssert( p11.getValue() == 0 );
  ufAssert( p12.getValue() == 0 );

  ufAssert( !p13.isSigned() );
  ufAssert( p13.isUnsigned() );
  ufAssert( p13.getBitWidth() == 2 );
  ufAssert( p13.isSigned() == p14.isSigned() );
  ufAssert( p14.isSigned() == p15.isSigned() );
  ufAssert( p13.isUnsigned() == p14.isUnsigned() );
  ufAssert( p14.isUnsigned() == p15.isUnsigned() );
  ufAssert( p13.getBitWidth() == p14.getBitWidth() );
  ufAssert( p14.getBitWidth() == p15.getBitWidth() );
  ufAssert( p10.getBitWidth() == 0 );
  ufAssert( p11.getBitWidth() == 0 );
  ufAssert( p12.getBitWidth() == 0 );

  WIR_Parameter &ref9 = p14;
  auto ref10 = dynamic_cast<TC_Const2_Unsigned &>( ref9 );
  ufAssert( ref10.getValue() == p14.getValue() );

  // Check min/max unsigned value for 2 bits.
  TC_Const2_Unsigned p16( 0 );
  TC_Const2_Unsigned p17( 3 );

  return( 0 );
}
