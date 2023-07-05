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
#include <arch/arm/armbase.h>


using namespace std;
using namespace WIR;


int main( void )
{
  WIR_Init();

  ARM_Const3_CoprocessorOpcode p1( 0 );
  ARM_Const3_CoprocessorOpcode p2( 5 );
  ARM_Const3_CoprocessorOpcode p3( 2 );

  // Check properties of the created parameters.
  ufAssert( p1.getType() == WIR_ParameterType::imm );
  ufAssert( p1.getType() == p2.getType() );
  ufAssert( p2.getType() == p3.getType() );

  ufAssert( p1.getValue() == 0 );
  ufAssert( p2.getValue() == 5 );
  ufAssert( p3.getValue() == 2 );

  p1.setValue( 3 );
  p2.setValue( 1 );
  p3.setValue( 4 );

  ufAssert( p1.getValue() == 3 );
  ufAssert( p2.getValue() == 1 );
  ufAssert( p3.getValue() == 4 );

  ufAssert( !p1.isSigned() );
  ufAssert( p1.isUnsigned() );
  ufAssert( p1.getBitWidth() == 3 );
  ufAssert( p1.isSigned() == p2.isSigned() );
  ufAssert( p2.isSigned() == p3.isSigned() );
  ufAssert( p1.isUnsigned() == p2.isUnsigned() );
  ufAssert( p2.isUnsigned() == p3.isUnsigned() );
  ufAssert( p1.getBitWidth() == p2.getBitWidth() );
  ufAssert( p2.getBitWidth() == p3.getBitWidth() );

  WIR_Parameter &ref1 = p1;
  auto ref2 = dynamic_cast<ARM_Const3_CoprocessorOpcode &>( ref1 );
  ufAssert( ref2.getValue() == p1.getValue() );

  // Test the copy constructors.
  ARM_Const3_CoprocessorOpcode p4( p1 );
  ARM_Const3_CoprocessorOpcode p5( p2 );
  ARM_Const3_CoprocessorOpcode p6( p3 );

  ufAssert( p4.getType() == WIR_ParameterType::imm );
  ufAssert( p4.getType() == p5.getType() );
  ufAssert( p5.getType() == p6.getType() );

  ufAssert( p4.getValue() == 3 );
  ufAssert( p5.getValue() == 1 );
  ufAssert( p6.getValue() == 4 );

  ufAssert( !p4.isSigned() );
  ufAssert( p4.isUnsigned() );
  ufAssert( p4.getBitWidth() == 3 );
  ufAssert( p4.isSigned() == p5.isSigned() );
  ufAssert( p5.isSigned() == p6.isSigned() );
  ufAssert( p4.isUnsigned() == p5.isUnsigned() );
  ufAssert( p5.isUnsigned() == p6.isUnsigned() );
  ufAssert( p4.getBitWidth() == p5.getBitWidth() );
  ufAssert( p5.getBitWidth() == p6.getBitWidth() );

  WIR_Parameter &ref3 = p5;
  auto ref4 = dynamic_cast<ARM_Const3_CoprocessorOpcode &>( ref3 );
  ufAssert( ref4.getValue() == p5.getValue() );

  // Test the move constructors.
  ARM_Const3_CoprocessorOpcode p7( move( p4 ) );
  ARM_Const3_CoprocessorOpcode p8( move( p5 ) );
  ARM_Const3_CoprocessorOpcode p9( move( p6 ) );

  ufAssert( p7.getType() == WIR_ParameterType::imm );
  ufAssert( p7.getType() == p8.getType() );
  ufAssert( p8.getType() == p9.getType() );

  ufAssert( p7.getValue() == 3 );
  ufAssert( p8.getValue() == 1 );
  ufAssert( p9.getValue() == 4 );
  ufAssert( p4.getValue() == 0 );
  ufAssert( p5.getValue() == 0 );
  ufAssert( p6.getValue() == 0 );

  ufAssert( !p7.isSigned() );
  ufAssert( p7.isUnsigned() );
  ufAssert( p7.getBitWidth() == 3 );
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
  auto ref6 = dynamic_cast<ARM_Const3_CoprocessorOpcode &>( ref5 );
  ufAssert( ref6.getValue() == p8.getValue() );

  // Test the copy assignment operator.
  ARM_Const3_CoprocessorOpcode p10( 1 );
  p10 = p1;
  ARM_Const3_CoprocessorOpcode p11( 2 );
  p11 = p2;
  ARM_Const3_CoprocessorOpcode p12( 3 );
  p12 = p3;

  ufAssert( p10.getType() == WIR_ParameterType::imm );
  ufAssert( p10.getType() == p11.getType() );
  ufAssert( p11.getType() == p12.getType() );

  ufAssert( p10.getValue() == 3 );
  ufAssert( p11.getValue() == 1 );
  ufAssert( p12.getValue() == 4 );

  ufAssert( !p10.isSigned() );
  ufAssert( p10.isUnsigned() );
  ufAssert( p10.getBitWidth() == 3 );
  ufAssert( p10.isSigned() == p11.isSigned() );
  ufAssert( p11.isSigned() == p12.isSigned() );
  ufAssert( p10.isUnsigned() == p11.isUnsigned() );
  ufAssert( p11.isUnsigned() == p12.isUnsigned() );
  ufAssert( p10.getBitWidth() == p11.getBitWidth() );
  ufAssert( p11.getBitWidth() == p12.getBitWidth() );

  WIR_Parameter &ref7 = p10;
  auto ref8 = dynamic_cast<ARM_Const3_CoprocessorOpcode &>( ref7 );
  ufAssert( ref8.getValue() == p10.getValue() );

  // Test the move assignment operator.
  ARM_Const3_CoprocessorOpcode p13( 7 );
  p13 = move( p10 );
  ARM_Const3_CoprocessorOpcode p14( 6 );
  p14 = move( p11 );
  ARM_Const3_CoprocessorOpcode p15( 5 );
  p15 = move( p12 );

  ufAssert( p13.getType() == WIR_ParameterType::imm );
  ufAssert( p13.getType() == p14.getType() );
  ufAssert( p14.getType() == p15.getType() );

  ufAssert( p13.getValue() == 3 );
  ufAssert( p14.getValue() == 1 );
  ufAssert( p15.getValue() == 4 );
  ufAssert( p10.getValue() == 0 );
  ufAssert( p11.getValue() == 0 );
  ufAssert( p12.getValue() == 0 );

  ufAssert( !p13.isSigned() );
  ufAssert( p13.isUnsigned() );
  ufAssert( p13.getBitWidth() == 3 );
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
  auto ref10 = dynamic_cast<ARM_Const3_CoprocessorOpcode &>( ref9 );
  ufAssert( ref10.getValue() == p14.getValue() );

  // Check min/max unsigned value for 3 bits.
  ARM_Const3_CoprocessorOpcode p16( 0 );
  ARM_Const3_CoprocessorOpcode p17( 7 );

  return( 0 );
}
