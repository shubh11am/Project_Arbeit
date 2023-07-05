/*

   This source file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Intermediate Representation Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2021 - 2022

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
#include <arch/riscv/rv32ic.h>


using namespace std;
using namespace WIR;


int main( void )
{
  WIR_Init();

  RV_Const6_Signed p1( 12 );
  RV_Const6_Signed p2( 24 );
  RV_Const6_Signed p3( -23 );

  // Check properties of the created parameters.
  ufAssert( p1.getType() == WIR_ParameterType::imm );
  ufAssert( p1.getType() == p2.getType() );
  ufAssert( p2.getType() == p3.getType() );

  ufAssert( p1.getValue() == 12 );
  ufAssert( p2.getValue() == 24 );
  ufAssert( p3.getValue() == -23 );

  p1.setValue( -23 );
  p2.setValue( 12 );
  p3.setValue( 24 );

  ufAssert( p1.getValue() == -23 );
  ufAssert( p2.getValue() == 12 );
  ufAssert( p3.getValue() == 24 );

  ufAssert( p1.isSigned() );
  ufAssert( !p1.isUnsigned() );
  ufAssert( p1.getBitWidth() == 6 );
  ufAssert( p1.isSigned() == p2.isSigned() );
  ufAssert( p2.isSigned() == p3.isSigned() );
  ufAssert( p1.isUnsigned() == p2.isUnsigned() );
  ufAssert( p2.isUnsigned() == p3.isUnsigned() );
  ufAssert( p1.getBitWidth() == p2.getBitWidth() );
  ufAssert( p2.getBitWidth() == p3.getBitWidth() );

  WIR_Parameter &ref1 = p1;
  auto ref2 = dynamic_cast<RV_Const6_Signed &>( ref1 );
  ufAssert( ref2.getValue() == p1.getValue() );

  // Test the copy constructors.
  RV_Const6_Signed p4( p1 );
  RV_Const6_Signed p5( p2 );
  RV_Const6_Signed p6( p3 );

  ufAssert( p4.getType() == WIR_ParameterType::imm );
  ufAssert( p4.getType() == p5.getType() );
  ufAssert( p5.getType() == p6.getType() );

  ufAssert( p4.getValue() == -23 );
  ufAssert( p5.getValue() == 12 );
  ufAssert( p6.getValue() == 24 );

  ufAssert( p4.isSigned() );
  ufAssert( !p4.isUnsigned() );
  ufAssert( p4.getBitWidth() == 6 );
  ufAssert( p4.isSigned() == p5.isSigned() );
  ufAssert( p5.isSigned() == p6.isSigned() );
  ufAssert( p4.isUnsigned() == p5.isUnsigned() );
  ufAssert( p5.isUnsigned() == p6.isUnsigned() );
  ufAssert( p4.getBitWidth() == p5.getBitWidth() );
  ufAssert( p5.getBitWidth() == p6.getBitWidth() );

  WIR_Parameter &ref3 = p5;
  auto ref4 = dynamic_cast<RV_Const6_Signed &>( ref3 );
  ufAssert( ref4.getValue() == p5.getValue() );

  // Test the move constructors.
  RV_Const6_Signed p7( move( p4 ) );
  RV_Const6_Signed p8( move( p5 ) );
  RV_Const6_Signed p9( move( p6 ) );

  ufAssert( p7.getType() == WIR_ParameterType::imm );
  ufAssert( p7.getType() == p8.getType() );
  ufAssert( p8.getType() == p9.getType() );

  ufAssert( p7.getValue() == -23 );
  ufAssert( p8.getValue() == 12 );
  ufAssert( p9.getValue() == 24 );
  ufAssert( p4.getValue() == 0 );
  ufAssert( p5.getValue() == 0 );
  ufAssert( p6.getValue() == 0 );

  ufAssert( p7.isSigned() );
  ufAssert( !p7.isUnsigned() );
  ufAssert( p7.getBitWidth() == 6 );
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
  auto ref6 = dynamic_cast<RV_Const6_Signed &>( ref5 );
  ufAssert( ref6.getValue() == p8.getValue() );

  // Test the copy assignment operator.
  RV_Const6_Signed p10( 23 );
  p10 = p1;
  RV_Const6_Signed p11( -31 );
  p11 = p2;
  RV_Const6_Signed p12( -12 );
  p12 = p3;

  ufAssert( p10.getType() == WIR_ParameterType::imm );
  ufAssert( p10.getType() == p11.getType() );
  ufAssert( p11.getType() == p12.getType() );

  ufAssert( p10.getValue() == -23 );
  ufAssert( p11.getValue() == 12 );
  ufAssert( p12.getValue() == 24 );

  ufAssert( p10.isSigned() );
  ufAssert( !p10.isUnsigned() );
  ufAssert( p10.getBitWidth() == 6 );
  ufAssert( p10.isSigned() == p11.isSigned() );
  ufAssert( p11.isSigned() == p12.isSigned() );
  ufAssert( p10.isUnsigned() == p11.isUnsigned() );
  ufAssert( p11.isUnsigned() == p12.isUnsigned() );
  ufAssert( p10.getBitWidth() == p11.getBitWidth() );
  ufAssert( p11.getBitWidth() == p12.getBitWidth() );

  WIR_Parameter &ref7 = p10;
  auto ref8 = dynamic_cast<RV_Const6_Signed &>( ref7 );
  ufAssert( ref8.getValue() == p10.getValue() );

  // Test the move assignment operator.
  RV_Const6_Signed p13( -23 );
  p13 = move( p10 );
  RV_Const6_Signed p14( -12 );
  p14 = move( p11 );
  RV_Const6_Signed p15( -8 );
  p15 = move( p12 );

  ufAssert( p13.getType() == WIR_ParameterType::imm );
  ufAssert( p13.getType() == p14.getType() );
  ufAssert( p14.getType() == p15.getType() );

  ufAssert( p13.getValue() == -23 );
  ufAssert( p14.getValue() == 12 );
  ufAssert( p15.getValue() == 24 );
  ufAssert( p10.getValue() == 0 );
  ufAssert( p11.getValue() == 0 );
  ufAssert( p12.getValue() == 0 );

  ufAssert( p13.isSigned() );
  ufAssert( !p13.isUnsigned() );
  ufAssert( p13.getBitWidth() == 6 );
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
  auto ref10 = dynamic_cast<RV_Const6_Signed &>( ref9 );
  ufAssert( ref10.getValue() == p14.getValue() );

  // Check min/max signed values for 6 bits.
  RV_Const6_Signed p16( -32 );
  RV_Const6_Signed p17( 31 );

  return( 0 );
}
