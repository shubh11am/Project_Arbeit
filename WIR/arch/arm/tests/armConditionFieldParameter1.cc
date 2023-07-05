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

  WIR_ConditionFieldParameter p1( ARM_Base::Condition::eq );
  WIR_ConditionFieldParameter p2( ARM_Base::Condition::ne );
  WIR_ConditionFieldParameter p3( ARM_Base::Condition::hs );
  WIR_ConditionFieldParameter p4( ARM_Base::Condition::lo );
  WIR_ConditionFieldParameter p5( ARM_Base::Condition::mi );
  WIR_ConditionFieldParameter p6( ARM_Base::Condition::pl );
  WIR_ConditionFieldParameter p7( ARM_Base::Condition::vs );
  WIR_ConditionFieldParameter p8( ARM_Base::Condition::vc );
  WIR_ConditionFieldParameter p9( ARM_Base::Condition::hi );
  WIR_ConditionFieldParameter p10( ARM_Base::Condition::ls );
  WIR_ConditionFieldParameter p11( ARM_Base::Condition::ge );
  WIR_ConditionFieldParameter p12( ARM_Base::Condition::lt );
  WIR_ConditionFieldParameter p13( ARM_Base::Condition::gt );
  WIR_ConditionFieldParameter p14( ARM_Base::Condition::le );
  WIR_ConditionFieldParameter p15( ARM_Base::Condition::al );

  // Check properties of the created parameters.
  ufAssert( p1.getType() == WIR_ParameterType::cond );
  ufAssert( p1.getType() == p2.getType() );

  ufAssert( p1.getCondition() == ARM_Base::Condition::eq );
  ufAssert( p2.getCondition() == ARM_Base::Condition::ne );
  ufAssert( p3.getCondition() == ARM_Base::Condition::hs );
  ufAssert( p4.getCondition() == ARM_Base::Condition::lo );
  ufAssert( p5.getCondition() == ARM_Base::Condition::mi );
  ufAssert( p6.getCondition() == ARM_Base::Condition::pl );
  ufAssert( p7.getCondition() == ARM_Base::Condition::vs );
  ufAssert( p8.getCondition() == ARM_Base::Condition::vc );
  ufAssert( p9.getCondition() == ARM_Base::Condition::hi );
  ufAssert( p10.getCondition() == ARM_Base::Condition::ls );
  ufAssert( p11.getCondition() == ARM_Base::Condition::ge );
  ufAssert( p12.getCondition() == ARM_Base::Condition::lt );
  ufAssert( p13.getCondition() == ARM_Base::Condition::gt );
  ufAssert( p14.getCondition() == ARM_Base::Condition::le );
  ufAssert( p15.getCondition() == ARM_Base::Condition::al );

  p1.setCondition( ARM_Base::Condition::al );
  p2.setCondition( ARM_Base::Condition::eq );
  p3.setCondition( ARM_Base::Condition::ne );
  p4.setCondition( ARM_Base::Condition::hs );
  p5.setCondition( ARM_Base::Condition::lo );
  p6.setCondition( ARM_Base::Condition::mi );
  p7.setCondition( ARM_Base::Condition::pl );
  p8.setCondition( ARM_Base::Condition::vs );
  p9.setCondition( ARM_Base::Condition::vc );
  p10.setCondition( ARM_Base::Condition::hi );
  p11.setCondition( ARM_Base::Condition::ls );
  p12.setCondition( ARM_Base::Condition::ge );
  p13.setCondition( ARM_Base::Condition::lt );
  p14.setCondition( ARM_Base::Condition::gt );
  p15.setCondition( ARM_Base::Condition::le );

  ufAssert( p1.getCondition() == ARM_Base::Condition::al );
  ufAssert( p2.getCondition() == ARM_Base::Condition::eq );
  ufAssert( p3.getCondition() == ARM_Base::Condition::ne );
  ufAssert( p4.getCondition() == ARM_Base::Condition::hs );
  ufAssert( p5.getCondition() == ARM_Base::Condition::lo );
  ufAssert( p6.getCondition() == ARM_Base::Condition::mi );
  ufAssert( p7.getCondition() == ARM_Base::Condition::pl );
  ufAssert( p8.getCondition() == ARM_Base::Condition::vs );
  ufAssert( p9.getCondition() == ARM_Base::Condition::vc );
  ufAssert( p10.getCondition() == ARM_Base::Condition::hi );
  ufAssert( p11.getCondition() == ARM_Base::Condition::ls );
  ufAssert( p12.getCondition() == ARM_Base::Condition::ge );
  ufAssert( p13.getCondition() == ARM_Base::Condition::lt );
  ufAssert( p14.getCondition() == ARM_Base::Condition::gt );
  ufAssert( p15.getCondition() == ARM_Base::Condition::le );

  WIR_Parameter &ref1 = p1;
  auto &ref2 = dynamic_cast<WIR_ConditionFieldParameter &>( ref1 );
  ufAssert( ref2.getCondition() == p1.getCondition() );

  // Test the copy constructors.
  WIR_ConditionFieldParameter c1( p1 );
  WIR_ConditionFieldParameter c2( p2 );
  WIR_ConditionFieldParameter c3( p3 );
  WIR_ConditionFieldParameter c4( p4 );
  WIR_ConditionFieldParameter c5( p5 );
  WIR_ConditionFieldParameter c6( p6 );
  WIR_ConditionFieldParameter c7( p7 );
  WIR_ConditionFieldParameter c8( p8 );
  WIR_ConditionFieldParameter c9( p9 );
  WIR_ConditionFieldParameter c10( p10 );
  WIR_ConditionFieldParameter c11( p11 );
  WIR_ConditionFieldParameter c12( p12 );
  WIR_ConditionFieldParameter c13( p13 );
  WIR_ConditionFieldParameter c14( p14 );
  WIR_ConditionFieldParameter c15( p15 );

  ufAssert( c4.getType() == WIR_ParameterType::cond );
  ufAssert( c4.getType() == c5.getType() );

  ufAssert( c1.getCondition() == ARM_Base::Condition::al );
  ufAssert( c2.getCondition() == ARM_Base::Condition::eq );
  ufAssert( c3.getCondition() == ARM_Base::Condition::ne );
  ufAssert( c4.getCondition() == ARM_Base::Condition::hs );
  ufAssert( c5.getCondition() == ARM_Base::Condition::lo );
  ufAssert( c6.getCondition() == ARM_Base::Condition::mi );
  ufAssert( c7.getCondition() == ARM_Base::Condition::pl );
  ufAssert( c8.getCondition() == ARM_Base::Condition::vs );
  ufAssert( c9.getCondition() == ARM_Base::Condition::vc );
  ufAssert( c10.getCondition() == ARM_Base::Condition::hi );
  ufAssert( c11.getCondition() == ARM_Base::Condition::ls );
  ufAssert( c12.getCondition() == ARM_Base::Condition::ge );
  ufAssert( c13.getCondition() == ARM_Base::Condition::lt );
  ufAssert( c14.getCondition() == ARM_Base::Condition::gt );
  ufAssert( c15.getCondition() == ARM_Base::Condition::le );

  // Test the move constructors.
  WIR_ConditionFieldParameter m1( move( c1 ) );
  WIR_ConditionFieldParameter m2( move( c2 ) );
  WIR_ConditionFieldParameter m3( move( c3 ) );
  WIR_ConditionFieldParameter m4( move( c4 ) );
  WIR_ConditionFieldParameter m5( move( c5 ) );
  WIR_ConditionFieldParameter m6( move( c6 ) );
  WIR_ConditionFieldParameter m7( move( c7 ) );
  WIR_ConditionFieldParameter m8( move( c8 ) );
  WIR_ConditionFieldParameter m9( move( c9 ) );
  WIR_ConditionFieldParameter m10( move( c10 ) );
  WIR_ConditionFieldParameter m11( move( c11 ) );
  WIR_ConditionFieldParameter m12( move( c12 ) );
  WIR_ConditionFieldParameter m13( move( c13 ) );
  WIR_ConditionFieldParameter m14( move( c14 ) );
  WIR_ConditionFieldParameter m15( move( c15 ) );

  ufAssert( m7.getType() == WIR_ParameterType::cond );
  ufAssert( m7.getType() == m8.getType() );

  ufAssert( m1.getCondition() == ARM_Base::Condition::al );
  ufAssert( m2.getCondition() == ARM_Base::Condition::eq );
  ufAssert( m3.getCondition() == ARM_Base::Condition::ne );
  ufAssert( m4.getCondition() == ARM_Base::Condition::hs );
  ufAssert( m5.getCondition() == ARM_Base::Condition::lo );
  ufAssert( m6.getCondition() == ARM_Base::Condition::mi );
  ufAssert( m7.getCondition() == ARM_Base::Condition::pl );
  ufAssert( m8.getCondition() == ARM_Base::Condition::vs );
  ufAssert( m9.getCondition() == ARM_Base::Condition::vc );
  ufAssert( m10.getCondition() == ARM_Base::Condition::hi );
  ufAssert( m11.getCondition() == ARM_Base::Condition::ls );
  ufAssert( m12.getCondition() == ARM_Base::Condition::ge );
  ufAssert( m13.getCondition() == ARM_Base::Condition::lt );
  ufAssert( m14.getCondition() == ARM_Base::Condition::gt );
  ufAssert( m15.getCondition() == ARM_Base::Condition::le );

  // Test the copy assignment operator.
  WIR_ConditionFieldParameter ca1( ARM_Base::Condition::le );
  ca1 = p1;
  WIR_ConditionFieldParameter ca2( ARM_Base::Condition::al );
  ca2 = p2;
  WIR_ConditionFieldParameter ca3( ARM_Base::Condition::al );
  ca3 = p3;
  WIR_ConditionFieldParameter ca4( ARM_Base::Condition::al );
  ca4 = p4;
  WIR_ConditionFieldParameter ca5( ARM_Base::Condition::al );
  ca5 = p5;
  WIR_ConditionFieldParameter ca6( ARM_Base::Condition::al );
  ca6 = p6;
  WIR_ConditionFieldParameter ca7( ARM_Base::Condition::al );
  ca7 = p7;
  WIR_ConditionFieldParameter ca8( ARM_Base::Condition::al );
  ca8 = p8;
  WIR_ConditionFieldParameter ca9( ARM_Base::Condition::al );
  ca9 = p9;
  WIR_ConditionFieldParameter ca10( ARM_Base::Condition::al );
  ca10 = p10;
  WIR_ConditionFieldParameter ca11( ARM_Base::Condition::al );
  ca11 = p11;
  WIR_ConditionFieldParameter ca12( ARM_Base::Condition::al );
  ca12 = p12;
  WIR_ConditionFieldParameter ca13( ARM_Base::Condition::al );
  ca13 = p13;
  WIR_ConditionFieldParameter ca14( ARM_Base::Condition::al );
  ca14 = p14;
  WIR_ConditionFieldParameter ca15( ARM_Base::Condition::al );
  ca15 = p15;

  ufAssert( ca7.getType() == WIR_ParameterType::cond );
  ufAssert( ca7.getType() == ca8.getType() );

  ufAssert( ca1.getCondition() == ARM_Base::Condition::al );
  ufAssert( ca2.getCondition() == ARM_Base::Condition::eq );
  ufAssert( ca3.getCondition() == ARM_Base::Condition::ne );
  ufAssert( ca4.getCondition() == ARM_Base::Condition::hs );
  ufAssert( ca5.getCondition() == ARM_Base::Condition::lo );
  ufAssert( ca6.getCondition() == ARM_Base::Condition::mi );
  ufAssert( ca7.getCondition() == ARM_Base::Condition::pl );
  ufAssert( ca8.getCondition() == ARM_Base::Condition::vs );
  ufAssert( ca9.getCondition() == ARM_Base::Condition::vc );
  ufAssert( ca10.getCondition() == ARM_Base::Condition::hi );
  ufAssert( ca11.getCondition() == ARM_Base::Condition::ls );
  ufAssert( ca12.getCondition() == ARM_Base::Condition::ge );
  ufAssert( ca13.getCondition() == ARM_Base::Condition::lt );
  ufAssert( ca14.getCondition() == ARM_Base::Condition::gt );
  ufAssert( ca15.getCondition() == ARM_Base::Condition::le );

  // Test the move assignment operator.
  WIR_ConditionFieldParameter ma1( ARM_Base::Condition::le );
  ma1 = move( ca1 );
  WIR_ConditionFieldParameter ma2( ARM_Base::Condition::al );
  ma2 = move( ca2 );
  WIR_ConditionFieldParameter ma3( ARM_Base::Condition::al );
  ma3 = move( ca3 );
  WIR_ConditionFieldParameter ma4( ARM_Base::Condition::al );
  ma4 = move( ca4 );
  WIR_ConditionFieldParameter ma5( ARM_Base::Condition::al );
  ma5 = move( ca5 );
  WIR_ConditionFieldParameter ma6( ARM_Base::Condition::al );
  ma6 = move( ca6 );
  WIR_ConditionFieldParameter ma7( ARM_Base::Condition::al );
  ma7 = move( ca7 );
  WIR_ConditionFieldParameter ma8( ARM_Base::Condition::al );
  ma8 = move( ca8 );
  WIR_ConditionFieldParameter ma9( ARM_Base::Condition::al );
  ma9 = move( ca9 );
  WIR_ConditionFieldParameter ma10( ARM_Base::Condition::al );
  ma10 = move( ca10 );
  WIR_ConditionFieldParameter ma11( ARM_Base::Condition::al );
  ma11 = move( ca11 );
  WIR_ConditionFieldParameter ma12( ARM_Base::Condition::al );
  ma12 = move( ca12 );
  WIR_ConditionFieldParameter ma13( ARM_Base::Condition::al );
  ma13 = move( ca13 );
  WIR_ConditionFieldParameter ma14( ARM_Base::Condition::al );
  ma14 = move( ca14 );
  WIR_ConditionFieldParameter ma15( ARM_Base::Condition::al );
  ma15 = move( ca15 );

  ufAssert( ma13.getType() == WIR_ParameterType::cond );
  ufAssert( ma13.getType() == ma14.getType() );

  ufAssert( ma1.getCondition() == ARM_Base::Condition::al );
  ufAssert( ma2.getCondition() == ARM_Base::Condition::eq );
  ufAssert( ma3.getCondition() == ARM_Base::Condition::ne );
  ufAssert( ma4.getCondition() == ARM_Base::Condition::hs );
  ufAssert( ma5.getCondition() == ARM_Base::Condition::lo );
  ufAssert( ma6.getCondition() == ARM_Base::Condition::mi );
  ufAssert( ma7.getCondition() == ARM_Base::Condition::pl );
  ufAssert( ma8.getCondition() == ARM_Base::Condition::vs );
  ufAssert( ma9.getCondition() == ARM_Base::Condition::vc );
  ufAssert( ma10.getCondition() == ARM_Base::Condition::hi );
  ufAssert( ma11.getCondition() == ARM_Base::Condition::ls );
  ufAssert( ma12.getCondition() == ARM_Base::Condition::ge );
  ufAssert( ma13.getCondition() == ARM_Base::Condition::lt );
  ufAssert( ma14.getCondition() == ARM_Base::Condition::gt );
  ufAssert( ma15.getCondition() == ARM_Base::Condition::le );

  return( 0 );
}
