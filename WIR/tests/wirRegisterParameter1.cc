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
#include <arch/generic/mips.h>


using namespace std;
using namespace WIR;


int main( void )
{
  WIR_Init();

  WIR_VirtualRegister r1( MIPS::RegisterType::reg ),
                      r2( MIPS::RegisterType::reg ),
                      r3( MIPS::RegisterType::reg );
  WIR_RegisterParameter p1( r1, WIR_Usage::def );
  WIR_RegisterParameter p2( r2, WIR_Usage::use );
  WIR_RegisterParameter p3( r3, WIR_Usage::defuse );
  p3.setImplicit();

  // Check properties of the created parameters.
  ufAssert( p1.getType() == WIR_ParameterType::reg );
  ufAssert( p1.getType() == p2.getType() );
  ufAssert( p2.getType() == p3.getType() );

  ufAssert( p1.getRegister() == r1 );
  ufAssert( p2.getRegister() == r2 );
  ufAssert( p3.getRegister() == r3 );
  ufAssert( p1.getRegister() != r2 );
  ufAssert( p2.getRegister() != r3 );
  ufAssert( p3.getRegister() != r1 );

  ufAssert( p1.getUsage() == WIR_Usage::def );
  ufAssert( p2.getUsage() == WIR_Usage::use );
  ufAssert( p3.getUsage() == WIR_Usage::defuse );
  ufAssert( p1.isDefined() && !p1.isUsed() && !p1.isDefUsed() );
  ufAssert( !p2.isDefined() && p2.isUsed() && !p2.isDefUsed() );
  ufAssert( !p3.isDefined() && !p3.isUsed() && p3.isDefUsed() );

  p1.setUsage( WIR_Usage:: use );
  p2.setUsage( WIR_Usage:: defuse );
  p3.setUsage( WIR_Usage:: def );
  ufAssert( !p1.isDefined() && p1.isUsed() && !p1.isDefUsed() );
  ufAssert( !p2.isDefined() && !p2.isUsed() && p2.isDefUsed() );
  ufAssert( p3.isDefined() && !p3.isUsed() && !p3.isDefUsed() );

  ufAssert( !p1.isImplicit() );
  ufAssert( !p2.isImplicit() );
  ufAssert( p3.isImplicit() );

  p1.setImplicit();
  p3.setImplicit( false );
  ufAssert( p1.isImplicit() );
  ufAssert( !p2.isImplicit() );
  ufAssert( !p3.isImplicit() );

  WIR_Parameter &ref1 = p3;
  auto ref2 = dynamic_cast<WIR_RegisterParameter &>( ref1 );
  ufAssert( ref2.getRegister() == p3.getRegister() );

  // Test the copy constructors.
  WIR_RegisterParameter p4( p1 );
  WIR_RegisterParameter p5( p2 );
  WIR_RegisterParameter p6( p3 );

  ufAssert( p4.getRegister() == r1 );
  ufAssert( p5.getRegister() == r2 );
  ufAssert( p6.getRegister() == r3 );

  ufAssert( p4.getUsage() == WIR_Usage::use );
  ufAssert( p5.getUsage() == WIR_Usage::defuse );
  ufAssert( p6.getUsage() == WIR_Usage::def );
  ufAssert( !p4.isDefined() && p4.isUsed() && !p4.isDefUsed() );
  ufAssert( !p5.isDefined() && !p5.isUsed() && p5.isDefUsed() );
  ufAssert( p6.isDefined() && !p6.isUsed() && !p6.isDefUsed() );

  ufAssert( p4.isImplicit() );
  ufAssert( !p5.isImplicit() );
  ufAssert( !p6.isImplicit() );

  WIR_Parameter &ref3 = p5;
  auto ref4 = dynamic_cast<WIR_RegisterParameter &>( ref3 );
  ufAssert( ref4.getRegister() == p5.getRegister() );

  // Test the move constructors.
  WIR_RegisterParameter p7( move( p4 ) );
  WIR_RegisterParameter p8( move( p5 ) );
  WIR_RegisterParameter p9( move( p6 ) );

  ufAssert( p7.getRegister() == r1 );
  ufAssert( p8.getRegister() == r2 );
  ufAssert( p9.getRegister() == r3 );

  ufAssert( p7.getUsage() == WIR_Usage::use );
  ufAssert( p8.getUsage() == WIR_Usage::defuse );
  ufAssert( p9.getUsage() == WIR_Usage::def );
  ufAssert( !p7.isDefined() && p7.isUsed() && !p7.isDefUsed() );
  ufAssert( !p8.isDefined() && !p8.isUsed() && p8.isDefUsed() );
  ufAssert( p9.isDefined() && !p9.isUsed() && !p9.isDefUsed() );

  ufAssert( p7.isImplicit() );
  ufAssert( !p8.isImplicit() );
  ufAssert( !p9.isImplicit() );

  WIR_Parameter &ref5 = p9;
  auto ref6 = dynamic_cast<WIR_RegisterParameter &>( ref5 );
  ufAssert( ref6.getRegister() == p9.getRegister() );

  // Test the copy assignment operator.
  WIR_RegisterParameter p10( r3, WIR_Usage::defuse );
  p10 = p1;
  WIR_RegisterParameter p11( r3, WIR_Usage::defuse );
  p11 = p2;
  WIR_RegisterParameter p12( r3, WIR_Usage::defuse );
  p12 = p3;

  ufAssert( p10.getRegister() == r1 );
  ufAssert( p11.getRegister() == r2 );
  ufAssert( p12.getRegister() == r3 );

  ufAssert( p10.getUsage() == WIR_Usage::use );
  ufAssert( p11.getUsage() == WIR_Usage::defuse );
  ufAssert( p12.getUsage() == WIR_Usage::def );
  ufAssert( !p10.isDefined() && p10.isUsed() && !p10.isDefUsed() );
  ufAssert( !p11.isDefined() && !p11.isUsed() && p11.isDefUsed() );
  ufAssert( p12.isDefined() && !p12.isUsed() && !p12.isDefUsed() );

  ufAssert( p10.isImplicit() );
  ufAssert( !p11.isImplicit() );
  ufAssert( !p12.isImplicit() );

  WIR_Parameter &ref7 = p11;
  auto ref8 = dynamic_cast<WIR_RegisterParameter &>( ref7 );
  ufAssert( ref8.getRegister() == p11.getRegister() );

  // Test the move assignment operator.
  WIR_RegisterParameter p13( r3, WIR_Usage::defuse );
  p13 = move( p10 );
  WIR_RegisterParameter p14( r3, WIR_Usage::defuse );
  p14 = move( p11 );
  WIR_RegisterParameter p15( r3, WIR_Usage::defuse );
  p15 = move( p12 );

  ufAssert( p13.getRegister() == r1 );
  ufAssert( p14.getRegister() == r2 );
  ufAssert( p15.getRegister() == r3 );

  ufAssert( p13.getUsage() == WIR_Usage::use );
  ufAssert( p14.getUsage() == WIR_Usage::defuse );
  ufAssert( p15.getUsage() == WIR_Usage::def );
  ufAssert( !p13.isDefined() && p13.isUsed() && !p13.isDefUsed() );
  ufAssert( !p14.isDefined() && !p14.isUsed() && p14.isDefUsed() );
  ufAssert( p15.isDefined() && !p15.isUsed() && !p15.isDefUsed() );

  ufAssert( p13.isImplicit() );
  ufAssert( !p14.isImplicit() );
  ufAssert( !p15.isImplicit() );

  WIR_Parameter &ref9 = p13;
  auto ref10 = dynamic_cast<WIR_RegisterParameter &>( ref9 );
  ufAssert( ref10.getRegister() == p13.getRegister() );

  return( 0 );
}
