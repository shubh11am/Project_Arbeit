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
#include <sstream>

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

  stringstream str;

  ARM_RegV r1;
  ufAssert( r1.getType() == ARM_Base::RegisterType::reg );
  ufAssert( r1.getBitWidth() == 32 );
  ufAssert( r1.isVirtual() );
  ufAssert( !r1.isPhysical() );
  ufAssert( !r1.isInserted() );
  str << ARM_Base::RegisterType::reg.getPrefixes()[ 1 ] << r1.getID()
      << ARM_Base::RegisterType::reg.getSuffixes()[ 1 ];
  ufAssert( r1.getName() == str.str() );

  WIR_Function f( "foo" );
  ARM_LoRegV &r2 =
    static_cast<ARM_LoRegV &>( f.pushBackVirtualRegister( ARM_LoRegV() ) );
  ufAssert( r2.getType() == ARM_Base::RegisterType::lo );
  ufAssert( r2.getBitWidth() == 32 );
  ufAssert( r2.isVirtual() );
  ufAssert( !r2.isPhysical() );
  ufAssert( r2.isInserted() );
  str.str( "" );
  str << ARM_Base::RegisterType::lo.getPrefixes()[ 1 ] << r2.getID()
      << ARM_Base::RegisterType::lo.getSuffixes()[ 1 ];
  ufAssert( r2.getName() == str.str() );
  ufAssert( r2.getFunction().getID() == f.getID() );

  ARM_HiRegV &r3 =
    static_cast<ARM_HiRegV &>( f.pushBackVirtualRegister( ARM_HiRegV() ) );
  ufAssert( r3.getType() == ARM_Base::RegisterType::hi );
  ufAssert( r3.getBitWidth() == 32 );
  ufAssert( r3.isVirtual() );
  ufAssert( !r3.isPhysical() );
  ufAssert( r3.isInserted() );
  str.str( "" );
  str << ARM_Base::RegisterType::hi.getPrefixes()[ 1 ] << r3.getID()
      << ARM_Base::RegisterType::hi.getSuffixes()[ 1 ];
  ufAssert( r3.getName() == str.str() );
  ufAssert( r3.getFunction().getID() == f.getID() );

  // Test the copy constructors.
  ARM_RegV r4 = r1;
  ARM_LoRegV r5 = r2;
  ARM_HiRegV r6 = r3;

  ufAssert( r4.getType() == ARM_Base::RegisterType::reg );
  ufAssert( r4.getBitWidth() == 32 );
  ufAssert( r4.isVirtual() );
  ufAssert( !r4.isPhysical() );
  ufAssert( !r4.isInserted() );
  str.str( "" );
  str << ARM_Base::RegisterType::reg.getPrefixes()[ 1 ] << r4.getID()
      << ARM_Base::RegisterType::reg.getSuffixes()[ 1 ];
  ufAssert( r4.getName() == str.str() );

  ufAssert( r5.getType() == ARM_Base::RegisterType::lo );
  ufAssert( r5.getBitWidth() == 32 );
  ufAssert( r5.isVirtual() );
  ufAssert( !r5.isPhysical() );
  ufAssert( !r5.isInserted() );
  str.str( "" );
  str << ARM_Base::RegisterType::lo.getPrefixes()[ 1 ] << r5.getID()
      << ARM_Base::RegisterType::lo.getSuffixes()[ 1 ];
  ufAssert( r5.getName() == str.str() );

  ufAssert( r6.getType() == ARM_Base::RegisterType::hi );
  ufAssert( r6.getBitWidth() == 32 );
  ufAssert( r6.isVirtual() );
  ufAssert( !r6.isPhysical() );
  ufAssert( !r6.isInserted() );
  str.str( "" );
  str << ARM_Base::RegisterType::hi.getPrefixes()[ 1 ] << r6.getID()
      << ARM_Base::RegisterType::hi.getSuffixes()[ 1 ];
  ufAssert( r6.getName() == str.str() );

  // Test the copy assignment operator.
  ARM_RegV r7;
  ARM_LoRegV r8;
  ARM_HiRegV r9;
  r7 = r4;
  r8 = r5;
  r9 = r6;

  ufAssert( r7.getType() == ARM_Base::RegisterType::reg );
  ufAssert( r7.getBitWidth() == 32 );
  ufAssert( r7.isVirtual() );
  ufAssert( !r7.isPhysical() );
  ufAssert( !r7.isInserted() );
  str.str( "" );
  str << ARM_Base::RegisterType::reg.getPrefixes()[ 1 ] << r7.getID()
      << ARM_Base::RegisterType::reg.getSuffixes()[ 1 ];
  ufAssert( r7.getName() == str.str() );

  ufAssert( r8.getType() == ARM_Base::RegisterType::lo );
  ufAssert( r8.getBitWidth() == 32 );
  ufAssert( r8.isVirtual() );
  ufAssert( !r8.isPhysical() );
  ufAssert( !r8.isInserted() );
  str.str( "" );
  str << ARM_Base::RegisterType::lo.getPrefixes()[ 1 ] << r8.getID()
      << ARM_Base::RegisterType::lo.getSuffixes()[ 1 ];
  ufAssert( r8.getName() == str.str() );

  ufAssert( r9.getType() == ARM_Base::RegisterType::hi );
  ufAssert( r9.getBitWidth() == 32 );
  ufAssert( r9.isVirtual() );
  ufAssert( !r9.isPhysical() );
  ufAssert( !r9.isInserted() );
  str.str( "" );
  str << ARM_Base::RegisterType::hi.getPrefixes()[ 1 ] << r9.getID()
      << ARM_Base::RegisterType::hi.getSuffixes()[ 1 ];
  ufAssert( r9.getName() == str.str() );

  // Test the move constructor.
  ARM_RegV r10 = move( r4 );
  ARM_LoRegV r11 = move( r5 );
  ARM_HiRegV r12 = move( r6 );

  ufAssert( r10.getType() == ARM_Base::RegisterType::reg );
  ufAssert( r10.getBitWidth() == 32 );
  ufAssert( r10.isVirtual() );
  ufAssert( !r10.isPhysical() );
  ufAssert( !r10.isInserted() );
  str.str( "" );
  str << ARM_Base::RegisterType::reg.getPrefixes()[ 1 ] << r10.getID()
      << ARM_Base::RegisterType::reg.getSuffixes()[ 1 ];
  ufAssert( r10.getName() == str.str() );

  ufAssert( r11.getType() == ARM_Base::RegisterType::lo );
  ufAssert( r11.getBitWidth() == 32 );
  ufAssert( r11.isVirtual() );
  ufAssert( !r11.isPhysical() );
  ufAssert( !r11.isInserted() );
  str.str( "" );
  str << ARM_Base::RegisterType::lo.getPrefixes()[ 1 ] << r11.getID()
      << ARM_Base::RegisterType::lo.getSuffixes()[ 1 ];
  ufAssert( r11.getName() == str.str() );

  ufAssert( r12.getType() == ARM_Base::RegisterType::hi );
  ufAssert( r12.getBitWidth() == 32 );
  ufAssert( r12.isVirtual() );
  ufAssert( !r12.isPhysical() );
  ufAssert( !r12.isInserted() );
  str.str( "" );
  str << ARM_Base::RegisterType::hi.getPrefixes()[ 1 ] << r12.getID()
      << ARM_Base::RegisterType::hi.getSuffixes()[ 1 ];
  ufAssert( r12.getName() == str.str() );

  // Test the move assignment operator.
  WIR_VirtualRegister r13( ARM_Base::RegisterType::reg );
  r13 = move( r7 );
  WIR_VirtualRegister r14( ARM_Base::RegisterType::lo );
  r14 = move( r8 );
  WIR_VirtualRegister r15( ARM_Base::RegisterType::hi );
  r15 = move( r9 );

  ufAssert( r13.getType() == ARM_Base::RegisterType::reg );
  ufAssert( r13.getBitWidth() == 32 );
  ufAssert( r13.isVirtual() );
  ufAssert( !r13.isPhysical() );
  ufAssert( !r13.isInserted() );
  str.str( "" );
  str << ARM_Base::RegisterType::reg.getPrefixes()[ 1 ] << r13.getID()
      << ARM_Base::RegisterType::reg.getSuffixes()[ 1 ];
  ufAssert( r13.getName() == str.str() );

  ufAssert( r14.getType() == ARM_Base::RegisterType::lo );
  ufAssert( r14.getBitWidth() == 32 );
  ufAssert( r14.isVirtual() );
  ufAssert( !r14.isPhysical() );
  ufAssert( !r14.isInserted() );
  str.str( "" );
  str << ARM_Base::RegisterType::lo.getPrefixes()[ 1 ] << r14.getID()
      << ARM_Base::RegisterType::lo.getSuffixes()[ 1 ];
  ufAssert( r14.getName() == str.str() );

  ufAssert( r15.getType() == ARM_Base::RegisterType::hi );
  ufAssert( r15.getBitWidth() == 32 );
  ufAssert( r15.isVirtual() );
  ufAssert( !r15.isPhysical() );
  ufAssert( !r15.isInserted() );
  str.str( "" );
  str << ARM_Base::RegisterType::hi.getPrefixes()[ 1 ] << r15.getID()
      << ARM_Base::RegisterType::hi.getSuffixes()[ 1 ];
  ufAssert( r15.getName() == str.str() );

  return( 0 );
}
