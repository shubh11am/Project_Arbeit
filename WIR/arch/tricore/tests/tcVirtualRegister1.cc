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
#include <arch/tricore/tc131.h>


using namespace std;
using namespace WIR;


int main( void )
{
  WIR_Init();

  stringstream str;

  WIR_VirtualRegister r1( TC131::RegisterType::aReg );
  ufAssert( r1.getType() == TC131::RegisterType::aReg );
  ufAssert( r1.getBitWidth() == 32 );
  ufAssert( r1.isVirtual() );
  ufAssert( !r1.isPhysical() );
  ufAssert( !r1.isInserted() );
  str << TC131::RegisterType::aReg.getPrefixes()[ 1 ] << r1.getID()
      << TC131::RegisterType::aReg.getSuffixes()[ 1 ];
  ufAssert( r1.getName() == str.str() );

  WIR_Function f( "foo" );
  WIR_VirtualRegister &r2 =
    f.pushBackVirtualRegister(
      WIR_VirtualRegister( TC131::RegisterType::dReg ) );
  ufAssert( r2.getType() == TC131::RegisterType::dReg );
  ufAssert( r2.getBitWidth() == 32 );
  ufAssert( r2.isVirtual() );
  ufAssert( !r2.isPhysical() );
  ufAssert( r2.isInserted() );
  str.str( "" );
  str << TC131::RegisterType::dReg.getPrefixes()[ 1 ] << r2.getID()
      << TC131::RegisterType::dReg.getSuffixes()[ 1 ];
  ufAssert( r2.getName() == str.str() );
  ufAssert( r2.getFunction().getID() == f.getID() );

  // Test the copy constructor.
  WIR_VirtualRegister r3 = r1;
  WIR_VirtualRegister r4 = r2;

  ufAssert( r3.getType() == TC131::RegisterType::aReg );
  ufAssert( r3.getBitWidth() == 32 );
  ufAssert( r3.isVirtual() );
  ufAssert( !r3.isPhysical() );
  ufAssert( !r3.isInserted() );
  str.str( "" );
  str << TC131::RegisterType::aReg.getPrefixes()[ 1 ] << r3.getID()
      << TC131::RegisterType::aReg.getSuffixes()[ 1 ];
  ufAssert( r3.getName() == str.str() );

  ufAssert( r4.getType() == TC131::RegisterType::dReg );
  ufAssert( r4.getBitWidth() == 32 );
  ufAssert( r4.isVirtual() );
  ufAssert( !r4.isPhysical() );
  ufAssert( !r4.isInserted() );
  str.str( "" );
  str << TC131::RegisterType::dReg.getPrefixes()[ 1 ] << r4.getID()
      << TC131::RegisterType::dReg.getSuffixes()[ 1 ];
  ufAssert( r4.getName() == str.str() );

  // Test the copy assignment operator.
  WIR_VirtualRegister r5( TC131::RegisterType::eReg ),
                      r6( TC131::RegisterType::eReg );
  r5 = r1;
  r6 = r2;

  ufAssert( r5.getType() == TC131::RegisterType::aReg );
  ufAssert( r5.getBitWidth() == 32 );
  ufAssert( r5.isVirtual() );
  ufAssert( !r5.isPhysical() );
  ufAssert( !r5.isInserted() );
  str.str( "" );
  str << TC131::RegisterType::aReg.getPrefixes()[ 1 ] << r5.getID()
      << TC131::RegisterType::aReg.getSuffixes()[ 1 ];
  ufAssert( r5.getName() == str.str() );

  ufAssert( r6.getType() == TC131::RegisterType::dReg );
  ufAssert( r6.getBitWidth() == 32 );
  ufAssert( r6.isVirtual() );
  ufAssert( !r6.isPhysical() );
  ufAssert( !r6.isInserted() );
  str.str( "" );
  str << TC131::RegisterType::dReg.getPrefixes()[ 1 ] << r6.getID()
      << TC131::RegisterType::dReg.getSuffixes()[ 1 ];
  ufAssert( r6.getName() == str.str() );

  // Test the move constructor.
  WIR_VirtualRegister r7 = move( r1 );

  ufAssert( r7.getType() == TC131::RegisterType::aReg );
  ufAssert( r7.getBitWidth() == 32 );
  ufAssert( r7.isVirtual() );
  ufAssert( !r7.isPhysical() );
  ufAssert( !r7.isInserted() );
  str.str( "" );
  str << TC131::RegisterType::aReg.getPrefixes()[ 1 ] << r7.getID()
      << TC131::RegisterType::aReg.getSuffixes()[ 1 ];
  ufAssert( r7.getName() == str.str() );

  // Test the move assignment operator.
  WIR_VirtualRegister r8( TC131::RegisterType::eReg );
  r8 = move( r6 );

  ufAssert( r8.getType() == TC131::RegisterType::dReg );
  ufAssert( r8.getBitWidth() == 32 );
  ufAssert( r8.isVirtual() );
  ufAssert( !r8.isPhysical() );
  ufAssert( !r8.isInserted() );
  str.str( "" );
  str << TC131::RegisterType::dReg.getPrefixes()[ 1 ] << r8.getID()
      << TC131::RegisterType::dReg.getSuffixes()[ 1 ];
  ufAssert( r8.getName() == str.str() );

  return( 0 );
}
