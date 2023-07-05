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

  // Test an extended virtual register that is not inserted in some function.
  // We only perform a limited set of tests here, since most of the
  // functionality of extended TriCore registers is already covered by test case
  // tcEReg1.
  TC_PRegV r1;
  ufAssert( r1.getType() == TC131::RegisterType::pReg );
  ufAssert( r1.getBitWidth() == 64 );
  ufAssert( r1.isVirtual() );
  ufAssert( !r1.isPhysical() );
  ufAssert( !r1.isInserted() );
  str.str( "" );
  str << TC131::RegisterType::pReg.getPrefixes()[ 1 ] << r1.getID()
      << TC131::RegisterType::pReg.getSuffixes()[ 1 ];
  ufAssert( r1.getName() == str.str() );

  ufAssert( r1.hasChilds() );
  ufAssert( !r1.isChild() );
  ufAssert( r1.getChilds().size() == 2 );
  ufAssert( r1.getRoot() == r1 );

  WIR_VirtualRegister &c1 = r1.getChilds().front().get();
  WIR_VirtualRegister &c2 = r1.getChilds().back().get();
  ufAssert( c1 != c2 );

  ufAssert( c1.getType() == TC131::RegisterType::aReg );
  ufAssert( c1.getBitWidth() == 32 );
  ufAssert( c1.isVirtual() );
  ufAssert( !c1.isInserted() );
  ufAssert( !c1.hasChilds() );
  ufAssert( c1.isChild() );
  ufAssert( c1.getParent() == r1 );
  ufAssert( c1.getRoot() == r1 );

  ufAssert( c2.getType() == TC131::RegisterType::aReg );
  ufAssert( c2.getBitWidth() == 32 );
  ufAssert( c2.isVirtual() );
  ufAssert( !c2.isInserted() );
  ufAssert( !c2.hasChilds() );
  ufAssert( c2.isChild() );
  ufAssert( c2.getParent() == r1 );
  ufAssert( c2.getRoot() == r1 );

  ufAssert( r1.containsChild( c1.getID() ) );
  ufAssert( r1.containsChild( c1 ) );
  ufAssert( r1.containsChild( c2.getID() ) );
  ufAssert( r1.containsChild( c2 ) );

  TC_ARegV dummy;
  ufAssert( !r1.containsChild( dummy.getID() ) );
  ufAssert( !r1.containsChild( dummy ) );

  auto it = r1.findChild( c1.getID() );
  ufAssert( (*it).get() == c1 );
  it = r1.findChild( c1 );
  ufAssert( (*it).get() == c1 );
  it = r1.findChild( c2.getID() );
  ufAssert( (*it).get() == c2 );
  it = r1.findChild( c2 );
  ufAssert( (*it).get() == c2 );
  it = r1.findChild( dummy.getID() );
  ufAssert( it == r1.getChilds().end() );
  it = r1.findChild( dummy );
  ufAssert( it == r1.getChilds().end() );

  auto v = r1.getLeafs();
  ufAssert( v.size() == 2 );
  ufAssert( v[ 0 ].get() == c1 );
  ufAssert( v[ 1 ].get() == c2 );

  v = c1.getLeafs();
  ufAssert( v.size() == 1 );
  ufAssert( v[ 0 ].get() == c1 );
  v = c2.getLeafs();
  ufAssert( v.size() == 1 );
  ufAssert( v[ 0 ].get() == c2 );

  // Test the copy constructor.
  TC_PRegV r3 = r1;

  ufAssert( r3.getType() == TC131::RegisterType::pReg );
  ufAssert( r3.getBitWidth() == 64 );
  ufAssert( r3.isVirtual() );
  ufAssert( !r3.isPhysical() );
  ufAssert( !r3.isInserted() );
  str.str( "" );
  str << TC131::RegisterType::pReg.getPrefixes()[ 1 ] << r3.getID()
      << TC131::RegisterType::pReg.getSuffixes()[ 1 ];
  ufAssert( r3.getName() == str.str() );

  ufAssert( r3.hasChilds() );
  ufAssert( !r3.isChild() );
  ufAssert( r3.getChilds().size() == 2 );
  ufAssert( r3.getRoot() == r3 );

  WIR_VirtualRegister &c5 = r3.getChilds().front().get();
  WIR_VirtualRegister &c6 = r3.getChilds().back().get();
  ufAssert( c5 != c6 );

  ufAssert( c5.getType() == TC131::RegisterType::aReg );
  ufAssert( c5.getBitWidth() == 32 );
  ufAssert( c5.isVirtual() );
  ufAssert( !c5.isInserted() );
  ufAssert( !c5.hasChilds() );
  ufAssert( c5.isChild() );
  ufAssert( c5.getParent() == r3 );
  ufAssert( c5.getRoot() == r3 );

  ufAssert( c6.getType() == TC131::RegisterType::aReg );
  ufAssert( c6.getBitWidth() == 32 );
  ufAssert( c6.isVirtual() );
  ufAssert( !c6.isInserted() );
  ufAssert( !c6.hasChilds() );
  ufAssert( c6.isChild() );
  ufAssert( c6.getParent() == r3 );
  ufAssert( c6.getRoot() == r3 );

  ufAssert( r3.containsChild( c5.getID() ) );
  ufAssert( r3.containsChild( c5 ) );
  ufAssert( r3.containsChild( c6.getID() ) );
  ufAssert( r3.containsChild( c6 ) );

  ufAssert( !r3.containsChild( c1.getID() ) );
  ufAssert( !r3.containsChild( c2 ) );

  it = r3.findChild( c5.getID() );
  ufAssert( (*it).get() == c5 );
  it = r3.findChild( c5 );
  ufAssert( (*it).get() == c5 );
  it = r3.findChild( c6.getID() );
  ufAssert( (*it).get() == c6 );
  it = r3.findChild( c6 );
  ufAssert( (*it).get() == c6 );
  it = r3.findChild( c1.getID() );
  ufAssert( it == r3.getChilds().end() );
  it = r3.findChild( c2 );
  ufAssert( it == r3.getChilds().end() );

  v = r3.getLeafs();
  ufAssert( v.size() == 2 );
  ufAssert( v[ 0 ].get() == c5 );
  ufAssert( v[ 1 ].get() == c6 );

  v = c5.getLeafs();
  ufAssert( v.size() == 1 );
  ufAssert( v[ 0 ].get() == c5 );
  v = c6.getLeafs();
  ufAssert( v.size() == 1 );
  ufAssert( v[ 0 ].get() == c6 );

  // Test the copy assignment operator.
  TC_PRegV r5;
  r5 = r1;

  ufAssert( r5.getType() == TC131::RegisterType::pReg );
  ufAssert( r5.getBitWidth() == 64 );
  ufAssert( r5.isVirtual() );
  ufAssert( !r5.isPhysical() );
  ufAssert( !r5.isInserted() );
  str.str( "" );
  str << TC131::RegisterType::pReg.getPrefixes()[ 1 ] << r5.getID()
      << TC131::RegisterType::pReg.getSuffixes()[ 1 ];
  ufAssert( r5.getName() == str.str() );

  ufAssert( r5.hasChilds() );
  ufAssert( !r5.isChild() );
  ufAssert( r5.getChilds().size() == 2 );
  ufAssert( r5.getRoot() == r5 );

  WIR_VirtualRegister &c9 = r5.getChilds().front().get();
  WIR_VirtualRegister &c10 = r5.getChilds().back().get();
  ufAssert( c9 != c10 );

  ufAssert( c9.getType() == TC131::RegisterType::aReg );
  ufAssert( c9.getBitWidth() == 32 );
  ufAssert( c9.isVirtual() );
  ufAssert( !c9.isInserted() );
  ufAssert( !c9.hasChilds() );
  ufAssert( c9.isChild() );
  ufAssert( c9.getParent() == r5 );
  ufAssert( c9.getRoot() == r5 );

  ufAssert( c10.getType() == TC131::RegisterType::aReg );
  ufAssert( c10.getBitWidth() == 32 );
  ufAssert( c10.isVirtual() );
  ufAssert( !c10.isInserted() );
  ufAssert( !c10.hasChilds() );
  ufAssert( c10.isChild() );
  ufAssert( c10.getParent() == r5 );
  ufAssert( c10.getRoot() == r5 );

  ufAssert( r5.containsChild( c9.getID() ) );
  ufAssert( r5.containsChild( c9 ) );
  ufAssert( r5.containsChild( c10.getID() ) );
  ufAssert( r5.containsChild( c10 ) );

  ufAssert( !r5.containsChild( c1.getID() ) );
  ufAssert( !r5.containsChild( c2 ) );

  it = r5.findChild( c9.getID() );
  ufAssert( (*it).get() == c9 );
  it = r5.findChild( c9 );
  ufAssert( (*it).get() == c9 );
  it = r5.findChild( c10.getID() );
  ufAssert( (*it).get() == c10 );
  it = r5.findChild( c10 );
  ufAssert( (*it).get() == c10 );
  it = r5.findChild( c1.getID() );
  ufAssert( it == r5.getChilds().end() );
  it = r5.findChild( c2 );
  ufAssert( it == r5.getChilds().end() );

  v = r5.getLeafs();
  ufAssert( v.size() == 2 );
  ufAssert( v[ 0 ].get() == c9 );
  ufAssert( v[ 1 ].get() == c10 );

  v = c9.getLeafs();
  ufAssert( v.size() == 1 );
  ufAssert( v[ 0 ].get() == c9 );
  v = c10.getLeafs();
  ufAssert( v.size() == 1 );
  ufAssert( v[ 0 ].get() == c10 );

  // Test the move constructor.
  TC_PRegV r7 = move( r1 );

  ufAssert( r7.getType() == TC131::RegisterType::pReg );
  ufAssert( r7.getBitWidth() == 64 );
  ufAssert( r7.isVirtual() );
  ufAssert( !r7.isPhysical() );
  ufAssert( !r7.isInserted() );
  str.str( "" );
  str << TC131::RegisterType::pReg.getPrefixes()[ 1 ] << r7.getID()
      << TC131::RegisterType::pReg.getSuffixes()[ 1 ];
  ufAssert( r7.getName() == str.str() );

  ufAssert( r7.hasChilds() );
  ufAssert( !r7.isChild() );
  ufAssert( r7.getChilds().size() == 2 );
  ufAssert( r7.getRoot() == r7 );

  ufAssert( !r1.hasChilds() );

  WIR_VirtualRegister &c13 = r7.getChilds().front().get();
  WIR_VirtualRegister &c14 = r7.getChilds().back().get();
  ufAssert( c13 != c14 );

  ufAssert( c13.getType() == TC131::RegisterType::aReg );
  ufAssert( c13.getBitWidth() == 32 );
  ufAssert( c13.isVirtual() );
  ufAssert( !c13.isInserted() );
  ufAssert( !c13.hasChilds() );
  ufAssert( c13.isChild() );
  ufAssert( c13.getParent() == r7 );
  ufAssert( c13.getRoot() == r7 );

  ufAssert( c14.getType() == TC131::RegisterType::aReg );
  ufAssert( c14.getBitWidth() == 32 );
  ufAssert( c14.isVirtual() );
  ufAssert( !c14.isInserted() );
  ufAssert( !c14.hasChilds() );
  ufAssert( c14.isChild() );
  ufAssert( c14.getParent() == r7 );
  ufAssert( c14.getRoot() == r7 );

  ufAssert( r7.containsChild( c13.getID() ) );
  ufAssert( r7.containsChild( c13 ) );
  ufAssert( r7.containsChild( c14.getID() ) );
  ufAssert( r7.containsChild( c14 ) );

  ufAssert( !r7.containsChild( c5.getID() ) );
  ufAssert( !r7.containsChild( c6 ) );

  it = r7.findChild( c13.getID() );
  ufAssert( (*it).get() == c13 );
  it = r7.findChild( c13 );
  ufAssert( (*it).get() == c13 );
  it = r7.findChild( c14.getID() );
  ufAssert( (*it).get() == c14 );
  it = r7.findChild( c14 );
  ufAssert( (*it).get() == c14 );
  it = r7.findChild( c5.getID() );
  ufAssert( it == r7.getChilds().end() );
  it = r7.findChild( c6 );
  ufAssert( it == r7.getChilds().end() );

  v = r7.getLeafs();
  ufAssert( v.size() == 2 );
  ufAssert( v[ 0 ].get() == c13 );
  ufAssert( v[ 1 ].get() == c14 );

  v = c13.getLeafs();
  ufAssert( v.size() == 1 );
  ufAssert( v[ 0 ].get() == c13 );
  v = c14.getLeafs();
  ufAssert( v.size() == 1 );
  ufAssert( v[ 0 ].get() == c14 );

  // Test the move assignment operator.
  TC_PRegV r8;
  r8 = move( r3 );

  ufAssert( r8.getType() == TC131::RegisterType::pReg );
  ufAssert( r8.getBitWidth() == 64 );
  ufAssert( r8.isVirtual() );
  ufAssert( !r8.isPhysical() );
  ufAssert( !r8.isInserted() );
  str.str( "" );
  str << TC131::RegisterType::pReg.getPrefixes()[ 1 ] << r8.getID()
      << TC131::RegisterType::pReg.getSuffixes()[ 1 ];
  ufAssert( r8.getName() == str.str() );

  ufAssert( r8.hasChilds() );
  ufAssert( !r8.isChild() );
  ufAssert( r8.getChilds().size() == 2 );
  ufAssert( r8.getRoot() == r8 );

  ufAssert( !r3.hasChilds() );

  WIR_VirtualRegister &c15 = r8.getChilds().front().get();
  WIR_VirtualRegister &c16 = r8.getChilds().back().get();
  ufAssert( c15 != c16 );

  ufAssert( c15.getType() == TC131::RegisterType::aReg );
  ufAssert( c15.getBitWidth() == 32 );
  ufAssert( c15.isVirtual() );
  ufAssert( !c15.isInserted() );
  ufAssert( !c15.hasChilds() );
  ufAssert( c15.isChild() );
  ufAssert( c15.getParent() == r8 );
  ufAssert( c15.getRoot() == r8 );

  ufAssert( c16.getType() == TC131::RegisterType::aReg );
  ufAssert( c16.getBitWidth() == 32 );
  ufAssert( c16.isVirtual() );
  ufAssert( !c16.isInserted() );
  ufAssert( !c16.hasChilds() );
  ufAssert( c16.isChild() );
  ufAssert( c16.getParent() == r8 );
  ufAssert( c16.getRoot() == r8 );

  ufAssert( r8.containsChild( c15.getID() ) );
  ufAssert( r8.containsChild( c15 ) );
  ufAssert( r8.containsChild( c16.getID() ) );
  ufAssert( r8.containsChild( c16 ) );

  ufAssert( !r8.containsChild( c1.getID() ) );
  ufAssert( !r8.containsChild( c2 ) );

  it = r8.findChild( c15.getID() );
  ufAssert( (*it).get() == c15 );
  it = r8.findChild( c15 );
  ufAssert( (*it).get() == c15 );
  it = r8.findChild( c16.getID() );
  ufAssert( (*it).get() == c16 );
  it = r8.findChild( c16 );
  ufAssert( (*it).get() == c16 );
  it = r8.findChild( c1.getID() );
  ufAssert( it == r8.getChilds().end() );
  it = r8.findChild( c2 );
  ufAssert( it == r8.getChilds().end() );

  v = r8.getLeafs();
  ufAssert( v.size() == 2 );
  ufAssert( v[ 0 ].get() == c15 );
  ufAssert( v[ 1 ].get() == c16 );

  v = c15.getLeafs();
  ufAssert( v.size() == 1 );
  ufAssert( v[ 0 ].get() == c15 );
  v = c16.getLeafs();
  ufAssert( v.size() == 1 );
  ufAssert( v[ 0 ].get() == c16 );

  return( 0 );
}
