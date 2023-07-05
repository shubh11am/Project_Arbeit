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
  TC_ERegV r1;
  ufAssert( r1.getType() == TC131::RegisterType::eReg );
  ufAssert( r1.getBitWidth() == 64 );
  ufAssert( r1.isVirtual() );
  ufAssert( !r1.isPhysical() );
  ufAssert( !r1.isInserted() );
  str.str( "" );
  str << TC131::RegisterType::eReg.getPrefixes()[ 1 ] << r1.getID()
      << TC131::RegisterType::eReg.getSuffixes()[ 1 ];
  ufAssert( r1.getName() == str.str() );

  ufAssert( r1.hasChilds() );
  ufAssert( !r1.isChild() );
  ufAssert( r1.getChilds().size() == 2 );
  ufAssert( r1.getRoot() == r1 );

  WIR_VirtualRegister &c1 = r1.getChilds().front().get();
  WIR_VirtualRegister &c2 = r1.getChilds().back().get();
  ufAssert( c1 != c2 );

  ufAssert( c1.getType() == TC131::RegisterType::dReg );
  ufAssert( c1.getBitWidth() == 32 );
  ufAssert( c1.isVirtual() );
  ufAssert( !c1.isInserted() );
  ufAssert( !c1.hasChilds() );
  ufAssert( c1.isChild() );
  ufAssert( c1.getParent() == r1 );
  ufAssert( c1.getRoot() == r1 );

  ufAssert( c2.getType() == TC131::RegisterType::dReg );
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

  TC_DRegV dummy;
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

  // Test an extended virtual register inserted in some function.
  WIR_Function f( "foo" );
  auto &tr1 = f.pushBackVirtualRegister( TC_ERegV() );
  TC_ERegV &r2 = static_cast<TC_ERegV &>( tr1 );

  ufAssert( r2.getType() == TC131::RegisterType::eReg );
  ufAssert( r2.getBitWidth() == 64 );
  ufAssert( r2.isVirtual() );
  ufAssert( !r2.isPhysical() );
  ufAssert( r2.isInserted() );
  str.str( "" );
  str << TC131::RegisterType::eReg.getPrefixes()[ 1 ] << r2.getID()
      << TC131::RegisterType::eReg.getSuffixes()[ 1 ];
  ufAssert( r2.getName() == str.str() );

  ufAssert( r2.hasChilds() );
  ufAssert( !r2.isChild() );
  ufAssert( r2.getChilds().size() == 2 );
  ufAssert( r2.getRoot() == r2 );

  WIR_VirtualRegister &c3 = r2.getChilds().front().get();
  WIR_VirtualRegister &c4 = r2.getChilds().back().get();
  ufAssert( c3 != c4 );

  ufAssert( c3.getType() == TC131::RegisterType::dReg );
  ufAssert( c3.getBitWidth() == 32 );
  ufAssert( c3.isVirtual() );
  ufAssert( c3.isInserted() );
  ufAssert( !c3.hasChilds() );
  ufAssert( c3.isChild() );
  ufAssert( c3.getParent() == r2 );
  ufAssert( c3.getRoot() == r2 );

  ufAssert( c4.getType() == TC131::RegisterType::dReg );
  ufAssert( c4.getBitWidth() == 32 );
  ufAssert( c4.isVirtual() );
  ufAssert( c4.isInserted() );
  ufAssert( !c4.hasChilds() );
  ufAssert( c4.isChild() );
  ufAssert( c4.getParent() == r2 );
  ufAssert( c4.getRoot() == r2 );

  ufAssert( r2.containsChild( c3.getID() ) );
  ufAssert( r2.containsChild( c3 ) );
  ufAssert( r2.containsChild( c4.getID() ) );
  ufAssert( r2.containsChild( c4 ) );

  ufAssert( !r2.containsChild( c1.getID() ) );
  ufAssert( !r2.containsChild( c2 ) );

  it = r2.findChild( c3.getID() );
  ufAssert( (*it).get() == c3 );
  it = r2.findChild( c3 );
  ufAssert( (*it).get() == c3 );
  it = r2.findChild( c4.getID() );
  ufAssert( (*it).get() == c4 );
  it = r2.findChild( c4 );
  ufAssert( (*it).get() == c4 );
  it = r2.findChild( c1.getID() );
  ufAssert( it == r2.getChilds().end() );
  it = r2.findChild( c2 );
  ufAssert( it == r2.getChilds().end() );

  v = r2.getLeafs();
  ufAssert( v.size() == 2 );
  ufAssert( v[ 0 ].get() == c3 );
  ufAssert( v[ 1 ].get() == c4 );

  v = c3.getLeafs();
  ufAssert( v.size() == 1 );
  ufAssert( v[ 0 ].get() == c3 );
  v = c4.getLeafs();
  ufAssert( v.size() == 1 );
  ufAssert( v[ 0 ].get() == c4 );

  // Test the copy constructor.
  TC_ERegV r3 = r1;
  TC_ERegV r4 = r2;

  ufAssert( r3.getType() == TC131::RegisterType::eReg );
  ufAssert( r3.getBitWidth() == 64 );
  ufAssert( r3.isVirtual() );
  ufAssert( !r3.isPhysical() );
  ufAssert( !r3.isInserted() );
  str.str( "" );
  str << TC131::RegisterType::eReg.getPrefixes()[ 1 ] << r3.getID()
      << TC131::RegisterType::eReg.getSuffixes()[ 1 ];
  ufAssert( r3.getName() == str.str() );

  ufAssert( r3.hasChilds() );
  ufAssert( !r3.isChild() );
  ufAssert( r3.getChilds().size() == 2 );
  ufAssert( r3.getRoot() == r3 );

  WIR_VirtualRegister &c5 = r3.getChilds().front().get();
  WIR_VirtualRegister &c6 = r3.getChilds().back().get();
  ufAssert( c5 != c6 );

  ufAssert( c5.getType() == TC131::RegisterType::dReg );
  ufAssert( c5.getBitWidth() == 32 );
  ufAssert( c5.isVirtual() );
  ufAssert( !c5.isInserted() );
  ufAssert( !c5.hasChilds() );
  ufAssert( c5.isChild() );
  ufAssert( c5.getParent() == r3 );
  ufAssert( c5.getRoot() == r3 );

  ufAssert( c6.getType() == TC131::RegisterType::dReg );
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

  ufAssert( r4.getType() == TC131::RegisterType::eReg );
  ufAssert( r4.getBitWidth() == 64 );
  ufAssert( r4.isVirtual() );
  ufAssert( !r4.isPhysical() );
  ufAssert( !r4.isInserted() );
  str.str( "" );
  str << TC131::RegisterType::eReg.getPrefixes()[ 1 ] << r4.getID()
      << TC131::RegisterType::eReg.getSuffixes()[ 1 ];
  ufAssert( r4.getName() == str.str() );

  ufAssert( r4.hasChilds() );
  ufAssert( !r4.isChild() );
  ufAssert( r4.getChilds().size() == 2 );
  ufAssert( r4.getRoot() == r4 );

  WIR_VirtualRegister &c7 = r4.getChilds().front().get();
  WIR_VirtualRegister &c8 = r4.getChilds().back().get();
  ufAssert( c7 != c8 );

  ufAssert( c7.getType() == TC131::RegisterType::dReg );
  ufAssert( c7.getBitWidth() == 32 );
  ufAssert( c7.isVirtual() );
  ufAssert( !c7.isInserted() );
  ufAssert( !c7.hasChilds() );
  ufAssert( c7.isChild() );
  ufAssert( c7.getParent() == r4 );
  ufAssert( c7.getRoot() == r4 );

  ufAssert( c8.getType() == TC131::RegisterType::dReg );
  ufAssert( c8.getBitWidth() == 32 );
  ufAssert( c8.isVirtual() );
  ufAssert( !c8.isInserted() );
  ufAssert( !c8.hasChilds() );
  ufAssert( c8.isChild() );
  ufAssert( c8.getParent() == r4 );
  ufAssert( c8.getRoot() == r4 );

  ufAssert( r4.containsChild( c7.getID() ) );
  ufAssert( r4.containsChild( c7 ) );
  ufAssert( r4.containsChild( c8.getID() ) );
  ufAssert( r4.containsChild( c8 ) );

  ufAssert( !r4.containsChild( c3.getID() ) );
  ufAssert( !r4.containsChild( c4 ) );

  it = r4.findChild( c7.getID() );
  ufAssert( (*it).get() == c7 );
  it = r4.findChild( c7 );
  ufAssert( (*it).get() == c7 );
  it = r4.findChild( c8.getID() );
  ufAssert( (*it).get() == c8 );
  it = r4.findChild( c8 );
  ufAssert( (*it).get() == c8 );
  it = r4.findChild( c3.getID() );
  ufAssert( it == r4.getChilds().end() );
  it = r4.findChild( c4 );
  ufAssert( it == r4.getChilds().end() );

  v = r4.getLeafs();
  ufAssert( v.size() == 2 );
  ufAssert( v[ 0 ].get() == c7 );
  ufAssert( v[ 1 ].get() == c8 );

  v = c7.getLeafs();
  ufAssert( v.size() == 1 );
  ufAssert( v[ 0 ].get() == c7 );
  v = c8.getLeafs();
  ufAssert( v.size() == 1 );
  ufAssert( v[ 0 ].get() == c8 );

  // Test the copy assignment operator.
  TC_ERegV r5, r6;
  r5 = r1;
  r6 = r2;

  ufAssert( r5.getType() == TC131::RegisterType::eReg );
  ufAssert( r5.getBitWidth() == 64 );
  ufAssert( r5.isVirtual() );
  ufAssert( !r5.isPhysical() );
  ufAssert( !r5.isInserted() );
  str.str( "" );
  str << TC131::RegisterType::eReg.getPrefixes()[ 1 ] << r5.getID()
      << TC131::RegisterType::eReg.getSuffixes()[ 1 ];
  ufAssert( r5.getName() == str.str() );

  ufAssert( r5.hasChilds() );
  ufAssert( !r5.isChild() );
  ufAssert( r5.getChilds().size() == 2 );
  ufAssert( r5.getRoot() == r5 );

  WIR_VirtualRegister &c9 = r5.getChilds().front().get();
  WIR_VirtualRegister &c10 = r5.getChilds().back().get();
  ufAssert( c9 != c10 );

  ufAssert( c9.getType() == TC131::RegisterType::dReg );
  ufAssert( c9.getBitWidth() == 32 );
  ufAssert( c9.isVirtual() );
  ufAssert( !c9.isInserted() );
  ufAssert( !c9.hasChilds() );
  ufAssert( c9.isChild() );
  ufAssert( c9.getParent() == r5 );
  ufAssert( c9.getRoot() == r5 );

  ufAssert( c10.getType() == TC131::RegisterType::dReg );
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

  ufAssert( r6.getType() == TC131::RegisterType::eReg );
  ufAssert( r6.getBitWidth() == 64 );
  ufAssert( r6.isVirtual() );
  ufAssert( !r6.isPhysical() );
  ufAssert( !r6.isInserted() );
  str.str( "" );
  str << TC131::RegisterType::eReg.getPrefixes()[ 1 ] << r6.getID()
      << TC131::RegisterType::eReg.getSuffixes()[ 1 ];
  ufAssert( r6.getName() == str.str() );

  ufAssert( r6.hasChilds() );
  ufAssert( !r6.isChild() );
  ufAssert( r6.getChilds().size() == 2 );
  ufAssert( r6.getRoot() == r6 );

  WIR_VirtualRegister &c11 = r6.getChilds().front().get();
  WIR_VirtualRegister &c12 = r6.getChilds().back().get();
  ufAssert( c11 != c12 );

  ufAssert( c11.getType() == TC131::RegisterType::dReg );
  ufAssert( c11.getBitWidth() == 32 );
  ufAssert( c11.isVirtual() );
  ufAssert( !c11.isInserted() );
  ufAssert( !c11.hasChilds() );
  ufAssert( c11.isChild() );
  ufAssert( c11.getParent() == r6 );
  ufAssert( c11.getRoot() == r6 );

  ufAssert( c12.getType() == TC131::RegisterType::dReg );
  ufAssert( c12.getBitWidth() == 32 );
  ufAssert( c12.isVirtual() );
  ufAssert( !c12.isInserted() );
  ufAssert( !c12.hasChilds() );
  ufAssert( c12.isChild() );
  ufAssert( c12.getParent() == r6 );
  ufAssert( c12.getRoot() == r6 );

  ufAssert( r6.containsChild( c11.getID() ) );
  ufAssert( r6.containsChild( c11 ) );
  ufAssert( r6.containsChild( c12.getID() ) );
  ufAssert( r6.containsChild( c12 ) );

  ufAssert( !r6.containsChild( c3.getID() ) );
  ufAssert( !r6.containsChild( c4 ) );

  it = r6.findChild( c11.getID() );
  ufAssert( (*it).get() == c11 );
  it = r6.findChild( c11 );
  ufAssert( (*it).get() == c11 );
  it = r6.findChild( c12.getID() );
  ufAssert( (*it).get() == c12 );
  it = r6.findChild( c12 );
  ufAssert( (*it).get() == c12 );
  it = r6.findChild( c3.getID() );
  ufAssert( it == r6.getChilds().end() );
  it = r6.findChild( c4 );
  ufAssert( it == r6.getChilds().end() );

  v = r6.getLeafs();
  ufAssert( v.size() == 2 );
  ufAssert( v[ 0 ].get() == c11 );
  ufAssert( v[ 1 ].get() == c12 );

  v = c11.getLeafs();
  ufAssert( v.size() == 1 );
  ufAssert( v[ 0 ].get() == c11 );
  v = c12.getLeafs();
  ufAssert( v.size() == 1 );
  ufAssert( v[ 0 ].get() == c12 );

  // Test the move constructor.
  TC_ERegV r7 = move( r1 );

  ufAssert( r7.getType() == TC131::RegisterType::eReg );
  ufAssert( r7.getBitWidth() == 64 );
  ufAssert( r7.isVirtual() );
  ufAssert( !r7.isPhysical() );
  ufAssert( !r7.isInserted() );
  str.str( "" );
  str << TC131::RegisterType::eReg.getPrefixes()[ 1 ] << r7.getID()
      << TC131::RegisterType::eReg.getSuffixes()[ 1 ];
  ufAssert( r7.getName() == str.str() );

  ufAssert( r7.hasChilds() );
  ufAssert( !r7.isChild() );
  ufAssert( r7.getChilds().size() == 2 );
  ufAssert( r7.getRoot() == r7 );

  ufAssert( !r1.hasChilds() );

  WIR_VirtualRegister &c13 = r7.getChilds().front().get();
  WIR_VirtualRegister &c14 = r7.getChilds().back().get();
  ufAssert( c13 != c14 );

  ufAssert( c13.getType() == TC131::RegisterType::dReg );
  ufAssert( c13.getBitWidth() == 32 );
  ufAssert( c13.isVirtual() );
  ufAssert( !c13.isInserted() );
  ufAssert( !c13.hasChilds() );
  ufAssert( c13.isChild() );
  ufAssert( c13.getParent() == r7 );
  ufAssert( c13.getRoot() == r7 );

  ufAssert( c14.getType() == TC131::RegisterType::dReg );
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

  ufAssert( !r7.containsChild( c3.getID() ) );
  ufAssert( !r7.containsChild( c4 ) );

  it = r7.findChild( c13.getID() );
  ufAssert( (*it).get() == c13 );
  it = r7.findChild( c13 );
  ufAssert( (*it).get() == c13 );
  it = r7.findChild( c14.getID() );
  ufAssert( (*it).get() == c14 );
  it = r7.findChild( c14 );
  ufAssert( (*it).get() == c14 );
  it = r7.findChild( c3.getID() );
  ufAssert( it == r7.getChilds().end() );
  it = r7.findChild( c4 );
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
  TC_ERegV r8;
  r8 = move( r6 );

  ufAssert( r8.getType() == TC131::RegisterType::eReg );
  ufAssert( r8.getBitWidth() == 64 );
  ufAssert( r8.isVirtual() );
  ufAssert( !r8.isPhysical() );
  ufAssert( !r8.isInserted() );
  str.str( "" );
  str << TC131::RegisterType::eReg.getPrefixes()[ 1 ] << r8.getID()
      << TC131::RegisterType::eReg.getSuffixes()[ 1 ];
  ufAssert( r8.getName() == str.str() );

  ufAssert( r8.hasChilds() );
  ufAssert( !r8.isChild() );
  ufAssert( r8.getChilds().size() == 2 );
  ufAssert( r8.getRoot() == r8 );

  ufAssert( !r6.hasChilds() );

  WIR_VirtualRegister &c15 = r8.getChilds().front().get();
  WIR_VirtualRegister &c16 = r8.getChilds().back().get();
  ufAssert( c15 != c16 );

  ufAssert( c15.getType() == TC131::RegisterType::dReg );
  ufAssert( c15.getBitWidth() == 32 );
  ufAssert( c15.isVirtual() );
  ufAssert( !c15.isInserted() );
  ufAssert( !c15.hasChilds() );
  ufAssert( c15.isChild() );
  ufAssert( c15.getParent() == r8 );
  ufAssert( c15.getRoot() == r8 );

  ufAssert( c16.getType() == TC131::RegisterType::dReg );
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

  ufAssert( !r8.containsChild( c3.getID() ) );
  ufAssert( !r8.containsChild( c4 ) );

  it = r8.findChild( c15.getID() );
  ufAssert( (*it).get() == c15 );
  it = r8.findChild( c15 );
  ufAssert( (*it).get() == c15 );
  it = r8.findChild( c16.getID() );
  ufAssert( (*it).get() == c16 );
  it = r8.findChild( c16 );
  ufAssert( (*it).get() == c16 );
  it = r8.findChild( c3.getID() );
  ufAssert( it == r8.getChilds().end() );
  it = r8.findChild( c4 );
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
