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
#include <vector>

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
  vector<WIR_id_t> IDs;
  vector<WIR_VirtualRegister *> regs;

  // Check insertion and removal from WIR pointer lists.
  WIR_Function f( "foo" );
  WIR_VirtualRegister r1( MIPS::RegisterType::reg );
  WIR_VirtualRegister r2( MIPS::RegisterType::reg );
  WIR_VirtualRegister r3( MIPS::RegisterType::reg );

  // Check the different variants of pushBack and pushFront.
  f.pushBackVirtualRegister( r1 );
  f.pushBackVirtualRegister( WIR_VirtualRegister( MIPS::RegisterType::reg ) );
  f.pushFrontVirtualRegister( r2 );
  f.pushFrontVirtualRegister( WIR_VirtualRegister( MIPS::RegisterType::reg ) );

  // Check the different variants of insert.
  auto it1 = f.getVirtualRegisters().begin();
  it1++;
  f.insertVirtualRegister( it1, r3 );
  it1 = f.getVirtualRegisters().end();
  it1--;
  it1--;
  f.insertVirtualRegister(
    it1, WIR_VirtualRegister( MIPS::RegisterType::reg ) );

  // Check the generated list.
  ufAssert( f.getVirtualRegisters().size() == 6 );
  it1 = f.getVirtualRegisters().begin();
  regs.push_back( &(it1->get() ) );
  IDs.push_back( (*it1++).get().getID() );
  regs.push_back( &(it1->get() ) );
  IDs.push_back( (*it1++).get().getID() );
  regs.push_back( &(it1->get() ) );
  IDs.push_back( (*it1++).get().getID() );
  regs.push_back( &(it1->get() ) );
  IDs.push_back( (*it1++).get().getID() );
  regs.push_back( &(it1->get() ) );
  IDs.push_back( (*it1++).get().getID() );
  regs.push_back( &(it1->get() ) );
  IDs.push_back( (*it1).get().getID() );

  // Check containsVirtualRegister by ID.
  ufAssert( f.containsVirtualRegister( IDs[ 0 ] ) );
  ufAssert( f.containsVirtualRegister( IDs[ 1 ] ) );
  ufAssert( f.containsVirtualRegister( IDs[ 2 ] ) );
  ufAssert( f.containsVirtualRegister( IDs[ 3 ] ) );
  ufAssert( f.containsVirtualRegister( IDs[ 4 ] ) );
  ufAssert( f.containsVirtualRegister( IDs[ 5 ] ) );

  ufAssert( !f.containsVirtualRegister( 0 ) );
  ufAssert( !f.containsVirtualRegister( 1 ) );
  ufAssert( !f.containsVirtualRegister( 2 ) );
  ufAssert( !f.containsVirtualRegister( 3 ) );
  ufAssert( !f.containsVirtualRegister( 5 ) );
  ufAssert( !f.containsVirtualRegister( 8 ) );
  ufAssert( !f.containsVirtualRegister( 11 ) );
  ufAssert( !f.containsVirtualRegister( 130 ) );
  ufAssert( !f.containsVirtualRegister( 0x815 ) );

  it1 = f.getVirtualRegisters().end();
  it1--;
  it1--;
  it1--;
  f.eraseVirtualRegister( it1 );
  ufAssert( !f.containsVirtualRegister( IDs[ 3 ] ) );

  // Check containsVirtualRegister by reference.
  WIR_VirtualRegister &ref1 = *(regs[ 0 ]);
  WIR_VirtualRegister &ref2 = *(regs[ 1 ]);
  WIR_VirtualRegister &ref3 = *(regs[ 2 ]);
  WIR_VirtualRegister &ref4 = *(regs[ 4 ]);
  WIR_VirtualRegister &ref5 = *(regs[ 5 ]);
  ufAssert( f.containsVirtualRegister( ref1 ) );
  ufAssert( f.containsVirtualRegister( ref2 ) );
  ufAssert( f.containsVirtualRegister( ref3 ) );
  ufAssert( f.containsVirtualRegister( ref4 ) );
  ufAssert( f.containsVirtualRegister( ref5 ) );
  ufAssert( !f.containsVirtualRegister( r1 ) );
  ufAssert( !f.containsVirtualRegister( r2 ) );
  ufAssert( !f.containsVirtualRegister( r3 ) );

  // Check findVirtualRegister by ID.
  it1 = f.findVirtualRegister( IDs[ 0 ] );
  ufAssert( (*it1).get().getID() == IDs[ 0 ] );
  it1 = f.findVirtualRegister( IDs[ 1 ] );
  ufAssert( (*it1).get().getID() == IDs[ 1 ] );
  it1 = f.findVirtualRegister( IDs[ 2 ] );
  ufAssert( (*it1).get().getID() == IDs[ 2 ] );
  it1 = f.findVirtualRegister( IDs[ 4 ] );
  ufAssert( (*it1).get().getID() == IDs[ 4 ] );
  ufAssert( (*f.findVirtualRegister( IDs[ 5 ] ) ).get().getID() == IDs[ 5 ] );
  ufAssert( f.findVirtualRegister( 0 ) == f.getVirtualRegisters().end() );
  ufAssert( f.findVirtualRegister( 1 ) == f.getVirtualRegisters().end() );
  ufAssert( f.findVirtualRegister( 2 ) == f.getVirtualRegisters().end() );
  ufAssert( f.findVirtualRegister( 3 ) == f.getVirtualRegisters().end() );
  ufAssert( f.findVirtualRegister( 5 ) == f.getVirtualRegisters().end() );
  ufAssert( f.findVirtualRegister( 8 ) == f.getVirtualRegisters().end() );
  ufAssert( f.findVirtualRegister( 11 ) == f.getVirtualRegisters().end() );
  ufAssert(
    f.findVirtualRegister( IDs[ 3 ] ) == f.getVirtualRegisters().end() );

  // Check findVirtualRegister by reference.
  ufAssert( f.findVirtualRegister( ref1 ) != f.getVirtualRegisters().end() );
  ufAssert( f.findVirtualRegister( ref2 ) != f.getVirtualRegisters().end() );
  ufAssert( f.findVirtualRegister( ref3 ) != f.getVirtualRegisters().end() );
  ufAssert( f.findVirtualRegister( ref4 ) != f.getVirtualRegisters().end() );
  ufAssert( f.findVirtualRegister( ref5 ) != f.getVirtualRegisters().end() );
  ufAssert( f.findVirtualRegister( r1 ) == f.getVirtualRegisters().end() );
  ufAssert( f.findVirtualRegister( r2 ) == f.getVirtualRegisters().end() );
  ufAssert( f.findVirtualRegister( r3 ) == f.getVirtualRegisters().end() );

  return( 0 );
}
