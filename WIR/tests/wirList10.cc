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
  vector<WIR_id_t> IDs;
  WIR_id_t tmp;

  // Check insertion and removal from WIR pointer lists.
  WIR_Function f( "main" );
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
  IDs.push_back( (*it1++).get().getID() );
  IDs.push_back( (*it1++).get().getID() );
  IDs.push_back( (*it1++).get().getID() );
  IDs.push_back( (*it1++).get().getID() );
  IDs.push_back( (*it1++).get().getID() );
  IDs.push_back( (*it1).get().getID() );

  // Check the different variants of replace.
  it1 = f.getVirtualRegisters().begin();
  it1++;
  WIR_VirtualRegister r4( MIPS::RegisterType::reg );
  auto it2 = f.replaceVirtualRegister( it1, r4 );
  it1 = f.getVirtualRegisters().end();
  it1--;
  it1--;
  auto it3 =
    f.replaceVirtualRegister(
      it1, WIR_VirtualRegister( MIPS::RegisterType::reg ) );

  // Check the generated list.
  ufAssert( f.getVirtualRegisters().size() == 6 );
  it1 = f.getVirtualRegisters().begin();
  ufAssert( (*it1++).get().getID() == IDs[ 0 ] );
  tmp = (*it1).get().getID();
  ufAssert( (*it1++).get().getID() != IDs[ 1 ] );
  IDs[ 1 ] = tmp;
  ufAssert( (*it1++).get().getID() == IDs[ 2 ] );
  ufAssert( (*it1++).get().getID() == IDs[ 3 ] );
  tmp = (*it1).get().getID();
  ufAssert( (*it1++).get().getID() != IDs[ 4 ] );
  IDs[ 4 ] = tmp;
  ufAssert( (*it1).get().getID() == IDs[ 5 ] );

  // Check that the iterator returned by replace points to the correct element.
  ufAssert( (*it2).get().getID() == IDs[ 1 ] );
  ufAssert( (*it3).get().getID() == IDs[ 4 ] );

  // Check popBack and popFront.
  f.popBackVirtualRegister();
  f.popFrontVirtualRegister();

  ufAssert( f.getVirtualRegisters().size() == 4 );
  it1 = f.getVirtualRegisters().begin();
  ufAssert( (*it1++).get().getID() == IDs[ 1 ] );
  ufAssert( (*it1++).get().getID() == IDs[ 2 ] );
  ufAssert( (*it1++).get().getID() == IDs[ 3 ] );
  ufAssert( (*it1).get().getID() == IDs[ 4 ] );

  // Check erase.
  it1 = f.getVirtualRegisters().end();
  it1--;
  it1--;
  auto it4 = f.eraseVirtualRegister( it1 );

  ufAssert( f.getVirtualRegisters().size() == 3 );
  it1 = f.getVirtualRegisters().begin();
  ufAssert( (*it1++).get().getID() == IDs[ 1 ] );
  ufAssert( (*it1++).get().getID() == IDs[ 2 ] );
  ufAssert( (*it1++).get().getID() == IDs[ 4 ] );

  // Check that the iterator returned by erase points to the correct element.
  ufAssert( (*it4++).get().getID() == IDs[ 4 ] );

  // Check clear.
  f.clearVirtualRegisters();
  ufAssert( f.getVirtualRegisters().size() == 0 );

  return( 0 );
}
