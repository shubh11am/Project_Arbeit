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

  // Check insertion and removal from WIR lists.
  WIR_Function f( "main" );
  MIPS_RegV r1, r2, r3;

  // Check the different variants of pushBack and pushFront.
  f.pushBackVirtualRegister( r1 );
  f.pushBackVirtualRegister( MIPS_RegV() );
  f.pushBackVirtualRegister( r2 );
  f.pushBackVirtualRegister( MIPS_RegV() );

  // Check the different variants of insert.
  auto it1 = f.getVirtualRegisters().begin();
  ++it1;
  f.insertVirtualRegister( it1, r3 );
  it1 = f.getVirtualRegisters().end();
  --it1;
  --it1;
  f.insertVirtualRegister( it1, MIPS_RegV() );

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
  ufAssert( !f.containsVirtualRegister( 4 ) );
  ufAssert( !f.containsVirtualRegister( 5 ) );
  ufAssert( !f.containsVirtualRegister( 6 ) );
  ufAssert( !f.containsVirtualRegister( 130 ) );
  ufAssert( !f.containsVirtualRegister( 0x815 ) );

  it1 = f.getVirtualRegisters().end();
  --it1;
  --it1;
  --it1;
  f.eraseVirtualRegister( it1 );
  ufAssert( !f.containsVirtualRegister( IDs[ 3 ] ) );

  // Check containsVirtualRegister by reference.
  MIPS_RegV &reg1 = static_cast<MIPS_RegV &>( *(regs[ 0 ]) );
  MIPS_RegV &reg2 = static_cast<MIPS_RegV &>( *(regs[ 1 ]) );
  MIPS_RegV &reg3 = static_cast<MIPS_RegV &>( *(regs[ 2 ]) );
  MIPS_RegV &reg4 = static_cast<MIPS_RegV &>( *(regs[ 4 ]) );
  MIPS_RegV &reg5 = static_cast<MIPS_RegV &>( *(regs[ 5 ]) );
  ufAssert( f.containsVirtualRegister( reg1 ) );
  ufAssert( f.containsVirtualRegister( reg2 ) );
  ufAssert( f.containsVirtualRegister( reg3 ) );
  ufAssert( f.containsVirtualRegister( reg4 ) );
  ufAssert( f.containsVirtualRegister( reg5 ) );
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
  ufAssert( f.findVirtualRegister( 4 ) == f.getVirtualRegisters().end() );
  ufAssert( f.findVirtualRegister( 5 ) == f.getVirtualRegisters().end() );
  ufAssert( f.findVirtualRegister( 6 ) == f.getVirtualRegisters().end() );
  ufAssert( f.findVirtualRegister( 12 ) == f.getVirtualRegisters().end() );
  ufAssert(
    f.findVirtualRegister( IDs[ 3 ] ) == f.getVirtualRegisters().end() );

  // Check findVirtualRegister by reference.
  ufAssert( f.findVirtualRegister( reg1 ) != f.getVirtualRegisters().end() );
  ufAssert( f.findVirtualRegister( reg2 ) != f.getVirtualRegisters().end() );
  ufAssert( f.findVirtualRegister( reg3 ) != f.getVirtualRegisters().end() );
  ufAssert( f.findVirtualRegister( reg4 ) != f.getVirtualRegisters().end() );
  ufAssert( f.findVirtualRegister( reg5 ) != f.getVirtualRegisters().end() );
  ufAssert( f.findVirtualRegister( r1 ) == f.getVirtualRegisters().end() );
  ufAssert( f.findVirtualRegister( r2 ) == f.getVirtualRegisters().end() );
  ufAssert( f.findVirtualRegister( r3 ) == f.getVirtualRegisters().end() );

  return( 0 );
}
