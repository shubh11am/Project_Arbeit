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

  WIR_Function f( "main" );

  // Insert registers in DESCENDING order of IDs.
  f.pushBackVirtualRegister( MIPS_RegV() );
  f.pushFrontVirtualRegister( MIPS_RegV() );
  f.pushFrontVirtualRegister( MIPS_RegV() );

  // Insert basic blocks in descending order of IDs.
  f.pushBackBasicBlock( WIR_BasicBlock() );
  f.pushFrontBasicBlock( WIR_BasicBlock() );
  f.pushFrontBasicBlock( WIR_BasicBlock() );

  // Copy function. The registers in f1 should also appear in DESCENDING ID
  // order.
  WIR_Function f1( f );

  ufAssert(
    f.getVirtualRegisters().size() == f1.getVirtualRegisters().size() );
  ufAssert( f.getBasicBlocks().size() == f1.getBasicBlocks().size() );

  // Check descending order of register IDs.
  WIR_id_t prev = nullid;
  for ( WIR_VirtualRegister &r : f1.getVirtualRegisters() ) {
    if ( prev == nullid )
      ufAssert( r.getID() > prev );
    else
      ufAssert( r.getID() < prev );
    prev = r.getID();
  }

  // Check descending order of basic block IDs.
  prev = nullid;
  for ( WIR_BasicBlock &b : f1 ) {
    if ( prev == nullid )
      ufAssert( b.getID() > prev );
    else
      ufAssert( b.getID() < prev );
    prev = b.getID();
  }

  return( 0 );
}
