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

// Include WIR headers
#include <wir/wir.h>
#include <arch/generic/mips.h>


using namespace std;
using namespace WIR;


int main( void )
{
  WIR_Init();

  // Create some WIR list.
  WIR_Function f( "main" );
  MIPS_RegV r1, r2, r3;

  f.pushBackVirtualRegister( r1 );
  f.pushBackVirtualRegister( MIPS_RegV() );
  f.pushFrontVirtualRegister( r1 );
  f.pushFrontVirtualRegister( MIPS_RegV() );

  auto it1 = f.getVirtualRegisters().begin();
  ++it1;
  f.insertVirtualRegister( it1, r3 );
  it1 = f.getVirtualRegisters().end();
  --it1;
  --it1;
  f.insertVirtualRegister( it1, MIPS_RegV() );

  // Check that erase actually destroys the removed element.
  auto it = f.getVirtualRegisters().begin();
  ++it;
  ++it;
  WIR_VirtualRegister &reg = it->get();
  f.eraseVirtualRegister( it );
  cout << reg.getID() << reg.getName() << endl;

  return( 0 );
}
