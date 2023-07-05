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

  WIR_BasicBlock b;

  // Insert instructions in DESCENDING order of IDs.
  b.pushBackInstruction( WIR_Instruction() );
  b.pushFrontInstruction( WIR_Instruction() );
  b.pushFrontInstruction( WIR_Instruction() );

  // Copy basic block. The instructions in b1 should also appear in DESCENDING
  // ID order.
  WIR_BasicBlock b1( b );

  ufAssert( b.getInstructions().size() == b1.getInstructions().size() );

  // Check descending order of instruction IDs.
  WIR_id_t prev = nullid;
  for ( WIR_Instruction &i : b1 ) {
    if ( prev == nullid )
      ufAssert( i.getID() > prev );
    else
      ufAssert( i.getID() < prev );
    prev = i.getID();
  }

  return( 0 );
}
