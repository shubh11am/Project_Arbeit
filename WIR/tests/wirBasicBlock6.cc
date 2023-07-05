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

  // Check the successor/predecessor relations of basic blocks.

  WIR_Function f( "foo" ), f1( "bar" );
  auto &r1 = f1.pushBackVirtualRegister( MIPS_RegV() );
  auto &r2 = f1.pushBackVirtualRegister( MIPS_RegV() );

  WIR_BasicBlock &b = f.pushBackBasicBlock( {} );

  // This must assert since b contains virtual registers owned by f1.
  WIR_Instruction &i2 =
    b.pushBackInstruction(
      { { MIPS::OpCode::LW, MIPS::OperationFormat::RIR_1,
          WIR_RegisterParameter( r1, WIR_Usage::def ),
          MIPS_Immediate16_Signed( -138 ),
          WIR_RegisterParameter( r2, WIR_Usage::use ) } } );

  // Fallback for disabled failsafe mode.
  if ( b.isInserted() ) {
    WIR_Function &f = b.getFunction();
    WIR_VirtualRegisterSet vregs = i2.getVREGs();

    for ( WIR_VirtualRegister &r : vregs )
      ufAssertT(
        !r.isInserted() || r.getFunction() == f,
        "Illegal attempt to insert an instruction into basic block '" <<
        b.getName() << "' owned by function '" << f.getName() << "' (ID " <<
        f.getID() << ") where virtual register '" << r.getName() <<
        "' belongs to function '" << r.getFunction().getName() << "' (ID " <<
        r.getFunction().getID() << ")!" );
  }

  return( 0 );
}
