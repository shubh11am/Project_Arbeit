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
  auto &r1 = f.pushBackVirtualRegister( MIPS_RegV() );
  auto &r2 = f.pushBackVirtualRegister( MIPS_RegV() );
  auto &r3 = f1.pushBackVirtualRegister( MIPS_RegV() );

  WIR_BasicBlock &b = f.pushBackBasicBlock( WIR_BasicBlock() );

  WIR_Instruction &i = b.pushBackInstruction( WIR_Instruction() );

  WIR_Operation &o = i.pushBackOperation(
    WIR_Operation(
      MIPS::OpCode::LW, MIPS::OperationFormat::RIR_1,
      WIR_RegisterParameter( r1, WIR_Usage::def ),
      MIPS_Immediate16_Signed( -138 ),
      WIR_RegisterParameter( r2, WIR_Usage::use ) ) );

  WIR_RegisterParameter p( r3, WIR_Usage::def );
  p.setImplicit();

  // This must assert since b contains virtual registers owned by f1.
  WIR_Parameter &p1 = o.pushBackParameter( p );

  // Fallback for disabled failsafe mode.
  if ( o.isInserted() ) {
    WIR_Instruction &i = o.getInstruction();

    if ( i.isInserted() ) {
      WIR_BasicBlock &b = i.getBasicBlock();

      if ( b.isInserted() ) {
        WIR_Function &f = b.getFunction();

        if ( p1.getType() == WIR_ParameterType::reg ) {
          auto &rp = dynamic_cast<const WIR_RegisterParameter &>( p1 );
          auto &r = rp.getRegister();

          if ( r.isVirtual() ) {
            auto &v = dynamic_cast<WIR_VirtualRegister &>( r );

            ufAssertT(
              !v.isInserted() || v.getFunction() == f,
              "Illegal attempt to insert a parameter into an operation " <<
              "owned by function '" << f.getName() << "' (ID " << f.getID() <<
              ") where virtual register '" << v.getName() <<
              "' belongs to function '" << v.getFunction().getName() <<
              "' (ID " << v.getFunction().getID() << ")!" );
          }
        }
      }
    }
  }

  return( 0 );
}
