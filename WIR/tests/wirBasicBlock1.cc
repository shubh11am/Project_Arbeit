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

  // Check the entire class hierarchy from WIR_BasicBlock down to WIR_Parameter.

  WIR_Function f( "main" );
  WIR_VirtualRegister &r1 =
    f.pushBackVirtualRegister( WIR_VirtualRegister( MIPS::RegisterType::reg ) );
  WIR_VirtualRegister &r2 =
    f.pushBackVirtualRegister( WIR_VirtualRegister( MIPS::RegisterType::reg ) );
  WIR_RegisterParameter p1( r1, WIR_Usage::def );
  WIR_RegisterParameter p2( r2, WIR_Usage::use );
  MIPS_Immediate16_Signed p3( 42 );

  WIR_Operation o(
    MIPS::OpCode::ADDI, MIPS::OperationFormat::RRI, p1, p2, p3 );
  WIR_Instruction i;
  WIR_BasicBlock b;

  i.pushBackOperation( o );
  i.pushBackOperation(
    WIR_Operation(
      MIPS::OpCode::SW, MIPS::OperationFormat::RIR_2,
      WIR_RegisterParameter( r1, WIR_Usage::use ),
      MIPS_Immediate16_Signed( 112 ),
      WIR_RegisterParameter( r2, WIR_Usage::use ) ) );
  b.pushBackInstruction( i );
  b.pushBackInstruction( WIR_Instruction() );

  // Traverse the entire hierarchy and check the IDs.
  ufAssert( !b.isInserted() );

  for ( WIR_Instruction &i : b ) {
    ufAssert( i.isInserted() );
    ufAssert( i.getBasicBlock().getID() == b.getID() );

    for ( WIR_Operation &o : i ) {
      ufAssert( o.isInserted() );
      ufAssert( o.getInstruction().getID() == i.getID() );

      for ( WIR_Parameter &p : o ) {
        ufAssert( p.isInserted() );
        ufAssert( p.getOperation().getID() == o.getID() );
      }
    }
  }

  // Test the copy constructors.
  WIR_BasicBlock b1( b );

  ufAssert( !b1.isInserted() );

  for ( WIR_Instruction &i : b1 ) {
    ufAssert( i.isInserted() );
    ufAssert( i.getBasicBlock().getID() == b1.getID() );

    for ( WIR_Operation &o : i ) {
      ufAssert( o.isInserted() );
      ufAssert( o.getInstruction().getID() == i.getID() );

      for ( WIR_Parameter &p : o ) {
        ufAssert( p.isInserted() );
        ufAssert( p.getOperation().getID() == o.getID() );
      }
    }
  }

  // Test the move constructors.
  WIR_BasicBlock b2( move( b ) );

  ufAssert( !b2.isInserted() );

  for ( WIR_Instruction &i : b2 ) {
    ufAssert( i.isInserted() );
    ufAssert( i.getBasicBlock().getID() == b2.getID() );

    for ( WIR_Operation &o : i ) {
      ufAssert( o.isInserted() );
      ufAssert( o.getInstruction().getID() == i.getID() );

      for ( WIR_Parameter &p : o ) {
        ufAssert( p.isInserted() );
        ufAssert( p.getOperation().getID() == o.getID() );
      }
    }
  }

  // Test the copy assignment operator.
  WIR_BasicBlock b3;
  b3 = b1;

  ufAssert( !b3.isInserted() );

  for ( WIR_Instruction &i : b3 ) {
    ufAssert( i.isInserted() );
    ufAssert( i.getBasicBlock().getID() == b3.getID() );

    for ( WIR_Operation &o : i ) {
      ufAssert( o.isInserted() );
      ufAssert( o.getInstruction().getID() == i.getID() );

      for ( WIR_Parameter &p : o ) {
        ufAssert( p.isInserted() );
        ufAssert( p.getOperation().getID() == o.getID() );
      }
    }
  }

  // Test the move assignment operator.
  WIR_BasicBlock b4;
  b4 = move( b1 );

  ufAssert( !b4.isInserted() );

  for ( WIR_Instruction &i : b4 ) {
    ufAssert( i.isInserted() );
    ufAssert( i.getBasicBlock().getID() == b4.getID() );

    for ( WIR_Operation &o : i ) {
      ufAssert( o.isInserted() );
      ufAssert( o.getInstruction().getID() == i.getID() );

      for ( WIR_Parameter &p : o ) {
        ufAssert( p.isInserted() );
        ufAssert( p.getOperation().getID() == o.getID() );
      }
    }
  }

  return( 0 );
}
