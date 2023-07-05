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
#include <string>

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

  // Check the entire class hierarchy from WIR_Function down to WIR_Parameter.

  WIR_Function f( "f42" );
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
  f.pushBackBasicBlock( b );
  f.pushBackBasicBlock( WIR_BasicBlock() );
  WIR_VirtualRegister r( MIPS::RegisterType::reg );
  f.pushBackVirtualRegister( r );
  f.pushBackVirtualRegister( WIR_VirtualRegister( MIPS::RegisterType::reg ) );
  f.pushBackVirtualRegister( r );

  // Traverse the entire hierarchy and check the IDs.
  ufAssert( !f.isInserted() );

  for ( WIR_VirtualRegister &r : f.getVirtualRegisters() ) {
    ufAssert( r.isInserted() );
    ufAssert( r.getFunction().getID() == f.getID() );
  }

  for ( WIR_BasicBlock &b : f ) {
    ufAssert( b.isInserted() );
    ufAssert( b.getFunction().getID() == f.getID() );

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
  }

  // Test the copy constructors.
  f.setDontOptimize();
  WIR_Function f1( f );
  f.setDontOptimize( false );

  ufAssert( !f1.isInserted() );
  ufAssert( !f1.getDontOptimize() );

  for ( WIR_VirtualRegister &r : f1.getVirtualRegisters() ) {
    ufAssert( r.isInserted() );
    ufAssert( r.getFunction().getID() == f1.getID() );
  }

  for ( WIR_BasicBlock &b : f1 ) {
    ufAssert( b.isInserted() );
    ufAssert( b.getFunction().getID() == f1.getID() );

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
  }

  // Test the move constructors.
  WIR_Function f2( move( f ) );

  ufAssert( !f2.isInserted() );

  for ( WIR_VirtualRegister &r : f2.getVirtualRegisters() ) {
    ufAssert( r.isInserted() );
    ufAssert( r.getFunction().getID() == f2.getID() );
  }

  for ( WIR_BasicBlock &b : f2 ) {
    ufAssert( b.isInserted() );
    ufAssert( b.getFunction().getID() == f2.getID() );

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
  }

  // Test the copy assignment operator.
  string s( "main" );
  WIR_Function f3( s );
  f3 = f1;

  ufAssert( !f3.isInserted() );

  for ( WIR_VirtualRegister &r : f3.getVirtualRegisters() ) {
    ufAssert( r.isInserted() );
    ufAssert( r.getFunction().getID() == f3.getID() );
  }

  for ( WIR_BasicBlock &b : f3 ) {
    ufAssert( b.isInserted() );
    ufAssert( b.getFunction().getID() == f3.getID() );

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
  }

  // Test the move assignment operator.
  WIR_Function f4( "foobar" );
  f4 = move( f1 );

  ufAssert( !f4.isInserted() );

  for ( WIR_VirtualRegister &r : f4.getVirtualRegisters() ) {
    ufAssert( r.isInserted() );
    ufAssert( r.getFunction().getID() == f4.getID() );
  }

  for ( WIR_BasicBlock &b : f4 ) {
    ufAssert( b.isInserted() );
    ufAssert( b.getFunction().getID() == f4.getID() );

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
  }

  return( 0 );
}
