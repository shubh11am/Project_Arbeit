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

  // Check the entire class hierarchy from WIR_System down to WIR_Parameter.

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
  WIR_CompilationUnit c;
  WIR_TaskManager t;
  WIR_System sys( "genericmips.sys", t );

  i.insertOperation( i.getOperations().begin(), o );
  i.pushBackOperation(
    WIR_Operation(
      MIPS::OpCode::SW, MIPS::OperationFormat::RIR_2,
      WIR_RegisterParameter( r1, WIR_Usage::use ),
      MIPS_Immediate16_Signed( 112 ),
      WIR_RegisterParameter( r2, WIR_Usage::use ) ) );

  b.insertInstruction( b.getInstructions().begin(), i );
  b.insertInstruction( b.getInstructions().begin(), WIR_Instruction() );
  f.insertBasicBlock( f.getBasicBlocks().begin(), b );
  f.insertBasicBlock( f.getBasicBlocks().begin(), WIR_BasicBlock() );
  WIR_VirtualRegister r( MIPS::RegisterType::reg );
  f.insertVirtualRegister( f.getVirtualRegisters().begin(), r );
  f.insertVirtualRegister(
    f.getVirtualRegisters().begin(),
    WIR_VirtualRegister( MIPS::RegisterType::reg ) );
  f.insertVirtualRegister( f.getVirtualRegisters().begin(), r );
  c.insertFunction( c.getFunctions().begin(), f );
  c.insertFunction( c.getFunctions().begin(), WIR_Function( "foobar" ) );
  sys.insertCompilationUnit( sys.getCompilationUnits().begin(), c );
  sys.insertCompilationUnit(
    sys.getCompilationUnits().begin(), WIR_CompilationUnit() );

  // Traverse the entire hierarchy and check the IDs.
  ufAssert( sys.getTaskManager().isInserted() );
  ufAssert( sys.getTaskManager().getSystem().getID() == sys.getID() );

  for ( WIR_CompilationUnit &c : sys ) {
    ufAssert( c.isInserted() );
    ufAssert( c.getSystem().getID() == sys.getID() );

    for ( WIR_Function &f : c ) {
      ufAssert( f.isInserted() );
      ufAssert( f.getCompilationUnit().getID() == c.getID() );

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
    }
  }

  return( 0 );
}
