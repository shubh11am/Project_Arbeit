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

  WIR_CompilationUnit c1;
  c1.setName( "file1" );
  WIR_Function &f1 = c1.pushBackFunction( WIR_Function( "foo1" ) );
  WIR_Function &f2 = c1.pushBackFunction( WIR_Function( "bar1" ) );
  WIR_Instruction i;

  WIR_BasicBlock &b1 = f1.pushBackBasicBlock( {} );
  b1.pushBackInstruction(
    { { MIPS::OpCode::JAL, MIPS::OperationFormat::L,
        WIR_LabelParameter( f2 ) } } );

  WIR_BasicBlock &b2 = f2.pushBackBasicBlock( {} );
  b2.pushBackInstruction(
    { { MIPS::OpCode::JAL, MIPS::OperationFormat::L,
        WIR_LabelParameter( f1 ) } } );

  // Copy compilation unit.
  WIR_CompilationUnit c2( c1 );
  c2.setName( "file2" );
  WIR_Function &f3 = c2.getFunctions().front();
  WIR_Function &f4 = c2.getFunctions().back();

  f3.setName( "foo2" );
  f4.setName( "bar2" );

  // Make sure that the JAL in f3 refers to f4 and vice versa.
  auto &i3 = f3.getBasicBlocks().front().get().getInstructions().front().get();
  auto &i4 = f4.getBasicBlocks().front().get().getInstructions().front().get();
  auto &o3 = i3.getOperations().front().get();
  auto &o4 = i4.getOperations().front().get();
  auto &p3 =
    dynamic_cast<WIR_LabelParameter &>( o3.getParameters().front().get() );
  auto &p4 =
    dynamic_cast<WIR_LabelParameter &>( o4.getParameters().front().get() );

  ufAssert( p3.getFunction() == f4 );
  ufAssert( p4.getFunction() == f3 );

  return( 0 );
}
