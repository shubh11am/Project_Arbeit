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

  WIR_Comment c1( "Comment 1" );


  // Check recursively erasing containers through the entire class hierarchy
  // from WIR_System down to WIR_Parameter.

  WIR_Function f( "foo" );
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
  c.pushBackFunction( f );
  c.pushBackFunction( WIR_Function( "bar" ) );
  sys.pushBackCompilationUnit( c );
  sys.pushBackCompilationUnit( WIR_CompilationUnit() );

  // Traverse the entire hierarchy and attach comments.
  sys.insertContainer( c1 );
  for ( WIR_CompilationUnit &c : sys ) {
    c.insertContainer( c1 );

    for ( WIR_Function &f : c ) {
      f.insertContainer( c1 );

      for ( WIR_BasicBlock &b : f ) {
        b.insertContainer( c1 );

        for ( WIR_Instruction &i : b ) {
          i.insertContainer( c1 );

          for ( WIR_Operation &o : i ) {
            o.insertContainer( c1 );

            for ( WIR_Parameter &p : o )
              p.insertContainer( c1 );
          }
        }
      }
    }
  }

  // Traverse the entire hierarchy and check that comments are attached.
  ufAssert( sys.containsContainers( WIR_Comment::getContainerTypeID() ) );
  for ( WIR_CompilationUnit &c : sys ) {
    ufAssert( c.containsContainers( WIR_Comment::getContainerTypeID() ) );

    for ( WIR_Function &f : c ) {
      ufAssert( f.containsContainers( WIR_Comment::getContainerTypeID() ) );

      for ( WIR_BasicBlock &b : f ) {
        ufAssert( b.containsContainers( WIR_Comment::getContainerTypeID() ) );

        for ( WIR_Instruction &i : b ) {
          ufAssert(
            i.containsContainers( WIR_Comment::getContainerTypeID() ) );

          for ( WIR_Operation &o : i ) {
            ufAssert(
              o.containsContainers( WIR_Comment::getContainerTypeID() ) );

            for ( WIR_Parameter &p : o )
              ufAssert(
                p.containsContainers( WIR_Comment::getContainerTypeID() ) );
          }
        }
      }
    }
  }

  // Erase comments recursively.
  sys.eraseContainers( WIR_Comment::getContainerTypeID(), true );

  // Traverse the entire hierarchy and check that no comments are attached.
  ufAssert( !sys.containsContainers( WIR_Comment::getContainerTypeID() ) );
  for ( WIR_CompilationUnit &c : sys ) {
    ufAssert( !c.containsContainers( WIR_Comment::getContainerTypeID() ) );

    for ( WIR_Function &f : c ) {
      ufAssert( !f.containsContainers( WIR_Comment::getContainerTypeID() ) );

      for ( WIR_BasicBlock &b : f ) {
        ufAssert(
          !b.containsContainers( WIR_Comment::getContainerTypeID() ) );

        for ( WIR_Instruction &i : b ) {
          ufAssert(
            !i.containsContainers( WIR_Comment::getContainerTypeID() ) );

          for ( WIR_Operation &o : i ) {
            ufAssert(
              !o.containsContainers( WIR_Comment::getContainerTypeID() ) );

            for ( WIR_Parameter &p : o )
              ufAssert(
                !p.containsContainers( WIR_Comment::getContainerTypeID() ) );
          }
        }
      }
    }
  }

  return( 0 );
}
