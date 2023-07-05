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

  // Check the entire class hierarchy from WIR_Operation down to WIR_Parameter.

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
  ufAssert( o.getOpCode() == MIPS::OpCode::ADDI );
  ufAssert( o.getOperationFormat() == MIPS::OperationFormat::RRI );

  // Traverse the entire hierarchy and check the IDs.
  ufAssert( !o.isInserted() );

  for ( WIR_Parameter &p : o ) {
    ufAssert( p.isInserted() );
    ufAssert( p.getOperation().getID() == o.getID() );
  }

  // Test the copy constructors.
  WIR_Operation o1( o );
  ufAssert( o1.getOpCode() == MIPS::OpCode::ADDI );
  ufAssert( o1.getOperationFormat() == MIPS::OperationFormat::RRI );

  ufAssert( !o1.isInserted() );

  for ( WIR_Parameter &p : o1 ) {
    ufAssert( p.isInserted() );
    ufAssert( p.getOperation().getID() == o1.getID() );
  }

  // Test the move constructors.
  WIR_Operation o2( move( o ) );
  ufAssert( o2.getOpCode() == MIPS::OpCode::ADDI );
  ufAssert( o2.getOperationFormat() == MIPS::OperationFormat::RRI );

  ufAssert( !o2.isInserted() );

  for ( WIR_Parameter &p : o2 ) {
    ufAssert( p.isInserted() );
    ufAssert( p.getOperation().getID() == o2.getID() );
  }

  // Test the copy assignment operator.
  WIR_Operation o3(
    MIPS::OpCode::SW, MIPS::OperationFormat::RIR_2,
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    MIPS_Immediate16_Signed( 112 ),
    WIR_RegisterParameter( r2, WIR_Usage::use ) );
  ufAssert( o3.getOpCode() == MIPS::OpCode::SW );
  ufAssert( o3.getOperationFormat() == MIPS::OperationFormat::RIR_2 );
  o3 = o1;
  ufAssert( o3.getOpCode() == MIPS::OpCode::ADDI );
  ufAssert( o3.getOperationFormat() == MIPS::OperationFormat::RRI );

  ufAssert( !o3.isInserted() );

  for ( WIR_Parameter &p : o3 ) {
    ufAssert( p.isInserted() );
    ufAssert( p.getOperation().getID() == o3.getID() );
  }

  // Test the move assignment operator.
  WIR_Operation o4(
    MIPS::OpCode::SRA, MIPS::OperationFormat::RRS,
    WIR_RegisterParameter(
      WIR_VirtualRegister( MIPS::RegisterType::reg ), WIR_Usage::def ),
    WIR_RegisterParameter(
      WIR_VirtualRegister( MIPS::RegisterType::reg ), WIR_Usage::use ),
    MIPS_Immediate5_Shamt( 3 ) );
  ufAssert( o4.getOpCode() == MIPS::OpCode::SRA );
  ufAssert( o4.getOperationFormat() == MIPS::OperationFormat::RRS );
  o4 = move( o1 );
  ufAssert( o4.getOpCode() == MIPS::OpCode::ADDI );
  ufAssert( o4.getOperationFormat() == MIPS::OperationFormat::RRI );

  ufAssert( !o4.isInserted() );

  for ( WIR_Parameter &p : o4 ) {
    ufAssert( p.isInserted() );
    ufAssert( p.getOperation().getID() == o4.getID() );
  }

  return( 0 );
}
