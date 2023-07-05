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
#include <arch/tricore/tc131.h>


using namespace std;
using namespace WIR;


int main( void )
{
  WIR_Init();

  TC131 tricore;
  const TC_DRegV d1, d2, d3;
  WIR_BasicBlock b, b2;
  WIR_Function f( "main" );
  WIR_Instruction i;

  // The following operations must be accepted according to the TriCore ISA.
  i.pushBackOperation(
    WIR_Operation(
      TC131::OpCode::ABS, TC131::OperationFormat::DD,
      WIR_RegisterParameter( d1, WIR_Usage::def ),
      WIR_RegisterParameter( d2, WIR_Usage::use ) ) );
  b.pushBackInstruction( i );
  i.clearOperations();

  i.pushBackOperation(
    WIR_Operation(
      TC131::OpCode::ABS_B, TC131::OperationFormat::DD,
      WIR_RegisterParameter( d1, WIR_Usage::def ),
      WIR_RegisterParameter( d2, WIR_Usage::use ) ) );
  b2.pushBackInstruction( i );
  i.clearOperations();

  i.pushBackOperation(
    WIR_Operation(
      TC131::OpCode::ABS_H, TC131::OperationFormat::DD,
      WIR_RegisterParameter( d1, WIR_Usage::def ),
      WIR_RegisterParameter( d2, WIR_Usage::use ) ) );
  b.pushBackInstruction( i );
  i.clearOperations();

  i.pushBackOperation(
    WIR_Operation(
      TC131::OpCode::ABSDIF, TC131::OperationFormat::DDC9_1,
      WIR_RegisterParameter( d1, WIR_Usage::def ),
      WIR_RegisterParameter( d2, WIR_Usage::use ),
      TC_Const9_Signed( -128 ) ) );
  b2.pushBackInstruction( i );
  i.clearOperations();

  i.pushBackOperation(
    WIR_Operation(
      TC131::OpCode::ABSDIF, TC131::OperationFormat::DDD_1,
      WIR_RegisterParameter( d1, WIR_Usage::def ),
      WIR_RegisterParameter( d2, WIR_Usage::use ),
      WIR_RegisterParameter( d3, WIR_Usage::use ) ) );
  b.pushBackInstruction( i );
  i.clearOperations();

  i.pushBackOperation(
    WIR_Operation(
      TC131::OpCode::ABSDIF_B, TC131::OperationFormat::DDD_1,
      WIR_RegisterParameter( d1, WIR_Usage::def ),
      WIR_RegisterParameter( d2, WIR_Usage::use ),
      WIR_RegisterParameter( d3, WIR_Usage::use ) ) );
  b2.pushBackInstruction( i );
  i.clearOperations();

  i.pushBackOperation(
    WIR_Operation(
      TC131::OpCode::ABSDIF_H, TC131::OperationFormat::DDD_1,
      WIR_RegisterParameter( d1, WIR_Usage::def ),
      WIR_RegisterParameter( d2, WIR_Usage::use ),
      WIR_RegisterParameter( d3, WIR_Usage::use ) ) );
  b.pushBackInstruction( i );
  i.clearOperations();

  i.pushBackOperation(
    WIR_Operation(
      TC131::OpCode::ABSDIFS, TC131::OperationFormat::DDC9_1,
      WIR_RegisterParameter( d1, WIR_Usage::def ),
      WIR_RegisterParameter( d2, WIR_Usage::use ),
      TC_Const9_Signed( -128 ) ) );
  b2.pushBackInstruction( i );
  i.clearOperations();

  i.pushBackOperation(
    WIR_Operation(
      TC131::OpCode::ABSDIFS, TC131::OperationFormat::DDD_1,
      WIR_RegisterParameter( d1, WIR_Usage::def ),
      WIR_RegisterParameter( d2, WIR_Usage::use ),
      WIR_RegisterParameter( d3, WIR_Usage::use ) ) );
  b.pushBackInstruction( i );
  i.clearOperations();

  i.pushBackOperation(
    WIR_Operation(
      TC131::OpCode::ABSDIFS_H, TC131::OperationFormat::DDD_1,
      WIR_RegisterParameter( d1, WIR_Usage::def ),
      WIR_RegisterParameter( d2, WIR_Usage::use ),
      WIR_RegisterParameter( d3, WIR_Usage::use ) ) );
  b2.pushBackInstruction( i );
  i.clearOperations();

  i.pushBackOperation(
    WIR_Operation(
      TC131::OpCode::ABSS, TC131::OperationFormat::DD,
      WIR_RegisterParameter( d1, WIR_Usage::def ),
      WIR_RegisterParameter( d2, WIR_Usage::use ) ) );
  b.pushBackInstruction( i );
  i.clearOperations();

  i.pushBackOperation(
    WIR_Operation(
      TC131::OpCode::ABSS_H, TC131::OperationFormat::DD,
      WIR_RegisterParameter( d1, WIR_Usage::def ),
      WIR_RegisterParameter( d2, WIR_Usage::use ) ) );
  b2.pushBackInstruction( i );
  i.clearOperations();

  i.pushBackOperation(
    WIR_Operation(
      TC131::OpCode::ADD, TC131::OperationFormat::DDC9_1,
      WIR_RegisterParameter( d1, WIR_Usage::def ),
      WIR_RegisterParameter( d2, WIR_Usage::use ),
      TC_Const9_Signed( -128 ) ) );
  b.pushBackInstruction( i );
  i.clearOperations();

  i.pushBackOperation(
    WIR_Operation(
      TC131::OpCode::ADD, TC131::OperationFormat::DDD_1,
      WIR_RegisterParameter( d1, WIR_Usage::def ),
      WIR_RegisterParameter( d2, WIR_Usage::use ),
      WIR_RegisterParameter( d3, WIR_Usage::use ) ) );
  b2.pushBackInstruction( i );
  i.clearOperations();

  i.pushBackOperation(
    WIR_Operation(
      TC131::OpCode::ADD, TC131::OperationFormat::SDC4_2,
      WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      TC_Const4_Signed( 5 ) ) );
  b.pushBackInstruction( i );
  i.clearOperations();

  i.pushBackOperation(
    WIR_Operation(
      TC131::OpCode::ADD, TC131::OperationFormat::SDIC4_2,
      WIR_RegisterParameter( d1, WIR_Usage::def ),
      WIR_RegisterParameter( tricore.D15(), WIR_Usage::use ),
      TC_Const4_Signed( -5 ) ) );
  b2.pushBackInstruction( i );
  i.clearOperations();

  i.pushBackOperation(
    WIR_Operation(
      TC131::OpCode::ADD, TC131::OperationFormat::SIDC4,
      WIR_RegisterParameter( tricore.D15(), WIR_Usage::def ),
      WIR_RegisterParameter( d1, WIR_Usage::use ),
      TC_Const4_Signed( -5 ) ) );
  b.pushBackInstruction( i );
  i.clearOperations();

  i.pushBackOperation(
    WIR_Operation(
      TC131::OpCode::ADD, TC131::OperationFormat::SDD_2,
      WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      WIR_RegisterParameter( d2, WIR_Usage::use ) ) );
  b2.pushBackInstruction( i );
  i.clearOperations();

  i.pushBackOperation(
    WIR_Operation(
      TC131::OpCode::ADD, TC131::OperationFormat::SDID_1,
      WIR_RegisterParameter( d1, WIR_Usage::def ),
      WIR_RegisterParameter( tricore.D15(), WIR_Usage::use ),
      WIR_RegisterParameter( d2, WIR_Usage::use ) ) );
  b.pushBackInstruction( i );
  i.clearOperations();

  i.pushBackOperation(
    WIR_Operation(
      TC131::OpCode::ADD, TC131::OperationFormat::SIDD,
      WIR_RegisterParameter( tricore.D15(), WIR_Usage::def ),
      WIR_RegisterParameter( d1, WIR_Usage::use ),
      WIR_RegisterParameter( d2, WIR_Usage::use ) ) );
  b2.pushBackInstruction( i );
  i.clearOperations();

  f.pushBackBasicBlock( b );
  f.pushBackBasicBlock( b2 );

  // Test getVREGs methods.
  WIR_VirtualRegisterSet vregs = f.getVREGs();
  ufAssert( vregs.size() == 3 );

  auto it = vregs.begin();
  ufAssert( (*it++).get().getID() == d1.getID() );
  ufAssert( (*it++).get().getID() == d2.getID() );
  ufAssert( (*it++).get().getID() == d3.getID() );

  return( 0 );
}
