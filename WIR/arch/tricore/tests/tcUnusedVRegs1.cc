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
  const TC_DRegP &d2 = tricore.D5();
  WIR_BasicBlock b, b2;
  WIR_Function f( "main" );
  auto &tr1 = f.pushBackVirtualRegister( TC_ERegV() );
  WIR_VirtualRegister &d1 = tr1.getChilds().front().get();
  WIR_VirtualRegister &d3 = f.pushBackVirtualRegister( TC_DRegV() );
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

  // Add some dummy virtual registers and get their IDs.
  WIR_id_t id1 = f.pushFrontVirtualRegister( TC_ERegV() ).getID();
  WIR_id_t id2 = f.pushBackVirtualRegister( TC_PRegV() ).getID();
  auto pos = f.insertVirtualRegister(
    ++(++(f.getVirtualRegisters().begin())), TC_ARegV() );
  WIR_id_t id3 = pos->get().getID();
  f.insertPrecolor( pos->get(), tricore.A11() );

  ufAssert( f.getVirtualRegisters().size() == 5 );

  // Remove unused virtual registers.
  WIR_UnusedVRegs unusedVRegs( f );
  unusedVRegs.optimize();

  // Check the remaining register IDs.
  ufAssert( f.getVirtualRegisters().size() == 2 );
  for ( WIR_VirtualRegister &r : f.getVirtualRegisters() )
    ufAssert(
      ( r.getID() != id1 ) && ( r.getID() != id2 ) && ( r.getID() != id3 ) );

  return( 0 );
}
