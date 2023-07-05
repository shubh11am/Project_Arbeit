/*

   This source file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Intermediate Representation Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2021 - 2022

   Hamburg University of Technology (TUHH)
   Institute of Embedded Systems
   21071 Hamburg
   Germany

   http://www.tuhh.de/es/esd/research/wcc

*/


//
// Include section
//

// Include WIR headers
#include <wir/wir.h>
#include <arch/riscv/rv32ic.h>


using namespace std;
using namespace WIR;


int main( void )
{
  WIR_Init();

  RV32IC p;
  const RV_RegP &x1 = p.x12(), &x2 = p.x12(), &x3 = p.x2();

  WIR_BasicBlock b;
  WIR_Function f( "main" );

  cout.iword( WIR_Indentation() ) = 8;
  cout << riscv << comment;

  // This lambda serves for generating a RISC-V operation, adding a comment to
  // it and finally dumping it.
  auto rvop = []( WIR_Operation &&o ) {
    o.insertContainer(
      WIR_Comment( "Operation Format: " + o.getOperationFormat().getName() ) );
    cout << o << endl;
  };

  // The following operations must be accepted according to the RISC-V RV32IC
  // ISA.
  rvop(
    { RV32IC::OpCode::CADD, RV32IC::OperationFormat::SRR_2,
      new WIR_RegisterParameter( x1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32IC::OpCode::CADDI, RV32IC::OperationFormat::SRC6_3,
      new WIR_RegisterParameter( x1, WIR_Usage::defuse ),
      new RV_Const6_Signed( -10 ) } );

  rvop(
    { RV32IC::OpCode::CADDI16SP, RV32IC::OperationFormat::SRC6_4,
      new WIR_RegisterParameter( x3, WIR_Usage::defuse ),
      new RV_Const6_Signed( -16 ) } );

  rvop(
    { RV32IC::OpCode::CADDI4SPN, RV32IC::OperationFormat::SRRC8_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x3, WIR_Usage::use ),
      new RV_Const8_Unsigned( 4 ) } );

  rvop(
    { RV32IC::OpCode::CAND, RV32IC::OperationFormat::SRR_2,
      new WIR_RegisterParameter( x1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32IC::OpCode::CANDI, RV32IC::OperationFormat::SRC6_3,
      new WIR_RegisterParameter( x1, WIR_Usage::defuse ),
      new RV_Const6_Signed( -10 ) } );

  rvop(
    { RV32IC::OpCode::CBEQZ, RV32IC::OperationFormat::SRL_1,
      new WIR_RegisterParameter( x1, WIR_Usage::use ),
      new WIR_LabelParameter ( b ) } );

  rvop(
    { RV32IC::OpCode::CBNEZ, RV32IC::OperationFormat::SRL_1,
      new WIR_RegisterParameter( x1, WIR_Usage::use ),
      new WIR_LabelParameter ( b ) } );

  rvop(
    { RV32IC::OpCode::CEBREAK, RV32IC::OperationFormat::SNULL_1 } );

  rvop(
    { RV32IC::OpCode::CJ, RV32IC::OperationFormat::SL_1,
      new WIR_LabelParameter ( b ) } );

  rvop(
    { RV32IC::OpCode::CJAL, RV32IC::OperationFormat::SL_1,
      new WIR_LabelParameter ( b ) } );

  rvop(
    { RV32IC::OpCode::CJALR, RV32IC::OperationFormat::SR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::use ) } );

  rvop(
    { RV32IC::OpCode::CJR, RV32IC::OperationFormat::SR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::use ) } );

  rvop(
    { RV32IC::OpCode::CLI, RV32IC::OperationFormat::SRC6_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new RV_Const6_Signed( -10 ) } );

  rvop(
    { RV32IC::OpCode::CLUI, RV32IC::OperationFormat::SRC6_2,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new RV_Const6_Unsigned( 10 ) } );

  rvop(
    { RV32IC::OpCode::CLW, RV32IC::OperationFormat::SRC5R_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new RV_Const5_Unsigned( 4 ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32IC::OpCode::CLWSP, RV32IC::OperationFormat::SRC6R_1,
     new WIR_RegisterParameter( x1, WIR_Usage::def ),
     new RV_Const6_Unsigned( 4 ),
     new WIR_RegisterParameter( x3, WIR_Usage::use ) } );

  rvop(
    { RV32IC::OpCode::CMV, RV32IC::OperationFormat::SRR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32IC::OpCode::CNOP, RV32IC::OperationFormat::SNULL_1 } );

  rvop(
    { RV32IC::OpCode::COR, RV32IC::OperationFormat::SRR_2,
      new WIR_RegisterParameter( x1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32IC::OpCode::CSLLI, RV32IC::OperationFormat::SRC5_1,
      new WIR_RegisterParameter( x1, WIR_Usage::defuse ),
      new RV_Const5_Unsigned( 10 ) } );

  rvop(
    { RV32IC::OpCode::CSRAI, RV32IC::OperationFormat::SRC5_1,
      new WIR_RegisterParameter( x1, WIR_Usage::defuse ),
      new RV_Const5_Unsigned( 10 ) } );

  rvop(
    { RV32IC::OpCode::CSRLI, RV32IC::OperationFormat::SRC5_1,
      new WIR_RegisterParameter( x1, WIR_Usage::defuse ),
      new RV_Const5_Unsigned( 10 ) } );

  rvop(
    { RV32IC::OpCode::CSUB, RV32IC::OperationFormat::SRR_2,
      new WIR_RegisterParameter( x1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32IC::OpCode::CSW, RV32IC::OperationFormat::SRC5R_2,
      new WIR_RegisterParameter( x1, WIR_Usage::use ),
      new RV_Const5_Unsigned( 4 ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32IC::OpCode::CSWSP, RV32IC::OperationFormat::SRC6R_2,
      new WIR_RegisterParameter( x1, WIR_Usage::use ),
      new RV_Const6_Unsigned( 4 ),
      new WIR_RegisterParameter( x3, WIR_Usage::use ) } );

  rvop(
    { RV32IC::OpCode::CXOR, RV32IC::OperationFormat::SRR_2,
      new WIR_RegisterParameter( x1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  return( 0 );
}
