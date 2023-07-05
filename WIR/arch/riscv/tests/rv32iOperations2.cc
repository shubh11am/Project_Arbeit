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
#include <arch/riscv/rv32i.h>


using namespace std;
using namespace WIR;


int main( void )
{
  WIR_Init();

  RV32I p;
  const RV_RegP &x1 = p.x1(), &x2 = p.x2(), &x3 = p.x3();

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

  // The following operations must be accepted according to the RISC-V RV32I
  // ISA.
  rvop(
    { RV32I::OpCode::ADD, RV32I::OperationFormat::RRR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new WIR_RegisterParameter( x3, WIR_Usage::use ) } );

  rvop(
    { RV32I::OpCode::ADDI, RV32I::OperationFormat::RRC12_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new RV_Const12_Signed( -1023 ) } );

  rvop(
    { RV32I::OpCode::ADDI, RV32I::OperationFormat::RRL_2,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new WIR_LabelParameter( b ) } );

  rvop(
    { RV32I::OpCode::AND, RV32I::OperationFormat::RRR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new WIR_RegisterParameter( x3, WIR_Usage::use ) } );

  rvop(
    { RV32I::OpCode::ANDI, RV32I::OperationFormat::RRC12_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new RV_Const12_Signed( -1023 ) } );

  rvop(
    { RV32I::OpCode::ANDI, RV32I::OperationFormat::RRL_2,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new WIR_LabelParameter( b ) } );

  rvop(
    { RV32I::OpCode::AUIPC, RV32I::OperationFormat::RC20_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new RV_Const20_Unsigned( 500000 ) } );

  rvop(
    { RV32I::OpCode::AUIPC, RV32I::OperationFormat::RL_2,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_LabelParameter( b ) } );

  rvop(
    { RV32I::OpCode::BEQ, RV32I::OperationFormat::RRL_1,
      new WIR_RegisterParameter( x1, WIR_Usage::use ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new WIR_LabelParameter( b ) } );

  rvop(
    { RV32I::OpCode::BGE, RV32I::OperationFormat::RRL_1,
      new WIR_RegisterParameter( x1, WIR_Usage::use ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new WIR_LabelParameter( b ) } );

  rvop(
    { RV32I::OpCode::BGEU, RV32I::OperationFormat::RRL_1,
      new WIR_RegisterParameter( x1, WIR_Usage::use ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new WIR_LabelParameter( b ) } );

  rvop(
    { RV32I::OpCode::BLT, RV32I::OperationFormat::RRL_1,
      new WIR_RegisterParameter( x1, WIR_Usage::use ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new WIR_LabelParameter( b ) } );

  rvop(
    { RV32I::OpCode::BLTU, RV32I::OperationFormat::RRL_1,
      new WIR_RegisterParameter( x1, WIR_Usage::use ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new WIR_LabelParameter( b ) } );

  rvop(
    { RV32I::OpCode::BNE, RV32I::OperationFormat::RRL_1,
      new WIR_RegisterParameter( x1, WIR_Usage::use ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new WIR_LabelParameter( b ) } );

  rvop(
    { RV32I::OpCode::CSRRC, RV32I::OperationFormat::RSR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "frm" ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32I::OpCode::CSRRC, RV32I::OperationFormat::RSR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "fcsr" ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32I::OpCode::CSRRC, RV32I::OperationFormat::RSR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "fflags" ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32I::OpCode::CSRRC, RV32I::OperationFormat::RSR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "instret" ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32I::OpCode::CSRRC, RV32I::OperationFormat::RSR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "instreth" ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32I::OpCode::CSRRC, RV32I::OperationFormat::RSR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "cycle" ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32I::OpCode::CSRRC, RV32I::OperationFormat::RSR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "cycleh" ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32I::OpCode::CSRRC, RV32I::OperationFormat::RSR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "time" ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32I::OpCode::CSRRC, RV32I::OperationFormat::RSR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "timeh" ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32I::OpCode::CSRRCI, RV32I::OperationFormat::RSC5_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "frm" ),
      new RV_Const5_Unsigned( 15 ) } );

  rvop(
    { RV32I::OpCode::CSRRCI, RV32I::OperationFormat::RSC5_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "fcsr" ),
      new RV_Const5_Unsigned( 15 ) } );

  rvop(
    { RV32I::OpCode::CSRRCI, RV32I::OperationFormat::RSC5_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "fflags" ),
      new RV_Const5_Unsigned( 15 ) } );

  rvop(
    { RV32I::OpCode::CSRRCI, RV32I::OperationFormat::RSC5_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "instret" ),
      new RV_Const5_Unsigned( 15 ) } );

  rvop(
    { RV32I::OpCode::CSRRCI, RV32I::OperationFormat::RSC5_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "instreth" ),
      new RV_Const5_Unsigned( 15 ) } );

  rvop(
    { RV32I::OpCode::CSRRCI, RV32I::OperationFormat::RSC5_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "cycle" ),
      new RV_Const5_Unsigned( 15 ) } );

  rvop(
    { RV32I::OpCode::CSRRCI, RV32I::OperationFormat::RSC5_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "cycleh" ),
      new RV_Const5_Unsigned( 15 ) } );

  rvop(
    { RV32I::OpCode::CSRRCI, RV32I::OperationFormat::RSC5_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "time" ),
      new RV_Const5_Unsigned( 15 ) } );

  rvop(
    { RV32I::OpCode::CSRRCI, RV32I::OperationFormat::RSC5_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "timeh" ),
      new RV_Const5_Unsigned( 15 ) } );

  rvop(
    { RV32I::OpCode::CSRRS, RV32I::OperationFormat::RSR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "frm" ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32I::OpCode::CSRRS, RV32I::OperationFormat::RSR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "fcsr" ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32I::OpCode::CSRRS, RV32I::OperationFormat::RSR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "fflags" ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32I::OpCode::CSRRS, RV32I::OperationFormat::RSR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "instret" ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32I::OpCode::CSRRS, RV32I::OperationFormat::RSR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "instreth" ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32I::OpCode::CSRRS, RV32I::OperationFormat::RSR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "cycle" ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32I::OpCode::CSRRS, RV32I::OperationFormat::RSR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "cycleh" ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32I::OpCode::CSRRS, RV32I::OperationFormat::RSR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "time" ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32I::OpCode::CSRRS, RV32I::OperationFormat::RSR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "timeh" ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32I::OpCode::CSRRSI, RV32I::OperationFormat::RSC5_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "frm" ),
      new RV_Const5_Unsigned( 15 ) } );

  rvop(
    { RV32I::OpCode::CSRRSI, RV32I::OperationFormat::RSC5_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "fcsr" ),
      new RV_Const5_Unsigned( 15 ) } );

  rvop(
    { RV32I::OpCode::CSRRSI, RV32I::OperationFormat::RSC5_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "fflags" ),
      new RV_Const5_Unsigned( 15 ) } );

  rvop(
    { RV32I::OpCode::CSRRSI, RV32I::OperationFormat::RSC5_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "instret" ),
      new RV_Const5_Unsigned( 15 ) } );

  rvop(
    { RV32I::OpCode::CSRRSI, RV32I::OperationFormat::RSC5_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "instreth" ),
      new RV_Const5_Unsigned( 15 ) } );

  rvop(
    { RV32I::OpCode::CSRRSI, RV32I::OperationFormat::RSC5_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "cycle" ),
      new RV_Const5_Unsigned( 15 ) } );

  rvop(
    { RV32I::OpCode::CSRRSI, RV32I::OperationFormat::RSC5_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "cycleh" ),
      new RV_Const5_Unsigned( 15 ) } );

  rvop(
    { RV32I::OpCode::CSRRSI, RV32I::OperationFormat::RSC5_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "time" ),
      new RV_Const5_Unsigned( 15 ) } );

  rvop(
    { RV32I::OpCode::CSRRSI, RV32I::OperationFormat::RSC5_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "timeh" ),
      new RV_Const5_Unsigned( 15 ) } );

  rvop(
    { RV32I::OpCode::CSRRW, RV32I::OperationFormat::RSR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "frm" ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32I::OpCode::CSRRW, RV32I::OperationFormat::RSR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "fcsr" ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32I::OpCode::CSRRW, RV32I::OperationFormat::RSR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "fflags" ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32I::OpCode::CSRRW, RV32I::OperationFormat::RSR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "instret" ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32I::OpCode::CSRRW, RV32I::OperationFormat::RSR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "instreth" ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32I::OpCode::CSRRW, RV32I::OperationFormat::RSR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "cycle" ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32I::OpCode::CSRRW, RV32I::OperationFormat::RSR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "cycleh" ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32I::OpCode::CSRRW, RV32I::OperationFormat::RSR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "time" ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32I::OpCode::CSRRW, RV32I::OperationFormat::RSR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "timeh" ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32I::OpCode::CSRRWI, RV32I::OperationFormat::RSC5_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "frm" ),
      new RV_Const5_Unsigned( 15 ) } );

  rvop(
    { RV32I::OpCode::CSRRWI, RV32I::OperationFormat::RSC5_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "fcsr" ),
      new RV_Const5_Unsigned( 15 ) } );

  rvop(
    { RV32I::OpCode::CSRRWI, RV32I::OperationFormat::RSC5_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "fflags" ),
      new RV_Const5_Unsigned( 15 ) } );

  rvop(
    { RV32I::OpCode::CSRRWI, RV32I::OperationFormat::RSC5_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "instret" ),
      new RV_Const5_Unsigned( 15 ) } );

  rvop(
    { RV32I::OpCode::CSRRWI, RV32I::OperationFormat::RSC5_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "instreth" ),
      new RV_Const5_Unsigned( 15 ) } );

  rvop(
    { RV32I::OpCode::CSRRWI, RV32I::OperationFormat::RSC5_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "cycle" ),
      new RV_Const5_Unsigned( 15 ) } );

  rvop(
    { RV32I::OpCode::CSRRWI, RV32I::OperationFormat::RSC5_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "cycleh" ),
      new RV_Const5_Unsigned( 15 ) } );

  rvop(
    { RV32I::OpCode::CSRRWI, RV32I::OperationFormat::RSC5_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "time" ),
      new RV_Const5_Unsigned( 15 ) } );

  rvop(
    { RV32I::OpCode::CSRRWI, RV32I::OperationFormat::RSC5_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "timeh" ),
      new RV_Const5_Unsigned( 15 ) } );

  rvop(
    { RV32I::OpCode::EBREAK, RV32I::OperationFormat::NULL_1 } );

  rvop(
    { RV32I::OpCode::ECALL, RV32I::OperationFormat::NULL_1 } );

  rvop(
    { RV32I::OpCode::J, RV32I::OperationFormat::L_1,
      new WIR_LabelParameter( b ) } );

  rvop(
    { RV32I::OpCode::JAL, RV32I::OperationFormat::RL_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_LabelParameter( b ) } );

  rvop(
    { RV32I::OpCode::JALR, RV32I::OperationFormat::RC12R_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new RV_Const12_Signed( -4 ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32I::OpCode::JALR, RV32I::OperationFormat::RLR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_LabelParameter( b ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32I::OpCode::JALR, RV32I::OperationFormat::RRC12_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new RV_Const12_Signed( 0 ) } );

  rvop(
    { RV32I::OpCode::JALR, RV32I::OperationFormat::RRL_2,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new WIR_LabelParameter( b ) } );

  rvop(
    { RV32I::OpCode::LB, RV32I::OperationFormat::RC12R_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new RV_Const12_Signed( -1023 ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32I::OpCode::LB, RV32I::OperationFormat::RLR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_LabelParameter( b ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32I::OpCode::LBU, RV32I::OperationFormat::RC12R_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new RV_Const12_Signed( -1023 ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32I::OpCode::LBU, RV32I::OperationFormat::RLR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_LabelParameter( b ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32I::OpCode::LH, RV32I::OperationFormat::RC12R_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new RV_Const12_Signed( -1023 ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32I::OpCode::LH, RV32I::OperationFormat::RLR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_LabelParameter( b ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32I::OpCode::LHU, RV32I::OperationFormat::RC12R_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new RV_Const12_Signed( -1023 ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32I::OpCode::LHU, RV32I::OperationFormat::RLR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_LabelParameter( b ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32I::OpCode::LUI, RV32I::OperationFormat::RC20_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new RV_Const20_Unsigned( 500000 ) } );

  rvop(
    { RV32I::OpCode::LUI, RV32I::OperationFormat::RL_2,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_LabelParameter( b ) } );

  rvop(
    { RV32I::OpCode::LW, RV32I::OperationFormat::RC12R_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new RV_Const12_Signed( -1023 ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32I::OpCode::LW, RV32I::OperationFormat::RLR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_LabelParameter( b ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32I::OpCode::MOV, RV32I::OperationFormat::RR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32I::OpCode::OR, RV32I::OperationFormat::RRR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new WIR_RegisterParameter( x3, WIR_Usage::use ) } );

  rvop(
    { RV32I::OpCode::ORI, RV32I::OperationFormat::RRC12_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new RV_Const12_Signed( -1023 ) } );

  rvop(
    { RV32I::OpCode::ORI, RV32I::OperationFormat::RRL_2,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new WIR_LabelParameter( b ) } );

  rvop(
    { RV32I::OpCode::SB, RV32I::OperationFormat::RC12R_2,
      new WIR_RegisterParameter( x1, WIR_Usage::use ),
      new RV_Const12_Signed( -1023 ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32I::OpCode::SB, RV32I::OperationFormat::RLR_2,
      new WIR_RegisterParameter( x1, WIR_Usage::use ),
      new WIR_LabelParameter( b ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32I::OpCode::SH, RV32I::OperationFormat::RC12R_2,
      new WIR_RegisterParameter( x1, WIR_Usage::use ),
      new RV_Const12_Signed( -1023 ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32I::OpCode::SH, RV32I::OperationFormat::RLR_2,
      new WIR_RegisterParameter( x1, WIR_Usage::use ),
      new WIR_LabelParameter( b ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32I::OpCode::SLL, RV32I::OperationFormat::RRR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new WIR_RegisterParameter( x3, WIR_Usage::use ) } );

  rvop(
    { RV32I::OpCode::SLLI, RV32I::OperationFormat::RRC5_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new RV_Const5_Unsigned( 15 ) } );

  rvop(
    { RV32I::OpCode::SLT, RV32I::OperationFormat::RRR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new WIR_RegisterParameter( x3, WIR_Usage::use ) } );

  rvop(
    { RV32I::OpCode::SLTI, RV32I::OperationFormat::RRC12_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new RV_Const12_Signed( -1023 ) } );

  rvop(
    { RV32I::OpCode::SLTI, RV32I::OperationFormat::RRL_2,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new WIR_LabelParameter( b ) } );

  rvop(
    { RV32I::OpCode::SLTIU, RV32I::OperationFormat::RRC12_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new RV_Const12_Signed( -1023 ) } );

  rvop(
    { RV32I::OpCode::SLTIU, RV32I::OperationFormat::RRL_2,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new WIR_LabelParameter( b ) } );

  rvop(
    { RV32I::OpCode::SLTU, RV32I::OperationFormat::RRR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new WIR_RegisterParameter( x3, WIR_Usage::use ) } );

  rvop(
    { RV32I::OpCode::SRA, RV32I::OperationFormat::RRR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new WIR_RegisterParameter( x3, WIR_Usage::use ) } );

  rvop(
    { RV32I::OpCode::SRAI, RV32I::OperationFormat::RRC5_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new RV_Const5_Unsigned( 15 ) } );

  rvop(
    { RV32I::OpCode::SRL, RV32I::OperationFormat::RRR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new WIR_RegisterParameter( x3, WIR_Usage::use ) } );

  rvop(
    { RV32I::OpCode::SRLI, RV32I::OperationFormat::RRC5_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new RV_Const5_Unsigned( 15 ) } );

  rvop(
    { RV32I::OpCode::SUB, RV32I::OperationFormat::RRR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new WIR_RegisterParameter( x3, WIR_Usage::use ) } );

  rvop(
    { RV32I::OpCode::SW, RV32I::OperationFormat::RC12R_2,
      new WIR_RegisterParameter( x1, WIR_Usage::use ),
      new RV_Const12_Signed( -1023 ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32I::OpCode::SW, RV32I::OperationFormat::RLR_2,
      new WIR_RegisterParameter( x1, WIR_Usage::use ),
      new WIR_LabelParameter( b ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32I::OpCode::XOR, RV32I::OperationFormat::RRR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new WIR_RegisterParameter( x3, WIR_Usage::use ) } );

  rvop(
    { RV32I::OpCode::XORI, RV32I::OperationFormat::RRC12_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new RV_Const12_Signed( -1023 ) } );

  rvop(
    { RV32I::OpCode::XORI, RV32I::OperationFormat::RRL_2,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new WIR_LabelParameter( b ) } );

  return( 0 );
}
