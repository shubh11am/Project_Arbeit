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

// Include libuseful headers
#include <libuseful/io.h>

// Include WIR headers
#include <wir/wir.h>
#include <arch/riscv/rv32imc.h>


using namespace std;
using namespace WIR;


void ITests( void )
{
  RV32IMC p;
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

  // The following operations must be accepted according to the RISC-V RV32I
  // ISA.
  rvop(
    { RV32IMC::OpCode::ADD, RV32IMC::OperationFormat::RRR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new WIR_RegisterParameter( x3, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::ADDI, RV32IMC::OperationFormat::RRC12_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new RV_Const12_Signed( -1023 ) } );

  rvop(
    { RV32IMC::OpCode::ADDI, RV32IMC::OperationFormat::RRL_2,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new WIR_LabelParameter( b ) } );

  rvop(
    { RV32IMC::OpCode::AND, RV32IMC::OperationFormat::RRR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new WIR_RegisterParameter( x3, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::ANDI, RV32IMC::OperationFormat::RRC12_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new RV_Const12_Signed( -1023 ) } );

  rvop(
    { RV32IMC::OpCode::ANDI, RV32IMC::OperationFormat::RRL_2,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new WIR_LabelParameter( b ) } );

  rvop(
    { RV32IMC::OpCode::AUIPC, RV32IMC::OperationFormat::RC20_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new RV_Const20_Unsigned( 500000 ) } );

  rvop(
    { RV32IMC::OpCode::AUIPC, RV32IMC::OperationFormat::RL_2,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_LabelParameter( b ) } );

  rvop(
    { RV32IMC::OpCode::BEQ, RV32IMC::OperationFormat::RRL_1,
      new WIR_RegisterParameter( x1, WIR_Usage::use ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new WIR_LabelParameter( b ) } );

  rvop(
    { RV32IMC::OpCode::BGE, RV32IMC::OperationFormat::RRL_1,
      new WIR_RegisterParameter( x1, WIR_Usage::use ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new WIR_LabelParameter( b ) } );

  rvop(
    { RV32IMC::OpCode::BGEU, RV32IMC::OperationFormat::RRL_1,
      new WIR_RegisterParameter( x1, WIR_Usage::use ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new WIR_LabelParameter( b ) } );

  rvop(
    { RV32IMC::OpCode::BLT, RV32IMC::OperationFormat::RRL_1,
      new WIR_RegisterParameter( x1, WIR_Usage::use ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new WIR_LabelParameter( b ) } );

  rvop(
    { RV32IMC::OpCode::BLTU, RV32IMC::OperationFormat::RRL_1,
      new WIR_RegisterParameter( x1, WIR_Usage::use ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new WIR_LabelParameter( b ) } );

  rvop(
    { RV32IMC::OpCode::BNE, RV32IMC::OperationFormat::RRL_1,
      new WIR_RegisterParameter( x1, WIR_Usage::use ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new WIR_LabelParameter( b ) } );

  rvop(
    { RV32IMC::OpCode::CSRRC, RV32IMC::OperationFormat::RSR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "frm" ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::CSRRC, RV32IMC::OperationFormat::RSR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "fcsr" ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::CSRRC, RV32IMC::OperationFormat::RSR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "fflags" ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::CSRRC, RV32IMC::OperationFormat::RSR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "instret" ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::CSRRC, RV32IMC::OperationFormat::RSR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "instreth" ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::CSRRC, RV32IMC::OperationFormat::RSR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "cycle" ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::CSRRC, RV32IMC::OperationFormat::RSR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "cycleh" ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::CSRRC, RV32IMC::OperationFormat::RSR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "time" ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::CSRRC, RV32IMC::OperationFormat::RSR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "timeh" ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::CSRRCI, RV32IMC::OperationFormat::RSC5_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "frm" ),
      new RV_Const5_Unsigned( 15 ) } );

  rvop(
    { RV32IMC::OpCode::CSRRCI, RV32IMC::OperationFormat::RSC5_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "fcsr" ),
      new RV_Const5_Unsigned( 15 ) } );

  rvop(
    { RV32IMC::OpCode::CSRRCI, RV32IMC::OperationFormat::RSC5_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "fflags" ),
      new RV_Const5_Unsigned( 15 ) } );

  rvop(
    { RV32IMC::OpCode::CSRRCI, RV32IMC::OperationFormat::RSC5_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "instret" ),
      new RV_Const5_Unsigned( 15 ) } );

  rvop(
    { RV32IMC::OpCode::CSRRCI, RV32IMC::OperationFormat::RSC5_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "instreth" ),
      new RV_Const5_Unsigned( 15 ) } );

  rvop(
    { RV32IMC::OpCode::CSRRCI, RV32IMC::OperationFormat::RSC5_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "cycle" ),
      new RV_Const5_Unsigned( 15 ) } );

  rvop(
    { RV32IMC::OpCode::CSRRCI, RV32IMC::OperationFormat::RSC5_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "cycleh" ),
      new RV_Const5_Unsigned( 15 ) } );

  rvop(
    { RV32IMC::OpCode::CSRRCI, RV32IMC::OperationFormat::RSC5_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "time" ),
      new RV_Const5_Unsigned( 15 ) } );

  rvop(
    { RV32IMC::OpCode::CSRRCI, RV32IMC::OperationFormat::RSC5_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "timeh" ),
      new RV_Const5_Unsigned( 15 ) } );

  rvop(
    { RV32IMC::OpCode::CSRRS, RV32IMC::OperationFormat::RSR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "frm" ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::CSRRS, RV32IMC::OperationFormat::RSR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "fcsr" ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::CSRRS, RV32IMC::OperationFormat::RSR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "fflags" ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::CSRRS, RV32IMC::OperationFormat::RSR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "instret" ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::CSRRS, RV32IMC::OperationFormat::RSR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "instreth" ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::CSRRS, RV32IMC::OperationFormat::RSR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "cycle" ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::CSRRS, RV32IMC::OperationFormat::RSR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "cycleh" ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::CSRRS, RV32IMC::OperationFormat::RSR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "time" ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::CSRRS, RV32IMC::OperationFormat::RSR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "timeh" ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::CSRRSI, RV32IMC::OperationFormat::RSC5_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "frm" ),
      new RV_Const5_Unsigned( 15 ) } );

  rvop(
    { RV32IMC::OpCode::CSRRSI, RV32IMC::OperationFormat::RSC5_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "fcsr" ),
      new RV_Const5_Unsigned( 15 ) } );

  rvop(
    { RV32IMC::OpCode::CSRRSI, RV32IMC::OperationFormat::RSC5_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "fflags" ),
      new RV_Const5_Unsigned( 15 ) } );

  rvop(
    { RV32IMC::OpCode::CSRRSI, RV32IMC::OperationFormat::RSC5_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "instret" ),
      new RV_Const5_Unsigned( 15 ) } );

  rvop(
    { RV32IMC::OpCode::CSRRSI, RV32IMC::OperationFormat::RSC5_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "instreth" ),
      new RV_Const5_Unsigned( 15 ) } );

  rvop(
    { RV32IMC::OpCode::CSRRSI, RV32IMC::OperationFormat::RSC5_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "cycle" ),
      new RV_Const5_Unsigned( 15 ) } );

  rvop(
    { RV32IMC::OpCode::CSRRSI, RV32IMC::OperationFormat::RSC5_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "cycleh" ),
      new RV_Const5_Unsigned( 15 ) } );

  rvop(
    { RV32IMC::OpCode::CSRRSI, RV32IMC::OperationFormat::RSC5_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "time" ),
      new RV_Const5_Unsigned( 15 ) } );

  rvop(
    { RV32IMC::OpCode::CSRRSI, RV32IMC::OperationFormat::RSC5_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "timeh" ),
      new RV_Const5_Unsigned( 15 ) } );

  rvop(
    { RV32IMC::OpCode::CSRRW, RV32IMC::OperationFormat::RSR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "frm" ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::CSRRW, RV32IMC::OperationFormat::RSR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "fcsr" ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::CSRRW, RV32IMC::OperationFormat::RSR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "fflags" ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::CSRRW, RV32IMC::OperationFormat::RSR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "instret" ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::CSRRW, RV32IMC::OperationFormat::RSR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "instreth" ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::CSRRW, RV32IMC::OperationFormat::RSR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "cycle" ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::CSRRW, RV32IMC::OperationFormat::RSR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "cycleh" ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::CSRRW, RV32IMC::OperationFormat::RSR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "time" ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::CSRRW, RV32IMC::OperationFormat::RSR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "timeh" ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::CSRRWI, RV32IMC::OperationFormat::RSC5_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "frm" ),
      new RV_Const5_Unsigned( 15 ) } );

  rvop(
    { RV32IMC::OpCode::CSRRWI, RV32IMC::OperationFormat::RSC5_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "fcsr" ),
      new RV_Const5_Unsigned( 15 ) } );

  rvop(
    { RV32IMC::OpCode::CSRRWI, RV32IMC::OperationFormat::RSC5_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "fflags" ),
      new RV_Const5_Unsigned( 15 ) } );

  rvop(
    { RV32IMC::OpCode::CSRRWI, RV32IMC::OperationFormat::RSC5_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "instret" ),
      new RV_Const5_Unsigned( 15 ) } );

  rvop(
    { RV32IMC::OpCode::CSRRWI, RV32IMC::OperationFormat::RSC5_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "instreth" ),
      new RV_Const5_Unsigned( 15 ) } );

  rvop(
    { RV32IMC::OpCode::CSRRWI, RV32IMC::OperationFormat::RSC5_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "cycle" ),
      new RV_Const5_Unsigned( 15 ) } );

  rvop(
    { RV32IMC::OpCode::CSRRWI, RV32IMC::OperationFormat::RSC5_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "cycleh" ),
      new RV_Const5_Unsigned( 15 ) } );

  rvop(
    { RV32IMC::OpCode::CSRRWI, RV32IMC::OperationFormat::RSC5_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "time" ),
      new RV_Const5_Unsigned( 15 ) } );

  rvop(
    { RV32IMC::OpCode::CSRRWI, RV32IMC::OperationFormat::RSC5_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_StringParameter( "timeh" ),
      new RV_Const5_Unsigned( 15 ) } );

  rvop(
    { RV32IMC::OpCode::EBREAK, RV32IMC::OperationFormat::NULL_1 } );

  rvop(
    { RV32IMC::OpCode::ECALL, RV32IMC::OperationFormat::NULL_1 } );

  rvop(
    { RV32IMC::OpCode::J, RV32IMC::OperationFormat::L_1,
      new WIR_LabelParameter( b ) } );

  rvop(
    { RV32IMC::OpCode::JAL, RV32IMC::OperationFormat::RL_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_LabelParameter( b ) } );

  rvop(
    { RV32IMC::OpCode::JALR, RV32IMC::OperationFormat::RC12R_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new RV_Const12_Signed( -4 ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::JALR, RV32IMC::OperationFormat::RLR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_LabelParameter( b ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::JALR, RV32IMC::OperationFormat::RRC12_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new RV_Const12_Signed( 0 ) } );

  rvop(
    { RV32IMC::OpCode::JALR, RV32IMC::OperationFormat::RRL_2,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new WIR_LabelParameter( b ) } );

  rvop(
    { RV32IMC::OpCode::LB, RV32IMC::OperationFormat::RC12R_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new RV_Const12_Signed( -1023 ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::LB, RV32IMC::OperationFormat::RLR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_LabelParameter( b ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::LBU, RV32IMC::OperationFormat::RC12R_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new RV_Const12_Signed( -1023 ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::LBU, RV32IMC::OperationFormat::RLR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_LabelParameter( b ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::LH, RV32IMC::OperationFormat::RC12R_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new RV_Const12_Signed( -1023 ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::LH, RV32IMC::OperationFormat::RLR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_LabelParameter( b ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::LHU, RV32IMC::OperationFormat::RC12R_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new RV_Const12_Signed( -1023 ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::LHU, RV32IMC::OperationFormat::RLR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_LabelParameter( b ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::LUI, RV32IMC::OperationFormat::RC20_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new RV_Const20_Unsigned( 500000 ) } );

  rvop(
    { RV32IMC::OpCode::LUI, RV32IMC::OperationFormat::RL_2,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_LabelParameter( b ) } );

  rvop(
    { RV32IMC::OpCode::LW, RV32IMC::OperationFormat::RC12R_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new RV_Const12_Signed( -1023 ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::LW, RV32IMC::OperationFormat::RLR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_LabelParameter( b ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::MOV, RV32IMC::OperationFormat::RR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::OR, RV32IMC::OperationFormat::RRR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new WIR_RegisterParameter( x3, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::ORI, RV32IMC::OperationFormat::RRC12_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new RV_Const12_Signed( -1023 ) } );

  rvop(
    { RV32IMC::OpCode::ORI, RV32IMC::OperationFormat::RRL_2,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new WIR_LabelParameter( b ) } );

  rvop(
    { RV32IMC::OpCode::SB, RV32IMC::OperationFormat::RC12R_2,
      new WIR_RegisterParameter( x1, WIR_Usage::use ),
      new RV_Const12_Signed( -1023 ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::SB, RV32IMC::OperationFormat::RLR_2,
      new WIR_RegisterParameter( x1, WIR_Usage::use ),
      new WIR_LabelParameter( b ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::SH, RV32IMC::OperationFormat::RC12R_2,
      new WIR_RegisterParameter( x1, WIR_Usage::use ),
      new RV_Const12_Signed( -1023 ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::SH, RV32IMC::OperationFormat::RLR_2,
      new WIR_RegisterParameter( x1, WIR_Usage::use ),
      new WIR_LabelParameter( b ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::SLL, RV32IMC::OperationFormat::RRR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new WIR_RegisterParameter( x3, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::SLLI, RV32IMC::OperationFormat::RRC5_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new RV_Const5_Unsigned( 15 ) } );

  rvop(
    { RV32IMC::OpCode::SLT, RV32IMC::OperationFormat::RRR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new WIR_RegisterParameter( x3, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::SLTI, RV32IMC::OperationFormat::RRC12_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new RV_Const12_Signed( -1023 ) } );

  rvop(
    { RV32IMC::OpCode::SLTI, RV32IMC::OperationFormat::RRL_2,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new WIR_LabelParameter( b ) } );

  rvop(
    { RV32IMC::OpCode::SLTIU, RV32IMC::OperationFormat::RRC12_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new RV_Const12_Signed( -1023 ) } );

  rvop(
    { RV32IMC::OpCode::SLTIU, RV32IMC::OperationFormat::RRL_2,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new WIR_LabelParameter( b ) } );

  rvop(
    { RV32IMC::OpCode::SLTU, RV32IMC::OperationFormat::RRR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new WIR_RegisterParameter( x3, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::SRA, RV32IMC::OperationFormat::RRR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new WIR_RegisterParameter( x3, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::SRAI, RV32IMC::OperationFormat::RRC5_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new RV_Const5_Unsigned( 15 ) } );

  rvop(
    { RV32IMC::OpCode::SRL, RV32IMC::OperationFormat::RRR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new WIR_RegisterParameter( x3, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::SRLI, RV32IMC::OperationFormat::RRC5_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new RV_Const5_Unsigned( 15 ) } );

  rvop(
    { RV32IMC::OpCode::SUB, RV32IMC::OperationFormat::RRR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new WIR_RegisterParameter( x3, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::SW, RV32IMC::OperationFormat::RC12R_2,
      new WIR_RegisterParameter( x1, WIR_Usage::use ),
      new RV_Const12_Signed( -1023 ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::SW, RV32IMC::OperationFormat::RLR_2,
      new WIR_RegisterParameter( x1, WIR_Usage::use ),
      new WIR_LabelParameter( b ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::XOR, RV32IMC::OperationFormat::RRR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new WIR_RegisterParameter( x3, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::XORI, RV32IMC::OperationFormat::RRC12_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new RV_Const12_Signed( -1023 ) } );

  rvop(
    { RV32IMC::OpCode::XORI, RV32IMC::OperationFormat::RRL_2,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new WIR_LabelParameter( b ) } );

  return;
};


void ICTests( void )
{
  RV32IMC p;
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
    { RV32IMC::OpCode::CADD, RV32IMC::OperationFormat::SRR_2,
      new WIR_RegisterParameter( x1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::CADDI, RV32IMC::OperationFormat::SRC6_3,
      new WIR_RegisterParameter( x1, WIR_Usage::defuse ),
      new RV_Const6_Signed( -10 ) } );

  rvop(
    { RV32IMC::OpCode::CADDI16SP, RV32IMC::OperationFormat::SRC6_4,
      new WIR_RegisterParameter( x3, WIR_Usage::defuse ),
      new RV_Const6_Signed( -16 ) } );

  rvop(
    { RV32IMC::OpCode::CADDI4SPN, RV32IMC::OperationFormat::SRRC8_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x3, WIR_Usage::use ),
      new RV_Const8_Unsigned( 4 ) } );

  rvop(
    { RV32IMC::OpCode::CAND, RV32IMC::OperationFormat::SRR_2,
      new WIR_RegisterParameter( x1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::CANDI, RV32IMC::OperationFormat::SRC6_3,
      new WIR_RegisterParameter( x1, WIR_Usage::defuse ),
      new RV_Const6_Signed( -10 ) } );

  rvop(
    { RV32IMC::OpCode::CBEQZ, RV32IMC::OperationFormat::SRL_1,
      new WIR_RegisterParameter( x1, WIR_Usage::use ),
      new WIR_LabelParameter ( b ) } );

  rvop(
    { RV32IMC::OpCode::CBNEZ, RV32IMC::OperationFormat::SRL_1,
      new WIR_RegisterParameter( x1, WIR_Usage::use ),
      new WIR_LabelParameter ( b ) } );

  rvop(
    { RV32IMC::OpCode::CEBREAK, RV32IMC::OperationFormat::SNULL_1 } );

  rvop(
    { RV32IMC::OpCode::CJ, RV32IMC::OperationFormat::SL_1,
      new WIR_LabelParameter ( b ) } );

  rvop(
    { RV32IMC::OpCode::CJAL, RV32IMC::OperationFormat::SL_1,
      new WIR_LabelParameter ( b ) } );

  rvop(
    { RV32IMC::OpCode::CJALR, RV32IMC::OperationFormat::SR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::CJR, RV32IMC::OperationFormat::SR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::CLI, RV32IMC::OperationFormat::SRC6_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new RV_Const6_Signed( -10 ) } );

  rvop(
    { RV32IMC::OpCode::CLUI, RV32IMC::OperationFormat::SRC6_2,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new RV_Const6_Unsigned( 10 ) } );

  rvop(
    { RV32IMC::OpCode::CLW, RV32IMC::OperationFormat::SRC5R_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new RV_Const5_Unsigned( 4 ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::CLWSP, RV32IMC::OperationFormat::SRC6R_1,
     new WIR_RegisterParameter( x1, WIR_Usage::def ),
     new RV_Const6_Unsigned( 4 ),
     new WIR_RegisterParameter( x3, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::CMV, RV32IMC::OperationFormat::SRR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::CNOP, RV32IMC::OperationFormat::SNULL_1 } );

  rvop(
    { RV32IMC::OpCode::COR, RV32IMC::OperationFormat::SRR_2,
      new WIR_RegisterParameter( x1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::CSLLI, RV32IMC::OperationFormat::SRC5_1,
      new WIR_RegisterParameter( x1, WIR_Usage::defuse ),
      new RV_Const5_Unsigned( 10 ) } );

  rvop(
    { RV32IMC::OpCode::CSRAI, RV32IMC::OperationFormat::SRC5_1,
      new WIR_RegisterParameter( x1, WIR_Usage::defuse ),
      new RV_Const5_Unsigned( 10 ) } );

  rvop(
    { RV32IMC::OpCode::CSRLI, RV32IMC::OperationFormat::SRC5_1,
      new WIR_RegisterParameter( x1, WIR_Usage::defuse ),
      new RV_Const5_Unsigned( 10 ) } );

  rvop(
    { RV32IMC::OpCode::CSUB, RV32IMC::OperationFormat::SRR_2,
      new WIR_RegisterParameter( x1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::CSW, RV32IMC::OperationFormat::SRC5R_2,
      new WIR_RegisterParameter( x1, WIR_Usage::use ),
      new RV_Const5_Unsigned( 4 ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::CSWSP, RV32IMC::OperationFormat::SRC6R_2,
      new WIR_RegisterParameter( x1, WIR_Usage::use ),
      new RV_Const6_Unsigned( 4 ),
      new WIR_RegisterParameter( x3, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::CXOR, RV32IMC::OperationFormat::SRR_2,
      new WIR_RegisterParameter( x1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ) } );

  return;
};


void IMTests( void )
{

  RV32IMC p;
  const RV_RegP &x1 = p.x1(), &x2 = p.x2(), &x3 = p.x3();

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

  // The following operations must be accepted according to the RISC-V RV32IM
  // ISA.
  rvop(
    { RV32IMC::OpCode::DIV, RV32IMC::OperationFormat::RRR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new WIR_RegisterParameter( x3, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::DIVU, RV32IMC::OperationFormat::RRR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new WIR_RegisterParameter( x3, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::MUL, RV32IMC::OperationFormat::RRR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new WIR_RegisterParameter( x3, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::MULH, RV32IMC::OperationFormat::RRR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new WIR_RegisterParameter( x3, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::MULHSU, RV32IMC::OperationFormat::RRR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new WIR_RegisterParameter( x3, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::MULHU, RV32IMC::OperationFormat::RRR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new WIR_RegisterParameter( x3, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::REM, RV32IMC::OperationFormat::RRR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new WIR_RegisterParameter( x3, WIR_Usage::use ) } );

  rvop(
    { RV32IMC::OpCode::REMU, RV32IMC::OperationFormat::RRR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new WIR_RegisterParameter( x3, WIR_Usage::use ) } );

  return;
};


int main( void )
{
  WIR_Init();

  // The tests are split into seperate functions to avoid conflicts due to
  // matching variable names.

  // RV32I tests.
  ITests();

  // RV32IC tests.
  ICTests();

  // RV32IM tests.
  IMTests();

  return( 0 );
};
