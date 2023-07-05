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
#include <arch/riscv/rv32i.h>


using namespace std;
using namespace WIR;


int main( void )
{
  WIR_Init();

  RV32I riscv;
  RV_RegV x1, x2, x3;
  WIR_BasicBlock b;
  WIR_Function f( "main" );

  // The following operations must be accepted according to the RISC-V ISA.
  WIR_Operation add1(
    RV32I::OpCode::ADD, RV32I::OperationFormat::RRR_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ),
    new WIR_RegisterParameter( x3, WIR_Usage::use ) );
  ufAssert( add1.getSize() == 4 );
  ufAssert(
    !add1.isMemoryAccess() && !add1.isMemoryStore() && !add1.isMemoryLoad() &&
    !add1.isMove() && !add1.isCall() && !add1.isIndirectCall() &&
    !add1.isReturn() && !add1.isJump() && !add1.isConditionalJump() &&
    !add1.isUnconditionalJump() && !add1.isIndirectJump() &&
    !add1.isAsmDataDirective() && !add1.hasSideEffects() );

  WIR_Operation addi1(
    RV32I::OpCode::ADDI, RV32I::OperationFormat::RRC12_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ),
    new RV_Const12_Signed( -1023 ) );
  ufAssert( addi1.getSize() == 4 );
  ufAssert(
    !addi1.isMemoryAccess() && !addi1.isMemoryStore() &&
    !addi1.isMemoryLoad() && !addi1.isMove() && !addi1.isCall() &&
    !addi1.isIndirectCall() && !addi1.isReturn() && !addi1.isJump() &&
    !addi1.isConditionalJump() && !addi1.isUnconditionalJump() &&
    !addi1.isIndirectJump() && !addi1.isAsmDataDirective() &&
    !addi1.hasSideEffects() );

  WIR_Operation addi2(
    RV32I::OpCode::ADDI, RV32I::OperationFormat::RRL_2,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ),
    new WIR_LabelParameter( b ) );
  ufAssert( addi2.getSize() == 4 );
  ufAssert(
    !addi2.isMemoryAccess() && !addi2.isMemoryStore() &&
    !addi2.isMemoryLoad() && !addi2.isMove() && !addi2.isCall() &&
    !addi2.isIndirectCall() && !addi2.isReturn() && !addi2.isJump() &&
    !addi2.isConditionalJump() && !addi2.isUnconditionalJump() &&
    !addi2.isIndirectJump() && !addi2.isAsmDataDirective() &&
    !addi2.hasSideEffects() );

  WIR_Operation and1(
    RV32I::OpCode::AND, RV32I::OperationFormat::RRR_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ),
    new WIR_RegisterParameter( x3, WIR_Usage::use ) );
  ufAssert( and1.getSize() == 4 );
  ufAssert(
    !and1.isMemoryAccess() && !and1.isMemoryStore() && !and1.isMemoryLoad() &&
    !and1.isMove() && !and1.isCall() && !and1.isIndirectCall() &&
    !and1.isReturn() && !and1.isJump() && !and1.isConditionalJump() &&
    !and1.isUnconditionalJump() && !and1.isIndirectJump() &&
    !and1.isAsmDataDirective() && !and1.hasSideEffects() );

  WIR_Operation andi1(
    RV32I::OpCode::ANDI, RV32I::OperationFormat::RRC12_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ),
    new RV_Const12_Signed( -1023 ) );
  ufAssert( andi1.getSize() == 4 );
  ufAssert(
    !andi1.isMemoryAccess() && !andi1.isMemoryStore() &&
    !andi1.isMemoryLoad() && !andi1.isMove() && !andi1.isCall() &&
    !andi1.isIndirectCall() && !andi1.isReturn() && !andi1.isJump() &&
    !andi1.isConditionalJump() && !andi1.isUnconditionalJump() &&
    !andi1.isIndirectJump() && !andi1.isAsmDataDirective() &&
    !andi1.hasSideEffects() );

  WIR_Operation andi2(
    RV32I::OpCode::ANDI, RV32I::OperationFormat::RRL_2,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ),
    new WIR_LabelParameter( b ) );
  ufAssert( andi2.getSize() == 4 );
  ufAssert(
    !andi2.isMemoryAccess() && !andi2.isMemoryStore() &&
    !andi2.isMemoryLoad() && !andi2.isMove() && !andi2.isCall() &&
    !andi2.isIndirectCall() && !andi2.isReturn() && !andi2.isJump() &&
    !andi2.isConditionalJump() && !andi2.isUnconditionalJump() &&
    !andi2.isIndirectJump() && !andi2.isAsmDataDirective() &&
    !andi2.hasSideEffects() );

  WIR_Operation auipc1(
    RV32I::OpCode::AUIPC, RV32I::OperationFormat::RC20_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new RV_Const20_Unsigned( 500000 ) );
  ufAssert( auipc1.getSize() == 4 );
  ufAssert(
    !auipc1.isMemoryAccess() && !auipc1.isMemoryStore() &&
    !auipc1.isMemoryLoad() && !auipc1.isMove() && !auipc1.isCall() &&
    !auipc1.isIndirectCall() && !auipc1.isReturn() && !auipc1.isJump() &&
    !auipc1.isConditionalJump() && !auipc1.isUnconditionalJump() &&
    !auipc1.isIndirectJump() && !auipc1.isAsmDataDirective() &&
    !auipc1.hasSideEffects() );

  WIR_Operation auipc2(
    RV32I::OpCode::AUIPC, RV32I::OperationFormat::RL_2,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_LabelParameter( b ) );
  ufAssert( auipc2.getSize() == 4 );
  ufAssert(
    !auipc2.isMemoryAccess() && !auipc2.isMemoryStore() &&
    !auipc2.isMemoryLoad() && !auipc2.isMove() && !auipc2.isCall() &&
    !auipc2.isIndirectCall() && !auipc2.isReturn() && !auipc2.isJump() &&
    !auipc2.isConditionalJump() && !auipc2.isUnconditionalJump() &&
    !auipc2.isIndirectJump() && !auipc2.isAsmDataDirective() &&
    !auipc2.hasSideEffects() );

  WIR_Operation beq1(
    RV32I::OpCode::BEQ, RV32I::OperationFormat::RRL_1,
    new WIR_RegisterParameter( x1, WIR_Usage::use ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ),
    new WIR_LabelParameter( b ) );
  ufAssert( beq1.getSize() == 4 );
  ufAssert(
    !beq1.isMemoryAccess() && !beq1.isMemoryStore() && !beq1.isMemoryLoad() &&
    !beq1.isMove() && !beq1.isCall() && !beq1.isIndirectCall() &&
    !beq1.isReturn() && beq1.isJump() && beq1.isConditionalJump() &&
    !beq1.isUnconditionalJump() && !beq1.isIndirectJump() &&
    !beq1.isAsmDataDirective() && !beq1.hasSideEffects() );

  WIR_Operation bge1(
    RV32I::OpCode::BGE, RV32I::OperationFormat::RRL_1,
    new WIR_RegisterParameter( x1, WIR_Usage::use ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ),
    new WIR_LabelParameter( b ) );
  ufAssert( bge1.getSize() == 4 );
  ufAssert(
    !bge1.isMemoryAccess() && !bge1.isMemoryStore() && !bge1.isMemoryLoad() &&
    !bge1.isMove() && !bge1.isCall() && !bge1.isIndirectCall() &&
    !bge1.isReturn() && bge1.isJump() && bge1.isConditionalJump() &&
    !bge1.isUnconditionalJump() && !bge1.isIndirectJump() &&
    !bge1.isAsmDataDirective() && !bge1.hasSideEffects() );

  WIR_Operation bgeu1(
    RV32I::OpCode::BGEU, RV32I::OperationFormat::RRL_1,
    new WIR_RegisterParameter( x1, WIR_Usage::use ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ),
    new WIR_LabelParameter( b ) );
  ufAssert( bgeu1.getSize() == 4 );
  ufAssert(
    !bgeu1.isMemoryAccess() && !bgeu1.isMemoryStore() &&
    !bgeu1.isMemoryLoad() && !bgeu1.isMove() && !bgeu1.isCall() &&
    !bgeu1.isIndirectCall() && !bgeu1.isReturn() && bgeu1.isJump() &&
    bgeu1.isConditionalJump() && !bgeu1.isUnconditionalJump() &&
    !bgeu1.isIndirectJump() && !bgeu1.isAsmDataDirective() &&
    !bgeu1.hasSideEffects() );

  WIR_Operation blt1(
    RV32I::OpCode::BLT, RV32I::OperationFormat::RRL_1,
    new WIR_RegisterParameter( x1, WIR_Usage::use ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ),
    new WIR_LabelParameter( b ) );
  ufAssert( blt1.getSize() == 4 );
  ufAssert(
    !blt1.isMemoryAccess() && !blt1.isMemoryStore() && !blt1.isMemoryLoad() &&
    !blt1.isMove() && !blt1.isCall() && !blt1.isIndirectCall() &&
    !blt1.isReturn() && blt1.isJump() && blt1.isConditionalJump() &&
    !blt1.isUnconditionalJump() && !blt1.isIndirectJump() &&
    !blt1.isAsmDataDirective() && !blt1.hasSideEffects() );

  WIR_Operation bltu1(
    RV32I::OpCode::BLTU, RV32I::OperationFormat::RRL_1,
    new WIR_RegisterParameter( x1, WIR_Usage::use ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ),
    new WIR_LabelParameter( b ) );
  ufAssert( bltu1.getSize() == 4 );
  ufAssert(
    !bltu1.isMemoryAccess() && !bltu1.isMemoryStore() &&
    !bltu1.isMemoryLoad() && !bltu1.isMove() && !bltu1.isCall() &&
    !bltu1.isIndirectCall() && !bltu1.isReturn() && bltu1.isJump() &&
    bltu1.isConditionalJump() && !bltu1.isUnconditionalJump() &&
    !bltu1.isIndirectJump() && !bltu1.isAsmDataDirective() &&
    !bltu1.hasSideEffects() );

  WIR_Operation bne1(
    RV32I::OpCode::BNE, RV32I::OperationFormat::RRL_1,
    new WIR_RegisterParameter( x1, WIR_Usage::use ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ),
    new WIR_LabelParameter( b ) );
  ufAssert( bne1.getSize() == 4 );
  ufAssert(
    !bne1.isMemoryAccess() && !bne1.isMemoryStore() && !bne1.isMemoryLoad() &&
    !bne1.isMove() && !bne1.isCall() && !bne1.isIndirectCall() &&
    !bne1.isReturn() && bne1.isJump() && bne1.isConditionalJump() &&
    !bne1.isUnconditionalJump() && !bne1.isIndirectJump() &&
    !bne1.isAsmDataDirective() && !bne1.hasSideEffects() );

  WIR_Operation csrrc1(
    RV32I::OpCode::CSRRC, RV32I::OperationFormat::RSR_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_StringParameter( "frm" ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ) );
  ufAssert( csrrc1.getSize() == 4 );
  ufAssert(
    !csrrc1.isMemoryAccess() && !csrrc1.isMemoryStore() &&
    !csrrc1.isMemoryLoad() && !csrrc1.isMove() && !csrrc1.isCall() &&
    !csrrc1.isIndirectCall() && !csrrc1.isReturn() && !csrrc1.isJump() &&
    !csrrc1.isConditionalJump() && !csrrc1.isUnconditionalJump() &&
    !csrrc1.isIndirectJump() && !csrrc1.isAsmDataDirective() &&
    csrrc1.hasSideEffects() );

  WIR_Operation csrrc2(
    RV32I::OpCode::CSRRC, RV32I::OperationFormat::RSR_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_StringParameter( "fcsr" ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ) );
  ufAssert( csrrc2.getSize() == 4 );
  ufAssert(
    !csrrc2.isMemoryAccess() && !csrrc2.isMemoryStore() &&
    !csrrc2.isMemoryLoad() && !csrrc2.isMove() && !csrrc2.isCall() &&
    !csrrc2.isIndirectCall() && !csrrc2.isReturn() && !csrrc2.isJump() &&
    !csrrc2.isConditionalJump() && !csrrc2.isUnconditionalJump() &&
    !csrrc2.isIndirectJump() && !csrrc2.isAsmDataDirective() &&
    csrrc2.hasSideEffects() );

  WIR_Operation csrrc3(
    RV32I::OpCode::CSRRC, RV32I::OperationFormat::RSR_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_StringParameter( "fflags" ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ) );
  ufAssert( csrrc3.getSize() == 4 );
  ufAssert(
    !csrrc3.isMemoryAccess() && !csrrc3.isMemoryStore() &&
    !csrrc3.isMemoryLoad() && !csrrc3.isMove() && !csrrc3.isCall() &&
    !csrrc3.isIndirectCall() && !csrrc3.isReturn() && !csrrc3.isJump() &&
    !csrrc3.isConditionalJump() && !csrrc3.isUnconditionalJump() &&
    !csrrc3.isIndirectJump() && !csrrc3.isAsmDataDirective() &&
    csrrc3.hasSideEffects() );

  WIR_Operation csrrc4(
    RV32I::OpCode::CSRRC, RV32I::OperationFormat::RSR_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_StringParameter( "instret" ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ) );
  ufAssert( csrrc4.getSize() == 4 );
  ufAssert(
    !csrrc4.isMemoryAccess() && !csrrc4.isMemoryStore() &&
    !csrrc4.isMemoryLoad() && !csrrc4.isMove() && !csrrc4.isCall() &&
    !csrrc4.isIndirectCall() && !csrrc4.isReturn() && !csrrc4.isJump() &&
    !csrrc4.isConditionalJump() && !csrrc4.isUnconditionalJump() &&
    !csrrc4.isIndirectJump() && !csrrc4.isAsmDataDirective() &&
    csrrc4.hasSideEffects() );

  WIR_Operation csrrc5(
    RV32I::OpCode::CSRRC, RV32I::OperationFormat::RSR_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_StringParameter( "instreth" ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ) );
  ufAssert( csrrc5.getSize() == 4 );
  ufAssert(
    !csrrc5.isMemoryAccess() && !csrrc5.isMemoryStore() &&
    !csrrc5.isMemoryLoad() && !csrrc5.isMove() && !csrrc5.isCall() &&
    !csrrc5.isIndirectCall() && !csrrc5.isReturn() && !csrrc5.isJump() &&
    !csrrc5.isConditionalJump() && !csrrc5.isUnconditionalJump() &&
    !csrrc5.isIndirectJump() && !csrrc5.isAsmDataDirective() &&
    csrrc5.hasSideEffects() );

  WIR_Operation csrrc6(
    RV32I::OpCode::CSRRC, RV32I::OperationFormat::RSR_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_StringParameter( "cycle" ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ) );
  ufAssert( csrrc6.getSize() == 4 );
  ufAssert(
    !csrrc6.isMemoryAccess() && !csrrc6.isMemoryStore() &&
    !csrrc6.isMemoryLoad() && !csrrc6.isMove() && !csrrc6.isCall() &&
    !csrrc6.isIndirectCall() && !csrrc6.isReturn() && !csrrc6.isJump() &&
    !csrrc6.isConditionalJump() && !csrrc6.isUnconditionalJump() &&
    !csrrc6.isIndirectJump() && !csrrc6.isAsmDataDirective() &&
    csrrc6.hasSideEffects() );

  WIR_Operation csrrc7(
    RV32I::OpCode::CSRRC, RV32I::OperationFormat::RSR_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_StringParameter( "cycleh" ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ) );
  ufAssert( csrrc7.getSize() == 4 );
  ufAssert(
    !csrrc7.isMemoryAccess() && !csrrc7.isMemoryStore() &&
    !csrrc7.isMemoryLoad() && !csrrc7.isMove() && !csrrc7.isCall() &&
    !csrrc7.isIndirectCall() && !csrrc7.isReturn() && !csrrc7.isJump() &&
    !csrrc7.isConditionalJump() && !csrrc7.isUnconditionalJump() &&
    !csrrc7.isIndirectJump() && !csrrc7.isAsmDataDirective() &&
    csrrc7.hasSideEffects() );

  WIR_Operation csrrc8(
    RV32I::OpCode::CSRRC, RV32I::OperationFormat::RSR_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_StringParameter( "time" ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ) );
  ufAssert( csrrc8.getSize() == 4 );
  ufAssert(
    !csrrc8.isMemoryAccess() && !csrrc8.isMemoryStore() &&
    !csrrc8.isMemoryLoad() && !csrrc8.isMove() && !csrrc8.isCall() &&
    !csrrc8.isIndirectCall() && !csrrc8.isReturn() && !csrrc8.isJump() &&
    !csrrc8.isConditionalJump() && !csrrc8.isUnconditionalJump() &&
    !csrrc8.isIndirectJump() && !csrrc8.isAsmDataDirective() &&
    csrrc8.hasSideEffects() );

  WIR_Operation csrrc9(
    RV32I::OpCode::CSRRC, RV32I::OperationFormat::RSR_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_StringParameter( "timeh" ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ) );
  ufAssert( csrrc9.getSize() == 4 );
  ufAssert(
    !csrrc9.isMemoryAccess() && !csrrc9.isMemoryStore() &&
    !csrrc9.isMemoryLoad() && !csrrc9.isMove() && !csrrc9.isCall() &&
    !csrrc9.isIndirectCall() && !csrrc9.isReturn() && !csrrc9.isJump() &&
    !csrrc9.isConditionalJump() && !csrrc9.isUnconditionalJump() &&
    !csrrc9.isIndirectJump() && !csrrc9.isAsmDataDirective() &&
    csrrc9.hasSideEffects() );

  WIR_Operation csrrci1(
    RV32I::OpCode::CSRRCI, RV32I::OperationFormat::RSC5_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_StringParameter( "frm" ),
    new RV_Const5_Unsigned( 15 ) );
  ufAssert( csrrci1.getSize() == 4 );
  ufAssert(
    !csrrci1.isMemoryAccess() && !csrrci1.isMemoryStore() &&
    !csrrci1.isMemoryLoad() && !csrrci1.isMove() && !csrrci1.isCall() &&
    !csrrci1.isIndirectCall() && !csrrci1.isReturn() && !csrrci1.isJump() &&
    !csrrci1.isConditionalJump() && !csrrci1.isUnconditionalJump() &&
    !csrrci1.isIndirectJump() && !csrrci1.isAsmDataDirective() &&
    csrrci1.hasSideEffects() );

  WIR_Operation csrrci2(
    RV32I::OpCode::CSRRCI, RV32I::OperationFormat::RSC5_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_StringParameter( "fcsr" ),
    new RV_Const5_Unsigned( 15 ) );
  ufAssert( csrrci2.getSize() == 4 );
  ufAssert(
    !csrrci2.isMemoryAccess() && !csrrci2.isMemoryStore() &&
    !csrrci2.isMemoryLoad() && !csrrci2.isMove() && !csrrci2.isCall() &&
    !csrrci2.isIndirectCall() && !csrrci2.isReturn() && !csrrci2.isJump() &&
    !csrrci2.isConditionalJump() && !csrrci2.isUnconditionalJump() &&
    !csrrci2.isIndirectJump() && !csrrci2.isAsmDataDirective() &&
    csrrci2.hasSideEffects() );

  WIR_Operation csrrci3(
    RV32I::OpCode::CSRRCI, RV32I::OperationFormat::RSC5_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_StringParameter( "fflags" ),
    new RV_Const5_Unsigned( 15 ) );
  ufAssert( csrrci3.getSize() == 4 );
  ufAssert(
    !csrrci3.isMemoryAccess() && !csrrci3.isMemoryStore() &&
    !csrrci3.isMemoryLoad() && !csrrci3.isMove() && !csrrci3.isCall() &&
    !csrrci3.isIndirectCall() && !csrrci3.isReturn() && !csrrci3.isJump() &&
    !csrrci3.isConditionalJump() && !csrrci3.isUnconditionalJump() &&
    !csrrci3.isIndirectJump() && !csrrci3.isAsmDataDirective() &&
    csrrci3.hasSideEffects() );

  WIR_Operation csrrci4(
    RV32I::OpCode::CSRRCI, RV32I::OperationFormat::RSC5_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_StringParameter( "instret" ),
    new RV_Const5_Unsigned( 15 ) );
  ufAssert( csrrci4.getSize() == 4 );
  ufAssert(
    !csrrci4.isMemoryAccess() && !csrrci4.isMemoryStore() &&
    !csrrci4.isMemoryLoad() && !csrrci4.isMove() && !csrrci4.isCall() &&
    !csrrci4.isIndirectCall() && !csrrci4.isReturn() && !csrrci4.isJump() &&
    !csrrci4.isConditionalJump() && !csrrci4.isUnconditionalJump() &&
    !csrrci4.isIndirectJump() && !csrrci4.isAsmDataDirective() &&
    csrrci4.hasSideEffects() );

  WIR_Operation csrrci5(
    RV32I::OpCode::CSRRCI, RV32I::OperationFormat::RSC5_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_StringParameter( "instreth" ),
    new RV_Const5_Unsigned( 15 ) );
  ufAssert( csrrci5.getSize() == 4 );
  ufAssert(
    !csrrci5.isMemoryAccess() && !csrrci5.isMemoryStore() &&
    !csrrci5.isMemoryLoad() && !csrrci5.isMove() && !csrrci5.isCall() &&
    !csrrci5.isIndirectCall() && !csrrci5.isReturn() && !csrrci5.isJump() &&
    !csrrci5.isConditionalJump() && !csrrci5.isUnconditionalJump() &&
    !csrrci5.isIndirectJump() && !csrrci5.isAsmDataDirective() &&
    csrrci5.hasSideEffects() );

  WIR_Operation csrrci6(
    RV32I::OpCode::CSRRCI, RV32I::OperationFormat::RSC5_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_StringParameter( "cycle" ),
    new RV_Const5_Unsigned( 15 ) );
  ufAssert( csrrci6.getSize() == 4 );
  ufAssert(
    !csrrci6.isMemoryAccess() && !csrrci6.isMemoryStore() &&
    !csrrci6.isMemoryLoad() && !csrrci6.isMove() && !csrrci6.isCall() &&
    !csrrci6.isIndirectCall() && !csrrci6.isReturn() && !csrrci6.isJump() &&
    !csrrci6.isConditionalJump() && !csrrci6.isUnconditionalJump() &&
    !csrrci6.isIndirectJump() && !csrrci6.isAsmDataDirective() &&
    csrrci6.hasSideEffects() );

  WIR_Operation csrrci7(
    RV32I::OpCode::CSRRCI, RV32I::OperationFormat::RSC5_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_StringParameter( "cycleh" ),
    new RV_Const5_Unsigned( 15 ) );
  ufAssert( csrrci7.getSize() == 4 );
  ufAssert(
    !csrrci7.isMemoryAccess() && !csrrci7.isMemoryStore() &&
    !csrrci7.isMemoryLoad() && !csrrci7.isMove() && !csrrci7.isCall() &&
    !csrrci7.isIndirectCall() && !csrrci7.isReturn() && !csrrci7.isJump() &&
    !csrrci7.isConditionalJump() && !csrrci7.isUnconditionalJump() &&
    !csrrci7.isIndirectJump() && !csrrci7.isAsmDataDirective() &&
    csrrci7.hasSideEffects() );

  WIR_Operation csrrci8(
    RV32I::OpCode::CSRRCI, RV32I::OperationFormat::RSC5_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_StringParameter( "time" ),
    new RV_Const5_Unsigned( 15 ) );
  ufAssert( csrrci8.getSize() == 4 );
  ufAssert(
    !csrrci8.isMemoryAccess() && !csrrci8.isMemoryStore() &&
    !csrrci8.isMemoryLoad() && !csrrci8.isMove() && !csrrci8.isCall() &&
    !csrrci8.isIndirectCall() && !csrrci8.isReturn() && !csrrci8.isJump() &&
    !csrrci8.isConditionalJump() && !csrrci8.isUnconditionalJump() &&
    !csrrci8.isIndirectJump() && !csrrci8.isAsmDataDirective() &&
    csrrci8.hasSideEffects() );

  WIR_Operation csrrci9(
    RV32I::OpCode::CSRRCI, RV32I::OperationFormat::RSC5_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_StringParameter( "timeh" ),
    new RV_Const5_Unsigned( 15 ) );
  ufAssert( csrrci9.getSize() == 4 );
  ufAssert(
    !csrrci9.isMemoryAccess() && !csrrci9.isMemoryStore() &&
    !csrrci9.isMemoryLoad() && !csrrci9.isMove() && !csrrci9.isCall() &&
    !csrrci9.isIndirectCall() && !csrrci9.isReturn() && !csrrci9.isJump() &&
    !csrrci9.isConditionalJump() && !csrrci9.isUnconditionalJump() &&
    !csrrci9.isIndirectJump() && !csrrci9.isAsmDataDirective() &&
    csrrci9.hasSideEffects() );

  WIR_Operation csrrs1(
    RV32I::OpCode::CSRRS, RV32I::OperationFormat::RSR_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_StringParameter( "frm" ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ) );
  ufAssert( csrrs1.getSize() == 4 );
  ufAssert(
    !csrrs1.isMemoryAccess() && !csrrs1.isMemoryStore() &&
    !csrrs1.isMemoryLoad() && !csrrs1.isMove() && !csrrs1.isCall() &&
    !csrrs1.isIndirectCall() && !csrrs1.isReturn() && !csrrs1.isJump() &&
    !csrrs1.isConditionalJump() && !csrrs1.isUnconditionalJump() &&
    !csrrs1.isIndirectJump() && !csrrs1.isAsmDataDirective() &&
    csrrs1.hasSideEffects() );

  WIR_Operation csrrs2(
    RV32I::OpCode::CSRRS, RV32I::OperationFormat::RSR_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_StringParameter( "fcsr" ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ) );
  ufAssert( csrrs2.getSize() == 4 );
  ufAssert(
    !csrrs2.isMemoryAccess() && !csrrs2.isMemoryStore() &&
    !csrrs2.isMemoryLoad() && !csrrs2.isMove() && !csrrs2.isCall() &&
    !csrrs2.isIndirectCall() && !csrrs2.isReturn() && !csrrs2.isJump() &&
    !csrrs2.isConditionalJump() && !csrrs2.isUnconditionalJump() &&
    !csrrs2.isIndirectJump() && !csrrs2.isAsmDataDirective() &&
    csrrs2.hasSideEffects() );

  WIR_Operation csrrs3(
    RV32I::OpCode::CSRRS, RV32I::OperationFormat::RSR_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_StringParameter( "fflags" ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ) );
  ufAssert( csrrs3.getSize() == 4 );
  ufAssert(
    !csrrs3.isMemoryAccess() && !csrrs3.isMemoryStore() &&
    !csrrs3.isMemoryLoad() && !csrrs3.isMove() && !csrrs3.isCall() &&
    !csrrs3.isIndirectCall() && !csrrs3.isReturn() && !csrrs3.isJump() &&
    !csrrs3.isConditionalJump() && !csrrs3.isUnconditionalJump() &&
    !csrrs3.isIndirectJump() && !csrrs3.isAsmDataDirective() &&
    csrrs3.hasSideEffects() );

  WIR_Operation csrrs4(
    RV32I::OpCode::CSRRS, RV32I::OperationFormat::RSR_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_StringParameter( "instret" ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ) );
  ufAssert( csrrs4.getSize() == 4 );
  ufAssert(
    !csrrs4.isMemoryAccess() && !csrrs4.isMemoryStore() &&
    !csrrs4.isMemoryLoad() && !csrrs4.isMove() && !csrrs4.isCall() &&
    !csrrs4.isIndirectCall() && !csrrs4.isReturn() && !csrrs4.isJump() &&
    !csrrs4.isConditionalJump() && !csrrs4.isUnconditionalJump() &&
    !csrrs4.isIndirectJump() && !csrrs4.isAsmDataDirective() &&
    csrrs4.hasSideEffects() );

  WIR_Operation csrrs5(
    RV32I::OpCode::CSRRS, RV32I::OperationFormat::RSR_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_StringParameter( "instreth" ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ) );
  ufAssert( csrrs5.getSize() == 4 );
  ufAssert(
    !csrrs5.isMemoryAccess() && !csrrs5.isMemoryStore() &&
    !csrrs5.isMemoryLoad() && !csrrs5.isMove() && !csrrs5.isCall() &&
    !csrrs5.isIndirectCall() && !csrrs5.isReturn() && !csrrs5.isJump() &&
    !csrrs5.isConditionalJump() && !csrrs5.isUnconditionalJump() &&
    !csrrs5.isIndirectJump() && !csrrs5.isAsmDataDirective() &&
    csrrs5.hasSideEffects() );

  WIR_Operation csrrs6(
    RV32I::OpCode::CSRRS, RV32I::OperationFormat::RSR_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_StringParameter( "cycle" ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ) );
  ufAssert( csrrs6.getSize() == 4 );
  ufAssert(
    !csrrs6.isMemoryAccess() && !csrrs6.isMemoryStore() &&
    !csrrs6.isMemoryLoad() && !csrrs6.isMove() && !csrrs6.isCall() &&
    !csrrs6.isIndirectCall() && !csrrs6.isReturn() && !csrrs6.isJump() &&
    !csrrs6.isConditionalJump() && !csrrs6.isUnconditionalJump() &&
    !csrrs6.isIndirectJump() && !csrrs6.isAsmDataDirective() &&
    csrrs6.hasSideEffects() );

  WIR_Operation csrrs7(
    RV32I::OpCode::CSRRS, RV32I::OperationFormat::RSR_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_StringParameter( "cycleh" ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ) );
  ufAssert( csrrs7.getSize() == 4 );
  ufAssert(
    !csrrs7.isMemoryAccess() && !csrrs7.isMemoryStore() &&
    !csrrs7.isMemoryLoad() && !csrrs7.isMove() && !csrrs7.isCall() &&
    !csrrs7.isIndirectCall() && !csrrs7.isReturn() && !csrrs7.isJump() &&
    !csrrs7.isConditionalJump() && !csrrs7.isUnconditionalJump() &&
    !csrrs7.isIndirectJump() && !csrrs7.isAsmDataDirective() &&
    csrrs7.hasSideEffects() );

  WIR_Operation csrrs8(
    RV32I::OpCode::CSRRS, RV32I::OperationFormat::RSR_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_StringParameter( "time" ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ) );
  ufAssert( csrrs8.getSize() == 4 );
  ufAssert(
    !csrrs8.isMemoryAccess() && !csrrs8.isMemoryStore() &&
    !csrrs8.isMemoryLoad() && !csrrs8.isMove() && !csrrs8.isCall() &&
    !csrrs8.isIndirectCall() && !csrrs8.isReturn() && !csrrs8.isJump() &&
    !csrrs8.isConditionalJump() && !csrrs8.isUnconditionalJump() &&
    !csrrs8.isIndirectJump() && !csrrs8.isAsmDataDirective() &&
    csrrs8.hasSideEffects() );

  WIR_Operation csrrs9(
    RV32I::OpCode::CSRRS, RV32I::OperationFormat::RSR_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_StringParameter( "timeh" ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ) );
  ufAssert( csrrs9.getSize() == 4 );
  ufAssert(
    !csrrs9.isMemoryAccess() && !csrrs9.isMemoryStore() &&
    !csrrs9.isMemoryLoad() && !csrrs9.isMove() && !csrrs9.isCall() &&
    !csrrs9.isIndirectCall() && !csrrs9.isReturn() && !csrrs9.isJump() &&
    !csrrs9.isConditionalJump() && !csrrs9.isUnconditionalJump() &&
    !csrrs9.isIndirectJump() && !csrrs9.isAsmDataDirective() &&
    csrrs9.hasSideEffects() );

  WIR_Operation csrrsi1(
    RV32I::OpCode::CSRRSI, RV32I::OperationFormat::RSC5_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_StringParameter( "frm" ),
    new RV_Const5_Unsigned( 15 ) );
  ufAssert( csrrsi1.getSize() == 4 );
  ufAssert(
    !csrrsi1.isMemoryAccess() && !csrrsi1.isMemoryStore() &&
    !csrrsi1.isMemoryLoad() && !csrrsi1.isMove() && !csrrsi1.isCall() &&
    !csrrsi1.isIndirectCall() && !csrrsi1.isReturn() && !csrrsi1.isJump() &&
    !csrrsi1.isConditionalJump() && !csrrsi1.isUnconditionalJump() &&
    !csrrsi1.isIndirectJump() && !csrrsi1.isAsmDataDirective() &&
    csrrsi1.hasSideEffects() );

  WIR_Operation csrrsi2(
    RV32I::OpCode::CSRRSI, RV32I::OperationFormat::RSC5_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_StringParameter( "fcsr" ),
    new RV_Const5_Unsigned( 15 ) );
  ufAssert( csrrsi2.getSize() == 4 );
  ufAssert(
    !csrrsi2.isMemoryAccess() && !csrrsi2.isMemoryStore() &&
    !csrrsi2.isMemoryLoad() && !csrrsi2.isMove() && !csrrsi2.isCall() &&
    !csrrsi2.isIndirectCall() && !csrrsi2.isReturn() && !csrrsi2.isJump() &&
    !csrrsi2.isConditionalJump() && !csrrsi2.isUnconditionalJump() &&
    !csrrsi2.isIndirectJump() && !csrrsi2.isAsmDataDirective() &&
    csrrsi2.hasSideEffects() );

  WIR_Operation csrrsi3(
    RV32I::OpCode::CSRRSI, RV32I::OperationFormat::RSC5_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_StringParameter( "fflags" ),
    new RV_Const5_Unsigned( 15 ) );
  ufAssert( csrrsi3.getSize() == 4 );
  ufAssert(
    !csrrsi3.isMemoryAccess() && !csrrsi3.isMemoryStore() &&
    !csrrsi3.isMemoryLoad() && !csrrsi3.isMove() && !csrrsi3.isCall() &&
    !csrrsi3.isIndirectCall() && !csrrsi3.isReturn() && !csrrsi3.isJump() &&
    !csrrsi3.isConditionalJump() && !csrrsi3.isUnconditionalJump() &&
    !csrrsi3.isIndirectJump() && !csrrsi3.isAsmDataDirective() &&
    csrrsi3.hasSideEffects() );

  WIR_Operation csrrsi4(
    RV32I::OpCode::CSRRSI, RV32I::OperationFormat::RSC5_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_StringParameter( "instret" ),
    new RV_Const5_Unsigned( 15 ) );
  ufAssert( csrrsi4.getSize() == 4 );
  ufAssert(
    !csrrsi4.isMemoryAccess() && !csrrsi4.isMemoryStore() &&
    !csrrsi4.isMemoryLoad() && !csrrsi4.isMove() && !csrrsi4.isCall() &&
    !csrrsi4.isIndirectCall() && !csrrsi4.isReturn() && !csrrsi4.isJump() &&
    !csrrsi4.isConditionalJump() && !csrrsi4.isUnconditionalJump() &&
    !csrrsi4.isIndirectJump() && !csrrsi4.isAsmDataDirective() &&
    csrrsi4.hasSideEffects() );

  WIR_Operation csrrsi5(
    RV32I::OpCode::CSRRSI, RV32I::OperationFormat::RSC5_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_StringParameter( "instreth" ),
    new RV_Const5_Unsigned( 15 ) );
  ufAssert( csrrsi5.getSize() == 4 );
  ufAssert(
    !csrrsi5.isMemoryAccess() && !csrrsi5.isMemoryStore() &&
    !csrrsi5.isMemoryLoad() && !csrrsi5.isMove() && !csrrsi5.isCall() &&
    !csrrsi5.isIndirectCall() && !csrrsi5.isReturn() && !csrrsi5.isJump() &&
    !csrrsi5.isConditionalJump() && !csrrsi5.isUnconditionalJump() &&
    !csrrsi5.isIndirectJump() && !csrrsi5.isAsmDataDirective() &&
    csrrsi5.hasSideEffects() );

  WIR_Operation csrrsi6(
    RV32I::OpCode::CSRRSI, RV32I::OperationFormat::RSC5_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_StringParameter( "cycle" ),
    new RV_Const5_Unsigned( 15 ) );
  ufAssert( csrrsi6.getSize() == 4 );
  ufAssert(
    !csrrsi6.isMemoryAccess() && !csrrsi6.isMemoryStore() &&
    !csrrsi6.isMemoryLoad() && !csrrsi6.isMove() && !csrrsi6.isCall() &&
    !csrrsi6.isIndirectCall() && !csrrsi6.isReturn() && !csrrsi6.isJump() &&
    !csrrsi6.isConditionalJump() && !csrrsi6.isUnconditionalJump() &&
    !csrrsi6.isIndirectJump() && !csrrsi6.isAsmDataDirective() &&
    csrrsi6.hasSideEffects() );

  WIR_Operation csrrsi7(
    RV32I::OpCode::CSRRSI, RV32I::OperationFormat::RSC5_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_StringParameter( "cycleh" ),
    new RV_Const5_Unsigned( 15 ) );
  ufAssert( csrrsi7.getSize() == 4 );
  ufAssert(
    !csrrsi7.isMemoryAccess() && !csrrsi7.isMemoryStore() &&
    !csrrsi7.isMemoryLoad() && !csrrsi7.isMove() && !csrrsi7.isCall() &&
    !csrrsi7.isIndirectCall() && !csrrsi7.isReturn() && !csrrsi7.isJump() &&
    !csrrsi7.isConditionalJump() && !csrrsi7.isUnconditionalJump() &&
    !csrrsi7.isIndirectJump() && !csrrsi7.isAsmDataDirective() &&
    csrrsi7.hasSideEffects() );

  WIR_Operation csrrsi8(
    RV32I::OpCode::CSRRSI, RV32I::OperationFormat::RSC5_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_StringParameter( "time" ),
    new RV_Const5_Unsigned( 15 ) );
  ufAssert( csrrsi8.getSize() == 4 );
  ufAssert(
    !csrrsi8.isMemoryAccess() && !csrrsi8.isMemoryStore() &&
    !csrrsi8.isMemoryLoad() && !csrrsi8.isMove() && !csrrsi8.isCall() &&
    !csrrsi8.isIndirectCall() && !csrrsi8.isReturn() && !csrrsi8.isJump() &&
    !csrrsi8.isConditionalJump() && !csrrsi8.isUnconditionalJump() &&
    !csrrsi8.isIndirectJump() && !csrrsi8.isAsmDataDirective() &&
    csrrsi8.hasSideEffects() );

  WIR_Operation csrrsi9(
    RV32I::OpCode::CSRRSI, RV32I::OperationFormat::RSC5_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_StringParameter( "timeh" ),
    new RV_Const5_Unsigned( 15 ) );
  ufAssert( csrrsi9.getSize() == 4 );
  ufAssert(
    !csrrsi9.isMemoryAccess() && !csrrsi9.isMemoryStore() &&
    !csrrsi9.isMemoryLoad() && !csrrsi9.isMove() && !csrrsi9.isCall() &&
    !csrrsi9.isIndirectCall() && !csrrsi9.isReturn() && !csrrsi9.isJump() &&
    !csrrsi9.isConditionalJump() && !csrrsi9.isUnconditionalJump() &&
    !csrrsi9.isIndirectJump() && !csrrsi9.isAsmDataDirective() &&
    csrrsi9.hasSideEffects() );

  WIR_Operation csrrw1(
    RV32I::OpCode::CSRRW, RV32I::OperationFormat::RSR_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_StringParameter( "frm" ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ) );
  ufAssert( csrrw1.getSize() == 4 );
  ufAssert(
    !csrrw1.isMemoryAccess() && !csrrw1.isMemoryStore() &&
    !csrrw1.isMemoryLoad() && !csrrw1.isMove() && !csrrw1.isCall() &&
    !csrrw1.isIndirectCall() && !csrrw1.isReturn() && !csrrw1.isJump() &&
    !csrrw1.isConditionalJump() && !csrrw1.isUnconditionalJump() &&
    !csrrw1.isIndirectJump() && !csrrw1.isAsmDataDirective() &&
    csrrw1.hasSideEffects() );

  WIR_Operation csrrw2(
    RV32I::OpCode::CSRRW, RV32I::OperationFormat::RSR_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_StringParameter( "fcsr" ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ) );
  ufAssert( csrrw2.getSize() == 4 );
  ufAssert(
    !csrrw2.isMemoryAccess() && !csrrw2.isMemoryStore() &&
    !csrrw2.isMemoryLoad() && !csrrw2.isMove() && !csrrw2.isCall() &&
    !csrrw2.isIndirectCall() && !csrrw2.isReturn() && !csrrw2.isJump() &&
    !csrrw2.isConditionalJump() && !csrrw2.isUnconditionalJump() &&
    !csrrw2.isIndirectJump() && !csrrw2.isAsmDataDirective() &&
    csrrw2.hasSideEffects() );

  WIR_Operation csrrw3(
    RV32I::OpCode::CSRRW, RV32I::OperationFormat::RSR_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_StringParameter( "fflags" ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ) );
  ufAssert( csrrw3.getSize() == 4 );
  ufAssert(
    !csrrw3.isMemoryAccess() && !csrrw3.isMemoryStore() &&
    !csrrw3.isMemoryLoad() && !csrrw3.isMove() && !csrrw3.isCall() &&
    !csrrw3.isIndirectCall() && !csrrw3.isReturn() && !csrrw3.isJump() &&
    !csrrw3.isConditionalJump() && !csrrw3.isUnconditionalJump() &&
    !csrrw3.isIndirectJump() && !csrrw3.isAsmDataDirective() &&
    csrrw3.hasSideEffects() );

  WIR_Operation csrrw4(
    RV32I::OpCode::CSRRW, RV32I::OperationFormat::RSR_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_StringParameter( "instret" ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ) );
  ufAssert( csrrw4.getSize() == 4 );
  ufAssert(
    !csrrw4.isMemoryAccess() && !csrrw4.isMemoryStore() &&
    !csrrw4.isMemoryLoad() && !csrrw4.isMove() && !csrrw4.isCall() &&
    !csrrw4.isIndirectCall() && !csrrw4.isReturn() && !csrrw4.isJump() &&
    !csrrw4.isConditionalJump() && !csrrw4.isUnconditionalJump() &&
    !csrrw4.isIndirectJump() && !csrrw4.isAsmDataDirective() &&
    csrrw4.hasSideEffects() );

  WIR_Operation csrrw5(
    RV32I::OpCode::CSRRW, RV32I::OperationFormat::RSR_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_StringParameter( "instreth" ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ) );
  ufAssert( csrrw5.getSize() == 4 );
  ufAssert(
    !csrrw5.isMemoryAccess() && !csrrw5.isMemoryStore() &&
    !csrrw5.isMemoryLoad() && !csrrw5.isMove() && !csrrw5.isCall() &&
    !csrrw5.isIndirectCall() && !csrrw5.isReturn() && !csrrw5.isJump() &&
    !csrrw5.isConditionalJump() && !csrrw5.isUnconditionalJump() &&
    !csrrw5.isIndirectJump() && !csrrw5.isAsmDataDirective() &&
    csrrw5.hasSideEffects() );

  WIR_Operation csrrw6(
    RV32I::OpCode::CSRRW, RV32I::OperationFormat::RSR_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_StringParameter( "cycle" ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ) );
  ufAssert( csrrw6.getSize() == 4 );
  ufAssert(
    !csrrw6.isMemoryAccess() && !csrrw6.isMemoryStore() &&
    !csrrw6.isMemoryLoad() && !csrrw6.isMove() && !csrrw6.isCall() &&
    !csrrw6.isIndirectCall() && !csrrw6.isReturn() && !csrrw6.isJump() &&
    !csrrw6.isConditionalJump() && !csrrw6.isUnconditionalJump() &&
    !csrrw6.isIndirectJump() && !csrrw6.isAsmDataDirective() &&
    csrrw6.hasSideEffects() );

  WIR_Operation csrrw7(
    RV32I::OpCode::CSRRW, RV32I::OperationFormat::RSR_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_StringParameter( "cycleh" ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ) );
  ufAssert( csrrw7.getSize() == 4 );
  ufAssert(
    !csrrw7.isMemoryAccess() && !csrrw7.isMemoryStore() &&
    !csrrw7.isMemoryLoad() && !csrrw7.isMove() && !csrrw7.isCall() &&
    !csrrw7.isIndirectCall() && !csrrw7.isReturn() && !csrrw7.isJump() &&
    !csrrw7.isConditionalJump() && !csrrw7.isUnconditionalJump() &&
    !csrrw7.isIndirectJump() && !csrrw7.isAsmDataDirective() &&
    csrrw7.hasSideEffects() );

  WIR_Operation csrrw8(
    RV32I::OpCode::CSRRW, RV32I::OperationFormat::RSR_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_StringParameter( "time" ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ) );
  ufAssert( csrrw8.getSize() == 4 );
  ufAssert(
    !csrrw8.isMemoryAccess() && !csrrw8.isMemoryStore() &&
    !csrrw8.isMemoryLoad() && !csrrw8.isMove() && !csrrw8.isCall() &&
    !csrrw8.isIndirectCall() && !csrrw8.isReturn() && !csrrw8.isJump() &&
    !csrrw8.isConditionalJump() && !csrrw8.isUnconditionalJump() &&
    !csrrw8.isIndirectJump() && !csrrw8.isAsmDataDirective() &&
    csrrw8.hasSideEffects() );

  WIR_Operation csrrw9(
    RV32I::OpCode::CSRRW, RV32I::OperationFormat::RSR_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_StringParameter( "timeh" ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ) );
  ufAssert( csrrw9.getSize() == 4 );
  ufAssert(
    !csrrw9.isMemoryAccess() && !csrrw9.isMemoryStore() &&
    !csrrw9.isMemoryLoad() && !csrrw9.isMove() && !csrrw9.isCall() &&
    !csrrw9.isIndirectCall() && !csrrw9.isReturn() && !csrrw9.isJump() &&
    !csrrw9.isConditionalJump() && !csrrw9.isUnconditionalJump() &&
    !csrrw9.isIndirectJump() && !csrrw9.isAsmDataDirective() &&
    csrrw9.hasSideEffects() );

  WIR_Operation csrrwi1(
    RV32I::OpCode::CSRRWI, RV32I::OperationFormat::RSC5_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_StringParameter( "frm" ),
    new RV_Const5_Unsigned( 15 ) );
  ufAssert( csrrwi1.getSize() == 4 );
  ufAssert(
    !csrrwi1.isMemoryAccess() && !csrrwi1.isMemoryStore() &&
    !csrrwi1.isMemoryLoad() && !csrrwi1.isMove() && !csrrwi1.isCall() &&
    !csrrwi1.isIndirectCall() && !csrrwi1.isReturn() && !csrrwi1.isJump() &&
    !csrrwi1.isConditionalJump() && !csrrwi1.isUnconditionalJump() &&
    !csrrwi1.isIndirectJump() && !csrrwi1.isAsmDataDirective() &&
    csrrwi1.hasSideEffects() );

  WIR_Operation csrrwi2(
    RV32I::OpCode::CSRRWI, RV32I::OperationFormat::RSC5_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_StringParameter( "fcsr" ),
    new RV_Const5_Unsigned( 15 ) );
  ufAssert( csrrwi2.getSize() == 4 );
  ufAssert(
    !csrrwi2.isMemoryAccess() && !csrrwi2.isMemoryStore() &&
    !csrrwi2.isMemoryLoad() && !csrrwi2.isMove() && !csrrwi2.isCall() &&
    !csrrwi2.isIndirectCall() && !csrrwi2.isReturn() && !csrrwi2.isJump() &&
    !csrrwi2.isConditionalJump() && !csrrwi2.isUnconditionalJump() &&
    !csrrwi2.isIndirectJump() && !csrrwi2.isAsmDataDirective() &&
    csrrwi2.hasSideEffects() );

  WIR_Operation csrrwi3(
    RV32I::OpCode::CSRRWI, RV32I::OperationFormat::RSC5_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_StringParameter( "fflags" ),
    new RV_Const5_Unsigned( 15 ) );
  ufAssert( csrrwi3.getSize() == 4 );
  ufAssert(
    !csrrwi3.isMemoryAccess() && !csrrwi3.isMemoryStore() &&
    !csrrwi3.isMemoryLoad() && !csrrwi3.isMove() && !csrrwi3.isCall() &&
    !csrrwi3.isIndirectCall() && !csrrwi3.isReturn() && !csrrwi3.isJump() &&
    !csrrwi3.isConditionalJump() && !csrrwi3.isUnconditionalJump() &&
    !csrrwi3.isIndirectJump() && !csrrwi3.isAsmDataDirective() &&
    csrrwi3.hasSideEffects() );

  WIR_Operation csrrwi4(
    RV32I::OpCode::CSRRWI, RV32I::OperationFormat::RSC5_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_StringParameter( "instret" ),
    new RV_Const5_Unsigned( 15 ) );
  ufAssert( csrrwi4.getSize() == 4 );
  ufAssert(
    !csrrwi4.isMemoryAccess() && !csrrwi4.isMemoryStore() &&
    !csrrwi4.isMemoryLoad() && !csrrwi4.isMove() && !csrrwi4.isCall() &&
    !csrrwi4.isIndirectCall() && !csrrwi4.isReturn() && !csrrwi4.isJump() &&
    !csrrwi4.isConditionalJump() && !csrrwi4.isUnconditionalJump() &&
    !csrrwi4.isIndirectJump() && !csrrwi4.isAsmDataDirective() &&
    csrrwi4.hasSideEffects() );

  WIR_Operation csrrwi5(
    RV32I::OpCode::CSRRWI, RV32I::OperationFormat::RSC5_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_StringParameter( "instreth" ),
    new RV_Const5_Unsigned( 15 ) );
  ufAssert( csrrwi5.getSize() == 4 );
  ufAssert(
    !csrrwi5.isMemoryAccess() && !csrrwi5.isMemoryStore() &&
    !csrrwi5.isMemoryLoad() && !csrrwi5.isMove() && !csrrwi5.isCall() &&
    !csrrwi5.isIndirectCall() && !csrrwi5.isReturn() && !csrrwi5.isJump() &&
    !csrrwi5.isConditionalJump() && !csrrwi5.isUnconditionalJump() &&
    !csrrwi5.isIndirectJump() && !csrrwi5.isAsmDataDirective() &&
    csrrwi5.hasSideEffects() );

  WIR_Operation csrrwi6(
    RV32I::OpCode::CSRRWI, RV32I::OperationFormat::RSC5_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_StringParameter( "cycle" ),
    new RV_Const5_Unsigned( 15 ) );
  ufAssert( csrrwi6.getSize() == 4 );
  ufAssert(
    !csrrwi6.isMemoryAccess() && !csrrwi6.isMemoryStore() &&
    !csrrwi6.isMemoryLoad() && !csrrwi6.isMove() && !csrrwi6.isCall() &&
    !csrrwi6.isIndirectCall() && !csrrwi6.isReturn() && !csrrwi6.isJump() &&
    !csrrwi6.isConditionalJump() && !csrrwi6.isUnconditionalJump() &&
    !csrrwi6.isIndirectJump() && !csrrwi6.isAsmDataDirective() &&
    csrrwi6.hasSideEffects() );

  WIR_Operation csrrwi7(
    RV32I::OpCode::CSRRWI, RV32I::OperationFormat::RSC5_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_StringParameter( "cycleh" ),
    new RV_Const5_Unsigned( 15 ) );
  ufAssert( csrrwi7.getSize() == 4 );
  ufAssert(
    !csrrwi7.isMemoryAccess() && !csrrwi7.isMemoryStore() &&
    !csrrwi7.isMemoryLoad() && !csrrwi7.isMove() && !csrrwi7.isCall() &&
    !csrrwi7.isIndirectCall() && !csrrwi7.isReturn() && !csrrwi7.isJump() &&
    !csrrwi7.isConditionalJump() && !csrrwi7.isUnconditionalJump() &&
    !csrrwi7.isIndirectJump() && !csrrwi7.isAsmDataDirective() &&
    csrrwi7.hasSideEffects() );

  WIR_Operation csrrwi8(
    RV32I::OpCode::CSRRWI, RV32I::OperationFormat::RSC5_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_StringParameter( "time" ),
    new RV_Const5_Unsigned( 15 ) );
  ufAssert( csrrwi8.getSize() == 4 );
  ufAssert(
    !csrrwi8.isMemoryAccess() && !csrrwi8.isMemoryStore() &&
    !csrrwi8.isMemoryLoad() && !csrrwi8.isMove() && !csrrwi8.isCall() &&
    !csrrwi8.isIndirectCall() && !csrrwi8.isReturn() && !csrrwi8.isJump() &&
    !csrrwi8.isConditionalJump() && !csrrwi8.isUnconditionalJump() &&
    !csrrwi8.isIndirectJump() && !csrrwi8.isAsmDataDirective() &&
    csrrwi8.hasSideEffects() );

  WIR_Operation csrrwi9(
    RV32I::OpCode::CSRRWI, RV32I::OperationFormat::RSC5_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_StringParameter( "timeh" ),
    new RV_Const5_Unsigned( 15 ) );
  ufAssert( csrrwi9.getSize() == 4 );
  ufAssert(
    !csrrwi9.isMemoryAccess() && !csrrwi9.isMemoryStore() &&
    !csrrwi9.isMemoryLoad() && !csrrwi9.isMove() && !csrrwi9.isCall() &&
    !csrrwi9.isIndirectCall() && !csrrwi9.isReturn() && !csrrwi9.isJump() &&
    !csrrwi9.isConditionalJump() && !csrrwi9.isUnconditionalJump() &&
    !csrrwi9.isIndirectJump() && !csrrwi9.isAsmDataDirective() &&
    csrrwi9.hasSideEffects() );

  WIR_Operation ebreak1(
    RV32I::OpCode::EBREAK, RV32I::OperationFormat::NULL_1 );
  ufAssert( ebreak1.getSize() == 4 );
  ufAssert(
    !ebreak1.isMemoryAccess() && !ebreak1.isMemoryStore() &&
    !ebreak1.isMemoryLoad() && !ebreak1.isMove() && !ebreak1.isCall() &&
    !ebreak1.isIndirectCall() && !ebreak1.isReturn() && !ebreak1.isJump() &&
    !ebreak1.isConditionalJump() && !ebreak1.isUnconditionalJump() &&
    !ebreak1.isIndirectJump() && !ebreak1.isAsmDataDirective() &&
    ebreak1.hasSideEffects() );

  WIR_Operation ecall1(
    RV32I::OpCode::ECALL, RV32I::OperationFormat::NULL_1 );
  ufAssert( ecall1.getSize() == 4 );
  ufAssert(
    !ecall1.isMemoryAccess() && !ecall1.isMemoryStore() &&
    !ecall1.isMemoryLoad() && !ecall1.isMove() && !ecall1.isCall() &&
    !ecall1.isIndirectCall() && !ecall1.isReturn() && !ecall1.isJump() &&
    !ecall1.isConditionalJump() && !ecall1.isUnconditionalJump() &&
    !ecall1.isIndirectJump() && !ecall1.isAsmDataDirective() &&
    ecall1.hasSideEffects() );

  WIR_Operation j1(
    RV32I::OpCode::J, RV32I::OperationFormat::L_1,
    new WIR_LabelParameter( b ) );
  ufAssert( j1.getSize() == 4 );
  ufAssert(
    !j1.isMemoryAccess() && !j1.isMemoryStore() && !j1.isMemoryLoad() &&
    !j1.isMove() && !j1.isCall() && !j1.isIndirectCall() &&
    !j1.isReturn() && j1.isJump() && !j1.isConditionalJump() &&
    j1.isUnconditionalJump() && !j1.isIndirectJump() &&
    !j1.isAsmDataDirective() && !j1.hasSideEffects() );

  WIR_Operation jal1(
    RV32I::OpCode::JAL, RV32I::OperationFormat::RL_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_LabelParameter( b ) );
  ufAssert( jal1.getSize() == 4 );
  ufAssert(
    !jal1.isMemoryAccess() && !jal1.isMemoryStore() && !jal1.isMemoryLoad() &&
    !jal1.isMove() && jal1.isCall() && !jal1.isIndirectCall() &&
    !jal1.isReturn() && !jal1.isJump() && !jal1.isConditionalJump() &&
    !jal1.isUnconditionalJump() && !jal1.isIndirectJump() &&
    !jal1.isAsmDataDirective() && !jal1.hasSideEffects() );

  WIR_Operation jalr1(
    RV32I::OpCode::JALR, RV32I::OperationFormat::RC12R_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new RV_Const12_Signed( -4 ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ) );
  ufAssert( jalr1.getSize() == 4 );
  ufAssert(
    !jalr1.isMemoryAccess() && !jalr1.isMemoryStore() &&
    !jalr1.isMemoryLoad() && !jalr1.isMove() && !jalr1.isCall() &&
    !jalr1.isIndirectCall() && !jalr1.isReturn() && jalr1.isJump() &&
    !jalr1.isConditionalJump() && !jalr1.isUnconditionalJump() &&
    jalr1.isIndirectJump() && !jalr1.isAsmDataDirective() &&
    !jalr1.hasSideEffects() );

  WIR_Operation jalr2(
    RV32I::OpCode::JALR, RV32I::OperationFormat::RLR_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_LabelParameter( b ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ) );
  ufAssert( jalr2.getSize() == 4 );
  ufAssert(
    !jalr2.isMemoryAccess() && !jalr2.isMemoryStore() &&
    !jalr2.isMemoryLoad() && !jalr2.isMove() && !jalr2.isCall() &&
    !jalr2.isIndirectCall() && !jalr2.isReturn() && jalr2.isJump() &&
    !jalr2.isConditionalJump() && !jalr2.isUnconditionalJump() &&
    jalr2.isIndirectJump() && !jalr2.isAsmDataDirective() &&
    !jalr2.hasSideEffects() );

  WIR_Operation jalr3(
    RV32I::OpCode::JALR, RV32I::OperationFormat::RRC12_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ),
    new RV_Const12_Signed( -4 ) );
  ufAssert( jalr3.getSize() == 4 );
  ufAssert(
    !jalr3.isMemoryAccess() && !jalr3.isMemoryStore() &&
    !jalr3.isMemoryLoad() && !jalr3.isMove() && !jalr3.isCall() &&
    !jalr3.isIndirectCall() && !jalr3.isReturn() && jalr3.isJump() &&
    !jalr3.isConditionalJump() && !jalr3.isUnconditionalJump() &&
    jalr3.isIndirectJump() && !jalr3.isAsmDataDirective() &&
    !jalr3.hasSideEffects() );

  WIR_Operation jalr4(
    RV32I::OpCode::JALR, RV32I::OperationFormat::RRL_2,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ),
    new WIR_LabelParameter( b ) );
  ufAssert( jalr4.getSize() == 4 );
  ufAssert(
    !jalr4.isMemoryAccess() && !jalr4.isMemoryStore() && !jalr4.isMemoryLoad() &&
    !jalr4.isMove() && !jalr4.isCall() && !jalr4.isIndirectCall() &&
    !jalr4.isReturn() && jalr4.isJump() && !jalr4.isConditionalJump() &&
    !jalr4.isUnconditionalJump() && jalr4.isIndirectJump() &&
    !jalr4.isAsmDataDirective() && !jalr4.hasSideEffects() );

  WIR_Operation lb1(
    RV32I::OpCode::LB, RV32I::OperationFormat::RC12R_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new RV_Const12_Signed( -1023 ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ) );
  ufAssert( lb1.getSize() == 4 );
  ufAssert(
    !lb1.isMemoryAccess() && !lb1.isMemoryStore() && lb1.isMemoryLoad() &&
    !lb1.isMove() && !lb1.isCall() && !lb1.isIndirectCall() &&
    !lb1.isReturn() && !lb1.isJump() && !lb1.isConditionalJump() &&
    !lb1.isUnconditionalJump() && !lb1.isIndirectJump() &&
    !lb1.isAsmDataDirective() && !lb1.hasSideEffects() );

  WIR_Operation lb2(
    RV32I::OpCode::LB, RV32I::OperationFormat::RLR_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_LabelParameter( b ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ) );
  ufAssert( lb2.getSize() == 4 );
  ufAssert(
    !lb2.isMemoryAccess() && !lb2.isMemoryStore() && lb2.isMemoryLoad() &&
    !lb2.isMove() && !lb2.isCall() && !lb2.isIndirectCall() &&
    !lb2.isReturn() && !lb2.isJump() && !lb2.isConditionalJump() &&
    !lb2.isUnconditionalJump() && !lb2.isIndirectJump() &&
    !lb2.isAsmDataDirective() && !lb2.hasSideEffects() );

  WIR_Operation lbu1(
    RV32I::OpCode::LBU, RV32I::OperationFormat::RC12R_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new RV_Const12_Signed( -1023 ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ) );
  ufAssert( lbu1.getSize() == 4 );
  ufAssert(
    !lbu1.isMemoryAccess() && !lbu1.isMemoryStore() && lbu1.isMemoryLoad() &&
    !lbu1.isMove() && !lbu1.isCall() && !lbu1.isIndirectCall() &&
    !lbu1.isReturn() && !lbu1.isJump() && !lbu1.isConditionalJump() &&
    !lbu1.isUnconditionalJump() && !lbu1.isIndirectJump() &&
    !lbu1.isAsmDataDirective() && !lbu1.hasSideEffects() );

  WIR_Operation lbu2(
    RV32I::OpCode::LBU, RV32I::OperationFormat::RLR_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_LabelParameter( b ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ) );
  ufAssert( lbu2.getSize() == 4 );
  ufAssert(
    !lbu2.isMemoryAccess() && !lbu2.isMemoryStore() && lbu2.isMemoryLoad() &&
    !lbu2.isMove() && !lbu2.isCall() && !lbu2.isIndirectCall() &&
    !lbu2.isReturn() && !lbu2.isJump() && !lbu2.isConditionalJump() &&
    !lbu2.isUnconditionalJump() && !lbu2.isIndirectJump() &&
    !lbu2.isAsmDataDirective() && !lbu2.hasSideEffects() );

  WIR_Operation lh1(
    RV32I::OpCode::LH, RV32I::OperationFormat::RC12R_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new RV_Const12_Signed( -1023 ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ) );
  ufAssert( lh1.getSize() == 4 );
  ufAssert(
    !lh1.isMemoryAccess() && !lh1.isMemoryStore() && lh1.isMemoryLoad() &&
    !lh1.isMove() && !lh1.isCall() && !lh1.isIndirectCall() &&
    !lh1.isReturn() && !lh1.isJump() && !lh1.isConditionalJump() &&
    !lh1.isUnconditionalJump() && !lh1.isIndirectJump() &&
    !lh1.isAsmDataDirective() && !lh1.hasSideEffects() );

  WIR_Operation lh2(
    RV32I::OpCode::LH, RV32I::OperationFormat::RLR_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_LabelParameter( b ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ) );
  ufAssert( lh2.getSize() == 4 );
  ufAssert(
    !lh2.isMemoryAccess() && !lh2.isMemoryStore() && lh2.isMemoryLoad() &&
    !lh2.isMove() && !lh2.isCall() && !lh2.isIndirectCall() &&
    !lh2.isReturn() && !lh2.isJump() && !lh2.isConditionalJump() &&
    !lh2.isUnconditionalJump() && !lh2.isIndirectJump() &&
    !lh2.isAsmDataDirective() && !lh2.hasSideEffects() );

  WIR_Operation lhu1(
    RV32I::OpCode::LHU, RV32I::OperationFormat::RC12R_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new RV_Const12_Signed( -1023 ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ) );
  ufAssert( lhu1.getSize() == 4 );
  ufAssert(
    !lhu1.isMemoryAccess() && !lhu1.isMemoryStore() && lhu1.isMemoryLoad() &&
    !lhu1.isMove() && !lhu1.isCall() && !lhu1.isIndirectCall() &&
    !lhu1.isReturn() && !lhu1.isJump() && !lhu1.isConditionalJump() &&
    !lhu1.isUnconditionalJump() && !lhu1.isIndirectJump() &&
    !lhu1.isAsmDataDirective() && !lhu1.hasSideEffects() );

  WIR_Operation lhu2(
    RV32I::OpCode::LHU, RV32I::OperationFormat::RLR_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_LabelParameter( b ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ) );
  ufAssert( lhu2.getSize() == 4 );
  ufAssert(
    !lhu2.isMemoryAccess() && !lhu2.isMemoryStore() && lhu2.isMemoryLoad() &&
    !lhu2.isMove() && !lhu2.isCall() && !lhu2.isIndirectCall() &&
    !lhu2.isReturn() && !lhu2.isJump() && !lhu2.isConditionalJump() &&
    !lhu2.isUnconditionalJump() && !lhu2.isIndirectJump() &&
    !lhu2.isAsmDataDirective() && !lhu2.hasSideEffects() );

  WIR_Operation lui1(
    RV32I::OpCode::LUI, RV32I::OperationFormat::RC20_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new RV_Const20_Unsigned( 500000 ) );
  ufAssert( lui1.getSize() == 4 );
  ufAssert(
    !lui1.isMemoryAccess() && !lui1.isMemoryStore() && !lui1.isMemoryLoad() &&
    !lui1.isMove() && !lui1.isCall() && !lui1.isIndirectCall() &&
    !lui1.isReturn() && !lui1.isJump() && !lui1.isConditionalJump() &&
    !lui1.isUnconditionalJump() && !lui1.isIndirectJump() &&
    !lui1.isAsmDataDirective() && !lui1.hasSideEffects() );

  WIR_Operation lui2(
    RV32I::OpCode::LUI, RV32I::OperationFormat::RL_2,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_LabelParameter( b ) );
  ufAssert( lui2.getSize() == 4 );
  ufAssert(
    !lui2.isMemoryAccess() && !lui2.isMemoryStore() && !lui2.isMemoryLoad() &&
    !lui2.isMove() && !lui2.isCall() && !lui2.isIndirectCall() &&
    !lui2.isReturn() && !lui2.isJump() && !lui2.isConditionalJump() &&
    !lui2.isUnconditionalJump() && !lui2.isIndirectJump() &&
    !lui2.isAsmDataDirective() && !lui2.hasSideEffects() );

  WIR_Operation lw1(
    RV32I::OpCode::LW, RV32I::OperationFormat::RC12R_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new RV_Const12_Signed( -1023 ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ) );
  ufAssert( lw1.getSize() == 4 );
  ufAssert(
    !lw1.isMemoryAccess() && !lw1.isMemoryStore() && lw1.isMemoryLoad() &&
    !lw1.isMove() && !lw1.isCall() && !lw1.isIndirectCall() &&
    !lw1.isReturn() && !lw1.isJump() && !lw1.isConditionalJump() &&
    !lw1.isUnconditionalJump() && !lw1.isIndirectJump() &&
    !lw1.isAsmDataDirective() && !lw1.hasSideEffects() );

  WIR_Operation lw2(
    RV32I::OpCode::LW, RV32I::OperationFormat::RLR_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_LabelParameter( b ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ) );
  ufAssert( lw2.getSize() == 4 );
  ufAssert(
    !lw2.isMemoryAccess() && !lw2.isMemoryStore() && lw2.isMemoryLoad() &&
    !lw2.isMove() && !lw2.isCall() && !lw2.isIndirectCall() &&
    !lw2.isReturn() && !lw2.isJump() && !lw2.isConditionalJump() &&
    !lw2.isUnconditionalJump() && !lw2.isIndirectJump() &&
    !lw2.isAsmDataDirective() && !lw2.hasSideEffects() );

  WIR_Operation mov(
    RV32I::OpCode::MOV, RV32I::OperationFormat::RR_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ) );
  ufAssert( mov.getSize() == 4 );
  ufAssert(
    !mov.isMemoryAccess() && !mov.isMemoryStore() &&
    !mov.isMemoryLoad() && mov.isMove() && !mov.isCall() &&
    !mov.isIndirectCall() && !mov.isReturn() && !mov.isJump() &&
    !mov.isConditionalJump() && !mov.isUnconditionalJump() &&
    !mov.isIndirectJump() && !mov.isAsmDataDirective() &&
    !mov.hasSideEffects() );

  WIR_Operation or1(
    RV32I::OpCode::OR, RV32I::OperationFormat::RRR_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ),
    new WIR_RegisterParameter( x3, WIR_Usage::use ) );
  ufAssert( or1.getSize() == 4 );
  ufAssert(
    !or1.isMemoryAccess() && !or1.isMemoryStore() && !or1.isMemoryLoad() &&
    !or1.isMove() && !or1.isCall() && !or1.isIndirectCall() &&
    !or1.isReturn() && !or1.isJump() && !or1.isConditionalJump() &&
    !or1.isUnconditionalJump() && !or1.isIndirectJump() &&
    !or1.isAsmDataDirective() && !or1.hasSideEffects() );

  WIR_Operation ori1(
    RV32I::OpCode::ORI, RV32I::OperationFormat::RRC12_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ),
    new RV_Const12_Signed( -1023 ) );
  ufAssert( ori1.getSize() == 4 );
  ufAssert(
    !ori1.isMemoryAccess() && !ori1.isMemoryStore() && !ori1.isMemoryLoad() &&
    !ori1.isMove() && !ori1.isCall() && !ori1.isIndirectCall() &&
    !ori1.isReturn() && !ori1.isJump() && !ori1.isConditionalJump() &&
    !ori1.isUnconditionalJump() && !ori1.isIndirectJump() &&
    !ori1.isAsmDataDirective() && !ori1.hasSideEffects() );

  WIR_Operation ori2(
    RV32I::OpCode::ORI, RV32I::OperationFormat::RRL_2,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ),
    new WIR_LabelParameter( b ) );
  ufAssert( ori2.getSize() == 4 );
  ufAssert(
    !ori2.isMemoryAccess() && !ori2.isMemoryStore() && !ori2.isMemoryLoad() &&
    !ori2.isMove() && !ori2.isCall() && !ori2.isIndirectCall() &&
    !ori2.isReturn() && !ori2.isJump() && !ori2.isConditionalJump() &&
    !ori2.isUnconditionalJump() && !ori2.isIndirectJump() &&
    !ori2.isAsmDataDirective() && !ori2.hasSideEffects() );

  WIR_Operation sb1(
    RV32I::OpCode::SB, RV32I::OperationFormat::RC12R_2,
    new WIR_RegisterParameter( x1, WIR_Usage::use ),
    new RV_Const12_Signed( -1023 ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ) );
  ufAssert( sb1.getSize() == 4 );
  ufAssert(
    !sb1.isMemoryAccess() && sb1.isMemoryStore() && !sb1.isMemoryLoad() &&
    !sb1.isMove() && !sb1.isCall() && !sb1.isIndirectCall() &&
    !sb1.isReturn() && !sb1.isJump() && !sb1.isConditionalJump() &&
    !sb1.isUnconditionalJump() && !sb1.isIndirectJump() &&
    !sb1.isAsmDataDirective() && !sb1.hasSideEffects() );

  WIR_Operation sb2(
    RV32I::OpCode::SB, RV32I::OperationFormat::RLR_2,
    new WIR_RegisterParameter( x1, WIR_Usage::use ),
    new WIR_LabelParameter( b ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ) );
  ufAssert( sb2.getSize() == 4 );
  ufAssert(
    !sb2.isMemoryAccess() && sb2.isMemoryStore() && !sb2.isMemoryLoad() &&
    !sb2.isMove() && !sb2.isCall() && !sb2.isIndirectCall() &&
    !sb2.isReturn() && !sb2.isJump() && !sb2.isConditionalJump() &&
    !sb2.isUnconditionalJump() && !sb2.isIndirectJump() &&
    !sb2.isAsmDataDirective() && !sb2.hasSideEffects() );

  WIR_Operation sh1(
    RV32I::OpCode::SH, RV32I::OperationFormat::RC12R_2,
    new WIR_RegisterParameter( x1, WIR_Usage::use ),
    new RV_Const12_Signed( -1023 ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ) );
  ufAssert( sh1.getSize() == 4 );
  ufAssert(
    !sh1.isMemoryAccess() && sh1.isMemoryStore() && !sh1.isMemoryLoad() &&
    !sh1.isMove() && !sh1.isCall() && !sh1.isIndirectCall() &&
    !sh1.isReturn() && !sh1.isJump() && !sh1.isConditionalJump() &&
    !sh1.isUnconditionalJump() && !sh1.isIndirectJump() &&
    !sh1.isAsmDataDirective() && !sh1.hasSideEffects() );

  WIR_Operation sh2(
    RV32I::OpCode::SH, RV32I::OperationFormat::RLR_2,
    new WIR_RegisterParameter( x1, WIR_Usage::use ),
    new WIR_LabelParameter( b ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ) );
  ufAssert( sh2.getSize() == 4 );
  ufAssert(
    !sh2.isMemoryAccess() && sh2.isMemoryStore() && !sh2.isMemoryLoad() &&
    !sh2.isMove() && !sh2.isCall() && !sh2.isIndirectCall() &&
    !sh2.isReturn() && !sh2.isJump() && !sh2.isConditionalJump() &&
    !sh2.isUnconditionalJump() && !sh2.isIndirectJump() &&
    !sh2.isAsmDataDirective() && !sh2.hasSideEffects() );

  WIR_Operation sll1(
    RV32I::OpCode::SLL, RV32I::OperationFormat::RRR_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ),
    new WIR_RegisterParameter( x3, WIR_Usage::use ) );
  ufAssert( sll1.getSize() == 4 );
  ufAssert(
    !sll1.isMemoryAccess() && !sll1.isMemoryStore() && !sll1.isMemoryLoad() &&
    !sll1.isMove() && !sll1.isCall() && !sll1.isIndirectCall() &&
    !sll1.isReturn() && !sll1.isJump() && !sll1.isConditionalJump() &&
    !sll1.isUnconditionalJump() && !sll1.isIndirectJump() &&
    !sll1.isAsmDataDirective() && !sll1.hasSideEffects() );

  WIR_Operation slli1(
    RV32I::OpCode::SLLI, RV32I::OperationFormat::RRC5_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ),
    new RV_Const5_Unsigned( 15 ) );
  ufAssert( slli1.getSize() == 4 );
  ufAssert(
    !slli1.isMemoryAccess() && !slli1.isMemoryStore() &&
    !slli1.isMemoryLoad() && !slli1.isMove() && !slli1.isCall() &&
    !slli1.isIndirectCall() && !slli1.isReturn() && !slli1.isJump() &&
    !slli1.isConditionalJump() && !slli1.isUnconditionalJump() &&
    !slli1.isIndirectJump() && !slli1.isAsmDataDirective() &&
    !slli1.hasSideEffects() );

  WIR_Operation slt1(
    RV32I::OpCode::SLT, RV32I::OperationFormat::RRR_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ),
    new WIR_RegisterParameter( x3, WIR_Usage::use ) );
  ufAssert( slt1.getSize() == 4 );
  ufAssert(
    !slt1.isMemoryAccess() && !slt1.isMemoryStore() && !slt1.isMemoryLoad() &&
    !slt1.isMove() && !slt1.isCall() && !slt1.isIndirectCall() &&
    !slt1.isReturn() && !slt1.isJump() && !slt1.isConditionalJump() &&
    !slt1.isUnconditionalJump() && !slt1.isIndirectJump() &&
    !slt1.isAsmDataDirective() && !slt1.hasSideEffects() );

  WIR_Operation slti1(
    RV32I::OpCode::SLTI, RV32I::OperationFormat::RRC12_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ),
    new RV_Const12_Signed( -1023 ) );
  ufAssert( slti1.getSize() == 4 );
  ufAssert(
    !slti1.isMemoryAccess() && !slti1.isMemoryStore() &&
    !slti1.isMemoryLoad() && !slti1.isMove() && !slti1.isCall() &&
    !slti1.isIndirectCall() && !slti1.isReturn() && !slti1.isJump() &&
    !slti1.isConditionalJump() && !slti1.isUnconditionalJump() &&
    !slti1.isIndirectJump() && !slti1.isAsmDataDirective() &&
    !slti1.hasSideEffects() );

  WIR_Operation slti2(
    RV32I::OpCode::SLTI, RV32I::OperationFormat::RRL_2,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ),
    new WIR_LabelParameter( b ) );
  ufAssert( slti2.getSize() == 4 );
  ufAssert(
    !slti2.isMemoryAccess() && !slti2.isMemoryStore() &&
    !slti2.isMemoryLoad() && !slti2.isMove() && !slti2.isCall() &&
    !slti2.isIndirectCall() && !slti2.isReturn() && !slti2.isJump() &&
    !slti2.isConditionalJump() && !slti2.isUnconditionalJump() &&
    !slti2.isIndirectJump() && !slti2.isAsmDataDirective() &&
    !slti2.hasSideEffects() );

  WIR_Operation sltiu1(
    RV32I::OpCode::SLTIU, RV32I::OperationFormat::RRC12_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ),
    new RV_Const12_Signed( -1023 ) );
  ufAssert( sltiu1.getSize() == 4 );
  ufAssert(
    !sltiu1.isMemoryAccess() && !sltiu1.isMemoryStore() &&
    !sltiu1.isMemoryLoad() && !sltiu1.isMove() && !sltiu1.isCall() &&
    !sltiu1.isIndirectCall() && !sltiu1.isReturn() && !sltiu1.isJump() &&
    !sltiu1.isConditionalJump() && !sltiu1.isUnconditionalJump() &&
    !sltiu1.isIndirectJump() && !sltiu1.isAsmDataDirective() &&
    !sltiu1.hasSideEffects() );

  WIR_Operation sltiu2(
    RV32I::OpCode::SLTIU, RV32I::OperationFormat::RRL_2,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ),
    new WIR_LabelParameter( b ) );
  ufAssert( sltiu2.getSize() == 4 );
  ufAssert(
    !sltiu2.isMemoryAccess() && !sltiu2.isMemoryStore() &&
    !sltiu2.isMemoryLoad() && !sltiu2.isMove() && !sltiu2.isCall() &&
    !sltiu2.isIndirectCall() && !sltiu2.isReturn() && !sltiu2.isJump() &&
    !sltiu2.isConditionalJump() && !sltiu2.isUnconditionalJump() &&
    !sltiu2.isIndirectJump() && !sltiu2.isAsmDataDirective() &&
    !sltiu2.hasSideEffects() );

  WIR_Operation sltu1(
    RV32I::OpCode::SLTU, RV32I::OperationFormat::RRR_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ),
    new WIR_RegisterParameter( x3, WIR_Usage::use ) );
  ufAssert( sltu1.getSize() == 4 );
  ufAssert(
    !sltu1.isMemoryAccess() && !sltu1.isMemoryStore() &&
    !sltu1.isMemoryLoad() && !sltu1.isMove() && !sltu1.isCall() &&
    !sltu1.isIndirectCall() && !sltu1.isReturn() && !sltu1.isJump() &&
    !sltu1.isConditionalJump() && !sltu1.isUnconditionalJump() &&
    !sltu1.isIndirectJump() && !sltu1.isAsmDataDirective() &&
    !sltu1.hasSideEffects() );

  WIR_Operation sra1(
    RV32I::OpCode::SRA, RV32I::OperationFormat::RRR_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ),
    new WIR_RegisterParameter( x3, WIR_Usage::use ) );
  ufAssert( sra1.getSize() == 4 );
  ufAssert(
    !sra1.isMemoryAccess() && !sra1.isMemoryStore() && !sra1.isMemoryLoad() &&
    !sra1.isMove() && !sra1.isCall() && !sra1.isIndirectCall() &&
    !sra1.isReturn() && !sra1.isJump() && !sra1.isConditionalJump() &&
    !sra1.isUnconditionalJump() && !sra1.isIndirectJump() &&
    !sra1.isAsmDataDirective() && !sra1.hasSideEffects() );

  WIR_Operation srai1(
    RV32I::OpCode::SRAI, RV32I::OperationFormat::RRC5_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ),
    new RV_Const5_Unsigned( 15 ) );
  ufAssert( srai1.getSize() == 4 );
  ufAssert(
    !srai1.isMemoryAccess() && !srai1.isMemoryStore() &&
    !srai1.isMemoryLoad() && !srai1.isMove() && !srai1.isCall() &&
    !srai1.isIndirectCall() && !srai1.isReturn() && !srai1.isJump() &&
    !srai1.isConditionalJump() && !srai1.isUnconditionalJump() &&
    !srai1.isIndirectJump() && !srai1.isAsmDataDirective() &&
    !srai1.hasSideEffects() );

  WIR_Operation srl1(
    RV32I::OpCode::SRL, RV32I::OperationFormat::RRR_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ),
    new WIR_RegisterParameter( x3, WIR_Usage::use ) );
  ufAssert( srl1.getSize() == 4 );
  ufAssert(
    !srl1.isMemoryAccess() && !srl1.isMemoryStore() && !srl1.isMemoryLoad() &&
    !srl1.isMove() && !srl1.isCall() && !srl1.isIndirectCall() &&
    !srl1.isReturn() && !srl1.isJump() && !srl1.isConditionalJump() &&
    !srl1.isUnconditionalJump() && !srl1.isIndirectJump() &&
    !srl1.isAsmDataDirective() && !srl1.hasSideEffects() );

  WIR_Operation srli1(
    RV32I::OpCode::SRLI, RV32I::OperationFormat::RRC5_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ),
    new RV_Const5_Unsigned( 15 ) );
  ufAssert( srli1.getSize() == 4 );
  ufAssert(
    !srli1.isMemoryAccess() && !srli1.isMemoryStore() &&
    !srli1.isMemoryLoad() && !srli1.isMove() && !srli1.isCall() &&
    !srli1.isIndirectCall() && !srli1.isReturn() && !srli1.isJump() &&
    !srli1.isConditionalJump() && !srli1.isUnconditionalJump() &&
    !srli1.isIndirectJump() && !srli1.isAsmDataDirective() &&
    !srli1.hasSideEffects() );

  WIR_Operation sub1(
    RV32I::OpCode::SUB, RV32I::OperationFormat::RRR_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ),
    new WIR_RegisterParameter( x3, WIR_Usage::use ) );
  ufAssert( sub1.getSize() == 4 );
  ufAssert(
    !sub1.isMemoryAccess() && !sub1.isMemoryStore() && !sub1.isMemoryLoad() &&
    !sub1.isMove() && !sub1.isCall() && !sub1.isIndirectCall() &&
    !sub1.isReturn() && !sub1.isJump() && !sub1.isConditionalJump() &&
    !sub1.isUnconditionalJump() && !sub1.isIndirectJump() &&
    !sub1.isAsmDataDirective() && !sub1.hasSideEffects() );

  WIR_Operation sw1(
    RV32I::OpCode::SW, RV32I::OperationFormat::RC12R_2,
    new WIR_RegisterParameter( x1, WIR_Usage::use ),
    new RV_Const12_Signed( -1023 ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ) );
  ufAssert( sw1.getSize() == 4 );
  ufAssert(
    !sw1.isMemoryAccess() && sw1.isMemoryStore() && !sw1.isMemoryLoad() &&
    !sw1.isMove() && !sw1.isCall() && !sw1.isIndirectCall() &&
    !sw1.isReturn() && !sw1.isJump() && !sw1.isConditionalJump() &&
    !sw1.isUnconditionalJump() && !sw1.isIndirectJump() &&
    !sw1.isAsmDataDirective() && !sw1.hasSideEffects() );

  WIR_Operation sw2(
    RV32I::OpCode::SW, RV32I::OperationFormat::RLR_2,
    new WIR_RegisterParameter( x1, WIR_Usage::use ),
    new WIR_LabelParameter( b ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ) );
  ufAssert( sw2.getSize() == 4 );
  ufAssert(
    !sw2.isMemoryAccess() && sw2.isMemoryStore() && !sw2.isMemoryLoad() &&
    !sw2.isMove() && !sw2.isCall() && !sw2.isIndirectCall() &&
    !sw2.isReturn() && !sw2.isJump() && !sw2.isConditionalJump() &&
    !sw2.isUnconditionalJump() && !sw2.isIndirectJump() &&
    !sw2.isAsmDataDirective() && !sw2.hasSideEffects() );

  WIR_Operation xor1(
    RV32I::OpCode::XOR, RV32I::OperationFormat::RRR_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ),
    new WIR_RegisterParameter( x3, WIR_Usage::use ) );
  ufAssert( xor1.getSize() == 4 );
  ufAssert(
    !xor1.isMemoryAccess() && !xor1.isMemoryStore() && !xor1.isMemoryLoad() &&
    !xor1.isMove() && !xor1.isCall() && !xor1.isIndirectCall() &&
    !xor1.isReturn() && !xor1.isJump() && !xor1.isConditionalJump() &&
    !xor1.isUnconditionalJump() && !xor1.isIndirectJump() &&
    !xor1.isAsmDataDirective() && !xor1.hasSideEffects() );

  WIR_Operation xori1(
    RV32I::OpCode::XORI, RV32I::OperationFormat::RRC12_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ),
    new RV_Const12_Signed( -1023 ) );
  ufAssert( xori1.getSize() == 4 );
  ufAssert(
    !xori1.isMemoryAccess() && !xori1.isMemoryStore() &&
    !xori1.isMemoryLoad() && !xori1.isMove() && !xori1.isCall() &&
    !xori1.isIndirectCall() && !xori1.isReturn() && !xori1.isJump() &&
    !xori1.isConditionalJump() && !xori1.isUnconditionalJump() &&
    !xori1.isIndirectJump() && !xori1.isAsmDataDirective() &&
    !xori1.hasSideEffects() );

  WIR_Operation xori2(
    RV32I::OpCode::XORI, RV32I::OperationFormat::RRL_2,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ),
    new WIR_LabelParameter( b ) );
  ufAssert( xori2.getSize() == 4 );
  ufAssert(
    !xori2.isMemoryAccess() && !xori2.isMemoryStore() &&
    !xori2.isMemoryLoad() && !xori2.isMove() && !xori2.isCall() &&
    !xori2.isIndirectCall() && !xori2.isReturn() && !xori2.isJump() &&
    !xori2.isConditionalJump() && !xori2.isUnconditionalJump() &&
    !xori2.isIndirectJump() && !xori2.isAsmDataDirective() &&
    !xori2.hasSideEffects() );

 return( 0 );
}
