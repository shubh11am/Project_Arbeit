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

  MIPS m;
  MIPS_RegV r1, r2, r3;
  WIR_BasicBlock b;
  WIR_Function f( "main" );


  // The following operations must be accepted according to the MIPS ISA.
  WIR_Operation add1(
    MIPS::OpCode::ADD, MIPS::OperationFormat::RRR,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( r3, WIR_Usage::use ) );
  ufAssert( add1.getSize() == 4 );
  ufAssert(
    !add1.isMemoryAccess() && !add1.isMemoryStore() && !add1.isMemoryLoad() &&
    !add1.isMove() && !add1.isCall() && !add1.isIndirectCall() &&
    !add1.isReturn() && !add1.isJump() && !add1.isConditionalJump() &&
    !add1.isUnconditionalJump() && !add1.isIndirectJump() );

  WIR_Operation addi1(
    MIPS::OpCode::ADDI, MIPS::OperationFormat::RRI,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    MIPS_Immediate16_Signed( -138 ) );
  ufAssert( addi1.getSize() == 4 );
  ufAssert(
    !addi1.isMemoryAccess() && !addi1.isMemoryStore() &&
    !addi1.isMemoryLoad() && !addi1.isMove() && !addi1.isCall() &&
    !addi1.isIndirectCall() && !addi1.isReturn() && !addi1.isJump() &&
    !addi1.isConditionalJump() && !addi1.isUnconditionalJump() &&
    !addi1.isIndirectJump() );

  WIR_Operation addiu1(
    MIPS::OpCode::ADDIU, MIPS::OperationFormat::RRIU,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    MIPS_Immediate16_Unsigned( 138 ) );
  ufAssert( addiu1.getSize() == 4 );
  ufAssert(
    !addiu1.isMemoryAccess() && !addiu1.isMemoryStore() &&
    !addiu1.isMemoryLoad() && !addiu1.isMove() && !addiu1.isCall() &&
    !addiu1.isIndirectCall() && !addiu1.isReturn() && !addiu1.isJump() &&
    !addiu1.isConditionalJump() && !addiu1.isUnconditionalJump() &&
    !addiu1.isIndirectJump() );

  WIR_Operation addu1(
    MIPS::OpCode::ADDU, MIPS::OperationFormat::RRR,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( r3, WIR_Usage::use ) );
  ufAssert( addu1.getSize() == 4 );
  ufAssert(
    !addu1.isMemoryAccess() && !addu1.isMemoryStore() &&
    !addu1.isMemoryLoad() && !addu1.isMove() && !addu1.isCall() &&
    !addu1.isIndirectCall() && !addu1.isReturn() && !addu1.isJump() &&
    !addu1.isConditionalJump() && !addu1.isUnconditionalJump() &&
    !addu1.isIndirectJump() );

  WIR_Operation and1(
    MIPS::OpCode::AND, MIPS::OperationFormat::RRR,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( r3, WIR_Usage::use ) );
  ufAssert( and1.getSize() == 4 );
  ufAssert(
    !and1.isMemoryAccess() && !and1.isMemoryStore() && !and1.isMemoryLoad() &&
    !and1.isMove() && !and1.isCall() && !and1.isIndirectCall() &&
    !and1.isReturn() && !and1.isJump() && !and1.isConditionalJump() &&
    !and1.isUnconditionalJump() && !and1.isIndirectJump() );

  WIR_Operation andi1(
    MIPS::OpCode::ANDI, MIPS::OperationFormat::RRIU,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    MIPS_Immediate16_Unsigned( 138 ) );
  ufAssert( andi1.getSize() == 4 );
  ufAssert(
    !andi1.isMemoryAccess() && !andi1.isMemoryStore() &&
    !andi1.isMemoryLoad() && !andi1.isMove() && !andi1.isCall() &&
    !andi1.isIndirectCall() && !andi1.isReturn() && !andi1.isJump() &&
    !andi1.isConditionalJump() && !andi1.isUnconditionalJump() &&
    !andi1.isIndirectJump() );

  WIR_Operation beq1(
    MIPS::OpCode::BEQ, MIPS::OperationFormat::RRL,
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_LabelParameter( b ) );
  ufAssert( beq1.getSize() == 4 );
  ufAssert(
    !beq1.isMemoryAccess() && !beq1.isMemoryStore() && !beq1.isMemoryLoad() &&
    !beq1.isMove() && !beq1.isCall() && !beq1.isIndirectCall() &&
    !beq1.isReturn() && beq1.isJump() && beq1.isConditionalJump() &&
    !beq1.isUnconditionalJump() && !beq1.isIndirectJump() );

  WIR_Operation bne1(
    MIPS::OpCode::BNE, MIPS::OperationFormat::RRL,
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_LabelParameter( b ) );
  ufAssert( bne1.getSize() == 4 );
  ufAssert(
    !bne1.isMemoryAccess() && !bne1.isMemoryStore() && !bne1.isMemoryLoad() &&
    !bne1.isMove() && !bne1.isCall() && !bne1.isIndirectCall() &&
    !bne1.isReturn() && bne1.isJump() && bne1.isConditionalJump() &&
    !bne1.isUnconditionalJump() && !bne1.isIndirectJump() );

  WIR_Operation div1(
    MIPS::OpCode::DIV, MIPS::OperationFormat::RR_2,
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    WIR_RegisterParameter( r2, WIR_Usage::use ) );
  ufAssert( div1.getSize() == 4 );
  ufAssert(
    !div1.isMemoryAccess() && !div1.isMemoryStore() && !div1.isMemoryLoad() &&
    !div1.isMove() && !div1.isCall() && !div1.isIndirectCall() &&
    !div1.isReturn() && !div1.isJump() && !div1.isConditionalJump() &&
    !div1.isUnconditionalJump() && !div1.isIndirectJump() );

  WIR_Operation divu1(
    MIPS::OpCode::DIVU, MIPS::OperationFormat::RR_2,
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    WIR_RegisterParameter( r2, WIR_Usage::use ) );
  ufAssert( divu1.getSize() == 4 );
  ufAssert(
    !divu1.isMemoryAccess() && !divu1.isMemoryStore() && !divu1.isMemoryLoad() &&
    !divu1.isMove() && !divu1.isCall() && !divu1.isIndirectCall() &&
    !divu1.isReturn() && !divu1.isJump() && !divu1.isConditionalJump() &&
    !divu1.isUnconditionalJump() && !divu1.isIndirectJump() );

  WIR_Operation j1(
    MIPS::OpCode::J, MIPS::OperationFormat::L,
    WIR_LabelParameter( b ) );
  ufAssert( j1.getSize() == 4 );
  ufAssert(
    !j1.isMemoryAccess() && !j1.isMemoryStore() && !j1.isMemoryLoad() &&
    !j1.isMove() && !j1.isCall() && !j1.isIndirectCall() &&
    !j1.isReturn() && j1.isJump() && !j1.isConditionalJump() &&
    j1.isUnconditionalJump() && !j1.isIndirectJump() );

  WIR_Operation jal1(
    MIPS::OpCode::JAL, MIPS::OperationFormat::L,
    WIR_LabelParameter( f ) );
  ufAssert( jal1.getSize() == 4 );
  ufAssert(
    !jal1.isMemoryAccess() && !jal1.isMemoryStore() && !jal1.isMemoryLoad() &&
    !jal1.isMove() && jal1.isCall() && !jal1.isIndirectCall() &&
    !jal1.isReturn() && !jal1.isJump() && !jal1.isConditionalJump() &&
    !jal1.isUnconditionalJump() && !jal1.isIndirectJump() );

  WIR_Operation jalr1(
    MIPS::OpCode::JALR, MIPS::OperationFormat::R_2,
    WIR_RegisterParameter( r1, WIR_Usage::use ) );
  ufAssert( jalr1.getSize() == 4 );
  ufAssert(
    !jalr1.isMemoryAccess() && !jalr1.isMemoryStore() && !jalr1.isMemoryLoad() &&
    !jalr1.isMove() && jalr1.isCall() && jalr1.isIndirectCall() &&
    !jalr1.isReturn() && !jalr1.isJump() && !jalr1.isConditionalJump() &&
    !jalr1.isUnconditionalJump() && !jalr1.isIndirectJump() );

  WIR_Operation jr1(
    MIPS::OpCode::JR, MIPS::OperationFormat::R_2,
    WIR_RegisterParameter( r1, WIR_Usage::use ) );
  ufAssert( jr1.getSize() == 4 );
  ufAssert(
    !jr1.isMemoryAccess() && !jr1.isMemoryStore() && !jr1.isMemoryLoad() &&
    !jr1.isMove() && !jr1.isCall() && !jr1.isIndirectCall() &&
    jr1.isReturn() && jr1.isJump() && !jr1.isConditionalJump() &&
    jr1.isUnconditionalJump() && jr1.isIndirectJump() );

  WIR_Operation lb1(
    MIPS::OpCode::LB, MIPS::OperationFormat::RIR_1,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    MIPS_Immediate16_Signed( -138 ),
    WIR_RegisterParameter( r2, WIR_Usage::use ) );
  ufAssert( lb1.getSize() == 4 );
  ufAssert(
    !lb1.isMemoryAccess() && !lb1.isMemoryStore() && lb1.isMemoryLoad() &&
    !lb1.isMove() && !lb1.isCall() && !lb1.isIndirectCall() &&
    !lb1.isReturn() && !lb1.isJump() && !lb1.isConditionalJump() &&
    !lb1.isUnconditionalJump() && !lb1.isIndirectJump() );

  WIR_Operation lbu1(
    MIPS::OpCode::LBU, MIPS::OperationFormat::RIUR,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    MIPS_Immediate16_Unsigned( 138 ),
    WIR_RegisterParameter( r2, WIR_Usage::use ) );
  ufAssert( lbu1.getSize() == 4 );
  ufAssert(
    !lbu1.isMemoryAccess() && !lbu1.isMemoryStore() && lbu1.isMemoryLoad() &&
    !lbu1.isMove() && !lbu1.isCall() && !lbu1.isIndirectCall() &&
    !lbu1.isReturn() && !lbu1.isJump() && !lbu1.isConditionalJump() &&
    !lbu1.isUnconditionalJump() && !lbu1.isIndirectJump() );

  WIR_Operation lh1(
    MIPS::OpCode::LH, MIPS::OperationFormat::RIR_1,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    MIPS_Immediate16_Signed( -138 ),
    WIR_RegisterParameter( r2, WIR_Usage::use ) );
  ufAssert( lh1.getSize() == 4 );
  ufAssert(
    !lh1.isMemoryAccess() && !lh1.isMemoryStore() && lh1.isMemoryLoad() &&
    !lh1.isMove() && !lh1.isCall() && !lh1.isIndirectCall() &&
    !lh1.isReturn() && !lh1.isJump() && !lh1.isConditionalJump() &&
    !lh1.isUnconditionalJump() && !lh1.isIndirectJump() );

  WIR_Operation lhu1(
    MIPS::OpCode::LHU, MIPS::OperationFormat::RIUR,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    MIPS_Immediate16_Unsigned( 138 ),
    WIR_RegisterParameter( r2, WIR_Usage::use ) );
  ufAssert( lhu1.getSize() == 4 );
  ufAssert(
    !lhu1.isMemoryAccess() && !lhu1.isMemoryStore() && lhu1.isMemoryLoad() &&
    !lhu1.isMove() && !lhu1.isCall() && !lhu1.isIndirectCall() &&
    !lhu1.isReturn() && !lhu1.isJump() && !lhu1.isConditionalJump() &&
    !lhu1.isUnconditionalJump() && !lhu1.isIndirectJump() );

  WIR_Operation lui1(
    MIPS::OpCode::LUI, MIPS::OperationFormat::RIU,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    MIPS_Immediate16_Unsigned( 138 ) );
  ufAssert( lui1.getSize() == 4 );
  ufAssert(
    !lui1.isMemoryAccess() && !lui1.isMemoryStore() && !lui1.isMemoryLoad() &&
    !lui1.isMove() && !lui1.isCall() && !lui1.isIndirectCall() &&
    !lui1.isReturn() && !lui1.isJump() && !lui1.isConditionalJump() &&
    !lui1.isUnconditionalJump() && !lui1.isIndirectJump() );

  WIR_Operation lw1(
    MIPS::OpCode::LW, MIPS::OperationFormat::RIR_1,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    MIPS_Immediate16_Signed( -138 ),
    WIR_RegisterParameter( r2, WIR_Usage::use ) );
  ufAssert( lw1.getSize() == 4 );
  ufAssert(
    !lw1.isMemoryAccess() && !lw1.isMemoryStore() && lw1.isMemoryLoad() &&
    !lw1.isMove() && !lw1.isCall() && !lw1.isIndirectCall() &&
    !lw1.isReturn() && !lw1.isJump() && !lw1.isConditionalJump() &&
    !lw1.isUnconditionalJump() && !lw1.isIndirectJump() );

  WIR_Operation mfco1(
    MIPS::OpCode::MFCO, MIPS::OperationFormat::RR_1,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::use ) );
  ufAssert( mfco1.getSize() == 4 );
  ufAssert(
    !mfco1.isMemoryAccess() && !mfco1.isMemoryStore() &&
    !mfco1.isMemoryLoad() && !mfco1.isMove() && !mfco1.isCall() &&
    !mfco1.isIndirectCall() && !mfco1.isReturn() && !mfco1.isJump() &&
    !mfco1.isConditionalJump() && !mfco1.isUnconditionalJump() &&
    !mfco1.isIndirectJump() );

  WIR_Operation mfhi1(
    MIPS::OpCode::MFHI, MIPS::OperationFormat::R_1,
    WIR_RegisterParameter( r1, WIR_Usage::def ) );
  ufAssert( mfhi1.getSize() == 4 );
  ufAssert(
    !mfhi1.isMemoryAccess() && !mfhi1.isMemoryStore() &&
    !mfhi1.isMemoryLoad() && !mfhi1.isMove() && !mfhi1.isCall() &&
    !mfhi1.isIndirectCall() && !mfhi1.isReturn() && !mfhi1.isJump() &&
    !mfhi1.isConditionalJump() && !mfhi1.isUnconditionalJump() &&
    !mfhi1.isIndirectJump() );

  WIR_Operation mflo1(
    MIPS::OpCode::MFLO, MIPS::OperationFormat::R_1,
    WIR_RegisterParameter( r1, WIR_Usage::def ) );
  ufAssert( mflo1.getSize() == 4 );
  ufAssert(
    !mflo1.isMemoryAccess() && !mflo1.isMemoryStore() &&
    !mflo1.isMemoryLoad() && !mflo1.isMove() && !mflo1.isCall() &&
    !mflo1.isIndirectCall() && !mflo1.isReturn() && !mflo1.isJump() &&
    !mflo1.isConditionalJump() && !mflo1.isUnconditionalJump() &&
    !mflo1.isIndirectJump() );

  WIR_Operation mult1(
    MIPS::OpCode::MULT, MIPS::OperationFormat::RR_2,
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    WIR_RegisterParameter( r2, WIR_Usage::use ) );
  ufAssert( mult1.getSize() == 4 );
  ufAssert(
    !mult1.isMemoryAccess() && !mult1.isMemoryStore() &&
    !mult1.isMemoryLoad() && !mult1.isMove() && !mult1.isCall() &&
    !mult1.isIndirectCall() && !mult1.isReturn() && !mult1.isJump() &&
    !mult1.isConditionalJump() && !mult1.isUnconditionalJump() &&
    !mult1.isIndirectJump() );

  WIR_Operation multu1(
    MIPS::OpCode::MULTU, MIPS::OperationFormat::RR_2,
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    WIR_RegisterParameter( r2, WIR_Usage::use ) );
  ufAssert( multu1.getSize() == 4 );
  ufAssert(
    !multu1.isMemoryAccess() && !multu1.isMemoryStore() &&
    !multu1.isMemoryLoad() && !multu1.isMove() && !multu1.isCall() &&
    !multu1.isIndirectCall() && !multu1.isReturn() && !multu1.isJump() &&
    !multu1.isConditionalJump() && !multu1.isUnconditionalJump() &&
    !multu1.isIndirectJump() );

  WIR_Operation nor1(
    MIPS::OpCode::NOR, MIPS::OperationFormat::RRR,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( r3, WIR_Usage::use ) );
  ufAssert( nor1.getSize() == 4 );
  ufAssert(
    !nor1.isMemoryAccess() && !nor1.isMemoryStore() && !nor1.isMemoryLoad() &&
    !nor1.isMove() && !nor1.isCall() && !nor1.isIndirectCall() &&
    !nor1.isReturn() && !nor1.isJump() && !nor1.isConditionalJump() &&
    !nor1.isUnconditionalJump() && !nor1.isIndirectJump() );

  WIR_Operation or1(
    MIPS::OpCode::OR, MIPS::OperationFormat::RRR,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( r3, WIR_Usage::use ) );
  ufAssert( or1.getSize() == 4 );
  ufAssert(
    !or1.isMemoryAccess() && !or1.isMemoryStore() && !or1.isMemoryLoad() &&
    !or1.isMove() && !or1.isCall() && !or1.isIndirectCall() &&
    !or1.isReturn() && !or1.isJump() && !or1.isConditionalJump() &&
    !or1.isUnconditionalJump() && !or1.isIndirectJump() );

  WIR_Operation ori1(
    MIPS::OpCode::ORI, MIPS::OperationFormat::RRIU,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    MIPS_Immediate16_Unsigned( 138 ) );
  ufAssert( ori1.getSize() == 4 );
  ufAssert(
    !ori1.isMemoryAccess() && !ori1.isMemoryStore() && !ori1.isMemoryLoad() &&
    !ori1.isMove() && !ori1.isCall() && !ori1.isIndirectCall() &&
    !ori1.isReturn() && !ori1.isJump() && !ori1.isConditionalJump() &&
    !ori1.isUnconditionalJump() && !ori1.isIndirectJump() );

  WIR_Operation sb1(
    MIPS::OpCode::SB, MIPS::OperationFormat::RIR_2,
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    MIPS_Immediate16_Signed( -138 ),
    WIR_RegisterParameter( r2, WIR_Usage::use ) );
  ufAssert( sb1.getSize() == 4 );
  ufAssert(
    !sb1.isMemoryAccess() && sb1.isMemoryStore() && !sb1.isMemoryLoad() &&
    !sb1.isMove() && !sb1.isCall() && !sb1.isIndirectCall() &&
    !sb1.isReturn() && !sb1.isJump() && !sb1.isConditionalJump() &&
    !sb1.isUnconditionalJump() && !sb1.isIndirectJump() );

  WIR_Operation sh1(
    MIPS::OpCode::SH, MIPS::OperationFormat::RIR_2,
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    MIPS_Immediate16_Signed( -138 ),
    WIR_RegisterParameter( r2, WIR_Usage::use ) );
  ufAssert( sh1.getSize() == 4 );
  ufAssert(
    !sh1.isMemoryAccess() && sh1.isMemoryStore() && !sh1.isMemoryLoad() &&
    !sh1.isMove() && !sh1.isCall() && !sh1.isIndirectCall() &&
    !sh1.isReturn() && !sh1.isJump() && !sh1.isConditionalJump() &&
    !sh1.isUnconditionalJump() && !sh1.isIndirectJump() );

  WIR_Operation sll1(
    MIPS::OpCode::SLL, MIPS::OperationFormat::RRS,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    MIPS_Immediate5_Shamt( 7 ) );
  ufAssert( sll1.getSize() == 4 );
  ufAssert(
    !sll1.isMemoryAccess() && !sll1.isMemoryStore() && !sll1.isMemoryLoad() &&
    !sll1.isMove() && !sll1.isCall() && !sll1.isIndirectCall() &&
    !sll1.isReturn() && !sll1.isJump() && !sll1.isConditionalJump() &&
    !sll1.isUnconditionalJump() && !sll1.isIndirectJump() );

  WIR_Operation sllv1(
    MIPS::OpCode::SLLV, MIPS::OperationFormat::RRR,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( r3, WIR_Usage::use ) );
  ufAssert( sllv1.getSize() == 4 );
  ufAssert(
    !sllv1.isMemoryAccess() && !sllv1.isMemoryStore() &&
    !sllv1.isMemoryLoad() && !sllv1.isMove() && !sllv1.isCall() &&
    !sllv1.isIndirectCall() && !sllv1.isReturn() && !sllv1.isJump() &&
    !sllv1.isConditionalJump() && !sllv1.isUnconditionalJump() &&
    !sllv1.isIndirectJump() );

  WIR_Operation slt1(
    MIPS::OpCode::SLT, MIPS::OperationFormat::RRR,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( r3, WIR_Usage::use ) );
  ufAssert( slt1.getSize() == 4 );
  ufAssert(
    !slt1.isMemoryAccess() && !slt1.isMemoryStore() && !slt1.isMemoryLoad() &&
    !slt1.isMove() && !slt1.isCall() && !slt1.isIndirectCall() &&
    !slt1.isReturn() && !slt1.isJump() && !slt1.isConditionalJump() &&
    !slt1.isUnconditionalJump() && !slt1.isIndirectJump() );

  WIR_Operation slti1(
    MIPS::OpCode::SLTI, MIPS::OperationFormat::RRI,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    MIPS_Immediate16_Signed( -138 ) );
  ufAssert( slti1.getSize() == 4 );
  ufAssert(
    !slti1.isMemoryAccess() && !slti1.isMemoryStore() &&
    !slti1.isMemoryLoad() && !slti1.isMove() && !slti1.isCall() &&
    !slti1.isIndirectCall() && !slti1.isReturn() && !slti1.isJump() &&
    !slti1.isConditionalJump() && !slti1.isUnconditionalJump() &&
    !slti1.isIndirectJump() );

  WIR_Operation sltiu1(
    MIPS::OpCode::SLTIU, MIPS::OperationFormat::RRIU,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    MIPS_Immediate16_Unsigned( 138 ) );
  ufAssert( sltiu1.getSize() == 4 );
  ufAssert(
    !sltiu1.isMemoryAccess() && !sltiu1.isMemoryStore() &&
    !sltiu1.isMemoryLoad() && !sltiu1.isMove() && !sltiu1.isCall() &&
    !sltiu1.isIndirectCall() && !sltiu1.isReturn() && !sltiu1.isJump() &&
    !sltiu1.isConditionalJump() && !sltiu1.isUnconditionalJump() &&
    !sltiu1.isIndirectJump() );

  WIR_Operation sltu1(
    MIPS::OpCode::SLTU, MIPS::OperationFormat::RRR,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( r3, WIR_Usage::use ) );
  ufAssert( sltu1.getSize() == 4 );
  ufAssert(
    !sltu1.isMemoryAccess() && !sltu1.isMemoryStore() &&
    !sltu1.isMemoryLoad() && !sltu1.isMove() && !sltu1.isCall() &&
    !sltu1.isIndirectCall() && !sltu1.isReturn() && !sltu1.isJump() &&
    !sltu1.isConditionalJump() && !sltu1.isUnconditionalJump() &&
    !sltu1.isIndirectJump() );

  WIR_Operation sra1(
    MIPS::OpCode::SRA, MIPS::OperationFormat::RRS,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    MIPS_Immediate5_Shamt( 7 ) );
  ufAssert( sra1.getSize() == 4 );
  ufAssert(
    !sra1.isMemoryAccess() && !sra1.isMemoryStore() && !sra1.isMemoryLoad() &&
    !sra1.isMove() && !sra1.isCall() && !sra1.isIndirectCall() &&
    !sra1.isReturn() && !sra1.isJump() && !sra1.isConditionalJump() &&
    !sra1.isUnconditionalJump() && !sra1.isIndirectJump() );

  WIR_Operation srav1(
    MIPS::OpCode::SRAV, MIPS::OperationFormat::RRR,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( r3, WIR_Usage::use ) );
  ufAssert( srav1.getSize() == 4 );
  ufAssert(
    !srav1.isMemoryAccess() && !srav1.isMemoryStore() &&
    !srav1.isMemoryLoad() && !srav1.isMove() && !srav1.isCall() &&
    !srav1.isIndirectCall() && !srav1.isReturn() && !srav1.isJump() &&
    !srav1.isConditionalJump() && !srav1.isUnconditionalJump() &&
    !srav1.isIndirectJump() );

  WIR_Operation srl1(
    MIPS::OpCode::SRL, MIPS::OperationFormat::RRS,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    MIPS_Immediate5_Shamt( 7 ) );
  ufAssert( srl1.getSize() == 4 );
  ufAssert(
    !srl1.isMemoryAccess() && !srl1.isMemoryStore() && !srl1.isMemoryLoad() &&
    !srl1.isMove() && !srl1.isCall() && !srl1.isIndirectCall() &&
    !srl1.isReturn() && !srl1.isJump() && !srl1.isConditionalJump() &&
    !srl1.isUnconditionalJump() && !srl1.isIndirectJump() );

  WIR_Operation srlv1(
    MIPS::OpCode::SRLV, MIPS::OperationFormat::RRR,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( r3, WIR_Usage::use ) );
  ufAssert( srlv1.getSize() == 4 );
  ufAssert(
    !srlv1.isMemoryAccess() && !srlv1.isMemoryStore() &&
    !srlv1.isMemoryLoad() && !srlv1.isMove() && !srlv1.isCall() &&
    !srlv1.isIndirectCall() && !srlv1.isReturn() && !srlv1.isJump() &&
    !srlv1.isConditionalJump() && !srlv1.isUnconditionalJump() &&
    !srlv1.isIndirectJump() );

  WIR_Operation sub1(
    MIPS::OpCode::SUB, MIPS::OperationFormat::RRR,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( r3, WIR_Usage::use ) );
  ufAssert( sub1.getSize() == 4 );
  ufAssert(
    !sub1.isMemoryAccess() && !sub1.isMemoryStore() && !sub1.isMemoryLoad() &&
    !sub1.isMove() && !sub1.isCall() && !sub1.isIndirectCall() &&
    !sub1.isReturn() && !sub1.isJump() && !sub1.isConditionalJump() &&
    !sub1.isUnconditionalJump() && !sub1.isIndirectJump() );

  WIR_Operation subu1(
    MIPS::OpCode::SUBU, MIPS::OperationFormat::RRR,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( r3, WIR_Usage::use ) );
  ufAssert( subu1.getSize() == 4 );
  ufAssert(
    !subu1.isMemoryAccess() && !subu1.isMemoryStore() &&
    !subu1.isMemoryLoad() && !subu1.isMove() && !subu1.isCall() &&
    !subu1.isIndirectCall() && !subu1.isReturn() && !subu1.isJump() &&
    !subu1.isConditionalJump() && !subu1.isUnconditionalJump() &&
    !subu1.isIndirectJump() );

  WIR_Operation sw1(
    MIPS::OpCode::SW, MIPS::OperationFormat::RIR_2,
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    MIPS_Immediate16_Signed( -138 ),
    WIR_RegisterParameter( r2, WIR_Usage::use ) );
  ufAssert( sw1.getSize() == 4 );
  ufAssert(
    !sw1.isMemoryAccess() && sw1.isMemoryStore() && !sw1.isMemoryLoad() &&
    !sw1.isMove() && !sw1.isCall() && !sw1.isIndirectCall() &&
    !sw1.isReturn() && !sw1.isJump() && !sw1.isConditionalJump() &&
    !sw1.isUnconditionalJump() && !sw1.isIndirectJump() );

  WIR_Operation xor1(
    MIPS::OpCode::XOR, MIPS::OperationFormat::RRR,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( r3, WIR_Usage::use ) );
  ufAssert( xor1.getSize() == 4 );
  ufAssert(
    !xor1.isMemoryAccess() && !xor1.isMemoryStore() && !xor1.isMemoryLoad() &&
    !xor1.isMove() && !xor1.isCall() && !xor1.isIndirectCall() &&
    !xor1.isReturn() && !xor1.isJump() && !xor1.isConditionalJump() &&
    !xor1.isUnconditionalJump() && !xor1.isIndirectJump() );

  WIR_Operation xori1(
    MIPS::OpCode::XORI, MIPS::OperationFormat::RRIU,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    MIPS_Immediate16_Unsigned( 138 ) );
  ufAssert( xori1.getSize() == 4 );
  ufAssert(
    !xori1.isMemoryAccess() && !xori1.isMemoryStore() &&
    !xori1.isMemoryLoad() && !xori1.isMove() && !xori1.isCall() &&
    !xori1.isIndirectCall() && !xori1.isReturn() && !xori1.isJump() &&
    !xori1.isConditionalJump() && !xori1.isUnconditionalJump() &&
    !xori1.isIndirectJump() );

  return( 0 );
}
