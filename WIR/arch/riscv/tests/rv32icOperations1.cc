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
#include <arch/riscv/rv32ic.h>


using namespace std;
using namespace WIR;


int main( void )
{
  WIR_Init();

  RV32IC riscv;
  RV_RegV x1, x2, x3;
  WIR_BasicBlock b;
  WIR_Function f( "main" );

  // The following operations must be accepted according to the RISC-V ISA.
  WIR_Operation cadd1(
    RV32IC::OpCode::CADD, RV32IC::OperationFormat::SRR_2,
    new WIR_RegisterParameter( x1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ) );
  ufAssert( cadd1.getSize() == 2 );
  ufAssert(
    !cadd1.isMemoryAccess() && !cadd1.isMemoryStore() &&
    !cadd1.isMemoryLoad() && !cadd1.isMove() && !cadd1.isCall() &&
    !cadd1.isIndirectCall() && !cadd1.isReturn() && !cadd1.isJump() &&
    !cadd1.isConditionalJump() && !cadd1.isUnconditionalJump() &&
    !cadd1.isIndirectJump() && !cadd1.isAsmDataDirective() &&
    !cadd1.hasSideEffects() );

  WIR_Operation caddi1(
    RV32IC::OpCode::CADDI, RV32IC::OperationFormat::SRC6_3,
    new WIR_RegisterParameter( x1, WIR_Usage::defuse ),
    new RV_Const6_Signed( -10 ) );
  ufAssert( caddi1.getSize() == 2 );
  ufAssert(
    !caddi1.isMemoryAccess() && !caddi1.isMemoryStore() &&
    !caddi1.isMemoryLoad() && !caddi1.isMove() && !caddi1.isCall() &&
    !caddi1.isIndirectCall() && !caddi1.isReturn() && !caddi1.isJump() &&
    !caddi1.isConditionalJump() && !caddi1.isUnconditionalJump() &&
    !caddi1.isIndirectJump() && !caddi1.isAsmDataDirective() &&
    !caddi1.hasSideEffects() );

  WIR_Operation caddi16sp1(
    RV32IC::OpCode::CADDI16SP, RV32IC::OperationFormat::SRC6_4,
    new WIR_RegisterParameter( riscv.x2(), WIR_Usage::defuse ),
    new RV_Const6_Signed( -10 ) );
  ufAssert( caddi16sp1.getSize() == 2 );
  ufAssert(
    !caddi16sp1.isMemoryAccess() && !caddi16sp1.isMemoryStore() &&
    !caddi16sp1.isMemoryLoad() && !caddi16sp1.isMove() && !caddi16sp1.isCall() &&
    !caddi16sp1.isIndirectCall() && !caddi16sp1.isReturn() && !caddi16sp1.isJump()
    && !caddi16sp1.isConditionalJump() && !caddi16sp1.isUnconditionalJump() &&
    !caddi16sp1.isIndirectJump() && !caddi16sp1.isAsmDataDirective() &&
    !caddi16sp1.hasSideEffects() );

  WIR_Operation caddi4spn1(
    RV32IC::OpCode::CADDI4SPN, RV32IC::OperationFormat::SRRC8_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_RegisterParameter( riscv.x2(), WIR_Usage::use ),
    new RV_Const8_Unsigned( 10 ) );
  ufAssert( caddi4spn1.getSize() == 2 );
  ufAssert(
    !caddi4spn1.isMemoryAccess() && !caddi4spn1.isMemoryStore() &&
    !caddi4spn1.isMemoryLoad() && !caddi4spn1.isMove() && !caddi4spn1.isCall() &&
    !caddi4spn1.isIndirectCall() && !caddi4spn1.isReturn() && !caddi4spn1.isJump()
    && !caddi4spn1.isConditionalJump() && !caddi4spn1.isUnconditionalJump() &&
    !caddi4spn1.isIndirectJump() && !caddi4spn1.isAsmDataDirective() &&
    !caddi4spn1.hasSideEffects() );

  WIR_Operation cand1(
    RV32IC::OpCode::CAND, RV32IC::OperationFormat::SRR_2,
    new WIR_RegisterParameter( x1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ) );
  ufAssert( cand1.getSize() == 2 );
  ufAssert(
    !cand1.isMemoryAccess() && !cand1.isMemoryStore() &&
    !cand1.isMemoryLoad() && !cand1.isMove() && !cand1.isCall() &&
    !cand1.isIndirectCall() && !cand1.isReturn() && !cand1.isJump() &&
    !cand1.isConditionalJump() && !cand1.isUnconditionalJump() &&
    !cand1.isIndirectJump() && !cand1.isAsmDataDirective() &&
    !cand1.hasSideEffects() );

  WIR_Operation candi1(
    RV32IC::OpCode::CANDI, RV32IC::OperationFormat::SRC6_3,
    new WIR_RegisterParameter( x1, WIR_Usage::defuse ),
    new RV_Const6_Signed( -10 ) );
  ufAssert( candi1.getSize() == 2 );
  ufAssert(
    !candi1.isMemoryAccess() && !candi1.isMemoryStore() &&
    !candi1.isMemoryLoad() && !candi1.isMove() && !candi1.isCall() &&
    !candi1.isIndirectCall() && !candi1.isReturn() && !candi1.isJump() &&
    !candi1.isConditionalJump() && !candi1.isUnconditionalJump() &&
    !candi1.isIndirectJump() && !candi1.isAsmDataDirective() &&
    !candi1.hasSideEffects() );

  WIR_Operation cbeqz1(
    RV32IC::OpCode::CBEQZ, RV32IC::OperationFormat::SRL_1,
    new WIR_RegisterParameter( x1, WIR_Usage::use ),
    new WIR_LabelParameter ( b ) );
  ufAssert( cbeqz1.getSize() == 2 );
  ufAssert(
    !cbeqz1.isMemoryAccess() && !cbeqz1.isMemoryStore() &&
    !cbeqz1.isMemoryLoad() && !cbeqz1.isMove() && !cbeqz1.isCall() &&
    !cbeqz1.isIndirectCall() && !cbeqz1.isReturn() && cbeqz1.isJump() &&
    cbeqz1.isConditionalJump() && !cbeqz1.isUnconditionalJump() &&
    !cbeqz1.isIndirectJump() && !cbeqz1.isAsmDataDirective() &&
    !cbeqz1.hasSideEffects() );

  WIR_Operation cbnez1(
    RV32IC::OpCode::CBNEZ, RV32IC::OperationFormat::SRL_1,
    new WIR_RegisterParameter( x1, WIR_Usage::use ),
    new WIR_LabelParameter ( b ) );
  ufAssert( cbnez1.getSize() == 2 );
  ufAssert(
    !cbnez1.isMemoryAccess() && !cbnez1.isMemoryStore() &&
    !cbnez1.isMemoryLoad() && !cbnez1.isMove() && !cbnez1.isCall() &&
    !cbnez1.isIndirectCall() && !cbnez1.isReturn() && cbnez1.isJump() &&
    cbnez1.isConditionalJump() && !cbnez1.isUnconditionalJump() &&
    !cbnez1.isIndirectJump() && !cbnez1.isAsmDataDirective() &&
    !cbnez1.hasSideEffects() );

  WIR_Operation cebreak1(
    RV32IC::OpCode::CEBREAK, RV32IC::OperationFormat::SNULL_1 );
  ufAssert( cebreak1.getSize() == 2 );
  ufAssert(
    !cebreak1.isMemoryAccess() && !cebreak1.isMemoryStore() &&
    !cebreak1.isMemoryLoad() && !cebreak1.isMove() && !cebreak1.isCall() &&
    !cebreak1.isIndirectCall() && !cebreak1.isReturn() && !cebreak1.isJump() &&
    !cebreak1.isConditionalJump() && !cebreak1.isUnconditionalJump() &&
    !cebreak1.isIndirectJump() && !cebreak1.isAsmDataDirective() &&
    cebreak1.hasSideEffects() );

  WIR_Operation cj1(
    RV32IC::OpCode::CJ, RV32IC::OperationFormat::SL_1,
    new WIR_LabelParameter ( b ) );
  ufAssert( cj1.getSize() == 2 );
  ufAssert(
    !cj1.isMemoryAccess() && !cj1.isMemoryStore() && !cj1.isMemoryLoad() &&
    !cj1.isMove() && !cj1.isCall() && !cj1.isIndirectCall() &&
    !cj1.isReturn() && cj1.isJump() && !cj1.isConditionalJump() &&
    cj1.isUnconditionalJump() && !cj1.isIndirectJump() &&
    !cj1.isAsmDataDirective() && !cj1.hasSideEffects() );

  WIR_Operation cjal1(
    RV32IC::OpCode::CJAL, RV32IC::OperationFormat::SL_1,
    new WIR_LabelParameter ( b ) );
  ufAssert( cjal1.getSize() == 2 );
  ufAssert(
    !cjal1.isMemoryAccess() && !cjal1.isMemoryStore() &&
    !cjal1.isMemoryLoad() && !cjal1.isMove() && cjal1.isCall() &&
    !cjal1.isIndirectCall() && !cjal1.isReturn() && !cjal1.isJump() &&
    !cjal1.isConditionalJump() && !cjal1.isUnconditionalJump() &&
    !cjal1.isIndirectJump() && !cjal1.isAsmDataDirective() &&
    !cjal1.hasSideEffects() );

  WIR_Operation cjalr1(
    RV32IC::OpCode::CJALR, RV32IC::OperationFormat::SR_1,
    new WIR_RegisterParameter( x1, WIR_Usage::use ) );
  ufAssert( cjalr1.getSize() == 2 );
  ufAssert(
    !cjalr1.isMemoryAccess() && !cjalr1.isMemoryStore() &&
    !cjalr1.isMemoryLoad() && !cjalr1.isMove() && !cjalr1.isCall() &&
    !cjalr1.isIndirectCall() && !cjalr1.isReturn() && cjalr1.isJump() &&
    !cjalr1.isConditionalJump() && !cjalr1.isUnconditionalJump() &&
    cjalr1.isIndirectJump() && !cjalr1.isAsmDataDirective() &&
    !cjalr1.hasSideEffects() );

  WIR_Operation cjr1(
    RV32IC::OpCode::CJR, RV32IC::OperationFormat::SR_1,
    new WIR_RegisterParameter( x1, WIR_Usage::use ) );
  ufAssert( cjr1.getSize() == 2 );
  ufAssert(
    !cjr1.isMemoryAccess() && !cjr1.isMemoryStore() && !cjr1.isMemoryLoad() &&
    !cjr1.isMove() && !cjr1.isCall() && !cjr1.isIndirectCall() &&
    !cjr1.isReturn() && cjr1.isJump() && !cjr1.isConditionalJump() &&
    !cjr1.isUnconditionalJump() && cjr1.isIndirectJump() &&
    !cjr1.isAsmDataDirective() && !cjr1.hasSideEffects() );

  WIR_Operation cli1(
    RV32IC::OpCode::CLI, RV32IC::OperationFormat::SRC6_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new RV_Const6_Signed( -10 ) );
  ufAssert( cli1.getSize() == 2 );
  ufAssert(
    !cli1.isMemoryAccess() && !cli1.isMemoryStore() && !cli1.isMemoryLoad() &&
    !cli1.isMove() && !cli1.isCall() && !cli1.isIndirectCall() &&
    !cli1.isReturn() && !cli1.isJump() && !cli1.isConditionalJump() &&
    !cli1.isUnconditionalJump() && !cli1.isIndirectJump() &&
    !cli1.isAsmDataDirective() && !cli1.hasSideEffects() );

  WIR_Operation clui1(
    RV32IC::OpCode::CLUI, RV32IC::OperationFormat::SRC6_2,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new RV_Const6_Unsigned( 10 ) );
  ufAssert( clui1.getSize() == 2 );
  ufAssert(
    !clui1.isMemoryAccess() && !clui1.isMemoryStore() &&
    !clui1.isMemoryLoad() && !clui1.isMove() && !clui1.isCall() &&
    !clui1.isIndirectCall() && !clui1.isReturn() && !clui1.isJump() &&
    !clui1.isConditionalJump() && !clui1.isUnconditionalJump() &&
    !clui1.isIndirectJump() && !clui1.isAsmDataDirective() &&
    !clui1.hasSideEffects() );

  WIR_Operation clw1(
    RV32IC::OpCode::CLW, RV32IC::OperationFormat::SRC5R_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new RV_Const5_Unsigned( 10 ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ) );
  ufAssert( clw1.getSize() == 2 );
  ufAssert(
    !clw1.isMemoryAccess() && !clw1.isMemoryStore() && clw1.isMemoryLoad() &&
    !clw1.isMove() && !clw1.isCall() && !clw1.isIndirectCall() &&
    !clw1.isReturn() && !clw1.isJump() && !clw1.isConditionalJump() &&
    !clw1.isUnconditionalJump() && !clw1.isIndirectJump() &&
    !clw1.isAsmDataDirective() && !clw1.hasSideEffects() );

  WIR_Operation clwsp1(
    RV32IC::OpCode::CLWSP, RV32IC::OperationFormat::SRC6R_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new RV_Const6_Unsigned( 4 ),
    new WIR_RegisterParameter( riscv.x2(), WIR_Usage::use ) );
  ufAssert( clwsp1.getSize() == 2 );
  ufAssert(
    !clwsp1.isMemoryAccess() && !clwsp1.isMemoryStore() &&
    clwsp1.isMemoryLoad() && !clwsp1.isMove() && !clwsp1.isCall() &&
    !clwsp1.isIndirectCall() && !clwsp1.isReturn() && !clwsp1.isJump() &&
    !clwsp1.isConditionalJump() && !clwsp1.isUnconditionalJump() &&
    !clwsp1.isIndirectJump() && !clwsp1.isAsmDataDirective() &&
    !clwsp1.hasSideEffects() );

  WIR_Operation cmv1(
    RV32IC::OpCode::CMV, RV32IC::OperationFormat::SRR_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ) );
  ufAssert( cmv1.getSize() == 2 );
  ufAssert(
    !cmv1.isMemoryAccess() && !cmv1.isMemoryStore() && !cmv1.isMemoryLoad() &&
    cmv1.isMove() && !cmv1.isCall() && !cmv1.isIndirectCall() &&
    !cmv1.isReturn() && !cmv1.isJump() && !cmv1.isConditionalJump() &&
    !cmv1.isUnconditionalJump() && !cmv1.isIndirectJump() &&
    !cmv1.isAsmDataDirective() && !cmv1.hasSideEffects() );

  WIR_Operation cnop1(
    RV32IC::OpCode::CNOP, RV32IC::OperationFormat::SNULL_1 );
  ufAssert( cnop1.getSize() == 2 );
  ufAssert(
    !cnop1.isMemoryAccess() && !cnop1.isMemoryStore() &&
    !cnop1.isMemoryLoad() && !cnop1.isMove() && !cnop1.isCall() &&
    !cnop1.isIndirectCall() && !cnop1.isReturn() && !cnop1.isJump() &&
    !cnop1.isConditionalJump() && !cnop1.isUnconditionalJump() &&
    !cnop1.isIndirectJump() && !cnop1.isAsmDataDirective() &&
    cnop1.hasSideEffects() );

  WIR_Operation cor1(
    RV32IC::OpCode::COR, RV32IC::OperationFormat::SRR_2,
    new WIR_RegisterParameter( x1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ) );
  ufAssert( cor1.getSize() == 2 );
  ufAssert(
    !cor1.isMemoryAccess() && !cor1.isMemoryStore() && !cor1.isMemoryLoad() &&
    !cor1.isMove() && !cor1.isCall() && !cor1.isIndirectCall() &&
    !cor1.isReturn() && !cor1.isJump() && !cor1.isConditionalJump() &&
    !cor1.isUnconditionalJump() && !cor1.isIndirectJump() &&
    !cor1.isAsmDataDirective() && !cor1.hasSideEffects() );

  WIR_Operation cslli1(
    RV32IC::OpCode::CSLLI, RV32IC::OperationFormat::SRC5_1,
    new WIR_RegisterParameter( x1, WIR_Usage::defuse ),
    new RV_Const5_Unsigned( 10 ) );
  ufAssert( cslli1.getSize() == 2 );
  ufAssert(
    !cslli1.isMemoryAccess() && !cslli1.isMemoryStore() &&
    !cslli1.isMemoryLoad() && !cslli1.isMove() && !cslli1.isCall() &&
    !cslli1.isIndirectCall() && !cslli1.isReturn() && !cslli1.isJump() &&
    !cslli1.isConditionalJump() && !cslli1.isUnconditionalJump() &&
    !cslli1.isIndirectJump() && !cslli1.isAsmDataDirective() &&
    !cslli1.hasSideEffects() );

  WIR_Operation csrai1(
    RV32IC::OpCode::CSRAI, RV32IC::OperationFormat::SRC5_1,
    new WIR_RegisterParameter( x1, WIR_Usage::defuse ),
    new RV_Const5_Unsigned( 10 ) );
  ufAssert( csrai1.getSize() == 2 );
  ufAssert(
    !csrai1.isMemoryAccess() && !csrai1.isMemoryStore() &&
    !csrai1.isMemoryLoad() && !csrai1.isMove() && !csrai1.isCall() &&
    !csrai1.isIndirectCall() && !csrai1.isReturn() && !csrai1.isJump() &&
    !csrai1.isConditionalJump() && !csrai1.isUnconditionalJump() &&
    !csrai1.isIndirectJump() && !csrai1.isAsmDataDirective() &&
    !csrai1.hasSideEffects() );

  WIR_Operation csrli1(
    RV32IC::OpCode::CSRLI, RV32IC::OperationFormat::SRC5_1,
    new WIR_RegisterParameter( x1, WIR_Usage::defuse ),
    new RV_Const5_Unsigned( 10 ) );
  ufAssert( csrli1.getSize() == 2 );
  ufAssert(
    !csrli1.isMemoryAccess() && !csrli1.isMemoryStore() &&
    !csrli1.isMemoryLoad() && !csrli1.isMove() && !csrli1.isCall() &&
    !csrli1.isIndirectCall() && !csrli1.isReturn() && !csrli1.isJump() &&
    !csrli1.isConditionalJump() && !csrli1.isUnconditionalJump() &&
    !csrli1.isIndirectJump() && !csrli1.isAsmDataDirective() &&
    !csrli1.hasSideEffects() );

  WIR_Operation csub1(
    RV32IC::OpCode::CSUB, RV32IC::OperationFormat::SRR_2,
    new WIR_RegisterParameter( x1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ) );
  ufAssert( csub1.getSize() == 2 );
  ufAssert(
    !csub1.isMemoryAccess() && !csub1.isMemoryStore() &&
    !csub1.isMemoryLoad() && !csub1.isMove() && !csub1.isCall() &&
    !csub1.isIndirectCall() && !csub1.isReturn() && !csub1.isJump() &&
    !csub1.isConditionalJump() && !csub1.isUnconditionalJump() &&
    !csub1.isIndirectJump() && !csub1.isAsmDataDirective() &&
    !csub1.hasSideEffects() );

  WIR_Operation csw1(
    RV32IC::OpCode::CSW, RV32IC::OperationFormat::SRC5R_2,
    new WIR_RegisterParameter( x1, WIR_Usage::use ),
    new RV_Const5_Unsigned( 10 ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ) );
  ufAssert( csw1.getSize() == 2 );
  ufAssert(
    !csw1.isMemoryAccess() && csw1.isMemoryStore() && !csw1.isMemoryLoad() &&
    !csw1.isMove() && !csw1.isCall() && !csw1.isIndirectCall() &&
    !csw1.isReturn() && !csw1.isJump() && !csw1.isConditionalJump() &&
    !csw1.isUnconditionalJump() && !csw1.isIndirectJump() &&
    !csw1.isAsmDataDirective() && !csw1.hasSideEffects() );

  WIR_Operation cswsp1(
    RV32IC::OpCode::CSWSP, RV32IC::OperationFormat::SRC6R_2,
    new WIR_RegisterParameter( x1, WIR_Usage::use ),
    new RV_Const6_Unsigned( 4 ),
    new WIR_RegisterParameter( riscv.x2(), WIR_Usage::use ) );
  ufAssert( cswsp1.getSize() == 2 );
  ufAssert(
    !cswsp1.isMemoryAccess() && cswsp1.isMemoryStore() &&
    !cswsp1.isMemoryLoad() && !cswsp1.isMove() && !cswsp1.isCall() &&
    !cswsp1.isIndirectCall() && !cswsp1.isReturn() && !cswsp1.isJump() &&
    !cswsp1.isConditionalJump() && !cswsp1.isUnconditionalJump() &&
    !cswsp1.isIndirectJump() && !cswsp1.isAsmDataDirective() &&
    !cswsp1.hasSideEffects() );

  WIR_Operation cxor1(
    RV32IC::OpCode::CXOR, RV32IC::OperationFormat::SRR_2,
    new WIR_RegisterParameter( x1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ) );
  ufAssert( cxor1.getSize() == 2 );
  ufAssert(
    !cxor1.isMemoryAccess() && !cxor1.isMemoryStore() &&
    !cxor1.isMemoryLoad() && !cxor1.isMove() && !cxor1.isCall() &&
    !cxor1.isIndirectCall() && !cxor1.isReturn() && !cxor1.isJump() &&
    !cxor1.isConditionalJump() && !cxor1.isUnconditionalJump() &&
    !cxor1.isIndirectJump() && !cxor1.isAsmDataDirective() &&
    !cxor1.hasSideEffects() );

 return( 0 );
}
