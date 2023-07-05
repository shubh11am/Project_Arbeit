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
#include <arch/riscv/rv32im.h>


using namespace std;
using namespace WIR;


int main( void )
{
  WIR_Init();

  RV32IM riscv;
  RV_RegV x1, x2, x3;
  WIR_Function f( "main" );

  // The following operations must be accepted according to the RISC-V ISA.
  WIR_Operation div1(
    RV32IM::OpCode::DIV, RV32IM::OperationFormat::RRR_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ),
    new WIR_RegisterParameter( x3, WIR_Usage::use ) );
  ufAssert( div1.getSize() == 4 );
  ufAssert(
    !div1.isMemoryAccess() && !div1.isMemoryStore() && !div1.isMemoryLoad() &&
    !div1.isMove() && !div1.isCall() && !div1.isIndirectCall() &&
    !div1.isReturn() && !div1.isJump() && !div1.isConditionalJump() &&
    !div1.isUnconditionalJump() && !div1.isIndirectJump() &&
    !div1.isAsmDataDirective() && !div1.hasSideEffects() );

  WIR_Operation divu1(
    RV32IM::OpCode::DIVU, RV32IM::OperationFormat::RRR_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ),
    new WIR_RegisterParameter( x3, WIR_Usage::use ) );
  ufAssert( divu1.getSize() == 4 );
  ufAssert(
    !divu1.isMemoryAccess() && !divu1.isMemoryStore() &&
    !divu1.isMemoryLoad() && !divu1.isMove() && !divu1.isCall() &&
    !divu1.isIndirectCall() && !divu1.isReturn() && !divu1.isJump() &&
    !divu1.isConditionalJump() && !divu1.isUnconditionalJump() &&
    !divu1.isIndirectJump() && !divu1.isAsmDataDirective() &&
    !divu1.hasSideEffects() );

  WIR_Operation mul1(
    RV32IM::OpCode::MUL, RV32IM::OperationFormat::RRR_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ),
    new WIR_RegisterParameter( x3, WIR_Usage::use ) );
  ufAssert( mul1.getSize() == 4 );
  ufAssert(
    !mul1.isMemoryAccess() && !mul1.isMemoryStore() && !mul1.isMemoryLoad() &&
    !mul1.isMove() && !mul1.isCall() && !mul1.isIndirectCall() &&
    !mul1.isReturn() && !mul1.isJump() && !mul1.isConditionalJump() &&
    !mul1.isUnconditionalJump() && !mul1.isIndirectJump() &&
    !mul1.isAsmDataDirective() && !mul1.hasSideEffects() );

  WIR_Operation mulh1(
    RV32IM::OpCode::MULH, RV32IM::OperationFormat::RRR_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ),
    new WIR_RegisterParameter( x3, WIR_Usage::use ) );
  ufAssert( mulh1.getSize() == 4 );
  ufAssert(
    !mulh1.isMemoryAccess() && !mulh1.isMemoryStore() &&
    !mulh1.isMemoryLoad() && !mulh1.isMove() && !mulh1.isCall() &&
    !mulh1.isIndirectCall() && !mulh1.isReturn() && !mulh1.isJump() &&
    !mulh1.isConditionalJump() && !mulh1.isUnconditionalJump() &&
    !mulh1.isIndirectJump() && !mulh1.isAsmDataDirective() &&
    !mulh1.hasSideEffects() );

  WIR_Operation mulhsu1(
    RV32IM::OpCode::MULHSU, RV32IM::OperationFormat::RRR_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ),
    new WIR_RegisterParameter( x3, WIR_Usage::use ) );
  ufAssert( mulhsu1.getSize() == 4 );
  ufAssert(
    !mulhsu1.isMemoryAccess() && !mulhsu1.isMemoryStore() &&
    !mulhsu1.isMemoryLoad() && !mulhsu1.isMove() && !mulhsu1.isCall() &&
    !mulhsu1.isIndirectCall() && !mulhsu1.isReturn() && !mulhsu1.isJump() &&
    !mulhsu1.isConditionalJump() && !mulhsu1.isUnconditionalJump() &&
    !mulhsu1.isIndirectJump() && !mulhsu1.isAsmDataDirective() &&
    !mulhsu1.hasSideEffects() );

  WIR_Operation mulhu1(
    RV32IM::OpCode::MULHU, RV32IM::OperationFormat::RRR_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ),
    new WIR_RegisterParameter( x3, WIR_Usage::use ) );
  ufAssert( mulhu1.getSize() == 4 );
  ufAssert(
    !mulhu1.isMemoryAccess() && !mulhu1.isMemoryStore() &&
    !mulhu1.isMemoryLoad() && !mulhu1.isMove() && !mulhu1.isCall() &&
    !mulhu1.isIndirectCall() && !mulhu1.isReturn() && !mulhu1.isJump() &&
    !mulhu1.isConditionalJump() && !mulhu1.isUnconditionalJump() &&
    !mulhu1.isIndirectJump() && !mulhu1.isAsmDataDirective() &&
    !mulhu1.hasSideEffects() );

  WIR_Operation rem1(
    RV32IM::OpCode::REM, RV32IM::OperationFormat::RRR_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ),
    new WIR_RegisterParameter( x3, WIR_Usage::use ) );
  ufAssert( rem1.getSize() == 4 );
  ufAssert(
    !rem1.isMemoryAccess() && !rem1.isMemoryStore() && !rem1.isMemoryLoad() &&
    !rem1.isMove() && !rem1.isCall() && !rem1.isIndirectCall() &&
    !rem1.isReturn() && !rem1.isJump() && !rem1.isConditionalJump() &&
    !rem1.isUnconditionalJump() && !rem1.isIndirectJump() &&
    !rem1.isAsmDataDirective() && !rem1.hasSideEffects() );

  WIR_Operation remu1(
    RV32IM::OpCode::REMU, RV32IM::OperationFormat::RRR_1,
    new WIR_RegisterParameter( x1, WIR_Usage::def ),
    new WIR_RegisterParameter( x2, WIR_Usage::use ),
    new WIR_RegisterParameter( x3, WIR_Usage::use ) );
  ufAssert( remu1.getSize() == 4 );
  ufAssert(
    !remu1.isMemoryAccess() && !remu1.isMemoryStore() &&
    !remu1.isMemoryLoad() && !remu1.isMove() && !remu1.isCall() &&
    !remu1.isIndirectCall() && !remu1.isReturn() && !remu1.isJump() &&
    !remu1.isConditionalJump() && !remu1.isUnconditionalJump() &&
    !remu1.isIndirectJump() && !remu1.isAsmDataDirective() &&
    !remu1.hasSideEffects() );

 return( 0 );
}
