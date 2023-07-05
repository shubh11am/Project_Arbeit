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
  TC_ARegV a1, a2, a3;
  TC_DRegV d1, d2, d3, d4;
  TC_ERegV e1, e2;
  TC_PRegV p1, p2;
  WIR_BasicBlock b;
  WIR_Function f( "main" );

  // The following operations must be accepted according to the TriCore ISA.
  WIR_Operation abs1(
    TC131::OpCode::ABS, TC131::OperationFormat::DD,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ) );
  ufAssert( abs1.getSize() == 4 );
  ufAssert(
    !abs1.isMemoryAccess() && !abs1.isMemoryStore() && !abs1.isMemoryLoad() &&
    !abs1.isMove() && !abs1.isCall() && !abs1.isIndirectCall() &&
    !abs1.isReturn() && !abs1.isJump() && !abs1.isConditionalJump() &&
    !abs1.isUnconditionalJump() && !abs1.isIndirectJump() &&
    !abs1.isAsmDataDirective() && !abs1.hasSideEffects() );

  WIR_Operation abs_b1(
    TC131::OpCode::ABS_B, TC131::OperationFormat::DD,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ) );
  ufAssert( abs_b1.getSize() == 4 );
  ufAssert(
    !abs_b1.isMemoryAccess() && !abs_b1.isMemoryStore() &&
    !abs_b1.isMemoryLoad() && !abs_b1.isMove() && !abs_b1.isCall() &&
    !abs_b1.isIndirectCall() && !abs_b1.isReturn() && !abs_b1.isJump() &&
    !abs_b1.isConditionalJump() && !abs_b1.isUnconditionalJump() && !abs_b1.isIndirectJump() && !abs_b1.isAsmDataDirective() &&
    !abs_b1.hasSideEffects() );

  WIR_Operation abs_h1(
    TC131::OpCode::ABS_H, TC131::OperationFormat::DD,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ) );
  ufAssert( abs_h1.getSize() == 4 );
  ufAssert(
    !abs_h1.isMemoryAccess() && !abs_h1.isMemoryStore() &&
    !abs_h1.isMemoryLoad() && !abs_h1.isMove() && !abs_h1.isCall() &&
    !abs_h1.isIndirectCall() && !abs_h1.isReturn() && !abs_h1.isJump() &&
    !abs_h1.isConditionalJump() && !abs_h1.isUnconditionalJump() &&
    !abs_h1.isIndirectJump() && !abs_h1.isAsmDataDirective() &&
    !abs_h1.hasSideEffects() );

  WIR_Operation absdif1(
    TC131::OpCode::ABSDIF, TC131::OperationFormat::DDC9_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const9_Signed( -128 ) );
  ufAssert( absdif1.getSize() == 4 );
  ufAssert(
    !absdif1.isMemoryAccess() && !absdif1.isMemoryStore() &&
    !absdif1.isMemoryLoad() && !absdif1.isMove() && !absdif1.isCall() &&
    !absdif1.isIndirectCall() && !absdif1.isReturn() && !absdif1.isJump() &&
    !absdif1.isConditionalJump() && !absdif1.isUnconditionalJump() &&
    !absdif1.isIndirectJump() && !absdif1.isAsmDataDirective() &&
    !absdif1.hasSideEffects() );

  WIR_Operation absdif2(
    TC131::OpCode::ABSDIF, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( absdif2.getSize() == 4 );

  WIR_Operation absdif_b1(
    TC131::OpCode::ABSDIF_B, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( absdif_b1.getSize() == 4 );
  ufAssert(
    !absdif_b1.isMemoryAccess() && !absdif_b1.isMemoryStore() &&
    !absdif_b1.isMemoryLoad() && !absdif_b1.isMove() && !absdif_b1.isCall() &&
    !absdif_b1.isIndirectCall() && !absdif_b1.isReturn() && !absdif_b1.isJump() &&
    !absdif_b1.isConditionalJump() && !absdif_b1.isUnconditionalJump() &&
    !absdif_b1.isIndirectJump() && !absdif_b1.isAsmDataDirective() &&
    !absdif_b1.hasSideEffects() );

  WIR_Operation absdif_h1(
    TC131::OpCode::ABSDIF_H, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( absdif_h1.getSize() == 4 );
  ufAssert(
    !absdif_h1.isMemoryAccess() && !absdif_h1.isMemoryStore() &&
    !absdif_h1.isMemoryLoad() && !absdif_h1.isMove() && !absdif_h1.isCall() &&
    !absdif_h1.isIndirectCall() && !absdif_h1.isReturn() && !absdif_h1.isJump() &&
    !absdif_h1.isConditionalJump() && !absdif_h1.isUnconditionalJump() &&
    !absdif_h1.isIndirectJump() && !absdif_h1.isAsmDataDirective() &&
    !absdif_h1.hasSideEffects() );

  WIR_Operation absdifs1(
    TC131::OpCode::ABSDIFS, TC131::OperationFormat::DDC9_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const9_Signed( -128 ) );
  ufAssert( absdifs1.getSize() == 4 );
  ufAssert(
    !absdifs1.isMemoryAccess() && !absdifs1.isMemoryStore() &&
    !absdifs1.isMemoryLoad() && !absdifs1.isMove() && !absdifs1.isCall() &&
    !absdifs1.isIndirectCall() && !absdifs1.isReturn() && !absdifs1.isJump() &&
    !absdifs1.isConditionalJump() && !absdifs1.isUnconditionalJump() &&
    !absdifs1.isIndirectJump() && !absdifs1.isAsmDataDirective() &&
    !absdifs1.hasSideEffects() );

  WIR_Operation absdifs2(
    TC131::OpCode::ABSDIFS, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( absdifs2.getSize() == 4 );

  WIR_Operation absdifs_h1(
    TC131::OpCode::ABSDIFS_H, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( absdifs_h1.getSize() == 4 );
  ufAssert(
    !absdifs_h1.isMemoryAccess() && !absdifs_h1.isMemoryStore() &&
    !absdifs_h1.isMemoryLoad() && !absdifs_h1.isMove() && !absdifs_h1.isCall() &&
    !absdifs_h1.isIndirectCall() && !absdifs_h1.isReturn() && !absdifs_h1.isJump()
    && !absdifs_h1.isConditionalJump() && !absdifs_h1.isUnconditionalJump() &&
    !absdifs_h1.isIndirectJump() && !absdifs_h1.isAsmDataDirective() &&
    !absdifs_h1.hasSideEffects() );

  WIR_Operation abss1(
    TC131::OpCode::ABSS, TC131::OperationFormat::DD,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ) );
  ufAssert( abss1.getSize() == 4 );
  ufAssert(
    !abss1.isMemoryAccess() && !abss1.isMemoryStore() &&
    !abss1.isMemoryLoad() && !abss1.isMove() && !abss1.isCall() &&
    !abss1.isIndirectCall() && !abss1.isReturn() && !abss1.isJump() &&
    !abss1.isConditionalJump() && !abss1.isUnconditionalJump() &&
    !abss1.isIndirectJump() && !abss1.isAsmDataDirective() &&
    !abss1.hasSideEffects() );

  WIR_Operation abss_h1(
    TC131::OpCode::ABSS_H, TC131::OperationFormat::DD,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ) );
  ufAssert( abss_h1.getSize() == 4 );
  ufAssert(
    !abss_h1.isMemoryAccess() && !abss_h1.isMemoryStore() &&
    !abss_h1.isMemoryLoad() && !abss_h1.isMove() && !abss_h1.isCall() &&
    !abss_h1.isIndirectCall() && !abss_h1.isReturn() && !abss_h1.isJump() &&
    !abss_h1.isConditionalJump() && !abss_h1.isUnconditionalJump() &&
    !abss_h1.isIndirectJump() && !abss_h1.isAsmDataDirective() &&
    !abss_h1.hasSideEffects() );

  WIR_Operation add1(
    TC131::OpCode::ADD, TC131::OperationFormat::DDC9_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const9_Signed( -128 ) );
  ufAssert( add1.getSize() == 4 );
  ufAssert(
    !add1.isMemoryAccess() && !add1.isMemoryStore() && !add1.isMemoryLoad() &&
    !add1.isMove() && !add1.isCall() && !add1.isIndirectCall() &&
    !add1.isReturn() && !add1.isJump() && !add1.isConditionalJump() &&
    !add1.isUnconditionalJump() && !add1.isIndirectJump() &&
    !add1.isAsmDataDirective() && !add1.hasSideEffects() );

  WIR_Operation add2(
    TC131::OpCode::ADD, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( add2.getSize() == 4 );

  WIR_Operation add3(
    TC131::OpCode::ADD, TC131::OperationFormat::SDC4_2,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new TC_Const4_Signed( 5 ) );
  ufAssert( add3.getSize() == 2 );

  WIR_Operation add4(
    TC131::OpCode::ADD, TC131::OperationFormat::SDIC4_2,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( tricore.D15(), WIR_Usage::use ),
    new TC_Const4_Signed( -5 ) );
  ufAssert( add4.getSize() == 2 );

  WIR_Operation add5(
    TC131::OpCode::ADD, TC131::OperationFormat::SIDC4,
    new WIR_RegisterParameter( tricore.D15(), WIR_Usage::def ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new TC_Const4_Signed( -5 ) );
  ufAssert( add5.getSize() == 2 );

  WIR_Operation add6(
    TC131::OpCode::ADD, TC131::OperationFormat::SDD_2,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ) );
  ufAssert( add6.getSize() == 2 );

  WIR_Operation add7(
    TC131::OpCode::ADD, TC131::OperationFormat::SDID_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( tricore.D15(), WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ) );
  ufAssert( add7.getSize() == 2 );

  WIR_Operation add8(
    TC131::OpCode::ADD, TC131::OperationFormat::SIDD,
    new WIR_RegisterParameter( tricore.D15(), WIR_Usage::def ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ) );
  ufAssert( add8.getSize() == 2 );

  WIR_Operation add_a1(
    TC131::OpCode::ADD_A, TC131::OperationFormat::AAA,
    new WIR_RegisterParameter( a1, WIR_Usage::def ),
    new WIR_RegisterParameter( a2, WIR_Usage::use ),
    new WIR_RegisterParameter( a3, WIR_Usage::use ) );
  ufAssert( add_a1.getSize() == 4 );
  ufAssert(
    !add_a1.isMemoryAccess() && !add_a1.isMemoryStore() &&
    !add_a1.isMemoryLoad() && !add_a1.isMove() && !add_a1.isCall() &&
    !add_a1.isIndirectCall() && !add_a1.isReturn() && !add_a1.isJump() &&
    !add_a1.isConditionalJump() && !add_a1.isUnconditionalJump() &&
    !add_a1.isIndirectJump() && !add_a1.isAsmDataDirective() &&
    !add_a1.hasSideEffects() );

  WIR_Operation add_a2(
    TC131::OpCode::ADD_A, TC131::OperationFormat::SAC4_2,
    new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
    new TC_Const4_Signed( -5 ) );
  ufAssert( add_a2.getSize() == 2 );

  WIR_Operation add_a3(
    TC131::OpCode::ADD_A, TC131::OperationFormat::SAA_5,
    new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( a2, WIR_Usage::use ) );
  ufAssert( add_a3.getSize() == 2 );

  WIR_Operation add_b1(
    TC131::OpCode::ADD_B, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( add_b1.getSize() == 4 );
  ufAssert(
    !add_b1.isMemoryAccess() && !add_b1.isMemoryStore() &&
    !add_b1.isMemoryLoad() && !add_b1.isMove() && !add_b1.isCall() &&
    !add_b1.isIndirectCall() && !add_b1.isReturn() && !add_b1.isJump() &&
    !add_b1.isConditionalJump() && !add_b1.isUnconditionalJump() &&
    !add_b1.isIndirectJump() && !add_b1.isAsmDataDirective() &&
    !add_b1.hasSideEffects() );

  WIR_Operation add_f1(
    TC131::OpCode::ADD_F, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( add_f1.getSize() == 4 );
  ufAssert(
    !add_f1.isMemoryAccess() && !add_f1.isMemoryStore() &&
    !add_f1.isMemoryLoad() && !add_f1.isMove() && !add_f1.isCall() &&
    !add_f1.isIndirectCall() && !add_f1.isReturn() && !add_f1.isJump() &&
    !add_f1.isConditionalJump() && !add_f1.isUnconditionalJump() &&
    !add_f1.isIndirectJump() && !add_f1.isAsmDataDirective() &&
    !add_f1.hasSideEffects() );

  WIR_Operation add_h1(
    TC131::OpCode::ADD_H, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( add_h1.getSize() == 4 );
  ufAssert(
    !add_h1.isMemoryAccess() && !add_h1.isMemoryStore() &&
    !add_h1.isMemoryLoad() && !add_h1.isMove() && !add_h1.isCall() &&
    !add_h1.isIndirectCall() && !add_h1.isReturn() && !add_h1.isJump() &&
    !add_h1.isConditionalJump() && !add_h1.isUnconditionalJump() &&
    !add_h1.isIndirectJump() && !add_h1.isAsmDataDirective() &&
    !add_h1.hasSideEffects() );

  WIR_Operation addc1(
    TC131::OpCode::ADDC, TC131::OperationFormat::DDC9PSW_2,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const9_Signed( -128 ),
    new WIR_RegisterParameter( tricore.PSW_C(), WIR_Usage::defuse ) );
  ufAssert( addc1.getSize() == 4 );
  ufAssert(
    !addc1.isMemoryAccess() && !addc1.isMemoryStore() &&
    !addc1.isMemoryLoad() && !addc1.isMove() && !addc1.isCall() &&
    !addc1.isIndirectCall() && !addc1.isReturn() && !addc1.isJump() &&
    !addc1.isConditionalJump() && !addc1.isUnconditionalJump() &&
    !addc1.isIndirectJump() && !addc1.isAsmDataDirective() &&
    !addc1.hasSideEffects() );

  WIR_Operation addc2(
    TC131::OpCode::ADDC, TC131::OperationFormat::DDDPSW_2,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( tricore.PSW_C(), WIR_Usage::defuse ) );
  ufAssert( addc2.getSize() == 4 );

  WIR_Operation addi1(
    TC131::OpCode::ADDI, TC131::OperationFormat::DDC16_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const16_Signed( -26361 ) );
  ufAssert( addi1.getSize() == 4 );
  ufAssert(
    !addi1.isMemoryAccess() && !addi1.isMemoryStore() &&
    !addi1.isMemoryLoad() && !addi1.isMove() && !addi1.isCall() &&
    !addi1.isIndirectCall() && !addi1.isReturn() && !addi1.isJump() &&
    !addi1.isConditionalJump() && !addi1.isUnconditionalJump() &&
    !addi1.isIndirectJump() && !addi1.isAsmDataDirective() &&
    !addi1.hasSideEffects() );

  WIR_Operation addih1(
    TC131::OpCode::ADDIH, TC131::OperationFormat::DDC16_2,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const16_Unsigned( 26361 ) );
  ufAssert( addih1.getSize() == 4 );
  ufAssert(
    !addih1.isMemoryAccess() && !addih1.isMemoryStore() &&
    !addih1.isMemoryLoad() && !addih1.isMove() && !addih1.isCall() &&
    !addih1.isIndirectCall() && !addih1.isReturn() && !addih1.isJump() &&
    !addih1.isConditionalJump() && !addih1.isUnconditionalJump() &&
    !addih1.isIndirectJump() && !addih1.isAsmDataDirective() &&
    !addih1.hasSideEffects() );

  WIR_Operation addih_a1(
    TC131::OpCode::ADDIH_A, TC131::OperationFormat::AAC16,
    new WIR_RegisterParameter( a1, WIR_Usage::def ),
    new WIR_RegisterParameter( a2, WIR_Usage::use ),
    new TC_Const16_Unsigned( 26361 ) );
  ufAssert( addih_a1.getSize() == 4 );
  ufAssert(
    !addih_a1.isMemoryAccess() && !addih_a1.isMemoryStore() &&
    !addih_a1.isMemoryLoad() && !addih_a1.isMove() && !addih_a1.isCall() &&
    !addih_a1.isIndirectCall() && !addih_a1.isReturn() && !addih_a1.isJump() &&
    !addih_a1.isConditionalJump() && !addih_a1.isUnconditionalJump() &&
    !addih_a1.isIndirectJump() && !addih_a1.isAsmDataDirective() &&
    !addih_a1.hasSideEffects() );

  WIR_Operation adds1(
    TC131::OpCode::ADDS, TC131::OperationFormat::DDC9_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const9_Signed( -128 ) );
  ufAssert( adds1.getSize() == 4 );
  ufAssert(
    !adds1.isMemoryAccess() && !adds1.isMemoryStore() &&
    !adds1.isMemoryLoad() && !adds1.isMove() && !adds1.isCall() &&
    !adds1.isIndirectCall() && !adds1.isReturn() && !adds1.isJump() &&
    !adds1.isConditionalJump() && !adds1.isUnconditionalJump() &&
    !adds1.isIndirectJump() && !adds1.isAsmDataDirective() &&
    !adds1.hasSideEffects() );

  WIR_Operation adds2(
    TC131::OpCode::ADDS, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( adds2.getSize() == 4 );

  WIR_Operation adds3(
    TC131::OpCode::ADDS, TC131::OperationFormat::SDD_2,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ) );
  ufAssert( adds3.getSize() == 2 );

  WIR_Operation adds_h1(
    TC131::OpCode::ADDS_H, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( adds_h1.getSize() == 4 );
  ufAssert(
    !adds_h1.isMemoryAccess() && !adds_h1.isMemoryStore() &&
    !adds_h1.isMemoryLoad() && !adds_h1.isMove() && !adds_h1.isCall() &&
    !adds_h1.isIndirectCall() && !adds_h1.isReturn() && !adds_h1.isJump() &&
    !adds_h1.isConditionalJump() && !adds_h1.isUnconditionalJump() &&
    !adds_h1.isIndirectJump() && !adds_h1.isAsmDataDirective() &&
    !adds_h1.hasSideEffects() );

  WIR_Operation adds_hu1(
    TC131::OpCode::ADDS_HU, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( adds_hu1.getSize() == 4 );
  ufAssert(
    !adds_hu1.isMemoryAccess() && !adds_hu1.isMemoryStore() &&
    !adds_hu1.isMemoryLoad() && !adds_hu1.isMove() && !adds_hu1.isCall() &&
    !adds_hu1.isIndirectCall() && !adds_hu1.isReturn() && !adds_hu1.isJump() &&
    !adds_hu1.isConditionalJump() && !adds_hu1.isUnconditionalJump() &&
    !adds_hu1.isIndirectJump() && !adds_hu1.isAsmDataDirective() &&
    !adds_hu1.hasSideEffects() );

  WIR_Operation adds_u1(
    TC131::OpCode::ADDS_U, TC131::OperationFormat::DDC9_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const9_Signed( -128 ) );
  ufAssert( adds_u1.getSize() == 4 );
  ufAssert(
    !adds_u1.isMemoryAccess() && !adds_u1.isMemoryStore() &&
    !adds_u1.isMemoryLoad() && !adds_u1.isMove() && !adds_u1.isCall() &&
    !adds_u1.isIndirectCall() && !adds_u1.isReturn() && !adds_u1.isJump() &&
    !adds_u1.isConditionalJump() && !adds_u1.isUnconditionalJump() &&
    !adds_u1.isIndirectJump() && !adds_u1.isAsmDataDirective() &&
    !adds_u1.hasSideEffects() );

  WIR_Operation adds_u2(
    TC131::OpCode::ADDS_U, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( adds_u2.getSize() == 4 );

  WIR_Operation addsc_a1(
    TC131::OpCode::ADDSC_A, TC131::OperationFormat::AADC2,
    new WIR_RegisterParameter( a1, WIR_Usage::def ),
    new WIR_RegisterParameter( a2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new TC_Const2_Unsigned( 2 ) );
  ufAssert( addsc_a1.getSize() == 4 );
  ufAssert(
    !addsc_a1.isMemoryAccess() && !addsc_a1.isMemoryStore() &&
    !addsc_a1.isMemoryLoad() && !addsc_a1.isMove() && !addsc_a1.isCall() &&
    !addsc_a1.isIndirectCall() && !addsc_a1.isReturn() && !addsc_a1.isJump() &&
    !addsc_a1.isConditionalJump() && !addsc_a1.isUnconditionalJump() &&
    !addsc_a1.isIndirectJump() && !addsc_a1.isAsmDataDirective() &&
    !addsc_a1.hasSideEffects() );

  WIR_Operation addsc_a2(
    TC131::OpCode::ADDSC_A, TC131::OperationFormat::SAAIC2,
    new WIR_RegisterParameter( a1, WIR_Usage::def ),
    new WIR_RegisterParameter( a2, WIR_Usage::use ),
    new WIR_RegisterParameter( tricore.D15(), WIR_Usage::use ),
    new TC_Const2_Unsigned( 3 ) );
  ufAssert( addsc_a2.getSize() == 2 );

  WIR_Operation addsc_at1(
    TC131::OpCode::ADDSC_AT, TC131::OperationFormat::AAD,
    new WIR_RegisterParameter( a1, WIR_Usage::def ),
    new WIR_RegisterParameter( a2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ) );
  ufAssert( addsc_at1.getSize() == 4 );
  ufAssert(
    !addsc_at1.isMemoryAccess() && !addsc_at1.isMemoryStore() &&
    !addsc_at1.isMemoryLoad() && !addsc_at1.isMove() && !addsc_at1.isCall() &&
    !addsc_at1.isIndirectCall() && !addsc_at1.isReturn() && !addsc_at1.isJump() &&
    !addsc_at1.isConditionalJump() && !addsc_at1.isUnconditionalJump() &&
    !addsc_at1.isIndirectJump() && !addsc_at1.isAsmDataDirective() &&
    !addsc_at1.hasSideEffects() );

  WIR_Operation addx1(
    TC131::OpCode::ADDX, TC131::OperationFormat::DDC9PSW_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const9_Signed( -128 ),
    new WIR_RegisterParameter( tricore.PSW_C(), WIR_Usage::def ) );
  ufAssert( addx1.getSize() == 4 );
  ufAssert(
    !addx1.isMemoryAccess() && !addx1.isMemoryStore() &&
    !addx1.isMemoryLoad() && !addx1.isMove() && !addx1.isCall() &&
    !addx1.isIndirectCall() && !addx1.isReturn() && !addx1.isJump() &&
    !addx1.isConditionalJump() && !addx1.isUnconditionalJump() &&
    !addx1.isIndirectJump() && !addx1.isAsmDataDirective() &&
    !addx1.hasSideEffects() );

  WIR_Operation addx2(
    TC131::OpCode::ADDX, TC131::OperationFormat::DDDPSW_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( tricore.PSW_C(), WIR_Usage::def ) );
  ufAssert( addx2.getSize() == 4 );

  WIR_Operation and1(
    TC131::OpCode::AND, TC131::OperationFormat::DDC9_2,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const9_Unsigned( 128 ) );
  ufAssert( and1.getSize() == 4 );
  ufAssert(
    !and1.isMemoryAccess() && !and1.isMemoryStore() && !and1.isMemoryLoad() &&
    !and1.isMove() && !and1.isCall() && !and1.isIndirectCall() &&
    !and1.isReturn() && !and1.isJump() && !and1.isConditionalJump() &&
    !and1.isUnconditionalJump() && !and1.isIndirectJump() &&
    !and1.isAsmDataDirective() && !and1.hasSideEffects() );

  WIR_Operation and2(
    TC131::OpCode::AND, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( and2.getSize() == 4 );

  WIR_Operation and3(
    TC131::OpCode::AND, TC131::OperationFormat::SIC8_2,
    new WIR_RegisterParameter( tricore.D15(), WIR_Usage::defuse ),
    new TC_Const8_Unsigned( 64 ) );
  ufAssert( and3.getSize() == 2 );

  WIR_Operation and4(
    TC131::OpCode::AND, TC131::OperationFormat::SDD_2,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ) );
  ufAssert( and4.getSize() == 2 );

  WIR_Operation and_and_t1(
    TC131::OpCode::AND_AND_T, TC131::OperationFormat::DDC5DC5_2,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const5_Unsigned( 13 ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new TC_Const5_Unsigned( 27 ) );
  ufAssert( and_and_t1.getSize() == 4 );
  ufAssert(
    !and_and_t1.isMemoryAccess() && !and_and_t1.isMemoryStore() &&
    !and_and_t1.isMemoryLoad() && !and_and_t1.isMove() && !and_and_t1.isCall() &&
    !and_and_t1.isIndirectCall() && !and_and_t1.isReturn() &&
    !and_and_t1.isJump() && !and_and_t1.isConditionalJump() &&
    !and_and_t1.isUnconditionalJump() && !and_and_t1.isIndirectJump() &&
    !and_and_t1.isAsmDataDirective() && !and_and_t1.hasSideEffects() );

  WIR_Operation and_andn_t1(
    TC131::OpCode::AND_ANDN_T, TC131::OperationFormat::DDC5DC5_2,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const5_Unsigned( 13 ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new TC_Const5_Unsigned( 27 ) );
  ufAssert( and_andn_t1.getSize() == 4 );
  ufAssert(
    !and_andn_t1.isMemoryAccess() && !and_andn_t1.isMemoryStore() &&
    !and_andn_t1.isMemoryLoad() && !and_andn_t1.isMove() &&
    !and_andn_t1.isCall() && !and_andn_t1.isIndirectCall() &&
    !and_andn_t1.isReturn() && !and_andn_t1.isJump() &&
    !and_andn_t1.isConditionalJump() && !and_andn_t1.isUnconditionalJump() &&
    !and_andn_t1.isIndirectJump() && !and_andn_t1.isAsmDataDirective() &&
    !and_andn_t1.hasSideEffects() );

  WIR_Operation and_eq1(
    TC131::OpCode::AND_EQ, TC131::OperationFormat::DDC9_3,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const9_Signed( -128 ) );
  ufAssert( and_eq1.getSize() == 4 );
  ufAssert(
    !and_eq1.isMemoryAccess() && !and_eq1.isMemoryStore() &&
    !and_eq1.isMemoryLoad() && !and_eq1.isMove() && !and_eq1.isCall() &&
    !and_eq1.isIndirectCall() && !and_eq1.isReturn() && !and_eq1.isJump() &&
    !and_eq1.isConditionalJump() && !and_eq1.isUnconditionalJump() &&
    !and_eq1.isIndirectJump() && !and_eq1.isAsmDataDirective() &&
    !and_eq1.hasSideEffects() );

  WIR_Operation and_eq2(
    TC131::OpCode::AND_EQ, TC131::OperationFormat::DDD_2,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( and_eq2.getSize() == 4 );

  WIR_Operation and_ge1(
    TC131::OpCode::AND_GE, TC131::OperationFormat::DDC9_3,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const9_Signed( -128 ) );
  ufAssert( and_ge1.getSize() == 4 );
  ufAssert(
    !and_ge1.isMemoryAccess() && !and_ge1.isMemoryStore() &&
    !and_ge1.isMemoryLoad() && !and_ge1.isMove() && !and_ge1.isCall() &&
    !and_ge1.isIndirectCall() && !and_ge1.isReturn() && !and_ge1.isJump() &&
    !and_ge1.isConditionalJump() && !and_ge1.isUnconditionalJump() &&
    !and_ge1.isIndirectJump() && !and_ge1.isAsmDataDirective() &&
    !and_ge1.hasSideEffects() );

  WIR_Operation and_ge2(
    TC131::OpCode::AND_GE, TC131::OperationFormat::DDD_2,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( and_ge2.getSize() == 4 );

  WIR_Operation and_ge_u1(
    TC131::OpCode::AND_GE_U, TC131::OperationFormat::DDC9_4,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const9_Unsigned( 128 ) );
  ufAssert( and_ge_u1.getSize() == 4 );
  ufAssert(
    !and_ge_u1.isMemoryAccess() && !and_ge_u1.isMemoryStore() &&
    !and_ge_u1.isMemoryLoad() && !and_ge_u1.isMove() && !and_ge_u1.isCall() &&
    !and_ge_u1.isIndirectCall() && !and_ge_u1.isReturn() &&
    !and_ge_u1.isJump() && !and_ge_u1.isConditionalJump() &&
    !and_ge_u1.isUnconditionalJump() && !and_ge_u1.isIndirectJump() &&
    !and_ge_u1.isAsmDataDirective() && !and_ge_u1.hasSideEffects() );

  WIR_Operation and_ge_u2(
    TC131::OpCode::AND_GE_U, TC131::OperationFormat::DDD_2,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( and_ge_u2.getSize() == 4 );

  WIR_Operation and_lt1(
    TC131::OpCode::AND_LT, TC131::OperationFormat::DDC9_3,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const9_Signed( -128 ) );
  ufAssert( and_lt1.getSize() == 4 );
  ufAssert(
    !and_lt1.isMemoryAccess() && !and_lt1.isMemoryStore() &&
    !and_lt1.isMemoryLoad() && !and_lt1.isMove() && !and_lt1.isCall() &&
    !and_lt1.isIndirectCall() && !and_lt1.isReturn() && !and_lt1.isJump() &&
    !and_lt1.isConditionalJump() && !and_lt1.isUnconditionalJump() &&
    !and_lt1.isIndirectJump() && !and_lt1.isAsmDataDirective() &&
    !and_lt1.hasSideEffects() );

  WIR_Operation and_lt2(
    TC131::OpCode::AND_LT, TC131::OperationFormat::DDD_2,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( and_lt2.getSize() == 4 );

  WIR_Operation and_lt_u1(
    TC131::OpCode::AND_LT_U, TC131::OperationFormat::DDC9_4,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const9_Unsigned( 128 ) );
  ufAssert( and_lt_u1.getSize() == 4 );
  ufAssert(
    !and_lt_u1.isMemoryAccess() && !and_lt_u1.isMemoryStore() &&
    !and_lt_u1.isMemoryLoad() && !and_lt_u1.isMove() && !and_lt_u1.isCall() &&
    !and_lt_u1.isIndirectCall() && !and_lt_u1.isReturn() &&
    !and_lt_u1.isJump() && !and_lt_u1.isConditionalJump() &&
    !and_lt_u1.isUnconditionalJump() && !and_lt_u1.isIndirectJump() &&
    !and_lt_u1.isAsmDataDirective() && !and_lt_u1.hasSideEffects() );

  WIR_Operation and_lt_u2(
    TC131::OpCode::AND_LT_U, TC131::OperationFormat::DDD_2,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( and_lt_u2.getSize() == 4 );

  WIR_Operation and_ne1(
    TC131::OpCode::AND_NE, TC131::OperationFormat::DDC9_3,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const9_Signed( -128 ) );
  ufAssert( and_ne1.getSize() == 4 );
  ufAssert(
    !and_ne1.isMemoryAccess() && !and_ne1.isMemoryStore() &&
    !and_ne1.isMemoryLoad() && !and_ne1.isMove() && !and_ne1.isCall() &&
    !and_ne1.isIndirectCall() && !and_ne1.isReturn() && !and_ne1.isJump() &&
    !and_ne1.isConditionalJump() && !and_ne1.isUnconditionalJump() &&
    !and_ne1.isIndirectJump() && !and_ne1.isAsmDataDirective() &&
    !and_ne1.hasSideEffects() );

  WIR_Operation and_ne2(
    TC131::OpCode::AND_NE, TC131::OperationFormat::DDD_2,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( and_ne2.getSize() == 4 );

  WIR_Operation and_nor_t1(
    TC131::OpCode::AND_NOR_T, TC131::OperationFormat::DDC5DC5_2,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const5_Unsigned( 13 ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new TC_Const5_Unsigned( 27 ) );
  ufAssert( and_nor_t1.getSize() == 4 );
  ufAssert(
    !and_nor_t1.isMemoryAccess() && !and_nor_t1.isMemoryStore() &&
    !and_nor_t1.isMemoryLoad() && !and_nor_t1.isMove() &&
    !and_nor_t1.isCall() && !and_nor_t1.isIndirectCall() &&
    !and_nor_t1.isReturn() && !and_nor_t1.isJump() &&
    !and_nor_t1.isConditionalJump() && !and_nor_t1.isUnconditionalJump() &&
    !and_nor_t1.isIndirectJump() && !and_nor_t1.isAsmDataDirective() &&
    !and_nor_t1.hasSideEffects() );

  WIR_Operation and_or_t1(
    TC131::OpCode::AND_OR_T, TC131::OperationFormat::DDC5DC5_2,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const5_Unsigned( 13 ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new TC_Const5_Unsigned( 27 ) );
  ufAssert( and_or_t1.getSize() == 4 );
  ufAssert(
    !and_or_t1.isMemoryAccess() && !and_or_t1.isMemoryStore() &&
    !and_or_t1.isMemoryLoad() && !and_or_t1.isMove() && !and_or_t1.isCall() &&
    !and_or_t1.isIndirectCall() && !and_or_t1.isReturn() &&
    !and_or_t1.isJump() && !and_or_t1.isConditionalJump() &&
    !and_or_t1.isUnconditionalJump() && !and_or_t1.isIndirectJump() &&
    !and_or_t1.isAsmDataDirective() && !and_or_t1.hasSideEffects() );

  WIR_Operation and_t1(
    TC131::OpCode::AND_T, TC131::OperationFormat::DDC5DC5_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const5_Unsigned( 13 ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new TC_Const5_Unsigned( 27 ) );
  ufAssert( and_t1.getSize() == 4 );
  ufAssert(
    !and_t1.isMemoryAccess() && !and_t1.isMemoryStore() &&
    !and_t1.isMemoryLoad() && !and_t1.isMove() && !and_t1.isCall() &&
    !and_t1.isIndirectCall() && !and_t1.isReturn() && !and_t1.isJump() &&
    !and_t1.isConditionalJump() && !and_t1.isUnconditionalJump() &&
    !and_t1.isIndirectJump() && !and_t1.isAsmDataDirective() &&
    !and_t1.hasSideEffects() );

  WIR_Operation andn1(
    TC131::OpCode::ANDN, TC131::OperationFormat::DDC9_2,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const9_Unsigned( 128 ) );
  ufAssert( andn1.getSize() == 4 );
  ufAssert(
    !andn1.isMemoryAccess() && !andn1.isMemoryStore() &&
    !andn1.isMemoryLoad() && !andn1.isMove() && !andn1.isCall() &&
    !andn1.isIndirectCall() && !andn1.isReturn() && !andn1.isJump() &&
    !andn1.isConditionalJump() && !andn1.isUnconditionalJump() &&
    !andn1.isIndirectJump() && !andn1.isAsmDataDirective() &&
    !andn1.hasSideEffects() );

  WIR_Operation andn2(
    TC131::OpCode::ANDN, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( andn2.getSize() == 4 );

  WIR_Operation andn_t1(
    TC131::OpCode::ANDN_T, TC131::OperationFormat::DDC5DC5_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const5_Unsigned( 13 ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new TC_Const5_Unsigned( 27 ) );
  ufAssert( andn_t1.getSize() == 4 );
  ufAssert(
    !andn_t1.isMemoryAccess() && !andn_t1.isMemoryStore() &&
    !andn_t1.isMemoryLoad() && !andn_t1.isMove() && !andn_t1.isCall() &&
    !andn_t1.isIndirectCall() && !andn_t1.isReturn() && !andn_t1.isJump() &&
    !andn_t1.isConditionalJump() && !andn_t1.isUnconditionalJump() &&
    !andn_t1.isIndirectJump() && !andn_t1.isAsmDataDirective() &&
    !andn_t1.hasSideEffects() );

  WIR_Operation bisr1(
    TC131::OpCode::BISR, TC131::OperationFormat::C9,
    new TC_Const9_Unsigned( 128 ) );
  ufAssert( bisr1.getSize() == 4 );
  ufAssert(
    bisr1.isMemoryAccess() && !bisr1.isMemoryStore() && !bisr1.isMemoryLoad() &&
    !bisr1.isMove() && !bisr1.isCall() && !bisr1.isIndirectCall() &&
    !bisr1.isReturn() && !bisr1.isJump() && !bisr1.isConditionalJump() &&
    !bisr1.isUnconditionalJump() && !bisr1.isIndirectJump() &&
    !bisr1.isAsmDataDirective() && bisr1.hasSideEffects() );

  WIR_Operation bisr2(
    TC131::OpCode::BISR, TC131::OperationFormat::SC8,
    new TC_Const8_Unsigned( 64 ) );
  ufAssert( bisr2.getSize() == 2 );

  WIR_Operation bmerge1(
    TC131::OpCode::BMERGE, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( bmerge1.getSize() == 4 );
  ufAssert(
    !bmerge1.isMemoryAccess() && !bmerge1.isMemoryStore() &&
    !bmerge1.isMemoryLoad() && !bmerge1.isMove() && !bmerge1.isCall() &&
    !bmerge1.isIndirectCall() && !bmerge1.isReturn() && !bmerge1.isJump() &&
    !bmerge1.isConditionalJump() && !bmerge1.isUnconditionalJump() &&
    !bmerge1.isIndirectJump() && !bmerge1.isAsmDataDirective() &&
    !bmerge1.hasSideEffects() );

  WIR_Operation bsplit1(
    TC131::OpCode::BSPLIT, TC131::OperationFormat::ED,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( bsplit1.getSize() == 4 );
  ufAssert(
    !bsplit1.isMemoryAccess() && !bsplit1.isMemoryStore() &&
    !bsplit1.isMemoryLoad() && !bsplit1.isMove() && !bsplit1.isCall() &&
    !bsplit1.isIndirectCall() && !bsplit1.isReturn() && !bsplit1.isJump() &&
    !bsplit1.isConditionalJump() && !bsplit1.isUnconditionalJump() &&
    !bsplit1.isIndirectJump() && !bsplit1.isAsmDataDirective() &&
    !bsplit1.hasSideEffects() );

  WIR_Operation cachea_i1(
    TC131::OpCode::CACHEA_I, TC131::OperationFormat::AC10BOA,
    new WIR_RegisterParameter( a1, WIR_Usage::use ),
    new TC_Const10_Signed( -486 ) );
  ufAssert( cachea_i1.getSize() == 4 );
  ufAssert(
    cachea_i1.isMemoryAccess() && !cachea_i1.isMemoryStore() &&
    !cachea_i1.isMemoryLoad() && !cachea_i1.isMove() && !cachea_i1.isCall() &&
    !cachea_i1.isIndirectCall() && !cachea_i1.isReturn() &&
    !cachea_i1.isJump() && !cachea_i1.isConditionalJump() &&
    !cachea_i1.isUnconditionalJump() && !cachea_i1.isIndirectJump() &&
    !cachea_i1.isAsmDataDirective() && cachea_i1.hasSideEffects() );

  WIR_Operation cachea_i2(
    TC131::OpCode::CACHEA_I, TC131::OperationFormat::PBRA,
    new WIR_RegisterParameter( p1, WIR_Usage::defuse ) );
  ufAssert( cachea_i2.getSize() == 4 );

  WIR_Operation cachea_i3(
    TC131::OpCode::CACHEA_I, TC131::OperationFormat::PC10CA,
    new WIR_RegisterParameter( p1, WIR_Usage::defuse ),
    new TC_Const10_Signed( -486 ) );
  ufAssert( cachea_i3.getSize() == 4 );

  WIR_Operation cachea_i4(
    TC131::OpCode::CACHEA_I, TC131::OperationFormat::AC10PIA,
    new WIR_AddressingModeParameter( TC131::AddressingMode::post ),
    new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
    new TC_Const10_Signed( -486 ) );
  ufAssert( cachea_i4.getSize() == 4 );

  WIR_Operation cachea_i5(
    TC131::OpCode::CACHEA_I, TC131::OperationFormat::AC10PIA,
    new WIR_AddressingModeParameter( TC131::AddressingMode::pre ),
    new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
    new TC_Const10_Signed( -486 ) );
  ufAssert( cachea_i5.getSize() == 4 );

  WIR_Operation cachea_w1(
    TC131::OpCode::CACHEA_W, TC131::OperationFormat::AC10BOA,
    new WIR_RegisterParameter( a1, WIR_Usage::use ),
    new TC_Const10_Signed( -486 ) );
  ufAssert( cachea_w1.getSize() == 4 );
  ufAssert(
    cachea_w1.isMemoryAccess() && !cachea_w1.isMemoryStore() &&
    !cachea_w1.isMemoryLoad() && !cachea_w1.isMove() && !cachea_w1.isCall() &&
    !cachea_w1.isIndirectCall() && !cachea_w1.isReturn() &&
    !cachea_w1.isJump() && !cachea_w1.isConditionalJump() &&
    !cachea_w1.isUnconditionalJump() && !cachea_w1.isIndirectJump() &&
    !cachea_w1.isAsmDataDirective() && cachea_w1.hasSideEffects() );

  WIR_Operation cachea_w2(
    TC131::OpCode::CACHEA_W, TC131::OperationFormat::PBRA,
    new WIR_RegisterParameter( p1, WIR_Usage::defuse ) );
  ufAssert( cachea_w2.getSize() == 4 );

  WIR_Operation cachea_w3(
    TC131::OpCode::CACHEA_W, TC131::OperationFormat::PC10CA,
    new WIR_RegisterParameter( p1, WIR_Usage::defuse ),
    new TC_Const10_Signed( -486 ) );
  ufAssert( cachea_w3.getSize() == 4 );

  WIR_Operation cachea_w4(
    TC131::OpCode::CACHEA_W, TC131::OperationFormat::AC10PIA,
    new WIR_AddressingModeParameter( TC131::AddressingMode::post ),
    new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
    new TC_Const10_Signed( -486 ) );
  ufAssert( cachea_w4.getSize() == 4 );

  WIR_Operation cachea_w5(
    TC131::OpCode::CACHEA_W, TC131::OperationFormat::AC10PIA,
    new WIR_AddressingModeParameter( TC131::AddressingMode::pre ),
    new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
    new TC_Const10_Signed( -486 ) );
  ufAssert( cachea_w5.getSize() == 4 );

  WIR_Operation cachea_wi1(
    TC131::OpCode::CACHEA_WI, TC131::OperationFormat::AC10BOA,
    new WIR_RegisterParameter( a1, WIR_Usage::use ),
    new TC_Const10_Signed( -486 ) );
  ufAssert( cachea_wi1.getSize() == 4 );
  ufAssert(
    cachea_wi1.isMemoryAccess() && !cachea_wi1.isMemoryStore() &&
    !cachea_wi1.isMemoryLoad() && !cachea_wi1.isMove() &&
    !cachea_wi1.isCall() && !cachea_wi1.isIndirectCall() &&
    !cachea_wi1.isReturn() && !cachea_wi1.isJump() &&
    !cachea_wi1.isConditionalJump() && !cachea_wi1.isUnconditionalJump() &&
    !cachea_wi1.isIndirectJump() && !cachea_wi1.isAsmDataDirective() &&
    cachea_wi1.hasSideEffects() );

  WIR_Operation cachea_wi2(
    TC131::OpCode::CACHEA_WI, TC131::OperationFormat::PBRA,
    new WIR_RegisterParameter( p1, WIR_Usage::defuse ) );
  ufAssert( cachea_wi2.getSize() == 4 );

  WIR_Operation cachea_wi3(
    TC131::OpCode::CACHEA_WI, TC131::OperationFormat::PC10CA,
    new WIR_RegisterParameter( p1, WIR_Usage::defuse ),
    new TC_Const10_Signed( -486 ) );
  ufAssert( cachea_wi3.getSize() == 4 );

  WIR_Operation cachea_wi4(
    TC131::OpCode::CACHEA_WI, TC131::OperationFormat::AC10PIA,
    new WIR_AddressingModeParameter( TC131::AddressingMode::post ),
    new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
    new TC_Const10_Signed( -486 ) );
  ufAssert( cachea_wi4.getSize() == 4 );

  WIR_Operation cachea_wi5(
    TC131::OpCode::CACHEA_WI, TC131::OperationFormat::AC10PIA,
    new WIR_AddressingModeParameter( TC131::AddressingMode::pre ),
    new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
    new TC_Const10_Signed( -486 ) );
  ufAssert( cachea_wi5.getSize() == 4 );

  WIR_Operation cachei_w1(
    TC131::OpCode::CACHEI_W, TC131::OperationFormat::AC10BOA,
    new WIR_RegisterParameter( a1, WIR_Usage::use ),
    new TC_Const10_Signed( -486 ) );
  ufAssert( cachei_w1.getSize() == 4 );
  ufAssert(
    cachei_w1.isMemoryAccess() && !cachei_w1.isMemoryStore() &&
    !cachei_w1.isMemoryLoad() && !cachei_w1.isMove() && !cachei_w1.isCall() &&
    !cachei_w1.isIndirectCall() && !cachei_w1.isReturn() &&
    !cachei_w1.isJump() && !cachei_w1.isConditionalJump() &&
    !cachei_w1.isUnconditionalJump() && !cachei_w1.isIndirectJump() &&
    !cachei_w1.isAsmDataDirective() && cachei_w1.hasSideEffects() );

  WIR_Operation cachei_w2(
    TC131::OpCode::CACHEI_W, TC131::OperationFormat::AC10PIA,
    new WIR_AddressingModeParameter( TC131::AddressingMode::post ),
    new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
    new TC_Const10_Signed( -486 ) );
  ufAssert( cachei_w2.getSize() == 4 );

  WIR_Operation cachei_w3(
    TC131::OpCode::CACHEI_W, TC131::OperationFormat::AC10PIA,
    new WIR_AddressingModeParameter( TC131::AddressingMode::pre ),
    new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
    new TC_Const10_Signed( -486 ) );
  ufAssert( cachei_w3.getSize() == 4 );

  WIR_Operation cachei_wi1(
    TC131::OpCode::CACHEI_WI, TC131::OperationFormat::AC10BOA,
    new WIR_RegisterParameter( a1, WIR_Usage::use ),
    new TC_Const10_Signed( -486 ) );
  ufAssert( cachei_wi1.getSize() == 4 );
  ufAssert(
    cachei_wi1.isMemoryAccess() && !cachei_wi1.isMemoryStore() &&
    !cachei_wi1.isMemoryLoad() && !cachei_wi1.isMove() &&
    !cachei_wi1.isCall() && !cachei_wi1.isIndirectCall() &&
    !cachei_wi1.isReturn() && !cachei_wi1.isJump() &&
    !cachei_wi1.isConditionalJump() && !cachei_wi1.isUnconditionalJump() &&
    !cachei_wi1.isIndirectJump() && !cachei_wi1.isAsmDataDirective() &&
    cachei_wi1.hasSideEffects() );

  WIR_Operation cachei_wi2(
    TC131::OpCode::CACHEI_WI, TC131::OperationFormat::AC10PIA,
    new WIR_AddressingModeParameter( TC131::AddressingMode::post ),
    new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
    new TC_Const10_Signed( -486 ) );
  ufAssert( cachei_wi2.getSize() == 4 );

  WIR_Operation cachei_wi3(
    TC131::OpCode::CACHEI_WI, TC131::OperationFormat::AC10PIA,
    new WIR_AddressingModeParameter( TC131::AddressingMode::pre ),
    new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
    new TC_Const10_Signed( -486 ) );
  ufAssert( cachei_wi3.getSize() == 4 );

  WIR_Operation cadd1(
    TC131::OpCode::CADD, TC131::OperationFormat::DDDC9_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new TC_Const9_Signed( -128 ) );
  ufAssert( cadd1.getSize() == 4 );
  ufAssert(
    !cadd1.isMemoryAccess() && !cadd1.isMemoryStore() &&
    !cadd1.isMemoryLoad() && !cadd1.isMove() && !cadd1.isCall() &&
    !cadd1.isIndirectCall() && !cadd1.isReturn() && !cadd1.isJump() &&
    !cadd1.isConditionalJump() && !cadd1.isUnconditionalJump() &&
    !cadd1.isIndirectJump() && !cadd1.isAsmDataDirective() &&
    !cadd1.hasSideEffects() );

  WIR_Operation cadd2(
    TC131::OpCode::CADD, TC131::OperationFormat::DDDD,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ) );
  ufAssert( cadd2.getSize() == 4 );

  WIR_Operation cadd3(
    TC131::OpCode::CADD, TC131::OperationFormat::SDIC4_3,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( tricore.D15(), WIR_Usage::use ),
    new TC_Const4_Signed( -5 ) );
  ufAssert( cadd3.getSize() == 2 );

  WIR_Operation caddn1(
    TC131::OpCode::CADDN, TC131::OperationFormat::DDDC9_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new TC_Const9_Signed( -128 ) );
  ufAssert( caddn1.getSize() == 4 );
  ufAssert(
    !caddn1.isMemoryAccess() && !caddn1.isMemoryStore() &&
    !caddn1.isMemoryLoad() && !caddn1.isMove() && !caddn1.isCall() &&
    !caddn1.isIndirectCall() && !caddn1.isReturn() && !caddn1.isJump() &&
    !caddn1.isConditionalJump() && !caddn1.isUnconditionalJump() &&
    !caddn1.isIndirectJump() && !caddn1.isAsmDataDirective() &&
    !caddn1.hasSideEffects() );

  WIR_Operation caddn2(
    TC131::OpCode::CADDN, TC131::OperationFormat::DDDD,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ) );
  ufAssert( caddn2.getSize() == 4 );

  WIR_Operation caddn3(
    TC131::OpCode::CADDN, TC131::OperationFormat::SDIC4_3,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( tricore.D15(), WIR_Usage::use ),
    new TC_Const4_Signed( -5 ) );
  ufAssert( caddn3.getSize() == 2 );

  WIR_Operation call1(
    TC131::OpCode::CALL, TC131::OperationFormat::L,
    new WIR_LabelParameter( f ) );
  ufAssert( call1.getSize() == 4 );
  ufAssert(
    call1.isMemoryAccess() && !call1.isMemoryStore() && !call1.isMemoryLoad() &&
    !call1.isMove() && call1.isCall() && !call1.isIndirectCall() &&
    !call1.isReturn() && !call1.isJump() && !call1.isConditionalJump() &&
    !call1.isUnconditionalJump() && !call1.isIndirectJump() &&
    !call1.isAsmDataDirective() && !call1.hasSideEffects() );

  WIR_Operation call2(
    TC131::OpCode::CALL, TC131::OperationFormat::SL,
    new WIR_LabelParameter( f ) );
  ufAssert( call2.getSize() == 2 );

  WIR_Operation calla1(
    TC131::OpCode::CALLA, TC131::OperationFormat::L,
    new WIR_LabelParameter( f ) );
  ufAssert( calla1.getSize() == 4 );
  ufAssert(
    calla1.isMemoryAccess() && !calla1.isMemoryStore() &&
    !calla1.isMemoryLoad() && !calla1.isMove() && calla1.isCall() &&
    !calla1.isIndirectCall() && !calla1.isReturn() && !calla1.isJump() &&
    !calla1.isConditionalJump() && !calla1.isUnconditionalJump() &&
    !calla1.isIndirectJump() && !calla1.isAsmDataDirective() &&
    !calla1.hasSideEffects() );

  WIR_Operation calli1(
    TC131::OpCode::CALLI, TC131::OperationFormat::A,
    new WIR_RegisterParameter( a1, WIR_Usage::use ) );
  ufAssert( calli1.getSize() == 4 );
  ufAssert(
    calli1.isMemoryAccess() && !calli1.isMemoryStore() &&
    !calli1.isMemoryLoad() && !calli1.isMove() && !calli1.isCall() &&
    calli1.isIndirectCall() && !calli1.isReturn() && !calli1.isJump() &&
    !calli1.isConditionalJump() && !calli1.isUnconditionalJump() &&
    !calli1.isIndirectJump() && !calli1.isAsmDataDirective() &&
    !calli1.hasSideEffects() );

  WIR_Operation clo1(
    TC131::OpCode::CLO, TC131::OperationFormat::DD,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ) );
  ufAssert( clo1.getSize() == 4 );
  ufAssert(
    !clo1.isMemoryAccess() && !clo1.isMemoryStore() && !clo1.isMemoryLoad() &&
    !clo1.isMove() && !clo1.isCall() && !clo1.isIndirectCall() &&
    !clo1.isReturn() && !clo1.isJump() && !clo1.isConditionalJump() &&
    !clo1.isUnconditionalJump() && !clo1.isIndirectJump() &&
    !clo1.isAsmDataDirective() && !clo1.hasSideEffects() );

  WIR_Operation clo_h1(
    TC131::OpCode::CLO_H, TC131::OperationFormat::DD,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ) );
  ufAssert( clo_h1.getSize() == 4 );
  ufAssert(
    !clo_h1.isMemoryAccess() && !clo_h1.isMemoryStore() &&
    !clo_h1.isMemoryLoad() && !clo_h1.isMove() && !clo_h1.isCall() &&
    !clo_h1.isIndirectCall() && !clo_h1.isReturn() && !clo_h1.isJump() &&
    !clo_h1.isConditionalJump() && !clo_h1.isUnconditionalJump() &&
    !clo_h1.isIndirectJump() && !clo_h1.isAsmDataDirective() &&
    !clo_h1.hasSideEffects() );

  WIR_Operation cls1(
    TC131::OpCode::CLS, TC131::OperationFormat::DD,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ) );
  ufAssert( cls1.getSize() == 4 );
  ufAssert(
    !cls1.isMemoryAccess() && !cls1.isMemoryStore() && !cls1.isMemoryLoad() &&
    !cls1.isMove() && !cls1.isCall() && !cls1.isIndirectCall() &&
    !cls1.isReturn() && !cls1.isJump() && !cls1.isConditionalJump() &&
    !cls1.isUnconditionalJump() && !cls1.isIndirectJump() &&
    !cls1.isAsmDataDirective() && !cls1.hasSideEffects() );

  WIR_Operation cls_h1(
    TC131::OpCode::CLS_H, TC131::OperationFormat::DD,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ) );
  ufAssert( clo_h1.getSize() == 4 );
  ufAssert(
    !cls_h1.isMemoryAccess() && !cls_h1.isMemoryStore() &&
    !cls_h1.isMemoryLoad() && !cls_h1.isMove() && !cls_h1.isCall() &&
    !cls_h1.isIndirectCall() && !cls_h1.isReturn() && !cls_h1.isJump() &&
    !cls_h1.isConditionalJump() && !cls_h1.isUnconditionalJump() &&
    !cls_h1.isIndirectJump() && !cls_h1.isAsmDataDirective() &&
    !cls_h1.hasSideEffects() );

  WIR_Operation clz1(
    TC131::OpCode::CLZ, TC131::OperationFormat::DD,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ) );
  ufAssert( clz1.getSize() == 4 );
  ufAssert(
    !clz1.isMemoryAccess() && !clz1.isMemoryStore() && !clz1.isMemoryLoad() &&
    !clz1.isMove() && !clz1.isCall() && !clz1.isIndirectCall() &&
    !clz1.isReturn() && !clz1.isJump() && !clz1.isConditionalJump() &&
    !clz1.isUnconditionalJump() && !clz1.isIndirectJump() &&
    !clz1.isAsmDataDirective() && !clz1.hasSideEffects() );

  WIR_Operation clz_h1(
    TC131::OpCode::CLZ_H, TC131::OperationFormat::DD,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ) );
  ufAssert( clz_h1.getSize() == 4 );
  ufAssert(
    !clz_h1.isMemoryAccess() && !clz_h1.isMemoryStore() &&
    !clz_h1.isMemoryLoad() && !clz_h1.isMove() && !clz_h1.isCall() &&
    !clz_h1.isIndirectCall() && !clz_h1.isReturn() && !clz_h1.isJump() &&
    !clz_h1.isConditionalJump() && !clz_h1.isUnconditionalJump() &&
    !clz_h1.isIndirectJump() && !clz_h1.isAsmDataDirective() &&
    !clz_h1.hasSideEffects() );

  WIR_Operation cmov1(
    TC131::OpCode::CMOV, TC131::OperationFormat::SDIC4_3,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( tricore.D15(), WIR_Usage::use ),
    new TC_Const4_Signed( -5 ) );
  ufAssert( cmov1.getSize() == 2 );
  ufAssert(
    !cmov1.isMemoryAccess() && !cmov1.isMemoryStore() &&
    !cmov1.isMemoryLoad() && !cmov1.isMove() && !cmov1.isCall() &&
    !cmov1.isIndirectCall() && !cmov1.isReturn() && !cmov1.isJump() &&
    !cmov1.isConditionalJump() && !cmov1.isUnconditionalJump() &&
    !cmov1.isIndirectJump() && !cmov1.isAsmDataDirective() &&
    !cmov1.hasSideEffects() );

  WIR_Operation cmov2(
    TC131::OpCode::CMOV, TC131::OperationFormat::SDID_2,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( tricore.D15(), WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ) );
  ufAssert( cmov2.getSize() == 2 );
  ufAssert(
    !cmov2.isMemoryAccess() && !cmov2.isMemoryStore() &&
    !cmov2.isMemoryLoad() && !cmov2.isMove() && !cmov2.isCall() &&
    !cmov2.isIndirectCall() && !cmov2.isReturn() && !cmov2.isJump() &&
    !cmov2.isConditionalJump() && !cmov2.isUnconditionalJump() &&
    !cmov2.isIndirectJump() && !cmov2.isAsmDataDirective() &&
    !cmov2.hasSideEffects() );

  WIR_Operation cmovn1(
    TC131::OpCode::CMOVN, TC131::OperationFormat::SDIC4_3,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( tricore.D15(), WIR_Usage::use ),
    new TC_Const4_Signed( -5 ) );
  ufAssert( cmovn1.getSize() == 2 );
  ufAssert(
    !cmovn1.isMemoryAccess() && !cmovn1.isMemoryStore() &&
    !cmovn1.isMemoryLoad() && !cmovn1.isMove() && !cmovn1.isCall() &&
    !cmovn1.isIndirectCall() && !cmovn1.isReturn() && !cmovn1.isJump() &&
    !cmovn1.isConditionalJump() && !cmovn1.isUnconditionalJump() &&
    !cmovn1.isIndirectJump() && !cmovn1.isAsmDataDirective() &&
    !cmovn1.hasSideEffects() );

  WIR_Operation cmovn2(
    TC131::OpCode::CMOVN, TC131::OperationFormat::SDID_2,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( tricore.D15(), WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ) );
  ufAssert( cmovn2.getSize() == 2 );
  ufAssert(
    !cmovn2.isMemoryAccess() && !cmovn2.isMemoryStore() &&
    !cmovn2.isMemoryLoad() && !cmovn2.isMove() && !cmovn2.isCall() &&
    !cmovn2.isIndirectCall() && !cmovn2.isReturn() &&
    !cmovn2.isJump() && !cmovn2.isConditionalJump() &&
    !cmovn2.isUnconditionalJump() && !cmovn2.isIndirectJump() &&
    !cmovn2.isAsmDataDirective() && !cmovn2.hasSideEffects() );

  WIR_Operation cmp_f1(
    TC131::OpCode::CMP_F, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( cmp_f1.getSize() == 4 );
  ufAssert(
    !cmp_f1.isMemoryAccess() && !cmp_f1.isMemoryStore() &&
    !cmp_f1.isMemoryLoad() && !cmp_f1.isMove() && !cmp_f1.isCall() &&
    !cmp_f1.isIndirectCall() && !cmp_f1.isReturn() && !cmp_f1.isJump() &&
    !cmp_f1.isConditionalJump() && !cmp_f1.isUnconditionalJump() &&
    !cmp_f1.isIndirectJump() && !cmp_f1.isAsmDataDirective() &&
    !cmp_f1.hasSideEffects() );

  WIR_Operation csub1(
    TC131::OpCode::CSUB, TC131::OperationFormat::DDDD,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ) );
  ufAssert( csub1.getSize() == 4 );
  ufAssert(
    !csub1.isMemoryAccess() && !csub1.isMemoryStore() &&
    !csub1.isMemoryLoad() && !csub1.isMove() && !csub1.isCall() &&
    !csub1.isIndirectCall() && !csub1.isReturn() && !csub1.isJump() &&
    !csub1.isConditionalJump() && !csub1.isUnconditionalJump() &&
    !csub1.isIndirectJump() && !csub1.isAsmDataDirective() &&
    !csub1.hasSideEffects() );

  WIR_Operation csubn1(
    TC131::OpCode::CSUBN, TC131::OperationFormat::DDDD,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ) );
  ufAssert( csubn1.getSize() == 4 );
  ufAssert(
    !csubn1.isMemoryAccess() && !csubn1.isMemoryStore() &&
    !csubn1.isMemoryLoad() && !csubn1.isMove() && !csubn1.isCall() &&
    !csubn1.isIndirectCall() && !csubn1.isReturn() && !csubn1.isJump() &&
    !csubn1.isConditionalJump() && !csubn1.isUnconditionalJump() &&
    !csubn1.isIndirectJump() && !csubn1.isAsmDataDirective() &&
    !csubn1.hasSideEffects() );

  WIR_Operation debug1(
    TC131::OpCode::DEBUG, TC131::OperationFormat::SYS );
  ufAssert( debug1.getSize() == 4 );
  ufAssert(
    !debug1.isMemoryAccess() && !debug1.isMemoryStore() &&
    !debug1.isMemoryLoad() && !debug1.isMove() && !debug1.isCall() &&
    !debug1.isIndirectCall() && !debug1.isReturn() && !debug1.isJump() &&
    !debug1.isConditionalJump() && !debug1.isUnconditionalJump() &&
    !debug1.isIndirectJump() && !debug1.isAsmDataDirective() &&
    debug1.hasSideEffects() );

  WIR_Operation debug2(
    TC131::OpCode::DEBUG, TC131::OperationFormat::S );
  ufAssert( debug2.getSize() == 2 );

  WIR_Operation dextr1(
    TC131::OpCode::DEXTR, TC131::OperationFormat::DDDC5,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new TC_Const5_Unsigned( 30 ) );
  ufAssert( dextr1.getSize() == 4 );
  ufAssert(
    !dextr1.isMemoryAccess() && !dextr1.isMemoryStore() &&
    !dextr1.isMemoryLoad() && !dextr1.isMove() && !dextr1.isCall() &&
    !dextr1.isIndirectCall() && !dextr1.isReturn() && !dextr1.isJump() &&
    !dextr1.isConditionalJump() && !dextr1.isUnconditionalJump() &&
    !dextr1.isIndirectJump() && !dextr1.isAsmDataDirective() &&
    !dextr1.hasSideEffects() );

  WIR_Operation dextr2(
    TC131::OpCode::DEXTR, TC131::OperationFormat::DDDD,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ) );
  ufAssert( dextr2.getSize() == 4 );

  WIR_Operation disable1(
    TC131::OpCode::DISABLE, TC131::OperationFormat::SYS );
  ufAssert( disable1.getSize() == 4 );
  ufAssert(
    !disable1.isMemoryAccess() && !disable1.isMemoryStore() &&
    !disable1.isMemoryLoad() && !disable1.isMove() && !disable1.isCall() &&
    !disable1.isIndirectCall() && !disable1.isReturn() && !disable1.isJump() &&
    !disable1.isConditionalJump() && !disable1.isUnconditionalJump() &&
    !disable1.isIndirectJump() && !disable1.isAsmDataDirective() &&
    disable1.hasSideEffects() );

  WIR_Operation div_f1(
    TC131::OpCode::DIV_F, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( div_f1.getSize() == 4 );
  ufAssert(
    !div_f1.isMemoryAccess() && !div_f1.isMemoryStore() &&
    !div_f1.isMemoryLoad() && !div_f1.isMove() && !div_f1.isCall() &&
    !div_f1.isIndirectCall() && !div_f1.isReturn() && !div_f1.isJump() &&
    !div_f1.isConditionalJump() && !div_f1.isUnconditionalJump() &&
    !div_f1.isIndirectJump() && !div_f1.isAsmDataDirective() &&
    !div_f1.hasSideEffects() );

  WIR_Operation dsync1(
    TC131::OpCode::DSYNC, TC131::OperationFormat::SYS );
  ufAssert( dsync1.getSize() == 4 );
  ufAssert(
    !dsync1.isMemoryAccess() && !dsync1.isMemoryStore() &&
    !dsync1.isMemoryLoad() && !dsync1.isMove() && !dsync1.isCall() &&
    !dsync1.isIndirectCall() && !dsync1.isReturn() && !dsync1.isJump() &&
    !dsync1.isConditionalJump() && !dsync1.isUnconditionalJump() &&
    !dsync1.isIndirectJump() && !dsync1.isAsmDataDirective() &&
    dsync1.hasSideEffects() );

  WIR_Operation dvadj1(
    TC131::OpCode::DVADJ, TC131::OperationFormat::EED,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ) );
  ufAssert( dvadj1.getSize() == 4 );
  ufAssert(
    !dvadj1.isMemoryAccess() && !dvadj1.isMemoryStore() &&
    !dvadj1.isMemoryLoad() && !dvadj1.isMove() && !dvadj1.isCall() &&
    !dvadj1.isIndirectCall() && !dvadj1.isReturn() && !dvadj1.isJump() &&
    !dvadj1.isConditionalJump() && !dvadj1.isUnconditionalJump() &&
    !dvadj1.isIndirectJump() && !dvadj1.isAsmDataDirective() &&
    !dvadj1.hasSideEffects() );

  WIR_Operation dvinit1(
    TC131::OpCode::DVINIT, TC131::OperationFormat::EDD,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ) );
  ufAssert( dvinit1.getSize() == 4 );
  ufAssert(
    !dvinit1.isMemoryAccess() && !dvinit1.isMemoryStore() &&
    !dvinit1.isMemoryLoad() && !dvinit1.isMove() && !dvinit1.isCall() &&
    !dvinit1.isIndirectCall() && !dvinit1.isReturn() && !dvinit1.isJump() &&
    !dvinit1.isConditionalJump() && !dvinit1.isUnconditionalJump() &&
    !dvinit1.isIndirectJump() && !dvinit1.isAsmDataDirective() &&
    !dvinit1.hasSideEffects() );

  WIR_Operation dvinit_b1(
    TC131::OpCode::DVINIT_B, TC131::OperationFormat::EDD,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ) );
  ufAssert( dvinit_b1.getSize() == 4 );
  ufAssert(
    !dvinit_b1.isMemoryAccess() && !dvinit_b1.isMemoryStore() &&
    !dvinit_b1.isMemoryLoad() && !dvinit_b1.isMove() && !dvinit_b1.isCall() &&
    !dvinit_b1.isIndirectCall() && !dvinit_b1.isReturn() &&
    !dvinit_b1.isJump() && !dvinit_b1.isConditionalJump() &&
    !dvinit_b1.isUnconditionalJump() && !dvinit_b1.isIndirectJump() &&
    !dvinit_b1.isAsmDataDirective() && !dvinit_b1.hasSideEffects() );

  WIR_Operation dvinit_bu1(
    TC131::OpCode::DVINIT_BU, TC131::OperationFormat::EDD,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ) );
  ufAssert( dvinit_bu1.getSize() == 4 );
  ufAssert(
    !dvinit_bu1.isMemoryAccess() && !dvinit_bu1.isMemoryStore() &&
    !dvinit_bu1.isMemoryLoad() && !dvinit_bu1.isMove() &&
    !dvinit_bu1.isCall() && !dvinit_bu1.isIndirectCall() &&
    !dvinit_bu1.isReturn() && !dvinit_bu1.isJump() &&
    !dvinit_bu1.isConditionalJump() && !dvinit_bu1.isUnconditionalJump() &&
    !dvinit_bu1.isIndirectJump() && !dvinit_bu1.isAsmDataDirective() &&
    !dvinit_bu1.hasSideEffects() );

  WIR_Operation dvinit_h1(
    TC131::OpCode::DVINIT_H, TC131::OperationFormat::EDD,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ) );
  ufAssert( dvinit_h1.getSize() == 4 );
  ufAssert(
    !dvinit_h1.isMemoryAccess() && !dvinit_h1.isMemoryStore() &&
    !dvinit_h1.isMemoryLoad() && !dvinit_h1.isMove() && !dvinit_h1.isCall() &&
    !dvinit_h1.isIndirectCall() && !dvinit_h1.isReturn() &&
    !dvinit_h1.isJump() && !dvinit_h1.isConditionalJump() &&
    !dvinit_h1.isUnconditionalJump() && !dvinit_h1.isIndirectJump() &&
    !dvinit_h1.isAsmDataDirective() && !dvinit_h1.hasSideEffects() );

  WIR_Operation dvinit_hu1(
    TC131::OpCode::DVINIT_HU, TC131::OperationFormat::EDD,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ) );
  ufAssert( dvinit_hu1.getSize() == 4 );
  ufAssert(
    !dvinit_hu1.isMemoryAccess() && !dvinit_hu1.isMemoryStore() &&
    !dvinit_hu1.isMemoryLoad() && !dvinit_hu1.isMove() &&
    !dvinit_hu1.isCall() && !dvinit_hu1.isIndirectCall() &&
    !dvinit_hu1.isReturn() && !dvinit_hu1.isJump() &&
    !dvinit_hu1.isConditionalJump() && !dvinit_hu1.isUnconditionalJump() &&
    !dvinit_hu1.isIndirectJump() && !dvinit_hu1.isAsmDataDirective() &&
    !dvinit_hu1.hasSideEffects() );

  WIR_Operation dvinit_u1(
    TC131::OpCode::DVINIT_U, TC131::OperationFormat::EDD,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ) );
  ufAssert( dvinit_u1.getSize() == 4 );
  ufAssert(
    !dvinit_u1.isMemoryAccess() && !dvinit_u1.isMemoryStore() &&
    !dvinit_u1.isMemoryLoad() && !dvinit_u1.isMove() && !dvinit_u1.isCall() &&
    !dvinit_u1.isIndirectCall() && !dvinit_u1.isReturn() &&
    !dvinit_u1.isJump() && !dvinit_u1.isConditionalJump() &&
    !dvinit_u1.isUnconditionalJump() && !dvinit_u1.isIndirectJump() &&
    !dvinit_u1.isAsmDataDirective() && !dvinit_u1.hasSideEffects() );

  WIR_Operation dvstep1(
    TC131::OpCode::DVSTEP, TC131::OperationFormat::EED,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ) );
  ufAssert( dvstep1.getSize() == 4 );
  ufAssert(
    !dvstep1.isMemoryAccess() && !dvstep1.isMemoryStore() &&
    !dvstep1.isMemoryLoad() && !dvstep1.isMove() && !dvstep1.isCall() &&
    !dvstep1.isIndirectCall() && !dvstep1.isReturn() && !dvstep1.isJump() &&
    !dvstep1.isConditionalJump() && !dvstep1.isUnconditionalJump() &&
    !dvstep1.isIndirectJump() && !dvstep1.isAsmDataDirective() &&
    !dvstep1.hasSideEffects() );

  WIR_Operation dvstep_u1(
    TC131::OpCode::DVSTEP_U, TC131::OperationFormat::EED,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ) );
  ufAssert( dvstep_u1.getSize() == 4 );
  ufAssert(
    !dvstep_u1.isMemoryAccess() && !dvstep_u1.isMemoryStore() &&
    !dvstep_u1.isMemoryLoad() && !dvstep_u1.isMove() && !dvstep_u1.isCall() &&
    !dvstep_u1.isIndirectCall() && !dvstep_u1.isReturn() &&
    !dvstep_u1.isJump() && !dvstep_u1.isConditionalJump() &&
    !dvstep_u1.isUnconditionalJump() && !dvstep_u1.isIndirectJump() &&
    !dvstep_u1.isAsmDataDirective() && !dvstep_u1.hasSideEffects() );

  WIR_Operation enable1(
    TC131::OpCode::ENABLE, TC131::OperationFormat::SYS );
  ufAssert( enable1.getSize() == 4 );
  ufAssert(
    !enable1.isMemoryAccess() && !enable1.isMemoryStore() &&
    !enable1.isMemoryLoad() && !enable1.isMove() && !enable1.isCall() &&
    !enable1.isIndirectCall() && !enable1.isReturn() && !enable1.isJump() &&
    !enable1.isConditionalJump() && !enable1.isUnconditionalJump() &&
    !enable1.isIndirectJump() && !enable1.isAsmDataDirective() &&
    enable1.hasSideEffects() );

  WIR_Operation eq1(
    TC131::OpCode::EQ, TC131::OperationFormat::DDC9_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const9_Signed( -128 ) );
  ufAssert( eq1.getSize() == 4 );
  ufAssert(
    !eq1.isMemoryAccess() && !eq1.isMemoryStore() && !eq1.isMemoryLoad() &&
    !eq1.isMove() && !eq1.isCall() && !eq1.isIndirectCall() &&
    !eq1.isReturn() && !eq1.isJump() && !eq1.isConditionalJump() &&
    !eq1.isUnconditionalJump() && !eq1.isIndirectJump() &&
    !eq1.isAsmDataDirective() && !eq1.hasSideEffects() );

  WIR_Operation eq2(
    TC131::OpCode::EQ, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( eq2.getSize() == 4 );

  WIR_Operation eq3(
    TC131::OpCode::EQ, TC131::OperationFormat::SIDC4,
    new WIR_RegisterParameter( tricore.D15(), WIR_Usage::def ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new TC_Const4_Signed( -5 ) );
  ufAssert( eq3.getSize() == 2 );

  WIR_Operation eq4(
    TC131::OpCode::EQ, TC131::OperationFormat::SIDD,
    new WIR_RegisterParameter( tricore.D15(), WIR_Usage::def ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ) );
  ufAssert( eq4.getSize() == 2 );

  WIR_Operation eq_a1(
    TC131::OpCode::EQ_A, TC131::OperationFormat::DAA,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( a1, WIR_Usage::use ),
    new WIR_RegisterParameter( a2, WIR_Usage::use ) );
  ufAssert( eq_a1.getSize() == 4 );
  ufAssert(
    !eq_a1.isMemoryAccess() && !eq_a1.isMemoryStore() &&
    !eq_a1.isMemoryLoad() && !eq_a1.isMove() && !eq_a1.isCall() &&
    !eq_a1.isIndirectCall() && !eq_a1.isReturn() && !eq_a1.isJump() &&
    !eq_a1.isConditionalJump() && !eq_a1.isUnconditionalJump() &&
    !eq_a1.isIndirectJump() && !eq_a1.isAsmDataDirective() &&
    !eq_a1.hasSideEffects() );

  WIR_Operation eq_b1(
    TC131::OpCode::EQ_B, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( eq_b1.getSize() == 4 );
  ufAssert(
    !eq_b1.isMemoryAccess() && !eq_b1.isMemoryStore() &&
    !eq_b1.isMemoryLoad() && !eq_b1.isMove() && !eq_b1.isCall() &&
    !eq_b1.isIndirectCall() && !eq_b1.isReturn() && !eq_b1.isJump() &&
    !eq_b1.isConditionalJump() && !eq_b1.isUnconditionalJump() &&
    !eq_b1.isIndirectJump() && !eq_b1.isAsmDataDirective() &&
    !eq_b1.hasSideEffects() );

  WIR_Operation eq_h1(
    TC131::OpCode::EQ_H, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( eq_h1.getSize() == 4 );
  ufAssert(
    !eq_h1.isMemoryAccess() && !eq_h1.isMemoryStore() &&
    !eq_h1.isMemoryLoad() && !eq_h1.isMove() && !eq_h1.isCall() &&
    !eq_h1.isIndirectCall() && !eq_h1.isReturn() && !eq_h1.isJump() &&
    !eq_h1.isConditionalJump() && !eq_h1.isUnconditionalJump() &&
    !eq_h1.isIndirectJump() && !eq_h1.isAsmDataDirective() &&
    !eq_h1.hasSideEffects() );

  WIR_Operation eq_w1(
    TC131::OpCode::EQ_W, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( eq_w1.getSize() == 4 );
  ufAssert(
    !eq_w1.isMemoryAccess() && !eq_w1.isMemoryStore() &&
    !eq_w1.isMemoryLoad() && !eq_w1.isMove() && !eq_w1.isCall() &&
    !eq_w1.isIndirectCall() && !eq_w1.isReturn() && !eq_w1.isJump() &&
    !eq_w1.isConditionalJump() && !eq_w1.isUnconditionalJump() &&
    !eq_w1.isIndirectJump() && !eq_w1.isAsmDataDirective() &&
    !eq_w1.hasSideEffects() );

  WIR_Operation eqany_b1(
    TC131::OpCode::EQANY_B, TC131::OperationFormat::DDC9_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const9_Signed( -128 ) );
  ufAssert( eqany_b1.getSize() == 4 );
  ufAssert(
    !eqany_b1.isMemoryAccess() && !eqany_b1.isMemoryStore() &&
    !eqany_b1.isMemoryLoad() && !eqany_b1.isMove() && !eqany_b1.isCall() &&
    !eqany_b1.isIndirectCall() && !eqany_b1.isReturn() && !eqany_b1.isJump() &&
    !eqany_b1.isConditionalJump() && !eqany_b1.isUnconditionalJump() &&
    !eqany_b1.isIndirectJump() && !eqany_b1.isAsmDataDirective() &&
    !eqany_b1.hasSideEffects() );

  WIR_Operation eqany_b2(
    TC131::OpCode::EQANY_B, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( eqany_b2.getSize() == 4 );

  WIR_Operation eqany_h1(
    TC131::OpCode::EQANY_H, TC131::OperationFormat::DDC9_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const9_Signed( -128 ) );
  ufAssert( eqany_h1.getSize() == 4 );
  ufAssert(
    !eqany_h1.isMemoryAccess() && !eqany_h1.isMemoryStore() &&
    !eqany_h1.isMemoryLoad() && !eqany_h1.isMove() && !eqany_h1.isCall() &&
    !eqany_h1.isIndirectCall() && !eqany_h1.isReturn() && !eqany_h1.isJump() &&
    !eqany_h1.isConditionalJump() && !eqany_h1.isUnconditionalJump() &&
    !eqany_h1.isIndirectJump() && !eqany_h1.isAsmDataDirective() &&
    !eqany_h1.hasSideEffects() );

  WIR_Operation eqany_h2(
    TC131::OpCode::EQANY_H, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( eqany_h2.getSize() == 4 );

  WIR_Operation eqz_a1(
    TC131::OpCode::EQZ_A, TC131::OperationFormat::DA,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( a1, WIR_Usage::use ) );
  ufAssert( eqz_a1.getSize() == 4 );
  ufAssert(
    !eqz_a1.isMemoryAccess() && !eqz_a1.isMemoryStore() &&
    !eqz_a1.isMemoryLoad() && !eqz_a1.isMove() && !eqz_a1.isCall() &&
    !eqz_a1.isIndirectCall() && !eqz_a1.isReturn() && !eqz_a1.isJump() &&
    !eqz_a1.isConditionalJump() && !eqz_a1.isUnconditionalJump() &&
    !eqz_a1.isIndirectJump() && !eqz_a1.isAsmDataDirective() &&
    !eqz_a1.hasSideEffects() );

  WIR_Operation extr1(
    TC131::OpCode::EXTR, TC131::OperationFormat::DDC5C5,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const5_Unsigned( 13 ),
    new TC_Const5_Unsigned( 9 ) );
  ufAssert( extr1.getSize() == 4 );
  ufAssert(
    !extr1.isMemoryAccess() && !extr1.isMemoryStore() &&
    !extr1.isMemoryLoad() && !extr1.isMove() && !extr1.isCall() &&
    !extr1.isIndirectCall() && !extr1.isReturn() && !extr1.isJump() &&
    !extr1.isConditionalJump() && !extr1.isUnconditionalJump() &&
    !extr1.isIndirectJump() && !extr1.isAsmDataDirective() &&
    !extr1.hasSideEffects() );

  WIR_Operation extr2(
    TC131::OpCode::EXTR, TC131::OperationFormat::DDE,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( e1, WIR_Usage::use ) );
  ufAssert( extr2.getSize() == 4 );

  WIR_Operation extr3(
    TC131::OpCode::EXTR, TC131::OperationFormat::DDDC5,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new TC_Const5_Unsigned( 9 ) );
  ufAssert( extr3.getSize() == 4 );

  WIR_Operation extr_u1(
    TC131::OpCode::EXTR_U, TC131::OperationFormat::DDC5C5,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const5_Unsigned( 13 ),
    new TC_Const5_Unsigned( 9 ) );
  ufAssert( extr_u1.getSize() == 4 );
  ufAssert(
    !extr_u1.isMemoryAccess() && !extr_u1.isMemoryStore() &&
    !extr_u1.isMemoryLoad() && !extr_u1.isMove() && !extr_u1.isCall() &&
    !extr_u1.isIndirectCall() && !extr_u1.isReturn() && !extr_u1.isJump() &&
    !extr_u1.isConditionalJump() && !extr_u1.isUnconditionalJump() &&
    !extr_u1.isIndirectJump() && !extr_u1.isAsmDataDirective() &&
    !extr_u1.hasSideEffects() );

  WIR_Operation extr_u2(
    TC131::OpCode::EXTR_U, TC131::OperationFormat::DDE,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( e1, WIR_Usage::use ) );
  ufAssert( extr_u2.getSize() == 4 );

  WIR_Operation extr_u3(
    TC131::OpCode::EXTR_U, TC131::OperationFormat::DDDC5,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new TC_Const5_Unsigned( 9 ) );
  ufAssert( extr_u3.getSize() == 4 );

  WIR_Operation ftoi1(
    TC131::OpCode::FTOI, TC131::OperationFormat::DD,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ) );
  ufAssert( ftoi1.getSize() == 4 );
  ufAssert(
    !ftoi1.isMemoryAccess() && !ftoi1.isMemoryStore() &&
    !ftoi1.isMemoryLoad() && !ftoi1.isMove() && !ftoi1.isCall() &&
    !ftoi1.isIndirectCall() && !ftoi1.isReturn() && !ftoi1.isJump() &&
    !ftoi1.isConditionalJump() && !ftoi1.isUnconditionalJump() &&
    !ftoi1.isIndirectJump() && !ftoi1.isAsmDataDirective() &&
    !ftoi1.hasSideEffects() );

  WIR_Operation ftoiz1(
    TC131::OpCode::FTOIZ, TC131::OperationFormat::DD,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ) );
  ufAssert( ftoiz1.getSize() == 4 );
  ufAssert(
    !ftoiz1.isMemoryAccess() && !ftoiz1.isMemoryStore() &&
    !ftoiz1.isMemoryLoad() && !ftoiz1.isMove() && !ftoiz1.isCall() &&
    !ftoiz1.isIndirectCall() && !ftoiz1.isReturn() && !ftoiz1.isJump() &&
    !ftoiz1.isConditionalJump() && !ftoiz1.isUnconditionalJump() &&
    !ftoiz1.isIndirectJump() && !ftoiz1.isAsmDataDirective() &&
    !ftoiz1.hasSideEffects() );

  WIR_Operation ftoq311(
    TC131::OpCode::FTOQ31, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( ftoq311.getSize() == 4 );
  ufAssert(
    !ftoq311.isMemoryAccess() && !ftoq311.isMemoryStore() &&
    !ftoq311.isMemoryLoad() && !ftoq311.isMove() && !ftoq311.isCall() &&
    !ftoq311.isIndirectCall() && !ftoq311.isReturn() && !ftoq311.isJump() &&
    !ftoq311.isConditionalJump() && !ftoq311.isUnconditionalJump() &&
    !ftoq311.isIndirectJump() && !ftoq311.isAsmDataDirective() &&
    !ftoq311.hasSideEffects() );

  WIR_Operation ftoq31z1(
    TC131::OpCode::FTOQ31Z, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( ftoq31z1.getSize() == 4 );
  ufAssert(
    !ftoq31z1.isMemoryAccess() && !ftoq31z1.isMemoryStore() &&
    !ftoq31z1.isMemoryLoad() && !ftoq31z1.isMove() && !ftoq31z1.isCall() &&
    !ftoq31z1.isIndirectCall() && !ftoq31z1.isReturn() && !ftoq31z1.isJump() &&
    !ftoq31z1.isConditionalJump() && !ftoq31z1.isUnconditionalJump() &&
    !ftoq31z1.isIndirectJump() && !ftoq31z1.isAsmDataDirective() &&
    !ftoq31z1.hasSideEffects() );

  WIR_Operation ftou1(
    TC131::OpCode::FTOU, TC131::OperationFormat::DD,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ) );
  ufAssert( ftou1.getSize() == 4 );
  ufAssert(
    !ftou1.isMemoryAccess() && !ftou1.isMemoryStore() &&
    !ftou1.isMemoryLoad() && !ftou1.isMove() && !ftou1.isCall() &&
    !ftou1.isIndirectCall() && !ftou1.isReturn() && !ftou1.isJump() &&
    !ftou1.isConditionalJump() && !ftou1.isUnconditionalJump() &&
    !ftou1.isIndirectJump() && !ftou1.isAsmDataDirective() &&
    !ftou1.hasSideEffects() );

  WIR_Operation ftouz1(
    TC131::OpCode::FTOUZ, TC131::OperationFormat::DD,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ) );
  ufAssert( ftouz1.getSize() == 4 );
  ufAssert(
    !ftouz1.isMemoryAccess() && !ftouz1.isMemoryStore() &&
    !ftouz1.isMemoryLoad() && !ftouz1.isMove() && !ftouz1.isCall() &&
    !ftouz1.isIndirectCall() && !ftouz1.isReturn() && !ftouz1.isJump() &&
    !ftouz1.isConditionalJump() && !ftouz1.isUnconditionalJump() &&
    !ftouz1.isIndirectJump() && !ftouz1.isAsmDataDirective() &&
    !ftouz1.hasSideEffects() );

  WIR_Operation ge1(
    TC131::OpCode::GE, TC131::OperationFormat::DDC9_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const9_Signed( -128 ) );
  ufAssert( ge1.getSize() == 4 );
  ufAssert(
    !ge1.isMemoryAccess() && !ge1.isMemoryStore() && !ge1.isMemoryLoad() &&
    !ge1.isMove() && !ge1.isCall() && !ge1.isIndirectCall() &&
    !ge1.isReturn() && !ge1.isJump() && !ge1.isConditionalJump() &&
    !ge1.isUnconditionalJump() && !ge1.isIndirectJump() &&
    !ge1.isAsmDataDirective() && !ge1.hasSideEffects() );

  WIR_Operation ge2(
    TC131::OpCode::GE, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( ge2.getSize() == 4 );

  WIR_Operation ge_a1(
    TC131::OpCode::GE_A, TC131::OperationFormat::DAA,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( a1, WIR_Usage::use ),
    new WIR_RegisterParameter( a2, WIR_Usage::use ) );
  ufAssert( ge_a1.getSize() == 4 );
  ufAssert(
    !ge_a1.isMemoryAccess() && !ge_a1.isMemoryStore() &&
    !ge_a1.isMemoryLoad() && !ge_a1.isMove() && !ge_a1.isCall() &&
    !ge_a1.isIndirectCall() && !ge_a1.isReturn() && !ge_a1.isJump() &&
    !ge_a1.isConditionalJump() && !ge_a1.isUnconditionalJump() &&
    !ge_a1.isIndirectJump() && !ge_a1.isAsmDataDirective() &&
    !ge_a1.hasSideEffects() );

  WIR_Operation ge_u1(
    TC131::OpCode::GE_U, TC131::OperationFormat::DDC9_2,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const9_Unsigned( 128 ) );
  ufAssert( ge_u1.getSize() == 4 );
  ufAssert(
    !ge_u1.isMemoryAccess() && !ge_u1.isMemoryStore() &&
    !ge_u1.isMemoryLoad() && !ge_u1.isMove() && !ge_u1.isCall() &&
    !ge_u1.isIndirectCall() && !ge_u1.isReturn() && !ge_u1.isJump() &&
    !ge_u1.isConditionalJump() && !ge_u1.isUnconditionalJump() &&
    !ge_u1.isIndirectJump() && !ge_u1.isAsmDataDirective() &&
    !ge_u1.hasSideEffects() );

  WIR_Operation ge_u2(
    TC131::OpCode::GE_U, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( ge_u2.getSize() == 4 );

  WIR_Operation imask1(
    TC131::OpCode::IMASK, TC131::OperationFormat::EC4C5C5,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new TC_Const4_Unsigned( 7 ),
    new TC_Const5_Unsigned( 13 ),
    new TC_Const5_Unsigned( 9 ) );
  ufAssert( imask1.getSize() == 4 );
  ufAssert(
    !imask1.isMemoryAccess() && !imask1.isMemoryStore() &&
    !imask1.isMemoryLoad() && !imask1.isMove() && !imask1.isCall() &&
    !imask1.isIndirectCall() && !imask1.isReturn() && !imask1.isJump() &&
    !imask1.isConditionalJump() && !imask1.isUnconditionalJump() &&
    !imask1.isIndirectJump() && !imask1.isAsmDataDirective() &&
    !imask1.hasSideEffects() );

  WIR_Operation imask2(
    TC131::OpCode::IMASK, TC131::OperationFormat::EC4DC5,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new TC_Const4_Unsigned( 7 ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new TC_Const5_Unsigned( 9 ) );
  ufAssert( imask2.getSize() == 4 );

  WIR_Operation imask3(
    TC131::OpCode::IMASK, TC131::OperationFormat::EDC5C5,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new TC_Const5_Unsigned( 7 ),
    new TC_Const5_Unsigned( 9 ) );
  ufAssert( imask3.getSize() == 4 );

  WIR_Operation imask4(
    TC131::OpCode::IMASK, TC131::OperationFormat::EDDC5,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const5_Unsigned( 9 ) );
  ufAssert( imask4.getSize() == 4 );

  WIR_Operation ins_t1(
    TC131::OpCode::INS_T, TC131::OperationFormat::DDC5DC5_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const5_Unsigned( 7 ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new TC_Const5_Unsigned( 9 ) );
  ufAssert( ins_t1.getSize() == 4 );
  ufAssert(
    !ins_t1.isMemoryAccess() && !ins_t1.isMemoryStore() &&
    !ins_t1.isMemoryLoad() && !ins_t1.isMove() && !ins_t1.isCall() &&
    !ins_t1.isIndirectCall() && !ins_t1.isReturn() && !ins_t1.isJump() &&
    !ins_t1.isConditionalJump() && !ins_t1.isUnconditionalJump() &&
    !ins_t1.isIndirectJump() && !ins_t1.isAsmDataDirective() &&
    !ins_t1.hasSideEffects() );

  WIR_Operation insert1(
    TC131::OpCode::INSERT, TC131::OperationFormat::DDC4C5C5,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const4_Unsigned( 7 ),
    new TC_Const5_Unsigned( 13 ),
    new TC_Const5_Unsigned( 9 ) );
  ufAssert( insert1.getSize() == 4 );
  ufAssert(
    !insert1.isMemoryAccess() && !insert1.isMemoryStore() &&
    !insert1.isMemoryLoad() && !insert1.isMove() && !insert1.isCall() &&
    !insert1.isIndirectCall() && !insert1.isReturn() && !insert1.isJump() &&
    !insert1.isConditionalJump() && !insert1.isUnconditionalJump() &&
    !insert1.isIndirectJump() && !insert1.isAsmDataDirective() &&
    !insert1.hasSideEffects() );

  WIR_Operation insert2(
    TC131::OpCode::INSERT, TC131::OperationFormat::DDC4E,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const4_Unsigned( 7 ),
    new WIR_RegisterParameter( e1, WIR_Usage::use ) );
  ufAssert( insert2.getSize() == 4 );

  WIR_Operation insert3(
    TC131::OpCode::INSERT, TC131::OperationFormat::DDC4DC5,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const4_Unsigned( 7 ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new TC_Const5_Unsigned( 9 ) );
  ufAssert( insert3.getSize() == 4 );

  WIR_Operation insert4(
    TC131::OpCode::INSERT, TC131::OperationFormat::DDDC5C5,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new TC_Const5_Unsigned( 13 ),
    new TC_Const5_Unsigned( 9 ) );
  ufAssert( insert4.getSize() == 4 );

  WIR_Operation insert5(
    TC131::OpCode::INSERT, TC131::OperationFormat::DDDE,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( e1, WIR_Usage::use ) );
  ufAssert( insert5.getSize() == 4 );

  WIR_Operation insert6(
    TC131::OpCode::INSERT, TC131::OperationFormat::DDDDC5,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ),
    new TC_Const5_Unsigned( 9 ) );
  ufAssert( insert6.getSize() == 4 );

  WIR_Operation insn_t1(
    TC131::OpCode::INSN_T, TC131::OperationFormat::DDC5DC5_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const5_Unsigned( 7 ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new TC_Const5_Unsigned( 9 ) );
  ufAssert( insn_t1.getSize() == 4 );
  ufAssert(
    !insn_t1.isMemoryAccess() && !insn_t1.isMemoryStore() &&
    !insn_t1.isMemoryLoad() && !insn_t1.isMove() && !insn_t1.isCall() &&
    !insn_t1.isIndirectCall() && !insn_t1.isReturn() && !insn_t1.isJump() &&
    !insn_t1.isConditionalJump() && !insn_t1.isUnconditionalJump() &&
    !insn_t1.isIndirectJump() && !insn_t1.isAsmDataDirective() &&
    !insn_t1.hasSideEffects() );

  WIR_Operation isync1(
    TC131::OpCode::ISYNC, TC131::OperationFormat::SYS );
  ufAssert( isync1.getSize() == 4 );
  ufAssert(
    !isync1.isMemoryAccess() && !isync1.isMemoryStore() &&
    !isync1.isMemoryLoad() && !isync1.isMove() && !isync1.isCall() &&
    !isync1.isIndirectCall() && !isync1.isReturn() && !isync1.isJump() &&
    !isync1.isConditionalJump() && !isync1.isUnconditionalJump() &&
    !isync1.isIndirectJump() && !isync1.isAsmDataDirective() &&
    isync1.hasSideEffects() );

  WIR_Operation itof1(
    TC131::OpCode::ITOF, TC131::OperationFormat::DD,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ) );
  ufAssert( itof1.getSize() == 4 );
  ufAssert(
    !itof1.isMemoryAccess() && !itof1.isMemoryStore() &&
    !itof1.isMemoryLoad() && !itof1.isMove() && !itof1.isCall() &&
    !itof1.isIndirectCall() && !itof1.isReturn() && !itof1.isJump() &&
    !itof1.isConditionalJump() && !itof1.isUnconditionalJump() &&
    !itof1.isIndirectJump() && !itof1.isAsmDataDirective() &&
    !itof1.hasSideEffects() );

  WIR_Operation ixmax1(
    TC131::OpCode::IXMAX, TC131::OperationFormat::EED,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ) );
  ufAssert( ixmax1.getSize() == 4 );
  ufAssert(
    !ixmax1.isMemoryAccess() && !ixmax1.isMemoryStore() &&
    !ixmax1.isMemoryLoad() && !ixmax1.isMove() && !ixmax1.isCall() &&
    !ixmax1.isIndirectCall() && !ixmax1.isReturn() && !ixmax1.isJump() &&
    !ixmax1.isConditionalJump() && !ixmax1.isUnconditionalJump() &&
    !ixmax1.isIndirectJump() && !ixmax1.isAsmDataDirective() &&
    !ixmax1.hasSideEffects() );

  WIR_Operation ixmax_u1(
    TC131::OpCode::IXMAX_U, TC131::OperationFormat::EED,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ) );
  ufAssert( ixmax_u1.getSize() == 4 );
  ufAssert(
    !ixmax_u1.isMemoryAccess() && !ixmax_u1.isMemoryStore() &&
    !ixmax_u1.isMemoryLoad() && !ixmax_u1.isMove() && !ixmax_u1.isCall() &&
    !ixmax_u1.isIndirectCall() && !ixmax_u1.isReturn() && !ixmax_u1.isJump() &&
    !ixmax_u1.isConditionalJump() && !ixmax_u1.isUnconditionalJump() &&
    !ixmax_u1.isIndirectJump() && !ixmax_u1.isAsmDataDirective() &&
    !ixmax_u1.hasSideEffects() );

  WIR_Operation ixmin1(
    TC131::OpCode::IXMIN, TC131::OperationFormat::EED,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ) );
  ufAssert( ixmin1.getSize() == 4 );
  ufAssert(
    !ixmin1.isMemoryAccess() && !ixmin1.isMemoryStore() &&
    !ixmin1.isMemoryLoad() && !ixmin1.isMove() && !ixmin1.isCall() &&
    !ixmin1.isIndirectCall() && !ixmin1.isReturn() && !ixmin1.isJump() &&
    !ixmin1.isConditionalJump() && !ixmin1.isUnconditionalJump() &&
    !ixmin1.isIndirectJump() && !ixmin1.isAsmDataDirective() &&
    !ixmin1.hasSideEffects() );

  WIR_Operation ixmin_u1(
    TC131::OpCode::IXMIN_U, TC131::OperationFormat::EED,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ) );
  ufAssert( ixmin_u1.getSize() == 4 );
  ufAssert(
    !ixmin_u1.isMemoryAccess() && !ixmin_u1.isMemoryStore() &&
    !ixmin_u1.isMemoryLoad() && !ixmin_u1.isMove() && !ixmin_u1.isCall() &&
    !ixmin_u1.isIndirectCall() && !ixmin_u1.isReturn() && !ixmin_u1.isJump() &&
    !ixmin_u1.isConditionalJump() && !ixmin_u1.isUnconditionalJump() &&
    !ixmin_u1.isIndirectJump() && !ixmin_u1.isAsmDataDirective() &&
    !ixmin_u1.hasSideEffects() );

  WIR_Operation j1(
    TC131::OpCode::J, TC131::OperationFormat::L,
    new WIR_LabelParameter( b ) );
  ufAssert( j1.getSize() == 4 );
  ufAssert(
    !j1.isMemoryAccess() && !j1.isMemoryStore() && !j1.isMemoryLoad() &&
    !j1.isMove() && !j1.isCall() && !j1.isIndirectCall() &&
    !j1.isReturn() && j1.isJump() && !j1.isConditionalJump() &&
    j1.isUnconditionalJump() && !j1.isIndirectJump() &&
    !j1.isAsmDataDirective() && !j1.hasSideEffects() );

  WIR_Operation j2(
    TC131::OpCode::J, TC131::OperationFormat::SL,
    new WIR_LabelParameter( b ) );
  ufAssert( j2.getSize() == 2 );

  WIR_Operation ja1(
    TC131::OpCode::JA, TC131::OperationFormat::L,
    new WIR_LabelParameter( b ) );
  ufAssert( ja1.getSize() == 4 );
  ufAssert(
    !ja1.isMemoryAccess() && !ja1.isMemoryStore() && !ja1.isMemoryLoad() &&
    !ja1.isMove() && !ja1.isCall() && !ja1.isIndirectCall() &&
    !ja1.isReturn() && ja1.isJump() && !ja1.isConditionalJump() &&
    ja1.isUnconditionalJump() && !ja1.isIndirectJump() &&
    !ja1.isAsmDataDirective() && !ja1.hasSideEffects() );

  WIR_Operation jeq1(
    TC131::OpCode::JEQ, TC131::OperationFormat::DC4L_1,
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new TC_Const4_Signed( 3 ),
    new WIR_LabelParameter( b ) );
  ufAssert( jeq1.getSize() == 4 );
  ufAssert(
    !jeq1.isMemoryAccess() && !jeq1.isMemoryStore() && !jeq1.isMemoryLoad() &&
    !jeq1.isMove() && !jeq1.isCall() && !jeq1.isIndirectCall() &&
    !jeq1.isReturn() && jeq1.isJump() && jeq1.isConditionalJump() &&
    !jeq1.isUnconditionalJump() && !jeq1.isIndirectJump() &&
    !jeq1.isAsmDataDirective() && !jeq1.hasSideEffects() );

  WIR_Operation jeq2(
    TC131::OpCode::JEQ, TC131::OperationFormat::DDL_1,
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_LabelParameter( b ) );
  ufAssert( jeq2.getSize() == 4 );

  WIR_Operation jeq3(
    TC131::OpCode::JEQ, TC131::OperationFormat::SIC4L,
    new WIR_RegisterParameter( tricore.D15(), WIR_Usage::use ),
    new TC_Const4_Signed( 3 ),
    new WIR_LabelParameter( b ) );
  ufAssert( jeq3.getSize() == 2 );

  WIR_Operation jeq4(
    TC131::OpCode::JEQ, TC131::OperationFormat::SIDL,
    new WIR_RegisterParameter( tricore.D15(), WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_LabelParameter( b ) );
  ufAssert( jeq4.getSize() == 2 );

  WIR_Operation jeq_a1(
    TC131::OpCode::JEQ_A, TC131::OperationFormat::AAL,
    new WIR_RegisterParameter( a1, WIR_Usage::use ),
    new WIR_RegisterParameter( a2, WIR_Usage::use ),
    new WIR_LabelParameter( b ) );
  ufAssert( jeq_a1.getSize() == 4 );
  ufAssert(
    !jeq_a1.isMemoryAccess() && !jeq_a1.isMemoryStore() &&
    !jeq_a1.isMemoryLoad() && !jeq_a1.isMove() && !jeq_a1.isCall() &&
    !jeq_a1.isIndirectCall() && !jeq_a1.isReturn() && jeq_a1.isJump() &&
    jeq_a1.isConditionalJump() && !jeq_a1.isUnconditionalJump() &&
    !jeq_a1.isIndirectJump() && !jeq_a1.isAsmDataDirective() &&
    !jeq_a1.hasSideEffects() );

  WIR_Operation jge1(
    TC131::OpCode::JGE, TC131::OperationFormat::DC4L_1,
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new TC_Const4_Signed( 3 ),
    new WIR_LabelParameter( b ) );
  ufAssert( jge1.getSize() == 4 );
  ufAssert(
    !jge1.isMemoryAccess() && !jge1.isMemoryStore() && !jge1.isMemoryLoad() &&
    !jge1.isMove() && !jge1.isCall() && !jge1.isIndirectCall() &&
    !jge1.isReturn() && jge1.isJump() && jge1.isConditionalJump() &&
    !jge1.isUnconditionalJump() && !jge1.isIndirectJump() &&
    !jge1.isAsmDataDirective() && !jge1.hasSideEffects() );

  WIR_Operation jge2(
    TC131::OpCode::JGE, TC131::OperationFormat::DDL_1,
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_LabelParameter( b ) );
  ufAssert( jge2.getSize() == 4 );

  WIR_Operation jge_u1(
    TC131::OpCode::JGE_U, TC131::OperationFormat::DC4L_2,
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new TC_Const4_Unsigned( 7 ),
    new WIR_LabelParameter( b ) );
  ufAssert( jge_u1.getSize() == 4 );
  ufAssert(
    !jge_u1.isMemoryAccess() && !jge_u1.isMemoryStore() &&
    !jge_u1.isMemoryLoad() && !jge_u1.isMove() && !jge_u1.isCall() &&
    !jge_u1.isIndirectCall() && !jge_u1.isReturn() && jge_u1.isJump() &&
    jge_u1.isConditionalJump() && !jge_u1.isUnconditionalJump() &&
    !jge_u1.isIndirectJump() && !jge_u1.isAsmDataDirective() &&
    !jge_u1.hasSideEffects() );

  WIR_Operation jge_u2(
    TC131::OpCode::JGE_U, TC131::OperationFormat::DDL_1,
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_LabelParameter( b ) );
  ufAssert( jge_u2.getSize() == 4 );

  WIR_Operation jgez1(
    TC131::OpCode::JGEZ, TC131::OperationFormat::SDL,
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_LabelParameter( b ) );
  ufAssert( jgez1.getSize() == 2 );
  ufAssert(
    !jgez1.isMemoryAccess() && !jgez1.isMemoryStore() &&
    !jgez1.isMemoryLoad() && !jgez1.isMove() && !jgez1.isCall() &&
    !jgez1.isIndirectCall() && !jgez1.isReturn() && jgez1.isJump() &&
    jgez1.isConditionalJump() && !jgez1.isUnconditionalJump() &&
    !jgez1.isIndirectJump() && !jgez1.isAsmDataDirective() &&
    !jgez1.hasSideEffects() );

  WIR_Operation jgtz1(
    TC131::OpCode::JGTZ, TC131::OperationFormat::SDL,
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_LabelParameter( b ) );
  ufAssert( jgtz1.getSize() == 2 );
  ufAssert(
    !jgtz1.isMemoryAccess() && !jgtz1.isMemoryStore() &&
    !jgtz1.isMemoryLoad() && !jgtz1.isMove() && !jgtz1.isCall() &&
    !jgtz1.isIndirectCall() && !jgtz1.isReturn() && jgtz1.isJump() &&
    jgtz1.isConditionalJump() && !jgtz1.isUnconditionalJump() &&
    !jgtz1.isIndirectJump() && !jgtz1.isAsmDataDirective() &&
    !jgtz1.hasSideEffects() );

  WIR_Operation ji1(
    TC131::OpCode::JI, TC131::OperationFormat::A,
    new WIR_RegisterParameter( a1, WIR_Usage::use ) );
  ufAssert( ji1.getSize() == 4 );
  ufAssert(
    !ji1.isMemoryAccess() && !ji1.isMemoryStore() && !ji1.isMemoryLoad() &&
    !ji1.isMove() && !ji1.isCall() && !ji1.isIndirectCall() &&
    !ji1.isReturn() && ji1.isJump() && !ji1.isConditionalJump() &&
    ji1.isUnconditionalJump() && ji1.isIndirectJump() &&
    !ji1.isAsmDataDirective() && !ji1.hasSideEffects() );

  WIR_Operation ji2(
    TC131::OpCode::JI, TC131::OperationFormat::SA,
    new WIR_RegisterParameter( a1, WIR_Usage::use ) );
  ufAssert( ji2.getSize() == 2 );

  WIR_Operation jl1(
    TC131::OpCode::JL, TC131::OperationFormat::L,
    new WIR_LabelParameter( f ) );
  ufAssert( jl1.getSize() == 4 );
  ufAssert(
    !jl1.isMemoryAccess() && !jl1.isMemoryStore() && !jl1.isMemoryLoad() &&
    !jl1.isMove() && jl1.isCall() && !jl1.isIndirectCall() &&
    !jl1.isReturn() && !jl1.isJump() && !jl1.isConditionalJump() &&
    !jl1.isUnconditionalJump() && !jl1.isIndirectJump() &&
    !jl1.isAsmDataDirective() && !jl1.hasSideEffects() );

  WIR_Operation jla1(
    TC131::OpCode::JLA, TC131::OperationFormat::L,
    new WIR_LabelParameter( f ) );
  ufAssert( jla1.getSize() == 4 );
  ufAssert(
    !jla1.isMemoryAccess() && !jla1.isMemoryStore() && !jla1.isMemoryLoad() &&
    !jla1.isMove() && jla1.isCall() && !jla1.isIndirectCall() &&
    !jla1.isReturn() && !jla1.isJump() && !jla1.isConditionalJump() &&
    !jla1.isUnconditionalJump() && !jla1.isIndirectJump() &&
    !jla1.isAsmDataDirective() && !jla1.hasSideEffects() );

  WIR_Operation jlez1(
    TC131::OpCode::JLEZ, TC131::OperationFormat::SDL,
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_LabelParameter( b ) );
  ufAssert( jlez1.getSize() == 2 );
  ufAssert(
    !jlez1.isMemoryAccess() && !jlez1.isMemoryStore() &&
    !jlez1.isMemoryLoad() && !jlez1.isMove() && !jlez1.isCall() &&
    !jlez1.isIndirectCall() && !jlez1.isReturn() && jlez1.isJump() &&
    jlez1.isConditionalJump() && !jlez1.isUnconditionalJump() &&
    !jlez1.isIndirectJump() && !jlez1.isAsmDataDirective() &&
    !jlez1.hasSideEffects() );

  WIR_Operation jli1(
    TC131::OpCode::JLI, TC131::OperationFormat::A,
    new WIR_RegisterParameter( a1, WIR_Usage::use ) );
  ufAssert( jli1.getSize() == 4 );
  ufAssert(
    !jli1.isMemoryAccess() && !jli1.isMemoryStore() && !jli1.isMemoryLoad() &&
    !jli1.isMove() && !jli1.isCall() && jli1.isIndirectCall() &&
    !jli1.isReturn() && !jli1.isJump() && !jli1.isConditionalJump() &&
    !jli1.isUnconditionalJump() && !jli1.isIndirectJump() &&
    !jli1.isAsmDataDirective() && !jli1.hasSideEffects() );

  WIR_Operation jlt1(
    TC131::OpCode::JLT, TC131::OperationFormat::DC4L_1,
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new TC_Const4_Signed( 3 ),
    new WIR_LabelParameter( b ) );
  ufAssert( jlt1.getSize() == 4 );
  ufAssert(
    !jlt1.isMemoryAccess() && !jlt1.isMemoryStore() && !jlt1.isMemoryLoad() &&
    !jlt1.isMove() && !jlt1.isCall() && !jlt1.isIndirectCall() &&
    !jlt1.isReturn() && jlt1.isJump() && jlt1.isConditionalJump() &&
    !jlt1.isUnconditionalJump() && !jlt1.isIndirectJump() &&
    !jlt1.isAsmDataDirective() && !jlt1.hasSideEffects() );

  WIR_Operation jlt2(
    TC131::OpCode::JLT, TC131::OperationFormat::DDL_1,
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_LabelParameter( b ) );
  ufAssert( jlt2.getSize() == 4 );

  WIR_Operation jlt_u1(
    TC131::OpCode::JLT_U, TC131::OperationFormat::DC4L_2,
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new TC_Const4_Unsigned( 7 ),
    new WIR_LabelParameter( b ) );
  ufAssert( jlt_u1.getSize() == 4 );
  ufAssert(
    !jlt_u1.isMemoryAccess() && !jlt_u1.isMemoryStore() &&
    !jlt_u1.isMemoryLoad() && !jlt_u1.isMove() && !jlt_u1.isCall() &&
    !jlt_u1.isIndirectCall() && !jlt_u1.isReturn() && jlt_u1.isJump() &&
    jlt_u1.isConditionalJump() && !jlt_u1.isUnconditionalJump() &&
    !jlt_u1.isIndirectJump() && !jlt_u1.isAsmDataDirective() &&
    !jlt_u1.hasSideEffects() );

  WIR_Operation jlt_u2(
    TC131::OpCode::JLT_U, TC131::OperationFormat::DDL_1,
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_LabelParameter( b ) );
  ufAssert( jlt_u2.getSize() == 4 );

  WIR_Operation jltz1(
    TC131::OpCode::JLTZ, TC131::OperationFormat::SDL,
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_LabelParameter( b ) );
  ufAssert( jltz1.getSize() == 2 );
  ufAssert(
    !jltz1.isMemoryAccess() && !jltz1.isMemoryStore() &&
    !jltz1.isMemoryLoad() && !jltz1.isMove() && !jltz1.isCall() &&
    !jltz1.isIndirectCall() && !jltz1.isReturn() && jltz1.isJump() &&
    jltz1.isConditionalJump() && !jltz1.isUnconditionalJump() &&
    !jltz1.isIndirectJump() && !jltz1.isAsmDataDirective() &&
    !jltz1.hasSideEffects() );

  WIR_Operation jne1(
    TC131::OpCode::JNE, TC131::OperationFormat::DC4L_1,
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new TC_Const4_Signed( 3 ),
    new WIR_LabelParameter( b ) );
  ufAssert( jne1.getSize() == 4 );
  ufAssert(
    !jne1.isMemoryAccess() && !jne1.isMemoryStore() && !jne1.isMemoryLoad() &&
    !jne1.isMove() && !jne1.isCall() && !jne1.isIndirectCall() &&
    !jne1.isReturn() && jne1.isJump() && jne1.isConditionalJump() &&
    !jne1.isUnconditionalJump() && !jne1.isIndirectJump() &&
    !jne1.isAsmDataDirective() && !jne1.hasSideEffects() );

  WIR_Operation jne2(
    TC131::OpCode::JNE, TC131::OperationFormat::DDL_1,
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_LabelParameter( b ) );
  ufAssert( jne2.getSize() == 4 );

  WIR_Operation jne3(
    TC131::OpCode::JNE, TC131::OperationFormat::SIC4L,
    new WIR_RegisterParameter( tricore.D15(), WIR_Usage::use ),
    new TC_Const4_Signed( 3 ),
    new WIR_LabelParameter( b ) );
  ufAssert( jne3.getSize() == 2 );

  WIR_Operation jne4(
    TC131::OpCode::JNE, TC131::OperationFormat::SIDL,
    new WIR_RegisterParameter( tricore.D15(), WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_LabelParameter( b ) );
  ufAssert( jne4.getSize() == 2 );

  WIR_Operation jne_a1(
    TC131::OpCode::JNE_A, TC131::OperationFormat::AAL,
    new WIR_RegisterParameter( a1, WIR_Usage::use ),
    new WIR_RegisterParameter( a2, WIR_Usage::use ),
    new WIR_LabelParameter( b ) );
  ufAssert( jne_a1.getSize() == 4 );
  ufAssert(
    !jne_a1.isMemoryAccess() && !jne_a1.isMemoryStore() &&
    !jne_a1.isMemoryLoad() && !jne_a1.isMove() && !jne_a1.isCall() &&
    !jne_a1.isIndirectCall() && !jne_a1.isReturn() && jne_a1.isJump() &&
    jne_a1.isConditionalJump() && !jne_a1.isUnconditionalJump() &&
    !jne_a1.isIndirectJump() && !jne_a1.isAsmDataDirective() &&
    !jne_a1.hasSideEffects() );

  WIR_Operation jned1(
    TC131::OpCode::JNED, TC131::OperationFormat::DC4L_3,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new TC_Const4_Signed( 3 ),
    new WIR_LabelParameter( b ) );
  ufAssert( jned1.getSize() == 4 );
  ufAssert(
    !jned1.isMemoryAccess() && !jned1.isMemoryStore() &&
    !jned1.isMemoryLoad() && !jned1.isMove() && !jned1.isCall() &&
    !jned1.isIndirectCall() && !jned1.isReturn() && jned1.isJump() &&
    jned1.isConditionalJump() && !jned1.isUnconditionalJump() &&
    !jned1.isIndirectJump() && !jned1.isAsmDataDirective() &&
    !jned1.hasSideEffects() );

  WIR_Operation jned2(
    TC131::OpCode::JNED, TC131::OperationFormat::DDL_2,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_LabelParameter( b ) );
  ufAssert( jned2.getSize() == 4 );

  WIR_Operation jnei1(
    TC131::OpCode::JNEI, TC131::OperationFormat::DC4L_3,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new TC_Const4_Signed( 3 ),
    new WIR_LabelParameter( b ) );
  ufAssert( jnei1.getSize() == 4 );
  ufAssert(
    !jnei1.isMemoryAccess() && !jnei1.isMemoryStore() &&
    !jnei1.isMemoryLoad() && !jnei1.isMove() && !jnei1.isCall() &&
    !jnei1.isIndirectCall() && !jnei1.isReturn() && jnei1.isJump() &&
    jnei1.isConditionalJump() && !jnei1.isUnconditionalJump() &&
    !jnei1.isIndirectJump() && !jnei1.isAsmDataDirective() &&
    !jnei1.hasSideEffects() );

  WIR_Operation jnei2(
    TC131::OpCode::JNEI, TC131::OperationFormat::DDL_2,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_LabelParameter( b ) );
  ufAssert( jnei2.getSize() == 4 );

  WIR_Operation jnz1(
    TC131::OpCode::JNZ, TC131::OperationFormat::SDL,
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_LabelParameter( b ) );
  ufAssert( jnz1.getSize() == 2 );
  ufAssert(
    !jnz1.isMemoryAccess() && !jnz1.isMemoryStore() && !jnz1.isMemoryLoad() &&
    !jnz1.isMove() && !jnz1.isCall() && !jnz1.isIndirectCall() &&
    !jnz1.isReturn() && jnz1.isJump() && jnz1.isConditionalJump() &&
    !jnz1.isUnconditionalJump() && !jnz1.isIndirectJump() &&
    !jnz1.isAsmDataDirective() && !jnz1.hasSideEffects() );

  WIR_Operation jnz2(
    TC131::OpCode::JNZ, TC131::OperationFormat::SIL,
    new WIR_RegisterParameter( tricore.D15(), WIR_Usage::use ),
    new WIR_LabelParameter( b ) );
  ufAssert( jnz2.getSize() == 2 );

  WIR_Operation jnz_a1(
    TC131::OpCode::JNZ_A, TC131::OperationFormat::AL_2,
    new WIR_RegisterParameter( a1, WIR_Usage::use ),
    new WIR_LabelParameter( b ) );
  ufAssert( jnz_a1.getSize() == 4 );
  ufAssert(
    !jnz_a1.isMemoryAccess() && !jnz_a1.isMemoryStore() &&
    !jnz_a1.isMemoryLoad() && !jnz_a1.isMove() && !jnz_a1.isCall() &&
    !jnz_a1.isIndirectCall() && !jnz_a1.isReturn() && jnz_a1.isJump() &&
    jnz_a1.isConditionalJump() && !jnz_a1.isUnconditionalJump() &&
    !jnz_a1.isIndirectJump() && !jnz_a1.isAsmDataDirective() &&
    !jnz_a1.hasSideEffects() );

  WIR_Operation jnz_a2(
    TC131::OpCode::JNZ_A, TC131::OperationFormat::SAL_1,
    new WIR_RegisterParameter( a1, WIR_Usage::use ),
    new WIR_LabelParameter( b ) );
  ufAssert( jnz_a2.getSize() == 2 );

  WIR_Operation jnz_t1(
    TC131::OpCode::JNZ_T, TC131::OperationFormat::DC5L,
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new TC_Const5_Unsigned( 13 ),
    new WIR_LabelParameter( b ) );
  ufAssert( jnz_t1.getSize() == 4 );
  ufAssert(
    !jnz_t1.isMemoryAccess() && !jnz_t1.isMemoryStore() &&
    !jnz_t1.isMemoryLoad() && !jnz_t1.isMove() && !jnz_t1.isCall() &&
    !jnz_t1.isIndirectCall() && !jnz_t1.isReturn() && jnz_t1.isJump() &&
    jnz_t1.isConditionalJump() && !jnz_t1.isUnconditionalJump() &&
    !jnz_t1.isIndirectJump() && !jnz_t1.isAsmDataDirective() &&
    !jnz_t1.hasSideEffects() );

  WIR_Operation jnz_t2(
    TC131::OpCode::JNZ_T, TC131::OperationFormat::SIC5L,
    new WIR_RegisterParameter( tricore.D15(), WIR_Usage::use ),
    new TC_Const5_Unsigned( 13 ),
    new WIR_LabelParameter( b ) );
  ufAssert( jnz_t2.getSize() == 2 );

  WIR_Operation jz1(
    TC131::OpCode::JZ, TC131::OperationFormat::SDL,
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_LabelParameter( b ) );
  ufAssert( jz1.getSize() == 2 );
  ufAssert(
    !jz1.isMemoryAccess() && !jz1.isMemoryStore() && !jz1.isMemoryLoad() &&
    !jz1.isMove() && !jz1.isCall() && !jz1.isIndirectCall() &&
    !jz1.isReturn() && jz1.isJump() && jz1.isConditionalJump() &&
    !jz1.isUnconditionalJump() && !jz1.isIndirectJump() &&
    !jz1.isAsmDataDirective() && !jz1.hasSideEffects() );

  WIR_Operation jz2(
    TC131::OpCode::JZ, TC131::OperationFormat::SIL,
    new WIR_RegisterParameter( tricore.D15(), WIR_Usage::use ),
    new WIR_LabelParameter( b ) );
  ufAssert( jz2.getSize() == 2 );

  WIR_Operation jz_a1(
    TC131::OpCode::JZ_A, TC131::OperationFormat::AL_2,
    new WIR_RegisterParameter( a1, WIR_Usage::use ),
    new WIR_LabelParameter( b ) );
  ufAssert( jz_a1.getSize() == 4 );
  ufAssert(
    !jz_a1.isMemoryAccess() && !jz_a1.isMemoryStore() &&
    !jz_a1.isMemoryLoad() && !jz_a1.isMove() && !jz_a1.isCall() &&
    !jz_a1.isIndirectCall() && !jz_a1.isReturn() && jz_a1.isJump() &&
    jz_a1.isConditionalJump() && !jz_a1.isUnconditionalJump() &&
    !jz_a1.isIndirectJump() && !jz_a1.isAsmDataDirective() &&
    !jz_a1.hasSideEffects() );

  WIR_Operation jz_a2(
    TC131::OpCode::JZ_A, TC131::OperationFormat::SAL_1,
    new WIR_RegisterParameter( a1, WIR_Usage::use ),
    new WIR_LabelParameter( b ) );
  ufAssert( jz_a2.getSize() == 2 );

  WIR_Operation jz_t1(
    TC131::OpCode::JZ_T, TC131::OperationFormat::DC5L,
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new TC_Const5_Unsigned( 13 ),
    new WIR_LabelParameter( b ) );
  ufAssert( jz_t1.getSize() == 4 );
  ufAssert(
    !jz_t1.isMemoryAccess() && !jz_t1.isMemoryStore() &&
    !jz_t1.isMemoryLoad() && !jz_t1.isMove() && !jz_t1.isCall() &&
    !jz_t1.isIndirectCall() && !jz_t1.isReturn() && jz_t1.isJump() &&
    jz_t1.isConditionalJump() && !jz_t1.isUnconditionalJump() &&
    !jz_t1.isIndirectJump() && !jz_t1.isAsmDataDirective() &&
    !jz_t1.hasSideEffects() );

  WIR_Operation jz_t2(
    TC131::OpCode::JZ_T, TC131::OperationFormat::SIC5L,
    new WIR_RegisterParameter( tricore.D15(), WIR_Usage::use ),
    new TC_Const5_Unsigned( 13 ),
    new WIR_LabelParameter( b ) );
  ufAssert( jz_t2.getSize() == 2 );

  WIR_Operation ld_a1(
    TC131::OpCode::LD_A, TC131::OperationFormat::AC18ABSA,
    new WIR_RegisterParameter( a1, WIR_Usage::def ),
    new TC_Const18_Unsigned( 16383 ) );
  ufAssert( ld_a1.getSize() == 4 );
  ufAssert(
    !ld_a1.isMemoryAccess() && !ld_a1.isMemoryStore() && ld_a1.isMemoryLoad() &&
    !ld_a1.isMove() && !ld_a1.isCall() && !ld_a1.isIndirectCall() &&
    !ld_a1.isReturn() && !ld_a1.isJump() && !ld_a1.isConditionalJump() &&
    !ld_a1.isUnconditionalJump() && !ld_a1.isIndirectJump() &&
    !ld_a1.isAsmDataDirective() && !ld_a1.hasSideEffects() );

  WIR_Operation ld_a2(
    TC131::OpCode::LD_A, TC131::OperationFormat::ALABSA,
    new WIR_RegisterParameter( a1, WIR_Usage::def ),
    new WIR_LabelParameter( b ) );
  ufAssert( ld_a2.getSize() == 4 );

  WIR_Operation ld_a3(
    TC131::OpCode::LD_A, TC131::OperationFormat::AAC10BOA,
    new WIR_RegisterParameter( a1, WIR_Usage::def ),
    new WIR_RegisterParameter( a2, WIR_Usage::use ),
    new TC_Const10_Signed( -512 ) );
  ufAssert( ld_a3.getSize() == 4 );

  WIR_Operation ld_a4(
    TC131::OpCode::LD_A, TC131::OperationFormat::APBRA,
    new WIR_RegisterParameter( a1, WIR_Usage::def ),
    new WIR_RegisterParameter( p1, WIR_Usage::defuse ) );
  ufAssert( ld_a4.getSize() == 4 );

  WIR_Operation ld_a5(
    TC131::OpCode::LD_A, TC131::OperationFormat::APC10CA,
    new WIR_RegisterParameter( a1, WIR_Usage::def ),
    new WIR_RegisterParameter( p1, WIR_Usage::defuse ),
    new TC_Const10_Signed( -512 ) );
  ufAssert( ld_a5.getSize() == 4 );

  WIR_Operation ld_a6(
    TC131::OpCode::LD_A, TC131::OperationFormat::AAC10PIA,
    new WIR_RegisterParameter( a1, WIR_Usage::def ),
    new WIR_AddressingModeParameter( TC131::AddressingMode::post ),
    new WIR_RegisterParameter( a2, WIR_Usage::defuse ),
    new TC_Const10_Signed( -512 ) );
  ufAssert( ld_a6.getSize() == 4 );

  WIR_Operation ld_a7(
    TC131::OpCode::LD_A, TC131::OperationFormat::AAC10PIA,
    new WIR_RegisterParameter( a1, WIR_Usage::def ),
    new WIR_AddressingModeParameter( TC131::AddressingMode::pre ),
    new WIR_RegisterParameter( a2, WIR_Usage::defuse ),
    new TC_Const10_Signed( -512 ) );
  ufAssert( ld_a7.getSize() == 4 );

  WIR_Operation ld_a8(
    TC131::OpCode::LD_A, TC131::OperationFormat::AAC16BOA,
    new WIR_RegisterParameter( a1, WIR_Usage::def ),
    new WIR_RegisterParameter( a2, WIR_Usage::use ),
    new TC_Const16_Signed( -32768 ) );
  ufAssert( ld_a8.getSize() == 4 );

  WIR_Operation ld_a9(
    TC131::OpCode::LD_A, TC131::OperationFormat::AALC16BOA,
    new WIR_RegisterParameter( a1, WIR_Usage::def ),
    new WIR_RegisterParameter( a2, WIR_Usage::use ),
    new WIR_LabelParameter( b ),
    new TC_Const16_Signed( -32768 ) );
  ufAssert( ld_a9.getSize() == 4 );

  WIR_Operation ld_a10(
    TC131::OpCode::LD_A, TC131::OperationFormat::SISPC10_1,
    new WIR_RegisterParameter( tricore.A15(), WIR_Usage::def ),
    new WIR_RegisterParameter( tricore.SP(), WIR_Usage::use ),
    new TC_Const10_Unsigned( 252 ) );
  ufAssert( ld_a10.getSize() == 2 );

  WIR_Operation ld_a11(
    TC131::OpCode::LD_A, TC131::OperationFormat::SAA_2,
    new WIR_RegisterParameter( a1, WIR_Usage::def ),
    new WIR_RegisterParameter( a2, WIR_Usage::use ) );
  ufAssert( ld_a11.getSize() == 2 );

  WIR_Operation ld_a12(
    TC131::OpCode::LD_A, TC131::OperationFormat::SAA_3,
    new WIR_RegisterParameter( a1, WIR_Usage::def ),
    new WIR_RegisterParameter( a2, WIR_Usage::defuse ) );
  ufAssert( ld_a12.getSize() == 2 );

  WIR_Operation ld_a13(
    TC131::OpCode::LD_A, TC131::OperationFormat::SAIC4,
    new WIR_RegisterParameter( a1, WIR_Usage::def ),
    new WIR_RegisterParameter( tricore.A15(), WIR_Usage::use ),
    new TC_Const4_Unsigned( 12 ) );
  ufAssert( ld_a13.getSize() == 2 );

  WIR_Operation ld_a14(
    TC131::OpCode::LD_A, TC131::OperationFormat::SIAC4_1,
    new WIR_RegisterParameter( tricore.A15(), WIR_Usage::def ),
    new WIR_RegisterParameter( a1, WIR_Usage::use ),
    new TC_Const4_Unsigned( 12 ) );
  ufAssert( ld_a14.getSize() == 2 );

  WIR_Operation ld_b1(
    TC131::OpCode::LD_B, TC131::OperationFormat::DC18ABSA,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new TC_Const18_Unsigned( 16383 ) );
  ufAssert( ld_b1.getSize() == 4 );
  ufAssert(
    !ld_b1.isMemoryAccess() && !ld_b1.isMemoryStore() && ld_b1.isMemoryLoad() &&
    !ld_b1.isMove() && !ld_b1.isCall() && !ld_b1.isIndirectCall() &&
    !ld_b1.isReturn() && !ld_b1.isJump() && !ld_b1.isConditionalJump() &&
    !ld_b1.isUnconditionalJump() && !ld_b1.isIndirectJump() &&
    !ld_b1.isAsmDataDirective() && !ld_b1.hasSideEffects() );

  WIR_Operation ld_b2(
    TC131::OpCode::LD_B, TC131::OperationFormat::DLABSA,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_LabelParameter( b ) );
  ufAssert( ld_b2.getSize() == 4 );

  WIR_Operation ld_b3(
    TC131::OpCode::LD_B, TC131::OperationFormat::DAC10BOA,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( a1, WIR_Usage::use ),
    new TC_Const10_Signed( -512 ) );
  ufAssert( ld_b3.getSize() == 4 );

  WIR_Operation ld_b4(
    TC131::OpCode::LD_B, TC131::OperationFormat::DPBRA,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( p1, WIR_Usage::defuse ) );
  ufAssert( ld_b4.getSize() == 4 );

  WIR_Operation ld_b5(
    TC131::OpCode::LD_B, TC131::OperationFormat::DPC10CA,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( p1, WIR_Usage::defuse ),
    new TC_Const10_Signed( -512 ) );
  ufAssert( ld_b5.getSize() == 4 );

  WIR_Operation ld_b6(
    TC131::OpCode::LD_B, TC131::OperationFormat::DAC10PIA,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_AddressingModeParameter( TC131::AddressingMode::post ),
    new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
    new TC_Const10_Signed( -512 ) );
  ufAssert( ld_b6.getSize() == 4 );

  WIR_Operation ld_b7(
    TC131::OpCode::LD_B, TC131::OperationFormat::DAC10PIA,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_AddressingModeParameter( TC131::AddressingMode::pre ),
    new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
    new TC_Const10_Signed( -512 ) );
  ufAssert( ld_b7.getSize() == 4 );

  WIR_Operation ld_bu1(
    TC131::OpCode::LD_BU, TC131::OperationFormat::DC18ABSA,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new TC_Const18_Unsigned( 16383 ) );
  ufAssert( ld_bu1.getSize() == 4 );
  ufAssert(
    !ld_bu1.isMemoryAccess() && !ld_bu1.isMemoryStore() &&
    ld_bu1.isMemoryLoad() && !ld_bu1.isMove() && !ld_bu1.isCall() &&
    !ld_bu1.isIndirectCall() && !ld_bu1.isReturn() && !ld_bu1.isJump() &&
    !ld_bu1.isConditionalJump() && !ld_bu1.isUnconditionalJump() &&
    !ld_bu1.isIndirectJump() && !ld_bu1.isAsmDataDirective() &&
    !ld_bu1.hasSideEffects() );

  WIR_Operation ld_bu2(
    TC131::OpCode::LD_BU, TC131::OperationFormat::DLABSA,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_LabelParameter( b ) );
  ufAssert( ld_bu2.getSize() == 4 );

  WIR_Operation ld_bu3(
    TC131::OpCode::LD_BU, TC131::OperationFormat::DAC10BOA,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( a1, WIR_Usage::use ),
    new TC_Const10_Signed( -512 ) );
  ufAssert( ld_bu3.getSize() == 4 );

  WIR_Operation ld_bu4(
    TC131::OpCode::LD_BU, TC131::OperationFormat::DPBRA,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( p1, WIR_Usage::defuse ) );
  ufAssert( ld_bu4.getSize() == 4 );

  WIR_Operation ld_bu5(
    TC131::OpCode::LD_BU, TC131::OperationFormat::DPC10CA,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( p1, WIR_Usage::defuse ),
    new TC_Const10_Signed( -512 ) );
  ufAssert( ld_bu5.getSize() == 4 );

  WIR_Operation ld_bu6(
    TC131::OpCode::LD_BU, TC131::OperationFormat::DAC10PIA,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_AddressingModeParameter( TC131::AddressingMode::post ),
    new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
    new TC_Const10_Signed( -512 ) );
  ufAssert( ld_bu6.getSize() == 4 );

  WIR_Operation ld_bu7(
    TC131::OpCode::LD_BU, TC131::OperationFormat::DAC10PIA,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_AddressingModeParameter( TC131::AddressingMode::pre ),
    new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
    new TC_Const10_Signed( -512 ) );
  ufAssert( ld_bu7.getSize() == 4 );

  WIR_Operation ld_bu8(
    TC131::OpCode::LD_BU, TC131::OperationFormat::SDA_2,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( a1, WIR_Usage::use ) );
  ufAssert( ld_bu8.getSize() == 2 );

  WIR_Operation ld_bu9(
    TC131::OpCode::LD_BU, TC131::OperationFormat::SDA_3,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( a1, WIR_Usage::defuse ) );
  ufAssert( ld_bu9.getSize() == 2 );

  WIR_Operation ld_bu10(
    TC131::OpCode::LD_BU, TC131::OperationFormat::SDIC4_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( tricore.A15(), WIR_Usage::use ),
    new TC_Const4_Unsigned( 7 ) );
  ufAssert( ld_bu10.getSize() == 2 );

  WIR_Operation ld_bu11(
    TC131::OpCode::LD_BU, TC131::OperationFormat::SIAC4_2,
    new WIR_RegisterParameter( tricore.D15(), WIR_Usage::def ),
    new WIR_RegisterParameter( a1, WIR_Usage::use ),
    new TC_Const4_Unsigned( 7 ) );
  ufAssert( ld_bu11.getSize() == 2 );

  WIR_Operation ld_d1(
    TC131::OpCode::LD_D, TC131::OperationFormat::EC18ABSA,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new TC_Const18_Unsigned( 16383 ) );
  ufAssert( ld_d1.getSize() == 4 );
  ufAssert(
    !ld_d1.isMemoryAccess() && !ld_d1.isMemoryStore() && ld_d1.isMemoryLoad() &&
    !ld_d1.isMove() && !ld_d1.isCall() && !ld_d1.isIndirectCall() &&
    !ld_d1.isReturn() && !ld_d1.isJump() && !ld_d1.isConditionalJump() &&
    !ld_d1.isUnconditionalJump() && !ld_d1.isIndirectJump() &&
    !ld_d1.isAsmDataDirective() && !ld_d1.hasSideEffects() );

  WIR_Operation ld_d2(
    TC131::OpCode::LD_D, TC131::OperationFormat::ELABSA,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_LabelParameter( b ) );
  ufAssert( ld_d2.getSize() == 4 );

  WIR_Operation ld_d3(
    TC131::OpCode::LD_D, TC131::OperationFormat::EAC10BOA,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( a1, WIR_Usage::use ),
    new TC_Const10_Signed( -512 ) );
  ufAssert( ld_d3.getSize() == 4 );

  WIR_Operation ld_d4(
    TC131::OpCode::LD_D, TC131::OperationFormat::EPBRA,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( p1, WIR_Usage::defuse ) );
  ufAssert( ld_d4.getSize() == 4 );

  WIR_Operation ld_d5(
    TC131::OpCode::LD_D, TC131::OperationFormat::EPC10CA,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( p1, WIR_Usage::defuse ),
    new TC_Const10_Signed( -512 ) );
  ufAssert( ld_d5.getSize() == 4 );

  WIR_Operation ld_d6(
    TC131::OpCode::LD_D, TC131::OperationFormat::EAC10PIA,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_AddressingModeParameter( TC131::AddressingMode::post ),
    new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
    new TC_Const10_Signed( -512 ) );
  ufAssert( ld_d6.getSize() == 4 );

  WIR_Operation ld_d7(
    TC131::OpCode::LD_D, TC131::OperationFormat::EAC10PIA,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_AddressingModeParameter( TC131::AddressingMode::pre ),
    new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
    new TC_Const10_Signed( -512 ) );
  ufAssert( ld_d7.getSize() == 4 );

  WIR_Operation ld_da1(
    TC131::OpCode::LD_DA, TC131::OperationFormat::PC18ABSA,
    new WIR_RegisterParameter( p1, WIR_Usage::def ),
    new TC_Const18_Unsigned( 16383 ) );
  ufAssert( ld_da1.getSize() == 4 );
  ufAssert(
    !ld_da1.isMemoryAccess() && !ld_da1.isMemoryStore() &&
    ld_da1.isMemoryLoad() && !ld_da1.isMove() && !ld_da1.isCall() &&
    !ld_da1.isIndirectCall() && !ld_da1.isReturn() && !ld_da1.isJump() &&
    !ld_da1.isConditionalJump() && !ld_da1.isUnconditionalJump() &&
    !ld_da1.isIndirectJump() && !ld_da1.isAsmDataDirective() &&
    !ld_da1.hasSideEffects() );

  WIR_Operation ld_da2(
    TC131::OpCode::LD_DA, TC131::OperationFormat::PLABSA,
    new WIR_RegisterParameter( p1, WIR_Usage::def ),
    new WIR_LabelParameter( b ) );
  ufAssert( ld_da2.getSize() == 4 );

  WIR_Operation ld_da3(
    TC131::OpCode::LD_DA, TC131::OperationFormat::PAC10BOA,
    new WIR_RegisterParameter( p1, WIR_Usage::def ),
    new WIR_RegisterParameter( a1, WIR_Usage::use ),
    new TC_Const10_Signed( -512 ) );
  ufAssert( ld_da3.getSize() == 4 );

  WIR_Operation ld_da4(
    TC131::OpCode::LD_DA, TC131::OperationFormat::PPBRA_1,
    new WIR_RegisterParameter( p1, WIR_Usage::def ),
    new WIR_RegisterParameter( p2, WIR_Usage::defuse ) );
  ufAssert( ld_da4.getSize() == 4 );

  WIR_Operation ld_da5(
    TC131::OpCode::LD_DA, TC131::OperationFormat::PPC10CA,
    new WIR_RegisterParameter( p1, WIR_Usage::def ),
    new WIR_RegisterParameter( p2, WIR_Usage::defuse ),
    new TC_Const10_Signed( -512 ) );
  ufAssert( ld_da5.getSize() == 4 );

  WIR_Operation ld_da6(
    TC131::OpCode::LD_DA, TC131::OperationFormat::PAC10PIA,
    new WIR_RegisterParameter( p1, WIR_Usage::def ),
    new WIR_AddressingModeParameter( TC131::AddressingMode::post ),
    new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
    new TC_Const10_Signed( -512 ) );
  ufAssert( ld_da6.getSize() == 4 );

  WIR_Operation ld_da7(
    TC131::OpCode::LD_DA, TC131::OperationFormat::PAC10PIA,
    new WIR_RegisterParameter( p1, WIR_Usage::def ),
    new WIR_AddressingModeParameter( TC131::AddressingMode::pre ),
    new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
    new TC_Const10_Signed( -512 ) );
  ufAssert( ld_da7.getSize() == 4 );

  WIR_Operation ld_h1(
    TC131::OpCode::LD_H, TC131::OperationFormat::DC18ABSA,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new TC_Const18_Unsigned( 16383 ) );
  ufAssert( ld_h1.getSize() == 4 );
  ufAssert(
    !ld_h1.isMemoryAccess() && !ld_h1.isMemoryStore() && ld_h1.isMemoryLoad() &&
    !ld_h1.isMove() && !ld_h1.isCall() && !ld_h1.isIndirectCall() &&
    !ld_h1.isReturn() && !ld_h1.isJump() && !ld_h1.isConditionalJump() &&
    !ld_h1.isUnconditionalJump() && !ld_h1.isIndirectJump() &&
    !ld_h1.isAsmDataDirective() && !ld_h1.hasSideEffects() );

  WIR_Operation ld_h2(
    TC131::OpCode::LD_H, TC131::OperationFormat::DLABSA,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_LabelParameter( b ) );
  ufAssert( ld_h2.getSize() == 4 );

  WIR_Operation ld_h3(
    TC131::OpCode::LD_H, TC131::OperationFormat::DAC10BOA,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( a1, WIR_Usage::use ),
    new TC_Const10_Signed( -512 ) );
  ufAssert( ld_h3.getSize() == 4 );

  WIR_Operation ld_h4(
    TC131::OpCode::LD_H, TC131::OperationFormat::DPBRA,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( p1, WIR_Usage::defuse ) );
  ufAssert( ld_h4.getSize() == 4 );

  WIR_Operation ld_h5(
    TC131::OpCode::LD_H, TC131::OperationFormat::DPC10CA,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( p1, WIR_Usage::defuse ),
    new TC_Const10_Signed( -512 ) );
  ufAssert( ld_h5.getSize() == 4 );

  WIR_Operation ld_h6(
    TC131::OpCode::LD_H, TC131::OperationFormat::DAC10PIA,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_AddressingModeParameter( TC131::AddressingMode::post ),
    new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
    new TC_Const10_Signed( -512 ) );
  ufAssert( ld_h6.getSize() == 4 );

  WIR_Operation ld_h7(
    TC131::OpCode::LD_H, TC131::OperationFormat::DAC10PIA,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_AddressingModeParameter( TC131::AddressingMode::pre ),
    new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
    new TC_Const10_Signed( -512 ) );
  ufAssert( ld_h7.getSize() == 4 );

  WIR_Operation ld_h8(
    TC131::OpCode::LD_H, TC131::OperationFormat::SDA_2,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( a1, WIR_Usage::use ) );
  ufAssert( ld_h8.getSize() == 2 );

  WIR_Operation ld_h9(
    TC131::OpCode::LD_H, TC131::OperationFormat::SDA_3,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( a1, WIR_Usage::defuse ) );
  ufAssert( ld_h9.getSize() == 2 );

  WIR_Operation ld_h10(
    TC131::OpCode::LD_H, TC131::OperationFormat::SDIC4_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( tricore.A15(), WIR_Usage::use ),
    new TC_Const4_Unsigned( 6 ) );
  ufAssert( ld_h10.getSize() == 2 );

  WIR_Operation ld_h11(
    TC131::OpCode::LD_H, TC131::OperationFormat::SIAC4_2,
    new WIR_RegisterParameter( tricore.D15(), WIR_Usage::def ),
    new WIR_RegisterParameter( a1, WIR_Usage::use ),
    new TC_Const4_Unsigned( 6 ) );
  ufAssert( ld_h11.getSize() == 2 );

  WIR_Operation ld_hu1(
    TC131::OpCode::LD_HU, TC131::OperationFormat::DC18ABSA,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new TC_Const18_Unsigned( 16383 ) );
  ufAssert( ld_hu1.getSize() == 4 );
  ufAssert(
    !ld_hu1.isMemoryAccess() && !ld_hu1.isMemoryStore() &&
    ld_hu1.isMemoryLoad() && !ld_hu1.isMove() && !ld_hu1.isCall() &&
    !ld_hu1.isIndirectCall() && !ld_hu1.isReturn() && !ld_hu1.isJump() &&
    !ld_hu1.isConditionalJump() && !ld_hu1.isUnconditionalJump() &&
    !ld_hu1.isIndirectJump() && !ld_hu1.isAsmDataDirective() &&
    !ld_hu1.hasSideEffects() );

  WIR_Operation ld_hu2(
    TC131::OpCode::LD_HU, TC131::OperationFormat::DLABSA,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_LabelParameter( b ) );
  ufAssert( ld_hu2.getSize() == 4 );

  WIR_Operation ld_hu3(
    TC131::OpCode::LD_HU, TC131::OperationFormat::DAC10BOA,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( a1, WIR_Usage::use ),
    new TC_Const10_Signed( -512 ) );
  ufAssert( ld_hu3.getSize() == 4 );

  WIR_Operation ld_hu4(
    TC131::OpCode::LD_HU, TC131::OperationFormat::DPBRA,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( p1, WIR_Usage::defuse ) );
  ufAssert( ld_hu4.getSize() == 4 );

  WIR_Operation ld_hu5(
    TC131::OpCode::LD_HU, TC131::OperationFormat::DPC10CA,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( p1, WIR_Usage::defuse ),
    new TC_Const10_Signed( -512 ) );
  ufAssert( ld_hu5.getSize() == 4 );

  WIR_Operation ld_hu6(
    TC131::OpCode::LD_HU, TC131::OperationFormat::DAC10PIA,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_AddressingModeParameter( TC131::AddressingMode::post ),
    new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
    new TC_Const10_Signed( -512 ) );
  ufAssert( ld_hu6.getSize() == 4 );

  WIR_Operation ld_hu7(
    TC131::OpCode::LD_HU, TC131::OperationFormat::DAC10PIA,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_AddressingModeParameter( TC131::AddressingMode::pre ),
    new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
    new TC_Const10_Signed( -512 ) );
  ufAssert( ld_hu7.getSize() == 4 );

  WIR_Operation ld_q1(
    TC131::OpCode::LD_Q, TC131::OperationFormat::DC18ABSA,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new TC_Const18_Unsigned( 16383 ) );
  ufAssert( ld_q1.getSize() == 4 );
  ufAssert(
    !ld_q1.isMemoryAccess() && !ld_q1.isMemoryStore() && ld_q1.isMemoryLoad() &&
    !ld_q1.isMove() && !ld_q1.isCall() && !ld_q1.isIndirectCall() &&
    !ld_q1.isReturn() && !ld_q1.isJump() && !ld_q1.isConditionalJump() &&
    !ld_q1.isUnconditionalJump() && !ld_q1.isIndirectJump() &&
    !ld_q1.isAsmDataDirective() && !ld_q1.hasSideEffects() );

  WIR_Operation ld_q2(
    TC131::OpCode::LD_Q, TC131::OperationFormat::DLABSA,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_LabelParameter( b ) );
  ufAssert( ld_q2.getSize() == 4 );

  WIR_Operation ld_q3(
    TC131::OpCode::LD_Q, TC131::OperationFormat::DAC10BOA,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( a1, WIR_Usage::use ),
    new TC_Const10_Signed( -512 ) );
  ufAssert( ld_q3.getSize() == 4 );

  WIR_Operation ld_q4(
    TC131::OpCode::LD_Q, TC131::OperationFormat::DPBRA,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( p1, WIR_Usage::defuse ) );
  ufAssert( ld_q4.getSize() == 4 );

  WIR_Operation ld_q5(
    TC131::OpCode::LD_Q, TC131::OperationFormat::DPC10CA,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( p1, WIR_Usage::defuse ),
    new TC_Const10_Signed( -512 ) );
  ufAssert( ld_q5.getSize() == 4 );

  WIR_Operation ld_q6(
    TC131::OpCode::LD_Q, TC131::OperationFormat::DAC10PIA,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_AddressingModeParameter( TC131::AddressingMode::post ),
    new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
    new TC_Const10_Signed( -512 ) );
  ufAssert( ld_q6.getSize() == 4 );

  WIR_Operation ld_q7(
    TC131::OpCode::LD_Q, TC131::OperationFormat::DAC10PIA,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_AddressingModeParameter( TC131::AddressingMode::pre ),
    new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
    new TC_Const10_Signed( -512 ) );
  ufAssert( ld_q7.getSize() == 4 );

  WIR_Operation ld_w1(
    TC131::OpCode::LD_W, TC131::OperationFormat::DC18ABSA,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new TC_Const18_Unsigned( 16383 ) );
  ufAssert( ld_w1.getSize() == 4 );
  ufAssert(
    !ld_w1.isMemoryAccess() && !ld_w1.isMemoryStore() && ld_w1.isMemoryLoad() &&
    !ld_w1.isMove() && !ld_w1.isCall() && !ld_w1.isIndirectCall() &&
    !ld_w1.isReturn() && !ld_w1.isJump() && !ld_w1.isConditionalJump() &&
    !ld_w1.isUnconditionalJump() && !ld_w1.isIndirectJump() &&
    !ld_w1.isAsmDataDirective() && !ld_w1.hasSideEffects() );

  WIR_Operation ld_w2(
    TC131::OpCode::LD_W, TC131::OperationFormat::DLABSA,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_LabelParameter( b ) );
  ufAssert( ld_w2.getSize() == 4 );

  WIR_Operation ld_w3(
    TC131::OpCode::LD_W, TC131::OperationFormat::DAC10BOA,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( a1, WIR_Usage::use ),
    new TC_Const10_Signed( -512 ) );
  ufAssert( ld_w3.getSize() == 4 );

  WIR_Operation ld_w4(
    TC131::OpCode::LD_W, TC131::OperationFormat::DPBRA,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( p1, WIR_Usage::defuse ) );
  ufAssert( ld_w4.getSize() == 4 );

  WIR_Operation ld_w5(
    TC131::OpCode::LD_W, TC131::OperationFormat::DPC10CA,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( p1, WIR_Usage::defuse ),
    new TC_Const10_Signed( -512 ) );
  ufAssert( ld_w5.getSize() == 4 );

  WIR_Operation ld_w6(
    TC131::OpCode::LD_W, TC131::OperationFormat::DAC10PIA,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_AddressingModeParameter( TC131::AddressingMode::post ),
    new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
    new TC_Const10_Signed( -512 ) );
  ufAssert( ld_w6.getSize() == 4 );

  WIR_Operation ld_w7(
    TC131::OpCode::LD_W, TC131::OperationFormat::DAC10PIA,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_AddressingModeParameter( TC131::AddressingMode::pre ),
    new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
    new TC_Const10_Signed( -512 ) );
  ufAssert( ld_w7.getSize() == 4 );

  WIR_Operation ld_w8(
    TC131::OpCode::LD_W, TC131::OperationFormat::DAC16BOA,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( a1, WIR_Usage::use ),
    new TC_Const16_Signed( -32768 ) );
  ufAssert( ld_w8.getSize() == 4 );

  WIR_Operation ld_w9(
    TC131::OpCode::LD_W, TC131::OperationFormat::DALC16BOA,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( a1, WIR_Usage::use ),
    new WIR_LabelParameter( b ),
    new TC_Const16_Signed( 16302 ) );
  ufAssert( ld_w9.getSize() == 4 );

  WIR_Operation ld_w10(
    TC131::OpCode::LD_W, TC131::OperationFormat::SISPC10_2,
    new WIR_RegisterParameter( tricore.D15(), WIR_Usage::def ),
    new WIR_RegisterParameter( tricore.SP(), WIR_Usage::use ),
    new TC_Const10_Unsigned( 1020 ) );
  ufAssert( ld_w10.getSize() == 2 );

  WIR_Operation ld_w11(
    TC131::OpCode::LD_W, TC131::OperationFormat::SDA_2,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( a1, WIR_Usage::use ) );
  ufAssert( ld_w11.getSize() == 2 );

  WIR_Operation ld_w12(
    TC131::OpCode::LD_W, TC131::OperationFormat::SDA_3,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( a1, WIR_Usage::defuse ) );
  ufAssert( ld_w12.getSize() == 2 );

  WIR_Operation ld_w13(
    TC131::OpCode::LD_W, TC131::OperationFormat::SDIC4_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( tricore.A15(), WIR_Usage::use ),
    new TC_Const4_Unsigned( 4 ) );
  ufAssert( ld_w13.getSize() == 2 );

  WIR_Operation ld_w14(
    TC131::OpCode::LD_W, TC131::OperationFormat::SIAC4_2,
    new WIR_RegisterParameter( tricore.D15(), WIR_Usage::def ),
    new WIR_RegisterParameter( a1, WIR_Usage::use ),
    new TC_Const4_Unsigned( 4 ) );
  ufAssert( ld_w14.getSize() == 2 );

  WIR_Operation ldlcx1(
    TC131::OpCode::LDLCX, TC131::OperationFormat::C18ABSA,
    new TC_Const18_Unsigned( 16383 ) );
  ufAssert( ldlcx1.getSize() == 4 );
  ufAssert(
    !ldlcx1.isMemoryAccess() && !ldlcx1.isMemoryStore() &&
    ldlcx1.isMemoryLoad() && !ldlcx1.isMove() && !ldlcx1.isCall() &&
    !ldlcx1.isIndirectCall() && !ldlcx1.isReturn() && !ldlcx1.isJump() &&
    !ldlcx1.isConditionalJump() && !ldlcx1.isUnconditionalJump() &&
    !ldlcx1.isIndirectJump() && !ldlcx1.isAsmDataDirective() &&
    ldlcx1.hasSideEffects() );

  WIR_Operation ldlcx2(
    TC131::OpCode::LDLCX, TC131::OperationFormat::LABSA,
    new WIR_LabelParameter( b ) );
  ufAssert( ldlcx2.getSize() == 4 );

  WIR_Operation ldlcx3(
    TC131::OpCode::LDLCX, TC131::OperationFormat::AC10BOA,
    new WIR_RegisterParameter( a1, WIR_Usage::use ),
    new TC_Const10_Signed( -486 ) );
  ufAssert( ldlcx3.getSize() == 4 );

  WIR_Operation ldmst1(
    TC131::OpCode::LDMST, TC131::OperationFormat::C18EABSA,
    new TC_Const18_Unsigned( 16383 ),
    new WIR_RegisterParameter( e1, WIR_Usage::use ) );
  ufAssert( ldmst1.getSize() == 4 );
  ufAssert(
    !ldmst1.isMemoryAccess() && ldmst1.isMemoryStore() &&
    ldmst1.isMemoryLoad() && !ldmst1.isMove() && !ldmst1.isCall() &&
    !ldmst1.isIndirectCall() && !ldmst1.isReturn() && !ldmst1.isJump() &&
    !ldmst1.isConditionalJump() && !ldmst1.isUnconditionalJump() &&
    !ldmst1.isIndirectJump() && !ldmst1.isAsmDataDirective() &&
    !ldmst1.hasSideEffects() );

  WIR_Operation ldmst2(
    TC131::OpCode::LDMST, TC131::OperationFormat::LEABSA,
    new WIR_LabelParameter( b ),
    new WIR_RegisterParameter( e1, WIR_Usage::use ) );
  ufAssert( ldmst2.getSize() == 4 );

  WIR_Operation ldmst3(
    TC131::OpCode::LDMST, TC131::OperationFormat::AC10EBOA,
    new WIR_RegisterParameter( a1, WIR_Usage::use ),
    new TC_Const10_Signed( -512 ),
    new WIR_RegisterParameter( e1, WIR_Usage::use ) );
  ufAssert( ldmst3.getSize() == 4 );

  WIR_Operation ldmst4(
    TC131::OpCode::LDMST, TC131::OperationFormat::PEBRA,
    new WIR_RegisterParameter( p1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( e1, WIR_Usage::use ) );
  ufAssert( ldmst4.getSize() == 4 );

  WIR_Operation ldmst5(
    TC131::OpCode::LDMST, TC131::OperationFormat::PC10ECA,
    new WIR_RegisterParameter( p1, WIR_Usage::defuse ),
    new TC_Const10_Signed( -512 ),
    new WIR_RegisterParameter( e1, WIR_Usage::use ) );
  ufAssert( ldmst5.getSize() == 4 );

  WIR_Operation ldmst6(
    TC131::OpCode::LDMST, TC131::OperationFormat::AC10EPIA,
    new WIR_AddressingModeParameter( TC131::AddressingMode::post ),
    new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
    new TC_Const10_Signed( -512 ),
    new WIR_RegisterParameter( e1, WIR_Usage::use ) );
  ufAssert( ldmst6.getSize() == 4 );

  WIR_Operation ldmst7(
    TC131::OpCode::LDMST, TC131::OperationFormat::AC10EPIA,
    new WIR_AddressingModeParameter( TC131::AddressingMode::pre ),
    new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
    new TC_Const10_Signed( -512 ),
    new WIR_RegisterParameter( e1, WIR_Usage::use ) );
  ufAssert( ldmst7.getSize() == 4 );

  WIR_Operation lducx1(
    TC131::OpCode::LDUCX, TC131::OperationFormat::C18ABSA,
    new TC_Const18_Unsigned( 16383 ) );
  ufAssert( lducx1.getSize() == 4 );
  ufAssert(
    !lducx1.isMemoryAccess() && !lducx1.isMemoryStore() &&
    lducx1.isMemoryLoad() && !lducx1.isMove() && !lducx1.isCall() &&
    !lducx1.isIndirectCall() && !lducx1.isReturn() && !lducx1.isJump() &&
    !lducx1.isConditionalJump() && !lducx1.isUnconditionalJump() &&
    !lducx1.isIndirectJump() && !lducx1.isAsmDataDirective() &&
    lducx1.hasSideEffects() );

  WIR_Operation lducx2(
    TC131::OpCode::LDUCX, TC131::OperationFormat::LABSA,
    new WIR_LabelParameter( b ) );
  ufAssert( lducx2.getSize() == 4 );

  WIR_Operation lducx3(
    TC131::OpCode::LDUCX, TC131::OperationFormat::AC10BOA,
    new WIR_RegisterParameter( a1, WIR_Usage::use ),
    new TC_Const10_Signed( -486 ) );
  ufAssert( lducx3.getSize() == 4 );

  WIR_Operation lea1(
    TC131::OpCode::LEA, TC131::OperationFormat::AC18ABSA,
    new WIR_RegisterParameter( a1, WIR_Usage::def ),
    new TC_Const18_Unsigned( 16383 ) );
  ufAssert( lea1.getSize() == 4 );
  ufAssert(
    !lea1.isMemoryAccess() && !lea1.isMemoryStore() && !lea1.isMemoryLoad() &&
    !lea1.isMove() && !lea1.isCall() && !lea1.isIndirectCall() &&
    !lea1.isReturn() && !lea1.isJump() && !lea1.isConditionalJump() &&
    !lea1.isUnconditionalJump() && !lea1.isIndirectJump() &&
    !lea1.isAsmDataDirective() && !lea1.hasSideEffects() );

  WIR_Operation lea2(
    TC131::OpCode::LEA, TC131::OperationFormat::ALABSA,
    new WIR_RegisterParameter( a1, WIR_Usage::def ),
    new WIR_LabelParameter( b ) );
  ufAssert( lea2.getSize() == 4 );

  WIR_Operation lea3(
    TC131::OpCode::LEA, TC131::OperationFormat::AAC10BOA,
    new WIR_RegisterParameter( a1, WIR_Usage::def ),
    new WIR_RegisterParameter( a2, WIR_Usage::use ),
    new TC_Const10_Signed( -512 ) );
  ufAssert( lea3.getSize() == 4 );

  WIR_Operation lea4(
    TC131::OpCode::LEA, TC131::OperationFormat::AAC16BOA,
    new WIR_RegisterParameter( a1, WIR_Usage::def ),
    new WIR_RegisterParameter( a2, WIR_Usage::use ),
    new TC_Const16_Signed( -32768 ) );
  ufAssert( lea4.getSize() == 4 );

  WIR_Operation lea5(
    TC131::OpCode::LEA, TC131::OperationFormat::AALC16BOA,
    new WIR_RegisterParameter( a1, WIR_Usage::def ),
    new WIR_RegisterParameter( a2, WIR_Usage::use ),
    new WIR_LabelParameter( b ),
    new TC_Const16_Signed( -32768 ) );
  ufAssert( lea5.getSize() == 4 );

  WIR_Operation loop1(
    TC131::OpCode::LOOP, TC131::OperationFormat::AL_3,
    new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
    new WIR_LabelParameter( b ) );
  ufAssert( loop1.getSize() == 4 );
  ufAssert(
    !loop1.isMemoryAccess() && !loop1.isMemoryStore() &&
    !loop1.isMemoryLoad() && !loop1.isMove() && !loop1.isCall() &&
    !loop1.isIndirectCall() && !loop1.isReturn() && loop1.isJump() &&
    loop1.isConditionalJump() && !loop1.isUnconditionalJump() &&
    !loop1.isIndirectJump() && !loop1.isAsmDataDirective() &&
    !loop1.hasSideEffects() );

  WIR_Operation loop2(
    TC131::OpCode::LOOP, TC131::OperationFormat::SAL_2,
    new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
    new WIR_LabelParameter( b ) );
  ufAssert( loop2.getSize() == 2 );

  WIR_Operation loopu1(
    TC131::OpCode::LOOPU, TC131::OperationFormat::L,
    new WIR_LabelParameter( b ) );
  ufAssert( loopu1.getSize() == 4 );
  ufAssert(
    !loopu1.isMemoryAccess() && !loopu1.isMemoryStore() &&
    !loopu1.isMemoryLoad() && !loopu1.isMove() && !loopu1.isCall() &&
    !loopu1.isIndirectCall() && !loopu1.isReturn() && loopu1.isJump() &&
    !loopu1.isConditionalJump() && loopu1.isUnconditionalJump() &&
    !loopu1.isIndirectJump() && !loopu1.isAsmDataDirective() &&
    !loopu1.hasSideEffects() );

  WIR_Operation lt1(
    TC131::OpCode::LT, TC131::OperationFormat::DDC9_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const9_Signed( -128 ) );
  ufAssert( lt1.getSize() == 4 );
  ufAssert(
    !lt1.isMemoryAccess() && !lt1.isMemoryStore() && !lt1.isMemoryLoad() &&
    !lt1.isMove() && !lt1.isCall() && !lt1.isIndirectCall() &&
    !lt1.isReturn() && !lt1.isJump() && !lt1.isConditionalJump() &&
    !lt1.isUnconditionalJump() && !lt1.isIndirectJump() &&
    !lt1.isAsmDataDirective() && !lt1.hasSideEffects() );

  WIR_Operation lt2(
    TC131::OpCode::LT, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( lt2.getSize() == 4 );

  WIR_Operation lt3(
    TC131::OpCode::LT, TC131::OperationFormat::SIDC4,
    new WIR_RegisterParameter( tricore.D15(), WIR_Usage::def ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new TC_Const4_Signed( -5 ) );
  ufAssert( lt3.getSize() == 2 );

  WIR_Operation lt4(
    TC131::OpCode::LT, TC131::OperationFormat::SIDD,
    new WIR_RegisterParameter( tricore.D15(), WIR_Usage::def ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ) );
  ufAssert( lt4.getSize() == 2 );

  WIR_Operation lt_a1(
    TC131::OpCode::LT_A, TC131::OperationFormat::DAA,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( a1, WIR_Usage::use ),
    new WIR_RegisterParameter( a2, WIR_Usage::use ) );
  ufAssert( lt_a1.getSize() == 4 );
  ufAssert(
    !lt_a1.isMemoryAccess() && !lt_a1.isMemoryStore() &&
    !lt_a1.isMemoryLoad() && !lt_a1.isMove() && !lt_a1.isCall() &&
    !lt_a1.isIndirectCall() && !lt_a1.isReturn() && !lt_a1.isJump() &&
    !lt_a1.isConditionalJump() && !lt_a1.isUnconditionalJump() &&
    !lt_a1.isIndirectJump() && !lt_a1.isAsmDataDirective() &&
    !lt_a1.hasSideEffects() );

  WIR_Operation lt_b1(
    TC131::OpCode::LT_B, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( lt_b1.getSize() == 4 );
  ufAssert(
    !lt_b1.isMemoryAccess() && !lt_b1.isMemoryStore() &&
    !lt_b1.isMemoryLoad() && !lt_b1.isMove() && !lt_b1.isCall() &&
    !lt_b1.isIndirectCall() && !lt_b1.isReturn() && !lt_b1.isJump() &&
    !lt_b1.isConditionalJump() && !lt_b1.isUnconditionalJump() && !lt_b1.isIndirectJump() && !lt_b1.isAsmDataDirective() &&
    !lt_b1.hasSideEffects() );

  WIR_Operation lt_bu1(
    TC131::OpCode::LT_BU, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( lt_bu1.getSize() == 4 );
  ufAssert(
    !lt_bu1.isMemoryAccess() && !lt_bu1.isMemoryStore() &&
    !lt_bu1.isMemoryLoad() && !lt_bu1.isMove() && !lt_bu1.isCall() &&
    !lt_bu1.isIndirectCall() && !lt_bu1.isReturn() && !lt_bu1.isJump() &&
    !lt_bu1.isConditionalJump() && !lt_bu1.isUnconditionalJump() &&
    !lt_bu1.isIndirectJump() && !lt_bu1.isAsmDataDirective() &&
    !lt_bu1.hasSideEffects() );

  WIR_Operation lt_h1(
    TC131::OpCode::LT_H, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( lt_h1.getSize() == 4 );
  ufAssert(
    !lt_h1.isMemoryAccess() && !lt_h1.isMemoryStore() &&
    !lt_h1.isMemoryLoad() && !lt_h1.isMove() && !lt_h1.isCall() &&
    !lt_h1.isIndirectCall() && !lt_h1.isReturn() && !lt_h1.isJump() &&
    !lt_h1.isConditionalJump() && !lt_h1.isUnconditionalJump() &&
    !lt_h1.isIndirectJump() && !lt_h1.isAsmDataDirective() &&
    !lt_h1.hasSideEffects() );

  WIR_Operation lt_hu1(
    TC131::OpCode::LT_HU, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( lt_hu1.getSize() == 4 );
  ufAssert(
    !lt_hu1.isMemoryAccess() && !lt_hu1.isMemoryStore() &&
    !lt_hu1.isMemoryLoad() && !lt_hu1.isMove() && !lt_hu1.isCall() &&
    !lt_hu1.isIndirectCall() && !lt_hu1.isReturn() && !lt_hu1.isJump() &&
    !lt_hu1.isConditionalJump() && !lt_hu1.isUnconditionalJump() &&
    !lt_hu1.isIndirectJump() && !lt_hu1.isAsmDataDirective() &&
    !lt_hu1.hasSideEffects() );

  WIR_Operation lt_u1(
    TC131::OpCode::LT_U, TC131::OperationFormat::DDC9_2,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const9_Unsigned( 128 ) );
  ufAssert( lt_u1.getSize() == 4 );
  ufAssert(
    !lt_u1.isMemoryAccess() && !lt_u1.isMemoryStore() &&
    !lt_u1.isMemoryLoad() && !lt_u1.isMove() && !lt_u1.isCall() &&
    !lt_u1.isIndirectCall() && !lt_u1.isReturn() && !lt_u1.isJump() &&
    !lt_u1.isConditionalJump() && !lt_u1.isUnconditionalJump() &&
    !lt_u1.isIndirectJump() && !lt_u1.isAsmDataDirective() &&
    !lt_u1.hasSideEffects() );

  WIR_Operation lt_u2(
    TC131::OpCode::LT_U, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( lt_u2.getSize() == 4 );

  WIR_Operation lt_w1(
    TC131::OpCode::LT_W, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( lt_w1.getSize() == 4 );
  ufAssert(
    !lt_w1.isMemoryAccess() && !lt_w1.isMemoryStore() &&
    !lt_w1.isMemoryLoad() && !lt_w1.isMove() && !lt_w1.isCall() &&
    !lt_w1.isIndirectCall() && !lt_w1.isReturn() && !lt_w1.isJump() &&
    !lt_w1.isConditionalJump() && !lt_w1.isUnconditionalJump() &&
    !lt_w1.isIndirectJump() && !lt_w1.isAsmDataDirective() &&
    !lt_w1.hasSideEffects() );

  WIR_Operation lt_wu1(
    TC131::OpCode::LT_WU, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( lt_wu1.getSize() == 4 );
  ufAssert(
    !lt_wu1.isMemoryAccess() && !lt_wu1.isMemoryStore() &&
    !lt_wu1.isMemoryLoad() && !lt_wu1.isMove() && !lt_wu1.isCall() &&
    !lt_wu1.isIndirectCall() && !lt_wu1.isReturn() && !lt_wu1.isJump() &&
    !lt_wu1.isConditionalJump() && !lt_wu1.isUnconditionalJump() &&
    !lt_wu1.isIndirectJump() && !lt_wu1.isAsmDataDirective() &&
    !lt_wu1.hasSideEffects() );

  WIR_Operation madd1(
    TC131::OpCode::MADD, TC131::OperationFormat::DDDC9_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new TC_Const9_Signed( -128 ) );
  ufAssert( madd1.getSize() == 4 );
  ufAssert(
    !madd1.isMemoryAccess() && !madd1.isMemoryStore() &&
    !madd1.isMemoryLoad() && !madd1.isMove() && !madd1.isCall() &&
    !madd1.isIndirectCall() && !madd1.isReturn() && !madd1.isJump() &&
    !madd1.isConditionalJump() && !madd1.isUnconditionalJump() &&
    !madd1.isIndirectJump() && !madd1.isAsmDataDirective() &&
    !madd1.hasSideEffects() );

  WIR_Operation madd2(
    TC131::OpCode::MADD, TC131::OperationFormat::EEDC9_1,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new TC_Const9_Signed( -128 ) );
  ufAssert( madd2.getSize() == 4 );

  WIR_Operation madd3(
    TC131::OpCode::MADD, TC131::OperationFormat::DDDD,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ) );
  ufAssert( madd3.getSize() == 4 );

  WIR_Operation madd4(
    TC131::OpCode::MADD, TC131::OperationFormat::EEDD,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ) );
  ufAssert( madd4.getSize() == 4 );

  WIR_Operation madd_f1(
    TC131::OpCode::MADD_F, TC131::OperationFormat::DDDD,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ) );
  ufAssert( madd_f1.getSize() == 4 );
  ufAssert(
    !madd_f1.isMemoryAccess() && !madd_f1.isMemoryStore() &&
    !madd_f1.isMemoryLoad() && !madd_f1.isMove() && !madd_f1.isCall() &&
    !madd_f1.isIndirectCall() && !madd_f1.isReturn() && !madd_f1.isJump() &&
    !madd_f1.isConditionalJump() && !madd_f1.isUnconditionalJump() &&
    !madd_f1.isIndirectJump() && !madd_f1.isAsmDataDirective() &&
    !madd_f1.hasSideEffects() );

  WIR_Operation madd_h1(
    TC131::OpCode::MADD_H, TC131::OperationFormat::EEDDC1_3,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( madd_h1.getSize() == 4 );
  ufAssert(
    !madd_h1.isMemoryAccess() && !madd_h1.isMemoryStore() &&
    !madd_h1.isMemoryLoad() && !madd_h1.isMove() && !madd_h1.isCall() &&
    !madd_h1.isIndirectCall() && !madd_h1.isReturn() && !madd_h1.isJump() &&
    !madd_h1.isConditionalJump() && !madd_h1.isUnconditionalJump() &&
    !madd_h1.isIndirectJump() && !madd_h1.isAsmDataDirective() &&
    !madd_h1.hasSideEffects() );

  WIR_Operation madd_h2(
    TC131::OpCode::MADD_H, TC131::OperationFormat::EEDDC1_4,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( madd_h2.getSize() == 4 );

  WIR_Operation madd_h3(
    TC131::OpCode::MADD_H, TC131::OperationFormat::EEDDC1_6,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( madd_h3.getSize() == 4 );

  WIR_Operation madd_h4(
    TC131::OpCode::MADD_H, TC131::OperationFormat::EEDDC1_7,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( madd_h4.getSize() == 4 );

  WIR_Operation madd_q1(
    TC131::OpCode::MADD_Q, TC131::OperationFormat::DDDDC1_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( madd_q1.getSize() == 4 );
  ufAssert(
    !madd_q1.isMemoryAccess() && !madd_q1.isMemoryStore() &&
    !madd_q1.isMemoryLoad() && !madd_q1.isMove() && !madd_q1.isCall() &&
    !madd_q1.isIndirectCall() && !madd_q1.isReturn() && !madd_q1.isJump() &&
    !madd_q1.isConditionalJump() && !madd_q1.isUnconditionalJump() &&
    !madd_q1.isIndirectJump() && !madd_q1.isAsmDataDirective() &&
    !madd_q1.hasSideEffects() );

  WIR_Operation madd_q2(
    TC131::OpCode::MADD_Q, TC131::OperationFormat::EEDDC1_1,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( madd_q2.getSize() == 4 );

  WIR_Operation madd_q3(
    TC131::OpCode::MADD_Q, TC131::OperationFormat::DDDDC1_2,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( madd_q3.getSize() == 4 );

  WIR_Operation madd_q4(
    TC131::OpCode::MADD_Q, TC131::OperationFormat::EEDDC1_2,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( madd_q4.getSize() == 4 );

  WIR_Operation madd_q5(
    TC131::OpCode::MADD_Q, TC131::OperationFormat::DDDDC1_5,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( madd_q5.getSize() == 4 );

  WIR_Operation madd_q6(
    TC131::OpCode::MADD_Q, TC131::OperationFormat::EEDDC1_5,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( madd_q6.getSize() == 4 );

  WIR_Operation madd_q7(
    TC131::OpCode::MADD_Q, TC131::OperationFormat::DDDDC1_8,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( madd_q7.getSize() == 4 );

  WIR_Operation madd_q8(
    TC131::OpCode::MADD_Q, TC131::OperationFormat::EEDDC1_8,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( madd_q8.getSize() == 4 );

  WIR_Operation madd_q9(
    TC131::OpCode::MADD_Q, TC131::OperationFormat::DDDDC1_9,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( madd_q9.getSize() == 4 );

  WIR_Operation madd_q10(
    TC131::OpCode::MADD_Q, TC131::OperationFormat::EEDDC1_9,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( madd_q10.getSize() == 4 );

  WIR_Operation madd_u1(
    TC131::OpCode::MADD_U, TC131::OperationFormat::EEDC9_2,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new TC_Const9_Unsigned( 234 ) );
  ufAssert( madd_u1.getSize() == 4 );
  ufAssert(
    !madd_u1.isMemoryAccess() && !madd_u1.isMemoryStore() &&
    !madd_u1.isMemoryLoad() && !madd_u1.isMove() && !madd_u1.isCall() &&
    !madd_u1.isIndirectCall() && !madd_u1.isReturn() && !madd_u1.isJump() &&
    !madd_u1.isConditionalJump() && !madd_u1.isUnconditionalJump() &&
    !madd_u1.isIndirectJump() && !madd_u1.isAsmDataDirective() &&
    !madd_u1.hasSideEffects() );

  WIR_Operation madd_u2(
    TC131::OpCode::MADD_U, TC131::OperationFormat::EEDD,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ) );
  ufAssert( madd_u2.getSize() == 4 );

  WIR_Operation maddm_h1(
    TC131::OpCode::MADDM_H, TC131::OperationFormat::EEDDC1_3,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( maddm_h1.getSize() == 4 );
  ufAssert(
    !maddm_h1.isMemoryAccess() && !maddm_h1.isMemoryStore() &&
    !maddm_h1.isMemoryLoad() && !maddm_h1.isMove() && !maddm_h1.isCall() &&
    !maddm_h1.isIndirectCall() && !maddm_h1.isReturn() && !maddm_h1.isJump() &&
    !maddm_h1.isConditionalJump() && !maddm_h1.isUnconditionalJump() &&
    !maddm_h1.isIndirectJump() && !maddm_h1.isAsmDataDirective() &&
    !maddm_h1.hasSideEffects() );

  WIR_Operation maddm_h2(
    TC131::OpCode::MADDM_H, TC131::OperationFormat::EEDDC1_4,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( maddm_h2.getSize() == 4 );

  WIR_Operation maddm_h3(
    TC131::OpCode::MADDM_H, TC131::OperationFormat::EEDDC1_6,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( maddm_h3.getSize() == 4 );

  WIR_Operation maddm_h4(
    TC131::OpCode::MADDM_H, TC131::OperationFormat::EEDDC1_7,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( maddm_h4.getSize() == 4 );

  WIR_Operation maddms_h1(
    TC131::OpCode::MADDMS_H, TC131::OperationFormat::EEDDC1_3,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( maddms_h1.getSize() == 4 );
  ufAssert(
    !maddms_h1.isMemoryAccess() && !maddms_h1.isMemoryStore() &&
    !maddms_h1.isMemoryLoad() && !maddms_h1.isMove() && !maddms_h1.isCall() &&
    !maddms_h1.isIndirectCall() && !maddms_h1.isReturn() && !maddms_h1.isJump() &&
    !maddms_h1.isConditionalJump() && !maddms_h1.isUnconditionalJump() &&
    !maddms_h1.isIndirectJump() && !maddms_h1.isAsmDataDirective() &&
    !maddms_h1.hasSideEffects() );

  WIR_Operation maddms_h2(
    TC131::OpCode::MADDMS_H, TC131::OperationFormat::EEDDC1_4,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( maddms_h2.getSize() == 4 );

  WIR_Operation maddms_h3(
    TC131::OpCode::MADDMS_H, TC131::OperationFormat::EEDDC1_6,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( maddms_h3.getSize() == 4 );

  WIR_Operation maddms_h4(
    TC131::OpCode::MADDMS_H, TC131::OperationFormat::EEDDC1_7,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( maddms_h4.getSize() == 4 );

  WIR_Operation maddr_h1(
    TC131::OpCode::MADDR_H, TC131::OperationFormat::DDDDC1_3,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( maddr_h1.getSize() == 4 );
  ufAssert(
    !maddr_h1.isMemoryAccess() && !maddr_h1.isMemoryStore() &&
    !maddr_h1.isMemoryLoad() && !maddr_h1.isMove() && !maddr_h1.isCall() &&
    !maddr_h1.isIndirectCall() && !maddr_h1.isReturn() && !maddr_h1.isJump() &&
    !maddr_h1.isConditionalJump() && !maddr_h1.isUnconditionalJump() &&
    !maddr_h1.isIndirectJump() && !maddr_h1.isAsmDataDirective() &&
    !maddr_h1.hasSideEffects() );

  WIR_Operation maddr_h2(
    TC131::OpCode::MADDR_H, TC131::OperationFormat::DDDDC1_4,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( maddr_h2.getSize() == 4 );

  WIR_Operation maddr_h3(
    TC131::OpCode::MADDR_H, TC131::OperationFormat::DDDDC1_6,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( maddr_h3.getSize() == 4 );

  WIR_Operation maddr_h4(
    TC131::OpCode::MADDR_H, TC131::OperationFormat::DEDDC1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( e1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( maddr_h4.getSize() == 4 );

  WIR_Operation maddr_h5(
    TC131::OpCode::MADDR_H, TC131::OperationFormat::DDDDC1_7,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( maddr_h5.getSize() == 4 );

  WIR_Operation maddr_q1(
    TC131::OpCode::MADDR_Q, TC131::OperationFormat::DDDDC1_8,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( maddr_q1.getSize() == 4 );
  ufAssert(
    !maddr_q1.isMemoryAccess() && !maddr_q1.isMemoryStore() &&
    !maddr_q1.isMemoryLoad() && !maddr_q1.isMove() && !maddr_q1.isCall() &&
    !maddr_q1.isIndirectCall() && !maddr_q1.isReturn() && !maddr_q1.isJump() &&
    !maddr_q1.isConditionalJump() && !maddr_q1.isUnconditionalJump() &&
    !maddr_q1.isIndirectJump() && !maddr_q1.isAsmDataDirective() &&
    !maddr_q1.hasSideEffects() );

  WIR_Operation maddr_q2(
    TC131::OpCode::MADDR_Q, TC131::OperationFormat::DDDDC1_9,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( maddr_q2.getSize() == 4 );

  WIR_Operation maddrs_h1(
    TC131::OpCode::MADDRS_H, TC131::OperationFormat::DDDDC1_3,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( maddrs_h1.getSize() == 4 );
  ufAssert(
    !maddrs_h1.isMemoryAccess() && !maddrs_h1.isMemoryStore() &&
    !maddrs_h1.isMemoryLoad() && !maddrs_h1.isMove() && !maddrs_h1.isCall() &&
    !maddrs_h1.isIndirectCall() && !maddrs_h1.isReturn() &&
    !maddrs_h1.isJump() && !maddrs_h1.isConditionalJump() &&
    !maddrs_h1.isUnconditionalJump() && !maddrs_h1.isIndirectJump() &&
    !maddrs_h1.isAsmDataDirective() && !maddrs_h1.hasSideEffects() );

  WIR_Operation maddrs_h2(
    TC131::OpCode::MADDRS_H, TC131::OperationFormat::DDDDC1_4,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( maddrs_h2.getSize() == 4 );

  WIR_Operation maddrs_h3(
    TC131::OpCode::MADDRS_H, TC131::OperationFormat::DDDDC1_6,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( maddrs_h3.getSize() == 4 );

  WIR_Operation maddrs_h4(
    TC131::OpCode::MADDRS_H, TC131::OperationFormat::DEDDC1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( e1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( maddrs_h4.getSize() == 4 );

  WIR_Operation maddrs_h5(
    TC131::OpCode::MADDRS_H, TC131::OperationFormat::DDDDC1_7,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( maddrs_h5.getSize() == 4 );

  WIR_Operation maddrs_q1(
    TC131::OpCode::MADDRS_Q, TC131::OperationFormat::DDDDC1_8,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( maddrs_q1.getSize() == 4 );
  ufAssert(
    !maddrs_q1.isMemoryAccess() && !maddrs_q1.isMemoryStore() &&
    !maddrs_q1.isMemoryLoad() && !maddrs_q1.isMove() && !maddrs_q1.isCall() &&
    !maddrs_q1.isIndirectCall() && !maddrs_q1.isReturn() &&
    !maddrs_q1.isJump() && !maddrs_q1.isConditionalJump() &&
    !maddrs_q1.isUnconditionalJump() && !maddrs_q1.isIndirectJump() &&
    !maddrs_q1.isAsmDataDirective() && !maddrs_q1.hasSideEffects() );

  WIR_Operation maddrs_q2(
    TC131::OpCode::MADDRS_Q, TC131::OperationFormat::DDDDC1_9,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( maddrs_q2.getSize() == 4 );

  WIR_Operation madds1(
    TC131::OpCode::MADDS, TC131::OperationFormat::DDDC9_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new TC_Const9_Signed( -128 ) );
  ufAssert( madds1.getSize() == 4 );
  ufAssert(
    !madds1.isMemoryAccess() && !madds1.isMemoryStore() &&
    !madds1.isMemoryLoad() && !madds1.isMove() && !madds1.isCall() &&
    !madds1.isIndirectCall() && !madds1.isReturn() && !madds1.isJump() &&
    !madds1.isConditionalJump() &&
    !madds1.isUnconditionalJump() && !madds1.isIndirectJump() &&
    !madds1.isAsmDataDirective() && !madds1.hasSideEffects() );

  WIR_Operation madds2(
    TC131::OpCode::MADDS, TC131::OperationFormat::EEDC9_1,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new TC_Const9_Signed( -128 ) );
  ufAssert( madds2.getSize() == 4 );

  WIR_Operation madds3(
    TC131::OpCode::MADDS, TC131::OperationFormat::DDDD,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ) );
  ufAssert( madds3.getSize() == 4 );

  WIR_Operation madds4(
    TC131::OpCode::MADDS, TC131::OperationFormat::EEDD,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ) );
  ufAssert( madds4.getSize() == 4 );

  WIR_Operation madds_h1(
    TC131::OpCode::MADDS_H, TC131::OperationFormat::EEDDC1_3,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( madds_h1.getSize() == 4 );
  ufAssert(
    !madds_h1.isMemoryAccess() && !madds_h1.isMemoryStore() &&
    !madds_h1.isMemoryLoad() && !madds_h1.isMove() && !madds_h1.isCall() &&
    !madds_h1.isIndirectCall() && !madds_h1.isReturn() && !madds_h1.isJump() &&
    !madds_h1.isConditionalJump() && !madds_h1.isUnconditionalJump() &&
    !madds_h1.isIndirectJump() && !madds_h1.isAsmDataDirective() &&
    !madds_h1.hasSideEffects() );

  WIR_Operation madds_h2(
    TC131::OpCode::MADDS_H, TC131::OperationFormat::EEDDC1_4,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( madds_h2.getSize() == 4 );

  WIR_Operation madds_h3(
    TC131::OpCode::MADDS_H, TC131::OperationFormat::EEDDC1_6,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( madds_h3.getSize() == 4 );

  WIR_Operation madds_h4(
    TC131::OpCode::MADDS_H, TC131::OperationFormat::EEDDC1_7,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( madds_h4.getSize() == 4 );

  WIR_Operation madds_q1(
    TC131::OpCode::MADDS_Q, TC131::OperationFormat::DDDDC1_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( madds_q1.getSize() == 4 );
  ufAssert(
    !madds_q1.isMemoryAccess() && !madds_q1.isMemoryStore() &&
    !madds_q1.isMemoryLoad() && !madds_q1.isMove() && !madds_q1.isCall() &&
    !madds_q1.isIndirectCall() && !madds_q1.isReturn() && !madds_q1.isJump() &&
    !madds_q1.isConditionalJump() && !madds_q1.isUnconditionalJump() &&
    !madds_q1.isIndirectJump() && !madds_q1.isAsmDataDirective() &&
    !madds_q1.hasSideEffects() );

  WIR_Operation madds_q2(
    TC131::OpCode::MADDS_Q, TC131::OperationFormat::EEDDC1_1,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( madds_q2.getSize() == 4 );

  WIR_Operation madds_q3(
    TC131::OpCode::MADDS_Q, TC131::OperationFormat::DDDDC1_2,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( madds_q3.getSize() == 4 );

  WIR_Operation madds_q4(
    TC131::OpCode::MADDS_Q, TC131::OperationFormat::EEDDC1_2,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( madds_q4.getSize() == 4 );

  WIR_Operation madds_q5(
    TC131::OpCode::MADDS_Q, TC131::OperationFormat::DDDDC1_5,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( madds_q5.getSize() == 4 );

  WIR_Operation madds_q6(
    TC131::OpCode::MADDS_Q, TC131::OperationFormat::EEDDC1_5,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( madds_q6.getSize() == 4 );

  WIR_Operation madds_q7(
    TC131::OpCode::MADDS_Q, TC131::OperationFormat::DDDDC1_8,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( madds_q7.getSize() == 4 );

  WIR_Operation madds_q8(
    TC131::OpCode::MADDS_Q, TC131::OperationFormat::EEDDC1_8,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( madds_q8.getSize() == 4 );

  WIR_Operation madds_q9(
    TC131::OpCode::MADDS_Q, TC131::OperationFormat::DDDDC1_9,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( madds_q9.getSize() == 4 );

  WIR_Operation madds_q10(
    TC131::OpCode::MADDS_Q, TC131::OperationFormat::EEDDC1_9,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( madds_q10.getSize() == 4 );

  WIR_Operation madds_u1(
    TC131::OpCode::MADDS_U, TC131::OperationFormat::DDDC9_2,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new TC_Const9_Unsigned( 234 ) );
  ufAssert( madds_u1.getSize() == 4 );
  ufAssert(
    !madds_u1.isMemoryAccess() && !madds_u1.isMemoryStore() &&
    !madds_u1.isMemoryLoad() && !madds_u1.isMove() && !madds_u1.isCall() &&
    !madds_u1.isIndirectCall() && !madds_u1.isReturn() && !madds_u1.isJump() &&
    !madds_u1.isConditionalJump() && !madds_u1.isUnconditionalJump() &&
    !madds_u1.isIndirectJump() && !madds_u1.isAsmDataDirective() &&
    !madds_u1.hasSideEffects() );

  WIR_Operation madds_u2(
    TC131::OpCode::MADDS_U, TC131::OperationFormat::EEDC9_2,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new TC_Const9_Unsigned( 234 ) );
  ufAssert( madds_u2.getSize() == 4 );

  WIR_Operation madds_u3(
    TC131::OpCode::MADDS_U, TC131::OperationFormat::DDDD,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ) );
  ufAssert( madds_u3.getSize() == 4 );

  WIR_Operation madds_u4(
    TC131::OpCode::MADDS_U, TC131::OperationFormat::EEDD,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ) );
  ufAssert( madds_u4.getSize() == 4 );

  WIR_Operation maddsu_h1(
    TC131::OpCode::MADDSU_H, TC131::OperationFormat::EEDDC1_3,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( maddsu_h1.getSize() == 4 );
  ufAssert(
    !maddsu_h1.isMemoryAccess() && !maddsu_h1.isMemoryStore() &&
    !maddsu_h1.isMemoryLoad() && !maddsu_h1.isMove() && !maddsu_h1.isCall() &&
    !maddsu_h1.isIndirectCall() && !maddsu_h1.isReturn() &&
    !maddsu_h1.isJump() && !maddsu_h1.isConditionalJump() &&
    !maddsu_h1.isUnconditionalJump() && !maddsu_h1.isIndirectJump() &&
    !maddsu_h1.isAsmDataDirective() && !maddsu_h1.hasSideEffects() );

  WIR_Operation maddsu_h2(
    TC131::OpCode::MADDSU_H, TC131::OperationFormat::EEDDC1_4,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( maddsu_h2.getSize() == 4 );

  WIR_Operation maddsu_h3(
    TC131::OpCode::MADDSU_H, TC131::OperationFormat::EEDDC1_6,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( maddsu_h3.getSize() == 4 );

  WIR_Operation maddsu_h4(
    TC131::OpCode::MADDSU_H, TC131::OperationFormat::EEDDC1_7,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( maddsu_h4.getSize() == 4 );

  WIR_Operation maddsum_h1(
    TC131::OpCode::MADDSUM_H, TC131::OperationFormat::EEDDC1_3,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( maddsum_h1.getSize() == 4 );
  ufAssert(
    !maddsum_h1.isMemoryAccess() && !maddsum_h1.isMemoryStore() &&
    !maddsum_h1.isMemoryLoad() && !maddsum_h1.isMove() &&
    !maddsum_h1.isCall() && !maddsum_h1.isIndirectCall() &&
    !maddsum_h1.isReturn() && !maddsum_h1.isJump() &&
    !maddsum_h1.isConditionalJump() && !maddsum_h1.isUnconditionalJump() &&
    !maddsum_h1.isIndirectJump() && !maddsum_h1.isAsmDataDirective() &&
    !maddsum_h1.hasSideEffects() );

  WIR_Operation maddsum_h2(
    TC131::OpCode::MADDSUM_H, TC131::OperationFormat::EEDDC1_4,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( maddsum_h2.getSize() == 4 );

  WIR_Operation maddsum_h3(
    TC131::OpCode::MADDSUM_H, TC131::OperationFormat::EEDDC1_6,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( maddsum_h3.getSize() == 4 );

  WIR_Operation maddsum_h4(
    TC131::OpCode::MADDSUM_H, TC131::OperationFormat::EEDDC1_7,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( maddsum_h4.getSize() == 4 );

  WIR_Operation maddsums_h1(
    TC131::OpCode::MADDSUMS_H, TC131::OperationFormat::EEDDC1_3,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( maddsums_h1.getSize() == 4 );
  ufAssert(
    !maddsums_h1.isMemoryAccess() && !maddsums_h1.isMemoryStore() &&
    !maddsums_h1.isMemoryLoad() && !maddsums_h1.isMove() &&
    !maddsums_h1.isCall() && !maddsums_h1.isIndirectCall() &&
    !maddsums_h1.isReturn() && !maddsums_h1.isJump() &&
    !maddsums_h1.isConditionalJump() && !maddsums_h1.isUnconditionalJump() &&
    !maddsums_h1.isIndirectJump() && !maddsums_h1.isAsmDataDirective() &&
    !maddsums_h1.hasSideEffects() );

  WIR_Operation maddsums_h2(
    TC131::OpCode::MADDSUMS_H, TC131::OperationFormat::EEDDC1_4,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( maddsums_h2.getSize() == 4 );

  WIR_Operation maddsums_h3(
    TC131::OpCode::MADDSUMS_H, TC131::OperationFormat::EEDDC1_6,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( maddsums_h3.getSize() == 4 );

  WIR_Operation maddsums_h4(
    TC131::OpCode::MADDSUMS_H, TC131::OperationFormat::EEDDC1_7,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( maddsums_h4.getSize() == 4 );

  WIR_Operation maddsur_h1(
    TC131::OpCode::MADDSUR_H, TC131::OperationFormat::DDDDC1_3,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( maddsur_h1.getSize() == 4 );
  ufAssert(
    !maddsur_h1.isMemoryAccess() && !maddsur_h1.isMemoryStore() &&
    !maddsur_h1.isMemoryLoad() && !maddsur_h1.isMove() &&
    !maddsur_h1.isCall() && !maddsur_h1.isIndirectCall() &&
    !maddsur_h1.isReturn() && !maddsur_h1.isJump() &&
    !maddsur_h1.isConditionalJump() && !maddsur_h1.isUnconditionalJump() &&
    !maddsur_h1.isIndirectJump() && !maddsur_h1.isAsmDataDirective() &&
    !maddsur_h1.hasSideEffects() );

  WIR_Operation maddsur_h2(
    TC131::OpCode::MADDSUR_H, TC131::OperationFormat::DDDDC1_4,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( maddsur_h2.getSize() == 4 );

  WIR_Operation maddsur_h3(
    TC131::OpCode::MADDSUR_H, TC131::OperationFormat::DDDDC1_6,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( maddsur_h3.getSize() == 4 );

  WIR_Operation maddsur_h4(
    TC131::OpCode::MADDSUR_H, TC131::OperationFormat::DDDDC1_7,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( maddsur_h4.getSize() == 4 );

  WIR_Operation maddsurs_h1(
    TC131::OpCode::MADDSURS_H, TC131::OperationFormat::DDDDC1_3,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( maddsurs_h1.getSize() == 4 );
  ufAssert(
    !maddsurs_h1.isMemoryAccess() && !maddsurs_h1.isMemoryStore() &&
    !maddsurs_h1.isMemoryLoad() && !maddsurs_h1.isMove() &&
    !maddsurs_h1.isCall() && !maddsurs_h1.isIndirectCall() &&
    !maddsurs_h1.isReturn() && !maddsurs_h1.isJump() &&
    !maddsurs_h1.isConditionalJump() && !maddsurs_h1.isUnconditionalJump() &&
    !maddsurs_h1.isIndirectJump() && !maddsurs_h1.isAsmDataDirective() &&
    !maddsurs_h1.hasSideEffects() );

  WIR_Operation maddsurs_h2(
    TC131::OpCode::MADDSURS_H, TC131::OperationFormat::DDDDC1_4,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( maddsurs_h2.getSize() == 4 );

  WIR_Operation maddsurs_h3(
    TC131::OpCode::MADDSURS_H, TC131::OperationFormat::DDDDC1_6,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( maddsurs_h3.getSize() == 4 );

  WIR_Operation maddsurs_h4(
    TC131::OpCode::MADDSURS_H, TC131::OperationFormat::DDDDC1_7,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( maddsurs_h4.getSize() == 4 );

  WIR_Operation maddsus_h1(
    TC131::OpCode::MADDSUS_H, TC131::OperationFormat::EEDDC1_3,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( maddsus_h1.getSize() == 4 );
  ufAssert(
    !maddsus_h1.isMemoryAccess() && !maddsus_h1.isMemoryStore() &&
    !maddsus_h1.isMemoryLoad() && !maddsus_h1.isMove() &&
    !maddsus_h1.isCall() && !maddsus_h1.isIndirectCall() &&
    !maddsus_h1.isReturn() && !maddsus_h1.isJump() &&
    !maddsus_h1.isConditionalJump() && !maddsus_h1.isUnconditionalJump() &&
    !maddsus_h1.isIndirectJump() && !maddsus_h1.isAsmDataDirective() &&
    !maddsus_h1.hasSideEffects() );

  WIR_Operation maddsus_h2(
    TC131::OpCode::MADDSUS_H, TC131::OperationFormat::EEDDC1_4,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( maddsus_h2.getSize() == 4 );

  WIR_Operation maddsus_h3(
    TC131::OpCode::MADDSUS_H, TC131::OperationFormat::EEDDC1_6,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( maddsus_h3.getSize() == 4 );

  WIR_Operation maddsus_h4(
    TC131::OpCode::MADDSUS_H, TC131::OperationFormat::EEDDC1_7,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( maddsus_h4.getSize() == 4 );

  WIR_Operation max1(
    TC131::OpCode::MAX, TC131::OperationFormat::DDC9_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const9_Signed( -128 ) );
  ufAssert( max1.getSize() == 4 );
  ufAssert(
    !max1.isMemoryAccess() && !max1.isMemoryStore() && !max1.isMemoryLoad() &&
    !max1.isMove() && !max1.isCall() && !max1.isIndirectCall() &&
    !max1.isReturn() && !max1.isJump() && !max1.isConditionalJump() &&
    !max1.isUnconditionalJump() && !max1.isIndirectJump() &&
    !max1.isAsmDataDirective() && !max1.hasSideEffects() );

  WIR_Operation max2(
    TC131::OpCode::MAX, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( max2.getSize() == 4 );

  WIR_Operation max_b1(
    TC131::OpCode::MAX_B, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( max_b1.getSize() == 4 );
  ufAssert(
    !max_b1.isMemoryAccess() && !max_b1.isMemoryStore() &&
    !max_b1.isMemoryLoad() && !max_b1.isMove() && !max_b1.isCall() &&
    !max_b1.isIndirectCall() && !max_b1.isReturn() && !max_b1.isJump() &&
    !max_b1.isConditionalJump() && !max_b1.isUnconditionalJump() &&
    !max_b1.isIndirectJump() && !max_b1.isAsmDataDirective() &&
    !max_b1.hasSideEffects() );

  WIR_Operation max_bu1(
    TC131::OpCode::MAX_BU, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( max_bu1.getSize() == 4 );
  ufAssert(
    !max_bu1.isMemoryAccess() && !max_bu1.isMemoryStore() &&
    !max_bu1.isMemoryLoad() && !max_bu1.isMove() && !max_bu1.isCall() &&
    !max_bu1.isIndirectCall() && !max_bu1.isReturn() && !max_bu1.isJump() &&
    !max_bu1.isConditionalJump() && !max_bu1.isUnconditionalJump() &&
    !max_bu1.isIndirectJump() && !max_bu1.isAsmDataDirective() &&
    !max_bu1.hasSideEffects() );

  WIR_Operation max_h1(
    TC131::OpCode::MAX_H, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( max_h1.getSize() == 4 );
  ufAssert(
    !max_h1.isMemoryAccess() && !max_h1.isMemoryStore() &&
    !max_h1.isMemoryLoad() && !max_h1.isMove() && !max_h1.isCall() &&
    !max_h1.isIndirectCall() && !max_h1.isReturn() && !max_h1.isJump() &&
    !max_h1.isConditionalJump() && !max_h1.isUnconditionalJump() &&
    !max_h1.isIndirectJump() && !max_h1.isAsmDataDirective() &&
    !max_h1.hasSideEffects() );

  WIR_Operation max_hu1(
    TC131::OpCode::MAX_HU, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( max_hu1.getSize() == 4 );
  ufAssert(
    !max_hu1.isMemoryAccess() && !max_hu1.isMemoryStore() &&
    !max_hu1.isMemoryLoad() && !max_hu1.isMove() && !max_hu1.isCall() &&
    !max_hu1.isIndirectCall() && !max_hu1.isReturn() && !max_hu1.isJump() &&
    !max_hu1.isConditionalJump() && !max_hu1.isUnconditionalJump() &&
    !max_hu1.isIndirectJump() && !max_hu1.isAsmDataDirective() &&
    !max_hu1.hasSideEffects() );

  WIR_Operation max_u1(
    TC131::OpCode::MAX_U, TC131::OperationFormat::DDC9_2,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const9_Unsigned( 128 ) );
  ufAssert( max_u1.getSize() == 4 );
  ufAssert(
    !max_u1.isMemoryAccess() && !max_u1.isMemoryStore() &&
    !max_u1.isMemoryLoad() && !max_u1.isMove() && !max_u1.isCall() &&
    !max_u1.isIndirectCall() && !max_u1.isReturn() && !max_u1.isJump() &&
    !max_u1.isConditionalJump() && !max_u1.isUnconditionalJump() &&
    !max_u1.isIndirectJump() && !max_u1.isAsmDataDirective() &&
    !max_u1.hasSideEffects() );

  WIR_Operation max_u2(
    TC131::OpCode::MAX_U, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( max_u2.getSize() == 4 );

  WIR_Operation mfcr1(
    TC131::OpCode::MFCR, TC131::OperationFormat::DC16PSW,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new TC_Const16_Unsigned( 26361 ),
    new WIR_RegisterParameter( tricore.PSW_C(), WIR_Usage::use ) );
  ufAssert( mfcr1.getSize() == 4 );
  ufAssert(
    !mfcr1.isMemoryAccess() && !mfcr1.isMemoryStore() &&
    !mfcr1.isMemoryLoad() && !mfcr1.isMove() && !mfcr1.isCall() &&
    !mfcr1.isIndirectCall() && !mfcr1.isReturn() && !mfcr1.isJump() &&
    !mfcr1.isConditionalJump() && !mfcr1.isUnconditionalJump() &&
    !mfcr1.isIndirectJump() && !mfcr1.isAsmDataDirective() &&
    !mfcr1.hasSideEffects() );

  WIR_Operation min1(
    TC131::OpCode::MIN, TC131::OperationFormat::DDC9_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const9_Signed( -128 ) );
  ufAssert( min1.getSize() == 4 );
  ufAssert(
    !min1.isMemoryAccess() && !min1.isMemoryStore() && !min1.isMemoryLoad() &&
    !min1.isMove() && !min1.isCall() && !min1.isIndirectCall() &&
    !min1.isReturn() && !min1.isJump() && !min1.isConditionalJump() &&
    !min1.isUnconditionalJump() && !min1.isIndirectJump() &&
    !min1.isAsmDataDirective() && !min1.hasSideEffects() );

  WIR_Operation min2(
    TC131::OpCode::MIN, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( min2.getSize() == 4 );

  WIR_Operation min_b1(
    TC131::OpCode::MIN_B, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( min_b1.getSize() == 4 );
  ufAssert(
    !min_b1.isMemoryAccess() && !min_b1.isMemoryStore() &&
    !min_b1.isMemoryLoad() && !min_b1.isMove() && !min_b1.isCall() &&
    !min_b1.isIndirectCall() && !min_b1.isReturn() && !min_b1.isJump() &&
    !min_b1.isConditionalJump() && !min_b1.isUnconditionalJump() &&
    !min_b1.isIndirectJump() && !min_b1.isAsmDataDirective() &&
    !min_b1.hasSideEffects() );

  WIR_Operation min_bu1(
    TC131::OpCode::MIN_BU, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( min_bu1.getSize() == 4 );
  ufAssert(
    !min_bu1.isMemoryAccess() && !min_bu1.isMemoryStore() &&
    !min_bu1.isMemoryLoad() && !min_bu1.isMove() && !min_bu1.isCall() &&
    !min_bu1.isIndirectCall() && !min_bu1.isReturn() && !min_bu1.isJump() &&
    !min_bu1.isConditionalJump() && !min_bu1.isUnconditionalJump() &&
    !min_bu1.isIndirectJump() && !min_bu1.isAsmDataDirective() &&
    !min_bu1.hasSideEffects() );

  WIR_Operation min_h1(
    TC131::OpCode::MIN_H, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( min_h1.getSize() == 4 );
  ufAssert(
    !min_h1.isMemoryAccess() && !min_h1.isMemoryStore() &&
    !min_h1.isMemoryLoad() && !min_h1.isMove() && !min_h1.isCall() &&
    !min_h1.isIndirectCall() && !min_h1.isReturn() && !min_h1.isJump() &&
    !min_h1.isConditionalJump() && !min_h1.isUnconditionalJump() &&
    !min_h1.isIndirectJump() && !min_h1.isAsmDataDirective() &&
    !min_h1.hasSideEffects() );

  WIR_Operation min_hu1(
    TC131::OpCode::MIN_HU, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( min_hu1.getSize() == 4 );
  ufAssert(
    !min_hu1.isMemoryAccess() && !min_hu1.isMemoryStore() &&
    !min_hu1.isMemoryLoad() && !min_hu1.isMove() && !min_hu1.isCall() &&
    !min_hu1.isIndirectCall() && !min_hu1.isReturn() && !min_hu1.isJump() &&
    !min_hu1.isConditionalJump() && !min_hu1.isUnconditionalJump() &&
    !min_hu1.isIndirectJump() && !min_hu1.isAsmDataDirective() &&
    !min_hu1.hasSideEffects() );

  WIR_Operation min_u1(
    TC131::OpCode::MIN_U, TC131::OperationFormat::DDC9_2,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const9_Unsigned( 128 ) );
  ufAssert( min_u1.getSize() == 4 );
  ufAssert(
    !min_u1.isMemoryAccess() && !min_u1.isMemoryStore() &&
    !min_u1.isMemoryLoad() && !min_u1.isMove() && !min_u1.isCall() &&
    !min_u1.isIndirectCall() && !min_u1.isReturn() && !min_u1.isJump() &&
    !min_u1.isConditionalJump() && !min_u1.isUnconditionalJump() &&
    !min_u1.isIndirectJump() && !min_u1.isAsmDataDirective() &&
    !min_u1.hasSideEffects() );

  WIR_Operation min_u2(
    TC131::OpCode::MIN_U, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( min_u2.getSize() == 4 );

  WIR_Operation mov1(
    TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new TC_Const16_Signed( -26361 ) );
  ufAssert( mov1.getSize() == 4 );
  ufAssert(
    !mov1.isMemoryAccess() && !mov1.isMemoryStore() && !mov1.isMemoryLoad() &&
    !mov1.isMove() && !mov1.isCall() && !mov1.isIndirectCall() &&
    !mov1.isReturn() && !mov1.isJump() && !mov1.isConditionalJump() &&
    !mov1.isUnconditionalJump() && !mov1.isIndirectJump() &&
    !mov1.isAsmDataDirective() && !mov1.hasSideEffects() );

  WIR_Operation mov2(
    TC131::OpCode::MOV, TC131::OperationFormat::SIC8_1,
    new WIR_RegisterParameter( tricore.D15(), WIR_Usage::def ),
    new TC_Const8_Unsigned( 64 ) );
  ufAssert( mov2.getSize() == 2 );

  WIR_Operation mov3(
    TC131::OpCode::MOV, TC131::OperationFormat::SDC4_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new TC_Const4_Signed( 5 ) );
  ufAssert( mov3.getSize() == 2 );

  WIR_Operation mov_rr1(
    TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ) );
  ufAssert( mov_rr1.getSize() == 4 );
  ufAssert(
    !mov_rr1.isMemoryAccess() && !mov_rr1.isMemoryStore() &&
    !mov_rr1.isMemoryLoad() && mov_rr1.isMove() && !mov_rr1.isCall() &&
    !mov_rr1.isIndirectCall() && !mov_rr1.isReturn() && !mov_rr1.isJump() &&
    !mov_rr1.isConditionalJump() && !mov_rr1.isUnconditionalJump() &&
    !mov_rr1.isIndirectJump() && !mov_rr1.isAsmDataDirective() &&
    !mov_rr1.hasSideEffects() );

  WIR_Operation mov_rr2(
    TC131::OpCode::MOV_RR, TC131::OperationFormat::SDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ) );
  ufAssert( mov_rr2.getSize() == 2 );

  WIR_Operation mov_a1(
    TC131::OpCode::MOV_A, TC131::OperationFormat::AD,
    new WIR_RegisterParameter( a1, WIR_Usage::def ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ) );
  ufAssert( mov_a1.getSize() == 4 );
  ufAssert(
    !mov_a1.isMemoryAccess() && !mov_a1.isMemoryStore() &&
    !mov_a1.isMemoryLoad() && !mov_a1.isMove() && !mov_a1.isCall() &&
    !mov_a1.isIndirectCall() && !mov_a1.isReturn() && !mov_a1.isJump() &&
    !mov_a1.isConditionalJump() && !mov_a1.isUnconditionalJump() &&
    !mov_a1.isIndirectJump() && !mov_a1.isAsmDataDirective() &&
    !mov_a1.hasSideEffects() );

  WIR_Operation mov_a2(
    TC131::OpCode::MOV_A, TC131::OperationFormat::SAC4_1,
    new WIR_RegisterParameter( a1, WIR_Usage::def ),
    new TC_Const4_Unsigned( 7 ) );
  ufAssert( mov_a2.getSize() == 2 );

  WIR_Operation mov_a3(
    TC131::OpCode::MOV_A, TC131::OperationFormat::SAD_1,
    new WIR_RegisterParameter( a1, WIR_Usage::def ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ) );
  ufAssert( mov_a3.getSize() == 2 );

  WIR_Operation mov_aa1(
    TC131::OpCode::MOV_AA, TC131::OperationFormat::AA,
    new WIR_RegisterParameter( a1, WIR_Usage::def ),
    new WIR_RegisterParameter( a2, WIR_Usage::use ) );
  ufAssert( mov_aa1.getSize() == 4 );
  ufAssert(
    !mov_aa1.isMemoryAccess() && !mov_aa1.isMemoryStore() &&
    !mov_aa1.isMemoryLoad() && mov_aa1.isMove() && !mov_aa1.isCall() &&
    !mov_aa1.isIndirectCall() && !mov_aa1.isReturn() && !mov_aa1.isJump() &&
    !mov_aa1.isConditionalJump() && !mov_aa1.isUnconditionalJump() &&
    !mov_aa1.isIndirectJump() && !mov_aa1.isAsmDataDirective() &&
    !mov_aa1.hasSideEffects() );

  WIR_Operation mov_aa2(
    TC131::OpCode::MOV_AA, TC131::OperationFormat::SAA_1,
    new WIR_RegisterParameter( a1, WIR_Usage::def ),
    new WIR_RegisterParameter( a2, WIR_Usage::use ) );
  ufAssert( mov_aa2.getSize() == 2 );

  WIR_Operation mov_d1(
    TC131::OpCode::MOV_D, TC131::OperationFormat::DA,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( a1, WIR_Usage::use ) );
  ufAssert( mov_d1.getSize() == 4 );
  ufAssert(
    !mov_d1.isMemoryAccess() && !mov_d1.isMemoryStore() &&
    !mov_d1.isMemoryLoad() && !mov_d1.isMove() && !mov_d1.isCall() &&
    !mov_d1.isIndirectCall() && !mov_d1.isReturn() && !mov_d1.isJump() &&
    !mov_d1.isConditionalJump() && !mov_d1.isUnconditionalJump() &&
    !mov_d1.isIndirectJump() && !mov_d1.isAsmDataDirective() &&
    !mov_d1.hasSideEffects() );

  WIR_Operation mov_d2(
    TC131::OpCode::MOV_D, TC131::OperationFormat::SDA_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( a1, WIR_Usage::use ) );
  ufAssert( mov_d2.getSize() == 2 );

  WIR_Operation mov_u1(
    TC131::OpCode::MOV_U, TC131::OperationFormat::DC16_2,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new TC_Const16_Unsigned( 26361 ) );
  ufAssert( mov_u1.getSize() == 4 );
  ufAssert(
    !mov_u1.isMemoryAccess() && !mov_u1.isMemoryStore() &&
    !mov_u1.isMemoryLoad() && !mov_u1.isMove() && !mov_u1.isCall() &&
    !mov_u1.isIndirectCall() && !mov_u1.isReturn() && !mov_u1.isJump() &&
    !mov_u1.isConditionalJump() && !mov_u1.isUnconditionalJump() &&
    !mov_u1.isIndirectJump() && !mov_u1.isAsmDataDirective() &&
    !mov_u1.hasSideEffects() );

  WIR_Operation movh1(
    TC131::OpCode::MOVH, TC131::OperationFormat::DC16_2,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new TC_Const16_Unsigned( 26361 ) );
  ufAssert( movh1.getSize() == 4 );
  ufAssert(
    !movh1.isMemoryAccess() && !movh1.isMemoryStore() &&
    !movh1.isMemoryLoad() && !movh1.isMove() && !movh1.isCall() &&
    !movh1.isIndirectCall() && !movh1.isReturn() && !movh1.isJump() &&
    !movh1.isConditionalJump() && !movh1.isUnconditionalJump() &&
    !movh1.isIndirectJump() && !movh1.isAsmDataDirective() &&
    !movh1.hasSideEffects() );

  WIR_Operation movh_a1(
    TC131::OpCode::MOVH_A, TC131::OperationFormat::AC16,
    new WIR_RegisterParameter( a1, WIR_Usage::def ),
    new TC_Const16_Unsigned( 26361 ) );
  ufAssert( movh_a1.getSize() == 4 );
  ufAssert(
    !movh_a1.isMemoryAccess() && !movh_a1.isMemoryStore() &&
    !movh_a1.isMemoryLoad() && !movh_a1.isMove() && !movh_a1.isCall() &&
    !movh_a1.isIndirectCall() && !movh_a1.isReturn() && !movh_a1.isJump() &&
    !movh_a1.isConditionalJump() && !movh_a1.isUnconditionalJump() &&
    !movh_a1.isIndirectJump() && !movh_a1.isAsmDataDirective() &&
    !movh_a1.hasSideEffects() );

  WIR_Operation movh_a2(
    TC131::OpCode::MOVH_A, TC131::OperationFormat::AL_1,
    new WIR_RegisterParameter( a1, WIR_Usage::def ),
    new WIR_LabelParameter( b ) );
  ufAssert( movh_a2.getSize() == 4 );

  WIR_Operation msub1(
    TC131::OpCode::MSUB, TC131::OperationFormat::DDDC9_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new TC_Const9_Signed( -128 ) );
  ufAssert( msub1.getSize() == 4 );
  ufAssert(
    !msub1.isMemoryAccess() && !msub1.isMemoryStore() &&
    !msub1.isMemoryLoad() && !msub1.isMove() && !msub1.isCall() &&
    !msub1.isIndirectCall() && !msub1.isReturn() && !msub1.isJump() &&
    !msub1.isConditionalJump() && !msub1.isUnconditionalJump() &&
    !msub1.isIndirectJump() && !msub1.isAsmDataDirective() &&
    !msub1.hasSideEffects() );

  WIR_Operation msub2(
    TC131::OpCode::MSUB, TC131::OperationFormat::EEDC9_1,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new TC_Const9_Signed( -128 ) );
  ufAssert( msub2.getSize() == 4 );

  WIR_Operation msub3(
    TC131::OpCode::MSUB, TC131::OperationFormat::DDDD,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ) );
  ufAssert( msub3.getSize() == 4 );

  WIR_Operation msub4(
    TC131::OpCode::MSUB, TC131::OperationFormat::EEDD,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ) );
  ufAssert( msub4.getSize() == 4 );

  WIR_Operation msub_f1(
    TC131::OpCode::MSUB_F, TC131::OperationFormat::DDDD,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ) );
  ufAssert( msub_f1.getSize() == 4 );
  ufAssert(
    !msub_f1.isMemoryAccess() && !msub_f1.isMemoryStore() &&
    !msub_f1.isMemoryLoad() && !msub_f1.isMove() && !msub_f1.isCall() &&
    !msub_f1.isIndirectCall() && !msub_f1.isReturn() && !msub_f1.isJump() &&
    !msub_f1.isConditionalJump() && !msub_f1.isUnconditionalJump() &&
    !msub_f1.isIndirectJump() && !msub_f1.isAsmDataDirective() &&
    !msub_f1.hasSideEffects() );

  WIR_Operation msub_h1(
    TC131::OpCode::MSUB_H, TC131::OperationFormat::EEDDC1_3,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msub_h1.getSize() == 4 );
  ufAssert(
    !msub_h1.isMemoryAccess() && !msub_h1.isMemoryStore() &&
    !msub_h1.isMemoryLoad() && !msub_h1.isMove() && !msub_h1.isCall() &&
    !msub_h1.isIndirectCall() && !msub_h1.isReturn() && !msub_h1.isJump() &&
    !msub_h1.isConditionalJump() && !msub_h1.isUnconditionalJump() &&
    !msub_h1.isIndirectJump() && !msub_h1.isAsmDataDirective() &&
    !msub_h1.hasSideEffects() );

  WIR_Operation msub_h2(
    TC131::OpCode::MSUB_H, TC131::OperationFormat::EEDDC1_4,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msub_h2.getSize() == 4 );

  WIR_Operation msub_h3(
    TC131::OpCode::MSUB_H, TC131::OperationFormat::EEDDC1_6,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msub_h3.getSize() == 4 );

  WIR_Operation msub_h4(
    TC131::OpCode::MSUB_H, TC131::OperationFormat::EEDDC1_7,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msub_h4.getSize() == 4 );

  WIR_Operation msub_q1(
    TC131::OpCode::MSUB_Q, TC131::OperationFormat::DDDDC1_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msub_q1.getSize() == 4 );
  ufAssert(
    !msub_q1.isMemoryAccess() && !msub_q1.isMemoryStore() &&
    !msub_q1.isMemoryLoad() && !msub_q1.isMove() && !msub_q1.isCall() &&
    !msub_q1.isIndirectCall() && !msub_q1.isReturn() && !msub_q1.isJump() &&
    !msub_q1.isConditionalJump() && !msub_q1.isUnconditionalJump() &&
    !msub_q1.isIndirectJump() && !msub_q1.isAsmDataDirective() &&
    !msub_q1.hasSideEffects() );

  WIR_Operation msub_q2(
    TC131::OpCode::MSUB_Q, TC131::OperationFormat::EEDDC1_1,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msub_q2.getSize() == 4 );

  WIR_Operation msub_q3(
    TC131::OpCode::MSUB_Q, TC131::OperationFormat::DDDDC1_2,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msub_q3.getSize() == 4 );

  WIR_Operation msub_q4(
    TC131::OpCode::MSUB_Q, TC131::OperationFormat::EEDDC1_2,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msub_q4.getSize() == 4 );

  WIR_Operation msub_q5(
    TC131::OpCode::MSUB_Q, TC131::OperationFormat::DDDDC1_5,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msub_q5.getSize() == 4 );

  WIR_Operation msub_q6(
    TC131::OpCode::MSUB_Q, TC131::OperationFormat::EEDDC1_5,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msub_q6.getSize() == 4 );

  WIR_Operation msub_q7(
    TC131::OpCode::MSUB_Q, TC131::OperationFormat::DDDDC1_8,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msub_q7.getSize() == 4 );

  WIR_Operation msub_q8(
    TC131::OpCode::MSUB_Q, TC131::OperationFormat::EEDDC1_8,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msub_q8.getSize() == 4 );

  WIR_Operation msub_q9(
    TC131::OpCode::MSUB_Q, TC131::OperationFormat::DDDDC1_9,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msub_q9.getSize() == 4 );

  WIR_Operation msub_q10(
    TC131::OpCode::MSUB_Q, TC131::OperationFormat::EEDDC1_9,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msub_q10.getSize() == 4 );

  WIR_Operation msub_u1(
    TC131::OpCode::MSUB_U, TC131::OperationFormat::EEDC9_2,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new TC_Const9_Unsigned( 234 ) );
  ufAssert( msub_u1.getSize() == 4 );
  ufAssert(
    !msub_u1.isMemoryAccess() && !msub_u1.isMemoryStore() &&
    !msub_u1.isMemoryLoad() && !msub_u1.isMove() && !msub_u1.isCall() &&
    !msub_u1.isIndirectCall() && !msub_u1.isReturn() && !msub_u1.isJump() &&
    !msub_u1.isConditionalJump() && !msub_u1.isUnconditionalJump() &&
    !msub_u1.isIndirectJump() && !msub_u1.isAsmDataDirective() &&
    !msub_u1.hasSideEffects() );

  WIR_Operation msub_u2(
    TC131::OpCode::MSUB_U, TC131::OperationFormat::EEDD,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ) );
  ufAssert( msub_u2.getSize() == 4 );

  WIR_Operation msubad_h1(
    TC131::OpCode::MSUBAD_H, TC131::OperationFormat::EEDDC1_3,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msubad_h1.getSize() == 4 );
  ufAssert(
    !msubad_h1.isMemoryAccess() && !msubad_h1.isMemoryStore() &&
    !msubad_h1.isMemoryLoad() && !msubad_h1.isMove() && !msubad_h1.isCall() &&
    !msubad_h1.isIndirectCall() && !msubad_h1.isReturn() &&
    !msubad_h1.isJump() && !msubad_h1.isConditionalJump() &&
    !msubad_h1.isUnconditionalJump() && !msubad_h1.isIndirectJump() &&
    !msubad_h1.isAsmDataDirective() && !msubad_h1.hasSideEffects() );

  WIR_Operation msubad_h2(
    TC131::OpCode::MSUBAD_H, TC131::OperationFormat::EEDDC1_4,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msubad_h2.getSize() == 4 );

  WIR_Operation msubad_h3(
    TC131::OpCode::MSUBAD_H, TC131::OperationFormat::EEDDC1_6,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msubad_h3.getSize() == 4 );

  WIR_Operation msubad_h4(
    TC131::OpCode::MSUBAD_H, TC131::OperationFormat::EEDDC1_7,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msubad_h4.getSize() == 4 );

  WIR_Operation msubadm_h1(
    TC131::OpCode::MSUBADM_H, TC131::OperationFormat::EEDDC1_3,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msubadm_h1.getSize() == 4 );
  ufAssert(
    !msubadm_h1.isMemoryAccess() && !msubadm_h1.isMemoryStore() &&
    !msubadm_h1.isMemoryLoad() && !msubadm_h1.isMove() &&
    !msubadm_h1.isCall() && !msubadm_h1.isIndirectCall() &&
    !msubadm_h1.isReturn() && !msubadm_h1.isJump() &&
    !msubadm_h1.isConditionalJump() && !msubadm_h1.isUnconditionalJump() &&
    !msubadm_h1.isIndirectJump() && !msubadm_h1.isAsmDataDirective() &&
    !msubadm_h1.hasSideEffects() );

  WIR_Operation msubadm_h2(
    TC131::OpCode::MSUBADM_H, TC131::OperationFormat::EEDDC1_4,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msubadm_h2.getSize() == 4 );

  WIR_Operation msubadm_h3(
    TC131::OpCode::MSUBADM_H, TC131::OperationFormat::EEDDC1_6,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msubadm_h3.getSize() == 4 );

  WIR_Operation msubadm_h4(
    TC131::OpCode::MSUBADM_H, TC131::OperationFormat::EEDDC1_7,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msubadm_h4.getSize() == 4 );

  WIR_Operation msubadms_h1(
    TC131::OpCode::MSUBADMS_H, TC131::OperationFormat::EEDDC1_3,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msubadms_h1.getSize() == 4 );
  ufAssert(
    !msubadms_h1.isMemoryAccess() && !msubadms_h1.isMemoryStore() &&
    !msubadms_h1.isMemoryLoad() && !msubadms_h1.isMove() &&
    !msubadms_h1.isCall() && !msubadms_h1.isIndirectCall() &&
    !msubadms_h1.isReturn() && !msubadms_h1.isJump() &&
    !msubadms_h1.isConditionalJump() && !msubadms_h1.isUnconditionalJump() &&
    !msubadms_h1.isIndirectJump() && !msubadms_h1.isAsmDataDirective() &&
    !msubadms_h1.hasSideEffects() );

  WIR_Operation msubadms_h2(
    TC131::OpCode::MSUBADMS_H, TC131::OperationFormat::EEDDC1_4,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msubadms_h2.getSize() == 4 );

  WIR_Operation msubadms_h3(
    TC131::OpCode::MSUBADMS_H, TC131::OperationFormat::EEDDC1_6,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msubadms_h3.getSize() == 4 );

  WIR_Operation msubadms_h4(
    TC131::OpCode::MSUBADMS_H, TC131::OperationFormat::EEDDC1_7,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msubadms_h4.getSize() == 4 );

  WIR_Operation msubadr_h1(
    TC131::OpCode::MSUBADR_H, TC131::OperationFormat::DDDDC1_3,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msubadr_h1.getSize() == 4 );
  ufAssert(
    !msubadr_h1.isMemoryAccess() && !msubadr_h1.isMemoryStore() &&
    !msubadr_h1.isMemoryLoad() && !msubadr_h1.isMove() &&
    !msubadr_h1.isCall() && !msubadr_h1.isIndirectCall() &&
    !msubadr_h1.isReturn() && !msubadr_h1.isJump() &&
    !msubadr_h1.isConditionalJump() && !msubadr_h1.isUnconditionalJump() &&
    !msubadr_h1.isIndirectJump() && !msubadr_h1.isAsmDataDirective() &&
    !msubadr_h1.hasSideEffects() );

  WIR_Operation msubadr_h2(
    TC131::OpCode::MSUBADR_H, TC131::OperationFormat::DDDDC1_4,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msubadr_h2.getSize() == 4 );

  WIR_Operation msubadr_h3(
    TC131::OpCode::MSUBADR_H, TC131::OperationFormat::DDDDC1_6,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msubadr_h3.getSize() == 4 );

  WIR_Operation msubadr_h4(
    TC131::OpCode::MSUBADR_H, TC131::OperationFormat::DDDDC1_7,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msubadr_h4.getSize() == 4 );

  WIR_Operation msubadrs_h1(
    TC131::OpCode::MSUBADRS_H, TC131::OperationFormat::DDDDC1_3,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msubadrs_h1.getSize() == 4 );
  ufAssert(
    !msubadrs_h1.isMemoryAccess() && !msubadrs_h1.isMemoryStore() &&
    !msubadrs_h1.isMemoryLoad() && !msubadrs_h1.isMove() &&
    !msubadrs_h1.isCall() && !msubadrs_h1.isIndirectCall() &&
    !msubadrs_h1.isReturn() && !msubadrs_h1.isJump() &&
    !msubadrs_h1.isConditionalJump() && !msubadrs_h1.isUnconditionalJump() &&
    !msubadrs_h1.isIndirectJump() && !msubadrs_h1.isAsmDataDirective() &&
    !msubadrs_h1.hasSideEffects() );

  WIR_Operation msubadrs_h2(
    TC131::OpCode::MSUBADRS_H, TC131::OperationFormat::DDDDC1_4,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msubadrs_h2.getSize() == 4 );

  WIR_Operation msubadrs_h3(
    TC131::OpCode::MSUBADRS_H, TC131::OperationFormat::DDDDC1_6,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msubadrs_h3.getSize() == 4 );

  WIR_Operation msubadrs_h4(
    TC131::OpCode::MSUBADRS_H, TC131::OperationFormat::DDDDC1_7,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msubadrs_h4.getSize() == 4 );

  WIR_Operation msubads_h1(
    TC131::OpCode::MSUBADS_H, TC131::OperationFormat::EEDDC1_3,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msubads_h1.getSize() == 4 );
  ufAssert(
    !msubads_h1.isMemoryAccess() && !msubads_h1.isMemoryStore() &&
    !msubads_h1.isMemoryLoad() && !msubads_h1.isMove() &&
    !msubads_h1.isCall() && !msubads_h1.isIndirectCall() &&
    !msubads_h1.isReturn() && !msubads_h1.isJump() &&
    !msubads_h1.isConditionalJump() && !msubads_h1.isUnconditionalJump() &&
    !msubads_h1.isIndirectJump() && !msubads_h1.isAsmDataDirective() &&
    !msubads_h1.hasSideEffects() );

  WIR_Operation msubads_h2(
    TC131::OpCode::MSUBADS_H, TC131::OperationFormat::EEDDC1_4,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msubads_h2.getSize() == 4 );

  WIR_Operation msubads_h3(
    TC131::OpCode::MSUBADS_H, TC131::OperationFormat::EEDDC1_6,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msubads_h3.getSize() == 4 );

  WIR_Operation msubads_h4(
    TC131::OpCode::MSUBADS_H, TC131::OperationFormat::EEDDC1_7,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msubads_h4.getSize() == 4 );

  WIR_Operation msubm_h1(
    TC131::OpCode::MSUBM_H, TC131::OperationFormat::EEDDC1_3,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msubm_h1.getSize() == 4 );
  ufAssert(
    !msubm_h1.isMemoryAccess() && !msubm_h1.isMemoryStore() &&
    !msubm_h1.isMemoryLoad() && !msubm_h1.isMove() && !msubm_h1.isCall() &&
    !msubm_h1.isIndirectCall() && !msubm_h1.isReturn() && !msubm_h1.isJump() &&
    !msubm_h1.isConditionalJump() && !msubm_h1.isUnconditionalJump() &&
    !msubm_h1.isIndirectJump() && !msubm_h1.isAsmDataDirective() &&
    !msubm_h1.hasSideEffects() );

  WIR_Operation msubm_h2(
    TC131::OpCode::MSUBM_H, TC131::OperationFormat::EEDDC1_4,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msubm_h2.getSize() == 4 );

  WIR_Operation msubm_h3(
    TC131::OpCode::MSUBM_H, TC131::OperationFormat::EEDDC1_6,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msubm_h3.getSize() == 4 );

  WIR_Operation msubm_h4(
    TC131::OpCode::MSUBM_H, TC131::OperationFormat::EEDDC1_7,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msubm_h4.getSize() == 4 );

  WIR_Operation msubms_h1(
    TC131::OpCode::MSUBMS_H, TC131::OperationFormat::EEDDC1_3,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msubms_h1.getSize() == 4 );
  ufAssert(
    !msubms_h1.isMemoryAccess() && !msubms_h1.isMemoryStore() &&
    !msubms_h1.isMemoryLoad() && !msubms_h1.isMove() && !msubms_h1.isCall() &&
    !msubms_h1.isIndirectCall() && !msubms_h1.isReturn() &&
    !msubms_h1.isJump() && !msubms_h1.isConditionalJump() &&
    !msubms_h1.isUnconditionalJump() && !msubms_h1.isIndirectJump() &&
    !msubms_h1.isAsmDataDirective() && !msubms_h1.hasSideEffects() );

  WIR_Operation msubms_h2(
    TC131::OpCode::MSUBMS_H, TC131::OperationFormat::EEDDC1_4,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msubms_h2.getSize() == 4 );

  WIR_Operation msubms_h3(
    TC131::OpCode::MSUBMS_H, TC131::OperationFormat::EEDDC1_6,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msubms_h3.getSize() == 4 );

  WIR_Operation msubms_h4(
    TC131::OpCode::MSUBMS_H, TC131::OperationFormat::EEDDC1_7,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msubms_h4.getSize() == 4 );

  WIR_Operation msubr_h1(
    TC131::OpCode::MSUBR_H, TC131::OperationFormat::DDDDC1_3,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msubr_h1.getSize() == 4 );
  ufAssert(
    !msubr_h1.isMemoryAccess() && !msubr_h1.isMemoryStore() &&
    !msubr_h1.isMemoryLoad() && !msubr_h1.isMove() && !msubr_h1.isCall() &&
    !msubr_h1.isIndirectCall() && !msubr_h1.isReturn() && !msubr_h1.isJump() &&
    !msubr_h1.isConditionalJump() && !msubr_h1.isUnconditionalJump() &&
    !msubr_h1.isIndirectJump() && !msubr_h1.isAsmDataDirective() &&
    !msubr_h1.hasSideEffects() );

  WIR_Operation msubr_h2(
    TC131::OpCode::MSUBR_H, TC131::OperationFormat::DDDDC1_4,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msubr_h2.getSize() == 4 );

  WIR_Operation msubr_h3(
    TC131::OpCode::MSUBR_H, TC131::OperationFormat::DDDDC1_6,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msubr_h3.getSize() == 4 );

  WIR_Operation msubr_h4(
    TC131::OpCode::MSUBR_H, TC131::OperationFormat::DEDDC1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( e1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msubr_h4.getSize() == 4 );

  WIR_Operation msubr_h5(
    TC131::OpCode::MSUBR_H, TC131::OperationFormat::DDDDC1_7,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msubr_h5.getSize() == 4 );

  WIR_Operation msubr_q1(
    TC131::OpCode::MSUBR_Q, TC131::OperationFormat::DDDDC1_8,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msubr_q1.getSize() == 4 );
  ufAssert(
    !msubr_q1.isMemoryAccess() && !msubr_q1.isMemoryStore() &&
    !msubr_q1.isMemoryLoad() && !msubr_q1.isMove() && !msubr_q1.isCall() &&
    !msubr_q1.isIndirectCall() && !msubr_q1.isReturn() && !msubr_q1.isJump() &&
    !msubr_q1.isConditionalJump() && !msubr_q1.isUnconditionalJump() &&
    !msubr_q1.isIndirectJump() && !msubr_q1.isAsmDataDirective() &&
    !msubr_q1.hasSideEffects() );

  WIR_Operation msubr_q2(
    TC131::OpCode::MSUBR_Q, TC131::OperationFormat::DDDDC1_9,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msubr_q2.getSize() == 4 );

  WIR_Operation msubrs_h1(
    TC131::OpCode::MSUBRS_H, TC131::OperationFormat::DDDDC1_3,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msubrs_h1.getSize() == 4 );
  ufAssert(
    !msubrs_h1.isMemoryAccess() && !msubrs_h1.isMemoryStore() &&
    !msubrs_h1.isMemoryLoad() && !msubrs_h1.isMove() && !msubrs_h1.isCall() &&
    !msubrs_h1.isIndirectCall() && !msubrs_h1.isReturn() &&
    !msubrs_h1.isJump() && !msubrs_h1.isConditionalJump() &&
    !msubrs_h1.isUnconditionalJump() && !msubrs_h1.isIndirectJump() &&
    !msubrs_h1.isAsmDataDirective() && !msubrs_h1.hasSideEffects() );

  WIR_Operation msubrs_h2(
    TC131::OpCode::MSUBRS_H, TC131::OperationFormat::DDDDC1_4,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msubrs_h2.getSize() == 4 );

  WIR_Operation msubrs_h3(
    TC131::OpCode::MSUBRS_H, TC131::OperationFormat::DDDDC1_6,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msubrs_h3.getSize() == 4 );

  WIR_Operation msubrs_h4(
    TC131::OpCode::MSUBRS_H, TC131::OperationFormat::DEDDC1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( e1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msubrs_h4.getSize() == 4 );

  WIR_Operation msubrs_h5(
    TC131::OpCode::MSUBRS_H, TC131::OperationFormat::DDDDC1_7,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msubrs_h5.getSize() == 4 );

  WIR_Operation msubrs_q1(
    TC131::OpCode::MSUBRS_Q, TC131::OperationFormat::DDDDC1_8,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msubrs_q1.getSize() == 4 );
  ufAssert(
    !msubrs_q1.isMemoryAccess() && !msubrs_q1.isMemoryStore() &&
    !msubrs_q1.isMemoryLoad() && !msubrs_q1.isMove() && !msubrs_q1.isCall() &&
    !msubrs_q1.isIndirectCall() && !msubrs_q1.isReturn() &&
    !msubrs_q1.isJump() && !msubrs_q1.isConditionalJump() &&
    !msubrs_q1.isUnconditionalJump() && !msubrs_q1.isIndirectJump() &&
    !msubrs_q1.isAsmDataDirective() && !msubrs_q1.hasSideEffects() );

  WIR_Operation msubrs_q2(
    TC131::OpCode::MSUBRS_Q, TC131::OperationFormat::DDDDC1_9,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msubrs_q2.getSize() == 4 );

  WIR_Operation msubs1(
    TC131::OpCode::MSUBS, TC131::OperationFormat::DDDC9_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new TC_Const9_Signed( -128 ) );
  ufAssert( msubs1.getSize() == 4 );
  ufAssert(
    !msubs1.isMemoryAccess() && !msubs1.isMemoryStore() &&
    !msubs1.isMemoryLoad() && !msubs1.isMove() && !msubs1.isCall() &&
    !msubs1.isIndirectCall() && !msubs1.isReturn() && !msubs1.isJump() &&
    !msubs1.isConditionalJump() && !msubs1.isUnconditionalJump() &&
    !msubs1.isIndirectJump() && !msubs1.isAsmDataDirective() &&
    !msubs1.hasSideEffects() );

  WIR_Operation msubs2(
    TC131::OpCode::MSUBS, TC131::OperationFormat::EEDC9_1,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new TC_Const9_Signed( -128 ) );
  ufAssert( msubs2.getSize() == 4 );

  WIR_Operation msubs3(
    TC131::OpCode::MSUBS, TC131::OperationFormat::DDDD,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ) );
  ufAssert( msubs3.getSize() == 4 );

  WIR_Operation msubs4(
    TC131::OpCode::MSUBS, TC131::OperationFormat::EEDD,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ) );
  ufAssert( msubs4.getSize() == 4 );

  WIR_Operation msubs_h1(
    TC131::OpCode::MSUBS_H, TC131::OperationFormat::EEDDC1_3,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msubs_h1.getSize() == 4 );
  ufAssert(
    !msubs_h1.isMemoryAccess() && !msubs_h1.isMemoryStore() &&
    !msubs_h1.isMemoryLoad() && !msubs_h1.isMove() && !msubs_h1.isCall() &&
    !msubs_h1.isIndirectCall() && !msubs_h1.isReturn() && !msubs_h1.isJump() &&
    !msubs_h1.isConditionalJump() && !msubs_h1.isUnconditionalJump() &&
    !msubs_h1.isIndirectJump() && !msubs_h1.isAsmDataDirective() &&
    !msubs_h1.hasSideEffects() );

  WIR_Operation msubs_h2(
    TC131::OpCode::MSUBS_H, TC131::OperationFormat::EEDDC1_4,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msubs_h2.getSize() == 4 );

  WIR_Operation msubs_h3(
    TC131::OpCode::MSUBS_H, TC131::OperationFormat::EEDDC1_6,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msubs_h3.getSize() == 4 );

  WIR_Operation msubs_h4(
    TC131::OpCode::MSUBS_H, TC131::OperationFormat::EEDDC1_7,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msubs_h4.getSize() == 4 );

  WIR_Operation msubs_q1(
    TC131::OpCode::MSUBS_Q, TC131::OperationFormat::DDDDC1_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msubs_q1.getSize() == 4 );
  ufAssert(
    !msubs_q1.isMemoryAccess() && !msubs_q1.isMemoryStore() &&
    !msubs_q1.isMemoryLoad() && !msubs_q1.isMove() && !msubs_q1.isCall() &&
    !msubs_q1.isIndirectCall() && !msubs_q1.isReturn() && !msubs_q1.isJump() &&
    !msubs_q1.isConditionalJump() && !msubs_q1.isUnconditionalJump() &&
    !msubs_q1.isIndirectJump() && !msubs_q1.isAsmDataDirective() &&
    !msubs_q1.hasSideEffects() );

  WIR_Operation msubs_q2(
    TC131::OpCode::MSUBS_Q, TC131::OperationFormat::EEDDC1_1,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msubs_q2.getSize() == 4 );

  WIR_Operation msubs_q3(
    TC131::OpCode::MSUBS_Q, TC131::OperationFormat::DDDDC1_2,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msubs_q3.getSize() == 4 );

  WIR_Operation msubs_q4(
    TC131::OpCode::MSUBS_Q, TC131::OperationFormat::EEDDC1_2,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msubs_q4.getSize() == 4 );

  WIR_Operation msubs_q5(
    TC131::OpCode::MSUBS_Q, TC131::OperationFormat::DDDDC1_5,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msubs_q5.getSize() == 4 );

  WIR_Operation msubs_q6(
    TC131::OpCode::MSUBS_Q, TC131::OperationFormat::EEDDC1_5,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msubs_q6.getSize() == 4 );

  WIR_Operation msubs_q7(
    TC131::OpCode::MSUBS_Q, TC131::OperationFormat::DDDDC1_8,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msubs_q7.getSize() == 4 );

  WIR_Operation msubs_q8(
    TC131::OpCode::MSUBS_Q, TC131::OperationFormat::EEDDC1_8,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msubs_q8.getSize() == 4 );

  WIR_Operation msubs_q9(
    TC131::OpCode::MSUBS_Q, TC131::OperationFormat::DDDDC1_9,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msubs_q9.getSize() == 4 );

  WIR_Operation msubs_q10(
    TC131::OpCode::MSUBS_Q, TC131::OperationFormat::EEDDC1_9,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( msubs_q10.getSize() == 4 );

  WIR_Operation msubs_u1(
    TC131::OpCode::MSUBS_U, TC131::OperationFormat::DDDC9_2,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new TC_Const9_Unsigned( 234 ) );
  ufAssert( msubs_u1.getSize() == 4 );
  ufAssert(
    !msubs_u1.isMemoryAccess() && !msubs_u1.isMemoryStore() &&
    !msubs_u1.isMemoryLoad() && !msubs_u1.isMove() && !msubs_u1.isCall() &&
    !msubs_u1.isIndirectCall() && !msubs_u1.isReturn() && !msubs_u1.isJump() &&
    !msubs_u1.isConditionalJump() && !msubs_u1.isUnconditionalJump() &&
    !msubs_u1.isIndirectJump() && !msubs_u1.isAsmDataDirective() &&
    !msubs_u1.hasSideEffects() );

  WIR_Operation msubs_u2(
    TC131::OpCode::MSUBS_U, TC131::OperationFormat::EEDC9_2,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new TC_Const9_Unsigned( 234 ) );
  ufAssert( msubs_u2.getSize() == 4 );

  WIR_Operation msubs_u3(
    TC131::OpCode::MSUBS_U, TC131::OperationFormat::DDDD,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ) );
  ufAssert( msubs_u3.getSize() == 4 );

  WIR_Operation msubs_u4(
    TC131::OpCode::MSUBS_U, TC131::OperationFormat::EEDD,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( e2, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ) );
  ufAssert( msubs_u4.getSize() == 4 );

  WIR_Operation mtcr1(
    TC131::OpCode::MTCR, TC131::OperationFormat::C16DPSW,
    new TC_Const16_Unsigned( 26361 ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( tricore.PSW_C(), WIR_Usage::def ) );
  ufAssert( mtcr1.getSize() == 4 );
  ufAssert(
    !mtcr1.isMemoryAccess() && !mtcr1.isMemoryStore() &&
    !mtcr1.isMemoryLoad() && !mtcr1.isMove() && !mtcr1.isCall() &&
    !mtcr1.isIndirectCall() && !mtcr1.isReturn() && !mtcr1.isJump() &&
    !mtcr1.isConditionalJump() && !mtcr1.isUnconditionalJump() &&
    !mtcr1.isIndirectJump() && !mtcr1.isAsmDataDirective() &&
    mtcr1.hasSideEffects() );

  WIR_Operation mul1(
    TC131::OpCode::MUL, TC131::OperationFormat::DDC9_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const9_Signed( -128 ) );
  ufAssert( mul1.getSize() == 4 );
  ufAssert(
    !mul1.isMemoryAccess() && !mul1.isMemoryStore() && !mul1.isMemoryLoad() &&
    !mul1.isMove() && !mul1.isCall() && !mul1.isIndirectCall() &&
    !mul1.isReturn() && !mul1.isJump() && !mul1.isConditionalJump() &&
    !mul1.isUnconditionalJump() && !mul1.isIndirectJump() &&
    !mul1.isAsmDataDirective() && !mul1.hasSideEffects() );

  WIR_Operation mul2(
    TC131::OpCode::MUL, TC131::OperationFormat::EDC9_1,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new TC_Const9_Signed( -128 ) );
  ufAssert( mul2.getSize() == 4 );

  WIR_Operation mul3(
    TC131::OpCode::MUL, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( mul3.getSize() == 4 );

  WIR_Operation mul4(
    TC131::OpCode::MUL, TC131::OperationFormat::EDD,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ) );
  ufAssert( mul4.getSize() == 4 );

  WIR_Operation mul5(
    TC131::OpCode::MUL, TC131::OperationFormat::SDD_2,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ) );
  ufAssert( mul5.getSize() == 2 );

  WIR_Operation mul_f1(
    TC131::OpCode::MUL_F, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( mul_f1.getSize() == 4 );
  ufAssert(
    !mul_f1.isMemoryAccess() && !mul_f1.isMemoryStore() &&
    !mul_f1.isMemoryLoad() && !mul_f1.isMove() && !mul_f1.isCall() &&
    !mul_f1.isIndirectCall() && !mul_f1.isReturn() && !mul_f1.isJump() &&
    !mul_f1.isConditionalJump() && !mul_f1.isUnconditionalJump() &&
    !mul_f1.isIndirectJump() && !mul_f1.isAsmDataDirective() &&
    !mul_f1.hasSideEffects() );

  WIR_Operation mul_h1(
    TC131::OpCode::MUL_H, TC131::OperationFormat::EDDC1_3,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( mul_h1.getSize() == 4 );
  ufAssert(
    !mul_h1.isMemoryAccess() && !mul_h1.isMemoryStore() &&
    !mul_h1.isMemoryLoad() && !mul_h1.isMove() && !mul_h1.isCall() &&
    !mul_h1.isIndirectCall() && !mul_h1.isReturn() && !mul_h1.isJump() &&
    !mul_h1.isConditionalJump() && !mul_h1.isUnconditionalJump() &&
    !mul_h1.isIndirectJump() && !mul_h1.isAsmDataDirective() &&
    !mul_h1.hasSideEffects() );

  WIR_Operation mul_h2(
    TC131::OpCode::MUL_H, TC131::OperationFormat::EDDC1_4,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( mul_h2.getSize() == 4 );

  WIR_Operation mul_h3(
    TC131::OpCode::MUL_H, TC131::OperationFormat::EDDC1_6,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( mul_h3.getSize() == 4 );

  WIR_Operation mul_h4(
    TC131::OpCode::MUL_H, TC131::OperationFormat::EDDC1_7,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( mul_h4.getSize() == 4 );

  WIR_Operation mul_q1(
    TC131::OpCode::MUL_Q, TC131::OperationFormat::DDDC1_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( mul_q1.getSize() == 4 );
  ufAssert(
    !mul_q1.isMemoryAccess() && !mul_q1.isMemoryStore() &&
    !mul_q1.isMemoryLoad() && !mul_q1.isMove() && !mul_q1.isCall() &&
    !mul_q1.isIndirectCall() && !mul_q1.isReturn() && !mul_q1.isJump() &&
    !mul_q1.isConditionalJump() && !mul_q1.isUnconditionalJump() &&
    !mul_q1.isIndirectJump() && !mul_q1.isAsmDataDirective() &&
    !mul_q1.hasSideEffects() );

  WIR_Operation mul_q2(
    TC131::OpCode::MUL_Q, TC131::OperationFormat::EDDC1_1,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( mul_q2.getSize() == 4 );

  WIR_Operation mul_q3(
    TC131::OpCode::MUL_Q, TC131::OperationFormat::DDDC1_2,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( mul_q3.getSize() == 4 );

  WIR_Operation mul_q4(
    TC131::OpCode::MUL_Q, TC131::OperationFormat::EDDC1_2,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( mul_q4.getSize() == 4 );

  WIR_Operation mul_q5(
    TC131::OpCode::MUL_Q, TC131::OperationFormat::DDDC1_5,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( mul_q5.getSize() == 4 );

  WIR_Operation mul_q6(
    TC131::OpCode::MUL_Q, TC131::OperationFormat::EDDC1_5,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( mul_q6.getSize() == 4 );

  WIR_Operation mul_q7(
    TC131::OpCode::MUL_Q, TC131::OperationFormat::DDDC1_8,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( mul_q7.getSize() == 4 );

  WIR_Operation mul_q8(
    TC131::OpCode::MUL_Q, TC131::OperationFormat::DDDC1_9,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( mul_q8.getSize() == 4 );

  WIR_Operation mul_u1(
    TC131::OpCode::MUL_U, TC131::OperationFormat::EDC9_2,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new TC_Const9_Unsigned( 128 ) );
  ufAssert( mul_u1.getSize() == 4 );
  ufAssert(
    !mul_u1.isMemoryAccess() && !mul_u1.isMemoryStore() &&
    !mul_u1.isMemoryLoad() && !mul_u1.isMove() && !mul_u1.isCall() &&
    !mul_u1.isIndirectCall() && !mul_u1.isReturn() && !mul_u1.isJump() &&
    !mul_u1.isConditionalJump() && !mul_u1.isUnconditionalJump() &&
    !mul_u1.isIndirectJump() && !mul_u1.isAsmDataDirective() &&
    !mul_u1.hasSideEffects() );

  WIR_Operation mul_u2(
    TC131::OpCode::MUL_U, TC131::OperationFormat::EDD,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ) );
  ufAssert( mul_u2.getSize() == 4 );

  WIR_Operation mulm_h1(
    TC131::OpCode::MULM_H, TC131::OperationFormat::EDDC1_3,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( mulm_h1.getSize() == 4 );
  ufAssert(
    !mulm_h1.isMemoryAccess() && !mulm_h1.isMemoryStore() &&
    !mulm_h1.isMemoryLoad() && !mulm_h1.isMove() && !mulm_h1.isCall() &&
    !mulm_h1.isIndirectCall() && !mulm_h1.isReturn() && !mulm_h1.isJump() &&
    !mulm_h1.isConditionalJump() && !mulm_h1.isUnconditionalJump() &&
    !mulm_h1.isIndirectJump() && !mulm_h1.isAsmDataDirective() &&
    !mulm_h1.hasSideEffects() );

  WIR_Operation mulm_h2(
    TC131::OpCode::MULM_H, TC131::OperationFormat::EDDC1_4,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( mulm_h2.getSize() == 4 );

  WIR_Operation mulm_h3(
    TC131::OpCode::MULM_H, TC131::OperationFormat::EDDC1_6,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( mulm_h3.getSize() == 4 );

  WIR_Operation mulm_h4(
    TC131::OpCode::MULM_H, TC131::OperationFormat::EDDC1_7,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( mulm_h4.getSize() == 4 );

  WIR_Operation mulr_h1(
    TC131::OpCode::MULR_H, TC131::OperationFormat::DDDC1_3,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( mulr_h1.getSize() == 4 );
  ufAssert(
    !mulr_h1.isMemoryAccess() && !mulr_h1.isMemoryStore() &&
    !mulr_h1.isMemoryLoad() && !mulr_h1.isMove() && !mulr_h1.isCall() &&
    !mulr_h1.isIndirectCall() && !mulr_h1.isReturn() && !mulr_h1.isJump() &&
    !mulr_h1.isConditionalJump() && !mulr_h1.isUnconditionalJump() &&
    !mulr_h1.isIndirectJump() && !mulr_h1.isAsmDataDirective() &&
    !mulr_h1.hasSideEffects() );

  WIR_Operation mulr_h2(
    TC131::OpCode::MULR_H, TC131::OperationFormat::DDDC1_4,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( mulr_h2.getSize() == 4 );

  WIR_Operation mulr_h3(
    TC131::OpCode::MULR_H, TC131::OperationFormat::DDDC1_6,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( mulr_h3.getSize() == 4 );

  WIR_Operation mulr_h4(
    TC131::OpCode::MULR_H, TC131::OperationFormat::DDDC1_7,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( mulr_h4.getSize() == 4 );

  WIR_Operation mulr_q1(
    TC131::OpCode::MULR_Q, TC131::OperationFormat::DDDC1_8,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( mulr_q1.getSize() == 4 );
  ufAssert(
    !mulr_q1.isMemoryAccess() && !mulr_q1.isMemoryStore() &&
    !mulr_q1.isMemoryLoad() && !mulr_q1.isMove() && !mulr_q1.isCall() &&
    !mulr_q1.isIndirectCall() && !mulr_q1.isReturn() && !mulr_q1.isJump() &&
    !mulr_q1.isConditionalJump() && !mulr_q1.isUnconditionalJump() &&
    !mulr_q1.isIndirectJump() && !mulr_q1.isAsmDataDirective() &&
    !mulr_q1.hasSideEffects() );

  WIR_Operation mulr_q2(
    TC131::OpCode::MULR_Q, TC131::OperationFormat::DDDC1_9,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new TC_Const1_Unsigned( 1 ) );
  ufAssert( mulr_q2.getSize() == 4 );

  WIR_Operation muls1(
    TC131::OpCode::MULS, TC131::OperationFormat::DDC9_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const9_Signed( -128 ) );
  ufAssert( muls1.getSize() == 4 );
  ufAssert(
    !muls1.isMemoryAccess() && !muls1.isMemoryStore() &&
    !muls1.isMemoryLoad() && !muls1.isMove() && !muls1.isCall() &&
    !muls1.isIndirectCall() && !muls1.isReturn() && !muls1.isJump() &&
    !muls1.isConditionalJump() && !muls1.isUnconditionalJump() &&
    !muls1.isIndirectJump() && !muls1.isAsmDataDirective() &&
    !muls1.hasSideEffects() );

  WIR_Operation muls2(
    TC131::OpCode::MULS, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( muls2.getSize() == 4 );

  WIR_Operation muls_u1(
    TC131::OpCode::MULS_U, TC131::OperationFormat::DDC9_2,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const9_Unsigned( 128 ) );
  ufAssert( muls_u1.getSize() == 4 );
  ufAssert(
    !muls_u1.isMemoryAccess() && !muls_u1.isMemoryStore() &&
    !muls_u1.isMemoryLoad() && !muls_u1.isMove() && !muls_u1.isCall() &&
    !muls_u1.isIndirectCall() && !muls_u1.isReturn() && !muls_u1.isJump() &&
    !muls_u1.isConditionalJump() && !muls_u1.isUnconditionalJump() &&
    !muls_u1.isIndirectJump() && !muls_u1.isAsmDataDirective() &&
    !muls_u1.hasSideEffects() );

  WIR_Operation muls_u2(
    TC131::OpCode::MULS_U, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( muls_u2.getSize() == 4 );

  WIR_Operation nand1(
    TC131::OpCode::NAND, TC131::OperationFormat::DDC9_2,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const9_Unsigned( 128 ) );
  ufAssert( nand1.getSize() == 4 );
  ufAssert(
    !nand1.isMemoryAccess() && !nand1.isMemoryStore() &&
    !nand1.isMemoryLoad() && !nand1.isMove() && !nand1.isCall() &&
    !nand1.isIndirectCall() && !nand1.isReturn() && !nand1.isJump() &&
    !nand1.isConditionalJump() && !nand1.isUnconditionalJump() &&
    !nand1.isIndirectJump() && !nand1.isAsmDataDirective() &&
    !nand1.hasSideEffects() );

  WIR_Operation nand2(
    TC131::OpCode::NAND, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( nand2.getSize() == 4 );

  WIR_Operation nand_t1(
    TC131::OpCode::NAND_T, TC131::OperationFormat::DDC5DC5_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const5_Unsigned( 13 ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new TC_Const5_Unsigned( 27 ) );
  ufAssert( nand_t1.getSize() == 4 );
  ufAssert(
    !nand_t1.isMemoryAccess() && !nand_t1.isMemoryStore() &&
    !nand_t1.isMemoryLoad() && !nand_t1.isMove() && !nand_t1.isCall() &&
    !nand_t1.isIndirectCall() && !nand_t1.isReturn() && !nand_t1.isJump() &&
    !nand_t1.isConditionalJump() && !nand_t1.isUnconditionalJump() &&
    !nand_t1.isIndirectJump() && !nand_t1.isAsmDataDirective() &&
    !nand_t1.hasSideEffects() );

  WIR_Operation ne1(
    TC131::OpCode::NE, TC131::OperationFormat::DDC9_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const9_Signed( -128 ) );
  ufAssert( ne1.getSize() == 4 );
  ufAssert(
    !ne1.isMemoryAccess() && !ne1.isMemoryStore() && !ne1.isMemoryLoad() &&
    !ne1.isMove() && !ne1.isCall() && !ne1.isIndirectCall() &&
    !ne1.isReturn() && !ne1.isJump() && !ne1.isConditionalJump() &&
    !ne1.isUnconditionalJump() && !ne1.isIndirectJump() &&
    !ne1.isAsmDataDirective() && !ne1.hasSideEffects() );

  WIR_Operation ne2(
    TC131::OpCode::NE, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( ne2.getSize() == 4 );

  WIR_Operation ne_a1(
    TC131::OpCode::NE_A, TC131::OperationFormat::DAA,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( a1, WIR_Usage::use ),
    new WIR_RegisterParameter( a2, WIR_Usage::use ) );
  ufAssert( ne_a1.getSize() == 4 );
  ufAssert(
    !ne_a1.isMemoryAccess() && !ne_a1.isMemoryStore() &&
    !ne_a1.isMemoryLoad() && !ne_a1.isMove() && !ne_a1.isCall() &&
    !ne_a1.isIndirectCall() && !ne_a1.isReturn() && !ne_a1.isJump() &&
    !ne_a1.isConditionalJump() && !ne_a1.isUnconditionalJump() &&
    !ne_a1.isIndirectJump() && !ne_a1.isAsmDataDirective() &&
    !ne_a1.hasSideEffects() );

  WIR_Operation nez_a1(
    TC131::OpCode::NEZ_A, TC131::OperationFormat::DA,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( a1, WIR_Usage::use ) );
  ufAssert( nez_a1.getSize() == 4 );
  ufAssert(
    !nez_a1.isMemoryAccess() && !nez_a1.isMemoryStore() &&
    !nez_a1.isMemoryLoad() && !nez_a1.isMove() && !nez_a1.isCall() &&
    !nez_a1.isIndirectCall() && !nez_a1.isReturn() && !nez_a1.isJump() &&
    !nez_a1.isConditionalJump() && !nez_a1.isUnconditionalJump() &&
    !nez_a1.isIndirectJump() && !nez_a1.isAsmDataDirective() &&
    !nez_a1.hasSideEffects() );

  WIR_Operation nop1(
    TC131::OpCode::NOP, TC131::OperationFormat::SYS );
  ufAssert( nop1.getSize() == 4 );
  ufAssert(
    !nop1.isMemoryAccess() && !nop1.isMemoryStore() && !nop1.isMemoryLoad() &&
    !nop1.isMove() && !nop1.isCall() && !nop1.isIndirectCall() &&
    !nop1.isReturn() && !nop1.isJump() && !nop1.isConditionalJump() &&
    !nop1.isUnconditionalJump() && !nop1.isIndirectJump() &&
    !nop1.isAsmDataDirective() && !nop1.hasSideEffects() );

  WIR_Operation nop2(
    TC131::OpCode::NOP, TC131::OperationFormat::S );
  ufAssert( nop2.getSize() == 2 );

  WIR_Operation nor1(
    TC131::OpCode::NOR, TC131::OperationFormat::DDC9_2,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const9_Unsigned( 128 ) );
  ufAssert( nor1.getSize() == 4 );
  ufAssert(
    !nor1.isMemoryAccess() && !nor1.isMemoryStore() && !nor1.isMemoryLoad() &&
    !nor1.isMove() && !nor1.isCall() && !nor1.isIndirectCall() &&
    !nor1.isReturn() && !nor1.isJump() && !nor1.isConditionalJump() &&
    !nor1.isUnconditionalJump() && !nor1.isIndirectJump() &&
    !nor1.isAsmDataDirective() && !nor1.hasSideEffects() );

  WIR_Operation nor2(
    TC131::OpCode::NOR, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( nor2.getSize() == 4 );

  WIR_Operation nor_t1(
    TC131::OpCode::NOR_T, TC131::OperationFormat::DDC5DC5_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const5_Unsigned( 13 ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new TC_Const5_Unsigned( 27 ) );
  ufAssert( nor_t1.getSize() == 4 );
  ufAssert(
    !nor_t1.isMemoryAccess() && !nor_t1.isMemoryStore() &&
    !nor_t1.isMemoryLoad() && !nor_t1.isMove() && !nor_t1.isCall() &&
    !nor_t1.isIndirectCall() && !nor_t1.isReturn() && !nor_t1.isJump() &&
    !nor_t1.isConditionalJump() && !nor_t1.isUnconditionalJump() &&
    !nor_t1.isIndirectJump() && !nor_t1.isAsmDataDirective() &&
    !nor_t1.hasSideEffects() );

  WIR_Operation not1(
    TC131::OpCode::NOT, TC131::OperationFormat::SD,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ) );
  ufAssert( not1.getSize() == 2 );
  ufAssert(
    !not1.isMemoryAccess() && !not1.isMemoryStore() && !not1.isMemoryLoad() &&
    !not1.isMove() && !not1.isCall() && !not1.isIndirectCall() &&
    !not1.isReturn() && !not1.isJump() && !not1.isConditionalJump() &&
    !not1.isUnconditionalJump() && !not1.isIndirectJump() &&
    !not1.isAsmDataDirective() && !not1.hasSideEffects() );

  WIR_Operation or1(
    TC131::OpCode::OR, TC131::OperationFormat::DDC9_2,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const9_Unsigned( 128 ) );
  ufAssert( or1.getSize() == 4 );
  ufAssert(
    !or1.isMemoryAccess() && !or1.isMemoryStore() && !or1.isMemoryLoad() &&
    !or1.isMove() && !or1.isCall() && !or1.isIndirectCall() &&
    !or1.isReturn() && !or1.isJump() && !or1.isConditionalJump() &&
    !or1.isUnconditionalJump() && !or1.isIndirectJump() &&
    !or1.isAsmDataDirective() && !or1.hasSideEffects() );

  WIR_Operation or2(
    TC131::OpCode::OR, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( or2.getSize() == 4 );

  WIR_Operation or3(
    TC131::OpCode::OR, TC131::OperationFormat::SIC8_2,
    new WIR_RegisterParameter( tricore.D15(), WIR_Usage::defuse ),
    new TC_Const8_Unsigned( 64 ) );
  ufAssert( or3.getSize() == 2 );

  WIR_Operation or4(
    TC131::OpCode::OR, TC131::OperationFormat::SDD_2,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ) );
  ufAssert( or4.getSize() == 2 );

  WIR_Operation or_and_t1(
    TC131::OpCode::OR_AND_T, TC131::OperationFormat::DDC5DC5_2,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const5_Unsigned( 13 ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new TC_Const5_Unsigned( 27 ) );
  ufAssert( or_and_t1.getSize() == 4 );
  ufAssert(
    !or_and_t1.isMemoryAccess() && !or_and_t1.isMemoryStore() &&
    !or_and_t1.isMemoryLoad() && !or_and_t1.isMove() && !or_and_t1.isCall() &&
    !or_and_t1.isIndirectCall() && !or_and_t1.isReturn() &&
    !or_and_t1.isJump() && !or_and_t1.isConditionalJump() &&
    !or_and_t1.isUnconditionalJump() && !or_and_t1.isIndirectJump() &&
    !or_and_t1.isAsmDataDirective() && !or_and_t1.hasSideEffects() );

  WIR_Operation or_andn_t1(
    TC131::OpCode::OR_ANDN_T, TC131::OperationFormat::DDC5DC5_2,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const5_Unsigned( 13 ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new TC_Const5_Unsigned( 27 ) );
  ufAssert( or_andn_t1.getSize() == 4 );
  ufAssert(
    !or_andn_t1.isMemoryAccess() && !or_andn_t1.isMemoryStore() &&
    !or_andn_t1.isMemoryLoad() && !or_andn_t1.isMove() &&
    !or_andn_t1.isCall() && !or_andn_t1.isIndirectCall() &&
    !or_andn_t1.isReturn() && !or_andn_t1.isJump() &&
    !or_andn_t1.isConditionalJump() && !or_andn_t1.isUnconditionalJump() &&
    !or_andn_t1.isIndirectJump() && !or_andn_t1.isAsmDataDirective() &&
    !or_andn_t1.hasSideEffects() );

  WIR_Operation or_eq1(
    TC131::OpCode::OR_EQ, TC131::OperationFormat::DDC9_3,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const9_Signed( -128 ) );
  ufAssert( or_eq1.getSize() == 4 );
  ufAssert(
    !or_eq1.isMemoryAccess() && !or_eq1.isMemoryStore() &&
    !or_eq1.isMemoryLoad() && !or_eq1.isMove() && !or_eq1.isCall() &&
    !or_eq1.isIndirectCall() && !or_eq1.isReturn() && !or_eq1.isJump() &&
    !or_eq1.isConditionalJump() && !or_eq1.isUnconditionalJump() &&
    !or_eq1.isIndirectJump() && !or_eq1.isAsmDataDirective() &&
    !or_eq1.hasSideEffects() );

  WIR_Operation or_eq2(
    TC131::OpCode::OR_EQ, TC131::OperationFormat::DDD_2,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( or_eq2.getSize() == 4 );

  WIR_Operation or_ge1(
    TC131::OpCode::OR_GE, TC131::OperationFormat::DDC9_3,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const9_Signed( -128 ) );
  ufAssert( or_ge1.getSize() == 4 );
  ufAssert(
    !or_ge1.isMemoryAccess() && !or_ge1.isMemoryStore() &&
    !or_ge1.isMemoryLoad() && !or_ge1.isMove() && !or_ge1.isCall() &&
    !or_ge1.isIndirectCall() && !or_ge1.isReturn() && !or_ge1.isJump() &&
    !or_ge1.isConditionalJump() && !or_ge1.isUnconditionalJump() &&
    !or_ge1.isIndirectJump() && !or_ge1.isAsmDataDirective() &&
    !or_ge1.hasSideEffects() );

  WIR_Operation or_ge2(
    TC131::OpCode::OR_GE, TC131::OperationFormat::DDD_2,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( or_ge2.getSize() == 4 );

  WIR_Operation or_ge_u1(
    TC131::OpCode::OR_GE_U, TC131::OperationFormat::DDC9_4,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const9_Unsigned( 128 ) );
  ufAssert( or_ge_u1.getSize() == 4 );
  ufAssert(
    !or_ge_u1.isMemoryAccess() && !or_ge_u1.isMemoryStore() &&
    !or_ge_u1.isMemoryLoad() && !or_ge_u1.isMove() && !or_ge_u1.isCall() &&
    !or_ge_u1.isIndirectCall() && !or_ge_u1.isReturn() && !or_ge_u1.isJump() &&
    !or_ge_u1.isConditionalJump() && !or_ge_u1.isUnconditionalJump() &&
    !or_ge_u1.isIndirectJump() && !or_ge_u1.isAsmDataDirective() &&
    !or_ge_u1.hasSideEffects() );

  WIR_Operation or_ge_u2(
    TC131::OpCode::OR_GE_U, TC131::OperationFormat::DDD_2,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( or_ge_u2.getSize() == 4 );

  WIR_Operation or_lt1(
    TC131::OpCode::OR_LT, TC131::OperationFormat::DDC9_3,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const9_Signed( -128 ) );
  ufAssert( or_lt1.getSize() == 4 );
  ufAssert(
    !or_lt1.isMemoryAccess() && !or_lt1.isMemoryStore() &&
    !or_lt1.isMemoryLoad() && !or_lt1.isMove() && !or_lt1.isCall() &&
    !or_lt1.isIndirectCall() && !or_lt1.isReturn() && !or_lt1.isJump() &&
    !or_lt1.isConditionalJump() && !or_lt1.isUnconditionalJump() &&
    !or_lt1.isIndirectJump() && !or_lt1.isAsmDataDirective() &&
    !or_lt1.hasSideEffects() );

  WIR_Operation or_lt2(
    TC131::OpCode::OR_LT, TC131::OperationFormat::DDD_2,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( or_lt2.getSize() == 4 );

  WIR_Operation or_lt_u1(
    TC131::OpCode::OR_LT_U, TC131::OperationFormat::DDC9_4,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const9_Unsigned( 128 ) );
  ufAssert( or_lt_u1.getSize() == 4 );
  ufAssert(
    !or_lt_u1.isMemoryAccess() && !or_lt_u1.isMemoryStore() &&
    !or_lt_u1.isMemoryLoad() && !or_lt_u1.isMove() && !or_lt_u1.isCall() &&
    !or_lt_u1.isIndirectCall() && !or_lt_u1.isReturn() && !or_lt_u1.isJump() &&
    !or_lt_u1.isConditionalJump() && !or_lt_u1.isUnconditionalJump() &&
    !or_lt_u1.isIndirectJump() && !or_lt_u1.isAsmDataDirective() &&
    !or_lt_u1.hasSideEffects() );

  WIR_Operation or_lt_u2(
    TC131::OpCode::OR_LT_U, TC131::OperationFormat::DDD_2,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( or_lt_u2.getSize() == 4 );

  WIR_Operation or_ne1(
    TC131::OpCode::OR_NE, TC131::OperationFormat::DDC9_3,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const9_Signed( -128 ) );
  ufAssert( or_ne1.getSize() == 4 );
  ufAssert(
    !or_ne1.isMemoryAccess() && !or_ne1.isMemoryStore() &&
    !or_ne1.isMemoryLoad() && !or_ne1.isMove() && !or_ne1.isCall() &&
    !or_ne1.isIndirectCall() && !or_ne1.isReturn() && !or_ne1.isJump() &&
    !or_ne1.isConditionalJump() && !or_ne1.isUnconditionalJump() &&
    !or_ne1.isIndirectJump() && !or_ne1.isAsmDataDirective() &&
    !or_ne1.hasSideEffects() );

  WIR_Operation or_ne2(
    TC131::OpCode::OR_NE, TC131::OperationFormat::DDD_2,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( or_ne2.getSize() == 4 );

  WIR_Operation or_nor_t1(
    TC131::OpCode::OR_NOR_T, TC131::OperationFormat::DDC5DC5_2,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const5_Unsigned( 13 ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new TC_Const5_Unsigned( 27 ) );
  ufAssert( or_nor_t1.getSize() == 4 );
  ufAssert(
    !or_nor_t1.isMemoryAccess() && !or_nor_t1.isMemoryStore() &&
    !or_nor_t1.isMemoryLoad() && !or_nor_t1.isMove() && !or_nor_t1.isCall() &&
    !or_nor_t1.isIndirectCall() && !or_nor_t1.isReturn() &&
    !or_nor_t1.isJump() && !or_nor_t1.isConditionalJump() &&
    !or_nor_t1.isUnconditionalJump() && !or_nor_t1.isIndirectJump() &&
    !or_nor_t1.isAsmDataDirective() && !or_nor_t1.hasSideEffects() );

  WIR_Operation or_or_t1(
    TC131::OpCode::OR_OR_T, TC131::OperationFormat::DDC5DC5_2,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const5_Unsigned( 13 ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new TC_Const5_Unsigned( 27 ) );
  ufAssert( or_or_t1.getSize() == 4 );
  ufAssert(
    !or_or_t1.isMemoryAccess() && !or_or_t1.isMemoryStore() &&
    !or_or_t1.isMemoryLoad() && !or_or_t1.isMove() && !or_or_t1.isCall() &&
    !or_or_t1.isIndirectCall() && !or_or_t1.isReturn() && !or_or_t1.isJump() &&
    !or_or_t1.isConditionalJump() && !or_or_t1.isUnconditionalJump() &&
    !or_or_t1.isIndirectJump() && !or_or_t1.isAsmDataDirective() &&
    !or_or_t1.hasSideEffects() );

  WIR_Operation or_t1(
    TC131::OpCode::OR_T, TC131::OperationFormat::DDC5DC5_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const5_Unsigned( 13 ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new TC_Const5_Unsigned( 27 ) );
  ufAssert( or_t1.getSize() == 4 );
  ufAssert(
    !or_t1.isMemoryAccess() && !or_t1.isMemoryStore() &&
    !or_t1.isMemoryLoad() && !or_t1.isMove() && !or_t1.isCall() &&
    !or_t1.isIndirectCall() && !or_t1.isReturn() && !or_t1.isJump() &&
    !or_t1.isConditionalJump() && !or_t1.isUnconditionalJump() &&
    !or_t1.isIndirectJump() && !or_t1.isAsmDataDirective() &&
    !or_t1.hasSideEffects() );

  WIR_Operation orn1(
    TC131::OpCode::ORN, TC131::OperationFormat::DDC9_2,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const9_Unsigned( 128 ) );
  ufAssert( orn1.getSize() == 4 );
  ufAssert(
    !orn1.isMemoryAccess() && !orn1.isMemoryStore() && !orn1.isMemoryLoad() &&
    !orn1.isMove() && !orn1.isCall() && !orn1.isIndirectCall() &&
    !orn1.isReturn() && !orn1.isJump() && !orn1.isConditionalJump() &&
    !orn1.isUnconditionalJump() && !orn1.isIndirectJump() &&
    !orn1.isAsmDataDirective() && !orn1.hasSideEffects() );

  WIR_Operation orn2(
    TC131::OpCode::ORN, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( orn2.getSize() == 4 );

  WIR_Operation orn_t1(
    TC131::OpCode::ORN_T, TC131::OperationFormat::DDC5DC5_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const5_Unsigned( 13 ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new TC_Const5_Unsigned( 27 ) );
  ufAssert( orn_t1.getSize() == 4 );
  ufAssert(
    !orn_t1.isMemoryAccess() && !orn_t1.isMemoryStore() &&
    !orn_t1.isMemoryLoad() && !orn_t1.isMove() && !orn_t1.isCall() &&
    !orn_t1.isIndirectCall() && !orn_t1.isReturn() && !orn_t1.isJump() &&
    !orn_t1.isConditionalJump() && !orn_t1.isUnconditionalJump() &&
    !orn_t1.isIndirectJump() && !orn_t1.isAsmDataDirective() &&
    !orn_t1.hasSideEffects() );

  WIR_Operation pack1(
    TC131::OpCode::PACK, TC131::OperationFormat::DEDPSW,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( e1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( tricore.PSW_C(), WIR_Usage::use ) );
  ufAssert( pack1.getSize() == 4 );
  ufAssert(
    !pack1.isMemoryAccess() && !pack1.isMemoryStore() &&
    !pack1.isMemoryLoad() && !pack1.isMove() && !pack1.isCall() &&
    !pack1.isIndirectCall() && !pack1.isReturn() && !pack1.isJump() &&
    !pack1.isConditionalJump() && !pack1.isUnconditionalJump() &&
    !pack1.isIndirectJump() && !pack1.isAsmDataDirective() &&
    !pack1.hasSideEffects() );

  WIR_Operation parity1(
    TC131::OpCode::PARITY, TC131::OperationFormat::DD,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ) );
  ufAssert( parity1.getSize() == 4 );
  ufAssert(
    !parity1.isMemoryAccess() && !parity1.isMemoryStore() &&
    !parity1.isMemoryLoad() && !parity1.isMove() && !parity1.isCall() &&
    !parity1.isIndirectCall() && !parity1.isReturn() && !parity1.isJump() &&
    !parity1.isConditionalJump() && !parity1.isUnconditionalJump() &&
    !parity1.isIndirectJump() && !parity1.isAsmDataDirective() &&
    !parity1.hasSideEffects() );

  WIR_Operation q31tof1(
    TC131::OpCode::Q31TOF, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( q31tof1.getSize() == 4 );
  ufAssert(
    !q31tof1.isMemoryAccess() && !q31tof1.isMemoryStore() &&
    !q31tof1.isMemoryLoad() && !q31tof1.isMove() && !q31tof1.isCall() &&
    !q31tof1.isIndirectCall() && !q31tof1.isReturn() && !q31tof1.isJump() &&
    !q31tof1.isConditionalJump() && !q31tof1.isUnconditionalJump() &&
    !q31tof1.isIndirectJump() && !q31tof1.isAsmDataDirective() &&
    !q31tof1.hasSideEffects() );

  WIR_Operation qseed_f1(
    TC131::OpCode::QSEED_F, TC131::OperationFormat::DD,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ) );
  ufAssert( qseed_f1.getSize() == 4 );
  ufAssert(
    !qseed_f1.isMemoryAccess() && !qseed_f1.isMemoryStore() &&
    !qseed_f1.isMemoryLoad() && !qseed_f1.isMove() && !qseed_f1.isCall() &&
    !qseed_f1.isIndirectCall() && !qseed_f1.isReturn() && !qseed_f1.isJump() &&
    !qseed_f1.isConditionalJump() && !qseed_f1.isUnconditionalJump() &&
    !qseed_f1.isIndirectJump() && !qseed_f1.isAsmDataDirective() &&
    !qseed_f1.hasSideEffects() );

  WIR_Operation ret1(
    TC131::OpCode::RET, TC131::OperationFormat::PSW,
    new WIR_RegisterParameter( tricore.PSW_C(), WIR_Usage::def ) );
  ufAssert( ret1.getSize() == 4 );
  ufAssert(
    ret1.isMemoryAccess() && !ret1.isMemoryStore() && !ret1.isMemoryLoad() &&
    !ret1.isMove() && !ret1.isCall() && !ret1.isIndirectCall() &&
    ret1.isReturn() && !ret1.isJump() && !ret1.isConditionalJump() &&
    !ret1.isUnconditionalJump() && !ret1.isIndirectJump() &&
    !ret1.isAsmDataDirective() && !ret1.hasSideEffects() );

  WIR_Operation ret2(
    TC131::OpCode::RET, TC131::OperationFormat::SPSW,
    new WIR_RegisterParameter( tricore.PSW_C(), WIR_Usage::def ) );
  ufAssert( ret2.getSize() == 2 );

  WIR_Operation rfe1(
    TC131::OpCode::RFE, TC131::OperationFormat::PSW,
    new WIR_RegisterParameter( tricore.PSW_C(), WIR_Usage::def ) );
  ufAssert( rfe1.getSize() == 4 );
  ufAssert(
    rfe1.isMemoryAccess() && !rfe1.isMemoryStore() && !rfe1.isMemoryLoad() &&
    !rfe1.isMove() && !rfe1.isCall() && !rfe1.isIndirectCall() &&
    rfe1.isReturn() && !rfe1.isJump() && !rfe1.isConditionalJump() &&
    !rfe1.isUnconditionalJump() && !rfe1.isIndirectJump() &&
    !rfe1.isAsmDataDirective() && !rfe1.hasSideEffects() );

  WIR_Operation rfe2(
    TC131::OpCode::RFE, TC131::OperationFormat::SPSW,
    new WIR_RegisterParameter( tricore.PSW_C(), WIR_Usage::def ) );
  ufAssert( rfe2.getSize() == 2 );

  WIR_Operation rfm1(
    TC131::OpCode::RFM, TC131::OperationFormat::PSW,
    new WIR_RegisterParameter( tricore.PSW_C(), WIR_Usage::def ) );
  ufAssert( rfm1.getSize() == 4 );
  ufAssert(
    rfm1.isMemoryAccess() && !rfm1.isMemoryStore() && !rfm1.isMemoryLoad() &&
    !rfm1.isMove() && !rfm1.isCall() && !rfm1.isIndirectCall() &&
    rfm1.isReturn() && !rfm1.isJump() && !rfm1.isConditionalJump() &&
    !rfm1.isUnconditionalJump() && !rfm1.isIndirectJump() &&
    !rfm1.isAsmDataDirective() && !rfm1.hasSideEffects() );

  WIR_Operation rslcx1(
    TC131::OpCode::RSLCX, TC131::OperationFormat::SYS );
  ufAssert( rslcx1.getSize() == 4 );
  ufAssert(
    !rslcx1.isMemoryAccess() && !rslcx1.isMemoryStore() &&
    rslcx1.isMemoryLoad() && !rslcx1.isMove() && !rslcx1.isCall() &&
    !rslcx1.isIndirectCall() && !rslcx1.isReturn() && !rslcx1.isJump() &&
    !rslcx1.isConditionalJump() && !rslcx1.isUnconditionalJump() &&
    !rslcx1.isIndirectJump() && !rslcx1.isAsmDataDirective() &&
    rslcx1.hasSideEffects() );

  WIR_Operation rstv1(
    TC131::OpCode::RSTV, TC131::OperationFormat::SYS );
  ufAssert( rstv1.getSize() == 4 );
  ufAssert(
    !rstv1.isMemoryAccess() && !rstv1.isMemoryStore() &&
    !rstv1.isMemoryLoad() && !rstv1.isMove() && !rstv1.isCall() &&
    !rstv1.isIndirectCall() && !rstv1.isReturn() && !rstv1.isJump() &&
    !rstv1.isConditionalJump() && !rstv1.isUnconditionalJump() &&
    !rstv1.isIndirectJump() && !rstv1.isAsmDataDirective() &&
    rstv1.hasSideEffects() );

  WIR_Operation rsub1(
    TC131::OpCode::RSUB, TC131::OperationFormat::DDC9_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const9_Signed( -128 ) );
  ufAssert( rsub1.getSize() == 4 );
  ufAssert(
    !rsub1.isMemoryAccess() && !rsub1.isMemoryStore() &&
    !rsub1.isMemoryLoad() && !rsub1.isMove() && !rsub1.isCall() &&
    !rsub1.isIndirectCall() && !rsub1.isReturn() && !rsub1.isJump() &&
    !rsub1.isConditionalJump() && !rsub1.isUnconditionalJump() &&
    !rsub1.isIndirectJump() && !rsub1.isAsmDataDirective() &&
    !rsub1.hasSideEffects() );

  WIR_Operation rsub2(
    TC131::OpCode::RSUB, TC131::OperationFormat::SD,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ) );
  ufAssert( rsub2.getSize() == 2 );

  WIR_Operation rsubs1(
    TC131::OpCode::RSUBS, TC131::OperationFormat::DDC9_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const9_Signed( -128 ) );
  ufAssert( rsubs1.getSize() == 4 );
  ufAssert(
    !rsubs1.isMemoryAccess() && !rsubs1.isMemoryStore() &&
    !rsubs1.isMemoryLoad() && !rsubs1.isMove() && !rsubs1.isCall() &&
    !rsubs1.isIndirectCall() && !rsubs1.isReturn() && !rsubs1.isJump() &&
    !rsubs1.isConditionalJump() && !rsubs1.isUnconditionalJump() &&
    !rsubs1.isIndirectJump() && !rsubs1.isAsmDataDirective() &&
    !rsubs1.hasSideEffects() );

  WIR_Operation rsubs_u1(
    TC131::OpCode::RSUBS_U, TC131::OperationFormat::DDC9_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const9_Signed( -128 ) );
  ufAssert( rsubs_u1.getSize() == 4 );
  ufAssert(
    !rsubs_u1.isMemoryAccess() && !rsubs_u1.isMemoryStore() &&
    !rsubs_u1.isMemoryLoad() && !rsubs_u1.isMove() && !rsubs_u1.isCall() &&
    !rsubs_u1.isIndirectCall() && !rsubs_u1.isReturn() && !rsubs_u1.isJump() &&
    !rsubs_u1.isConditionalJump() && !rsubs_u1.isUnconditionalJump() &&
    !rsubs_u1.isIndirectJump() && !rsubs_u1.isAsmDataDirective() &&
    !rsubs_u1.hasSideEffects() );

  WIR_Operation sat_b1(
    TC131::OpCode::SAT_B, TC131::OperationFormat::DD,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ) );
  ufAssert( sat_b1.getSize() == 4 );
  ufAssert(
    !sat_b1.isMemoryAccess() && !sat_b1.isMemoryStore() &&
    !sat_b1.isMemoryLoad() && !sat_b1.isMove() && !sat_b1.isCall() &&
    !sat_b1.isIndirectCall() && !sat_b1.isReturn() && !sat_b1.isJump() &&
    !sat_b1.isConditionalJump() && !sat_b1.isUnconditionalJump() &&
    !sat_b1.isIndirectJump() && !sat_b1.isAsmDataDirective() &&
    !sat_b1.hasSideEffects() );

  WIR_Operation sat_b2(
    TC131::OpCode::SAT_B, TC131::OperationFormat::SD,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ) );
  ufAssert( sat_b2.getSize() == 2 );

  WIR_Operation sat_bu1(
    TC131::OpCode::SAT_BU, TC131::OperationFormat::DD,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ) );
  ufAssert( sat_bu1.getSize() == 4 );
  ufAssert(
    !sat_bu1.isMemoryAccess() && !sat_bu1.isMemoryStore() &&
    !sat_bu1.isMemoryLoad() && !sat_bu1.isMove() && !sat_bu1.isCall() &&
    !sat_bu1.isIndirectCall() && !sat_bu1.isReturn() && !sat_bu1.isJump() &&
    !sat_bu1.isConditionalJump() && !sat_bu1.isUnconditionalJump() &&
    !sat_bu1.isIndirectJump() && !sat_bu1.isAsmDataDirective() &&
    !sat_bu1.hasSideEffects() );

  WIR_Operation sat_bu2(
    TC131::OpCode::SAT_BU, TC131::OperationFormat::SD,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ) );
  ufAssert( sat_bu2.getSize() == 2 );

  WIR_Operation sat_h1(
    TC131::OpCode::SAT_H, TC131::OperationFormat::DD,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ) );
  ufAssert( sat_h1.getSize() == 4 );
  ufAssert(
    !sat_h1.isMemoryAccess() && !sat_h1.isMemoryStore() &&
    !sat_h1.isMemoryLoad() && !sat_h1.isMove() && !sat_h1.isCall() &&
    !sat_h1.isIndirectCall() && !sat_h1.isReturn() && !sat_h1.isJump() &&
    !sat_h1.isConditionalJump() && !sat_h1.isUnconditionalJump() &&
    !sat_h1.isIndirectJump() && !sat_h1.isAsmDataDirective() &&
    !sat_h1.hasSideEffects() );

  WIR_Operation sat_h2(
    TC131::OpCode::SAT_H, TC131::OperationFormat::SD,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ) );
  ufAssert( sat_h2.getSize() == 2 );

  WIR_Operation sat_hu1(
    TC131::OpCode::SAT_HU, TC131::OperationFormat::DD,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ) );
  ufAssert( sat_hu1.getSize() == 4 );
  ufAssert(
    !sat_hu1.isMemoryAccess() && !sat_hu1.isMemoryStore() &&
    !sat_hu1.isMemoryLoad() && !sat_hu1.isMove() && !sat_hu1.isCall() &&
    !sat_hu1.isIndirectCall() && !sat_hu1.isReturn() && !sat_hu1.isJump() &&
    !sat_hu1.isConditionalJump() && !sat_hu1.isUnconditionalJump() &&
    !sat_hu1.isIndirectJump() && !sat_hu1.isAsmDataDirective() &&
    !sat_hu1.hasSideEffects() );

  WIR_Operation sat_hu2(
    TC131::OpCode::SAT_HU, TC131::OperationFormat::SD,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ) );
  ufAssert( sat_hu2.getSize() == 2 );

  WIR_Operation sel1(
    TC131::OpCode::SEL, TC131::OperationFormat::DDDC9_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new TC_Const9_Signed( -128 ) );
  ufAssert( sel1.getSize() == 4 );
  ufAssert(
    !sel1.isMemoryAccess() && !sel1.isMemoryStore() && !sel1.isMemoryLoad() &&
    !sel1.isMove() && !sel1.isCall() && !sel1.isIndirectCall() &&
    !sel1.isReturn() && !sel1.isJump() && !sel1.isConditionalJump() &&
    !sel1.isUnconditionalJump() && !sel1.isIndirectJump() &&
    !sel1.isAsmDataDirective() && !sel1.hasSideEffects() );

  WIR_Operation sel2(
    TC131::OpCode::SEL, TC131::OperationFormat::DDDD,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ) );
  ufAssert( sel2.getSize() == 4 );

  WIR_Operation seln1(
    TC131::OpCode::SELN, TC131::OperationFormat::DDDC9_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new TC_Const9_Signed( -128 ) );
  ufAssert( seln1.getSize() == 4 );
  ufAssert(
    !seln1.isMemoryAccess() && !seln1.isMemoryStore() &&
    !seln1.isMemoryLoad() && !seln1.isMove() && !seln1.isCall() &&
    !seln1.isIndirectCall() && !seln1.isReturn() && !seln1.isJump() &&
    !seln1.isConditionalJump() && !seln1.isUnconditionalJump() &&
    !seln1.isIndirectJump() && !seln1.isAsmDataDirective() &&
    !seln1.hasSideEffects() );

  WIR_Operation seln2(
    TC131::OpCode::SELN, TC131::OperationFormat::DDDD,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( d4, WIR_Usage::use ) );
  ufAssert( seln2.getSize() == 4 );

  WIR_Operation sh1(
    TC131::OpCode::SH, TC131::OperationFormat::DDC9_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const9_Signed( -128 ) );
  ufAssert( sh1.getSize() == 4 );
  ufAssert(
    !sh1.isMemoryAccess() && !sh1.isMemoryStore() && !sh1.isMemoryLoad() &&
    !sh1.isMove() && !sh1.isCall() && !sh1.isIndirectCall() &&
    !sh1.isReturn() && !sh1.isJump() && !sh1.isConditionalJump() &&
    !sh1.isUnconditionalJump() && !sh1.isIndirectJump() &&
    !sh1.isAsmDataDirective() && !sh1.hasSideEffects() );

  WIR_Operation sh2(
    TC131::OpCode::SH, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( sh2.getSize() == 4 );

  WIR_Operation sh3(
    TC131::OpCode::SH, TC131::OperationFormat::SDC4_2,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new TC_Const4_Signed( -5 ) );
  ufAssert( sh3.getSize() == 2 );

  WIR_Operation sh_and_t1(
    TC131::OpCode::SH_AND_T, TC131::OperationFormat::DDC5DC5_2,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const5_Unsigned( 13 ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new TC_Const5_Unsigned( 27 ) );
  ufAssert( sh_and_t1.getSize() == 4 );
  ufAssert(
    !sh_and_t1.isMemoryAccess() && !sh_and_t1.isMemoryStore() &&
    !sh_and_t1.isMemoryLoad() && !sh_and_t1.isMove() && !sh_and_t1.isCall() &&
    !sh_and_t1.isIndirectCall() && !sh_and_t1.isReturn() &&
    !sh_and_t1.isJump() && !sh_and_t1.isConditionalJump() &&
    !sh_and_t1.isUnconditionalJump() && !sh_and_t1.isIndirectJump() &&
    !sh_and_t1.isAsmDataDirective() && !sh_and_t1.hasSideEffects() );

  WIR_Operation sh_andn_t1(
    TC131::OpCode::SH_ANDN_T, TC131::OperationFormat::DDC5DC5_2,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const5_Unsigned( 13 ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new TC_Const5_Unsigned( 27 ) );
  ufAssert( sh_andn_t1.getSize() == 4 );
  ufAssert(
    !sh_andn_t1.isMemoryAccess() && !sh_andn_t1.isMemoryStore() &&
    !sh_andn_t1.isMemoryLoad() && !sh_andn_t1.isMove() &&
    !sh_andn_t1.isCall() && !sh_andn_t1.isIndirectCall() &&
    !sh_andn_t1.isReturn() && !sh_andn_t1.isJump() &&
    !sh_andn_t1.isConditionalJump() && !sh_andn_t1.isUnconditionalJump() &&
    !sh_andn_t1.isIndirectJump() && !sh_andn_t1.isAsmDataDirective() &&
    !sh_andn_t1.hasSideEffects() );

  WIR_Operation sh_eq1(
    TC131::OpCode::SH_EQ, TC131::OperationFormat::DDC9_3,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const9_Signed( -128 ) );
  ufAssert( sh_eq1.getSize() == 4 );
  ufAssert(
    !sh_eq1.isMemoryAccess() && !sh_eq1.isMemoryStore() &&
    !sh_eq1.isMemoryLoad() && !sh_eq1.isMove() && !sh_eq1.isCall() &&
    !sh_eq1.isIndirectCall() && !sh_eq1.isReturn() && !sh_eq1.isJump() &&
    !sh_eq1.isConditionalJump() && !sh_eq1.isUnconditionalJump() &&
    !sh_eq1.isIndirectJump() && !sh_eq1.isAsmDataDirective() &&
    !sh_eq1.hasSideEffects() );

  WIR_Operation sh_eq2(
    TC131::OpCode::SH_EQ, TC131::OperationFormat::DDD_2,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( sh_eq2.getSize() == 4 );

  WIR_Operation sh_ge1(
    TC131::OpCode::SH_GE, TC131::OperationFormat::DDC9_3,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const9_Signed( -128 ) );
  ufAssert( sh_ge1.getSize() == 4 );
  ufAssert(
    !sh_ge1.isMemoryAccess() && !sh_ge1.isMemoryStore() &&
    !sh_ge1.isMemoryLoad() && !sh_ge1.isMove() && !sh_ge1.isCall() &&
    !sh_ge1.isIndirectCall() && !sh_ge1.isReturn() && !sh_ge1.isJump() &&
    !sh_ge1.isConditionalJump() && !sh_ge1.isUnconditionalJump() &&
    !sh_ge1.isIndirectJump() && !sh_ge1.isAsmDataDirective() &&
    !sh_ge1.hasSideEffects() );

  WIR_Operation sh_ge2(
    TC131::OpCode::SH_GE, TC131::OperationFormat::DDD_2,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( sh_ge2.getSize() == 4 );

  WIR_Operation sh_ge_u1(
    TC131::OpCode::SH_GE_U, TC131::OperationFormat::DDC9_4,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const9_Unsigned( 128 ) );
  ufAssert( sh_ge_u1.getSize() == 4 );
  ufAssert(
    !sh_ge_u1.isMemoryAccess() && !sh_ge_u1.isMemoryStore() &&
    !sh_ge_u1.isMemoryLoad() && !sh_ge_u1.isMove() && !sh_ge_u1.isCall() &&
    !sh_ge_u1.isIndirectCall() && !sh_ge_u1.isReturn() && !sh_ge_u1.isJump() &&
    !sh_ge_u1.isConditionalJump() && !sh_ge_u1.isUnconditionalJump() &&
    !sh_ge_u1.isIndirectJump() && !sh_ge_u1.isAsmDataDirective() &&
    !sh_ge_u1.hasSideEffects() );

  WIR_Operation sh_ge_u2(
    TC131::OpCode::SH_GE_U, TC131::OperationFormat::DDD_2,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( sh_ge_u2.getSize() == 4 );

  WIR_Operation sh_h1(
    TC131::OpCode::SH_H, TC131::OperationFormat::DDC9_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const9_Signed( -128 ) );
  ufAssert( sh_h1.getSize() == 4 );
  ufAssert(
    !sh_h1.isMemoryAccess() && !sh_h1.isMemoryStore() &&
    !sh_h1.isMemoryLoad() && !sh_h1.isMove() && !sh_h1.isCall() &&
    !sh_h1.isIndirectCall() && !sh_h1.isReturn() && !sh_h1.isJump() &&
    !sh_h1.isConditionalJump() && !sh_h1.isUnconditionalJump() &&
    !sh_h1.isIndirectJump() && !sh_h1.isAsmDataDirective() &&
    !sh_h1.hasSideEffects() );

  WIR_Operation sh_h2(
    TC131::OpCode::SH_H, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( sh_h2.getSize() == 4 );

  WIR_Operation sh_lt1(
    TC131::OpCode::SH_LT, TC131::OperationFormat::DDC9_3,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const9_Signed( -128 ) );
  ufAssert( sh_lt1.getSize() == 4 );
  ufAssert(
    !sh_lt1.isMemoryAccess() && !sh_lt1.isMemoryStore() &&
    !sh_lt1.isMemoryLoad() && !sh_lt1.isMove() && !sh_lt1.isCall() &&
    !sh_lt1.isIndirectCall() && !sh_lt1.isReturn() && !sh_lt1.isJump() &&
    !sh_lt1.isConditionalJump() && !sh_lt1.isUnconditionalJump() &&
    !sh_lt1.isIndirectJump() && !sh_lt1.isAsmDataDirective() &&
    !sh_lt1.hasSideEffects() );

  WIR_Operation sh_lt2(
    TC131::OpCode::SH_LT, TC131::OperationFormat::DDD_2,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( sh_lt2.getSize() == 4 );

  WIR_Operation sh_lt_u1(
    TC131::OpCode::SH_LT_U, TC131::OperationFormat::DDC9_4,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const9_Unsigned( 128 ) );
  ufAssert( sh_lt_u1.getSize() == 4 );
  ufAssert(
    !sh_lt_u1.isMemoryAccess() && !sh_lt_u1.isMemoryStore() &&
    !sh_lt_u1.isMemoryLoad() && !sh_lt_u1.isMove() && !sh_lt_u1.isCall() &&
    !sh_lt_u1.isIndirectCall() && !sh_lt_u1.isReturn() && !sh_lt_u1.isJump() &&
    !sh_lt_u1.isConditionalJump() && !sh_lt_u1.isUnconditionalJump() &&
    !sh_lt_u1.isIndirectJump() && !sh_lt_u1.isAsmDataDirective() &&
    !sh_lt_u1.hasSideEffects() );

  WIR_Operation sh_lt_u2(
    TC131::OpCode::SH_LT_U, TC131::OperationFormat::DDD_2,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( sh_lt_u2.getSize() == 4 );

  WIR_Operation sh_nand_t1(
    TC131::OpCode::SH_NAND_T, TC131::OperationFormat::DDC5DC5_2,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const5_Unsigned( 13 ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new TC_Const5_Unsigned( 27 ) );
  ufAssert( sh_nand_t1.getSize() == 4 );
  ufAssert(
    !sh_nand_t1.isMemoryAccess() && !sh_nand_t1.isMemoryStore() &&
    !sh_nand_t1.isMemoryLoad() && !sh_nand_t1.isMove() &&
    !sh_nand_t1.isCall() && !sh_nand_t1.isIndirectCall() &&
    !sh_nand_t1.isReturn() && !sh_nand_t1.isJump() &&
    !sh_nand_t1.isConditionalJump() && !sh_nand_t1.isUnconditionalJump() &&
    !sh_nand_t1.isIndirectJump() && !sh_nand_t1.isAsmDataDirective() &&
    !sh_nand_t1.hasSideEffects() );

  WIR_Operation sh_ne1(
    TC131::OpCode::SH_NE, TC131::OperationFormat::DDC9_3,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const9_Signed( -128 ) );
  ufAssert( sh_ne1.getSize() == 4 );
  ufAssert(
    !sh_ne1.isMemoryAccess() && !sh_ne1.isMemoryStore() &&
    !sh_ne1.isMemoryLoad() && !sh_ne1.isMove() && !sh_ne1.isCall() &&
    !sh_ne1.isIndirectCall() && !sh_ne1.isReturn() && !sh_ne1.isJump() &&
    !sh_ne1.isConditionalJump() && !sh_ne1.isUnconditionalJump() &&
    !sh_ne1.isIndirectJump() && !sh_ne1.isAsmDataDirective() &&
    !sh_ne1.hasSideEffects() );

  WIR_Operation sh_ne2(
    TC131::OpCode::SH_NE, TC131::OperationFormat::DDD_2,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( sh_ne2.getSize() == 4 );

  WIR_Operation sh_nor_t1(
    TC131::OpCode::SH_NOR_T, TC131::OperationFormat::DDC5DC5_2,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const5_Unsigned( 13 ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new TC_Const5_Unsigned( 27 ) );
  ufAssert( sh_nor_t1.getSize() == 4 );
  ufAssert(
    !sh_nor_t1.isMemoryAccess() && !sh_nor_t1.isMemoryStore() &&
    !sh_nor_t1.isMemoryLoad() && !sh_nor_t1.isMove() && !sh_nor_t1.isCall() &&
    !sh_nor_t1.isIndirectCall() && !sh_nor_t1.isReturn() &&
    !sh_nor_t1.isJump() && !sh_nor_t1.isConditionalJump() &&
    !sh_nor_t1.isUnconditionalJump() && !sh_nor_t1.isIndirectJump() &&
    !sh_nor_t1.isAsmDataDirective() && !sh_nor_t1.hasSideEffects() );

  WIR_Operation sh_or_t1(
    TC131::OpCode::SH_OR_T, TC131::OperationFormat::DDC5DC5_2,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const5_Unsigned( 13 ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new TC_Const5_Unsigned( 27 ) );
  ufAssert( sh_or_t1.getSize() == 4 );
  ufAssert(
    !sh_or_t1.isMemoryAccess() && !sh_or_t1.isMemoryStore() &&
    !sh_or_t1.isMemoryLoad() && !sh_or_t1.isMove() && !sh_or_t1.isCall() &&
    !sh_or_t1.isIndirectCall() && !sh_or_t1.isReturn() && !sh_or_t1.isJump() &&
    !sh_or_t1.isConditionalJump() && !sh_or_t1.isUnconditionalJump() && !sh_or_t1.isIndirectJump() && !sh_or_t1.isAsmDataDirective() &&
    !sh_or_t1.hasSideEffects() );

  WIR_Operation sh_orn_t1(
    TC131::OpCode::SH_ORN_T, TC131::OperationFormat::DDC5DC5_2,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const5_Unsigned( 13 ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new TC_Const5_Unsigned( 27 ) );
  ufAssert( sh_orn_t1.getSize() == 4 );
  ufAssert(
    !sh_orn_t1.isMemoryAccess() && !sh_orn_t1.isMemoryStore() &&
    !sh_orn_t1.isMemoryLoad() && !sh_orn_t1.isMove() && !sh_orn_t1.isCall() &&
    !sh_orn_t1.isIndirectCall() && !sh_orn_t1.isReturn() &&
    !sh_orn_t1.isJump() && !sh_orn_t1.isConditionalJump() &&
    !sh_orn_t1.isUnconditionalJump() && !sh_orn_t1.isIndirectJump() &&
    !sh_orn_t1.isAsmDataDirective() && !sh_orn_t1.hasSideEffects() );

  WIR_Operation sh_xnor_t1(
    TC131::OpCode::SH_XNOR_T, TC131::OperationFormat::DDC5DC5_2,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const5_Unsigned( 13 ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new TC_Const5_Unsigned( 27 ) );
  ufAssert( sh_xnor_t1.getSize() == 4 );
  ufAssert(
    !sh_xnor_t1.isMemoryAccess() && !sh_xnor_t1.isMemoryStore() &&
    !sh_xnor_t1.isMemoryLoad() && !sh_xnor_t1.isMove() &&
    !sh_xnor_t1.isCall() && !sh_xnor_t1.isIndirectCall() &&
    !sh_xnor_t1.isReturn() && !sh_xnor_t1.isJump() &&
    !sh_xnor_t1.isConditionalJump() && !sh_xnor_t1.isUnconditionalJump() &&
    !sh_xnor_t1.isIndirectJump() && !sh_xnor_t1.isAsmDataDirective() &&
    !sh_xnor_t1.hasSideEffects() );

  WIR_Operation sh_xor_t1(
    TC131::OpCode::SH_XOR_T, TC131::OperationFormat::DDC5DC5_2,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const5_Unsigned( 13 ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new TC_Const5_Unsigned( 27 ) );
  ufAssert( sh_xor_t1.getSize() == 4 );
  ufAssert(
    !sh_xor_t1.isMemoryAccess() && !sh_xor_t1.isMemoryStore() &&
    !sh_xor_t1.isMemoryLoad() && !sh_xor_t1.isMove() && !sh_xor_t1.isCall() &&
    !sh_xor_t1.isIndirectCall() && !sh_xor_t1.isReturn() &&
    !sh_xor_t1.isJump() && !sh_xor_t1.isConditionalJump() &&
    !sh_xor_t1.isUnconditionalJump() && !sh_xor_t1.isIndirectJump() &&
    !sh_xor_t1.isAsmDataDirective() && !sh_xor_t1.hasSideEffects() );

  WIR_Operation sha1(
    TC131::OpCode::SHA, TC131::OperationFormat::DDC9PSW_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const9_Signed( -128 ),
    new WIR_RegisterParameter( tricore.PSW_C(), WIR_Usage::def ) );
  ufAssert( sha1.getSize() == 4 );
  ufAssert(
    !sha1.isMemoryAccess() && !sha1.isMemoryStore() && !sha1.isMemoryLoad() &&
    !sha1.isMove() && !sha1.isCall() && !sha1.isIndirectCall() &&
    !sha1.isReturn() && !sha1.isJump() && !sha1.isConditionalJump() &&
    !sha1.isUnconditionalJump() && !sha1.isIndirectJump() &&
    !sha1.isAsmDataDirective() && !sha1.hasSideEffects() );

  WIR_Operation sha2(
    TC131::OpCode::SHA, TC131::OperationFormat::DDDPSW_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( tricore.PSW_C(), WIR_Usage::def ) );
  ufAssert( sha2.getSize() == 4 );

  WIR_Operation sha3(
    TC131::OpCode::SHA, TC131::OperationFormat::SDC4PSW,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new TC_Const4_Signed( -5 ),
    new WIR_RegisterParameter( tricore.PSW_C(), WIR_Usage::def ) );
  ufAssert( sha3.getSize() == 2 );

  WIR_Operation sha_h1(
    TC131::OpCode::SHA_H, TC131::OperationFormat::DDC9_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const9_Signed( -128 ) );
  ufAssert( sha_h1.getSize() == 4 );
  ufAssert(
    !sha_h1.isMemoryAccess() && !sha_h1.isMemoryStore() &&
    !sha_h1.isMemoryLoad() && !sha_h1.isMove() && !sha_h1.isCall() &&
    !sha_h1.isIndirectCall() && !sha_h1.isReturn() && !sha_h1.isJump() &&
    !sha_h1.isConditionalJump() && !sha_h1.isUnconditionalJump() &&
    !sha_h1.isIndirectJump() && !sha_h1.isAsmDataDirective() &&
    !sha_h1.hasSideEffects() );

  WIR_Operation sha_h2(
    TC131::OpCode::SHA_H, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( sha_h2.getSize() == 4 );

  WIR_Operation shas1(
    TC131::OpCode::SHAS, TC131::OperationFormat::DDC9_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const9_Signed( -128 ) );
  ufAssert( shas1.getSize() == 4 );
  ufAssert(
    !shas1.isMemoryAccess() && !shas1.isMemoryStore() &&
    !shas1.isMemoryLoad() && !shas1.isMove() && !shas1.isCall() &&
    !shas1.isIndirectCall() && !shas1.isReturn() && !shas1.isJump() &&
    !shas1.isConditionalJump() && !shas1.isUnconditionalJump() &&
    !shas1.isIndirectJump() && !shas1.isAsmDataDirective() &&
    !shas1.hasSideEffects() );

  WIR_Operation shas2(
    TC131::OpCode::SHAS, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( shas2.getSize() == 4 );

  WIR_Operation st_a1(
    TC131::OpCode::ST_A, TC131::OperationFormat::C18AABSA,
    new TC_Const18_Unsigned( 16383 ),
    new WIR_RegisterParameter( a1, WIR_Usage::use ) );
  ufAssert( st_a1.getSize() == 4 );
  ufAssert(
    !st_a1.isMemoryAccess() && st_a1.isMemoryStore() && !st_a1.isMemoryLoad() &&
    !st_a1.isMove() && !st_a1.isCall() && !st_a1.isIndirectCall() &&
    !st_a1.isReturn() && !st_a1.isJump() && !st_a1.isConditionalJump() &&
    !st_a1.isUnconditionalJump() && !st_a1.isIndirectJump() &&
    !st_a1.isAsmDataDirective() && !st_a1.hasSideEffects() );

  WIR_Operation st_a2(
    TC131::OpCode::ST_A, TC131::OperationFormat::LAABSA,
    new WIR_LabelParameter( b ),
    new WIR_RegisterParameter( a1, WIR_Usage::use ) );
  ufAssert( st_a2.getSize() == 4 );

  WIR_Operation st_a3(
    TC131::OpCode::ST_A, TC131::OperationFormat::AC10ABOA,
    new WIR_RegisterParameter( a1, WIR_Usage::use ),
    new TC_Const10_Signed( -512 ),
    WIR_RegisterParameter( a2, WIR_Usage::use ) );
  ufAssert( st_a3.getSize() == 4 );

  WIR_Operation st_a4(
    TC131::OpCode::ST_A, TC131::OperationFormat::PABRA,
    new WIR_RegisterParameter( p1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( a1, WIR_Usage::use ) );
  ufAssert( st_a4.getSize() == 4 );

  WIR_Operation st_a5(
    TC131::OpCode::ST_A, TC131::OperationFormat::PC10ACA,
    new WIR_RegisterParameter( p1, WIR_Usage::defuse ),
    new TC_Const10_Signed( -512 ),
    new WIR_RegisterParameter( a1, WIR_Usage::use ) );
  ufAssert( st_a5.getSize() == 4 );

  WIR_Operation st_a6(
    TC131::OpCode::ST_A, TC131::OperationFormat::AC10APIA,
    new WIR_AddressingModeParameter( TC131::AddressingMode::post ),
    new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
    new TC_Const10_Signed( -512 ),
    new WIR_RegisterParameter( a2, WIR_Usage::use ) );
  ufAssert( st_a6.getSize() == 4 );

  WIR_Operation st_a7(
    TC131::OpCode::ST_A, TC131::OperationFormat::AC10APIA,
    new WIR_AddressingModeParameter( TC131::AddressingMode::pre ),
    new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
    new TC_Const10_Signed( -512 ),
    new WIR_RegisterParameter( a2, WIR_Usage::use ) );
  ufAssert( st_a7.getSize() == 4 );

  WIR_Operation st_a8(
    TC131::OpCode::ST_A, TC131::OperationFormat::SSPC10I_1,
    new WIR_RegisterParameter( tricore.SP(), WIR_Usage::use ),
    new TC_Const10_Unsigned( 252 ),
    new WIR_RegisterParameter( tricore.A15(), WIR_Usage::use ) );
  ufAssert( st_a8.getSize() == 2 );

  WIR_Operation st_a9(
    TC131::OpCode::ST_A, TC131::OperationFormat::SAC4I_1,
    new WIR_RegisterParameter( a1, WIR_Usage::use ),
    new TC_Const4_Unsigned( 12 ),
    new WIR_RegisterParameter( tricore.A15(), WIR_Usage::use ) );
  ufAssert( st_a9.getSize() == 2 );

  WIR_Operation st_a10(
    TC131::OpCode::ST_A, TC131::OperationFormat::SAA_4,
    new WIR_RegisterParameter( a1, WIR_Usage::use ),
    new WIR_RegisterParameter( a2, WIR_Usage::use ) );
  ufAssert( st_a10.getSize() == 2 );

  WIR_Operation st_a11(
    TC131::OpCode::ST_A, TC131::OperationFormat::SAA_6,
    new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( a2, WIR_Usage::use ) );
  ufAssert( st_a11.getSize() == 2 );

  WIR_Operation st_a12(
    TC131::OpCode::ST_A, TC131::OperationFormat::SIC4A,
    new WIR_RegisterParameter( tricore.A15(), WIR_Usage::use ),
    new TC_Const4_Unsigned( 12 ),
    new WIR_RegisterParameter( a1, WIR_Usage::use ) );
  ufAssert( st_a12.getSize() == 2 );

  WIR_Operation st_b1(
    TC131::OpCode::ST_B, TC131::OperationFormat::C18DABSA_1,
    new TC_Const18_Unsigned( 16383 ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ) );
  ufAssert( st_b1.getSize() == 4 );
  ufAssert(
    !st_b1.isMemoryAccess() && st_b1.isMemoryStore() && !st_b1.isMemoryLoad() &&
    !st_b1.isMove() && !st_b1.isCall() && !st_b1.isIndirectCall() &&
    !st_b1.isReturn() && !st_b1.isJump() && !st_b1.isConditionalJump() &&
    !st_b1.isUnconditionalJump() && !st_b1.isIndirectJump() &&
    !st_b1.isAsmDataDirective() && !st_b1.hasSideEffects() );

  WIR_Operation st_b2(
    TC131::OpCode::ST_B, TC131::OperationFormat::LDABSA_1,
    new WIR_LabelParameter( b ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ) );
  ufAssert( st_b2.getSize() == 4 );

  WIR_Operation st_b3(
    TC131::OpCode::ST_B, TC131::OperationFormat::AC10DBOA_1,
    new WIR_RegisterParameter( a1, WIR_Usage::use ),
    new TC_Const10_Signed( -512 ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ) );
  ufAssert( st_b3.getSize() == 4 );

  WIR_Operation st_b4(
    TC131::OpCode::ST_B, TC131::OperationFormat::PDBRA_1,
    new WIR_RegisterParameter( p1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ) );
  ufAssert( st_b4.getSize() == 4 );

  WIR_Operation st_b5(
    TC131::OpCode::ST_B, TC131::OperationFormat::PC10DCA_1,
    new WIR_RegisterParameter( p1, WIR_Usage::defuse ),
    new TC_Const10_Signed( -512 ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ) );
  ufAssert( st_b5.getSize() == 4 );

  WIR_Operation st_b6(
    TC131::OpCode::ST_B, TC131::OperationFormat::AC10DPIA_1,
    new WIR_AddressingModeParameter( TC131::AddressingMode::post ),
    new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
    new TC_Const10_Signed( -512 ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ) );
  ufAssert( st_b6.getSize() == 4 );

  WIR_Operation st_b7(
    TC131::OpCode::ST_B, TC131::OperationFormat::AC10DPIA_1,
    new WIR_AddressingModeParameter( TC131::AddressingMode::pre ),
    new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
    new TC_Const10_Signed( -512 ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ) );
  ufAssert( st_b7.getSize() == 4 );

  WIR_Operation st_b8(
    TC131::OpCode::ST_B, TC131::OperationFormat::SAC4I_2,
    new WIR_RegisterParameter( a1, WIR_Usage::use ),
    new TC_Const4_Unsigned( 15 ),
    new WIR_RegisterParameter( tricore.D15(), WIR_Usage::use ) );
  ufAssert( st_b8.getSize() == 2 );

  WIR_Operation st_b9(
    TC131::OpCode::ST_B, TC131::OperationFormat::SAD_2,
    new WIR_RegisterParameter( a1, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ) );
  ufAssert( st_b9.getSize() == 2 );

  WIR_Operation st_b10(
    TC131::OpCode::ST_B, TC131::OperationFormat::SAD_3,
    new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ) );
  ufAssert( st_b10.getSize() == 2 );

  WIR_Operation st_b11(
    TC131::OpCode::ST_B, TC131::OperationFormat::SIC4D,
    new WIR_RegisterParameter( tricore.A15(), WIR_Usage::use ),
    new TC_Const4_Unsigned( 15 ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ) );
  ufAssert( st_b11.getSize() == 2 );

  WIR_Operation st_d1(
    TC131::OpCode::ST_D, TC131::OperationFormat::C18EABSA,
    new TC_Const18_Unsigned( 16383 ),
    new WIR_RegisterParameter( e1, WIR_Usage::use ) );
  ufAssert( st_d1.getSize() == 4 );
  ufAssert(
    !st_d1.isMemoryAccess() && st_d1.isMemoryStore() && !st_d1.isMemoryLoad() &&
    !st_d1.isMove() && !st_d1.isCall() && !st_d1.isIndirectCall() &&
    !st_d1.isReturn() && !st_d1.isJump() && !st_d1.isConditionalJump() &&
    !st_d1.isUnconditionalJump() && !st_d1.isIndirectJump() &&
    !st_d1.isAsmDataDirective() && !st_d1.hasSideEffects() );

  WIR_Operation st_d2(
    TC131::OpCode::ST_D, TC131::OperationFormat::LEABSA,
    new WIR_LabelParameter( b ),
    new WIR_RegisterParameter( e1, WIR_Usage::use ) );
  ufAssert( st_d2.getSize() == 4 );

  WIR_Operation st_d3(
    TC131::OpCode::ST_D, TC131::OperationFormat::AC10EBOA,
    new WIR_RegisterParameter( a1, WIR_Usage::use ),
    new TC_Const10_Signed( -512 ),
    new WIR_RegisterParameter( e1, WIR_Usage::use ) );
  ufAssert( st_d3.getSize() == 4 );

  WIR_Operation st_d4(
    TC131::OpCode::ST_D, TC131::OperationFormat::PEBRA,
    new WIR_RegisterParameter( p1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( e1, WIR_Usage::use ) );
  ufAssert( st_d4.getSize() == 4 );

  WIR_Operation st_d5(
    TC131::OpCode::ST_D, TC131::OperationFormat::PC10ECA,
    new WIR_RegisterParameter( p1, WIR_Usage::defuse ),
    new TC_Const10_Signed( -512 ),
    new WIR_RegisterParameter( e1, WIR_Usage::use ) );
  ufAssert( st_d5.getSize() == 4 );

  WIR_Operation st_d6(
    TC131::OpCode::ST_D, TC131::OperationFormat::AC10EPIA,
    new WIR_AddressingModeParameter( TC131::AddressingMode::post ),
    new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
    new TC_Const10_Signed( -512 ),
    new WIR_RegisterParameter( e1, WIR_Usage::use ) );
  ufAssert( st_d6.getSize() == 4 );

  WIR_Operation st_d7(
    TC131::OpCode::ST_D, TC131::OperationFormat::AC10EPIA,
    new WIR_AddressingModeParameter( TC131::AddressingMode::pre ),
    new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
    new TC_Const10_Signed( -512 ),
    new WIR_RegisterParameter( e1, WIR_Usage::use ) );
  ufAssert( st_d7.getSize() == 4 );

  WIR_Operation st_da1(
    TC131::OpCode::ST_DA, TC131::OperationFormat::C18PABSA,
    new TC_Const18_Unsigned( 16383 ),
    new WIR_RegisterParameter( p1, WIR_Usage::use ) );
  ufAssert( st_da1.getSize() == 4 );
  ufAssert(
    !st_da1.isMemoryAccess() && st_da1.isMemoryStore() &&
    !st_da1.isMemoryLoad() && !st_da1.isMove() && !st_da1.isCall() &&
    !st_da1.isIndirectCall() && !st_da1.isReturn() && !st_da1.isJump() &&
    !st_da1.isConditionalJump() && !st_da1.isUnconditionalJump() &&
    !st_da1.isIndirectJump() && !st_da1.isAsmDataDirective() &&
    !st_da1.hasSideEffects() );

  WIR_Operation st_da2(
    TC131::OpCode::ST_DA, TC131::OperationFormat::LPABSA,
    new WIR_LabelParameter( b ),
    new WIR_RegisterParameter( p1, WIR_Usage::use ) );
  ufAssert( st_da2.getSize() == 4 );

  WIR_Operation st_da3(
    TC131::OpCode::ST_DA, TC131::OperationFormat::AC10PBOA,
    new WIR_RegisterParameter( a1, WIR_Usage::use ),
    new TC_Const10_Signed( -512 ),
    new WIR_RegisterParameter( p1, WIR_Usage::use ) );
  ufAssert( st_da3.getSize() == 4 );

  WIR_Operation st_da4(
    TC131::OpCode::ST_DA, TC131::OperationFormat::PPBRA_2,
    new WIR_RegisterParameter( p1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( p2, WIR_Usage::use ) );
  ufAssert( st_da4.getSize() == 4 );

  WIR_Operation st_da5(
    TC131::OpCode::ST_DA, TC131::OperationFormat::PC10PCA,
    new WIR_RegisterParameter( p1, WIR_Usage::defuse ),
    new TC_Const10_Signed( -512 ),
    new WIR_RegisterParameter( p2, WIR_Usage::use ) );
  ufAssert( st_da5.getSize() == 4 );

  WIR_Operation st_da6(
    TC131::OpCode::ST_DA, TC131::OperationFormat::AC10PPIA,
    new WIR_AddressingModeParameter( TC131::AddressingMode::post ),
    new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
    new TC_Const10_Signed( -512 ),
    new WIR_RegisterParameter( p1, WIR_Usage::use ) );
  ufAssert( st_da6.getSize() == 4 );

  WIR_Operation st_da7(
    TC131::OpCode::ST_DA, TC131::OperationFormat::AC10PPIA,
    new WIR_AddressingModeParameter( TC131::AddressingMode::pre ),
    new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
    new TC_Const10_Signed( -512 ),
    new WIR_RegisterParameter( p1, WIR_Usage::use ) );
  ufAssert( st_da7.getSize() == 4 );

  WIR_Operation st_h1(
    TC131::OpCode::ST_H, TC131::OperationFormat::C18DABSA_1,
    new TC_Const18_Unsigned( 16383 ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ) );
  ufAssert( st_h1.getSize() == 4 );
  ufAssert(
    !st_h1.isMemoryAccess() && st_h1.isMemoryStore() && !st_h1.isMemoryLoad() &&
    !st_h1.isMove() && !st_h1.isCall() && !st_h1.isIndirectCall() &&
    !st_h1.isReturn() && !st_h1.isJump() && !st_h1.isConditionalJump() &&
    !st_h1.isUnconditionalJump() && !st_h1.isIndirectJump() &&
    !st_h1.isAsmDataDirective() && !st_h1.hasSideEffects() );

  WIR_Operation st_h2(
    TC131::OpCode::ST_H, TC131::OperationFormat::LDABSA_1,
    new WIR_LabelParameter( b ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ) );
  ufAssert( st_h2.getSize() == 4 );

  WIR_Operation st_h3(
    TC131::OpCode::ST_H, TC131::OperationFormat::AC10DBOA_1,
    new WIR_RegisterParameter( a1, WIR_Usage::use ),
    new TC_Const10_Signed( -512 ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ) );
  ufAssert( st_h3.getSize() == 4 );

  WIR_Operation st_h4(
    TC131::OpCode::ST_H, TC131::OperationFormat::PDBRA_1,
    new WIR_RegisterParameter( p1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ) );
  ufAssert( st_h4.getSize() == 4 );

  WIR_Operation st_h5(
    TC131::OpCode::ST_H, TC131::OperationFormat::PC10DCA_1,
    new WIR_RegisterParameter( p1, WIR_Usage::defuse ),
    new TC_Const10_Signed( -512 ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ) );
  ufAssert( st_h5.getSize() == 4 );

  WIR_Operation st_h6(
    TC131::OpCode::ST_H, TC131::OperationFormat::AC10DPIA_1,
    new WIR_AddressingModeParameter( TC131::AddressingMode::post ),
    new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
    new TC_Const10_Signed( -512 ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ) );
  ufAssert( st_h6.getSize() == 4 );

  WIR_Operation st_h7(
    TC131::OpCode::ST_H, TC131::OperationFormat::AC10DPIA_1,
    new WIR_AddressingModeParameter( TC131::AddressingMode::pre ),
    new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
    new TC_Const10_Signed( -512 ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ) );
  ufAssert( st_h7.getSize() == 4 );

  WIR_Operation st_h8(
    TC131::OpCode::ST_H, TC131::OperationFormat::SAC4I_2,
    new WIR_RegisterParameter( a1, WIR_Usage::use ),
    new TC_Const4_Unsigned( 12 ),
    new WIR_RegisterParameter( tricore.D15(), WIR_Usage::use ) );
  ufAssert( st_h8.getSize() == 2 );

  WIR_Operation st_h9(
    TC131::OpCode::ST_H, TC131::OperationFormat::SAD_2,
    new WIR_RegisterParameter( a1, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ) );
  ufAssert( st_h9.getSize() == 2 );

  WIR_Operation st_h10(
    TC131::OpCode::ST_H, TC131::OperationFormat::SAD_3,
    new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ) );
  ufAssert( st_h10.getSize() == 2 );

  WIR_Operation st_h11(
    TC131::OpCode::ST_H, TC131::OperationFormat::SIC4D,
    new WIR_RegisterParameter( tricore.A15(), WIR_Usage::use ),
    new TC_Const4_Unsigned( 12 ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ) );
  ufAssert( st_h11.getSize() == 2 );

  WIR_Operation st_q1(
    TC131::OpCode::ST_Q, TC131::OperationFormat::C18DABSA_1,
    new TC_Const18_Unsigned( 16383 ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ) );
  ufAssert( st_q1.getSize() == 4 );
  ufAssert(
    !st_q1.isMemoryAccess() && st_q1.isMemoryStore() && !st_q1.isMemoryLoad() &&
    !st_q1.isMove() && !st_q1.isCall() && !st_q1.isIndirectCall() &&
    !st_q1.isReturn() && !st_q1.isJump() && !st_q1.isConditionalJump() &&
    !st_q1.isUnconditionalJump() && !st_q1.isIndirectJump() &&
    !st_q1.isAsmDataDirective() && !st_q1.hasSideEffects() );

  WIR_Operation st_q2(
    TC131::OpCode::ST_Q, TC131::OperationFormat::LDABSA_1,
    new WIR_LabelParameter( b ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ) );
  ufAssert( st_q2.getSize() == 4 );

  WIR_Operation st_q3(
    TC131::OpCode::ST_Q, TC131::OperationFormat::AC10DBOA_1,
    new WIR_RegisterParameter( a1, WIR_Usage::use ),
    new TC_Const10_Signed( -512 ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ) );
  ufAssert( st_q3.getSize() == 4 );

  WIR_Operation st_q4(
    TC131::OpCode::ST_Q, TC131::OperationFormat::PDBRA_1,
    new WIR_RegisterParameter( p1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ) );
  ufAssert( st_q4.getSize() == 4 );

  WIR_Operation st_q5(
    TC131::OpCode::ST_Q, TC131::OperationFormat::PC10DCA_1,
    new WIR_RegisterParameter( p1, WIR_Usage::defuse ),
    new TC_Const10_Signed( -512 ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ) );
  ufAssert( st_q5.getSize() == 4 );

  WIR_Operation st_q6(
    TC131::OpCode::ST_Q, TC131::OperationFormat::AC10DPIA_1,
    new WIR_AddressingModeParameter( TC131::AddressingMode::post ),
    new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
    new TC_Const10_Signed( -512 ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ) );
  ufAssert( st_q6.getSize() == 4 );

  WIR_Operation st_q7(
    TC131::OpCode::ST_Q, TC131::OperationFormat::AC10DPIA_1,
    new WIR_AddressingModeParameter( TC131::AddressingMode::pre ),
    new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
    new TC_Const10_Signed( -512 ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ) );
  ufAssert( st_q7.getSize() == 4 );

  WIR_Operation st_t1(
    TC131::OpCode::ST_T, TC131::OperationFormat::C18C3C1,
    new TC_Const18_Unsigned( 16383 ),
    new TC_Const3_Unsigned( 7 ),
    new TC_Const1_Unsigned( 0 ) );
  ufAssert( st_t1.getSize() == 4 );
  ufAssert(
    !st_t1.isMemoryAccess() && st_t1.isMemoryStore() && st_t1.isMemoryLoad() &&
    !st_t1.isMove() && !st_t1.isCall() && !st_t1.isIndirectCall() &&
    !st_t1.isReturn() && !st_t1.isJump() && !st_t1.isConditionalJump() &&
    !st_t1.isUnconditionalJump() && !st_t1.isIndirectJump() &&
    !st_t1.isAsmDataDirective() && !st_t1.hasSideEffects() );

  WIR_Operation st_w1(
    TC131::OpCode::ST_W, TC131::OperationFormat::C18DABSA_1,
    new TC_Const18_Unsigned( 16383 ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ) );
  ufAssert( st_w1.getSize() == 4 );
  ufAssert(
    !st_w1.isMemoryAccess() && st_w1.isMemoryStore() && !st_w1.isMemoryLoad() &&
    !st_w1.isMove() && !st_w1.isCall() && !st_w1.isIndirectCall() &&
    !st_w1.isReturn() && !st_w1.isJump() && !st_w1.isConditionalJump() &&
    !st_w1.isUnconditionalJump() && !st_w1.isIndirectJump() &&
    !st_w1.isAsmDataDirective() && !st_w1.hasSideEffects() );

  WIR_Operation st_w2(
    TC131::OpCode::ST_W, TC131::OperationFormat::LDABSA_1,
    new WIR_LabelParameter( b ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ) );
  ufAssert( st_w2.getSize() == 4 );

  WIR_Operation st_w3(
    TC131::OpCode::ST_W, TC131::OperationFormat::AC10DBOA_1,
    new WIR_RegisterParameter( a1, WIR_Usage::use ),
    new TC_Const10_Signed( -512 ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ) );
  ufAssert( st_w3.getSize() == 4 );

  WIR_Operation st_w4(
    TC131::OpCode::ST_W, TC131::OperationFormat::PDBRA_1,
    new WIR_RegisterParameter( p1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ) );
  ufAssert( st_w4.getSize() == 4 );

  WIR_Operation st_w5(
    TC131::OpCode::ST_W, TC131::OperationFormat::PC10DCA_1,
    new WIR_RegisterParameter( p1, WIR_Usage::defuse ),
    new TC_Const10_Signed( -512 ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ) );
  ufAssert( st_w5.getSize() == 4 );

  WIR_Operation st_w6(
    TC131::OpCode::ST_W, TC131::OperationFormat::AC10DPIA_1,
    new WIR_AddressingModeParameter( TC131::AddressingMode::post ),
    new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
    new TC_Const10_Signed( -512 ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ) );
  ufAssert( st_w6.getSize() == 4 );

  WIR_Operation st_w7(
    TC131::OpCode::ST_W, TC131::OperationFormat::AC10DPIA_1,
    new WIR_AddressingModeParameter( TC131::AddressingMode::pre ),
    new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
    new TC_Const10_Signed( -512 ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ) );
  ufAssert( st_w7.getSize() == 4 );

  WIR_Operation st_w8(
    TC131::OpCode::ST_W, TC131::OperationFormat::AC16DBOA,
    new WIR_RegisterParameter( a1, WIR_Usage::use ),
    new TC_Const16_Signed( -26361 ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ) );
  ufAssert( st_w8.getSize() == 4 );

  WIR_Operation st_w9(
    TC131::OpCode::ST_W, TC131::OperationFormat::ALC16DBOA,
    new WIR_RegisterParameter( a1, WIR_Usage::use ),
    new WIR_LabelParameter( b ),
    new TC_Const16_Signed( -26361 ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ) );
  ufAssert( st_w9.getSize() == 4 );

  WIR_Operation st_w10(
    TC131::OpCode::ST_W, TC131::OperationFormat::SSPC10I_2,
    new WIR_RegisterParameter( tricore.SP(), WIR_Usage::use ),
    new TC_Const10_Unsigned( 1020 ),
    new WIR_RegisterParameter( tricore.D15(), WIR_Usage::use ) );
  ufAssert( st_w10.getSize() == 2 );

  WIR_Operation st_w11(
    TC131::OpCode::ST_W, TC131::OperationFormat::SAC4I_2,
    new WIR_RegisterParameter( a1, WIR_Usage::use ),
    new TC_Const4_Unsigned( 12 ),
    new WIR_RegisterParameter( tricore.D15(), WIR_Usage::use ) );
  ufAssert( st_w11.getSize() == 2 );

  WIR_Operation st_w12(
    TC131::OpCode::ST_W, TC131::OperationFormat::SAD_2,
    new WIR_RegisterParameter( a1, WIR_Usage::use ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ) );
  ufAssert( st_w12.getSize() == 2 );

  WIR_Operation st_w13(
    TC131::OpCode::ST_W, TC131::OperationFormat::SAD_3,
    new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ) );
  ufAssert( st_w13.getSize() == 2 );

  WIR_Operation st_w14(
    TC131::OpCode::ST_W, TC131::OperationFormat::SIC4D,
    new WIR_RegisterParameter( tricore.A15(), WIR_Usage::use ),
    new TC_Const4_Unsigned( 12 ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ) );
  ufAssert( st_w14.getSize() == 2 );

  WIR_Operation stlcx1(
    TC131::OpCode::STLCX, TC131::OperationFormat::C18ABSA,
    new TC_Const18_Unsigned( 16383 ) );
  ufAssert( stlcx1.getSize() == 4 );
  ufAssert(
    !stlcx1.isMemoryAccess() && stlcx1.isMemoryStore() &&
    !stlcx1.isMemoryLoad() && !stlcx1.isMove() && !stlcx1.isCall() &&
    !stlcx1.isIndirectCall() && !stlcx1.isReturn() && !stlcx1.isJump() &&
    !stlcx1.isConditionalJump() && !stlcx1.isUnconditionalJump() &&
    !stlcx1.isIndirectJump() && !stlcx1.isAsmDataDirective() &&
    !stlcx1.hasSideEffects() );

  WIR_Operation stlcx2(
    TC131::OpCode::STLCX, TC131::OperationFormat::LABSA,
    new WIR_LabelParameter( b ) );
  ufAssert( stlcx2.getSize() == 4 );

  WIR_Operation stlcx3(
    TC131::OpCode::STLCX, TC131::OperationFormat::AC10BOA,
    new WIR_RegisterParameter( a1, WIR_Usage::use ),
    new TC_Const10_Signed( -486 ) );
  ufAssert( stlcx3.getSize() == 4 );

  WIR_Operation stucx1(
    TC131::OpCode::STUCX, TC131::OperationFormat::C18ABSAPSW,
    new TC_Const18_Unsigned( 16383 ),
    new WIR_RegisterParameter( tricore.PSW_C(), WIR_Usage::use ) );
  ufAssert( stucx1.getSize() == 4 );
  ufAssert(
    !stucx1.isMemoryAccess() && stucx1.isMemoryStore() &&
    !stucx1.isMemoryLoad() && !stucx1.isMove() && !stucx1.isCall() &&
    !stucx1.isIndirectCall() && !stucx1.isReturn() && !stucx1.isJump() &&
    !stucx1.isConditionalJump() && !stucx1.isUnconditionalJump() &&
    !stucx1.isIndirectJump() && !stucx1.isAsmDataDirective() &&
    !stucx1.hasSideEffects() );

  WIR_Operation stucx2(
    TC131::OpCode::STUCX, TC131::OperationFormat::LABSAPSW,
    new WIR_LabelParameter( b ),
    new WIR_RegisterParameter( tricore.PSW_C(), WIR_Usage::use ) );
  ufAssert( stucx2.getSize() == 4 );

  WIR_Operation stucx3(
    TC131::OpCode::STUCX, TC131::OperationFormat::AC10BOAPSW,
    new WIR_RegisterParameter( a1, WIR_Usage::use ),
    new TC_Const10_Signed( -486 ),
    new WIR_RegisterParameter( tricore.PSW_C(), WIR_Usage::use ) );
  ufAssert( stucx3.getSize() == 4 );

  WIR_Operation sub1(
    TC131::OpCode::SUB, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( sub1.getSize() == 4 );
  ufAssert(
    !sub1.isMemoryAccess() && !sub1.isMemoryStore() && !sub1.isMemoryLoad() &&
    !sub1.isMove() && !sub1.isCall() && !sub1.isIndirectCall() &&
    !sub1.isReturn() && !sub1.isJump() && !sub1.isConditionalJump() &&
    !sub1.isUnconditionalJump() && !sub1.isIndirectJump() &&
    !sub1.isAsmDataDirective() && !sub1.hasSideEffects() );

  WIR_Operation sub2(
    TC131::OpCode::SUB, TC131::OperationFormat::SDD_2,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ) );
  ufAssert( sub2.getSize() == 2 );

  WIR_Operation sub3(
    TC131::OpCode::SUB, TC131::OperationFormat::SDID_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( tricore.D15(), WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ) );
  ufAssert( sub3.getSize() == 2 );

  WIR_Operation sub4(
    TC131::OpCode::SUB, TC131::OperationFormat::SIDD,
    new WIR_RegisterParameter( tricore.D15(), WIR_Usage::def ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ) );
  ufAssert( sub4.getSize() == 2 );

  WIR_Operation sub_a1(
    TC131::OpCode::SUB_A, TC131::OperationFormat::AAA,
    new WIR_RegisterParameter( a1, WIR_Usage::def ),
    new WIR_RegisterParameter( a2, WIR_Usage::use ),
    new WIR_RegisterParameter( a3, WIR_Usage::use ) );
  ufAssert( sub_a1.getSize() == 4 );
  ufAssert(
    !sub_a1.isMemoryAccess() && !sub_a1.isMemoryStore() &&
    !sub_a1.isMemoryLoad() && !sub_a1.isMove() && !sub_a1.isCall() &&
    !sub_a1.isIndirectCall() && !sub_a1.isReturn() && !sub_a1.isJump() &&
    !sub_a1.isConditionalJump() && !sub_a1.isUnconditionalJump() &&
    !sub_a1.isIndirectJump() && !sub_a1.isAsmDataDirective() &&
    !sub_a1.hasSideEffects() );

  WIR_Operation sub_a2(
    TC131::OpCode::SUB_A, TC131::OperationFormat::SSPC8,
    new WIR_RegisterParameter( tricore.SP(), WIR_Usage::defuse ),
    new TC_Const8_Unsigned( 255 ) );
  ufAssert( sub_a2.getSize() == 2 );

  WIR_Operation sub_b1(
    TC131::OpCode::SUB_B, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( sub_b1.getSize() == 4 );
  ufAssert(
    !sub_b1.isMemoryAccess() && !sub_b1.isMemoryStore() &&
    !sub_b1.isMemoryLoad() && !sub_b1.isMove() && !sub_b1.isCall() &&
    !sub_b1.isIndirectCall() && !sub_b1.isReturn() && !sub_b1.isJump() &&
    !sub_b1.isConditionalJump() && !sub_b1.isUnconditionalJump() &&
    !sub_b1.isIndirectJump() && !sub_b1.isAsmDataDirective() &&
    !sub_b1.hasSideEffects() );

  WIR_Operation sub_f1(
    TC131::OpCode::SUB_F, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( sub_f1.getSize() == 4 );
  ufAssert(
    !sub_f1.isMemoryAccess() && !sub_f1.isMemoryStore() &&
    !sub_f1.isMemoryLoad() && !sub_f1.isMove() && !sub_f1.isCall() &&
    !sub_f1.isIndirectCall() && !sub_f1.isReturn() && !sub_f1.isJump() &&
    !sub_f1.isConditionalJump() && !sub_f1.isUnconditionalJump() &&
    !sub_f1.isIndirectJump() && !sub_f1.isAsmDataDirective() &&
    !sub_f1.hasSideEffects() );

  WIR_Operation sub_h1(
    TC131::OpCode::SUB_H, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( sub_h1.getSize() == 4 );
  ufAssert(
    !sub_h1.isMemoryAccess() && !sub_h1.isMemoryStore() &&
    !sub_h1.isMemoryLoad() && !sub_h1.isMove() && !sub_h1.isCall() &&
    !sub_h1.isIndirectCall() && !sub_h1.isReturn() && !sub_h1.isJump() &&
    !sub_h1.isConditionalJump() && !sub_h1.isUnconditionalJump() &&
    !sub_h1.isIndirectJump() && !sub_h1.isAsmDataDirective() &&
    !sub_h1.hasSideEffects() );

  WIR_Operation subc1(
    TC131::OpCode::SUBC, TC131::OperationFormat::DDDPSW_2,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( tricore.PSW_C(), WIR_Usage::defuse ) );
  ufAssert( subc1.getSize() == 4 );
  ufAssert(
    !subc1.isMemoryAccess() && !subc1.isMemoryStore() &&
    !subc1.isMemoryLoad() && !subc1.isMove() && !subc1.isCall() &&
    !subc1.isIndirectCall() && !subc1.isReturn() && !subc1.isJump() &&
    !subc1.isConditionalJump() && !subc1.isUnconditionalJump() &&
    !subc1.isIndirectJump() && !subc1.isAsmDataDirective() &&
    !subc1.hasSideEffects() );

  WIR_Operation subs1(
    TC131::OpCode::SUBS, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( subs1.getSize() == 4 );
  ufAssert(
    !subs1.isMemoryAccess() && !subs1.isMemoryStore() &&
    !subs1.isMemoryLoad() && !subs1.isMove() && !subs1.isCall() &&
    !subs1.isIndirectCall() && !subs1.isReturn() && !subs1.isJump() &&
    !subs1.isConditionalJump() && !subs1.isUnconditionalJump() &&
    !subs1.isIndirectJump() && !subs1.isAsmDataDirective() &&
    !subs1.hasSideEffects() );

  WIR_Operation subs2(
    TC131::OpCode::SUBS, TC131::OperationFormat::SDD_2,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ) );
  ufAssert( subs2.getSize() == 2 );

  WIR_Operation subs_h1(
    TC131::OpCode::SUBS_H, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( subs_h1.getSize() == 4 );
  ufAssert(
    !subs_h1.isMemoryAccess() && !subs_h1.isMemoryStore() &&
    !subs_h1.isMemoryLoad() && !subs_h1.isMove() && !subs_h1.isCall() &&
    !subs_h1.isIndirectCall() && !subs_h1.isReturn() && !subs_h1.isJump() &&
    !subs_h1.isConditionalJump() && !subs_h1.isUnconditionalJump() &&
    !subs_h1.isIndirectJump() && !subs_h1.isAsmDataDirective() &&
    !subs_h1.hasSideEffects() );

  WIR_Operation subs_hu1(
    TC131::OpCode::SUBS_HU, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( subs_hu1.getSize() == 4 );
  ufAssert(
    !subs_hu1.isMemoryAccess() && !subs_hu1.isMemoryStore() &&
    !subs_hu1.isMemoryLoad() && !subs_hu1.isMove() && !subs_hu1.isCall() &&
    !subs_hu1.isIndirectCall() && !subs_hu1.isReturn() && !subs_hu1.isJump() &&
    !subs_hu1.isConditionalJump() && !subs_hu1.isUnconditionalJump() &&
    !subs_hu1.isIndirectJump() && !subs_hu1.isAsmDataDirective() &&
    !subs_hu1.hasSideEffects() );

  WIR_Operation subs_u1(
    TC131::OpCode::SUBS_U, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( subs_u1.getSize() == 4 );
  ufAssert(
    !subs_u1.isMemoryAccess() && !subs_u1.isMemoryStore() &&
    !subs_u1.isMemoryLoad() && !subs_u1.isMove() && !subs_u1.isCall() &&
    !subs_u1.isIndirectCall() && !subs_u1.isReturn() && !subs_u1.isJump() &&
    !subs_u1.isConditionalJump() && !subs_u1.isUnconditionalJump() &&
    !subs_u1.isIndirectJump() && !subs_u1.isAsmDataDirective() &&
    !subs_u1.hasSideEffects() );

  WIR_Operation subx1(
    TC131::OpCode::SUBX, TC131::OperationFormat::DDDPSW_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new WIR_RegisterParameter( tricore.PSW_C(), WIR_Usage::def ) );
  ufAssert( subx1.getSize() == 4 );
  ufAssert(
    !subx1.isMemoryAccess() && !subx1.isMemoryStore() &&
    !subx1.isMemoryLoad() && !subx1.isMove() && !subx1.isCall() &&
    !subx1.isIndirectCall() && !subx1.isReturn() && !subx1.isJump() &&
    !subx1.isConditionalJump() && !subx1.isUnconditionalJump() &&
    !subx1.isIndirectJump() && !subx1.isAsmDataDirective() &&
    !subx1.hasSideEffects() );

  WIR_Operation svlcx1(
    TC131::OpCode::SVLCX, TC131::OperationFormat::SYS );
  ufAssert( svlcx1.getSize() == 4 );
  ufAssert(
    !svlcx1.isMemoryAccess() && svlcx1.isMemoryStore() &&
    !svlcx1.isMemoryLoad() && !svlcx1.isMove() && !svlcx1.isCall() &&
    !svlcx1.isIndirectCall() && !svlcx1.isReturn() && !svlcx1.isJump() &&
    !svlcx1.isConditionalJump() && !svlcx1.isUnconditionalJump() &&
    !svlcx1.isIndirectJump() && !svlcx1.isAsmDataDirective() &&
    svlcx1.hasSideEffects() );

  WIR_Operation swap_w1(
    TC131::OpCode::SWAP_W, TC131::OperationFormat::C18DABSA_2,
    new TC_Const18_Unsigned( 16383 ),
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ) );
  ufAssert( swap_w1.getSize() == 4 );
  ufAssert(
    !swap_w1.isMemoryAccess() && swap_w1.isMemoryStore() &&
    swap_w1.isMemoryLoad() && !swap_w1.isMove() && !swap_w1.isCall() &&
    !swap_w1.isIndirectCall() && !swap_w1.isReturn() && !swap_w1.isJump() &&
    !swap_w1.isConditionalJump() && !swap_w1.isUnconditionalJump() &&
    !swap_w1.isIndirectJump() && !swap_w1.isAsmDataDirective() &&
    !swap_w1.hasSideEffects() );

  WIR_Operation swap_w2(
    TC131::OpCode::SWAP_W, TC131::OperationFormat::LDABSA_2,
    new WIR_LabelParameter( b ),
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ) );
  ufAssert( swap_w2.getSize() == 4 );

  WIR_Operation swap_w3(
    TC131::OpCode::SWAP_W, TC131::OperationFormat::AC10DBOA_2,
    new WIR_RegisterParameter( a1, WIR_Usage::use ),
    new TC_Const10_Signed( -512 ),
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ) );
  ufAssert( swap_w3.getSize() == 4 );

  WIR_Operation swap_w4(
    TC131::OpCode::SWAP_W, TC131::OperationFormat::PDBRA_2,
    new WIR_RegisterParameter( p1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ) );
  ufAssert( swap_w4.getSize() == 4 );

  WIR_Operation swap_w5(
    TC131::OpCode::SWAP_W, TC131::OperationFormat::PC10DCA_2,
    new WIR_RegisterParameter( p1, WIR_Usage::defuse ),
    new TC_Const10_Signed( -512 ),
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ) );
  ufAssert( swap_w5.getSize() == 4 );

  WIR_Operation swap_w6(
    TC131::OpCode::SWAP_W, TC131::OperationFormat::AC10DPIA_2,
    new WIR_AddressingModeParameter( TC131::AddressingMode::post ),
    new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
    new TC_Const10_Signed( -512 ),
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ) );
  ufAssert( swap_w6.getSize() == 4 );

  WIR_Operation swap_w7(
    TC131::OpCode::SWAP_W, TC131::OperationFormat::AC10DPIA_2,
    new WIR_AddressingModeParameter( TC131::AddressingMode::pre ),
    new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
    new TC_Const10_Signed( -512 ),
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ) );
  ufAssert( swap_w7.getSize() == 4 );

  WIR_Operation syscall1(
    TC131::OpCode::SYSCALL, TC131::OperationFormat::C9PSW,
    new TC_Const9_Unsigned( 128 ),
    new WIR_RegisterParameter( tricore.PSW_C(), WIR_Usage::use ) );
  ufAssert( syscall1.getSize() == 4 );
  ufAssert(
    !syscall1.isMemoryAccess() && !syscall1.isMemoryStore() &&
    !syscall1.isMemoryLoad() && !syscall1.isMove() && !syscall1.isCall() &&
    !syscall1.isIndirectCall() && !syscall1.isReturn() && !syscall1.isJump() &&
    !syscall1.isConditionalJump() && !syscall1.isUnconditionalJump() &&
    !syscall1.isIndirectJump() && !syscall1.isAsmDataDirective() &&
    syscall1.hasSideEffects() );

  WIR_Operation tlbdemap1(
    TC131::OpCode::TLBDEMAP, TC131::OperationFormat::D,
    new WIR_RegisterParameter( d1, WIR_Usage::use ) );
  ufAssert( tlbdemap1.getSize() == 4 );
  ufAssert(
    !tlbdemap1.isMemoryAccess() && !tlbdemap1.isMemoryStore() &&
    !tlbdemap1.isMemoryLoad() && !tlbdemap1.isMove() && !tlbdemap1.isCall() &&
    !tlbdemap1.isIndirectCall() && !tlbdemap1.isReturn() &&
    !tlbdemap1.isJump() &&  !tlbdemap1.isConditionalJump() &&
    !tlbdemap1.isUnconditionalJump() && !tlbdemap1.isIndirectJump() &&
    !tlbdemap1.isAsmDataDirective() && tlbdemap1.hasSideEffects() );

  WIR_Operation tlbflush_a1(
    TC131::OpCode::TLBFLUSH_A, TC131::OperationFormat::SYS );
  ufAssert( tlbflush_a1.getSize() == 4 );
  ufAssert(
    !tlbflush_a1.isMemoryAccess() && !tlbflush_a1.isMemoryStore() &&
    !tlbflush_a1.isMemoryLoad() && !tlbflush_a1.isMove() &&
    !tlbflush_a1.isCall() && !tlbflush_a1.isIndirectCall() &&
    !tlbflush_a1.isReturn() && !tlbflush_a1.isJump() &&
    !tlbflush_a1.isConditionalJump() && !tlbflush_a1.isUnconditionalJump() &&
    !tlbflush_a1.isIndirectJump() && !tlbflush_a1.isAsmDataDirective() &&
    tlbflush_a1.hasSideEffects() );

  WIR_Operation tlbflush_b1(
    TC131::OpCode::TLBFLUSH_B, TC131::OperationFormat::SYS );
  ufAssert( tlbflush_b1.getSize() == 4 );
  ufAssert(
    !tlbflush_b1.isMemoryAccess() && !tlbflush_b1.isMemoryStore() &&
    !tlbflush_b1.isMemoryLoad() && !tlbflush_b1.isMove() &&
    !tlbflush_b1.isCall() && !tlbflush_b1.isIndirectCall() &&
    !tlbflush_b1.isReturn() && !tlbflush_b1.isJump() &&
    !tlbflush_b1.isConditionalJump() && !tlbflush_b1.isUnconditionalJump() &&
    !tlbflush_b1.isIndirectJump() && !tlbflush_b1.isAsmDataDirective() &&
    tlbflush_b1.hasSideEffects() );

  WIR_Operation tlbmap1(
    TC131::OpCode::TLBMAP, TC131::OperationFormat::E,
    new WIR_RegisterParameter( e1, WIR_Usage::use ) );
  ufAssert( tlbmap1.getSize() == 4 );
  ufAssert(
    !tlbmap1.isMemoryAccess() && !tlbmap1.isMemoryStore() &&
    !tlbmap1.isMemoryLoad() && !tlbmap1.isMove() && !tlbmap1.isCall() &&
    !tlbmap1.isIndirectCall() && !tlbmap1.isReturn() && !tlbmap1.isJump() &&
    !tlbmap1.isConditionalJump() && !tlbmap1.isUnconditionalJump() &&
    !tlbmap1.isIndirectJump() && !tlbmap1.isAsmDataDirective() &&
    tlbmap1.hasSideEffects() );

  WIR_Operation tlbprobe_a1(
    TC131::OpCode::TLBPROBE_A, TC131::OperationFormat::D,
    new WIR_RegisterParameter( d1, WIR_Usage::use ) );
  ufAssert( tlbprobe_a1.getSize() == 4 );
  ufAssert(
    !tlbprobe_a1.isMemoryAccess() && !tlbprobe_a1.isMemoryStore() &&
    !tlbprobe_a1.isMemoryLoad() && !tlbprobe_a1.isMove() &&
    !tlbprobe_a1.isCall() && !tlbprobe_a1.isIndirectCall() &&
    !tlbprobe_a1.isReturn() && !tlbprobe_a1.isJump() &&
    !tlbprobe_a1.isConditionalJump() && !tlbprobe_a1.isUnconditionalJump() &&
    !tlbprobe_a1.isIndirectJump() && !tlbprobe_a1.isAsmDataDirective() &&
    tlbprobe_a1.hasSideEffects() );

  WIR_Operation tlbprobe_i1(
    TC131::OpCode::TLBPROBE_I, TC131::OperationFormat::D,
    new WIR_RegisterParameter( d1, WIR_Usage::use ) );
  ufAssert( tlbprobe_i1.getSize() == 4 );
  ufAssert(
    !tlbprobe_i1.isMemoryAccess() && !tlbprobe_i1.isMemoryStore() &&
    !tlbprobe_i1.isMemoryLoad() && !tlbprobe_i1.isMove() &&
    !tlbprobe_i1.isCall() && !tlbprobe_i1.isIndirectCall() &&
    !tlbprobe_i1.isReturn() && !tlbprobe_i1.isJump() &&
    !tlbprobe_i1.isConditionalJump() && !tlbprobe_i1.isUnconditionalJump() &&
    !tlbprobe_i1.isIndirectJump() && !tlbprobe_i1.isAsmDataDirective() &&
    tlbprobe_i1.hasSideEffects() );

  WIR_Operation trapsv1(
    TC131::OpCode::TRAPSV, TC131::OperationFormat::SYS );
  ufAssert( trapsv1.getSize() == 4 );
  ufAssert(
    !trapsv1.isMemoryAccess() && !trapsv1.isMemoryStore() &&
    !trapsv1.isMemoryLoad() && !trapsv1.isMove() && !trapsv1.isCall() &&
    !trapsv1.isIndirectCall() && !trapsv1.isReturn() && !trapsv1.isJump() &&
    !trapsv1.isConditionalJump() && !trapsv1.isUnconditionalJump() &&
    !trapsv1.isIndirectJump() && !trapsv1.isAsmDataDirective() &&
    trapsv1.hasSideEffects() );

  WIR_Operation trapv1(
    TC131::OpCode::TRAPV, TC131::OperationFormat::SYS );
  ufAssert( trapv1.getSize() == 4 );
  ufAssert(
    !trapv1.isMemoryAccess() && !trapv1.isMemoryStore() &&
    !trapv1.isMemoryLoad() && !trapv1.isMove() && !trapv1.isCall() &&
    !trapv1.isIndirectCall() && !trapv1.isReturn() && !trapv1.isJump() &&
    !trapv1.isConditionalJump() && !trapv1.isUnconditionalJump() &&
    !trapv1.isIndirectJump() && !trapv1.isAsmDataDirective() &&
    trapv1.hasSideEffects() );

  WIR_Operation unpack1(
    TC131::OpCode::UNPACK, TC131::OperationFormat::ED,
    new WIR_RegisterParameter( e1, WIR_Usage::def ),
    new WIR_RegisterParameter( d1, WIR_Usage::use ) );
  ufAssert( unpack1.getSize() == 4 );
  ufAssert(
    !unpack1.isMemoryAccess() && !unpack1.isMemoryStore() &&
    !unpack1.isMemoryLoad() && !unpack1.isMove() && !unpack1.isCall() &&
    !unpack1.isIndirectCall() && !unpack1.isReturn() && !unpack1.isJump() &&
    !unpack1.isConditionalJump() && !unpack1.isUnconditionalJump() &&
    !unpack1.isIndirectJump() && !unpack1.isAsmDataDirective() &&
    !unpack1.hasSideEffects() );

  WIR_Operation updfl1(
    TC131::OpCode::UPDFL, TC131::OperationFormat::D,
    new WIR_RegisterParameter( d1, WIR_Usage::use ) );
  ufAssert( updfl1.getSize() == 4 );
  ufAssert(
    !updfl1.isMemoryAccess() && !updfl1.isMemoryStore() &&
    !updfl1.isMemoryLoad() && !updfl1.isMove() && !updfl1.isCall() &&
    !updfl1.isIndirectCall() && !updfl1.isReturn() && !updfl1.isJump() &&
    !updfl1.isConditionalJump() && !updfl1.isUnconditionalJump() &&
    !updfl1.isIndirectJump() && !updfl1.isAsmDataDirective() &&
    updfl1.hasSideEffects() );

  WIR_Operation utof1(
    TC131::OpCode::UTOF, TC131::OperationFormat::DD,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ) );
  ufAssert( utof1.getSize() == 4 );
  ufAssert(
    !utof1.isMemoryAccess() && !utof1.isMemoryStore() &&
    !utof1.isMemoryLoad() && !utof1.isMove() && !utof1.isCall() &&
    !utof1.isIndirectCall() && !utof1.isReturn() && !utof1.isJump() &&
    !utof1.isConditionalJump() && !utof1.isUnconditionalJump() &&
    !utof1.isIndirectJump() && !utof1.isAsmDataDirective() &&
    !utof1.hasSideEffects() );

  WIR_Operation xnor1(
    TC131::OpCode::XNOR, TC131::OperationFormat::DDC9_2,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const9_Unsigned( 128 ) );
  ufAssert( xnor1.getSize() == 4 );
  ufAssert(
    !xnor1.isMemoryAccess() && !xnor1.isMemoryStore() &&
    !xnor1.isMemoryLoad() && !xnor1.isMove() && !xnor1.isCall() &&
    !xnor1.isIndirectCall() && !xnor1.isReturn() && !xnor1.isJump() &&
    !xnor1.isConditionalJump() && !xnor1.isUnconditionalJump() &&
    !xnor1.isIndirectJump() && !xnor1.isAsmDataDirective() &&
    !xnor1.hasSideEffects() );

  WIR_Operation xnor2(
    TC131::OpCode::XNOR, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( xnor2.getSize() == 4 );

  WIR_Operation xnor_t1(
    TC131::OpCode::XNOR_T, TC131::OperationFormat::DDC5DC5_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const5_Unsigned( 13 ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new TC_Const5_Unsigned( 27 ) );
  ufAssert( xnor_t1.getSize() == 4 );
  ufAssert(
    !xnor_t1.isMemoryAccess() && !xnor_t1.isMemoryStore() &&
    !xnor_t1.isMemoryLoad() && !xnor_t1.isMove() && !xnor_t1.isCall() &&
    !xnor_t1.isIndirectCall() && !xnor_t1.isReturn() && !xnor_t1.isJump() &&
    !xnor_t1.isConditionalJump() && !xnor_t1.isUnconditionalJump() &&
    !xnor_t1.isIndirectJump() && !xnor_t1.isAsmDataDirective() &&
    !xnor_t1.hasSideEffects() );

  WIR_Operation xor1(
    TC131::OpCode::XOR, TC131::OperationFormat::DDC9_2,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const9_Unsigned( 128 ) );
  ufAssert( xor1.getSize() == 4 );
  ufAssert(
    !xor1.isMemoryAccess() && !xor1.isMemoryStore() && !xor1.isMemoryLoad() &&
    !xor1.isMove() && !xor1.isCall() && !xor1.isIndirectCall() &&
    !xor1.isReturn() && !xor1.isJump() && !xor1.isConditionalJump() &&
    !xor1.isUnconditionalJump() && !xor1.isIndirectJump() &&
    !xor1.isAsmDataDirective() && !xor1.hasSideEffects() );

  WIR_Operation xor2(
    TC131::OpCode::XOR, TC131::OperationFormat::DDD_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( xor2.getSize() == 4 );

  WIR_Operation xor3(
    TC131::OpCode::XOR, TC131::OperationFormat::SDD_2,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ) );
  ufAssert( xor3.getSize() == 2 );

  WIR_Operation xor_eq1(
    TC131::OpCode::XOR_EQ, TC131::OperationFormat::DDC9_3,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const9_Signed( -128 ) );
  ufAssert( xor_eq1.getSize() == 4 );
  ufAssert(
    !xor_eq1.isMemoryAccess() && !xor_eq1.isMemoryStore() &&
    !xor_eq1.isMemoryLoad() && !xor_eq1.isMove() && !xor_eq1.isCall() &&
    !xor_eq1.isIndirectCall() && !xor_eq1.isReturn() && !xor_eq1.isJump() &&
    !xor_eq1.isConditionalJump() && !xor_eq1.isUnconditionalJump() &&
    !xor_eq1.isIndirectJump() && !xor_eq1.isAsmDataDirective() &&
    !xor_eq1.hasSideEffects() );

  WIR_Operation xor_eq2(
    TC131::OpCode::XOR_EQ, TC131::OperationFormat::DDD_2,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( xor_eq2.getSize() == 4 );

  WIR_Operation xor_ge1(
    TC131::OpCode::XOR_GE, TC131::OperationFormat::DDC9_3,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const9_Signed( -128 ) );
  ufAssert( xor_ge1.getSize() == 4 );
  ufAssert(
    !xor_ge1.isMemoryAccess() && !xor_ge1.isMemoryStore() &&
    !xor_ge1.isMemoryLoad() && !xor_ge1.isMove() && !xor_ge1.isCall() &&
    !xor_ge1.isIndirectCall() && !xor_ge1.isReturn() && !xor_ge1.isJump() &&
    !xor_ge1.isConditionalJump() && !xor_ge1.isUnconditionalJump() &&
    !xor_ge1.isIndirectJump() && !xor_ge1.isAsmDataDirective() &&
    !xor_ge1.hasSideEffects() );

  WIR_Operation xor_ge2(
    TC131::OpCode::XOR_GE, TC131::OperationFormat::DDD_2,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( xor_ge2.getSize() == 4 );

  WIR_Operation xor_ge_u1(
    TC131::OpCode::XOR_GE_U, TC131::OperationFormat::DDC9_4,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const9_Unsigned( 128 ) );
  ufAssert( xor_ge_u1.getSize() == 4 );
  ufAssert(
    !xor_ge_u1.isMemoryAccess() && !xor_ge_u1.isMemoryStore() &&
    !xor_ge_u1.isMemoryLoad() && !xor_ge_u1.isMove() && !xor_ge_u1.isCall() &&
    !xor_ge_u1.isIndirectCall() && !xor_ge_u1.isReturn() &&
    !xor_ge_u1.isJump() && !xor_ge_u1.isConditionalJump() &&
    !xor_ge_u1.isUnconditionalJump() && !xor_ge_u1.isIndirectJump() &&
    !xor_ge_u1.isAsmDataDirective() && !xor_ge_u1.hasSideEffects() );

  WIR_Operation xor_ge_u2(
    TC131::OpCode::XOR_GE_U, TC131::OperationFormat::DDD_2,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( xor_ge_u2.getSize() == 4 );

  WIR_Operation xor_lt1(
    TC131::OpCode::XOR_LT, TC131::OperationFormat::DDC9_3,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const9_Signed( -128 ) );
  ufAssert( xor_lt1.getSize() == 4 );
  ufAssert(
    !xor_lt1.isMemoryAccess() && !xor_lt1.isMemoryStore() &&
    !xor_lt1.isMemoryLoad() && !xor_lt1.isMove() && !xor_lt1.isCall() &&
    !xor_lt1.isIndirectCall() && !xor_lt1.isReturn() && !xor_lt1.isJump() &&
    !xor_lt1.isConditionalJump() && !xor_lt1.isUnconditionalJump() &&
    !xor_lt1.isIndirectJump() && !xor_lt1.isAsmDataDirective() &&
    !xor_lt1.hasSideEffects() );

  WIR_Operation xor_lt2(
    TC131::OpCode::XOR_LT, TC131::OperationFormat::DDD_2,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( xor_lt2.getSize() == 4 );

  WIR_Operation xor_lt_u1(
    TC131::OpCode::XOR_LT_U, TC131::OperationFormat::DDC9_4,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const9_Unsigned( 128 ) );
  ufAssert( xor_lt_u1.getSize() == 4 );
  ufAssert(
    !xor_lt_u1.isMemoryAccess() && !xor_lt_u1.isMemoryStore() &&
    !xor_lt_u1.isMemoryLoad() && !xor_lt_u1.isMove() && !xor_lt_u1.isCall() &&
    !xor_lt_u1.isIndirectCall() && !xor_lt_u1.isReturn() &&
    !xor_lt_u1.isJump() && !xor_lt_u1.isConditionalJump() &&
    !xor_lt_u1.isUnconditionalJump() && !xor_lt_u1.isIndirectJump() &&
    !xor_lt_u1.isAsmDataDirective() && !xor_lt_u1.hasSideEffects() );

  WIR_Operation xor_lt_u2(
    TC131::OpCode::XOR_LT_U, TC131::OperationFormat::DDD_2,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( xor_lt_u2.getSize() == 4 );

  WIR_Operation xor_ne1(
    TC131::OpCode::XOR_NE, TC131::OperationFormat::DDC9_3,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const9_Signed( -128 ) );
  ufAssert( xor_ne1.getSize() == 4 );
  ufAssert(
    !xor_ne1.isMemoryAccess() && !xor_ne1.isMemoryStore() &&
    !xor_ne1.isMemoryLoad() && !xor_ne1.isMove() && !xor_ne1.isCall() &&
    !xor_ne1.isIndirectCall() && !xor_ne1.isReturn() && !xor_ne1.isJump() &&
    !xor_ne1.isConditionalJump() && !xor_ne1.isUnconditionalJump() &&
    !xor_ne1.isIndirectJump() && !xor_ne1.isAsmDataDirective() &&
    !xor_ne1.hasSideEffects() );

  WIR_Operation xor_ne2(
    TC131::OpCode::XOR_NE, TC131::OperationFormat::DDD_2,
    new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ) );
  ufAssert( xor_ne2.getSize() == 4 );

  WIR_Operation xor_t1(
    TC131::OpCode::XOR_T, TC131::OperationFormat::DDC5DC5_1,
    new WIR_RegisterParameter( d1, WIR_Usage::def ),
    new WIR_RegisterParameter( d2, WIR_Usage::use ),
    new TC_Const5_Unsigned( 13 ),
    new WIR_RegisterParameter( d3, WIR_Usage::use ),
    new TC_Const5_Unsigned( 27 ) );
  ufAssert( xor_t1.getSize() == 4 );
  ufAssert(
    !xor_t1.isMemoryAccess() && !xor_t1.isMemoryStore() &&
    !xor_t1.isMemoryLoad() && !xor_t1.isMove() && !xor_t1.isCall() &&
    !xor_t1.isIndirectCall() && !xor_t1.isReturn() && !xor_t1.isJump() &&
    !xor_t1.isConditionalJump() && !xor_t1.isUnconditionalJump() &&
    !xor_t1.isIndirectJump() && !xor_t1.isAsmDataDirective() &&
    !xor_t1.hasSideEffects() );

  return( 0 );
}
