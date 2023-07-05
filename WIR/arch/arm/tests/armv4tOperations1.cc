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

// Include standard headers
#include <vector>

// Include libuseful headers
#include <libuseful/io.h>

// Include WIR headers
#include <wir/wir.h>
#include <arch/arm/armv4t.h>


using namespace std;
using namespace WIR;


int main( void )
{
  WIR_Init();

  ARMv4T p;
  const ARM_LoRegP &r1 = p.R2(), &r2 = p.R6(), &r3 = p.R0(), &r6 = p.R1(),
                   &r7 = p.R3(), &r8 = p.R4(), &r9 = p.R5(), &r10 = p.R7();
  const ARM_HiRegP &r4 = p.R11(), &r5 = p.R9();
  WIR_BasicBlock b;
  WIR_Function f( "main" );

  // The following operations must be accepted for the ARMv4T ISA.
  const vector<ARM_Base::Condition> conditions {
    ARM_Base::Condition::eq,
    ARM_Base::Condition::ne,
    ARM_Base::Condition::hs,
    ARM_Base::Condition::lo,
    ARM_Base::Condition::mi,
    ARM_Base::Condition::pl,
    ARM_Base::Condition::vs,
    ARM_Base::Condition::vc,
    ARM_Base::Condition::hi,
    ARM_Base::Condition::ls,
    ARM_Base::Condition::ge,
    ARM_Base::Condition::lt,
    ARM_Base::Condition::gt,
    ARM_Base::Condition::le,
    ARM_Base::Condition::al };

  for ( auto &cond : conditions ) {
    WIR_Operation o(
      ARMv4T::OpCode::BX, ARMv4T::OperationFormat::CR_3,
      WIR_ConditionFieldParameter( cond ),
      WIR_RegisterParameter( r2, WIR_Usage::use ) );

    ufAssert( o.getSize() == 4 );
    ufAssert(
      !o.isMemoryAccess() && !o.isMemoryStore() && !o.isMemoryLoad() &&
      !o.isMove() && !o.isCall() && !o.isIndirectCall() && !o.isReturn() &&
      o.isJump() && !o.isConditionalJump() && !o.isUnconditionalJump() &&
      o.isIndirectJump() );
  }

  WIR_Operation o1(
    ARMv4T::OpCode::ADC, ARMv4T::OperationFormat::TRR_5,
    WIR_RegisterParameter( r1, WIR_Usage::defuse ),
    WIR_RegisterParameter( r2, WIR_Usage::use ) );

  ufAssert( o1.getSize() == 2 );
  ufAssert(
    !o1.isMemoryAccess() && !o1.isMemoryStore() && !o1.isMemoryLoad() &&
    !o1.isMove() && !o1.isCall() && !o1.isIndirectCall() && !o1.isReturn() &&
    !o1.isJump() && !o1.isConditionalJump() && !o1.isUnconditionalJump() &&
    !o1.isIndirectJump() );

  WIR_Operation o2(
    ARM_Base::OpCode::ADD, ARMv4T::OperationFormat::TRRC3_1,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    ARM_Const3_Unsigned( 0 ) );

  ufAssert( o2.getSize() == 2 );
  ufAssert(
    !o2.isMemoryAccess() && !o2.isMemoryStore() && !o2.isMemoryLoad() &&
    !o2.isMove() && !o2.isCall() && !o2.isIndirectCall() && !o2.isReturn() &&
    !o2.isJump() && !o2.isConditionalJump() && !o2.isUnconditionalJump() &&
    !o2.isIndirectJump() );

  WIR_Operation o3(
    ARM_Base::OpCode::ADD, ARMv4T::OperationFormat::TRC8_3,
    WIR_RegisterParameter( r1, WIR_Usage::defuse ),
    ARM_Const8_Unsigned( 255 ) );

  ufAssert( o3.getSize() == 2 );
  ufAssert(
    !o3.isMemoryAccess() && !o3.isMemoryStore() && !o3.isMemoryLoad() &&
    !o3.isMove() && !o3.isCall() && !o3.isIndirectCall() && !o3.isReturn() &&
    !o3.isJump() && !o3.isConditionalJump() && !o3.isUnconditionalJump() &&
    !o3.isIndirectJump() );

  WIR_Operation o4(
    ARM_Base::OpCode::ADD, ARMv4T::OperationFormat::TRRR_1,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( r3, WIR_Usage::use ) );

  ufAssert( o4.getSize() == 2 );
  ufAssert(
    !o4.isMemoryAccess() && !o4.isMemoryStore() && !o4.isMemoryLoad() &&
    !o4.isMove() && !o4.isCall() && !o4.isIndirectCall() && !o4.isReturn() &&
    !o4.isJump() && !o4.isConditionalJump() && !o4.isUnconditionalJump() &&
    !o4.isIndirectJump() );

  WIR_Operation o5(
    ARM_Base::OpCode::ADD, ARMv4T::OperationFormat::TRR_6,
    WIR_RegisterParameter( r4, WIR_Usage::defuse ),
    WIR_RegisterParameter( r5, WIR_Usage::use ) );

  ufAssert( o5.getSize() == 2 );
  ufAssert(
    !o5.isMemoryAccess() && !o5.isMemoryStore() && !o5.isMemoryLoad() &&
    !o5.isMove() && !o5.isCall() && !o5.isIndirectCall() && !o5.isReturn() &&
    !o5.isJump() && !o5.isConditionalJump() && !o5.isUnconditionalJump() &&
    !o5.isIndirectJump() );

  WIR_Operation o6(
    ARM_Base::OpCode::ADD, ARMv4T::OperationFormat::TRPCC10_1,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( p.PC(), WIR_Usage::use ),
    ARM_Const10_Unsigned4( 1020 ) );

  ufAssert( o6.getSize() == 2 );
  ufAssert(
    !o6.isMemoryAccess() && !o6.isMemoryStore() && !o6.isMemoryLoad() &&
    !o6.isMove() && !o6.isCall() && !o6.isIndirectCall() && !o6.isReturn() &&
    !o6.isJump() && !o6.isConditionalJump() && !o6.isUnconditionalJump() &&
    !o6.isIndirectJump() );

  WIR_Operation o7(
    ARM_Base::OpCode::ADD, ARMv4T::OperationFormat::TRSPC10_1,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( p.SP(), WIR_Usage::use ),
    ARM_Const10_Unsigned4( 1020 ) );

  ufAssert( o7.getSize() == 2 );
  ufAssert(
    !o7.isMemoryAccess() && !o7.isMemoryStore() && !o7.isMemoryLoad() &&
    !o7.isMove() && !o7.isCall() && !o7.isIndirectCall() && !o7.isReturn() &&
    !o7.isJump() && !o7.isConditionalJump() && !o7.isUnconditionalJump() &&
    !o7.isIndirectJump() );

  WIR_Operation o8(
    ARM_Base::OpCode::ADD, ARMv4T::OperationFormat::TC9_1,
    ARM_Const9_Unsigned( 44 ) );

  ufAssert( o8.getSize() == 2 );
  ufAssert(
    !o8.isMemoryAccess() && !o8.isMemoryStore() && !o8.isMemoryLoad() &&
    !o8.isMove() && !o8.isCall() && !o8.isIndirectCall() && !o8.isReturn() &&
    !o8.isJump() && !o8.isConditionalJump() && !o8.isUnconditionalJump() &&
    !o8.isIndirectJump() );

  WIR_Operation o9(
    ARM_Base::OpCode::AND, ARMv4T::OperationFormat::TRR_5,
    WIR_RegisterParameter( r1, WIR_Usage::defuse ),
    WIR_RegisterParameter( r2, WIR_Usage::use ) );

  ufAssert( o9.getSize() == 2 );
  ufAssert(
    !o9.isMemoryAccess() && !o9.isMemoryStore() && !o9.isMemoryLoad() &&
    !o9.isMove() && !o9.isCall() && !o9.isIndirectCall() && !o9.isReturn() &&
    !o9.isJump() && !o9.isConditionalJump() && !o9.isUnconditionalJump() &&
    !o9.isIndirectJump() );

  WIR_Operation o10(
    ARMv4T::OpCode::ASR, ARMv4T::OperationFormat::TRRC6_1,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    ARM_Const6_Unsigned0( 17 ) );

  ufAssert( o10.getSize() == 2 );
  ufAssert(
    !o10.isMemoryAccess() && !o10.isMemoryStore() && !o10.isMemoryLoad() &&
    !o10.isMove() && !o10.isCall() && !o10.isIndirectCall() &&
    !o10.isReturn() && !o10.isJump() && !o10.isConditionalJump() &&
    !o10.isUnconditionalJump() && !o10.isIndirectJump() );

  WIR_Operation o11(
    ARMv4T::OpCode::ASR, ARMv4T::OperationFormat::TRR_5,
    WIR_RegisterParameter( r1, WIR_Usage::defuse ),
    WIR_RegisterParameter( r2, WIR_Usage::use ) );

  ufAssert( o11.getSize() == 2 );
  ufAssert(
    !o11.isMemoryAccess() && !o11.isMemoryStore() && !o11.isMemoryLoad() &&
    !o11.isMove() && !o11.isCall() && !o11.isIndirectCall() &&
    !o11.isReturn() && !o11.isJump() && !o11.isConditionalJump() &&
    !o11.isUnconditionalJump() && !o11.isIndirectJump() );

  for ( auto &cond : conditions ) {
    WIR_Operation o(
      ARM_Base::OpCode::B, ARMv4T::OperationFormat::TCL,
      WIR_ConditionFieldParameter( cond ),
      WIR_LabelParameter( b ) );

    ufAssert( o.getSize() == 2 );
    if ( cond == ARM_Base::Condition::al )
      ufAssert(
        !o.isMemoryAccess() && !o.isMemoryStore() && !o.isMemoryLoad() &&
        !o.isMove() && !o.isCall() && !o.isIndirectCall() && !o.isReturn() &&
        o.isJump() && !o.isConditionalJump() && o.isUnconditionalJump() &&
        !o.isIndirectJump() );
    else {
      ufAssert(
        !o.isMemoryAccess() && !o.isMemoryStore() && !o.isMemoryLoad() &&
        !o.isMove() && !o.isCall() && !o.isIndirectCall() && !o.isReturn() &&
        o.isJump() && o.isConditionalJump() && !o.isUnconditionalJump() &&
        !o.isIndirectJump() );
    }
  }

  WIR_Operation o12(
    ARMv4T::OpCode::B, ARMv4T::OperationFormat::TL_1, WIR_LabelParameter( b ) );

  ufAssert( o12.getSize() == 2 );
  ufAssert(
    !o12.isMemoryAccess() && !o12.isMemoryStore() && !o12.isMemoryLoad() &&
    !o12.isMove() && !o12.isCall() && !o12.isIndirectCall() &&
    !o12.isReturn() && o12.isJump() && !o12.isConditionalJump() &&
    o12.isUnconditionalJump() && !o12.isIndirectJump() );

  WIR_Operation o13(
    ARMv4T::OpCode::BIC, ARMv4T::OperationFormat::TRR_5,
    WIR_RegisterParameter( r1, WIR_Usage::defuse ),
    WIR_RegisterParameter( r2, WIR_Usage::use ) );

  ufAssert( o13.getSize() == 2 );
  ufAssert(
    !o13.isMemoryAccess() && !o13.isMemoryStore() && !o13.isMemoryLoad() &&
    !o13.isMove() && !o13.isCall() && !o13.isIndirectCall() &&
    !o13.isReturn() && !o13.isJump() && !o13.isConditionalJump() &&
    !o13.isUnconditionalJump() && !o13.isIndirectJump() );

  WIR_Operation o14(
    ARMv4T::OpCode::BL, ARMv4T::OperationFormat::TL_2,
    WIR_LabelParameter( f ) );

  ufAssert( o14.getSize() == 4 );
  ufAssert(
    !o14.isMemoryAccess() && !o14.isMemoryStore() && !o14.isMemoryLoad() &&
    !o14.isMove() && o14.isCall() && !o14.isIndirectCall() && !o14.isReturn() &&
    !o14.isJump() && !o14.isConditionalJump() && !o14.isUnconditionalJump() &&
    !o14.isIndirectJump() );

  WIR_Operation o15(
    ARMv4T::OpCode::BX, ARMv4T::OperationFormat::TR_1,
    WIR_RegisterParameter( r2, WIR_Usage::use ) );

  ufAssert( o15.getSize() == 2 );
  ufAssert(
    !o15.isMemoryAccess() && !o15.isMemoryStore() && !o15.isMemoryLoad() &&
    !o15.isMove() && !o15.isCall() && !o15.isIndirectCall() &&
    !o15.isReturn() && o15.isJump() && !o15.isConditionalJump() &&
    !o15.isUnconditionalJump() && o15.isIndirectJump() );

  WIR_Operation o16(
    ARMv4T::OpCode::BX, ARMv4T::OperationFormat::TR_1,
    WIR_RegisterParameter( p.R14(), WIR_Usage::use ) );

  ufAssert( o16.getSize() == 2 );
  ufAssert(
    !o16.isMemoryAccess() && !o16.isMemoryStore() && !o16.isMemoryLoad() &&
    !o16.isMove() && !o16.isCall() && !o16.isIndirectCall() && o16.isReturn() &&
    !o16.isJump() && !o16.isConditionalJump() && !o16.isUnconditionalJump() &&
    !o16.isIndirectJump() );

  WIR_Operation o17(
    ARMv4T::OpCode::CMN, ARMv4T::OperationFormat::TRR_5,
    WIR_RegisterParameter( r1, WIR_Usage::defuse ),
    WIR_RegisterParameter( r2, WIR_Usage::use ) );

  ufAssert( o17.getSize() == 2 );
  ufAssert(
    !o17.isMemoryAccess() && !o17.isMemoryStore() && !o17.isMemoryLoad() &&
    !o17.isMove() && !o17.isCall() && !o17.isIndirectCall() &&
    !o17.isReturn() && !o17.isJump() && !o17.isConditionalJump() &&
    !o17.isUnconditionalJump() && !o17.isIndirectJump() );

  WIR_Operation o18(
    ARM_Base::OpCode::CMP, ARMv4T::OperationFormat::TRC8_2,
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    ARM_Const8_Unsigned( 255 ) );

  ufAssert( o18.getSize() == 2 );
  ufAssert(
    !o18.isMemoryAccess() && !o18.isMemoryStore() && !o18.isMemoryLoad() &&
    !o18.isMove() && !o18.isCall() && !o18.isIndirectCall() &&
    !o18.isReturn() && !o18.isJump() && !o18.isConditionalJump() &&
    !o18.isUnconditionalJump() && !o18.isIndirectJump() );

  WIR_Operation o19(
    ARM_Base::OpCode::CMP, ARMv4T::OperationFormat::TRR_3,
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    WIR_RegisterParameter( r2, WIR_Usage::use ) );

  ufAssert( o19.getSize() == 2 );
  ufAssert(
    !o19.isMemoryAccess() && !o19.isMemoryStore() && !o19.isMemoryLoad() &&
    !o19.isMove() && !o19.isCall() && !o19.isIndirectCall() &&
    !o19.isReturn() && !o19.isJump() && !o19.isConditionalJump() &&
    !o19.isUnconditionalJump() && !o19.isIndirectJump() );

  WIR_Operation o20(
    ARM_Base::OpCode::CMP, ARMv4T::OperationFormat::TRR_4,
    WIR_RegisterParameter( r4, WIR_Usage::use ),
    WIR_RegisterParameter( r5, WIR_Usage::use ) );

  ufAssert( o20.getSize() == 2 );
  ufAssert(
    !o20.isMemoryAccess() && !o20.isMemoryStore() && !o20.isMemoryLoad() &&
    !o20.isMove() && !o20.isCall() && !o20.isIndirectCall() &&
    !o20.isReturn() && !o20.isJump() && !o20.isConditionalJump() &&
    !o20.isUnconditionalJump() && !o20.isIndirectJump() );

  WIR_Operation o21(
    ARMv4T::OpCode::EOR, ARMv4T::OperationFormat::TRR_5,
    WIR_RegisterParameter( r1, WIR_Usage::defuse ),
    WIR_RegisterParameter( r2, WIR_Usage::use ) );

  ufAssert( o21.getSize() == 2 );
  ufAssert(
    !o21.isMemoryAccess() && !o21.isMemoryStore() && !o21.isMemoryLoad() &&
    !o21.isMove() && !o21.isCall() && !o21.isIndirectCall() &&
    !o21.isReturn() && !o21.isJump() && !o21.isConditionalJump() &&
    !o21.isUnconditionalJump() && !o21.isIndirectJump() );

  WIR_Operation o22(
    ARMv4T::OpCode::LDMIA, ARMv4T::OperationFormat::TRR1_1,
    WIR_RegisterParameter( r1, WIR_Usage::defuse ),
    WIR_RegisterParameter( r2, WIR_Usage::def ) );

  ufAssert( o22.getSize() == 2 );
  ufAssert(
    !o22.isMemoryAccess() && !o22.isMemoryStore() && o22.isMemoryLoad() &&
    !o22.isMove() && !o22.isCall() && !o22.isIndirectCall() &&
    !o22.isReturn() && !o22.isJump() && !o22.isConditionalJump() &&
    !o22.isUnconditionalJump() && !o22.isIndirectJump() );

  WIR_Operation o23(
    ARMv4T::OpCode::LDMIA, ARMv4T::OperationFormat::TRR2_1,
    WIR_RegisterParameter( r1, WIR_Usage::defuse ),
    WIR_RegisterParameter( r2, WIR_Usage::def ),
    WIR_RegisterParameter( r3, WIR_Usage::def ) );

  ufAssert( o23.getSize() == 2 );
  ufAssert(
    !o23.isMemoryAccess() && !o23.isMemoryStore() && o23.isMemoryLoad() &&
    !o23.isMove() && !o23.isCall() && !o23.isIndirectCall() &&
    !o23.isReturn() && !o23.isJump() && !o23.isConditionalJump() &&
    !o23.isUnconditionalJump() && !o23.isIndirectJump() );

  WIR_Operation o24(
    ARMv4T::OpCode::LDMIA, ARMv4T::OperationFormat::TRR3_1,
    WIR_RegisterParameter( r1, WIR_Usage::defuse ),
    WIR_RegisterParameter( r2, WIR_Usage::def ),
    WIR_RegisterParameter( r3, WIR_Usage::def ),
    WIR_RegisterParameter( r1, WIR_Usage::def ) );

  ufAssert( o24.getSize() == 2 );
  ufAssert(
    !o24.isMemoryAccess() && !o24.isMemoryStore() && o24.isMemoryLoad() &&
    !o24.isMove() && !o24.isCall() && !o24.isIndirectCall() &&
    !o24.isReturn() && !o24.isJump() && !o24.isConditionalJump() &&
    !o24.isUnconditionalJump() && !o24.isIndirectJump() );

  WIR_Operation o25(
    ARMv4T::OpCode::LDMIA, ARMv4T::OperationFormat::TRR4_1,
    WIR_RegisterParameter( r1, WIR_Usage::defuse ),
    WIR_RegisterParameter( r2, WIR_Usage::def ),
    WIR_RegisterParameter( r3, WIR_Usage::def ),
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r6, WIR_Usage::def ) );

  ufAssert( o25.getSize() == 2 );
  ufAssert(
    !o25.isMemoryAccess() && !o25.isMemoryStore() && o25.isMemoryLoad() &&
    !o25.isMove() && !o25.isCall() && !o25.isIndirectCall() &&
    !o25.isReturn() && !o25.isJump() && !o25.isConditionalJump() &&
    !o25.isUnconditionalJump() && !o25.isIndirectJump() );

  WIR_Operation o26(
    ARMv4T::OpCode::LDMIA, ARMv4T::OperationFormat::TRR5_1,
    WIR_RegisterParameter( r1, WIR_Usage::defuse ),
    WIR_RegisterParameter( r2, WIR_Usage::def ),
    WIR_RegisterParameter( r3, WIR_Usage::def ),
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r6, WIR_Usage::def ),
    WIR_RegisterParameter( r7, WIR_Usage::def ) );

  ufAssert( o26.getSize() == 2 );
  ufAssert(
    !o26.isMemoryAccess() && !o26.isMemoryStore() && o26.isMemoryLoad() &&
    !o26.isMove() && !o26.isCall() && !o26.isIndirectCall() &&
    !o26.isReturn() && !o26.isJump() && !o26.isConditionalJump() &&
    !o26.isUnconditionalJump() && !o26.isIndirectJump() );

  WIR_Operation o27(
    ARMv4T::OpCode::LDMIA, ARMv4T::OperationFormat::TRR6_1,
    WIR_RegisterParameter( r1, WIR_Usage::defuse ),
    WIR_RegisterParameter( r2, WIR_Usage::def ),
    WIR_RegisterParameter( r3, WIR_Usage::def ),
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r6, WIR_Usage::def ),
    WIR_RegisterParameter( r7, WIR_Usage::def ),
    WIR_RegisterParameter( r8, WIR_Usage::def ) );

  ufAssert( o27.getSize() == 2 );
  ufAssert(
    !o27.isMemoryAccess() && !o27.isMemoryStore() && o27.isMemoryLoad() &&
    !o27.isMove() && !o27.isCall() && !o27.isIndirectCall() &&
    !o27.isReturn() && !o27.isJump() && !o27.isConditionalJump() &&
    !o27.isUnconditionalJump() && !o27.isIndirectJump() );

  WIR_Operation o28(
    ARMv4T::OpCode::LDMIA, ARMv4T::OperationFormat::TRR7_1,
    WIR_RegisterParameter( r1, WIR_Usage::defuse ),
    WIR_RegisterParameter( r2, WIR_Usage::def ),
    WIR_RegisterParameter( r3, WIR_Usage::def ),
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r6, WIR_Usage::def ),
    WIR_RegisterParameter( r7, WIR_Usage::def ),
    WIR_RegisterParameter( r8, WIR_Usage::def ),
    WIR_RegisterParameter( r9, WIR_Usage::def ) );

  ufAssert( o28.getSize() == 2 );
  ufAssert(
    !o28.isMemoryAccess() && !o28.isMemoryStore() && o28.isMemoryLoad() &&
    !o28.isMove() && !o28.isCall() && !o28.isIndirectCall() &&
    !o28.isReturn() && !o28.isJump() && !o28.isConditionalJump() &&
    !o28.isUnconditionalJump() && !o28.isIndirectJump() );

  WIR_Operation o29(
    ARMv4T::OpCode::LDMIA, ARMv4T::OperationFormat::TRR8_1,
    WIR_RegisterParameter( r1, WIR_Usage::defuse ),
    WIR_RegisterParameter( r2, WIR_Usage::def ),
    WIR_RegisterParameter( r3, WIR_Usage::def ),
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r6, WIR_Usage::def ),
    WIR_RegisterParameter( r7, WIR_Usage::def ),
    WIR_RegisterParameter( r8, WIR_Usage::def ),
    WIR_RegisterParameter( r9, WIR_Usage::def ),
    WIR_RegisterParameter( r10, WIR_Usage::def ) );

  ufAssert( o29.getSize() == 2 );
  ufAssert(
    !o29.isMemoryAccess() && !o29.isMemoryStore() && o29.isMemoryLoad() &&
    !o29.isMove() && !o29.isCall() && !o29.isIndirectCall() &&
    !o29.isReturn() && !o29.isJump() && !o29.isConditionalJump() &&
    !o29.isUnconditionalJump() && !o29.isIndirectJump() );

  WIR_Operation o30(
    ARMv4T::OpCode::LDR, ARMv4T::OperationFormat::TRRC7_1,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    ARM_Const7_Unsigned( 16 ) );

  ufAssert( o30.getSize() == 2 );
  ufAssert(
    !o30.isMemoryAccess() && !o30.isMemoryStore() && o30.isMemoryLoad() &&
    !o30.isMove() && !o30.isCall() && !o30.isIndirectCall() &&
    !o30.isReturn() && !o30.isJump() && !o30.isConditionalJump() &&
    !o30.isUnconditionalJump() && !o30.isIndirectJump() );

  WIR_Operation o31(
    ARM_Base::OpCode::LDR, ARMv4T::OperationFormat::TRRR_2,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( r3, WIR_Usage::use ) );

  ufAssert( o31.getSize() == 2 );
  ufAssert(
    !o31.isMemoryAccess() && !o31.isMemoryStore() && o31.isMemoryLoad() &&
    !o31.isMove() && !o31.isCall() && !o31.isIndirectCall() &&
    !o31.isReturn() && !o31.isJump() && !o31.isConditionalJump() &&
    !o31.isUnconditionalJump() && !o31.isIndirectJump() );

  WIR_Operation o32(
    ARM_Base::OpCode::LDR, ARMv4T::OperationFormat::TRPCC10_2,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( p.PC(), WIR_Usage::use ),
    ARM_Const10_Unsigned4( 1020 ) );

  ufAssert( o32.getSize() == 2 );
  ufAssert(
    !o32.isMemoryAccess() && !o32.isMemoryStore() && o32.isMemoryLoad() &&
    !o32.isMove() && !o32.isCall() && !o32.isIndirectCall() &&
    !o32.isReturn() && !o32.isJump() && !o32.isConditionalJump() &&
    !o32.isUnconditionalJump() && !o32.isIndirectJump() );

  WIR_Operation o33(
    ARM_Base::OpCode::LDR, ARMv4T::OperationFormat::TRSPC10_2,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( p.SP(), WIR_Usage::use ),
    ARM_Const10_Unsigned4( 1020 ) );

  ufAssert( o33.getSize() == 2 );
  ufAssert(
    !o33.isMemoryAccess() && !o33.isMemoryStore() && o33.isMemoryLoad() &&
    !o33.isMove() && !o33.isCall() && !o33.isIndirectCall() &&
    !o33.isReturn() && !o33.isJump() && !o33.isConditionalJump() &&
    !o33.isUnconditionalJump() && !o33.isIndirectJump() );

  WIR_Operation o34(
    ARMv4T::OpCode::LDRB, ARMv4T::OperationFormat::TRRC5_2,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    ARM_Const5_Unsigned( 31 ) );

  ufAssert( o34.getSize() == 2 );
  ufAssert(
    !o34.isMemoryAccess() && !o34.isMemoryStore() && o34.isMemoryLoad() &&
    !o34.isMove() && !o34.isCall() && !o34.isIndirectCall() &&
    !o34.isReturn() && !o34.isJump() && !o34.isConditionalJump() &&
    !o34.isUnconditionalJump() && !o34.isIndirectJump() );

  WIR_Operation o35(
    ARM_Base::OpCode::LDRB, ARMv4T::OperationFormat::TRRR_2,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( r3, WIR_Usage::use ) );

  ufAssert( o35.getSize() == 2 );
  ufAssert(
    !o35.isMemoryAccess() && !o35.isMemoryStore() && o35.isMemoryLoad() &&
    !o35.isMove() && !o35.isCall() && !o35.isIndirectCall() &&
    !o35.isReturn() && !o35.isJump() && !o35.isConditionalJump() &&
    !o35.isUnconditionalJump() && !o35.isIndirectJump() );

  WIR_Operation o36(
    ARMv4T::OpCode::LDRH, ARMv4T::OperationFormat::TRRC6_2,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    ARM_Const6_Unsigned( 62 ) );

  ufAssert( o36.getSize() == 2 );
  ufAssert(
    !o36.isMemoryAccess() && !o36.isMemoryStore() && o36.isMemoryLoad() &&
    !o36.isMove() && !o36.isCall() && !o36.isIndirectCall() &&
    !o36.isReturn() && !o36.isJump() && !o36.isConditionalJump() &&
    !o36.isUnconditionalJump() && !o36.isIndirectJump() );

  WIR_Operation o37(
    ARM_Base::OpCode::LDRH, ARMv4T::OperationFormat::TRRR_2,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( r3, WIR_Usage::use ) );

  ufAssert( o37.getSize() == 2 );
  ufAssert(
    !o37.isMemoryAccess() && !o37.isMemoryStore() && o37.isMemoryLoad() &&
    !o37.isMove() && !o37.isCall() && !o37.isIndirectCall() &&
    !o37.isReturn() && !o37.isJump() && !o37.isConditionalJump() &&
    !o37.isUnconditionalJump() && !o37.isIndirectJump() );

  WIR_Operation o38(
    ARM_Base::OpCode::LDRSB, ARMv4T::OperationFormat::TRRR_2,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( r3, WIR_Usage::use ) );

  ufAssert( o38.getSize() == 2 );
  ufAssert(
    !o38.isMemoryAccess() && !o38.isMemoryStore() && o38.isMemoryLoad() &&
    !o38.isMove() && !o38.isCall() && !o38.isIndirectCall() &&
    !o38.isReturn() && !o38.isJump() && !o38.isConditionalJump() &&
    !o38.isUnconditionalJump() && !o38.isIndirectJump() );

  WIR_Operation o39(
    ARM_Base::OpCode::LDRSH, ARMv4T::OperationFormat::TRRR_2,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( r3, WIR_Usage::use ) );

  ufAssert( o39.getSize() == 2 );
  ufAssert(
    !o39.isMemoryAccess() && !o39.isMemoryStore() && o39.isMemoryLoad() &&
    !o39.isMove() && !o39.isCall() && !o39.isIndirectCall() &&
    !o39.isReturn() && !o39.isJump() && !o39.isConditionalJump() &&
    !o39.isUnconditionalJump() && !o39.isIndirectJump() );

  WIR_Operation o40(
    ARMv4T::OpCode::LSL, ARMv4T::OperationFormat::TRRC5_1,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    ARM_Const5_Unsigned( 31 ) );

  ufAssert( o40.getSize() == 2 );
  ufAssert(
    !o40.isMemoryAccess() && !o40.isMemoryStore() && !o40.isMemoryLoad() &&
    !o40.isMove() && !o40.isCall() && !o40.isIndirectCall() &&
    !o40.isReturn() && !o40.isJump() && !o40.isConditionalJump() &&
    !o40.isUnconditionalJump() && !o40.isIndirectJump() );

  WIR_Operation o41(
    ARMv4T::OpCode::LSL, ARMv4T::OperationFormat::TRR_5,
    WIR_RegisterParameter( r1, WIR_Usage::defuse ),
    WIR_RegisterParameter( r2, WIR_Usage::use ) );

  ufAssert( o41.getSize() == 2 );
  ufAssert(
    !o41.isMemoryAccess() && !o41.isMemoryStore() && !o41.isMemoryLoad() &&
    !o41.isMove() && !o41.isCall() && !o41.isIndirectCall() &&
    !o41.isReturn() && !o41.isJump() && !o41.isConditionalJump() &&
    !o41.isUnconditionalJump() && !o41.isIndirectJump() );

  WIR_Operation o42(
    ARMv4T::OpCode::LSR, ARMv4T::OperationFormat::TRRC6_1,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    ARM_Const6_Unsigned0( 17 ) );

  ufAssert( o42.getSize() == 2 );
  ufAssert(
    !o42.isMemoryAccess() && !o42.isMemoryStore() && !o42.isMemoryLoad() &&
    !o42.isMove() && !o42.isCall() && !o42.isIndirectCall() &&
    !o42.isReturn() && !o42.isJump() && !o42.isConditionalJump() &&
    !o42.isUnconditionalJump() && !o42.isIndirectJump() );

  WIR_Operation o43(
    ARMv4T::OpCode::LSR, ARMv4T::OperationFormat::TRR_5,
    WIR_RegisterParameter( r1, WIR_Usage::defuse ),
    WIR_RegisterParameter( r2, WIR_Usage::use ) );

  ufAssert( o43.getSize() == 2 );
  ufAssert(
    !o43.isMemoryAccess() && !o43.isMemoryStore() && !o43.isMemoryLoad() &&
    !o43.isMove() && !o43.isCall() && !o43.isIndirectCall() &&
    !o43.isReturn() && !o43.isJump() && !o43.isConditionalJump() &&
    !o43.isUnconditionalJump() && !o43.isIndirectJump() );

  WIR_Operation o44(
    ARMv4T::OpCode::MOV, ARMv4T::OperationFormat::TRC8_1,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    ARM_Const8_Unsigned( 255 ) );

  ufAssert( o44.getSize() == 2 );
  ufAssert(
    !o44.isMemoryAccess() && !o44.isMemoryStore() && !o44.isMemoryLoad() &&
    !o44.isMove() && !o44.isCall() && !o44.isIndirectCall() &&
    !o44.isReturn() && !o44.isJump() && !o44.isConditionalJump() &&
    !o44.isUnconditionalJump() && !o44.isIndirectJump() );

  WIR_Operation o45(
    ARMv4T::OpCode::MOV, ARMv4T::OperationFormat::TRR_1,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::use ) );

  ufAssert( o45.getSize() == 2 );
  ufAssert(
    !o45.isMemoryAccess() && !o45.isMemoryStore() && !o45.isMemoryLoad() &&
    o45.isMove() && !o45.isCall() && !o45.isIndirectCall() &&
    !o45.isReturn() && !o45.isJump() && !o45.isConditionalJump() &&
    !o45.isUnconditionalJump() && !o45.isIndirectJump() );

  WIR_Operation o46(
    ARMv4T::OpCode::MOV, ARMv4T::OperationFormat::TRR_2,
    WIR_RegisterParameter( r4, WIR_Usage::def ),
    WIR_RegisterParameter( r5, WIR_Usage::use ) );

  ufAssert( o46.getSize() == 2 );
  ufAssert(
    !o46.isMemoryAccess() && !o46.isMemoryStore() && !o46.isMemoryLoad() &&
    o46.isMove() && !o46.isCall() && !o46.isIndirectCall() &&
    !o46.isReturn() && !o46.isJump() && !o46.isConditionalJump() &&
    !o46.isUnconditionalJump() && !o46.isIndirectJump() );

  WIR_Operation o47(
    ARMv4T::OpCode::MUL, ARMv4T::OperationFormat::TRR_5,
    WIR_RegisterParameter( r1, WIR_Usage::defuse ),
    WIR_RegisterParameter( r2, WIR_Usage::use ) );

  ufAssert( o47.getSize() == 2 );
  ufAssert(
    !o47.isMemoryAccess() && !o47.isMemoryStore() && !o47.isMemoryLoad() &&
    !o47.isMove() && !o47.isCall() && !o47.isIndirectCall() &&
    !o47.isReturn() && !o47.isJump() && !o47.isConditionalJump() &&
    !o47.isUnconditionalJump() && !o47.isIndirectJump() );

  WIR_Operation o48(
    ARMv4T::OpCode::MVN, ARMv4T::OperationFormat::TRR_1,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::use ) );

  ufAssert( o48.getSize() == 2 );
  ufAssert(
    !o48.isMemoryAccess() && !o48.isMemoryStore() && !o48.isMemoryLoad() &&
    !o48.isMove() && !o48.isCall() && !o48.isIndirectCall() &&
    !o48.isReturn() && !o48.isJump() && !o48.isConditionalJump() &&
    !o48.isUnconditionalJump() && !o48.isIndirectJump() );

  WIR_Operation o49(
    ARMv4T::OpCode::NEG, ARMv4T::OperationFormat::TRR_1,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::use ) );

  ufAssert( o49.getSize() == 2 );
  ufAssert(
    !o49.isMemoryAccess() && !o49.isMemoryStore() && !o49.isMemoryLoad() &&
    !o49.isMove() && !o49.isCall() && !o49.isIndirectCall() &&
    !o49.isReturn() && !o49.isJump() && !o49.isConditionalJump() &&
    !o49.isUnconditionalJump() && !o49.isIndirectJump() );

  WIR_Operation o50(
    ARMv4T::OpCode::ORR, ARMv4T::OperationFormat::TRR_5,
    WIR_RegisterParameter( r1, WIR_Usage::defuse ),
    WIR_RegisterParameter( r2, WIR_Usage::use ) );

  ufAssert( o50.getSize() == 2 );
  ufAssert(
    !o50.isMemoryAccess() && !o50.isMemoryStore() && !o50.isMemoryLoad() &&
    !o50.isMove() && !o50.isCall() && !o50.isIndirectCall() &&
    !o50.isReturn() && !o50.isJump() && !o50.isConditionalJump() &&
    !o50.isUnconditionalJump() && !o50.isIndirectJump() );

  WIR_Operation o51(
    ARMv4T::OpCode::POP, ARMv4T::OperationFormat::TR1_1,
    WIR_RegisterParameter( r1, WIR_Usage::def ) );

  ufAssert( o51.getSize() == 2 );
  ufAssert(
    !o51.isMemoryAccess() && !o51.isMemoryStore() && o51.isMemoryLoad() &&
    !o51.isMove() && !o51.isCall() && !o51.isIndirectCall() &&
    !o51.isReturn() && !o51.isJump() && !o51.isConditionalJump() &&
    !o51.isUnconditionalJump() && !o51.isIndirectJump() );

  WIR_Operation o52(
    ARMv4T::OpCode::POP, ARMv4T::OperationFormat::TR1PC,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( p.PC(), WIR_Usage::def ) );

  ufAssert( o52.getSize() == 2 );
  ufAssert(
    !o52.isMemoryAccess() && !o52.isMemoryStore() && o52.isMemoryLoad() &&
    !o52.isMove() && !o52.isCall() && !o52.isIndirectCall() &&
    !o52.isReturn() && !o52.isJump() && !o52.isConditionalJump() &&
    !o52.isUnconditionalJump() && !o52.isIndirectJump() );

  WIR_Operation o53(
    ARMv4T::OpCode::POP, ARMv4T::OperationFormat::TR2_1,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::def ) );

  ufAssert( o53.getSize() == 2 );
  ufAssert(
    !o53.isMemoryAccess() && !o53.isMemoryStore() && o53.isMemoryLoad() &&
    !o53.isMove() && !o53.isCall() && !o53.isIndirectCall() &&
    !o53.isReturn() && !o53.isJump() && !o53.isConditionalJump() &&
    !o53.isUnconditionalJump() && !o53.isIndirectJump() );

  WIR_Operation o54(
    ARMv4T::OpCode::POP, ARMv4T::OperationFormat::TR2PC,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::def ),
    WIR_RegisterParameter( p.PC(), WIR_Usage::def ) );

  ufAssert( o54.getSize() == 2 );
  ufAssert(
    !o54.isMemoryAccess() && !o54.isMemoryStore() && o54.isMemoryLoad() &&
    !o54.isMove() && !o54.isCall() && !o54.isIndirectCall() &&
    !o54.isReturn() && !o54.isJump() && !o54.isConditionalJump() &&
    !o54.isUnconditionalJump() && !o54.isIndirectJump() );

  WIR_Operation o55(
    ARMv4T::OpCode::POP, ARMv4T::OperationFormat::TR3_1,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::def ),
    WIR_RegisterParameter( r3, WIR_Usage::def ) );

  ufAssert( o55.getSize() == 2 );
  ufAssert(
    !o55.isMemoryAccess() && !o55.isMemoryStore() && o55.isMemoryLoad() &&
    !o55.isMove() && !o55.isCall() && !o55.isIndirectCall() &&
    !o55.isReturn() && !o55.isJump() && !o55.isConditionalJump() &&
    !o55.isUnconditionalJump() && !o55.isIndirectJump() );

  WIR_Operation o56(
    ARMv4T::OpCode::POP, ARMv4T::OperationFormat::TR3PC,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::def ),
    WIR_RegisterParameter( r3, WIR_Usage::def ),
    WIR_RegisterParameter( p.PC(), WIR_Usage::def ) );

  ufAssert( o56.getSize() == 2 );
  ufAssert(
    !o56.isMemoryAccess() && !o56.isMemoryStore() && o56.isMemoryLoad() &&
    !o56.isMove() && !o56.isCall() && !o56.isIndirectCall() &&
    !o56.isReturn() && !o56.isJump() && !o56.isConditionalJump() &&
    !o56.isUnconditionalJump() && !o56.isIndirectJump() );

  WIR_Operation o57(
    ARMv4T::OpCode::POP, ARMv4T::OperationFormat::TR4_1,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::def ),
    WIR_RegisterParameter( r3, WIR_Usage::def ),
    WIR_RegisterParameter( r6, WIR_Usage::def ) );

  ufAssert( o57.getSize() == 2 );
  ufAssert(
    !o57.isMemoryAccess() && !o57.isMemoryStore() && o57.isMemoryLoad() &&
    !o57.isMove() && !o57.isCall() && !o57.isIndirectCall() &&
    !o57.isReturn() && !o57.isJump() && !o57.isConditionalJump() &&
    !o57.isUnconditionalJump() && !o57.isIndirectJump() );

  WIR_Operation o58(
    ARMv4T::OpCode::POP, ARMv4T::OperationFormat::TR4PC,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::def ),
    WIR_RegisterParameter( r3, WIR_Usage::def ),
    WIR_RegisterParameter( r6, WIR_Usage::def ),
    WIR_RegisterParameter( p.PC(), WIR_Usage::def ) );

  ufAssert( o58.getSize() == 2 );
  ufAssert(
    !o58.isMemoryAccess() && !o58.isMemoryStore() && o58.isMemoryLoad() &&
    !o58.isMove() && !o58.isCall() && !o58.isIndirectCall() &&
    !o58.isReturn() && !o58.isJump() && !o58.isConditionalJump() &&
    !o58.isUnconditionalJump() && !o58.isIndirectJump() );

  WIR_Operation o59(
    ARMv4T::OpCode::POP, ARMv4T::OperationFormat::TR5_1,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::def ),
    WIR_RegisterParameter( r3, WIR_Usage::def ),
    WIR_RegisterParameter( r6, WIR_Usage::def ),
    WIR_RegisterParameter( r7, WIR_Usage::def ) );

  ufAssert( o59.getSize() == 2 );
  ufAssert(
    !o59.isMemoryAccess() && !o59.isMemoryStore() && o59.isMemoryLoad() &&
    !o59.isMove() && !o59.isCall() && !o59.isIndirectCall() &&
    !o59.isReturn() && !o59.isJump() && !o59.isConditionalJump() &&
    !o59.isUnconditionalJump() && !o59.isIndirectJump() );

  WIR_Operation o60(
    ARMv4T::OpCode::POP, ARMv4T::OperationFormat::TR5PC,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::def ),
    WIR_RegisterParameter( r3, WIR_Usage::def ),
    WIR_RegisterParameter( r6, WIR_Usage::def ),
    WIR_RegisterParameter( r7, WIR_Usage::def ),
    WIR_RegisterParameter( p.PC(), WIR_Usage::def ) );

  ufAssert( o60.getSize() == 2 );
  ufAssert(
    !o60.isMemoryAccess() && !o60.isMemoryStore() && o60.isMemoryLoad() &&
    !o60.isMove() && !o60.isCall() && !o60.isIndirectCall() &&
    !o60.isReturn() && !o60.isJump() && !o60.isConditionalJump() &&
    !o60.isUnconditionalJump() && !o60.isIndirectJump() );

  WIR_Operation o61(
    ARMv4T::OpCode::POP, ARMv4T::OperationFormat::TR6_1,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::def ),
    WIR_RegisterParameter( r3, WIR_Usage::def ),
    WIR_RegisterParameter( r6, WIR_Usage::def ),
    WIR_RegisterParameter( r7, WIR_Usage::def ),
    WIR_RegisterParameter( r8, WIR_Usage::def ) );

  ufAssert( o61.getSize() == 2 );
  ufAssert(
    !o61.isMemoryAccess() && !o61.isMemoryStore() && o61.isMemoryLoad() &&
    !o61.isMove() && !o61.isCall() && !o61.isIndirectCall() &&
    !o61.isReturn() && !o61.isJump() && !o61.isConditionalJump() &&
    !o61.isUnconditionalJump() && !o61.isIndirectJump() );

  WIR_Operation o62(
    ARMv4T::OpCode::POP, ARMv4T::OperationFormat::TR6PC,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::def ),
    WIR_RegisterParameter( r3, WIR_Usage::def ),
    WIR_RegisterParameter( r6, WIR_Usage::def ),
    WIR_RegisterParameter( r7, WIR_Usage::def ),
    WIR_RegisterParameter( r8, WIR_Usage::def ),
    WIR_RegisterParameter( p.PC(), WIR_Usage::def ) );

  ufAssert( o62.getSize() == 2 );
  ufAssert(
    !o62.isMemoryAccess() && !o62.isMemoryStore() && o62.isMemoryLoad() &&
    !o62.isMove() && !o62.isCall() && !o62.isIndirectCall() &&
    !o62.isReturn() && !o62.isJump() && !o62.isConditionalJump() &&
    !o62.isUnconditionalJump() && !o62.isIndirectJump() );

  WIR_Operation o63(
    ARMv4T::OpCode::POP, ARMv4T::OperationFormat::TR7_1,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::def ),
    WIR_RegisterParameter( r3, WIR_Usage::def ),
    WIR_RegisterParameter( r6, WIR_Usage::def ),
    WIR_RegisterParameter( r7, WIR_Usage::def ),
    WIR_RegisterParameter( r8, WIR_Usage::def ),
    WIR_RegisterParameter( r9, WIR_Usage::def ) );

  ufAssert( o63.getSize() == 2 );
  ufAssert(
    !o63.isMemoryAccess() && !o63.isMemoryStore() && o63.isMemoryLoad() &&
    !o63.isMove() && !o63.isCall() && !o63.isIndirectCall() &&
    !o63.isReturn() && !o63.isJump() && !o63.isConditionalJump() &&
    !o63.isUnconditionalJump() && !o63.isIndirectJump() );

  WIR_Operation o64(
    ARMv4T::OpCode::POP, ARMv4T::OperationFormat::TR7PC,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::def ),
    WIR_RegisterParameter( r3, WIR_Usage::def ),
    WIR_RegisterParameter( r6, WIR_Usage::def ),
    WIR_RegisterParameter( r7, WIR_Usage::def ),
    WIR_RegisterParameter( r8, WIR_Usage::def ),
    WIR_RegisterParameter( r9, WIR_Usage::def ),
    WIR_RegisterParameter( p.PC(), WIR_Usage::def ) );

  ufAssert( o64.getSize() == 2 );
  ufAssert(
    !o64.isMemoryAccess() && !o64.isMemoryStore() && o64.isMemoryLoad() &&
    !o64.isMove() && !o64.isCall() && !o64.isIndirectCall() &&
    !o64.isReturn() && !o64.isJump() && !o64.isConditionalJump() &&
    !o64.isUnconditionalJump() && !o64.isIndirectJump() );

  WIR_Operation o65(
    ARMv4T::OpCode::POP, ARMv4T::OperationFormat::TR8_1,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::def ),
    WIR_RegisterParameter( r3, WIR_Usage::def ),
    WIR_RegisterParameter( r6, WIR_Usage::def ),
    WIR_RegisterParameter( r7, WIR_Usage::def ),
    WIR_RegisterParameter( r8, WIR_Usage::def ),
    WIR_RegisterParameter( r9, WIR_Usage::def ),
    WIR_RegisterParameter( r10, WIR_Usage::def ) );

  ufAssert( o65.getSize() == 2 );
  ufAssert(
    !o65.isMemoryAccess() && !o65.isMemoryStore() && o65.isMemoryLoad() &&
    !o65.isMove() && !o65.isCall() && !o65.isIndirectCall() &&
    !o65.isReturn() && !o65.isJump() && !o65.isConditionalJump() &&
    !o65.isUnconditionalJump() && !o65.isIndirectJump() );

  WIR_Operation o66(
    ARMv4T::OpCode::POP, ARMv4T::OperationFormat::TR8PC,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::def ),
    WIR_RegisterParameter( r3, WIR_Usage::def ),
    WIR_RegisterParameter( r6, WIR_Usage::def ),
    WIR_RegisterParameter( r7, WIR_Usage::def ),
    WIR_RegisterParameter( r8, WIR_Usage::def ),
    WIR_RegisterParameter( r9, WIR_Usage::def ),
    WIR_RegisterParameter( r10, WIR_Usage::def ),
    WIR_RegisterParameter( p.PC(), WIR_Usage::def ) );

  ufAssert( o66.getSize() == 2 );
  ufAssert(
    !o66.isMemoryAccess() && !o66.isMemoryStore() && o66.isMemoryLoad() &&
    !o66.isMove() && !o66.isCall() && !o66.isIndirectCall() &&
    !o66.isReturn() && !o66.isJump() && !o66.isConditionalJump() &&
    !o66.isUnconditionalJump() && !o66.isIndirectJump() );

  WIR_Operation o67(
    ARMv4T::OpCode::PUSH, ARMv4T::OperationFormat::TR1_2,
    WIR_RegisterParameter( r1, WIR_Usage::use ) );

  ufAssert( o67.getSize() == 2 );
  ufAssert(
    !o67.isMemoryAccess() && o67.isMemoryStore() && !o67.isMemoryLoad() &&
    !o67.isMove() && !o67.isCall() && !o67.isIndirectCall() &&
    !o67.isReturn() && !o67.isJump() && !o67.isConditionalJump() &&
    !o67.isUnconditionalJump() && !o67.isIndirectJump() );

  WIR_Operation o68(
    ARMv4T::OpCode::PUSH, ARMv4T::OperationFormat::TR1LR,
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    WIR_RegisterParameter( p.LR(), WIR_Usage::use ) );

  ufAssert( o68.getSize() == 2 );
  ufAssert(
    !o68.isMemoryAccess() && o68.isMemoryStore() && !o68.isMemoryLoad() &&
    !o68.isMove() && !o68.isCall() && !o68.isIndirectCall() &&
    !o68.isReturn() && !o68.isJump() && !o68.isConditionalJump() &&
    !o68.isUnconditionalJump() && !o68.isIndirectJump() );

  WIR_Operation o69(
    ARMv4T::OpCode::PUSH, ARMv4T::OperationFormat::TR2_2,
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    WIR_RegisterParameter( r2, WIR_Usage::use ) );

  ufAssert( o69.getSize() == 2 );
  ufAssert(
    !o69.isMemoryAccess() && o69.isMemoryStore() && !o69.isMemoryLoad() &&
    !o69.isMove() && !o69.isCall() && !o69.isIndirectCall() &&
    !o69.isReturn() && !o69.isJump() && !o69.isConditionalJump() &&
    !o69.isUnconditionalJump() && !o69.isIndirectJump() );

  WIR_Operation o70(
    ARMv4T::OpCode::PUSH, ARMv4T::OperationFormat::TR2LR,
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( p.LR(), WIR_Usage::use ) );

  ufAssert( o70.getSize() == 2 );
  ufAssert(
    !o70.isMemoryAccess() && o70.isMemoryStore() && !o70.isMemoryLoad() &&
    !o70.isMove() && !o70.isCall() && !o70.isIndirectCall() &&
    !o70.isReturn() && !o70.isJump() && !o70.isConditionalJump() &&
    !o70.isUnconditionalJump() && !o70.isIndirectJump() );

  WIR_Operation o71(
    ARMv4T::OpCode::PUSH, ARMv4T::OperationFormat::TR3_2,
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( r3, WIR_Usage::use ) );

  ufAssert( o71.getSize() == 2 );
  ufAssert(
    !o71.isMemoryAccess() && o71.isMemoryStore() && !o71.isMemoryLoad() &&
    !o71.isMove() && !o71.isCall() && !o71.isIndirectCall() &&
    !o71.isReturn() && !o71.isJump() && !o71.isConditionalJump() &&
    !o71.isUnconditionalJump() && !o71.isIndirectJump() );

  WIR_Operation o72(
    ARMv4T::OpCode::PUSH, ARMv4T::OperationFormat::TR3LR,
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( r3, WIR_Usage::use ),
    WIR_RegisterParameter( p.LR(), WIR_Usage::use ) );

  ufAssert( o72.getSize() == 2 );
  ufAssert(
    !o72.isMemoryAccess() && o72.isMemoryStore() && !o72.isMemoryLoad() &&
    !o72.isMove() && !o72.isCall() && !o72.isIndirectCall() &&
    !o72.isReturn() && !o72.isJump() && !o72.isConditionalJump() &&
    !o72.isUnconditionalJump() && !o72.isIndirectJump() );

  WIR_Operation o73(
    ARMv4T::OpCode::PUSH, ARMv4T::OperationFormat::TR4_2,
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( r3, WIR_Usage::use ),
    WIR_RegisterParameter( r6, WIR_Usage::use ) );

  ufAssert( o73.getSize() == 2 );
  ufAssert(
    !o73.isMemoryAccess() && o73.isMemoryStore() && !o73.isMemoryLoad() &&
    !o73.isMove() && !o73.isCall() && !o73.isIndirectCall() &&
    !o73.isReturn() && !o73.isJump() && !o73.isConditionalJump() &&
    !o73.isUnconditionalJump() && !o73.isIndirectJump() );

  WIR_Operation o74(
    ARMv4T::OpCode::PUSH, ARMv4T::OperationFormat::TR4LR,
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( r3, WIR_Usage::use ),
    WIR_RegisterParameter( r6, WIR_Usage::use ),
    WIR_RegisterParameter( p.LR(), WIR_Usage::use ) );

  ufAssert( o74.getSize() == 2 );
  ufAssert(
    !o74.isMemoryAccess() && o74.isMemoryStore() && !o74.isMemoryLoad() &&
    !o74.isMove() && !o74.isCall() && !o74.isIndirectCall() &&
    !o74.isReturn() && !o74.isJump() && !o74.isConditionalJump() &&
    !o74.isUnconditionalJump() && !o74.isIndirectJump() );

  WIR_Operation o75(
    ARMv4T::OpCode::PUSH, ARMv4T::OperationFormat::TR5_2,
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( r3, WIR_Usage::use ),
    WIR_RegisterParameter( r6, WIR_Usage::use ),
    WIR_RegisterParameter( r7, WIR_Usage::use ) );

  ufAssert( o75.getSize() == 2 );
  ufAssert(
    !o75.isMemoryAccess() && o75.isMemoryStore() && !o75.isMemoryLoad() &&
    !o75.isMove() && !o75.isCall() && !o75.isIndirectCall() &&
    !o75.isReturn() && !o75.isJump() && !o75.isConditionalJump() &&
    !o75.isUnconditionalJump() && !o75.isIndirectJump() );

  WIR_Operation o76(
    ARMv4T::OpCode::PUSH, ARMv4T::OperationFormat::TR5LR,
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( r3, WIR_Usage::use ),
    WIR_RegisterParameter( r6, WIR_Usage::use ),
    WIR_RegisterParameter( r7, WIR_Usage::use ),
    WIR_RegisterParameter( p.LR(), WIR_Usage::use ) );

  ufAssert( o76.getSize() == 2 );
  ufAssert(
    !o76.isMemoryAccess() && o76.isMemoryStore() && !o76.isMemoryLoad() &&
    !o76.isMove() && !o76.isCall() && !o76.isIndirectCall() &&
    !o76.isReturn() && !o76.isJump() && !o76.isConditionalJump() &&
    !o76.isUnconditionalJump() && !o76.isIndirectJump() );

  WIR_Operation o77(
    ARMv4T::OpCode::PUSH, ARMv4T::OperationFormat::TR6_2,
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( r3, WIR_Usage::use ),
    WIR_RegisterParameter( r6, WIR_Usage::use ),
    WIR_RegisterParameter( r7, WIR_Usage::use ),
    WIR_RegisterParameter( r8, WIR_Usage::use ) );

  ufAssert( o77.getSize() == 2 );
  ufAssert(
    !o77.isMemoryAccess() && o77.isMemoryStore() && !o77.isMemoryLoad() &&
    !o77.isMove() && !o77.isCall() && !o77.isIndirectCall() &&
    !o77.isReturn() && !o77.isJump() && !o77.isConditionalJump() &&
    !o77.isUnconditionalJump() && !o77.isIndirectJump() );

  WIR_Operation o78(
    ARMv4T::OpCode::PUSH, ARMv4T::OperationFormat::TR6LR,
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( r3, WIR_Usage::use ),
    WIR_RegisterParameter( r6, WIR_Usage::use ),
    WIR_RegisterParameter( r7, WIR_Usage::use ),
    WIR_RegisterParameter( r8, WIR_Usage::use ),
    WIR_RegisterParameter( p.LR(), WIR_Usage::use ) );

  ufAssert( o78.getSize() == 2 );
  ufAssert(
    !o78.isMemoryAccess() && o78.isMemoryStore() && !o78.isMemoryLoad() &&
    !o78.isMove() && !o78.isCall() && !o78.isIndirectCall() &&
    !o78.isReturn() && !o78.isJump() && !o78.isConditionalJump() &&
    !o78.isUnconditionalJump() && !o78.isIndirectJump() );

  WIR_Operation o79(
    ARMv4T::OpCode::PUSH, ARMv4T::OperationFormat::TR7_2,
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( r3, WIR_Usage::use ),
    WIR_RegisterParameter( r6, WIR_Usage::use ),
    WIR_RegisterParameter( r7, WIR_Usage::use ),
    WIR_RegisterParameter( r8, WIR_Usage::use ),
    WIR_RegisterParameter( r9, WIR_Usage::use ) );

  ufAssert( o79.getSize() == 2 );
  ufAssert(
    !o79.isMemoryAccess() && o79.isMemoryStore() && !o79.isMemoryLoad() &&
    !o79.isMove() && !o79.isCall() && !o79.isIndirectCall() &&
    !o79.isReturn() && !o79.isJump() && !o79.isConditionalJump() &&
    !o79.isUnconditionalJump() && !o79.isIndirectJump() );

  WIR_Operation o80(
    ARMv4T::OpCode::PUSH, ARMv4T::OperationFormat::TR7LR,
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( r3, WIR_Usage::use ),
    WIR_RegisterParameter( r6, WIR_Usage::use ),
    WIR_RegisterParameter( r7, WIR_Usage::use ),
    WIR_RegisterParameter( r8, WIR_Usage::use ),
    WIR_RegisterParameter( r9, WIR_Usage::use ),
    WIR_RegisterParameter( p.LR(), WIR_Usage::use ) );

  ufAssert( o80.getSize() == 2 );
  ufAssert(
    !o80.isMemoryAccess() && o80.isMemoryStore() && !o80.isMemoryLoad() &&
    !o80.isMove() && !o80.isCall() && !o80.isIndirectCall() &&
    !o80.isReturn() && !o80.isJump() && !o80.isConditionalJump() &&
    !o80.isUnconditionalJump() && !o80.isIndirectJump() );

  WIR_Operation o81(
    ARMv4T::OpCode::PUSH, ARMv4T::OperationFormat::TR8_2,
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( r3, WIR_Usage::use ),
    WIR_RegisterParameter( r6, WIR_Usage::use ),
    WIR_RegisterParameter( r7, WIR_Usage::use ),
    WIR_RegisterParameter( r8, WIR_Usage::use ),
    WIR_RegisterParameter( r9, WIR_Usage::use ),
    WIR_RegisterParameter( r10, WIR_Usage::use ) );

  ufAssert( o81.getSize() == 2 );
  ufAssert(
    !o81.isMemoryAccess() && o81.isMemoryStore() && !o81.isMemoryLoad() &&
    !o81.isMove() && !o81.isCall() && !o81.isIndirectCall() &&
    !o81.isReturn() && !o81.isJump() && !o81.isConditionalJump() &&
    !o81.isUnconditionalJump() && !o81.isIndirectJump() );

  WIR_Operation o82(
    ARMv4T::OpCode::PUSH, ARMv4T::OperationFormat::TR8LR,
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( r3, WIR_Usage::use ),
    WIR_RegisterParameter( r6, WIR_Usage::use ),
    WIR_RegisterParameter( r7, WIR_Usage::use ),
    WIR_RegisterParameter( r8, WIR_Usage::use ),
    WIR_RegisterParameter( r9, WIR_Usage::use ),
    WIR_RegisterParameter( r10, WIR_Usage::use ),
    WIR_RegisterParameter( p.LR(), WIR_Usage::use ) );

  ufAssert( o82.getSize() == 2 );
  ufAssert(
    !o82.isMemoryAccess() && o82.isMemoryStore() && !o82.isMemoryLoad() &&
    !o82.isMove() && !o82.isCall() && !o82.isIndirectCall() &&
    !o82.isReturn() && !o82.isJump() && !o82.isConditionalJump() &&
    !o82.isUnconditionalJump() && !o82.isIndirectJump() );

  WIR_Operation o83(
    ARMv4T::OpCode::ROR, ARMv4T::OperationFormat::TRR_5,
    WIR_RegisterParameter( r1, WIR_Usage::defuse ),
    WIR_RegisterParameter( r2, WIR_Usage::use ) );

  ufAssert( o83.getSize() == 2 );
  ufAssert(
    !o83.isMemoryAccess() && !o83.isMemoryStore() && !o83.isMemoryLoad() &&
    !o83.isMove() && !o83.isCall() && !o83.isIndirectCall() &&
    !o83.isReturn() && !o83.isJump() && !o83.isConditionalJump() &&
    !o83.isUnconditionalJump() && !o83.isIndirectJump() );

  WIR_Operation o84(
    ARMv4T::OpCode::SBC, ARMv4T::OperationFormat::TRR_5,
    WIR_RegisterParameter( r1, WIR_Usage::defuse ),
    WIR_RegisterParameter( r2, WIR_Usage::use ) );

  ufAssert( o84.getSize() == 2 );
  ufAssert(
    !o84.isMemoryAccess() && !o84.isMemoryStore() && !o84.isMemoryLoad() &&
    !o84.isMove() && !o84.isCall() && !o84.isIndirectCall() &&
    !o84.isReturn() && !o84.isJump() && !o84.isConditionalJump() &&
    !o84.isUnconditionalJump() && !o84.isIndirectJump() );

  WIR_Operation o85(
    ARMv4T::OpCode::STMIA, ARMv4T::OperationFormat::TRR1_2,
    WIR_RegisterParameter( r6, WIR_Usage::defuse ),
    WIR_RegisterParameter( r1, WIR_Usage::use ) );

  ufAssert( o85.getSize() == 2 );
  ufAssert(
    !o85.isMemoryAccess() && o85.isMemoryStore() && !o85.isMemoryLoad() &&
    !o85.isMove() && !o85.isCall() && !o85.isIndirectCall() &&
    !o85.isReturn() && !o85.isJump() && !o85.isConditionalJump() &&
    !o85.isUnconditionalJump() && !o85.isIndirectJump() );

  WIR_Operation o86(
    ARMv4T::OpCode::STMIA, ARMv4T::OperationFormat::TRR2_2,
    WIR_RegisterParameter( r6, WIR_Usage::defuse ),
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    WIR_RegisterParameter( r2, WIR_Usage::use ) );

  ufAssert( o86.getSize() == 2 );
  ufAssert(
    !o86.isMemoryAccess() && o86.isMemoryStore() && !o86.isMemoryLoad() &&
    !o86.isMove() && !o86.isCall() && !o86.isIndirectCall() &&
    !o86.isReturn() && !o86.isJump() && !o86.isConditionalJump() &&
    !o86.isUnconditionalJump() && !o86.isIndirectJump() );

  WIR_Operation o87(
    ARMv4T::OpCode::STMIA, ARMv4T::OperationFormat::TRR3_2,
    WIR_RegisterParameter( r6, WIR_Usage::defuse ),
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( r3, WIR_Usage::use ) );

  ufAssert( o87.getSize() == 2 );
  ufAssert(
    !o87.isMemoryAccess() && o87.isMemoryStore() && !o87.isMemoryLoad() &&
    !o87.isMove() && !o87.isCall() && !o87.isIndirectCall() &&
    !o87.isReturn() && !o87.isJump() && !o87.isConditionalJump() &&
    !o87.isUnconditionalJump() && !o87.isIndirectJump() );

  WIR_Operation o88(
    ARMv4T::OpCode::STMIA, ARMv4T::OperationFormat::TRR4_2,
    WIR_RegisterParameter( r6, WIR_Usage::defuse ),
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( r3, WIR_Usage::use ),
    WIR_RegisterParameter( r6, WIR_Usage::use ) );

  ufAssert( o88.getSize() == 2 );
  ufAssert(
    !o88.isMemoryAccess() && o88.isMemoryStore() && !o88.isMemoryLoad() &&
    !o88.isMove() && !o88.isCall() && !o88.isIndirectCall() &&
    !o88.isReturn() && !o88.isJump() && !o88.isConditionalJump() &&
    !o88.isUnconditionalJump() && !o88.isIndirectJump() );

  WIR_Operation o89(
    ARMv4T::OpCode::STMIA, ARMv4T::OperationFormat::TRR5_2,
    WIR_RegisterParameter( r6, WIR_Usage::defuse ),
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( r3, WIR_Usage::use ),
    WIR_RegisterParameter( r6, WIR_Usage::use ),
    WIR_RegisterParameter( r7, WIR_Usage::use ) );

  ufAssert( o89.getSize() == 2 );
  ufAssert(
    !o89.isMemoryAccess() && o89.isMemoryStore() && !o89.isMemoryLoad() &&
    !o89.isMove() && !o89.isCall() && !o89.isIndirectCall() &&
    !o89.isReturn() && !o89.isJump() && !o89.isConditionalJump() &&
    !o89.isUnconditionalJump() && !o89.isIndirectJump() );

  WIR_Operation o90(
    ARMv4T::OpCode::STMIA, ARMv4T::OperationFormat::TRR6_2,
    WIR_RegisterParameter( r6, WIR_Usage::defuse ),
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( r3, WIR_Usage::use ),
    WIR_RegisterParameter( r6, WIR_Usage::use ),
    WIR_RegisterParameter( r7, WIR_Usage::use ),
    WIR_RegisterParameter( r8, WIR_Usage::use ) );

  ufAssert( o90.getSize() == 2 );
  ufAssert(
    !o90.isMemoryAccess() && o90.isMemoryStore() && !o90.isMemoryLoad() &&
    !o90.isMove() && !o90.isCall() && !o90.isIndirectCall() &&
    !o90.isReturn() && !o90.isJump() && !o90.isConditionalJump() &&
    !o90.isUnconditionalJump() && !o90.isIndirectJump() );

  WIR_Operation o91(
    ARMv4T::OpCode::STMIA, ARMv4T::OperationFormat::TRR7_2,
    WIR_RegisterParameter( r6, WIR_Usage::defuse ),
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( r3, WIR_Usage::use ),
    WIR_RegisterParameter( r6, WIR_Usage::use ),
    WIR_RegisterParameter( r7, WIR_Usage::use ),
    WIR_RegisterParameter( r8, WIR_Usage::use ),
    WIR_RegisterParameter( r9, WIR_Usage::use ) );

  ufAssert( o91.getSize() == 2 );
  ufAssert(
    !o91.isMemoryAccess() && o91.isMemoryStore() && !o91.isMemoryLoad() &&
    !o91.isMove() && !o91.isCall() && !o91.isIndirectCall() &&
    !o91.isReturn() && !o91.isJump() && !o91.isConditionalJump() &&
    !o91.isUnconditionalJump() && !o91.isIndirectJump() );

  WIR_Operation o92(
    ARMv4T::OpCode::STMIA, ARMv4T::OperationFormat::TRR8_2,
    WIR_RegisterParameter( r6, WIR_Usage::defuse ),
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( r3, WIR_Usage::use ),
    WIR_RegisterParameter( r6, WIR_Usage::use ),
    WIR_RegisterParameter( r7, WIR_Usage::use ),
    WIR_RegisterParameter( r8, WIR_Usage::use ),
    WIR_RegisterParameter( r9, WIR_Usage::use ),
    WIR_RegisterParameter( r10, WIR_Usage::use ) );

  ufAssert( o92.getSize() == 2 );
  ufAssert(
    !o92.isMemoryAccess() && o92.isMemoryStore() && !o92.isMemoryLoad() &&
    !o92.isMove() && !o92.isCall() && !o92.isIndirectCall() &&
    !o92.isReturn() && !o92.isJump() && !o92.isConditionalJump() &&
    !o92.isUnconditionalJump() && !o92.isIndirectJump() );

  WIR_Operation o93(
    ARMv4T::OpCode::STR, ARMv4T::OperationFormat::TRRC7_2,
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    ARM_Const7_Unsigned( 16 ) );

  ufAssert( o93.getSize() == 2 );
  ufAssert(
    !o93.isMemoryAccess() && o93.isMemoryStore() && !o93.isMemoryLoad() &&
    !o93.isMove() && !o93.isCall() && !o93.isIndirectCall() &&
    !o93.isReturn() && !o93.isJump() && !o93.isConditionalJump() &&
    !o93.isUnconditionalJump() && !o93.isIndirectJump() );

  WIR_Operation o94(
    ARM_Base::OpCode::STR, ARMv4T::OperationFormat::TRRR_3,
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( r3, WIR_Usage::use ) );

  ufAssert( o94.getSize() == 2 );
  ufAssert(
    !o94.isMemoryAccess() && o94.isMemoryStore() && !o94.isMemoryLoad() &&
    !o94.isMove() && !o94.isCall() && !o94.isIndirectCall() &&
    !o94.isReturn() && !o94.isJump() && !o94.isConditionalJump() &&
    !o94.isUnconditionalJump() && !o94.isIndirectJump() );

  WIR_Operation o95(
    ARM_Base::OpCode::STR, ARMv4T::OperationFormat::TRSPC10_3,
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    WIR_RegisterParameter( p.SP(), WIR_Usage::use ),
    ARM_Const10_Unsigned4( 1020 ) );

  ufAssert( o95.getSize() == 2 );
  ufAssert(
    !o95.isMemoryAccess() && o95.isMemoryStore() && !o95.isMemoryLoad() &&
    !o95.isMove() && !o95.isCall() && !o95.isIndirectCall() &&
    !o95.isReturn() && !o95.isJump() && !o95.isConditionalJump() &&
    !o95.isUnconditionalJump() && !o95.isIndirectJump() );

  WIR_Operation o96(
    ARMv4T::OpCode::STRB, ARMv4T::OperationFormat::TRRC5_3,
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    ARM_Const5_Unsigned( 31 ) );

  ufAssert( o96.getSize() == 2 );
  ufAssert(
    !o96.isMemoryAccess() && o96.isMemoryStore() && !o96.isMemoryLoad() &&
    !o96.isMove() && !o96.isCall() && !o96.isIndirectCall() &&
    !o96.isReturn() && !o96.isJump() && !o96.isConditionalJump() &&
    !o96.isUnconditionalJump() && !o96.isIndirectJump() );

  WIR_Operation o97(
    ARM_Base::OpCode::STRB, ARMv4T::OperationFormat::TRRR_3,
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( r3, WIR_Usage::use ) );

  ufAssert( o97.getSize() == 2 );
  ufAssert(
    !o97.isMemoryAccess() && o97.isMemoryStore() && !o97.isMemoryLoad() &&
    !o97.isMove() && !o97.isCall() && !o97.isIndirectCall() &&
    !o97.isReturn() && !o97.isJump() && !o97.isConditionalJump() &&
    !o97.isUnconditionalJump() && !o97.isIndirectJump() );

  WIR_Operation o98(
    ARMv4T::OpCode::STRH, ARMv4T::OperationFormat::TRRC6_3,
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    ARM_Const6_Unsigned( 62 ) );

  ufAssert( o98.getSize() == 2 );
  ufAssert(
    !o98.isMemoryAccess() && o98.isMemoryStore() && !o98.isMemoryLoad() &&
    !o98.isMove() && !o98.isCall() && !o98.isIndirectCall() &&
    !o98.isReturn() && !o98.isJump() && !o98.isConditionalJump() &&
    !o98.isUnconditionalJump() && !o98.isIndirectJump() );

  WIR_Operation o99(
    ARM_Base::OpCode::STRH, ARMv4T::OperationFormat::TRRR_3,
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( r3, WIR_Usage::use ) );

  ufAssert( o99.getSize() == 2 );
  ufAssert(
    !o99.isMemoryAccess() && o99.isMemoryStore() && !o99.isMemoryLoad() &&
    !o99.isMove() && !o99.isCall() && !o99.isIndirectCall() &&
    !o99.isReturn() && !o99.isJump() && !o99.isConditionalJump() &&
    !o99.isUnconditionalJump() && !o99.isIndirectJump() );

  WIR_Operation o100(
    ARM_Base::OpCode::SUB, ARMv4T::OperationFormat::TRRC3_1,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    ARM_Const3_Unsigned( 0 ) );

  ufAssert( o100.getSize() == 2 );
  ufAssert(
    !o100.isMemoryAccess() && !o100.isMemoryStore() && !o100.isMemoryLoad() &&
    !o100.isMove() && !o100.isCall() && !o100.isIndirectCall() &&
    !o100.isReturn() && !o100.isJump() && !o100.isConditionalJump() &&
    !o100.isUnconditionalJump() && !o100.isIndirectJump() );

  WIR_Operation o101(
    ARM_Base::OpCode::SUB, ARMv4T::OperationFormat::TRC8_3,
    WIR_RegisterParameter( r1, WIR_Usage::defuse ),
    ARM_Const8_Unsigned( 255 ) );

  ufAssert( o101.getSize() == 2 );
  ufAssert(
    !o101.isMemoryAccess() && !o101.isMemoryStore() && !o101.isMemoryLoad() &&
    !o101.isMove() && !o101.isCall() && !o101.isIndirectCall() &&
    !o101.isReturn() && !o101.isJump() && !o101.isConditionalJump() &&
    !o101.isUnconditionalJump() && !o101.isIndirectJump() );

  WIR_Operation o102(
    ARM_Base::OpCode::SUB, ARMv4T::OperationFormat::TRRR_1,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( r3, WIR_Usage::use ) );

  ufAssert( o102.getSize() == 2 );
  ufAssert(
    !o102.isMemoryAccess() && !o102.isMemoryStore() && !o102.isMemoryLoad() &&
    !o102.isMove() && !o102.isCall() && !o102.isIndirectCall() &&
    !o102.isReturn() && !o102.isJump() && !o102.isConditionalJump() &&
    !o102.isUnconditionalJump() && !o102.isIndirectJump() );

  WIR_Operation o103(
    ARM_Base::OpCode::SUB, ARMv4T::OperationFormat::TC9_1,
    ARM_Const9_Unsigned( 44 ) );

  ufAssert( o103.getSize() == 2 );
  ufAssert(
    !o103.isMemoryAccess() && !o103.isMemoryStore() && !o103.isMemoryLoad() &&
    !o103.isMove() && !o103.isCall() && !o103.isIndirectCall() &&
    !o103.isReturn() && !o103.isJump() && !o103.isConditionalJump() &&
    !o103.isUnconditionalJump() && !o103.isIndirectJump() );

  WIR_Operation o104(
    ARM_Base::OpCode::SWI, ARMv4T::OperationFormat::TC8_1,
    ARM_Const8_Unsigned( 255 ) );

  ufAssert( o104.getSize() == 2 );
  ufAssert(
    !o104.isMemoryAccess() && !o104.isMemoryStore() && !o104.isMemoryLoad() &&
    !o104.isMove() && !o104.isCall() && o104.isIndirectCall() &&
    !o104.isReturn() && !o104.isJump() && !o104.isConditionalJump() &&
    !o104.isUnconditionalJump() && !o104.isIndirectJump() );

  WIR_Operation o105(
    ARM_Base::OpCode::TST, ARMv4T::OperationFormat::TRR_3,
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    WIR_RegisterParameter( r2, WIR_Usage::use ) );

  ufAssert( o105.getSize() == 2 );
  ufAssert(
    !o105.isMemoryAccess() && !o105.isMemoryStore() && !o105.isMemoryLoad() &&
    !o105.isMove() && !o105.isCall() && !o105.isIndirectCall() &&
    !o105.isReturn() && !o105.isJump() && !o105.isConditionalJump() &&
    !o105.isUnconditionalJump() && !o105.isIndirectJump() );

  return( 0 );
}
