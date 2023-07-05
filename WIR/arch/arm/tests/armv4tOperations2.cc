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

// Include WIR headers
#include <wir/wir.h>
#include <arch/arm/armv4t.h>


using namespace std;
using namespace WIR;


int main( void )
{
  WIR_Init();

  ARMv4T p;
  const ARM_LoRegP &r1 = p.R0(), &r2 = p.R1(), &r3 = p.R2(), &r6 = p.R3(),
                   &r7 = p.R4(), &r8 = p.R5(), &r9 = p.R6(), &r10 = p.R7();
  const ARM_HiRegP &r4 = p.R11(), &r5 = p.R9();
  WIR_BasicBlock b;
  WIR_Function f( "main" );

  cout.iword( WIR_Indentation() ) = 8;
  cout << arm;

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
    cout << o << endl;
  }

  WIR_Operation o1(
    ARM_Base::OpCode::ADC, ARMv4T::OperationFormat::TRR_5,
    WIR_RegisterParameter( r1, WIR_Usage::defuse ),
    WIR_RegisterParameter( r2, WIR_Usage::use ) );
  cout << o1 << endl;

  WIR_Operation o2(
    ARM_Base::OpCode::ADD, ARMv4T::OperationFormat::TRRC3_1,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    ARM_Const3_Unsigned( 0 ) );
  cout << o2 << endl;

  WIR_Operation o3(
    ARM_Base::OpCode::ADD, ARMv4T::OperationFormat::TRC8_3,
    WIR_RegisterParameter( r1, WIR_Usage::defuse ),
    ARM_Const8_Unsigned( 255 ) );
  cout << o3 << endl;

  WIR_Operation o4(
    ARM_Base::OpCode::ADD, ARMv4T::OperationFormat::TRRR_1,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( r3, WIR_Usage::use ) );
  cout << o4 << endl;

  WIR_Operation o5(
    ARM_Base::OpCode::ADD, ARMv4T::OperationFormat::TRR_6,
    WIR_RegisterParameter( r4, WIR_Usage::defuse ),
    WIR_RegisterParameter( r5, WIR_Usage::use ) );
  cout << o5 << endl;

  WIR_Operation o6(
    ARM_Base::OpCode::ADD, ARMv4T::OperationFormat::TRPCC10_1,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( p.PC(), WIR_Usage::use ),
    ARM_Const10_Unsigned4( 1020 ) );
  cout << o6 << endl;

  WIR_Operation o7(
    ARM_Base::OpCode::ADD, ARMv4T::OperationFormat::TRSPC10_1,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( p.SP(), WIR_Usage::use ),
    ARM_Const10_Unsigned4( 1020 ) );
  cout << o7 << endl;

  WIR_Operation o8(
    ARM_Base::OpCode::ADD, ARMv4T::OperationFormat::TC9_1,
    ARM_Const9_Unsigned( 44 ) );
  cout << o8 << endl;

  WIR_Operation o9(
    ARM_Base::OpCode::AND, ARMv4T::OperationFormat::TRR_5,
    WIR_RegisterParameter( r1, WIR_Usage::defuse ),
    WIR_RegisterParameter( r2, WIR_Usage::use ) );
  cout << o9 << endl;

  WIR_Operation o10(
    ARMv4T::OpCode::ASR, ARMv4T::OperationFormat::TRRC6_1,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    ARM_Const6_Unsigned0( 17 ) );
  cout << o10 << endl;

  WIR_Operation o11(
    ARMv4T::OpCode::ASR, ARMv4T::OperationFormat::TRR_5,
    WIR_RegisterParameter( r1, WIR_Usage::defuse ),
    WIR_RegisterParameter( r2, WIR_Usage::use ) );
  cout << o11 << endl;

  for ( auto &cond : conditions ) {
    WIR_Operation o(
      ARM_Base::OpCode::B, ARMv4T::OperationFormat::TCL,
      WIR_ConditionFieldParameter( cond ),
      WIR_LabelParameter( b ) );
    cout << o << endl;
  }

  WIR_Operation o12(
    ARMv4T::OpCode::B, ARMv4T::OperationFormat::TL_1, WIR_LabelParameter( b ) );
  cout << o12 << endl;

  WIR_Operation o13(
    ARMv4T::OpCode::BIC, ARMv4T::OperationFormat::TRR_5,
    WIR_RegisterParameter( r1, WIR_Usage::defuse ),
    WIR_RegisterParameter( r2, WIR_Usage::use ) );
  cout << o13 << endl;

  WIR_Operation o14(
    ARMv4T::OpCode::BL, ARMv4T::OperationFormat::TL_2,
    WIR_LabelParameter( f ) );
  cout << o14 << endl;

  WIR_Operation o15(
    ARMv4T::OpCode::BX, ARMv4T::OperationFormat::TR_1,
    WIR_RegisterParameter( r2, WIR_Usage::use ) );
  cout << o15 << endl;

  WIR_Operation o16(
    ARMv4T::OpCode::BX, ARMv4T::OperationFormat::TR_1,
    WIR_RegisterParameter( p.R14(), WIR_Usage::use ) );
  cout << o16 << endl;

  WIR_Operation o17(
    ARMv4T::OpCode::CMN, ARMv4T::OperationFormat::TRR_5,
    WIR_RegisterParameter( r1, WIR_Usage::defuse ),
    WIR_RegisterParameter( r2, WIR_Usage::use ) );
  cout << o17 << endl;

  WIR_Operation o18(
    ARM_Base::OpCode::CMP, ARMv4T::OperationFormat::TRC8_2,
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    ARM_Const8_Unsigned( 255 ) );
  cout << o18 << endl;

  WIR_Operation o19(
    ARM_Base::OpCode::CMP, ARMv4T::OperationFormat::TRR_3,
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    WIR_RegisterParameter( r2, WIR_Usage::use ) );
  cout << o19 << endl;

  WIR_Operation o20(
    ARM_Base::OpCode::CMP, ARMv4T::OperationFormat::TRR_4,
    WIR_RegisterParameter( r4, WIR_Usage::use ),
    WIR_RegisterParameter( r5, WIR_Usage::use ) );
  cout << o20 << endl;

  WIR_Operation o21(
    ARMv4T::OpCode::EOR, ARMv4T::OperationFormat::TRR_5,
    WIR_RegisterParameter( r1, WIR_Usage::defuse ),
    WIR_RegisterParameter( r2, WIR_Usage::use ) );
  cout << o21 << endl;

  WIR_Operation o22(
    ARMv4T::OpCode::LDMIA, ARMv4T::OperationFormat::TRR1_1,
    WIR_RegisterParameter( r1, WIR_Usage::defuse ),
    WIR_RegisterParameter( r2, WIR_Usage::def ) );
  cout << o22 << endl;

  WIR_Operation o23(
    ARMv4T::OpCode::LDMIA, ARMv4T::OperationFormat::TRR2_1,
    WIR_RegisterParameter( r1, WIR_Usage::defuse ),
    WIR_RegisterParameter( r2, WIR_Usage::def ),
    WIR_RegisterParameter( r3, WIR_Usage::def ) );
  cout << o23 << endl;

  WIR_Operation o24(
    ARMv4T::OpCode::LDMIA, ARMv4T::OperationFormat::TRR3_1,
    WIR_RegisterParameter( r1, WIR_Usage::defuse ),
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::def ),
    WIR_RegisterParameter( r3, WIR_Usage::def ) );
  cout << o24 << endl;

  WIR_Operation o25(
    ARMv4T::OpCode::LDMIA, ARMv4T::OperationFormat::TRR4_1,
    WIR_RegisterParameter( r1, WIR_Usage::defuse ),
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::def ),
    WIR_RegisterParameter( r3, WIR_Usage::def ),
    WIR_RegisterParameter( r6, WIR_Usage::def ) );
  cout << o25 << endl;

  WIR_Operation o26(
    ARMv4T::OpCode::LDMIA, ARMv4T::OperationFormat::TRR5_1,
    WIR_RegisterParameter( r1, WIR_Usage::defuse ),
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::def ),
    WIR_RegisterParameter( r3, WIR_Usage::def ),
    WIR_RegisterParameter( r6, WIR_Usage::def ),
    WIR_RegisterParameter( r7, WIR_Usage::def ) );
  cout << o26 << endl;

  WIR_Operation o27(
    ARMv4T::OpCode::LDMIA, ARMv4T::OperationFormat::TRR6_1,
    WIR_RegisterParameter( r1, WIR_Usage::defuse ),
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::def ),
    WIR_RegisterParameter( r3, WIR_Usage::def ),
    WIR_RegisterParameter( r6, WIR_Usage::def ),
    WIR_RegisterParameter( r7, WIR_Usage::def ),
    WIR_RegisterParameter( r8, WIR_Usage::def ) );
  cout << o27 << endl;

  WIR_Operation o28(
    ARMv4T::OpCode::LDMIA, ARMv4T::OperationFormat::TRR7_1,
    WIR_RegisterParameter( r1, WIR_Usage::defuse ),
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::def ),
    WIR_RegisterParameter( r3, WIR_Usage::def ),
    WIR_RegisterParameter( r6, WIR_Usage::def ),
    WIR_RegisterParameter( r7, WIR_Usage::def ),
    WIR_RegisterParameter( r8, WIR_Usage::def ),
    WIR_RegisterParameter( r9, WIR_Usage::def ) );
  cout << o28 << endl;

  WIR_Operation o29(
    ARMv4T::OpCode::LDMIA, ARMv4T::OperationFormat::TRR8_1,
    WIR_RegisterParameter( r1, WIR_Usage::defuse ),
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::def ),
    WIR_RegisterParameter( r3, WIR_Usage::def ),
    WIR_RegisterParameter( r6, WIR_Usage::def ),
    WIR_RegisterParameter( r7, WIR_Usage::def ),
    WIR_RegisterParameter( r8, WIR_Usage::def ),
    WIR_RegisterParameter( r9, WIR_Usage::def ),
    WIR_RegisterParameter( r10, WIR_Usage::def ) );
  cout << o29 << endl;

  WIR_Operation o30(
    ARMv4T::OpCode::LDR, ARMv4T::OperationFormat::TRRC7_1,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    ARM_Const7_Unsigned( 16 ) );
  cout << o30 << endl;

  WIR_Operation o31(
    ARM_Base::OpCode::LDR, ARMv4T::OperationFormat::TRRR_2,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( r3, WIR_Usage::use ) );
  cout << o31 << endl;

  WIR_Operation o32(
    ARM_Base::OpCode::LDR, ARMv4T::OperationFormat::TRPCC10_2,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( p.PC(), WIR_Usage::use ),
    ARM_Const10_Unsigned4( 1020 ) );
  cout << o32 << endl;

  WIR_Operation o33(
    ARM_Base::OpCode::LDR, ARMv4T::OperationFormat::TRSPC10_2,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( p.SP(), WIR_Usage::use ),
    ARM_Const10_Unsigned4( 1020 ) );
  cout << o33 << endl;

  WIR_Operation o34(
    ARMv4T::OpCode::LDRB, ARMv4T::OperationFormat::TRRC5_2,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    ARM_Const5_Unsigned( 31 ) );
  cout << o34 << endl;

  WIR_Operation o35(
    ARM_Base::OpCode::LDRB, ARMv4T::OperationFormat::TRRR_2,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( r3, WIR_Usage::use ) );
  cout << o35 << endl;

  WIR_Operation o36(
    ARMv4T::OpCode::LDRH, ARMv4T::OperationFormat::TRRC6_2,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    ARM_Const6_Unsigned( 62 ) );
  cout << o36 << endl;

  WIR_Operation o37(
    ARM_Base::OpCode::LDRH, ARMv4T::OperationFormat::TRRR_2,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( r3, WIR_Usage::use ) );
  cout << o37 << endl;

  WIR_Operation o38(
    ARM_Base::OpCode::LDRSB, ARMv4T::OperationFormat::TRRR_2,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( r3, WIR_Usage::use ) );
  cout << o38 << endl;

  WIR_Operation o39(
    ARM_Base::OpCode::LDRSH, ARMv4T::OperationFormat::TRRR_2,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( r3, WIR_Usage::use ) );
  cout << o39 << endl;

  WIR_Operation o40(
    ARMv4T::OpCode::LSL, ARMv4T::OperationFormat::TRRC5_1,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    ARM_Const5_Unsigned( 31 ) );
  cout << o40 << endl;

  WIR_Operation o41(
    ARMv4T::OpCode::LSL, ARMv4T::OperationFormat::TRR_5,
    WIR_RegisterParameter( r1, WIR_Usage::defuse ),
    WIR_RegisterParameter( r2, WIR_Usage::use ) );
  cout << o41 << endl;

  WIR_Operation o42(
    ARMv4T::OpCode::LSR, ARMv4T::OperationFormat::TRRC6_1,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    ARM_Const6_Unsigned0( 17 ) );
  cout << o42 << endl;

  WIR_Operation o43(
    ARMv4T::OpCode::LSR, ARMv4T::OperationFormat::TRR_5,
    WIR_RegisterParameter( r1, WIR_Usage::defuse ),
    WIR_RegisterParameter( r2, WIR_Usage::use ) );
  cout << o43 << endl;

  WIR_Operation o44(
    ARMv4T::OpCode::MOV, ARMv4T::OperationFormat::TRC8_1,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    ARM_Const8_Unsigned( 255 ) );
  cout << o44 << endl;

  WIR_Operation o45(
    ARMv4T::OpCode::MOV, ARMv4T::OperationFormat::TRR_1,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::use ) );
  cout << o45 << endl;

  WIR_Operation o46(
    ARMv4T::OpCode::MOV, ARMv4T::OperationFormat::TRR_2,
    WIR_RegisterParameter( r4, WIR_Usage::def ),
    WIR_RegisterParameter( r5, WIR_Usage::use ) );
  cout << o46 << endl;

  WIR_Operation o47(
    ARMv4T::OpCode::MUL, ARMv4T::OperationFormat::TRR_5,
    WIR_RegisterParameter( r1, WIR_Usage::defuse ),
    WIR_RegisterParameter( r2, WIR_Usage::use ) );
  cout << o47 << endl;

  WIR_Operation o48(
    ARMv4T::OpCode::MVN, ARMv4T::OperationFormat::TRR_1,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::use ) );
  cout << o48 << endl;

  WIR_Operation o49(
    ARMv4T::OpCode::NEG, ARMv4T::OperationFormat::TRR_1,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::use ) );
  cout << o49 << endl;

  WIR_Operation o50(
    ARMv4T::OpCode::ORR, ARMv4T::OperationFormat::TRR_5,
    WIR_RegisterParameter( r1, WIR_Usage::defuse ),
    WIR_RegisterParameter( r2, WIR_Usage::use ) );
  cout << o50 << endl;

  WIR_Operation o51(
    ARMv4T::OpCode::POP, ARMv4T::OperationFormat::TR1_1,
    WIR_RegisterParameter( r1, WIR_Usage::def ) );
  cout << o51 << endl;

  WIR_Operation o52(
    ARMv4T::OpCode::POP, ARMv4T::OperationFormat::TR1PC,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( p.PC(), WIR_Usage::def ) );
  cout << o52 << endl;

  WIR_Operation o53(
    ARMv4T::OpCode::POP, ARMv4T::OperationFormat::TR2_1,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::def ) );
  cout << o53 << endl;

  WIR_Operation o54(
    ARMv4T::OpCode::POP, ARMv4T::OperationFormat::TR2PC,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::def ),
    WIR_RegisterParameter( p.PC(), WIR_Usage::def ) );
  cout << o54 << endl;

  WIR_Operation o55(
    ARMv4T::OpCode::POP, ARMv4T::OperationFormat::TR3_1,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::def ),
    WIR_RegisterParameter( r3, WIR_Usage::def ) );
  cout << o55 << endl;

  WIR_Operation o56(
    ARMv4T::OpCode::POP, ARMv4T::OperationFormat::TR3PC,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::def ),
    WIR_RegisterParameter( r3, WIR_Usage::def ),
    WIR_RegisterParameter( p.PC(), WIR_Usage::def ) );
  cout << o56 << endl;

  WIR_Operation o57(
    ARMv4T::OpCode::POP, ARMv4T::OperationFormat::TR4_1,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::def ),
    WIR_RegisterParameter( r3, WIR_Usage::def ),
    WIR_RegisterParameter( r6, WIR_Usage::def ) );
  cout << o57 << endl;

  WIR_Operation o58(
    ARMv4T::OpCode::POP, ARMv4T::OperationFormat::TR4PC,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::def ),
    WIR_RegisterParameter( r3, WIR_Usage::def ),
    WIR_RegisterParameter( r6, WIR_Usage::def ),
    WIR_RegisterParameter( p.PC(), WIR_Usage::def ) );
  cout << o58 << endl;

  WIR_Operation o59(
    ARMv4T::OpCode::POP, ARMv4T::OperationFormat::TR5_1,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::def ),
    WIR_RegisterParameter( r3, WIR_Usage::def ),
    WIR_RegisterParameter( r6, WIR_Usage::def ),
    WIR_RegisterParameter( r7, WIR_Usage::def ) );
  cout << o59 << endl;

  WIR_Operation o60(
    ARMv4T::OpCode::POP, ARMv4T::OperationFormat::TR5PC,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::def ),
    WIR_RegisterParameter( r3, WIR_Usage::def ),
    WIR_RegisterParameter( r6, WIR_Usage::def ),
    WIR_RegisterParameter( r7, WIR_Usage::def ),
    WIR_RegisterParameter( p.PC(), WIR_Usage::def ) );
  cout << o60 << endl;

  WIR_Operation o61(
    ARMv4T::OpCode::POP, ARMv4T::OperationFormat::TR6_1,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::def ),
    WIR_RegisterParameter( r3, WIR_Usage::def ),
    WIR_RegisterParameter( r6, WIR_Usage::def ),
    WIR_RegisterParameter( r7, WIR_Usage::def ),
    WIR_RegisterParameter( r8, WIR_Usage::def ) );
  cout << o61 << endl;

  WIR_Operation o62(
    ARMv4T::OpCode::POP, ARMv4T::OperationFormat::TR6PC,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::def ),
    WIR_RegisterParameter( r3, WIR_Usage::def ),
    WIR_RegisterParameter( r6, WIR_Usage::def ),
    WIR_RegisterParameter( r7, WIR_Usage::def ),
    WIR_RegisterParameter( r8, WIR_Usage::def ),
    WIR_RegisterParameter( p.PC(), WIR_Usage::def ) );
  cout << o62 << endl;

  WIR_Operation o63(
    ARMv4T::OpCode::POP, ARMv4T::OperationFormat::TR7_1,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::def ),
    WIR_RegisterParameter( r3, WIR_Usage::def ),
    WIR_RegisterParameter( r6, WIR_Usage::def ),
    WIR_RegisterParameter( r7, WIR_Usage::def ),
    WIR_RegisterParameter( r8, WIR_Usage::def ),
    WIR_RegisterParameter( r9, WIR_Usage::def ) );
  cout << o63 << endl;

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
  cout << o64 << endl;

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
  cout << o65 << endl;

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
  cout << o66 << endl;

  WIR_Operation o67(
    ARMv4T::OpCode::PUSH, ARMv4T::OperationFormat::TR1_2,
    WIR_RegisterParameter( r1, WIR_Usage::use ) );
  cout << o67 << endl;

  WIR_Operation o68(
    ARMv4T::OpCode::PUSH, ARMv4T::OperationFormat::TR1LR,
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    WIR_RegisterParameter( p.LR(), WIR_Usage::use ) );
  cout << o68 << endl;

  WIR_Operation o69(
    ARMv4T::OpCode::PUSH, ARMv4T::OperationFormat::TR2_2,
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    WIR_RegisterParameter( r2, WIR_Usage::use ) );
  cout << o69 << endl;

  WIR_Operation o70(
    ARMv4T::OpCode::PUSH, ARMv4T::OperationFormat::TR2LR,
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( p.LR(), WIR_Usage::use ) );
  cout << o70 << endl;

  WIR_Operation o71(
    ARMv4T::OpCode::PUSH, ARMv4T::OperationFormat::TR3_2,
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( r3, WIR_Usage::use ) );
  cout << o71 << endl;

  WIR_Operation o72(
    ARMv4T::OpCode::PUSH, ARMv4T::OperationFormat::TR3LR,
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( r3, WIR_Usage::use ),
    WIR_RegisterParameter( p.LR(), WIR_Usage::use ) );
  cout << o72 << endl;

  WIR_Operation o73(
    ARMv4T::OpCode::PUSH, ARMv4T::OperationFormat::TR4_2,
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( r3, WIR_Usage::use ),
    WIR_RegisterParameter( r6, WIR_Usage::use ) );
  cout << o73 << endl;

  WIR_Operation o74(
    ARMv4T::OpCode::PUSH, ARMv4T::OperationFormat::TR4LR,
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( r3, WIR_Usage::use ),
    WIR_RegisterParameter( r6, WIR_Usage::use ),
    WIR_RegisterParameter( p.LR(), WIR_Usage::use ) );
  cout << o74 << endl;

  WIR_Operation o75(
    ARMv4T::OpCode::PUSH, ARMv4T::OperationFormat::TR5_2,
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( r3, WIR_Usage::use ),
    WIR_RegisterParameter( r6, WIR_Usage::use ),
    WIR_RegisterParameter( r7, WIR_Usage::use ) );
  cout << o75 << endl;

  WIR_Operation o76(
    ARMv4T::OpCode::PUSH, ARMv4T::OperationFormat::TR5LR,
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( r3, WIR_Usage::use ),
    WIR_RegisterParameter( r6, WIR_Usage::use ),
    WIR_RegisterParameter( r7, WIR_Usage::use ),
    WIR_RegisterParameter( p.LR(), WIR_Usage::use ) );
  cout << o76 << endl;

  WIR_Operation o77(
    ARMv4T::OpCode::PUSH, ARMv4T::OperationFormat::TR6_2,
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( r3, WIR_Usage::use ),
    WIR_RegisterParameter( r6, WIR_Usage::use ),
    WIR_RegisterParameter( r7, WIR_Usage::use ),
    WIR_RegisterParameter( r8, WIR_Usage::use ) );
  cout << o77 << endl;

  WIR_Operation o78(
    ARMv4T::OpCode::PUSH, ARMv4T::OperationFormat::TR6LR,
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( r3, WIR_Usage::use ),
    WIR_RegisterParameter( r6, WIR_Usage::use ),
    WIR_RegisterParameter( r7, WIR_Usage::use ),
    WIR_RegisterParameter( r8, WIR_Usage::use ),
    WIR_RegisterParameter( p.LR(), WIR_Usage::use ) );
  cout << o78 << endl;

  WIR_Operation o79(
    ARMv4T::OpCode::PUSH, ARMv4T::OperationFormat::TR7_2,
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( r3, WIR_Usage::use ),
    WIR_RegisterParameter( r6, WIR_Usage::use ),
    WIR_RegisterParameter( r7, WIR_Usage::use ),
    WIR_RegisterParameter( r8, WIR_Usage::use ),
    WIR_RegisterParameter( r9, WIR_Usage::use ) );
  cout << o79 << endl;

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
  cout << o80 << endl;

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
  cout << o81 << endl;

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
  cout << o82 << endl;

  WIR_Operation o83(
    ARMv4T::OpCode::ROR, ARMv4T::OperationFormat::TRR_5,
    WIR_RegisterParameter( r1, WIR_Usage::defuse ),
    WIR_RegisterParameter( r2, WIR_Usage::use ) );
  cout << o83 << endl;

  WIR_Operation o84(
    ARMv4T::OpCode::SBC, ARMv4T::OperationFormat::TRR_5,
    WIR_RegisterParameter( r1, WIR_Usage::defuse ),
    WIR_RegisterParameter( r2, WIR_Usage::use ) );
  cout << o84 << endl;

  WIR_Operation o85(
    ARMv4T::OpCode::STMIA, ARMv4T::OperationFormat::TRR1_2,
    WIR_RegisterParameter( r6, WIR_Usage::defuse ),
    WIR_RegisterParameter( r1, WIR_Usage::use ) );
  cout << o85 << endl;

  WIR_Operation o86(
    ARMv4T::OpCode::STMIA, ARMv4T::OperationFormat::TRR2_2,
    WIR_RegisterParameter( r6, WIR_Usage::defuse ),
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    WIR_RegisterParameter( r2, WIR_Usage::use ) );
  cout << o86 << endl;

  WIR_Operation o87(
    ARMv4T::OpCode::STMIA, ARMv4T::OperationFormat::TRR3_2,
    WIR_RegisterParameter( r6, WIR_Usage::defuse ),
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( r3, WIR_Usage::use ) );
  cout << o87 << endl;

  WIR_Operation o88(
    ARMv4T::OpCode::STMIA, ARMv4T::OperationFormat::TRR4_2,
    WIR_RegisterParameter( r6, WIR_Usage::defuse ),
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( r3, WIR_Usage::use ),
    WIR_RegisterParameter( r6, WIR_Usage::use ) );
  cout << o88 << endl;

  WIR_Operation o89(
    ARMv4T::OpCode::STMIA, ARMv4T::OperationFormat::TRR5_2,
    WIR_RegisterParameter( r6, WIR_Usage::defuse ),
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( r3, WIR_Usage::use ),
    WIR_RegisterParameter( r6, WIR_Usage::use ),
    WIR_RegisterParameter( r7, WIR_Usage::use ) );
  cout << o89 << endl;

  WIR_Operation o90(
    ARMv4T::OpCode::STMIA, ARMv4T::OperationFormat::TRR6_2,
    WIR_RegisterParameter( r6, WIR_Usage::defuse ),
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( r3, WIR_Usage::use ),
    WIR_RegisterParameter( r6, WIR_Usage::use ),
    WIR_RegisterParameter( r7, WIR_Usage::use ),
    WIR_RegisterParameter( r8, WIR_Usage::use ) );
  cout << o90 << endl;

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
  cout << o91 << endl;

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
  cout << o92 << endl;

  WIR_Operation o93(
    ARMv4T::OpCode::STR, ARMv4T::OperationFormat::TRRC7_2,
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    ARM_Const7_Unsigned( 16 ) );
  cout << o93 << endl;

  WIR_Operation o94(
    ARM_Base::OpCode::STR, ARMv4T::OperationFormat::TRRR_3,
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( r3, WIR_Usage::use ) );
  cout << o94 << endl;

  WIR_Operation o95(
    ARM_Base::OpCode::STR, ARMv4T::OperationFormat::TRSPC10_3,
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    WIR_RegisterParameter( p.SP(), WIR_Usage::use ),
    ARM_Const10_Unsigned4( 1020 ) );
  cout << o95 << endl;

  WIR_Operation o96(
    ARMv4T::OpCode::STRB, ARMv4T::OperationFormat::TRRC5_3,
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    ARM_Const5_Unsigned( 31 ) );
  cout << o96 << endl;

  WIR_Operation o97(
    ARM_Base::OpCode::STRB, ARMv4T::OperationFormat::TRRR_3,
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( r3, WIR_Usage::use ) );
  cout << o97 << endl;

  WIR_Operation o98(
    ARMv4T::OpCode::STRH, ARMv4T::OperationFormat::TRRC6_3,
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    ARM_Const6_Unsigned( 62 ) );
  cout << o98 << endl;

  WIR_Operation o99(
    ARM_Base::OpCode::STRH, ARMv4T::OperationFormat::TRRR_3,
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( r3, WIR_Usage::use ) );
  cout << o99 << endl;

  WIR_Operation o100(
    ARM_Base::OpCode::SUB, ARMv4T::OperationFormat::TRRC3_1,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    ARM_Const3_Unsigned( 0 ) );
  cout << o100 << endl;

  WIR_Operation o101(
    ARM_Base::OpCode::SUB, ARMv4T::OperationFormat::TRC8_3,
    WIR_RegisterParameter( r1, WIR_Usage::defuse ),
    ARM_Const8_Unsigned( 255 ) );
  cout << o101 << endl;

  WIR_Operation o102(
    ARM_Base::OpCode::SUB, ARMv4T::OperationFormat::TRRR_1,
    WIR_RegisterParameter( r1, WIR_Usage::def ),
    WIR_RegisterParameter( r2, WIR_Usage::use ),
    WIR_RegisterParameter( r3, WIR_Usage::use ) );
  cout << o102 << endl;

  WIR_Operation o103(
    ARM_Base::OpCode::SUB, ARMv4T::OperationFormat::TC9_1,
    ARM_Const9_Unsigned( 44 ) );
  cout << o103 << endl;

  WIR_Operation o104(
    ARM_Base::OpCode::SWI, ARMv4T::OperationFormat::TC8_1,
    ARM_Const8_Unsigned( 255 ) );
  cout << o104 << endl;

  WIR_Operation o105(
    ARM_Base::OpCode::TST, ARMv4T::OperationFormat::TRR_3,
    WIR_RegisterParameter( r1, WIR_Usage::use ),
    WIR_RegisterParameter( r2, WIR_Usage::use ) );
  cout << o105 << endl;

  return( 0 );
}
