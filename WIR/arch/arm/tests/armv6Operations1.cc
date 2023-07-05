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
#include <arch/arm/armv6.h>


using namespace std;
using namespace WIR;


int main( void )
{
  WIR_Init();

  ARMv6 p;
  const ARM_LoRegP &r3 = p.R3(), &r6 = p.R6();
  const ARM_HiRegP &r8 = p.R8(), &r11 = p.R11();

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
  const vector<ARM_Base::AddressingMode> coprocessors {
    ARM_Base::AddressingMode::p0,
    ARM_Base::AddressingMode::p1,
    ARM_Base::AddressingMode::p2,
    ARM_Base::AddressingMode::p3,
    ARM_Base::AddressingMode::p4,
    ARM_Base::AddressingMode::p5,
    ARM_Base::AddressingMode::p6,
    ARM_Base::AddressingMode::p7,
    ARM_Base::AddressingMode::p8,
    ARM_Base::AddressingMode::p9,
    ARM_Base::AddressingMode::p10,
    ARM_Base::AddressingMode::p11,
    ARM_Base::AddressingMode::p12,
    ARM_Base::AddressingMode::p13,
    ARM_Base::AddressingMode::p14,
    ARM_Base::AddressingMode::p15 };
  const vector<ARM_Base::AddressingMode> endianess {
    ARM_Base::AddressingMode::be,
    ARM_Base::AddressingMode::le };
  const vector<ARM_Base::AddressingMode> multipleModes1 {
    ARM_Base::AddressingMode::ia,
    ARM_Base::AddressingMode::ib,
    ARM_Base::AddressingMode::da,
    ARM_Base::AddressingMode::db,
    ARM_Base::AddressingMode::fd,
    ARM_Base::AddressingMode::ed,
    ARM_Base::AddressingMode::fa,
    ARM_Base::AddressingMode::ea };
  const vector<ARM_Base::AddressingMode> multipleModes2 {
    ARM_Base::AddressingMode::ia,
    ARM_Base::AddressingMode::ib,
    ARM_Base::AddressingMode::da,
    ARM_Base::AddressingMode::db };
  const vector<ARM_Base::AddressingMode> processorStateBits {
    ARM_Base::AddressingMode::cpsra,
    ARM_Base::AddressingMode::cpsrf,
    ARM_Base::AddressingMode::cpsri,
    ARM_Base::AddressingMode::cpsraf,
    ARM_Base::AddressingMode::cpsrai,
    ARM_Base::AddressingMode::cpsrfi,
    ARM_Base::AddressingMode::cpsrafi };

  // The following operations must be accepted for the ARMv6 ISA.
  WIR_Operation o1(
    ARMv6::OpCode::CPS, ARMv6::OperationFormat::C5_1,
    ARM_Const5_Unsigned( 31 ) );

  ufAssert( o1.getSize() == 4 );
  ufAssert(
    !o1.isMemoryAccess() && !o1.isMemoryStore() && !o1.isMemoryLoad() &&
    !o1.isMove() && !o1.isCall() && !o1.isIndirectCall() && !o1.isReturn() &&
    !o1.isJump() && !o1.isConditionalJump() && !o1.isUnconditionalJump() &&
    !o1.isIndirectJump() );

  for ( auto &opcode : vector<ARMv6::OpCode> { ARMv6::OpCode::CPSID,
                                               ARMv6::OpCode::CPSIE } )
    for ( auto &state : processorStateBits ) {
      WIR_Operation o1(
        opcode, ARMv6::OperationFormat::A_1,
        WIR_AddressingModeParameter( state ) );

      ufAssert( o1.getSize() == 4 );
      ufAssert(
        !o1.isMemoryAccess() && !o1.isMemoryStore() && !o1.isMemoryLoad() &&
        !o1.isMove() && !o1.isCall() && !o1.isIndirectCall() &&
        !o1.isReturn() && !o1.isJump() && !o1.isConditionalJump() &&
        !o1.isUnconditionalJump() && !o1.isIndirectJump() );

      WIR_Operation o2(
        opcode, ARMv6::OperationFormat::AC5_1,
        WIR_AddressingModeParameter( state ),
        ARM_Const5_Unsigned( 31 ) );

      ufAssert( o2.getSize() == 4 );
      ufAssert(
        !o2.isMemoryAccess() && !o2.isMemoryStore() && !o2.isMemoryLoad() &&
        !o2.isMove() && !o2.isCall() && !o2.isIndirectCall() &&
        !o2.isReturn() && !o2.isJump() && !o2.isConditionalJump() &&
        !o2.isUnconditionalJump() && !o2.isIndirectJump() );

      WIR_Operation o3(
        opcode, ARMv6::OperationFormat::TA_1,
        WIR_AddressingModeParameter( state ) );

      ufAssert( o3.getSize() == 2 );
      ufAssert(
        !o3.isMemoryAccess() && !o3.isMemoryStore() && !o3.isMemoryLoad() &&
        !o3.isMove() && !o3.isCall() && !o3.isIndirectCall() &&
        !o3.isReturn() && !o3.isJump() && !o3.isConditionalJump() &&
        !o3.isUnconditionalJump() && !o3.isIndirectJump() );
    }

  for ( auto &cond : conditions ) {
    WIR_Operation o(
      ARMv6::OpCode::CPY, ARM_Base::OperationFormat::CRR_1,
      WIR_ConditionFieldParameter( cond ),
      WIR_RegisterParameter( r8, WIR_Usage::def ),
      WIR_RegisterParameter( r3, WIR_Usage::use ) );

    ufAssert( o.getSize() == 4 );
    ufAssert(
      !o.isMemoryAccess() && !o.isMemoryStore() && !o.isMemoryLoad() &&
      !o.isCall() && !o.isIndirectCall() && !o.isReturn() &&
      !o.isJump() && !o.isConditionalJump() && !o.isUnconditionalJump() &&
      !o.isIndirectJump() );

    if ( cond == ARM_Base::Condition::al )
      ufAssert( o.isMove() );
    else
      ufAssert( !o.isMove() );
  }

  WIR_Operation o2(
    ARMv6::OpCode::CPY, ARMv4T::OperationFormat::TRR_2,
    WIR_RegisterParameter( r8, WIR_Usage::def ),
    WIR_RegisterParameter( r3, WIR_Usage::use ) );

    ufAssert( o2.getSize() == 2 );
    ufAssert(
      !o2.isMemoryAccess() && !o2.isMemoryStore() && !o2.isMemoryLoad() &&
      o2.isMove() && !o2.isCall() && !o2.isIndirectCall() &&
      !o2.isReturn() && !o2.isJump() && !o2.isConditionalJump() &&
      !o2.isUnconditionalJump() && !o2.isIndirectJump() );

  for ( auto &cond : conditions ) {
    WIR_Operation o(
      ARMv6::OpCode::LDREX, ARMv6::OperationFormat::CRR_7,
      WIR_ConditionFieldParameter( cond ),
      WIR_RegisterParameter( r8, WIR_Usage::def ),
      WIR_RegisterParameter( r3, WIR_Usage::use ) );

    ufAssert( o.getSize() == 4 );
    ufAssert(
      !o.isMemoryAccess() && !o.isMemoryStore() && o.isMemoryLoad() &&
      !o.isMove() && !o.isCall() && !o.isIndirectCall() && !o.isReturn() &&
      !o.isJump() && !o.isConditionalJump() && !o.isUnconditionalJump() &&
      !o.isIndirectJump() );
  }

  for ( auto &proc : coprocessors ) {
    WIR_Operation o(
      ARMv6::OpCode::MCRR2, ARMv6::OperationFormat::AORRS_2,
      WIR_AddressingModeParameter( proc ),
      ARM_Const3_CoprocessorOpcode( 0 ),
      WIR_RegisterParameter( r8, WIR_Usage::use ),
      WIR_RegisterParameter( r3, WIR_Usage::use ),
      WIR_StringParameter( "c9" ) );

    ufAssert( o.getSize() == 4 );
    ufAssert(
      !o.isMemoryAccess() && !o.isMemoryStore() && !o.isMemoryLoad() &&
      !o.isMove() && !o.isCall() && !o.isIndirectCall() && !o.isReturn() &&
      !o.isJump() && !o.isConditionalJump() && !o.isUnconditionalJump() &&
      !o.isIndirectJump() );
  }

  for ( auto &proc : coprocessors ) {
    WIR_Operation o(
      ARMv6::OpCode::MRRC2, ARMv6::OperationFormat::AORRS_1,
      WIR_AddressingModeParameter( proc ),
      ARM_Const3_CoprocessorOpcode( 0 ),
      WIR_RegisterParameter( r8, WIR_Usage::def ),
      WIR_RegisterParameter( r3, WIR_Usage::def ),
      WIR_StringParameter( "c9" ) );

    ufAssert( o.getSize() == 4 );
    ufAssert(
      !o.isMemoryAccess() && !o.isMemoryStore() && !o.isMemoryLoad() &&
      !o.isMove() && !o.isCall() && !o.isIndirectCall() && !o.isReturn() &&
      !o.isJump() && !o.isConditionalJump() && !o.isUnconditionalJump() &&
      !o.isIndirectJump() );
  }

  for ( auto &cond : conditions ) {
    WIR_Operation o(
      ARMv6::OpCode::PKHBT, ARMv6::OperationFormat::CRRRC5_1,
      WIR_ConditionFieldParameter( cond ),
      WIR_RegisterParameter( r8, WIR_Usage::def ),
      WIR_RegisterParameter( r3, WIR_Usage::use ),
      WIR_RegisterParameter( r11, WIR_Usage::use ),
      ARM_Const5_Unsigned( 31 ) );

    ufAssert( o.getSize() == 4 );
    ufAssert(
      !o.isMemoryAccess() && !o.isMemoryStore() && !o.isMemoryLoad() &&
      !o.isMove() && !o.isCall() && !o.isIndirectCall() && !o.isReturn() &&
      !o.isJump() && !o.isConditionalJump() && !o.isUnconditionalJump() &&
      !o.isIndirectJump() );
  }

  for ( auto &cond : conditions ) {
    WIR_Operation o(
      ARMv6::OpCode::PKHTB, ARMv6::OperationFormat::CRRRC5_3,
      WIR_ConditionFieldParameter( cond ),
      WIR_RegisterParameter( r8, WIR_Usage::def ),
      WIR_RegisterParameter( r3, WIR_Usage::use ),
      WIR_RegisterParameter( r11, WIR_Usage::use ),
      ARM_Const5_Unsigned( 31 ) );

    ufAssert( o.getSize() == 4 );
    ufAssert(
      !o.isMemoryAccess() && !o.isMemoryStore() && !o.isMemoryLoad() &&
      !o.isMove() && !o.isCall() && !o.isIndirectCall() && !o.isReturn() &&
      !o.isJump() && !o.isConditionalJump() && !o.isUnconditionalJump() &&
      !o.isIndirectJump() );
  }

  for ( auto &opcode : vector<ARMv5TE::OpCode> { ARMv6::OpCode::QADD16,
                                                 ARMv6::OpCode::QADD8,
                                                 ARMv6::OpCode::QADDSUBX,
                                                 ARMv6::OpCode::QSUB16,
                                                 ARMv6::OpCode::QSUB8,
                                                 ARMv6::OpCode::QSUBADDX } )
    for ( auto &cond : conditions ) {
      WIR_Operation o(
        opcode, ARMv5TE::OperationFormat::CRRR_1,
        WIR_ConditionFieldParameter( cond ),
        WIR_RegisterParameter( r8, WIR_Usage::def ),
        WIR_RegisterParameter( r11, WIR_Usage::use ),
        WIR_RegisterParameter( r3, WIR_Usage::use ) );

      ufAssert( o.getSize() == 4 );
      ufAssert(
        !o.isMemoryAccess() && !o.isMemoryStore() && !o.isMemoryLoad() &&
        !o.isMove() && !o.isCall() && !o.isIndirectCall() && !o.isReturn() &&
        !o.isJump() && !o.isConditionalJump() && !o.isUnconditionalJump() &&
        !o.isIndirectJump() );
    }

  for ( auto &opcode : vector<ARMv5TE::OpCode> { ARMv6::OpCode::REV,
                                                 ARMv6::OpCode::REV16,
                                                 ARMv6::OpCode::REVSH } )
    for ( auto &cond : conditions ) {
      WIR_Operation o1(
        opcode, ARM_Base::OperationFormat::CRR_1,
        WIR_ConditionFieldParameter( cond ),
        WIR_RegisterParameter( r8, WIR_Usage::def ),
        WIR_RegisterParameter( r3, WIR_Usage::use ) );

      ufAssert( o1.getSize() == 4 );
      ufAssert(
        !o1.isMemoryAccess() && !o1.isMemoryStore() && !o1.isMemoryLoad() &&
        !o1.isMove() && !o1.isCall() && !o1.isIndirectCall() && !o1.isReturn() &&
        !o1.isJump() && !o1.isConditionalJump() && !o1.isUnconditionalJump() &&
        !o1.isIndirectJump() );

      WIR_Operation o2(
        opcode, ARMv4T::OperationFormat::TRR_1,
        WIR_RegisterParameter( r6, WIR_Usage::def ),
        WIR_RegisterParameter( r3, WIR_Usage::use ) );

      ufAssert( o2.getSize() == 2 );
      ufAssert(
        !o2.isMemoryAccess() && !o2.isMemoryStore() && !o2.isMemoryLoad() &&
        !o2.isMove() && !o2.isCall() && !o2.isIndirectCall() && !o2.isReturn() &&
        !o2.isJump() && !o2.isConditionalJump() && !o2.isUnconditionalJump() &&
        !o2.isIndirectJump() );
    }

  for ( auto &mode : multipleModes1 ) {
    WIR_Operation o1(
      ARMv6::OpCode::RFE, ARMv6::OperationFormat::AR_1,
      WIR_AddressingModeParameter( mode ),
      WIR_RegisterParameter( r3, WIR_Usage::use ) );

    ufAssert( o1.getSize() == 4 );
    ufAssert(
      !o1.isMemoryAccess() && !o1.isMemoryStore() && !o1.isMemoryLoad() &&
      !o1.isMove() && !o1.isCall() && !o1.isIndirectCall() && o1.isReturn() &&
      !o1.isJump() && !o1.isConditionalJump() && !o1.isUnconditionalJump() &&
      !o1.isIndirectJump() );

    WIR_Operation o2(
      ARMv6::OpCode::RFE, ARMv6::OperationFormat::AR_2,
      WIR_AddressingModeParameter( mode ),
      WIR_RegisterParameter( r3, WIR_Usage::defuse ) );

    ufAssert( o2.getSize() == 4 );
    ufAssert(
      !o2.isMemoryAccess() && !o2.isMemoryStore() && !o2.isMemoryLoad() &&
      !o2.isMove() && !o2.isCall() && !o2.isIndirectCall() && o2.isReturn() &&
      !o2.isJump() && !o2.isConditionalJump() && !o2.isUnconditionalJump() &&
      !o2.isIndirectJump() );
  }

  for ( auto &opcode : vector<ARMv5TE::OpCode> { ARMv6::OpCode::SADD16,
                                                 ARMv6::OpCode::SADD8,
                                                 ARMv6::OpCode::SADDSUBX,
                                                 ARMv6::OpCode::SEL } )
    for ( auto &cond : conditions ) {
      WIR_Operation o(
        opcode, ARMv5TE::OperationFormat::CRRR_1,
        WIR_ConditionFieldParameter( cond ),
        WIR_RegisterParameter( r8, WIR_Usage::def ),
        WIR_RegisterParameter( r11, WIR_Usage::use ),
        WIR_RegisterParameter( r3, WIR_Usage::use ) );

      ufAssert( o.getSize() == 4 );
      ufAssert(
        !o.isMemoryAccess() && !o.isMemoryStore() && !o.isMemoryLoad() &&
        !o.isMove() && !o.isCall() && !o.isIndirectCall() && !o.isReturn() &&
        !o.isJump() && !o.isConditionalJump() && !o.isUnconditionalJump() &&
        !o.isIndirectJump() );
    }

  for ( auto &mode : endianess ) {
    WIR_Operation o1(
      ARMv6::OpCode::SETEND, ARMv6::OperationFormat::A_2,
      WIR_AddressingModeParameter( mode ) );

    ufAssert( o1.getSize() == 4 );
    ufAssert(
      !o1.isMemoryAccess() && !o1.isMemoryStore() && !o1.isMemoryLoad() &&
      !o1.isMove() && !o1.isCall() && !o1.isIndirectCall() && !o1.isReturn() &&
      !o1.isJump() && !o1.isConditionalJump() && !o1.isUnconditionalJump() &&
      !o1.isIndirectJump() );

    WIR_Operation o2(
      ARMv6::OpCode::SETEND, ARMv6::OperationFormat::TA_2,
      WIR_AddressingModeParameter( mode ) );

    ufAssert( o2.getSize() == 2 );
    ufAssert(
      !o2.isMemoryAccess() && !o2.isMemoryStore() && !o2.isMemoryLoad() &&
      !o2.isMove() && !o2.isCall() && !o2.isIndirectCall() && !o2.isReturn() &&
      !o2.isJump() && !o2.isConditionalJump() && !o2.isUnconditionalJump() &&
      !o2.isIndirectJump() );
  }

  for ( auto &opcode : vector<ARMv5TE::OpCode> { ARMv6::OpCode::SADD16,
                                                 ARMv6::OpCode::SADD8,
                                                 ARMv6::OpCode::SHADDSUBX,
                                                 ARMv6::OpCode::SHSUB16,
                                                 ARMv6::OpCode::SHSUB8,
                                                 ARMv6::OpCode::SHSUBADDX } )
    for ( auto &cond : conditions ) {
      WIR_Operation o(
        opcode, ARMv5TE::OperationFormat::CRRR_1,
        WIR_ConditionFieldParameter( cond ),
        WIR_RegisterParameter( r8, WIR_Usage::def ),
        WIR_RegisterParameter( r11, WIR_Usage::use ),
        WIR_RegisterParameter( r3, WIR_Usage::use ) );

      ufAssert( o.getSize() == 4 );
      ufAssert(
        !o.isMemoryAccess() && !o.isMemoryStore() && !o.isMemoryLoad() &&
        !o.isMove() && !o.isCall() && !o.isIndirectCall() && !o.isReturn() &&
        !o.isJump() && !o.isConditionalJump() && !o.isUnconditionalJump() &&
        !o.isIndirectJump() );
    }

  for ( auto &cond : conditions ) {
    WIR_Operation o1(
      ARMv6::OpCode::SMLAD, ARMv6::OperationFormat::CRRRR_1,
      WIR_ConditionFieldParameter( cond ),
      WIR_RegisterParameter( r8, WIR_Usage::def ),
      WIR_RegisterParameter( r3, WIR_Usage::use ),
      WIR_RegisterParameter( r11, WIR_Usage::use ),
      WIR_RegisterParameter( r6, WIR_Usage::use ) );

    ufAssert( o1.getSize() == 4 );
    ufAssert(
      !o1.isMemoryAccess() && !o1.isMemoryStore() && !o1.isMemoryLoad() &&
      !o1.isMove() && !o1.isCall() && !o1.isIndirectCall() && !o1.isReturn() &&
      !o1.isJump() && !o1.isConditionalJump() && !o1.isUnconditionalJump() &&
      !o1.isIndirectJump() );

    WIR_Operation o2(
      ARMv6::OpCode::SMLADX, ARMv6::OperationFormat::CRRRR_1,
      WIR_ConditionFieldParameter( cond ),
      WIR_RegisterParameter( r8, WIR_Usage::def ),
      WIR_RegisterParameter( r3, WIR_Usage::use ),
      WIR_RegisterParameter( r11, WIR_Usage::use ),
      WIR_RegisterParameter( r6, WIR_Usage::use ) );

    ufAssert( o2.getSize() == 4 );
    ufAssert(
      !o2.isMemoryAccess() && !o2.isMemoryStore() && !o2.isMemoryLoad() &&
      !o2.isMove() && !o2.isCall() && !o2.isIndirectCall() && !o2.isReturn() &&
      !o2.isJump() && !o2.isConditionalJump() && !o2.isUnconditionalJump() &&
      !o2.isIndirectJump() );
  }

  for ( auto &cond : conditions ) {
    WIR_Operation o1(
      ARMv6::OpCode::SMLALD, ARMv6::OperationFormat::CRRRR_5,
      WIR_ConditionFieldParameter( cond ),
      WIR_RegisterParameter( r8, WIR_Usage::def ),
      WIR_RegisterParameter( r3, WIR_Usage::def ),
      WIR_RegisterParameter( r11, WIR_Usage::use ),
      WIR_RegisterParameter( r6, WIR_Usage::use ) );

    ufAssert( o1.getSize() == 4 );
    ufAssert(
      !o1.isMemoryAccess() && !o1.isMemoryStore() && !o1.isMemoryLoad() &&
      !o1.isMove() && !o1.isCall() && !o1.isIndirectCall() && !o1.isReturn() &&
      !o1.isJump() && !o1.isConditionalJump() && !o1.isUnconditionalJump() &&
      !o1.isIndirectJump() );

    WIR_Operation o2(
      ARMv6::OpCode::SMLALDX, ARMv6::OperationFormat::CRRRR_5,
      WIR_ConditionFieldParameter( cond ),
      WIR_RegisterParameter( r8, WIR_Usage::def ),
      WIR_RegisterParameter( r3, WIR_Usage::def ),
      WIR_RegisterParameter( r11, WIR_Usage::use ),
      WIR_RegisterParameter( r6, WIR_Usage::use ) );

    ufAssert( o2.getSize() == 4 );
    ufAssert(
      !o2.isMemoryAccess() && !o2.isMemoryStore() && !o2.isMemoryLoad() &&
      !o2.isMove() && !o2.isCall() && !o2.isIndirectCall() && !o2.isReturn() &&
      !o2.isJump() && !o2.isConditionalJump() && !o2.isUnconditionalJump() &&
      !o2.isIndirectJump() );
  }

  for ( auto &cond : conditions ) {
    WIR_Operation o1(
      ARMv6::OpCode::SMLSD, ARMv6::OperationFormat::CRRRR_1,
      WIR_ConditionFieldParameter( cond ),
      WIR_RegisterParameter( r8, WIR_Usage::def ),
      WIR_RegisterParameter( r3, WIR_Usage::use ),
      WIR_RegisterParameter( r11, WIR_Usage::use ),
      WIR_RegisterParameter( r6, WIR_Usage::use ) );

    ufAssert( o1.getSize() == 4 );
    ufAssert(
      !o1.isMemoryAccess() && !o1.isMemoryStore() && !o1.isMemoryLoad() &&
      !o1.isMove() && !o1.isCall() && !o1.isIndirectCall() && !o1.isReturn() &&
      !o1.isJump() && !o1.isConditionalJump() && !o1.isUnconditionalJump() &&
      !o1.isIndirectJump() );

    WIR_Operation o2(
      ARMv6::OpCode::SMLSDX, ARMv6::OperationFormat::CRRRR_1,
      WIR_ConditionFieldParameter( cond ),
      WIR_RegisterParameter( r8, WIR_Usage::def ),
      WIR_RegisterParameter( r3, WIR_Usage::use ),
      WIR_RegisterParameter( r11, WIR_Usage::use ),
      WIR_RegisterParameter( r6, WIR_Usage::use ) );

    ufAssert( o2.getSize() == 4 );
    ufAssert(
      !o2.isMemoryAccess() && !o2.isMemoryStore() && !o2.isMemoryLoad() &&
      !o2.isMove() && !o2.isCall() && !o2.isIndirectCall() && !o2.isReturn() &&
      !o2.isJump() && !o2.isConditionalJump() && !o2.isUnconditionalJump() &&
      !o2.isIndirectJump() );
  }

  for ( auto &cond : conditions ) {
    WIR_Operation o1(
      ARMv6::OpCode::SMLSLD, ARMv6::OperationFormat::CRRRR_5,
      WIR_ConditionFieldParameter( cond ),
      WIR_RegisterParameter( r8, WIR_Usage::def ),
      WIR_RegisterParameter( r3, WIR_Usage::def ),
      WIR_RegisterParameter( r11, WIR_Usage::use ),
      WIR_RegisterParameter( r6, WIR_Usage::use ) );

    ufAssert( o1.getSize() == 4 );
    ufAssert(
      !o1.isMemoryAccess() && !o1.isMemoryStore() && !o1.isMemoryLoad() &&
      !o1.isMove() && !o1.isCall() && !o1.isIndirectCall() && !o1.isReturn() &&
      !o1.isJump() && !o1.isConditionalJump() && !o1.isUnconditionalJump() &&
      !o1.isIndirectJump() );

    WIR_Operation o2(
      ARMv6::OpCode::SMLSLDX, ARMv6::OperationFormat::CRRRR_5,
      WIR_ConditionFieldParameter( cond ),
      WIR_RegisterParameter( r8, WIR_Usage::def ),
      WIR_RegisterParameter( r3, WIR_Usage::def ),
      WIR_RegisterParameter( r11, WIR_Usage::use ),
      WIR_RegisterParameter( r6, WIR_Usage::use ) );

    ufAssert( o2.getSize() == 4 );
    ufAssert(
      !o2.isMemoryAccess() && !o2.isMemoryStore() && !o2.isMemoryLoad() &&
      !o2.isMove() && !o2.isCall() && !o2.isIndirectCall() && !o2.isReturn() &&
      !o2.isJump() && !o2.isConditionalJump() && !o2.isUnconditionalJump() &&
      !o2.isIndirectJump() );
  }

  for ( auto &opcode : vector<ARMv5TE::OpCode> { ARMv6::OpCode::SMMLA,
                                                 ARMv6::OpCode::SMMLAR,
                                                 ARMv6::OpCode::SMMLS,
                                                 ARMv6::OpCode::SMMLSR } )
    for ( auto &cond : conditions ) {
      WIR_Operation o(
        opcode, ARMv6::OperationFormat::CRRRR_1,
        WIR_ConditionFieldParameter( cond ),
        WIR_RegisterParameter( r8, WIR_Usage::def ),
        WIR_RegisterParameter( r11, WIR_Usage::use ),
        WIR_RegisterParameter( r3, WIR_Usage::use ),
        WIR_RegisterParameter( r6, WIR_Usage::use ) );

      ufAssert( o.getSize() == 4 );
      ufAssert(
        !o.isMemoryAccess() && !o.isMemoryStore() && !o.isMemoryLoad() &&
        !o.isMove() && !o.isCall() && !o.isIndirectCall() && !o.isReturn() &&
        !o.isJump() && !o.isConditionalJump() && !o.isUnconditionalJump() &&
        !o.isIndirectJump() );
    }

  for ( auto &opcode : vector<ARMv5TE::OpCode> { ARMv6::OpCode::SMMUL,
                                                 ARMv6::OpCode::SMMULR,
                                                 ARMv6::OpCode::SMUAD,
                                                 ARMv6::OpCode::SMUADX,
                                                 ARMv6::OpCode::SMUSD,
                                                 ARMv6::OpCode::SMUSDX } )
    for ( auto &cond : conditions ) {
      WIR_Operation o(
        opcode, ARMv6::OperationFormat::CRRR_1,
        WIR_ConditionFieldParameter( cond ),
        WIR_RegisterParameter( r8, WIR_Usage::def ),
        WIR_RegisterParameter( r3, WIR_Usage::use ),
        WIR_RegisterParameter( r6, WIR_Usage::use ) );

      ufAssert( o.getSize() == 4 );
      ufAssert(
        !o.isMemoryAccess() && !o.isMemoryStore() && !o.isMemoryLoad() &&
        !o.isMove() && !o.isCall() && !o.isIndirectCall() && !o.isReturn() &&
        !o.isJump() && !o.isConditionalJump() && !o.isUnconditionalJump() &&
        !o.isIndirectJump() );
    }

  for ( auto &mode : multipleModes2 ) {
    WIR_Operation o1(
      ARMv6::OpCode::SRS, ARMv6::OperationFormat::AC5_2,
      WIR_AddressingModeParameter( mode ),
      ARM_Const5_Unsigned( 31 ) );

    ufAssert( o1.getSize() == 4 );
    ufAssert(
      !o1.isMemoryAccess() && o1.isMemoryStore() && !o1.isMemoryLoad() &&
      !o1.isMove() && !o1.isCall() && !o1.isIndirectCall() && !o1.isReturn() &&
      !o1.isJump() && !o1.isConditionalJump() && !o1.isUnconditionalJump() &&
      !o1.isIndirectJump() );

    WIR_Operation o2(
      ARMv6::OpCode::SRS, ARMv6::OperationFormat::AC5_3,
      WIR_AddressingModeParameter( mode ),
      ARM_Const5_Unsigned( 31 ) );

    ufAssert( o2.getSize() == 4 );
    ufAssert(
      !o2.isMemoryAccess() && o2.isMemoryStore() && !o2.isMemoryLoad() &&
      !o2.isMove() && !o2.isCall() && !o2.isIndirectCall() && !o2.isReturn() &&
      !o2.isJump() && !o2.isConditionalJump() && !o2.isUnconditionalJump() &&
      !o2.isIndirectJump() );
  }

  for ( auto &cond : conditions ) {
    WIR_Operation o1(
      ARMv6::OpCode::SSAT, ARMv6::OperationFormat::CRC60R_1,
      WIR_ConditionFieldParameter( cond ),
      WIR_RegisterParameter( r8, WIR_Usage::def ),
      ARM_Const6_Unsigned0( 27 ),
      WIR_RegisterParameter( r6, WIR_Usage::use ) );

    ufAssert( o1.getSize() == 4 );
    ufAssert(
      !o1.isMemoryAccess() && !o1.isMemoryStore() && !o1.isMemoryLoad() &&
      !o1.isMove() && !o1.isCall() && !o1.isIndirectCall() && !o1.isReturn() &&
      !o1.isJump() && !o1.isConditionalJump() && !o1.isUnconditionalJump() &&
      !o1.isIndirectJump() );

    WIR_Operation o2(
      ARMv6::OpCode::SSAT, ARMv6::OperationFormat::CRC60RC5_1,
      WIR_ConditionFieldParameter( cond ),
      WIR_RegisterParameter( r8, WIR_Usage::def ),
      ARM_Const6_Unsigned0( 27 ),
      WIR_RegisterParameter( r6, WIR_Usage::use ),
      ARM_Const5_Unsigned( 14 ) );

    ufAssert( o2.getSize() == 4 );
    ufAssert(
      !o2.isMemoryAccess() && !o2.isMemoryStore() && !o2.isMemoryLoad() &&
      !o2.isMove() && !o2.isCall() && !o2.isIndirectCall() && !o2.isReturn() &&
      !o2.isJump() && !o2.isConditionalJump() && !o2.isUnconditionalJump() &&
      !o2.isIndirectJump() );

    WIR_Operation o3(
      ARMv6::OpCode::SSAT, ARMv6::OperationFormat::CRC60RC60_1,
      WIR_ConditionFieldParameter( cond ),
      WIR_RegisterParameter( r8, WIR_Usage::def ),
      ARM_Const6_Unsigned0( 27 ),
      WIR_RegisterParameter( r6, WIR_Usage::use ),
      ARM_Const6_Unsigned0( 14 ) );

    ufAssert( o3.getSize() == 4 );
    ufAssert(
      !o3.isMemoryAccess() && !o3.isMemoryStore() && !o3.isMemoryLoad() &&
      !o3.isMove() && !o3.isCall() && !o3.isIndirectCall() && !o3.isReturn() &&
      !o3.isJump() && !o3.isConditionalJump() && !o3.isUnconditionalJump() &&
      !o3.isIndirectJump() );

    WIR_Operation o4(
      ARMv6::OpCode::SSAT16, ARMv6::OperationFormat::CRC5SPR_1,
      WIR_ConditionFieldParameter( cond ),
      WIR_RegisterParameter( r8, WIR_Usage::def ),
      ARM_Const5_SatPos( 16 ),
      WIR_RegisterParameter( r6, WIR_Usage::use ) );

    ufAssert( o4.getSize() == 4 );
    ufAssert(
      !o4.isMemoryAccess() && !o4.isMemoryStore() && !o4.isMemoryLoad() &&
      !o4.isMove() && !o4.isCall() && !o4.isIndirectCall() && !o4.isReturn() &&
      !o4.isJump() && !o4.isConditionalJump() && !o4.isUnconditionalJump() &&
      !o4.isIndirectJump() );
  }

  for ( auto &opcode : vector<ARMv5TE::OpCode> { ARMv6::OpCode::SSUB16,
                                                 ARMv6::OpCode::SSUB8,
                                                 ARMv6::OpCode::SSUBADDX } )
    for ( auto &cond : conditions ) {
      WIR_Operation o(
        opcode, ARMv5TE::OperationFormat::CRRR_1,
        WIR_ConditionFieldParameter( cond ),
        WIR_RegisterParameter( r8, WIR_Usage::def ),
        WIR_RegisterParameter( r11, WIR_Usage::use ),
        WIR_RegisterParameter( r3, WIR_Usage::use ) );

      ufAssert( o.getSize() == 4 );
      ufAssert(
        !o.isMemoryAccess() && !o.isMemoryStore() && !o.isMemoryLoad() &&
        !o.isMove() && !o.isCall() && !o.isIndirectCall() && !o.isReturn() &&
        !o.isJump() && !o.isConditionalJump() && !o.isUnconditionalJump() &&
        !o.isIndirectJump() );
    }

  for ( auto &cond : conditions ) {
    WIR_Operation o(
      ARMv6::OpCode::STREX, ARMv6::OperationFormat::CRRR_5,
      WIR_ConditionFieldParameter( cond ),
      WIR_RegisterParameter( r8, WIR_Usage::def ),
      WIR_RegisterParameter( r11, WIR_Usage::use ),
      WIR_RegisterParameter( r6, WIR_Usage::use ) );

    ufAssert( o.getSize() == 4 );
    ufAssert(
      !o.isMemoryAccess() && o.isMemoryStore() && !o.isMemoryLoad() &&
      !o.isMove() && !o.isCall() && !o.isIndirectCall() && !o.isReturn() &&
      !o.isJump() && !o.isConditionalJump() && !o.isUnconditionalJump() &&
      !o.isIndirectJump() );
  }

  for ( auto &opcode : vector<ARMv5TE::OpCode> { ARMv6::OpCode::SXTAB,
                                                 ARMv6::OpCode::SXTAB16,
                                                 ARMv6::OpCode::SXTAH } )
    for ( auto &mode : vector<ARM_Base::AddressingMode> { ARM_Base::AddressingMode::ror0,
                                                          ARM_Base::AddressingMode::ror8,
                                                          ARM_Base::AddressingMode::ror16,
                                                          ARM_Base::AddressingMode::ror24 } )
      for ( auto &cond : conditions ) {
        WIR_Operation o(
          opcode, ARMv6::OperationFormat::CRRRA_1,
          WIR_ConditionFieldParameter( cond ),
          WIR_RegisterParameter( r8, WIR_Usage::def ),
          WIR_RegisterParameter( r11, WIR_Usage::use ),
          WIR_RegisterParameter( r3, WIR_Usage::use ),
          WIR_AddressingModeParameter( mode ) );

        ufAssert( o.getSize() == 4 );
        ufAssert(
          !o.isMemoryAccess() && !o.isMemoryStore() && !o.isMemoryLoad() &&
          !o.isMove() && !o.isCall() && !o.isIndirectCall() && !o.isReturn() &&
          !o.isJump() && !o.isConditionalJump() && !o.isUnconditionalJump() &&
          !o.isIndirectJump() );
      }

  for ( auto &opcode : vector<ARMv5TE::OpCode> { ARMv6::OpCode::SXTB,
                                                 ARMv6::OpCode::SXTB16,
                                                 ARMv6::OpCode::SXTH } )
    for ( auto &mode : vector<ARM_Base::AddressingMode> { ARM_Base::AddressingMode::ror0,
                                                          ARM_Base::AddressingMode::ror8,
                                                          ARM_Base::AddressingMode::ror16,
                                                          ARM_Base::AddressingMode::ror24 } )
      for ( auto &cond : conditions ) {
        WIR_Operation o(
          opcode, ARMv6::OperationFormat::CRRA_1,
          WIR_ConditionFieldParameter( cond ),
          WIR_RegisterParameter( r8, WIR_Usage::def ),
          WIR_RegisterParameter( r3, WIR_Usage::use ),
          WIR_AddressingModeParameter( mode ) );

        ufAssert( o.getSize() == 4 );
        ufAssert(
          !o.isMemoryAccess() && !o.isMemoryStore() && !o.isMemoryLoad() &&
          !o.isMove() && !o.isCall() && !o.isIndirectCall() && !o.isReturn() &&
          !o.isJump() && !o.isConditionalJump() && !o.isUnconditionalJump() &&
          !o.isIndirectJump() );
      }

  for ( auto &opcode : vector<ARMv5TE::OpCode> { ARMv6::OpCode::SXTB,
                                                 ARMv6::OpCode::SXTH } ) {
    WIR_Operation o(
      opcode, ARMv4T::OperationFormat::TRR_1,
      WIR_RegisterParameter( r6, WIR_Usage::def ),
      WIR_RegisterParameter( r3, WIR_Usage::use ) );

    ufAssert( o.getSize() == 2 );
    ufAssert(
      !o.isMemoryAccess() && !o.isMemoryStore() && !o.isMemoryLoad() &&
      !o.isMove() && !o.isCall() && !o.isIndirectCall() && !o.isReturn() &&
      !o.isJump() && !o.isConditionalJump() && !o.isUnconditionalJump() &&
      !o.isIndirectJump() );
  }

  for ( auto &opcode : vector<ARMv5TE::OpCode> { ARMv6::OpCode::UADD16,
                                                 ARMv6::OpCode::UADD8,
                                                 ARMv6::OpCode::UADDSUBX,
                                                 ARMv6::OpCode::UHADD16,
                                                 ARMv6::OpCode::UHADD8,
                                                 ARMv6::OpCode::UHADDSUBX,
                                                 ARMv6::OpCode::UHSUB16,
                                                 ARMv6::OpCode::UHSUB8,
                                                 ARMv6::OpCode::UHSUBADDX } )
    for ( auto &cond : conditions ) {
      WIR_Operation o(
        opcode, ARMv5TE::OperationFormat::CRRR_1,
        WIR_ConditionFieldParameter( cond ),
        WIR_RegisterParameter( r8, WIR_Usage::def ),
        WIR_RegisterParameter( r11, WIR_Usage::use ),
        WIR_RegisterParameter( r3, WIR_Usage::use ) );

      ufAssert( o.getSize() == 4 );
      ufAssert(
        !o.isMemoryAccess() && !o.isMemoryStore() && !o.isMemoryLoad() &&
        !o.isMove() && !o.isCall() && !o.isIndirectCall() && !o.isReturn() &&
        !o.isJump() && !o.isConditionalJump() && !o.isUnconditionalJump() &&
        !o.isIndirectJump() );
    }

  for ( auto &cond : conditions ) {
    WIR_Operation o(
      ARMv6::OpCode::UMAAL, ARMv6::OperationFormat::CRRRR_5,
      WIR_ConditionFieldParameter( cond ),
      WIR_RegisterParameter( r8, WIR_Usage::def ),
      WIR_RegisterParameter( r3, WIR_Usage::def ),
      WIR_RegisterParameter( r11, WIR_Usage::use ),
      WIR_RegisterParameter( r6, WIR_Usage::use ) );

    ufAssert( o.getSize() == 4 );
    ufAssert(
      !o.isMemoryAccess() && !o.isMemoryStore() && !o.isMemoryLoad() &&
      !o.isMove() && !o.isCall() && !o.isIndirectCall() && !o.isReturn() &&
      !o.isJump() && !o.isConditionalJump() && !o.isUnconditionalJump() &&
      !o.isIndirectJump() );
  }

  for ( auto &opcode : vector<ARMv5TE::OpCode> { ARMv6::OpCode::UQADD16,
                                                 ARMv6::OpCode::UQADD8,
                                                 ARMv6::OpCode::UQADDSUBX,
                                                 ARMv6::OpCode::UQSUB16,
                                                 ARMv6::OpCode::UQSUB8,
                                                 ARMv6::OpCode::UQSUBADDX,
                                                 ARMv6::OpCode::USAD8 } )
    for ( auto &cond : conditions ) {
      WIR_Operation o(
        opcode, ARMv5TE::OperationFormat::CRRR_1,
        WIR_ConditionFieldParameter( cond ),
        WIR_RegisterParameter( r8, WIR_Usage::def ),
        WIR_RegisterParameter( r11, WIR_Usage::use ),
        WIR_RegisterParameter( r3, WIR_Usage::use ) );

      ufAssert( o.getSize() == 4 );
      ufAssert(
        !o.isMemoryAccess() && !o.isMemoryStore() && !o.isMemoryLoad() &&
        !o.isMove() && !o.isCall() && !o.isIndirectCall() && !o.isReturn() &&
        !o.isJump() && !o.isConditionalJump() && !o.isUnconditionalJump() &&
        !o.isIndirectJump() );
    }

  for ( auto &cond : conditions ) {
    WIR_Operation o(
      ARMv6::OpCode::USADA8, ARMv6::OperationFormat::CRRRR_1,
      WIR_ConditionFieldParameter( cond ),
      WIR_RegisterParameter( r8, WIR_Usage::def ),
      WIR_RegisterParameter( r3, WIR_Usage::use ),
      WIR_RegisterParameter( r11, WIR_Usage::use ),
      WIR_RegisterParameter( r6, WIR_Usage::use ) );

    ufAssert( o.getSize() == 4 );
    ufAssert(
      !o.isMemoryAccess() && !o.isMemoryStore() && !o.isMemoryLoad() &&
      !o.isMove() && !o.isCall() && !o.isIndirectCall() && !o.isReturn() &&
      !o.isJump() && !o.isConditionalJump() && !o.isUnconditionalJump() &&
      !o.isIndirectJump() );
  }

  for ( auto &cond : conditions ) {
    WIR_Operation o1(
      ARMv6::OpCode::USAT, ARMv6::OperationFormat::CRC5RC5_1,
      WIR_ConditionFieldParameter( cond ),
      WIR_RegisterParameter( r8, WIR_Usage::def ),
      ARM_Const5_Unsigned0( 31 ),
      WIR_RegisterParameter( r6, WIR_Usage::use ),
      ARM_Const5_Unsigned( 14 ) );

    ufAssert( o1.getSize() == 4 );
    ufAssert(
      !o1.isMemoryAccess() && !o1.isMemoryStore() && !o1.isMemoryLoad() &&
      !o1.isMove() && !o1.isCall() && !o1.isIndirectCall() && !o1.isReturn() &&
      !o1.isJump() && !o1.isConditionalJump() && !o1.isUnconditionalJump() &&
      !o1.isIndirectJump() );

    WIR_Operation o2(
      ARMv6::OpCode::USAT, ARMv6::OperationFormat::CRC5RC60_1,
      WIR_ConditionFieldParameter( cond ),
      WIR_RegisterParameter( r8, WIR_Usage::def ),
      ARM_Const5_Unsigned0( 27 ),
      WIR_RegisterParameter( r6, WIR_Usage::use ),
      ARM_Const6_Unsigned0( 14 ) );

    ufAssert( o2.getSize() == 4 );
    ufAssert(
      !o2.isMemoryAccess() && !o2.isMemoryStore() && !o2.isMemoryLoad() &&
      !o2.isMove() && !o2.isCall() && !o2.isIndirectCall() && !o2.isReturn() &&
      !o2.isJump() && !o2.isConditionalJump() && !o2.isUnconditionalJump() &&
      !o2.isIndirectJump() );

    WIR_Operation o3(
      ARMv6::OpCode::USAT16, ARMv6::OperationFormat::CRC4R_1,
      WIR_ConditionFieldParameter( cond ),
      WIR_RegisterParameter( r8, WIR_Usage::def ),
      ARM_Const4_CoprocessorOpcode( 15 ),
      WIR_RegisterParameter( r6, WIR_Usage::use ) );

    ufAssert( o3.getSize() == 4 );
    ufAssert(
      !o3.isMemoryAccess() && !o3.isMemoryStore() && !o3.isMemoryLoad() &&
      !o3.isMove() && !o3.isCall() && !o3.isIndirectCall() && !o3.isReturn() &&
      !o3.isJump() && !o3.isConditionalJump() && !o3.isUnconditionalJump() &&
      !o3.isIndirectJump() );
  }

  for ( auto &opcode : vector<ARMv5TE::OpCode> { ARMv6::OpCode::USUB16,
                                                 ARMv6::OpCode::USUB8,
                                                 ARMv6::OpCode::USUBADDX } )
    for ( auto &cond : conditions ) {
      WIR_Operation o(
        opcode, ARMv5TE::OperationFormat::CRRR_1,
        WIR_ConditionFieldParameter( cond ),
        WIR_RegisterParameter( r8, WIR_Usage::def ),
        WIR_RegisterParameter( r11, WIR_Usage::use ),
        WIR_RegisterParameter( r3, WIR_Usage::use ) );

      ufAssert( o.getSize() == 4 );
      ufAssert(
        !o.isMemoryAccess() && !o.isMemoryStore() && !o.isMemoryLoad() &&
        !o.isMove() && !o.isCall() && !o.isIndirectCall() && !o.isReturn() &&
        !o.isJump() && !o.isConditionalJump() && !o.isUnconditionalJump() &&
        !o.isIndirectJump() );
    }

  for ( auto &opcode : vector<ARMv5TE::OpCode> { ARMv6::OpCode::UXTAB,
                                                 ARMv6::OpCode::UXTAB16,
                                                 ARMv6::OpCode::UXTAH } )
    for ( auto &mode : vector<ARM_Base::AddressingMode> { ARM_Base::AddressingMode::ror0,
                                                          ARM_Base::AddressingMode::ror8,
                                                          ARM_Base::AddressingMode::ror16,
                                                          ARM_Base::AddressingMode::ror24 } )
      for ( auto &cond : conditions ) {
        WIR_Operation o(
          opcode, ARMv6::OperationFormat::CRRRA_1,
          WIR_ConditionFieldParameter( cond ),
          WIR_RegisterParameter( r8, WIR_Usage::def ),
          WIR_RegisterParameter( r11, WIR_Usage::use ),
          WIR_RegisterParameter( r3, WIR_Usage::use ),
          WIR_AddressingModeParameter( mode ) );

        ufAssert( o.getSize() == 4 );
        ufAssert(
          !o.isMemoryAccess() && !o.isMemoryStore() && !o.isMemoryLoad() &&
          !o.isMove() && !o.isCall() && !o.isIndirectCall() && !o.isReturn() &&
          !o.isJump() && !o.isConditionalJump() && !o.isUnconditionalJump() &&
          !o.isIndirectJump() );
      }

  for ( auto &opcode : vector<ARMv5TE::OpCode> { ARMv6::OpCode::UXTB,
                                                 ARMv6::OpCode::UXTB16,
                                                 ARMv6::OpCode::UXTH } )
    for ( auto &mode : vector<ARM_Base::AddressingMode> { ARM_Base::AddressingMode::ror0,
                                                          ARM_Base::AddressingMode::ror8,
                                                          ARM_Base::AddressingMode::ror16,
                                                          ARM_Base::AddressingMode::ror24 } )
      for ( auto &cond : conditions ) {
        WIR_Operation o(
          opcode, ARMv6::OperationFormat::CRRA_1,
          WIR_ConditionFieldParameter( cond ),
          WIR_RegisterParameter( r8, WIR_Usage::def ),
          WIR_RegisterParameter( r3, WIR_Usage::use ),
          WIR_AddressingModeParameter( mode ) );

        ufAssert( o.getSize() == 4 );
        ufAssert(
          !o.isMemoryAccess() && !o.isMemoryStore() && !o.isMemoryLoad() &&
          !o.isMove() && !o.isCall() && !o.isIndirectCall() && !o.isReturn() &&
          !o.isJump() && !o.isConditionalJump() && !o.isUnconditionalJump() &&
          !o.isIndirectJump() );
      }

  for ( auto &opcode : vector<ARMv5TE::OpCode> { ARMv6::OpCode::UXTB,
                                                 ARMv6::OpCode::UXTH } ) {
    WIR_Operation o(
      opcode, ARMv4T::OperationFormat::TRR_1,
      WIR_RegisterParameter( r6, WIR_Usage::def ),
      WIR_RegisterParameter( r3, WIR_Usage::use ) );

    ufAssert( o.getSize() == 2 );
    ufAssert(
      !o.isMemoryAccess() && !o.isMemoryStore() && !o.isMemoryLoad() &&
      !o.isMove() && !o.isCall() && !o.isIndirectCall() && !o.isReturn() &&
      !o.isJump() && !o.isConditionalJump() && !o.isUnconditionalJump() &&
      !o.isIndirectJump() );
  }

  return( 0 );
}
