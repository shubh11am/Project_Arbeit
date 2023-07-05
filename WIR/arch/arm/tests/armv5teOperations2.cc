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
#include <arch/arm/armv5te.h>


using namespace std;
using namespace WIR;


int main( void )
{
  WIR_Init();

  ARMv5TE p;
  const ARM_LoRegP &r4 = p.R4(), &r6 = p.R6();
  const ARM_HiRegP &r8 = p.R8(), &r11 = p.R11();
  const ARMv5TE_PRegP &p6 = p.P6();

  cout.iword( WIR_Indentation() ) = 8;
  cout << arm;

  // The following operations must be accepted for the ARMv5TE ISA.
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
  const vector<ARM_Base::AddressingMode> memoryAddressingModes {
    ARM_Base::AddressingMode::pre,
    ARM_Base::AddressingMode::post };
  const vector<ARM_Base::AddressingMode> memoryAddSubModes {
    ARM_Base::AddressingMode::plus,
    ARM_Base::AddressingMode::minus };
  const vector<ARM_Base::AddressingMode> rightShiftModes {
    ARM_Base::AddressingMode::lsr,
    ARM_Base::AddressingMode::asr };

  for ( auto &cond : conditions )
    for ( auto &addsub : memoryAddSubModes ) {
      WIR_Operation o1(
        ARMv5TE::OpCode::LDRD, ARMv5TE::OperationFormat::CPRAC8_1,
        WIR_ConditionFieldParameter( cond ),
        WIR_RegisterParameter( p6, WIR_Usage::def ),
        WIR_RegisterParameter( r4, WIR_Usage::use ),
        WIR_AddressingModeParameter( addsub ),
        ARM_Const8_Unsigned( 255 ) );
      cout << o1 << endl;

      WIR_Operation o2(
        ARMv5TE::OpCode::LDRD, ARMv5TE::OperationFormat::CPRAR_1,
        WIR_ConditionFieldParameter( cond ),
        WIR_RegisterParameter( p6, WIR_Usage::def ),
        WIR_RegisterParameter( r4, WIR_Usage::use ),
        WIR_AddressingModeParameter( addsub ),
        WIR_RegisterParameter( r11, WIR_Usage::use ) );
      cout << o2 << endl;

      for ( auto &prepost : memoryAddressingModes ) {
        WIR_Operation o3(
          ARMv5TE::OpCode::LDRD, ARMv5TE::OperationFormat::CPARAC8_1,
          WIR_ConditionFieldParameter( cond ),
          WIR_RegisterParameter( p6, WIR_Usage::def ),
          WIR_AddressingModeParameter( prepost ),
          WIR_RegisterParameter( r4, WIR_Usage::defuse ),
          WIR_AddressingModeParameter( addsub ),
          ARM_Const8_Unsigned( 255 ) );
        cout << o3 << endl;

        WIR_Operation o4(
          ARMv5TE::OpCode::LDRD, ARMv5TE::OperationFormat::CPARAR_1,
          WIR_ConditionFieldParameter( cond ),
          WIR_RegisterParameter( p6, WIR_Usage::def ),
          WIR_AddressingModeParameter( prepost ),
          WIR_RegisterParameter( r4, WIR_Usage::defuse ),
          WIR_AddressingModeParameter( addsub ),
          WIR_RegisterParameter( r11, WIR_Usage::use ) );
        cout << o4 << endl;
      }
    }

  for ( auto &cond : conditions )
    for ( auto &proc : coprocessors ) {
      WIR_Operation o(
        ARMv5TE::OpCode::MCRR, ARMv5TE::OperationFormat::CAORRS_2,
        WIR_ConditionFieldParameter( cond ),
        WIR_AddressingModeParameter( proc ),
        ARM_Const3_CoprocessorOpcode( 0 ),
        WIR_RegisterParameter( r8, WIR_Usage::use ),
        WIR_RegisterParameter( r11, WIR_Usage::use ),
        WIR_StringParameter( "c9" ) );
      cout << o << endl;
    }

  for ( auto &cond : conditions )
    for ( auto &proc : coprocessors ) {
      WIR_Operation o(
        ARMv5TE::OpCode::MRRC, ARMv5TE::OperationFormat::CAORRS_1,
        WIR_ConditionFieldParameter( cond ),
        WIR_AddressingModeParameter( proc ),
        ARM_Const3_CoprocessorOpcode( 0 ),
        WIR_RegisterParameter( r8, WIR_Usage::def ),
        WIR_RegisterParameter( r11, WIR_Usage::def ),
        WIR_StringParameter( "c9" ) );
      cout << o << endl;
    }

  for ( auto &addsub : memoryAddSubModes ) {
    WIR_Operation o1(
      ARMv5TE::OpCode::PLD, ARMv5TE::OperationFormat::RAC12_1,
      WIR_RegisterParameter( r6, WIR_Usage::use ),
      WIR_AddressingModeParameter( addsub ),
      ARM_Const12_Unsigned( 4095 ) );
    cout << o1 << endl;

    WIR_Operation o2(
      ARMv5TE::OpCode::PLD, ARMv5TE::OperationFormat::RAR_1,
      WIR_RegisterParameter( r6, WIR_Usage::use ),
      WIR_AddressingModeParameter( addsub ),
      WIR_RegisterParameter( r11, WIR_Usage::use ) );
    cout << o2 << endl;

    WIR_Operation o3(
      ARMv5TE::OpCode::PLD, ARMv5TE::OperationFormat::RARC5_1,
      WIR_RegisterParameter( r6, WIR_Usage::use ),
      WIR_AddressingModeParameter( addsub ),
      WIR_RegisterParameter( r11, WIR_Usage::use ),
      ARM_Const5_Unsigned( 31 ) );
    cout << o3 << endl;

    for ( auto &rshift : rightShiftModes ) {
      WIR_Operation o(
        ARMv5TE::OpCode::PLD, ARMv5TE::OperationFormat::RARAC60_1,
        WIR_RegisterParameter( r6, WIR_Usage::use ),
        WIR_AddressingModeParameter( addsub ),
        WIR_RegisterParameter( r11, WIR_Usage::use ),
        WIR_AddressingModeParameter( rshift ),
        ARM_Const6_Unsigned0( 32 ) );
      cout << o << endl;
    }

    WIR_Operation o4(
      ARMv5TE::OpCode::PLD, ARMv5TE::OperationFormat::RARC50_1,
      WIR_RegisterParameter( r6, WIR_Usage::use ),
      WIR_AddressingModeParameter( addsub ),
      WIR_RegisterParameter( r11, WIR_Usage::use ),
      ARM_Const5_Unsigned0( 31 ) );
    cout << o4 << endl;

    WIR_Operation o5(
      ARMv5TE::OpCode::PLD, ARMv5TE::OperationFormat::RAR_2,
      WIR_RegisterParameter( r6, WIR_Usage::use ),
      WIR_AddressingModeParameter( addsub ),
      WIR_RegisterParameter( r11, WIR_Usage::use ) );
    cout << o5 << endl;
  }

  for ( auto &opcode : vector<ARMv5TE::OpCode> { ARMv5TE::OpCode::QADD,
                                                 ARMv5TE::OpCode::QDADD,
                                                 ARMv5TE::OpCode::QDSUB,
                                                 ARMv5TE::OpCode::QSUB } )
    for ( auto &cond : conditions ) {
      WIR_Operation o(
        opcode, ARMv5TE::OperationFormat::CRRR_1,
        WIR_ConditionFieldParameter( cond ),
        WIR_RegisterParameter( r8, WIR_Usage::def ),
        WIR_RegisterParameter( r11, WIR_Usage::use ),
        WIR_RegisterParameter( r4, WIR_Usage::use ) );
      cout << o << endl;
    }

  for ( auto &opcode : vector<ARMv5TE::OpCode> { ARMv5TE::OpCode::SMLABB,
                                                 ARMv5TE::OpCode::SMLABT,
                                                 ARMv5TE::OpCode::SMLATB,
                                                 ARMv5TE::OpCode::SMLATT,
                                                 ARMv5TE::OpCode::SMLALBB,
                                                 ARMv5TE::OpCode::SMLALBT,
                                                 ARMv5TE::OpCode::SMLALTB,
                                                 ARMv5TE::OpCode::SMLALTT,
                                                 ARMv5TE::OpCode::SMLAWB,
                                                 ARMv5TE::OpCode::SMLAWT } )
    for ( auto &cond : conditions ) {
      WIR_Operation o(
        opcode, ARMv5TE::OperationFormat::CRRRR_1,
        WIR_ConditionFieldParameter( cond ),
        WIR_RegisterParameter( r8, WIR_Usage::def ),
        WIR_RegisterParameter( r11, WIR_Usage::use ),
        WIR_RegisterParameter( r6, WIR_Usage::use ),
        WIR_RegisterParameter( r4, WIR_Usage::use ) );
      cout << o << endl;
    }

  for ( auto &opcode : vector<ARMv5TE::OpCode> { ARMv5TE::OpCode::SMULBB,
                                                 ARMv5TE::OpCode::SMULBT,
                                                 ARMv5TE::OpCode::SMULTB,
                                                 ARMv5TE::OpCode::SMULTT,
                                                 ARMv5TE::OpCode::SMULWB,
                                                 ARMv5TE::OpCode::SMULWT } )
    for ( auto &cond : conditions ) {
      WIR_Operation o(
        opcode, ARMv5TE::OperationFormat::CRRR_1,
        WIR_ConditionFieldParameter( cond ),
        WIR_RegisterParameter( r8, WIR_Usage::def ),
        WIR_RegisterParameter( r11, WIR_Usage::use ),
        WIR_RegisterParameter( r4, WIR_Usage::use ) );
      cout << o << endl;
    }

  for ( auto &cond : conditions )
    for ( auto &addsub : memoryAddSubModes ) {
      WIR_Operation o1(
        ARMv5TE::OpCode::STRD, ARMv5TE::OperationFormat::CPRAC8_2,
        WIR_ConditionFieldParameter( cond ),
        WIR_RegisterParameter( p6, WIR_Usage::use ),
        WIR_RegisterParameter( r4, WIR_Usage::use ),
        WIR_AddressingModeParameter( addsub ),
        ARM_Const8_Unsigned( 255 ) );
      cout << o1 << endl;

      WIR_Operation o2(
        ARMv5TE::OpCode::STRD, ARMv5TE::OperationFormat::CPRAR_2,
        WIR_ConditionFieldParameter( cond ),
        WIR_RegisterParameter( p6, WIR_Usage::use ),
        WIR_RegisterParameter( r4, WIR_Usage::use ),
        WIR_AddressingModeParameter( addsub ),
        WIR_RegisterParameter( r11, WIR_Usage::use ) );
      cout << o2 << endl;

      for ( auto &prepost : memoryAddressingModes ) {
        WIR_Operation o3(
          ARMv5TE::OpCode::STRD, ARMv5TE::OperationFormat::CPARAC8_2,
          WIR_ConditionFieldParameter( cond ),
          WIR_RegisterParameter( p6, WIR_Usage::use ),
          WIR_AddressingModeParameter( prepost ),
          WIR_RegisterParameter( r4, WIR_Usage::defuse ),
          WIR_AddressingModeParameter( addsub ),
          ARM_Const8_Unsigned( 255 ) );
        cout << o3 << endl;

        WIR_Operation o4(
          ARMv5TE::OpCode::STRD, ARMv5TE::OperationFormat::CPARAR_2,
          WIR_ConditionFieldParameter( cond ),
          WIR_RegisterParameter( p6, WIR_Usage::use ),
          WIR_AddressingModeParameter( prepost ),
          WIR_RegisterParameter( r4, WIR_Usage::defuse ),
          WIR_AddressingModeParameter( addsub ),
          WIR_RegisterParameter( r11, WIR_Usage::use ) );
        cout << o4 << endl;
      }
    }

  return( 0 );
}
