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
#include <arch/arm/armbase.h>


using namespace std;
using namespace WIR;


int main( void )
{
  WIR_Init();

  ARM_Base p;
  const ARM_LoRegP &r1 = p.R2(), &r2 = p.R6();
  const ARM_HiRegP &r3 = p.R11(), &r4 = p.R9();
  WIR_BasicBlock b;
  WIR_Function f( "main" );

  cout.iword( WIR_Indentation() ) = 8;
  cout << arm;

  // The following operations must be accepted for any ARM-based architecture.
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

  const vector<ARM_Base::OperationFormat> f11 {
    ARM_Base::OperationFormat::CRRC8RA_1,
    ARM_Base::OperationFormat::CRRC8RA_2 };
  const vector<ARM_Base::OperationFormat> f12 {
    ARM_Base::OperationFormat::CRRR_1, ARM_Base::OperationFormat::CRRR_2,
    ARM_Base::OperationFormat::CRRR_3, ARM_Base::OperationFormat::CRRR_4 };
  const vector<ARM_Base::OperationFormat> f13 {
    ARM_Base::OperationFormat::CRRRAR_1, ARM_Base::OperationFormat::CRRRAR_2 };
  const vector<ARM_Base::OperationFormat> f14 {
    ARM_Base::OperationFormat::CRRRC5_1, ARM_Base::OperationFormat::CRRRC5_2 };
  const vector<ARM_Base::OperationFormat> f15 {
    ARM_Base::OperationFormat::CRRRAC60_1,
    ARM_Base::OperationFormat::CRRRAC60_2 };
  const vector<ARM_Base::OperationFormat> f16 {
    ARM_Base::OperationFormat::CRRRC50_1,
    ARM_Base::OperationFormat::CRRRC50_2 };

  const vector<ARM_Base::AddressingMode> amode1 {
    ARM_Base::AddressingMode::lsl,
    ARM_Base::AddressingMode::lsr,
    ARM_Base::AddressingMode::asr,
    ARM_Base::AddressingMode::ror };
  const vector<ARM_Base::AddressingMode> amode2 {
    ARM_Base::AddressingMode::lsr,
    ARM_Base::AddressingMode::asr };
  const vector<ARM_Base::AddressingMode> memoryAddSubModes {
    ARM_Base::AddressingMode::plus,
    ARM_Base::AddressingMode::minus };
  const vector<ARM_Base::AddressingMode> memoryAddressingModes {
    ARM_Base::AddressingMode::pre,
    ARM_Base::AddressingMode::post };
  const vector<ARM_Base::AddressingMode> rightShiftModes {
    ARM_Base::AddressingMode::lsr,
    ARM_Base::AddressingMode::asr };
  const vector<ARM_Base::AddressingMode> multipleModes {
    ARM_Base::AddressingMode::ia,
    ARM_Base::AddressingMode::ib,
    ARM_Base::AddressingMode::da,
    ARM_Base::AddressingMode::db,
    ARM_Base::AddressingMode::fd,
    ARM_Base::AddressingMode::ed,
    ARM_Base::AddressingMode::fa,
    ARM_Base::AddressingMode::ea };
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

  // Standard data-processing operations.
  const vector<ARM_Base::OpCode> dataProcessingOps1 {
    ARM_Base::OpCode::ADC,
    ARM_Base::OpCode::ADD,
    ARM_Base::OpCode::AND,
    ARM_Base::OpCode::BIC,
    ARM_Base::OpCode::EOR,
    ARM_Base::OpCode::ORR,
    ARM_Base::OpCode::RSB,
    ARM_Base::OpCode::RSC,
    ARM_Base::OpCode::SBC,
    ARM_Base::OpCode::SUB };

  for ( auto &opcode : dataProcessingOps1 )
    for ( auto &cond : conditions ) {
      for ( auto &format : f11 ) {
        WIR_Operation o(
          opcode, format, WIR_ConditionFieldParameter( cond ),
          new WIR_RegisterParameter( r1, WIR_Usage::def ),
          new WIR_RegisterParameter( r2, WIR_Usage::use ),
          new ARM_Const8_Unsigned( 42 ),
          new ARM_Const5_RotateAmount( 6 ) );
        cout << o << endl;
      }

      for ( auto &format : f12 ) {
        WIR_Operation o(
          opcode, format, WIR_ConditionFieldParameter( cond ),
          new WIR_RegisterParameter( r1, WIR_Usage::def ),
          new WIR_RegisterParameter( r2, WIR_Usage::use ),
          new WIR_RegisterParameter( r3, WIR_Usage::use ) );
        cout << o << endl;
      }

      for ( auto &format : f13 )
        for ( auto &amode : amode1 ) {
          WIR_Operation o(
            opcode, format, WIR_ConditionFieldParameter( cond ),
            new WIR_RegisterParameter( r1, WIR_Usage::def ),
            new WIR_RegisterParameter( r2, WIR_Usage::use ),
            new WIR_RegisterParameter( r3, WIR_Usage::use ),
            new WIR_AddressingModeParameter( amode ),
            new WIR_RegisterParameter( r4, WIR_Usage::use ) );
          cout << o << endl;
        }

      for ( auto &format : f14 ) {
        WIR_Operation o(
          opcode, format, WIR_ConditionFieldParameter( cond ),
          new WIR_RegisterParameter( r1, WIR_Usage::def ),
          new WIR_RegisterParameter( r2, WIR_Usage::use ),
          new WIR_RegisterParameter( r3, WIR_Usage::use ),
          new ARM_Const5_Unsigned( 0 ) );
        cout << o << endl;
      }

      for ( auto &format : f15 )
        for ( auto &amode : amode2 ) {
          WIR_Operation o(
            opcode, format, WIR_ConditionFieldParameter( cond ),
            new WIR_RegisterParameter( r1, WIR_Usage::def ),
            new WIR_RegisterParameter( r2, WIR_Usage::use ),
            new WIR_RegisterParameter( r3, WIR_Usage::use ),
            new WIR_AddressingModeParameter( amode ),
            new ARM_Const6_Unsigned0( 1 ) );
          cout << o << endl;
        }

      for ( auto &format : f16 ) {
        WIR_Operation o(
          opcode, format, WIR_ConditionFieldParameter( cond ),
          new WIR_RegisterParameter( r1, WIR_Usage::def ),
          new WIR_RegisterParameter( r2, WIR_Usage::use ),
          new WIR_RegisterParameter( r3, WIR_Usage::use ),
          new ARM_Const5_Unsigned0( 1 ) );
        cout << o << endl;
      }
    }

  const vector<ARM_Base::OperationFormat> f21 {
    ARM_Base::OperationFormat::CRC8RA_1, ARM_Base::OperationFormat::CRC8RA_2 };
  const vector<ARM_Base::OperationFormat> f22 {
    ARM_Base::OperationFormat::CRR_1, ARM_Base::OperationFormat::CRR_2,
    ARM_Base::OperationFormat::CRR_3, ARM_Base::OperationFormat::CRR_4 };
  const vector<ARM_Base::OperationFormat> f23 {
    ARM_Base::OperationFormat::CRRAR_1, ARM_Base::OperationFormat::CRRAR_2 };
  const vector<ARM_Base::OperationFormat> f24 {
    ARM_Base::OperationFormat::CRRC5_1, ARM_Base::OperationFormat::CRRC5_2 };
  const vector<ARM_Base::OperationFormat> f25 {
    ARM_Base::OperationFormat::CRRAC60_1,
    ARM_Base::OperationFormat::CRRAC60_2 };
  const vector<ARM_Base::OperationFormat> f26 {
    ARM_Base::OperationFormat::CRRC50_1, ARM_Base::OperationFormat::CRRC50_2 };

  const vector<ARM_Base::OpCode> dataProcessingOps2 {
    ARM_Base::OpCode::MOV,
    ARM_Base::OpCode::MVN };

  for ( auto &opcode : dataProcessingOps2 )
    for ( auto &cond : conditions ) {
      for ( auto &format : f21 ) {
        WIR_Operation o(
          opcode, format, WIR_ConditionFieldParameter( cond ),
          new WIR_RegisterParameter( r1, WIR_Usage::def ),
          new ARM_Const8_Unsigned( 42 ),
          new ARM_Const5_RotateAmount( 6 ) );
        cout << o << endl;
      }

      for ( auto &format : f22 ) {
        WIR_Operation o(
          opcode, format, WIR_ConditionFieldParameter( cond ),
          new WIR_RegisterParameter( r1, WIR_Usage::def ),
          new WIR_RegisterParameter( r3, WIR_Usage::use ) );
        cout << o << endl;
      }

      for ( auto &format : f23 )
        for ( auto &amode : amode1 ) {
          WIR_Operation o(
            opcode, format, WIR_ConditionFieldParameter( cond ),
            new WIR_RegisterParameter( r1, WIR_Usage::def ),
            new WIR_RegisterParameter( r3, WIR_Usage::use ),
            new WIR_AddressingModeParameter( amode ),
            new WIR_RegisterParameter( r4, WIR_Usage::use ) );
          cout << o << endl;
        }

      for ( auto &format : f24 ) {
        WIR_Operation o(
          opcode, format, WIR_ConditionFieldParameter( cond ),
          new WIR_RegisterParameter( r1, WIR_Usage::def ),
          new WIR_RegisterParameter( r3, WIR_Usage::use ),
          new ARM_Const5_Unsigned( 0 ) );
        cout << o << endl;
      }

      for ( auto &format : f25 )
        for ( auto &amode : amode2 ) {
          WIR_Operation o(
            opcode, format, WIR_ConditionFieldParameter( cond ),
            new WIR_RegisterParameter( r1, WIR_Usage::def ),
            new WIR_RegisterParameter( r3, WIR_Usage::use ),
            new WIR_AddressingModeParameter( amode ),
            ARM_Const6_Unsigned0( 1 ) );
          cout << o << endl;
        }

      for ( auto &format : f26 ) {
        WIR_Operation o(
          opcode, format, WIR_ConditionFieldParameter( cond ),
          new WIR_RegisterParameter( r1, WIR_Usage::def ),
          new WIR_RegisterParameter( r3, WIR_Usage::use ),
          new ARM_Const5_Unsigned0( 1 ) );
        cout << o << endl;
      }
    }

  const vector<ARM_Base::OperationFormat> f32 {
    ARM_Base::OperationFormat::CRR_5, ARM_Base::OperationFormat::CRR_6 };

  const vector<ARM_Base::OpCode> dataProcessingOps3 {
    ARM_Base::OpCode::CMN,
    ARM_Base::OpCode::CMP,
    ARM_Base::OpCode::TEQ,
    ARM_Base::OpCode::TST };

  for ( auto &opcode : dataProcessingOps3 )
    for ( auto &cond : conditions ) {
      WIR_Operation o1(
        opcode, ARM_Base::OperationFormat::CRC8RA_3,
        new WIR_ConditionFieldParameter( cond ),
        new WIR_RegisterParameter( r2, WIR_Usage::use ),
        new ARM_Const8_Unsigned( 42 ),
        new ARM_Const5_RotateAmount( 6 ) );
      cout << o1 << endl;

      for ( auto &format : f32 ) {
        WIR_Operation o(
          opcode, format, WIR_ConditionFieldParameter( cond ),
          new WIR_RegisterParameter( r2, WIR_Usage::use ),
          new WIR_RegisterParameter( r3, WIR_Usage::use ) );
        cout << o << endl;
      }

      for ( auto amode : amode1 ) {
        WIR_Operation o(
          opcode, ARM_Base::OperationFormat::CRRAR_5,
          new WIR_ConditionFieldParameter( cond ),
          new WIR_RegisterParameter( r2, WIR_Usage::use ),
          new WIR_RegisterParameter( r3, WIR_Usage::use ),
          new WIR_AddressingModeParameter( amode ),
          new WIR_RegisterParameter( r4, WIR_Usage::use ) );
        cout << o << endl;
      }

      WIR_Operation o2(
        opcode, ARM_Base::OperationFormat::CRRC5_3,
        new WIR_ConditionFieldParameter( cond ),
        new WIR_RegisterParameter( r2, WIR_Usage::use ),
        new WIR_RegisterParameter( r3, WIR_Usage::use ),
        new ARM_Const5_Unsigned( 0 ) );
      cout << o2 << endl;

      for ( auto amode : amode2 ) {
        WIR_Operation o(
          opcode, ARM_Base::OperationFormat::CRRAC60_3,
          new WIR_ConditionFieldParameter( cond ),
          new WIR_RegisterParameter( r2, WIR_Usage::use ),
          new WIR_RegisterParameter( r3, WIR_Usage::use ),
          new WIR_AddressingModeParameter( amode ),
          new ARM_Const6_Unsigned0( 1 ) );
        cout << o << endl;
      }

      WIR_Operation o3(
        opcode, ARM_Base::OperationFormat::CRRC50_3,
        new WIR_ConditionFieldParameter( cond ),
        new WIR_RegisterParameter( r2, WIR_Usage::use ),
        new WIR_RegisterParameter( r3, WIR_Usage::use ),
        new ARM_Const5_Unsigned0( 1 ) );
      cout << o3 << endl;
    }

  const vector<ARM_Base::OperationFormat> f41 {
    ARM_Base::OperationFormat::CRRRR_1, ARM_Base::OperationFormat::CRRRR_2 };

  for ( auto &cond : conditions ) {
    for ( auto &format : f41 ) {
      WIR_Operation o(
        ARM_Base::OpCode::MLA, format, WIR_ConditionFieldParameter( cond ),
        new WIR_RegisterParameter( r1, WIR_Usage::def ),
        new WIR_RegisterParameter( r2, WIR_Usage::use ),
        new WIR_RegisterParameter( r3, WIR_Usage::use ),
        new WIR_RegisterParameter( r4, WIR_Usage::use ) );
      cout << o << endl;
    }
  }

  for ( auto &cond : conditions ) {
    WIR_Operation o1(
      ARM_Base::OpCode::MUL, ARM_Base::OperationFormat::CRRR_1,
      new WIR_ConditionFieldParameter( cond ),
      new WIR_RegisterParameter( r1, WIR_Usage::def ),
      new WIR_RegisterParameter( r2, WIR_Usage::use ),
      new WIR_RegisterParameter( r4, WIR_Usage::use ) );
    cout << o1 << endl;

    WIR_Operation o2(
      ARM_Base::OpCode::MUL, ARM_Base::OperationFormat::CRRR_2,
      new WIR_ConditionFieldParameter( cond ),
      new WIR_RegisterParameter( r1, WIR_Usage::def ),
      new WIR_RegisterParameter( r2, WIR_Usage::use ),
      new WIR_RegisterParameter( r4, WIR_Usage::use ) );
    cout << o2 << endl;
  }

  const vector<ARM_Base::OperationFormat> f51 {
    ARM_Base::OperationFormat::CRRRR_3, ARM_Base::OperationFormat::CRRRR_4 };

  const vector<ARM_Base::OpCode> dataProcessingOps5 {
    ARM_Base::OpCode::SMLAL,
    ARM_Base::OpCode::SMULL,
    ARM_Base::OpCode::UMLAL,
    ARM_Base::OpCode::UMULL };

  for ( auto &opcode : dataProcessingOps5 )
    for ( auto &cond : conditions ) {
      for ( auto &format : f51 ) {
        WIR_Operation o(
          opcode, format, WIR_ConditionFieldParameter( cond ),
          new WIR_RegisterParameter( r1, WIR_Usage::defuse ),
          new WIR_RegisterParameter( r2, WIR_Usage::defuse ),
          new WIR_RegisterParameter( r3, WIR_Usage::use ),
          new WIR_RegisterParameter( r4, WIR_Usage::use ) );
        cout << o << endl;
      }
    }

  // Load operations
  const vector<ARM_Base::OpCode> ldOps1 {
    ARM_Base::OpCode::LDR,
    ARM_Base::OpCode::LDRB };

  for ( auto &opcode : ldOps1 )
    for ( auto &cond : conditions )
      for ( auto &addsub : memoryAddSubModes ) {
        for ( auto &prepost : memoryAddressingModes ) {
          WIR_Operation o1(
            opcode, ARM_Base::OperationFormat::CRARAC12_1,
            new WIR_ConditionFieldParameter( cond ),
            new WIR_RegisterParameter( r1, WIR_Usage::def ),
            new WIR_AddressingModeParameter( prepost ),
            new WIR_RegisterParameter( r2, WIR_Usage::defuse ),
            new WIR_AddressingModeParameter( addsub ),
            new ARM_Const12_Unsigned( 4095 ) );
          cout << o1 << endl;

          WIR_Operation o2(
            opcode, ARM_Base::OperationFormat::CRARAR_1,
            new WIR_ConditionFieldParameter( cond ),
            new WIR_RegisterParameter( r1, WIR_Usage::def ),
            new WIR_AddressingModeParameter( prepost ),
            new WIR_RegisterParameter( r2, WIR_Usage::defuse ),
            new WIR_AddressingModeParameter( addsub ),
            new WIR_RegisterParameter( r3, WIR_Usage::use ) );
          cout << o2 << endl;
        }

        WIR_Operation o1(
          opcode, ARM_Base::OperationFormat::CRRAC12_1,
          new WIR_ConditionFieldParameter( cond ),
          new WIR_RegisterParameter( r1, WIR_Usage::def ),
          new WIR_RegisterParameter( r2, WIR_Usage::use ),
          new WIR_AddressingModeParameter( addsub ),
          new ARM_Const12_Unsigned( 4095 ) );
        cout << o1 << endl;

        WIR_Operation o2(
          opcode, ARM_Base::OperationFormat::CRRAR_3,
          new WIR_ConditionFieldParameter( cond ),
          new WIR_RegisterParameter( r1, WIR_Usage::def ),
          new WIR_RegisterParameter( r2, WIR_Usage::use ),
          new WIR_AddressingModeParameter( addsub ),
          new WIR_RegisterParameter( r3, WIR_Usage::use ) );
        cout << o2 << endl;

        WIR_Operation o3(
          opcode, ARM_Base::OperationFormat::CRRARC5_1,
          new WIR_ConditionFieldParameter( cond ),
          new WIR_RegisterParameter( r1, WIR_Usage::def ),
          new WIR_RegisterParameter( r2, WIR_Usage::use ),
          new WIR_AddressingModeParameter( addsub ),
          new WIR_RegisterParameter( r3, WIR_Usage::use ),
          new ARM_Const5_Unsigned( 31 ) );
        cout << o3 << endl;

        for ( auto &rshift : rightShiftModes ) {
          WIR_Operation o4(
            opcode, ARM_Base::OperationFormat::CRRARAC60_1,
            new WIR_ConditionFieldParameter( cond ),
            new WIR_RegisterParameter( r1, WIR_Usage::def ),
            new WIR_RegisterParameter( r2, WIR_Usage::use ),
            new WIR_AddressingModeParameter( addsub ),
            new WIR_RegisterParameter( r3, WIR_Usage::use ),
            new WIR_AddressingModeParameter( rshift ),
            new ARM_Const6_Unsigned0( 32 ) );
          cout << o4 << endl;
        }

        WIR_Operation o4(
          opcode, ARM_Base::OperationFormat::CRRARC50_1,
          new WIR_ConditionFieldParameter( cond ),
          new WIR_RegisterParameter( r1, WIR_Usage::def ),
          new WIR_RegisterParameter( r2, WIR_Usage::use ),
          new WIR_AddressingModeParameter( addsub ),
          new WIR_RegisterParameter( r3, WIR_Usage::use ),
          new ARM_Const5_Unsigned0( 31 ) );
        cout << o4 << endl;

        WIR_Operation o5(
          opcode, ARM_Base::OperationFormat::CRRAR_4,
          new WIR_ConditionFieldParameter( cond ),
          new WIR_RegisterParameter( r1, WIR_Usage::def ),
          new WIR_RegisterParameter( r2, WIR_Usage::use ),
          new WIR_AddressingModeParameter( addsub ),
          new WIR_RegisterParameter( r3, WIR_Usage::use ) );
        cout << o5 << endl;

        for ( auto &prepost : memoryAddressingModes ) {
          WIR_Operation o6(
            opcode, ARM_Base::OperationFormat::CRARARC5_1,
            new WIR_ConditionFieldParameter( cond ),
            new WIR_RegisterParameter( r1, WIR_Usage::def ),
            new WIR_AddressingModeParameter( prepost ),
            new WIR_RegisterParameter( r2, WIR_Usage::defuse ),
            new WIR_AddressingModeParameter( addsub ),
            new WIR_RegisterParameter( r3, WIR_Usage::use ),
            new ARM_Const5_Unsigned( 31 ) );
          cout << o6 << endl;

          for ( auto &rshift : rightShiftModes ) {
            WIR_Operation o7(
              opcode, ARM_Base::OperationFormat::CRARARAC60_1,
              new WIR_ConditionFieldParameter( cond ),
              new WIR_RegisterParameter( r1, WIR_Usage::def ),
              new WIR_AddressingModeParameter( prepost ),
              new WIR_RegisterParameter( r2, WIR_Usage::defuse ),
              new WIR_AddressingModeParameter( addsub ),
              new WIR_RegisterParameter( r3, WIR_Usage::use ),
              new WIR_AddressingModeParameter( rshift ),
              new ARM_Const6_Unsigned0( 32 ) );
            cout << o7 << endl;
          }

          WIR_Operation o8(
            opcode, ARM_Base::OperationFormat::CRARARC50_1,
            new WIR_ConditionFieldParameter( cond ),
            new WIR_RegisterParameter( r1, WIR_Usage::def ),
            new WIR_AddressingModeParameter( prepost ),
            new WIR_RegisterParameter( r2, WIR_Usage::defuse ),
            new WIR_AddressingModeParameter( addsub ),
            new WIR_RegisterParameter( r3, WIR_Usage::use ),
            new ARM_Const5_Unsigned0( 31 ) );
          cout << o8 << endl;

          WIR_Operation o9(
            opcode, ARM_Base::OperationFormat::CRARAR_2,
            new WIR_ConditionFieldParameter( cond ),
            new WIR_RegisterParameter( r1, WIR_Usage::def ),
            new WIR_AddressingModeParameter( prepost ),
            new WIR_RegisterParameter( r2, WIR_Usage::defuse ),
            new WIR_AddressingModeParameter( addsub ),
            new WIR_RegisterParameter( r3, WIR_Usage::use ) );
          cout << o9 << endl;
        }
      }

  const vector<ARM_Base::OpCode> ldOps2 {
    ARM_Base::OpCode::LDRBT,
    ARM_Base::OpCode::LDRT };

  for ( auto &opcode : ldOps2 )
    for ( auto &cond : conditions )
      for ( auto &addsub : memoryAddSubModes ) {
        WIR_Operation o1(
          opcode, ARM_Base::OperationFormat::CRARAC12_1,
          new WIR_ConditionFieldParameter( cond ),
          new WIR_RegisterParameter( r1, WIR_Usage::def ),
          new WIR_AddressingModeParameter( ARM_Base::AddressingMode::post ),
          new WIR_RegisterParameter( r2, WIR_Usage::defuse ),
          new WIR_AddressingModeParameter( addsub ),
          new ARM_Const12_Unsigned( 4095 ) );
        cout << o1 << endl;

        WIR_Operation o2(
          opcode, ARM_Base::OperationFormat::CRARAR_1,
          new WIR_ConditionFieldParameter( cond ),
          new WIR_RegisterParameter( r1, WIR_Usage::def ),
          new WIR_AddressingModeParameter( ARM_Base::AddressingMode::post ),
          new WIR_RegisterParameter( r2, WIR_Usage::defuse ),
          new WIR_AddressingModeParameter( addsub ),
          new WIR_RegisterParameter( r3, WIR_Usage::use ) );
        cout << o2 << endl;

        WIR_Operation o6(
          opcode, ARM_Base::OperationFormat::CRARARC5_1,
          new WIR_ConditionFieldParameter( cond ),
          new WIR_RegisterParameter( r1, WIR_Usage::def ),
          new WIR_AddressingModeParameter( ARM_Base::AddressingMode::post ),
          new WIR_RegisterParameter( r2, WIR_Usage::defuse ),
          new WIR_AddressingModeParameter( addsub ),
          new WIR_RegisterParameter( r3, WIR_Usage::use ),
          new ARM_Const5_Unsigned( 31 ) );
        cout << o6 << endl;

        for ( auto rshift : rightShiftModes ) {
          WIR_Operation o7(
            opcode, ARM_Base::OperationFormat::CRARARAC60_1,
            new WIR_ConditionFieldParameter( cond ),
            new WIR_RegisterParameter( r1, WIR_Usage::def ),
            new WIR_AddressingModeParameter( ARM_Base::AddressingMode::post ),
            new WIR_RegisterParameter( r2, WIR_Usage::defuse ),
            new WIR_AddressingModeParameter( addsub ),
            new WIR_RegisterParameter( r3, WIR_Usage::use ),
            new WIR_AddressingModeParameter( rshift ),
            new ARM_Const6_Unsigned0( 32 ) );
          cout << o7 << endl;
        }

        WIR_Operation o8(
          opcode, ARM_Base::OperationFormat::CRARARC50_1,
          new WIR_ConditionFieldParameter( cond ),
          new WIR_RegisterParameter( r1, WIR_Usage::def ),
          new WIR_AddressingModeParameter( ARM_Base::AddressingMode::post ),
          new WIR_RegisterParameter( r2, WIR_Usage::defuse ),
          new WIR_AddressingModeParameter( addsub ),
          new WIR_RegisterParameter( r3, WIR_Usage::use ),
          new ARM_Const5_Unsigned0( 31 ) );
        cout << o8 << endl;

        WIR_Operation o9(
          opcode, ARM_Base::OperationFormat::CRARAR_2,
          new WIR_ConditionFieldParameter( cond ),
          new WIR_RegisterParameter( r1, WIR_Usage::def ),
          new WIR_AddressingModeParameter( ARM_Base::AddressingMode::post ),
          new WIR_RegisterParameter( r2, WIR_Usage::defuse ),
          new WIR_AddressingModeParameter( addsub ),
          new WIR_RegisterParameter( r3, WIR_Usage::use ) );
        cout << o9 << endl;
      }

  for ( auto &opcode : vector<ARM_Base::OpCode> { ARM_Base::OpCode::LDRH,
                                                  ARM_Base::OpCode::LDRSB,
                                                  ARM_Base::OpCode::LDRSH } )
    for ( auto &cond : conditions )
      for ( auto &addsub : memoryAddSubModes ) {
        WIR_Operation o1(
          opcode, ARM_Base::OperationFormat::CRRAC8_1,
          new WIR_ConditionFieldParameter( cond ),
          new WIR_RegisterParameter( r1, WIR_Usage::def ),
          new WIR_RegisterParameter( r2, WIR_Usage::use ),
          new WIR_AddressingModeParameter( addsub ),
          new ARM_Const8_Unsigned( 255 ) );
        cout << o1 << endl;

        WIR_Operation o2(
          opcode, ARM_Base::OperationFormat::CRRAR_3,
          new WIR_ConditionFieldParameter( cond ),
          new WIR_RegisterParameter( r1, WIR_Usage::def ),
          new WIR_RegisterParameter( r2, WIR_Usage::use ),
          new WIR_AddressingModeParameter( addsub ),
          new WIR_RegisterParameter( r3, WIR_Usage::use ) );
        cout << o2 << endl;

        for ( auto &prepost : memoryAddressingModes ) {
          WIR_Operation o3(
            opcode, ARM_Base::OperationFormat::CRARAC8_1,
            new WIR_ConditionFieldParameter( cond ),
            new WIR_RegisterParameter( r1, WIR_Usage::def ),
            new WIR_AddressingModeParameter( prepost ),
            new WIR_RegisterParameter( r2, WIR_Usage::defuse ),
            new WIR_AddressingModeParameter( addsub ),
            new ARM_Const8_Unsigned( 255 ) );
          cout << o3 << endl;

          WIR_Operation o4(
            opcode, ARM_Base::OperationFormat::CRARAR_1,
            new WIR_ConditionFieldParameter( cond ),
            new WIR_RegisterParameter( r1, WIR_Usage::def ),
            new WIR_AddressingModeParameter( prepost ),
            new WIR_RegisterParameter( r2, WIR_Usage::defuse ),
            new WIR_AddressingModeParameter( addsub ),
            new WIR_RegisterParameter( r3, WIR_Usage::use ) );
          cout << o4 << endl;
        }
      }

  // Store operations
  const vector<ARM_Base::OpCode> stOps1 {
    ARM_Base::OpCode::STR,
    ARM_Base::OpCode::STRB };

  for ( auto &opcode : stOps1 )
    for ( auto &cond : conditions )
      for ( auto &addsub : memoryAddSubModes ) {
        for ( auto &prepost : memoryAddressingModes ) {
          WIR_Operation o1(
            opcode, ARM_Base::OperationFormat::CRARAC12_2,
            new WIR_ConditionFieldParameter( cond ),
            new WIR_RegisterParameter( r1, WIR_Usage::use ),
            new WIR_AddressingModeParameter( prepost ),
            new WIR_RegisterParameter( r2, WIR_Usage::defuse ),
            new WIR_AddressingModeParameter( addsub ),
            new ARM_Const12_Unsigned( 4095 ) );
          cout << o1 << endl;

          WIR_Operation o2(
            opcode, ARM_Base::OperationFormat::CRARAR_3,
            new WIR_ConditionFieldParameter( cond ),
            new WIR_RegisterParameter( r1, WIR_Usage::use ),
            new WIR_AddressingModeParameter( prepost ),
            new WIR_RegisterParameter( r2, WIR_Usage::defuse ),
            new WIR_AddressingModeParameter( addsub ),
            new WIR_RegisterParameter( r3, WIR_Usage::use ) );
          cout << o2 << endl;
        }

        WIR_Operation o1(
          opcode, ARM_Base::OperationFormat::CRRAC12_2,
          new WIR_ConditionFieldParameter( cond ),
          new WIR_RegisterParameter( r1, WIR_Usage::use ),
          new WIR_RegisterParameter( r2, WIR_Usage::use ),
          new WIR_AddressingModeParameter( addsub ),
          new ARM_Const12_Unsigned( 4095 ) );
        cout << o1 << endl;

        WIR_Operation o2(
          opcode, ARM_Base::OperationFormat::CRRAR_6,
          new WIR_ConditionFieldParameter( cond ),
          new WIR_RegisterParameter( r1, WIR_Usage::use ),
          new WIR_RegisterParameter( r2, WIR_Usage::use ),
          new WIR_AddressingModeParameter( addsub ),
          new WIR_RegisterParameter( r3, WIR_Usage::use ) );
        cout << o2 << endl;

        WIR_Operation o3(
          opcode, ARM_Base::OperationFormat::CRRARC5_2,
          new WIR_ConditionFieldParameter( cond ),
          new WIR_RegisterParameter( r1, WIR_Usage::use ),
          new WIR_RegisterParameter( r2, WIR_Usage::use ),
          new WIR_AddressingModeParameter( addsub ),
          new WIR_RegisterParameter( r3, WIR_Usage::use ),
          new ARM_Const5_Unsigned( 31 ) );
        cout << o3 << endl;

        for ( auto &rshift : rightShiftModes ) {
          WIR_Operation o4(
            opcode, ARM_Base::OperationFormat::CRRARAC60_2,
            new WIR_ConditionFieldParameter( cond ),
            new WIR_RegisterParameter( r1, WIR_Usage::use ),
            new WIR_RegisterParameter( r2, WIR_Usage::use ),
            new WIR_AddressingModeParameter( addsub ),
            new WIR_RegisterParameter( r3, WIR_Usage::use ),
            new WIR_AddressingModeParameter( rshift ),
            new ARM_Const6_Unsigned0( 32 ) );
          cout << o4 << endl;
        }

        WIR_Operation o4(
          opcode, ARM_Base::OperationFormat::CRRARC50_2,
          new WIR_ConditionFieldParameter( cond ),
          new WIR_RegisterParameter( r1, WIR_Usage::use ),
          new WIR_RegisterParameter( r2, WIR_Usage::use ),
          new WIR_AddressingModeParameter( addsub ),
          new WIR_RegisterParameter( r3, WIR_Usage::use ),
          new ARM_Const5_Unsigned0( 31 ) );
        cout << o4 << endl;

        WIR_Operation o5(
          opcode, ARM_Base::OperationFormat::CRRAR_7,
          new WIR_ConditionFieldParameter( cond ),
          new WIR_RegisterParameter( r1, WIR_Usage::use ),
          new WIR_RegisterParameter( r2, WIR_Usage::use ),
          new WIR_AddressingModeParameter( addsub ),
          new WIR_RegisterParameter( r3, WIR_Usage::use ) );
        cout << o5 << endl;

        for ( auto &prepost : memoryAddressingModes ) {
          WIR_Operation o6(
            opcode, ARM_Base::OperationFormat::CRARARC5_2,
            new WIR_ConditionFieldParameter( cond ),
            new WIR_RegisterParameter( r1, WIR_Usage::use ),
            new WIR_AddressingModeParameter( prepost ),
            new WIR_RegisterParameter( r2, WIR_Usage::defuse ),
            new WIR_AddressingModeParameter( addsub ),
            new WIR_RegisterParameter( r3, WIR_Usage::use ),
            new ARM_Const5_Unsigned( 31 ) );
          cout << o6 << endl;

          for ( auto &rshift : rightShiftModes ) {
            WIR_Operation o7(
              opcode, ARM_Base::OperationFormat::CRARARAC60_2,
              new WIR_ConditionFieldParameter( cond ),
              new WIR_RegisterParameter( r1, WIR_Usage::use ),
              new WIR_AddressingModeParameter( prepost ),
              new WIR_RegisterParameter( r2, WIR_Usage::defuse ),
              new WIR_AddressingModeParameter( addsub ),
              new WIR_RegisterParameter( r3, WIR_Usage::use ),
              new WIR_AddressingModeParameter( rshift ),
              new ARM_Const6_Unsigned0( 32 ) );
            cout << o7 << endl;
          }

          WIR_Operation o8(
            opcode, ARM_Base::OperationFormat::CRARARC50_2,
            new WIR_ConditionFieldParameter( cond ),
            new WIR_RegisterParameter( r1, WIR_Usage::use ),
            new WIR_AddressingModeParameter( prepost ),
            new WIR_RegisterParameter( r2, WIR_Usage::defuse ),
            new WIR_AddressingModeParameter( addsub ),
            new WIR_RegisterParameter( r3, WIR_Usage::use ),
            new ARM_Const5_Unsigned0( 31 ) );
          cout << o8 << endl;

          WIR_Operation o9(
            opcode, ARM_Base::OperationFormat::CRARAR_4,
            new WIR_ConditionFieldParameter( cond ),
            new WIR_RegisterParameter( r1, WIR_Usage::use ),
            new WIR_AddressingModeParameter( prepost ),
            new WIR_RegisterParameter( r2, WIR_Usage::defuse ),
            new WIR_AddressingModeParameter( addsub ),
            new WIR_RegisterParameter( r3, WIR_Usage::use ) );
          cout << o9 << endl;
        }
      }

  const vector<ARM_Base::OpCode> stOps2 {
    ARM_Base::OpCode::STRBT,
    ARM_Base::OpCode::STRT };

  for ( auto &opcode : stOps2 )
    for ( auto &cond : conditions )
      for ( auto &addsub : memoryAddSubModes ) {
        WIR_Operation o1(
          opcode, ARM_Base::OperationFormat::CRARAC12_2,
          new WIR_ConditionFieldParameter( cond ),
          new WIR_RegisterParameter( r1, WIR_Usage::use ),
          new WIR_AddressingModeParameter( ARM_Base::AddressingMode::post ),
          new WIR_RegisterParameter( r2, WIR_Usage::defuse ),
          new WIR_AddressingModeParameter( addsub ),
          new ARM_Const12_Unsigned( 4095 ) );
        cout << o1 << endl;

        WIR_Operation o2(
          opcode, ARM_Base::OperationFormat::CRARAR_3,
          new WIR_ConditionFieldParameter( cond ),
          new WIR_RegisterParameter( r1, WIR_Usage::use ),
          new WIR_AddressingModeParameter( ARM_Base::AddressingMode::post ),
          new WIR_RegisterParameter( r2, WIR_Usage::defuse ),
          new WIR_AddressingModeParameter( addsub ),
          new WIR_RegisterParameter( r3, WIR_Usage::use ) );
        cout << o2 << endl;

        WIR_Operation o6(
          opcode, ARM_Base::OperationFormat::CRARARC5_2,
          new WIR_ConditionFieldParameter( cond ),
          new WIR_RegisterParameter( r1, WIR_Usage::use ),
          new WIR_AddressingModeParameter( ARM_Base::AddressingMode::post ),
          new WIR_RegisterParameter( r2, WIR_Usage::defuse ),
          new WIR_AddressingModeParameter( addsub ),
          new WIR_RegisterParameter( r3, WIR_Usage::use ),
          new ARM_Const5_Unsigned( 31 ) );
        cout << o6 << endl;

        for ( auto &rshift : rightShiftModes ) {
          WIR_Operation o7(
            opcode, ARM_Base::OperationFormat::CRARARAC60_2,
            new WIR_ConditionFieldParameter( cond ),
            new WIR_RegisterParameter( r1, WIR_Usage::use ),
            new WIR_AddressingModeParameter( ARM_Base::AddressingMode::post ),
            new WIR_RegisterParameter( r2, WIR_Usage::defuse ),
            new WIR_AddressingModeParameter( addsub ),
            new WIR_RegisterParameter( r3, WIR_Usage::use ),
            new WIR_AddressingModeParameter( rshift ),
            new ARM_Const6_Unsigned0( 32 ) );
          cout << o7 << endl;
        }

        WIR_Operation o8(
          opcode, ARM_Base::OperationFormat::CRARARC50_2,
          new WIR_ConditionFieldParameter( cond ),
          new WIR_RegisterParameter( r1, WIR_Usage::use ),
          new WIR_AddressingModeParameter( ARM_Base::AddressingMode::post ),
          new WIR_RegisterParameter( r2, WIR_Usage::defuse ),
          new WIR_AddressingModeParameter( addsub ),
          new WIR_RegisterParameter( r3, WIR_Usage::use ),
          new ARM_Const5_Unsigned0( 31 ) );
        cout << o8 << endl;

        WIR_Operation o9(
          opcode, ARM_Base::OperationFormat::CRARAR_4,
          new WIR_ConditionFieldParameter( cond ),
          new WIR_RegisterParameter( r1, WIR_Usage::use ),
          new WIR_AddressingModeParameter( ARM_Base::AddressingMode::post ),
          new WIR_RegisterParameter( r2, WIR_Usage::defuse ),
          new WIR_AddressingModeParameter( addsub ),
          new WIR_RegisterParameter( r3, WIR_Usage::use ) );
        cout << o9 << endl;
      }

  for ( auto &cond : conditions )
    for ( auto &addsub : memoryAddSubModes ) {
      WIR_Operation o1(
        ARM_Base::OpCode::STRH, ARM_Base::OperationFormat::CRRAC8_2,
        new WIR_ConditionFieldParameter( cond ),
        new WIR_RegisterParameter( r1, WIR_Usage::use ),
        new WIR_RegisterParameter( r2, WIR_Usage::use ),
        new WIR_AddressingModeParameter( addsub ),
        new ARM_Const8_Unsigned( 255 ) );
      cout << o1 << endl;

      WIR_Operation o2(
        ARM_Base::OpCode::STRH, ARM_Base::OperationFormat::CRRAR_6,
        new WIR_ConditionFieldParameter( cond ),
        new WIR_RegisterParameter( r1, WIR_Usage::use ),
        new WIR_RegisterParameter( r2, WIR_Usage::use ),
        new WIR_AddressingModeParameter( addsub ),
        new WIR_RegisterParameter( r3, WIR_Usage::use ) );
      cout << o2 << endl;

      for ( auto &prepost : memoryAddressingModes ) {
        WIR_Operation o3(
          ARM_Base::OpCode::STRH, ARM_Base::OperationFormat::CRARAC8_2,
          new WIR_ConditionFieldParameter( cond ),
          new WIR_RegisterParameter( r1, WIR_Usage::use ),
          new WIR_AddressingModeParameter( prepost ),
          new WIR_RegisterParameter( r2, WIR_Usage::defuse ),
          new WIR_AddressingModeParameter( addsub ),
          new ARM_Const8_Unsigned( 255 ) );
        cout << o3 << endl;

        WIR_Operation o4(
          ARM_Base::OpCode::STRH, ARM_Base::OperationFormat::CRARAR_3,
          new WIR_ConditionFieldParameter( cond ),
          new WIR_RegisterParameter( r1, WIR_Usage::use ),
          new WIR_AddressingModeParameter( prepost ),
          new WIR_RegisterParameter( r2, WIR_Usage::defuse ),
          new WIR_AddressingModeParameter( addsub ),
          new WIR_RegisterParameter( r3, WIR_Usage::use ) );
        cout << o4 << endl;
      }
    }

  // Branch operations.
  for ( auto &cond : conditions ) {
    WIR_Operation o1(
      ARM_Base::OpCode::B, ARM_Base::OperationFormat::CL,
      new WIR_ConditionFieldParameter( cond ),
      new WIR_LabelParameter( b ) );
    cout << o1 << endl;

    WIR_Operation o2(
      ARM_Base::OpCode::BL, ARM_Base::OperationFormat::CL,
      new WIR_ConditionFieldParameter( cond ),
      new WIR_LabelParameter( f ) );
    cout << o2 << endl;
  }

  // Load multiple operations.
  for ( auto &cond : conditions )
    for ( auto &mode : multipleModes ) {

      for ( auto &format :
            vector<ARM_Base::OperationFormat> {
              ARM_Base::OperationFormat::CARR1_1,
              ARM_Base::OperationFormat::CARR1_3,
              ARM_Base::OperationFormat::CARR1_5 } ) {
        WIR_Operation o(
          ARM_Base::OpCode::LDM, format, WIR_ConditionFieldParameter( cond ),
          new WIR_AddressingModeParameter( mode ),
          new WIR_RegisterParameter( r1, WIR_Usage::use ),
          new WIR_RegisterParameter( p.R0(), WIR_Usage::def ) );
        cout << o << endl;
      }

      for ( auto &format :
            vector<ARM_Base::OperationFormat> {
              ARM_Base::OperationFormat::CARR1_2,
              ARM_Base::OperationFormat::CARR1_4 } ) {
        WIR_Operation o(
          ARM_Base::OpCode::LDM, format, WIR_ConditionFieldParameter( cond ),
          new WIR_AddressingModeParameter( mode ),
          new WIR_RegisterParameter( r1, WIR_Usage::defuse ),
          new WIR_RegisterParameter( p.R0(), WIR_Usage::def ) );
        cout << o << endl;
      }

      for ( auto &format :
            vector<ARM_Base::OperationFormat> {
              ARM_Base::OperationFormat::CARR2_1,
              ARM_Base::OperationFormat::CARR2_3,
              ARM_Base::OperationFormat::CARR2_5 } ) {
        WIR_Operation o(
          ARM_Base::OpCode::LDM, format, WIR_ConditionFieldParameter( cond ),
          new WIR_AddressingModeParameter( mode ),
          new WIR_RegisterParameter( r1, WIR_Usage::use ),
          new WIR_RegisterParameter( p.R0(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R1(), WIR_Usage::def ) );
        cout << o << endl;
      }

      for ( auto &format :
            vector<ARM_Base::OperationFormat> {
              ARM_Base::OperationFormat::CARR2_2,
              ARM_Base::OperationFormat::CARR2_4 } ) {
        WIR_Operation o(
          ARM_Base::OpCode::LDM, format, WIR_ConditionFieldParameter( cond ),
          new WIR_AddressingModeParameter( mode ),
          new WIR_RegisterParameter( r1, WIR_Usage::defuse ),
          new WIR_RegisterParameter( p.R0(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R1(), WIR_Usage::def ) );
        cout << o << endl;
      }

      for ( auto &format :
            vector<ARM_Base::OperationFormat> {
              ARM_Base::OperationFormat::CARR3_1,
              ARM_Base::OperationFormat::CARR3_3,
              ARM_Base::OperationFormat::CARR3_5 } ) {
        WIR_Operation o(
          ARM_Base::OpCode::LDM, format, WIR_ConditionFieldParameter( cond ),
          new WIR_AddressingModeParameter( mode ),
          new WIR_RegisterParameter( r1, WIR_Usage::use ),
          new WIR_RegisterParameter( p.R0(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R1(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R2(), WIR_Usage::def ) );
        cout << o << endl;
      }

      for ( auto &format :
            vector<ARM_Base::OperationFormat> {
              ARM_Base::OperationFormat::CARR3_2,
              ARM_Base::OperationFormat::CARR3_4 } ) {
        WIR_Operation o(
          ARM_Base::OpCode::LDM, format, WIR_ConditionFieldParameter( cond ),
          new WIR_AddressingModeParameter( mode ),
          new WIR_RegisterParameter( r1, WIR_Usage::defuse ),
          new WIR_RegisterParameter( p.R0(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R1(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R2(), WIR_Usage::def ) );
        cout << o << endl;
      }

      for ( auto &format :
            vector<ARM_Base::OperationFormat> {
              ARM_Base::OperationFormat::CARR4_1,
              ARM_Base::OperationFormat::CARR4_3,
              ARM_Base::OperationFormat::CARR4_5 } ) {
        WIR_Operation o(
          ARM_Base::OpCode::LDM, format, WIR_ConditionFieldParameter( cond ),
          new WIR_AddressingModeParameter( mode ),
          new WIR_RegisterParameter( r1, WIR_Usage::use ),
          new WIR_RegisterParameter( p.R0(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R1(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R2(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R3(), WIR_Usage::def ) );
        cout << o << endl;
      }

      for ( auto &format :
            vector<ARM_Base::OperationFormat> {
              ARM_Base::OperationFormat::CARR4_2,
              ARM_Base::OperationFormat::CARR4_4 } ) {
        WIR_Operation o(
          ARM_Base::OpCode::LDM, format, WIR_ConditionFieldParameter( cond ),
          new WIR_AddressingModeParameter( mode ),
          new WIR_RegisterParameter( r1, WIR_Usage::defuse ),
          new WIR_RegisterParameter( p.R0(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R1(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R2(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R3(), WIR_Usage::def ) );
        cout << o << endl;
      }

      for ( auto &format :
            vector<ARM_Base::OperationFormat> {
              ARM_Base::OperationFormat::CARR5_1,
              ARM_Base::OperationFormat::CARR5_3,
              ARM_Base::OperationFormat::CARR5_5 } ) {
        WIR_Operation o(
          ARM_Base::OpCode::LDM, format, WIR_ConditionFieldParameter( cond ),
          new WIR_AddressingModeParameter( mode ),
          new WIR_RegisterParameter( r1, WIR_Usage::use ),
          new WIR_RegisterParameter( p.R0(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R1(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R2(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R3(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R4(), WIR_Usage::def ) );
        cout << o << endl;
      }

      for ( auto &format :
            vector<ARM_Base::OperationFormat> {
              ARM_Base::OperationFormat::CARR5_2,
              ARM_Base::OperationFormat::CARR5_4 } ) {
        WIR_Operation o(
          ARM_Base::OpCode::LDM, format, WIR_ConditionFieldParameter( cond ),
          new WIR_AddressingModeParameter( mode ),
          new WIR_RegisterParameter( r1, WIR_Usage::defuse ),
          new WIR_RegisterParameter( p.R0(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R1(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R2(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R3(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R4(), WIR_Usage::def ) );
        cout << o << endl;
      }

      for ( auto &format :
            vector<ARM_Base::OperationFormat> {
              ARM_Base::OperationFormat::CARR6_1,
              ARM_Base::OperationFormat::CARR6_3,
              ARM_Base::OperationFormat::CARR6_5 } ) {
        WIR_Operation o(
          ARM_Base::OpCode::LDM, format, WIR_ConditionFieldParameter( cond ),
          new WIR_AddressingModeParameter( mode ),
          new WIR_RegisterParameter( r1, WIR_Usage::use ),
          new WIR_RegisterParameter( p.R0(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R1(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R2(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R3(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R4(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R5(), WIR_Usage::def ) );
        cout << o << endl;
      }

      for ( auto &format :
            vector<ARM_Base::OperationFormat> {
              ARM_Base::OperationFormat::CARR6_2,
              ARM_Base::OperationFormat::CARR6_4 } ) {
        WIR_Operation o(
          ARM_Base::OpCode::LDM, format, WIR_ConditionFieldParameter( cond ),
          new WIR_AddressingModeParameter( mode ),
          new WIR_RegisterParameter( r1, WIR_Usage::defuse ),
          new WIR_RegisterParameter( p.R0(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R1(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R2(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R3(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R4(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R5(), WIR_Usage::def ) );
        cout << o << endl;
      }

      for ( auto &format :
            vector<ARM_Base::OperationFormat> {
              ARM_Base::OperationFormat::CARR7_1,
              ARM_Base::OperationFormat::CARR7_3,
              ARM_Base::OperationFormat::CARR7_5 } ) {
        WIR_Operation o(
          ARM_Base::OpCode::LDM, format, WIR_ConditionFieldParameter( cond ),
          new WIR_AddressingModeParameter( mode ),
          new WIR_RegisterParameter( r1, WIR_Usage::use ),
          new WIR_RegisterParameter( p.R0(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R1(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R2(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R3(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R4(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R5(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R6(), WIR_Usage::def ) );
        cout << o << endl;
      }

      for ( auto &format :
            vector<ARM_Base::OperationFormat> {
              ARM_Base::OperationFormat::CARR7_2,
              ARM_Base::OperationFormat::CARR7_4 } ) {
        WIR_Operation o(
          ARM_Base::OpCode::LDM, format, WIR_ConditionFieldParameter( cond ),
          new WIR_AddressingModeParameter( mode ),
          new WIR_RegisterParameter( r1, WIR_Usage::defuse ),
          new WIR_RegisterParameter( p.R0(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R1(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R2(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R3(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R4(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R5(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R6(), WIR_Usage::def ) );
        cout << o << endl;
      }

      for ( auto &format :
            vector<ARM_Base::OperationFormat> {
              ARM_Base::OperationFormat::CARR8_1,
              ARM_Base::OperationFormat::CARR8_3,
              ARM_Base::OperationFormat::CARR8_5 } ) {
        WIR_Operation o(
          ARM_Base::OpCode::LDM, format, WIR_ConditionFieldParameter( cond ),
          new WIR_AddressingModeParameter( mode ),
          new WIR_RegisterParameter( r1, WIR_Usage::use ),
          new WIR_RegisterParameter( p.R0(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R1(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R2(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R3(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R4(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R5(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R6(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R7(), WIR_Usage::def ) );
        cout << o << endl;
      }

      for ( auto &format :
            vector<ARM_Base::OperationFormat> {
              ARM_Base::OperationFormat::CARR8_2,
              ARM_Base::OperationFormat::CARR8_4 } ) {
        WIR_Operation o(
          ARM_Base::OpCode::LDM, format, WIR_ConditionFieldParameter( cond ),
          new WIR_AddressingModeParameter( mode ),
          new WIR_RegisterParameter( r1, WIR_Usage::defuse ),
          new WIR_RegisterParameter( p.R0(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R1(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R2(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R3(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R4(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R5(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R6(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R7(), WIR_Usage::def ) );
        cout << o << endl;
      }

      for ( auto &format :
            vector<ARM_Base::OperationFormat> {
              ARM_Base::OperationFormat::CARR9_1,
              ARM_Base::OperationFormat::CARR9_3,
              ARM_Base::OperationFormat::CARR9_5 } ) {
        WIR_Operation o(
          ARM_Base::OpCode::LDM, format, WIR_ConditionFieldParameter( cond ),
          new WIR_AddressingModeParameter( mode ),
          new WIR_RegisterParameter( r1, WIR_Usage::use ),
          new WIR_RegisterParameter( p.R0(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R1(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R2(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R3(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R4(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R5(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R6(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R7(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R8(), WIR_Usage::def ) );
        cout << o << endl;
      }

      for ( auto &format :
            vector<ARM_Base::OperationFormat> {
              ARM_Base::OperationFormat::CARR9_2,
              ARM_Base::OperationFormat::CARR9_4 } ) {
        WIR_Operation o(
          ARM_Base::OpCode::LDM, format, WIR_ConditionFieldParameter( cond ),
          new WIR_AddressingModeParameter( mode ),
          new WIR_RegisterParameter( r1, WIR_Usage::defuse ),
          new WIR_RegisterParameter( p.R0(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R1(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R2(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R3(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R4(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R5(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R6(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R7(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R8(), WIR_Usage::def ) );
        cout << o << endl;
      }

      for ( auto &format :
            vector<ARM_Base::OperationFormat> {
              ARM_Base::OperationFormat::CARR10_1,
              ARM_Base::OperationFormat::CARR10_3,
              ARM_Base::OperationFormat::CARR10_5 } ) {
        WIR_Operation o(
          ARM_Base::OpCode::LDM, format, WIR_ConditionFieldParameter( cond ),
          new WIR_AddressingModeParameter( mode ),
          new WIR_RegisterParameter( r1, WIR_Usage::use ),
          new WIR_RegisterParameter( p.R0(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R1(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R2(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R3(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R4(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R5(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R6(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R7(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R8(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R9(), WIR_Usage::def ) );
        cout << o << endl;
      }

      for ( auto &format :
            vector<ARM_Base::OperationFormat> {
              ARM_Base::OperationFormat::CARR10_2,
              ARM_Base::OperationFormat::CARR10_4 } ) {
        WIR_Operation o(
          ARM_Base::OpCode::LDM, format, WIR_ConditionFieldParameter( cond ),
          new WIR_AddressingModeParameter( mode ),
          new WIR_RegisterParameter( r1, WIR_Usage::defuse ),
          new WIR_RegisterParameter( p.R0(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R1(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R2(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R3(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R4(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R5(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R6(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R7(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R8(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R9(), WIR_Usage::def ) );
        cout << o << endl;
      }

      for ( auto &format :
            vector<ARM_Base::OperationFormat> {
              ARM_Base::OperationFormat::CARR11_1,
              ARM_Base::OperationFormat::CARR11_3,
              ARM_Base::OperationFormat::CARR11_5 } ) {
        WIR_Operation o(
          ARM_Base::OpCode::LDM, format, WIR_ConditionFieldParameter( cond ),
          new WIR_AddressingModeParameter( mode ),
          new WIR_RegisterParameter( r1, WIR_Usage::use ),
          new WIR_RegisterParameter( p.R0(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R1(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R2(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R3(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R4(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R5(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R6(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R7(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R8(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R9(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R10(), WIR_Usage::def ) );
        cout << o << endl;
      }

      for ( auto &format :
            vector<ARM_Base::OperationFormat> {
              ARM_Base::OperationFormat::CARR11_2,
              ARM_Base::OperationFormat::CARR11_4 } ) {
        WIR_Operation o(
          ARM_Base::OpCode::LDM, format, WIR_ConditionFieldParameter( cond ),
          new WIR_AddressingModeParameter( mode ),
          new WIR_RegisterParameter( r1, WIR_Usage::defuse ),
          new WIR_RegisterParameter( p.R0(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R1(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R2(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R3(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R4(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R5(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R6(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R7(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R8(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R9(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R10(), WIR_Usage::def ) );
        cout << o << endl;
      }

      for ( auto &format :
            vector<ARM_Base::OperationFormat> {
              ARM_Base::OperationFormat::CARR12_1,
              ARM_Base::OperationFormat::CARR12_3,
              ARM_Base::OperationFormat::CARR12_5 } ) {
        WIR_Operation o(
          ARM_Base::OpCode::LDM, format, WIR_ConditionFieldParameter( cond ),
          new WIR_AddressingModeParameter( mode ),
          new WIR_RegisterParameter( r1, WIR_Usage::use ),
          new WIR_RegisterParameter( p.R0(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R1(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R2(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R3(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R4(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R5(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R6(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R7(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R8(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R9(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R10(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R11(), WIR_Usage::def ) );
        cout << o << endl;
      }

      for ( auto &format :
            vector<ARM_Base::OperationFormat> {
              ARM_Base::OperationFormat::CARR12_2,
              ARM_Base::OperationFormat::CARR12_4 } ) {
        WIR_Operation o(
          ARM_Base::OpCode::LDM, format, WIR_ConditionFieldParameter( cond ),
          new WIR_AddressingModeParameter( mode ),
          new WIR_RegisterParameter( r1, WIR_Usage::defuse ),
          new WIR_RegisterParameter( p.R0(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R1(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R2(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R3(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R4(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R5(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R6(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R7(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R8(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R9(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R10(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R11(), WIR_Usage::def ) );
        cout << o << endl;
      }

      for ( auto &format :
            vector<ARM_Base::OperationFormat> {
              ARM_Base::OperationFormat::CARR13_1,
              ARM_Base::OperationFormat::CARR13_3,
              ARM_Base::OperationFormat::CARR13_5 } ) {
        WIR_Operation o(
          ARM_Base::OpCode::LDM, format, WIR_ConditionFieldParameter( cond ),
          new WIR_AddressingModeParameter( mode ),
          new WIR_RegisterParameter( r1, WIR_Usage::use ),
          new WIR_RegisterParameter( p.R0(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R1(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R2(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R3(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R4(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R5(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R6(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R7(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R8(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R9(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R10(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R11(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R12(), WIR_Usage::def ) );
        cout << o << endl;
      }

      for ( auto &format :
            vector<ARM_Base::OperationFormat> {
              ARM_Base::OperationFormat::CARR13_2,
              ARM_Base::OperationFormat::CARR13_4 } ) {
        WIR_Operation o(
          ARM_Base::OpCode::LDM, format, WIR_ConditionFieldParameter( cond ),
          new WIR_AddressingModeParameter( mode ),
          new WIR_RegisterParameter( r1, WIR_Usage::defuse ),
          new WIR_RegisterParameter( p.R0(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R1(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R2(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R3(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R4(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R5(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R6(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R7(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R8(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R9(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R10(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R11(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R12(), WIR_Usage::def ) );
        cout << o << endl;
      }

      for ( auto &format :
            vector<ARM_Base::OperationFormat> {
              ARM_Base::OperationFormat::CARR14_1,
              ARM_Base::OperationFormat::CARR14_3,
              ARM_Base::OperationFormat::CARR14_5 } ) {
        WIR_Operation o(
          ARM_Base::OpCode::LDM, format, WIR_ConditionFieldParameter( cond ),
          new WIR_AddressingModeParameter( mode ),
          new WIR_RegisterParameter( r1, WIR_Usage::use ),
          new WIR_RegisterParameter( p.R0(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R1(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R2(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R3(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R4(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R5(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R6(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R7(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R8(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R9(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R10(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R11(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R12(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R13(), WIR_Usage::def ) );
        cout << o << endl;
      }

      for ( auto &format :
            vector<ARM_Base::OperationFormat> {
              ARM_Base::OperationFormat::CARR14_2,
              ARM_Base::OperationFormat::CARR14_4 } ) {
        WIR_Operation o(
          ARM_Base::OpCode::LDM, format, WIR_ConditionFieldParameter( cond ),
          new WIR_AddressingModeParameter( mode ),
          new WIR_RegisterParameter( r1, WIR_Usage::defuse ),
          new WIR_RegisterParameter( p.R0(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R1(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R2(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R3(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R4(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R5(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R6(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R7(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R8(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R9(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R10(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R11(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R12(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R13(), WIR_Usage::def ) );
        cout << o << endl;
      }

      for ( auto &format :
            vector<ARM_Base::OperationFormat> {
              ARM_Base::OperationFormat::CARR15_1,
              ARM_Base::OperationFormat::CARR15_3,
              ARM_Base::OperationFormat::CARR15_5 } ) {
        WIR_Operation o(
          ARM_Base::OpCode::LDM, format, WIR_ConditionFieldParameter( cond ),
          new WIR_AddressingModeParameter( mode ),
          new WIR_RegisterParameter( r1, WIR_Usage::use ),
          new WIR_RegisterParameter( p.R0(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R1(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R2(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R3(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R4(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R5(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R6(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R7(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R8(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R9(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R10(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R11(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R12(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R13(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R14(), WIR_Usage::def ) );
        cout << o << endl;
      }

      for ( auto &format :
            vector<ARM_Base::OperationFormat> {
              ARM_Base::OperationFormat::CARR15_2,
              ARM_Base::OperationFormat::CARR15_4 } ) {
        WIR_Operation o(
          ARM_Base::OpCode::LDM, format, WIR_ConditionFieldParameter( cond ),
          new WIR_AddressingModeParameter( mode ),
          new WIR_RegisterParameter( r1, WIR_Usage::defuse ),
          new WIR_RegisterParameter( p.R0(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R1(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R2(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R3(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R4(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R5(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R6(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R7(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R8(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R9(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R10(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R11(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R12(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R13(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R14(), WIR_Usage::def ) );
        cout << o << endl;
      }

      for ( auto &format :
            vector<ARM_Base::OperationFormat> {
              ARM_Base::OperationFormat::CARR16_1,
              ARM_Base::OperationFormat::CARR16_3 } ) {
        WIR_Operation o(
          ARM_Base::OpCode::LDM, format, WIR_ConditionFieldParameter( cond ),
          new WIR_AddressingModeParameter( mode ),
          new WIR_RegisterParameter( r1, WIR_Usage::use ),
          new WIR_RegisterParameter( p.R0(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R1(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R2(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R3(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R4(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R5(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R6(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R7(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R8(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R9(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R10(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R11(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R12(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R13(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R14(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R15(), WIR_Usage::def ) );
        cout << o << endl;
      }

      for ( auto &format :
            vector<ARM_Base::OperationFormat> {
              ARM_Base::OperationFormat::CARR16_2,
              ARM_Base::OperationFormat::CARR16_4 } ) {
        WIR_Operation o(
          ARM_Base::OpCode::LDM, format, WIR_ConditionFieldParameter( cond ),
          new WIR_AddressingModeParameter( mode ),
          new WIR_RegisterParameter( r1, WIR_Usage::defuse ),
          new WIR_RegisterParameter( p.R0(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R1(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R2(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R3(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R4(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R5(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R6(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R7(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R8(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R9(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R10(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R11(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R12(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R13(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R14(), WIR_Usage::def ),
          new WIR_RegisterParameter( p.R15(), WIR_Usage::def ) );
        cout << o << endl;
      }
    }

  // Store multiple operations.
  for ( auto &cond : conditions )
    for ( auto &mode : multipleModes ) {

      for ( auto &format :
            vector<ARM_Base::OperationFormat> {
              ARM_Base::OperationFormat::CARR1_6,
              ARM_Base::OperationFormat::CARR1_8 } ) {
        WIR_Operation o(
          ARM_Base::OpCode::STM, format, WIR_ConditionFieldParameter( cond ),
          new WIR_AddressingModeParameter( mode ),
          new WIR_RegisterParameter( r1, WIR_Usage::use ),
          new WIR_RegisterParameter( p.R0(), WIR_Usage::use ) );
        cout << o << endl;
      }

      WIR_Operation o12(
        ARM_Base::OpCode::STM, ARM_Base::OperationFormat::CARR1_7,
        new WIR_ConditionFieldParameter( cond ),
        new WIR_AddressingModeParameter( mode ),
        new WIR_RegisterParameter( r1, WIR_Usage::defuse ),
        new WIR_RegisterParameter( p.R0(), WIR_Usage::use ) );
      cout << o12 << endl;

      for ( auto &format :
            vector<ARM_Base::OperationFormat> {
              ARM_Base::OperationFormat::CARR2_6,
              ARM_Base::OperationFormat::CARR2_8 } ) {
        WIR_Operation o(
          ARM_Base::OpCode::STM, format, WIR_ConditionFieldParameter( cond ),
          new WIR_AddressingModeParameter( mode ),
          new WIR_RegisterParameter( r1, WIR_Usage::use ),
          new WIR_RegisterParameter( p.R0(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R1(), WIR_Usage::use ) );
        cout << o << endl;
      }

      WIR_Operation o22(
        ARM_Base::OpCode::STM, ARM_Base::OperationFormat::CARR2_7,
        new WIR_ConditionFieldParameter( cond ),
        new WIR_AddressingModeParameter( mode ),
        new WIR_RegisterParameter( r1, WIR_Usage::defuse ),
        new WIR_RegisterParameter( p.R0(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R1(), WIR_Usage::use ) );
      cout << o22 << endl;

      for ( auto &format :
            vector<ARM_Base::OperationFormat> {
              ARM_Base::OperationFormat::CARR3_6,
              ARM_Base::OperationFormat::CARR3_8 } ) {
        WIR_Operation o(
          ARM_Base::OpCode::STM, format, WIR_ConditionFieldParameter( cond ),
          new WIR_AddressingModeParameter( mode ),
          new WIR_RegisterParameter( r1, WIR_Usage::use ),
          new WIR_RegisterParameter( p.R0(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R1(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R2(), WIR_Usage::use ) );
        cout << o << endl;
      }

      WIR_Operation o32(
        ARM_Base::OpCode::STM, ARM_Base::OperationFormat::CARR3_7,
        new WIR_ConditionFieldParameter( cond ),
        new WIR_AddressingModeParameter( mode ),
        new WIR_RegisterParameter( r1, WIR_Usage::defuse ),
        new WIR_RegisterParameter( p.R0(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R1(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R2(), WIR_Usage::use ) );
      cout << o32 << endl;

      for ( auto &format :
            vector<ARM_Base::OperationFormat> {
              ARM_Base::OperationFormat::CARR4_6,
              ARM_Base::OperationFormat::CARR4_8 } ) {
        WIR_Operation o(
          ARM_Base::OpCode::STM, format, WIR_ConditionFieldParameter( cond ),
          new WIR_AddressingModeParameter( mode ),
          new WIR_RegisterParameter( r1, WIR_Usage::use ),
          new WIR_RegisterParameter( p.R0(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R1(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R2(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R3(), WIR_Usage::use ) );
        cout << o << endl;
      }

      WIR_Operation o42(
        ARM_Base::OpCode::STM, ARM_Base::OperationFormat::CARR4_7,
        new WIR_ConditionFieldParameter( cond ),
        new WIR_AddressingModeParameter( mode ),
        new WIR_RegisterParameter( r1, WIR_Usage::defuse ),
        new WIR_RegisterParameter( p.R0(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R1(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R2(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R3(), WIR_Usage::use ) );
      cout << o42 << endl;

      for ( auto &format :
            vector<ARM_Base::OperationFormat> {
              ARM_Base::OperationFormat::CARR5_6,
              ARM_Base::OperationFormat::CARR5_8 } ) {
        WIR_Operation o(
          ARM_Base::OpCode::STM, format, WIR_ConditionFieldParameter( cond ),
          new WIR_AddressingModeParameter( mode ),
          new WIR_RegisterParameter( r1, WIR_Usage::use ),
          new WIR_RegisterParameter( p.R0(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R1(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R2(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R3(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R4(), WIR_Usage::use ) );
        cout << o << endl;
      }

      WIR_Operation o52(
        ARM_Base::OpCode::STM, ARM_Base::OperationFormat::CARR5_7,
        new WIR_ConditionFieldParameter( cond ),
        new WIR_AddressingModeParameter( mode ),
        new WIR_RegisterParameter( r1, WIR_Usage::defuse ),
        new WIR_RegisterParameter( p.R0(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R1(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R2(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R3(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R4(), WIR_Usage::use ) );
      cout << o52 << endl;

      for ( auto &format :
            vector<ARM_Base::OperationFormat> {
              ARM_Base::OperationFormat::CARR6_6,
              ARM_Base::OperationFormat::CARR6_8 } ) {
        WIR_Operation o(
          ARM_Base::OpCode::STM, format, WIR_ConditionFieldParameter( cond ),
          new WIR_AddressingModeParameter( mode ),
          new WIR_RegisterParameter( r1, WIR_Usage::use ),
          new WIR_RegisterParameter( p.R0(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R1(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R2(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R3(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R4(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R5(), WIR_Usage::use ) );
        cout << o << endl;
      }

      WIR_Operation o62(
        ARM_Base::OpCode::STM, ARM_Base::OperationFormat::CARR6_7,
        new WIR_ConditionFieldParameter( cond ),
        new WIR_AddressingModeParameter( mode ),
        new WIR_RegisterParameter( r1, WIR_Usage::defuse ),
        new WIR_RegisterParameter( p.R0(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R1(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R2(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R3(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R4(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R5(), WIR_Usage::use ) );
      cout << o62 << endl;

      for ( auto &format :
            vector<ARM_Base::OperationFormat> {
              ARM_Base::OperationFormat::CARR7_6,
              ARM_Base::OperationFormat::CARR7_8 } ) {
        WIR_Operation o(
          ARM_Base::OpCode::STM, format, WIR_ConditionFieldParameter( cond ),
          new WIR_AddressingModeParameter( mode ),
          new WIR_RegisterParameter( r1, WIR_Usage::use ),
          new WIR_RegisterParameter( p.R0(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R1(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R2(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R3(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R4(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R5(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R6(), WIR_Usage::use ) );
        cout << o << endl;
      }

      WIR_Operation o72(
        ARM_Base::OpCode::STM, ARM_Base::OperationFormat::CARR7_7,
        new WIR_ConditionFieldParameter( cond ),
        new WIR_AddressingModeParameter( mode ),
        new WIR_RegisterParameter( r1, WIR_Usage::defuse ),
        new WIR_RegisterParameter( p.R0(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R1(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R2(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R3(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R4(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R5(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R6(), WIR_Usage::use ) );
      cout << o72 << endl;

      for ( auto &format :
            vector<ARM_Base::OperationFormat> {
              ARM_Base::OperationFormat::CARR8_6,
              ARM_Base::OperationFormat::CARR8_8 } ) {
        WIR_Operation o(
          ARM_Base::OpCode::STM, format, WIR_ConditionFieldParameter( cond ),
          new WIR_AddressingModeParameter( mode ),
          new WIR_RegisterParameter( r1, WIR_Usage::use ),
          new WIR_RegisterParameter( p.R0(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R1(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R2(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R3(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R4(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R5(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R6(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R7(), WIR_Usage::use ) );
        cout << o << endl;
      }

      WIR_Operation o82(
        ARM_Base::OpCode::STM, ARM_Base::OperationFormat::CARR8_7,
        new WIR_ConditionFieldParameter( cond ),
        new WIR_AddressingModeParameter( mode ),
        new WIR_RegisterParameter( r1, WIR_Usage::defuse ),
        new WIR_RegisterParameter( p.R0(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R1(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R2(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R3(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R4(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R5(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R6(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R7(), WIR_Usage::use ) );
      cout << o82 << endl;

      for ( auto &format :
            vector<ARM_Base::OperationFormat> {
              ARM_Base::OperationFormat::CARR9_6,
              ARM_Base::OperationFormat::CARR9_8 } ) {
        WIR_Operation o(
          ARM_Base::OpCode::STM, format, WIR_ConditionFieldParameter( cond ),
          new WIR_AddressingModeParameter( mode ),
          new WIR_RegisterParameter( r1, WIR_Usage::use ),
          new WIR_RegisterParameter( p.R0(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R1(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R2(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R3(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R4(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R5(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R6(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R7(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R8(), WIR_Usage::use ) );
        cout << o << endl;
      }

      WIR_Operation o92(
        ARM_Base::OpCode::STM, ARM_Base::OperationFormat::CARR9_7,
        new WIR_ConditionFieldParameter( cond ),
        new WIR_AddressingModeParameter( mode ),
        new WIR_RegisterParameter( r1, WIR_Usage::defuse ),
        new WIR_RegisterParameter( p.R0(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R1(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R2(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R3(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R4(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R5(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R6(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R7(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R8(), WIR_Usage::use ) );
      cout << o92 << endl;

      for ( auto &format :
            vector<ARM_Base::OperationFormat> {
              ARM_Base::OperationFormat::CARR10_6,
              ARM_Base::OperationFormat::CARR10_8 } ) {
        WIR_Operation o(
          ARM_Base::OpCode::STM, format, WIR_ConditionFieldParameter( cond ),
          new WIR_AddressingModeParameter( mode ),
          new WIR_RegisterParameter( r1, WIR_Usage::use ),
          new WIR_RegisterParameter( p.R0(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R1(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R2(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R3(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R4(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R5(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R6(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R7(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R8(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R9(), WIR_Usage::use ) );
        cout << o << endl;
      }

      WIR_Operation o102(
        ARM_Base::OpCode::STM, ARM_Base::OperationFormat::CARR10_7,
        new WIR_ConditionFieldParameter( cond ),
        new WIR_AddressingModeParameter( mode ),
        new WIR_RegisterParameter( r1, WIR_Usage::defuse ),
        new WIR_RegisterParameter( p.R0(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R1(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R2(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R3(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R4(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R5(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R6(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R7(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R8(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R9(), WIR_Usage::use ) );
      cout << o102 << endl;

      for ( auto &format :
            vector<ARM_Base::OperationFormat> {
              ARM_Base::OperationFormat::CARR11_6,
              ARM_Base::OperationFormat::CARR11_8 } ) {
        WIR_Operation o(
          ARM_Base::OpCode::STM, format, WIR_ConditionFieldParameter( cond ),
          new WIR_AddressingModeParameter( mode ),
          new WIR_RegisterParameter( r1, WIR_Usage::use ),
          new WIR_RegisterParameter( p.R0(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R1(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R2(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R3(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R4(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R5(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R6(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R7(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R8(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R9(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R10(), WIR_Usage::use ) );
        cout << o << endl;
      }

      WIR_Operation o112(
        ARM_Base::OpCode::STM, ARM_Base::OperationFormat::CARR11_7,
        new WIR_ConditionFieldParameter( cond ),
        new WIR_AddressingModeParameter( mode ),
        new WIR_RegisterParameter( r1, WIR_Usage::defuse ),
        new WIR_RegisterParameter( p.R0(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R1(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R2(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R3(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R4(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R5(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R6(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R7(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R8(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R9(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R10(), WIR_Usage::use ) );
      cout << o112 << endl;

      for ( auto &format :
            vector<ARM_Base::OperationFormat> {
              ARM_Base::OperationFormat::CARR12_6,
              ARM_Base::OperationFormat::CARR12_8 } ) {
        WIR_Operation o(
          ARM_Base::OpCode::STM, format, WIR_ConditionFieldParameter( cond ),
          new WIR_AddressingModeParameter( mode ),
          new WIR_RegisterParameter( r1, WIR_Usage::use ),
          new WIR_RegisterParameter( p.R0(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R1(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R2(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R3(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R4(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R5(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R6(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R7(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R8(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R9(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R10(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R11(), WIR_Usage::use ) );
        cout << o << endl;
      }

      WIR_Operation o122(
        ARM_Base::OpCode::STM, ARM_Base::OperationFormat::CARR12_7,
        new WIR_ConditionFieldParameter( cond ),
        new WIR_AddressingModeParameter( mode ),
        new WIR_RegisterParameter( r1, WIR_Usage::defuse ),
        new WIR_RegisterParameter( p.R0(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R1(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R2(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R3(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R4(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R5(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R6(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R7(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R8(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R9(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R10(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R11(), WIR_Usage::use ) );
      cout << o122 << endl;

      for ( auto &format :
            vector<ARM_Base::OperationFormat> {
              ARM_Base::OperationFormat::CARR13_6,
              ARM_Base::OperationFormat::CARR13_8 } ) {
        WIR_Operation o(
          ARM_Base::OpCode::STM, format, WIR_ConditionFieldParameter( cond ),
          new WIR_AddressingModeParameter( mode ),
          new WIR_RegisterParameter( r1, WIR_Usage::use ),
          new WIR_RegisterParameter( p.R0(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R1(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R2(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R3(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R4(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R5(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R6(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R7(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R8(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R9(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R10(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R11(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R12(), WIR_Usage::use ) );
        cout << o << endl;
      }

      WIR_Operation o132(
        ARM_Base::OpCode::STM, ARM_Base::OperationFormat::CARR13_7,
        new WIR_ConditionFieldParameter( cond ),
        new WIR_AddressingModeParameter( mode ),
        new WIR_RegisterParameter( r1, WIR_Usage::defuse ),
        new WIR_RegisterParameter( p.R0(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R1(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R2(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R3(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R4(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R5(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R6(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R7(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R8(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R9(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R10(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R11(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R12(), WIR_Usage::use ) );
      cout << o132 << endl;

      for ( auto &format :
            vector<ARM_Base::OperationFormat> {
              ARM_Base::OperationFormat::CARR14_6,
              ARM_Base::OperationFormat::CARR14_8 } ) {
        WIR_Operation o(
          ARM_Base::OpCode::STM, format, WIR_ConditionFieldParameter( cond ),
          new WIR_AddressingModeParameter( mode ),
          new WIR_RegisterParameter( r1, WIR_Usage::use ),
          new WIR_RegisterParameter( p.R0(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R1(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R2(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R3(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R4(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R5(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R6(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R7(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R8(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R9(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R10(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R11(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R12(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R13(), WIR_Usage::use ) );
        cout << o << endl;
      }

      WIR_Operation o142(
        ARM_Base::OpCode::STM, ARM_Base::OperationFormat::CARR14_7,
        new WIR_ConditionFieldParameter( cond ),
        new WIR_AddressingModeParameter( mode ),
        new WIR_RegisterParameter( r1, WIR_Usage::defuse ),
        new WIR_RegisterParameter( p.R0(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R1(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R2(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R3(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R4(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R5(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R6(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R7(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R8(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R9(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R10(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R11(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R12(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R13(), WIR_Usage::use ) );
      cout << o142 << endl;

      for ( auto &format :
            vector<ARM_Base::OperationFormat> {
              ARM_Base::OperationFormat::CARR15_6,
              ARM_Base::OperationFormat::CARR15_8 } ) {
        WIR_Operation o(
          ARM_Base::OpCode::STM, format, WIR_ConditionFieldParameter( cond ),
          new WIR_AddressingModeParameter( mode ),
          new WIR_RegisterParameter( r1, WIR_Usage::use ),
          new WIR_RegisterParameter( p.R0(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R1(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R2(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R3(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R4(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R5(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R6(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R7(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R8(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R9(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R10(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R11(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R12(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R13(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R14(), WIR_Usage::use ) );
        cout << o << endl;
      }

      WIR_Operation o152(
        ARM_Base::OpCode::STM, ARM_Base::OperationFormat::CARR15_7,
        new WIR_ConditionFieldParameter( cond ),
        new WIR_AddressingModeParameter( mode ),
        new WIR_RegisterParameter( r1, WIR_Usage::defuse ),
        new WIR_RegisterParameter( p.R0(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R1(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R2(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R3(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R4(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R5(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R6(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R7(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R8(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R9(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R10(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R11(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R12(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R13(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R14(), WIR_Usage::use ) );
      cout << o152 << endl;

      for ( auto &format :
            vector<ARM_Base::OperationFormat> {
              ARM_Base::OperationFormat::CARR16_6,
              ARM_Base::OperationFormat::CARR16_8 } ) {
        WIR_Operation o(
          ARM_Base::OpCode::STM, format, WIR_ConditionFieldParameter( cond ),
          new WIR_AddressingModeParameter( mode ),
          new WIR_RegisterParameter( r1, WIR_Usage::use ),
          new WIR_RegisterParameter( p.R0(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R1(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R2(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R3(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R4(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R5(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R6(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R7(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R8(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R9(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R10(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R11(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R12(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R13(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R14(), WIR_Usage::use ),
          new WIR_RegisterParameter( p.R15(), WIR_Usage::use ) );
        cout << o << endl;
      }

      WIR_Operation o162(
        ARM_Base::OpCode::STM, ARM_Base::OperationFormat::CARR16_7,
        new WIR_ConditionFieldParameter( cond ),
        new WIR_AddressingModeParameter( mode ),
        new WIR_RegisterParameter( r1, WIR_Usage::defuse ),
        new WIR_RegisterParameter( p.R0(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R1(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R2(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R3(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R4(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R5(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R6(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R7(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R8(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R9(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R10(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R11(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R12(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R13(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R14(), WIR_Usage::use ),
        new WIR_RegisterParameter( p.R15(), WIR_Usage::use ) );
      cout << o162 << endl;
    }

  for ( auto &cond : conditions )
    for ( auto &format :
            vector<ARM_Base::OperationFormat> {
              ARM_Base::OperationFormat::CR_1,
              ARM_Base::OperationFormat::CR_2 } ) {
      WIR_Operation o(
        ARM_Base::OpCode::MRS, format, WIR_ConditionFieldParameter( cond ),
        new WIR_RegisterParameter( r1, WIR_Usage::def ) );
      cout << o << endl;
    }

  for ( auto &cond : conditions ) {
    for ( auto &format :
            vector<ARM_Base::OperationFormat> {
              ARM_Base::OperationFormat::CAAAAC8RA_1,
              ARM_Base::OperationFormat::CAAAAC8RA_2 } ) {
      WIR_Operation o(
        ARM_Base::OpCode::MSR, format, WIR_ConditionFieldParameter( cond ),
        new WIR_AddressingModeParameter( ARM_Base::AddressingMode::f ),
        new WIR_AddressingModeParameter( ARM_Base::AddressingMode::x ),
        new WIR_AddressingModeParameter( ARM_Base::AddressingMode::c ),
        new WIR_AddressingModeParameter( ARM_Base::AddressingMode::s ),
        new ARM_Const8_Unsigned( 42 ),
        new ARM_Const5_RotateAmount( 6 ) );
      cout << o << endl;
    }

    for ( auto &format :
            vector<ARM_Base::OperationFormat> {
              ARM_Base::OperationFormat::CAAAAR_1,
              ARM_Base::OperationFormat::CAAAAR_2 } ) {
      WIR_Operation o(
        ARM_Base::OpCode::MSR, format, WIR_ConditionFieldParameter( cond ),
        new WIR_AddressingModeParameter( ARM_Base::AddressingMode::s ),
        new WIR_AddressingModeParameter( ARM_Base::AddressingMode::x ),
        new WIR_AddressingModeParameter( ARM_Base::AddressingMode::s ),
        new WIR_AddressingModeParameter( ARM_Base::AddressingMode::s ),
        new WIR_RegisterParameter( r1, WIR_Usage::use ) );
      cout << o << endl;
    }
  }

  for ( auto &cond : conditions ) {
    WIR_Operation o(
      ARM_Base::OpCode::SWI, ARM_Base::OperationFormat::CC24,
      new WIR_ConditionFieldParameter( cond ),
      new ARM_Const24_Unsigned( 42 ) );
    cout << o << endl;
  }

  for ( auto &cond : conditions ) {
    WIR_Operation o1(
      ARM_Base::OpCode::SWP, ARM_Base::OperationFormat::CRRR_5,
      new WIR_ConditionFieldParameter( cond ),
      new WIR_RegisterParameter( r1, WIR_Usage::def ),
      new WIR_RegisterParameter( r2, WIR_Usage::use ),
      new WIR_RegisterParameter( r4, WIR_Usage::use ) );
    cout << o1 << endl;

    WIR_Operation o2(
      ARM_Base::OpCode::SWPB, ARM_Base::OperationFormat::CRRR_5,
      new WIR_ConditionFieldParameter( cond ),
      new WIR_RegisterParameter( r1, WIR_Usage::def ),
      new WIR_RegisterParameter( r2, WIR_Usage::use ),
      new WIR_RegisterParameter( r4, WIR_Usage::use ) );
    cout << o2 << endl;
  }

  // Coprocessor operations.
  for ( auto &cond : conditions )
    for ( auto &proc : coprocessors ) {
      WIR_Operation o1(
        ARM_Base::OpCode::MCR, ARM_Base::OperationFormat::CAORSSO_2,
        new WIR_ConditionFieldParameter( cond ),
        new WIR_AddressingModeParameter( proc ),
        new ARM_Const3_CoprocessorOpcode( 0 ),
        new WIR_RegisterParameter( p.R9(), WIR_Usage::use ),
        new WIR_StringParameter( "c9" ),
        new WIR_StringParameter( "c5" ),
        new ARM_Const3_CoprocessorOpcode( 1 ) );
      cout << o1 << endl;

      WIR_Operation o2(
        ARM_Base::OpCode::MRC, ARM_Base::OperationFormat::CAORSSO_1,
        new WIR_ConditionFieldParameter( cond ),
        new WIR_AddressingModeParameter( proc ),
        new ARM_Const3_CoprocessorOpcode( 0 ),
        new WIR_RegisterParameter( p.R9(), WIR_Usage::def ),
        new WIR_StringParameter( "c9" ),
        new WIR_StringParameter( "c5" ),
        new ARM_Const3_CoprocessorOpcode( 1 ) );
      cout << o2 << endl;

      WIR_Operation o3(
        ARM_Base::OpCode::CDP, ARM_Base::OperationFormat::CAOSSSO,
        new WIR_ConditionFieldParameter( cond ),
        new WIR_AddressingModeParameter( proc ),
        new ARM_Const4_CoprocessorOpcode( 15 ),
        new WIR_StringParameter( "c9" ),
        new WIR_StringParameter( "c5" ),
        new WIR_StringParameter( "c6" ),
        new ARM_Const3_CoprocessorOpcode( 1 ) );
      cout << o3 << endl;

      for ( auto &opcode : vector<ARM_Base::OpCode> { ARM_Base::OpCode::LDC,
                                                      ARM_Base::OpCode::STC } ) {
        for ( auto &addsub : memoryAddSubModes ) {
          for ( auto &format : vector<ARM_Base::OperationFormat> { ARM_Base::OperationFormat::CASRAC8_1,
                                                                   ARM_Base::OperationFormat::CASRAC8_2 } ) {
            WIR_Operation o(
              opcode, format,
              new WIR_ConditionFieldParameter( cond ),
              new WIR_AddressingModeParameter( proc ),
              new WIR_StringParameter( "c9" ),
              new WIR_RegisterParameter( p.R9(), WIR_Usage::use ),
              new WIR_AddressingModeParameter( addsub ),
              new ARM_Const10_CoprocessorOffset( 20 ) );
            cout << o << endl;
          }

          for ( auto &prepost : memoryAddressingModes )
            for ( auto &format : vector<ARM_Base::OperationFormat> { ARM_Base::OperationFormat::CASARAC8_1,
                                                                     ARM_Base::OperationFormat::CASARAC8_2 } ) {
              WIR_Operation o(
                opcode, format,
                new WIR_ConditionFieldParameter( cond ),
                new WIR_AddressingModeParameter( proc ),
                new WIR_StringParameter( "c12" ),
                new WIR_AddressingModeParameter( prepost ),
                new WIR_RegisterParameter( p.R9(), WIR_Usage::defuse ),
                new WIR_AddressingModeParameter( addsub ),
                new ARM_Const10_CoprocessorOffset( 44 ) );
              cout << o << endl;
            }

          for ( auto &format : vector<ARM_Base::OperationFormat> { ARM_Base::OperationFormat::CASRC8_1,
                                                                   ARM_Base::OperationFormat::CASRC8_2 } ) {
            WIR_Operation o(
              opcode, format,
              new WIR_ConditionFieldParameter( cond ),
              new WIR_AddressingModeParameter( proc ),
              new WIR_StringParameter( "c4" ),
              new WIR_RegisterParameter( p.R9(), WIR_Usage::use ),
              new ARM_Const8_Unsigned( 255 ) );
            cout << o << endl;
          }
        }
      }
    }

  return( 0 );
}
