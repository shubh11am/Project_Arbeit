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
#include <arch/arm/armv5t.h>


using namespace std;
using namespace WIR;


int main( void )
{
  WIR_Init();

  ARMv5T p;
  const ARM_HiRegP &r8 = p.R8(), &r11 = p.R11();
  WIR_Function f( "main" );

  cout.iword( WIR_Indentation() ) = 8;
  cout << arm;

  // The following operations must be accepted for the ARMv5T ISA.
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

  WIR_Operation o1(
    ARMv5T::OpCode::BKPT, ARMv5T::OperationFormat::C16_1,
    ARM_Const16_Unsigned( 65535 ) );
  cout << o1 << endl;

  WIR_Operation o2(
    ARMv5T::OpCode::BLX, ARMv5T::OperationFormat::L,
    WIR_LabelParameter( f ) );
  cout << o2 << endl;

  for ( auto &cond : conditions ) {
    WIR_Operation o(
      ARMv5T::OpCode::BLX, ARMv5T::OperationFormat::CR_3,
      WIR_ConditionFieldParameter( cond ),
      WIR_RegisterParameter( r8, WIR_Usage::use ) );
    cout << o << endl;
  }

  for ( auto &proc : coprocessors ) {
    WIR_Operation o(
      ARMv5T::OpCode::CDP2, ARMv5T::OperationFormat::AOSSSO,
      WIR_AddressingModeParameter( proc ),
      ARM_Const4_CoprocessorOpcode( 15 ),
      WIR_StringParameter( "c9" ),
      WIR_StringParameter( "c5" ),
      WIR_StringParameter( "c6" ),
      ARM_Const3_CoprocessorOpcode( 1 ) );
    cout << o << endl;
  }

  for ( auto &cond : conditions ) {
    WIR_Operation o(
      ARMv5T::OpCode::CLZ, ARMv5T::OperationFormat::CRR_1,
      WIR_ConditionFieldParameter( cond ),
      WIR_RegisterParameter( r8, WIR_Usage::def ),
      WIR_RegisterParameter( r11, WIR_Usage::use ) );
    cout << o << endl;
  }

  for ( auto &proc : coprocessors ) {
    for ( auto &addsub : memoryAddSubModes ) {
      for ( auto &format : vector<ARM_Base::OperationFormat> { ARMv5T::OperationFormat::ASRAC8_1,
                                                               ARMv5T::OperationFormat::ASRAC8_2 } ) {
        WIR_Operation o(
          ARMv5T::OpCode::LDC2, format,
          WIR_AddressingModeParameter( proc ),
          WIR_StringParameter( "c9" ),
          WIR_RegisterParameter( r8, WIR_Usage::use ),
          WIR_AddressingModeParameter( addsub ),
          ARM_Const10_CoprocessorOffset( 20 ) );
        cout << o << endl;
      }

      for ( auto &prepost : memoryAddressingModes )
        for ( auto &format : vector<ARM_Base::OperationFormat> { ARMv5T::OperationFormat::ASARAC8_1,
                                                                 ARMv5T::OperationFormat::ASARAC8_2 } ) {
          WIR_Operation o(
            ARMv5T::OpCode::LDC2, format,
            WIR_AddressingModeParameter( proc ),
            WIR_StringParameter( "c12" ),
            WIR_AddressingModeParameter( prepost ),
            WIR_RegisterParameter( p.R9(), WIR_Usage::defuse ),
            WIR_AddressingModeParameter( addsub ),
            ARM_Const10_CoprocessorOffset( 44 ) );
          cout << o << endl;
        }

      for ( auto &format : vector<ARM_Base::OperationFormat> { ARMv5T::OperationFormat::ASRC8_1,
                                                               ARMv5T::OperationFormat::ASRC8_2 } ) {
        WIR_Operation o(
          ARMv5T::OpCode::LDC2, format,
          WIR_AddressingModeParameter( proc ),
          WIR_StringParameter( "c4" ),
          WIR_RegisterParameter( p.R9(), WIR_Usage::use ),
          ARM_Const8_Unsigned( 255 ) );
        cout << o << endl;
      }
    }
  }

  for ( auto &proc : coprocessors ) {
    WIR_Operation o(
      ARMv5T::OpCode::MCR2, ARMv5T::OperationFormat::AORSSO_2,
      WIR_AddressingModeParameter( proc ),
      ARM_Const3_CoprocessorOpcode( 0 ),
      WIR_RegisterParameter( r11, WIR_Usage::use ),
      WIR_StringParameter( "c9" ),
      WIR_StringParameter( "c5" ),
      ARM_Const3_CoprocessorOpcode( 1 ) );
    cout << o << endl;
  }

  for ( auto &proc : coprocessors ) {
    for ( auto &addsub : memoryAddSubModes ) {
      for ( auto &format : vector<ARM_Base::OperationFormat> { ARMv5T::OperationFormat::ASRAC8_1,
                                                               ARMv5T::OperationFormat::ASRAC8_2 } ) {
        WIR_Operation o(
          ARMv5T::OpCode::STC2, format,
          WIR_AddressingModeParameter( proc ),
          WIR_StringParameter( "c9" ),
          WIR_RegisterParameter( r8, WIR_Usage::use ),
          WIR_AddressingModeParameter( addsub ),
          ARM_Const10_CoprocessorOffset( 20 ) );
        cout << o << endl;
      }

      for ( auto &prepost : memoryAddressingModes )
        for ( auto &format : vector<ARM_Base::OperationFormat> { ARMv5T::OperationFormat::ASARAC8_1,
                                                                 ARMv5T::OperationFormat::ASARAC8_2 } ) {
          WIR_Operation o(
            ARMv5T::OpCode::STC2, format,
            WIR_AddressingModeParameter( proc ),
            WIR_StringParameter( "c12" ),
            WIR_AddressingModeParameter( prepost ),
            WIR_RegisterParameter( p.R9(), WIR_Usage::defuse ),
            WIR_AddressingModeParameter( addsub ),
            ARM_Const10_CoprocessorOffset( 44 ) );
          cout << o << endl;
        }

      for ( auto &format : vector<ARM_Base::OperationFormat> { ARMv5T::OperationFormat::ASRC8_1,
                                                               ARMv5T::OperationFormat::ASRC8_2 } ) {
        WIR_Operation o(
          ARMv5T::OpCode::STC2, format,
          WIR_AddressingModeParameter( proc ),
          WIR_StringParameter( "c4" ),
          WIR_RegisterParameter( p.R9(), WIR_Usage::use ),
          ARM_Const8_Unsigned( 255 ) );
        cout << o << endl;
      }
    }
  }

  WIR_Operation o3(
    ARMv5T::OpCode::BLX, ARMv4T::OperationFormat::TL_2,
    WIR_LabelParameter( f ) );
  cout << o3 << endl;

  WIR_Operation o4(
    ARMv5T::OpCode::BLX, ARMv5T::OperationFormat::TR_1,
    WIR_RegisterParameter( r8, WIR_Usage::use ) );
  cout << o4 << endl;

  return( 0 );
}
