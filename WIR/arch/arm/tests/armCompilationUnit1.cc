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
  WIR_CompilationUnit c;
  WIR_Function f( "main" );
  WIR_Function f1( "foo" );

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
  const vector<ARM_Base::AddressingMode> memoryAddSubModes {
    ARM_Base::AddressingMode::plus,
    ARM_Base::AddressingMode::minus };
  const vector<ARM_Base::AddressingMode> rightShiftModes {
    ARM_Base::AddressingMode::lsr,
    ARM_Base::AddressingMode::asr };

  auto &b1 = f.pushBackBasicBlock( {} );
  for ( auto &cond : conditions )
    for ( auto &addsub : memoryAddSubModes )
      b1.pushBackInstruction(
        { { ARMv5TE::OpCode::LDRD, ARMv5TE::OperationFormat::CPRAC8_1,
            WIR_ConditionFieldParameter( cond ),
            WIR_RegisterParameter( p6, WIR_Usage::def ),
            WIR_RegisterParameter( r4, WIR_Usage::use ),
            WIR_AddressingModeParameter( addsub ),
            ARM_Const8_Unsigned( 255 ) } } );

  auto &b2 = f.pushBackBasicBlock( {} );
  for ( auto &cond : conditions )
    for ( auto &proc : coprocessors )
      b2.pushBackInstruction(
        { { ARMv5TE::OpCode::MCRR, ARMv5TE::OperationFormat::CAORRS_2,
            WIR_ConditionFieldParameter( cond ),
            WIR_AddressingModeParameter( proc ),
            ARM_Const3_CoprocessorOpcode( 0 ),
            WIR_RegisterParameter( r8, WIR_Usage::use ),
            WIR_RegisterParameter( r11, WIR_Usage::use ),
            WIR_StringParameter( "c9" ) } } );

  auto &b3 = f1.pushBackBasicBlock( {} );
  for ( auto &cond : conditions )
    for ( auto &proc : coprocessors )
      b3.pushBackInstruction(
        { { ARMv5TE::OpCode::MRRC, ARMv5TE::OperationFormat::CAORRS_1,
            WIR_ConditionFieldParameter( cond ),
            WIR_AddressingModeParameter( proc ),
            ARM_Const3_CoprocessorOpcode( 0 ),
            WIR_RegisterParameter( r8, WIR_Usage::def ),
            WIR_RegisterParameter( r11, WIR_Usage::def ),
            WIR_StringParameter( "c9" ) } } );

  auto &b4 = f1.pushBackBasicBlock( {} );
  for ( auto &addsub : memoryAddSubModes ) {
    b4.pushBackInstruction(
      { { ARMv5TE::OpCode::PLD, ARMv5TE::OperationFormat::RAC12_1,
          WIR_RegisterParameter( r6, WIR_Usage::use ),
          WIR_AddressingModeParameter( addsub ),
          ARM_Const12_Unsigned( 4095 ) } } );

    b4.pushBackInstruction(
      { { ARMv5TE::OpCode::PLD, ARMv5TE::OperationFormat::RAR_1,
          WIR_RegisterParameter( r6, WIR_Usage::use ),
          WIR_AddressingModeParameter( addsub ),
          WIR_RegisterParameter( r11, WIR_Usage::use ) } } );

    b4.pushBackInstruction(
      { { ARMv5TE::OpCode::PLD, ARMv5TE::OperationFormat::RARC5_1,
          WIR_RegisterParameter( r6, WIR_Usage::use ),
          WIR_AddressingModeParameter( addsub ),
          WIR_RegisterParameter( r11, WIR_Usage::use ),
          ARM_Const5_Unsigned( 31 ) } } );

    for ( auto &rshift : rightShiftModes )
      b4.pushBackInstruction(
        { { ARMv5TE::OpCode::PLD, ARMv5TE::OperationFormat::RARAC60_1,
            WIR_RegisterParameter( r6, WIR_Usage::use ),
            WIR_AddressingModeParameter( addsub ),
            WIR_RegisterParameter( r11, WIR_Usage::use ),
            WIR_AddressingModeParameter( rshift ),
            ARM_Const6_Unsigned0( 32 ) } } );

    b4.pushBackInstruction(
      { { ARMv5TE::OpCode::PLD, ARMv5TE::OperationFormat::RARC50_1,
          WIR_RegisterParameter( r6, WIR_Usage::use ),
          WIR_AddressingModeParameter( addsub ),
          WIR_RegisterParameter( r11, WIR_Usage::use ),
          ARM_Const5_Unsigned0( 31 ) } } );

    b4.pushBackInstruction(
      { { ARMv5TE::OpCode::PLD, ARMv5TE::OperationFormat::RAR_2,
          WIR_RegisterParameter( r6, WIR_Usage::use ),
          WIR_AddressingModeParameter( addsub ),
          WIR_RegisterParameter( r11, WIR_Usage::use ) } } );
  }

  f.insertContainer( WIR_Comment( "Comment 1" ) );
  f.insertContainer( WIR_FileInfo( "test.c", 42 ) );
  f1.insertContainer( WIR_Comment( "Comment 2" ) );
  f1.insertContainer( WIR_FileInfo( "test.c", 333 ) );

  c.pushBackFunction( f );
  c.pushBackFunction( f1 );
  c.setName( "test.c" );

  c.insertContainer( WIR_Comment( "Comment 3" ) );
  c.insertContainer( WIR_FileInfo( "test.c", 1 ) );

  cout << arm << comment << fileinfo << c;

  return( 0 );
}
