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
#include <set>

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

  WIR_TaskManager t;
  WIR_System sys( "lpc2880.sys", t );
  auto &p = sys.getComponents<ARMv4T>().begin()->get();

  WIR_CompilationUnit c1;
  WIR_Function f( "foo" );

  WIR_BasicBlock &bb1 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &bb3 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &bb2 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &bb5 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &bb4 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &bb6 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &bb7 = f.pushBackBasicBlock( {} );

  // Create a CFG.
  bb1.pushBackInstruction(
    { { ARM_Base::OpCode::ADD, ARMv4T::OperationFormat::TRRR_1,
        WIR_RegisterParameter( p.R1(), WIR_Usage::def ),
        WIR_RegisterParameter( p.R2(), WIR_Usage::use ),
        WIR_RegisterParameter( p.R3(), WIR_Usage::use ) } } );
  bb1.pushBackInstruction(
    { { ARMv4T::OpCode::BX, ARMv4T::OperationFormat::CR_3,
        WIR_ConditionFieldParameter( ARM_Base::Condition::pl ),
        WIR_RegisterParameter( p.R4(), WIR_Usage::use ) } } );

  bb3.pushBackInstruction(
    { { ARM_Base::OpCode::ADD, ARMv4T::OperationFormat::TRPCC10_1,
        WIR_RegisterParameter( p.R5(), WIR_Usage::def ),
        WIR_RegisterParameter( p.PC(), WIR_Usage::use ),
        ARM_Const10_Unsigned4( 1020 ) } } );
  bb3.pushBackInstruction(
    { { ARMv4T::OpCode::POP, ARMv4T::OperationFormat::TR7PC,
        WIR_RegisterParameter( p.R0(), WIR_Usage::def ),
        WIR_RegisterParameter( p.R1(), WIR_Usage::def ),
        WIR_RegisterParameter( p.R2(), WIR_Usage::def ),
        WIR_RegisterParameter( p.R3(), WIR_Usage::def ),
        WIR_RegisterParameter( p.R4(), WIR_Usage::def ),
        WIR_RegisterParameter( p.R5(), WIR_Usage::def ),
        WIR_RegisterParameter( p.R6(), WIR_Usage::def ),
        WIR_RegisterParameter( p.PC(), WIR_Usage::def ) } } );

  bb2.pushBackInstruction(
    { { ARMv4T::OpCode::PUSH, ARMv4T::OperationFormat::TR3LR,
        WIR_RegisterParameter( p.R0(), WIR_Usage::use ),
        WIR_RegisterParameter( p.R2(), WIR_Usage::use ),
        WIR_RegisterParameter( p.R6(), WIR_Usage::use ),
        WIR_RegisterParameter( p.LR(), WIR_Usage::use ) } });
  bb2.pushBackInstruction(
    { { ARM_Base::OpCode::MUL, ARM_Base::OperationFormat::CRRR_1,
        WIR_ConditionFieldParameter( ARM_Base::Condition::gt ),
        WIR_RegisterParameter( p.R1(), WIR_Usage::def ),
        WIR_RegisterParameter( p.R2(), WIR_Usage::use ),
        WIR_RegisterParameter( p.R4(), WIR_Usage::use ) } } );
  bb2.pushBackInstruction(
    { { ARM_Base::OpCode::B, ARM_Base::OperationFormat::CL,
        WIR_ConditionFieldParameter( ARM_Base::Condition::vs ),
        WIR_LabelParameter( bb6 ) } } );

  bb5.pushBackInstruction(
    { { ARM_Base::OpCode::MRS, ARM_Base::OperationFormat::CR_2,
        WIR_ConditionFieldParameter( ARM_Base::Condition::eq ),
        WIR_RegisterParameter( p.R1(), WIR_Usage::def ) } } );
  bb5.pushBackInstruction(
    { { ARM_Base::OpCode::MSR, ARM_Base::OperationFormat::CAAAAC8RA_1,
        WIR_ConditionFieldParameter( ARM_Base::Condition::al ),
        WIR_AddressingModeParameter( ARM_Base::AddressingMode::f ),
        WIR_AddressingModeParameter( ARM_Base::AddressingMode::x ),
        WIR_AddressingModeParameter( ARM_Base::AddressingMode::c ),
        WIR_AddressingModeParameter( ARM_Base::AddressingMode::s ),
        ARM_Const8_Unsigned( 42 ),
        ARM_Const5_RotateAmount( 6 ) } } );
  bb5.pushBackInstruction(
    { { ARM_Base::OpCode::BL, ARM_Base::OperationFormat::CL,
        WIR_ConditionFieldParameter( ARM_Base::Condition::hi ),
        WIR_LabelParameter( f ) } } );

  bb4.pushBackInstruction(
    { { ARM_Base::OpCode::SWI, ARM_Base::OperationFormat::CC24,
        WIR_ConditionFieldParameter( ARM_Base::Condition::ls ),
        ARM_Const24_Unsigned( 42 ) } } );
  bb4.pushBackInstruction(
    { { ARM_Base::OpCode::SWP, ARM_Base::OperationFormat::CRRR_5,
        WIR_ConditionFieldParameter( ARM_Base::Condition::le ),
        WIR_RegisterParameter( p.R1(), WIR_Usage::def ),
        WIR_RegisterParameter( p.R2(), WIR_Usage::use ),
        WIR_RegisterParameter( p.R4(), WIR_Usage::use ) } } );

  bb6.pushBackInstruction(
    { { ARM_Base::OpCode::SUB, ARMv4T::OperationFormat::TRRR_1,
        WIR_RegisterParameter( p.R1(), WIR_Usage::def ),
        WIR_RegisterParameter( p.R2(), WIR_Usage::use ),
        WIR_RegisterParameter( p.R3(), WIR_Usage::use ) } } );

  bb7.pushBackInstruction(
    { { ARMv4T::OpCode::STR, ARMv4T::OperationFormat::TRRC7_2,
        WIR_RegisterParameter( p.R1(), WIR_Usage::use ),
        WIR_RegisterParameter( p.R2(), WIR_Usage::use ),
        ARM_Const7_Unsigned( 16 ) } } );

  c1.pushBackFunction( move( f ) );
  c1.pushBackFunction( WIR_Function( "bar" ) );

  WIR_CompilationUnit &c = sys.pushBackCompilationUnit( c1 );
  WIR_Function &f1 = c.getFunctions().front().get();
  auto it = f1.getBasicBlocks().begin();
  ++it;
  ++it;
  WIR_BasicBlock &b3 = it->get();

  auto &d3 = c.pushBackData( WIR_Data( "d3" ) );
  auto &d2 = c.pushFrontData( WIR_Data( "d2" ) );
  auto &d1 = c.pushFrontData( WIR_Data( "d1" ) );
  auto &d4 = c.pushBackData( WIR_Data( "d4" ) );

  d1.setSize( 5 );
  d2.setSize( 13 );
  d3.setSize( 42 );
  d4.setSize( 1000 );

  // Move some items to different sections.
  sys.findSymbol( d2 ).setConst();                  // -> .rodata
  d4.pushBackInitData( WIR_DataInit( 42 ) );        // -> .data
  sys.findSymbol( b3 ).setSection( *( p.findSection( ".text_spm" ) ) );
  sys.findSymbol( f1 ).setGlobal();

  // Dump the entire code for arm-elf-as.
  cout << arm << sys;

  return( 0 );
}
