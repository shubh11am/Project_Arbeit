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
#include <arch/tricore/tc13.h>


using namespace std;
using namespace WIR;


int main( void )
{
  WIR_Init();

  WIR_TaskManager t;
  WIR_System sys( "tc1796.sys", t );
  auto &p = sys.getComponents<TC13>().begin()->get();

  WIR_CompilationUnit c1;
  WIR_Function f( "foo" );
  const TC_ARegP &ar1 = p.A5();
  const TC_DRegP &dr1 = p.D4(), &dr2 = p.D5(), &dr3 = p.D6(), &dr4 = p.D7();
  const TC_ERegP &er1 = p.E6();
  const TC_PRegP &pr1 = p.P0(), &pr2 = p.P8();

  WIR_BasicBlock &bb1 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &bb3 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &bb2 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &bb5 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &bb4 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &bb6 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &bb7 = f.pushBackBasicBlock( {} );

  // Create a CFG.
  bb1.pushBackInstruction(
    { { TC13::OpCode::ABSDIFS_H, TC13::OperationFormat::DDD_1,
        WIR_RegisterParameter( dr1, WIR_Usage::def ),
        WIR_RegisterParameter( dr2, WIR_Usage::use ),
        WIR_RegisterParameter( dr3, WIR_Usage::use ) } } );
  bb1.pushBackInstruction(
    { { TC13::OpCode::ADDS, TC13::OperationFormat::SDD_2,
        WIR_RegisterParameter( dr1, WIR_Usage::defuse ),
        WIR_RegisterParameter( dr2, WIR_Usage::use ) } } );

  bb3.pushBackInstruction(
    { { TC13::OpCode::EXTR, TC13::OperationFormat::DDDC5,
        WIR_RegisterParameter( dr1, WIR_Usage::def ),
        WIR_RegisterParameter( dr2, WIR_Usage::use ),
        WIR_RegisterParameter( dr3, WIR_Usage::use ),
        TC_Const5_Unsigned( 9 ) } } );
  bb3.pushBackInstruction(
    { { TC13::OpCode::DVINIT, TC13::OperationFormat::EDD,
        WIR_RegisterParameter( er1, WIR_Usage::def ),
        WIR_RegisterParameter( dr1, WIR_Usage::use ),
        WIR_RegisterParameter( dr2, WIR_Usage::use ) } } );

  bb2.pushBackInstruction(
    { { TC13::OpCode::INSN_T, TC13::OperationFormat::DDC5DC5_1,
        WIR_RegisterParameter( dr1, WIR_Usage::def ),
        WIR_RegisterParameter( dr2, WIR_Usage::use ),
        TC_Const5_Unsigned( 7 ),
        WIR_RegisterParameter( dr3, WIR_Usage::use ),
        TC_Const5_Unsigned( 9 ) } } );
  bb2.pushBackInstruction(
    { { TC13::OpCode::LD_DA, TC13::OperationFormat::PPBRA_1,
        WIR_RegisterParameter( pr1, WIR_Usage::def ),
        WIR_RegisterParameter( pr2, WIR_Usage::defuse ) } } );
  bb2.pushBackInstruction(
    { { TC13::OpCode::JZ_A, TC13::OperationFormat::SAL_1,
        WIR_RegisterParameter( ar1, WIR_Usage::use ),
        WIR_LabelParameter( bb4 ) } } );

  bb5.pushBackInstruction(
    { { TC13::OpCode::MADDSUR_H, TC13::OperationFormat::DDDDC1_7,
        WIR_RegisterParameter( dr1, WIR_Usage::def ),
        WIR_RegisterParameter( dr2, WIR_Usage::use ),
        WIR_RegisterParameter( dr3, WIR_Usage::use ),
        WIR_RegisterParameter( dr4, WIR_Usage::use ),
        TC_Const1_Unsigned( 1 ) } } );
  bb5.pushBackInstruction(
    { { TC13::OpCode::MSUB_F, TC13::OperationFormat::DDDD,
        WIR_RegisterParameter( dr1, WIR_Usage::def ),
        WIR_RegisterParameter( dr2, WIR_Usage::use ),
        WIR_RegisterParameter( dr3, WIR_Usage::use ),
        WIR_RegisterParameter( dr4, WIR_Usage::use ) } } );
  bb5.pushBackInstruction(
    { { TC13::OpCode::JGEZ, TC13::OperationFormat::SDL,
        WIR_RegisterParameter( dr1, WIR_Usage::use ),
        WIR_LabelParameter( bb6 ) } } );

  bb4.pushBackInstruction(
    { { TC13::OpCode::OR_OR_T, TC13::OperationFormat::DDC5DC5_2,
        WIR_RegisterParameter( dr1, WIR_Usage::defuse ),
        WIR_RegisterParameter( dr2, WIR_Usage::use ),
        TC_Const5_Unsigned( 13 ),
        WIR_RegisterParameter( dr3, WIR_Usage::use ),
        TC_Const5_Unsigned( 27 ) } } );
  bb4.pushBackInstruction(
    { { TC13::OpCode::J, TC13::OperationFormat::SL,
        WIR_LabelParameter( bb7 ) } } );

  bb6.pushBackInstruction(
    { { TC13::OpCode::RSUB, TC13::OperationFormat::SD,
        WIR_RegisterParameter( dr1, WIR_Usage::defuse ) } } );

  bb7.pushBackInstruction(
    { { TC13::OpCode::ST_D, TC13::OperationFormat::C18EABSA,
        TC_Const18_Unsigned( 16383 ),
        WIR_RegisterParameter( er1, WIR_Usage::use ) } } );

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

  // Dump the entire code for tricore-as.
  cout << tricore << sys;

  return( 0 );
}
