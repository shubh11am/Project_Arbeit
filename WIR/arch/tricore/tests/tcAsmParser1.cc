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
#include <sstream>
#include <string>

// Include WIR headers
#include <wir/wir.h>
#include <arch/tricore/tc131.h>
#include <arch/tricore/asmparser/tcasmargument.h>
#include <arch/tricore/asmparser/tcasmparser.h>


using namespace std;
using namespace WIR;


int main( void )
{
  WIR_Init();

  WIR_TaskManager t;
  WIR_System sys( "tc1797.sys", t );
  auto &p = sys.getComponents<TC131>().begin()->get();

  const TC_ARegP &a1 = p.A5(), &a2 = p.A9(), &a3 = p.A13();
  const TC_DRegP &d1 = p.D4(), &d2 = p.D5(), &d3 = p.D6(), &d4 = p.D7();
  const TC_ERegP &e1 = p.E6(), &e2 = p.E10();
  const TC_PRegP &p1 = p.P0(), &p2 = p.P8();
  const TC_PSWBit &pswC = p.PSW_C();

  WIR_CompilationUnit &c = sys.pushBackCompilationUnit( {} );
  WIR_Function &f = c.pushBackFunction( WIR_Function( "main" ) );
  WIR_BasicBlock &b1 = f.pushBackBasicBlock( {} );

  stringstream sstr;
  sstr.iword( WIR_Indentation() ) = 8;
  sstr << tricore;

  // This lambda serves for inserting a TriCore operation and adding a comment
  // to it.
  auto tcop = [&]( WIR_Operation &&o ) {
    auto &op = b1.pushBackInstruction( WIR_Instruction( WIR_Operation( o ) ) );
    op.insertContainer(
      WIR_Comment( "Operation Format: " + o.getOperationFormat().getName() ) );
  };

  // The following operations must be accepted according to the TriCore ISA.
  tcop(
    { TC131::OpCode::ABS, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ABS_B, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ABS_H, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ABSDIF, TC131::OperationFormat::DDC9_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const9_Signed( -128 ) } );

  tcop(
    { TC131::OpCode::ABSDIF, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ABSDIF_B, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ABSDIF_H, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ABSDIFS, TC131::OperationFormat::DDC9_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const9_Signed( -128 ) } );

  tcop(
    { TC131::OpCode::ABSDIFS, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ABSDIFS_H, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ABSS, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ABSS_H, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ADD, TC131::OperationFormat::DDC9_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const9_Signed( -128 ) } );

  tcop(
    { TC131::OpCode::ADD, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ADD, TC131::OperationFormat::SDC4_2,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new TC_Const4_Signed( 5 ) } );

  tcop(
    { TC131::OpCode::ADD, TC131::OperationFormat::SDIC4_2,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( p.D15(), WIR_Usage::use ),
      new TC_Const4_Signed( -5 ) } );

  tcop(
    { TC131::OpCode::ADD, TC131::OperationFormat::SIDC4,
      new WIR_RegisterParameter( p.D15(), WIR_Usage::def ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new TC_Const4_Signed( -5 ) } );

  tcop(
    { TC131::OpCode::ADD, TC131::OperationFormat::SDD_2,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ADD, TC131::OperationFormat::SDID_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( p.D15(), WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ADD, TC131::OperationFormat::SIDD,
      new WIR_RegisterParameter( p.D15(), WIR_Usage::def ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ADD_A, TC131::OperationFormat::AAA,
      new WIR_RegisterParameter( a1, WIR_Usage::def ),
      new WIR_RegisterParameter( a2, WIR_Usage::use ),
      new WIR_RegisterParameter( a3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ADD_A, TC131::OperationFormat::SAC4_2,
      new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
      new TC_Const4_Signed( -5 ) } );

  tcop(
    { TC131::OpCode::ADD_A, TC131::OperationFormat::SAA_5,
      new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( a2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ADD_B, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ADD_F, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ADD_H, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ADDC, TC131::OperationFormat::DDC9PSW_2,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const9_Signed( -128 ),
      new WIR_RegisterParameter( pswC, WIR_Usage::defuse ) } );

  tcop(
    { TC131::OpCode::ADDC, TC131::OperationFormat::DDDPSW_2,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( pswC, WIR_Usage::defuse ) } );

  tcop(
    { TC131::OpCode::ADDI, TC131::OperationFormat::DDC16_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const16_Signed( -26361 ) } );

  tcop(
    { TC131::OpCode::ADDIH, TC131::OperationFormat::DDC16_2,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const16_Unsigned( 26361 ) } );

  tcop(
    { TC131::OpCode::ADDIH_A, TC131::OperationFormat::AAC16,
      new WIR_RegisterParameter( a1, WIR_Usage::def ),
      new WIR_RegisterParameter( a2, WIR_Usage::use ),
      new TC_Const16_Unsigned( 26361 ) } );

  tcop(
    { TC131::OpCode::ADDS, TC131::OperationFormat::DDC9_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const9_Signed( -128 ) } );

  tcop(
    { TC131::OpCode::ADDS, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ADDS, TC131::OperationFormat::SDD_2,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ADDS_H, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ADDS_HU, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ADDS_U, TC131::OperationFormat::DDC9_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const9_Signed( -128 ) } );

  tcop(
    { TC131::OpCode::ADDS_U, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ADDSC_A, TC131::OperationFormat::AADC2,
      new WIR_RegisterParameter( a1, WIR_Usage::def ),
      new WIR_RegisterParameter( a2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new TC_Const2_Unsigned( 2 ) } );

  tcop(
    { TC131::OpCode::ADDSC_A, TC131::OperationFormat::SAAIC2,
      new WIR_RegisterParameter( a1, WIR_Usage::def ),
      new WIR_RegisterParameter( a2, WIR_Usage::use ),
      new WIR_RegisterParameter( p.D15(), WIR_Usage::use ),
      new TC_Const2_Unsigned( 3 ) } );

  tcop(
    { TC131::OpCode::ADDSC_AT, TC131::OperationFormat::AAD,
      new WIR_RegisterParameter( a1, WIR_Usage::def ),
      new WIR_RegisterParameter( a2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ADDX, TC131::OperationFormat::DDC9PSW_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const9_Signed( -128 ),
      new WIR_RegisterParameter( pswC, WIR_Usage::def ) } );

  tcop(
    { TC131::OpCode::ADDX, TC131::OperationFormat::DDDPSW_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( pswC, WIR_Usage::def ) } );

  tcop(
    { TC131::OpCode::AND, TC131::OperationFormat::DDC9_2,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const9_Unsigned( 128 ) } );

  tcop(
    { TC131::OpCode::AND, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::AND, TC131::OperationFormat::SIC8_2,
      new WIR_RegisterParameter( p.D15(), WIR_Usage::defuse ),
      new TC_Const8_Unsigned( 64 ) } );

  tcop(
    { TC131::OpCode::AND, TC131::OperationFormat::SDD_2,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::AND_AND_T, TC131::OperationFormat::DDC5DC5_2,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const5_Unsigned( 13 ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new TC_Const5_Unsigned( 27 ) } );

  tcop(
    { TC131::OpCode::AND_ANDN_T, TC131::OperationFormat::DDC5DC5_2,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const5_Unsigned( 13 ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new TC_Const5_Unsigned( 27 ) } );

  tcop(
    { TC131::OpCode::AND_EQ, TC131::OperationFormat::DDC9_3,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const9_Signed( -128 ) } );

  tcop(
    { TC131::OpCode::AND_EQ, TC131::OperationFormat::DDD_2,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::AND_GE, TC131::OperationFormat::DDC9_3,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const9_Signed( -128 ) } );

  tcop(
    { TC131::OpCode::AND_GE, TC131::OperationFormat::DDD_2,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::AND_GE_U, TC131::OperationFormat::DDC9_4,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const9_Unsigned( 128 ) } );

  tcop(
    { TC131::OpCode::AND_GE_U, TC131::OperationFormat::DDD_2,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::AND_LT, TC131::OperationFormat::DDC9_3,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const9_Signed( -128 ) } );

  tcop(
    { TC131::OpCode::AND_LT, TC131::OperationFormat::DDD_2,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::AND_LT_U, TC131::OperationFormat::DDC9_4,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const9_Unsigned( 128 ) } );

  tcop(
    { TC131::OpCode::AND_LT_U, TC131::OperationFormat::DDD_2,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::AND_NE, TC131::OperationFormat::DDC9_3,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const9_Signed( -128 ) } );

  tcop(
    { TC131::OpCode::AND_NE, TC131::OperationFormat::DDD_2,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::AND_NOR_T, TC131::OperationFormat::DDC5DC5_2,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const5_Unsigned( 13 ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new TC_Const5_Unsigned( 27 ) } );

  tcop(
    { TC131::OpCode::AND_OR_T, TC131::OperationFormat::DDC5DC5_2,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const5_Unsigned( 13 ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new TC_Const5_Unsigned( 27 ) } );

  tcop(
    { TC131::OpCode::AND_T, TC131::OperationFormat::DDC5DC5_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const5_Unsigned( 13 ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new TC_Const5_Unsigned( 27 ) } );

  tcop(
    { TC131::OpCode::ANDN, TC131::OperationFormat::DDC9_2,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const9_Unsigned( 128 ) } );

  tcop(
    { TC131::OpCode::ANDN, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ANDN_T, TC131::OperationFormat::DDC5DC5_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const5_Unsigned( 13 ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new TC_Const5_Unsigned( 27 ) } );

  tcop(
    { TC131::OpCode::BISR, TC131::OperationFormat::C9,
      new TC_Const9_Unsigned( 128 ) } );

  tcop(
    { TC131::OpCode::BISR, TC131::OperationFormat::SC8,
      new TC_Const8_Unsigned( 64 ) } );

  tcop(
    { TC131::OpCode::BMERGE, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::BSPLIT, TC131::OperationFormat::ED,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::CACHEA_I, TC131::OperationFormat::AC10BOA,
      new WIR_RegisterParameter( a1, WIR_Usage::use ),
      new TC_Const10_Signed( -486 ) } );

  tcop(
    { TC131::OpCode::CACHEA_I, TC131::OperationFormat::PBRA,
      new WIR_RegisterParameter( p1, WIR_Usage::defuse ) } );

  tcop(
    { TC131::OpCode::CACHEA_I, TC131::OperationFormat::PC10CA,
      new WIR_RegisterParameter( p1, WIR_Usage::defuse ),
      new TC_Const10_Signed( -486 ) } );

  tcop(
    { TC131::OpCode::CACHEA_I, TC131::OperationFormat::AC10PIA,
      new WIR_AddressingModeParameter( TC131::AddressingMode::post ),
      new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
      new TC_Const10_Signed( -486 ) } );

  tcop(
    { TC131::OpCode::CACHEA_I, TC131::OperationFormat::AC10PIA,
      new WIR_AddressingModeParameter( TC131::AddressingMode::pre ),
      new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
      new TC_Const10_Signed( -486 ) } );

  tcop(
    { TC131::OpCode::CACHEA_W, TC131::OperationFormat::AC10BOA,
      new WIR_RegisterParameter( a1, WIR_Usage::use ),
      new TC_Const10_Signed( -486 ) } );

  tcop(
    { TC131::OpCode::CACHEA_W, TC131::OperationFormat::PBRA,
      new WIR_RegisterParameter( p1, WIR_Usage::defuse ) } );

  tcop(
    { TC131::OpCode::CACHEA_W, TC131::OperationFormat::PC10CA,
      new WIR_RegisterParameter( p1, WIR_Usage::defuse ),
      new TC_Const10_Signed( -486 ) } );

  tcop(
    { TC131::OpCode::CACHEA_W, TC131::OperationFormat::AC10PIA,
      new WIR_AddressingModeParameter( TC131::AddressingMode::post ),
      new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
      new TC_Const10_Signed( -486 ) } );

  tcop(
    { TC131::OpCode::CACHEA_W, TC131::OperationFormat::AC10PIA,
      new WIR_AddressingModeParameter( TC131::AddressingMode::pre ),
      new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
      new TC_Const10_Signed( -486 ) } );

  tcop(
    { TC131::OpCode::CACHEA_WI, TC131::OperationFormat::AC10BOA,
      new WIR_RegisterParameter( a1, WIR_Usage::use ),
      new TC_Const10_Signed( -486 ) } );

  tcop(
    { TC131::OpCode::CACHEA_WI, TC131::OperationFormat::PBRA,
      new WIR_RegisterParameter( p1, WIR_Usage::defuse ) } );

  tcop(
    { TC131::OpCode::CACHEA_WI, TC131::OperationFormat::PC10CA,
      new WIR_RegisterParameter( p1, WIR_Usage::defuse ),
      new TC_Const10_Signed( -486 ) } );

  tcop(
    { TC131::OpCode::CACHEA_WI, TC131::OperationFormat::AC10PIA,
      new WIR_AddressingModeParameter( TC131::AddressingMode::post ),
      new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
      new TC_Const10_Signed( -486 ) } );

  tcop(
    { TC131::OpCode::CACHEA_WI, TC131::OperationFormat::AC10PIA,
      new WIR_AddressingModeParameter( TC131::AddressingMode::pre ),
      new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
      new TC_Const10_Signed( -486 ) } );

  tcop(
    { TC131::OpCode::CACHEI_W, TC131::OperationFormat::AC10BOA,
      new WIR_RegisterParameter( a1, WIR_Usage::use ),
      new TC_Const10_Signed( -486 ) } );

  tcop(
    { TC131::OpCode::CACHEI_W, TC131::OperationFormat::AC10PIA,
      new WIR_AddressingModeParameter( TC131::AddressingMode::post ),
      new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
      new TC_Const10_Signed( -486 ) } );

  tcop(
    { TC131::OpCode::CACHEI_W, TC131::OperationFormat::AC10PIA,
      new WIR_AddressingModeParameter( TC131::AddressingMode::pre ),
      new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
      new TC_Const10_Signed( -486 ) } );

  tcop(
    { TC131::OpCode::CACHEI_WI, TC131::OperationFormat::AC10BOA,
      new WIR_RegisterParameter( a1, WIR_Usage::use ),
      new TC_Const10_Signed( -486 ) } );

  tcop(
    { TC131::OpCode::CACHEI_WI, TC131::OperationFormat::AC10PIA,
      new WIR_AddressingModeParameter( TC131::AddressingMode::post ),
      new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
      new TC_Const10_Signed( -486 ) } );

  tcop(
    { TC131::OpCode::CACHEI_WI, TC131::OperationFormat::AC10PIA,
      new WIR_AddressingModeParameter( TC131::AddressingMode::pre ),
      new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
      new TC_Const10_Signed( -486 ) } );

  tcop(
    { TC131::OpCode::CADD, TC131::OperationFormat::DDDC9_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new TC_Const9_Signed( -128 ) } );

  tcop(
    { TC131::OpCode::CADD, TC131::OperationFormat::DDDD,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::CADD, TC131::OperationFormat::SDIC4_3,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( p.D15(), WIR_Usage::use ),
      new TC_Const4_Signed( -5 ) } );

  tcop(
    { TC131::OpCode::CADDN, TC131::OperationFormat::DDDC9_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new TC_Const9_Signed( -128 ) } );

  tcop(
    { TC131::OpCode::CADDN, TC131::OperationFormat::DDDD,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::CADDN, TC131::OperationFormat::SDIC4_3,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( p.D15(), WIR_Usage::use ),
      new TC_Const4_Signed( -5 ) } );

  tcop(
    { TC131::OpCode::CALL, TC131::OperationFormat::L,
      new WIR_LabelParameter( f ) } );

  tcop(
    { TC131::OpCode::CALL, TC131::OperationFormat::SL,
      new WIR_LabelParameter( f ) } );

  tcop(
    { TC131::OpCode::CALLA, TC131::OperationFormat::L,
      new WIR_LabelParameter( f ) } );

  tcop(
    { TC131::OpCode::CALLI, TC131::OperationFormat::A,
      new WIR_RegisterParameter( a1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::CLO, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::CLO_H, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::CLS, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::CLS_H, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::CLZ, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::CLZ_H, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::CMOV, TC131::OperationFormat::SDIC4_3,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( p.D15(), WIR_Usage::use ),
      new TC_Const4_Signed( -5 ) } );

  tcop(
    { TC131::OpCode::CMOV, TC131::OperationFormat::SDID_2,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( p.D15(), WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::CMOVN, TC131::OperationFormat::SDIC4_3,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( p.D15(), WIR_Usage::use ),
      new TC_Const4_Signed( -5 ) } );

  tcop(
    { TC131::OpCode::CMOVN, TC131::OperationFormat::SDID_2,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( p.D15(), WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::CMP_F, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::CSUB, TC131::OperationFormat::DDDD,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::CSUBN, TC131::OperationFormat::DDDD,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::DEBUG, TC131::OperationFormat::SYS } );

  tcop(
    { TC131::OpCode::DEBUG, TC131::OperationFormat::S } );

  tcop(
    { TC131::OpCode::DEXTR, TC131::OperationFormat::DDDC5,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new TC_Const5_Unsigned( 30 ) } );

  tcop(
    { TC131::OpCode::DEXTR, TC131::OperationFormat::DDDD,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::DISABLE, TC131::OperationFormat::SYS } );

  tcop(
    { TC131::OpCode::DIV_F, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::DSYNC, TC131::OperationFormat::SYS } );

  tcop(
    { TC131::OpCode::DVADJ, TC131::OperationFormat::EED,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::DVINIT, TC131::OperationFormat::EDD,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::DVINIT_B, TC131::OperationFormat::EDD,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::DVINIT_BU, TC131::OperationFormat::EDD,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::DVINIT_H, TC131::OperationFormat::EDD,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::DVINIT_HU, TC131::OperationFormat::EDD,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::DVINIT_U, TC131::OperationFormat::EDD,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::DVSTEP, TC131::OperationFormat::EED,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::DVSTEP_U, TC131::OperationFormat::EED,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ENABLE, TC131::OperationFormat::SYS } );

  tcop(
    { TC131::OpCode::EQ, TC131::OperationFormat::DDC9_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const9_Signed( -128 ) } );

  tcop(
    { TC131::OpCode::EQ, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::EQ, TC131::OperationFormat::SIDC4,
      new WIR_RegisterParameter( p.D15(), WIR_Usage::def ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new TC_Const4_Signed( -5 ) } );

  tcop(
    { TC131::OpCode::EQ, TC131::OperationFormat::SIDD,
      new WIR_RegisterParameter( p.D15(), WIR_Usage::def ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::EQ_A, TC131::OperationFormat::DAA,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( a1, WIR_Usage::use ),
      new WIR_RegisterParameter( a2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::EQ_B, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::EQ_H, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::EQ_W, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::EQANY_B, TC131::OperationFormat::DDC9_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const9_Signed( -128 ) } );

  tcop(
    { TC131::OpCode::EQANY_B, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::EQANY_H, TC131::OperationFormat::DDC9_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const9_Signed( -128 ) } );

  tcop(
    { TC131::OpCode::EQANY_H, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::EQZ_A, TC131::OperationFormat::DA,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( a1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::EXTR, TC131::OperationFormat::DDC5C5,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const5_Unsigned( 13 ),
      new TC_Const5_Unsigned( 9 ) } );

  tcop(
    { TC131::OpCode::EXTR, TC131::OperationFormat::DDE,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( e1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::EXTR, TC131::OperationFormat::DDDC5,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new TC_Const5_Unsigned( 9 ) } );

  tcop(
    { TC131::OpCode::EXTR_U, TC131::OperationFormat::DDC5C5,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const5_Unsigned( 13 ),
      new TC_Const5_Unsigned( 9 ) } );

  tcop(
    { TC131::OpCode::EXTR_U, TC131::OperationFormat::DDE,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( e1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::EXTR_U, TC131::OperationFormat::DDDC5,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new TC_Const5_Unsigned( 9 ) } );

  tcop(
    { TC131::OpCode::FTOI, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::FTOIZ, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::FTOQ31, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::FTOQ31Z, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::FTOU, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::FTOUZ, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::GE, TC131::OperationFormat::DDC9_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const9_Signed( -128 ) } );

  tcop(
    { TC131::OpCode::GE, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::GE_A, TC131::OperationFormat::DAA,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( a1, WIR_Usage::use ),
      new WIR_RegisterParameter( a2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::GE_U, TC131::OperationFormat::DDC9_2,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const9_Unsigned( 128 ) } );

  tcop(
    { TC131::OpCode::GE_U, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::IMASK, TC131::OperationFormat::EC4C5C5,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new TC_Const4_Unsigned( 7 ),
      new TC_Const5_Unsigned( 13 ),
      new TC_Const5_Unsigned( 9 ) } );

  tcop(
    { TC131::OpCode::IMASK, TC131::OperationFormat::EC4DC5,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new TC_Const4_Unsigned( 7 ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new TC_Const5_Unsigned( 9 ) } );

  tcop(
    { TC131::OpCode::IMASK, TC131::OperationFormat::EDC5C5,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new TC_Const5_Unsigned( 7 ),
      new TC_Const5_Unsigned( 9 ) } );

  tcop(
    { TC131::OpCode::IMASK, TC131::OperationFormat::EDDC5,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const5_Unsigned( 9 ) } );

  tcop(
    { TC131::OpCode::INS_T, TC131::OperationFormat::DDC5DC5_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const5_Unsigned( 7 ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new TC_Const5_Unsigned( 9 ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDC4C5C5,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const4_Unsigned( 7 ),
      new TC_Const5_Unsigned( 13 ),
      new TC_Const5_Unsigned( 9 ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDC4E,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const4_Unsigned( 7 ),
      new WIR_RegisterParameter( e1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDC4DC5,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const4_Unsigned( 7 ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new TC_Const5_Unsigned( 9 ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDC5C5,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new TC_Const5_Unsigned( 13 ),
      new TC_Const5_Unsigned( 9 ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDE,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( e1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDDC5,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const5_Unsigned( 9 ) } );

  tcop(
    { TC131::OpCode::INSN_T, TC131::OperationFormat::DDC5DC5_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const5_Unsigned( 7 ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new TC_Const5_Unsigned( 9 ) } );

  tcop(
    { TC131::OpCode::ISYNC, TC131::OperationFormat::SYS } );

  tcop(
    { TC131::OpCode::ITOF, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::IXMAX, TC131::OperationFormat::EED,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::IXMAX_U, TC131::OperationFormat::EED,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::IXMIN, TC131::OperationFormat::EED,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::IXMIN_U, TC131::OperationFormat::EED,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::J, TC131::OperationFormat::L,
      new WIR_LabelParameter( b1 ) } );

  tcop(
    { TC131::OpCode::J, TC131::OperationFormat::SL,
      new WIR_LabelParameter( b1 ) } );

  tcop(
    { TC131::OpCode::JA, TC131::OperationFormat::L,
      new WIR_LabelParameter( b1 ) } );

  tcop(
    { TC131::OpCode::JEQ, TC131::OperationFormat::DC4L_1,
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new TC_Const4_Signed( 3 ),
      new WIR_LabelParameter( b1 ) } );

  tcop(
    { TC131::OpCode::JEQ, TC131::OperationFormat::DDL_1,
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_LabelParameter( b1 ) } );

  tcop(
    { TC131::OpCode::JEQ, TC131::OperationFormat::SIC4L,
      new WIR_RegisterParameter( p.D15(), WIR_Usage::use ),
      new TC_Const4_Signed( 3 ),
      new WIR_LabelParameter( b1 ) } );

  tcop(
    { TC131::OpCode::JEQ, TC131::OperationFormat::SIDL,
      new WIR_RegisterParameter( p.D15(), WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_LabelParameter( b1 ) } );

  tcop(
    { TC131::OpCode::JEQ_A, TC131::OperationFormat::AAL,
      new WIR_RegisterParameter( a1, WIR_Usage::use ),
      new WIR_RegisterParameter( a2, WIR_Usage::use ),
      new WIR_LabelParameter( b1 ) } );

  tcop(
    { TC131::OpCode::JGE, TC131::OperationFormat::DC4L_1,
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new TC_Const4_Signed( 3 ),
      new WIR_LabelParameter( b1 ) } );

  tcop(
    { TC131::OpCode::JGE, TC131::OperationFormat::DDL_1,
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_LabelParameter( b1 ) } );

  tcop(
    { TC131::OpCode::JGE_U, TC131::OperationFormat::DC4L_2,
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new TC_Const4_Unsigned( 7 ),
      new WIR_LabelParameter( b1 ) } );

  tcop(
    { TC131::OpCode::JGE_U, TC131::OperationFormat::DDL_1,
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_LabelParameter( b1 ) } );

  tcop(
    { TC131::OpCode::JGEZ, TC131::OperationFormat::SDL,
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_LabelParameter( b1 ) } );

  tcop(
    { TC131::OpCode::JGTZ, TC131::OperationFormat::SDL,
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_LabelParameter( b1 ) } );

  tcop(
    { TC131::OpCode::JI, TC131::OperationFormat::A,
      new WIR_RegisterParameter( a1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::JI, TC131::OperationFormat::SA,
      new WIR_RegisterParameter( a1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::JL, TC131::OperationFormat::L,
      new WIR_LabelParameter( f ) } );

  tcop(
    { TC131::OpCode::JLA, TC131::OperationFormat::L,
      new WIR_LabelParameter( f ) } );

  tcop(
    { TC131::OpCode::JLEZ, TC131::OperationFormat::SDL,
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_LabelParameter( b1 ) } );

  tcop(
    { TC131::OpCode::JLI, TC131::OperationFormat::A,
      new WIR_RegisterParameter( a1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::JLT, TC131::OperationFormat::DC4L_1,
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new TC_Const4_Signed( 3 ),
      new WIR_LabelParameter( b1 ) } );

  tcop(
    { TC131::OpCode::JLT, TC131::OperationFormat::DDL_1,
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_LabelParameter( b1 ) } );

  tcop(
    { TC131::OpCode::JLT_U, TC131::OperationFormat::DC4L_2,
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new TC_Const4_Unsigned( 7 ),
      new WIR_LabelParameter( b1 ) } );

  tcop(
    { TC131::OpCode::JLT_U, TC131::OperationFormat::DDL_1,
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_LabelParameter( b1 ) } );

  tcop(
    { TC131::OpCode::JLTZ, TC131::OperationFormat::SDL,
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_LabelParameter( b1 ) } );

  tcop(
    { TC131::OpCode::JNE, TC131::OperationFormat::DC4L_1,
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new TC_Const4_Signed( 3 ),
      new WIR_LabelParameter( b1 ) } );

  tcop(
    { TC131::OpCode::JNE, TC131::OperationFormat::DDL_1,
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_LabelParameter( b1 ) } );

  tcop(
    { TC131::OpCode::JNE, TC131::OperationFormat::SIC4L,
      new WIR_RegisterParameter( p.D15(), WIR_Usage::use ),
      new TC_Const4_Signed( 3 ),
      new WIR_LabelParameter( b1 ) } );

  tcop(
    { TC131::OpCode::JNE, TC131::OperationFormat::SIDL,
      new WIR_RegisterParameter( p.D15(), WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_LabelParameter( b1 ) } );

  tcop(
    { TC131::OpCode::JNE_A, TC131::OperationFormat::AAL,
      new WIR_RegisterParameter( a1, WIR_Usage::use ),
      new WIR_RegisterParameter( a2, WIR_Usage::use ),
      new WIR_LabelParameter( b1 ) } );

  tcop(
    { TC131::OpCode::JNED, TC131::OperationFormat::DC4L_3,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new TC_Const4_Signed( 3 ),
      new WIR_LabelParameter( b1 ) } );

  tcop(
    { TC131::OpCode::JNED, TC131::OperationFormat::DDL_2,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_LabelParameter( b1 ) } );

  tcop(
    { TC131::OpCode::JNEI, TC131::OperationFormat::DC4L_3,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new TC_Const4_Signed( 3 ),
      new WIR_LabelParameter( b1 ) } );

  tcop(
    { TC131::OpCode::JNEI, TC131::OperationFormat::DDL_2,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_LabelParameter( b1 ) } );

  tcop(
    { TC131::OpCode::JNZ, TC131::OperationFormat::SDL,
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_LabelParameter( b1 ) } );

  tcop(
    { TC131::OpCode::JNZ, TC131::OperationFormat::SIL,
      new WIR_RegisterParameter( p.D15(), WIR_Usage::use ),
      new WIR_LabelParameter( b1 ) } );

  tcop(
    { TC131::OpCode::JNZ_A, TC131::OperationFormat::AL_2,
      new WIR_RegisterParameter( a1, WIR_Usage::use ),
      new WIR_LabelParameter( b1 ) } );

  tcop(
    { TC131::OpCode::JNZ_A, TC131::OperationFormat::SAL_1,
      new WIR_RegisterParameter( a1, WIR_Usage::use ),
      new WIR_LabelParameter( b1 ) } );

  tcop(
    { TC131::OpCode::JNZ_T, TC131::OperationFormat::DC5L,
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new TC_Const5_Unsigned( 13 ),
      new WIR_LabelParameter( b1 ) } );

  tcop(
    { TC131::OpCode::JNZ_T, TC131::OperationFormat::SIC5L,
      new WIR_RegisterParameter( p.D15(), WIR_Usage::use ),
      new TC_Const5_Unsigned( 13 ),
      new WIR_LabelParameter( b1 ) } );

  tcop(
    { TC131::OpCode::JZ, TC131::OperationFormat::SDL,
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_LabelParameter( b1 ) } );

  tcop(
    { TC131::OpCode::JZ, TC131::OperationFormat::SIL,
      new WIR_RegisterParameter( p.D15(), WIR_Usage::use ),
      new WIR_LabelParameter( b1 ) } );

  tcop(
    { TC131::OpCode::JZ_A, TC131::OperationFormat::AL_2,
      new WIR_RegisterParameter( a1, WIR_Usage::use ),
      new WIR_LabelParameter( b1 ) } );

  tcop(
    { TC131::OpCode::JZ_A, TC131::OperationFormat::SAL_1,
      new WIR_RegisterParameter( a1, WIR_Usage::use ),
      new WIR_LabelParameter( b1 ) } );

  tcop(
    { TC131::OpCode::JZ_T, TC131::OperationFormat::DC5L,
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new TC_Const5_Unsigned( 13 ),
      new WIR_LabelParameter( b1 ) } );

  tcop(
    { TC131::OpCode::JZ_T, TC131::OperationFormat::SIC5L,
      new WIR_RegisterParameter( p.D15(), WIR_Usage::use ),
      new TC_Const5_Unsigned( 13 ),
      new WIR_LabelParameter( b1 ) } );

  tcop(
    { TC131::OpCode::LD_A, TC131::OperationFormat::AC18ABSA,
      new WIR_RegisterParameter( a1, WIR_Usage::def ),
      new TC_Const18_Unsigned( 16383 ) } );

  tcop(
    { TC131::OpCode::LD_A, TC131::OperationFormat::ALABSA,
      new WIR_RegisterParameter( a1, WIR_Usage::def ),
      new WIR_LabelParameter( b1 ) } );

  tcop(
    { TC131::OpCode::LD_A, TC131::OperationFormat::AAC10BOA,
      new WIR_RegisterParameter( a1, WIR_Usage::def ),
      new WIR_RegisterParameter( a2, WIR_Usage::use ),
      new TC_Const10_Signed( -512 ) } );

  tcop(
    { TC131::OpCode::LD_A, TC131::OperationFormat::APBRA,
      new WIR_RegisterParameter( a1, WIR_Usage::def ),
      new WIR_RegisterParameter( p1, WIR_Usage::defuse ) } );

  tcop(
    { TC131::OpCode::LD_A, TC131::OperationFormat::APC10CA,
      new WIR_RegisterParameter( a1, WIR_Usage::def ),
      new WIR_RegisterParameter( p1, WIR_Usage::defuse ),
      new TC_Const10_Signed( -512 ) } );

  tcop(
    { TC131::OpCode::LD_A, TC131::OperationFormat::AAC10PIA,
      new WIR_RegisterParameter( a1, WIR_Usage::def ),
      new WIR_AddressingModeParameter( TC131::AddressingMode::post ),
      new WIR_RegisterParameter( a2, WIR_Usage::defuse ),
      new TC_Const10_Signed( -512 ) } );

  tcop(
    { TC131::OpCode::LD_A, TC131::OperationFormat::AAC10PIA,
      new WIR_RegisterParameter( a1, WIR_Usage::def ),
      new WIR_AddressingModeParameter( TC131::AddressingMode::pre ),
      new WIR_RegisterParameter( a2, WIR_Usage::defuse ),
      new TC_Const10_Signed( -512 ) } );

  tcop(
    { TC131::OpCode::LD_A, TC131::OperationFormat::AAC16BOA,
      new WIR_RegisterParameter( a1, WIR_Usage::def ),
      new WIR_RegisterParameter( a2, WIR_Usage::use ),
      new TC_Const16_Signed( -32768 ) } );

  tcop(
    { TC131::OpCode::LD_A, TC131::OperationFormat::AALC16BOA,
      new WIR_RegisterParameter( a1, WIR_Usage::def ),
      new WIR_RegisterParameter( a2, WIR_Usage::use ),
      new WIR_LabelParameter( b1 ),
      new TC_Const16_Signed( 0 ) } );

  tcop(
    { TC131::OpCode::LD_A, TC131::OperationFormat::SISPC10_1,
      new WIR_RegisterParameter( p.A15(), WIR_Usage::def ),
      new WIR_RegisterParameter( p.SP(), WIR_Usage::use ),
      new TC_Const10_Unsigned( 252 ) } );

  tcop(
    { TC131::OpCode::LD_A, TC131::OperationFormat::SAA_2,
      new WIR_RegisterParameter( a1, WIR_Usage::def ),
      new WIR_RegisterParameter( a2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::LD_A, TC131::OperationFormat::SAA_3,
      new WIR_RegisterParameter( a1, WIR_Usage::def ),
      new WIR_RegisterParameter( a2, WIR_Usage::defuse ) } );

  tcop(
    { TC131::OpCode::LD_A, TC131::OperationFormat::SAIC4,
      new WIR_RegisterParameter( a1, WIR_Usage::def ),
      new WIR_RegisterParameter( p.A15(), WIR_Usage::use ),
      new TC_Const4_Unsigned( 12 ) } );

  tcop(
    { TC131::OpCode::LD_A, TC131::OperationFormat::SIAC4_1,
      new WIR_RegisterParameter( p.A15(), WIR_Usage::def ),
      new WIR_RegisterParameter( a1, WIR_Usage::use ),
      new TC_Const4_Unsigned( 12 ) } );

  tcop(
    { TC131::OpCode::LD_B, TC131::OperationFormat::DC18ABSA,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new TC_Const18_Unsigned( 16383 ) } );

  tcop(
    { TC131::OpCode::LD_B, TC131::OperationFormat::DLABSA,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_LabelParameter( b1 ) } );

  tcop(
    { TC131::OpCode::LD_B, TC131::OperationFormat::DAC10BOA,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( a1, WIR_Usage::use ),
      new TC_Const10_Signed( -512 ) } );

  tcop(
    { TC131::OpCode::LD_B, TC131::OperationFormat::DPBRA,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( p1, WIR_Usage::defuse ) } );

  tcop(
    { TC131::OpCode::LD_B, TC131::OperationFormat::DPC10CA,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( p1, WIR_Usage::defuse ),
      new TC_Const10_Signed( -512 ) } );

  tcop(
    { TC131::OpCode::LD_B, TC131::OperationFormat::DAC10PIA,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_AddressingModeParameter( TC131::AddressingMode::post ),
      new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
      new TC_Const10_Signed( -512 ) } );

  tcop(
    { TC131::OpCode::LD_B, TC131::OperationFormat::DAC10PIA,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_AddressingModeParameter( TC131::AddressingMode::pre ),
      new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
      new TC_Const10_Signed( -512 ) } );

  tcop(
    { TC131::OpCode::LD_BU, TC131::OperationFormat::DC18ABSA,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new TC_Const18_Unsigned( 16383 ) } );

  tcop(
    { TC131::OpCode::LD_BU, TC131::OperationFormat::DLABSA,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_LabelParameter( b1 ) } );

  tcop(
    { TC131::OpCode::LD_BU, TC131::OperationFormat::DAC10BOA,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( a1, WIR_Usage::use ),
      new TC_Const10_Signed( -512 ) } );

  tcop(
    { TC131::OpCode::LD_BU, TC131::OperationFormat::DPBRA,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( p1, WIR_Usage::defuse ) } );

  tcop(
    { TC131::OpCode::LD_BU, TC131::OperationFormat::DPC10CA,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( p1, WIR_Usage::defuse ),
      new TC_Const10_Signed( -512 ) } );

  tcop(
    { TC131::OpCode::LD_BU, TC131::OperationFormat::DAC10PIA,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_AddressingModeParameter( TC131::AddressingMode::post ),
      new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
      new TC_Const10_Signed( -512 ) } );

  tcop(
    { TC131::OpCode::LD_BU, TC131::OperationFormat::DAC10PIA,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_AddressingModeParameter( TC131::AddressingMode::pre ),
      new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
      new TC_Const10_Signed( -512 ) } );

  tcop(
    { TC131::OpCode::LD_BU, TC131::OperationFormat::SDA_2,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( a1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::LD_BU, TC131::OperationFormat::SDA_3,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( a1, WIR_Usage::defuse ) } );

  tcop(
    { TC131::OpCode::LD_BU, TC131::OperationFormat::SDIC4_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( p.A15(), WIR_Usage::use ),
      new TC_Const4_Unsigned( 7 ) } );

  tcop(
    { TC131::OpCode::LD_BU, TC131::OperationFormat::SIAC4_2,
      new WIR_RegisterParameter( p.D15(), WIR_Usage::def ),
      new WIR_RegisterParameter( a1, WIR_Usage::use ),
      new TC_Const4_Unsigned( 7 ) } );

  tcop(
    { TC131::OpCode::LD_D, TC131::OperationFormat::EC18ABSA,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new TC_Const18_Unsigned( 16383 ) } );

  tcop(
    { TC131::OpCode::LD_D, TC131::OperationFormat::ELABSA,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_LabelParameter( b1 ) } );

  tcop(
    { TC131::OpCode::LD_D, TC131::OperationFormat::EAC10BOA,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( a1, WIR_Usage::use ),
      new TC_Const10_Signed( -512 ) } );

  tcop(
    { TC131::OpCode::LD_D, TC131::OperationFormat::EPBRA,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( p1, WIR_Usage::defuse ) } );

  tcop(
    { TC131::OpCode::LD_D, TC131::OperationFormat::EPC10CA,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( p1, WIR_Usage::defuse ),
      new TC_Const10_Signed( -512 ) } );

  tcop(
    { TC131::OpCode::LD_D, TC131::OperationFormat::EAC10PIA,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_AddressingModeParameter( TC131::AddressingMode::post ),
      new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
      new TC_Const10_Signed( -512 ) } );

  tcop(
    { TC131::OpCode::LD_D, TC131::OperationFormat::EAC10PIA,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_AddressingModeParameter( TC131::AddressingMode::pre ),
      new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
      new TC_Const10_Signed( -512 ) } );

  tcop(
    { TC131::OpCode::LD_DA, TC131::OperationFormat::PC18ABSA,
      new WIR_RegisterParameter( p1, WIR_Usage::def ),
      new TC_Const18_Unsigned( 16383 ) } );

  tcop(
    { TC131::OpCode::LD_DA, TC131::OperationFormat::PLABSA,
      new WIR_RegisterParameter( p1, WIR_Usage::def ),
      new WIR_LabelParameter( b1 ) } );

  tcop(
    { TC131::OpCode::LD_DA, TC131::OperationFormat::PAC10BOA,
      new WIR_RegisterParameter( p1, WIR_Usage::def ),
      new WIR_RegisterParameter( a1, WIR_Usage::use ),
      new TC_Const10_Signed( -512 ) } );

  tcop(
    { TC131::OpCode::LD_DA, TC131::OperationFormat::PPBRA_1,
      new WIR_RegisterParameter( p1, WIR_Usage::def ),
      new WIR_RegisterParameter( p2, WIR_Usage::defuse ) } );

  tcop(
    { TC131::OpCode::LD_DA, TC131::OperationFormat::PPC10CA,
      new WIR_RegisterParameter( p1, WIR_Usage::def ),
      new WIR_RegisterParameter( p2, WIR_Usage::defuse ),
      new TC_Const10_Signed( -512 ) } );

  tcop(
    { TC131::OpCode::LD_DA, TC131::OperationFormat::PAC10PIA,
      new WIR_RegisterParameter( p1, WIR_Usage::def ),
      new WIR_AddressingModeParameter( TC131::AddressingMode::post ),
      new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
      new TC_Const10_Signed( -512 ) } );

  tcop(
    { TC131::OpCode::LD_DA, TC131::OperationFormat::PAC10PIA,
      new WIR_RegisterParameter( p1, WIR_Usage::def ),
      new WIR_AddressingModeParameter( TC131::AddressingMode::pre ),
      new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
      new TC_Const10_Signed( -512 ) } );

  tcop(
    { TC131::OpCode::LD_H, TC131::OperationFormat::DC18ABSA,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new TC_Const18_Unsigned( 16383 ) } );

  tcop(
    { TC131::OpCode::LD_H, TC131::OperationFormat::DLABSA,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_LabelParameter( b1 ) } );

  tcop(
    { TC131::OpCode::LD_H, TC131::OperationFormat::DAC10BOA,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( a1, WIR_Usage::use ),
      new TC_Const10_Signed( -512 ) } );

  tcop(
    { TC131::OpCode::LD_H, TC131::OperationFormat::DPBRA,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( p1, WIR_Usage::defuse ) } );

  tcop(
    { TC131::OpCode::LD_H, TC131::OperationFormat::DPC10CA,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( p1, WIR_Usage::defuse ),
      new TC_Const10_Signed( -512 ) } );

  tcop(
    { TC131::OpCode::LD_H, TC131::OperationFormat::DAC10PIA,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_AddressingModeParameter( TC131::AddressingMode::post ),
      new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
      new TC_Const10_Signed( -512 ) } );

  tcop(
    { TC131::OpCode::LD_H, TC131::OperationFormat::DAC10PIA,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_AddressingModeParameter( TC131::AddressingMode::pre ),
      new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
      new TC_Const10_Signed( -512 ) } );

  tcop(
    { TC131::OpCode::LD_H, TC131::OperationFormat::SDA_2,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( a1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::LD_H, TC131::OperationFormat::SDA_3,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( a1, WIR_Usage::defuse ) } );

  tcop(
    { TC131::OpCode::LD_H, TC131::OperationFormat::SDIC4_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( p.A15(), WIR_Usage::use ),
      new TC_Const4_Unsigned( 6 ) } );

  tcop(
    { TC131::OpCode::LD_H, TC131::OperationFormat::SIAC4_2,
      new WIR_RegisterParameter( p.D15(), WIR_Usage::def ),
      new WIR_RegisterParameter( a1, WIR_Usage::use ),
      new TC_Const4_Unsigned( 6 ) } );

  tcop(
    { TC131::OpCode::LD_HU, TC131::OperationFormat::DC18ABSA,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new TC_Const18_Unsigned( 16383 ) } );

  tcop(
    { TC131::OpCode::LD_HU, TC131::OperationFormat::DLABSA,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_LabelParameter( b1 ) } );

  tcop(
    { TC131::OpCode::LD_HU, TC131::OperationFormat::DAC10BOA,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( a1, WIR_Usage::use ),
      new TC_Const10_Signed( -512 ) } );

  tcop(
    { TC131::OpCode::LD_HU, TC131::OperationFormat::DPBRA,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( p1, WIR_Usage::defuse ) } );

  tcop(
    { TC131::OpCode::LD_HU, TC131::OperationFormat::DPC10CA,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( p1, WIR_Usage::defuse ),
      new TC_Const10_Signed( -512 ) } );

  tcop(
    { TC131::OpCode::LD_HU, TC131::OperationFormat::DAC10PIA,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_AddressingModeParameter( TC131::AddressingMode::post ),
      new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
      new TC_Const10_Signed( -512 ) } );

  tcop(
    { TC131::OpCode::LD_HU, TC131::OperationFormat::DAC10PIA,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_AddressingModeParameter( TC131::AddressingMode::pre ),
      new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
      new TC_Const10_Signed( -512 ) } );

  tcop(
    { TC131::OpCode::LD_Q, TC131::OperationFormat::DC18ABSA,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new TC_Const18_Unsigned( 16383 ) } );

  tcop(
    { TC131::OpCode::LD_Q, TC131::OperationFormat::DLABSA,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_LabelParameter( b1 ) } );

  tcop(
    { TC131::OpCode::LD_Q, TC131::OperationFormat::DAC10BOA,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( a1, WIR_Usage::use ),
      new TC_Const10_Signed( -512 ) } );

  tcop(
    { TC131::OpCode::LD_Q, TC131::OperationFormat::DPBRA,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( p1, WIR_Usage::defuse ) } );

  tcop(
    { TC131::OpCode::LD_Q, TC131::OperationFormat::DPC10CA,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( p1, WIR_Usage::defuse ),
      new TC_Const10_Signed( -512 ) } );

  tcop(
    { TC131::OpCode::LD_Q, TC131::OperationFormat::DAC10PIA,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_AddressingModeParameter( TC131::AddressingMode::post ),
      new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
      new TC_Const10_Signed( -512 ) } );

  tcop(
    { TC131::OpCode::LD_Q, TC131::OperationFormat::DAC10PIA,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_AddressingModeParameter( TC131::AddressingMode::pre ),
      new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
      new TC_Const10_Signed( -512 ) } );

  tcop(
    { TC131::OpCode::LD_W, TC131::OperationFormat::DC18ABSA,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new TC_Const18_Unsigned( 16383 ) } );

  tcop(
    { TC131::OpCode::LD_W, TC131::OperationFormat::DLABSA,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_LabelParameter( b1 ) } );

  tcop(
    { TC131::OpCode::LD_W, TC131::OperationFormat::DAC10BOA,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( a1, WIR_Usage::use ),
      new TC_Const10_Signed( -512 ) } );

  tcop(
    { TC131::OpCode::LD_W, TC131::OperationFormat::DPBRA,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( p1, WIR_Usage::defuse ) } );

  tcop(
    { TC131::OpCode::LD_W, TC131::OperationFormat::DPC10CA,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( p1, WIR_Usage::defuse ),
      new TC_Const10_Signed( -512 ) } );

  tcop(
    { TC131::OpCode::LD_W, TC131::OperationFormat::DAC10PIA,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_AddressingModeParameter( TC131::AddressingMode::post ),
      new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
      new TC_Const10_Signed( -512 ) } );

  tcop(
    { TC131::OpCode::LD_W, TC131::OperationFormat::DAC10PIA,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_AddressingModeParameter( TC131::AddressingMode::pre ),
      new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
      new TC_Const10_Signed( -512 ) } );

  tcop(
    { TC131::OpCode::LD_W, TC131::OperationFormat::DAC16BOA,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( a1, WIR_Usage::use ),
      new TC_Const16_Signed( -32768 ) } );

  tcop(
    { TC131::OpCode::LD_W, TC131::OperationFormat::DALC16BOA,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( a1, WIR_Usage::use ),
      new WIR_LabelParameter( b1 ),
      new TC_Const16_Signed( 0 ) } );

  tcop(
    { TC131::OpCode::LD_W, TC131::OperationFormat::SISPC10_2,
      new WIR_RegisterParameter( p.D15(), WIR_Usage::def ),
      new WIR_RegisterParameter( p.SP(), WIR_Usage::use ),
      new TC_Const10_Unsigned( 252 ) } );

  tcop(
    { TC131::OpCode::LD_W, TC131::OperationFormat::SDA_2,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( a1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::LD_W, TC131::OperationFormat::SDA_3,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( a1, WIR_Usage::defuse ) } );

  tcop(
    { TC131::OpCode::LD_W, TC131::OperationFormat::SDIC4_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( p.A15(), WIR_Usage::use ),
      new TC_Const4_Unsigned( 4 ) } );

  tcop(
    { TC131::OpCode::LD_W, TC131::OperationFormat::SIAC4_2,
      new WIR_RegisterParameter( p.D15(), WIR_Usage::def ),
      new WIR_RegisterParameter( a1, WIR_Usage::use ),
      new TC_Const4_Unsigned( 4 ) } );

  tcop(
    { TC131::OpCode::LDLCX, TC131::OperationFormat::C18ABSA,
      new TC_Const18_Unsigned( 16383 ) } );

  tcop(
    { TC131::OpCode::LDLCX, TC131::OperationFormat::LABSA,
      new WIR_LabelParameter( b1 ) } );

  tcop(
    { TC131::OpCode::LDLCX, TC131::OperationFormat::AC10BOA,
      new WIR_RegisterParameter( a1, WIR_Usage::use ),
      new TC_Const10_Signed( -486 ) } );

  tcop(
    { TC131::OpCode::LDMST, TC131::OperationFormat::C18EABSA,
      new TC_Const18_Unsigned( 16383 ),
      new WIR_RegisterParameter( e1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::LDMST, TC131::OperationFormat::LEABSA,
      new WIR_LabelParameter( b1 ),
      new WIR_RegisterParameter( e1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::LDMST, TC131::OperationFormat::AC10EBOA,
      new WIR_RegisterParameter( a1, WIR_Usage::use ),
      new TC_Const10_Signed( -512 ),
      new WIR_RegisterParameter( e1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::LDMST, TC131::OperationFormat::PEBRA,
      new WIR_RegisterParameter( p1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( e1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::LDMST, TC131::OperationFormat::PC10ECA,
      new WIR_RegisterParameter( p1, WIR_Usage::defuse ),
      new TC_Const10_Signed( -512 ),
      new WIR_RegisterParameter( e1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::LDMST, TC131::OperationFormat::AC10EPIA,
      new WIR_AddressingModeParameter( TC131::AddressingMode::post ),
      new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
      new TC_Const10_Signed( -512 ),
      new WIR_RegisterParameter( e1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::LDMST, TC131::OperationFormat::AC10EPIA,
      new WIR_AddressingModeParameter( TC131::AddressingMode::pre ),
      new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
      new TC_Const10_Signed( -512 ),
      new WIR_RegisterParameter( e1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::LDUCX, TC131::OperationFormat::C18ABSA,
      new TC_Const18_Unsigned( 16383 ) } );

  tcop(
    { TC131::OpCode::LDUCX, TC131::OperationFormat::LABSA,
      new WIR_LabelParameter( b1 ) } );

  tcop(
    { TC131::OpCode::LDUCX, TC131::OperationFormat::AC10BOA,
      new WIR_RegisterParameter( a1, WIR_Usage::use ),
      new TC_Const10_Signed( -486 ) } );

  tcop(
    { TC131::OpCode::LEA, TC131::OperationFormat::AC18ABSA,
      new WIR_RegisterParameter( a1, WIR_Usage::def ),
      new TC_Const18_Unsigned( 16383 ) } );

  tcop(
    { TC131::OpCode::LEA, TC131::OperationFormat::ALABSA,
      new WIR_RegisterParameter( a1, WIR_Usage::def ),
      new WIR_LabelParameter( b1 ) } );

  tcop(
    { TC131::OpCode::LEA, TC131::OperationFormat::AAC10BOA,
      new WIR_RegisterParameter( a1, WIR_Usage::def ),
      new WIR_RegisterParameter( a2, WIR_Usage::use ),
      new TC_Const10_Signed( -512 ) } );

  tcop(
    { TC131::OpCode::LEA, TC131::OperationFormat::AAC16BOA,
      new WIR_RegisterParameter( a1, WIR_Usage::def ),
      new WIR_RegisterParameter( a2, WIR_Usage::use ),
      new TC_Const16_Signed( -32768 ) } );

  tcop(
    { TC131::OpCode::LEA, TC131::OperationFormat::AALC16BOA,
      new WIR_RegisterParameter( a1, WIR_Usage::def ),
      new WIR_RegisterParameter( a2, WIR_Usage::use ),
      new WIR_LabelParameter( b1 ),
      new TC_Const16_Signed( 0 ) } );

  tcop(
    { TC131::OpCode::LOOP, TC131::OperationFormat::AL_3,
      new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
      new WIR_LabelParameter( b1 ) } );

  tcop(
    { TC131::OpCode::LOOP, TC131::OperationFormat::SAL_2,
      new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
      new WIR_LabelParameter( b1 ) } );

  tcop(
    { TC131::OpCode::LOOPU, TC131::OperationFormat::L,
      new WIR_LabelParameter( b1 ) } );

  tcop(
    { TC131::OpCode::LT, TC131::OperationFormat::DDC9_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const9_Signed( -128 ) } );

  tcop(
    { TC131::OpCode::LT, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::LT, TC131::OperationFormat::SIDC4,
      new WIR_RegisterParameter( p.D15(), WIR_Usage::def ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new TC_Const4_Signed( -5 ) } );

  tcop(
    { TC131::OpCode::LT, TC131::OperationFormat::SIDD,
      new WIR_RegisterParameter( p.D15(), WIR_Usage::def ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::LT_A, TC131::OperationFormat::DAA,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( a1, WIR_Usage::use ),
      new WIR_RegisterParameter( a2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::LT_B, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::LT_BU, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::LT_H, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::LT_HU, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::LT_U, TC131::OperationFormat::DDC9_2,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const9_Unsigned( 128 ) } );

  tcop(
    { TC131::OpCode::LT_U, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::LT_W, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::LT_WU, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MADD, TC131::OperationFormat::DDDC9_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new TC_Const9_Signed( -128 ) } );

  tcop(
    { TC131::OpCode::MADD, TC131::OperationFormat::EEDC9_1,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new TC_Const9_Signed( -128 ) } );

  tcop(
    { TC131::OpCode::MADD, TC131::OperationFormat::DDDD,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MADD, TC131::OperationFormat::EEDD,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MADD_F, TC131::OperationFormat::DDDD,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MADD_H, TC131::OperationFormat::EEDDC1_3,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADD_H, TC131::OperationFormat::EEDDC1_4,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADD_H, TC131::OperationFormat::EEDDC1_6,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADD_H, TC131::OperationFormat::EEDDC1_7,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADD_Q, TC131::OperationFormat::DDDDC1_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADD_Q, TC131::OperationFormat::EEDDC1_1,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADD_Q, TC131::OperationFormat::DDDDC1_2,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADD_Q, TC131::OperationFormat::EEDDC1_2,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADD_Q, TC131::OperationFormat::DDDDC1_5,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADD_Q, TC131::OperationFormat::EEDDC1_5,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADD_Q, TC131::OperationFormat::DDDDC1_8,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADD_Q, TC131::OperationFormat::EEDDC1_8,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADD_Q, TC131::OperationFormat::DDDDC1_9,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADD_Q, TC131::OperationFormat::EEDDC1_9,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADD_U, TC131::OperationFormat::EEDC9_2,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new TC_Const9_Unsigned( 234 ) } );

  tcop(
    { TC131::OpCode::MADD_U, TC131::OperationFormat::EEDD,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MADDM_H, TC131::OperationFormat::EEDDC1_3,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADDM_H, TC131::OperationFormat::EEDDC1_4,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADDM_H, TC131::OperationFormat::EEDDC1_6,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADDM_H, TC131::OperationFormat::EEDDC1_7,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADDMS_H, TC131::OperationFormat::EEDDC1_3,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADDMS_H, TC131::OperationFormat::EEDDC1_4,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADDMS_H, TC131::OperationFormat::EEDDC1_6,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADDMS_H, TC131::OperationFormat::EEDDC1_7,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADDR_H, TC131::OperationFormat::DDDDC1_3,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADDR_H, TC131::OperationFormat::DDDDC1_4,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADDR_H, TC131::OperationFormat::DDDDC1_6,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADDR_H, TC131::OperationFormat::DEDDC1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( e1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADDR_H, TC131::OperationFormat::DDDDC1_7,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADDR_Q, TC131::OperationFormat::DDDDC1_8,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADDR_Q, TC131::OperationFormat::DDDDC1_9,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADDRS_H, TC131::OperationFormat::DDDDC1_3,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADDRS_H, TC131::OperationFormat::DDDDC1_4,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADDRS_H, TC131::OperationFormat::DDDDC1_6,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADDRS_H, TC131::OperationFormat::DEDDC1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( e1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADDRS_H, TC131::OperationFormat::DDDDC1_7,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADDRS_Q, TC131::OperationFormat::DDDDC1_8,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADDRS_Q, TC131::OperationFormat::DDDDC1_9,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADDS, TC131::OperationFormat::DDDC9_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new TC_Const9_Signed( -128 ) } );

  tcop(
    { TC131::OpCode::MADDS, TC131::OperationFormat::EEDC9_1,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new TC_Const9_Signed( -128 ) } );

  tcop(
    { TC131::OpCode::MADDS, TC131::OperationFormat::DDDD,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MADDS, TC131::OperationFormat::EEDD,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MADDS_H, TC131::OperationFormat::EEDDC1_3,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADDS_H, TC131::OperationFormat::EEDDC1_4,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADDS_H, TC131::OperationFormat::EEDDC1_6,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADDS_H, TC131::OperationFormat::EEDDC1_7,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADDS_Q, TC131::OperationFormat::DDDDC1_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADDS_Q, TC131::OperationFormat::EEDDC1_1,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADDS_Q, TC131::OperationFormat::DDDDC1_2,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADDS_Q, TC131::OperationFormat::EEDDC1_2,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADDS_Q, TC131::OperationFormat::DDDDC1_5,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADDS_Q, TC131::OperationFormat::EEDDC1_5,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADDS_Q, TC131::OperationFormat::DDDDC1_8,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADDS_Q, TC131::OperationFormat::EEDDC1_8,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADDS_Q, TC131::OperationFormat::DDDDC1_9,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADDS_Q, TC131::OperationFormat::EEDDC1_9,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADDS_U, TC131::OperationFormat::DDDC9_2,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new TC_Const9_Unsigned( 234 ) } );

  tcop(
    { TC131::OpCode::MADDS_U, TC131::OperationFormat::EEDC9_2,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new TC_Const9_Unsigned( 234 ) } );

  tcop(
    { TC131::OpCode::MADDS_U, TC131::OperationFormat::DDDD,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MADDS_U, TC131::OperationFormat::EEDD,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MADDSU_H, TC131::OperationFormat::EEDDC1_3,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADDSU_H, TC131::OperationFormat::EEDDC1_4,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADDSU_H, TC131::OperationFormat::EEDDC1_6,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADDSU_H, TC131::OperationFormat::EEDDC1_7,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADDSUM_H, TC131::OperationFormat::EEDDC1_3,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADDSUM_H, TC131::OperationFormat::EEDDC1_4,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADDSUM_H, TC131::OperationFormat::EEDDC1_6,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADDSUM_H, TC131::OperationFormat::EEDDC1_7,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADDSUMS_H, TC131::OperationFormat::EEDDC1_3,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADDSUMS_H, TC131::OperationFormat::EEDDC1_4,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADDSUMS_H, TC131::OperationFormat::EEDDC1_6,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADDSUMS_H, TC131::OperationFormat::EEDDC1_7,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADDSUR_H, TC131::OperationFormat::DDDDC1_3,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADDSUR_H, TC131::OperationFormat::DDDDC1_4,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADDSUR_H, TC131::OperationFormat::DDDDC1_6,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADDSUR_H, TC131::OperationFormat::DDDDC1_7,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADDSURS_H, TC131::OperationFormat::DDDDC1_3,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADDSURS_H, TC131::OperationFormat::DDDDC1_4,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADDSURS_H, TC131::OperationFormat::DDDDC1_6,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADDSURS_H, TC131::OperationFormat::DDDDC1_7,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADDSUS_H, TC131::OperationFormat::EEDDC1_3,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADDSUS_H, TC131::OperationFormat::EEDDC1_4,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADDSUS_H, TC131::OperationFormat::EEDDC1_6,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MADDSUS_H, TC131::OperationFormat::EEDDC1_7,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MAX, TC131::OperationFormat::DDC9_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const9_Signed( -128 ) } );

  tcop(
    { TC131::OpCode::MAX, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MAX_B, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MAX_BU, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MAX_H, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MAX_HU, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MAX_U, TC131::OperationFormat::DDC9_2,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const9_Unsigned( 128 ) } );

  tcop(
    { TC131::OpCode::MAX_U, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MFCR, TC131::OperationFormat::DC16PSW,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new TC_Const16_Unsigned( 26361 ),
      new WIR_RegisterParameter( pswC, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MIN, TC131::OperationFormat::DDC9_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const9_Signed( -128 ) } );

  tcop(
    { TC131::OpCode::MIN, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MIN_B, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MIN_BU, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MIN_H, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MIN_HU, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MIN_U, TC131::OperationFormat::DDC9_2,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const9_Unsigned( 128 ) } );

  tcop(
    { TC131::OpCode::MIN_U, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new TC_Const16_Signed( -26361 ) } );

  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::SIC8_1,
      new WIR_RegisterParameter( p.D15(), WIR_Usage::def ),
      new TC_Const8_Unsigned( 64 ) } );

  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::SDC4_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new TC_Const4_Signed( 5 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::SDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_A, TC131::OperationFormat::AD,
      new WIR_RegisterParameter( a1, WIR_Usage::def ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_A, TC131::OperationFormat::SAC4_1,
      new WIR_RegisterParameter( a1, WIR_Usage::def ),
      new TC_Const4_Unsigned( 7 ) } );

  tcop(
    { TC131::OpCode::MOV_A, TC131::OperationFormat::SAD_1,
      new WIR_RegisterParameter( a1, WIR_Usage::def ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_AA, TC131::OperationFormat::AA,
      new WIR_RegisterParameter( a1, WIR_Usage::def ),
      new WIR_RegisterParameter( a2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_AA, TC131::OperationFormat::SAA_1,
      new WIR_RegisterParameter( a1, WIR_Usage::def ),
      new WIR_RegisterParameter( a2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_D, TC131::OperationFormat::DA,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( a1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_D, TC131::OperationFormat::SDA_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( a1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_U, TC131::OperationFormat::DC16_2,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new TC_Const16_Unsigned( 26361 ) } );

  tcop(
    { TC131::OpCode::MOVH, TC131::OperationFormat::DC16_2,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new TC_Const16_Unsigned( 26361 ) } );

  tcop(
    { TC131::OpCode::MOVH_A, TC131::OperationFormat::AC16,
      new WIR_RegisterParameter( a1, WIR_Usage::def ),
      new TC_Const16_Unsigned( 26361 ) } );

  tcop(
    { TC131::OpCode::MOVH_A, TC131::OperationFormat::AL_1,
      new WIR_RegisterParameter( a1, WIR_Usage::def ),
      new WIR_LabelParameter( b1 ) } );

  tcop(
    { TC131::OpCode::MSUB, TC131::OperationFormat::DDDC9_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new TC_Const9_Signed( -128 ) } );

  tcop(
    { TC131::OpCode::MSUB, TC131::OperationFormat::EEDC9_1,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new TC_Const9_Signed( -128 ) } );

  tcop(
    { TC131::OpCode::MSUB, TC131::OperationFormat::DDDD,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MSUB, TC131::OperationFormat::EEDD,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MSUB_F, TC131::OperationFormat::DDDD,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MSUB_H, TC131::OperationFormat::EEDDC1_3,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUB_H, TC131::OperationFormat::EEDDC1_4,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUB_H, TC131::OperationFormat::EEDDC1_6,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUB_H, TC131::OperationFormat::EEDDC1_7,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUB_Q, TC131::OperationFormat::DDDDC1_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUB_Q, TC131::OperationFormat::EEDDC1_1,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUB_Q, TC131::OperationFormat::DDDDC1_2,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUB_Q, TC131::OperationFormat::EEDDC1_2,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUB_Q, TC131::OperationFormat::DDDDC1_5,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUB_Q, TC131::OperationFormat::EEDDC1_5,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUB_Q, TC131::OperationFormat::DDDDC1_8,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUB_Q, TC131::OperationFormat::EEDDC1_8,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUB_Q, TC131::OperationFormat::DDDDC1_9,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUB_Q, TC131::OperationFormat::EEDDC1_9,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUB_U, TC131::OperationFormat::EEDC9_2,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new TC_Const9_Unsigned( 234 ) } );

  tcop(
    { TC131::OpCode::MSUB_U, TC131::OperationFormat::EEDD,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MSUBAD_H, TC131::OperationFormat::EEDDC1_3,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUBAD_H, TC131::OperationFormat::EEDDC1_4,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUBAD_H, TC131::OperationFormat::EEDDC1_6,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUBAD_H, TC131::OperationFormat::EEDDC1_7,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUBADM_H, TC131::OperationFormat::EEDDC1_3,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUBADM_H, TC131::OperationFormat::EEDDC1_4,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUBADM_H, TC131::OperationFormat::EEDDC1_6,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUBADM_H, TC131::OperationFormat::EEDDC1_7,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUBADMS_H, TC131::OperationFormat::EEDDC1_3,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUBADMS_H, TC131::OperationFormat::EEDDC1_4,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUBADMS_H, TC131::OperationFormat::EEDDC1_6,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUBADMS_H, TC131::OperationFormat::EEDDC1_7,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUBADR_H, TC131::OperationFormat::DDDDC1_3,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUBADR_H, TC131::OperationFormat::DDDDC1_4,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUBADR_H, TC131::OperationFormat::DDDDC1_6,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUBADR_H, TC131::OperationFormat::DDDDC1_7,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUBADRS_H, TC131::OperationFormat::DDDDC1_3,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUBADRS_H, TC131::OperationFormat::DDDDC1_4,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUBADRS_H, TC131::OperationFormat::DDDDC1_6,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUBADRS_H, TC131::OperationFormat::DDDDC1_7,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUBADS_H, TC131::OperationFormat::EEDDC1_3,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUBADS_H, TC131::OperationFormat::EEDDC1_4,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUBADS_H, TC131::OperationFormat::EEDDC1_6,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUBADS_H, TC131::OperationFormat::EEDDC1_7,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUBM_H, TC131::OperationFormat::EEDDC1_3,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUBM_H, TC131::OperationFormat::EEDDC1_4,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUBM_H, TC131::OperationFormat::EEDDC1_6,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUBM_H, TC131::OperationFormat::EEDDC1_7,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUBMS_H, TC131::OperationFormat::EEDDC1_3,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUBMS_H, TC131::OperationFormat::EEDDC1_4,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUBMS_H, TC131::OperationFormat::EEDDC1_6,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUBMS_H, TC131::OperationFormat::EEDDC1_7,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUBR_H, TC131::OperationFormat::DDDDC1_3,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUBR_H, TC131::OperationFormat::DDDDC1_4,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUBR_H, TC131::OperationFormat::DDDDC1_6,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUBR_H, TC131::OperationFormat::DEDDC1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( e1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUBR_H, TC131::OperationFormat::DDDDC1_7,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUBR_Q, TC131::OperationFormat::DDDDC1_8,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUBR_Q, TC131::OperationFormat::DDDDC1_9,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUBRS_H, TC131::OperationFormat::DDDDC1_3,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUBRS_H, TC131::OperationFormat::DDDDC1_4,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUBRS_H, TC131::OperationFormat::DDDDC1_6,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUBRS_H, TC131::OperationFormat::DEDDC1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( e1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUBRS_H, TC131::OperationFormat::DDDDC1_7,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUBRS_Q, TC131::OperationFormat::DDDDC1_8,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUBRS_Q, TC131::OperationFormat::DDDDC1_9,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUBS, TC131::OperationFormat::DDDC9_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new TC_Const9_Signed( -128 ) } );

  tcop(
    { TC131::OpCode::MSUBS, TC131::OperationFormat::EEDC9_1,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new TC_Const9_Signed( -128 ) } );

  tcop(
    { TC131::OpCode::MSUBS, TC131::OperationFormat::DDDD,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MSUBS, TC131::OperationFormat::EEDD,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MSUBS_H, TC131::OperationFormat::EEDDC1_3,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUBS_H, TC131::OperationFormat::EEDDC1_4,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUBS_H, TC131::OperationFormat::EEDDC1_6,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUBS_H, TC131::OperationFormat::EEDDC1_7,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUBS_Q, TC131::OperationFormat::DDDDC1_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUBS_Q, TC131::OperationFormat::EEDDC1_1,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUBS_Q, TC131::OperationFormat::DDDDC1_2,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUBS_Q, TC131::OperationFormat::EEDDC1_2,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUBS_Q, TC131::OperationFormat::DDDDC1_5,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUBS_Q, TC131::OperationFormat::EEDDC1_5,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUBS_Q, TC131::OperationFormat::DDDDC1_8,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUBS_Q, TC131::OperationFormat::EEDDC1_8,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUBS_Q, TC131::OperationFormat::DDDDC1_9,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUBS_Q, TC131::OperationFormat::EEDDC1_9,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MSUBS_U, TC131::OperationFormat::DDDC9_2,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new TC_Const9_Unsigned( 234 ) } );

  tcop(
    { TC131::OpCode::MSUBS_U, TC131::OperationFormat::EEDC9_2,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new TC_Const9_Unsigned( 234 ) } );

  tcop(
    { TC131::OpCode::MSUBS_U, TC131::OperationFormat::DDDD,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MSUBS_U, TC131::OperationFormat::EEDD,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( e2, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MTCR, TC131::OperationFormat::C16DPSW,
      new TC_Const16_Unsigned( 26361 ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( pswC, WIR_Usage::def ) } );

  tcop(
    { TC131::OpCode::MUL, TC131::OperationFormat::DDC9_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const9_Signed( -128 ) } );

  tcop(
    { TC131::OpCode::MUL, TC131::OperationFormat::EDC9_1,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new TC_Const9_Signed( -128 ) } );

  tcop(
    { TC131::OpCode::MUL, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MUL, TC131::OperationFormat::EDD,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MUL, TC131::OperationFormat::SDD_2,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MUL_F, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MUL_H, TC131::OperationFormat::EDDC1_3,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MUL_H, TC131::OperationFormat::EDDC1_4,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MUL_H, TC131::OperationFormat::EDDC1_6,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MUL_H, TC131::OperationFormat::EDDC1_7,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MUL_Q, TC131::OperationFormat::DDDC1_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MUL_Q, TC131::OperationFormat::EDDC1_1,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MUL_Q, TC131::OperationFormat::DDDC1_2,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MUL_Q, TC131::OperationFormat::EDDC1_2,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MUL_Q, TC131::OperationFormat::DDDC1_5,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MUL_Q, TC131::OperationFormat::EDDC1_5,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MUL_Q, TC131::OperationFormat::DDDC1_8,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MUL_Q, TC131::OperationFormat::DDDC1_9,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MUL_U, TC131::OperationFormat::EDC9_2,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new TC_Const9_Unsigned( 128 ) } );

  tcop(
    { TC131::OpCode::MUL_U, TC131::OperationFormat::EDD,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MULM_H, TC131::OperationFormat::EDDC1_3,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MULM_H, TC131::OperationFormat::EDDC1_4,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MULM_H, TC131::OperationFormat::EDDC1_6,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MULM_H, TC131::OperationFormat::EDDC1_7,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MULR_H, TC131::OperationFormat::DDDC1_3,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MULR_H, TC131::OperationFormat::DDDC1_4,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MULR_H, TC131::OperationFormat::DDDC1_6,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MULR_H, TC131::OperationFormat::DDDC1_7,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MULR_Q, TC131::OperationFormat::DDDC1_8,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MULR_Q, TC131::OperationFormat::DDDC1_9,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new TC_Const1_Unsigned( 1 ) } );

  tcop(
    { TC131::OpCode::MULS, TC131::OperationFormat::DDC9_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const9_Signed( -128 ) } );

  tcop(
    { TC131::OpCode::MULS, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MULS_U, TC131::OperationFormat::DDC9_2,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const9_Unsigned( 128 ) } );

  tcop(
    { TC131::OpCode::MULS_U, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::NAND, TC131::OperationFormat::DDC9_2,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const9_Unsigned( 128 ) } );

  tcop(
    { TC131::OpCode::NAND, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::NAND_T, TC131::OperationFormat::DDC5DC5_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const5_Unsigned( 13 ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new TC_Const5_Unsigned( 27 ) } );

  tcop(
    { TC131::OpCode::NE, TC131::OperationFormat::DDC9_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const9_Signed( -128 ) } );

  tcop(
    { TC131::OpCode::NE, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::NE_A, TC131::OperationFormat::DAA,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( a1, WIR_Usage::use ),
      new WIR_RegisterParameter( a2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::NEZ_A, TC131::OperationFormat::DA,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( a1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::NOP, TC131::OperationFormat::SYS } );

  tcop(
    { TC131::OpCode::NOP, TC131::OperationFormat::S } );

  tcop(
    { TC131::OpCode::NOR, TC131::OperationFormat::DDC9_2,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const9_Unsigned( 128 ) } );

  tcop(
    { TC131::OpCode::NOR, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::NOR_T, TC131::OperationFormat::DDC5DC5_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const5_Unsigned( 13 ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new TC_Const5_Unsigned( 27 ) } );

  tcop(
    { TC131::OpCode::NOT, TC131::OperationFormat::SD,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ) } );

  tcop(
    { TC131::OpCode::OR, TC131::OperationFormat::DDC9_2,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const9_Unsigned( 128 ) } );

  tcop(
    { TC131::OpCode::OR, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::OR, TC131::OperationFormat::SIC8_2,
      new WIR_RegisterParameter( p.D15(), WIR_Usage::defuse ),
      new TC_Const8_Unsigned( 64 ) } );

  tcop(
    { TC131::OpCode::OR, TC131::OperationFormat::SDD_2,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::OR_AND_T, TC131::OperationFormat::DDC5DC5_2,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const5_Unsigned( 13 ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new TC_Const5_Unsigned( 27 ) } );

  tcop(
    { TC131::OpCode::OR_ANDN_T, TC131::OperationFormat::DDC5DC5_2,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const5_Unsigned( 13 ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new TC_Const5_Unsigned( 27 ) } );

  tcop(
    { TC131::OpCode::OR_EQ, TC131::OperationFormat::DDC9_3,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const9_Signed( -128 ) } );

  tcop(
    { TC131::OpCode::OR_EQ, TC131::OperationFormat::DDD_2,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::OR_GE, TC131::OperationFormat::DDC9_3,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const9_Signed( -128 ) } );

  tcop(
    { TC131::OpCode::OR_GE, TC131::OperationFormat::DDD_2,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::OR_GE_U, TC131::OperationFormat::DDC9_4,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const9_Unsigned( 128 ) } );

  tcop(
    { TC131::OpCode::OR_GE_U, TC131::OperationFormat::DDD_2,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::OR_LT, TC131::OperationFormat::DDC9_3,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const9_Signed( -128 ) } );

  tcop(
    { TC131::OpCode::OR_LT, TC131::OperationFormat::DDD_2,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::OR_LT_U, TC131::OperationFormat::DDC9_4,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const9_Unsigned( 128 ) } );

  tcop(
    { TC131::OpCode::OR_LT_U, TC131::OperationFormat::DDD_2,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::OR_NE, TC131::OperationFormat::DDC9_3,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const9_Signed( -128 ) } );

  tcop(
    { TC131::OpCode::OR_NE, TC131::OperationFormat::DDD_2,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::OR_NOR_T, TC131::OperationFormat::DDC5DC5_2,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const5_Unsigned( 13 ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new TC_Const5_Unsigned( 27 ) } );

  tcop(
    { TC131::OpCode::OR_OR_T, TC131::OperationFormat::DDC5DC5_2,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const5_Unsigned( 13 ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new TC_Const5_Unsigned( 27 ) } );

  tcop(
    { TC131::OpCode::OR_T, TC131::OperationFormat::DDC5DC5_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const5_Unsigned( 13 ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new TC_Const5_Unsigned( 27 ) } );

  tcop(
    { TC131::OpCode::ORN, TC131::OperationFormat::DDC9_2,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const9_Unsigned( 128 ) } );

  tcop(
    { TC131::OpCode::ORN, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ORN_T, TC131::OperationFormat::DDC5DC5_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const5_Unsigned( 13 ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new TC_Const5_Unsigned( 27 ) } );

  tcop(
    { TC131::OpCode::PACK, TC131::OperationFormat::DEDPSW,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( e1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( pswC, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::PARITY, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::Q31TOF, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::QSEED_F, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::RET, TC131::OperationFormat::PSW,
      new WIR_RegisterParameter( pswC, WIR_Usage::def ) } );

  tcop(
    { TC131::OpCode::RET, TC131::OperationFormat::SPSW,
      new WIR_RegisterParameter( pswC, WIR_Usage::def ) } );

  tcop(
    { TC131::OpCode::RFE, TC131::OperationFormat::PSW,
      new WIR_RegisterParameter( pswC, WIR_Usage::def ) } );

  tcop(
    { TC131::OpCode::RFE, TC131::OperationFormat::SPSW,
      new WIR_RegisterParameter( pswC, WIR_Usage::def ) } );

  tcop(
    { TC131::OpCode::RFM, TC131::OperationFormat::PSW,
      new WIR_RegisterParameter( pswC, WIR_Usage::def ) } );

  tcop(
    { TC131::OpCode::RSLCX, TC131::OperationFormat::SYS } );

  tcop(
    { TC131::OpCode::RSTV, TC131::OperationFormat::SYS } );

  tcop(
    { TC131::OpCode::RSUB, TC131::OperationFormat::DDC9_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const9_Signed( -128 ) } );

  tcop(
    { TC131::OpCode::RSUB, TC131::OperationFormat::SD,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ) } );

  tcop(
    { TC131::OpCode::RSUBS, TC131::OperationFormat::DDC9_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const9_Signed( -128 ) } );

  tcop(
    { TC131::OpCode::RSUBS_U, TC131::OperationFormat::DDC9_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const9_Signed( -128 ) } );

  tcop(
    { TC131::OpCode::SAT_B, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::SAT_B, TC131::OperationFormat::SD,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ) } );

  tcop(
    { TC131::OpCode::SAT_BU, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::SAT_BU, TC131::OperationFormat::SD,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ) } );

  tcop(
    { TC131::OpCode::SAT_H, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::SAT_H, TC131::OperationFormat::SD,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ) } );

  tcop(
    { TC131::OpCode::SAT_HU, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::SAT_HU, TC131::OperationFormat::SD,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ) } );

  tcop(
    { TC131::OpCode::SEL, TC131::OperationFormat::DDDC9_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new TC_Const9_Signed( -128 ) } );

  tcop(
    { TC131::OpCode::SEL, TC131::OperationFormat::DDDD,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::SELN, TC131::OperationFormat::DDDC9_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new TC_Const9_Signed( -128 ) } );

  tcop(
    { TC131::OpCode::SELN, TC131::OperationFormat::DDDD,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::SH, TC131::OperationFormat::DDC9_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const9_Signed( -128 ) } );

  tcop(
    { TC131::OpCode::SH, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::SH, TC131::OperationFormat::SDC4_2,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new TC_Const4_Signed( -5 ) } );

  tcop(
    { TC131::OpCode::SH_AND_T, TC131::OperationFormat::DDC5DC5_2,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const5_Unsigned( 13 ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new TC_Const5_Unsigned( 27 ) } );

  tcop(
    { TC131::OpCode::SH_ANDN_T, TC131::OperationFormat::DDC5DC5_2,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const5_Unsigned( 13 ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new TC_Const5_Unsigned( 27 ) } );

  tcop(
    { TC131::OpCode::SH_EQ, TC131::OperationFormat::DDC9_3,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const9_Signed( -128 ) } );

  tcop(
    { TC131::OpCode::SH_EQ, TC131::OperationFormat::DDD_2,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::SH_GE, TC131::OperationFormat::DDC9_3,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const9_Signed( -128 ) } );

  tcop(
    { TC131::OpCode::SH_GE, TC131::OperationFormat::DDD_2,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::SH_GE_U, TC131::OperationFormat::DDC9_4,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const9_Unsigned( 128 ) } );

  tcop(
    { TC131::OpCode::SH_GE_U, TC131::OperationFormat::DDD_2,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::SH_H, TC131::OperationFormat::DDC9_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const9_Signed( -128 ) } );

  tcop(
    { TC131::OpCode::SH_H, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::SH_LT, TC131::OperationFormat::DDC9_3,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const9_Signed( -128 ) } );

  tcop(
    { TC131::OpCode::SH_LT, TC131::OperationFormat::DDD_2,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::SH_LT_U, TC131::OperationFormat::DDC9_4,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const9_Unsigned( 128 ) } );

  tcop(
    { TC131::OpCode::SH_LT_U, TC131::OperationFormat::DDD_2,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::SH_NAND_T, TC131::OperationFormat::DDC5DC5_2,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const5_Unsigned( 13 ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new TC_Const5_Unsigned( 27 ) } );

  tcop(
    { TC131::OpCode::SH_NE, TC131::OperationFormat::DDC9_3,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const9_Signed( -128 ) } );

  tcop(
    { TC131::OpCode::SH_NE, TC131::OperationFormat::DDD_2,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::SH_NOR_T, TC131::OperationFormat::DDC5DC5_2,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const5_Unsigned( 13 ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new TC_Const5_Unsigned( 27 ) } );

  tcop(
    { TC131::OpCode::SH_OR_T, TC131::OperationFormat::DDC5DC5_2,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const5_Unsigned( 13 ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new TC_Const5_Unsigned( 27 ) } );

  tcop(
    { TC131::OpCode::SH_ORN_T, TC131::OperationFormat::DDC5DC5_2,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const5_Unsigned( 13 ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new TC_Const5_Unsigned( 27 ) } );

  tcop(
    { TC131::OpCode::SH_XNOR_T, TC131::OperationFormat::DDC5DC5_2,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const5_Unsigned( 13 ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new TC_Const5_Unsigned( 27 ) } );

  tcop(
    { TC131::OpCode::SH_XOR_T, TC131::OperationFormat::DDC5DC5_2,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const5_Unsigned( 13 ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new TC_Const5_Unsigned( 27 ) } );

  tcop(
    { TC131::OpCode::SHA, TC131::OperationFormat::DDC9PSW_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const9_Signed( -128 ),
      new WIR_RegisterParameter( pswC, WIR_Usage::def ) } );

  tcop(
    { TC131::OpCode::SHA, TC131::OperationFormat::DDDPSW_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( pswC, WIR_Usage::def ) } );

  tcop(
    { TC131::OpCode::SHA, TC131::OperationFormat::SDC4PSW,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new TC_Const4_Signed( -5 ),
      new WIR_RegisterParameter( pswC, WIR_Usage::def ) } );

  tcop(
    { TC131::OpCode::SHA_H, TC131::OperationFormat::DDC9_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const9_Signed( -128 ) } );

  tcop(
    { TC131::OpCode::SHA_H, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::SHAS, TC131::OperationFormat::DDC9_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const9_Signed( -128 ) } );

  tcop(
    { TC131::OpCode::SHAS, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_A, TC131::OperationFormat::C18AABSA,
      new TC_Const18_Unsigned( 16383 ),
      new WIR_RegisterParameter( a1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_A, TC131::OperationFormat::LAABSA,
      new WIR_LabelParameter( b1 ),
      new WIR_RegisterParameter( a1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_A, TC131::OperationFormat::AC10ABOA,
      new WIR_RegisterParameter( a1, WIR_Usage::use ),
      new TC_Const10_Signed( -512 ),
      new WIR_RegisterParameter( a2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_A, TC131::OperationFormat::PABRA,
      new WIR_RegisterParameter( p1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( a1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_A, TC131::OperationFormat::PC10ACA,
      new WIR_RegisterParameter( p1, WIR_Usage::defuse ),
      new TC_Const10_Signed( -512 ),
      new WIR_RegisterParameter( a1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_A, TC131::OperationFormat::AC10APIA,
      new WIR_AddressingModeParameter( TC131::AddressingMode::post ),
      new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
      new TC_Const10_Signed( -512 ),
      new WIR_RegisterParameter( a2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_A, TC131::OperationFormat::AC10APIA,
      new WIR_AddressingModeParameter( TC131::AddressingMode::pre ),
      new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
      new TC_Const10_Signed( -512 ),
      new WIR_RegisterParameter( a2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_A, TC131::OperationFormat::SSPC10I_1,
      new WIR_RegisterParameter( p.SP(), WIR_Usage::use ),
      new TC_Const10_Unsigned( 252 ),
      new WIR_RegisterParameter( p.A15(), WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_A, TC131::OperationFormat::SAC4I_1,
      new WIR_RegisterParameter( a1, WIR_Usage::use ),
      new TC_Const4_Unsigned( 12 ),
      new WIR_RegisterParameter( p.A15(), WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_A, TC131::OperationFormat::SAA_4,
      new WIR_RegisterParameter( a1, WIR_Usage::use ),
      new WIR_RegisterParameter( a2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_A, TC131::OperationFormat::SAA_6,
      new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( a2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_A, TC131::OperationFormat::SIC4A,
      new WIR_RegisterParameter( p.A15(), WIR_Usage::use ),
      new TC_Const4_Unsigned( 12 ),
      new WIR_RegisterParameter( a1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_B, TC131::OperationFormat::C18DABSA_1,
      new TC_Const18_Unsigned( 16383 ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_B, TC131::OperationFormat::LDABSA_1,
      new WIR_LabelParameter( b1 ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_B, TC131::OperationFormat::AC10DBOA_1,
      new WIR_RegisterParameter( a1, WIR_Usage::use ),
      new TC_Const10_Signed( -512 ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_B, TC131::OperationFormat::PDBRA_1,
      new WIR_RegisterParameter( p1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_B, TC131::OperationFormat::PC10DCA_1,
      new WIR_RegisterParameter( p1, WIR_Usage::defuse ),
      new TC_Const10_Signed( -512 ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_B, TC131::OperationFormat::AC10DPIA_1,
      new WIR_AddressingModeParameter( TC131::AddressingMode::post ),
      new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
      new TC_Const10_Signed( -512 ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_B, TC131::OperationFormat::AC10DPIA_1,
      new WIR_AddressingModeParameter( TC131::AddressingMode::pre ),
      new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
      new TC_Const10_Signed( -512 ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_B, TC131::OperationFormat::SAC4I_2,
      new WIR_RegisterParameter( a1, WIR_Usage::use ),
      new TC_Const4_Unsigned( 15 ),
      new WIR_RegisterParameter( p.D15(), WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_B, TC131::OperationFormat::SAD_2,
      new WIR_RegisterParameter( a1, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_B, TC131::OperationFormat::SAD_3,
      new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_B, TC131::OperationFormat::SIC4D,
      new WIR_RegisterParameter( p.A15(), WIR_Usage::use ),
      new TC_Const4_Unsigned( 15 ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_D, TC131::OperationFormat::C18EABSA,
      new TC_Const18_Unsigned( 16383 ),
      new WIR_RegisterParameter( e1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_D, TC131::OperationFormat::LEABSA,
      new WIR_LabelParameter( b1 ),
      new WIR_RegisterParameter( e1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_D, TC131::OperationFormat::AC10EBOA,
      new WIR_RegisterParameter( a1, WIR_Usage::use ),
      new TC_Const10_Signed( -512 ),
      new WIR_RegisterParameter( e1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_D, TC131::OperationFormat::PEBRA,
      new WIR_RegisterParameter( p1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( e1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_D, TC131::OperationFormat::PC10ECA,
      new WIR_RegisterParameter( p1, WIR_Usage::defuse ),
      new TC_Const10_Signed( -512 ),
      new WIR_RegisterParameter( e1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_D, TC131::OperationFormat::AC10EPIA,
      new WIR_AddressingModeParameter( TC131::AddressingMode::post ),
      new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
      new TC_Const10_Signed( -512 ),
      new WIR_RegisterParameter( e1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_D, TC131::OperationFormat::AC10EPIA,
      new WIR_AddressingModeParameter( TC131::AddressingMode::pre ),
      new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
      new TC_Const10_Signed( -512 ),
      new WIR_RegisterParameter( e1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_DA, TC131::OperationFormat::C18PABSA,
      new TC_Const18_Unsigned( 16383 ),
      new WIR_RegisterParameter( p1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_DA, TC131::OperationFormat::LPABSA,
      new WIR_LabelParameter( b1 ),
      new WIR_RegisterParameter( p1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_DA, TC131::OperationFormat::AC10PBOA,
      new WIR_RegisterParameter( a1, WIR_Usage::use ),
      new TC_Const10_Signed( -512 ),
      new WIR_RegisterParameter( p1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_DA, TC131::OperationFormat::PPBRA_2,
      new WIR_RegisterParameter( p1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( p2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_DA, TC131::OperationFormat::PC10PCA,
      new WIR_RegisterParameter( p1, WIR_Usage::defuse ),
      new TC_Const10_Signed( -512 ),
      new WIR_RegisterParameter( p2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_DA, TC131::OperationFormat::AC10PPIA,
      new WIR_AddressingModeParameter( TC131::AddressingMode::post ),
      new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
      new TC_Const10_Signed( -512 ),
      new WIR_RegisterParameter( p1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_DA, TC131::OperationFormat::AC10PPIA,
      new WIR_AddressingModeParameter( TC131::AddressingMode::pre ),
      new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
      new TC_Const10_Signed( -512 ),
      new WIR_RegisterParameter( p1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_H, TC131::OperationFormat::C18DABSA_1,
      new TC_Const18_Unsigned( 16383 ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_H, TC131::OperationFormat::LDABSA_1,
      new WIR_LabelParameter( b1 ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_H, TC131::OperationFormat::AC10DBOA_1,
      new WIR_RegisterParameter( a1, WIR_Usage::use ),
      new TC_Const10_Signed( -512 ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_H, TC131::OperationFormat::PDBRA_1,
      new WIR_RegisterParameter( p1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_H, TC131::OperationFormat::PC10DCA_1,
      new WIR_RegisterParameter( p1, WIR_Usage::defuse ),
      new TC_Const10_Signed( -512 ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_H, TC131::OperationFormat::AC10DPIA_1,
      new WIR_AddressingModeParameter( TC131::AddressingMode::post ),
      new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
      new TC_Const10_Signed( -512 ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_H, TC131::OperationFormat::AC10DPIA_1,
      new WIR_AddressingModeParameter( TC131::AddressingMode::pre ),
      new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
      new TC_Const10_Signed( -512 ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_H, TC131::OperationFormat::SAC4I_2,
      new WIR_RegisterParameter( a1, WIR_Usage::use ),
      new TC_Const4_Unsigned( 12 ),
      new WIR_RegisterParameter( p.D15(), WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_H, TC131::OperationFormat::SAD_2,
      new WIR_RegisterParameter( a1, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_H, TC131::OperationFormat::SAD_3,
      new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_H, TC131::OperationFormat::SIC4D,
      new WIR_RegisterParameter( p.A15(), WIR_Usage::use ),
      new TC_Const4_Unsigned( 12 ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_Q, TC131::OperationFormat::C18DABSA_1,
      new TC_Const18_Unsigned( 16383 ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_Q, TC131::OperationFormat::LDABSA_1,
      new WIR_LabelParameter( b1 ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_Q, TC131::OperationFormat::AC10DBOA_1,
      new WIR_RegisterParameter( a1, WIR_Usage::use ),
      new TC_Const10_Signed( -512 ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_Q, TC131::OperationFormat::PDBRA_1,
      new WIR_RegisterParameter( p1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_Q, TC131::OperationFormat::PC10DCA_1,
      new WIR_RegisterParameter( p1, WIR_Usage::defuse ),
      new TC_Const10_Signed( -512 ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_Q, TC131::OperationFormat::AC10DPIA_1,
      new WIR_AddressingModeParameter( TC131::AddressingMode::post ),
      new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
      new TC_Const10_Signed( -512 ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_Q, TC131::OperationFormat::AC10DPIA_1,
      new WIR_AddressingModeParameter( TC131::AddressingMode::pre ),
      new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
      new TC_Const10_Signed( -512 ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_T, TC131::OperationFormat::C18C3C1,
      new TC_Const18_Unsigned( 16383 ),
      new TC_Const3_Unsigned( 7 ),
      new TC_Const1_Unsigned( 0 ) } );

  tcop(
    { TC131::OpCode::ST_W, TC131::OperationFormat::C18DABSA_1,
      new TC_Const18_Unsigned( 16383 ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_W, TC131::OperationFormat::LDABSA_1,
      new WIR_LabelParameter( b1 ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_W, TC131::OperationFormat::AC10DBOA_1,
      new WIR_RegisterParameter( a1, WIR_Usage::use ),
      new TC_Const10_Signed( -512 ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_W, TC131::OperationFormat::PDBRA_1,
      new WIR_RegisterParameter( p1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_W, TC131::OperationFormat::PC10DCA_1,
      new WIR_RegisterParameter( p1, WIR_Usage::defuse ),
      new TC_Const10_Signed( -512 ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_W, TC131::OperationFormat::AC10DPIA_1,
      new WIR_AddressingModeParameter( TC131::AddressingMode::post ),
      new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
      new TC_Const10_Signed( -512 ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_W, TC131::OperationFormat::AC10DPIA_1,
      new WIR_AddressingModeParameter( TC131::AddressingMode::pre ),
      new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
      new TC_Const10_Signed( -512 ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_W, TC131::OperationFormat::AC16DBOA,
      new WIR_RegisterParameter( a1, WIR_Usage::use ),
      new TC_Const16_Signed( -26361 ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_W, TC131::OperationFormat::ALC16DBOA,
      new WIR_RegisterParameter( a1, WIR_Usage::use ),
      new WIR_LabelParameter( b1 ),
      new TC_Const16_Signed( 0 ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_W, TC131::OperationFormat::SSPC10I_2,
      new WIR_RegisterParameter( p.SP(), WIR_Usage::use ),
      new TC_Const10_Unsigned( 64 ),
      new WIR_RegisterParameter( p.D15(), WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_W, TC131::OperationFormat::SAC4I_2,
      new WIR_RegisterParameter( a1, WIR_Usage::use ),
      new TC_Const4_Unsigned( 12 ),
      new WIR_RegisterParameter( p.D15(), WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_W, TC131::OperationFormat::SAD_2,
      new WIR_RegisterParameter( a1, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_W, TC131::OperationFormat::SAD_3,
      new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ST_W, TC131::OperationFormat::SIC4D,
      new WIR_RegisterParameter( p.A15(), WIR_Usage::use ),
      new TC_Const4_Unsigned( 12 ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::STLCX, TC131::OperationFormat::C18ABSA,
      new TC_Const18_Unsigned( 16383 ) } );

  tcop(
    { TC131::OpCode::STLCX, TC131::OperationFormat::LABSA,
      new WIR_LabelParameter( b1 ) } );

  tcop(
    { TC131::OpCode::STLCX, TC131::OperationFormat::AC10BOA,
      new WIR_RegisterParameter( a1, WIR_Usage::use ),
      new TC_Const10_Signed( -486 ) } );

  tcop(
    { TC131::OpCode::STUCX, TC131::OperationFormat::C18ABSAPSW,
      new TC_Const18_Unsigned( 16383 ),
      new WIR_RegisterParameter( pswC, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::STUCX, TC131::OperationFormat::LABSAPSW,
      new WIR_LabelParameter( b1 ),
      new WIR_RegisterParameter( pswC, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::STUCX, TC131::OperationFormat::AC10BOAPSW,
      new WIR_RegisterParameter( a1, WIR_Usage::use ),
      new TC_Const10_Signed( -486 ),
      new WIR_RegisterParameter( pswC, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::SUB, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::SUB, TC131::OperationFormat::SDD_2,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::SUB, TC131::OperationFormat::SDID_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( p.D15(), WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::SUB, TC131::OperationFormat::SIDD,
      new WIR_RegisterParameter( p.D15(), WIR_Usage::def ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::SUB_A, TC131::OperationFormat::AAA,
      new WIR_RegisterParameter( a1, WIR_Usage::def ),
      new WIR_RegisterParameter( a2, WIR_Usage::use ),
      new WIR_RegisterParameter( a3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::SUB_A, TC131::OperationFormat::SSPC8,
      new WIR_RegisterParameter( p.SP(), WIR_Usage::defuse ),
      new TC_Const8_Unsigned( 255 ) } );

  tcop(
    { TC131::OpCode::SUB_B, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::SUB_F, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::SUB_H, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::SUBC, TC131::OperationFormat::DDDPSW_2,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( pswC, WIR_Usage::defuse ) } );

  tcop(
    { TC131::OpCode::SUBS, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::SUBS, TC131::OperationFormat::SDD_2,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::SUBS_H, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::SUBS_HU, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::SUBS_U, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::SUBX, TC131::OperationFormat::DDDPSW_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( pswC, WIR_Usage::def ) } );

  tcop(
    { TC131::OpCode::SVLCX, TC131::OperationFormat::SYS } );

  tcop(
    { TC131::OpCode::SWAP_W, TC131::OperationFormat::C18DABSA_2,
      new TC_Const18_Unsigned( 16383 ),
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ) } );

  tcop(
    { TC131::OpCode::SWAP_W, TC131::OperationFormat::LDABSA_2,
      new WIR_LabelParameter( b1 ),
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ) } );

  tcop(
    { TC131::OpCode::SWAP_W, TC131::OperationFormat::AC10DBOA_2,
      new WIR_RegisterParameter( a1, WIR_Usage::use ),
      new TC_Const10_Signed( -512 ),
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ) } );

  tcop(
    { TC131::OpCode::SWAP_W, TC131::OperationFormat::PDBRA_2,
      new WIR_RegisterParameter( p1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ) } );

  tcop(
    { TC131::OpCode::SWAP_W, TC131::OperationFormat::PC10DCA_2,
      new WIR_RegisterParameter( p1, WIR_Usage::defuse ),
      new TC_Const10_Signed( -512 ),
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ) } );

  tcop(
    { TC131::OpCode::SWAP_W, TC131::OperationFormat::AC10DPIA_2,
      new WIR_AddressingModeParameter( TC131::AddressingMode::post ),
      new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
      new TC_Const10_Signed( -512 ),
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ) } );

  tcop(
    { TC131::OpCode::SWAP_W, TC131::OperationFormat::AC10DPIA_2,
      new WIR_AddressingModeParameter( TC131::AddressingMode::pre ),
      new WIR_RegisterParameter( a1, WIR_Usage::defuse ),
      new TC_Const10_Signed( -512 ),
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ) } );

  tcop(
    { TC131::OpCode::SYSCALL, TC131::OperationFormat::C9PSW,
      new TC_Const9_Unsigned( 128 ),
      new WIR_RegisterParameter( pswC, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::TLBDEMAP, TC131::OperationFormat::D,
      new WIR_RegisterParameter( d1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::TLBFLUSH_A, TC131::OperationFormat::SYS } );

  tcop(
    { TC131::OpCode::TLBFLUSH_B, TC131::OperationFormat::SYS } );

  tcop(
    { TC131::OpCode::TLBMAP, TC131::OperationFormat::E,
      new WIR_RegisterParameter( e1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::TLBPROBE_A, TC131::OperationFormat::D,
      new WIR_RegisterParameter( d1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::TLBPROBE_I, TC131::OperationFormat::D,
      new WIR_RegisterParameter( d1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::TRAPSV, TC131::OperationFormat::SYS } );

  tcop(
    { TC131::OpCode::TRAPV, TC131::OperationFormat::SYS } );

  tcop(
    { TC131::OpCode::UNPACK, TC131::OperationFormat::ED,
      new WIR_RegisterParameter( e1, WIR_Usage::def ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::UPDFL, TC131::OperationFormat::D,
      new WIR_RegisterParameter( d1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::UTOF, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::XNOR, TC131::OperationFormat::DDC9_2,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const9_Unsigned( 128 ) } );

  tcop(
    { TC131::OpCode::XNOR, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::XNOR_T, TC131::OperationFormat::DDC5DC5_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const5_Unsigned( 13 ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new TC_Const5_Unsigned( 27 ) } );

  tcop(
    { TC131::OpCode::XOR, TC131::OperationFormat::DDC9_2,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const9_Unsigned( 128 ) } );

  tcop(
    { TC131::OpCode::XOR, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::XOR, TC131::OperationFormat::SDD_2,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::XOR_EQ, TC131::OperationFormat::DDC9_3,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const9_Signed( -128 ) } );

  tcop(
    { TC131::OpCode::XOR_EQ, TC131::OperationFormat::DDD_2,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::XOR_GE, TC131::OperationFormat::DDC9_3,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const9_Signed( -128 ) } );

  tcop(
    { TC131::OpCode::XOR_GE, TC131::OperationFormat::DDD_2,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::XOR_GE_U, TC131::OperationFormat::DDC9_4,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const9_Unsigned( 128 ) } );

  tcop(
    { TC131::OpCode::XOR_GE_U, TC131::OperationFormat::DDD_2,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::XOR_LT, TC131::OperationFormat::DDC9_3,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const9_Signed( -128 ) } );

  tcop(
    { TC131::OpCode::XOR_LT, TC131::OperationFormat::DDD_2,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::XOR_LT_U, TC131::OperationFormat::DDC9_4,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const9_Unsigned( 128 ) } );

  tcop(
    { TC131::OpCode::XOR_LT_U, TC131::OperationFormat::DDD_2,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::XOR_NE, TC131::OperationFormat::DDC9_3,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const9_Signed( -128 ) } );

  tcop(
    { TC131::OpCode::XOR_NE, TC131::OperationFormat::DDD_2,
      new WIR_RegisterParameter( d1, WIR_Usage::defuse ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::XOR_T, TC131::OperationFormat::DDC5DC5_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const5_Unsigned( 13 ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new TC_Const5_Unsigned( 27 ) } );

  // Create string to be parsed, avoid .section directive here.
  sstr << b1;
  sstr.str( string() );
  sstr << b1;

  // Parse the TriCore assembly string.
  vector<unique_ptr<TC_AsmArgument>> dummy;
  TC_AsmParser parser;
  parser.run( sstr.str(), dummy, b1, "tcAsmParser1.cc" );

  // bIt1 points to the original basic block to be parsed.
  auto bIt1 = f.begin();

  // iIt1 points to an original TriCore instruction to be parsed.
  auto iIt1 = bIt1->get().begin();

  // bIt2 points to the first basic block created by the parser.
  auto bIt2 = bIt1;
  ++bIt2;

  // Iterate over all basic blocks created by the parser.
  for ( ; bIt2 != f.end(); ++bIt2 )
    // Iterate over all TriCore instructions in lockstep, both original one and
    // the one created by the parser.
    for ( auto iIt2 = bIt2->get().begin(); iIt2 != bIt2->get().end();
          ++iIt2, ++iIt1 ) {
      // Perform pairwise comparison of the current two instructions.
      ufAssert(
        iIt2->get().getOperations().size() ==
          iIt1->get().getOperations().size() );

      auto oIt1 = iIt1->get().begin();
      auto oIt2 = iIt2->get().begin();

      // Iterate over all TriCore operations in lockstep.
      for ( ; oIt2 != iIt2->get().end(); ++oIt2, ++oIt1 ) {
        // Perform pairwise comparison of the current two operations.
        ufAssert( oIt2->get().getOpCode() == oIt1->get().getOpCode() );
        ufAssert(
          oIt2->get().getOperationFormat() ==
            oIt1->get().getOperationFormat() );
        ufAssert(
          oIt2->get().getParameters().size() >=
            oIt1->get().getParameters().size() );

        auto pIt1 = oIt1->get().begin();
        auto pIt2 = oIt2->get().begin();

        // Iterate over all TriCore parameters in lockstep.
        for ( ; pIt1 != oIt1->get().end(); ++pIt2, ++pIt1 ) {
          WIR_Parameter &p1 = pIt1->get();
          WIR_Parameter &p2 = pIt2->get();

          ufAssert( p2.getType() == p1.getType() );

          switch( p2.getType() ) {

            case WIR_ParameterType::addr: {
              ufAssert(
                dynamic_cast<WIR_AddressingModeParameter &>(
                  p2 ).getAddressingMode() ==
                dynamic_cast<WIR_AddressingModeParameter &>(
                  p1 ).getAddressingMode() );
              break;
            }

            case WIR_ParameterType::imm: {
              auto &ip1 = dynamic_cast<WIR_BaseImmediateParameter &>( p1 );
              auto &ip2 = dynamic_cast<WIR_BaseImmediateParameter &>( p2 );

              ufAssert( ip2.isSigned() == ip1.isSigned() );
              if ( ip2.isSigned() )
                ufAssert( ip2.getSignedValue() == ip1.getSignedValue() );
              else
                ufAssert( ip2.getUnsignedValue() == ip1.getUnsignedValue() );
              break;
            }

            case WIR_ParameterType::label: {
              auto &lp1 = dynamic_cast<WIR_LabelParameter &>( p1 );
              auto &lp2 = dynamic_cast<WIR_LabelParameter &>( p2 );

              ufAssert( lp2.getLabelType() == lp1.getLabelType() );

              switch ( lp2.getLabelType() ) {
                case WIR_SymbolType::block: {
                  // Block labels need not be equal after parsing so that we
                  // simply don't do any checks here.
                  break;
                }

                case WIR_SymbolType::data: {
                  ufAssert( lp2.getData() == lp1.getData() );
                  break;
                }

                case WIR_SymbolType::function: {
                  ufAssert( lp2.getFunction() == lp1.getFunction() );
                  break;
                }
              }

              break;
            }

            case WIR_ParameterType::reg: {
              ufAssert(
                dynamic_cast<WIR_RegisterParameter &>( p2 ).getRegister() ==
                dynamic_cast<WIR_RegisterParameter &>( p1 ).getRegister() );
              break;
            }

            default: {
              ufAssertT(
                false,
                "Parameter type detected that should not occur for TriCore." );
              break;
            }
          }
        }
      }
    }

  return( 0 );
}
