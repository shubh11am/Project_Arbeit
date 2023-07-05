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
#include <iterator>

// Include libuseful headers
#include <libuseful/io.h>

// Include WIR headers
#include <wir/wir.h>
#include <arch/tricore/tc131.h>
#include <arch/tricore/analyses/bit/tcbitdfa.h>


using namespace std;
using namespace WIR;


int main( void )
{
  WIR_Init();

  // This test case tests the bit-true top-down data flow analysis for TriCore
  // operations SH & co.

  TC131 p;
  WIR_CompilationUnit c;
  c.setName( __FILE__ );
  WIR_Function &f = c.pushBackFunction( WIR_Function { "main" } );
  WIR_BasicBlock &b1 = f.pushBackBasicBlock( {} );
  const TC_DRegP &D4 = p.D4();
  const TC_PSWBit &pswC = p.PSW_C();

  auto &dExt = f.pushBackVirtualRegister( TC_DRegV() );
  auto &dMi123 = f.pushBackVirtualRegister( TC_DRegV() );
  auto &dPl64 = f.pushBackVirtualRegister( TC_DRegV() );
  auto &dPl12 = f.pushBackVirtualRegister( TC_DRegV() );
  auto &dMi12 = f.pushBackVirtualRegister( TC_DRegV() );
  auto &tmp0 = f.pushBackVirtualRegister( TC_DRegV() );
  auto &bitMask = f.pushBackVirtualRegister( TC_DRegV() );
  auto &notOne = f.pushBackVirtualRegister( TC_DRegV() );

  // This lambda serves for generating and inserting a TriCore operation.
  auto tcop = [&]( WIR_Operation &&o ) -> WIR_Operation & {
    auto &i = b1.pushBackInstruction( WIR_Instruction { WIR_Operation { o } } );
    return( i.begin()->get() );
  };

  // This lambda serves to retrieve an operation's outgoing down value.
  auto dval = []( const WIR_Operation &o,
                  unsigned int pos ) -> const WIR_UpDownValue & {
    auto it = o.begin();
    std::advance( it, pos );
    auto &c = it->get().getContainers<WIR_BitValues>().begin()->get();
    return( c.getOutValues().begin()->downVal );
  };

  // This lambda serves to retrieve an operation's incoming up value.
  auto uval = []( const WIR_Operation &o,
                  unsigned int pos ) -> const WIR_UpDownValue & {
    auto it = o.begin();
    std::advance( it, pos );
    auto &c = it->get().getContainers<WIR_BitValues>().begin()->get();
    return( c.getInValues().begin()->upVal );
  };

  // Create WIR code.

  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( dMi123, WIR_Usage::def ),
      new TC_Const16_Signed( -123 ) } );

  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( dPl64, WIR_Usage::def ),
      new TC_Const16_Signed( 64 ) } );

  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( dPl12, WIR_Usage::def ),
      new TC_Const16_Signed( 12 ) } );

  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( dMi12, WIR_Usage::def ),
      new TC_Const16_Signed( -12 ) } );

  tcop(
    { TC131::OpCode::MOVH, TC131::OperationFormat::DC16_2,
      new WIR_RegisterParameter( bitMask, WIR_Usage::def ),
      new TC_Const16_Unsigned( 0xFFFF ) } );

  tcop(
    { TC131::OpCode::ADDI, TC131::OperationFormat::DDC16_1,
      new WIR_RegisterParameter( bitMask, WIR_Usage::def ),
      new WIR_RegisterParameter( bitMask, WIR_Usage::use ),
      new TC_Const16_Signed( 0x7FFF ) } );

  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( notOne, WIR_Usage::def ),
      new TC_Const16_Signed( 1 ) } );

  tcop(
    { TC131::OpCode::NOT, TC131::OperationFormat::SD,
      new WIR_RegisterParameter( notOne, WIR_Usage::defuse ) } );

  // SH
  auto &sh1 = tcop(
    { TC131::OpCode::SH, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl12, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::AND, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( bitMask, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &sh2 = tcop(
    { TC131::OpCode::SH, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( dMi12, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::AND, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( bitMask, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &sh3 = tcop(
    { TC131::OpCode::SH, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // SH.EQ
  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ) } );

  auto &sheq1 = tcop(
    { TC131::OpCode::SH_EQ, TC131::OperationFormat::DDC9_3,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const9_Signed( -128 ) } );

  tcop(
    { TC131::OpCode::AND, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( bitMask, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &sheq2 = tcop(
    { TC131::OpCode::SH_EQ, TC131::OperationFormat::DDC9_3,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const9_Signed( 64 ) } );

  tcop(
    { TC131::OpCode::AND, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( bitMask, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &sheq3 = tcop(
    { TC131::OpCode::SH_EQ, TC131::OperationFormat::DDC9_3,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new TC_Const9_Signed( 64 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &sheq4 = tcop(
    { TC131::OpCode::SH_EQ, TC131::OperationFormat::DDD_2,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // SH.GE
  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ) } );

  auto &shge1 = tcop(
    { TC131::OpCode::SH_GE, TC131::OperationFormat::DDC9_3,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const9_Signed( -128 ) } );

  tcop(
    { TC131::OpCode::AND, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( bitMask, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &shge2 = tcop(
    { TC131::OpCode::SH_GE, TC131::OperationFormat::DDC9_3,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const9_Signed( 65 ) } );

  tcop(
    { TC131::OpCode::AND, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( notOne, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &shge3 = tcop(
    { TC131::OpCode::SH_GE, TC131::OperationFormat::DDC9_3,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new TC_Const9_Signed( 65 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &shge4 = tcop(
    { TC131::OpCode::SH_GE, TC131::OperationFormat::DDD_2,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // SH.GE.U
  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ) } );

  auto &shgeu1 = tcop(
    { TC131::OpCode::SH_GE_U, TC131::OperationFormat::DDC9_4,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const9_Unsigned( 384 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &shgeu2 = tcop(
    { TC131::OpCode::SH_GE_U, TC131::OperationFormat::DDC9_4,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const9_Unsigned( 64 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &shgeu3 = tcop(
    { TC131::OpCode::SH_GE_U, TC131::OperationFormat::DDC9_4,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new TC_Const9_Unsigned( 64 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // SH.H
  auto &shh1 = tcop(
    { TC131::OpCode::SH_H, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl12, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::AND, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( bitMask, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &shh2 = tcop(
    { TC131::OpCode::SH_H, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( dMi12, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOVH, TC131::OperationFormat::DC16_2,
      new WIR_RegisterParameter( bitMask, WIR_Usage::def ),
      new TC_Const16_Unsigned( 0xFFFF ) } );

  tcop(
    { TC131::OpCode::ADDI, TC131::OperationFormat::DDC16_1,
      new WIR_RegisterParameter( bitMask, WIR_Usage::def ),
      new WIR_RegisterParameter( bitMask, WIR_Usage::use ),
      new TC_Const16_Signed( 0x7FF7 ) } );

  tcop(
    { TC131::OpCode::AND, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( bitMask, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOVH, TC131::OperationFormat::DC16_2,
      new WIR_RegisterParameter( bitMask, WIR_Usage::def ),
      new TC_Const16_Unsigned( 0xFFFF ) } );

  tcop(
    { TC131::OpCode::ADDI, TC131::OperationFormat::DDC16_1,
      new WIR_RegisterParameter( bitMask, WIR_Usage::def ),
      new WIR_RegisterParameter( bitMask, WIR_Usage::use ),
      new TC_Const16_Signed( 0x7FFF ) } );

  auto &shh3 = tcop(
    { TC131::OpCode::SH_H, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // SH.LT
  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ) } );

  auto &shlt1 = tcop(
    { TC131::OpCode::SH_LT, TC131::OperationFormat::DDC9_3,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const9_Signed( -128 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &shlt2 = tcop(
    { TC131::OpCode::SH_LT, TC131::OperationFormat::DDC9_3,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const9_Signed( 65 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &shlt3 = tcop(
    { TC131::OpCode::SH_LT, TC131::OperationFormat::DDC9_3,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new TC_Const9_Signed( 65 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &shlt4 = tcop(
    { TC131::OpCode::SH_LT, TC131::OperationFormat::DDD_2,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // SH.LT.U
  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ) } );

  auto &shltu1 = tcop(
    { TC131::OpCode::SH_LT_U, TC131::OperationFormat::DDC9_4,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const9_Unsigned( 384 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &shltu2 = tcop(
    { TC131::OpCode::SH_LT_U, TC131::OperationFormat::DDC9_4,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const9_Unsigned( 64 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &shltu3 = tcop(
    { TC131::OpCode::SH_LT_U, TC131::OperationFormat::DDC9_4,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new TC_Const9_Unsigned( 64 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // SH.NE
  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ) } );

  auto &shne1 = tcop(
    { TC131::OpCode::SH_NE, TC131::OperationFormat::DDC9_3,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const9_Signed( -128 ) } );

  tcop(
    { TC131::OpCode::AND, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( bitMask, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &shne2 = tcop(
    { TC131::OpCode::SH_NE, TC131::OperationFormat::DDC9_3,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const9_Signed( 64 ) } );

  tcop(
    { TC131::OpCode::AND, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( bitMask, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &shne3 = tcop(
    { TC131::OpCode::SH_NE, TC131::OperationFormat::DDC9_3,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new TC_Const9_Signed( 64 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &shne4 = tcop(
    { TC131::OpCode::SH_NE, TC131::OperationFormat::DDD_2,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // SH.AND.T
  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ) } );

  auto &shandt1 = tcop(
    { TC131::OpCode::SH_AND_T, TC131::OperationFormat::DDC5DC5_2,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const5_Unsigned( 25 ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 6 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &shandt2 = tcop(
    { TC131::OpCode::SH_AND_T, TC131::OperationFormat::DDC5DC5_2,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const5_Unsigned( 25 ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 5 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // SH.ANDN.T
  auto &shandnt1 = tcop(
    { TC131::OpCode::SH_ANDN_T, TC131::OperationFormat::DDC5DC5_2,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const5_Unsigned( 25 ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 6 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &shandnt2 = tcop(
    { TC131::OpCode::SH_ANDN_T, TC131::OperationFormat::DDC5DC5_2,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const5_Unsigned( 25 ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 5 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // SH.NAND.T
  auto &shnandt1 = tcop(
    { TC131::OpCode::SH_NAND_T, TC131::OperationFormat::DDC5DC5_2,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const5_Unsigned( 25 ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 6 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &shnandt2 = tcop(
    { TC131::OpCode::SH_NAND_T, TC131::OperationFormat::DDC5DC5_2,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const5_Unsigned( 25 ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 5 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // SH.NOR.T
  auto &shnort1 = tcop(
    { TC131::OpCode::SH_NOR_T, TC131::OperationFormat::DDC5DC5_2,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const5_Unsigned( 3 ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 5 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &shnort2 = tcop(
    { TC131::OpCode::SH_NOR_T, TC131::OperationFormat::DDC5DC5_2,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const5_Unsigned( 2 ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 5 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // SH.OR.T
  auto &short1 = tcop(
    { TC131::OpCode::SH_OR_T, TC131::OperationFormat::DDC5DC5_2,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const5_Unsigned( 3 ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 6 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &short2 = tcop(
    { TC131::OpCode::SH_OR_T, TC131::OperationFormat::DDC5DC5_2,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const5_Unsigned( 3 ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 5 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // SH.ORN.T
  auto &shornt1 = tcop(
    { TC131::OpCode::SH_ORN_T, TC131::OperationFormat::DDC5DC5_2,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const5_Unsigned( 3 ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 6 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &shornt2 = tcop(
    { TC131::OpCode::SH_ORN_T, TC131::OperationFormat::DDC5DC5_2,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const5_Unsigned( 3 ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 5 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // SH.XNOR.T
  auto &shxnort1 = tcop(
    { TC131::OpCode::SH_XNOR_T, TC131::OperationFormat::DDC5DC5_2,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const5_Unsigned( 3 ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 6 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &shxnort2 = tcop(
    { TC131::OpCode::SH_XNOR_T, TC131::OperationFormat::DDC5DC5_2,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const5_Unsigned( 3 ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 5 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // SH.XOR.T
  auto &shxort1 = tcop(
    { TC131::OpCode::SH_XOR_T, TC131::OperationFormat::DDC5DC5_2,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const5_Unsigned( 3 ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 6 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &shxort2 = tcop(
    { TC131::OpCode::SH_XOR_T, TC131::OperationFormat::DDC5DC5_2,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const5_Unsigned( 3 ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 5 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // SHA
  auto &sha1 = tcop(
    { TC131::OpCode::SHA, TC131::OperationFormat::DDDPSW_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl12, WIR_Usage::use ),
      new WIR_RegisterParameter( pswC, WIR_Usage::def ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &sha2 = tcop(
    { TC131::OpCode::SHA, TC131::OperationFormat::DDDPSW_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( dMi12, WIR_Usage::use ),
      new WIR_RegisterParameter( pswC, WIR_Usage::def ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &sha3 = tcop(
    { TC131::OpCode::SHA, TC131::OperationFormat::DDDPSW_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new WIR_RegisterParameter( pswC, WIR_Usage::def ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // SHA.H
  auto &shah1 = tcop(
    { TC131::OpCode::SHA_H, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl12, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &shah2 = tcop(
    { TC131::OpCode::SHA_H, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( dMi12, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOVH, TC131::OperationFormat::DC16_2,
      new WIR_RegisterParameter( bitMask, WIR_Usage::def ),
      new TC_Const16_Unsigned( 0xFFFF ) } );

  tcop(
    { TC131::OpCode::ADDI, TC131::OperationFormat::DDC16_1,
      new WIR_RegisterParameter( bitMask, WIR_Usage::def ),
      new WIR_RegisterParameter( bitMask, WIR_Usage::use ),
      new TC_Const16_Signed( 0x7FF7 ) } );

  tcop(
    { TC131::OpCode::AND, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( bitMask, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOVH, TC131::OperationFormat::DC16_2,
      new WIR_RegisterParameter( bitMask, WIR_Usage::def ),
      new TC_Const16_Unsigned( 0xFFFF ) } );

  tcop(
    { TC131::OpCode::ADDI, TC131::OperationFormat::DDC16_1,
      new WIR_RegisterParameter( bitMask, WIR_Usage::def ),
      new WIR_RegisterParameter( bitMask, WIR_Usage::use ),
      new TC_Const16_Signed( 0x7FFF ) } );

  auto &shah3 = tcop(
    { TC131::OpCode::SHA_H, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOVH, TC131::OperationFormat::DC16_2,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new TC_Const16_Unsigned( 0x8765 ) } );

  tcop(
    { TC131::OpCode::ADDI, TC131::OperationFormat::DDC16_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new TC_Const16_Signed( 0x7654 ) } );

  auto &shah4 = tcop(
    { TC131::OpCode::SHA_H, TC131::OperationFormat::DDC9_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new TC_Const9_Signed( -16 ) } );

  tcop(
    { TC131::OpCode::MOVH, TC131::OperationFormat::DC16_2,
      new WIR_RegisterParameter( bitMask, WIR_Usage::def ),
      new TC_Const16_Unsigned( 0xFFFF ) } );

  tcop(
    { TC131::OpCode::ADDI, TC131::OperationFormat::DDC16_1,
      new WIR_RegisterParameter( bitMask, WIR_Usage::def ),
      new WIR_RegisterParameter( bitMask, WIR_Usage::use ),
      new TC_Const16_Signed( 0x7FF7 ) } );

  tcop(
    { TC131::OpCode::AND, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( bitMask, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOVH, TC131::OperationFormat::DC16_2,
      new WIR_RegisterParameter( bitMask, WIR_Usage::def ),
      new TC_Const16_Unsigned( 0xFFFF ) } );

  tcop(
    { TC131::OpCode::ADDI, TC131::OperationFormat::DDC16_1,
      new WIR_RegisterParameter( bitMask, WIR_Usage::def ),
      new WIR_RegisterParameter( bitMask, WIR_Usage::use ),
      new TC_Const16_Signed( 0x7FFF ) } );

  // SHAS
  auto &shas1 = tcop(
    { TC131::OpCode::SHAS, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl12, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &shas2 = tcop(
    { TC131::OpCode::SHAS, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( dMi12, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &shas3 = tcop(
    { TC131::OpCode::SHAS, TC131::OperationFormat::DDC9_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const9_Signed( 25 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &shas4 = tcop(
    { TC131::OpCode::SHAS, TC131::OperationFormat::DDC9_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const9_Signed( 25 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &shas5 = tcop(
    { TC131::OpCode::SHAS, TC131::OperationFormat::DDC9_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const9_Signed( -2 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &shas6 = tcop(
    { TC131::OpCode::SHAS, TC131::OperationFormat::DDC9_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const9_Signed( -2 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // Do bit-true data flow analysis.
  TC_BitDFA a { f };
  a.analyze();

  // Evaluate analysis results.
  ufAssert( dval( sh1, 0 ).getSignedValue() == 0xFFF85000 );
  ufAssert( uval( sh1, 1 ).extract( 20, 12 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( sh1, 1 ).extract( 4, 16 ).isBinaryInteger() );
  ufAssert( uval( sh1, 1 ).at( 3 ) == WIR_L4::bX );
  ufAssert( uval( sh1, 1 ).extract( 0, 3 ).isBinaryInteger() );
  ufAssert( uval( sh1, 2 ).extract( 0, 6 ).isBinaryInteger() );
  ufAssert( uval( sh1, 2 ).extract( 6, 26 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( sh2, 0 ).getSignedValue() == 0xFFF85 );
  ufAssert( uval( sh2, 1 ).extract( 0, 12 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( sh2, 1 ).extract( 12, 15 ).isBinaryInteger() );
  ufAssert( uval( sh2, 1 ).at( 27 ) == WIR_L4::bX );
  ufAssert( uval( sh2, 1 ).extract( 28, 4 ).isBinaryInteger() );
  ufAssert( uval( sh2, 2 ).extract( 0, 6 ).isBinaryInteger() );
  ufAssert( uval( sh2, 2 ).extract( 6, 26 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( sh3, 0 ).containsOnlyBit( WIR_L4::bL ) );

  ufAssert( dval( sheq1, 0 ).getSignedValue() == 128 );
  ufAssert( uval( sheq1, 0 ).extract( 0, 14 ).isBinaryInteger() );
  ufAssert( uval( sheq1, 0 ).at( 14 ) == WIR_L4::bX );
  ufAssert( uval( sheq1, 0 ).extract( 15, 16 ).isBinaryInteger() );
  ufAssert( uval( sheq1, 0 ).at( 31 ) == WIR_L4::bX );
  ufAssert( uval( sheq1, 1 ).extract( 0, 6 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( sheq1, 1 ).at( 6 ) == WIR_L4::b1 );
  ufAssert( uval( sheq1, 1 ).extract( 7, 25 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( sheq1, 2 ).extract( 0, 6 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( sheq1, 2 ).at( 6 ) == WIR_L4::b0 );
  ufAssert( uval( sheq1, 2 ).extract( 7, 2 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( sheq2, 0 ).getSignedValue() == 257 );
  ufAssert( uval( sheq2, 0 ).extract( 0, 14 ).isBinaryInteger() );
  ufAssert( uval( sheq2, 0 ).at( 14 ) == WIR_L4::bX );
  ufAssert( uval( sheq2, 0 ).extract( 15, 16 ).isBinaryInteger() );
  ufAssert( uval( sheq2, 0 ).at( 31 ) == WIR_L4::bX );
  ufAssert( uval( sheq2, 1 ).isBinaryInteger() );
  ufAssert( uval( sheq2, 2 ).isBinaryInteger() );

  ufAssert( dval( sheq3, 0 ).extract( 1, 31 ).getSignedValue() == 257 );
  ufAssert( dval( sheq3, 0 ).at( 0 ) == WIR_L4::bL );
  ufAssert( dval( sheq4, 0 ).at( 0 ) == WIR_L4::b1 );

  ufAssert( dval( shge1, 0 ).getSignedValue() == 129 );
  ufAssert( uval( shge1, 0 ).extract( 0, 14 ).isBinaryInteger() );
  ufAssert( uval( shge1, 0 ).at( 14 ) == WIR_L4::bX );
  ufAssert( uval( shge1, 0 ).extract( 15, 16 ).isBinaryInteger() );
  ufAssert( uval( shge1, 0 ).at( 31 ) == WIR_L4::bX );
  ufAssert( uval( shge1, 1 ).isBinaryInteger() );
  ufAssert( uval( shge1, 2 ).isBinaryInteger() );

  ufAssert( dval( shge2, 0 ).getSignedValue() == 258 );
  ufAssert( uval( shge2, 0 ).extract( 0, 31 ).isBinaryInteger() );
  ufAssert( uval( shge2, 0 ).at( 31 ) == WIR_L4::bX );
  ufAssert( uval( shge2, 1 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( shge2, 2 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( shge3, 0 ).extract( 1, 31 ).getSignedValue() == 258 );
  ufAssert( dval( shge3, 0 ).at( 0 ) == WIR_L4::bL );
  ufAssert( dval( shge4, 0 ).at( 0 ) == WIR_L4::b1 );

  ufAssert( dval( shgeu1, 0 ).getSignedValue() == 128 );
  ufAssert( dval( shgeu2, 0 ).getSignedValue() == 257 );
  ufAssert( dval( shgeu3, 0 ).extract( 1, 31 ).getSignedValue() == 257 );
  ufAssert( dval( shgeu3, 0 ).at( 0 ) == WIR_L4::bL );

  ufAssert( dval( shh1, 0 ).getSignedValue() == 0xF0005000 );
  ufAssert( uval( shh1, 1 ).extract( 0, 3 ).isBinaryInteger() );
  ufAssert( uval( shh1, 1 ).extract( 3, 13 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( shh1, 1 ).extract( 16, 4 ).isBinaryInteger() );
  ufAssert( uval( shh1, 1 ).extract( 20, 12 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( shh1, 2 ).extract( 5, 27 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( shh2, 0 ).getSignedValue() == 0x000F0005 );
  ufAssert( uval( shh2, 1 ).extract( 0, 12 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( shh2, 1 ).extract( 12, 3 ).isBinaryInteger() );
  ufAssert( uval( shh2, 1 ).extract( 15, 13 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( shh2, 1 ).extract( 28, 4 ).isBinaryInteger() );
  ufAssert( uval( shh2, 2 ).extract( 5, 27 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( shh3, 0 ).containsOnlyBit( WIR_L4::bL ) );

  ufAssert( dval( shlt1, 0 ).getSignedValue() == 128 );
  ufAssert( dval( shlt2, 0 ).getSignedValue() == 257 );
  ufAssert( dval( shlt3, 0 ).extract( 1, 31 ).getSignedValue() == 257 );
  ufAssert( dval( shlt3, 0 ).at( 0 ) == WIR_L4::bL );
  ufAssert( dval( shlt4, 0 ).at( 0 ) == WIR_L4::b0 );

  ufAssert( dval( shltu1, 0 ).getSignedValue() == 129 );
  ufAssert( dval( shltu2, 0 ).getSignedValue() == 258 );
  ufAssert( dval( shltu3, 0 ).extract( 1, 31 ).getSignedValue() == 258 );
  ufAssert( dval( shltu3, 0 ).at( 0 ) == WIR_L4::bL );

  ufAssert( dval( shne1, 0 ).getSignedValue() == 129 );
  ufAssert( uval( shne1, 0 ).extract( 0, 14 ).isBinaryInteger() );
  ufAssert( uval( shne1, 0 ).at( 14 ) == WIR_L4::bX );
  ufAssert( uval( shne1, 0 ).extract( 15, 16 ).isBinaryInteger() );
  ufAssert( uval( shne1, 0 ).at( 31 ) == WIR_L4::bX );
  ufAssert( uval( shne1, 1 ).extract( 0, 6 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( shne1, 1 ).at( 6 ) == WIR_L4::b1 );
  ufAssert( uval( shne1, 1 ).extract( 7, 25 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( shne1, 2 ).extract( 0, 6 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( shne1, 2 ).at( 6 ) == WIR_L4::b0 );
  ufAssert( uval( shne1, 2 ).extract( 7, 2 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( shne2, 0 ).getSignedValue() == 258 );
  ufAssert( uval( shne2, 0 ).extract( 0, 14 ).isBinaryInteger() );
  ufAssert( uval( shne2, 0 ).at( 14 ) == WIR_L4::bX );
  ufAssert( uval( shne2, 0 ).extract( 15, 16 ).isBinaryInteger() );
  ufAssert( uval( shne2, 0 ).at( 31 ) == WIR_L4::bX );
  ufAssert( uval( shne2, 1 ).isBinaryInteger() );
  ufAssert( uval( shne2, 2 ).isBinaryInteger() );

  ufAssert( dval( shne3, 0 ).extract( 1, 31 ).getSignedValue() == 258 );
  ufAssert( dval( shne3, 0 ).at( 0 ) == WIR_L4::bL );
  ufAssert( dval( shne4, 0 ).at( 0 ) == WIR_L4::b0 );

  ufAssert( dval( shandt1, 0 ).getSignedValue() == 129 );
  ufAssert( uval( shandt1, 0 ).extract( 0, 31 ).isBinaryInteger() );
  ufAssert( uval( shandt1, 0 ).at( 31 ) == WIR_L4::bX );
  ufAssert( uval( shandt1, 1 ).extract( 0, 25 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( shandt1, 1 ).at( 25 ) == WIR_L4::b1 );
  ufAssert( uval( shandt1, 1 ).extract( 26, 6 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( shandt1, 3 ).extract( 0, 6 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( shandt1, 3 ).at( 6 ) == WIR_L4::b1 );
  ufAssert( uval( shandt1, 3 ).extract( 7, 25 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( shandt2, 0 ).getSignedValue() == 258 );
  ufAssert( uval( shandt2, 0 ).extract( 0, 31 ).isBinaryInteger() );
  ufAssert( uval( shandt2, 0 ).at( 31 ) == WIR_L4::bX );
  ufAssert( uval( shandt2, 1 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( shandt2, 3 ).extract( 0, 5 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( shandt2, 3 ).at( 5 ) == WIR_L4::b0 );
  ufAssert( uval( shandt2, 3 ).extract( 6, 26 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( shandnt1, 0 ).getSignedValue() == 516 );
  ufAssert( uval( shandnt1, 0 ).extract( 0, 31 ).isBinaryInteger() );
  ufAssert( uval( shandnt1, 0 ).at( 31 ) == WIR_L4::bX );
  ufAssert( uval( shandnt1, 1 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( shandnt1, 3 ).extract( 0, 6 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( shandnt1, 3 ).at( 6 ) == WIR_L4::b1 );
  ufAssert(
    uval( shandnt1, 3 ).extract( 7, 25 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( shandnt2, 0 ).getSignedValue() == 1033 );
  ufAssert( uval( shandnt2, 0 ).extract( 0, 31 ).isBinaryInteger() );
  ufAssert( uval( shandnt2, 0 ).at( 31 ) == WIR_L4::bX );
  ufAssert(
    uval( shandnt2, 1 ).extract( 0, 25 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( shandnt2, 1 ).at( 25 ) == WIR_L4::b1 );
  ufAssert(
    uval( shandnt2, 1 ).extract( 26, 6 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( shandnt2, 3 ).extract( 0, 5 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( shandnt2, 3 ).at( 5 ) == WIR_L4::b0 );
  ufAssert(
    uval( shandnt2, 3 ).extract( 6, 26 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( shnandt1, 0 ).getSignedValue() == 2066 );
  ufAssert( uval( shnandt1, 0 ).extract( 0, 31 ).isBinaryInteger() );
  ufAssert( uval( shnandt1, 0 ).at( 31 ) == WIR_L4::bX );
  ufAssert(
    uval( shnandt1, 1 ).extract( 0, 25 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( shnandt1, 1 ).at( 25 ) == WIR_L4::b1 );
  ufAssert(
    uval( shnandt1, 1 ).extract( 26, 6 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( shnandt1, 3 ).extract( 0, 6 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( shnandt1, 3 ).at( 6 ) == WIR_L4::b1 );
  ufAssert(
    uval( shnandt1, 3 ).extract( 7, 25 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( shnandt2, 0 ).getSignedValue() == 4133 );
  ufAssert( uval( shnandt2, 0 ).extract( 0, 31 ).isBinaryInteger() );
  ufAssert( uval( shnandt2, 0 ).at( 31 ) == WIR_L4::bX );
  ufAssert( uval( shnandt2, 1 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( shnandt2, 3 ).extract( 0, 5 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( shnandt2, 3 ).at( 5 ) == WIR_L4::b0 );
  ufAssert(
    uval( shnandt2, 3 ).extract( 6, 26 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( shnort1, 0 ).getSignedValue() == 8267 );
  ufAssert( uval( shnort1, 0 ).extract( 0, 31 ).isBinaryInteger() );
  ufAssert( uval( shnort1, 0 ).at( 31 ) == WIR_L4::bX );
  ufAssert( uval( shnort1, 1 ).extract( 0, 3 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( shnort1, 1 ).at( 3 ) == WIR_L4::b0 );
  ufAssert( uval( shnort1, 1 ).extract( 4, 28 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( shnort1, 3 ).extract( 0, 5 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( shnort1, 3 ).at( 5 ) == WIR_L4::b0 );
  ufAssert( uval( shnort1, 3 ).extract( 6, 26 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( shnort2, 0 ).getSignedValue() == 16534 );
  ufAssert( uval( shnort2, 0 ).extract( 0, 31 ).isBinaryInteger() );
  ufAssert( uval( shnort2, 0 ).at( 31 ) == WIR_L4::bX );
  ufAssert( uval( shnort2, 1 ).extract( 0, 2 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( shnort2, 1 ).at( 2 ) == WIR_L4::b1 );
  ufAssert( uval( shnort2, 1 ).extract( 3, 29 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( shnort2, 3 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( short1, 0 ).getSignedValue() == 33069 );
  ufAssert( uval( short1, 0 ).extract( 0, 31 ).isBinaryInteger() );
  ufAssert( uval( short1, 0 ).at( 31 ) == WIR_L4::bX );
  ufAssert( uval( short1, 1 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( short1, 3 ).extract( 0, 6 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( short1, 3 ).at( 6 ) == WIR_L4::b1 );
  ufAssert( uval( short1, 3 ).extract( 7, 25 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( short2, 0 ).getSignedValue() == 66138 );
  ufAssert( uval( short2, 0 ).extract( 0, 31 ).isBinaryInteger() );
  ufAssert( uval( short2, 0 ).at( 31 ) == WIR_L4::bX );
  ufAssert( uval( short2, 1 ).extract( 0, 3 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( short2, 1 ).at( 3 ) == WIR_L4::b0 );
  ufAssert( uval( short2, 1 ).extract( 4, 28 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( short2, 3 ).extract( 0, 5 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( short2, 3 ).at( 5 ) == WIR_L4::b0 );
  ufAssert( uval( short2, 3 ).extract( 6, 26 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( shornt1, 0 ).getSignedValue() == 132276 );
  ufAssert( uval( shornt1, 0 ).extract( 0, 31 ).isBinaryInteger() );
  ufAssert( uval( shornt1, 0 ).at( 31 ) == WIR_L4::bX );
  ufAssert( uval( shornt1, 1 ).extract( 0, 3 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( shornt1, 1 ).at( 3 ) == WIR_L4::b0 );
  ufAssert( uval( shornt1, 1 ).extract( 4, 28 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( shornt1, 3 ).extract( 0, 6 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( shornt1, 3 ).at( 6 ) == WIR_L4::b1 );
  ufAssert( uval( shornt1, 3 ).extract( 7, 25 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( shornt2, 0 ).getSignedValue() == 264553 );
  ufAssert( uval( shornt2, 0 ).extract( 0, 31 ).isBinaryInteger() );
  ufAssert( uval( shornt2, 0 ).at( 31 ) == WIR_L4::bX );
  ufAssert( uval( shornt2, 1 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( shornt2, 3 ).extract( 0, 5 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( shornt2, 3 ).at( 5 ) == WIR_L4::b0 );
  ufAssert( uval( shornt2, 3 ).extract( 6, 26 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( shxnort1, 0 ).getSignedValue() == 529106 );
  ufAssert( dval( shxnort2, 0 ).getSignedValue() == 1058213 );

  ufAssert( dval( shxort1, 0 ).getSignedValue() == 2116427 );
  ufAssert( dval( shxort2, 0 ).getSignedValue() == 4232854 );

  ufAssert( dval( sha1, 0 ).getSignedValue() == -503808 );
  ufAssert( dval( sha2, 0 ).getSignedValue() == -123 );
  ufAssert( dval( sha3, 0 ).containsOnlyBit( WIR_L4::bL ) );

  ufAssert( dval( shah1, 0 ).getSignedValue() == 0xF0005000 );

  ufAssert( dval( shah2, 0 ).getSignedValue() == 0xFFFF0005 );
  ufAssert( uval( shah2, 1 ).extract( 0, 12 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( shah2, 1 ).extract( 12, 4 ).isBinaryInteger() );
  ufAssert( uval( shah2, 1 ).extract( 16, 12 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( shah2, 1 ).extract( 28, 4 ).isBinaryInteger() );
  ufAssert( uval( shah2, 2 ).extract( 5, 27 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( shah3, 0 ).containsOnlyBit( WIR_L4::bL ) );

  ufAssert( dval( shah4, 0 ).extract( 0, 16 ).containsOnlyBit( WIR_L4::b0 ) );
  ufAssert( dval( shah4, 0 ).extract( 16, 16 ).containsOnlyBit( WIR_L4::b1 ) );
  ufAssert( uval( shah4, 1 ).extract( 0, 15 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( shah4, 1 ).at( 15 ) == WIR_L4::b0 );
  ufAssert( uval( shah4, 1 ).extract( 16, 15 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( shah4, 1 ).at( 31 ) == WIR_L4::b1 );
  ufAssert( uval( shah4, 2 ).extract( 5, 4 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( shas1, 0 ).getSignedValue() == -503808 );
  ufAssert( dval( shas2, 0 ).getSignedValue() == -123 );
  ufAssert( dval( shas3, 0 ).getSignedValue() == -2147483648 );
  ufAssert( dval( shas4, 0 ).getSignedValue() == 2147483647 );
  ufAssert( dval( shas5, 0 ).getSignedValue() == -31 );
  ufAssert( dval( shas6, 0 ).getSignedValue() == 16 );

  return( 0 );
}
