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
  // operations SUB & co.

  TC131 p;
  WIR_CompilationUnit c;
  c.setName( __FILE__ );
  WIR_Function &f = c.pushBackFunction( WIR_Function { "main" } );
  WIR_BasicBlock &b1 = f.pushBackBasicBlock( {} );
  const TC_DRegP &D4 = p.D4();
  const TC_ARegP &A4 = p.A4();
  const TC_ARegP &SP = p.SP();
  const TC_PSWBit &pswC = p.PSW_C();

  auto &dExt = f.pushBackVirtualRegister( TC_DRegV() );
  auto &dMi123 = f.pushBackVirtualRegister( TC_DRegV() );
  auto &dPl64 = f.pushBackVirtualRegister( TC_DRegV() );
  auto &aF000 = f.pushBackVirtualRegister( TC_ARegV() );
  auto &tmp0 = f.pushBackVirtualRegister( TC_DRegV() );
  auto &tmp1 = f.pushBackVirtualRegister( TC_DRegV() );
  auto &maxNeg = f.pushBackVirtualRegister( TC_DRegV() );
  auto &maxPos = f.pushBackVirtualRegister( TC_DRegV() );
  auto &bitMask = f.pushBackVirtualRegister( TC_DRegV() );

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
    { TC131::OpCode::MOVH, TC131::OperationFormat::DC16_2,
      new WIR_RegisterParameter( maxNeg, WIR_Usage::def ),
      new TC_Const16_Unsigned( 32768 ) } );

  tcop(
    { TC131::OpCode::MOVH, TC131::OperationFormat::DC16_2,
      new WIR_RegisterParameter( maxPos, WIR_Usage::def ),
      new TC_Const16_Unsigned( 32768 ) } );

  tcop(
    { TC131::OpCode::ADDI, TC131::OperationFormat::DDC16_1,
      new WIR_RegisterParameter( maxPos, WIR_Usage::def ),
      new WIR_RegisterParameter( maxPos, WIR_Usage::use ),
      new TC_Const16_Signed( -1 ) } );

  tcop(
    { TC131::OpCode::MOVH_A, TC131::OperationFormat::AC16,
      new WIR_RegisterParameter( aF000, WIR_Usage::def ),
      new TC_Const16_Unsigned( 0xF000 ) } );

  tcop(
    { TC131::OpCode::MOVH, TC131::OperationFormat::DC16_2,
      new WIR_RegisterParameter( bitMask, WIR_Usage::def ),
      new TC_Const16_Unsigned( 0x0F00 ) } );

  // SUB
  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ) } );

  auto &sub1 = tcop(
    { TC131::OpCode::SUB, TC131::OperationFormat::SDD_2,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &sub2 = tcop(
    { TC131::OpCode::SUB, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &sub3 = tcop(
    { TC131::OpCode::SUB, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new WIR_RegisterParameter( maxPos, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &sub4 = tcop(
    { TC131::OpCode::SUB, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new WIR_RegisterParameter( maxNeg, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::AND, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( bitMask, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &sub5 = tcop(
    { TC131::OpCode::SUB, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // SUB.A
  tcop(
    { TC131::OpCode::MOV_AA, TC131::OperationFormat::SAA_1,
      new WIR_RegisterParameter( SP, WIR_Usage::def ),
      new WIR_RegisterParameter( aF000, WIR_Usage::use ) } );

  auto &suba1 = tcop(
    { TC131::OpCode::SUB_A, TC131::OperationFormat::SSPC8,
      new WIR_RegisterParameter( SP, WIR_Usage::defuse ),
      new TC_Const8_Unsigned( 5 ) } );

  tcop(
    { TC131::OpCode::MOV_AA, TC131::OperationFormat::SAA_1,
      new WIR_RegisterParameter( A4, WIR_Usage::def ),
      new WIR_RegisterParameter( SP, WIR_Usage::use ) } );

  // SUB.B
  tcop(
    { TC131::OpCode::MOVH, TC131::OperationFormat::DC16_2,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new TC_Const16_Unsigned( 0x7F80 ) } );

  tcop(
    { TC131::OpCode::ADDI, TC131::OperationFormat::DDC16_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new TC_Const16_Signed( 0x4085 ) } );

  tcop(
    { TC131::OpCode::MOVH, TC131::OperationFormat::DC16_2,
      new WIR_RegisterParameter( tmp1, WIR_Usage::def ),
      new TC_Const16_Unsigned( 0x4300 ) } );

  tcop(
    { TC131::OpCode::ADDI, TC131::OperationFormat::DDC16_1,
      new WIR_RegisterParameter( tmp1, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ),
      new TC_Const16_Signed( -17795 ) } );

  auto &subb1 = tcop(
    { TC131::OpCode::SUB_B, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOVH, TC131::OperationFormat::DC16_2,
      new WIR_RegisterParameter( bitMask, WIR_Usage::def ),
      new TC_Const16_Unsigned( 0xF00F ) } );

  tcop(
    { TC131::OpCode::ADDI, TC131::OperationFormat::DDC16_1,
      new WIR_RegisterParameter( bitMask, WIR_Usage::def ),
      new WIR_RegisterParameter( bitMask, WIR_Usage::use ),
      new TC_Const16_Signed( 64 ) } );

  tcop(
    { TC131::OpCode::AND, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( bitMask, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &subb2 = tcop(
    { TC131::OpCode::SUB_B, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // SUB.H
  tcop(
    { TC131::OpCode::MOVH, TC131::OperationFormat::DC16_2,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new TC_Const16_Unsigned( 0x7F80 ) } );

  tcop(
    { TC131::OpCode::ADDI, TC131::OperationFormat::DDC16_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new TC_Const16_Signed( 0x4085 ) } );

  auto &subh1 = tcop(
    { TC131::OpCode::SUB_H, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOVH, TC131::OperationFormat::DC16_2,
      new WIR_RegisterParameter( bitMask, WIR_Usage::def ),
      new TC_Const16_Unsigned( 0xF00F ) } );

  tcop(
    { TC131::OpCode::ADDI, TC131::OperationFormat::DDC16_1,
      new WIR_RegisterParameter( bitMask, WIR_Usage::def ),
      new WIR_RegisterParameter( bitMask, WIR_Usage::use ),
      new TC_Const16_Signed( 0x00FF ) } );

  tcop(
    { TC131::OpCode::AND, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( bitMask, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &subh2 = tcop(
    { TC131::OpCode::SUB_H, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // SUBS
  auto &subs1 = tcop(
    { TC131::OpCode::SUBS, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &subs2 = tcop(
    { TC131::OpCode::SUBS, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new WIR_RegisterParameter( maxPos, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &subs3 = tcop(
    { TC131::OpCode::SUBS, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new WIR_RegisterParameter( maxNeg, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &subs4 = tcop(
    { TC131::OpCode::SUBS, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // SUBS.U
  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new TC_Const16_Signed( -13 ) } );

  auto &subsu1 = tcop(
    { TC131::OpCode::SUBS_U, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new TC_Const16_Signed( 13 ) } );

  auto &subsu2 = tcop(
    { TC131::OpCode::SUBS_U, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &subsu3 = tcop(
    { TC131::OpCode::SUBS_U, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // SUBS.H
  tcop(
    { TC131::OpCode::MOVH, TC131::OperationFormat::DC16_2,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new TC_Const16_Unsigned( 32701 ) } );

  tcop(
    { TC131::OpCode::ADDI, TC131::OperationFormat::DDC16_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new TC_Const16_Signed( -32700 ) } );

  tcop(
    { TC131::OpCode::MOVH, TC131::OperationFormat::DC16_2,
      new WIR_RegisterParameter( tmp1, WIR_Usage::def ),
      new TC_Const16_Unsigned( 65436 ) } );

  tcop(
    { TC131::OpCode::ADDI, TC131::OperationFormat::DDC16_1,
      new WIR_RegisterParameter( tmp1, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ),
      new TC_Const16_Signed( 100 ) } );

  auto &subsh1 = tcop(
    { TC131::OpCode::SUBS_H, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOVH, TC131::OperationFormat::DC16_2,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new TC_Const16_Unsigned( 32701 ) } );

  tcop(
    { TC131::OpCode::ADDI, TC131::OperationFormat::DDC16_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new TC_Const16_Signed( -32700 ) } );

  tcop(
    { TC131::OpCode::MOVH, TC131::OperationFormat::DC16_2,
      new WIR_RegisterParameter( tmp1, WIR_Usage::def ),
      new TC_Const16_Unsigned( 11 ) } );

  tcop(
    { TC131::OpCode::ADDI, TC131::OperationFormat::DDC16_1,
      new WIR_RegisterParameter( tmp1, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ),
      new TC_Const16_Signed( -10 ) } );

  auto &subsh2 = tcop(
    { TC131::OpCode::SUBS_H, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &subsh3 = tcop(
    { TC131::OpCode::SUBS_H, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // SUBS.HU
  tcop(
    { TC131::OpCode::MOVH, TC131::OperationFormat::DC16_2,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new TC_Const16_Unsigned( 65500 ) } );

  tcop(
    { TC131::OpCode::MOVH, TC131::OperationFormat::DC16_2,
      new WIR_RegisterParameter( tmp1, WIR_Usage::def ),
      new TC_Const16_Unsigned( 0xFF9C ) } );

  tcop(
    { TC131::OpCode::ADDI, TC131::OperationFormat::DDC16_1,
      new WIR_RegisterParameter( tmp1, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ),
      new TC_Const16_Signed( 100 ) } );

  auto &subshu1 = tcop(
    { TC131::OpCode::SUBS_HU, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOVH, TC131::OperationFormat::DC16_2,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new TC_Const16_Unsigned( 32701 ) } );

  tcop(
    { TC131::OpCode::ADDI, TC131::OperationFormat::DDC16_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new TC_Const16_Signed( 5000 ) } );

  tcop(
    { TC131::OpCode::MOVH, TC131::OperationFormat::DC16_2,
      new WIR_RegisterParameter( tmp1, WIR_Usage::def ),
      new TC_Const16_Unsigned( 11 ) } );

  tcop(
    { TC131::OpCode::ADDI, TC131::OperationFormat::DDC16_1,
      new WIR_RegisterParameter( tmp1, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ),
      new TC_Const16_Signed( 42 ) } );

  auto &subshu2 = tcop(
    { TC131::OpCode::SUBS_HU, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // SUBX
  auto &subx1 = tcop(
    { TC131::OpCode::SUBX, TC131::OperationFormat::DDDPSW_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new WIR_RegisterParameter( pswC, WIR_Usage::def ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &subx2 = tcop(
    { TC131::OpCode::SUBX, TC131::OperationFormat::DDDPSW_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new WIR_RegisterParameter( pswC, WIR_Usage::def ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &subx3 = tcop(
    { TC131::OpCode::SUBX, TC131::OperationFormat::DDDPSW_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new WIR_RegisterParameter( maxPos, WIR_Usage::use ),
      new WIR_RegisterParameter( pswC, WIR_Usage::def ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &subx4 = tcop(
    { TC131::OpCode::SUBX, TC131::OperationFormat::DDDPSW_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new WIR_RegisterParameter( maxNeg, WIR_Usage::use ),
      new WIR_RegisterParameter( pswC, WIR_Usage::def ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // Do bit-true data flow analysis.
  TC_BitDFA a { f };
  a.analyze();

  // Evaluate analysis results.
  ufAssert( dval( sub1, 0 ).extract( 1, 31 ).containsOnlyBit( WIR_L4::bL ) );
  ufAssert( dval( sub1, 0 ).at( 0 ) == WIR_L4::bN );
  ufAssert( dval( sub2, 0 ).getSignedValue() == -187 );
  ufAssert( dval( sub3, 0 ).getSignedValue() == 2147483526 );

  ufAssert( dval( sub4, 0 ).getSignedValue() == -2147483584 );
  ufAssert( uval( sub4, 1 ).extract( 28, 4 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( sub4, 1 ).extract( 0, 28 ).isBinaryInteger() );
  ufAssert( uval( sub4, 2 ).extract( 28, 4 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( sub4, 2 ).extract( 0, 28 ).isBinaryInteger() );

  ufAssert( dval( sub5, 0 ).getSignedValue() == 0 );

  ufAssert( dval( suba1, 0 ).getSignedValue() == 0xEFFFFFFB );

  ufAssert( dval( subb1, 0 ).extract( 24, 8 ).getSignedValue() == 61 );
  ufAssert( dval( subb1, 0 ).extract( 16, 8 ).getSignedValue() == 129 );
  ufAssert( dval( subb1, 0 ).extract( 8, 8 ).getSignedValue() == 134 );
  ufAssert( dval( subb1, 0 ).extract( 0, 8 ).getSignedValue() == 8 );
  ufAssert( uval( subb1, 1 ).extract( 24, 8 ).isBinaryInteger() );
  ufAssert( uval( subb1, 1 ).extract( 20, 4 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( subb1, 1 ).extract( 16, 4 ).isBinaryInteger() );
  ufAssert( uval( subb1, 1 ).extract( 8, 8 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( subb1, 1 ).at( 7 ) == WIR_L4::bX );
  ufAssert( uval( subb1, 1 ).extract( 0, 7 ).isBinaryInteger() );
  ufAssert( uval( subb1, 2 ).extract( 24, 8 ).isBinaryInteger() );
  ufAssert( uval( subb1, 2 ).extract( 20, 4 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( subb1, 2 ).extract( 16, 4 ).isBinaryInteger() );
  ufAssert( uval( subb1, 2 ).extract( 8, 8 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( subb1, 2 ).at( 7 ) == WIR_L4::bX );
  ufAssert( uval( subb1, 2 ).extract( 0, 7 ).isBinaryInteger() );

  ufAssert( dval( subb2, 0 ).getSignedValue() == 0 );

  ufAssert( dval( subh1, 0 ).extract( 16, 16 ).getSignedValue() == 15489 );
  ufAssert( dval( subh1, 0 ).extract( 0, 16 ).getSignedValue() == 34312 );
  ufAssert( uval( subh1, 1 ).extract( 16, 16 ).isBinaryInteger() );
  ufAssert( uval( subh1, 1 ).extract( 8, 8 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( subh1, 1 ).extract( 0, 8 ).isBinaryInteger() );
  ufAssert( uval( subh1, 2 ).extract( 16, 16 ).isBinaryInteger() );
  ufAssert( uval( subh1, 2 ).extract( 8, 8 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( subh1, 2 ).extract( 0, 8 ).isBinaryInteger() );

  ufAssert( dval( subh2, 0 ).getSignedValue() == 0 );

  ufAssert( dval( subs1, 0 ).getSignedValue() == -187 );
  ufAssert( dval( subs2, 0 ).getSignedValue() == -2147483648 );
  ufAssert( dval( subs3, 0 ).getSignedValue() == 2147483647 );
  ufAssert( dval( subs4, 0 ).getSignedValue() == 0 );

  ufAssert( dval( subsu1, 0 ).getSignedValue() == 0 );
  ufAssert( dval( subsu2, 0 ).getSignedValue() == 51 );
  ufAssert( dval( subsu3, 0 ).getSignedValue() == 0 );

  ufAssert( dval( subsh1, 0 ).extract( 16, 16 ).getSignedValue() == 32767 );
  ufAssert( dval( subsh1, 0 ).extract( 0, 16 ).getSignedValue() == 32768 );
  ufAssert( dval( subsh2, 0 ).extract( 16, 16 ).getSignedValue() == 32690 );
  ufAssert( dval( subsh2, 0 ).extract( 0, 16 ).getSignedValue() == 32846 );
  ufAssert( dval( subsh3, 0 ).getSignedValue() == 0 );

  ufAssert( dval( subshu1, 0 ).extract( 16, 16 ).getSignedValue() == 64 );
  ufAssert( dval( subshu1, 0 ).extract( 0, 16 ).getSignedValue() == 0 );
  ufAssert( dval( subshu2, 0 ).extract( 16, 16 ).getSignedValue() == 32690 );
  ufAssert( dval( subshu2, 0 ).extract( 0, 16 ).getSignedValue() == 4958 );

  ufAssert( dval( subx1, 0 ).extract( 1, 31 ).containsOnlyBit( WIR_L4::bL ) );
  ufAssert( dval( subx1, 0 ).at( 0 ) == WIR_L4::bN );
  ufAssert( dval( subx2, 0 ).getSignedValue() == -187 );
  ufAssert( dval( subx3, 0 ).getSignedValue() == 2147483526 );
  ufAssert( dval( subx4, 0 ).getSignedValue() == -2147483584 );

  return( 0 );
}
