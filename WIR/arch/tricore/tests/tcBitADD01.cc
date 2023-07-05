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
  // operations ADD & co.

  TC131 p;
  WIR_CompilationUnit c;
  c.setName( __FILE__ );
  WIR_Function &f = c.pushBackFunction( WIR_Function { "main" } );
  WIR_BasicBlock &b1 = f.pushBackBasicBlock( {} );
  const TC_DRegP &D4 = p.D4();
  const TC_ARegP &A4 = p.A4();
  auto &dExt = f.pushBackVirtualRegister( TC_DRegV() );
  auto &dMi123 = f.pushBackVirtualRegister( TC_DRegV() );
  auto &dPl64 = f.pushBackVirtualRegister( TC_DRegV() );
  auto &aF000 = f.pushBackVirtualRegister( TC_ARegV() );
  auto &bitMask = f.pushBackVirtualRegister( TC_DRegV() );
  auto &tmp0 = f.pushBackVirtualRegister( TC_DRegV() );
  auto &tmp1 = f.pushBackVirtualRegister( TC_ARegV() );
  auto &tmp2 = f.pushBackVirtualRegister( TC_DRegV() );
  auto &maxNeg = f.pushBackVirtualRegister( TC_DRegV() );
  auto &maxPos = f.pushBackVirtualRegister( TC_DRegV() );

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

  // ADD
  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ) } );

  auto &add1 = tcop(
    { TC131::OpCode::ADD, TC131::OperationFormat::SDD_2,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &add2 = tcop(
    { TC131::OpCode::ADD, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &add3 = tcop(
    { TC131::OpCode::ADD, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new WIR_RegisterParameter( maxNeg, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &add4 = tcop(
    { TC131::OpCode::ADD, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( maxPos, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::AND, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( bitMask, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // ADD.A
  tcop(
    { TC131::OpCode::MOV_AA, TC131::OperationFormat::SAA_1,
      new WIR_RegisterParameter( tmp1, WIR_Usage::def ),
      new WIR_RegisterParameter( aF000, WIR_Usage::use ) } );

  auto &adda1 = tcop(
    { TC131::OpCode::ADD_A, TC131::OperationFormat::SAC4_2,
      new WIR_RegisterParameter( tmp1, WIR_Usage::defuse ),
      new TC_Const4_Signed( -5 ) } );

  tcop(
    { TC131::OpCode::MOV_AA, TC131::OperationFormat::SAA_1,
      new WIR_RegisterParameter( A4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ) } );

  auto &adda2 = tcop(
    { TC131::OpCode::ADD_A, TC131::OperationFormat::SAC4_2,
      new WIR_RegisterParameter( tmp1, WIR_Usage::defuse ),
      new TC_Const4_Signed( 5 ) } );

  tcop(
    { TC131::OpCode::MOV_AA, TC131::OperationFormat::SAA_1,
      new WIR_RegisterParameter( A4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ) } );

  // ADD.B
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
      new WIR_RegisterParameter( tmp2, WIR_Usage::def ),
      new TC_Const16_Unsigned( 0x4300 ) } );

  tcop(
    { TC131::OpCode::ADDI, TC131::OperationFormat::DDC16_1,
      new WIR_RegisterParameter( tmp2, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp2, WIR_Usage::use ),
      new TC_Const16_Signed( -17795 ) } );

  auto &addb1 = tcop(
    { TC131::OpCode::ADD_B, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( tmp2, WIR_Usage::use ) } );

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

  // ADD.H
  tcop(
    { TC131::OpCode::MOVH, TC131::OperationFormat::DC16_2,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new TC_Const16_Unsigned( 0x7F80 ) } );

  tcop(
    { TC131::OpCode::ADDI, TC131::OperationFormat::DDC16_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new TC_Const16_Signed( 0x4085 ) } );

  auto &addh1 = tcop(
    { TC131::OpCode::ADD_H, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( tmp2, WIR_Usage::use ) } );

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

  // ADDI: Tested implicitly above.

  // ADDIH
  tcop(
    { TC131::OpCode::MOVH, TC131::OperationFormat::DC16_2,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new TC_Const16_Unsigned( 0x7F80 ) } );

  auto &addih1 = tcop(
    { TC131::OpCode::ADDIH, TC131::OperationFormat::DDC16_2,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new TC_Const16_Unsigned( 26361 ) } );

  tcop(
    { TC131::OpCode::MOVH, TC131::OperationFormat::DC16_2,
      new WIR_RegisterParameter( bitMask, WIR_Usage::def ),
      new TC_Const16_Unsigned( 0xF ) } );

  tcop(
    { TC131::OpCode::AND, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( bitMask, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // ADDIH.A
  tcop(
    { TC131::OpCode::MOVH_A, TC131::OperationFormat::AC16,
      new WIR_RegisterParameter( tmp1, WIR_Usage::def ),
      new TC_Const16_Unsigned( 0x0F0F ) } );

  auto &addiha1 = tcop(
    { TC131::OpCode::ADDIH_A, TC131::OperationFormat::AAC16,
      new WIR_RegisterParameter( tmp1, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ),
      new TC_Const16_Unsigned( 26361 ) } );

  tcop(
    { TC131::OpCode::MOV_AA, TC131::OperationFormat::SAA_1,
      new WIR_RegisterParameter( A4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOVH_A, TC131::OperationFormat::AC16,
      new WIR_RegisterParameter( tmp1, WIR_Usage::def ),
      new TC_Const16_Unsigned( 0xFFFF ) } );

  auto &addiha2 = tcop(
    { TC131::OpCode::ADDIH_A, TC131::OperationFormat::AAC16,
      new WIR_RegisterParameter( tmp1, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ),
      new TC_Const16_Unsigned( 2 ) } );

  tcop(
    { TC131::OpCode::MOV_AA, TC131::OperationFormat::SAA_1,
      new WIR_RegisterParameter( A4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ) } );

  // ADDS

  auto &adds1 = tcop(
    { TC131::OpCode::ADDS, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &adds2 = tcop(
    { TC131::OpCode::ADDS, TC131::OperationFormat::DDC9_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( maxNeg, WIR_Usage::use ),
      new TC_Const9_Signed( -128 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &adds3 = tcop(
    { TC131::OpCode::ADDS, TC131::OperationFormat::DDC9_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( maxPos, WIR_Usage::use ),
      new TC_Const9_Signed( 128 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // ADDS.H
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
      new WIR_RegisterParameter( tmp2, WIR_Usage::def ),
      new TC_Const16_Unsigned( 100 ) } );

  tcop(
    { TC131::OpCode::ADDI, TC131::OperationFormat::DDC16_1,
      new WIR_RegisterParameter( tmp2, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp2, WIR_Usage::use ),
      new TC_Const16_Signed( -100 ) } );

  auto &addsh1 = tcop(
    { TC131::OpCode::ADDS_H, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( tmp2, WIR_Usage::use ) } );

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
      new WIR_RegisterParameter( tmp2, WIR_Usage::def ),
      new TC_Const16_Unsigned( 11 ) } );

  tcop(
    { TC131::OpCode::ADDI, TC131::OperationFormat::DDC16_1,
      new WIR_RegisterParameter( tmp2, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp2, WIR_Usage::use ),
      new TC_Const16_Signed( -10 ) } );

  auto &addsh2 = tcop(
    { TC131::OpCode::ADDS_H, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( tmp2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // ADDS.HU
  tcop(
    { TC131::OpCode::MOVH, TC131::OperationFormat::DC16_2,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new TC_Const16_Unsigned( 65501 ) } );

  tcop(
    { TC131::OpCode::ADDI, TC131::OperationFormat::DDC16_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new TC_Const16_Signed( -32768 ) } );

  tcop(
    { TC131::OpCode::MOVH, TC131::OperationFormat::DC16_2,
      new WIR_RegisterParameter( tmp2, WIR_Usage::def ),
      new TC_Const16_Unsigned( 101 ) } );

  tcop(
    { TC131::OpCode::ADDI, TC131::OperationFormat::DDC16_1,
      new WIR_RegisterParameter( tmp2, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp2, WIR_Usage::use ),
      new TC_Const16_Signed( -100 ) } );

  auto &addshu1 = tcop(
    { TC131::OpCode::ADDS_HU, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( tmp2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // ADDS.U
  auto &addsu1 = tcop(
    { TC131::OpCode::ADDS_U, TC131::OperationFormat::DDC9_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const9_Signed( -13 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &addsu2 = tcop(
    { TC131::OpCode::ADDS_U, TC131::OperationFormat::DDC9_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const9_Signed( 13 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // ADDSC.A
  auto &addsca1 = tcop(
    { TC131::OpCode::ADDSC_A, TC131::OperationFormat::AADC2,
      new WIR_RegisterParameter( tmp1, WIR_Usage::def ),
      new WIR_RegisterParameter( aF000, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const2_Unsigned( 2 ) } );

  tcop(
    { TC131::OpCode::MOVH, TC131::OperationFormat::DC16_2,
      new WIR_RegisterParameter( bitMask, WIR_Usage::def ),
      new TC_Const16_Unsigned( 0x0F00 ) } );

  tcop(
    { TC131::OpCode::MOV_D, TC131::OperationFormat::DA,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::AND, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( bitMask, WIR_Usage::use ) } );

  // ADDSC.AT
  tcop(
    { TC131::OpCode::MOVH_A, TC131::OperationFormat::AC16,
      new WIR_RegisterParameter( tmp1, WIR_Usage::def ),
      new TC_Const16_Unsigned( 0xF001 ) } );

  tcop(
    { TC131::OpCode::ADD_A, TC131::OperationFormat::SAC4_2,
      new WIR_RegisterParameter( tmp1, WIR_Usage::defuse ),
      new TC_Const4_Signed( -1 ) } );

  auto &addscat1 = tcop(
    { TC131::OpCode::ADDSC_AT, TC131::OperationFormat::AAD,
      new WIR_RegisterParameter( tmp1, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOVH, TC131::OperationFormat::DC16_2,
      new WIR_RegisterParameter( bitMask, WIR_Usage::def ),
      new TC_Const16_Unsigned( 0x1F00 ) } );

  tcop(
    { TC131::OpCode::MOV_D, TC131::OperationFormat::DA,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::AND, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( bitMask, WIR_Usage::use ) } );

  // ADDX
  auto &addx1 = tcop(
    { TC131::OpCode::ADD, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &addx2 = tcop(
    { TC131::OpCode::ADD, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new WIR_RegisterParameter( maxNeg, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &addx3 = tcop(
    { TC131::OpCode::ADD, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( maxPos, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // Do bit-true data flow analysis.
  TC_BitDFA a { f };
  a.analyze();

  // Evaluate analysis results.
  ufAssert( dval( add1, 0 ).extract( 1, 31 ).containsOnlyBit( WIR_L4::bL ) );
  ufAssert( dval( add1, 0 ).at( 0 ) == WIR_L4::bN );
  ufAssert( dval( add2, 0 ).getSignedValue() == -59 );
  ufAssert( dval( add3, 0 ).getSignedValue() == 2147483525 );

  ufAssert( dval( add4, 0 ).getSignedValue() == -2147483585 );
  ufAssert( uval( add4, 1 ).extract( 28, 4 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( add4, 1 ).extract( 0, 28 ).isBinaryInteger() );
  ufAssert( uval( add4, 2 ).extract( 28, 4 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( add4, 2 ).extract( 0, 28 ).isBinaryInteger() );

  ufAssert( dval( adda1, 0 ).getSignedValue() == 0xEFFFFFFB );
  ufAssert( dval( adda2, 0 ).getSignedValue() == 0xF0000000 );

  ufAssert( dval( addb1, 0 ).extract( 24, 8 ).getSignedValue() == 193 );
  ufAssert( dval( addb1, 0 ).extract( 16, 8 ).getSignedValue() == 127 );
  ufAssert( dval( addb1, 0 ).extract( 8, 8 ).getSignedValue() == 250 );
  ufAssert( dval( addb1, 0 ).extract( 0, 8 ).getSignedValue() == 2 );
  ufAssert( uval( addb1, 1 ).extract( 24, 8 ).isBinaryInteger() );
  ufAssert( uval( addb1, 1 ).extract( 20, 4 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( addb1, 1 ).extract( 16, 4 ).isBinaryInteger() );
  ufAssert( uval( addb1, 1 ).extract( 8, 8 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( addb1, 1 ).at( 7 ) == WIR_L4::bX );
  ufAssert( uval( addb1, 1 ).extract( 0, 7 ).isBinaryInteger() );
  ufAssert( uval( addb1, 2 ).extract( 24, 8 ).isBinaryInteger() );
  ufAssert( uval( addb1, 2 ).extract( 20, 4 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( addb1, 2 ).extract( 16, 4 ).isBinaryInteger() );
  ufAssert( uval( addb1, 2 ).extract( 8, 8 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( addb1, 2 ).at( 7 ) == WIR_L4::bX );
  ufAssert( uval( addb1, 2 ).extract( 0, 7 ).isBinaryInteger() );

  ufAssert( dval( addh1, 0 ).extract( 16, 16 ).getSignedValue() == 49791 );
  ufAssert( dval( addh1, 0 ).extract( 0, 16 ).getSignedValue() == 64258 );
  ufAssert( uval( addh1, 1 ).extract( 16, 16 ).isBinaryInteger() );
  ufAssert( uval( addh1, 1 ).extract( 8, 8 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( addh1, 1 ).extract( 0, 8 ).isBinaryInteger() );
  ufAssert( uval( addh1, 2 ).extract( 16, 16 ).isBinaryInteger() );
  ufAssert( uval( addh1, 2 ).extract( 8, 8 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( addh1, 2 ).extract( 0, 8 ).isBinaryInteger() );

  ufAssert( dval( addih1, 0 ).getSignedValue() == 0xE6790000 );
  ufAssert( uval( addih1, 1 ).extract( 20, 12 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( addih1, 1 ).extract( 0, 20 ).isBinaryInteger() );
  ufAssert( uval( addih1, 2 ).extract( 4, 12 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( addih1, 2 ).extract( 0, 4 ).isBinaryInteger() );

  ufAssert( dval( addiha1, 0 ).getSignedValue() == 0x76080000 );
  ufAssert( dval( addiha2, 0 ).getSignedValue() == 0x10000 );

  ufAssert( dval( adds1, 0 ).getSignedValue() == -59 );
  ufAssert( dval( adds2, 0 ).getSignedValue() == -2147483648 );
  ufAssert( dval( adds3, 0 ).getSignedValue() == 2147483647 );

  ufAssert( dval( addsh1, 0 ).extract( 16, 16 ).getSignedValue() == 32767 );
  ufAssert( dval( addsh1, 0 ).extract( 0, 16 ).getSignedValue() == 32768 );
  ufAssert( dval( addsh2, 0 ).extract( 16, 16 ).getSignedValue() == 32710 );
  ufAssert( dval( addsh2, 0 ).extract( 0, 16 ).getSignedValue() == 32826 );

  ufAssert( dval( addshu1, 0 ).extract( 16, 16 ).getSignedValue() == 0xFFFF );
  ufAssert( dval( addshu1, 0 ).extract( 0, 16 ).getSignedValue() == 0xFFFF );

  ufAssert( dval( addsu1, 0 ).getSignedValue() == 0xFFFFFFFF );
  ufAssert( dval( addsu2, 0 ).getSignedValue() == 77 );

  ufAssert( dval( addsca1, 0 ).getSignedValue() == 0xF0000100 );
  ufAssert( uval( addsca1, 1 ).extract( 28, 4 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( addsca1, 1 ).extract( 0, 28 ).isBinaryInteger() );
  ufAssert( uval( addsca1, 2 ).extract( 26, 6 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( addsca1, 2 ).extract( 0, 26 ).isBinaryInteger() );

  ufAssert( dval( addscat1, 0 ).getSignedValue() == 0xF000FFEC );
  ufAssert(
    uval( addscat1, 1 ).extract( 29, 3 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( addscat1, 1 ).extract( 0, 29 ).isBinaryInteger() );
  ufAssert( uval( addscat1, 2 ).extract( 3, 29 ).isBinaryInteger() );
  ufAssert( uval( addscat1, 2 ).extract( 0, 3 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( addx1, 0 ).getSignedValue() == -59 );
  ufAssert( dval( addx2, 0 ).getSignedValue() == 2147483525 );
  ufAssert( dval( addx3, 0 ).getSignedValue() == -2147483585 );

  return( 0 );
}
