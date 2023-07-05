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
  // operations XOR & co.

  TC131 p;
  WIR_CompilationUnit c;
  c.setName( __FILE__ );
  WIR_Function &f = c.pushBackFunction( WIR_Function { "main" } );
  WIR_BasicBlock &b1 = f.pushBackBasicBlock( {} );
  const TC_DRegP &D4 = p.D4();
  auto &dExt = f.pushBackVirtualRegister( TC_DRegV() );
  auto &dMi123 = f.pushBackVirtualRegister( TC_DRegV() );
  auto &dPl64 = f.pushBackVirtualRegister( TC_DRegV() );
  auto &dFF0000EE = f.pushBackVirtualRegister( TC_DRegV() );
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
    { TC131::OpCode::MOVH, TC131::OperationFormat::DC16_2,
      new WIR_RegisterParameter( dFF0000EE, WIR_Usage::def ),
      new TC_Const16_Unsigned( 0xFF00 ) } );

  tcop(
    { TC131::OpCode::ADDI, TC131::OperationFormat::DDC16_1,
      new WIR_RegisterParameter( dFF0000EE, WIR_Usage::def ),
      new WIR_RegisterParameter( dFF0000EE, WIR_Usage::use ),
      new TC_Const16_Signed( 0x00EE ) } );

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
      new WIR_RegisterParameter( bitMask, WIR_Usage::def ),
      new TC_Const16_Unsigned( 0x0F00 ) } );

  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( notOne, WIR_Usage::def ),
      new TC_Const16_Signed( 1 ) } );

  tcop(
    { TC131::OpCode::NOT, TC131::OperationFormat::SD,
      new WIR_RegisterParameter( notOne, WIR_Usage::defuse ) } );

  // XNOR
  auto &xnor1 = tcop(
    { TC131::OpCode::XNOR, TC131::OperationFormat::DDC9_2,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dFF0000EE, WIR_Usage::use ),
      new TC_Const9_Unsigned( 122 ) } );

  tcop(
    { TC131::OpCode::AND, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( bitMask, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &xnor2 = tcop(
    { TC131::OpCode::XNOR, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // XNOR.T
  auto &xnort1 = tcop(
    { TC131::OpCode::XNOR_T, TC131::OperationFormat::DDC5DC5_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const5_Unsigned( 6 ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 5 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &xnort2 = tcop(
    { TC131::OpCode::XNOR_T, TC131::OperationFormat::DDC5DC5_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const5_Unsigned( 7 ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 5 ) } );

  tcop(
    { TC131::OpCode::AND, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( notOne, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // XOR
  auto &xor1 = tcop(
    { TC131::OpCode::XOR, TC131::OperationFormat::DDC9_2,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dFF0000EE, WIR_Usage::use ),
      new TC_Const9_Unsigned( 122 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &xor2 = tcop(
    { TC131::OpCode::XOR, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // XOR.EQ
  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new TC_Const16_Signed( 0 ) } );

  auto &xoreq1 = tcop(
    { TC131::OpCode::XOR_EQ, TC131::OperationFormat::DDC9_3,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const9_Signed( 64 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &xoreq2 = tcop(
    { TC131::OpCode::XOR_EQ, TC131::OperationFormat::DDC9_3,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const9_Signed( 65 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ADDI, TC131::OperationFormat::DDC16_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new TC_Const16_Signed( 1 ) } );

  auto &xoreq3 = tcop(
    { TC131::OpCode::XOR_EQ, TC131::OperationFormat::DDC9_3,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new TC_Const9_Signed( 68 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &xoreq4 = tcop(
    { TC131::OpCode::XOR_EQ, TC131::OperationFormat::DDD_2,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // XOR.GE
  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new TC_Const16_Signed( 0 ) } );

  auto &xorge1 = tcop(
    { TC131::OpCode::XOR_GE, TC131::OperationFormat::DDC9_3,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const9_Signed( -64 ) } );

  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( bitMask, WIR_Usage::def ),
      new TC_Const16_Signed( 0x7FFE ) } );

  tcop(
    { TC131::OpCode::AND, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( bitMask, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &xorge2 = tcop(
    { TC131::OpCode::XOR_GE, TC131::OperationFormat::DDC9_3,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const9_Signed( 64 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &xorge3 = tcop(
    { TC131::OpCode::XOR_GE, TC131::OperationFormat::DDC9_3,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new TC_Const9_Signed( -64 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &xorge4 = tcop(
    { TC131::OpCode::XOR_GE, TC131::OperationFormat::DDD_2,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // XOR.GE.U
  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new TC_Const16_Signed( 0 ) } );

  auto &xorgeu1 = tcop(
    { TC131::OpCode::XOR_GE_U, TC131::OperationFormat::DDC9_4,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const9_Unsigned( 65 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &xorgeu2 = tcop(
    { TC131::OpCode::XOR_GE_U, TC131::OperationFormat::DDC9_4,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const9_Unsigned( 64 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // XOR.LT
  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new TC_Const16_Signed( 0 ) } );

  auto &xorlt1 = tcop(
    { TC131::OpCode::XOR_LT, TC131::OperationFormat::DDC9_3,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const9_Signed( -64 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &xorlt2 = tcop(
    { TC131::OpCode::XOR_LT, TC131::OperationFormat::DDC9_3,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const9_Signed( 64 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &xorlt3 = tcop(
    { TC131::OpCode::XOR_LT, TC131::OperationFormat::DDC9_3,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new TC_Const9_Signed( -64 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &xorlt4 = tcop(
    { TC131::OpCode::XOR_LT, TC131::OperationFormat::DDD_2,
      new WIR_RegisterParameter( dPl64, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ) } );

  // XOR.LT.U
  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new TC_Const16_Signed( 0 ) } );

  auto &xorltu1 = tcop(
    { TC131::OpCode::XOR_LT_U, TC131::OperationFormat::DDC9_4,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const9_Unsigned( 65 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &xorltu2 = tcop(
    { TC131::OpCode::XOR_LT_U, TC131::OperationFormat::DDC9_4,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const9_Unsigned( 64 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // XOR.NE
  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new TC_Const16_Signed( 0 ) } );

  auto &xorne1 = tcop(
    { TC131::OpCode::XOR_NE, TC131::OperationFormat::DDC9_3,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const9_Signed( 64 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &xorne2 = tcop(
    { TC131::OpCode::XOR_NE, TC131::OperationFormat::DDC9_3,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const9_Signed( 65 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ADDI, TC131::OperationFormat::DDC16_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new TC_Const16_Signed( 1 ) } );

  auto &xorne3 = tcop(
    { TC131::OpCode::XOR_NE, TC131::OperationFormat::DDC9_3,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new TC_Const9_Signed( 68 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &xorne4 = tcop(
    { TC131::OpCode::XOR_NE, TC131::OperationFormat::DDD_2,
      new WIR_RegisterParameter( dPl64, WIR_Usage::defuse ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // XOR.T
  auto &xort1 = tcop(
    { TC131::OpCode::XOR_T, TC131::OperationFormat::DDC5DC5_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const5_Unsigned( 6 ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 5 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &xort2 = tcop(
    { TC131::OpCode::XOR_T, TC131::OperationFormat::DDC5DC5_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const5_Unsigned( 7 ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 5 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // Do bit-true data flow analysis.
  TC_BitDFA a { f };
  a.analyze();

  // Evaluate analysis results.
  ufAssert( dval( xnor1, 0 ).getSignedValue() == 0x00FFFF6B );
  ufAssert( uval( xnor1, 1 ).extract( 0, 24 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( xnor1, 1 ).extract( 24, 4 ).isBinaryInteger() );
  ufAssert( uval( xnor1, 1 ).extract( 28, 4 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( xnor1, 2 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( xnor2, 0 ).containsOnlyBit( WIR_L4::b1 ) );

  ufAssert( dval( xnort1, 0 ).at( 0 ) == WIR_L4::b1 );
  ufAssert( uval( xnort1, 1 ).extract( 0, 6 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( xnort1, 1 ).at( 6 ) == WIR_L4::b0 );
  ufAssert( uval( xnort1, 1 ).extract( 7, 25 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( xnort1, 3 ).extract( 0, 5 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( xnort1, 3 ).at( 5 ) == WIR_L4::b0 );
  ufAssert( uval( xnort1, 3 ).extract( 6, 26 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( xnort2, 0 ).at( 0 ) == WIR_L4::b0 );
  ufAssert( uval( xnort2, 1 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( xnort2, 3 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( xor1, 0 ).getSignedValue() == 0xFF000094 );

  ufAssert( dval( xor2, 0 ).getSignedValue() == 0 );

  ufAssert( dval( xoreq1, 0 ).at( 0 ) == WIR_L4::b1 );
  ufAssert( uval( xoreq1, 1 ).isBinaryInteger() );
  ufAssert( uval( xoreq1, 2 ).isBinaryInteger() );

  ufAssert( dval( xoreq2, 0 ).at( 0 ) == WIR_L4::b1 );
  ufAssert( uval( xoreq2, 1 ).at( 0 ) == WIR_L4::b0 );
  ufAssert( uval( xoreq2, 1 ).extract( 1, 31 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( xoreq2, 2 ).at( 0 ) == WIR_L4::b1 );
  ufAssert( uval( xoreq2, 2 ).extract( 1, 8 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( xoreq3, 0 ).at( 0 ) == WIR_L4::bL );

  ufAssert( dval( xoreq4, 0 ).at( 0 ) == WIR_L4::bN );

  ufAssert( dval( xorge1, 0 ).at( 0 ) == WIR_L4::b0 );
  ufAssert( uval( xorge1, 0 ).at( 0 ) == WIR_L4::bX );
  ufAssert( uval( xorge1, 0 ).extract( 1, 14 ).isBinaryInteger() );
  ufAssert( uval( xorge1, 0 ).extract( 15, 17 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( xorge1, 1 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( xorge1, 2 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( xorge2, 0 ).at( 0 ) == WIR_L4::b1 );
  ufAssert( dval( xorge3, 0 ).at( 0 ) == WIR_L4::bL );
  ufAssert( dval( xorge4, 0 ).at( 0 ) == WIR_L4::bN );

  ufAssert( dval( xorgeu1, 0 ).at( 0 ) == WIR_L4::b0 );
  ufAssert( dval( xorgeu2, 0 ).at( 0 ) == WIR_L4::b1 );

  ufAssert( dval( xorlt1, 0 ).at( 0 ) == WIR_L4::b1 );
  ufAssert( dval( xorlt2, 0 ).at( 0 ) == WIR_L4::b1 );
  ufAssert( dval( xorlt3, 0 ).at( 0 ) == WIR_L4::bL );
  ufAssert( dval( xorlt4, 0 ).getSignedValue() == 64 );

  ufAssert( dval( xorltu1, 0 ).at( 0 ) == WIR_L4::b1 );
  ufAssert( dval( xorltu2, 0 ).at( 0 ) == WIR_L4::b1 );

  ufAssert( dval( xorne1, 0 ).at( 0 ) == WIR_L4::b0 );
  ufAssert( uval( xorne1, 1 ).isBinaryInteger() );
  ufAssert( uval( xorne1, 2 ).isBinaryInteger() );

  ufAssert( dval( xorne2, 0 ).at( 0 ) == WIR_L4::b1 );
  ufAssert( uval( xorne2, 1 ).at( 0 ) == WIR_L4::b0 );
  ufAssert( uval( xorne2, 1 ).extract( 1, 31 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( xorne2, 2 ).at( 0 ) == WIR_L4::b1 );
  ufAssert( uval( xorne2, 2 ).extract( 1, 8 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( xorne3, 0 ).at( 0 ) == WIR_L4::bL );
  ufAssert( dval( xorne4, 0 ).getSignedValue() == 64 );

  ufAssert( dval( xort1, 0 ).at( 0 ) == WIR_L4::b0 );
  ufAssert( dval( xort2, 0 ).at( 0 ) == WIR_L4::b1 );

  return( 0 );
}
