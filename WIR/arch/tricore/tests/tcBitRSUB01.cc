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
  // operations RSUB & co.

  TC131 p;
  WIR_CompilationUnit c;
  c.setName( __FILE__ );
  WIR_Function &f = c.pushBackFunction( WIR_Function { "main" } );
  WIR_BasicBlock &b1 = f.pushBackBasicBlock( {} );
  const TC_DRegP &D4 = p.D4();
  auto &dMi123 = f.pushBackVirtualRegister( TC_DRegV() );
  auto &dPl64 = f.pushBackVirtualRegister( TC_DRegV() );
  auto &tmp0 = f.pushBackVirtualRegister( TC_DRegV() );
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
    { TC131::OpCode::MOVH, TC131::OperationFormat::DC16_2,
      new WIR_RegisterParameter( bitMask, WIR_Usage::def ),
      new TC_Const16_Unsigned( 0x0F00 ) } );

  // RSUB
  auto &rsub1 = tcop(
    { TC131::OpCode::RSUB, TC131::OperationFormat::DDC9_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const9_Signed( -99 ) } );

  tcop(
    { TC131::OpCode::AND, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( bitMask, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &rsub2 = tcop(
    { TC131::OpCode::RSUB, TC131::OperationFormat::DDC9_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const9_Signed( -99 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &rsub3 = tcop(
    { TC131::OpCode::RSUB, TC131::OperationFormat::SD,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ) } );

  tcop(
    { TC131::OpCode::AND, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( bitMask, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // RSUBS
  auto &rsubs1 = tcop(
    { TC131::OpCode::RSUBS, TC131::OperationFormat::DDC9_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const9_Signed( 64 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &rsubs2 = tcop(
    { TC131::OpCode::RSUBS, TC131::OperationFormat::DDC9_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( maxPos, WIR_Usage::use ),
      new TC_Const9_Signed( -128 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &rsubs3 = tcop(
    { TC131::OpCode::RSUBS, TC131::OperationFormat::DDC9_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( maxNeg, WIR_Usage::use ),
      new TC_Const9_Signed( 128 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // RSUBS.U
  auto &rsubsu1 = tcop(
    { TC131::OpCode::RSUBS_U, TC131::OperationFormat::DDC9_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const9_Signed( -13 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &rsubsu2 = tcop(
    { TC131::OpCode::RSUBS_U, TC131::OperationFormat::DDC9_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const9_Signed( 13 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // Do bit-true data flow analysis.
  TC_BitDFA a { f };
  a.analyze();

  // Evaluate analysis results.
  ufAssert( dval( rsub1, 0 ).getSignedValue() == -163 );
  ufAssert( uval( rsub1, 1 ).extract( 0, 28 ).isBinaryInteger() );
  ufAssert( uval( rsub1, 1 ).extract( 28, 4 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( rsub2, 0 ).getSignedValue() == 24 );

  ufAssert( dval( rsub3, 0 ).getSignedValue() == -24 );
  ufAssert( uval( rsub3, 0 ).extract( 0, 28 ).isBinaryInteger() );
  ufAssert( uval( rsub3, 0 ).extract( 28, 4 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( rsubs1, 0 ).getSignedValue() == 187 );
  ufAssert( dval( rsubs2, 0 ).getSignedValue() == -2147483648 );
  ufAssert( dval( rsubs3, 0 ).getSignedValue() == 2147483647 );

  ufAssert( dval( rsubsu1, 0 ).getSignedValue() == 0xFFFFFFB3 );
  ufAssert( dval( rsubsu2, 0 ).getSignedValue() == 0 );

  return( 0 );
}
