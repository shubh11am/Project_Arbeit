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
  // operations CMOV & co.

  TC131 p;
  WIR_CompilationUnit c;
  c.setName( __FILE__ );
  WIR_Function &f = c.pushBackFunction( WIR_Function { "main" } );
  WIR_BasicBlock &b1 = f.pushBackBasicBlock( {} );
  const TC_DRegP &D4 = p.D4();
  const TC_DRegP &D15 = p.D15();
  auto &dExt = f.pushBackVirtualRegister( TC_DRegV() );
  auto &tmp0 = f.pushBackVirtualRegister( TC_DRegV() );
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
    { TC131::OpCode::MOVH, TC131::OperationFormat::DC16_2,
      new WIR_RegisterParameter( bitMask, WIR_Usage::def ),
      new TC_Const16_Unsigned( 0x0F00 ) } );

  tcop(
    { TC131::OpCode::ADDI, TC131::OperationFormat::DDC16_1,
      new WIR_RegisterParameter( bitMask, WIR_Usage::def ),
      new WIR_RegisterParameter( bitMask, WIR_Usage::use ),
      new TC_Const16_Signed( 0x7FF5 ) } );

  // CMOV
  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( D15, WIR_Usage::def ),
      new TC_Const16_Signed( -123 ) } );

  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new TC_Const16_Signed( 64 ) } );

  auto &cmov1 = tcop(
    { TC131::OpCode::CMOV, TC131::OperationFormat::SDIC4_3,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( D15, WIR_Usage::use ),
      new TC_Const4_Signed( -5 ) } );

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
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( D15, WIR_Usage::def ),
      new TC_Const16_Signed( 0 ) } );

  auto &cmov2 = tcop(
    { TC131::OpCode::CMOV, TC131::OperationFormat::SDIC4_3,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( D15, WIR_Usage::use ),
      new TC_Const4_Signed( 5 ) } );

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
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D15, WIR_Usage::def ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ) } );

  auto &cmov3 = tcop(
    { TC131::OpCode::CMOV, TC131::OperationFormat::SDIC4_3,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( D15, WIR_Usage::use ),
      new TC_Const4_Signed( 5 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // CMOVN
  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( D15, WIR_Usage::def ),
      new TC_Const16_Signed( -123 ) } );

  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new TC_Const16_Signed( 64 ) } );

  auto &cmovn1 = tcop(
    { TC131::OpCode::CMOVN, TC131::OperationFormat::SDIC4_3,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( D15, WIR_Usage::use ),
      new TC_Const4_Signed( -5 ) } );

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
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( D15, WIR_Usage::def ),
      new TC_Const16_Signed( 0 ) } );

  auto &cmovn2 = tcop(
    { TC131::OpCode::CMOVN, TC131::OperationFormat::SDIC4_3,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( D15, WIR_Usage::use ),
      new TC_Const4_Signed( 5 ) } );

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
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D15, WIR_Usage::def ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ) } );

  auto &cmovn3 = tcop(
    { TC131::OpCode::CMOVN, TC131::OperationFormat::SDIC4_3,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ),
      new WIR_RegisterParameter( D15, WIR_Usage::use ),
      new TC_Const4_Signed( 5 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // Do bit-true data flow analysis.
  TC_BitDFA a { f };
  a.analyze();

  // Evaluate analysis results.
  ufAssert( dval( cmov1, 0 ).getSignedValue() == -5 );
  ufAssert( uval( cmov1, 0 ).isBinaryInteger() );
  ufAssert( uval( cmov1, 2 ).at( 3 ) == WIR_L4::bX );
  ufAssert( uval( cmov1, 2 ).at( 2 ) == WIR_L4::b0 );
  ufAssert( uval( cmov1, 2 ).at( 1 ) == WIR_L4::bX );
  ufAssert( uval( cmov1, 2 ).at( 0 ) == WIR_L4::b1 );

  ufAssert( dval( cmov2, 0 ).getSignedValue() == 0xF007FF1 );
  ufAssert( uval( cmov2, 0 ).extract( 28, 4 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( cmov2, 0 ).extract( 24, 4 ).isBinaryInteger() );
  ufAssert( uval( cmov2, 0 ).extract( 15, 9 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( cmov2, 0 ).extract( 4, 11 ).isBinaryInteger() );
  ufAssert( uval( cmov2, 0 ).at( 3 ) == WIR_L4::bX );
  ufAssert( uval( cmov2, 0 ).at( 2 ) == WIR_L4::b0 );
  ufAssert( uval( cmov2, 0 ).at( 1 ) == WIR_L4::bX );
  ufAssert( uval( cmov2, 0 ).at( 0 ) == WIR_L4::b1 );
  ufAssert( uval( cmov2, 2 ).isBinaryInteger() );

  ufAssert( dval( cmov3, 0 ).containsOnlyBit( WIR_L4::bL ) );

  ufAssert( dval( cmovn1, 0 ).getSignedValue() == 64 );
  ufAssert( uval( cmovn1, 0 ).extract( 28, 4 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( cmovn1, 0 ).extract( 24, 4 ).isBinaryInteger() );
  ufAssert( uval( cmovn1, 0 ).extract( 15, 9 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( cmovn1, 0 ).extract( 4, 11 ).isBinaryInteger() );
  ufAssert( uval( cmovn1, 0 ).at( 3 ) == WIR_L4::bX );
  ufAssert( uval( cmovn1, 0 ).at( 2 ) == WIR_L4::b0 );
  ufAssert( uval( cmovn1, 0 ).at( 1 ) == WIR_L4::bX );
  ufAssert( uval( cmovn1, 0 ).at( 0 ) == WIR_L4::b0 );
  ufAssert( uval( cmovn1, 2 ).isBinaryInteger() );

  ufAssert( dval( cmovn2, 0 ).getSignedValue() == 5 );
  ufAssert( uval( cmovn2, 0 ).isBinaryInteger() );
  ufAssert( uval( cmovn2, 2 ).at( 3 ) == WIR_L4::bX );
  ufAssert( uval( cmovn2, 2 ).at( 2 ) == WIR_L4::b1 );
  ufAssert( uval( cmovn2, 2 ).at( 1 ) == WIR_L4::bX );
  ufAssert( uval( cmovn2, 2 ).at( 0 ) == WIR_L4::b1 );

  ufAssert( dval( cmovn3, 0 ).containsOnlyBit( WIR_L4::bL ) );

  return( 0 );
}
