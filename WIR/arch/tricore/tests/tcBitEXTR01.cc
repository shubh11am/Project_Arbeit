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
  // operations EXTR & co.

  TC131 p;
  WIR_CompilationUnit c;
  c.setName( __FILE__ );
  WIR_Function &f = c.pushBackFunction( WIR_Function { "main" } );
  WIR_BasicBlock &b1 = f.pushBackBasicBlock( {} );
  const TC_DRegP &D4 = p.D4();
  auto &dExt = f.pushBackVirtualRegister( TC_DRegV() );
  auto &dMi123 = f.pushBackVirtualRegister( TC_DRegV() );
  auto &dPl64 = f.pushBackVirtualRegister( TC_DRegV() );
  auto &tmp0 = f.pushBackVirtualRegister( TC_DRegV() );
  auto &tmp1 = f.pushBackVirtualRegister( TC_ERegV() );
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

  // This lambda serves to retrieve an operation's incoming up value.
  auto uval1 = []( const WIR_Operation &o,
                   unsigned int pos ) -> const WIR_UpDownValue & {
    auto it = o.begin();
    std::advance( it, pos );
    auto &c = it->get().getContainers<WIR_BitValues>().begin()->get();
    return( c.getInValues().rbegin()->upVal );
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
      new WIR_RegisterParameter( bitMask, WIR_Usage::def ),
      new TC_Const16_Signed( 5 ) } );

  // EXTR
  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( tmp1.rbegin()->get(), WIR_Usage::def ),
      new TC_Const16_Signed( 3 ) } );     // width

  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( tmp1.begin()->get(), WIR_Usage::def ),
      new TC_Const16_Signed( 5 ) } );     // pos

  auto &extr1 = tcop(
    { TC131::OpCode::EXTR, TC131::OperationFormat::DDE,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::AND, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( bitMask, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &extr2 = tcop(
    { TC131::OpCode::EXTR, TC131::OperationFormat::DDE,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &extr3 = tcop(
    { TC131::OpCode::EXTR, TC131::OperationFormat::DDE,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDC5C5,
      new WIR_RegisterParameter( tmp1.begin()->get(), WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1.begin()->get(), WIR_Usage::use ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new TC_Const5_Unsigned( 3 ),
      new TC_Const5_Unsigned( 1 ) } );

  auto &extr4 = tcop(
    { TC131::OpCode::EXTR, TC131::OperationFormat::DDE,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // EXTR.U
  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( tmp1.rbegin()->get(), WIR_Usage::def ),
      new TC_Const16_Signed( 3 ) } );     // width

  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( tmp1.begin()->get(), WIR_Usage::def ),
      new TC_Const16_Signed( 5 ) } );     // pos

  auto &extru1 = tcop(
    { TC131::OpCode::EXTR_U, TC131::OperationFormat::DDE,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &extru2 = tcop(
    { TC131::OpCode::EXTR_U, TC131::OperationFormat::DDE,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &extru3 = tcop(
    { TC131::OpCode::EXTR_U, TC131::OperationFormat::DDE,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDC5C5,
      new WIR_RegisterParameter( tmp1.rbegin()->get(), WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1.rbegin()->get(), WIR_Usage::use ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new TC_Const5_Unsigned( 3 ),
      new TC_Const5_Unsigned( 1 ) } );

  auto &extru4 = tcop(
    { TC131::OpCode::EXTR_U, TC131::OperationFormat::DDE,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // Do bit-true data flow analysis.
  TC_BitDFA a { f };
  a.analyze();

  // Evaluate analysis results.
  ufAssert( dval( extr1, 0 ).getSignedValue() == 2 );
  ufAssert( uval( extr1, 2 ).extract( 0, 5 ).isBinaryInteger() );
  ufAssert( uval( extr1, 2 ).extract( 0, 5 ).getSignedValue() == 3 );
  ufAssert( uval( extr1, 2 ).extract( 5, 27 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval1( extr1, 2 ).extract( 0, 5 ).isBinaryInteger() );
  ufAssert( uval1( extr1, 2 ).extract( 0, 5 ).getSignedValue() == 5 );
  ufAssert( uval1( extr1, 2 ).extract( 5, 27 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( extr1, 1 ).extract( 0, 5 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( extr1, 1 ).at( 5 ) == WIR_L4::b0 );
  ufAssert( uval( extr1, 1 ).at( 6 ) == WIR_L4::bX );
  ufAssert( uval( extr1, 1 ).at( 7 ) == WIR_L4::b0 );
  ufAssert( uval( extr1, 1 ).extract( 8, 24 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( extr2, 0 ).getSignedValue() == -4 );
  ufAssert( dval( extr3, 0 ).containsOnlyBit( WIR_L4::bL ) );

  ufAssert( dval( extr4, 0 ).containsOnlyBit( WIR_L4::bL ) );
  ufAssert( uval( extr4, 2 ).extract( 0, 5 ).isBinaryInteger() );
  ufAssert( uval( extr4, 2 ).extract( 0, 5 ).getSignedValue() == 3 );
  ufAssert( uval( extr4, 2 ).extract( 5, 27 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert(
    uval1( extr4, 2 ).extract( 0, 5 ).containsOnlyBits(
      { WIR_L4::bL, WIR_L4::b0, WIR_L4::b1 } ) );
  ufAssert( uval1( extr4, 2 ).extract( 5, 27 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( extru1, 0 ).getSignedValue() == 2 );
  ufAssert( dval( extru2, 0 ).getSignedValue() == 4 );
  ufAssert( dval( extru3, 0 ).extract( 3, 29 ).containsOnlyBit( WIR_L4::b0 ) );
  ufAssert( dval( extru3, 0 ).extract( 0, 3 ).containsOnlyBit( WIR_L4::bL ) );
  ufAssert( dval( extru4, 0 ).containsOnlyBit( WIR_L4::bL ) );

  return( 0 );
}
