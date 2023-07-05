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
  // operations CLO & co.

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

  // CLO
  auto &clo1 = tcop(
    { TC131::OpCode::CLO, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &clo2 = tcop(
    { TC131::OpCode::CLO, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDC5C5,
      new WIR_RegisterParameter( dMi123, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new TC_Const5_Unsigned( 24 ),
      new TC_Const5_Unsigned( 1 ) } );

  auto &clo3 = tcop(
    { TC131::OpCode::CLO, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // CLO.H
  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( dMi123, WIR_Usage::def ),
      new TC_Const16_Signed( -123 ) } );

  auto &cloh1 = tcop(
    { TC131::OpCode::CLO_H, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDC5C5,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 0 ),
      new TC_Const5_Unsigned( 16 ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDC5C5,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const5_Unsigned( 29 ),
      new TC_Const5_Unsigned( 3 ) } );

  auto &cloh2 = tcop(
    { TC131::OpCode::CLO_H, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // CLS
  auto &cls1 = tcop(
    { TC131::OpCode::CLS, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &cls2 = tcop(
    { TC131::OpCode::CLS, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDC5C5,
      new WIR_RegisterParameter( dMi123, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new TC_Const5_Unsigned( 24 ),
      new TC_Const5_Unsigned( 1 ) } );

  auto &cls3 = tcop(
    { TC131::OpCode::CLS, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // CLS.H
  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( dMi123, WIR_Usage::def ),
      new TC_Const16_Signed( -123 ) } );

  auto &clsh1 = tcop(
    { TC131::OpCode::CLS_H, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDC5C5,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 0 ),
      new TC_Const5_Unsigned( 16 ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDC5C5,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const5_Unsigned( 29 ),
      new TC_Const5_Unsigned( 3 ) } );

  auto &clsh2 = tcop(
    { TC131::OpCode::CLS_H, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // CLZ
  auto &clz1 = tcop(
    { TC131::OpCode::CLZ, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &clz2 = tcop(
    { TC131::OpCode::CLZ, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDC5C5,
      new WIR_RegisterParameter( dPl64, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new TC_Const5_Unsigned( 24 ),
      new TC_Const5_Unsigned( 1 ) } );

  auto &clz3 = tcop(
    { TC131::OpCode::CLZ, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // CLZ.H
  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( dPl64, WIR_Usage::def ),
      new TC_Const16_Signed( 64 ) } );

  auto &clzh1 = tcop(
    { TC131::OpCode::CLZ_H, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDC5C5,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 0 ),
      new TC_Const5_Unsigned( 16 ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDC5C5,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const5_Unsigned( 30 ),
      new TC_Const5_Unsigned( 2 ) } );

  auto &clzh2 = tcop(
    { TC131::OpCode::CLZ_H, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // Do bit-true data flow analysis.
  TC_BitDFA a { f };
  a.analyze();

  // Evaluate analysis results.
  ufAssert( dval( clo1, 0 ).getSignedValue() == 0 );
  ufAssert( uval( clo1, 1 ).at( 31 ) == WIR_L4::b0 );
  ufAssert( uval( clo1, 1 ).extract( 0, 31 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( clo2, 0 ).getSignedValue() == 25 );
  ufAssert( uval( clo2, 1 ).extract( 7, 25 ).containsOnlyBit( WIR_L4::b1 ) );
  ufAssert( uval( clo2, 1 ).at( 6 ) == WIR_L4::b0 );
  ufAssert( uval( clo2, 1 ).extract( 0, 6 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( clo3, 0 ).extract( 6, 26 ).containsOnlyBit( WIR_L4::b0 ) );
  ufAssert( dval( clo3, 0 ).extract( 0, 6 ).containsOnlyBit( WIR_L4::bL ) );
  ufAssert(
    uval( clo3, 1 ).containsOnlyBits(
      { WIR_L4::bU, WIR_L4::bL, WIR_L4::bN, WIR_L4::b0, WIR_L4::b1 } ) );

  ufAssert( dval( cloh1, 0 ).extract( 16, 16 ).getSignedValue() == 16 );
  ufAssert( uval( cloh1, 1 ).extract( 16, 16 ).containsOnlyBit( WIR_L4::b1 ) );
  ufAssert( dval( cloh1, 0 ).extract( 0, 16 ).getSignedValue() == 9 );
  ufAssert( uval( cloh1, 1 ).extract( 7, 9 ).containsOnlyBit( WIR_L4::b1 ) );
  ufAssert( uval( cloh1, 1 ).at( 6 ) == WIR_L4::b0 );
  ufAssert( uval( cloh1, 1 ).extract( 0, 6 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( cloh2, 0 ).extract( 16, 16 ).getSignedValue() == 1 );
  ufAssert( uval( cloh2, 1 ).at( 31 ) == WIR_L4::b1 );
  ufAssert( uval( cloh2, 1 ).at( 30 ) == WIR_L4::b0 );
  ufAssert( uval( cloh2, 1 ).extract( 16, 14 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( cloh2, 0 ).extract( 0, 16 ).getSignedValue() == 0 );
  ufAssert( uval( cloh2, 1 ).at( 15 ) == WIR_L4::b0 );
  ufAssert( uval( cloh2, 1 ).extract( 0, 15 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( cls1, 0 ).getSignedValue() == 24 );
  ufAssert( uval( cls1, 1 ).extract( 7, 25 ).containsOnlyBit( WIR_L4::b0 ) );
  ufAssert( uval( cls1, 1 ).at( 6 ) == WIR_L4::b1 );
  ufAssert( uval( cls1, 1 ).extract( 0, 6 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( cls2, 0 ).getSignedValue() == 24 );
  ufAssert( uval( cls2, 1 ).extract( 7, 25 ).containsOnlyBit( WIR_L4::b1 ) );
  ufAssert( uval( cls2, 1 ).at( 6 ) == WIR_L4::b0 );
  ufAssert( uval( cls2, 1 ).extract( 0, 6 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( cls3, 0 ).extract( 6, 26 ).containsOnlyBit( WIR_L4::b0 ) );
  ufAssert( dval( cls3, 0 ).extract( 0, 6 ).containsOnlyBit( WIR_L4::bL ) );
  ufAssert(
    uval( cls3, 1 ).containsOnlyBits(
      { WIR_L4::bU, WIR_L4::bL, WIR_L4::bN, WIR_L4::b0, WIR_L4::b1 } ) );

  ufAssert( dval( clsh1, 0 ).extract( 16, 16 ).getSignedValue() == 15 );
  ufAssert( uval( clsh1, 1 ).extract( 16, 16 ).containsOnlyBit( WIR_L4::b1 ) );
  ufAssert( dval( clsh1, 0 ).extract( 0, 16 ).getSignedValue() == 8 );
  ufAssert( uval( clsh1, 1 ).extract( 7, 9 ).containsOnlyBit( WIR_L4::b1 ) );
  ufAssert( uval( clsh1, 1 ).at( 6 ) == WIR_L4::b0 );
  ufAssert( uval( clsh1, 1 ).extract( 0, 6 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( clsh2, 0 ).extract( 16, 16 ).getSignedValue() == 0 );
  ufAssert( uval( clsh2, 1 ).at( 31 ) == WIR_L4::b1 );
  ufAssert( uval( clsh2, 1 ).at( 30 ) == WIR_L4::b0 );
  ufAssert( uval( clsh2, 1 ).extract( 16, 14 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( dval( clsh2, 0 ).extract( 0, 16 ).getSignedValue() == 8 );
  ufAssert( uval( clsh2, 1 ).extract( 7, 9 ).containsOnlyBit( WIR_L4::b0 ) );
  ufAssert( uval( clsh2, 1 ).at( 6 ) == WIR_L4::b1 );
  ufAssert( uval( clsh2, 1 ).extract( 0, 6 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( clz1, 0 ).getSignedValue() == 25 );
  ufAssert( uval( clz1, 1 ).extract( 7, 25 ).containsOnlyBit( WIR_L4::b0 ) );
  ufAssert( uval( clz1, 1 ).at( 6 ) == WIR_L4::b1 );
  ufAssert( uval( clz1, 1 ).extract( 0, 6 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( clz2, 0 ).getSignedValue() == 0 );
  ufAssert( uval( clz2, 1 ).at( 31 ) == WIR_L4::b1 );
  ufAssert( uval( clz2, 1 ).extract( 0, 31 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( clz3, 0 ).extract( 6, 26 ).containsOnlyBit( WIR_L4::b0 ) );
  ufAssert( dval( clz3, 0 ).extract( 0, 6 ).containsOnlyBit( WIR_L4::bL ) );
  ufAssert(
    uval( clz3, 1 ).containsOnlyBits(
      { WIR_L4::bU, WIR_L4::bL, WIR_L4::bN, WIR_L4::b0, WIR_L4::b1 } ) );

  ufAssert( dval( clzh1, 0 ).extract( 16, 16 ).getSignedValue() == 16 );
  ufAssert( uval( clzh1, 1 ).extract( 16, 16 ).containsOnlyBit( WIR_L4::b0 ) );
  ufAssert( dval( clzh1, 0 ).extract( 0, 16 ).getSignedValue() == 9 );
  ufAssert( uval( clzh1, 1 ).extract( 7, 9 ).containsOnlyBit( WIR_L4::b0 ) );
  ufAssert( uval( clzh1, 1 ).at( 6 ) == WIR_L4::b1 );
  ufAssert( uval( clzh1, 1 ).extract( 0, 6 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( clzh2, 0 ).extract( 16, 16 ).getSignedValue() == 1 );
  ufAssert( uval( clzh2, 1 ).at( 31 ) == WIR_L4::b0 );
  ufAssert( uval( clzh2, 1 ).at( 30 ) == WIR_L4::b1 );
  ufAssert( uval( clzh2, 1 ).extract( 16, 14 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( dval( clzh2, 0 ).extract( 0, 16 ).getSignedValue() == 9 );
  ufAssert( uval( clzh2, 1 ).extract( 7, 9 ).containsOnlyBit( WIR_L4::b0 ) );
  ufAssert( uval( clzh2, 1 ).at( 6 ) == WIR_L4::b1 );
  ufAssert( uval( clzh2, 1 ).extract( 0, 6 ).containsOnlyBit( WIR_L4::bX ) );

  return( 0 );
}
