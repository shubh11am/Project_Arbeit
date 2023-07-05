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
  // operations BMERGE & co.

  TC131 p;
  WIR_CompilationUnit c;
  c.setName( __FILE__ );
  WIR_Function &f = c.pushBackFunction( WIR_Function { "main" } );
  WIR_BasicBlock &b1 = f.pushBackBasicBlock( {} );
  const TC_DRegP &D4 = p.D4();
  auto &dExt1 = f.pushBackVirtualRegister( TC_DRegV() );
  auto &dExt2 = f.pushBackVirtualRegister( TC_DRegV() );
  auto &tmp0 = f.pushBackVirtualRegister( TC_DRegV() );
  auto &tmp1 = f.pushBackVirtualRegister( TC_ERegV() );
  auto &tmp2 = f.pushBackVirtualRegister( TC_DRegV() );
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
      new TC_Const16_Unsigned( 0xFF0F ) } );

  tcop(
    { TC131::OpCode::ADDI, TC131::OperationFormat::DDC16_1,
      new WIR_RegisterParameter( bitMask, WIR_Usage::def ),
      new WIR_RegisterParameter( bitMask, WIR_Usage::use ),
      new TC_Const16_Signed( 0x0FF0 ) } );

  // BMERGE
  auto &bmerge1 = tcop(
    { TC131::OpCode::BMERGE, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dExt1, WIR_Usage::use ),
      new WIR_RegisterParameter( dExt2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::AND, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp2, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( bitMask, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp2, WIR_Usage::use ) } );

  // BSPLIT
  tcop(
    { TC131::OpCode::BMERGE, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dExt1, WIR_Usage::use ),
      new WIR_RegisterParameter( dExt2, WIR_Usage::use ) } );

  auto &bsplit1 = tcop(
    { TC131::OpCode::BSPLIT, TC131::OperationFormat::ED,
      new WIR_RegisterParameter( tmp1, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( bitMask, WIR_Usage::def ),
      new TC_Const16_Signed( 25549 ) } );

  tcop(
    { TC131::OpCode::AND, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp2, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1.begin()->get(), WIR_Usage::use ),
      new WIR_RegisterParameter( bitMask, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::AND, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp2, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1.rbegin()->get(), WIR_Usage::use ),
      new WIR_RegisterParameter( bitMask, WIR_Usage::use ) } );

  // Do bit-true data flow analysis.
  TC_BitDFA a { f };
  a.analyze();

  // Evaluate analysis results.
  ufAssert( dval( bmerge1, 0 ).containsOnlyBit( WIR_L4::bL ) );
  ufAssert(
    dval( bmerge1, 0 ).getLocation( 31 ).getRegisterParameter().getRegister() ==
      dExt1 );
  ufAssert( dval( bmerge1, 0 ).getLocation( 31 ).getBitPosition() == 15 );
  ufAssert(
    dval( bmerge1, 0 ).getLocation( 30 ).getRegisterParameter().getRegister() ==
      dExt2 );
  ufAssert( dval( bmerge1, 0 ).getLocation( 30 ).getBitPosition() == 15 );
  ufAssert(
    dval( bmerge1, 0 ).getLocation( 29 ).getRegisterParameter().getRegister() ==
      dExt1 );
  ufAssert( dval( bmerge1, 0 ).getLocation( 29 ).getBitPosition() == 14 );
  ufAssert(
    dval( bmerge1, 0 ).getLocation( 28 ).getRegisterParameter().getRegister() ==
      dExt2 );
  ufAssert( dval( bmerge1, 0 ).getLocation( 28 ).getBitPosition() == 14 );
  ufAssert(
    dval( bmerge1, 0 ).getLocation( 27 ).getRegisterParameter().getRegister() ==
      dExt1 );
  ufAssert( dval( bmerge1, 0 ).getLocation( 27 ).getBitPosition() == 13 );
  ufAssert(
    dval( bmerge1, 0 ).getLocation( 26 ).getRegisterParameter().getRegister() ==
      dExt2 );
  ufAssert( dval( bmerge1, 0 ).getLocation( 26 ).getBitPosition() == 13 );
  ufAssert(
    dval( bmerge1, 0 ).getLocation( 25 ).getRegisterParameter().getRegister() ==
      dExt1 );
  ufAssert( dval( bmerge1, 0 ).getLocation( 25 ).getBitPosition() == 12 );
  ufAssert(
    dval( bmerge1, 0 ).getLocation( 24 ).getRegisterParameter().getRegister() ==
      dExt2 );
  ufAssert( dval( bmerge1, 0 ).getLocation( 24 ).getBitPosition() == 12 );
  ufAssert(
    dval( bmerge1, 0 ).getLocation( 23 ).getRegisterParameter().getRegister() ==
      dExt1 );
  ufAssert( dval( bmerge1, 0 ).getLocation( 23 ).getBitPosition() == 11 );
  ufAssert(
    dval( bmerge1, 0 ).getLocation( 22 ).getRegisterParameter().getRegister() ==
      dExt2 );
  ufAssert( dval( bmerge1, 0 ).getLocation( 22 ).getBitPosition() == 11 );
  ufAssert(
    dval( bmerge1, 0 ).getLocation( 21 ).getRegisterParameter().getRegister() ==
      dExt1 );
  ufAssert( dval( bmerge1, 0 ).getLocation( 21 ).getBitPosition() == 10 );
  ufAssert(
    dval( bmerge1, 0 ).getLocation( 20 ).getRegisterParameter().getRegister() ==
      dExt2 );
  ufAssert( dval( bmerge1, 0 ).getLocation( 20 ).getBitPosition() == 10 );
  ufAssert(
    dval( bmerge1, 0 ).getLocation( 19 ).getRegisterParameter().getRegister() ==
      dExt1 );
  ufAssert( dval( bmerge1, 0 ).getLocation( 19 ).getBitPosition() == 9 );
  ufAssert(
    dval( bmerge1, 0 ).getLocation( 18 ).getRegisterParameter().getRegister() ==
      dExt2 );
  ufAssert( dval( bmerge1, 0 ).getLocation( 18 ).getBitPosition() == 9 );
  ufAssert(
    dval( bmerge1, 0 ).getLocation( 17 ).getRegisterParameter().getRegister() ==
      dExt1 );
  ufAssert( dval( bmerge1, 0 ).getLocation( 17 ).getBitPosition() == 8 );
  ufAssert(
    dval( bmerge1, 0 ).getLocation( 16 ).getRegisterParameter().getRegister() ==
      dExt2 );
  ufAssert( dval( bmerge1, 0 ).getLocation( 16 ).getBitPosition() == 8 );
  ufAssert(
    dval( bmerge1, 0 ).getLocation( 15 ).getRegisterParameter().getRegister() ==
      dExt1 );
  ufAssert( dval( bmerge1, 0 ).getLocation( 15 ).getBitPosition() == 7 );
  ufAssert(
    dval( bmerge1, 0 ).getLocation( 14 ).getRegisterParameter().getRegister() ==
      dExt2 );
  ufAssert( dval( bmerge1, 0 ).getLocation( 14 ).getBitPosition() == 7 );
  ufAssert(
    dval( bmerge1, 0 ).getLocation( 13 ).getRegisterParameter().getRegister() ==
      dExt1 );
  ufAssert( dval( bmerge1, 0 ).getLocation( 13 ).getBitPosition() == 6 );
  ufAssert(
    dval( bmerge1, 0 ).getLocation( 12 ).getRegisterParameter().getRegister() ==
      dExt2 );
  ufAssert( dval( bmerge1, 0 ).getLocation( 12 ).getBitPosition() == 6 );
  ufAssert(
    dval( bmerge1, 0 ).getLocation( 11 ).getRegisterParameter().getRegister() ==
      dExt1 );
  ufAssert( dval( bmerge1, 0 ).getLocation( 11 ).getBitPosition() == 5 );
  ufAssert(
    dval( bmerge1, 0 ).getLocation( 10 ).getRegisterParameter().getRegister() ==
      dExt2 );
  ufAssert( dval( bmerge1, 0 ).getLocation( 10 ).getBitPosition() == 5 );
  ufAssert(
    dval( bmerge1, 0 ).getLocation( 9 ).getRegisterParameter().getRegister() ==
      dExt1 );
  ufAssert( dval( bmerge1, 0 ).getLocation( 9 ).getBitPosition() == 4 );
  ufAssert(
    dval( bmerge1, 0 ).getLocation( 8 ).getRegisterParameter().getRegister() ==
      dExt2 );
  ufAssert( dval( bmerge1, 0 ).getLocation( 8 ).getBitPosition() == 4 );
  ufAssert(
    dval( bmerge1, 0 ).getLocation( 7 ).getRegisterParameter().getRegister() ==
      dExt1 );
  ufAssert( dval( bmerge1, 0 ).getLocation( 7 ).getBitPosition() == 3 );
  ufAssert(
    dval( bmerge1, 0 ).getLocation( 6 ).getRegisterParameter().getRegister() ==
      dExt2 );
  ufAssert( dval( bmerge1, 0 ).getLocation( 6 ).getBitPosition() == 3 );
  ufAssert(
    dval( bmerge1, 0 ).getLocation( 5 ).getRegisterParameter().getRegister() ==
      dExt1 );
  ufAssert( dval( bmerge1, 0 ).getLocation( 5 ).getBitPosition() == 2 );
  ufAssert(
    dval( bmerge1, 0 ).getLocation( 4 ).getRegisterParameter().getRegister() ==
      dExt2 );
  ufAssert( dval( bmerge1, 0 ).getLocation( 4 ).getBitPosition() == 2 );
  ufAssert(
    dval( bmerge1, 0 ).getLocation( 3 ).getRegisterParameter().getRegister() ==
      dExt1 );
  ufAssert( dval( bmerge1, 0 ).getLocation( 3 ).getBitPosition() == 1 );
  ufAssert(
    dval( bmerge1, 0 ).getLocation( 2 ).getRegisterParameter().getRegister() ==
      dExt2 );
  ufAssert( dval( bmerge1, 0 ).getLocation( 2 ).getBitPosition() == 1 );
  ufAssert(
    dval( bmerge1, 0 ).getLocation( 1 ).getRegisterParameter().getRegister() ==
      dExt1 );
  ufAssert( dval( bmerge1, 0 ).getLocation( 1 ).getBitPosition() == 0 );
  ufAssert(
    dval( bmerge1, 0 ).getLocation( 0 ).getRegisterParameter().getRegister() ==
      dExt2 );
  ufAssert( dval( bmerge1, 0 ).getLocation( 0 ).getBitPosition() == 0 );

  ufAssert(
    uval( bmerge1, 1 ).extract( 16, 16 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( bmerge1, 1 ).extract( 12, 4 ).containsOnlyBit( WIR_L4::bL ) );
  ufAssert( uval( bmerge1, 1 ).extract( 10, 2 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( bmerge1, 1 ).extract( 8, 2 ).containsOnlyBit( WIR_L4::bL ) );
  ufAssert( uval( bmerge1, 1 ).extract( 6, 2 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( bmerge1, 1 ).extract( 2, 4 ).containsOnlyBit( WIR_L4::bL ) );
  ufAssert( uval( bmerge1, 1 ).extract( 0, 2 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert(
    uval( bmerge1, 2 ).extract( 16, 16 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( bmerge1, 2 ).extract( 12, 4 ).containsOnlyBit( WIR_L4::bL ) );
  ufAssert( uval( bmerge1, 2 ).extract( 10, 2 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( bmerge1, 2 ).extract( 8, 2 ).containsOnlyBit( WIR_L4::bL ) );
  ufAssert( uval( bmerge1, 2 ).extract( 6, 2 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( bmerge1, 2 ).extract( 2, 4 ).containsOnlyBit( WIR_L4::bL ) );
  ufAssert( uval( bmerge1, 2 ).extract( 0, 2 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert(
    dval( bsplit1, 0 ).extract( 48, 16 ).containsOnlyBit( WIR_L4::b0 ) );
  for ( int i = 47; i >= 32; --i ) {
    ufAssert(
      dval( bsplit1, 0 ).getLocation( i ).getRegisterParameter().getRegister() == dExt1 );
    ufAssert( (int) dval( bsplit1, 0 ).getLocation( i ).getBitPosition() ==
      i - 32 );
  }
  ufAssert(
    dval( bsplit1, 0 ).extract( 16, 16 ).containsOnlyBit( WIR_L4::b0 ) );
  for ( int i = 15; i >= 0; --i ) {
    ufAssert(
      dval( bsplit1, 0 ).getLocation( i ).getRegisterParameter().getRegister() == dExt2 );
    ufAssert( (int) dval( bsplit1, 0 ).getLocation( i ).getBitPosition() == i );
  }

  ufAssert( uval( bsplit1, 1 ).extract( 0, 2 ).containsOnlyBit( WIR_L4::bL ) );
  ufAssert( uval( bsplit1, 1 ).extract( 2, 2 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( bsplit1, 1 ).extract( 4, 4 ).containsOnlyBit( WIR_L4::bL ) );
  ufAssert( uval( bsplit1, 1 ).extract( 8, 4 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( bsplit1, 1 ).extract( 12, 8 ).containsOnlyBit( WIR_L4::bL ) );
  ufAssert( uval( bsplit1, 1 ).extract( 20, 6 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( bsplit1, 1 ).extract( 26, 4 ).containsOnlyBit( WIR_L4::bL ) );
  ufAssert( uval( bsplit1, 1 ).extract( 30, 2 ).containsOnlyBit( WIR_L4::bX ) );

  return( 0 );
}
