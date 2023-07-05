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
  // operations INS* & co.

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
    { TC131::OpCode::MOVH, TC131::OperationFormat::DC16_2,
      new WIR_RegisterParameter( bitMask, WIR_Usage::def ),
      new TC_Const16_Unsigned( 0xFF7F ) } );

  // INS.T
  auto &inst1 = tcop(
    { TC131::OpCode::INS_T, TC131::OperationFormat::DDC5DC5_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const5_Unsigned( 23 ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new TC_Const5_Unsigned( 9 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // INSN.T
  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new TC_Const16_Signed( 64 ) } );

  auto &insnt1 = tcop(
    { TC131::OpCode::INSN_T, TC131::OperationFormat::DDC5DC5_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const5_Unsigned( 23 ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new TC_Const5_Unsigned( 9 ) } );

  tcop(
    { TC131::OpCode::AND, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( bitMask, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // INSERT
  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( tmp1.rbegin()->get(), WIR_Usage::def ),
      new TC_Const16_Signed( 3 ) } );     // width

  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( tmp1.begin()->get(), WIR_Usage::def ),
      new TC_Const16_Signed( 13 ) } );     // pos

  auto &insert1 = tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDE,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( bitMask, WIR_Usage::def ),
      new TC_Const16_Signed( -12289 ) } );

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
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDE,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( tmp1.begin()->get(), WIR_Usage::def ),
      new TC_Const16_Signed( 7 ) } );     // pos

  auto &insert2 = tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDE,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // Do bit-true data flow analysis.
  TC_BitDFA a { f };
  a.analyze();

  // Evaluate analysis results.
  ufAssert( dval( inst1, 0 ).extract( 24, 8 ).getSignedValue() == 0xFF );
  ufAssert( dval( inst1, 0 ).at( 23 ) == WIR_L4::bL );
  ufAssert(
    dval( inst1, 0 ).getLocation( 23 ).getRegisterParameter().getRegister() ==
      dExt );
  ufAssert( dval( inst1, 0 ).getLocation( 23 ).getBitPosition() == 9 );
  ufAssert( dval( inst1, 0 ).extract( 0, 23 ).getSignedValue() == 0x7FFF85 );
  ufAssert( uval( inst1, 1 ).extract( 0, 23 ).isBinaryInteger() );
  ufAssert( uval( inst1, 1 ).at( 23 ) == WIR_L4::bX );
  ufAssert( uval( inst1, 1 ).extract( 24, 8 ).isBinaryInteger() );
  ufAssert( uval( inst1, 3 ).extract( 0, 9 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( inst1, 3 ).at( 9 ) == WIR_L4::bL );
  ufAssert( uval( inst1, 3 ).extract( 10, 22 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( insnt1, 0 ).extract( 24, 8 ).getSignedValue() == 0xFF );
  ufAssert( dval( insnt1, 0 ).at( 23 ) == WIR_L4::bN );
  ufAssert(
    dval( insnt1, 0 ).getLocation( 23 ).getRegisterParameter().getRegister() ==
      dExt );
  ufAssert( dval( insnt1, 0 ).getLocation( 23 ).getBitPosition() == 9 );
  ufAssert( dval( insnt1, 0 ).extract( 0, 23 ).getSignedValue() == 0x7FFF85 );
  ufAssert( uval( insnt1, 1 ).extract( 0, 16 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( insnt1, 1 ).extract( 16, 7 ).isBinaryInteger() );
  ufAssert( uval( insnt1, 1 ).at( 23 ) == WIR_L4::bX );
  ufAssert( uval( insnt1, 1 ).extract( 24, 8 ).isBinaryInteger() );
  ufAssert( uval( insnt1, 3 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( insert1, 0 ).getSignedValue() == 41024 );
  ufAssert( uval( insert1, 1 ).extract( 0, 12 ).isBinaryInteger() );
  ufAssert( uval( insert1, 1 ).extract( 12, 4 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( insert1, 1 ).extract( 16, 16 ).isBinaryInteger() );
  ufAssert( uval( insert1, 2 ).at( 0 ) == WIR_L4::bX );
  ufAssert( uval( insert1, 2 ).extract( 1, 2 ).isBinaryInteger() );
  ufAssert( uval( insert1, 2 ).extract( 3, 29 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( insert1, 3 ).extract( 0, 5 ).isBinaryInteger() );
  ufAssert( uval( insert1, 3 ).extract( 5, 27 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval1( insert1, 3 ).extract( 0, 5 ).isBinaryInteger() );
  ufAssert( uval1( insert1, 3 ).extract( 5, 27 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( insert2, 0 ).extract( 0, 7 ).getSignedValue() == 64 );
  ufAssert( dval( insert2, 0 ).at( 7 ) == WIR_L4::bL );
  ufAssert(
    dval( insert2, 0 ).getLocation( 7 ).getRegisterParameter().getRegister() ==
      dExt );
  ufAssert( dval( insert2, 0 ).getLocation( 7 ).getBitPosition() == 0 );
  ufAssert( dval( insert2, 0 ).at( 8 ) == WIR_L4::bL );
  ufAssert(
    dval( insert2, 0 ).getLocation( 8 ).getRegisterParameter().getRegister() ==
      dExt );
  ufAssert( dval( insert2, 0 ).getLocation( 8 ).getBitPosition() == 1 );
  ufAssert( dval( insert2, 0 ).at( 9 ) == WIR_L4::bL );
  ufAssert(
    dval( insert2, 0 ).getLocation( 9 ).getRegisterParameter().getRegister() ==
      dExt );
  ufAssert( dval( insert2, 0 ).getLocation( 9 ).getBitPosition() == 2 );
  ufAssert( dval( insert2, 0 ).extract( 10, 22 ).getSignedValue() == 40 );

  return( 0 );
}
