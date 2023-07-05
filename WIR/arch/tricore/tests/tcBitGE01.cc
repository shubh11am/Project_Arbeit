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
  // operations GE & co.

  TC131 p;
  WIR_CompilationUnit c;
  c.setName( __FILE__ );
  WIR_Function &f = c.pushBackFunction( WIR_Function { "main" } );
  WIR_BasicBlock &b1 = f.pushBackBasicBlock( {} );
  const TC_DRegP &D4 = p.D4();
  auto &dExt = f.pushBackVirtualRegister( TC_DRegV() );
  auto &dMi123 = f.pushBackVirtualRegister( TC_DRegV() );
  auto &dPl64 = f.pushBackVirtualRegister( TC_DRegV() );
  auto &aExt = f.pushBackVirtualRegister( TC_ARegV() );
  auto &a0 = f.pushBackVirtualRegister( TC_ARegV() );
  auto &aPl64 = f.pushBackVirtualRegister( TC_ARegV() );
  auto &aPl123 = f.pushBackVirtualRegister( TC_ARegV() );
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
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( dMi123, WIR_Usage::def ),
      new TC_Const16_Signed( -123 ) } );

  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( dPl64, WIR_Usage::def ),
      new TC_Const16_Signed( 64 ) } );

  tcop(
    { TC131::OpCode::LEA, TC131::OperationFormat::AC18ABSA,
      new WIR_RegisterParameter( a0, WIR_Usage::def ),
      new TC_Const18_Unsigned( 0 ) } );

  tcop(
    { TC131::OpCode::LEA, TC131::OperationFormat::AC18ABSA,
      new WIR_RegisterParameter( aPl64, WIR_Usage::def ),
      new TC_Const18_Unsigned( 64 ) } );

  tcop(
    { TC131::OpCode::LEA, TC131::OperationFormat::AC18ABSA,
      new WIR_RegisterParameter( aPl123, WIR_Usage::def ),
      new TC_Const18_Unsigned( 123 ) } );

  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( bitMask, WIR_Usage::def ),
      new TC_Const16_Signed( 0x7FFE ) } );

  // GE
  auto &ge1 = tcop(
    { TC131::OpCode::GE, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::AND, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( bitMask, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &ge2 = tcop(
    { TC131::OpCode::GE, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &ge3 = tcop(
    { TC131::OpCode::GE, TC131::OperationFormat::DDC9_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const9_Signed( 64 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &ge4 = tcop(
    { TC131::OpCode::GE, TC131::OperationFormat::DDC9_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new TC_Const9_Signed( 64 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &ge5 = tcop(
    { TC131::OpCode::GE, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // GE.U
  auto &geu1 = tcop(
    { TC131::OpCode::GE_U, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &geu2 = tcop(
    { TC131::OpCode::GE_U, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &geu3 = tcop(
    { TC131::OpCode::GE_U, TC131::OperationFormat::DDC9_2,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const9_Unsigned( 64 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &geu4 = tcop(
    { TC131::OpCode::GE_U, TC131::OperationFormat::DDC9_2,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new TC_Const9_Unsigned( 64 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // GE.A
  auto &gea1 = tcop(
    { TC131::OpCode::GE_A, TC131::OperationFormat::DAA,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( aPl64, WIR_Usage::use ),
      new WIR_RegisterParameter( aPl123, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &gea2 = tcop(
    { TC131::OpCode::GE_A, TC131::OperationFormat::DAA,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( aPl123, WIR_Usage::use ),
      new WIR_RegisterParameter( aPl64, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &gea3 = tcop(
    { TC131::OpCode::GE_A, TC131::OperationFormat::DAA,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( aPl123, WIR_Usage::use ),
      new WIR_RegisterParameter( aPl123, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &gea4 = tcop(
    { TC131::OpCode::GE_A, TC131::OperationFormat::DAA,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( aPl123, WIR_Usage::use ),
      new WIR_RegisterParameter( aExt, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // Do bit-true data flow analysis.
  TC_BitDFA a { f };
  a.analyze();

  // Evaluate analysis results.
  ufAssert( dval( ge1, 0 ).extract( 1, 31 ).containsOnlyBit( WIR_L4::b0 ) );
  ufAssert( dval( ge1, 0 ).at( 0 ) == WIR_L4::b1 );
  ufAssert( uval( ge1, 1 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( ge1, 2 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( ge2, 0 ).extract( 1, 31 ).containsOnlyBit( WIR_L4::b0 ) );
  ufAssert( dval( ge2, 0 ).at( 0 ) == WIR_L4::b0 );
  ufAssert( dval( ge3, 0 ).extract( 1, 31 ).containsOnlyBit( WIR_L4::b0 ) );
  ufAssert( dval( ge3, 0 ).at( 0 ) == WIR_L4::b1 );
  ufAssert( dval( ge4, 0 ).extract( 1, 31 ).containsOnlyBit( WIR_L4::b0 ) );
  ufAssert( dval( ge4, 0 ).at( 0 ) == WIR_L4::bL );
  ufAssert( dval( ge5, 0 ).getSignedValue() == 1 );

  ufAssert( dval( geu1, 0 ).extract( 1, 31 ).containsOnlyBit( WIR_L4::b0 ) );
  ufAssert( dval( geu1, 0 ).at( 0 ) == WIR_L4::b0 );
  ufAssert( dval( geu2, 0 ).extract( 1, 31 ).containsOnlyBit( WIR_L4::b0 ) );
  ufAssert( dval( geu2, 0 ).at( 0 ) == WIR_L4::b1 );
  ufAssert( dval( geu3, 0 ).extract( 1, 31 ).containsOnlyBit( WIR_L4::b0 ) );
  ufAssert( dval( geu3, 0 ).at( 0 ) == WIR_L4::b1 );
  ufAssert( dval( geu4, 0 ).extract( 1, 31 ).containsOnlyBit( WIR_L4::b0 ) );
  ufAssert( dval( geu4, 0 ).at( 0 ) == WIR_L4::bL );

  ufAssert( dval( gea1, 0 ).extract( 1, 31 ).containsOnlyBit( WIR_L4::b0 ) );
  ufAssert( dval( gea1, 0 ).at( 0 ) == WIR_L4::b0 );
  ufAssert( dval( gea2, 0 ).extract( 1, 31 ).containsOnlyBit( WIR_L4::b0 ) );
  ufAssert( dval( gea2, 0 ).at( 0 ) == WIR_L4::b1 );
  ufAssert( dval( gea3, 0 ).extract( 1, 31 ).containsOnlyBit( WIR_L4::b0 ) );
  ufAssert( dval( gea3, 0 ).at( 0 ) == WIR_L4::b1 );
  ufAssert( dval( gea4, 0 ).extract( 1, 31 ).containsOnlyBit( WIR_L4::b0 ) );
  ufAssert( dval( gea4, 0 ).at( 0 ) == WIR_L4::bL );

  return( 0 );
}
