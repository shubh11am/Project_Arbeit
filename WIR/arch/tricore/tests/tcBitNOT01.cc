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
  // operation NOT.

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

  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new TC_Const16_Signed( 64 ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDC5C5,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const5_Unsigned( 8 ),
      new TC_Const5_Unsigned( 8 ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDC5C5,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( dExt, WIR_Usage::use ),
      new TC_Const5_Unsigned( 16 ),
      new TC_Const5_Unsigned( 8 ) } );

  auto &ins1 = tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDC5C5,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 24 ),
      new TC_Const5_Unsigned( 8 ) } );

  // NOT
  auto &not1 = tcop(
    { TC131::OpCode::NOT, TC131::OperationFormat::SD,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &not2 = tcop(
    { TC131::OpCode::NOT, TC131::OperationFormat::SD,
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ) } );

  tcop(
    { TC131::OpCode::EXTR, TC131::OperationFormat::DDC5C5,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new TC_Const5_Unsigned( 13 ),
      new TC_Const5_Unsigned( 9 ) } );

  // Do bit-true data flow analysis.
  TC_BitDFA a { f };
  a.analyze();

  // Evaluate analysis results.
  ufAssert( dval( not1, 0 ).extract( 0, 8 ).getSignedValue() == 191 );
  ufAssert( dval( not1, 0 ).extract( 8, 8 ).getSignedValue() == 122 );
  ufAssert( dval( not1, 0 ).extract( 16, 8 ).containsOnlyBit( WIR_L4::bN ) );
  ufAssert(
    dval( not1, 0 ).getLocation( 16 ).getRegisterParameter().getRegister() ==
      dExt );
  ufAssert( dval( not1, 0 ).getLocation( 16 ).getBitPosition() == 0 );
  ufAssert(
    dval( not1, 0 ).getLocation( 17 ).getRegisterParameter().getRegister() ==
      dExt );
  ufAssert( dval( not1, 0 ).getLocation( 17 ).getBitPosition() == 1 );
  ufAssert(
    dval( not1, 0 ).getLocation( 18 ).getRegisterParameter().getRegister() ==
      dExt );
  ufAssert( dval( not1, 0 ).getLocation( 18 ).getBitPosition() == 2 );
  ufAssert(
    dval( not1, 0 ).getLocation( 19 ).getRegisterParameter().getRegister() ==
      dExt );
  ufAssert( dval( not1, 0 ).getLocation( 19 ).getBitPosition() == 3 );
  ufAssert(
    dval( not1, 0 ).getLocation( 20 ).getRegisterParameter().getRegister() ==
      dExt );
  ufAssert( dval( not1, 0 ).getLocation( 20 ).getBitPosition() == 4 );
  ufAssert(
    dval( not1, 0 ).getLocation( 21 ).getRegisterParameter().getRegister() ==
      dExt );
  ufAssert( dval( not1, 0 ).getLocation( 21 ).getBitPosition() == 5 );
  ufAssert(
    dval( not1, 0 ).getLocation( 22 ).getRegisterParameter().getRegister() ==
      dExt );
  ufAssert( dval( not1, 0 ).getLocation( 22 ).getBitPosition() == 6 );
  ufAssert(
    dval( not1, 0 ).getLocation( 23 ).getRegisterParameter().getRegister() ==
      dExt );
  ufAssert( dval( not1, 0 ).getLocation( 23 ).getBitPosition() == 7 );
  ufAssert( dval( not1, 0 ).extract( 24, 8 ).getSignedValue() == 191 );

  ufAssert( ( dval( not2, 0 ) == dval( ins1, 0 ) ) == WIR_L4::b1 );
  ufAssert( uval( not2, 0 ).extract( 0, 13 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( not2, 0 ).extract( 13, 3 ).isBinaryInteger() );
  ufAssert( uval( not2, 0 ).extract( 16, 6 ).containsOnlyBit( WIR_L4::bN ) );
  ufAssert( uval( not2, 0 ).extract( 22, 10 ).containsOnlyBit( WIR_L4::bX ) );

  return( 0 );
}
