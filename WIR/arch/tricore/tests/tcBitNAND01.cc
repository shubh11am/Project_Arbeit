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
  // operations NAND & co.

  TC131 p;
  WIR_CompilationUnit c;
  c.setName( __FILE__ );
  WIR_Function &f = c.pushBackFunction( WIR_Function { "main" } );
  WIR_BasicBlock &b1 = f.pushBackBasicBlock( {} );
  const TC_DRegP &D4 = p.D4();
  auto &dMi123 = f.pushBackVirtualRegister( TC_DRegV() );
  auto &dPl64 = f.pushBackVirtualRegister( TC_DRegV() );
  auto &dFF0000EF = f.pushBackVirtualRegister( TC_DRegV() );
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
    { TC131::OpCode::MOVH, TC131::OperationFormat::DC16_2,
      new WIR_RegisterParameter( dFF0000EF, WIR_Usage::def ),
      new TC_Const16_Unsigned( 0xFF00 ) } );

  tcop(
    { TC131::OpCode::ADDI, TC131::OperationFormat::DDC16_1,
      new WIR_RegisterParameter( dFF0000EF, WIR_Usage::def ),
      new WIR_RegisterParameter( dFF0000EF, WIR_Usage::use ),
      new TC_Const16_Signed( 0x00EF ) } );

  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( dMi123, WIR_Usage::def ),
      new TC_Const16_Signed( -123 ) } );

  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( dPl64, WIR_Usage::def ),
      new TC_Const16_Signed( 64 ) } );

  // NAND
  auto &nand1 = tcop(
    { TC131::OpCode::NAND, TC131::OperationFormat::DDC9_2,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dFF0000EF, WIR_Usage::use ),
      new TC_Const9_Unsigned( 123 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // NAND.T
  auto &nandt1 = tcop(
    { TC131::OpCode::NAND_T, TC131::OperationFormat::DDC5DC5_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const5_Unsigned( 25 ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new TC_Const5_Unsigned( 6 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &nandt2 = tcop(
    { TC131::OpCode::NAND_T, TC131::OperationFormat::DDC5DC5_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new TC_Const5_Unsigned( 25 ),
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
  ufAssert( dval( nand1, 0 ).getSignedValue() == 0xFFFFFF94 );
  ufAssert( uval( nand1, 1 ).extract( 0, 2 ).isBinaryInteger() );
  ufAssert( uval( nand1, 1 ).at( 2 ) == WIR_L4::bX );
  ufAssert( uval( nand1, 1 ).extract( 3, 4 ).isBinaryInteger() );
  ufAssert( uval( nand1, 1 ).extract( 7, 25 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( nand1, 2 ).extract( 0, 4 ).isBinaryInteger() );
  ufAssert( uval( nand1, 2 ).at( 4 ) == WIR_L4::bX );
  ufAssert( uval( nand1, 2 ).extract( 5, 4 ).isBinaryInteger() );

  ufAssert( dval( nandt1, 0 ).extract( 1, 31  ).containsOnlyBit( WIR_L4::b0 ) );
  ufAssert( dval( nandt1, 0 ).at( 0 ) == WIR_L4::b0 );
  ufAssert( uval( nandt1, 1 ).extract( 0, 25 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( nandt1, 1 ).at( 25 ) == WIR_L4::b1 );
  ufAssert( uval( nandt1, 1 ).extract( 26, 6 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( nandt1, 3 ).extract( 0, 6 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( nandt1, 3 ).at( 6 ) == WIR_L4::b1 );
  ufAssert( uval( nandt1, 3 ).extract( 7, 25 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( nandt2, 0 ).extract( 1, 31  ).containsOnlyBit( WIR_L4::b0 ) );
  ufAssert( dval( nandt2, 0 ).at( 0 ) == WIR_L4::b1 );
  ufAssert( uval( nandt2, 1 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( nandt2, 3 ).extract( 0, 5 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( nandt2, 3 ).at( 5 ) == WIR_L4::b0 );
  ufAssert( uval( nandt2, 3 ).extract( 6, 26 ).containsOnlyBit( WIR_L4::bX ) );

  return( 0 );
}
