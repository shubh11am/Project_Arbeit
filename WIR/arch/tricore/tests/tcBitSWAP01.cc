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
  // operation SWAP.W.

  TC131 p;
  WIR_CompilationUnit c;
  c.setName( __FILE__ );
  WIR_Function &f = c.pushBackFunction( WIR_Function { "main" } );
  WIR_BasicBlock &b1 = f.pushBackBasicBlock( {} );
  const TC_ARegP &A4 = p.A4();
  auto &pbra = f.pushBackVirtualRegister( TC_PRegV() );
  auto &a42 = f.pushBackVirtualRegister( TC_ARegV() );
  auto &tmp0 = f.pushBackVirtualRegister( TC_DRegV() );
  auto &tmp1 = f.pushBackVirtualRegister( TC_DRegV() );
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
    { TC131::OpCode::MOVH_A, TC131::OperationFormat::AC16,
      new WIR_RegisterParameter( pbra.rbegin()->get(), WIR_Usage::def ),
      new TC_Const16_Unsigned( 0xF000 ) } );    // incr

  tcop(
    { TC131::OpCode::LEA, TC131::OperationFormat::AAC16BOA,
      new WIR_RegisterParameter( pbra.rbegin()->get(), WIR_Usage::def ),
      new WIR_RegisterParameter( pbra.rbegin()->get(), WIR_Usage::use ),
      new TC_Const16_Signed( 42 ) } );          // index

  tcop(
    { TC131::OpCode::LEA, TC131::OperationFormat::AC18ABSA,
      new WIR_RegisterParameter( pbra.begin()->get(), WIR_Usage::def ),
      new TC_Const18_Unsigned( 0x42 ) } );

  tcop(
    { TC131::OpCode::LEA, TC131::OperationFormat::AC18ABSA,
      new WIR_RegisterParameter( a42, WIR_Usage::def ),
      new TC_Const18_Unsigned( 42 ) } );

  tcop(
    { TC131::OpCode::MOVH, TC131::OperationFormat::DC16_2,
      new WIR_RegisterParameter( bitMask, WIR_Usage::def ),
      new TC_Const16_Unsigned( 0x0FFF ) } );

  // SWAP.W
  auto &swapw1 = tcop(
    { TC131::OpCode::SWAP_W, TC131::OperationFormat::PDBRA_2,
      new WIR_RegisterParameter( pbra, WIR_Usage::defuse ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ) } );

  tcop(
    { TC131::OpCode::MOV_AA, TC131::OperationFormat::SAA_1,
      new WIR_RegisterParameter( A4, WIR_Usage::def ),
      new WIR_RegisterParameter( pbra.rbegin()->get(), WIR_Usage::use ) } );

  auto &swapw2 = tcop(
    { TC131::OpCode::SWAP_W, TC131::OperationFormat::AC10DPIA_2,
      new WIR_AddressingModeParameter( TC131::AddressingMode::pre ),
      new WIR_RegisterParameter( a42, WIR_Usage::defuse ),
      new TC_Const10_Signed( -12 ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::defuse ) } );

  tcop(
    { TC131::OpCode::MOV_D, TC131::OperationFormat::DA,
      new WIR_RegisterParameter( tmp1, WIR_Usage::def ),
      new WIR_RegisterParameter( a42, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::AND, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( tmp1, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ),
      new WIR_RegisterParameter( bitMask, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_A, TC131::OperationFormat::SAD_1,
      new WIR_RegisterParameter( a42, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_AA, TC131::OperationFormat::SAA_1,
      new WIR_RegisterParameter( A4, WIR_Usage::def ),
      new WIR_RegisterParameter( a42, WIR_Usage::use ) } );

  // Do bit-true data flow analysis.
  TC_BitDFA a { f };
  a.analyze();

  // Evaluate analysis results.
  ufAssert( dval( swapw1, 0 ).extract( 32, 16 ).getSignedValue() == 0xF02A );
  ufAssert( dval( swapw1, 0 ).extract( 48, 16 ).getSignedValue() == 0xF000 );

  ufAssert( dval( swapw2, 1 ).getSignedValue() == 30 );
  ufAssert( uval( swapw2, 1 ).extract( 0, 28 ).isBinaryInteger() );
  ufAssert( uval( swapw2, 1 ).extract( 28, 4 ).containsOnlyBit( WIR_L4::bX ) );


  return( 0 );
}
