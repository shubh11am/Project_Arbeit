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
  // operations MOV & co.

  TC131 p;
  WIR_CompilationUnit c;
  c.setName( __FILE__ );
  WIR_Function &f = c.pushBackFunction( WIR_Function { "main" } );
  WIR_BasicBlock &b1 = f.pushBackBasicBlock( {} );
  const TC_DRegP &D4 = p.D4();
  const TC_DRegP &D15 = p.D15();
  const TC_ARegP &A4 = p.A4();
  auto &dMi123 = f.pushBackVirtualRegister( TC_DRegV() );
  auto &dPl64 = f.pushBackVirtualRegister( TC_DRegV() );
  auto &tmp0 = f.pushBackVirtualRegister( TC_DRegV() );
  auto &tmp1 = f.pushBackVirtualRegister( TC_ARegV() );
  auto &tmp2 = f.pushBackVirtualRegister( TC_DRegV() );
  auto &tmp3 = f.pushBackVirtualRegister( TC_ARegV() );

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
    auto &rp = dynamic_cast<WIR_RegisterParameter &>( it->get() );
    auto &c = rp.getContainers<WIR_BitValues>().begin()->get();
    return( c.getOutValues().begin()->downVal );
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

  // MOV + MOV_RR
  auto &mov1 = tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new TC_Const16_Signed( -123 ) } );

  auto &movrr1 = tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( tmp2, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp2, WIR_Usage::use ) } );

  auto &mov2 = tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::SIC8_1,
      new WIR_RegisterParameter( D15, WIR_Usage::def ),
      new TC_Const8_Unsigned( 64 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::SDD_1,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( D15, WIR_Usage::use ) } );

  auto &mov3 = tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::SDC4_1,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new TC_Const4_Signed( 5 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // MOV.A + MOV_AA
  auto &mova1 = tcop(
    { TC131::OpCode::MOV_A, TC131::OperationFormat::AD,
      new WIR_RegisterParameter( tmp1, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ) } );

  auto &movaa1 = tcop(
    { TC131::OpCode::MOV_AA, TC131::OperationFormat::AA,
      new WIR_RegisterParameter( tmp3, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_AA, TC131::OperationFormat::AA,
      new WIR_RegisterParameter( A4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp3, WIR_Usage::use ) } );

  auto &mova2 = tcop(
    { TC131::OpCode::MOV_A, TC131::OperationFormat::SAC4_1,
      new WIR_RegisterParameter( tmp1, WIR_Usage::def ),
      new TC_Const4_Unsigned( 7 ) } );

  tcop(
    { TC131::OpCode::MOV_AA, TC131::OperationFormat::AA,
      new WIR_RegisterParameter( A4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ) } );

  auto &mova3 = tcop(
    { TC131::OpCode::MOV_A, TC131::OperationFormat::SAD_1,
      new WIR_RegisterParameter( tmp1, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_AA, TC131::OperationFormat::SAA_1,
      new WIR_RegisterParameter( A4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ) } );

  // MOV.D
  auto &movd1 = tcop(
    { TC131::OpCode::MOV_D, TC131::OperationFormat::DA,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // MOV.U
  auto &movu1 = tcop(
    { TC131::OpCode::MOV_U, TC131::OperationFormat::DC16_2,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new TC_Const16_Unsigned( 26361 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // MOVH
  auto &movh1 = tcop(
    { TC131::OpCode::MOVH, TC131::OperationFormat::DC16_2,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new TC_Const16_Unsigned( 26361 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // MOVH.A
  auto &movha1 = tcop(
    { TC131::OpCode::MOVH_A, TC131::OperationFormat::AC16,
      new WIR_RegisterParameter( tmp1, WIR_Usage::def ),
      new TC_Const16_Unsigned( 26361 ) } );

  tcop(
    { TC131::OpCode::MOV_AA, TC131::OperationFormat::SAA_1,
      new WIR_RegisterParameter( A4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ) } );

  auto &movha2 = tcop(
    { TC131::OpCode::MOVH_A, TC131::OperationFormat::AL_1,
      new WIR_RegisterParameter( tmp1, WIR_Usage::def ),
      new WIR_LabelParameter( b1 ) } );

  tcop(
    { TC131::OpCode::MOV_AA, TC131::OperationFormat::SAA_1,
      new WIR_RegisterParameter( A4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ) } );

  // Do bit-true data flow analysis.
  TC_BitDFA a { f };
  a.analyze();

  // Evaluate analysis results.
  ufAssert( dval( mov1, 0 ).getSignedValue() == -123 );
  ufAssert( dval( mov2, 0 ).getSignedValue() == 64 );
  ufAssert( dval( mov3, 0 ).getSignedValue() == 5 );

  ufAssert( dval( movrr1, 0 ).getSignedValue() == -123 );

  ufAssert( dval( mova1, 0 ).getSignedValue() == 64 );
  ufAssert( dval( mova2, 0 ).getSignedValue() == 7 );
  ufAssert( dval( mova3, 0 ).getSignedValue() == 0xFFFFFF85 );

  ufAssert( dval( movaa1, 0 ).getSignedValue() == 64 );

  ufAssert( dval( movd1, 0 ).getSignedValue() == 0xFFFFFF85 );

  ufAssert( dval( movu1, 0 ).getSignedValue() == 26361 );

  ufAssert( dval( movh1, 0 ).getSignedValue() == 0x66F90000 );

  ufAssert( dval( movha1, 0 ).getSignedValue() == 0x66F90000 );
  ufAssert( dval( movha2, 0 ).containsOnlyBit( WIR_L4::bL ) );

  return( 0 );
}
