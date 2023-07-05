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
  // operations LD* & co.

  TC131 p;
  WIR_CompilationUnit c;
  c.setName( __FILE__ );
  WIR_Function &f = c.pushBackFunction( WIR_Function { "main" } );
  WIR_BasicBlock &b1 = f.pushBackBasicBlock( {} );
  const TC_DRegP &D4 = p.D4();
  const TC_ARegP &A4 = p.A4();
  auto &pbra = f.pushBackVirtualRegister( TC_PRegV() );
  auto &a42 = f.pushBackVirtualRegister( TC_ARegV() );
  auto &tmp0 = f.pushBackVirtualRegister( TC_ARegV() );
  auto &tmp1 = f.pushBackVirtualRegister( TC_DRegV() );
  auto &tmp2 = f.pushBackVirtualRegister( TC_ERegV() );
  auto &tmp3 = f.pushBackVirtualRegister( TC_PRegV() );
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

  // LD.A
  auto &lda1 = tcop(
    { TC131::OpCode::LD_A, TC131::OperationFormat::APBRA,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( pbra, WIR_Usage::defuse ) } );

  tcop(
    { TC131::OpCode::MOV_AA, TC131::OperationFormat::SAA_1,
      new WIR_RegisterParameter( A4, WIR_Usage::def ),
      new WIR_RegisterParameter( pbra.rbegin()->get(), WIR_Usage::use ) } );

  auto &lda2 = tcop(
    { TC131::OpCode::LD_A, TC131::OperationFormat::AAC10PIA,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_AddressingModeParameter( TC131::AddressingMode::post ),
      new WIR_RegisterParameter( a42, WIR_Usage::defuse ),
      new TC_Const10_Signed( -12 ) } );

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

  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( tmp1, WIR_Usage::def ),
      new TC_Const16_Signed( 30 ) } );

  tcop(
    { TC131::OpCode::MOV_A, TC131::OperationFormat::SAD_1,
      new WIR_RegisterParameter( a42, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ) } );

  auto &lda3 = tcop(
    { TC131::OpCode::LD_A, TC131::OperationFormat::SAA_3,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( a42, WIR_Usage::defuse ) } );

  tcop(
    { TC131::OpCode::MOV_AA, TC131::OperationFormat::SAA_1,
      new WIR_RegisterParameter( A4, WIR_Usage::def ),
      new WIR_RegisterParameter( a42, WIR_Usage::use ) } );

  // LD.B
  auto &ldb1 = tcop(
    { TC131::OpCode::LD_B, TC131::OperationFormat::DPBRA,
      new WIR_RegisterParameter( tmp1, WIR_Usage::def ),
      new WIR_RegisterParameter( pbra, WIR_Usage::defuse ) } );

  tcop(
    { TC131::OpCode::MOV_AA, TC131::OperationFormat::SAA_1,
      new WIR_RegisterParameter( A4, WIR_Usage::def ),
      new WIR_RegisterParameter( pbra.rbegin()->get(), WIR_Usage::use ) } );

  auto &ldb2 = tcop(
    { TC131::OpCode::LD_B, TC131::OperationFormat::DAC10PIA,
      new WIR_RegisterParameter( tmp1, WIR_Usage::def ),
      new WIR_AddressingModeParameter( TC131::AddressingMode::pre ),
      new WIR_RegisterParameter( a42, WIR_Usage::defuse ),
      new TC_Const10_Signed( 13 ) } );

  tcop(
    { TC131::OpCode::MOV_AA, TC131::OperationFormat::SAA_1,
      new WIR_RegisterParameter( A4, WIR_Usage::def ),
      new WIR_RegisterParameter( a42, WIR_Usage::use ) } );

  // LD.BU
  auto &ldbu1 = tcop(
    { TC131::OpCode::LD_BU, TC131::OperationFormat::DPBRA,
      new WIR_RegisterParameter( tmp1, WIR_Usage::def ),
      new WIR_RegisterParameter( pbra, WIR_Usage::defuse ) } );

  tcop(
    { TC131::OpCode::MOV_AA, TC131::OperationFormat::SAA_1,
      new WIR_RegisterParameter( A4, WIR_Usage::def ),
      new WIR_RegisterParameter( pbra.rbegin()->get(), WIR_Usage::use ) } );

  auto &ldbu2 = tcop(
    { TC131::OpCode::LD_BU, TC131::OperationFormat::DAC10PIA,
      new WIR_RegisterParameter( tmp1, WIR_Usage::def ),
      new WIR_AddressingModeParameter( TC131::AddressingMode::post ),
      new WIR_RegisterParameter( a42, WIR_Usage::defuse ),
      new TC_Const10_Signed( 3 ) } );

  tcop(
    { TC131::OpCode::MOV_AA, TC131::OperationFormat::SAA_1,
      new WIR_RegisterParameter( A4, WIR_Usage::def ),
      new WIR_RegisterParameter( a42, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ) } );

  auto &ldbu3 = tcop(
    { TC131::OpCode::LD_BU, TC131::OperationFormat::SDA_3,
      new WIR_RegisterParameter( tmp1, WIR_Usage::def ),
      new WIR_RegisterParameter( a42, WIR_Usage::defuse ) } );

  tcop(
    { TC131::OpCode::MOV_AA, TC131::OperationFormat::SAA_1,
      new WIR_RegisterParameter( A4, WIR_Usage::def ),
      new WIR_RegisterParameter( a42, WIR_Usage::use ) } );

  // LD.D
  auto &ldd1 = tcop(
    { TC131::OpCode::LD_D, TC131::OperationFormat::EPBRA,
      new WIR_RegisterParameter( tmp2, WIR_Usage::def ),
      new WIR_RegisterParameter( pbra, WIR_Usage::defuse ) } );

  tcop(
    { TC131::OpCode::MOV_AA, TC131::OperationFormat::SAA_1,
      new WIR_RegisterParameter( A4, WIR_Usage::def ),
      new WIR_RegisterParameter( pbra.rbegin()->get(), WIR_Usage::use ) } );

  auto &ldd2 = tcop(
    { TC131::OpCode::LD_D, TC131::OperationFormat::EAC10PIA,
      new WIR_RegisterParameter( tmp2, WIR_Usage::def ),
      new WIR_AddressingModeParameter( TC131::AddressingMode::pre ),
      new WIR_RegisterParameter( a42, WIR_Usage::defuse ),
      new TC_Const10_Signed( -14 ) } );

  tcop(
    { TC131::OpCode::MOV_AA, TC131::OperationFormat::SAA_1,
      new WIR_RegisterParameter( A4, WIR_Usage::def ),
      new WIR_RegisterParameter( a42, WIR_Usage::use ) } );

  // LD.DA
  auto &ldda1 = tcop(
    { TC131::OpCode::LD_DA, TC131::OperationFormat::PPBRA_1,
      new WIR_RegisterParameter( tmp3, WIR_Usage::def ),
      new WIR_RegisterParameter( pbra, WIR_Usage::defuse ) } );

  tcop(
    { TC131::OpCode::MOV_AA, TC131::OperationFormat::SAA_1,
      new WIR_RegisterParameter( A4, WIR_Usage::def ),
      new WIR_RegisterParameter( pbra.rbegin()->get(), WIR_Usage::use ) } );

  auto &ldda2 = tcop(
    { TC131::OpCode::LD_DA, TC131::OperationFormat::PAC10PIA,
      new WIR_RegisterParameter( tmp3, WIR_Usage::def ),
      new WIR_AddressingModeParameter( TC131::AddressingMode::post ),
      new WIR_RegisterParameter( a42, WIR_Usage::defuse ),
      new TC_Const10_Signed( 0 ) } );

  tcop(
    { TC131::OpCode::MOV_AA, TC131::OperationFormat::SAA_1,
      new WIR_RegisterParameter( A4, WIR_Usage::def ),
      new WIR_RegisterParameter( a42, WIR_Usage::use ) } );

  // LD.H
  auto &ldh1 = tcop(
    { TC131::OpCode::LD_H, TC131::OperationFormat::DPBRA,
      new WIR_RegisterParameter( tmp1, WIR_Usage::def ),
      new WIR_RegisterParameter( pbra, WIR_Usage::defuse ) } );

  tcop(
    { TC131::OpCode::MOV_AA, TC131::OperationFormat::SAA_1,
      new WIR_RegisterParameter( A4, WIR_Usage::def ),
      new WIR_RegisterParameter( pbra.rbegin()->get(), WIR_Usage::use ) } );

  auto &ldh2 = tcop(
    { TC131::OpCode::LD_H, TC131::OperationFormat::DAC10PIA,
      new WIR_RegisterParameter( tmp1, WIR_Usage::def ),
      new WIR_AddressingModeParameter( TC131::AddressingMode::pre ),
      new WIR_RegisterParameter( a42, WIR_Usage::defuse ),
      new TC_Const10_Signed( 9 ) } );

  tcop(
    { TC131::OpCode::MOV_AA, TC131::OperationFormat::SAA_1,
      new WIR_RegisterParameter( A4, WIR_Usage::def ),
      new WIR_RegisterParameter( a42, WIR_Usage::use ) } );

  auto &ldh3 = tcop(
    { TC131::OpCode::LD_H, TC131::OperationFormat::SDA_3,
      new WIR_RegisterParameter( tmp1, WIR_Usage::def ),
      new WIR_RegisterParameter( a42, WIR_Usage::defuse ) } );

  tcop(
    { TC131::OpCode::MOV_AA, TC131::OperationFormat::SAA_1,
      new WIR_RegisterParameter( A4, WIR_Usage::def ),
      new WIR_RegisterParameter( a42, WIR_Usage::use ) } );

  // LD.HU
  auto &ldhu1 = tcop(
    { TC131::OpCode::LD_HU, TC131::OperationFormat::DPBRA,
      new WIR_RegisterParameter( tmp1, WIR_Usage::def ),
      new WIR_RegisterParameter( pbra, WIR_Usage::defuse ) } );

  tcop(
    { TC131::OpCode::MOV_AA, TC131::OperationFormat::SAA_1,
      new WIR_RegisterParameter( A4, WIR_Usage::def ),
      new WIR_RegisterParameter( pbra.rbegin()->get(), WIR_Usage::use ) } );

  auto &ldhu2 = tcop(
    { TC131::OpCode::LD_HU, TC131::OperationFormat::DAC10PIA,
      new WIR_RegisterParameter( tmp1, WIR_Usage::def ),
      new WIR_AddressingModeParameter( TC131::AddressingMode::post ),
      new WIR_RegisterParameter( a42, WIR_Usage::defuse ),
      new TC_Const10_Signed( -40 ) } );

  tcop(
    { TC131::OpCode::MOV_AA, TC131::OperationFormat::SAA_1,
      new WIR_RegisterParameter( A4, WIR_Usage::def ),
      new WIR_RegisterParameter( a42, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ) } );

  // LD.Q
  auto &ldq1 = tcop(
    { TC131::OpCode::LD_Q, TC131::OperationFormat::DPBRA,
      new WIR_RegisterParameter( tmp1, WIR_Usage::def ),
      new WIR_RegisterParameter( pbra, WIR_Usage::defuse ) } );

  tcop(
    { TC131::OpCode::MOV_AA, TC131::OperationFormat::SAA_1,
      new WIR_RegisterParameter( A4, WIR_Usage::def ),
      new WIR_RegisterParameter( pbra.rbegin()->get(), WIR_Usage::use ) } );

  auto &ldq2 = tcop(
    { TC131::OpCode::LD_Q, TC131::OperationFormat::DAC10PIA,
      new WIR_RegisterParameter( tmp1, WIR_Usage::def ),
      new WIR_AddressingModeParameter( TC131::AddressingMode::pre ),
      new WIR_RegisterParameter( a42, WIR_Usage::defuse ),
      new TC_Const10_Signed( 20 ) } );

  tcop(
    { TC131::OpCode::MOV_AA, TC131::OperationFormat::SAA_1,
      new WIR_RegisterParameter( A4, WIR_Usage::def ),
      new WIR_RegisterParameter( a42, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ) } );

  // LD.W
  auto &ldw1 = tcop(
    { TC131::OpCode::LD_W, TC131::OperationFormat::DPBRA,
      new WIR_RegisterParameter( tmp1, WIR_Usage::def ),
      new WIR_RegisterParameter( pbra, WIR_Usage::defuse ) } );

  tcop(
    { TC131::OpCode::MOV_AA, TC131::OperationFormat::SAA_1,
      new WIR_RegisterParameter( A4, WIR_Usage::def ),
      new WIR_RegisterParameter( pbra.rbegin()->get(), WIR_Usage::use ) } );

  auto &ldw2 = tcop(
    { TC131::OpCode::LD_W, TC131::OperationFormat::DAC10PIA,
      new WIR_RegisterParameter( tmp1, WIR_Usage::def ),
      new WIR_AddressingModeParameter( TC131::AddressingMode::pre ),
      new WIR_RegisterParameter( a42, WIR_Usage::defuse ),
      new TC_Const10_Signed( 10 ) } );

  tcop(
    { TC131::OpCode::MOV_AA, TC131::OperationFormat::SAA_1,
      new WIR_RegisterParameter( A4, WIR_Usage::def ),
      new WIR_RegisterParameter( a42, WIR_Usage::use ) } );

  auto &ldw3 = tcop(
    { TC131::OpCode::LD_W, TC131::OperationFormat::SDA_3,
      new WIR_RegisterParameter( tmp1, WIR_Usage::def ),
      new WIR_RegisterParameter( a42, WIR_Usage::defuse ) } );

  tcop(
    { TC131::OpCode::MOV_AA, TC131::OperationFormat::SAA_1,
      new WIR_RegisterParameter( A4, WIR_Usage::def ),
      new WIR_RegisterParameter( a42, WIR_Usage::use ) } );

  // LDMST
  auto &ldmst1 = tcop(
    { TC131::OpCode::LDMST, TC131::OperationFormat::PEBRA,
      new WIR_RegisterParameter( pbra, WIR_Usage::defuse ),
      new WIR_RegisterParameter( tmp2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_AA, TC131::OperationFormat::SAA_1,
      new WIR_RegisterParameter( A4, WIR_Usage::def ),
      new WIR_RegisterParameter( pbra.rbegin()->get(), WIR_Usage::use ) } );

  auto &ldmst2 = tcop(
    { TC131::OpCode::LDMST, TC131::OperationFormat::AC10EPIA,
      new WIR_AddressingModeParameter( TC131::AddressingMode::pre ),
      new WIR_RegisterParameter( a42, WIR_Usage::defuse ),
      new TC_Const10_Signed( 24 ),
      new WIR_RegisterParameter( tmp2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_AA, TC131::OperationFormat::SAA_1,
      new WIR_RegisterParameter( A4, WIR_Usage::def ),
      new WIR_RegisterParameter( a42, WIR_Usage::use ) } );

  // LEA
  auto &lea1 = tcop(
    { TC131::OpCode::LEA, TC131::OperationFormat::AC18ABSA,
      new WIR_RegisterParameter( pbra.rbegin()->get(), WIR_Usage::def ),
      new TC_Const18_Unsigned( 16383 ) } );

  tcop(
    { TC131::OpCode::MOV_AA, TC131::OperationFormat::SAA_1,
      new WIR_RegisterParameter( A4, WIR_Usage::def ),
      new WIR_RegisterParameter( pbra.rbegin()->get(), WIR_Usage::use ) } );

  auto &lea2 = tcop(
    { TC131::OpCode::LEA, TC131::OperationFormat::AAC10BOA,
      new WIR_RegisterParameter( pbra.rbegin()->get(), WIR_Usage::def ),
      new WIR_RegisterParameter( a42, WIR_Usage::use ),
      new TC_Const10_Signed( -24 ) } );

  tcop(
    { TC131::OpCode::MOV_AA, TC131::OperationFormat::SAA_1,
      new WIR_RegisterParameter( A4, WIR_Usage::def ),
      new WIR_RegisterParameter( pbra.rbegin()->get(), WIR_Usage::use ) } );

  // Do bit-true data flow analysis.
  TC_BitDFA a { f };
  a.analyze();

  // Evaluate analysis results.
  ufAssert( dval( lda1, 1 ).extract( 32, 16 ).getSignedValue() == 0xF02A );
  ufAssert( dval( lda1, 1 ).extract( 48, 16 ).getSignedValue() == 0xF000 );

  ufAssert( dval( lda2, 2 ).getSignedValue() == 30 );
  ufAssert( uval( lda2, 2 ).extract( 0, 28 ).isBinaryInteger() );
  ufAssert( uval( lda2, 2 ).extract( 28, 4 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( lda3, 1 ).getSignedValue() == 34 );

  ufAssert( dval( ldb1, 1 ).extract( 32, 16 ).getSignedValue() == 0x782A );
  ufAssert( dval( ldb1, 1 ).extract( 48, 16 ).getSignedValue() == 0xF000 );
  ufAssert( dval( ldb2, 2 ).getSignedValue() == 47 );

  ufAssert( dval( ldbu1, 1 ).extract( 32, 16 ).getSignedValue() == 0xB42A );
  ufAssert( dval( ldbu1, 1 ).extract( 48, 16 ).getSignedValue() == 0xF000 );
  ufAssert( dval( ldbu2, 2 ).getSignedValue() == 50 );
  ufAssert( dval( ldbu2, 0 ).extract( 8, 24 ).containsOnlyBit( WIR_L4::b0 ) );
  ufAssert( dval( ldbu3, 1 ).getSignedValue() == 51 );

  ufAssert( dval( ldd1, 1 ).extract( 32, 16 ).getSignedValue() == 0x3C2A );
  ufAssert( dval( ldd1, 1 ).extract( 48, 16 ).getSignedValue() == 0xF000 );
  ufAssert( dval( ldd2, 2 ).getSignedValue() == 37 );

  ufAssert( dval( ldda1, 1 ).extract( 32, 16 ).getSignedValue() == 0xD22A );
  ufAssert( dval( ldda1, 1 ).extract( 48, 16 ).getSignedValue() == 0xF000 );
  ufAssert( dval( ldda2, 2 ).getSignedValue() == 37 );

  ufAssert( dval( ldh1, 1 ).extract( 32, 16 ).getSignedValue() == 0x5A2A );
  ufAssert( dval( ldh1, 1 ).extract( 48, 16 ).getSignedValue() == 0xF000 );
  ufAssert( dval( ldh2, 2 ).getSignedValue() == 46 );
  ufAssert( dval( ldh3, 1 ).getSignedValue() == 48 );

  ufAssert( dval( ldhu1, 1 ).extract( 32, 16 ).getSignedValue() == 0x962A );
  ufAssert( dval( ldhu1, 1 ).extract( 48, 16 ).getSignedValue() == 0xF000 );
  ufAssert( dval( ldhu2, 2 ).getSignedValue() == 8 );
  ufAssert( dval( ldhu2, 0 ).extract( 16, 16 ).containsOnlyBit( WIR_L4::b0 ) );

  ufAssert( dval( ldq1, 1 ).extract( 32, 16 ).getSignedValue() == 0x1E2A );
  ufAssert( dval( ldq1, 1 ).extract( 48, 16 ).getSignedValue() == 0xF000 );
  ufAssert( dval( ldq2, 2 ).getSignedValue() == 28 );
  ufAssert( dval( ldq2, 0 ).extract( 0, 16 ).containsOnlyBit( WIR_L4::b0 ) );

  ufAssert( dval( ldw1, 1 ).extract( 32, 16 ).getSignedValue() == 0xE12A );
  ufAssert( dval( ldw1, 1 ).extract( 48, 16 ).getSignedValue() == 0xF000 );
  ufAssert( dval( ldw2, 2 ).getSignedValue() == 38 );
  ufAssert( dval( ldw3, 1 ).getSignedValue() == 42 );

  ufAssert( dval( ldmst1, 0 ).extract( 32, 16 ).getSignedValue() == 0x692A );
  ufAssert( dval( ldmst1, 0 ).extract( 48, 16 ).getSignedValue() == 0xF000 );
  ufAssert( dval( ldmst1, 0 ).extract( 0, 32 ).getSignedValue() == 66 );
  ufAssert( dval( ldmst2, 1 ).getSignedValue() == 0x42 );

  ufAssert( dval( lea1, 0 ).getSignedValue() == 16383 );
  ufAssert( dval( lea2, 0 ).getSignedValue() == 42 );

  return( 0 );
}
