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
  // operations ABS & co.

  TC131 p;
  WIR_CompilationUnit c;
  c.setName( __FILE__ );
  WIR_Function &f = c.pushBackFunction( WIR_Function { "main" } );
  WIR_BasicBlock &b1 = f.pushBackBasicBlock( {} );
  const TC_DRegP &D4 = p.D4();
  auto &d0 = f.pushBackVirtualRegister( TC_DRegV() );
  auto &d1 = f.pushBackVirtualRegister( TC_DRegV() );
  auto &d2 = f.pushBackVirtualRegister( TC_DRegV() );
  auto &d3 = f.pushBackVirtualRegister( TC_DRegV() );
  auto &d4 = f.pushBackVirtualRegister( TC_DRegV() );
  auto &d5 = f.pushBackVirtualRegister( TC_DRegV() );
  auto &d6 = f.pushBackVirtualRegister( TC_DRegV() );
  auto &d7 = f.pushBackVirtualRegister( TC_DRegV() );
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
      new TC_Const16_Unsigned( 0x0F00 ) } );

  tcop(
    { TC131::OpCode::ADDI, TC131::OperationFormat::DDC16_1,
      new WIR_RegisterParameter( bitMask, WIR_Usage::def ),
      new WIR_RegisterParameter( bitMask, WIR_Usage::use ),
      new TC_Const16_Signed( 0x7FFF ) } );

  // ABS
  auto &abs1 = tcop(
    { TC131::OpCode::ABS, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( d3, WIR_Usage::def ),
      new WIR_RegisterParameter( d0, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( d1, WIR_Usage::def ),
      new TC_Const16_Signed( -123 ) } );

  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( d2, WIR_Usage::def ),
      new TC_Const16_Signed( 64 ) } );

  auto &abs2 = tcop(
    { TC131::OpCode::ABS, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( d3, WIR_Usage::def ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::AND, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d3, WIR_Usage::def ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( bitMask, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  auto &abs3 = tcop(
    { TC131::OpCode::ABS, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( d3, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::AND, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d3, WIR_Usage::def ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( bitMask, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  // ABS.B
  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDC5C5,
      new WIR_RegisterParameter( d4, WIR_Usage::def ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new WIR_RegisterParameter( d0, WIR_Usage::use ),
      new TC_Const5_Unsigned( 24 ),
      new TC_Const5_Unsigned( 8 ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDC5C5,
      new WIR_RegisterParameter( d4, WIR_Usage::def ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new TC_Const5_Unsigned( 16 ),
      new TC_Const5_Unsigned( 8 ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDC4C5C5,
      new WIR_RegisterParameter( d4, WIR_Usage::def ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const4_Unsigned( 7 ),
      new TC_Const5_Unsigned( 8 ),
      new TC_Const5_Unsigned( 8 ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDC5C5,
      new WIR_RegisterParameter( d4, WIR_Usage::def ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const5_Unsigned( 0 ),
      new TC_Const5_Unsigned( 8 ) } );

  auto &absb1 = tcop(
    { TC131::OpCode::ABS_B, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( d3, WIR_Usage::def ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( bitMask, WIR_Usage::def ),
      new TC_Const16_Signed( 0 ) } );

  tcop(
    { TC131::OpCode::AND, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d3, WIR_Usage::def ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( bitMask, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  // ABS.H
  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDC5C5,
      new WIR_RegisterParameter( d5, WIR_Usage::def ),
      new WIR_RegisterParameter( d5, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new TC_Const5_Unsigned( 16 ),
      new TC_Const5_Unsigned( 16 ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDC5C5,
      new WIR_RegisterParameter( d5, WIR_Usage::def ),
      new WIR_RegisterParameter( d5, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const5_Unsigned( 0 ),
      new TC_Const5_Unsigned( 16 ) } );

  auto &absh1 = tcop(
    { TC131::OpCode::ABS_H, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( d3, WIR_Usage::def ),
      new WIR_RegisterParameter( d5, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOVH, TC131::OperationFormat::DC16_2,
      new WIR_RegisterParameter( bitMask, WIR_Usage::def ),
      new TC_Const16_Unsigned( 0x0F00 ) } );

  tcop(
    { TC131::OpCode::ADDI, TC131::OperationFormat::DDC16_1,
      new WIR_RegisterParameter( bitMask, WIR_Usage::def ),
      new WIR_RegisterParameter( bitMask, WIR_Usage::use ),
      new TC_Const16_Signed( 0x7FFF ) } );

  tcop(
    { TC131::OpCode::AND, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d3, WIR_Usage::def ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ),
      new WIR_RegisterParameter( bitMask, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  // ABSDIF
  auto &absdif1 = tcop(
    { TC131::OpCode::ABSDIF, TC131::OperationFormat::DDC9_1,
      new WIR_RegisterParameter( d3, WIR_Usage::def ),
      new WIR_RegisterParameter( d0, WIR_Usage::use ),
      new TC_Const9_Signed( -120 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  auto &absdif2 = tcop(
    { TC131::OpCode::ABSDIF, TC131::OperationFormat::DDC9_1,
      new WIR_RegisterParameter( d3, WIR_Usage::def ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new TC_Const9_Signed( -120 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  auto &absdif3 = tcop(
    { TC131::OpCode::ABSDIF, TC131::OperationFormat::DDC9_1,
      new WIR_RegisterParameter( d3, WIR_Usage::def ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ),
      new TC_Const9_Signed( -120 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  auto &absdif4 = tcop(
    { TC131::OpCode::ABSDIF, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d3, WIR_Usage::def ),
      new WIR_RegisterParameter( d7, WIR_Usage::use ),
      new WIR_RegisterParameter( d7, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  // ABSDIF.B
  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDC4C5C5,
      new WIR_RegisterParameter( d4, WIR_Usage::def ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const4_Unsigned( 7 ),
      new TC_Const5_Unsigned( 0 ),
      new TC_Const5_Unsigned( 8 ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDC5C5,
      new WIR_RegisterParameter( d4, WIR_Usage::def ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new TC_Const5_Unsigned( 8 ),
      new TC_Const5_Unsigned( 8 ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDC5C5,
      new WIR_RegisterParameter( d4, WIR_Usage::def ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new WIR_RegisterParameter( d0, WIR_Usage::use ),
      new TC_Const5_Unsigned( 16 ),
      new TC_Const5_Unsigned( 8 ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDC4C5C5,
      new WIR_RegisterParameter( d5, WIR_Usage::def ),
      new WIR_RegisterParameter( d5, WIR_Usage::use ),
      new TC_Const4_Unsigned( 2 ),
      new TC_Const5_Unsigned( 0 ),
      new TC_Const5_Unsigned( 8 ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDC4C5C5,
      new WIR_RegisterParameter( d5, WIR_Usage::def ),
      new WIR_RegisterParameter( d5, WIR_Usage::use ),
      new TC_Const4_Unsigned( 3 ),
      new TC_Const5_Unsigned( 8 ),
      new TC_Const5_Unsigned( 8 ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDC4C5C5,
      new WIR_RegisterParameter( d5, WIR_Usage::def ),
      new WIR_RegisterParameter( d5, WIR_Usage::use ),
      new TC_Const4_Unsigned( 5 ),
      new TC_Const5_Unsigned( 16 ),
      new TC_Const5_Unsigned( 8 ) } );

  auto &absdifb1 = tcop(
    { TC131::OpCode::ABSDIF_B, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d3, WIR_Usage::def ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new WIR_RegisterParameter( d5, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  auto &absdifb2 = tcop(
    { TC131::OpCode::ABSDIF_B, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d3, WIR_Usage::def ),
      new WIR_RegisterParameter( d7, WIR_Usage::use ),
      new WIR_RegisterParameter( d7, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  // ABSDIF.H
  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDDC5C5,
      new WIR_RegisterParameter( d4, WIR_Usage::def ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new TC_Const5_Unsigned( 16 ),
      new TC_Const5_Unsigned( 16 ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDC4C5C5,
      new WIR_RegisterParameter( d4, WIR_Usage::def ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new TC_Const4_Unsigned( 7 ),
      new TC_Const5_Unsigned( 0 ),
      new TC_Const5_Unsigned( 16 ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDC4C5C5,
      new WIR_RegisterParameter( d5, WIR_Usage::def ),
      new WIR_RegisterParameter( d5, WIR_Usage::use ),
      new TC_Const4_Unsigned( 3 ),
      new TC_Const5_Unsigned( 16 ),
      new TC_Const5_Unsigned( 16 ) } );

  tcop(
    { TC131::OpCode::INSERT, TC131::OperationFormat::DDC4C5C5,
      new WIR_RegisterParameter( d5, WIR_Usage::def ),
      new WIR_RegisterParameter( d5, WIR_Usage::use ),
      new TC_Const4_Unsigned( 2 ),
      new TC_Const5_Unsigned( 0 ),
      new TC_Const5_Unsigned( 16 ) } );

  auto &absdifh1 = tcop(
    { TC131::OpCode::ABSDIF_H, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d3, WIR_Usage::def ),
      new WIR_RegisterParameter( d4, WIR_Usage::use ),
      new WIR_RegisterParameter( d5, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  auto &absdifh2 = tcop(
    { TC131::OpCode::ABSDIF_H, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d3, WIR_Usage::def ),
      new WIR_RegisterParameter( d7, WIR_Usage::use ),
      new WIR_RegisterParameter( d7, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  // ABSDIFS
  tcop(
    { TC131::OpCode::MOVH, TC131::OperationFormat::DC16_2,
      new WIR_RegisterParameter( d6, WIR_Usage::def ),
      new TC_Const16_Unsigned( 32768 ) } );

  auto &absdifs1 = tcop(
    { TC131::OpCode::ABSDIFS, TC131::OperationFormat::DDC9_1,
      new WIR_RegisterParameter( d3, WIR_Usage::def ),
      new WIR_RegisterParameter( d6, WIR_Usage::use ),
      new TC_Const9_Signed( 0 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  auto &absdifs2 = tcop(
    { TC131::OpCode::ABSDIFS, TC131::OperationFormat::DDC9_1,
      new WIR_RegisterParameter( d3, WIR_Usage::def ),
      new WIR_RegisterParameter( d0, WIR_Usage::use ),
      new TC_Const9_Signed( 0 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  auto &absdifs3 = tcop(
    { TC131::OpCode::ABSDIFS, TC131::OperationFormat::DDC9_1,
      new WIR_RegisterParameter( d3, WIR_Usage::def ),
      new WIR_RegisterParameter( d1, WIR_Usage::use ),
      new TC_Const9_Signed( 0 ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  auto &absdifs4 = tcop(
    { TC131::OpCode::ABSDIFS, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d3, WIR_Usage::def ),
      new WIR_RegisterParameter( d7, WIR_Usage::use ),
      new WIR_RegisterParameter( d7, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  // ABSDIFS.H
  auto &absdifsh1 = tcop(
    { TC131::OpCode::ABSDIFS_H, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d3, WIR_Usage::def ),
      new WIR_RegisterParameter( d6, WIR_Usage::use ),
      new WIR_RegisterParameter( d2, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  auto &absdifsh2 = tcop(
    { TC131::OpCode::ABSDIFS_H, TC131::OperationFormat::DDD_1,
      new WIR_RegisterParameter( d3, WIR_Usage::def ),
      new WIR_RegisterParameter( d7, WIR_Usage::use ),
      new WIR_RegisterParameter( d7, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  // ABSS
  auto &abss1 = tcop(
    { TC131::OpCode::ABSS, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( d3, WIR_Usage::def ),
      new WIR_RegisterParameter( d6, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );

  // ABSS.H
  auto &abssh1 = tcop(
    { TC131::OpCode::ABSS_H, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( d3, WIR_Usage::def ),
      new WIR_RegisterParameter( d6, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( d3, WIR_Usage::use ) } );


  // Do bit-true data flow analysis.
  TC_BitDFA a { f };
  a.analyze();

  // Evaluate analysis results.
  ufAssert( dval( abs1, 0 ).containsOnlyBit( WIR_L4::bL ) );

  ufAssert( dval( abs2, 0 ).getSignedValue() == 123 );
  ufAssert( uval( abs2, 1 ).isBinaryInteger() );

  ufAssert( dval( abs3, 0 ).getSignedValue() == 64 );
  ufAssert( uval( abs3, 1 ).extract( 28, 4 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( abs3, 1 ).extract( 24, 4 ).containsOnlyBit( WIR_L4::b0 ) );
  ufAssert( uval( abs3, 1 ).extract( 15, 9 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( abs3, 1 ).extract( 0, 15 ).isBinaryInteger() );

  ufAssert( dval( absb1, 0 ).extract( 24, 8 ).containsOnlyBit( WIR_L4::bL ) );
  ufAssert( dval( absb1, 0 ).extract( 16, 8 ).getSignedValue() == 123 );
  ufAssert( dval( absb1, 0 ).extract( 8, 8 ).getSignedValue() == 7 );
  ufAssert( dval( absb1, 0 ).extract( 0, 8 ).getSignedValue() == 64 );
  ufAssert( uval( absb1, 1 ).extract( 0, 16 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( absb1, 1 ).extract( 16, 8 ).isBinaryInteger() );
  ufAssert( uval( absb1, 1 ).extract( 24, 8 ).containsOnlyBit( WIR_L4::bL ) );

  ufAssert( dval( absh1, 0 ).extract( 16, 16 ).getSignedValue() == 123 );
  ufAssert( dval( absh1, 0 ).extract( 0, 16 ).getSignedValue() == 64 );
  ufAssert( uval( absh1, 1 ).extract( 16, 6 ).isBinaryInteger() );
  ufAssert( uval( absh1, 1 ).at( 15 ) == WIR_L4::bX );
  ufAssert( uval( absh1, 1 ).extract( 0, 15 ).isBinaryInteger() );

  ufAssert( dval( absdif1, 0 ).containsOnlyBit( WIR_L4::bL ) );
  ufAssert( dval( absdif2, 0 ).getSignedValue() == 3 );
  ufAssert( dval( absdif3, 0 ).getSignedValue() == 184 );
  ufAssert( dval( absdif4, 0 ).getSignedValue() == 0 );

  ufAssert( dval( absdifb1, 0 ).extract( 24, 8 ).containsOnlyBit( WIR_L4::bL ) );
  ufAssert( dval( absdifb1, 0 ).extract( 16, 8 ).containsOnlyBit( WIR_L4::bL ) );
  ufAssert( dval( absdifb1, 0 ).extract( 8, 8 ).getSignedValue() == 126 );
  ufAssert( dval( absdifb1, 0 ).extract( 0, 8 ).getSignedValue() == 5 );
  ufAssert( dval( absdifb2, 0 ).getSignedValue() == 0 );

  ufAssert( dval( absdifh1, 0 ).extract( 16, 16 ).getSignedValue() == 126 );
  ufAssert( dval( absdifh1, 0 ).extract( 0, 16 ).getSignedValue() == 5 );
  ufAssert( dval( absdifh2, 0 ).getSignedValue() == 0 );

  ufAssert( dval( absdifs1, 0 ).getSignedValue() == 2147483647 );
  ufAssert( dval( absdifs2, 0 ).containsOnlyBit( WIR_L4::bL ) );
  ufAssert( dval( absdifs3, 0 ).getSignedValue() == 123 );
  ufAssert( dval( absdifs4, 0 ).getSignedValue() == 0 );

  ufAssert( dval( absdifsh1, 0 ).extract( 16, 16 ).getSignedValue() == 32767 );
  ufAssert( dval( absdifsh1, 0 ).extract( 0, 16 ).getSignedValue() == 64 );
  ufAssert( dval( absdifsh2, 0 ).getSignedValue() == 0 );

  ufAssert( dval( abss1, 0 ).getSignedValue() == 2147483647 );

  ufAssert( dval( abssh1, 0 ).extract( 16, 16 ).getSignedValue() == 32767 );
  ufAssert( dval( abssh1, 0 ).extract( 0, 16 ).getSignedValue() == 0 );

  return( 0 );
}
