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
  // operation JNE* & co.

  TC131 p;
  WIR_CompilationUnit c;
  c.setName( __FILE__ );
  WIR_Function &f = c.pushBackFunction( WIR_Function { "main" } );
  WIR_BasicBlock &b1 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b2 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b3 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b4 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b5 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b6 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b7 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b8 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b9 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b10 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b11 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b12 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b13 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b14 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b15 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b = f.pushBackBasicBlock( {} );
  const TC_DRegP &D4 = p.D4();
  auto &dMi123 = f.pushBackVirtualRegister( TC_DRegV() );
  auto &dPl64 = f.pushBackVirtualRegister( TC_DRegV() );
  auto &d0 = f.pushBackVirtualRegister( TC_DRegV() );
  auto &dExt1 = f.pushBackVirtualRegister( TC_DRegV() );
  auto &dExt2 = f.pushBackVirtualRegister( TC_DRegV() );
  auto &aExt1 = f.pushBackVirtualRegister( TC_ARegV() );

  // This lambda serves for generating and inserting a TriCore operation.
  auto tcop = [&]( WIR_Operation &&o, WIR_BasicBlock &bb ) -> WIR_Operation & {
    auto &i = bb.pushBackInstruction( WIR_Instruction { WIR_Operation { o } } );
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
      new TC_Const16_Signed( -123 ) }, b1 );

  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( dPl64, WIR_Usage::def ),
      new TC_Const16_Signed( 64 ) }, b1 );

  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( d0, WIR_Usage::def ),
      new TC_Const16_Signed( 0 ) }, b1 );

  // JEQ
  auto &jeq1 = tcop(
    { TC131::OpCode::JEQ, TC131::OperationFormat::DDL_1,
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new WIR_RegisterParameter( d0, WIR_Usage::use ),
      new WIR_LabelParameter( b ) }, b1 );

  auto &jeq2 = tcop(
    { TC131::OpCode::JEQ, TC131::OperationFormat::DDL_1,
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new WIR_LabelParameter( b ) }, b2 );

  // JGEZ
  auto &jgez1 = tcop(
    { TC131::OpCode::JGEZ, TC131::OperationFormat::SDL,
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ),
      new WIR_LabelParameter( b ) }, b3 );

  // JNE
  auto &jne1 = tcop(
    { TC131::OpCode::JNE, TC131::OperationFormat::DDL_1,
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new WIR_RegisterParameter( d0, WIR_Usage::use ),
      new WIR_LabelParameter( b ) }, b4 );

  auto &jne2 = tcop(
    { TC131::OpCode::JNE, TC131::OperationFormat::DDL_1,
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new WIR_LabelParameter( b ) }, b5 );

  // JNED
  auto &jned1 = tcop(
    { TC131::OpCode::JNED, TC131::OperationFormat::DC4L_3,
      new WIR_RegisterParameter( dPl64, WIR_Usage::defuse ),
      new TC_Const4_Signed( 3 ),
      new WIR_LabelParameter( b ) }, b6 );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ) }, b7 );

  auto &jned2 = tcop(
    { TC131::OpCode::JNED, TC131::OperationFormat::DC4L_3,
      new WIR_RegisterParameter( dMi123, WIR_Usage::defuse ),
      new TC_Const4_Signed( 3 ),
      new WIR_LabelParameter( b ) }, b7 );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ) }, b8 );

  auto &jned3 = tcop(
    { TC131::OpCode::JNED, TC131::OperationFormat::DC4L_3,
      new WIR_RegisterParameter( dExt1, WIR_Usage::defuse ),
      new TC_Const4_Signed( 3 ),
      new WIR_LabelParameter( b ) }, b8 );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( dExt1, WIR_Usage::use ) }, b9 );

  // JNEI
  auto &jnei1 = tcop(
    { TC131::OpCode::JNEI, TC131::OperationFormat::DC4L_3,
      new WIR_RegisterParameter( dPl64, WIR_Usage::defuse ),
      new TC_Const4_Signed( 3 ),
      new WIR_LabelParameter( b ) }, b9 );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ) }, b10 );

  auto &jnei2 = tcop(
    { TC131::OpCode::JNEI, TC131::OperationFormat::DC4L_3,
      new WIR_RegisterParameter( dMi123, WIR_Usage::defuse ),
      new TC_Const4_Signed( 3 ),
      new WIR_LabelParameter( b ) }, b10 );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ) }, b11 );

  auto &jnei3 = tcop(
    { TC131::OpCode::JNEI, TC131::OperationFormat::DC4L_3,
      new WIR_RegisterParameter( dExt2, WIR_Usage::defuse ),
      new TC_Const4_Signed( 3 ),
      new WIR_LabelParameter( b ) }, b11 );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( dExt2, WIR_Usage::use ) }, b12 );

  // JNZ
  auto &jnz1 = tcop(
    { TC131::OpCode::JNZ, TC131::OperationFormat::SDL,
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ),
      new WIR_LabelParameter( b ) }, b13 );

  auto &jnz2 = tcop(
    { TC131::OpCode::JNZ, TC131::OperationFormat::SDL,
      new WIR_RegisterParameter( d0, WIR_Usage::use ),
      new WIR_LabelParameter( b ) }, b14 );

  // JI
  auto &ji1 = tcop(
    { TC131::OpCode::JI, TC131::OperationFormat::A,
      new WIR_RegisterParameter( aExt1, WIR_Usage::use ) }, b15 );

  // Do bit-true data flow analysis.
  TC_BitDFA a { f };
  a.analyze();

  // Evaluate analysis results.
  ufAssert( uval( jeq1, 0 ).extract( 0, 6 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( jeq1, 0 ).at( 6 ) == WIR_L4::b1 );
  ufAssert( uval( jeq1, 0 ).extract( 7, 25 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( jeq1, 1 ).extract( 0, 6 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( jeq1, 1 ).at( 6 ) == WIR_L4::b0 );
  ufAssert( uval( jeq1, 1 ).extract( 7, 25 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( uval( jeq2, 0 ).isBinaryInteger() );
  ufAssert( uval( jeq2, 1 ).isBinaryInteger() );

  ufAssert( uval( jgez1, 0 ).extract( 0, 31 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( jgez1, 0 ).at( 31 ) == WIR_L4::b1 );

  ufAssert( uval( jne1, 0 ).extract( 0, 6 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( jne1, 0 ).at( 6 ) == WIR_L4::b1 );
  ufAssert( uval( jne1, 0 ).extract( 7, 25 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( jne1, 1 ).extract( 0, 6 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( jne1, 1 ).at( 6 ) == WIR_L4::b0 );
  ufAssert( uval( jne1, 1 ).extract( 7, 25 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( uval( jne2, 0 ).isBinaryInteger() );
  ufAssert( uval( jne2, 1 ).isBinaryInteger() );

  ufAssert( dval( jned1, 0 ).getSignedValue() == 63 );
  ufAssert( uval( jned1, 0 ).at( 0 ) == WIR_L4::b0 );
  ufAssert( uval( jned1, 0 ).extract( 1, 31 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( jned1, 1 ).at( 0 ) == WIR_L4::b1 );
  ufAssert( uval( jned1, 1 ).extract( 1, 3 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( jned2, 0 ).getSignedValue() == -124 );
  ufAssert( dval( jned3, 0 ).at( 0 ) == WIR_L4::bN );
  ufAssert( dval( jned3, 0 ).extract( 1, 31 ).containsOnlyBit( WIR_L4::bL ) );

  ufAssert( dval( jnei1, 0 ).getSignedValue() == 64 );
  ufAssert( dval( jnei2, 0 ).getSignedValue() == -123 );
  ufAssert( dval( jnei3, 0 ).at( 0 ) == WIR_L4::bN );
  ufAssert( dval( jnei3, 0 ).extract( 1, 31 ).containsOnlyBit( WIR_L4::bL ) );

  ufAssert( uval( jnz1, 0 ).extract( 0, 6 ).containsOnlyBit( WIR_L4::bX ) );
  ufAssert( uval( jnz1, 0 ).at( 6 ) == WIR_L4::b1 );
  ufAssert( uval( jnz1, 0 ).extract( 7, 25 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( uval( jnz2, 0 ).isBinaryInteger() );

  ufAssert( uval( ji1, 0 ).at( 0 ) == WIR_L4::bX );
  ufAssert( uval( ji1, 0 ).extract( 1, 31 ).containsOnlyBit( WIR_L4::bL ) );

  return( 0 );
}
