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
  // operations SAT* & co.

  TC131 p;
  WIR_CompilationUnit c;
  c.setName( __FILE__ );
  WIR_Function &f = c.pushBackFunction( WIR_Function { "main" } );
  WIR_BasicBlock &b1 = f.pushBackBasicBlock( {} );
  const TC_DRegP &D4 = p.D4();
  auto &dMi123 = f.pushBackVirtualRegister( TC_DRegV() );
  auto &dMi40k = f.pushBackVirtualRegister( TC_DRegV() );
  auto &dPl64 = f.pushBackVirtualRegister( TC_DRegV() );
  auto &dPl40k = f.pushBackVirtualRegister( TC_DRegV() );
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

  tcop(
    { TC131::OpCode::MOV_U, TC131::OperationFormat::DC16_2,
      new WIR_RegisterParameter( dPl40k, WIR_Usage::def ),
      new TC_Const16_Unsigned( 40000 ) } );

  tcop(
    { TC131::OpCode::RSUB, TC131::OperationFormat::DDC9_1,
      new WIR_RegisterParameter( dMi40k, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl40k, WIR_Usage::use ),
      new TC_Const9_Signed( 0 ) } );

  // SAT.B
  auto &satb1 = tcop(
    { TC131::OpCode::SAT_B, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &satb2 = tcop(
    { TC131::OpCode::SAT_B, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi40k, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &satb3 = tcop(
    { TC131::OpCode::SAT_B, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &satb4 = tcop(
    { TC131::OpCode::SAT_B, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl40k, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // SAT.BU
  auto &satbu1 = tcop(
    { TC131::OpCode::SAT_BU, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &satbu2 = tcop(
    { TC131::OpCode::SAT_BU, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi40k, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &satbu3 = tcop(
    { TC131::OpCode::SAT_BU, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &satbu4 = tcop(
    { TC131::OpCode::SAT_BU, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl40k, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // SAT.H
  auto &sath1 = tcop(
    { TC131::OpCode::SAT_H, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &sath2 = tcop(
    { TC131::OpCode::SAT_H, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi40k, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &sath3 = tcop(
    { TC131::OpCode::SAT_H, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &sath4 = tcop(
    { TC131::OpCode::SAT_H, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl40k, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // SAT.HU
  auto &sathu1 = tcop(
    { TC131::OpCode::SAT_HU, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi123, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &sathu2 = tcop(
    { TC131::OpCode::SAT_HU, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi40k, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &sathu3 = tcop(
    { TC131::OpCode::SAT_HU, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl64, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  auto &sathu4 = tcop(
    { TC131::OpCode::SAT_HU, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl40k, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  // Do bit-true data flow analysis.
  TC_BitDFA a { f };
  a.analyze();

  // Evaluate analysis results.
  ufAssert( dval( satb1, 0 ).getSignedValue() == -123 );
  ufAssert( dval( satb2, 0 ).getSignedValue() == -128 );
  ufAssert( dval( satb3, 0 ).getSignedValue() == 64 );
  ufAssert( dval( satb4, 0 ).getSignedValue() == 127 );

  ufAssert( dval( satbu1, 0 ).getSignedValue() == 255 );
  ufAssert( dval( satbu2, 0 ).getSignedValue() == 255 );
  ufAssert( dval( satbu3, 0 ).getSignedValue() == 64 );
  ufAssert( dval( satbu4, 0 ).getSignedValue() == 255 );

  ufAssert( dval( sath1, 0 ).getSignedValue() == -123 );
  ufAssert( dval( sath2, 0 ).getSignedValue() == -32768 );
  ufAssert( dval( sath3, 0 ).getSignedValue() == 64 );
  ufAssert( dval( sath4, 0 ).getSignedValue() == 32767 );

  ufAssert( dval( sathu1, 0 ).getSignedValue() == 65535 );
  ufAssert( dval( sathu2, 0 ).getSignedValue() == 65535 );
  ufAssert( dval( sathu3, 0 ).getSignedValue() == 64 );
  ufAssert( dval( sathu4, 0 ).getSignedValue() == 40000 );

  return( 0 );
}
