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
  // operations IXMAX* & co.

  TC131 p;
  WIR_CompilationUnit c;
  c.setName( __FILE__ );
  WIR_Function &f = c.pushBackFunction( WIR_Function { "main" } );
  WIR_BasicBlock &b1 = f.pushBackBasicBlock( {} );
  const TC_DRegP &D4 = p.D4();
  const TC_DRegP &D5 = p.D5();
  auto &tmp0 = f.pushBackVirtualRegister( TC_DRegV() );
  auto &tmp1 = f.pushBackVirtualRegister( TC_ERegV() );
  auto &tmp2 = f.pushBackVirtualRegister( TC_ERegV() );

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

  // IXMAX
  auto &ixmax1 = tcop(
    { TC131::OpCode::IXMAX, TC131::OperationFormat::EED,
      new WIR_RegisterParameter( tmp2, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp2.begin()->get(), WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D5, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp2.rbegin()->get(), WIR_Usage::use ) } );

  // IXMAX.U
  auto &ixmaxu1 = tcop(
    { TC131::OpCode::IXMAX_U, TC131::OperationFormat::EED,
      new WIR_RegisterParameter( tmp2, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp2.begin()->get(), WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D5, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp2.rbegin()->get(), WIR_Usage::use ) } );

  // IXMIN
  auto &ixmin1 = tcop(
    { TC131::OpCode::IXMIN, TC131::OperationFormat::EED,
      new WIR_RegisterParameter( tmp2, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp2.begin()->get(), WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D5, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp2.rbegin()->get(), WIR_Usage::use ) } );

  // IXMIN.U
  auto &ixminu1 = tcop(
    { TC131::OpCode::IXMIN_U, TC131::OperationFormat::EED,
      new WIR_RegisterParameter( tmp2, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp1, WIR_Usage::use ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp2.begin()->get(), WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D5, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp2.rbegin()->get(), WIR_Usage::use ) } );

  // Do bit-true data flow analysis.
  TC_BitDFA a { f };
  a.analyze();

  // Evaluate analysis results.
  ufAssert( dval( ixmax1, 0 ).extract( 0, 48 ).containsOnlyBit( WIR_L4::bL ) );
  ufAssert( dval( ixmax1, 0 ).extract( 48, 16 ).containsOnlyBit( WIR_L4::b0 ) );
  ufAssert( uval( ixmax1, 1 ).extract( 0, 48 ).containsOnlyBit( WIR_L4::bL ) );
  ufAssert( uval( ixmax1, 1 ).extract( 48, 16 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( ixmaxu1, 0 ).extract( 0, 48 ).containsOnlyBit( WIR_L4::bL ) );
  ufAssert(
    dval( ixmaxu1, 0 ).extract( 48, 16 ).containsOnlyBit( WIR_L4::b0 ) );
  ufAssert( uval( ixmaxu1, 1 ).extract( 0, 48 ).containsOnlyBit( WIR_L4::bL ) );
  ufAssert(
    uval( ixmaxu1, 1 ).extract( 48, 16 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( ixmin1, 0 ).extract( 0, 48 ).containsOnlyBit( WIR_L4::bL ) );
  ufAssert( dval( ixmin1, 0 ).extract( 48, 16 ).containsOnlyBit( WIR_L4::b0 ) );
  ufAssert( uval( ixmin1, 1 ).extract( 0, 48 ).containsOnlyBit( WIR_L4::bL ) );
  ufAssert( uval( ixmin1, 1 ).extract( 48, 16 ).containsOnlyBit( WIR_L4::bX ) );

  ufAssert( dval( ixminu1, 0 ).extract( 0, 48 ).containsOnlyBit( WIR_L4::bL ) );
  ufAssert(
    dval( ixminu1, 0 ).extract( 48, 16 ).containsOnlyBit( WIR_L4::b0 ) );
  ufAssert( uval( ixminu1, 1 ).extract( 0, 48 ).containsOnlyBit( WIR_L4::bL ) );
  ufAssert(
    uval( ixminu1, 1 ).extract( 48, 16 ).containsOnlyBit( WIR_L4::bX ) );

  return( 0 );
}
