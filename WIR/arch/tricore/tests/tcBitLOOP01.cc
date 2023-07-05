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
  // operation LOOP.

  TC131 p;
  WIR_CompilationUnit c;
  c.setName( __FILE__ );
  WIR_Function &f = c.pushBackFunction( WIR_Function { "main" } );
  WIR_BasicBlock &b1 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &b2 = f.pushBackBasicBlock( {} );
  const TC_ARegP &A4 = p.A4();
  auto &a42 = f.pushBackVirtualRegister( TC_ARegV() );

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
    auto &rp = dynamic_cast<WIR_RegisterParameter &>( it->get() );
    auto &c = rp.getContainers<WIR_BitValues>().begin()->get();
    return( c.getOutValues().begin()->downVal );
  };

  // Create WIR code.

  tcop(
    { TC131::OpCode::LEA, TC131::OperationFormat::AC18ABSA,
      new WIR_RegisterParameter( a42, WIR_Usage::def ),
      new TC_Const18_Unsigned( 42 ) }, b1 );

  // LOOP
  auto &loop1 = tcop(
    { TC131::OpCode::LOOP, TC131::OperationFormat::AL_3,
      new WIR_RegisterParameter( a42, WIR_Usage::defuse ),
      new WIR_LabelParameter( b1 ) }, b1 );

  tcop(
    { TC131::OpCode::MOV_AA, TC131::OperationFormat::SAA_1,
      new WIR_RegisterParameter( A4, WIR_Usage::def ),
      new WIR_RegisterParameter( a42, WIR_Usage::use ) }, b2 );

  // Do bit-true data flow analysis.
  TC_BitDFA a { f };
  a.analyze();

  // Evaluate analysis results.
  ufAssert( dval( loop1, 0 ).getSignedValue() == 41 );

  return( 0 );
}
