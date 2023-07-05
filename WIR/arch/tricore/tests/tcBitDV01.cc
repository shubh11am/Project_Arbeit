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
  // operations DV* & co.

  TC131 p;
  WIR_CompilationUnit c;
  c.setName( __FILE__ );
  WIR_Function &f = c.pushBackFunction( WIR_Function { "main" } );
  WIR_BasicBlock &b1 = f.pushBackBasicBlock( {} );
  const TC_DRegP &D4 = p.D4();
  auto &dPl118 = f.pushBackVirtualRegister( TC_DRegV() );
  auto &dPl42 = f.pushBackVirtualRegister( TC_DRegV() );
  auto &dMi118 = f.pushBackVirtualRegister( TC_DRegV() );
  auto &dMi42 = f.pushBackVirtualRegister( TC_DRegV() );
  auto &tmp0 = f.pushBackVirtualRegister( TC_ERegV() );

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
      new WIR_RegisterParameter( dPl118, WIR_Usage::def ),
      new TC_Const16_Signed( 118 ) } );

  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( dPl42, WIR_Usage::def ),
      new TC_Const16_Signed( 42 ) } );

  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( dMi118, WIR_Usage::def ),
      new TC_Const16_Signed( -118 ) } );

  tcop(
    { TC131::OpCode::MOV, TC131::OperationFormat::DC16_1,
      new WIR_RegisterParameter( dMi42, WIR_Usage::def ),
      new TC_Const16_Signed( -42 ) } );

  // DVINIT.BU; DVSTEP.U
  tcop(
    { TC131::OpCode::DVINIT_BU, TC131::OperationFormat::EDD,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl118, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl42, WIR_Usage::use ) } );

  auto &dvbu1 = tcop(
    { TC131::OpCode::DVSTEP_U, TC131::OperationFormat::EED,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl42, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0.begin()->get(), WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0.rbegin()->get(), WIR_Usage::use ) } );

  // DVINIT.B; DVSTEP; DVADJ
  auto &dvinitb1 = tcop(
    { TC131::OpCode::DVINIT_B, TC131::OperationFormat::EDD,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl118, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl42, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::DVSTEP, TC131::OperationFormat::EED,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl42, WIR_Usage::use ) } );

  auto &dvb1 = tcop(
    { TC131::OpCode::DVADJ, TC131::OperationFormat::EED,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl42, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0.begin()->get(), WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0.rbegin()->get(), WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::DVINIT_B, TC131::OperationFormat::EDD,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi118, WIR_Usage::use ),
      new WIR_RegisterParameter( dMi42, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::DVSTEP, TC131::OperationFormat::EED,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( dMi42, WIR_Usage::use ) } );

  auto &dvb2 = tcop(
    { TC131::OpCode::DVADJ, TC131::OperationFormat::EED,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( dMi42, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0.begin()->get(), WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0.rbegin()->get(), WIR_Usage::use ) } );

  // DVINIT.HU; DVSTEP.U; DVSTEP.U
  tcop(
    { TC131::OpCode::DVINIT_HU, TC131::OperationFormat::EDD,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl118, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl42, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::DVSTEP_U, TC131::OperationFormat::EED,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl42, WIR_Usage::use ) } );

  auto &dvhu1 = tcop(
    { TC131::OpCode::DVSTEP_U, TC131::OperationFormat::EED,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl42, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0.begin()->get(), WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0.rbegin()->get(), WIR_Usage::use ) } );

  // DVINIT.H; DVSTEP; DVSTEP; DVADJ
  auto &dvinith1 = tcop(
    { TC131::OpCode::DVINIT_H, TC131::OperationFormat::EDD,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl118, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl42, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::DVSTEP, TC131::OperationFormat::EED,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl42, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::DVSTEP, TC131::OperationFormat::EED,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl42, WIR_Usage::use ) } );

  auto &dvh1 = tcop(
    { TC131::OpCode::DVADJ, TC131::OperationFormat::EED,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl42, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0.begin()->get(), WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0.rbegin()->get(), WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::DVINIT_H, TC131::OperationFormat::EDD,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi118, WIR_Usage::use ),
      new WIR_RegisterParameter( dMi42, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::DVSTEP, TC131::OperationFormat::EED,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( dMi42, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::DVSTEP, TC131::OperationFormat::EED,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( dMi42, WIR_Usage::use ) } );

  auto &dvh2 = tcop(
    { TC131::OpCode::DVADJ, TC131::OperationFormat::EED,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( dMi42, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0.begin()->get(), WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0.rbegin()->get(), WIR_Usage::use ) } );

  // DVINIT.U; DVSTEP.U; DVSTEP.U; DVSTEP.U; DVSTEP.U
  tcop(
    { TC131::OpCode::DVINIT_U, TC131::OperationFormat::EDD,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl118, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl42, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::DVSTEP_U, TC131::OperationFormat::EED,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl42, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::DVSTEP_U, TC131::OperationFormat::EED,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl42, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::DVSTEP_U, TC131::OperationFormat::EED,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl42, WIR_Usage::use ) } );

  auto &dvu1 = tcop(
    { TC131::OpCode::DVSTEP_U, TC131::OperationFormat::EED,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl42, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0.begin()->get(), WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0.rbegin()->get(), WIR_Usage::use ) } );

  // DVINIT; DVSTEP; DVSTEP; DVSTEP; DVSTEP; DVADJ
  tcop(
    { TC131::OpCode::DVINIT, TC131::OperationFormat::EDD,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dPl118, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl42, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::DVSTEP, TC131::OperationFormat::EED,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl42, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::DVSTEP, TC131::OperationFormat::EED,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl42, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::DVSTEP, TC131::OperationFormat::EED,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl42, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::DVSTEP, TC131::OperationFormat::EED,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl42, WIR_Usage::use ) } );

  auto &dv1 = tcop(
    { TC131::OpCode::DVADJ, TC131::OperationFormat::EED,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( dPl42, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0.begin()->get(), WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0.rbegin()->get(), WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::DVINIT, TC131::OperationFormat::EDD,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( dMi118, WIR_Usage::use ),
      new WIR_RegisterParameter( dMi42, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::DVSTEP, TC131::OperationFormat::EED,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( dMi42, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::DVSTEP, TC131::OperationFormat::EED,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( dMi42, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::DVSTEP, TC131::OperationFormat::EED,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( dMi42, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::DVSTEP, TC131::OperationFormat::EED,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( dMi42, WIR_Usage::use ) } );

  auto &dv2 = tcop(
    { TC131::OpCode::DVADJ, TC131::OperationFormat::EED,
      new WIR_RegisterParameter( tmp0, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0, WIR_Usage::use ),
      new WIR_RegisterParameter( dMi42, WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0.begin()->get(), WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_RR, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( D4, WIR_Usage::def ),
      new WIR_RegisterParameter( tmp0.rbegin()->get(), WIR_Usage::use ) } );

  // Do bit-true data flow analysis.
  TC_BitDFA a { f };
  a.analyze();

  // Evaluate analysis results.
  ufAssert( dval( dvbu1, 0 ).extract( 32, 32 ).getSignedValue() == 34 );
  ufAssert( dval( dvbu1, 0 ).extract( 0, 32 ).getSignedValue() == 2 );

  ufAssert( dval( dvb1, 0 ).extract( 32, 32 ).getSignedValue() == 34 );
  ufAssert( dval( dvb1, 0 ).extract( 0, 32 ).getSignedValue() == 2 );

  ufAssert( dval( dvb2, 0 ).extract( 32, 32 ).getSignedValue() == 0xFFFFFFDE );
  ufAssert( dval( dvb2, 0 ).extract( 0, 32 ).getSignedValue() == 2 );

  ufAssert( dval( dvhu1, 0 ).extract( 32, 32 ).getSignedValue() == 34 );
  ufAssert( dval( dvhu1, 0 ).extract( 0, 32 ).getSignedValue() == 2 );

  ufAssert( dval( dvh1, 0 ).extract( 32, 32 ).getSignedValue() == 34 );
  ufAssert( dval( dvh1, 0 ).extract( 0, 32 ).getSignedValue() == 2 );

  ufAssert( dval( dvh2, 0 ).extract( 32, 32 ).getSignedValue() == 0xFFFFFFDE );
  ufAssert( dval( dvh2, 0 ).extract( 0, 32 ).getSignedValue() == 2 );

  ufAssert( dval( dvu1, 0 ).extract( 32, 32 ).getSignedValue() == 34 );
  ufAssert( dval( dvu1, 0 ).extract( 0, 32 ).getSignedValue() == 2 );

  ufAssert( dval( dv1, 0 ).extract( 32, 32 ).getSignedValue() == 34 );
  ufAssert( dval( dv1, 0 ).extract( 0, 32 ).getSignedValue() == 2 );

  ufAssert( dval( dv2, 0 ).extract( 32, 32 ).getSignedValue() == 0xFFFFFFDE );
  ufAssert( dval( dv2, 0 ).extract( 0, 32 ).getSignedValue() == 2 );

  ufAssert( uval( dvinitb1, 2 ).at( 31 ) == WIR_L4::b0 );
  ufAssert( uval( dvinith1, 2 ).at( 31 ) == WIR_L4::b0 );

  return( 0 );
}
