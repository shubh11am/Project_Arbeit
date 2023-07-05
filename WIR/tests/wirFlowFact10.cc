/*

   This source file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Intermediate Representation Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2021 - 2022

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
#include <list>
#include <utility>

// Include libuseful headers
#include <libuseful/io.h>

// Include WIR headers
#include <wir/wir.h>
#include <arch/generic/mips.h>


using namespace std;
using namespace WIR;


/*
  Testing WIR_FlowfactRef
*/

int main( void )
{
  WIR_Init();

  // Set up WIR system and basic blocks to insert flow facts into.
  WIR_CompilationUnit c;
  WIR_TaskManager t;
  WIR_System *sys_p = new WIR_System( "genericmips.sys", t );
  WIR_System &sys = *sys_p;
  WIR_Function f { "foo" };

  auto &r1  = f.pushBackVirtualRegister( MIPS_RegV() );
  auto &r2  = f.pushBackVirtualRegister( MIPS_RegV() );

  auto &bb1 = f.pushBackBasicBlock( {} );
  auto &bb2 = f.pushBackBasicBlock( {} );
  auto &bb3 = f.pushBackBasicBlock( {} );

  bb1.pushBackInstruction(
    { { MIPS::OpCode::LW, MIPS::OperationFormat::RIR_1,
        WIR_RegisterParameter( r1, WIR_Usage::def ),
        MIPS_Immediate16_Signed( -138 ),
        WIR_RegisterParameter( r2, WIR_Usage::use ) } } );

  bb2.pushBackInstruction(
    { { MIPS::OpCode::JR, MIPS::OperationFormat::R_2,
        WIR_RegisterParameter( r1, WIR_Usage::use ) } } );
  bb2.rbegin()->get().begin()->get().addJumpTarget( bb2 );
  bb2.rbegin()->get().begin()->get().addJumpTarget( bb3 );

  bb3.pushBackInstruction(
    { { MIPS::OpCode::ADDI, MIPS::OperationFormat::RRI,
        WIR_RegisterParameter( r1, WIR_Usage::def ),
        WIR_RegisterParameter( r2, WIR_Usage::use ),
        MIPS_Immediate16_Signed( 0 ) } } );

  c.pushBackFunction( f );
  sys.pushBackCompilationUnit( c );

  // Get references to the inserted basic blocks.
  auto bb_iter = sys.begin()->get().begin()->get().begin();
  WIR_BasicBlock &b1 = *bb_iter++;
  WIR_BasicBlock &b2 = *bb_iter++;
  WIR_BasicBlock &b3 = *bb_iter++;

  WIR_LoopBound _lb
    { 2, 10, b2, WIR_LoopBound::LoopControlType::headcontrolled };

  auto &lb = sys.pushBackFlowFact( _lb );

  // Create some flow restrictions.
  list<pair<int, reference_wrapper<const WIR_BasicBlock>>> leq;
  list<pair<int, reference_wrapper<const WIR_BasicBlock>>> geq;

  leq.push_back( make_pair( 1, cref( b1 ) ) );
  leq.push_back( make_pair( 2, cref( b2 ) ) );
  geq.push_back( make_pair( 3, cref( b3 ) ) );

  WIR_FlowRestriction _fr1 { leq, geq };

  geq.pop_back();
  geq.push_back( make_pair( 6, cref( b3 ) ) );

  WIR_FlowRestriction _fr2 { move( leq ), move( geq ) };

  auto &fr1 = sys.pushBackFlowFact( _fr1 );
  auto &fr2 = sys.pushBackFlowFact( _fr2 );

  WIR_System sys2 { sys };

  auto sys2_bb_iter = sys2.begin()->get().begin()->get().begin();
  WIR_BasicBlock &bc1 = *sys2_bb_iter++;
  WIR_BasicBlock &bc2 = *sys2_bb_iter++;
  WIR_BasicBlock &bc3 = *sys2_bb_iter++;

  auto sys2_fr_list = sys2.getFlowFacts<WIR_FlowRestriction>();
  auto sys2_fr_iter = sys2_fr_list.begin();
  WIR_FlowRestriction &fr1_copy = *sys2_fr_iter++;
  WIR_FlowRestriction &fr2_copy = *sys2_fr_iter++;

  WIR_LoopBound &lb_copy = sys2.getFlowFacts<WIR_LoopBound>().begin()->get();

  //
  // Test first system.
  //

  auto &ref1 = WIR_FlowFactRef::get( b1 );
  auto &ref2 = WIR_FlowFactRef::get( b2 );
  auto &ref3 = WIR_FlowFactRef::get( b3 );

  ufAssert( ref1.getFlowFacts().empty() == false );
  ufAssert( ref2.getFlowFacts().empty() == false );
  ufAssert( ref3.getFlowFacts().empty() == false );

  ufAssert( ref1.getFlowFacts<WIR_LoopBound>().size() == 0 );
  ufAssert( ref1.getFlowFacts<WIR_FlowRestriction>().front().get() == fr1 );
  ufAssert( ref1.getFlowFacts<WIR_FlowRestriction>().back().get() == fr2 );

  ufAssert( ref2.getFlowFacts<WIR_LoopBound>().front().get() == lb );
  ufAssert( ref2.getFlowFacts<WIR_FlowRestriction>().front().get() == fr1 );
  ufAssert( ref2.getFlowFacts<WIR_FlowRestriction>().back().get() == fr2 );

  ufAssert( ref3.getFlowFacts<WIR_LoopBound>().size() == 0 );
  ufAssert( ref3.getFlowFacts<WIR_FlowRestriction>().front().get() == fr1 );
  ufAssert( ref3.getFlowFacts<WIR_FlowRestriction>().back().get() == fr2 );


  //
  // Test copied system.
  //

  auto &ref1_copy = WIR_FlowFactRef::get( bc1 );
  auto &ref2_copy = WIR_FlowFactRef::get( bc2 );
  auto &ref3_copy = WIR_FlowFactRef::get( bc3 );

  ufAssert( ref1_copy.getFlowFacts().empty() == false );
  ufAssert( ref2_copy.getFlowFacts().empty() == false );
  ufAssert( ref3_copy.getFlowFacts().empty() == false );

  ufAssert( ref1_copy.getFlowFacts<WIR_LoopBound>().size() == 0 );
  ufAssert(
    ref1_copy.getFlowFacts<WIR_FlowRestriction>().front().get() == fr1_copy );
  ufAssert(
    ref1_copy.getFlowFacts<WIR_FlowRestriction>().back().get() == fr2_copy );

  ufAssert( ref2_copy.getFlowFacts<WIR_LoopBound>().front().get() == lb_copy );
  ufAssert(
    ref2_copy.getFlowFacts<WIR_FlowRestriction>().front().get() == fr1_copy );
  ufAssert(
    ref2_copy.getFlowFacts<WIR_FlowRestriction>().back().get() == fr2_copy );

  ufAssert( ref3_copy.getFlowFacts<WIR_LoopBound>().size() == 0 );
  ufAssert(
    ref3_copy.getFlowFacts<WIR_FlowRestriction>().front().get() == fr1_copy );
  ufAssert(
    ref3_copy.getFlowFacts<WIR_FlowRestriction>().back().get() == fr2_copy );

  delete( sys_p );
};
