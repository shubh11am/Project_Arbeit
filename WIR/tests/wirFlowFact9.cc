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
  Testing insertion into systems and WIR_System copy constructor.
*/

int main( void )
{
  WIR_Init();

  // Set up WIR systems and basic blocks to insert flow facts into.
  WIR_CompilationUnit c;
  WIR_TaskManager t;
  WIR_System sys( "genericmips.sys", t );

  // Create a simple function with a basic block hierarchy representing a
  // self-loop.
  WIR_Function f { "foo" };

  auto &r1  = f.pushBackVirtualRegister( MIPS_RegV() );
  auto &r2  = f.pushBackVirtualRegister( MIPS_RegV() );

  auto &b1 = f.pushBackBasicBlock( {} );
  auto &b2 = f.pushBackBasicBlock( {} );
  auto &b3 = f.pushBackBasicBlock( {} );

  b1.pushBackInstruction(
    { { MIPS::OpCode::LW, MIPS::OperationFormat::RIR_1,
        WIR_RegisterParameter( r1, WIR_Usage::def ),
        MIPS_Immediate16_Signed( -138 ),
        WIR_RegisterParameter( r2, WIR_Usage::use ) } } );

  b2.pushBackInstruction(
    { { MIPS::OpCode::JR, MIPS::OperationFormat::R_2,
        WIR_RegisterParameter( r1, WIR_Usage::use ) } } );
  b2.rbegin()->get().begin()->get().addJumpTarget( b2 );
  b2.rbegin()->get().begin()->get().addJumpTarget( b3 );

  b3.pushBackInstruction(
    { { MIPS::OpCode::ADDI, MIPS::OperationFormat::RRI,
        WIR_RegisterParameter( r1, WIR_Usage::def ),
        WIR_RegisterParameter( r2, WIR_Usage::use ),
        MIPS_Immediate16_Signed( 0 ) } } );

  c.pushBackFunction( f );
  sys.pushBackCompilationUnit( c );

  // Get references to the inserted basic blocks.
  auto bb_iter = sys.begin()->get().begin()->get().begin();
  WIR_BasicBlock &bb1 = *bb_iter++;
  WIR_BasicBlock &bb2 = *bb_iter++;
  WIR_BasicBlock &bb3 = *bb_iter++;

  ufAssert( bb2.isInserted() );
  ufAssert( bb2.getFunction().isInserted() );
  ufAssert( bb2.getFunction().getCompilationUnit().isInserted() );
  ufAssert( bb2.getFunction().getCompilationUnit().getSystem() == sys );

  // Create some flow restrictions.
  list<pair<int, reference_wrapper<const WIR_BasicBlock>>> leq;
  list<pair<int, reference_wrapper<const WIR_BasicBlock>>> geq;

  leq.push_back( make_pair( 1, cref( bb1 ) ) );
  leq.push_back( make_pair( 2, cref( bb2 ) ) );
  geq.push_back( make_pair( 3, cref( bb3 ) ) );

  WIR_FlowRestriction fr1 { leq, geq };

  geq.pop_back();
  geq.push_back( make_pair( 6, cref( bb3 ) ) );

  WIR_FlowRestriction fr2 { move( leq ), move( geq ) };
  WIR_FlowRestriction fr3 { fr1 };

  // Create some loop bounds.
  WIR_LoopBound lb1
    { 2, 10, bb2, WIR_LoopBound::LoopControlType::headcontrolled };
  WIR_LoopBound lb2 { lb1 };

  // Test insertion into WIR System.
  auto &fr = sys.pushBackFlowFact( fr1 );
  sys.pushBackFlowFact( fr2 );
  auto &loop = sys.pushBackFlowFact( lb1 );

  ufAssert( sys.getFlowFacts().size() == 3 );
  ufAssert( sys.getFlowFacts<WIR_LoopBound>().size() == 1 );
  ufAssert( sys.getFlowFacts<WIR_LoopBound>().begin()->get() == loop );
  ufAssert( sys.getFlowFacts<WIR_FlowRestriction>().size() == 2 );
  ufAssert( sys.getFlowFacts<WIR_FlowRestriction>().begin()->get() == fr );
  ufAssert( sys.containsFlowFact( loop.getID() ) );

  // Check that mSystempointer is set properly.
  for ( const WIR_FlowFact &ff : sys.getFlowFacts() )  {
    ufAssert( ff.isInserted() == true );
    ufAssert( ff.getSystem() == sys );
  }

  // Make a copy of WIR_System, check it later.
  WIR_System sys2 { sys };

  // Test erasing flow facts.
  // Erase by ID.
  WIR_id_t loopID = loop.getID();
  ufAssert( loop.isInserted() == true );
  ufAssert( sys.getFlowFacts().size() == 3 );
  sys.eraseFlowFact( loop.getID() );
  ufAssert( sys.getFlowFacts().size() == 2 );
  ufAssert( sys.containsFlowFact( loopID ) == false );

  // Erasing twice shouldn't change anything.
  sys.eraseFlowFact( loopID );
  ufAssert( sys.getFlowFacts().size() == 2 );
  ufAssert( sys.containsFlowFact( loopID ) == false );

  // Erase by iterator.
  auto &flowfactset = sys.getFlowFacts();
  WIR_id_t frID = flowfactset.begin()->get().getID();
  ufAssert( flowfactset.begin()->get().isInserted() == true );
  ufAssert( flowfactset.size() == 2 );
  sys.eraseFlowFact( flowfactset.begin() );
  ufAssert( sys.containsFlowFact( frID ) == false );
  ufAssert( flowfactset.size() == 1 );

  // Clear flow facts.
  sys.clearFlowFacts();
  ufAssert( sys.getFlowFacts().size() == 0 );

  // This last flow fact should get deleted upon destruction of the WIR_System.
  sys.pushBackFlowFact( lb2 );

  // Check the copy of sys created earlier.
  ufAssert( sys2.getFlowFacts().size() == 3 );

  auto sys2_bb_iter = sys2.begin()->get().begin()->get().begin();
  WIR_BasicBlock &bc1 = *sys2_bb_iter++;
  WIR_BasicBlock &bc2 = *sys2_bb_iter++;
  WIR_BasicBlock &bc3 = *sys2_bb_iter++;

  ufAssert( sys2.getFlowFacts<WIR_LoopBound>().size() == 1 );
  WIR_LoopBound &loop_copy = *sys2.getFlowFacts<WIR_LoopBound>().begin();
  ufAssert( loop_copy.getMin() == 2 );
  ufAssert( loop_copy.getMax() == 10 );
  ufAssert( loop_copy.getLoop() == bc2 );
  ufAssert(
    loop_copy.getLoopControlType() ==
      WIR_LoopBound::LoopControlType::headcontrolled );

  ufAssert( sys2.getFlowFacts<WIR_FlowRestriction>().size() == 2 );
  WIR_FlowRestriction &fr_copy =
    *sys2.getFlowFacts<WIR_FlowRestriction>().begin();
  ufAssert( fr_copy.isPartOfLeq( bc1 ) == true );
  ufAssert( fr_copy.isPartOfLeq( bc2 ) == true );
  ufAssert( fr_copy.isPartOfGeq( bc3 ) == true );

  // Check that mSystempointer is set properly.
  for ( const WIR_FlowFact &ff : sys2.getFlowFacts() )  {
    ufAssert( ff.isInserted() == true );
    ufAssert( ff.getSystem() == sys2 );
  }

  return( 0 );
};
