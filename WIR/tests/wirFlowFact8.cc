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
  This file tests class WIR_Flowrestriction, it should exit normally, no
  assertion should fail.
*/

int main( void )
{
  WIR_Init();

  WIR_BasicBlock b1, b2, b3, b4, b5;

  // Test default constructor.
  WIR_FlowRestriction fr1;

  ufAssert( fr1.getType() == WIR_FlowFactType::flowrestriction );

  ufAssert( fr1.getLeq().size() == 0 );
  ufAssert( fr1.getGeq().size() == 0 );

  // This will also test isPartOfLeq and isPartOfGeq.
  ufAssert( fr1.isPartOfFlowFact( b1 ) == false );
  ufAssert( fr1.isSignificant() == false );

  // Test addToGeq.
  fr1.addToGeq( 1, b1 );
  ufAssert( fr1.isPartOfFlowFact( b1 ) == true );
  ufAssert( fr1.isPartOfGeq( b1 ) == true );
  ufAssert( fr1.isSignificant() == false );

  // Test addToLeq.
  fr1.addToLeq( 2, b2 );
  ufAssert( fr1.isPartOfFlowFact( b2 ) == true );
  ufAssert( fr1.isPartOfLeq( b2 ) == true );
  ufAssert( fr1.isSignificant() == true );

  // Test list getter-functions and internal data.
  ufAssert( fr1.getLeq().size() == 1 );
  ufAssert( fr1.getGeq().size() == 1 );

  auto leq1 = fr1.getLeq();
  auto geq1 = fr1.getGeq();
  ufAssert( geq1.front().second.get().getID() == b1.getID() );
  ufAssert( geq1.front().first == 1 );
  ufAssert( leq1.front().second.get().getID() == b2.getID() );
  ufAssert( leq1.front().first == 2 );

  // Add to existing summand via addToLeq.
  fr1.addToLeq( 3, b2 );
  leq1 = fr1.getLeq();
  ufAssert( leq1.size() == 1 );
  ufAssert( leq1.front().second.get().getID() == b2.getID() );
  ufAssert( leq1.front().first == 5 );

  // Test removal of a summand.
  ufAssert( fr1.eraseSummand( b2 ) == 5 );
  ufAssert( fr1.isPartOfFlowFact( b2 ) == false );
  ufAssert( fr1.isSignificant() == false );

  // Test exchange of a summand.
  ufAssert( fr1.isPartOfGeq( b1 ) == true );
  ufAssert( fr1.isPartOfLeq( b1 ) == false );
  fr1.replaceSummand( b1, b2 );
  ufAssert( fr1.isPartOfGeq( b1 ) == false );
  ufAssert( fr1.isPartOfGeq( b2 ) == true );
  geq1 = fr1.getGeq();
  ufAssert( geq1.size() == 1 );
  ufAssert( geq1.front().second.get().getID() == b2.getID() );
  ufAssert( geq1.front().first == 1 );

  // Test list initializers.
  list<pair<int, reference_wrapper<const WIR_BasicBlock>>> leq2, geq2;

  leq2.push_back( make_pair( 1, cref( b1 ) ) );
  leq2.push_back( make_pair( 2, cref( b2 ) ) );
  leq2.push_back( make_pair( 3, cref( b3 ) ) );
  geq2.push_back( make_pair( 4, cref( b4 ) ) );
  geq2.push_back( make_pair( 5, cref( b5 ) ) );

  // Test list copy constructor.
  WIR_FlowRestriction fr2( leq2, geq2 );

  // Test proper internal assignment.
  ufAssert(
    fr2.isPartOfLeq( b1 ) && fr2.isPartOfLeq( b2 ) && fr2.isPartOfLeq( b3 ) );
  ufAssert( fr2.isPartOfGeq( b4 ) && fr2.isPartOfGeq( b5 ) );

  // Test internal data structures.
  leq2 = fr2.getLeq();
  geq2 = fr2.getGeq();

  ufAssert( leq2.size() == 3 );
  auto leq2_it = leq2.begin();
  ufAssert( leq2_it->second.get().getID() == b1.getID() );
  ufAssert( leq2_it->first == 1 );
  ++leq2_it;
  ufAssert( leq2_it->second.get().getID() == b2.getID() );
  ufAssert( leq2_it->first == 2 );
  ++leq2_it;
  ufAssert( leq2_it->second.get().getID() == b3.getID() );
  ufAssert( leq2_it->first == 3 );

  auto geq2_it = geq2.begin();
  ufAssert( geq2_it->second.get().getID() == b4.getID() );
  ufAssert( geq2_it->first == 4 );
  ++geq2_it;
  ufAssert( geq2_it->second.get().getID() == b5.getID() );
  ufAssert( geq2_it->first == 5 );

  // Test list move constructor.
  list<pair<int, reference_wrapper<const WIR_BasicBlock>>> leq3( leq2 );
  list<pair<int, reference_wrapper<const WIR_BasicBlock>>> geq3( geq2 );

  WIR_FlowRestriction fr3( move( leq3 ), move( geq3 ) );

  // Test proper internal assignment.
  ufAssert(
    fr3.isPartOfLeq( b1 ) && fr3.isPartOfLeq( b2 ) && fr3.isPartOfLeq( b3 ) );
  ufAssert( fr3.isPartOfGeq( b4 ) && fr3.isPartOfGeq( b5 ) );

  // Test internal data structures.
  leq3 = fr3.getLeq();
  geq3 = fr3.getGeq();

  ufAssert( leq3.size() == 3 );
  auto leq3_it = leq3.begin();
  ufAssert( leq3_it->second.get().getID() == b1.getID() );
  ufAssert( leq3_it->first == 1 );
  ++leq3_it;
  ufAssert( leq3_it->second.get().getID() == b2.getID() );
  ufAssert( leq3_it->first == 2 );
  ++leq3_it;
  ufAssert( leq3_it->second.get().getID() == b3.getID() );
  ufAssert( leq3_it->first == 3 );

  auto geq3_it = geq3.begin();
  ufAssert( geq3_it->second.get().getID() == b4.getID() );
  ufAssert( geq3_it->first == 4 );
  ++geq3_it;
  ufAssert( geq3_it->second.get().getID() == b5.getID() );
  ufAssert( geq3_it->first == 5 );

  // Test equality operator.
  ufAssert( fr2.getID() != fr3.getID() );
  ufAssert( fr2.isEqual( fr3 ) );

  // Test bad input data.
  WIR_FlowRestriction fr4;

  fr4.addToGeq( 4, b4 );
  fr4.addToLeq( 0, b1 );
  ufAssert( fr4.isSignificant() == false );
  ufAssert( fr4.getLeq().size() == 0 );

  list<pair<int, reference_wrapper<const WIR_BasicBlock>>> leq_zero, geq_zero;

  leq_zero.push_back( make_pair( -1, b1 ) );
  geq_zero.push_back( make_pair( 4, b4 ) );

  WIR_FlowRestriction fr5( leq_zero, geq_zero );

  ufAssert( fr5.isSignificant() == false );
  ufAssert( fr5.getLeq().size() == 0 );
  ufAssert( fr5.getGeq().size() == 0 );

  WIR_FlowRestriction fr6( move( leq_zero ), move( geq_zero ) );

  ufAssert( fr5.isSignificant() == false );
  ufAssert( fr5.getLeq().size() == 0 );
  ufAssert( fr5.getGeq().size() == 0 );

  // Test copy constructor.
  WIR_FlowRestriction fr7( fr2 );

  // Test proper internal assignment.
  ufAssert(
    fr7.isPartOfLeq( b1 ) && fr7.isPartOfLeq( b2 ) && fr7.isPartOfLeq( b3 ) );
  ufAssert( fr7.isPartOfGeq( b4 ) && fr7.isPartOfGeq( b5 ) );

  // Test internal data structures.
  auto leq7 = fr7.getLeq();
  auto geq7 = fr7.getGeq();

  ufAssert( leq7.size() == 3 );
  auto leq7_it = leq7.begin();
  ufAssert( leq7_it->second.get().getID() == b1.getID() );
  ufAssert( leq7_it->first == 1 );
  ++leq7_it;
  ufAssert( leq7_it->second.get().getID() == b2.getID() );
  ufAssert( leq7_it->first == 2 );
  ++leq7_it;
  ufAssert( leq7_it->second.get().getID() == b3.getID() );
  ufAssert( leq7_it->first == 3 );

  auto geq7_it = geq7.begin();
  ufAssert( geq7_it->second.get().getID() == b4.getID() );
  ufAssert( geq7_it->first == 4 );
  ++geq7_it;
  ufAssert( geq7_it->second.get().getID() == b5.getID() );
  ufAssert( geq7_it->first == 5 );

  return( 0 );
};
