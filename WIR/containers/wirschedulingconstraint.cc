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

/*!
  @file wirschedulingconstraint.cc
  @brief This file implements a %WIR container representing constraints for an
         instruction scheduler.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include standard headers
#include <map>
#include <set>

// Include libuseful headers
#include <libuseful/debugmacros.h>
#include <libuseful/io.h>

// Include WIR headers
#include <wir/wir.h>


//
// Code section
//

namespace WIR {


using namespace std;


//
// Public class methods
//

/*
  Copy constructor.
*/
WIR_SchedulingConstraint::WIR_SchedulingConstraint( const WIR_SchedulingConstraint &__o ) :
  WIR_Container<WIR_SchedulingConstraint> { __o },
  mType { __o.mType },
  mInstrSequence { __o.mInstrSequence },
  mInstrs { __o.mInstrs }
{
  DSTART(
    "WIR_SchedulingConstraint::WIR_SchedulingConstraint(const WIR_SchedulingConstraint&)" );
};


/*
  Move constructor.
*/
WIR_SchedulingConstraint::WIR_SchedulingConstraint( WIR_SchedulingConstraint &&__o ) :
  WIR_Container<WIR_SchedulingConstraint> { move( __o ) },
  mType { __o.mType },
  mInstrSequence { move( __o.mInstrSequence ) },
  mInstrs { move( __o.mInstrs ) }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  __o.mInstrSequence.clear();
  __o.mInstrs.clear();
};


/*
  Destructor.
*/
WIR_SchedulingConstraint::~WIR_SchedulingConstraint( void )
{
  DSTART( "virtual WIR_SchedulingConstraint::~WIR_SchedulingConstraint()" );
};


/*
  Copy-assignment operator.
*/
WIR_SchedulingConstraint & WIR_SchedulingConstraint::operator = ( const WIR_SchedulingConstraint &__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  WIR_Container<WIR_SchedulingConstraint>::operator = ( __o );

  mType = __o.mType;
  mInstrSequence = __o.mInstrSequence;
  mInstrs = __o.mInstrs;

  return( *this );
};


/*
  Move-assignment operator.
*/
WIR_SchedulingConstraint & WIR_SchedulingConstraint::operator = ( WIR_SchedulingConstraint &&__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  WIR_Container<WIR_SchedulingConstraint>::operator = ( move( __o ) );

  mType = __o.mType;

  mInstrSequence = move( __o.mInstrSequence );
  __o.mInstrSequence.clear();

  mInstrs =  move( __o.mInstrs );
  __o.mInstrs.clear();

  return( *this );
};


/*
  isUnique returns whether scheduling constraints are unique, i.e., whether at
  most one instance of this container type can be attached to a WIR class.
*/
bool WIR_SchedulingConstraint::isUnique( void ) const
{
  DSTART( "virtual bool WIR_SchedulingConstraint::isUnique() const" );

  return( false );
};


/*
  getType returns the type of a scheduling constraint.
*/
WIR_SchedulingConstraintType WIR_SchedulingConstraint::getType( void ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  return( mType );
};


/*
  getInstructionSequence returns the list mInstrSequence.
*/
const std::list<std::reference_wrapper<WIR_Instruction>> &WIR_SchedulingConstraint::getInstructionSequence( void ) const
{
  DSTART(
    "const list<reference_wrapper<WIR_Instruction> >& WIR_SchedulingConstraint::getInstructionSequence() const" );

  return( mInstrSequence );
};


/*
  getInstructions returns the set of all constrained instructions.
*/
const WIR_InstructionSet &WIR_SchedulingConstraint::getInstructions( void ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  return( mInstrs );
};


/*
  check checks the validity of a scheduling constraint.
*/
bool WIR_SchedulingConstraint::check( void ) const
{
  DSTART( "bool WIR_SchedulingConstraint::check() const" );

  map<WIR_id_t, unsigned long long> rank;
  long long cnt = 0;

  for ( WIR_Instruction &i : mInstrSequence )
    for ( WIR_Instruction &j : i.getBasicBlock() )
      if ( !rank.count( j.getID() ) )
        rank[ j.getID() ] = cnt++;

  cnt = -1;
  for ( WIR_Instruction &i : mInstrSequence ) {
    if ( cnt != -1 ) {
      if ( ( mType == WIR_SchedulingConstraintType::sequential ) &&
           ( rank[i.getID()] != static_cast<unsigned long long>( cnt ) + 1 ) )
        return ( false );
      else if ( ( mType == WIR_SchedulingConstraintType::ordered ) &&
                ( rank[i.getID()] <= static_cast<unsigned long long>( cnt ) ) )
        return ( false );
    }

    cnt = rank[ i.getID() ];
  }

  return( true );
};


//
// Private class methods
//

/*
  Function for terminating the setting of a scheduling constraint.

  A scheduling constraint must involve at least two different instructions.
  Since scheduling constraints only support local instruction scheduling within
  a single basic block, setConstraint fails with an assertion if any involved
  instruction is not inserted into a basic block or if instructions from
  different basic blocks are involved.

  setConstraint also forwards the scheduling constraint to all involved WIR
  instructions. Finally, setConstraint checks the validity of the given
  constraint and fails with an assertion if the constraint is invalid.
*/
void WIR_SchedulingConstraint::setConstraint( void ) const
{
  DSTART( "void WIR_SchedulingConstraint::setConstraint() const" );

  // Check the structural integrity of a constraint.
  set<WIR_id_t> blockIDs;

  for ( WIR_Instruction &i : mInstrSequence ) {
    ufAssert( i.isInserted() );

    blockIDs.insert( i.getBasicBlock().getID() );
  }

  ufAssert( mInstrs.size() > 1 );
  ufAssert( blockIDs.size() == 1 );

  // Check the validity of the scheduling constraint.
  ufAssertT( check(), "Scheduling constraint violated." );
};

}       // namespace WIR
