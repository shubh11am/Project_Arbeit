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

/*!
  @file wirflowfact.cc
  @brief This file implements an abstract base class for %WIR flow facts.

  @author Til Mauersberger <Til.Mauersberger@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

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
  Default constructor.
*/
WIR_FlowFact::WIR_FlowFact( void ) :
  WIR_ID_API {},
  mSystemPointer { nullptr }
{
  DSTART( "WIR_FlowFact::WIR_FlowFact()" );
};


/*
  Copy constructor.

  When copying a flow fact that is inserted in some WIR system, the resulting
  copy will not be inserted in a system.
*/
WIR_FlowFact::WIR_FlowFact( const WIR_FlowFact &__o ) :
  WIR_ID_API { __o },
  mSystemPointer { nullptr }
{
  DSTART( "WIR_FlowFact::WIR_FlowFact(const WIR_FlowFact&)" );
};


/*
  Destructor.
*/
WIR_FlowFact::~WIR_FlowFact( void )
{
  DSTART( "virtual WIR_FlowFact::~WIR_FlowFact()" );
};


/*
  Copy-assignment operator.

  When assigning a flow fact that is inserted in some WIR system, the resulting
  copy will not be inserted in a system.
*/
WIR_FlowFact & WIR_FlowFact::operator = ( const WIR_FlowFact &__o )
{
  DSTART( "WIR_FlowFact& WIR_FlowFact::operator=(const WIR_FlowFact&)" );

  (void) __o;

  mSystemPointer = nullptr;

  return( *this );
};


/*
  The << operator dumps a WIR flow fact to an output stream.
*/
std::ostream & operator << ( std::ostream &os, const WIR_FlowFact &f )
{
  DSTART( "ostream& operator<<(ostream&, const WIR_FlowFact&)" );

  switch ( f.getType() ) {

    case WIR_FlowFactType::entrypoint: {
      os << dynamic_cast<const WIR_EntryPoint &>( f );

      break;
    }

    case WIR_FlowFactType::flowrestriction: {
      os << dynamic_cast<const WIR_FlowRestriction &>( f );

      break;
    }

    case WIR_FlowFactType::loopbound: {
      os << dynamic_cast<const WIR_LoopBound &>( f );

      break;
    }

    default: {
      ufAssertT( false, "Unsupported flow fact type detected!" );
      break;
    }
  }

  return( os );
};


/*
  isInserted returns wether this object is inserted into some WIR_System.
*/
bool WIR_FlowFact::isInserted( void ) const
{
  DSTART( "bool WIR_FlowFact::isInserted() const" );

  DACTION(
    DOUT( "myID = " << getID() << "; mSystem = " );
    if ( mSystemPointer == nullptr )
      DOUT( "<nullptr>" << endl );
    else
      DOUT( mSystemPointer->getID() << endl ); );

  return( mSystemPointer != nullptr );
};


/*
  getSystem returns the WIR_System to which this flow fact is assigned.

  If this object has not been assigned to a WIR_System before, getSystem will
  fail with an assertion.
*/
WIR_System &WIR_FlowFact::getSystem( void ) const
{
  DSTART( "WIR_System& WIR_FlowFact::getSystem() const" );

  DACTION(
    DOUT( "myID = " << getID() << "; mSystem = " );
    if ( mSystemPointer == nullptr )
      DOUT( "<nullptr>" << endl );
    else
      DOUT( mSystemPointer->getID() << endl ); );

  return( *mSystemPointer );
};


/*
  isSignificant returns whether a flow fact is significant for WCET calculation
  or not.

  In some cases, a flow fact may not be significant for WCET calculation, e.g.:
  - A loop bound with min: -1 and max: -1
  - A flow restriction with 0 <= SUM.
  In such cases, this method should return false, in all other cases true.
  This could be used to remove all unnecessary flow facts.
*/
bool WIR_FlowFact::isSignificant( void ) const
{
  DSTART( "bool WIR_FlowFact::isSignificant() const" );

  // By default, every flow fact is significant.
  return( true );
};


//
// Protected class methods
//

/*
  addReference adds a reference to a given WIR basic block.

  The FlowFactRef container of the specified basic block is told that this flow
  fact is using it (so that that FlowFactRef has to hold a reference to this
  flow fact), but only if this flow fact is currently inserted into a WIR
  system. If no FlowFactRef already exists, a new one (for the given basic
  block) is created.
*/
void WIR_FlowFact::addReference( const WIR_BasicBlock &b )
{
  DSTART( "void WIR_FlowFact::addReference(const WIR_BasicBlock&)" );

  // Only add references of already inserted flow facts.
  if ( !isInserted() )
    return;

  // Get the basic block's FlowFactRef.
  auto &ref = WIR_FlowFactRef::get( const_cast<WIR_BasicBlock &>( b ) );

  ref.pushBackFlowFact( *this );
};


/*
  eraseReference erases this flow fact from a given basic block's FlowFactRef.

  The FlowFactRef of the given basic block is told that this flow fact is no
  longer using it, but only if this flow fact is currently inserted into a WIR
  system.
*/
void WIR_FlowFact::eraseReference( const WIR_BasicBlock &b )
{
  DSTART( "void WIR_FlowFact::eraseReference(const WIR_BasicBlock&)" );

  // Only do this for inserted flow facts.
  if ( !isInserted() )
    return;

  // Check if a FlowFactRef even exists.
  if ( !b.containsContainers( WIR_FlowFactRef::getContainerTypeID() ) )
    return;

  auto &ref = WIR_FlowFactRef::get( const_cast<WIR_BasicBlock &>( b ) );

  ref.eraseFlowFact( *this );
};


/*
  onInsert is called whenever this flow fact is added to a WIR_System.

  This method is virtual so that flow facts deriving from this base class can
  implement their own behaviour regarding adding themselves to all concerning
  FlowFactRefs.

  Note: When overriding this method, make sure to either call this base class'
  implementation to set mSystemPointer, or assign it by yourself.
*/
void WIR_FlowFact::onInsert( WIR_System *s )
{
  DSTART( "void WIR_FlowFact::onInsert(WIR_System*)" );

  mSystemPointer = s;

  DACTION(
    DOUT( "myID = " << getID() << "; setting mSystem = " );
    if ( mSystemPointer == nullptr )
      DOUT( "<nullptr>" << endl );
    else
      DOUT( mSystemPointer->getID() << endl ); );
};

}       // namespace WIR
