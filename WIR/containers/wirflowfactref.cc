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
  @file wirflowfactref.cc
  @brief This file implements a %WIR container representing references of flow
         facts to basic blocks.

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
  Copy constructor.
*/
WIR_FlowFactRef::WIR_FlowFactRef( const WIR_FlowFactRef &__o ) :
  WIR_Container<WIR_FlowFactRef> { __o }
{
  DSTART( "WIR_FlowFactRef::WIR_FlowFactRef(const WIR_FlowFactRef&)" );
};


/*
  Destructor.
*/
WIR_FlowFactRef::~WIR_FlowFactRef( void )
{
  DSTART( "virtual WIR_FlowFactRef::~WIR_FlowFactRef()" );
};


/*
  Copy-assignment operator.
*/
WIR_FlowFactRef & WIR_FlowFactRef::operator = ( const WIR_FlowFactRef &__o )
{
  DSTART(
    "WIR_FlowFactRef& WIR_FlowFactRef::operator=(const WIR_FlowFactRef&)" );

  WIR_Container<WIR_FlowFactRef>::operator = ( __o );

  return( *this );
};


/*
  isUnique returns whether flow fact references are unique, i.e., whether at
  most one instance of this container type can be attached to a WIR class.
*/
bool WIR_FlowFactRef::isUnique( void ) const
{
  DSTART( "virtual bool WIR_FlowFactRef::isUnique() const" );

  return( true );
};


/*
  get returns the FlowFactRef attached to a given basic block.

  get either returns the FlowFactRef already attached to the given basic block
  or, in case that it currently does not contain a FlowFactRef, creates a new
  one and attaches it automatically.

  Note: This is the recommended method of getting access to FlowFactRefs.
        Constructors are for internal use only.
*/
WIR_FlowFactRef &WIR_FlowFactRef::get( WIR_BasicBlock &b )
{
  DSTART(
    "static WIR_FlowFactRef& WIR_FlowFactRef::get(WIR_BasicBlock&)" );

  if ( !b.containsContainers( WIR_FlowFactRef::getContainerTypeID() ) ) {
    DOUT ( "Creating a new FlowFactRef." << endl );
    new WIR_FlowFactRef { b };
  }

  return( b.getContainers<WIR_FlowFactRef>().begin()->get() );
};


/*
  getFlowFacts returns a list of all flow facts belonging to this basic block.

  Only flow facts which are part of a WIR System are returned.
*/
const list<reference_wrapper<WIR_FlowFact>> &WIR_FlowFactRef::getFlowFacts( void ) const
{
  DSTART(
    "const list<reference_wrapper<WIR_FlowFact> >& "
    "WIR_FlowFactRef::getFlowFacts() const" );

  return( mFlowFacts );
};


/*
  The << operator dumps all flow facts stored by this container to an output
  stream.
*/
std::ostream & operator << ( std::ostream &os, const WIR_FlowFactRef &c )
{
  DSTART( "ostream& operator<<(ostream&, const WIR_FlowFactRef&)" );

  for ( const WIR_FlowFact &f : c.mFlowFacts )
    os << f;

  return( os );
};


//
// Private class methods
//

/*
  Default constructor.

  This constructor creates an empty FlowFactRef for the given basic block and
  attaches it as a container to the given basic block.

  Note: Only use this constructor when dynamically allocating a new FlowFactRef.
*/
WIR_FlowFactRef::WIR_FlowFactRef( WIR_BasicBlock &b ) :
  WIR_Container<WIR_FlowFactRef> {}
{
  DSTART( "WIR_FlowFactRef::WIR_FlowFactRef(WIR_BasicBlock&)" );

  b.insertContainer( this );
};


/*
  pushBackFlowFact adds a reference to a flow fact.

  pushBackFlowFact is used by the flow facts to tell the FlowFactRef that they
  concern it and are part of a WIR System.
*/
void WIR_FlowFactRef::pushBackFlowFact( const WIR_FlowFact &f )
{
  DSTART( "void WIR_FlowFactRef::pushBackFlowFact(const WIR_FlowFact&)" );

  // Test if the flow fact already exists.
  for ( const auto &m_ff : mFlowFacts )
    ufAssertT( f != m_ff, "Invalid attempt to add a flow fact again." );

  // Add flow fact to internal container.
  mFlowFacts.push_back( const_cast<WIR_FlowFact &>( f ) );
};


/*
  eraseFlowFact erases a reference to a flow fact.

  eraseFlowFact is used by the flow facts to tell the FlowFactRef that they no
  longer concern it or are removed from a WIR System.
*/
void WIR_FlowFactRef::eraseFlowFact( WIR_FlowFact &f )
{
  DSTART( "void WIR_FlowFactRef::eraseFlowFact(WIR_FlowFact&)" );

  // Test if flow fact really exists.
  for ( auto it = mFlowFacts.begin(); it != mFlowFacts.end(); ++it )
    if ( it->get().getID() == f.getID() ) {
      DOUT(
        "Found flow fact (ID: " << f.getID() << "). Erasing it now!" << endl );
      mFlowFacts.erase( it );
      return;
    }
};

}      // namespace WIR
