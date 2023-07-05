/*

   This source file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Compiler Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2022

   Hamburg University of Technology (TUHH)
   Institute of Embedded Systems
   21071 Hamburg
   Germany

   http://www.tuhh.de/es/esd/research/wcc

*/

/*!
  @file wirentrypoint.cc
  @brief This file implements %WIR entry points, a type of flow fact marking
         functions as entry points for execution.

  @author Til Mauersberger <Til.Mauersberger@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include boost headers
#include <boost/current_function.hpp>

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
  Default constructor creating an empty entry point.
*/
WIR_EntryPoint::WIR_EntryPoint( void ) :
  WIR_FlowFact {},
  mFunctionPointer { nullptr },
  mAttributes {}
{
  DSTART( "WIR_EntryPoint::WIR_EntryPoint()" );
};


/*
  Constructor creating an entry point referring to a WIR function.
*/
WIR_EntryPoint::WIR_EntryPoint( const WIR_Function &__f,
                                const std::map<std::string, std::string> &__a ) :
  WIR_FlowFact {},
  mFunctionPointer { const_cast<WIR_Function *>( &__f ) },
  mAttributes { __a }
{
  DSTART(
    "WIR_EntryPoint::WIR_EntryPoint(WIR_Function&, const "
    "map<string, string >&)" );
};


/*
  Copy constructor.

  The copy will not be inserted in any WIR system or referenced by any
  WIR_FlowFactRefs.
*/
WIR_EntryPoint::WIR_EntryPoint( const WIR_EntryPoint &__o ) :
  WIR_FlowFact { __o },
  mFunctionPointer { __o.mFunctionPointer },
  mAttributes { __o.mAttributes }
{
  DSTART( "WIR_EntryPoint::WIR_EntryPoint(const WIR_EntryPoint&)" );
};


/*
  Default destructor.
*/
WIR_EntryPoint::~WIR_EntryPoint( void )
{
  DSTART( "virtual WIR_EntryPoint::~WIR_EntryPoint()" );
};


/*
  Copy-assignement operator.

  The copy will not be inserted in any WIR system or referenced by any
  WIR_FlowFactRefs.
*/
WIR_EntryPoint &WIR_EntryPoint::operator = ( const WIR_EntryPoint &__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  WIR_FlowFact::operator = ( __o );

  mFunctionPointer = __o.mFunctionPointer;
  mAttributes = __o.mAttributes;

  return( *this );
};


/*
  getType returns the type of a WIR flow fact, i.e., whether it is an entry
  point, flow restriction or loop bound.
*/
WIR_FlowFactType WIR_EntryPoint::getType( void ) const
{
  DSTART( "virtual WIR_FlowFactType WIR_EntryPoint::getType() const" );

  return( WIR_FlowFactType::entrypoint );
};


/*
  The << operator dumps a WIR entry point to an output stream.
*/
std::ostream & operator << ( std::ostream &os, const WIR_EntryPoint &ep )
{
  DSTART( "ostream& WIR::operator<<(ostream&, const WIR_EntryPoint&)" );

  os << "Entry point: "
     << string(
          ( ep.mFunctionPointer ) ? ep.getFunction().getName() : "<nullptr>" )
     << endl;

  return( os );
};


/*
  setFunction sets the WIR function this entry point describes.
*/
void WIR_EntryPoint::setFunction( const WIR_Function &f )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  mFunctionPointer = const_cast<WIR_Function *>( &f );
};


/*
  getFunction returns the function marked as entry point.

  getFunction fails with an assertion if the entry point's function is not set.
*/
WIR_Function &WIR_EntryPoint::getFunction( void ) const
{
  DSTART( "WIR_Function& WIR_EntryPoint::getFunction() const" );

  ufAssertT(
    mFunctionPointer,
    "Invalid attempt to access unset function of entry point." );

  return( *mFunctionPointer );
};


/*
  getEntryPointNames determines the names of all functions within a given WIR
  system that are marked as entry point.

  getEntryPointNames adds all entry points found in a given WIR system to the
  returned set. If no entry points are present in a WIR system, the "main"
  function is returned as the default entry point.
*/
std::set<std::string> WIR_EntryPoint::getEntryPointNames( const WIR_System &sys )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  set<string> res;
  auto entryPoints = sys.getFlowFacts<WIR_EntryPoint>();

  for ( WIR_EntryPoint &ep : entryPoints )
    res.insert( ep.getFunction().getName() );

  if ( res.empty() )
    res.insert( "main" );

  return( res );
};


/*
  getAttributes returns an entry point's map of general purpose attributes.
*/
const std::map<std::string, std::string> &WIR_EntryPoint::getAttributes( void ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  return( mAttributes );
};


/*
  reorganize adjusts all references to WIR functions stored by an entry point
  after a deep copy of flow facts.
*/
void WIR_EntryPoint::reorganize( const std::map<WIR_id_t, WIR_BasicBlock *> &blockIDMap )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  if ( mFunctionPointer == nullptr )
    return;

  // Determine the original WIR basic block this entry point means to annotate
  // (the first basic block of a function).
  auto block_it = blockIDMap.find( mFunctionPointer->begin()->get().getID() );
  if ( block_it == blockIDMap.end() ) {
    mFunctionPointer = nullptr;
    return;
  }

  // Adjust entry point.
  mFunctionPointer = &(block_it->second->getFunction());
};


//
// Protected class methods
//

/*
  onInsert is called whenever this entry point is added to a WIR_System.
*/
void WIR_EntryPoint::onInsert( WIR_System *s )
{
  DSTART( "virtual void WIR_EntryPoint::onInsert(WIR_System*)" );

  // Call base class implementation to set mSystemPointer.
  WIR_FlowFact::onInsert( s );

  // Add reference to entry block of the marked function.
  if ( mFunctionPointer )
    addReference( *mFunctionPointer->begin() );
};


/*
  clone creates a copy of a WIR entry point.

  This method only calls the copy constructor and allocates a new WIR entry
  point on the heap.
*/
WIR_FlowFact *WIR_EntryPoint::clone( void ) const
{
  DSTART( "virtual WIR_FlowFact* WIR_EntryPoint::clone() const" );

  return( new WIR_EntryPoint( *this ) );
};

}       // namespace WIR
