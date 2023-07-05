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
  @file wirtaskmanager.cc
  @brief This file implements %WIR task managers.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
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
  Default constructor creating an empty task manager.
*/
WIR_TaskManager::WIR_TaskManager( void ) :
  WIR_ID_API {},
  mSystemPointer { nullptr }
{
  DSTART( "WIR_TaskManager::WIR_TaskManager()" );
};


/*
  Copy constructor.

  When copying a task manager that is inserted in some %WIR system, the
  resulting copy will not be inserted in a system.
*/
WIR_TaskManager::WIR_TaskManager( const WIR_TaskManager &__o ) :
  WIR_ID_API { __o },
  mSystemPointer { nullptr }
{
  DSTART( "WIR_TaskManager::WIR_TaskManager(const WIR_TaskManager&)" );
};


/*
  Move constructor.

  Trying to move a task manager that is inserted in some %WIR system results in
  an assertion, since you are not allowed to move a task manager whose ownership
  is managed by a system.
*/
WIR_TaskManager::WIR_TaskManager( WIR_TaskManager &&__o ) :
  WIR_ID_API { move( __o ) },
  mSystemPointer { nullptr }
{
  DSTART( "WIR_TaskManager::WIR_TaskManager(WIR_TaskManager&&)" );

  ufAssertT(
    __o.mSystemPointer == nullptr,
    "Invalid attempt to move a task manager out of its owning system '" <<
    __o.getSystem().getName() << "'." );
};


/*
  Destructor.
*/
WIR_TaskManager::~WIR_TaskManager( void )
{
  DSTART( "virtual WIR_TaskManager::~WIR_TaskManager()" );
};


/*
  Copy-assignment operator.

  When copying a task manager that is inserted in some %WIR system, the
  resulting copy will not be inserted in a system.
*/
WIR_TaskManager & WIR_TaskManager::operator = ( const WIR_TaskManager &__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  (void) __o;

  mSystemPointer = nullptr;

  return( *this );
};


/*
  Move-assignment operator.

  Trying to move a task manager that is inserted in some %WIR system results in
  an assertion, since you are not allowed to move a task manager whose ownership
  is managed by a system.
*/
WIR_TaskManager & WIR_TaskManager::operator = ( WIR_TaskManager &&__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  ufAssertT(
    __o.mSystemPointer == nullptr,
    "Invalid attempt to move a task manager out of its owning system '" <<
    __o.getSystem().getName() << "'." );

  mSystemPointer = nullptr;

  return( *this );
};


//
// API implementations.
//

WIR_INSERTION_IMPL( WIR_System, System, WIR_TaskManager );


/*
  getEntryPoints returns all entry point flow facts attached to a WIR system.
*/
list<reference_wrapper<WIR_EntryPoint>> WIR_TaskManager::getEntryPoints( void ) const
{
  DSTART(
    "list<reference_wrapper<WIR_EntryPoint> > "
    "WIR_TaskManager::getEntryPoints() const" );

  ufAssertT(
    isInserted(),
    "Invalid attempt to access entry points via a task manager that is not "
    "attached to a WIR system." );

  return( mSystemPointer->getFlowFacts<WIR_EntryPoint>() );
};


/*
  getTaskEntries returns all basic blocks being task entries of a WIR system.
*/
WIR_BasicBlockSet WIR_TaskManager::getTaskEntries( void ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto entryPoints = getEntryPoints();
  WIR_BasicBlockSet res;

  for ( const WIR_EntryPoint &ep : entryPoints ) {
    auto &entryFunction = ep.getFunction();
    res.insert( *(entryFunction.begin()) );
  }

  return( res );
};


//
// Protected class methods
//

/*
  clone creates a copy of a WIR task manager.

  Clone just calls the corresponding copy constructor.
*/
WIR_TaskManager *WIR_TaskManager::clone( void ) const
{
  DSTART( "virtual WIR_TaskManager* WIR_TaskManager::clone() const" );

  return( new WIR_TaskManager( *this ) );
};

}       // namespace WIR
