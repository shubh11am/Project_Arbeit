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
  @file wirsystem.cc
  @brief This file implements the top level of an entire %WIR system.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include standard headers
#include <algorithm>
#include <cstddef>
#include <fstream>
#include <list>
#include <map>
#include <set>
#include <sstream>
#include <vector>

// Include boost headers
#include <boost/current_function.hpp>

// Include libuseful headers
#include <libuseful/debugmacros.h>
#include <libuseful/exceptions.h>
#include <libuseful/io.h>
#include <libuseful/stringtools.h>

// Include WIR headers
#include <wir/wir.h>
#include <wir/wirsystemconfig.h>


//
// Code section
//

namespace WIR {


using namespace std;


//
// Public class methods
//

/*
  Default constructor for WIR systems using the specified hardware configuration
  and a given task manager.

  Matching system names with configuration file paths is done according to the
  following rules:

  - If n already is a path (either absolute or relative), i.e., n contains at
    least one '/' somewhere, this input path n is directly returned. Note that
    it lies in the sole responsibility of the caller of this constructor that n
    is a valid path in this case!
  - Otherwise, getConfigPath probes the WIR-internal directly structure
    (archdir and sysconfdir) and checks whether a file called n exists below
    archdir or sysconfdir. If so, the full path to the found file is returned.
  - Otherwise, no valid file path was determined and an error message is issued.
*/
WIR_System::WIR_System( const std::string &n, const WIR_TaskManager &t ) :
  WIR_ID_API {},
  WIR_Container_API {},
  WIR_Name_API {},
  WIR_SymbolTable_API {},
  mSystemName { "" },
  mDontOptimize { false }
{
  DSTART( "WIR_System::WIR_System(const string&, const WIR_TaskManager&)" );

  string configFile = getConfigPath( n );
  if ( configFile == "" )
    throw
      ufFatalError(
        "Failed to find configuration file for architecture '" + n + "'.",
        false );

  createComponents( configFile );

  // We create a copy of the passed task manager depending on its actual type
  // and take over its exclusive ownership.
  mTaskManager.reset( t.clone() );
  (*mTaskManager).onInsert( this );
};


/*
  Copy constructor.
*/
WIR_System::WIR_System( const WIR_System &__o ) :
  WIR_ID_API { __o },
  WIR_Container_API { __o },
  WIR_Name_API { __o },
  WIR_SymbolTable_API { __o }
{
  DSTART( "WIR_System::WIR_System(const WIR_System&)" );

  ufAssertT(
    ( __o.mTaskManager.get() != nullptr ),
    "Invalid attempt to copy-construct a system that has already been moved!" );

  mTaskManager.reset( __o.mTaskManager->clone() );
  (*mTaskManager).onInsert( this );

  copySystem( __o );
};


/*
  Move constructor.
*/
WIR_System::WIR_System( WIR_System &&__o ) :
  WIR_ID_API { move( __o ) },
  WIR_Container_API { move( __o ) },
  WIR_Name_API { move( __o ) },
  WIR_SymbolTable_API { move( __o ) },
  mCompilationUnits { move( __o.mCompilationUnits ) },
  mCompilationUnitReferences { move( __o.mCompilationUnitReferences ) },
  mSystemName { move( __o.mSystemName ) },
  mComponentPointers { move( __o.mComponentPointers ) },
  mComponentReferences { move( __o.mComponentReferences ) },
  mComponentHierarchyLevels { move( __o.mComponentHierarchyLevels ) },
  mFlowFactPointers { move( __o.mFlowFactPointers ) },
  mFlowFactReferences { move( __o.mFlowFactReferences ) },
  mDontOptimize { __o.mDontOptimize },
  mWarnedRegions { move( __o.mWarnedRegions ) }
{
  DSTART( "WIR_System::WIR_System(WIR_System&&)" );

  ufAssertT(
    ( __o.mTaskManager.get() != nullptr ),
    "Invalid attempt to move-construct a system that has already been moved!" );

  __o.mCompilationUnits.clear();
  __o.mCompilationUnitReferences.clear();

  __o.mSystemName.clear();

  __o.mComponentPointers.clear();
  __o.mComponentReferences.clear();
  __o.mComponentHierarchyLevels.clear();

  __o.mFlowFactPointers.clear();
  __o.mFlowFactReferences.clear();

  __o.mWarnedRegions.clear();

  // Adjust the parent IDs of the system's compilation units.
  for ( WIR_CompilationUnit &c : mCompilationUnits )
    c.onInsert( this );

  // Adjust the parent IDs of the system's components.
  for ( WIR_SystemComponent &c : mComponentReferences )
    c.onInsert( this );

  // Adjust the parent Ids of the systems's flowfacts.
  for ( WIR_FlowFact &ff: mFlowFactReferences )
    ff.onInsert( this );

  mTaskManager = move( __o.mTaskManager );
  __o.mTaskManager.release();
  (*mTaskManager).onInsert( this );
};


/*
  Destructor.
*/
WIR_System::~WIR_System( void )
{
  DSTART( "virtual WIR_System::~WIR_System()" );

  clearFlowFacts();
  clearCompilationUnits();
};


/*
  Copy-assignment operator.
*/
WIR_System & WIR_System::operator = ( const WIR_System &__o )
{
  DSTART( "WIR_System& WIR_System::operator=(const WIR_System&)" );

  ufAssertT(
    ( __o.mTaskManager.get() != nullptr ),
    "Invalid attempt to copy-assign a system that has already been moved!" );

  WIR_Container_API::operator = ( __o );
  WIR_Name_API::operator = ( __o );
  WIR_SymbolTable_API::operator = ( __o );

  mTaskManager.reset( __o.mTaskManager->clone() );
  (*mTaskManager).onInsert( this );

  copySystem( __o );

  return( *this );
};


/*
  Move-assignment operator.
*/
WIR_System & WIR_System::operator = ( WIR_System &&__o )
{
  DSTART( "WIR_System& WIR_System::operator=(WIR_System&&)" );

  ufAssertT(
    ( __o.mTaskManager.get() != nullptr ),
    "Invalid attempt to move-assign a system that has already been moved!" );

  WIR_Container_API::operator = ( move( __o ) );
  WIR_Name_API::operator = ( move( __o ) );
  WIR_SymbolTable_API::operator = ( move( __o ) );
  mDontOptimize = __o.mDontOptimize;

  mCompilationUnits = move( __o.mCompilationUnits );
  __o.mCompilationUnits.clear();
  mCompilationUnitReferences = move( __o.mCompilationUnitReferences );
  __o.mCompilationUnitReferences.clear();

  mSystemName = move( __o.mSystemName );
  __o.mSystemName.clear();

  mComponentPointers = move( __o.mComponentPointers );
  __o.mComponentPointers.clear();
  mComponentReferences = move( __o.mComponentReferences );
  __o.mComponentReferences.clear();
  mComponentHierarchyLevels = move( __o.mComponentHierarchyLevels );
  __o.mComponentHierarchyLevels.clear();

  mFlowFactPointers = move( __o.mFlowFactPointers );
  __o.mFlowFactPointers.clear();
  mFlowFactReferences = move( __o.mFlowFactReferences );
  __o.mFlowFactReferences.clear();

  mWarnedRegions = move( __o.mWarnedRegions );
  __o.mWarnedRegions.clear();

  // Adjust the parent IDs of the system's compilation units.
  for ( WIR_CompilationUnit &c : mCompilationUnits )
    c.onInsert( this );

  // Adjust the parent IDs of the system's components.
  for ( WIR_SystemComponent &c : mComponentReferences )
    c.onInsert( this );

  // Adjust the parent IDs of the system's flowfacts.
  for ( WIR_FlowFact &ff : mFlowFactReferences )
    ff.onInsert( this );

  mTaskManager = move( __o.mTaskManager );
  __o.mTaskManager.release();
  (*mTaskManager).onInsert( this );

  return( *this );
};


/*
  getSystemName returns a %WIR system's architecture name as given by the
  hardware vendor.
*/
std::string WIR_System::getSystemName( void ) const
{
  DSTART( "string WIR_System::getSystemName() const" );

  return( mSystemName );
};


//
// API implementations: Compilation Units.
//

WIR_CompilationUnit & WIR_System::pushBackCompilationUnit( const WIR_CompilationUnit &o )
{
  DSTART(
    "WIR_CompilationUnit& WIR_System::pushBackCompilationUnit(const WIR_CompilationUnit&)" );

  checkDontOptimize();

  mCompilationUnits.push_back( o );
  mCompilationUnits.back().onInsert( this );
  mCompilationUnitReferences.push_back( mCompilationUnits.back() );

  insertSymbols( mCompilationUnits.back() );

  return( mCompilationUnits.back() );
};


WIR_CompilationUnit & WIR_System::pushBackCompilationUnit( WIR_CompilationUnit &&o )
{
  DSTART(
    "WIR_CompilationUnit& WIR_System::pushBackCompilationUnit(WIR_CompilationUnit&&)" );

  checkDontOptimize();

  mCompilationUnits.emplace_back( move( o ) );
  mCompilationUnits.back().onInsert( this );
  mCompilationUnitReferences.push_back( mCompilationUnits.back() );

  insertSymbols( mCompilationUnits.back() );

  return( mCompilationUnits.back() );
};


WIR_CompilationUnit & WIR_System::pushFrontCompilationUnit( const WIR_CompilationUnit &o )
{
  DSTART(
    "WIR_CompilationUnit& WIR_System::pushFrontCompilationUnit(const WIR_CompilationUnit&)" );

  checkDontOptimize();

  mCompilationUnits.push_front( o );
  mCompilationUnits.front().onInsert( this );
  mCompilationUnitReferences.push_front( mCompilationUnits.front() );

  insertSymbols( mCompilationUnits.front() );

  return( mCompilationUnits.front() );
};


WIR_CompilationUnit & WIR_System::pushFrontCompilationUnit( WIR_CompilationUnit &&o )
{
  DSTART(
    "WIR_CompilationUnit& WIR_System::pushFrontCompilationUnit(WIR_CompilationUnit&&)" );

  checkDontOptimize();

  mCompilationUnits.emplace_front( move( o ) );
  mCompilationUnits.front().onInsert( this );
  mCompilationUnitReferences.push_front( mCompilationUnits.front() );

  insertSymbols( mCompilationUnits.front() );

  return( mCompilationUnits.front() );
};


std::list<std::reference_wrapper<WIR_CompilationUnit>>::iterator WIR_System::insertCompilationUnit( std::list<std::reference_wrapper<WIR_CompilationUnit>>::const_iterator pos,
                                                                                                    const WIR_CompilationUnit &o )
{
  DSTART(
    "list<reference_wrapper<WIR_CompilationUnit> >::iterator WIR_System::insertCompilationUnit(list<reference_wrapper<WIR_CompilationUnit> >::const_iterator, const WIR_CompilationUnit&)" );

  checkDontOptimize();

  auto it = mCompilationUnits.begin();
  for ( auto itr = mCompilationUnitReferences.begin(); itr != pos;
        ++itr, ++it ) ;
  auto it1 = mCompilationUnits.insert( it, o );
  (*it1).onInsert( this );

  insertSymbols( *it1 );

  return( mCompilationUnitReferences.insert( pos, *it1 ) );
};


std::list<std::reference_wrapper<WIR_CompilationUnit>>::iterator WIR_System::insertCompilationUnit( std::list<std::reference_wrapper<WIR_CompilationUnit>>::const_iterator pos,
                                                                                                    WIR_CompilationUnit &&o )
{
  DSTART(
    "list<reference_wrapper<WIR_CompilationUnit> >::iterator WIR_System::insertCompilationUnit(list<reference_wrapper<WIR_CompilationUnit> >::const_iterator, WIR_CompilationUnit&&)" );

  checkDontOptimize();

  auto it = mCompilationUnits.begin();
  for ( auto itr = mCompilationUnitReferences.begin(); itr != pos;
        ++itr, ++it ) ;
  auto it1 = mCompilationUnits.insert( it, move( o ) );
  (*it1).onInsert( this );

  insertSymbols( *it1 );

  return( mCompilationUnitReferences.insert( pos, *it1 ) );
};


std::list<std::reference_wrapper<WIR_CompilationUnit>>::iterator WIR_System::replaceCompilationUnit( std::list<std::reference_wrapper<WIR_CompilationUnit>>::const_iterator pos,
                                                                                                     const WIR_CompilationUnit &o )
{
  DSTART(
    "list<reference_wrapper<WIR_CompilationUnit> >::iterator WIR_System::replaceCompilationUnit(list<reference_wrapper<WIR_CompilationUnit> >::const_iterator, const WIR_CompilationUnit&)" );

  checkDontOptimize();

  auto it1 = mCompilationUnits.begin();
  for ( auto itr = mCompilationUnitReferences.begin(); itr != pos;
        ++itr, ++it1 ) ;
  auto it2 = mCompilationUnits.insert( it1, o );
  (*it2).onInsert( this );

  insertSymbols( *it2 );

  auto it = mCompilationUnitReferences.insert( pos, *it2 );
  mCompilationUnitReferences.erase( pos );
  mCompilationUnits.erase( it1 );
  return( it );
};


std::list<std::reference_wrapper<WIR_CompilationUnit>>::iterator WIR_System::replaceCompilationUnit( std::list<std::reference_wrapper<WIR_CompilationUnit>>::const_iterator pos,
                                                                                                     WIR_CompilationUnit &&o )
{
  DSTART(
    "list<reference_wrapper<WIR_CompilationUnit> >::iterator WIR_System::replaceCompilationUnit(list<reference_wrapper<WIR_CompilationUnit> >::const_iterator, WIR_CompilationUnit&&)" );

  checkDontOptimize();

  auto it1 = mCompilationUnits.begin();
  for ( auto itr = mCompilationUnitReferences.begin(); itr != pos;
        ++itr, ++it1 ) ;
  auto it2 = mCompilationUnits.insert( it1, move( o ) );
  (*it2).onInsert( this );

  insertSymbols( *it2 );

  auto it = mCompilationUnitReferences.insert( pos, *it2 );
  mCompilationUnitReferences.erase( pos );
  mCompilationUnits.erase( it1 );
  return( it );
};


void WIR_System::popBackCompilationUnit( void )
{
  DSTART( "void WIR_System::popBackCompilationUnit()" );

  checkDontOptimize();

  if ( mCompilationUnitReferences.empty() )
    return;

  mCompilationUnitReferences.pop_back();
  mCompilationUnits.pop_back();
};


void WIR_System::popFrontCompilationUnit( void )
{
  DSTART( "void WIR_System::popFrontCompilationUnit()" );

  checkDontOptimize();

  if ( mCompilationUnitReferences.empty() )
    return;

  mCompilationUnitReferences.pop_front();
  mCompilationUnits.pop_front();
};


std::list<std::reference_wrapper<WIR_CompilationUnit>>::iterator WIR_System::eraseCompilationUnit( std::list<std::reference_wrapper<WIR_CompilationUnit>>::const_iterator pos )
{
  DSTART(
    "list<reference_wrapper<WIR_CompilationUnit> >::iterator WIR_System::eraseCompilationUnit(list<reference_wrapper<WIR_CompilationUnit> >::const_iterator)" );

  checkDontOptimize();

  auto it1 = mCompilationUnits.begin();
  for ( auto itr = mCompilationUnitReferences.begin(); itr != pos;
        ++itr, ++it1 ) ;
  auto it = mCompilationUnitReferences.erase( pos );
  mCompilationUnits.erase( it1 );
  return( it );
};


void WIR_System::clearCompilationUnits( void )
{
  DSTART( "void WIR_System::clearCompilationUnits()" );

  checkDontOptimize();

  mCompilationUnitReferences.clear();
  mCompilationUnits.clear();
};


const std::list<std::reference_wrapper<WIR_CompilationUnit>> &WIR_System::getCompilationUnits( void ) const
{
  DSTART(
    "const list<reference_wrapper<WIR_CompilationUnit> >& WIR_System::getCompilationUnits() const" );

  return( mCompilationUnitReferences );
};


/*
  begin returns an iterator to the first compilation unit of a system.
*/
std::list<std::reference_wrapper<WIR_CompilationUnit>>::const_iterator WIR_System::begin( void ) const
{
  DSTART(
    "list<reference_wrapper<WIR_CompilationUnit> >::const_iterator WIR_System::begin() const" );

  return( mCompilationUnitReferences.begin() );
};


/*
  end returns an iterator to the end of a function's basic block list.
*/
std::list<std::reference_wrapper<WIR_CompilationUnit>>::const_iterator WIR_System::end( void ) const
{
  DSTART(
    "list<reference_wrapper<WIR_CompilationUnit> >::const_iterator WIR_System::end() const" );

  return( mCompilationUnitReferences.end() );
};


bool WIR_System::containsCompilationUnit( WIR_id_t id ) const
{
  DSTART( "bool WIR_System::containsCompilationUnit(WIR_id_t) const" );

  for ( auto &item : mCompilationUnits )
    if ( item.getID() == id )
      return( true );

  return( false );
};


bool WIR_System::containsCompilationUnit( const WIR_CompilationUnit &o ) const
{
  DSTART(
    "bool WIR_System::containsCompilationUnit(const WIR_CompilationUnit&) const" );

  for ( auto &item : mCompilationUnits )
    if ( item == o )
      return( true );

  return( false );
};


std::list<std::reference_wrapper<WIR_CompilationUnit>>::const_iterator WIR_System::findCompilationUnit( WIR_id_t id ) const
{
  DSTART(
    "list<reference_wrapper<WIR_CompilationUnit> >::const_iterator WIR_System::findCompilationUnit(WIR_id_t) const" );

  for ( auto it = mCompilationUnitReferences.begin();
        it != mCompilationUnitReferences.end(); ++it )
    if ( (*it).get().getID() == id )
      return( it );

  return( mCompilationUnitReferences.end() );
};


std::list<std::reference_wrapper<WIR_CompilationUnit>>::const_iterator WIR_System::findCompilationUnit( const WIR_CompilationUnit &o ) const
{
  DSTART(
    "list<reference_wrapper<WIR_CompilationUnit> >::const_iterator WIR_System::findCompilationUnit(const WIR_CompilationUnit&) const" );

  for ( auto it = mCompilationUnitReferences.begin();
        it != mCompilationUnitReferences.end(); ++it )
    if ( (*it).get() == o )
      return( it );

  return( mCompilationUnitReferences.end() );
};


/*
  getComponents returns the set mComponentReferences.
*/
const WIR_SystemComponentSet &WIR_System::getComponents( void ) const
{
  DSTART( "const WIR_SystemComponentSet& WIR_System::getComponents() const" );

  return( mComponentReferences );
};


/*
  getComponentHierarchyLevels returns a map that groups together all
  cores/caches/buses of a system's component hierarchy in sets that lie on the
  very same hierarchy level.

  The map returned by getComponentHierarchyLevels can be queried with a number
  of a memory hierarchy level, with 0 being the level closest to the system's
  processor cores.
*/
const std::map<unsigned int, WIR_SystemComponentSet> &WIR_System::getComponentHierarchyLevels( void ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  return( mComponentHierarchyLevels );
};


/*
  findComponent finds a WIR_SystemComponent with the specified ID in set
  mComponentReferences.
*/
WIR_SystemComponentSet::const_iterator WIR_System::findComponent( WIR_id_t id ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  for ( auto it = mComponentReferences.begin();
        it != mComponentReferences.end(); ++it )
    if ( it->get().getID() == id )
      return( it );

  return( mComponentReferences.end() );
};


/*
  findComponent finds a WIR_SystemComponent with the specified name in set
  mComponentReferences.
*/
WIR_SystemComponentSet::const_iterator WIR_System::findComponent( const std::string &n ) const
{
  DSTART(
    "set<reference_wrapper<WIR_SystemComponent>, WIR_Compare<WIR_SystemComponent> >::const_iterator WIR_System::findComponent(const string&) const" );

  for ( auto it = mComponentReferences.begin();
        it != mComponentReferences.end(); ++it )
    if ( it->get().getName() == n )
      return( it );

  return( mComponentReferences.end() );
};


/*
  containsComponent returns whether list mComponentReferences contains a
  WIR_SystemComponent with the specified ID.
*/
bool WIR_System::containsComponent( WIR_id_t id ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  for ( WIR_SystemComponent &item : mComponentReferences )
    if ( item.getID() == id )
      return( true );

  return( false );
};


/*
  containsComponent returns whether list mComponentReferences contains a
  WIR_SystemComponent with the specified name.
*/
bool WIR_System::containsComponent( const std::string &n ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  for ( WIR_SystemComponent &item : mComponentReferences )
    if ( item.getName() == n )
      return( true );

  return( false );
};


/*
  findMemoryRegion finds a WIR_MemoryRegion in set mComponentReferences that
  includes the specified address.
*/
WIR_SystemComponentSet::const_iterator WIR_System::findMemoryRegion( const WIR_MemoryAddress &a ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  for ( auto it = mComponentReferences.begin();
        it != mComponentReferences.end(); ++it )
    if ( it->get().getType() == WIR_SystemComponentType::memory ) {
      const WIR_MemoryRegion &r =
        dynamic_cast<const WIR_MemoryRegion &>( it->get() );

      if ( ( r.getBaseAddress() <= a ) &&
           ( a < r.getBaseAddress() + r.getLength() ) )
        return( it );
    }

  return( mComponentReferences.end() );
};


/*
  findMemoryRegions finds all memory regions that intersect with the given
  address range.
*/
WIR_SystemComponentSet WIR_System::findMemoryRegions( const WIR_AddressRange &a ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  WIR_SystemComponentSet res;

  for ( WIR_SystemComponent &m : mComponentReferences )
    if ( m.getType() == WIR_SystemComponentType::memory ) {
      WIR_MemoryRegion &r =
        dynamic_cast<WIR_MemoryRegion &>( m );
      WIR_AddressRange range { r.getBaseAddress(),
                               r.getBaseAddress() + r.getLength() - 1 };

      if ( !a.hasEmptyIntersection( range ) )
        res.insert( r );
    }

  return( res );
};


/*
  getFlowFacts returns the set mFlowFactReferences.
*/
const std::list<std::reference_wrapper<WIR_FlowFact>> &WIR_System::getFlowFacts( void ) const
{
  DSTART(
    "const std::list<std::reference_wrapper<WIR_FlowFact>>& "
    "WIR_System::getFlowFacts() const" );

  return( mFlowFactReferences );
};


/*
  pushBackFlowFact adds a new WIR_FlowFact at the end of lists mFlowFactPointers
  and mFlowFactReferences.

  The content of f is copied to the new list element.
*/
WIR_FlowFact &WIR_System::pushBackFlowFact( const WIR_FlowFact &f )
{
  DSTART( "WIR_FlowFact& WIR_System::pushBackFlowFact(const WIR_FlowFact&)" );

  WIR_FlowFact *ff_ptr = f.clone();

  mFlowFactReferences.push_back( ref( *ff_ptr ) );
  mFlowFactPointers.push_back( unique_ptr<WIR_FlowFact>( ff_ptr ) );

  // Enable automatic handling by ref collections.
  ff_ptr->onInsert( this );

  return( *ff_ptr );
};


/*
  eraseFlowFact removes a single flow fact from the specified position.

  This destroys the removed element.
*/
void WIR_System::eraseFlowFact( std::list<std::reference_wrapper<WIR_FlowFact>>::const_iterator pos )
{
  DSTART(
    "void WIR_System::eraseFlowFact(list<reference_wrapper<WIR::WIR_FlowFact>>"
    "::const_iterator)" );

  // Advance an iterator of the pointer list to the same position.
  auto ptr_it = mFlowFactPointers.begin();
  advance( ptr_it, distance( mFlowFactReferences.cbegin(), pos ) );

  if ( ptr_it != mFlowFactPointers.end() ) {
    mFlowFactReferences.erase( pos );
    mFlowFactPointers.erase( ptr_it );
  }
};


/*
  eraseFlowFact erases a single flow fact with the specified ID.

  This destroys the removed element.
*/
void WIR_System::eraseFlowFact( WIR_id_t id )
{
  DSTART( "void WIR_System::eraseFlowFact(WIR::WIR_id_t)" );

  auto ptr_it =
    find_if(
      mFlowFactPointers.begin(), mFlowFactPointers.end(),
      [id]( const auto &ptr ) { return( id == ptr->getID() ); } );
  auto ref_it =
    find_if(
      mFlowFactReferences.begin(), mFlowFactReferences.end(),
      [id]( const auto &ref ) { return( id == ref.get().getID() ); } );

  if ( ptr_it != mFlowFactPointers.end() ) {
    mFlowFactReferences.erase( ref_it );
    mFlowFactPointers.erase( ptr_it );
  }
};


/*
  containsFlowFact returns whether the list mFlowFactReferences contains a flow
  fact with the specified ID.
*/
bool WIR_System::containsFlowFact( WIR_id_t id ) const
{
  DSTART( "bool WIR_System::containsFlowFact(WIR_id_t) const" );

  return(
    mFlowFactReferences.end() !=
    find_if(
      mFlowFactReferences.begin(), mFlowFactReferences.end(),
      [id]( const WIR_FlowFact &f ) { return( f.getID() == id ); } ) );
};


/*
  clearFlowFacts removes all elements from lists mFlowFactPointers and
  mFlowFactReferences.
*/
void WIR_System::clearFlowFacts( void )
{
  DSTART( "void WIR_System::clearFlowFacts()" );

  // Clear sets. This destroys all managed flow facts.
  mFlowFactReferences.clear();
  mFlowFactPointers.clear();
};


/*
  getTaskManager returns the %WIR task manager associated with this WIR system.
*/
WIR_TaskManager &WIR_System::getTaskManager( void ) const
{
  DSTART( "WIR_TaskManager& WIR_System::getTaskManager() const" );

  return( *mTaskManager );
};


/*
  setDontOptimize sets whether a WIR system can be modified or must not be
  changed by some optimization or transformation.
*/
void WIR_System::setDontOptimize( bool f )
{
  DSTART( "void WIR_System::setDontOptimize(bool)" );

  mDontOptimize = f;
};


/*
  getDontOptimize returns whether a WIR system can be modified or must not
  be changed by some optimization or transformation.
*/
bool WIR_System::getDontOptimize( void ) const
{
  DSTART( "bool WIR_System::getDontOptimize() const" );

  return( mDontOptimize );
};


/*
  The << operator dumps a WIR system to an output stream.

  By applying processor-specific I/O manipulators to the output stream
  beforehand, this << operator can flexibly emit valid assembly output for
  arbitrary processor architectures.
*/
std::ostream & operator << ( std::ostream &os, const WIR_System &s )
{
  DSTART( "ostream& operator<<(ostream&, const WIR_System&)" );

  // Dump a system's code or its linker script, depending on the ldscript I/O
  // manipulator.
  if ( os.iword( WIR_LdScript() ) == 0 )
    WIR_Registry::getSystemDumper( os.iword( WIR_ProcessorIO() ) )( os, s );
  else
    WIR_Registry::getLdScriptDumper( os.iword( WIR_ProcessorIO() ) )( os, s );

  return( os );
};


//
// Private class methods
//

/*
  copySystem performs actions common to the copy constructor and copy assignment
  operator of WIR systems.
*/
void WIR_System::copySystem( const WIR_System &__o )
{
  DSTART( "void WIR_System::copySystem(const WIR_System&)" );

  mDontOptimize = false;
  mSystemName = __o.mSystemName;

  // Copy component hierarchy.
  map<WIR_id_t, WIR_SystemComponent *> componentIDMap;
  map<WIR_id_t, WIR_Section *> sectionIDMap;

  // Copy system components, fill map oldComponentIDs -> newComponents.
  clearComponents();
  for ( WIR_SystemComponent &c : __o.mComponentReferences )
    componentIDMap[ c.getID() ] = &(insertComponent( c ));

  // Fill map oldSectionIDs -> newSectionIDs.
  auto cit = mComponentReferences.begin();
  for ( auto cit1 = __o.mComponentReferences.begin();
        cit1 != __o.mComponentReferences.end(); ++cit1, ++cit ) {
    #ifdef FAILSAFEMODE
    ufAssert( cit1->get().getName() == cit->get().getName() );
    #endif

    if ( cit1->get().getType() == WIR_SystemComponentType::core ) {
      WIR_BaseProcessor &origCore =
        dynamic_cast<WIR_BaseProcessor &>( cit1->get() );
      WIR_BaseProcessor &newCore =
        dynamic_cast<WIR_BaseProcessor &>( cit->get() );

      auto sit = newCore.getSections().begin();
      for ( auto sit1 = origCore.getSections().begin();
            sit1 != origCore.getSections().end(); ++sit1, ++sit ) {
        #ifdef FAILSAFEMODE
        ufAssert( sit1->get().getName() == sit->get().getName() );
        #endif

        sectionIDMap[ sit1->get().getID() ] = &(sit->get());
      }
    }
  }

  for ( auto p : __o.mComponentHierarchyLevels )
    for ( WIR_SystemComponent &m : p.second ) {
      mComponentHierarchyLevels[ p.first ].insert(
        *componentIDMap[ m.getID() ] );
    }

  for ( WIR_SystemComponent &c : __o.mComponentReferences )
    if ( c.getType() == WIR_SystemComponentType::memory ) {
      WIR_MemoryRegion &o = dynamic_cast<WIR_MemoryRegion &>( c );
      WIR_MemoryRegion &m =
        dynamic_cast<WIR_MemoryRegion &>( *(componentIDMap[ o.getID() ] ) );

      // Adjust WIR_MemoryRegion::mHierarchyLevels.
      m.mHierarchyLevels.clear();

      for ( auto &l : o.getHierarchy() ) {
        list<reference_wrapper<WIR_SystemComponent>> hierarchy;
        for ( WIR_SystemComponent &oc : l )
          hierarchy.push_back( *(componentIDMap[ oc.getID() ] ) );
        m.insertHierarchy( move( hierarchy ) );
      }

      // Adjust WIR_MemoryRegion::mSections.
      WIR_SectionSet newSections;
      for ( WIR_Section &s : m.mSections )
        newSections.insert( *(sectionIDMap[ s.getID() ]) );
      m.mSections = move( newSections );
    }

  // Adjust WIR_Section::mRegion and WIR_Section::mLoadRegion.
  for ( WIR_BaseProcessor &c : getComponents<WIR_BaseProcessor>() )
    for ( WIR_Section &s : c ) {
      s.mRegion =
        dynamic_cast<WIR_MemoryRegion *>(
          componentIDMap[ (s.mRegion)->getID() ] );

      if ( s.mLoadRegion != nullptr )
        s.mLoadRegion =
          dynamic_cast<WIR_MemoryRegion *>(
            componentIDMap[ (s.mLoadRegion)->getID() ] );
    }

  // Copy flow facts.

  // Sort the IDs of flow facts from __o.
  vector<WIR_id_t> originalFlowfactIDs;
  map<WIR_id_t, WIR_FlowFact *> oldFlowfactIDMap;
  for ( WIR_FlowFact &ff : __o.mFlowFactReferences ) {
    originalFlowfactIDs.push_back( ff.getID() );
    oldFlowfactIDMap[ ff.getID() ] = &ff;
  }
  sort( originalFlowfactIDs.begin(), originalFlowfactIDs.end() );

  // Copy flow facts according to the sorted order.
  map<WIR_id_t, WIR_FlowFact *> flowfactIDMap;
  list<WIR_FlowFact *> newFlowfacts;
  for ( auto id : originalFlowfactIDs ) {
    newFlowfacts.push_back( oldFlowfactIDMap[ id ]->clone() );
    flowfactIDMap[ id ] = newFlowfacts.back();
    //newFlowfacts.back()->onInsert( this );
  }

  // Insert flow facts into this system in the right order.
  clearFlowFacts();
  for ( WIR_FlowFact &ff : __o.mFlowFactReferences ) {
    mFlowFactReferences.push_back( ref( *(flowfactIDMap[ ff.getID() ]) ) );
    mFlowFactPointers.emplace_back( flowfactIDMap[ ff.getID() ] );
  }


  // Copy compilation units.

  // Sort the IDs of compilation units from __o.
  vector<WIR_id_t> originalIDs;
  map<WIR_id_t, WIR_CompilationUnit *> oldCompilationUnitIDMap;
  for ( WIR_CompilationUnit &c : __o.mCompilationUnitReferences ) {
    originalIDs.push_back( c.getID() );
    oldCompilationUnitIDMap[ c.getID() ] = &c;
  }
  sort( originalIDs.begin(), originalIDs.end() );

  // Copy compilation units according to the sorted order.
  map<WIR_id_t, WIR_CompilationUnit *> compilationUnitIDMap;
  list<WIR_CompilationUnit> newCompilationUnits;
  for ( auto id : originalIDs ) {
    newCompilationUnits.push_back( *(oldCompilationUnitIDMap[ id ] ) );
    compilationUnitIDMap[ id ] = &(newCompilationUnits.back());
  }

  // Insert copied compilation units into this system in the right order.
  clearCompilationUnits();
  for ( WIR_CompilationUnit &c : __o.mCompilationUnitReferences )
    pushBackCompilationUnit( move( *(compilationUnitIDMap[ c.getID() ]) ) );

  // Patch all function label parameters of copied operations.
  // First, build a map mapping the IDs of original functions to the IDs of
  // copied basic functions.
  // Also reorganize the basic block references of all Flowfacts.
  // To do that, build a map mapping the IDs of original basic blocks to the
  // copied basic blocks.
  map<WIR_id_t, WIR_Function *> functionIDMap;
  map<WIR_id_t, WIR_BasicBlock *> blockIDMap;

  auto origUnitIt = __o.begin();
  auto newUnitIt = this->begin();

  for ( ; origUnitIt != __o.end();
        ++origUnitIt, ++newUnitIt ) {
    auto origFunctionIt = (*origUnitIt).get().getFunctions().begin();
    auto newFunctionIt = (*newUnitIt).get().getFunctions().begin();

    for ( ; origFunctionIt != (*origUnitIt).get().getFunctions().end();
          ++origFunctionIt, ++newFunctionIt ) {
      functionIDMap[ (*origFunctionIt).get().getID() ] =
        &(newFunctionIt->get());

      auto origBlockIt = (*origFunctionIt).get().getBasicBlocks().begin();
      auto newBlockIt = (*newFunctionIt).get().getBasicBlocks().begin();

      for ( ; origBlockIt != (*origFunctionIt).get().getBasicBlocks().end();
            ++origBlockIt, ++newBlockIt ) {
        blockIDMap[ (*origBlockIt).get().getID() ] =
          &(newBlockIt->get());
      }
    }
  }

  // Scan all function label parameters of the copied operations and replace
  // them by the copied function labels.
  for ( WIR_CompilationUnit &c : getCompilationUnits() )
    for ( WIR_Function &f : c )
      for ( WIR_BasicBlock &b : f )
        for ( WIR_Instruction &i : b )
          for ( WIR_Operation &o : i )
            for ( auto it = o.getParameters().begin();
                  it != o.getParameters().end(); ++it ) {
              auto &p = (*it).get();

              if ( p.getType() == WIR_ParameterType::label ) {
                auto &labP = dynamic_cast<WIR_LabelParameter &>( p );

                if ( labP.getLabelType() == WIR_SymbolType::function ) {
                  WIR_Function &fct = labP.getFunction();

                  if ( functionIDMap.count( fct.getID() ) )
                    // Replace function label.
                    it =
                      o.replaceParameter(
                        it,
                        WIR_LabelParameter( *(functionIDMap[ fct.getID() ]) ) );
                }
              }
            }

  // Reorganize all copied Flowfacts references to basic blocks
  for ( WIR_FlowFact &ff : mFlowFactReferences ) {
    //ff.onInsert( this );
    ff.reorganize( blockIDMap );
    ff.onInsert( this );
  }

  mDontOptimize = __o.mDontOptimize;
};


/*
  checkDontOptimize checks whether a WIR system must not be modified.

  If this system must not be modified, checkDontOptimize asserts.
*/
void WIR_System::checkDontOptimize( void ) const
{
  DSTART( "void WIR_System::checkDontOptimize() const" );

  ufAssertT(
    !getDontOptimize(),
    "Illegal attempt to modify a system that is set as 'don't optimize'!" );
};


/*
  setSystemName sets a %WIR system's architecture name as given by the hardware
  vendor.
*/
void WIR_System::setSystemName( const std::string &s )
{
  DSTART( "void WIR_System::setSystemName(const string&)" );

  checkDontOptimize();
  mSystemName = s;
};


/*
  For the given name of system configuration, getConfigPath determines the file
  path to the corresponding system configuration file.

  Matching system names with configuration file paths is done according to
  the following rules:

  - If n already is a path (either absolute or relative), i.e., n contains at
    least one '/' somewhere, this input path n is directly returned. Note that
    it lies in the sole responsibility of the caller of getConfigPath that n is
    a valid path in this case!
  - Otherwise, getConfigPath probes the WIR-internal directly structure (archdir
    and sysconfdir) and checks whether a file called n exists below archdir or
    sysconfdir. If so, the full path to the found file is returned.
  - Otherwise, no valid file path was determined and getConfigPath returns the
    empty string.
*/
string WIR_System::getConfigPath( const std::string &n )
{
  DSTART( "string WIR_System::getConfigPath(const string&)" );

  if ( n.find( "/" ) != string::npos )
    // n already is a path, so we return it.
    return( stripPath( trim( n ) ) );

  string archdir { ARCHDIR };
  string sysconfdir { SYSCONFDIR };
  string archs { ARCHS };

  // Attach all enabled architectures from archs to the paths sysconfdir and
  // archdir.
  list<string> pathsToProbe;
  stringstream sstr( archs );
  string a;
  sstr >> a;
  list<string>::iterator pos = pathsToProbe.end();

  while ( a != "" ) {

    pathsToProbe.push_back( archdir + "/" + a );
    if ( pos == pathsToProbe.end() )
      --pos;
    pathsToProbe.insert( pos, sysconfdir + "/" + a );

    a = "";
    sstr >> a;
  }

  for ( auto &s : pathsToProbe ) {
    // Try to open file at path s/n.
    string fullPath = s + "/" + n;

    DOUT( "Testing configuration file '" << fullPath << "'." << endl );
    ifstream ifs( fullPath );
    bool fail = ifs.fail();
    ifs.close();

    if ( !fail )
      return( fullPath );
  }

  return( "" );
};


/*
  createComponents reads a system's component hierarchy from a configuration
  file and creates an according set of WIR_SystemComponent objects.
*/
void WIR_System::createComponents( const std::string &f )
{
  DSTART( "void WIR_System::createComponents(const string&)" );

  checkDontOptimize();
  mComponentHierarchyLevels.clear();

  // paragraphNames holds the names of all processed paragraphs of the system
  // configuration file in order to detect invalid duplicate names.
  set<string> paragraphNames;

  // loadSectionNames stores for each memory region the names of sections that
  // are loaded from the region at a system's boot time.
  map<reference_wrapper<WIR_MemoryRegion>, set<string>, WIR_Compare<WIR_MemoryRegion>> loadSectionNames;

  // Read configuration file.
  DOUT( "Reading system configuration file '" << f << "'." << endl );
  WIR_SystemConfig configFile( f );

  // This lambda helps to detect multiple paragraphs with the same name.
  auto checkDuplicateNames = [&]( const WIR_SystemConfig::Paragraph &p ) {
    if ( paragraphNames.count( p.getName() ) )
      throw
        ufFatalError(
          f, p.getLine(),
          "Multiple components with same name '" + p.getName() + "' specified.",
          false );

    paragraphNames.insert( p.getName() );
  };

  // Process cores first, buses and caches next, memory regions only in the
  // final third round.
  for ( unsigned int round = 1; round <= 4; ++round ) {
    // Create components for each paragraph of the configuration file.
    for ( const WIR_SystemConfig::Paragraph &p : configFile.getParagraphs() ) {
      // Ensure that a correct type is given for the current config file
      // paragraph.
      if ( !p.isTypeSet() )
        throw
          ufFatalError(
            f, p.getLine(),
            "Type of component '" + p.getName() + "' not specified.", false );

      if ( ( round == 1 ) &&
           ( p.getType() == WIR_SystemComponentType::system ) ) {
        setSystemName( p.getName() );
        DOUT(
          "Setting system architecture name to '" << getSystemName() << "'." <<
          endl );
      }

      if ( ( round == 1 ) &&
           ( p.getType() == WIR_SystemComponentType::core ) ) {
        // Extract the core's ISA.
        string isa;
        unsigned int line = 0;

        // Iterate over all key-value properties of the current paragraph.
        for ( auto prop : p.getProperties() )
          if ( prop.first == "isa" ) {
            isa = prop.second.value;
            line = prop.second.line;
          }

        if ( isa.empty() )
          throw
            ufFatalError(
              f, p.getLine(),
              "No ISA specified for core '" + p.getName() + "'.", false );

        if ( !WIR_Registry::isProcessorRegistered( isa ) )
          throw
            ufFatalError(
              f, line,
              "Core '" + p.getName() + "' refers to unknown ISA '" + isa + "'.",
              false );

        // Create new processor core.
        WIR_BaseProcessor *c = WIR_Registry::getNewProcessor( isa );
        c->setName( p.getName() );

        mComponentPointers.insert( unique_ptr<WIR_SystemComponent>( c ) );
        c->onInsert( this );
        mComponentReferences.insert( *c );

        // Check for duplicate section names.
        checkDuplicateNames( p );
        DOUT( "Created new processor core '" << c->getName() << "'" << endl );
        DOUT(
          "  Instruction set architecture: '" << c->getISAName() << "'" <<
          endl );

        // Iterate over all key-value properties of the current paragraph.
        for ( auto prop : p.getProperties() ) {
          string key = prop.first;
          string value = prop.second.value;
          unsigned int line = prop.second.line;

          if ( key == "clockfreq" ) {
            c->setClockFrequency( toLong( value ) );
            DOUT(
              "  Clock frequency: " << c->getClockFrequency() << " Hz" <<
              endl );
          } else

          if ( key == "voltage" ) {
            c->setVoltage( toDouble( value ) );
            DOUT( "  Voltage: " << c->getVoltage() << " V" << endl );
          } else

          if ( key != "isa" )
            ufWarnMsg << ufFile( f, line ) << "Currently unsupported core "
                      << "attribute " << "'" + key + "'" << " found, "
                      << "ignoring." << endl;
        }
      }

      if ( ( round == 2 ) &&
           ( ( p.getType() == WIR_SystemComponentType::bus ) ||
             ( p.getType() == WIR_SystemComponentType::cache ) ) ) {
        if ( p.getType() == WIR_SystemComponentType::bus ) {
          // Determine the buses arbitration policy.
          WIR_BusArbitrationType arbitration;
          bool arbitrationSet = false;

          for ( auto prop : p.getProperties() )
            if ( prop.first == "arbitration" ) {
              if ( prop.second.value == "fp" )
                arbitration = WIR_BusArbitrationType::fp;
              else

              if ( prop.second.value == "pd" )
                arbitration = WIR_BusArbitrationType::pd;
              else

              if ( prop.second.value == "rr" )
                arbitration = WIR_BusArbitrationType::rr;
              else

              if ( prop.second.value == "tdma" )
                arbitration = WIR_BusArbitrationType::tdma;
              else
                throw
                  ufFatalError(
                    f, prop.second.line,
                    "Invalid arbitration policy '" + prop.second.value +
                      "' specified for bus '" + p.getName() + "'.",
                    false );

              arbitrationSet = true;
            }

          if ( !arbitrationSet )
            throw
              ufFatalError(
                f, p.getLine(),
                "No arbitration policy specified for bus '" + p.getName() +
                  "'.",
                false );

          // A small lambda to check duplicates and dump some debug output.
          auto checkBus = [&]( const WIR_Bus &b ) {
            (void) b;
            checkDuplicateNames( p );
            DOUT( "Created new bus '" << b.getName() << "'" << endl );
            DOUT(
              "  Arbitration policy: " <<
              string(
                b.getArbitrationType() == WIR_BusArbitrationType::fp ?
                  "Fixed priority" :
                  ( b.getArbitrationType() == WIR_BusArbitrationType::pd ?
                    "Priority division" :
                    ( b.getArbitrationType() == WIR_BusArbitrationType::rr ?
                      "Round robin" : "TDMA" ) ) ) << endl );
          };

          switch ( arbitration ) {

            case WIR_BusArbitrationType::fp: {
              WIR_FixedPriorityBusArbitration arb(
                getComponents<WIR_BaseProcessor>().size() );

              // Iterate over all key-value properties of the current paragraph.
              for ( auto prop : p.getProperties() ) {
                string key = prop.first;
                string value = prop.second.value;
                unsigned int line = prop.second.line;

                if ( key == "priorities" ) {
                  set<WIR_id_t> addedCores;
                  WIR_FixedPriorityBusArbitration::Priority currPrio =
                    arb.getNumberOfCores();
                  stringstream sstr( value );
                  string level;
                  sstr >> level;

                  while ( level != "" ) {
                    auto it = findComponent( level );

                    if ( it == getComponents().end() )
                      throw
                        ufFatalError(
                          f, line,
                          "Unknown system component '" + level +
                            "' specified for fixed-priority bus '" +
                            p.getName() + "'.",
                          false );

                    WIR_SystemComponent &comp = *it;

                    // Only processor cores are allowed in a priorities
                    // directive.
                    if ( comp.getType() != WIR_SystemComponentType::core )
                      throw
                        ufFatalError(
                          f, line,
                          "Invalid non-core component '" + level +
                            "' specified for fixed-priority bus '" +
                            p.getName() + "'.",
                          false );

                    arb.setPriority(
                      dynamic_cast<WIR_BaseProcessor &>( comp ), currPrio-- );
                    addedCores.insert( comp.getID() );

                    level = "";
                    sstr >> level;
                  }

                  // Set priorities of all unspecified cores to 0.
                  for ( WIR_BaseProcessor &core :
                          getComponents<WIR_BaseProcessor>() )
                    if ( !addedCores.count( core.getID() ) )
                      arb.setPriority( core, 0 );
                } else

                if ( key != "arbitration" )
                  ufWarnMsg << ufFile( f, line ) << "Currently unsupported "
                            << "fixed-priority bus attribute "
                            << "'" + key + "'" << " found, ignoring." << endl;
              }

              // Create new bus.
              WIR_Bus &b =
                dynamic_cast<WIR_Bus &>(
                  insertComponent( WIR_Bus( p.getName(), arb ) ) );

              // Check created bus.
              checkBus( b );

              break;
            }

            case WIR_BusArbitrationType::pd: {
              unsigned int arbitrationDelay = 0;

              // Check whether PD arbitration delay is specified.
              for ( auto prop : p.getProperties() )
                if ( prop.first == "arbitrationdelay" )
                  arbitrationDelay = toLong( prop.second.value );

              WIR_PriorityDivisionBusArbitration arb(
                getComponents<WIR_BaseProcessor>().size(), 0,
                arbitrationDelay );

              // Iterate over all key-value properties of the current paragraph.
              for ( auto prop : p.getProperties() ) {
                string key = prop.first;
                string value = prop.second.value;
                unsigned int line = prop.second.line;

                if ( key == "slot" ) {
                  stringstream sstr( value );
                  string type;
                  sstr >> type;

                  if ( ( type == "pd" ) || ( type == "tdma" ) ) {
                    string coreName;
                    string slotLength;

                    sstr >> coreName;

                    if ( coreName != "" )
                      sstr >> slotLength;

                    if ( slotLength != "" ) {
                      auto it = findComponent( coreName );

                      if ( it == getComponents().end() )
                        throw
                          ufFatalError(
                            f, line,
                            "Unknown system component '" + coreName +
                              "' specified for priority-division bus '" +
                              p.getName() + "'.",
                            false );

                      WIR_SystemComponent &comp = *it;

                      // Only processor cores are allowed in slot directives.
                      if ( comp.getType() != WIR_SystemComponentType::core )
                        throw
                          ufFatalError(
                            f, line,
                            "Invalid non-core component '" + coreName +
                              "' specified for priority-division bus '" +
                              p.getName() + "'.",
                            false );

                      arb.pushBackSlot(
                        ( type == "pd" ?
                            WIR_BusArbitrationType::pd :
                            WIR_BusArbitrationType::tdma ),
                        toLong( slotLength ),
                        dynamic_cast<WIR_BaseProcessor &>( comp ) );
                    } else
                      throw
                        ufFatalError(
                          f, line,
                          "Incomplete " +
                            string( type == "pd" ? "PD" : "TDMA" ) +
                            " slot specification found for bus '" +
                            p.getName() + "'.",
                          false );
                  } else

                  if ( ( type == "fp" ) || ( type == "rr" ) ) {
                    string slotLength;

                    sstr >> slotLength;

                    if ( slotLength != "" )
                      arb.pushBackSlot(
                        ( type == "fp" ?
                            WIR_BusArbitrationType::fp :
                            WIR_BusArbitrationType::rr ),
                        toLong( slotLength ) );
                    else
                      throw
                        ufFatalError(
                          f, line,
                          "Incomplete " +
                            string( type == "fp" ? "FP" : "RR" ) +
                            " slot specification found for bus '" +
                            p.getName() + "'.",
                          false );
                  } else
                    throw
                      ufFatalError(
                        f, line,
                        "Invalid slot arbitration policy '" + type +
                          "' specified for bus '" + p.getName() + "'.",
                        false );
                } else

                if ( key == "priorities" ) {
                  set<WIR_id_t> addedCores;
                  WIR_FixedPriorityBusArbitration::Priority currPrio =
                    arb.getNumberOfCores();
                  stringstream sstr( value );
                  string level;
                  sstr >> level;

                  while ( level != "" ) {
                    auto it = findComponent( level );

                    if ( it == getComponents().end() )
                      throw
                        ufFatalError(
                          f, line,
                          "Unknown system component '" + level +
                            "' specified for priority-division bus '" +
                            p.getName() + "'.",
                          false );

                    WIR_SystemComponent &comp = *it;

                    // Only processor cores are allowed in a priorities
                    // directive.
                    if ( comp.getType() != WIR_SystemComponentType::core )
                      throw
                        ufFatalError(
                          f, line,
                          "Invalid non-core component '" + level +
                            "' specified for priority-division bus '" +
                            p.getName() + "'.",
                          false );

                    arb.setPriority(
                      dynamic_cast<WIR_BaseProcessor &>( comp ), currPrio-- );
                    addedCores.insert( comp.getID() );

                    level = "";
                    sstr >> level;
                  }

                  // Set priorities of all unspecified cores to 0.
                  for ( WIR_BaseProcessor &core :
                          getComponents<WIR_BaseProcessor>() )
                    if ( !addedCores.count( core.getID() ) )
                      arb.setPriority( core, 0 );
                } else

                if ( ( key != "arbitration" ) && ( key != "arbitrationdelay" ) )
                  ufWarnMsg << ufFile( f, line ) << "Currently unsupported "
                            << "priority-division bus attribute "
                            << "'" + key + "'" << " found, ignoring." << endl;
              }

              // Create new bus.
              WIR_Bus &b =
                dynamic_cast<WIR_Bus &>(
                  insertComponent( WIR_Bus( p.getName(), arb ) ) );

              // Check created bus.
              checkBus( b );

              break;
            }

            case WIR_BusArbitrationType::rr: {
              WIR_RoundRobinBusArbitration arb;

              // Iterate over all key-value properties of the current paragraph.
              for ( auto prop : p.getProperties() )
                // Nothing needs to be configured for round robin.
                if ( prop.first != "arbitration" )
                  ufWarnMsg << ufFile( f, prop.second.line ) << "Currently "
                            << "unsupported round robin bus attribute "
                            << "'" + prop.first + "'" << " found, ignoring."
                            << endl;

              // Create new bus.
              WIR_Bus &b =
                dynamic_cast<WIR_Bus &>(
                  insertComponent( WIR_Bus( p.getName(), arb ) ) );

              // Check created bus.
              checkBus( b );

              break;
            }

            case WIR_BusArbitrationType::tdma: {
              unsigned int arbitrationDelay = 0;

              // Check whether TDMA arbitration delay is specified.
              for ( auto prop : p.getProperties() )
                if ( prop.first == "arbitrationdelay" )
                  arbitrationDelay = toLong( prop.second.value );

              WIR_TDMABusArbitration arb( 0, arbitrationDelay );

              // Iterate over all key-value properties of the current paragraph.
              for ( auto prop : p.getProperties() ) {
                string key = prop.first;
                string value = prop.second.value;
                unsigned int line = prop.second.line;

                if ( key == "slot" ) {
                  stringstream sstr( value );
                  string coreName;
                  string slotLength;
                  sstr >> coreName;

                  if ( coreName != "" )
                    sstr >> slotLength;

                  if ( slotLength != "" ) {
                    auto it = findComponent( coreName );

                    if ( it == getComponents().end() )
                      throw
                        ufFatalError(
                          f, line,
                          "Unknown system component '" + coreName +
                            "' specified for TDMA bus '" + p.getName() + "'.",
                          false );

                    WIR_SystemComponent &comp = *it;

                    // Only processor cores are allowed in slot directives.
                    if ( comp.getType() != WIR_SystemComponentType::core )
                      throw
                        ufFatalError(
                          f, line,
                          "Invalid non-core component '" + coreName +
                            "' specified for TDMA bus '" + p.getName() + "'.",
                          false );

                    arb.pushBackSlot(
                      toLong( slotLength ),
                      dynamic_cast<WIR_BaseProcessor &>( comp ) );
                  } else
                    throw
                      ufFatalError(
                        f, line,
                        "Incomplete TDMA slot specification found for bus '" +
                          p.getName() + "'.",
                        false );
                } else

                if ( ( key != "arbitration" ) && ( key != "arbitrationdelay" ) )
                  ufWarnMsg << ufFile( f, line ) << "Currently unsupported TDMA"
                            << " bus attribute " << "'" + key + "'"
                            << " found, ignoring." << endl;
              }

              // Create new bus.
              WIR_Bus &b =
                dynamic_cast<WIR_Bus &>(
                  insertComponent( WIR_Bus( p.getName(), arb ) ) );

              // Check created bus.
              checkBus( b );
              DDECLARE(
                auto &arb1 =
                  dynamic_cast<const WIR_TDMABusArbitration &>(
                    b.getBusArbitration() ) );
              DOUT(
                "  Number of arbitrated cores: " <<
                arb1.getNumberOfArbitratedCores() << endl );
              DOUT(
                "  Number of slots: " << arb1.getNumberOfSlots() << endl );
              DOUT( "  Schedule length: " << arb1.getScheduleLength() << endl );
              DACTION(
                DOUT( "  Slots:" );
                for ( auto slot : arb1.getSlots() )
                  DOUT(
                    " { " << slot.getIndex() << ", '"
                          << slot.getOwner().getName() << "', "
                          << slot.getStart() << ", " << slot.getLength()
                          << " }" );
                DOUT( endl ); );
              DACTION(
                for ( WIR_BaseProcessor &c :
                        getComponents<WIR_BaseProcessor>() ) {
                  DOUT( "  Slots of '" << c.getName() << "':" );
                  for ( auto slot : arb.getSlots( c ) )
                    DOUT(
                      " { " << slot.getIndex() << ", '"
                            << slot.getOwner().getName() << "', "
                            << slot.getStart() << ", " << slot.getLength()
                            << " }" );
                  DOUT( endl );
                } );
              DACTION(
                for ( WIR_BaseProcessor &c :
                        getComponents<WIR_BaseProcessor>() ) {
                  DOUT(
                    "  Grant windows of '" << c.getName() << "': " <<
                    arb1.getGrantWindows( c ) << endl );
                } );

              break;
            }

          }
        } else {
          // Extract the cache's basic properties.
          unsigned int size = 0;
          unsigned int lineSize = 0;
          bool sizeSet = false;
          unsigned int assoc = 0;
          unsigned int lineAssoc = 0;
          bool assocSet = false;
          unsigned int lsize = 0;
          unsigned int lineLsize = 0;
          bool lsizeSet = false;

          for ( auto prop : p.getProperties() ) {
            string key = prop.first;
            string value = prop.second.value;
            unsigned int line = prop.second.line;

            auto checkParamSet = [&]( string k, unsigned int &v, bool &b,
                                      unsigned int &l ) {
              if ( key == k ) {
                v = toLong( value );
                l = line;
                b = true;
              }
            };
            checkParamSet( "size", size, sizeSet, lineSize );
            checkParamSet( "associativity", assoc, assocSet, lineAssoc );
            checkParamSet( "linesize", lsize, lsizeSet, lineLsize );
          }

          auto checkCacheParams = [&]( bool b, unsigned int v, unsigned int l,
                                       string str1, string str2 ) {
            if ( !b )
              throw
                ufFatalError(
                  f, p.getLine(),
                  "No " + str1 + " was given for cache '" + p.getName() + "'.",
                  false );
            else

            if ( !isPowerOfTwo( v ) )
              throw
                ufFatalError(
                  f, l,
                  str2 + " of cache '" + p.getName() +
                    "' must be a power of 2.",
                  false );
          };

          checkCacheParams( sizeSet, size, lineSize, "size", "Size" );
          checkCacheParams(
            assocSet, assoc, lineAssoc, "associativity", "Associativity" );
          checkCacheParams(
            lsizeSet, lsize, lineLsize, "line size", "Line size" );

          // Create new cache.
          WIR_Cache &c =
            dynamic_cast<WIR_Cache &>(
              insertComponent( WIR_Cache( p.getName(), size, assoc, lsize ) ) );
          // Check for duplicate paragraph names.
          checkDuplicateNames( p );
          DOUT( "Created new cache '" << c.getName() << "'" << endl );
          DOUT( "  Size: " << c.getSize() << " bytes" << endl );
          DOUT( "  Associativity: " << c.getAssociativity() << endl );
          DOUT( "  Line size: " << c.getLineSize() << " bytes" << endl );
          DOUT( "  Number of sets: " << c.getNumberOfSets() << endl );
          DOUT( "  Number of offset bits: " << c.getOffsetBits() << endl );
          DOUT( "  Number of index bits: " << c.getIndexBits() << endl );
          DOUT( "  Number of tag bits: " << c.getTagBits() << endl );
          DOUT( "  Offset bit mask: " << c.mOffsetMask << endl );
          DOUT( "  Index bit mask: " << c.mIndexMask << endl );
          DOUT( "  Offset+index bit mask: " << c.mIndexOffsetMask << endl );

          // Iterate over all key-value properties of the current paragraph.
          bool busWidthSet = false;
          for ( auto prop : p.getProperties() ) {
            string key = prop.first;
            string value = prop.second.value;
            unsigned int line = prop.second.line;

            if ( key == "enabled" ) {
              c.setEnabled( toLong( value ) != 0 );
              DOUT( "  Enabled: " << boolalpha << c.isEnabled() << endl );
            } else

            if ( key == "shared" ) {
              c.setShared( toLong( value ) != 0 );
              DOUT( "  Shared: " << boolalpha << c.isShared() << endl );
            } else

            if ( key == "writethrough" ) {
              c.setWriteThrough( toLong( value ) != 0 );
              DOUT(
                "  Write-through: " << boolalpha << c.isWriteThrough() <<
                endl );
            } else

            if ( key == "writeallocate" ) {
              c.setWriteAllocate( toLong( value ) != 0 );
              DOUT(
                "  Write-allocate: " << boolalpha << c.isWriteAllocate() <<
                endl );
            } else

            if ( key == "hitdelay" ) {
              c.setHitDelay( toLong( value ) );
              DOUT( "  Hit delay: " << c.getHitDelay() << " cycles" << endl );
            } else

            if ( key == "missdelay" ) {
              c.setMissDelay( toLong( value ) );
              DOUT( "  Miss delay: " << c.getMissDelay() << " cycles" << endl );
            } else

            if ( key == "buswidth" ) {
              c.setBusWidth( toLong( value ) );
              busWidthSet = true;
              DOUT( "  Bus width: " << c.getBusWidth() << " bytes" << endl );
            } else

            if ( key == "cache_type" ) {
              if ( ( value.size() == 1 ) && ( tolower( value[ 0 ] ) == 'd' ) )
                c.setCacheType( WIR_Cache::CacheType::D );
              else

              if ( ( value.size() == 1 ) && ( tolower( value[ 0 ] ) == 'i' ) )
                c.setCacheType( WIR_Cache::CacheType::I );
              else

              if ( ( value.size() == 1 ) && ( tolower( value[ 0 ] ) == 'u' ) )
                c.setCacheType( WIR_Cache::CacheType::U );
              else
                throw
                  ufFatalError(
                    f, line,
                    "Invalid cache type '" + value + "' specified for cache '" +
                      p.getName() + "'.",
                    false );

              DOUT(
                "  Cache type: " <<
                string(
                  ( c.getCacheType() == WIR_Cache::CacheType::D ) ?
                    "Data" :
                    ( ( c.getCacheType() == WIR_Cache::CacheType::I ) ?
                      "Instruction" : "Unified" ) ) << endl );
            } else

            if ( key == "percent_size" ) {
              c.setSizeInPercent( toLong( value ) );
              DOUT( "  Relative size: " << toLong( value ) << "%" << endl );
            } else

            if ( ( key != "size" ) && ( key != "associativity" ) &&
                 ( key != "linesize" ) )
              ufWarnMsg << ufFile( f, line ) << "Currently unsupported cache "
                        << "attribute " << "'" + key + "'" << " found, "
                        << "ignoring." << endl;
          }

          if ( !busWidthSet )
            throw
              ufFatalError(
                f, p.getLine(),
                "No bus width specified for cache '" + p.getName() + "'.",
                false );
        }
      }

      if ( ( round == 3 ) &&
           ( p.getType() == WIR_SystemComponentType::memory ) ) {
        set<string> sectionNames;

        // Create new memory region.
        WIR_MemoryRegion &r =
          dynamic_cast<WIR_MemoryRegion &>(
            insertComponent( WIR_MemoryRegion( p.getName() ) ) );
        // Check for duplicate paragraph names.
        checkDuplicateNames( p );
        DOUT( "Created new memory region '" << r.getName() << "'" << endl );
        loadSectionNames[ r ].clear();

        // Iterate over all key-value properties of the current paragraph.
        for ( auto prop : p.getProperties() ) {
          string key = prop.first;
          string value = prop.second.value;
          unsigned int line = prop.second.line;

          if ( key == "origin" ) {
            r.setBaseAddress( WIR_MemoryAddress( toLong( value ) ) );
            DOUT( "  Base address: 0x" << hex << r.getBaseAddress() << endl );
          } else

          if ( key == "length" ) {
            if ( toLong( value ) == 0 )
              throw
                ufFatalError(
                  f, line,
                  "Invalid length of " + value +
                    " bytes specified for region '" + r.getName() + "'.",
                  false );

            r.setLength( toLong( value ) );
            DOUT( "  Length: 0x" << hex << r.getLength() << endl );
          } else

          if ( key == "dynamic_allocation" ) {
            r.setDynamicAllocation( toLong( value ) != 0 );
            DOUT(
              "  Dynamic allocation: " << boolalpha <<
              r.isDynamicallyAllocated() << endl );
          } else

          if ( key == "attributes" ) {
            unsigned long bitMask =
              (unsigned long) WIR_MemoryRegionAttributes::none;
            unsigned long currentItem =
              (unsigned long) WIR_MemoryRegionAttributes::none;
            bool inverted = false;

            for ( size_t i = 0; i < value.size(); ++i ) {
              value[ i ] = tolower( value[ i ] );

              switch ( value[ i ] ) {
                case 'a': {
                  currentItem =
                    (unsigned long) WIR_MemoryRegionAttributes::allocated;
                  break;
                }

                case 'c': {
                  currentItem =
                    (unsigned long) WIR_MemoryRegionAttributes::cached;
                  break;
                }

                case 'i': {
                  currentItem =
                    (unsigned long) WIR_MemoryRegionAttributes::initialized;
                  break;
                }

                case 'r': {
                  currentItem =
                    (unsigned long) WIR_MemoryRegionAttributes::read;
                  break;
                }

                case 'w': {
                  currentItem =
                    (unsigned long) WIR_MemoryRegionAttributes::write;
                  break;
                }

                case 'x': {
                  currentItem =
                    (unsigned long) WIR_MemoryRegionAttributes::execute;
                  break;
                }

                case '!': {
                  inverted = !inverted;
                  continue;
                }

                case ' ':
                case '\t':
                case '\r':
                default:
                  break;
              }

              if ( inverted )
                // Unset the chosen bit.
                bitMask &=
                  (unsigned long) WIR_MemoryRegionAttributes::all ^ currentItem;
              else
                // Set the chosen bit.
                bitMask |= currentItem;
            }

            r.setAttributes( bitMask );
            DOUT( "  Attributes: 0x" << hex << r.getAttributes() << endl );
          } else

          if ( key == "memory_type" ) {
            r.setMemoryType( value );
            DOUT( "  Memory type: '" << r.getMemoryType() << "'" << endl );
          } else

          if ( key == "clockratio" ) {
            r.setClockRatio( toFraction( value ) );
            DOUT( "  Clock ratio: " << r.getClockRatio() << endl );
          } else

          if ( key == "cycles" ) {
            r.setDelay( toLong( value ) );
            DOUT( "  Delay: " << r.getMaxDelay() << endl );
          } else

          if ( key == "burstcycles" ) {
            r.setBurstDelay( toLong( value ) );
            DOUT( "  Burst delay: " << r.getMaxBurstDelay() << endl );
          } else

          if ( key == "buswidth" ) {
            r.setBusWidth( toLong( value ) );
            DOUT( "  Bus width: " << r.getBusWidth() << endl );
          } else

          if ( key == "hierarchy" ) {
            set<WIR_id_t> alreadyInserted;
            unsigned int levelCount = 0;
            list<reference_wrapper<WIR_SystemComponent>> hierarchy;

            stringstream sstr( value );
            string level;
            sstr >> level;

            while ( level != "" ) {
              auto it = findComponent( level );

              if ( it == getComponents().end() )
                throw
                  ufFatalError(
                    f, line,
                    "Unknown system component '" + level +
                      "' specified for region '" + r.getName() + "'.",
                    false );

              WIR_SystemComponent &comp = *it;

              // Ensure that the specified component does not occur multiple
              // times within one hierarchy directive.
              if ( alreadyInserted.count( comp.getID() ) )
                throw
                  ufFatalError(
                    f, line,
                    "System component '" + level + "' specified multiple " +
                      "times for region '" + r.getName() + "'.",
                    false );

              // Ensure that the very first entry of a hierarchy directive
              // refers to a processor core.
              if ( hierarchy.empty() &&
                   ( comp.getType() != WIR_SystemComponentType::core ) )
                throw
                  ufFatalError(
                    f, line,
                    "The first entry '" + level + "' of a hierarchy " +
                      "directive must refer to a processor core.",
                    false );

              // Add specified component to this region's memory hierarchy.
              hierarchy.push_back( comp );
              alreadyInserted.insert( comp.getID() );

              // If it's a cache, set its cache level explicitly.
              if ( comp.getType() == WIR_SystemComponentType::cache ) {
                WIR_Cache &c = dynamic_cast<WIR_Cache &>( comp );
                ++levelCount;

                if ( levelCount > 2 )
                  throw
                    ufFatalError(
                      f, line,
                      "Cache '" + c.getName() + "' specified at a too deep " +
                        "level of the memory hierarchy of region '" +
                        r.getName() + "'.",
                      false );

                if ( levelCount == 1 )
                  c.setLevel( WIR_Cache::CacheLevel::L1 );
                else

                if ( levelCount == 2 )
                  c.setLevel( WIR_Cache::CacheLevel::L2 );
              }

              level = "";
              sstr >> level;
            }

            if ( hierarchy.empty() )
              throw
                ufFatalError(
                  f, line, "Hierarchy directives must not be empty.", false );

            r.insertHierarchy( move( hierarchy ) );
          } else

          if ( key == "sections" ) {

            stringstream sstr( value );
            string sec;
            sstr >> sec;

            while ( sec != "" ) {
              // Simply store the attached section names for later processing.
              sectionNames.insert( sec );

              sec = "";
              sstr >> sec;
            }
          } else

          if ( key == "load" ) {

            stringstream sstr( value );
            string sec;
            sstr >> sec;

            while ( sec != "" ) {
              // Simply store the attached section names for later processing.
              loadSectionNames[ r ].insert( sec );

              sec = "";
              sstr >> sec;
            }
          } else
            ufWarnMsg << ufFile( f, line ) << "Currently unsupported memory "
                      << "region attribute " << "'" + key + "'" << " found, "
                      << "ignoring." << endl;
        }

        DACTION(
          DOUT( "  Memory hierarchy:" );
          for ( auto &l : r.getHierarchy() ) {
            DOUT( " {" );
            for ( WIR_SystemComponent &m : l )
              DOUT( " '" << m.getName() << "'" );
            DOUT( " }" );
          }
          DOUT( endl );
        );

        // Add the address range that is covered by this new memory region to
        // components of the memory hierarchy that are located in front of this
        // region.
        WIR_AddressRange range { r.getBaseAddress(),
                                 r.getBaseAddress() + r.getLength() - 1 };
        for ( auto &l : r.getHierarchy() )
          for ( WIR_SystemComponent &m : l ) {
            // Check that the address ranges accessible by each processor core
            // do not overlap.
            if ( ( m.getType() == WIR_SystemComponentType::core ) &&
                 m.isActiveInRange( range ) ) {
              stringstream sstr;
              sstr << hex << range;
              throw
                ufFatalError(
                  f, p.getLine(),
                  "Address range " + sstr.str() + " of region '" + r.getName() +
                    "' overlaps with other active address ranges of core '" +
                    m.getName() + "'.",
                  false );
            }

            m.addAddressRange( range );
          }

        // Finally, create all specified sections and attach them to their
        // cores.
        createSections( r, sectionNames, f, p.getLine() );
      }

      if ( ( round == 4 ) &&
           ( p.getType() == WIR_SystemComponentType::section ) ) {
        // Determine section from paragraph name.
        stringstream sstr( p.getName() );
        string secName;
        sstr >> secName;

        string coreName;
        sstr >> coreName;

        auto cores = getComponents<WIR_BaseProcessor>();
        auto it = cores.end();

        if ( ( cores.size() == 1 ) && coreName.empty() )
          it = cores.begin();
        else

        if ( coreName.empty() )
          throw
            ufFatalError(
              f, p.getLine(),
              "No processor core specified for section '" + secName + "'.",
              false );
        else {
          for ( auto it1 = cores.begin(); it1 != cores.end(); ++it1 )
            if ( it1->get().getName() == coreName ) {
              it = it1;
              break;
            }
        }

        if ( it == cores.end() )
          throw
            ufFatalError(
              f, p.getLine(),
              "Unknown core '" + coreName + "' specified for section '" +
                secName + "'.",
              false );

        WIR_BaseProcessor &c = it->get();

        if ( !c.containsSection( secName ) )
          throw
            ufFatalError(
              f, p.getLine(),
              "Unknown section '" + secName + "' specified for core '" +
                coreName + "'.",
              false );

        WIR_Section &s = c.findSection( secName )->get();
        DOUT(
          "Updating section '" << s.getName() << "' of core '" << c.getName() <<
          "'" << endl );

        // Iterate over all key-value properties of the current paragraph.
        for ( auto prop : p.getProperties() ) {
          string key = prop.first;
          string value = prop.second.value;
          unsigned int line = prop.second.line;

          if ( key == "alignment" ) {
            s.setAlignment( toLong( value ) );
            DOUT( "  Alignment: " << s.getAlignment() << " Bits" << endl );
          } else

          if ( key == "block" ) {
            s.setBlock( toDouble( value ) );
            DOUT( "  Block: " << s.getBlock() << " Bits" << endl );
          } else
            ufWarnMsg << ufFile( f, line ) << "Currently unsupported section "
                      << "attribute " << "'" + key + "'" << " found, "
                      << "ignoring." << endl;
        }
      }
    }

    if ( ( round == 1 ) &&
         ( getComponents<WIR_BaseProcessor>().size() == 0 ) )
      throw
        ufFatalError(
          f, 0, "At least one processor core must be specified.", false );
  }

  // Ensure that each processor core has the 4 default sections .text, .data,
  // .bss and .rodata.
  for ( WIR_BaseProcessor &c : getComponents<WIR_BaseProcessor>() )
    for ( auto secName : list<string> { ".text", ".data", ".bss", ".rodata" } )
      if ( !c.containsSection( secName ) )
        throw
          ufFatalError(
            f, 0,
            "Mandatory default section '" + secName + "' not specified for " +
              "core '" + c.getName() + "'.",
            false );

  // Attach all loaded sections to their respective regions.
  attachLoadedSections( loadSectionNames, f );

  // Do post-processing.
  computeMaximumAccessTimes();
  computeHierarchyLevels();
};


/*
  createSections creates the specified sections and attaches them to their
  respective processor cores.
*/
void WIR_System::createSections( WIR_MemoryRegion &r,
                                 const std::set<std::string> &sectionNames,
                                 const std::string &f, unsigned int l ) const
{
  DSTART(
    "void WIR_System::createSections(WIR_MemoryRegion&, const set<string>&, const string&, unsigned int) const" );

  // Identify the involved processor cores first.
  auto cores = getComponents<WIR_BaseProcessor>();

  if ( cores.size() != 1 ) {
    cores.clear();

    // It's a multi-core system. So, check all hierarchy directives of the
    // memory region and extract their very first entry which must be a core.
    for ( auto &h : r.getHierarchy() )
      cores.insert( dynamic_cast<WIR_BaseProcessor &>( h.front().get() ) );
  }

  if ( cores.size() == 0 )
    throw
      ufFatalError(
        f, l,
        "Failed to identify cores for sections directive. Please add " +
          string( "hierarchy directives." ),
        false );

  WIR_SectionSet sections;

  DOUT( "Sections: " );

  // Attach the sections to their cores.
  for ( WIR_BaseProcessor &p : cores ) {
    DACTION(
      if ( p != cores.begin()->get() )
        DOUT( "          " );
      DOUT( "'" << p.getName() << "':" ); );

    for ( auto &s : sectionNames )
      if ( !p.containsSection( s ) ) {
        auto &sec = p.insertSection( WIR_Section( s, r ) );
        sections.insert( sec );
        DOUT( " '" << sec.getName() << "'" );
      }

    DOUT( endl );
  }

  // Attach the sections to its region.
  for ( WIR_Section &s : sections )
    r.insertSection( s );
};


/*
  attachLoadedSections attaches all sections specified in some 'load' directives
  to their corresponding memory regions.
*/
void WIR_System::attachLoadedSections( const std::map<std::reference_wrapper<WIR_MemoryRegion>, std::set<std::string>, WIR_Compare<WIR_MemoryRegion>> &l,
                                       const std::string &f )
{
  DSTART( "void WIR_System::attachLoadedSections(const map<reference_wrapper<WIR_MemoryRegion>, set<string>, WIR_Compare<WIR_MemoryRegion> >&, const string&)" );

  // Iterate all load directives in l.
  for ( auto &p : l ) {
    WIR_MemoryRegion &r = p.first.get();
    const set<string> &loadedSectionNames = p.second;

    // Identify the involved processor cores first.
    auto cores = getComponents<WIR_BaseProcessor>();

    if ( cores.size() != 1 ) {
      cores.clear();

      // It's a multi-core system. So, check all hierarchy directives of the
      // memory region and extract their very first entry which must be a core.
      for ( auto &h : r.getHierarchy() )
        cores.insert( dynamic_cast<WIR_BaseProcessor &>( h.front().get() ) );
    }

    // Attach all loaded sections to the current region.
    for ( WIR_BaseProcessor &c : cores ) {
      for ( auto &s : loadedSectionNames ) {
        auto it = c.findSection( s );

        if ( it == c.getSections().end() )
          throw
            ufFatalError(
              f, 0,
              "Load directive of region '" + r.getName() + "' refers to " +
                "unknown section '" + s + "' for processor core '" +
                c.getName() + "'.",
              false );

        it->get().setLoadRegion( r );
        DOUT(
          "Setting load region of section '" << s << "' to '" << r.getName() <<
          "'." << endl );
      }
    }
  }
};


/*
  computeMaximumAccessTimes computes the maximum access times that memory
  regions can have and propagates them into a system's buses.

  computeMaximumAccessTimes scans all 'hierarchy' directives of memory regions
  backwards and maximizes/accumulates the access times of all system components
  visited this way.
*/
void WIR_System::computeMaximumAccessTimes( void )
{
  DSTART( "void WIR_System::computeMaximumAccessTimes()" );

  map<WIR_id_t, unsigned int> accessTimes;
  set<reference_wrapper<WIR_Bus>, WIR_Compare<WIR_Bus>> buses;

  // Iterate over all memory regions.
  for ( WIR_MemoryRegion &r : getComponents<WIR_MemoryRegion>() ) {
    accessTimes[ r.getID() ] = r.getMaxDelay();
    DOUT(
      "accessTimes[ '" << r.getName() << "' ] = " << accessTimes[ r.getID() ] <<
      endl );

    // Iterate backwards over all hierarchy specifications.
    for ( auto &l : r.getHierarchy() ) {
      auto prevItem = l.rbegin();
      for ( auto it = l.rbegin(); it != l.rend(); ++it ) {
        WIR_SystemComponent &c = it->get();

        if ( it == l.rbegin() ) {
          DOUT(
            "  (1) accessTimes[ '" << c.getName() << "' ] = max{ " <<
            accessTimes[ c.getID() ] << ", " <<
            c.getMaxDelay( r.getMaxDelay() ) << " } = " );
          accessTimes[ c.getID() ] =
            max( accessTimes[ c.getID() ], c.getMaxDelay( r.getMaxDelay() ) );
          DOUT( accessTimes[ c.getID() ] << endl );
        } else {
          DOUT(
            "  (2) accessTimes[ '" << c.getName() << "' ] = max{ " <<
            accessTimes[ c.getID() ] << ", " <<
            c.getMaxDelay( accessTimes[ prevItem->get().getID() ] ) <<
            " } = " );
          accessTimes[ c.getID() ] =
            max(
              accessTimes[ c.getID() ],
              c.getMaxDelay( accessTimes[ prevItem->get().getID() ] ) );
          DOUT( accessTimes[ c.getID() ] << endl );
        }

        if ( c.getType() == WIR_SystemComponentType::bus )
          buses.insert( dynamic_cast<WIR_Bus &>( c ) );

        prevItem = it;
      }
    }
  }

  // Propagate the computed values into the system's buses.
  for ( WIR_Bus &b : buses ) {
    b.setMaxAccessTime( accessTimes[ b.getID() ] );

    DACTION(
      if ( b.getArbitrationType() == WIR_BusArbitrationType::tdma ) {
        auto &arb =
          dynamic_cast<const WIR_TDMABusArbitration &>( b.getBusArbitration() );
        DOUT( "Update for bus '" << b.getName() << "'" << endl );

        for ( WIR_BaseProcessor &p : getComponents<WIR_BaseProcessor>() ) {
          DOUT(
            "  Grant windows of '" << p.getName() << "': " <<
            arb.getGrantWindows( p ) << endl );
        }
      } else

      if ( b.getArbitrationType() == WIR_BusArbitrationType::pd ) {
        auto &arb =
          dynamic_cast<const WIR_PriorityDivisionBusArbitration &>(
            b.getBusArbitration() );
        DOUT( "Update for bus '" << b.getName() << "'" << endl );

        for ( WIR_BaseProcessor &p : getComponents<WIR_BaseProcessor>() ) {
          DOUT(
            "  MUST Grant windows of '" << p.getName() << "': " <<
            arb.getMustGrantWindows( p ) << endl );
        }
        for ( WIR_BaseProcessor &p : getComponents<WIR_BaseProcessor>() ) {
          DOUT(
            "  MAY Grant windows of '" << p.getName() << "': " <<
            arb.getMayGrantWindows( p ) << endl );
        } } );
  }
};


/*
  computeHierarchyLevels computes to which level of the component hierarchy all
  the cores, caches and buses in mComponentReferences belong.

  Independent buses/caches are put in the same level of the hierarchy, while
  cascaded ones are put in different levels.
*/
void WIR_System::computeHierarchyLevels( void )
{
  DSTART( "void WIR_System::computeHierarchyLevels()" );

  // predecessors stores the successor components per core/cache/bus.
  map<WIR_id_t, set<WIR_id_t>> predecessors;

  // successors stores the successor components per core/cache/bus.
  map<WIR_id_t, set<WIR_id_t>> successors;

  // startNodes contains all those components having no predecessors.
  list<WIR_id_t> startNodes;

  // hierarchyLevel maps each core/cache/bus to its level in the system's
  // architectural hierarchy.
  map<WIR_id_t, unsigned long long> hierarchyLevel;

  // Initialize predecessors and successors.
  for ( WIR_SystemComponent &m : mComponentReferences ) {
    if ( m.getType() != WIR_SystemComponentType::memory ) {
      predecessors[ m.getID() ].clear();
      successors[ m.getID() ].clear();
      hierarchyLevel[ m.getID() ] = 0;
    }

    // We always start topological sorting with all processor cores.
    if ( m.getType() == WIR_SystemComponentType::core ) {
      DOUT( "Inserting core '" << m.getName() << "' into startNodes." << endl );
      startNodes.push_back( m.getID() );
    }
  }

  // Compute predecessors and successor sets per core/cache/bus.
  for ( WIR_SystemComponent &m : mComponentReferences ) {
    if ( m.getType() == WIR_SystemComponentType::memory ) {
      WIR_MemoryRegion &r = dynamic_cast<WIR_MemoryRegion &>( m );

      for ( auto &l : r.getHierarchy() ) {
        WIR_SystemComponent *previous = nullptr;

        for ( WIR_SystemComponent &mem : l ) {
          if ( previous != nullptr ) {
            auto &succs = successors[ previous->getID() ];
            succs.insert( mem.getID() );
            DOUT(
              "Adding '" << mem.getName() << "' as successor of '" <<
              previous->getName() << "'." << endl );

            auto &preds = predecessors[ mem.getID() ];
            preds.insert( previous->getID() );
            DOUT(
              "Adding '" << previous->getName() << "' as predecessor of '" <<
              mem.getName()  << "'." << endl );
          }
          previous = &mem;
        }
      }
    }
  }

  // Do a topological traversal of the system's architecture.
  while ( !startNodes.empty() ) {
    WIR_id_t currentNode = startNodes.front();
    startNodes.pop_front();
    auto level = hierarchyLevel[ currentNode ];

    auto &succs = successors[ currentNode ];
    for ( auto succ : succs ) {
      predecessors[ succ ].erase( currentNode );

      if ( predecessors[ succ ].empty() )
        startNodes.push_back( succ );

      // Update the successors' hierarchy levels.
      if ( level + 1 > hierarchyLevel[ succ ] )
        hierarchyLevel[ succ ] = level + 1;
    }
  }

  // Retrieve hierarchy level values.
  for ( WIR_SystemComponent &m : mComponentReferences )
    if ( m.getType() != WIR_SystemComponentType::memory ) {
      auto level = hierarchyLevel[ m.getID() ];

      DOUT( "'" << m.getName() << "': Level " << level << endl );

      mComponentHierarchyLevels[ level ].insert( m );
    }
};


/*
  insertComponent adds a new WIR_SystemComponent to the sets mComponentPointers
  and mComponentReferences.

  As a side effect, all already computed memory hierarchy levels of a system are
  discarded, since the insertion of a new component into the entire hierarchy
  might change the previously computed information. The content of o is copied
  to the new set element.
*/
WIR_SystemComponent &WIR_System::insertComponent( const WIR_SystemComponent &o )
{
  DSTART(
    "WIR_SystemComponent& WIR_System::insertComponent(const WIR_SystemComponent&)" );

  mComponentHierarchyLevels.clear();

  auto *p = o.clone();
  mComponentPointers.insert( unique_ptr<WIR_SystemComponent>( p ) );
  p->onInsert( this );
  mComponentReferences.insert( *p );

  return( *p );
};


/*
  clearComponents removes all elements from sets mComponentPointers,
  mComponentReferences and mComponentHierarchyLevels.

  This destroys all removed elements.
*/
void WIR_System::clearComponents( void )
{
  DSTART( "void WIR_System::clearComponents()" );

  mComponentHierarchyLevels.clear();
  mComponentReferences.clear();
  mComponentPointers.clear();
};


/*
  For all functions, basic blocks and daba objects of the specified compilation
  unit, insertSymbols adds corresponding symbols to the system's symbol table.
*/
void WIR_System::insertSymbols( const WIR_CompilationUnit &u )
{
  DSTART( "void WIR_System::insertSymbols(const WIR_CompilationUnit&)" );

  for ( WIR_Function &f : u ) {
    insertSymbol( f );

    for ( WIR_BasicBlock &b : f )
      insertSymbol( b );
  }

  for ( WIR_Data &d : u.getData() )
    insertSymbol( d );
};


/*
  computeMemoryLayout examines all data objects, functions and basic blocks of a
  WIR system and computes their absolute start addresses according to the
  address ranges of their respective memory regions.
*/
void WIR_System::computeMemoryLayout( void )
{
  DSTART( "void WIR_System::computeMemoryLayout()" );

  // If the symbol table is clean, do nothing.
  if ( !mSymbolsDirty )
    return;

  mRegion2sections.clear();
  mHighestAddresses.clear();
  mSymbolsOfSection.clear();

  // Compute the interal layout of all sections and the section sizes first.
  computeSectionLayout();

  // Process sections with fixed offsets first, i.e., sections with VMA != LMA.
  for ( WIR_BaseProcessor &p : getComponents<WIR_BaseProcessor>() ) {
    for ( WIR_Section &sec : p )
      if ( sec.isStartSet() ) {
        // The section's VMA has been set, this is thus a fixed-offset section.
        WIR_MemoryRegion &r = sec.getRegion();
        auto &sectionList = mRegion2sections[ r.getID() ];

        // Determine the correct position of the section in mRegion2sections.
        auto it = sectionList.begin();
        while ( ( it != sectionList.end() ) &&
                ( it->get().getStart() ) < sec.getStart() )
          ++it;

        // Insert the section at the determined position inside the region.
        sectionList.insert( it, sec );

        DOUT(
          "Section '" << sec.getName() << "' of core '" << p.getName() <<
          "' mapped to memory region '" << r.getName() << "' with fixed " <<
          "start address 0x" << hex << sec.getStart() << "." << endl );
      }

    // Break down the offsets of symbols within sections and of sections within
    // regions to actual physical addresses of all symbols.
    computePhysicalAddresses();
  }

  // Process sections without fixed offsets next using a first-fit strategy.
  for ( WIR_BaseProcessor &p : getComponents<WIR_BaseProcessor>() ) {
    for ( WIR_Section &sec : p )
      if ( !sec.isStartSet() ) {
        // The section's VMA is not set, this is thus a float-offset section.
        WIR_MemoryRegion &r = sec.getRegion();
        auto &sectionList = mRegion2sections[ r.getID() ];

        // Find a gap in the region's section list into which the current
        // section fits (first-fit).
        size_t offset = 0;
        auto it = sectionList.begin();
        while ( it != sectionList.end() ) {
          // Case 1: The current section in the list has a fixed offset. The gap
          //         size is thus the difference between that section's start
          //         and the current offset.
          if ( it->get().isStartSet() ) {
            if ( it->get().getStart() - offset > sec.mLength )
              // OK, the gap is large enough for the current float-offset
              // section.
              break;

            // The gap is too small, we thus have to continue in the region's
            // section list with an incremented offset.
            offset = it->get().getStart() + it->get().mLength;
          } else
            // Case 2: The current section in the list also has a floating
            //         offset. So, we have to continue and look behind that
            //         floating section.
            offset += it->get().mLength;

          ++it;
        }

        // Insert the section at the determined position inside the region.
        sectionList.insert( it, sec );

        DOUT(
          "Section '" << sec.getName() << "' of core '" << p.getName() <<
          "' mapped to memory region '" << r.getName() << "' at floating " <<
          "offset 0x" << hex << offset << "." << endl );
      }

    // Break down the offsets of symbols within sections and of sections within
    // regions to actual physical addresses of all symbols.
    computePhysicalAddresses();
  }

  mRegion2sections.clear();
  mHighestAddresses.clear();
  mSymbolsOfSection.clear();

  // Compute free space left over in the regions.
  size_t totallyUsedSpace = 0;

  for ( WIR_MemoryRegion &r : getComponents<WIR_MemoryRegion>() ) {
    DOUT(
      "Memory region '" << r.getName() << "' of size 0x" << hex <<
      r.mLength << " contains" << endl );

    size_t usedSpace = 0;
    for ( WIR_Section &sec : r.getSections() ) {
      usedSpace += sec.mLength;
      DOUT(
        "  section '" << sec.getName() << "' of size 0x" << hex <<
        sec.mLength << endl );
    }

    // Set the region's free space.
    if ( ( usedSpace > r.getLength() ) && !mWarnedRegions.count( r.getID() ) ) {
      mWarnedRegions.insert( r.getID() );
      stringstream str;
      str << "Memory region '" << r.getName() << "' of size 0x" << hex
          << r.mLength << " overloaded by 0x" << usedSpace - r.getLength()
          << " bytes." << dec << endl;
      ufWarnMsg << ufFile() << str.str() << endl;
    }
    // ufAssert( r.getLength() >= usedSpace );
    r.setFreeSpace( r.getLength() - usedSpace );

    totallyUsedSpace += usedSpace;

    DOUT(
      "  Free space of this region: 0x" << hex << r.mFreeSpace << endl );
  }

  // Scale cache sizes if they are configured for relative sizes.
  for ( WIR_SystemComponent &comp : getComponents() )
    if ( comp.getType() == WIR_SystemComponentType::cache ) {
      auto &c = dynamic_cast<WIR_Cache &>( comp );

      if ( c.isSizedInPercent() )
        c.setRelativeSize( totallyUsedSpace );
    }

  mSymbolsDirty = false;
};


/*
  computeSectionLayout examines all data objects, functions and basic blocks of
  a WIR system and computes their start addresses within their assigned ELF
  sections as well as the byte sizes of the sections.
*/
void WIR_System::computeSectionLayout( void )
{
  DSTART( "void WIR_System::computeSectionLayout()" );

  map<WIR_id_t, size_t> sectionOffset;

  for ( WIR_CompilationUnit &c : getCompilationUnits() ) {

    // Process data objects.
    for ( WIR_Data &d : c.getData() ) {
      WIR_Symbol &sym = findSymbol( d );
      auto &sec = sym.getSection();
      mSymbolsOfSection[ sec.getID() ].insert( sym );

      if ( sectionOffset.count( sec.getID() ) == 0 ) {
        if ( d.getSize() != 0 ) {
          sectionOffset[ sec.getID() ] = d.getSize();
          sym.setBaseAddress( WIR_MemoryAddress( 0 ) );

          DOUT(
            "Setting base address of data object '" << d.getName() <<
            "' to 0x" << hex << sym.mBaseAddress << "." << endl );
          DOUT(
            "Setting offset of section '" << sec.getName() << "' of core '" <<
            sec.getProcessor().getName() << "' to 0x" << hex <<
            sectionOffset[ sec.getID() ] << " due to data object '" <<
            d.getName() << "'." << endl );
        }
      } else {
        size_t offset = sectionOffset[ sec.getID() ];

        // If the current section offset is not properly aligned, round up.
        size_t alignment = 1 << sec.getBlock();
        if ( offset & ( alignment - 1 ) ) {
          offset = ( ( offset + alignment - 1 ) & ~( alignment - 1 ) );
          DOUT(
            "Applying section-internal alignment by " << sec.getBlock() <<
            " bits, resulting offset is 0x" << hex << offset << "." << endl );
        }

        sectionOffset[ sec.getID() ] = offset + d.getSize();
        sym.setBaseAddress( WIR_MemoryAddress( offset ) );

        DOUT(
          "Setting base address of data object '" << d.getName() << "' to 0x" <<
          hex << sym.mBaseAddress << "." << endl );
        DOUT(
          "Setting offset of section '" << sec.getName() << "' of core '" <<
          sec.getProcessor().getName() << "' to 0x" << hex <<
          sectionOffset[ sec.getID() ] << " due to data object '" <<
          d.getName() << "'." << endl );
      }
    }

    // Process functions and basic blocks.
    for ( WIR_Function &f : c ) {
      WIR_Symbol &fsym = findSymbol( f );
      mSymbolsOfSection[ fsym.getSection().getID() ].insert( fsym );

      for ( WIR_BasicBlock &b : f ) {
        WIR_Symbol &sym = findSymbol( b );
        auto &sec = sym.getSection();
        size_t blockSize = b.getSize();
        mSymbolsOfSection[ sec.getID() ].insert( sym );

        if ( sectionOffset.count( sec.getID() ) == 0 ) {
          sectionOffset[ sec.getID() ] = blockSize;
          sym.setBaseAddress( WIR_MemoryAddress( 0 ) );

          if ( b == f.getBasicBlocks().front().get() ) {
            fsym.setBaseAddress( WIR_MemoryAddress( 0 ) );
            DOUT(
              "Setting base address of function '" << f.getName() <<
              "' to 0x" << hex << fsym.mBaseAddress << "." << endl );
          }

          DOUT(
            "Setting base address of basic block '" << b.getName() <<
            "' to 0x" << hex << sym.mBaseAddress << "." << endl );
          DOUT(
            "Setting offset of section '" << sec.getName() << "' of core '" <<
            sec.getProcessor().getName() << "' to " <<
            sectionOffset[ sec.getID() ] << " due to basic block '" <<
            b.getName() << "'." << endl );
        } else {
          size_t offset = sectionOffset[ sec.getID() ];

          // If the current basic block is the first one of a function, we have
          // to take care of the alignment.
          if ( ( b == f.getBasicBlocks().front().get() ) &&
               ( sec.getBlock() != 0 ) ) {
            // If the current section offset is not properly aligned, round up.
            size_t alignment = 1 << sec.getBlock();
            if ( offset & ( alignment - 1 ) ) {
              offset = ( ( offset + alignment - 1 ) & ~( alignment - 1 ) );
              DOUT(
                "Applying section-internal alignment by " << sec.getBlock() <<
                " bits, resulting offset is " << offset << "." << endl );
            }
          }

          sectionOffset[ sec.getID() ] = offset + blockSize;
          sym.setBaseAddress( WIR_MemoryAddress( offset ) );

          if ( b == f.getBasicBlocks().front().get() ) {
            fsym.setBaseAddress( WIR_MemoryAddress( offset ) );
            DOUT(
              "Setting base address of function '" << f.getName() <<
              "' to 0x" << hex << fsym.mBaseAddress << "." << endl );
          }

          DOUT(
            "Setting base address of basic block '" << b.getName() <<
            "' to 0x" << hex << sym.mBaseAddress << "." << endl );
          DOUT(
            "Setting offset of section '" << sec.getName() << "' of core '" <<
            sec.getProcessor().getName() << "' to " <<
            sectionOffset[ sec.getID() ] << " due to basic block '" <<
            b.getName() << "'." << endl );
        }
      }
    }
  }

  // Update section sizes.
  for ( WIR_BaseProcessor &p : getComponents<WIR_BaseProcessor>() )
    for ( WIR_Section &sec : p ) {
      auto it = sectionOffset.find( sec.getID() );

      if ( it != sectionOffset.end() )
        sec.setLength( it->second );
      else
        sec.setLength( 0 );

      DOUT(
        "Setting length of section '" << sec.getName() << "' of core '" <<
        p.getName() << "' to 0x" << hex << sec.mLength << "." << endl );
    }
};


/*
  computePhysicalAddresses takes the start address of data objects, functions
  and basic blocks (stemming from computeSectionLayout) relative to their
  assigned section plus the ordering of the sections (stemming from
  computeMemoryLayout) within their assigned memory regions and determines
  the final physical memory addresses of all symbols in a system's symbol table.
*/
void WIR_System::computePhysicalAddresses( void )
{
  DSTART( "void WIR_System::computePhysicalAddresses()" );

  // Iterate over all memory regions.
  for ( WIR_MemoryRegion &r : getComponents<WIR_MemoryRegion>() ) {
    size_t regionOffset = r.getBaseAddress();
    auto &sectionList = mRegion2sections[ r.getID() ];

    // Iterate over all sections assigned to a memory region.
    for ( WIR_Section &sec : sectionList ) {
      size_t alignment;

      // If the section has a fixed offset, its physical address is the region's
      // base address plus the section's fixed offset within the region.
      if ( sec.isStartSet() )
        regionOffset = r.getBaseAddress() + sec.getStart();

      // Check whether a section with the same name has already been assembled
      // into the current memory region.
      auto &sectionNameMap = mHighestAddresses[ r.getID() ];
      auto it = sectionNameMap.find( sec.getName() );
      DDECLARE( bool internalAlign = false );
      DDECLARE( size_t algn = 0 );

      if ( it != sectionNameMap.end() ) {
        // A section with the same name actually already exists inside the
        // current region. We thus fetch the highest address of this previous
        // section with the same name and place the current section directly
        // after.
        regionOffset = it->second;

        DOUT(
          "Memory region '" << r.getName() << "' already contains a section " <<
          "with name '" << sec.getName() << "', setting offset to 0x" << hex <<
          regionOffset << "." << endl );
        DACTION( internalAlign = true );

        // Apply section-internal alignment in this case.
        alignment = 1 << sec.getBlock();
        DACTION( algn = sec.getBlock() );
      } else {
        // A completely new section for the current region. Apply section-
        // external alignment.
        alignment = 1 << sec.getAlignment();
        DACTION( algn = sec.getAlignment() );
      }

      // If the current region offset is not properly aligned, round up.
      if ( regionOffset & ( alignment - 1 ) ) {
        regionOffset =
          ( ( regionOffset + alignment - 1 ) & ~( alignment - 1 ) );
        DOUT(
          "Applying section-" << string( internalAlign ? "in" : "ex" ) <<
          "ternal alignment by " << algn << " bits, resulting offset is " <<
          regionOffset << "." << endl );
      }

      // Iterate over all symbols assigned to the current section.
      auto &symbols = mSymbolsOfSection[ sec.getID() ];
      for ( WIR_Symbol &sym : symbols ) {
        // The symbol's physical address is the sum of its offset within its
        // section and the offset of the section within its region.
        sym.setBaseAddress(
          WIR_MemoryAddress( regionOffset + sym.mBaseAddress ) );

        DACTION(
          DOUT( "Setting base address of " );

          if ( sym.getType() == WIR_SymbolType::block )
            DOUT( "basic block '" << sym.getBasicBlock().getName() );
          else

          if ( sym.getType() == WIR_SymbolType::data )
            DOUT( "data object '" << sym.getData().getName() );
          else
            DOUT( "function '" << sym.getFunction().getName() );

          DOUT( "' to 0x" << hex << sym.mBaseAddress << "." << endl );
        );
      }

      // Increase the region offset by the current section's length.
      regionOffset += sec.mLength;

      if ( ( it == sectionNameMap.end() ) || ( regionOffset > it->second ) )
        // Store currently highest address for section name.
        sectionNameMap[ sec.getName() ] = regionOffset;
    }
  }
};

}       // namespace WIR
