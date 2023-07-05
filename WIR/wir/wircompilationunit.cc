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
  @file wircompilationunit.cc
  @brief This file implements %WIR compilation units (i.e., source code file).

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
#include <list>
#include <map>
#include <vector>

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
  Default constructor creating an empty compilation unit.
*/
WIR_CompilationUnit::WIR_CompilationUnit( void ) :
  WIR_ID_API {},
  WIR_Container_API {},
  WIR_Name_API {},
  mSystemPointer { nullptr },
  mDontOptimize { false }
{
  DSTART( "WIR_CompilationUnit::WIR_CompilationUnit()" );
};


/*
  Copy constructor.

  When copying a compilation unit that is inserted in some %WIR system, the
  resulting copy will not be inserted in a system.
*/
WIR_CompilationUnit::WIR_CompilationUnit( const WIR_CompilationUnit &__o ) :
  WIR_ID_API { __o },
  WIR_Container_API { __o },
  WIR_Name_API { __o },
  mSystemPointer { nullptr }
{
  DSTART(
    "WIR_CompilationUnit::WIR_CompilationUnit(const WIR_CompilationUnit&)" );

  copyCompilationUnit( __o );
};


/*
  Move constructor.

  Trying to move a compilation unit that is inserted in some %WIR system results
  in an assertion, since you are not allowed to move a compilation unit whose
  ownership is managed by a system.
*/
WIR_CompilationUnit::WIR_CompilationUnit( WIR_CompilationUnit &&__o ) :
  WIR_ID_API { move( __o ) },
  WIR_Container_API { move( __o ) },
  WIR_Name_API { move( __o ) },
  mFunctions { move( __o.mFunctions ) },
  mFunctionReferences { move( __o.mFunctionReferences ) },
  mSystemPointer { nullptr },
  mData { move( __o.mData ) },
  mDataReferences { move( __o.mDataReferences ) },
  mDontOptimize { __o.mDontOptimize }
{
  DSTART( "WIR_CompilationUnit::WIR_CompilationUnit(WIR_CompilationUnit&&)" );

  ufAssertT(
    __o.mSystemPointer == nullptr,
    "Invalid attempt to move compilation unit '" << __o.getName() <<
    "' out of its owning system '" << __o.getSystem().getName() << "'." );

  __o.mFunctions.clear();
  __o.mFunctionReferences.clear();
  __o.mData.clear();
  __o.mDataReferences.clear();

  // Adjust the parent IDs of the compilation unit's functions.
  for ( auto &f : mFunctions )
    f.onInsert( this );

  // Adjust the parent IDs of the compilation unit's data objects.
  for ( auto &d : mData )
    d.onInsert( this );
};


/*
  Destructor.
*/
WIR_CompilationUnit::~WIR_CompilationUnit( void )
{
  DSTART( "virtual WIR_CompilationUnit::~WIR_CompilationUnit()" );

  clearFunctions();
  clearData();
};


/*
  Copy-assignment operator.

  When copying a compilation unit that is inserted in some %WIR system, the
  resulting copy will not be inserted in a system.
*/
WIR_CompilationUnit & WIR_CompilationUnit::operator = ( const WIR_CompilationUnit &__o )
{
  DSTART(
    "WIR_CompilationUnit& WIR_CompilationUnit::operator=(const WIR_CompilationUnit&)" );

  WIR_Container_API::operator = ( __o );
  WIR_Name_API::operator = ( __o );

  mSystemPointer = nullptr;

  copyCompilationUnit( __o );

  return( *this );
};


/*
  Move-assignment operator.

  Trying to move a compilation unit that is inserted in some %WIR system results
  in an assertion, since you are not allowed to move a compilation unit whose
  ownership is managed by a system.
*/
WIR_CompilationUnit & WIR_CompilationUnit::operator = ( WIR_CompilationUnit &&__o )
{
  DSTART(
    "WIR_CompilationUnit& WIR_CompilationUnit::operator=(WIR_CompilationUnit&&)" );

  ufAssertT(
    __o.mSystemPointer == nullptr,
    "Invalid attempt to move compilation unit '" << __o.getName() <<
    "' out of its owning system '" << __o.getSystem().getName() << "'." );

  WIR_Container_API::operator = ( move( __o ) );
  WIR_Name_API::operator = ( move( __o ) );

  mFunctions = move( __o.mFunctions );
  __o.mFunctions.clear();
  mFunctionReferences = move( __o.mFunctionReferences );
  __o.mFunctionReferences.clear();
  mData = move( __o.mData );
  __o.mData.clear();
  mDataReferences = move( __o.mDataReferences );
  __o.mDataReferences.clear();
  mSystemPointer = nullptr;
  mDontOptimize = __o.mDontOptimize;

  // Adjust the parent IDs of the compilation unit's functions.
  for ( auto &f : mFunctions )
    f.onInsert( this );

  // Adjust the parent IDs of the compilation unit's data objects.
  for ( auto &d : mData )
    d.onInsert( this );

  return( *this );
};


//
// API implementations.
//

WIR_INSERTION_IMPL( WIR_System, System, WIR_CompilationUnit );


WIR_Function & WIR_CompilationUnit::pushBackFunction( const WIR_Function &o )
{
  DSTART(
    "WIR_Function& WIR_CompilationUnit::pushBackFunction(const WIR_Function&)" );

  checkDontOptimize();

  mFunctions.push_back( o );
  mFunctions.back().onInsert( this );
  mFunctionReferences.push_back( mFunctions.back() );

  insertSymbols( mFunctions.back() );

  return( mFunctions.back() );
};


WIR_Function & WIR_CompilationUnit::pushBackFunction( WIR_Function &&o )
{
  DSTART(
    "WIR_Function& WIR_CompilationUnit::pushBackFunction(WIR_Function&&)" );

  checkDontOptimize();

  mFunctions.emplace_back( move( o ) );
  mFunctions.back().onInsert( this );
  mFunctionReferences.push_back( mFunctions.back() );

  insertSymbols( mFunctions.back() );

  return( mFunctions.back() );
};


WIR_Function & WIR_CompilationUnit::pushFrontFunction( const WIR_Function &o )
{
  DSTART(
    "WIR_Function& WIR_CompilationUnit::pushFrontFunction(const WIR_Function&)" );

  checkDontOptimize();

  mFunctions.push_front( o );
  mFunctions.front().onInsert( this );
  mFunctionReferences.push_front( mFunctions.front() );

  insertSymbols( mFunctions.front() );

  return( mFunctions.front() );
};


WIR_Function & WIR_CompilationUnit::pushFrontFunction( WIR_Function &&o )
{
  DSTART(
    "WIR_Function& WIR_CompilationUnit::pushFrontFunction(WIR_Function&&)" );

  checkDontOptimize();

  mFunctions.emplace_front( move( o ) );
  mFunctions.front().onInsert( this );
  mFunctionReferences.push_front( mFunctions.front() );

  insertSymbols( mFunctions.front() );

  return( mFunctions.front() );
};


std::list<std::reference_wrapper<WIR_Function>>::iterator WIR_CompilationUnit::insertFunction( std::list<std::reference_wrapper<WIR_Function>>::const_iterator pos,
                                                                                               const WIR_Function &o )
{
  DSTART(
    "list<reference_wrapper<WIR_Function> >::iterator WIR_CompilationUnit::insertFunction(list<reference_wrapper<WIR_Function> >::const_iterator, const WIR_Function&)" );

  checkDontOptimize();

  auto it = mFunctions.begin();
  for ( auto itr = mFunctionReferences.begin(); itr != pos; ++itr, ++it ) ;
  auto it1 = mFunctions.insert( it, o );
  (*it1).onInsert( this );

  insertSymbols( *it1 );

  return( mFunctionReferences.insert( pos, *it1 ) );
};


std::list<std::reference_wrapper<WIR_Function>>::iterator WIR_CompilationUnit::insertFunction( std::list<std::reference_wrapper<WIR_Function>>::const_iterator pos,
                                                                                               WIR_Function &&o )
{
  DSTART(
    "list<reference_wrapper<WIR_Function> >::iterator WIR_CompilationUnit::insertFunction(list<reference_wrapper<WIR_Function> >::const_iterator, WIR_Function&&)" );

  checkDontOptimize();

  auto it = mFunctions.begin();
  for ( auto itr = mFunctionReferences.begin(); itr != pos; ++itr, ++it ) ;
  auto it1 = mFunctions.insert( it, move( o ) );
  (*it1).onInsert( this );

  insertSymbols( *it1 );

  return( mFunctionReferences.insert( pos, *it1 ) );
};


std::list<std::reference_wrapper<WIR_Function>>::iterator WIR_CompilationUnit::replaceFunction( std::list<std::reference_wrapper<WIR_Function>>::const_iterator pos,
                                                                                                const WIR_Function &o )
{
  DSTART(
    "list<reference_wrapper<WIR_Function> >::iterator WIR_CompilationUnit::replaceFunction(list<reference_wrapper<WIR_Function> >::const_iterator, const WIR_Function&)" );

  checkDontOptimize();

  auto it1 = mFunctions.begin();
  for ( auto itr = mFunctionReferences.begin(); itr != pos; ++itr, ++it1 ) ;
  auto it2 = mFunctions.insert( it1, o );
  (*it2).onInsert( this );

  insertSymbols( *it2 );

  auto it = mFunctionReferences.insert( pos, *it2 );
  mFunctionReferences.erase( pos );
  mFunctions.erase( it1 );
  return( it );
};


std::list<std::reference_wrapper<WIR_Function>>::iterator WIR_CompilationUnit::replaceFunction( std::list<std::reference_wrapper<WIR_Function>>::const_iterator pos,
                                                                                                WIR_Function &&o )
{
  DSTART(
    "list<reference_wrapper<WIR_Function> >::iterator WIR_CompilationUnit::replaceFunction(list<reference_wrapper<WIR_Function> >::const_iterator, WIR_Function&&)" );

  checkDontOptimize();

  auto it1 = mFunctions.begin();
  for ( auto itr = mFunctionReferences.begin(); itr != pos; ++itr, ++it1 ) ;
  auto it2 = mFunctions.insert( it1, move( o ) );
  (*it2).onInsert( this );

  insertSymbols( *it2 );

  auto it = mFunctionReferences.insert( pos, *it2 );
  mFunctionReferences.erase( pos );
  mFunctions.erase( it1 );
  return( it );
};


void WIR_CompilationUnit::popBackFunction( void )
{
  DSTART( "void WIR_CompilationUnit::popBackFunction()" );

  checkDontOptimize();

  if ( mFunctionReferences.empty() )
    return;

  mFunctionReferences.pop_back();
  mFunctions.pop_back();
};


void WIR_CompilationUnit::popFrontFunction( void )
{
  DSTART( "void WIR_CompilationUnit::popFrontFunction()" );

  checkDontOptimize();

  if ( mFunctionReferences.empty() )
    return;

  mFunctionReferences.pop_front();
  mFunctions.pop_front();
};


std::list<std::reference_wrapper<WIR_Function>>::iterator WIR_CompilationUnit::eraseFunction( std::list<std::reference_wrapper<WIR_Function>>::const_iterator pos )
{
  DSTART(
    "list<reference_wrapper<WIR_Function> >::iterator WIR_CompilationUnit::eraseFunction(list<reference_wrapper<WIR_Function> >::const_iterator)" );

  checkDontOptimize();

  auto it1 = mFunctions.begin();
  for ( auto itr = mFunctionReferences.begin(); itr != pos; ++itr, ++it1 ) ;
  auto it = mFunctionReferences.erase( pos );
  mFunctions.erase( it1 );
  return( it );
};


void WIR_CompilationUnit::clearFunctions( void )
{
  DSTART( "void WIR_CompilationUnit::clearFunctions()" );

  checkDontOptimize();

  mFunctionReferences.clear();
  mFunctions.clear();
};


const std::list<std::reference_wrapper<WIR_Function>> &WIR_CompilationUnit::getFunctions( void ) const
{
  DSTART(
    "const list<reference_wrapper<WIR_Function> >& WIR_CompilationUnit::getFunctions() const" );

  return( mFunctionReferences );
};


bool WIR_CompilationUnit::containsFunction( WIR_id_t id ) const
{
  DSTART( "bool WIR_CompilationUnit::containsFunction(WIR_id_t) const" );

  for ( auto &item : mFunctions )
    if ( item.getID() == id )
      return( true );

  return( false );
};


bool WIR_CompilationUnit::containsFunction( const WIR_Function &o ) const
{
  DSTART(
    "bool WIR_CompilationUnit::containsFunction(const WIR_Function&) const" );

  for ( auto &item : mFunctions )
    if ( item == o )
      return( true );

  return( false );
};


std::list<std::reference_wrapper<WIR_Function>>::const_iterator WIR_CompilationUnit::findFunction( WIR_id_t id ) const
{
  DSTART(
    "list<reference_wrapper<WIR_Function> >::const_iterator WIR_CompilationUnit::findFunction(WIR_id_t) const" );

  for ( auto it = mFunctionReferences.begin();
        it != mFunctionReferences.end(); ++it )
    if ( (*it).get().getID() == id )
      return( it );

  return( mFunctionReferences.end() );
};


std::list<std::reference_wrapper<WIR_Function>>::const_iterator WIR_CompilationUnit::findFunction( const WIR_Function &o ) const
{
  DSTART(
    "list<reference_wrapper<WIR_Function> >::const_iterator WIR_CompilationUnit::findFunction(const WIR_Function&) const" );

  for ( auto it = mFunctionReferences.begin();
        it != mFunctionReferences.end(); ++it )
    if ( (*it).get() == o )
      return( it );

  return( mFunctionReferences.end() );
};


/*
  begin returns an iterator to the first function of a compilation unit.
*/
std::list<std::reference_wrapper<WIR_Function>>::const_iterator WIR_CompilationUnit::begin( void ) const
{
  DSTART(
    "list<reference_wrapper<WIR_Function> >::const_iterator WIR_CompilationUnit::begin() const" );

  return( mFunctionReferences.begin() );
};


/*
  end returns an iterator to the end of a compilation unit's function list.
*/
std::list<std::reference_wrapper<WIR_Function>>::const_iterator WIR_CompilationUnit::end( void ) const
{
  DSTART(
    "list<reference_wrapper<WIR_Function> >::const_iterator WIR_CompilationUnit::end() const" );

  return( mFunctionReferences.end() );
};


/*
  pushBackData adds a new WIR_Data at the end of list mData, after its current
  last element.
*/
WIR_Data &WIR_CompilationUnit::pushBackData( const WIR_Data &o )
{
  DSTART( "WIR_Data& WIR_CompilationUnit::pushBackData(const WIR_Data&)" );

  checkDontOptimize();

  mData.push_back( o );
  mData.back().onInsert( this );
  mDataReferences.push_back( mData.back() );

  insertSymbol( mData.back() );

  return( mData.back() );
};


/*
  pushBackData adds a new WIR_Data at the end of list mData, after its current
  last element.
*/
WIR_Data &WIR_CompilationUnit::pushBackData( WIR_Data &&o )
{
  DSTART( "WIR_Data& WIR_CompilationUnit::pushBackData(WIR_Data&&)" );

  checkDontOptimize();

  mData.emplace_back( move( o ) );
  mData.back().onInsert( this );
  mDataReferences.push_back( mData.back() );

  insertSymbol( mData.back() );

  return( mData.back() );
};


/*
  pushFrontData adds a new WIR_Data at the beginning of list mData, right before
  its current first element.
*/
WIR_Data &WIR_CompilationUnit::pushFrontData( const WIR_Data &o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  checkDontOptimize();

  mData.push_front( o );
  mData.front().onInsert( this );
  mDataReferences.push_front( mData.front() );

  insertSymbol( mData.front() );

  return( mData.front() );
};


/*
  pushFrontData adds a new WIR_Data at the beginning of list mData, right before
  its current first element.
*/
WIR_Data &WIR_CompilationUnit::pushFrontData( WIR_Data &&o )
{
  DSTART( "WIR_Data& WIR_CompilationUnit::pushFrontData(WIR_Data&&)" );

  checkDontOptimize();

  mData.emplace_front( move( o ) );
  mData.front().onInsert( this );
  mDataReferences.push_front( mData.front() );

  insertSymbol( mData.front() );

  return( mData.front() );
};


/*
  insertData inserts a new WIR_Data before the element at the specified
  position.
*/
std::list<std::reference_wrapper<WIR_Data>>::iterator WIR_CompilationUnit::insertData( std::list<std::reference_wrapper<WIR_Data>>::const_iterator pos,
                                                                                       const WIR_Data &o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  checkDontOptimize();

  auto it = mData.begin();
  for ( auto itr = mDataReferences.begin(); itr != pos; ++itr, ++it ) ;
  auto it1 = mData.insert( it, o );
  (*it1).onInsert( this );

  insertSymbol( *it1 );

  return( mDataReferences.insert( pos, *it1 ) );
};


/*
  insertData inserts a new WIR_Data before the element at the specified
  position.
*/
std::list<std::reference_wrapper<WIR_Data>>::iterator WIR_CompilationUnit::insertData( std::list<std::reference_wrapper<WIR_Data>>::const_iterator pos,
                                                                                       WIR_Data &&o )
{
  DSTART(
    "list<reference_wrapper<WIR_Data> >::iterator WIR_CompilationUnit::insertData(list<reference_wrapper<WIR_Data> >::const_iterator, WIR_Data&&)" );

  checkDontOptimize();

  auto it = mData.begin();
  for ( auto itr = mDataReferences.begin(); itr != pos; ++itr, ++it ) ;
  auto it1 = mData.insert( it, move( o ) );
  (*it1).onInsert( this );

  insertSymbol( *it1 );

  return( mDataReferences.insert( pos, *it1 ) );
};


/*
  replaceData replaces the list element at the specified position by a new
  WIR_Data.
*/
std::list<std::reference_wrapper<WIR_Data>>::iterator WIR_CompilationUnit::replaceData( std::list<std::reference_wrapper<WIR_Data>>::const_iterator pos,
                                                                                        const WIR_Data &o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  checkDontOptimize();

  auto it1 = mData.begin();
  for ( auto itr = mDataReferences.begin(); itr != pos; ++itr, ++it1 ) ;
  auto it2 = mData.insert( it1, o );
  (*it2).onInsert( this );

  insertSymbol( *it2 );

  auto it = mDataReferences.insert( pos, *it2 );
  mDataReferences.erase( pos );
  mData.erase( it1 );
  return( it );
};


/*
  replaceData replaces the list element at the specified position by a new
  WIR_Data.
*/
std::list<std::reference_wrapper<WIR_Data>>::iterator WIR_CompilationUnit::replaceData( std::list<std::reference_wrapper<WIR_Data>>::const_iterator pos,
                                                                                        WIR_Data &&o )
{
  DSTART(
    "list<reference_wrapper<WIR_Data> >::iterator WIR_CompilationUnit::replaceData(list<reference_wrapper<WIR_Data> >::const_iterator, WIR_Data&&)" );

  checkDontOptimize();

  auto it1 = mData.begin();
  for ( auto itr = mDataReferences.begin(); itr != pos; ++itr, ++it1 ) ;
  auto it2 = mData.insert( it1, move( o ) );
  (*it2).onInsert( this );

  insertSymbol( *it2 );

  auto it = mDataReferences.insert( pos, *it2 );
  mDataReferences.erase( pos );
  mData.erase( it1 );
  return( it );
};


/*
  popBackData removes the last WIR_Data from list mData.
*/
void WIR_CompilationUnit::popBackData( void )
{
  DSTART( "void WIR_CompilationUnit::popBackData()" );

  checkDontOptimize();

  if ( mDataReferences.empty() )
    return;

  mDataReferences.pop_back();
  mData.pop_back();
};


/*
  popFrontData removes the first WIR_Data from list mData.
*/
void WIR_CompilationUnit::popFrontData( void )
{
  DSTART( "void WIR_CompilationUnit::popFrontData()" );

  checkDontOptimize();

  if ( mDataReferences.empty() )
    return;

  mDataReferences.pop_front();
  mData.pop_front();
};


/*
  eraseData removes a single WIR_Data from the specified position.
*/
std::list<std::reference_wrapper<WIR_Data>>::iterator WIR_CompilationUnit::eraseData( std::list<std::reference_wrapper<WIR_Data>>::const_iterator pos )
{
  DSTART(
    "list<reference_wrapper<WIR_Data> >::iterator WIR_CompilationUnit::eraseData(list<reference_wrapper<WIR_Data> >::const_iterator)" );

  checkDontOptimize();

  auto it1 = mData.begin();
  for ( auto itr = mDataReferences.begin(); itr != pos; ++itr, ++it1 ) ;
  auto it = mDataReferences.erase( pos );
  mData.erase( it1 );
  return( it );
};


/*
  clearData removes all elements from list mData.
*/
void WIR_CompilationUnit::clearData( void )
{
  DSTART( "void WIR_CompilationUnit::clearData()" );

  checkDontOptimize();

  mDataReferences.clear();
  mData.clear();
};


/*
  getData returns the list mDataReferences.
*/
const std::list<std::reference_wrapper<WIR_Data>> &WIR_CompilationUnit::getData( void ) const
{
  DSTART(
    "const list<reference_wrapper<WIR_Data> >& WIR_CompilationUnit::getData() const" );

  return( mDataReferences );
};


/*
  containsData returns whether list mData contains a WIR_Data with the specified
  ID.
*/
bool WIR_CompilationUnit::containsData( WIR_id_t id ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  for ( auto &item : mData )
    if ( item.getID() == id )
      return( true );

  return( false );
};


/*
  containsData returns whether list mData contains the specified WIR_Data.
*/
bool WIR_CompilationUnit::containsData( const WIR_Data &o ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  for ( auto &item : mData )
    if ( item == o )
      return( true );

  return( false );
};


/*
  findData finds a WIR_Data with the specified ID in list mData.
*/
std::list<std::reference_wrapper<WIR_Data>>::const_iterator WIR_CompilationUnit::findData( WIR_id_t id ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  for ( auto it = mDataReferences.begin(); it != mDataReferences.end(); ++it )
    if ( (*it).get().getID() == id )
      return( it );

  return( mDataReferences.end() );
};


/*
  findData finds the specified WIR_Data in list mData.
*/
std::list<std::reference_wrapper<WIR_Data>>::const_iterator WIR_CompilationUnit::findData( const WIR_Data &o ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  for ( auto it = mDataReferences.begin(); it != mDataReferences.end(); ++it )
    if ( (*it).get() == o )
      return( it );

  return( mDataReferences.end() );
};


/*
  getVREGs determines all virtual registers that occur in this compilation
  unit's functions.
*/
WIR_VirtualRegisterSet WIR_CompilationUnit::getVREGs( void ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  WIR_VirtualRegisterSet res;

  for ( WIR_Function &f : getFunctions() )
    for ( WIR_VirtualRegister &r : f.getVREGs() )
      res.insert( r );

  return( res );
};


/*
  setDontOptimize sets whether a compilation unit can be modified or must not be
  changed by some optimization or transformation.
*/
void WIR_CompilationUnit::setDontOptimize( bool f )
{
  DSTART( "void WIR_CompilationUnit::setDontOptimize(bool)" );

  mDontOptimize = f;
};


/*
  getDontOptimize returns whether a compilation unit can be modified or must not
  be changed by some optimization or transformation.

  A compilation unit must not be modified if the compilation unit by itself has
  been marked as such using setDontOptimize, or if it is inserted into a WIR
  system that in turn must not be modified.
*/
bool WIR_CompilationUnit::getDontOptimize( void ) const
{
  DSTART( "bool WIR_CompilationUnit::getDontOptimize() const" );

  return( mDontOptimize || ( isInserted() && getSystem().getDontOptimize() ) );
};


/*
  The << operator dumps a WIR compilation unit to an output stream.

  By applying processor-specific I/O manipulators to the output stream
  beforehand, this << operator can flexibly emit valid assembly output for
  arbitrary processor architectures.
*/
std::ostream & operator << ( std::ostream &os, const WIR_CompilationUnit &c )
{
  DSTART( "ostream& operator<<(ostream&, const WIR_CompilationUnit&)" );

  WIR_Registry::getCompilationUnitDumper( os.iword( WIR_ProcessorIO() ) )(
    os, c );

  return( os );
};


//
// Private class methods
//

/*
  Dummy function for adding functions which does nothing.

  It only serves to terminate the recursion of the variadic method
  addFunctions.
*/
void WIR_CompilationUnit::addFunctions( void ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  copyCompilationUnit performs actions common to the copy constructor and copy
  assignment operator of WIR compilation units.
*/
void WIR_CompilationUnit::copyCompilationUnit( const WIR_CompilationUnit &__o )
{
  DSTART(
    "void WIR_CompilationUnit::copyCompilationUnit(const WIR_CompilationUnit&)" );

  mDontOptimize = false;

  // Copy functions.

  // Sort the IDs of functions from __o.
  vector<WIR_id_t> originalIDs;
  map<WIR_id_t, WIR_Function *> oldFunctionIDMap;
  for ( WIR_Function &f : __o.mFunctionReferences ) {
    originalIDs.push_back( f.getID() );
    oldFunctionIDMap[ f.getID() ] = &f;
  }
  sort( originalIDs.begin(), originalIDs.end() );

  // Copy functions according to the sorted order.
  map<WIR_id_t, WIR_Function *> functionIDMap;
  list<WIR_Function> newFunctions;
  for ( auto id : originalIDs ) {
    newFunctions.push_back( *(oldFunctionIDMap[ id ]) );
    functionIDMap[ id ] = &(newFunctions.back());
  }

  // Insert copied functions into this compilation unit in the right order.
  clearFunctions();
  for ( WIR_Function &f : __o.mFunctionReferences ) {
    pushBackFunction( move( *(functionIDMap[ f.getID() ]) ) );
    functionIDMap[ f.getID() ] = &(mFunctions.back());
  }

  // Patch all function label parameters of copied operations.
  // Scan all function label parameters of the copied operations and replace
  // them by the copied function labels.
  for ( WIR_Function &f : getFunctions() )
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

  // Copy data objects.

  // Sort the IDs of data objects from __o.
  originalIDs.clear();
  map<WIR_id_t, WIR_Data *> oldDataIDMap;
  for ( WIR_Data &d : __o.mDataReferences ) {
    originalIDs.push_back( d.getID() );
    oldDataIDMap[ d.getID() ] = &d;
  }
  sort( originalIDs.begin(), originalIDs.end() );

  // Copy data objects according to the sorted order.
  map<WIR_id_t, WIR_Data *> dataIDMap;
  list<WIR_Data> newData;
  for ( auto id : originalIDs ) {
    newData.push_back( *(oldDataIDMap[ id ]) );
    dataIDMap[ id ] = &(newData.back());
  }

  // Insert copied data objects into this compilation unit in the right order.
  clearData();
  for ( WIR_Data &d : __o.mDataReferences )
    pushBackData( move( *(dataIDMap[ d.getID() ]) ) );

  mDontOptimize = __o.mDontOptimize;
};


/*
  checkDontOptimize checks whether a compilation unit must not be modified.

  If this compilation unit must not be modified, checkDontOptimize asserts.
*/
void WIR_CompilationUnit::checkDontOptimize( void ) const
{
  DSTART( "void WIR_CompilationUnit::checkDontOptimize() const" );

  ufAssertT(
    !getDontOptimize(),
    "Illegal attempt to modify a compilation unit that is set as " <<
    "'don't optimize'!" );
};


/*
  For all basic blocks of the specified function and for the function itself,
  insertSymbols adds corresponding symbols to the system's symbol table.

  insertSymbols only creates new symbols if the current compilation unit is
  actually inserted into some %WIR system.
*/
void WIR_CompilationUnit::insertSymbols( const WIR_Function &f )
{
  DSTART( "void WIR_CompilationUnit::insertSymbols(const WIR_Function&)" );

  if ( isInserted() ) {
    WIR_System &sys = getSystem();
    sys.insertSymbol( f );

    for ( WIR_BasicBlock &b : f )
      sys.insertSymbol( b );
  }
};


/*
  For all the specified data object, insertSymbol adds a symbol to the system's
  symbol table.

  insertSymbol only creates new symbols if the current compilation unit is
  actually inserted into some %WIR system.
*/
void WIR_CompilationUnit::insertSymbol( const WIR_Data &d )
{
  DSTART( "void WIR_CompilationUnit::insertSymbol(const WIR_Data&)" );

  if ( isInserted() )
    getSystem().insertSymbol( d );
};

}       // namespace WIR
