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
  @file wirdata.cc
  @brief This file implements %WIR data objects.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include standard headers
#include <cstddef>

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
  Default constructor creating an empty data object.

  This constructor asserts if it is passed an empty string.
*/
WIR_Data::WIR_Data( const std::string &s ) :
  WIR_ID_API {},
  WIR_Container_API {},
  mCompilationUnitPointer { nullptr },
  mDontOptimize { false },
  mName { s },
  mSize { 0 }
{
  DSTART( "WIR_Data::WIR_Data(const string&)" );

  ufAssert( !s.empty() );
};


/*
  Default constructor creating an empty data object.

  This constructor asserts if it is passed an empty string.
*/
WIR_Data::WIR_Data( std::string &&s ) :
  WIR_ID_API {},
  WIR_Container_API {},
  mCompilationUnitPointer { nullptr },
  mDontOptimize { false },
  mSize { 0 }
{
  DSTART( "WIR_Data::WIR_Data(string&&)" );

  ufAssert( !s.empty() );
  mName = move( s );
  s.clear();
};


/*
  Copy constructor.

  When copying a data object that is inserted in some WIR compilation unit, the
  resulting copy will not be inserted in a compilation unit. Copying a data
  object implies that the newly created data is set as
  getDontOptimize() == false.
*/
WIR_Data::WIR_Data( const WIR_Data &__o ) :
  WIR_ID_API { __o },
  WIR_Container_API { __o },
  mCompilationUnitPointer { nullptr },
  mDontOptimize { false },
  mName { __o.mName },
  mSize { __o.mSize }
{
  DSTART( "WIR_Data::WIR_Data(const WIR_Data&)" );

  // Copy initialization data.
  clearInitData();
  for ( WIR_DataInit &i : __o.mInitDataReferences )
    pushBackInitData( i );
};


/*
  Move constructor.

  Trying to move a data object that is inserted in some WIR compilation unit
  results in an assertion, since you are not allowed to move data objects whose
  ownership is managed by a WIR compilation unit.
*/
WIR_Data::WIR_Data( WIR_Data &&__o ) :
  WIR_ID_API { move( __o ) },
  WIR_Container_API { move( __o ) },
  mCompilationUnitPointer { nullptr },
  mDontOptimize { __o.mDontOptimize },
  mName { move( __o.mName ) },
  mSize { move( __o.mSize ) },
  mInitData { move( __o.mInitData ) },
  mInitDataReferences { move( __o.mInitDataReferences ) }
{
  DSTART( "WIR_Data::WIR_Data(WIR_Data&&)" );

  ufAssertT(
    __o.mCompilationUnitPointer == nullptr,
    "Invalid attempt to move data object '" << __o.getName() <<
    "' out of its owning compilation unit '" <<
    __o.getCompilationUnit().getName() << "'." );

  __o.mDontOptimize = false;
  __o.mName.clear();
  __o.mSize = 0;
  __o.mInitData.clear();
  __o.mInitDataReferences.clear();
};


/*
  Destructor.
*/
WIR_Data::~WIR_Data( void )
{
  DSTART( "virtual WIR_Data::~WIR_Data()" );

  // If a data object is inserted indrectly into some WIR system, we have to
  // remove the data object from the system's symbol table.
  if ( isInserted() ) {
    WIR_CompilationUnit &c = getCompilationUnit();
    if ( c.isInserted() ) {
      WIR_System &s = c.getSystem();
      s.eraseSymbol( *this );
    }
  }
};


/*
  Copy-assignment operator.

  When copying a data object that is inserted in some WIR compilation unit, the
  resulting copy will not be inserted in a compilation unit. Copying a data
  object implies that the newly created data is set as
  getDontOptimize() == false.
*/
WIR_Data & WIR_Data::operator = ( const WIR_Data &__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  WIR_Container_API::operator = ( __o );
  checkDontOptimize();

  mCompilationUnitPointer = nullptr;
  mDontOptimize = false;
  mName = __o.mName;
  mSize = __o.mSize;

  // Copy initialization data.
  clearInitData();
  for ( WIR_DataInit &i : __o.mInitDataReferences )
    pushBackInitData( i );

  return( *this );
};


/*
  Move-assignment operator.

  Trying to move a data object that is inserted in some WIR compilation unit
  results in an assertion, since you are not allowed to move data objects whose
  ownership is managed by a WIR compilation unit.
*/
WIR_Data & WIR_Data::operator = ( WIR_Data &&__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  ufAssertT(
    __o.mCompilationUnitPointer == nullptr,
    "Invalid attempt to move data object '" << __o.getName() <<
    "' out of its owning compilation unit '" <<
    __o.getCompilationUnit().getName() << "'." );
  checkDontOptimize();

  WIR_Container_API::operator = ( move( __o ) );

  mCompilationUnitPointer = nullptr;
  mDontOptimize = __o.mDontOptimize;
  __o.mDontOptimize = false;
  mName = move( __o.mName );
  __o.mName.clear();
  mSize = move( __o.mSize );
  __o.mSize = 0;
  mInitData = move( __o.mInitData );
  __o.mInitData.clear();
  mInitDataReferences = move( __o.mInitDataReferences );
  __o.mInitDataReferences.clear();

  return( *this );
};


//
// API implementations.
//

WIR_INSERTION_IMPL( WIR_CompilationUnit, CompilationUnit, WIR_Data );


/*
  setName sets a data object's specific name.

  setName asserts if it is passed an empty string.
*/
void WIR_Data::setName( const std::string &s )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  checkDontOptimize();
  ufAssert( !s.empty() );

  mName = s;
};


/*
  setName sets a data object's specific name.

  setName asserts if it is passed an empty string.
*/
void WIR_Data::setName( std::string &&s )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  ufAssert( !s.empty() );

  checkDontOptimize();
  mName = move( s );
  s.clear();
};


/*
  getName returns a data object's specific name.
*/
std::string WIR_Data::getName( void ) const
{
  DSTART( "string WIR_Data::getName() const" );

  return( mName );
};


/*
  setSize sets a data object's size in bytes.
*/
void WIR_Data::setSize( std::size_t s )
{
  DSTART( "void WIR_Data::setSize(size_t)" );

  checkDontOptimize();
  mSize = s;
};


/*
  getSize returns a data object's size in bytes.
*/
std::size_t WIR_Data::getSize( void ) const
{
  DSTART( "size_t WIR_Data::getSize() const" );

  return( mSize );
};


/*
  isInitialized returns whether a data object is initialized or not.
*/
bool WIR_Data::isInitialized( void ) const
{
  DSTART( "bool WIR_Data::isInitialized() const" );

  return( !mInitData.empty() );
};


/*
  pushBackInitData adds a new WIR_DataInit at the end of list mInitData, after
  its current last element.

  The content of o is copied to the new list element.
*/
WIR_DataInit &WIR_Data::pushBackInitData( const WIR_DataInit &o )
{
  DSTART( "WIR_DataInit& WIR_Data::pushBackInitData(const WIR_DataInit&)" );

  checkDontOptimize();

  mInitData.push_back( o );
  mInitDataReferences.push_back( mInitData.back() );

  return( mInitData.back() );
};


/*
  pushBackInitData adds a new WIR_DataInit at the end of list mInitData, after
  its current last element.

  The content of o is moved to the new list element.
*/
WIR_DataInit &WIR_Data::pushBackInitData( WIR_DataInit &&o )
{
  DSTART( "WIR_DataInit& WIR_Data::pushBackInitData(WIR_DataInit&&)" );

  checkDontOptimize();

  mInitData.emplace_back( move( o ) );
  mInitDataReferences.push_back( mInitData.back() );

  return( mInitData.back() );
};


/*
  clearInitData removes all elements from list mInitData.

  This destroys all removed elements.
*/
void WIR_Data::clearInitData( void )
{
  DSTART( "void WIR_Data::clearInitData()" );

  checkDontOptimize();

  mInitDataReferences.clear();
  mInitData.clear();
};


/*
  getInitData returns the list mInitData.
*/
const std::list<std::reference_wrapper<WIR_DataInit>> &WIR_Data::getInitData( void ) const
{
  DSTART(
    "const list<reference_wrapper<WIR_DataInit> >& WIR_Data::getInitData() const" );

  return( mInitDataReferences );
};


/*
  begin returns an iterator to the first basic block of a function.
*/
std::list<std::reference_wrapper<WIR_DataInit>>::const_iterator WIR_Data::begin( void ) const
{
  DSTART(
    "list<reference_wrapper<WIR_DataInit> >::const_iterator WIR_Data::begin() const" );

  return( mInitDataReferences.begin() );
};


/*
  end returns an iterator to the end of a function's basic block list.
*/
std::list<std::reference_wrapper<WIR_DataInit>>::const_iterator WIR_Data::end( void ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  return( mInitDataReferences.end() );
};


/*
  foldSpaces folds subsequent space entries in the initialization list into one
  combined space entry.

  This destroys all removed elements.
*/
void WIR_Data::foldSpaces( void )
{
  DSTART( "void WIR_Data::foldSpaces()" );

  checkDontOptimize();

  size_t spaces = 0;

  auto it = mInitData.begin();
  auto itr = mInitDataReferences.begin();

  while ( it != mInitData.end() ) {
    auto &i = *it;

    if ( ( spaces > 0 ) && ( i.getType() != WIR_DataInitType::ispace ) ) {
      auto it1 = mInitData.insert( it, WIR_DataInit( spaces ) );
      mInitDataReferences.insert( itr, *it1 );
      spaces = 0;

      ++it;
      ++itr;
    } else

    if ( i.getType() == WIR_DataInitType::ispace ) {
      spaces += i.getSpace();
      itr = mInitDataReferences.erase( itr );
      it = mInitData.erase( it );
    } else {
      ++it;
      ++itr;
    }
  }
};


/*
  setDontOptimize sets whether a data object can be modified or must not be
  changed by some optimization or transformation.
*/
void WIR_Data::setDontOptimize( bool f )
{
  DSTART( "void WIR_Data::setDontOptimize(bool)" );

  mDontOptimize = f;
};


/*
  getDontOptimize returns whether a data object can be modified or must not be
  changed by some optimization or transformation.

  A data object must not be modified if the data by itself has been marked as
  such using setDontOptimize, or if it is inserted into a %WIR compilation unit
  that in turn must not be modified.
*/
bool WIR_Data::getDontOptimize( void ) const
{
  DSTART( "bool WIR_Data::getDontOptimize() const" );

  return(
    mDontOptimize ||
    ( isInserted() && getCompilationUnit().getDontOptimize() ) );
};


/*
  The << operator dumps a WIR data object to an output stream.

  By applying processor-specific I/O manipulators to the output stream
  beforehand, this << operator can flexibly emit valid assembly output for
  arbitrary processor architectures.
*/
std::ostream & operator << ( std::ostream &os, const WIR_Data &d )
{
  DSTART( "ostream& operator<<(ostream&, const WIR_Data&)" );

  WIR_Registry::getDataDumper( os.iword( WIR_ProcessorIO() ) )( os, d );

  return( os );
};


//
// Private class methods
//

/*
  checkDontOptimize checks whether a data object must not be modified.

  If this data object must not be modified, checkDontOptimize asserts.
*/
void WIR_Data::checkDontOptimize( void ) const
{
  DSTART( "void WIR_Data::checkDontOptimize() const" );

  ufAssertT(
    !getDontOptimize(),
    "Illegal attempt to modify a data object that is set as 'don't optimize'!" );
};

}       // namespace WIR
