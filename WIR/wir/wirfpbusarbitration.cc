/*

   This source file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Intermediate Representation Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2015 - 2022, Heiko Falk, Timon Kelter.

   Hamburg University of Technology (TUHH)
   Institute of Embedded Systems
   21071 Hamburg
   Germany

   http://www.tuhh.de/es/esd/research/wcc

*/

/*!
  @file wirfpbusarbitration.cc
  @brief This file implements a fixed priority %WIR bus arbitration policy.

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

/*!
  @brief Default constructor.

  @param[in] n An unsigned integer denoting the number of processor cores
                that are managed by this fixed-priority bus arbiter.

  This constructor does not assign any priority to the cores. This has to be
  done explicitly using setPriority().

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
WIR_FixedPriorityBusArbitration::WIR_FixedPriorityBusArbitration( unsigned int n ) :
  WIR_BusArbitration {},
  mNumberOfCores { n },
  mSort { true }
{
  DSTART(
    "WIR_FixedPriorityBusArbitration::WIR_FixedPriorityBusArbitration(unsigned int)" );
};


/*
  Destructor.
*/
WIR_FixedPriorityBusArbitration::~WIR_FixedPriorityBusArbitration( void )
{
  DSTART(
    "virtual WIR_FixedPriorityBusArbitration::~WIR_FixedPriorityBusArbitration()" );
};


/*
  This operator compares two WIR_BusArbitrations for equality.
*/
bool WIR_FixedPriorityBusArbitration::operator == ( const WIR_BusArbitration &__o ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto *p = dynamic_cast<const WIR_FixedPriorityBusArbitration *>( &__o );
  return( ( p != nullptr ) && ( mPriorities == p->mPriorities ) );
};


/*
  This operator compares two WIR_BusArbitrations for inequality.
*/
bool WIR_FixedPriorityBusArbitration::operator != ( const WIR_BusArbitration &__o ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  return( !( *this == __o ) );
};


/*
  getPriority returns a core's arbitration priority.

  Priority values are allowed from the interval [0, mNumberOfCores], a
  numerically high value denotes a high priority. A priority of 0 indicates that
  the given core is not considered for arbitration.
*/
WIR_FixedPriorityBusArbitration::Priority WIR_FixedPriorityBusArbitration::getPriority( const WIR_BaseProcessor &c ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto it = mPriorities.find( c.getID() );

  if ( it != mPriorities.end() )
    return( it->second );

  return( 0 );
};


/*
  getPriorities returns all cores' priorities for this bus.

  Numerically larger values denote higher priorities. If the system features n
  cores, then the priority values must be unique for each core, and they must be
  from the range [1, n]. A priority of 0 indicates that the given core is not
  considered for arbitration.
*/
const map<WIR_id_t, WIR_FixedPriorityBusArbitration::Priority> &WIR_FixedPriorityBusArbitration::getPriorities( void ) const
{
  DSTART(
    "const map<WIR_id_t, unsigned int>& WIR_FixedPriorityBusArbitration::getPriorities() const" );

  return( mPriorities );
};


/*
  getNumberOfCores returns the overall number of cores for this fixed priority
  arbiter.
*/
unsigned int WIR_FixedPriorityBusArbitration::getNumberOfCores( void ) const
{
  DSTART(
    "unsigned int WIR_FixedPriorityBusArbitration::getNumberOfCores() const" );

  return( mNumberOfCores );
};


/*
  getNumberOfArbitratedCores returns the number of cores with non-zero priority.

  Cores with priority 0 are not considered for arbitration. This scenario only
  makes sense if fixed priority arbitration is combined with some other
  arbitration policy like, e.g., priority division.
*/
unsigned int WIR_FixedPriorityBusArbitration::getNumberOfArbitratedCores( void ) const
{
  DSTART(
    "virtual unsigned int WIR_FixedPriorityBusArbitration::getNumberOfArbitratedCores() const" );

  unsigned int res = 0;

  for ( auto p : mPriorities )
    if ( p.second != 0 )
      ++res;

  return( res );
};


/*
  getCoreOrder returns the vector of cores managed by this arbitration policy,
  ordered by priority (highest priority first).
*/
const vector<WIR_id_t> &WIR_FixedPriorityBusArbitration::getCoreOrder( void ) const
{
  DSTART(
    "const vector<WIR_id_t>& WIR_FixedPriorityBusArbitration::getCoreOrder() const" );

  if ( mSort ) {
    // Sort processor core IDs.
    vector<WIR_id_t> res( mPriorities.size() );
    auto it = mPriorities.begin();
    for ( unsigned int pos = 0; pos < mPriorities.size(); ++pos, ++it )
      res[ pos ] = it->first;
    sort(
      res.begin(), res.end(),
      [&]( WIR_id_t i1, WIR_id_t i2 ) {
        return( mPriorities.at( i1 ) > mPriorities.at( i2 ) );
      } );

    mCoreOrder = move( res );
    mSort = false;
  }

  return( mCoreOrder );
};


//
// Protected class methods
//

/*
  clone creates a copy of a fixed priority bus arbiter.
*/
WIR_FixedPriorityBusArbitration *WIR_FixedPriorityBusArbitration::clone( void ) const
{
  DSTART(
    "virtual WIR_FixedPriorityBusArbitration* WIR_FixedPriorityBusArbitration::clone() const" );

  return( new WIR_FixedPriorityBusArbitration( *this ) );
};


/*
  setPriority assigns an arbitration priority to a core.

  Priority values are allowed from the interval [0, mNumberOfCores], a
  numerically high value denotes a high priority. A priority of 0 indicates that
  the given core is not considered for arbitration.

  setPriority asserts if the given priority value is already used for some other
  core.
*/
void WIR_FixedPriorityBusArbitration::setPriority( const WIR_BaseProcessor &c,
                                                   // cppcheck-suppress passedByValue
                                                   WIR_FixedPriorityBusArbitration::Priority p )
{
  DSTART(
    "void WIR_FixedPriorityBusArbitration::setPriority(const WIR_BaseProcessor&, ::WIR_FixedPriorityBusArbitration::Priority)" );

  ufAssert( p <= mNumberOfCores );
  if ( p != 0 )
    for ( auto p1 : mPriorities )
      ufAssertT(
        p1.second != p,
        "Priority " << p << " already used for some other core." );

  mPriorities[ c.getID() ] = p;
  mSort = true;
};

}       // namespace WIR
