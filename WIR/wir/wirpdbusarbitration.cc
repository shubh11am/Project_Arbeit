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
  @file wirpdbusarbitration.cc
  @brief This file implements a priority division %WIR bus arbitration policy.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include standard headers
#include <set>
#include <utility>

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
  Default constructor producing an empty PD parameter set.
*/
WIR_PriorityDivisionBusArbitration::WIR_PriorityDivisionBusArbitration( unsigned int n,
                                                                        unsigned int t,
                                                                        unsigned int d ) :
  WIR_FixedPriorityBusArbitration( n ),
  WIR_TDMABusArbitration( t, d ),
  mRecomputePDGrantWindows { true }
{
  DSTART(
    "WIR_PriorityDivisionBusArbitration::WIR_PriorityDivisionBusArbitration(unsigned int, unsigned int, unsigned int)" );
};


/*
  Constructor producing a PD parameter set that mimics the given TDMA
  parameters.

  This constructor simply copies all TDMA parameter values and sets all slot
  modes to TDMA.
*/
WIR_PriorityDivisionBusArbitration::WIR_PriorityDivisionBusArbitration( const WIR_TDMABusArbitration &t ) :
  WIR_FixedPriorityBusArbitration( t.getNumberOfArbitratedCores() ),
  WIR_TDMABusArbitration( t ),
  mRecomputePDGrantWindows { true }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  mSlotModes.resize( getNumberOfSlots() );
  for ( unsigned int i = 0; i < getNumberOfSlots(); ++i )
    mSlotModes.push_back( WIR_BusArbitrationType::tdma );

  mRecomputeGrantWindows = true;
};


/*
  Destructor.
*/
WIR_PriorityDivisionBusArbitration::~WIR_PriorityDivisionBusArbitration( void )
{
  DSTART(
    "virtual WIR_PriorityDivisionBusArbitration::~WIR_PriorityDivisionBusArbitration()" );
};


/*
  This operator compares two WIR_BusArbitrations for equality.
*/
bool WIR_PriorityDivisionBusArbitration::operator == ( const WIR_BusArbitration &__o ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto *p = dynamic_cast<const WIR_PriorityDivisionBusArbitration *>( &__o );
  return(
    ( p != nullptr ) && WIR_FixedPriorityBusArbitration::operator==( *p ) &&
    WIR_TDMABusArbitration::operator==( *p ) &&
    ( mSlotModes == p->mSlotModes ) );
};


/*
  This operator compares two WIR_BusArbitrations for inequality.
*/
bool WIR_PriorityDivisionBusArbitration::operator != ( const WIR_BusArbitration &__o ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  return( !( *this == __o ) );
};


/*
  getNumberOfArbitratedCores returns the number of cores that are managed under
  PD arbitration.
*/
unsigned int WIR_PriorityDivisionBusArbitration::getNumberOfArbitratedCores( void ) const
{
  DSTART(
    "virtual unsigned int WIR_PriorityDivisionBusArbitration::getNumberOfArbitratedCores() const" );

  set<WIR_id_t> res;

  for ( auto p : mSlotsOfCore )
    res.insert( p.first );

  for ( auto p : getPriorities() )
    res.insert( p.first );

  return( res.size() );
};


/*
  getSlotModes returns the operation modes per bus slot.
*/
const vector<WIR_BusArbitrationType> &WIR_PriorityDivisionBusArbitration::getSlotModes( void ) const
{
  DSTART(
    "const vector<WIR_BusArbitrationType>& WIR_PriorityDivisionBusArbitration::getSlotModes() const" );

  return( mSlotModes );
};


/*
  getMustGrantWindows returns all windows in which an access by the specified
  processor core will always be granted.
*/
const WIR_OffsetIntervalSet &WIR_PriorityDivisionBusArbitration::getMustGrantWindows( const WIR_BaseProcessor &p ) const
{
  DSTART(
    "const WIR_OffsetIntervalSet& WIR_PriorityDivisionBusArbitration::getMustGrantWindows(const WIR_BaseProcessor&) const" );

  recomputePDGrantWindows();

  auto it = mMustGrantWindows.find( p.getID() );

  ufAssert( it != mMustGrantWindows.end() );
  return( it->second );
};


/*
  getMayGrantWindows returns all windows in which an access by the specified
  processor core may be possible but cannot be granted.
*/
const WIR_OffsetIntervalSet &WIR_PriorityDivisionBusArbitration::getMayGrantWindows( const WIR_BaseProcessor &p ) const
{
  DSTART(
    "const WIR_OffsetIntervalSet& WIR_PriorityDivisionBusArbitration::getMayGrantWindows(const WIR_BaseProcessor&) const" );

  recomputePDGrantWindows();

  auto it = mMayGrantWindows.find( p.getID() );

  ufAssert( it != mMayGrantWindows.end() );
  return( it->second );
};


//
// Protected class methods
//

/*
  clone creates a copy of a priority division bus arbiter.
*/
WIR_PriorityDivisionBusArbitration *WIR_PriorityDivisionBusArbitration::clone( void ) const
{
  DSTART(
    "virtual WIR_PriorityDivisionBusArbitration* WIR_PriorityDivisionBusArbitration::clone() const" );

  return( new WIR_PriorityDivisionBusArbitration( *this ) );
};


/*
  pushBackSlot adds a new bus slot at the end of vector mSlots, after its
  current last element.

  This variant of pushBackSlot must only be used for arbitration types TDMA or
  PD of the current slot. pushBackSlot asserts if other slot arbitration types
  are specified, since slots under another arbitration type do not have an
  owning processor core. For these other slot arbitration policies, use
  pushBackSlot( WIR_BusArbitrationType, WIR_TDMAOffset ) below.
*/
void WIR_PriorityDivisionBusArbitration::pushBackSlot( WIR_BusArbitrationType t,
                                                       WIR_TDMAOffset l,
                                                       const WIR_BaseProcessor &p )
{
  DSTART(
    "void WIR_PriorityDivisionBusArbitration::pushBackSlot(WIR_BusArbitrationType, WIR_TDMAOffset, const WIR_BaseProcessor&)" );

  ufAssert(
    ( t == WIR_BusArbitrationType::tdma ) ||
    ( t == WIR_BusArbitrationType::pd ) );

  mSlotModes.push_back( t );
  WIR_TDMABusArbitration::pushBackSlot( l, p );

  mRecomputePDGrantWindows = true;
};


/*
  pushBackSlot adds a new bus slot at the end of vector mSlots, after its
  current last element.

  This variant of pushBackSlot must only be used for arbitration types other
  than TDMA or PD of the current slot. pushBackSlot asserts if TDMA or PD are
  specified as slot arbitration type, since slots under these arbitration type
  must have an owning processor core. For TDMA or PD as slot arbitration policy
  use
  pushBackSlot( WIR_BusArbitrationType, WIR_TDMAOffset, WIR_BaseProcessor & )
  above.
*/
void WIR_PriorityDivisionBusArbitration::pushBackSlot( WIR_BusArbitrationType t,
                                                       WIR_TDMAOffset l )
{
  DSTART(
    "void WIR_PriorityDivisionBusArbitration::pushBackSlot(WIR_BusArbitrationType, WIR_TDMAOffset)" );

  ufAssert(
    ( t != WIR_BusArbitrationType::tdma ) &&
    ( t != WIR_BusArbitrationType::pd ) );

  mSlotModes.push_back( t );

  unsigned int slotIndex = mSlots.size();

  mSlots.push_back( SlotInfo( slotIndex, mScheduleLength, l ) );

  mScheduleLength += l;

  mRecomputeGrantWindows = true;
  mRecomputePDGrantWindows = true;
};


/*
  recomputePDGrantWindows computes the priority division grant windows, if
  necessary.
*/
void WIR_PriorityDivisionBusArbitration::recomputePDGrantWindows( void ) const
{
  DSTART(
    "void WIR_PriorityDivisionBusArbitration::recomputePDGrantWindows() const" );

  // Trigger recomputation of TDMA grant slots.
  recomputeGrantWindows();

  if ( !mRecomputePDGrantWindows )
    return;

  set<WIR_id_t> coreIDs;

  // Compute MAY grant windows first.
  mMayGrantWindows.clear();
  for ( auto p : mGrantWindows ) {
    coreIDs.insert( p.first );
    mMayGrantWindows.insert(
      make_pair(
        p.first,
        WIR_OffsetIntervalSet(
          mGrantWindows.begin()->second.getModuloBound() ) ) );
  }

  // Check each core and slot if it needs to be considered.
  for ( auto core : coreIDs ) {
    auto slotIt = mSlots.begin();
    auto slotModeIt = mSlotModes.begin();

    for ( ; slotIt != mSlots.end(); ++slotIt, ++slotModeIt )
      switch ( *slotModeIt ) {

        case WIR_BusArbitrationType::fp:
        case WIR_BusArbitrationType::pd: {
          // Priority-arbitrated slots are definitely not accessible by a core
          // if the core's priority is 0. Otherwise, we have to assume that the
          // bus is free so that the current slot MAY be accessed.
          auto it = getPriorities().find( core );

          if ( ( it != getPriorities().end() ) && ( it->second != 0 ) ) {
            auto it1 = mMayGrantWindows.find( core );
            it1->second.unite( slotIt->getGrantWindow() );
          }

          break;
        }

        case WIR_BusArbitrationType::tdma: {
          break;
        }

        default: {
          ufAssertT( false, "Unsupported bus arbitration policy!" );
          break;
        }
      }
  }

  // Compute MUST grant windows next.
  mMustGrantWindows.clear();
  for ( auto p : mMayGrantWindows ) {
    WIR_id_t core = p.first;
    auto it = mGrantWindows.find( core );

    mMustGrantWindows.insert(
      make_pair(
        core,
        WIR_OffsetIntervalSet(
          mScheduleLength, it->second.difference( p.second ) ) ) );
  }

  mRecomputePDGrantWindows = false;
};


/*
  setMaxAccessTime sets the maximum access time for a single memory access via a
  PD-arbitrated bus.
*/
void WIR_PriorityDivisionBusArbitration::setMaxAccessTime( unsigned int t )
{
  DSTART(
    "virtual void WIR_PriorityDivisionBusArbitration::setMaxAccessTime(unsigned int)" );

  WIR_TDMABusArbitration::setMaxAccessTime( t );
  mRecomputePDGrantWindows = true;
};

}       // namespace WIR
