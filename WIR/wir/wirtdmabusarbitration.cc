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
  @file wirtdmabusarbitration.cc
  @brief This file implements a TDMA %WIR bus arbitration policy.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include standard headers
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
  Default constructor for TDMA bus slot information.
*/
WIR_TDMABusArbitration::SlotInfo::SlotInfo( unsigned int i,
                                            const WIR_BaseProcessor &p,
                                            WIR_TDMAOffset s,
                                            WIR_TDMAOffset l ) :
  mIndex { i },
  mOwner { const_cast<WIR_BaseProcessor *>( &p ) },
  mStart { s },
  mLength { l },
  mGrantWindow { 1 }
{
  DSTART(
    "WIR_TDMABusArbitration::SlotInfo::SlotInfo(unsigned int, WIR_BaseProcessor&, WIR_TDMAOffset, WIR_TDMAOffset)" );
};


/*
  Default constructor for bus slot information specific to the priority division
  arbitration policy.
*/
WIR_TDMABusArbitration::SlotInfo::SlotInfo( unsigned int i,
                                            WIR_TDMAOffset s,
                                            WIR_TDMAOffset l ) :
  mIndex { i },
  mOwner { nullptr },
  mStart { s },
  mLength { l },
  mGrantWindow { 1 }
{
  DSTART(
    "WIR_TDMABusArbitration::SlotInfo::SlotInfo(unsigned int, WIR_TDMAOffset, WIR_TDMAOffset)" );
};


/*
  getIndex returns a TDMA slot's index.
*/
unsigned int WIR_TDMABusArbitration::SlotInfo::getIndex( void ) const
{
  DSTART( "unsigned int WIR_TDMABusArbitration::SlotInfo::getIndex() const" );

  return( mIndex );
};


/*
  getOwner returns the processor core that owns a TDMA slot.
*/
const WIR_BaseProcessor &WIR_TDMABusArbitration::SlotInfo::getOwner( void ) const
{
  DSTART(
    "const WIR_BaseProcessor& WIR_TDMABusArbitration::SlotInfo::getOwner() const" );

  return( *mOwner );
};


/*
  getStart returns a TDMA slot's start offset, measured from the beginning of
  the TDMA schedule.
*/
WIR_TDMAOffset WIR_TDMABusArbitration::SlotInfo::getStart( void ) const
{
  DSTART( "WIR_TDMAOffset WIR_TDMABusArbitration::SlotInfo::getStart() const" );

  return( mStart );
};


/*
  getLength returns a TDMA slot's length in clock cycles.
*/
WIR_TDMAOffset WIR_TDMABusArbitration::SlotInfo::getLength( void ) const
{
  DSTART(
    "WIR_TDMAOffset WIR_TDMABusArbitration::SlotInfo::getLength() const" );

  return( mLength );
};


/*
  getGrantWindow returns a TDMA slot's grant window.
*/
const WIR_OffsetIntervalSet &WIR_TDMABusArbitration::SlotInfo::getGrantWindow( void ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  return( mGrantWindow );
};


/*
  This operator compares two TDMA slots for equality.
*/
bool WIR_TDMABusArbitration::SlotInfo::operator == ( const SlotInfo &__o ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  return(
    ( mOwner->getID() == __o.mOwner->getID() ) && ( mStart == __o.mStart ) &&
    ( mLength == __o.mLength ) && ( mGrantWindow == __o.mGrantWindow ) );
};


/*
  setGrantWindow sets a TDMA slot's grant value.
*/
void WIR_TDMABusArbitration::SlotInfo::setGrantWindow( WIR_OffsetIntervalSet &&g )
{
  DSTART(
    "void WIR_TDMABusArbitration::SlotInfo::setGrantWindow(WIR_OffsetIntervalSet&&)" );

  mGrantWindow = move( g );

  DOUT(
    "Grant window of TDMA slot " << mIndex << " is " << mGrantWindow << endl );
};


/*
  Default constructor.
*/
WIR_TDMABusArbitration::WIR_TDMABusArbitration( unsigned int t,
                                                unsigned int d ) :
  WIR_BusArbitration {},
  mMaxAccessTime { t },
  mScheduleLength { 0 },
  mArbitrationDelay { (WIR_TDMAOffset) d },
  mRecomputeGrantWindows { true }
{
  DSTART(
    "WIR_TDMABusArbitration::WIR_TDMABusArbitration(unsigned int, unsigned int)" );
};


/*
  Destructor.
*/
WIR_TDMABusArbitration::~WIR_TDMABusArbitration( void )
{
  DSTART( "virtual WIR_TDMABusArbitration::~WIR_TDMABusArbitration()" );
};


/*
  This operator compares two WIR_BusArbitrations for equality.
*/
bool WIR_TDMABusArbitration::operator == ( const WIR_BusArbitration &__o ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto *p = dynamic_cast<const WIR_TDMABusArbitration *>( &__o );
  return(
    ( p != nullptr ) && ( mSlots == p->mSlots ) &&
    ( mScheduleLength == p->mScheduleLength ) );
};


/*
  This operator compares two WIR_BusArbitrations for inequality.
*/
bool WIR_TDMABusArbitration::operator != ( const WIR_BusArbitration &__o ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  return( !( *this == __o ) );
};


/*
  getNumberOfArbitratedCores returns the number of cores that are managed under
  TDMA arbitration.
*/
unsigned int WIR_TDMABusArbitration::getNumberOfArbitratedCores( void ) const
{
  DSTART(
    "virtual unsigned int WIR_TDMABusArbitration::getNumberOfArbitratedCores() const" );

  return( mSlotsOfCore.size() );
};


/*
  getSlots returns the information about all TDMA slots for this bus.
*/
const std::vector<WIR_TDMABusArbitration::SlotInfo> &WIR_TDMABusArbitration::getSlots( void ) const
{
  DSTART(
    "const vector<WIR_TDMABusArbitration::SlotInfo>& WIR_TDMABusArbitration::getSlots() const" );

  return( mSlots );
};


/*!
  @brief getSlots returns all TDMA slots owned by the specified processor core.

  @param[in] p A const reference to a processor core.
  @return A list of TDMA slots owned by the specified processor core.

  Within the returned list, all TDMA slots are sorted by their start offsets
  so that the order of slots in the list reflects their order within the
  TDMA schedule.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
const std::list<WIR_TDMABusArbitration::SlotInfo> WIR_TDMABusArbitration::getSlots( const WIR_BaseProcessor &p ) const
{
  DSTART(
    "const list<WIR_TDMABusArbitration::SlotInfo> WIR_TDMABusArbitration::getSlots(const WIR_BaseProcessor&) const" );

  auto it = mSlotsOfCore.find( p.getID() );

  ufAssert( it != mSlotsOfCore.end() );

  list<WIR_TDMABusArbitration::SlotInfo> res;

  for ( auto id : it->second )
    res.push_back( mSlots.at( id ) );

  return( res );
};


/*
  getNumberOfSlots returns the number of TDMA slots for this bus.
*/
unsigned int WIR_TDMABusArbitration::getNumberOfSlots( void ) const
{
  DSTART( "unsigned int WIR_TDMABusArbitration::getNumberOfSlots() const" );

  return( mSlots.size() );
};


/*
  getScheduleLength returns the total length of the configured TDMA schedule.
*/
WIR_TDMAOffset WIR_TDMABusArbitration::getScheduleLength( void ) const
{
  DSTART( "WIR_TDMAOffset WIR_TDMABusArbitration::getScheduleLength() const" );

  return( mScheduleLength );
};


/*!
  @brief getGrantWindows returns all TDMA offsets at which an access by the
          specified processor core may be granted.

  @param[in] p A const reference to a processor core.
  @return A set of TDMA offsets for which bus access may be granted.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
const WIR_OffsetIntervalSet &WIR_TDMABusArbitration::getGrantWindows( const WIR_BaseProcessor &p ) const
{
  DSTART(
    "const WIR_OffsetIntervalSet& WIR_TDMABusArbitration::getGrantWindows(const WIR_BaseProcessor&) const" );

  recomputeGrantWindows();

  auto it = mGrantWindows.find( p.getID() );

  ufAssert( it != mGrantWindows.end() );
  return( it->second );
};


//
// Protected class methods
//

/*
  clone creates a copy of a TDMA bus arbiter.
*/
WIR_TDMABusArbitration *WIR_TDMABusArbitration::clone( void ) const
{
  DSTART(
    "virtual WIR_TDMABusArbitration* WIR_TDMABusArbitration::clone() const" );

  return( new WIR_TDMABusArbitration( *this ) );
};


/*
  recomputeGrantWindows computes the grant windows of all TDMA slots and
  processor cores, if necessary.
*/
void WIR_TDMABusArbitration::recomputeGrantWindows( void ) const
{
  DSTART( "void WIR_TDMABusArbitration::recomputeGrantWindows() const" );

  if ( !mRecomputeGrantWindows )
    return;

  // Compute grant window of the TDMA slots first.
  for ( auto &si : mSlots ) {
    ufAssert( si.getLength() >= mMaxAccessTime );
    WIR_TDMAOffset startGrant = si.getStart();
    WIR_TDMAOffset endGrant = si.getStart() + si.getLength() - mMaxAccessTime;
    si.setGrantWindow(
      WIR_OffsetIntervalSet(
        mScheduleLength,
        WIR_Interval<WIR_TDMAOffset>( { startGrant, endGrant } ) ) -
      WIR_Interval( mArbitrationDelay ) );
  }

  // Compute grant windows of the cores.
  mGrantWindows.clear();
  for ( auto p : mSlotsOfCore )
    mGrantWindows.insert(
      make_pair( p.first, WIR_OffsetIntervalSet( mScheduleLength ) ) );

  for ( auto &si : mSlots )
    if ( si.mOwner != nullptr ) {
      auto it = mGrantWindows.find( si.mOwner->getID() );
      it->second.unite( si.getGrantWindow() );
    }

  mRecomputeGrantWindows = false;
};


/*
  setMaxAccessTime sets the maximum access time for a single memory access via a
  TDMA-arbitrated bus.
*/
void WIR_TDMABusArbitration::setMaxAccessTime( unsigned int t )
{
  DSTART(
    "virtual void WIR_TDMABusArbitration::setMaxAccessTime(unsigned int)" );

  mMaxAccessTime = t;
  mRecomputeGrantWindows = true;
};


//
// Private class methods
//

/*
  pushBackSlot adds a new TDMA slot at the end of vector mSlots, after its
  current last element.
*/
void WIR_TDMABusArbitration::pushBackSlot( WIR_TDMAOffset l,
                                           const WIR_BaseProcessor &p )
{
  DSTART(
    "void WIR_TDMABusArbitration::pushBackSlot(WIR_TDMAOffset, const WIR_BaseProcessor&)" );

  unsigned int slotIndex = mSlots.size();

  mSlots.push_back( SlotInfo( slotIndex, p, mScheduleLength, l ) );

  mScheduleLength += l;
  mSlotsOfCore[ p.getID() ].push_back( slotIndex );

  mRecomputeGrantWindows = true;
};

}       // namespace WIR
