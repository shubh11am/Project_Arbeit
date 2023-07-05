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
  @file wirmemoryregion.cc
  @brief This file implements %WIR memory regions.

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
  Copy constructor.
*/
WIR_MemoryRegion::WIR_MemoryRegion( const WIR_MemoryRegion &__o ) :
  WIR_SystemComponent { __o },
  mBaseAddress { __o.mBaseAddress },
  mLength { __o.mLength },
  mFreeSpace { __o.mFreeSpace },
  mDynamicAllocation { __o.mDynamicAllocation },
  mAttributes { __o.mAttributes },
  mMemoryType { __o.mMemoryType },
  mClockRatio { __o.mClockRatio },
  mDelay { __o.mDelay },
  mBurstDelay { __o.mBurstDelay },
  mBusWidth { __o.mBusWidth },
  mHierarchyLevels { __o.mHierarchyLevels },
  mSections { __o.mSections }
{
  DSTART( "WIR_MemoryRegion::WIR_MemoryRegion(const WIR_MemoryRegion&)" );
};


/*
  Move constructor.
*/
WIR_MemoryRegion::WIR_MemoryRegion( WIR_MemoryRegion &&__o ) :
  WIR_SystemComponent { move( __o ) },
  mBaseAddress { move( __o.mBaseAddress ) },
  mLength { move( __o.mLength ) },
  mFreeSpace { move( __o.mFreeSpace ) },
  mDynamicAllocation { move( __o.mDynamicAllocation ) },
  mAttributes { move( __o.mAttributes ) },
  mMemoryType { move( __o.mMemoryType ) },
  mClockRatio { move( __o.mClockRatio ) },
  mDelay { move( __o.mDelay ) },
  mBurstDelay { move( __o.mBurstDelay ) },
  mBusWidth { move( __o.mBusWidth ) },
  mHierarchyLevels { move( __o.mHierarchyLevels ) },
  mSections { move( __o.mSections ) }
{
  DSTART( "WIR_MemoryRegion::WIR_MemoryRegion(WIR_MemoryRegion&&)" );

  __o.mBaseAddress = 0;
  __o.mLength = 0;
  __o.mFreeSpace = 0;
  __o.mDynamicAllocation = false;
  __o.mAttributes = (unsigned long) WIR_MemoryRegionAttributes::none;
  __o.mMemoryType.clear();
  __o.mClockRatio = 0.0l;
  __o.mDelay = 0;
  __o.mBurstDelay = 0;
  __o.mBusWidth = 0;
  __o.mHierarchyLevels.clear();
  __o.mSections.clear();
};


/*
  Destructor.
*/
WIR_MemoryRegion::~WIR_MemoryRegion( void )
{
  DSTART( "virtual WIR_MemoryRegion::~WIR_MemoryRegion()" );
};


/*
  getType returns the type of a system component, i.e., that it is a memory
  region.
*/
WIR_SystemComponentType WIR_MemoryRegion::getType( void ) const
{
  DSTART( "virtual WIR_SystemComponentType WIR_MemoryRegion::getType() const" );

  return( WIR_SystemComponentType::memory );
};


/*
  getBaseAddress returns a memory region's base address.
*/
WIR_MemoryAddress WIR_MemoryRegion::getBaseAddress( void ) const
{
  DSTART( "size_t WIR_MemoryRegion::getBaseAddress() const" );

  return( mBaseAddress );
};


/*
  getLength returns a memory region's length in bytes.
*/
std::size_t WIR_MemoryRegion::getLength( void ) const
{
  DSTART( "size_t WIR_MemoryRegion::getLength() const" );

  return( mLength );
};


/*
  getFreeSpace returns a memory region's free space in bytes.
*/
std::size_t WIR_MemoryRegion::getFreeSpace( void )
{
  DSTART( "size_t WIR_MemoryRegion::getFreeSpace()" );

  getSystem().computeMemoryLayout();
  return( mFreeSpace );
};


/*
  isDynamicallyAllocated returns a memory region's dynamic allocation flag.
*/
bool WIR_MemoryRegion::isDynamicallyAllocated( void ) const
{
  DSTART( "bool WIR_MemoryRegion::isDynamicallyAllocated() const" );

  return( mDynamicAllocation );
};


/*
  brief getAttributes returns a memory region's attributes.
*/
unsigned long WIR_MemoryRegion::getAttributes( void ) const
{
  DSTART( "long unsigned int WIR_MemoryRegion::getAttributes() const" );

  return( mAttributes );
};


/*
  getMemoryType returns the name of the memory type that a region belongs to.
*/
std::string WIR_MemoryRegion::getMemoryType( void ) const
{
  DSTART( "string WIR_MemoryRegion::getMemoryType() const" );

  return( mMemoryType );
};


/*
  getClockRatio returns the ratio of the memory region's clock frequency
  compared to the processor's clock frequency.
*/
long double WIR_MemoryRegion::getClockRatio( void ) const
{
  DSTART( "long double WIR_MemoryRegion::getClockRatio() const" );

  return( mClockRatio );
};


/*
  getMinDelay returns the minimum number of clock cycles for which a memory
  region can delay a memory access.
*/
unsigned int WIR_MemoryRegion::getMinDelay( void ) const
{
  DSTART( "unsigned int WIR_MemoryRegion::getMinDelay() const" );

  return( mDelay );
};


/*
  getMaxDelay returns the maximum number of clock cycles for which a memory
  region can delay a memory access.
*/
unsigned int WIR_MemoryRegion::getMaxDelay( const unsigned int d ) const
{
  DSTART(
    "virtual unsigned int WIR_MemoryRegion::getMaxDelay(unsigned int) const" );

  (void) d;
  return( mDelay );
};


/*
  getMinBurstDelay returns a memory region's minimum burst access delay in clock
  cycles.

  The burst access delay denotes the number of cycles that elapses until
  content of the memory is completely read/written in burst mode, after the
  very first access of the burst has already happened.
*/
unsigned long WIR_MemoryRegion::getMinBurstDelay( void ) const
{
  DSTART( "long unsigned int WIR_MemoryRegion::getMinBurstDelay() const" );

  return( mBurstDelay );
};


/*
  getMaxBurstDelay returns a memory region's maximum burst access delay in clock
  cycles.

  The burst access delay denotes the number of cycles that elapses until
  content of the memory is completely read/written in burst mode, after the
  very first access of the burst has already happened.
*/
unsigned long WIR_MemoryRegion::getMaxBurstDelay( void ) const
{
  DSTART( "long unsigned int WIR_MemoryRegion::getMaxBurstDelay() const" );

  return( mBurstDelay );
};


/*
  getBusWidth returns the width in bytes of the memory bus that connects a
  region.
*/
unsigned long WIR_MemoryRegion::getBusWidth( void ) const
{
  DSTART( "long unsigned int WIR_MemoryRegion::getBusWidth() const" );

  return( mBusWidth );
};


/*
  getHierarchy returns all lists of all system components that are located in
  front of this memory region.

  Entries at the beginning of a list are close to the processor (i.e., level 1)
  while entries at a list's tail are farther away (e.g., level 3).
  The returned lists only contains caches or buses.
*/
const set<list<reference_wrapper<WIR_SystemComponent>>> &WIR_MemoryRegion::getHierarchy( void ) const
{
  DSTART(
    "const set<list<reference_wrapper<WIR_SystemComponent> > >& WIR_MemoryRegion::getHierarchy() const" );

  return( mHierarchyLevels );
};


/*
  getSections returns the set of sections attached to a region.
*/
const WIR_SectionSet &WIR_MemoryRegion::getSections( void ) const
{
  DSTART( "const WIR_SectionSet& WIR_MemoryRegion::getSections() const" );

  return( mSections );
};


/*
  containsSection returns whether set mSections contains a given WIR_Section.
*/
bool WIR_MemoryRegion::containsSection( const WIR_Section &s ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  return( mSections.count( const_cast<WIR_Section &>( s ) ) ? true : false );
};


//
// Private class methods
//

/*
  Default constructor creating an empty memory region.
*/
WIR_MemoryRegion::WIR_MemoryRegion( const std::string &s ) :
  WIR_SystemComponent { s },
  mBaseAddress { 0 },
  mLength { 0 },
  mFreeSpace { 0 },
  mDynamicAllocation { false },
  mAttributes { (unsigned long) WIR_MemoryRegionAttributes::none },
  mMemoryType { "" },
  mClockRatio { 0.0l },
  mDelay { 0 },
  mBurstDelay { 0 },
  mBusWidth { 0 }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Default constructor creating an empty memory region.
*/
WIR_MemoryRegion::WIR_MemoryRegion( std::string &&s ) :
  WIR_SystemComponent { move( s ) },
  mBaseAddress { 0 },
  mLength { 0 },
  mFreeSpace { 0 },
  mDynamicAllocation { false },
  mAttributes { (unsigned long) WIR_MemoryRegionAttributes::none },
  mMemoryType { "" },
  mClockRatio { 0.0l },
  mDelay { 0 },
  mBurstDelay { 0 },
  mBusWidth { 0 }
{
  DSTART( "WIR_MemoryRegion::WIR_MemoryRegion(string&&)" );
};


/*
  Copy-assignment operator.
*/
WIR_MemoryRegion & WIR_MemoryRegion::operator = ( const WIR_MemoryRegion &__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  WIR_SystemComponent::operator = ( __o );

  mBaseAddress = __o.mBaseAddress;
  mLength = __o.mLength;
  mFreeSpace  = __o.mFreeSpace;
  mDynamicAllocation = __o.mDynamicAllocation;
  mAttributes = __o.mAttributes;
  mMemoryType = __o.mMemoryType;
  mClockRatio = __o.mClockRatio;
  mDelay = __o.mDelay;
  mBurstDelay = __o.mBurstDelay;
  mBusWidth = __o.mBusWidth;
  mHierarchyLevels = __o.mHierarchyLevels;
  mSections = __o.mSections;

  return( *this );
};


/*
  Move-assignment operator.
*/
WIR_MemoryRegion & WIR_MemoryRegion::operator = ( WIR_MemoryRegion &&__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  WIR_SystemComponent::operator = ( move( __o ) );

  mBaseAddress = move( __o.mBaseAddress );
  __o.mBaseAddress = 0;
  mLength = move( __o.mLength );
  __o.mLength = 0;
  mFreeSpace = move( __o.mFreeSpace );
  __o.mFreeSpace = 0;
  mDynamicAllocation = move( __o.mDynamicAllocation );
  __o.mDynamicAllocation = false;
  mAttributes = move( __o.mAttributes );
  __o.mAttributes = 0;
  mMemoryType = move( __o.mMemoryType );
  __o.mMemoryType = "";
  mClockRatio = move( __o.mClockRatio );
  __o.mClockRatio = 0.0l;
  mDelay = move( __o.mDelay );
  __o.mDelay = 0;
  mBurstDelay = move( __o.mBurstDelay );
  __o.mBurstDelay = 0;
  mBusWidth = move( __o.mBusWidth );
  __o.mBusWidth = 0;
  mHierarchyLevels = move( __o.mHierarchyLevels );
  __o.mHierarchyLevels.clear();
  mSections = move( __o.mSections );
  __o.mSections.clear();

  return( *this );
};


/*
  clone creates a copy of a %WIR memory region.
*/
WIR_MemoryRegion *WIR_MemoryRegion::clone( void ) const
{
  DSTART( "virtual WIR_MemoryRegion* WIR_MemoryRegion::clone() const" );

  return( new WIR_MemoryRegion( *this ) );
};


/*
  setBaseAddress sets a memory region's base address.
*/
void WIR_MemoryRegion::setBaseAddress( const WIR_MemoryAddress &s )
{
  DSTART( "void WIR_MemoryRegion::setBaseAddress(size_t)" );

  mBaseAddress = s;
};


/*
  setLength sets a memory region's length in bytes.
*/
void WIR_MemoryRegion::setLength( std::size_t s )
{
  DSTART( "void WIR_MemoryRegion::setLength(size_t)" );

  mLength = s;
};


/*
  setFreeSpace sets a memory region's available space in bytes.
*/
void WIR_MemoryRegion::setFreeSpace( std::size_t s )
{
  DSTART( "void WIR_MemoryRegion::setFreeSpace(size_t)" );

  mFreeSpace = s;
};


/*
  setDynamicAllocation sets a memory region's dynamic allocation flag.
*/
void WIR_MemoryRegion::setDynamicAllocation( bool f )
{
  DSTART( "void WIR_MemoryRegion::setDynamicAllocation(bool)" );

  mDynamicAllocation = f;
};


/*
  setAttributes sets a memory region's attributes.
*/
void WIR_MemoryRegion::setAttributes( unsigned long a )
{
  DSTART( "void WIR_MemoryRegion::setAttributes(long unsigned int)" );

  mAttributes = a;
};


/*
  setMemoryType sets the name of the memory type that a region belongs to.
*/
void WIR_MemoryRegion::setMemoryType( const std::string &n )
{
  DSTART( "void WIR_MemoryRegion::setMemoryType(const string&)" );

  mMemoryType = n;
};


/*
  setClockRatio sets the ratio of the memory region's clock frequency compared
  to the processor's clock frequency.
*/
void WIR_MemoryRegion::setClockRatio( long double r )
{
  DSTART( "void WIR_MemoryRegion::setClockRatio(long double)" );

  mClockRatio = r;
};


/*
  setDelay sets a memory region's access delay in clock cycles.

  The access delay denotes the number of cycles that elapses until content of
  the memory is completely read/written.
*/
void WIR_MemoryRegion::setDelay ( unsigned long d )
{
  DSTART( "void WIR_MemoryRegion::setDelay(long unsigned int)" );

  mDelay = d;
};


/*
  setBurstDelay sets a memory region's burst access delay in clock cycles.

  The burst access delay denotes the number of cycles that elapses until content
  of the memory is completely read/written in burst mode, after the very first
  access of the burst has already happened.
*/
void WIR_MemoryRegion::setBurstDelay( unsigned long d )
{
  DSTART( "void WIR_MemoryRegion::setBurstDelay(long unsigned int)" );

  mBurstDelay = d;
};


/*
  setBusWidth sets the width in bytes of the memory bus that connects a region.
*/
void WIR_MemoryRegion::setBusWidth( unsigned long w )
{
  DSTART( "void WIR_MemoryRegion::setBusWidth(long unsigned int)" );

  mBusWidth = w;
};


/*
  insertHierarchy adds a new list of system components to set mHierarchyLevels.

  insertHierarchy asserts if components other than caches or buses shall be
  inserted.
*/
void WIR_MemoryRegion::insertHierarchy( std::list<std::reference_wrapper<WIR_SystemComponent>> &&l )
{
  DSTART(
    "void WIR_MemoryRegion::insertHierarchy(list<reference_wrapper<WIR_SystemComponent> >&&)" );

  for ( WIR_SystemComponent &c : l )
    ufAssert( c.getType() != WIR_SystemComponentType::memory );
  mHierarchyLevels.insert( l );
};


/*
  insertSection inserts the specified ELF executable section into set mSections.
*/
void WIR_MemoryRegion::insertSection( const WIR_Section &s )
{
  DSTART( "void WIR_MemoryRegion::insertSection(const WIR_Section&)" );

  mSections.insert( const_cast<WIR_Section &>( s ) );
};

}       // namespace WIR
