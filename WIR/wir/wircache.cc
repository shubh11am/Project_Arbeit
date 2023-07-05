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
  @file wircache.cc
  @brief This file implements %WIR caches.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include standard headers
#include <strings.h>

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


// mHitCounter stores a counter for cache hits.
unsigned int WIR_Cache::mHitCounter = 0;

// mMissCounter stores a counter for cache misses.
unsigned int WIR_Cache::mMissCounter = 0;

// mUnknownCounter stores a counter for unknown cache accesses.
unsigned int WIR_Cache::mUnknownCounter = 0;


//
// Public class methods
//

/*
  Copy constructor.
*/
WIR_Cache::WIR_Cache( const WIR_Cache &__o ) :
  WIR_SystemComponent { __o },
  mEnabled { __o.mEnabled },
  mShared { __o.mShared },
  mLevel { __o.mLevel },
  mType { __o.mType },
  mAssociativity { __o.mAssociativity },
  mNumberOfSets { __o.mNumberOfSets },
  mSize { __o.mSize },
  mSizeInPercent { __o.mSizeInPercent },
  mLineSize { __o.mLineSize },
  mWriteThrough { __o.mWriteThrough },
  mWriteAllocate { __o.mWriteAllocate },
  mOffsetBits { __o.mOffsetBits },
  mIndexBits { __o.mIndexBits },
  mTagBits { __o.mTagBits },
  mHitDelay { __o.mHitDelay },
  mMissDelay { __o.mMissDelay },
  mBusWidth { __o.mBusWidth },
  mIndexOffsetMask { __o.mIndexOffsetMask },
  mOffsetMask { __o.mOffsetMask },
  mIndexMask { __o.mIndexMask }
{
  DSTART( "WIR_Cache::WIR_Cache(const WIR_Cache&)" );
};


/*
  Move constructor.
*/
WIR_Cache::WIR_Cache( WIR_Cache &&__o ) :
  WIR_SystemComponent { move( __o ) },
  mEnabled { move( __o.mEnabled ) },
  mShared { move( __o.mShared ) },
  mLevel { move( __o.mLevel ) },
  mType { move( __o.mType ) },
  mAssociativity { move( __o.mAssociativity ) },
  mNumberOfSets { move( __o.mNumberOfSets ) },
  mSize { move( __o.mSize ) },
  mSizeInPercent { __o.mSizeInPercent },
  mLineSize { move( __o.mLineSize ) },
  mWriteThrough { move( __o.mWriteThrough ) },
  mWriteAllocate { move( __o.mWriteAllocate ) },
  mOffsetBits { move( __o.mOffsetBits ) },
  mIndexBits { move( __o.mIndexBits ) },
  mTagBits { move( __o.mTagBits ) },
  mHitDelay { move( __o.mHitDelay ) },
  mMissDelay { move( __o.mMissDelay ) },
  mBusWidth { move( __o.mBusWidth ) },
  mIndexOffsetMask { move( __o.mIndexOffsetMask ) },
  mOffsetMask { move( __o.mOffsetMask ) },
  mIndexMask { move( __o.mIndexMask ) }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  __o.mEnabled = false;
  __o.mShared = false;
  __o.mLevel = CacheLevel::NOTSET;
  __o.mType = CacheType::I;
  __o.mAssociativity = 0;
  __o.mNumberOfSets = 0;
  __o.mSize = 0;
  __o.mSizeInPercent = boost::optional<unsigned int>();
  __o.mLineSize = 0;
  __o.mWriteThrough = false;
  __o.mWriteAllocate = false;
  __o.mOffsetBits = 0;
  __o.mIndexBits = 0;
  __o.mTagBits = 0;
  __o.mHitDelay = 0;
  __o.mMissDelay = 0;
  __o.mBusWidth = 0;
  __o.mIndexOffsetMask = 0;
  __o.mOffsetMask = 0;
  __o.mIndexMask = 0;
};


/*
  Destructor.
*/
WIR_Cache::~WIR_Cache( void )
{
  DSTART( "virtual WIR_Cache::~WIR_Cache()" );
};


/*
  getType returns the type of a system component, i.e., that it is a cache.
*/
WIR_SystemComponentType WIR_Cache::getType( void ) const
{
  DSTART( "virtual WIR_MemoryType WIR_Cache::getType() const" );

  return( WIR_SystemComponentType::cache );
};


/*
  isEnabled returns whether the cache is active or not.
*/
bool WIR_Cache::isEnabled( void ) const
{
  DSTART( "bool WIR_Cache::isEnabled() const" );

  return( mEnabled );
};


/*
  isShared returns whether the cache is shared between multiple cores or not.
*/
bool WIR_Cache::isShared( void ) const
{
  DSTART( "bool WIR_Cache::isShared() const" );

  return( mShared );
};


/*
  isSizedInPercent returns whether the cache's size was given as a percentage of
  a task's size or not.
*/
bool WIR_Cache::isSizedInPercent( void ) const
{
  DSTART( "bool WIR_Cache::isSizedInPercent() const" );

  return( mSizeInPercent.is_initialized() );
};


/*
  getLevel a cache's position in the memory hierarchy.

  The cache level is also an implicit property, since the caches are stored in a
  list within WIR_System, ordered by their level. Nevertheless, it is stored
  here also explicitly for convenience, in order to have an enum value which
  identifies each cache level.
*/
WIR_Cache::CacheLevel WIR_Cache::getLevel( void ) const
{
  DSTART( "WIR_Cache::CacheLevel WIR_Cache::getLevel() const" );

  return( mLevel );
};


/*
  getCacheType returns a cache's type.
*/
WIR_Cache::CacheType WIR_Cache::getCacheType( void ) const
{
  DSTART( "WIR_Cache::CacheType WIR_Cache::getCacheType() const" );

  return( mType );
};


/*
  getAssociativity returns a cache's associativity.

  Typical values range from 1 (direct mappd) to 8 (set-associative). It is
  ensured that the associativity is a power of 2.
*/
unsigned int WIR_Cache::getAssociativity( void ) const
{
  DSTART( "unsigned int WIR_Cache::getAssociativity() const" );

  return( mEnabled ? mAssociativity : 0 );
};


/*
  brief getNumberOfSets returns a cache's number of sets.

  The number of cache sets is
    "getSize() / ( getAssociativity() * getLineSize() )"
  It is ensured that the number of sets is a power of 2.
*/
unsigned int WIR_Cache::getNumberOfSets( void ) const
{
  DSTART( "unsigned int WIR_Cache::getNumberOfSets() const" );

  return( mEnabled ? mNumberOfSets : 0 );
};


/*
  getSize returns a cache's size in bytes.

  It is ensured that the cache size is a power of 2.
*/
unsigned int WIR_Cache::getSize( void ) const
{
  DSTART( "unsigned int WIR_Cache::getSize() const" );

  return( mEnabled ? mSize : 0 );
};


/*
  setRelativeSize sets the cache's absolute size to a percentage of a given task
  size.

  The percentage used by setRelativeSize needs to be specified before by a call
  to setSizeInPercent(). setRelativeSize asserts if it is called without a prior
  invocation of setSizeInPercent. The resulting absolute cache size can
  afterwards be retrieved with getSize().

  The resulting absolute byte size is rounded up to the closest power of two and
  will leave enough space for a least one cache set.
*/
void WIR_Cache::setRelativeSize( unsigned int t )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  ufAssert( isSizedInPercent() );

  mSize =
    max(
      1u << static_cast<unsigned int>(
              round( log2( t * ( *mSizeInPercent / 100.0 ) ) ) ),
      mLineSize * mAssociativity );

  checkParameters();
};


/*
  getLineSize returns a cache's line size in bytes.

  Here, a cache line is defined as an element of a cache set. I.e., a 4-way
  set-associative cache consists of individual sets, each of them holding 4
  cache lines.

  It is ensured that the line size is a power of 2.
*/
unsigned int WIR_Cache::getLineSize( void ) const
{
  DSTART( "unsigned int WIR_Cache::getLineSize() const" );

  return( mEnabled ? mLineSize : 0 );
};


/*
  isWriteThrough returns whether the cache implements the write-through (true)
  or write-back (false) policy.
*/
bool WIR_Cache::isWriteThrough( void ) const
{
  DSTART( "bool WIR_Cache::isWriteThrough() const" );

  return( mWriteThrough );
};


/*
  isWriteAllocate returns whether the cache cache allocates on writes (true) or
  not (false).
*/
bool WIR_Cache::isWriteAllocate( void ) const
{
  DSTART( "bool WIR_Cache::isWriteAllocate() const" );

  return( mWriteAllocate );
};


/*
  getOffsetBits returns the number of offset bits in addresses for this cache.
*/
int WIR_Cache::getOffsetBits( void ) const
{
  DSTART( "int WIR_Cache::getOffsetBits() const" );

  return( mEnabled ? mOffsetBits : 0 );
};


/*
  getIndexBits returns the number of index bits in addresses for this cache.
*/
int WIR_Cache::getIndexBits( void ) const
{
  DSTART( "int WIR_Cache::getIndexBits() const" );

  return( mEnabled ? mIndexBits : 0 );
};


/*
  getTagBits returns the number of tag bits in addresses for this cache.
*/
int WIR_Cache::getTagBits( void ) const
{
  DSTART( "int WIR_Cache::getTagBits() const" );

  return( mEnabled ? mTagBits : 0 );
};


/*
  getHitDelay returns the time in cycles that a hit in this cache needs to
  complete in the best case.
*/
unsigned int WIR_Cache::getHitDelay( void ) const
{
  DSTART( "unsigned int WIR_Cache::getHitDelay() const" );

  return( mEnabled ? mHitDelay : 0 );
};


/*
  getMissDelay returns the time in cycles that a miss in this cache needs to
  complete in the worst case.
*/
unsigned int WIR_Cache::getMissDelay( void ) const
{
  DSTART( "unsigned int WIR_Cache::getMissDelay() const" );

  return( mEnabled ? mMissDelay : 0 );
};


/*
  getMaxDelay returns the maximum number of clock cycles for which a cache can
  delay a memory access.
*/
unsigned int WIR_Cache::getMaxDelay( const unsigned int d ) const
{
  DSTART( "virtual unsigned int WIR_Cache::getMaxDelay(unsigned int) const" );

  if ( !mEnabled )
    return( d );
  else
    // 2* because we may need to fetch a new line and write an old one back.
    return( mMissDelay + 2 * ( mLineSize / mBusWidth * d ) );
};


/*
  getBusWidth returns the width in bytes of the memory bus that connects a
  cache.
*/
unsigned long WIR_Cache::getBusWidth( void ) const
{
  DSTART( "long unsigned int WIR_Cache::getBusWidth() const" );

  return( mBusWidth );
};


/*
  isActiveInRange returns whether a cache is active for the specified address
  range.

  A cache is active if it is enabled and if @a r is in the active address range.
*/
bool WIR_Cache::isActiveInRange( const WIR_AddressRange &r ) const
{
  DSTART(
    "virtual bool WIR_Cache::isActiveInRange(const WIR_AddressRange&) const" );

  return( mEnabled && WIR_SystemComponent::isActiveInRange( r ) );
};


/*
  getBlockID extracts a block's ID (tag and index) from the given address.
*/
WIR_Cache::CacheBlockID WIR_Cache::getBlockID( const WIR_MemoryAddress &a ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  // Shift down and clear offset bits.
  return( a >> mOffsetBits );
};


/*
  getIndex extracts the index bits from the given address.
*/
WIR_Cache::CacheIndex WIR_Cache::getIndex( const WIR_MemoryAddress &a ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  // Mask away everything except the index.
  WIR_MemoryAddress res( a & mIndexMask );
  res >>= mOffsetBits;
  return( res );
};


/*!
  @brief getTag extracts the tag bits from the given address.

  @return An addresses tag bits.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
WIR_Cache::CacheTag WIR_Cache::getTag( const WIR_MemoryAddress &a ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  // Shift down and clear offset and index bits.
  return( a >> ( mIndexBits + mOffsetBits ) );
};


/*
  getAffectedSets determines the range of cache sets that may be affected by the
  given memory access.
*/
WIR_Interval<WIR_Cache::CacheIndex> WIR_Cache::getAffectedSets( const WIR_AddressRange &a ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  if ( !mEnabled )
    return( WIR_Interval<CacheIndex>() );

  // If the access range is bigger than the cache size, all sets may be
  // affected.
  if ( a.getWidth() > getSize() )
    return
      WIR_Interval(
        WIR_Cache::CacheIndex( 0 ),
        WIR_Cache::CacheIndex( getNumberOfSets() - 1 ) );

  // Determine the set of indices to which the given access may map.
  CacheIndex lowerIdx = getIndex( a.getLower() );
  CacheIndex upperIdx = getIndex( a.getUpper() );

  if ( lowerIdx <= upperIdx )
    return( WIR_Interval<WIR_Cache::CacheIndex>( lowerIdx, upperIdx ) );
  else
    // If the indices wrap around the upper border, we cannot represent the
    // results as a single interval. We thus return the maximum in that case.
    return
      WIR_Interval(
        WIR_Cache::CacheIndex( 0 ),
        WIR_Cache::CacheIndex( getNumberOfSets() - 1 ) );
};


/*
  getAffectedTags determines the range of cache tags that may be affected by the
  given memory access.

  Note that the returned tags may belong to different sets, since the index
  is increased with the address before the tag is increased.
*/
WIR_Interval<WIR_Cache::CacheTag> WIR_Cache::getAffectedTags( const WIR_AddressRange &a ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  if ( !mEnabled )
    return( WIR_Interval<CacheTag>() );

  // Since the tags begin at the MSB, we know that the lower (upper) bound of
  // the access range induces the lower (upper) bound on the tag range.
  return(
    WIR_Interval<WIR_Cache::CacheTag>(
      getTag( a.getLower() ), getTag( a.getUpper() ) ) );
};


/*
  getAffectedBlockIDs determines the range of cache block IDs that may be
  affected by the given memory access.
*/
WIR_Interval<WIR_Cache::CacheBlockID> WIR_Cache::getAffectedBlockIDs( const WIR_AddressRange &a ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  if ( !mEnabled )
    return( WIR_Interval<CacheBlockID>() );

  return(
    WIR_Interval<WIR_Cache::CacheBlockID>(
      getBlockID( a.getLower() ), getBlockID( a.getUpper() ) ) );
};


/*
  getHitCount returns the number of totally counted cache hits.
*/
unsigned int WIR_Cache::getHitCount( void ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  return( mHitCounter );
};


/*
  increaseHitCount increases the number of totally counted cache hits by one.
*/
void WIR_Cache::increaseHitCount( void ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  mHitCounter++;
};


/*
  clearHitCount sets the number of totally counted cache hits to 0.
*/
void WIR_Cache::clearHitCount( void ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  mHitCounter = 0;
};


/*
  getMissCount returns the number of totally counted cache misses.
*/
unsigned int WIR_Cache::getMissCount( void ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  return( mMissCounter );
};


/*
  increaseMissCount increases the number of totally counted cache misses by one.
*/
void WIR_Cache::increaseMissCount( void ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  mMissCounter++;
};


/*
  clearMissCount sets the number of totally counted cache misses to 0.
*/
void WIR_Cache::clearMissCount( void ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  mMissCounter = 0;
};


/*
  getUnknownCount returns the number of totally counted unknown cache
  hits/misses.
*/
unsigned int WIR_Cache::getUnknownCount( void ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  return( mUnknownCounter );
};


/*
  increaseUnknownCount increases the number of totally counted unknown cache
  hits/misses by one.
*/
void WIR_Cache::increaseUnknownCount( void ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  mUnknownCounter++;
};


/*
  clearUnknownCount sets the number of totally counted unknown cache hits/misses
  to 0.
*/
void WIR_Cache::clearUnknownCount( void ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  mUnknownCounter = 0;
};


//
// Private class methods
//

/*
  Default constructor creating an absolutely-sized cache.

  Typical associativity values range from 1 (direct mappd) to 8
  (set-associative).
  A cache line is defined here as an element of a cache set. I.e., a 4-way
  set-associative cache consists of individual sets, each of them holding 4
  cache lines.

  This constructor asserts if the numerical cache parameters are no powers of 2.
*/
WIR_Cache::WIR_Cache( const std::string &n, unsigned int s, unsigned int a,
                      unsigned int l ) :
  WIR_SystemComponent { n },
  mEnabled { true },
  mShared { false },
  mLevel { CacheLevel::NOTSET },
  mAssociativity { a },
  mSize { s },
  mLineSize { l }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  checkParameters();
};


/*
  Default constructor creating an absolutely-sized cache.

  Typical associativity values range from 1 (direct mappd) to 8
  (set-associative).
  A cache line is defined here as an element of a cache set. I.e., a 4-way
  set-associative cache consists of individual sets, each of them holding 4
  cache lines.

  This constructor asserts if the numerical cache parameters are no powers of 2.
*/
WIR_Cache::WIR_Cache( std::string &&n, unsigned int s, unsigned int a,
                      unsigned int l ) :
  WIR_SystemComponent { move( n ) },
  mEnabled { true },
  mShared { false },
  mLevel { CacheLevel::NOTSET },
  mAssociativity { a },
  mSize { s },
  mLineSize { l },
  mHitDelay { 0 },
  mMissDelay { 0 }
{
  DSTART(
    "WIR_Cache::WIR_Cache(string&&, unsigned int, unsigned int, unsigned int)" );

  checkParameters();
};


/*
  Copy-assignment operator.
*/
WIR_Cache & WIR_Cache::operator = ( const WIR_Cache &__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  WIR_SystemComponent::operator = ( __o );

  mEnabled = __o.mEnabled;
  mShared = __o.mShared;
  mLevel = __o.mLevel;
  mType = __o.mType;
  mAssociativity = __o.mAssociativity;
  mNumberOfSets = __o.mNumberOfSets;
  mSize = __o.mSize;
  mSizeInPercent = __o.mSizeInPercent;
  mLineSize = __o.mLineSize;
  mWriteThrough = __o.mWriteThrough;
  mWriteAllocate = __o.mWriteAllocate;
  mOffsetBits = __o.mOffsetBits;
  mIndexBits = __o.mIndexBits;
  mTagBits = __o.mTagBits;
  mHitDelay = __o.mHitDelay;
  mMissDelay = __o.mMissDelay;
  mBusWidth = __o.mBusWidth;
  mIndexOffsetMask = __o.mIndexOffsetMask;
  mOffsetMask = __o.mOffsetMask;
  mIndexMask = __o.mIndexMask;

  return( *this );
};


/*
  Move-assignment operator.
*/
WIR_Cache & WIR_Cache::operator = ( WIR_Cache &&__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  WIR_SystemComponent::operator = ( move( __o ) );

  mEnabled = move( __o.mEnabled );
  __o.mEnabled = false;
  mShared = move( __o.mShared );
  __o.mShared = false;
  mLevel = move( __o.mLevel );
  __o.mLevel = CacheLevel::NOTSET;
  mType = move( __o.mType );
  __o.mType = CacheType::I;
  mAssociativity = move( __o.mAssociativity );
  __o.mAssociativity = 0;
  mNumberOfSets = move( __o.mNumberOfSets );
  __o.mNumberOfSets = 0;
  mSize = move( __o.mSize );
  __o.mSize = 0;
  mSizeInPercent = __o.mSizeInPercent;
  __o.mSizeInPercent = boost::optional<unsigned int>();
  mLineSize = move( __o.mLineSize );
  __o.mLineSize = 0;
  mWriteThrough = move( __o.mWriteThrough );
  __o.mWriteThrough = false;
  mWriteAllocate = move( __o.mWriteAllocate );
  __o.mWriteAllocate = false;
  mOffsetBits = move( __o.mOffsetBits );
  __o.mOffsetBits = 0;
  mIndexBits = move( __o.mIndexBits );
  __o.mIndexBits = 0;
  mTagBits = move( __o.mTagBits );
  __o.mTagBits = 0;
  mHitDelay = move( __o.mHitDelay );
  __o.mHitDelay = 0;
  mMissDelay = move( __o.mMissDelay );
  __o.mMissDelay = 0;
  mBusWidth = move( __o.mBusWidth );
  __o.mBusWidth = 0;
  mIndexOffsetMask = move( __o.mIndexOffsetMask );
  __o.mIndexOffsetMask = 0;
  mOffsetMask = move( __o.mOffsetMask );
  __o.mOffsetMask = 0;
  mIndexMask = move( __o.mIndexMask );
  __o.mIndexMask = 0;

  return( *this );
};


/*
  clone creates a copy of a %WIR cache.
*/
WIR_Cache *WIR_Cache::clone( void ) const
{
  DSTART( "virtual WIR_Cache* WIR_Cache::clone() const" );

  return( new WIR_Cache( *this ) );
};


/*
  setEnabled sets whether the cache is active or not.
*/
void WIR_Cache::setEnabled( bool e )
{
  DSTART( "void WIR_Cache::setEnabled(bool)" );

  mEnabled = e;
};


/*
  setShared sets whether the cache is shared between multiple cores or not.
*/
void WIR_Cache::setShared( bool s )
{
  DSTART( "void WIR_Cache::setShared(bool)" );

  mShared = s;
};


/*
  setSizeInPercent sets the cache's size to be a percentage of a task's size.

  setSizeInPercent asserts if it is passed values outside the interval [0, 100].
*/
void WIR_Cache::setSizeInPercent( unsigned int p )
{
  DSTART( "void WIR_Cache::setSizeInPercent(unsigned int)" );

  ufAssertT(
    ( p <= 100 ),
    "The relative cache size must be a percentage between 0 and 100." );

  mSizeInPercent = p;
};


/*
  setLevel sets a cache's position in the memory hierarchy.

  setLevel asserts if it is tried to set a cache's level to different values. A
  cache occurs at a fixed position within a system's memory hierarchy and thus
  must only have exactly one fixed level.

  The cache level is also an implicit property, since the caches are stored in a
  list within WIR_MemoryRegion, ordered by their level. Nevertheless, it is
  stored here also explicitly for convenience, in order to have an enum value
  which identifies each cache level.
*/
void WIR_Cache::setLevel( CacheLevel l )
{
  DSTART( "void WIR_Cache::setLevel(WIR_Cache::CacheLevel)" );

  ufAssertT(
    ( mLevel == CacheLevel::NOTSET ) || ( l == mLevel ),
    "Illegal attempt to set the level of cache '" << getName() << "' to " <<
    string( l == CacheLevel::L1 ? "L1" : "L2" ) << " while it has been set " <<
    "to " << string( mLevel == CacheLevel::L1 ? "L1" : "L2" ) <<
    " previously." );

  mLevel = l;
};


/*
  setCacheType sets a cache's type.
*/
void WIR_Cache::setCacheType( CacheType t )
{
  DSTART( "void WIR_Cache::setCacheType(WIR_Cache::CacheType)" );

  mType = t;
};


/*
  setWriteThrough sets whether the cache applies the write-through or write-back
  policy.
*/
void WIR_Cache::setWriteThrough( bool w )
{
  DSTART( "void WIR_Cache::setWriteThrough(bool)" );

  mWriteThrough = w;
};


/*
  setWriteAllocate sets whether the cache applies the write-allocate policy or
  not.
*/
void WIR_Cache::setWriteAllocate( bool w )
{
  DSTART( "void WIR_Cache::setWriteAllocate(bool)" );

  mWriteAllocate = w;
};


/*
  setHitDelay sets the time in cycles that a hit in this cache needs to complete
  in the best case.
*/
void WIR_Cache::setHitDelay( unsigned int d )
{
  DSTART( "void WIR_Cache::setHitDelay(unsigned int)" );

  mHitDelay = d;
};


/*
  setMissDelay sets the time in cycles that a miss in this cache needs to
  complete in the worst case.
*/
void WIR_Cache::setMissDelay( unsigned int d )
{
  DSTART( "void WIR_Cache::setMissDelay(unsigned int)" );

  mMissDelay = d;
};


/*
  setBusWidth sets the width in bytes of the memory bus that connects a cache.
*/
void WIR_Cache::setBusWidth( unsigned long w )
{
  DSTART( "void WIR_Cache::setBusWidth(long unsigned int)" );

  mBusWidth = w;
};


/*
  checkParameters checks the parameters passed to the constructors for
  plausibility and derives other values (e.g., number of sets or numbers of
  tag/index bits) from them.
*/
void WIR_Cache::checkParameters( void )
{
  DSTART( "void WIR_Cache::checkParameters()" );

  ufAssert( isPowerOfTwo( mAssociativity ) );
  ufAssert( isPowerOfTwo( mSize ) );
  ufAssert( isPowerOfTwo( mLineSize ) );

  mNumberOfSets = mSize / ( mAssociativity * mLineSize );

  // Compute numbers of address bits.
  mOffsetBits = ffs( mLineSize ) - 1;
  mIndexBits = ffs( mNumberOfSets ) -1;
  mTagBits = ( sizeof( WIR_MemoryAddress ) * 8 ) - mOffsetBits - mIndexBits;

  // Compute bit masks used to filter index/tag/offset bits from a given
  // address. These bit masks are precomputed in order to speed up frequent
  // address decompositions.
  mIndexOffsetMask = ( 1u << ( mOffsetBits + mIndexBits ) ) - 1;
  mOffsetMask = ( 1u << mOffsetBits ) - 1;
  mIndexMask = mIndexOffsetMask - mOffsetMask;
};

}       // namespace WIR
