/*

   This header file belongs to the

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
  @file wircache.h
  @brief This file provides the interface of %WIR caches.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_CACHE_H
#define _WIR_CACHE_H


//
// Include section
//

// Include standard headers
#include <string>

// Include boost headers
#include <boost/optional.hpp>

// Include WIR headers
#include <wir/wirsystemcomponent.h>
#include <analyses/generic/wirinterval.h>


//
// Header section
//

namespace WIR {

/*!
  @brief Class WIR_Cache represents a cache of some system architecture.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_Cache final : public WIR_SystemComponent
{

  public:

    //
    // Local type definitions.
    //

    /*!
      @brief This enum represents a cache's position in the memory hierarchy.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    enum class CacheLevel : char
    {
      //! Denotes a cache whose level has not yet been specified.
      NOTSET = 0,

      //! Denotes a level-1 cache.
      L1,

      //! Denotes a level-2 cache.
      L2
    };

    /*!
      @brief This enum represents a cache's type.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    enum class CacheType : char
    {
      //! Denotes an instruction cache.
      I,

      //! Denotes a data cache.
      D,

      //! Denotes a unified cache.
      U
    };

    //! A type to represent cache blocks (tag + index).
    using CacheBlockID = WIR_MemoryAddress;

    //! A type to represent cache index bits.
    using CacheIndex = WIR_MemoryAddress;

    //! A type to represent cache tag bits.
    using CacheTag = WIR_MemoryAddress;


    //
    // Constructors and destructors.
    //

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Cache( const WIR_Cache & );

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Cache( WIR_Cache && );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_Cache( void );


    //
    // Generic type handling.
    //

    /*!
      @brief getType returns the type of a system component, i.e., that it is a
             cache.

      @return WIR_SystemComponentType::cache

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_SystemComponentType getType( void ) const;


    //
    // Cache properties.
    //

    /*!
      @brief isEnabled returns whether the cache is active or not.

      @return true iff the cache is active, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isEnabled( void ) const;

    /*!
      @brief isShared returns whether the cache is shared between multiple
             cores or not.

      @return true iff the cache is shared, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isShared( void ) const;

    /*!
      @brief isSizedInPercent returns whether the cache's size was given as a
             percentage of a task's size or not.

      @return true iff the cache size was given as percentage, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isSizedInPercent( void ) const;

    /*!
      @brief getLevel a cache's position in the memory hierarchy.

      @return The cache's level.

      The cache level is also an implicit property, since the caches are stored
      in a list within WIR_System, ordered by their level. Nevertheless, it is
      stored here also explicitly for convenience, in order to have an enum
      value which identifies each cache level.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    CacheLevel getLevel( void ) const;

    /*!
      @brief getCacheType returns a cache's type.

      @return The cache type.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    CacheType getCacheType( void ) const;

    /*!
      @brief getAssociativity returns a cache's associativity.

      @return The cache's associativity if it is enabled, 0 otherwise.

      Typical values range from 1 (direct mappd) to 8 (set-associative). It is
      ensured that the associativity is a power of 2.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    unsigned int getAssociativity( void ) const;

    /*!
      @brief getNumberOfSets returns a cache's number of sets.

      @return The number of cache sets if it is enabled, 0 otherwise.

      The number of cache sets is
        "getSize() / ( getAssociativity() * getLineSize() )"
      It is ensured that the number of sets is a power of 2.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    unsigned int getNumberOfSets( void ) const;

    /*!
      @brief getSize returns a cache's size in bytes.

      @return The size of a cache in bytes if enabled, 0 otherwise.

      It is ensured that the cache size is a power of 2.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    unsigned int getSize( void ) const;

    /*!
      @brief setRelativeSize sets the cache's absolute size to a percentage of a
             given task size.

      @param[in] t The total task size in bytes which is considered to be 100%

      The percentage used by setRelativeSize needs to be specified before by a
      call to setSizeInPercent(). setRelativeSize asserts if it is called
      without a prior invocation of setSizeInPercent. The resulting absolute
      cache size can afterwards be retrieved with getSize().

      The resulting absolute byte size is rounded up to the closest power of two
      and will leave enough space for a least one cache set.
    */
    void setRelativeSize( unsigned int );

    /*!
      @brief getLineSize returns a cache's line size in bytes.

      @return The size of a cache line in bytes if it is enabled, 0 otherwise.

      Here, a cache line is defined as an element of a cache set. I.e., a 4-way
      set-associative cache consists of individual sets, each of them holding 4
      cache lines.

      It is ensured that the line size is a power of 2.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    unsigned int getLineSize( void ) const;

    /*!
      @brief isWriteThrough returns whether the cache implements the
             write-through (true) or write-back (false) policy.

      @return true iff the cache is write-through, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isWriteThrough( void ) const;

    /*!
      @brief isWriteAllocate returns whether the cache cache allocates on writes
             (true) or not (false).

      @return true iff the cache allocates on writes, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isWriteAllocate( void ) const;

    /*!
      @brief getOffsetBits returns the number of offset bits in addresses for
             this cache.

      @return The number of address offset bits if the cache is enabled, 0
              otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    int getOffsetBits( void ) const;

    /*!
      @brief getIndexBits returns the number of index bits in addresses for this
             cache.

      @return The number of address index bits if the cache is enabled, 0
              otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    int getIndexBits( void ) const;

    /*!
      @brief getTagBits returns the number of tag bits in addresses for this
             cache.

      @return The number of address tag bits if the cache is enabled, 0
              otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    int getTagBits( void ) const;

    /*!
      @brief getHitDelay returns the time in cycles that a hit in this cache
             needs to complete in the best case.

      @return The number of cycles for a cache hit if enabled, 0 otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    unsigned int getHitDelay( void ) const;

    /*!
      @brief getMissDelay returns the time in cycles that a miss in this cache
             needs to complete in the worst case.

      @return The number of cycles for a cache miss if enabled, 0 otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    unsigned int getMissDelay( void ) const;

    /*!
      @brief getMaxDelay returns the maximum number of clock cycles for which
             a cache can delay a memory access.

      @param[in] d The delay by which an access may be delayed by other
                   components in the memory hierarchy that are behind this
                   current component.
      @return The maximal delay for an access to this cache.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual unsigned int getMaxDelay( const unsigned int d = 0 ) const;

    /*!
      @brief getBusWidth returns the width in bytes of the memory bus that
             connects a cache.

      @return The memory buses width in bytes.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    unsigned long getBusWidth( void ) const;


    //
    // Address range handling.
    //

    /*!
      @brief isActiveInRange returns whether a cache is active for the specified
             address range.

      @param[in] r A const reference to an address range.
      @return true if the cache is active for r, false otherwise.

      A cache is active if it is enabled and if @a r is in the active address
      range.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool isActiveInRange( const WIR_AddressRange & ) const;

    /*!
      @brief getBlockID extracts a block's ID (tag and index) from the given
             address.

      @param[in] a A const reference to a memory address.
      @return An addresses block ID.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    CacheBlockID getBlockID( const WIR_MemoryAddress & ) const;

    /*!
      @brief getIndex extracts the index bits from the given address.

      @param[in] a A const reference to a memory address.
      @return An addresses index bits.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    CacheIndex getIndex( const WIR_MemoryAddress & ) const;

    /*!
      @brief getTag extracts the tag bits from the given address.

      @param[in] a A const reference to a memory address.
      @return An addresses tag bits.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    CacheTag getTag( const WIR_MemoryAddress & ) const;

    /*!
      @brief getAffectedSets determines the range of cache sets that may be
             affected by the given memory access.

      @param[in] a A const reference to a memory access in a given address
                   range.
      @return An interval of potentially affected cache sets if the cache is
              enabled, an empty interval otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Interval<CacheIndex> getAffectedSets( const WIR_AddressRange & ) const;

    /*!
      @brief getAffectedTags determines the range of cache tags that may be
             affected by the given memory access.

      @param[in] a A const reference to a memory access in a given address
                   range.
      @return An interval of potentially affected cache tags if the cache is
              enabled, an empty interval otherwise.

      Note that the returned tags may belong to different sets, since the index
      is increased with the address before the tag is increased.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Interval<CacheTag> getAffectedTags( const WIR_AddressRange & ) const;

    /*!
      @brief getAffectedBlockIDs determines the range of cache block IDs that
             may be affected by the given memory access.

      @param[in] a A const reference to a memory access in a given address
                   range.
      @return An interval of potentially affected cache block IDs if the cache
              is enabled, an empty interval otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Interval<CacheBlockID> getAffectedBlockIDs( const WIR_AddressRange & ) const;


    //
    // Cache event counting.
    //

    /*!
      @brief getHitCount returns the number of totally counted cache hits.

      @return The number of counted cache hits.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    unsigned int getHitCount( void ) const;

    /*!
      @brief increaseHitCount increases the number of totally counted cache
             hits by one.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void increaseHitCount( void ) const;

    /*!
      @brief clearHitCount sets the number of totally counted cache hits to 0.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void clearHitCount( void ) const;

    /*!
      @brief getMissCount returns the number of totally counted cache misses.

      @return The number of counted cache misses.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    unsigned int getMissCount( void ) const;

    /*!
      @brief increaseMissCount increases the number of totally counted cache
             misses by one.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void increaseMissCount( void ) const;

    /*!
      @brief clearMissCount sets the number of totally counted cache misses to
             0.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void clearMissCount( void ) const;

    /*!
      @brief getUnknownCount returns the number of totally counted unknown cache
             hits/misses.

      @return The number of counted unknown cache accesses.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    unsigned int getUnknownCount( void ) const;

    /*!
      @brief increaseUnknownCount increases the number of totally counted
             unknown cache hits/misses by one.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void increaseUnknownCount( void ) const;

    /*!
      @brief clearUnknownCount sets the number of totally counted unknown cache
             hits/misses to 0.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void clearUnknownCount( void ) const;


  private:

    friend class WIR_System;


    //
    // Constructors.
    //

    /*!
      @brief No standard construction allowed, users must use
             WIR_Cache( std::string )
             instead.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Cache( void ) = delete;

    /*!
      @brief Default constructor creating an absolutely-sized cache.

      @param[in] n A const reference to a string to be copied that holds the
                   cache's name.
      @param[in] s The size of a cache in bytes.
      @param[in] a The cache's associativity.
      @param[in] l The size of a cache line in bytes.

      Typical associativity values range from 1 (direct mappd) to 8
      (set-associative).
      A cache line is defined here as an element of a cache set. I.e., a 4-way
      set-associative cache consists of individual sets, each of them holding 4
      cache lines.

      This constructor asserts if the numerical cache parameters are no powers
      of 2.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Cache( const std::string &, unsigned int, unsigned int, unsigned int );

    /*!
      @brief Default constructor creating an absolutely-sized cache.

      @param[in] n An R-value reference to a string to be moved that holds the
                   cache's name.
      @param[in] s The size of a cache in bytes.
      @param[in] a The cache's associativity.
      @param[in] l The size of a cache line in bytes.

      Typical associativity values range from 1 (direct mappd) to 8
      (set-associative).
      A cache line is defined here as an element of a cache set. I.e., a 4-way
      set-associative cache consists of individual sets, each of them holding 4
      cache lines.

      This constructor asserts if the numerical cache parameters are no powers
      of 2.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Cache( std::string &&, unsigned int, unsigned int, unsigned int );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Cache & operator = ( const WIR_Cache & );

    /*!
      @brief Move-assignment operator.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Cache & operator = ( WIR_Cache && );

    /*!
      @brief clone creates a copy of a %WIR cache.

      @return A pointer to the newly created copy of this cache.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_Cache *clone( void ) const;


    //
    // Cache properties.
    //

    /*!
      @brief setEnabled sets whether the cache is active or not.

      @param[in] e A Boolean specifying whether the cache is active or not.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setEnabled( bool = true );

    /*!
      @brief setShared sets whether the cache is shared between multiple cores
             or not.

      @param[in] s A Boolean specifying whether the cache is shared or not.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setShared( bool = true );

    /*!
      @brief setSizeInPercent sets the cache's size to be a percentage of a
             task's size.

      @param[in] p The cache's size as a percentage of a task's size.

      setSizeInPercent asserts if it is passed values outside the interval
      [0, 100].

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setSizeInPercent( unsigned int );

    /*!
      @brief setLevel sets a cache's position in the memory hierarchy.

      @param[in] l The cache level.

      setLevel asserts if it is tried to set a cache's level to different
      values. A cache occurs at a fixed position within a system's memory
      hierarchy and thus must only have exactly one fixed level.

      The cache level is also an implicit property, since the caches are stored
      in a list within WIR_MemoryRegion, ordered by their level. Nevertheless,
      it is stored here also explicitly for convenience, in order to have an
      enum value which identifies each cache level.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setLevel( CacheLevel );

    /*!
      @brief setCacheType sets a cache's type.

      @param[in] t The cache type.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setCacheType( CacheType );

    /*!
      @brief setWriteThrough sets whether the cache applies the write-through or
             write-back policy.

      @param[in] w A Boolean specifying whether write-through is applied or not.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setWriteThrough( bool = true );

    /*!
      @brief setWriteAllocate sets whether the cache applies the write-allocate
             policy or not.

      @param[in] w A Boolean specifying whether write-allocate is applied or
                   not.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setWriteAllocate( bool = true );

    /*!
      @brief setHitDelay sets the time in cycles that a hit in this cache needs
             to complete in the best case.

      @param[in] d The cache's hit delay in clock cycles.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setHitDelay( unsigned int );

    /*!
      @brief setMissDelay sets the time in cycles that a miss in this cache
             needs to complete in the worst case.

      @param[in] d The cache's miss delay in clock cycles.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setMissDelay( unsigned int );

    /*!
      @brief setBusWidth sets the width in bytes of the memory bus that connects
            a cache.

      @param[in] w The cache's bus width in bytes.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setBusWidth( unsigned long );

    /*!
      @brief checkParameters checks the parameters passed to the constructors
             for plausibility and derives other values (e.g., number of sets or
             numbers of tag/index bits) from them.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void checkParameters( void );

    //! mEnabled stores whether the cache is activated or bypassed.
    bool mEnabled;

    //! mShard stores whether the cache is shared between multiple cores.
    bool mShared;

    //! mLevel stores the level of the cache.
    CacheLevel mLevel;

    //! mType stores the type of the cache.
    CacheType mType;

    /*!
      @brief mAssociativity stores the cache's associativity.

      Typical values range from 1 (direct mappd) to 8 (set-associative).
    */
    unsigned int mAssociativity;

    //! mNumberOfSets stores the number of sets in the cache.
    unsigned int mNumberOfSets;

    //! mSize stores the size of the cache in bytes.
    unsigned int mSize;

    /*!
      @brief mSizeInPercent stores the size of the cache as a percentage of a
             task's size.

      mSizeInPercent only holds the number of percents. It is translated to an
      absolute cache size in field mSize by method applyPercentalSize().
    */
    boost::optional<unsigned int> mSizeInPercent;

    /*!
      @brief mLineSize stores a cache's line size in bytes.

      Here, a cache line is defined as an element of a cache set. I.e., a 4-way
      set-associative cache consists of individual sets, each of them holding 4
      cache lines.
    */
    unsigned int mLineSize;

    /*!
      @brief mWriteThrough stores whether the cache implements the write-through
             (true) or write-back (false) policy.
    */
    bool mWriteThrough;

    /*!
      @brief mWriteAllocate stores whether the cache allocates on writes (true)
             or not (false).
    */
    bool mWriteAllocate;

    /*!
      @brief mOffsetBits stores the number of offset bits in addresses for this
             cache.
    */
    unsigned int mOffsetBits;

    /*!
      @brief mIndexBits stores the number of index bits in addresses for this
             cache.
    */
    unsigned int mIndexBits;

    //! mTagBits stores the number of tag bits in addresses for this cache.
    unsigned int mTagBits;

    /*!
      @brief mHitDelay stores the time in cycles that a hit in this cache needs
             to complete in the best case.
    */
    unsigned int mHitDelay;

    /*!
      @brief mMissDelay stores the time in cycles that a miss in this cache
             needs to complete in the worst case.
    */
    unsigned int mMissDelay;

    /*!
     @brief mBusWidth stores the width in bytes of the memory bus that connects
            a cache.
    */
    unsigned long mBusWidth;

    /*!
      @brief mIndexOffsetMask stores a mask for extracting the index and offset
             bits from an address.
    */
    WIR_MemoryAddress mIndexOffsetMask;

    /*!
      @brief mOffsetMask stores a mask for extracting the offset from an
             address.
    */
    WIR_MemoryAddress mOffsetMask;

    /*!
      @brief mIndexMask stores a mask for extracting the index from an address.
    */
    WIR_MemoryAddress mIndexMask;

    //! mHitCounter stores a counter for cache hits.
    static unsigned int mHitCounter;

    //! mMissCounter stores a counter for cache misses.
    static unsigned int mMissCounter;

    //! mUnknownCounter stores a counter for unknown cache accesses.
    static unsigned int mUnknownCounter;

};

}       // namespace WIR

#endif  // _WIR_CACHE_H
