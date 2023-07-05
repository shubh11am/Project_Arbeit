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
  @file wirmemoryregion.h
  @brief This file provides the interface of %WIR memory regions.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_MEMORY_REGION_H
#define _WIR_MEMORY_REGION_H


//
// Include section
//

// Include standard headers
#include <cstddef>
#include <functional>
#include <list>
#include <set>
#include <string>

// Include WIR headers
#include <wir/wirsystemcomponent.h>
#include <wir/wirtypes.h>


//
// Header section
//

namespace WIR {

/*!
  @brief Class WIR_MemoryRegion represents physical memory regions of some
         system architecture.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_MemoryRegion final : public WIR_SystemComponent
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_MemoryRegion( const WIR_MemoryRegion & );

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_MemoryRegion( WIR_MemoryRegion && );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_MemoryRegion( void );


    //
    // Generic type handling.
    //

    /*!
      @brief getType returns the type of a system component, i.e., that it is a
             memory region.

      @return WIR_SystemComponentType::memory

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_SystemComponentType getType( void ) const;


    //
    // Memory region properties.
    //

    /*!
      @brief getBaseAddress returns a memory region's base address.

      @return The region's physical start address.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_MemoryAddress getBaseAddress( void ) const;

    /*!
      @brief getLength returns a memory region's length in bytes.

      @return The region's length in bytes.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::size_t getLength( void ) const;

    /*!
      @brief getFreeSpace returns a memory region's free space in bytes.

      @return The region's available space in bytes.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::size_t getFreeSpace( void );

    /*!
      @brief isDynamicallyAllocated returns a memory region's dynamic
             allocation flag.

      @return true if the amount of used space in a memory region may vary
              dynamically at runtime (e.g., for a region containing .stack),
              false otherwise.

      @author <Til.Mauersberger@tuhh.de>
    */
    bool isDynamicallyAllocated( void ) const;

    /*!
      @brief getAttributes returns a memory region's attributes.

      @return A bit-field encoded with type WIR_MemoryRegionAttributes that
              specifies which attributes are set or not set for this memory
              region.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    unsigned long getAttributes( void ) const;

    /*!
      @brief getMemoryType returns the name of the memory type that a region
             belongs to.

      @return The name of the region's memory type.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::string getMemoryType( void ) const;

    /*!
      @brief getClockRatio returns the ratio of the memory region's clock
             frequency compared to the processor's clock frequency.

      @return The region's clock frequency ration relative to the processor
              speed.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    long double getClockRatio( void ) const;

    /*!
      @brief getMinDelay returns the minimum number of clock cycles for which
             a memory region can delay a memory access.

      @return The minimal delay for an access to this memory region.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    unsigned int getMinDelay( void ) const;

    /*!
      @brief getMaxDelay returns the maximum number of clock cycles for which
             a memory region can delay a memory access.

      @param[in] d The delay by which an access may be delayed by other
                   components in the memory hierarchy that are behind this
                   current component. Since memory regions are by definition the
                   last elements in a memory hierarchy without any other caches
                   or buses after them, this parameter is ignored here.
      @return The maximal delay for an access to this memory region.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual unsigned int getMaxDelay( const unsigned int d = 0 ) const;

    /*!
      @brief getMinBurstDelay returns a memory region's minimum burst access
             delay in clock cycles.

      @return The region's minimum burst access delay.

      The burst access delay denotes the number of cycles that elapses until
      content of the memory is completely read/written in burst mode, after the
      very first access of the burst has already happened.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    unsigned long getMinBurstDelay( void ) const;

    /*!
      @brief getMaxBurstDelay returns a memory region's maximum burst access
             delay in clock cycles.

      @return The region's maximum burst access delay.

      The burst access delay denotes the number of cycles that elapses until
      content of the memory is completely read/written in burst mode, after the
      very first access of the burst has already happened.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    unsigned long getMaxBurstDelay( void ) const;

    /*!
      @brief getBusWidth returns the width in bytes of the memory bus that
             connects a region.

      @return The memory buses width in bytes.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    unsigned long getBusWidth( void ) const;

    /*!
      @brief getHierarchy returns all lists of all system components that are
             located in front of this memory region.

      @return A const reference to the set mHierarchyLevels.

      Entries at the beginning of a list are close to the processor (i.e., level
      1) while entries at a list's tail are farther away (e.g., level 3).
      The returned lists only contains caches or buses.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const std::set<std::list<std::reference_wrapper<WIR_SystemComponent>>> &getHierarchy( void ) const;


    //
    // Section handling.
    //

    /*!
      @brief getSections returns the set of sections attached to a region.

      @return A const reference to the set of all attached sections of a
              region.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const WIR_SectionSet &getSections( void ) const;

    /*!
      @brief containsSection returns whether set mSections contains a given
             WIR_Section.

      @param[in] s A const reference to a section to be found.
      @return true if mSections contains the given section, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool containsSection( const WIR_Section & ) const;


  private:

    friend class WIR_System;


    //
    // Constructors.
    //

    /*!
      @brief No standard construction allowed, users must use
             WIR_MemoryRegion( std::string )
             instead.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_MemoryRegion( void ) = delete;

    /*!
      @brief Default constructor creating an empty memory region.

      @param[in] s A const reference to a string to be copied that holds the
                   region's name.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_MemoryRegion( const std::string & );

    /*!
      @brief Default constructor creating an empty memory region.

      @param[in] s An R-value reference to a string to be moved that holds the
                   region's name.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_MemoryRegion( std::string && );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_MemoryRegion & operator = ( const WIR_MemoryRegion & );

    /*!
      @brief Move-assignment operator.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_MemoryRegion & operator = ( WIR_MemoryRegion && );

    /*!
      @brief clone creates a copy of a %WIR memory region.

      @return A pointer to the newly created copy of this memory region.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_MemoryRegion *clone( void ) const;


    //
    // Memory region properties.
    //

    /*!
      @brief setBaseAddress sets a memory region's base address.

      @param[in] s A const reference to the region's physical start address.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setBaseAddress( const WIR_MemoryAddress & );

    /*!
      @brief setLength sets a memory region's length in bytes.

      @param[in] s The region's length in bytes.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setLength( std::size_t );

    /*!
      @brief setFreeSpace sets a memory region's available space in bytes.

      @param[in] s The region's available space in bytes.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setFreeSpace( std::size_t );

    /*!
      @brief setDynamicAllocation sets a memory region's dynamic allocation
             flag.

      @param[in] f A Boolean flag denoting whether the amount of used space in a
                   memory region may vary dynamically at runtime (e.g., for a
                   region containing .stack) or not.

      @author <Til.Mauersberger@tuhh.de>
    */
    void setDynamicAllocation( bool );

    /*!
      @brief setAttributes sets a memory region's attributes.

      @param[in] a A bit-field encoded with type WIR_MemoryRegionAttributes that
                   specifies which attributes are set or not set for this memory
                   region.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setAttributes( unsigned long );

    /*!
      @brief setMemoryType sets the name of the memory type that a region
             belongs to.

      @param[in] n A const reference to the name of the region's memory type.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setMemoryType( const std::string & );

    /*!
      @brief setClockRatio sets the ratio of the memory region's clock frequency
             compared to the processor's clock frequency.

      @param[in] r The region's clock frequency ration relative to the processor
                   speed.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setClockRatio( long double );

    /*!
      @brief setDelay sets a memory region's access delay in clock cycles.

      @param[in] d The region's access latency.

      The access delay denotes the number of cycles that elapses until content
      of the memory is completely read/written.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setDelay( unsigned long );

    /*!
      @brief setBurstDelay sets a memory region's burst access delay in clock
             cycles.

      @param[in] d The region's burst access delay.

      The burst access delay denotes the number of cycles that elapses until
      content of the memory is completely read/written in burst mode, after the
      very first access of the burst has already happened.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setBurstDelay( unsigned long );

    /*!
      @brief setBusWidth sets the width in bytes of the memory bus that connects
             a region.

      @param[in] w The memory buses width in bytes.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setBusWidth( unsigned long );

    /*!
      @brief insertHierarchy adds a new list of system components to set
             mHierarchyLevels.

      @param[in] l An R-value reference to the list of system components to be
                   added.

      insertHierarchy asserts if components other than caches or buses shall be
      inserted.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertHierarchy( std::list<std::reference_wrapper<WIR_SystemComponent>> && );

    /*!
      @brief insertSection inserts the specified ELF executable section into set
             mSections.

      @param[in] s A const reference to the section to be inserted.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertSection( const WIR_Section & );

    /*!
      @brief mBaseAddress stores a memory region's base address.

      The base address denotes the physical start address of a memory region.
    */
    WIR_MemoryAddress mBaseAddress;

    //! mLength stores a memory region's length in bytes.
    std::size_t mLength;

    /*!
      @brief mFreeSpace stores the available space within a region after the
             assembly of sections into the region.
    */
    std::size_t mFreeSpace;

    /*!
      @brief mDynamicAllocation stores whether sections can be allocated
             dynamically during runtime.

      This attribute is mainly used for static WCET analysis and has no physical
      implications or consequences.
    */
    bool mDynamicAllocation;

    /*!
      @brief mAttributes stores a memory region's attributes in the form of a
             bit-field.

      The individual attributes are encoded by type WIR_MemoryRegionAttributes.
    */
    unsigned long mAttributes;

    //! mMemoryType stores the name of the memory type that a region belongs to.
    std::string mMemoryType;

    /*!
      @brief mClockRatio stores the ratio of the memory region's clock frequency
             compared to the processor's clock frequency.
    */
    long double mClockRatio;

    /*!
      @brief mDelay stores a memory region's access delay in clock cycles.

      The access delay denotes the number of cycles that elapses until content
      of the memory is completely read/written.
    */
    unsigned long mDelay;

    /*!
      @brief mBurstDelay stores a memory region's burst access delay in clock
             cycles.

      The burst access delay denotes the number of cycles that elapses until
      content of the memory is completely read/written in burst mode, after the
      very first access of the burst has already happened.
    */
    unsigned long mBurstDelay;

    /*!
      @brief mBusWidth stores the width in bytes of the memory bus that connects
             a region.
    */
    unsigned long mBusWidth;

    /*!
      @brief mHierarchyLevels holds lists of (wrapped) references to all those
             system components that are located in front of this memory region.

      Entries at the beginning of a list are close to the processor (i.e., level
      1) while entries at a list's tail are farther away (e.g., level 3).
    */
    std::set<std::list<std::reference_wrapper<WIR_SystemComponent>>> mHierarchyLevels;

    //! mSections stores the WIR_Sections attached to this region.
    WIR_SectionSet mSections;

};

}       // namespace WIR

#endif  // _WIR_MEMORY_REGION_H
