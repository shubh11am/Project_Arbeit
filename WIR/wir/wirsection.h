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
  @file wirsection.h
  @brief This file provides the interface of %WIR object file sections.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_SECTION_H
#define _WIR_SECTION_H


//
// Include section
//

// Include standard headers
#include <string>

// Include boost headers
#include <boost/optional.hpp>

// Include WIR headers
#include <wir/API/wiridapi.h>
#include <wir/API/wirinsertionapi.h>
#include <wir/wirtypes.h>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_BaseProcessor;
class WIR_MemoryRegion;


/*!
  @brief Class WIR_Section is the generic representation of object file
         sections.

  This class serves as virtual base class from which sections for actual
  processor instances are derived.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_Section final : public WIR_ID_API
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor creating a named section.

      @param[in] s A const reference to a string to be copied that holds the
                   section's name.
      @param[in] r A const reference to a %WIR memory region to which this
                   section is attached.

      Default alignment and block values (10 and 3, resp.) are taken over from
      WCC's old memorylayout, for both ARM and TriCore. This constructor asserts
      if it is passed an empty string.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Section( const std::string &, const WIR_MemoryRegion & );

    /*!
      @brief Default constructor creating a named section.

      @param[in] s An R-value reference to a string to be moved that holds the
                   section's name.
      @param[in] r A const reference to a %WIR memory region to which this
                   section is attached.

      Default alignment and block values (10 and 3, resp.) are taken over from
      WCC's old memorylayout, for both ARM and TriCore. This constructor asserts
      if it is passed an empty string.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Section( std::string &&, const WIR_MemoryRegion & );

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Section( const WIR_Section & );

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Section( WIR_Section && );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ~WIR_Section( void );


    //
    // System handling.
    //

    // Realize the API to manage a section's parent system.
    WIR_INSERTION_DECL( WIR_BaseProcessor, Processor );


    //
    // Name handling.
    //

    /*!
      @brief getName returns a section's specific name.

      @return A string that holds the section's name.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::string getName( void ) const;


    //
    // Section properties.
    //

    /*!
      @brief setAlignment sets the section's external alignment, i.e., the
             alignment of the section's start/end boundaries.

      @param[in] a The section's new alignment.

      The alignment denotes the number of least-significant zero bits that
      start/end addresses must have. An alignment of 3 means that the
      least-significant 3 bits are '0', i.e., that the resulting start/end
      addresses are multiples of 8.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setAlignment( std::size_t );

    /*!
      @brief getAlignment returns the section's external alignment, i.e., the
             alignment of the section's start/end boundaries.

      @return The section's alignment.

      The alignment denotes the number of least-significant zero bits that
      start/end addresses must have. An alignment of 3 means that the
      least-significant 3 bits are '0', i.e., that the resulting start/end
      addresses are multiples of 8.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::size_t getAlignment( void ) const;

    /*!
      @brief setBlock sets the section's internal alignment, i.e., the alignment
             of the section's internal relocation counter after an advancement.

      @param[in] a The section's internal alignment.

      The alignment denotes the number of least-significant zero bits that the
      relocation counter must have. An alignment of 3 means that the
      least-significant 3 bits are '0', i.e., that the resulting relocation
      counter is a multiple of 8.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setBlock( std::size_t );

    /*!
      @brief getBlock returns the section's internal alignment, i.e., the
             alignment of the section's internal relocation counter after an
             advancement.

      @return The section's internal alignment.

      The alignment denotes the number of least-significant zero bits that the
      relocation counter must have. An alignment of 3 means that the
      least-significant 3 bits are '0', i.e., that the resulting relocation
      counter is a multiple of 8.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::size_t getBlock( void ) const;

    /*!
      @brief getStart returns a section's Virtual Memory Address (VMA).

      @return The section's start address (mStart).

      In most cases, LMA and VMA are the same. However, some ROM-based systems
      might need to copy a section from ROM into RAM during startup time. For
      this particular situation, the LMA denotes the ROM address to which a
      section is loaded, and the VMA denotes the RAM address to which the
      section is copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_MemoryAddress getStart( void ) const;

    /*!
      @brief isStartSet returns whether the section's VMA has been set
             previously.

      @return true iff the VMA is set, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isStartSet( void ) const;

    /*!
      @brief getAt returns a section's Loaded Memory Address (LMA).

      @return The section's load address (mAt).

      In most cases, LMA and VMA are the same. However, some ROM-based systems
      might need to copy a section from ROM into RAM during startup time. For
      this particular situation, the LMA denotes the ROM address to which a
      section is loaded, and the VMA denotes the RAM address to which the
      section is copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_MemoryAddress getAt( void ) const;

    /*!
      @brief isAtSet returns whether the section's LMA has been set previously.

      @return true iff the LMA is set, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isAtSet( void ) const;

    /*!
      @brief getLength returns a section's length in bytes.

      @return The section's length.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::size_t getLength( void );

    /*!
      @brief getFill returns the fill pattern for uninitialized section
             intervals.

      @return The section's fill pattern.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    unsigned short getFill( void ) const;

    /*!
      @brief getProcessor returns the WIR_MemoryRegion to which this section is
             attached.

      @return A reference to the attached WIR_MemoryRegion.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_MemoryRegion &getRegion( void ) const;

    /*!
      @brief getLoadRegion returns a region from which this section is loaded
             during a system's boot time.

      @return A reference to the section's load region.

      If no load region was set previously, getLoadRegion fails with an
      assertion.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_MemoryRegion &getLoadRegion( void ) const;

    /*!
      @brief isLoadRegionSet returns whether a load region has been set for this
             section previously.

      @return true iff a load region is set, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isLoadRegionSet( void ) const;


  private:

    friend class WIR_BaseProcessor;
    friend class WIR_System;

    //
    // Constructors.
    //

    /*!
      @brief No standard construction allowed, users must use
             WIR_Section( const std::string ) instead.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Section( void ) = delete;

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Section & operator = ( const WIR_Section & );

    /*!
      @brief Move-assignment operator.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Section & operator = ( WIR_Section && );


    //
    // Section properties.
    //

    /*!
      @brief setStart sets a section's Virtual Memory Address (VMA).

      @param[in] a The section's start address (mStart).

      In most cases, LMA and VMA are the same. However, some ROM-based systems
      might need to copy a section from ROM into RAM during startup time. For
      this particular situation, the LMA denotes the ROM address to which a
      section is loaded, and the VMA denotes the RAM address to which the
      section is copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setStart( const WIR_MemoryAddress & );

    /*!
      @brief setAt sets a section's Loaded Memory Address (LMA).

      @param[in] a The section's load address (mAt).

      In most cases, LMA and VMA are the same. However, some ROM-based systems
      might need to copy a section from ROM into RAM during startup time. For
      this particular situation, the LMA denotes the ROM address to which a
      section is loaded, and the VMA denotes the RAM address to which the
      section is copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setAt( const WIR_MemoryAddress & );

    /*!
      @brief setLength sets a section's length in bytes.

      @param[in] l The section's length.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setLength( std::size_t );

    /*!
      @brief setFill sets the fill pattern for uninitialized section intervals.

      @param[in] f The section's fill pattern.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setFill( unsigned short );

    /*!
      @brief setLoadRegion sets a region from which this section is loaded
             during a system's boot time.

      @param[in] r A reference to the section's load region.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setLoadRegion( WIR_MemoryRegion & );

    //! mName holds an object's name.
    std::string mName;

    /*!
      @brief mAlignment stores the section's external alignment, i.e., the
             alignment of the section's start/end boundaries.

      The alignment denotes the number of least-significant zero bits that
      start/end addresses must have. An alignment of 3 means that the
      least-significant 3 bits are '0', i.e., that the resulting start/end
      addresses are multiples of 8.
    */
    std::size_t mAlignment;

    /*!
      @brief mBlock stores the section's internal alignment, i.e., the alignment
             of the section's internal relocation counter after an advancement.

      The alignment denotes the number of least-significant zero bits that the
      relocation counter must have. An alignment of 3 means that the
      least-significant 3 bits are '0', i.e., that the resulting relocation
      counter is a multiple of 8.
    */
    std::size_t mBlock;

    /*!
      @brief mAt stores a section's Virtual Memory Address (VMA).

      In most cases, LMA and VMA are the same. However, some ROM-based systems
      might need to copy a section from ROM into RAM during startup time. For
      this particular situation, the LMA denotes the ROM address to which a
      section is loaded, and the VMA denotes the RAM address to which the
      section is copied.
    */
    boost::optional<WIR_MemoryAddress> mStart;

    /*!
      @brief mAt stores a section's Loaded Memory Address (LMA).

      In most cases, LMA and VMA are the same. However, some ROM-based systems
      might need to copy a section from ROM into RAM during startup time. For
      this particular situation, the LMA denotes the ROM address to which a
      section is loaded, and the VMA denotes the RAM address to which the
      section is copied.
    */
    boost::optional<WIR_MemoryAddress> mAt;

    //! mLength stores a section's length in bytes.
    std::size_t mLength;

    //! mFill stores the fill pattern for uninitialized section intervals.
    unsigned short mFill;

    /*!
      @brief mRegion points to the %WIR memory region to which this object is
             attached.
    */
    WIR_MemoryRegion *mRegion;

    /*!
      @brief mLoadRegion points to the %WIR memory region from which this
             section is loaded during a system's boot time.
    */
    WIR_MemoryRegion *mLoadRegion;

};

}       // namespace WIR

#endif  // _WIR_SECTION_H
