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
  @file wirsystemconfig.h
  @brief This file provides the interface of a simple parser for %WIR system
         configuration files.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_SYSTEM_CONFIG_H
#define _WIR_SYSTEM_CONFIG_H


//
// Include section
//

// Include standard headers
#include <list>
#include <string>
#include <utility>

// Include WIR headers
#include <wir/wirtypes.h>


//
// Header section
//

namespace WIR {

/*!
  @brief Class WIR_SystemConfig models simple plain-text files used to specify
         system configurations.

  Configuration files have to adhere to the following syntactical and semantical
  rules:
  -# Empty lines and comments (starting with '<tt>#</tt>' going until the end of
     the line) are ignored.
  -# A configuration file describes a set of %WIR system components, i.e.,
     buses, caches, processor cores, memory regions and ELF executable file
     sections. Each such component is described in one dedicated paragraph
     within the config file.
  -# A paragraph always starts with a line specifying the component's
     name: <br />
     <tt>[</tt><em>&lt;name&gt;</em><tt>]</tt> <br />
     %Paragraph names must be unique within one system config file.
  -# The properties of a system component are specified after the paragraph name
     using <br />
     <em>&lt;key&gt;</em> <tt>=</tt> <em>&lt;value&gt;</em> <br />
     directives, exactly one per line.
  -# Each paragraph must contain a specification of the component's type: <br />
     <tt>type = { bus | cache | core | memory | section }</tt> <br />
     The individual properties that can be specified in a config file heavily
     depend on this component type and are detailed in the following.
  -# For <tt>type = bus</tt>:
     -# <tt>arbitration = { fp | pd | rr | tdma }</tt> <br />
        specifies (case-sensitive) the buse's arbitration policy to be
        fixed-priority, priority-division, round robin or TDMA, resp. Depending
        on the specified arbitration policy, the following directives also
        apply.
     -# For <tt>arbitration = fp</tt>:
        -# <tt>priorities = </tt><em>&lt;core list&gt;</em> <br />
           specifies a whitespace-separated list of processor cores. Cores at
           the beginning of this list have high priority, cores at the list tail
           have low priority. Cores of your system that are not specified in
           the priority list have priority 0, i.e., they will never get bus
           access.
     -# For <tt>arbitration = pd</tt>:
        -# <tt>arbitrationdelay = </tt><em>&lt;unsigned integer&gt;</em> <br />
           specifies the bus arbiter's arbitration delay in clock cycles.
        -# <tt>priorities = </tt><em>&lt;core list&gt;</em> <br />
           see above for fixed-priority arbitration.
        -# <tt>slot = </tt><em>&lt;slot specification&gt;</em> <br />
           specifies the characteristics of a bus slot. A slot specification for
           PD is a whitespace-separated tuple that always starts with the slot's
           internal arbitration policy (i.e., one of <tt>pd</tt>, <tt>fp</tt>,
           <tt>rr</tt> or <tt>tdma</tt> again). If the slot is managed using
           <tt>fp</tt> or <tt>rr</tt>, the slot length in clock cycles has to be
           specified next. In case of the other slot arbitration policies, the
           slot specification contains the name of the processor core owning the
           slot, and an unsigned integer denoting the slot's length in clock
           cycles.
     -# For <tt>arbitration = tdma</tt>:
        -# <tt>arbitrationdelay = </tt><em>&lt;unsigned integer&gt;</em> <br />
           see above for priority-division arbitration.
        -# <tt>slot = </tt><em>&lt;slot specification&gt;</em> <br />
           specifies the characteristics of a bus slot. A slot specification for
           TDMA is a whitespace-separated 2-tuple that consists of the name of
           the processor core owning the slot, and an unsigned integer denoting
           the slot's length in clock cycles.
  -# For <tt>type = cache</tt>:
     -# <tt>size = </tt><em>&lt;unsigned integer&gt;</em> <br />
        specifies a cache's total size in bytes. Each paragraph of type
        <tt>cache</tt> must contain a <tt>size</tt> directive.
     -# <tt>associativity = </tt><em>&lt;unsigned integer&gt;</em> <br />
        specifies a cache's associativity. Each paragraph of type <tt>cache</tt>
        must contain an <tt>associativity</tt> directive.
     -# <tt>linesize = </tt><em>&lt;unsigned integer&gt;</em> <br />
        specifies the size of a cache line in bytes. Each paragraph  of type
        <tt>cache</tt> must contain a <tt>linesize</tt> directive.
     -# <tt>cache_type = { D | I | U }</tt> <br />
        specifies (case-insensitive) whether the cache is a data, instruction
        or unified cache.
     -# <tt>enabled = { 0 | 1 }</tt> <br />
        specifies whether the cache is enabled and considered during analysis,
        or not.
     -# <tt>shared = { 0 | 1 }</tt> <br />
        specifies whether the cache is shared among multiple cores or not.
     -# <tt>writethrough = { 0 | 1 }</tt> <br />
        specifies whether the cache applies the write-through policy or not.
     -# <tt>writeallocate = { 0 | 1 }</tt> <br />
        specifies whether the cache applies the write-allocate policy or not.
     -# <tt>hitdelay = </tt><em>&lt;unsigned integer&gt;</em> <br />
        specifies the time in cycles that a hit in this cache needs to complete
        in the best case.
     -# <tt>missdelay = </tt><em>&lt;unsigned integer&gt;</em> <br />
        specifies the time in cycles that a miss in this cache needs to complete
        in the worst case.
     -# <tt>buswidth = </tt><em>&lt;unsigned integer&gt;</em> <br />
        specifies the width in bytes of the memory bus that connects a cache.
     -# <tt>percent_size = </tt><em>&lt;unsigned integer&gt;</em> <br />
        specifies a cache's total size as a percentage of a task's size.
  -# For <tt>type = core</tt>:
     -# <tt>isa = </tt><em>&lt;name&gt;</em> <br />
        specifies a processor core's ISA; use the same name as provided by
        WIR_BaseProcessor::getISAName() here. Each paragraph of type
        <tt>core</tt> must contain an <tt>isa</tt> directive.
     -# <tt>clockfreq = </tt><em>&lt;unsigned integer&gt;</em> <br />
        specifies a core's clock frequency in Hertz.
     -# <tt>voltage = </tt><em>&lt;double&gt;</em> <br />
        specifies a core's supply voltage in Volt. Use '<tt>.</tt>' as decimal
        point.
  -# For <tt>type = memory</tt>:
     -# <tt>origin = </tt><em>&lt;unsigned integer&gt;</em> <br />
        specifies a memory region's physical start address.
     -# <tt>length = </tt><em>&lt;unsigned integer&gt;</em> <br />
        specifies a memory region's physical length. For each processor core,
        the specified memory address ranges must not overlap.
     -# <tt>dynamic_allocation = { 0 | 1 }</tt> <br />
        specifies whether the amount of used space in a memory region may vary
        dynamically at runtime (e.g., for a region containing .stack), or not.
        This information can act as helper to reduce the complexity of static
        WCET analysis.
     -# <tt>attributes = </tt><em>&lt;attribs&gt;</em> <br />
        specifies a memory region's attributes. Use a combination of the letters
        '<tt>a</tt>', '<tt>c</tt>', '<tt>i</tt>', '<tt>r</tt>', '<tt>w</tt>' or
        '<tt>x</tt>' (case-insensitive) to specify whether the region is
        <em>allocatable</em> (i.e., the memory should be allocated by the
        linker), <em>cached</em>, <em>initialized</em>, <em>readable</em>,
        <em>writable</em> or <em>executable</em>, i.e., instructions can be
        executed from this region. Use '<tt>!</tt>' to invert the following
        attribute.
     -# <tt>memory_type = </tt><em>&lt;name&gt;</em> <br />
        specifies the name of the physical memory that contains the current
        memory region. This might be useful if you virtually split a physical
        memory into different regions and want to document that all these
        regions reside in the same physical memory.
     -# <tt>clockratio = </tt><em>&lt;ratio&gt;</em> <br />
        specifies the ratio of the memory region's clock frequency compared to
        the processor's clock frequency. Use '<tt>:</tt>' to separate both parts
        ot the ratio.
     -# <tt>cycles = </tt><em>&lt;unsigned integer&gt;</em> <br />
        specifies a memory region's access delay in clock cycles. The access
        delay denotes the number of cycles that elapses until content of the
        memory is completely read/written.
     -# <tt>burstcycles = </tt><em>&lt;unsigned integer&gt;</em> <br />
        specifies a memory region's burst access delay in clock cycles. The
        burst access delay denotes the number of cycles that elapses until
        content of the memory is completely read/written in burst mode, after
        the very first access of the burst has already happened.
     -# <tt>buswidth = </tt><em>&lt;unsigned integer&gt;</em> <br />
        specifies the width in bytes of the memory bus that connects a region.
     -# <tt>hierarchy = </tt><em>&lt;component list&gt;</em> <br />
        specifies a path through a system architecture along which a processor
        core can reach the current memory region. Such a path is given as a
        whitespace-separated list of component names. A path always has to start
        with the name of a processor core which is then followed by a sequence
        of caches and/or buses. A path implicitly always ends with the current
        memory region so that this one must not be specified here. <br />
        Use multiple <tt>hierarchy</tt> directives if several cores in a
        multi-core setup can access one single, shared memory region.
     -# <tt>sections = </tt><em>&lt;section list&gt;</em> <br />
        specifies which ELF executable file sections are attached to the current
        memory region. Such a list of sections is given as a
        whitespace-separated list of section names. In a multi-core setup, %WIR
        assumes that an ELF executable file is generated for each core of the
        system. Thus, the specified sections of a memory region must also be
        related to the system's cores. This is done by means of the
        <tt>hierarchy</tt> directives above which specify from which core a
        memory region can be reached. Thus, ELF sections will be created for all
        sections listed in a region's <tt>section</tt> directive AND for all
        cores occuring in a region's <tt>hierarchy</tt> directives. For each
        core, there must at least exist the default sections <tt>.text</tt>,
        <tt>.data</tt>, <tt>.bss</tt> and <tt>.rodata</tt>.
     -# <tt>load = </tt><em>&lt;section list&gt;</em> <br />
        specifies sections which are loaded from the current memory region
        (which thus usually is some kind of ROM or NVRAM) to their destination
        region during a system's boot time.
  -# For <tt>type = section</tt>:
     -# The name of an ELF executable file section has to be specified such that
        it is unambiguously clear which section of which core is meant. In case
        of a single-core architecture, it is thus sufficient to start the
        paragraph just with the section name: <br />
        <tt>[</tt><em>&lt;section name&gt;</em><tt>]</tt> <br />
        For multi-core architectures, both section and core name have to be
        specified, separated by a whitespace: <br />
        <tt>[</tt><em>&lt;section name&gt;</em>&nbsp;<em>&lt;core
        name&gt;</em><tt>]</tt> <br />
     -# <tt>alignment = </tt><em>&lt;unsigned integer&gt;</em> <br />
        specifies a section's external alignment, i.e., the alignment of the
        section's start/end boundaries. <br />
        The alignment denotes the number of least-significant zero bits that
        start/end addresses must have. An alignment of 3 means that the
        least-significant 3 bits are '<tt>0</tt>', i.e., that the resulting
        start/end addresses are multiples of 8.
     -# <tt>block = </tt><em>&lt;unsigned integer&gt;</em> <br />
        specifies a section's internal alignment, i.e., the alignment of the
        section's internal relocation counter after an advancement.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_SystemConfig final
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor reading a system configuration file.

      @param[in] f A const reference to a string holding the configuration file
                   name to be read.

      This constructor asserts if the specified filename cannot be opened or if
      the configuration file contains syntax errors.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_SystemConfig( const std::string & );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ~WIR_SystemConfig( void );


    //
    // Data structures used to model system configuration files.
    //

    /*!
      @brief class Paragraph models a configuration file paragraph that has a
             dedicated name and consists of key-value pairs.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    class Paragraph
    {

      public:

        //
        // Local type definitions.
        //

        /*!
          @brief For each property of a configuration file paragraph (i.e., each
                 'key = value' pair), its value is represented by struct value.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        struct value
        {

          //! value stores a property's value as a plain-text string.
          std::string value;

          /*!
            @brief line stores the line number where a property occured in a
                   configuration file.
          */
          unsigned int line;

        };

        /*!
          @brief Default constructor creating a named paragraph.

          @param[in] n A const reference to a string holding the paragraph's
                       name.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        explicit Paragraph( const std::string & );

        /*!
          @brief getName returns a paragraph's specific name.

          @return A string holding the paragraph's name.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        std::string getName( void ) const;

        /*!
          @brief setLine sets the line number where a config file paragraph
                 starts.

          @param[in] l The line number.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        void setLine( unsigned int );

        /*!
          @brief getLine returns the line number where a config file paragraph
                 starts.

          @return The paragraph's line number.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        unsigned int getLine( void ) const;

        /*!
          @brief setType sets a paragraph's specific type.

          @param[in] t The paragraph's memory type.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        void setType( const WIR_SystemComponentType );

        /*!
          @brief getType returns a paragraph's specific type.

          @return The paragraph's memory type.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        WIR_SystemComponentType getType( void ) const;

        /*!
          @brief isTypeSet returns whether a paragraph's type has been
                 specified.

          @return true iff the type has been given previously, false otherwise.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        bool isTypeSet( void ) const;

        /*!
          @brief addProperty adds the given key-value pair to a paragraph.

          @param[in] k A const reference to a string holding the key.
          @param[in] v A const reference to a string holding the value.
          @param[in] l The line number of the key-value pair.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        void addProperty( const std::string &, const std::string &,
                          unsigned int );

        /*!
          @brief getProperties returns all properties associated with a
                 paragraph.

          @return A const reference to the list mProperties.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        const std::list<std::pair<std::string, struct value>> &getProperties( void ) const;


      private:

        //! mName stores the name of a config file paragraph.
        std::string mName;

        //! mLine stores the line number where a config file paragraph starts.
        unsigned int mLine;

        /*!
          @brief mType stores a paragraph's type, i.e., whether it is a bus,
                 cache, core, memory region or ELF section.
        */
        WIR_SystemComponentType mType;

        //! mTypeSet stores whether a paragraph's type has been set or not.
        bool mTypeSet;

        //! mProperties stores key/value pairs.
        std::list<std::pair<std::string, struct value>> mProperties;

    };

    /*!
      @brief getParagraphs returns all paragraphs read from a configuration
             file.

      @return A const reference to the set mParagraphs.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const std::list<Paragraph> &getParagraphs( void ) const;


  private:

    // No standard construction allowed, users must use
    // WIR_SystemConfig( const std::string & ) instead.
    WIR_SystemConfig( void ) = delete;
    WIR_SystemConfig( const WIR_SystemConfig & ) = delete;
    WIR_SystemConfig( WIR_SystemConfig && ) = delete;
    WIR_SystemConfig & operator = ( const WIR_SystemConfig & ) = delete;
    WIR_SystemConfig & operator = ( WIR_SystemConfig && ) = delete;

    //! mParagraphs holds all paragraphs read from a configuration file.
    std::list<Paragraph> mParagraphs;

};

}       // namespace WIR

#endif  // _WIR_SYSTEM_CONFIG_H
