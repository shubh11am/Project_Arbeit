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
  @file wirsystem.h
  @brief This file provides the interface of the top level of an entire %WIR
         system.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_SYSTEM_H
#define _WIR_SYSTEM_H


//
// Include section
//

// Include standard headers
#include <functional>
#include <list>
#include <map>
#include <memory>
#include <set>
#include <string>
#include <typeinfo>

// Include WIR headers
#include <wir/API/wircontainerapi.h>
#include <wir/API/wiridapi.h>
#include <wir/API/wirlistapi.h>
#include <wir/API/wirnameapi.h>
#include <wir/API/wirpointerlistapi.h>
#include <wir/API/wirsymboltableapi.h>
#include <wir/wirtaskmanager.h>
#include <wir/wirtypes.h>
#include <flowfacts/wirflowfact.h>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_BaseProcessor;
class WIR_CompilationUnit;
class WIR_MemoryRegion;
class WIR_Section;
class WIR_SystemComponent;
class WIR_FlowFact;


/*!
  @brief Class WIR_System models a complete heterogeneous multi-core hardware
         and software system.

  For this purpose, WIR_System models all hardware resources featured by a
  target architecture like, e.g., processors, shared memories, shared busses
  and arbitration schemes for shared hardware resources.

  To model the software running on this architecture, WIR_System manages
  different WIR tasks.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_System final : public WIR_ID_API,
                         public WIR_Container_API,
                         public WIR_Name_API,
                         public WIR_SymbolTable_API
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor for %WIR systems using the specified hardware
             configuration and a given task manager.

      @param[in] n A const reference to a string holding the name of a system
                   configuration to be used.
      @param[in] t A const reference to a task manager to be used.

      Matching system names with configuration file paths is done according to
      the following rules:

      -# If n already is a path (either absolute or relative), i.e., n contains
         at least one '/' somewhere, this input path n is directly returned.
         Note that it lies in the sole responsibility of the caller of this
         constructor that n is a valid path in this case!
      -# Otherwise, getConfigPath probes the WIR-internal directly structure
         (archdir and sysconfdir) and checks whether a file called n exists
         below archdir or sysconfdir. If so, the full path to the found file is
         returned.
      -# Otherwise, no valid file path was determined and an error message is
         issued.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_System( const std::string &, const WIR_TaskManager & );

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_System( const WIR_System & );

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_System( WIR_System && );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ~WIR_System( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_System & operator = ( const WIR_System & );

    /*!
      @brief Move-assignment operator.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_System & operator = ( WIR_System && );

    /*!
      @brief getSystemName returns a %WIR system's architecture name as given by
             the hardware vendor.

      @return A string holding the system's architecture name.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::string getSystemName( void ) const;


    //
    // Various lists.
    //

    // A list API for all compilation units managed by a %WIR system.
    WIR_LIST_DECL( WIR_CompilationUnit, CompilationUnit, public );

    /*!
      @brief begin returns an iterator to the first compilation unit of a
             system.

      @return A const iterator pointing to the first compilation unit.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<std::reference_wrapper<WIR_CompilationUnit>>::const_iterator begin( void ) const;

    /*!
      @brief end returns an iterator to the end of a system's compilation unit
             list.

      @return A const iterator pointing to the position after the last
              compilation unit of a system.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<std::reference_wrapper<WIR_CompilationUnit>>::const_iterator end( void ) const;


    //
    // System component handling.
    //

    /*!
      @brief getComponents returns the set mComponentReferences.

      @return A const reference to the set mComponentReferences.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const WIR_SystemComponentSet &getComponents( void ) const;

    /*!
      @brief This variant of getComponents returns a correctly casted set of
             system components of type DerivedComponentClass.

      @tparam DerivedComponentClass The name of a derived system component
                                    class.
      @return A set containing (wrapped) references to all attached system
              components of template type DerivedComponentClass. The returned
              references are already correctly casted.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    template<typename DerivedComponentClass>
    const std::set<std::reference_wrapper<DerivedComponentClass>, WIR_Compare<DerivedComponentClass>> getComponents( void ) const
    {
      DSTART(
        "const set<reference_wrapper<DerivedComponentClass>, WIR_Compare<DerivedContainerClass> > WIR_System::getComponents() const" );

      std::set<std::reference_wrapper<DerivedComponentClass>, WIR_Compare<DerivedComponentClass>> res;

      for ( WIR_SystemComponent &c : mComponentReferences ) {
        try {
          res.insert( dynamic_cast<DerivedComponentClass &>( c ) );
        }
        catch ( std::bad_cast &bc ) {
        }
      }

      return( res );
    };

    /*!
      @brief getComponentHierarchyLevels returns a map that groups together all
             cores/caches/buses of a system's component hierarchy in sets that
             lie on the very same hierarchy level.

      @return A const reference to the map mComponentHierarchyLevels.

      The map returned by getComponentHierarchyLevels can be queried with a
      number of a memory hierarchy level, with 0 being the level closest to the
      system's processor cores.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const std::map<unsigned int, WIR_SystemComponentSet> &getComponentHierarchyLevels( void ) const;

    /*!
      @brief findComponent finds a WIR_SystemComponent with the specified ID in
             set mComponentReferences.

      @param[in] id A system component's ID to be found.
      @return An iterator pointing to the found element with the specified ID,
              or the end() iterator otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_SystemComponentSet::const_iterator findComponent( WIR_id_t ) const;

    /*!
      @brief findComponent finds a WIR_SystemComponent with the specified name
             in set mComponentReferences.

      @param[in] n A system component's name to be found.
      @return An iterator pointing to the found element with the specified name,
              or the end() iterator otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_SystemComponentSet::const_iterator findComponent( const std::string & ) const;

    /*!
      @brief containsComponent returns whether set mComponentReferences
             contains a WIR_SystemComponent with the specified ID.

      @param[in] id A system component's ID to be found.
      @return true if mComponentReferences contains an object with the given ID,
              false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool containsComponent( WIR_id_t ) const;

    /*!
      @brief containsComponent returns whether set mComponentReferences
             contains a WIR_SystemComponent with the specified name.

      @param[in] n A system component's name to be found.
      @return true if mComponentReferences contains an object with the given
              name, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool containsComponent( const std::string & ) const;

    /*!
      @brief findMemoryRegion finds a WIR_MemoryRegion in set
             mComponentReferences that includes the specified address.

      @param[in] a A const reference to an address covered by the memory region
                   to be found.
      @return An iterator pointing to the found element covering the specified
              address, or the end() iterator otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_SystemComponentSet::const_iterator findMemoryRegion( const WIR_MemoryAddress & ) const;

    /*!
      @brief findMemoryRegions finds all memory regions that intersect with the
             given address range.

      @param[in] a A const reference to an address range.
      @return A set including all memory regions that are affected by the given
              address range.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_SystemComponentSet findMemoryRegions( const WIR_AddressRange & ) const;


    //
    // Flow fact handling.
    //

    /*!
      @brief getFlowFacts returns the set mFlowFactReferences.

      @return A const reference to the set mFlowFactReferences.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    const std::list<std::reference_wrapper<WIR_FlowFact>> &getFlowFacts( void ) const;

    /*!
      @brief This variant of getFlowFacts returns a correctly casted set of flow
             facts of type DerivedFlowFactClass.

      @tparam DerivedFlowFactClass The name of a derived flow fact class.
      @return A set containing (wrapped) references to all attached flow facts
              of template type DerivedFlowFactClass. The returned references are
              already correctly casted.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    template<typename DerivedFlowFactClass>
    const std::list<std::reference_wrapper<DerivedFlowFactClass>> getFlowFacts( void ) const
    {
      DSTART(
        "const list<reference_wrapper<DerivedFlowFactClass> > "
        "WIR_System::getFlowFacts() const" );

      std::list<std::reference_wrapper<DerivedFlowFactClass>> res;
      WIR_FlowFactType ff_type = DerivedFlowFactClass().getType();

      for ( WIR_FlowFact &f : mFlowFactReferences )
        if ( ff_type == f.getType() )
          res.push_back( dynamic_cast<DerivedFlowFactClass &>( f ) );

      return( res );
    };

    /*!
      @brief pushBackFlowFact adds a new WIR_FlowFact at the end of lists
             mFlowFactPointers and mFlowFactReferences.

      @param[in] f A const reference to the flow fact to be copy-added.
      @return A reference to the newly inserted element.

      The content of f is copied to the new list element.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    WIR_FlowFact &pushBackFlowFact( const WIR_FlowFact & );

    /*!
      @brief eraseFlowFact removes a single flow fact from the specified
             position.

      @param[in] pos An iterator denoting the position where the element
                     is removed.

      This destroys the removed element.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    void eraseFlowFact( std::list<std::reference_wrapper<WIR_FlowFact>>::const_iterator );

    /*!
      @brief eraseFlowFact removes a single flow fact with the specified ID.

      @param[in] id The ID of the object to be removed.

      This destroys the removed element.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    void eraseFlowFact( WIR_id_t );

    /*!
      @brief containsFlowFact returns whether the list mFlowFactReferences
             contains a flow fact with the specified ID.

      @param[in] id An object's ID to be found.
      @return true if mFlowFactReferences contains an object with the given
              ID, false otherwise.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    bool containsFlowFact( WIR_id_t ) const;

    /*!
      @brief clearFlowFacts removes all elements from lists
             mFlowFactPointers and mFlowFactReferences.

      This destroys all removed elements.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    void clearFlowFacts( void );


    //
    // Task manager handling.
    //

    /*!
      @brief getTaskManager returns the %WIR task manager associated with this
             %WIR system.

      @return A reference to the used %WIR task manager.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_TaskManager &getTaskManager( void ) const;


    //
    // Modification prevention.
    //

    /*!
      @brief setDontOptimize sets whether a %WIR system can be modified or must
             not be changed by some optimization or transformation.

      @param[in] f A Boolean flag denoting whether the system must not be
                   modified.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setDontOptimize( bool = true );

    /*!
      @brief getDontOptimize returns whether a %WIR system can be modified or
             must not be changed by some optimization or transformation.

      @return true if the system must not be modified, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool getDontOptimize( void ) const;


    //
    // Stream I/O.
    //

    /*!
      @brief The << operator dumps a %WIR system to an output stream.

      @param[in] os A reference to an output stream.
      @param[in] s A const reference to the %WIR system to be dumped.
      @return A reference to the same output stream.

      By applying processor-specific I/O manipulators to the output stream
      beforehand, this << operator can flexibly emit valid assembly output for
      arbitrary processor architectures.

      When activating I/O manipulator ldscript before, this << operator does not
      emit assembly code but a linker script for the system's memory layout
      instead.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    friend std::ostream & operator << ( std::ostream &,
                                        const WIR_System & );


  private:

    friend class WIR_MemoryRegion;
    friend class WIR_Section;
    friend class WIR_Symbol;

    /*!
      @brief No standard construction allowed, users must use
             WIR_System( const WIR_TaskManager & ) instead.
    */
    WIR_System( void ) = delete;

    /*!
      @brief copySystem performs actions common to the copy constructor and copy
             assignment operator of %WIR systems.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void copySystem( const WIR_System & );

    /*!
      @brief checkDontOptimize checks whether a %WIR system must not be
             modified.

      If this system must not be modified, checkDontOptimize asserts.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void checkDontOptimize( void ) const;

    /*!
      @brief setSystemName sets a %WIR system's architecture name as given by
             the hardware vendor.

      @param[in] s A const reference to a string holding the system's name.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setSystemName( const std::string & );


    //
    // System component handling.
    //

    /*!
      @brief For the given name of system configuration, getConfigPath
             determines the file path to the corresponding system configuration
             file.

      @param[in] n A const reference to a string holding the name of a system
                   configuration to be used.
      @return A string holding the file path to a configuration file that is
              associated with the specified configuration name.

      Matching system names with configuration file paths is done according to
      the following rules:

      -# If n already is a path (either absolute or relative), i.e., n contains
         at least one '/' somewhere, this input path n is directly returned.
         Note that it lies in the sole responsibility of the caller of
         getConfigPath that n is a valid path in this case!
      -# Otherwise, getConfigPath probes the WIR-internal directly structure
         (archdir and sysconfdir) and checks whether a file called n exists
         below archdir or sysconfdir. If so, the full path to the found file is
         returned.
      -# Otherwise, no valid file path was determined and getConfigPath returns
         the empty string.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::string getConfigPath( const std::string & );

    /*!
      @brief createComponents reads a system's component hierarchy from a
             configuration file and creates an according set of
             WIR_SystemComponent objects.

      @param[in] f A const reference to a string holding the configuration file
                   name to be read.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void createComponents( const std::string & );

    /*!
      @brief createSections creates the specified sections and attaches them to
             their respective processor cores.

      @param[in] r A reference to a memory region used to identify the relevant
                   processor cores.
      @param[in] sectionNames A const reference to a set of section names to be
                              created.
      @param[in] f A const reference to a string denoting the system config file
                   name
      @param[in] l An unsigned integer denoting the line number within the
                   system config file.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void createSections( WIR_MemoryRegion &, const std::set<std::string> &,
                         const std::string &, unsigned int ) const;

    /*!
      @brief attachLoadedSections attaches all sections specified in some 'load'
             directives to their corresponding memory regions.

      @param[in] l A const reference to a map storing the load directives.
      @param[in] f A const reference to a string denoting the system config file
                   name

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void attachLoadedSections( const std::map<std::reference_wrapper<WIR_MemoryRegion>, std::set<std::string>, WIR_Compare<WIR_MemoryRegion>> &,
                               const std::string & );

    /*!
      @brief computeMaximumAccessTimes computes the maximum access times that
             memory regions can have and propagates them into a system's buses.

      computeMaximumAccessTimes scans all 'hierarchy' directives of memory
      regions backwards and maximizes/accumulates the access times of all system
      components visited this way.
    */
    void computeMaximumAccessTimes( void );

    /*!
      @brief computeHierarchyLevels computes to which level of the component
             hierarchy all the cores, caches and buses in mComponentReferences
             belong.

      Independent buses/caches are put in the same level of the hierarchy, while
      cascaded ones are put in different levels.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void computeHierarchyLevels( void );

    /*!
      @brief insertComponent adds a new WIR_SystemComponent to the sets
             mComponentPointers and mComponentReferences.

      @param[in] o A const reference to the WIR_SystemComponent to be
                   copy-added.
      @return A reference to the newly inserted element.

      As a side effect, all already computed memory hierarchy levels of a system
      are discarded, since the insertion of a new component into the entire
      hierarchy might change the previously computed information. The content of
      o is copied to the new set element.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_SystemComponent &insertComponent( const WIR_SystemComponent & );

    /*!
      @brief clearComponents removes all elements from sets mComponentPointers,
             mComponentReferences and mComponentHierarchyLevels.

      This destroys all removed elements.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void clearComponents( void );


    //
    // Symbol handling.
    //

    /*!
      @brief For all functions, basic blocks and data objects of the specified
             compilation unit, insertSymbols adds corresponding symbols to the
             system's symbol table.

      @param[in] u A const reference to a compilation for which symbols shall be
                   added.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertSymbols( const WIR_CompilationUnit & );

    /*!
      @brief computeMemoryLayout examines all data objects, functions and basic
             blocks of a %WIR system and computes their absolute start
             addresses according to the address ranges of their respective
             memory regions.

      @note This method is the redesigned variant of good old
            LLIR_MemoryLayout::assembleSymbolTables().

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void computeMemoryLayout( void );

    /*!
      @brief computeSectionLayout examines all data objects, functions and basic
             blocks of a %WIR system and computes their start addresses within
             their assigned ELF sections as well as the byte sizes of the
             sections.

      @note This method is the redesigned variant of good old
            LLIR_ObjectSectionLayout::refreshSymbolTable().

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void computeSectionLayout( void );

    /*!
      @brief computePhysicalAddresses takes the start address of data objects,
             functions and basic blocks (stemming from computeSectionLayout)
             relative to their assigned section plus the ordering of the
             sections (stemming from computeMemoryLayout) within their assigned
             memory regions and determines the final physical memory addresses
             of all symbols in a system's symbol table.

      @note This method is the redesigned variant of good old
            LLIR_MemoryLayout::refreshLLIRSymbolTable().

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void computePhysicalAddresses( void );

    std::string mSystemName;

    /*!
      @brief mComponentPointers holds managed pointers to all stored %WIR system
             components.
    */
    std::set<std::unique_ptr<WIR_SystemComponent>> mComponentPointers;

    /*!
      @brief mComponentReferences holds (wrapped) references to all stored %WIR
             system components.
    */
    WIR_SystemComponentSet mComponentReferences;

    /*!
      @brief mComponentHierarchyLevels groups together all those
             cores/caches/buses in a common set that lie on the very same level
             of the system's memory hierarchy.

      mComponentHierarchyLevels can be queried with a number of a memory
      hierarchy level, with 0 being the level closest to the system's processor
      cores.
    */
    std::map<unsigned int, WIR_SystemComponentSet> mComponentHierarchyLevels;

    //! mFlowFactPointers holds managed pointers to all stored %WIR flow facts.
    std::list<std::unique_ptr<WIR_FlowFact>> mFlowFactPointers;

    /*!
      @brief mFlowFactReferences holds (wrapped) references to all stored %WIR
             flow facts.
    */
    std::list<std::reference_wrapper<WIR_FlowFact>> mFlowFactReferences;

    //! mTaskManager stores a managed pointer to a system's used task manager.
    std::unique_ptr<WIR_TaskManager> mTaskManager;

    /*!
      @brief mDontOptimize stores whether a %WIR system can be modified or must
             not be changed by some optimization or transformation.
    */
    bool mDontOptimize;

    /*!
      @brief mRegion2sections maps the ID of a memory region to a list of
             sections that a region contains.

      This list of sections is ordered by the section's start addresses within
      the region. Thus, such a list specifies the order in which the sections
      will be assembled into a memory region.
    */
    std::map<WIR_id_t, std::list<std::reference_wrapper<WIR_Section>>> mRegion2sections;

    /*!
      @brief For each memory region ID, mHighestAddresses provides an address
             map.

      This address map maps a section name to the highest address that a
      section with the given name occupies in the given memory region. This
      strange construct is required to assemble multiple sections with the same
      name (e.g., several .text sections of multiple cores) in a consecutive
      order into the same memory region.
    */
    std::map<WIR_id_t, std::map<std::string, WIR_MemoryAddress>> mHighestAddresses;

    /*!
      @brief mSymbolsOfSection realizes a kind of reversed symbol table that
             maps the ID of ELF sections to all symbols within that section.
    */
    std::map<WIR_id_t, WIR_SymbolSet> mSymbolsOfSection;

    /*!
      mWarnedRegions contains IDs of regions for which warnings have been
      emitted during memory layout computation in order to avoid duplicates.
    */
    std::set<WIR_id_t> mWarnedRegions;

};

}       // namespace WIR

#endif  // _WIR_SYSTEM_H
