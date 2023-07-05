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
  @file wirsystemcomponent.h
  @brief This file provides the interface of generic %WIR system components.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_SYSTEM_COMPONENT_H
#define _WIR_SYSTEM_COMPONENT_H


//
// Include section
//

// Include standard headers
#include <string>

// Include WIR headers
#include <wir/API/wiridapi.h>
#include <wir/API/wirinsertionapi.h>
#include <wir/wirtypes.h>
#include <analyses/generic/wirinterval.h>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_System;


/*!
  @brief Class WIR_SystemComponent is the generic representation of components
         building a %WIR system.

  This class serves as virtual base class from which actual system components
  (e.g., memories, caches, buses) are derived.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_SystemComponent : public WIR_ID_API
{

  public:

    //
    // Destructors.
    //

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_SystemComponent( void );


    //
    // System handling.
    //

    // Realize the API to manage a component's parent system.
    WIR_INSERTION_DECL( WIR_System, System );


    //
    // Name handling.
    //

    /*!
      @brief getName returns a system component's specific name.

      @return A string that holds the component's name.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::string getName( void ) const;


    //
    // Generic type handling.
    //

    /*!
      @brief getType returns the type of a system component, i.e., whether it is
             a cache, bus or memory region.

      @return The system component's type.

      Since types are characterised by actual system components that are
      defined by inheriting from this class, getType is purely virtual here.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_SystemComponentType getType( void ) const = 0;


    //
    // Address range handling.
    //

    /*!
      @brief getAddressRanges returns the set of address ranges for which this
             system component is active.

      @return A const reference to set mAddressRanges.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const WIR_AddressRangeSet &getAddressRanges( void ) const;

    /*!
      @brief getHullRange returns the convex hull over all address ranges for
             which this system component is active.

      @return An address range representing the convex hull over mAddressRanges.

      Obviously, getHullRange produces an address range that may contain
      addresses for which this system component is not active, since these
      addresses lie between other, active ranges.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const WIR_AddressRange getHullRange( void ) const;

    /*!
      @brief isActiveInRange returns whether this system component is active for
             the specified address range.

      @param[in] r A const reference to an address range.
      @return true if the system component is active for r, false otherwise.

      Derived classes may overwrite this method if they need to support
      different conditions for enabling system components other than the active
      address range assignment.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool isActiveInRange( const WIR_AddressRange & ) const;

    /*!
      @brief getMaxDelay returns the maximum number of clock cycles for which a
             system component can delay a memory access.

      @param[in] d The delay by which an access may be delayed by other system
                   components in the hierarchy that are behind this current
                   component. This is needed if this current component can
                   forward accesses to these deeper levels in the system
                   hierarchy (e.g., for a cache or bus).
      @return The maximal delay for a memory access as determined by this and
              all deeper components of the system hierarchy.

      Since delays are characterised by actual system components that are
      defined by inheriting from this class, getMaxDelay is purely virtual here.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual unsigned int getMaxDelay( const unsigned int d ) const = 0;


  protected:

    //
    // Constructors.
    //

    /*!
      @brief No standard construction allowed, users must use
             WIR_SystemComponent( const std::string &,
                                  const WIR_AddressRangeSet & )
             instead.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_SystemComponent( void ) = delete;

    /*!
      @brief Default constructor creating a named system component.

      @param[in] s A const reference to a string to be copied that holds the
                   component's name.

      This constructor asserts if it is passed an empty string.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_SystemComponent( const std::string & );

    /*!
      @brief Default constructor creating a named system component.

      @param[in] s An R-value reference to a string to be moved that holds the
                   component's name.

      This constructor asserts if it is passed an empty string.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_SystemComponent( std::string && );

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      When copying a component that is inserted in some %WIR system, the
      resulting copy will not be inserted in the system.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_SystemComponent( const WIR_SystemComponent & );

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      When moving a component that is inserted in some %WIR system, the moved
      component will not be inserted in the system.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_SystemComponent( WIR_SystemComponent && );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      When copying a component that is inserted in some %WIR system, the
      resulting copy will not be inserted in the system.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_SystemComponent & operator = ( const WIR_SystemComponent & );

    /*!
      @brief Move-assignment operator.

      @param[in] __o An R-value reference to another object to be moved.

      When moving a component that is inserted in some %WIR system, the moved
      component will not be inserted in the system.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_SystemComponent & operator = ( WIR_SystemComponent && );

    /*!
      @brief clone creates a copy of a %WIR system component.

      @return A pointer to the newly created copy of this component.

      Since the implementation details depend on some actual component's
      characteristics, clone is a pure virtual method.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_SystemComponent *clone( void ) const = 0;


    //
    // Name handling.
    //

    /*!
      @brief setName sets an system component's specific name.

      @param[in] s A const reference to a string to be copied that holds the
                   component's name.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setName( const std::string & );

    /*!
      @brief setName sets an system component's specific name.

      @param[in] s An R-value reference to a string to be moved that holds the
                   component's name.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setName( std::string && );


    //
    // Address range handling.
    //

    /*!
      @brief addAddressRange adds a range to the set of address ranges for which
             a system component is active.

      @param[in] r A const reference to a new address range.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void addAddressRange( const WIR_AddressRange & );


  private:

    //! mName holds an object's name.
    std::string mName;

    /*!
      @brief mAddressRanges stores the address ranges for which this memory
             hierarchy component is active.
    */
    WIR_AddressRangeSet mAddressRanges;

};

}       // namespace WIR

#endif  // _WIR_SYSTEM_COMPONENT_H
