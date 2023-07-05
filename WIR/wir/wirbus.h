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
  @file wirbus.h
  @brief This file provides the interface of %WIR buses.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_BUS_H
#define _WIR_BUS_H


//
// Include section
//

// Include standard headers
#include <memory>

// Include WIR headers
#include <wir/wirsystemcomponent.h>
#include <wir/wirtypes.h>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_BusArbitration;


/*!
  @brief Class WIR_Bus represents buses of some system architecture.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_Bus final : public WIR_SystemComponent
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
    WIR_Bus( const WIR_Bus & );

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Bus( WIR_Bus && );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_Bus( void );


    //
    // Generic type handling.
    //

    /*!
      @brief getType returns the type of a system component, i.e., that it is a
             bus.

      @return WIR_SystemComponentType::bus

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_SystemComponentType getType( void ) const;


    //
    // Bus properties.
    //

    /*!
      @brief getBusArbitration returns a buses' arbitration configuration.

      @return A const reference to the bus arbiter's configuration.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const WIR_BusArbitration &getBusArbitration( void ) const;

    /*!
      @brief getArbitrationType returns a buses' arbitration strategy.

      @return A const reference to the bus arbiter.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_BusArbitrationType getArbitrationType( void ) const;

    /*!
      @brief getMaxDelay returns the maximum number of clock cycles for which
             a bus can delay a memory access.

      @param[in] d The delay by which an access may be delayed by other
                   components in a system's hierarchy that are behind this
                   current component.
      @return The maximal delay for an access to this system component including
              the delay caused by other system components behind this one.

      @note Since the current implementation does not support hierarchies with
            multiple levels of buses, getMaxDelay simply returns @a d.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual unsigned int getMaxDelay( const unsigned int d = 0 ) const;

    /*!
      @brief getMaxAccessTime returns the maximum access time for a single
             memory access via this bus.

      @return The buses maximum memory access time.

      @note This is NOT the arbitration time, only the pure memory module access
            time. Arbitration policies can use this value to determine whether
            an incoming request can still complete in a give time frame.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    unsigned int getMaxAccessTime( void ) const;


  private:

    friend class WIR_System;


    //
    // Constructors.
    //

    /*!
      @brief No standard construction allowed, users must use
             WIR_Bus( std::string &&, const WIR_BusArbitration & ) instead.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Bus( void ) = delete;

    /*!
      @brief Default constructor.

      @param[in] s An R-value reference to a string to be moved that holds the
                   buses name.
      @param[in] a A const reference to a bus arbitration instance.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Bus( std::string &&, const WIR_BusArbitration & );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Bus & operator = ( const WIR_Bus & );

    /*!
      @brief Move-assignment operator.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Bus & operator = ( WIR_Bus && );

    /*!
      @brief clone creates a copy of a %WIR bus.

      @return A pointer to the newly created copy of this bus.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_Bus *clone( void ) const;

    /*!
      @brief setMaxAccessTime sets the maximum access time for a single memory
             access via this bus.

      @param[in] t The buses maximum memory access time.

      @note This is NOT the arbitration time, only the pure memory module access
            time.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setMaxAccessTime( unsigned int );

    //! mBusArbitration stores the configuration of the buses arbiter.
    std::unique_ptr<WIR_BusArbitration> mBusArbitration;

    //! mArbitrationType stores the arbitration strategy of this %WIR bus.
    WIR_BusArbitrationType mArbitrationType;

    /*!
      @brief mMaxAccessTime stores the maximum access time for a single memory
             access.
    */
    unsigned int mMaxAccessTime;

};

}       // namespace WIR

#endif  // _WIR_BUS_H
