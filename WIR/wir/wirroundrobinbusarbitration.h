/*

   This header file belongs to the

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
  @file wirroundrobinbusarbitration.h
  @brief This file provides the basic interface of a round robin %WIR bus
         arbitration policy.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_ROUND_ROBIN_BUS_ARBITRATION_H
#define _WIR_ROUND_ROBIN_BUS_ARBITRATION_H


//
// Include section
//

// Include WIR headers
#include <wir/wirbusarbitration.h>


//
// Header section
//

namespace WIR {

/*!
  @brief Class WIR_RoundRobinBusArbitration represents parameters for a round
         robin bus arbitration policy.

  Round robin arbitration means that a modulo counter is used that iterates over
  the cores and grants the bus to the next core that is requesting the bus.

  Actually, there is nothing to configure for round robin arbitration. :-)

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_RoundRobinBusArbitration : public virtual WIR_BusArbitration
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_RoundRobinBusArbitration( void ) = default;

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_RoundRobinBusArbitration( void );

    /*!
      @brief This operator compares two WIR_BusArbitrations for equality.

      @param[in] __o A const reference to another object to be compared.
      @return true iff both operands are equal, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool operator == ( const WIR_BusArbitration & ) const;

    /*!
      @brief This operator compares two WIR_BusArbitrations for inequality.

      @param[in] __o A const reference to another object to be compared.
      @return true iff both operands are inequal, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool operator != ( const WIR_BusArbitration & ) const;


  protected:

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_RoundRobinBusArbitration( const WIR_RoundRobinBusArbitration &__o ) = default;

    /*!
      @brief clone creates a copy of a round robin bus arbiter.

      @return A pointer to the newly created copy of this round robin arbiter.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_RoundRobinBusArbitration *clone( void ) const;

};

}       // namespace WIR

#endif  // _WIR_ROUND_ROBIN_BUS_ARBITRATION_H
