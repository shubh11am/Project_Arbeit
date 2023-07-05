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
  @file tcschedulingpriority.h
  @brief This file provides the interface of a TriCore-specific class computing
         scheduling priorities for %WIR operations.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _TC_SCHEDULINGPRIORITY_H
#define _TC_SCHEDULINGPRIORITY_H


//
// Include section
//

// Include standard headers
#include <functional>
#include <list>

// Include WIR headers
#include <wir/wirtypes.h>
#include <optimizations/scheduling/wirschedulingpriority.h>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_Operation;
class WIR_SchedulingRegion;


/*!
  @brief Class TC_SchedulingPriority is a TriCore-specific class used to
         determine scheduling priorities for %WIR operations.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class TC_SchedulingPriority : virtual public WIR_SchedulingPriority
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC_SchedulingPriority( void );

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC_SchedulingPriority( const TC_SchedulingPriority & );

    /*!
      @brief Destructor.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~TC_SchedulingPriority( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC_SchedulingPriority & operator = ( const TC_SchedulingPriority & );


  protected:

    //
    // Scheduling priority handling.
    //

    /*!
      @brief setupCurrentCycle pre-computes some priority-relevant data about
             the operations of the current scheduling region.

      @param[in] cycle A signed long long value denoting the current execution
                       cycle to be considered.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setupCurrentCycle( long long );

    /*!
      @brief computePriority computes the scheduling priority for a given
             TriCore %WIR operation.

      @param[in] o A const reference to an operation whose scheduling priority
                   is computed.
      @param[in] pred A const pointer to a previously scheduled %WIR operation
                      that now is a predecessor for the operations to be
                      scheduled right now in the current execution cycle.
      @param[in] cycle A signed long long value denoting the current execution
                       cycle to be considered.
      @return A tuple consisting of the determined scheduling priority for
              operation o plus a list of operations to be scheduled together
              with o.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual std::pair<long long,
                      std::list<WIR_Operation *>> computePriority( const WIR_Operation &,
                                                                   const WIR_Operation *,
                                                                   long long );

    /*!
      @brief computePriority computes the scheduling priority for the given pair
             of TriCore operations.

      @param[in] o A const reference to an operation whose scheduling priority
                   is computed.
      @param[in] pred A const pointer to a previously scheduled %WIR operation
                      that now is a predecessor for the operations to be
                      scheduled right now in the current execution cycle.
      @return A signed long long value denoting o's scheduling priority.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    long long computePriority( const WIR_Operation &,
                               const WIR_Operation * ) const;

    /*!
      @brief computeBundlePriority computes the scheduling priority for a given
             bundle of LS-IP TriCore operations.

      @param[in] ip A const reference to an IP operation.
      @param[in] ls A const reference to a LS operation.
      @param[in] pred A pointer to a previously scheduled %WIR operation that
                      now is a predecessor for the operations to be scheduled
                      right now in the current execution cycle.
      @return A signed long long value denoting the bundle's combined scheduling
              priority.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    long long computeBundlePriority( const WIR_Operation &,
                                     const WIR_Operation &,
                                     const WIR_Operation * ) const;


    //
    // Attributes
    //

    /*!
      @brief mIPOperations holds all IP operations of the given scheduling
             region for a certain execution cycle.
    */
    std::list<std::reference_wrapper<WIR_Operation>> mIPOperations;

};

}       // namespace WIR

#endif  // _TC_SCHEDULINGPRIORITY_H
