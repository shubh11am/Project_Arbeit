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
  @file wirfpbusarbitration.h
  @brief This file provides the basic interface of a fixed priority %WIR bus
         arbitration policy.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_FP_BUS_ARBITRATION_H
#define _WIR_FP_BUS_ARBITRATION_H


//
// Include section
//

// Include standard headers
#include <map>
#include <vector>

// Include WIR headers
#include <wir/wirbusarbitration.h>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_BaseProcessor;


/*!
  @brief Class WIR_FixedPriorityBusArbitration represents parameters for a fixed
         priority bus arbitration policy.

  Fixed priority arbitration assigns an integer priority to each core. Bus
  requests are never interrupted, though. Therefore, any request may be delayed
  by (maximumAccessTime-1) cycles by lower-priority requests.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_FixedPriorityBusArbitration : public virtual WIR_BusArbitration
{

  public:

    //
    // Local type definitions.
    //

    //! Priorities are represented as unsigned integers.
    using Priority = unsigned int;


    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor.

      @param[in] n An unsigned integer denoting the number of processor cores
                   that are managed by this fixed-priority bus arbiter.

      This constructor does not assign any priority to the cores. This has to be
      done explicitly using setPriority().

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_FixedPriorityBusArbitration( unsigned int );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_FixedPriorityBusArbitration( void );

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


    //
    // Priority handling.
    //

    /*!
      @brief getPriority returns a core's arbitration priority.

      @param[in] c A const reference to a processor core.
      @return The processor's priority value.

      Priority values are allowed from the interval [0, mNumberOfCores], a
      numerically high value denotes a high priority. A priority of 0 indicates
      that the given core is not considered for arbitration.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    Priority getPriority( const WIR_BaseProcessor & ) const;

    /*!
      @brief getPriorities returns all cores' priorities for this bus.

      @return A const reference to a map translating the cores' IDs to their
              priorities.

      Numerically larger values denote higher priorities. If the system features
      n cores, then the priority values must be unique for each core, and they
      must be from the range [1, n]. A priority of 0 indicates that the given
      core is not considered for arbitration.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const std::map<WIR_id_t, Priority> &getPriorities( void ) const;

    /*!
      @brief getNumberOfCores returns the overall number of cores for this fixed
             priority arbiter.

      @return The value of mNumberOfCores.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    unsigned int getNumberOfCores( void ) const;

    /*!
      @brief getNumberOfArbitratedCores returns the number of cores with
             non-zero priority.

      @return The number of cores managed by this bus arbiter.

      Cores with priority 0 are not considered for arbitration. This scenario
      only makes sense if fixed priority arbitration is combined with some other
      arbitration policy like, e.g., priority division.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual unsigned int getNumberOfArbitratedCores( void ) const;

    /*!
      @brief getCoreOrder returns the vector of cores managed by this
             arbitration policy, ordered by priority (highest priority first).

      @return A const reference to the vector mCoreOrder.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const std::vector<WIR_id_t> &getCoreOrder( void ) const;


  protected:

    friend class WIR_System;

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_FixedPriorityBusArbitration( const WIR_FixedPriorityBusArbitration &__o ) = default;

    /*!
      @brief clone creates a copy of a fixed priority bus arbiter.

      @return A pointer to the newly created copy of this fixed priority
              arbiter.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_FixedPriorityBusArbitration *clone( void ) const;


    //
    // Priority handling.
    //

    /*!
      @brief setPriority assigns an arbitration priority to a core.

      @param[in] c A const reference to a processor core.
      @param[in] p A priority value.

      Priority values are allowed from the interval [0, mNumberOfCores], a
      numerically high value denotes a high priority. A priority of 0 indicates
      that the given core is not considered for arbitration.

      setPriority asserts if the given priority value is already used for some
      other core.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setPriority( const WIR_BaseProcessor &, Priority );


  private:

    /*!
      @brief No standard construction allowed, users must use
             WIR_FixedPriorityBusArbitration( unsigned int ) instead.
    */
    WIR_FixedPriorityBusArbitration( void ) = delete;

    /*!
      @brief mNumberOfCores stores the number of processor cores that are
             managed by this fixed-priority bus arbiter.
    */
    unsigned int mNumberOfCores;

    //! mPriorities maps the ID of a processor core to its priority.
    std::map<WIR_id_t, Priority> mPriorities;

    /*!
      @brief mCoreOrder stores the cores managed by this arbitration policy,
             ordered by priority.

      High-priority cores appear at the front of mCoreOrder, low-priority cores
      at the tail.
    */
    mutable std::vector<WIR_id_t> mCoreOrder;

    //! mSort specifies whether vector mCoreOrder needs to be sorted or not.
    mutable bool mSort;

};

}       // namespace WIR

#endif  // _WIR_FP_BUS_ARBITRATION_H
