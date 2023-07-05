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
  @file wirschedulingpriority.h
  @brief This file provides the interface of an abstract base class computing
         scheduling priorities for %WIR operations.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_SCHEDULINGPRIORITY_H
#define _WIR_SCHEDULINGPRIORITY_H


//
// Include section
//

// Include standard headers
#include <functional>
#include <list>
#include <map>
#include <utility>

// Include WIR headers
#include <wir/wirtypes.h>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_Instruction;
class WIR_Operation;
class WIR_SchedulingRegion;


/*!
  @brief Class WIR_SchedulingPriority is a base class used to determine
         scheduling priorities for %WIR operations.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_SchedulingPriority
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_SchedulingPriority( void );

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_SchedulingPriority( const WIR_SchedulingPriority & );

    /*!
      @brief Destructor.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_SchedulingPriority( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_SchedulingPriority & operator = ( const WIR_SchedulingPriority & );


    //
    // Local type definitions.
    //

    //! invalid denotes a special, invalid scheduling priority.
    static const long long invalid;

    //! penalty denotes a special, penalty scheduling priority.
    static const long long penalty;

    //! skip denotes a special, skip scheduling priority.
    static const long long skip;

    //! valid denotes a special, valid scheduling priority.
    static const long long valid;


    //
    // Scheduling priority handling.
    //

    /*!
      @brief setRegion sets the scheduling region for which priorities will be
             computed.

      @param[in] r A reference to a scheduling region.
      @param[in] asap A Boolean flag defaulting to true that denotes whether
                      as-soon-as-possible (true) or as-late-as-possible (false)
                      scheduling is performed.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setRegion( WIR_SchedulingRegion &, bool = true );

    /*!
      @brief For a given execution cycle, getBestOperations returns the sequence
             of operations of a region having the best scheduling priority.

      @param[in] cycle A signed long long value denoting the current execution
                       cycle to be considered.
      @param[in] prev A pointer to a %WIR instruction previously generated
                      during scheduling.
      @return A list of pointers to operations of a region to be scheduled in
              exactly the list order. This list could also include null pointers
              which indicates that execution slots for a superscalar
              architecture shall be left free.

      getBestOperations fails with an assertion if setRegion was not invoked
      before.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<WIR_Operation *> getBestOperations( long long,
                                                  WIR_Instruction * );


  protected:

    friend class WIR_LocalScheduling;

    /*!
      @brief setupCurrentCycle pre-computes some priority-relevant data about
             the operations of the current scheduling region.

      @param[in] cycle A signed long long value denoting the current execution
                       cycle to be considered.

      Since this task is processor-specific and might or might not be necessary
      for some actual processor, this method is virtual and can be overloaded if
      required.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void setupCurrentCycle( long long );

    /*!
      @brief computePriority computes the scheduling priority for a given %WIR
             operation.

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

      Since the computation of an operation's actual scheduling priority depends
      on the used priority heuritics and might or might not be
      processor-specific, this method is purely virtual.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual std::pair<long long,
                      std::list<WIR_Operation *>> computePriority( const WIR_Operation &o,
                                                                   const WIR_Operation *pred,
                                                                   long long cycle ) = 0;

    /*!
      @brief getPriority determines an operation's scheduling priority based on
             some actual scheduling priority.

      @param[in] o A const reference to an operation whose scheduling priority
                   is computed.
      @return A signed long long value denoting the operation's scheduling
              priority.

      Since the computation of an operation's actual scheduling priority depends
      on the used priority heuristics, this method is purely virtual.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual long long getPriority( const WIR_Operation &o ) const = 0;

    /*!
      @brief clone creates a copy of a %WIR scheduling priority.

      @return A pointer to the newly created copy of a priority object.

      Since the implementation details depend on characteristics of derived
      classes, clone is a purely virtual method.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_SchedulingPriority *clone( void ) const = 0;


    //
    // Attributes
    //

    /*!
      @brief mRegion points to the scheduling region considered during priority
             computation.
    */
    WIR_SchedulingRegion *mRegion;

    /*!
      @brief mEarliestCycleMap points to a map providing all %WIR operations
             to be scheduled into one earliest execution cycle.
    */
    std::map<long long, WIR_OperationSet> *mEarliestCycleMap;

    /*!
      @brief mMaxDelayMap points to a map providing the maximum delay for each
             %WIR operation of a region.
    */
    std::map<WIR_id_t, unsigned long long> *mMaxDelayMap;

    /*!
      @brief mMobility points to a map providing the mobility of %WIR
             operations, i.e., the difference between the operation's latest and
             earliest execution cycle.
    */
    std::map<WIR_id_t, long long> *mMobilityMap;

};

}       // namespace WIR

#endif  // _WIR_SCHEDULINGPRIORITY_H
