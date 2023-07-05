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
  @file wirlocalscheduling.h
  @brief This file provides the interface of an optimization performing
         instruction scheduling locally within basic blocks.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_LOCALSCHEDULING_H
#define _WIR_LOCALSCHEDULING_H


//
// Include section
//

// Include standard headers
#include <list>
#include <map>
#include <memory>

// Include WIR headers
#include <wir/wirtypes.h>
#include <optimizations/generic/wiroptimization.h>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_BasicBlock;
class WIR_BlockSchedulingRegion;
class WIR_CompilationUnit;
class WIR_Function;
class WIR_SchedulingPriority;
class WIR_System;


/*!
  @brief Class WIR_LocalScheduling is an optimization that schedules
         instructions locally within %WIR basic blocks.

  The scheduler re-arranges individual %WIR operations and groups them to %WIR
  instructions such that all operations in one instruction shall be executed in
  parallel in the same execution cycle.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_LocalScheduling : public WIR_Optimization
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor for system-level optimization.

      @param[in] s A reference to a WIR_System to be optimized.
      @param[in] p A const reference to a scheduling priority object.
      @param[in] verbosity A Boolean denoting whether verbose messages shall be
                           dumped.
      @param[in] keepTmpFiles A Boolean denoting whether temporary files shall
                              be kept.

      This constructor copies the scheduling priority object and deletes this
      copy upon destruction.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_LocalScheduling( WIR_System &, const WIR_SchedulingPriority &,
                         bool = false, bool = false );

    /*!
      @brief Default constructor for compilation unit-level optimization.

      @param[in] c A reference to a WIR_CompilationUnit to be optimized.
      @param[in] p A const reference to a scheduling priority object.
      @param[in] verbosity A Boolean denoting whether verbose messages shall be
                           dumped.
      @param[in] keepTmpFiles A Boolean denoting whether temporary files shall
                              be kept.

      This constructor copies the scheduling priority object and deletes this
      copy upon destruction.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_LocalScheduling( WIR_CompilationUnit &, const WIR_SchedulingPriority &,
                         bool = false, bool = false );

    /*!
      @brief Default constructor for function-level optimization.

      @param[in] f A reference to a WIR_Function to be optimized.
      @param[in] p A const reference to a scheduling priority object.
      @param[in] verbosity A Boolean denoting whether verbose messages shall be
                           dumped.
      @param[in] keepTmpFiles A Boolean denoting whether temporary files shall
                              be kept.

      This constructor copies the scheduling priority object and deletes this
      copy upon destruction.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_LocalScheduling( WIR_Function &, const WIR_SchedulingPriority &,
                         bool = false, bool = false );

    /*!
      @brief Destructor.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_LocalScheduling( void );


    //
    // Scheduler configuration
    //

    /*!
      @brief setASAP configures the scheduler to use as-soon-as-possible (ASAP)
             scheduling.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setASAP( void );

    /*!
      @brief setALAP configures the scheduler to use as-late-as-possible (ALAP)
             scheduling.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setALAP( void );


  protected:

    /*!
      @brief runOptimization schedules instructions in the given system.

      @param[in] s A reference to a WIR_System to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void runOptimization( WIR_System & );

    /*!
      @brief runOptimization schedules instructions in the given compilation
             unit.

      @param[in] c A reference to a WIR_CompilationUnit to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void runOptimization( WIR_CompilationUnit & );

    /*!
      @brief runOptimization schedules instructions in the given function.

      @param[in] f A reference to a WIR_Function to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void runOptimization( WIR_Function & );

    /*!
      @brief init initializes internal data structures before the actual
             instruction scheduling.

      @param[in] f A reference to a WIR_Function to be scheduled.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void init( WIR_Function & );

    /*!
      @brief generateBlockRegion generates a new processor-specific block
             region for scheduling.

      @param[in] b A reference to a %WIR basic block for which a new scheduling
                   region is created.
      @return A pointer to the newly created block region.

      The pointer returned by generateBlockRegion will automatically be freed by
      the scheduler class WIR_LocalScheduling.

      Since block scheduling regions are processor-specific, this method is
      purely virtual and has to be overloaded by derived classes.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_BlockSchedulingRegion *generateBlockRegion( WIR_BasicBlock &b ) const = 0;


    //
    // List Scheduling
    //

    /*!
      @brief schedule performs list scheduling for a given scheduling region.

      @param[in,out] r A reference to a scheduling region to be scheduled.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void schedule( WIR_BlockSchedulingRegion & );

    /*!
      @brief removeEmptyInstructions removes all instructions from a given
             scheduling region that do not contain any operation.

      @param[in,out] r A reference to a scheduling region to be scheduled.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void removeEmptyInstructions( WIR_BlockSchedulingRegion & );


    //
    // Attributes
    //

    //! mPriority is a managed pointer to a scheduling priority object.
    std::unique_ptr<WIR_SchedulingPriority> mPriority;

    //! mRegion is the list of all regions to be scheduled.
    std::list<std::unique_ptr<WIR_BlockSchedulingRegion>> mRegions;

    /*!
      @brief mReadyList points to the list scheduler's ready list.

      The ready list is implemented as a map that maps a %WIR operation's ID to
      its corresponding execution cycle.
    */
    std::map<WIR_id_t, long long> *mReadyList;

    /*!
      @brief mASAP stores whether as-soon-as-possible (ASAP) scheduling shall be
             performed (true) or as-late-as-possible (ALAP) scheduling (false).
    */
    bool mASAP;

    //! mVerbosity stores whether verbose messages should be dumped.
    bool mVerbosity;

    //! mKeepTmpFiles stores whether temporary files should be kept.
    bool mKeepTmpFiles;

};

}       // namespace WIR

#endif  // _WIR_LOCALSCHEDULING_H
