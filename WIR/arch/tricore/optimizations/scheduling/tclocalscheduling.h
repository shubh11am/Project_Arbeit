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
  @file tclocalscheduling.h
  @brief This file provides the interface of a TriCore-specific optimization
         performing instruction scheduling locally within basic blocks.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _TC_LOCALSCHEDULING_H
#define _TC_LOCALSCHEDULING_H


//
// Include section
//

// Include WIR headers
#include <optimizations/scheduling/wirlocalscheduling.h>
#include <arch/tricore/optimizations/scheduling/tcmaxdelaypriority.h>
#include <arch/tricore/optimizations/scheduling/tcmobilitypriority.h>
#include <arch/tricore/optimizations/scheduling/tcnoofsuccspriority.h>


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
  @brief Class TC_LocalScheduling is an optimization for the TriCore
         architecture that schedules instructions locally within %WIR basic
         blocks.

  The scheduler re-arranges individual %WIR operations and groups them to %WIR
  instructions such that all operations in one instruction shall be executed in
  parallel in the same execution cycle.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class TC_LocalScheduling : public WIR_LocalScheduling
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
    TC_LocalScheduling( WIR_System &, const WIR_SchedulingPriority &,
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
    TC_LocalScheduling( WIR_CompilationUnit &, const WIR_SchedulingPriority &,
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
    TC_LocalScheduling( WIR_Function &, const WIR_SchedulingPriority &,
                        bool = false, bool = false );

    /*!
      @brief Destructor.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~TC_LocalScheduling( void );


  protected:

    /*!
      @brief generateBlockRegion generates a new TriCore block region for
             scheduling.

      @param[in] b A reference to a %WIR basic block for which a new scheduling
                   region is created.
      @return A pointer to the newly created TriCore block region.

      The pointer returned by generateBlockRegion will automatically be freed by
      the scheduler class TC_LocalScheduling.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_BlockSchedulingRegion *generateBlockRegion( WIR_BasicBlock & ) const;

};

}       // namespace WIR

#endif  // _TC_LOCALSCHEDULING_H
