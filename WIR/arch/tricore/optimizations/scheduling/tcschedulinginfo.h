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
  @file tcschedulinginfo.h
  @brief This file provides TriCore-specific helper functions for instruction
         scheduling.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _TC_SCHEDULING_INFO_H
#define _TC_SCHEDULING_INFO_H


//
// Include section
//

// Include standard headers
#include <map>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_BaseProcessor;
class WIR_Operation;


/*!
  @brief Class TC_SchedulingInfo contains TriCore-specific static methods and
         constant tables required for instruction scheduling.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class TC_SchedulingInfo
{

  public:

    //
    // Instruction Set Classification.
    //

    /*!
      @brief This enum represents different types of TriCore operations.

      See also
      - TriCore Compiler Writer's Guide, section 2.1.1.1, page 31.
      - TriCore User Guide V1.6.4, DSP Optimization Guide Part 1, 2003.
      - TriCore User's Manual V1.3.8, Volume 2 Instruction Set, 2008, chapter 4,
        pages 507ff.
      - Thomas Pucyk, Local and global instruction scheduling approaches for the
        TriCore processor, diploma thesis, TU Dortmund, 2009, section 2.1.3.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    enum class OperationType : char
    {
      //! Integer pipeline operation.
      ip,

      //! Load/store pipeline operation.
      ls,

      //! Dual-pipeline operation.
      dp,

      //! MAC operation.
      mac,

      //! Loop pipeline operation.
      lp,

      //! Floating-point operation.
      fp
    };

    /*!
      @brief getType determines the TriCore-specific type of a %WIR operation.

      @param[in] o A const reference to a TriCore operation.
      @return The operation's type.

      See also
      - TriCore Compiler Writer's Guide, section 2.1.1.1, page 31.
      - TriCore User Guide V1.6.4, DSP Optimization Guide Part 1, 2003.
      - TriCore User's Manual V1.3.8, Volume 2 Instruction Set, 2008, chapter 4,
        pages 507ff.
      - Thomas Pucyk, Local and global instruction scheduling approaches for the
        TriCore processor, diploma thesis, TU Dortmund, 2009, section 2.1.3.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    static OperationType getType( const WIR_Operation & );

    /*!
      @brief getLatency determines the number of clock cycles it takes from
             issuing the first operation until availability of its results,
             depending on the second operation which is issued immediately after
             the first one.

      @param[in] o1 A const reference to a first %WIR operation.
      @param[in] o2 A const reference to another %WIR operation that is
                    immediately issued after the first one.
      @return The latency of o1 in clock cycles.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    static long long getLatency( const WIR_Operation &,
                                 const WIR_Operation & );

    /*!
      @brief getLatency determines the number of clock cycles it takes from
             issuing the given operation until availability of its results,
             assuming that the operation is the last one within a basic block.

      @param[in] o A const reference to a %WIR operation.
      @return The latency of o in clock cycles.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    static long long getLatency( const WIR_Operation & );

    /*!
      @brief getPriority determines an operation's scheduling priority depending
             on another predecessor operation.

      @param[in] o A const reference to a %WIR operation.
      @param[in] pred A const reference to a predecessor operation.
      @return The scheduling priority of o.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    static long long getPriority( const WIR_Operation &,
                                  const WIR_Operation & );

    /*!
      @brief getPriority determines the scheduling priority of a stand-alone
             operation.

      @param[in] o A const reference to a %WIR operation.
      @return The scheduling priority of o.

      An operation is considered stand-alone if it has no predecessor in the
      current schedule, i.e., it if is an entry of a scheduling region.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    static long long getPriority( const WIR_Operation & );


  private:

    /*!
      @brief initOperationTypes initializes map mOperationTypes.

      See the comment for attribute mOperationTypesInitialized for more
      information.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    static void initOperationTypes( void );

    /*!
      @brief initFloatLatency initializes map mFloatLatency.

      See the comment for attribute mFloatLatencyInitialized for more
      information.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    static void initFloatLatency( void );


    //
    // Attributes
    //

    //! mOperationTypes maps TriCore opcodes to their operation types.
    static std::map<WIR_BaseProcessor::OpCode, OperationType> mOperationTypes;

    /*!
      @brief mOperationTypesInitialized stores whether map mOperationTypes
             above has already been initialized or not.

      It is unfortunately not possible to pre-initialize the map with const
      data, but the initialization has to be done explicitly during execution
      time. The reason is that the map has static linkage, and the map's keys,
      i.e., the opcodes, too. Since, according to the C++ standard, the order in
      which objects of static linkage are actually initialized, is not defined,
      it could be that the map gets initialized (with nonsense stuff), before
      the opcodes are properly setup. In order to avoid this and to ensure that
      the map gets initialized after the opcodes, explicit init code for the
      map is used here.
    */
    static bool mOperationTypesInitialized;

    /*!
      @brief mFloatLatency maps TriCore floating-point opcodes to their
             latencies in clock cycles.
    */
    static std::map<WIR_BaseProcessor::OpCode, long long> mFloatLatency;

    /*!
      @brief mFloatLatencyInitialized stores whether map mFloatLatency
             above has already been initialized or not.

      It is unfortunately not possible to pre-initialize the map with const
      data, but the initialization has to be done explicitly during execution
      time. The reason is that the map has static linkage, and the map's keys,
      i.e., the opcodes, too. Since, according to the C++ standard, the order in
      which objects of static linkage are actually initialized, is not defined,
      it could be that the map gets initialized (with nonsense stuff), before
      the opcodes are properly setup. In order to avoid this and to ensure that
      the map gets initialized after the opcodes, explicit init code for the
      map is used here.
    */
    static bool mFloatLatencyInitialized;

    /*!
      @brief mTypeLatency maps the types of two succeeding operations to the
             number of clock cycles it takes from issuing the first operation
             until availability of its results, depending on the type of the
             operation issued immediately after the first one.

      Multi-cycle operations are considered as single-cycle here, since they
      block issuing any other following operation.

      Latencies of floating-point operations are managed in table mFloatLatency.
    */
    static const std::map<OperationType,
                          std::map<OperationType, long long>> mTypeLatency;

    /*!
      @brief mPairPriority maps the types of two succeeding operations to their
             respective scheduling priority.

      Rows denote preceding operation types, columns the currently scheduled
      operation.
    */
    static const std::map<OperationType,
                          std::map<OperationType, long long>> mPairPriority;

    /*!
      @brief mSinglePriority maps the types of operations having no predecessor
             to their respective scheduling priority.

      Operations without previously scheduled predecessors are those that are
      entries of a scheduling region.
    */
    static const std::map<OperationType, long long> mSinglePriority;

};

}       // namespace WIR

#endif  // _TC_SCHEDULING_INFO_H
