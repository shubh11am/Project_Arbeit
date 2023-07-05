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
  @file wirschedulingregion.h
  @brief This file provides the interface of an abstract base class representing
         regions in which scheduling is performed.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_SCHEDULINGREGION_H
#define _WIR_SCHEDULINGREGION_H


//
// Include section
//

// Include standard headers
#include <fstream>
#include <functional>
#include <list>
#include <map>
#include <set>

// Include WIR headers
#include <wir/wirtypes.h>
#include <optimizations/scheduling/wirdgedgeproperty.h>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_BaseRegister;
class WIR_BasicBlock;
class WIR_Instruction;
class WIR_Operation;


/*!
  @brief Class WIR_SchedulingRegion is an abstract base class representing
         arbitrary scheduling regions.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_SchedulingRegion
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor.

      @param[in] b A reference to a %WIR basic block that is the scheduling
                   region's start block.
      @param[in] verbosity A Boolean defaulting to false that denotes whether
                           verbose messages shall be dumped.
      @param[in] keepTmpFiles A Boolean defaulting to false that denotes whether
                              temporary files shall be kept.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_SchedulingRegion( WIR_BasicBlock &, bool = false, bool = false );

    /*!
      @brief Destructor.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_SchedulingRegion( void );


    //
    // Dependence graph handling
    //

    /*!
      @brief buildDG builds a region's dependence graph.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void buildDG( void );


    //
    // General region properties.
    //

    /*!
      @brief getBasicBlocks returns the list of basic blocks of a scheduling
             region.

      @return A const reference to list mBasicBlocks.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const std::list<WIR_BasicBlock *> &getBasicBlocks( void ) const;

    /*!
      @brief getOperationCount returns the number of %WIR operations within the
             current scheduling region.

      @return An unsigned integer value denoting the total operation count.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    unsigned int getOperationCount( void ) const;

    /*!
      @brief getNumberOfSuccessors determines the number of successors of a %WIR
             operation in the dependence graph.

      @param[in] o A const reference to an operation whose successor count is
                   computed.
      @return An unsigned integer denoting the given operation's number of
              successors.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    unsigned int getNumberOfSuccessors( const WIR_Operation & ) const;

    /*!
      @brief getUnscheduledPredecessors computes the set of unscheduled
             predecessor nodes of an operation in the dependence graph.

      @param[in] o A const reference to an operation whose unscheduled
                   predecessors are computed.
      @return A set of %WIR operations containing all unscheduled predecessor
              operations of o.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_OperationSet getUnscheduledPredecessors( const WIR_Operation & ) const;

    /*!
      @brief visualize dumps the current region's dependence graph into a DOT
             file and invokes xdot on it.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void visualize( void ) const;


    //
    // Latency and Cycle handling
    //

    /*!
      @brief getEarliestCycleOpsMap returns the map providing all %WIR
             operations to be scheduled into one earliest execution cycle.

      @return A reference to map mEarliestCycleOps.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::map<long long, WIR_OperationSet> &getEarliestCycleOpsMap( void );

    /*!
      @brief getEarliestCycleMap returns the map providing the earliest
             execution cycle for each %WIR operation of a region.

      @return A reference to map mEarliestCycle.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::map<WIR_id_t, long long> &getEarliestCycleMap( void );

    /*!
      @brief getLatestCycleOpsMap returns the map providing all %WIR operations
             to be scheduled into one latest execution cycle.

      @return A reference to map mLatestCycleOps.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::map<long long, WIR_OperationSet> &getLatestCycleOpsMap( void );

    /*!
      @brief getLatestCycleMap returns the map providing the latest execution
             cycle for each %WIR operation of a region.

      @return A reference to map mLatestCycle.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::map<WIR_id_t, long long> &getLatestCycleMap( void );

    /*!
      @brief getMaxDelayMap returns the map providing the maximum delay for each
             %WIR operation of a region.

      @return A reference to map mMaxDelay.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::map<WIR_id_t, unsigned long long> &getMaxDelayMap( void );

    /*!
      @brief getMobilityMap returns the map providing the mobility for each %WIR
             operation of a region, i.e., to the difference between the
             operation's latest and earliest execution cycle.

      @return A reference to map mMobility.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::map<WIR_id_t, long long> &getMobilityMap( void );

    /*!
      @brief updateCycleMaps updates a region's earliest and latest cycles maps
             after having scheduled a bundle of operations within the region.

      @param[in] b A const reference to a list (bundle) of %WIR operation
                   pointers currently scheduled.
      @param[in] currentCycle A signed long long value denoting the scheduler's
                              current execution cycle.
      @return A signed long long value denoting the additional latency resulting
              from the scheduled operation bundle.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    long long updateCycleMaps( const std::list<WIR_Operation *> &, long long );


    //
    // Code reordering.
    //

    /*!
      @brief moveOperations moves a bundle of operations to a new position
             after a given instruction within a scheduling region.

      @param[in,out] b A reference to a list (bundle) of %WIR operation pointers
                       to be moved.
      @param[in] pred A const pointer to a %WIR instruction after which the
                      bundle's operations will be moved. If pred is the nullptr,
                      the bundle's operations will be moved to the very
                      beginning of the region.
      @return A pointer to a newly generated %WIR instruction containing the
              given bundle.

      All operations of a bundle are assumed to be executed in the same
      execution cycle, i.e., in parallel. Thus, moveOperations groups them
      altogether into a single, novel %WIR instruction.

      Since moving of operations might depend on specific region types or might
      be processor-specific, this method is purely virtual.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_Instruction *moveOperations( std::list<WIR_Operation *> &b,
                                             const WIR_Instruction *pred ) = 0;

    /*!
      @brief updateMovedOperations updates a region's internal data structures
             after having moved the operations of a given bundle.

      @param[in] b A const reference to a list (bundle) of %WIR operation
                   pointers.

      While moving operations of a bundle to new instructions, the pointers to
      the involved operations do change. updateMovedOperations updates all
      internal data structures of a scheduling regions such that the correct
      pointers given in the specified bundle are stored.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void updateMovedOperations( std::list<WIR_Operation *> & );

    /*!
      @brief postProcessingHook allows to perform processor-specific actions
             after having done instruction scheduling for a region.

      Since these actions are processor-specific and might or might not be
      necessary for some actual processor architecture, this method is virtual
      and can be overloaded if required.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void postProcessingHook( void );


  protected:

    //
    // Local type definitions.
    //

    /*!
      @brief DGraph represents the internal boost structure of the dependence
             graph.
    */
    using DGraph = WIR_DirMGraph<WIR_Operation *, WIR_DGEdgeProperty>;

    //! DGraphVertex represents a dependence graph vertex.
    using DGraphVertex =
      WIR_DirMGraphVertex<WIR_Operation *, WIR_DGEdgeProperty>;

    //! DGraphEdge represents a dependence graph edge.
    using DGraphEdge = WIR_DirMGraphEdge<WIR_Operation *, WIR_DGEdgeProperty>;


    //
    // Dependence graph handling
    //

    /*!
      @brief addDependences checks for RAW, WAR, WAW and other dependences
             between two operations and adds them as edges to the dependence
             graph.

      @param[in] o1 A const reference to a first %WIR operation.
      @param[in] o2 A const reference to a second %WIR operation.

      Since dependences might depend on specific region types or might be
      processor-specific, this method is virtual so that it can be overloaded
      by derived classes.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void addDependences( const WIR_Operation &,
                                 const WIR_Operation & );

    /*!
      @brief checkRAW checks whether there is a classical data dependence
             (read-after-write) between an operation and some register
             parameter.

      @param[in] o1 A const reference to a %WIR operation.
      @param[in] rp2 A const reference to a %WIR register parameter.
      @return true if o1 defines a register used by rp2, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool checkRAW( const WIR_Operation &, const WIR_RegisterParameter & ) const;

    /*!
      @brief checkWAR checks whether there is an anti dependence
             (write-after-read) between an operation and some register
             parameter.

      @param[in] o1 A const reference to a %WIR operation.
      @param[in] rp2 A const reference to a %WIR register parameter.
      @return true if o1 uses a register defined by rp2, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool checkWAR( const WIR_Operation &, const WIR_RegisterParameter & ) const;

    /*!
      @brief checkWAW checks whether there is an output dependence
             (write-after-write) between an operation and some register
             parameter.

      @param[in] o1 A const reference to a %WIR operation.
      @param[in] rp2 A const reference to a %WIR register parameter.
      @return true if o1 defines a register defined by rp2, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool checkWAW( const WIR_Operation &, const WIR_RegisterParameter & ) const;

    /*!
      @brief checkUCert checks whether there is an uncertain dependence
             between an operation and some register parameter.

      @param[in] o1 A const reference to a %WIR operation.
      @param[in] rp2 A const reference to a %WIR register parameter.
      @return true if there is an uncertain dependence between o1 and rp2, false
              otherwise.

      Since this kind of dependences is heavily processor-specific, this method
      is purely virtual and has to be overloaded by derived classes.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool checkUCert( const WIR_Operation &o1,
                             const WIR_RegisterParameter &rp2 ) const = 0;

    /*!
      @brief checkCtrl checks whether there is a control dependence between an
             operation and some register parameter.

      @param[in] o1 A const reference to a %WIR operation.
      @param[in] rp2 A const reference to a %WIR register parameter.
      @return true if there is a control dependence between o1 and rp2, false
              otherwise.

      Since this kind of dependences is heavily processor-specific, this method
      is purely virtual and has to be overloaded by derived classes.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool checkCtrl( const WIR_Operation &o1,
                            const WIR_RegisterParameter &rp2 ) const = 0;

    /*!
      @brief eraseRedundantEdges removes redundant edges from a region's
             dependence graph.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void eraseRedundantEdges( void );


    //
    // Latency and Cycle handling
    //

    /*!
      @brief For each edge in the dependence graph, computeLatencies determines
             the latency between the involved, dependent operations.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void computeLatencies( void );

    /*!
      @brief computeLatency computes the latency between two dependent %WIR
             operations.

      @param[in] o1 A const reference to a first %WIR operation.
      @param[in] o2 A const reference to a second %WIR operation.
      @param[in] t A specifier denoting the type of dependence between both
                   operations.
      @return A signed long long value denoting the latency between the two
              given operations in clock cycles.

      Since the computation of operation latencies is heavily
      processor-specific, this method is purely virtual and has to be overloaded
      by derived classes.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual long long computeLatency( const WIR_Operation &o1,
                                      const WIR_Operation &o2,
                                      WIR_DGEdgeType t ) = 0;

    /*!
      @brief getLatency determines the number of clock cycles it takes from
             issuing the given operation until availability of its results.

      @param[in] o A const reference to a %WIR operation.
      @return The latency of o in clock cycles.

      Since the computation of operation latencies is heavily
      processor-specific, this method is purely virtual and has to be overloaded
      by derived classes.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual long long getLatency( const WIR_Operation &o ) = 0;

    /*!
      @brief computeExecutionCycles computes the earliest and latest execution
             cycles for all operations of a region.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void computeExecutionCycles( void );

    /*!
      @brief setEarliestCycle sets the earliest execution cycle of an operation.

      @param[in] o A const reference to a %WIR operation.
      @param[in] c A long long value denoting the execution cycle to be set.
      @param[in,out] cycles A reference to a cycles map where to set the
                            operation's earliest execution cycle.

      When setting an operation's earliest execution cycle, this change is also
      propagated recursively to all successors in the region's dependence graph.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setEarliestCycle( const WIR_Operation &, long long,
                           std::map<WIR_id_t, long long> & );

    /*!
      @brief setLatestCycle sets the latest execution cycle of an operation.

      @param[in] o A const reference to a %WIR operation.
      @param[in] c A long long value denoting the execution cycle to be set.
      @param[in,out] cycles A reference to a cycles map where to set the
                            operation's latest execution cycle.

      When setting an operation's latest execution cycle, this change is also
      propagated recursively to all predecessors in the region's dependence
      graph.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setLatestCycle( const WIR_Operation &, long long,
                         std::map<WIR_id_t, long long> & );

    /*!
      @brief getStartCycle determines an operation's absolutely earliest
             execution cycle if that operation were scheduled as the region's
             very first operation.

      @param[in] o A const reference to a %WIR operation.
      @return A long long value denoting the operation's earliest start cycle.

      In general, getStartCycle simply returns the constant value 1, since this
      is the absolutely earliest start time of any operation. However, this can
      be processor-specific so that this method is virtual and can be overloaded
      by derived classes.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual long long getStartCycle( const WIR_Operation & ) const;

    /*!
      @brief computeMaxDelays computes maximum delays for all operations of a
             region.

      @return An unsigned value denoting the critical path length.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    unsigned long long computeMaxDelays( void );

    /*!
      @brief setMaxDelay sets the maximum delay of an operation.

      @param[in] o A const reference to a %WIR operation.
      @param[in] d An unsigned value denoting the given operation's delay value
                   to be set.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setMaxDelay( const WIR_Operation &, unsigned long long );

    /*!
      @brief computeMobility computes the mobility of all operations of a
             region.

      The mobility of an operation is the difference between its latest and
      earliest execution cycle value. Note that the mobility is defined to be
      greater than or equal to 1 in order to avoid divisions by zero.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void computeMobility( void );


    //
    // Helper methods
    //

    /*!
      @brief sameRegisters determines whether two given %WIR registers
             (partially) are the same.

      @param[in] r1 A const reference to a first register.
      @param[in] r2 A const reference to a second register.
      @return true if both registers are partially the same, false otherwise.

      Two hierarchical registers are partially the same if they share a common
      leaf register, including potential pre-colorings of virtual registers.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool sameRegisters( const WIR_BaseRegister &,
                        const WIR_BaseRegister & ) const;


    //
    // Attributes
    //

    /*!
      @brief mBasicBlocks stores pointers to all %WIR basic blocks belonging to
             a scheduling region.
    */
    std::list<WIR_BasicBlock *> mBasicBlocks;

    /*!
      @brief mBlockIDs stores the IDs of all %WIR basic blocks belonging to a
             scheduling region.
    */
    std::set<WIR_id_t> mBlockIDs;

    //! For a given ID, mOpOfID provides the belonging %WIR operation.
    std::map<WIR_id_t, std::reference_wrapper<WIR_Operation>> mOpOfID;

    /*!
      @brief mEarliestCycle maps the ID of a region's %WIR operation to its
             earliest execution cycle.
    */
    std::map<WIR_id_t, long long> mEarliestCycle;

    /*!
      @brief mEarliestCycleOps is the reverse of map mEarliestCycle above, i.e.,
             for each earliest execution cycle, mEarliestCycleOps provides the
             %WIR operations scheduled into that cycle.
    */
    std::map<long long, WIR_OperationSet> mEarliestCycleOps;

    /*!
      @brief mLatestCycle maps the ID of a region's %WIR operation to its latest
             execution cycle.
    */
    std::map<WIR_id_t, long long> mLatestCycle;

    /*!
      @brief mLatestCycleOps is the reverse of map mLatestCycle above, i.e., for
             each latest execution cycle, mLatestCycleOps provides the %WIR
             operations scheduled into that cycle.
    */
    std::map<long long, WIR_OperationSet> mLatestCycleOps;

    /*!
      @brief mMaxDelay maps the ID of a region's %WIR operation to its maximum
             delay.
    */
    std::map<WIR_id_t, unsigned long long> mMaxDelay;

    /*!
      @brief mMaxCycle stores the maximum/latest execution cycle determined for
             a scheduling region.
    */
    long long mMaxCycle;

    //! mMaximumDelay stores the length of the longest path of a region.
    unsigned long long mMaximumDelay;

    /*!
      @brief mMobility maps the ID of a region's %WIR operation to its mobility,
             i.e., to the difference between the operation's latest and earliest
             execution cycle.
    */
    std::map<WIR_id_t, long long> mMobility;

    /*!
      @brief mDGraph represents the internal boost graph data structure for the
             dependence graph.
    */
    DGraph mDGraph;

    /*!
      @brief mNodeByID maps a %WIR operation's ID to its representing dependence
             graph node.
    */
    std::map<WIR_id_t, DGraphVertex> mNodeByID;

    //! mVerbosity stores whether verbose messages should be dumped.
    bool mVerbosity;

    //! mKeepTmpFiles stores whether temporary files should be kept.
    bool mKeepTmpFiles;


  private:

    //
    // Graph visualization.
    //

    /*!
      @brief visualizeNodes dumps the dependence graph nodes into a given DOT
             file.

      @param[in] dotFile A reference to a DOT file opened for writing.
      @param[out] nodeToDotInt A reference to a map mapping a Boost graph node
                               to an integer used to identify the node in the
                               DOT file.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void visualizeNodes( std::fstream &,
                         std::map<DGraphVertex, unsigned int> & ) const;

    /*!
      @brief visualizeEdges dumps the dependence graph edges into a given DOT
             file.

      @param[in] dotFile A reference to a DOT file opened for writing.
      @param[in] nodeToDotInt A const reference to a map mapping a Boost graph
                              node to an integer used to identify the node in
                              the DOT file.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void visualizeEdges( std::fstream &,
                         const std::map<DGraphVertex, unsigned int> & ) const;

};

}       // namespace WIR

#endif  // _WIR_SCHEDULINGREGION_H
