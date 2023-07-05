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
  @file tcblockschedulingregion.h
  @brief This file provides the interface of a TriCore-specific class
         representing basic block regions in which scheduling is performed.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _TC_BLOCKSCHEDULINGREGION_H
#define _TC_BLOCKSCHEDULINGREGION_H


//
// Include section
//

// Include standard headers
#include <list>
#include <map>

// Include WIR headers
#include <wir/wirtypes.h>
#include <optimizations/scheduling/wirblockschedulingregion.h>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_BasicBlock;
class WIR_Instruction;
class WIR_Operation;
class WIR_RegisterParameter;


/*!
  @brief Class TC_BlockSchedulingRegion represents TriCore-specific basic block
         scheduling regions.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class TC_BlockSchedulingRegion : public WIR_BlockSchedulingRegion
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor.

      @param[in] b A reference to a %WIR basic block to be considered as
                   scheduling region.
      @param[in] verbosity A Boolean defaulting to false that denotes whether
                           verbose messages shall be dumped.
      @param[in] keepTmpFiles A Boolean defaulting to false that denotes whether
                              temporary files shall be kept.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC_BlockSchedulingRegion( WIR_BasicBlock &, bool = false, bool = false );

    /*!
      @brief Destructor.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~TC_BlockSchedulingRegion( void );


    //
    // Code reordering.
    //

    /*!
      @brief moveOperations moves a bundle of TriCore operations to a new
             position after a given instruction within a scheduling region.

      @param[in,out] b A reference to a list (bundle) of TriCore operation
                       pointers to be moved.
      @param[in] pred A const pointer to a %WIR instruction after which the
                      bundle's operations will be moved. If pred is the nullptr,
                      the bundle's operations will be moved to the very
                      beginning of the region.
      @return A pointer to a newly generated %WIR instruction containing the
              given bundle.

      All operations of a bundle are assumed to be executed in the same
      execution cycle, i.e., in parallel. Thus, moveOperations groups them
      altogether into a single, novel %WIR instruction.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_Instruction *moveOperations( std::list<WIR_Operation *> &,
                                             const WIR_Instruction * );


    //
    // TriCore-specific methods.
    //

    /*!
      @brief isSiliconBugNOP determines whether an operation is a NOP related to
             a silicon bug.

      @param[in] o A const reference to a %WIR operation to be checked.
      @return true iff the operation is a silicon bug-related NOP, false
              otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isSiliconBugNOP( const WIR_Operation & ) const;

    /*!
      @brief postProcessingHook performs TriCore-specific actions after having
             done instruction scheduling for a block region.

      This method removes NOP operations originally required as silicon bug fix,
      and that became obsolete after instruction scheduling.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void postProcessingHook( void );


  protected:

    //
    // Initialization
    //

    /*!
      @brief checkUCert checks whether there is an uncertain TriCore-specific
             dependence between an operation and some register parameter.

      @param[in] o1 A const reference to a %WIR operation.
      @param[in] rp2 A const reference to a %WIR register parameter.
      @return true if there is an uncertain dependence between o1 and rp2, false
              otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool checkUCert( const WIR_Operation &,
                             const WIR_RegisterParameter & ) const;

    /*!
      @brief checkCtrl checks whether there is a TriCore-specific control
             dependence between an operation and some register parameter.

      @param[in] o1 A const reference to a %WIR operation.
      @param[in] rp2 A const reference to a %WIR register parameter.
      @return true if there is a control dependence between o1 and rp2, false
              otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool checkCtrl( const WIR_Operation &,
                            const WIR_RegisterParameter & ) const;


    //
    // Latency and Cycle handling
    //

    /*!
      @brief computeLatency computes the TriCore-specific latency between two
             dependent %WIR operations.

      @param[in] o1 A const reference to a first %WIR operation.
      @param[in] o2 A const reference to a second %WIR operation.
      @param[in] t A specifier denoting the type of dependence between both
                   operations.
      @return A signed long long value denoting the latency between the two
              given operations in clock cycles.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual long long computeLatency( const WIR_Operation &,
                                      const WIR_Operation &,
                                      WIR_DGEdgeType );

    /*!
      @brief getLatency determines the number of clock cycles it takes from
             issuing the given operation until availability of its results.

      @param[in] o A const reference to a %WIR operation.
      @return The latency of o in clock cycles.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual long long getLatency( const WIR_Operation & );

    /*!
      @brief computeSiliconBugStall computes the amount of additional stall
             cycles to be accounted for if two operations depend on each other
             due to a silicon bug fix.

      @param[in] o1 A const reference to a first %WIR operation.
      @param[in] o2 A const reference to a second %WIR operation.
      @return A signed long long value denoting the number of additional stall
              cycles between the two given operations.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    long long computeSiliconBugStall( const WIR_Operation &,
                                      const WIR_Operation & );

    /*!
      @brief getStartCycle determines a TriCore operation's absolutely earliest
             execution cycle if that operation were scheduled as the region's
             very first operation.

      @param[in] o A const reference to a %WIR operation.
      @return A long long value denoting the operation's earliest start cycle.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual long long getStartCycle( const WIR_Operation & ) const;


    //
    // Helper methods
    //

    /*!
      @brief determineCallRetSituation determines whether the current region is
             a direct successor of some CALL or RET operation, or whether the
             region itself ends with a RET.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void determineCallRetSituation( void );


  private:

    /*!
      @brief No standard construction allowed, users must use one of the
             standard constructors above instead.
    */
    TC_BlockSchedulingRegion( void ) = delete;


    //
    // Attributes
    //

    /*!
      @brief mCallRetSituation stores whether the current region is a direct
             successor of some CALL or RET operation, or whether the region
             itself ends with a RET.
    */
    bool mCallRetSituation;

    //! mSiliconBugNOPs stores a region's NOP operations related to silicon bugs.
    WIR_OperationSet mSiliconBugNOPs;

    /*!
      @brief Silicon bug fixes for TriCore always show up in the form of two
             operations. One is the buggy operation, and the second one is the
             fixing operation which usually is a NOP. All these NOPs are
             collected above in set mSiliconBug NOPs. The buggy operations are
             stored here in map mSiliconBugNOPPartner where the ID of the
             corresponding NOP operation serves as key. During instruction
             scheduling, it can happen that other operations are scheduled
             between such silicon bug fix-related operation pairs, turning the
             NOP obsolete. This map mSiliconBugNOPPartner helps to identify such
             situations in order to remove obsolete NOPs after scheduling.
    */
    std::map<WIR_id_t,
             std::reference_wrapper<WIR_Operation>> mSiliconBugNOPPartner;

};

}       // namespace WIR

#endif  // _TC_BLOCKSCHEDULINGREGION_H
