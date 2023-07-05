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
  @file wirpdbusarbitration.h
  @brief This file provides the basic interface of a priority division %WIR bus
         arbitration policy.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_PD_BUS_ARBITRATION_H
#define _WIR_PD_BUS_ARBITRATION_H


//
// Include section
//

// Include standard headers
#include <map>
#include <vector>

// Include WIR headers
#include <wir/wirfpbusarbitration.h>
#include <wir/wirtdmabusarbitration.h>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_BaseProcessor;


/*!
  @brief Class WIR_PriorityDivisionBusArbitration represents parameters for a
         priority division bus arbitration policy.

  Priority division is a combination of fixed priority and TDMA arbitration.
  A bus slot may be configured to be arbitrated with "pure TDMA" (i.e., the slot
  has exactly one exclusive owner, and only this owner may access the bus during
  the slot) or with "pure PD". In PD mode, unique priorities are assigned to
  each slot and core. If the slot is not claimed by its owner, the bus is
  granted to that requesting core with the highest positive priority. Cores with
  priority 0 are excluded from arbitration.

  Bus requests are never interrupted.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_PriorityDivisionBusArbitration : public WIR_FixedPriorityBusArbitration,
                                           public WIR_TDMABusArbitration
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor producing an empty PD parameter set.

      @param[in] n An unsigned integer denoting the number of processor cores
                   that are managed by this priority division bus arbiter.
      @param[in] t An unsigned integer denoting the maximal access time.
      @param[in] d An unsigned integer denoting the number of cycles for the
                   arbitration delay.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_PriorityDivisionBusArbitration( unsigned int, unsigned int,
                                        unsigned int );

    /*!
      @brief Constructor producing a PD parameter set that mimics the given TDMA
             parameters.

      @param[in] t A const reference to a TDMA arbitration parameter set.

      This constructor simply copies all TDMA parameter values and sets all slot
      modes to TDMA.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_PriorityDivisionBusArbitration( const WIR_TDMABusArbitration & );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_PriorityDivisionBusArbitration( void );

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
    // Priority division handling.
    //

    /*!
      @brief getNumberOfArbitratedCores returns the number of cores that are
             managed under PD arbitration.

      @return The number of cores managed by this bus arbiter, i.e., the number
              of cores that owns a TDMA/PD slot or that has non-zero priority.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual unsigned int getNumberOfArbitratedCores( void ) const;

    /*!
      @brief getSlotModes returns the operation modes per bus slot.

      @return A const reference to the vector of slot operation modes.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const std::vector<WIR_BusArbitrationType> &getSlotModes( void ) const;

    /*!
      @brief getMustGrantWindows returns all windows in which an access by the
             specified processor core will always be granted.

      @param[in] p A const reference to a processor core.
      @return A set of windows for which bus access will always be granted.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const WIR_OffsetIntervalSet &getMustGrantWindows( const WIR_BaseProcessor & ) const;

    /*!
      @brief getMayGrantWindows returns all windows in which an access by the
             specified processor core may be possible but cannot be granted.

      @param[in] p A const reference to a processor core.
      @return A set of windows for which bus access will may be granted.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const WIR_OffsetIntervalSet &getMayGrantWindows( const WIR_BaseProcessor & ) const;


  protected:

    friend class WIR_Bus;
    friend class WIR_System;

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_PriorityDivisionBusArbitration( const WIR_PriorityDivisionBusArbitration &__o ) = default;

    /*!
      @brief clone creates a copy of a priority division bus arbiter.

      @return A pointer to the newly created copy of this priority division
              arbiter.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_PriorityDivisionBusArbitration *clone( void ) const;

    /*!
      @brief pushBackSlot adds a new bus slot at the end of vector mSlots,
             after its current last element.

      @param[in] t The slot's arbitration type.
      @param[in] l The slot's length in clock cycles.
      @param[in] p A const reference to the processor core that owns this slot.

      This variant of pushBackSlot must only be used for arbitration types TDMA
      or PD of the current slot. pushBackSlot asserts if other slot arbitration
      types are specified, since slots under another arbitration type do not
      have an owning processor core. For these other slot arbitration policies,
      use pushBackSlot( WIR_BusArbitrationType, WIR_TDMAOffset ) below.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void pushBackSlot( WIR_BusArbitrationType, WIR_TDMAOffset,
                       const WIR_BaseProcessor & );

    /*!
      @brief pushBackSlot adds a new bus slot at the end of vector mSlots,
             after its current last element.

      @param[in] t The slot's arbitration type.
      @param[in] l The slot's length in clock cycles.

      This variant of pushBackSlot must only be used for arbitration types other
      than TDMA or PD of the current slot. pushBackSlot asserts if TDMA or PD
      are specified as slot arbitration type, since slots under these
      arbitration type must have an owning processor core. For TDMA or PD as
      slot arbitration policy use
      pushBackSlot( WIR_BusArbitrationType, WIR_TDMAOffset, WIR_BaseProcessor & )
      above.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void pushBackSlot( WIR_BusArbitrationType, WIR_TDMAOffset );

    /*!
      @brief recomputePDGrantWindows computes the priority division grant
             windows, if necessary.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void recomputePDGrantWindows( void ) const;

    /*!
      @brief setMaxAccessTime sets the maximum access time for a single memory
             access via a PD-arbitrated bus.

      @param[in] t The buses maximum memory access time.

      @note This is NOT the arbitration time, only the pure memory module access
            time.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void setMaxAccessTime( unsigned int );

    /*!
      @brief mRecomputePDGrantWindows specifies whether the priority division
             grant windows need to be recomputed or not.
    */
    mutable bool mRecomputePDGrantWindows;


  private:

    /*!
      @brief No standard construction allowed, users must use
             WIR_TDMABusArbitration( unsigned int ) instead.
    */
    WIR_PriorityDivisionBusArbitration( void ) = delete;

    //! mSlotModes stores the operation modes per bus slot.
    std::vector<WIR_BusArbitrationType> mSlotModes;

    /*!
      @brief mMustGrantWindows maps the ID of a processor core to those windows
             in which an access will always be granted.
    */
    mutable std::map<WIR_id_t, WIR_OffsetIntervalSet> mMustGrantWindows;

    /*!
      @brief mMayGrantWindows maps the ID of a processor core to those windows
             in which an access may be possible but cannot be granted.
    */
    mutable std::map<WIR_id_t, WIR_OffsetIntervalSet> mMayGrantWindows;

};

}       // namespace WIR

#endif  // _WIR_PD_BUS_ARBITRATION_H
