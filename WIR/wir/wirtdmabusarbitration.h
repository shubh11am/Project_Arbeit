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
  @file wirtdmabusarbitration.h
  @brief This file provides the basic interface of a TDMA %WIR bus arbitration
         policy.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_TDMA_BUS_ARBITRATION_H
#define _WIR_TDMA_BUS_ARBITRATION_H


//
// Include section
//

// Include standard headers
#include <list>
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
  @brief Class WIR_TDMABusArbitration represents parameters for a TDMA bus
         arbitration policy.

  TDMA arbitration means that a cyclic schedule is built that consists of slots
  of configurable size. Each of the slots is assigned to exactly one owner core
  which is the only core which may access the bus in this slot.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_TDMABusArbitration : public virtual WIR_BusArbitration
{

  public:

    //
    // Local type definitions.
    //

    /*!
      @brief Class SlotInfo holds information about a TDMA bus slot.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    class SlotInfo
    {

      public:

        /*!
          @brief Default constructor for TDMA bus slot information.

          @param[in] i The slot's index (between 0 and numberOfSlots - 1).
          @param[in] p A const reference to the processor core that owns this
                       slot.
          @param[in] s The slot's start offset, measured from the beginning of
                       the TDMA schedule.
          @param[in] l The slot's length in clock cycles.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        SlotInfo( unsigned int, const WIR_BaseProcessor &, WIR_TDMAOffset,
                  WIR_TDMAOffset );

        /*!
          @brief Default constructor for bus slot information specific to the
                 priority division arbitration policy.

          @param[in] i The slot's index (between 0 and numberOfSlots - 1).
          @param[in] s The slot's start offset, measured from the beginning of
                       the TDMA schedule.
          @param[in] l The slot's length in clock cycles.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        SlotInfo( unsigned int, WIR_TDMAOffset, WIR_TDMAOffset );

        /*!
          @brief getIndex returns a TDMA slot's index.

          @return The slot's index (between 0 and numberOfSlots - 1).

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        unsigned int getIndex( void ) const;

        /*!
          @brief getOwner returns the processor core that owns a TDMA slot.

          @return A const reference to the owning WIR_BaseProcessor.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        const WIR_BaseProcessor &getOwner( void ) const;

        /*!
          @brief getStart returns a TDMA slot's start offset, measured from the
                 beginning of the TDMA schedule.

          @return The slot's start offset.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        WIR_TDMAOffset getStart( void ) const;

        /*!
          @brief getLength returns a TDMA slot's length in clock cycles.

          @return The slot's length.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        WIR_TDMAOffset getLength( void ) const;

        /*!
          @brief getGrantWindow returns a TDMA slot's grant window.

          @return The slot's grand window.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        const WIR_OffsetIntervalSet &getGrantWindow( void ) const;

        /*!
          @brief This operator compares two TDMA slots for equality.

          @param[in] __o A const reference to another object to be compared.
          @return true iff both operands are equal, false otherwise.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        bool operator == ( const SlotInfo & ) const;


      private:

        friend class WIR_TDMABusArbitration;

        /*!
          @brief setGrantWindow sets a TDMA slot's grant value.

          @param[in] g An R-value reference to the new grant window.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        void setGrantWindow( WIR_OffsetIntervalSet && );

        //! mIndex holds a slot's index (between 0 and numberOfSlots - 1).
        unsigned int mIndex;

        //! mOwner points to the processor core owning this TDMA slot.
        WIR_BaseProcessor *mOwner;

        /*!
          @brief mStart holds the slot's start offset, measured from the
                 beginning of the TDMA schedule.
        */
        WIR_TDMAOffset mStart;

        //! mLength holds the slot's length in clock cycles.
        WIR_TDMAOffset mLength;

        /*!
          @brief mGrantWindow holds the TDMA offsets at which an access can be
                 granted.

          These are in general (all computations modulo schedule length):
            [ slot start - arbitration delay,
              slot end - arbitration delay - maximum access time ]

          The offsets are measured from beginning of the TDMA schedule.
        */
        WIR_OffsetIntervalSet mGrantWindow;

    };


    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor.

      @param[in] t An unsigned integer denoting the maximal access time.
      @param[in] d An unsigned integer denoting the number of cycles for the
                   arbitration delay.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_TDMABusArbitration( unsigned int, unsigned int );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_TDMABusArbitration( void );

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
    // TDMA slot handling.
    //

    /*!
      @brief getNumberOfArbitratedCores returns the number of cores that are
             managed under TDMA arbitration.

      @return The number of cores managed by this bus arbiter.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual unsigned int getNumberOfArbitratedCores( void ) const;

    /*!
      @brief getSlots returns the information about all TDMA slots for this bus.

      @return A const reference to vector mSlots.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const std::vector<SlotInfo> &getSlots( void ) const;

    /*!
      @brief getSlots returns all TDMA slots owned by the specified processor
             core.

      @param[in] p A const reference to a processor core.
      @return A list of TDMA slots owned by the specified processor core.

      Within the returned list, all TDMA slots are sorted by their start offsets
      so that the order of slots in the list reflects their order within the
      TDMA schedule.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const std::list<SlotInfo> getSlots( const WIR_BaseProcessor & ) const;

    /*!
      @brief getNumberOfSlots returns the number of TDMA slots for this bus.

      @return An unsigned int containing of number TDMA bus slots.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    unsigned int getNumberOfSlots( void ) const;

    /*!
      @brief getScheduleLength returns the total length of the configured TDMA
             schedule.

      @return A TDMA offset modelling the total schedule length.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_TDMAOffset getScheduleLength( void ) const;

    /*!
      @brief getGrantWindows returns all TDMA offsets at which an access by the
             specified processor core may be granted.

      @param[in] p A const reference to a processor core.
      @return A set of TDMA offsets for which bus access may be granted.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const WIR_OffsetIntervalSet &getGrantWindows( const WIR_BaseProcessor & ) const;


  protected:

    friend class WIR_Bus;
    friend class WIR_PriorityDivisionBusArbitration;
    friend class WIR_System;

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_TDMABusArbitration( const WIR_TDMABusArbitration &__o ) = default;

    /*!
      @brief clone creates a copy of a TDMA bus arbiter.

      @return A pointer to the newly created copy of this TDMA arbiter.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_TDMABusArbitration *clone( void ) const;

    /*!
      @brief recomputeGrantWindows computes the grant windows of all TDMA slots
             and processor cores, if necessary.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void recomputeGrantWindows( void ) const;

    /*!
      @brief setMaxAccessTime sets the maximum access time for a single memory
             access via a TDMA-arbitrated bus.

      @param[in] t The buses maximum memory access time.

      @note This is NOT the arbitration time, only the pure memory module access
            time.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void setMaxAccessTime( unsigned int );

    //! mMaxAccessTime stores the maximum access time for a TDMA bus.
    unsigned int mMaxAccessTime;

    //! mSlots stores the information about all configured TDMA bus slots.
    mutable std::vector<SlotInfo> mSlots;

    /*!
      @brief mSlotsOfCore maps each processor core's ID to the IDs of those TDMA
             slots that the core owns.
    */
    std::map<WIR_id_t, std::list<unsigned int>> mSlotsOfCore;

    //! mGrantWindows maps the ID of a processor core to its grant window.
    mutable std::map<WIR_id_t, WIR_OffsetIntervalSet> mGrantWindows;

    //! mScheduleLength stores the total length of the configured TDMA schedule.
    WIR_TDMAOffset mScheduleLength;

    //! mArbitrationDelay stores the bus arbiter's arbitration delay.
    WIR_TDMAOffset mArbitrationDelay;

    /*!
      @brief mRecomputeGrantWindows specifies whether the grant windows need to
             be recomputed or not.
    */
    mutable bool mRecomputeGrantWindows;


  private:

    /*!
      @brief No standard construction allowed, users must use
             WIR_TDMABusArbitration( unsigned int ) instead.
    */
    WIR_TDMABusArbitration( void ) = delete;

    /*!
      @brief pushBackSlot adds a new TDMA slot at the end of vector mSlots,
             after its current last element.

      @param[in] l The slot's length in clock cycles.
      @param[in] p A const reference to the processor core that owns this slot.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void pushBackSlot( WIR_TDMAOffset, const WIR_BaseProcessor & );

};

}       // namespace WIR

#endif  // _WIR_TDMA_BUS_ARBITRATION_H
