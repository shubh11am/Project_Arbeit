/*

   This header file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Intermediate Representation Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2021 - 2022

   Hamburg University of Technology (TUHH)
   Institute of Embedded Systems
   21071 Hamburg
   Germany

   http://www.tuhh.de/es/esd/research/wcc

*/

/*!
  @file wirflowfact.h
  @brief This file provides the interface of an abstract base class for %WIR
         flow facts.

  @author Til Mauersberger <Til.Mauersberger@tuhh.de>
*/


#ifndef _WIR_FLOWFACT_H
#define _WIR_FLOWFACT_H


//
// Include section
//

// Include standard headers
#include <map>
#include <ostream>

// Include WIR headers
#include <wir/API/wiridapi.h>
#include <wir/wirtypes.h>


//
// Header Section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_System;
class WIR_BasicBlock;


/*!
  @brief Class WIR_FlowFact is a generic representation of different kinds of
         %WIR flow facts.

  This class provides a minimum set of methods every %WIR flow fact has to
  support. Each derived flow fact has to implement these methods to guarantee a
  common management by the FlowFactSet (included in class WIR_System) and
  FlowFactRef mechanisms.

  @author Til Mauersberger <Til.Mauersberger@tuhh.de>
*/
class WIR_FlowFact : public WIR_ID_API
{

  public:

    //
    //  Constructors and destructors.
    //

    /*!
      @brief Default constructor.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    WIR_FlowFact( void );

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another flow fact to be copied.

      When copying a flow fact that is inserted in some %WIR system, the
      resulting copy will not be inserted in a system.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    WIR_FlowFact( const WIR_FlowFact & );

    /*!
      @brief Destructor.
      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    virtual ~WIR_FlowFact( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      When assigning a flow fact that is inserted in some %WIR system, the
      resulting copy will not be inserted in a system.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_FlowFact & operator = ( const WIR_FlowFact & );


    //
    // Generic type handling.
    //

    /*!
      @brief getType returns the type of a %WIR flow fact, i.e., whether it is
             an entry point, flow restriction or loop bound.

      @return The flow fact's type.

      Since types are characterized by actual flow facts that are defined by
      inheriting from this class, getType is purely virtual here.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    virtual WIR_FlowFactType getType( void ) const = 0;


    //
    // Stream I/O.
    //

    /*!
      @brief The << operator dumps a %WIR flow fact to an output stream.

      @param[in] os A reference to an output stream.
      @param[in] f A const reference to the %WIR flow fact to be dumped.
      @return A reference to the same output stream.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    friend std::ostream & operator << ( std::ostream &, const WIR_FlowFact & );


    //
    // System handling.
    //

    /*!
      @brief isInserted returns wether this object is inserted into some
             WIR_System.

      @return true if the object is inserted, false otherwise.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    bool isInserted( void ) const;

    /*!
      @brief getSystem returns the WIR_System to which this flow fact is
             assigned.

      @return A reference to the assigned WIR_System.

      If this object has not been assigned to a WIR_System before, getSystem
      will fail with an assertion.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    WIR_System &getSystem( void ) const;


    //
    // Evaluation.
    //

    /*!
      @brief isSignificant returns whether a flow fact is significant for WCET
             calculation or not.

      @return true if this flow fact is significant for WCET calculation, false
              otherwise.

      In some cases, a flow fact may not be significant for WCET calculation,
      e.g.:
      - A loop bound with min: -1 and max: -1
      - A flow restriction with 0 <= SUM.
      In such cases, this method should return false, in all other cases true.
      This could be used to remove all unnecessary flow facts.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    virtual bool isSignificant( void ) const;


    //
    // Administration.
    //

    /*!
      @brief reorganize adjusts all references to %WIR basic blocks stored by a
             flow fact after a deep copy of flow facts.

      @param[in] blockIDMap A const reference to a map translating the IDs of
                            the original basic blocks to the new basic block
                            "replacing" it.

      Since the implementation details depend on some actual flow fact's
      characteristics, reorganize is purely virtual here.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    virtual void reorganize( const std::map<WIR_id_t, WIR_BasicBlock *> &blockIDMap ) = 0;


  protected:

    friend class WIR_System;

    //
    // Methods for interaction with FlowFactRefs.
    //

    /*!
      @brief addReference adds a reference to a given %WIR basic block.

      @param[in] b A const reference to a %WIR basic block to which a reference
                   to this flow fact is added.

      The FlowFactRef container of the specified basic block is told that this
      flow fact is using it (so that that FlowFactRef has to hold a reference to
      this flow fact), but only if this flow fact is currently inserted into
      a %WIR system. If no FlowFactRef already exists, a new one (for the given
      basic block) is created.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    void addReference( const WIR_BasicBlock & );

    /*!
      @brief eraseReference erases this flow fact from a given basic block's
             FlowFactRef.

      @param[in] b A const reference to a %WIR basic block from which a
                   reference to this flow fact is erased.

      The FlowFactRef of the given basic block is told that this flow fact is no
      longer using it, but only if this flow fact is currently inserted into a
      %WIR system.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    void eraseReference( const WIR_BasicBlock & );

    /*!
      @brief onInsert is called whenever this flow fact is added to a
             WIR_System.

      @param[in] s A pointer to the WIR_System to which this object is added. If
                   no pointer is specified, this flow fact is marked as not to
                   be assigend to any WIR_System at all.

      This method is virtual so that flow facts deriving from this base class
      can implement their own behaviour regarding adding themselves to all
      concerning FlowFactRefs.

      @note When overriding this method, make sure to either call this base
            class' implementation to set mSystemPointer, or assign it by
            yourself.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    virtual void onInsert( WIR_System * = nullptr );

    /*!
      @brief clone creates a copy of a %WIR flow fact.

      @return A pointer to the newly created copy of this flow fact.

      Since the implementation details depend on some actual flow facts's
      characteristics, clone is a pure virtual method.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    virtual WIR_FlowFact *clone( void ) const = 0;

    /*!
      @brief mSystemPointer points to that %WIR system to which this flow fact
             belongs.
    */
    WIR_System *mSystemPointer;

};

}       // namespace WIR

#endif  // _WIR_FLOWFACT_H
