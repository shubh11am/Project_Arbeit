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
  @file wirflowrestriction.h
  @brief This file provides the interface of %WIR flow restrictions.

  @author Til Mauersberger <Til.Mauersberger@tuhh.de>
*/


#ifndef _WIR_FLOWRESTRICTION_H
#define _WIR_FLOWRESTRICTION_H


//
// Include section
//

// Include standard headers
#include <functional>
#include <list>
#include <map>
#include <ostream>
#include <utility>

// Include WIR headers
#include <flowfacts/wirflowfact.h>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_BasicBlock;
class WIR_System;


/*!
  @brief Class WIR_FlowRestriction provides a description of (in-)feasible paths
         in the form of inequations over execution frequencies of basic blocks.

  A flow restriction is an inequation like, e.g.,

  3*Codeblock1 + 2*Codeblock2 <= 6*Codeblock3

  It consists of a greater-equal and less-equal side. A restriction holds
  information about the execution frequency of code blocks in relation to
  one another.

  @author Til Mauersberger <Til.Mauersberger@tuhh.de>
*/
class WIR_FlowRestriction final : public WIR_FlowFact
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor creating an empty flow restriction.

      The individual summands can be added using methods
      - @ref addToLeq
      - @ref addToGeq

      This flow fact is not automatically inserted into a %WIR system.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    WIR_FlowRestriction( void );

    /*!
      @brief Constructor creating a non-empty flow restriction.

      @param[in] leq A const reference to a list of summands for the
                     less-equal side.
      @param[in] geq A const reference to a list of summands for the
                     greater-equal side.

      This flow fact is not automatically inserted into a %WIR system.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    WIR_FlowRestriction( const std::list<std::pair<int, std::reference_wrapper<const WIR_BasicBlock>>> &,
                         const std::list<std::pair<int, std::reference_wrapper<const WIR_BasicBlock>>> & );

    /*!
      @brief Constructor creating a non-empty flow restriction.

      @param[in] leq An rvalue reference to a list of summands for the
                     less-equal side.
      @param[in] geq An rvalue reference to a list of summands for the
                     greater-equal side.

      This flow fact is not automatically inserted into a %WIR system.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    WIR_FlowRestriction( std::list<std::pair<int, std::reference_wrapper<const WIR_BasicBlock>>> &&,
                         std::list<std::pair<int, std::reference_wrapper<const WIR_BasicBlock>>> && );

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      The copy will not be inserted in any WIR_System or referenced by any
      WIR_FlowFactRefs.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    WIR_FlowRestriction( const WIR_FlowRestriction & );

    /*!
      @brief Destructor.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    virtual ~WIR_FlowRestriction( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      The copy will not be inserted in any WIR_System or referenced by any
      WIR_FlowFactRefs.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_FlowRestriction & operator = ( const WIR_FlowRestriction & );


    //
    // Generic type handling.
    //

    /*!
      @brief getType returns the type of a %WIR flow fact, i.e., whether it is
             an entry point, flow restriction or loop bound.

      @return WIR_FlowFactType::flowrestriction

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    virtual WIR_FlowFactType getType( void ) const override;


    //
    // Stream I/O.
    //

    /*!
      @brief The << operator dumps a %WIR flow restriction to an output stream.

      @param[in] os A reference to an output stream.
      @param[in] f A const reference to the %WIR flow restriction to be dumped.
      @return A reference to the same output stream.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    friend std::ostream & operator << ( std::ostream &,
                                        const WIR_FlowRestriction & );


    //
    // Manipulation of flow restrictions.
    //

    /*!
      @brief addToLeq adds a summand to a flow restriction's less-equal side.

      @param[in] factor A signed integer denoting a weighting factor for the
                        execution frequency.
      @param[in] b A const reference to a %WIR basic block to add.

      The basic block with the given integer factor (negative factors add the
      summand to the geq-side) is added to the leq side. If factor is zero,
      nothing changes internally and the summand will not be regarded.

      This is the only method manipulating the internal data structures (and by
      this invokes the update mechanisms of class FlowFactRef).

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    void addToLeq( int, const WIR_BasicBlock & );

    /*!
      @brief addToGeq adds a summand to a flow restriction's greater-equal side.

      @param[in] factor A signed integer denoting a weighting factor for the
                        execution frequency.
      @param[in] b A const reference to a %WIR basic block to add.

      The basic block with the given integer as factor (negative factors will
      add the summand to the leq-side) is added to the geq side.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    void addToGeq( int, const WIR_BasicBlock & );

    /*!
      @brief eraseSummand erases a summand from a flow restriction.

      @param[in] b A const reference to a %WIR basic block to be removed from
                   this flow restriction.
      @return The factor of the erased summand (regardless whether it was on the
              leq- or geq-side). A return value of zero indicates that the
              summand was not found.

      A whole summand is removed from a flow restriction.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    int eraseSummand( const WIR_BasicBlock & );

    /*!
      @brief replaceSummand replaces a summand in a flow restriction.

      @param[in] bOld A const reference to the basic block to be replaced.
      @param[in] bNew A const reference to that basic block the summand is
                      changed to.

      The factor of the summand remains unchanged (no differentiation between
      leq- and geq-side).

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    void replaceSummand( const WIR_BasicBlock &, const WIR_BasicBlock & );


    //
    // Access to flow restrictions.
    //

    /*!
      @brief getLeq returns the less-equal side of a flow restriction.

      @return A const reference to a list holding all leq summands.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    const std::list<std::pair<int, std::reference_wrapper<const WIR_BasicBlock>>> &getLeq( void ) const;

    /*!
      @brief getGeq returns the greater-equal side of a flow restriction.

      @returns A const reference to a list holding all geq summands.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    const std::list<std::pair<int, std::reference_wrapper<const WIR_BasicBlock>>> &getGeq( void ) const;


    //
    // Evaluation.
    //

    /*!
      @brief isPartOfLeq tests whether a basic block belongs to the leq side.

      @param[in] b A const reference to a basic block.
      @return true if the basic block is found on the leq side, false otherwise.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    bool isPartOfLeq( const WIR_BasicBlock & ) const;

    /*!
      @brief isPartOfGeq tests whether a basic block belongs to the geq side.

      @param[in] b A const reference to a basic block.
      @return true if the basic block is found on the geq side, false otherwise.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    bool isPartOfGeq( const WIR_BasicBlock & ) const;

    /*!
      @brief isPartOfFlowFact tests whether a basic block is part of this flow
             restriction.

      @param[in] b A const reference to a basic block.
      @return true if the given basic block is referenced on either side of this
              flow restriction, false otherwise.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    bool isPartOfFlowFact( const WIR_BasicBlock & ) const;

    /*!
      @brief isSignificant returns whether a flow restriction is significant for
             WCET calculation or not.

      @return true if there are summands on the leq side, false otherwise.

      In some cases, a flow fact may not be significant for WCET calculation,
      e.g.:
      - A flow restriction with 0 <= SUM.
      In such cases, this method returns false, in all other cases true.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    virtual bool isSignificant( void ) const override;

    /*!
      @brief isEqual returns true iff this flow restriction is equal to the
             specified one.

      @param[in] __o A const reference to a flow restriction to compare against.
      @return true if both flow restrictions are equal according to the
              requirements stated below, false otherwise.

      Two flow restrictions r and r' are equal iff
        - each summand of the leq-side of r is equal to the summand of r' at
          the same position of the leq-side of r', and
        - each summand of the geq-side of r is equal to the summand of r' at
          the same position of the geq-side of r', and
      two summands s and s' are equal iff
        - they both have the same factor, and
        - the LABELS of the referenced basic block are identical.   (!)

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    bool isEqual( const WIR_FlowRestriction & ) const;


    //
    // Administration.
    //

    /*!
      @brief reorganize adjusts all references to %WIR basic blocks stored by a
             flow fact after a deep copy of flow facts.

      @param[in] blockIDMap A const reference to a map translating the IDs of
                            the original basic blocks to the new basic block
                            "replacing" it.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    virtual void reorganize( const std::map<WIR_id_t, WIR_BasicBlock *> & ) override;


  protected:

    /*!
      @brief onInsert is called whenever this flow restriction is added to a
             WIR_System.

      @param[in] s A pointer to the WIR_System to which this object is added. If
                   no pointer is specified, this flow restriction is marked as
                   not to be assigend to any WIR_System at all.

      This method overrides the method defined by the base class to add
      references of itself to all basic block's FlowfactRefs that this flow
      restriction is made up of.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    void onInsert( WIR_System * = nullptr ) override;

    /*!
      @brief clone creates a copy of a %WIR flow restriction.

      @return A pointer to the newly created copy of this flow restriction.

      This method only calls the copy constructor and allocates a new %WIR
      flow restriction on the heap.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    virtual WIR_FlowFact *clone( void ) const override;


  private:

    //! mLEQ holds all summands of the leq-side of a flow restriction.
    std::list<std::pair<int, std::reference_wrapper<const WIR_BasicBlock>>> mLEQ;

    //! mGEQ holds all summands of the geq-side of a flow restriction.
    std::list<std::pair<int, std::reference_wrapper<const WIR_BasicBlock>>> mGEQ;

};

}       // namespace WIR

#endif  // _WIR_FLOWRESTRICTION_H
