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
  @file wirschedulingconstraint.h
  @brief This file provides the interface of a %WIR container representing
         constraints for an instruction scheduler.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_SCHEDULING_CONSTRAINT_H
#define _WIR_SCHEDULING_CONSTRAINT_H


//
// Include section
//

// Include standard headers
#include <functional>
#include <list>
#include <utility>

// Include boost headers
#include <boost/current_function.hpp>

// Include WIR headers
#include <wir/wircontainer.h>
#include <wir/wirtypes.h>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_Instruction;


/*!
  @brief Class WIR_SchedulingConstraint models constraints for a local
         instruction scheduler.

  WIR_SchedulingConstraint containers are attached to WIR_BasicBlock objects
  during any optimization pass that produces code subject to scheduling
  constraints.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_SchedulingConstraint : public WIR_Container<WIR_SchedulingConstraint>
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor.

      @tparam Args References of class WIR_Instruction.
      @param[in] t A scheduling constraint's type.
      @param[in] args A variadic number of references to %WIR instructions.

      Since scheduling constraints only support local instruction scheduling
      within a single basic block, this constructor fails with an assertion if
      any involved instruction is not inserted into a basic block or if
      instructions from different basic blocks are involved.

      This constructor uses variadic templates so that argument lists of
      arbitrary lengths can be passed to it.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    template<typename... Args>
    WIR_SchedulingConstraint( WIR_SchedulingConstraintType t, Args&&... args ) :
      mType { t }
    {
      DSTART(
        "WIR_SchedulingConstraint::WIR_SchedulingConstraint(WIR_SchedulingConstraintType, Args&& ...)" );

      setConstraint( std::forward<Args>( args )... );
    };

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_SchedulingConstraint( const WIR_SchedulingConstraint & );

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_SchedulingConstraint( WIR_SchedulingConstraint && );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_SchedulingConstraint( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_SchedulingConstraint & operator = ( const WIR_SchedulingConstraint & );

    /*!
      @brief Move-assignment operator.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_SchedulingConstraint & operator = ( WIR_SchedulingConstraint && );

    /*!
      @brief isUnique returns whether scheduling constraints are unique, i.e.,
             whether at most one instance of this container type can be attached
             to a %WIR class.

      @return Always false, comments are not unique.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool isUnique( void ) const;


    //
    // Scheduling constraint handling.
    //

    /*!
      @brief getType returns the type of a scheduling constraint.

      @return The scheduling constraint's type.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_SchedulingConstraintType getType( void ) const;

    /*!
      @brief getInstructionSequence returns the list mInstrSequence.

      @return A const reference to the list mInstrSequence.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const std::list<std::reference_wrapper<WIR_Instruction>> &getInstructionSequence( void ) const;

    /*!
      @brief getInstructions returns the set of all constrained instructions.

      @return A const reference to the set mInstrs.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const WIR_InstructionSet &getInstructions( void ) const;

    /*!
      @brief check checks the validity of a scheduling constraint.

      @return true if a constraint is fulfilled, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool check( void ) const;


  private:

    friend class WIR_BasicBlock;
    friend class WIR_Instruction;

    /*!
      @brief No standard construction allowed, users must use the default
             constructors above instead.
    */
    WIR_SchedulingConstraint( void ) = delete;

    /*!
      @brief setConstraint sets the given list of instructions as scheduling
             constraint.

      @tparam Args References of class WIR_Instruction.
      @param[in] a A reference to a %WIR instruction.
      @param[in] args A variadic number of references to %WIR instructions.

      Since scheduling constraints only support local instruction scheduling
      within a single basic block, setConstraint fails with an assertion if any
      involved instruction is not inserted into a basic block or if instructions
      from different basic blocks are involved.

      setConstraint uses variadic templates so that argument lists of arbitrary
      lengths can be passed to it.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    template<typename... Args>
    void setConstraint( const WIR_Instruction &a, Args&&... args )
    {
      DSTART(
        "void WIR_SchedulingConstraint::setConstraint(const WIR_Instruction&, Args&& ...)" );

      mInstrSequence.push_back( const_cast<WIR_Instruction &>( a ) );
      mInstrs.insert( const_cast<WIR_Instruction &>( a ) );

      setConstraint( std::forward<Args>( args )... );
    };

    /*!
      @brief Function for terminating the setting of a scheduling constraint.

      A scheduling constraint must involve at least two different instructions.
      Since scheduling constraints only support local instruction scheduling
      within a single basic block, setConstraint fails with an assertion if any
      involved instruction is not inserted into a basic block or if instructions
      from different basic blocks are involved.

      setConstraint also forwards the scheduling constraint to all involved %WIR
      instructions. Finally, setConstraint checks the validity of the given
      constraint and fails with an assertion if the constraint is invalid.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setConstraint( void ) const;


    //
    // Attributes.
    //

    //! mType stores a scheduling constraint's type.
    WIR_SchedulingConstraintType mType;

    /*!
      @brief mInstrSequence stores all instructions connected via a scheduling
             constraint in their proper sequential order.
    */
    std::list<std::reference_wrapper<WIR_Instruction>> mInstrSequence;

    /*!
      @brief mInstrs stores all constrained instructions in a set for efficient
             queries.
    */
    WIR_InstructionSet mInstrs;

};

}       // namespace WIR

#endif  // _WIR_SCHEDULING_CONSTRAINT_H
