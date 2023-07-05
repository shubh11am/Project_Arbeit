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
  @file tcpredicatedexecution.h
  @brief This file provides the interface of a TriCore-specific optimization
         that replaces conditionally executed arithmetical operations by
         predicated operations.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _TC_PREDICATEDEXECUTION_H
#define _TC_PREDICATEDEXECUTION_H


//
// Include section
//

// Include WIR headers
#include <optimizations/generic/wiroptimization.h>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_BasicBlock;
class WIR_CompilationUnit;
class WIR_Function;
class WIR_Operation;
class WIR_System;
class WIR_VirtualRegister;


/*!
  @brief Class TC_PredicatedExecution is a TriCore-specific optimization that
         replaces conditionally executed arithmetical operations by predicated
         operations.

  This optimization follows the recommendations from Infineon's TriCore Compiler
  Writer's Guide, sections 1.2.8 and 1.2.9.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class TC_PredicatedExecution final : public WIR_Optimization
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor for system-level optimization.

      @param[in] s A reference to a WIR_System to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit TC_PredicatedExecution( WIR_System & );

    /*!
      @brief Default constructor for compilation unit-level optimization.

      @param[in] c A reference to a WIR_CompilationUnit to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit TC_PredicatedExecution( WIR_CompilationUnit & );

    /*!
      @brief Default constructor for function-level optimization.

      @param[in] f A reference to a WIR_Function to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit TC_PredicatedExecution( WIR_Function & );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~TC_PredicatedExecution( void );


  protected:

    /*!
      @brief runOptimization performs predicated execution in the given
             function.

      @param[in] f A reference to a WIR_Function to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void runOptimization( WIR_Function & );


  private:

    /*!
      @brief checkIf checks whether a basic block is the head of an if-then
             control region that is suitable for predicated execution.

      @param[in] b A const reference to a %WIR basic block.
      @return true if b is the head of a suitable if-then control region, false
              otherwise.

      An if-else control region is suitable for predicated execution iff:
        -# b ends with a conditional jump that depends on a Boolean condition
           (in contrast to computed, indirect jumps).
        -# The then-part of the if-else region consists of a single basic block
           only.
        -# All operations inside the then-part are suitable for predicated
           execution, i.e., are either ADD, SUB or MOV operations of appropriate
           formats.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool checkIf( const WIR_BasicBlock & );

    /*!
      @brief isConditionalJump checks whether the last %WIR operation of a basic
             block realizes a conditional jump suitable for predicated
             execution.

      @param[in] b A const reference to a %WIR basic block.
      @return true if the given basic block ends with a suitable conditional
              jump, false otherwise.

      If a suitable conditional jump is identified, it is also stored in class
      member mConditionalJump.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isConditionalJump( const WIR_BasicBlock & );

    /*!
      @brief isSuitableBB checks whether all operations in a basic block are
             suitable for predicated execution.

      @param[in] b A const reference to a %WIR basic block.
      @return true if all operations in the given basic block are suitable,
              false otherwise.

      Suitable operations are ADD, SUB and MOV of particular formats.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isSuitableBB( const WIR_BasicBlock & ) const;

    /*!
      @brief optimizeIf performs predicated execution for a basic block
             identified as head of a suitable if-then control region.

      The optimization removes the conditional jump from the basic block and
      transforms all operations in the identified then-part into their
      predicated variants.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void optimizeIf( void );

    /*!
      @brief transformJump inserts a comparison operation immediately before the
             identified conditional jump.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void transformJump( void );

    /*!
      @brief transformThen takes all operations from basic block mThenPart and
             transforms them to predicated execution.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void transformThen( void );

    //! mConditionalJump stores a pointer to the identified conditional jump.
    WIR_Operation *mConditionalJump;

    /*!
      @brief mConditionalReg stores a pointer to the virtual register holding
             the conditional predicate.
    */
    WIR_VirtualRegister *mConditionalReg;

    /*!
      @brief mThenPart points to a basic block identified as then-part of an
             if-then or if-else control region.
    */
    WIR_BasicBlock *mThenPart;

    /*!
      @brief mCreateNegatedForms stores whether the negated forms of predicated
             operations (i.e., CADDN, CSUBN, SELN or CMOVN) have to be
             generated.
    */
    bool mCreateNegatedForms;

};

}       // namespace WIR

#endif  // _TC_PREDICATEDEXECUTION_H
