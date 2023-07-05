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
  @file wirloopinvariantcm.h
  @brief This file provides the interface of a loop-invariant code motion
         optimization.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_LICM_H
#define _WIR_LICM_H


//
// Include section
//

// Include standard headers
#include <list>
#include <map>
#include <set>
#include <utility>

// Include WIR headers
#include <wir/wirtypes.h>
#include <optimizations/generic/wiroptimization.h>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_BasicBlock;
class WIR_ControlTreeNode;
class WIR_CompilationUnit;
class WIR_Function;
class WIR_Operation;
class WIR_Parameter;
class WIR_StringParameter;
class WIR_System;


/*!
  @brief Class WIR_LoopInvariantCM is a loop-invariant code motion optimization.

  The basic algorithms implemented in this class stem from the following
  sources:
  - Steven S. Muchnick. Advanced Compiler Design & Implementation. Morgan
    Kaufmann Publishers, 2011.
    Chapter 13.2

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_LoopInvariantCM : public WIR_Optimization
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
    explicit WIR_LoopInvariantCM( WIR_System & );

    /*!
      @brief Default constructor for compilation unit-level optimization.

      @param[in] c A reference to a WIR_CompilationUnit to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_LoopInvariantCM( WIR_CompilationUnit & );

    /*!
      @brief Default constructor for function-level optimization.

      @param[in] f A reference to a WIR_Function to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_LoopInvariantCM( WIR_Function & );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_LoopInvariantCM( void );


  protected:

    /*!
      @brief runOptimization moves loop-invariant code in the given system.

      @param[in] s A reference to a WIR_System to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void runOptimization( WIR_System & );

    /*!
      @brief runOptimization moves loop-invariant code in the given compilation
             unit.

      @param[in] c A reference to a WIR_CompilationUnit to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void runOptimization( WIR_CompilationUnit & );

    /*!
      @brief runOptimization moves loop-invariant code in the given function.

      @param[in] f A reference to a WIR_Function to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void runOptimization( WIR_Function & );


  protected:

    /*!
      @brief isLoopInvariant determines whether an unstructured string parameter
             is loop-invariant or not.

      @param[in] s A const reference to an unstructured string parameter.
      @return Always false.

      Since this task is processor-specific and might or might not be necessary
      for some actual processor, this method is virtual and can be overloaded if
      required.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool isLoopInvariant( const WIR_StringParameter & ) const;


    //
    // Attributes
    //

    /*!
      @brief mInvariantInstrIDs stores the IDs of identified loop-invariant
             instructions.
    */
    std::set<WIR_id_t> mInvariantInstrIDs;

    //! mInvariantOpIDs stores the IDs of identified loop-invariant operations.
    std::set<WIR_id_t> mInvariantOpIDs;

    /*!
      @brief mInvariantParamIDs stores the IDs of identified loop-invariant
             parameters.
    */
    std::set<WIR_id_t> mInvariantParamIDs;


  private:

    /*!
      @brief identifyLoops identifies all cyclic control structures in a given
             %WIR function.

      @param[in] f A const reference to a WIR_Function in which loops shall be
                   identified.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void identifyLoops( const WIR_Function & );

    /*!
      @brief identifyLIC identifies loop-invariant code in a given loop.

      @param[in] l A const reference to a cyclic control tree region.

      This method implements procedure Mark_Invar() from S. S. Muchnick, figure
      13.17, page 398.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void identifyLIC( const WIR_ControlTreeNode & );

    /*!
      @brief identifyLIC identifies loop-invariant code in a given basic block.

      @param[in] b A const reference to a %WIR basic block.
      @return true if loop-invariant code was identified, false otherwise.

      This method implements procedure Mark_Block() from S. S. Muchnick, figure
      13.17, page 399.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool identifyLIC( const WIR_BasicBlock & );

    /*!
      @brief moveLIC moves loop-invariant code out of a given loop.

      This method implements procedure Move_Invar() from S. S. Muchnick, figure
      13.32, page 404.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void moveLIC( void );

    /*!
      @brief checkLoopInvariant checks whether a %WIR operation is
             loop-invariant.

      @param[in] o A const reference to a %WIR operation.

      If the operation is found to be loop-invariant, its ID is added to set
      mInvariantOpIDs.

      An operation is loop-invariant if, for each of its parameters (cf. S. S.
      Muchnick, page 397):
      1. the parameter is constant, or
      2. all definitions that reach this use of the parameter are located
         outside the current loop, or
      3. there is exactly one definition of the parameter that reaches the
         operation and that definition is an operation inside the loop that
         is itself loop-invariant.
      Calls, returns or jumps are never loop-invariant.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void checkLoopInvariant( const WIR_Operation & );

    /*!
      @brief checkLoopInvariant checks whether a %WIR parameter is
             loop-invariant.

      @param[in] p A const reference to a %WIR parameter.

      If the parameter is found to be loop-invariant, its ID is added to set
      mInvariantParamIDs.

      A parameter is loop-invariant if (cf. S. S. Muchnick, page 397):
      1. it is constant, or
      2. all definitions that reach this use of the parameter are located
         outside the current loop, or
      3. there is exactly one definition of the parameter that reaches the
         parameter and that definition is an operation inside the loop that
         is itself loop-invariant.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void checkLoopInvariant( const WIR_Parameter & );

    /*!
      @brief isExecutedBefore determines whether one operation is always
             executed before another operation in the very same loop.

      @param[in] o1 A const reference to an operation to be executed first.
      @param[in] o2 A const reference to another operation to be executed after
                    o1.
      @return true if o1 is executed before o2, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isExecutedBefore( const WIR_Operation &, const WIR_Operation & ) const;

    /*!
      @brief insertPreHeader inserts a new pre-header basic block into which
             loop-invariant code will be moved.

      @return A reference to the newly generated pre-header block.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_BasicBlock &insertPreHeader( void );


    //
    // Attributes
    //

    //! mLoops stores all loops of a function, from innermost to outermost ones.
    std::list<WIR_ControlTreeNode *> mLoops;

    /*!
      @brief mCurrentLoop refers to that element of list mLoops that is
             currently processed by the loop-invariant code motion.
    */
    std::list<WIR_ControlTreeNode *>::iterator mCurrentLoop;

    //! mEntry points to the entry block of the currently processed loop.
    WIR_BasicBlock *mEntry;

    //! mBSet stores all basic blocks of the currently processed loop.
    WIR_BasicBlockSet mBSet;

    //! mExits stores all exit blocks of the currently processed loop.
    WIR_BasicBlockSet mExits;

    /*!
      @brief mIsForLoop stores whether the currently processed loop is a for- or
             while-do loop.
    */
    bool mIsForLoop;

    /*!
      @brief mBackEdges contains all back edges inside a currently processed
             loop in the form of basic block pairs.
    */
    WIR_BasicBlockPairSet mBackEdges;

    /*!
      @brief mPreHeader stores for each processed loop with its ID which basic
             block has been inserted as new pre-header.
    */
    std::map<WIR_id_t, WIR_BasicBlock *> mPreHeader;

    /*!
      @brief mNewEntry stores whether a loop with a given ID requires a new
             entry basic block due to the insertion of a new pre-header for some
             innermost loop.
    */
    std::map<WIR_id_t, WIR_BasicBlock *> mNewEntry;

    /*!
      @brief mRegisterDefinitions stores how often a register with a given ID is
             defined within a loop.
    */
    std::map<WIR_id_t, unsigned int> mRegisterDefinitions;

    /*!
      @brief mInvariantInstrs stores all loop-invariant instructions in their
             proper order.
    */
    std::list<WIR_Instruction *> mInvariantInstrs;

};

}       // namespace WIR

#endif  // _WIR_LICM_H
