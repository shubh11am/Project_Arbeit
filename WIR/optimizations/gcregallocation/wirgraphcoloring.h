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
  @file wirgraphcoloring.h
  @brief This file provides the basic interface of a graph-coloring based
         register allocator.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_GRAPHCOLORING_H
#define _WIR_GRAPHCOLORING_H


//
// Include section
//

// Include standard headers
#include <functional>
#include <list>
#include <map>
#include <set>
#include <utility>
#include <vector>

// Include WIR headers
#include <wir/wirtypes.h>
#include <optimizations/bitopt/wirbitopt.h>

// Include local headers
#include "wirinterferencegraph.h"


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_BaseRegister;
class WIR_BasicBlock;
class WIR_CompilationUnit;
class WIR_Function;
class WIR_Instruction;
class WIR_InterferenceGraph;
class WIR_PhysicalRegister;
class WIR_System;
class WIR_VirtualRegister;


/*!
  @brief Class WIR_GraphColoring provides a generic, processor-independent
         implementation of a graph-coloring register allocator.

  This register allocator provides the following features:
  - Live range splitting of virtual registers by generating webs
  - Safe coalescing using both Briggs' and George's heuristics
  - Full support of arbitrary LLIR register hierarchies, also during coalescing
  - Coalescing heuristic taking loop nesting depths into account
  - Spilling heuristic from Chaitin taking individual positions of defs/uses of
    a potential spill, their loop nesting depths, the graph node's degree and
    whether source/target of a move instruction are spilled into account.
  - Alternative best-of-three spilling heuristic from Bernstein taking the area
    of virtual registers into account.

  The basic algorithms implemented in this class stem from the following
  sources:
  - Steven S. Muchnik. Advanced Compiler Design & Implementation. Morgan
    Kaufmann Publishers, 2011.
    Chapter 16.3
  - Andrew W. Appel. Modern Compiler Implementation in C. Cambridge University
    Press, 2004.
    Chapter 11
  - The errata list to A. W. Appel's book from
    http://www.cs.princeton.edu/~appel/modern/c/errata99.html
  - Preston Briggs. Register Allocation via Graph Coloring. PhD Thesis, Rice
    University. 1992.
  - David Bernstein, Martin C. Golumbic, Yishay Mansour, Ron Y. Pinter, Dina Q.
    Goldin, Hugo Krawczyk, Itai Nahshon. Spill code minimization techniques for
    optimizing compilers. PLDI '89, pages 258-263, July 1989.
  - Gregory J. Chaitin. Register Allocation and Spilling via Graph Coloring.
    SIGPLAN Notices, 17(6):98-105, June 1982.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_GraphColoring : public WIR_BitOpt
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor for system-level optimization.

      @param[in] s A reference to a WIR_System to be optimized.
      @param[in] verbosity A Boolean denoting whether verbose messages shall be
                           dumped.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_GraphColoring( WIR_System &, bool = false );

    /*!
      @brief Default constructor for compilation unit-level optimization.

      @param[in] c A reference to a WIR_CompilationUnit to be optimized.
      @param[in] verbosity A Boolean denoting whether verbose messages shall be
                           dumped.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_GraphColoring( WIR_CompilationUnit &, bool = false );

    /*!
      @brief Default constructor for function-level optimization.

      @param[in] f A reference to a WIR_Function to be optimized.
      @param[in] verbosity A Boolean denoting whether verbose messages shall be
                           dumped.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_GraphColoring( WIR_Function &, bool = false );

    /*!
      @brief Destructor.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_GraphColoring( void );


    //
    // Methods for coalescing.
    //

    /*!
      @brief setCoalescingBriggs (de-) activates coalescing according to Briggs.

      @param[in] coalescingBriggs A Boolean specifying whether this kind of
                                  coalescing shall be activated or not.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setCoalescingBriggs( bool = true );

    /*!
      @brief getCoalescingBriggs returns whether coalescing according to Briggs
             is activated.

      @return true if coalescing according to Briggs is active, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool getCoalescingBriggs( void ) const;

    /*!
      @brief setCoalescingGeorge (de-) activates coalescing according to George.

      @param[in] coalescingGeorge A Boolean specifying whether this kind of
                                  coalescing shall be activated or not.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setCoalescingGeorge( bool = true );

    /*!
      @brief getCoalescingGeorge returns whether coalescing according to George
             is activated.

      @return true if coalescing according to George is active, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool getCoalescingGeorge( void ) const;

    /*!
      @brief getCoalescing returns whether any kind of coalescing is activated.

      @return true if coalescing is active, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool getCoalescing( void ) const;


    //
    // Methods for the generation of webs.
    //

    /*!
      @brief setGenerateWebs (de-) activates the splitting of virtual registers
             into webs.

      @param[in] makeWebs A Boolean specifying whether the generation of webs
                          shall be activated or not.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setGenerateWebs( bool = true );

    /*!
      @brief getGenerateWebs returns whether splitting of virtual registers into
             webs is activated.

      @return true iff webs are generated, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool getGenerateWebs( void ) const;


    //
    // Methods for best-of-three spilling.
    //

    /*!
      @brief setBestOfThreeSpilling (de-) activates the best-of-three spilling
             heuristic proposed by Bernstein.

      @param[in] bestOf3 A Boolean specifying whether this spilling heuristic
                         shall be activated or not.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setBestOfThreeSpilling( bool = true );

    /*!
      @brief getBestOfThreeSpilling returns whether the best-of-three spilling
             heuristic proposed by Bernstein is activated.

      @return true iff this spilling heuristic is active, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool getBestOfThreeSpilling( void ) const;


    //
    // Methods for rematerialization.
    //

    /*!
      @brief setRematerialization (de-) activates rematerialization.

      @param[in] rematerialize A Boolean specifying whether rematerialization
                               shall be activated or not.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setRematerialization( bool = true );

    /*!
      @brief getRematerialization returns whether rematerialization is
             activated.

      @return true iff rematerialization is active, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool getRematerialization( void ) const;


    //
    // Methods for stack frame organization for spills.
    //

    /*!
      @brief setCoalesceStackLocations (de-) activates coalescing of stack
             locations for spills.

      @param[in] coalesceStackLocations A Boolean specifying whether coalescing
                                        of actual spills shall be done in order
                                        to reduce the size of the required stack
                                        frame.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setCoalesceStackLocations( bool = true );

    /*!
      @brief getCoalesceStackLocations returns whether coalescing of stack
             locations for spills is activated.

      @return true iff coalescing of spills is active, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool getCoalesceStackLocations( void ) const;


    //
    // Methods for full spilling of all virtual registers.
    //

    /*!
      @brief setSpillAll (de-) activates full spilling of all virtual registers.

      @param[in] spillAll A Boolean specifying whether full spilling of all
                          virtual register shall be done or not.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setSpillAll( bool = true );

    /*!
      @brief getSpillAll returns whether full spilling of all virtual registers
             is activated.

      @return true iff full spilling of all virtual registers is active, false
              otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool getSpillAll( void ) const;


    //
    // Miscellaneous methods.
    //

    /*!
      @brief setMarkSpillCode (de-) activates marking of all generated spill and
             rematerialization instructions with a dedicated comment.

      @param[in] markSpill A Boolean specifying whether spill and
                           rematerialization instructions shall be marked or
                           not.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setMarkSpillCode( bool = true );

    /*!
      @brief getMarkSpillCode returns whether all generated spill and
             rematerialization instructions shall be marked with a dedicated
             comment.

      @return true iff marking spill instructions is active, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool getMarkSpillCode( void ) const;

    /*!
      @brief getVerbosity returns whether verbose messages should be dumped.

      @return true if verbose messages should be dumped, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool getVerbosity( void ) const;


  protected:

    using WIR_RematerializationMap =
      std::map<std::reference_wrapper<WIR_RegisterParameter>,
               std::list<WIR_Instruction *>,
               WIR_Compare<WIR_RegisterParameter>>;

    //
    // Methods for optimization management.
    //

    /*!
      @brief runOptimization allocates registers in the given system.

      @param[in] s A reference to a %WIR_System to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void runOptimization( WIR_System & );

    /*!
      @brief runOptimization allocates registers in the given compilation unit.

      @param[in] c A reference to a WIR_CompilationUnit to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void runOptimization( WIR_CompilationUnit & );

    /*!
      @brief runOptimization allocates registers in the given function.

      @param[in] f A reference to a WIR_Function to be optimized.

      This method implements procedure Main() from A. W. Appel, page 251.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void runOptimization( WIR_Function & );

    /*!
      @brief saveBestSolutionHook allows to save processor-specific allocation
             data in the course of Bernstein's best-of-three spilling heuristic.

      Since this task is processor-specific and might or might not be necessary
      for some actual processor's EABI, this method is virtual and can be
      overloaded if required.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void saveBestSolutionHook( void );

    /*!
      @brief restoreBestSolutionHook allows to restore processor-specific
             allocation data in the course of Bernstein's best-of-three spilling
             heuristic.

      Since this task is processor-specific and might or might not be necessary
      for some actual processor's EABI, this method is virtual and can be
      overloaded if required.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void restoreBestSolutionHook( void );


    //
    // Methods for processor-specific physical registers.
    //

    /*!
      @brief createPhregs sets up the lists mPhregs and
             mPhregsForPrecoloringOnly of all processor-specific physical
             registers contained in the specified %WIR function.

      @param[in] f A reference to a %WIR function.

      Since listing of phyiscal registers is processor-specific, this method is
      purely virtual.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void createPhregs( WIR_Function &f ) = 0;

    /*!
      @brief initializationHook allows to perform processor-specific actions
             before doing some actual coloring or spilling.

      @param[in] f A reference to a %WIR function.

      Since this task is processor-specific and might or might not be necessary
      for some actual processor's EABI, this method is virtual and can be
      overloaded if required.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void initializationHook( WIR_Function & );

    /*!
      @brief isStackPointer returns whether the specified %WIR register is a
             system's stack pointer.

      @param[in] r A const reference to a %WIR register to be examined.
      @return True iff the specified register if the stack pointer, false
              otherwise.

      Since the identification of the stack pointer is processor-specific, this
      method is purely virtual.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool isStackPointer( const WIR_BaseRegister &r ) const = 0;


    //
    // Pre-coloring.
    //

    /*!
      @brief isPrecolored checks map mPrecolored whether the specified %WIR
             register is pre-colored.

      @param[in] r A const reference to a %WIR register.
      @return true if r is pre-colored, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isPrecolored( const WIR_BaseRegister & ) const;


    //
    // Interference graph construction.
    //

    /*!
      @brief buildProcessorSpecificInterferences adds edges to the interference
             graph expressing processor-specific interferences.

      @param[in] f A reference to a %WIR function.
      @param[in,out] igraph A reference to the interference graph.

      Since this task is processor-specific, this method is purely virtual.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void buildProcessorSpecificInterferences( WIR_Function &f,
                                                      WIR_InterferenceGraph &igraph ) = 0;


    //
    // Spilling.
    //

    /*!
      @brief isPriorityRegister returns whether a given register has high
             priority for color assignment.

      @param[in] r A const reference to a virtual register.
      @return true iff the register has high priority, false otherwise.

      Since the notion of high-priority registers is processor-specific, this
      method is virtual so that it can be overloaded.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool isPriorityRegister( const WIR_VirtualRegister & ) const;

    /*!
      @brief computeSpillCosts computes the spill costs of all registers in the
             given set and stores them in map mSpillCosts.

      @param[in] f A const reference to a WIR_Function.
      @param[in] igraph A const reference to the interference graph.
      @param[in] workListSpill A const reference to the current set of
                               candidates for spilling, i.e., to all
                               significant-degree nodes in igraph.
      @param[in,out] rematInsns A reference to a map storing potential code for
                                rematerialization for a given %WIR parameter.

      The cost of spilling a register r is taken to be

        defwt * \\sum_{def \\in r} 10^depth(def) +
        usewt * \\sum_{use \\in r} 10^depth(use) -
        copywt * \\sum{copy \\in r} 10^depth(copy)

      where def, use, and copy are individual definitions, uses, and register
      copies of the register r; defwt, usewt, and copywt are relative weights
      assigned to the instruction types.

      Computing spill costs should take the following into account:

      1. If a register's value can be more efficiently recomputed than reloaded,
         the cost of recomputing it is used instead.
      2. If the source or target of a copy instruction is spilled, the
         instruction is no longer needed.
      3. If a spilled value is used several times in the same basic block and
         the restored value remains live until the last use of the spilled value
         in that block, then only a single load of the value is needed in the
         block.

      This method implements procedure Compute_Spill_Costs() from S. S.
      Muchnick, page 502. The structure of computeSpillCosts corresponds to that
      of insertSpillCode for obvious reasons. Since for some processors, some
      other methods for spill cost computation might be desired, this method is
      virtual so that it can be overloaded.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void computeSpillCosts( const WIR_Function &,
                                    const WIR_InterferenceGraph &,
                                    const WIR_VirtualRegisterSet &,
                                    WIR_RematerializationMap & );

    /*!
      @brief getSpillLoadCosts returns the costs of one single spill-load for
             the specified register parameter.

      @param[in] p A const reference to a %WIR register parameter.
      @return Some processor-specific cost measure for a spill-load of register
              parameter p.

      Since the determination of spill-load costs is processor-specific, this
      method is purely virtual.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual unsigned int getSpillLoadCosts( const WIR_RegisterParameter &p ) const = 0;

    /*!
      @brief getSpillStoreCosts returns the costs of one single spill-store for
             the specified register parameter.

      @param[in] p A const reference to a %WIR register parameter.
      @return Some processor-specific cost measure for a spill-store of register
              parameter p.

      Since the determination of spill-store costs is processor-specific, this
      method is purely virtual.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual unsigned int getSpillStoreCosts( const WIR_RegisterParameter &p ) const = 0;

    /*!
      @brief getMoveCosts returns the costs of one single move operation that
             can be omitted due to spilling.

      @param[in] o A const reference to a move operation.
      @return Some processor-specific cost measure for a move operation.

      Since the determination of move costs is processor-specific, this method
      is purely virtual.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual unsigned int getMoveCosts( const WIR_Operation &o ) const = 0;

    /*!
      @brief selectSpillCandidate selects one register from workListSpill as a
             candidate for spilling, using either Chaitin's or Bernstein's
             spilling heuristics.

      @param[in] igraph A const reference to the interference graph.
      @param[in] workListSpill A const reference to the current set of
                               candidates for spilling, i.e., to all
                               significant-degree nodes in igraph.
      @return A reference to the virtual register selected for spilling.

      Since for some processors, some other spilling heuristics might be
      desired, this method is virtual so that it can be overloaded.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_VirtualRegister &selectSpillCandidate( const WIR_InterferenceGraph &,
                                                       const WIR_VirtualRegisterSet & ) const;

    /*!
      @brief spillCostsChaitin returns the spill costs for the specified
             register according to Chaitin's rule.

      @param[in] r A const reference to a virtual %WIR register.
      @param[in] igraph A const reference to the interference graph.
      @return spillCostsChaitin( r ) = mSpillCosts[w] / degree(w) (cf. P.
              Briggs, page 19, equation (2.1)).

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    long double spillCostsChaitin( const WIR_VirtualRegister &,
                                   const WIR_InterferenceGraph & ) const;

    /*!
      @brief spillCostsBernstein1 returns the spill costs for the specified
             register according to Bernstein's first rule.

      @param[in] r A const reference to a virtual %WIR register.
      @param[in] igraph A const reference to the interference graph.
      @return spillCostsBernstein1( r ) = mSpillCosts[w] / degree(w)^2 (cf. P.
              Briggs, page 19, equation (2.2), or S. S. Muchnick, page 521 ff).

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    long double spillCostsBernstein1( const WIR_VirtualRegister &,
                                      const WIR_InterferenceGraph & ) const;

    /*!
      @brief spillCostsBernstein2 returns the spill costs for the specified
             register according to Bernstein's second rule.

      @param[in] r A const reference to a virtual %WIR register.
      @param[in] igraph A const reference to the interference graph.
      @return spillCostsBernstein2( r ) =
              mSpillCosts[w] / ( area(w) * degree(w) ) (cf. P. Briggs, page 19,
              equation (2.3), or S. S. Muchnick, page 523).

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    long double spillCostsBernstein2( const WIR_VirtualRegister &,
                                      const WIR_InterferenceGraph & ) const;

    /*!
      @brief spillCostsBernstein3 returns the spill costs for the specified
             register according to Bernstein's third rule.

      @param[in] r A const reference to a virtual %WIR register.
      @param[in] igraph A const reference to the interference graph.
      @return spillCostsBernstein3( r ) =
              mSpillCosts[w] / ( area(w) * degree(w)^2 ) (cf. P. Briggs, page
              19, equation (2.4), or S. S. Muchnick, page 523).

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    long double spillCostsBernstein3( const WIR_VirtualRegister &,
                                      const WIR_InterferenceGraph & ) const;

    /*!
      @brief area represents a spill cost measure attempting to quantify the
             impact a register has on live ranges throughout a %WIR function.

      @param[in] r A const reference to a virtual %WIR register.
      @param[in] igraph A const reference to the interference graph.
      @return area( r ) = \\sum_{I \\in inst(r)} (width(I) * 5^depth(I))
              (cf. P. Briggs, page 19, equation (2.5), or S. S. Muchnick, page
              523).

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    unsigned int area( const WIR_VirtualRegister &,
                       const WIR_InterferenceGraph & ) const;


    //
    // Coalescing.
    //

    /*!
      @brief getUseOfMove returns the used register of the specified %WIR move
             operation.

      @param[in] o A const reference to a move operation.
      @return A reference to the %WIR register used by the move operation.

      Since the identification of used registers in move operations is
      processor-specific, this method is purely virtual.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_BaseRegister &getUseOfMove( const WIR_Operation &o ) const = 0;

    /*!
      @brief getDefOfMove returns the defined register of the specified %WIR
             move operation.

      @param[in] o A const reference to a move operation.
      @return A reference to the %WIR register defined by the move operation.

      Since the identification of defined registers in move operations is
      processor-specific, this method is purely virtual.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_BaseRegister &getDefOfMove( const WIR_Operation &o ) const = 0;

    /*!
      @brief selectCoalescingCandidate selects one register from workListMoves
             as a candidate for coalescing, using some favorite coalescing
             heuristic.

      @param[in] igraph A const reference to the interference graph.
      @param[in] workListMoves A const reference to the current set of
                               candidates for coalescing.
      @return A reference to the move operation selected for coalescing.

      This method implements the heuristic to select a move operation located in
      that basic block with the maximal loop nesting depth (cf. P. Briggs, page
      104). Since for some processor architectures, other heuristics might be
      desired, this method is virtual so that it can be overloaded.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_Operation &selectCoalescingCandidate( const WIR_InterferenceGraph &,
                                                      const WIR_OperationSet & ) const;

    /*!
      @brief avoidCoalescing can be used to check whether the two given
             registers which are both move-related must not be coalesced due to
             processor-specific reasons.

      @param[in] o A const reference to a move operation.
      @param[in] r1 A const reference to the first register to be checked.
      @param[in] r2 A const reference to the second register to be checked.
      @param[in] igraph A const reference to the interference graph.
      @return true iff coalescing r1 and r2 must be avoided.

      Since the result of this function is processor-specific, this method is
      virtual so that it can be overloaded.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool avoidCoalescing( const WIR_Operation &,
                                  const WIR_BaseRegister &,
                                  const WIR_BaseRegister &,
                                  const WIR_InterferenceGraph & ) const;


    //
    // Rematerialization.
    //

    /*!
      @brief getRematerializationCosts returns the costs of one single
             recomputation of the specified used parameter.

      @param[in] p A const reference to a %WIR register parameter.
      @return Some processor-specific cost measure for a rematerialization of
              the register in used in p.

      Since the determination of rematerialization costs is processor-specific,
      this method is virtual so that it can be overloaded.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual unsigned int getRematerializationCosts( const WIR_RegisterParameter & ) const;

    /*!
      @brief getRematerializationInstructions returns a list of machine
             instructions for one single recomputation of the specified used
             parameter.

      @param[in] p A const reference to a %WIR register parameter.
      @return Some processor-specific machine instructions for the
              rematerialization of the register in used in p.

      Since the determination of rematerialization code is processor-specific,
      this method is purely virtual.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual std::list<WIR_Instruction *> getRematerializationInstructions( const WIR_RegisterParameter &p ) const = 0;


    //
    // Coloring.
    //

    using WIR_ColorMap =
      std::map<std::reference_wrapper<WIR_VirtualRegister>, unsigned int,
               WIR_Compare<WIR_VirtualRegister>>;

    /*!
      @brief selectColors assigns actual colors to the leaf registers in the
             specified vector.

      @param[in] leafs A const reference to a vector containing all virtual leaf
                       registers of one arbitrary interference graph node.
      @param[in] igraph A const reference to the interference graph.
      @return A map mapping each leaf from vector leafs to its color number.
              Valid color numbers are from the interval
              [1, getAvailableColors()]. If the returned map contains no entry,
              no suitable color for the leafs could be found so that they become
              actual spills.

      This method must not yet assign colors to the interference graph - this is
      done elsewhere. In order to determine feasible colors for the leaf
      registers, this method should make use of
      WIR_InterferenceGraph::getPossibleColors(). It must be ensured that the
      returned map is either empty or contains exactly one entry per leaf. It
      must hold that none of the colors used in this returned map is already
      used for adjacent interference graph nodes.

      Since the selection of actual colors is processor-specific, this method is
      purely virtual.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_ColorMap selectColors( const std::vector<std::reference_wrapper<WIR_VirtualRegister>> &leafs,
                                       const WIR_InterferenceGraph &igraph ) = 0;


    //
    // Code transformation.
    //

    /*!
      @brief isSpillStore checks whether the specified instruction spill-stores
             a certain virtual register.

      @param[in] i A const reference to a %WIR instruction.
      @param[in] r A const reference to a virtual register to be examined.
      @return true iff the given instruction is a spill-store.

      Since this task is processor-specific, this method is purely virtual.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool isSpillStore( const WIR_Instruction &i,
                               const WIR_VirtualRegister &r ) const = 0;

    /*!
      @brief isSpillLoad checks whether the specified instruction spill-loads a
             certain virtual register.

      @param[in] i A const reference to a %WIR instruction.
      @param[in] r A const reference to a virtual register to be examined.
      @return true iff the given instruction is a spill-load.

      Since this task is processor-specific, this method is purely virtual.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool isSpillLoad( const WIR_Instruction &i,
                              const WIR_VirtualRegister &r ) const = 0;

    /*!
      @brief resolveSpillCoalescingConflict resolves a conflict when two
             registers with different positions in a register hierarchy shall be
             coalesced during computeStackLocations.

      @param[in] r1 A const reference to a first %WIR virtual register to be
                    coalesced.
      @param[in] r2 A const reference to a second %WIR register to be coalesced.
      @return A pair of registers that have the same number of leafs in their
              respective hierarchies and that can thus be used for coalescing.

      Since the resolution of such a coalescing conflict is processor-specific,
      this method is virtual and can be overloaded if required.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual std::pair<std::reference_wrapper<WIR_VirtualRegister>,
                      std::reference_wrapper<WIR_VirtualRegister>> resolveSpillCoalescingConflict( const WIR_VirtualRegister &,
                                                                                                   const WIR_VirtualRegister & ) const;

    /*!
      @brief getStackSize returns the byte size of the specified register on the
             stack.

      @param[in] r A const reference to a %WIR register.
      @return The number of bytes r occupies on the stack.

      Since the computation of stack sizes is processor-specific, this method is
      virtual and can be overloaded if required.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual unsigned int getStackSize( const WIR_BaseRegister & ) const;

    /*!
      @brief getStackPosOfSubReg returns the stack position of some child
             register, if the root of the entire register hierarchy is located
             in the specified stack position.

      @param[in] r A const reference to a virtual %WIR child register.
      @param[in] rootPos The position of r's root register on the stack.
      @return The according stack position of r on the stack.

      Since the computation of stack positions is processor-specific, this
      method is purely virtual.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual unsigned int getStackPosOfSubReg( const WIR_VirtualRegister &r,
                                              unsigned int rootPos ) const = 0;

    /*!
      @brief insertSpillLoad inserts code for a spill-load of a register into
             the %WIR.

      @param[in] clone A const reference to a cloned register created during
                       spilling-related live range splitting. This cloned
                       register will be used by the actually generated
                       spill-load instruction.
      @param[in] r A const reference to the original, un-cloned %WIR register.
      @param[in] stackPos The stack position from which the register is spill-
                          loaded.
      @param[in] b A reference to a %WIR basic block in which to insert the
                   generated spill code.
      @param[in] pos A const iterator refering to the position before which the
                     generated spill code will be inserted.

      Since the creation of spill-load instructions is processor-specific, this
      method is purely virtual. insertSpillLoad is responsible to add all
      generated spill-load instructions to map mSpillLoads.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void insertSpillLoad( const WIR_BaseRegister &clone,
                                  const WIR_BaseRegister &r,
                                  int stackPos, WIR_BasicBlock &b,
                                  std::list<std::reference_wrapper<WIR_Instruction>>::const_iterator pos ) = 0;

    /*!
      @brief insertSpillStore inserts code for a spill-store of a register into
             the %WIR.

      @param[in] clone A const reference to a cloned register created during
                       spilling-related live range splitting. This cloned
                       register will be used by the actually generated
                       spill-store instruction.
      @param[in] r A const reference to the original, un-cloned %WIR register.
      @param[in] stackPos The stack position to which the register is spill-
                          stored.
      @param[in] b A reference to a %WIR basic block in which to insert the
                   generated spill code.
      @param[in] pos A const iterator refering to the position before which the
                     generated spill code will be inserted.

      Since the creation of spill-store instructions is processor-specific, this
      method is purely virtual. insertSpillStore is responsible to add all
      generated spill-store instructions to map mSpillStores.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void insertSpillStore( const WIR_BaseRegister &clone,
                                   const WIR_BaseRegister &r,
                                   int stackPos, WIR_BasicBlock &b,
                                   std::list<std::reference_wrapper<WIR_Instruction>>::const_iterator pos ) = 0;

    /*!
      @brief markSpillInstruction marks the given %WIR instruction as spill code
             using a dedicated %WIR comment.

      @param[in,out] i A reference to the instruction to be marked as spill
                       code.
      @param[in] r A const reference to the register that is spilled by the
                   given instruction.
      @return A reference to the marked instruction i.

      markSpillInstruction shall be invoked by implementations of the methods
      insertSpillLoad and insertSpillStore above.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Instruction &markSpillInstruction( WIR_Instruction &,
                                           const WIR_BaseRegister & );

    /*!
      @brief rewriteProgramHook allows to perform processor-specific actions
             after having transformed the %WIR code.

      @param[in,out] f A reference to a %WIR function.

      Since these actions are processor-specific and might or might not be
      necessary for some actual processor's EABI, this method is virtual and can
      be overloaded if required.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void rewriteProgramHook( WIR_Function & );

    /*!
      @brief getCandidatePhregs returns a set of physical registers that could
             be used for the specified virtual register according to a
             processor's ISA.

      @param[in] r A const reference to a virtual %WIR register.
      @return A set of physical registers.

      Since this task is processor-specific, this method is purely virtual.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_PhysicalRegisterSet getCandidatePhregs( const WIR_VirtualRegister &r ) = 0;

    /*!
      @brief getCandidatePhreg returns one element from the specified set of
             registers that will finally be used within
             allocateUncoloredActualSpills for spilling.

      @param[in] candidates A const reference to a set of potential candidate
                            phregs for spilling.
      @return A reference to one of the registers in set candidates.

      Since this task is processor-specific, this method is purely virtual.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual const WIR_PhysicalRegister &getCandidatePhreg( const WIR_PhysicalRegisterSet &candidates ) = 0;

    /*!
      @brief postProcessingHook allows to perform processor-specific actions
             after having done register allocation for a function, using e.g.,
             the set of inserted spill operations mInsertedSpillCode.

      @param[in,out] f A reference to a %WIR function.

      For example, postProcessingHook can be used to realize processor-specific
      calling conventions afterwards, if necessary.

      Since these actions are processor-specific and might or might not be
      necessary for some actual processor's EABI, this method is virtual and can
      be overloaded if required.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void postProcessingHook( WIR_Function & );

    /*!
      @brief adjustStack allocates additional space in the specified function's
             stack frame and adjusts all stack-related memory accesses
             accordingly.

      @param[in,out] f A reference to a %WIR function.

      Since stack handling is processor-specific, this method is purely virtual.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void adjustStack( WIR_Function &f ) = 0;

    /*!
      @brief postRACleanup allows to perform very final processor-specific
             cleanup actions, particularly after stack frame reorganization.

      @param[in] f A reference to a %WIR function.

      Since these actions are processor-specific and might or might not be
      necessary for some actual processor's EABI, this method is virtual and can
      be overloaded if required.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void postRACleanup( WIR_Function & );

    /*!
      @brief For a list of instructions implementing one spill-load or -store,
             getPhregOfSpill determines that physical register that is actually
             spill-loaded or -stored.

      @param[in] spill A const reference to a list of spill instructions.

      Since spill-code is processor-specific, this method is purely virtual.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_PhysicalRegister &getPhregOfSpill( const std::list<std::reference_wrapper<WIR_Instruction>> &spill ) = 0;


    //
    // Attributes.
    //

    /*!
      @brief mAdditionalStackSpace stores for the currently processed %WIR
             function, how many additional bytes have to be allocated in the
             function's stack frame for spilling.
    */
    unsigned int mAdditionalStackSpace;

    /*!
      @brief mPhregs holds all processor-specific physical registers to be used
             by register allocation.
    */
    std::list<std::reference_wrapper<const WIR_PhysicalRegister>> mPhregs;

    /*!
      @brief mPhregsForPrecoloringOnly holds all processor-specific physical
             registers that might be used JUST for pre-coloring but that must
             not be used by register allocation itself.
    */
    WIR_PhysicalRegisterSet mPhregsForPrecoloringOnly;

    /*!
      @brief mAllocationSequence optionally stores a precedence list of virtual
             registers.

      If mAllocationSequence is non-empty, then the allocator only allocates
      those registers that are specified in mAllocationSequence. All other
      registers are ignored.

      The order in which virtual registers occur in the list is relevant. The
      allocator tries to keep registers at the front of the list in registers,
      while virtual registers at the tail of the list might be candidates for
      spilling.

      mAllocationSequence is not actively used here in this basic register
      allocator. However, it might be used in some derived classes implementing,
      e. g., a WCET-aware register allocator that constrains itself to certain
      subsets of virtual registers.
    */
    std::list<std::reference_wrapper<WIR_VirtualRegister>> mAllocationSequence;

    /*!
      @brief mPrecolored maps of all (virtual and physical) registers pre-
             assigned a color to their pre-colored physical register.
    */
    std::map<std::reference_wrapper<WIR_BaseRegister>,
             std::reference_wrapper<WIR_PhysicalRegister>,
             WIR_Compare<WIR_BaseRegister>> mPrecolored;

    /*!
      @brief mSpillCosts maps a virtual register from mWorkListSpill to its
             spill costs.
    */
    std::map<WIR_id_t, unsigned int> mSpillCosts;

    /*!
      @brief mInsertedSpillCode stores all additionally generated spill
             instructions. This might be useful when adjusting a function's
             stack accesses.
    */
    std::list<std::reference_wrapper<WIR_Instruction>> mInsertedSpillCode;

    /*!
      @brief If partial register allocation is done using mAllocationSequence,
             then mBBToBeAllocated points to that basic block that actually
             shall be allocated.

      I.e., mBBToBeAllocated is that basic block that contains all these
      registers from mAllocationSequence.
    */
    WIR_BasicBlock *mBBToBeAllocated;

    /*!
      @brief For a given virtual register ID, mSpillLoads stores all finally
             generated spill-load instructions.
    */
    std::map<WIR_id_t, std::list<std::list<std::reference_wrapper<WIR_Instruction>>>> mSpillLoads;

    /*!
      @brief For a given virtual register ID, mSpillStores stores all finally
             generated spill-store instructions.
    */
    std::map<WIR_id_t, std::list<std::list<std::reference_wrapper<WIR_Instruction>>>> mSpillStores;

    /*!
      @brief For a given virtual register ID, mRematerializedSpills stores all
             finally generated rematerialization instructions.
    */
    std::map<WIR_id_t, std::list<std::list<std::reference_wrapper<WIR_Instruction>>>> mRematerializedSpills;

    /*!
      @brief For a given spill instruction's ID, mStackOffsetOfSpillInstruction
             stores the stack offset to that/from which is spilled.
    */
    std::map<WIR_id_t, int> mStackOffsetOfSpillInstruction;


  private:

    //
    // Methods for optimization management.
    //

    /*!
      @brief saveBestSolution saves the best allocation determined so far using
             Bernstein's best-of-three spilling heuristic.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void saveBestSolution( void );

    /*!
      @brief restoreBestSolution restores the previously saved best allocation
             from Bernstein's best-of-three spilling heuristic.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void restoreBestSolution( void );


    //
    // Pre-coloring.
    //

    /*!
      @brief precolorSpecialRegs scans the specified %WIR function and replaces
             all occurrences of virtual registers pre-colored with a phreg from
             mPhregsForPrecoloringOnly by the associated physical register.

      @param[in,out] f A reference to a %WIR function.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void precolorSpecialRegs( WIR_Function & ) const;

    /*!
      @brief initPrecolors initializes the map mPrecolored according to the
             pre-color information attached to the specified %WIR function.

      @param[in] f A const reference to a %WIR function.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void initPrecolors( const WIR_Function & );


    //
    // Interference graph construction.
    //

    /*!
      @brief build builds the interference graph for the specified %WIR
             function.

      @param[in] f A reference to a %WIR function.
      @param[in,out] igraph A reference to the interference graph.

      Construct the interference graph, and categorize each node as either
      move-related or non-move-related. A move-related node is one that is
      either the source or destination of a move instruction.

      This function implements procedure Build() from A. W. Appel, page 252.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void build( WIR_Function &, WIR_InterferenceGraph & );

    /*!
      @brief buildPrecolors realizes the pre-coloring of the interference graph.

      @param[in,out] igraph A reference to the interference graph.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void buildPrecolors( WIR_InterferenceGraph & ) const;

    /*!
      @brief buildLoopNestingDepth assigns each interference graph node its loop
             nesting depth.

      @param[in,out] igraph A reference to the interference graph.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void buildLoopNestingDepth( WIR_InterferenceGraph & ) const;


    //
    // Work list handling.
    //

    /*!
      @brief makeWorkList sets up the various data structures to keep track of
             graph nodes and their states, i.e., mWorkListSpill, mWorkListFreeze
             and mWorkListSimplify.

      @param[in] f A reference to a %WIR function.
      @param[in] igraph A const reference to the interference graph.

      This function implements procedure MakeWorklist() from A. W. Appel, page
      252.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void makeWorkList( WIR_Function &, const WIR_InterferenceGraph & );

    /*!
      @brief decrementDegree inspects an interference graph node and moves it
             into its appropriate work lists if necessary.

      @param[in] r A const reference to a virtual register.
      @param[in] igraph A const reference to the interference graph.

      This function implements procedure DecrementDegree(m) from A. W. Appel,
      page 253.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void decrementDegree( const WIR_VirtualRegister &,
                          const WIR_InterferenceGraph & );

    /*!
      @brief addWorkList adds an interference graph node to the simplify work
             list if possible.

      @param[in] r A const reference to a register.
      @param[in] igraph A const reference to the interference graph.

      This function implements procedure AddWorkList(u) from A. W. Appel, page
      254.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void addWorkList( const WIR_BaseRegister &, const WIR_InterferenceGraph & );


    //
    // Interference graph simplification.
    //

    /*!
      @brief simplifyPriority removes a high-priority node in mWorkListPriority
             from the interference graph and pushes it onto the priority stack.

      @param[in,out] igraph A reference to the interference graph.

      This function implements procedure Simplify() from A. W. Appel, page 253.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void simplifyPriority( WIR_InterferenceGraph & );

    /*!
      @brief simplify removes a non-move-related node in mWorkListSimplify from
             the interference graph and pushes it onto the stack.

      @param[in,out] igraph A reference to the interference graph.

      One at a time, remove non-move-related nodes of low degree from the graph.

      This function implements procedure Simplify() from A. W. Appel, page 253.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void simplify( WIR_InterferenceGraph & );


    //
    // Spilling.
    //

    /*!
      @brief computeLoopNestingDepths computes the loop nesting level of all
             basic blocks of the specified function and stores the information
             in map mBBLoopDepth.

      @param[in] f A reference to a %WIR function for which loop nesting data
                   shall be computed.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void computeLoopNestingDepths( WIR_Function & );

    /*!
      @brief getLoopNestingDepth returns the loop nesting depth of the specified
             basic block.

      @param[in] b A const reference to a %WIR basic block.
      @return The block's loop nesting depth according to map mBBLoopDepth.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    unsigned int getLoopNestingDepth( const WIR_BasicBlock & ) const;

    /*!
      @brief selectSpill selects a significant-degree node for potential
             spilling and pushes it on the stack.

      @param[in] f A const reference to a WIR_Function.
      @param[in,out] igraph A reference to the interference graph.

      This function implements procedure SelectSpill() from A. W. Appel, page
      255.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void selectSpill( const WIR_Function &, WIR_InterferenceGraph & );

    /*!
      @brief computeSpillLoadCosts computes the costs for spill-loading the
             specified register at parameter p.

      @param[in] p A const reference to the %WIR register parameter where r
                   shall be spill-loaded.
      @param[in] key A const reference to the %WIR register serving as key of r
                     in the interference graph.
      @param[in] nestingDepth The loop nesting depth of p.
      @param[in,out] rematInsns A reference to a map storing potential code for
                                rematerialization for a given %WIR parameter.
      @param[in,out] movesToBeDeleted A reference to the current set of move
                                      operations that would be deleted in the
                                      course of spilling.
      @param[in] deleteMove A Boolean denoting whether the operation of p is a
                            move operation to be deleted.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void computeSpillLoadCosts( const WIR_RegisterParameter &,
                                const WIR_BaseRegister &, unsigned int,
                                WIR_RematerializationMap &, WIR_OperationSet &,
                                bool = false );

    /*!
      @brief computeSpillStoreCosts computes the costs for spill-storing the
             specified register at parameter p.

      @param[in] p A const reference to the %WIR register parameter where r
                   shall be spill-stored.
      @param[in] key A const reference to the %WIR register serving as key of r
                     in the interference graph.
      @param[in] nestingDepth The loop nesting depth of p.
      @param[in,out] movesToBeDeleted A reference to the current set of move
                                      operations that would be deleted in the
                                      course of spilling.
      @param[in] deleteMove A Boolean denoting whether the operation of p is a
                            move operation to be deleted.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void computeSpillStoreCosts( const WIR_RegisterParameter &,
                                 const WIR_BaseRegister &, unsigned int,
                                 WIR_OperationSet &, bool = false );

    /*!
      @brief propagateFalseLiveness marks that the liveness of the given
             register including all its childs and parents stops within the
             considered basic block.

      @param[in] r A const reference to a virtual %WIR register whose liveness
                   shall be updated.
      @param[in] b A const reference to a %WIR basic block.
      @param[in,out] falseLivenessInSameBB A reference to a set of virtual
                                           registers that are no more alive in
                                           b.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void propagateFalseLiveness( const WIR_VirtualRegister &,
                                 const WIR_BasicBlock &,
                                 WIR_VirtualRegisterSet & ) const;

    /*!
      @brief propagateTrueLiveness marks that the liveness of the given register
             including all its childs starts within the considered basic block.

      @param[in] r A const reference to a virtual %WIR register whose liveness
                   shall be updated.
      @param[in] b A const reference to a %WIR basic block.
      @param[in,out] trueLivenessInSameBB A reference to a set of virtual
                                          registers that are alive in b.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void propagateTrueLiveness( const WIR_VirtualRegister &,
                                const WIR_BasicBlock &,
                                WIR_VirtualRegisterSet & ) const;

    /*!
      @brief updateLivenessInSameBB updates partial basic block-level liveness
             information in the course of spill cost computation.

      @param[in] r A const reference to a virtual %WIR register whose liveness
                   shall be updated.
      @param[in] live A Boolean denoting whether register r is live or not.
      @param[in,out] stillLiveInSameBB A reference to the map containing partial
                                       basic block-level liveness information.
      @param[in] igraph A const reference to the interference graph.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void updateLivenessInSameBB( const WIR_VirtualRegister &, bool,
                                 std::map<WIR_id_t, bool> &,
                                 const WIR_InterferenceGraph & ) const;


    //
    // Coalescing.
    //

    /*!
      @brief isCoalescableMove checks whether a given move operation could
             potentially be coalesced, i. e., it checks whether all register
             parameters of the operation are represented by interference graph
             nodes and whether the involved registers are different.

      @param[in] o A const reference to a move operation to be examined.
      @param[in] igraph A const reference to the interference graph.
      @return true iff all register parameters of the specified operation are
              represented as nodes in the specified interference graph and the
              involved graph nodes are different, false otherwise.

      This check is required while initializing the register allocator's data
      structures, since the user of this class can externally specify, which
      physical registers to consider during register allocation (see method
      createPhregs). If, for some reasons, the user wishes not to use a certain
      physical register during allocation, but a %WIR function contains a move
      operation where this physical register is involved, we must make sure that
      this move operation is not considered during coalescing, since not all
      involved operands of the move operation are represented by interference
      graph nodes.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isCoalescableMove( const WIR_Operation &,
                            const WIR_InterferenceGraph & ) const;

    /*!
      @brief isMoveRelated returns whether a given register is move-related.

      @param[in] r A const reference to a virtual register.
      @return true iff the register is move-related, false otherwise.

      This function implements function MoveRelated(n) from A. W. Appel, page
      253.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isMoveRelated( const WIR_BaseRegister & ) const;

    /*!
      @brief nodeMoves returns the set of move operations associated with the
             given register, that are either enabled or not yet ready for
             coalescing.

      @param[in] r A const reference to a virtual register.
      @return The set of move operations as specified above.

      This function implements function NodeMoves(n) from A. W. Appel, page 253.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_OperationSet nodeMoves( const WIR_BaseRegister & ) const;

    /*!
      @brief For a given interference graph node, freezeMoves freezes all moves
             in which this node is involved.

      @param[in] r A const reference to the virtual register to be freezed.
      @param[in,out] igraph A reference to the interference graph.

      That is, we give up hope of coalescing these moves. This causes the node
      (and perhaps other nodes related to the frozen moves) to be considered
      non-move-related, which should enable more simplification.

      This function implements procedure FreezeMoves(u) from A. W. Appel, page
      255.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void freezeMoves( const WIR_VirtualRegister &, WIR_InterferenceGraph & );

    /*!
      @brief enableMoves inspects an interference graph node and its neighbors,
             checks whether they are move-related and enables them for
             coalescing.

      @param[in] r A const reference to a register.
      @param[in] igraph A reference to the interference graph.

      This function implements procedure EnableMoves(nodes) from A. W. Appel,
      page 253.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void enableMoves( const WIR_BaseRegister &, const WIR_InterferenceGraph & );

    /*!
      @brief coalesce performs conservative coalescing on the reduced graph
             obtained from simplify().

      @param[in,out] igraph A reference to the interference graph.

      Perform conservative coalescing on the reduced graph obtained in the
      simplification phase. Since the degrees of many nodes have been reduced by
      simplify, the conservative strategy is likely to find many more moves to
      coalesce than it would have in the initial interference graph. After two
      nodes have been coalesced, if the resulting node is no longer move-related
      it will be available for the next round of simplification. simplify and
      coalesce are repeated until only significant-degree or move-related nodes
      remain.

      This function implements procedure Coalesce() from A. W. Appel, page 254.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void coalesce( WIR_InterferenceGraph & );

    /*!
      @brief areConstrainedHierarchicalRegs checks whether the two given
             registers which are both move-related must not be coalesced due to
             incompatible positions in complex register hierarchies.

      @param[in] r1 A const reference to the first register to be checked.
      @param[in] r2 A const reference to the second register to be checked.
      @param[in] igraph A const reference to the interference graph.
      @return true iff the specified registers are virtual hierarchical
              registers which are in 'inequivalent' positions in their
              respective register hierarchies so that it is illegal to coalesce
              them.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool areConstrainedHierarchicalRegs( const WIR_BaseRegister &,
                                         const WIR_BaseRegister &,
                                         const WIR_InterferenceGraph & ) const;

    /*!
      @brief areIncompatiblyColoredNodes checks whether the two given registers
             which are both move-related must not be coalesced due to an
             incompatible coloring resulting from this coalescence.

      @param[in] r1 A const referenc to the first register to be checked.
      @param[in] r2 A const reference to the second register to be checked.
      @param[in] igraph A const reference to the interference graph.
      @return true iff coalescing r1 and r2 yields an edge whose two nodes share
              at least one common color.

      Suppose, a node u is to be coalesced into some other node v. During this
      coalescing, all edges between u and its neighbors n are re-directed from u
      to v. I.e., an edge (u, n) becomes (v, n) afterwards. In the case that v
      and n use some common color due to pre-coloring, then this coalescence
      violates the graph coloring property that no two adjacent nodes may have
      the same color.

      Another situation for an incompatible pre-coloring is: The vanishing node
      propagates its colors to the remaining node during coalescing. However,
      the remaining node is already adjacent to another neighbor having the same
      colors as the vanishing node would induce. In order to detect this
      scenario, we first add all colors induced somehow by the vanishing node to
      set colorsOfRemainingNode. After that, we check all neighbors of the
      remaining node and verify that the intersection of the neighbor's color
      set with colorsOfRemainingNode is empty.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool areIncompatiblyColoredNodes( const WIR_BaseRegister &,
                                      const WIR_BaseRegister &,
                                      const WIR_InterferenceGraph & ) const;

    /*!
      @brief coalescingTestBriggs tests whether conservative coalescing of the
             two given registers according to Briggs is applicable.

      @param[in] r1 A const reference to the first register to be coalesced.
      @param[in] r2 A const reference to the second register to be coalesced.
      @param[in] igraph A const reference to the interference graph.
      @return true iff the coalesced node (u/v) has less than K neighbors with
              high degree.

      This function implements function Conservative(nodes) from A. W. Appel,
      page 254.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool coalescingTestBriggs( const WIR_BaseRegister &,
                               const WIR_BaseRegister &,
                               const WIR_InterferenceGraph & ) const;

    /*!
      @brief coalescingTestGeorge tests whether conservative coalescing of the
             two given registers according to George is applicable.

      @param[in] r1 A const reference to the first register to be coalesced.
      @param[in] r2 A const reference to the second register to be coalesced.
      @param[in] igraph A const reference to the interference graph.
      @return true iff, for each neighbor w of v, w has low degree or w
              interferes with u.

      This function implements function OK(t,r) from A. W. Appel, page 254.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool coalescingTestGeorge( const WIR_BaseRegister &,
                               const WIR_BaseRegister &,
                               const WIR_InterferenceGraph & ) const;

    /*!
      @brief combine coalesces the two given interference graph nodes and
             updates the work lists.

      @param[in] r1 A const reference to the first register to be coalesced.
      @param[in] r2 A const reference to the second register to be coalesced.
      @param[in] igraph A const reference to the interference graph.
      @return The register remaining as node in the interference graph after
              coalescing.

      This function implements procedure Combine(u,v) from A. W. Appel, page
      255.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_BaseRegister &combine( const WIR_BaseRegister &,
                               const WIR_BaseRegister &,
                               WIR_InterferenceGraph & );

    /*!
      @brief freeze takes an interference graph node and freezes all moves in
             which this node is involved.

      @param[in] igraph A reference to the interference graph.

      If neither simplify nor coalesce applies, we look for a move-related node
      of low degree. We freeze the moves in which this node is involved: that
      is, we give up hope of coalescing these moves. This causes the node (and
      perhaps other nodes related to the frozen moves) to be considered
      non-move-related, which should enable more simplification. Now, simplify
      and coalesce are resumed.

      This function implements procedure Freeze() from A. W. Appel, page 255.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void freeze( WIR_InterferenceGraph & );


    //
    // Coloring.
    //

    /*!
      @brief assignColors finally assigns colors to the interference graph
             nodes.

      @param[in,out] igraph A reference to the interference graph.

      This function implements procedure AssignColors() from A. W, Appel, page
      256.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void assignColors( WIR_InterferenceGraph & );


    //
    // Code transformation.
    //

    /*!
      @brief isSpillRegister checks whether the specified register is one that
             has been generated artificially for spilling.

      @param[in] r A const reference to a virtual register to be examined.
      @return true iff the given register is used for spilling.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isSpillRegister( const WIR_VirtualRegister & ) const;

    /*!
      @brief rewriteProgram transforms the specified %WIR function according to
             the decisions taken by the register allocator.

      @param[in,out] f A reference to a %WIR function.

      Basically, rewriteProgram transforms registers according to the found
      coloring, coalesces moves and inserts spill code.

      This function implements procedure RewriteProgram() from A. W. Appel, page
      256.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void rewriteProgram( WIR_Function & );

    /*!
      @brief For each actual spill in mSpilledNodes, computeStackLocations
             computes its relative place in the specified function's stack frame
             and stores it in mStackByteOffset.

      @param[in] f A const reference to a %WIR function.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void computeStackLocations( const WIR_Function & );

    /*!
      @brief replaceCoalescedSpills scans the specified %WIR function and
             replaces all occurrences of spilled coalesced virtual registers in
             mSpillAliases by their unaliased registers.

      @param[in,out] f A reference to a %WIR function.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void replaceCoalescedSpills( WIR_Function & );

    /*!
      @brief insertSpillCode inserts spill code into the %WIR for all actual
             spills in mSpilledNodes.

      @param[in,out] f A reference to a %WIR function.

      The structure of insertSpillCode corresponds to that of computeSpillCosts
      for obvious reasons.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertSpillCode( WIR_Function & );

    /*!
      @brief insertRematerializationCode inserts code to rematerialize a
             register into the %WIR.

      @param[in] clone A const reference to a cloned register created during
                       spilling-related live range splitting. This cloned
                       register will be used by the actually generated
                       rematerialization instructions.
      @param[in] r A const reference to the original, un-cloned %WIR register.
      @param[in] rp A reference to the %WIR register parameter that shall be
                    rematerialized.
      @param[in] b A reference to a %WIR basic block in which to insert the
                   generated rematerialization code.
      @param[in] pos A const iterator refering to the position before which the
                     generated rematerialization code will be inserted.

      insertRematerializationCode is responsible to add all generated
      rematerialization instructions to map mRematerializedSpills.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertRematerializationCode( const WIR_BaseRegister &,
                                      const WIR_VirtualRegister &,
                                      WIR_RegisterParameter &,
                                      WIR_BasicBlock &,
                                      std::list<std::reference_wrapper<WIR_Instruction>>::const_iterator );

    /*!
      @brief removeCoalescedMoves removes all operations in set mCoalescedMoves
             from the %WIR code.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void removeCoalescedMoves( void );

    /*!
      @brief replaceColoredRegisters scans the specified %WIR function and
             replaces all occurrences of virtual registers in mRegisterMapping
             by their associated physical registers.

      @param[in,out] f A reference to a %WIR function.

      Besides replacing all occurrences of colored virtual registers by their
      physical counterparts, replaceColoredRegisters also deletes unnecessary
      pre-color information from f.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void replaceColoredRegisters( WIR_Function & );

    /*!
      @brief allocateUncoloredActualSpills processes all registers in
             mUncoloredActualSpills and assigns them to physical registers.

      @param[in,out] f A reference to a %WIR function.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void allocateUncoloredActualSpills( WIR_Function & );

    /*!
      @brief removeRedundantSpills removes unnecessary spill code from a %WIR
             function.

      In particular, spill-stores for which no spill-loads exist (due to
      rematerialization) are removed, and immediately following combinations of
      load/store, store/load, load/load and store/store refering to the very
      same register are removed.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void removeRedundantSpills( void );


    //
    // Attributes.
    //

    //! mVerbosity stores whether verbose messages should be dumped.
    bool mVerbosity;

    /*!
      @brief mCoalescingBriggs stores whether coalescing according to Briggs
             shall be applied.
    */
    bool mCoalescingBriggs;

    /*!
      @brief mCoalescingGeorge stores whether coalescing according to George
             shall be applied.
    */
    bool mCoalescingGeorge;

    //! mGenerateWebs stores whether virtual registers shall be split into webs.
    bool mGenerateWebs;

    /*!
      @brief mBestOfThreeSpilling stores whether the best-of-three spilling
             heuristic proposed by Bernstein shall be applied.
    */
    bool mBestOfThreeSpilling;

    //! mRematerialization stores whether rematerialization shall be applied.
    bool mRematerialization;

    /*!
      @brief mCoalesceStackLocations stores whether coalescing of actual spills
             shall be done in order to reduce the size of the required stack
             frame.
    */
    bool mCoalesceStackLocations;

    /*!
      @brief mSpillAll stores whether all virtual registers should be turned
             into actual spills, instead of performing some intelligent register
             allocation.
    */
    bool mSpillAll;

    /*!
      @brief mMarkSpillCode stores whether all generated spill and
             rematerialization instructions shall be marked by dedicated
             comments.
    */
    bool mMarkSpillCode;

    /*!
      @brief mUncoloredActualSpills holds all registers cloned during live range
             splitting of actual spills, and for which the algorithm failed to
             select a feasible color.
    */
    WIR_VirtualRegisterSet mUncoloredActualSpills;

    //! mBBLoopDepth maps a basic block to its loop nesting depth.
    std::map<std::reference_wrapper<WIR_BasicBlock>, unsigned int,
             WIR_Compare<WIR_BasicBlock>> mBBLoopDepth;

    /*!
      @brief mSpillHeuristicToApply stores which spill heuristic shall be
             applied by the central simplification loop in method
             runOptimization( WIR_Function & ).

      Currently, the following values for mSpillHeuristicToApply are supported:

      - 0 = Chaitin's classical spilling heuristic.
      - 1 = Bernstein's first spilling heuristic.
      - 2 = Bernstein's second spilling heuristic.
      - 3 = Bernstein's third spilling heuristic.
    */
    unsigned int mSpillHeuristicToApply;

    /*!
      @brief mMinimalTotalSpillCosts temporarily stores value mTotalSpillCosts
             during Bernstein's best-of-three spilling.
    */
    unsigned long long mMinimalTotalSpillCosts;

    /*!
      @brief mBestSpillHeuristic temporarily stores value mSpillHeuristicToApply
             during Bernstein's best-of-three spilling.
    */
    unsigned int mBestSpillHeuristic;

    /*!
      @brief mBestSpilledNodes temporarily stores set mSpilledNodes during
             Bernstein's best-of-three spilling.
    */
    WIR_VirtualRegisterSet mBestSpilledNodes;

    /*!
      @brief mBestCoalescedMoves temporarily stores set mCoalescedMoves above
             during Bernstein's best-of-three spilling.
    */
    WIR_OperationSet mBestCoalescedMoves;

    /*!
      @brief mBestRegisterMapping temporarily stores map mRegisterMapping during
             Bernstein's best-of-three spilling.
    */
    std::map<WIR_id_t, std::reference_wrapper<WIR_PhysicalRegister>> mBestRegisterMapping;

    /*!
      @brief mBestSpillAliases temporarily stores map mSpillAliases during
             Bernstein's best-of-three spilling.
    */
    std::map<WIR_id_t, std::reference_wrapper<WIR_BaseRegister>> mBestSpillAliases;

    /*!
      @brief mBestRematerializationInstructions temporarily stores map
             mRematerializationInstructions during Bernstein's best-of-three
             spilling.
    */
    WIR_RematerializationMap mBestRematerializationInstructions;

    /*!
      @brief mBestIgraph stores the currently best interference graph determined
             by method optimize.
    */
    WIR_InterferenceGraph mBestIgraph;

    /*!
      @brief mTotalSpillCosts accumulates the spill costs realized by all
             spilled registers.
    */
    unsigned long long mTotalSpillCosts;

    //! mActiveMoves contains all moves not yet ready for coalescing.
    WIR_OperationSet mActiveMoves;

    //! mCoalescedMoves contains all moves that have been coalesced.
    WIR_OperationSet mCoalescedMoves;

    //! mConstrainedMoves contains all moves whose source and target interfere.
    WIR_OperationSet mConstrainedMoves;

    /*!
      @brief mFrozenMoves contains all moves that will no longer be considered
             for coalescing.
    */
    WIR_OperationSet mFrozenMoves;

    //! mWorkListMoves contains all moves that might be coalesceable.
    WIR_OperationSet mWorkListMoves;

    //! mWorkListFreeze contains all low-degree move-related nodes.
    WIR_VirtualRegisterSet mWorkListFreeze;

    /*!
      @brief mWorkListPriority contains all high-priority nodes that shall be
             colored at first so that they are likely not to be spilled.
    */
    WIR_VirtualRegisterSet mWorkListPriority;

    //! mWorkListSimplify contains all low-degree non-move-related nodes.
    WIR_VirtualRegisterSet mWorkListSimplify;

    //! mWorkListSpill contains all high-degree nodes.
    WIR_VirtualRegisterSet mWorkListSpill;

    /*!
      @brief mMoveList maps a register ID (i. e., an interference graph node) to
             its associated list of move operations.
    */
    std::map<WIR_id_t, WIR_OperationSet> mMoveList;

    /*!
      @brief mRematerializationInstructions stores a list of machine
             instructions that will be inserted if a given %WIR register
             parameter is used for rematerialization.
    */
    WIR_RematerializationMap mRematerializationInstructions;

    /*!
      @brief mRegisterMapping maps each individual virtual register ID
             (irrespective of root, leaf or intermediate register in a
             hierarchy) to its allocated physical register.
    */
    std::map<WIR_id_t, std::reference_wrapper<WIR_PhysicalRegister>> mRegisterMapping;

    /*!
      @brief mSpillAliases maps the ID of a register that is coalesced into an
             actual spill to this actually spilled, unaliased, register.
    */
    std::map<WIR_id_t, std::reference_wrapper<WIR_BaseRegister>> mSpillAliases;

    /*!
      @brief mSpilledNodes contains all nodes marked for spilling during the
             current coloring round of optimize.
    */
    WIR_VirtualRegisterSet mSpilledNodes;

    /*!
      @brief mStackByteOffset stores the relative byte offset of each actually
             spilled register on the stack.
    */
    std::map<WIR_id_t, int> mStackByteOffset;

};

}       // namespace WIR

#endif  // _WIR_GRAPHCOLORING_H
