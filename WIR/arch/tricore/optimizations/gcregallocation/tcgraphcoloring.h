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
  @file tcgraphcoloring.h
  @brief This file provides the basic interface of a TriCore-specific
         graph-coloring based register allocator.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _TC_GRAPHCOLORING_H
#define _TC_GRAPHCOLORING_H


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
#include <optimizations/gcregallocation/wirgraphcoloring.h>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_BaseRegister;
class WIR_BasicBlock;
class WIR_Function;
class WIR_Instruction;
class WIR_InterferenceGraph;
class WIR_Operation;
class WIR_Parameter;
class WIR_PhysicalRegister;
class WIR_RegisterParameter;
class WIR_UpDownValue;
class WIR_VirtualRegister;


/*!
  @brief Class TC_GraphColoring performs graph coloring-based register
         allocation for the TriCore architecture.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class TC_GraphColoring : public WIR_GraphColoring
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor for system-level optimization.

      @param[in] s A reference to a WIR_System to be optimized.
      @param[in] verbosity A Boolean defaulting to false that denotes whether
                           verbose messages shall be dumped or not.
      @param[in] uc A Boolean defaulting to false that denotes whether the
                    register allocator shall only use physical registers from
                    the TriCore's upper context or not.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC_GraphColoring( WIR_System &, bool = false, bool = false );

    /*!
      @brief Default constructor for compilation unit-level optimization.

      @param[in] c A reference to a WIR_CompilationUnit to be optimized.
      @param[in] verbosity A Boolean defaulting to false that denotes whether
                           verbose messages shall be dumped or not.
      @param[in] uc A Boolean defaulting to false that denotes whether the
                    register allocator shall only use physical registers from
                    the TriCore's upper context or not.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC_GraphColoring( WIR_CompilationUnit &, bool = false, bool = false );

    /*!
      @brief Default constructor for function-level optimization.

      @param[in] f A reference to a WIR_Function to be optimized.
      @param[in] verbosity A Boolean defaulting to false that denotes whether
                           verbose messages shall be dumped or not.
      @param[in] uc A Boolean defaulting to false that denotes whether the
                    register allocator shall only use physical registers from
                    the TriCore's upper context or not.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC_GraphColoring( WIR_Function &, bool = false, bool = false );

    /*!
      @brief Destructor.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~TC_GraphColoring( void );


    //
    // Configuration switches.
    //

    /*!
      @brief setUseOnlyUC (de-) activates whether the register allocator should
             only use physical registers from the TriCore's upper context, or
             not.

      @param[in] uc A Boolean defaulting to true that specifies whether to use
                    the upper context only, or not.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setUseOnlyUC( bool = true );

    /*!
      @brief getUseOnlyUC returns whether the register allocator should only use
             physical registers from the TriCore's upper context, or not.

      @return true if only physical registers from the TriCore's upper context
              are used, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool getUseOnlyUC( void ) const;


  protected:

    //
    // Methods for optimization management.
    //

    /*!
      @brief runOptimization allocates registers in the given function.

      @param[in] f A reference to a WIR_Function to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void runOptimization( WIR_Function & );

    /*!
      @brief saveBestSolutionHook allows to save processor-specific allocation
             data in the course of Bernstein's best-of-three spilling heuristic.

      Here, saveBestSolutionHook is used to save TriCore-specific information on
      lower-context registers alive across function calls.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void saveBestSolutionHook( void );

    /*!
      @brief restoreBestSolutionHook allows to restore processor-specific
             allocation data in the course of Bernstein's best-of-three spilling
             heuristic.

      Here, restoreBestSolutionHook is used to restore TriCore-specific
      information on lower-context registers alive across function calls.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void restoreBestSolutionHook( void );


    //
    // Methods for TriCore-specific physical registers.
    //

    /*!
      @brief createPhregs sets up the lists mPhregs and
             mPhregsForPrecoloringOnly of all TriCore-specific physical
             registers contained in the specified %WIR function.

      @param[in] f A reference to a %WIR function.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void createPhregs( WIR_Function & );

    /*!
      @brief initializationHook allows to perform processor-specific actions
             before doing some actual coloring or spilling.

      @param[in] f A reference to a %WIR function.

      Here, initializationHook is used to perform the bit-true data flow
      analysis for TriCore before jumping into the actual graph coloring.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void initializationHook( WIR_Function & );

    /*!
      @brief isStackPointer returns whether the specified %WIR register is the
             TriCore's stack pointer.

      @param[in] r A const reference to a %WIR register to be examined.
      @return True iff the specified register if the stack pointer, false
              otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool isStackPointer( const WIR_BaseRegister & ) const;

    /*!
      @brief checkLCRegsAliveAcrossCall checks whether the two specified virtual
             leaf registers are mapped to a lower-context physical register and
             whether they are alive across a function call.

      @param[in] phreg A const reference to a physical TriCore register.
      @param[in] vregs_c1 A const reference to a set of registers denoting the
                          first virtual leaf register to be checked, including
                          its coalescing aliases.
      @param[in] vregs_c2 A const reference to a set of registers denoting the
                          second virtual leaf register to be checked, including
                          its coalescing aliases.

      If so, the respective function call and the physical register are stored
      in mLCRegsAliveAcrossCall.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void checkLCRegsAliveAcrossCall( const WIR_PhysicalRegister &,
                                     const WIR_RegisterSet &,
                                     const WIR_RegisterSet & );


    //
    // Interference graph construction.
    //

    /*!
      @brief buildProcessorSpecificInterferences adds edges to the interference
             graph expressing TriCore-specific interferences.

      @param[in] f A reference to a %WIR function.
      @param[in,out] igraph A reference to the interference graph.

      Furthermore, buildProcessorSpecificInterferences checks which virtual
      registers are live-out across function calls inside f.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void buildProcessorSpecificInterferences( WIR_Function &,
                                                      WIR_InterferenceGraph & );

    /*!
      @brief isFunctionReturnMove checks whether the move operation stores the
             result of a function call somewhere.

      @param[in] o A const reference to a move operation.
      @return isFunctionReturnMove returns true iff
              - the register used by the move is D2, D3 or E3, and
              - the move immediately follows a call instruction.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isFunctionReturnMove( const WIR_Operation & ) const;


    //
    // Spilling.
    //

    /*!
      @brief isPriorityRegister returns whether a given register has high
             priority for color assignment.

      @param[in] r A const reference to a virtual register.
      @return true iff the register has high priority, false otherwise.

      For the TriCore register allocator, high-priority registers are kept in
      set mHighPriorityRegs. Thus, this method only checks whether r is in
      mHighPriorityRegs or not.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool isPriorityRegister( const WIR_VirtualRegister & ) const;

    /*!
      @brief getSpillLoadCosts returns the TriCore-specific costs of one single
             spill-load for the specified register parameter.

      @param[in] p A const reference to a %WIR register parameter.
      @return Some TriCore-specific cost measure for a spill-load of register
              parameter p.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual unsigned int getSpillLoadCosts( const WIR_RegisterParameter & ) const;

    /*!
      @brief getSpillStoreCosts returns the TriCore-specific costs of one single
             spill-store for the specified register parameter.

      @param[in] p A const reference to a %WIR register parameter.
      @return Some TriCore-specific cost measure for a spill-store of register
              parameter p.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual unsigned int getSpillStoreCosts( const WIR_RegisterParameter & ) const;

    /*!
      @brief getMoveCosts returns the TriCore-specific costs of one single move
             operation that can be omitted due to spilling.

      @param[in] o A const reference to a move operation.
      @return Some TriCore-specific cost measure for a move operation.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual unsigned int getMoveCosts( const WIR_Operation & ) const;


    //
    // Coalescing.
    //

    /*!
      @brief getUseOfMove returns the used register of the specified TriCore
             %WIR move operation.

      @param[in] o A const reference to a move operation.
      @return A reference to the %WIR register used by the move operation.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_BaseRegister &getUseOfMove( const WIR_Operation & ) const;

    /*!
      @brief getDefOfMove returns the defined register of the specified TriCore
             %WIR move operation.

      @param[in] o A const reference to a move operation.
      @return A reference to the %WIR register defined by the move operation.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_BaseRegister &getDefOfMove( const WIR_Operation & ) const;

    /*!
      @brief avoidCoalescing checks whether the two given registers which are
             both move-related must not be coalesced due to TriCore-specific
             reasons.

      @param[in] o A const reference to a move operation.
      @param[in] r1 A const reference to the first register to be checked.
      @param[in] r2 A const reference to the second register to be checked.
      @param[in] igraph A const reference to the interference graph.
      @return true iff coalescing r1 and r2 must be avoided.

      Coalescing will be avoided if
      - the register used by the move is D2, D3 or E3, and
      - the move immediately follows a call instruction.

      or if
      - one of the two registers is pre-colored and the other not,
      - the pre-colored register is a lower-context register, and
      - the non pre-colored register is alive across a function call,

      In the first case, the move is used to store away a result of a function
      call. We keep these explicit moves since otherwise, it would cause us
      trouble when implementing the caller-saving/restoring of LC registers
      later.

      If we would allow coalescing in the second case, the non pre-colored
      register will be allocated to the lower-context register specified in the
      pre-color. However, since it is alive across a function call,
      caller-saving stack loads and stores must be added which is bad. Instead
      of inserting these costly memory loads and stores, we simply keep the move
      instruction and do not coalesce.

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
      @brief getRematerializationCosts returns the TriCore-specific costs of one
             single recomputation of the specified used parameter.

      @param[in] p A const reference to a %WIR register parameter.
      @return Some TriCore-specific cost measure for a rematerialization of the
              register in used in p.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual unsigned int getRematerializationCosts( const WIR_RegisterParameter & ) const;

    /*!
      @brief getRematerializationConstant returns a constant integer value that
             is equivalent to the specified %WIR parameter.

      @param[in] p A const reference to a %WIR register parameter.
      @return A constant value c if p boils down to c according to data flow
              analysis, maxint otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    int getRematerializationConstant( const WIR_RegisterParameter & ) const;

    /*!
      @brief getRematerializationInstructions returns a list of TriCore
             instructions for one single recomputation of the specified used
             parameter.

      @param[in] p A const reference to a %WIR register parameter.
      @return Some TriCore-specific machine instructions for the
              rematerialization of the register in used in p.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual std::list<WIR_Instruction *> getRematerializationInstructions( const WIR_RegisterParameter & ) const;


    //
    // Coloring.
    //

    /*!
      @brief selectColors assigns actual colors to the TriCore leaf registers in
             the specified vector.

      @param[in] leafs A const reference to a vector containing all leaf
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

      This method checks whether the leafs to be processed are TriCore data or
      address registers, and whether they are simple or extended registers. It
      first tries to select the implicit registers D15 and A15, since this is
      beneficial for the generation of 16-bit instructions. If D15/A15 are not
      available, other data/address registers from the TriCore's upper context
      are checked next. This is done, because upper context registers are
      automatically saved across function calls without any additional context
      saving code. If the entire upper context is not available, the registers
      of the lower context are finally checked.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_GraphColoring::WIR_ColorMap selectColors( const std::vector<std::reference_wrapper<WIR_VirtualRegister>> &,
                                                          const WIR_InterferenceGraph & );


    //
    // Code transformation.
    //

    /*!
      @brief isSpillStore checks whether the specified instruction spill-stores
             a certain virtual register in the TriCore ISA.

      @param[in] i A const reference to a %WIR instruction.
      @param[in] r A const reference to a virtual register to be examined.
      @return true iff the given instruction is a spill-store.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool isSpillStore( const WIR_Instruction &,
                               const WIR_VirtualRegister & ) const;

    /*!
      @brief isSpillLoad checks whether the specified instruction spill-loads a
             certain virtual register in the TriCore ISA.

      @param[in] i A const reference to a %WIR instruction.
      @param[in] r A const reference to a virtual register to be examined.
      @return true iff the given instruction is a spill-load.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool isSpillLoad( const WIR_Instruction &,
                              const WIR_VirtualRegister & ) const;

    /*!
      @brief resolveSpillCoalescingConflict resolves a conflict when two
             registers with different positions in a register hierarchy shall be
             coalesced during computeStackLocations.

      @param[in] r1 A const reference to a first %WIR virtual register to be
                    coalesced.
      @param[in] r2 A const reference to a second %WIR register to be coalesced.
      @return A pair of registers that have the same number of leafs in their
              respective hierarchies and that can thus be used for coalescing.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual std::pair<std::reference_wrapper<WIR_VirtualRegister>,
                      std::reference_wrapper<WIR_VirtualRegister>> resolveSpillCoalescingConflict( const WIR_VirtualRegister &,
                                                                                                   const WIR_VirtualRegister & ) const;

    /*!
      @brief getStackPosOfSubReg returns the stack position of some child
             register, if the root of the entire register hierarchy is located
             in the specified stack position.

      @param[in] r A const reference to a virtual %WIR child register.
      @param[in] rootPos The position of r's root register on the stack.
      @return The according stack position of r on the stack.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual unsigned int getStackPosOfSubReg( const WIR_VirtualRegister &,
                                              unsigned int ) const;

    /*!
      @brief insertSpillLoad inserts TriCore code for a spill-load of a register
             into the %WIR.

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

      insertSpillLoad is responsible to add all generated spill-load
      instructions to map mSpillLoads.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void insertSpillLoad( const WIR_BaseRegister &,
                                  const WIR_BaseRegister &,
                                  int, WIR_BasicBlock &,
                                  std::list<std::reference_wrapper<WIR_Instruction>>::const_iterator );

    /*!
      @brief insertSpillStore inserts TriCore code for a spill-store of a
             register into the %WIR.

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

      insertSpillStore is responsible to add all generated spill-store
      instructions to map mSpillStores.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void insertSpillStore( const WIR_BaseRegister &,
                                   const WIR_BaseRegister &,
                                   int, WIR_BasicBlock &,
                                   std::list<std::reference_wrapper<WIR_Instruction>>::const_iterator );

    /*!
      @brief insertSpillCode inserts TriCore code for a spill-load or
             spill-store of a register into the %WIR.

      @param[in] clone A const reference to a cloned register to be spilled.
      @param[in] r A const reference to the original, un-cloned %WIR register.
      @param[in] stackPos The stack position to/from which the register is
                          spilled.
      @param[in] b A reference to a %WIR basic block in which to insert the
                   generated spill code.
      @param[in] pos A const iterator refering to the position before which the
                     generated spill code will be inserted.
      @param[in] spillStore A Boolean switch that controls whether a spill-store
                            (true) or a spill-load (false) is generated.
      @return A reference to an eventually newly inserted basic block, or a
              reference to b if no new basic block was created.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_BasicBlock &insertSpillCode( const WIR_BaseRegister &,
                                             const WIR_BaseRegister &,
                                             int, WIR_BasicBlock &,
                                             std::list<std::reference_wrapper<WIR_Instruction>>::const_iterator,
                                             bool );

    /*!
      @brief isAdjustedLoadOrStoreInstruction returns whether the given iterator
             refers to a load or a store instruction with a stack pointer
             relative addressing mode which is surrounded by LEA instructions
             that adjust the stack pointer.

      @param[in] b A const reference to a %WIR basic block whose instructions
                   shall be checked.
      @param[in] pos A const iterator refering to the position of an instruction
                     to be checked.
      @return true iff pos refers to an adjusted stack access, false otherwise.

      isAdjustedLoadOrStoreInstruction checks if the code looks like

        lea    A10, A10, const1             # with const1 > 0
        ld__   __, [A10] __                 # pos
        lea    A10, A10, -const2            # with const2 > 0

      or

        lea    A10, A10, const1             # with const1 > 0
        st__   [A10] __, __                 # pos
        lea    A10, A10, -const2            # with const2 > 0

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isAdjustedLoadOrStoreInstruction( const WIR_BasicBlock &,
                                           std::list<std::reference_wrapper<WIR_Instruction>>::const_iterator ) const;

    /*!
      @brief rewriteProgramHook allows to perform TriCore-specific actions after
             having transformed the %WIR code.

      @param[in,out] f A reference to a %WIR function.

      Here, rewriteProgramHook is used to realize handling of dummy parameters
      that need to be attached to CALL instruction in certain cases (see also
      the lenghty comment inside method checkLCRegsAliveAcrossCall).

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void rewriteProgramHook( WIR_Function & );

    /*!
      @brief getCandidatePhregs returns a set of physical registers that could
             be used for the specified virtual register according to the
             TriCore's ISA.

      @param[in] r A const reference to a virtual %WIR register.
      @return A set of physical TriCore registers.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_PhysicalRegisterSet getCandidatePhregs( const WIR_VirtualRegister & );

    /*!
      @brief getCandidatePhreg returns one element from the specified set of
             registers that will finally be used within
             allocateUncoloredActualSpills for spilling.

      @param[in] candidates A const reference to a set of potential candidate
                            phregs for spilling.
      @return A reference to one of the registers in set candidates.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual const WIR_PhysicalRegister &getCandidatePhreg( const WIR_PhysicalRegisterSet & );

    /*!
      @brief postProcessingHook allows to perform TriCore-specific actions after
             having done register allocation for a function, using e.g., the set
             of inserted spill operations mInsertedSpillCode.

      @param[in,out] f A reference to a %WIR function.

      Here, postProcessingHook is used to realize the TriCore-specific calling
      conventions afterwards.
    */
    virtual void postProcessingHook( WIR_Function & );

    /*!
      @brief adjustStack allocates additional space in the specified function's
             stack frame and adjusts all stack-related memory accesses
             accordingly.

      @param[in,out] f A reference to a %WIR function.

      According to the TriCore EABI (section 2.2.2.1), the stack pointer points
      to the bottom (low address) of the stack frame. The stack pointer
      alignment is 8 bytes. The argument overflow area for outgoing arguments
      must be located at the bottom (low address end) of the frame, with the
      first overflow argument at zero offset from the stack pointer:

      @verbatim
      (Stack
       growing
       direction)
           |
           |   +-------------------------+      (high address)
           |   | Local Variables Frame 1 |
           |   +-------------------------+
           |   | Argument Overflow Area, |
           |   | Function 2 Arguments    |      (first argument passed on stack)
           |   +-------------------------+
           |   | Local Variables Frame 2 |
           |   +-------------------------+
           |   | Argument Overflow Area, |
           |   | Function 3 Arguments    |
           |   +-------------------------+ <--- Stack Pointer (SP) at entry
           V   | Local Variables Frame 3 |      (CALL) to Function 3
               +-------------------------+
               | Argument Overflow Area  |
               +-------------------------+      (low address)
      @endverbatim

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void adjustStack( WIR_Function & );

    /*!
      @brief postRACleanup allows to perform very final TriCore-specific cleanup
             actions, particularly after stack frame reorganization.

      @param[in] f A reference to a %WIR function.

      Here, postRACleanup is used to remove redundant MOV and SWAP.W
      instructions. Furthermore, don't optimize flags of parameters (indicating
      accesses to the TriCore's argument overflow stack region) are reset.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void postRACleanup( WIR_Function & );

    /*!
      @brief For a list of instructions implementing one spill-load or -store,
             getPhregOfSpill determines that physical register that is actually
             spill-loaded or -stored.

      @param[in] spill A const reference to a list of spill instructions.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_PhysicalRegister &getPhregOfSpill( const std::list<std::reference_wrapper<WIR_Instruction>> & );


    //
    // Attributes.
    //

    /*!
      @brief mLCRegsAliveAcrossCall provides a set of lower-context registers
             that are live-out at a given call instruction.
    */
    std::map<std::reference_wrapper<WIR_Instruction>, WIR_RegisterSet, WIR_Compare<WIR_Instruction>> mLCRegsAliveAcrossCall;

    /*!
      @brief mDummyParameters stores all dummy parameters artificially attached
             to call instructions in order to maintain correct liveness
             information.
    */
    std::list<std::reference_wrapper<WIR_Parameter>> mDummyParameters;

    /*!
      @brief mVregsAliveAcrossCall provides a set of virtual registers that are
             live-out at a given call instruction.
    */
    std::map<std::reference_wrapper<WIR_Instruction>, WIR_VirtualRegisterSet, WIR_Compare<WIR_Instruction>> mVregsAliveAcrossCall;

    /*!
      @brief mFunctionReturnMoves maps the ID of a %WIR instruction holding a
             function call to the number how many move operations storing a
             function call's result directly follow this function call.
    */
    std::map<WIR_id_t, unsigned int> mFunctionReturnMoves;

    /*!
      @brief mHighPriorityRegs contains the IDs of all TriCore registers that
             shall be colored with high priority so that they are likely not to
             be spilled.
    */
    std::set<WIR_id_t> mHighPriorityRegs;

    /*!
      @brief mLocalLCRegsAliveAcrossCall stores information about lower-context
             registers alive during TriCore CALLs.
    */
    std::map<std::reference_wrapper<WIR_Instruction>, WIR_RegisterSet, WIR_Compare<WIR_Instruction>> mLocalLCRegsAliveAcrossCall;

    /*!
      @brief mBestLCRegsAliveAcrossCall temporarily stores map
             mLCRegsAliveAcrossCall above during Bernstein's best-of-three
             spilling.
    */
    std::map<std::reference_wrapper<WIR_Instruction>, WIR_RegisterSet, WIR_Compare<WIR_Instruction>> mBestLCRegsAliveAcrossCall;

    /*!
      @brief mLocalDummyParameters stores information about dummy parameters for
             TriCore CALLs.
    */
    std::list<std::pair<std::reference_wrapper<WIR_PhysicalRegister>,
                        std::reference_wrapper<WIR_Instruction>>> mLocalDummyParameters;

    /*!
      @brief mBestLocalDummyParameters temporarily stores list
             mLocalDummyParameters above during Bernstein's best-of-three
             spilling.
    */
    std::list<std::pair<std::reference_wrapper<WIR_PhysicalRegister>,
                        std::reference_wrapper<WIR_Instruction>>> mBestLocalDummyParameters;

    /*!
      @brief mUncoloredSpill points to a virtual register that was artificially
             created during spill code generation.
    */
    WIR_VirtualRegister *mUncoloredSpill;


  private:

    /*!
      @brief Method deleted, a data flow analysis will always be carried out.

      @param[in] f A Boolean flag defaulting to true that denotes whether a data
                   flow analysis will be carried out (true) or not.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setRunDFA( bool f = true ) = delete;

    /*!
      @brief mStackAccessLatency stores the memory access latency of the
             physical memory where the stack resides.
    */
    unsigned int mStackAccessLatency;

    /*!
      @brief mBBAccessLatency stores the memory access latency of the physical
             memory where the basic block with the given ID resides.
    */
    std::map<WIR_id_t, unsigned int> mBBAccessLatency;

    /*!
      @brief mUseOnlyUC denotes whether the register allocator should only use
             physical registers from the TriCore's upper context, or not.
    */
    bool mUseOnlyUC;

    /*!
      @brief mOrderedAREGsUC holds all TriCore AREGs of the upper context in the
             precedence order in which they should be used for coloring.
    */
    std::vector<std::reference_wrapper<WIR_PhysicalRegister>> mOrderedAREGsUC;

    /*!
      @brief mOrderedDREGsUC holds all TriCore DREGs of the upper context in the
             precedence order in which they should be used for coloring.
    */
    std::vector<std::reference_wrapper<WIR_PhysicalRegister>> mOrderedDREGsUC;

    /*!
      @brief mOrderedDREGsUC holds all TriCore EREGs of the upper context in the
             precedence order in which they should be used for coloring.
    */
    std::vector<std::reference_wrapper<WIR_PhysicalRegister>> mOrderedEREGsUC;

    /*!
      @brief mOrderedDREGsUC holds all TriCore PREGs of the upper context in the
             precedence order in which they should be used for coloring.
    */
    std::vector<std::reference_wrapper<WIR_PhysicalRegister>> mOrderedPREGsUC;

    /*!
      @brief mOrderedAREGsAliveAcrossCall holds all TriCore AREGs in the
             precedence order in which they should be used for coloring of VREGs
             alive across a function call.
    */
    std::vector<std::reference_wrapper<WIR_PhysicalRegister>> mOrderedAREGsAliveAcrossCall;

    /*!
      @brief mOrderedDREGsAliveAcrossCall holds all TriCore DREGs in the
             precedence order in which they should be used for coloring of VREGs
             alive across a function call.
    */
    std::vector<std::reference_wrapper<WIR_PhysicalRegister>> mOrderedDREGsAliveAcrossCall;

    /*!
      @brief mOrderedEREGsAliveAcrossCall holds all TriCore EREGs in the
             precedence order in which they should be used for coloring of VREGs
             alive across a function call.
    */
    std::vector<std::reference_wrapper<WIR_PhysicalRegister>> mOrderedEREGsAliveAcrossCall;

    /*!
      @brief mOrderedPREGsAliveAcrossCall holds all TriCore PREGs in the
             precedence order in which they should be used for coloring of VREGs
             alive across a function call.
    */
    std::vector<std::reference_wrapper<WIR_PhysicalRegister>> mOrderedPREGsAliveAcrossCall;

};

}       // namespace WIR

#endif  // _TC_GRAPHCOLORING_H
