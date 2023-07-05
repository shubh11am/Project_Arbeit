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
  @file rvgraphcoloring.h
  @brief This file provides the basic interface of a RISC-V-specific
         graph-coloring based register allocator.
*/


#ifndef _RV_GRAPHCOLORING_H
#define _RV_GRAPHCOLORING_H


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
class WIR_CompilationUnit;
class WIR_Function;
class WIR_Instruction;
class WIR_InterferenceGraph;
class WIR_Operation;
class WIR_Parameter;
class WIR_PhysicalRegister;
class WIR_RegisterParameter;
class WIR_System;
class WIR_VirtualRegister;


/*!
  @brief Class RV_GraphColoring performs graph coloring-based register
         allocation for the RISC-V architecture.

  @author Ben Bahe <Ben.Bahe@tuhh.de>
*/
class RV_GraphColoring : public WIR_GraphColoring
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

      @author Ben Bahe <Ben.Bahe@tuhh.de>
    */
    RV_GraphColoring( WIR_System &, bool = false );

    /*!
      @brief Default constructor for compilation unit-level optimization.

      @param[in] c A reference to a WIR_CompilationUnit to be optimized.
      @param[in] verbosity A Boolean defaulting to false that denotes whether
                           verbose messages shall be dumped or not.

      @author Ben Bahe <Ben.Bahe@tuhh.de>
    */
    RV_GraphColoring( WIR_CompilationUnit &, bool = false );

    /*!
      @brief Default constructor for function-level optimization.

      @param[in] f A reference to a WIR_Function to be optimized.
      @param[in] verbosity A Boolean defaulting to false that denotes whether
                           verbose messages shall be dumped or not.

      @author Ben Bahe <Ben.Bahe@tuhh.de>
    */
    RV_GraphColoring( WIR_Function &, bool = false );

    /*!
      @brief Destructor.

      @author Ben Bahe <Ben.Bahe@tuhh.de>
    */
    virtual ~RV_GraphColoring( void );

  protected:

    //
    // Methods for optimization management.
    //

    /*!
      @brief runOptimization allocates registers in the given function.

      @param[in] f A reference to a WIR_Function to be optimized.

      @author Simon Kopischke <Simon.Kopischke@tuhh.de>
    */
    virtual void runOptimization( WIR_Function & );


    /*!
      @brief saveBestSolutionHook allows to save processor-specific allocation
             data in the course of Bernstein's best-of-three spilling heuristic.

      Here, saveBestSolutionHook is used to save RISC-V-specific information on
      lower-context registers alive across function calls.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void saveBestSolutionHook( void );

    /*!
      @brief restoreBestSolutionHook allows to restore processor-specific
             allocation data in the course of Bernstein's best-of-three spilling
             heuristic.

      Here, restoreBestSolutionHook is used to restore RISC-V-specific
      information on lower-context registers alive across function calls.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void restoreBestSolutionHook( void );


    //
    // Methods for RISC-V-specific physical registers.
    //

    /*!
      @brief createPhregs sets up the lists mPhregs and
             mPhregsForPrecoloringOnly of all RISC-V-specific physical
             registers contained in the specified %WIR function.

      @param[in] f A reference to a %WIR function.

      @author Ben Bahe <Ben.Bahe@tuhh.de>
    */
    virtual void createPhregs( WIR_Function & );

    /*!
      @brief isCallerSaved checks whether the specified physical register is
             caller-saved.

      @param[in] r A const reference to a physical register to be checked.
      @return true iff the physical register is caller-saved, false otherwise.

      @author Ben Bahe <ben.bahe@tuhh.de>
    */
    bool isCallerSaved( const WIR_PhysicalRegister & ) const;

    /*!
      @brief isStackPointer returns whether the specified %WIR register is the
             RISC-V's stack pointer.

      @param[in] r A const reference to a %WIR register to be examined.
      @return true iff the specified register if the stack pointer, false
              otherwise.

      @author Simon Kopischke <Simon.Kopischke@tuhh.de>
    */
    virtual bool isStackPointer( const WIR_BaseRegister & ) const;

    /*!
      @brief checkCallerSavedRegsAliveAcrossCall checks whether the two
             specified virtual leaf registers are mapped to a caller-saved
             physical register and whether they are alive across a function
             call.

      @param[in] phreg A const reference to a physical RISC-V register.
      @param[in] vregs A const reference to a set of registers denoting the
                       first virtual leaf register to be checked, including
                       its coalescing aliases.

      If so, the respective function call and the physical register are stored
      in mCallerSavedRegsAliveAcrossCall.

      @author Ben Bahe <Ben.Bahe@tuhh.de>
    */
    void checkCallerSavedRegsAliveAcrossCall( const WIR_PhysicalRegister &,
                                              const WIR_RegisterSet & );

    /*!
      @brief rewriteProgramHook allows to perform RISC-V-specific actions after
             having transformed the %WIR code.

      @param[in,out] f A reference to a %WIR function.

      Here, rewriteProgramHook is used to realize handling of dummy parameters
      that need to be attached to CALL instruction in certain cases (see also
      the lenghty comment inside method checkLCRegsAliveAcrossCall).

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void rewriteProgramHook( WIR_Function & );


    //
    // Interference graph construction.
    //

    /*!
      @brief buildProcessorSpecificInterferences adds edges to the interference
             graph expressing RISC-V-specific interferences.

      @param[in] f A reference to a %WIR function.
      @param[in,out] igraph A reference to the interference graph.

      Furthermore, buildProcessorSpecificInterferences checks which virtual
      registers are live-out across function calls inside f.

      @author Ben Bahe <Ben.Bahe@tuhh.de>
    */
    virtual void buildProcessorSpecificInterferences( WIR_Function &,
                                                      WIR_InterferenceGraph & );

    /*!
      @brief isFunctionReturnMove checks whether the move operation stores the
             result of a function call somewhere.

      @param[in] o A const reference to a move operation.
      @return isFunctionReturnMove returns true iff
              - the register used by the move is R10, and
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

      For the RISC-V register allocator, high-priority registers are kept in set
      mHighPriorityRegs. Thus, this method only checks whether r is in
      mHighPriorityRegs or not.

      @author Simon Kopischke <Simon.Kopischke@tuhh.de>
    */
    virtual bool isPriorityRegister( const WIR_VirtualRegister & ) const;

    /*!
      @brief getSpillLoadCosts returns the RISC-V-specific costs of one single
             spill-load for the specified register parameter.

      @param[in] p A const reference to a %WIR register parameter.
      @return Some RISC-V-specific cost measure for a spill-load of register
              parameter p.

      @author Ben Bahe <Ben.Bahe@tuhh.de>
    */
    virtual unsigned int getSpillLoadCosts( const WIR_RegisterParameter & ) const;

    /*!
      @brief getSpillStoreCosts returns the RISC-V-specific costs of one single
             spill-store for the specified register parameter.

      @param[in] p A const reference to a %WIR register parameter.
      @return Some RISC-V-specific cost measure for a spill-store of register
              parameter p.

      @author Ben Bahe <Ben.Bahe@tuhh.de>
    */
    virtual unsigned int getSpillStoreCosts( const WIR_RegisterParameter & ) const;

    /*!
      @brief getMoveCosts returns the RISC-V-specific costs of one single move
             operation that can be omitted due to spilling.

      @param[in] o A const reference to a move operation.
      @return Some RISC-V-specific cost measure for a move operation.

      @author Ben Bahe <Ben.Bahe@tuhh.de>
    */
    virtual unsigned int getMoveCosts( const WIR_Operation & ) const;


    //
    // Coalescing.
    //

    /*!
      @brief getUseOfMove returns the used register of the specified RISC-V %WIR
             move operation.

      @param[in] o A const reference to a move operation.
      @return A reference to the %WIR register used by the move operation.

      @author Simon Kopischke <Simon.Kopischke@tuhh.de>
    */
    virtual WIR_BaseRegister &getUseOfMove( const WIR_Operation & ) const;

    /*!
      @brief getDefOfMove returns the defined register of the specified RISC-V
             %WIR move operation.

      @param[in] o A const reference to a move operation.
      @return A reference to the %WIR register defined by the move operation.

      @author Simon Kopischke <Simon.Kopischke@tuhh.de>
    */
    virtual WIR_BaseRegister &getDefOfMove( const WIR_Operation & ) const;

    /*!
      @brief avoidCoalescing checks whether the two given registers which are
             both move-related must not be coalesced due to RISC-V-specific
             reasons.

      @param[in] o A const reference to a move operation.
      @param[in] r1 A const reference to the first register to be checked.
      @param[in] r2 A const reference to the second register to be checked.
      @param[in] igraph A const reference to the interference graph.
      @return true iff coalescing r1 and r2 must be avoided.

      Coalescing will be avoided if
      - the register used by the move is R10, and
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
      @brief getRematerializationInstructions returns a list of RISC-V
             instructions for one single recomputation of the specified used
             parameter.

      @param[in] p A const reference to a %WIR register parameter.
      @return Some RISC-V-specific machine instructions for the
              rematerialization of the register in used in p.

      @author Ben Bahe <Ben.Bahe@tuhh.de>
    */
    virtual std::list<WIR_Instruction *> getRematerializationInstructions( const WIR_RegisterParameter & ) const;


    //
    // Coloring.
    //

    /*!
      @brief selectColors assigns actual colors to the RISC-V leaf registers in
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

      The registers are prioritized according to mOrderedPhregs.

      @author Simon Kopischke <Simon.Kopischke@tuhh.de>
    */
    virtual WIR_GraphColoring::WIR_ColorMap selectColors( const std::vector<std::reference_wrapper<WIR_VirtualRegister>> &,
                                                          const WIR_InterferenceGraph & );


    //
    // Code transformation.
    //

    /*!
      @brief isSpillStore checks whether the specified instruction spill-stores
             a certain virtual register in the RISC-V ISA.

      @param[in] i A const reference to a %WIR instruction.
      @param[in] r A const reference to a virtual register to be examined.
      @return true iff the given instruction is a spill-store.

      @author Ben Bahe <Ben.Bahe@tuhh.de>
    */
    virtual bool isSpillStore( const WIR_Instruction &,
                               const WIR_VirtualRegister & ) const;

    /*!
      @brief isSpillLoad checks whether the specified instruction spill-loads a
             certain virtual register in the RISC-V ISA.

      @param[in] i A const reference to a %WIR instruction.
      @param[in] r A const reference to a virtual register to be examined.
      @return true iff the given instruction is a spill-load.

      @author Ben Bahe <Ben.Bahe@tuhh.de>
    */
    virtual bool isSpillLoad( const WIR_Instruction &,
                              const WIR_VirtualRegister & ) const;

    /*!
      @brief getStackPosOfSubReg returns the stack position of some child
             register, if the root of the entire register hierarchy is located
             in the specified stack position.

      @param[in] r A const reference to a virtual %WIR child register.
      @param[in] rootPos The position of r's root register on the stack.
      @return The according stack position of r on the stack.

      @note: Since RISC-V registers are not hierarchical and thus have no child
             register, this method always returns the position of the given root
             register on the stack.

      @author Simon Kopischke <Simon.Kopischke@tuhh.de>
    */
    virtual unsigned int getStackPosOfSubReg( const WIR_VirtualRegister &,
                                              unsigned int ) const;

    /*!
      @brief insertSpillLoad inserts RISC-V code for a spill-load of a register
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

      @author Ben Bahe <Ben.Bahe@tuhh.de>
    */
    virtual void insertSpillLoad( const WIR_BaseRegister &,
                                  const WIR_BaseRegister &,
                                  int, WIR_BasicBlock &,
                                  std::list<std::reference_wrapper<WIR_Instruction>>::const_iterator );

    /*!
      @brief insertSpillStore inserts RISC-V code for a spill-store of a
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

      @author Ben Bahe <Ben.Bahe@tuhh.de>
    */
    virtual void insertSpillStore( const WIR_BaseRegister &,
                                   const WIR_BaseRegister &,
                                   int, WIR_BasicBlock &,
                                   std::list<std::reference_wrapper<WIR_Instruction>>::const_iterator );

    /*!
      @brief insertSpillCode inserts RISC-V code for a spill-load or spill-store
             of a register into the %WIR.

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

      @author Ben Bahe <Ben.Bahe@tuhh.de>
    */
    virtual WIR_BasicBlock &insertSpillCode( const WIR_BaseRegister &,
                                             const WIR_BaseRegister &,
                                             int, WIR_BasicBlock &,
                                             std::list<std::reference_wrapper<WIR_Instruction>>::const_iterator,
                                             bool );

    /*!
      @brief isAdjustedLoadOrStoreInstruction returns whether the given iterator
             refers to a load or a store instruction with a stack pointer
             relative addressing mode which is surrounded by ADDI instructions
             that adjust the stack pointer.

      @param[in] b A const reference to a %WIR basic block whose instructions
                   shall be checked.
      @param[in] pos A const iterator refering to the position of an instruction
                     to be checked.
      @return true iff pos refers to an adjusted stack access, false otherwise.

     isAdjustedLoadOrStoreInstruction checks if the code looks like

       addi   x2, x2, const1               # with const1 > 0
       l__    __, __(x2)                   # pos
       addi   x2, x2, -const2              # with const2 > 0

     or

       addi   x2, x2, const1               # with const1 > 0
       s__    __, __(x2)                   # pos
       addi   x2, x2, -const2              # with const2 > 0

     TODO: Since the current implementation of handling offsets larger than 12
           bits violates the RISC-V ABI, this needs to be rewritten once that
           the implementation is changed to be ABI-compatible.

      @author Simon Kopischke <Simon.Kopischke@tuhh.de>
    */
    bool isAdjustedLoadOrStoreInstruction( const WIR_BasicBlock &,
                                           std::list<std::reference_wrapper<WIR_Instruction>>::const_iterator ) const;

    /*!
      @brief getCandidatePhregs returns a set of physical registers that could
             be used for the specified virtual register according to the
             RISC-V's ISA.

      @param[in] r A const reference to a virtual %WIR register.
      @return A set of physical RISC-V registers.

      @author Simon Kopischke <Simon.Kopischke@tuhh.de>
    */
    virtual WIR_PhysicalRegisterSet getCandidatePhregs( const WIR_VirtualRegister & );

    /*!
      @brief getCandidatePhreg returns one element from the specified set of
             registers that will finally be used within
             allocateUncoloredActualSpills for spilling.

      @param[in] candidates A const reference to a set of potential candidate
                            phregs for spilling.
      @return A reference to one of the registers in set candidates.

      @author Simon Kopischke <Simon.Kopischke@tuhh.de>
    */
    virtual const WIR_PhysicalRegister &getCandidatePhreg( const WIR_PhysicalRegisterSet & );

    /*!
      @brief postProcessingHook allows to perform RISC-V-specific actions after
             having done register allocation for a function, using e.g., the set
             of inserted spill operations mInsertedSpillCode.

      @param[in,out] f A reference to a %WIR function.

      Here, postProcessingHook is used to realize the RISC-V-specific calling
      conventions afterwards.
    */
    virtual void postProcessingHook( WIR_Function & );

    /*!
      @brief adjustStack allocates additional space in the specified function's
             stack frame and adjusts all stack-related memory accesses
             accordingly.

      @param[in,out] f A reference to a %WIR function.

      According to the RISC-V ABI (section 2.1, Integer Calling Convention), the
      stack grows downwards (towards lower addresses) and the stack pointer
      shall be aligned to a 128-bit boundary upon procedure entry. The first
      argument passed on the stack is located at offset zero of the stack
      pointer on function entry; following arguments are stored at
      correspondingly higher addresses.

      In the standard ABI, the stack pointer must remain aligned throughout
      procedure execution. [...]

      Procedures must not rely upon the persistence of stack-allocated data
      whose addresses lie below the stack pointer.

      @verbatim
      (Stack
       growing
       direction)
           |
           |   +-------------------------+      (high address)
           |   | Local Variables Func 1  |
           |   +-------------------------+
           |   | Argument Area for func- |
           |   | tions called by Func 1  |      (first argument passed on stack)
           |   +-------------------------+
           |   | Local Variables Func 2  |
           |   +-------------------------+
           |   | Argument Area for func- |
           |   | tions called by Func 2  |
           |   +-------------------------+ <--- Stack Pointer (SP) at entry
           V   | Local Variables Func 3  |      (CALL) to Function 3
               +-------------------------+
               | Argument Area for func- |
               | tions called by Func 3  |
               +-------------------------+ <--- Stack Pointer (SP) after stack
               |                         |      allocation of Function 3
               |           ...           |
               +-------------------------+      (low address)
      @endverbatim

      @author Simon Kopischke <Simon.Kopischke@tuhh.de>
    */
    virtual void adjustStack( WIR_Function & );

    /*!
      @brief postRACleanup allows to perform very final RISC-V-specific cleanup
             actions, particularly after stack frame reorganization.

      @param[in] f A reference to a %WIR function.

      Here, postRACleanup is used to remove redundant MOV
      instructions. Furthermore, don't optimize flags of parameters (indicating
      accesses to the RISC-V's argument overflow stack region) are reset.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void postRACleanup( WIR_Function & );

    /*!
      @brief For a list of instructions implementing one spill-load or -store,
             getPhregOfSpill determines that physical register that is actually
             spill-loaded or -stored.

      @param[in] spill A const reference to a list of spill instructions.

      @author Ben Bahe <Ben.Bahe@tuhh.de>
    */
    virtual WIR_PhysicalRegister &getPhregOfSpill( const std::list<std::reference_wrapper<WIR_Instruction>> & );


    //
    // Attributes.
    //

    /*!
      @brief mCallerSavedRegsAliveAcrossCall provides a set of lower-context
             registers that are live-out at a given call instruction.
    */
    std::map<std::reference_wrapper<WIR_Instruction>, WIR_RegisterSet,
      WIR_Compare<WIR_Instruction>> mCallerSavedRegsAliveAcrossCall;

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
    std::map<std::reference_wrapper<WIR_Instruction>, WIR_VirtualRegisterSet,
      WIR_Compare<WIR_Instruction>> mVregsAliveAcrossCall;

    /*!
      @brief mFunctionReturnMoves maps the ID of a %WIR instruction holding a
             function call to the number how many move operations storing a
             function call's result directly follow this function call.
    */
    std::map<WIR_id_t, unsigned int> mFunctionReturnMoves;

    /*!
      @brief mHighPriorityRegs contains the IDs of all RISC-V registers that
             shall be colored with high priority so that they are likely not to
             be spilled.
    */
    std::set<WIR_id_t> mHighPriorityRegs;

    /*!
      @brief mLocalCallerSavedRegsAliveAcrossCall stores information about
             caller-saved registers alive during RISC-V CALLs.
    */
    std::map<std::reference_wrapper<WIR_Instruction>, WIR_RegisterSet,
      WIR_Compare<WIR_Instruction>> mLocalCallerSavedRegsAliveAcrossCall;

    /*!
      @brief mBestLCRegsAliveAcrossCall temporarily stores map
             mLCRegsAliveAcrossCall above during Bernstein's best-of-three
             spilling.
    */
    std::map<std::reference_wrapper<WIR_Instruction>, WIR_RegisterSet,
      WIR_Compare<WIR_Instruction>> mBestLocalCallerSavedRegsAliveAcrossCall;

    /*!
      @brief mLocalDummyParameters stores information about dummy parameters for
             RISC-V CALLs.
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
      @brief mOrderedPhregs holds all RISC-V physical registers in the
             precedence order in whitch they should be used for coloring
    */
    std::vector<std::reference_wrapper<WIR_PhysicalRegister>> mOrderedPhregs;

    /*!
      @brief mOrderedREGsAliveAcrossCall holds all RIEG-V registers in the
             precedence order in which they should be used for coloring of VREGs
             alive across a function call.
    */
    std::vector<std::reference_wrapper<WIR_PhysicalRegister>> mOrderedREGsAliveAcrossCall;

};

}       // namespace WIR

#endif  // _RV_GRAPHCOLORING_H
