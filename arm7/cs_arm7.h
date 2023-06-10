/*

   This header file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Compiler Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2016 - 2022

   Hamburg University of Technology (TUHH)
   Institute of Embedded Systems
   21071 Hamburg
   Germany

   http://www.tuhh.de/es/esd/research/wcc

*/


#ifndef _CODESEL_ARM7_H
#define _CODESEL_ARM7_H


//
// Include section
//

// Standard headers
#include <cstdlib>
#include <cstring>
#include <list>
#include <map>
#include <string>
#include <sstream>
#include <utility>

// Include ICD headers
#include <icd-c.h>
#include <icdint/icdint.h>
#include <icdint/float_type.h>
#include <llir3/llir3.h>
#include <llir3/llirloopbound.h>

#ifdef HAVE_ALIAS_ANALYSIS
#include <aliasanalysis/iraliasanalysis.h>
#endif

// Include local headers
#include <codesel/codesel.h>
#include <arm7/instructionfactory.h>
#include <arm7/symbolinfo.h>
#include <arm7/stack.h>


//
// Preprocessor macros
//

#ifdef DEBUG_WCC
#define CODESEL_DBG_TARGET "cout"
#endif


//
// Class forward declarations
//

namespace WIR {
class WIR_BasicBlock;
class WIR_Data;
class WIR_System;
}

class Configuration;
class BackAnnotation;
class LLIR_Statistics;


//
// Header section
//

/*!
  @brief Class ARM7_CodeSelector represents the ARM7 code selector along with
         its interface functions.
*/
class ARM7_CodeSelector : public CodeSelector
{

  // Auxiliary typedefs to simplify use of nested maps for generation of a
  // unique static symbol name
  using coreMapType = std::map<std::string, std::string>;
  using outerMapType = std::map<IR_SymbolTable *, coreMapType>;


  public:

    //! Constructor with parameter Configuration and BackAnnotation.
    ARM7_CodeSelector( WIR::WIR_System &, TaskEntry &te, BackAnnotation * );

    //! Default Destructor.
    virtual ~ARM7_CodeSelector();

    /*!
      @brief doCodeSelection is responsible for the actual task of code
             selection for the ARM ISA.

      doCodeSelection pre-prcesses the IR of @a mTE and generates data flow
      trees on the fly, calls the tree pattern matcher which performs a tree
      pattern matching based on a tree grammar's rule set. Post-processing of
      the generated assembly code creates the missing control flow edges and the
      control flow graph.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void doCodeSelection( void );

    //! reset deletes the private member objects to allow repetitive code selections
    void reset();

    //! getConfig returns the the Configuration object.
    Configuration *getConfig();

    //! Returns the contained instruction factory object
    InstructionFactory &getInstructionFactory( void );

    #ifdef HAVE_ALIAS_ANALYSIS
    //! Returns the currently used alias analysis (if any)
    IR_AliasAnalysis *getAliasAnalysis() const;
    #endif

    /*!
      @brief setCurrentInstruction sets the pointer to the LLIR instruction
             lastly generated during code selection.

      @param[in] i A pointer to the last LLIR instruction generated during code
                   selection.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void setCurrentInstruction( LLIR_Instruction * ) override;

    /*!
      @brief getCurrentInstruction returns a pointer to the LLIR instruction
             lastly generated during code selection.

      @return A pointer to the last LLIR instruction generated during code
              selection, or nullptr otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual LLIR_Instruction *getCurrentInstruction( void ) const override;

    //! getStack returns the current stack.
    Stack *getStack( void );

    //! The function getStaticName return a unique name for a static symbol
    /*!
       According to C, it is allowed to have multiple static variables of same
       name in different symbol tables. However, the high-level IR just
       obtains the symbol name from the source code which might be not
       unambiguous. Thus, the symbol's symbol map is read out and used as the
       key for the nested map. The value of the key is another map that
       contains as key the symbol name and as value the unique static symbol
       name. It's not just sufficient to have the symbol table as key since
       there might be more than one static symbol within a symbol table.
       If the is no entry for the requested value in the map, it is generated,
       added to the map and return otherwise it's read out from the map.
    */
    const std::string getStaticName( const IR_Symbol * );


    //
    // WIR basic block handling.
    //

    /*!
      @brief startNewBasicBlock starts a new %WIR basic block immediately after
             that one currently processed by the code selector.

      @param[in] b A const reference to an IR basic block.
      @return A reference to the newly generated %WIR basic block.

      Using the given IR basic block, startNewBasicBlock updates the
      back-annotation mapping (TODO: Missing!).

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR::WIR_BasicBlock &startNewBasicBlock( const IR_BasicBlock & );

    /*!
      @brief startNewBasicBlock starts a new %WIR basic block immediately after
             that one currently processed by the code selector.

      @return A reference to the newly generated %WIR basic block.

      startNewBasicBlock updates the back-annotation mapping by inserting a join
      mapping that marks the current and the new basic block as joined (mapped
      to the same IR BB) (TODO: Missing!).

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR::WIR_BasicBlock &startNewBasicBlock( void );

    //! The function addLoopboundToLLIR adds a Loopbound for a BB.
    /*!
      Loops which are created by the rules of the codeselector needs loopbounds,
      which are created by this function.
    */
    void addLoopboundToLLIR( LLIR_BB *, int, int, LLIR_Loopbound::LoopControlType );

    //! Adds implicit register params to calls inside the given LLIR.
    /*!
     * @brief addImplParams adds implicit parameters to function calls inside
     *        the given LLIR. This is required when these are non-existent, e.g.
     *        when using an external code selector. Yet, these informations are
     *        crucial when performing, e.g., a livetime analysis. This
     *        information is tried to be received from the corresponding IR.
     *        In special cases (e.g., calls to known external functions like
     *        floating point arithmetic ones) the register parameters can be
     *        added without an IR. Otherwise, always the worst case is assumed.
     * @param l Pointer to a LLIR.
     * @param ir Optional pointer to corresponding IR.
     */
    static void addImplParams( LLIR* l, IR* ir = nullptr );

    //! Tries to fix the position of loopbound objectives if needed.
    /*!
     * @brief fixLoopBoundPlacement tries to check if the loopbounds are
     * actually attached to the corresponding loop entry basic block. This can
     * be the case if, e.g., an external code selector was used and the matching
     * based on DWARF line numbers was not precise enough. If the loopbound is
     * not attached to the entry block, it is moved there.
     * @param l Pointer to a LLIR.
     */
    static void fixLoopBoundPlacement( LLIR* l );


  private:

    /*!
      @brief symtabIterator is used as a parameter to
             IR_SymbolTable::iterateSymbolTables.

      iterateSymbolTables calls this function whenever a new symbol table is
      found and passes the new table as a parameter to this function.
      Furthermore, it creates assembly directives when a static variable is
      found.
    */
    static void symtabIterator( IR_SymbolTable &, ARM7_CodeSelector * );

    //! Generates an initializer string from an IR initializer expression.
    virtual std::pair<bool, std::string> irToAsmInitializer( IR_Exp *,
                                                             IR_Type * );

    /*!
      @brief getSize computes the size in bytes of an IR_Type.

      @param[in] t A const pointer to the IR type whose size shall be computed.
      @return An unsigned int containing the IR type's byte size.
    */
    virtual unsigned int getSize( const IR_Type * ) const;

    /*!
      @brief getAlignment computes the alignment in bytes of an IR_Type.

      @param[in] t A const pointer to the IR type whose alignment shall be
                   computed.
      @return An unsigned int containing the IR type's byte alignment.
    */
    virtual unsigned int getAlignment( const IR_Type * ) const;

    //! Generates an adressing expression with symbols and offsets.
    std::string getGlobalAddressExpr( IR_Exp *e );

    //! The nested map mStaticName holds unique static symbol names.
    /*
       The key of the nested map is the address of the symbol's symbol table.
       Its value is another map containing the symbol name as key and the
       unique static symbol name as value.
    */
    outerMapType mStaticName;

    //! mStack represents the stack which accommodates all symbols.
    Stack mStack;

     //! The factory object to create instructions during code selection.
    InstructionFactory mInstructionFactory;

    //! When nested FSMs are created, mLastCodeSel holds the codeselector of the outer FSM.
    /*!
      mLastCodeSel helps to preserve the pointer to the codeselector of the outer FSM
      within the current (nested) FSM. After leaving the current FSM, the codeselector
      pointer of the outer FSM must be set back.
    */
    CodeSelector *mLastCodeSel;

    #ifdef HAVE_ALIAS_ANALYSIS
    //Alias analysis for creating data access pragmas in load and store instructions
    IR_AliasAnalysis *mAlias;
    #endif

};

#endif
