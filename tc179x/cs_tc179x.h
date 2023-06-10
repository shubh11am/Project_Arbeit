/*

   This header file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Compiler Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2005 - 2022

   Hamburg University of Technology (TUHH)
   Institute of Embedded Systems
   21071 Hamburg
   Germany

   http://www.tuhh.de/es/esd/research/wcc

*/


#ifndef _CODESEL_TC179X_H
#define _CODESEL_TC179X_H


//
// Include section
//

// Include standard headers
#include <deque>
#include <functional>
#include <iostream>
#include <list>
#include <map>
#include <sstream>
#include <string>

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
#include <tc179x/instructionfactory.h>
#include <tc179x/symbolinfo.h>
#include <tc179x/stack.h>


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
class TC13;
}

class Configuration;
class BackAnnotation;
class LLIR_Statistics;


//
// Header section
//

/*!
  @brief Class TC179x_CodeSelector represents the TriCore code selector along
         with its interface functions.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class TC179x_CodeSelector : public CodeSelector
{

  // Auxiliary typedefs to simplify use of nested maps for generation of a
  // unique static symbol name
  using coreMapType = std::map<std::string, std::string>;
  using outerMapType = std::map<IR_SymbolTable *, coreMapType>;


  public:

    //! Constructor with parameter Configuration and BackAnnotation.
    TC179x_CodeSelector( WIR::WIR_System &, TaskEntry &te, BackAnnotation * );

    //! Destructor.
    virtual ~TC179x_CodeSelector( void );

    //! reset deletes the private member objects to allow repetitive code selections
    void reset();

    //! getConfig returns the the Configuration object.
    Configuration *getConfig( void );

    //! Returns the contained instruction factory object
    InstructionFactory &getInstructionFactory( void );

    #ifdef HAVE_ALIAS_ANALYSIS
    //! Returns the currently used alias analysis (if any)
    IR_AliasAnalysis *getAliasAnalysis( void ) const;
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

    /*!
      @brief doCodeSelection is responsible for the actual task of code
             selection for the Infineon TriCore ISA.

      doCodeSelection pre-prcesses the IR of @a mTE and generates data flow
      trees on the fly, calls the tree pattern matcher which performs a tree
      pattern matching based on a tree grammar's rule set. Post-processing of
      the generated assembly code creates the missing control flow edges and the
      control flow graph.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void doCodeSelection( void );

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
    void addLoopboundToLLIR( LLIR_BB *, int, int,
                             LLIR_Loopbound::LoopControlType );

    //! mLOOPApplicable indicates the number of loop iterations determined by ICD's loop analyzer.
    /*!
      If a loop statement is contained in the map, then the LOOP instruction
      can be applied to that loop. The value represents the number of
      loop iterations.
    */
    std::map< IR_LoopStmt *, unsigned int > mLOOPApplicable;

    //! findLOOPCandidates constructs the map mLOOPApplicable for each IR function.
    /*!
      This proceccedure must be invoked after the creation of each LLIR function
      to add virtual registers.
    */
    void findLOOPCandidates( IR_Function * );

    //! The callback function iterates over all stmts and stores loops and their iteration counts in mLOOPApplicable.
    static void stmtIteratorFindLoopIterations( IR_Stmt &, void * );

    //! The callback function iterates over all nested loop statements and returns if a loop nest > 2 was found.
    /*!
      The iterator should be invoked with the current loop nest and if a nested loop statment with
      a nest level greater than two was found, -1 is returned.
    */
    static void stmtIteratorNestLevel( IR_Stmt &, int * );

    //! Adds loop statement and iteration counts to map mLOOPApplicable.
    void addApplicableLoop( const IR_LoopStmt *, unsigned int );

    //! Determines whether the passed loop statement should be handled by LOOP instruction.
    bool hasApplicableLoop( const IR_LoopStmt * );

    //! Gets the iteration counts for loop statement to be handled by LOOP instrction.
    unsigned int getLOOPIterationCounts( const IR_LoopStmt * );

    //! isLoopInsApplicable determines whether the LOOP instruction is applicable to the given loop statement.
    static unsigned int isLoopInsApplicable( IR_LoopStmt * );

    //! getNumberOfEnclosingLOOPs returns the number of loops, that enclose 'stmt' and may be expressed using the LOOP stmt.
    unsigned int getNumberOfEnclosingLOOPs( const IR_Stmt & );

    /*!
      @brief getStringConstData returns a %WIR data object that contains the
             specified string constant.

      @param[in] c A const reference to a string.
      @return A reference to the corresponding %WIR data object.

      If the %WIR system for which code is currently generated does not yet
      contain a data object for the given string constant, a novel read-only
      data object is created and returned.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR::WIR_Data &getStringConstData( const std::string & );


  private:

    /*!
      @brief symtabIterator is used as a parameter to
             IR_SymbolTable::iterateSymbolTables.

      iterateSymbolTables calls this function whenever a new symbol table is
      found and passes the new table as a parameter to this function.
      Furthermore, it creates assembly directives when a static variable is
      found.
    */
    static void symtabIterator( IR_SymbolTable &, TC179x_CodeSelector * );

    //! flattenPostIncDec flattens PRE/POSTincrement, transforms any Xincrement to its explicit form.
    /*!
      The function first transforms any code of type "i++" into "i=i+1".
    */
    void flattenPostIncDec( void );

    //! flattenExpIterator is an expression iterator used for flattening the Xincrements.
    static void flattenExpIterator( IR_Exp &, IR_Stmt * );

    //! flattenNestedFunCalls flattens nested function calls by storing return values used as function arguments in temporary variables.
    void flattenNestedFunCalls();

    //! flattenMestedFunCalls flattens nested function calls within a given expression.
    bool flattenNestedFunCalls( IR_Exp & );

    //! Inserts a constant-true condition into all for loops without a condition.
    void fixForStmtsWithoutCondition( void );

    //! Callback method for 'fixForStmtsWithoutCondition()'
    static void fixForStmtsWithoutCondition( IR_Stmt &, void * );

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
    std::string getGlobalAddressExpr( IR_Exp * );

    //! Initializes integer constants
    /*!
       This method resolves unary expressions and returns the integer value of an expression
    */
    IR_Integer getInitIntConst( IR_Exp * ) const;

    //! Initializes float constants
    /*!
       This method resolves unary expressions and returns the float value of an expression
    */
    IR_Float getInitFloatConst( IR_Exp *, IR_Type::Type ) const;

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

    std::map<std::string,
             std::reference_wrapper<WIR::WIR_Data>> mStringConstants;

    //! The nested map mStaticName holds unique static symbol names.
    /*
       The key of the nested map is the address of the symbol's symbol table.
       Its value is another map containing the symbol name as key and the
       unique static symbol name as value.
    */
    outerMapType mStaticName;

    #ifdef HAVE_ALIAS_ANALYSIS
    //Alias analysis for creating data access pragmas in load and store instructions
    IR_AliasAnalysis *mAlias;
    #endif

};

#endif  // _CODESEL_TC179X_H
