/*

   This header file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Compiler Framework

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
  @file rv32codesel.h
  @brief This file provides the interface of the RISC-V RV32 tree pattern
         matching-based code selector.
*/


#ifndef _RV32_CODESEL_H
#define _RV32_CODESEL_H


//
// Include section
//

// Include standard headers
#include <functional>
#include <map>
#include <string>
#include <utility>

// Include code selector headers
#include <codesel/codesel.h>
#include <rv32/rv32instructionfactory.h>
#include <rv32/rv32stack.h>


//
// Preprocessor macros
//

#ifdef DEBUG_WCC
#define CODESEL_DBG_TARGET "cout"
#endif


//
// Class forward declarations
//

class IR_AliasAnalysis;
class IR_BasicBlock;
class IR_Exp;
class IR_Symbol;
class IR_SymbolTable;
class IR_Type;

namespace WIR {
class WIR_BasicBlock;
class WIR_Data;
class WIR_System;
}

class BackAnnotation;
class Configuration;
class TaskEntry;


//
// Header section
//

namespace RV32 {

/*!
  @brief Class RV32_CodeSelector represents the RISC-V RV32 code selector along
         with its interface functions.
*/
class RV32_CodeSelector : public CodeSelector
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor.

      @param[in,out] sys A reference to a %WIR system for which code will be
                         generated.
      @param[in] te A reference to a WCC task entry which is used to get the
                    compiler's configuration object and IR.
      @param[in] backannotation A pointer to a back-annotation object.
    */
    RV32_CodeSelector( WIR::WIR_System &, TaskEntry &, BackAnnotation * );

    /*!
      @brief Destructor.
    */
    virtual ~RV32_CodeSelector( void );


    //
    // Code selection management.
    //

    /*!
      @brief reset clears all private members of this class in order to allow
             repetitive code selection.
    */
    void reset( void );

    /*!
      @brief getConfig returns the current compiler configuration.

      @return A pointer to the compiler's configuration object.
    */
    Configuration *getConfig( void );

    /*!
      @brief getInstructionFactory returns the RV32 instruction factory used by
             this code selector.

      @return A reference to the code selector's RV32 instruction factory.
    */
    RV32_InstructionFactory &getInstructionFactory( void );

    #ifdef HAVE_ALIAS_ANALYSIS
    /*!
      @brief getAliasAnalysis returns the ICD-C alias analysis used by this
             code selector (if any).

      @return A pointer to the used alias analysis, or nullptr otherwise.
    */
    IR_AliasAnalysis *getAliasAnalysis( void ) const;
    #endif

    /*!
      @brief doCodeSelection is responsible for the actual task of code
             selection for the RISC-V RV32 ISA.

      doCodeSelection pre-processes the IR of @a mTE and generates data flow
      trees on the fly, calls the tree pattern matcher which performs a tree
      pattern matching based on a tree grammar's rule set. Post-processing of
      the generated assembly code creates the missing control flow edges and the
      control flow graph.
    */
    virtual void doCodeSelection( void );


    //
    // Stack and symbol handling.
    //

    /*!
      @brief getStack returns the code selector's current stack object.

      @return A reference to a stack object.
    */
    RV32_Stack &getStack( void );

    /*!
      @brief getStaticName computes a unique name for a given static symbol.

      @param[in] irsym A const pointer to an IR symbol.
      @return A unique string to be used for the given IR symbol within the
              RISC-V assembly code produced by this code selector.

      According to C, it is allowed to have multiple static variables of same
      name in different symbol tables. However, the high-level IR just obtains
      the symbol name from the source code which might be not unambiguous. Thus,
      the symbol's symbol map is read out and used as the key for the nested
      map. The value of the key is another map that contains as key the symbol
      name and as value the unique static symbol name. It's not just sufficient
      to have the symbol table as key, since there might be more than one static
      symbol within a symbol table. If there is no entry for the requested value
      in the map, it is generated, added to the map and returned. Otherwise, the
      existing entry is read out from the map.
    */
    const std::string getStaticName( const IR_Symbol * );

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


  private:

    //
    // Private type declarations.
    //

    using coreMapType = std::map<std::string, std::string>;
    using outerMapType = std::map<IR_SymbolTable *, coreMapType>;


    //
    // ICD-C pre-processing.
    //

    /*!
      @brief symtabIterator pre-processes an IR symbol table for a given RISC-V
             RV32 code selector. This method serves as callback function for
             IR_SymbolTable::iterateSymbolTables.

      @param[in] symtab A reference to an IR symbol table.
      @param[in,out] p A pointer to a RISC-V RV32 code selector instance.

      iterateSymbolTables calls this function whenever a new symbol table is
      found. Furthermore, symtabIterator creates assembly directives when a
      static variable is found.
    */
    static void symtabIterator( IR_SymbolTable &, RV32_CodeSelector * );

    /*!
      @brief irToAsmInitializer generates a string to be used for assembly-level
             initialization of a given IR expression.

      @param[in] exp A pointer to an IR initializer expression.
      @param[in] targetType A pointer to an IR type to be assumed for
                            initialization.
      @return A tuple of a Boolean flag denoting whether the initializer is a
              numeric literal, and a string containing the actual assembly-level
              initializer.
    */
    virtual std::pair<bool, std::string> irToAsmInitializer( IR_Exp *,
                                                             IR_Type * );

    /*!
      @brief getSize computes the byte size of an IR type.

      @param[in] t A const pointer to the IR type whose size shall be computed.
      @return An unsigned int containing the IR type's byte size.
    */
    virtual unsigned int getSize( const IR_Type * ) const;

    /*!
      @brief getAlignment computes the byte-alignment of an IR type.

      @param[in] t A const pointer to the IR type whose alignment shall be
                   computed.
      @return An unsigned int containing the IR type's byte alignment.
    */
    virtual unsigned int getAlignment( const IR_Type * ) const;

    /*!
      @brief mStack stores the code selector's assembly-level representation of
             the RISC-V RV32 runtime stack.
    */
    RV32_Stack mStack;

    /*!
      @brief mInstructionFactory is used to create RISC-V RV32 assembly
             instructions during code selection.
    */
    RV32_InstructionFactory mInstructionFactory;

    /*!
      @brief mLastCodeSel points to a code selector instance of some outer
             compiler FSM.

      When running WCC, the compiler's internal activities are controlled by the
      FSM module. When running this FSM, it can happen that many nested FSMs are
      created on the fly, each of them independently performing compilation
      tasks within WCC.

      In such a scenario of nested FSMs, mLastCodeSel helps to preserve the
      pointer to the code selector of some outer FSM within the current (nested)
      FSM. After terminating the current FSM, the code selector pointer of the
      outer FSM must be set back.
    */
    CodeSelector *mLastCodeSel;

    /*!
      @brief mStringConstants maps C string constants present in the ICD-C IR to
             WIR data objects at the assembly level.
    */
    std::map<std::string,
             std::reference_wrapper<WIR::WIR_Data>> mStringConstants;

    /*!
      @brief mStaticName stores unique names for static symbols within ICD-C.

      The key of the nested map is the address of the symbol's symbol table. Its
      value is another map containing the symbol name as key and the unique
      static symbol name as value.
    */
    outerMapType mStaticName;

    #ifdef HAVE_ALIAS_ANALYSIS
    /*!
      @brief mAlias points to an ICD-C alias analysis used to add data access
             pragmas to load and store RISC-V RV32 operations.
    */
    IR_AliasAnalysis *mAlias;
    #endif

};

}       // namespace RV32

#endif  // _RV32_CODESEL_H
