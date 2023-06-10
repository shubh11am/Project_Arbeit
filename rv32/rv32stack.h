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
  @file rv32stack.h
  @brief This file provides the interface of the RISC-V RV32 class describing
         a function's stack frame.
*/


#ifndef _RV32_STACK_H
#define _RV32_STACK_H


//
// Include section
//

// Include standard headers
#include <functional>
#include <list>
#include <map>
#include <set>

// Include local headers
#include <rv32/rv32symbolinfo.h>


//
// Class forward declarations
//

class IR_ComposedType;
class IR_Exp;
class IR_Function;
class IR_FunctionType;
class IR_Stmt;
class IR_Symbol;
class IR_SymbolExp;
class IR_SymbolTable;
class IR_Type;

namespace WIR {
class WIR_VirtualRegister;
class RV32IMC;
class RV_RegV;
class WIR_Function;
class WIR_BasicBlock;
}


//
// Header section
//

namespace RV32 {

/*!

  @brief Class RV32_Stack represents a %WIR function's stack frame.

  The information on symbols is stored in a map indexed by IR symbols.
  Furthermore, this class provides various static functions in order to
  determine the size of certain data types.
*/
class RV32_Stack
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor.
    */
    RV32_Stack( void );

    /*!
      @brief Copy constructor.

      @param[in] st A const reference to another object to be copied.
    */
    RV32_Stack( const RV32_Stack & );

    /*!
      @brief Destructor.
    */
    ~RV32_Stack( void );

    /*!
      @brief reset deletes the private member objects to allow repetitive code
             selections.
    */
    void reset( void );


    //
    // IR function handling.
    //

    /*!
      @brief pushFunction handles the chore of pushing an IR_Function onto the
             stack.

      @param[in] f A reference to an IR function to be analyzed in terms
                   of its stack frame.
    */
    void pushFunction( IR_Function & );

    /*!
      @brief getStackFrameSize returns the stack frame size in bytes required by
             a function.

      @param[in] f A const reference to an IR function.
      @return An integer denoting the function's stack frame byte size.
    */
    int getStackFrameSize( const IR_Function & ) const;

    /*!
      @brief getMaxArgOverflowSize returns the maximum size of the stack region
             needed to store overflow function parameters.

      @param[in] f A const reference to an IR function.
      @return An integer denoting the stack's overflow region size.
    */
    int getMaxArgOverflowSize( const IR_Function & ) const;

    /*!
      @brief getCallResultBufferOffset returns the offset of the buffer space
             for composed return values with respect to the current stack
             pointer value.

      @param[in] f A const reference to an IR function.
      @return An integer denoting the offset of the result buffer space.
    */
    int getCallResultBufferOffset( const IR_Function & ) const;


    //
    // IR symbol handling.
    //

    /*!
      @brief getSymbolMap returns the stack's symbol map.

      @return A reference to a map storing symbol information for IR symbols.
    */
    std::map<IR_Symbol *, RV32_SymbolInfo> &getSymbolMap( void );

    /*!
      @brief getOffset returns the total offset of the stack used with a
             function.

      @return An integer denoting the total stack offset.
    */
    int getOffset( void ) const;

    /*!
      @brief getSymbolInfo returns the information assigned to a given IR
             symbol.

      @param[in] sym A const reference to an IR symbol.
      @return A pointer to the symbol information associated with the queried
              IR symbol if the given IR symbol is managed by this class, nullptr
              otherwise.
    */
    RV32_SymbolInfo *getSymbolInfo( const IR_Symbol & );

    /*!
      @brief getSymbolOffset returns the offset of a given symbol with respect
             to the current stack pointer value.

      @param[in] sym A const reference to an IR symbol.
      @return A signed integer denoting the symbol's stack offset, or -1 if the
              symbol is neither a function argument nor a stack-allocated local
              variable.
    */
    int getSymbolOffset( const IR_Symbol & ) const;

    /*!
      @brief getComposedParameterBufferOffset returns the offset of the buffer
             that is used to store a copy of the given composed type object
             which shall be passed by value.

      @param[in] sym A const reference to an IR symbol of composed type.
      @return An integer denoting the offset of the copy buffer space.

      Composed type objects may be passed by value in registers or by pointer,
      but in any case, after having received the object in any of these forms,
      the callee must copy the composed type object to its local stack to make
      sure that pass-by-value behaviour is achieved.
     */
    int getComposedParameterBufferOffset( const IR_Symbol & ) const;

    /*!
      @brief getSymbolType returns the type of a given symbol.

      @param[in] sym A const reference to an IR symbol.
      @return The symbol's type.
    */
    RV32_SymbolInfo::Type getSymbolType( const IR_Symbol & ) const;

    /*!
      @brief getArgumentPos returns the position of a symbol if it is used
             as function argument passed via a register.

      @param[in] sym A const reference to an IR symbol.
      @return An integer denoting the symbol's position if it is a function
              argument, or -1 otherwise.
    */
    int getArgumentPos( const IR_Symbol & ) const;

    /*!
      @brief setSymbolReg associates the given virtual %WIR register with an IR
             symbol.

      @param[in] sym A const reference to an IR symbol.
      @param[in] r A const reference to a virtual %WIR register.
    */
    void setSymbolReg( const IR_Symbol &, const WIR::WIR_VirtualRegister & );

    /*!
      @brief getSymbolReg returns the virtual %WIR register associated with the
             given IR symbol.

      @param[in] sym A const reference to an IR symbol to be looked up.
      @return A reference to the virtual %WIR register that is used to hold the
              IR symbol.

      getSymbolReg fails with an assertion if no %WIR register has previously
      been associated with a symbol.
    */
    WIR::WIR_VirtualRegister &getSymbolReg( const IR_Symbol & ) const;

    /*!
      @brief isSymbolRegSet returns whether a %WIR register has previously been
             associated with an IR symbol.

      @param[in] sym A const reference to an IR symbol to be looked up.
      @return true if a register is associated, false otherwise.
    */
    bool isSymbolRegSet( const IR_Symbol & ) const;

    /*!
      @brief setAddrReg associates a given virtual RISC-V register as address
             register of an IR symbol.

      @param[in] sym A const reference to an IR symbol.
      @param[in] r A const reference to a virtual RISC-V register.
    */
    void setAddrReg( const IR_Symbol &, const WIR::RV_RegV & );

    /*!
      @brief getAddrReg returns the virtual RISC-V address register associated
             with a given IR symbol.

      @param[in] sym A const reference to an IR symbol to be looked up.
      @return A reference to the associated virtual address register.
    */
    WIR::RV_RegV &getAddrReg( const IR_Symbol & ) const;

    /*!
      @brief isAddrRegSet returns whether a RISC-V address register has
             previously been associated with an IR symbol.

      @param[in] sym A const reference to an IR symbol to be looked up.
      @return true if a register is associated, false otherwise.
    */
    bool isAddrRegSet( const IR_Symbol & ) const;

    /*!
      @brief setAddressTaken sets whether the address of a given symbol was
             taken.

      @param[in] sym A const reference to an IR symbol.
      @param[in] t A Boolean denoting whether the address of a symbol is taken
                   or not.

      This information is valid only for symbols being function arguments.
    */
    void setAddressTaken( const IR_Symbol &, bool );

    /*!
      @brief isAddressTaken returns whether the address of a given symbol was
             taken.

      @param[in] sym A const reference to an IR symbol.
      @return true if a symbol's address was taken, false otherwise.

      This information is valid only for symbols being function arguments.
    */
    bool isAddressTaken( const IR_Symbol & ) const;

    /*!
      @brief setStoreInstructionsGenerated sets whether store instructions are
             already generated for a symbol at the beginning of a function.

      @param[in] sym A const reference to an IR symbol.
      @param[in] stInsGenerated A Boolean denoting whether store instructions
                                are already generated at the beginning of a
                                function, for function arguments passed via
                                registers whose address is taken.

      This information is valid only for symbols being function arguments, whose
      address have been taken.
    */
    void setStoreInstructionsGenerated( const IR_Symbol &, bool );

    /*!
      @brief areStoreInstructionsGenerated returns whether store instructions
             are already generated for a symbol at the beginning of a function.

      @param[in] sym A const reference to an IR symbol.
      @return true if store instructions are already generated at the beginning
              of a function, false otherwise.

      This information is valid only for symbols being function arguments, whose
      address have been taken.
    */
    bool areStoreInstructionsGenerated( const IR_Symbol & ) const;

    /*!
      @brief setComposedPushed indicates that a given symbol of composed type is
             already pushed onto the stack.

      @param[in] sym A const reference to an IR symbol of composed type.
    */
    void setComposedPushed( const IR_Symbol & );

    /*!
      @brief isComposedPushed returns whether a symbol of composed type is
             already pushed onto the stack.

      @param[in] sym A const reference to an IR symbol of composed type.
      @return true if the given symbol is pushed onto the stack, false
              otherwise.
    */
    bool isComposedPushed( const IR_Symbol & ) const;

    /*!
      @brief setComposedPushCostAdded indicates that the costs for pushing a
             symbol of composed type to the local stack is already added.

      @param[in] sym A const reference to an IR symbol of composed type.
    */
    void setComposedPushCostAdded( const IR_Symbol & );

    /*!
      @brief getComposedPushCostAdded returns whether the costs for pushing a
             symbol of composed type to the local stack is already added.

      @param[in] sym A const reference to an IR symbol of composed type.
      @return true if the the costs for pushing the given symbol onto the stack
              are already added, false otherwise.
    */
    bool getComposedPushCostAdded( const IR_Symbol & ) const;

    /*!
      @brief isPassedThroughRegister returns whether a given function argument
             symbol will be passed via a register.

      @param[in] sym A const reference to an IR symbol of composed type.
      @return An integer denoting the register number to be used to pass the
              given symbol, or -1 otherwise.
    */
    static int isPassedThroughRegister( const IR_Symbol & );


    //
    // IR type handling.
    //

    /*!
      @brief getParameterStackFrameSize returns the byte size of the stack
             region needed by the parameters of a given function type.

      @param[in] t A const reference to an IR function type.
      @return An integer denoting the stack's parameter region size.
    */
    static int getParameterStackFrameSize( const IR_FunctionType & );

    /*!
      @brief InitStack generates the instructions of the stack backtrace
             and stores the registers of last function upon entering a
             callee function

      @param[in] p     A const reference to a %WIR processor whose function
                       shall be initialized.
      @param[in,out] f A reference to the %WIR function that will contain
                       the new stack allocating and callee save instructions at
                       its very beginning.
      @param[in,out] b A reference to the %WIR basic block that will contain
                       the new stack allocating instructions at its very
                       beginning.
      @param[in] stackAdjOffset An integer denoting the true stack frame size
                                of the given %WIR function.
    */
    void initStack( const WIR::RV32IMC &, WIR::WIR_Function &,
                    WIR::WIR_BasicBlock &, int);

    /*!
      @brief Returns all register numbers which are saved at every
             entry of a function.

      @return List of callee saved registers numbers
    */
    static std::list<int> getCalleeSavedRegs();

    /*!
      @brief adjustStackFrame creates stack adjusting code.

      @param[in] proc A const reference to a %WIR processor whose stack pointer
                      shall be adjusted.
      @param[in,out] b A reference to the %WIR basic block that will contain
                       the new stack allocating instructions at its very
                       beginning.
      @param[in] offset The offset by which the stack pointer is adjusted.
    */
    static void adjustStackFrame( const WIR::RV32IMC &, WIR::WIR_BasicBlock &,
                                  int );

    /*!
      @brief getStackSize computes the byte size of a given IR type if pushed on
             the stack.

      @param[in] t A const reference to an IR type.
      @return An integer denoting the type's byte size on the stack.
    */
    static int getStackSize( const IR_Type & );

    /*!
      @brief getStructBytes returns the byte size of a given struct type.

      @param[in] t A const reference to an IR composed type.
      @return An integer denoting the composed type's byte size.
    */
    static int getStructBytes( const IR_ComposedType & );

    /*!
      @brief isPassedThroughRegister returns whether the nth argument of an IR
             function type will be passed by register.

      @param[in] t A const reference to an IR function type.
      @param[in] n An integer denoting the index number of the argument to be
                   inspected.
      @return An integer denoting the register number to be used to pass the
              nth function argument, or -1 otherwise.
    */
    static int isPassedThroughRegister( const IR_FunctionType &, int );


  private:

    //
    // Local type definitions.
    //

    /*!
      @brief struct storeInfo is used to hold information about (nested) symbols
             of composed types, and their respective stack offsets.
    */
    struct storeInfo
    {
      std::list<IR_Symbol *>::const_iterator itBegin;
      std::list<IR_Symbol *>::const_iterator itEnd;
      bool isStruct;
      int currentOffset;
      IR_ComposedType *currentCompType;
    };

    /*!
      @brief struct memRefByInlineAsm is used to check if a symbol expression is
             used as inline assembly operand with memory constraint.
    */
    struct memRefByInlineAsm : public std::unary_function<IR_SymbolExp *, bool>
    {
      /*!
        @brief This operator checks whether a given symbol expression is used as
               inline assembly operand with memory constraint.

        @param[in] exp A pointer to an IR symbol expression.
        @return true if the symbol expression is an inline assembly operand with
                memory constraint, false otherwise.
      */
      bool operator()( IR_SymbolExp * );
    };


    //
    // IR function handling.
    //

    /*!
      @brief mapFunctionArguments maps the arguments of a given function into
             the internal symbol map which is basically the stack.

      @param[in] f A const reference to an IR function.
    */
    void mapFunctionArguments( const IR_Function & );

    /*!
      @brief mapFunctionSymbols maps the local symbols of a given function into
             the internal symbol map which is basically the stack.

      @param[in] f A reference to an IR function.
    */
    void mapFunctionSymbols( IR_Function & );


    //
    // IR symbol handling.
    //

    /*!
      @brief mapNewStackSymbol creates a new stack entry for a given symbol.

      @param[in] sym A const reference to an IR symbol.
      @param[in,out] s A pointer to a stack object.
    */
    static void mapNewStackSymbol( IR_Symbol &, RV32_Stack * );

    /*!
      @brief isAddressDirectlyTaken determines whether the address of a symbol
             was directly taken as in <tt>&a</tt> for symbol <tt>a</tt>.

      @param[in] sym A const reference to an IR symbol.

      ICD-C also classifies symbol <tt>a</tt>'s address as taken if the symbol
      is used in an expression like, e.g., <tt>&(a[ 12 ])</tt>.
      isAddressDirectlyTaken can be used to distinguish these two cases.
    */
    static bool isAddressDirectlyTaken( const IR_Symbol & );

    /*!
      @brief align aligns a given integer value at a specified alignment width.

      @param[in,out] toAlign An integer to be aligned.
      @param[in] alignmentWidth The alignmend with to be applied.
    */
    static void align( int &, int );


    //
    // IR type handling.
    //

    /*!
      @brief neededRegs computes the number of registers needed to hold a value
             of a given type.

      @param[in] t A const reference to an IR type.
      @return An integer denoting the number of registers needed for the IR
              type.
    */
    int neededRegs( const IR_Type & ) const;


    //
    // IR iterators.
    //

    /*!
      @brief symtabIterator iterates a given symbol table and stores all symbols
             in mSymbolMap.

      @param[in] symtab A reference to an IR symbol table to be iterated.
      @param[in,out] p A void pointer to an RV32_Stack object that stores all
                       visited symbols.
    */
    static void symtabIterator( IR_SymbolTable &, void * );

    /*!
      @brief statementIterator iterates a given statement and looks for that
             call expression having the biggest composed type as return value.

      @param[in] stmt A reference to an IR statement to be iterated.
      @param[in,out] p A void pointer to an RV32_Stack object that stores the
                       information about the biggest found composed type as
                       return value.
    */
    static void statementIterator( IR_Stmt &, void * );

    /*!
      @brief expressionIterator iterates a given expression and looks for that
             call expression having the biggest composed type as return value.

      @param[in] exp A reference to an IR expression to be iterated.
      @param[in,out] p A void pointer to an RV32_Stack object that stores the
                       information about the biggest found composed type as
                       return value.
    */
    static void expressionIterator( IR_Exp &, void * );


    //
    // Attributes.
    //

    /*!
      @brief mSymbolMap maps every symbol declared in a function to an object
             holding the symbol information such as symbol type and offset.
    */
    std::map<IR_Symbol *, RV32_SymbolInfo> mSymbolMap;

    /*!
      @brief mCallResultBufferMap maps every IR function to that special symbol
             that is used to model the buffer space on the stack to which the
             given function may write its composed-type return value.
    */
    std::map<IR_Function *, IR_Symbol *> mCallResultBufferMap;

    /*!
      @brief mComposedParameterBufferMap maps IR symbols of composed type that
             serve as function argument and that are passed via a register to
             special symbols representing a copy of the function argument stored
             on the stack.
    */
    std::map<IR_Symbol *, IR_Symbol *> mComposedParameterBufferMap;

    /*!
      @brief mStackFrameMap maps every IR function to its stack frame size.
    */
    std::map<IR_Function *, int> mStackFrameMap;

    /*!
      @brief mComposedPushed stores all symbols of composed type that have
             already been pushed onto the stack.
    */
    std::set<const IR_Symbol *> mComposedPushed;

    /*!
      @brief mComposedPushCostAdded stores all symbols of composed type for
             which the costs of pushing them to the stack have already been
             added.
    */
    std::set<const IR_Symbol *> mComposedPushCostAdded;

    /*!
      @brief mOffset stores the total offset of the stack pointer for the active
             function.
    */
    int mOffset;

    /*!
      @brief mBiggestReturn points to the biggest composed type that is returned
             by any called function seen so far.
    */
    IR_ComposedType *mBiggestReturn;

};

}

#endif  // _RV32_STACK_H
