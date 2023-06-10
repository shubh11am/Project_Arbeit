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


#ifndef _WCC_STACK_ARM7_H
#define _WCC_STACK_ARM7_H


// Include standard headers
#include <cstring>
#include <map>


// Include ICD headers
#include <icd-c.h>
#include <llir3/llir3.h>

// Include local headers
#include <arm7/symbolinfo.h>

namespace WIR {
class WIR_System;
class WIR_BasicBlock;
class ARM_Base;
}


/*!
  This class represents a stack. The information on symbols is stored in
  a map. Furthermore, this class provides various static functions in order to
  determine the size of certain data types.
*/
class Stack
{

  public:

    //! Constructor creating a default stack.
    Stack();

    //! Copy constructor.
    Stack( const Stack & );

    //! Default Destructor.
    ~Stack();

    //! reset deletes the private member objects to allow repetitive code selections.
    void reset( void );

    //! pushFunction handles the chore of pushing an IR_Function onto the stack.
    void pushFunction( IR_Function * );

    //! getSymbolMap returns the stack data structure.
    std::map<IR_Symbol *, SymbolInfo> *getSymbolMap();

    //! getOffset returns the total offset of the stack used with a function
    int getOffset() const;

    //! getSymbolInfo returns the entire symbol information assigned to the IR symbol.
    /*!
       If the symbol is not part of the symbol map, the NULL pointer is
       returned.
    */
    SymbolInfo *getSymbolInfo( const IR_Symbol * );

    //! getSymbolOffset returns the offset of a particular symbol with respect to the current stack pointer value.
    /*!
       If the symbol is neither a function argument nor a stack-allocated local
       variable, -1 is returned.
    */
    int getSymbolOffset( const IR_Symbol * );

    //! getCallResultBufferOffset returns the offset of the buffer space for composed return values with respect to the current stack pointer value.
    int getCallResultBufferOffset( const IR_Function * );

    //! getComposedParameterBufferOffset returns the offset of the buffer that is used to store a copy of the passed composed type object (pass by value)
    /*!
      Composed type objects may be passed in registers, extended registers
      or by pointer but in any case, after having received the object in any
      of these forms the callee must copy the composed type object to his local
      stack to make sure that pass-by-value behaviour is achieved.
      TODO: If it is known that the callee will never modify the passed struct,
            then we could omit the copying, but this requires a thorough analyis
            that is best done at the high level IR via DefUse-sets.
     */
    int getComposedParameterBufferOffset( const IR_Symbol * );

    //! getSymbolType returns the type of a particular symbol.
    SymbolInfo::Type getSymbolType( const IR_Symbol * );

    //! getArgumentPos returns the position of a particular symbol within the list of arguments that is passed through a (data/address) register
    /*!
       If the symbol is not passed via a register or is not a function argument, -1 is returned.
    */
    int getArgumentPos( const IR_Symbol * );

    //! getSymbolReg returns the LLIR register name assigned to the IR symbol.
    std::string getSymbolReg( const IR_Symbol * );

    //! setSymbolReg assigns a LLIR register name to an IR symbol.
    bool setSymbolReg( const IR_Symbol *, std::string );

    //! getInternalVReg returns the LLIR register name assigned to a function argument within its function itself.
    std::string getInternalVReg( const IR_Symbol * );

    //! setInternalVReg assigns a function internal LLIR register name to an IR argument.
    bool setInternalVReg( const IR_Symbol *, std::string );

    //! getAddrReg returns the address register name assigned to the IR symbol.
    std::string getAddrReg( const IR_Symbol * );

    //! setAddrReg assigns an address register name to an IR symbol.
    bool setAddrReg( const IR_Symbol *, std::string );

    //! setComposedPushed indicates that the struct is already pushed onto the stack.
    void setComposedPushed( const IR_Symbol * );

    //! getComposedPushed indicates if the struct is already pushed onto the stack
    bool getComposedPushed( const IR_Symbol * ) const;

    //! setComposedPushed indicates that the cost for pushing the struct to the local stack is already added
    void setComposedPushCostAdded( const IR_Symbol * );

    //! getComposedPushed indicates if the cost for pushing the struct to the local stack is already added
    bool getComposedPushCostAdded( const IR_Symbol * ) const;

    //! setAddressTaken sets whether the address of the current symbol was taken.
    /*!
      This information is valid only for symbols being function arguments.
    */
    void setAddressTaken( const IR_Symbol *, bool );

    //! getAddressTaken returns whether the address of the current symbol was taken.
    /*!
      This information is valid only for symbols being function arguments.
    */
    bool getAddressTaken( const IR_Symbol * ) const;

    //! setStoreInstructionsGenerated sets whether required ST instructions are already generated during code selection at the very beginning of a LLIR function for function arguments passed via registers, whose address is taken.
    void setStoreInstructionsGenerated( const IR_Symbol *, bool );

    //! getStoreInstructionsGenerated returns whether required ST instructions are already generated during code selection at the very beginning of a LLIR function for function arguments passed via registers, whose address is taken.
    bool getStoreInstructionsGenerated( const IR_Symbol * ) const;

    //! getStackFrameSize returns the stack frame size in bytes required by a function.
    int getStackFrameSize( const IR_Function * );

    //! getMaxArgOverflowSize returns the maximum size of the stackframe needed
    // to store overflow function parameters
    int getMaxArgOverflowSize( IR_Function *theIrFct );

    //! getParameterStackFrameSize returns the stack frame size in bytes required by the parameters of a function type.
    static int getParameterStackFrameSize( const IR_FunctionType & );

    //! InitStack generates the instructions of the stack backtrace
    //! This function stores the registers of last function upon entering a callee function
    static std::list<LLIR_Instruction*> InitStack( LLIR_Function *theFunc,
                                                   LLIR_BB *theBB);

    //! Returns all registers which are saved at every entry of a function.
    static std::list< std::string > getCalleeSavedRegs();

    /*!
      @brief adjustStackFrame creates the instruction SUB SP to adjust the stack
             pointer according to the number of overflow parameters taking into
             account the 8 byte alignment of the SP and returns the generated
             LLIR_Instructions.
    */
    static std::list<LLIR_Instruction *> adjustStackFrame( const WIR::ARM_Base &,
                                                           WIR::WIR_BasicBlock &,
                                                           LLIR_Function *,
                                                           LLIR_BB *, int );

    //! getStackSize computes the size of an item of the given IR type if pushed on the stack.
    static int getStackSize( const IR_Type * );

    //! getStructBytes returns the width of the passed struct in bytes
    static int getStructBytes( const IR_ComposedType * );

    //!isPassedThroughRegister returns whether the given function argument symbol will be passed by register
    /*!
     If the parameter can be passed by register, then the number of the register
     which will be used for the passing is returned (f.e. '4' for 'D4', etc.)

     For composed type objects this is true if the composed type parameter
     can be passed by register, extended register or address register,
     depending on its size. It is especially true if the composed type
     can be passed by address register (pointer) though of course the
     value of the composed type object is not passed through a register,
     only its address is.
    */
    static int isPassedThroughRegister( const IR_Symbol &sym );

    //!isPassedThroughRegister returns whether the given function argument symbol will be passed by register
    /*!
     If the parameter can be passed by register, then the number of the register
     which will be used for the passing is returned (f.e. '4' for 'D4', etc.)

     For composed type objects this is true if the composed type parameter
     can be passed by register, extended register or address register,
     depending on its size. It is especially true if the composed type
     can be passed by address register (pointer) though of course the
     value of the composed type object is not passed through a register,
     only its address is.
    */
    static int isPassedThroughRegister( const IR_FunctionType &funcType,
                                         unsigned int parameterIndex );

  private:

    typedef std::map<IR_Symbol *, SymbolInfo> offsetType;

    //! mSymbolMap maps every symbol declared in a function to the structure holding the symbol information such as symbol type and offset.
    std::map<IR_Symbol *, SymbolInfo> mSymbolMap;

    //! mCallResultBufferMap holds the special symbols that are used to model the buffer space on the stack to which called functions who return composed types may write their results
    std::map<IR_Function *, IR_Symbol *> mCallResultBufferMap;

    //! mComposedParameterBufferMap holds the special symbols that are used to model the buffer space on the stack to which called functions who return composed types may write their results
    std::map<IR_Symbol *, IR_Symbol *> mComposedParameterBufferMap;

    //! mStackFrameMap maps every IR function to its belonging stack frame size.
    std::map<IR_Function *, int> mStackFrameMap;

    //! mComposedPushed accommodates all structs that have been already pushed onto the stack.
    std::set<const IR_Symbol *> mComposedPushed;
    //! mComposedPushCostAdded accommodates all structs for which the cost of pushing them to the stack have already been added.
    std::set<const IR_Symbol *> mComposedPushCostAdded;

    //! Internal variable denoting the total offset of the stack pointer for the active function.
    int mOffset;

    //! Internal variable that denotes the biggest composed type that is returned by any called function seen so far.
    IR_ComposedType *mBiggestReturn;

    //! mapFunctionArguments maps the arguments of the function into the internal symbol map which is basically the stack.
    void mapFunctionArguments( IR_Function * );

    //! mapFunctionSymbols maps the local symbols of the function into the internal symbol map which is basically the stack.
    void mapFunctionSymbols( IR_Function * );

    //! neededRegs calculates the number of registers needed to contain a value of the given type
    int neededRegs( IR_Type * );

    //! symtabIterator iterates through all symbols of the table and stores them in the map mSymbolMap.
    static void symtabIterator( IR_SymbolTable &, void * );

    //! statementIterator is used to search for the call expression with the biggest composed type return value
    static void statementIterator( IR_Stmt &stmt, void *p );
    //! expressionIterator is used to search for the call expression with the biggest composed type return value
    static void expressionIterator( IR_Exp &exp, void *p );

    //! mapNewStackSymbol creates a new stack entry for the given symbol. It is called by the other map functions to do the main work.
    static void mapNewStackSymbol( IR_Symbol &, Stack * );

    //! isAddressDirectlyTaken determines whether the address of a symbol was directly taken as in "&a" for symbol "a"
    /*!
      ICD-C also classifies symbol "a"'s address as taken when the symbol is
      used in an expression like "&(a[12])". This method can be used to seperate
      the two cases.
     */
    static bool isAddressDirectlyTaken( const IR_Symbol &sym );
};

#endif  // _WCC_STACK_ARM7_H
