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
  @file rv32_incl.h
  @brief This file provides the interface of various helper functions used by
         the RISC-V RV32 code selector within its tree grammar.
*/


#ifndef _RV32_INCL_H_
#define _RV32_INCL_H_


//
// Include section
//

// Include standard headers
#include <functional>
#include <list>
#include <string>

// Include local headers
#include <rv32/rv32lvalue.h>
#include <rv32/rv32stack.h>


//
// Class forward declarations
//

class IR_CallExp;
class IR_Exp;
class IR_Integer;
class IR_Symbol;
class IR_SymbolExp;
class IR_Type;

namespace WIR {
class WIR_BaseRegister;
class WIR_VirtualRegister;
}


//
// Header section
//

namespace RV32 {

//
// Preprocessor macros.
//

/*!
  @brief Macro IS_TYPE_EXP_WRAPPER generates a wrapper function for a given
         type-check function.
*/
#define IS_TYPE_EXP_WRAPPER( funcName )                                        \
  inline bool funcName( const IR_Exp &exp )                                    \
  {                                                                            \
    return( funcName( exp.getType() ) );                                       \
  };


/*!
  @brief Macro IS_TYPE_SYMBOL_WRAPPER generates a wrapper function for a given
         type-check function.
*/
#define IS_TYPE_SYMBOL_WRAPPER( funcName )                                     \
  inline bool funcName( const IR_Symbol &sym )                                 \
  {                                                                            \
    return( funcName( sym.getType() ) );                                       \
  };


/*!
  @brief Macro IS_POINTER_EXP_WRAPPER generates a wrapper function for a given
         type-check function.
*/
#define IS_POINTER_EXP_WRAPPER( funcName, nonPointerFunc )                     \
  inline bool funcName( const IR_Exp &exp )                                    \
  {                                                                            \
    auto *ptype = dynamic_cast<IR_PointerType *>( &( exp.getType() ) );        \
    return( ptype && nonPointerFunc( ptype->getBaseType() ) );                 \
  };


/*!
  @brief Macro IS_ARRAY_EXP_WRAPPER generates a wrapper function for a given
         type-check function.
*/
#define IS_ARRAY_EXP_WRAPPER( funcName, nonPointerFunc )                       \
  inline bool funcName( const IR_Exp &exp )                                    \
  {                                                                            \
    auto *atype = dynamic_cast<IR_ArrayType *>( &( exp.getType() ) );          \
    return( atype && nonPointerFunc( atype->getBaseType() ) );                 \
  };


/*!
  @brief Macro SOFTFLOAT_COST evaluates the costs a soft-float operation would
         cause.

  TODO: Is this appropriate and correct for RISC-V?!

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
#define SOFTFLOAT_COST                                                         \
  ( 3 * RV32I::OperationFormat::RR_1.getSize() +                               \
    RV32I::OperationFormat::L_1.getSize() )


/*!
  @brief This type declaration is due to technical reasons (icd-cg allows no
         '<' or '>' in rv32_decl.m4).
*/
using argList = std::list<std::reference_wrapper<WIR::WIR_BaseRegister>>;


/*!
  @brief Enum RegType describes how a value is passed (by register or not by
         register).
*/
enum class RegType : char
{
  REGISTER,
  NO_REGISTER
};


/*!
  @brief Enum Modifier describes modifiers of inline assembly operands.
*/
enum class Modifier : char
{
  //! WRITE represents the '<tt>=</tt>' inline assembly modifier.
  WRITE = 1,

  //! READWRITE represents the '<tt>+</tt>' inline assembly modifier.
  READWRITE = 1 << 1,

  //! EARLYCLOB represents the '<tt>&</tt>' inline assembly modifier.
  EARLYCLOB = 1 << 2
};


//
// Helper functions.
//

/*!
  @brief DEBUG_RULE_ACTION prints out which action part of which rule is
         entered, and for which C expression. If debugging of WCC is disabled,
         DEBUG_RULE_ACTION does nothing.

  @param[in] ruleSignature A const reference to a string containing the rule's
                           signature.
  @param[in] treeElem treeElem A pointer to the current TPM tree node.

  DEBUG_RULE_ACTION should be invoked right at the beginning of the action part
  of each rule.

  This function is named in uppercase letters against the usual convention,
  which would force us to use lowercase letters, because it mimics a debugmacro
  functionality and all the debugmacro stuff is written in uppercase letters.
*/
void DEBUG_RULE_ACTION( const std::string &ruleSignature, NODEPTR treeElem );


/*!
  @brief isBounded32BitConstant determines whether the given IR value of the
         given type denotes an integer constant of at most 32 bits width, whose
         value is in the interval [minValue, maxValue] and whose signedness is
         equivalent to isSigned.

  @param[in] v A const reference to an IR integer value to be checked.
  @param[in] t A const reference to the IR type of v.
  @param[in] minValue An integer denoting the minimal value of the possible
                      value range.
  @param[in] maxValue An integer denoting the maximal value of the possible
                      value range.
  @param[in] isSigned A Boolean denoting whether v shall be interpreted as
                      signed or unsigned value.
  @return true if the expression and its type fulfill the criteria, false
          otherwise.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
bool isBounded32BitConstant( const IR_Integer &v, const IR_Type &t,
                             int minValue, int maxValue, bool isSigned );


/*!
  @brief isUConst5 determines whether the given IR integer is a constant that
         can be represented by the 'uconst5' nonterminal.

  @param[in] v A const reference to the IR integer value.
  @param[in] t A const reference to the underlying IR type.
  @return true if v can be represented by 'uconst5', false otherwise.
*/
inline bool isUConst5( const IR_Integer &v, const IR_Type &t );


/*!
  @brief isConst6 determines whether the given IR integer is a constant that can
         be represented by the 'const6' nonterminal.

  @param[in] v A const reference to the IR integer value.
  @param[in] t A const reference to the underlying IR type.
  @return true if v can be represented by 'const6', false otherwise.
*/
inline bool isConst6( const IR_Integer &v, const IR_Type &t );


/*!
  @brief isUConst6 determines whether the given IR integer is a constant that
         can be represented by the 'uconst6' nonterminal.

  @param[in] v A const reference to the IR integer value.
  @param[in] t A const reference to the underlying IR type.
  @return true if v can be represented by 'uconst6', false otherwise.
*/
inline bool isUConst6( const IR_Integer &v, const IR_Type &t );


/*!
  @brief isUConst8 determines whether the given IR integer is a constant that
         can be represented by the 'uconst8' nonterminal.

  @param[in] v A const reference to the IR integer value.
  @param[in] t A const reference to the underlying IR type.
  @return true if v can be represented by 'uconst8', false otherwise.
*/
inline bool isUConst8( const IR_Integer &v, const IR_Type &t );


/*!
  @brief isConst12 determines whether the given IR integer is a constant that
         can be represented by the 'const12' nonterminal.

  @param[in] v A const reference to the IR integer value.
  @param[in] t A const reference to the underlying IR type.
  @return true if v can be represented by 'const12', false otherwise.
*/
inline bool isConst12( const IR_Integer &v, const IR_Type &t );


/*!
  @brief isUConst20 determines whether the given IR integer is a constant that
         can be represented by the 'uconst20' nonterminal.

  @param[in] v A const reference to the IR integer value.
  @param[in] t A const reference to the underlying IR type.
  @return true if v can be represented by 'uconst20', false otherwise.
*/
inline bool isUConst20( const IR_Integer &v, const IR_Type &t );


/*!
  @brief computeSizeOf computes the size of an IR type in bytes.

  @param[in] t A const pointer to an IR type.
  @return An integer value representing the byte size of the specified IR type.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
int computeSizeOf( const IR_Type *t );


/*!
  @brief computeSizeOf computes the size of a tree element in bytes.

  @param[in] treeElem A const pointer to a data flow tree node denoting an IR
                      sizeof expression.
  @return An integer value representing the byte size for the specified
          expression.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
int computeSizeOf( const NODEPTR treeElem );


/*!
  @brief getFctArgReg determines the pre-colored virtual %WIR register to be
         used for passing an IR symbol as function argument.

  @param[in] sym A const reference to a function argument symbol.
  @return A reference to a pre-colored virtual register to be used for parameter
          passing.

  If the function argument symbol was already assigned to a register previously,
  that register is returned. Otherwise, a new register of matching type is
  created, pre-colored and registered as the representative of the given
  function argument. The new register is then returned.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
WIR::WIR_VirtualRegister &getFctArgReg( const IR_Symbol &sym );


/*!
  @brief getFctArgReg determines the pre-colored virtual %WIR register to be
         used for passing a function argument.

  @param[in] exp A const reference to a function call expression whose arguments
                 are analyzed.
  @param[in] argIdx The index of the argument in the argument list of 'exp'.
  @param[in] phRegIdx The index of the physical register with which the argument
                      shall be pre-colored (only used for indirect function
                      calls).
  @return A reference to a pre-colored virtual register to be used for parameter
          passing.

  If the function argument symbol was already assigned to a register previously,
  that register is returned. Otherwise, a new register of matching type is
  created, pre-colored and registered as the representative of the given
  function argument. The new register is then returned.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
WIR::WIR_VirtualRegister &getFctArgReg( const IR_CallExp &exp,
                                        unsigned int argIdx,
                                        unsigned int phRegIdx );


/*!
  @brief Computes whether 'e' is an expression that denotes a memory location
  where the result of an operation must be written to.
  This is true for cases like, e.g.,
  - 'a[i]' in 'a[i] = 0'
  - '*p' in '*p += 1'
  - 'c->comp' in 'c->comp++'
  - any assignment to global / local stack symbols
  @param[in] e A const reference to an IR expression.
  @return A Boolean spefying whether the expression given specifies a memory
          location where the result of an operation must be written to.
*/
bool isMemoryWriteLocation( const IR_Exp & );


/*!
  @brief bindToPHREG precolors the given virtual %WIR register with a physical
         RISC-V register.

  @param[in] r A const reference to a virtual register to be precolored.
  @param[in] phregNumber A number between 0 and 31 denoting one of the RISC-V's
                         physical registers x0 - x31.
*/
void bindToPHREG( const WIR::WIR_VirtualRegister &r, unsigned int phregNumber );


/*!
  @brief getNewRegister creates a new virtual RISC-V %WIR register for the given
         IR type.

  @param[in] t A const reference to an IR type.
  @return A reference to the newly created virtual RISC-V %WIR register.
*/
WIR::WIR_VirtualRegister &getNewRegister( const IR_Type &t );


/*!
  @brief getNewRegister creates a new virtual RISC-V %WIR register for the given
         IR symbol expression.

  @param[in] symExp A const reference to an IR symbol expression.
  @return A reference to the newly created virtual RISC-V %WIR register.
*/
WIR::WIR_VirtualRegister &getNewRegister( const IR_SymbolExp &symExp );


/*!
  @brief loadGlobalSymbolCost computes the costs for loading a global symbol
         into an appropriate register.

  @param[in] symExp A reference to an IR symbol expression to be loaded.
  @return The costs for the required address computation and memory access
          instructions.
*/
COST loadGlobalSymbolCost( const IR_SymbolExp &symExp );


/*!
  @brief loadGlobalSymbol generates code that loads a global symbol into its
         appropriate register.

  @param[in] symExp A const reference to an IR symbol expression to be loaded.
  @param[in] loadResult A Boolean spefying whether the global symbol shall
                        actually be loaded from memory, or whether just its
                        address information shall be computed.
  @return An lvalue containing information about the memory access.
*/
RV32::RV32_LValue loadGlobalSymbol( const IR_SymbolExp &symExp,
                                    bool loadResult );


/*!
  @brief loadStackSymbolCost computes the costs for loading a local stack symbol
         into its appropriate register.

  @param[in] symExp A reference to an IR symbol expression to be loaded.
  @return The costs for the required address computation and memory access
          instructions.
*/
COST loadStackSymbolCost( const IR_SymbolExp &symExp );


/*!
  @brief loadStackSymbol generates code that loads a local stack symbol into its
         appropriate register.

  @param[in] symExp A const reference to an IR symbol expression to be loaded.
  @param[in] loadResult A Boolean spefying whether the global symbol shall
                        actually be loaded from memory, or whether just its
                        address information shall be computed.
  @return An lvalue containing information about the memory access.
*/
RV32::RV32_LValue loadStackSymbol( const IR_SymbolExp &symExp, bool loadResult );


/*!
  @brief loadRegisterSymbolCost computes the costs for using
         loadRegSym( symExp ).

  @param[in] symExp A const reference to a symbol expression whose load costs
                    shall be determined.
  @return The costs for loading the specified symbol.
*/
COST loadRegisterSymbolCost( const IR_SymbolExp &symExp );


/*!
  @brief loadRegSym loads a non-stack, non-global symbol into its appropriate
         register, as determined by the settings in the Stack class and then
         returns the register.

  @param[in] symExp A const reference to an expression whose symbol shall be
                    loaded.
  @return A reference to the %WIR register finally containing the loaded symbol.
*/
WIR::WIR_BaseRegister &loadRegSym( const IR_SymbolExp &symExp );


/*!
  @brief effectiveType returns an expression's effective type under
         consideration of implicit casts.

  @param[in] exp A const reference to an IR expression.
  @return A reference to the expression's effective type.
*/
inline IR_Type &effectiveType( const IR_Exp &exp )
{
  IR_Type *ret = exp.getImplicitCastType();
  return( ret ? *ret : exp.getType() );
};


/*!
  @brief isFunctionType returns whether a given type is a function type.

  @param[in] t A const reference to an IR type.
  @return true if the given type is a function type, false otherwise.
*/
bool isFunctionType( const IR_Type &t );

IS_TYPE_EXP_WRAPPER( isFunctionType );
IS_TYPE_SYMBOL_WRAPPER( isFunctionType );
IS_POINTER_EXP_WRAPPER( isFunctionPointer, isFunctionType );

/*!
  @brief isPointerType returns whether a given type is a pointer type.

  @param[in] t A const reference to an IR type.
  @return true if the given type is a pointer type, false otherwise.
*/
bool isPointerType( const IR_Type &t );
IS_TYPE_EXP_WRAPPER(    isPointerType );
IS_TYPE_SYMBOL_WRAPPER( isPointerType );


#undef IS_TYPE_EXP_WRAPPER
#undef IS_TYPE_SYMBOL_WRAPPER
#undef IS_ARRAY_EXP_WRAPPER
#undef IS_POINTER_EXP_WRAPPER

}

#endif  // _RV32_INCL_H_
