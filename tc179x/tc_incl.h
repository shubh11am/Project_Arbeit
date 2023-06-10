/*

   This header file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Compiler Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2010 - 2022

   Hamburg University of Technology (TUHH)
   Institute of Embedded Systems
   21071 Hamburg
   Germany

   http://www.tuhh.de/es/esd/research/wcc

*/


#ifndef _TC_INCL_H_
#define _TC_INCL_H_


//
// Include section
//

// Include standard headers
#include <deque>
#include <list>
#include <memory>
#include <string>
#include <utility>
#include <vector>

// Include WIR headers
#include <wir/wirtypes.h>

// Include local headers
#include <tc179x/auxfuncs.h>
#include <tc179x/cs_tc179x.h>
#include <tc179x/tclvalue.h>


//
// Class forward declarations
//

class IR_AsmOperand;
class IR_BasicBlock;
class IR_CallExp;
class IR_DoWhileStmt;
class IR_Exp;
class IR_Integer;
class IR_Symbol;
class IR_SymbolExp;
class IR_Type;

namespace WIR {
class WIR_BaseRegister;
class WIR_Function;
class WIR_ID_API;
class WIR_VirtualRegister;
class TC_ARegV;
class TC_DRegV;
class TC_AsmArgument;
}

class LLIR_BB;
class LLIR_Register;

namespace Tricap {
class Argument;
}

class Registrar;


//
// Header section
//

/*!
  @brief This type declaration is due to technical reasons (icd-cg allows no
         '<' or '>' in tc_decl.m4).
*/
using regptrList = std::deque<LLIR_Register *>;


/*!
  @brief This type declaration is due to technical reasons (icd-cg allows no
         '<' or '>' in tc_decl.m4).
*/
using argList = std::list<std::reference_wrapper<WIR::WIR_BaseRegister>>;


/*!
  @brief This enum describes how a certain value is passed ( register / address
         register / extended register / no register at all ).
*/
enum class RegType : char
{
  DATA_REGISTER,
  ADDRESS_REGISTER,
  EXTENDED_REGISTER,
  NO_REGISTER
};


enum class Modifier : char
{
  WRITE = 1,
  READWRITE = 1 << 1,
  EARLYCLOB = 1 << 2
};


enum class Constraint : char
{
  MEMORY = 'm',
  AREG = 'a',
  DREG = 'd',
  CONST = 'i',
  PREVIOUS = '0',
};


/*!
  @brief generateOutputOperand converts an output operand of an inline assembly
         statement to an argument for the TriCore assembly parser.

  @param[in] op A pointer to the output operand to be converted.
  @param[in,out] lvalues A reference to a list of lvalues describing the
                         required memory accesses.
  @return A pair of pointers to the generated arguments for both the LLIR and
          %WIR assembly parsers.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
std::pair<Tricap::Argument *, WIR::TC_AsmArgument *> generateOutputOperand( IR_AsmOperand *op,
                                                                            std::vector<TC_LValue> &lvalues );


/*!
  @brief generateInputOperand converts an input operand of an inline assembly
         statement to an argument for the TriCore assembly parser.

  @param[in] op A pointer to the input operand to be converted.
  @param[in] arguments A const reference to a vector of LLIR assembly parser
                       arguments processed up to now.
  @param[in] args A const reference to a vector of %WIR assembly parser
                  arguments processed up to now.
  @return A pair of pointers to the generated arguments for both the LLIR and
          %WIR assembly parsers.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
std::pair<Tricap::Argument *, WIR::TC_AsmArgument *> generateInputOperand( IR_AsmOperand *op,
                                                                           const std::vector<Tricap::Argument *> &arguments,
                                                                           const std::vector<std::unique_ptr<WIR::TC_AsmArgument>> &args );


//######################################################################
//
// Utility methods
//
//######################################################################

/*!
  Returns whether 'exp' is a LHS of an assignment expression.
*/
inline bool isLHSOfAssignment( const IR_Exp & );


/*!
  Returns whether 'exp' is a RHS of an assignment expression.
*/
inline bool isRHSOfAssignment( const IR_Exp & );


/*!
  If the given expression ia a symbol expression that hold a function argument
  symbol, then this function returns 'true', else 'false'.
*/
inline bool isFunctionArgument( const IR_Exp & );


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

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
bool isBounded32BitConstant( const IR_Integer &v, const IR_Type &t,
                             int minValue, int maxValue, bool isSigned );


/*!
  @brief is32BitConstantValue determines whether the given IR value of the given
         type denotes an integer constant of at most 32 bits width and whose
         value is 'val'.

  @param[in] v A const reference to an IR integer value to be checked.
  @param[in] t A const reference to the IR type of v.
  @param[in] val An integer denoting the required value of the constant.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
bool is32BitConstantValue( const IR_Integer &v, const IR_Type &t, int val );


/*!
  Returns whether the given expression is a constant expression that can be
  represented by the 'addrOffset' nonterminal.
*/
inline bool isAddrOffset( const IR_Integer &v, const IR_Type &t );


/*!
  @brief isConst4 determines whether the given IR integer is a constant that can
         be represented by the 'const4' nonterminal.

  @param[in] v A const reference to the IR integer value.
  @param[in] t A const reference to the underlying IR type.
  @return true if v can be represented by 'const4', false otherwise.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
inline bool isConst4( const IR_Integer &v, const IR_Type &t );


/*!
  @brief isUConst4 determines whether the given IR integer is a constant that
         can be represented by the 'uconst4' nonterminal.

  @param[in] v A const reference to the IR integer value.
  @param[in] t A const reference to the underlying IR type.
  @return true if v can be represented by 'uconst4', false otherwise.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
inline bool isUConst4( const IR_Integer &v, const IR_Type &t );


/*!
  @brief isConst9 determines whether the given IR integer is a constant that can
         be represented by the 'const9' nonterminal.

  @param[in] v A const reference to the IR integer value.
  @param[in] t A const reference to the underlying IR type.
  @return true if v can be represented by 'const9', false otherwise.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
inline bool isConst9( const IR_Integer &v, const IR_Type &t );


/*!
  @brief isUConst9 determines whether the given IR integer is a constant that
         can be represented by the 'uconst9' nonterminal.

  @param[in] v A const reference to the IR integer value.
  @param[in] t A const reference to the underlying IR type.
  @return true if v can be represented by 'uconst9', false otherwise.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
inline bool isUConst9( const IR_Integer &v, const IR_Type &t );


/*!
  @brief isConst16 determines whether the given IR integer is a constant that
         can be represented by the 'const16' nonterminal.

  @param[in] v A const reference to the IR integer value.
  @param[in] t A const reference to the underlying IR type.
  @return true if v can be represented by 'const16', false otherwise.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
inline bool isConst16( const IR_Integer &v, const IR_Type &t );


/*!
  @brief isConstant0 determines whether the given IR integer is a constant that
         can be represented by the 'constant0' nonterminal.

  @param[in] v A const reference to the IR integer value.
  @param[in] t A const reference to the underlying IR type.
  @return true if v can be represented by 'constant0', false otherwise.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
inline bool isConstant0( const IR_Integer &v, const IR_Type &t );


/*!
  @brief isConstant8 determines whether the given IR integer is a constant that
         can be represented by the 'constant8' nonterminal.

  @param[in] v A const reference to the IR integer value.
  @param[in] t A const reference to the underlying IR type.
  @return true if v can be represented by 'constant8', false otherwise.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
inline bool isConstant8( const IR_Integer &v, const IR_Type &t );


/*!
  @brief isConstant256 determines whether the given IR integer is a constant
         that can be represented by the 'constant256' nonterminal.

  @param[in] v A const reference to the IR integer value.
  @param[in] t A const reference to the underlying IR type.
  @return true if v can be represented by 'constant256', false otherwise.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
inline bool isConstant256( const IR_Integer &v, const IR_Type &t );


/*!
  @brief isPowerOfTwo determines whether the given IR integer is a constant
         that can be represented by the 'powerOfTwo' nonterminal.

  @param[in] v A const reference to the IR integer value.
  @param[in] t A const reference to the underlying IR type.
  @return If the constant is a power of two, the power is returned, 0 otherwise.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
inline int isPowerOfTwo( const IR_Integer &v, const IR_Type &t );


/*!
  @brief isNegativePowerOfTwo determines whether the given IR integer is a
         constant that can be represented by the 'negPowerOfTwo' nonterminal.

  @param[in] v A const reference to the IR integer value.
  @param[in] t A const reference to the underlying IR type.
  @return If the constant is a power of two, the power is returned, 0 otherwise.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
inline int isNegativePowerOfTwo( const IR_Integer &v, const IR_Type &t );


/*!
  @brief isConst9Neg determines whether the given IR integer is a constant that
         can be represented by the 'const9_neg' nonterminal.

  @param[in] v A const reference to the IR integer value.
  @param[in] t A const reference to the underlying IR type.
  @return true if v can be represented by 'const9_neg', false otherwise.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
inline bool isConst9Neg( const IR_Integer &v, const IR_Type &t );


/*!
  Returns whether the given 'exp' is an address operator expression that must be
  handled without emitting any code, like, e.g., &*a, &a[i] or the like (see
  ANSI C sec. 6.5.3.2.).
*/
bool isZeroOpADDR( const IR_Exp & );


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
  This function binds the given register to the given physical register number,
  no matter whether it is a data register, an address register or an extended
  register.
*/
void bindToPHREG( LLIR_Register &, unsigned int );


/*!
  @brief bindToPHREG precolors the given virtual %WIR register with a physical
         TriCore register.

  @param[in] r A const reference to virtual %WIR register to be precolored.
  @param[in] phregNumber An unsigned int from the interval [0, 15] denoting the
                         number of the physical TriCore register to be used for
                         precoloring.

  Depending on the virtual register's type, bindtoPHREG assigns r to either a
  data register, an address register or an extended data register.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
void bindToPHREG( const WIR::WIR_VirtualRegister &r, unsigned int phregNumber );


/*!
   If the function argument was already assigned to a register, then that
   register is returned, else a new register of matching type is created
   and registered as the representation of that argument. The new register
   is then returned.

   'sym' is the function parameter symbol
 */
LLIR_Register &getFunctionArgumentRegister( const IR_Symbol & );


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
   If the function argument was already assigned to a register, then that
   register is returned, else a new register of matching type is created
   and registered as the representation of that argument. The new register
   is then returned.

   'funcCall' is the function call expression whose arguments are analyzed
   'argumentIndex' is the index of the argument in the arg. list of 'funcCall'
   'phRegIndex' is the index of the physical register to which the argument
                should be bound (only used for function pointer calls).
*/
LLIR_Register &getFunctionArgumentRegister( const IR_CallExp &, unsigned int,
                                            unsigned int );


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
  Generates a new basic block and inserts it into the current function and
  updates the BackAnnotation mappings by mapping the new block to the given IR
  basic block.
*/
LLIR_BB &beginNewLLIRBasicBlock( const char *, IR_BasicBlock & );


/*!
  Wrapper for "beginNewLLIRBasicBlock( const char *name, IR_BasicBlock &bb )"
  that always uses a new unique block name.
*/
LLIR_BB &beginNewLLIRBasicBlock( IR_BasicBlock & );


/*!
  Generates a new basic block and inserts it into the current function and
  updates the BackAnnotation mappings by inserting a join mapping that marks the
  new basic block and the last LLIR BB as joined together (mapped to the same IR
  BB).
*/
LLIR_BB &beginNewLLIRBasicBlock( const char * );


/*!
  Wrapper for "beginNewLLIRBasicBlock( const char *name )" that always uses a
  new unique block name.
*/
LLIR_BB &beginNewLLIRBasicBlock( void );


/*!
  Computes whether 'e' is an expression that denotes a memory location where the
  result of an operation must be written to. This is true for cases like, e.g.,

  - 'a[i]' in 'a[i] = 0'
  - '*p' in '*p += 1'
  - 'c->comp' in 'c->comp++'
  - any assignment to global / local stack symbols
*/
bool isMemoryWriteLocation( const IR_Exp & );


/*!
  Loads the memory location that is described by the given lvalue into an
  address register and returns this register.

  'baseReg' holds the base register of the memory access
  'offset' holds the offset (bytes) of the memory access
  'target' should be the name of the areg that should be loaded with the address
          (may be the empty string)
  'loadExp' should be the expression in which the load is performed (if any)
*/
LLIR_Register &loadAccessLocationToAReg( LLIR_Register *, int,
                                         const std::string &, const IR_Exp * );


/*!
  @brief loadRegisterRelativeAddressCost computes the cost of instructions that
         load a register-relative address into an address register.

  @param[in] byteSize An unsigned value denoting the underlying base type's byte
                      size.
  @return The costs for loading the specified effective address.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
COST loadRegisterRelativeAddressCost( unsigned int byteSize );


/*!
  Loads the address that is denoted by the given parameters into a new
  virtual address register.

  'baseReg' is an address register that holds the base address
  'offsetReg' is a data register that holds the offset from the base address
              in multiples of 'elementByteSize'
  'elementByteSize' is the size of a single element
  'loadExp' should be the expression in which the load is performed (if any)
*/
LLIR_Register *loadRegisterRelativeAddress( LLIR_Register *, LLIR_Register *,
                                            int, const IR_Exp * );


/*!
  @brief loadRegRelativeAddr generates code that loads a register-relative
         address into an address register.

  @param[in] a A const reference to a virtual address register that holds the
               base address.
  @param[in] d A const reference to a virtual data register that holds the
               offset from the base address in multiples of the base type's byte
               size.
  @param[in] byteSize An unsigned value denoting the underlying base type's byte
                      size.
  @param[in] exp A const pointer defaulting to nullptr that points to an IR
                 expression to be used for the generation of debug information
                 for the newly inserted assembly instruction.
  @return A reference to the virtual %WIR address register finally containing
          the effective address.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
WIR::TC_ARegV &loadRegRelativeAddr( const WIR::TC_ARegV &a,
                                    const WIR::TC_DRegV &d,
                                    unsigned int byteSize,
                                    const IR_Exp *exp = nullptr );


/*!
  @brief transformToLOOPLoopCost computes the costs for a do-while loop using
         the LOOP instruction.

  @param[in] doWhileStmt A const reference to an IR do-while loop whose costs
                         shall be computed.
  @return The costs for the required LOOP and LEA instructions.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
COST transformToLOOPLoopCost( const IR_DoWhileStmt &doWhileStmt );


/*!
  @brief transformToLOOPLoop generates code for a do-while loop using the LOOP
         instruction.

  @param[in] doWhileStmt A const reference to an IR do-while loop for which a
                         LOOP instruction shall be generated.
  @param[in] exp A const pointer defaulting to nullptr that points to an IR
                 expression to be used for the generation of debug information
                 for the newly inserted assembly instruction.

  transformToLOOPLoop does just the conversion, it does not check whether the
  prerequisites for doing so are fulfilled.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
void transformToLOOPLoop( const IR_DoWhileStmt &doWhileStmt,
                          const IR_Exp *exp = nullptr );


/*!
  This function determines for a given symbol expression whether it is a
  function symbol or an array that is not a function parameter. Both cases must
  be handled separately by the loadSymbol functions, because in each case, they
  must then only load the address of the denoted symbol.
*/
bool isTrueArrayOrFunctionSymbol( const IR_SymbolExp & );


/*!
  Returns a new LLIR_Register object for register 'target' that matches the type
  of the given symbol expression.
*/
LLIR_Register *getNewRegister( const std::string &, const IR_SymbolExp & );


/*!
  @brief getNewRegister creates a new virtual TriCore %WIR register for the
         given IR type.

  @param[in] t A const reference to an IR type.
  @return A reference to the newly created virtual TriCore %WIR register.

  Depending on the type, getNewRegister generates either a data register, an
  address register or an extended data register.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
WIR::WIR_VirtualRegister &getNewRegister( const IR_Type &t );

/*!
  @brief getNewRegister creates a new virtual TriCore %WIR register for the
         given IR symbol expression.

  @param[in] symExp A const reference to an IR symbol expression.
  @return A reference to the newly created virtual TriCore %WIR register.

  Depending on the symbol expression's type, getNewRegister generates either a
  data register, an address register or an extended data register.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
WIR::WIR_VirtualRegister &getNewRegister( const IR_SymbolExp &symExp );


/*!
  @brief loadGlobalSymbolCost computes the costs for loading a global symbol
         into an appropriate register.

  @param[in] symExp A reference to an IR symbol expression to be loaded.
  @return The costs for the required address computation and memory access
          instructions.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
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

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
TC_LValue loadGlobalSymbol( const IR_SymbolExp &symExp, bool loadResult );


/*!
  @brief loadStackSymbolCost computes the costs for loading a local stack symbol
         into its appropriate register.

  @param[in] symExp A reference to an IR symbol expression to be loaded.
  @return The costs for the required address computation and memory access
          instructions.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
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

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
TC_LValue loadStackSymbol( const IR_SymbolExp &symExp, bool loadResult );


/*!
  @brief loadRegisterSymbolCost computes the cost for using
         loadRegSym( symExp ).

  @param[in] symExp A const reference to a symbol expression whose load costs
                    shall be determined.
  @return The costs for loading the specified symbol.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
COST loadRegisterSymbolCost( const IR_SymbolExp &symExp );


/*!
  Loads the non-stack, non-global symbol 'sym' into it's appropriate register,
  as determined by the settings in the Stack class and then returns the
  register.
*/
LLIR_Register *loadRegisterSymbol( IR_SymbolExp & );


/*!
  @brief loadRegSym loads the non-stack, non-global symbol 'sym' into its
         appropriate register, as determined by the settings in the Stack class
         and then returns the register.

  @param[in] symExp A const reference to an expression whose symbol shall be
                    loaded.
  @return A reference to the %WIR register finally containing the loaded symbol.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
WIR::WIR_BaseRegister &loadRegSym( const IR_SymbolExp &symExp );


int addModifier( int, char );


Constraint readConstraint( const std::string & );


/*!
  @brief Returns a register which contains the value associated with the symbol
         expression.
*/
LLIR_Register *loadRegister( IR_SymbolExp * );


/*!
  @brief loadRegs emits code to load the given symbol expression into a TriCore
         register.

  @param[in] e A const reference to a symbol expression.
  @return A reference to the TriCore register that contains the symbol
          expression's value.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
WIR::WIR_BaseRegister &loadReg( const IR_SymbolExp &e );


void operandParseError( IR_Exp *, const std::string &, const std::string & );


//######################################################################
//
// Definitions of public inline functions
//
//######################################################################

/*! This macro generates a wrapper function for a given type-check function. */
#define IS_TYPE_EXP_WRAPPER( funcName )                                        \
inline bool funcName( const IR_Exp &exp )                                      \
{                                                                              \
  return( funcName( exp.getType() ) );                                         \
};

/*! This macro generates a wrapper function for a given type-check function. */
#define IS_TYPE_SYMBOL_WRAPPER( funcName )                                     \
inline bool funcName( const IR_Symbol &sym )                                   \
{                                                                              \
  return( funcName( sym.getType() ) );                                         \
};

/*! This macro generates a wrapper function for a given type-check function. */
#define IS_POINTER_EXP_WRAPPER( funcName, nonPointerFunc )                     \
inline bool funcName( const IR_Exp &exp )                                      \
{                                                                              \
  auto *ptype = dynamic_cast<IR_PointerType *>( &( exp.getType() ) );          \
  return( ptype && nonPointerFunc( ptype->getBaseType() ) );                   \
};

/*! This macro generates a wrapper function for a given type-check function. */
#define IS_ARRAY_EXP_WRAPPER( funcName, nonPointerFunc )                       \
inline bool funcName( const IR_Exp &exp )                                      \
{                                                                              \
  auto *atype = dynamic_cast<IR_ArrayType *>( &( exp.getType() ) );            \
  return( atype && nonPointerFunc( atype->getBaseType() ) );                   \
};


/*!
  Returns whether the given type is a pointer type.
*/
bool isPointerType( const IR_Type & );
IS_TYPE_EXP_WRAPPER( isPointerType );
IS_TYPE_SYMBOL_WRAPPER( isPointerType );


/*!
  Returns whether the given type is an array type.
*/
bool isArrayType( const IR_Type & );
IS_TYPE_EXP_WRAPPER( isArrayType );
IS_TYPE_SYMBOL_WRAPPER( isArrayType );


/*!
  Returns whether the given type is a composed type.
*/
bool isComposedType( const IR_Type & );
IS_TYPE_EXP_WRAPPER( isComposedType );
IS_TYPE_SYMBOL_WRAPPER( isComposedType );
IS_ARRAY_EXP_WRAPPER( isComposedArray, isComposedType );
IS_POINTER_EXP_WRAPPER( isComposedPointer, isComposedType );


/*!
  Returns whether the given type is a function type.
*/
bool isFunctionType( const IR_Type & );
IS_TYPE_EXP_WRAPPER( isFunctionType );
IS_TYPE_SYMBOL_WRAPPER( isFunctionType );
IS_POINTER_EXP_WRAPPER( isFunctionPointer, isFunctionType );


/*!
  Returns whether the given type is a bitfield type.
*/
bool isBitfieldType( const IR_Type & );
IS_TYPE_EXP_WRAPPER( isBitfieldType );
IS_TYPE_SYMBOL_WRAPPER( isBitfieldType );


/*
  Returns whether the given type is a long long type.
*/
bool isLongLongType( const IR_Type & );
IS_TYPE_EXP_WRAPPER( isLongLongType );
IS_TYPE_SYMBOL_WRAPPER( isLongLongType );
IS_POINTER_EXP_WRAPPER( isLongLongPointer, isLongLongType );


/*!
  Returns whether the given int const expression has long long type.
*/
bool isLongLongConstant( const IR_IntConstExp & );


/*!
  Returns whether the given type is a double type.
*/
bool isDoubleType( const IR_Type & );
IS_TYPE_EXP_WRAPPER( isDoubleType );
IS_TYPE_SYMBOL_WRAPPER( isDoubleType );


/*!
  Returns whether the given type is float (no double/long double).
*/
bool isFloatType( const IR_Type & );
IS_TYPE_EXP_WRAPPER( isFloatType );
IS_TYPE_SYMBOL_WRAPPER( isFloatType );


/*!
  Returns whether the given type is a character type.
*/
bool isCharType( const IR_Type & );
IS_TYPE_EXP_WRAPPER( isCharType );
IS_TYPE_SYMBOL_WRAPPER( isCharType );


/*!
  Returns whether the given type is a short integer type.
*/
bool isShortType( const IR_Type & );
IS_TYPE_EXP_WRAPPER( isShortType );
IS_TYPE_SYMBOL_WRAPPER( isShortType );


/*!
  Returns whether the given type is a type that can be represented by
  nonterminal 'dreg'.
*/
bool isDRegType( const IR_Type & );
IS_TYPE_EXP_WRAPPER( isDRegType );
IS_TYPE_SYMBOL_WRAPPER( isDRegType );


/*!
  Returns whether the given type is a type that can be represented by
  nonterminal 'areg'.
*/
bool isARegType( const IR_Type & );
IS_TYPE_EXP_WRAPPER( isARegType );
IS_TYPE_SYMBOL_WRAPPER( isARegType );


/*!
  Returns whether the given type is a type that must be stored in an extended
  register.
*/
bool isERegType( const IR_Type & );
IS_TYPE_EXP_WRAPPER( isERegType );
IS_TYPE_SYMBOL_WRAPPER( isERegType );


#undef IS_TYPE_EXP_WRAPPER
#undef IS_TYPE_SYMBOL_WRAPPER
#undef IS_ARRAY_EXP_WRAPPER
#undef IS_POINTER_EXP_WRAPPER

#endif
