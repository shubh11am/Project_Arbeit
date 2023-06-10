/*

   This source file belongs to the

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
  @file rv32stack.cc
  @brief This file implements the RISC-V RV32 class describing a function's
         stack frame.
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wcc.h>
#endif

// Include standard headers
#include <deque>
#include <stack>

// Include boost headers
#include <boost/current_function.hpp>

// Include ICD headers
#include <icd-c.h>

// Include WIR headers
#include <wir/wir.h>
#include <arch/riscv/rv32imc.h>

// Include libuseful headers
#include <libuseful/exceptions.h>
#include <libuseful/io.h>

// Include local headers
#include <rv32/rv32codesel.h>
#include <rv32/rv32instructionfactory.h>
#include <rv32/rv32registrar.h>
#include <rv32/rv32stack.h>
#include <rv32/rv32_incl.h>


//
// Code section
//

using namespace std;
using namespace WIR;


namespace RV32 {


//
// Public class methods
//

/*
  Default constructor.
*/
RV32_Stack::RV32_Stack( void ) :
  mOffset { 0 },
  mBiggestReturn { nullptr }
{
  DSTART( "RV32_Stack::RV32_Stack()" );

  mSymbolMap.clear();
  mStackFrameMap.clear();

  mComposedPushed.clear();
};


/*
  Copy constructor.
*/
RV32_Stack::RV32_Stack( const RV32_Stack &st ) :
  mSymbolMap { st.mSymbolMap },
  mCallResultBufferMap { st.mCallResultBufferMap },
  mComposedParameterBufferMap { st.mComposedParameterBufferMap },
  mStackFrameMap { st.mStackFrameMap },
  mComposedPushed { st.mComposedPushed },
  mComposedPushCostAdded { st.mComposedPushCostAdded },
  mOffset { st.mOffset },
  mBiggestReturn { st.mBiggestReturn }
{
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Destructor.
*/
RV32_Stack::~RV32_Stack( void )
{
  DSTART( "RV32_Stack::~RV32_Stack()" );
};


/*
  reset deletes the private member objects to allow repetitive code selections.
*/
void RV32_Stack::reset( void )
{
  DSTART( BOOST_CURRENT_FUNCTION );

  mSymbolMap.clear();
  mCallResultBufferMap.clear();
  mComposedParameterBufferMap.clear();
  mStackFrameMap.clear();
  mComposedPushed.clear();
  mComposedPushCostAdded.clear();
  mOffset = 0;
  mBiggestReturn = nullptr;
};


/*
  pushFunction handles the chore of pushing an IR_Function onto the stack.
*/
void RV32_Stack::pushFunction( IR_Function &f )
{
  DSTART( "void RV32_Stack::pushFunction(IR_Function&)" );

  /* The stack design is:

  (Stack growing direction)
  |   (high address)
  |   +--------------------------------+
  |   |     * Caller stack frame *     | | Caller stack frame
  |   +--------------------------------+ |
  |   | Overflow arguments area        | | <- Further overflow arguments
  |   | for callee function arguments  | V <- SP before callee entry /
                                              First overflow argument
  |   +--------------------------------+
  |   | Return address for callee      | |
  |   +--------------------------------+ | Callee stack frame
  |   | Frame pointer of caller        | |
  |   +--------------------------------+ |
  |   | Callee saved register s1       | |
  V   | ...                            | |
      | Callee saved register s11      | |
      +--------------------------------+ |
      | Loval variables in the order   | | <- First local stack variable
      | the appear in the callee       | | <- This area does not exist if the
                                              callee has no local stack variables
      +--------------------------------+ |
      | possible alignment area to     | | <- This area does not hold data
      | ensure 16 byte aligned sp      | | <- This area does not exist if the
                                              stack frame is naturally aligned
      +--------------------------------+ |
      | possible spill area created    | | <- This area does not exists if a
                                              function does not require spills
      | by the register allocator      | | <- First spill data at lowest offset
      +--------------------------------+ |
      | Overflow arguments area for    | | <- Further overflow arguments
      | functions called by callee     | V <- SP after callee prologue /
                                              First overflow argument
      +--------------------------------+
      (low address)

      As the stack is growing towards the lower addresses, we must compute the
      offsets in the reversed order (lower offsets access lowest part of the
      function's own stack)


    TODO: Clarify this for the RISC-V EABI, specify where, e.g., return value
          buffers, struct parameter buffers, overflow areas and local variables
          reside in detail!
  */

  // Amount of stack used for over flow arguments for functions
  // called by this function
  mOffset = getMaxArgOverflowSize( f );

  // Amount of stack used for over local stack variables
  mapFunctionSymbols( f );

  // We have to consider the amount of stack used to save the relevant regs
  // from the callee.
  mOffset += getCalleeSavedRegs().size() * 4;

  // Align caller stack frame size to the next 16 bytes position.
  align( mOffset, 16 );

  // Adjust offset for accessing overflow arguments from the caller of f.
  // The stack frame offset that is involuntarily introduced here is cleared in
  // the codeselector when initStack is called with an adjusted stack frame size
  mapFunctionArguments( f );

  mStackFrameMap[ const_cast<IR_Function *>( &f ) ] = mOffset;
};


/*
  getStackFrameSize returns the stack frame size in bytes required by a
  function.
*/
int RV32_Stack::getStackFrameSize( const IR_Function &f ) const
{
  DSTART( "int RV32_Stack::getStackFrameSize( const IR_Function &f ) const" );

  auto it = mStackFrameMap.find( const_cast<IR_Function*>( &f ) );

  return( it != mStackFrameMap.end() ? it->second : -1 );
};


/*
  getMaxArgOverflowSize returns the maximum size of the stack region needed to
  store overflow function parameters.
*/
int RV32_Stack::getMaxArgOverflowSize( const IR_Function &f ) const
{
  DSTART( "int RV32_Stack::getMaxArgOverflowSize(const IR_Function&) const" );

  int overflowsize = 0;

  for ( auto it = f.getCalledFunctions().begin();
        it != f.getCalledFunctions().end(); ++it ) {
    int funcoverflow = getParameterStackFrameSize( it->first->getType() );
    if ( overflowsize < funcoverflow )
      overflowsize = funcoverflow;
  }

  for ( auto it = f.getIndirectFunctionCalls().begin();
        it != f.getIndirectFunctionCalls().end(); ++it ) {
    int funcoverflow = getParameterStackFrameSize( (*it)->getFunctionType() );
    if ( overflowsize < funcoverflow )
      overflowsize = funcoverflow;
  }

  return( overflowsize );
};


/*
  getCallResultBufferOffset returns the offset of the buffer space for composed
  return values with respect to the current stack pointer value.
*/
int RV32_Stack::getCallResultBufferOffset( const IR_Function &f ) const
{
  DSTART( BOOST_CURRENT_FUNCTION );

  auto it = mCallResultBufferMap.find( const_cast<IR_Function *>( &f ) );

  return(
    it == mCallResultBufferMap.end() ? 0 : getSymbolOffset( *(it->second) ) );
};


/*
  getSymbolMap returns the stack's symbol map.
*/
map<IR_Symbol *, RV32_SymbolInfo> &RV32_Stack::getSymbolMap( void )
{
  DSTART( BOOST_CURRENT_FUNCTION );

  return( mSymbolMap );
};


/*
  getOffset returns the total offset of the stack used with a function.
*/
int RV32_Stack::getOffset( void ) const
{
  DSTART( BOOST_CURRENT_FUNCTION );

  return( mOffset );
};


/*
  getSymbolInfo returns the information assigned to a given IR symbol.
*/
RV32_SymbolInfo *RV32_Stack::getSymbolInfo( const IR_Symbol &sym )
{
  DSTART( "RV32_SymbolInfo* RV32_Stack::getSymbolInfo(const IR_Symbol&)" );

  auto it = mSymbolMap.find( const_cast<IR_Symbol *>( &sym ) );

  return( it == mSymbolMap.end() ? nullptr : &( (*it).second ) );
};


/*
  getSymbolOffset returns the offset of a given symbol with respect to the
  current stack pointer value.
*/
int RV32_Stack::getSymbolOffset( const IR_Symbol &sym ) const
{
  DSTART( "int RV32_Stack::getSymbolOffset( const IR_Symbol &sym ) const" );

  auto it = mSymbolMap.find( const_cast<IR_Symbol *>( &sym ) );

  if ( it == mSymbolMap.end() )
    return( 0 );

  if ( ( it->second.getSymbolType() == RV32_SymbolInfo::Type::R_ARGUMENT ) ||
       ( it->second.getSymbolType() == RV32_SymbolInfo::Type::LOCAL_STACK_VAR ) )
    return( it->second.getSymbolOffset() );

  return( -1 );
};


/*
  getComposedParameterBufferOffset returns the offset of the buffer that is used
  to store a copy of the given composed type object which shall be passed by
  value.

  Composed type objects may be passed by value in registers or by pointer, but
  in any case, after having received the object in any of these forms, the
  callee must copy the composed type object to its local stack to make sure that
  pass-by-value behaviour is achieved.
*/
int RV32_Stack::getComposedParameterBufferOffset( const IR_Symbol &sym ) const
{
  DSTART( BOOST_CURRENT_FUNCTION );

  // TODO: Is all this stuff relevant and correct for RISC-V?!? Maybe consider
  //       the ARM implementation as another, more appropriate code example?
  ufAssertT(
    sym.getSymbolTable().getFunction(), "Symbol is no function argument." );
  ufAssertT(
    dynamic_cast<IR_ComposedType *>( &sym.getType() ),
    "Symbol is not of composed type." );

  // TODO: If it is known that the callee will never modify the passed struct,
  //       then we could omit the copying, but this requires a thorough analyis
  //       that is best done at the high level IR via DefUse-sets.

  auto it = mComposedParameterBufferMap.find( const_cast<IR_Symbol *>( &sym ) );

  return(
    it == mComposedParameterBufferMap.end() ?
      0 : getSymbolOffset( *(it->second) ) );
};


/*
  getSymbolType returns the type of a given symbol.
*/
RV32_SymbolInfo::Type RV32_Stack::getSymbolType( const IR_Symbol &sym ) const
{
  DSTART( BOOST_CURRENT_FUNCTION );

  auto it = mSymbolMap.find( const_cast<IR_Symbol *>( &sym ) );
  return( it->second.getSymbolType() );
};


/*
  getArgumentPos returns the position of a symbol if it is used as function
  argument passed via a register.
*/
int RV32_Stack::getArgumentPos( const IR_Symbol &sym ) const
{
  DSTART( BOOST_CURRENT_FUNCTION );

  auto it = mSymbolMap.find( const_cast<IR_Symbol *>( &sym ) );
  return( it == mSymbolMap.end() ? -1 : it->second.getSymbolArgumentPos() );
};


/*
  setSymbolReg associates the given virtual WIR register with an IR symbol.
*/
void RV32_Stack::setSymbolReg( const IR_Symbol &sym,
                               const WIR::WIR_VirtualRegister &r )
{
  DSTART( BOOST_CURRENT_FUNCTION );

  auto it = mSymbolMap.find( const_cast<IR_Symbol *>( &sym ) );

  if ( it != mSymbolMap.end() )
    it->second.setSymbolReg( r );
};


/*
  getSymbolReg returns the virtual WIR register associated with the given IR
  symbol.

  getSymbolReg fails with an assertion if no WIR register has previously been
  associated with a symbol.
*/
WIR::WIR_VirtualRegister &RV32_Stack::getSymbolReg( const IR_Symbol &sym ) const
{
  DSTART( BOOST_CURRENT_FUNCTION );

  auto it = mSymbolMap.find( const_cast<IR_Symbol *>( &sym ) );
  return( it->second.getSymbolReg() );
};


/*
  isSymbolRegSet returns whether a WIR register has previously been associated
  with an IR symbol.
*/
bool RV32_Stack::isSymbolRegSet( const IR_Symbol &sym ) const
{
  DSTART( BOOST_CURRENT_FUNCTION );

  auto it = mSymbolMap.find( const_cast<IR_Symbol *>( &sym ) );
  return( it == mSymbolMap.end() ? false : it->second.isSymbolRegSet() );
};


/*
  setAddrReg associates a given virtual RISC-V register as address register of
  an IR symbol.
*/
void RV32_Stack::setAddrReg( const IR_Symbol &sym, const WIR::RV_RegV &r )
{
  DSTART( BOOST_CURRENT_FUNCTION );

  auto it = mSymbolMap.find( const_cast<IR_Symbol *>( &sym ) );

  if ( it != mSymbolMap.end() )
    it->second.setAddrReg( r );
};


/*
  getAddrReg returns the virtual RISC-V address register associated with a given
  IR symbol.
*/
WIR::RV_RegV &RV32_Stack::getAddrReg( const IR_Symbol &sym ) const
{
  DSTART( BOOST_CURRENT_FUNCTION );

  auto it = mSymbolMap.find( const_cast<IR_Symbol *>( &sym ) );
  return( it->second.getAddrReg() );
};


/*
  isAddrRegSet returns whether a RISC-V address register has previously been
  associated with an IR symbol.
*/
bool RV32_Stack::isAddrRegSet( const IR_Symbol &sym ) const
{
  DSTART( BOOST_CURRENT_FUNCTION );

  auto it = mSymbolMap.find( const_cast<IR_Symbol *>( &sym ) );
  return( it->second.isAddrRegSet() );
};


/*
  setAddressTaken sets whether the address of a given symbol was taken.

  This information is valid only for symbols being function arguments.
*/
void RV32_Stack::setAddressTaken( const IR_Symbol &sym, bool t )
{
  DSTART( BOOST_CURRENT_FUNCTION );

  auto it = mSymbolMap.find( const_cast<IR_Symbol *>( &sym ) );

  if ( it != mSymbolMap.end() )
    it->second.setAddressTaken( t );
};


/*
  isAddressTaken returns whether the address of a given symbol was taken.

  This information is valid only for symbols being function arguments.
*/
bool RV32_Stack::isAddressTaken( const IR_Symbol &sym ) const
{
  DSTART( BOOST_CURRENT_FUNCTION );

  auto it = mSymbolMap.find( const_cast<IR_Symbol *>( &sym ) );
  return( it == mSymbolMap.end() ? false : it->second.isAddressTaken() );
};


/*
  setStoreInstructionsGenerated sets whether store instructions are already
  generated for a symbol at the beginning of a function.

  This information is valid only for symbols being function arguments, whose
  address have been taken.
*/
void RV32_Stack::setStoreInstructionsGenerated( const IR_Symbol &sym,
                                                bool stInsGenerated )
{
  DSTART( BOOST_CURRENT_FUNCTION );

  auto it = mSymbolMap.find( const_cast<IR_Symbol *>( &sym ) );

  if ( it != mSymbolMap.end() )
    it->second.setStoreInstructionsGenerated( stInsGenerated );
};


/*
  areStoreInstructionsGenerated returns whether store instructions are already
  generated for a symbol at the beginning of a function.

  This information is valid only for symbols being function arguments, whose
  address have been taken.
*/
bool RV32_Stack::areStoreInstructionsGenerated( const IR_Symbol &sym ) const
{
  DSTART( BOOST_CURRENT_FUNCTION );

  auto it = mSymbolMap.find( const_cast<IR_Symbol *>( &sym ) );
  return(
    it == mSymbolMap.end() ?
      false : it->second.areStoreInstructionsGenerated() );
};


/*
  setComposedPushed indicates that a given symbol of composed type is already
  pushed onto the stack.
*/
void RV32_Stack::setComposedPushed( const IR_Symbol &sym )
{
  DSTART( BOOST_CURRENT_FUNCTION );

  mComposedPushed.insert( &sym );
};


/*
  isComposedPushed returns whether a symbol of composed type is already pushed
  onto the stack.
*/
bool RV32_Stack::isComposedPushed( const IR_Symbol &sym ) const
{
  DSTART( BOOST_CURRENT_FUNCTION );

  return( mComposedPushed.count( &sym ) );
};


/*
  setComposedPushCostAdded indicates that the costs for pushing a symbol of
  composed type to the local stack is already added.
*/
void RV32_Stack::setComposedPushCostAdded( const IR_Symbol &sym )
{
  DSTART( BOOST_CURRENT_FUNCTION );

  mComposedPushCostAdded.insert( &sym );
};


/*
  getComposedPushCostAdded returns whether the costs for pushing a symbol of
  composed type to the local stack is already added.
*/
bool RV32_Stack::getComposedPushCostAdded( const IR_Symbol &sym ) const
{
  DSTART( BOOST_CURRENT_FUNCTION );

  return( mComposedPushCostAdded.count( &sym ) );
};


/*
  isPassedThroughRegister returns whether a given function argument symbol will
  be passed via a register.
*/
int RV32_Stack::isPassedThroughRegister( const IR_Symbol &sym )
{
  DSTART( "static int RV32_Stack::isPassedThroughRegister(const IR_Symbol&)" );

  // TODO: Use this method throughout the codeselector. Locations:
  // * all other function in this class
  // * argument passing rules
  // * symbolExp rules

  // If the symbol is not a function argument, then we return -1
  if ( sym.getSymbolTable().getFunction() == nullptr )
    return( -1 );
  // Else get the function type and determine the parameter index of the symbol.
  IR_FunctionType &funcType =
    sym.getSymbolTable().getFunction()->getSymbol().getType();
  const list<IR_Symbol*> &symbols = sym.getSymbolTable().getSymbols();

  int paramIndex = 0;
  for ( auto *theSym : symbols )
    if ( theSym == &sym )
      break;
    else
      ++paramIndex;

  ufAssertT( paramIndex < (int) symbols.size(), "Invalid parameter index!" );

  // Call the main "isPassedThroughRegister" function in order to get the
  // indexes' actual register number.
  return( isPassedThroughRegister( funcType, paramIndex ) );
};


/*
  getParameterStackFrameSize returns the byte size of the stack region needed by
  the parameters of a given function type.
*/
int RV32_Stack::getParameterStackFrameSize( const IR_FunctionType &funcType )
{
  DSTART(
    "static int RV32_Stack::getParameterStackFrameSize(const "
    "IR_FunctionType&)" );

  // The beginning of the parameter stack area is the beginning of the caller's
  // stack frame, so it is aligned at 16-byte borders. Therefore, the first
  // align call will not influence the computed size.

  int stackSize = 0;
  int parameterIndex = 0;

  for ( auto *argType : funcType.getArgumentTypes() ) {
    if ( isPassedThroughRegister( funcType, parameterIndex ) == -1 ) {
      auto currentType = argType->getType();

      switch ( currentType ) {
        case IR_Type::CHAR:
        case IR_Type::UNSIGNED_CHAR:
        case IR_Type::BOOL: {
          stackSize += intBytes;
          break;
        }

        case IR_Type::SHORT:
        case IR_Type::UNSIGNED_SHORT: {
          align( stackSize, RVIR_CONFIGURATION->alignmentShort );
          stackSize += intBytes;
          break;
        }

        case IR_Type::INT:
        case IR_Type::UNSIGNED_INT: {
          align( stackSize, RVIR_CONFIGURATION->alignmentInt );
          stackSize += getStackSize( *argType );
          break;
        }

        case IR_Type::LONG:
        case IR_Type::UNSIGNED_LONG: {
          align( stackSize, RVIR_CONFIGURATION->alignmentLong );
          stackSize += getStackSize( *argType );
          break;
        }

        case IR_Type::LONG_LONG:
        case IR_Type::UNSIGNED_LONG_LONG: {
          align( stackSize, RVIR_CONFIGURATION->alignmentLongLong );
          stackSize += getStackSize( *argType );
          break;
        }

        case IR_Type::LONG_DOUBLE: {
          align( stackSize, RVIR_CONFIGURATION->alignmentLongDouble );
          stackSize += getStackSize( *argType );
          break;
        }

        case IR_Type::DOUBLE: {
          align( stackSize, RVIR_CONFIGURATION->alignmentDouble );
          stackSize += getStackSize( *argType );
          break;
        }

        case IR_Type::FLOAT: {
          align( stackSize, RVIR_CONFIGURATION->alignmentFloat );
          stackSize += getStackSize( *argType );
          break;
        }

        default: {
          throw
            ufFatalError(
              "Unsupported type of element for calculation of stack size." );
          break;
        }
      }
    }

    parameterIndex++;
  }

  return( stackSize );
};


/*
  getStackSize computes the byte size of a given IR type if pushed on the stack.
*/
int RV32_Stack::getStackSize( const IR_Type &t )
{
  DSTART( "static int RV32_Stack::getStackSize(const IR_Type&)" );

  int result = 0;

  switch ( t.getType() ) {
    case IR_Type::VOID: {
      result = 0;
      break;
    }

    case IR_Type::BOOL: {
      result =
        ( RV32I::boolBytes < RVIR_CONFIGURATION->alignmentBool ) ?
          RVIR_CONFIGURATION->alignmentBool : RV32I::boolBytes;
      break;
    }

    case IR_Type::CHAR: {
      result =
        ( RV32I::charBytes < RVIR_CONFIGURATION->alignmentChar ) ?
          RVIR_CONFIGURATION->alignmentChar : RV32I::charBytes;
      break;
    }

    case IR_Type::UNSIGNED_CHAR: {
      result =
        ( RV32I::charBytes < RVIR_CONFIGURATION->alignmentChar ) ?
          RVIR_CONFIGURATION->alignmentChar : RV32I::charBytes;
      break;
    }

    case IR_Type::INT: {
      result =
        ( RV32I::intBytes < RVIR_CONFIGURATION->alignmentInt ) ?
          RVIR_CONFIGURATION->alignmentInt : RV32I::intBytes;
      break;
    }

    case IR_Type::UNSIGNED_INT: {
      result =
        ( RV32I::intBytes < RVIR_CONFIGURATION->alignmentInt ) ?
          RVIR_CONFIGURATION->alignmentInt : RV32I::intBytes;
      break;
    }

    case IR_Type::LONG: {
      result =
        ( RV32I::longBytes < RVIR_CONFIGURATION->alignmentLong ) ?
          RVIR_CONFIGURATION->alignmentLong : RV32I::longBytes;
      break;
    }

    case IR_Type::UNSIGNED_LONG: {
      result =
        ( RV32I::longBytes < RVIR_CONFIGURATION->alignmentLong ) ?
          RVIR_CONFIGURATION->alignmentLong : RV32I::longBytes;
      break;
    }

    case IR_Type::SHORT: {
      result =
        ( RV32I::shortBytes < RVIR_CONFIGURATION->alignmentShort ) ?
          RVIR_CONFIGURATION->alignmentShort : RV32I::shortBytes;
      break;
    }

    case IR_Type::UNSIGNED_SHORT: {
      result =
        ( RV32I::shortBytes < RVIR_CONFIGURATION->alignmentShort ) ?
          RVIR_CONFIGURATION->alignmentShort : RV32I::shortBytes;
      break;
    }

    case IR_Type::LONG_LONG: {
      result =
        ( RV32I::longLongBytes < RVIR_CONFIGURATION->alignmentLongLong ) ?
          RVIR_CONFIGURATION->alignmentLongLong : RV32I::longLongBytes;
      break;
    }

    case IR_Type::UNSIGNED_LONG_LONG: {
      result =
        ( RV32I::longLongBytes < RVIR_CONFIGURATION->alignmentLongLong ) ?
          RVIR_CONFIGURATION->alignmentLongLong : RV32I::longLongBytes;
      break;
    }

    case IR_Type::LONG_DOUBLE: {
      result =
        ( RV32I::longDoubleBytes < RVIR_CONFIGURATION->alignmentLongDouble ) ?
          RVIR_CONFIGURATION->alignmentLongDouble : RV32I::longDoubleBytes;
      break;
    }

    case IR_Type::DOUBLE: {
      result =
        ( RV32I::doubleBytes < RVIR_CONFIGURATION->alignmentDouble ) ?
          RVIR_CONFIGURATION->alignmentDouble : RV32I::doubleBytes;
      break;
    }

    case IR_Type::FLOAT: {
      result =
        ( RV32I::floatBytes < RVIR_CONFIGURATION->alignmentFloat ) ?
          RVIR_CONFIGURATION->alignmentFloat : RV32I::floatBytes;
      break;
    }

    case IR_Type::POINTER:
    case IR_Type::ARRAY: {
      result =
        ( RV32I::pointerBytes < RVIR_CONFIGURATION->alignmentPointer ) ?
          RVIR_CONFIGURATION->alignmentPointer : RV32I::pointerBytes;
      break;
    }

    default: {
      throw ufFatalError( "Unsupported type of function argument." );
      break;
    }
  }

  return( result );
};


/*
  isPassedThroughRegister returns whether the nth argument of an IR function
  type will be passed by register.
*/
int RV32_Stack::isPassedThroughRegister( const IR_FunctionType &funcType,
                                         int parameterIndex )
{
  DSTART(
    "static int RV32_Stack::isPassedThroughRegister(const IR_FunctionType&, "
    "int)" );

  // Helper variables for keeping track of the used up registers
  const int numRegs = 8;
  bool regMap[numRegs] = { false, false, false, false, false, false, false,
                           false };
  unsigned int thisPos = 0;
  int currentParamIndex = 0;

  const list<IR_Type *> &paramTypes = funcType.getArgumentTypes();
  for ( list<IR_Type *>::const_iterator it  = paramTypes.begin();
        it != paramTypes.end(); ++it ) {
    // Whether the current symbol is passed by register
    bool passedThroughRegister = false;

    // A small lambda to check for free registers.
    auto checkForFreeReg = [&thisPos, &regMap, &passedThroughRegister] () {
      int firstFreeReg = numRegs;
      for ( int loop = numRegs-1; loop >= 0; loop-- )
        if ( !regMap[loop] )
          firstFreeReg = loop;

      passedThroughRegister = firstFreeReg < numRegs;
      if ( passedThroughRegister ) {
        thisPos = static_cast< unsigned int >( firstFreeReg );
        regMap[firstFreeReg] = true;
      }
    };

    checkForFreeReg(); // Call lambda
    // If the current symbol was the one that we searched for,
    // then we have our return value
    if ( currentParamIndex == parameterIndex ) {
      if ( passedThroughRegister ) {
        // Check for ilegal argument position
        ufAssert( thisPos <= (numRegs-1) );
        // RV32I argument registers are registers x10-x17
        return thisPos + 10;
      } else {
        return -1;
      }
    } else {
      currentParamIndex++;
    }
  }
  // We did not find the parameter index, though we iterated over all parameters
  // -> the given parameter index must be invalid
  ufAssertT( 0, "Internal error: Invalid parameter index!" );
  return -1;
}


//
// Private class methods
//

/*
  This operator checks whether a given symbol expression is used as inline
  assembly operand with memory constraint.
*/
bool RV32_Stack::memRefByInlineAsm::operator()( IR_SymbolExp *exp )
{
  DSTART( "bool RV32_Stack::memRefByInlineAsm::operator()(IR_SymbolExp*)" );

  auto *asmStmt = dynamic_cast<IR_AsmStmt *>( &( exp->getStmt() ) );

  if ( asmStmt == nullptr )
    return( false );

  auto *operand = asmStmt->getOperand( asmStmt->getOperandExpIndex( exp ) );
  return(
    operand->getArgumentType().find( 'm' ) != operand->getArgumentType().npos );
};


list<int> RV32_Stack::getCalleeSavedRegs()
{
  DSTART( "list<int> RV32_Stack::getCalleeSavedRegs()" );
  // We can define here, which registers should be saved by default on each
  // function entry.
  list<int> regs = { 1,  // ra
           8,  // fp/s0
           9,  // s1
           18, // s2
           19, // s3
           20, // s4
           21, // s5
           22, // s6
           23, // s7
           24, // s8
           25, // s9
           26, // s10
           27};// s11
  return regs;
};


void RV32_Stack::initStack( const WIR::RV32IMC &p, WIR::WIR_Function &f,
                            WIR::WIR_BasicBlock &b , int stackAdjOffset)
{
  DSTART( "void RV32_Stack::initStack( const WIR::RV32IMC &p, WIR::WIR_Function"
          " &f, WIR::WIR_BasicBlock &b )" );

  auto &sp = RVINSTRUCTIONS.createReg();
  f.insertPrecolor( sp, p.SP() );
  auto &fp = RVINSTRUCTIONS.createReg();
  f.insertPrecolor( fp, p.x8() );

  //Adjust sp by the amount needed for the stack frame (without spills)
  adjustStackFrame(p, *(f.begin()), stackAdjOffset );

  //Number of callee saved registers and resulting sp offset
  list<int> calleeSavedRegs = getCalleeSavedRegs();
  ufAssertT( calleeSavedRegs.size() >= 2,
    "At least the ra and fp have to be saved by the callee!" );
  int offset = stackAdjOffset;

  // Store callee saved registers on the stack
  for ( int nSreg : calleeSavedRegs ) {
    auto &sreg = RVINSTRUCTIONS.createReg();
    bindToPHREG( sreg, nSreg );
    offset -= 4; // decrement before
    b.pushBackInstruction(
    { { RV32I::OpCode::SW, RV32I::OperationFormat::RC12R_2,
        new WIR_RegisterParameter( sreg, WIR_Usage::use ),
        new RV_Const12_Signed( offset ),
        new WIR_RegisterParameter( sp, WIR_Usage::use ) } } );
  }

  //Set new fp for callee
  b.pushBackInstruction(
    { { RV32I::OpCode::ADDI, RV32I::OperationFormat::RRC12_1,
        new WIR_RegisterParameter( fp, WIR_Usage::def ),
        new WIR_RegisterParameter( sp, WIR_Usage::use ),
        new RV_Const12_Signed( stackAdjOffset - 8 ) } } );

  ufAssertT( offset == stackAdjOffset - (calleeSavedRegs.size() * 4),
    "Illegal stack pointer offset after pushing function arguments!" );
};


void RV32_Stack::adjustStackFrame( const WIR::RV32IMC &proc,
                                   WIR::WIR_BasicBlock &b, int offset )
{
  DSTART( "static void RV32_Stack::adjustStackFrame(const RV32IMC&, "
          "WIR_BasicBlock&, int)" );

  if ( offset == 0 )
    return;

  // The stack poiner MUST stay aligned at 16-byte borders all the time.
  ufAssertT( offset % 16 == 0, "Illegal stack pointer modification!" );

  // Divide offset by 16 since CADDI16SP itself multiplies by 16
  //TODO CADDI16SP offset = offset / 16;

  if ( ( -offset >= RV_Const12_Signed::getMinValue( 12 ) ) &&
       ( -offset <= RV_Const12_Signed::getMaxValue( 12 ) ) ){
    //TODO CADDI16SP
    // b.pushFrontInstruction(
    //   { { RV32IC::OpCode::CADDI16SP, RV32IC::OperationFormat::SRC6_4,
    //       new WIR_RegisterParameter( proc.SP(), WIR_Usage::defuse ),
    //       new RV_Const6_Signed( -offset ) } } );
    b.pushFrontInstruction(
      { { RV32I::OpCode::ADDI, RV32I::OperationFormat::RRC12_1,
          new WIR_RegisterParameter(  proc.SP(), WIR_Usage::def ),
          new WIR_RegisterParameter(  proc.SP(), WIR_Usage::use ),
          new RV_Const12_Signed( -offset ) } } );

  } else {
    auto p = RV32_InstructionFactory::splitOffset( -offset );
    //TODO CADDI16SP
    // b.pushFrontInstruction(
    //   { { RV32IC::OpCode::CADDI16SP, RV32IC::OperationFormat::SRC6_4,
    //       new WIR_RegisterParameter( proc.SP(), WIR_Usage::defuse ),
    //       new RV_Const6_Signed( p.second ) } } );
    b.pushFrontInstruction(
      { { RV32I::OpCode::ADDI, RV32I::OperationFormat::RRC12_1,
          new WIR_RegisterParameter(  proc.SP(), WIR_Usage::def ),
          new WIR_RegisterParameter(  proc.SP(), WIR_Usage::use ),
          new RV_Const12_Signed( p.second ) } } );
  }
};


/*
  mapFunctionArguments maps the arguments of a given function into the internal
  symbol map which is basically the stack.
*/
void RV32_Stack::mapFunctionArguments( const IR_Function &f )
{
  DSTART( "void RV32_Stack::mapFunctionArguments(const IR_Function&)" );

  int oldOffset = mOffset;

  for ( auto *argSym : f.functionArguments.getSymbols() ) {
    auto currentType = argSym->getType().getType();

    RV32_SymbolInfo SymInfo
      { RV32_SymbolInfo::Type::R_ARGUMENT, -1, -1, false, false };

    // The address-taken information was already computed during
    // mapFunctionSymbols because the buffer spaces for those symbols must be in
    // the local variable stack area.
    auto *existingSymInf = getSymbolInfo( *argSym );
    if ( existingSymInf )
      SymInfo = *existingSymInf;

    // Check whether there is a free register available.
    const int myRegister = isPassedThroughRegister( *argSym );

    SymInfo.setSymbolType( RV32_SymbolInfo::Type::R_ARGUMENT );

    if ( myRegister != -1 ) {
      SymInfo.setSymbolArgumentPos( myRegister + 1 );
    } else {

      /*
        TODO: Currently, all integer types with bitwidth < 32 bit are stored
              in a word, which wastes stack space. Such arguments (char /
              short) should be stored in a byte or halfword. The argument
              handling and the symbol expression rules would have to be adapted
              adapted to this. Also, if the prototype of a function is not
              known (only possible for calls to external functions), then we
              must use the default argument promotions, which force us to
              extend all values to 32-bit integers or doubles.
      */

      switch ( currentType ) {
        case IR_Type::CHAR:
        case IR_Type::UNSIGNED_CHAR:
        case IR_Type::BOOL: {
          // TODO: Why don't we align this one? (Short is int-aligned too)
          SymInfo.setSymbolOffset( mOffset );

          // Treat as integer...
          mOffset += RV32I::intBytes;
          break;
        }

        case IR_Type::SHORT:
        case IR_Type::UNSIGNED_SHORT: {
          align( mOffset, RVIR_CONFIGURATION->alignmentInt );
          SymInfo.setSymbolOffset( mOffset );

          // Treat as integer...
          mOffset += RV32I::intBytes;
          break;
        }

        case IR_Type::INT:
        case IR_Type::UNSIGNED_INT: {
          align( mOffset, RVIR_CONFIGURATION->alignmentInt );
          SymInfo.setSymbolOffset( mOffset );

          mOffset += RV32I::intBytes;
          break;
        }

        case IR_Type::LONG:
        case IR_Type::UNSIGNED_LONG: {
          align( mOffset, RVIR_CONFIGURATION->alignmentLong );
          SymInfo.setSymbolOffset( mOffset );

          mOffset += RV32I::longBytes;
          break;
        }

        case IR_Type::LONG_LONG:
        case IR_Type::UNSIGNED_LONG_LONG: {
          align( mOffset, RVIR_CONFIGURATION->alignmentLongLong );
          SymInfo.setSymbolOffset( mOffset );

          mOffset += RV32I::longLongBytes;
          break;
        }

        case IR_Type::LONG_DOUBLE: {
          align( mOffset, RVIR_CONFIGURATION->alignmentLongDouble );
          SymInfo.setSymbolOffset( mOffset );

          mOffset += RV32I::longDoubleBytes;
          break;
        }

        case IR_Type::DOUBLE: {
          align( mOffset, RVIR_CONFIGURATION->alignmentDouble );
          SymInfo.setSymbolOffset( mOffset );

          mOffset += RV32I::doubleBytes;
          break;
        }

        case IR_Type::FLOAT: {
          align( mOffset, RVIR_CONFIGURATION->alignmentFloat );
          SymInfo.setSymbolOffset( mOffset );

          mOffset += RV32I::floatBytes;
          break;
        }

        default: {
          throw ufFatalError( "Unsupported stack argument detected." );
          break;
        }
      }
    }

    mSymbolMap[ argSym ] = SymInfo;
  }

  // Assert that the two methods for computing the parameter stack frames are
  // coherent.
  const int sizeDifference =
    ( mOffset - oldOffset ) -
      getParameterStackFrameSize( f.getSymbol().getType() );
  ufAssertT(
    sizeDifference == 0,
    "Incoherent results for parameter stack frame size computation!" );
};


/*
  mapFunctionSymbols maps the local symbols of a given function into the
  internal symbol map which is basically the stack.
*/
void RV32_Stack::mapFunctionSymbols( IR_Function &f )
{
  DSTART( "void RV32_Stack::mapFunctionSymbols(IR_Function&)" );

  // 1.) Handle all the normal symbols that are present in the IR.
  f.getTopCompound().localSymbolTable.iterateSymbolTables(
    symtabIterator, this );

  mBiggestReturn = 0;
  f.getTopCompound().iterateStatements(
    statementIterator, this );

  if ( mBiggestReturn ) {
    // Create and map a new artificial symbol that represents the buffer space
    IR_InternalSymbol *bufferSym = new IR_InternalSymbol( *mBiggestReturn );

    mapNewStackSymbol( *bufferSym, this );
    mCallResultBufferMap[ &f ] = bufferSym;
  }

  // 4.) Handle the buffer space that is needed to store the arguments that were
  //     passed by register, but whose address was taken, and who thus require a
  //     stack location. In the above cases, we have used a new symbol to
  //     represent the buffer space. But here, we directly attach the buffer
  //     information to the original symbol.
  for ( auto *argSym : f.functionArguments.getSymbols() ){
    // Consider only symbols that are not passed via the stack anyways.
    if ( isPassedThroughRegister( *argSym ) != -1 ) {
      auto currentType = argSym->getType().getType();

      // We only fill the symbol info with the address-taken specific values
      // here, the rest is done in mapFunctionArguments.
      RV32_SymbolInfo SymInfo
        { RV32_SymbolInfo::Type::R_ARGUMENT, -1, -1, false, false };

      if ( !argSym->getAddrTaken().empty() ) {
        SymInfo.setAddressTaken( true );

        switch ( currentType ) {
          case IR_Type::BOOL: {
            align( mOffset, RVIR_CONFIGURATION->alignmentBool );
            SymInfo.setSymbolOffset( mOffset );

            mOffset += RV32I::boolBytes;
            break;
          }

          case IR_Type::CHAR:
          case IR_Type::UNSIGNED_CHAR: {
            align( mOffset, RVIR_CONFIGURATION->alignmentChar );
            SymInfo.setSymbolOffset( mOffset );

            mOffset += RV32I::charBytes;
            break;
          }

          case IR_Type::SHORT:
          case IR_Type::UNSIGNED_SHORT: {
            align( mOffset, RVIR_CONFIGURATION->alignmentShort );
            SymInfo.setSymbolOffset( mOffset );

            mOffset += RV32I::shortBytes;
            break;
          }

          case IR_Type::INT:
          case IR_Type::UNSIGNED_INT: {
            align( mOffset, RVIR_CONFIGURATION->alignmentInt );
            SymInfo.setSymbolOffset( mOffset );

            mOffset += RV32I::intBytes;
            break;
          }

          case IR_Type::LONG:
          case IR_Type::UNSIGNED_LONG: {
            align( mOffset, RVIR_CONFIGURATION->alignmentLong );
            SymInfo.setSymbolOffset( mOffset );

            mOffset += RV32I::longBytes;
            break;
          }

          case IR_Type::LONG_LONG:
          case IR_Type::UNSIGNED_LONG_LONG: {
            align( mOffset, RVIR_CONFIGURATION->alignmentLongLong );
            SymInfo.setSymbolOffset( mOffset );

            mOffset += RV32I::longLongBytes;
            break;
          }

          case IR_Type::LONG_DOUBLE: {
            align( mOffset, RVIR_CONFIGURATION->alignmentLongDouble );
            SymInfo.setSymbolOffset( mOffset );

            mOffset += RV32I::longDoubleBytes;
            break;
          }

          case IR_Type::DOUBLE: {
            align( mOffset, RVIR_CONFIGURATION->alignmentDouble );
            SymInfo.setSymbolOffset( mOffset );

            mOffset += RV32I::doubleBytes;
            break;
          }

          case IR_Type::FLOAT: {
            align( mOffset, RVIR_CONFIGURATION->alignmentFloat );
            SymInfo.setSymbolOffset( mOffset );

            mOffset += RV32I::floatBytes;
            break;
          }

          default: {
            throw ufFatalError( "Unsupported stack argument detected." );
            break;
          }
        }
      }

      if ( SymInfo.isAddressTaken() )
        mSymbolMap[ argSym ] = SymInfo;
    }
  }
};


/*
  mapNewStackSymbol creates a new stack entry for a given symbol.
*/
void RV32_Stack::mapNewStackSymbol( IR_Symbol &sym, RV32_Stack *s )
{
  DSTART(
    "static void RV32_Stack::mapNewStackSymbol(IR_Symbol&, RV32_Stack*)" );

  // TODO: This method desperately needs some cleanup

  int stackSize = 0;

  auto currentType = sym.getType().getType();
  auto *thePointerType = dynamic_cast<IR_PointerType *>( &sym.getType() );
  auto *theArrayType = dynamic_cast<IR_ArrayType *>( &sym.getType() );
  
  bool isArrayType =
    ( thePointerType != nullptr ) && ( theArrayType != nullptr );
  bool isPointerType =
    ( thePointerType != nullptr ) && ( theArrayType == nullptr );


  // If the address of a local scalar variable is taken, we have to store the
  // symbol on the stack.
  if ( ( !sym.getAddrTaken().empty() &&

         // These cases are handled later.
         ( currentType != IR_Type::STRUCT ) &&
         ( currentType != IR_Type::UNION ) && ( !isArrayType ) &&

         // A pointer's address is only treated as taken when it is taken
         // directly. If it is taken in an expression like, e.g., "*(a + 12)",
         // then it is not treated as taken.
         ( !isPointerType || isAddressDirectlyTaken( sym ) ) ) ||

       // If the address is used by inline assembly we have to store the symbol
       // on the stack as well.
       find_if(
        sym.getUsedIn().begin(), sym.getUsedIn().end(), memRefByInlineAsm() ) !=
          sym.getUsedIn().end() ) {
    RV32_SymbolInfo SymInfo
      { RV32_SymbolInfo::Type::LOCAL_STACK_VAR, -1, -1, false, false };

    stackSize = getStackSize( sym.getType() );

    if ( sym.getType().isArithmeticType() || isPointerType ) {
      align( s->mOffset, sym.getType().alignment() );

      SymInfo.setSymbolOffset( s->mOffset );
      s->mOffset += stackSize;
    } else
      ufAssertT( false, "Address of an unsupported type taken." );

    s->mSymbolMap[ &sym ] = SymInfo;
  } else {
    // The enumeration values reside in the same name space as their
    // corresponding type's tag symbol. Since they are handled the the enum
    // type is analyzed (see above), there no need to read them again.
    // Thus, they are omitted here.
    if ( sym.getEnumType() == 0 ) {
      // If the address of a local variable is not taken, we can store the
      // symbol as virtual register.
      RV32_SymbolInfo symInfo
        { RV32_SymbolInfo::Type::LOCAL_VAR, -1, -1, false, false };

      s->mSymbolMap[ &sym ] = symInfo;
    }
  }
};


/*
  isAddressDirectlyTaken determines whether the address of a symbol was directly
  taken as in <tt>&a</tt> for symbol <tt>a</tt>.

  ICD-C also classifies symbol <tt>a</tt>'s address as taken if the symbol is
  used in an expression like, e.g., <tt>&(a[ 12 ])</tt>. isAddressDirectlyTaken
  can be used to distinguish these two cases.
*/
bool RV32_Stack::isAddressDirectlyTaken( const IR_Symbol &sym )
{
  DSTART( "static bool RV32_Stack::isAddressDirectlyTaken(const IR_Symbol&)" );

  // Iterate all the locations where the address is reported to be taken.
  for ( auto *sExp : sym.getAddrTaken() ) {
    // Find the '&' operator in the current expression tree.
    IR_Exp *e = sExp;
    auto *unaryExp = dynamic_cast<IR_UnaryExp *>( e );

    while ( !unaryExp || ( unaryExp->getOperator() != IR_UnaryExp::ADDR ) ) {
      // The following assertion is required for the clang static analyzer.
      ufAssert( e != nullptr );

      e = e->getParent();
      unaryExp = dynamic_cast<IR_UnaryExp *>( e );

      if ( e == nullptr )
        break;
    }

    if ( e != nullptr ) {
      // Check if the address operator is applied directly to the symbol itself.
      IR_Exp *child = &( unaryExp->getOp() );
      auto *symExp = dynamic_cast<IR_SymbolExp *>( child );

      if ( symExp && ( symExp->getSymbol() == sym ) )
        // Only then, the address is directly taken.
        return( true );
    }
  }

  return( false );
};


/*
  align aligns a given integer value at a specified alignment width.
*/
void RV32_Stack::align( int &toAlign, int alignmentWidth )
{
  DSTART( "static void RV32_Stack::align(int&, int)" );

  int remainder = toAlign % alignmentWidth;

  if ( remainder != 0 ) {
    int missingAlignment = alignmentWidth - remainder;
    toAlign += missingAlignment;
  }
};


/*
  neededRegs computes the number of registers needed to hold a value of a given
  type.
*/
int RV32_Stack::neededRegs( const IR_Type &t ) const
{
  DSTART( BOOST_CURRENT_FUNCTION );

  int stackSize = getStackSize( t );
  int padding = ( 4 - ( stackSize % 4 ) ) % 4;
  int neededRegs = ( stackSize + padding ) / 4;

  return( neededRegs );
};


/*
  symtabIterator iterates a given symbol table and stores all symbols in
  mSymbolMap.
*/
void RV32_Stack::symtabIterator( IR_SymbolTable &symtab, void *p )
{
  DSTART( "static void RV32_Stack::symtabIterator(IR_SymbolTable&, void*)" );

  auto *s = reinterpret_cast<RV32_Stack *>( p );

  for ( auto *sym : symtab.getSymbols() ) {
    auto *sourceSym = dynamic_cast<IR_SourceSymbol *>( sym );
    auto *intSym = dynamic_cast<IR_InternalSymbol *>( sym );

    if ( ( sourceSym || intSym ) &&
         ( sym->getType().getStorageClass() != IR_Type::STATIC ) &&
         !sym->isGlobal() )
      mapNewStackSymbol( *sym, s );
  }

  // Some padding might be required to get 4-bytes blocks.
  align( s->mOffset, 4 );
};


/*
  statementIterator iterates a given statement and looks for that call
  expression having the biggest composed type as return value.
*/
void RV32_Stack::statementIterator( IR_Stmt &stmt, void *p )
{
  DSTART( "static void RV32_Stack::statementIterator(IR_Stmt&, void*)" );

  stmt.iterateExpressions( expressionIterator, p );
};


/*
  expressionIterator iterates a given expression and looks for that call
  expression having the biggest composed type as return value.
*/
void RV32_Stack::expressionIterator( IR_Exp &exp, void *p )
{
  DSTART( "static void RV32_Stack::expressionIterator(IR_Exp&, void*)" );

  auto *callExp = dynamic_cast<IR_CallExp *>( &exp );

  if ( callExp ) {
    auto *s = reinterpret_cast<RV32_Stack *>( p );
    auto *compType = dynamic_cast<IR_ComposedType *>( &callExp->getType() );

    // Check if the type is bigger than the biggest that we've seen so far.
    if ( compType &&
         ( !s->mBiggestReturn ||
           getStackSize( *compType ) > getStackSize( *(s->mBiggestReturn) ) ) )
      s->mBiggestReturn = compType;
  }
};

}       // namespace RV32
