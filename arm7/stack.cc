/*

   This source file belongs to the

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


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wcc.h>
#endif

// Include standard headers
#include <sstream>
#include <cassert>
#include <cmath>
#include <stack>
#include <deque>
#include <llir3/llir3.h>

// Include boost headers
#include <boost/current_function.hpp>

// Include LIBUSEFUL headers
#include <libuseful/exceptions.h>
#include <libuseful/io.h>

// Include WIR headers
#include <wir/wir.h>
#include <arch/arm/armv6.h>

// Include misc headers
#include <misc/auxfuncs.h>

// Include private headers
#include "stack.h"
#include "registrar.h"


using namespace std;
using namespace WIR;


//! Short helper function
static inline bool typeIsStoredInEReg( const IR_Type &type )
{
  const enum IR_Type::Type metaType = type.getType();
  return metaType == IR_Type::DOUBLE ||
         metaType == IR_Type::LONG_DOUBLE ||
         metaType == IR_Type::LONG_LONG ||
         metaType == IR_Type::UNSIGNED_LONG_LONG;
}

//! Aligns a given integer value at alignment width 'alignmentWidth'
static inline void align( int &toAlign, int alignmentWidth )
{
  int remainder = toAlign % alignmentWidth;

  if ( remainder != 0 ) {
    int missingAlignment = alignmentWidth - remainder;
    toAlign += missingAlignment;
  }
}

//
// Class section
//

struct storeInfo {
  list< IR_Symbol* >::const_iterator itBegin;
  list< IR_Symbol* >::const_iterator itEnd;
  bool isStruct;
  int currentOffset;
  IR_ComposedType* currentCompType;
};

Stack::Stack()
  : mOffset( 0 )
{
  mBiggestReturn = nullptr;
  mSymbolMap.clear();
  mStackFrameMap.clear();

  mComposedPushed.clear();
}


Stack::Stack( const Stack &st )
{
  mOffset = st.mOffset;
  mBiggestReturn = nullptr;
  mSymbolMap = st.mSymbolMap;
  mStackFrameMap = st.mStackFrameMap;

  mComposedPushed = st.mComposedPushed;
}


Stack::~Stack()
{
}


/*
  reset deletes the private member objects to allow repetitive code selections.
*/
void Stack::reset( void )
{
  mSymbolMap.clear();
  mCallResultBufferMap.clear();
  mComposedParameterBufferMap.clear();
  mStackFrameMap.clear();
  mComposedPushed.clear();
  mComposedPushCostAdded.clear();
  mOffset = 0;
  mBiggestReturn = nullptr;
};


void Stack::pushFunction( IR_Function *irfunc )
{
  /* The stack design is:

    (high address)
    + own overflow arguments                | Caller stack frame
    -----------------------------------------
    + address-taken buffers for arguments   |
    + struct return value buffer            |
    + struct parameter buffers              | Own stack frame
    + own local variables                   |
    + overflow area for own function calls  |
    (low address)

    As the stack is growing towards the lower addresses, we must compute the
    offsets in the reversed order (lower offsets access lowest part of the
    function's own stack)
  */

  mOffset = getMaxArgOverflowSize( irfunc );
  mapFunctionSymbols( irfunc );

  // We have to consider the amount of stack used to save the relevant regs
  // from the callee.
  mOffset += getCalleeSavedRegs().size() * 4;

  // Align caller stack frame size to the next 8 bytes position.
  align( mOffset, 8 );
  mapFunctionArguments( irfunc );

  mStackFrameMap.insert( pair<IR_Function *, int>( irfunc, mOffset ) );
}


map<IR_Symbol *, SymbolInfo> *Stack::getSymbolMap()
{
  return &mSymbolMap;
}


int Stack::getOffset() const
{
  return mOffset;
}


SymbolInfo *Stack::getSymbolInfo( const IR_Symbol *sym )
{
  map<IR_Symbol *, SymbolInfo>::iterator it (mSymbolMap.find(
    const_cast<IR_Symbol*>( sym ) ) );

  if ( it != mSymbolMap.end() )
    return &( (*it).second );

  return 0;
}


SymbolInfo::Type Stack::getSymbolType( const IR_Symbol *sym )
{
  map<IR_Symbol *, SymbolInfo>::const_iterator it( mSymbolMap.find(
    const_cast<IR_Symbol*>( sym ) ) );

  return (*it).second.getSymbolType();
}


int Stack::getSymbolOffset( const IR_Symbol *sym )
{
  map<IR_Symbol *, SymbolInfo>::const_iterator it( mSymbolMap.find(
    const_cast<IR_Symbol*>( sym ) ) );

  if ( it == mSymbolMap.end() )
    return 0;
  else {
    if ( ( (*it).second.getSymbolType() == SymbolInfo::D_ARGUMENT ) ||
        ( (*it).second.getSymbolType() == SymbolInfo::A_ARGUMENT ) ||
        ( (*it).second.getSymbolType() == SymbolInfo::LOCAL_COMPOSED_ARGUMENT ) ||
        ( (*it).second.getSymbolType() == SymbolInfo::LOCAL_STACK_VAR ) )
      return (*it).second.getSymbolOffset();
    else
      return -1;
  }
}


int Stack::getCallResultBufferOffset( const IR_Function *func )
{
  map<IR_Function *, IR_Symbol *>::const_iterator it(
    mCallResultBufferMap.find( const_cast<IR_Function*>( func ) ) );

  if ( it == mCallResultBufferMap.end() )
    return 0;
  else {
    // Return offset of the artificial buffer symbol
    return getSymbolOffset( it->second );
  }
}


int Stack::getComposedParameterBufferOffset( const IR_Symbol *composedTypeParam )
{
  // Argument was not a function parameter!
  ufAssert( composedTypeParam->getSymbolTable().getFunction() );
  // Argument was not of composed type!
  ufAssert( dynamic_cast<IR_ComposedType*>( &composedTypeParam->getType() ) );

  map<IR_Symbol*, IR_Symbol *>::const_iterator it(
    mComposedParameterBufferMap.find(
      const_cast<IR_Symbol*>( composedTypeParam ) ) );

  if ( it == mComposedParameterBufferMap.end() )
    return 0;
  else {
    // Return offset of the artificial buffer symbol
    return getSymbolOffset( it->second );
  }
}


int Stack::getArgumentPos( const IR_Symbol *sym )
{
  map<IR_Symbol *, SymbolInfo>::const_iterator it ( mSymbolMap.find(
    const_cast<IR_Symbol*>( sym ) ) );

  if ( it != mSymbolMap.end() )
    return (*it).second.getSymbolArgumentPos();

  return -1;
}


string Stack::getSymbolReg( const IR_Symbol *sym )
{
  map<IR_Symbol *, SymbolInfo>::const_iterator it( mSymbolMap.find(
    const_cast<IR_Symbol*>( sym ) ) );

  if ( it != mSymbolMap.end() )
    return (*it).second.getSymbolReg();

  return "";
}


bool Stack::setSymbolReg( const IR_Symbol *sym, string reg )
{
  map<IR_Symbol *, SymbolInfo>::iterator it( mSymbolMap.find(
    const_cast<IR_Symbol*>( sym ) ) );

  if ( it != mSymbolMap.end() ) {
    (*it).second.setSymbolReg( reg );
    return true;
  } else
    return false;
}


string Stack::getInternalVReg( const IR_Symbol *sym )
{
  map<IR_Symbol *, SymbolInfo>::const_iterator it( mSymbolMap.find(
    const_cast<IR_Symbol*>( sym ) ) );

  if ( it != mSymbolMap.end() )
    return (*it).second.getInternalVReg();

  return "";
}


bool Stack::setInternalVReg( const IR_Symbol *sym, string reg )
{
  map<IR_Symbol *, SymbolInfo>::iterator it( mSymbolMap.find(
    const_cast<IR_Symbol*>( sym ) ) );

  if ( it != mSymbolMap.end() ) {
    (*it).second.setInternalVReg( reg );
    return true;
  } else
    return false;
}


string Stack::getAddrReg( const IR_Symbol *sym )
{
  map<IR_Symbol *, SymbolInfo>::const_iterator it( mSymbolMap.find(
    const_cast<IR_Symbol*>( sym ) ) );

  if ( it != mSymbolMap.end() )
    return (*it).second.getAddrReg();

  return "";
}


bool Stack::setAddrReg( const IR_Symbol *sym, string reg )
{
  map<IR_Symbol *, SymbolInfo>::iterator it( mSymbolMap.find(
    const_cast<IR_Symbol*>( sym ) ) );

  if ( it != mSymbolMap.end() ) {
    (*it).second.setAddrReg( reg );
    return true;
  } else
    return false;
}


bool Stack::getComposedPushed( const IR_Symbol *symb ) const
{
  return mComposedPushed.find( symb ) != mComposedPushed.end();
}


void Stack::setComposedPushed( const IR_Symbol *symb )
{
  if ( symb ) {
    mComposedPushed.insert( symb );
  }
}


bool Stack::getComposedPushCostAdded( const IR_Symbol *symb ) const
{
  return mComposedPushCostAdded.find( symb ) != mComposedPushCostAdded.end();
}


void Stack::setComposedPushCostAdded( const IR_Symbol *symb )
{
  if ( symb ) {
    mComposedPushCostAdded.insert( symb );
  }
}


/*
  setAddressTaken sets whether the address of the current symbol was taken.
  This information is valid only for symbols being function arguments.
*/
void Stack::setAddressTaken( const IR_Symbol *s, bool addrTaken )
{
  map<IR_Symbol *, SymbolInfo>::iterator it( mSymbolMap.find(
    const_cast<IR_Symbol*>( s ) ) );


  if ( it != mSymbolMap.end() )
    (*it).second.setAddressTaken( addrTaken );
}


/*
  getAddressTaken returns whether the address of the current symbol was taken.
  This information is valid only for symbols being function arguments.
*/
bool Stack::getAddressTaken( const IR_Symbol *s ) const
{
  map<IR_Symbol *, SymbolInfo>::const_iterator it( mSymbolMap.find(
    const_cast<IR_Symbol*>( s ) ) );


  if( it != mSymbolMap.end() )
    return (*it).second.getAddressTaken();

  return false;
}


/*
  setStoreInstructionsGenerated sets whether required ST instructions are
  already generated during code selection at the very beginning of a LLIR
  function for function arguments passed via registers, whose address is taken.
*/
void Stack::setStoreInstructionsGenerated( const IR_Symbol *s, bool generated )
{
  map<IR_Symbol *, SymbolInfo>::iterator it( mSymbolMap.find(
    const_cast<IR_Symbol*>( s ) ) );


  if ( it != mSymbolMap.end() )
    (*it).second.setStoreInstructionsGenerated( generated );
}


/*
  getStoreInstructionsGenerated returns whether required ST instructions are
  already generated during code selection at the very beginning of a LLIR
  function for function arguments passed via registers, whose address is taken.
*/
bool Stack::getStoreInstructionsGenerated( const IR_Symbol *s ) const
{
  map<IR_Symbol *, SymbolInfo>::const_iterator it( mSymbolMap.find(
    const_cast<IR_Symbol*>( s ) ) );


  if ( it != mSymbolMap.end() )
    return (*it).second.getStoreInstructionsGenerated();

  return false;
}


int Stack::getStructBytes( const IR_ComposedType *composed )
{
  assert ( composed && "Wrong parameter passed to function getStructBytes" );

  const int bitSize = composed->bitSize();
  int structSize = ( bitSize / 8 ) + ( bitSize % 8 > 0 ? 1 : 0 );
  // Adjust alignment to 4-bytes boundaries
  align( structSize, 4 );

  return structSize;
};


int Stack::getStackSize( const IR_Type *theType )
{
  int result = 0;

  switch ( theType->getType() ) {
    case IR_Type::VOID:
      result = 0;
      break;
    case IR_Type::BOOL:
      result =
        ( charBytes < ARMIR_CONFIGURATION->alignmentBool ) ?
          ARMIR_CONFIGURATION->alignmentBool :
          charBytes;
      break;
    case IR_Type::CHAR:
      result =
        ( charBytes < ARMIR_CONFIGURATION->alignmentChar ) ?
          ARMIR_CONFIGURATION->alignmentChar :
          charBytes;
      break;

    case IR_Type::UNSIGNED_CHAR:
      result =
        ( charBytes < ARMIR_CONFIGURATION->alignmentChar ) ?
          ARMIR_CONFIGURATION->alignmentChar :
          charBytes;
      break;

    case IR_Type::INT:
      result =
        ( intBytes < ARMIR_CONFIGURATION->alignmentInt ) ?
          ARMIR_CONFIGURATION->alignmentInt :
          intBytes;
      break;

    case IR_Type::UNSIGNED_INT:
      result =
        ( intBytes < ARMIR_CONFIGURATION->alignmentInt ) ?
          ARMIR_CONFIGURATION->alignmentInt :
          intBytes;
      break;

    case IR_Type::LONG:
      result =
        ( longBytes < ARMIR_CONFIGURATION->alignmentLong ) ?
          ARMIR_CONFIGURATION->alignmentLong :
          longBytes;
      break;

    case IR_Type::UNSIGNED_LONG:
      result =
        ( longBytes < ARMIR_CONFIGURATION->alignmentLong ) ?
          ARMIR_CONFIGURATION->alignmentLong :
          longBytes;
      break;

    case IR_Type::SHORT:
      result =
        ( shortBytes < ARMIR_CONFIGURATION->alignmentShort ) ?
          ARMIR_CONFIGURATION->alignmentShort :
          shortBytes;
      break;

    case IR_Type::UNSIGNED_SHORT:
      result =
        ( shortBytes < ARMIR_CONFIGURATION->alignmentShort ) ?
          ARMIR_CONFIGURATION->alignmentShort :
          shortBytes;
      break;

    case IR_Type::LONG_LONG:
      result =
        ( longLongBytes < ARMIR_CONFIGURATION->alignmentLongLong ) ?
          ARMIR_CONFIGURATION->alignmentLongLong :
          longLongBytes;
      break;

    case IR_Type::UNSIGNED_LONG_LONG:
      result =
        ( longLongBytes < ARMIR_CONFIGURATION->alignmentLongLong ) ?
          ARMIR_CONFIGURATION->alignmentLongLong :
          longLongBytes;
      break;

    case IR_Type::LONG_DOUBLE:
      result =
        ( longLongBytes < ARMIR_CONFIGURATION->alignmentLongDouble ) ?
          ARMIR_CONFIGURATION->alignmentLongDouble :
          longLongBytes;
      break;

    case IR_Type::DOUBLE:
      result =
        ( doubleBytes < ARMIR_CONFIGURATION->alignmentDouble ) ?
          ARMIR_CONFIGURATION->alignmentDouble :
          doubleBytes;
      break;

    case IR_Type::FLOAT:
      result =
        ( floatBytes < ARMIR_CONFIGURATION->alignmentFloat ) ?
          ARMIR_CONFIGURATION->alignmentFloat :
          floatBytes;
      break;

    case IR_Type::POINTER:
    case IR_Type::ARRAY:
      result =
        ( pointerBytes < ARMIR_CONFIGURATION->alignmentPointer ) ?
          ARMIR_CONFIGURATION->alignmentPointer :
          pointerBytes;
      break;

    case IR_Type::STRUCT: {
      const IR_ComposedType *struc = dynamic_cast<const IR_ComposedType* >( theType );
      // Struct object is NULL
      ufAssert( struc );

      result = getStructBytes( struc );
      if ( result < ARMIR_CONFIGURATION->alignmentComposedType )
        result = ARMIR_CONFIGURATION->alignmentComposedType;
      break;
    }

    case IR_Type::UNION: {
      const IR_ComposedType *struc = dynamic_cast<const IR_ComposedType*>( theType );
      // Union object is NULL
      ufAssert( struc );

      result = getStructBytes( struc );
      if ( result < ARMIR_CONFIGURATION->alignmentComposedType )
        result = ARMIR_CONFIGURATION->alignmentComposedType;
      break;
    }

    default:
      throw ufFatalError( "Non supported type of function argument." );
  }

  return result;
}


int Stack::getStackFrameSize( const IR_Function *theIrFct )
{
  map<IR_Function *, int>::const_iterator stackFrameSizeIterator =
    mStackFrameMap.find( const_cast<IR_Function*>( theIrFct ) );


  if ( stackFrameSizeIterator != mStackFrameMap.end() )
    return (*stackFrameSizeIterator).second;
  else
    return -1;
}

int Stack::getMaxArgOverflowSize( IR_Function *theIrFct )
{
  int overflowsize = 0;

  map<IR_FunctionSymbol *, int>::const_iterator it = theIrFct->getCalledFunctions().begin() ;
  for ( ; it != theIrFct->getCalledFunctions().end(); ++it){
    int funcoverflow = getParameterStackFrameSize( it->first->getType() );
    if ( overflowsize < funcoverflow ){
       overflowsize = funcoverflow;
    }
  }

  set<IR_CallExp *>::const_iterator iit = theIrFct->getIndirectFunctionCalls().begin() ;
  for ( ; iit != theIrFct->getIndirectFunctionCalls().end(); ++iit){
    int funcoverflow = getParameterStackFrameSize( (*iit)->getFunctionType() );
    if ( overflowsize < funcoverflow ){
       overflowsize = funcoverflow;
    }
  }

  return overflowsize;
}


int Stack::getParameterStackFrameSize( const IR_FunctionType &funcType )
{
  // The beginning of the parameter stack area is the beginning of the callers
  // stack frame, so it is aligned at 8-byte borders. Therefore the first "align"
  // call will not influence the computed size.

  list<IR_Type *> paramTypes = funcType.getArgumentTypes();
  int stackSize = 0;
  int parameterIndex = 0;

  for ( list<IR_Type *>::const_iterator
        it  = paramTypes.begin();
        it != paramTypes.end(); ++it ) {

    const enum IR_Type::Type currentType = (*it)->getType();
    IR_PointerType * const pointerType = dynamic_cast<IR_PointerType*>( *it );

    if ( isPassedThroughRegister( funcType, parameterIndex ) == -1 ) {
      if ( pointerType ) {

        align( stackSize,
          ARMIR_CONFIGURATION->alignmentPointer );
        stackSize += getStackSize( *it );

      } else if ( currentType == IR_Type::STRUCT ||
                  currentType == IR_Type::UNION ) {

        IR_ComposedType *compType = dynamic_cast<IR_ComposedType*>( *it );
        // Invalid type info!
        ufAssert( compType );

        const int composedTypeSize = getStackSize( compType );

        // Structs bigger than 64 bit are passed by pointer, smaller ones are
        // passed in a data or extended register. In each case we may have to
        // pass the pointer/reg/ereg via the stack if the registers are already
        // occupied.
        if ( composedTypeSize <= 4 ) {

          align( stackSize,
            ARMIR_CONFIGURATION->alignmentComposedType );
          stackSize += intBytes;

        } else if ( composedTypeSize <= 8 ) {

          align( stackSize,
            ARMIR_CONFIGURATION->alignmentComposedType );
          stackSize += 2 * intBytes;

        } else {

          align( stackSize,
            ARMIR_CONFIGURATION->alignmentPointer );
          stackSize += pointerBytes;

        }

      } else {

        switch ( currentType ) {

          case IR_Type::CHAR:
          case IR_Type::UNSIGNED_CHAR:
          case IR_Type::BOOL:
            stackSize += intBytes;
            break;

          case IR_Type::SHORT:
          case IR_Type::UNSIGNED_SHORT:
            align( stackSize,
              ARMIR_CONFIGURATION->alignmentShort );
            stackSize += intBytes;
            break;

          case IR_Type::INT:
          case IR_Type::UNSIGNED_INT:
            align( stackSize,
              ARMIR_CONFIGURATION->alignmentInt );
            stackSize += getStackSize( *it );
            break;

          case IR_Type::LONG:
          case IR_Type::UNSIGNED_LONG:
            align( stackSize,
              ARMIR_CONFIGURATION->alignmentLong );
            stackSize += getStackSize( *it );
            break;

          case IR_Type::LONG_LONG:
          case IR_Type::UNSIGNED_LONG_LONG:
            align( stackSize,
              ARMIR_CONFIGURATION->alignmentLongLong );
            stackSize += getStackSize( *it );
            break;

          case IR_Type::LONG_DOUBLE:
            align( stackSize,
              ARMIR_CONFIGURATION->alignmentLongDouble );
            stackSize += getStackSize( *it );
            break;

          case IR_Type::DOUBLE:
            align( stackSize,
              ARMIR_CONFIGURATION->alignmentDouble );
            stackSize += getStackSize( *it );
            break;

          case IR_Type::FLOAT:
            align( stackSize,
              ARMIR_CONFIGURATION->alignmentFloat );
            stackSize += getStackSize( *it );
            break;

          default:
            throw ufFatalError( "Non supported type of element for calculation "
                                "of stack size." );

        }

      }

    }

    parameterIndex++;
  }

  return stackSize;
}

list<LLIR_Instruction*> Stack::InitStack( LLIR_Function *theFunc,
                                          LLIR_BB *theBB)
{
    list<LLIR_Instruction*> generated;
    LLIR_Instruction *stmfd = 0;
    LLIR_Instruction *mov = 0;
    LLIR_Instruction *sub1 = 0;

    deque<LLIR_Register *> regs;

    // Collect the to be saved registers.
    for ( string sreg : getCalleeSavedRegs() ) {
      LLIR_Register *r = LLIR_Register::Create( theFunc, sreg.c_str(), 0 );
      regs.push_back( r );
    }

    LLIR_Register *fp = LLIR_Register::Create( theFunc, PHREG_FP, 0 );
    LLIR_Register *sp = LLIR_Register::Create( theFunc, PHREG_SP, 0 );
    LLIR_Register *ip = LLIR_Register::Create( theFunc, PHREG_IP, 0 );

    sub1 = insSUB(OPER_AL, "", fp, ip, 4, theBB, sub1);
    generated.push_front( sub1 );

    stmfd = insSTM(OPER_FD, OPER_AL, sp, OPER_WRITEBACK, &regs, theBB, stmfd);
    generated.push_front( stmfd );

     mov = insMOV( OPER_AL, "",  ip, sp, theBB, mov );
     generated.push_front( mov );

    return generated;

}


list<LLIR_Instruction *> Stack::adjustStackFrame( const WIR::ARM_Base &p,
                                                  WIR_BasicBlock &b,
                                                  LLIR_Function *theFunc,
                                                  LLIR_BB *theBB,
                                                  int offset )
{
  DSTART(
    "static list<LLIR_Instruction*> Stack::adjustStackFrame(const ARM_Base&, WIR_BasicBlock&, LLIR_Function*, LLIR_BB*, int)" );

  // The stack poiner MUST stay aligned at 8-byte borders all the time.
  ufAssertT( offset % 8 == 0, "Illegal stack pointer modification!" );
  // We assume, that the stack frame shoudl be *always* >= 0.
  ufAssertT( offset >= 0, "The offset should always >= 0!" );

  int bitMaskPattern = 0xFF;

  list<LLIR_Instruction *> generated;
  LLIR_Instruction *pred = nullptr;
  LLIR_Register *sp = LLIR_Register::Create( theFunc, PHREG_SP, 0 );

  // We check in 8 bit chunks and create corresponding SUB instructions.
  for ( unsigned int windowPos = 0; windowPos < 4; ++windowPos ) {
    // Only generate an instruction in case the current window contains
    // data.
    int curData = offset & ( bitMaskPattern << ( windowPos * 8 ) );
    if ( curData != 0 ) {
      pred = insSUB( OPER_AL, "", sp, sp, curData, theBB, pred );
      generated.push_front( pred );

      b.pushBackInstruction(
        { { ARM_Base::OpCode::SUB, ARM_Base::OperationFormat::CRRC8RA_1,
            WIR_ConditionFieldParameter( ARM_Base::Condition::al ),
            WIR_RegisterParameter( p.SP(), WIR_Usage::def ),
            WIR_RegisterParameter( p.SP(), WIR_Usage::use ),
            ARM_Const8_Unsigned( curData >> ( windowPos * 8 ) ),
            ARM_Const5_RotateAmount( ( ( 4 - windowPos ) % 4 ) * 8 ) } } );
    }
  }

  return( generated );
};


void Stack::mapFunctionArguments( IR_Function *theIrFct )
{
  const int oldOffset = mOffset;

  const list<IR_Symbol *> &argList = theIrFct->functionArguments.getSymbols();
  for ( list<IR_Symbol *>::const_iterator it  = argList.begin();
        it != argList.end(); ++it ) {

    const enum IR_Type::Type currentType = ( *it )->getType().getType();

    SymbolInfo SymInfo( SymbolInfo::D_ARGUMENT, -1, -1,
                        "", "", "", false, false );
    // The address-taken information was already computed during
    // "mapFunctionSymbols" because the buffer spaces for those
    // symbols must be in the local variable stack area.
    SymbolInfo * const existingSymInf = getSymbolInfo( *it );
    if ( existingSymInf ) {
      SymInfo = *existingSymInf;
    }

    // Still free register available?
    const int myRegister = isPassedThroughRegister( **it );

    if ( ( currentType == IR_Type::POINTER ) ||
         ( currentType == IR_Type::ARRAY ) ) {

      SymInfo.setSymbolType( SymbolInfo::A_ARGUMENT );

      if ( myRegister != -1 ) {
        SymInfo.setSymbolArgumentPos( myRegister + 1 );
      } else {
        align( mOffset,
          ARMIR_CONFIGURATION->alignmentPointer );

        SymInfo.setSymbolOffset( mOffset );
        mOffset += pointerBytes;
      }

    } else
    if ( ( currentType == IR_Type::STRUCT ) ||
         ( currentType == IR_Type::UNION ) ) {

      IR_ComposedType *compType = dynamic_cast<IR_ComposedType*>
        ( &(*it)->getType() );
      // Check for invalid type info
      ufAssert( compType );

      const int composedTypeSize = getStackSize( compType );

      // Structs bigger than 64 bit are passed by pointer, smaller ones are
      // passed in a data or extended register. In each case we may have to
      // pass the pointer/reg/ereg via the stack if the registers are already
      // occupied.
      if ( composedTypeSize <= 4 ) {
        // This struct will be passed in a reg if possible

        if ( myRegister != -1 ) {
          SymInfo.setSymbolType( SymbolInfo::D_ARGUMENT );
          SymInfo.setSymbolArgumentPos( myRegister + 1 );
        } else {
          SymInfo.setSymbolType( SymbolInfo::LOCAL_COMPOSED_ARGUMENT );

          // Update the stack information
          align( mOffset,
            ARMIR_CONFIGURATION->alignmentComposedType );
          SymInfo.setSymbolOffset( mOffset );
          mOffset += intBytes;
        }

      } else if ( composedTypeSize <= 8 ) {
        // This struct will be passed in an ereg if possible

        if ( myRegister != -1 ) {
          SymInfo.setSymbolType( SymbolInfo::D_ARGUMENT );
          SymInfo.setSymbolArgumentPos( myRegister + 1 );
        } else {
          SymInfo.setSymbolType( SymbolInfo::LOCAL_COMPOSED_ARGUMENT );

          // Update the stack information
          align( mOffset,
            ARMIR_CONFIGURATION->alignmentComposedType );
          SymInfo.setSymbolOffset( mOffset );
          mOffset += 2 * intBytes;
        }

      } else {
        // This struct will be passed in an areg if possible

        if ( myRegister != -1 ) {
          SymInfo.setSymbolType( SymbolInfo::A_ARGUMENT );
          SymInfo.setSymbolArgumentPos( myRegister + 1 );
        } else {
          SymInfo.setSymbolType( SymbolInfo::LOCAL_COMPOSED_ARGUMENT );

          // Update the stack information
          align( mOffset,
            ARMIR_CONFIGURATION->alignmentPointer );

          SymInfo.setSymbolOffset( mOffset );
          mOffset += pointerBytes;
        }
      }

    } else {
      SymInfo.setSymbolType( SymbolInfo::D_ARGUMENT );

      if ( myRegister != -1 ) {
        SymInfo.setSymbolArgumentPos( myRegister + 1 );
      } else {

        /* TODO: At the moment all integer types with bitwidth < 32 bit are
                 stored in a word, which wastes stack space. Those arguments
                 (char / short) should be stored a byte or halfword. The
                 argument handling and the symbol expression rules would have
                 to be adapted to this. Also, if the prototype of a function
                 is not known (only possible for calls to external functions),
                 then we must use the default argument promotions, which force
                 us to extend all values to 32-bit integers or doubles. */

        switch ( currentType ) {
          case IR_Type::CHAR:
          case IR_Type::UNSIGNED_CHAR:
          case IR_Type::BOOL: {
            // TODO: Why don't we align this one? (Short is int-aligned too)

            SymInfo.setSymbolOffset( mOffset );
            /* Treat as integer */
            mOffset += intBytes;
            break;
          }

          case IR_Type::SHORT:
          case IR_Type::UNSIGNED_SHORT: {
            /* Treat as integer */

            align( mOffset,
              ARMIR_CONFIGURATION->alignmentInt );

            SymInfo.setSymbolOffset( mOffset );
            mOffset += intBytes;
            break;
          }

          case IR_Type::INT:
          case IR_Type::UNSIGNED_INT: {

            align( mOffset,
              ARMIR_CONFIGURATION->alignmentInt );

            SymInfo.setSymbolOffset( mOffset );
            mOffset += intBytes;
            break;
          }

          case IR_Type::LONG:
          case IR_Type::UNSIGNED_LONG: {

            align( mOffset,
              ARMIR_CONFIGURATION->alignmentLong );

            SymInfo.setSymbolOffset( mOffset );
            mOffset += longBytes;
            break;
          }

          case IR_Type::LONG_LONG:
          case IR_Type::UNSIGNED_LONG_LONG: {

            align( mOffset,
              ARMIR_CONFIGURATION->alignmentLongLong );

            SymInfo.setSymbolOffset( mOffset );
            mOffset += longLongBytes;
            break;
          }

          case IR_Type::LONG_DOUBLE: {

            align( mOffset,
              ARMIR_CONFIGURATION->alignmentLongDouble );

            SymInfo.setSymbolOffset( mOffset );
            mOffset += longLongBytes;
            break;
          }

          case IR_Type::DOUBLE: {

            align( mOffset,
              ARMIR_CONFIGURATION->alignmentDouble );

            SymInfo.setSymbolOffset( mOffset );
            mOffset += doubleBytes;
            break;
          }

          case IR_Type::FLOAT: {

            align( mOffset,
              ARMIR_CONFIGURATION->alignmentFloat );

            SymInfo.setSymbolOffset( mOffset );
            mOffset += floatBytes;
            break;
          }

          default: {
            throw ufFatalError( "Unsupported stack argument detected." );
          }
        }
      }

    }

    mSymbolMap[ *it ] = SymInfo;
  }

  // Assert that the two methods for computing the parameter stack frames are
  // coherent.
  const int sizeDifference = ( mOffset - oldOffset ) -
    getParameterStackFrameSize( theIrFct->getSymbol().getType() );
  // Check for incoherent results for parameter stack frame size computation
  ufAssert( sizeDifference == 0 );
}


void Stack::mapFunctionSymbols( IR_Function *theIrFct )
{
  // 1.) Handle all the normal symbols that are present in the IR
  theIrFct->getTopCompound().localSymbolTable.iterateSymbolTables(
    symtabIterator, this );

  // 2.) Handle the buffer space that is needed for all composed type function
  // arguments of 'theIrFct'. Unfortunately we must insert those symbols here
  // and not during argument handling, because they must lie in the local symbol
  // area which is allocated right here.
  const list<IR_Symbol *> &symbolList = theIrFct->functionArguments.getSymbols();
  for ( list<IR_Symbol *>::const_iterator
        itArg  = symbolList.begin();
        itArg != symbolList.end(); ++itArg ) {

    if ( ( *itArg )->getType().getType() == IR_Type::STRUCT ||
         ( *itArg )->getType().getType() == IR_Type::UNION ) {

      IR_ComposedType *compType = dynamic_cast<IR_ComposedType*>
        ( &( *itArg )->getType() );
      // Check for invalid type info
      ufAssert( compType );

      // Size of the composed type
      const unsigned int composedTypeSize = Stack::getStackSize( compType );

      // For any composed type parameter which is bigger than 64 bit, we must
      // reserve stack space at the callee that can be used to copy the
      // parameter onto the callee's stack. This is needed because the EABI
      // dictates that such composed type parameters are passed as pointers and
      // get copied by the callee. For this purpose the callee needs a
      // dedicated stack area for each composed type parameter. This is why we
      // create an artificial symbol at this point that represents that stack
      // space. Composed type objects which are smaller than 64 bit are passed
      // by value on the stack, and thus we don't need to make another copy of
      // them.
      if ( composedTypeSize <= 8 && isPassedThroughRegister( **itArg ) == -1 ) {
        mComposedParameterBufferMap[ *itArg ] = *itArg;
      } else {
        IR_Symbol *paramBuffer = new IR_InternalSymbol( *compType );
        mComposedParameterBufferMap[ *itArg ] = paramBuffer;
        mapNewStackSymbol( *paramBuffer, this );
      }
    }
  }

  // 3.) Handle the buffer space that is needed for all functions which return
  // composed types. We must allocate a buffer space that is big enough to hold
  // the biggest returned composed object.
  mBiggestReturn = 0;
  theIrFct->getTopCompound().iterateStatements(
    statementIterator, this );

  if ( mBiggestReturn ) {
    // Create and map a new artificial symbol that represents the buffer space
    IR_InternalSymbol *bufferSym = new IR_InternalSymbol( *mBiggestReturn );

    mapNewStackSymbol( *bufferSym, this );
    mCallResultBufferMap[ theIrFct ] = bufferSym;
  }

  // 4.) Handle the buffer space that is needed to store the arguments that were
  // passed by register, but whose address was taken, and who thus require a
  // stack location. In the above cases we have used a new symbol to represent
  // the buffer space, but here we directly attach the buffer information to the
  // original symbol.
  for ( list<IR_Symbol*>::const_iterator itArg  = symbolList.begin();
        itArg != symbolList.end(); ++itArg ) {

    // Consider only symbols who are not passed on the stack anyways
    if ( isPassedThroughRegister( **itArg ) != -1 ) {

      const enum IR_Type::Type currentType = ( *itArg )->getType().getType();

      // We only fill the symbol info with the address-taken specific values
      // here, the rest is done in mapFunctionArguments
      SymbolInfo SymInfo( SymbolInfo::D_ARGUMENT, -1, -1,
                          "", "", "", false, false );

      if ( ( currentType == IR_Type::POINTER ) ||
           ( currentType == IR_Type::ARRAY ) ) {

        // Determine whether the address of the current symbol is taken using the
        // '&' operator. Pay attention that ICD-C's getAddrTaken() method is
        // only partially useful, because it it too general. In an expression
        // 'int *a = ...; &(a[12])' it would produce the result that the address
        // of 'a' was taken. For function arguments this is wrong, because the
        // address of the pointer/array was not taken, only the address of its
        // target object/array.
        if ( isAddressDirectlyTaken( **itArg ) ) {
          SymInfo.setAddressTaken( true );

          align( mOffset, ARMIR_CONFIGURATION->alignmentPointer );

          SymInfo.setSymbolOffset( mOffset );
          mOffset += pointerBytes;

        }

      } else
      if ( ( currentType == IR_Type::STRUCT ) ||
           ( currentType == IR_Type::UNION ) ) {

      // Composed type arguments are not handled here. If their address is
      // taken, this doesn't change much, because they are stored on the stack
      // anyways.

      } else {

        if ( !( *itArg )->getAddrTaken().empty() ) {
          SymInfo.setAddressTaken( true );

          switch ( currentType ) {

            case IR_Type::BOOL: {

              align( mOffset, ARMIR_CONFIGURATION->alignmentBool );
              SymInfo.setSymbolOffset( mOffset );
              mOffset += charBytes;
              break;
            }

            case IR_Type::CHAR:
            case IR_Type::UNSIGNED_CHAR: {

              align( mOffset, ARMIR_CONFIGURATION->alignmentChar );

              SymInfo.setSymbolOffset( mOffset );
              mOffset += charBytes;
              break;
            }

            case IR_Type::SHORT:
            case IR_Type::UNSIGNED_SHORT: {

              align( mOffset, ARMIR_CONFIGURATION->alignmentShort );

              SymInfo.setSymbolOffset( mOffset );
              mOffset += shortBytes;
              break;
            }

            case IR_Type::INT:
            case IR_Type::UNSIGNED_INT: {

              align( mOffset, ARMIR_CONFIGURATION->alignmentInt );

              SymInfo.setSymbolOffset( mOffset );
              mOffset += intBytes;
              break;
            }

            case IR_Type::LONG:
            case IR_Type::UNSIGNED_LONG: {

              align( mOffset, ARMIR_CONFIGURATION->alignmentLong );

              SymInfo.setSymbolOffset( mOffset );
              mOffset += longBytes;
              break;
            }

            case IR_Type::LONG_LONG:
            case IR_Type::UNSIGNED_LONG_LONG: {

              align( mOffset, ARMIR_CONFIGURATION->alignmentLongLong );

              SymInfo.setSymbolOffset( mOffset );
              mOffset += longLongBytes;
              break;
            }

            case IR_Type::LONG_DOUBLE: {

              align( mOffset, ARMIR_CONFIGURATION->alignmentLongDouble );

              SymInfo.setSymbolOffset( mOffset );
              mOffset += longLongBytes;
              break;
            }

            case IR_Type::DOUBLE: {

              align( mOffset, ARMIR_CONFIGURATION->alignmentDouble );

              SymInfo.setSymbolOffset( mOffset );
              mOffset += doubleBytes;
              break;
            }

            case IR_Type::FLOAT: {

              align( mOffset, ARMIR_CONFIGURATION->alignmentFloat );

              SymInfo.setSymbolOffset( mOffset );
              mOffset += floatBytes;
              break;
            }

            default: {
              throw ufFatalError( "Unsupported stack argument detected." );
            }
          }
        }
      }

      if ( SymInfo.getAddressTaken() ) {
        mSymbolMap[ *itArg ] = SymInfo;
      }
    }
  }
}


int Stack::neededRegs( IR_Type * type )
{
  int stackSize = getStackSize( type );
  int padding = ( 4 - ( stackSize % 4 ) ) % 4;
  int neededRegs = ( stackSize + padding ) / 4;
  return neededRegs;
}


void Stack::symtabIterator( IR_SymbolTable &symtab, void *p )
{
  list<IR_Symbol *> symbolList = ( &symtab )->getSymbols();
  Stack * const s = reinterpret_cast<Stack*>( p );

  for ( list<IR_Symbol *>::iterator
        irsym  = symbolList.begin();
        irsym != symbolList.end(); ++irsym ) {

    IR_SourceSymbol *sourceSym = dynamic_cast<IR_SourceSymbol *>( ( *irsym ) );
    IR_InternalSymbol *intSym = dynamic_cast<IR_InternalSymbol *>( ( *irsym ) );

    if ( ( sourceSym || intSym ) &&
         ( *irsym )->getType().getStorageClass() != IR_Type::STATIC &&
         ( *irsym )->isGlobal() == false ) {

      mapNewStackSymbol( **irsym, s );
    }
  }

  // Some padding required to get 4-bytes blocks
  align( s->mOffset, 4 );
}


void Stack::statementIterator( IR_Stmt &stmt, void *p )
{
  stmt.iterateExpressions( expressionIterator, p );
}


void Stack::expressionIterator( IR_Exp &exp, void *p )
{
  IR_CallExp * const callExp = dynamic_cast<IR_CallExp*>( &exp );

  if ( callExp ) {
    Stack * const s = reinterpret_cast<Stack*>( p );
    IR_ComposedType * const compType = dynamic_cast<IR_ComposedType*>(
                                         &callExp->getType() );

    // See if the type is bigger than the biggest that we've seen so far
    if ( compType &&
         ( !s->mBiggestReturn ||
           getStackSize( compType ) > getStackSize( s->mBiggestReturn )
         ) ) {

      s->mBiggestReturn = compType;
    }
  }
}

// Check if a Symbol Expression is used as asm operand with memory constraint
struct memRefByInlineAsm : public unary_function<IR_SymbolExp *, bool>
{
  bool operator()( IR_SymbolExp *exp )
  {
    IR_AsmStmt *asmStmt = dynamic_cast<IR_AsmStmt*>( &( exp->getStmt() ) );
    if ( asmStmt == 0 )
      return false;
    const IR_AsmOperand *operand =
        asmStmt->getOperand( asmStmt->getOperandExpIndex( exp ) );
    return operand->getArgumentType().find( 'm' ) !=
           operand->getArgumentType().npos;
  }
};

void Stack::mapNewStackSymbol( IR_Symbol &sym, Stack *s )
{
  // TODO: This method desperately needs some cleanup

  // Get some helper variables
  int stackSize;

  enum IR_Type::Type currentType = sym.getType().getType();
  IR_PointerType *thePointerType = dynamic_cast<IR_PointerType *>(
    &sym.getType() );
  IR_ArrayType *theArrayType = dynamic_cast<IR_ArrayType *>( &sym.getType() );
  IR_EnumType *enumType = dynamic_cast< IR_EnumType* >( &sym.getType() );

  bool isArrayType = ( thePointerType != 0 ) && ( theArrayType != 0 );
  bool isPointerType = ( thePointerType != 0 ) && ( theArrayType == 0 );

  // If the address of a local scalar variable is taken,
  // we have to store the symbol on the stack.
  if ( ( ( !sym.getAddrTaken().empty() ) &&

       // Those cases are handled later
       ( currentType != IR_Type::STRUCT ) &&
       ( currentType != IR_Type::UNION ) &&
       ( !isArrayType ) &&

       // A pointer's address is only treated as taken when it is taken directly.
       // If it is taken in an expression like "*(a + 12)", then it is not
       // treated as taken.
       ( !isPointerType || isAddressDirectlyTaken( sym ) ) )

      // if the address is used by inline assembly we have to store the symbol
      // on the stack as well
     || find_if( sym.getUsedIn().begin(), sym.getUsedIn().end(),
                memRefByInlineAsm() ) != sym.getUsedIn().end() ) {

    SymbolInfo SymInfo(
      SymbolInfo::LOCAL_STACK_VAR, -1, -1, "", "", "", false, false );

    stackSize = getStackSize( &sym.getType() );

    if ( enumType ) {
      // Enumeration section
      if ( sym.isTag() == false ) {

        align( s->mOffset, ARMIR_CONFIGURATION->alignmentInt );

        SymInfo.setSymbolOffset( s->mOffset );
        s->mOffset += stackSize;

        map< IR_Symbol *, IR_Integer >::const_iterator enumIt =
          enumType->getSymbols().begin();

        // Go through all enumeration values, create a symbolinfo object that
        // indicates the element if of type LOCAL_STACK_VAR. Offset is same
        // as composed type symbol. SymboIinfo type is required for
        // tc_rules.
        for( ; enumIt != enumType->getSymbols().end(); ++enumIt ) {
          SymbolInfo SymElementInfo(
            SymbolInfo::LOCAL_STACK_VAR, -1, -1, "", "", "", false, false );
          SymElementInfo.setSymbolOffset( SymInfo.getSymbolOffset() );

          s->mSymbolMap.insert( pair<IR_Symbol *, SymbolInfo>
            ( enumIt->first, SymElementInfo ) );
        }

      } // !isTag
    } // end enumType
    else if (sym.getType().isArithmeticType() || isPointerType ) {

      align( s->mOffset, sym.getType().alignment() );
      SymInfo.setSymbolOffset( s->mOffset );
      s->mOffset += stackSize;

    } else {

      ufAssertT( 0, "Address of an unsupported type taken." );

    }

    s->mSymbolMap.insert( pair<IR_Symbol *, SymbolInfo>( &sym, SymInfo ) );
  } else // address is not taken

  if ( isArrayType ) {

    // Local arrays are always stored on the stack.
    IR_ArrayType *arrayType = dynamic_cast<IR_ArrayType *>(
      &( sym.getType() ) );

    IR_Type &baseType = arrayType->getBaseType();
    IR_Type::Type theBaseType = baseType.getType();

    SymbolInfo SymInfo(
      SymbolInfo::LOCAL_STACK_VAR, -1, -1, "", "", "", false, false );


    if ( baseType.isArithmeticType() ||
        theBaseType == IR_Type::POINTER ||
        theBaseType == IR_Type::ARRAY ||
        theBaseType == IR_Type::STRUCT ||
        theBaseType == IR_Type::UNION ) {

      align( s->mOffset, baseType.alignment() );
      SymInfo.setSymbolOffset( s->mOffset );
      s->mOffset += sym.getType().sizeOf();

    } else {

      ufAssertT( 0, "Array of an unsupported type found." );

    }

    s->mSymbolMap.insert( pair<IR_Symbol *, SymbolInfo>( &sym, SymInfo ) );

  } else // not arrayType

  if ( currentType == IR_Type::UNION ) {
    // Declared, but not initialized structs are omitted.
    if ( sym.isTag() == false ) {

      stackSize = getStackSize( &sym.getType() );

      SymbolInfo SymInfo(
        SymbolInfo::LOCAL_STACK_VAR, -1, -1, "", "", "", false, false );

      // TODO: Use union-dependent alignment here. The alignment that is
      //       returned by the ICD-C is not usable here, because we have to
      //       adhere to specific ARM7-EABI (?) rules.
      align( s->mOffset,
        ARMIR_CONFIGURATION->alignmentComposedType );

      SymInfo.setSymbolOffset( s->mOffset );
      s->mOffset += stackSize;

      s->mSymbolMap.insert( pair<IR_Symbol *, SymbolInfo>( &sym, SymInfo ) );

      // Iterate through all elements and create symbolinfo of type
      // LOCAL_STACK_VAR. Required for proper stack handling.
      IR_ComposedType *compType = dynamic_cast<IR_ComposedType *>(
        &sym.getType() );
      ufAssert( compType );

      list<IR_Symbol *>::const_iterator
        symElementIt( compType->getComponents().getSymbols().begin() );

      list<IR_Symbol*>::const_iterator
        symElementItEnd( compType->getComponents().getSymbols().end() );

      deque< storeInfo > itStack;

      storeInfo structInfo = { symElementIt, symElementItEnd, false,
        SymInfo.getSymbolOffset(), compType };

      itStack.push_back( structInfo );

      int internalOffset = SymInfo.getSymbolOffset();

      do {
        while ( itStack.back().itBegin != itStack.back().itEnd ) {

          SymbolInfo SymElementInfo(
            SymbolInfo::LOCAL_STACK_VAR, -1, -1, "", "", "", false, false );

          SymElementInfo.setSymbolOffset( itStack.back().currentOffset );

          symElementIt = itStack.back().itBegin;

          IR_Type &elemType = (*symElementIt)->getType();
          enum IR_Type::Type theType = elemType.getType();

          stackSize = getStackSize( &(*symElementIt)->getType() );

          if ( elemType.isArithmeticType() || theType == IR_Type::POINTER ) {

            align( internalOffset, (*symElementIt)->getType().alignment());
            SymElementInfo.setSymbolOffset( internalOffset );
            internalOffset += stackSize;

          } else if ( theType == IR_Type::STRUCT ) {

            ( itStack.back().itBegin )++;

            IR_ComposedType *nestedType = dynamic_cast<IR_ComposedType*>(
              &(*symElementIt)->getType() );
            ufAssert( nestedType );

            list< IR_Symbol* >::const_iterator symNestedIt =
              nestedType->getComponents().getSymbols().begin();

            list< IR_Symbol* >::const_iterator symNestedItEnd =
              nestedType->getComponents().getSymbols().end();

            storeInfo structNested = { symNestedIt, symNestedItEnd, true,
              internalOffset, nestedType };

            itStack.push_back( structNested );

            continue;
          }
          else if ( theType == IR_Type::UNION ) {

            ( itStack.back().itBegin )++;

            IR_ComposedType *nestedType = dynamic_cast<IR_ComposedType*>
              ( &(*symElementIt)->getType() );
            ufAssert( nestedType );

            list< IR_Symbol* >::const_iterator symNestedIt =
              nestedType->getComponents().getSymbols().begin();

            list< IR_Symbol* >::const_iterator symNestedItEnd =
              nestedType->getComponents().getSymbols().end();

            storeInfo structNested = { symNestedIt, symNestedItEnd, false,
              internalOffset, nestedType };

            itStack.push_back( structNested );
          } else if ( theType == IR_Type::ARRAY ) {
            // Use IR_Type's function to determine size of array
            stackSize = (*symElementIt)->getType().sizeOf();
            if ( itStack.back().isStruct ) {

              align( internalOffset, (*symElementIt)->getType().alignment() );

              SymElementInfo.setSymbolOffset( internalOffset );
              internalOffset += stackSize;

            }
          } else {
            throw ufFatalError( "Non supported type of union element "
                                "detected." );
          }

          s->mSymbolMap.insert( pair<IR_Symbol *, SymbolInfo>
              ( *symElementIt, SymElementInfo ) );

          ( itStack.back().itBegin )++;
        }

        // When a struct has been completelt processed, remove iterator
        if ( itStack.back().itBegin == itStack.back().itEnd )
          itStack.pop_back();

      } while (!itStack.empty() );

    } // end !isTag

  } else                    // !UNION

  if ( currentType == IR_Type::STRUCT ) {
    // Declared, but not initialized structs are omitted.
    if ( sym.isTag() == false ) {
      // Local structs are stored on the stack. More accurately, the
      // component list is traversed and all elements pushed on the stack

      SymbolInfo SymInfo(
        SymbolInfo::LOCAL_STACK_VAR, -1, -1, "", "", "", false, false );

      // TODO: Use union-dependent alignment here. The alignment that is
      //       returned by the ICD-C is not usable here, because we have to
      //       adhere to specific ARM7-EABI (?) rules.
      align( s->mOffset,
        ARMIR_CONFIGURATION->alignmentComposedType );

      SymInfo.setSymbolOffset( s->mOffset );

      s->mSymbolMap.insert( pair<IR_Symbol *, SymbolInfo>( &sym, SymInfo ) );

      // Additionally, each element of the composed type is read and added
      // to the symbol map. The purpose is to indicate that these
      // symbols are of type "LOCAL_STACK_VAR".

      IR_ComposedType *compType = dynamic_cast<IR_ComposedType *>(
        &sym.getType() );
      ufAssert( compType );

      list<IR_Symbol *>::const_iterator
        symElementIt( compType->getComponents().getSymbols().begin() );

      list<IR_Symbol*>::const_iterator
        symElementItEnd( compType->getComponents().getSymbols().end() );

      // This queue (FIFO) stores iterators of current struct to enable
      // recursive traversal
      stack< pair< list< IR_Symbol* >::const_iterator,
                   list< IR_Symbol* >::const_iterator > > itStack;

      itStack.push( make_pair( symElementIt, symElementItEnd ) );

      // Iterate through all composed type elements and set symbol offset.
      do {
        // Iterate through all composed type elements
        while ( itStack.top().first != itStack.top().second ) {
          // Get last iterator
          symElementIt = itStack.top().first;

          // Determine type of element.
          IR_Type &currentType = (*symElementIt)->getType();
          enum IR_Type::Type theCurrentType = currentType.getType();

          SymbolInfo SymElementInfo(
            SymbolInfo::LOCAL_STACK_VAR, -1, -1, "", "", "", false, false );

          stackSize = getStackSize( &(*symElementIt)->getType() );

          if ( currentType.isArithmeticType() ||
              theCurrentType == IR_Type::POINTER ) {

            align( s->mOffset, currentType.alignment() );

            SymElementInfo.setSymbolOffset( s->mOffset );
            s->mOffset += stackSize;

          } else if ( theCurrentType == IR_Type::STRUCT ) {
            (itStack.top().first)++;

            IR_ComposedType *nestedType = dynamic_cast<IR_ComposedType *>(
              &(*symElementIt)->getType() );
            ufAssert( nestedType );

            list<IR_Symbol *>::const_iterator
              symNestedIt(
                nestedType->getComponents().getSymbols().begin() );

            list<IR_Symbol *>::const_iterator
              symNestedItEnd(
                nestedType->getComponents().getSymbols().end() );

            // In the next loop iteration, this struct is processsed.
            itStack.push( make_pair( symNestedIt, symNestedItEnd ) );

            align( s->mOffset,
              ARMIR_CONFIGURATION->alignmentComposedType );

            SymElementInfo.setSymbolOffset( s->mOffset );

            s->mSymbolMap.insert( pair<IR_Symbol *, SymbolInfo>(
              *symElementIt, SymElementInfo ) );
            continue;

          } else if ( theCurrentType == IR_Type::ARRAY ) {
            // Use IR_Type's function to determine size of array
            stackSize = (*symElementIt)->getType().sizeOf();

            align( s->mOffset, (*symElementIt)->getType().alignment() );

            SymElementInfo.setSymbolOffset( s->mOffset );
            s->mOffset += stackSize;
          } else if ( theCurrentType == IR_Type::UNION ) {
            // Nothing to do
            ;
          } else {
            throw ufFatalError( "Non supported type of struct element "
                                "detected." );
          }

          s->mSymbolMap.insert( pair<IR_Symbol *, SymbolInfo>(
            *symElementIt, SymElementInfo ) );

          ( itStack.top().first )++;
        } // end while

        // When a struct has been completely processed, remove iterator.
        if ( itStack.top().first == itStack.top().second )
          itStack.pop();

      } while ( !itStack.empty() ); // end do
    } // end isTag == false

  } else                    // !STRUCT

  if ( enumType ) {
    // Enumeration section
    if ( sym.isTag() == false ) {

      stackSize = getStackSize( &sym.getType() );

      SymbolInfo SymInfo(
        SymbolInfo::LOCAL_STACK_VAR, -1, -1, "", "", "", false, false );

      align( s->mOffset, ARMIR_CONFIGURATION->alignmentInt );

      SymInfo.setSymbolOffset( s->mOffset );
      s->mOffset += stackSize;

      s->mSymbolMap.insert( pair<IR_Symbol *, SymbolInfo>( &sym, SymInfo ) );

      map< IR_Symbol *, IR_Integer >::const_iterator enumIt =
        enumType->getSymbols().begin();

      // Go through all enumeration values, create a symbolinfo object that
      // indicates the element if of type LOCAL_STACK_VAR. Offset is same
      // as composed type symbol. SymbolInfo type is required for tc_rules.
      for( ; enumIt != enumType->getSymbols().end(); ++enumIt ) {
        SymbolInfo SymElementInfo(
          SymbolInfo::LOCAL_STACK_VAR, -1, -1, "", "", "", false, false );
        SymElementInfo.setSymbolOffset( SymInfo.getSymbolOffset() );

        s->mSymbolMap.insert( pair<IR_Symbol *, SymbolInfo>
           ( enumIt->first, SymElementInfo ) );
      }

    } // end !isTag

  } else {
    // The enumeration values reside in the same name space as their
    // corresponding type's tag symbol. Since they are handled the the enum
    // type is analyzed (see above), there no need to read them again.
    // Thus, they are omitted here.
    if ( sym.getEnumType() == 0 ) {
      // If the address of a local variable is not taken, we can store the
      // symbol as virtual register.
      SymbolInfo symInfo(
        SymbolInfo::LOCAL_VAR, -1, -1, "", "", "", false, false );

      s->mSymbolMap.insert(
          pair<IR_Symbol *, SymbolInfo>( &sym, symInfo ) );
    }
  }
}


int Stack::isPassedThroughRegister( const IR_Symbol &sym )
{
  // TODO: Use this method throughout the codeselector. Locations:
  // * all other function in this class
  // * argument passing rules
  // * symbolExp rules

  // If the symbol is not a function argument, then we return -1
  if ( sym.getSymbolTable().getFunction() ) {

    // Else get the function type and the parameter index of the symbol ...
    IR_FunctionType &funcType =
      sym.getSymbolTable().getFunction()->getSymbol().getType();
    unsigned int paramIndex = 0;
    const list<IR_Symbol*> &symbols = sym.getSymbolTable().getSymbols();
    for ( list<IR_Symbol*>::const_iterator itSym  = symbols.begin();
          itSym != symbols.end(); ++itSym ) {
      if ( *itSym == &sym ) {
        break;
      } else {
        paramIndex++;
      }
    }
    // Check for invalid parameter index
    ufAssert( paramIndex < symbols.size() );

    // ... and call the main "isPassedThroughRegister" function
    return isPassedThroughRegister( funcType, paramIndex );

  } else {

    return -1;

  }
}


int Stack::isPassedThroughRegister( const IR_FunctionType &funcType,
  unsigned int parameterIndex )
{
  // Helper variables for keeping track of the used up registers
  bool regMap[4] = { false, false, false, false };
  unsigned int thisPos = 0;
  unsigned int currentParamIndex = 0;

  // If the return value is a struct that is bigger than 32 bits,
  // then r0 is already occupied for the return value
  IR_Type &returnType = funcType.getReturnType();
  if ( ( ( returnType.getType() == IR_Type::STRUCT ) ||
         ( returnType.getType() == IR_Type::UNION ) ) &&
       getStackSize( &returnType ) > 4 ) {
    regMap[ 0 ] = true;
  }


  const list<IR_Type *> &paramTypes = funcType.getArgumentTypes();
  for ( list<IR_Type *>::const_iterator it  = paramTypes.begin();
        it != paramTypes.end(); ++it ) {

    // Whether the current symbol is passed by register
    bool passedThroughRegister = false;

    // A small lambda to check for free registers.
    auto checkForFreeReg = [&thisPos, &regMap, &passedThroughRegister] () {
      int firstFreeReg = 4;
      for ( int loop = 3; loop >= 0; loop-- )
        if ( !regMap[loop] )
          firstFreeReg = loop;

      passedThroughRegister = firstFreeReg < 4;
      if ( passedThroughRegister ) {
        thisPos = static_cast< unsigned int >( firstFreeReg );
        regMap[firstFreeReg] = true;
      }
    };

    const enum IR_Type::Type currentType = ( *it )->getType();

    // only stucts and unions have to be handled seperatly
    // variables, arrays and pointers will be passed through single registers
    if ( currentType == IR_Type::STRUCT || currentType == IR_Type::UNION ) {

      IR_ComposedType * const compType = dynamic_cast<IR_ComposedType*>( *it );
      // Check for invalid type info
      ufAssert( compType );

      const int composedTypeSize = getStackSize( compType );

      // Structs bigger than 64 bit are passed by pointer, smaller ones are
      // passed in a data or extended register. In each case we may have to
      // pass the pointer/reg/ereg via the stack if the registers are already
      // occupied.
      if ( composedTypeSize <= 4 ) {
        // This struct will be passed in a reg if possible
        checkForFreeReg();

      } else if ( composedTypeSize <= 8 ) {
        // This struct will be passed in an ereg if possible

        if ( !regMap[0] && !regMap[1] ) {
          passedThroughRegister = true;
          thisPos = 0;
          regMap[0] = true;
          regMap[1] = true;
        } else
        if ( !regMap[2] && !regMap[3] ) {
          passedThroughRegister = true;
          thisPos = 2;
          regMap[2] = true;
          regMap[3] = true;
        }

      } else {
        checkForFreeReg();
      }

    } else {

      /* the four registers holding the function-parametes have to be filled
       * completly. This leads to the need of rearanging the function-parameters
       * to take care of ereg's.
       * Example:
       *   void a ( float, double, int )
       *   float  -> d4
       *   double -> e6 ( d6 + d7 )
       *   int    -> d5
       */

      if ( typeIsStoredInEReg( **it ) ) {

        // Still a free extended register available?
        if ( !regMap[0] && !regMap[1] ) {
          passedThroughRegister = true;
          thisPos = 0;
          regMap[0] = true;
          regMap[1] = true;
        } else
        if ( !regMap[2] && !regMap[3] ) {
          passedThroughRegister = true;
          thisPos = 2;
          regMap[2] = true;
          regMap[3] = true;
        }

      } else {
        // Still free data register available?
        checkForFreeReg();
      }

    }

    // If the current symbol was the one that we searched for,
    // then we have our return value
    if ( currentParamIndex == parameterIndex ) {
      if ( passedThroughRegister ) {
        // Check for ilegal argument position
        ufAssert( thisPos <= 3 );
        return thisPos;
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


bool Stack::isAddressDirectlyTaken( const IR_Symbol &sym )
{
  // Look at all the locations where the address is reported to be taken
  for ( set<IR_SymbolExp *>::const_iterator
        itTakeExp  = sym.getAddrTaken().begin();
        itTakeExp != sym.getAddrTaken().end(); ++itTakeExp ) {

    // Find the '&' operator in the current expression tree.
    IR_Exp *e = *itTakeExp;
    IR_UnaryExp *unaryExp = dynamic_cast<IR_UnaryExp *>( e );

    while ( !unaryExp || ( unaryExp->getOperator() != IR_UnaryExp::ADDR ) ) {
      // The following assertion is required for the clang static analyzer.
      ufAssert( e != nullptr );

      e = e->getParent();
      unaryExp = dynamic_cast<IR_UnaryExp *>( e );

      if( !e )
        break;
    }

    if ( e ) {
      // See if the address operator is applied directly to the symbol itself
      IR_Exp *child = &( unaryExp->getOp() );
      IR_SymbolExp *symExp = dynamic_cast<IR_SymbolExp *>( child );

      if ( symExp && symExp->getSymbol() == sym ) {
        // Only then, the address is directly taken
        return true;
      }
    }
  }

  return false;
}

list< string > Stack::getCalleeSavedRegs()
{
  // We can define here, which registers should be saved by default on each
  // function entry.
  return { PHREG_FP, PHREG_IP, PHREG_LR, PHREG_PC };
}
