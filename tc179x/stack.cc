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

// Include boost headers
#include <boost/current_function.hpp>

// Include WIR headers
#include <wir/wir.h>
#include <arch/tricore/tc131.h>

// Include LIBUSEFUL headers
#include <libuseful/exceptions.h>
#include <libuseful/io.h>

// Include private headers
#include "instructionfactory.h"
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
  mSymbolMap.clear();
  mStackFrameMap.clear();

  mComposedPushed.clear();
}


Stack::Stack( const Stack &st )
{
  mOffset = st.mOffset;
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

  // Align caller stack frame size to the next 8 bytes position.
  align( mOffset, 8 );
  mapFunctionArguments( irfunc );

  mStackFrameMap[ irfunc ] = mOffset;
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
  ufAssertT( composedTypeParam->getSymbolTable().getFunction(),
    "Argument was not a function parameter!" );
  ufAssertT( dynamic_cast<IR_ComposedType*>( &composedTypeParam->getType() ),
    "Argument was not of composed type!" );

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
    return (*it).second.getSymReg();

  return "";
}


bool Stack::setSymbolReg( const IR_Symbol *sym, const std::string &reg )
{
  map<IR_Symbol *, SymbolInfo>::iterator it( mSymbolMap.find(
    const_cast<IR_Symbol*>( sym ) ) );

  if ( it != mSymbolMap.end() ) {
    (*it).second.setSymbolReg( reg );
    return true;
  } else
    return false;
}


/*
  setSymbolReg associates the given virtual WIR register with an IR symbol.
*/
void Stack::setSymbolReg( const IR_Symbol &sym,
                          const WIR::WIR_VirtualRegister &r )
{
  DSTART(
    "void Stack::setSymbolReg(const IR_Symbol&, const WIR_VirtualRegister&)" );

  auto it = mSymbolMap.find( const_cast<IR_Symbol *>( &sym ) );

  if ( it == mSymbolMap.end() )
    return;

  auto &symInfo = it->second;
  symInfo.setSymbolReg( r );
};


/*
  getSymbolReg returns the virtual WIR register associated with the given IR
  symbol.

  getSymbolReg fails with an assertion if no WIR register has previously been
  associated with a symbol.
*/
WIR::WIR_VirtualRegister &Stack::getSymbolReg( const IR_Symbol &sym ) const
{
  DSTART( "WIR_VirtualRegister& Stack::getSymbolReg(const IR_Symbol&) const" );

  auto &symInfo =
    mSymbolMap.find( const_cast<IR_Symbol *>( &sym ) )->second;

  return( symInfo.getSymbolReg() );
};


/*
  isSymbolRegSet returns whether a WIR register has previously been associated
  with an IR symbol.
*/
bool Stack::isSymbolRegSet( const IR_Symbol &sym ) const
{
  DSTART( "bool Stack::isSymbolRegSet(const IR_Symbol&) const" );

  auto it = mSymbolMap.find( const_cast<IR_Symbol *>( &sym ) );

  if ( it == mSymbolMap.end() )
    return( false );

  return( it->second.isSymbolRegSet() );
};


string Stack::getInternalVReg( const IR_Symbol *sym )
{
  map<IR_Symbol *, SymbolInfo>::const_iterator it( mSymbolMap.find(
    const_cast<IR_Symbol*>( sym ) ) );

  if ( it != mSymbolMap.end() )
    return (*it).second.getIntVReg();

  return "";
}


bool Stack::setInternalVReg( const IR_Symbol *sym, const std::string &reg )
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
    return (*it).second.getAdrReg();

  return "";
}


bool Stack::setAddrReg( const IR_Symbol *sym, const std::string &reg )
{
  map<IR_Symbol *, SymbolInfo>::iterator it( mSymbolMap.find(
    const_cast<IR_Symbol*>( sym ) ) );

  if ( it != mSymbolMap.end() ) {
    (*it).second.setAddrReg( reg );
    return true;
  } else
    return false;
}


/*
  setAddressReg associates the given virtual TriCore address register with an IR
  symbol.
*/
void Stack::setAddressReg( const IR_Symbol &sym, const WIR::TC_ARegV &r )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto it = mSymbolMap.find( const_cast<IR_Symbol *>( &sym ) );

  if ( it == mSymbolMap.end() )
    return;

  auto &symInfo = it->second;
  symInfo.setAddrReg( r );
};


/*
  getAddressReg returns the virtual TriCore address register associated with the
  given IR symbol.

  getAddressReg fails with an assertion if no %WIR register has previously been
  associated with a symbol.
*/
WIR::TC_ARegV &Stack::getAddressReg( const IR_Symbol &sym ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &symInfo =
    mSymbolMap.find( const_cast<IR_Symbol *>( &sym ) )->second;

  return( symInfo.getAddrReg() );
};


/*
  isAddressRegSet returns whether a TriCore address register has previously been
  associated with an IR symbol.
*/
bool Stack::isAddressRegSet( const IR_Symbol &sym ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  auto &symInfo =
    mSymbolMap.find( const_cast<IR_Symbol *>( &sym ) )->second;

  return( symInfo.isAddrRegSet() );
};


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
        ( boolBytes < TCIR_CONFIGURATION->alignmentBool ) ?
          TCIR_CONFIGURATION->alignmentBool :
          boolBytes;
      break;
    case IR_Type::CHAR:
      result =
        ( charBytes < TCIR_CONFIGURATION->alignmentChar ) ?
          TCIR_CONFIGURATION->alignmentChar :
          charBytes;
      break;

    case IR_Type::UNSIGNED_CHAR:
      result =
        ( charBytes < TCIR_CONFIGURATION->alignmentChar ) ?
          TCIR_CONFIGURATION->alignmentChar :
          charBytes;
      break;

    case IR_Type::INT:
      result =
        ( intBytes < TCIR_CONFIGURATION->alignmentInt ) ?
          TCIR_CONFIGURATION->alignmentInt :
          intBytes;
      break;

    case IR_Type::UNSIGNED_INT:
      result =
        ( intBytes < TCIR_CONFIGURATION->alignmentInt ) ?
          TCIR_CONFIGURATION->alignmentInt :
          intBytes;
      break;

    case IR_Type::LONG:
      result =
        ( longBytes < TCIR_CONFIGURATION->alignmentLong ) ?
          TCIR_CONFIGURATION->alignmentLong :
          longBytes;
      break;

    case IR_Type::UNSIGNED_LONG:
      result =
        ( longBytes < TCIR_CONFIGURATION->alignmentLong ) ?
          TCIR_CONFIGURATION->alignmentLong :
          longBytes;
      break;

    case IR_Type::SHORT:
      result =
        ( shortBytes < TCIR_CONFIGURATION->alignmentShort ) ?
          TCIR_CONFIGURATION->alignmentShort :
          shortBytes;
      break;

    case IR_Type::UNSIGNED_SHORT:
      result =
        ( shortBytes < TCIR_CONFIGURATION->alignmentShort ) ?
          TCIR_CONFIGURATION->alignmentShort :
          shortBytes;
      break;

    case IR_Type::LONG_LONG:
      result =
        ( longLongBytes < TCIR_CONFIGURATION->alignmentLongLong ) ?
          TCIR_CONFIGURATION->alignmentLongLong :
          longLongBytes;
      break;

    case IR_Type::UNSIGNED_LONG_LONG:
      result =
        ( longLongBytes < TCIR_CONFIGURATION->alignmentLongLong ) ?
          TCIR_CONFIGURATION->alignmentLongLong :
          longLongBytes;
      break;

    case IR_Type::LONG_DOUBLE:
      result =
        ( longDoubleBytes < TCIR_CONFIGURATION->alignmentLongDouble ) ?
          TCIR_CONFIGURATION->alignmentLongDouble :
          longDoubleBytes;
      break;

    case IR_Type::DOUBLE:
      result =
        ( doubleBytes < TCIR_CONFIGURATION->alignmentDouble ) ?
          TCIR_CONFIGURATION->alignmentDouble :
          doubleBytes;
      break;

    case IR_Type::FLOAT:
      result =
        ( floatBytes < TCIR_CONFIGURATION->alignmentFloat ) ?
          TCIR_CONFIGURATION->alignmentFloat :
          floatBytes;
      break;

    case IR_Type::POINTER:
    case IR_Type::ARRAY:
      result =
        ( pointerBytes < TCIR_CONFIGURATION->alignmentPointer ) ?
          TCIR_CONFIGURATION->alignmentPointer :
          pointerBytes;
      break;

    case IR_Type::STRUCT: {
      const IR_ComposedType *struc = dynamic_cast<const IR_ComposedType* >( theType );
      ufAssertT( struc, "Struct object is NULL" );

      result = getStructBytes( struc );
      if ( result < TCIR_CONFIGURATION->alignmentComposedType )
        result = TCIR_CONFIGURATION->alignmentComposedType;
      break;
    }

    case IR_Type::UNION: {
      const IR_ComposedType *struc = dynamic_cast<const IR_ComposedType*>( theType );
      ufAssertT( struc, "Union object is NULL" );

      result = getStructBytes( struc );
      if ( result < TCIR_CONFIGURATION->alignmentComposedType )
        result = TCIR_CONFIGURATION->alignmentComposedType;
      break;
    }

    default: {
      throw ufFatalError( "Unsupported type of function argument." );
      break;
    }
  }

  return result;
}


int Stack::getStackFrameSize( const IR_Function *theIrFct )
{
  auto it = mStackFrameMap.find( const_cast<IR_Function *>( theIrFct ) );

  if ( it != mStackFrameMap.end() )
    return( it->second );
  else
    return( -1 );
};


int Stack::getMaxArgOverflowSize( IR_Function *theIrFct )
{
  int overflowsize = 0;

  map<IR_FunctionSymbol *, int>::const_iterator it = theIrFct->getCalledFunctions().begin() ;
  for ( ; it != theIrFct->getCalledFunctions().end(); it++){
    int funcoverflow = getParameterStackFrameSize( it->first->getType() );
    if ( overflowsize < funcoverflow ){
       overflowsize = funcoverflow;
    }
  }

  set<IR_CallExp *>::const_iterator iit = theIrFct->getIndirectFunctionCalls().begin() ;
  for ( ; iit != theIrFct->getIndirectFunctionCalls().end(); iit++){
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
        it != paramTypes.end(); it++ ) {

    const enum IR_Type::Type currentType = (*it)->getType();
    IR_PointerType * const pointerType = dynamic_cast<IR_PointerType*>( *it );

    if ( !isPassedThroughRegister( funcType, parameterIndex ) ) {

      if ( pointerType ) {

        align( stackSize,
          TCIR_CONFIGURATION->alignmentPointer );
        stackSize += getStackSize( *it );

      } else if ( currentType == IR_Type::STRUCT ||
                  currentType == IR_Type::UNION ) {

        IR_ComposedType *compType = dynamic_cast<IR_ComposedType*>( *it );
        ufAssertT( compType, "Invalid type info!" );

        const int composedTypeSize = getStackSize( compType );

        // Structs bigger than 64 bit are passed by pointer, smaller ones are
        // passed in a data or extended register. In each case we may have to
        // pass the pointer/reg/ereg via the stack if the registers are already
        // occupied.
        if ( composedTypeSize <= 4 ) {

          align( stackSize,
            TCIR_CONFIGURATION->alignmentComposedType );
          stackSize += intBytes;

        } else if ( composedTypeSize <= 8 ) {

          align( stackSize,
            TCIR_CONFIGURATION->alignmentComposedType );
          stackSize += 2 * intBytes;

        } else {

          align( stackSize,
            TCIR_CONFIGURATION->alignmentPointer );
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
              TCIR_CONFIGURATION->alignmentShort );
            stackSize += intBytes;
            break;

          case IR_Type::INT:
          case IR_Type::UNSIGNED_INT:
            align( stackSize,
              TCIR_CONFIGURATION->alignmentInt );
            stackSize += getStackSize( *it );
            break;

          case IR_Type::LONG:
          case IR_Type::UNSIGNED_LONG:
            align( stackSize,
              TCIR_CONFIGURATION->alignmentLong );
            stackSize += getStackSize( *it );
            break;

          case IR_Type::LONG_LONG:
          case IR_Type::UNSIGNED_LONG_LONG:
            align( stackSize,
              TCIR_CONFIGURATION->alignmentLongLong );
            stackSize += getStackSize( *it );
            break;

          case IR_Type::LONG_DOUBLE:
            align( stackSize,
              TCIR_CONFIGURATION->alignmentLongDouble );
            stackSize += getStackSize( *it );
            break;

          case IR_Type::DOUBLE:
            align( stackSize,
              TCIR_CONFIGURATION->alignmentDouble );
            stackSize += getStackSize( *it );
            break;

          case IR_Type::FLOAT:
            align( stackSize,
              TCIR_CONFIGURATION->alignmentFloat );
            stackSize += getStackSize( *it );
            break;

          default: {
            throw
              ufFatalError(
                "Unsupported type of element for calculation of stack size." );
            break;
          }
        }

      }

    }

    parameterIndex++;
  }

  return stackSize;
}


list<LLIR_Instruction *> Stack::adjustStackFrame( LLIR_Function *theFunc,
                                                  LLIR_BB *theBB, int offset )
{
  DSTART(
    "static list<LLIR_Instruction*> Stack::adjustStackFrame(LLIR_Function*, LLIR_BB*, int)" );

  // The stack poiner MUST stay aligned at 8-byte borders all the time.
  ufAssertT( offset % 8 == 0, "Illegal stack pointer modification!" );

  list<LLIR_Instruction *> generated;
  LLIR_Instruction *pred = nullptr;
  LLIR_Register *sp = LLIR_Register::Create( theFunc, PHREG_SP, 0 );

  if ( ( -offset >= minSignedConst16Value ) &&
       ( -offset <= maxSignedConst16Value ) ) {
    pred = insLEA( sp, OPER_BASE, sp, -offset, theBB, pred );
    generated.push_back( pred );
  } else {
    auto p = InstructionFactory::splitOffset( -offset );

    pred = insLEA( sp, OPER_BASE, sp, p.second, theBB, pred );
    generated.push_back( pred );
    pred = insADDIH_A( sp, sp, p.first, theBB, pred );
    generated.push_back( pred );
  }

  return( generated );
};


void Stack::adjustStackFrame( const WIR::TC13 &proc, WIR::WIR_BasicBlock &b,
                              int offset )
{
  DSTART(
    "static void Stack::adjustStackFrame(const TC13&, WIR_BasicBlock&, int)" );

  if ( offset == 0 )
    return;

  // The stack poiner MUST stay aligned at 8-byte borders all the time.
  ufAssertT( offset % 8 == 0, "Illegal stack pointer modification!" );

  if ( ( -offset >= TC_Const16_Signed::getMinValue( 16 ) ) &&
       ( -offset <= TC_Const16_Signed::getMaxValue( 16 ) ) )
    b.pushFrontInstruction(
      { { TC131::OpCode::LEA, TC131::OperationFormat::AAC16BOA,
          new WIR_RegisterParameter( proc.SP(), WIR_Usage::def ),
          new WIR_RegisterParameter( proc.SP(), WIR_Usage::use ),
          new TC_Const16_Signed( -offset ) } } );
  else {
    auto p = InstructionFactory::splitOffset( -offset );

    // Generate the operation (see TriCore Architecture Manual, page 5-19).
    b.pushFrontInstruction(
      { { TC131::OpCode::ADDIH_A, TC131::OperationFormat::AAC16,
          new WIR_RegisterParameter( proc.SP(), WIR_Usage::def ),
          new WIR_RegisterParameter( proc.SP(), WIR_Usage::use ),
          new TC_Const16_Unsigned( p.first ) } } );
    b.pushFrontInstruction(
      { { TC131::OpCode::LEA, TC131::OperationFormat::AAC16BOA,
          new WIR_RegisterParameter( proc.SP(), WIR_Usage::def ),
          new WIR_RegisterParameter( proc.SP(), WIR_Usage::use ),
          new TC_Const16_Signed( p.second ) } } );
  }
};


void Stack::mapFunctionArguments( IR_Function *theIrFct )
{
  const int oldOffset = mOffset;

  const list<IR_Symbol *> &argList = theIrFct->functionArguments.getSymbols();
  for ( list<IR_Symbol *>::const_iterator
        it  = argList.begin();
        it != argList.end(); it++ ) {

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
    const unsigned int myRegister = isPassedThroughRegister( **it );

    if ( ( currentType == IR_Type::POINTER ) ||
         ( currentType == IR_Type::ARRAY ) ) {

      SymInfo.setSymbolType( SymbolInfo::A_ARGUMENT );

      if ( myRegister ) {
        SymInfo.setSymbolArgumentPos( myRegister - 3 );
      } else {
        align( mOffset,
          TCIR_CONFIGURATION->alignmentPointer );

        SymInfo.setSymbolOffset( mOffset );
        mOffset += pointerBytes;
      }

    } else
    if ( ( currentType == IR_Type::STRUCT ) ||
         ( currentType == IR_Type::UNION ) ) {

      IR_ComposedType *compType = dynamic_cast<IR_ComposedType*>
        ( &(*it)->getType() );
      ufAssertT( compType, "Invalid type info!" );

      const int composedTypeSize = getStackSize( compType );

      // Structs bigger than 64 bit are passed by pointer, smaller ones are
      // passed in a data or extended register. In each case we may have to
      // pass the pointer/reg/ereg via the stack if the registers are already
      // occupied.
      if ( composedTypeSize <= 4 ) {
        // This struct will be passed in a reg if possible

        if ( myRegister ) {
          SymInfo.setSymbolType( SymbolInfo::D_ARGUMENT );
          SymInfo.setSymbolArgumentPos( myRegister - 3 );
        } else {
          SymInfo.setSymbolType( SymbolInfo::LOCAL_COMPOSED_ARGUMENT );

          // Update the stack information
          align( mOffset,
            TCIR_CONFIGURATION->alignmentComposedType );
          SymInfo.setSymbolOffset( mOffset );
          mOffset += intBytes;
        }

      } else if ( composedTypeSize <= 8 ) {
        // This struct will be passed in an ereg if possible

        if ( myRegister ) {
          SymInfo.setSymbolType( SymbolInfo::D_ARGUMENT );
          SymInfo.setSymbolArgumentPos( myRegister - 3 );
        } else {
          SymInfo.setSymbolType( SymbolInfo::LOCAL_COMPOSED_ARGUMENT );

          // Update the stack information
          align( mOffset,
            TCIR_CONFIGURATION->alignmentComposedType );
          SymInfo.setSymbolOffset( mOffset );
          mOffset += 2 * intBytes;
        }

      } else {
        // This struct will be passed in an areg if possible

        if ( myRegister ) {
          SymInfo.setSymbolType( SymbolInfo::A_ARGUMENT );
          SymInfo.setSymbolArgumentPos( myRegister - 3 );
        } else {
          SymInfo.setSymbolType( SymbolInfo::LOCAL_COMPOSED_ARGUMENT );

          // Update the stack information
          align( mOffset,
            TCIR_CONFIGURATION->alignmentPointer );

          SymInfo.setSymbolOffset( mOffset );
          mOffset += pointerBytes;
        }
      }

    } else {
      SymInfo.setSymbolType( SymbolInfo::D_ARGUMENT );

      if ( myRegister ) {
        SymInfo.setSymbolArgumentPos( myRegister - 3 );
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
              TCIR_CONFIGURATION->alignmentInt );

            SymInfo.setSymbolOffset( mOffset );
            mOffset += intBytes;
            break;
          }

          case IR_Type::INT:
          case IR_Type::UNSIGNED_INT: {

            align( mOffset,
              TCIR_CONFIGURATION->alignmentInt );

            SymInfo.setSymbolOffset( mOffset );
            mOffset += intBytes;
            break;
          }

          case IR_Type::LONG:
          case IR_Type::UNSIGNED_LONG: {

            align( mOffset,
              TCIR_CONFIGURATION->alignmentLong );

            SymInfo.setSymbolOffset( mOffset );
            mOffset += longBytes;
            break;
          }

          case IR_Type::LONG_LONG:
          case IR_Type::UNSIGNED_LONG_LONG: {

            align( mOffset,
              TCIR_CONFIGURATION->alignmentLongLong );

            SymInfo.setSymbolOffset( mOffset );
            mOffset += longLongBytes;
            break;
          }

          case IR_Type::LONG_DOUBLE: {

            align( mOffset,
              TCIR_CONFIGURATION->alignmentLongDouble );

            SymInfo.setSymbolOffset( mOffset );
            mOffset += longDoubleBytes;
            break;
          }

          case IR_Type::DOUBLE: {

            align( mOffset,
              TCIR_CONFIGURATION->alignmentDouble );

            SymInfo.setSymbolOffset( mOffset );
            mOffset += doubleBytes;
            break;
          }

          case IR_Type::FLOAT: {

            align( mOffset,
              TCIR_CONFIGURATION->alignmentFloat );

            SymInfo.setSymbolOffset( mOffset );
            mOffset += floatBytes;
            break;
          }

          default: {
            throw ufFatalError( "Unsupported stack argument detected." );
            break;
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
  ufAssertT( sizeDifference == 0,
    "Incoherent results for parameter stack frame size computation!" );
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
        itArg != symbolList.end(); itArg++ ) {

    if ( ( *itArg )->getType().getType() == IR_Type::STRUCT ||
         ( *itArg )->getType().getType() == IR_Type::UNION ) {

      IR_ComposedType *compType = dynamic_cast<IR_ComposedType*>
        ( &( *itArg )->getType() );
      ufAssertT( compType, "Invalid type info!" );

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
      if ( composedTypeSize <= 8 && !isPassedThroughRegister( **itArg ) ) {
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
  for ( list<IR_Symbol*>::const_iterator
        itArg  = symbolList.begin();
        itArg != symbolList.end(); itArg++ ) {

    // Consider only symbols who are not passed on the stack anyways
    if ( isPassedThroughRegister( **itArg ) ) {

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

          align( mOffset,
            TCIR_CONFIGURATION->alignmentPointer );

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

              align( mOffset,
                TCIR_CONFIGURATION->alignmentBool );
              SymInfo.setSymbolOffset( mOffset );
              mOffset += boolBytes;
              break;
            }

            case IR_Type::CHAR:
            case IR_Type::UNSIGNED_CHAR: {

              align( mOffset,
                TCIR_CONFIGURATION->alignmentChar );

              SymInfo.setSymbolOffset( mOffset );
              mOffset += charBytes;
              break;
            }

            case IR_Type::SHORT:
            case IR_Type::UNSIGNED_SHORT: {

              align( mOffset,
                TCIR_CONFIGURATION->alignmentShort );

              SymInfo.setSymbolOffset( mOffset );
              mOffset += shortBytes;
              break;
            }

            case IR_Type::INT:
            case IR_Type::UNSIGNED_INT: {

              align( mOffset,
                TCIR_CONFIGURATION->alignmentInt );

              SymInfo.setSymbolOffset( mOffset );
              mOffset += intBytes;
              break;
            }

            case IR_Type::LONG:
            case IR_Type::UNSIGNED_LONG: {

              align( mOffset,
                TCIR_CONFIGURATION->alignmentLong );

              SymInfo.setSymbolOffset( mOffset );
              mOffset += longBytes;
              break;
            }

            case IR_Type::LONG_LONG:
            case IR_Type::UNSIGNED_LONG_LONG: {

              align( mOffset,
                TCIR_CONFIGURATION->alignmentLongLong );

              SymInfo.setSymbolOffset( mOffset );
              mOffset += longLongBytes;
              break;
            }

            case IR_Type::LONG_DOUBLE: {

              align( mOffset,
                TCIR_CONFIGURATION->alignmentLongDouble );

              SymInfo.setSymbolOffset( mOffset );
              mOffset += longDoubleBytes;
              break;
            }

            case IR_Type::DOUBLE: {

              align( mOffset,
                TCIR_CONFIGURATION->alignmentDouble );

              SymInfo.setSymbolOffset( mOffset );
              mOffset += doubleBytes;
              break;
            }

            case IR_Type::FLOAT: {

              align( mOffset,
                TCIR_CONFIGURATION->alignmentFloat );

              SymInfo.setSymbolOffset( mOffset );
              mOffset += floatBytes;
              break;
            }

            default: {
              throw ufFatalError( "Unsupported stack argument detected." );
              break;
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
  int stackSize = 0;

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

        align( s->mOffset,
          TCIR_CONFIGURATION->alignmentInt );

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
      //       adhere to special TriCore-EABI rules like:
      // * Any struct, bigger than 1 byte (possibly smaller than 2 byte when
      //   bitfields come into play) must be 2-byte aligned
      // * Bitfields are not aligned at all and the influence that they have on
      //   alignment of the overall struct is not statically defineable.
      //   (See EABI sec. 2.1.4.3.)
      // TODO: Even if we u
      align( s->mOffset,
        TCIR_CONFIGURATION->alignmentComposedType );

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
            throw ufFatalError( "Unsupported type of union element detected." );
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

      // TODO: Use struct-dependent alignment here. The alignment that is
      //       returned by the ICD-C is not usable here, because we have to
      //       adhere to special TriCore-EABI rules like:
      // * Any struct, bigger than 1 byte (possibly smaller than 2 byte when
      //   bitfields come into play) must be 2-byte aligned
      // * Bitfields are not aligned at all and the influence that they have on
      //   alignment of the overall struct is not statically defineable.
      //   (See EABI sec. 2.1.4.3.)
      align( s->mOffset,
        TCIR_CONFIGURATION->alignmentComposedType );

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
              TCIR_CONFIGURATION->alignmentComposedType );

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
            // Use IR_Type's function to determine size of this union
            stackSize = (*symElementIt)->getType().sizeOf();

            align( s->mOffset, (*symElementIt)->getType().alignment() );

            SymElementInfo.setSymbolOffset( s->mOffset );
            s->mOffset += stackSize;

          } else {
            throw
              ufFatalError( "Unsupported type of struct element detected." );
          }

          s->mSymbolMap.insert( pair<IR_Symbol *, SymbolInfo>(
            *symElementIt, SymElementInfo ) );

          (itStack.top().first)++;
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

      align( s->mOffset, TCIR_CONFIGURATION->alignmentInt );

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


unsigned int Stack::isPassedThroughRegister( const IR_Symbol &sym )
{
  // TODO: Use this method throughout the codeselector. Locations:
  // * all other function in this class
  // * argument passing rules
  // * symbolExp rules

  // If the symbol is not a function argument, then we return 0
  if ( sym.getSymbolTable().getFunction() ) {

    // Else get the function type and the parameter index of the symbol ...
    IR_FunctionType &funcType = sym.getSymbolTable().getFunction()
                                    ->getSymbol().getType();
    unsigned int paramIndex = 0;
    const list<IR_Symbol*> &symbols = sym.getSymbolTable().getSymbols();
    for ( list<IR_Symbol*>::const_iterator
          itSym  = symbols.begin();
          itSym != symbols.end(); itSym++ ) {
      if ( *itSym == &sym ) {
        break;
      } else {
        paramIndex++;
      }
    }
    ufAssertT( paramIndex < symbols.size(), "Invalid parameter index!" );

    // ... and call the main "isPassedThroughRegister" function
    return isPassedThroughRegister( funcType, paramIndex );

  } else {

    return 0;

  }
}


unsigned int Stack::isPassedThroughRegister( const IR_FunctionType &funcType,
  unsigned int parameterIndex )
{
  // Helper variables for keeping track of the used up registers
  bool regMap[4] = { false, false, false, false };
  unsigned int addrArgPos = 4;
  unsigned int currentParamIndex = 0;

  // If the return value is a struct that is bigger than 64 bits,
  // then a4 is already occupied for the return value
  IR_Type &returnType = funcType.getReturnType();
  if ( ( ( returnType.getType() == IR_Type::STRUCT ) ||
         ( returnType.getType() == IR_Type::UNION ) ) &&
       getStackSize( &returnType ) > 8 ) {
    addrArgPos++;
  }

  const list<IR_Type *> &paramTypes = funcType.getArgumentTypes();
  for ( list<IR_Type *>::const_iterator
        it  = paramTypes.begin();
        it != paramTypes.end(); it++ ) {

    // Whether the current symbol is passed by register
    bool passedThroughDataRegister = false;
    bool passedThroughAddrRegister = false;
    // Indicates, in which register the current symbol will be passed, if it
    // will be passed by register at all (as indicated by the boolean flag)
    unsigned int thisDataPos = 0;
    unsigned int thisAddrPos = addrArgPos;

    const enum IR_Type::Type currentType = ( *it )->getType();

    if ( currentType == IR_Type::POINTER ||
         currentType == IR_Type::ARRAY ) {

      passedThroughAddrRegister = addrArgPos < 8;
      addrArgPos++;

    } else if ( currentType == IR_Type::STRUCT ||
                currentType == IR_Type::UNION ) {

      IR_ComposedType * const compType = dynamic_cast<IR_ComposedType*>( *it );
      ufAssertT( compType, "Invalid type info!" );

      const int composedTypeSize = getStackSize( compType );

      // Structs bigger than 64 bit are passed by pointer, smaller ones are
      // passed in a data or extended register. In each case we may have to
      // pass the pointer/reg/ereg via the stack if the registers are already
      // occupied.
      if ( composedTypeSize <= 4 ) {
        // This struct will be passed in a reg if possible

        int firstFreeReg = 4;
        for ( int loop = 3; loop >= 0; loop-- )
          if ( !regMap[loop] )
            firstFreeReg = loop;

        passedThroughDataRegister = firstFreeReg < 4;
        if ( passedThroughDataRegister ) {
          thisDataPos = 4 + firstFreeReg;
          regMap[firstFreeReg] = true;
        }

      } else if ( composedTypeSize <= 8 ) {
        // This struct will be passed in an ereg if possible

        if ( !regMap[0] && !regMap[1] ) {
          passedThroughDataRegister = true;
          thisDataPos = 4;
          regMap[0] = true;
          regMap[1] = true;
        } else
        if ( !regMap[2] && !regMap[3] ) {
          passedThroughDataRegister = true;
          thisDataPos = 6;
          regMap[2] = true;
          regMap[3] = true;
        }

      } else {
        // This struct will be passed in an areg if possible

        passedThroughAddrRegister = addrArgPos < 8;
        addrArgPos += 1;

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
          passedThroughDataRegister = true;
          thisDataPos = 4;
          regMap[0] = true;
          regMap[1] = true;
        } else
        if ( !regMap[2] && !regMap[3] ) {
          passedThroughDataRegister = true;
          thisDataPos = 6;
          regMap[2] = true;
          regMap[3] = true;
        }

      } else {

        // Still free data register available?
        int firstFreeReg = 4;
        for ( int loop = 3; loop >= 0; loop-- )
          if ( !regMap[loop] )
            firstFreeReg = loop;

        passedThroughDataRegister = firstFreeReg < 4;
        if ( passedThroughDataRegister ) {
          thisDataPos = 4 + firstFreeReg;
          regMap[firstFreeReg] = true;
        }
      }

    }

    // If the current symbol was the one that we searched for,
    // then we have our return value
    if ( currentParamIndex == parameterIndex ) {
      if ( passedThroughDataRegister ) {
        ufAssertT( 4 <= thisDataPos && thisDataPos <= 7,
                   "Ilegal argument position!" );
        return thisDataPos;
      } else
      if ( passedThroughAddrRegister ) {
        ufAssertT( 4 <= thisAddrPos && thisAddrPos <= 7,
                   "Ilegal argument position!" );
        return thisAddrPos;
      } else {
        return 0;
      }
    } else {
      currentParamIndex++;
    }
  }

  // We did not find the parameter index, though we iterated over all parameters
  // -> the given parameter index must be invalid
  ufAssertT( 0, "Internal error: Invalid parameter index!" );
  return 0;
}


bool Stack::isAddressDirectlyTaken( const IR_Symbol &sym )
{
  // Look at all the locations where the address is reported to be taken
  for ( set<IR_SymbolExp *>::const_iterator
        itTakeExp  = sym.getAddrTaken().begin();
        itTakeExp != sym.getAddrTaken().end(); itTakeExp++ ) {

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
