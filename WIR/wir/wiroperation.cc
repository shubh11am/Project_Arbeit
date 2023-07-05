/*

   This source file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Intermediate Representation Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2015 - 2022, Heiko Falk.

   Hamburg University of Technology (TUHH)
   Institute of Embedded Systems
   21071 Hamburg
   Germany

   http://www.tuhh.de/es/esd/research/wcc

*/

/*!
  @file wiroperation.cc
  @brief This file implements %WIR operations.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include standard headers
#include <algorithm>
#include <iostream>
#include <iterator>
#include <list>
#include <map>
#include <typeinfo>
#include <vector>

// Include boost headers
#include <boost/current_function.hpp>

// Include libuseful headers
#include <libuseful/debugmacros.h>
#include <libuseful/io.h>

// Include WIR headers
#include <wir/wir.h>


//
// Code section
//

namespace WIR {


using namespace std;


// Depending on whether WIR is compiled in failsafe mode or not, constant
// mDefaultParameterChecking stores whether method checkParameters will be
// enabled or not by default.
#ifdef FAILSAFEMODE
const bool WIR_Operation::mDefaultParameterChecking = true;
#else
const bool WIR_Operation::mDefaultParameterChecking = false;
#endif

// mEnforceParameterChecking stores whether parameter checks by method
// checkParameters shall be enfored or not, irrespective of whether WIR is built
// in failsame mode or not.
bool WIR_Operation::mEnforceParameterChecking = false;


//
// Public class methods
//

/*
  Copy constructor.

  When copying an operation that is inserted in some %WIR instruction, the
  resulting copy will not be inserted in an instruction.
*/
WIR_Operation::WIR_Operation( const WIR_Operation &__o ) :
  WIR_ID_API { __o },
  WIR_Container_API { __o },
  mInstructionPointer { nullptr },
  mOpCode { __o.mOpCode },
  mOperationFormat { __o.mOperationFormat },
  mCheckParameters { true },
  mJumpTargets { __o.mJumpTargets }
{
  DSTART( "WIR_Operation::WIR_Operation(const WIR_Operation&)" );

  copyOperation( __o );
};


/*
  Move constructor.

  Trying to move an operation that is inserted in some %WIR instruction results
  in an assertion, since you are not allowed to move an operation whose
  ownership is managed by an instruction.
*/
WIR_Operation::WIR_Operation( WIR_Operation &&__o ) :
  WIR_ID_API { move( __o ) },
  WIR_Container_API { move( __o ) },
  mParameterPointers { move( __o.mParameterPointers ) },
  mParameterReferences { move( __o.mParameterReferences ) },
  mInstructionPointer { nullptr },
  mExplicitParameterReferences { move( __o.mExplicitParameterReferences ) },
  mOpCode { __o.mOpCode },
  mOperationFormat { __o.mOperationFormat },
  mCheckParameters { true },
  mDontOptimize { __o.mDontOptimize },
  mJumpTargets { move( __o.mJumpTargets ) }
{
  DSTART( "WIR_Operation::WIR_Operation(WIR_Operation&&)" );

  ufAssertT(
    __o.mInstructionPointer == nullptr,
    "Invalid attempt to move an operation out of its owning instruction." );

  __o.mOpCode = nullptr;
  __o.mOperationFormat = nullptr;
  __o.mExplicitParameterReferences.clear();
  __o.mParameterReferences.clear();
  __o.mParameterPointers.clear();
  __o.mJumpTargets.clear();

  // Adjust the parent IDs of the operation's parameters.
  for ( WIR_Parameter &p : mParameterReferences )
    p.onInsert( this );
};


/*
  Destructor.
*/
WIR_Operation::~WIR_Operation( void )
{
  DSTART( "virtual WIR_Operation::~WIR_Operation()" );

  mCheckParameters = false;
  clearParameters();
};


/*
  Copy-assignment operator.

  When copying an operation that is inserted in some %WIR instruction, the
  resulting copy will not be inserted in an instruction.
*/
WIR_Operation & WIR_Operation::operator = ( const WIR_Operation &__o )
{
  DSTART( "WIR_Operation& WIR_Operation::operator=(const WIR_Operation&)" );

  WIR_Container_API::operator = ( __o );

  mOpCode = __o.mOpCode;
  mOperationFormat = __o.mOperationFormat;
  mInstructionPointer = nullptr;
  mJumpTargets = __o.mJumpTargets;

  copyOperation( __o );

  return( *this );
};


/*
  Move-assignment operator.

  Trying to move an operation that is inserted in some %WIR instruction results
  in an assertion, since you are not allowed to move an operation whose
  ownership is managed by an instruction.
*/
WIR_Operation & WIR_Operation::operator = ( WIR_Operation &&__o )
{
  DSTART( "WIR_Operation& WIR_Operation::operator=(WIR_Operation&&)" );

  ufAssertT(
    __o.mInstructionPointer == nullptr,
    "Invalid attempt to move an operation out of its owning instruction." );

  WIR_Container_API::operator = ( move( __o ) );

  mOpCode = move( __o.mOpCode );
  __o.mOpCode = nullptr;
  mOperationFormat = move( __o.mOperationFormat );
  __o.mOperationFormat = nullptr;
  mParameterPointers = move( __o.mParameterPointers );
  __o.mParameterPointers.clear();
  mParameterReferences = move( __o.mParameterReferences );
  __o.mParameterReferences.clear();
  mExplicitParameterReferences = move( __o.mExplicitParameterReferences );
  __o.mExplicitParameterReferences.clear();
  mInstructionPointer = nullptr;
  mDontOptimize = __o.mDontOptimize;
  mJumpTargets = move( __o.mJumpTargets );
  __o.mJumpTargets.clear();

  // Adjust the parent IDs of the operation's parameters.
  for ( WIR_Parameter &p : mParameterReferences )
    p.onInsert( this );

  return( *this );
};


/*
  getOpCode returns an operation's opcode.
*/
const WIR_BaseProcessor::OpCode &WIR_Operation::getOpCode( void ) const
{
  DSTART( "const WIR_BaseProcessor::OpCode& WIR_Operation::getOpCode() const" );

  ufAssertT(
    mOpCode != nullptr,
    "Attempt to get opcode from an operation that has previously been moved." );

  return( *mOpCode );
};


/*
  getOperationFormat returns an operation's format.
*/
const WIR_BaseProcessor::OperationFormat &WIR_Operation::getOperationFormat( void ) const
{
  DSTART(
    "const WIR_BaseProcessor::OperationFormat& WIR_Operation::getOperationFormat() const" );

  ufAssertT(
    mOperationFormat != nullptr,
    "Attempt to get operation format from an operation that has previously " <<
    "been moved." );

  return( *mOperationFormat );
};


//
// API implementations.
//

WIR_INSERTION_IMPL( WIR_Instruction, Instruction, WIR_Operation );


WIR_Parameter & WIR_Operation::pushBackParameter( const WIR_Parameter &o )
{
  DSTART(
    "WIR_Parameter& WIR_Operation::pushBackParameter(const WIR_Parameter&)" );

  checkDontOptimize();
  checkVREGs( o );

  auto *p = o.clone();
  mParameterPointers.push_back( unique_ptr<WIR_Parameter>( p ) );
  p->onInsert( this );
  mParameterReferences.push_back( *p );
  if ( p->isExplicit() )
    mExplicitParameterReferences.push_back( *p );

  if ( mCheckParameters )
    checkParameters();

  return( mParameterReferences.back().get() );
};


WIR_Parameter & WIR_Operation::pushBackParameter( WIR_Parameter &&o )
{
  DSTART(
    "WIR_Parameter& WIR_Operation::pushBackParameter(WIR_Parameter&&)" );

  checkDontOptimize();
  checkVREGs( o );

  auto *p = o.clone();
  mParameterPointers.push_back( unique_ptr<WIR_Parameter>( p ) );
  p->onInsert( this );
  mParameterReferences.push_back( *p );
  if ( p->isExplicit() )
    mExplicitParameterReferences.push_back( *p );

  if ( mCheckParameters )
    checkParameters();

  return( mParameterReferences.back().get() );
};


/*
  pushBackParameter adds a new WIR_Parameter at the end of lists
  mParameterPointers and mParameterReferences, after its current last element.

  Class WIR_Operation takes over full control over the ownership of the given
  pointer! In particular, WIR_Operation automatically destroys the object
  pointed to. Users of this variant of pushBackParameter are strongly
  discouraged of continuing to use this pointer afterwards. This variant of
  pushBackParameter is more efficient than the previous ones since it completely
  avoids any (polymorphic) copy operations. Thus, it should only be used if
  large amounts of parameters shall be created/added highly efficiently as,
  e.g., in operation factories of a compiler's code selector.
*/
WIR_Parameter & WIR_Operation::pushBackParameter( WIR_Parameter *p )
{
  DSTART( "WIR_Parameter& WIR_Operation::pushBackParameter(WIR_Parameter*)" );

  checkDontOptimize();
  checkVREGs( *p );

  mParameterPointers.push_back( unique_ptr<WIR_Parameter>( p ) );
  p->onInsert( this );
  mParameterReferences.push_back( *p );
  if ( p->isExplicit() )
    mExplicitParameterReferences.push_back( *p );

  if ( mCheckParameters )
    checkParameters();

  return( mParameterReferences.back().get() );
};


WIR_Parameter & WIR_Operation::pushFrontParameter( const WIR_Parameter &o )
{
  DSTART(
    "WIR_Parameter& WIR_Operation::pushFrontParameter(const WIR_Parameter&)" );

  checkDontOptimize();
  checkVREGs( o );

  auto *p = o.clone();
  mParameterPointers.push_front( unique_ptr<WIR_Parameter>( p ) );
  p->onInsert( this );
  mParameterReferences.push_front( *p );
  if ( p->isExplicit() )
    mExplicitParameterReferences.push_front( *p );

  if ( mCheckParameters )
    checkParameters();

  return( mParameterReferences.front().get() );
};


WIR_Parameter & WIR_Operation::pushFrontParameter( WIR_Parameter &&o )
{
  DSTART( "WIR_Parameter& WIR_Operation::pushFrontParameter(WIR_Parameter&&)" );

  checkDontOptimize();
  checkVREGs( o );

  auto *p = o.clone();
  mParameterPointers.push_front( unique_ptr<WIR_Parameter>( p ) );
  p->onInsert( this );
  mParameterReferences.push_front( *p );
  if ( p->isExplicit() )
    mExplicitParameterReferences.push_front( *p );

  if ( mCheckParameters )
    checkParameters();

  return( mParameterReferences.front().get() );
};


std::list<std::reference_wrapper<WIR_Parameter>>::iterator WIR_Operation::insertParameter( std::list<std::reference_wrapper<WIR_Parameter>>::const_iterator pos,
                                                                                           const WIR_Parameter &o )
{
  DSTART(
    "list<reference_wrapper<WIR_Parameter> >::iterator WIR_Operation::insertParameter(list<reference_wrapper<WIR_Parameter> >::const_iterator, const WIR_Parameter&)" );

  checkDontOptimize();
  checkVREGs( o );

  auto itp = mParameterPointers.begin();
  auto iter = mExplicitParameterReferences.begin();
  for ( auto itr = mParameterReferences.begin(); itr != pos; ++itr, ++itp )
    if ( itr->get().isExplicit() )
      ++iter;

  auto *p = o.clone();
  mParameterPointers.insert( itp, unique_ptr<WIR_Parameter>( p ) );
  p->onInsert( this );
  auto it = mParameterReferences.insert( pos, *p );
  if ( p->isExplicit() )
    mExplicitParameterReferences.insert( iter, *p );

  if ( mCheckParameters )
    checkParameters();

  return( it );
};


std::list<std::reference_wrapper<WIR_Parameter>>::iterator WIR_Operation::insertParameter( std::list<std::reference_wrapper<WIR_Parameter>>::const_iterator pos,
                                                                                           WIR_Parameter &&o )
{
  DSTART(
    "list<reference_wrapper<WIR_Parameter> >::iterator WIR_Operation::insertParameter(list<reference_wrapper<WIR_Parameter> >::const_iterator, WIR_Parameter&&)" );

  checkDontOptimize();
  checkVREGs( o );

  auto itp = mParameterPointers.begin();
  auto iter = mExplicitParameterReferences.begin();
  for ( auto itr = mParameterReferences.begin(); itr != pos; ++itr, ++itp )
    if ( itr->get().isExplicit() )
      ++iter;

  auto *p = o.clone();
  mParameterPointers.insert( itp, unique_ptr<WIR_Parameter>( p ) );
  p->onInsert( this );
  auto it = mParameterReferences.insert( pos, *p );
  if ( p->isExplicit() )
    mExplicitParameterReferences.insert( iter, *p );

  if ( mCheckParameters )
    checkParameters();

  return( it );
};


std::list<std::reference_wrapper<WIR_Parameter>>::iterator WIR_Operation::replaceParameter( std::list<std::reference_wrapper<WIR_Parameter>>::const_iterator pos,
                                                                                            const WIR_Parameter &o )
{
  DSTART(
    "list<reference_wrapper<WIR_Parameter> >::iterator WIR_Operation::replaceParameter(list<reference_wrapper<WIR_Parameter> >::const_iterator, const WIR_Parameter&)" );

  checkDontOptimize();
  checkVREGs( o );

  auto itp = mParameterPointers.begin();
  auto iter = mExplicitParameterReferences.begin();
  for ( auto itr = mParameterReferences.begin(); itr != pos; ++itr, ++itp )
    if ( itr->get().isExplicit() )
      ++iter;

  auto *p = o.clone();
  mParameterPointers.insert( itp, unique_ptr<WIR_Parameter>( p ) );
  p->onInsert( this );
  auto it = mParameterReferences.insert( pos, *p );
  if ( p->isExplicit() )
    mExplicitParameterReferences.insert( iter, *p );

  if ( ( iter != mExplicitParameterReferences.end() ) &&
       iter->get().isExplicit() )
    mExplicitParameterReferences.erase( iter );
  mParameterReferences.erase( pos );
  mParameterPointers.erase( itp );

  checkParameters();
  return( it );
};


std::list<std::reference_wrapper<WIR_Parameter>>::iterator WIR_Operation::replaceParameter( std::list<std::reference_wrapper<WIR_Parameter>>::const_iterator pos,
                                                                                            WIR_Parameter &&o )
{
  DSTART(
    "list<reference_wrapper<WIR_Parameter> >::iterator WIR_Operation::replaceParameter(list<reference_wrapper<WIR_Parameter> >::const_iterator, WIR_Parameter&&)" );

  checkDontOptimize();
  checkVREGs( o );

  auto itp = mParameterPointers.begin();
  auto iter = mExplicitParameterReferences.begin();
  for ( auto itr = mParameterReferences.begin(); itr != pos; ++itr, ++itp )
    if ( itr->get().isExplicit() )
      ++iter;

  auto *p = o.clone();
  mParameterPointers.insert( itp, unique_ptr<WIR_Parameter>( p ) );
  p->onInsert( this );
  auto it = mParameterReferences.insert( pos, *p );
  if ( p->isExplicit() )
    mExplicitParameterReferences.insert( iter, *p );

  if ( ( iter != mExplicitParameterReferences.end() ) &&
       iter->get().isExplicit() )
    mExplicitParameterReferences.erase( iter );
  mParameterReferences.erase( pos );
  mParameterPointers.erase( itp );

  checkParameters();
  return( it );
};


void WIR_Operation::popBackParameter( void )
{
  DSTART( "void WIR_Operation::popBackParameter()" );

  checkDontOptimize();

  if ( mParameterReferences.empty() )
    return;

  if ( mParameterReferences.back().get().isExplicit() )
    mExplicitParameterReferences.pop_back();
  mParameterReferences.pop_back();
  mParameterPointers.pop_back();

  checkParameters();
};


void WIR_Operation::popFrontParameter( void )
{
  DSTART( "void WIR_Operation::popFrontParameter()" );

  checkDontOptimize();

  if ( mParameterReferences.empty() )
    return;

  if ( mParameterReferences.front().get().isExplicit() )
    mExplicitParameterReferences.pop_front();
  mParameterReferences.pop_front();
  mParameterPointers.pop_front();

  checkParameters();
};


std::list<std::reference_wrapper<WIR_Parameter>>::iterator WIR_Operation::eraseParameter( std::list<std::reference_wrapper<WIR_Parameter>>::const_iterator pos )
{
  DSTART(
    "list<reference_wrapper<WIR_Parameter> >::iterator WIR_Operation::eraseParameter(list<reference_wrapper<WIR_Parameter> >::const_iterator)" );

  checkDontOptimize();

  auto itp = mParameterPointers.begin();
  auto iter = mExplicitParameterReferences.begin();
  for ( auto itr = mParameterReferences.begin(); itr != pos; ++itr, ++itp )
    if ( itr->get().isExplicit() )
      ++iter;

  if ( pos->get().isExplicit() )
    mExplicitParameterReferences.erase( iter );
  auto it = mParameterReferences.erase( pos );
  mParameterPointers.erase( itp );

  checkParameters();
  return( it );
};


void WIR_Operation::clearParameters( void )
{
  DSTART( "void WIR_Operation::clearParameters()" );

  checkDontOptimize();

  mExplicitParameterReferences.clear();
  mParameterReferences.clear();
  mParameterPointers.clear();

  if ( mCheckParameters )
    checkParameters();
};


const std::list<std::reference_wrapper<WIR_Parameter>> &WIR_Operation::getParameters( void ) const
{
  DSTART(
    "const list<reference_wrapper<WIR_Parameter> >& WIR_Operation::getParameters() const" );

  return( mParameterReferences );
};


const std::list<std::reference_wrapper<WIR_Parameter>> &WIR_Operation::getExplicitParameters( void ) const
{
  DSTART(
    "const list<reference_wrapper<WIR_Parameter> >& WIR_Operation::getExplicitParameters() const" );

  return( mExplicitParameterReferences );
};


/*
  getExplicitParameter returns the nth explicit parameter of an operation.

  getExplicitParameter asserts if n is larger than the actual number of explicit
  parameters of an operation.
*/
WIR_Parameter &WIR_Operation::getExplicitParameter( unsigned int n ) const
{
  DSTART(
    "WIR_Parameter& WIR_Operation::getExplicitParameter(unsigned int) const" );

  #ifdef FAILSAFEMODE
  ufAssert( n <= mExplicitParameterReferences.size() );
  #endif

  auto it = mExplicitParameterReferences.begin();
  advance( it, n - 1 );

  return( it->get() );
};


/*
  begin returns an iterator to the first parameter of an operation.
*/
std::list<std::reference_wrapper<WIR_Parameter>>::const_iterator WIR_Operation::begin( void ) const
{
  DSTART(
    "list<reference_wrapper<WIR_Parameter> >::const_iterator WIR_Operation::begin() const" );

  return( mParameterReferences.begin() );
};


/*
  end returns an iterator to the end of an operation's parameter list.
*/
std::list<std::reference_wrapper<WIR_Parameter>>::const_iterator WIR_Operation::end( void ) const
{
  DSTART(
    "list<reference_wrapper<WIR_Parameter> >::const_iterator WIR_Operation::end() const" );

  return( mParameterReferences.end() );
};


/*
  rbegin returns an iterator to the reverse-first parameter of an operation.
*/
std::list<std::reference_wrapper<WIR_Parameter>>::const_reverse_iterator WIR_Operation::rbegin( void ) const
{
  DSTART(
    "list<reference_wrapper<WIR_Parameter> >::const_reverse_iterator WIR_Operation::rbegin() const" );

  return( mParameterReferences.rbegin() );
};


/*
  rend returns an iterator to the reverse-end of an operation's parameter list.
*/
std::list<std::reference_wrapper<WIR_Parameter>>::const_reverse_iterator WIR_Operation::rend( void ) const
{
  DSTART(
    "list<reference_wrapper<WIR_Parameter> >::const_reverse_iterator WIR_Operation::rend() const" );

  return( mParameterReferences.rend() );
};


bool WIR_Operation::containsParameter( WIR_id_t id ) const
{
  DSTART( "bool WIR_Operation::containsParameter(WIR_id_t) const" );

  for ( WIR_Parameter &item : mParameterReferences )
    if ( item.getID() == id )
      return( true );

  return( false );
};


bool WIR_Operation::containsParameter( const WIR_Parameter &o ) const
{
  DSTART( "bool WIR_Operation::containsParameter(const WIR_Parameter&) const" );

  for ( WIR_Parameter &item : mParameterReferences )
    if ( item == o )
      return( true );

  return( false );
};


std::list<std::reference_wrapper<WIR_Parameter>>::const_iterator WIR_Operation::findParameter( WIR_id_t id ) const
{
  DSTART(
    "list<reference_wrapper<WIR_Parameter> >::const_iterator WIR_Operation::findParameter(WIR_id_t) const" );

  for ( auto it = mParameterReferences.begin();
        it != mParameterReferences.end(); ++it )
    if ( (*it).get().getID() == id )
      return( it );

  return( mParameterReferences.end() );
};


std::list<std::reference_wrapper<WIR_Parameter>>::const_iterator WIR_Operation::findParameter( const WIR_Parameter &o ) const
{
  DSTART(
    "list<reference_wrapper<WIR_Parameter> >::const_iterator WIR_Operation::findParameter(const WIR_Parameter&) const" );

  for ( auto it = mParameterReferences.begin();
        it != mParameterReferences.end(); ++it )
    if ( (*it).get() == o )
      return( it );

  return( mParameterReferences.end() );
};


/*
  getNumberOfExplicitParameters returns the number of explicit parameters of an
  operation.
*/
unsigned int WIR_Operation::getNumberOfExplicitParameters( void ) const
{
  DSTART( "unsigned int WIR_Operation::getNumberOfExplicitParameters() const" );

  return( mExplicitParameterReferences.size() );
};


/*
  getNumberOfImplicitParameters returns the number of implicit parameters of an
  operation.
*/
unsigned int WIR_Operation::getNumberOfImplicitParameters( void ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  return( mParameterReferences.size() - mExplicitParameterReferences.size() );
};


/*
  enforceParameterChecking enables parameter checks by method checkParameters,
  even if WIR is not built in failsafe mode.

  Use this method with care as it has a performance impact. It should be used,
  e.g., within an assembly code parser in order to ensure that all operations
  created by the parser actually have a good operation format.
*/
void WIR_Operation::enforceParameterChecking( void )
{
  DSTART( "static void WIR_Operation::enforceParameterChecking()" );

  mEnforceParameterChecking = true;
};


/*
  resetParameterChecking deactivates the effect of method
  enforceParameterChecking and returns to WIR's default behavior, depending on
  whether the failsafe mode is used or not.
*/
void WIR_Operation::resetParameterChecking( void )
{
  DSTART( "static void WIR_Operation::resetParameterChecking()" );

  mEnforceParameterChecking = false;
};


/*
  getBitWidth returns an operation's bit width.
*/
unsigned int WIR_Operation::getBitWidth( void ) const
{
  DSTART( "unsigned int WIR_Operation::getBitWidth() const" );

  return( getOperationFormat().getBitWidth() );
};


/*
  getSize returns an operation's size in bytes.

  getSize rounds upwards. I.e., if an operation occupies 20 bits, getSize
  returns a size of 3 bytes.
*/
unsigned long long WIR_Operation::getSize( void ) const
{
  DSTART( "long long unsigned int WIR_Operation::getSize() const" );

  unsigned int bits = getBitWidth();

  if ( bits % 8 != 0 )
    return( getBitWidth() / 8 + 1 );
  else
    return( getBitWidth() / 8 );
};


/*
  isMemoryAccess returns whether an operation accesses memory without being an
  explicit load or store operation.
*/
bool WIR_Operation::isMemoryAccess( void ) const
{
  DSTART( "bool WIR_Operation::isMemoryAccess() const" );

  return( getOpCode().isMemoryAccess( *this ) );
};


/*
  isMemoryStore returns whether an operation writes to memory.
*/
bool WIR_Operation::isMemoryStore( void ) const
{
  DSTART( "bool WIR_Operation::isMemoryStore() const" );

  return( getOpCode().isMemoryStore( *this ) );
};


/*
  isMemoryLoad returns whether an operation reads from memory.
*/
bool WIR_Operation::isMemoryLoad( void ) const
{
  DSTART( "bool WIR_Operation::isMemoryLoad() const" );

  return( getOpCode().isMemoryLoad( *this ) );
};


/*
  isMove returns whether an operation is a register-register move.
*/
bool WIR_Operation::isMove( void ) const
{
  DSTART( "bool WIR_Operation::isMove() const" );

  return( getOpCode().isMove( *this ) );
};


/*
  isCall returns whether an operation calls a function.
*/
bool WIR_Operation::isCall( void ) const
{
  DSTART( "bool WIR_Operation::isCall() const" );

  return( getOpCode().isCall( *this ) );
};


/*
  isIndirectCall returns whether an operation indirectly calls a function.
*/
bool WIR_Operation::isIndirectCall( void ) const
{
  DSTART( "bool WIR_Operation::isIndirectCall() const" );

  return( getOpCode().isIndirectCall( *this ) );
};


/*
  isReturn returns whether an operation returns from a function call.
*/
bool WIR_Operation::isReturn( void ) const
{
  DSTART( "bool WIR_Operation::isReturn() const" );

  return( getOpCode().isReturn( *this ) );
};


/*
  isJump returns whether an operation performs a jump.
*/
bool WIR_Operation::isJump( void ) const
{
  DSTART( "bool WIR_Operation::isJump() const" );

  return( isConditionalJump() || isUnconditionalJump() || isIndirectJump() );
};


/*
  isConditionalJump returns whether an operation performs a conditional jump.
*/
bool WIR_Operation::isConditionalJump( void ) const
{
  DSTART( "bool WIR_Operation::isConditionalJump() const" );

  return( getOpCode().isConditionalJump( *this ) );
};


/*
  isUnconditionalJump returns whether an operation performs an unconditional
  jump.
*/
bool WIR_Operation::isUnconditionalJump( void ) const
{
  DSTART( "bool WIR_Operation::isUnconditionalJump() const" );

  return( getOpCode().isUnconditionalJump( *this ) );
};


/*
  isIndirectJump returns whether an operation indirectly performs a jump.
*/
bool WIR_Operation::isIndirectJump( void ) const
{
  DSTART( "bool WIR_Operation::isIndirectJump() const" );

  return( getOpCode().isIndirectJump( *this ) );
};


/*
  isAsmDataDirective returns whether an opcode is an assembly data directive.
*/
bool WIR_Operation::isAsmDataDirective( void ) const
{
  DSTART( "bool WIR_Operation::isAsmDataDirective() const" );

  return( getOpCode().isAsmDataDirective( *this ) );
};


/*
  hasSideEffects returns whether an opcode has side effects.

  Side effects could be non-obvious changes of a processor's internal status
  like, e.g., disabling interrupts, changing into supervisor mode or
  invalidating cache lines etc.
*/
bool WIR_Operation::hasSideEffects( void ) const
{
  DSTART( "bool WIR_Operation::hasSideEffects() const" );

  return( getOpCode().hasSideEffects( *this ) );
};


/*
  If this WIR_Operation is a jump, addJumpTarget adds the specified WIR basic
  block to the set of explicit jump targets.
*/
void WIR_Operation::addJumpTarget( const WIR_BasicBlock &b )
{
  DSTART( "void WIR_Operation::addJumpTarget(const WIR_BasicBlock&)" );

  if ( isJump() )
    mJumpTargets.insert( const_cast<WIR_BasicBlock &>( b ) );
};


/*
  If this WIR_Operation is a jump, eraseJumpTarget removes the specified WIR
  basic block from the set of explicit jump targets.
*/
void WIR_Operation::eraseJumpTarget( const WIR_BasicBlock &b )
{
  DSTART( "void WIR_Operation::eraseJumpTarget(const WIR_BasicBlock&)" );

  if ( isJump() )
    mJumpTargets.erase( const_cast<WIR_BasicBlock &>( b ) );
};


/*
  jumpTargetsAdded returns whether explicit jump targets have been specified for
  this WIR_Operation.
*/
bool WIR_Operation::jumpTargetsAdded( void ) const
{
  DSTART( "bool WIR_Operation::jumpTargetsAdded() const" );

  return( !mJumpTargets.empty() );
};


/*
  If this WIR_Operation is a jump, getJumpTargets returns a set of WIR basic
  blocks to which this operation jumps.

  The set of jump targets is determined as follows:
  -# If this operation is not a jump, the empty set is returned.
  -# If jump targets were explicitly specified using addJumpTarget, then
     the set of these explicit jump targets is returned.
  -# If no explicit jump targets were specified, it is checked whether this
     jump contains label parameters, and the set of all basic blocks that are
     refered by these labels is returned.
*/
WIR_BasicBlockSet WIR_Operation::getJumpTargets( void ) const
{
  DSTART( "WIR_BasicBlockSet WIR_Operation::getJumpTargets() const" );

  WIR_BasicBlockSet res;

  // If it's no jump, return the empty set.
  if ( !isJump() )
    return( res );

  // If explicit jump targets were given, return them.
  if ( !mJumpTargets.empty() ) {
    for ( WIR_BasicBlock &b : mJumpTargets )
      res.insert( b );

    return( res );
  }

  // Otherwise, check the jump's label parameters.
  for ( WIR_Parameter &p : getParameters() )
    if ( p.getType() == WIR_ParameterType::label ) {
      WIR_LabelParameter &lp = dynamic_cast<WIR_LabelParameter &>( p );
      WIR_BasicBlock *tmp = nullptr;

      if ( lp.getLabelType() == WIR_SymbolType::block )
        // The label refers to a basic block. Store b in tmp.
        tmp = &(lp.getBasicBlock());
      else

      if ( lp.getLabelType() == WIR_SymbolType::function ) {

        // The label refers to a function.
        WIR_Function &f = lp.getFunction();

        // Store f's first basic block in tmp.
        for ( WIR_BasicBlock &b : f ) {
          tmp = &b;
          break;
        }
      }

      // Add found basic block from tmp to res.
      if ( tmp != nullptr )
        res.insert( *tmp );
    }

  return( res );
};


/*
  getVREGs determines all virtual registers that occur in this operation's
  parameters.
*/
WIR_VirtualRegisterSet WIR_Operation::getVREGs( void ) const
{
  DSTART( "WIR_VirtualRegisterSet WIR_Operation::getVREGs() const" );

  WIR_VirtualRegisterSet res;

  for ( WIR_Parameter &p : getParameters() )
    if ( p.getType() == WIR_ParameterType::reg ) {
      auto &rp = dynamic_cast<WIR_RegisterParameter &>( p );
      auto &r = rp.getRegister();

      if ( r.isVirtual() )
        res.insert( dynamic_cast<WIR_VirtualRegister &>( r ) );
    }

  return( res );
};


/*
  setDontOptimize sets whether an operation can be modified or must not be
  changed by some optimization or transformation.
*/
void WIR_Operation::setDontOptimize( bool f )
{
  DSTART( "void WIR_Operation::setDontOptimize(bool)" );

  mDontOptimize = f;
};


/*
  getDontOptimize returns whether an operation can be modified or must not be
  changed by some optimization or transformation.

  An operation must not be modified if the operation by itself has been marked
  as such using setDontOptimize, or if it is inserted into a %WIR instruction
  that in turn must not be modified.
*/
bool WIR_Operation::getDontOptimize( void ) const
{
  DSTART( "bool WIR_Operation::getDontOptimize() const" );

  return(
    mDontOptimize || ( isInserted() && getInstruction().getDontOptimize() ) );
};


/*
  The << operator dumps a WIR operation to an output stream.

  By applying processor-specific I/O manipulators to the output stream
  beforehand, this << operator can flexibly emit valid assembly output for
  arbitrary processor architectures.
*/
std::ostream & operator << ( std::ostream &os, const WIR_Operation &o )
{
  DSTART( "ostream& operator<<(ostream&, const WIR_Operation&)" );

  WIR_Registry::getOperationDumper( os.iword( WIR_ProcessorIO() ) )( os, o );

  return( os );
};


//
// Private class methods
//

/*
  checkOperationFormat ensures that the operation format stored in
  mOperationFormat is legal for the opcode from mOpCode.

  If this operation contains an illegal opcode/operation format combination,
  checkOperationFormat asserts.
*/
void WIR_Operation::checkOperationFormat( void ) const
{
  DSTART( "void WIR_Operation::checkOperationFormat() const" );

  #ifdef FAILSAFEMODE
  ufAssertT(
    WIR_Registry::isLegalOperationFormat( *mOpCode, *mOperationFormat ),
    "Instruction format '" << mOperationFormat->getName() <<
    "' incompatible with opcode '" << mOpCode->getName() << "'." );
  #endif
};


/*
  checkParameters ensures that the parameters stored in list
  mParameterReferences adhere to the operation format from mOperationFormat.

  If this operation contains a parameter sequence that does not match the
  specified operation format, checkParameters asserts.
*/
void WIR_Operation::checkParameters( void ) const
{
  DSTART( "void WIR_Operation::checkParameters() const" );

  if ( !mDefaultParameterChecking && !mEnforceParameterChecking )
    return;

  auto &format = WIR_Registry::getOperationFormat( *mOperationFormat );
  auto &formatParameters = format.getParameters();

  // First, we have to check that the number of parameters is equal.
  ufAssertT(
    getNumberOfExplicitParameters() == formatParameters.size(),
    "Number of parameters incompatible with operation format '" <<
    mOperationFormat->getName() << "'." );

  // Next, we compare the explicit parameters with the operation format one by
  // one.
  auto formatIt = formatParameters.begin();
  for ( WIR_Parameter &p : mExplicitParameterReferences ) {
    bool ok = true;

    if ( formatIt == formatParameters.end() )
      ok = false;
    else

    if ( p.getType() != (*formatIt).get().getType() )
      ok = false;
    else
      switch ( p.getType() ) {

        case WIR_ParameterType::addr: {
          // For addressing mode parameters, we need to make sure that the
          // involved addressing modes are the same.
          auto &regP = dynamic_cast<WIR_AddressingModeParameter &>( p );
          auto &regF =
            dynamic_cast<WIR_AddressingModeParameter &>( (*formatIt).get() );

          if ( !regP.getAddressingMode().isCompatible(
                  regF.getAddressingMode(), getOperationFormat(),
                  getOpCode() ) )
            ok = false;

          break;
        }

        case WIR_ParameterType::cond: {
          // For condition field parameters, we need to make sure that the
          // involved conditions are the same.
          auto &regP = dynamic_cast<WIR_ConditionFieldParameter &>( p );
          auto &regF =
            dynamic_cast<WIR_ConditionFieldParameter &>( (*formatIt).get() );

          if ( regP.getCondition().getProcessorTypeName() !=
                regF.getCondition().getProcessorTypeName() )
            ok = false;

          break;
        }

        case WIR_ParameterType::imm: {
          // For immediate parameters, we need to make sure that the
          // parameters are of the same derived class type.
          auto &regP = dynamic_cast<WIR_BaseImmediateParameter &>( p );
          auto &regF =
            dynamic_cast<WIR_BaseImmediateParameter &>( (*formatIt).get() );

          if ( regP.getImmediateType() != regF.getImmediateType() )
            ok = false;

          break;
        }

        case WIR_ParameterType::reg: {
          // For register parameters, we need to make sure that the involved
          // registers have the same register type AND that their usage types
          // are the same.
          auto &regP = dynamic_cast<WIR_RegisterParameter &>( p );
          auto &regF =
            dynamic_cast<WIR_RegisterParameter &>( (*formatIt).get() );

          if ( !regP.getRegister().getType().isCompatible(
                  regF.getRegister().getType(), regP.getRegister(),
                  regF.getRegister() ) ||
                ( regP.getUsage() != regF.getUsage() ) )
            ok = false;

          // Furthermore, if the operation format requires a dedicated
          // physical register, we need to check that the operation parameter
          // refers to exactly this physical register.
          if ( regF.getRegister().isPhysical() &&
                ( regF.getRegister().getName() !=
                    regP.getRegister().getName() ) )
            ok = false;

          break;
        }

        default: {
          // Catch-all for label parameters.
          break;
        }

      }

    ufAssertT(
      ok,
      "Parameters incompatible with operation format '" <<
      mOperationFormat->getName() << "'." );

    ++formatIt;
  }
};


/*
  copyOperation performs actions common to the copy constructor and copy
  assignment operator of WIR operations.
*/
void WIR_Operation::copyOperation( const WIR_Operation &__o )
{
  DSTART( "void WIR_Operation::copyOperation(const WIR_Operation&)" );

  mDontOptimize = false;
  mCheckParameters = false;

  // Sort the IDs of parameters from __o.
  vector<WIR_id_t> originalIDs;
  for ( WIR_Parameter &p : __o.mParameterReferences )
    originalIDs.push_back( p.getID() );
  sort( originalIDs.begin(), originalIDs.end() );

  // Copy and insert parameters according to the sorted order.
  clearParameters();
  for ( auto id : originalIDs ) {
    auto it = mParameterReferences.begin();
    auto it1 = __o.mParameterReferences.begin();

    while ( (*it1).get().getID() != id ) {
      if ( (*it1).get().getID() < id )
        ++it;
      ++it1;
    }

    insertParameter( it, (*it1).get() );
  }

  mCheckParameters = true;
  mDontOptimize = __o.mDontOptimize;
};


/*
  checkDontOptimize checks whether an operation must not be modified.

  If this operation must not be modified, checkDontOptimize asserts.
*/
void WIR_Operation::checkDontOptimize( void ) const
{
  DSTART( "void WIR_Operation::checkDontOptimize() const" );

  ufAssertT(
    !getDontOptimize(),
    "Illegal attempt to modify an operation that is set as 'don't optimize'!" );
};


/*
  Upon insertion of a parameter into an operation, checkVREGs verifies that no
  virtual register occuring in the parameter belongs to a function different
  than that owning this operation.

  If a virtual register belongs to a different function, checkVREGs asserts.
*/
void WIR_Operation::checkVREGs( const WIR_Parameter &p ) const
{
  DSTART( "void WIR_Operation::checkVREGs(const WIR_Parameter&) const" );

  (void) p;

  #ifdef FAILSAFEMODE
  if ( isInserted() ) {
    WIR_Instruction &i = getInstruction();

    if ( i.isInserted() ) {
      WIR_BasicBlock &b = i.getBasicBlock();

      if ( b.isInserted() ) {
        WIR_Function &f = b.getFunction();

        if ( p.getType() == WIR_ParameterType::reg ) {
          auto &rp = dynamic_cast<const WIR_RegisterParameter &>( p );
          auto &r = rp.getRegister();

          if ( r.isVirtual() ) {
            auto &v = dynamic_cast<WIR_VirtualRegister &>( r );

            ufAssertT(
              !v.isInserted() || ( v.isInserted() && ( v.getFunction() == f ) ),
              "Illegal attempt to insert a parameter into an operation " <<
              "owned by function '" << f.getName() << "' (ID " << f.getID() <<
              ") where virtual register '" << v.getName() <<
              "' belongs to function '" << v.getFunction().getName() <<
              "' (ID " << v.getFunction().getID() << ")!" );
          }
        }
      }
    }
  }
  #endif
};


/*
  Dummy function for adding parameters which does nothing.

  It only serves to terminate the recursion of the variadic method
  addParameters.
*/
void WIR_Operation::addParameters( void ) const
{
  DSTART( "void WIR_Operation::addParameters() const" );
};

}       // namespace WIR
