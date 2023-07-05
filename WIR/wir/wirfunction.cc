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
  @file wirfunction.cc
  @brief This file implements %WIR functions.

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
#include <functional>
#include <list>
#include <map>
#include <stdlib.h>
#include <sstream>
#include <string>
#include <unistd.h>
#include <vector>

// Include boost headers
#include <boost/current_function.hpp>

// Include libuseful headers
#include <libuseful/debugmacros.h>
#include <libuseful/exceptions.h>
#include <libuseful/io.h>
#include <libuseful/stringtools.h>

// Include WIR headers
#include <wir/wir.h>


//
// Code section
//

namespace WIR {


using namespace std;


//
// Public class methods
//

/*
  Default constructor creating an empty function.

  This constructor asserts if it is passed an empty string.
*/
WIR_Function::WIR_Function( const std::string &s ) :
  WIR_ID_API {},
  WIR_Container_API {},
  mCompilationUnitPointer { nullptr },
  mDontOptimize { false },
  mCheckVREGs { true },
  mName { s },
  mFrameSize { 0 }
{
  DSTART( "WIR_Function::WIR_Function(const string&)" );

  ufAssert( !s.empty() );
};


/*
  Default constructor creating an empty function.

  This constructor asserts if it is passed an empty string.
*/
WIR_Function::WIR_Function( std::string &&s ) :
  WIR_ID_API {},
  WIR_Container_API {},
  mCompilationUnitPointer { nullptr },
  mDontOptimize { false },
  mCheckVREGs { true },
  mFrameSize { 0 }
{
  DSTART( "WIR_Function::WIR_Function(string&&)" );

  ufAssert( !s.empty() );
  mName = move( s );
  s.clear();
};


/*
  Copy constructor.

  When copying a function that is inserted in some WIR compilation unit, the
  resulting copy will not be inserted in a compilation unit. Copying a function
  implies that the newly created function and all its basic blocks,
  instructions, operations etc. are all set as getDontOptimize() == false.
*/
WIR_Function::WIR_Function( const WIR_Function &__o ) :
  WIR_ID_API { __o },
  WIR_Container_API { __o },
  mCompilationUnitPointer { nullptr },
  mCheckVREGs { __o.mCheckVREGs },
  mName { __o.mName },
  mFrameSize { __o.mFrameSize }
{
  DSTART( "WIR_Function::WIR_Function(const WIR_Function&)" );

  copyFunction( __o );
};


/*
  Move constructor.

  Trying to move a function that is inserted in some %WIR compilation unit
  results in an assertion, since you are not allowed to move a function whose
  ownership is managed by a compilation unit.
*/
WIR_Function::WIR_Function( WIR_Function &&__o ) :
  WIR_ID_API { move( __o ) },
  WIR_Container_API { move( __o ) },
  mBasicBlocks { move( __o.mBasicBlocks ) },
  mBasicBlockReferences { move( __o.mBasicBlockReferences ) },
  mVirtualRegisterPointers { move( __o.mVirtualRegisterPointers ) },
  mVirtualRegisterReferences { move( __o.mVirtualRegisterReferences ) },
  mCompilationUnitPointer { nullptr },
  mPrecolors { move( __o.mPrecolors ) },
  mInterferences { move( __o.mInterferences ) },
  mDontOptimize { __o.mDontOptimize },
  mCheckVREGs { __o.mCheckVREGs },
  mName { move( __o.mName ) },
  mFrameSize { move( __o.mFrameSize ) },
  mFunctionInputs { move( __o.mFunctionInputs ) }
{
  DSTART( "WIR_Function::WIR_Function(WIR_Function&&)" );

  ufAssertT(
    __o.mCompilationUnitPointer == nullptr,
    "Invalid attempt to move function '" << __o.getName() <<
    "' out of its owning compilation unit '" <<
    __o.getCompilationUnit().getName() << "'." );

  __o.mBasicBlocks.clear();
  __o.mBasicBlockReferences.clear();
  __o.mVirtualRegisterReferences.clear();
  __o.mVirtualRegisterPointers.clear();
  __o.mPrecolors.clear();
  __o.mInterferences.clear();
  __o.mName.clear();
  __o.mFrameSize = 0;
  __o.mFunctionInputs.clear();

  // Adjust the parent IDs of the function's basic blocks.
  for ( auto &b : mBasicBlocks )
    b.onInsert( this );

  // Adjust the parent IDs of the function's registers.
  for ( WIR_VirtualRegister &r : mVirtualRegisterReferences )
    r.onInsert( this );
};


/*
  Destructor.
*/
WIR_Function::~WIR_Function( void )
{
  DSTART( "virtual WIR_Function::~WIR_Function()" );

  clearBasicBlocks();
  clearPrecolors();
  clearVirtualRegisters();

  // If a function is inserted indrectly into some WIR system, we have to remove
  // the function from the system's symbol table.
  if ( isInserted() ) {
    WIR_CompilationUnit &c = getCompilationUnit();
    if ( c.isInserted() ) {
      WIR_System &s = c.getSystem();
      s.eraseSymbol( *this );
    }
  }
};


/*
  Copy-assignment operator.

  When copying a function that is inserted in some WIR compilation unit, the
  resulting copy will not be inserted in a compilation unit. Copying a function
  implies that the newly created function and all its basic blocks,
  instructions, operations etc. are all set as getDontOptimize() == false.
*/
WIR_Function & WIR_Function::operator = ( const WIR_Function &__o )
{
  DSTART( "WIR_Function& WIR_Function::operator=(const WIR_Function&)" );

  WIR_Container_API::operator = ( __o );

  mCompilationUnitPointer = nullptr;
  mCheckVREGs = __o.mCheckVREGs;
  mName = __o.mName;
  mFrameSize = __o.mFrameSize;

  copyFunction( __o );

  return( *this );
};


/*
  Move-assignment operator.

  Trying to move a function that is inserted in some %WIR compilation unit
  results in an assertion, since you are not allowed to move a function whose
  ownership is managed by a compilation unit.
*/
WIR_Function & WIR_Function::operator = ( WIR_Function &&__o )
{
  DSTART( "WIR_Function& WIR_Function::operator=(WIR_Function&&)" );

  ufAssertT(
    __o.mCompilationUnitPointer == nullptr,
    "Invalid attempt to move function '" << __o.getName() <<
    "' out of its owning compilation unit '" <<
    __o.getCompilationUnit().getName() << "'." );

  WIR_Container_API::operator = ( move( __o ) );

  mBasicBlocks = move( __o.mBasicBlocks );
  __o.mBasicBlocks.clear();
  mBasicBlockReferences = move( __o.mBasicBlockReferences );
  __o.mBasicBlockReferences.clear();
  mVirtualRegisterPointers = move( __o.mVirtualRegisterPointers );
  __o.mVirtualRegisterPointers.clear();
  mVirtualRegisterReferences = move( __o.mVirtualRegisterReferences );
  __o.mVirtualRegisterReferences.clear();
  mCompilationUnitPointer = nullptr;
  mPrecolors = move( __o.mPrecolors );
  __o.mPrecolors.clear();
  mInterferences = move( __o.mInterferences );
  __o.mInterferences.clear();
  mDontOptimize = __o.mDontOptimize;
  mCheckVREGs = __o.mCheckVREGs;
  mName = move( __o.mName );
  __o.mName.clear();
  mFrameSize = move( __o.mFrameSize );
  __o.mFrameSize = 0;
  mFunctionInputs = move( __o.mFunctionInputs );
  __o.mFunctionInputs.clear();

  // Adjust the parent IDs of the function's basic blocks.
  for ( auto &b : mBasicBlocks )
    b.onInsert( this );

  // Adjust the parent IDs of the function's registers.
  for ( WIR_VirtualRegister &r : mVirtualRegisterReferences )
    r.onInsert( this );

  return( *this );
};


//
// API implementations.
//

WIR_INSERTION_IMPL( WIR_CompilationUnit, CompilationUnit, WIR_Function );


WIR_BasicBlock & WIR_Function::pushBackBasicBlock( const WIR_BasicBlock &o )
{
  DSTART(
    "WIR_BasicBlock& WIR_Function::pushBackBasicBlock(const WIR_BasicBlock&)" );

  checkDontOptimize();
  checkVREGs( o );

  WIR_BasicBlock *prevBack =
    mBasicBlocks.empty() ? nullptr : &(mBasicBlocks.back());

  mBasicBlocks.push_back( o );
  mBasicBlocks.back().onInsert( this );
  mBasicBlockReferences.push_back( mBasicBlocks.back() );

  if ( prevBack != nullptr )
    prevBack->mDirectSuccessor = &(mBasicBlocks.back());
  mBasicBlocks.back().mDirectSuccessor = nullptr;

  insertSymbol( mBasicBlocks.back() );

  return( mBasicBlocks.back() );
};


WIR_BasicBlock & WIR_Function::pushBackBasicBlock( WIR_BasicBlock &&o )
{
  DSTART(
    "WIR_BasicBlock& WIR_Function::pushBackBasicBlock(WIR_BasicBlock&&)" );

  checkDontOptimize();
  checkVREGs( o );

  WIR_BasicBlock *prevBack =
    mBasicBlocks.empty() ? nullptr : &(mBasicBlocks.back());

  mBasicBlocks.emplace_back( move( o ) );
  mBasicBlocks.back().onInsert( this );
  mBasicBlockReferences.push_back( mBasicBlocks.back() );

  if ( prevBack != nullptr )
    prevBack->mDirectSuccessor = &(mBasicBlocks.back());
  mBasicBlocks.back().mDirectSuccessor = nullptr;

  insertSymbol( mBasicBlocks.back() );

  return( mBasicBlocks.back() );
};


WIR_BasicBlock & WIR_Function::pushFrontBasicBlock( const WIR_BasicBlock &o )
{
  DSTART(
    "WIR_BasicBlock& WIR_Function::pushFrontBasicBlock(const WIR_BasicBlock&)" );

  checkDontOptimize();
  checkVREGs( o );

  WIR_BasicBlock *prevFront =
    mBasicBlocks.empty() ? nullptr : &(mBasicBlocks.front() );

  mBasicBlocks.push_front( o );
  mBasicBlocks.front().onInsert( this );
  mBasicBlockReferences.push_front( mBasicBlocks.front() );

  mBasicBlocks.front().mDirectSuccessor = prevFront;

  insertSymbol( mBasicBlocks.front() );

  return( mBasicBlocks.front() );
};


WIR_BasicBlock & WIR_Function::pushFrontBasicBlock( WIR_BasicBlock &&o )
{
  DSTART(
    "WIR_BasicBlock& WIR_Function::pushFrontBasicBlock(WIR_BasicBlock&&)" );

  checkDontOptimize();
  checkVREGs( o );

  WIR_BasicBlock *prevFront =
    mBasicBlocks.empty() ? nullptr : &(mBasicBlocks.front() );

  mBasicBlocks.emplace_front( move( o ) );
  mBasicBlocks.front().onInsert( this );
  mBasicBlockReferences.push_front( mBasicBlocks.front() );

  mBasicBlocks.front().mDirectSuccessor = prevFront;

  insertSymbol( mBasicBlocks.front() );

  return( mBasicBlocks.front() );
};


std::list<std::reference_wrapper<WIR_BasicBlock>>::iterator WIR_Function::insertBasicBlock( std::list<std::reference_wrapper<WIR_BasicBlock>>::const_iterator pos,
                                                                                            const WIR_BasicBlock &o )
{
  DSTART(
    "list<reference_wrapper<WIR_BasicBlock> >::iterator WIR_Function::insertBasicBlock(list<reference_wrapper<WIR_BasicBlock> >::const_iterator, const WIR_BasicBlock&)" );

  checkDontOptimize();
  checkVREGs( o );

  auto it = mBasicBlocks.begin();
  auto rit = mBasicBlocks.rend();
  for ( auto itr = mBasicBlockReferences.begin(); itr != pos;
        ++itr, ++it, --rit ) ;
  WIR_BasicBlock *prevPred = ( rit == mBasicBlocks.rend() ) ? nullptr : &(*rit);
  auto it1 = mBasicBlocks.insert( it, o );
  (*it1).onInsert( this );

  if ( prevPred != nullptr )
    prevPred->mDirectSuccessor = &(*it1);
  it1->mDirectSuccessor = ( it != mBasicBlocks.end() ) ? &(*it) : nullptr;

  insertSymbol( *it1 );

  return( mBasicBlockReferences.insert( pos, *it1 ) );
};


std::list<std::reference_wrapper<WIR_BasicBlock>>::iterator WIR_Function::insertBasicBlock( std::list<std::reference_wrapper<WIR_BasicBlock>>::const_iterator pos,
                                                                                            WIR_BasicBlock &&o )
{
  DSTART(
    "list<reference_wrapper<WIR_BasicBlock> >::iterator WIR_Function::insertBasicBlock(list<reference_wrapper<WIR_BasicBlock> >::const_iterator, WIR_BasicBlock&&)" );

  checkDontOptimize();
  checkVREGs( o );

  auto it = mBasicBlocks.begin();
  auto rit = mBasicBlocks.rend();
  for ( auto itr = mBasicBlockReferences.begin(); itr != pos;
        ++itr, ++it, --rit ) ;
  WIR_BasicBlock *prevPred = ( rit == mBasicBlocks.rend() ) ? nullptr : &(*rit);
  auto it1 = mBasicBlocks.insert( it, move( o ) );
  (*it1).onInsert( this );

  if ( prevPred != nullptr )
    prevPred->mDirectSuccessor = &(*it1);
  it1->mDirectSuccessor = ( it != mBasicBlocks.end() ) ? &(*it) : nullptr;

  insertSymbol( *it1 );

  return( mBasicBlockReferences.insert( pos, *it1 ) );
};


std::list<std::reference_wrapper<WIR_BasicBlock>>::iterator WIR_Function::replaceBasicBlock( std::list<std::reference_wrapper<WIR_BasicBlock>>::const_iterator pos,
                                                                                             const WIR_BasicBlock &o )
{
  DSTART(
    "list<reference_wrapper<WIR_BasicBlock> >::iterator WIR_Function::replaceBasicBlock(list<reference_wrapper<WIR_BasicBlock> >::const_iterator, const WIR_BasicBlock&)" );

  checkDontOptimize();
  checkVREGs( o );

  auto it1 = mBasicBlocks.begin();
  auto rit = mBasicBlocks.rend();
  for ( auto itr = mBasicBlockReferences.begin(); itr != pos;
        ++itr, ++it1, --rit ) ;
  WIR_BasicBlock *prevPred = ( rit == mBasicBlocks.rend() ) ? nullptr : &(*rit);
  WIR_BasicBlock *prevSucc = pos->get().mDirectSuccessor;
  auto it2 = mBasicBlocks.insert( it1, o );
  (*it2).onInsert( this );

  if ( prevPred != nullptr )
    prevPred->mDirectSuccessor = &(*it2);
  it2->mDirectSuccessor = prevSucc;

  insertSymbol( *it2 );

  auto it = mBasicBlockReferences.insert( pos, *it2 );
  mBasicBlockReferences.erase( pos );
  mBasicBlocks.erase( it1 );

  return( it );
};


std::list<std::reference_wrapper<WIR_BasicBlock>>::iterator WIR_Function::replaceBasicBlock( std::list<std::reference_wrapper<WIR_BasicBlock>>::const_iterator pos,
                                                                                            WIR_BasicBlock &&o )
{
  DSTART(
    "list<reference_wrapper<WIR_BasicBlock> >::iterator WIR_Function::replaceBasicBlock(list<reference_wrapper<WIR_BasicBlock> >::const_iterator, WIR_BasicBlock&&)" );

  checkDontOptimize();
  checkVREGs( o );

  auto it1 = mBasicBlocks.begin();
  auto rit = mBasicBlocks.rend();
  for ( auto itr = mBasicBlockReferences.begin(); itr != pos;
        ++itr, ++it1, --rit ) ;
  WIR_BasicBlock *prevPred = ( rit == mBasicBlocks.rend() ) ? nullptr : &(*rit);
  WIR_BasicBlock *prevSucc = pos->get().mDirectSuccessor;
  auto it2 = mBasicBlocks.insert( it1, move( o ) );
  (*it2).onInsert( this );

  if ( prevPred != nullptr )
    prevPred->mDirectSuccessor = &(*it2);
  it2->mDirectSuccessor = prevSucc;

  insertSymbol( *it2 );

  auto it = mBasicBlockReferences.insert( pos, *it2 );
  mBasicBlockReferences.erase( pos );
  mBasicBlocks.erase( it1 );

  return( it );
};


void WIR_Function::popBackBasicBlock( void )
{
  DSTART( "void WIR_Function::popBackBasicBlock()" );

  checkDontOptimize();

  if ( mBasicBlockReferences.empty() )
    return;

  mBasicBlockReferences.pop_back();
  mBasicBlocks.pop_back();

  if ( !mBasicBlocks.empty() )
    mBasicBlocks.back().mDirectSuccessor = nullptr;
};


void WIR_Function::popFrontBasicBlock( void )
{
  DSTART( "void WIR_Function::popFrontBasicBlock()" );

  checkDontOptimize();

  if ( mBasicBlockReferences.empty() )
    return;

  mBasicBlockReferences.pop_front();
  mBasicBlocks.pop_front();
};


std::list<std::reference_wrapper<WIR_BasicBlock>>::iterator WIR_Function::eraseBasicBlock( std::list<std::reference_wrapper<WIR_BasicBlock>>::const_iterator pos )
{
  DSTART(
    "list<reference_wrapper<WIR_BasicBlock> >::iterator WIR_Function::eraseBasicBlock(list<reference_wrapper<WIR_BasicBlock> >::const_iterator)" );

  checkDontOptimize();

  auto it1 = mBasicBlocks.begin();
  auto rit = mBasicBlocks.rend();
  for ( auto itr = mBasicBlockReferences.begin(); itr != pos;
        ++itr, ++it1, --rit ) ;
  WIR_BasicBlock *prevPred = ( rit == mBasicBlocks.rend() ) ? nullptr : &(*rit);
  WIR_BasicBlock *prevSucc = pos->get().mDirectSuccessor;
  auto it = mBasicBlockReferences.erase( pos );
  mBasicBlocks.erase( it1 );

  if ( prevPred != nullptr )
    prevPred->mDirectSuccessor = prevSucc;

  return( it );
};


void WIR_Function::clearBasicBlocks( void )
{
  DSTART( "void WIR_Function::clearBasicBlocks()" );

  checkDontOptimize();

  mBasicBlockReferences.clear();
  mBasicBlocks.clear();
};


const std::list<std::reference_wrapper<WIR_BasicBlock>> &WIR_Function::getBasicBlocks( void ) const
{
  DSTART(
    "const list<reference_wrapper<WIR_BasicBlock> >& WIR_Function::getBasicBlocks() const" );

  return( mBasicBlockReferences );
};


/*
  begin returns an iterator to the first basic block of a function.
*/
std::list<std::reference_wrapper<WIR_BasicBlock>>::const_iterator WIR_Function::begin( void ) const
{
  DSTART(
    "list<reference_wrapper<WIR_BasicBlock> >::const_iterator WIR_Function::begin() const" );

  return( mBasicBlockReferences.begin() );
};


/*
  end returns an iterator to the end of a function's basic block list.
*/
std::list<std::reference_wrapper<WIR_BasicBlock>>::const_iterator WIR_Function::end( void ) const
{
  DSTART(
    "list<reference_wrapper<WIR_BasicBlock> >::const_iterator WIR_Function::end() const" );

  return( mBasicBlockReferences.end() );
};


/*
  rbegin returns an iterator to the reverse-first basic block of a function.
*/
std::list<std::reference_wrapper<WIR_BasicBlock>>::const_reverse_iterator WIR_Function::rbegin( void ) const
{
  DSTART(
    "list<reference_wrapper<WIR_BasicBlock> >::const_reverse_iterator WIR_Function::rbegin() const" );

  return( mBasicBlockReferences.rbegin() );
};


/*
  rend returns an iterator to the reverse-end of a function's basic block list.
*/
std::list<std::reference_wrapper<WIR_BasicBlock>>::const_reverse_iterator WIR_Function::rend( void ) const
{
  DSTART(
    "list<reference_wrapper<WIR_BasicBlock> >::const_reverse_iterator WIR_Function::rend() const" );

  return( mBasicBlockReferences.rend() );
};


bool WIR_Function::containsBasicBlock( WIR_id_t id ) const
{
  DSTART( "bool WIR_Function::containsBasicBlock(WIR_id_t) const" );

  for ( auto &item : mBasicBlocks )
    if ( item.getID() == id )
      return( true );

  return( false );
};


bool WIR_Function::containsBasicBlock( const WIR_BasicBlock &o ) const
{
  DSTART(
    "bool WIR_Function::containsBasicBlock(const WIR_BasicBlock&) const" );

  for ( auto &item : mBasicBlocks )
    if ( item == o )
      return( true );

  return( false );
};


std::list<std::reference_wrapper<WIR_BasicBlock>>::const_iterator WIR_Function::findBasicBlock( WIR_id_t id ) const
{
  DSTART(
    "list<reference_wrapper<WIR_BasicBlock> >::const_iterator WIR_Function::findBasicBlock(WIR_id_t) const" );

  for ( auto it = mBasicBlockReferences.begin();
        it != mBasicBlockReferences.end(); ++it )
    if ( (*it).get().getID() == id )
      return( it );

  return( mBasicBlockReferences.end() );
};


std::list<std::reference_wrapper<WIR_BasicBlock>>::const_iterator WIR_Function::findBasicBlock( const WIR_BasicBlock &o ) const
{
  DSTART(
    "list<reference_wrapper<WIR_BasicBlock> >::const_iterator WIR_Function::findBasicBlock(const WIR_BasicBlock&) const" );

  for ( auto it = mBasicBlockReferences.begin();
        it != mBasicBlockReferences.end(); ++it )
    if ( (*it).get() == o )
      return( it );

  return( mBasicBlockReferences.end() );
};


WIR_VirtualRegister & WIR_Function::pushBackVirtualRegister( const WIR_VirtualRegister &o )
{
  DSTART(
    "WIR_VirtualRegister& WIR_Function::pushBackVirtualRegister(const WIR_VirtualRegister&)" );

  checkDontOptimize();

  ufAssertT(
    !o.isChild(),
    "Illegal attempt to insert only a part of a hierarchical register into a" <<
    " function." );

  WIR_VirtualRegister *p = static_cast<WIR_VirtualRegister *>( o.clone() );
  mVirtualRegisterPointers.push_back( unique_ptr<WIR_VirtualRegister>( p ) );
  p->onInsert( this );
  mVirtualRegisterReferences.push_back( *p );

  return( mVirtualRegisterReferences.back().get() );
};


WIR_VirtualRegister & WIR_Function::pushBackVirtualRegister( WIR_VirtualRegister &&o )
{
  DSTART(
    "WIR_VirtualRegister& WIR_Function::pushBackVirtualRegister(WIR_VirtualRegister&&)" );

  checkDontOptimize();

  ufAssertT(
    !o.isChild(),
    "Illegal attempt to insert only a part of a hierarchical register into a" <<
    " function." );

  WIR_VirtualRegister *p = static_cast<WIR_VirtualRegister *>( o.clone() );
  mVirtualRegisterPointers.push_back( unique_ptr<WIR_VirtualRegister>( p ) );
  p->onInsert( this );
  mVirtualRegisterReferences.push_back( *p );

  return( mVirtualRegisterReferences.back().get() );
};


/*
  pushBackVirtualRegister adds a new WIR_VirtualRegister at the end of lists
  mVirtualRegisterPointers and mVirtualRegisterReferences, after its current
  last element.

  Class WIR_Function takes over full control over the ownership of the given
  pointer! In particular, WIR_Function automatically destroys the object pointed
  to. Users of this variant of pushBackVirtualRegister are strongly discouraged
  of continuing to use this pointer afterwards. This variant of
  pushBackVirtualRegister is more efficient than the previous ones since it
  completely avoids any (polymorphic) copy operations. Thus, it should only be
  used if large amounts of virtual registers shall be created/added highly
  efficiently as, e.g., in a compiler's code selector.
*/
WIR_VirtualRegister & WIR_Function::pushBackVirtualRegister( WIR_VirtualRegister *r )
{
  DSTART(
    "WIR_VirtualRegister& WIR_Function::pushBackVirtualRegister(WIR_VirtualRegister*)" );

  checkDontOptimize();

  ufAssertT(
    !r->isChild(),
    "Illegal attempt to insert only a part of a hierarchical register into a" <<
    " function." );

  mVirtualRegisterPointers.push_back( unique_ptr<WIR_VirtualRegister>( r ) );
  r->onInsert( this );
  mVirtualRegisterReferences.push_back( *r );

  return( mVirtualRegisterReferences.back().get() );
};


WIR_VirtualRegister & WIR_Function::pushFrontVirtualRegister( const WIR_VirtualRegister &o )
{
  DSTART(
    "WIR_VirtualRegister& WIR_Function::pushFrontVirtualRegister(const WIR_VirtualRegister&)" );

  checkDontOptimize();

  ufAssertT(
    !o.isChild(),
    "Illegal attempt to insert only a part of a hierarchical register into a" <<
    " function." );

  WIR_VirtualRegister *p = static_cast<WIR_VirtualRegister *>( o.clone() );
  mVirtualRegisterPointers.push_front( unique_ptr<WIR_VirtualRegister>( p ) );
  p->onInsert( this );
  mVirtualRegisterReferences.push_front( *p );

  return( mVirtualRegisterReferences.front().get() );
};


WIR_VirtualRegister & WIR_Function::pushFrontVirtualRegister( WIR_VirtualRegister &&o )
{
  DSTART(
    "WIR_VirtualRegister& WIR_Function::pushFrontVirtualRegister(WIR_VirtualRegister&&)" );

  checkDontOptimize();

  ufAssertT(
    !o.isChild(),
    "Illegal attempt to insert only a part of a hierarchical register into a" <<
    " function." );

  WIR_VirtualRegister *p = static_cast<WIR_VirtualRegister *>( o.clone() );
  mVirtualRegisterPointers.push_front( unique_ptr<WIR_VirtualRegister>( p ) );
  p->onInsert( this );
  mVirtualRegisterReferences.push_front( *p );

  return( mVirtualRegisterReferences.front().get() );
};


std::list<std::reference_wrapper<WIR_VirtualRegister>>::iterator WIR_Function::insertVirtualRegister( std::list<std::reference_wrapper<WIR_VirtualRegister>>::const_iterator pos,
                                                                                                      const WIR_VirtualRegister &o )
{
  DSTART(
    "list<reference_wrapper<WIR_VirtualRegister> >::iterator WIR_Function::insertVirtualRegister(list<reference_wrapper<WIR_VirtualRegister> >::const_iterator, const WIR_VirtualRegister&)" );

  checkDontOptimize();

  ufAssertT(
    !o.isChild(),
    "Illegal attempt to insert only a part of a hierarchical register into a" <<
    " function." );

  auto itp = mVirtualRegisterPointers.begin();
  for ( auto itr = mVirtualRegisterReferences.begin(); itr != pos;
        ++itr, ++itp ) ;
  WIR_VirtualRegister *p = static_cast<WIR_VirtualRegister *>( o.clone() );
  mVirtualRegisterPointers.insert( itp, unique_ptr<WIR_VirtualRegister>( p ) );
  p->onInsert( this );
  return( mVirtualRegisterReferences.insert( pos, *p ) );
};


std::list<std::reference_wrapper<WIR_VirtualRegister>>::iterator WIR_Function::insertVirtualRegister( std::list<std::reference_wrapper<WIR_VirtualRegister>>::const_iterator pos,
                                                                                                      WIR_VirtualRegister &&o )
{
  DSTART(
    "list<reference_wrapper<WIR_VirtualRegister> >::iterator WIR_Function::insertVirtualRegister(list<reference_wrapper<WIR_VirtualRegister> >::const_iterator, WIR_VirtualRegister&&)" );

  checkDontOptimize();

  ufAssertT(
    !o.isChild(),
    "Illegal attempt to insert only a part of a hierarchical register into a" <<
    " function." );

  auto itp = mVirtualRegisterPointers.begin();
  for ( auto itr = mVirtualRegisterReferences.begin(); itr != pos;
        ++itr, ++itp ) ;
  WIR_VirtualRegister *p = static_cast<WIR_VirtualRegister *>( o.clone() );
  mVirtualRegisterPointers.insert( itp, unique_ptr<WIR_VirtualRegister>( p ) );
  p->onInsert( this );
  return( mVirtualRegisterReferences.insert( pos, *p ) );
};


std::list<std::reference_wrapper<WIR_VirtualRegister>>::iterator WIR_Function::replaceVirtualRegister( std::list<std::reference_wrapper<WIR_VirtualRegister>>::const_iterator pos,
                                                                                                       const WIR_VirtualRegister &o )
{
  DSTART(
    "list<reference_wrapper<WIR_VirtualRegister> >::iterator WIR_Function::replaceVirtualRegister(list<reference_wrapper<WIR_VirtualRegister> >::const_iterator, const WIR_VirtualRegister&)" );

  checkDontOptimize();

  ufAssertT(
    !o.isChild(),
    "Illegal attempt to insert only a part of a hierarchical register into a" <<
    " function." );

  auto itp = mVirtualRegisterPointers.begin();
  for ( auto itr = mVirtualRegisterReferences.begin(); itr != pos;
        ++itr, ++itp ) ;
  WIR_VirtualRegister *p = static_cast<WIR_VirtualRegister *>( o.clone() );
  mVirtualRegisterPointers.insert( itp, unique_ptr<WIR_VirtualRegister>( p ) );
  p->onInsert( this );
  auto it = mVirtualRegisterReferences.insert( pos, *p );
  erasePrecolor( (*pos).get() );
  mVirtualRegisterReferences.erase( pos );
  mVirtualRegisterPointers.erase( itp );
  return( it );
};


std::list<std::reference_wrapper<WIR_VirtualRegister>>::iterator WIR_Function::replaceVirtualRegister( std::list<std::reference_wrapper<WIR_VirtualRegister>>::const_iterator pos,
                                                                                                       WIR_VirtualRegister &&o )
{
  DSTART(
    "list<reference_wrapper<WIR_VirtualRegister> >::iterator WIR_Function::replaceVirtualRegister(list<reference_wrapper<WIR_VirtualRegister> >::const_iterator, WIR_VirtualRegister&&)" );

  checkDontOptimize();

  ufAssertT(
    !o.isChild(),
    "Illegal attempt to insert only a part of a hierarchical register into a" <<
    " function." );

  auto itp = mVirtualRegisterPointers.begin();
  for ( auto itr = mVirtualRegisterReferences.begin(); itr != pos;
        ++itr, ++itp ) ;
  WIR_VirtualRegister *p = static_cast<WIR_VirtualRegister *>( o.clone() );
  mVirtualRegisterPointers.insert( itp, unique_ptr<WIR_VirtualRegister>( p ) );
  p->onInsert( this );
  auto it = mVirtualRegisterReferences.insert( pos, *p );
  erasePrecolor( (*pos).get() );
  mVirtualRegisterReferences.erase( pos );
  mVirtualRegisterPointers.erase( itp );
  return( it );
};


void WIR_Function::popBackVirtualRegister( void )
{
  DSTART( "void WIR_Function::popBackVirtualRegister()" );

  checkDontOptimize();

  if ( mVirtualRegisterReferences.empty() )
    return;

  erasePrecolor( mVirtualRegisterReferences.back().get() );
  mVirtualRegisterReferences.pop_back();
  mVirtualRegisterPointers.pop_back();
};


void WIR_Function::popFrontVirtualRegister( void )
{
  DSTART( "void WIR_Function::popFrontVirtualRegister()" );

  checkDontOptimize();

  if ( mVirtualRegisterReferences.empty() )
    return;

  erasePrecolor( mVirtualRegisterReferences.front().get() );
  mVirtualRegisterReferences.pop_front();
  mVirtualRegisterPointers.pop_front();
};


std::list<std::reference_wrapper<WIR_VirtualRegister>>::iterator WIR_Function::eraseVirtualRegister( std::list<std::reference_wrapper<WIR_VirtualRegister>>::const_iterator pos )
{
  DSTART(
    "list<reference_wrapper<WIR_VirtualRegister> >::iterator WIR_Function::eraseVirtualRegister(list<reference_wrapper<WIR_VirtualRegister> >::const_iterator)" );

  checkDontOptimize();

  auto itp = mVirtualRegisterPointers.begin();
  for ( auto itr = mVirtualRegisterReferences.begin(); itr != pos;
        ++itr, ++itp ) ;
  erasePrecolor( (*pos).get() );
  auto it = mVirtualRegisterReferences.erase( pos );
  mVirtualRegisterPointers.erase( itp );
  return( it );
};


void WIR_Function::clearVirtualRegisters( void )
{
  DSTART( "void WIR_Function::clearVirtualRegisters()" );

  checkDontOptimize();

  clearPrecolors();
  mVirtualRegisterReferences.clear();
  mVirtualRegisterPointers.clear();
};


const std::list<std::reference_wrapper<WIR_VirtualRegister>> &WIR_Function::getVirtualRegisters( void ) const
{
  DSTART(
    "const list<reference_wrapper<WIR_VirtualRegister> >& WIR_Function::getVirtualRegisters() const" );

  return( mVirtualRegisterReferences );
};


bool WIR_Function::containsVirtualRegister( WIR_id_t id ) const
{
  DSTART( "bool WIR_Function::containsVirtualRegister(WIR_id_t) const" );

  for ( WIR_VirtualRegister &item : mVirtualRegisterReferences )
    if ( item.getID() == id )
      return( true );

  return( false );
};


bool WIR_Function::containsVirtualRegister( const WIR_VirtualRegister &o ) const
{
  DSTART(
    "bool WIR_Function::containsVirtualRegister(const WIR_VirtualRegister&) const" );

  for ( WIR_VirtualRegister &item : mVirtualRegisterReferences )
    if ( item == o )
      return( true );

  return( false );
};


std::list<std::reference_wrapper<WIR_VirtualRegister>>::const_iterator WIR_Function::findVirtualRegister( WIR_id_t id ) const
{
  DSTART(
    "list<reference_wrapper<WIR_VirtualRegister> >::const_iterator WIR_Function::findVirtualRegister(WIR_id_t) const" );

  for ( auto it = mVirtualRegisterReferences.begin();
        it != mVirtualRegisterReferences.end(); ++it )
    if ( (*it).get().getID() == id )
      return( it );

  return( mVirtualRegisterReferences.end() );
};


std::list<std::reference_wrapper<WIR_VirtualRegister>>::const_iterator WIR_Function::findVirtualRegister( const WIR_VirtualRegister &o ) const
{
  DSTART(
    "list<reference_wrapper<WIR_VirtualRegister> >::const_iterator WIR_Function::findVirtualRegister(const WIR_VirtualRegister&) const" );

  for ( auto it = mVirtualRegisterReferences.begin();
        it != mVirtualRegisterReferences.end(); ++it )
    if ( (*it).get() == o )
      return( it );

  return( mVirtualRegisterReferences.end() );
};


/*
  setName sets a function's specific name.

  setName asserts if it is passed an empty string.
*/
void WIR_Function::setName( const std::string &s )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  ufAssert( !s.empty() );

  mName = s;
};


/*
  setName sets a function's specific name.

  setName asserts if it is passed an empty string.
*/
void WIR_Function::setName( std::string &&s )
{
  DSTART( "void WIR_Function::setName(string&&)" );

  ufAssert( !s.empty() );

  mName = move( s );
  s.clear();
};


/*
  getName returns a function's specific name.
*/
std::string WIR_Function::getName( void ) const
{
  DSTART( "string WIR_Function::getName() const" );

  return( mName );
};


/*
  getSize returns a function's code size in bytes.
*/
unsigned long long WIR_Function::getSize( void ) const
{
  DSTART( "long long unsigned int WIR_Function::getSize() const" );

  unsigned long long res = 0;

  for ( WIR_BasicBlock &b : getBasicBlocks() )
    res += b.getSize();

  return( res );
};


/*
  insertPrecolor precolors the specified virtual register with a given physical
  register for register allocation.

  If v is part of a register hierarchy, the entire hierarchy of virtual
  registers is precolored with their corresponding hierarchical physical
  registers. If insertPrecolor is applied to a virtual register that is already
  precolored, the new precolor information silently overrides the old one.

  insertPrecolor asserts if it is called with a virtual register that does not
  belong to this WIR function. It furthermore asserts if both the virtual and
  physical register have different register types, i.e., if it is attempted to
  precolor a TriCore virtual register with an ARM's physical register. It
  finally asserts if the two registers that shall be precolored have already
  been marked as interfering before.
*/
void WIR_Function::insertPrecolor( const WIR_VirtualRegister &v,
                                   const WIR_PhysicalRegister &p )
{
  DSTART(
    "void WIR_Function::insertPrecolor(const WIR_VirtualRegister&, const WIR_PhysicalRegister&)" );

  #ifdef FAILSAFEMODE
  ufAssert( containsVirtualRegister( v.getRoot() ) );
  ufAssert( v.getType() == p.getType() );
  #endif
  ufAssert( !interfere( v, p ) );

  // Propagate the precoloring relationship between v and p through the entire
  // register hierarchies of v and p, if any.
  // Get the common roots of the register hierarchies of v and p.
  WIR_VirtualRegister *vRoot = &( const_cast<WIR_VirtualRegister &>( v ) );
  WIR_PhysicalRegister *pRoot = &( const_cast<WIR_PhysicalRegister &>( p ) );
  while ( ( vRoot->getParent() != *vRoot ) &&
          ( pRoot->getParent() != *pRoot ) ) {
    vRoot = &(vRoot->getParent());
    pRoot = &(pRoot->getParent());
  }

  // Propagate precolors through hierarchies rooted by vRoot and pRoot using a
  // worklist algorithm.
  list<WIR_VirtualRegister *> vWorklist;
  list<WIR_PhysicalRegister *> pWorklist;

  vWorklist.push_back( vRoot );
  pWorklist.push_back( pRoot );

  do {
    WIR_VirtualRegister &vReg = *(vWorklist.front());
    vWorklist.pop_front();
    WIR_PhysicalRegister &pReg = *(pWorklist.front());
    pWorklist.pop_front();

    mPrecolors[ vReg ] = &pReg;

    for ( WIR_VirtualRegister &c : vReg )
      vWorklist.push_back( &c );
    for ( WIR_PhysicalRegister &c : pReg )
      pWorklist.push_back( &c );
  } while ( !vWorklist.empty() );
};


/*
  erasePrecolor removes any precoloring information related to the specified
  virtual register.

  If the given virtual register is not precolored at all or if it does not
  belong to this WIR function, erasePrecolor silently ignores these issues.
*/
void WIR_Function::erasePrecolor( const WIR_VirtualRegister &v )
{
  DSTART( "void WIR_Function::erasePrecolor(const WIR_VirtualRegister&)" );

  // Erase the precolor information in the entire register hierarchy of v.
  list<WIR_VirtualRegister *> vWorklist;
  vWorklist.push_back( &(v.getRoot()) );

  do {
    WIR_VirtualRegister &vReg = *(vWorklist.front());
    vWorklist.pop_front();

    mPrecolors.erase( vReg );

    for ( WIR_VirtualRegister &c : vReg )
      vWorklist.push_back( &c );
  } while ( !vWorklist.empty() );
};


/*
  clearPrecolors removes all precoloring information from this WIR function.
*/
void WIR_Function::clearPrecolors( void )
{
  DSTART( "void WIR_Function::clearPrecolors()" );

  mPrecolors.clear();
};


/*
  containsPrecolor returns whether the specified virtual register is precolored
  or not.
*/
bool WIR_Function::containsPrecolor( const WIR_VirtualRegister &v ) const
{
  DSTART(
    "bool WIR_Function::containsPrecolor(const WIR_VirtualRegister&) const" );

  return( mPrecolors.count( const_cast<WIR_VirtualRegister &>( v ) ) != 0 );
};


/*
  findPrecolor returns the physical register associated with the specified
  virtual register for register allocation.

  findPrecolor asserts if the specified virtual register does not belong to this
  WIR function or if it is not precolored.
*/
WIR_PhysicalRegister &WIR_Function::findPrecolor( const WIR_VirtualRegister &v ) const
{
  DSTART(
    "WIR_PhysicalRegister& WIR_Function::findPrecolor(const WIR_VirtualRegister&) const" );

  #ifdef FAILSAFEMODE
  ufAssert( containsPrecolor( v ) );
  #endif

  return( *(mPrecolors.at( const_cast<WIR_VirtualRegister &>( v ) )) );
};


/*
  insertInterference marks the two specified registers as interfering for
  register allocation.

  If v is part of a register hierarchy, interferences are inserted for the
  entire hierarchies of both argument registers.

  insertInterference asserts if it is called with a virtual register that does
  not belong to this WIR function. It furthermore asserts if both registers have
  different register types, i.e., if it is attempted to let a data register
  interfere with an address register. It finally asserts if the two registers
  that shall interfere have already been precolored with each other.
*/
void WIR_Function::insertInterference( const WIR_VirtualRegister &v,
                                       const WIR_PhysicalRegister &p )
{
  DSTART(
    "void WIR_Function::insertInterference(const WIR_VirtualRegister&, const WIR_PhysicalRegister&)" );

  #ifdef FAILSAFEMODE
  ufAssert( containsVirtualRegister( v.getRoot() ) );
  ufAssert( v.getType() == p.getType() );
  #endif
  ufAssert( !containsPrecolor( v ) || ( findPrecolor( v ) != p ) );

  // Propagate the interference information between v and p through the entire
  // register hierarchies of v and p, if any.
  // Get the common roots of the register hierarchies of v and p.
  WIR_VirtualRegister *vRoot = &( const_cast<WIR_VirtualRegister &>( v ) );
  WIR_PhysicalRegister *pRoot = &( const_cast<WIR_PhysicalRegister &>( p ) );
  while ( ( vRoot->getParent() != *vRoot ) &&
          ( pRoot->getParent() != *pRoot ) ) {
    vRoot = &(vRoot->getParent());
    pRoot = &(pRoot->getParent());
  }

  // Propagate interference through hierarchies rooted by vRoot and pRoot using
  // a worklist algorithm.
  list<WIR_VirtualRegister *> vWorklist;
  list<WIR_PhysicalRegister *> pWorklist;

  vWorklist.push_back( vRoot );
  pWorklist.push_back( pRoot );

  do {
    WIR_VirtualRegister &vReg = *(vWorklist.front());
    vWorklist.pop_front();
    WIR_PhysicalRegister &pReg = *(pWorklist.front());
    pWorklist.pop_front();

    auto &phRegs = mInterferences[ vReg.getID() ];
    phRegs.insert( pReg );

    for ( WIR_VirtualRegister &c : vReg )
      vWorklist.push_back( &c );
    for ( WIR_PhysicalRegister &c : pReg )
      pWorklist.push_back( &c );
  } while ( !vWorklist.empty() );
};


/*
  eraseInterference erases an interference between the two specified registers.

  If the two given registers do not interfere or if the specified virtual
  register does not belong to this WIR function, eraseInterference silently
  ignores these issues.
*/
void WIR_Function::eraseInterference( const WIR_VirtualRegister &v,
                                      const WIR_PhysicalRegister &p )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  // Erase the interference information between v and p for the entire register
  // hierarchies of v and p, if any.
  // Get the common roots of the register hierarchies of v and p.
  WIR_VirtualRegister *vRoot = &(const_cast<WIR_VirtualRegister &>( v ) );
  WIR_PhysicalRegister *pRoot = &(const_cast<WIR_PhysicalRegister &>( p ));
  while ( ( vRoot->getParent() != *vRoot ) &&
          ( pRoot->getParent() != *pRoot ) ) {
    vRoot = &(vRoot->getParent());
    pRoot = &(pRoot->getParent());
  }

  // Propagate interference through hierarchies rooted by vRoot and pRoot using
  // a worklist algorithm.
  list<WIR_VirtualRegister *> vWorklist;
  list<WIR_PhysicalRegister *> pWorklist;

  vWorklist.push_back( vRoot );
  pWorklist.push_back( pRoot );

  do {
    WIR_VirtualRegister &vReg = *(vWorklist.front());
    vWorklist.pop_front();
    WIR_PhysicalRegister &pReg = *(pWorklist.front());
    pWorklist.pop_front();

    auto it = mInterferences.find( vReg.getID() );
    if ( it != mInterferences.end() ) {
      auto &phRegs = it->second;
      phRegs.erase( pReg );
      if ( phRegs.empty() )
        mInterferences.erase( it );
    }

    for ( WIR_VirtualRegister &c : vReg )
      vWorklist.push_back( &c );
    for ( WIR_PhysicalRegister &c : pReg )
      pWorklist.push_back( &c );
  } while ( !vWorklist.empty() );
};


/*
  clearInterferences removes all interference information from this WIR
  function.
*/
void WIR_Function::clearInterferences( void )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  mInterferences.clear();
};


/*
  interfere returns whether the two specified registers interfere.
*/
bool WIR_Function::interfere( const WIR_VirtualRegister &v,
                              const WIR_PhysicalRegister &p ) const
{
  DSTART(
    "bool WIR_Function::interfere(const WIR_VirtualRegister&, const WIR_PhysicalRegister&) const" );

  auto it = mInterferences.find( v.getID() );
  if ( it != mInterferences.end() )
    return( it->second.count( const_cast<WIR_PhysicalRegister &>( p ) ) != 0 );
  else
    return( false );
};


/*
  findInterferences returns the set of all physical registers that a virtual
  register of this WIR function interferes with.
*/
WIR_PhysicalRegisterSet WIR_Function::findInterferences( const WIR_VirtualRegister &v ) const
{
  DSTART(
    "WIR_PhysicalRegisterSet WIR_Function::findInterferences(const WIR_VirtualRegister&) const" );

  WIR_PhysicalRegisterSet res;

  auto it = mInterferences.find( v.getID() );
  if ( it != mInterferences.end() ) {
    auto &phregs = it->second;
    for ( WIR_PhysicalRegister &preg : phregs )
      res.insert( preg );
  }

  return( res );
};


/*
  getFrameSize sets a function's processor-specific stack frame size.
*/
void WIR_Function::setFrameSize( int f )
{
  DSTART( "void WIR_Function::setFrameSize(int)" );

  mFrameSize = f;
};


/*
  getFrameSize returns a function's processor-specific stack frame size.
*/
int WIR_Function::getFrameSize( void ) const
{
  DSTART( "int WIR_Function::getFrameSize() const" );

  return( mFrameSize );
};


/*
  addFunctionInput adds a physical register used for argument passing to the set
  of function inputs.
*/
void WIR_Function::addFunctionInput( const WIR_PhysicalRegister &p )
{
  DSTART( "void WIR_Function::addFunctionInput(const WIR_PhysicalRegister&)" );

  mFunctionInputs.insert( const_cast<WIR_PhysicalRegister &>( p ) );
};


/*
  getFunctionInputs returns the set of physical registers providing external
  inputs to a function.
*/
const WIR_PhysicalRegisterSet &WIR_Function::getFunctionInputs( void ) const
{
  DSTART(
    "const WIR_PhysicalRegisterSet& WIR_Function::getFunctionInputs() const" );

  return( mFunctionInputs );
};


/*
  getVREGs determines all virtual registers that occur in this function's basic
  blocks.
*/
WIR_VirtualRegisterSet WIR_Function::getVREGs( void ) const
{
  DSTART( "WIR_VirtualRegisterSet WIR_Function::getVREGs() const" );

  WIR_VirtualRegisterSet res;

  for ( WIR_BasicBlock &b : getBasicBlocks() )
    for ( WIR_VirtualRegister &r : b.getVREGs() )
      res.insert( r );

  return( res );
};


/*
  setDontOptimize sets whether a function can be modified or must not be changed
  by some optimization or transformation.
*/
void WIR_Function::setDontOptimize( bool f )
{
  DSTART( "void WIR_Function::setDontOptimize(bool)" );

  mDontOptimize = f;
};


/*
  getDontOptimize returns whether a function can be modified or must not be
  changed by some optimization or transformation.

  A function must not be modified if the function by itself has been marked as
  such using setDontOptimize, or if it is inserted into a WIR compilation unit
  that in turn must not be modified.
*/
bool WIR_Function::getDontOptimize( void ) const
{
  DSTART( "bool WIR_Function::getDontOptimize() const" );

  return(
    mDontOptimize ||
    ( isInserted() && getCompilationUnit().getDontOptimize() ) );
};


/*
  The << operator dumps a WIR function to an output stream.

  By applying processor-specific I/O manipulators to the output stream
  beforehand, this << operator can flexibly emit valid assembly output for
  arbitrary processor architectures.
*/
std::ostream & operator << ( std::ostream &os, const WIR_Function &f )
{
  DSTART( "ostream& operator<<(ostream&, const WIR_Function&)" );

  WIR_Registry::getFunctionDumper( os.iword( WIR_ProcessorIO() ) )( os, f );

  return( os );
};


//
// Private class methods
//

/*
  Dummy function for adding basic blocks which does nothing.

  It only serves to terminate the recursion of the variadic method
  addBasicBlocks.
*/
void WIR_Function::addBasicBlocks( void ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  copyFunction performs actions common to the copy constructor and copy
  assignment operator of WIR functions.
*/
void WIR_Function::copyFunction( const WIR_Function &__o )
{
  DSTART( "void WIR_Function::copyFunction(const WIR_Function&)" );

  mDontOptimize = false;

  // Copy registers.
  // While copying registers, we maintain a map mapping the
  // unique IDs of registers from __o to the IDs of the newly created copied
  // registers.
  // Furthermore, create the copies of the registers in an order that matches
  // the order of the original registers' IDs.

  // Sort the IDs of registers from __o.
  vector<WIR_id_t> originalIDs;
  for ( WIR_VirtualRegister &r : __o.mVirtualRegisterReferences )
    originalIDs.push_back( r.getID() );
  sort( originalIDs.begin(), originalIDs.end() );

  // Copy and insert registers according to the sorted order.
  clearVirtualRegisters();
  map<WIR_id_t, WIR_VirtualRegister *> registerIDMap;
  for ( auto id : originalIDs ) {
    auto it = mVirtualRegisterReferences.begin();
    auto it1 = __o.mVirtualRegisterReferences.begin();

    while ( (*it1).get().getID() != id ) {
      if ( (*it1).get().getID() < id )
        ++it;
      ++it1;
    }

    it = insertVirtualRegister( it, (*it1).get() );

    // Put the complete register hierarchy into registerIDMap.
    list<WIR_VirtualRegister *> origRegs;
    list<WIR_VirtualRegister *> newRegs;
    origRegs.push_back( &(it1->get()) );
    newRegs.push_back( &(it->get()) );

    do {
      WIR_VirtualRegister *origReg = origRegs.front();
      WIR_VirtualRegister *newReg = newRegs.front();
      origRegs.pop_front();
      newRegs.pop_front();

      registerIDMap[ origReg->getID() ] = newReg;

      for ( WIR_VirtualRegister &c : *origReg )
        origRegs.push_back( &c );
      for ( WIR_VirtualRegister &c : *newReg )
        newRegs.push_back( &c );
    } while ( !origRegs.empty() );
  }

  // Copy precoloring information.
  mPrecolors.clear();
  for ( auto p : __o.mPrecolors ) {
    WIR_id_t vregID = p.first.get().getID();
    WIR_PhysicalRegister *phreg = p.second;

    // Check if the VREG from __o's precolor has been copied before.
    auto it = registerIDMap.find( vregID );
    if ( it != registerIDMap.end() )
      mPrecolors[ *(it->second) ] = phreg;
  }

  // Copy interference information.
  mInterferences.clear();
  for ( auto p : __o.mInterferences ) {
    WIR_id_t vregID = p.first;
    WIR_PhysicalRegisterSet &phregs = p.second;

    // Check if the VREG from __o's precolor has been copied before.
    auto it = registerIDMap.find( vregID );
    if ( it != registerIDMap.end() )
      mInterferences.insert( make_pair( it->second->getID(), phregs ) );
  }

  // Copy basic blocks.

  // Sort the IDs of basic blocks from __o.
  originalIDs.clear();
  map<WIR_id_t, WIR_BasicBlock *> oldBlockIDMap;
  for ( WIR_BasicBlock &b : __o.mBasicBlockReferences ) {
    originalIDs.push_back( b.getID() );
    oldBlockIDMap[ b.getID() ] = &b;
  }
  sort( originalIDs.begin(), originalIDs.end() );

  // Copy basic blocks according to the sorted order.
  map<WIR_id_t, WIR_BasicBlock *> blockIDMap;
  list<WIR_BasicBlock> newBlocks;
  for ( auto id : originalIDs ) {
    newBlocks.push_back( *(oldBlockIDMap[ id ]) );
    blockIDMap[ id ] = &(newBlocks.back());
  }

  // Insert copied basic blocks into this function in the right order.
  clearBasicBlocks();
  mCheckVREGs = false;
  for ( WIR_BasicBlock &b : __o.mBasicBlockReferences ) {
    pushBackBasicBlock( move( *(blockIDMap[ b.getID() ]) ) );
    blockIDMap[ b.getID() ] = &(mBasicBlocks.back());
  }
  mCheckVREGs = true;

  // Patch all virtual registers within the copied basic blocks such that only
  // virtual registers of the newly copied function are used, and not the
  // virtual registers of the original function __o.
  for ( WIR_BasicBlock &b : mBasicBlockReferences ) {
    b.setDontOptimize( false );

    for ( WIR_Instruction &i : b ) {
      i.setDontOptimize( false );

      for ( WIR_Operation &o : i ) {
        o.setDontOptimize( false );

        for ( auto it = o.getParameters().begin();
              it != o.getParameters().end(); ++it ) {
          auto &p = (*it).get();
          p.setDontOptimize( false );

          if ( p.getType() == WIR_ParameterType::reg ) {
            auto &regP = dynamic_cast<WIR_RegisterParameter &>( p );
            auto &r = regP.getRegister();

            if ( r.isVirtual() ) {
              auto &vr = dynamic_cast<WIR_VirtualRegister &>( r );

              // Ensure that vr actually belongs to __o.
              ufAssertT(
                vr.isInserted() && vr.getFunction() == __o,
                "Illegal attempt to copy function '" << __o.getName() <<
                "' with dangling register parameter '" << vr.getName() <<
                "'." );

              // Determine copied instance of vr.
              auto &newvr = *(registerIDMap[ vr.getID() ]);

              // Replace register parameter p within o.
              it = o.replaceParameter(
                it, WIR_RegisterParameter( newvr, regP.getUsage() ) );
            }
          }
        }
      }
    }
  }

  // Patch all label parameters and explicitly specified jump targets of copied
  // operations.
  // Scan all jump targets attached to the copied operations and replace them
  // by the copied jump targets.
  for ( WIR_BasicBlock &b : getBasicBlocks() )
    for ( WIR_Instruction &i : b )
      for ( WIR_Operation &o : i ) {
        if ( o.jumpTargetsAdded() ) {
          for ( WIR_BasicBlock &target : o.getJumpTargets() )
            if ( blockIDMap.count( target.getID() ) ) {
              o.eraseJumpTarget( target );
              o.addJumpTarget( *(blockIDMap[ target.getID() ]) );
            }
        }

        for ( auto it = o.getParameters().begin();
              it != o.getParameters().end(); ++it ) {
          auto &p = (*it).get();

          if ( p.getType() == WIR_ParameterType::label ) {
            auto &labP = dynamic_cast<WIR_LabelParameter &>( p );

            if ( labP.getLabelType() == WIR_SymbolType::block ) {
              WIR_BasicBlock &labelB = labP.getBasicBlock();

              if ( blockIDMap.count( labelB.getID() ) )
                // Replace basic block label.
                it =
                  o.replaceParameter(
                    it,
                    WIR_LabelParameter( *(blockIDMap[ labelB.getID()]) ) );
            } else

            if ( labP.getLabelType() == WIR_SymbolType::function ) {
              WIR_Function &labelF = labP.getFunction();

              if ( labelF == __o )
                // Replace function label.
                it = o.replaceParameter( it, WIR_LabelParameter( *this ) );
            }
          }
        }
      }

  // Copy function inputs.
  mFunctionInputs = __o.mFunctionInputs;
};


/*
  checkDontOptimize checks whether a function must not be modified.

  If this function must not be modified, checkDontOptimize asserts.
*/
void WIR_Function::checkDontOptimize( void ) const
{
  DSTART( "void WIR_Function::checkDontOptimize() const" );

  ufAssertT(
    !getDontOptimize(),
    "Illegal attempt to modify a function that is set as 'don't optimize'!" );
};


/*
  Upon insertion of a basic block into a function, checkVREGs verifies that no
  virtual register occuring in the basic block belongs to a different function.

  If a virtual register belongs to a different function, checkVREGs asserts.
*/
void WIR_Function::checkVREGs( const WIR_BasicBlock &b ) const
{
  DSTART( "void WIR_Function::checkVREGs(const WIR_BasicBlock&) const" );

  (void) b;

  #ifdef FAILSAFEMODE
  if ( mCheckVREGs ) {
    WIR_VirtualRegisterSet vregs = b.getVREGs();

    for ( WIR_VirtualRegister &r : vregs )
      ufAssertT(
        !r.isInserted() || ( r.isInserted() && ( r.getFunction() == *this ) ),
        "Illegal attempt to insert basic block '" << b.getName() <<
        "' into function '" << getName() << "' (ID " << getID() <<
        ") where virtual register '" << r.getName() <<
        "' belongs to function '" << r.getFunction().getName() << "' (ID " <<
        r.getFunction().getID() << ")!" );
  }
  #endif
};


/*
  For the specified basic block, insertSymbol adds a corresponding symbol to the
  system's symbol table.

  insertSymbol only creates a new symbol if the current function is actually
  inserted into some %WIR system.
*/
void WIR_Function::insertSymbol( const WIR_BasicBlock &b )
{
  DSTART( "void WIR_Function::insertSymbol(const WIR_BasicBlock&)" );

  if ( isInserted() ) {
    WIR_CompilationUnit &c = getCompilationUnit();

    if ( c.isInserted() ) {
      WIR_System &sys = c.getSystem();
      sys.insertSymbol( b );
    }
  }
};

}       // namespace WIR
