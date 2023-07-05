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
  @file wirbasicblock.cc
  @brief This file implements %WIR basic blocks.

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
#include <iterator>
#include <list>
#include <map>
#include <sstream>
#include <vector>

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


//
// Public class methods
//

/*
  Default constructor creating an empty basic block.
*/
WIR_BasicBlock::WIR_BasicBlock( void ) :
  WIR_ID_API {},
  WIR_Container_API {},
  mFunctionPointer { nullptr },
  mDontOptimize { false },
  mDirectSuccessor { nullptr }
{
  DSTART( "WIR_BasicBlock::WIR_BasicBlock()" );
};


/*
  Copy constructor.

  When copying a basic block that is inserted in some %WIR function, the
  resulting copy will not be inserted in a function.
*/
WIR_BasicBlock::WIR_BasicBlock( const WIR_BasicBlock &__o ) :
  WIR_ID_API { __o },
  WIR_Container_API { __o },
  mFunctionPointer { nullptr },
  mDirectSuccessor { nullptr }
{
  DSTART( "WIR_BasicBlock::WIR_BasicBlock(const WIR_BasicBlock&)" );

  copyBasicBlock( __o );
};


/*
  Move constructor.

  Trying to move a basic block that is inserted in some %WIR function results in
  an assertion, since you are not allowed to move a basic block whose ownership
  is managed by a function.
*/
WIR_BasicBlock::WIR_BasicBlock( WIR_BasicBlock &&__o ) :
  WIR_ID_API { move( __o ) },
  WIR_Container_API { move( __o ) },
  mInstructions { move( __o.mInstructions ) },
  mInstructionReferences { move( __o.mInstructionReferences ) },
  mFunctionPointer { nullptr },
  mDontOptimize { __o.mDontOptimize },
  mDirectSuccessor { nullptr }
{
  DSTART( "WIR_BasicBlock::WIR_BasicBlock(WIR_BasicBlock&&)" );

  ufAssertT(
    __o.mFunctionPointer == nullptr,
    "Invalid attempt to move basic block '" << __o.getName() <<
    "' out of its owning function '" << __o.getFunction().getName() << "'." );

  __o.mInstructions.clear();
  __o.mInstructionReferences.clear();
  __o.mFunctionPointer = nullptr;
  __o.mDirectSuccessor = nullptr;

  // Adjust the parent IDs of the basic block's instructions.
  for ( auto &i : mInstructions )
    i.onInsert( this );
};


/*
  Destructor.
*/
WIR_BasicBlock::~WIR_BasicBlock( void )
{
  DSTART( "virtual WIR_BasicBlock::~WIR_BasicBlock()" );

  // Upon removal of a basic block from a function, we need to check whether the
  // block is explicit jump target of some WIR operations and have to remove
  // this explicit control flow property.
  if ( isInserted() ) {

    // Erase explicit jump target annotations from the last operation of each
    // predecessor block of b.
    // FIXME TODO: For architectures with delay slots (e.g., Leon3), it's not
    //             the /last/ operation that is the jump!
    for ( WIR_BasicBlock &pred : getPredecessors() ) {
      WIR_Operation *lastOp = nullptr;

      // Determine the predecessor's last operation.
      // FIXME TODO: For architectures with delay slots (e.g., Leon3), it's not
      //             the /last/ operation that is the jump!
      for ( auto it1 = pred.getInstructions().rbegin();
            it1 != pred.getInstructions().rend(); ++it1 ) {
        for ( auto it2 = (*it1).get().getOperations().rbegin();
              it2 != (*it1).get().getOperations().rend(); ++it2 ) {
          lastOp = &((*it2).get());
          break;
        }

        if ( lastOp != nullptr )
          break;
      }

      if ( lastOp != nullptr )
        lastOp->eraseJumpTarget( *this );
    }
  }

  // If a basic block is inserted indrectly into some WIR system, we have to
  // remove the basic block from the system's symbol table.
  if ( isInserted() ) {
    WIR_Function &f = getFunction();
    if ( f.isInserted() ) {
      WIR_CompilationUnit &c = f.getCompilationUnit();
      if ( c.isInserted() ) {
        WIR_System &s = c.getSystem();
        s.eraseSymbol( *this );
      }
    }
  }

  clearInstructions();
};


/*
  Copy-assignment operator.

  When copying a basic block that is inserted in some %WIR function, the
  resulting copy will not be inserted in a function.
*/
WIR_BasicBlock & WIR_BasicBlock::operator = ( const WIR_BasicBlock &__o )
{
  DSTART(
    "WIR_BasicBlock& WIR_BasicBlock::operator=(const WIR_BasicBlock&)" );

  WIR_Container_API::operator = ( __o );

  mFunctionPointer = nullptr;
  mDirectSuccessor = nullptr;

  copyBasicBlock( __o );

  return( *this );
};


/*
  Move-assignment operator.

  Trying to move a basic block that is inserted in some %WIR function results in
  an assertion, since you are not allowed to move a basic block  whose ownership
  is managed by a function.
*/
WIR_BasicBlock & WIR_BasicBlock::operator = ( WIR_BasicBlock &&__o )
{
  DSTART(
    "WIR_BasicBlock& WIR_BasicBlock::operator=(WIR_BasicBlock&&)" );

  ufAssertT(
    __o.mFunctionPointer == nullptr,
    "Invalid attempt to move basic block '" << __o.getName() <<
    "' out of its owning function '" << __o.getFunction().getName() << "'." );

  WIR_Container_API::operator = ( move( __o ) );

  mInstructions = move( __o.mInstructions );
  __o.mInstructions.clear();
  mInstructionReferences = move( __o.mInstructionReferences );
  __o.mInstructionReferences.clear();
  mFunctionPointer = nullptr;
  mDontOptimize = __o.mDontOptimize;
  mDirectSuccessor = nullptr;
  __o.mDirectSuccessor = nullptr;

  // Adjust the parent IDs of the basic block's instructions.
  for ( auto &i : mInstructions )
    i.onInsert( this );

  return( *this );
};


//
// API implementations.
//

WIR_INSERTION_IMPL( WIR_Function, Function, WIR_BasicBlock );


WIR_Instruction & WIR_BasicBlock::pushBackInstruction( const WIR_Instruction &o )
{
  DSTART(
    "WIR_Instruction& WIR_BasicBlock::pushBackInstruction(const WIR_Instruction&)" );

  checkDontOptimize();
  checkVREGs( o );

  mInstructions.push_back( o );
  mInstructions.back().onInsert( this );
  mInstructionReferences.push_back( mInstructions.back() );

  invalidateSymbols();

  return( mInstructions.back() );
};


WIR_Instruction & WIR_BasicBlock::pushBackInstruction( WIR_Instruction &&o )
{
  DSTART(
    "WIR_Instruction& WIR_BasicBlock::pushBackInstruction(WIR_Instruction&&)" );

  checkDontOptimize();
  checkVREGs( o );

  mInstructions.emplace_back( move( o ) );
  mInstructions.back().onInsert( this );
  mInstructionReferences.push_back( mInstructions.back() );

  invalidateSymbols();

  return( mInstructions.back() );
};


/*
  moveInstruction moves the specified %WIR instruction from its previous basic
  block to this one.

  moveInstruction fails with an assertion if the instruction to be moved refers
  to virtual registers belonging to a function that is different than that
  owning this basic block.
*/
WIR_Instruction &WIR_BasicBlock::moveInstruction( WIR_Instruction &i )
{
  DSTART(
    "WIR_Instruction& WIR_BasicBlock::moveInstruction(WIR_Instruction&)" );

  auto &previousOwner = i.getBasicBlock();

  checkDontOptimize();
  previousOwner.checkDontOptimize();

  ufAssert(
    !previousOwner.isInserted() ||
    ( previousOwner.mFunctionPointer == mFunctionPointer ) );

  // Determine where within scheduling constraints instruction i occurs.
  map<WIR_id_t, list<reference_wrapper<WIR_Instruction>>::iterator> constrPos;

  for ( WIR_SchedulingConstraint &c :
          previousOwner.getContainers<WIR_SchedulingConstraint>() )
    for ( auto it = c.mInstrSequence.begin(); it != c.mInstrSequence.end();
          ++it )
      if ( it->get() == i ) {
        constrPos[ c.getID() ] = it;
        c.mInstrs.erase( i );
      }

  // Determine position of i in the internal lists of its previous owner.
  auto it1 = previousOwner.mInstructions.begin();
  auto it2 = previousOwner.mInstructionReferences.begin();
  for ( ; *it1 != i; ++it1, ++it2 ) ;

  // Move i from its previous owner to the new one.
  i.mBasicBlockPointer = nullptr;
  mInstructions.emplace_back( std::move( i ) );
  mInstructions.back().onInsert( this );
  mInstructionReferences.push_back( mInstructions.back() );

  invalidateSymbols();

  // Remove i from its previous owner.
  previousOwner.mInstructions.erase( it1 );
  previousOwner.mInstructionReferences.erase( it2 );

  // Add the moved instruction to all involved scheduling constraints.
  for ( WIR_SchedulingConstraint &c :
          previousOwner.getContainers<WIR_SchedulingConstraint>() )
    if ( constrPos.count( c.getID() ) ) {
      c.mInstrs.insert( mInstructions.back() );
      c.mInstrSequence.insert( constrPos[ c.getID() ], mInstructions.back() );
      c.mInstrSequence.erase( constrPos[ c.getID() ] );
    }

  // Check scheduling constraints of the involved basic blocks.
  for ( WIR_SchedulingConstraint &c :
          previousOwner.getContainers<WIR_SchedulingConstraint>() )
    ufAssertT( c.check(), "Scheduling constraint violated." );
  for ( WIR_SchedulingConstraint &c :
          getContainers<WIR_SchedulingConstraint>() )
    ufAssertT( c.check(), "Scheduling constraint violated." );

  return( mInstructions.back() );
};


WIR_Instruction & WIR_BasicBlock::pushFrontInstruction( const WIR_Instruction &o )
{
  DSTART(
    "WIR_Instruction& WIR_BasicBlock::pushFrontInstruction(const WIR_Instruction&)" );

  checkDontOptimize();
  checkVREGs( o );

  mInstructions.push_front( o );
  mInstructions.front().onInsert( this );
  mInstructionReferences.push_front( mInstructions.front() );

  invalidateSymbols();

  return( mInstructions.front() );
};


WIR_Instruction & WIR_BasicBlock::pushFrontInstruction( WIR_Instruction &&o )
{
  DSTART(
    "WIR_Instruction& WIR_BasicBlock::pushFrontInstruction(WIR_Instruction&&)" );

  checkDontOptimize();
  checkVREGs( o );

  mInstructions.emplace_front( move( o ) );
  mInstructions.front().onInsert( this );
  mInstructionReferences.push_front( mInstructions.front() );

  invalidateSymbols();

  return( mInstructions.front() );
};


std::list<std::reference_wrapper<WIR_Instruction>>::iterator WIR_BasicBlock::insertInstruction( std::list<std::reference_wrapper<WIR_Instruction>>::const_iterator pos,
                                                                                                const WIR_Instruction &o )
{
  DSTART(
    "list<reference_wrapper<WIR_Instruction> >::iterator WIR_BasicBlock::insertInstruction(list<reference_wrapper<WIR_Instruction> >::const_iterator, const WIR_Instruction&)" );

  checkDontOptimize();
  checkVREGs( o );

  auto it = mInstructions.begin();
  for ( auto itr = mInstructionReferences.begin(); itr != pos; ++itr, ++it ) ;
  auto it1 = mInstructions.insert( it, o );
  (*it1).onInsert( this );

  invalidateSymbols();

  auto res = mInstructionReferences.insert( pos, *it1 );

  // Check scheduling constraints.
  for ( WIR_SchedulingConstraint &c :
          getContainers<WIR_SchedulingConstraint>() )
    ufAssertT( c.check(), "Scheduling constraint violated." );

  return( res );
};


std::list<std::reference_wrapper<WIR_Instruction>>::iterator WIR_BasicBlock::insertInstruction( std::list<std::reference_wrapper<WIR_Instruction>>::const_iterator pos,
                                                                                                WIR_Instruction &&o )
{
  DSTART(
    "list<reference_wrapper<WIR_Instruction> >::iterator WIR_BasicBlock::insertInstruction(list<reference_wrapper<WIR_Instruction> >::const_iterator, WIR_Instruction&&)" );

  checkDontOptimize();
  checkVREGs( o );

  auto it = mInstructions.begin();
  for ( auto itr = mInstructionReferences.begin(); itr != pos; ++itr, ++it ) ;
  auto it1 = mInstructions.insert( it, move( o ) );
  (*it1).onInsert( this );

  invalidateSymbols();

  auto res = mInstructionReferences.insert( pos, *it1 );

  // Check scheduling constraints.
  for ( WIR_SchedulingConstraint &c :
          getContainers<WIR_SchedulingConstraint>() )
    ufAssertT( c.check(), "Scheduling constraint violated." );

  return( res );
};


std::list<std::reference_wrapper<WIR_Instruction>>::iterator WIR_BasicBlock::replaceInstruction( std::list<std::reference_wrapper<WIR_Instruction>>::const_iterator pos,
                                                                                                 const WIR_Instruction &o )
{
  DSTART(
    "list<reference_wrapper<WIR_Instruction> >::iterator WIR_BasicBlock::replaceInstruction(list<reference_wrapper<WIR_Instruction> >::const_iterator, const WIR_Instruction&)" );

  checkDontOptimize();
  checkVREGs( o );

  auto it1 = mInstructions.begin();
  for ( auto itr = mInstructionReferences.begin(); itr != pos; ++itr, ++it1 ) ;
  auto it2 = mInstructions.insert( it1, o );
  (*it2).onInsert( this );

  invalidateSymbols();

  auto it = mInstructionReferences.insert( pos, *it2 );
  mInstructionReferences.erase( pos );
  mInstructions.erase( it1 );

  // Check scheduling constraints.
  for ( WIR_SchedulingConstraint &c :
          getContainers<WIR_SchedulingConstraint>() )
    ufAssertT( c.check(), "Scheduling constraint violated." );

  return( it );
};


std::list<std::reference_wrapper<WIR_Instruction>>::iterator WIR_BasicBlock::replaceInstruction( std::list<std::reference_wrapper<WIR_Instruction>>::const_iterator pos,
                                                                                                 WIR_Instruction &&o )
{
  DSTART(
    "list<reference_wrapper<WIR_Instruction> >::iterator WIR_BasicBlock::replaceInstruction(list<reference_wrapper<WIR_Instruction> >::const_iterator, WIR_Instruction&&)" );

  checkDontOptimize();
  checkVREGs( o );

  auto it1 = mInstructions.begin();
  for ( auto itr = mInstructionReferences.begin(); itr != pos; ++itr, ++it1 ) ;
  auto it2 = mInstructions.insert( it1, move( o ) );
  (*it2).onInsert( this );

  invalidateSymbols();

  auto it = mInstructionReferences.insert( pos, *it2 );
  mInstructionReferences.erase( pos );
  mInstructions.erase( it1 );

  // Check scheduling constraints.
  for ( WIR_SchedulingConstraint &c :
          getContainers<WIR_SchedulingConstraint>() )
    ufAssertT( c.check(), "Scheduling constraint violated." );

  return( it );
};


void WIR_BasicBlock::popBackInstruction( void )
{
  DSTART( "void WIR_BasicBlock::popBackInstruction()" );

  checkDontOptimize();

  if ( mInstructionReferences.empty() )
    return;

  mInstructionReferences.pop_back();
  mInstructions.pop_back();

  invalidateSymbols();
};


void WIR_BasicBlock::popFrontInstruction( void )
{
  DSTART( "void WIR_BasicBlock::popFrontInstruction()" );

  checkDontOptimize();

  if ( mInstructionReferences.empty() )
    return;

  mInstructionReferences.pop_front();
  mInstructions.pop_front();

  invalidateSymbols();
};


std::list<std::reference_wrapper<WIR_Instruction>>::iterator WIR_BasicBlock::eraseInstruction( std::list<std::reference_wrapper<WIR_Instruction>>::const_iterator pos )
{
  DSTART(
    "list<reference_wrapper<WIR_Instruction> >::iterator WIR_BasicBlock::eraseInstruction(list<reference_wrapper<WIR_Instruction> >::const_iterator)" );

  checkDontOptimize();

  auto it1 = mInstructions.begin();
  for ( auto itr = mInstructionReferences.begin(); itr != pos; ++itr, ++it1 ) ;
  auto it = mInstructionReferences.erase( pos );
  mInstructions.erase( it1 );

  invalidateSymbols();

  return( it );
};


void WIR_BasicBlock::clearInstructions( void )
{
  DSTART( "void WIR_BasicBlock::clearInstructions()" );

  checkDontOptimize();

  // Clear all scheduling constraints.
  eraseContainers( WIR_SchedulingConstraint::getContainerTypeID() );

  // Clear all instructions.
  mInstructionReferences.clear();
  mInstructions.clear();

  invalidateSymbols();
};


const std::list<std::reference_wrapper<WIR_Instruction>> &WIR_BasicBlock::getInstructions( void ) const
{
  DSTART(
    "const list<reference_wrapper<WIR_Instruction> >& WIR_BasicBlock::getInstructions() const" );

  return( mInstructionReferences );
};


/*
  begin returns an iterator to the first instruction of a basic block.
*/
std::list<std::reference_wrapper<WIR_Instruction>>::const_iterator WIR_BasicBlock::begin( void ) const
{
  DSTART(
    "list<reference_wrapper<WIR_Instruction> >::const_iterator WIR_BasicBlock::begin() const" );

  return( mInstructionReferences.begin() );
};


/*
  end returns an iterator to the end of a basic block's instruction list.
*/
std::list<std::reference_wrapper<WIR_Instruction>>::const_iterator WIR_BasicBlock::end( void ) const
{
  DSTART(
    "list<reference_wrapper<WIR_Instruction> >::const_iterator WIR_BasicBlock::end() const" );

  return( mInstructionReferences.end() );
};


/*
  rbegin returns an iterator to the reverse-first instruction of a basic block.
*/
std::list<std::reference_wrapper<WIR_Instruction>>::const_reverse_iterator WIR_BasicBlock::rbegin( void ) const
{
  DSTART(
    "list<reference_wrapper<WIR_Instruction> >::const_reverse_iterator WIR_BasicBlock::rbegin() const" );

  return( mInstructionReferences.rbegin() );
};


/*
  rend returns an iterator to the reverse-end of a basic block's instruction
  list.
*/
std::list<std::reference_wrapper<WIR_Instruction>>::const_reverse_iterator WIR_BasicBlock::rend( void ) const
{
  DSTART(
    "list<reference_wrapper<WIR_Instruction> >::const_reverse_iterator WIR_BasicBlock::rend() const" );

  return( mInstructionReferences.rend() );
};


bool WIR_BasicBlock::containsInstruction( WIR_id_t id ) const
{
  DSTART( "bool WIR_BasicBlock::containsInstruction(WIR_id_t) const" );

  for ( auto &item : mInstructions )
    if ( item.getID() == id )
      return( true );

  return( false );
};


bool WIR_BasicBlock::containsInstruction( const WIR_Instruction &o ) const
{
  DSTART(
    "bool WIR_BasicBlock::containsInstruction(const WIR_Instruction&) const" );

  for ( auto &item : mInstructions )
    if ( item == o )
      return( true );

  return( false );
};


std::list<std::reference_wrapper<WIR_Instruction>>::const_iterator WIR_BasicBlock::findInstruction( WIR_id_t id ) const
{
  DSTART(
    "list<reference_wrapper<WIR_Instruction> >::const_iterator WIR_BasicBlock::findInstruction(WIR_id_t) const" );

  for ( auto it = mInstructionReferences.begin();
        it != mInstructionReferences.end(); ++it )
    if ( (*it).get().getID() == id )
      return( it );

  return( mInstructionReferences.end() );
};


std::list<std::reference_wrapper<WIR_Instruction>>::const_iterator WIR_BasicBlock::findInstruction( const WIR_Instruction &o ) const
{
  DSTART(
    "list<reference_wrapper<WIR_Instruction> >::const_iterator WIR_BasicBlock::findInstruction(const WIR_Instruction&) const" );

  for ( auto it = mInstructionReferences.begin();
        it != mInstructionReferences.end(); ++it )
    if ( (*it).get() == o )
      return( it );

  return( mInstructionReferences.end() );
};


/*
  getName returns a basic block's specific name as constructed by function
  dumpWIRBlockLabel.
*/
std::string WIR_BasicBlock::getName( void ) const
{
  DSTART( "string WIR_BasicBlock::getName() const" );

  stringstream str;

  WIR_Registry::getBlockLabelDumper( 0 )( str, *this );
  return( str.str() );
};


/*
  getSize returns a basic block's size in bytes.

  getSize rounds upwards. I.e., if a basic block occupies 10 bytes and 4 bits,
  getSize returns a size of 11 bytes.
*/
unsigned long long WIR_BasicBlock::getSize( void ) const
{
  DSTART( "long long unsigned int WIR_BasicBlock::getSize() const" );

  unsigned long long res = 0;
  unsigned long long remainingBits = 0;

  for ( WIR_Instruction &i : getInstructions() ) {
    remainingBits += i.getBitWidth();

    res += remainingBits / 8;
    remainingBits %= 8;
  }

  if ( remainingBits != 0 )
    ++res;

  return( res );
};


/*
  getSuccessors returns a set of WIR basic blocks that could be executed
  immediately after this block according to the control flow structure.

  The set of successor blocks is determined as follows:
  -# If the last operation of this block is a jump, the target blocks of that
     jump are returned.
  -# Otherwise, the immediate successor of this block within its WIR function is
     returned.
  Note that interprocedural control flow stemming from function calls is not
  considered here.

  // FIXME TODO: For architectures with delay slots (e.g., Leon3), it's not
  //             the /last/ operation that is the jump!
*/
WIR_BasicBlockSet WIR_BasicBlock::getSuccessors( void ) const
{
  DSTART( "WIR_BasicBlockSet WIR_BasicBlock::getSuccessors() const" );

  WIR_BasicBlockSet res;
  WIR_Operation *lastOp = nullptr;

  // Determine the last operation.
  // FIXME TODO: For architectures with delay slots (e.g., Leon3), it's not
  //             the /last/ operation that is the jump!
  for ( auto it1 = getInstructions().rbegin(); it1 != getInstructions().rend();
        ++it1 ) {
    for ( auto it2 = (*it1).get().getOperations().rbegin();
          it2 != (*it1).get().getOperations().rend(); ++it2 ) {
      lastOp = &(it2->get());
      break;
    }

    if ( lastOp != nullptr )
      break;
  }

  if ( lastOp != nullptr ) {
    WIR_Operation &o = *lastOp;

    // A return from a function has, by construction, no successors in the CFG.
    // However, the proper detection of a return is a bit tricky. On some
    // architectures (e.g., TriCore), there is a dedicated RET operation whose
    // only purpose is to return from a function. However, for other
    // architectures (e.g., ARM or MIPS) indirect jumps are used which might be
    // a return or simply an indirect jump elsewhere. However, classical
    // indirect jumps should be properly annotated with their jump targets. So,
    // we assume in the following that an indirect jump without any jump target
    // annotations is a function return.
    if ( o.isReturn() && !o.jumpTargetsAdded() )
      return( res );

    if ( o.isJump() ) {
      // The last operation is a jump. So, let's add all jump target blocks.
      res = o.getJumpTargets();

      // If o is an unconditional or an indirect jump, or if the jump targets
      // are explicitly specified by the user, we have collected all successors
      // with the above code based on o.getJumpTargets so that we can return
      // now. If, however, o is a conditional jump, we have to add the immediate
      // successor block if the jump is not taken, which is done below.
      if ( o.jumpTargetsAdded() || !o.isConditionalJump() )
        return( res );
    }
  }

  // For all remaining cases (current basic block is empty, or its last
  // operation is not a jump, or it is a conditional jump), add the immediate
  // successor block.
  if ( mDirectSuccessor != nullptr )
    res.insert( *mDirectSuccessor );

  return( res );
};


/*
  getPredecessors returns a set of WIR basic blocks that could be executed
  immediately before this block according to the control flow structure.

  The set of predecessor blocks is given by all those basic blocks of this
  block's function where this block appears as successor.
*/
WIR_BasicBlockSet WIR_BasicBlock::getPredecessors( void ) const
{
  DSTART( "WIR_BasicBlockSet WIR_BasicBlock::getPredecessors() const" );

  WIR_BasicBlockSet res;

  if ( isInserted() ) {
    WIR_Function &f = getFunction();

    // Check successors of all basic blocks of the current function.
    for ( WIR_BasicBlock &b : f ) {
      auto succs = b.getSuccessors();
      if ( succs.count( const_cast<WIR_BasicBlock &>( *this ) ) )
        res.insert( b );
    }
  }

  return( res );
};


/*
  getVREGs determines all virtual registers that occur in this basic block's
  instructions.
*/
WIR_VirtualRegisterSet WIR_BasicBlock::getVREGs( void ) const
{
  DSTART( "WIR_VirtualRegisterSet WIR_BasicBlock::getVREGs() const" );

  WIR_VirtualRegisterSet res;

  for ( WIR_Instruction &i : getInstructions() )
    for ( WIR_VirtualRegister &r : i.getVREGs() )
      res.insert( r );

  return( res );
};


/*
  setDontOptimize sets whether a basic block can be modified or must not be
  changed by some optimization or transformation.
*/
void WIR_BasicBlock::setDontOptimize( bool f )
{
  DSTART( "void WIR_BasicBlock::setDontOptimize(bool)" );

  mDontOptimize = f;
};


/*
  getDontOptimize returns whether a basic block can be modified or must not be
  changed by some optimization or transformation.

  A basic block must not be modified if the block by itself has been marked as
  such using setDontOptimize, or if it is inserted into a %WIR function that in
  turn must not be modified.
*/
bool WIR_BasicBlock::getDontOptimize( void ) const
{
  DSTART( "bool WIR_BasicBlock::getDontOptimize() const" );

  return(
    mDontOptimize || ( isInserted() && getFunction().getDontOptimize() ) );
};


/*
  The << operator dumps a WIR basic block to an output stream.

  By applying processor-specific I/O manipulators to the output stream
  beforehand, this << operator can flexibly emit valid assembly output for
  arbitrary processor architectures.
*/
std::ostream & operator << ( std::ostream &os, const WIR_BasicBlock &b )
{
  DSTART( "ostream& operator<<(ostream&, const WIR_BasicBlock&)" );

  WIR_Registry::getBasicBlockDumper( os.iword( WIR_ProcessorIO() ) )( os, b );

  return( os );
};


//
// Private class methods
//

/*
  Dummy function for adding instructions which does nothing.

  It only serves to terminate the recursion of the variadic method
  addInstructions.
*/
void WIR_BasicBlock::addInstructions( void ) const
{
  DSTART( "void WIR_BasicBlock::addInstructions() const" );
};


/*
  copyBasicBlock performs actions common to the copy constructor and copy
  assignment operator of WIR basic blocks.
*/
void WIR_BasicBlock::copyBasicBlock( const WIR_BasicBlock &__o )
{
  DSTART( "void WIR_BasicBlock::copyBasicBlock(const WIR_BasicBlock&)" );

  mDontOptimize = false;

  // Sort the IDs of instructions from __o.
  vector<WIR_id_t> originalIDs;
  map<WIR_id_t, WIR_Instruction *> oldInstructionIDMap;
  for ( WIR_Instruction &i : __o.mInstructionReferences ) {
    originalIDs.push_back( i.getID() );
    oldInstructionIDMap[ i.getID() ] = &i;
  }
  sort( originalIDs.begin(), originalIDs.end() );

  // Copy instructions according to the sorted order.
  map<WIR_id_t, WIR_Instruction *> instructionIDMap;
  list<WIR_Instruction> newInstructions;
  for ( auto id : originalIDs ) {
    newInstructions.push_back( *(oldInstructionIDMap[ id ]) );
    instructionIDMap[ id ] = &(newInstructions.back());
  }

  // Insert copied instructions into this basic block in the right order.
  clearInstructions();
  for ( WIR_Instruction &i : __o.mInstructionReferences )
    pushBackInstruction( move( *(instructionIDMap[ i.getID() ] ) ) );

  mDontOptimize = __o.mDontOptimize;

  // Adapt scheduling constraints.
  instructionIDMap.clear();
  auto iIt = mInstructionReferences.begin();
  for ( auto oIt = __o.mInstructionReferences.begin();
        oIt != __o.mInstructionReferences.end(); ++oIt, ++iIt )
    instructionIDMap[ oIt->get().getID() ] = &(iIt->get());

  for ( WIR_SchedulingConstraint &c :
          getContainers<WIR_SchedulingConstraint>() )
    for ( auto it = c.mInstrSequence.begin(); it != c.mInstrSequence.end(); ) {
      WIR_Instruction &oldInstr = it->get();
      WIR_Instruction &newInstr = *(instructionIDMap[ oldInstr.getID() ]);

      c.mInstrs.erase( oldInstr );
      c.mInstrs.insert( newInstr );

      c.mInstrSequence.insert( it, newInstr );
      it = c.mInstrSequence.erase( it );
    }
};


/*
  checkDontOptimize checks whether a basic block must not be modified.

  If this basic block must not be modified, checkDontOptimize asserts.
*/
void WIR_BasicBlock::checkDontOptimize( void ) const
{
  DSTART( "void WIR_BasicBlock::checkDontOptimize() const" );

  ufAssertT(
    !getDontOptimize(),
    "Illegal attempt to modify a basic block that is set as 'don't optimize'!" );
};


/*
  Upon insertion of an instruction into a basic block, checkVREGs verifies that
  no virtual register occuring in the instruction belongs to a function
  different than that owning this basic block.

  If a virtual register belongs to a different function, checkVREGs asserts.
*/
void WIR_BasicBlock::checkVREGs( const WIR_Instruction &i ) const
{
  DSTART( "void WIR_BasicBlock::checkVREGs(const WIR_Instruction&) const" );

  (void) i;

  #ifdef FAILSAFEMODE
  if ( isInserted() ) {
    WIR_Function &f = getFunction();
    WIR_VirtualRegisterSet vregs = i.getVREGs();

    for ( WIR_VirtualRegister &r : vregs )
      ufAssertT(
        !r.isInserted() || ( r.isInserted() && ( r.getFunction() == f ) ),
        "Illegal attempt to insert an instruction into basic block '" <<
        getName() << "' owned by function '" << f.getName() << "' (ID " <<
        f.getID() << ") where virtual register '" << r.getName() <<
        "' belongs to function '" << r.getFunction().getName() << "' (ID " <<
        r.getFunction().getID() << ")!" );
  }
  #endif
};


/*
  invalidateSymbols marks the information of a system's global symbol table as
  invalid.

  invalidateSymbols only invalidates the symbol table if the current basic block
  is actually inserted into some WIR system.
*/
void WIR_BasicBlock::invalidateSymbols( void ) const
{
  DSTART( "void WIR_BasicBlock::invalidateSymbols() const" );

  if ( isInserted() ) {
    WIR_Function &f = getFunction();
    if ( f.isInserted() ) {
      WIR_CompilationUnit &c = f.getCompilationUnit();
      if ( c.isInserted() ) {
        WIR_System &s = c.getSystem();
        s.invalidateSymbols();
      }
    }
  }
};

}       // namespace WIR
