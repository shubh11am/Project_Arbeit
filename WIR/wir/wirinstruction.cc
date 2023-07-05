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
  @file wirinstruction.cc
  @brief This file implements %WIR instructions.

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
#include <list>
#include <map>
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


//
// Public class methods
//

/*
  Default constructor creating an empty instruction.
*/
WIR_Instruction::WIR_Instruction( void ) :
  WIR_ID_API {},
  WIR_Container_API {},
  mBasicBlockPointer { nullptr },
  mDontOptimize { false }
{
  DSTART( "WIR_Instruction::WIR_Instruction()" );
};


/*
  Copy constructor.

  When copying an instruction that is inserted in some %WIR basic block, the
  resulting copy will not be inserted in a basic block.
*/
WIR_Instruction::WIR_Instruction( const WIR_Instruction &__o ) :
  WIR_ID_API { __o },
  WIR_Container_API { __o },
  mBasicBlockPointer { nullptr }
{
  DSTART( "WIR_Instruction::WIR_Instruction(const WIR_Instruction&)" );

  copyInstruction( __o );
};


/*
  Move constructor.

  Trying to move an instruction that is inserted in some %WIR basic block
  results in an assertion, since you are not allowed to move a register whose
  ownership is managed by a basic block.
*/
WIR_Instruction::WIR_Instruction( WIR_Instruction &&__o ) :
  WIR_ID_API { move( __o ) },
  WIR_Container_API { move( __o ) },
  mOperations { move( __o.mOperations ) },
  mOperationReferences { move( __o.mOperationReferences ) },
  mBasicBlockPointer { nullptr },
  mDontOptimize { __o.mDontOptimize }
{
  DSTART( "WIR_Instruction::WIR_Instruction(WIR_Instruction&&)" );

  ufAssertT(
    __o.mBasicBlockPointer == nullptr,
    "Invalid attempt to move an instruction out of its owning basic block '" <<
    __o.getBasicBlock().getName() << "'." );

  __o.mOperations.clear();
  __o.mOperationReferences.clear();

  // Adjust the parent IDs of the instruction's operations.
  for ( auto &o : mOperations )
    o.onInsert( this );
};


/*
  Destructor.
*/
WIR_Instruction::~WIR_Instruction( void )
{
  DSTART( "virtual WIR_Instruction::~WIR_Instruction()" );

  clearOperations();

  // Remove current instruction from scheduling constraints of its owning basic
  // block.
  if ( isInserted() ) {
    WIR_BasicBlock &b = getBasicBlock();
    set<WIR_SchedulingConstraint *> del;

    for ( WIR_SchedulingConstraint &c :
            b.getContainers<WIR_SchedulingConstraint>() )
      if ( c.mInstrs.count( *this ) ) {
        for ( auto it = c.mInstrSequence.begin();
              it != c.mInstrSequence.end(); )
          if ( it->get() == *this )
            it = c.mInstrSequence.erase( it );
          else
            ++it;

        c.mInstrs.erase( *this );

        // If the constraint finally only covers less than 2 instructions, it is
        // no longer a constraint and thus gets erased.
        if ( c.mInstrs.size() < 2 )
          del.insert( &c );
      }

    for ( auto *c : del )
      b.eraseContainer( *c );
  }
};


/*
  Copy-assignment operator.

  When copying an instruction that is inserted in some %WIR basic block, the
  resulting copy will not be inserted in a basic block.
*/
WIR_Instruction & WIR_Instruction::operator = ( const WIR_Instruction &__o )
{
  DSTART(
    "WIR_Instruction& WIR_Instruction::operator=(const WIR_Instruction&)" );

  WIR_Container_API::operator = ( __o );

  mBasicBlockPointer = nullptr;

  copyInstruction( __o );

  return( *this );
};


/*
  Move-assignment operator.

  Trying to move an instruction that is inserted in some %WIR basic block
  results in an assertion, since you are not allowed to move a register whose
  ownership is managed by a basic block.
*/
WIR_Instruction & WIR_Instruction::operator = ( WIR_Instruction &&__o )
{
  DSTART( "WIR_Instruction& WIR_Instruction::operator=(WIR_Instruction&&)" );

  ufAssertT(
    __o.mBasicBlockPointer == nullptr,
    "Invalid attempt to move an instruction out of its owning basic block '" <<
    __o.getBasicBlock().getName() << "'." );

  WIR_Container_API::operator = ( move( __o ) );

  mOperations = move( __o.mOperations );
  __o.mOperations.clear();
  mOperationReferences = move( __o.mOperationReferences );
  __o.mOperationReferences.clear();
  mBasicBlockPointer = nullptr;
  mDontOptimize = __o.mDontOptimize;

  // Adjust the parent IDs of the instruction's operations.
  for ( auto &o : mOperations )
    o.onInsert( this );

  return( *this );
};


//
// API implementations.
//

WIR_INSERTION_IMPL( WIR_BasicBlock, BasicBlock, WIR_Instruction );


WIR_Operation & WIR_Instruction::pushBackOperation( const WIR_Operation &o )
{
  DSTART(
    "WIR_Operation& WIR_Instruction::pushBackOperation(const WIR_Operation&)" );

  checkDontOptimize();
  checkVREGs( o );

  mOperations.push_back( o );
  mOperations.back().onInsert( this );
  mOperationReferences.push_back( mOperations.back() );

  return( mOperations.back() );
};


WIR_Operation & WIR_Instruction::pushBackOperation( WIR_Operation &&o )
{
  DSTART(
    "WIR_Operation& WIR_Instruction::pushBackOperation(WIR_Operation&&)" );

  checkDontOptimize();
  checkVREGs( o );

  mOperations.emplace_back( move( o ) );
  mOperations.back().onInsert( this );
  mOperationReferences.push_back( mOperations.back() );

  return( mOperations.back() );
};


WIR_Operation & WIR_Instruction::pushFrontOperation( const WIR_Operation &o )
{
  DSTART(
    "WIR_Operation& WIR_Instruction::pushFrontOperation(const WIR_Operation&)" );

  checkDontOptimize();
  checkVREGs( o );

  mOperations.push_front( o );
  mOperations.front().onInsert( this );
  mOperationReferences.push_front( mOperations.front() );

  return( mOperations.front() );
};


WIR_Operation & WIR_Instruction::pushFrontOperation( WIR_Operation &&o )
{
  DSTART(
    "WIR_Operation& WIR_Instruction::pushFrontOperation(WIR_Operation&&)" );

  checkDontOptimize();
  checkVREGs( o );

  mOperations.emplace_front( move( o ) );
  mOperations.front().onInsert( this );
  mOperationReferences.push_front( mOperations.front() );

  return( mOperations.front() );
};


std::list<std::reference_wrapper<WIR_Operation>>::iterator WIR_Instruction::insertOperation( std::list<std::reference_wrapper<WIR_Operation>>::const_iterator pos,
                                                                                             const WIR_Operation &o )
{
  DSTART(
    "list<reference_wrapper<WIR_Operation> >::iterator WIR_Instruction::insertOperation(list<reference_wrapper<WIR_Operation> >::const_iterator, const WIR_Operation&)" );

  checkDontOptimize();
  checkVREGs( o );

  auto it = mOperations.begin();
  for ( auto itr = mOperationReferences.begin(); itr != pos; ++itr, ++it ) ;
  auto it1 = mOperations.insert( it, o );
  (*it1).onInsert( this );
  return( mOperationReferences.insert( pos, *it1 ) );
};


std::list<std::reference_wrapper<WIR_Operation>>::iterator WIR_Instruction::insertOperation( std::list<std::reference_wrapper<WIR_Operation>>::const_iterator pos,
                                                                                             WIR_Operation &&o )
{
  DSTART(
    "list<reference_wrapper<WIR_Operation> >::iterator WIR_Instruction::insertOperation(list<reference_wrapper<WIR_Operation> >::const_iterator, WIR_Operation&&)" );

  checkDontOptimize();
  checkVREGs( o );

  auto it = mOperations.begin();
  for ( auto itr = mOperationReferences.begin(); itr != pos; ++itr, ++it ) ;
  auto it1 = mOperations.insert( it, move( o ) );
  (*it1).onInsert( this );
  return( mOperationReferences.insert( pos, *it1 ) );
};


std::list<std::reference_wrapper<WIR_Operation>>::iterator WIR_Instruction::replaceOperation( std::list<std::reference_wrapper<WIR_Operation>>::const_iterator pos,
                                                                                              const WIR_Operation &o )
{
  DSTART(
    "list<reference_wrapper<WIR_Operation> >::iterator WIR_Instruction::replaceOperation(list<reference_wrapper<WIR_Operation> >::const_iterator, const WIR_Operation&)" );

  checkDontOptimize();
  checkVREGs( o );

  auto it1 = mOperations.begin();
  for ( auto itr = mOperationReferences.begin(); itr != pos; ++itr, ++it1 ) ;
  auto it2 = mOperations.insert( it1, o );
  (*it2).onInsert( this );
  auto it = mOperationReferences.insert( pos, *it2 );
  mOperationReferences.erase( pos );
  mOperations.erase( it1 );
  return( it );
};


std::list<std::reference_wrapper<WIR_Operation>>::iterator WIR_Instruction::replaceOperation( std::list<std::reference_wrapper<WIR_Operation>>::const_iterator pos,
                                                                                              WIR_Operation &&o )
{
  DSTART(
    "list<reference_wrapper<WIR_Operation> >::iterator WIR_Instruction::replaceOperation(list<reference_wrapper<WIR_Operation> >::const_iterator, WIR_Operation&&)" );

  checkDontOptimize();
  checkVREGs( o );

  auto it1 = mOperations.begin();
  for ( auto itr = mOperationReferences.begin(); itr != pos; ++itr, ++it1 ) ;
  auto it2 = mOperations.insert( it1, move( o ) );
  (*it2).onInsert( this );
  auto it = mOperationReferences.insert( pos, *it2 );
  mOperationReferences.erase( pos );
  mOperations.erase( it1 );
  return( it );
};


void WIR_Instruction::popBackOperation( void )
{
  DSTART( "void WIR_Instruction::popBackOperation()" );

  checkDontOptimize();

  if ( mOperationReferences.empty() )
    return;

  mOperationReferences.pop_back();
  mOperations.pop_back();

  invalidateSymbols();
};


void WIR_Instruction::popFrontOperation( void )
{
  DSTART( "void WIR_Instruction::popFrontOperation()" );

  checkDontOptimize();

  if ( mOperationReferences.empty() )
    return;

  mOperationReferences.pop_front();
  mOperations.pop_front();

  invalidateSymbols();
};


std::list<std::reference_wrapper<WIR_Operation>>::iterator WIR_Instruction::eraseOperation( std::list<std::reference_wrapper<WIR_Operation>>::const_iterator pos )
{
  DSTART(
    "list<reference_wrapper<WIR_Operation> >::iterator WIR_Instruction::eraseOperation(list<reference_wrapper<WIR_Operation> >::const_iterator)" );

  checkDontOptimize();

  auto it1 = mOperations.begin();
  for ( auto itr = mOperationReferences.begin(); itr != pos; ++itr, ++it1 ) ;
  auto it = mOperationReferences.erase( pos );
  mOperations.erase( it1 );

  invalidateSymbols();

  return( it );
};


void WIR_Instruction::clearOperations( void )
{
  DSTART( "void WIR_Instruction::clearOperations()" );

  checkDontOptimize();

  mOperationReferences.clear();
  mOperations.clear();

  invalidateSymbols();
};


const std::list<std::reference_wrapper<WIR_Operation>> &WIR_Instruction::getOperations( void ) const
{
  DSTART(
    "const list<reference_wrapper<WIR_Operation> >& WIR_Instruction::getOperations() const" );

  return( mOperationReferences );
};


/*
  moveOperation moves the specified WIR operation from its current instruction
  to this one.

  The moved operation is added at the tail of this instruction's list of
  operations, i.e., a pushBackOperation is performed internally.

  moveOperation fails with an assertion if the operation to be moved refers to
  virtual registers belonging to a function that is different than that owning
  this instruction.
*/
WIR_Operation &WIR_Instruction::moveOperation( WIR_Operation &o )
{
  DSTART( "WIR_Operation& WIR_Instruction::moveOperation(WIR_Operation&)" );

  auto &previousOwner = o.getInstruction();

  checkDontOptimize();
  previousOwner.checkDontOptimize();

  ufAssert(
    !previousOwner.getBasicBlock().isInserted() ||
    ( previousOwner.getBasicBlock().getFunction() ==
        getBasicBlock().getFunction() ) );

  // Determine position of o in the internal lists of its previous owner.
  auto it1 = previousOwner.mOperations.begin();
  auto it2 = previousOwner.mOperationReferences.begin();
  for ( ; *it1 != o; ++it1, ++it2 ) ;

  // Move o from its previous owner to the new one.
  o.mInstructionPointer = nullptr;
  mOperations.emplace_back( std::move( o ) );
  mOperations.back().onInsert( this );
  mOperationReferences.push_back( mOperations.back() );

  invalidateSymbols();

  // Remove o from its previous owner.
  previousOwner.mOperations.erase( it1 );
  previousOwner.mOperationReferences.erase( it2 );

  return( mOperations.back() );
};


/*
  begin returns an iterator to the first operation of an instruction.
*/
std::list<std::reference_wrapper<WIR_Operation>>::const_iterator WIR_Instruction::begin( void ) const
{
  DSTART(
    "list<reference_wrapper<WIR_Operation> >::const_iterator WIR_Instruction::begin() const" );

  return( mOperationReferences.begin() );
};


/*
  end returns an iterator to the end of an instruction's operation list.
*/
std::list<std::reference_wrapper<WIR_Operation>>::const_iterator WIR_Instruction::end( void ) const
{
  DSTART(
    "list<reference_wrapper<WIR_Operation> >::const_iterator WIR_Instruction::end() const" );

  return( mOperationReferences.end() );
};


/*
  rbegin returns an iterator to the reverse-first operation of an instruction.
*/
std::list<std::reference_wrapper<WIR_Operation>>::const_reverse_iterator WIR_Instruction::rbegin( void ) const
{
  DSTART(
    "list<reference_wrapper<WIR_Operation> >::const_reverse_iterator WIR_Instruction::rbegin() const" );

  return( mOperationReferences.rbegin() );
};


/*
  rend returns an iterator to the reverse-end of an instruction's operation
  list.
*/
std::list<std::reference_wrapper<WIR_Operation>>::const_reverse_iterator WIR_Instruction::rend( void ) const
{
  DSTART(
    "list<reference_wrapper<WIR_Operation> >::const_reverse_iterator WIR_Instruction::rend() const" );

  return( mOperationReferences.rend() );
};


bool WIR_Instruction::containsOperation( WIR_id_t id ) const
{
  DSTART( "bool WIR_Instruction::containsOperation(WIR_id_t) const" );

  for ( auto &item : mOperations )
    if ( item.getID() == id )
      return( true );

  return( false );
};


bool WIR_Instruction::containsOperation( const WIR_Operation &o ) const
{
  DSTART(
    "bool WIR_Instruction::containsOperation(const WIR_Operation&) const" );

  for ( auto &item : mOperations )
    if ( item == o )
      return( true );

  return( false );
};


std::list<std::reference_wrapper<WIR_Operation>>::const_iterator WIR_Instruction::findOperation( WIR_id_t id ) const
{
  DSTART(
    "list<reference_wrapper<WIR_Operation> >::const_iterator WIR_Instruction::findOperation(WIR_id_t) const" );

  for ( auto it = mOperationReferences.begin();
        it != mOperationReferences.end(); ++it )
    if ( (*it).get().getID() == id )
      return( it );

  return( mOperationReferences.end() );
};


std::list<std::reference_wrapper<WIR_Operation>>::const_iterator WIR_Instruction::findOperation( const WIR_Operation &o ) const
{
  DSTART(
    "list<reference_wrapper<WIR_Operation> >::const_iterator WIR_Instruction::findOperation(const WIR_Operation&) const" );

  for ( auto it = mOperationReferences.begin();
        it != mOperationReferences.end(); ++it )
    if ( (*it).get() == o )
      return( it );

  return( mOperationReferences.end() );
};


/*
  getBitWidth returns an instruction's bit width.
*/
unsigned int WIR_Instruction::getBitWidth( void ) const
{
  DSTART( "unsigned int WIR_Instruction::getBitWidth() const" );

  unsigned int res = 0;

  for ( WIR_Operation &o : getOperations() )
    res += o.getBitWidth();

  return( res );
};


/*
  getSize returns an instruction's size in bytes.

  getSize rounds upwards. I.e., if an instruction occupies 20 bits, getSize
  returns a size of 3 bytes.
*/
unsigned long long WIR_Instruction::getSize( void ) const
{
  DSTART( "long long unsigned int WIR_Instruction::getSize() const" );

  unsigned int bits = getBitWidth();

  if ( bits % 8 != 0 )
    return( getBitWidth() / 8 + 1 );
  else
    return( getBitWidth() / 8 );
};


/*
  getVREGs determines all virtual registers that occur in this instruction's
  operations.
*/
WIR_VirtualRegisterSet WIR_Instruction::getVREGs( void ) const
{
  DSTART( "WIR_VirtualRegisterSet WIR_Instruction::getVREGs() const" );

  WIR_VirtualRegisterSet res;

  for ( WIR_Operation &o : getOperations() )
    for ( WIR_VirtualRegister &r : o.getVREGs() )
      res.insert( r );

  return( res );
};


/*
  setDontOptimize sets whether an instruction can be modified or must not be
  changed by some optimization or transformation.
*/
void WIR_Instruction::setDontOptimize( bool f )
{
  DSTART( "void WIR_Instruction::setDontOptimize(bool)" );

  mDontOptimize = f;
};


/*
  getDontOptimize returns whether an instruction can be modified or must not be
  changed by some optimization or transformation.

  An instruction must not be modified if the instruction by itself has been
  marked as such using setDontOptimize, or if it is inserted into a %WIR basic
  block that in turn must not be modified.
*/
bool WIR_Instruction::getDontOptimize( void ) const
{
  DSTART( "bool WIR_Instruction::getDontOptimize() const" );

  return(
    mDontOptimize || ( isInserted() && getBasicBlock().getDontOptimize() ) );
};


/*
  The << operator dumps a WIR instruction to an output stream.

  By applying processor-specific I/O manipulators to the output stream
  beforehand, this << operator can flexibly emit valid assembly output for
  arbitrary processor architectures.
*/
std::ostream & operator << ( std::ostream &os, const WIR_Instruction &i )
{
  DSTART( "ostream& operator<<(ostream&, const WIR_Instruction&)" );

  WIR_Registry::getInstructionDumper( os.iword( WIR_ProcessorIO() ) )( os, i );

  return( os );
};


//
// Private class methods
//

/*
  Dummy function for adding operations which does nothing.

  It only serves to terminate the recursion of the variadic method
  addOperations.
*/
void WIR_Instruction::addOperations( void ) const
{
  DSTART( "void WIR_Instruction::addOperations() const" );
};


/*
  copyInstruction performs actions common to the copy constructor and copy
  assignment operator of WIR instructions.
*/
void WIR_Instruction::copyInstruction( const WIR_Instruction &__o )
{
  DSTART( "void WIR_Instruction::copyInstruction(const WIR_Instruction&)" );

  mDontOptimize = false;

  // Sort the IDs of operations from __o.
  vector<WIR_id_t> originalIDs;
  map<WIR_id_t, WIR_Operation *> oldOperationIDMap;
  for ( WIR_Operation &o : __o.mOperationReferences ) {
    originalIDs.push_back( o.getID() );
    oldOperationIDMap[ o.getID() ] = &o;
  }
  sort( originalIDs.begin(), originalIDs.end() );

  // Copy operations according to the sorted order.
  map<WIR_id_t, WIR_Operation *> operationIDMap;
  list<WIR_Operation> newOperations;
  for ( auto id : originalIDs ) {
    newOperations.push_back( *(oldOperationIDMap[ id ]) );
    operationIDMap[ id ] = &(newOperations.back());
  }

  // Insert copied operations into this instruction in the right order.
  clearOperations();
  for ( WIR_Operation &o : __o.mOperationReferences )
    pushBackOperation( move( *(operationIDMap[ o.getID() ]) ) );

  mDontOptimize = __o.mDontOptimize;
};


/*
  checkDontOptimize checks whether an instruction must not be modified.

  If this instruction must not be modified, checkDontOptimize asserts.
*/
void WIR_Instruction::checkDontOptimize( void ) const
{
  DSTART( "void WIR_Instruction::checkDontOptimize() const" );

  ufAssertT(
    !getDontOptimize(),
    "Illegal attempt to modify an instruction that is set as " <<
    "'don't optimize'!" );
};


/*
  Upon insertion of an operation into an instruction, checkVREGs verifies that
  no virtual register occuring in the operation belongs to a function different
  than that owning this instruction.

  If a virtual register belongs to a different function, checkVREGs asserts.
  Furthermore, checkVREGs also directly invalidates the symbol table if the
  current basic block is actually inserted into some %WIR system. Although this
  symbol table invalidation is also explicitly done by method invalidateSymbols
  below, we also do it in checkVREGs for efficiency reasons.
*/
void WIR_Instruction::checkVREGs( const WIR_Operation &o ) const
{
  DSTART( "void WIR_Instruction::checkVREGs(const WIR_Operation&) const" );

  (void) o;

  if ( isInserted() ) {
    WIR_BasicBlock &b = getBasicBlock();

    if ( b.isInserted() ) {
      WIR_Function &f = b.getFunction();

      #ifdef FAILSAFEMODE
      WIR_VirtualRegisterSet vregs = o.getVREGs();
      for ( WIR_VirtualRegister &r : vregs )
        ufAssertT(
          !r.isInserted() || ( r.isInserted() && ( r.getFunction() == f ) ),
          "Illegal attempt to insert an operation into an instruction owned " <<
          "by function '" << f.getName() << "' (ID " << f.getID() <<
          ") where virtual register '" << r.getName() <<
          "' belongs to function '" << r.getFunction().getName() << "' (ID " <<
          r.getFunction().getID() << ")!" );
      #endif

      if ( f.isInserted() ) {
        WIR_CompilationUnit &c = f.getCompilationUnit();
        if ( c.isInserted() ) {
          WIR_System &s = c.getSystem();
          s.invalidateSymbols();
        }
      }
    }
  }
};


/*
  invalidateSymbols marks the information of a system's global symbol table as
  invalid.

  invalidateSymbols only invalidates the symbol table if the current instruction
  is actually inserted into some WIR system.
*/
void WIR_Instruction::invalidateSymbols( void ) const
{
  DSTART( "void WIR_Instruction::invalidateSymbols() const" );

  if ( isInserted() ) {
    WIR_BasicBlock &b = getBasicBlock();
    if ( b.isInserted() ) {
      WIR_Function &f = b.getFunction();
      if ( f.isInserted() ) {
        WIR_CompilationUnit &c = f.getCompilationUnit();
        if ( c.isInserted() ) {
          WIR_System &s = c.getSystem();
          s.invalidateSymbols();
        }
      }
    }
  }
};

}       // namespace WIR
