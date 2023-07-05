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
  @file wirjumpcorrection.cc
  @brief This file implements a generic optimimzation detecting and correcting
         jump instructions with too large displacements.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include boost headers
#include <boost/current_function.hpp>

// Include libuseful headers
#include <libuseful/debugmacros.h>
#include <libuseful/io.h>

// Include WIR headers
#include <wir/wir.h>
#include <optimizations/jumpcorrection/wirjumpcorrection.h>


//
// Preprocessor macros
//

// #define ENABLE_INVARIANTS

#ifdef ENABLE_INVARIANTS

// This invariant verifies that the memory addresses as determined by the full
// WIR memory layout and by the internal light-weight data structure mMemLayout
// are the same for a WIR basic block.
#define DEBUG_MEMORYLAYOUT_INVARIANT( __b )                                    \
  {                                                                            \
    DSTART( "WIR_JumpCorrection.invariants" );                                 \
    DACTION(                                                                   \
      DOUT(                                                                    \
        "Checking memory layout invariant for block '" << __b.getName() <<     \
        "' (0x" << hex << mBBPosition.at( __b.getID() )->address << ")." <<    \
        endl );                                                                \
      auto &__sym = mSystem.findSymbol( __b );                                 \
      bool __memLayoutInvariant =                                              \
        ( __sym.getBaseAddress().getContent() ==                               \
            mBBPosition.at( __b.getID() )->address );                          \
      if ( !__memLayoutInvariant )                                             \
        DOUT(                                                                  \
          "Memory Layout Invariant failed for basic block '" <<                \
          __b.getName() << "': memory layout = 0x" << hex <<                   \
          __sym.getBaseAddress().getContent() << ", mMemLayout = 0x" <<        \
          mBBPosition.at( __b.getID() )->address << endl );                    \
      ufAssert( __memLayoutInvariant );                                        \
    );                                                                         \
  }

#else

#define DEBUG_MEMORYLAYOUT_INVARIANT(...)

#endif


//
// Code section
//

namespace WIR {


using namespace std;


//
// Public class methods
//

/*
  Default constructor for system-level optimization.
*/
WIR_JumpCorrection::WIR_JumpCorrection( WIR_System &s, bool warn ) :
  WIR_Optimization { s },
  mSystem { s },
  mWarn { warn },
  mPhysicalWIR { true },
  mSinglePass { false },
  mCorrectedJumps { 0 }
{
  DSTART( "WIR_JumpCorrection::WIR_JumpCorrection(WIR_System&, bool)" );
};


/*
  Destructor.
*/
WIR_JumpCorrection::~WIR_JumpCorrection( void )
{
  DSTART( "virtual WIR_JumpCorrection::~WIR_JumpCorrection()" );
};


/*
  setPhysicalWIR sets whether jump correction is applied to physical or virtual
  WIR code.
*/
void WIR_JumpCorrection::setPhysicalWIR( bool b )
{
  DSTART( "void WIR_JumpCorrection::setPhysicalWIR(bool)" );

  mPhysicalWIR = b;
};


/*
  getPhysicalWIR returns whether jump correction is applied to physical or
  virtual WIR code.
*/
bool WIR_JumpCorrection::getPhysicalWIR( void ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  return( mPhysicalWIR );
};


/*
  setSinglePass sets whether jump correction is applied once or if it executes
  until a fixed point is reached.
*/
void WIR_JumpCorrection::setSinglePass( bool b )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  mSinglePass = b;
};


/*
  getSinglePass returns whether jump correction is applied once or if it
  executes until a fixed point is reached.
*/
bool WIR_JumpCorrection::getSinglePass( void ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  return( mSinglePass );
};


//
// Protected class methods
//

WIR_JumpCorrection::MemLayoutInfo::MemLayoutInfo( const WIR_BasicBlock &b,
                                                  unsigned long long adr ) :
  block { &b },
  address { adr },
  alignment { 0 },
  fixedAddress { false }
{
  DSTART(
    "WIR_JumpCorrection::MemLayoutInfo::MemLayoutInfo(const WIR_BasicBlock&, long long unsigned int)" );
};


/*
  runOptimization performs jump correction in the given system.
*/
void WIR_JumpCorrection::runOptimization( WIR_System &s )
{
  DSTART( "virtual void WIR_JumpCorrection::runOptimization(WIR_System&)" );

  do {
    mCorrectedJumps = 0;

    for ( WIR_CompilationUnit &c : s )
      runOptimization( c );
  } while ( ( mCorrectedJumps != 0 ) && !mSinglePass );
};


/*
  runOptimization performs jump correction in the given compilation unit.
*/
void WIR_JumpCorrection::runOptimization( WIR_CompilationUnit &c )
{
  DSTART(
    "virtual void WIR_JumpCorrection::runOptimization(WIR_CompilationUnit&)" );

  for ( WIR_Function &f : c ) {
    runOptimization( f );

    // Cleanup the current function.
    WIR_EmptyBlocks eb( f );
    eb.optimize();

    WIR_UnreachableBlocks ub( f );
    ub.optimize();

    WIR_RedundantBlocks rb( f );
    rb.optimize();
  }
};


/*
  initializeMemoryLayout sets up the jump correction's internal data structures
  representing the system's memory layout.

  This method is virtual and thus can be overloaded if required for
  processor-specific initializations.
*/
void WIR_JumpCorrection::initializeMemoryLayout( void )
{
  DSTART( "void WIR_JumpCorrection::initializeMemoryLayout()" );

  // Step 1: Build a sequence of basic blocks ordered by the start addresses and
  //         collect the block's memory regions.

  // regionOfBB maps the ID of a basic block to its physical memory region.
  map<WIR_id_t, WIR_MemoryRegion *> regionOfBB;

  // sortedBBs stores all basic blocks of the WIR system, sorted ascending by
  // their start addresses in memory.
  multimap<pair<unsigned long long, unsigned long long>,
           WIR_BasicBlock *> sortedBBs;
  unsigned long long bbCnt = 0;

  for ( WIR_CompilationUnit &c : mSystem )
    for ( WIR_Function &f : c )
      for ( WIR_BasicBlock &b : f ) {
        auto &sym = mSystem.findSymbol( b );
        sortedBBs.insert(
          { { sym.getBaseAddress().getContent(), bbCnt++ }, &b } );

        auto &r = sym.getSection().getRegion();
        regionOfBB.insert( { b.getID(), &r } );
      }

  // Step 2: Put all basic blocks in their region's lists in mMemLayout in
  //         sorted order.

  mMemLayout.clear();
  mBBPosition.clear();

  for ( auto &p : sortedBBs ) {
    WIR_BasicBlock &b = *(p.second);
    WIR_MemoryRegion &r = *(regionOfBB.at( b.getID() ));

    mMemLayout[ r.getID() ].push_back( { b, p.first.first } );
    DOUT(
      "Pushing back '" << b.getName() << "' with start address 0x" << hex <<
      p.first.first << dec << " in region '" << r.getName() << "'." << endl );

    mBBPosition[ b.getID() ] = std::prev( mMemLayout[ r.getID() ].end() );
  }

  // Step 3: Adjust the blocks' alignments according to the following rules:
  // A) Within each section:
  //    1) The very first basic block inside a section will not be aligned here,
  //       this case is handeled by rule B.2) below.
  //    2) Otherwise: The very first block of a function is aligned using
  //       getBlock().
  // B) Within each region:
  //    1) For a section with fixed start address, the start address of this
  //       section's very first basic block is fixed.
  //    2) When starting a completely fresh section inside a region, align the
  //       section's very first basic block using getAlignment().

  // nonEmptySections contains the IDs of all sections into which some basic
  // blocks have already been assembled. This is to support rule A.1) above.
  set<WIR_id_t> nonEmptySections;

  // firstBBOfSection contains a pointer to a section's very first basic block.
  // This is to support rules A.1) and B.1) above.
  map<WIR_id_t, WIR_BasicBlock *> firstBBOfSection;

  for ( WIR_CompilationUnit &c : mSystem )
    for ( WIR_Function &f : c )
      for ( WIR_BasicBlock &b : f ) {
        auto &sym = mSystem.findSymbol( b );
        auto &sec = sym.getSection();

        if ( !firstBBOfSection.count( sec.getID() ) ) {
          // Rule A.1): b is the first block of its respective section.
          firstBBOfSection.insert( { sec.getID(), &b } );

          DOUT(
            "Rule A.1) applies for block '" << b.getName() <<
            "' and section '" << sec.getName() << "'." << endl );
        } else

        if ( b == f.begin()->get() ) {
          // Rule A.2): b is the first block of its respective function.
          MemLayoutInfo &memInfo = *(mBBPosition[ b.getID() ]);
          memInfo.alignment = sec.getBlock();

          DOUT(
            "Rule A.2) applies for block '" << b.getName() <<
            "' and section '" << sec.getName() << "': Setting alignment to " <<
            mBBPosition[ b.getID() ]->alignment << " bits (getBlock())." <<
            endl );
        }
      }

  // Process sections with fixed offsets first, i.e., sections with VMA != LMA.
  for ( WIR_BaseProcessor &p : mSystem.getComponents<WIR_BaseProcessor>() )
    for ( WIR_Section &sec : p )
      if ( firstBBOfSection.count( sec.getID() ) && sec.isStartSet() ) {
        // Rule B.1): The address of the very first basic block of a section
        //            with fixed start address is also fixed.
        WIR_BasicBlock &b = *(firstBBOfSection.at( sec.getID() ));
        MemLayoutInfo &memInfo = *(mBBPosition[ b.getID() ]);
        memInfo.fixedAddress = true;

        DOUT(
          "Rule B.1) applies for block '" << b.getName() <<
          "' and section '" << sec.getName() <<
          "': Setting fixed address to " <<
          string(
            mBBPosition[ b.getID() ]->fixedAddress ? "true." : "false." ) <<
          endl );
      }

  for ( WIR_MemoryRegion &r : mSystem.getComponents<WIR_MemoryRegion>() ) {
    for ( WIR_Section &sec : r.getSections() )
      if ( firstBBOfSection.count( sec.getID() ) && !sec.isStartSet() ) {
        // Rule B.2): A completely fresh section is started inside a region.
        WIR_BasicBlock &b = *(firstBBOfSection.at( sec.getID() ));
        MemLayoutInfo &memInfo = *(mBBPosition[ b.getID() ]);
        memInfo.alignment = sec.getAlignment();

        DOUT(
          "Rule B.2) applies for block '" << b.getName() <<
          "' and section '" << sec.getName() << "': Setting alignment to " <<
          mBBPosition[ b.getID() ]->alignment << " bits (getAlignment())." <<
          endl );
      }
  }

  {
    DSTART( "WIR_JumpCorrection.invariants" );
    DACTION(
      unsigned int layoutCounter = 0;
      for ( auto &p : mMemLayout ) {
        list<MemLayoutInfo> &l = p.second;
        for ( auto &i : l ) {
          (void) i;
          ++layoutCounter;
          DEBUG_MEMORYLAYOUT_INVARIANT( (*(i.block)) );
        }
      }

      unsigned int posCounter = 0;
      for ( auto &p : mBBPosition ) {
        ++posCounter;
        ufAssert( p.first == p.second->block->getID() );
      }

      ufAssert( layoutCounter == posCounter );
    );
  }

  verifyMemoryLayout();
};


/*
  updateMemoryLayout incrementally updates the jump correction's internal memory
  layout data structures from the position of the specified basic block on in
  memory.

  This method is virtual and thus can be overloaded if required for
  processor-specific updates.
*/
void WIR_JumpCorrection::updateMemoryLayout( const WIR_BasicBlock &b )
{
  DSTART(
    "void WIR_JumpCorrection::updateMemoryLayout(const WIR_BasicBlock&)" );

  // Collect region and end address of the basic block where the updates shall
  // begin.
  auto memoryPos = mBBPosition.at( b.getID() );
  WIR_MemoryRegion &r = mSystem.findSymbol( b ).getSection().getRegion();
  unsigned long long prevBBEnd = memoryPos->address + b.getSize();

  DEBUG_MEMORYLAYOUT_INVARIANT( b );

  // Propagate updates to all basic blocks allocated after b in memory.
  ++memoryPos;
  while ( memoryPos != mMemLayout[ r.getID() ].end() ) {
    MemLayoutInfo &memInfo = *memoryPos;

    DOUT(
      "Updating basic block '" << memInfo.block->getName() <<
      "' at original address 0x" << hex << memInfo.address << "." << endl );

    // If the current basic block is nailed to a fixed memory address, we can
    // stop here since no further changes need to be propagated.
    if ( memInfo.fixedAddress ) {
      DOUT(
        "Terminating update since block has a fixed memory address." << endl );
      DEBUG_MEMORYLAYOUT_INVARIANT( (*(memInfo.block)) );
      break;
    }

    // Otherwise the new start address of the current basic block is the end
    // address of the previous one, under consideration of potential alignments.
    unsigned long long currentBBStart = prevBBEnd;
    DOUT( "End address of previous block: 0x" << hex << prevBBEnd << endl );

    // Check and correct alignment.
    if ( memInfo.alignment != 0 ) {
      unsigned long long alignment = 1 << memInfo.alignment;

      if ( currentBBStart & ( alignment - 1 ) ) {
        currentBBStart =
          ( ( currentBBStart + alignment - 1 ) & ~( alignment - 1 ) );
        DOUT(
          "Applying alignment by " << memInfo.alignment <<
          " bits, resulting start address is 0x" << hex << currentBBStart <<
          "." << endl );
      }
    }

    // Update memory layout if updates apply. Otherwise, simply stop if there
    // are no changes.
    if ( currentBBStart != memInfo.address ) {
      memInfo.address = currentBBStart;
      prevBBEnd = currentBBStart + memInfo.block->getSize();
      DOUT(
        "Setting new start address of block '" << memInfo.block->getName() <<
        "' to 0x" << hex << memInfo.address << "." << endl );
      DEBUG_MEMORYLAYOUT_INVARIANT( (*(memInfo.block)) );
      ++memoryPos;
    } else {
      DOUT(
        "Terminating update since block address does not change." << endl );
      DEBUG_MEMORYLAYOUT_INVARIANT( (*(memInfo.block)) );
      break;
    }
  }

  verifyMemoryLayout();
};


/*
  updateMemoryLayout incrementally updates the jump correction's internal memory
  layout data structures for the complete specified function.
*/
void WIR_JumpCorrection::updateMemoryLayout( const WIR_Function &f )
{
  DSTART( "void WIR_JumpCorrection::updateMemoryLayout(const WIR_Function&)" );

  for ( WIR_BasicBlock &b : f )
    updateMemoryLayout( b );
};


/*
  verifyMemoryLayout verifies that the jump correction's internal memory layout
  is fully coherent with the overall system's memory layout.
*/
void WIR_JumpCorrection::verifyMemoryLayout( void ) const
{
  DSTART( "void WIR_JumpCorrection::verifyMemoryLayout() const" );

  DACTION(
    doMemoryLayoutVerification() );
};


/*
  isPhysicalSuccessor determines whether one basic blocks is a direct successor
  of another basic block in the address space.
*/
bool WIR_JumpCorrection::isPhysicalSuccessor( const WIR_BasicBlock &b1,
                                              const WIR_BasicBlock &b2 ) const
{
  DSTART(
    "bool WIR_JumpCorrection::isPhysicalSuccessor(const WIR_BasicBlock&, const WIR_BasicBlock&) const" );

  // Collect positions of both basic blocks in the memory layout.
  auto memoryPos1 = mBBPosition.at( b1.getID() );
  auto memoryPos2 = mBBPosition.at( b2.getID() );

  // Determine start addresses of both blocks.
  auto start1 = memoryPos1->address;
  auto start2 = memoryPos2->address;

  DOUT(
    "b1 = '" << b1.getName() << "' (start: 0x" << hex << start1 << ", size: 0x" <<
    b1.getSize() << ")" << endl <<
    "b2 = '" << b2.getName() << "' (start: 0x" << start2 << ")" << endl <<
    "  0x" << start2 << " == 0x" << start1 << " + 0x" << b1.getSize() << dec <<
    "? " << string( start2 == start1 + b1.getSize() ? "true" : "false" ) <<
    endl );

  return( start2 == start1 + b1.getSize() );
};


//
// Protected class methods
//

/*
  doMemoryLayoutVerification performs the actual verification of the jump
  correction's internal memory layout.
*/
void WIR_JumpCorrection::doMemoryLayoutVerification( void ) const
{
  DSTART( "void WIR_JumpCorrection::doMemoryLayoutVerification() const" );

  // Step 1: Build a sequence of basic blocks ordered by the start addresses and
  //         collect the block's memory regions.
  map<WIR_id_t, WIR_MemoryRegion *> regionOfBB;

  // sortedBBs stores all basic blocks of the WIR system, sorted ascending by
  // their start addresses in memory.
  multimap<pair<unsigned long long, unsigned long long>,
           WIR_BasicBlock *> sortedBBs;

  map<WIR_id_t, std::list<MemLayoutInfo>::const_iterator> regionIterator;
  unsigned long long bbCnt = 0;

  for ( WIR_CompilationUnit &c : mSystem )
    for ( WIR_Function &f : c )
      for ( WIR_BasicBlock &b : f ) {
        auto &sym = mSystem.findSymbol( b );
        sortedBBs.insert(
          { { sym.getBaseAddress().getContent(), bbCnt++ }, &b } );

        auto &r = sym.getSection().getRegion();
        regionOfBB.insert( { b.getID(), &r } );
        regionIterator[ r.getID() ] = mMemLayout.at( r.getID() ).begin();
      }

  // Step 2: Verify all basic blocks in their region's lists in mMemLayout in
  //         sorted order.
  ufAssert( regionIterator.size() == mMemLayout.size() );

  for ( auto &p : sortedBBs ) {
    WIR_BasicBlock &b = *(p.second);
    WIR_MemoryRegion &r = *(regionOfBB.at( b.getID() ));

    ufAssert( regionIterator[ r.getID() ] != mMemLayout.at( r.getID() ).end() );

    auto &memInfo = *(regionIterator[ r.getID() ]);
    ufAssertT(
      memInfo.block == &b,
      "Expected pointer to basic block '" << b.getName() << "' (ID " <<
      b.getID() << ", addr " << hex << &b << "), found pointer to addr " <<
      memInfo.block << dec << "." );
    ufAssert( memInfo.address == p.first.first );

    ufAssert( mBBPosition.count( b.getID() ) );
    ufAssert( &memInfo == &(*(mBBPosition.at( b.getID() ))) );

    ++( regionIterator[ r.getID() ] );
  }

  for ( auto &p : regionIterator ) {
    auto rID = p.first;
    auto rIt = p.second;

    ufAssert( rIt == mMemLayout.at( rID ).end() );
  }

  ufAssert( mBBPosition.size() == sortedBBs.size() );
};

}       // namespace WIR
