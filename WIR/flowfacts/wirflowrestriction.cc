/*

   This source file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Intermediate Representation Framework

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
  @file wirflowrestriction.cc
  @brief This file implements %WIR flow restrictions.

  @author Til Mauersberger <Til.Mauersberger@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include standard headers
#include <algorithm>

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
  Default constructor creating an empty flow restriction.

  The individual summands can be added using methods
  - addToLeq
  - addToGeq

  This flow fact is not automatically inserted into a WIR system.
*/
WIR_FlowRestriction::WIR_FlowRestriction( void ) :
  WIR_FlowFact {}
{
  DSTART( "WIR_FlowRestriction::WIR_FlowRestriction()" );
};


/*
  Constructor creating a non-empty flow restriction.

  This flow fact is not automatically inserted into a WIR system.
*/
WIR_FlowRestriction::WIR_FlowRestriction( const std::list<std::pair<int, std::reference_wrapper<const WIR_BasicBlock>>> &leq,
                                          const std::list<std::pair<int, std::reference_wrapper<const WIR_BasicBlock>>> &geq ) :
  WIR_FlowFact {},
  mLEQ { leq },
  mGEQ { geq }
{
  DSTART(
    "WIR_FlowRestriction::WIR_FlowRestriction(const list<pair<int, "
    "reference_wrapper<const WIR_BasicBlock> > >&, const list<pair<int, "
    "reference_wrapper<const WIR_BasicBlock> > >&)" );

  // Check data for validity.
  for ( const auto &summand : leq )
    if ( summand.first < 0 ) {
      mLEQ.clear();
      mGEQ.clear();

      ufWarnMsg << ufFile()
                << "Invalid factor " << summand.first << " found for basic "
                << "block '" + summand.second.get().getName() + "',"
                << " clearing entire flow restriction." << endl;
      return;
    }

  for ( const auto &summand : geq )
    if ( summand.first < 0 ) {
      mLEQ.clear();
      mGEQ.clear();

      ufWarnMsg << ufFile()
                << "Invalid factor " << summand.first << " found for basic "
                << "block '" + summand.second.get().getName() + "',"
                << " clearing entire flow restriction." << endl;
      return;
    }
};


/*
  Constructor creating a non-empty flow restriction.

  This flow fact is not automatically inserted into a WIR system.
*/
WIR_FlowRestriction::WIR_FlowRestriction( std::list<std::pair<int, std::reference_wrapper<const WIR_BasicBlock>>> &&leq,
                                          std::list<std::pair<int, std::reference_wrapper<const WIR_BasicBlock>>> &&geq ) :
  WIR_FlowFact {},
  mLEQ { move( leq ) },
  mGEQ { move( geq ) }
{
  DSTART(
    "WIR_FlowRestriction::WIR_FlowRestriction(list<pair<int, "
    "reference_wrapper<const WIR_BasicBlock> > >&&, list<pair<int, "
    "reference_wrapper<const WIR_BasicBlock> > >&&)" );

  // Check data for validity.
  for ( const auto &summand : mLEQ )
    if ( summand.first < 0 ) {
      mLEQ.clear();
      mGEQ.clear();

      ufWarnMsg << ufFile()
                << "Invalid factor " << summand.first << " found for basic "
                << "block '" + summand.second.get().getName() + "',"
                << " clearing entire flow restriction." << endl;
      return;
    }

  for ( const auto &summand : mGEQ )
    if ( summand.first < 0 ) {
      mLEQ.clear();
      mGEQ.clear();

      ufWarnMsg << ufFile()
                << "Invalid factor " << summand.first << " found for basic "
                << "block '" + summand.second.get().getName() + "',"
                << " clearing entire flow restriction." << endl;
      return;
    }
};


/*
  Copy constructor.

  The copy will not be inserted in any WIR_System or referenced by any
  WIR_FlowFactRefs.
*/
WIR_FlowRestriction::WIR_FlowRestriction( const WIR_FlowRestriction &__o ) :
  WIR_FlowFact { __o },
  mLEQ { __o.mLEQ },
  mGEQ { __o.mGEQ }
{
  DSTART(
    "WIR_FlowRestriction::WIR_FlowRestriction(const WIR_FlowRestriction&)" );
};


/*
  Destructor.
*/
WIR_FlowRestriction::~WIR_FlowRestriction( void )
{
  DSTART( "virtual WIR_FlowRestriction::~WIR_FlowRestriction()" );

  if ( !isInserted() )
    return;

  // Remove this flow restriction from all relevant FlowfactRefs.
  auto removeRef = [this]( pair<int,
                                reference_wrapper<const WIR_BasicBlock>> &s ) {
    eraseReference( s.second );
  };

  for_each( mLEQ.begin(), mLEQ.end(), removeRef );
  for_each( mGEQ.begin(), mGEQ.end(), removeRef );
};


/*
  Copy-assignment operator.

  The copy will not be inserted in any WIR_System or referenced by any
  WIR_FlowFactRefs.
*/
WIR_FlowRestriction & WIR_FlowRestriction::operator = ( const WIR_FlowRestriction &__o )
{
  DSTART(
    "WIR_FlowRestriction& WIR_FlowRestriction::operator=(const "
    "WIR_FlowRestriction&)" );

  WIR_FlowFact::operator = ( __o );

  mLEQ = __o.mLEQ;
  mGEQ = __o.mGEQ;

  return( *this );
};


/*
  getType returns the type of a %WIR flow fact, i.e., whether it is an entry
  point, flow restriction or loop bound.
*/
WIR_FlowFactType WIR_FlowRestriction::getType( void ) const
{
  DSTART( "virtual WIR_FlowFactType WIR_FlowRestriction::getType() const" );

  return( WIR_FlowFactType::flowrestriction );
};


/*
  The << operator dumps a WIR flow restriction to an output stream.
*/
std::ostream & operator << ( std::ostream &os, const WIR_FlowRestriction &f )
{
  DSTART( "ostream& operator<<(ostream&, const WIR_FlowRestriction&)" );

  // Write preamble.
  os << "Flow restriction: ";

  // Write leq-side.
  for ( auto it = f.mLEQ.begin(); it != f.mLEQ.end(); ++it ) {
    if ( it != f.mLEQ.begin() )
      os << " + ";
    os << it->first << " * " << it->second.get().getName();
  }

  // Write comparator.
  os << " <= ";

  // Write geq-side.
  for ( auto it = f.mGEQ.begin(); it != f.mGEQ.end(); ++it ) {
    if ( it != f.mGEQ.begin() )
      os << " + ";
    os << it->first << " * " << it->second.get().getName();
  }

  // Flush output stream.
  os << endl;

  return( os );
};


/*
  addToLeq adds a summand to a flow restriction's less-equal side.

  The basic block with the given integer factor (negative factors add the
  summand to the geq-side) is added to the leq side. If factor is zero, nothing
  changes internally and the summand will not be regarded.

  This is the only method manipulating the internal data structures (and by this
  invokes the update mechanisms of class FlowFactRef).
*/
void WIR_FlowRestriction::addToLeq( int factor, const WIR_BasicBlock &b )
{
  DSTART( "void WIR_FlowRestriction::addToLeq(int, const WIR_BasicBlock&)" );

  // Check data for validity.
  if ( factor == 0 ) {
    ufWarnMsg << ufFile()
              << "Invalid attempt to add basic block '" + b.getName() + "'"
              << " with factor " << dec << factor << " to flow restriction, "
              << "ignoring." << endl;
    return;
  }

  DOUT( "BB ID is: " << b.getID() << endl );

  for ( auto leq_it = mLEQ.begin(); leq_it != mLEQ.end(); ++leq_it ) {
    auto &[leq_factor, leq_bb] = *leq_it;

    if ( b == leq_bb ) {
      DOUT( "Found BB " << b.getID() << " on leq-side." << endl );

      // The given bb was found, now change its factor.
      leq_factor += factor;

      // Analyze situation.
      if ( leq_factor < 0 ) {
        mGEQ.push_back( make_pair( -leq_factor, leq_bb ) );
        mLEQ.erase( leq_it );
      } else

      if ( leq_factor == 0 ) {
        eraseReference( b );
        mLEQ.erase( leq_it );
      }

      return;
    }
  }

  for ( auto geq_it = mGEQ.begin(); geq_it != mGEQ.end(); ++geq_it ) {
    auto &[geq_factor, geq_bb] = *geq_it;

    if ( b == geq_bb ) {
      DOUT( "Found BB " << b.getID() << " on geq-side." << endl );

      // The given bb was found, now change its factor.
      geq_factor -= factor;

      // Analyze situation.
      if ( geq_factor < 0 ) {
        mLEQ.push_back( make_pair( -geq_factor, geq_bb ) );
        mGEQ.erase( geq_it );
      } else

      if ( geq_factor == 0 ) {
        eraseReference( b );
        mGEQ.erase( geq_it );
      }

      return;
    }
  }

  // Basic block was not found in lists, add new summand to this flow
  // restriction.
  if ( factor > 0 ) {
    DOUT(
      "Creating new pair on leq-side: " << factor << " , " << b.getID() <<
      endl );
    mLEQ.push_back( make_pair( factor, cref( b ) ) );
  } else {
    // Negative factor means to add to geq-side.
    DOUT(
      "Creating new pair on geq-side: " << -factor << " , " << b.getID() <<
      endl );
    mGEQ.push_back( make_pair( -factor, cref( b ) ) );
  }

  addReference( b );
};


/*
  addToGeq adds a summand to a flow restriction's greater-equal side.

  The basic block with the given integer as factor (negative factors will add
  the summand to the leq-side) is added to the geq side.
*/
void WIR_FlowRestriction::addToGeq( int factor, const WIR_BasicBlock &b )
{
  DSTART( "void WIR_FlowRestriction::addToGeq(int, const WIR_BasicBlock&)" );

  addToLeq( -factor, b );
};


/*
  eraseSummand erases a summand from a flow restriction.

  A whole summand is removed from a flow restriction.
*/
int WIR_FlowRestriction::eraseSummand( const WIR_BasicBlock &b )
{
  DSTART( "int WIR_FlowRestriction::eraseSummand(const WIR_BasicBlock&)" );

  // Iterate leq-side.
  for ( const auto &[factor, leq_bb] : mLEQ )
    // Check if bb is found.
    if ( b == leq_bb ) {
      int ret = factor;

      // Negate factor to remove the whole summand.
      addToLeq( -factor, leq_bb );
      return( ret );
    }

  // Iterate geq-side.
  for ( const auto &[factor, geq_bb] : mGEQ )
    // Check if bb is found.
    if ( b == geq_bb ) {
      int ret = factor;

      // Negate factor to remove the whole summand.
      addToGeq( -factor, geq_bb );
      return( ret );
    }

  return( 0 );
};


/*
  replaceSummand replaces a summand in a flow restriction.

  The factor of the summand remains unchanged (no differentiation between leq-
  and geq-side).
*/
void WIR_FlowRestriction::replaceSummand( const WIR_BasicBlock &bOld,
                                          const WIR_BasicBlock &bNew )
{
  DSTART(
    "void WIR_FlowRestriction::replaceSummand(const WIR_BasicBlock&, "
    "const WIR_BasicBlock&)" );

  // Remember from which side of the inequation the summand was.
  bool wasLeq = isPartOfLeq( bOld );

  // Erase old summand, remember its factor.
  int factor = eraseSummand( bOld );

  DOUT(
    "The BB to exchange was on " << ( wasLeq ? 'l' : 'g' ) << "eq-side" <<
    " with factor " << factor << "." << endl );

  // Add new summand.
  if ( wasLeq )
    addToLeq( factor, bNew );
  else
    addToGeq( factor, bNew );

  return;
};


/*
  getLeq returns the less-equal side of a flow restriction.
*/
const list<pair<int, reference_wrapper<const WIR_BasicBlock>>> &WIR_FlowRestriction::getLeq( void ) const
{
  DSTART(
    "const list<pair<int, reference_wrapper<const WIR_BasicBlock> > >& "
    "WIR_FlowRestriction::getLeq() const" );

  return( mLEQ );
};


/*
  getGeq returns the greater-equal side of a flow restriction.
*/
const list<pair<int, reference_wrapper<const WIR_BasicBlock>>> &WIR_FlowRestriction::getGeq( void ) const
{
  DSTART(
    "const list<pair<int, reference_wrapper<const WIR_BasicBlock> > >& "
    "WIR_FlowRestriction::getGeq() const" );

  return( mGEQ );
};


/*
  isPartOfLeq tests whether a basic block belongs to the leq side.
*/
bool WIR_FlowRestriction::isPartOfLeq( const WIR_BasicBlock &b ) const
{
  DSTART(
    "bool WIR_FlowRestriction::isPartOfLeq(const WIR_BasicBlock&) const" );

  for ( const auto &summand : mLEQ )
    if ( b == summand.second )
      return( true );

  return( false );
};


/*
  isPartOfGeq tests whether a basic block belongs to the geq side.
*/
bool WIR_FlowRestriction::isPartOfGeq( const WIR_BasicBlock &b ) const
{
  DSTART(
    "bool WIR_FlowRestriction::isPartOfGeq(const WIR_BasicBlock&) const" );

  for ( const auto &summand : mGEQ )
    if ( b == summand.second )
      return( true );

  return( false );
};


/*
  isPartOfFlowFact tests whether a basic block is part of this flow restriction.
*/
bool WIR_FlowRestriction::isPartOfFlowFact( const WIR_BasicBlock &b ) const
{
  DSTART(
    "bool WIR_FlowRestriction::isPartOfFlowFact(const WIR_BasicBlock&) const" );

  return( isPartOfLeq( b ) || isPartOfGeq( b ) );
};


/*
  isSignificant returns whether a flow restriction is significant for WCET
  calculation or not.

  In some cases, a flow fact may not be significant for WCET calculation, e.g.:
  - A flow restriction with 0 <= SUM.
  In such cases, this method returns false, in all other cases true.
*/
bool WIR_FlowRestriction::isSignificant( void ) const
{
  DSTART( "virtual bool WIR_FlowRestriction::isSignificant() const" );

  // If the left (leq) side is empty, this has no significant information,
  // because 0 <= SUM is always true for positive factors and positive decision
  // variables.
  return( !mLEQ.empty() );
};


/*
  isEqual returns true iff this flow restriction is equal to the specified one.

  Two flow restrictions r and r' are equal iff
    - each summand of the leq-side of r is equal to the summand of r' at
      the same position of the leq-side of r', and
    - each summand of the geq-side of r is equal to the summand of r' at
      the same position of the geq-side of r', and
  Two summands s and s' are equal iff
    - they both have the same factor, and
    - the LABELS of the referenced basic block are identical.   (!)
*/
bool WIR_FlowRestriction::isEqual( const WIR_FlowRestriction &__o ) const
{
  DSTART(
    "bool WIR_FlowRestriction::isEqual(const WIR_FlowRestriction&) const" );

  // Create a comparator for summands according to the requirements.
  using Summand =
    std::pair<int, std::reference_wrapper<const WIR::WIR_BasicBlock>>;

  auto cmpSummands = []( const Summand &s1, const Summand &s2 ) -> bool {
    // Compare factors first.
    if ( s1.first != s2.first )
      return( false );

    // Compare basic block labels next.
    if ( s1.second.get().getName() != s2.second.get().getName() )
      return( false );

    return( true );
  };

  // std::equal returns false for different lengths of the input ranges, it thus
  // does the job of comparing our two lists perfectly.
  return (
    equal(
      mLEQ.begin(), mLEQ.end(), __o.mLEQ.begin(), __o.mLEQ.end(),
      cmpSummands ) &&
    equal(
      mGEQ.begin(), mGEQ.end(), __o.mGEQ.begin(), __o.mGEQ.end(),
      cmpSummands ) );
};


/*
  reorganize adjusts all references to WIR basic blocks stored by a flow fact
  after a deep copy of flow facts.
*/
void WIR_FlowRestriction::reorganize( const std::map<WIR_id_t, WIR_BasicBlock *> &blockIDMap )
{
  DSTART(
    "virtual void WIR_FlowRestriction::reorganize(const map<long long unsigned "
    "int, WIR_BasicBlock*>&)" );

  auto exchangeBBReferences = [this, &blockIDMap]( auto &summand ) -> void {
    // Look for current basic block in the map.
    auto block_it = blockIDMap.find( summand.second.get().getID() );
    if ( block_it == blockIDMap.end() )
      return;

    // Change the reference to the new basic block provided by the map.
    summand.second = cref( *(block_it->second) );
  };

  for_each( mLEQ.begin(), mLEQ.end(), exchangeBBReferences );
  for_each( mGEQ.begin(), mGEQ.end(), exchangeBBReferences );
};


//
// Protected class methods
//

/*
  onInsert is called whenever this flow restriction is added to a WIR_System.

  This method overrides the method defined by the base class to add references
  of itself to all basic block's FlowFactRefs that this flow restriction is made
  up of.
*/
void WIR_FlowRestriction::onInsert( WIR_System *s )
{
  DSTART( "virtual void WIR_FlowRestriction::onInsert(WIR_System*)" );

  // Call base class implementation to set the pointer.
  WIR_FlowFact::onInsert( s );

  auto insertRef = [this]( const auto &s ) {
    addReference( s.second );
  };

  for_each( mLEQ.begin(), mLEQ.end(), insertRef );
  for_each( mGEQ.begin(), mGEQ.end(), insertRef );
};


/*
  clone creates a copy of a WIR flow restriction.

  This method only calls the copy constructor and allocates a new WIR flow
  restriction on the heap.
*/
WIR_FlowFact *WIR_FlowRestriction::clone( void ) const
{
  DSTART( "virtual WIR_FlowFact* WIR_FlowRestriction::clone() const" );

  return( new WIR_FlowRestriction( *this ) );
};

}      // namespace WIR
