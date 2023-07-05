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
  @file wirstats.cc
  @brief This file implements %WIR code statistics.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include standard headers
#include <iostream>
#include <sstream>
#include <string>

// Include boost headers
#include <boost/current_function.hpp>

// Include libuseful headers
#include <libuseful/debugmacros.h>

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
  Default constructor creating an empty statistics.
*/
WIR_Stats::WIR_Stats( void ) :
  mIndent { 0 }
{
  DSTART( "WIR_Stats::WIR_Stats()" );
};


/*
  Default constructor for system-level statistics.

  The WIR objects counted in o during construction serve as first reference
  point.
*/
WIR_Stats::WIR_Stats( const WIR_System &o )
{
  DSTART( "WIR_Stats::WIR_Stats(const WIR_System&)" );

  count(
    o, mSystems1, mCompilationUnits1, mDataObjects1, mFunctions1, mBasicBlocks1,
    mInstructions1, mOperations1, mParameters1, mRegisters1 );
};


/*
  Default constructor for compilation unit-level analysis.

  The WIR objects counted in o during construction serve as first reference
  point.
*/
WIR_Stats::WIR_Stats( const WIR_CompilationUnit &o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  count(
    o, mCompilationUnits1, mDataObjects1, mFunctions1, mBasicBlocks1,
    mInstructions1, mOperations1, mParameters1, mRegisters1 );
};


/*
  Default constructor for function-level analysis.

  The WIR objects counted in o during construction serve as first reference
  point.
*/
WIR_Stats::WIR_Stats( const WIR_Function &o )
{
  DSTART( "WIR_Stats::WIR_Stats(const WIR_Function&)" );

  count(
    o, mFunctions1, mBasicBlocks1, mInstructions1, mOperations1, mParameters1,
    mRegisters1 );
};


/*
  Default constructor for basic block-level analysis.

  The WIR objects counted in o during construction serve as first reference
  point.
*/
WIR_Stats::WIR_Stats( const WIR_BasicBlock &o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  count(
    o, mBasicBlocks1, mInstructions1, mOperations1, mParameters1, mRegisters1 );
};


/*
  Default constructor for instruction-level analysis.

  The WIR objects counted in o during construction serve as first reference
  point.
*/
WIR_Stats::WIR_Stats( const WIR_Instruction &o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  count( o, mInstructions1, mOperations1, mParameters1, mRegisters1 );
};


/*
  Default constructor for operation-level analysis.

  The WIR objects counted in o during construction serve as first reference
  point.
*/
WIR_Stats::WIR_Stats( const WIR_Operation &o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  count( o, mOperations1, mParameters1, mRegisters1 );
};


/*
  Copy constructor.
*/
WIR_Stats::WIR_Stats( const WIR_Stats &__o ) :
  mSystems1 { __o.mSystems1 },
  mCompilationUnits1 { __o.mCompilationUnits1 },
  mFunctions1 { __o.mFunctions1 },
  mBasicBlocks1 { __o.mBasicBlocks1 },
  mInstructions1 { __o.mInstructions1 },
  mOperations1 { __o.mOperations1 },
  mParameters1 { __o.mParameters1 },
  mRegisters1 { __o.mRegisters1 },
  mDataObjects1 { __o.mDataObjects1 },
  mSystems2 { __o.mSystems2 },
  mCompilationUnits2 { __o.mCompilationUnits2 },
  mFunctions2 { __o.mFunctions2 },
  mBasicBlocks2 { __o.mBasicBlocks2 },
  mInstructions2 { __o.mInstructions2 },
  mOperations2 { __o.mOperations2 },
  mParameters2 { __o.mParameters2 },
  mRegisters2 { __o.mRegisters2 },
  mDataObjects2 { __o.mDataObjects2 },
  mIndent { __o.mIndent }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Move constructor.
*/
WIR_Stats::WIR_Stats( WIR_Stats &&__o ) :
  mSystems1 { move( __o.mSystems1 ) },
  mCompilationUnits1 { move( __o.mCompilationUnits1 ) },
  mFunctions1 { move( __o.mFunctions1 ) },
  mBasicBlocks1 { move( __o.mBasicBlocks1 ) },
  mInstructions1 { move( __o.mInstructions1 ) },
  mOperations1 { move( __o.mOperations1 ) },
  mParameters1 { move( __o.mParameters1 ) },
  mRegisters1 { move( __o.mRegisters1 ) },
  mDataObjects1 { move( __o.mDataObjects1 ) },
  mSystems2 { move( __o.mSystems2 ) },
  mCompilationUnits2 { move( __o.mCompilationUnits2 ) },
  mFunctions2 { move( __o.mFunctions2 ) },
  mBasicBlocks2 { move( __o.mBasicBlocks2 ) },
  mInstructions2 { move( __o.mInstructions2 ) },
  mOperations2 { move( __o.mOperations2 ) },
  mParameters2 { move( __o.mParameters2 ) },
  mRegisters2 { move( __o.mRegisters2 ) },
  mDataObjects2 { move( __o.mDataObjects2 ) }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  __o.clear();
};


/*
  Destructor.
*/
WIR_Stats::~WIR_Stats( void )
{
  DSTART( "WIR_Stats::~WIR_Stats()" );
};


/*
  Copy-assignment operator.
*/
WIR_Stats & WIR_Stats::operator = ( const WIR_Stats &__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  mSystems1 = __o.mSystems1;
  mCompilationUnits1 = __o.mCompilationUnits1;
  mFunctions1 = __o.mFunctions1;
  mBasicBlocks1 = __o.mBasicBlocks1;
  mInstructions1 = __o.mInstructions1;
  mOperations1 = __o.mOperations1;
  mParameters1 = __o.mParameters1;
  mRegisters1 = __o.mRegisters1;
  mDataObjects1 = __o.mDataObjects1;
  mSystems2 = __o.mSystems2;
  mCompilationUnits2 = __o.mCompilationUnits2;
  mFunctions2 = __o.mFunctions2;
  mBasicBlocks2 = __o.mBasicBlocks2;
  mInstructions2 = __o.mInstructions2;
  mOperations2 = __o.mOperations2;
  mParameters2 = __o.mParameters2;
  mRegisters2 = __o.mRegisters2;
  mDataObjects2 = __o.mDataObjects2;
  mIndent = __o.mIndent;

  return( *this );
};


/*
  Move-assignment operator.
*/
WIR_Stats & WIR_Stats::operator = ( WIR_Stats &&__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  mSystems1 = move( __o.mSystems1 );
  mCompilationUnits1 = move( __o.mCompilationUnits1 );
  mFunctions1 = move( __o.mFunctions1 );
  mBasicBlocks1 = move( __o.mBasicBlocks1 );
  mInstructions1 = move( __o.mInstructions1 );
  mOperations1 = move( __o.mOperations1 );
  mParameters1 = move( __o.mParameters1 );
  mRegisters1 = move( __o.mRegisters1 );
  mDataObjects1 = move( __o.mDataObjects1 );
  mSystems2 = move( __o.mSystems2 );
  mCompilationUnits2 = move( __o.mCompilationUnits2 );
  mFunctions2 = move( __o.mFunctions2 );
  mBasicBlocks2 = move( __o.mBasicBlocks2 );
  mInstructions2 = move( __o.mInstructions2 );
  mOperations2 = move( __o.mOperations2 );
  mParameters2 = move( __o.mParameters2 );
  mRegisters2 = move( __o.mRegisters2 );
  mDataObjects2 = move( __o.mDataObjects2 );

  __o.clear();

  return( *this );
};


/*
  The == operator checks for equality of statistics.
*/
bool WIR_Stats::operator == ( const WIR_Stats &__o ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  return(
    ( mSystems1 == __o.mSystems1 ) &&
    ( mCompilationUnits1 == __o.mCompilationUnits1 ) &&
    ( mFunctions1 == __o.mFunctions1 ) &&
    ( mBasicBlocks1 == __o.mBasicBlocks1 ) &&
    ( mInstructions1 == __o.mInstructions1 ) &&
    ( mOperations1 == __o.mOperations1 ) &&
    ( mParameters1 == __o.mParameters1 ) &&
    ( mRegisters1 == __o.mRegisters1 ) &&
    ( mDataObjects1 == __o.mDataObjects1 ) &&
    ( mSystems2 == __o.mSystems2 ) &&
    ( mCompilationUnits2 == __o.mCompilationUnits2 ) &&
    ( mFunctions2 == __o.mFunctions2 ) &&
    ( mBasicBlocks2 == __o.mBasicBlocks2 ) &&
    ( mInstructions2 == __o.mInstructions2 ) &&
    ( mOperations2 == __o.mOperations2 ) &&
    ( mParameters2 == __o.mParameters2 ) &&
    ( mRegisters2 == __o.mRegisters2 ) &&
    ( mDataObjects2 == __o.mDataObjects2 ) );
};


/*
  The != operator checks for inequality of statistics.
*/
bool WIR_Stats::operator != ( const WIR_Stats &__o ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  return( !operator == ( __o ) );
};


/*
  count counts the numbers of WIR objects for system-level statistics.

  The WIR objects counted in o serve as second reference point.
*/
void WIR_Stats::count( const WIR_System &o )
{
  DSTART( "void WIR_Stats::count(const WIR_System&)" );

  clearSets2();
  count(
    o, mSystems2, mCompilationUnits2, mDataObjects2, mFunctions2, mBasicBlocks2,
    mInstructions2, mOperations2, mParameters2, mRegisters2 );
};


/*
  count counts the numbers of WIR objects for compilation unit-level statistics.

  The WIR objects counted in o serve as second reference point.
*/
void WIR_Stats::count( const WIR_CompilationUnit &o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  clearSets2();
  count(
    o, mCompilationUnits2, mDataObjects2, mFunctions2, mBasicBlocks2,
    mInstructions2, mOperations2, mParameters2, mRegisters2 );
};


/*
  count counts the numbers of WIR objects for function-level statistics.

  The WIR objects counted in o serve as second reference point.
*/
void WIR_Stats::count( const WIR_Function &o )
{
  DSTART( "void WIR_Stats::count(const WIR_Function&)" );

  clearSets2();
  count(
    o, mFunctions2, mBasicBlocks2, mInstructions2, mOperations2, mParameters2,
    mRegisters2 );
};


/*
  count counts the numbers of WIR objects for basic block-level statistics.

  The WIR objects counted in o serve as second reference point.
*/
void WIR_Stats::count( const WIR_BasicBlock &o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  clearSets2();
  count(
    o, mBasicBlocks2, mInstructions2, mOperations2, mParameters2, mRegisters2 );
};


/*
  count counts the numbers of WIR objects for instruction-level statistics.

  The WIR objects counted in o serve as second reference point.
*/
void WIR_Stats::count( const WIR_Instruction &o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  clearSets2();
  count( o, mInstructions2, mOperations2, mParameters2, mRegisters2 );
};


/*
  count counts the numbers of WIR objects for operation-level statistics.

  The WIR objects counted in o serve as second reference point.
*/
void WIR_Stats::count( const WIR_Operation &o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  clearSets2();
  count( o, mOperations2, mParameters2, mRegisters2 );
};


/*
  rotate associates the data collected for the second reference point with the
  first reference point.

  This way, WIR_Stats supports incremental counting of WIR objects over multiple
  code transformation steps.
*/
void WIR_Stats::rotate( void )
{
  DSTART( "void WIR_Stats::rotate()" );

  mSystems1 = move( mSystems2 );
  mCompilationUnits1 = move( mCompilationUnits2 );
  mFunctions1 = move( mFunctions2 );
  mBasicBlocks1 = move( mBasicBlocks2 );
  mInstructions1 = move( mInstructions2 );
  mOperations1 = move( mOperations2 );
  mParameters1 = move( mParameters2 );
  mRegisters1 = move( mRegisters2 );
  mDataObjects1 = move( mDataObjects2 );
};


/*
  clear removes all counted elements from the statistics.
*/
void WIR_Stats::clear( void )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  clearSets1();
  clearSets2();
};


/*
  empty returns whether a statistics is empty.
*/
bool WIR_Stats::empty( void ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  return(
    mSystems1.empty() && mCompilationUnits1.empty() && mFunctions1.empty() &&
    mBasicBlocks1.empty() && mInstructions1.empty() && mOperations1.empty() &&
    mParameters1.empty() && mRegisters1.empty() && mDataObjects1.empty() &&
    mSystems2.empty() && mCompilationUnits2.empty() && mFunctions2.empty() &&
    mBasicBlocks2.empty() && mInstructions2.empty() && mOperations2.empty() &&
    mParameters2.empty() && mRegisters2.empty() && mDataObjects2.empty() );
};


/*
  setIndentation sets the number of white spaces to be displayed at the very
  beginning of each new line when dumping a WIR statistics with the << operator.
*/
void WIR_Stats::setIndentation( unsigned int i )
{
  DSTART( "void WIR_Stats::setIndentation(unsigned int)" );

  mIndent = i;
};


/*
  The << operator dumps a WIR statistics to an output stream.

  If the statistics to be dumped contains only one reference point (i.e., the
  statistics objects has just been created using the above constructors), the
  absolute number of WIR objects counted by the constructors is dumped. If a
  second reference point has been counted by using some of the count methods
  above, the difference between the two reference points in terms of
  added/deleted WIR objects is dumped.
*/
std::ostream & operator << ( std::ostream &os, const WIR_Stats &s )
{
  DSTART( "ostream& operator<<(ostream&, const WIR_Stats&)" );
  const unsigned int rows = 9;

  // Check whether one or two reference points were captured.
  if ( s.mSystems2.empty() && s.mCompilationUnits2.empty() &&
       s.mFunctions2.empty() && s.mBasicBlocks2.empty() &&
       s.mInstructions2.empty() && s.mOperations2.empty() &&
       s.mParameters2.empty() && s.mRegisters2.empty() &&
       s.mDataObjects2.empty() ) {
    // Only one reference point.
    string labels[ rows ] = {
      "Systems: ", "Compilation Units: ", "Functions: ", "Basic Blocks: ",
      "Instructions: ", "Operations: ", "Parameters: ", "Registers: ",
      "Data objects: " };
    size_t val[ rows ] = {
      s.mSystems1.size(), s.mCompilationUnits1.size(), s.mFunctions1.size(),
      s.mBasicBlocks1.size(), s.mInstructions1.size(), s.mOperations1.size(),
      s.mParameters1.size(), s.mRegisters1.size(), s.mDataObjects1.size() };
    string str[ rows ];
    string::size_type width = 0;

    // Determine maximum width of column.
    for ( unsigned int i = 0; i < rows; ++i ) {
      stringstream stream;

      stream << val[ i ];
      str[ i ] = labels[ i ] + stream.str();

      if ( str[ i ].size() > width )
        width = str[ i ].size();
    }

    // Print formatted output.
    for ( unsigned int i = 0; i < rows; ++i )
      cout << labels[ i ] << string( width - str[ i ].size(), ' ' ) << val[ i ]
           << endl;
  } else {
    // Two reference points.
    string labels[ rows ][ 2 ] = {
      { "Added Systems: ", "Deleted Systems: " },
      { "Added Compilation Units: ", "Deleted Compilation Units: " },
      { "Added Functions: ", "Deleted Functions: " },
      { "Added Basic Blocks: ", "Deleted Basic Blocks: " },
      { "Added Instructions: ", "Deleted Instructions: " },
      { "Added Operations: ", "Deleted Operations: " },
      { "Added Parameters: ", "Deleted Parameters: " },
      { "Added Registers: ", "Deleted Registers: " },
      { "Added Data Objects: ", "Deleted Data Objects: " } };
    unsigned int val[ rows ][ 2 ] = {
      { s.diff( s.mSystems2, s.mSystems1 ),
        s.diff( s.mSystems1, s.mSystems2 ) },
      { s.diff( s.mCompilationUnits2, s.mCompilationUnits1 ),
        s.diff( s.mCompilationUnits1, s.mCompilationUnits2 ) },
      { s.diff( s.mFunctions2, s.mFunctions1 ),
        s.diff( s.mFunctions1, s.mFunctions2 ) },
      { s.diff( s.mBasicBlocks2, s.mBasicBlocks1 ),
        s.diff( s.mBasicBlocks1, s.mBasicBlocks2 ) },
      { s.diff( s.mInstructions2, s.mInstructions1 ),
        s.diff( s.mInstructions1, s.mInstructions2 ) },
      { s.diff( s.mOperations2, s.mOperations1 ),
        s.diff( s.mOperations1, s.mOperations2 ) },
      { s.diff( s.mParameters2, s.mParameters1 ),
        s.diff( s.mParameters1, s.mParameters2 ) },
      { s.diff( s.mRegisters2, s.mRegisters1 ),
        s.diff( s.mRegisters1, s.mRegisters2 ) },
      { s.diff( s.mDataObjects2, s.mDataObjects1 ),
        s.diff( s.mDataObjects1, s.mDataObjects2 ) } };
    string str[ rows ][ 2 ];
    string::size_type width[ 2 ] = { 0, 0 };

    // Determine maximum widths of columns.
    for ( unsigned int i = 0; i < rows; ++i )
      // Only consider rows with non-zero values.
      if ( ( val[ i ][ 0 ] != 0 ) || ( val[ i ][ 1 ] != 0 ) )
        for ( unsigned int j = 0; j < 2; ++j ) {
          stringstream stream;

          stream << val[ i ][ j ];
          str[ i ][ j ] = labels[ i ][ j ] + stream.str();

          if ( str[ i ][ j ].size() > width[ j ] )
            width[ j ] = str[ i ][ j ].size();
        }

    // Print formatted output.
    for ( unsigned int i = 0; i < rows; ++i )
      // Only consider rows with non-zero values.
      if ( ( val[ i ][ 0 ] != 0 ) || ( val[ i ][ 1 ] != 0 ) ) {
        cout << string( s.mIndent, ' ' );
        for ( unsigned int j = 0; j < 2; ++j ) {
          cout << labels[ i ][ j ]
              << string( width[ j ] - str[ i ][ j ].size(), ' ' )
              << val[ i ][ j ];

          if ( j != 1 )
            cout << "  ";
          else
            cout << endl;
        }
      }
  }

  return( os );
};


//
// Private class methods
//

/*
  count collects the IDs of WIR objects for system-level statistics.

  The IDs of WIR objects are collected in the corresponding sets passed as
  arguments.
*/
void WIR_Stats::count( const WIR_System &s, std::set<WIR_id_t> &sid,
                       std::set<WIR_id_t> &cid, std::set<WIR_id_t> &did,
                       std::set<WIR_id_t> &fid, std::set<WIR_id_t> &bid,
                       std::set<WIR_id_t> &iid, std::set<WIR_id_t> &oid,
                       std::set<WIR_id_t> &pid, std::set<WIR_id_t> &rid )
{
  DSTART( "void WIR_Stats::count(const WIR_System&, set<WIR_id_t>&, set<WIR_id_t>&, set<WIR_id_t>&, set<WIR_id_t>&, set<WIR_id_t>&, set<WIR_id_t>&, set<WIR_id_t>&, set<WIR_id_t>&, set<WIR_id_t>&)" );

  sid.insert( s.getID() );
  for ( WIR_CompilationUnit &c : s )
    count( c, cid, did, fid, bid, iid, oid, pid, rid );
};


/*
  count collects the IDs of WIR objects for compilation unit-level statistics.

  The IDs of WIR objects are collected in the corresponding sets passed as
  arguments.
*/
void WIR_Stats::count( const WIR_CompilationUnit &c, std::set<WIR_id_t> &cid,
                       std::set<WIR_id_t> &did, std::set<WIR_id_t> &fid,
                       std::set<WIR_id_t> &bid, std::set<WIR_id_t> &iid,
                       std::set<WIR_id_t> &oid, std::set<WIR_id_t> &pid,
                       std::set<WIR_id_t> &rid )
{
  DSTART(
    "void WIR_Stats::count(const WIR_CompilationUnit&, set<WIR_id_t>&, set<WIR_id_t>&, set<WIR_id_t>&, set<WIR_id_t>&, set<WIR_id_t>&, set<WIR_id_t>&, set<WIR_id_t>&, set<WIR_id_t>&)" );

  cid.insert( c.getID() );
  for ( WIR_Data &d : c.getData() )
    did.insert( d.getID() );
  for ( WIR_Function &f : c )
    count( f, fid, bid, iid, oid, pid, rid );
};


/*
  count collects the IDs of WIR objects for function-level statistics.

  The IDs of WIR objects are collected in the corresponding sets passed as
  arguments.
*/
void WIR_Stats::count( const WIR_Function &f, std::set<WIR_id_t> &fid,
                       std::set<WIR_id_t> &bid, std::set<WIR_id_t> &iid,
                       std::set<WIR_id_t> &oid, std::set<WIR_id_t> &pid,
                       std::set<WIR_id_t> &rid )
{
  DSTART(
    "void WIR_Stats::count(const WIR_Function&, set<WIR_id_t>&, set<WIR_id_t>&, set<WIR_id_t>&, set<WIR_id_t>&, set<WIR_id_t>&, set<WIR_id_t>&)" );

  fid.insert( f.getID() );
  for ( WIR_BaseRegister &r : f.getVirtualRegisters() )
    rid.insert( r.getID() );

  for ( WIR_BasicBlock &b : f )
    count( b, bid, iid, oid, pid, rid );
};


/*
  count collects the IDs of WIR objects for basic block-level statistics.

  The IDs of WIR objects are collected in the corresponding sets passed as
  arguments.
*/
void WIR_Stats::count( const WIR_BasicBlock &b, std::set<WIR_id_t> &bid,
                       std::set<WIR_id_t> &iid, std::set<WIR_id_t> &oid,
                       std::set<WIR_id_t> &pid, std::set<WIR_id_t> &rid )
{
  DSTART(
    "void WIR_Stats::count(const WIR_BasicBlock&, set<WIR_id_t>&, set<WIR_id_t>&, set<WIR_id_t>&, set<WIR_id_t>&, set<WIR_id_t>&)" );

  bid.insert( b.getID() );
  for ( WIR_Instruction &i : b )
    count( i, iid, oid, pid, rid );
};


/*
  count collects the IDs of WIR objects for instruction-level statistics.

  The IDs of WIR objects are collected in the corresponding sets passed as
  arguments.
*/
void WIR_Stats::count( const WIR_Instruction &i, std::set<WIR_id_t> &iid,
                       std::set<WIR_id_t> &oid, std::set<WIR_id_t> &pid,
                       std::set<WIR_id_t> &rid )
{
  DSTART(
    "void WIR_Stats::count(const WIR_Instruction&, set<WIR_id_t>&, set<WIR_id_t>&, set<WIR_id_t>&, set<WIR_id_t>&)" );

  iid.insert( i.getID() );
  for ( WIR_Operation &o : i )
    count( o, oid, pid, rid );
};


/*
  count collects the IDs of WIR objects for operation-level statistics.

  The IDs of WIR objects are collected in the corresponding sets passed as
  arguments.
*/
void WIR_Stats::count( const WIR_Operation &o, std::set<WIR_id_t> &oid,
                       std::set<WIR_id_t> &pid, std::set<WIR_id_t> &rid )
{
  DSTART(
    "void WIR_Stats::count(const WIR_Operation&, set<WIR_id_t>&, set<WIR_id_t>&, set<WIR_id_t>&)" );

  oid.insert( o.getID() );
  for ( WIR_Parameter &p : o )
    this->count( p, pid, rid );
};


/*
  count collects the IDs of WIR objects for parameter-level statistics.

  The IDs of WIR objects are collected in the corresponding sets passed as
  arguments.
*/
void WIR_Stats::count( const WIR_Parameter &p, std::set<WIR_id_t> &pid,
                       std::set<WIR_id_t> &rid )
{
  DSTART(
    "void WIR_Stats::count(const WIR_Parameter&, set<WIR_id_t>&, set<WIR_id_t>&)" );

  pid.insert( p.getID() );

  if ( p.getType() == WIR_ParameterType::reg )
    rid.insert(
      dynamic_cast<const WIR_RegisterParameter &>( p ).getRegister().getID() );
};


/*
  clearSets1 erases all data associated with a statistic's first reference
  point.
*/
void WIR_Stats::clearSets1( void )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  mSystems1.clear();
  mCompilationUnits1.clear();
  mFunctions1.clear();
  mBasicBlocks1.clear();
  mInstructions1.clear();
  mOperations1.clear();
  mParameters1.clear();
  mRegisters1.clear();
  mDataObjects1.clear();
};


/*
  clearSets2 erases all data associated with a statistic's second reference
  point.
*/
void WIR_Stats::clearSets2( void )
{
  DSTART( "void WIR_Stats::clearSets2()" );

  mSystems2.clear();
  mCompilationUnits2.clear();
  mFunctions2.clear();
  mBasicBlocks2.clear();
  mInstructions2.clear();
  mOperations2.clear();
  mParameters2.clear();
  mRegisters2.clear();
  mDataObjects2.clear();
};


/*
  diff computes how many IDs appear in the first set but not in the second one.
*/
unsigned int WIR_Stats::diff( const std::set<WIR_id_t> &s1,
                              const std::set<WIR_id_t> &s2 ) const
{
  DSTART(
    "unsigned int WIR_Stats::diff(const set<WIR_id_t>&, const set<WIR_id_t>&) const" );

  unsigned int res = 0;

  for ( WIR_id_t id : s1 )
    if ( !s2.count( id ) )
      ++res;

  return( res );
};

}       // namespace WIR
