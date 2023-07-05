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
  @file wirschedulingregion.cc
  @brief This file implements an abstract base class representing regions in
         which scheduling is performed.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include standard headers
#include <limits>
#include <set>
#include <sstream>
#include <stdlib.h>
#include <string>
#include <unistd.h>
#include <vector>

// Include boost headers
#include <boost/current_function.hpp>
#include <boost/graph/floyd_warshall_shortest.hpp>
#include <boost/graph/exterior_property.hpp>
#include <boost/graph/iteration_macros.hpp>
#include <boost/graph/topological_sort.hpp>
#include <boost/graph/named_function_params.hpp>

// Include libuseful headers
#include <libuseful/debugmacros.h>
#include <libuseful/exceptions.h>
#include <libuseful/io.h>

// Include WIR headers
#include <wir/wir.h>

// Include local headers
#include "wirschedulingregion.h"


//
// Code section
//

namespace WIR {


using namespace std;


//
// Public class methods
//

/*
  Default constructor.
*/
WIR_SchedulingRegion::WIR_SchedulingRegion( WIR_BasicBlock &b, bool verbosity,
                                            bool keepTmpFiles ) :
  mMaxCycle { 0 },
  mMaximumDelay { 0 },
  mVerbosity { verbosity },
  mKeepTmpFiles { keepTmpFiles }
{
  DSTART(
    "WIR_SchedulingRegion::WIR_SchedulingRegion(WIR_BasicBlock&, bool, bool)" );

  mBasicBlocks.push_back( &b );
  mBlockIDs.insert( b.getID() );

  // Construct map of IDs and their operations.
  for ( auto *b : mBasicBlocks )
    for ( WIR_Instruction &i : *b )
      for ( WIR_Operation &o : i )
        mOpOfID.insert( { o.getID(), o } );
};


/*
  Destructor.
*/
WIR_SchedulingRegion::~WIR_SchedulingRegion( void )
{
  DSTART( "virtual WIR_SchedulingRegion::~WIR_SchedulingRegion()" );
};


/*
  buildDG builds a region's dependence graph.
*/
void WIR_SchedulingRegion::buildDG( void )
{
  DSTART( "void WIR_SchedulingRegion::buildDG()" );

  DACTION(
    DOUT(
      "Building dependence graph of region with root basic block '" <<
      mBasicBlocks.front()->getName() << "'." << endl );
    for ( auto *b : mBasicBlocks )
      DOUT( *b ); );

  mDGraph.clear();
  mNodeByID.clear();

  // Add nodes to the dependence graph for all operations of a region.
  list<WIR_Instruction *> instrs;

  for ( WIR_BasicBlock *b : mBasicBlocks )
    for ( WIR_Instruction &i : *b ) {
      instrs.push_back( &i );

      for ( WIR_Operation &o : i ) {
        DGraphVertex v = add_vertex( &o, mDGraph );
        mNodeByID[ o.getID() ] = v;
        DACTION(
          stringstream str;
          str << o;
          DOUT(
            "Adding node for operation '" << str.str().substr( 8 ) << "'." << endl ); );
      }
    }

  // Check dependences between operations and add edges to the dependence
  // graph.
  for ( auto iIt = instrs.begin(); iIt != instrs.end(); ++iIt ) {
    auto succIt = iIt;
    for ( ++succIt; succIt != instrs.end(); ++succIt ) {
      for ( WIR_Operation &o1 : *(*iIt) )
        for ( WIR_Operation &o2 : *(*succIt) )
          addDependences( o1, o2 );
    }
  }

  // Add scheduling constraints of a region's basic blocks to the dependence
  // graph.
  for ( WIR_BasicBlock *b : mBasicBlocks )
    for ( WIR_SchedulingConstraint &c :
            b->getContainers<WIR_SchedulingConstraint>() ) {
      auto &constrainedInstrs = c.getInstructionSequence();

      for ( auto iIt = constrainedInstrs.begin();
            iIt != constrainedInstrs.end(); ++iIt ) {
        auto nextIt = iIt;
        for ( ++nextIt; nextIt != constrainedInstrs.end(); ++nextIt ) {
          for ( WIR_Operation &o1 : iIt->get() )
            for ( WIR_Operation &o2 : nextIt->get() ) {
              DGraphVertex v1 = mNodeByID[ o1.getID() ];
              DGraphVertex v2 = mNodeByID[ o2.getID() ];

              DACTION(
                stringstream str;
                stringstream str1;
                str << o1;
                str1 << o2;
                DOUT(
                  "Adding edge '" << str.str().substr( 8 ) << "' -> '" <<
                  str1.str().substr( 8 ) << "'." << endl ); );
              add_edge( v1, v2, { -1, WIR_DGEdgeType::constr }, mDGraph );
            }
        }
      }
    }

  // Compute latencies for all identified dependences.
  computeLatencies();

  // Compute earliest and latest execution cycles per operation of a region.
  computeExecutionCycles();

  // Compute maximum delays for all operations of a region.
  computeMaxDelays();

  // Compute mobility values for all operations of a region.
  computeMobility();

  {
    DSTART( "void WIR_SchedulingRegion::buildDG().visualize" );
    DACTION( visualize(); );
  }
};


/*
  getBasicBlocks returns the list of basic blocks of a scheduling region.
*/
const std::list<WIR_BasicBlock *> &WIR_SchedulingRegion::getBasicBlocks( void ) const
{
  DSTART(
    "const list<WIR_BasicBlock*>& WIR_SchedulingRegion::getBasicBlocks() const" );

  return( mBasicBlocks );
};


/*
  getOperationCount returns the number of WIR operations within the current
  scheduling region.
*/
unsigned int WIR_SchedulingRegion::getOperationCount( void ) const
{
  DSTART( "unsigned int WIR_SchedulingRegion::getOperationCount() const" );

  unsigned int res = 0;

  for ( WIR_BasicBlock *b : mBasicBlocks )
    for ( WIR_Instruction &i : *b )
      res += i.getOperations().size();

  return( res );
};


/*
  getNumberOfSuccessors determines the number of successors of a WIR operation
  in the dependence graph.
*/
unsigned int WIR_SchedulingRegion::getNumberOfSuccessors( const WIR_Operation &o ) const
{
  DSTART(
    "unsigned int WIR_SchedulingRegion::getNumberOfSuccessors(const WIR_Operation&) const" );

  return( out_degree( mNodeByID.at( o.getID() ), mDGraph ) );
};


/*
  getUnscheduledPredecessors computes the set of unscheduled predecessor nodes
  of an operation in the dependence graph.
*/
WIR_OperationSet WIR_SchedulingRegion::getUnscheduledPredecessors( const WIR_Operation &o ) const
{
  DSTART(
    "WIR_OperationSet WIR_SchedulingRegion::getUnscheduledPredecessors(const WIR_Operation&) const" );

  DACTION(
    stringstream str;
    str << o;
    DOUT( "Unscheduled predecessors of '" << str.str().substr( 8 ) << "':" ); );

  WIR_OperationSet res;

  auto v = mNodeByID.at( o.getID() );

  for ( auto p = in_edges( v, mDGraph ); p.first != p.second; ++p.first ) {
    DGraphEdge e = *(p.first);
    auto src = source( e, mDGraph );
    WIR_Operation &srcOp = *(mDGraph[ src ]);

    if ( mEarliestCycle.count( srcOp.getID() ) ) {
      res.insert( srcOp );
      DACTION(
        stringstream str;
        str << srcOp;
        DOUT( " '" << str.str().substr( 8 ) << "'" ); );
    }
  }

  DOUT( endl );

  return( res );
};


/*
  visualize dumps the current current region's dependence graph into a DOT file
  and invokes xdot on it.
*/
void WIR_SchedulingRegion::visualize( void ) const
{
  DSTART( "void WIR_SchedulingRegion::visualize() const" );

  char mask[12] = "/tmp/XXXXXX";
  int fileDescriptor = mkstemp( mask );

  if ( fileDescriptor == -1 )
    throw ufFatalError( "Failed to allocate temporary file." );
  else
    close( fileDescriptor );

  string fileName( mask );
  fstream dotFile( fileName, ios_base::out );

  // Write dot graph header.
  dotFile << "digraph G {" << endl << "graph [rotate=90]" << endl;

  // Write dot graph nodes.
  map<DGraphVertex, unsigned int> nodeToDotInt;
  visualizeNodes( dotFile, nodeToDotInt );

  // Write dot graph edges.
  visualizeEdges( dotFile, nodeToDotInt );

  // Write dot graph closing.
  dotFile << "}" << endl;
  dotFile.close();

  // Call xdot.
  ostringstream sstr;
  sstr << "xdot " << fileName << " > /dev/null 2>&1";
  if ( mVerbosity )
    ufNoteMsg << ufFile() << sstr.str() << endl;
  if ( system( sstr.str().c_str() ) != 0 )
    throw ufFatalError( "Failed to execute xdot." );

  if ( !mKeepTmpFiles )
    unlink( fileName.c_str() );
};


/*
  getEarliestCycleOpsMap returns the map providing all WIR operations to be scheduled into one earliest execution cycle.
*/
std::map<long long, WIR_OperationSet> &WIR_SchedulingRegion::getEarliestCycleOpsMap( void )
{
  DSTART(
    "map<long long int, set<reference_wrapper<WIR_Operation>, WIR_Compare<WIR_Operation> > >& WIR_SchedulingRegion::getEarliestCycleOpsMap()" );

  return( mEarliestCycleOps );
};


/*
  getEarliestCycleMap returns the map providing the earliest execution cycle
  for each WIR operation of a region.
*/
std::map<WIR_id_t, long long> &WIR_SchedulingRegion::getEarliestCycleMap( void )
{
  DSTART(
    "map<long long unsigned int, long long int>& WIR_SchedulingRegion::getEarliestCycleMap()" );

  return( mEarliestCycle );
};


/*
  getLatestCycleOpsMap returns the map providing all WIR operations to be
  scheduled into one latest execution cycle.
*/
std::map<long long, WIR_OperationSet> &WIR_SchedulingRegion::getLatestCycleOpsMap( void )
{
  DSTART(
    "map<long long int, set<reference_wrapper<WIR_Operation>, WIR_Compare<WIR_Operation> > >& WIR_SchedulingRegion::getLatestCycleOpsMap()" );

  return( mLatestCycleOps );
};


/*
  getLatestCycleMap returns the map providing the latest execution cycle for
  each WIR operation of a region.
*/
std::map<WIR_id_t, long long> &WIR_SchedulingRegion::getLatestCycleMap( void )
{
  DSTART(
    "map<long long unsigned int, long long int>& WIR_SchedulingRegion::getLatestCycleMap()" );

  return( mLatestCycle );
};


/*
  getMaxDelayMap returns the map providing the maximum delay for each WIR
  operation of a region.
*/
std::map<WIR_id_t, unsigned long long> &WIR_SchedulingRegion::getMaxDelayMap( void )
{
  DSTART(
    "map<long long unsigned int, long long unsigned int>& WIR_SchedulingRegion::getMaxDelayMap()" );

  return( mMaxDelay );
};


/*
  getMobilityMap returns the map providing the mobility for each WIR operation
  of a region, i.e., to the difference between the operation's latest and
  earliest execution cycle.
*/
std::map<WIR_id_t, long long> &WIR_SchedulingRegion::getMobilityMap( void )
{
  DSTART(
    "map<long long unsigned int, long long int>& WIR_SchedulingRegion::getMobilityMap()" );

  return( mMobility );
};


/*
  updateCycleMaps updates a region's earliest and latest cycles maps after
  having scheduled a bundle of operations within the region.
*/
long long WIR_SchedulingRegion::updateCycleMaps( const std::list<WIR_Operation *> &b,
                                                 long long currentCycle )
{
  DSTART(
    "long long int WIR_SchedulingRegion::updateCycleMaps(const list<WIR_Operation*>&, long long int)" );

  DACTION(
    DOUT(
      "Updating region maps for current execution cycle " << currentCycle <<
      " and scheduled operation bundle {" );
    for ( auto *op : b )
      if ( op != nullptr ) {
        stringstream str;
        str << *op;
        DOUT( " '" << str.str().substr( 8 ) << "'" );
      } else
        DOUT( " <nullptr>" );
    DOUT( " }" << endl ); );

  // Collect IDs of all operations in the current bundle and determine the
  // bundle's maximal latency.
  set<WIR_id_t> bundleIDs;
  long long latency = 0;

  for ( auto *o : b )
    if ( o != nullptr ) {
      bundleIDs.insert( o->getID() );
      if ( getLatency( *o ) > latency )
        latency = getLatency( *o );
    }

  // Update all operations with a lower earliest cycle than the current cycle.
  for ( auto &p : mEarliestCycle )
    if ( !bundleIDs.count( p.first ) && ( p.second <= currentCycle + latency ) )
      setEarliestCycle(
        mOpOfID.at( p.first ).get(), currentCycle + latency, mEarliestCycle );
  for ( auto &p : mLatestCycle )
    if ( !bundleIDs.count( p.first ) && ( p.second <= currentCycle + latency ) )
      setEarliestCycle(
        mOpOfID.at( p.first ).get(), currentCycle + latency, mLatestCycle );

  // Update earliest cycles of all operations of the current bundle.
  for ( auto id : bundleIDs ) {
    setEarliestCycle( mOpOfID.at( id ).get(), currentCycle, mEarliestCycle );
    setEarliestCycle( mOpOfID.at( id ).get(), currentCycle, mLatestCycle );
  }

  // Now erase the operations of the current bundle so that they will not be
  // considered again.
  for ( auto id : bundleIDs ) {
    mEarliestCycle.erase( id );
    mLatestCycle.erase( id );
  }

  // Rebuild earliest and latest cycles reverse maps.
  mEarliestCycleOps.clear();
  for ( auto &p : mEarliestCycle )
    mEarliestCycleOps[ p.second ].insert( mOpOfID.at( p.first ).get() );

  mLatestCycleOps.clear();
  for ( auto &p : mLatestCycle )
    mLatestCycleOps[ p.second ].insert( mOpOfID.at( p.first ).get() );

  return( latency );
};


/*
  updateMovedOperations updates a region's internal data structures after having
  moved the operations of a given bundle.

  While moving operations of a bundle to new instructions, the pointers to the
  involved operations do change. updateMovedOperations updates all internal data
  structures of a scheduling regions such that the correct pointers given in the
  specified bundle are stored.
*/
void WIR_SchedulingRegion::updateMovedOperations( std::list<WIR_Operation *> &b )
{
  DSTART(
    "void WIR_SchedulingRegion::updateMovedOperations(list<WIR_Operation*>&)" );

  for ( auto *o : b )
    if ( o != nullptr ) {
      mOpOfID.erase( o->getID() );
      mOpOfID.insert( { o->getID(), *o } );

      mDGraph[ mNodeByID[ o->getID() ] ] = o;
    }
};


/*
  postProcessingHook allows to perform processor-specific actions after having
  done instruction scheduling for a region.

  Since these actions are processor-specific and might or might not be necessary
  for some actual processor architecture, this method is virtual and can be
  overloaded if required.
*/
void WIR_SchedulingRegion::postProcessingHook( void )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );
};


//
// Protected class methods
//

/*
  addDependences checks for RAW, WAR, WAW and other dependences between two
  operations and adds them as edges to the dependence graph.

  Since dependences might depend on specific region types or might be
  processor-specific, this method is virtual so that it can be overloaded by
  derived classes.
*/
void WIR_SchedulingRegion::addDependences( const WIR_Operation &o1,
                                           const WIR_Operation &o2 )
{
  DSTART(
    "virtual void WIR_SchedulingRegion::addDependences(const WIR_Operation&, const WIR_Operation&)" );

  DACTION(
    stringstream str;
    stringstream str1;
    str << o1;
    str1 << o2;
    DOUT(
      "Checking dependences between '" << str.str().substr( 8 ) << "' and '" <<
      str1.str().substr( 8 ) << "'." << endl ); );

  WIR_DGEdgeProperty dep;

  // Iterate all parameters of o2.
  bool o2hasRegParams = false;

  for ( WIR_Parameter &p2 : o2 )
    if ( p2.getType() == WIR_ParameterType::reg ) {
      auto &rp2 = dynamic_cast<WIR_RegisterParameter &>( p2 );
      o2hasRegParams = true;

      // Check for control dependences.
      if ( ( dep.mType < WIR_DGEdgeType::ctrl ) && checkCtrl( o1, rp2 ) ) {
        dep.mType = WIR_DGEdgeType::ctrl;
        DOUT( "Control dependence detected." << endl );
        break;
      } else

      // Check for uncertain dependences.
      if ( ( dep.mType < WIR_DGEdgeType::ucert ) && checkUCert( o1, rp2 ) ) {
        dep.mType = WIR_DGEdgeType::ucert;
        DOUT( "Uncertain dependence detected." << endl );
      } else

      // Check for output dependences.
      if ( ( dep.mType < WIR_DGEdgeType::waw ) && checkWAW( o1, rp2 ) ) {
        dep.mType = WIR_DGEdgeType::waw;
        DOUT( "WAW dependence detected." << endl );
      } else

      // Check for anti dependences.
      if ( ( dep.mType < WIR_DGEdgeType::war ) && checkWAR( o1, rp2 ) ) {
        dep.mType = WIR_DGEdgeType::war;
        DOUT( "WAR dependence detected." << endl );
      } else

      // Check for data dependences.
      if ( ( dep.mType < WIR_DGEdgeType::raw ) && checkRAW( o1, rp2 ) ) {
        dep.mType = WIR_DGEdgeType::raw;
        DOUT( "RAW dependence detected." << endl );
      }
    }

  if ( !o2hasRegParams &&
       ( ( o1.isMemoryLoad() && o2.isMemoryStore() ) ||
         ( o1.isMemoryStore() && o2.isMemoryLoad() ) ||
         ( o1.isMemoryStore() && o2.isMemoryStore() ) ) ) {
    // Special handling is required for operations o2 that have no register
    // parameters but are nevertheless memory-related.
    dep.mType = WIR_DGEdgeType::ucert;
    DOUT( "Uncertain dependence detected." << endl );
  } else

  if ( !o2hasRegParams && ( o2.isCall() || o2.isReturn() || o2.isJump() ) ) {
    // Special handling is required for operations o2 that have no register
    // parameters but are nevertheless control flow-related.
    dep.mType = WIR_DGEdgeType::ctrl;
    DOUT( "Control dependence detected." << endl );
  }

  // Add edges to the dependence graph.
  if ( dep.mType != WIR_DGEdgeType::none ) {
    DGraphVertex v1 = mNodeByID[ o1.getID() ];
    DGraphVertex v2 = mNodeByID[ o2.getID() ];

    DACTION(
      DOUT( "Adding " );
      if ( dep.mType == WIR_DGEdgeType::raw )
        DOUT( "RAW " );
      if ( dep.mType == WIR_DGEdgeType::war )
        DOUT( "WAR " );
      if ( dep.mType == WIR_DGEdgeType::waw )
        DOUT( "WAW " );
      if ( dep.mType == WIR_DGEdgeType::ucert )
        DOUT( "UCERT " );
      if ( dep.mType == WIR_DGEdgeType::ctrl )
        DOUT( "CTRL " );
      stringstream str;
      str << o1;
      stringstream str1;
      str1 << o2;
      DOUT(
        "edge '" << str.str().substr( 8 ) << "' -> '" <<
        str1.str().substr( 8 ) << "'." << endl ); );
    add_edge( v1, v2, dep, mDGraph );
  }
};


/*
  checkRAW checks whether there is a classical data dependence
  (read-after-write) between an operation and some register parameter.
*/
bool WIR_SchedulingRegion::checkRAW( const WIR_Operation &o1,
                                     const WIR_RegisterParameter &rp2 ) const
{
  DSTART(
    "bool WIR_SchedulingRegion::checkRAW(const WIR_Operation&, const WIR_RegisterParameter&) const" );

  if ( rp2.getUsage() == WIR_Usage::def )
    return( false );

  for ( WIR_Parameter &p1 : o1 )
    if ( p1.getType() == WIR_ParameterType::reg ) {
      auto &rp1 = dynamic_cast<WIR_RegisterParameter &>( p1 );

      if ( ( rp1.getUsage() != WIR_Usage::use ) &&
           sameRegisters( rp1.getRegister(), rp2.getRegister() ) )
        return( true );
    }

  return( false );
};


/*
  checkWAR checks whether there is an anti dependence (write-after-read) between
  an operation and some register parameter.
*/
bool WIR_SchedulingRegion::checkWAR( const WIR_Operation &o1,
                                     const WIR_RegisterParameter &rp2 ) const
{
  DSTART(
    "bool WIR_SchedulingRegion::checkWAR(const WIR_Operation&, const WIR_RegisterParameter&) const" );

  if ( rp2.getUsage() == WIR_Usage::use )
    return( false );

  for ( WIR_Parameter &p1 : o1 )
    if ( p1.getType() == WIR_ParameterType::reg ) {
      auto &rp1 = dynamic_cast<WIR_RegisterParameter &>( p1 );

      if ( ( rp1.getUsage() != WIR_Usage::def ) &&
           sameRegisters( rp1.getRegister(), rp2.getRegister() ) )
        return( true );
    }

  return( false );
};


/*
  checkWAW checks whether there is an output dependence (write-after-write)
  between an operation and some register parameter.
*/
bool WIR_SchedulingRegion::checkWAW( const WIR_Operation &o1,
                                     const WIR_RegisterParameter &rp2 ) const
{
  DSTART(
    "bool WIR_SchedulingRegion::checkWAW(const WIR_Operation&, const WIR_RegisterParameter&) const" );

  if ( rp2.getUsage() == WIR_Usage::use )
    return( false );

  for ( WIR_Parameter &p1 : o1 )
    if ( p1.getType() == WIR_ParameterType::reg ) {
      auto &rp1 = dynamic_cast<WIR_RegisterParameter &>( p1 );

      if ( ( rp1.getUsage() != WIR_Usage::use ) &&
           sameRegisters( rp1.getRegister(), rp2.getRegister() ) )
        return( true );
    }

  return( false );
};


/*
  eraseRedundantEdges removes redundant edges from a region's dependence graph.
*/
void WIR_SchedulingRegion::eraseRedundantEdges( void )
{
  DSTART( "void WIR_SchedulingRegion::eraseRedundantEdges()" );

  // Solve the all-pairs shortest paths problem.
  using DistanceProperty = boost::exterior_vertex_property<DGraph, long long>;
  using DistanceMatrix = DistanceProperty::matrix_type;
  using DistanceMatrixMap = DistanceProperty::matrix_map_type;

  DistanceMatrix distances( num_vertices( mDGraph ) );
  DistanceMatrixMap dm( distances, mDGraph );
  bool valid =
    floyd_warshall_all_pairs_shortest_paths(
      mDGraph, dm,
      weight_map( get( &WIR_DGEdgeProperty::mLatency, mDGraph ) ) );

  ufAssertT( valid, "Found negative weight cycle in dependence graph." );

  // Sort the dependence graph topologically.
  vector<DGraphVertex> topSort( num_vertices( mDGraph ) );
  topological_sort( mDGraph, topSort.begin() );

  // Traverse the dependence graph and erase edges node by node.
  for ( auto topIt = topSort.rbegin(); topIt != topSort.rend(); ++topIt ) {
    DGraphVertex a = *topIt;

    // An edge (a -> b) is redundant if there is another path P_a,b from a to b,
    // and the distance along P_a,b is greater than or equal to the distance
    // across edge (a -> b). The distance along a path is the sum of the
    // latencies of the path's edges. The distance across an edge is the edge's
    // latency.
    // In a schedule of the dependence graph nodes, edge (a -> b) enforces the
    // partial order of a and b and the minimum latency between a and b.
    // However, because P_a,b enforces both of these conditions, edge (a -> b)
    // is unnecessary and can be removed.

    // Prepare a list of all out-edges of a.
    list<DGraphEdge> outEdges;
    for ( auto p = out_edges( a, mDGraph ); p.first != p.second; ++p.first )
      outEdges.push_back( *(p.first) );

    // Iterate each outgoing edge e = (a -> b).
    for ( auto it = outEdges.begin(); it != outEdges.end(); ) {
      DGraphEdge e = *it;
      auto b = target( e, mDGraph );
      bool edgeErased = false;

      // Examine every direct successor c of a except b itself.
      for ( auto e1 : outEdges ) {
        auto c = target( e1, mDGraph );

        if ( c == b )
          continue;

        long long distance = distances[ c ][ b ];

        // Check whether there is a path from a to b via c.
        if ( distance != ( numeric_limits<long long>::max )() ) {
          // The edge (a -> b) can be removed if the latency along the edge is
          // less than or equal to the latencies of the path from a to b via c.
          if ( mDGraph[ e ].mLatency <= mDGraph[ e1 ].mLatency + distance ) {
            DACTION(
              stringstream str;
              str << *(mDGraph[ a ]);
              stringstream str1;
              str1 << *(mDGraph[ b ]);
              DOUT(
                "Erasing redundant edge '" << str.str().substr( 8 ) <<
                "' -> '" << str1.str().substr( 8 ) << "'." << endl ); );

            edgeErased = true;
            remove_edge( e, mDGraph );
            break;
          }
        }
      }

      if ( edgeErased )
        it = outEdges.erase( it );
      else
        ++it;
    }
  }
};


/*
  For each edge in the dependence graph, computeLatencies determines the latency
  between the involved, dependent operations.
*/
void WIR_SchedulingRegion::computeLatencies( void )
{
  DSTART( "void WIR_SchedulingRegion::computeLatencies()" );

  BGL_FORALL_EDGES( e, mDGraph, DGraph ) {
    auto &o1 = *(mDGraph[ source( e, mDGraph ) ]);
    auto &o2 = *(mDGraph[ target( e, mDGraph ) ]);

    mDGraph[ e ].mLatency = computeLatency( o1, o2, mDGraph[ e ].mType );
    DACTION(
      stringstream str;
      str << o1;
      stringstream str1;
      str1 << o2;
      DOUT(
        "Setting latency of edge '" << str.str().substr( 8 ) << "' -> '" <<
        str1.str().substr( 8 ) << "' to " << mDGraph[ e ].mLatency << "." <<
        endl ); );
  }
};


/*
  computeExecutionCycles computes the earliest and latest execution cycles for
  all operations of a region.
*/
void WIR_SchedulingRegion::computeExecutionCycles( void )
{
  DSTART( "void WIR_SchedulingRegion::computeExecutionCycles()" );

  mEarliestCycle.clear();
  mLatestCycle.clear();
  mEarliestCycleOps.clear();
  mLatestCycleOps.clear();
  mMaxCycle = 0;

  list<WIR_Operation *> opList;

  // ASAP heuristic: Determine earliest execution cycle for each operation in
  // the current region, beginning with root nodes.
  for ( WIR_BasicBlock *b : mBasicBlocks )
    for ( WIR_Instruction &i : *b )
      for ( WIR_Operation &o : i ) {
        opList.push_back( &o );
        setEarliestCycle( o, getStartCycle( o ), mEarliestCycle );
      }

  // Fill the reverse map of all earliest execution cycles.
  for ( WIR_Operation *o : opList ) {
    auto it = mEarliestCycle.find( o->getID() );
    mEarliestCycleOps[ it->second ].insert( *o );
  }

  // ALAP heuristic: Determine latest execution cycle for each operation in the
  // current region.
  for ( auto rIt = opList.rbegin(); rIt != opList.rend(); ++rIt )
    setLatestCycle( *(*rIt), mMaxCycle, mLatestCycle );

  // Fill the reverse map of all latest execution cycles.
  for ( WIR_Operation *o : opList ) {
    auto it = mLatestCycle.find( o->getID() );
    mLatestCycleOps[ it->second ].insert( *o );
  }

  // Finally, remove redundant edges from the dependence graph.
  eraseRedundantEdges();
};


/*
  setEarliestCycle sets the earliest execution cycle of an operation.

  When setting an operation's earliest execution cycle, this change is also
  propagated recursively to all successors in the region's dependence graph.
*/
void WIR_SchedulingRegion::setEarliestCycle( const WIR_Operation &o,
                                             long long c,
                                             std::map<WIR_id_t, long long> &cycles )
{
  DSTART(
    "void WIR_SchedulingRegion::setEarliestCycle(const WIR_Operation&, long long int, map<long long unsigned int, long long int>&)" );

  // Find an eventually already existing entry for the given operation.
  auto it = cycles.find( o.getID() );

  if ( ( it != cycles.end() ) && ( c <= it->second ) )
    // Abort if we don't need to update the operation's earliest cycle.
    return;

  DACTION(
    stringstream str;
    str << o;
    DOUT(
      "Setting earliest cycle of '" << str.str().substr( 8 ) << "' to " << c <<
      "." << endl ); );
  cycles[ o.getID() ] = c;

  // Update the region's maximum execution cycle.
  mMaxCycle = ( c > mMaxCycle ) ? c : mMaxCycle;

  // Propagate the updated execution cycle to o's successors.
  auto v = mNodeByID[ o.getID() ];

  for ( auto p = out_edges( v, mDGraph ); p.first != p.second; ++p.first ) {
    DGraphEdge e = *(p.first);
    auto tgt = target( e, mDGraph );
    WIR_Operation &tgtOp = *(mDGraph[ tgt ]);

    long long tgtCycle = c + mDGraph[ e ].mLatency;

    if ( mBlockIDs.count( tgtOp.getInstruction().getBasicBlock().getID() ) )
      setEarliestCycle( tgtOp, tgtCycle, cycles );
  }
};


/*
  setLatestCycle sets the latest execution cycle of an operation.

  When setting an operation's latest execution cycle, this change is also
  propagated recursively to all predecessors in the region's dependence
  graph.
*/
void WIR_SchedulingRegion::setLatestCycle( const WIR_Operation &o, long long c,
                                           std::map<WIR_id_t, long long> &cycles )
{
  DSTART(
    "void WIR_SchedulingRegion::setLatestCycle(const WIR_Operation&, long long int, map<long long unsigned int, long long int>&)" );

  // Find an eventually already existing entry for the given operation.
  auto it = cycles.find( o.getID() );

  if ( ( it != cycles.end() ) && ( c >= it->second ) )
    // Abort if we don't need to update the operation's earliest cycle.
    return;

  DACTION(
    stringstream str;
    str << o;
    DOUT(
      "Setting latest cycle of '" << str.str().substr( 8 ) << "' to " << c <<
      "." << endl ); );
  cycles[ o.getID() ] = c;

  // Propagate the updated execution cycle to o's predecessors.
  auto v = mNodeByID[ o.getID() ];

  for ( auto p = in_edges( v, mDGraph ); p.first != p.second; ++p.first ) {
    DGraphEdge e = *(p.first);
    auto src = source( e, mDGraph );
    WIR_Operation &srcOp = *(mDGraph[ src ]);

    long long srcCycle = c - mDGraph[ e ].mLatency;

    if ( mBlockIDs.count( srcOp.getInstruction().getBasicBlock().getID() ) )
      setLatestCycle( srcOp, srcCycle, cycles );
  }
};


/*
  getStartCycle determines an operation's absolutely earliest execution cycle if
  that operation were scheduled as the region's very first operation.

  In general, getStartCycle simply returns the constant value 1, since this is
  the absolutely earliest start time of any operation. However, this can be
  processor-specific so that this method is virtual and can be overloaded by
  derived classes.
*/
long long WIR_SchedulingRegion::getStartCycle( const WIR_Operation &o ) const
{
  DSTART(
    "virtual long long int WIR_SchedulingRegion::getStartCycle(const WIR_Operation&) const" );

  (void) o;

  return( 1 );
};


/*
  computeMaxDelays computes maximum delays for all operations of a region.
*/
unsigned long long WIR_SchedulingRegion::computeMaxDelays( void )
{
  DSTART( "long long unsigned int WIR_SchedulingRegion::computeMaxDelays()" );

  mMaxDelay.clear();

  for ( auto bIt = mBasicBlocks.rbegin(); bIt != mBasicBlocks.rend(); ++bIt )
    for ( auto iIt = (*bIt)->rbegin(); iIt != (*bIt)->rend(); ++iIt )
      for ( auto oIt = iIt->get().rbegin(); oIt != iIt->get().rend(); ++oIt )
        // Delays start with a value of 1 instead of 0, because the value 0 is
        // reserved as a penalty "priority".
        setMaxDelay( oIt->get(), 1 );

  // Saturate the delays for all dependence edges that lead to operations
  // outside the region's basic block set or that connect operations from
  // different basic blocks or that connect operations of the same basic block
  // but in an order against the control flow.
  BGL_FORALL_EDGES( e, mDGraph, DGraph ) {
    auto &o1 = *(mDGraph[ source( e, mDGraph ) ]);
    auto &o2 = *(mDGraph[ target( e, mDGraph ) ]);

    auto b1 = o1.getInstruction().getBasicBlock();
    auto b2 = o2.getInstruction().getBasicBlock();

    if ( mBlockIDs.count( b1.getID() ) ) {
      bool b2OutsideRegion = !mBlockIDs.count( b2.getID() );
      bool differentBBs = ( b1 != b2 );
      bool sameBBbutAgainstCF = false;

      if ( !differentBBs ) {
        for ( WIR_Instruction &i: b2 )
          for ( WIR_Operation &o: i )
            if ( o == o2 ) {
              sameBBbutAgainstCF = true;
              break;
            } else if ( o == o1 )
              break;
      }

      if ( b2OutsideRegion || differentBBs || sameBBbutAgainstCF )
        setMaxDelay( o1, mDGraph[ e ].mLatency );
    }
  }

  return( mMaximumDelay );
};


/*
  setMaxDelay sets the maximum delay of an operation.
*/
void WIR_SchedulingRegion::setMaxDelay( const WIR_Operation &o,
                                        unsigned long long d )
{
  DSTART(
    "void WIR_SchedulingRegion::setMaxDelay(const WIR_Operation&, long long unsigned int)" );

  // Find an eventually already existing entry for the given operation.
  auto it = mMaxDelay.find( o.getID() );

  if ( ( it != mMaxDelay.end() ) && ( d <= it->second ) )
    // Abort if we don't need to update the operation's earliest cycle.
    return;

  DACTION(
    stringstream str;
    str << o;
    DOUT(
      "Setting maximum delay of '" << str.str().substr( 8 ) << "' to " << d <<
      "." << endl ); );
  mMaxDelay[ o.getID() ] = d;

  // Determine the normalized maximum delay for the entire region.
  if ( d > mMaximumDelay )
    mMaximumDelay += ( d - mMaximumDelay ) > 1 ? 1 : d - mMaximumDelay;

  // Propagate the updated delay to o's predecessors.
  auto v = mNodeByID[ o.getID() ];

  for ( auto p = in_edges( v, mDGraph ); p.first != p.second; ++p.first ) {
    DGraphEdge e = *(p.first);
    auto src = source( e, mDGraph );
    WIR_Operation &srcOp = *(mDGraph[ src ]);

    long long srcDelay = d + mDGraph[ e ].mLatency;

    if ( mBlockIDs.count( srcOp.getInstruction().getBasicBlock().getID() ) )
      setMaxDelay( srcOp, srcDelay );
  }
};


/*
  computeMobility computes the mobility of all operations of a region.

  The mobility of an operation is the difference between its latest and earliest
  execution cycle value. Note that the mobility is defined to be greater than or
  equal to 1 in order to avoid divisions by zero.
*/
void WIR_SchedulingRegion::computeMobility( void )
{
  DSTART( "void WIR_SchedulingRegion::computeMobility()" );

  mMobility.clear();

  // Iterate all operations in the current region.
  for ( WIR_BasicBlock *b : mBasicBlocks )
    for ( WIR_Instruction &i : *b )
      for ( WIR_Operation &o : i ) {
        mMobility[ o.getID() ] =
          mLatestCycle[ o.getID() ] - mEarliestCycle[ o.getID() ] + 1;
        DACTION(
          stringstream str;
          str << o;
          DOUT(
            "Setting mobility of '" << str.str().substr( 8 ) << "' to " <<
            mMobility[ o.getID() ] << "." << endl ); );
      }
};


/*
  sameRegisters determines whether two given WIR registers (partially) are the
  same.

  Two hierarchical registers are partially the same if they share a common leaf
  register, including potential pre-colorings of virtual registers.
*/
bool WIR_SchedulingRegion::sameRegisters( const WIR_BaseRegister &r1,
                                          const WIR_BaseRegister &r2 ) const
{
  DSTART(
    "bool WIR_SchedulingRegion::sameRegisters(const WIR_BaseRegister&, const WIR_BaseRegister&) const" );

  for ( WIR_BaseRegister &l1 : r1.getLeafs() )
    for ( WIR_BaseRegister &l2 : r2.getLeafs() ) {
      if ( l1 == l2 ) {
        DOUT(
          "Registers '" << r1.getName() << "' and '" << r2.getName() <<
          "' are the same." << endl );
        return( true );
      }

      if ( l1.isVirtual() ) {
        auto &vreg1 = dynamic_cast<WIR_VirtualRegister &>( l1 );
        if ( vreg1.isPrecolored() ) {
          if ( vreg1.getPrecolor() == l2 ) {
            DOUT(
              "Registers '" << r1.getName() << "' and '" << r2.getName() <<
              "' are the same." << endl );
            return( true );
          }

          if ( l2.isVirtual() ) {
            auto &vreg2 = dynamic_cast<WIR_VirtualRegister &>( l2 );
            if ( vreg2.isPrecolored() ) {
              if ( vreg1.getPrecolor() == vreg2.getPrecolor() ) {
                DOUT(
                  "Registers '" << r1.getName() << "' and '" << r2.getName() <<
                  "' are the same." << endl );
                return( true );
              }
            }
          }
        }
      }

      if ( l2.isVirtual() ) {
        auto &vreg2 = dynamic_cast<WIR_VirtualRegister &>( l2 );
        if ( vreg2.isPrecolored() ) {
          if ( vreg2.getPrecolor() == l1 ) {
            DOUT(
              "Registers '" << r1.getName() << "' and '" << r2.getName() <<
              "' are the same." << endl );
            return( true );
          }
        }
      }
    }

  DOUT(
    "Registers '" << r1.getName() << "' and '" << r2.getName() <<
    "' are not the same." << endl );
  return( false );
};


//
// Private class methods
//

/*
  visualizeNodes dumps the dependence graph nodes into a given DOT file.
*/
void WIR_SchedulingRegion::visualizeNodes( std::fstream &dotFile,
                                           std::map<DGraphVertex, unsigned int> &nodeToDotInt ) const
{
  DSTART(
    "void WIR_SchedulingRegion::visualizeNodes(fstream&, map<unsigned int, unsigned int>&) const" );

  unsigned int nodeCounter = 0;

  // Produce a legend in the visualization.
  dotFile << "subgraph clusterLegend {" << endl;

  dotFile << nodeCounter++ << "[label=\"Operation\",shape=box]" << endl << ";"
          << endl;

  dotFile << nodeCounter++ << "[shape=point]" << endl << ";" << endl;
  dotFile << nodeCounter++ << "[shape=point]" << endl << ";" << endl;
  dotFile << nodeCounter - 2 << "->" << nodeCounter - 1
          << "[label=\"RAW dependence\",dir=forward,arrowhead=vee,"
          << "arrowtail=none]" << endl << ";" << endl;

  dotFile << nodeCounter++ << "[shape=point]" << endl << ";" << endl;
  dotFile << nodeCounter++ << "[shape=point]" << endl << ";" << endl;
  dotFile << nodeCounter - 2 << "->" << nodeCounter - 1
          << "[label=\"WAR dependence\",dir=forward,arrowhead=vee,"
          << "arrowtail=none,style=dashed]" << endl << ";" << endl;

  dotFile << nodeCounter++ << "[shape=point]" << endl << ";" << endl;
  dotFile << nodeCounter++ << "[shape=point]" << endl << ";" << endl;
  dotFile << nodeCounter - 2 << "->" << nodeCounter - 1
          << "[label=\"WAW dependence\",dir=forward,arrowhead=vee,"
          << "arrowtail=none,style=dotted]" << endl << ";" << endl;

  dotFile << nodeCounter++ << "[shape=point]" << endl << ";" << endl;
  dotFile << nodeCounter++ << "[shape=point]" << endl << ";" << endl;
  dotFile << nodeCounter - 2 << "->" << nodeCounter - 1
          << "[label=\"Constraint dependence\",dir=forward,arrowhead=vee,"
          << "arrowtail=none,style=dotted,color=red]" << endl << ";" << endl;

  dotFile << nodeCounter++ << "[shape=point]" << endl << ";" << endl;
  dotFile << nodeCounter++ << "[shape=point]" << endl << ";" << endl;
  dotFile << nodeCounter - 2 << "->" << nodeCounter - 1
          << "[label=\"Uncertain or Control dependence\",dir=forward,"
          << "arrowhead=vee,arrowtail=none,style=bold]" << endl << ";" << endl;

  dotFile << nodeCounter++ << "[label=\""
          << mBasicBlocks.front()->getFunction().getCompilationUnit().getName()
          << "\\n" << mBasicBlocks.front()->getName() << "\",color=white]"
          << endl << ";" << endl;

  dotFile << "}" << endl;

  // Visualize all graph nodes.
  BGL_FORALL_VERTICES( v, mDGraph, DGraph ) {
    auto &o = *(mDGraph[ v ]);

    dotFile << "subgraph cluster" << nodeCounter++ << " {" << endl;

    for ( auto it = o.rbegin(); it != o.rend(); ++it ) {
      dotFile << nodeCounter++ << " [label=\"" << *it;
      if ( it != o.rbegin() )
        dotFile << ",";
      dotFile << "\",color=white,shape=box]" << endl << ";" << endl;
    }

    nodeToDotInt[ v ] = nodeCounter;

    dotFile << nodeCounter++ << " [label=\"" << o.getOpCode().getName()
            << " (ID=" << o.getID() << ")" << "\",color=white,shape=box]"
            << endl << ";" << endl;

    dotFile << "}" << endl;
  }
};


/*
  visualizeEdges dumps the control  flow graph edges into a given DOT file.
*/
void WIR_SchedulingRegion::visualizeEdges( std::fstream &dotFile,
                                           const std::map<DGraphVertex, unsigned int> &nodeToDotInt ) const
{
  DSTART(
    "void WIR_SchedulingRegion::visualizeEdges(fstream&, const map<unsigned int, unsigned int>&) const" );

  BGL_FORALL_EDGES( e, mDGraph, DGraph ) {
    DGraphVertex src = source( e, mDGraph );
    DGraphVertex tgt = target( e, mDGraph );

    dotFile << nodeToDotInt.at( src ) << "->" << nodeToDotInt.at( tgt )
            << "[dir=forward,arrowhead=vee,arrowtail=none,label="
            << mDGraph[ e ].mLatency;

    if ( mDGraph[ e ].mType == WIR_DGEdgeType::war )
      dotFile << ",style=dashed";
    else

    if ( mDGraph[ e ].mType == WIR_DGEdgeType::waw )
      dotFile << ",style=dotted";
    else

    if ( mDGraph[ e ].mType == WIR_DGEdgeType::constr )
      dotFile << ",style=dotted,color=red";
    else

    if ( ( mDGraph[ e ].mType == WIR_DGEdgeType::ucert ) ||
         ( mDGraph[ e ].mType == WIR_DGEdgeType::ctrl ) )
      dotFile << ",style=bold";

    dotFile << "]" << endl << ";" << endl;
  }
};

}       // namespace WIR
