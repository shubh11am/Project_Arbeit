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
  @file wirbitdfa.cc
  @brief This file implements generic bit-true data flow analyses.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include standard headers
#include <functional>
#include <list>
#include <set>
#include <sstream>

// Include boost headers
#include <boost/graph/iteration_macros.hpp>

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
  Default constructor for function-level analysis.
*/
WIR_BitDFA::WIR_BitDFA( WIR_Function &f ) :
  WIR_Analysis { f },
  WIR_DFG { f }
{
  DSTART( "WIR_BitDFA::WIR_BitDFA(WIR_Function&)" );
};


WIR_BitDFA::~WIR_BitDFA( void )
{
  DSTART( "virtual WIR_BitDFA::~WIR_BitDFA()" );
};


/*
  getHierarchicalRegisterOffset computes the offset in bits by that a first
  register occurs in the hierarchy of a second register, or vice versa.
*/
unsigned int WIR_BitDFA::getHierarchicalRegisterOffset( const WIR_RegisterParameter &p1,
                                                        const WIR_RegisterParameter &p2 )
{
  DSTART(
    "static unsigned int WIR_BitDFA::getHierarchicalRegisterOffset(const WIR_RegisterParameter&, const WIR_RegisterParameter&)" );

  auto &r1 = p1.getRegister();
  auto &r2 = p2.getRegister();

  unsigned int offset = 0;

  if ( r1.getBitWidth() != r2.getBitWidth() ) {
    auto leafs1 = r1.getLeafs();
    auto leafs2 = r2.getLeafs();

    if ( r1.isChildOf( r2 ) )
      // r1 is a child of r2. So search the first leaf register of r1 in the
      // list of leafs of r2 and accumulate bit widths of r2 leaf registers.
      for ( auto it = leafs2.begin(); it->get() != leafs1.begin()->get(); ++it )
        offset += it->get().getBitWidth();
    else

    if ( r2.isChildOf( r1 ) )
      // r2 is a child of r1. So search the first leaf register of r2 in the
      // list of leafs of r1 and accumulate bit widths of r1 leaf registers.
      for ( auto it = leafs1.begin(); it->get() != leafs2.begin()->get(); ++it )
        offset += it->get().getBitWidth();
  }

  return( offset );
};


/*
  getHierarchicalUpDownValue returns an edge's up/down value under consideration
  of the edge's source and target parameter bit widths.

  While determining the up/down value for the current edge, the bit widths of
  the edge's source and target register parameters are considered and
  appropriate insert or extract operations are applied if required.

  If top-down analysis is considered, getHierarchicalUpDownValue behaves as
  follows:
  - If the edge's source register is wider than the target register, the
    location of target within the hierarchical source register is determined
    and exactly those bits are extracted from v and returned.
  - If otherwise the target register is wider than the source register, the
    location of source within the hierarchical target register is determined
    and v is inserted into an up/down value of target width into exactly
    that location.

  If bottom-up analysis is considered, getHierarchicalUpDownValue behaves as
  follows:
  - If the edge's source register is wider than the target register, the
    location of target within the hierarchical source register is determined
    and v is inserted into an up/down value of source width into exactly
    that location.
  - If otherwise the target register is wider than the source register, the
    location of source within the hierarchical target register is determined
    and exactly those bits are extracted from v and returned.
*/
WIR_UpDownValue WIR_BitDFA::getHierarchicalUpDownValue( const WIR_RegisterParameter &src,
                                                        const WIR_RegisterParameter &tgt,
                                                        const WIR_UpDownValue &v,
                                                        bool t )
{
  DSTART(
    "static WIR_UpDownValue WIR_BitDFA::getHierarchicalUpDownValue(const WIR_RegisterParameter&, const WIR_RegisterParameter&, const WIR_UpDownValue&, bool)" );

  unsigned int srcWidth = src.getRegister().getBitWidth();
  unsigned int tgtWidth = tgt.getRegister().getBitWidth();

  if ( srcWidth == tgtWidth )
    return( v );

  if ( t ) {
    // Top-down analysis: Value v flows from source to target node.

    // Source is wider than target. Thus, extract relevant bits of v.
    if ( srcWidth > tgtWidth ) {
      unsigned int offset = getHierarchicalRegisterOffset( src, tgt );
      return( v.extract( offset, tgtWidth ) );
    }

    // Target is wider than source. Thus, insert v into target.
    WIR_UpDownValue res { WIR_L4::bU, tgtWidth, v.isSigned() };
    insert( res, v, getHierarchicalRegisterOffset( src, tgt ) );
    return( res );
  } else {
    // Bottom-up analysis: Value v flows from target to source node.

    // Source is wider than target. Thus, insert v into source.
    if ( srcWidth > tgtWidth ) {
      WIR_UpDownValue res { WIR_L4::bU, srcWidth, v.isSigned() };
      insert( res, v, getHierarchicalRegisterOffset( src, tgt ) );
      return( res );
    }

    // Target is wider than source. Thus, extract relevant bits of v.
    unsigned int offset = getHierarchicalRegisterOffset( src, tgt );
    return( v.extract( offset, srcWidth ) );
  }
};


/*
  combineInEdge combines the up/down value of a node's incoming edge with
  previous up/down values, if multiple edges with potentially different up/down
  values refer to the very same parameter of a WIR operation.
*/
void WIR_BitDFA::combineInEdge( const WIR_Parameter &src,
                                const WIR_Parameter &tgt,
                                const WIR_UpDownValue &v,
                                std::map<WIR_id_t, WIR_UpDownValue> &operands,
                                std::map<WIR_id_t, WIR_UpDownValue> &initializedOperandBits )
{
  DSTART(
    "static void WIR_BitDFA::combineInEdge(const WIR_Parameter&, const WIR_Parameter&, const WIR_UpDownValue&, map<long long unsigned int, WIR_UpDownValue>&, map<long long unsigned int, WIR_UpDownValue>&)" );

  // Retrieve ID of the current edge's relevant target parameter.
  WIR_id_t pID = tgt.getID();

  auto it = operands.find( pID );

  DOUT( "Processing incoming edge '" << src << "' -> '" << tgt << "': " );

  bool edgeTypeOp =
    ( ( src.getType() == WIR_ParameterType::reg ) &&
      ( tgt.getType() == WIR_ParameterType::reg ) );

  if ( it == operands.end() ) {
    auto val =
      edgeTypeOp ?
        getHierarchicalUpDownValue(
          dynamic_cast<const WIR_RegisterParameter &>( src ),
          dynamic_cast<const WIR_RegisterParameter &>( tgt ), v ) : v;
    operands.insert( { pID, val } );
    DOUT(
      "Initializing operand value to '" << operands.at( pID ) << "'." <<
      endl );

    auto initIt = initializedOperandBits.insert( { pID, { val } } ).first;
    unsigned int pos =
      edgeTypeOp ?
        getHierarchicalRegisterOffset(
          dynamic_cast<const WIR_RegisterParameter &>( src ),
          dynamic_cast<const WIR_RegisterParameter &>( tgt ) ) : 0;
    for ( unsigned int i = 0; i < initIt->second.getBitWidth(); ++i )
      if ( pos >= initIt->second.getBitWidth() ) {
        if ( i < v.getBitWidth() )
          initIt->second.setBit( i, WIR_L4::bX );
        else
          initIt->second.setBit( i, WIR_L4::bU );
      } else {
        if ( ( i >= pos ) && ( i < v.getBitWidth() + pos ) )
          initIt->second.setBit( i, WIR_L4::bX );
        else
          initIt->second.setBit( i, WIR_L4::bU );
      }

    DOUT(
      "Marking of initialized bits: '" << initIt->second << "'." << endl );
  } else {
    auto initIt = initializedOperandBits.find( pID );
    unsigned int pos =
      edgeTypeOp ?
        getHierarchicalRegisterOffset(
          dynamic_cast<const WIR_RegisterParameter &>( src ),
          dynamic_cast<const WIR_RegisterParameter &>( tgt ) ) : 0;
    unsigned int width = v.getBitWidth();
    WIR_UpDownValue relevantBits { initIt->second };
    if ( relevantBits.getBitWidth() >= pos + width )
      relevantBits = initIt->second.extract( pos, width );

    DDECLARE( auto oldVal = it->second; );

    if ( relevantBits.containsOnlyBit( WIR_L4::bX ) )
      // If the relevant bit positions have already been initialized via
      // another incoming edge, we need to apply the combine operator.
      it->second = combine( it->second, v, pos );
    else
      // Otherwise, we simply insert the current up value at the relevant bit
      // positions.
      insert( it->second, v, pos );

    DOUT(
      "Combining previous operand value '" << oldVal << "' with '" << v <<
      "' to new operand value '" << it->second << "'." << endl );

    for ( unsigned int i = pos;
          ( i < initIt->second.getBitWidth() ) && ( i < width + pos ); ++i )
      initIt->second.setBit( i, WIR_L4::bX );
    DOUT(
      "Marking of initialized bits: '" << initIt->second << "'." << endl );
  }
};


/*
  combineOutEdge combines the up/down value of a node's outgoing edge with
  previous up/down values, if multiple edges with potentially different up/down
  values refer to the very same parameter of a WIR operation.
*/
void WIR_BitDFA::combineOutEdge( const WIR_RegisterParameter &src,
                                 const WIR_RegisterParameter &tgt,
                                 const WIR_UpDownValue &v,
                                 std::map<WIR_id_t, WIR_UpDownValue> &operands,
                                 std::map<WIR_id_t, WIR_UpDownValue> &initializedOperandBits )
{
  DSTART(
    "static void WIR_BitDFA::combineOutEdge(const WIR_RegisterParameter&, const WIR_RegisterParameter&, const WIR_UpDownValue&, map<long long unsigned int, WIR_UpDownValue>&, map<long long unsigned int, WIR_UpDownValue>&)" );

  // Retrieve ID of the current edge's relevant parameters.
  WIR_id_t pID = src.getID();

  #ifdef FAILSAFEMODE
  ufAssert( v.getBitWidth() == src.getRegister().getBitWidth() );
  #endif

  DOUT( "Processing outgoing edge '" << src << "' -> '" << tgt << "': " );

  // Determine the relevant bits flowing bottom-up from tgt to src.
  unsigned int pos =
    src.getRegister().getBitWidth() > tgt.getRegister().getBitWidth() ?
      getHierarchicalRegisterOffset( src, tgt ) : 0;
  auto relevantBits =
    src.getRegister().getBitWidth() > tgt.getRegister().getBitWidth() ?
      v.extract( pos, tgt.getRegister().getBitWidth() ) : v;

  auto it = operands.find( pID );

  if ( it == operands.end() ) {
    auto val = v;
    insert( val, relevantBits, pos );
    operands.insert( { pID, val } );
    DOUT(
      "Initializing operand value to '" << operands.at( pID ) << "'." <<
      endl );

    auto initIt = initializedOperandBits.insert( { pID, { val } } ).first;
    for ( unsigned int i = 0; i < initIt->second.getBitWidth(); ++i )
      if ( ( i >= pos ) && ( i < relevantBits.getBitWidth() + pos ) )
        initIt->second.setBit( i, WIR_L4::bX );
      else
        initIt->second.setBit( i, WIR_L4::bU );

    DOUT(
      "Marking of initialized bits: '" << initIt->second << "'." << endl );
  } else {
    auto initIt = initializedOperandBits.find( pID );
    auto initializedBits =
      initIt->second.extract( pos, relevantBits.getBitWidth() );

    DDECLARE( auto oldVal = it->second; );

    if ( initializedBits.containsOnlyBit( WIR_L4::bX ) )
      // If the relevant bit positions have already been initialized via
      // another outgoing edge, we need to apply the combine operator.
      it->second = combine( it->second, relevantBits, pos );
    else
      // Otherwise, we simply insert the current up value at the relevant bit
      // positions.
      insert( it->second, relevantBits, pos );

    DOUT(
      "Combining previous operand value '" << oldVal << "' with '" <<
      relevantBits << "' to new operand value '" << it->second << "'." <<
      endl );

    for ( unsigned int i = pos; i < relevantBits.getBitWidth(); ++i )
      initIt->second.setBit( i, WIR_L4::bX );
    DOUT(
      "Marking of initialized bits: '" << initIt->second << "'." << endl );
  }
};


//
// Protected class methods
//

/*
  runAnalysis performs bit-true data flow flow analysis of the given function.
*/
void WIR_BitDFA::runAnalysis( WIR_Function &f )
{
  DSTART( "virtual void WIR_BitDFA::runAnalysis(WIR_Function&)" );

  // Build data flow graph.
  build();

  // Initialize parameter-level data structures.
  init( f );

  // Do top-down analysis.
  topDownAnalysis();

  {
    DSTART(
      "virtual void WIR_BitDFA::runAnalysis(WIR_Function&).topdown.visualize" );
    DACTION( visualize(); );
  }

  // Copy computed down values into up values.
  BGL_FORALL_EDGES( e, mDGraph, DGraph )
    mDGraph[ e ].initUpValue();

  // Do bottom-up analysis.
  bottomUpAnalysis();

  {
    DSTART(
      "virtual void WIR_BitDFA::runAnalysis(WIR_Function&).bottomup.visualize" );
    DACTION( visualize(); );
  }

  // Attach all up/down values to WIR.
  createContainers();
};


/*
  getHierarchicalRegisterOffset computes the offset in bits by that the edge's
  source register occurs in the hierarchy of the edge's target register, or vice
  versa.
*/
unsigned int WIR_BitDFA::getHierarchicalRegisterOffset( DGraphEdge e ) const
{
  DSTART(
    "unsigned int WIR_BitDFA::getHierarchicalRegisterOffset(WIR_DFG::DGraphEdge) const" );

  auto &eInfo = mDGraph[ e ];

  // If the edge does not represent classical data flow between two operations
  // via (potentially hierarchical) registers, the offset naturally is 0.
  if ( eInfo.getType() != WIR_DFGNodeType::op )
    return( 0 );

  auto &srcP = eInfo.getSourceRegisterParameter();
  auto &tgtP = eInfo.getTargetRegisterParameter();

  return getHierarchicalRegisterOffset( srcP, tgtP );
};


/*
  getHierarchicalUpDownValue returns an edge's up/down value under consideration
  of the edge's source and target parameter bit widths.

  While determining the up/down value for the current edge, the bit widths of
  the edge's source and target register parameters are considered and
  appropriate insert or extract operations are applied if required.

  If top-down analysis is considered, getHierarchicalUpDownValue behaves as
  follows:
  - If the edge's source register is wider than the target register, the
    location of target within the hierarchical source register is determined
    and exactly those bits are extracted from v and returned.
  - If otherwise the target register is wider than the source register, the
    location of source within the hierarchical target register is determined
    and v is inserted into an up/down value of target width into exactly
    that location.

  If bottom-up analysis is considered, getHierarchicalUpDownValue behaves as
  follows:
  - If the edge's source register is wider than the target register, the
    location of target within the hierarchical source register is determined
    and v is inserted into an up/down value of source width into exactly
    that location.
  - If otherwise the target register is wider than the source register, the
    location of source within the hierarchical target register is determined
    and exactly those bits are extracted from v and returned.

  getHierarchicalUpDownValue asserts if it is invoked for an edge of type
  inequal 'op'.
*/
WIR_UpDownValue WIR_BitDFA::getHierarchicalUpDownValue( DGraphEdge e,
                                                        const WIR_UpDownValue &v,
                                                        bool t ) const
{
  DSTART(
    "WIR_UpDownValue WIR_BitDFA::getHierarchicalUpDownValue(WIR_DFG::DGraphEdge, const WIR_UpDownValue&, bool) const" );

  auto &eInfo = mDGraph[ e ];

  #ifdef FAILSAFEMODE
  ufAssert( eInfo.getType() == WIR_DFGNodeType::op );
  #endif

  return(
    getHierarchicalUpDownValue(
      eInfo.getSourceRegisterParameter(), eInfo.getTargetRegisterParameter(), v,
      t ) );
};


//
// Private class methods
//

/*
  init initializes data structures by attaching fresh containers to the register
  parameters of the specified WIR function.
*/
void WIR_BitDFA::init( WIR_Function &f )
{
  DSTART( "void WIR_BitDFA::init(WIR_Function&)" );

  // Clear previous analysis results by attaching fresh containers.
  for ( WIR_BasicBlock &b : f )
    for ( WIR_Instruction &i : b )
      for ( WIR_Operation &o : i )
        for ( WIR_Parameter &p : o )
          if ( ( p.getType() == WIR_ParameterType::reg ) ||
               ( p.getType() == WIR_ParameterType::imm ) )
            p.insertContainer( new WIR_BitValues() );
};


/*
  topDownAnalysis performs top-down data flow analysis of the current function.

  The work flow of top-down analysis is described in section 4.4.8 of J. Wagner.
  "Retargierbare Ausnutzung von Spezialoperationen für Eingebettete Systeme mit
  Hilfe bitgenauer Wertflussanalyse". Ph.D. thesis, Dortmund University, page
  173ff., 2006.
*/
void WIR_BitDFA::topDownAnalysis( void )
{
  DSTART( "void WIR_BitDFA::topDownAnalysis()" );

  // Keep track of which operations weren't yet analyzed, in order to catch all.
  WIR_OperationSet unvisitedOps;

  for ( WIR_BasicBlock &b : mFunction )
    for ( WIR_Instruction &i : b )
      for ( WIR_Operation &o : i )
        unvisitedOps.insert( o );

  // Put all DFG source nodes in both a work list and a set.
  list<reference_wrapper<WIR_Operation>> nodeWorkList;
  set<WIR_id_t> nodeIDs;

  for ( WIR_Operation &o : mSourceNodes ) {
    DOUT(
      "Adding source node '" << buildNodeName( mNodeByID[ o.getID() ] ) <<
      "' (ID " << o.getID() << ") to work list." << endl );

    nodeWorkList.push_back( o );
    nodeIDs.insert( o.getID() );
  }

  while ( !unvisitedOps.empty() ) {
    while ( !nodeWorkList.empty() ) {
      WIR_Operation &o = nodeWorkList.front();
      nodeWorkList.pop_front();
      nodeIDs.erase( o.getID() );
      DGraphVertex v = mNodeByID[ o.getID() ];
      unvisitedOps.erase( o );

      DOUT(
        "Removing node '" << buildNodeName( v ) << "' (ID " << o.getID() <<
        ") from work list." << endl );

      // Do top-down data flow analysis for the current node/operation.
      topDownAnalysis( v, o );

      // Iterate all outgoing edges of current node v.
      for ( auto p = out_edges( v, mDGraph ); p.first != p.second; ++p.first ) {
        DGraphEdge e = *(p.first);

        // For each out-edge not having reached a fixed point, add its target
        // node to the work list.
        if ( !mDGraph[ e ].isFix() ) {
          DGraphVertex tgt = target( e, mDGraph );
          WIR_Operation &tgtOp = mDGraph[ tgt ].getOperation();

          if ( !nodeIDs.count( tgtOp.getID() ) ) {
            DOUT(
              "Adding node '" << buildNodeName( tgt ) << "' (ID " <<
              tgtOp.getID() << ") to work list." << endl );

            nodeWorkList.push_back( tgtOp );
            nodeIDs.insert( tgtOp.getID() );
          }
        }
      }
    }

    // Check for unvisited nodes.
    if ( !unvisitedOps.empty() ) {
      auto &o = unvisitedOps.begin()->get();

      DOUT(
        "Adding unvisited node '" << buildNodeName( mNodeByID[ o.getID() ] ) <<
        "' (ID " << o.getID() << ") to work list." << endl );

      nodeWorkList.push_back( o );
      nodeIDs.insert( o.getID() );
    }
  }
};


/*
  topDownAnalysis performs top-down data flow analysis for the given DFG node
  and its associated WIR operation.
*/
void WIR_BitDFA::topDownAnalysis( DGraphVertex v, const WIR_Operation &o )
{
  DSTART(
    "void WIR_BitDFA::topDownAnalysis(WIR_DFG::DGraphVertex, const WIR_Operation&)" );

  DOUT(
    "Performing top-down analysis of node '" << buildNodeName( v ) <<
    "' (ID " << o.getID() << ")." << endl );

  map<WIR_id_t, WIR_UpDownValue> results;

  // Iterate all outgoing edges of the current node and reserve some storage.
  for ( auto p = out_edges( v, mDGraph ); p.first != p.second; ++p.first ) {
    DGraphEdge e = *(p.first);
    auto &eInfo = mDGraph[ e ];

    // Retrieve ID of the current edge's source parameter.
    auto &rp = eInfo.getSourceRegisterParameter();
    WIR_id_t pID = rp.getID();

    if ( !results.count( pID ) )
      results.insert(
        { pID, { WIR_L4::bU, rp.getRegister().getBitWidth(), false } } );
  }

  // If there are no results to be produced by top-down analysis, we can simply
  // skip this current node.
  if ( results.empty() ) {
    DOUT(
      "Skipping current node '" << buildNodeName( v ) << "' (ID " <<
      o.getID() << ") due to lacking out-edges." << endl );
    return;
  }

  // An operation's parameter can, depending on the operation's position in the
  // control flow, be reached via several DFG edges. Before the actual top-down
  // analysis of the current operation, the (possibly several) down values per
  // operation parameter have to be combined into one single down value per
  // parameter.
  map<WIR_id_t, WIR_UpDownValue> operands;
  bool allInEdgesFix = true;

  // initializedOperandBits is used for book-keeping. For hierarchical
  // registers, we need to keep track, which sub-parts of the hierarchical
  // registers have already been initialized explicitly and which other parts
  // not.
  map<WIR_id_t, WIR_UpDownValue> initializedOperandBits;

  // Iterate all incoming edges of current node v and combine their down values.
  for ( auto p = in_edges( v, mDGraph ); p.first != p.second; ++p.first ) {
    DGraphEdge e = *(p.first);
    auto &eInfo = mDGraph[ e ];

    if ( !eInfo.isFix() )
      allInEdgesFix = false;

    combineInEdge( e, operands, initializedOperandBits, true );
  }

  // Do the actual top-down simulation of o.
  DOUT( "Starting top-down simulation." << endl );
  simulateTopDown( o, operands, results );

  // Annotate simulation results back into the current node's outgoing edges.
  for ( auto p = out_edges( v, mDGraph ); p.first != p.second; ++p.first ) {
    DGraphEdge e = *(p.first);
    auto &eInfo = mDGraph[ e ];

    if ( !allInEdgesFix )
      eInfo.setFix( false );

    DOUT(
      "Processing outgoing edge '" << eInfo.getSourceRegisterParameter() <<
      "' -> '" << eInfo.getTargetRegisterParameter() << "': Setting result '" );

    // Retrieve ID of the current edge's source parameter.
    auto &rp = eInfo.getSourceRegisterParameter();
    WIR_id_t pID = rp.getID();

    WIR_UpDownValue res = results.at( pID );
    DOUT( res << "' / '" );

    // Replace U bits by locations produced by the current edge's source
    // parameter.
    res.replaceUByLocation( rp );
    DOUT( res << "' / '" );

    // Replace self-referencing locations (i.e., L or N bits refering to the
    // operation that is target of the current edge) by U bits.
    res.replaceLocationByU( eInfo.getTargetRegisterParameter().getOperation() );
    DOUT( res << "' to final down value '" );

    // Store the final result of top-down simulation in the current DFG edge
    // under consideration of potentially hierarchical source and target
    // registers.
    eInfo.setDownValue( move( res ) );
    DOUT( eInfo.getDownValue() << "'." << endl );
  }
};


/*
  bottomUpAnalysis performs bottom-up data flow analysis of the current
  function.

  The work flow of bottom-up analysis is described in section 4.4.8 of J.
  Wagner. "Retargierbare Ausnutzung von Spezialoperationen für Eingebettete
  Systeme mit Hilfe bitgenauer Wertflussanalyse". Ph.D. thesis, Dortmund
  University, page 173ff., 2006.
*/
void WIR_BitDFA::bottomUpAnalysis( void )
{
  DSTART( "void WIR_BitDFA::bottomUpAnalysis()" );

  // Keep track of which operations weren't yet analyzed, in order to catch all.
  WIR_OperationSet unvisitedOps;

  for ( WIR_BasicBlock &b : mFunction )
    for ( WIR_Instruction &i : b )
      for ( WIR_Operation &o : i )
        unvisitedOps.insert( o );

  // Put DFG nodes in both a work list and a set.
  list<reference_wrapper<WIR_Operation>> nodeWorkList;
  set<WIR_id_t> nodeIDs;

  for ( WIR_Operation &o : mSinkNodes ) {
    DOUT(
      "Adding sink node '" << buildNodeName( mNodeByID[ o.getID() ] ) <<
      "' (ID " << o.getID() << ") to work list." << endl );

    nodeWorkList.push_back( o );
    nodeIDs.insert( o.getID() );
  }

  while ( !unvisitedOps.empty() ) {
    while ( !nodeWorkList.empty() ) {
      WIR_Operation &o = nodeWorkList.front();
      nodeWorkList.pop_front();
      nodeIDs.erase( o.getID() );
      DGraphVertex v = mNodeByID[ o.getID() ];
      unvisitedOps.erase( o );

      DOUT(
        "Removing node '" << buildNodeName( v ) << "' (ID " << o.getID() <<
        ") from work list." << endl );

      // Do bottom-up data flow analysis for the current node/operation.
      bottomUpAnalysis( v, o );

      // Iterate all incoming edges of current node v.
      for ( auto p = in_edges( v, mDGraph ); p.first != p.second; ++p.first ) {
        DGraphEdge e = *(p.first);

        // For each in-edge not having reached a fixed point, add its source
        // node to the work list.
        if ( !mDGraph[ e ].isFix() ) {
          DGraphVertex src = source( e, mDGraph );

          if ( mDGraph[ src ].isOperation() ) {
            WIR_Operation &srcOp = mDGraph[ src ].getOperation();

            if ( !nodeIDs.count( srcOp.getID() ) ) {
              DOUT(
                "Adding node '" << buildNodeName( src ) << "' (ID " <<
                srcOp.getID() << ") to work list." << endl );

              nodeWorkList.push_back( srcOp );
              nodeIDs.insert( srcOp.getID() );
            }
          }
        }
      }
    }

    // Check for unvisited nodes.
    if ( !unvisitedOps.empty() ) {
      auto &o = unvisitedOps.begin()->get();

      DOUT(
        "Adding unvisited node '" << buildNodeName( mNodeByID[ o.getID() ] ) <<
        "' (ID " << o.getID() << ") to work list." << endl );

      nodeWorkList.push_back( o );
      nodeIDs.insert( o.getID() );
    }
  }
};


/*
  bottomUpAnalysis performs bottom-up data flow analysis for the given DFG node
  and its associated WIR operation.
*/
void WIR_BitDFA::bottomUpAnalysis( DGraphVertex v, const WIR_Operation &o )
{
  DSTART(
    "void WIR_BitDFA::bottomUpAnalysis(WIR_DFG::DGraphVertex, const WIR_Operation&)" );

  DOUT(
    "Performing bottom-up analysis of node '" << buildNodeName( v ) <<
    "' (ID " << o.getID() << ")." << endl );

  // An operation's parameter can, depending on the operation's position in the
  // control flow, be reached via several DFG edges. Before the actual bottom-up
  // analysis of the current operation, the (possibly several) up values per
  // operation parameter have to be combined into one single up value per
  // parameter.
  // Likewise for parameters associated with multiple incoming edges: Here
  // again, the up values of all these incoming edges have to be combined.
  map<WIR_id_t, WIR_UpDownValue> in;
  map<WIR_id_t, WIR_UpDownValue> out;

  // initializedOperandBits is used for book-keeping. For hierarchical
  // registers, we need to keep track, which sub-parts of the hierarchical
  // registers have already been initialized explicitly and which other parts
  // not.
  map<WIR_id_t, WIR_UpDownValue> initializedOperandBits;

  // Iterate all incoming edges of current node v and combine their up values.
  for ( auto p = in_edges( v, mDGraph ); p.first != p.second; ++p.first ) {
    DGraphEdge e = *(p.first);
    combineInEdge( e, in, initializedOperandBits, false );
  }

  // Iterate all outgoing edges of the current node and combine their up values.
  initializedOperandBits.clear();
  for ( auto p = out_edges( v, mDGraph ); p.first != p.second; ++p.first ) {
    DGraphEdge e = *(p.first);
    combineOutEdge( e, out, initializedOperandBits );
  }

  // Add default operand entries for parameters not connected via edges.
  for ( WIR_Parameter &p : o )
    if ( ( p.getType() == WIR_ParameterType::reg ) ||
         ( p.getType() == WIR_ParameterType::imm ) ) {
      unsigned int width =
        ( p.getType() == WIR_ParameterType::reg ) ?
          dynamic_cast<WIR_RegisterParameter &>(
            p ).getRegister().getBitWidth() :
          dynamic_cast<WIR_BaseImmediateParameter &>( p ).getBitWidth();

      bool isUsed =
        ( p.getType() == WIR_ParameterType::imm ) ||
        dynamic_cast<WIR_RegisterParameter &>( p ).isUsed() ||
        dynamic_cast<WIR_RegisterParameter &>( p ).isDefUsed();
      bool isDefined =
        ( p.getType() == WIR_ParameterType::reg ) &&
        ( dynamic_cast<WIR_RegisterParameter &>( p ).isDefined() ||
          dynamic_cast<WIR_RegisterParameter &>( p ).isDefUsed() );

      if ( isUsed && ( in.find( p.getID() ) == in.end() ) )
        in.insert( { p.getID(), { WIR_L4::bU, width } } );
      if ( isDefined && ( out.find( p.getID() ) == out.end() ) )
        out.insert( { p.getID(), { WIR_L4::bU, width } } );
    }

  // Do the actual bottom-up simulation of o.
  DOUT( "Starting bottom-up simulation." << endl );
  map<WIR_id_t, WIR_UpDownValue> results;
  simulateBottomUp( o, in, out, results );

  // Annotate simulation results back into the current node's incoming edges.
  for ( auto p = in_edges( v, mDGraph ); p.first != p.second; ++p.first ) {
    DGraphEdge e = *(p.first);
    auto &eInfo = mDGraph[ e ];

    // Retrieve ID of the current edge's target parameter.
    WIR_id_t pID = 0;
    if ( ( eInfo.getType() == WIR_DFGNodeType::op ) ||
         ( eInfo.getType() == WIR_DFGNodeType::reg ) )
      pID = eInfo.getTargetRegisterParameter().getID();
    else
      pID = eInfo.getImmediateParameter().getID();

    auto it = results.find( pID );

    // Mark unmodified incoming edges as fixed.
    if ( it == results.end() ) {
      eInfo.setFix();
      continue;
    }

    DACTION(
      DOUT( "Processing incoming edge '" );
      if ( eInfo.getType() == WIR_DFGNodeType::op )
        DOUT(
          eInfo.getSourceRegisterParameter() << "' -> '" <<
          eInfo.getTargetRegisterParameter() );
      else

      if ( eInfo.getType() == WIR_DFGNodeType::reg )
        DOUT( eInfo.getTargetRegisterParameter() ) ;
      else
        DOUT( eInfo.getImmediateParameter() );
      DOUT( "': Applying X mask '" << it->second << "' to '" ); );

    WIR_UpDownValue res = eInfo.getUpValue();
    DOUT( res << "': '" );

    // Adjust simulation result to potential register hierarchy along edge e.
    WIR_UpDownValue simVal =
      eInfo.getType() == WIR_DFGNodeType::op ?
        getHierarchicalUpDownValue( e, it->second, false ) : it->second;

    #ifdef FAILSAFEMODE
    ufAssert( res.getBitWidth() == simVal.getBitWidth() );
    #endif

    // Copy X bits from results map to the current edge's up value.
    for ( unsigned int i = 0; i < simVal.getBitWidth(); ++i )
      if ( simVal.at( i ) == WIR_L4::bX )
        res.setBit( i, WIR_L4::bX );

    // Store the final result of bottom-up simulation in the current DFG edge.
    eInfo.setUpValue( move( res ) );
    DOUT( eInfo.getUpValue() << "'." << endl );

    // Mark back edges as fix in order to avoid re-traversals of the current
    // DFG node.
    if ( eInfo.isBackEdge() )
      eInfo.setFix();
  }
};


/*
  combineInEdge combines the up/down value of a node's incoming edge with
  previous up/down values, if multiple edges with potentially different up/down
  values refer to the very same parameter of a WIR operation.
*/
void WIR_BitDFA::combineInEdge( const DGraphEdge e,
                                std::map<WIR_id_t, WIR_UpDownValue> &operands,
                                std::map<WIR_id_t, WIR_UpDownValue> &initializedOperandBits,
                                bool d ) const
{
  DSTART(
    "void WIR_BitDFA::combineInEdge(WIR_DFG::DGraphEdge, map<long long unsigned int, WIR_UpDownValue>&, map<long long unsigned int, WIR_UpDownValue>&, bool) const" );

  auto &eInfo = const_cast<WIR_DFGEdgeProperty &>( mDGraph[ e ] );

  // Retrieve the edge's up/down value.
  auto &v = d ? eInfo.getDownValue() : eInfo.getUpValue();

  auto &src =
    ( ( eInfo.getType() == WIR_DFGNodeType::op ) ||
      ( eInfo.getType() == WIR_DFGNodeType::reg ) ) ?
      static_cast<const WIR_Parameter &>( eInfo.getSourceRegisterParameter() ) :
      static_cast<const WIR_Parameter &>( eInfo.getImmediateParameter() );
  auto &tgt =
    ( ( eInfo.getType() == WIR_DFGNodeType::op ) ||
      ( eInfo.getType() == WIR_DFGNodeType::reg ) ) ?
      eInfo.getTargetRegisterParameter() : src;

  combineInEdge( src, tgt, v, operands, initializedOperandBits );
};


/*
  combineOutEdge combines the up value of a node's outgoing edge with previous
  up values, if multiple edges with potentially different up values refer to the
  very same parameter of a WIR operation.
*/
void WIR_BitDFA::combineOutEdge( const DGraphEdge e,
                                 std::map<WIR_id_t, WIR_UpDownValue> &operands,
                                 std::map<WIR_id_t, WIR_UpDownValue> &initializedOperandBits ) const
{
  DSTART(
    "void WIR_BitDFA::combineOutEdge(WIR_DFG::DGraphEdge, map<long long unsigned int, WIR_UpDownValue>&, map<long long unsigned int, WIR_UpDownValue>&) const" );

  auto &eInfo = const_cast<WIR_DFGEdgeProperty &>( mDGraph[ e ] );

  // Retrieve the current edge's relevant parameters.
  auto &src = eInfo.getSourceRegisterParameter();
  auto &tgt = eInfo.getTargetRegisterParameter();

  combineOutEdge(
    src, tgt, eInfo.getUpValue(), operands, initializedOperandBits );
};


/*
  combine returns the "smallest common" combination of two up/down values.

  The com operator is defined in Jens Wagner, Retargierbare Ausnutzung von
  Spezialoperationen für Eingebettete Systeme mit Hilfe bitgenauer
  Wertflussanalyse, page 177, Figure 4.19.
*/
WIR_UpDownValue WIR_BitDFA::combine( const WIR_UpDownValue &v1,
                                     const WIR_UpDownValue &v2, unsigned int o )
{
  DSTART(
    "static WIR_UpDownValue WIR_BitDFA::combine(const WIR_UpDownValue&, const WIR_UpDownValue&, unsigned int)" );

  // If both operands to be combined have the same bit width, we just combine
  // them and don't need to do any offset handling for hierarchical registers.
  if ( v1.getBitWidth() == v2.getBitWidth() )
    o = 0;

  DOUT(
    "Combining '" << v1 << "' with '" << v2 << "' at offset " << o << endl );

  // Determine which of the two operands is the smaller one.
  auto &small = ( v1.getBitWidth() < v2.getBitWidth() ) ? v1 : v2;
  auto &large = ( v1.getBitWidth() < v2.getBitWidth() ) ? v2 : v1;

  WIR_UpDownValue res { large };

  // Extract only the relevant bits from the large operand.
  auto relevant { large.extract( o, small.getBitWidth() ) };

  // Combine them with the small operand.
  auto combination { small.combine( relevant ) };

  // Insert the bitwise combination into the result up/down value.
  insert( res, combination, o );
  return( res );
};


/*
  createContainers takes the up/down values from the data flow graph and
  attaches them persistently to the current WIR function using bit-value
  containers.
*/
void WIR_BitDFA::createContainers( void )
{
  DSTART( "void WIR_BitDFA::createContainers()" );

  // Iterate all DFG edges.
  BGL_FORALL_EDGES( e, mDGraph, DGraph ) {
    auto &eInfo = mDGraph[ e ];

    if ( eInfo.getType() == WIR_DFGNodeType::op ) {
      // e is a regular DFG edge from one defined register parameter to another
      // used register parameter.
      auto &src = eInfo.getSourceRegisterParameter();
      auto &tgt = eInfo.getTargetRegisterParameter();

      // Determine containers of both register parameters.
      auto &srcContainer = src.getContainers<WIR_BitValues>().begin()->get();
      auto &tgtContainer = tgt.getContainers<WIR_BitValues>().begin()->get();

      // Store up/down values in the source node's container.
      srcContainer.insertOutValues(
        const_cast<WIR_RegisterParameter &>( tgt ) ,
        WIR_UpDownValue( eInfo.getDownValue() ),
        WIR_UpDownValue( eInfo.getUpValue() ) );

      // Store up/down values in the target node's container.
      tgtContainer.insertInValues(
        const_cast<WIR_RegisterParameter &>( src ),
        move( eInfo.getDownValue() ), move( eInfo.getUpValue() ) );
    } else

    if ( eInfo.getType() == WIR_DFGNodeType::reg ) {
      // e is an incoming DFG edge for a register parameter denoting graph
      // input.
      auto &tgt = eInfo.getTargetRegisterParameter();

      // Determine container of the involved register parameter.
      auto &tgtContainer = tgt.getContainers<WIR_BitValues>().begin()->get();

      // Store up/down values in the target node's container.
      tgtContainer.insertInValues(
        const_cast<WIR_RegisterParameter &>( tgt ),
        move( eInfo.getDownValue() ), move( eInfo.getUpValue() ) );
    } else

    if ( eInfo.getType() == WIR_DFGNodeType::imm ) {
      // e is an incoming DFG edge for an immediate parameter.
      auto &tgt = eInfo.getImmediateParameter();

      // Determine container of immediate parameter.
      auto &tgtContainer = tgt.getContainers<WIR_BitValues>().begin()->get();

      // Store up/down values in the target node's container.
      tgtContainer.insertInValues(
        const_cast<WIR_BaseImmediateParameter &>( tgt ),
        move( eInfo.getDownValue() ), move( eInfo.getUpValue() ) );
    }
  }
};

}       // namespace WIR
