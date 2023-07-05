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
  @file wirinterferencegraph.cc
  @brief This file implements interference graphs.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include standard headers
#include <stdlib.h>
#include <sstream>
#include <string>
#include <unistd.h>
#include <utility>

// Include boost headers
#include <boost/current_function.hpp>
#include <boost/graph/copy.hpp>
#include <boost/graph/iteration_macros.hpp>
#include <boost/graph/graphviz.hpp>
#include <boost/tuple/tuple.hpp>

// Include libuseful headers
#include <libuseful/debugmacros.h>
#include <libuseful/exceptions.h>
#include <libuseful/io.h>

// Include WIR headers
#include <wir/wir.h>

// Include local headers
#include "wirinterferencegraph.h"


//
// Code section
//

namespace WIR {


using namespace boost;
using namespace std;


//! graphvizColors lists all named graphviz colors in a simple constant array.
static const string graphvizColors[ 283 ] = {
  "coral1", "beige", "darkorange1", "darkgoldenrod1", "chartreuse1",
  "aquamarine1", "aliceblue", "blueviolet", "crimson", "brown1", "orange1",
  "gold1", "darkgreen", "cyan1", "blue1", "darkorchid1", "darksalmon",
  "burlywood1", "orangered1", "goldenrod1", "darkolivegreen1",
  "darkturquoise", "blueviolet", "darkviolet", "deeppink1", "chocolate1",
  "darkorange2", "greenyellow", "darkseagreen1", "lightcyan1", "cadetblue1",
  "magenta1", "firebrick1", "darkkhaki", "orange2", "lightgoldenrod1",
  "forestgreen", "mediumaquamarine", "cornflowerblue", "mediumorchid1",
  "hotpink1", "khaki1", "orangered2", "lightgoldenrodyellow", "green1",
  "mediumturquoise", "darkslateblue", "mediumpurple1", "indianred1", "peru",
  "darkorange3", "lightyellow1", "greenyellow", "paleturquoise1",
  "deepskyblue1", "mediumvioletred", "lightpink1", "rosybrown1", "orange3",
  "palegoldenrod", "lawngreen", "turquoise1", "dodgerblue1", "orchid1",
  "lightsalmon1", "saddlebrown", "orangered3", "yellow1", "lightseagreen",
  "aquamarine2", "indigo", "plum1", "maroon1", "sandybrown", "darkorange4",
  "yellowgreen", "limegreen", "cyan2", "lightblue1", "purple1",
  "mediumvioletred", "sienna1", "orange4", "darkgoldenrod2", "mediumseagreen",
  "lightcyan2", "lightskyblue1", "violet", "orangered1", "tan1", "orangered4",
  "gold2", "mediumspringgreen", "paleturquoise2", "darkorchid2",
  "palevioletred1", "brown2", "goldenrod2", "mintcream", "turquoise2",
  "mediumblue", "magenta2", "pink1", "burlywood2", "lightgoldenrod2",
  "olivedrab1", "aquamarine3", "mediumslateblue", "mediumorchid2", "red1",
  "chocolate2", "lightyellow2", "palegreen1", "cyan3", "midnightblue",
  "mediumpurple2", "salmon1", "khaki2", "yellow2", "seagreen1", "lightcyan3",
  "navy", "orchid2", "tomato1", "rosybrown2", "darkgoldenrod3",
  "springgreen1", "paleturquoise3", "navyblue", "plum2", "violetred1",
  "sienna2", "gold3", "yellowgreen", "turquoise3", "powderblue", "purple2",
  "coral2", "tan2", "goldenrod3", "chartreuse2", "aquamarine4", "royalblue1",
  "darkorchid3", "deeppink2", "brown3", "lightgoldenrod3", "darkolivegreen2",
  "cyan4", "skyblue1", "magenta3", "firebrick2", "burlywood3", "lightyellow3",
  "darkseagreen2", "lightcyan4", "slateblue1", "mediumorchid3", "hotpink2",
  "chocolate3", "yellow3", "green2", "paleturquoise4", "steelblue1",
  "mediumpurple3", "indianred2", "khaki3", "darkgoldenrod4", "olivedrab2",
  "turquoise4", "blue2", "orchid3", "lightpink2", "rosybrown3", "gold4",
  "palegreen2", "cadetblue2", "plum3", "lightsalmon2", "sienna3",
  "goldenrod4", "seagreen2", "deepskyblue2", "purple3", "maroon2", "tan3",
  "lightyellow4", "springgreen2", "dodgerblue2", "darkorchid4", "orangered2",
  "brown4", "yellow4", "chartreuse3", "lightblue2", "magenta4",
  "palevioletred2", "burlywood4", "darkolivegreen3", "lightskyblue2",
  "mediumorchid4", "pink2", "chocolate4", "darkseagreen3", "mediumpurple4",
  "red2", "khaki4", "green3", "royalblue2", "orchid4", "salmon2",
  "rosybrown4", "olivedrab3", "skyblue2", "plum4", "tomato2", "sienna4",
  "palegreen3", "slateblue2", "purple4", "violetred2", "tan4", "seagreen3",
  "steelblue2", "coral3", "springgreen3", "blue3", "deeppink3", "chartreuse4",
  "cadetblue3", "firebrick3", "darkolivegreen4", "deepskyblue3", "hotpink3",
  "darkseagreen4", "dodgerblue3", "indianred3", "green4", "lightblue3",
  "lightpink3", "olivedrab4", "lightskyblue3", "lightsalmon3", "palegreen4",
  "maroon3", "seagreen4", "royalblue3", "orangered3", "springgreen4",
  "skyblue3", "palevioletred3", "slateblue3", "pink3", "steelblue3", "red3",
  "blue4", "salmon3", "cadetblue4", "tomato3", "deepskyblue4", "violetred3",
  "dodgerblue4", "coral4", "lightblue4", "deeppink4", "lightskyblue4",
  "firebrick4", "hotpink4", "royalblue4", "indianred4", "skyblue4",
  "lightpink4", "slateblue4", "lightsalmon4", "steelblue4", "maroon4",
  "orangered4", "palevioletred4", "pink4", "red4", "salmon4", "tomato4",
  "violetred4"
};


//
// Public class methods
//

/*
  Default constructor for a given WIR function.

  This constructor creates a dedicated node for each physical register of phregs
  in the interference graph, and each such node gets its own unique color right
  from the beginning. Furthermore, the number of created physical registers
  determines the number of available colors for the entire interference graph,
  i.e., the value returned by getAvailableColors().

  For all virtual registers contained in f and optionally constrained by set
  vregs, this constructor also adds dedicated uncolored nodes to the
  interference graph.
*/
WIR_InterferenceGraph::WIR_InterferenceGraph( const std::list<std::reference_wrapper<const WIR_PhysicalRegister>> &phregs,
                                              WIR_Function &f,
                                              const WIR_VirtualRegisterSet &vregs,
                                              bool verbosity,
                                              bool keepTmpFiles ) :
  mPhregs { phregs },
  mFunction { &f },
  mVerbosity { verbosity },
  mKeepTmpFiles { keepTmpFiles },
  mAvailableColors { 0 }
{
  DSTART(
    "WIR_InterferenceGraph::WIR_InterferenceGraph(const list<reference_wrapper<const WIR_PhysicalRegister> >&, WIR_Function&, const WIR_VirtualRegisterSet&, bool, bool)" );

  // Add nodes for the processor's physical registers to the interference graph
  // and assign a unique color to each of these interference graph nodes.
  addNodesForPHREGs();

  // Add nodes for the function's virtual registers to the interference graph.
  addNodesForVREGs( vregs );
};


/*
  Constructor for a completely empty interference graph.

  This constructor produces a completely empty interference graph with no nodes
  for both physical and virtual registers, and with no associated WIR elements.

  The use of this constructor is strongly discouraged, you are requested to use
  one of the other constructors above. The only scenario where to use this
  discouraged constructor is to create an empty graph that shall later be filled
  using the '=' operator below.
*/
WIR_InterferenceGraph::WIR_InterferenceGraph( void ) :
  mFunction { nullptr },
  mVerbosity { false },
  mKeepTmpFiles { false },
  mAvailableColors { 0 }
{
  DSTART( "WIR_InterferenceGraph::WIR_InterferenceGraph()" );
};


/*
  Copy constructor.
*/
WIR_InterferenceGraph::WIR_InterferenceGraph( const WIR_InterferenceGraph &__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  // Clear the target graph first.
  set<IGraphVertex> removedVertices;
  BGL_FORALL_VERTICES( v, mIGraph, IGraph )
    removedVertices.insert( v );
  for ( auto v : removedVertices ) {
    clear_vertex( v, mIGraph );
    remove_vertex( v, mIGraph );
  }

  // Copy the underlying Boost graph into the empty target graph afterwards.
  using vertex_map = map<IGraphVertex, IGraphVertex>;
  vertex_map vertexMap;
  associative_property_map<vertex_map> vertexMapWrapper( vertexMap );

  copy_graph( __o.mIGraph, mIGraph, boost::orig_to_copy( vertexMapWrapper ) );

  // Next, copy the register-to-node map.
  mNodeByRegister.clear();
  for ( auto p : __o.mNodeByRegister ) {
    auto id = p.first;
    IGraphVertex vOrig = p.second;
    IGraphVertex vCopy = vertexMap[ vOrig ];

    mNodeByRegister[ id ] = vCopy;
  }

  // Finally, copy the "simple" attributes of the class.
  mPhregs = __o.mPhregs;
  mFunction = __o.mFunction;
  mVerbosity = __o.mVerbosity;
  mKeepTmpFiles = __o.mKeepTmpFiles;
  mColorOfPhreg = __o.mColorOfPhreg;
  mPhregOfColor = __o.mPhregOfColor;
  mAvailableColors = __o.mAvailableColors;
  mAliases = __o.mAliases;
  mNodeStack = __o.mNodeStack;
  mPriorityNodeStack = __o.mPriorityNodeStack;
};


/*
  Destructor.
*/
WIR_InterferenceGraph::~WIR_InterferenceGraph( void )
{
  DSTART( "WIR_InterferenceGraph::~WIR_InterferenceGraph()" );
};


/*
  Copy-assignment operator.
*/
WIR_InterferenceGraph & WIR_InterferenceGraph::operator = ( const WIR_InterferenceGraph &__o )
{
  DSTART(
    "WIR_InterferenceGraph& WIR_InterferenceGraph::operator=(const WIR_InterferenceGraph&)" );

  // Clear the target graph first.
  set<IGraphVertex> removedVertices;
  BGL_FORALL_VERTICES( v, mIGraph, IGraph )
    removedVertices.insert( v );
  for ( auto v : removedVertices ) {
    clear_vertex( v, mIGraph );
    remove_vertex( v, mIGraph );
  }

  // Copy the underlying Boost graph into the empty target graph afterwards.
  using vertex_map = map<IGraphVertex, IGraphVertex>;
  vertex_map vertexMap;
  associative_property_map<vertex_map> vertexMapWrapper( vertexMap );

  copy_graph( __o.mIGraph, mIGraph, boost::orig_to_copy( vertexMapWrapper ) );

  // Next, copy the register-to-node map.
  mNodeByRegister.clear();
  for ( auto p : __o.mNodeByRegister ) {
    auto id = p.first;
    IGraphVertex vOrig = p.second;
    IGraphVertex vCopy = vertexMap[ vOrig ];

    mNodeByRegister[ id ] = vCopy;
  }

  // Finally, copy the "simple" attributes of the class.
  mPhregs = __o.mPhregs;
  mFunction = __o.mFunction;
  mVerbosity = __o.mVerbosity;
  mKeepTmpFiles = __o.mKeepTmpFiles;
  mColorOfPhreg = __o.mColorOfPhreg;
  mPhregOfColor = __o.mPhregOfColor;
  mAvailableColors = __o.mAvailableColors;
  mAliases = __o.mAliases;
  mNodeStack = __o.mNodeStack;
  mPriorityNodeStack = __o.mPriorityNodeStack;

  return( *this );
};


/*
  containsNode checks whether a node for the specified WIR register already
  exists in the interference graph.
*/
bool WIR_InterferenceGraph::containsNode( const WIR_BaseRegister &r ) const
{
  DSTART(
    "bool WIR_InterferenceGraph::containsNode(const WIR_BaseRegister&) const" );

  return( mNodeByRegister.find( r.getID() ) != mNodeByRegister.end() );
};


/*
  areSameNodes checks whether the interference graph nodes for the two specified
  registers are the same.
*/
bool WIR_InterferenceGraph::areSameNodes( const WIR_BaseRegister &r1,
                                          const WIR_BaseRegister &r2 ) const
{
  DSTART(
    "bool WIR_InterferenceGraph::areSameNodes(const WIR_BaseRegister&, const WIR_BaseRegister&) const" );

  auto i1 = mNodeByRegister.find( r1.getID() );
  auto i2 = mNodeByRegister.find( r2.getID() );

  if ( ( i1 == mNodeByRegister.end() ) || ( i2 == mNodeByRegister.end() ) )
    return( false );

  return( i1->second == i2->second );
};


/*
  getDegree returns the degree of the interference graph node specified by the
  given WIR register.
*/
unsigned int WIR_InterferenceGraph::getDegree( const WIR_BaseRegister &r ) const
{
  DSTART(
    "unsigned int WIR_InterferenceGraph::getDegree(const WIR_BaseRegister&) const" );

  // Determine the corresponding boost graph node.
  IGraphVertex v = mNodeByRegister.at( r.getID() );

  return( mIGraph[ v ].mDegree );
};


/*
  getNeighbors returns a set of all neighboring registers.

  The set returned by this method includes both virtual and physical neighboring
  registers. Furthermore, only such nodes which have not been pushed onto the
  stack are included. getNeighbors only returns the component mRegister of all
  neighboring graph nodes.
*/
WIR_RegisterSet WIR_InterferenceGraph::getNeighbors( const WIR_BaseRegister &r ) const
{
  DSTART(
    "WIR_RegisterSet WIR_InterferenceGraph::getNeighbors(const WIR_BaseRegister&) const" );

  WIR_RegisterSet res;

  // Determine the corresponding boost graph node.
  IGraphVertex v = mNodeByRegister.at( r.getID() );

  // Return the empty set if the current node is pushed onto the stack.
  if ( mIGraph[ v ].mIsPushed )
    return( res );

  // Iterate over all nodes w adjacent to v and add them to the result.
  BGL_FORALL_ADJ( v, w, mIGraph, IGraph ) {
    DOUT(
      "Checking edge {" << buildNodeName( v ) << ", " << buildNodeName( w ) <<
      "}." << endl );

    if ( !mIGraph[ w ].mIsPushed )
      res.insert( mIGraph[ w ].mRegister.get() );
  }

  return( res );
};


/*
  getNeighborVREGs returns a set of all neighboring virtual registers.

  The set returned by this method includes only virtual neighboring registers.
  Furthermore, only such nodes which have not been pushed onto the stack are
  included. getNeighborVREGs only returns the component mRegister of all
  neighboring graph nodes.
*/
WIR_VirtualRegisterSet WIR_InterferenceGraph::getNeighborVREGs( const WIR_BaseRegister &r ) const
{
  DSTART(
    "WIR_VirtualRegisterSet WIR_InterferenceGraph::getNeighborVREGs(const WIR_BaseRegister&) const" );

  WIR_VirtualRegisterSet res;

  // Determine the corresponding boost graph node.
  IGraphVertex v = mNodeByRegister.at( r.getID() );

  // Return the empty set if the current node is pushed onto the stack.
  if ( mIGraph[ v ].mIsPushed )
    return( res );

  // Iterate over all nodes w adjacent to v and add them to the result.
  BGL_FORALL_ADJ( v, w, mIGraph, IGraph ) {
    DOUT(
      "Checking edge {" << buildNodeName( v ) << ", " << buildNodeName( w ) <<
      "}." << endl );

    if ( !mIGraph[ w ].mIsPushed &&
         mIGraph[ w ].mRegister.get().get().isVirtual() )
      res.insert(
        dynamic_cast<WIR_VirtualRegister &>(
          mIGraph[ w ].mRegister.get().get() ) );
  }

  return( res );
};


/*
  setLoopNestingDepth sets the maximal loop depth in which the specified
  register is defined or used.
*/
void WIR_InterferenceGraph::setLoopNestingDepth( const WIR_BaseRegister &r,
                                                 unsigned int depth )
{
  DSTART(
    "void WIR_InterferenceGraph::setLoopNestingDepth(const WIR_BaseRegister&, unsigned int)" );

  // Determine the corresponding boost graph node.
  IGraphVertex v = mNodeByRegister.at( r.getID() );

  // Set the graph node's attribute.
  mIGraph[ v ].mLoopNestingDepth = depth;
};


/*
  getLoopNestingDepth returns the maximal loop depth in which the specified
  register is defined or used.
*/
unsigned int WIR_InterferenceGraph::getLoopNestingDepth( const WIR_BaseRegister &r ) const
{
  DSTART(
    "unsigned int WIR_InterferenceGraph::getLoopNestingDepth(const WIR_BaseRegister&) const" );

  // Determine the corresponding boost graph node.
  IGraphVertex v = mNodeByRegister.at( r.getID() );

  // Retrieve the graph node's attribute.
  return( mIGraph[ v ].mLoopNestingDepth );
};


/*
  visualize dumps the current interference graph into a DOT file and invokes
  xdot on it.
*/
void WIR_InterferenceGraph::visualize( bool allNodes ) const
{
  DSTART( "void WIR_InterferenceGraph::visualize(bool) const" );

  char mask[12] = "/tmp/XXXXXX";
  int fileDescriptor = mkstemp( mask );

  if ( fileDescriptor == -1 )
    throw ufFatalError( "Failed to allocate temporary file." );
  else
    close( fileDescriptor );

  string fileName( mask );
  fstream dotFile( fileName, ios_base::out );

  // Write dot graph header.
  dotFile << "graph G {" << endl << "graph [rotate=90]" << endl;

  // Write dot graph nodes.
  map<WIR_id_t, unsigned int> nodeToDotInt;
  visualizeNodes( dotFile, nodeToDotInt, allNodes );

  // Write dot graph edges.
  visualizeEdges( dotFile, nodeToDotInt, allNodes );

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
  addInterference adds interference edges between the nodes specified by the
  given WIR registers.

  Using the parameter e, it can be specified how many parallel edges between two
  nodes will be inserted. This might be necessary for graph nodes representing
  hierarchical registers. In such a case, the interference graph becomes a
  multi-graph. When using the default value 0 for e, addInterference
  automatically determines how many childs a hierarchical node contains and
  inserts exactly this number of parallel edges. If this automatic behavior is
  undesired, appropriate numbers of parallel edges can be specified explicitly.
*/
void WIR_InterferenceGraph::addInterference( const WIR_BaseRegister &r1,
                                             const WIR_BaseRegister &r2,
                                             unsigned int e )
{
  DSTART(
    "void WIR_InterferenceGraph::addInterference(const WIR_BaseRegister&, const WIR_BaseRegister&, unsigned int)" );

  // Determine the corresponding boost graph nodes.
  IGraphVertex v1 = mNodeByRegister.at( r1.getID() );
  IGraphVertex v2 = mNodeByRegister.at( r2.getID() );

  // Avoid self-loops.
  if ( v1 == v2 )
    return;

  // Determine how many edges have to be inserted between v1 and v2.
  unsigned int edges = e;

  if ( edges == 0 ) {
    unsigned int e2;

    // Determine how many edges are required due to node v1.
    if ( r1.isHierarchical() )
      edges = mIGraph[ v1 ].mLeafRegisters.size();
    else
      edges = 1;

    // Determine how many edges are required due to node v2.
    if ( r2.isHierarchical() )
      e2 = mIGraph[ v2 ].mLeafRegisters.size();
    else
      e2 = 1;

    // Take the maximum value.
    if ( e2 > edges )
      edges = e2;
  }

  // Create the determined amount of edges.
  for ( unsigned int i = 0; i < edges; ++i ) {
    DOUT(
      "Adding edge " << mIGraph[ v1 ].mRegister.get().get().getName() <<
      " <-> " << mIGraph[ v2 ].mRegister.get().get().getName() << endl );
    add_edge( v1, v2, mIGraph );
    mIGraph[ v1 ].mDegree++;
    mIGraph[ v2 ].mDegree++;
  }
};


/*
  interfere returns whether the nodes represented by the specified WIR registers
  interfere.

  Interference is indicated if there is an edge between the two nodes in the
  interference graph, and this edge is not pushed onto the stack.
*/
bool WIR_InterferenceGraph::interfere( const WIR_BaseRegister &r1,
                                       const WIR_BaseRegister &r2 ) const
{
  DSTART(
    "bool WIR_InterferenceGraph::interfere(const WIR_BaseRegister&, const WIR_BaseRegister&) const" );

  // Determine the corresponding boost graph nodes.
  IGraphVertex v1 = mNodeByRegister.at( r1.getID() );
  IGraphVertex v2 = mNodeByRegister.at( r2.getID() );

  // Check the existence of an edge e={v1,v2}.
  IGraphEdge e;
  bool edgeExists;

  tuples::tie( e, edgeExists ) = edge( v1, v2, mIGraph );

  // The two nodes v1 and v2 interfere if there is an edge between them in the
  // interference graph that is not pushed onto the stack.
  return( edgeExists && !mIGraph[ e ].mIsPushed );
};


/*
  getAvailableColors returns the number of colors available to color the
  interference graph.
*/
unsigned int WIR_InterferenceGraph::getAvailableColors() const
{
  DSTART( "unsigned int WIR_InterferenceGraph::getAvailableColors() const" );

  return( mAvailableColors );
};


/*
  getColorOfPhreg returns the color number internally assigned to the specified
  physical register.
*/
unsigned int WIR_InterferenceGraph::getColorOfPhreg( const WIR_PhysicalRegister &r ) const
{
  DSTART(
    "unsigned int WIR_InterferenceGraph::getColorOfPhreg(const WIR_PhysicalRegister&) const" );

  // Determine the corresponding boost graph node.
  IGraphVertex v = mNodeByRegister.at( r.getID() );

  WIR_BaseRegister &key = mIGraph[ v ].mRegister.get().get();
  return( mColorOfPhreg.at( key.getID() ) );
};


/*
  getPhregOfColor returns the physical register assigned to the specified color.
*/
WIR_PhysicalRegister &WIR_InterferenceGraph::getPhregOfColor( unsigned int c ) const
{
  DSTART(
    "WIR_PhysicalRegister& WIR_InterferenceGraph::getPhregOfColor(unsigned int) const" );

  #ifdef FAILSAFEMODE
  ufAssert( ( c >= 1 ) && ( c <= getAvailableColors() ) );
  #endif

  return( mPhregOfColor.at( c ).get() );
};


/*
  setColor colors the node of the specified %WIR register with the given color.

  The parameter c must be in the interval [0, getAvailableColors()]. Assigning a
  color from [1, getAvailableColors()] implies that the corresponding node is
  unmarked as potential and actual spill. Assigning a color 0 to a register
  means to uncolor a node.
*/
void WIR_InterferenceGraph::setColor( const WIR_BaseRegister &r,
                                      unsigned int c )
{
  DSTART(
    "void WIR_InterferenceGraph::setColor(const WIR_BaseRegister&, unsigned int)" );

  #ifdef FAILSAFEMOE
  ufAssert( containsNode( r ) && ( c <= getAvailableColors() ) );
  #endif

  DOUT(
    "Setting color of register '" << r.getName() << "' to " << c << "." <<
    endl );

  // Determine the corresponding boost graph node.
  IGraphVertex v = mNodeByRegister[ r.getID() ];

  if ( !r.isHierarchical() ) {
    // For a simple, non-hierarchical register, insert its one and only unique
    // color.

    if ( c != 0 )
      mIGraph[ v ].mColors[ r.getID() ] = c;
    else
      mIGraph[ v ].mColors.clear();
  } else {
    #ifdef FAILSAFEMOE
    // For a virtual hierarchical register, ensure that nobody attempts to
    // assign a color to the hierarchy's root node, as only the leafs are
    // colorable.
    ufAssert(
      ( r.isVirtual() && ( r != mIGraph[ v ].mRegister.get().get() ) ) ||
      r.isPhysical() );
    #endif

    if ( c != 0 )
      mIGraph[ v ].mColors[ r.getID() ] = c;
    else {
      // Uncolor the node, i.e., remove its entry from the mColors map.
      auto it = mIGraph[ v ].mColors.find( r.getID() );

      if ( it != mIGraph[ v ].mColors.end() )
        mIGraph[ v ].mColors.erase( it );
    }
  }

  mIGraph[ v ].mIsPotentialSpill = mIGraph[ v ].mIsActualSpill = false;
};


/*
  getColors returns the set of colors assigned to the node of the specified WIR
  register.
*/
set<unsigned int> WIR_InterferenceGraph::getColors( const WIR_BaseRegister &r ) const
{
  DSTART(
    "set<unsigned int> WIR_InterferenceGraph::getColors(const WIR_BaseRegister&) const" );

  set<unsigned int> res;

  WIR_BaseRegister &reg = getUnaliasedReg( r );

  // Determine the corresponding boost graph node.
  IGraphVertex v = mNodeByRegister.at( reg.getID() );

  if ( reg.isHierarchical() &&
       ( reg == mIGraph[ v ].mRegister.get().get() ) )
    // The colors of a register hierarchy's root are requested. We thus return
    // all color entries in the map mColors.
    for ( auto color_entry : mIGraph[ v ].mColors )
      res.insert( color_entry.second );
  else {
    // The colors of a non-hierarchical register or of a leaf of a register
    // hierarchy are requested. We simply return it.
    auto &colorMap = mIGraph[ v ].mColors;
    auto it = colorMap.find( reg.getID() );

    if ( it != colorMap.end() )
      res.insert( it->second );
  }

  return( res );
};


/*
  getColorName returns the name of the physical register with which the node of
  the specified WIR register is colored.

  This method only returns a valid name if the specified WIR register is either
  a simple, non-hierarchical register, or if it is a leaf of a register
  hierarchy. For a non-leaf of a register hierarchy, it does not make sense to
  return one single color name.
*/
string WIR_InterferenceGraph::getColorName( const WIR_BaseRegister &r ) const
{
  DSTART(
    "string WIR_InterferenceGraph::getColorName(const WIR_BaseRegister&) const" );

  string res;
  set<unsigned int> colors = getColors( r );
  bool isHierarchical = r.isHierarchical();

  if ( ( colors.size() == 1 ) &&
       ( !isHierarchical ||
         ( r.isChild() && !r.hasChilds() ) ) ) {
    unsigned int c1 = *(colors.begin());

    for ( const WIR_PhysicalRegister &preg : mPhregs ) {
      set<unsigned int> phRegColors = getColors( preg );

      if ( *(phRegColors.begin()) == c1 ) {
        res = preg.getName();
        break;
      }
    }
  }

  return( res );
};


/*
  isColored returns whether the node of the specified WIR register is colored.
*/
bool WIR_InterferenceGraph::isColored( const WIR_BaseRegister &r ) const
{
  DSTART(
    "bool WIR_InterferenceGraph::isColored(const WIR_BaseRegister&) const" );

  return( getColors( getUnaliasedReg( r ) ).size() > 0 );
};


/*
  containsUncoloredNodes returns whether the interference graph contains any
  uncolored node.
*/
bool WIR_InterferenceGraph::containsUncoloredNodes( void ) const
{
  DSTART( "bool WIR_InterferenceGraph::containsUncoloredNodes() const" );

  // Iterate over all graph nodes not pushed onto the stack and check whether
  // they are colored or not.
  BGL_FORALL_VERTICES( v, mIGraph, IGraph )
    if ( !mIGraph[ v ].mIsPushed &&
         ( getColors( mIGraph[ v ].mRegister.get().get() ).size() == 0 ) )
      return( true );

  return( false );
};


/*
  getPossibleColors returns a set of colors that can be used to color the
  specified node.
*/
set<unsigned int> WIR_InterferenceGraph::getPossibleColors( const WIR_BaseRegister &r ) const
{
  DSTART(
    "set<unsigned int> WIR_InterferenceGraph::getPossibleColors(const WIR_BaseRegister&) const" );

  set<unsigned int> possibleColors;

  // Determine the corresponding boost graph node.
  IGraphVertex v = mNodeByRegister.at( r.getID() );

  // If the currently queried node is already colored, we return the empty set.
  if ( isColored( r ) )
    return( possibleColors );

  // Initially, all available colors are possible for the current node.
  for ( unsigned int c = 1; c <= mAvailableColors; ++c )
    possibleColors.insert( c );

  // Iterate over all nodes w adjacent to v and erase their colors.
  BGL_FORALL_ADJ( v, w, mIGraph, IGraph )
    if ( !mIGraph[ w ].mIsPushed ) {
      auto neighborColors = getColors( mIGraph[ w ].mRegister.get().get() );

      for ( auto c : neighborColors )
        possibleColors.erase( c );
    }

  return( possibleColors );
};


/*
  coalesceNodes coalesces the two interference graph nodes specified by the
  given WIR registers to a single node.

  coalesceNodes is only applicable to pairs (r1, r2) of registers for which
  holds:

  - r1 and r2 are both neither pushed on the stack, nor physical registers.
  - r1 and r2 do not interfere, i.e., there is no edge {r1, r2} in the
    interference graph, and r1 and r2 are not represented by the same
    interference graph node.

  If these preconditions hold, coalesceNodes merges both registers to a single
  node and connects the edges of the two original nodes to the new coalesced
  node.
*/
WIR_BaseRegister &WIR_InterferenceGraph::coalesceNodes( const WIR_BaseRegister &r1,
                                                        const WIR_BaseRegister &r2 )
{
  DSTART(
    "WIR_BaseRegister& WIR_InterferenceGraph::coalesceNodes(const WIR_BaseRegister&, const WIR_BaseRegister&)" );

  // Determine the corresponding boost graph nodes.
  IGraphVertex v1 = mNodeByRegister.at( r1.getID() );
  IGraphVertex v2 = mNodeByRegister.at( r2.getID() );

  // Check preconditions for coalescing.
  #ifdef FAILSAFEMODE
  ufAssert( !mIGraph[ v1 ].mIsPushed && !mIGraph[ v2 ].mIsPushed );
  ufAssert(
    mIGraph[ v1 ].mRegister.get().get().isVirtual() ||
    mIGraph[ v2 ].mRegister.get().get().isVirtual() );
  ufAssert( !interfere( r1, r2 ) );
  ufAssert( v1 != v2 );
  #endif

  // Now, actually determine which of the two registers r1 and r2 will remain
  // and which one will vanish.
  WIR_BaseRegister &remainingReg = getRemainingReg( r1, r2 );
  WIR_BaseRegister &vanishingReg = getVanishingReg( r1, r2 );

  IGraphVertex remainingNode = ( remainingReg == r1 ) ? v1 : v2;
  IGraphVertex vanishingNode = ( vanishingReg == r2 ) ? v2 : v1;

  // Determine, whether a PHREG node shall be coalesced with a VREG node, for
  // later consideration.
  bool physicalCoalescing =
    !mIGraph[ vanishingNode ].mRegister.get().get().isVirtual();

  // Iterate over all edges e of vanishingNode and redirect them to
  // remainingNode if required.
  // During this process, we have to pay attention that the interference graph
  // can be a multi-graph with several edges between a single pair of nodes (for
  // hierarchical registers). So, if we want to coalesce remainingNode and
  // vanishingNode, we check all edges incident to vanishingNode. If there are,
  // e.g., 2 edges {vanishingNode,x} and only one edge {remainingNode,x}, we
  // must make sure to redirect one more edge {vanishingNode,x} to
  // {remainingNode,x}. It is thus insufficient to simply check whether an edge
  // {remainingNode,x} exists at all and to only redirect {vanishingNode,x} to
  // remainingNode if no such edge exists. Instead, we first of all count how
  // many edges {remainingNode,x} exist. If there are more edges
  // {vanishingNode,x} than this counter, we only add the difference of missing
  // edges to remainingNode. Thus, we perform some kind of maximum computation
  // over edges incident to remainingNode and vanishingNode, resp.
  list<pair<IGraphVertex, bool>> newEdges;
  map<IGraphVertex, unsigned int> howManyEdgesFromRemainingNodeTo;
  set<IGraphEdge> removedEdges;

  BGL_FORALL_OUTEDGES( remainingNode, e, mIGraph, IGraph ) {
    IGraphVertex x =
      ( source( e, mIGraph ) == remainingNode ) ?
        target( e, mIGraph ) : source( e, mIGraph );

    // Increment counter for the current edge {remainingNode,x}.
    howManyEdgesFromRemainingNodeTo[ x ] =
      howManyEdgesFromRemainingNodeTo[ x ] + 1;
  }

  BGL_FORALL_OUTEDGES( vanishingNode, e, mIGraph, IGraph ) {
    IGraphVertex x =
      ( source( e, mIGraph ) == vanishingNode ) ?
        target( e, mIGraph ) : source( e, mIGraph );

    DOUT(
      "Checking edge {" << buildNodeName( vanishingNode ) << ", " <<
      buildNodeName( x ) << "}." << endl );

    // Check the existence of an edge e={remainingNode,x}.
    if ( howManyEdgesFromRemainingNodeTo[ x ] == 0 ) {
      // An edge {vanishingNode,x} exists, but {remainingNode,x} not. Thus, we
      // add a new edge {remainingNode,x}.
      DOUT(
        "Preparing to add new edge {" << buildNodeName( remainingNode ) <<
        ", " << buildNodeName( x ) << "}." << endl );
      newEdges.push_back( make_pair( x, mIGraph[ e ].mIsPushed ) );
    } else {
      // There exist edges {vanishingNode,x} and {remainingNode,x}. Thus, we do
      // not add a new edge {remainingNode,x} because they fall together after
      // coalescing. We simply skip the current edge e and decrement the
      // corresponding counter for remainingNode.
      DOUT(
        "Skipping edge {" << buildNodeName( vanishingNode ) << ", " <<
        buildNodeName( x ) << "}." << endl );
      howManyEdgesFromRemainingNodeTo[ x ] =
        howManyEdgesFromRemainingNodeTo[ x ] - 1;
    }

    if ( physicalCoalescing )
      removedEdges.insert( e );
  }

  // Add all the identified new edges.
  for ( auto newEdge_entry : newEdges ) {
    DOUT(
      "Adding new edge {" << buildNodeName( remainingNode ) << ", " <<
      buildNodeName( newEdge_entry.first ) << "}." << endl );

    IGraphEdge newEdge =
      add_edge( remainingNode, newEdge_entry.first, mIGraph ).first;
    mIGraph[ newEdge ].mIsPushed = newEdge_entry.second;

    if ( !newEdge_entry.second ) {
      mIGraph[ remainingNode ].mDegree++;
      mIGraph[ newEdge_entry.first ].mDegree++;
    }
  }

  // Remove all the checked edges if a physical register is coalesced.
  for ( auto e : removedEdges ) {
    IGraphVertex s = source( e, mIGraph );
    IGraphVertex t = target( e, mIGraph );
    bool isPushed = mIGraph[ e ].mIsPushed;

    DOUT(
      "Removing edge {" << mIGraph[ s ].mRegister.get().get().getName() <<
      "," << mIGraph[ t ].mRegister.get().get().getName() << "}." << endl );
    remove_edge( e, mIGraph );

    if ( !isPushed ) {
      mIGraph[ s ].mDegree--;
      mIGraph[ t ].mDegree--;
    }
  }
  removedEdges.clear();

  // Mark remainingNode as coalesced node.
  if ( !physicalCoalescing ) {
    DOUT(
      "Marking " << buildNodeName( remainingNode ) << " as coalesced node." <<
      endl );
    mIGraph[ remainingNode ].mIsCoalescedNode = true;
  }

  // Update alias and color information.
  if ( mIGraph[ remainingNode ].mLeafRegisters.size() ==
       mIGraph[ vanishingNode ].mLeafRegisters.size() ) {
    // Both remainingNode and vanishingNode have the same number of leaf
    // registers. So, we have to merge the aliases of the roots of remainingNode
    // and vanishingNode, and the aliases and colors of all leafs of
    // remainingNode and vanishingNode.

    // Here, we merge the aliases of the roots of remainingNode and
    // vanishingNode.
    if ( !physicalCoalescing )
      mergeAliases(
        mIGraph[ remainingNode ].mRegister.get().get(),
        mIGraph[ vanishingNode ].mRegister.get().get() );

    // Here, we merge the aliases and colors of all leafs of remainingNode and
    // vanishingNode.
    auto it1 = mIGraph[ remainingNode ].mLeafRegisters.begin();
    auto it2 = mIGraph[ vanishingNode ].mLeafRegisters.begin();

    for ( ; it1 != mIGraph[ remainingNode ].mLeafRegisters.end();
          ++it1, ++it2 ) {
      if ( !physicalCoalescing )
        mergeAliases( *it1, *it2 );
      mergeColors( *it1, *it2, remainingNode, vanishingNode );
    }
  } else {
    // remainingNode and vanishingNode have a different number of leafs. So, we
    // have to merge the aliases and colors of the leafs of the involved
    // sub-registers of remainingNode and vanishingNode.

    // Find the common roots of the register hierarchies of remainingReg and
    // vanishingReg.
    WIR_BaseRegister &root1 = remainingReg;
    WIR_BaseRegister &root2 = vanishingReg;

    while ( root1.isChild() && root2.isChild() ) {
      root1 = root1.getParent();
      root2 = root2.getParent();
    }

    auto leafRegsr1 = root1.getLeafs();
    auto leafRegsr2 = root2.getLeafs();

    // Both register hierarchies of remainingReg and vanishingReg that are
    // connected by a move operation must have the same number of leafs, i.e.,
    // their register hierarchies should match.
    #ifdef FAILSAFEMODE
    ufAssert( leafRegsr1.size() == leafRegsr2.size() );
    #endif

    auto it1 = leafRegsr1.begin();
    auto it2 = leafRegsr2.begin();

    for ( ; it1 != leafRegsr1.end(); ++it1, ++it2 ) {
      if ( !physicalCoalescing )
        mergeAliases( *it1, *it2 );
      mergeColors( *it1, *it2, remainingNode, vanishingNode );
    }
  }

  // Take maximal loop nesting depth from remainingNode or vanishingNode and
  // assign it to remainingNode.
  mIGraph[ remainingNode ].mLoopNestingDepth =
    ( mIGraph[ remainingNode ].mLoopNestingDepth >
      mIGraph[ vanishingNode ].mLoopNestingDepth ?
        mIGraph[ remainingNode ].mLoopNestingDepth :
        mIGraph[ vanishingNode ].mLoopNestingDepth );

  // Remove vanishingNode.
  if ( !physicalCoalescing ) {
    DOUT(
      "Deleting node " << buildNodeName( vanishingNode ) << " and its edges." <<
      endl );

    BGL_FORALL_OUTEDGES( vanishingNode, e, mIGraph, IGraph )
      removedEdges.insert( e );

    for ( auto e : removedEdges ) {
      IGraphVertex s = source( e, mIGraph );
      IGraphVertex t = target( e, mIGraph );
      bool isPushed = mIGraph[ e ].mIsPushed;

      DOUT(
        "Removing edge {" << mIGraph[ s ].mRegister.get().get().getName() <<
        ", " << mIGraph[ t ].mRegister.get().get().getName() << "}." << endl );
      remove_edge( e, mIGraph );

      if ( !isPushed ) {
        mIGraph[ s ].mDegree--;
        mIGraph[ t ].mDegree--;
      }
    }
    removedEdges.clear();

    clear_vertex( vanishingNode, mIGraph );
    remove_vertex( vanishingNode, mIGraph );

    // Completely rebuild internal node map as remove_vertex potentially
    // modifies all vertex IDs.
    mNodeByRegister.clear();

    BGL_FORALL_VERTICES( y, mIGraph, IGraph ) {
      mNodeByRegister[ mIGraph[ y ].mRegister.get().get().getID() ] = y;
      for ( WIR_BaseRegister &alias :
              mAliases[ mIGraph[ y ].mRegister.get().get().getID() ] )
        mNodeByRegister[ alias.getID() ] = y;

      for ( WIR_BaseRegister &l : mIGraph[ y ].mLeafRegisters ) {
        mNodeByRegister[ l.getID() ] = y;
        for ( WIR_BaseRegister &lalias : mAliases[ l.getID() ] )
          mNodeByRegister[ lalias.getID() ] = y;
      }
    }
  }

  return( remainingReg );
};


/*
  getCoalescedAliases returns the set of coalescing aliases of the specified WIR
  register, if any.

  If the specified register is not coalesced with any other register, an empty
  set is returned.
*/
WIR_RegisterSet WIR_InterferenceGraph::getCoalescedAliases( const WIR_BaseRegister &r ) const
{
  DSTART(
    "WIR_RegisterSet WIR_InterferenceGraph::getCoalescedAliases(const WIR_BaseRegister&) const");

  auto it = mAliases.find( r.getID() );

  if ( it != mAliases.end() )
    return( it->second );

  return( WIR_RegisterSet {} );
};


/*
  For some arbitrary leaf or root register in a register hierarchy,
  getUnaliasedReg returns this register's un-aliased partner as stored natively
  in some interference graph node.
*/
WIR_BaseRegister &WIR_InterferenceGraph::getUnaliasedReg( const WIR_BaseRegister &r ) const
{
  DSTART(
    "WIR_BaseRegister& WIR_InterferenceGraph::getUnaliasedReg(const WIR_BaseRegister&) const" );

  // Determine the corresponding boost graph node.
  IGraphVertex v = mNodeByRegister.at( r.getID() );
  WIR_BaseRegister &reg = const_cast<WIR_BaseRegister &>( r );

  // Check whether r corresponds to a leaf of v.
  for ( WIR_BaseRegister &leaf : mIGraph[ v ].mLeafRegisters ) {
    auto it = mAliases.find( leaf.getID() );

    if ( ( r == leaf ) ||
         ( ( it != mAliases.end() ) && ( it->second.count( reg ) ) ) )
      return( leaf );
  }

  // r thus has to correspond to a hierarchy's root.
  WIR_BaseRegister &root = mIGraph[ v ].mRegister.get().get();

  #ifdef FAILSAFEMODE
  auto it = mAliases.find( root.getID() );
  ufAssert(
    ( r == root ) ||
    ( ( it != mAliases.end() ) && ( it->second.count( reg ) ) ) );
  #endif

  return( root );
};


/*
  getRemainingReg returns that one of the two given registers that will remain
  in the interference graph when coalescing the two graph nodes.
*/
WIR_BaseRegister &WIR_InterferenceGraph::getRemainingReg( const WIR_BaseRegister &r1,
                                                          const WIR_BaseRegister &r2 ) const
{
  DSTART(
    "WIR_BaseRegister& WIR_InterferenceGraph::getRemainingReg(const WIR_BaseRegister&, const WIR_BaseRegister&) const" );

  // Determine the corresponding boost graph nodes.
  IGraphVertex v1 = mNodeByRegister.at( r1.getID() );
  IGraphVertex v2 = mNodeByRegister.at( r2.getID() );

  // Check preconditions for coalescing.
  #ifdef FAILSAFEMODE
  ufAssert( !mIGraph[ v1 ].mIsPushed && !mIGraph[ v2 ].mIsPushed );
  ufAssert(
    mIGraph[ v1 ].mRegister.get().get().isVirtual() ||
    mIGraph[ v2 ].mRegister.get().get().isVirtual() );
  ufAssert( !interfere( r1, r2 ) );
  ufAssert( v1 != v2 );
  #endif

  // Now, actually determine which of the two registers r1 and r2 will remain
  // and which one will vanish.
  // If r1 represents a PHREG, or if its node v1 is "smaller", i.e., contains
  // less leaf registers, r2 will remain in the interference graph.
  if ( !mIGraph[ v1 ].mRegister.get().get().isVirtual() ||
       ( mIGraph[ v1 ].mLeafRegisters.size() <
         mIGraph[ v2 ].mLeafRegisters.size() ) )
    return( const_cast<WIR_BaseRegister &>( r2 ) );

  return( const_cast<WIR_BaseRegister &>( r1 ) );
};


/*
  getVanishingReg returns that one of the two given registers that will vanish
  when coalescing the two graph nodes.
*/
WIR_BaseRegister &WIR_InterferenceGraph::getVanishingReg( const WIR_BaseRegister &r1,
                                                          const WIR_BaseRegister &r2 ) const
{
  DSTART(
    "WIR_BaseRegister& WIR_InterferenceGraph::getVanishingReg(const WIR_BaseRegister&, const WIR_BaseRegister&) const"  );

  return(
    const_cast<WIR_BaseRegister &>(
      ( getRemainingReg( r1, r2 ) == r1 ) ? r2 : r1 ) );
};


/*
  setPotentialSpill marks or unmarks the node of the specified virtual register
  as potential spill.

  If this method is applied to a node not representing an uncolored virtual
  register, no changes are made. Marking a node as potential spill has the
  side-effect that it is no longer marked as actual spill.
*/
void WIR_InterferenceGraph::setPotentialSpill( const WIR_VirtualRegister &r,
                                               bool m )
{
  DSTART(
    "void WIR_InterferenceGraph::setPotentialSpill(const WIR_VirtualRegister&, bool)" );

  // Determine the corresponding boost graph node.
  IGraphVertex v = mNodeByRegister.at( r.getID() );

  if ( isColored( r ) ) {
    DOUT( "Skipping node '" << r.getName() << "'." << endl );
    return;
  }

  DOUT(
    ( m ? "M" : "Unm" ) << "arked register '" << r.getName() <<
    "' as potential spill." << endl );

  mIGraph[ v ].mIsPotentialSpill = m;

  if ( m )
    mIGraph[ v ].mIsActualSpill = false;
};


/*
  isPotentialSpill returns whether the node of the specified virtual register is
  marked as potential spill.
*/
bool WIR_InterferenceGraph::isPotentialSpill( const WIR_VirtualRegister &r ) const
{
  DSTART(
    "bool WIR_InterferenceGraph::isPotentialSpill(const WIR_VirtualRegister&) const" );

  // Determine the corresponding boost graph node.
  IGraphVertex v = mNodeByRegister.at( r.getID() );

  return( mIGraph[ v ].mIsPotentialSpill );
};


/*
  setActualSpill marks or unmarks the node of the specified virtual register as
  actual spill.

  If this method is applied to a node not representing an uncolored virtual
  register, no changes are made. Marking a node as actual spill has the
  side-effect that it is no longer marked as potential spill.
*/
void WIR_InterferenceGraph::setActualSpill( const WIR_VirtualRegister &r,
                                            bool m )
{
  DSTART(
    "void WIR_InterferenceGraph::setActualSpill(const WIR_VirtualRegister&, bool)" );

  // Determine the corresponding boost graph node.
  IGraphVertex v = mNodeByRegister.at( r.getID() );

  if ( isColored( r ) ) {
    DOUT( "Skipping node '" << r.getName() << "'." << endl );
    return;
  }

  DOUT(
    ( m ? "M" : "Unm" ) << "arked register '" << r.getName() <<
    "' as actual spill." << endl );

  mIGraph[ v ].mIsActualSpill = m;

  if ( m )
    mIGraph[ v ].mIsPotentialSpill = false;
};


/*
  isActualSpill returns whether the node of the specified virtual register is
  marked as actual spill.
*/
bool WIR_InterferenceGraph::isActualSpill( const WIR_VirtualRegister &r ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  // Determine the corresponding boost graph node.
  IGraphVertex v = mNodeByRegister.at( r.getID() );

  return( mIGraph[ v ].mIsActualSpill );
};


/*
  setSpillCosts sets the spill costs of the specified virtual register.
*/
void WIR_InterferenceGraph::setSpillCosts( const WIR_VirtualRegister &r,
                                           unsigned int c )
{
  DSTART(
    "void WIR_InterferenceGraph::setSpillCosts(const WIR_VirtualRegister&, unsigned int)" );

  // Determine the corresponding boost graph node.
  IGraphVertex v = mNodeByRegister.at( r.getID() );

  mIGraph[ v ].mSpillCosts = c;
};


/*
  getSpillCosts returns the spill costs of the specified virtual register.
*/
unsigned int WIR_InterferenceGraph::getSpillCosts( const WIR_VirtualRegister &r ) const
{
  DSTART(
    "unsigned int WIR_InterferenceGraph::getSpillCosts(const WIR_VirtualRegister&) const" );

  // Determine the corresponding boost graph node.
  IGraphVertex v = mNodeByRegister.at( r.getID() );

  return( mIGraph[ v ].mSpillCosts );
};


/*
  pushNode pushes the specified interference graph node to the internal stack
  and virtually removes all incident edges.

  This method only pushes nodes representing virtual registers onto the stack.
  If it is applied to a node that is already pushed onto the stack, no changes
  are made.
*/
void WIR_InterferenceGraph::pushNode( const WIR_VirtualRegister &r )
{
  DSTART( "void WIR_InterferenceGraph::pushNode(const WIR_VirtualRegister&)" );

  // Determine the corresponding boost graph node.
  IGraphVertex v = mNodeByRegister.at( r.getID() );

  if ( mIGraph[ v ].mIsPushed ) {
    DOUT( "Skipping node '" << r.getName() << "'." << endl );
    return;
  }

  // Push the current node v onto the stack and mark it as pushed.
  DOUT( "Pushing node '" << r.getName() << "'." << endl );
  mNodeStack.push_front( const_cast<WIR_VirtualRegister &>( r ) );
  mIGraph[ v ].mIsPushed = true;

  // Mark all edges e={v,w} as pushed if the adjacent node w is still part of
  // the interference graph.
  BGL_FORALL_OUTEDGES( v, e, mIGraph, IGraph ) {
    IGraphVertex w =
      ( source( e, mIGraph ) == v ) ?
        target( e, mIGraph ) : source( e, mIGraph );

    if ( !(mIGraph[ e ].mIsPushed ) ) {
      DOUT(
        "Pushing edge {" << r.getName() << ", " <<
        mIGraph[ w ].mRegister.get().get().getName() << "}." << endl );
      mIGraph[ e ].mIsPushed = true;
      mIGraph[ v ].mDegree--;
      mIGraph[ w ].mDegree--;
    }
  }
};


/*
  pushPriority Node pushes the specified interference graph node to the internal
  priority stack and virtually removes all incident edges.

  This method only pushes nodes representing virtual registers onto the priority
  stack. If it is applied to a physical register, or to a node that is already
  pushed onto the stack, no changes are made.
*/
void WIR_InterferenceGraph::pushPriorityNode( const WIR_VirtualRegister &r )
{
  DSTART(
    "void WIR_InterferenceGraph::pushPriorityNode(const WIR_VirtualRegister&)" );

  // Determine the corresponding boost graph node.
  IGraphVertex v = mNodeByRegister.at( r.getID() );

  if ( mIGraph[ v ].mIsPushed ) {
    DOUT( "Skipping node '" << r.getName() << "'." << endl );
    return;
  }

  // Push the current node v onto the stack and mark it as pushed.
  DOUT( "Pushing node '" << r.getName() << "'." << endl );
  mPriorityNodeStack.push_front( const_cast<WIR_VirtualRegister &>( r ) );
  mIGraph[ v ].mIsPushed = true;

  // Mark all edges e={v,w} as pushed if the adjacent node w is still part of
  // the interference graph.
  BGL_FORALL_OUTEDGES( v, e, mIGraph, IGraph ) {
    IGraphVertex w =
      ( source( e, mIGraph ) == v ) ?
        target( e, mIGraph ) : source( e, mIGraph );

    if ( !(mIGraph[ e ].mIsPushed ) ) {
      DOUT(
        "Pushing edge {" << r.getName() << "," <<
        mIGraph[ w ].mRegister.get().get().getName() << "}." << endl );
      mIGraph[ e ].mIsPushed = true;
      mIGraph[ v ].mDegree--;
      mIGraph[ w ].mDegree--;
    }
  }
};


/*
  popNode pops the top node from the internal stack(s) and re-inserts its node
  and incident edges back into the interference graph.

  First, all high-priority nodes from mPriorityNodeStack are popped, if any,
  followed by the nodes from mNodeStack.
*/
WIR_VirtualRegister &WIR_InterferenceGraph::popNode( void )
{
  DSTART( "WIR_VirtualRegister& WIR_InterferenceGraph::popNode()" );

  // Pop the current node v from one of the stacks and unmark it as pushed.
  WIR_VirtualRegister &r =
    ( !mPriorityNodeStack.empty() ?
        mPriorityNodeStack.front().get() : mNodeStack.front().get() );

  if ( !mPriorityNodeStack.empty() )
    mPriorityNodeStack.pop_front();
  else
    mNodeStack.pop_front();
  DOUT( "Popping node '" << r.getName() << "'." << endl );

  // Determine the corresponding boost graph node.
  IGraphVertex v = mNodeByRegister.at( r.getID() );
  mIGraph[ v ].mIsPushed = false;

  // Unmark all edges e={v,w} as pushed if the adjacent node w is still part of
  // the interference graph.
  BGL_FORALL_OUTEDGES( v, e, mIGraph, IGraph ) {
    IGraphVertex w =
      ( source( e, mIGraph ) == v ) ?
        target( e, mIGraph ) : source( e, mIGraph );

    if ( !(mIGraph[ w ].mIsPushed ) ) {
      DOUT(
        "Popping edge {" << r.getName() << "," <<
        mIGraph[ w ].mRegister.get().get().getName() << "}." << endl );
      mIGraph[ e ].mIsPushed = false;
      mIGraph[ v ].mDegree++;
      mIGraph[ w ].mDegree++;
    }
  }

  return( r );
};


/*
  isStackEmpty returns whether the interference graph's internal stacks of
  pushed nodes are empty or not.
*/
bool WIR_InterferenceGraph::isStackEmpty( void ) const
{
  DSTART( "bool WIR_InterferenceGraph::isStackEmpty() const" );

  return( mNodeStack.empty() && mPriorityNodeStack.empty() );
};


//
// Private class methods
//

/*
  addNodesForPHREGs adds interference graph nodes for all specified physical
  registers.

  Interference graph nodes will be created for all leaf registers, but not for
  parents in register hierarchies. For each added node, an individual color is
  already assigned.
*/
void WIR_InterferenceGraph::addNodesForPHREGs( void )
{
  DSTART( "void WIR_InterferenceGraph::addNodesForPHREGs()" );

  set<WIR_id_t> insertedPhRegs;

  for ( const WIR_PhysicalRegister &preg : mPhregs )
    for ( WIR_PhysicalRegister &leaf : preg.getLeafs() )
      if ( insertedPhRegs.count( leaf.getID() ) == 0 ) {
        ++mAvailableColors;
        mColorOfPhreg[ leaf.getID() ] = mAvailableColors;
        mPhregOfColor.insert( make_pair( mAvailableColors, std::ref( leaf ) ) );

        addNode( leaf );
        setColor( leaf, mAvailableColors );
        insertedPhRegs.insert( leaf.getID() );
      }
};


/*
  addNodesForVREGs adds interference graph nodes for all virtual registers in
  the specified WIR function.
*/
void WIR_InterferenceGraph::addNodesForVREGs( const WIR_VirtualRegisterSet &vregs )
{
  DSTART(
    "void WIR_InterferenceGraph::addNodesForVREGs(const WIR_VirtualRegisterSet&)" );

  for ( WIR_VirtualRegister &vreg : mFunction->getVirtualRegisters() )
    if ( !vreg.isChild() &&
         ( vregs.empty() || vregs.count( vreg ) ) )
      // If the current register is a simple non-hierarchical register or is the
      // root of a register hierarchy, add an interference graph node.
      addNode( vreg );
};


/*
  addNode adds a node for the specified register to the interference graph.

  If a node for the given register already exists in the interference graph, no
  duplicate node is added.
*/
void WIR_InterferenceGraph::addNode( const WIR_BaseRegister &r )
{
  DSTART( "void WIR_InterferenceGraph::addNode(const WIR_BaseRegister&)" );

  if ( !containsNode( r ) ) {
    DOUT(
      "Adding node for " <<
      ( r.isVirtual() ? "virtual" : "physical") << " register '" <<
      r.getName() << "'." << endl );

    IGraphVertex v = add_vertex( mIGraph );
    mNodeByRegister[ r.getID() ] = v;

    mIGraph[ v ].mRegister = std::ref( const_cast<WIR_BaseRegister &>( r ) );

    // Add all leafs of a virtual register hierarchy.
    if ( r.isVirtual() && r.hasChilds() )
      for ( WIR_BaseRegister &leaf : r.getLeafs() ) {
        mNodeByRegister[ leaf.getID() ] = v;
        mIGraph[ v ].mLeafRegisters.push_back( leaf );
      }
  }
};


/*
  In the course of coalescing, mergeAliases updates the internal alias data
  structure and merges the aliases related to the specified two registers to be
  coalesced.
*/
void WIR_InterferenceGraph::mergeAliases( const WIR_BaseRegister &r1,
                                          const WIR_BaseRegister &r2 )
{
  DSTART(
    "void WIR_InterferenceGraph::mergeAliases(const WIR_BaseRegister&, const WIR_BaseRegister&)" );

  auto &reg1 = const_cast<WIR_BaseRegister &>( r1 );
  auto &reg2 = const_cast<WIR_BaseRegister &>( r2 );

  // allAliases := {r1,r2} U mAliases[ r1 ] U mAliases[ r2 ]
  WIR_RegisterSet allAliases;

  allAliases.insert( reg1 );
  allAliases.insert( reg2 );

  allAliases.insert(
    mAliases[ r1.getID() ].begin(), mAliases[ r1.getID() ].end() );
  allAliases.insert(
    mAliases[ r2.getID() ].begin(), mAliases[ r2.getID() ].end() );

  for ( WIR_BaseRegister &alias : allAliases ) {
    mAliases[ alias.getID() ] = allAliases;
    mAliases[ alias.getID() ].erase( alias );
  }
};


/*
  In the course of coalescing, mergeColors merges the colors related to the
  specified two registers to be coalesced.
*/
void WIR_InterferenceGraph::mergeColors( const WIR_BaseRegister &r1,
                                         const WIR_BaseRegister &r2,
                                         const IGraphVertex &v1,
                                         const IGraphVertex &v2 )
{
  DSTART(
    "void WIR_InterferenceGraph::mergeColors(const WIR_BaseRegister&, const WIR_BaseRegister&, const IGraphVertex&, const IGraphVertex&)" );

  // Move color information from the vanishing node v2 to v1.
  auto r2colorIt = mIGraph[ v2 ].mColors.find( r2.getID() );
  if ( r2colorIt != mIGraph[ v2 ].mColors.end() ) {
    // The current leaf of v2 is colored.
    unsigned int colorOfR2 = r2colorIt->second;

    #ifdef FAILSAFEMODE
    unsigned int colorOfR1 = 0;

    auto r1colorIt = mIGraph[ v1 ].mColors.find( r1.getID() );
    if ( r1colorIt != mIGraph[ v1 ].mColors.end() )
      colorOfR1 = r1colorIt->second;

    ufAssert( ( colorOfR1 == colorOfR2 ) || ( colorOfR1 == 0 ) );
    #endif

    mIGraph[ v1 ].mColors[ r1.getID() ] = colorOfR2;
  }
};


/*
  visualizeNodes dumps the interference graph nodes into a given DOT file.
*/
void WIR_InterferenceGraph::visualizeNodes( std::fstream &dotFile,
                                            std::map<WIR_id_t, unsigned int> &nodeToDotInt,
                                            bool allNodes ) const
{
  DSTART(
    "void WIR_InterferenceGraph::visualizeNodes(fstream&, map<WIR_id_t, unsigned int>&, bool) const" );

  unsigned int nodeCounter = 0;

  // Produce a legend in the visualization.
  dotFile << "subgraph clusterLegend {" << endl;

  dotFile << nodeCounter++ << "[label=\"PHREG\",shape=box]" << endl << ";"
          << endl;

  if ( allNodes )
    dotFile << nodeCounter++
            << "[label=< <table border=\"0\"><tr><td><font color='gray'>Node "
            << "pushed<br />on Stack</font></td></tr></table> >,color=gray,"
            << "style=dotted]" << endl << ";" << endl;

  dotFile << nodeCounter++ << "[label=\"Actual\\nSpill\",shape=tripleoctagon]"
          << endl << ";" << endl;

  dotFile << nodeCounter++ << "[label=\"Potential\\nSpill\",shape=hexagon]"
          << endl << ";" << endl;

  dotFile << nodeCounter++ << "[label=\"Unspilled\\nVREG\"]" << endl << ";"
          << endl;

  dotFile << nodeCounter++ << "[label=\""
          << mFunction->getCompilationUnit().getName() << "\\n"
          << mFunction->getName() << "\",color=white]" << endl << ";" << endl;

  dotFile << "}" << endl;

  // Visualize all graph nodes.
  BGL_FORALL_VERTICES( v, mIGraph, IGraph ) {
    if ( !mIGraph[ v ].mIsPushed || allNodes ) {
      dotFile << nodeCounter << "[label=";

      // Build the node's label depending on whether the node is an extended
      // register, a coalesced node and on its color and loop nesting depth.
      if ( mIGraph[ v ].mRegister.get().get().hasChilds() ) {

        auto &leafRegs = mIGraph[ v ].mLeafRegisters;

        // We visualize a hierarchical register here. First, display the
        // parent's name.
        dotFile << "< <table border=\"0\"><tr><td colspan=\""
                << leafRegs.size() << "\">"
                << ( mIGraph[ v ].mIsPushed ? "<font color='gray'>" : "" )
                << buildNodeName( v ) << buildNodeDepth( v )
                << ( mIGraph[ v ].mIsPushed ? "</font>" : "" )
                << "</td></tr><tr>";

        // Iterate over all childs and display them.
        auto &colors = mIGraph[ v ].mColors;
        for ( WIR_BaseRegister &leaf : leafRegs ) {
          dotFile << "<td";

          // Set the child's background color, if it is already colored.
          auto it = colors.find( leaf.getID() );
          unsigned int c = ( it == colors.end() ) ? 0 : it->second;

          if ( c > 0 )
            dotFile << " bgcolor='" << getFillColor( c ) << "'";

          dotFile << ">";
          dotFile << ( mIGraph[ v ].mIsPushed ? "<font color='gray'>" : "" )
                  << buildRegName( leaf ) << buildNodeColor( c, leaf )
                  << ( mIGraph[ v ].mIsPushed ? "</font>" : "" )
                  << "</td>";
        }

        dotFile << "</tr></table> >";

      } else {

        // We visualize a simple, non-hierarchical register here.
        auto &colors = mIGraph[ v ].mColors;
        auto it = colors.find( mIGraph[ v ].mRegister.get().get().getID() );
        unsigned int c = ( it == colors.end() ) ? 0 : it->second;

        dotFile << "< <table border=\"0\"><tr><td>"
                << ( mIGraph[ v ].mIsPushed ? "<font color='gray'>" : "" )
                << buildNodeName( v )
                << buildNodeColor( c, mIGraph[ v ].mRegister.get().get() )
                << buildNodeDepth( v )
                << ( mIGraph[ v ].mIsPushed ? "</font>" : "" )
                << "</td></tr></table> >";

        // Set the node's background color, if the node is already colored.
        if ( c > 0 )
          dotFile << ",style=filled,fillcolor=" << getFillColor( c );

      }

      // Pushed nodes are displayed differently.
      if ( mIGraph[ v ].mIsPushed )
        dotFile << ",color=gray,style=dotted";

      // Set the node's shape according to its marks as potential or actual
      // spill.
      if ( !mIGraph[ v ].mRegister.get().get().isVirtual() )
        // Physical registers are depicted as boxes.
        dotFile << ",shape=box";
      else

      if ( mIGraph[ v ].mIsActualSpill )
        // Actual spills are depicted as triple-octagons.
        dotFile << ",shape=tripleoctagon";
      else

      if ( mIGraph[ v ].mIsPotentialSpill )
        // Potential spills are depicted as hexagons.
        dotFile << ",shape=hexagon";

      dotFile << "]" << endl << ";" << endl;
      nodeToDotInt[ mIGraph[ v ].mRegister.get().get().getID() ] =
        nodeCounter++;
    }
  }
};


/*
  visualizeEdges dumps the interference graph edges into a given DOT file.
*/
void WIR_InterferenceGraph::visualizeEdges( std::fstream &dotFile,
                                            const std::map<WIR_id_t, unsigned int> &nodeToDotInt,
                                            bool allNodes ) const
{
  DSTART(
    "void WIR_InterferenceGraph::visualizeEdges(fstream&, const map<WIR_id_t, unsigned int>&, bool) const" );

  BGL_FORALL_EDGES( e, mIGraph, IGraph ) {
    if ( !mIGraph[ e ].mIsPushed || allNodes ) {
      IGraphVertex src = source( e, mIGraph ), tgt = target( e, mIGraph );

      auto srcIt =
        nodeToDotInt.find( mIGraph[ src ].mRegister.get().get().getID() );
      auto tgtIt =
        nodeToDotInt.find( mIGraph[ tgt ].mRegister.get().get().getID() );

      dotFile << srcIt->second << "--" << tgtIt->second
              << "[arrowhead=none,arrowtail=none";

      // Pushed edges are displayed differently.
      if ( mIGraph[ e ].mIsPushed )
        dotFile << ",color=gray,style=dotted";

      dotFile << "]" << endl << ";" << endl;
    }
  }
};


/*
  buildNodeName returns a string denoting the specified node's name.
*/
string WIR_InterferenceGraph::buildNodeName( const IGraphVertex &v ) const
{
  DSTART(
    "string WIR_InterferenceGraph::buildNodeName(const IGraphVertex&) const" );

  return( buildRegName( mIGraph[ v ].mRegister.get().get() ) );
};


/*
  buildRegName returns a string denoting the specified register's name.
*/
string WIR_InterferenceGraph::buildRegName( const WIR_BaseRegister &r ) const
{
  DSTART(
    "string WIR_InterferenceGraph::buildRegName(const WIR_BaseRegister&) const" );

  ostringstream sstr;

  sstr << r.getName();

  // Add coalescing aliases to node name.
  auto it = mAliases.find( r.getID() );

  if ( it != mAliases.end() )
    for ( WIR_BaseRegister &a : it->second )
      sstr << "/" << a.getName();

  return( sstr.str() );
};


/*
  buildNodeColor returns a string denoting the DOT name of the specified color
  number.
*/
string WIR_InterferenceGraph::buildNodeColor( unsigned int c,
                                              const WIR_BaseRegister &r ) const
{
  DSTART(
    "string WIR_InterferenceGraph::buildNodeColor(unsigned int, const WIR_BaseRegister&) const" );

  ostringstream sstr;

  if ( c > 0 ) {
    sstr << "<br />" << "(col " << c;
    if ( r.isVirtual() )
      sstr << "/" << getColorName( r );
    sstr << ")";
  }

  return( sstr.str() );
};


/*
  buildNodeDepth returns a string denoting the specified node's loop nesting
  depth.
*/
string WIR_InterferenceGraph::buildNodeDepth( const IGraphVertex &v ) const
{
  DSTART(
    "string WIR_InterferenceGraph::buildNodeDepth(const IGraphVertex&) const" );

  ostringstream sstr;

  if ( mIGraph[ v ].mLoopNestingDepth > 0 )
    sstr << "<br />" << "(depth "
         << mIGraph[ v ].mLoopNestingDepth << ")";

  return( sstr.str() );
};


/*
  getFillColor returns a string denoting a graphviz color to be used for color
  value c.
*/
string WIR_InterferenceGraph::getFillColor( unsigned int c ) const
{
  DSTART( "string WIR_InterferenceGraph::getFillColor(unsigned int) const" );

  return graphvizColors[ c % 283 ];
};

}       // namespace WIR
