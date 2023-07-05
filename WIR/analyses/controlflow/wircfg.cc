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
  @file wircfg.cc
  @brief This file implements control flow graphs.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include standard headers
#include <sstream>
#include <stdlib.h>
#include <unistd.h>

// Include boost headers
#include <boost/current_function.hpp>
#include <boost/graph/iteration_macros.hpp>

// Include libuseful headers
#include <libuseful/debugmacros.h>
#include <libuseful/exceptions.h>
#include <libuseful/io.h>

// Include WIR headers
#include <wir/wir.h>


//
// Code section
//

namespace WIR {


using namespace boost;
using namespace std;


//
// Public class methods
//

/*
  Default constructor for a given WIR function.
*/
WIR_CFG::WIR_CFG( WIR_Function &f, bool r, bool verbosity, bool keepTmpFiles ) :
  mFunction { f },
  mOnlyReachableBlocks { r },
  mVerbosity { verbosity },
  mKeepTmpFiles { keepTmpFiles }
{
  DSTART( "WIR_CFG::WIR_CFG(WIR_Function&, bool, bool, bool)" );

  addNodes();
  addEdges();

  classify();
};


/*
  Destructor.
*/
WIR_CFG::~WIR_CFG( void )
{
  DSTART( "WIR_CFG::~WIR_CFG()" );
};


/*
  getDFSOrder returns the list of all basic blocks in their depth-first
  traversal order.
*/
const std::list<std::reference_wrapper<WIR_BasicBlock>> &WIR_CFG::getDFSOrder( void ) const
{
  DSTART(
    "const list<reference_wrapper<WIR_BasicBlock> >& WIR_CFG::getDFSOrder() const" );

  return( mDFSOrder );
};


/*
  getReversePostOrder returns the list of all basic blocks in their reverse
  postorder.
*/
const std::list<std::reference_wrapper<WIR_BasicBlock>> &WIR_CFG::getReversePostOrder( void ) const
{
  DSTART(
    "const list<reference_wrapper<WIR_BasicBlock> >& WIR_CFG::getReversePostOrder() const" );

  return( mReversePostOrder );
};


/*
  getReverseTopologicalOrder returns the list of all basic blocks in their
  reverse-topological order.
*/
const std::list<std::reference_wrapper<WIR_BasicBlock>> &WIR_CFG::getReverseTopologicalOrder( void ) const
{
  DSTART(
    "const list<reference_wrapper<WIR_BasicBlock> >& WIR_CFG::getReverseTopologicalOrder() const" );

  return( mReverseTopologicalOrder );
};


/*
  getStartNodes returns all start nodes of the current CFG.
*/
const WIR_BasicBlockSet &WIR_CFG::getStartNodes( void ) const
{
  DSTART( "const WIR_BasicBlockSet& WIR_CFG::getStartNodes() const" );

  return( mStartNodes );
};


/*
  getBackEdges returns the set of back edges in the CFG.
*/
const WIR_BasicBlockPairSet &WIR_CFG::getBackEdges( void ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  return( mWIRBackEdges );
};


/*
  visualize dumps the current control flow graph into a DOT file and invokes
  xdot on it.
*/
void WIR_CFG::visualize( bool in ) const
{
  DSTART( "void WIR_CFG::visualize(bool) const" );

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
  map<CGraphVertex, unsigned int> nodeToDotInt;
  visualizeNodes( dotFile, nodeToDotInt, in );

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


//
// Protected class methods
//

/*
  rebuild rebuilds the internal CFG from scratch for function mFunction.
*/
void WIR_CFG::rebuild( void )
{
  DSTART( "void WIR_CFG::rebuild()" );

  // Clear all internal data structures.
  BGL_FORALL_VERTICES( v, mCGraph, CGraph )
    clear_vertex( v, mCGraph );

  mNodeByID.clear();
  mStartNodes.clear();
  mBackEdges.clear();
  mWIRBackEdges.clear();
  mDFSOrder.clear();
  mReversePostOrder.clear();
  mReverseTopologicalOrder.clear();

  // Rebuild CFG.
  addNodes();
  addEdges();

  classify();
};


//
// Private class methods
//

/*
  addNodes adds CFG nodes for all basic blocks in the specified WIR function.
*/
void WIR_CFG::addNodes( void )
{
  DSTART( "void WIR_CFG::addNodes()" );

  // Traverse the current WIR function.
  for ( WIR_BasicBlock &b : mFunction ) {
    CGraphVertex v = add_vertex( WIR_CFGNodeProperty( b ), mCGraph );
    mNodeByID[ b.getID() ] = v;
    DOUT( "Adding node for basic block '" << b.getName() << "'." << endl );
  }
};


/*
  addEdges adds edges between all control flow-dependent CFG nodes.
*/
void WIR_CFG::addEdges( void )
{
  DSTART( "void WIR_CFG::addEdges()" );

  // Traverse the current WIR function.
  for ( WIR_BasicBlock &b : mFunction ) {
    CGraphVertex v1 = mNodeByID[ b.getID() ];

    for ( WIR_BasicBlock &succ : b.getSuccessors() ) {
      CGraphVertex v2 = mNodeByID[ succ.getID() ];

      DOUT(
        "Adding edge '" << b.getName() << "' -> '" << succ.getName() << "'." <<
        endl );
      add_edge( v1, v2, {}, mCGraph );
    }
  }
};


/*
  classify traverses the CFG in depth-first order and determines the nodes' and
  edges' types.
*/
void WIR_CFG::classify( void )
{
  DSTART( "void WIR_CFG::classify()" );

  if ( mFunction.getBasicBlocks().empty() )
    return;

  // Do depth-first traversal of CFG starting at the current function's very
  // first basic block.
  map<CGraphEdge, WIR_CFGEdgeType> edgeType;
  CFGVisitor vis
    { mStartNodes, mDFSOrder, mReversePostOrder, mReverseTopologicalOrder,
      edgeType, mBackEdges };
  vector<boost::default_color_type> color_map( boost::num_vertices( mCGraph ) );

  if ( mOnlyReachableBlocks )
    boost::depth_first_visit(
      mCGraph,
      mNodeByID[ mFunction.begin()->get().getID() ],
      vis,
      boost::make_iterator_property_map(
        color_map.begin(), boost::get( boost::vertex_index, mCGraph ),
        color_map[ 0 ] ) );
  else
    boost::depth_first_search(
      mCGraph,
      vis,
      boost::make_iterator_property_map(
        color_map.begin(), boost::get( boost::vertex_index, mCGraph ),
        color_map[ 0 ] ),
      mNodeByID[ mFunction.begin()->get().getID() ] );

  // Propagate results of DFS traversal back into the CFG node and edge
  // properties.
  for ( WIR_BasicBlock &b : mStartNodes )
    mCGraph[ mNodeByID[ b.getID() ] ].setStart();

  for ( auto p : edgeType )
    mCGraph[ p.first ].mType = p.second;

  for ( auto e : mBackEdges ) {
    mCGraph[ e ].mIsBackEdge = true;

    CGraphVertex src = source( e, mCGraph );
    CGraphVertex tgt = target( e, mCGraph );

    mWIRBackEdges.insert(
      { mCGraph[ src ].getBasicBlock(), mCGraph[ tgt ].getBasicBlock() } );
  }
};


/*
  visualizeNodes dumps the control flow graph nodes into a given DOT file.
*/
void WIR_CFG::visualizeNodes( std::fstream &dotFile,
                              std::map<CGraphVertex, unsigned int> &nodeToDotInt,
                              bool in ) const
{
  DSTART(
    "void WIR_CFG::visualizeNodes(fstream&, map<unsigned int, unsigned int>&, bool) const" );

  unsigned int nodeCounter = 0;

  // Produce a legend in the visualization.
  dotFile << "subgraph clusterLegend {" << endl;

  dotFile << nodeCounter++ << "[label=\"Basic Block\",shape=box]" << endl << ";"
          << endl;

  dotFile << nodeCounter++ << "[label=\"Start Block\",shape=box,style=bold]"
          << endl << ";" << endl;

  dotFile << nodeCounter++ << "[shape=point]" << endl << ";" << endl;
  dotFile << nodeCounter++ << "[shape=point]" << endl << ";" << endl;
  dotFile << nodeCounter - 2 << "->" << nodeCounter - 1
          << "[label=\"Regular edge\",dir=forward,arrowhead=vee,arrowtail=none]" << endl << ";" << endl;

  dotFile << nodeCounter++ << "[shape=point]" << endl << ";" << endl;
  dotFile << nodeCounter++ << "[shape=point]" << endl << ";" << endl;
  dotFile << nodeCounter - 2 << "->" << nodeCounter - 1
          << "[label=\"True edge\",dir=forward,arrowhead=vee,arrowtail=none,"
          << "style=bold]" << endl << ";" << endl;

  dotFile << nodeCounter++ << "[shape=point]" << endl << ";" << endl;
  dotFile << nodeCounter++ << "[shape=point]" << endl << ";" << endl;
  dotFile << nodeCounter - 2 << "->" << nodeCounter - 1
          << "[label=\"Back edge\",dir=forward,arrowhead=vee,arrowtail=none,"
          << "style=dashed]" << endl << ";" << endl;

  dotFile << nodeCounter++ << "[shape=point]" << endl << ";" << endl;
  dotFile << nodeCounter++ << "[shape=point]" << endl << ";" << endl;
  dotFile << nodeCounter - 2 << "->" << nodeCounter - 1
          << "[label=\"Call/return edge\",dir=forward,arrowhead=vee,"
          << "arrowtail=none,color=gray,style=dotted]" << endl
          << ";" << endl;

  dotFile << nodeCounter++ << "[label=\"";
  if ( mFunction.isInserted() )
    dotFile << mFunction.getCompilationUnit().getName() << "\\n";
  dotFile << mFunction.getName() << "\",color=white]" << endl << ";" << endl;

  dotFile << "}" << endl;

  // Visualize all graph nodes.
  BGL_FORALL_VERTICES( v, mCGraph, CGraph ) {
    dotFile << nodeCounter << " [label=" << buildNodeName( v, in )
            << ",shape=box";

    if ( mCGraph[ v ].isStart() )
      dotFile << ",style=bold";

    dotFile << "]" << endl << ";" << endl;

    nodeToDotInt[ v ] = nodeCounter++;
  }
};


/*
  visualizeEdges dumps the control  flow graph edges into a given DOT file.
*/
void WIR_CFG::visualizeEdges( std::fstream &dotFile,
                              const std::map<CGraphVertex, unsigned int> &nodeToDotInt ) const
{
  DSTART(
    "void WIR_CFG::visualizeEdges(fstream&, const map<unsigned int, unsigned int>&) const" );

  BGL_FORALL_EDGES( e, mCGraph, CGraph ) {
    CGraphVertex src = source( e, mCGraph );
    CGraphVertex tgt = target( e, mCGraph );

    dotFile << nodeToDotInt.at( src ) << "->" << nodeToDotInt.at( tgt )
            << "[dir=forward,arrowhead=vee,arrowtail=none";

    if ( ( mCGraph[ e ].mType == WIR_CFGEdgeType::call ) ||
         ( mCGraph[ e ].mType == WIR_CFGEdgeType::ret ) )
      dotFile << ",color=gray,style=dotted";
    else

    if ( mCGraph[ e ].mType == WIR_CFGEdgeType::tru )
      dotFile << ",style=bold";

    if ( mCGraph[ e ].mIsBackEdge )
      dotFile << ",style=dashed";

    dotFile << "]" << endl << ";" << endl;
  }
};


/*
  buildNodeName returns a string denoting the specified node's name.
*/
string WIR_CFG::buildNodeName( const CGraphVertex v, bool in ) const
{
  DSTART( "string WIR_CFG::buildNodeName(const WIR_CFG::CGraphVertex, bool) const" );

  ostringstream str;
  str << wir;

  switch ( mCGraph[ v ].getType() ) {
    case WIR_CFGNodeType::fct: {
      auto &f = mCGraph[ v ].getFunction();

      str << "< <b>" << f.getName() << "</b> >";

      break;
    }

    case WIR_CFGNodeType::bb: {
      auto &b = mCGraph[ v ].getBasicBlock();

      str << "< ";

      if ( in ) {
        str << "<table border=\"0\">";
        str << "<tr><td colspan=\"2\"><b>" << b.getName() << ":</b></td></tr>";

        for ( WIR_Instruction &i : b ) {
          bool firstOp = true;
          for ( WIR_Operation &o : i ) {
            str << "<tr><td>";

            if ( !firstOp )
              str << "||";
            else
              firstOp = false;

            str << "</td><td align=\"left\">" << o.getOpCode().getName();

            bool firstParam = true;
            for ( WIR_Parameter &p : o )
              if ( p.isExplicit() ) {
                if ( firstParam ) {
                  str << " " << p;
                  firstParam = false;
                } else
                  str << ", " << p;
              }

            str << "</td></tr>";
          }
        }
        str << "</table>";
      } else
        str << "<b>" << b.getName() << "</b>";

      str << " >";

      break;
    }
  }

  return( str.str() );
};


/*
  Default constructor for CFG visitors.
*/
// cppcheck-suppress uninitMemberVar
WIR_CFG::CFGVisitor::CFGVisitor( WIR_BasicBlockSet &s,
                                 std::list<std::reference_wrapper<WIR_BasicBlock>> &o,
                                 std::list<std::reference_wrapper<WIR_BasicBlock>> &p,
                                 std::list<std::reference_wrapper<WIR_BasicBlock>> &r,
                                 std::map<CGraphEdge, WIR_CFGEdgeType> &t,
                                 std::set<CGraphEdge> &b ) :
  mStartNodes { s },
  mDFSOrder { o },
  mReversePostOrder { p },
  mReverseTopologicalOrder { r },
  mEdgeType { t },
  mBackEdges { b }
{
  DSTART(
    "WIR_CFG::CFGVisitor::CFGVisitor(WIR_BasicBlockSet&, list<reference_wrapper<WIR_BasicBlock> >&, list<reference_wrapper<WIR_BasicBlock> >&, list<reference_wrapper<WIR_BasicBlock> >&, map<CGraphEdge, WIR_CFGEdgeType>&, set<CGraphEdge>&)" );
};


void WIR_CFG::CFGVisitor::start_vertex( CGraphVertex v, const CGraph &g ) const
{
  DSTART(
    "void WIR_CFG::CFGVisitor::start_vertex(WIR_CFG::CGraphVertex, const CGraph&) const" );

  DOUT(
    "Start node '" << g[ v ].getBasicBlock().getName() << "' detected." <<
    endl );

  mStartNodes.insert( g[ v ].getBasicBlock() );
};


/*
  discover_vertex visits a new CFG node.
*/
void WIR_CFG::CFGVisitor::discover_vertex( CGraphVertex v, const CGraph &g ) const
{
  DSTART(
    "void WIR_CFG::CFGVisitor::discover_vertex(WIR_CFG::CGraphVertex, const CGraph&) const" );

  DOUT( "Visiting node '" << g[ v ].getBasicBlock().getName() << "'." << endl );

  mDFSOrder.push_back( g[ v ].getBasicBlock() );
};


/*
  finish_vertex finishes a visited CFG node.
*/
void WIR_CFG::CFGVisitor::finish_vertex( CGraphVertex v, const CGraph &g ) const
{
  DSTART(
    "void WIR_CFG::CFGVisitor::finish_vertex(WIR_CFG::CGraphVertex, const CGraph&) const" );

  DOUT(
    "Finishing node '" << g[ v ].getBasicBlock().getName() << "'." << endl );

  mReversePostOrder.push_front( g[ v ].getBasicBlock() );
  mReverseTopologicalOrder.push_back( g[ v ].getBasicBlock() );
};


/*
  examine_edge marks a new CFG edge as regular, true, call or return edge.
*/
void WIR_CFG::CFGVisitor::examine_edge( CGraphEdge e, const CGraph &g ) const
{
  DSTART(
    "void WIR_CFG::CFGVisitor::examine_edge(WIR_CFG::CGraphEdge, const CGraph&) const" );

  CGraphVertex src = source( e, g );
  CGraphVertex tgt = target( e, g );

  WIR_BasicBlock &s = g[ src ].getBasicBlock();
  WIR_BasicBlock &t = g[ tgt ].getBasicBlock();

  DOUT(
    "Examining edge '" << s.getName() << "' -> '" << t.getName() << "' (" );

  // Edges leaving an empty basic block are always regular.
  if ( s.getInstructions().empty() ) {
    DOUT( "regular)." << endl );
    return;
  }

  // Find last non-empty instruction in s.
  auto iIt = s.rbegin();
  for ( ; iIt != s.rend(); ++iIt )
    if ( !iIt->get().getOperations().empty() )
      break;

  // If the instructions in s are all empty, the edge leaving s is also regular.
  if ( iIt == s.rend() ) {
    DOUT( "regular)." << endl );
    return;
  }

  WIR_Instruction &i = iIt->get();

  // Inspect all operations in i and check whether they are calls, returns or
  // conditional jumps.
  for ( auto oIt = i.rbegin(); oIt != i.rend(); ++oIt ) {
    WIR_Operation &o = oIt->get();

    if ( ( o.isCall() || o.isIndirectCall() ) &&
         ( ( s.getFunction() != t.getFunction() ) ||
           ( t == s.getFunction().begin()->get() ) ) ) {
      DOUT( "call)." << endl );
      mEdgeType[ e ] = WIR_CFGEdgeType::call;
      return;
    } else

    if ( o.isReturn() ) {
      DOUT( "return)." << endl );
      mEdgeType[ e ] = WIR_CFGEdgeType::ret;
      return;
    } else

    if ( o.isConditionalJump() ) {
      // Get the implicit successor of s.
      auto bIt = s.getFunction().findBasicBlock( s );
      ++bIt;

      if ( bIt == s.getFunction().getBasicBlocks().end() )
        break;

      // If the implicit successor of s in not t, then the edge s -> t is a true
      // edge.
      if ( bIt->get() != t ) {
        DOUT( "true)." << endl );
        mEdgeType[ e ] = WIR_CFGEdgeType::tru;
        return;
      }
    }
  }

  DOUT( "regular)." << endl );
};


/*
  back_edge marks an identified back edge.
*/
void WIR_CFG::CFGVisitor::back_edge( CGraphEdge e, const CGraph &g ) const
{
  DSTART(
    "void WIR_CFG::CFGVisitor::back_edge(WIR_CFG::CGraphEdge, const CGraph&) const" );

  (void) g;

  DACTION(
    CGraphVertex src = source( e, g );
    CGraphVertex tgt = target( e, g );

    WIR_BasicBlock &s = g[ src ].getBasicBlock();
    WIR_BasicBlock &t = g[ tgt ].getBasicBlock();

    DOUT(
      "Examining edge '" << s.getName() << "' -> '" << t.getName() <<
      "' (back)." << endl ); );

  mBackEdges.insert( e );
};

}       // namespace WIR
