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
  @file wirdfg.cc
  @brief This file implements bit-true data flow graphs.

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
WIR_DFG::WIR_DFG( WIR_Function &f, bool verbosity, bool keepTmpFiles ) :
  mFunction { f },
  mVerbosity { verbosity },
  mKeepTmpFiles { keepTmpFiles }
{
  DSTART( "WIR_DFG::WIR_DFG(WIR_Function&, bool, bool)" );
};


/*
  Destructor.
*/
WIR_DFG::~WIR_DFG( void )
{
  DSTART( "virtual WIR_DFG::~WIR_DFG()" );
};


/*
  build rebuilds the DFG from scratch.
*/
void WIR_DFG::build( void )
{
  DSTART( "void WIR_DFG::build()" );

  // Clear all internal data structures.
  mDGraph.clear();
  mNodeByID.clear();
  mSourceNodes.clear();
  mSinkNodes.clear();

  // Do def-use/use-def chain analysis.
  WIR_DUUDChainAnalysis duchain( mFunction );
  duchain.analyze();

  // Do reachability analysis.
  WIR_ReachabilityAnalysis reachability( mFunction );
  reachability.analyze();

  addNodes();
  addEdges();

  classify();
  computeDistances();

  // Free some no longer needed memory.
  mFunction.eraseContainers( WIR_DUUDChain::getContainerTypeID(), true );
  mFunction.eraseContainers( WIR_Reachability::getContainerTypeID(), true );
};


/*
  getSourceNodes returns the set of all WIR operations denoting DFG source
  nodes.

  According to Jens Wagner, Retargierbare Ausnutzung von Spezialoperationen für
  Eingebettete Systeme mit Hilfe bitgenauer Wertflussanalyse, page 155,
  definition 4.5, a node is a source node if it has no incoming edges except
  back-edges.
*/
const WIR_OperationSet &WIR_DFG::getSourceNodes( void ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  return( mSourceNodes );
};


/*
  getSinkNodes returns the set of all WIR operations denoting DFG sink nodes.

  According to Jens Wagner, Retargierbare Ausnutzung von Spezialoperationen für
  Eingebettete Systeme mit Hilfe bitgenauer Wertflussanalyse, page 155,
  definition 4.6, a node is a sink node if it has no outgoing edges except
  back-edges.
*/
const WIR_OperationSet &WIR_DFG::getSinkNodes( void ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  return( mSinkNodes );
};


/*
  getDistance returns the distance between any pair of nodes in the data flow
  graph.

  The distance between two DFG nodes is defined as the minimum number of edges
  along the shortest path from the first to the second node.
*/
long long WIR_DFG::getDistance( const WIR_Operation &o1,
                                const WIR_Operation &o2 ) const
{
  DSTART(
    "long long int WIR_DFG::getDistance(const WIR_Operation&, const WIR_Operation&) const" );

  return(
    mDistances.at( mNodeByID.at( o1.getID() ) ).at(
      mNodeByID.at( o2.getID() ) ) );
};


/*
  visualize dumps the current data flow graph into a DOT file and invokes xdot
  on it.
*/
void WIR_DFG::visualize( void ) const
{
  DSTART( "void WIR_DFG::visualize() const" );

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
  map<WIR_id_t, unsigned int> idToDotInt;
  visualizeNodes( dotFile, nodeToDotInt, idToDotInt );

  // Write dot graph edges.
  visualizeEdges( dotFile, nodeToDotInt, idToDotInt );

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
  buildNodeName returns a string denoting the specified node's name.
*/
string WIR_DFG::buildNodeName( DGraphVertex v ) const
{
  DSTART( "string WIR_DFG::buildNodeName(WIR_DFG::DGraphVertex) const" );

  ostringstream str;

  switch ( mDGraph[ v ].getType() ) {
    case WIR_DFGNodeType::op: {
      auto &o = mDGraph[ v ].getOperation();

      str << o.getOpCode().getName();

      bool firstParam = true;
      for ( WIR_Parameter &p : o ) {
        if ( firstParam ) {
          str << " " << p;
          firstParam = false;
        } else
          str << ", " << p;
      }

      break;
    }

    case WIR_DFGNodeType::imm: {
      if ( mDGraph[ v ].getImmediateParameter().isSigned() )
        str << mDGraph[ v ].getImmediateParameter().getSignedValue();
      else
        str << mDGraph[ v ].getImmediateParameter().getUnsignedValue();

      break;
    }

    case WIR_DFGNodeType::reg: {
      auto &r = mDGraph[ v ].getRegisterParameter().getRegister();

      if ( r.isVirtual() ) {
        auto &vreg = dynamic_cast<WIR_VirtualRegister &>( r );

        if ( vreg.isPrecolored() )
          return( vreg.getPrecolor().getName() );
        else
          return( r.getName() );
      } else
        return( r.getName() );

      break;
    }
  }

  return( str.str() );
};


//
// Private class methods
//

/*
  addNodes adds DFG nodes for all operations or input parameters in the
  specified WIR function.

  If special nodes are created for immediate parameters or registers that are
  defined outside the current WIR function, addNodes also directly adds the
  corresponding graph edge.
*/
void WIR_DFG::addNodes( void )
{
  DSTART( "void WIR_DFG::addNodes()" );

  // Some maps for bookkeeping that only a single DFG node is created per
  // external register.
  map<WIR_id_t, DGraphVertex> extRegisters;

  // Traverse the current WIR function.
  for ( WIR_BasicBlock &b : mFunction )
    for ( WIR_Instruction &i : b )
      for ( WIR_Operation &o : i ) {
        DGraphVertex v1 = add_vertex( WIR_DFGNodeProperty( o ), mDGraph );
        mNodeByID[ o.getID() ] = v1;
        DOUT(
          "Adding node for operation '" << buildNodeName( v1 ) << "'." <<
          endl );

        for ( WIR_Parameter &p : o ) {
          if ( p.getType() == WIR_ParameterType::imm ) {
            auto &ip = dynamic_cast<WIR_BaseImmediateParameter &>( p );

            DGraphVertex v2 = add_vertex( WIR_DFGNodeProperty( ip ), mDGraph );
            DOUT(
              "Adding node for immediate parameter '" << buildNodeName( v2 ) <<
              "'." << endl );

            mNodeByID[ p.getID() ] = v2;

            DOUT(
              "Adding edge '" << buildNodeName( v2 ) << "' -> '" <<
              buildNodeName( v1 ) << "'." << endl );
            add_edge( v2, v1, WIR_DFGEdgeProperty( ip, *this ), mDGraph );
          } else

          if ( p.getType() == WIR_ParameterType::reg ) {
            auto &rp = dynamic_cast<WIR_RegisterParameter &>( p );

            if ( rp.isUsed() || rp.isDefUsed() ) {
              // Check whether there is no UD chain ending in p. If so, p is an
              // externally defined register.
              WIR_DUUDChain &rpContainer =
                rp.getContainers<WIR_DUUDChain>().begin()->get();

              auto &reg = rp.getRegister();
              WIR_PhysicalRegister *phReg = nullptr;
              if ( reg.isPhysical() )
                phReg = dynamic_cast<WIR_PhysicalRegister *>( &reg );
              else {
                auto &vReg = dynamic_cast<WIR_VirtualRegister &>( reg );
                if ( vReg.isPrecolored() )
                  phReg = &(vReg.getPrecolor() );
              }

              if ( rpContainer.getUDChains().empty() ||
                   ( phReg && rpContainer.getUDInputs().count( *phReg ) ) ) {
                DGraphVertex v2;

                // Determine the actual external register depending on whether
                // it is physical or virtual or precolored.
                WIR_BaseRegister &reg =
                  rp.getRegister().isPhysical() ?
                    rp.getRegister() :
                    !dynamic_cast<WIR_VirtualRegister &>(
                      rp.getRegister() ).isPrecolored() ?
                      rp.getRegister() :
                      dynamic_cast<WIR_VirtualRegister &>(
                        rp.getRegister() ).getPrecolor();

                // Check whether a graph node for the current external register
                // already exists.
                auto it = extRegisters.find( reg.getID() );

                if ( it != extRegisters.end() )
                  v2 = it->second;
                else {
                  v2 = add_vertex( WIR_DFGNodeProperty( rp ), mDGraph );
                  extRegisters[ reg.getID() ] = v2;

                  DOUT(
                    "Adding node for externally defined register parameter '" <<
                    buildNodeName( v2 ) << "'." << endl );
                }

                mNodeByID[ p.getID() ] = v2;

                DOUT(
                  "Adding edge '" << buildNodeName( v2 ) << "' -> '" <<
                  buildNodeName( v1 ) << "'." << endl );
                auto p =
                  add_edge( v2, v1, WIR_DFGEdgeProperty( rp, *this ), mDGraph );
                postProcessEdge( mDGraph[ p.first ] );
              }
            }
          }
        }
      }
};


/*
  addEdges adds DFG edges between all data flow-dependent operations in the
  specified WIR function.
*/
void WIR_DFG::addEdges( void )
{
  DSTART( "void WIR_DFG::addEdges()" );

  // Traverse the current WIR function.
  for ( WIR_BasicBlock &b : mFunction )
    for ( WIR_Instruction &i : b )
      for ( WIR_Operation &o : i )
        for ( WIR_Parameter &p : o ) {
          DGraphVertex v1 = mNodeByID[ o.getID() ];

          if ( p.getType() == WIR_ParameterType::reg ) {
            auto &rp = dynamic_cast<WIR_RegisterParameter &>( p );

            if ( rp.isUsed() || rp.isDefUsed() ) {
              // For a used or def-used register parameter, determine the UD
              // chains ending in p.
              WIR_DUUDChain &rpContainer =
                rp.getContainers<WIR_DUUDChain>().begin()->get();

              // Iterate all UD chains and add DFG edges.
              for ( WIR_RegisterParameter &def : rpContainer.getUDChains() )
                if ( addEdge( def, rp ) ) {
                  DGraphVertex v2 = mNodeByID[ def.getOperation().getID() ];

                  DOUT(
                    "Adding edge '" << buildNodeName( v2 ) << "' -> '" <<
                    buildNodeName( v1 ) << "'." << endl );
                  add_edge( v2, v1, { def, rp, *this }, mDGraph );
                }
            }
          }
        }
};


/*
  postProcessEdge can be used to set properties of newly created DFG edges to
  particular, processor-dependent values.

  This method does no post-processing at all. For particular processor
  architectures, however, this method can be overloaded in order to properly
  reflect processor-specific properties in the DFG.

  A prime example could be a register that always contains the value 0. Reading
  this register should result in up/down values only containing 0 bits, which
  can be realized here (see, e.g., register x0 of RISC-V processor
  architectures).
*/
void WIR_DFG::postProcessEdge( WIR_DFGEdgeProperty &ep )
{
  DSTART( "virtual void WIR_DFG::postProcessEdge(WIR_DFGEdgeProperty&)" );

  (void) ep;
};


/*
  addEdge returns whether an edge from a defined register parameter to a used
  register parameter shall be added to the DFG.

  This method always returns true, all edges are always added to the DFG. For
  particular processor architectures, however, this method can be overloaded in
  order to properly reflect processor-specific properties in the DFG.

  A prime example could be a register that always contains the value 0,
  irrespective of what is written into it. For such a register, a proper def-use
  relationship in the DFG does not make sense so that no edge should be added
  (see, e.g., register x0 of RISC-V processor architectures).
*/
bool WIR_DFG::addEdge( const WIR_RegisterParameter &def,
                       const WIR_RegisterParameter &use )
{
  DSTART(
    "virtual bool WIR_DFG::addEdge(const WIR_RegisterParameter&, const "
    "WIR_RegisterParameter&)" );

  (void) def;
  (void) use;

  return( true );
};


/*
  classify traverses the DFG and classifies the nodes as sources or sinks of
  data flow.

  Only DFG nodes representing true WIR operations are classified as source or
  sink, since the other types of DFG nodes by construction denote graph inputs.

  A node is a source if it has no incoming edges except from back edges. A node
  is a sink if it has no outgoing edges except from back edges. See definitions
  4.5 and 4.6 in J. Wagner. "Retargierbare Ausnutzung von Spezialoperationen für
  Eingebettete Systeme mit Hilfe bitgenauer Wertflussanalyse". Ph.D. thesis,
  Dortmund University, page 155, 2006.
*/
void WIR_DFG::classify( void )
{
  DSTART( "void WIR_DFG::classify()" );

  BGL_FORALL_VERTICES( v, mDGraph, DGraph ) {
    if ( mDGraph[ v ].isOperation() ) {
      bool isSourceNode = true;

      for ( auto p = in_edges( v, mDGraph ); p.first != p.second; ++p.first ) {
        auto e = *(p.first);
        auto start = source( e, mDGraph );

        if ( mDGraph[ start ].isOperation() && !mDGraph[ e ].isBackEdge() ) {
          isSourceNode = false;
          break;
        }
      }

      if ( isSourceNode ) {
        DOUT(
          "Marking node '" << buildNodeName( v ) << "' as source." << endl );
        mDGraph[ v ].setSource( isSourceNode );
        mSourceNodes.insert( mDGraph[ v ].getOperation() );
      }

      bool isSinkNode = true;

      for ( auto p = out_edges( v, mDGraph ); p.first != p.second; ++p.first )
        if ( !mDGraph[ *(p.first) ].isBackEdge() ) {
          isSinkNode = false;
          break;
        }

      if ( isSinkNode ) {
        DOUT( "Marking node '" << buildNodeName( v ) << "' as sink." << endl );
        mDGraph[ v ].setSink( isSinkNode );
        mSinkNodes.insert( mDGraph[ v ].getOperation() );
      }
    }
  }
};


/*
  computeDistances computes the distance between any pair of nodes in the DFG.

  The distance between two DFG nodes is defined as the minimum number of edges
  along the shortest path from the first to the second node. computeDistances
  thus basically performs an all-pairs shortest path computation on the DFG and
  stores the resulting distances in map mDistances.
*/
void WIR_DFG::computeDistances( void )
{
  DSTART( "void WIR_DFG::computeDistances()" );

  mDistances.clear();

  // Solve the all-pairs shortest paths problem.
  using DistanceProperty = boost::exterior_vertex_property<DGraph, long long>;
  using DistanceMatrix = DistanceProperty::matrix_type;
  using DistanceMatrixMap = DistanceProperty::matrix_map_type;

  DistanceMatrix distances( num_vertices( mDGraph ) );
  DistanceMatrixMap dm( distances, mDGraph );
  bool valid =
    floyd_warshall_all_pairs_shortest_paths(
      mDGraph, dm,
      weight_map( get( &WIR_DFGEdgeProperty::mDistance, mDGraph ) ) );

  ufAssertT( valid, "Found negative weight cycle in data flow graph." );

  // Permanently store all computed pair-wise distances.
  BGL_FORALL_VERTICES( v, mDGraph, DGraph ) {
    BGL_FORALL_VERTICES( w, mDGraph, DGraph )
      mDistances[ v ][ w ] = distances[ v ][ w ];
  }
};


/*
  visualizeNodes dumps the data flow graph nodes into a given DOT file.
*/
void WIR_DFG::visualizeNodes( std::fstream &dotFile,
                              std::map<DGraphVertex, unsigned int> &nodeToDotInt,
                              std::map<WIR_id_t, unsigned int> &idToDotInt ) const
{
  DSTART(
    "void WIR_DFG::visualizeNodes(fstream&, map<unsigned int, unsigned int>&, map<long long unsigned int, unsigned int>&) const" );

  unsigned int nodeCounter = 0;

  // Produce a legend in the visualization.
  dotFile << "subgraph clusterLegend {" << endl;

  dotFile << nodeCounter++ << "[label=\"Operation\",shape=box]" << endl << ";"
          << endl;

  dotFile << nodeCounter++ << "[label=\"Source Node\",shape=box,style=bold]"
          << endl << ";" << endl;

  dotFile << nodeCounter++ << "[label=\"Sink Node\",shape=box,style=dashed]"
          << endl << ";" << endl;

  dotFile << nodeCounter++ << "[label=\"Immediate Value\",style=dotted]" << endl
          << ";" << endl;

  dotFile << nodeCounter++ << "[label=\"External Register\",style=dotted"
          << ",shape=hexagon]" << endl << ";" << endl;

  dotFile << nodeCounter++ << "[shape=point]" << endl << ";" << endl;
  dotFile << nodeCounter++ << "[shape=point]" << endl << ";" << endl;
  dotFile << nodeCounter - 2 << "->" << nodeCounter - 1
          << "[label=\"Back edge\",dir=forward,arrowhead=vee,arrowtail=none,"
          << "style=dashed]" << endl << ";" << endl;

  dotFile << nodeCounter++ << "[label=\""
          << mFunction.getCompilationUnit().getName() << "\\n"
          << mFunction.getName() << "\",color=white]" << endl << ";" << endl;

  dotFile << "}" << endl;

  // Visualize all graph nodes.
  BGL_FORALL_VERTICES( v, mDGraph, DGraph ) {
    switch ( mDGraph[ v ].getType() ) {
      case WIR_DFGNodeType::op: {
        auto &o = mDGraph[ v ].getOperation();

        dotFile << "subgraph cluster" << nodeCounter << " {" << endl;

        if ( mDGraph[ v ].isSource() )
          dotFile << "style=bold;" << endl;
        else

        if ( mDGraph[ v ].isSink() )
          dotFile << "style=dashed;" << endl;

        nodeToDotInt[ v ] = nodeCounter++;

        for ( auto it = o.rbegin(); it != o.rend(); ++it ) {
          idToDotInt[ it->get().getID() ] = nodeCounter;

          dotFile << nodeCounter++ << " [label=\"" << *it;
          if ( it != o.rbegin() )
            dotFile << ",";
          dotFile << "\",color=white,shape=box]" << endl << ";" << endl;
        }

        dotFile << nodeCounter++ << " [label=\"" << o.getOpCode().getName()
                << " (ID=" << o.getID() << ")" << "\",color=white,shape=box]"
                << endl << ";" << endl;

        dotFile << "}" << endl;

        break;
      }

      case WIR_DFGNodeType::imm: {
        dotFile << nodeCounter << " [label=";
        dotFile << buildNodeName( v ) << ",style=dotted";
        dotFile << "]" << endl << ";" << endl;

        nodeToDotInt[ v ] = nodeCounter++;

        break;
      }

      case WIR_DFGNodeType::reg: {
        dotFile << nodeCounter << " [label=";
        dotFile << "\"" << buildNodeName( v )
                << "\",style=dotted,shape=hexagon";
        dotFile << "]" << endl << ";" << endl;

        nodeToDotInt[ v ] = nodeCounter++;

        break;
      }
    }
  }
};


/*
  visualizeEdges dumps the data flow graph edges into a given DOT file.
*/
void WIR_DFG::visualizeEdges( std::fstream &dotFile,
                              const std::map<DGraphVertex, unsigned int> &nodeToDotInt,
                              const std::map<WIR_id_t, unsigned int> &idToDotInt ) const
{
  DSTART(
    "void WIR_DFG::visualizeEdges(fstream&, const map<unsigned int, unsigned int>&, const map<long long unsigned int, unsigned int>&) const" );

  BGL_FORALL_EDGES( e, mDGraph, DGraph ) {
    DGraphVertex src = source( e, mDGraph );
    auto &edgeProperty = const_cast<WIR_DFGEdgeProperty &>( mDGraph[ e ] );
    unsigned int s, t;

    switch ( edgeProperty.getType() ) {
      case WIR_DFGNodeType::op: {
        s = idToDotInt.at( edgeProperty.getSourceRegisterParameter().getID() );
        t = idToDotInt.at( edgeProperty.getTargetRegisterParameter().getID() );

        break;
      }

      case WIR_DFGNodeType::imm: {
        s = nodeToDotInt.at( src );
        t = idToDotInt.at( edgeProperty.getImmediateParameter().getID() );

        break;
      }

      case WIR_DFGNodeType::reg: {
        s = nodeToDotInt.at( src );
        t = idToDotInt.at( edgeProperty.getTargetRegisterParameter().getID() );

        break;
      }
    }

    dotFile << s << "->" << t
            << " [label=< <table border=\"0\"><tr><td>Dn:</td><td>"
            << edgeProperty.getDownValue() << "</td></tr><tr><td>Up:</td>"
            << "<td>" << edgeProperty.getUpValue() << "</td></tr></table> >"
            << ",dir=forward,arrowhead=vee,arrowtail=none";
    if ( edgeProperty.isBackEdge() )
      dotFile << ",style=dashed";
    dotFile << "]" << endl << ";" << endl;
  }
};

}       // namespace WIR
