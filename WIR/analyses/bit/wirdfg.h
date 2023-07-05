/*

   This header file belongs to the

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
  @file wirdfg.h
  @brief This file provides the basic interface of bit-true data flow graphs.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_DFG_H
#define _WIR_DFG_H


//
// Include section
//

// Include standard headers
#include <fstream>
#include <map>
#include <string>

// Include WIR headers
#include <wir/wirtypes.h>

// Include local headers
#include "wirdfgedgeproperty.h"
#include "wirdfgnodeproperty.h"


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_DFGEdgeProperty;
class WIR_Function;
class WIR_Operation;
class WIR_RegisterParameter;


/*!
  @brief Class WIR_DFG implements a generic, machine-independent, %WIR data flow
         graph for bit-true data and value flow analysis.

  This data flow graph basically holds nodes for all %WIR operations of a
  function. Special nodes are maintained for DFG inputs, i.e., for immediate
  operands and registers defined outside a %WIR function.

  Directed edges between nodes model classical data flow based on DEF/USE
  chains where bit-true information about the value flowing along an edge is
  modeled using %WIR up and down values. Each edge refers to exactly one
  parameter of the source node's operation and to another parameter of the
  target node's operation. There may be multiple parallel edges between any two
  graph nodes.

  This modeling of bit-true data flow graphs is taken from:
  J. Wagner. "Retargierbare Ausnutzung von Spezialoperationen für Eingebettete
  Systeme mit Hilfe bitgenauer Wertflussanalyse". Ph.D. thesis, Dortmund
  University, 2006.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_DFG
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor for a given %WIR function.

      @param[in] f A reference to a %WIR function used to construct the data
                   flow graph.
      @param[in] verbosity A Boolean denoting whether verbose messages shall be
                           dumped.
      @param[in] keepTmpFiles A Boolean denoting whether temporary files shall
                              be kept.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_DFG( WIR_Function &, bool = false, bool = false );

    /*!
      @brief Destructor.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_DFG( void );


    //
    // General graph properties.
    //

    /*!
      @brief build rebuilds the DFG from scratch.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void build( void );

    /*!
      @brief getSourceNodes returns the set of all %WIR operations denoting DFG
             source nodes.

      @return A const reference to set mSourceNodes.

      According to Jens Wagner, Retargierbare Ausnutzung von Spezialoperationen
      für Eingebettete Systeme mit Hilfe bitgenauer Wertflussanalyse, page 155,
      definition 4.5, a node is a source node if it has no incoming edges except
      back-edges.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const WIR_OperationSet &getSourceNodes( void ) const;

    /*!
      @brief getSinkNodes returns the set of all %WIR operations denoting DFG
             sink nodes.

      @return A const reference to set mSinkNodes.

      According to Jens Wagner, Retargierbare Ausnutzung von Spezialoperationen
      für Eingebettete Systeme mit Hilfe bitgenauer Wertflussanalyse, page 155,
      definition 4.6, a node is a sink node if it has no outgoing edges except
      back-edges.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const WIR_OperationSet &getSinkNodes( void ) const;

    /*!
      @brief getDistance returns the distance between any pair of nodes in the
             data flow graph.

      @param[in] o1 A const reference to a first %WIR operation.
      @param[in] o2 A const reference to a second %WIR operation.
      @return A signed long long denoting the distance from node o1 to o2.

      The distance between two DFG nodes is defined as the minimum number of
      edges along the shortest path from the first to the second node.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    long long getDistance( const WIR_Operation &, const WIR_Operation & ) const;

    /*!
      @brief visualize dumps the current data flow graph into a DOT file and
             invokes xdot on it.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void visualize( void ) const;


  protected:

    //
    // Local type definitions.
    //

    /*!
      @brief DGraph represents the internal boost structure of the data flow
             graph.
    */
    using DGraph = WIR_DirMGraph<WIR_DFGNodeProperty, WIR_DFGEdgeProperty>;

    //! DGraphVertex represents a data flow graph vertex.
    using DGraphVertex =
      WIR_DirMGraphVertex<WIR_DFGNodeProperty, WIR_DFGEdgeProperty>;

    //! DGraphEdge represents a data flow graph edge.
    using DGraphEdge =
      WIR_DirMGraphEdge<WIR_DFGNodeProperty, WIR_DFGEdgeProperty>;


    //
    // Graph visualization.
    //

    /*!
      @brief buildNodeName returns a string denoting the specified node's name.

      @param[in] v A graph node whose name shall be built.
      @return A string containing the node's name as it will be displayed in
              dot.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::string buildNodeName( DGraphVertex ) const;


    //
    // Attributes.
    //

    /*!
      @brief mFunction stores a reference to the %WIR function to be
             modeled as data flow graph.
    */
    WIR_Function &mFunction;

    /*!
      @brief mDGraph represents the internal boost graph data structure for the
             data flow graph.
    */
    DGraph mDGraph;

    /*!
      @brief mDistances stores the distance between any pair of nodes in the
             data flow graph.

      The distance between two DFG nodes is defined as the minimum number of
      edges along the shortest path from the first to the second node.
    */
    std::map<DGraphVertex, std::map<DGraphVertex, long long>> mDistances;

    /*!
      @brief mNodeByID maps a %WIR operation's or parameter's ID to its
             representing data flow graph node.
    */
    std::map<WIR_id_t, DGraphVertex> mNodeByID;

    //! mSourceNodes stores all graph source nodes.
    WIR_OperationSet mSourceNodes;

    //! mSinkNodes stores all graph sink nodes.
    WIR_OperationSet mSinkNodes;

    //! mVerbosity stores whether verbose messages should be dumped.
    bool mVerbosity;

    //! mKeepTmpFiles stores whether temporary files should be kept.
    bool mKeepTmpFiles;


  private:

    /*!
      @brief No standard construction allowed, users must use one of the above
             default constructors instead.
    */
    WIR_DFG( void ) = delete;


    //
    // General graph properties.
    //

    /*!
      @brief addNodes adds DFG nodes for all operations or input parameters in
             the specified %WIR function.

      If special nodes are created for immediate parameters or registers that
      are defined outside the current %WIR function, addNodes also directly adds
      the corresponding graph edge.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void addNodes( void );

    /*!
      @brief addEdges adds DFG edges between all data flow-dependent operations
             in the specified %WIR function.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void addEdges( void );

    /*!
      @brief postProcessEdge can be used to set properties of newly created DFG
             edges to particular, processor-dependent values.

      @param[in,out] ep A reference to a DFG edge's properties to be
                        post-processed.

      This method does no post-processing at all. For particular processor
      architectures, however, this method can be overloaded in order to properly
      reflect processor-specific properties in the DFG.

      A prime example could be a register that always contains the value 0.
      Reading this register should result in up/down values only containing 0
      bits, which can be realized here (see, e.g., register x0 of RISC-V
      processor architectures).
    */
    virtual void postProcessEdge( WIR_DFGEdgeProperty & );

    /*!
      @brief addEdge returns whether an edge from a defined register parameter
             to a used register parameter shall be added to the DFG.

      @param[in] def A const reference to a defined register parameter.
      @param[in] use A const reference to a used register parameter.
      @return Always true.

      This method always returns true, all edges are always added to the DFG.
      For particular processor architectures, however, this method can be
      overloaded in order to properly reflect processor-specific properties in
      the DFG.

      A prime example could be a register that always contains the value 0,
      irrespective of what is written into it. For such a register, a proper
      def-use relationship in the DFG does not make sense so that no edge should
      be added (see, e.g., register x0 of RISC-V processor architectures).

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool addEdge( const WIR_RegisterParameter &,
                          const WIR_RegisterParameter & );

    /*!
      @brief classify traverses the DFG and classifies the nodes as sources or
             sinks of data flow.

      Only DFG nodes representing true %WIR operations are classified as source
      or sink, since the other types of DFG nodes by construction denote graph
      inputs.

      A node is a source if it has no incoming edges except from back edges. A
      node is a sink if it has no outgoing edges except from back edges. See
      definitions 4.5 and 4.6 in J. Wagner. "Retargierbare Ausnutzung von
      Spezialoperationen für Eingebettete Systeme mit Hilfe bitgenauer
      Wertflussanalyse". Ph.D. thesis, Dortmund University, page 155, 2006.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void classify( void );

    /*!
      @brief computeDistances computes the distance between any pair of nodes in
             the DFG.

      The distance between two DFG nodes is defined as the minimum number of
      edges along the shortest path from the first to the second node.
      computeDistances thus basically performs an all-pairs shortest path
      computation on the DFG and stores the resulting distances in map
      mDistances.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void computeDistances( void );


    //
    // Graph visualization.
    //

    /*!
      @brief visualizeNodes dumps the data flow graph nodes into a given DOT
             file.

      @param[in] dotFile A reference to a DOT file opened for writing.
      @param[out] nodeToDotInt A reference to a map mapping a Boost graph node
                               to an integer used to identify the node in the
                               DOT file.
      @param[out] idToDotInt A reference to a map mapping a %WIR parameter's ID
                             to an integer used to identify the parameter in the
                             DOT file.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void visualizeNodes( std::fstream &,
                         std::map<DGraphVertex, unsigned int> &,
                         std::map<WIR_id_t, unsigned int> & ) const;

    /*!
      @brief visualizeEdges dumps the data flow graph edges into a given DOT
             file.

      @param[in] dotFile A reference to a DOT file opened for writing.
      @param[in] nodeToDotInt A const reference to a map mapping a Boost graph
                              node to an integer used to identify the node in
                              the DOT file.
      @param[in] idToDotInt A const reference to a map mapping a %WIR
                            parameter's ID to an integer used to identify the
                            parameter in the DOT file.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void visualizeEdges( std::fstream &,
                         const std::map<DGraphVertex, unsigned int> &,
                         const std::map<WIR_id_t, unsigned int> & ) const;

};

}       // namespace WIR

#endif  // _WIR_DFG_H
