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
  @file wircfg.h
  @brief This file provides the basic interface of control flow graphs.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_CFG_H
#define _WIR_CFG_H


//
// Include section
//

// Include standard headers
#include <fstream>
#include <functional>
#include <list>
#include <map>
#include <set>
#include <string>
#include <utility>

// Include boost headers
#include <boost/graph/depth_first_search.hpp>

// Include WIR headers
#include <wir/wirtypes.h>

// Include local headers
#include "wircfgedgeproperty.h"
#include "wircfgnodeproperty.h"


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_BasicBlock;
class WIR_Function;


/*!
  @brief Class WIR_CFG implements a %WIR control flow graph.

  CFG nodes stand for %WIR basic blocks. Two nodes v and w are connected with a
  directed edge if w can be executed immediately after v. CFG edges are
  classified as regular edges, true edges indicating a taken conditional branch
  target, call and return edges and back edges.

  The CFG provides access to its nodes in depth-first as well as in post- and
  reverse-topological order.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_CFG
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor for a given %WIR function.

      @param[in] f A reference to a %WIR function used to construct the control
                   flow graph.
      @param[in] r A Boolean defaulting to false that denotes whether only
                   reachable blocks (true) or all blocks (false) shall be
                   visited.
      @param[in] verbosity A Boolean denoting whether verbose messages shall be
                           dumped.
      @param[in] keepTmpFiles A Boolean denoting whether temporary files shall
                              be kept.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_CFG( WIR_Function &, bool = false, bool = false, bool = false );

    /*!
      @brief Destructor.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ~WIR_CFG( void );


    //
    // General graph properties.
    //

    /*!
      @brief getDFSOrder returns the list of all basic blocks in their
             depth-first traversal order.

      @return A const reference to list mDFSOrder.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const std::list<std::reference_wrapper<WIR_BasicBlock>> &getDFSOrder( void ) const;

    /*!
      @brief getReversePostOrder returns the list of all basic blocks in their
             reverse postorder.

      @return A const reference to list mReversePostOrder.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const std::list<std::reference_wrapper<WIR_BasicBlock>> &getReversePostOrder( void ) const;

    /*!
      @brief getReverseTopologicalOrder returns the list of all basic blocks in
             their reverse-topological order.

      @return A const reference to list mReverseTopologicalOrder.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const std::list<std::reference_wrapper<WIR_BasicBlock>> &getReverseTopologicalOrder( void ) const;

    /*!
      @brief getStartNodes returns all start nodes of the current CFG.

      @return A const reference to the set of CFG start nodes.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const WIR_BasicBlockSet &getStartNodes( void ) const;

    /*!
      @brief getBackEdges returns the set of back edges in the CFG.

      @return A const reference to a set storing back edges as tuples of basic
              block references.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const WIR_BasicBlockPairSet &getBackEdges( void ) const;

    /*!
      @brief visualize dumps the current control flow graph into a DOT file and
             invokes xdot on it.

      @param[in] in A Boolean defaulting to 'false' and specifying whether the
                    internals of CFG nodes (i.e., instructions within a basic
                    block) shall be displayed.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void visualize( bool = false ) const;


  protected:

    //
    // Local type definitions.
    //

    /*!
      @brief CGraph represents the internal boost structure of the control flow
             graph.
    */
    using CGraph = WIR_DirMGraph<WIR_CFGNodeProperty, WIR_CFGEdgeProperty>;

    //! CGraphVertex represents a control flow graph vertex.
    using CGraphVertex =
      WIR_DirMGraphVertex<WIR_CFGNodeProperty, WIR_CFGEdgeProperty>;

    //! CGraphEdge represents a control flow graph edge.
    using CGraphEdge =
      WIR_DirMGraphEdge<WIR_CFGNodeProperty, WIR_CFGEdgeProperty>;


    //
    // Initialization
    //

    /*!
      @brief rebuild rebuilds the internal CFG from scratch for function
             mFunction.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void rebuild( void );


    //
    // Attributes.
    //

    /*!
      @brief mFunction stores a const reference to the %WIR function to be
             modeled as control flow graph.
    */
    const WIR_Function &mFunction;

    /*!
      @brief mCGraph represents the internal boost graph data structure for the
             control flow graph.
    */
    CGraph mCGraph;

    /*!
      @brief mNodeByID maps a %WIR basic block's ID to its representing control
             flow graph node.
    */
    std::map<WIR_id_t, CGraphVertex> mNodeByID;

    //! mStartNodes contains all start nodes of the CFG.
    WIR_BasicBlockSet mStartNodes;

    //! mBackEdges contains all identified back edges in the CFG.
    std::set<CGraphEdge> mBackEdges;

    /*!
      @brief mWIRBackEdges contains all identified back edges in the form of
             tuples of basic block IDs.
    */
    WIR_BasicBlockPairSet mWIRBackEdges;

    /*!
      @brief mDFSOrder lists all basic blocks of the current function in their
             depth-first traversal order.
    */
    std::list<std::reference_wrapper<WIR_BasicBlock>> mDFSOrder;

    /*!
      @brief mReversePostOrder lists all basic blocks of the current function
             in their reverse postorder.
    */
    std::list<std::reference_wrapper<WIR_BasicBlock>> mReversePostOrder;

    /*!
      @brief mReverseTopologicalOrder lists all basic blocks of the current
             function in their reverse-topological order.
    */
    std::list<std::reference_wrapper<WIR_BasicBlock>> mReverseTopologicalOrder;

    /*!
      @brief mOnlyReachableBlocks stores whether a depth-first traversal shall
             start only in a function's entry point so that only all reachable
             blocks are visited.
    */
    bool mOnlyReachableBlocks;

    //! mVerbosity stores whether verbose messages should be dumped.
    bool mVerbosity;

    //! mKeepTmpFiles stores whether temporary files should be kept.
    bool mKeepTmpFiles;


  private:

    //
    // General graph properties.
    //

    /*!
      @brief No standard construction allowed, users must use one of the above
             default constructors instead.
    */
    WIR_CFG( void ) = delete;

    /*!
      @brief addNodes adds CFG nodes for all basic blocks in the specified %WIR
             function.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void addNodes( void );

    /*!
      @brief addEdges adds edges between all control flow-dependent CFG nodes.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void addEdges( void );

    /*!
      @brief classify traverses the CFG in depth-first order and determines the
             nodes' and edges' types.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void classify( void );


    //
    // Graph visualization.
    //

    /*!
      @brief visualizeNodes dumps the control flow graph nodes into a given DOT
             file.

      @param[in] dotFile A reference to a DOT file opened for writing.
      @param[out] nodeToDotInt A reference to a map mapping a Boost graph node
                               to an integer used to identify the node in the
                               DOT file.
      @param[in] in A Boolean defaulting to 'false' and specifying whether the
                    internals of CFG nodes (i.e., instructions within a basic
                    block) shall be displayed.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void visualizeNodes( std::fstream &,
                         std::map<CGraphVertex, unsigned int> &,
                         bool = false ) const;

    /*!
      @brief visualizeEdges dumps the control flow graph edges into a given DOT
             file.

      @param[in] dotFile A reference to a DOT file opened for writing.
      @param[in] nodeToDotInt A const reference to a map mapping a Boost graph
                              node to an integer used to identify the node in
                              the DOT file.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void visualizeEdges( std::fstream &,
                         const std::map<CGraphVertex, unsigned int> & ) const;

    /*!
      @brief buildNodeName returns a string denoting the specified node's name.

      @param[in] v A graph node whose name shall be built.
      @param[in] in A Boolean defaulting to 'false' and specifying whether the
                    internals of CFG nodes (i.e., instructions within a basic
                    block) shall be displayed.
      @return A string containing the node's name as it will be displayed in
              dot.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::string buildNodeName( const CGraphVertex, bool = false ) const;


    //
    // DFS visitor.
    //

    class CFGVisitor : public boost::default_dfs_visitor
    {

      public:

        /*!
          @brief Default constructor for CFG visitors.

          @param[in,out] s A reference to a set containing all start nodes of a
                           CFG after DFS graph traversal.
          @param[in,out] o A reference to a list of CFG nodes in their
                           depth-first traversal order.
          @param[in,out] p A reference to a list of CFG nodes in their reverse
                           postorder.
          @param[in,out] r A reference to a list of CFG nodes in their
                           reverse-topological order.
          @param[in,out] t A reference to a map storing each CFG edge's type.
          @param[in,out] b A reference to a set containing all back edges of a
                           CFG after DFS graph traversal.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        CFGVisitor( WIR_BasicBlockSet &s,
                    std::list<std::reference_wrapper<WIR_BasicBlock>> &o,
                    std::list<std::reference_wrapper<WIR_BasicBlock>> &p,
                    std::list<std::reference_wrapper<WIR_BasicBlock>> &r,
                    std::map<CGraphEdge, WIR_CFGEdgeType> &t,
                    std::set<CGraphEdge> &b );

        /*!
          @brief start_vertex marks an identified start node of a CFG.

          @param[in] v The start node.
          @param[in] g A const reference to the CFG.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        void start_vertex( CGraphVertex, const CGraph & ) const;

        /*!
          @brief discover_vertex visits a new CFG node.

          @param[in] v The currently visited CFG node.
          @param[in] g A const reference to the CFG.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        void discover_vertex( CGraphVertex, const CGraph & ) const;

        /*!
          @brief finish_vertex finishes a visited CFG node.

          @param[in] v The currently visited CFG node.
          @param[in] g A const reference to the CFG.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        void finish_vertex( CGraphVertex, const CGraph & ) const;

        /*!
          @brief examine_edge marks a new CFG edge as regular, true, call or
                 return edge.

          @param[in] e The currently examined CFG edge.
          @param[in] g A const reference to the CFG.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        void examine_edge( CGraphEdge, const CGraph & ) const;

        /*!
          @brief back_edge marks an identified back edge.

          @param[in] e The currently examined CFG edge.
          @param[in] g A const reference to the CFG.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        void back_edge( CGraphEdge, const CGraph & ) const;


      private:

        //! mStartNodes refers to a set containing all start nodes of a CFG.
        WIR_BasicBlockSet &mStartNodes;

        /*!
          @brief mDFSOrder refers to a list of CFG nodes in their depth-first
                 traversal order.
        */
        std::list<std::reference_wrapper<WIR_BasicBlock>> &mDFSOrder;

        /*!
          @brief mReversePostOrder refers to a list of CFG nodes in their
                 reverse postorder.
        */
        std::list<std::reference_wrapper<WIR_BasicBlock>> &mReversePostOrder;

        /*!
          @brief mReverseTopologicalOrder refers to a list of CFG nodes in their
                 reverse-topological order.
        */
        std::list<std::reference_wrapper<WIR_BasicBlock>> &mReverseTopologicalOrder;

        //! mEdgeType refers to a map storing each CFG edge's type.
        std::map<CGraphEdge, WIR_CFGEdgeType> &mEdgeType;

        /*!
          @brief mBackEdges refers to a set containing all identified back edges
                 in a CFG.
        */
        std::set<CGraphEdge> &mBackEdges;

    };

};

}       // namespace WIR

#endif  // _WIR_CFG_H
