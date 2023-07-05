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
  @file wirhierarchicalcfg.h
  @brief This file provides the basic interface of hierarchical control flow
         graphs.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_HIERARCHICALCFG_H
#define _WIR_HIERARCHICALCFG_H


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

// Include boost headers
#include <boost/graph/depth_first_search.hpp>

// Include WIR headers
#include <wir/wirtypes.h>
#include <analyses/structuralcontrolflow/wirbasicblocktreenode.h>
#include <analyses/structuralcontrolflow/wirblocktreenode.h>
#include <analyses/structuralcontrolflow/wirifthenelsetreenode.h>
#include <analyses/structuralcontrolflow/wirifthentreenode.h>
#include <analyses/structuralcontrolflow/wirimpropertreenode.h>
#include <analyses/structuralcontrolflow/wirpropertreenode.h>
#include <analyses/structuralcontrolflow/wirnaturallooptreenode.h>
#include <analyses/structuralcontrolflow/wirselflooptreenode.h>
#include <analyses/structuralcontrolflow/wirswitchcasetreenode.h>
#include <analyses/structuralcontrolflow/wirwhilelooptreenode.h>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_BasicBlock;
class WIR_BasicBlockTreeNode;
class WIR_BlockTreeNode;
class WIR_ControlTreeNode;
class WIR_Function;


/*!
  @brief Class WIR_HierarchicalCFG implements a hierarchical %WIR control flow
         graph.

  CFG nodes stand for hierarchical control tree nodes. Two nodes v and w are
  connected with a directed edge if w can be executed immediately after v.

  The hierarchical CFG provides access to its nodes in DFS postorder.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_HierarchicalCFG
{

  public:

    //
    // Local type definitions.
    //

    /*!
      @brief WIR_Compare_HCFGNode is a comparator class that is used to sort
             sets of pointers to hierarchical control flow graph nodes.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    struct WIR_Compare_HCFGNode
    {
      bool operator()( const WIR_ControlTreeNode *lhs,
                       const WIR_ControlTreeNode *rhs ) const;
    };

    /*!
      @brief WIR_HCFGNodeSet represents sets of pointers to hierarchical control
             tree nodes.
    */
    using WIR_HCFGNodeSet =
      std::set<WIR_ControlTreeNode *, WIR_Compare_HCFGNode>;


    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor for a given %WIR function.

      @param[in] f A reference to a %WIR function used to construct the control
                   flow graph.
      @param[in] verbosity A Boolean defaulting to false that denotes whether
                           verbose messages shall be dumped.
      @param[in] keepTmpFiles A Boolean defaulting to false that denotes whether
                              temporary files shall be kept.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_HierarchicalCFG( WIR_Function &, bool = false, bool = false );

    /*!
      @brief Destructor.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ~WIR_HierarchicalCFG( void );


    //
    // General graph properties.
    //

    /*!
      @brief dfs traverses the CFG in depth-first order and builds the postorder
             traversal and the set of start nodes.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void dfs( void );

    /*!
      @brief dfsDryRun traverses the CFG in depth-first order and prints the
             visited nodes and edges for debugging purposes.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void dfsDryRun( void );

    /*!
      @brief getPostOrder returns the list of all hierarchical control flow
             graph nodes in their DFS postorder.

      @return A const reference to list mPostOrder.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const std::list<WIR_ControlTreeNode *> &getPostOrder( void ) const;

    /*!
      @brief getStartNodes returns all start nodes of the current CFG.

      @return A const reference to the set of CFG start nodes.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const WIR_HCFGNodeSet &getStartNodes( void ) const;

    /*!
      @brief getNumberOfBackEdges returns the number of back edges in the
             current CFG.

      @return An unsigned integer denoting the number of CFG back edges.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    unsigned int getNumberOfBackEdges( void ) const;

    /*!
      @brief visualize dumps the current hierarchical control flow graph into a
             DOT file and invokes xdot on it.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void visualize( void ) const;


  protected:

    //
    // Local type definitions.
    //

    /*!
      @brief For each edge of the hierarchical control flow graph, this struct
             stores its relevant properties.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    struct edgeInfo
    {
      //! isBackEdge denotes whether a CFG edge is a back edge or not.
      bool isBackEdge;

      //! isBreakEdge denotes whether a CFG edge breaks out of a loop or not.
      bool isBreakEdge;

      /*!
        @brief isLoopExitEdge denotes whether a CFG edge is a regular exit of a
               for- or while-do loop.
      */
      bool isLoopExitEdge;

      //! Default constructor initializing all struct members with false.
      edgeInfo() : isBackEdge { false },
                   isBreakEdge { false },
                   isLoopExitEdge { false }
      {};
    };

    /*!
      @brief CGraph represents the internal boost structure of the hierarchical
             control flow graph.
    */
    using CGraph = WIR_DirGraph<WIR_ControlTreeNode *, edgeInfo>;

    //! CGraphVertex represents a hierarchical control flow graph vertex.
    using CGraphVertex = WIR_DirGraphVertex<WIR_ControlTreeNode *, edgeInfo>;

    //! CGraphEdge represents a control flow graph edge.
    using CGraphEdge = WIR_DirGraphEdge<WIR_ControlTreeNode *, edgeInfo>;


    //
    // Attributes.
    //

    /*!
      @brief mFunction stores a reference to the %WIR function to be modeled as
             hierarchical control flow graph.
    */
    WIR_Function &mFunction;

    /*!
      @brief mCGraph represents the internal boost graph data structure for the
             hierarchical control flow graph.
    */
    CGraph mCGraph;

    /*!
      @brief mNodeByID maps IDs of %WIR basic blocks and of control tree nodes
             to its representing Boost control flow graph node.
    */
    std::map<WIR_id_t, CGraphVertex> mNodeByID;

    //! mStartNodes contains all start nodes of the CFG.
    WIR_HCFGNodeSet mStartNodes;

    //! mBackEdges contains all identified back edges in the CFG.
    std::set<CGraphEdge> mBackEdges;

    /*!
      @brief mPostOrder lists all nodes of a hierarchical control flow graph in
             their DFS postorder.
    */
    std::list<WIR_ControlTreeNode *> mPostOrder;

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
    WIR_HierarchicalCFG( void ) = delete;

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


    //
    // Graph visualization.
    //

    /*!
      @brief visualizeNodes dumps the control flow graph nodes into a given DOT
             file.

      @param[in] dotFile A reference to a DOT file opened for writing.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void visualizeNodes( std::fstream & ) const;

    /*!
      @brief visualizeEdges dumps the control flow graph edges into a given DOT
             file.

      @param[in] dotFile A reference to a DOT file opened for writing.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void visualizeEdges( std::fstream & ) const;


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
          @param[in,out] p A reference to a list of CFG nodes in their DFS
                           postorder.
          @param[in,out] b A reference to a set containing all back edges of a
                           CFG after DFS graph traversal.
          @param[in] dryRun A Boolen denoting whether a dry-run DFS traversal
                            shall be performed or not.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        CFGVisitor( WIR_HCFGNodeSet &s, std::list<WIR_ControlTreeNode *> &p,
                    std::set<CGraphEdge> &b, bool = false );

        /*!
          @brief start_vertex marks an identified start node of a CFG.

          @param[in] v The start node.
          @param[in] g A const reference to the CFG.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        void start_vertex( CGraphVertex, const CGraph & ) const;

        /*!
          @brief finish_vertex finishes a visited CFG node.

          @param[in] v The currently visited CFG node.
          @param[in] g A const reference to the CFG.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        void finish_vertex( CGraphVertex, const CGraph & ) const;

        /*!
          @brief back_edge marks an identified back edge.

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
        WIR_HCFGNodeSet &mStartNodes;

        /*!
          @brief mPostOrder refers to a list of CFG nodes in their DFS
                 postorder.
        */
        std::list<WIR_ControlTreeNode *> &mPostOrder;

        /*!
          @brief mBackEdges refers to a set containing all identified back edges
                 in a CFG.
        */
        std::set<CGraphEdge> &mBackEdges;

        bool mDryRun;

    };

};

}       // namespace WIR

#endif  // _WIR_HIERARCHICALCFG_H
