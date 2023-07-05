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
  @file wirstructuralanalysis.h
  @brief This file provides the interface of the %WIR structural control flow
         analysis.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_STRUCTURALANALYSIS_H
#define _WIR_STRUCTURALANALYSIS_H


//
// Include section
//

// Include standard headers
#include <map>

// Include WIR headers
#include <wir/wirtypes.h>
#include <analyses/generic/wiranalysis.h>
#include <analyses/structuralcontrolflow/wircontroltreenode.h>
#include <analyses/structuralcontrolflow/wirhierarchicalcfg.h>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_BasicBlock;
class WIR_ControlTreeNode;
class WIR_Function;


/*!
  @brief Class WIR_StructuralAnalysis is the %WIR structural control flow
         analysis according to Steven S. Muchnick, Advanced Compiler Design and
         Implementation, chapter 7.7, page 202ff.

  Analysis results are stored in WIR_ControlTree containers that are attached to
  %WIR basic blocks.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_StructuralAnalysis : public WIR_Analysis,
                               public WIR_HierarchicalCFG
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor for function-level analysis.

      @param[in] f A reference to a WIR_Function to be analyzed.
      @param[in] verbosity A Boolean defaulting to false that denotes whether
                           verbose messages shall be dumped.
      @param[in] keepTmpFiles A Boolean defaulting to false that denotes whether
                              temporary files shall be kept.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_StructuralAnalysis( WIR_Function &, bool = false, bool = false );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_StructuralAnalysis( void );


  protected:

    /*!
      @brief runAnalysis performs structural control flow analysis of the given
             function.

      @param[in] f A reference to a WIR_Function to be analyzed.

      This method implements procedure Structural_Analysis() from S. S.
      Muchnick, page 206.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void runAnalysis( WIR_Function & );


  private:

    //
    // Private methods.
    //

    /*!
      @brief init initializes internal data structures by collecting information
             about predecessor/successor relations between basic blocks.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void init( void );

    /*!
      @brief findAcyclicRegion determines whether the specified CFG node forms
             an acyclic control region.

      @param[in] node A const reference to a CFG node to be examined.
      @param[out] nSet A reference to a set of hierarchical CFG nodes identified
                       in an acyclic control region.
      @return A pointer to a newly created abstract control region if an acyclic
              control region was detected, or nullptr otherwise.

      This method implements procedure Acyclic_Region_Type() from S. S.
      Muchnick, page 208.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_ControlTreeNode *findAcyclicRegion( const WIR_ControlTreeNode &,
                                            WIR_HierarchicalCFG::WIR_HCFGNodeSet & );

    /*!
      @brief findCyclicRegion determines whether the specified CFG node forms a
             cyclic control region.

      @param[in] node A const reference to a CFG node to be examined.
      @param[in,out] nSet A reference to a set of hierarchical CFG nodes
                          contained in a cyclic control region.
      @return A pointer to a newly created abstract control region if a cyclic
              control region was detected, or nullptr otherwise.

      This method implements procedure Cyclic_Region_Type() from S. S.
      Muchnick, page 208ff.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_ControlTreeNode *findCyclicRegion( const WIR_ControlTreeNode &,
                                           WIR_HierarchicalCFG::WIR_HCFGNodeSet & );

    /*!
      @brief checkBlock checks whether the specified CFG node forms a block
             region (S. S. Muchnick, Fig. 7.35, page 203).

      @param[in] node A const reference to a CFG node to be examined.
      @param[in,out] nSet A reference to a set of hierarchical CFG nodes
                          identified in a block control region.
      @return A pointer to a newly created block region node if such a region
              was detected, or nullptr otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_BlockTreeNode *checkBlock( const WIR_ControlTreeNode &,
                                   WIR_HierarchicalCFG::WIR_HCFGNodeSet & ) const;

    /*!
      @brief checkIfThenElse checks whether the specified CFG node forms an
             if-then-else region (S. S. Muchnick, Fig. 7.35, page 203).

      @param[in] node A const reference to a CFG node to be examined.
      @param[in,out] nSet A reference to a set of hierarchical CFG nodes
                          identified in an if-then-else control region.
      @return A pointer to a newly created if-then-else region node if such a
              region was detected, or nullptr otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_IfThenElseTreeNode *checkIfThenElse( const WIR_ControlTreeNode &,
                                             WIR_HierarchicalCFG::WIR_HCFGNodeSet & ) const;

    /*!
      @brief checkIfElseSelfLoop checks whether the specified CFG node forms an
             if-then-else region simultaneously being a self-loop.

      @param[in] node A const reference to a CFG node to be examined.
      @param[in,out] nSet A reference to a set of hierarchical CFG nodes
                          identified in an if-then-else control region.
      @return A pointer to a newly created self-loop region node if such a
              region was detected, or nullptr otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_SelfLoopTreeNode *checkIfElseSelfLoop( const WIR_ControlTreeNode &,
                                               WIR_HierarchicalCFG::WIR_HCFGNodeSet & );

    /*!
      @brief checkIfThen checks whether the specified CFG node forms an if-then
             region (S. S. Muchnick, Fig. 7.35, page 203).

      @param[in] node A const reference to a CFG node to be examined.
      @param[in,out] nSet A reference to a set of hierarchical CFG nodes
                          identified in an if-then control region.
      @return A pointer to a newly created if-then region node if such a region
              was detected, or nullptr otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_IfThenTreeNode *checkIfThen( const WIR_ControlTreeNode &,
                                     WIR_HierarchicalCFG::WIR_HCFGNodeSet & ) const;

    /*!
      @brief checkSwitchCase checks whether the specified CFG node forms a
             switch-case region (S. S. Muchnick, Fig. 7.35, page 203).

      @param[in] node A const reference to a CFG node to be examined.
      @param[in,out] nSet A reference to a set of hierarchical CFG nodes
                          identified in a switch-case control region.
      @return A pointer to a newly created switch-case region node if such a
              region was detected, or nullptr otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_SwitchCaseTreeNode *checkSwitchCase( const WIR_ControlTreeNode &,
                                             WIR_HierarchicalCFG::WIR_HCFGNodeSet & ) const;

    /*!
      @brief checkProper checks whether the specified CFG node forms a proper
             acyclic region (S. S. Muchnick, Fig. 7.37, page 204).

      @param[in] node A const reference to a CFG node to be examined.
      @param[in,out] nSet A reference to a set of hierarchical CFG nodes
                          identified in a proper acyclic control region.
      @return A pointer to a newly created proper region node if such a region
              was detected, or nullptr otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_ProperTreeNode *checkProper( const WIR_ControlTreeNode &,
                                     WIR_HierarchicalCFG::WIR_HCFGNodeSet & );

    /*!
      @brief checkSelfLoop checks whether the specified CFG node forms a self-
             loop (S. S. Muchnick, Fig. 7.36, page 203).

      @param[in] node A const reference to a CFG node to be examined.
      @param[in] nSet A reference to a set of hierarchical CFG nodes contained
                      in a cyclic control region.
      @return A pointer to a newly created self-loop region node if such a
              region was detected, or nullptr otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_SelfLoopTreeNode *checkSelfLoop( const WIR_ControlTreeNode &,
                                         WIR_HierarchicalCFG::WIR_HCFGNodeSet & ) const;

    /*!
      @brief checkImproper checks whether the specified CFG node forms an
             improper cyclic region (S. S. Muchnick, Fig. 7.36, page 203).

      @param[in] node A const reference to a CFG node to be examined.
      @param[in] nSet A reference to a set of hierarchical CFG nodes contained
                      in a cyclic improper region.
      @return A pointer to a newly created improper region node if such a
              region was detected, or nullptr otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_ImproperTreeNode *checkImproper( const WIR_ControlTreeNode &,
                                         WIR_HierarchicalCFG::WIR_HCFGNodeSet & );

    /*!
      @brief checkWhileLoop checks whether the specified CFG node forms a
             regular for- or while-do loop (S. S. Muchnick, Fig. 7.36, page
             203).

      @param[in] node A const reference to a CFG node to be examined.
      @param[in] nSet A reference to a set of hierarchical CFG nodes contained
                      in a for-/while-do loop region.
      @return A pointer to a newly created while loop region node if such a
              region was detected, or nullptr otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_WhileLoopTreeNode *checkWhileLoop( const WIR_ControlTreeNode &,
                                           WIR_HierarchicalCFG::WIR_HCFGNodeSet & );

    /*!
      @brief checkNaturalLoop checks whether the specified CFG node forms a
             natural do-while loop (S. S. Muchnick, Fig. 7.36, page 203).

      @param[in] node A const reference to a CFG node to be examined.
      @param[in] nSet A reference to a set of hierarchical CFG nodes contained
                      in a natural do-while loop region.
      @return A pointer to a newly created natural loop region node if such a
              region was detected, or nullptr otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_NaturalLoopTreeNode *checkNaturalLoop( const WIR_ControlTreeNode &,
                                               WIR_HierarchicalCFG::WIR_HCFGNodeSet & );

    /*!
      @brief checkRecursionLoop checks whether the specified CFG node forms a
             cyclic structure stemming from tail recursion elimination.

      @param[in] node A const reference to a CFG node to be examined.
      @param[in] nSet A reference to a set of hierarchical CFG nodes contained
                      in a recursion loop region.
      @return A pointer to a newly created natural loop region node if such a
              region was detected, or nullptr otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_NaturalLoopTreeNode *checkRecursionLoop( const WIR_ControlTreeNode &,
                                                 WIR_HierarchicalCFG::WIR_HCFGNodeSet & );

    /*!
      @brief testProper tests recursively whether the specified CFG node forms a
             proper acyclic region (S. S. Muchnick, Fig. 7.37, page 204).

      @param[in] node A const reference to a CFG node to be examined.
      @param[in,out] nSet A reference to a minimal set of hierarchical CFG nodes
                          identified in a proper acyclic control region.
      @return true iff the test for proper regions was successful, false
              otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool testProper( const WIR_ControlTreeNode &,
                     WIR_HierarchicalCFG::WIR_HCFGNodeSet & ) const;

    /*!
      @brief pathBack checks whether there is a node k such that there is a
             (possibly empty) path from m to k that does not pass through n and
             an edge k -> n that is a back edge.

      @param[in] m A Boost graph node to be checked.
      @param[in] n A Boost graph node being potential starting point of a cyclic
                   subgraph.
      @param[in] b A Boost graph back edge k -> n.
      @return true if a node k with the above properties exists, false
              otherwise.

      This method implements procedure Path_Back(m, n) from S. S. Muchnick, page
      207.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool pathBack( CGraphVertex, CGraphVertex, CGraphEdge ) const;

    /*!
      @brief minimizeImproper minimizes an improper interval containing a given
             node.

      @param[in] node A const reference to a CFG node to be examined.
      @param[in,out] nSet A reference to a set of hierarchical CFG nodes
                          contained in a cyclic improper region.
      @return A reference to the control tree node being the single entry into
              the improper interval.

      This method implements procedure Minimize_Improper(m, n) from S. S.
      Muchnick, Fig. 7.45, page 207.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_ControlTreeNode &minimizeImproper( const WIR_ControlTreeNode &,
                                           WIR_HierarchicalCFG::WIR_HCFGNodeSet & );

    /*!
      @brief reduce replaces the given node set by a new abstract control
             region.

      @param[in] node A reference to the newly created abstract control region.
      @param[in,out] nodeSet A reference to a set of CFG nodes to be reduced.
      @return The ID of the new hierarchical control region created for input
              node.

      This method implements procedure Reduce() from S. S. Muchnick, page 209
      and procedure Replace() from S. S. Muchnick, page 210.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_id_t reduce( WIR_ControlTreeNode &,
                     WIR_HierarchicalCFG::WIR_HCFGNodeSet & );

    /*!
      @brief createContainers takes the leaf nodes of the created control trees
             and attaches them persistently to the current %WIR function's basic
             blocks using control tree containers.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void createContainers( void );


    //
    // Attributes.
    //

    /*!
      @brief mIDomAnalysisDone stores whether an immediate domination analysis
             was done for the current function or not.
    */
    bool mIDomAnalysisDone;

    /*!
      @brief mCheckProperRegions stores whether checks for proper acyclic
             regions shall be performed or not.
    */
    bool mCheckProperRegions;

    /*!
      @brief mIgnoreBreakEdges stores whether checks for acyclic shall ignore
             break edges or not.
    */
    bool mIgnoreBreakEdges;

};

}       // namespace WIR

#endif  // _WIR_STRUCTURALANALYSIS_H
