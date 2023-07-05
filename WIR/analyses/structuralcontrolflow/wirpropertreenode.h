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
  @file wirpropertreenode.h
  @brief This file provides the basic properties of proper acyclic tree nodes.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_PROPERTREENODE_H
#define _WIR_PROPERTREENODE_H


//
// Include section
//

// Include standard headers
#include <fstream>
#include <functional>
#include <set>

// Include WIR headers
#include <wir/wirtypes.h>
#include <analyses/structuralcontrolflow/wircontroltreenode.h>


//
// Header section
//

namespace WIR {

/*!
  @brief Class WIR_ProperTreeNode represents proper acyclic regions of the %WIR
         control tree.

  - Cyclic: No
  - Number of Entries: 1
  - Number of Exits: >= 1

  Proper regions are connected subgraphs of the control flow graph with
  - exactly one unique entry node, and
  - at least two different nodes, and
  - without any cycles.

  C.f. S. S. Muchnick, Fig. 7.37, page 204 for an example.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_ProperTreeNode : public WIR_ControlTreeNode
{

  public:

    /*!
      @brief Default constructor.

      @param[in] n A const reference to a proper region's entry node.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_ProperTreeNode( const WIR_ControlTreeNode & );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_ProperTreeNode( void );


    //
    // Type handling.
    //

    /*!
      @brief getType returns the type of a proper region node.

      @return WIR_CTNodeType::proper

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_CTNodeType getType( void ) const;

    /*!
      @brief isCyclic returns whether a proper region is cyclic or not.

      @return Always false.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool isCyclic( void ) const;


    //
    // Control Tree hierarchy.
    //

    /*!
      @brief getEntry returns a proper region's unique entry child node.

      @return A const reference to mEntry.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual const WIR_ControlTreeNode &getEntry( void ) const;


    //
    // Handling of nodes.
    //

    /*!
      @brief getNodes returns the set of all stored nodes.

      @return A const reference to the set mNodes.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const WIR_ControlTreeNodeSet &getNodes( void ) const;


  protected:

    //
    // Visualization.
    //

    /*!
      @brief visualize dumps a proper region node into a given DOT file.

      @param[in,out] dotFile A reference to a DOT file opened for writing.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void visualize( std::fstream & ) const;


  private:

    friend class WIR_StructuralAnalysis;

    /*!
      @brief No standard construction allowed, users must use the public
             constructor above instead.
    */
    WIR_ProperTreeNode( void ) = delete;


    //
    // Handling of nodes.
    //

    /*!
      @brief insertNode adds a new node to a proper region.

      @param[in] n A reference to the control tree node to be inserted into a
                   proper region.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertNode( WIR_ControlTreeNode & );

    /*!
      @brief insertEdge adds a new edge between two nodes to a proper region.

      @param[in] s A reference to the control tree node being the edge's source.
      @param[in] t A reference to the control tree node being the edge's
                   target.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertEdge( const WIR_ControlTreeNode &, const WIR_ControlTreeNode & );


    //
    // Attributes.
    //

    //! mEntry refers to a proper region's entry node.
    const WIR_ControlTreeNode &mEntry;

    //! mNodes holds wrapped references to all stored nodes.
    WIR_ControlTreeNodeSet mNodes;

    //! mEdges holds pairs of nodes denoting a proper region's edges.
    WIR_ControlTreeEdgeSet mEdges;

};

}       // namespace WIR

#endif  // _WIR_PROPERTREENODE_H
