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
  @file wirnaturallooptreenode.h
  @brief This file provides the basic properties of natural do-while loop tree
         nodes.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_NATURALLOOPTREENODE_H
#define _WIR_NATURALLOOPTREENODE_H


//
// Include section
//

// Include standard headers
#include <fstream>

// Include WIR headers
#include <wir/wirtypes.h>
#include <analyses/structuralcontrolflow/wircontroltreenode.h>


//
// Header section
//

namespace WIR {

/*!
  @brief Class WIR_NaturalLoopTreeNode represents natural do-while loop regions
         of the %WIR control tree.

  - Cyclic: Yes
  - Number of Entries: 1
  - Number of Exits: >= 1

  C.f. S. S. Muchnick, Fig. 7.36, page 203.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_NaturalLoopTreeNode : public WIR_ControlTreeNode
{

  public:

    /*!
      @brief Default constructor.

      @param[in] n A const reference to a while-do loop's entry node.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_NaturalLoopTreeNode( const WIR_ControlTreeNode & );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_NaturalLoopTreeNode( void );


    //
    // Type handling.
    //

    /*!
      @brief getType returns the type of a do-while loop region node.

      @return WIR_CTNodeType::naturalloop

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_CTNodeType getType( void ) const;

    /*!
      @brief isCyclic returns whether a do-while loop region is cyclic or not.

      @return Always true.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool isCyclic( void ) const;


    //
    // Control Tree hierarchy.
    //

    /*!
      @brief getEntry returns a do-while loop's unique entry child node.

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
      @brief visualize dumps a do-while loop region node into a given DOT file.

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
    WIR_NaturalLoopTreeNode( void ) = delete;


    //
    // Handling of nodes.
    //

    /*!
      @brief insertNode adds a new node to a do-while loop region.

      @param[in] n A reference to the control tree node to be inserted into a
                   while-do loop region.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertNode( WIR_ControlTreeNode & );

    /*!
      @brief insertEdge adds a new edge between two nodes to a do-while loop
             region.

      @param[in] s A reference to the control tree node being the edge's source.
      @param[in] t A reference to the control tree node being the edge's
                   target.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertEdge( const WIR_ControlTreeNode &, const WIR_ControlTreeNode & );

    /*!
      @brief insertBackEdge adds a new back-edge between two nodes to a do-while
             loop region.

      @param[in] s A reference to the control tree node being the edge's source.
      @param[in] t A reference to the control tree node being the edge's
                   target.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertBackEdge( const WIR_ControlTreeNode &,
                         const WIR_ControlTreeNode & );


    //
    // Attributes.
    //

    //! mEntry refers to a do-while loop region's entry node.
    const WIR_ControlTreeNode &mEntry;

    //! mNodes holds wrapped references to all stored nodes.
    WIR_ControlTreeNodeSet mNodes;

    //! mEdges holds pairs of nodes denoting a do-while loop region's edges.
    WIR_ControlTreeEdgeSet mEdges;

};

}       // namespace WIR

#endif  // _WIR_NATURALLOOPTREENODE_H
