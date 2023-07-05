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
  @file wircontroltreenode.h
  @brief This file provides the basic properties of control tree nodes.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_CONTROLTREENODE_H
#define _WIR_CONTROLTREENODE_H


//
// Include section
//

// Include standard headers
#include <fstream>
#include <functional>
#include <list>
#include <memory>
#include <set>
#include <string>
#include <utility>

// Include WIR headers
#include <wir/wirtypes.h>
#include <wir/API/wiridapi.h>


//
// Header section
//

namespace WIR {

/*!
  @brief This enum represents different types of %WIR control tree nodes.

  See Steven S. Muchnik, Advanced Compiler Design & Implementation, Morgan
  Kaufmann Publishers, 2011 (Chapter 7.7, page 203ff.) for a description of
  the individual types.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
enum class WIR_CTNodeType : char
{
  //! A single basic block.
  bb,

  //! Block schema.
  block,

  //! If-then.
  ifthen,

  //! If-then-else.
  ifthenelse,

  //! Switch-case.
  switchcase,

  //! Proper interval.
  proper,

  //! Self loop.
  selfloop,

  //! Regular for- or while-do loops.
  whileloop,

  //! Natural do-while loop.
  naturalloop,

  //! Improper interval.
  improper
};


/*!
  @brief Class WIR_ControlTreeNode represents nodes of the %WIR control tree.

  - Cyclic: Depends on the nature of derived classes
  - Number of Entries: Always 1
  - Number of Exits: Depends on the nature of derived classes

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_ControlTreeNode : public WIR_ID_API
{

  public:

    //
    // Local type definitions.
    //

    //! WIR_ControlTreeEdgeSet represents sets of control tree edges.
    using WIR_ControlTreeEdgeSet =
      std::set<std::pair<WIR_ControlTreeNode *, WIR_ControlTreeNode *>>;


    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_ControlTreeNode( void );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_ControlTreeNode( void );


    //
    // Type handling.
    //

    /*!
      @brief getType returns the type of a %WIR control tree node.

      @return The control tree node's type.

      Since the tree nodes type depend on the actual implementation of nodes in
      derived classes, this method is purely virtual.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_CTNodeType getType( void ) const = 0;

    /*!
      @brief isCyclic returns whether a %WIR control tree node is cyclic or not.

      @return true if a node is cyclic, false otherwise.

      Since the property whether a node is cyclic or not depends on a node's
      actual structure, this method is purely virtual.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool isCyclic( void ) const = 0;

    /*!
      @brief isAcyclic returns whether a %WIR control tree node is acyclic or
             not.

      @return true if a node is acyclic, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isAcyclic( void ) const;


    //
    // Control Tree hierarchy.
    //

    /*!
      @brief getEntry returns a control tree node's unique entry child node.

      @return A const reference to the entry child node, or a reference to
              itself if it has no childs.

      Since the property which of the childs is the entry node depends on a
      node's actual structure, this method is purely virtual.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual const WIR_ControlTreeNode &getEntry( void ) const = 0;

    /*!
      @brief isEntry returns whether a control tree node is the entry node of a
             potential parent or not.

      @return true if the node is an entry, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isEntry( void ) const;

    /*!
      @brief getChilds returns the set of all control tree child nodes.

      @return A const reference to the set mChildReferences.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const WIR_ControlTreeNodeSet &getChilds( void ) const;

    /*!
      @brief begin returns an iterator to the first child node.

      @return A const iterator pointing to the first child.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_ControlTreeNodeSet::const_iterator begin( void ) const;

    /*!
      @brief end returns an iterator to the end of the child node set.

      @return A const iterator pointing to the position after the last child
              node.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_ControlTreeNodeSet::const_iterator end( void ) const;

    /*!
      @brief getParent returns a control tree node's parent node.

      @return A reference to the parent node, or a reference to itself if it has
              no parent.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_ControlTreeNode &getParent( void ) const;

    /*!
      @brief getRoot returns the root of the entire control tree that this node
             is part of.

      @return A reference to the tree's root node, or a reference to itself if
              the current node is the root itself.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_ControlTreeNode &getRoot( void ) const;


    //
    // Basic block handling.
    //

    /*!
      @brief getBasicBlocks determines the set of actual %WIR basic blocks
             hierarchically included in a control tree node.

      @return A set of all basic blocks included in a control tree node.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_BasicBlockSet getBasicBlocks( void ) const;


    //
    // Handling of edges.
    //

    /*!
      @brief getBackEdges returns the set of back-edges of a region.

      @return A const reference to the set mBackEdges.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const WIR_ControlTreeEdgeSet &getBackEdges( void ) const;


    //
    // Visualization.
    //

    /*!
      @brief visualize dumps a control flow graph node into a given DOT file.

      @param[in,out] dotFile A reference to a DOT file opened for writing.

      Since a node's visualization depends on the node's actual type, this
      method is purely virtual.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void visualize( std::fstream &dotFile ) const = 0;


  protected:

    /*!
      @brief buildNodeName returns a string denoting the specified node's name.

      @return A string containing the node's name as it will be displayed in
              dot.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::string buildNodeName( void ) const;

    //! mBackEdges holds pairs of nodes denoting a cyclic region's back-edges.
    WIR_ControlTreeEdgeSet mBackEdges;


  private:

    friend class WIR_StructuralAnalysis;
    friend class WIR_LoopInvariantCM;


    //
    // Control Tree hierarchy.
    //

    /*!
      @brief insertChild inserts a child into a control tree node's hierarchy.

      @param[in] c A pointer to the child node to be inserted.

      @note The control tree node into which a child is inserted takes over
      control of the specified pointer. In particular, the parent node will
      automatically destroy a child node. Manual destruction of child nodes
      which are inserted into a hierarchical control tree is thus strongly
      discouraged!

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertChild( WIR_ControlTreeNode * );


    //
    // Attributes.
    //

    /*!
      @brief mParent points to the parent control tree node that this one is
             child of.
    */
    WIR_ControlTreeNode *mParent;

    /*!
      @brief mChildPointers holds managed pointers to all stored control tree
             child nodes.
    */
    std::list<std::unique_ptr<WIR_ControlTreeNode>> mChildPointers;

    /*!
      @brief mChildReferences holds (wrapped) references to all stored control
             tree child nodes.
    */
    WIR_ControlTreeNodeSet mChildReferences;

};

}       // namespace WIR

#endif  // _WIR_CONTROLTREENODE_H
