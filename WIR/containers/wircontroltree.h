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
  @file wircontroltree.h
  @brief This file provides the interface of a %WIR container representing
         control trees.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_CONTROLTREE_H
#define _WIR_CONTROLTREE_H


//
// Include section
//

// Include standard headers
#include <list>

// Include WIR headers
#include <wir/wircontainer.h>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_BasicBlockTreeNode;
class WIR_ControlTreeNode;


/*!
  @brief Class WIR_ControlTree models leaf or root nodes of control trees.

  WIR_ControlTree containers are created during structural control flow
  analysis, and they can be attached to %WIR basic blocks and to %WIR functions.
  If attached to a basic block, the container refers to a leaf node of a control
  tree which actually represents the given basic block. This scenario is the
  primary use case for control tree containers and should only be actively used.

  The scenario where a control tree container is associated with a %WIR function
  solely serves for memory management. At function level, the container stores
  the roots of the function's control trees. Upon destruction of this function-
  level container, the entire memory for all associated hierarchical control
  trees will be released.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_ControlTree : public WIR_Container<WIR_ControlTree>
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor for the normal use case where a container
             refers to a leaf node of a control tree, i.e., to a single basic
             block.

      @param[in] b A reference to a basic block-level control tree node.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_ControlTree( WIR_BasicBlockTreeNode & );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_ControlTree( void );

    /*!
      @brief isUnique returns whether control tree containers are unique, i.e.,
             whether at most one instance of this container type can be attached
             to a %WIR class.

      @return Always true, control tree containers are unique.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool isUnique( void ) const;


    //
    // Basic block handling.
    //

    /*!
      @brief getBasicBlockTreeNode returns the basic block tree node associated
             with a control tree container.

      @return A reference to the tree node to that mLeafNode points.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_BasicBlockTreeNode &getBasicBlockTreeNode( void ) const;


  private:

    friend class WIR_StructuralAnalysis;

    /*!
      @brief Default constructor for the internal use case where a container
             refers to a root node of a control tree.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_ControlTree( void );


    //
    // Handling of control tree root nodes.
    //

    /*!
      @brief pushBackRootNode adds a new control tree root to list mRootNodes.

      @param[in] n A reference to the control tree root node to be added.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void pushBackRootNode( WIR_ControlTreeNode & );

    //! mLeafNode points to a control tree's leaf node.
    WIR_BasicBlockTreeNode *mLeafNode;

    //! mRootNodes stores root nodes of control trees.
    std::list<WIR_ControlTreeNode *> mRootNodes;

};

}       // namespace WIR

#endif  // _WIR_CONTROLTREE_H
