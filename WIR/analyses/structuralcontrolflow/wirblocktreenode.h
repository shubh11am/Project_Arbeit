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
  @file wirblocktreenode.h
  @brief This file provides the basic properties of block tree nodes.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_BLOCKTREENODE_H
#define _WIR_BLOCKTREENODE_H


//
// Include section
//

// Include standard headers
#include <fstream>
#include <functional>
#include <list>

// Include WIR headers
#include <analyses/structuralcontrolflow/wircontroltreenode.h>


//
// Header section
//

namespace WIR {

/*!
  @brief Class WIR_BlockTreeNode represents block regions of the %WIR control
         tree.

  - Cyclic: No
  - Number of Entries: 1
  - Number of Exits: 1

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_BlockTreeNode : public WIR_ControlTreeNode
{

  public:

    /*!
      @brief Default constructor.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_BlockTreeNode( void );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_BlockTreeNode( void );


    //
    // Type handling.
    //

    /*!
      @brief getType returns the type of a block region node.

      @return WIR_CTNodeType::block

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_CTNodeType getType( void ) const;

    /*!
      @brief isCyclic returns whether a block region node is cyclic or not.

      @return Always false.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool isCyclic( void ) const;


    //
    // Control Tree hierarchy.
    //

    /*!
      @brief getEntry returns a block region's unique entry child node.

      @return A const reference to the first entry in the block list.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual const WIR_ControlTreeNode &getEntry( void ) const;


    //
    // Handling of block nodes.
    //

    /*!
      @brief getBlockList returns the list of stored child nodes in their
             sequential order.

      @return A const reference to the list mBlockList.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const std::list<std::reference_wrapper<WIR_ControlTreeNode>> &getBlockList( void ) const;


  protected:

    //
    // Visualization.
    //

    /*!
      @brief visualize dumps a block region node into a given DOT file.

      @param[in,out] dotFile A reference to a DOT file opened for writing.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void visualize( std::fstream & ) const;


  private:

    friend class WIR_StructuralAnalysis;

    //
    // Handling of block nodes.
    //

    /*!
      @brief pushBackBlockNode adds a new node to a block region at the end of
             list mBlockList.

      @param[in] n A reference to the control tree node to be inserted into a
                   block.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void pushBackBlockNode( WIR_ControlTreeNode & );


    //
    // Attributes.
    //

    /*!
      @brief mBlockList holds wrapped references to all stored child nodes in
             their sequential order.
    */
    std::list<std::reference_wrapper<WIR_ControlTreeNode>> mBlockList;

};

}       // namespace WIR

#endif  // _WIR_BLOCKTREENODE_H
