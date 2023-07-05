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
  @file wirswitchcasetreenode.h
  @brief This file provides the basic properties of switch-case tree nodes.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_SWITCHCASETREENODE_H
#define _WIR_SWITCHCASETREENODE_H


//
// Include section
//

// Include standard headers
#include <fstream>
#include <functional>
#include <list>
#include <set>

// Include WIR headers
#include <wir/wirtypes.h>
#include <analyses/structuralcontrolflow/wircontroltreenode.h>


//
// Header section
//

namespace WIR {

/*!
  @brief Class WIR_SwitchCaseTreeNode represents switch-case regions of the %WIR
         control tree.

  - Cyclic: No
  - Number of Entries: 1
  - Number of Exits: > 1

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_SwitchCaseTreeNode : public WIR_ControlTreeNode
{

  public:

    /*!
      @brief Default constructor.

      @param[in] c A const reference to a switch-case's condition node.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_SwitchCaseTreeNode( const WIR_ControlTreeNode & );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_SwitchCaseTreeNode( void );


    //
    // Type handling.
    //

    /*!
      @brief getType returns the type of a switch-case region node.

      @return WIR_CTNodeType::switchcase

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_CTNodeType getType( void ) const;

    /*!
      @brief isCyclic returns whether a switch-case region is cyclic or not.

      @return Always false.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool isCyclic( void ) const;


    //
    // Control Tree hierarchy.
    //

    /*!
      @brief getEntry returns a switch-case region's unique entry child node.

      @return A const reference to a switch-case's condition node.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual const WIR_ControlTreeNode &getEntry( void ) const;


    //
    // Handling of case nodes.
    //

    /*!
      @brief getCondition returns an switch-case's condition node.

      @return A const reference to mCondition.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const WIR_ControlTreeNode& getCondition( void ) const;

    /*!
      @brief getCases returns the list of stored case nodes in their sequential
             order.

      @return A const reference to the list mCaseList.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const std::list<std::reference_wrapper<WIR_ControlTreeNode>> &getCases( void ) const;

    /*!
      @brief getFallthroughCases returns a list of fall-through case nodes in
             their sequential order.

      @return A list containing iterators pointing into list mCaseList at the
              positions of fall-through cases.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const std::list<std::list<std::reference_wrapper<WIR_ControlTreeNode>>::const_iterator> getFallthroughCases( void ) const;


  protected:

    //
    // Visualization.
    //

    /*!
      @brief visualize dumps a switch-case region node into a given DOT file.

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
    WIR_SwitchCaseTreeNode( void ) = delete;


    //
    // Handling of case nodes.
    //

    /*!
      @brief pushBackCaseNode adds a new node to a switch-case region at the end
             of list mCaseList.

      @param[in] n A reference to the control tree node to be inserted into a
                   switch-case.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void pushBackCaseNode( WIR_ControlTreeNode & );

    /*!
      @brief pushBackFallthroughCaseNode adds a new node to a switch-case region
             at the end of list mCaseList and marks it as fall-through.

      @param[in] n A reference to the control tree node to be inserted into a
                   switch-case.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void pushBackFallthroughCaseNode( WIR_ControlTreeNode & );


    //
    // Attributes.
    //

    //! mCondition refers to a switch-case's condition node.
    const WIR_ControlTreeNode &mCondition;

    /*!
      @brief mCaseList holds wrapped references to all stored case nodes in
             their sequential order.
    */
    std::list<std::reference_wrapper<WIR_ControlTreeNode>> mCaseList;

    //! mFallthroughCases holds the IDs of all fall-through cases.
    std::set<WIR_id_t> mFallthroughCases;

};

}       // namespace WIR

#endif  // _WIR_SWITCHCASETREENODE_H
