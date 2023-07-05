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
  @file wircfgnodeproperty.h
  @brief This file provides the basic properties of control flow graph nodes.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_CFG_NODE_PROPERTY_H
#define _WIR_CFG_NODE_PROPERTY_H


//
// Include section
//

// Include standard headers
#include <functional>

// Include boost headers
#include <boost/optional.hpp>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_Function;
class WIR_BasicBlock;


/*!
  @brief This enum represents different types of %WIR control flow graph nodes.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
enum class WIR_CFGNodeType : char
{
  //! A %WIR function.
  fct,

  //! A %WIR basic block.
  bb
};


/*!
  @brief Class WIR_CFGNodeProperty represents the information attached to each
         node of the %WIR control flow graph.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_CFGNodeProperty
{

  public:

    /*!
      @brief Default constructor.

      @note Do not use this constructor! It exists only in order to make the
            Boost Graph Library happy. Instead, use one of the constructors
            below.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_CFGNodeProperty( void );

    /*!
      @brief Default constructur for control flow graph node information
             representing a %WIR function.

      @param[in] f A reference to a %WIR function to be represented by a control
                   flow graph node.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_CFGNodeProperty( WIR_Function & );

    /*!
      @brief Default constructur for control flow graph node information
             representing a %WIR basic block.

      @param[in] b A reference to a %WIR basic block to be represented by a
                   control flow graph node.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_CFGNodeProperty( WIR_BasicBlock & );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ~WIR_CFGNodeProperty( void );


    //
    // Type handling.
    //

    /*!
      @brief getType returns the type of a %WIR control flow graph node, i.e.,
             whether it is a function, basic block or instruction.

      @return The CFG node's type.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_CFGNodeType getType( void ) const;

    /*!
      @brief isFunction returns whether a CFG node represents a %WIR function.

      @return true if the CFG node represents a function, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isFunction( void ) const;

    /*!
      @brief isBasicBlock returns whether a CFG node represents a %WIR basic
             block.

      @return true if the CFG node represents a basic block, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isBasicBlock( void ) const;

    /*!
      @brief getFunction returns the %WIR function associated with a CFG node.

      @return A reference to the %WIR function represented by a CFG node.

      getFunction asserts if the CFG node does not represent a function.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Function &getFunction( void ) const;

    /*!
      @brief getBasicBlock returns the %WIR basic block associated with a CFG
             node.

      @return A reference to the %WIR basic block represented by a CFG node.

      getBasicBlock asserts if the CFG node does not represent a basic block.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_BasicBlock &getBasicBlock( void ) const;


    //
    // Node properties.
    //

    /*!
      @brief isStart returns whether a CFG node is a start node.

      @return true if the node is a start node, false otherwise.

      A node is a start node if it has no incoming edges except back-edges.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isStart( void ) const;

    /*!
      @brief setStart marks a CFG node as start node.

      @param[in] s A Boolean defaulting to true indicating whether the node is a
                 start node or not.

      A node is a start node if it has no incoming edges except back-edges.
    */
    void setStart( bool = true );


  private:

    //! mFct refers to the %WIR function a CFG node potentially represents.
    const boost::optional<std::reference_wrapper<WIR_Function>> mFct;

    //! mBB refers to the %WIR basic block a CFG node potentially represents.
    const boost::optional<std::reference_wrapper<WIR_BasicBlock>> mBB;

    //! mType stores a CFG node's actual type.
    const WIR_CFGNodeType mType;

    /*!
      @brief mIsStart marks a CFG node as start, i.e., as node without incoming
             edges except back-edges).
    */
    bool mIsStart;

};

}       // namespace WIR

#endif  // _WIR_CFG_NODE_PROPERTY_H
