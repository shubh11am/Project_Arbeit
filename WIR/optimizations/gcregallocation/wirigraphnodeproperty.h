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
  @file wirigraphnodeproperty.h
  @brief This file provides the basic properties of interference graph nodes.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_IGRAPH_NODE_PROPERTY_H
#define _WIR_IGRAPH_NODE_PROPERTY_H


//
// Include section
//

// Include standard headers
#include <functional>
#include <list>
#include <map>

// Include boost headers
#include <boost/optional.hpp>

// Include WIR headers
#include <wir/wirtypes.h>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_BaseRegister;


/*!
  @brief Struct IGraphNodeProperty represents the information attached to each
         node of the %WIR interference graph.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
struct IGraphNodeProperty
{

  public:

    /*!
      @brief Default constructur for interference graph node information.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    IGraphNodeProperty() :
      mIsCoalescedNode { false },
      mIsPotentialSpill { false },
      mIsActualSpill { false },
      mIsPushed { false },
      mLoopNestingDepth { 0 },
      mSpillCosts { 0 },
      mDegree { 0 }
    {
    };

    /*!
      @brief mRegister refers to the %WIR register an interference graph node
             represents.
    */
    boost::optional<std::reference_wrapper<WIR_BaseRegister>> mRegister;

    /*!
      @brief mLeafRegisters contains the set of registers which represent the
             leafs of a potential register hierarchy via mRegister.
    */
    std::list<std::reference_wrapper<WIR_BaseRegister>> mLeafRegisters;

    //! mIsCoalescedNode marks an interference graph node as being coalesced.
    bool mIsCoalescedNode;

    //! mIsPotentialSpill marks an interference graph node as potential spill.
    bool mIsPotentialSpill;

    //! mIsActualSpill marks an interference graph node as actual spill.
    bool mIsActualSpill;

    /*!
      @brief mIsPushed marks an interference graph node as being removed from
             the interference graph and pushed onto the stack.
    */
    bool mIsPushed;

    /*!
      @brief mColors represents an interference graph node's color(s).

      If mColors's size is 0, then this interference graph node is uncolored.

      Otherwise, if this node's primary register mRegister is a simple,
      non-hierarchical register, mColors[ mRegister ] returns the only one and
      unique color of this node.

      Alternatively, if this node's primary register mRegister is a hierarchical
      register, mColors must be indexed with the IDs of the leafs of the
      register hierarchy and then returns the color assigned to an individual
      leaf register of the current hierarchy.
    */
    std::map<WIR_id_t, unsigned int> mColors;

    /*!
      @brief mLoopNestingDepth represents the maximal loop depth in which the
             current node/register is defined or used.
    */
    unsigned int mLoopNestingDepth;

    /*!
      @brief mSpillCosts represents the estimated costs if the current node is
             spilled.
    */
    unsigned int mSpillCosts;

    //! mDegree holds a graph node's current degree.
    unsigned int mDegree;

};

}       // namespace WIR

#endif  // _WIR_IGRAPH_NODE_PROPERTY_H
