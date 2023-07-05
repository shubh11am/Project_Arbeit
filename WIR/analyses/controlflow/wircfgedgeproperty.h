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
  @file wircfgedgeproperty.h
  @brief This file provides the basic properties of control flow graph edges.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_CFG_EDGE_PROPERTY_H
#define _WIR_CFG_EDGE_PROPERTY_H


//
// Header section
//

namespace WIR {

/*!
  @brief This enum represents different types of %WIR control flow graph edges.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
enum class WIR_CFGEdgeType : char
{
  //! A regular edge.
  regular,

  //! A true-edge of a conditional jump.
  tru,

  //! A function call edge.
  call,

  //! A function return edge.
  ret
};


/*!
  @brief Class WIR_CFGEdgeProperty represents the information attached to each
         edge of the %WIR control flow graph.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_CFGEdgeProperty
{

  public:

    /*!
      @brief Default constructor for regular control flow graph edges.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_CFGEdgeProperty( void ) :
      mType { WIR_CFGEdgeType::regular },
      mIsBackEdge { false }
    {
    };

    //! mType stores a CFG edge's actual type.
    WIR_CFGEdgeType mType;

    //! mIsBackEdge stores whether a CFG edge is a back edge or not.
    bool mIsBackEdge;

};

}       // namespace WIR

#endif  // _WIR_CFG_EDGE_PROPERTY_H
