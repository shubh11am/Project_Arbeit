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
  @file wirigraphedgeproperty.h
  @brief This file provides the basic properties of interference graph edges.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_IGRAPH_EDGE_PROPERTY_H
#define _WIR_IGRAPH_EDGE_PROPERTY_H


//
// Header section
//

namespace WIR {

/*!
  @brief Struct IGraphEdgeProperty represents the information attached to each
         edge of the %WIR interference graph.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
struct IGraphEdgeProperty
{

  public:

    /*!
      @brief Default constructur for interference graph edge information.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    IGraphEdgeProperty() :
      mIsPushed( false )
    {
    };

    /*!
      @brief mIsPushed marks an interference graph edge as being removed from
             the interference graph and pushed onto the stack.
    */
    bool mIsPushed;

};

}       // namespace WIR

#endif  // _WIR_IGRAPH_EDGE_PROPERTY_H
