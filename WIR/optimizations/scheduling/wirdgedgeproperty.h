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
  @file wirdgedgeproperty.h
  @brief This file provides the basic properties of dependence graph edges.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_DG_EDGE_PROPERTY_H
#define _WIR_DG_EDGE_PROPERTY_H


//
// Header section
//

namespace WIR {

/*!
  @brief This enum represents different types of %WIR dependence graph edges.

  See also S. S. Muchnick, section 9.2, page 269.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
enum class WIR_DGEdgeType : char
{
  //! No dependence along a graph edge.
  none,

  //! A classical data dependence (read-after-write) along a graph edge.
  raw,

  //! An anti dependence (write-after-read) along a graph edge.
  war,

  //! An output dependence (write-after-write) along a graph edge.
  waw,

  //! A dependence along a graph edge resulting from a scheduling constraint.
  constr,

  /*!
    @brief An uncertain dependence where we cannot determine whether one
           operation can be moved beyond another.

    E.g., a load followed by a store where different address registers are
    involved and where we cannot determine whether the addressed locations might
    overlap.
  */
  ucert,

  //! A control dependence along a graph edge.
  ctrl
};


/*!
  @brief Class WIR_DGEdgeProperty represents the information attached to each
         edge of the %WIR dependence graph.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_DGEdgeProperty
{

  public:

    /*!
      @brief Default constructor.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_DGEdgeProperty( void ) :
      mLatency { -1 },
      mType { WIR_DGEdgeType::none }
    {
    };

    /*!
      @brief Default constructor for edges with given properties.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_DGEdgeProperty( long long l, WIR_DGEdgeType t ) :
      mLatency { l },
      mType { t }
    {
    };

    /*!
      @brief mLatency stores the required latency between two dependent
             operations.

      For an edge o1 -> o2 in the dependence graph, the latency is the delay
      required between the initiation times of o1 and o2, minus the execution
      time required by o1 before any other instruction can begin executing (cf.
      S. S. Muchnick, section 9.2, page 269).
    */
    long long mLatency;

    //! mType stores a DG edge's actual type.
    WIR_DGEdgeType mType;

};

}       // namespace WIR

#endif  // _WIR_DG_EDGE_PROPERTY_H
