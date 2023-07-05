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
  @file wirdfgedgeproperty.h
  @brief This file provides the basic properties of data flow graph edges.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_DFG_EDGE_PROPERTY_H
#define _WIR_DFG_EDGE_PROPERTY_H


//
// Include section
//

// Include standard headers
#include <functional>

// Include local headers
#include "wirdfgnodeproperty.h"
#include "wirupdownvalue.h"


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_BaseImmediateParameter;
class WIR_DFG;
class WIR_Parameter;
class WIR_RegisterParameter;


/*!
  @brief Class WIR_DFGEdgeProperty represents the information attached to each
         edge of the bit-true %WIR data flow graph.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_DFGEdgeProperty
{

  public:

    /*!
      @brief Default constructor for data flow graph edges between two source
             and target register parameters.

      @param[in] s A const reference to a %WIR register parameter denoting the
                   edge's source.
      @param[in] t A const reference to a %WIR register parameter denoting the
                   edge's target.
      @param[in] d A const reference to the DFG instance to which an edge
                   belongs.

      Upon creation of a DFG edge, the data flow from source to target is
      assumed to be unsigned, since the semantics of the source and target %WIR
      operations are unknown here. The signedness of the up/down values attached
      to this edge thus has to be corrected later during up or down analysis
      where processor-specific handlers of the operations are involved.

      An edge's source parameter must be a defined or def-used register
      parameter. An edge's target parameter must be a used or def-used register
      parameter. Both register parameters have to refer to the same register.
      This constructor asserts if these preconditions are not met.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_DFGEdgeProperty( const WIR_RegisterParameter &,
                         const WIR_RegisterParameter &, const WIR_DFG & );

    /*!
      @brief Default constructor for data flow graph edges denoting an input
             from an immediate value.

      @param[in] p A const reference to a %WIR immediate parameter denoting the
                   graph input.
      @param[in] d A const reference to the DFG instance to which an edge
                   belongs.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_DFGEdgeProperty( const WIR_BaseImmediateParameter &, const WIR_DFG & );

    /*!
      @brief Default constructor for data flow graph edges denoting an input
             from a function-external register.

      @param[in] p A const reference to a %WIR register parameter denoting the
                   graph input.
      @param[in] d A const reference to the DFG instance to which an edge
                   belongs.

      Upon creation of a DFG edge, the data flow via a function-external
      register is assumed to be unsigned, since the semantics of the involved
      %WIR operation are unknown here. The signedness of the up/down values
      attached to this edge thus has to be corrected later during up or down
      analysis where processor-specific handlers of the operations are involved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_DFGEdgeProperty( const WIR_RegisterParameter &, const WIR_DFG & );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ~WIR_DFGEdgeProperty( void );


    //
    // Edge properties.
    //

    /*!
      @brief getType returns the type of a %WIR data flow graph edge, i.e.,
             whether it is an operation or an immediate or register parameter.

      @return The DFG edge's type.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_DFGNodeType getType( void ) const;

    /*!
      @brief If an edge represents data flow via a register,
             getSourceRegisterParameter returns the edge's source register
             parameter.

      @return A const reference to the edge's source register parameter.

      An edge represents data flow via a register if the edge type is either
      'op' or 'reg'. getSourceRegisterParameter asserts if it is called for
      different edge types.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const WIR_RegisterParameter &getSourceRegisterParameter( void ) const;

    /*!
      @brief If an edge represents data flow via a register,
             getTargetRegisterParameter returns the edge's target register
             parameter.

      @return A const reference to the edge's target register parameter.

      An edge represents data flow via a register if the edge type is either
      'op' or 'reg'. getTargetRegisterParameter asserts if it is called for
      different edge types.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const WIR_RegisterParameter &getTargetRegisterParameter( void ) const;

    /*!
      @brief If an edge represents data flow via an immediate,
             getImmediateParameter returns the edge's immediate parameter.

      @return A const reference to the edge's immediate parameter.

      An edge represents data flow via an immediate if the edge type is 'imm'.
      getImmediateParameter asserts if it is called for different edge types.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const WIR_BaseImmediateParameter &getImmediateParameter( void ) const;

    /*!
      @brief setDownValue sets an edge's bit-true down value.

      @param[in] v An R-value reference to the down value to be set.

      The edge's down value is only updated if v is bitwise higher in half-order
      L4 than the edge's previous down value. If the edge's down value actually
      remains unmodified, the edge is marked to have reached a fixed point.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setDownValue( WIR_UpDownValue && );

    /*!
      @brief getDownValue provides an edge's bit-true down value.

      @return A reference to the edge's down value.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_UpDownValue &getDownValue( void );

    /*!
      @brief setUpValue sets an edge's bit-true up value.

      @param[in] v An  R-value reference to the up value to be set.

      The edge's up value is only updated if v is bitwise higher in half-order
      L4 than the edge's previous up value. If the edge's up value actually
      remains unmodified, the edge is marked to have reached a fixed point.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setUpValue( WIR_UpDownValue && );

    /*!
      @brief getUpValue provides an edge's bit-true up value.

      @return A reference to the edge's up value.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_UpDownValue &getUpValue( void );

    /*!
      @brief initUpValue initializes an edge's up value with the current content
             of the edge's down value.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void initUpValue( void );

    /*!
      @brief isBackEdge returns whether a DFG edge is a back edge.

      @return true if the edge is a back edge, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isBackEdge( void ) const;

    /*!
      @brief isFix returns whether a DFG edge reached a fixed point or not.

      @return true if the edge reached a fixed point during analysis, false
              otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isFix( void ) const;

    /*!
      @brief setFix sets whether a DFG edge reached a fixed point during
             analysis or not.

      @param[in] f A Boolean defaulting to true that denotes whether the edge
                   reached a fixed point (true) or not (false).

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setFix( bool = true );

    /*!
      @brief mDistance stores the distance of one single DFG edge between any
             two nodes, i.e., the constant 1.
    */
    const long long mDistance { 1 };


  private:

    /*!
      @brief No standard construction allowed, users must use one of the above
             default constructors instead.
    */
    WIR_DFGEdgeProperty( void ) = delete;

    //! mSource refers to the source %WIR parameter of a DFG edge.
    const WIR_Parameter &mSource;

    //! mTarget refers to the target %WIR parameter of a DFG edge.
    const WIR_Parameter &mTarget;

    //! mType stores a DFG edge's actual type.
    const WIR_DFGNodeType mType;

    //! mDownValue holds the bit-true down value of a DFG edge.
    WIR_UpDownValue mDownValue;

    //! mUpValue holds the bit-true up value of a DFG edge.
    WIR_UpDownValue mUpValue;

    //! mIsBackEdge marks an edge as back edge stemming from a loop.
    bool mIsBackEdge;

    /*!
      @brief mIsFix stores whether an edge reached a fixed point during analysis
             or not.
    */
    bool mIsFix;

    //! mDFG points to the DFG instance to which an edge belongs.
    const WIR_DFG *mDFG;

};

}       // namespace WIR

#endif  // _WIR_DFG_EDGE_PROPERTY_H
