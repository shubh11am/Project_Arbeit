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
  @file wirdfgnodeproperty.h
  @brief This file provides the basic properties of data flow graph nodes.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_DFG_NODE_PROPERTY_H
#define _WIR_DFG_NODE_PROPERTY_H


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

class WIR_BaseImmediateParameter;
class WIR_Operation;
class WIR_RegisterParameter;


/*!
  @brief This enum represents different types of %WIR data flow graph nodes.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
enum class WIR_DFGNodeType : char
{
  //! A %WIR operation.
  op,

  //! An immediate parameter.
  imm,

  //! A register parameter.
  reg
};


/*!
  @brief Class WIR_DFGNodeProperty represents the information attached to each
         node of the bit-true %WIR data flow graph.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_DFGNodeProperty
{

  public:

    /*!
      @brief Default constructor.

      @note Do not use this constructor! It exists only in order to make the
            Boost Graph Library happy. Instead, use one of the constructors
            below.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_DFGNodeProperty( void );

    /*!
      @brief Default constructur for data flow graph node information
             representing a %WIR operation.

      @param[in] o A reference to a %WIR operation to be represented by a data
                   flow graph node.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_DFGNodeProperty( WIR_Operation & );

    /*!
      @brief Default constructur for data flow graph node information
             representing an immediate parameter as graph input.

      @param[in] p A reference to a %WIR immediate parameter to be represented
                   by a data flow graph node.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_DFGNodeProperty( WIR_BaseImmediateParameter & );

    /*!
      @brief Default constructur for data flow graph node information
             representing a register as graph input.

      @param[in] p A reference to a %WIR register parameter to be represented by
                   a data flow graph node.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_DFGNodeProperty( WIR_RegisterParameter & );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ~WIR_DFGNodeProperty( void );


    //
    // Type handling.
    //

    /*!
      @brief getType returns the type of a %WIR data flow graph node, i.e.,
             whether it is an operation or an immediate or register parameter.

      @return The DFG node's type.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_DFGNodeType getType( void ) const;

    /*!
      @brief isOperation returns whether a DFG node represents a %WIR operation.

      @return true if the DFG node represents an operation, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isOperation( void ) const;

    /*!
      @brief getOperation returns the %WIR operation associated with a DFG node.

      @return A reference to the %WIR operation represented by a DFG node.

      getOperation asserts if the DFG node does not represent an operation.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Operation &getOperation( void ) const;

    /*!
      @brief getImmediateParameter returns the %WIR immediate parameter
             associated with a DFG node.

      @return A reference to the %WIR immediate parameter represented by a DFG
              node.

      getImmediateParameter asserts if the DFG node does not represent an
      immediate parameter.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_BaseImmediateParameter &getImmediateParameter( void ) const;

    /*!
      @brief getRegisterParameter returns the %WIR register parameter associated
             with a DFG node.

      @return A reference to the %WIR register parameter represented by a DFG
              node.

      getRegisterParameter asserts if the DFG node does not represent a register
      parameter.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_RegisterParameter &getRegisterParameter( void ) const;


    //
    // Node properties.
    //

    /*!
      @brief setSource marks a DFG node as source node.

      @param[in] s A Boolean defaulting to true that denotes whether the current
                   node is a source node or not.

      According to Jens Wagner, Retargierbare Ausnutzung von Spezialoperationen
      für Eingebettete Systeme mit Hilfe bitgenauer Wertflussanalyse, page 155,
      definition 4.5, a node is a source node if it has no incoming edges except
      back-edges.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setSource( bool = true );

    /*!
      @brief isSource returns whether a DFG node is a source node.

      @return true if the node is a source node, false otherwise.

      According to Jens Wagner, Retargierbare Ausnutzung von Spezialoperationen
      für Eingebettete Systeme mit Hilfe bitgenauer Wertflussanalyse, page 155,
      definition 4.5, a node is a source node if it has no incoming edges except
      back-edges.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isSource( void ) const;

    /*!
      @brief setSink marks a DFG node as sink node.

      @param[in] s A Boolean defaulting to true that denotes whether the current
                   node is a sink node or not.

      According to Jens Wagner, Retargierbare Ausnutzung von Spezialoperationen
      für Eingebettete Systeme mit Hilfe bitgenauer Wertflussanalyse, page 155,
      definition 4.6, a node is a sink node if it has no outgoing edges except
      back-edges.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setSink( bool = true );

    /*!
      @brief isSink returns whether a DFG node is a sink node.

      @return true if the node is a sink node, false otherwise.

      According to Jens Wagner, Retargierbare Ausnutzung von Spezialoperationen
      für Eingebettete Systeme mit Hilfe bitgenauer Wertflussanalyse, page 155,
      definition 4.6, a node is a sink node if it has no outgoing edges except
      back-edges.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isSink( void ) const;


  private:

    //! mOp refers to the %WIR operation a DFG node potentially represents.
    const boost::optional<std::reference_wrapper<WIR_Operation>> mOp;

    /*!
      @brief mImm refers to the %WIR immediate parameter a DFG node potentially
             represents.
    */
    const boost::optional<std::reference_wrapper<WIR_BaseImmediateParameter>> mImm;

    /*!
      @brief mReg refers to the %WIR register parameter a DFG node potentially
             represents.
    */
    const boost::optional<std::reference_wrapper<WIR_RegisterParameter>> mReg;

    //! mType stores a DFG node's actual type.
    const WIR_DFGNodeType mType;

    /*!
      @brief mIsSource marks a DFG node as source, i.e., as node without
             incoming edges (except back-edges, cf. Jens Wagner, Retargierbare
             Ausnutzung von Spezialoperationen für Eingebettete Systeme mit
             Hilfe bitgenauer Wertflussanalyse, page 155, definition 4.5).
    */
    bool mIsSource;

    /*!
      @brief mIsSink marks a DFG node as sink, i.e., as node without outgoing
             edges (except back-edges, cf. Jens Wagner, Retargierbare Ausnutzung
             von Spezialoperationen für Eingebettete Systeme mit Hilfe
             bitgenauer Wertflussanalyse, page 155, definition 4.6).
    */
    bool mIsSink;

};

}       // namespace WIR

#endif  // _WIR_DFG_NODE_PROPERTY_H
