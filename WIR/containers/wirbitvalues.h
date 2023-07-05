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
  @file wirbitvalues.h
  @brief This file provides the interface of a %WIR container representing
         bit-true up/down values of register parameters.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_BITVALUE_H
#define _WIR_BITVALUE_H


//
// Include section
//

// Include standard headers
#include <list>

// Include WIR headers
#include <wir/wircontainer.h>
#include <analyses/bit/wirupdownvalue.h>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_Parameter;


/*!
  @brief Class WIR_BitValues models bit-true up and down values of register
         parameters.

  WIR_BitValues containers are attached to %WIR register and immediate parameter
  objects during bit-true data flow analysis. If a register parameter is used,
  it is a target node of data flow graph edges. Accordingly, this container
  stores all incoming up/down values for this used register parameter. If the
  register parameter is defined, this container also stores all its outgoing
  up/down values.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_BitValues : public WIR_Container<WIR_BitValues>
{

  public:

    //
    // Local type definitions.
    //

    /*!
      @brief struct edge is a container for both the up and the down value of a
             data flow graph edge incident with that parameter to which the
             current WIR_BitValues container is attached.

      Assume a register parameter rp1 to which a WIR_BitValues container is
      attached. If rp1 is used, there is an incoming edge from some other
      register parameter rp2 to rp1 in the DFG: (rp2 -> rp1). If rp1 is defined,
      there is an outgoing edge from rp1 to some other register parameter rp2 in
      the DFG: (rp1 -> rp2). All these edges and their associated up/down values
      are represented by rp1's WIR_BitValues container using one instance of
      struct edge.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    struct edge
    {

      public:

        /*!
          @brief Default constructor for data flow graph edge information.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        edge( WIR_Parameter &p, WIR_UpDownValue &&d, WIR_UpDownValue &&u ) :
          rp { &p },
          downVal { std::move( d ) },
          upVal { std::move( u ) } {};

        /*!
          @brief rp points to the adjacent parameter in the DFG (rp2 in the
                 above comment).
        */
        WIR_Parameter *rp;

        //! downVal stores a DFG edge's bit-true down value.
        WIR_UpDownValue downVal;

        //! upVal stores a DFG edge's bit-true up value.
        WIR_UpDownValue upVal;

    };


    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_BitValues( void );

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_BitValues( const WIR_BitValues & );

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_BitValues( WIR_BitValues && );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_BitValues( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_BitValues & operator = ( const WIR_BitValues & );

    /*!
      @brief Move-assignment operator.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_BitValues & operator = ( WIR_BitValues && );

    /*!
      @brief isUnique returns whether bit-true up/down values are unique, i.e.,
             whether at most one instance of this container type can be attached
             to a %WIR class.

      @return Always true, bit-true up/down values are unique.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool isUnique( void ) const;


    //
    // Handling of incoming up/down values.
    //

    /*!
      @brief insertInValues adds new incoming edge values to set mInValues.

      @param[in] p A reference to a parameter that is the source of the new
                   incoming edge.
      @param[in] d An R-value reference to the new incoming edge's down value.
      @param[in] u An R-value reference to the new incoming edge's up value.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertInValues( WIR_Parameter &, WIR_UpDownValue &&,
                         WIR_UpDownValue && );

    /*!
      @brief eraseInValues removes the incoming edge starting at the specified
             parameter from set mInValues.

      @param[in] p A const reference to the parameter to be erased.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void eraseInValues( const WIR_Parameter & );

    /*!
      @brief eraseInValues removes the specified incoming edge from set
             mInValues.

      @param[in] pos An iterator denoting the edge to be erased.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void eraseInValues( std::list<edge>::iterator );

    /*!
      @brief clearInValues removes all elements from set mInValues.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void clearInValues( void );

    /*!
      @brief getInValues returns the set mInValues.

      @return A reference to the set mInValues.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<edge> &getInValues( void );

    /*!
      @brief findInValues finds the incoming up/down values of the edge starting
             at the specified parameter.

      @param[in] p A const reference to the parameter to be found.
      @return An iterator pointing to the found element, or the end() iterator
              otherwise.

      @note This function is rather inefficient: Its complexity is linear in the
            set's size. Thus, this method should be used with care.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<edge>::iterator findInValues( const WIR_Parameter & );


    //
    // Handling of outgoing up/down values.
    //

    /*!
      @brief insertOutValues adds new outgoing edge values to set mOutValues.

      @param[in] p A reference to a parameter that is the target of the new
                   outgoing edge.
      @param[in] d An R-value reference to the new outgoing edge's down value.
      @param[in] u An R-value reference to the new outgoing edge's up value.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertOutValues( WIR_Parameter &, WIR_UpDownValue &&,
                          WIR_UpDownValue && );

    /*!
      @brief eraseOutValues removes the outgoing edge ending at the specified
             parameter from set mOutValues.

      @param[in] p A const reference to the parameter to be erased.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void eraseOutValues( const WIR_Parameter & );

    /*!
      @brief clearOutValues removes all elements from set mOutValues.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void clearOutValues( void );

    /*!
      @brief getOutValues returns the set mOutValues.

      @return A reference to the set mOutValues.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<edge> &getOutValues( void );

    /*!
      @brief findOutValues finds the outgoing up/down values of the edge ending
             at the specified parameter.

      @param[in] p A const reference to the parameter to be found.
      @return An iterator pointing to the found element, or the end() iterator
              otherwise.

      @note This function is rather inefficient: Its complexity is linear in the
            set's size. Thus, this method should be used with care.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<edge>::iterator findOutValues( const WIR_Parameter & );


  private:

    /*!
      @brief mInValues holds the bit-true up/down values for all incoming edges
             of a register parameter.
    */
    std::list<edge> mInValues;

    /*!
      @brief mOutValues holds the bit-true up/down values for all outgoing edges
             of a register parameter.
    */
    std::list<edge> mOutValues;

};

}       // namespace WIR

#endif  // _WIR_BITVALUE_H
