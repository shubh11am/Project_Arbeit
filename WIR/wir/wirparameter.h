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
  @file wirparameter.h
  @brief This file provides the interface of parameters of %WIR operations.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_PARAMETER_H
#define _WIR_PARAMETER_H


//
// Include section
//

// Include standard headers
#include <ostream>

// Include WIR headers
#include <wir/API/wircontainerapi.h>
#include <wir/API/wiridapi.h>
#include <wir/API/wirinsertionapi.h>
#include <wir/API/wirlistapi.h>
#include <wir/wirtypes.h>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_Operation;


/*!
  @brief Class WIR_Parameter is the generic representation of parameters of code
         operations.

  This class serves as virtual base class from which parameters for actual
  processor instances are derived.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_Parameter : public WIR_ID_API,
                      public WIR_Container_API
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor creating an empty parameter.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Parameter( void );

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      When copying a parameter that is inserted in some %WIR operation, the
      resulting copy will not be inserted in an operation.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Parameter( const WIR_Parameter & );

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      Trying to move a parameter that is inserted in some %WIR operation results
      in an assertion, since you are not allowed to move a parameter whose
      ownership is managed by an operation.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Parameter( WIR_Parameter && );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_Parameter( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      When copying a parameter that is inserted in some %WIR operation, the
      resulting copy will not be inserted in an operation.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Parameter & operator = ( const WIR_Parameter & );

    /*!
      @brief Move-assignment operator.

      @param[in] __o An R-value reference to another object to be moved.

      Trying to move a parameter that is inserted in some %WIR operation results
      in an assertion, since you are not allowed to move a parameter whose
      ownership is managed by an operation.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Parameter & operator = ( WIR_Parameter && );


    //
    // Operation handling.
    //

    // Realize the API to manage a parameter's parent operation.
    WIR_INSERTION_DECL( WIR_Operation, Operation );


    //
    // Generic type handling.
    //

    /*!
      @brief getType returns the type of a %WIR parameter, i.e., whether it is a
             register, label, immediate or other kind of parameter.

      @return The parameter's type.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_ParameterType getType( void ) const = 0;


    //
    // Implicit parameter handling.
    //

    /*!
      @brief setImplicit specifies a parameter as implicit or not.

      @param[in] i A Boolean value denoting whether a parameter is implicit or
                   not.

      An implicit parameter will not be made explicit in a %WIR assembly code
      dump. Instead, such implicit parameters are hidden ones that internally
      reflect data dependencies. Using implicit parameters, e.g., additional
      def/use relationships can be modeled.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setImplicit( bool = true );

    /*!
      @brief isExplicit returns whether a parameter is explicit or not.

      @return true if the parameter is explicit, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isExplicit( void ) const;

    /*!
      @brief isImplicit returns whether a parameter is implicit or not.

      @return true if the parameter is implicit, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isImplicit( void ) const;


    //
    // Modification prevention.
    //

    /*!
      @brief setDontOptimize sets whether a parameter can be modified or must
             not be changed by some optimization or transformation.

      @param[in] f A Boolean flag denoting whether the parameter must not be
                   modified.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setDontOptimize( bool = true );

    /*!
      @brief getDontOptimize returns whether a parameter can be modified or
             must not be changed by some optimization or transformation.

      @return true if the parameter must not be modified, false otherwise.

      A parameter must not be modified if the parameter by itself has been
      marked as such using setDontOptimize, or if it is inserted into a %WIR
      operation that in turn must not be modified.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool getDontOptimize( void ) const;


    //
    // Stream I/O.
    //

    /*!
      @brief The << operator dumps a %WIR parameter to an output stream.

      @param[in] os A reference to an output stream.
      @param[in] p A const reference to the %WIR parameter to be dumped.
      @return A reference to the same output stream.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    friend std::ostream & operator << ( std::ostream &, const WIR_Parameter & );


  protected:

    /*!
      @brief checkDontOptimize checks whether a parameter must not be modified.

      If this parameter must not be modified, checkDontOptimize asserts.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void checkDontOptimize( void ) const;

    /*!
      @brief clone creates a copy of a %WIR parameter.

      @return A pointer to the newly created copy of this parameter.

      Since the implementation details depend on some actual parameter's
      characteristics, clone is a pure virtual method.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_Parameter *clone( void ) const = 0;


  private:

    friend class WIR_OperationFormat;

    //! mImplicit stores whether a register parameter is implicit or not.
    bool mImplicit;

    /*!
      @brief mDontOptimize stores whether a parameter can be modified or must
             not be changed by some optimization or transformation.
    */
    bool mDontOptimize;

};

}       // namespace WIR

#endif  // _WIR_PARAMETER_H
