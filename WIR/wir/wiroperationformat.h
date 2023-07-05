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
  @file wiroperationformat.h
  @brief This file provides the interface of %WIR operation formats.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_OPERATIONFORMAT_H
#define _WIR_OPERATIONFORMAT_H


//
// Include section
//

// Include standard headers
#include <functional>
#include <list>
#include <memory>
#include <string>

// Include libuseful headers
#include <libuseful/debugmacros.h>

// Include WIR headers
#include <wir/wirtypes.h>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_Parameter;
class WIR_BaseRegister;


/*!
  @brief Class WIR_OperationFormat models generic formats of machine operations.

  Using WIR_OperationFormat, different formats of machine operations of an
  actual processor architecture can be distinguished.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_OperationFormat final
{

  public:

    /*!
      @brief Default constructor for %WIR operations.

      @tparam Args R-value references of class WIR_Parameter or derived classes.
      @param[in] __args A variadic number of R-value references to
                        WIR_Parameters for this operation format.

      This constructor uses variadic templates so that argument lists of
      arbitrary lengths can be passed to it.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    template<typename... Args>
    // cppcheck-suppress noExplicitConstructor
    WIR_OperationFormat( Args&&... __args )
    {
      DSTART( "WIR_OperationFormat::WIR_OperationFormat(Args&& ...)" );

      addParameters( std::forward<Args>( __args )... );
    };

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ~WIR_OperationFormat( void );

    /*!
      @brief getParameters returns the list mParameterReferences.

      @return A const reference to the list mParameterReferences.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const std::list<std::reference_wrapper<WIR_Parameter>> &getParameters( void ) const;

    /*!
      @brief begin returns an iterator to the first parameter of an operation
             format.

      @return A const iterator pointing to the first parameter.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<std::reference_wrapper<WIR_Parameter>>::const_iterator begin( void ) const;

    /*!
      @brief end returns an iterator to the end of an operation format's
             parameter list.

      @return A const iterator pointing to the position after the last parameter
              of an operation.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<std::reference_wrapper<WIR_Parameter>>::const_iterator end( void ) const;


  private:

    friend class WIR_Registry;

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_OperationFormat( WIR_OperationFormat && );

    /*!
      @brief Move-assignment operator.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_OperationFormat & operator = ( WIR_OperationFormat && );

    /*!
      @brief addParameters adds the given list of parameters to this operation
             format.

      @tparam T One R-value reference of class WIR_Parameter or derived classes.
      @tparam Args R-value references of class WIR_Parameter or derived classes.
      @param[in] a An R-value reference to a %WIR parameter.
      @param[in] args A variadic number of R-value references to %WIR
                      parameters.

      addParameters uses variadic templates so that argument lists of arbitrary
      lengths can be passed to it.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    template<typename T, typename... Args>
    void addParameters( T&& a, Args&&... args )
    {
      DSTART( "void WIR_OperationFormat::addParameters(T&&, Args&& ...)" );

      mParameterReferences.push_back( *a );
      mParameterPointers.push_back( std::unique_ptr<WIR_Parameter>( a ) );

      addParameters( std::forward<Args>( args )... );
    };

    /*!
      @brief Dummy function for adding parameters which does nothing.

      It only serves to terminate the recursion of the variadic method
      addParameters.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void addParameters( void ) const;

    /*!
      @brief mParameterPointers holds managed pointers to all parameters
             that determine an operation format.
    */
    std::list<std::unique_ptr<WIR_Parameter>> mParameterPointers;

    /*!
      @brief mParameterReferences holds the sequence of (wrapped) parameters
             that determines an operation format.
    */
    std::list<std::reference_wrapper<WIR_Parameter>> mParameterReferences;

};

}       // namespace WIR

#endif  // _WIR_OPERATIONFORMAT_H
