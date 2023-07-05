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
  @file wirunsignedimmediateparameter.h
  @brief This file provides the interface of parameters representing unsigned
         immediate operands.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_UNSIGNEDIMMEDIATEPARAMETER_H
#define _WIR_UNSIGNEDIMMEDIATEPARAMETER_H


//
// Include section
//

// Include standard headers
#include <iostream>

// Include boost headers
#include <boost/current_function.hpp>

// Include libuseful headers
#include <libuseful/debugmacros.h>

// Include WIR headers
#include <wir/wirimmediateparameter.h>
#include <wir/wirtypes.h>


//
// Header section
//

namespace WIR {

/*!
  @brief Class WIR_UnsignedImmediateParameter is the generic representation of
         parameters holding unsigned immediate operands.

  It is simply a full specialization of the template
  WIR_ImmediateParameter<DerivedClass, false> with forwarding of constructors
  and assignment operators only. The key implementation issues of unsigned
  immediates can thus be found in the template specialization
  WIR_ImmediateParameter<DerivedClass, false>.

  WIR_UnsignedImmediateParameter adds functionality for statically and uniquely
  assigning immediate parameter type IDs. This way, immediates of different
  types can unambiguously be distinguished.

  WIR_UnsignedImmediateParameter serves as base class from which actual
  immediates for real processor architectures are derived. For this purpose,
  every immediate parameter type must inherit from
  WIR_UnsignedImmediateParameter in the following way:

  class MyImmediate : public WIR_UnsignedImmediateParameter<MyImmediate>
  {
    ...
  };

  This creates a template instantiation of WIR_UnsignedImmediateParameter that
  adds a static field for the immediate type ID of 'MyImmediate'. Furthermore,
  static initializers are added to MyImmediate that fill this field with a valid
  value allocated from class WIR_BaseImmediateParameter via a call to method
  registerNewImmediateType.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
template<typename DerivedUnsignedImmediateClass>
class WIR_UnsignedImmediateParameter : public WIR_ImmediateParameter<WIR_UnsignedImmediateParameter<DerivedUnsignedImmediateClass>, false>
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor for unsigned immediate parameters of the
             specified bit width registering its immediate parameter type.

      @param[in] __i The immediate value.
      @param[in] __w The number of available bits.

      The constructor ensures that __i lies in the range of values that can be
      represented by the parameter's bit width.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_UnsignedImmediateParameter( unsigned long long __i, unsigned int __w ) :
      WIR_ImmediateParameter<WIR_UnsignedImmediateParameter<DerivedUnsignedImmediateClass>, false> { __i, __w }
    {
      DSTART(
        "WIR_UnsignedImmediateParameter<DerivedUnsignedImmediateClass>::WIR_UnsignedImmediateParameter(long long unsigned int, unsigned int)" );

      // The following call is important, see also comments in
      // WIR_BaseImmediateParameter::Registrator::touch.
      mRegistrator.touch();
    };

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_UnsignedImmediateParameter( const WIR_UnsignedImmediateParameter &__o ) :
      WIR_ImmediateParameter<WIR_UnsignedImmediateParameter<DerivedUnsignedImmediateClass>, false> { __o }
    {
      DSTART(
        "WIR_UnsignedImmediateParameter<DerivedUnsignedImmediateClass>::WIR_UnsignedImmediateParameter(const WIR_UnsignedImmediateParameter<DerivedUnsignedImmediateClass>&)" );
    };

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_UnsignedImmediateParameter( WIR_UnsignedImmediateParameter &&__o ) :
      WIR_ImmediateParameter<WIR_UnsignedImmediateParameter<DerivedUnsignedImmediateClass>, false> { std::move( __o ) }
    {
      DSTART(
        "WIR_UnsignedImmediateParameter<DerivedUnsignedImmediateClass>::WIR_UnsignedImmediateParameter(WIR_UnsignedImmediateParameter<DerivedUnsignedImmediateClass>&&)" );
    };

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_UnsignedImmediateParameter( void )
    {
      DSTART(
        "WIR_UnsignedImmediateParameter<DerivedUnsignedImmediateClass>::~WIR_UnsignedImmediateParameter()" );
    };

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_UnsignedImmediateParameter & operator = ( const WIR_UnsignedImmediateParameter &__o )
    {
      DSTART(
        "WIR_UnsignedImmediateParameter<DerivedUnsignedImmediateClass>& WIR_UnsignedImmediateParameter<DerivedUnsignedImmediateClass>::operator=(const WIR_UnsignedImmediateParameter<DerivedUnsignedImmediateClass>&)" );

      WIR_ImmediateParameter<WIR_UnsignedImmediateParameter<DerivedUnsignedImmediateClass>, false>::operator = (
        __o );

      return( *this );
    };

    /*!
      @brief Move-assignment operator.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_UnsignedImmediateParameter & operator = ( WIR_UnsignedImmediateParameter &&__o )
    {
      DSTART(
        "WIR_UnsignedImmediateParameter<DerivedUnsignedImmediateClass>& WIR_UnsignedImmediateParameter<DerivedUnsignedImmediateClass>::operator=(WIR_UnsignedImmediateParameter<DerivedUnsignedImmediateClass>&&)" );

      WIR_ImmediateParameter<WIR_UnsignedImmediateParameter<DerivedUnsignedImmediateClass>, false>::operator = (
        std::move( __o ) );

      return( *this );
    };


    //
    // Generic type handling.
    //

    /*!
      @brief getImmediateTypeID returns the ID of an immediate parameter type.

      @return The immediate parameter type's ID.

      Since getImmediateTypeID is a static method, it can be used to query types
      for derived immediate parameter classes without having actual objects of
      such a parameter class.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    static WIR_id_t getImmediateTypeID( void )
    {
      std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      return( mImmediateID );
    };

    /*!
      @brief getImmediateType returns the ID of an immediate parameter type.

      @return The immediate parameter type's ID.

      getImmediateType can be used to query types for actual immediate parameter
      objects. Please note the difference to method getImmediateTypeID above that
      serves the same purpose but that is a static method.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_id_t getImmediateType( void ) const
    {
      DSTART(
        "WIR_id_t WIR_UnsignedImmediateParameter<DerivedUnsignedImmediateClass>::getImmediateType() const" );

      return( mImmediateID );
    };


  private:

    /*!
      @brief No standard construction allowed, users must use
             WIR_UnsignedImmediateParameter( unsigned long long, unsigned int )
             instead.
    */
    WIR_UnsignedImmediateParameter( void ) = delete;

    /*!
      @brief clone creates a copy of an unsigned immediate parameter.

      @return A pointer to the newly created copy of this parameter.

      Using the template argument, clone just calls the correct copy constructor
      of the actual immediate parameter class.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_BaseImmediateParameter *clone( void ) const
    {
      DSTART(
        "WIR_BaseImmediateParameter* WIR_UnsignedImmediateParameter<DerivedUnsignedImmediateClass>::clone() const" );

      return(
        new DerivedUnsignedImmediateClass(
          *( static_cast<const DerivedUnsignedImmediateClass *>( this ) ) ) );
    };

    //! mImmediateID holds the statically initialized immediate type ID.
    static WIR_id_t mImmediateID;

    //! mRegistrator is used to initialize mImmediateID via its constructor.
    static WIR_BaseImmediateParameter::Registrator mRegistrator;

};

template<typename DerivedUnsignedImmediateClass>
WIR_id_t WIR_UnsignedImmediateParameter<DerivedUnsignedImmediateClass>::mImmediateID;

template<typename DerivedUnsignedImmediateClass>
WIR_BaseImmediateParameter::Registrator WIR_UnsignedImmediateParameter<DerivedUnsignedImmediateClass>::mRegistrator(
  DerivedUnsignedImmediateClass::mImmediateID );

}       // namespace WIR

#endif  // _WIR_UNSIGNEDIMMEDIATEPARAMETER_H
