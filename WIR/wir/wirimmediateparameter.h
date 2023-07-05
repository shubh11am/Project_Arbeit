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
  @file wirimmediateparameter.h
  @brief This file provides the template-based interface of both signed and
         unsigned immediate parameters.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_IMMEDIATEPARAMETER_H
#define _WIR_IMMEDIATEPARAMETER_H


//
// Include section
//

// Include standard headers
#include <iostream>
#include <string>
#include <sstream>

// Include boost headers
#include <boost/current_function.hpp>

// Include libuseful headers
#include <libuseful/debugmacros.h>
#include <libuseful/io.h>

// Include WIR headers
#include <wir/wirbaseimmediateparameter.h>
#include <wir/wirmisc.h>
#include <wir/wirregistry.h>


//
// Header section
//

namespace WIR {


/*!
  @brief Class WIR_GenericImmediateParameter models generic immediate
         parameters, be it signed or unsigned.

  @tparam DerivedClass This template argument is used to specify the actual type
                       of immediate parameter to be instantiated.
  @tparam Signed This Boolean template argument is used to distinguish between
                  signed and unsigned immediates in template instantiations. Set
                  it to true if you instantiate a signed immediate class,
                  otherwise use false.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
template <typename DerivedClass, bool Signed>
class WIR_GenericImmediateParameter : public WIR_BaseImmediateParameter
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor creating an immediate parameter for the
             specified bit width.

      @param[in] __w The number of available bits.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_GenericImmediateParameter( unsigned int __w ) :
      WIR_BaseImmediateParameter( __w )
    {
      DSTART(
        "WIR_GenericImmediateParameter<DerivedClass, Signed>::WIR_GenericImmediateParameter(unsigned int)" );
    };

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_GenericImmediateParameter( const WIR_GenericImmediateParameter &__o ) :
      WIR_BaseImmediateParameter( __o )
    {
      DSTART(
        "WIR_GenericImmediateParameter<DerivedClass, Signed>::WIR_GenericImmediateParameter(const WIR_GenericImmediateParameter<DerivedClass, Signed>&)" );
    };

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_GenericImmediateParameter( WIR_GenericImmediateParameter &&__o ) :
      WIR_BaseImmediateParameter( std::move( __o ) )
    {
      DSTART(
        "WIR_GenericImmediateParameter<DerivedClass, Signed>::WIR_GenericImmediateParameter(WIR_GenericImmediateParameter<DerivedClass, Signed>&&)" );
    };

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_GenericImmediateParameter( void )
    {
      DSTART(
        "WIR_GenericImmediateParameter<DerivedClass, Signed>::~WIR_GenericImmediateParameter() " );
    };

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_GenericImmediateParameter & operator = ( const WIR_GenericImmediateParameter &__o )
    {
      DSTART(
        "WIR_GenericImmediateParameter<DerivedClass, Signed>& WIR_GenericImmediateParameter<DerivedClass, Signed>::operator=(const WIR_GenericImmediateParameter<DerivedClass, Signed>&)" );

      WIR_BaseImmediateParameter::operator = ( __o );

      return( *this );
    };

    /*!
      @brief Move-assignment operator.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_GenericImmediateParameter & operator = ( WIR_GenericImmediateParameter &&__o )
    {
      DSTART(
        "WIR_GenericImmediateParameter<DerivedClass, Signed>& WIR_GenericImmediateParameter<DerivedClass, Signed>::operator=(WIR_GenericImmediateParameter<DerivedClass, Signed>&&)" );

      WIR_BaseImmediateParameter::operator = ( std::move( __o ) );

      return( *this );
    };


    //
    // Value handling.
    //

    /*!
      @brief isSigned returns whether an immediate parameter is signed or not.

      @return true if the immediate parameter is signed, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool isSigned( void ) const
    {
      DSTART(
        "bool WIR_GenericImmediateParameter<DerivedClass, Signed>::isSigned() const" );

      return( Signed );
    };


  private:

    /*!
      @brief No standard construction allowed, users must use
             WIR_GenericImmediateParameter( unsigned int ) instead.
    */
    WIR_GenericImmediateParameter( void ) = delete;

};


/*!
  @brief Class WIR_ImmediateParameter models generic immediate parameters, be it
         signed or unsigned.

  @tparam DerivedClass This template argument is used to specify the actual type
                       of immediate parameter to be instantiated.
  @tparam Signed This Boolean template argument is used to distinguish between
                 signed and unsigned immediates in template instantiations. Set
                 it to true if you instantiate a signed immediate class,
                 otherwise use false.

  This variant of WIR_ImmediateParameter just serves as primary template from
  which specialized template instantiations along the Boolean template argument
  Signed are derived. Thus, WIR_ImmediateParameter here can be seen just as a
  piece of dummy code that is not really used elsewhere. The key implementation
  issues for signed or unsigned immediate parameters can be found in the two
  template specializations below.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
template <typename DerivedClass, bool Signed>
class WIR_ImmediateParameter : public WIR_GenericImmediateParameter<DerivedClass, Signed>
{
};


/*!
  @brief The specialized template class
         WIR_ImmediateParameter<DerivedClass, true> models generic signed
         immediate parameters.

  @tparam DerivedClass This template argument is used to specify the actual type
                       of immediate parameter to be instantiated.
  @tparam Signed This Boolean template argument is used to distinguish between
                 signed and unsigned immediates in template instantiations. Set
                 it to true if you instantiate a signed immediate class,
                 otherwise use false.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
template <typename DerivedClass>
class WIR_ImmediateParameter<DerivedClass, true> : public WIR_GenericImmediateParameter<DerivedClass, true>
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor for signed immediate parameters of the
             specified bit width.

      @param[in] __i The immediate value.
      @param[in] __w The number of available bits.

      The constructor ensures that __i lies in the range of values that can be
      represented by the parameter's bit width, assuming two's-complement as
      underlying data format.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    // cppcheck-suppress uninitMemberVar
    WIR_ImmediateParameter( signed long long __i, unsigned int __w ) :
      WIR_GenericImmediateParameter<DerivedClass, true> { __w },
      mValue { 0 }
    {
      DSTART(
        "WIR_ImmediateParameter<DerivedClass, true>::WIR_ImmediateParameter(long long int, unsigned int)" );

      setValue( __i );
    };

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    // cppcheck-suppress uninitMemberVar
    WIR_ImmediateParameter( const WIR_ImmediateParameter &__o ) :
      WIR_GenericImmediateParameter<DerivedClass, true> { __o },
      mValue { __o.mValue }
    {
      DSTART(
        "WIR_ImmediateParameter<DerivedClass, true>::WIR_ImmediateParameter(const WIR_ImmediateParameter<DerivedClass, true>&)" );
    };

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    // cppcheck-suppress uninitMemberVar
    WIR_ImmediateParameter( WIR_ImmediateParameter &&__o ) :
      WIR_GenericImmediateParameter<DerivedClass, true> { std::move( __o ) },
      mValue { std::move( __o.mValue ) }
    {
      DSTART(
        "WIR_ImmediateParameter<DerivedClass, true>::WIR_ImmediateParameter(WIR_ImmediateParameter<DerivedClass, true>&&)" );

      __o.mValue = 0;
    };

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_ImmediateParameter( void )
    {
      DSTART(
        "WIR_ImmediateParameter<DerivedClass, true>::~WIR_ImmediateParameter()" );
    };

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    DerivedClass & operator = ( const DerivedClass &__o )
    {
      DSTART(
        "DerivedClass& WIR_ImmediateParameter<DerivedClass, true>::operator=(const DerivedClass&)" );

      WIR_GenericImmediateParameter<DerivedClass, true>::operator = ( __o );

      mValue = __o.mValue;

      return( static_cast<DerivedClass &>( *this ) );
    };

    /*!
      @brief Move-assignment operator.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    DerivedClass & operator = ( DerivedClass &&__o )
    {
      DSTART(
        "DerivedClass& WIR_ImmediateParameter<DerivedClass, true>::operator=(DerivedClass&&)" );

      WIR_GenericImmediateParameter<DerivedClass, true>::operator = (
        std::move( __o ) );

      mValue = std::move( __o.mValue );
      __o.mValue = 0;

      return( static_cast<DerivedClass &>( *this ) );
    };


    //
    // Value handling.
    //

    /*!
      @brief setValue sets a signed immediate parameter's actual value.

      @param[in] i The parameter's new immediate value.

      setValue ensures that i lies in the range of values that can be
      represented by the parameter's bit width, assuming two's-complement as
      underlying data format.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void setValue( signed long long i )
    {
      DSTART(
        "void WIR_ImmediateParameter<DerivedClass, true>::setValue(long long int)" );

      WIR_Parameter::checkDontOptimize();

      // Ensure that i lies in the range of values that can be represented in
      // two's-complement by the parameter's bit width.
      signed long long minValue = getMinValue(
        WIR_BaseImmediateParameter::mBitWidth );
      signed long long maxValue = getMaxValue(
        WIR_BaseImmediateParameter::mBitWidth );

      ufAssertT(
        ( i >= minValue ) && ( i <= maxValue ),
        "Immediate value " << i << " is not within the interval [" <<
        minValue << ", " << maxValue << "] of " <<
        WIR_BaseImmediateParameter::mBitWidth << "-bit signed values." );

      mValue = i;
    };

    /*!
      @brief getValue gets a signed immediate parameter's value.

      @return The parameter's current value.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    signed long long getValue( void ) const
    {
      DSTART(
        "long long int WIR_ImmediateParameter<DerivedClass, true>::getValue() const" );

      return( mValue );
    };

    /*!
      @brief getSignedValue gets a signed immediate parameter's value.

      @return The parameter's current signed value.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual signed long long getSignedValue( void ) const
    {
      DSTART(
        "long long int WIR_ImmediateParameter<DerivedClass, true>::getSignedValue() const" );

      return( getValue() );
    };

    /*!
      @brief getUnsignedValue gets an unsigned immediate parameter's value.

      @return Nothing

      Since this class models signed immediate parameters, getUnsignedValue
      simply fails with an assertion.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual unsigned long long getUnsignedValue( void ) const
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      ufAssert( false );
      return( 0 );
    };

    /*!
      @brief getMinValue returns the minimal signed value that can be
             represented in two's-complement with the specified amount of bits.

      @param[in] bitwidth The number of available bits.
      @return The minimal representable value.

      In its current implementation, getMinValue supports bit widths of up to
      sizeof( signed long long ) * 8. If larger bit widths are given,
      getMinValue asserts.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    static signed long long getMinValue( unsigned int bitwidth )
    {
      DSTART(
        "static long long int WIR_ImmediateParameter<DerivedClass, true>::getMinValue(unsigned int)" );

      // Ensure that the bit width is sizeof( signed long long ) * 8 at most.
      ufAssert(
        ( bitwidth > 0 ) && ( bitwidth <= sizeof( signed long long ) * 8 ) );

      // Construct the minimal signed value by setting only the most-significant
      // bit to '1'.
      signed long long result { 1 };
      for ( unsigned int i = 0; i < bitwidth - 1; ++i, result <<= 1 ) ;

      return( -result );
    };

    /*!
      @brief getMaxValue returns the maximal signed value that can be
             represented in two's-complement with the specified amount of bits.

      @param[in] bitwidth The number of available bits.
      @return The maximal representable value.

      In its current implementation, getMaxValue supports bit widths of up to
      sizeof( signed long long ) * 8. If larger bit widths are given,
      getMaxValue asserts.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    static signed long long getMaxValue( unsigned int bitwidth )
    {
      DSTART(
        "static long long int WIR_ImmediateParameter<DerivedClass, true>::getMaxValue(unsigned int)" );

      // Ensure that the bit width is sizeof( signed long long ) * 8 at most.
      ufAssert(
        ( bitwidth > 0 ) && ( bitwidth <= sizeof( signed long long ) * 8 ) );

      // Construct the maximal signed value by setting all bits except the most-
      // significant bit to '1'.
      signed long long result { 0 };
      unsigned int bitMask { 1 };
      for ( unsigned int i = 0; i < bitwidth - 1; ++i, bitMask <<= 1 )
        result |= bitMask;

      return( result );
    };


    //
    // Stream I/O.
    //

    /*!
      @brief getValueString returns a string containing an immediate
             parameters's value.

      @return A string with the immediate's value.

      This method is internally used for doing I/O dumps of immediate
      parameters.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual std::string getValueString( void ) const
    {
      DSTART(
        "string WIR_ImmediateParameter<DerivedClass, true>::getValueString() const" );

      std::stringstream str;
      str << mValue;

      return( str.str() );
    };


  private:

    /*!
      @brief No standard construction allowed, users must use
             WIR_ImmediateParameter( signed long long, unsigned int ) instead.
    */
    WIR_ImmediateParameter( void ) = delete;

    /*!
      @brief clone creates a copy of a signed immediate parameter.

      @return A pointer to the newly created copy of this parameter.

      Using the template argument, clone just calls the correct copy constructor
      of the actual immediate parameter class.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_BaseImmediateParameter *clone( void ) const
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      return(
        new DerivedClass( *( static_cast<const DerivedClass *>( this ) ) ) );
    };

    //! mValue stores a signed immediate parameter's actual value.
    signed long long mValue;

};


/*!
  @brief The specialized template class
         WIR_ImmediateParameter<DerivedClass, false> models generic unsigned
         immediate parameters.

  @tparam DerivedClass This template argument is used to specify the actual type
                       of immediate parameter to be instantiated.
  @tparam Signed This Boolean template argument is used to distinguish between
                 signed and unsigned immediates in template instantiations. Set
                 it to true if you instantiate a signed immediate class,
                 otherwise use false.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
template <typename DerivedClass>
// cppcheck-suppress copyCtorAndEqOperator
class WIR_ImmediateParameter<DerivedClass, false> : public WIR_GenericImmediateParameter<DerivedClass, false>
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor for unsigned immediate parameters of the
             specified bit width.

      @param[in] __i The immediate value.
      @param[in] __w The number of available bits.

      The constructor ensures that __i lies in the range of values that can be
      represented by the parameter's bit width.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_ImmediateParameter( unsigned long long __i, unsigned int __w ) :
      WIR_GenericImmediateParameter<DerivedClass, false> { __w }
    {
      DSTART(
        "WIR_ImmediateParameter<DerivedClass, false>::WIR_ImmediateParameter(long long unsigned int, unsigned int)" );

      setValue( __i );
    };

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_ImmediateParameter( const WIR_ImmediateParameter &__o ) :
      WIR_GenericImmediateParameter<DerivedClass, false> { __o },
      mValue { __o.mValue }
    {
      DSTART(
        "WIR_ImmediateParameter<DerivedClass, false>::WIR_ImmediateParameter(const WIR_ImmediateParameter<DerivedClass, false>&)" );
    };

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_ImmediateParameter( WIR_ImmediateParameter &&__o ) :
      WIR_GenericImmediateParameter<DerivedClass, false> { std::move( __o ) },
      mValue { std::move( __o.mValue ) }
    {
      DSTART(
        "WIR_ImmediateParameter<DerivedClass, false>::WIR_ImmediateParameter(WIR_ImmediateParameter<DerivedClass, false>&&)" );

      __o.mValue = 0;
    };

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_ImmediateParameter( void )
    {
      DSTART(
        "WIR_ImmediateParameter<DerivedClass, false>::~WIR_ImmediateParameter()" );
    };

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    DerivedClass & operator = ( const DerivedClass &__o )
    {
      DSTART(
        "DerivedClass& WIR_ImmediateParameter<DerivedClass, false>::operator=(const DerivedClass&) " );

      WIR_GenericImmediateParameter<DerivedClass, false>::operator = ( __o );

      mValue = __o.mValue;

      return( static_cast<DerivedClass &>( *this ) );
    };

    /*!
      @brief Move-assignment operator.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    DerivedClass & operator = ( DerivedClass &&__o )
    {
      DSTART(
        "DerivedClass& WIR_ImmediateParameter<DerivedClass, false>::operator=(DerivedClass&&)" );

      WIR_GenericImmediateParameter<DerivedClass, false>::operator = (
        std::move( __o ) );

      mValue = std::move( __o.mValue );
      __o.mValue = 0;

      return( static_cast<DerivedClass &>( *this ) );
    };


    //
    // Value handling.
    //

    /*!
      @brief setValue sets an unsigned immediate parameter's actual value.

      @param[in] i The parameter's new immediate value.

      setValue ensures that i lies in the range of values that can be
      represented by the parameter's bit width.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void setValue( unsigned long long i )
    {
      DSTART(
        "void WIR_ImmediateParameter<DerivedClass, false>::setValue(long long unsigned int)" );

      WIR_Parameter::checkDontOptimize();

      // Ensure that i lies in the range of values that can be represented by
      // the parameter's bit width.
      unsigned long long maxValue = getMaxValue(
        WIR_BaseImmediateParameter::mBitWidth );

      ufAssertT(
        ( i <= maxValue ),
        "Immediate value " << i << " is not within the interval [0, " <<
        maxValue << "] of " << WIR_BaseImmediateParameter::mBitWidth <<
        "-bit unsigned values." );

      mValue = i;
    };

    /*!
      @brief getValue gets an unsigned immediate parameter's value.

      @return The parameter's current value.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    unsigned long long getValue( void ) const
    {
      DSTART(
        "long long unsigned int WIR_ImmediateParameter<DerivedClass, false>::getValue() const" );

      return( mValue );
    };

    /*!
      @brief getSignedValue gets an unsigned immediate parameter's value.

      @return Nothing

      Since this class models unsigned immediate parameters, getSignedValue
      simply fails with an assertion.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual signed long long getSignedValue( void ) const
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      ufAssert( false );
      return( 0 );
    };

    /*!
      @brief getUnsignedValue gets an unsigned immediate parameter's value.

      @return The parameter's current unsigned value.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual unsigned long long getUnsignedValue( void ) const
    {
      DSTART(
        "long long unsigned int WIR_ImmediateParameter<DerivedClass, false>::getUnsignedValue() const" );

      return( getValue() );
    };

    /*!
      @brief getMaxValue returns the maximal unsigned value that can be
             represented with the specified amount of bits.

      @param[in] bitwidth The number of available bits.
      @return The maximal representable value.

      In its current implementation, getMaxValue supports bit widths of up to
      sizeof( unsigned long long ) * 8. If larger bit widths are given,
      getMaxValue asserts.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    static unsigned long long getMaxValue( unsigned int bitwidth )
    {
      DSTART(
        "static long long unsigned int WIR_ImmediateParameter<DerivedClass, false>::getMaxValue(unsigned int)" );

      // Ensure that the bit width is sizeof( unsigned long long ) * 8 at most.
      ufAssert(
        ( bitwidth > 0 ) && ( bitwidth <= sizeof( unsigned long long ) * 8 ) );

      // Construct the maximal unsigned value by setting all bits up to the
      // given width to '1'.
      unsigned long long result { 0 };
      unsigned int bitMask { 1 };
      for ( unsigned int i = 0; i < bitwidth; ++i, bitMask <<= 1 )
        result |= bitMask;

      return( result );
    };


    //
    // Stream I/O.
    //

    /*!
      @brief getValueString returns a string containing an immediate
             parameters's value.

      @return A string with the immediate's value.

      This method is internally used for doing I/O dumps of immediate
      parameters.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual std::string getValueString( void ) const
    {
      DSTART(
        "string WIR_ImmediateParameter<DerivedClass, false>::getValueString() const" );

      std::stringstream str;
      str << mValue;

      return( str.str() );
    };


  private:

    /*!
      @brief No standard construction allowed, users must use
             WIR_ImmediateParameter( unsigned long long, unsigned int ) instead.
    */
    WIR_ImmediateParameter( void ) = delete;

    /*!
      @brief clone creates a copy of an unsigned immediate parameter.

      @return A pointer to the newly created copy of this parameter.

      Using the template argument, clone just calls the correct copy constructor
      of the actual immediate parameter class.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_BaseImmediateParameter *clone( void ) const
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      return(
        new DerivedClass( *( static_cast<const DerivedClass *>( this ) ) ) );
    };

    //! mValue stores an unsigned immediate parameter's actual value.
    unsigned long long mValue;

};

}       // namespace WIR

#endif  // _WIR_IMMEDIATEPARAMETER_H
