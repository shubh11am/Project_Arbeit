/*

   This header file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Intermediate Representation Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2015 - 2022, Heiko Falk, Timon Kelter.

   Hamburg University of Technology (TUHH)
   Institute of Embedded Systems
   21071 Hamburg
   Germany

   http://www.tuhh.de/es/esd/research/wcc

*/

/*!
  @file wirinterval.h
  @brief This file provides the basic interface of generic intervals of values.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_INTERVAL_H
#define _WIR_INTERVAL_H


//
// Include section
//

// Include standard headers
#include <initializer_list>
#include <iostream>
#include <limits>
#include <sstream>
#include <stdexcept>
#include <type_traits>
#include <utility>

// Include boost headers
#include <boost/current_function.hpp>
#include <boost/iterator/counting_iterator.hpp>
#include <boost/range/iterator_range.hpp>

// Include libuseful headers
#include <libuseful/debugmacros.h>
#include <libuseful/io.h>

// Include WIR headers
#include <wir/wirmisc.h>
#include <wir/wirtypes.h>
#include <analyses/generic/wirintervalset.h>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

template<typename IntegerType, WIR_IntervalCompression CompType>
class WIR_IntervalSet;


/*!
  @brief Class WIR_Interval represents an interval.

  @tparam BaseType The type of the values to be used for an interval.

  Unlike boost::interval, this class is tailored towards integral intervals
  to be used in the approximation of integral values and interval arithmetic.

  @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
*/
template<typename BaseType>
class WIR_Interval
{

  public:

    //
    // Local type definitions.
    //

    //! The class' own type.
    using intervalType = WIR_Interval<BaseType>;

    //! The base type of an interval.
    using baseType = BaseType;

    //! The iterator type to iterate over elements of an interval.
    using iterator = boost::counting_iterator<baseType>;

    //! The type of sets of intervals.
    using intervalSet =
      WIR_IntervalSet<BaseType, WIR_IntervalCompression::none>;


    //
    // Constructors and destructors.
    //

    /*!
      @brief Constructor producing the invalid interval [1,0].

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    WIR_Interval( void ) :
      mLower { 1 },
      mUpper { 0 }
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );
    };

    /*!
      @brief Constructor creating the interval [constant, constant].

      @param[in] constant The value to be used as interval.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    explicit WIR_Interval( baseType constant ) :
      mLower { constant },
      mUpper { constant }
    {
      DSTART(
        "WIR_Interval<BaseType>::WIR_Interval(WIR_Interval<BaseType>::baseType)" );
    };

    /*!
      @brief Constructor creating the interval [l, u].

      @param[in] l The lower bound for the interval.
      @param[in] u The upper bound for the interval.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    WIR_Interval( baseType l, baseType u ) :
      mLower { l },
      mUpper { u }
    {
      DSTART(
        "WIR_Interval<BaseType>::WIR_Interval(WIR_Interval<BaseType>::baseType, WIR_Interval<BaseType>::baseType)" );
    };

    /*!
      @brief Constructor creating the interval [l, u] where l and u are the
             first two elements of an initializer list.

      @param[in] l A const reference to an initializer list.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    template<typename IntType>
    explicit WIR_Interval( const std::initializer_list<IntType> &l )
    {
      DSTART(
        "WIR_Interval<BaseType>::WIR_Interval(const initializer_list<IntType>&)" );

      auto it = l.begin();
      if ( l.size() != 2 )
        throw std::invalid_argument(
          "A modulo interval init list needs exactly 2 arguments" );
      else {
        mLower = *it++;
        mUpper = *it;
      }
    };

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    // cppcheck-suppress noExplicitConstructor
    WIR_Interval( const intervalType &__o ) :
      mLower( __o.mLower ),
      mUpper( __o.mUpper )
    {
      DSTART( "WIR_Interval<BaseType>::WIR_Interval(const intervalType&)" );
    };

    /*!
      @brief Copy constructor with specific base type.

      @param[in] __o A const reference to another object to be copied.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    template<typename IntType>
    WIR_Interval( const WIR_Interval<IntType> &__o ) :
      mLower( __o.mLower ),
      mUpper( __o.mUpper )
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );
    };

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    // cppcheck-suppress noExplicitConstructor
    WIR_Interval( intervalType &&__o ) :
      mLower( __o.mLower ),
      mUpper( __o.mUpper )
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );
    };

    /*!
      @brief Move constructor with specific base type.

      @param[in] __o An R-value reference to another object to be moved.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    template<typename IntType>
    WIR_Interval( WIR_Interval<IntType> &&__o ) :
      mLower( __o.mLower ),
      mUpper( __o.mUpper )
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );
    };

    /*!
      @brief Destructor.
      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    virtual ~WIR_Interval()
    {
      DSTART( "WIR_Interval<BaseType>::~WIR_Interval()" );
    };


    //
    // Universe / empty set.
    //

    /*!
      @brief whole creates an interval from minimal to maximal value of base
             type.

      @return A maximal interval for the specified base type.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    static intervalType whole( void )
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      static_assert(
        std::numeric_limits<baseType>::is_specialized,
        "std::numeric_limits is not specialized for this baseType!" );
      return(
        intervalType(
          std::numeric_limits<baseType>::min(),
          std::numeric_limits<baseType>::max() ) );
    };

    /*!
      @brief empty creates an empy interval of base type.

      @return An empty interval for the specified base type.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    static intervalType empty( void )
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      return( intervalType() );
    };


    //
    // Getters / setters.
    //

    /*!
      @brief getLower returns the lower bound of the interval.

      @return The interval's lower bound.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    baseType getLower( void ) const
    {
      DSTART(
        "WIR_Interval<BaseType>::baseType WIR_Interval<BaseType>::getLower() const" );

      return( mLower );
    };

    /*!
      @brief setLower sets the lower bound of the interval.

      @param[in] lower The new lower bound of the interval.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    void setLower( baseType lower )
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      mLower = lower;
    };

    /*!
      @brief getUpper returns the upper bound of the interval.

      @return The interval's upper bound.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    baseType getUpper( void ) const
    {
      DSTART(
        "WIR_Interval<BaseType>::baseType WIR_Interval<BaseType>::getUpper() const" );

      return( mUpper );
    };

    /*!
      @brief setUpper sets the upper bound of the interval.

      @param[in] upper The new upper bound of the interval.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    void setUpper( baseType upper )
    {
      DSTART(
        "void WIR_Interval<BaseType>::setUpper(WIR_Interval<BaseType>::baseType)" );

      mUpper = upper;
    };

    /*!
      @brief getWidth returns the width of the interval (number of elements).

      @return The interval's width.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    baseType getWidth( void ) const
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      if ( isValid() )
        return( mLower - mUpper + 1 );

      return( baseType( 0 ) );
    };

    /*!
      @brief isZero returns whether the interval is equal to [0, 0].

      @return True if the interval is equal to [0, 0], false otherwise.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    bool isZero( void ) const
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      return( ( mLower == 0 ) && ( mUpper == 0 ) );
    };

    /*!
      @brief isValid returns whether the interval is valid.

      @return True if the interval is valid, false otherwise.

      For a valid interval, lower bound <= upper bound holds.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    bool isValid( void ) const
    {
      DSTART( "bool WIR_Interval<BaseType>::isValid() const" );

      return( mLower <= mUpper );
    };

    /*!
      @brief isEmpty returns whether the interval is empty.

      @return True if the interval is empty, false otherwise.

      An interval is empty if mLower > mUpper holds.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    bool isEmpty( void ) const
    {
      DSTART( "bool WIR_Interval<BaseType>::isEmpty() const" );

      return( mLower > mUpper );
    };

    /*!
      @brief isSingleton returns whether the interval contains only one point.

      @return True if the interval is a singleton, false otherwise.

      An interval is a singleton if mLower == mUpper holds.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    bool isSingleton( void ) const
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      return( mLower == mUpper );
    };

    /*!
      @brief setInvalid sets an interval to an invalid/empty value.

      Internally, the interval is set to [1, 0].

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    void setInvalid( void )
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      mLower = 1;
      mUpper = 0;
    };


    //
    // Iterators.
    //

    /*!
      @brief begin returns an iterator to the beginning of this interval.

      @return An iterator pointing to the first element of the interval.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    iterator begin( void ) const
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      if ( !isValid() )
        throw std::runtime_error( "An invalid interval cannot be iterated!" );

      return( iterator( mLower ) );
    };

    /*!
      @brief end returns an iterator to the end of this interval.

      @return An iterator pointing to position after the last element of this
              interval.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    iterator end( void ) const
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      if ( !isValid() )
        throw std::runtime_error( "In invalid interval cannot be iterated!" );

      return( iterator( mUpper + 1 ) );
    };


    //
    // Helper macros.
    //

    // We specify all of the following operations for operands of type
    // "reference to interval". To also work with initializer lists, we use the
    // following macros to define wrapper operations.
    // The same wrappers are also defined for single values.

    #define WRAPPER_BASE( type, opname, body, constfunc ) \
      template<typename IntType> \
      type opname ( const std::initializer_list<IntType> &l ) constfunc \
      { \
        body; \
      }; \
      template<typename IntType> type opname ( const IntType &l ) constfunc \
      { \
        body; \
      };

    #define METHOD_RETURN_WRAPPERS( return_type, methodname, constfunc ) \
      WRAPPER_BASE( \
        return_type, methodname, return methodname( WIR_Interval( l ) ), \
        constfunc )
    #define METHOD_NORETURN_WRAPPERS( methodname, constfunc ) \
      WRAPPER_BASE( \
        void, methodname, methodname( WIR_Interval( l ) ), constfunc )
    #define OPERATOR_RETURN_WRAPPERS( return_type, operatorname, constfunc ) \
      WRAPPER_BASE( \
        return_type, operator operatorname, \
        return( *this operatorname WIR_Interval( l ) ), constfunc )


    //
    // Comparison operators (== , !=, <, <=, >, >=).
    //

    /*!
      @brief This operator compares two WIR_Intervals for equality.

      @param[in] i A const reference to the operand to be compared with.
      @return true iff both operands are equal, false otherwise.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    template<typename IntType>
    bool operator == ( const WIR_Interval<IntType> &i ) const
    {
      DSTART(
        "bool WIR_Interval<BaseType>::operator==(const WIR_Interval<IntType>&) const" );

      return(
        ( isEmpty() && i.isEmpty() ) ||
        ( ( mLower == i.getLower() ) && ( mUpper == i.getUpper() ) ) );
    };
    OPERATOR_RETURN_WRAPPERS( bool, ==, const );

    /*!
      @brief This operator compares two WIR_Intervals for inequality.

      @param[in] i A const reference to the operand to be compared with.
      @return true iff both operands are inequal, false otherwise.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    template<typename IntType>
    bool operator != ( const WIR_Interval<IntType> &i ) const
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      return( !( *this == i ) );
    };
    OPERATOR_RETURN_WRAPPERS( bool, !=, const );

    /*!
      @brief This operator performs a less-than comparison of two WIR_Intervals.

      @param[in] i A const reference to the operand to be compared with.
      @return true iff the left operand is less than the right one, false
              otherwise.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    template<typename IntType>
    bool operator < ( const WIR_Interval<IntType> &i ) const
    {
      DSTART(
        "bool WIR_Interval<BaseType>::operator<(const WIR_Interval<IntType>&) const" );

      checkValidity( *this, i );
      return( mUpper < i.mLower );
    };
    OPERATOR_RETURN_WRAPPERS( bool, <, const );

    /*!
      @brief This operator performs a less-equal comparison of two
             WIR_Intervals.

      @param[in] i A const reference to the operand to be compared with.
      @return true iff the left operand is less than or equal to the right one,
              false otherwise.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    template<typename IntType>
    bool operator <= ( const WIR_Interval<IntType> &i ) const
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      checkValidity( *this, i );
      return( mUpper <= i.mLower );
    };
    OPERATOR_RETURN_WRAPPERS( bool, <=, const );

    /*!
      @brief This operator performs a greater-than comparison of two
             WIR_Intervals.

      @param[in] i A const reference to the operand to be compared with.
      @return true iff the left operand is greater than the right one, false
              otherwise.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    template<typename IntType>
    bool operator > ( const WIR_Interval<IntType> &i ) const
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      return( i < *this );
    };
    OPERATOR_RETURN_WRAPPERS( bool, >, const );

    /*!
      @brief This operator performs a greater-equal comparison of two
             WIR_Intervals.

      @param[in] i A const reference to the operand to be compared with.
      @return true iff the left operand is greater than or equal to the right
              one, false otherwise.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    template<typename IntType>
    bool operator >= ( const WIR_Interval<IntType> &i ) const
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      return( i <= *this );
    };
    OPERATOR_RETURN_WRAPPERS( bool, >=, const );


    //
    // Assignment operators.
    //

    /*!
      @brief Copy-assignment operator.

      @param[in] i A const reference to another object to be copied.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    intervalType & operator = ( const intervalType &i )
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      mLower = i.getLower();
      mUpper = i.getUpper();
      return( *this );
    };

    /*!
      @brief Copy-assignment operator with specific base type.

      @param[in] i A const reference to another object to be copied.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    template<typename IntType>
    intervalType & operator = ( const WIR_Interval<IntType> &i )
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      mLower = i.getLower();
      mUpper = i.getUpper();
      return( *this );
    };

    /*!
      @brief Move-assignment operator.

      @param[in] i An R-value reference to another object to be moved.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    intervalType & operator = ( intervalType &&i )
    {
      DSTART(
        "WIR_Interval<BaseType>::intervalType& WIR_Interval<BaseType>::operator=(WIR_Interval<BaseType>::intervalType&&)" );

      mLower = std::move( i.getLower() );
      mUpper = std::move( i.getUpper() );
      return( *this );
    };

    /*!
      @brief Move-assignment operator with specific base type.

      @param[in] i An R-value reference to another object to be moved.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    template<typename IntType>
    intervalType & operator = ( WIR_Interval<IntType> &&i )
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      mLower = std::move( i.getLower() );
      mUpper = std::move( i.getUpper() );
      return( *this );
    };
    OPERATOR_RETURN_WRAPPERS( intervalType &, =, );


    //
    // Arithmetical operators (unary -, +, +=, -, -=, *, *=, /, /=, %, %=).
    //

    /*!
      @brief Unary minus operator.

      @return An interval containing the negation of this interval.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    intervalType operator - ( void ) const
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      return( intervalType( -mUpper, -mLower ) );
    };

    /*!
      @brief Addition operator.

      @param[in] i A const reference to another object to be added.
      @return An interval containing the sum of the two intervals.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    template<typename IntType>
    intervalType operator + ( const WIR_Interval<IntType> &i ) const
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      intervalType clone( *this );
      return( clone += i );
    };
    OPERATOR_RETURN_WRAPPERS( intervalType, +, const );

    /*!
      @brief Addition-assignment operator.

      @param[in] i A const reference to another object to be added.
      @return A reference to this interval containing the sum.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    template<typename IntType>
    intervalType & operator += ( const WIR_Interval<IntType> &i )
    {
      DSTART(
        "WIR_Interval<BaseType>::intervalType& WIR_Interval<BaseType>::operator+=(const WIR_Interval<IntType>&)" );

      checkValidity( *this, i );
      mLower += i.getLower();
      mUpper += i.getUpper();
      return( *this );
    };
    OPERATOR_RETURN_WRAPPERS( intervalType &, += , );

    /*!
      @brief Subtraction operator.

      @param[in] i A const reference to another object to be subtracted.
      @return An interval containing the difference of the two intervals.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    template<typename IntType>
    intervalType operator - ( const WIR_Interval<IntType> &i ) const
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      intervalType clone( *this );
      return( clone -= i );
    };
    OPERATOR_RETURN_WRAPPERS( intervalType, -, const );

    /*!
      @brief Subtraction-assignment operator.

      @param[in] i A const reference to another object to be subtracted.
      @return A reference to this interval containing the difference.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    template<typename IntType>
    intervalType & operator -= ( const WIR_Interval<IntType> &i )
    {
      DSTART(
        "WIR_Interval<BaseType>::intervalType& WIR_Interval<BaseType>::operator-=(const WIR_Interval<IntType>&)" );

      checkValidity( *this, i );
      mLower -= i.getUpper();
      mUpper -= i.getLower();
      return( *this );
    };
    OPERATOR_RETURN_WRAPPERS( intervalType &, -=, );

    /*!
      @brief Multiplication operator.

      @param[in] i A const reference to another object to be multiplied.
      @return An interval containing the product of the two intervals.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    template<typename IntType>
    intervalType operator * ( const WIR_Interval<IntType> &i ) const
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      intervalType clone( *this );
      return( clone *= i );
    };
    OPERATOR_RETURN_WRAPPERS( intervalType, *, const );

    /*!
      @brief Multiplication-assignment operator.

      @param[in] i A const reference to another object to be multiplied.
      @return A reference to this interval containing the product.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    template<typename IntType>
    intervalType &operator *= ( const WIR_Interval<IntType> &i )
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      checkValidity( *this, i );
      const baseType ll( this->getLower() ), lu( this->getUpper() ),
                     rl( i.getLower() ), ru( i.getUpper() );
      const baseType v1( ll * rl ), v2( ll * ru ), v3( lu * rl ), v4( lu * ru );
      mLower = std::min( std::min( v1, v2 ), std::min( v3, v4 ) );
      mUpper = std::max( std::max( v1, v2 ), std::max( v3, v4 ) );
      return( *this );
    };
    OPERATOR_RETURN_WRAPPERS( intervalType &, *= , );

    /*!
      @brief Division operator.

      @param[in] divisor A const reference to the divisor.
      @return An interval containing the quotient of the two intervals.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    template<typename IntType>
    intervalType operator / ( const WIR_Interval<IntType> &divisor ) const
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      intervalType clone( *this );
      return( clone /= divisor );
    };
    OPERATOR_RETURN_WRAPPERS( intervalType, /, const );

    /*!
      @brief Division-assignment operator.

      @param[in] divisor A const reference to the divisor.
      @return A reference to this interval containing the quotient.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    template<typename IntType>
    intervalType &operator /= ( const WIR_Interval<IntType> &divisor )
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      checkValidity( *this, divisor );

      if ( divisor.contains( 0 ) )
        // We cannot represent such intervals currently. Using "whole()" is
        // not an option, since successive operations would then perhaps move
        // the boundaries of whole() and lead an underestimation, since the
        // true boundaries were (-infinite,+infinite). Consider
        //   ( [ 1, 2 ] / [ 0 ] ) + [ 1 ] = [ min + 1, max ]
        // which is simply wrong behavior. Therefore, we need an explicit
        // representation of infinity or throw an exception.
        throw std::invalid_argument( "Division by zero!" );
      else {
        // Mathematically, this is: [a,b]/[c,d] = [a,b]*[1/c,1/d]
        // since we must be able to cope with integral types, we must resolve
        // the multiplication here, 1/c and 1/d would be zero in all cases.
        const baseType ll( this->getLower() ), lu( this->getUpper() ),
                       rl( divisor.getLower() ), ru( divisor.getUpper() );
        const baseType v1( ll / rl ), v2( ll / ru ), v3( lu / rl ),
                       v4( lu / ru );
        mLower = std::min( std::min( v1, v2 ), std::min( v3, v4 ) );
        mUpper = std::max( std::max( v1, v2 ), std::max( v3, v4 ) );
      }
      return( *this );
    };
    OPERATOR_RETURN_WRAPPERS( intervalType &, /= , );

    /*!
      @brief Modulo operator.

      @param[in] divisor A const reference to the divisor.
      @return An interval containing the modulus of the two intervals.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    template<typename IntType>
    intervalType operator % ( const WIR_Interval<IntType> &divisor ) const
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      intervalType clone( *this );
      return( clone %= divisor );
    };
    OPERATOR_RETURN_WRAPPERS( intervalType, %, const );

    /*!
      @brief Modulo-assignment operator.

      @param[in] divisor A const reference to the divisor.
      @return A reference to this interval containing the modulus.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    template<typename IntType>
    intervalType & operator %= ( const WIR_Interval<IntType> &divisor )
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      checkValidity( *this, divisor );

      // TODO: This can certainly be made faster.
      intervalType result;
      for ( baseType l : *this )
        for ( baseType r : divisor )
          result.unite( l % r );
      *this = result;
      return( *this );
    };
    OPERATOR_RETURN_WRAPPERS( intervalType &, %=, );


    //
    // Miscellaneous.
    //

    /*!
      @brief hasEmptyIntersection returns whether two intervals do not
             intersect.

      @param[in] i A const reference to another interval.
      @return true iff both intervals intersect, false otherwise.

      hasEmptyIntersection performs an efficient test for empty intersection
      without actually computing the intersection.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    template<typename IntType>
    bool hasEmptyIntersection( const WIR_Interval<IntType> &i ) const
    {
      DSTART(
        "bool WIR_Interval<BaseType>::hasEmptyIntersection(const WIR_Interval<IntType>&) const" );

      return(
        isEmpty() || i.isEmpty() ||
        mUpper < i.mLower ||            // *this is "before" i
        mLower > i.mUpper );            // *this is "after" i
    }
    METHOD_RETURN_WRAPPERS( bool, hasEmptyIntersection, const );

    /*!
      @brief unite computes the union of two intervals.

      @param[in] i A const reference to another object to be united.

      unite works in-place, i.e., it modifies "this".

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    template<typename IntType> void unite( const WIR_Interval<IntType> &i )
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      // Treat empty intervals specially, otherwise unifying [2,1] and [5,4]
      // would yield [2,4] in the other if branch.
      if ( i.isEmpty() )
        return;
      else

      if ( isEmpty() )
        *this = i;
      else {
        mLower = std::min( mLower, i.getLower() );
        mUpper = std::max( mUpper, i.getUpper() );
      }
    };
    METHOD_NORETURN_WRAPPERS( unite, );

    /*!
      @brief unification computes the union of two intervals.

      @param[in] i A const reference to another object.
      @return An interval containing the union.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    template<typename IntType>
    intervalType unification( const WIR_Interval<IntType> &i ) const
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      intervalType clone( *this );
      clone.unite( i );
      return( clone );
    };
    METHOD_RETURN_WRAPPERS( intervalType, unification, const );

    /*!
      @brief intersect computes the intersection of two intervals.

      @param[in] i A const reference to another object to be intersected.

      intersect works in-place, i.e., it modifies "this".

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    template<typename IntType> void intersect( const WIR_Interval<IntType> &i )
    {
      DSTART(
        "void WIR_Interval<BaseType>::intersect(const WIR_Interval<IntType>&)" );

      // Treat empty intervals specially, otherwise intersecting [4,3] and [5,4]
      // would yield [4,4] in the other if branch.
      if ( isEmpty() )
        return;
      else

      if ( i.isEmpty() )
        *this = i;
      else {
        mLower = std::max( mLower, i.mLower );
        mUpper = std::min( mUpper, i.mUpper );
      }
    };
    METHOD_NORETURN_WRAPPERS( intersect, );

    /*!
      @brief intersection computes the intersection of two intervals.

      @param[in] i A const reference to another object.
      @return An interval containing the intersection.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    template<typename IntType>
    intervalType intersection( const WIR_Interval<IntType> &i ) const
    {
      DSTART(
        "WIR_Interval<BaseType>::intervalType WIR_Interval<BaseType>::intersection(const WIR_Interval<IntType>&) const" );

      intervalType clone( *this );
      clone.intersect( i );
      return( clone );
    };
    METHOD_RETURN_WRAPPERS( intervalType, intersection, const );

    /*!
      @brief remove removes the given interval from the current one.

      @param[in] i A const reference to another object to be removed.

      remove works in-place, i.e., it modifies "this".
      Removal is only possible, if the difference of "this" and @a i has less
      than 2 elements. Otherwise, there would be more than one interval
      remaining as the result of the subtraction.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    template<typename IntType> void remove( const WIR_Interval<IntType> &i )
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      if ( i.isValid() && isValid() ) {
        if ( i.contains( *this ) )
          setInvalid();
         else {
          auto diff = difference( i );
          ufAssertT(
            !diff.isEmpty(), "The intervals must have a difference "
            "due to the above condition tests!" );

          if ( diff.getIntervals().size() == 1 )
            *this = *diff.getIntervals().begin();
          else
            throw std::range_error( "Interval cannot be removed" );
        }
      }
    };
    METHOD_NORETURN_WRAPPERS( remove, );

    /*!
      @brief without computes the interval that results when i is removed from
             this.

      @param[in] i A const reference to another object to be removed.

      @see remove

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    template<typename IntType>
    intervalType without( const WIR_Interval<IntType> &i ) const
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      intervalType clone( *this );
      clone.remove( i );
      return( clone );
    };
    METHOD_RETURN_WRAPPERS( intervalType, without, const );

    /*!
      @brief difference computes the remaining intervals that result when the
             given interval is removed from the current one.

      @param[in] i A const reference to another object to be removed.
      @return An interval set containing the difference.

      If there is no interval remaining after the removal (i.e., i contains this
      interval), then the returned interval set is empty.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    template<typename IntType>
    intervalSet difference( const WIR_Interval<IntType> &i ) const
    {
      DSTART(
        "WIR_Interval<BaseType>::intervalSet WIR_Interval<BaseType>::difference(const WIR_Interval<IntType>&) const" );

      intervalSet result;

      if ( !i.isValid() )
        result.unite( *this );
      else

      if ( !isValid() ) {
        // No result
      } else {
        ufAssert ( i.isValid() && isValid() );

        if ( i.getLower() <= mLower ) {

          // Acronyms:
          //   two spaces per interval element,
          //   i for element of i
          //   t for element of this

          if ( i.getUpper() < mLower )
            // Case: ----- i i i i i  t t t t t t t t
            result.unite( *this );
          else

          if ( i.getUpper() < mUpper )
            // Case: ----- i i i i itititit t t t t t
            result.unite(
              WIR_Interval(
                { static_cast<baseType>( i.getUpper() + 1 ), mUpper } ) );
           else {
            // Case: ----- i i i i itititititititititi i i i
          }
        } else {

          if ( i.getUpper() < mUpper ) {
            // Case: ----- t t t t ititititit t t t t
            result.unite(
              WIR_Interval(
                { mLower, static_cast<baseType>( i.getLower() - 1 ) } ) );
            result.unite(
              WIR_Interval(
                { static_cast<baseType>( i.getUpper() + 1 ), mUpper } ) );
          } else

          if ( i.getLower() <= mUpper ) {
            // Case: ----- t t t t itititititititititi i i i
            result.unite(
              WIR_Interval(
                { mLower, static_cast<baseType>( i.getLower() - 1 ) } ) );
          } else
            // Case: ----- t t t t     i i i i i i i i i i i
            result.unite( *this );
        }
      }
      return( result );
    };
    METHOD_RETURN_WRAPPERS( intervalSet, difference, const );

    /*!
      @brief symmetricDifference computes the symmetric difference with the
             given interval.

      @param[in] i A const reference to the second operand for the symmetric
                   difference.
      @return An interval set containing one or two intervals which constitute
              the symmetric difference of "this" and @a i. This set may not join
              chunks, since symdiff([0,1], [2,3]) = {[0,1], [2,3]} and not
              {[0,3]} which would result if the result type joined neighbor
              chunks.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    template<typename IntType>
    intervalSet symmetricDifference( const WIR_Interval<IntType> &i ) const
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      // If the intervals do not overlap, return both. Otherwise, compute
      // union \ intersection. Doing the latter in all cases is invalid, since
      // the union may /add/ previously non-contained elements for
      // non-overlapping intervals.
      if ( hasEmptyIntersection( i ) ) {
        intervalSet result;
        if ( !isEmpty() )
          result.unite( *this );
        if ( !i.isEmpty() )
          result.unite( i );
        return( result );
      } else {
        intervalType united = unification( i );
        intervalType intersected = intersection( i );
        intervalSet symDiff = united.difference( intersected );
        return( symDiff );
      }
    };
    METHOD_RETURN_WRAPPERS( intervalSet, symmetricDifference, const );

    /*!
      @brief contains computes whether this interval fully contains @a i

      @param[in] i A const reference to another object.
      @return True iff this interval fully contains i, false otherwise.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    template<typename IntType> bool contains( const WIR_Interval<IntType> &i ) const
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      if ( !i.isValid() )
        return( true );
      else

      if ( !isValid() )
        return( false );
      else
        return( mLower <= i.getLower() && mUpper >= i.getUpper() );
    };
    METHOD_RETURN_WRAPPERS( bool, contains, const );

    /*!
      @brief translate translates this interval to another base.

      @param[in] i A const reference to another object.

      This can be used, e.g., when the interval represents a time interval for
      an event and one wants to know how much time has passed since a previous
      event also bounded by another interval.

      Effectively, this method just subtracts the lower bounds and the upper
      bounds. Note that this is not the same as applying the operator -, since
      this one will subtract the bounds crosswise.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    template<typename IntType> void translate( const WIR_Interval<IntType> &i )
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      if ( !isEmpty() && i.isEmpty() )
        return;
      else

      if ( isEmpty() && !i.isEmpty() )
        throw std::invalid_argument( "Translation base may not be empty!" );
      else {
        mLower -= i.getLower();
        mUpper -= i.getUpper();
      }
    };
    METHOD_NORETURN_WRAPPERS( translate, );

    /*!
      @brief translation computes the translation of this interval and @a i.

      @param[in] i A const reference to another object.
      @return A new interval in another base.

      @see translate

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    template<typename IntType>
    intervalType translation( const WIR_Interval<IntType> &i ) const
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      intervalType clone( *this );
      clone.translate( i );
      return( clone );
    };
    METHOD_RETURN_WRAPPERS( intervalType, translation, const );

    #undef WRAPPER_BASE
    #undef METHOD_RETURN_WRAPPERS
    #undef METHOD_NORETURN_WRAPPERS
    #undef OPERATOR_RETURN_WRAPPERS

    /*!
      @brief toString produces a human-readable string representation of an
             interval.

      @return A string representing the interval.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    std::string toString( void ) const {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      std::stringstream out;
      out << *this;
      return( out.str() );
    };


  private:

    /*!
      @brief checkValidity is a helper method that throws an exception if the
             given intervals are invalid.

      @tparam IntType Some basic integer type for the first interval.
      @tparam IntervalTypes The type of the remaining intervals.
      @param[in] i A const reference to the first interval.
      @param[in] args A variadic number of remaining intervals.

      checkValidity uses variadic templates so that argument lists of arbitrary
      lengths can be passed to it.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    template<typename IntType, typename ... IntervalTypes>
    static void checkValidity( const WIR_Interval<IntType> &i,
                               IntervalTypes&&... args )
    {
      DSTART(
        "static void WIR_Interval<BaseType>::checkValidity(const WIR_Interval<IntType>&, IntervalTypes&& ...)" );

      if ( !i.isValid() )
        throw std::runtime_error( "This operation requires valid intervals!" );
      checkValidity( args... );
    };

    /*!
      @brief checkValidity is a helper method that throws an exception if the
             given interval is invalid.

      @tparam IntType Some basic integer type for the first interval.
      @param[in] i A const reference to an interval whose validity is to be
                   checked.

      It only serves to terminate the recursion of the variadic method
      checkValidity above.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    template<typename IntType>
    static void checkValidity( const WIR_Interval<IntType> &i )
    {
      DSTART(
        "static void WIR_Interval<BaseType>::checkValidity(const WIR_Interval<IntType>&)" );

      if ( !i.isValid() )
        throw std::runtime_error( "This operation requires valid intervals!" );
    };

    //! mLower stores an interval's lower bound.
    baseType mLower;

    //! mUpper stores an interval's upper bound.
    baseType mUpper;

};


//
// External operators (for symmetry purposes).
//
// Since we allow operators to take the form
//   interval operatorXY initializer_list
// and
//   interval operatorXY single_value,
// we should allow the symmetric cases, too. To accomplish this, we have to
// define external functions with the following macros.

#define EXTERNAL_OPERATOR_WRAPPER( return_type, operatorname ) \
  /* Normally, this template would be sufficient */ \
  template<typename BaseType, typename IntType> \
  typename std::enable_if<std::is_convertible<IntType, BaseType>::value, \
                          return_type>::type \
  operator operatorname ( const IntType &l, \
                          const WIR_Interval<BaseType> &i ) \
  { \
    return( WIR_Interval<BaseType>( l ) operatorname i ); \
  }; \
  \
  /* This one is only added to avoid compiler warnings */ \
  template<typename BaseType, typename IntType> \
  typename std::enable_if<std::is_convertible<IntType, BaseType>::value, \
                          return_type>::type \
  operator operatorname ( const std::initializer_list<IntType> &l, \
                          const WIR_Interval<BaseType> &i ) \
  { \
    return( WIR_Interval<BaseType>( l ) operatorname i ); \
  };

EXTERNAL_OPERATOR_WRAPPER( bool, == )
EXTERNAL_OPERATOR_WRAPPER( bool, != )
EXTERNAL_OPERATOR_WRAPPER( bool, <= )
EXTERNAL_OPERATOR_WRAPPER( bool, < )
EXTERNAL_OPERATOR_WRAPPER( bool, >= )
EXTERNAL_OPERATOR_WRAPPER( bool, > )
EXTERNAL_OPERATOR_WRAPPER( WIR_Interval<BaseType>, + )
EXTERNAL_OPERATOR_WRAPPER( WIR_Interval<BaseType>, - )
EXTERNAL_OPERATOR_WRAPPER( WIR_Interval<BaseType>, * )
EXTERNAL_OPERATOR_WRAPPER( WIR_Interval<BaseType>, / )
EXTERNAL_OPERATOR_WRAPPER( WIR_Interval<BaseType>, % )

#undef EXTERNAL_OPERATOR_WRAPPER


//
// Stream I/O.
//

/*!
  @brief The << operator dumps a %WIR interval to an output stream.

  @param[in] os A reference to an output stream.
  @param[in] i A const reference to the %WIR interval to be dumped.
  @return A reference to the same output stream.

  @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
*/
template<typename BaseType>
std::ostream &operator << ( std::ostream &os, const WIR_Interval<BaseType> &i )
{
  DSTART(
    "ostream& operator<<(ostream&, const WIR_Interval<IntegerType>&)" );

  if ( i.isEmpty() )
    os << "empty";
  else

  if ( i.getLower() == i.getUpper() )
    // The '+' signs in the output are here to print char types in numeric mode
    // and not as characters. This is a bit cryptic, but it works, since the '+'
    // operator activates a type promotion which lengthens "char" and
    // "unsigned char" to the desired numerical type, which is __not__ printed
    // as a character.
    os << +i.getLower();
  else
    // For the '+' sign: See above.
    os << "[" << +i.getLower() << "-" << +i.getUpper() << "]";

  return( os );
};

}       // namespace WIR

#endif  // _WIR_INTERVAL_H
