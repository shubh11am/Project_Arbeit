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
  @file wirsaturatingint.h
  @brief This file provides the basic interface of integer values with
         saturation.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_SATURATING_INT_H
#define _WIR_SATURATING_INT_H


//
// Include section
//

// Include standard headers
#include <initializer_list>
#include <limits>
#include <stdexcept>
#include <type_traits>

// Include boost headers
#include <boost/current_function.hpp>

// Include libuseful headers
#include <libuseful/debugmacros.h>

// Include WIR headers
#include <analyses/generic/safeint.h>


//
// Header section
//

namespace WIR {

/*!
  @brief Class WIR_SaturatingInt represents an integer number with saturation.

  @tparam BaseType The type of the values to be used for a saturating integer,
                   which has to be an arithmetic type.

  WIR_SaturatingInt supports assignment, addition, multiplication, division and
  modulo computation with arbitrary other integer types or saturating integer
  types.

  @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
*/
template<typename BaseType,
         typename std::enable_if<std::is_arithmetic<BaseType>::value,
                                 bool>::type = true>
class WIR_SaturatingInt
{

  public:

    //
    // Local type definitions.
    //

    //! The instantiated type of this class.
    using self_type = WIR_SaturatingInt<BaseType>;

    //! The base type of this class.
    using base_type = BaseType;


    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    WIR_SaturatingInt( void )
    {
      DSTART( "WIR_SaturatingInt<BaseType>::WIR_SaturatingInt()" );
    };

    /*!
      @brief Constructor generating a saturating integer from some convertible
             integer value.

      @tparam BaseType2 The type of the value to be used for construction of a
                        saturating integer, which has to be convertible to the
                        saturating int's base type.
      @param[in] b A const reference to a value to be used for construction.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    template<typename BaseType2,
             typename std::enable_if<std::is_convertible<BaseType2, BaseType>::value,
                                     bool>::type = true>
    explicit WIR_SaturatingInt( const BaseType2 &b )
    {
      DSTART(
        "WIR_SaturatingInt<BaseType>::WIR_SaturatingInt(const BaseType2&)" );

      *this = b;
    };

    /*!
      @brief Constructor generating a saturating integer from some other
             saturating integer value.

      @tparam BaseType2 The base type of the saturating intege rvalue to be used
                        for construction, which has to be convertible to this
                        saturating int's base type.
      @param[in] i A const reference to a saturating integer value to be used
                   for construction.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    template<typename BaseType2,
             typename std::enable_if<std::is_convertible<BaseType2, BaseType>::value,
                                     bool>::type = true>
    WIR_SaturatingInt( const WIR_SaturatingInt<BaseType2> &i )
    {
      DSTART(
        "WIR_SaturatingInt<BaseType>::WIR_SaturatingInt(const WIR_SaturatingInt<BaseType2>&)" );

      *this = i;
    };


    //
    // Helper macros.
    //

    /*!
      @brief SATURATE_ASSIGN saturates the current object when an overflow
             occurs in the given computation.

      Saturation is done depending on the boolean second parameter either
      towards the minimum or the maximum value.
    */
    #define SATURATE_ASSIGN( opeq, value, canSaturate, saturateToMinimum ) \
      { \
        try { \
          mContent opeq value; \
        } catch ( const THIRDRDPARTY::SafeIntException &e ) { \
          if ( canSaturate ) { \
            if ( e.m_code == THIRDRDPARTY::SafeIntArithmeticOverflow ) { \
              if ( saturateToMinimum ) \
                mContent = std::numeric_limits<BaseType>::min(); \
              else \
                mContent = std::numeric_limits<BaseType>::max(); \
            } else { \
              throw; \
            } \
          } else { \
            throw; \
          } \
        } \
      }

    // We try to always only implement the assignment compound operator with
    // WIR_SaturatingInt operands and to reduce all other forms to this case.
    #define OPERATOR_WRAPPERS( op, opeq ) \
      template<typename BaseType2> \
      WIR_SaturatingInt<BaseType> & operator opeq( const BaseType2 &i ) \
      { \
        self_type wrappedI( i ); \
        return( *this opeq wrappedI ); \
      }; \
      \
      template<typename BaseType2> \
      auto operator op( const WIR_SaturatingInt<BaseType2> &i ) const -> \
        WIR_SaturatingInt<decltype( BaseType() op BaseType2() )> \
      { \
        self_type clone( *this ); \
        clone opeq i; \
        return( clone ); \
      }; \
      \
      template<typename BaseType2> \
      auto operator op( const BaseType2 &i ) const -> \
        WIR_SaturatingInt<decltype( BaseType() op BaseType2() )> \
      { \
        return( *this op WIR_SaturatingInt<BaseType2>( i ) ); \
      };

    #define COMPARISON_WRAPPER( op ) \
      template<typename BaseType2> \
      bool operator op( const BaseType2 &i ) const \
      { \
        return( mContent op i ); \
      }; \
      \
      template<typename BaseType2> \
      bool operator op( const WIR_SaturatingInt<BaseType2> &i ) const \
      { \
        return( mContent op i.getContent() ); \
      };


    //
    // Assignment operators.
    //

    /*!
      @brief Copy-assignment operator with different base type.

      @tparam BaseType2 The type of the value to be assigned.
      @param[in] i A const reference to another integer value to be
                   copy-assigned.

      If, during assignment, the new value is out of the range that the current
      type can represent, we have to saturate.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    template<typename BaseType2>
    self_type & operator = ( const BaseType2 &i )
    {
      DSTART(
        "WIR_SaturatingInt<BaseType>::self_type& WIR_SaturatingInt<BaseType>::operator=(const BaseType2&)" );

      // Make sure that the following comparison is done with sign if
      // neccessary, in order to be able to detect cases when a negative signed
      // int is converted to an unsigned int. If we would not convert
      // explicitly, e.g., -1 < 0u yields "false", and would saturate to the
      // upper end, though it should saturate towards 0u.
      using signedType =
        typename std::make_signed<decltype( mContent.Ref() + i )>::type;
      SATURATE_ASSIGN(
        =, i, true,
        signedType( i ) < signedType( std::numeric_limits<BaseType>::min() ) );
      return( *this );
    };

    /*!
      @brief Copy-assignment operator of another saturating int.

      @tparam BaseType2 The type of the saturating int to be assigned.
      @param[in] i A const reference to another saturating int to be
                     copy-assigned.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    template<typename BaseType2>
    self_type & operator = ( const WIR_SaturatingInt<BaseType2> &i )
    {
      DSTART(
        "WIR_SaturatingInt<BaseType>::self_type& WIR_SaturatingInt<BaseType>::operator=(const WIR_SaturatingInt<BaseType2>&)" );

      *this = i.getContent();
      return( *this );
    };


    //
    // Arithmetical operators (+, +=, -, -=, *, *=, /, /=, %, %=).
    //

    // cppcheck-suppress noExplicitConstructor
    OPERATOR_WRAPPERS( +, += )

    /*!
      @brief Addition-assignment operator.

      @param[in] i A const reference to another saturating int to be added.
      @return A reference to this saturating int containing the sum.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    template<typename BaseType2>
    self_type & operator += ( const WIR_SaturatingInt<BaseType2> &i )
    {
      DSTART(
        "WIR_SaturatingInt<BaseType>::self_type& WIR_SaturatingInt<BaseType>::operator+=(const WIR_SaturatingInt<BaseType2>&)" );

      // Addition saturates to the maximum for unsigned types.
      // For signed types:
      //   - Saturate to the minimum when signs differ
      //   - Saturate to the maximum when signs are same
      SATURATE_ASSIGN( +=, i.getContent(), true, sign() && i.sign() );
      return( *this );
    };

    // cppcheck-suppress noExplicitConstructor
    OPERATOR_WRAPPERS( -, -= )

    /*!
      @brief Subtraction-assignment operator.

      @param[in] i A const reference to another saturating int to be subtracted.
      @return A reference to this saturating int containing the difference.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    template<typename BaseType2>
    self_type & operator -= ( const WIR_SaturatingInt<BaseType2> &i )
    {
      DSTART(
        "WIR_SaturatingInt<BaseType>::self_type& WIR_SaturatingInt<BaseType>::operator-=(const WIR_SaturatingInt<BaseType2>&)" );

      // For unsigned types:
      //   - Saturate to the minimum only (overflow is impossible)
      // For signed types:
      //   - Saturate to the minimum when signs differ
      //   - Saturate to the maximum when signs are same
      SATURATE_ASSIGN(
        -=, i.getContent(), true,
        std::is_unsigned<BaseType>::value || ( sign() && !i.sign() ) );
      return( *this );
    };

    // cppcheck-suppress noExplicitConstructor
    OPERATOR_WRAPPERS( *, *= )

    /*!
      @brief Multiplication-assignment operator.

      @param[in] i A const reference to another saturating int to be multiplied.
      @return A reference to this saturating int containing the product.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    template<typename BaseType2>
    self_type & operator *= ( const WIR_SaturatingInt<BaseType2> &i )
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      // Result cannot saturate to the minimum for unsigned types.
      // For signed types:
      //   - Saturate to the minimum when signs differ
      //   - Saturate to the maximum when signs are same
      SATURATE_ASSIGN( *=, i.getContent(), true, sign() != i.sign() );
      return( *this );
    };

    // cppcheck-suppress noExplicitConstructor
    OPERATOR_WRAPPERS( /, /= )

    /*!
      @brief Division-assignment operator.

      @param[in] i A const reference to the divisor.
      @return A reference to this saturating int containing the quotient.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    template<typename BaseType2>
    self_type & operator /= ( const WIR_SaturatingInt<BaseType2> &i )
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      // Result cannot saturate at all.
      SATURATE_ASSIGN( /=, i.getContent(), false, sign() != i.sign() );
      return( *this );
    };

    // cppcheck-suppress noExplicitConstructor
    OPERATOR_WRAPPERS( %, %= )

    /*!
      @brief Modulo-assignment operator.

      @param[in] i A const reference to the divisor.
      @return A reference to this saturating int containing the modulus.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    template<typename BaseType2>
    self_type & operator %= ( const WIR_SaturatingInt<BaseType2> &i )
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      // Result cannot saturate at all.
      SATURATE_ASSIGN( %=, i.getContent(), false, sign() != i.sign() );
      return( *this );
    };

    //
    // Pre-/Post- increment/decrement operators.
    //

    /*!
      @brief Pre-increment operator.

      @return A reference to this saturating int incremented by one.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    self_type & operator ++ ( void )
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      return( *this += 1 );
    };

    /*!
      @brief Pre-decrement operator.

      @return A reference to this saturating int decremented by one.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    self_type & operator -- ( void )
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      return( *this -= 1 );
    };

    /*!
      @brief Post-increment operator.

      @return A saturating int containing the value of this object before
              incrementing it by one.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    self_type operator ++ ( int )
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      self_type clone( *this );
      *this += 1;
      return( clone );
    };

    /*!
      @brief Post-decrement operator.

      @return A saturating int containing the value of this object before
              decrementing it by one.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    self_type operator -- ( int )
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      self_type clone( *this );
      *this -= 1;
      return( clone );
    };


    //
    // Shift operators (<<, <<=, >>, >>=).
    // (only for unsigned types, since SafeInt provides no overflow checking
    // here)
    //

    // cppcheck-suppress noExplicitConstructor
    OPERATOR_WRAPPERS( <<, <<= )

    /*!
      @brief Left-shift-assignment operator.

      @param[in] i A const reference to a saturating int acting as shift amount.
      @return A reference to this saturating int containing the shifted value.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    template<typename BaseType2, typename BaseTypeCopy = BaseType>
    typename std::enable_if<std::is_unsigned<BaseTypeCopy>::value, self_type &>::type
    operator <<= ( const WIR_SaturatingInt<BaseType2> &i )
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      mContent <<= i.getContent();
      return( *this );
    };

    // cppcheck-suppress noExplicitConstructor
    OPERATOR_WRAPPERS( >>, >>= )

    /*!
      @brief Right-shift-assignment operator.

      @param[in] i A const reference to a saturating int acting as shift amount.
      @return A reference to this saturating int containing the shifted value.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    template<typename BaseType2, typename BaseTypeCopy = BaseType>
    typename std::enable_if<std::is_unsigned<BaseTypeCopy>::value, self_type &>::type
    operator >>= ( const WIR_SaturatingInt<BaseType2> &i )
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      mContent >>= i.getContent();
      return( *this );
    };


    //
    // Bitwise logical operators (&, &=, |, |=, ^, ^=).
    // (only for unsigned types, since SafeInt provides no overflow checking
    // here)
    //

    // cppcheck-suppress noExplicitConstructor
    OPERATOR_WRAPPERS( &, &= )

    /*!
      @brief AND-assignment operator.

      @param[in] i A const reference to a saturating int to be ANDed.
      @return A reference to this saturating int containing the masked value.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    template<typename BaseType2, typename BaseTypeCopy = BaseType>
    typename std::enable_if<std::is_unsigned<BaseTypeCopy>::value, self_type &>::type
    operator &= ( const WIR_SaturatingInt<BaseType2> &i )
    {
      DSTART(
        "WIR_SaturatingInt<BaseType>::self_type& WIR_SaturatingInt<BaseType>::operator&=(const WIR_SaturatingInt<BaseType2>&)" );

      mContent &= i.getContent();
      return( *this );
    };

    // cppcheck-suppress noExplicitConstructor
    OPERATOR_WRAPPERS( |, |= )

    /*!
      @brief OR-assignment operator.

      @param[in] i A const reference to a saturating int to be ORed.
      @return A reference to this saturating int containing the masked value.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    template<typename BaseType2, typename BaseTypeCopy = BaseType>
    typename std::enable_if<std::is_unsigned<BaseTypeCopy>::value, self_type &>::type
    operator |= ( const WIR_SaturatingInt<BaseType2> &i )
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      mContent |= i.getContent();
      return( *this );
    };

    // cppcheck-suppress noExplicitConstructor
    OPERATOR_WRAPPERS( ^, ^= )

    /*!
      @brief XOR-assignment operator.

      @param[in] i A const reference to a saturating int to be XORed.
      @return A reference to this saturating int containing the masked value.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    template<typename BaseType2, typename BaseTypeCopy = BaseType>
    typename std::enable_if<std::is_unsigned<BaseTypeCopy>::value, self_type &>::type
    operator ^= ( const WIR_SaturatingInt<BaseType2> &i )
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      mContent ^= i.getContent();
      return( *this );
    };


    //
    // Comparison operators (==, !=, >, <, >=, <=).
    //

    COMPARISON_WRAPPER( == )
    COMPARISON_WRAPPER( != )
    COMPARISON_WRAPPER( > )
    COMPARISON_WRAPPER( < )
    COMPARISON_WRAPPER( >= )
    COMPARISON_WRAPPER( <= )

    #undef OPERATOR_WRAPPERS
    #undef COMPARISON_WRAPPER
    #undef SATURATE_ASSIGN


    //
    // Cast operator.
    //

    /*!
      @brief Operator allowing typecast to BaseType.

      @return The saturating int's value as base type.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    operator BaseType( void ) const
    {
      DSTART(
        "WIR_SaturatingInt<BaseType, <anonymous> >::operator BaseType() const" );

      return( mContent );
    };


    //
    // Miscellaneous.
    //

    /*!
      @brief getContent returns a saturating int's value.

      @return The saturating int's value as base type.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    BaseType getContent( void ) const
    {
      DSTART( "BaseType WIR_SaturatingInt<BaseType>::getContent() const" );

      return( mContent );
    };

    /*!
      @brief sign returns whether a saturated int is negative or positive.

      @return True iff a saturating int is negative, false otherwise.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    bool sign( void ) const
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

       return( sign( mContent.Ref() ) );
    };

    /*!
      @brief sign returns whether a base type value is negative or positive.

      @param[in] i A const reference to an integer value whose sign is to be
                   checked.
      @return True iff i is negative, false otherwise.

      This method only supports signed base types.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    template<typename BaseType2>
    static typename std::enable_if<std::is_signed<BaseType2>::value, bool>::type
    sign( const BaseType2 &i )
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      return( i < 0 );
    };

    /*!
      @brief sign returns whether a base type value is negative or positive.

      @param[in] i A const reference to an integer value whose sign is to be
                   checked.
      @return True iff i is negative, false otherwise.

      This method only supports unsigned base types and thus always returns
      false.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    template<typename BaseType2>
    static typename std::enable_if<std::is_unsigned<BaseType2>::value, bool>::type
    sign( const BaseType2 &i )
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      (void) i;

      return( false );
    };


  private:

    //! mContent stores a saturating int's actual value.
    THIRDRDPARTY::SafeInt<BaseType> mContent;

};


//
// Stream I/O.
//

/*!
  @brief The << operator dumps a %WIR saturating int to an output stream.

  @param[in] os A reference to an output stream.
  @param[in] i A const reference to the %WIR saturating int to be dumped.
  @return A reference to the same output stream.

  @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
*/
template<typename BaseType>
std::ostream & operator << ( std::ostream &os,
                             const WIR_SaturatingInt<BaseType> &i )
{
  DSTART( "ostream& operator<<(ostream&, const WIR_SaturatingInt<BaseType>&)" );

  return( os << i.getContent() );
};

}       // namespace WIR


namespace std {

//
// Wrappers for standard facilities specialized for integers, here specialized
// for WIR_SaturatingInt, to make it usable anywhere a built-in integer may be
// used.
//

template<typename T>
struct numeric_limits<WIR::WIR_SaturatingInt<T>> : public numeric_limits<T>
{

  typedef numeric_limits<T> base_type;
  typedef WIR::WIR_SaturatingInt<T> sat_type;

  static constexpr sat_type
  min() noexcept { return base_type::min(); }

  static constexpr sat_type
  max() noexcept { return base_type::max(); }

  static constexpr sat_type
  lowest() noexcept { return base_type::lowest(); }

  static constexpr sat_type
  epsilon() noexcept { return base_type::epsilon(); }

  static constexpr sat_type
  round_error() noexcept { return base_type::round_error(); }

  static constexpr sat_type
  infinity() noexcept { return base_type::infinity(); }

  static constexpr sat_type
  quiet_NaN() noexcept { return base_type::quiet_NaN(); }

  static constexpr sat_type
  signaling_NaN() noexcept { return base_type::signaling_NaN(); }

  static constexpr sat_type
  denorm_min() noexcept { return base_type::denorm_min(); }

};

template<typename T>
struct is_signed<WIR::WIR_SaturatingInt<T>> : public is_signed<T> {};

template<typename T>
struct is_unsigned<WIR::WIR_SaturatingInt<T>> : public is_unsigned<T> {};

template<typename T>
struct is_integral<WIR::WIR_SaturatingInt<T>> : public is_integral<T> {};

template<typename T>
struct is_floating_point<WIR::WIR_SaturatingInt<T>> : public is_floating_point<T>
{};

template<typename T>
struct is_arithmetic<WIR::WIR_SaturatingInt<T>> : public is_arithmetic<T> {};

template<typename T>
struct is_scalar<WIR::WIR_SaturatingInt<T>> : public is_scalar<T> {};

template<typename T>
struct make_signed<WIR::WIR_SaturatingInt<T>> : public make_signed<T>
{
  using type = WIR::WIR_SaturatingInt<typename make_signed<T>::type>;
};

template<typename T>
struct make_unsigned<WIR::WIR_SaturatingInt<T>> : public make_unsigned<T>
{
  using type = WIR::WIR_SaturatingInt<typename make_unsigned<T>::type>;
};

template<typename T>
typename std::enable_if<std::is_signed<T>::value,
                        WIR::WIR_SaturatingInt<T>>::type
abs( const WIR::WIR_SaturatingInt<T> &i )
{
  return( WIR::WIR_SaturatingInt<T>( std::abs( i.getContent() ) ) );
};

template<typename T>
typename std::enable_if<std::is_unsigned<T>::value,
                        WIR::WIR_SaturatingInt<T>>::type
abs( const WIR::WIR_SaturatingInt<T> &i )
{
  return( i );
};

}       // namespace std

#endif  // _WIR_SATURATING_INT_H
