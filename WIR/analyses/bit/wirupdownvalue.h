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
  @file wirupdownvalue.h
  @brief This file provides the interface of L4-based bit vectors storing up and
         down values for the bit-true data and value flow analysis.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_UPDOWNVALUE_H
#define _WIR_UPDOWNVALUE_H


//
// Include section
//

// Include standard headers
#include <functional>
#include <ostream>
#include <set>
#include <utility>
#include <vector>

// Include boost headers
#include <boost/optional.hpp>

// Include WIR headers
#include <analyses/bit/wirl4.h>
#include <analyses/bit/wirlocation.h>


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
  @brief Class WIR_UpDownValue models vectors of L4 bit values which are used as
         up or down values.

  Cf. Jens Wagner, Retargierbare Ausnutzung von Spezialoperationen für
  Eingebettete Systeme mit Hilfe bitgenauer Wertflussanalyse, page 155ff.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_UpDownValue
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor creating an unsigned up/down value of a given
             size.

      @param[in] s An unsigned integer denoting the bit width of the up/down
                   value.

      All bits are initialized to the value 'U'.

      In its current implementation, WIR_UpDownValue supports bit widths of up
      to sizeof( long long ) * 8. If larger bit widths are given, this
      constructor asserts.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_UpDownValue( unsigned int );

    /*!
      @brief Default constructor creating an up/down value of a given size and
             signedness with all bits initialized to so some L4 value.

      @param[in] v An L4 value used to initialize all bits. L and N must not be
                   used here.
      @param[in] s An unsigned integer denoting the bit width of the up/down
                   value.
      @param[in] b A Boolean defaulting to false that denotes whether the
                   created up/down value is signed or not.

      In its current implementation, WIR_UpDownValue supports bit widths of up
      to sizeof( long long ) * 8. If larger bit widths are given, this
      constructor asserts.

      This constructor asserts if it is passed an L or N value as initializer.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_UpDownValue( WIR_L4, unsigned int, bool = false );

    /*!
      @brief Default constructor creating an up/down value initialized to some
             given immediate value.

      @param[in] p A const reference to the immediate parameter to be used as
                   initializer.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_UpDownValue( const WIR_BaseImmediateParameter & );

    /*!
      @brief Default constructor creating an up/down value initialized to some
             register location.

      @param[in] p A const reference to the register parameter to be used as
                   location.
      @param[in] b A Boolean that denotes whether the created up/down value is
                   signed or not.
      @param[in] n A Boolean defaulting to false that denotes whether the
                   created up/down value shall be initialized to N values
                   instead of L values.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_UpDownValue( const WIR_RegisterParameter &, bool, bool = false );

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_UpDownValue( const WIR_UpDownValue & );

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_UpDownValue( WIR_UpDownValue && );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_UpDownValue( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_UpDownValue & operator = ( const WIR_UpDownValue & );

    /*!
      @brief Move-assignment operator.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_UpDownValue & operator = ( WIR_UpDownValue && );


    //
    // Value handling.
    //

    /*!
      @brief setAllBits sets all bits of an up/down value to the given L4 value.

      @param[in] v An L4 value to which all bits of an up/down value shall be
                   set. L and N must not be used here.

      setAllBits asserts if it is passed an L or N value.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setAllBits( WIR_L4 );

    /*!
      @brief setAllBits sets all bits of an up/down value to the given L4
             location.

      @param[in] v One of the L4 values L or N to which all bits of an up/down
                   value shall be set.
      @param[in] l A const reference to a location to that all bits of an
                   up/down shall refer.

      setAllBits asserts if it is passed an L4 value different than L or N.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setAllBits( WIR_L4, const WIR_Location & );

    /*!
      @brief setBit sets the bit at the given position in an up/down value to
             the given L4 value.

      @param[in] i An unsigned integer that denotes the position of the bit to
                   be set in an up/down value, with 0 <= i < mBitWidth and i = 0
                   denoting the up/down value's least-significant bit.
      @param[in] v An L4 value to which the given bit of an up/down value shall
                   be set. L and N must not be used here.

      setBit asserts if it is passed an L or N value.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setBit( unsigned int, WIR_L4 );

    /*!
      @brief setBit sets the bit at the given position in an up/down value to
             the given L4 location.

      @param[in] i An unsigned integer that denotes the position of the bit to
                   be set in an up/down value, with 0 <= i < mBitWidth and i = 0
                   denoting the up/down value's least-significant bit.
      @param[in] v One of the L4 values L or N to which the given bit of an
                   up/down value shall be set.
      @param[in] l A const reference to a location to which the specified bit of
                   an up/down shall refer.

      setBit asserts if it is passed an L4 value different than L or N.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setBit( unsigned int, WIR_L4, const WIR_Location & );

    /*!
      @brief Element access operator returning an up/down value's L4 value of
             the given bit position.

      @param[in] i An unsigned integer that denotes the bit position to be
                   returned, with 0 <= i < mBitWidth and i = 0 denoting the
                   up/down value's least-significant bit.
      @return A reference to the L4 value at bit position i.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_L4 & operator [] ( unsigned int );

    /*!
      @brief Element access operator returning an up/down value's const L4 value
             of the given bit position.

      @param[in] i An unsigned integer that denotes the bit position to be
                   returned, with 0 <= i < mBitWidth and i = 0 denoting the
                   up/down value's least-significant bit.
      @return A const reference to the L4 value at bit position i.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const WIR_L4 &at( unsigned int ) const;

    /*!
      @brief getLocation returns an up/down value's location of the given bit
             position.

      @param[in] i An unsigned integer that denotes the bit position to be
                   returned, with 0 <= i < mBitWidth and i = 0 denoting the
                   up/down value's least-significant bit.
      @return A const reference to the location refered to by bit position i of
              an up/down value.

      getLocation asserts if bit position i contains neither an L nor an N L4
      value.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const WIR_Location & getLocation( unsigned int ) const;

    /*!
      @brief getBitWidth returns an up/down value's bit width.

      @return An unsigned integer denoting the up/down value's bit width.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    unsigned int getBitWidth( void ) const;

    /*!
      @brief getSignedValue gets an up/down value's signed integer number.

      @return A signed long long integer representing the up/down value's
              numerical value.

      getSignedValue only considers 1-bits for its integer value computation.
      Bits having different values in L4 are ignored, i.e., assumed to be 0.
      Thus, getSignedValue silently assumes that isBinaryInteger() returns true
      (which actually makes sense if you want to obtain an integer value).

      While computing the integer value, getSignedValue uses either a classical
      binary representation for unsigned up/down values or 2's complement for
      signed up/down values. In the end, getSignedValue returns a signed (!)
      long long, irrespective of whether the up/down value itself is signed or
      unsigned.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    signed long long getSignedValue( void ) const;

    /*!
      @brief setSignedness sets the signedness of an up/down value.

      @param[in] s A Boolean defaulting to true that denotes whether an up/down
                   value is signed or not.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setSignedness( bool = true );

    /*!
      @brief isSigned returns whether an up/down value is signed or not.

      @return true if the up/down value is signed, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isSigned( void ) const;

    /*!
      @brief isUnsigned returns whether an up/down value is unsigned or not.

      @return true if the up/down value is unsigned, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isUnsigned( void ) const;

    /*!
      @brief isInteger returns whether an up/down value represents an integer
             number, i.e., only contains 0, 1 or X bits.

      @return true if the up/down value is an integer, false below.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isInteger( void ) const;

    /*!
      @brief isBinaryInteger returns whether an up/down value represents a
             true binary integer number, i.e., only contains 0 or 1 bits.

      @return true if the up/down value is a binary integer, false below.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isBinaryInteger( void ) const;

    /*!
      @brief isPositive returns whether an up/down value is a positive integer
             number.

      @return true if the up/down value is either unsigned or if it is a signed
              integer whose most-significant bit is not '1'.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isPositive( void ) const;

    /*!
      @brief isNegative returns whether an up/down value is a negative integer
             number.

      @return true if the up/down value is a signed integer whose
              most-significant bit is '1'.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isNegative( void ) const;

    /*!
      @brief containsBit returns whether an up/down value contains the specified
             bit value.

      @param[in] v An L4 bit value to be found.
      @return true if the up/down value contains the specified bit value, false
              otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool containsBit( const WIR_L4 ) const;

    /*!
      @brief containsBits returns whether an up/down value contains at least one
             of the specified bit values.

      @param[in] v An R-value reference to a set of L4 bit values to be
                   verified.
      @return true if the up/down value contains one of the specified bit
              values, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool containsBits( WIR_L4Set && ) const;

    /*!
      @brief containsOnlyBit returns whether an up/down value contains only the
             specified bit value.

      @param[in] v An L4 bit value to be verified.
      @return true if the up/down value contains nothing but the specified bit
              value, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool containsOnlyBit( const WIR_L4 ) const;

    /*!
      @brief containsOnlyBits returns whether an up/down value contains only bit
             values from the specified set.

      @param[in] v An R-value reference to a set of L4 bit values to be
                   verified.
      @return true if the up/down value contains nothing but the bit values from
              the specified set, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool containsOnlyBits( WIR_L4Set && ) const;

    /*!
      @brief isLocationBit returns whether the bit at the specified position is
             either a location or a negated location.

      @param[in] i An unsigned integer that denotes the position of the bit to
                   be checked, with 0 <= i < mBitWidth and i = 0 denoting the
                   up/down value's least-significant bit.
      @return true if bit i of an up/down value is either L or N, false
              otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isLocationBit( unsigned int ) const;


    //
    // Comparison operators.
    //

    /*!
      @brief This operator compares two WIR_UpDownValues for provable, strictly
             bitwise equality.

      @param[in] __o A const reference to another object to be compared.
      @return 1 iff both operands are provably equal, 0 iff both operands are
                provably inequal, U otherwise.

      In order to be provably equal, two up/down values must have the same bit
      widths. If this is not the case, the == operator returns 0.

      The == operator returns 1 if for each bit position
      - either both compared bits are 1 or both are 0, OR
      - either both compared bits are L or both are N and they both refer to the
        very same location.
      The == operator returns 0 if for some bit position
      - one compared bit is 1 and the other one is 0, OR
      - both compared bits refer to the very same location but one is L and the
        other is N.

      In all other cases, no clear statement about equality can be made so that
      the == operator returns U.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_L4 operator == ( const WIR_UpDownValue & ) const;

    /*!
      @brief This operator compares two WIR_UpDownValues for provable, strictly
             bitwise inequality.

      @param[in] __o A const reference to another object to be compared.
      @return 1 iff both operands are provably inequal, 0 iff both operands are
                provably equal, U otherwise.

      The != operator returns 0 if the two up/down values have different bit
      widhts.

      The != operator returns 1 if for some bit position
      - one compared bit is 1 and the other is 0, OR
      - one compared bit is L and the other is N and they both refer to the very
        same location.
      The != operator returns 0 if for each bit position
      - both compared bits are 1 or both are 0, OR
      - either both compared bits are L or both are N and they both refer to the
        very same location.

      In all other cases, no clear statement about inequality can be made so
      that the != operator returns U.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_L4 operator != ( const WIR_UpDownValue & ) const;

    /*!
      @brief This operator performs a provable greater-equal comparison of two
             WIR_UpDownValues.

      @param[in] __o A const reference to another object to be compared.
      @return 1 iff this object is provably greater-equal than __o, 0 iff this
              object is provably not greater-equal than __o, U otherwise.

      If any operand of the >= operator contains U, L or N bits, i.e., is not an
      integer number, no greater-equal comparison can be performed and the
      operator returns U. Otherwise, the integer values of both operands are
      compared and 0 or 1 is returned.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_L4 operator >= ( const WIR_UpDownValue & ) const;

    /*!
      @brief This operator performs a provable less-than comparison of two
             WIR_UpDownValues.

      @param[in] __o A const reference to another object to be compared.
      @return 1 iff this object is provably less than __o, 0 iff this object is
              provably not less than __o, U otherwise.

      If any operand of the < operator contains U, L or N bits, i.e., is not an
      integer number, no less-than comparison can be performed and the operator
      returns U. Otherwise, the integer values of both operands are compared and
      0 or 1 is returned.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_L4 operator < ( const WIR_UpDownValue & ) const;

    /*!
      @brief This operator performs a greater-than comparison of two up/down
             values.

      @param[in] __o A const reference to another object to be compared.
      @return true iff the LHS operand is greater than the RHS operand, false
              otherwise.

      An up/down value is greater than another value if at least one bit in LHS
      is greater than the corresponding bit in RHS, and if no bit from LHS is
      less than RHS.

      Since a greater-than comparison of up/down values of different bit widths
      does not make sense, this operator returns false in this case.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool operator > ( const WIR_UpDownValue & ) const;

    /*!
      @brief isEqual compares two WIR_UpDownValues for "relaxed" bitwise
             equality.

      @param[in] __o A const reference to another object to be compared.
      @return true iff both operands are equal, false otherwise.

      In order to be equal, two up/down values must have the same bit widths and
      have to contain exactly the same bit values where U bits are considered
      equal by default. Furthermore, L or N bits have to refer to exactly the
      same register parameter and bit position therein in order to be equal.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isEqual( const WIR_UpDownValue & ) const;


    //
    // Arithmetics.
    //

    /*!
      @brief This operator performs a bitwise AND of two up/down values.

      @param[in] __o A const reference to another up/down value to be ANDed.
      @return The up/down value resulting from the AND operation.

      This operator asserts if up/down values of different bit widths are
      combined.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_UpDownValue operator & ( const WIR_UpDownValue &__o ) const;

    /*!
      @brief This operator performs a bitwise OR of two up/down values.

      @param[in] __o A const reference to another up/down value to be ORed.
      @return The up/down value resulting from the OR operation.

      This operator asserts if up/down values of different bit widths are
      combined.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_UpDownValue operator | ( const WIR_UpDownValue & ) const;

    /*!
      @brief This operator performs a bitwise XOR of two up/down values.

      @param[in] __o A const reference to another up/down value to be XORed.
      @return The up/down value resulting from the XOR operation.

      This operator asserts if up/down values of different bit widths are
      combined.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_UpDownValue operator ^ ( const WIR_UpDownValue & ) const;

    /*!
      @brief This operator performs a bitwise negation of an up/down value.

      @return The negated up/down value.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_UpDownValue operator ~ ( void ) const;

    /*!
      @brief This operator performs a bitwise left shift of an up/down value by
             an integer shift amount.

      @param[in] shamt An integer value containing the shift amount.
      @return The up/down value resulting from the shift operation. The result
              always has the same bit width as the operand to be shifted.

      A signed and negative shift amount is treated as a right shift by the
      positive shift amount. When shifting right, the vacated bits are filled
      either with '0' or with this up/down value's sign bit, depending on the
      value's signedness.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_UpDownValue operator << ( int ) const;

    /*!
      @brief This operator performs a bitwise left shift of two up/down values.

      @param[in] __o A const reference to another up/down value containing the
                     shift amount.
      @return The up/down value resulting from the shift operation. The result
              always has the same bit width as the operand to be shifted.

      If the shift amount is not an integer, i.e., contains 'U', 'L' or 'N'
      bits, the result of the shift operator is fully unknown.

      'X' bits in the shift amount are interpreted either as '1' for positive
      shift amount values or as '0' for negative shift amount values. This
      maximizes the shift amount's absolute value and thus leads to a maximal
      number of '0' bits in the shifted output.

      A signed and negative shift amount is treated as a right shift by the
      positive shift amount.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_UpDownValue operator << ( const WIR_UpDownValue & ) const;

    /*!
      @brief This operator performs a bitwise right shift of an up/down value by
             an integer shift amount.

      @param[in] shamt An integer value containing the shift amount.
      @return The up/down value resulting from the shift operation. The result
              always has the same bit width as the operand to be shifted.

      A signed and negative shift amount is treated as a left shift by the
      positive shift amount.

      Depending on whether the up/down value to be shifted is signed or
      unsigned, either arithmetical or logical right shifting is applied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_UpDownValue operator >> ( int ) const;

    /*!
      @brief This operator performs a bitwise right shift of two up/down values.

      @param[in] __o A const reference to another up/down value containing the
                     shift amount.
      @return The up/down value resulting from the shift operation. The result
              always has the same bit width as the operand to be shifted.

      If the shift amount is not an integer, i.e., contains 'U', 'L' or 'N'
      bits, the result of the shift operator is fully unknown.

      'X' bits in the shift amount are interpreted either as '1' for positive
      shift amount values or as '0' for negative shift amount values. This
      maximizes the shift amount's absolute value and thus leads to a maximal
      number of '0' bits in the shifted output.

      A signed and negative shift amount is treated as a left shift by the
      positive shift amount.

      Depending on whether the up/down value to be shifted is signed or
      unsigned, either arithmetical or logical right shifting is applied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_UpDownValue operator >> ( const WIR_UpDownValue & ) const;

    /*!
      @brief This operator performs a bitwise addition of two up/down values.

      @param[in] __o A const reference to another up/down value to be added.
      @return The up/down value resulting from the addition.

      This operator asserts if up/down values of different bit widths are
      combined.

      The result is an unsigned number if both operands are unsigned, otherwise
      the result is signed.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_UpDownValue operator + ( const WIR_UpDownValue & ) const;

    /*!
      @brief This operator performs an arithmetical unary minus of an up/down
             value.

      @return The arithmetically inverted value of an up/down value.

      Due to the nature of the unary arithmetical minus, the result's bit width
      equals that of its operand, and the result is always signed. The unary
      minus is computed by applying 2's-complement representation, i.e., all
      bits are inverted and the value +1 is added afterwards.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_UpDownValue operator - ( void ) const;

    /*!
      @brief This operator performs a bitwise subtraction of two up/down values.

      @param[in] __o A const reference to another up/down value to be
                     subtracted.
      @return The up/down value resulting from the subtraction.

      This operator asserts if up/down values of different bit widths are
      combined.

      Due to the nature of the arithmetical minus, the result is always signed.
      The subtraction a - b is reduced to the addition a + (-b).

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_UpDownValue operator - ( const WIR_UpDownValue & ) const;

    /*!
      @brief This operator performs a bitwise multiplication of two up/down
             values.

      @param[in] __o A const reference to another up/down value to be
                     multiplied.
      @return The up/down value resulting from the multiplication.

      This operator asserts if up/down values of different bit widths are
      combined.

      The result is an unsigned number if both operands are unsigned, otherwise
      the result is signed.

      @note Currently, this operator computes no actual multiplication but
            returns U* instead.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_UpDownValue operator * ( const WIR_UpDownValue & ) const;

    /*!
      @brief This operator performs a bitwise division of two up/down values.

      @param[in] __o A const reference to another up/down value to be divided.
      @return The up/down value resulting from the division.

      This operator asserts if up/down values of different bit widths are
      combined.

      The result is an unsigned number of both operands are unsigned, otherwise
      the result is signed.

      @note Currently, this operator computes no actual division but returns U*
            instead.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_UpDownValue operator / ( const WIR_UpDownValue & ) const;

    /*!
      @brief This operator performs a bitwise modulo computation of two up/down
             values.

      @param[in] __o A const reference to another up/down value.
      @return The up/down value resulting from the modulo computation.

      This operator asserts if up/down values of different bit widths are
      combined.

      The result of this operator is always an unsigned number.

      @note Currently, this operator computes no actual modulo but returns U*
            instead.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_UpDownValue operator % ( const WIR_UpDownValue & ) const;

    /*!
      @brief extend extends an up/down value to a given bit width.

      @param[in] s An unsigned integer denoting the bit width of the up/down
                   value.
      @return The width-extended up/down value.

      extend asserts if the given bit width is smaller than the up/down value's
      current width.

      Depending on whether the up/down value is signed or not, either sign
      extension or 0-extension is performed.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_UpDownValue extend( unsigned int ) const;


    //
    // Miscellaneous operators.
    //

    /*!
      @brief abs returns the absolute value of an up/down value.

      @param[in] __o A const reference to the up/down value whose absolute value
                     is computed.
      @return The up/down value's absolute value.

      Due to the nature of the abs operator, the result's bit width equals that
      of its operand, and the result is always signed.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    friend WIR_UpDownValue abs( const WIR_UpDownValue & );

    /*!
      @brief combine returns the "smallest common" combination of two up/down
             values.

      @param[in] __o A const reference to the up/down value to be combined with
                     this object.
      @return The up/down value resulting from the combination.

      The com operator is defined in Jens Wagner, Retargierbare Ausnutzung von
      Spezialoperationen für Eingebettete Systeme mit Hilfe bitgenauer
      Wertflussanalyse, page 175, Figure 4.18.

      This function asserts if up/down values of different bit widths are
      combined.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_UpDownValue combine( const WIR_UpDownValue & ) const;

    /*!
      @brief extract performs extraction of a bit packet from an up/down value.

      @param[in] o An unsigned integer denoting the bit offset where the bit
                   packet to be extracted starts.
      @param[in] w An unsigned integer denoting the bit packet's width.
      @return An up/down value of width w that contains the specified bit packet
              only.

      This function asserts if offset and width exceed the up/down value's bit
      width.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_UpDownValue extract( unsigned int, unsigned int ) const;

    /*!
      @brief insert inserts an up/down value into another up/down value at the
             given bit offset.

      @param[in,out] r A reference to an up/down value in which the insertion
                       will take place.
      @param[in] v A const reference to an up/down value to be inserted.
      @param[in] o An unsigned integer denoting the bit offset where the
                   insertion begins.

      insert asserts if bit width of the operand and the offset exceed the
      up/down value's bit width.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    friend void insert( WIR_UpDownValue &, const WIR_UpDownValue &,
                        unsigned int );

    /*!
      @brief replaceUByLocation replaces each U bit of an up/down value by an
             L location refering to the given register parameter.

      @param[in] p A const reference to the register parameter to be used as
                   location.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void replaceUByLocation( const WIR_RegisterParameter & );

    /*!
      @brief replaceLocationByU replaces those L or N bits of an up/down value
             by U that refer to the given %WIR operation.

      @param[in] o A const reference to the %WIR operation to be checked.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void replaceLocationByU( const WIR_Operation & );


    //
    // Stream I/O.
    //

    /*!
      @brief The << operator dumps an up/down value to an output stream.

      @param[in] os A reference to an output stream.
      @param[in] v A const reference to the up/down value to be dumped.
      @return A reference to the same output stream.

      The format of the produced output strongly depends on the actual up/down
      value. If the up/down value is a true binary integer number, i.e., only
      consists of 0 and 1 bits, its numerical integer value is output, either in
      2's complement or as natural integer depending on its signedness.
      Otherwise, if the up/down value also contains U, L, N or X bits, a bitwise
      output is produced with the least-significant bit being displayed at the
      right-most side.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    friend std::ostream & operator << ( std::ostream &,
                                        const WIR_UpDownValue & );


  protected:

    //
    // General up/down value attributes.
    //

    //! mBitValues stores the L4 data for each bit of an up/down value.
    std::vector<WIR_L4> mBitValues;

    //! mBitWidth stores an up/down value's bit width.
    unsigned int mBitWidth;

    //! mIsSigned stores whether an up/down value is signed or not.
    bool mIsSigned;

    /*!
      @brief mLocation refers to locations if mBitValues[ i ] carries an L or N
             bit value.
    */
    std::vector<boost::optional<WIR_Location>> mLocation;


    //
    // General attributes.
    //

    /*!
      @brief mIsDirty stores whether the information stored in the up/down value
             is dirty or valid.
    */
    bool mIsDirty;


  private:

    /*!
      @brief No standard construction allowed, users must use one of the above
             default constructors instead.
    */
    WIR_UpDownValue( void ) = delete;

    //
    // Arithmetics.
    //

    /*!
      @brief This type represents a bit's full information, i.e., its value in
             the L4 half-order plus, optionally, its bit location.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    using WIR_BitInfo = std::pair<WIR_L4, boost::optional<WIR_Location>>;

    /*!
      @brief bitAND is an internal static helper routine computing the bitwise
             AND under full consideration of eventually provided bit locations.

      @param[in] i1 Bit information for the first operand.
      @param[in] i2 Bit information for the second operand.
      @return Bit information describing the result of i1 AND i2.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    static WIR_BitInfo bitAND( WIR_BitInfo, WIR_BitInfo );

    /*!
      @brief bitOR is an internal static helper routine computing the bitwise OR
             under full consideration of eventually provided bit locations.

      @param[in] i1 Bit information for the first operand.
      @param[in] i2 Bit information for the second operand.
      @return Bit information describing the result of i1 OR i2.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    static WIR_BitInfo bitOR( WIR_BitInfo, WIR_BitInfo );

    /*!
      @brief bitXOR is an internal static helper routine computing the bitwise
             XOR under full consideration of eventually provided bit locations.

      @param[in] i1 Bit information for the first operand.
      @param[in] i2 Bit information for the second operand.
      @return Bit information describing the result of i1 XOR i2.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    static WIR_BitInfo bitXOR( WIR_BitInfo, WIR_BitInfo );

    /*!
      @brief bitCom is an internal static helper routine computing the bitwise
             com function under full consideration of eventually provided bit
             locations.

      @param[in] i1 Bit information for the first operand.
      @param[in] i2 Bit information for the second operand.
      @return Bit information describing the result of i1 com i2.

      The com operator is defined in Jens Wagner, Retargierbare Ausnutzung von
      Spezialoperationen für Eingebettete Systeme mit Hilfe bitgenauer
      Wertflussanalyse, page 177, Figure 4.19.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    static WIR_BitInfo bitCom( WIR_BitInfo, WIR_BitInfo );

};

}       // namespace WIR

#endif  // _WIR_UPDOWNVALUE_H
