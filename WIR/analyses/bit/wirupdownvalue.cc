/*

   This source file belongs to the

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
  @file wirupdownvalue.cc
  @brief This file implements L4-based bit vectors storing up and down values
         for the bit-true data and value flow analysis.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include standard headers
#include <list>
#include <sstream>
#include <stdlib.h>

// Include libuseful headers
#include <libuseful/debugmacros.h>
#include <libuseful/io.h>
#include <libuseful/stringtools.h>

// Include WIR headers
#include <wir/wir.h>


//
// Code section
//

namespace WIR {


using namespace std;


//
// Public class methods
//

/*
  Default constructor creating an unsigned up/down value of a given size.

  All bits are initialized to the value 'U'.

  In its current implementation, WIR_UpDownValue supports bit widths of up to
  sizeof( long long ) * 8. If larger bit widths are given, this constructor
  asserts.
*/
WIR_UpDownValue::WIR_UpDownValue( unsigned int s ) :
  mBitValues { s, WIR_L4::bU },
  mBitWidth { s },
  mIsSigned { false },
  mLocation { s },
  mIsDirty { true }
{
  DSTART( "WIR_UpDownValue::WIR_UpDownValue(unsigned int)" );

  // Ensure that the bit width is sizeof( long long ) * 8 at most.
  ufAssert( ( s > 0 ) && ( s <= sizeof( long long ) * 8 ) );
};


/*
  Default constructor creating an up/down value of a given size and signedness
  with all bits initialized to so some L4 value.

  In its current implementation, WIR_UpDownValue supports bit widths of up to
  sizeof( long long ) * 8. If larger bit widths are given, this constructor
  asserts.

  This constructor asserts if it is passed an L or N value as initializer.
*/
WIR_UpDownValue::WIR_UpDownValue( WIR_L4 v, unsigned int s, bool b ) :
  mBitValues { s, v },
  mBitWidth { s },
  mIsSigned { b },
  mLocation { s },
  mIsDirty { true }
{
  DSTART( "WIR_UpDownValue::WIR_UpDownValue(WIR_L4, unsigned int, bool)" );

  ufAssertT(
    ( v != WIR_L4::bL ) && ( v != WIR_L4::bN ),
    "This constructor does not accept L or N as initializer values." );

  // Ensure that the bit width is sizeof( long long ) * 8 at most.
  ufAssert( ( s > 0 ) && ( s <= sizeof( long long ) * 8 ) );
};


/*
  Default constructor creating an up/down value initialized to some given
  immediate value.
*/
WIR_UpDownValue::WIR_UpDownValue( const WIR_BaseImmediateParameter &p ) :
  mBitValues { p.getBitWidth() },
  mBitWidth { p.getBitWidth() },
  mIsSigned { p.isSigned() },
  mLocation { p.getBitWidth() },
  mIsDirty { true }
{
  DSTART(
    "WIR_UpDownValue::WIR_UpDownValue(const WIR_BaseImmediateParameter&)" );

  unsigned long long mask = 1u;

  if ( mIsSigned ) {
    signed long long val = p.getSignedValue();

    for ( unsigned int i = 0; i < mBitWidth; ++i, mask <<= 1 )
      #ifdef FAILSAFEMODE
      mBitValues.at( i ) =
      #else
      mBitValues[ i ] =
      #endif
        ( ( val & mask ) == 0 ) ? WIR_L4::b0 : WIR_L4::b1;
  } else {
    unsigned long long val = p.getUnsignedValue();

    for ( unsigned int i = 0; i < mBitWidth; ++i, mask <<= 1 )
      #ifdef FAILSAFEMODE
      mBitValues.at( i ) =
      #else
      mBitValues[ i ] =
      #endif
        ( ( val & mask ) == 0 ) ? WIR_L4::b0 : WIR_L4::b1;
  }
};


/*
  Default constructor creating an up/down value initialized to some register
  location.
*/
WIR_UpDownValue::WIR_UpDownValue( const WIR_RegisterParameter &p, bool b,
                                  bool n ) :
  mBitValues { p.getRegister().getBitWidth(), n ? WIR_L4::bN : WIR_L4::bL },
  mBitWidth { p.getRegister().getBitWidth() },
  mIsSigned { b },
  mLocation { p.getRegister().getBitWidth() },
  mIsDirty { true }
{
  DSTART(
    "WIR_UpDownValue::WIR_UpDownValue(const WIR_RegisterParameter&, bool, bool)" );

  for ( unsigned int i = 0; i < mBitWidth; ++i )
    mLocation[ i ].emplace( WIR_Location( cref( p ), i ) );
};


/*
  Copy constructor.
*/
WIR_UpDownValue::WIR_UpDownValue( const WIR_UpDownValue &__o ) :
  mBitValues { __o.mBitValues },
  mBitWidth { __o.mBitWidth },
  mIsSigned { __o.mIsSigned },
  mLocation { __o.mLocation },
  mIsDirty { true }
{
  DSTART( "WIR_UpDownValue::WIR_UpDownValue(const WIR_UpDownValue&)" );
};


/*
  Move constructor.
*/
WIR_UpDownValue::WIR_UpDownValue( WIR_UpDownValue &&__o ) :
  mBitValues { move( __o.mBitValues ) },
  mBitWidth { __o.mBitWidth },
  mIsSigned { __o.mIsSigned },
  mLocation { std::move( __o.mLocation ) },
  mIsDirty { true }
{
  DSTART( "WIR_UpDownValue::WIR_UpDownValue(WIR_UpDownValue&&)" );

  __o.mBitValues.clear();
  __o.mBitWidth = 0;
  __o.mLocation.clear();
};


/*
  Destructor.
*/
WIR_UpDownValue::~WIR_UpDownValue( void )
{
  DSTART( "virtual WIR_UpDownValue::~WIR_UpDownValue()" );
};


/*
  Copy-assignment operator.
*/
WIR_UpDownValue & WIR_UpDownValue::operator = ( const WIR_UpDownValue &__o )
{
  DSTART(
    "WIR_UpDownValue& WIR_UpDownValue::operator=(const WIR_UpDownValue&)" );

  mBitValues = __o.mBitValues;
  mBitWidth = __o.mBitWidth;
  mIsSigned = __o.mIsSigned;
  mLocation = __o.mLocation;
  mIsDirty = true;

  return( *this );
};


/*
  Move-assignment operator.
*/
WIR_UpDownValue & WIR_UpDownValue::operator = ( WIR_UpDownValue &&__o )
{
  DSTART( "WIR_UpDownValue& WIR_UpDownValue::operator=(WIR_UpDownValue&&)" );

  mBitValues = move( __o.mBitValues );
  __o.mBitValues.clear();

  mBitWidth = __o.mBitWidth;
  __o.mBitWidth = 0;

  mIsSigned = __o.mIsSigned;

  mLocation = std::move( __o.mLocation );
  __o.mLocation.clear();

  mIsDirty = true;

  return( *this );
};


/*
  setAllBits sets all bits of an up/down value to the given L4 value.

  setAllBits asserts if it is passed an L or N value.
*/
void WIR_UpDownValue::setAllBits( WIR_L4 v )
{
  DSTART( "void WIR_UpDownValue::setAllBits(WIR_L4)" );

  ufAssertT(
    ( v != WIR_L4::bL ) && ( v != WIR_L4::bN ),
    "setAllBits does not accept L or N as initializer values." );

  mBitValues.assign( mBitWidth, v );
  mLocation.assign( mBitWidth, boost::none );

  mIsDirty = true;
};


/*
  setAllBits sets all bits of an up/down value to the given L4 location.

  setAllBits asserts if it is passed an L4 value different than L or N.
*/
void WIR_UpDownValue::setAllBits( WIR_L4 v, const WIR_Location &l )
{
  DSTART( "void WIR_UpDownValue::setAllBits(WIR_L4, const WIR_Location&)" );

  ufAssertT(
    ( v == WIR_L4::bL ) || ( v == WIR_L4::bN ),
    "setAllBits does not accept initializer values other than L or N." );

  mBitValues.assign( mBitWidth, v );
  mLocation.assign( mBitWidth, { l } );

  mIsDirty = true;
};


/*
  setBit sets the bit at the given position in an up/down value to the given L4
  value.

  setBit asserts if it is passed an L or N value.
*/
void WIR_UpDownValue::setBit( unsigned int i, WIR_L4 v )
{
  DSTART( "void WIR_UpDownValue::setBit(unsigned int, WIR_L4)" );

  ufAssertT(
    ( v != WIR_L4::bL ) && ( v != WIR_L4::bN ),
    "setBit does not accept L or N as initializer values." );

  #ifdef FAILSAFEMODE
  mBitValues.at( i ) = v;
  mLocation.at( i ) = boost::none;
  #else
  mBitValues[ i ] = v;
  mLocation[ i ] = boost::none;
  #endif

  mIsDirty = false;
};


/*
  setBit sets the bit at the given position in an up/down value to the given L4
  location.

  setBit asserts if it is passed an L4 value different than L or N.
*/
void WIR_UpDownValue::setBit( unsigned int i, WIR_L4 v, const WIR_Location &l )
{
  DSTART(
    "void WIR_UpDownValue::setBit(unsigned int, WIR_L4, const WIR_Location&)" );

  ufAssertT(
    ( v == WIR_L4::bL ) || ( v == WIR_L4::bN ),
    "setBit does not accept initializer values other than L or N." );

  #ifdef FAILSAFEMODE
  mBitValues.at( i ) = v;
  mLocation.at( i ) = l;
  #else
  mBitValues[ i ] = v;
  mLocation[ i ] = l;
  #endif

  mIsDirty = false;
};


/*
  Element access operator returning an up/down value's L4 value of the given bit
  position.
*/
WIR_L4 & WIR_UpDownValue::operator [] ( unsigned int i )
{
  DSTART( "WIR_L4& WIR_UpDownValue::operator[](unsigned int)" );

  #ifdef FAILSAFEMODE
  return( mBitValues.at( i ) );
  #else
  return( mBitValues[ i ] );
  #endif
};


/*
  Element access operator returning an up/down value's const L4 value of the
  given bit position.
*/
const WIR_L4 &WIR_UpDownValue::at( unsigned int i ) const
{
  DSTART( "const WIR_L4& WIR_UpDownValue::at(unsigned int) const" );

  return( mBitValues.at( i ) );
};


/*
  getLocation returns an up/down value's location of the given bit position.

  getLocation asserts if bit position i contains neither an L nor an N L4
  value.
*/
const WIR_Location & WIR_UpDownValue::getLocation( unsigned int i ) const
{
  DSTART(
    "const WIR_Location& WIR_UpDownValue::getLocation(unsigned int) const" );

  #ifdef FAILSAFEMODE
  ufAssert(
    ( mBitValues.at( i ) == WIR_L4::bL ) ||
    ( mBitValues.at( i ) == WIR_L4::bN ) );
  #endif

  #ifdef FAILSAFEMODE
  return( mLocation.at( i ).get() );
  #else
  return( mLocation[ i ].get() );
  #endif
};


/*
  getBitWidth returns an up/down value's bit width.
*/
unsigned int WIR_UpDownValue::getBitWidth( void ) const
{
  DSTART( "unsigned int WIR_UpDownValue::getBitWidth() const" );

  return( mBitWidth );
};


/*
  getSignedValue gets an up/down value's signed integer number.

  getSignedValue only considers 1-bits for its integer value computation. Bits
  having different values in L4 are ignored, i.e., assumed to be 0. Thus,
  getSignedValue silently assumes that isBinaryInteger() returns true (which
  actually makes sense if you want to obtain an integer value).

  While computing the integer value, getSignedValue uses either a classical
  binary representation for unsigned up/down values or 2's complement for signed
  up/down values. In the end, getSignedValue returns a signed (!) long long,
  irrespective of whether the up/down value itself is signed or unsigned.
*/
signed long long WIR_UpDownValue::getSignedValue( void ) const
{
  DSTART( "long long int WIR_UpDownValue::getSignedValue() const" );

  signed long long res = 0;
  unsigned long long mask = 1u;

  for ( unsigned int i = 0; i < mBitWidth; ++i, mask <<= 1 )
    // For a 1-bit in integer representation, do a classical base-number
    // decomposition.
    if ( mBitValues[ i ] == WIR_L4::b1 )
      res += mask;

  // Properly subtract 2^n, i.e., the current mask after exiting the above loop,
  // for negative signed integers in 2's complement.
  if ( mIsSigned && ( mBitValues[ mBitWidth - 1 ] == WIR_L4::b1 ) )
    res -= mask;

  return( res );
};


/*
  setSignedness sets the signedness of an up/down value.
*/
void WIR_UpDownValue::setSignedness( bool s )
{
  DSTART( "void WIR_UpDownValue::setSignedness(bool)" );

  mIsSigned = s;
};


/*
  isSigned returns whether an up/down value is signed or not.
*/
bool WIR_UpDownValue::isSigned( void ) const
{
  DSTART( "bool WIR_UpDownValue::isSigned() const" );

  return( mIsSigned );
};


/*
  isUnsigned returns whether an up/down value is unsigned or not.
*/
bool WIR_UpDownValue::isUnsigned( void ) const
{
  DSTART( "bool WIR_UpDownValue::isUnsigned() const" );

  return( !mIsSigned );
};


/*
  brief isInteger returns whether an up/down value represents an integer number,
  i.e., only contains 0, 1 or X bits.

  true if the up/down value is an integer, false below.
*/
bool WIR_UpDownValue::isInteger( void ) const
{
  DSTART( "bool WIR_UpDownValue::isInteger() const" );

  for ( unsigned int i = 0; i < mBitWidth; ++i )
    if ( ( mBitValues[ i ] != WIR_L4::b0 ) &&
         ( mBitValues[ i ] != WIR_L4::b1 ) &&
         ( mBitValues[ i ] != WIR_L4::bX ) )
      return( false );

  return( true );
};


/*
  brief isBinaryInteger returns whether an up/down value represents a true
  binary integer number, i.e., only contains 0 or 1 bits.

  true if the up/down value is a binary integer, false below.
*/
bool WIR_UpDownValue::isBinaryInteger( void ) const
{
  DSTART( "bool WIR_UpDownValue::isBinaryInteger() const" );

  for ( unsigned int i = 0; i < mBitWidth; ++i )
    if ( ( mBitValues[ i ] != WIR_L4::b0 ) &&
         ( mBitValues[ i ] != WIR_L4::b1 ) )
      return( false );

  return( true );
};


/*
  isPositive returns whether an up/down value is a positive integer number.
*/
bool WIR_UpDownValue::isPositive( void ) const
{
  DSTART( "bool WIR_UpDownValue::isPositive() const" );

  return(
    isUnsigned() ||
    ( isInteger() && ( mBitValues[ mBitWidth - 1 ] == WIR_L4::b0 ) ) );
};


/*
  isNegative returns whether an up/down value is a negative integer number.
*/
bool WIR_UpDownValue::isNegative( void ) const
{
  DSTART( "bool WIR_UpDownValue::isNegative() const" );

  return(
    isInteger() && isSigned() &&
    ( mBitValues[ mBitWidth - 1 ] == WIR_L4::b1 ) );
};


/*
  containsBit returns whether an up/down value contains the specified bit value.
*/
bool WIR_UpDownValue::containsBit( const WIR_L4 v ) const
{
  DSTART( "bool WIR_UpDownValue::containsBit(WIR_L4) const" );

  for ( unsigned int i = 0; i < mBitWidth; ++i )
    if ( mBitValues[ i ] == v )
      return( true );

  return( false );
};


/*
  containsBits returns whether an up/down value contains at least one of the
  specified bit values.
*/
bool WIR_UpDownValue::containsBits( WIR_L4Set &&v ) const
{
  DSTART( "bool WIR_UpDownValue::containsBits(WIR_L4Set&&) const" );

  for ( unsigned int i = 0; i < mBitWidth; ++i )
    if ( v.count( mBitValues[ i ] ) )
      return( true );

  return( false );
};


/*
  containsOnlyBit returns whether an up/down value contains only the specified
  bit value.
*/
bool WIR_UpDownValue::containsOnlyBit( const WIR_L4 v ) const
{
  DSTART( "bool WIR_UpDownValue::containsOnlyBit(WIR_L4) const" );

  for ( unsigned int i = 0; i < mBitWidth; ++i )
    if ( mBitValues[ i ] != v )
      return( false );

  return( true );
};


/*
  containsOnlyBits returns whether an up/down value contains only bit values
  from the specified set.
*/
bool WIR_UpDownValue::containsOnlyBits( WIR_L4Set &&v ) const
{
  DSTART( "bool WIR_UpDownValue::containsOnlyBits(WIR_L4Set&&) const" );

  for ( unsigned int i = 0; i < mBitWidth; ++i )
    if ( !v.count( mBitValues[ i ] ) )
      return( false );

  return( true );
};


/*
  isLocationBit returns whether the bit at the specified position is either a
  location or a negated location.
*/
bool WIR_UpDownValue::isLocationBit( unsigned int i ) const
{
  DSTART( "bool WIR_UpDownValue::isLocationBit(unsigned int) const" );

  auto b =
    #ifdef FAILSAFEMODE
    mBitValues.at( i );
    #else
    mBitValues[ i ];
    #endif

  return( ( b == WIR_L4::bL ) || ( b == WIR_L4::bN ) );
};


/*
  This operator compares two WIR_UpDownValues for provable, strictly bitwise
  equality.

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

  In all other cases, no clear statement about equality can be made so that the
  == operator returns U.
*/
WIR_L4 WIR_UpDownValue::operator == ( const WIR_UpDownValue &__o ) const
{
  DSTART( "WIR_L4 WIR_UpDownValue::operator==(const WIR_UpDownValue&) const" );

  if ( mBitWidth != __o.mBitWidth )
    return( WIR_L4::b0 );

  for ( unsigned int i = 0; i < mBitWidth; ++i ) {
    WIR_L4 b1 = mBitValues[ i ], b2 = __o.mBitValues[ i ];

    if ( ( b1 == WIR_L4::bU ) || ( b2 == WIR_L4::bU ) )
      // If either of the two currently compared bits are U, we don't know
      // absolutely anything about the bits and thus can't compare them.
      return( WIR_L4::bU );

    if ( getLevel( b1 ) != getLevel( b2 ) )
      // If X is compared with L or 1 is compared with N etc, we again don't
      // know anything about the outcome of the comparison.
      return( WIR_L4::bU );

    // From here on, both bits are from the same L4 level and can thus be
    // compared correctly.
    if ( ( getLevel( b1 ) == 2 ) && ( b1 != b2 ) )
      // The current bits are true 0 and 1, and they are different. Thus, the
      // two up/down values cannot be equal.
      return( WIR_L4::b0 );

    if ( getLevel( b1 ) == 1 ) {
      if ( mLocation[ i ].get() != __o.mLocation[ i ].get() )
        // The current bits are refering to different locations so that we can't
        // say anything about the comparison result.
        return( WIR_L4::bU );

      if ( b1 != b2 )
        // The current bits are refering to the same location, and they are
        // different. Thus, the two up/down values cannot be equal.
        return( WIR_L4::b0 );
    }
  }

  return( WIR_L4::b1 );
};


/*
  This operator compares two WIR_UpDownValues for provable, strictly bitwise
  inequality.

  The != operator returns 0 if the two up/down values have different bit widhts.

  The != operator returns 1 if for some bit position
  - one compared bit is 1 and the other is 0, OR
  - one compared bit is L and the other is N and they both refer to the very
    same location.
  The != operator returns 0 if for each bit position
  - both compared bits are 1 or both are 0, OR
  - either both compared bits are L or both are N and they both refer to the
    very same location.

  In all other cases, no clear statement about inequality can be made so that
  the != operator returns U.
*/
WIR_L4 WIR_UpDownValue::operator != ( const WIR_UpDownValue &__o ) const
{
  DSTART( "WIR_L4 WIR_UpDownValue::operator!=(const WIR_UpDownValue&) const" );

  if ( mBitWidth != __o.mBitWidth )
    return( WIR_L4::b0 );

  WIR_L4 res = WIR_L4::b0;
  for ( unsigned int i = 0; i < mBitWidth; ++i ) {
    WIR_L4 b1 = mBitValues[ i ], b2 = __o.mBitValues[ i ];

    if ( ( b1 == WIR_L4::bU ) || ( b2 == WIR_L4::bU ) ) {
      // If either of the two currently compared bits are U, we don't know
      // absolutely anything about the bits and thus can't compare them.
      res = WIR_L4::bU;
      continue;
    }

    if ( getLevel( b1 ) != getLevel( b2 ) ) {
      // If X is compared with L or 1 is compared with N etc, we again don't
      // know anything about the outcome of the comparison.
      res = WIR_L4::bU;
      continue;
    }

    // From here on, both bits are from the same L4 level and can thus be
    // compared correctly.
    if ( ( getLevel( b1 ) == 2 ) && ( b1 != b2 ) )
      // The current bits are true 0 and 1, and they are different. Thus, the
      // two up/down values must be inequal.
      return( WIR_L4::b1 );

    if ( getLevel( b1 ) == 1 ) {
      if ( ( b1 != b2 ) &&
           ( mLocation[ i ].get() == __o.mLocation[ i ].get() ) )
        // The current bits are refering to the same location, and they are
        // different. Thus, the two up/down values must be inequal.
        return( WIR_L4::b1 );
      else

      if ( mLocation[ i ].get() != __o.mLocation[ i ].get() )
        // The current bits are refering to different locations so that we can't
        // say anything about the comparison result.
        res = WIR_L4::bU;
    }
  }

  return( res );
};


/*
  This operator performs a provable greater-equal comparison of two
  WIR_UpDownValues.

  If any operand of the >= operator contains U, L or N bits, i.e., is not an
  integer number, no greater-equal comparison can be performed and the operator
  returns U. Otherwise, the integer values of both operands are compared and 0
  or 1 is returned.
*/
WIR_L4 WIR_UpDownValue::operator >= ( const WIR_UpDownValue &__o ) const
{
  DSTART( "WIR_L4 WIR_UpDownValue::operator>=(const WIR_UpDownValue&) const" );

  if ( !isInteger() || !__o.isInteger() )
    return( WIR_L4::bU );

  return( getSignedValue() >= __o.getSignedValue() ? WIR_L4::b1 : WIR_L4::b0 );
};


/*
  This operator performs a provable less-than comparison of two
  WIR_UpDownValues.

  If any operand of the < operator contains U, L or N bits, i.e., is not an
  integer number, no less-than comparison can be performed and the operator
  returns U. Otherwise, the integer values of both operands are compared and 0
  or 1 is returned.
*/
WIR_L4 WIR_UpDownValue::operator < ( const WIR_UpDownValue &__o ) const
{
  DSTART( "WIR_L4 WIR_UpDownValue::operator<(const WIR_UpDownValue&) const" );

  if ( !isInteger() || !__o.isInteger() )
    return( WIR_L4::bU );

  return( getSignedValue() < __o.getSignedValue() ? WIR_L4::b1 : WIR_L4::b0 );
};


/*
  This operator performs a greater-than comparison of two up/down values.

  An up/down value is greater than another value if at least one bit in LHS is
  greater than the corresponding bit in RHS, and if no bit from LHS is less than
  RHS.

  Since a greater-than comparison of up/down values of different bit widths does
  not make sense, this operator returns false in this case.
*/
bool WIR_UpDownValue::operator > ( const WIR_UpDownValue &__o ) const
{
  DSTART( "bool WIR_UpDownValue::operator>(const WIR_UpDownValue&) const" );

  if ( mBitWidth != __o.mBitWidth )
    return( false );

  bool greaterBitFound = false;

  for ( unsigned int i = 0; i < mBitWidth; ++i ) {
    WIR_L4 b1 = mBitValues[ i ], b2 = __o.mBitValues[ i ];

    if ( b1 < b2 )
      return( false );

    if ( b1 > b2 )
      greaterBitFound = true;
  }

  return( greaterBitFound );
};


/*
  isEqual compares two WIR_UpDownValues for "relaxed" bitwise equality.

  In order to be equal, two up/down values must have the same bit widths and
  have to contain exactly the same bit values where U bits are considered equal
  by default. Furthermore, L or N bits have to refer to exactly the same
  register parameter and bit position therein in order to be equal.
*/
bool WIR_UpDownValue::isEqual( const WIR_UpDownValue &__o ) const
{
  DSTART( "bool WIR_UpDownValue::isEqual(const WIR_UpDownValue&) const" );

  if ( mBitWidth != __o.mBitWidth )
    return( false );

  for ( unsigned int i = 0; i < mBitWidth; ++i ) {
    WIR_L4 b1 = mBitValues[ i ], b2 = __o.mBitValues[ i ];

    if ( b1 != b2 )
      return( false );

    if ( ( b1 == WIR_L4::bL ) || ( b1 == WIR_L4::bN ) ) {
      // In order to check L or N bits for equality, we check the location's
      // refering register parameter and the bit position therein.
      if ( mLocation[ i ].get() != __o.mLocation[ i ].get() )
        return( false );
    }
  }

  return( true );
};


/*
  This operator performs a bitwise AND of two up/down values.

  This operator asserts if up/down values of different bit widths are combined.
*/
WIR_UpDownValue WIR_UpDownValue::operator & ( const WIR_UpDownValue &__o ) const
{
  DSTART(
    "WIR_UpDownValue WIR_UpDownValue::operator&(const WIR_UpDownValue&) const" );

  #ifdef FAILSAFEMODE
  ufAssert( mBitWidth == __o.mBitWidth );
  #endif

  WIR_UpDownValue res { WIR_L4::bU, mBitWidth, mIsSigned };

  for ( unsigned int i = 0; i < mBitWidth; ++i ) {
    auto bitInfo =
      bitAND(
        { mBitValues[ i ], mLocation[ i ] },
        { __o.mBitValues[ i ], __o.mLocation[ i ] } );

    res[ i ] = bitInfo.first;
    res.mLocation[ i ] = bitInfo.second;
  }

  return( res );
};


/*
  This operator performs a bitwise OR of two up/down values.

  This operator asserts if up/down values of different bit widths are combined.
*/
WIR_UpDownValue WIR_UpDownValue::operator | ( const WIR_UpDownValue &__o ) const
{
  DSTART(
    "WIR_UpDownValue WIR_UpDownValue::operator|(const WIR_UpDownValue&) const" );

  #ifdef FAILSAFEMODE
  ufAssert( mBitWidth == __o.mBitWidth );
  #endif

  WIR_UpDownValue res { WIR_L4::bU, mBitWidth, mIsSigned };

  for ( unsigned int i = 0; i < mBitWidth; ++i ) {
    auto bitInfo =
      bitOR(
        { mBitValues[ i ], mLocation[ i ] },
        { __o.mBitValues[ i ], __o.mLocation[ i ] } );

    res[ i ] = bitInfo.first;
    res.mLocation[ i ] = bitInfo.second;
  }

  return( res );
};


/*
  This operator performs a bitwise XOR of two up/down values.

  This operator asserts if up/down values of different bit widths are combined.
*/
WIR_UpDownValue WIR_UpDownValue::operator ^ ( const WIR_UpDownValue &__o ) const
{
  DSTART(
    "WIR_UpDownValue WIR_UpDownValue::operator^(const WIR_UpDownValue&) const" );

  #ifdef FAILSAFEMODE
  ufAssert( mBitWidth == __o.mBitWidth );
  #endif

  WIR_UpDownValue res { WIR_L4::bU, mBitWidth, mIsSigned };

  for ( unsigned int i = 0; i < mBitWidth; ++i ) {
    auto bitInfo =
      bitXOR(
        { mBitValues[ i ], mLocation[ i ] },
        { __o.mBitValues[ i ], __o.mLocation[ i ] } );

    res[ i ] = bitInfo.first;
    res.mLocation[ i ] = bitInfo.second;
  }

  return( res );
};


/*
  This operator performs a bitwise negation of an up/down value.
*/
WIR_UpDownValue WIR_UpDownValue::operator ~ ( void ) const
{
  DSTART( "WIR_UpDownValue WIR_UpDownValue::operator~() const");

  WIR_UpDownValue res { *this };

  for ( unsigned int i = 0; i < mBitWidth; ++i )
    res[ i ] = ~res[ i ];

  return( res );
};


/*
  This operator performs a bitwise left shift of an up/down value by an integer
  shift amount.

  A signed and negative shift amount is treated as a right shift by the positive
  shift amount. When shifting right, the vacated bits are filled either with '0'
  or with this up/down value's sign bit, depending on the value's signedness.
*/
WIR_UpDownValue WIR_UpDownValue::operator << ( int shamt ) const
{
  DSTART( "WIR_UpDownValue WIR_UpDownValue::operator<<(int) const" );

  if ( shamt == 0 )
    // Nothing to do for a shift amount of 0.
    return( *this );

  WIR_UpDownValue res { WIR_L4::bU, mBitWidth, mIsSigned };

  if ( shamt > 0 ) {
    // Perform the desired left shift.
    for ( unsigned int i = mBitWidth - 1; i >= (unsigned int) shamt; --i ) {
      res.mBitValues[ i ] = mBitValues[ i - shamt ];
      res.mLocation[ i ] = mLocation[ i - shamt ];
    }
    for ( unsigned int i = 0; i < (unsigned int) shamt; ++i )
      // Fill least-significant bits with '0' for a left shift.
      if ( i < mBitWidth )
        res.mBitValues[ i ] = WIR_L4::b0;
  } else {
    // Perform a right shift due to the negative shift amount. Note that shamt
    // contains a negative value which explains the seemingly incorrect use of
    // '+' and '-' in the following code.
    for ( int i = 0; i < (int)( mBitWidth + shamt ); ++i ) {
      res.mBitValues[ i ] = mBitValues[ i - shamt ];
      res.mLocation[ i ] = mLocation[ i - shamt ];
    }
    for ( int i = mBitWidth + shamt; i < (int) mBitWidth; ++i )
      // Fill most-significant bits either with sign bit or with '0' for a right
      // shift.
      if ( i >= 0 ) {
        if ( mIsSigned ) {
          res.mBitValues[ i ] = mBitValues[ mBitWidth - 1 ];
          res.mLocation[ i ] = mLocation[ mBitWidth - 1 ];
        } else
          res.mBitValues[ i ] = WIR_L4::b0;
      }
  }

  return( res );
};


/*
  This operator performs a bitwise left shift of two up/down values.

  If the shift amount is not an integer, i.e., contains 'U', 'L' or 'N' bits,
  the result of the shift operator is fully unknown.

  'X' bits in the shift amount are interpreted either as '1' for positive shift
  amount values or as '0' for negative shift amount values. This maximizes the
  shift amount's absolute value and thus leads to a maximal number of '0' bits
  in the shifted output.

  A signed and negative shift amount is treated as a right shift by the positive
  shift amount.
*/
WIR_UpDownValue WIR_UpDownValue::operator << ( const WIR_UpDownValue &__o ) const
{
  DSTART(
    "WIR_UpDownValue WIR_UpDownValue::operator<<(const WIR_UpDownValue&) const" );

  // Take a copy of the shift amount for its proper interpretation.
  auto shamt { __o };

  // Determine whether shift amount is a positive or negative integer number.
  bool positiveShamt =
    shamt.isUnsigned() ||
    ( shamt.mBitValues[ shamt.mBitWidth - 1 ] != WIR_L4::b1 );

  // Iterate the shift amount.
  for ( unsigned int i = 0; i < shamt.mBitWidth; ++i ) {
    // If we encounter a 'U', 'L' or 'N' bit in the shift amount, return U*.
    if ( ( shamt.mBitValues[ i ] == WIR_L4::bU ) ||
         ( shamt.mBitValues[ i ] == WIR_L4::bL ) ||
         ( shamt.mBitValues[ i ] == WIR_L4::bN ) )
      return( WIR_UpDownValue( WIR_L4::bU, mBitWidth, mIsSigned ) );

    // Replace 'X' either by '1' or '0', depending on whether the shift amount
    // is a positive number or not.
    if ( shamt.mBitValues[ i ] == WIR_L4::bX ) {
      if ( i == shamt.mBitWidth - 1 )
        // Special handling of the most-significant sign bit that is 'X' here.
        shamt.mBitValues[ i ] = shamt.isUnsigned() ? WIR_L4::b1 : WIR_L4::b0;
      else
        shamt.mBitValues[ i ] = positiveShamt ? WIR_L4::b1 : WIR_L4::b0;
    }
  }

  // Finally, get the shift amount's value.
  signed long long shiftVal = shamt.getSignedValue();
  return( *this << shiftVal );
};


/*
  This operator performs a bitwise right shift of an up/down value by an integer
  shift amount.

  A signed and negative shift amount is treated as a left shift by the positive
  shift amount.

  Depending on whether the up/down value to be shifted is signed or unsigned,
  either arithmetical or logical right shifting is applied.
*/
WIR_UpDownValue WIR_UpDownValue::operator >> ( int shamt ) const
{
  DSTART( "WIR_UpDownValue WIR_UpDownValue::operator>>(int) const" );

  if ( shamt == 0 )
    // Nothing to do for a shift amount of 0.
    return( *this );

  if ( (unsigned int) std::abs( shamt ) >= mBitWidth )
    return( WIR_UpDownValue( WIR_L4::b0, mBitWidth, mIsSigned ) );

  WIR_UpDownValue res { WIR_L4::bU, mBitWidth, mIsSigned };

  if ( shamt > 0 ) {
    // Perform the desired right shift.
    for ( unsigned int i = 0; i < mBitWidth - shamt; ++i ) {
      res.mBitValues[ i ] = mBitValues[ i + shamt ];
      res.mLocation[ i ] = mLocation[ i + shamt ];
    }

    for ( unsigned int i = mBitWidth - shamt; i < mBitWidth; ++i ) {
      // Fill most-significant bits either with sign bit or with '0' for a right
      // shift.
      res.mBitValues[ i ] =
        mIsSigned ? mBitValues[ mBitWidth - 1 ] : WIR_L4::b0;

      // Pay attention if sign bit is a location.
      if ( mIsSigned && isLocationBit( mBitWidth - 1 ) )
        res.setBit(
          i, mBitValues[ mBitWidth - 1 ], getLocation( mBitWidth - 1 ) );
    }
  } else {
    // Perform a left shift due to the negative shift amount.
    for ( unsigned int i = mBitWidth - 1; i >= (unsigned int) -shamt; --i ) {
      res.mBitValues[ i ] = mBitValues[ i + shamt ];
      res.mLocation[ i ] = mLocation[ i + shamt ];
    }
    for ( unsigned int i = 0; i < (unsigned int) -shamt; ++i )
      // Fill least-significant bits with '0' for a left shift.
      res.mBitValues[ i ] = WIR_L4::b0;
  }

  return( res );
};


/*
  This operator performs a bitwise right shift of two up/down values.

  If the shift amount is not an integer, i.e., contains 'U', 'L' or 'N' bits,
  the result of the shift operator is fully unknown.

  'X' bits in the shift amount are interpreted either as '1' for positive shift
  amount values or as '0' for negative shift amount values. This maximizes the
  shift amount's absolute value and thus leads to a maximal number of '0' bits
  in the shifted output.

  A signed and negative shift amount is treated as a left shift by the positive
  shift amount.

  Depending on whether the up/down value to be shifted is signed or unsigned,
  either arithmetical or logical right shifting is applied.
*/
WIR_UpDownValue WIR_UpDownValue::operator >> ( const WIR_UpDownValue &__o ) const
{
  DSTART(
    "WIR_UpDownValue WIR_UpDownValue::operator>>(const WIR_UpDownValue&) const" );

  // Take a copy of the shift amount for its proper interpretation.
  auto shamt { __o };

  // Determine whether shift amount is a positive or negative integer number.
  bool positiveShamt =
    shamt.isUnsigned() ||
    ( shamt.mBitValues[ shamt.mBitWidth - 1 ] != WIR_L4::b1 );

  // Iterate the shift amount.
  for ( unsigned int i = 0; i < shamt.mBitWidth; ++i ) {
    // If we encounter a 'U', 'L' or 'N' bit in the shift amount, return U*.
    if ( ( shamt.mBitValues[ i ] == WIR_L4::bU ) ||
         ( shamt.mBitValues[ i ] == WIR_L4::bL ) ||
         ( shamt.mBitValues[ i ] == WIR_L4::bN ) )
      return( WIR_UpDownValue( WIR_L4::bU, mBitWidth, mIsSigned ) );

    // Replace 'X' either by '1' or '0', depending on whether the shift amount
    // is a positive number or not.
    if ( shamt.mBitValues[ i ] == WIR_L4::bX ) {
      if ( i == mBitWidth - 1 )
        // Special handling of the most-significant sign bit that is 'X' here.
        shamt.mBitValues[ i ] = shamt.isUnsigned() ? WIR_L4::b1 : WIR_L4::b0;
      else
        shamt.mBitValues[ i ] = positiveShamt ? WIR_L4::b1 : WIR_L4::b0;
    }
  }

  // Finally, get the shift amount's value.
  signed long long shiftVal = shamt.getSignedValue();
  return( *this >> shiftVal );
};


/*
  This operator performs a bitwise addition of two up/down values.

  This operator asserts if up/down values of different bit widths are combined.

  The result is an unsigned number if both operands are unsigned, otherwise the
  result is signed.
*/
WIR_UpDownValue WIR_UpDownValue::operator + ( const WIR_UpDownValue &__o ) const
{
  DSTART(
    "WIR_UpDownValue WIR_UpDownValue::operator+(const WIR_UpDownValue&) const" );

  #ifdef FAILSAFEMODE
  ufAssert( mBitWidth == __o.mBitWidth );
  #endif

  WIR_UpDownValue res { WIR_L4::bU, mBitWidth, mIsSigned || __o.mIsSigned };

  // Do bitwise addition based on a full-adder with inputs a, b, cin and
  // outputs s, cout:
  // s    = (a ^ b) ^ cin
  // cout = (a & b) | ((a ^ b) & cin)
  WIR_BitInfo carry { WIR_L4::b0, boost::none };

  for ( unsigned int i = 0; i < mBitWidth; ++i ) {
    auto b1 = mBitValues[ i ], b2 = __o.mBitValues[ i ];

    // Compute sum s first.
    // Step 1: a ^ b
    auto aXORb = bitXOR( { b1, mLocation[ i ] }, { b2, __o.mLocation[ i ] } );

    // Step 2: (a ^ b) ^ cin
    auto sum = bitXOR( aXORb, carry );
    res.mBitValues[ i ] = sum.first;
    res.mLocation[ i ] = sum.second;

    // Compute carry bit next.
    // Step 1: a & b
    auto aANDb = bitAND( { b1, mLocation[ i ] }, { b2, __o.mLocation[ i ] } );

    // Step 2: (a ^ b) & cin
    auto aXORbANDc = bitAND( aXORb, carry );

    // Step 3: (a & b) | ((a ^ b) & cin)
    carry = bitOR( aANDb, aXORbANDc );
  }

  return( res );
};


/*
  This operator performs an arithmetical unary minus of an up/down value.

  Due to the nature of the unary arithmetical minus, the result's bit width
  equals that of its operand, and the result is always signed. The unary minus
  is computed by applying 2's-complement representation, i.e., all bits are
  inverted and the value +1 is added afterwards.
*/
WIR_UpDownValue WIR_UpDownValue::operator - ( void ) const
{
  DSTART( "WIR_UpDownValue WIR_UpDownValue::operator-() const" );

  WIR_UpDownValue one { WIR_L4::b0, mBitWidth, true };
  one[ 0 ] = WIR_L4::b1;

  return( ~( *this ) + one );
};


/*
  This operator performs a bitwise subtraction of two up/down values.

  This operator asserts if up/down values of different bit widths are combined.

  Due to the nature of the arithmetical minus, the result is always signed. The
  subtraction a - b is reduced to the addition a + (-b).
*/
WIR_UpDownValue WIR_UpDownValue::operator - ( const WIR_UpDownValue &__o ) const
{
  DSTART(
    "WIR_UpDownValue WIR_UpDownValue::operator-(const WIR_UpDownValue&) const" );

  return( *this + (-__o) );
};


/*
  This operator performs a bitwise multiplication of two up/down values.

  This operator asserts if up/down values of different bit widths are combined.

  The result is an unsigned number if both operands are unsigned, otherwise the
  result is signed.

  Currently, this operator computes no actual multiplication but returns U*
  instead.
*/
WIR_UpDownValue WIR_UpDownValue::operator * ( const WIR_UpDownValue &__o ) const
{
  DSTART(
    "WIR_UpDownValue WIR_UpDownValue::operator*(const WIR_UpDownValue&) const" );

  #ifdef FAILSAFEMODE
  ufAssert( mBitWidth == __o.mBitWidth );
  #endif

  return(
    WIR_UpDownValue( { WIR_L4::bU, mBitWidth, mIsSigned || __o.mIsSigned } ) );
};


/*
  This operator performs a bitwise division of two up/down values.

  This operator asserts if up/down values of different bit widths are combined.

  The result is an unsigned number of both operands are unsigned, otherwise the
  result is signed.

  Currently, this operator computes no actual division but returns U* instead.
*/
WIR_UpDownValue WIR_UpDownValue::operator / ( const WIR_UpDownValue &__o ) const
{
  DSTART(
    "WIR_UpDownValue WIR_UpDownValue::operator/(const WIR_UpDownValue&) const" );

  #ifdef FAILSAFEMODE
  ufAssert( mBitWidth == __o.mBitWidth );
  #endif

  return(
    WIR_UpDownValue( { WIR_L4::bU, mBitWidth, mIsSigned || __o.mIsSigned } ) );
};


/*
  This operator performs a bitwise modulo computation of two up/down values.

  This operator asserts if up/down values of different bit widths are combined.

  The result of this operator is always an unsigned number.

  Currently, this operator computes no actual modulo but returns U* instead.
*/
WIR_UpDownValue WIR_UpDownValue::operator % ( const WIR_UpDownValue &__o ) const
{
  DSTART(
    "WIR_UpDownValue WIR_UpDownValue::operator%(const WIR_UpDownValue&) const" );

  (void) __o;

  #ifdef FAILSAFEMODE
  ufAssert( mBitWidth == __o.mBitWidth );
  #endif

  return( WIR_UpDownValue( { WIR_L4::bU, mBitWidth, true } ) );
};


/*
  extend extends an up/down value to a given bit width.

  extend asserts if the given bit width is smaller than the up/down value's
  current width.

  Depending on whether the up/down value is signed or not, either sign
  extension or 0-extension is performed.
*/
WIR_UpDownValue WIR_UpDownValue::extend( unsigned int s ) const
{
  DSTART( "WIR_UpDownValue WIR_UpDownValue::extend(unsigned int) const" );

  #ifdef FAILSAFEMODE
  ufAssert( s >= mBitWidth );
  #endif

  WIR_UpDownValue res { WIR_L4::b0, s, mIsSigned };

  res.mIsDirty = true;

  for ( unsigned int i = 0; i < mBitWidth; ++i ) {
    res.mBitValues[ i ] = mBitValues[ i ];
    res.mLocation[ i ] = mLocation[ i ];
  }

  if ( mIsSigned )
    for ( unsigned int i = mBitWidth; i < s; ++i ) {
      // Do sign extension.
      res.mBitValues[ i ] = mBitValues[ mBitWidth - 1 ];
      res.mLocation[ i ] = mLocation[ mBitWidth - 1 ];
    }

  return( res );
};


/*
  abs returns the absolute value of an up/down value.

  Due to the nature of the abs operator, the result's bit width equals that of
  its operand, and the result is always signed.
*/
WIR_UpDownValue abs( const WIR_UpDownValue &__o )
{
  DSTART( "WIR_UpDownValue abs(const WIR_UpDownValue&)" );

  // For abs, we have to interprete the operand as signed value.
  auto v = __o;
  v.mIsSigned = true;

  if ( v.isPositive() )
    return( v );

  if ( v.isNegative() )
    return( -v );

  return( WIR_UpDownValue( WIR_L4::bU, v.mBitWidth, true ) );
};


/*
  combine returns the "smallest common" combination of two up/down values.

  The com operator is defined in Jens Wagner, Retargierbare Ausnutzung von
  Spezialoperationen für Eingebettete Systeme mit Hilfe bitgenauer
  Wertflussanalyse, page 175, Figure 4.18.

  This function asserts if up/down values of different bit widths are combined.
*/
WIR_UpDownValue WIR_UpDownValue::combine( const WIR_UpDownValue &__o ) const
{
  DSTART(
    "WIR_UpDownValue WIR_UpDownValue::combine(const WIR_UpDownValue&) const" );

  #ifdef FAILSAFEMODE
  ufAssert( mBitWidth == __o.mBitWidth );
  #endif

  WIR_UpDownValue res { WIR_L4::bU, mBitWidth, mIsSigned };

  for ( unsigned int i = 0; i < mBitWidth; ++i ) {
    auto bitInfo =
      bitCom(
        { mBitValues[ i ], mLocation[ i ] },
        { __o.mBitValues[ i ], __o.mLocation[ i ] } );

    res[ i ] = bitInfo.first;
    res.mLocation[ i ] = bitInfo.second;
  }

  return( res );
};


/*
  extract performs extraction of a bit packet from an up/down value.

  This function asserts if offset and width exceed the up/down value's bit
  width.
*/
WIR_UpDownValue WIR_UpDownValue::extract( unsigned int o,
                                          unsigned int w ) const
{
  DSTART(
    "WIR_UpDownValue WIR_UpDownValue::extract(unsigned int, unsigned int) const" );

  #ifdef FAILSAFEMODE
  ufAssert( mBitWidth >= o + w );
  #endif

  WIR_UpDownValue res { WIR_L4::bU, w, mIsSigned };

  for ( unsigned int i = 0; i < w; ++i ) {
    res[ i ] = mBitValues[ i + o ];
    res.mLocation[ i ] = mLocation[ i + o ];
  }

  return( res );
};


/*
  insert inserts an up/down value into another up/down value at the given bit
  offset.

  insert asserts if bit width of the operand and the offset exceed the up/down
  value's bit width.
*/
void insert( WIR_UpDownValue &r, const WIR_UpDownValue &v, unsigned int o )
{
  DSTART(
    "void insert(WIR_UpDownValue&, const WIR_UpDownValue&, unsigned int)" );

  #ifdef FAILSAFEMODE
  ufAssert( r.getBitWidth() >= v.getBitWidth() + o );
  #endif

  for ( unsigned int i = 0; i < v.getBitWidth(); ++i ) {
    r[ i + o ] = v.mBitValues[ i ];
    r.mLocation[ i + o ] = v.mLocation[ i ];
  }
};


/*
  replaceUByLocation replaces each U bit of an up/down value by an L location
  refering to the given register parameter.
*/
void WIR_UpDownValue::replaceUByLocation( const WIR_RegisterParameter &p )
{
  DSTART(
    "void WIR_UpDownValue::replaceUByLocation(const WIR_RegisterParameter&)" );

  for ( unsigned int i = 0; i < mBitWidth; ++i )
    if ( mBitValues[ i ] == WIR_L4::bU )
      setBit( i, WIR_L4::bL, { p, i } );
};


/*
  replaceLocationByU replaces those L or N bits of an up/down value by U that
  refer to the given WIR operation.
*/
void WIR_UpDownValue::replaceLocationByU( const WIR_Operation &o )
{
  DSTART( "void WIR_UpDownValue::replaceLocationByU(const WIR_Operation&)" );

  for ( unsigned int i = 0; i < mBitWidth; ++i )
    if ( ( mBitValues[ i ] == WIR_L4::bL ) ||
         ( mBitValues[ i ] == WIR_L4::bN ) ) {
      auto &l = getLocation( i );

      if ( l.isRegisterParameter() &&
           ( l.getRegisterParameter().getOperation() == o ) )
        setBit( i, WIR_L4::bU );
    }
};


/*
  The << operator dumps an up/down value to an output stream.

  The format of the produced output strongly depends on the actual up/down
  value. If the up/down value is a true binary integer number, i.e., only
  consists of 0 and 1 bits, its numerical integer value is output, either in 2's
  complement or as natural integer depending on its signedness. Otherwise, if
  the up/down value also contains U, L, N or X bits, a bitwise output is
  produced with the least-significant bit being displayed at the right-most
  side.
*/
std::ostream & operator << ( std::ostream &os, const WIR_UpDownValue &v )
{
  DSTART( "ostream& operator<<(ostream&, const WIR_UpDownValue&)" );

  bool isPureInt = true;
  signed long long signedVal = 0;
  unsigned long long unsignedVal = 0;
  list<string> bitVal;
  WIR_L4Set usedBits;

  unsigned long long mask = 1u;

  // Some data structures for bookkeeping whether we found a bit packet (i.e., a
  // sequence of either L or N bits, all of them refering to the same register
  // but with consecutive bit offsets.
  bool currentlyInBitPacket = false;
  WIR_L4 currentBitPacketLN = WIR_L4::bU;
  WIR_ID_API *currentBitPacket = nullptr;
  WIR_LocationType currentBitPacketType;
  unsigned int currentBitPacketLSB = v.mBitWidth + 1;
  unsigned int currentBitPacketMSB = v.mBitWidth + 1;

  // A small lambda to produce compact output for a detected bit packet.
  auto dumpBitPacket = [&]( stringstream &str ) {
    str << currentBitPacketLN << "( ";
    if ( currentBitPacketType == WIR_LocationType::reg ) {
      auto &p = *(dynamic_cast<WIR_RegisterParameter *>( currentBitPacket ));
      auto &r = p.getRegister();
      str << r.getName();
      if ( r.isVirtual() ) {
        auto &vreg = dynamic_cast<WIR_VirtualRegister &>( r );
        if ( vreg.isPrecolored() )
          str << "/" << vreg.getPrecolor().getName();
      }
    } else {
      auto &s = *(dynamic_cast<WIR_Symbol *>( currentBitPacket ));
      str << "&amp;" << s.getName();
    }
    str << "[ " << currentBitPacketMSB;
    if ( currentBitPacketMSB != currentBitPacketLSB )
      str << " - " << currentBitPacketLSB;
    str << " ] )";
  };

  for ( unsigned int i = 0; i < v.mBitWidth; ++i, mask <<= 1 ) {
    WIR_L4 b = const_cast<WIR_UpDownValue &>( v )[ i ];
    usedBits.insert( b );

    // Prepare bitwise output of current bit.
    stringstream str;

    // Prepare numerical output.
    if ( ( b == WIR_L4::b0 ) || ( b == WIR_L4::b1 ) ) {
      // For a 1-bit in integer representation, do a classical base-number
      // decomposition.
      if ( v.isUnsigned() && ( b == WIR_L4::b1 ) )
        unsignedVal += mask;
      else

      if ( v.isSigned() && ( b == WIR_L4::b1 ) )
        signedVal += mask;

      // Add b to bitwise output.
      str << b;

      // Check whether the current 0 or 1 bit ends a current bit packet.
      if ( currentlyInBitPacket ) {
        // Output the current bit packet and terminate it.
        dumpBitPacket( str );
        currentlyInBitPacket = false;
      }
    } else {
      isPureInt = false;

      // For L and N, add refered location to bitwise output.
      if ( ( b == WIR_L4::bL ) || ( b == WIR_L4::bN ) ) {
        auto &l = v.getLocation( i );

        if ( !currentlyInBitPacket ) {
          // We encountered a start of a bit packet.
          currentlyInBitPacket = true;

          currentBitPacketLN = b;
          if ( l.isRegisterParameter() )
            currentBitPacket =
              const_cast<WIR_RegisterParameter *>(
                &(l.getRegisterParameter() ) );
          else
            currentBitPacket = const_cast<WIR_Symbol *>( &(l.getSymbol() ) );
          currentBitPacketType = l.getType();
          currentBitPacketLSB = l.getBitPosition();
          currentBitPacketMSB = l.getBitPosition();
        } else {
          // We are currently within a bit packet. Does the current bit b extend
          // the current bit packet, or does a new bit packet start here?
          if ( ( b == currentBitPacketLN ) &&
               ( ( l.isRegisterParameter() &&
                   ( l.getRegisterParameter().getID() ==
                       currentBitPacket->getID() ) ) ||
                 ( l.isSymbol() &&
                   ( l.getSymbol().getID() == currentBitPacket->getID() ) ) ) &&
               ( l.getBitPosition() == currentBitPacketMSB + 1 ) )
            // The current bit packet is extended by the current bit b.
            ++currentBitPacketMSB;
          else {
            // The locations don't match, a new bit packet starts here. Output
            // the previous bit packet.
            dumpBitPacket( str );

            currentBitPacketLN = b;
            if ( l.isRegisterParameter() )
              currentBitPacket =
                const_cast<WIR_RegisterParameter *>(
                  &(l.getRegisterParameter() ) );
            else
              currentBitPacket = const_cast<WIR_Symbol *>( &(l.getSymbol() ) );
            currentBitPacketType = l.getType();
            currentBitPacketLSB = l.getBitPosition();
            currentBitPacketMSB = l.getBitPosition();
          }
        }

        // When being at the left-most bit position, make sure that the current
        // bit packet is also properly displayed.
        if ( currentlyInBitPacket && ( i == v.mBitWidth - 1 ) )
          dumpBitPacket( str );
      } else {
        // Add b to bitwise output.
        str << b;

        // Check whether the current U or X bit ends a current bit packet.
        if ( currentlyInBitPacket ) {
          // Output the current bit packet and terminate it.
          dumpBitPacket( str );
          currentlyInBitPacket = false;
        }
      }
    }

    // Every 4-th bit position, add a whitespace to the bitwise output for the
    // sake of legibility.
    if ( ( i != 0 ) && ( i % 4 == 0 ) && ( !str.str().empty() ) )
      str << " ";

    // Prepend bitwise output by current bit.
    bitVal.push_front( str.str() );
  }

  // Properly subtract 2^n, i.e., the current mask after exiting the above loop,
  // for negative signed integers in 2's complement.
  if ( v.isSigned() &&
       ( const_cast<WIR_UpDownValue &>( v )[ v.getBitWidth() - 1 ] ==
           WIR_L4::b1 ) )
    signedVal -= mask;

  // Do the final output.
  if ( isPureInt ) {
    // Numerical output.
    if ( v.isSigned() )
      os << signedVal;
    else
      os << unsignedVal;
  } else

  if ( ( usedBits.size() == 1 ) && ( v.mBitWidth > 3 ) &&
       ( ( *(usedBits.begin()) == WIR_L4::bU ) ||
         ( *(usedBits.begin()) == WIR_L4::bX ) ) )
    // U^n or X^n, respectively.
    os << *(usedBits.begin()) << "^" << v.mBitWidth;
  else {
    stringstream str;

    // Bitwise output.
    for ( auto s : bitVal )
      str << s;
    os << trim( str.str() );
  }

  return( os );
};


//
// Private class methods
//

/*
  bitAND is an internal static helper routine computing the bitwise AND under
  full consideration of eventually provided bit locations.
*/
WIR_UpDownValue::WIR_BitInfo WIR_UpDownValue::bitAND( WIR_BitInfo i1,
                                                      WIR_BitInfo i2 )
{
  DSTART(
    "static WIR_UpDownValue::WIR_BitInfo WIR_UpDownValue::bitAND(WIR_UpDownValue::WIR_BitInfo, WIR_UpDownValue::WIR_BitInfo)" );

  WIR_BitInfo res = make_pair( i1.first & i2.first, boost::none );
  auto levelRes = getLevel( res.first );

  // If [L|N] AND [L|N] is performed, check whether the involved locations are
  // inequal. If so, the result of AND must be 'U'.
  if ( ( getLevel( i1.first ) == 1 ) && ( getLevel( i2.first ) == 1 ) &&
       ( i1.second.get() != i2.second.get() ) ) {
    res.first = WIR_L4::bU;
    levelRes = 0;
  }

  // If a result bit finally is either 'L' or 'N', update the result's location.
  if ( levelRes == 1 )
    res.second =
      ( getLevel( i1.first ) == 1 ) ? i1.second : i2.second;

  return( res );
};


/*
  bitOR is an internal static helper routine computing the bitwise OR under full
  consideration of eventually provided bit locations.
*/
WIR_UpDownValue::WIR_BitInfo WIR_UpDownValue::bitOR( WIR_BitInfo i1,
                                                     WIR_BitInfo i2 )
{
  DSTART(
    "static WIR_UpDownValue::WIR_BitInfo WIR_UpDownValue::bitOR(WIR_UpDownValue::WIR_BitInfo, WIR_UpDownValue::WIR_BitInfo)" );

  WIR_BitInfo res = make_pair( i1.first | i2.first, boost::none );
  auto levelRes = getLevel( res.first );

  // If [L|N] OR [L|N] is performed, check whether the involved locations are
  // inequal. If so, the result of OR must be 'U'.
  if ( ( getLevel( i1.first ) == 1 ) && ( getLevel( i2.first ) == 1 ) &&
       ( i1.second.get() != i2.second.get() ) ) {
    res.first = WIR_L4::bU;
    levelRes = 0;
  }

  // If a result bit finally is either 'L' or 'N', update the result's location.
  if ( levelRes == 1 )
    res.second =
      ( getLevel( i1.first ) == 1 ) ? i1.second : i2.second;

  return( res );
};


/*
  bitXOR is an internal static helper routine computing the bitwise XOR under
  full consideration of eventually provided bit locations.
*/
WIR_UpDownValue::WIR_BitInfo WIR_UpDownValue::bitXOR( WIR_BitInfo i1,
                                                      WIR_BitInfo i2 )
{
  DSTART(
    "static WIR_UpDownValue::WIR_BitInfo WIR_UpDownValue::bitXOR(WIR_UpDownValue::WIR_BitInfo, WIR_UpDownValue::WIR_BitInfo)" );

  WIR_BitInfo res = make_pair( i1.first ^ i2.first, boost::none );
  auto levelRes = getLevel( res.first );

  // If [L|N] XOR [L|N] is performed, check whether the involved locations are
  // inequal. If so, the result of XOR must be 'U'.
  if ( ( getLevel( i1.first ) == 1 ) && ( getLevel( i2.first ) == 1 ) &&
       ( i1.second.get() != i2.second.get() ) ) {
    res.first = WIR_L4::bU;
    levelRes = 0;
  }

  // If a result bit finally is either 'L' or 'N', update the result's location.
  if ( levelRes == 1 )
    res.second =
      ( getLevel( i1.first ) == 1 ) ? i1.second : i2.second;

  return( res );
};


/*
  bitCom is an internal static helper routine computing the bitwise com function
  under full consideration of eventually provided bit locations.

  The com operator is defined in Jens Wagner, Retargierbare Ausnutzung von
  Spezialoperationen für Eingebettete Systeme mit Hilfe bitgenauer
  Wertflussanalyse, page 177, Figure 4.19.
*/
WIR_UpDownValue::WIR_BitInfo WIR_UpDownValue::bitCom( WIR_BitInfo i1,
                                                      WIR_BitInfo i2 )
{
  DSTART(
    "static WIR_UpDownValue::WIR_BitInfo WIR_UpDownValue::bitCom(WIR_UpDownValue::WIR_BitInfo, WIR_UpDownValue::WIR_BitInfo)");

  WIR_BitInfo res = make_pair( com( i1.first, i2.first ), boost::none );
  auto levelRes = getLevel( res.first );

  // If [L|N] com [L|N] is performed, check whether the involved locations are
  // inequal. If so, the result of com must be 'U'.
  if ( ( getLevel( i1.first ) == 1 ) && ( getLevel( i2.first ) == 1 ) &&
       ( i1.second.get() != i2.second.get() ) ) {
    res.first = WIR_L4::bU;
    levelRes = 0;
  }

  // If a result bit finally is either 'L' or 'N', update the result's location.
  if ( levelRes == 1 )
    res.second =
      ( getLevel( i1.first ) == 1 ) ? i1.second : i2.second;

  return( res );
};

}       // namespace WIR
