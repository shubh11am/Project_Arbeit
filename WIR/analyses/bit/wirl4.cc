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
  @file wirl4.cc
  @brief This file implements the L4 half-order that is used as representation
         of bit values for the bit-true data and value flow analysis.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include standard headers
#include <string>

// Include libuseful headers
#include <libuseful/debugmacros.h>

// Include WIR headers
#include <wir/wir.h>


//
// Code section
//

namespace WIR {


using namespace std;


/*
  This operator performs a less-than comparison of two bit values from L4.
*/
bool operator < ( WIR_L4 lhs, WIR_L4 rhs )
{
  DSTART( "bool operator<(WIR_L4, WIR_L4)" );

  return( getLevel( lhs ) < getLevel( rhs ) );
};


/*
  This operator performs a greater-than comparison of two bit values from L4.
*/
bool operator > ( WIR_L4 lhs, WIR_L4 rhs )
{
  DSTART( "bool operator>(WIR_L4, WIR_L4)" );

  return( getLevel( lhs ) > getLevel( rhs ) );
};


/*
  WIR_Compare_L4 is a comparator class that is used to sort, e.g., sets of
  WIR_L4 bits uniquely.

  The above < operator does not fulfill this purpose as it is ambiguous due to
  the levels of the half-order.
*/
bool WIR_Compare_L4::operator()( const WIR_L4 lhs, const WIR_L4 rhs ) const
{
  DSTART( "bool WIR_Compare_L4::operator()(WIR_L4, WIR_L4) const" );

  return( static_cast<char>( lhs ) < static_cast<char>( rhs ) );
};


/*
  This operator performs a bitwise AND of two bit values from L4.

  If lhs and rhs both denote an L or N value, then this operator assumes that
  both lhs and rhs refer to the very same location. If both operands refer to
  different locations, this operator should return a U value which is not the
  case and thus must be handeled elsewhere!
*/
WIR_L4 operator & ( WIR_L4 lhs, WIR_L4 rhs )
{
  DSTART( "WIR_L4 operator&(WIR_L4, WIR_L4)" );

  static const WIR_L4 truthTable[ 6 ][ 6 ] =
    //    rhs = X     rhs = 0     rhs = 1     rhs = L     rhs = N     rhs = U
    { { WIR_L4::bX, WIR_L4::b0, WIR_L4::bX, WIR_L4::b0, WIR_L4::b0, WIR_L4::b0 },   // lhs = X
      { WIR_L4::b0, WIR_L4::b0, WIR_L4::b0, WIR_L4::b0, WIR_L4::b0, WIR_L4::b0 },   // lhs = 0
      { WIR_L4::bX, WIR_L4::b0, WIR_L4::b1, WIR_L4::bL, WIR_L4::bN, WIR_L4::bU },   // lhs = 1
      { WIR_L4::b0, WIR_L4::b0, WIR_L4::bL, WIR_L4::bL, WIR_L4::b0, WIR_L4::bU },   // lhs = L
      { WIR_L4::b0, WIR_L4::b0, WIR_L4::bN, WIR_L4::b0, WIR_L4::bN, WIR_L4::bU },   // lhs = N
      { WIR_L4::b0, WIR_L4::b0, WIR_L4::bU, WIR_L4::bU, WIR_L4::bU, WIR_L4::bU } }; // lhs = U

  return( truthTable[ static_cast<int>( lhs ) ][ static_cast<int>( rhs ) ] );
};


/*
  This operator performs a bitwise OR of two bit values from L4.

  If lhs and rhs both denote an L or N value, then this operator assumes that
  both lhs and rhs refer to the very same location. If both operands refer to
  different locations, this operator should return a U value which is not the
  case and thus must be handeled elsewhere!
*/
WIR_L4 operator | ( WIR_L4 lhs, WIR_L4 rhs )
{
  DSTART( "WIR_L4 operator|(WIR_L4, WIR_L4)" );

  static const WIR_L4 truthTable[ 6 ][ 6 ] =
    //    rhs = X     rhs = 0     rhs = 1     rhs = L     rhs = N     rhs = U
    { { WIR_L4::bX, WIR_L4::bX, WIR_L4::b1, WIR_L4::b1, WIR_L4::b1, WIR_L4::b1 },   // lhs = X
      { WIR_L4::bX, WIR_L4::b0, WIR_L4::b1, WIR_L4::bL, WIR_L4::bN, WIR_L4::bU },   // lhs = 0
      { WIR_L4::b1, WIR_L4::b1, WIR_L4::b1, WIR_L4::b1, WIR_L4::b1, WIR_L4::b1 },   // lhs = 1
      { WIR_L4::b1, WIR_L4::bL, WIR_L4::b1, WIR_L4::bL, WIR_L4::b1, WIR_L4::bU },   // lhs = L
      { WIR_L4::b1, WIR_L4::bN, WIR_L4::b1, WIR_L4::b1, WIR_L4::bN, WIR_L4::bU },   // lhs = N
      { WIR_L4::b1, WIR_L4::bU, WIR_L4::b1, WIR_L4::bU, WIR_L4::bU, WIR_L4::bU } }; // lhs = U

  return( truthTable[ static_cast<int>( lhs ) ][ static_cast<int>( rhs ) ] );
};


/*
  This operator performs a bitwise XOR of two bit values from L4.

  If lhs and rhs both denote an L or N value, then this operator assumes that
  both lhs and rhs refer to the very same location. If both operands refer to
  different locations, this operator should return a U value which is not the
  case and thus must be handeled elsewhere!
*/
WIR_L4 operator ^ ( WIR_L4 lhs, WIR_L4 rhs )
{
  DSTART( "WIR_L4 operator^(WIR_L4, WIR_L4)" );

  static const WIR_L4 truthTable[ 6 ][ 6 ] =
    //    rhs = X     rhs = 0     rhs = 1     rhs = L     rhs = N     rhs = U
    { { WIR_L4::bX, WIR_L4::bX, WIR_L4::bX, WIR_L4::bL, WIR_L4::bN, WIR_L4::bU },   // lhs = X
      { WIR_L4::bX, WIR_L4::b0, WIR_L4::b1, WIR_L4::bL, WIR_L4::bN, WIR_L4::bU },   // lhs = 0
      { WIR_L4::bX, WIR_L4::b1, WIR_L4::b0, WIR_L4::bN, WIR_L4::bL, WIR_L4::bU },   // lhs = 1
      { WIR_L4::bL, WIR_L4::bL, WIR_L4::bN, WIR_L4::b0, WIR_L4::b1, WIR_L4::bU },   // lhs = L
      { WIR_L4::bN, WIR_L4::bN, WIR_L4::bL, WIR_L4::b1, WIR_L4::b0, WIR_L4::bU },   // lhs = N
      { WIR_L4::bU, WIR_L4::bU, WIR_L4::bU, WIR_L4::bU, WIR_L4::bU, WIR_L4::bU } }; // lhs = U

  return( truthTable[ static_cast<int>( lhs ) ][ static_cast<int>( rhs ) ] );
};


/*
  This operator performs a bitwise NOT of a bit value from L4.
*/
WIR_L4 operator ~ ( WIR_L4 b )
{
  DSTART( "WIR_L4 operator~(WIR_L4)" );

  static const WIR_L4 truthTable[ 6 ] =
    //   b = X       b = 0       b = 1       b = L       b = N       b = U
    { WIR_L4::bX, WIR_L4::b1, WIR_L4::b0, WIR_L4::bN, WIR_L4::bL, WIR_L4::bU };

  return( truthTable[ static_cast<int>( b ) ] );
};


/*
  getLevel returns the level of an L4 value in the above Hasse diagram.
*/
unsigned int getLevel( WIR_L4 b )
{
  DSTART( "unsigned int getLevel(WIR_L4)" );

  switch ( b ) {
    case WIR_L4::bX:
      return( 3 );

    case WIR_L4::b0:
    case WIR_L4::b1:
      return( 2 );

    case WIR_L4::bL:
    case WIR_L4::bN:
      return( 1 );

    default:
      return( 0 );
  }
};


/*
  com returns the "smallest common" combination of two bit values from L4.

  If lhs and rhs both denote an L or N value, then this function assumes that
  both lhs and rhs refer to the very same location. If both operands refer to
  different locations, com should return a U value which is not the case and
  thus must be handeled elsewhere!

  The com operator is defined in Jens Wagner, Retargierbare Ausnutzung von
  Spezialoperationen für Eingebettete Systeme mit Hilfe bitgenauer
  Wertflussanalyse, page 177, Figure 4.19.
*/
WIR_L4 com( WIR_L4 lhs, WIR_L4 rhs )
{
  DSTART( "WIR_L4 com(WIR_L4, WIR_L4)" );

  static const WIR_L4 truthTable[ 6 ][ 6 ] =
    //    rhs = X     rhs = 0     rhs = 1     rhs = L     rhs = N     rhs = U
    { { WIR_L4::bX, WIR_L4::b0, WIR_L4::b1, WIR_L4::bL, WIR_L4::bN, WIR_L4::bU },   // lhs = X
      { WIR_L4::b0, WIR_L4::b0, WIR_L4::bU, WIR_L4::bU, WIR_L4::bU, WIR_L4::bU },   // lhs = 0
      { WIR_L4::b1, WIR_L4::bU, WIR_L4::b1, WIR_L4::bU, WIR_L4::bU, WIR_L4::bU },   // lhs = 1
      { WIR_L4::bL, WIR_L4::bU, WIR_L4::bU, WIR_L4::bL, WIR_L4::bU, WIR_L4::bU },   // lhs = L
      { WIR_L4::bN, WIR_L4::bU, WIR_L4::bU, WIR_L4::bU, WIR_L4::bN, WIR_L4::bU },   // lhs = N
      { WIR_L4::bU, WIR_L4::bU, WIR_L4::bU, WIR_L4::bU, WIR_L4::bU, WIR_L4::bU } }; // lhs = U

  return( truthTable[ static_cast<int>( lhs ) ][ static_cast<int>( rhs ) ] );
};


/*
  The << operator dumps an L4 value to an output stream.
*/
std::ostream & operator << ( std::ostream &os, const WIR_L4 &v )
{
  DSTART( "ostream& operator<<(ostream&, const WIR_L4&)" );

  string s {
    v == WIR_L4::bU ? "U" :
      v == WIR_L4::bL ? "L" :
        v == WIR_L4::bN ? "N" :
          v == WIR_L4::b0 ? "0" :
            v == WIR_L4::b1 ? "1" : "X" };

  os << s;

  return( os );
};

}       // namespace WIR
