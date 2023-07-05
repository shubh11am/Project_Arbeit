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
  @file wirl4.h
  @brief This file provides the interface of the L4 half-order that is used as
         representation of bit values for the bit-true data and value flow
         analysis.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_L4_H
#define _WIR_L4_H


//
// Include section
//

// Include standard headers
#include <functional>
#include <ostream>
#include <set>
#include <utility>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_RegisterParameter;


/*!
  @brief This enum represents the L4 half-order according to Jens Wagner,
         Retargierbare Ausnutzung von Spezialoperationen für Eingebettete
         Systeme mit Hilfe bitgenauer Wertflussanalyse, page 124, figure 3.11.

  @verbatim
           X                    Level 3
          / \
         0   1                  Level 2
         |\ /|
         | X |
         |/ \|
         L   N                  Level 1
          \ /
           U                    Level 0
  @endverbatim

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
enum class WIR_L4 : char
{
  //! Arbitrary bit value, don't care.
  bX = 0,

  //! Exactly known bit value of '0'.
  b0 = 1,

  //! Exactly known bit value of '1'.
  b1 = 2,

  //! Unknown bit value but known location of the bit's origin.
  bL = 3,

  //! Unknown bit value but negated known location of the bit's origin.
  bN = 4,

  //! Unknown bit value and unknown origin.
  bU = 5
};


/*!
  @brief This operator performs a less-than comparison of two bit values from
         L4.

  @param[in] lhs The left-hand operand.
  @param[in] rhs The right-hand operand.
  @return true iff the left operand is less than the right one according to the
          L4 half-order, false otherwise.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
bool operator < ( WIR_L4 lhs, WIR_L4 rhs );


/*!
  @brief This operator performs a greater-than comparison of two bit values from
         L4.

  @param[in] lhs The left-hand operand.
  @param[in] rhs The right-hand operand.
  @return true iff the left operand is greater than the right one according to
          the L4 half-order, false otherwise.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
bool operator > ( WIR_L4 lhs, WIR_L4 rhs );


/*!
  @brief WIR_Compare_L4 is a comparator class that is used to sort, e.g., sets
         of WIR_L4 bits uniquely.

  The above < operator does not fulfill this purpose as it is ambiguous due to
  the levels of the half-order.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
struct WIR_Compare_L4
{
  bool operator()( const WIR_L4 lhs, const WIR_L4 rhs ) const;
};


/*!
  @brief WIR_L4Set represents sets of L4 bits.
*/
using WIR_L4Set = std::set<WIR_L4, WIR_Compare_L4>;


/*!
  @brief This operator performs a bitwise AND of two bit values from L4.

  @param[in] lhs The left-hand operand.
  @param[in] rhs The right-hand operand.
  @return The bit value resulting from lhs AND rhs.

  @note If lhs and rhs both denote an L or N value, then this operator assumes
        that both lhs and rhs refer to the very same location. If both operands
        refer to different locations, this operator should return a U value
        which is not the case and thus must be handeled elsewhere!

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
WIR_L4 operator & ( WIR_L4 lhs, WIR_L4 rhs );


/*!
  @brief This operator performs a bitwise OR of two bit values from L4.

  @param[in] lhs The left-hand operand.
  @param[in] rhs The right-hand operand.
  @return The bit value resulting from lhs OR rhs.

  @note If lhs and rhs both denote an L or N value, then this operator assumes
        that both lhs and rhs refer to the very same location. If both operands
        refer to different locations, this operator should return a U value
        which is not the case and thus must be handeled elsewhere!

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
WIR_L4 operator | ( WIR_L4 lhs, WIR_L4 rhs );


/*!
  @brief This operator performs a bitwise XOR of two bit values from L4.

  @param[in] lhs The left-hand operand.
  @param[in] rhs The right-hand operand.
  @return The bit value resulting from lhs XOR rhs.

  @note If lhs and rhs both denote an L or N value, then this operator assumes
        that both lhs and rhs refer to the very same location. If both operands
        refer to different locations, this operator should return a U value
        which is not the case and thus must be handeled elsewhere!

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
WIR_L4 operator ^ ( WIR_L4 lhs, WIR_L4 rhs );


/*!
  @brief This operator performs a bitwise NOT of a bit value from L4.

  @param[in] b The operand.
  @return The bit value resulting from NOT b.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
WIR_L4 operator ~ ( WIR_L4 b );

/*!
  @brief getLevel returns the level of an L4 value in the above Hasse diagram.

  @param[in] b The operand.
  @return The level of b within the L4 half-order.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
unsigned int getLevel( WIR_L4 b );

/*!
  @brief com returns the "smallest common" combination of two bit values from
         L4.

  @param[in] lhs The left-hand operand.
  @param[in] rhs The right-hand operand.
  @return The bit value resulting from lhs com rhs.

  @note If lhs and rhs both denote an L or N value, then this function assumes
        that both lhs and rhs refer to the very same location. If both operands
        refer to different locations, com should return a U value which is not
        the case and thus must be handeled elsewhere!

  The com operator is defined in Jens Wagner, Retargierbare Ausnutzung von
  Spezialoperationen für Eingebettete Systeme mit Hilfe bitgenauer
  Wertflussanalyse, page 177, Figure 4.19.
*/
WIR_L4 com( WIR_L4 lhs, WIR_L4 rhs );


//
// Stream I/O.
//

/*!
  @brief The << operator dumps an L4 value to an output stream.

  @param[in] os A reference to an output stream.
  @param[in] v A const reference to the L4 value to be dumped.
  @return A reference to the same output stream.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
std::ostream & operator << ( std::ostream &os, const WIR_L4 &v );

}       // namespace WIR

#endif  // _WIR_L4_H
