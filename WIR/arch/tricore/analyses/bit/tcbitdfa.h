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
  @file tcbitdfa.h
  @brief This file provides the basic interface of a TriCore-specific bit-true
         data flow analysis.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _TC_BITDFA_H
#define _TC_BITDFA_H


//
// Include section
//

// Include standard headers
#include <map>

// Include WIR headers
#include <wir/wirtypes.h>
#include <analyses/bit/wirbitdfa.h>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_Function;


/*!
  @brief Class TC_BitDFA performs bit-true data flow analysis for the TriCore
         architecture based on the %WIR data flow graph.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class TC_BitDFA : public WIR_BitDFA
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor for function-level analysis.

      @param[in] f A reference to a WIR_Function to be analyzed.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit TC_BitDFA( WIR_Function & );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~TC_BitDFA( void );


  protected:

    /*!
      @brief simulateTopDown performs the TriCore-specific top-down simulation
             of the given %WIR operation.

      @param[in] o A const reference to the operation to be simulated top-down.
      @param[in] operands A reference to a map containing the down values of o's
                          operands, depending on the IDs of their associated
                          %WIR parameters.
      @param[in,out] results A reference to a map containing the down values of
                             o's results, depending on the IDs of their
                             associated %WIR parameters.

      For a documentation of the semantics of TriCore operations, please refer
      to the TriCore User's Manual Volume 2 (Instruction Set).

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void simulateTopDown( const WIR_Operation &,
                                  std::map<WIR_id_t, WIR_UpDownValue> &,
                                  std::map<WIR_id_t, WIR_UpDownValue> & );

    /*!
      @brief simulateBottomUp performs the TriCore-specific bottom-up simulation
             of the given %WIR operation.

      @param[in] o A const reference to the operation to be simulated bottom-up.
      @param[in] in A reference to a map containing the up values of o's
                    incoming edges, depending on the IDs of their associated
                    %WIR parameters.
      @param[in] out A reference to a map containing the up values of o's
                     outgoing edges, depending on the IDs of their associated
                     %WIR parameters.
      @param[in,out] results A reference to a map containing the up values of
                             o's results, depending on the IDs of their
                             associated %WIR parameters.

      For a documentation of the semantics of TriCore operations, please refer
      to the TriCore User's Manual Volume 2 (Instruction Set).

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void simulateBottomUp( const WIR_Operation &,
                                   std::map<WIR_id_t, WIR_UpDownValue> &,
                                   std::map<WIR_id_t, WIR_UpDownValue> &,
                                   std::map<WIR_id_t, WIR_UpDownValue> & );

    /*!
      @brief ssov realizes saturation of a given up/down value on signed
             overflow.

      @param[in] v A const reference to an up/down value interpreted as signed
                   value to be saturated.
      @param[in] w An unsigned integer denoting the bit width (and thus the
                   feasible min/max values applied for saturation) of the
                   saturated result.
      @return A saturated up/down value.

      According to the TriCore Architecture User's Manual Volume 2 (Instruction
      Set), page 2-11, ssov is defined as follows:

      ssov(x, y) = max_pos = (1 << (y - 1)) - 1;
                   max_neg = -(1 << (y - 1));
                   return(
                     (x > max_pos) ? max_pos :
                       ((x < max_neg) ? max_neg : x) ;

      In order to properly detect whether the original up/down value v exceeds
      the given bit width's min/max bounds, v must be signed and one bit wider
      than the given bit width. I.e., if a signed saturation for 32 bits is
      desired, then v must be 33 (!) bits wide. ssov asserts if this is not the
      case.

      However, the result returned by ssov is ensured to have a width of w bits.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_UpDownValue ssov( const WIR_UpDownValue &, unsigned int ) const;

    /*!
      @brief suov realizes saturation of a given up/down value on unsigned
             overflow.

      @param[in] v A const reference to an up/down value to be saturated.
      @param[in] w An unsigned integer denoting the bit width (and thus the
                   feasible min/max values applied for saturation) of the
                   saturated result.
      @return A saturated unsigned up/down value.

      According to the TriCore Architecture User's Manual Volume 2 (Instruction
      Set), page 2-11, suov is defined as follows:

      suov(x, y) = max_pos = (1 << y) - 1;
                   return( (x > max_pos) ? max_pos : ((x < 0) ? 0 : x) );

      In order to properly detect whether the original up/down value v exceeds
      the given bit width's min/max bounds, v must be one bit wider than the
      given bit width. I.e., if an unsigned saturation for 32 bits is desired,
      then v must be 33 (!) bits wide. suov asserts if this is not the case.

      However, the result returned by suov is ensured to have a width of w bits.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_UpDownValue suov( const WIR_UpDownValue &, unsigned int ) const;

    /*!
      @brief leading_ones returns the number of leading ones of a given up/down
             value.

      @param[in] v A const reference to an up/down value whose leading one bits
                   shall be counted.
      @return An unsigned up/down value of bit width 5 containing the counted
              number of leading one bits.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_UpDownValue leading_ones( const WIR_UpDownValue & ) const;

    /*!
      @brief leading_signs returns the number of leading sign bits of a
             given up/down value.

      @param[in] v A const reference to an up/down value whose leading sign bits
                   shall be counted.
      @return An unsigned up/down value of bit width 5 containing the counted
              number of leading sign bits.

      The value returned by leading_signs is the number of leading sign bits
      minus one, giving the number of redundant sign bits in v.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_UpDownValue leading_signs( const WIR_UpDownValue & ) const;

    /*!
      @brief leading_zeros returns the number of leading zeros of a given
             up/down value.

      @param[in] v A const reference to an up/down value whose leading zero bits
                   shall be counted.
      @return An unsigned up/down value of bit width 5 containing the counted
              number of leading zero bits.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_UpDownValue leading_zeros( const WIR_UpDownValue & ) const;

    /*!
      @brief reverse returns the bit-reverse of a given up/down value.

      @param[in] v A const reference to an up/down value whose bits shall be
                   reversed.
      @return An up/down value of containing the reversed bits of v.

      Bit width and signedness of the result are the same as of the argument
      value v.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_UpDownValue reverse( const WIR_UpDownValue & ) const;

    /*!
      @brief arithOp_ADD identifies common don't care bits for arithmetical
             addition operations.

      @param[in] cOut A const reference to the up value associated with the
                      outgoing edge for operand D[c].
      @param[in,out] a A reference to the up value associated with the incoming
                       edge for operand D[a].
      @param[in,out] b A reference to the up value associated with the incoming
                       edge for operand D[b].

      During bottom-up analysis, arithOp_ADD performs the following steps:
      -# Iterate all bits of cOut from the most-significant down to the least-
         significant bit.
      -# If the current bit of cOut is X, set the same bits of a and b to X.
      -# Otherwise exit.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void arithOp_ADD( const WIR_UpDownValue &, WIR_UpDownValue &,
                      WIR_UpDownValue & ) const;

    /*!
      @brief arithOp_SH identifies common don't care bits for shift operations.

      @param[in] cOut A const reference to the up value associated with the
                      outgoing edge for operand D[c].
      @param[in,out] a A reference to the up value associated with the incoming
                       edge for operand D[a].
      @param[in,out] shamt A reference to the up value associated with the 6-bit
                           signed shift amount.
      @param[in] keepShiftedOutBits A Boolean defaulting to false that specifies
                                    whether the shifted out bits must be kept or
                                    could be set to X.

      During bottom-up analysis, arithOp_SH performs the following steps:
      -# Nothing is done if the shift amount contains U, L or N bits.
      -# X bits in the shift amount are interpreted either as 1 for positive
         shift amount values or as 0 for negative shift amount values.
      -# If the shift amount is greater-equal 0 and keepShiftedOutBits is false,
         the most-significant shamt bits of a are set to X.
      -# Otherwise, the least-significant shamt bits of a are set to X if
         keepShiftedOutBits is false.
      -# If a bit of cOut is X, the corresponding bit of a at reverse-shifted
         position is set to X.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void arithOp_SH( const WIR_UpDownValue &, WIR_UpDownValue &,
                     WIR_UpDownValue &, bool = false ) const;

    /*!
      @brief bitOp_DDC5DC5_1 identifies common don't care bits for bitwise
             logical TriCore operations AND.T, ANDN.T, NAND.T, NOR.T, OR.T,
             ORN.T, XNOR.T and XOR.T having format DDC5DC5_1.

      @param[in] cOut A const reference to the up value associated with the
                      outgoing edge for operand D[c].
      @param[in,out] a A reference to the up value associated with the incoming
                       edge for operand D[a].
      @param[in] pos1 An unsigned integer denoting the position of the relevant
                      bit of D[a].
      @param[in,out] b A reference to the up value associated with the incoming
                       edge for operand D[b].
      @param[in] pos2 An unsigned integer denoting the position of the relevant
                      bit of D[b].

      During bottom-up analysis, bitOp_DDC5DC5_1 performs the following steps:
      -# All bits of a are set to X, except bit a[ pos1 ].
      -# All bits of b are set to X, except bit b[ pos2 ].
      -# a[ pos1 ] and b[ pos2 ] are set to X if cOut[ 0 ] is X.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void bitOp_DDC5DC5_1( const WIR_UpDownValue &, WIR_UpDownValue &,
                          unsigned int, WIR_UpDownValue &, unsigned int ) const;

    /*!
      @brief bitOp_DDC5DC5_2 identifies common don't care bits for bitwise
             logical TriCore operations of format DDC5DC5_2.

      @param[in] cOut A const reference to the up value associated with the
                      outgoing edge for operand D[c].
      @param[in,out] cIn A reference to the up value associated with the
                         incoming edge for operand D[c].
      @param[in,out] a A reference to the up value associated with the incoming
                       edge for operand D[a].
      @param[in] pos1 An unsigned integer denoting the position of the relevant
                      bit of D[a].
      @param[in,out] b A reference to the up value associated with the incoming
                       edge for operand D[b].
      @param[in] pos2 An unsigned integer denoting the position of the relevant
                      bit of D[b].

      During bottom-up analysis, bitOp_DDC5DC5_2 performs the following steps:
      -# All bits of a are set to X, except bit a[ pos1 ].
      -# All bits of b are set to X, except bit b[ pos2 ].
      -# All those bits of cIn are set to X that are already X in cOut.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void bitOp_DDC5DC5_2( const WIR_UpDownValue &, WIR_UpDownValue &,
                          WIR_UpDownValue &, unsigned int, WIR_UpDownValue &,
                          unsigned int ) const;

    /*!
      @brief bitOp_AND3 performs bottom-up simulation of the ternary AND
             operator found in some bitwise logical TriCore operations of format
             DDC5DC5_2.

      @param[in] cOut A const reference to the up value associated with the
                      outgoing edge for operand D[c].
      @param[in,out] cIn A reference to the up value associated with the
                         incoming edge for operand D[c].
      @param[in,out] a A reference to the up value associated with the incoming
                       edge for operand D[a].
      @param[in] pos1 An unsigned integer denoting the position of the relevant
                      bit of D[a].
      @param[in,out] b A reference to the up value associated with the incoming
                       edge for operand D[b].
      @param[in] pos2 An unsigned integer denoting the position of the relevant
                      bit of D[b].

      During bottom-up analysis, bitOp_AND3 performs the following steps:
      -# If b[ pos2 ] is 0, set cIn[ 0 ] and a[ pos1 ] to X.
      -# Otherwise, if a[ pos1 ] is 0, set cIn[ 0 ] and b[ pos2 ] to X.
      -# Otherwise, if cIn[ 0 ] is 0, set a[ pos1 ] and b[ pos2 ] to X.
      -# If cOut[ 0 ] is X, set cIn[ 0 ], a[ pos1 ] and b[ pos2 ] to X.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void bitOp_AND3( const WIR_UpDownValue &, WIR_UpDownValue &,
                     WIR_UpDownValue &, unsigned int, WIR_UpDownValue &,
                     unsigned int ) const;

    /*!
      @brief bitOp_ANDN3 performs bottom-up simulation of the ternary ANDN
             operator found in some bitwise logical TriCore operations of format
             DDC5DC5_2.

      @param[in] cOut A const reference to the up value associated with the
                      outgoing edge for operand D[c].
      @param[in,out] cIn A reference to the up value associated with the
                         incoming edge for operand D[c].
      @param[in,out] a A reference to the up value associated with the incoming
                       edge for operand D[a].
      @param[in] pos1 An unsigned integer denoting the position of the relevant
                      bit of D[a].
      @param[in,out] b A reference to the up value associated with the incoming
                       edge for operand D[b].
      @param[in] pos2 An unsigned integer denoting the position of the relevant
                      bit of D[b].

      During bottom-up analysis, bitOp_AND3 performs the following steps:
      -# If b[ pos2 ] is 1, set cIn[ 0 ] and a[ pos1 ] to X.
      -# Otherwise, if a[ pos1 ] is 0, set cIn[ 0 ] and b[ pos2 ] to X.
      -# Otherwise, if cIn[ 0 ] is 0, set a[ pos1 ] and b[ pos2 ] to X.
      -# If cOut[ 0 ] is X, set cIn[ 0 ], a[ pos1 ] and b[ pos2 ] to X.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void bitOp_ANDN3( const WIR_UpDownValue &, WIR_UpDownValue &,
                      WIR_UpDownValue &, unsigned int, WIR_UpDownValue &,
                      unsigned int ) const;

    /*!
      @brief bitOp_DDC9_3 identifies common don't care bits for bitwise logical
             TriCore operations of format DDC9_3.

      @param[in] cOut A const reference to the up value associated with the
                      outgoing edge for operand D[c].
      @param[in,out] cIn A reference to the up value associated with the
                         incoming edge for operand D[c].

      During bottom-up analysis, bitOp_DDC9_3 performs the following steps:
      -# All those bits of cIn are set to X that are already X in cOut.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void bitOp_DDC9_3( const WIR_UpDownValue &, WIR_UpDownValue & ) const;

    /*!
      @brief bitOp_AND_CMP performs bottom-up simulation of the AND.{EQ|GE|...}
             family of logical TriCore operations.

      @param[in] cOut A const reference to the up value associated with the
                      outgoing edge for operand D[c].
      @param[in,out] cIn A reference to the up value associated with the
                         incoming edge for operand D[c].
      @param[in,out] a A reference to the up value associated with the incoming
                       edge for operand D[a].
      @param[in,out] b A reference to the up value associated with the incoming
                       edge for operand D[b].
      @param[in] cmp An L4 bit value denoting the result of the EQ/GE/...
                     comparison.

      During bottom-up analysis, bitOp_AND_CMP performs the following steps:
      -# If cIn[ 0 ] is 0, set a and b to X.
      -# Otherwise, if cmp is 0, set cIn[ 0 ] to X.
      -# If cOut[ 0 ] is X, set cIn[ 0 ], a and b to X.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void bitOp_AND_CMP( const WIR_UpDownValue &, WIR_UpDownValue &,
                        WIR_UpDownValue &, WIR_UpDownValue &, WIR_L4 ) const;

    /*!
      @brief bitOp_OR_CMP performs bottom-up simulation of the OR.{EQ|GE|...}
             family of logical TriCore operations.

      @param[in] cOut A const reference to the up value associated with the
                      outgoing edge for operand D[c].
      @param[in,out] cIn A reference to the up value associated with the
                         incoming edge for operand D[c].
      @param[in,out] a A reference to the up value associated with the incoming
                       edge for operand D[a].
      @param[in,out] b A reference to the up value associated with the incoming
                       edge for operand D[b].
      @param[in] cmp An L4 bit value denoting the result of the EQ/GE/...
                     comparison.

      During bottom-up analysis, bitOp_OR_CMP performs the following steps:
      -# If cIn[ 0 ] is 1, set a and b to X.
      -# Otherwise, if cmp is 1, set cIn[ 0 ] to X.
      -# If cOut[ 0 ] is X, set cIn[ 0 ], a and b to X.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void bitOp_OR_CMP( const WIR_UpDownValue &, WIR_UpDownValue &,
                       WIR_UpDownValue &, WIR_UpDownValue &, WIR_L4 ) const;

    /*!
      @brief shiftOp_DDC5DC5_2 identifies common don't care bits for TriCore
             shift operations of format DDC5DC5_2.

      @param[in,out] a A reference to the up value associated with the incoming
                       edge for operand D[a].
      @param[in] pos1 An unsigned integer denoting the position of the relevant
                      bit of D[a].
      @param[in,out] b A reference to the up value associated with the incoming
                       edge for operand D[b].
      @param[in] pos2 An unsigned integer denoting the position of the relevant
                      bit of D[b].

      During bottom-up analysis, shiftOp_DDC5DC5_2 performs the following steps:
      -# All bits of a are set to X, except bit a[ pos1 ].
      -# All bits of b are set to X, except bit b[ pos2 ].

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void shiftOp_DDC5DC5_2( WIR_UpDownValue &, unsigned int, WIR_UpDownValue &,
                            unsigned int ) const;

};

}       // namespace WIR

#endif  // _TC_BITDFA_H
