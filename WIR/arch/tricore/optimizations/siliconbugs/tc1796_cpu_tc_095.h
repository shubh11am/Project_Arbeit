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
  @file tc1796_cpu_tc_095.h
  @brief This file provides the interface of a peephole optimizer for silicon
         bug TC1796 CPU_TC.095 detection and correction.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_SILICONBUGS_TC1796_CPU_TC_095_H
#define _WIR_SILICONBUGS_TC1796_CPU_TC_095_H


//
// Include section
//

// Include WIR headers
#include <optimizations/siliconbugs/wirsiliconbugs.h>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_Function;


/*!
  @brief Class TC1796_CPU_TC_094 is a peephole optimizer for Infineon TriCore
         TC1796 silicon bug CPU_TC.095.

  See TC1796, EES-BC, ES-BS, BC Errata Sheet Rel. 2.0, 23.06.2008.

  <B>CPU_TC.095: Incorrect Forwarding in <TT>SAT</TT>, Mixed Register
                 Instruction Sequence</B>

  In a small number of very specific instruction sequences, involving Load-Store
  (LS) pipeline instructions with data general purpose register (DGPR) operands,
  the operand forwarding in the TriCore1 CPU may fail and the data dependency
  between two instructions be missed, leading to incorrect operation. The
  problem may occur in one of two instruction sequences as follows:

  Problem Sequence 1)
  -# LS instruction with DGPR destinagion {<TT>MOV.D</TT>, <TT>EQ.A</TT>,
     <TT>NE.A</TT>, <TT>LT.A</TT>, <TT>GE.A</TT>, <TT>EQZ.A</TT>,
     <TT>NEZ.A</TT>, <TT>MFCR</TT>}
  -# <TT>SAT.H</TT> instruction
  -# LS instruction with DGPR source {<TT>ADDSC.A</TT>, <TT>ADDSC.AT</TT>,
     <TT>MOV.A</TT>, <TT>MTCR</TT>}

  If the DGPR source register of (3) is equal to the DGPR destination register
  of (1), then the interaction with the <TT>SAT.H</TT> instruction may cause the
  dependency to be missed and the original DGPR value to be passed to (3).

  Problem Sequence 2)
  -# Load instruction with 64-bit DGPR destination {<TT>LD.D</TT>,
     <TT>LDLCX</TT>, <TT>LDUCX</TT>, <TT>RSLCX</TT>, <TT>RFE</TT>, <TT>RFM</TT>,
     <TT>RET</TT>}
  -# <TT>SAT.B</TT> or <TT>SAT.H</TT> instruction
  -# LS instruction with DGPR source {<TT>ADDSC.A</TT>, <TT>ADDSC.AT</TT>,
     <TT>MOV.A</TT>, <TT>MTCR</TT>}

  In this case if the DGPR source register of (3) is equal to the high 32-bit
  DGPR destination register of (1), then the interaction with the
  <TT>SAT.B</TT>/<TT>SAT.H</TT> instruction may cause the dependency to be
  missed and the original DGPR value to be passed to (3).

  <B>Example</B>

  <PRE>
  ...
  MOV.D   D2, A12
  SAT.H   D7
  MOV.A   A4, D2
  ...
  </PRE>

  Note that for the second problem sequence the first instruction of the
  sequence could be <TT>RFE</TT> and as such occur asynchronous with respect to
  the program flow.

  <B>Workaround</B>

  A single <TT>NOP</TT> instruction must be inserted between any
  <TT>SAT.B</TT>/<TT>SAT.H</TT> instruction and a following Load-Store
  instruction with a DGPR source operand {<TT>ADDSC.A</TT>, <TT>ADDSC.AT</TT>,
  <TT>MOV.A</TT>, <TT>MTCR</TT>}.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class TC1796_CPU_TC_095 final : public WIR_SiliconBugs
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor.

      @param[in,out] f A reference to a WIR_Function to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit TC1796_CPU_TC_095( WIR_Function & );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~TC1796_CPU_TC_095( void );


  protected:

    /*!
      @brief matchSiliconBug determines whether the specified peephole matches
             with silicon bug CPU_TC.095.

      @param[in] p A const reference to a peephole.
      @return true if p matches with silicon bug CPU_TC.095, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool matchSiliconBug( const WIR_Peephole::peephole & );

    /*!
      @brief fixSiliconBug fixes silicon bug CPU_TC.095.

      @param[in] p A const reference to a peephole.
      @return The peephole after bug fixing.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_Peephole::peephole fixSiliconBug( const WIR_Peephole::peephole & ) const;


  private:

    /*!
      @brief isSequence1Start determines whether an operation is the first one
             from Problem Sequence 1) subject to silicon bug CPU_TC.095.

      @param[in] o A const reference to an operation to be examined.
      @return true iff o is the first operation of Problem Sequence 1) subject
              to CPU_TC.095, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isSequence1Start( const WIR_Operation & ) const;

    /*!
      @brief isSequence2Start determines whether an operation is the first one
             from Problem Sequence 2) subject to silicon bug CPU_TC.095.

      @param[in] o A const reference to an operation to be examined.
      @return true iff o is the first operation of Problem Sequence 2) subject
              to CPU_TC.095, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isSequence2Start( const WIR_Operation & ) const;

    /*!
      @brief isSequenceEnd determines whether an operation is the last one
             from Problem Sequences subject to silicon bug CPU_TC.095.

      @param[in] o A const reference to an operation to be examined.
      @return true iff o is the last operation of a Problem Sequence subject
              to CPU_TC.095, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isSequenceEnd( const WIR_Operation & ) const;

    /*!
      @brief isSATH determines whether an operation is a SAT.H subject to
             silicon bug CPU_TC.095.

      @param[in] o A const reference to an operation to be examined.
      @return true iff o is a SAT.H operation subject to CPU_TC.095, false
              otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isSATH( const WIR_Operation & ) const;

    /*!
      @brief isSAT determines whether an operation is a SAT.B or SAT.H subject
             to silicon bug CPU_TC.095.

      @param[in] o A const reference to an operation to be examined.
      @return true iff o is a SAT.B/SAT.H operation subject to CPU_TC.095, false
              otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isSAT( const WIR_Operation & ) const;

};

}       // namespace WIR

#endif  // _WIR_SILICONBUGS_TC1796_CPU_TC_095_H
