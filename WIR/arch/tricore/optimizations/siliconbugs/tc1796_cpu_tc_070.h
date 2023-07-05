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
  @file tc1796_cpu_tc_070.h
  @brief This file provides the interface of a peephole optimizer for silicon
         bug TC1796 CPU_TC.070 detection and correction.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_SILICONBUGS_TC1796_CPU_TC_070_H
#define _WIR_SILICONBUGS_TC1796_CPU_TC_070_H


//
// Include section
//

// Include standard headers
#include <set>

// Include WIR headers
#include <wir/wirtypes.h>
#include <optimizations/siliconbugs/wirsiliconbugs.h>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_Function;


//
// Header section
//

/*!
  @brief Class TC1796_CPU_TC_070 is a peephole optimizer for Infineon TriCore
         TC1796 silicon bug CPU_TC.070.

  See TC1796, EES-BC, ES-BS, BC Errata Sheet Rel. 2.0, 23.06.2008.

  <B>CPU_TC.070: Error when conditional jump precedes loop instruction</B>

  An error in the program flow may occur when a conditional jump instruction is
  directly followed by a loop instruction (either conditional or unconditional).
  Both integer pipeline and load-store pipeline conditional jumps (i.e. those
  checking the values of data and address registers respectively) may cause the
  erroneous behavior.

  The incorrect behavior occurs when the two instructions are not dual-issued,
  such that the conditional jump is in the execute stage of the pipeline and the
  loop instruction is at the decode stage. In this case, both the conditional
  jump instruction and the loop instruction will be resolved in the same cycle.
  The problem occurs because priority is given to the loop mis-prediction logic,
  despite the conditional jump instruction being semantically before the loop
  instruction in the program flow. In this error case the program flow continues
  as if the loop has exited: the PC is taken from the loop mis-prediction
  branch. In order for the erroneous behavior to occur, the conditional jump
  must be incorrectly predicted as not taken. Since all conditional jump
  instructions, with the exception of 32-bit format forward jumps, are predicted
  as taken, only 32-bit forward jumps can cause the problem behavior.

  <B>Example</B>

  <PRE>
  ...
  JNE.A   A1, A0, jump_target_1_              ; 32-bit forward jump
  LOOP    A6, loop_target_1_
  ...
  jump_target_1_:
  ...
  </PRE>

  <B>Workaround</B>

  A conditional jump instruction may not be directly followed by a loop
  instruction (conditional or not). A <TT>NOP</TT> must be inserted between any
  load-store pipeline conditional jump (where the condition is dependent on an
  address register) and a loop instruction. Two <TT>NOP</TT>s must be inserted
  between any integer pipeline conditional jump (where the condition is
  dependent on a data register) and a loop instruction.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class TC1796_CPU_TC_070 final : public WIR_SiliconBugs
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
    explicit TC1796_CPU_TC_070( WIR_Function & );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~TC1796_CPU_TC_070( void );


  protected:

    /*!
      @brief matchSiliconBug determines whether the specified peephole matches
             with silicon bug CPU_TC.070.

      @param[in] p A const reference to a peephole.
      @return true if p matches with silicon bug CPU_TC.070, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool matchSiliconBug( const WIR_Peephole::peephole & );

    /*!
      @brief fixSiliconBug fixes silicon bug CPU_TC.070.

      @param[in] p A const reference to a peephole.
      @return The peephole after bug fixing.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_Peephole::peephole fixSiliconBug( const WIR_Peephole::peephole & ) const;


  private:

    /*!
      @brief isCJump determines whether an operation is a conditional jump
             subject to silicon bug CPU_TC.070.

      @param[in] o A const reference to an operation to be examined.
      @return true iff o is a conditional jump subject to CPU_TC.070, false
              otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isCJump( const WIR_Operation & ) const;

    /*!
      @brief isDJump determines whether an operation is a conditional jump
             depending on a data register, subject to silicon bug CPU_TC.070.

      @param[in] o A const reference to an operation to be examined.
      @return true iff o is a conditional jump depending on a data register,
              subject to CPU_TC.070, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isDJump( const WIR_Operation & ) const;

    /*!
      @brief isLOOP determines whether an operation is a LOOP/LOOPU operation
             subject to silicon bug CPU_TC.070.

      @param[in] o A const reference to an operation to be examined.
      @return true iff o is a LOOP/LOOPU operation subject to CPU_TC.070, false
              otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isLOOP( const WIR_Operation & ) const;

    /*!
      @brief mMatchedPeepholes stores IDs of the first operations of all matched
             peepholes in order to avoid fixing the same code sequence several
             times for peepholes of different sizes.
    */
    std::set<WIR_id_t> mMatchedPeepholes;

};

}       // namespace WIR

#endif  // _WIR_SILICONBUGS_TC1796_CPU_TC_070_H
