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
  @file tc1796_cpu_tc_094.h
  @brief This file provides the interface of a peephole optimizer for silicon
         bug TC1796 CPU_TC.094 detection and correction.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_SILICONBUGS_TC1796_CPU_TC_094_H
#define _WIR_SILICONBUGS_TC1796_CPU_TC_094_H


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
         TC1796 silicon bug CPU_TC.094.

  See TC1796, EES-BC, ES-BS, BC Errata Sheet Rel. 2.0, 23.06.2008.

  <B>CPU_TC.094: Potential Performance Loss when CSA Instruction follows IP
                 Jump</B>

  The TriCore1 CPU contains shadow registers for the upper context registers, to
  optimise the latency of certain CSA list operations. As such, the latency of
  instructions operating on the CSA list is variable dependent on the state of
  the context system. For instance, a return instruction will take fewer cycles
  when the previous upper context is held in the shadow registers than when the
  shadow registers are empty and the upper context has to be re-loaded from
  memory.

  In situations where the CSA list is located in single cycle access memory
  (i.e. Data Scratchpad RAM), instructions operating on the upper context (such
  as <TT>CALL</TT>, <TT>RET</TT>) will have a latency of between 2 and 5 cycles,
  dependent on the state of the context system. In the case where the CSA list
  instruction will take 4 or 5 cycles, the instruction will cause the
  instruction fetch request to be negated whilst the initial access of the
  context operation complete.

  A performance problem exists when certain jump instructions which are executed
  by the integer pipeline are followed immediately by certain CSA list
  instructions, such that the instructions are dual-issued. In this case, where
  the jump instruction is predicted taken, the effect of the CSA list
  instruction on the fetch request is not immediately cancelled, which can lead
  to the jump instruction taking 2 cycles longer than expected. This effect is
  especially noticeable where the jump instruction is used to implement a short
  loop, since the loop may take 2 cycles more than expected. In addition, since
  the state of the context system may be modified by asynchronous events such as
  interrupts, the execution time of the loop before and after an interrupt is
  taken may be different.

  Integer pipeline jump instructions are those that operate on data register
  values as follows:

  <TT>JEQ</TT>, <TT>JGE</TT>, <TT>JGE.U</TT>, <TT>JGEZ</TT>, <TT>JGTZ</TT>,
  <TT>JLEZ</TT>, <TT>JLT</TT>, <TT>JLT.U</TT>, <TT>JLTZ</TT>, <TT>JNE</TT>,
  <TT>JNED</TT>, <TT>JNEI</TT>, <TT>JNZ</TT>, <TT>JNZ.T</TT>, <TT>JZ</TT>,
  <TT>JZ.T</TT>

  CSA list instructions which may cause the performance loss are as follows:

  <TT>CALL</TT>, <TT>CALLA</TT>, <TT>CALLI</TT>, <TT>SYSCALL</TT>, <TT>RET</TT>,
  <TT>RFE</TT>

  <B>Workaround</B>

  In order to avoid any performance loss, in particular where the IP jump
  instruction is used to implement a loop and as such is taken multiple times, a
  <TT>NOP</TT> instruction should be inserted between the IP jump and the CSA
  list instruction.

  <B>Example</B>

  <PRE>
  ...
  JLT.U   D12, D7, jump_target_
  NOP
  RET
  ...
  </PRE>

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class TC1796_CPU_TC_094 final : public WIR_SiliconBugs
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
    explicit TC1796_CPU_TC_094( WIR_Function & );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~TC1796_CPU_TC_094( void );


  protected:

    /*!
      @brief matchSiliconBug determines whether the specified peephole matches
             with silicon bug CPU_TC.094.

      @param[in] p A const reference to a peephole.
      @return true if p matches with silicon bug CPU_TC.094, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool matchSiliconBug( const WIR_Peephole::peephole & );

    /*!
      @brief fixSiliconBug fixes silicon bug CPU_TC.094.

      @param[in] p A const reference to a peephole.
      @return The peephole after bug fixing.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_Peephole::peephole fixSiliconBug( const WIR_Peephole::peephole & ) const;


  private:

    /*!
      @brief isIPJmp determines whether an operation is a jump executed in the
             IP pipeline, which is subject to silicon bug CPU_TC.094.

      @param[in] o A const reference to an operation to be examined.
      @return true iff o is an IP jump subject to CPU_TC.094, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isIPJmp( const WIR_Operation & ) const;

    /*!
      @brief isCSA determines whether an operation is a CSA list operation
             subject to silicon bug CPU_TC.094.

      @param[in] o A const reference to an operation to be examined.
      @return true iff o is a CSA list subject to CPU_TC.094, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isCSA( const WIR_Operation & ) const;

};

}       // namespace WIR

#endif  // _WIR_SILICONBUGS_TC1796_CPU_TC_094_H
