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
  @file tc1796_cpu_tc_096.h
  @brief This file provides the interface of a peephole optimizer for silicon
         bug TC1796 CPU_TC.096 detection and correction.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_SILICONBUGS_TC1796_CPU_TC_096_H
#define _WIR_SILICONBUGS_TC1796_CPU_TC_096_H


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


/*!
  @brief Class TC1796_CPU_TC_096 is a peephole optimizer for Infineon TriCore
         TC1796 silicon bug CPU_TC.096.

  See TC1796, EES-BC, ES-BS, BC Errata Sheet Rel. 2.0, 23.06.2008.

  <B>CPU_TC.096: Error when Conditional Loop targets Single Issue Group Loop</B>

  An error in the program flow may occur when a conditional loop instruction
  (<TT>LOOP</TT>) has as its target an instruction which forms part of a single
  issue group loop. Single issue group loops consist of an optional Integer
  Pipeline (IP) instruction, optional Load-Store Pipeline (LS) instruction and a
  loop instruction targeting the first instruction of the group. In order for
  the problem to occur the outer loop must first be cancelled (for instance due
  to a pipeline hazard) before being executed normally. When the problem occurs
  the loop counter of the outer loop instruction is not decremented correctly
  and the loop executed an incorrect number of times.

  <B>Example</B>

  <PRE>
  ...
  loop_target_:
  ADD     D2, D1                              ; Optional IP instruction
  ADD.A   A2, A1                              ; Optional LS instruction
  LOOP    Ax, loop_target_                    ; Single Issue Group Loop
  ...
  LD.A    Am, &lt;addressing mode&lt;
  LD.W    Dx, [Am]                            ; Address dependency causes cancel
  LOOP    Ay, loop_target_                    ; Conditional loop targets single issue group loop
  ...
  </PRE>

  <B>Workaround</B>

  Single issue group loops should not be used. Where a single issue group loop
  consists of an IP instruction and a loop instruction targeting the IP
  instruction, two <TT>NOP</TT>s must be inserted between the IP and loop
  instructions. Where a single issue group loop consists of an optional IP
  instruction, a single LS instruction and a loop instruction targeting the
  first instruction of this group, a single <TT>NOP</TT> must be inserted
  between the LS instruction and the loop instruction. Since single issue group
  loops do not operate optimally on the current TriCore1 implementation (not
  zero overhead), no loss of performance is incurred.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class TC1796_CPU_TC_096 final : public WIR_SiliconBugs
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
    explicit TC1796_CPU_TC_096( WIR_Function & );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~TC1796_CPU_TC_096( void );


  protected:

    /*!
      @brief matchSiliconBug determines whether the specified peephole matches
             with silicon bug CPU_TC.096.

      @param[in] p A const reference to a peephole.
      @return true if p matches with silicon bug CPU_TC.096, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool matchSiliconBug( const WIR_Peephole::peephole & );

    /*!
      @brief fixSiliconBug fixes silicon bug CPU_TC.096.

      @param[in] p A const reference to a peephole.
      @return The peephole after bug fixing.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_Peephole::peephole fixSiliconBug( const WIR_Peephole::peephole & ) const;

    /*!
      @brief mMatchedPeepholes stores IDs of the first operations of all matched
             peepholes in order to avoid fixing the same code sequence several
             times for peepholes of different sizes.
    */
    std::set<WIR_id_t> mMatchedPeepholes;

};

}       // namespace WIR

#endif  // _WIR_SILICONBUGS_TC1796_CPU_TC_096_H
