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
  @file tc1796_cpu_tc_071.h
  @brief This file provides the interface of a peephole optimizer for silicon
         bug TC1796 CPU_TC.071 detection and correction.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_SILICONBUGS_TC1796_CPU_TC_071_H
#define _WIR_SILICONBUGS_TC1796_CPU_TC_071_H


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


//
// Header section
//

/*!
  @brief Class TC1796_CPU_TC_071 is a peephole optimizer for Infineon TriCore
         TC1796 silicon bug CPU_TC.071.

  See TC1796, EES-BC, ES-BS, BC Errata Sheet Rel. 2.0, 23.06.2008.

  <B>CPU_TC.071: Error when Conditional Loop targets Unconditional Loop</B>

  An error in the program flow may occur when a conditional loop instruction
  (<TT>LOOP</TT>) has as its target an unconditional loop instruction
  (<TT>LOOPU</TT>). The incorrect behavior occurs in certain circumstances when
  the two instructions are resolved in the same cycle. If the conditional loop
  instruction is mispredicted, i.e. the conditional loop should be exited, the
  unconditional loop instruction is correctly cancelled but instead of program
  execution continuing at the first instruction after the conditional loop, the
  program flow is corrupted.

  <B>Example</B>

  <PRE>
  ...
  cond_loop_target_:
  LOOPU   uncond_loop_target_                 ; Unconditional loop
  ...
  LOOP    A6, cond_loop_target_               ; Conditional loop targets unconditional loop
  ...
  </PRE>

  <B>Workaround</B>

  The first instruction of a conditional loop may not be an unconditional loop.
  If necessary, precede this unconditional loop instruction with a single
  <TT>NOP</TT>.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class TC1796_CPU_TC_071 final : public WIR_SiliconBugs
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
    explicit TC1796_CPU_TC_071( WIR_Function & );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~TC1796_CPU_TC_071( void );


  protected:

    /*!
      @brief matchSiliconBug determines whether the specified peephole matches
             with silicon bug CPU_TC.071.

      @param[in] p A const reference to a peephole.
      @return true if p matches with silicon bug CPU_TC.071, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool matchSiliconBug( const WIR_Peephole::peephole & );

    /*!
      @brief fixSiliconBug fixes silicon bug CPU_TC.071.

      @param[in] p A const reference to a peephole.
      @return The peephole after bug fixing.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_Peephole::peephole fixSiliconBug( const WIR_Peephole::peephole & ) const;

};

}       // namespace WIR

#endif  // _WIR_SILICONBUGS_TC1796_CPU_TC_071_H
