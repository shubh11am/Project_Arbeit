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
  @file tc1796_cpu_tc_072.h
  @brief This file provides the interface of a peephole optimizer for silicon
         bug TC1796 CPU_TC.072 detection and correction.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_SILICONBUGS_TC1796_CPU_TC_072_H
#define _WIR_SILICONBUGS_TC1796_CPU_TC_072_H


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
  @brief Class TC1796_CPU_TC_072 is a peephole optimizer for Infineon TriCore
         TC1796 silicon bug CPU_TC.072.

  See TC1796, EES-BC, ES-BS, BC Errata Sheet Rel. 2.0, 23.06.2008.

  <B>CPU_TC.072: Error when Loop Counter modified prior to Loop instruction</B>

  An error in the program flow may occur when an instruction that updates an
  address register is directly followed by a conditional loop instruction which
  uses that address register as its loop counter. The problem occurs when the
  address register holding the loop counter is initially zero, such that the
  loop will exit, but is written to a non-zero value by the instruction
  preceding the conditional loop. In this case the loop prediction logic fails
  and the program flow is corrupted.

  <B>Example</B>

  <PRE>
  ...
  LD.A    A6, &lt;any addressing mode&gt;
  LOOP    A6, loop_target_1_                  ; unconditional loop
  ...
  </PRE>

  <B>Workaround</B>

  Insert one <TT>NOP</TT> instruction between the instruction updating the
  address register and the conditional loop instruction dependent on this
  address register.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class TC1796_CPU_TC_072 final : public WIR_SiliconBugs
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
    explicit TC1796_CPU_TC_072( WIR_Function & );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~TC1796_CPU_TC_072( void );


  protected:

    /*!
      @brief matchSiliconBug determines whether the specified peephole matches
             with silicon bug CPU_TC.072.

      @param[in] p A const reference to a peephole.
      @return true if p matches with silicon bug CPU_TC.072, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool matchSiliconBug( const WIR_Peephole::peephole & );

    /*!
      @brief fixSiliconBug fixes silicon bug CPU_TC.072.

      @param[in] p A const reference to a peephole.
      @return The peephole after bug fixing.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_Peephole::peephole fixSiliconBug( const WIR_Peephole::peephole & ) const;

};

}       // namespace WIR

#endif  // _WIR_SILICONBUGS_TC1796_CPU_TC_072_H
