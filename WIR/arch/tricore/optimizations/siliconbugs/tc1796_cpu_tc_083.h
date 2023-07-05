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
  @file tc1796_cpu_tc_083.h
  @brief This file provides the interface of a peephole optimizer for silicon
         bug TC1796 CPU_TC.083 detection and correction.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_SILICONBUGS_TC1796_CPU_TC_083_H
#define _WIR_SILICONBUGS_TC1796_CPU_TC_083_H


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
  @brief Class TC1796_CPU_TC_083 is a peephole optimizer for Infineon TriCore
         TC1796 silicon bug CPU_TC.083.

  See TC1796, EES-BC, ES-BS, BC Errata Sheet Rel. 2.0, 23.06.2008.

  <B>CPU_TC.083: Interrupt may be taken following <TT>DISABLE</TT>
                 instruction</B>

  The TriCore Architecture requires that the <TT>DISABLE</TT> instruction gives
  deterministic behavior, i.e. no interrupt may be taken following the execution
  of the <TT>DISABLE</TT> instruction.

  However, the current implementation allows an interrupt to be taken
  immediately following the execution of the <TT>DISABLE</TT> instruction, i.e.
  between the <TT>DISABLE</TT> and the following instruction. Once the first
  instruction after the <TT>DISABLE</TT> instruction has been executed it is
  still guaranteed that no interrupt will be taken.

  <B>Workaround</B>

  If an instruction sequence must not be interrupted, then the <TT>DISABLE</TT>
  instruction must be followed by a single <TT>NOP</TT> instruction, before the
  critical code sequence.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class TC1796_CPU_TC_083 final : public WIR_SiliconBugs
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
    explicit TC1796_CPU_TC_083( WIR_Function & );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~TC1796_CPU_TC_083( void );


  protected:

    /*!
      @brief matchSiliconBug determines whether the specified peephole matches
             with silicon bug CPU_TC.083.

      @param[in] p A const reference to a peephole.
      @return true if p matches with silicon bug CPU_TC.083, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool matchSiliconBug( const WIR_Peephole::peephole & );

    /*!
      @brief fixSiliconBug fixes silicon bug CPU_TC.083.

      @param[in] p A const reference to a peephole.
      @return The peephole after bug fixing.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_Peephole::peephole fixSiliconBug( const WIR_Peephole::peephole & ) const;

};

}       // namespace WIR

#endif  // _WIR_SILICONBUGS_TC1796_CPU_TC_083_H
