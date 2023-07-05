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
  @file tc1796_cpu_tc_048.h
  @brief This file provides the interface of a peephole optimizer for silicon
         bug TC1796 CPU_TC.048 detection and correction.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_SILICONBUGS_TC1796_CPU_TC_048_H
#define _WIR_SILICONBUGS_TC1796_CPU_TC_048_H


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
  @brief Class TC1796_CPU_TC_048 is a peephole optimizer for Infineon TriCore
         TC1796 silicon bug CPU_TC.048.

  See TC1796, EES-BC, ES-BS, BC Errata Sheet Rel. 2.0, 23.06.2008.

  <B>CPU_TC.048: CPU fetches program from unexpected address</B>

  There is a case which can cause the CPU to fetch program code from an
  unexpected address. Although this code will not be executed the program fetch
  itself can cause side effects (performance degradation, program fetch bus
  error trap).

  If a load address register instruction <TT>LD.A</TT>/<TT>LD.DA</TT> is being
  followed immediately by an indirect jump <TT>JI</TT>, <TT>JLI</TT> or indirect
  call <TT>CALLI</TT> instruction with the same address register as parameter,
  the CPU might fetch program from an unexpected address.

  <B>Workaround</B>

  Insert a <TT>NOP</TT> instruction or any other load/store instruction between
  the load and the indirect jump/call instruction. (See also note "Pipeline
  Effects", below)

  <B>Example</B>

  <PRE>
  ...
  LD.A    A14, &lt;any addressing mode&gt;
  NOP                                         ; workaround to prevent program fetch from undefined address
  <em>&lt;one optional IP instruction&gt;</em>
  CALLI   A14
  ...
  </PRE>

  <B>Pipeline Effects</B>

  The CPU core architecture allows to decode and execute instructions for the
  integer pipeline (IP) and the load/store pipeline (LS) in parallel. Therefore
  this bug hits also if there is only one IP instruction sitting in front of the
  offending LS instruction (<TT>"CALLI A14"</TT> in above example). A detailed
  list of IP instructions can be found in the document "TriCore DSP Optimization
  Guide - Part 1: Instruction Set, Chapter 13.1.3, Table of Dual Issue
  Instructions".

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class TC1796_CPU_TC_048 final : public WIR_SiliconBugs
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
    explicit TC1796_CPU_TC_048( WIR_Function & );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~TC1796_CPU_TC_048( void );


  protected:

    /*!
      @brief matchSiliconBug determines whether the specified peephole matches
             with silicon bug CPU_TC.048.

      @param[in] p A const reference to a peephole.
      @return true if p matches with silicon bug CPU_TC.048, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool matchSiliconBug( const WIR_Peephole::peephole & );

    /*!
      @brief fixSiliconBug fixes silicon bug CPU_TC.048.

      @param[in] p A const reference to a peephole.
      @return The peephole after bug fixing.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_Peephole::peephole fixSiliconBug( const WIR_Peephole::peephole & ) const;


  private:

    /*!
      @brief isLDA determines whether an operation is an address register load
             subject to silicon bug CPU_TC.048.

      @param[in] o A const reference to an operation to be examined.
      @return true iff o is an address register load subject to CPU_TC.048,
              false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isLDA( const WIR_Operation & ) const;

    /*!
      @brief isIndirect determines whether an operation is an indirect jump or
             call subject to silicon bug CPU_TC.048.

      @param[in] o A const reference to an operation to be examined.
      @return true iff o is an indirect jump/call subject to CPU_TC.048, false
              otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isIndirect( const WIR_Operation & ) const;

    /*!
      @brief isIP determines whether an operation is an IP operation subject to
             silicon bug CPU_TC.048.

      @param[in] o A const reference to an operation to be examined.
      @return true iff o is an IP operation subject to CPU_TC.048, false
              otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isIP( const WIR_Operation & ) const;

    /*!
      @brief mMatchedPeepholes stores IDs of the first operations of all matched
             peepholes in order to avoid fixing the same code sequence several
             times for peepholes of different sizes.
    */
    std::set<WIR_id_t> mMatchedPeepholes;

};

}       // namespace WIR

#endif  // _WIR_SILICONBUGS_TC1796_CPU_TC_048_H
