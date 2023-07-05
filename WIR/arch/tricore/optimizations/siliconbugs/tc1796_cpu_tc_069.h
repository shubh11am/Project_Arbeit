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
  @file tc1796_cpu_tc_069.h
  @brief This file provides the interface of a peephole optimizer for silicon
         bug TC1796 CPU_TC.069 detection and correction.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_SILICONBUGS_TC1796_CPU_TC_069_H
#define _WIR_SILICONBUGS_TC1796_CPU_TC_069_H


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
  @brief Class TC1796_CPU_TC_069 is a peephole optimizer for Infineon TriCore
         TC1796 silicon bug CPU_TC.069.

  See TC1796, EES-BC, ES-BS, BC Errata Sheet Rel. 2.0, 23.06.2008.

  <B>CPU_TC.069: Potential incorrect operation of <TT>RSLCX</TT> instruction</B>

  A problem exists in the implementation of the <TT>RSLCX</TT> instruction,
  which, under certain circumstances, may lead to data corruption in the TriCore
  internal registers. The problem is caused by the <TT>RSLCX</TT> instruction
  incorrectly detecting a dependency to the following load-store (LS) or loop
  (LP) pipeline instruction, if that instruction uses either address register
  <TT>A0</TT> or <TT>A1</TT> as a source operand, and erroneous forwarding paths
  being enabled.

  Two failure cases are possible:
  -# If the instruction following the <TT>RSLCX</TT> instruction uses
     <TT>A1</TT> as its source 1 operand, the <TT>PCX</TT> value updated by the
     <TT>RSLCX</TT> instruction will be corrupted. Instead of restoring the
     <TT>PCX</TT> value from the lower context information being restored, it
     will restore the return address (<TT>A11</TT>).
  -# If the instruction following the <TT>RSLCX</TT> instruction uses either
     <TT>A1</TT> or <TT>A0</TT> as source 2 operand, the value forwarded (for
     the second instruction) will not be the one stored in the register but the
     one that has just been loaded from memory for the context restore
     (<TT>A11</TT>/<TT>PCX</TT>).

  Note that the problem is triggered whenever the following load-store pipeline
  instruction uses <TT>A0</TT> or <TT>A1</TT> as a source operand. If an integer
  pipeline instruction is executed between the <TT>RSLCX</TT> and the following
  load-store or loop instruction, the problem may still exist.

  <B>Example</B>

  <PRE>
  ...
  RSLCX
  <em>&lt;one optional IP instruction&gt;</em>
  LEA     A0, [A0]0x158c
  ...
  </PRE>

  <B>Workaround</B>

  Any <TT>RSLCX</TT> instruction should be followed by a <TT>NOP</TT> to avoid
  the detection of these false dependencies.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class TC1796_CPU_TC_069 final : public WIR_SiliconBugs
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
    explicit TC1796_CPU_TC_069( WIR_Function & );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~TC1796_CPU_TC_069( void );


  protected:

    /*!
      @brief matchSiliconBug determines whether the specified peephole matches
             with silicon bug CPU_TC.069.

      @param[in] p A const reference to a peephole.
      @return true if p matches with silicon bug CPU_TC.069, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool matchSiliconBug( const WIR_Peephole::peephole & );

    /*!
      @brief fixSiliconBug fixes silicon bug CPU_TC.069.

      @param[in] p A const reference to a peephole.
      @return The peephole after bug fixing.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_Peephole::peephole fixSiliconBug( const WIR_Peephole::peephole & ) const;


  private:

    /*!
      @brief isRSLCX determines whether an operation is an RSLCX operation
             subject to silicon bug CPU_TC.069.

      @param[in] o A const reference to an operation to be examined.
      @return true iff o is an RSLCX operation subject to CPU_TC.069, false
              otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isRSLCX( const WIR_Operation & ) const;

    /*!
      @brief isLSLP determines whether an operation is a LS or LP operation
             subject to silicon bug CPU_TC.069.

      @param[in] o A const reference to an operation to be examined.
      @return true iff o is a LS/LP operation subject to CPU_TC.069, false
              otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isLSLP( const WIR_Operation & ) const;

    /*!
      @brief isIP determines whether an operation is an IP operation subject to
             silicon bug CPU_TC.069.

      @param[in] o A const reference to an operation to be examined.
      @return true iff o is an IP operation subject to CPU_TC.069, false
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

#endif  // _WIR_SILICONBUGS_TC1796_CPU_TC_069_H
