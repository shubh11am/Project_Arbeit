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
  @file tc1796_cpu_tc_081.h
  @brief This file provides the interface of a peephole optimizer for silicon
         bug TC1796 CPU_TC.081 detection and correction.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_SILICONBUGS_TC1796_CPU_TC_081_H
#define _WIR_SILICONBUGS_TC1796_CPU_TC_081_H


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
  @brief Class TC1796_CPU_TC_081 is a peephole optimizer for Infineon TriCore
         TC1796 silicon bug CPU_TC.081.

  See TC1796, EES-BC, ES-BS, BC Errata Sheet Rel. 2.0, 23.06.2008.

  <B>CPU_TC.081: Error during Load A[10], Call / Exception Sequence</B>

  A problem may occur when an address register load instruction, <TT>LD.A</TT>
  or <TT>LD.DA</TT>, targeting the <TT>A[10]</TT> register, is immediately
  followed by an operation causing a context switch. The problem may occur in
  one of two situations:
  -# The address register load instruction, targeting <TT>A[10]</TT>, is
     followed immediately by a call instruction (<TT>CALL</TT>, <TT>CALLA</TT>,
     <TT>CALLI</TT>).
  -# The address register load instruction, targeting <TT>A[10]</TT>, is
     followed immediately by a context switch caused by an interrupt or trap
     being taken, where the interrupt stack is already in use (<TT>PWS.IS</TT> =
     1).

  In both these situations the value of <TT>A[10]</TT> is required to be
  maintained across the context switch. However, where the context switch is
  preceded by a load to <TT>A[10]</TT>, the address register dependency is not
  detected correctly and the called context inherits the wrong value of
  <TT>A[10]</TT>. In this case the value of <TT>A[10]</TT> before the load
  instruction is inherited.

  <B>Example</B>

  <PRE>
  ...
  LD.A    A10, &lt;any addressing mode&gt;
  CALL    call_target_
  ...
  </PRE>

  <B>Workaround</B>

  The problem only occurs when <TT>A[10]</TT> is loaded directly from memory.
  The software workaround therefore consists of loading another address register
  from memory and moving the contents to <TT>A[10]</TT>.

  <B>Example</B>

  <PRE>
  ...
  LD.A    A12, &lt;any addressing mode&gt;
  MOV.AA  A10, A12
  CALL    call_target_
  ...
  </PRE>

  Since the detection of a free alternative address register is difficult after
  register allocation, this silicon bug is fixed for %WIR code using virtual
  registers, i.e., before register allocation. If the problematic situation is
  detected in %WIR code with physical registers, a fatal exception is thrown.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class TC1796_CPU_TC_081 final : public WIR_SiliconBugs
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
    explicit TC1796_CPU_TC_081( WIR_Function & );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~TC1796_CPU_TC_081( void );


  protected:

    /*!
      @brief matchSiliconBug determines whether the specified peephole matches
             with silicon bug CPU_TC.081.

      @param[in] p A const reference to a peephole.
      @return true if p matches with silicon bug CPU_TC.081, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool matchSiliconBug( const WIR_Peephole::peephole & );

    /*!
      @brief fixSiliconBug fixes silicon bug CPU_TC.081.

      @param[in] p A const reference to a peephole.
      @return The peephole after bug fixing.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_Peephole::peephole fixSiliconBug( const WIR_Peephole::peephole & ) const;


  private:

    /*!
      @brief isLDA determines whether an operation is an address register load
             subject to silicon bug CPU_TC.081.

      @param[in] o A const reference to an operation to be examined.
      @return true iff o is an address register load subject to CPU_TC.081,
              false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isLDA( const WIR_Operation & ) const;

    /*!
      @brief definesSP determines whether an operation defines the stack pointer
             A10.

      @param[in] o A const reference to an operation to be examined.
      @return true iff o defines A10, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool definesSP( const WIR_Operation & ) const;

    /*!
      @brief mMatchedPeepholes stores IDs of the first operations of all matched
             peepholes in order to avoid fixing the same code sequence several
             times for peepholes of different sizes.
    */
    std::set<WIR_id_t> mMatchedPeepholes;

};

}       // namespace WIR

#endif  // _WIR_SILICONBUGS_TC1796_CPU_TC_081_H
