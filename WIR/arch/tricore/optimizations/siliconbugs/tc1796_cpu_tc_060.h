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
  @file tc1796_cpu_tc_060.h
  @brief This file provides the interface of a peephole optimizer for silicon
         bug TC1796 CPU_TC.060 detection and correction.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_SILICONBUGS_TC1796_CPU_TC_060_H
#define _WIR_SILICONBUGS_TC1796_CPU_TC_060_H


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
  @brief Class TC1796_CPU_TC_060 is a peephole optimizer for Infineon TriCore
         TC1796 silicon bug CPU_TC.060.

  See TC1796, EES-BC, ES-BS, BC Errata Sheet Rel. 2.0, 23.06.2008.

  <B>CPU_TC.060: LD.[A,DA] followed by a dependent LD.[DA,D,W] can produce
                 unreliable results</B>

  An <TT>LD.A</TT> or <TT>LD.DA</TT> instruction followed back to back by an
  unaligned <TT>LD.DA</TT>, <TT>LD.D</TT> or <TT>LD.W</TT> instruction can lead
  to unreliable results. This problem is independent of the instruction formats
  (16 and 32 bit versions of both instructions are similarly affected)

  The problem shows up if the <TT>LD.DA</TT>, <TT>LD.D</TT> or <TT>LD.W</TT>
  uses an address register which is loaded by the preceding <TT>LD.A</TT> or
  <TT>LD.DA</TT> and if the <TT>LD.DA</TT>, <TT>LD.D</TT> or <TT>LD.W</TT>
  accesses data which leads to a multicycle execution of this second
  instruction.

  A multicycle execution of <TT>LD.DA</TT>, <TT>LD.D</TT> or <TT>LD.W</TT> will
  be triggered only if the accessed data spans a 128 bit boundary in the local
  <TT>DSPR</TT> space or a 128 bit boundary in the cached space. In the non
  cached space an access spanning a 64 bit boundary can lead to a multicycle
  execution.

  The malfunction is additionally dependent on the previous content of the used
  address register - the bug appears if the content points to the unimplemented
  <TT>DSPR</TT> space.

  In the buggy case the upper portion of the multicycle load is derived from a
  wrong address (the address is dependent on the previous content of that
  address register) and the buggy case can lead to a one cycle FASTER execution
  of this back to back case. (one stall bubble is lacking in this case)

  The 16 and 32 bit variants of both instructions are affected equally. A single
  IP instruction as workaround is NOT sufficient, as it gets dual issued with
  the <TT>LD.[DA,D,W]</TT> and therefore no bubble is seen by the LS pipeline in
  such a case.

  <B>Example</B>

  <PRE>
  ...
  LD.A    A3, &lt;any addressing mode&gt;           ; load pointer into A3
  LD.W    D1, [A3]&lt;any addressing mode&gt;       ; load data value from pointer
  ...
  </PRE>

  <B>Workaround</B>

  Insert one <TT>NOP</TT> instruction between the address register load/store
  instruction and the data load/store instruction to allow the "Load Word to
  Address Register" operation to be completed first. This leads to a slight
  performance degradation.

  <PRE>
  ...
  LD.A    A3, &lt;any addressing mode&gt;           ; load pointer into A3
  NOP
  LD.W    D1, [A3]&lt;any addressing mode&gt;       ; load data value from pointer
  ...
  </PRE>

  <B>Alternative Workaround</B>

  To avoid the slight performance degradation, an alternative workaround is to
  avoid any data structures that are accessed in an unaligned manner as then the
  described instruction sequence does NOT exhibit any problems.

  Since, however, such an alignment has not yet been realized within WCC
  (neither for data nor for stack objects), this alternative is currently
  infeasible.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class TC1796_CPU_TC_060 final : public WIR_SiliconBugs
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
    explicit TC1796_CPU_TC_060( WIR_Function & );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~TC1796_CPU_TC_060( void );


  protected:

    /*!
      @brief matchSiliconBug determines whether the specified peephole matches
             with silicon bug CPU_TC.060.

      @param[in] p A const reference to a peephole.
      @return true if p matches with silicon bug CPU_TC.060, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool matchSiliconBug( const WIR_Peephole::peephole & );

    /*!
      @brief fixSiliconBug fixes silicon bug CPU_TC.060.

      @param[in] p A const reference to a peephole.
      @return The peephole after bug fixing.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_Peephole::peephole fixSiliconBug( const WIR_Peephole::peephole & ) const;


  private:

    /*!
      @brief isLDA determines whether an operation is an address register load
             subject to silicon bug CPU_TC.060.

      @param[in] o A const reference to an operation to be examined.
      @return true iff o is an address register load subject to CPU_TC.060,
              false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isLDA( const WIR_Operation & ) const;

    /*!
      @brief isLDAW determines whether an operation is an LD_DA, LD_D or LD_W
             subject to silicon bug CPU_TC.060.

      @param[in] o A const reference to an operation to be examined.
      @return true iff o is an LD_DA, LD_D or LD_W subject to silicon bug
              CPU_TC.060., false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isLDAW( const WIR_Operation & ) const;

};

}       // namespace WIR

#endif  // _WIR_SILICONBUGS_TC1796_CPU_TC_060_H
