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
  @file tc1796_cpu_tc_082.h
  @brief This file provides the interface of a peephole optimizer for silicon
         bug TC1796 CPU_TC.082 detection and correction.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_SILICONBUGS_TC1796_CPU_TC_082_H
#define _WIR_SILICONBUGS_TC1796_CPU_TC_082_H


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
  @brief Class TC1796_CPU_TC_082 is a peephole optimizer for Infineon TriCore
         TC1796 silicon bug CPU_TC.082.

  See TC1796, EES-BC, ES-BS, BC Errata Sheet Rel. 2.0, 23.06.2008.

  <B>CPU_TC.082: Data corruption possible when Memory Load follows Context
                 Store</B>

  Data corruption may occur when a context store operation, <TT>STUCX</TT> or
  <TT>STLCX</TT> is immediately followed by a memory load operation which reads
  from the last double-word address written by the context store.

  Context store operations store a complete upper or lower context to a 16-word
  region of memory, aligned on a 16-word boundary. If the context store is
  immediately followed by a memory load operation which reads from the last
  double-word of the 16-word context region just written, the dependency is not
  detected correctly and the previous value held in this memory location may be
  returned by the memory load.

  The memory load instructions which may return corrupt data are as follows:

  <TT>LD.B</TT>, <TT>LD.BU</TT>, <TT>LD.H</TT>, <TT>LD.HU</TT>, <TT>LD.Q</TT>,
  <TT>LD.W</TT>, <TT>LD.D</TT>, <TT>LD.A</TT>, <TT>LD.DA</TT>

  <B>Example</B>

  <PRE>
  ...
  STLCX   0xD0000040
  LD.W    D15, 0xD0000078
  ...
  </PRE>

  Note that the TriCore architecture does not require a context save operation
  (<TT>CALL</TT>, <TT>SVLCX</TT>, etc.) to update the CSA list semantically
  before the next operation (but does require the CSA list to be up to date
  after the execution of a <TT>DSYNC</TT> instruction). As such the same problem
  may occur for context save operations, but the result of such a sequence is
  architecturally undefined in any case.

  <B>Workaround</B>

  One <TT>NOP</TT> instruction must be inserted between the context store
  operation and a following memory load instruction if the memory load may read
  from the last double-word of the 16-word context region just written.

  <B>Example</B>

  <PRE>
  ...
  STLCX   0xD0000040
  NOP
  LD.W    D15, 0xD0000078
  ...
  </PRE>

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class TC1796_CPU_TC_082 final : public WIR_SiliconBugs
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
    explicit TC1796_CPU_TC_082( WIR_Function & );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~TC1796_CPU_TC_082( void );


  protected:

    /*!
      @brief matchSiliconBug determines whether the specified peephole matches
             with silicon bug CPU_TC.082.

      @param[in] p A const reference to a peephole.
      @return true if p matches with silicon bug CPU_TC.082, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool matchSiliconBug( const WIR_Peephole::peephole & );

    /*!
      @brief fixSiliconBug fixes silicon bug CPU_TC.082.

      @param[in] p A const reference to a peephole.
      @return The peephole after bug fixing.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_Peephole::peephole fixSiliconBug( const WIR_Peephole::peephole & ) const;


  private:

    /*!
      @brief isSTCTX determines whether an operation is a context store subject
             to silicon bug CPU_TC.082.

      @param[in] o A const reference to an operation to be examined.
      @return true iff o is a context store subject to CPU_TC.082, false
              otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isSTCTX( const WIR_Operation & ) const;

    /*!
      @brief isLD determines whether an operation is a load subject to silicon
             bug CPU_TC.082.

      @param[in] o A const reference to an operation to be examined.
      @return true iff o is a load subject to CPU_TC.082, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isLD( const WIR_Operation & ) const;

};

}       // namespace WIR

#endif  // _WIR_SILICONBUGS_TC1796_CPU_TC_082_H
