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
  @file tc1796_cpu_tc_013.h
  @brief This file provides the interface of a peephole optimizer for silicon
         bug TC1796 CPU_TC.013 detection and correction.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_SILICONBUGS_TC1796_CPU_TC_013_H
#define _WIR_SILICONBUGS_TC1796_CPU_TC_013_H


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
  @brief Class TC1796_CPU_TC_013 is a peephole optimizer for Infineon TriCore
         TC1796 silicon bug CPU_TC.013.

  See TC1796, EES-BC, ES-BS, BC Errata Sheet Rel. 2.0, 23.06.2008.

  <B>CPU_TC.013: Unreliable context load/store operation following an address
                 register load instruction</B>

  When an address register is being loaded by a load/store instruction
  <TT>LD.A</TT>/<TT>LD.DA</TT> and this address register is being used as
  address pointer in a following context load/store instruction
  <TT>LD*CX</TT>/<TT>ST*CX</TT> it may lead to unpredictable behavior.

  <B>Example</B>

  <PRE>
  ...
  LD.A    A3, &lt;any addressing mode&gt;           ; load value into A3
  LDLCX   [A3]                                ; context load
  ...
  </PRE>

  <B>Workaround</B>

  Insert one <TT>NOP</TT> instruction between the address register load/store
  instruction and the context load/store instruction to allow the "Load Word to
  Address Register" operation to be completed first.

  <PRE>
  ...
  LD.A    A3, &lt;any addressing mode&gt;
  NOP
  LDLCX   [A3]
  ...
  </PRE>

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class TC1796_CPU_TC_013 final : public WIR_SiliconBugs
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
    explicit TC1796_CPU_TC_013( WIR_Function & );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~TC1796_CPU_TC_013( void );


  protected:

    /*!
      @brief matchSiliconBug determines whether the specified peephole matches
             with silicon bug CPU_TC.013.

      @param[in] p A const reference to a peephole.
      @return true if p matches with silicon bug CPU_TC.013, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool matchSiliconBug( const WIR_Peephole::peephole & );

    /*!
      @brief fixSiliconBug fixes silicon bug CPU_TC.013.

      @param[in] p A const reference to a peephole.
      @return The peephole after bug fixing.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_Peephole::peephole fixSiliconBug( const WIR_Peephole::peephole & ) const;


  private:

    /*!
      @brief isLDA determines whether an operation is an address register load
             subject to silicon bug CPU_TC.013.

      @param[in] o A const reference to an operation to be examined.
      @return true iff o is an address register load subject to CPU_TC.013,
              false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isLDA( const WIR_Operation & ) const;

    /*!
      @brief isCTX determines whether an operation is a context load/store
             subject to silicon bug CPU_TC.013.

      @param[in] o A const reference to an operation to be examined.
      @return true iff o is a context load/store subject to CPU_TC.013, false
              otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isCTX( const WIR_Operation & ) const;

};

}       // namespace WIR

#endif  // _WIR_SILICONBUGS_TC1796_CPU_TC_013_H
