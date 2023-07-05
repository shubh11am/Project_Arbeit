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
  @file tc179x_cpu_tc_105.h
  @brief This file provides the interface of a peephole optimizer for silicon
         bug TC1796/TC1797 CPU_TC.105 detection and correction.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_SILICONBUGS_TC179x_CPU_TC_105_H
#define _WIR_SILICONBUGS_TC179x_CPU_TC_105_H


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
  @brief Class TC179x_CPU_TC_105 is a peephole optimizer for Infineon TriCore
         TC1796/TC1797 silicon bug CPU_TC.105 and for TC1797 silicon bug
         CPU_TC.106.

  See TC1796, EES-BC, ES-BS, BC Errata Sheet Rel. 2.0, 23.06.2008.
  See TC1797, EES-AC, ES-AC, AC Errata Sheet Rel. 1.3, 18.12.2009.

  <B>CPU_TC.105: User / Supervisor mode not staged correctly for Store
                 Instructions</B>

  Bus transactions initiated by TriCore load or store instructions have a number
  of associated attributes such as address, data size etc. derived from the load
  or store instruction itself. In addition, bus transactions also have an IO
  privilege level status flag (User/Supervisor mode) derived from the
  <TT>PSW.IO</TT> bit field. Unlike attributes derived from the instruction, the
  User/Supervisor mode status of TriCore initiated bus transactions is not
  staged correctly in the TriCore pipeline and is derived directly from the
  <TT>PSW.IO</TT> bit field.

  This issue can only cause a problem in certain circumstances, specifically
  when a store transaction is outstanding (e.g. held in the CPU store buffer)
  and the <TT>PSW</TT> is modified to switch from Supervisor to User-0 or User-1
  mode. In this case, the outstanding store transaction, executed in Supervisor
  mode, may be transferred to the bus in User mode (the bus systems do not
  discriminate between User-0 and User-1 modes). Due to the blocking nature of
  load transactions and the fact that User mode code cannot modify the
  <TT>PSW</TT>, neither of these situations can cause a problem.

  <B>Example</B>

  <PRE>
  ...
  ST.W    [aX], dX                      ; Store to Supervisor mode protected SFR
  MTCR    &#35;PSW, dY                  ; Modify PSW.IO to switch to User mode
  ...
  </PRE>

  <B>Workaround</B>

  Any <TT>MTCR</TT> instruction targeting the <TT>PSW</TT>, which may change the
  <TT>PSW.IO</TT> bit field, must be preceded by a <TT>DSYNC</TT> instruction,
  unless it can be guaranteed that not store transaction is outstanding.

  <PRE>
  ...
  ST.W    [aX], dX                      ; Store to Supervisor mode protected SFR
  DSYNC
  MTCR    &#35;PSW, dY                  ; Modify PSW.IO to switch to User mode
  ...
  </PRE>

  <B>CPU_TC.106: Incorrect PSW update for certain IP instructions dual-issued
                 with <TT>MTCR PSW</TT> </B>

  In certain situations where an Integer Pipeline (IP) instruction which updates
  the <TT>PSW</TT> user status bits (e.g. <TT>PSW.V</TT> - Overflow) is followed
  immediately by an <TT>MTCR</TT> instruction targeting the <TT>PSW</TT>, with
  the instructions being dual-issued, the update priority is incorrect. In this
  case, the <TT>PSW</TT> user status bits are updated with the value from the IP
  instruction rather than the later <TT>MTCR</TT> instruction. This situation
  only occurs in 2 cases:
  - <TT>MUL</TT>/<TT>MADD</TT>/<TT>MSUB</TT> instruction followed by
    <TT>MTCR PSW</TT>
  - <TT>RSTV</TT> instruction followed by <TT>MTCR PSW</TT>

  <B>Example</B>

  <PRE>
  ...
  RSTV
  MTCR    &#35;PSW, dY                  ; Modify PSW
  ...
  </PRE>

  <B>Workaround</B>

  Insert one <TT>NOP</TT> instruction between the
  <TT>MUL</TT>/<TT>MADD</TT>/<TT>MSUB</TT>/<TT>RSTV</TT> instruction and the
  <TT>MTCR</TT> instruction updating the <TT>PSW</TT>.

  <PRE>
  ...
  RSTV
  NOP
  MTCR    &#35;PSW, dY                  ; Modify PSW
  ...
  </PRE>

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class TC179x_CPU_TC_105 final : public WIR_SiliconBugs
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
    explicit TC179x_CPU_TC_105( WIR_Function & );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~TC179x_CPU_TC_105( void );


  protected:

    /*!
      @brief matchSiliconBug determines whether the specified peephole matches
             with silicon bug CPU_TC.105.

      @param[in] p A const reference to a peephole.
      @return true if p matches with silicon bug CPU_TC.105, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool matchSiliconBug( const WIR_Peephole::peephole & );

    /*!
      @brief fixSiliconBug fixes silicon bug CPU_TC.105.

      @param[in] p A const reference to a peephole.
      @return The peephole after bug fixing.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_Peephole::peephole fixSiliconBug( const WIR_Peephole::peephole & ) const;


  private:

    /*!
      @brief isMTCR determines whether an operation is an MTCR subject to
             silicon bug CPU_TC.105.

      @param[in] o A const reference to an operation to be examined.
      @return true iff o is an MTCR subject to CPU_TC.105, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isMTCR( const WIR_Operation & ) const;

};

}       // namespace WIR

#endif  // _WIR_SILICONBUGS_TC179x_CPU_TC_105_H
