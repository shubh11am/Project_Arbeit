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
  @file peep_ldextr.h
  @brief This file provides the interface of a peephole optimizer for
         <TT>LD.B</TT>/<TT>LD.BU</TT>/<TT>LD.H</TT>/<TT>LD.HU</TT>/<TT>LD.W</TT>
         operations followed by a <TT>EXTR</TT>/<TT>EXTR.U</TT>.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_PEEP_LDEXTR_H
#define _WIR_PEEP_LDEXTR_H


//
// Include section
//

// Include WIR headers
#include <wir/wirtypes.h>
#include <optimizations/bitopt/wirbitopt.h>
#include <optimizations/peephole/wirpeephole.h>


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
  @brief Class TC_Peep_LDEXTR is a peephole optimizer for Infineon TriCore
         <TT>LD</TT> operations followed by an <TT>EXTR</TT> operation.

  <B>Example</B>

  <PRE>
  ...
  LD.B    dA, [aX]                      ; Load least-significant byte of A
  EXTR    dB, dA, 0, 8                  ; Extract least-significant byte of A
  ...
  </PRE>

  <B>Workaround</B>

  Erase the succeeding <TT>EXTR</TT> operation if data flow permits.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class TC_Peep_LDEXTR final : public WIR_BitOpt,
                             public WIR_Peephole
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
    explicit TC_Peep_LDEXTR( WIR_Function & );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~TC_Peep_LDEXTR( void );


    //
    // Optimization management.
    //

    /*!
      @brief optimize performs the peephole optimization.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void optimize( void );


  protected:

    /*!
      @brief runOptimization performs peephole optimization in the given
             function.

      @param[in] f A reference to a WIR_Function to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void runOptimization( WIR_Function & );

    /*!
      @brief matchPeephole determines whether the specified peephole matches
             with the operation pattern <TT>LD</TT>, <TT>EXTR</TT>.

      @param[in] p A const reference to a peephole.
      @return true if p matches with the given operation pattern, false
              otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool matchPeephole( const WIR_Peephole::peephole & );

    /*!
      @brief transformPeephole optimizes the operation pattern <TT>LD</TT>,
             <TT>EXTR</TT> in the specified peephole.

      @param[in] p A const reference to a peephole.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void transformPeephole( const WIR_Peephole::peephole & );


  private:

    /*!
      @brief mFoundEXTRs stores all EXTR operations suitable for optimization
             for one actual LD operation.
    */
    WIR_OperationSet mFoundEXTRs;

    /*!
      @brief mPatchLd stores whether a LD operation needs to be patched to LD.U
             or vice versa.
    */
    bool mPatchLd;

    //! mNewOpcode stores an LD's new opcode if that one needs to be patched.
    const WIR_BaseProcessor::OpCode *mNewOpcode;

};

}       // namespace WIR

#endif  // _WIR_PEEP_LDEXTR_H
