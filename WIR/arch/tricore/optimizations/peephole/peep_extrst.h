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
  @file peep_extrst.h
  @brief This file provides the interface of a peephole optimizer for
         <TT>EXTR</TT>/<TT>EXTR.U</TT> operations followed by a
         <TT>ST.B</TT>/<TT>ST.H</TT>/<TT>ST.W</TT>.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_PEEP_EXTRST_H
#define _WIR_PEEP_EXTRST_H


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
  @brief Class TC_Peep_EXTRST is a peephole optimizer for Infineon TriCore
         <TT>EXTR</TT> operations followed by a <TT>ST</TT> operation.

  <B>Example</B>

  <PRE>
  ...
  EXTR    dA, dB, 0, 8                  ; Extract least-significant byte into A
  ST.B    [aX], dA                      ; Store least-significant byte of A
  ...
  </PRE>

  <B>Workaround</B>

  Change the <TT>ST</TT> operation to

  <PRE>
  ST.B    [aX], dB
  </PRE>

  Erase the preceding <TT>EXTR</TT> operation if data flow permits.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class TC_Peep_EXTRST final : public WIR_BitOpt,
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
    explicit TC_Peep_EXTRST( WIR_Function & );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~TC_Peep_EXTRST( void );


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
             with the operation pattern <TT>EXTR</TT>, <TT>ST</TT>.

      @param[in] p A const reference to a peephole.
      @return true if p matches with the given operation pattern, false
              otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool matchPeephole( const WIR_Peephole::peephole & );

    /*!
      @brief transformPeephole optimizes the operation pattern <TT>EXTR</TT>,
             <TT>ST</TT> in the specified peephole.

      @param[in] p A const reference to a peephole.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void transformPeephole( const WIR_Peephole::peephole & );


  private:

    /*!
      @brief mFoundSTs stores all ST operations suitable for optimization for
             one actual EXTR operation.
    */
    WIR_OperationSet mFoundSTs;

};

}       // namespace WIR

#endif  // _WIR_PEEP_EXTRST_H
