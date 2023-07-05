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
  @file peep_preincr.h
  @brief This file provides the interface of a peephole optimizer for
         <TT>ADD.A</TT>/<TT>LEA</TT>/<TT>SUB.A</TT> operations followed by a
         <TT>LD</TT>/<TT>ST</TT> using base+offset addressing with an offset of
         0.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_PEEP_PREINCR_H
#define _WIR_PEEP_PREINCR_H


//
// Include section
//

// Include WIR headers
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
class WIR_Operation;


//
// Header section
//

/*!
  @brief Class TC_Peep_PreIncr is a peephole optimizer for Infineon TriCore
         <TT>ADD.A</TT>/<TT>LEA</TT>/<TT>SUB.A</TT> operations followed by a
         <TT>LD</TT>/<TT>ST</TT> using base+offset addressing with an offset of
         0.

  <B>Example</B>

  <PRE>
  ...
  ADD.A   aA, 6                         ; Increment address register aA by 6
  ST.B    [aA]0, dX                     ; Access memory at address aA + 0
  ...
  </PRE>

  <B>Workaround</B>

  Change the <TT>ST</TT> operation to

  <PRE>
  ST.B    [+aA]6, dX                    ; Access memory at address aA + 6
  </PRE>

  Erase the preceding address register increment/decrement.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class TC_Peep_PreIncr final : public WIR_BitOpt,
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
    explicit TC_Peep_PreIncr( WIR_Function & );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~TC_Peep_PreIncr( void );


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
             with the operation pattern
             <TT>ADD.A</TT>/<TT>LEA</TT>/<TT>SUB.A</TT>,
             <TT>LD</TT>/<TT>ST</TT>.

      @param[in] p A const reference to a peephole.
      @return true if p matches with the given operation pattern, false
              otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool matchPeephole( const WIR_Peephole::peephole & );

    /*!
      @brief transformPeephole optimizes the operation pattern
             <TT>ADD.A</TT>/<TT>LEA</TT>/<TT>SUB.A</TT>, <TT>LD</TT>/<TT>ST</TT>
             in the specified peephole.

      @param[in] p A const reference to a peephole.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void transformPeephole( const WIR_Peephole::peephole & );


  private:

    /*!
      @brief getNewOperationFormat determines the new format of an optimized
             memory access depending on the original memory access's format.

      @param[in] o A const reference to the original memory access operation.
      @return A reference to the operation format resulting from this peephole
              optimization.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const WIR_BaseProcessor::OperationFormat &getNewOperationFormat( const WIR_Operation & ) const;

    /*!
      @brief mFoundIncr points to an address register increment operation
             suitable for optimization.
    */
    WIR_Operation *mFoundIncr;

    //! mIncrement stores the signed byte offset used for address increment.
    long long mIncrement;

    /*!
      @brief mSingleAReg stores whether the found increment operation uses a
             single address register or not.
    */
    bool mSingleAReg;

    /*!
      @brief mReachingDefsAnalyzed stores whether a reaching definitions
             analysis has already been done for the current function or not.
    */
    bool mReachingDefsAnalyzed;

    //! mAReg points to the address register involved in the optimization.
    WIR_BaseRegister *mAReg;

};

}       // namespace WIR

#endif  // _WIR_PEEP_PREINCR_H
