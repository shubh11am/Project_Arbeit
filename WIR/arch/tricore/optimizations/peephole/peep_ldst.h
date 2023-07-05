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
  @file peep_ldst.h
  @brief This file provides the interface of a peephole optimizer for
         <TT>LD</TT> operations followed by a redundant <TT>ST</TT>.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_PEEP_LDST_H
#define _WIR_PEEP_LDST_H


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
class WIR_LabelParameter;
class WIR_Operation;


//
// Header section
//

/*!
  @brief Class TC_Peep_LDST is a peephole optimizer for Infineon TriCore
         <TT>LD</TT> operations followed by a redundant <TT>ST</TT> operation.

  <B>Example</B>

  <PRE>
  ...
  LD.B    dA, [aB]                      ; Load byte into A
  ST.B    [aB], dA                      ; Store the very same byte of A to the
  ...                                   ; very same address
  </PRE>

  <B>Workaround</B>

  Erase the redundant <TT>ST</TT> operation if data flow permits and the
  addresses of both the <TT>LD</TT> and the <TT>ST</TT> are the same and if the
  <TT>ST</TT> does not refer to a volatile symbol.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class TC_Peep_LDST final : public WIR_BitOpt,
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
    explicit TC_Peep_LDST( WIR_Function & );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~TC_Peep_LDST( void );


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
             with the operation pattern <TT>LD</TT>, <TT>ST</TT>.

      @param[in] p A const reference to a peephole.
      @return true if p matches with the given operation pattern, false
              otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool matchPeephole( const WIR_Peephole::peephole & );

    /*!
      @brief transformPeephole optimizes the operation pattern <TT>LD</TT>,
             <TT>ST</TT> in the specified peephole.

      @param[in] p A const reference to a peephole.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void transformPeephole( const WIR_Peephole::peephole & );


  private:

    /*!
      @brief sameAddress checks whether the specified <TT>LD</TT> and
             <TT>ST</TT> operations refer to the same address.

      @param[in] ld A const reference to an <TT>LD</TT> operation.
      @param[in] st A const reference to a <TT>ST</TT> operation.
      @return true if both operations refer to the same address, false
              otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool sameAddress( const WIR_Operation &, const WIR_Operation & ) const;

    /*!
      @brief sameLabel checks whether two label parameters are equal.

      @param[in] lab1 A const reference to a first label parameter.
      @param[in] lab2 A const reference to a second label parameter.
      @return true if both labels refer to the same place, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool sameLabel( const WIR_LabelParameter &,
                    const WIR_LabelParameter & ) const;

    /*!
      @brief isStackAccesses whether a <TT>LD</TT> or <TT>ST</TT> operation uses
             the stack pointer and thus accesses the stack.

      @param[in] o A const reference to a <TT>LD</TT> or <TT>ST</TT> operation.
      @return true if the operation uses the stack pointer, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isStackAccess( const WIR_Operation & ) const;

    /*!
      @brief accessesVolatileSymbol determines whether a <TT>ST</TT> operation
             accesses a volatile data object.

      @param[in] o A const reference to a <TT>ST</TT> operation.
      @return true if the operation accesses volatile data, false otherwise.

      In order to determine which data objects are actually accessed, attached
      data access containers are inspected as well as information from bit-true
      data flow analysis is considered.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool accessesVolatileData( const WIR_Operation & ) const;

    /*!
      @brief mFoundSTs stores all redundant ST operations suitable for
             optimization for one actual LD operation.
    */
    WIR_OperationSet mFoundSTs;

};

}       // namespace WIR

#endif  // _WIR_PEEP_LDST_H
