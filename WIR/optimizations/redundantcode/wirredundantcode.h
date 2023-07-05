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
  @file wirredundantcode.h
  @brief This file provides the interface of an optimization eliminating
         redundant code that computes bit-wise equivalent results.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_REDUNDANTCODE_H
#define _WIR_REDUNDANTCODE_H


//
// Include section
//

// Include standard headers
#include <map>

// Include WIR headers
#include <wir/wirtypes.h>
#include <optimizations/bitopt/wirbitopt.h>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_BitDFA;
class WIR_CompilationUnit;
class WIR_Function;
class WIR_RegisterParameter;
class WIR_System;
class WIR_UpDownValue;


/*!
  @brief Class WIR_RedundantCode is a generic, processor-indemendent
         optimization that removes redundant code that computes bit-wise
         equivalent results from %WIR functions.

  This optimization identifies pairs of operations o1 and o2 such that
  -# o1 defines a register parameter p1 and o2 uses a register parameter p2
     where the bit-true data flow along all outgoing edges of p1 is equivalent
     to that of all incoming edges of p2, and
  -# the definition of p1 always reaches the use in p2, and
  -# the registers defined/used by p1 and p2 are different but are of the same
     type.

  In such a situation, the used register parameter p2 is modified such that it
  directly uses the register defined in p1.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_RedundantCode : public WIR_BitOpt
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor for system-level optimization.

      @param[in] s A reference to a WIR_System to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_RedundantCode( WIR_System & );

    /*!
      @brief Default constructor for compilation unit-level optimization.

      @param[in] c A reference to a WIR_CompilationUnit to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_RedundantCode( WIR_CompilationUnit & );

    /*!
      @brief Default constructor for function-level optimization.

      @param[in] f A reference to a WIR_Function to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_RedundantCode( WIR_Function & );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_RedundantCode( void );


  protected:

    /*!
      @brief runOptimization eliminates redundant code in the given system.

      @param[in] s A reference to a WIR_System to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void runOptimization( WIR_System & );

    /*!
      @brief runOptimization eliminates redundant code in the given compilation
             unit.

      @param[in] c A reference to a WIR_CompilationUnit to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void runOptimization( WIR_CompilationUnit & );

    /*!
      @brief runOptimization eliminates redundant code in the given function.

      @param[in] f A reference to a WIR_Function to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void runOptimization( WIR_Function & );

    /*!
      @brief areRedundant checks whether the outgoing bit-value of a defined
             register parameter and the incoming bit-value of a used one are
             redundant for some actual processor architecture.

      @param[in] def A const reference to a defined register parameter.
      @param[in] outValue A const reference to the outgoing up-value of the
                          defined register parameter.
      @param[in] use A const reference to a used register parameter.
      @param[in] inValue A const reference to the incoming up-value of the used
                         register parameter.
      @return Always true.

      Since this task is processor-specific and might or might not be necessary
      for some actual processor, this method is virtual and can be overloaded if
      required.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool areRedundant( const WIR_RegisterParameter &,
                               const WIR_UpDownValue &,
                               const WIR_RegisterParameter &,
                               const WIR_UpDownValue & ) const;


  private:

    /*!
      @brief checkRegisters checks whether the registers involved in a defined
             and a used register parameter are suited for redundant code
             elimination.

      @param[in] def A const reference to a defined register parameter.
      @param[in] use A const reference to a used register parameter.
      @return true iff the involved registers are suited for redundant code
              elimination, false otherwise.

      The involved register are suited for redundant code elimination iff
      -# they are both of the same register type, and
      -# they are different, where the check of difference of registers also
         involves potential precolorings of virtual registers.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool checkRegisters( const WIR_RegisterParameter &,
                         const WIR_RegisterParameter & ) const;

    /*!
      @brief areRedundant checks whether the outgoing bit-value of a defined
             register parameter and the incoming bit-value of a used one are
             redundant.

      @param[in] outValue A const reference to the outgoing up-value of the
                          defined register parameter.
      @param[in] inValue A const reference to the incoming up-value of the used
                         register parameter.
      @return true iff both bit-values are redundant, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool areRedundant( const WIR_UpDownValue &, const WIR_UpDownValue & ) const;

    /*!
      @brief replaceParameters processes map mParameterReplacement and replaces
             all redundant register parameters by their equivalent available
             definition.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void replaceParameters( void );

    /*!
      @brief mParameterReplacement maps a redundant register parameter to that
             register parameter serving as replacement.
    */
    std::map<std::reference_wrapper<WIR_RegisterParameter>, WIR_RegisterParameter *, WIR_Compare<WIR_RegisterParameter>> mParameterReplacement;

    /*!
      @brief mDownValue maps the ID of a redundant register parameter to the
             bit-true down-value.
    */
    std::map<WIR_id_t, WIR_UpDownValue> mDownValue;

    /*!
      @brief mUpValue maps the ID of a redundant register parameter to the
             bit-true up-value.
    */
    std::map<WIR_id_t, WIR_UpDownValue> mUpValue;

};

}       // namespace WIR

#endif  // _WIR_REDUNDANTCODE_H
