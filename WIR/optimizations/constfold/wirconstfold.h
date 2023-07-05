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
  @file wirconstfold.h
  @brief This file provides the interface of a constant folding optimization.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_CONSTFOLD_H
#define _WIR_CONSTFOLD_H


//
// Include section
//

// Include standard headers
#include <functional>
#include <list>
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

class WIR_CompilationUnit;
class WIR_Function;
enum class WIR_L4 : char;
class WIR_Operation;
class WIR_RegisterParameter;
class WIR_System;
class WIR_UpDownValue;


/*!
  @brief Class WIR_ConstFold is an optimization that folds constants in %WIR
         functions.

  According to Steven S. Muchnik (Advanced Compiler Design & Implementation,
  page 329), "constant folding refers to the evaluation at compile time of
  expressions whose operands are known to be constant."

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_ConstFold : public WIR_BitOpt
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
    explicit WIR_ConstFold( WIR_System & );

    /*!
      @brief Default constructor for compilation unit-level optimization.

      @param[in] c A reference to a WIR_CompilationUnit to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_ConstFold( WIR_CompilationUnit & );

    /*!
      @brief Default constructor for function-level optimization.

      @param[in] f A reference to a WIR_Function to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_ConstFold( WIR_Function & );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_ConstFold( void );


  protected:

    /*!
      @brief runOptimization folds constants in the given system.

      @param[in] s A reference to a WIR_System to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void runOptimization( WIR_System & );

    /*!
      @brief runOptimization folds constants in the given compilation unit.

      @param[in] c A reference to a WIR_CompilationUnit to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void runOptimization( WIR_CompilationUnit & );

    /*!
      @brief runOptimization folds constants in the given function.

      @param[in] f A reference to a WIR_Function to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void runOptimization( WIR_Function & );

    /*!
      @brief For an operation identified to be constant, doConstFolding does the
             actual folding.

      @param[in] o A const reference to a %WIR operation identified as constant.
      @param[in] outValue A const reference to a map mapping all defined or
                          def-used register parameters to their outgoing bit
                          value.
      @param[in] inValue A const reference to a map mapping all used or def-used
                         register parameters to their incoming bit value.
      @return A Boolean denoting whether new instructions were produced for o or
              not.

      Since the actual folding of a constant operation is processor-specific,
      this method is purely virtual.

      doConstFolding does not actually modify the currently examined %WIR
      operation o. Instead, new instructions realizing the constant folding of o
      are added to map mNewInstructions.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool doConstFolding( const WIR_Operation &o,
                                 const std::map<WIR_id_t, WIR_UpDownValue> &outValue,
                                 const std::map<WIR_id_t, WIR_UpDownValue> &inValue ) = 0;

    /*!
      @brief replace replaces all 'X' bits in the given up/down value by the
             specified L4 value.

      @param[in] v A const reference to an up/down value.
      @param[in] b The L4 value to be used as replacement for 'X' bits.  L and N
                   must not be used here.
      @return A signed integer representing the up/down value's numerical value
              after replacement of 'X' bits.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    signed long long replace( const WIR_UpDownValue &, WIR_L4 ) const;

    /*!
      mNewInstructions maps the a constant-folded operation to a list of new
      instructions implementing the actual folding.
    */
    std::map<std::reference_wrapper<WIR_Operation>, std::list<WIR_Instruction>, WIR_Compare<WIR_Operation>> mNewInstructions;


  private:

    /*!
      @brief constFold performs the generic, processor-independent parts of
             constant folding.

      @param[in] o A const reference to a %WIR operation to be folded.
      @return A Boolean denoting whether new instructions were produced for o or
              not.

      constFold basically combines the possibly several outgoing up/down values
      per parameter into a single up/down value and calls the processor-specific
      virtual methods that check and finally realize folding.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool constFold( const WIR_Operation & );

    /*!
      @brief addNewInstructions processes map mNewInstructions and adds the
             generated constant-folded instructions immediately behind each
             original constant operation.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void addNewInstructions( void );

    /*!
      @brief eraseInstructions processes list mErasePositions and erases all
             instructions therein.

      @return A Boolean denoting whether some basic block's control flow has
              changed.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool eraseInstructions( void );

    /*!
      @brief For an operation ID, mIterator stores an iterator pointing to that
             operation's instruction.
    */
    std::map<WIR_id_t, std::list<std::reference_wrapper<WIR_Instruction>>::const_iterator> mIterator;

    /*!
      @brief mErasePositions stores iterators to WIR instructions to be erased
             after constant propagation.
    */
    std::list<std::list<std::reference_wrapper<WIR_Instruction>>::const_iterator> mErasePositions;

};

}       // namespace WIR

#endif  // _WIR_CONSTFOLD_H
