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
  @file tcconstfold.h
  @brief This file provides the interface of a TriCore-specific constant folding
         optimization.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _TC_CONSTFOLD_H
#define _TC_CONSTFOLD_H


//
// Include section
//

// Include standard headers
#include <list>
#include <map>
#include <utility>

// Include WIR headers
#include <wir/wirtypes.h>
#include <optimizations/constfold/wirconstfold.h>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_CompilationUnit;
class WIR_Function;
class WIR_Instruction;
class WIR_Operation;
class WIR_RegisterParameter;
class WIR_System;
class WIR_UpDownValue;


/*!
  @brief Class TC_ConstFold is a TriCore-specific optimization that folds
         constants in %WIR functions.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class TC_ConstFold final : public WIR_ConstFold
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
    explicit TC_ConstFold( WIR_System & );

    /*!
      @brief Default constructor for compilation unit-level optimization.

      @param[in] c A reference to a WIR_CompilationUnit to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit TC_ConstFold( WIR_CompilationUnit & );

    /*!
      @brief Default constructor for function-level optimization.

      @param[in] f A reference to a WIR_Function to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit TC_ConstFold( WIR_Function & );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~TC_ConstFold( void );


  protected:

    /*!
      @brief runOptimization folds constants in the given function.

      @param[in] f A reference to a WIR_Function to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void runOptimization( WIR_Function & );

    /*!
      @brief For an operation identified to be constant, doConstFolding does the
             actual TriCore-specific folding.

      @param[in] o A const reference to a %WIR operation identified as constant.
      @param[in] outValue A const reference to a map mapping all defined or
                          def-used register parameters to their outgoing bit
                          value.
      @param[in] inValue A const reference to a map mapping all used or def-used
                         register parameters to their incoming bit value.
      @return A Boolean denoting whether new instructions were produced for o or
              not.

      doConstFolding does not actually modify the currently examined %WIR
      operation o. Instead, new instructions realizing the constant folding of o
      are added to map mNewInstructions.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool doConstFolding( const WIR_Operation &,
                                 const std::map<WIR_id_t, WIR_UpDownValue> &,
                                 const std::map<WIR_id_t, WIR_UpDownValue> & );


  private:

    /*!
      @brief For an operation identified to be constant, doBranchFolding
             performs TriCore-specific folding of conditional branches.

      @param[in] o A const reference to a %WIR operation identified as constant.
      @param[in] inValue A const reference to a map mapping all used or def-used
                         register parameters to their incoming bit value.
      @return A Boolean denoting whether new instructions were produced for o or
              not.

      If the result of jump condition is statically known, the conditional
      branch gets folded either to an unconditional branch or gets removed
      completely.

      doBranchFolding does not actually modify the currently examined %WIR
      operation o. Instead, new instructions realizing the constant folding of o
      are added to map mNewInstructions.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool doBranchFolding( const WIR_Operation &,
                          const std::map<WIR_id_t, WIR_UpDownValue> & );


    /*!
      @brief For an operation identified to be constant, doMOVFolding performs
             TriCore-specific folding into register MOV operations.

      @param[in] o A const reference to a %WIR operation identified as constant.
      @param[in] inValue A const reference to a map mapping all used or def-used
                         register parameters to their incoming bit value.
      @return A Boolean denoting whether new instructions were produced for o or
              not.

      If the computations of an operation are statically known to have no
      arithmetical effect, doMOVFolding folds the operation into a register MOV.

      doMOVFolding does not actually modify the currently examined %WIR
      operation o. Instead, new instructions realizing the constant folding of o
      are added to map mNewInstructions.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool doMOVFolding( const WIR_Operation &,
                       const std::map<WIR_id_t, WIR_UpDownValue> & );

    /*!
      @brief getLEA generates TriCore instructions loading an address register
             with a constant.

      @param[in] p A const reference to a register parameter whose address
                   register shall be loaded with a constant value.
      @param[in] v A const reference to an up value to be loaded into the
                   register.
      @return A list of TriCore instructions implementing the desired action.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<WIR_Instruction> getLEA( const WIR_Parameter &,
                                       const WIR_UpDownValue & ) const;

    /*!
      @brief getLEA_P generates TriCore instructions loading an extended address
             register with a constant.

      @param[in] p A const reference to a register parameter whose extended
                   address register shall be loaded with a constant value.
      @param[in] v A const reference to an up value to be loaded into the
                   register.
      @return A list of TriCore instructions implementing the desired action.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<WIR_Instruction> getLEA_P( const WIR_Parameter &,
                                         const WIR_UpDownValue & ) const;

    /*!
      @brief getMOV generates TriCore instructions loading a data register with
             a constant.

      @param[in] p A const reference to a register parameter whose data register
                   shall be loaded with a constant value.
      @param[in] v A const reference to an up value to be loaded into the
                   register.
      @return A list of TriCore instructions implementing the desired action.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<WIR_Instruction> getMOV( const WIR_Parameter &,
                                       const WIR_UpDownValue & ) const;

    /*!
      @brief getMOV generates TriCore instructions performing a
             register-register MOV.

      @param[in] t A const reference to a register parameter being the MOV's
                   target.
      @param[in] s A const reference to a register parameter being the MOV's
                   source.
      @return A list of TriCore instructions implementing the desired action.

      Depending on whether t is a data or an address register, getMOV generates
      either a MOV_RR or a MOV_AA operation.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<WIR_Instruction> getMOV( const WIR_RegisterParameter &,
                                       const WIR_RegisterParameter & );

    /*!
      @brief getMOV_E generates TriCore instructions loading an extended data
             register with a constant.

      @param[in] p A const reference to a register parameter whose extended data
                   register shall be loaded with a constant value.
      @param[in] v A const reference to an up value to be loaded into the
                   register.
      @return A list of TriCore instructions implementing the desired action.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<WIR_Instruction> getMOV_E( const WIR_Parameter &,
                                         const WIR_UpDownValue & ) const;

    /*!
      @brief getADD generates TriCore instructions incrementing a register by
             the given constant.

      @param[in] p A const reference to a register parameter whose data register
                   shall be incremented.
      @param[in] v A signed integer denoting the increment value.
      @return A list of TriCore instructions implementing the desired action.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<WIR_Instruction> getADD( const WIR_Parameter &, int );

    /*!
      @brief getADDSUB generates TriCore ADD or SUB instructions for a given
             original ADDX/SUBX instruction.

      @param[in] o A const reference to an original ADDX/SUBX operation.
      @return A list of TriCore instructions implementing the desired action.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<WIR_Instruction> getADDSUB( const WIR_Operation & );

    /*!
      @brief getJ generates TriCore instructions unconditionally jumping to the
             given label.

      @param[in] p A const reference to a label parameter that denotes the jump
                   target.
      @return A list of TriCore instructions implementing the desired action.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<WIR_Instruction> getJ( const WIR_LabelParameter & ) const;

    /*!
      @brief insertLEA generates the actual TriCore instructions to load an
             address register with a constant.

      @param[in] r A const reference to the address register to be loaded.
      @param[in] v A const reference to an up value to be loaded into the
                   register.
      @param[in,out] l A reference to a list of TriCore instructions
                       implementing the desired constant LEA.
      @return A reference to a defined register parameter that finally holds the
              correctly loaded data register.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_RegisterParameter &insertLEA( const WIR_BaseRegister &,
                                      const WIR_UpDownValue &,
                                      std::list<WIR_Instruction> & ) const;

    /*!
      @brief insertMOV generates the actual TriCore instructions to load a data
             register with a constant.

      @param[in] r A const reference to the data register to be loaded.
      @param[in] v A const reference to an up value to be loaded into the
                   register.
      @param[in,out] l A reference to a list of TriCore instructions
                       implementing the desired constant MOV.
      @return A reference to a defined register parameter that finally holds the
              correctly loaded data register.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_RegisterParameter &insertMOV( const WIR_BaseRegister &,
                                      const WIR_UpDownValue &,
                                      std::list<WIR_Instruction> & ) const;

    /*!
      @brief For a given extended register, getChilds returns the two child
             registers.

      @param[in] r A const reference to an extended TriCore register which can
                   be either an extended data or an extended address register.
      @return A tuple with pointers to the child registers. The tuple's first
              element denotes the least-significant child of the extended
              register.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::pair<WIR_BaseRegister *, WIR_BaseRegister *> getChilds( const WIR_BaseRegister & ) const;

    /*!
      @brief areChildsUsed determines whether the two child registers of an
             extended register are actually used according to the data flow
             related to the given %WIR parameter.

      @param[in] p A const reference to the %WIR register parameter defining an
                   extended register.
      @param[in] c1 A const reference to the extended register's
                    least-significant child.
      @param[in] c2 A const reference to the extended register's
                    most-significant child.
      @return A tuple of Booleans indicating whether first or second child are
              used, resp.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::pair<bool, bool> areChildsUsed( const WIR_Parameter &,
                                         const WIR_BaseRegister &,
                                         const WIR_BaseRegister & ) const;

};

}       // namespace WIR

#endif  // _TC_CONSTFOLD_H
