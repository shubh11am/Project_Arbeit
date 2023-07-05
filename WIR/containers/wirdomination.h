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
  @file wirdomination.h
  @brief This file provides the interface of a %WIR container representing sets
         of dominating basic blocks or instructions that are available at
         other blocks or instructions, resp.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_DOMINATION_H
#define _WIR_DOMINATION_H


//
// Include section
//

// Include WIR headers
#include <wir/wircontainer.h>
#include <wir/wirtypes.h>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_BasicBlock;
class WIR_Instruction;


/*!
  @brief Class WIR_Domination models sets of dominating basic blocks and
         instructions that are available at other basic blocks or instructions,
         resp.

  WIR_Domination containers are attached to either WIR_BasicBlock only or to
  WIR_BasicBlock and WIR_Instruction objects during domination analysis,
  depending on whether the analysis is carried out at block-level only, or at
  both block- and instruction-level.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_Domination : public WIR_Container<WIR_Domination>
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Domination( void );

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Domination( const WIR_Domination &__o ) = default;

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_Domination( void );

    /*!
      @brief isUnique returns whether domination sets are unique, i.e., whether
             at most one instance of this container type can be attached to a
             %WIR class.

      @return Always true, domination sets are unique.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool isUnique( void ) const;


    //
    // Domination set handling.
    //

    /*!
      @brief insertDominator adds a new dominator block to set mDominatorBlocks.

      @param[in] b A const reference to the dominating block to be added.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertDominator( const WIR_BasicBlock & );

    /*!
      @brief insertDominator adds a new dominator instruction to set
             mDominatorInstructions.

      @param[in] i A const reference to the dominating instruction to be added.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertDominator( const WIR_Instruction & );

    /*!
      @brief getDominatorBlocks returns the set mDominatorBlocks.

      @return A const reference to the set mDominatorBlocks.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const WIR_BasicBlockSet &getDominatorBlocks( void ) const;

    /*!
      @brief getDominatorInstructions returns the set mDominatorInstructions.

      @return A const reference to the set mDominatorInstructions.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const WIR_InstructionSet &getDominatorInstructions( void ) const;


  private:

    //! mDominatorBlocks holds the set of dominating basic blocks.
    WIR_BasicBlockSet mDominatorBlocks;

    //! mDominatorInstructions holds the set of dominating instructions.
    WIR_InstructionSet mDominatorInstructions;

};

}       // namespace WIR

#endif  // _WIR_DOMINATION_H
