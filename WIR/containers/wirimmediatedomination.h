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
  @file wirimmediatedomination.h
  @brief This file provides the interface of a %WIR container representing
         immediate dominators of basic blocks.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_IMMEDIATEDOMINATION_H
#define _WIR_IMMEDIATEDOMINATION_H


//
// Include section
//

// Include WIR headers
#include <wir/wircontainer.h>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_BasicBlock;


/*!
  @brief Class WIR_ImmediateDomination models immediate dominators of basic
         blocks.

  WIR_ImmediateDomination containers are attached to %WIR basic blocks during
  immediate domination analysis. However, only basic blocks that are reachable
  from a function's entry point get an immediate domination container attached,
  unreachable blocks will never carry this container.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_ImmediateDomination : public WIR_Container<WIR_ImmediateDomination>
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_ImmediateDomination( void );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_ImmediateDomination( void );

    /*!
      @brief isUnique returns whether immediate dominators are unique, i.e.,
             whether at most one instance of this container type can be attached
             to a %WIR class.

      @return Always true, immediate dominators are unique.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool isUnique( void ) const;


    //
    // Immediate dominator handling.
    //

    /*!
      @brief insertImmediateDominator adds a new immediate dominator.

      @param[in] b A reference to the immediate dominator block to be added.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertImmediateDominator( WIR_BasicBlock & );

    /*!
      @brief getImmediateDominator returns the immediate dominator block.

      @return A reference to the block to that mImmediateDominator points.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_BasicBlock &getImmediateDominator( void ) const;


  private:

    //! mImmediateDominator points to the immediate dominator block.
    WIR_BasicBlock *mImmediateDominator;

};

}       // namespace WIR

#endif  // _WIR_IMMEDIATEDOMINATION_H
