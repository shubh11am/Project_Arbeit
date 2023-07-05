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
  @file wirredundantblocks.h
  @brief This file provides the interface of an optimization merging succeeding
         redundant basic blocks.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_REDUNDANTBLOCKS_H
#define _WIR_REDUNDANTBLOCKS_H


//
// Include section
//

// Include WIR headers
#include <optimizations/generic/wiroptimization.h>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_CompilationUnit;
class WIR_Function;
class WIR_System;


/*!
  @brief Class WIR_EmptyBlocks is an optimization that merges two succeeding
         redundant basic blocks.

  Two basic blocks b1 and b2 are said to be redundant if and only if:

  - b1 has exactly one successor, namely b2, AND
  - b2 has exactly one predecessor, namely b1, AND
  - b2 is the immediate successor of b1 within the current %WIR function.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_RedundantBlocks : public WIR_Optimization
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
    explicit WIR_RedundantBlocks( WIR_System & );

    /*!
      @brief Default constructor for compilation unit-level optimization.

      @param[in] c A reference to a WIR_CompilationUnit to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_RedundantBlocks( WIR_CompilationUnit & );

    /*!
      @brief Default constructor for function-level optimization.

      @param[in] f A reference to a WIR_Function to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_RedundantBlocks( WIR_Function & );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_RedundantBlocks( void );


  protected:

    /*!
      @brief runOptimization merges redundant basic blocks in the given system.

      @param[in] s A reference to a WIR_System to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void runOptimization( WIR_System & );

    /*!
      @brief runOptimization merges redundant basic blocks in the given
             compilation unit.

      @param[in] c A reference to a WIR_CompilationUnit to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void runOptimization( WIR_CompilationUnit & );

    /*!
      @brief runOptimization merges redundant basic blocks in the given
             function.

      @param[in] f A reference to a WIR_Function to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void runOptimization( WIR_Function & );

};

}       // namespace WIR

#endif  // _WIR_REDUNDANTBLOCKS_H
