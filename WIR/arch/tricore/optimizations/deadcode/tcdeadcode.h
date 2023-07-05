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
  @file tcdeadcode.h
  @brief This file provides the interface of a TriCore-specific dead code
         elimination.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _TC_DEADCODE_H
#define _TC_DEADCODE_H


//
// Include section
//

// Include WIR headers
#include <optimizations/deadcode/wirdeadcode.h>


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
  @brief Class TC_DeadCode is a TriCore-specific optimization that eliminates
         dead code in %WIR functions.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class TC_DeadCode final : public WIR_DeadCode
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
    explicit TC_DeadCode( WIR_System & );

    /*!
      @brief Default constructor for compilation unit-level optimization.

      @param[in] c A reference to a WIR_CompilationUnit to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit TC_DeadCode( WIR_CompilationUnit & );

    /*!
      @brief Default constructor for function-level optimization.

      @param[in] f A reference to a WIR_Function to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit TC_DeadCode( WIR_Function & );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~TC_DeadCode( void );


  protected:

    /*!
      @brief runOptimization eliminates dead code in the given function.

      @param[in] f A reference to a WIR_Function to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void runOptimization( WIR_Function & );

};

}       // namespace WIR

#endif  // _TC_DEADCODE_H
