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
  @file wirdeadcode.h
  @brief This file provides the interface of an optimization eliminating dead
         code.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_DEADCODE_H
#define _WIR_DEADCODE_H


//
// Include section
//

// Include WIR headers
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
class WIR_Operation;
class WIR_System;


/*!
  @brief Class WIR_DeadCode is an optimization that eliminates dead code from
         %WIR functions.

  A %WIR operation is considered dead if it computes only values that are not
  used on any executable path leading from the operation (cf. S. S. Muchnick,
  "Advanced Compiler Design & Implementation", page 592).

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_DeadCode : public WIR_BitOpt
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
    explicit WIR_DeadCode( WIR_System & );

    /*!
      @brief Default constructor for compilation unit-level optimization.

      @param[in] c A reference to a WIR_CompilationUnit to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_DeadCode( WIR_CompilationUnit & );

    /*!
      @brief Default constructor for function-level optimization.

      @param[in] f A reference to a WIR_Function to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_DeadCode( WIR_Function & );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_DeadCode( void );


  protected:

    /*!
      @brief runOptimization eliminates dead code in the given system.

      @param[in] s A reference to a WIR_System to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void runOptimization( WIR_System & );

    /*!
      @brief runOptimization eliminates dead code in the given compilation unit.

      @param[in] c A reference to a WIR_CompilationUnit to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void runOptimization( WIR_CompilationUnit & );

    /*!
      @brief runOptimization eliminates dead code in the given function.

      @param[in] f A reference to a WIR_Function to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void runOptimization( WIR_Function & );

    /*!
      @brief isDead determines whether a %WIR operation is dead or not.

      @param[in] o A const reference to an operation to be checked as dead code.
      @return true if o is dead, false otherwise.

      isDead is a virtual method in order to allow overloading in derived
      processor-specific classes for particular dead code checks, if required.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool isDead( const WIR_Operation & ) const;


  private:

    /*!
      @brief eliminateDefUses eliminates def-use relationships of all those DFG
             edges where the up-values only contain 'X*'.

      @param[in] f A reference to a WIR_Function where to eliminate dead def-use
                   relationships.

      For edges containing 'X*' only, the contents of the data flowing along
      that edge is completely irrelevant. De facto, no data actually flows along
      such an edge.

      After bit-true data flow analysis, the def-use relationships along DFG
      edges are modeled using bitValue containers attached to register
      parameters. Thus, in order to eliminate a dead def-use relationship, the
      contents of the involved bitValue containers is modified by
      eliminateDefUses.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void eliminateDefUses( WIR_Function & ) const;

    /*!
      @brief eliminateDeadOperations eliminates dead operations from a %WIR
             function.

      @param[in] f A reference to a WIR_Function where to eliminate dead
                   operations.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void eliminateDeadOperations( WIR_Function & ) const;

};

}       // namespace WIR

#endif  // _WIR_DEADCODE_H
