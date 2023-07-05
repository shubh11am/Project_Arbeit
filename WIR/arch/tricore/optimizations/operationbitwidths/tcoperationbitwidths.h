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
  @file tcoperationbitwidths.h
  @brief This file provides the interface of a TriCore-specific optimization
         reducing the bit widths of machine operations.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _TC_OPERATIONBITWIDTHS_H
#define _TC_OPERATIONBITWIDTHS_H


//
// Include section
//

// Include standard headers
#include <functional>
#include <list>

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
class WIR_Instruction;
class WIR_Operation;
class WIR_System;


/*!
  @brief Class TC_OperationBitWidths is a TriCore-specific optimization that
         reduces the bit widths of machine operations.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class TC_OperationBitWidths final : public WIR_Optimization
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
    explicit TC_OperationBitWidths( WIR_System & );

    /*!
      @brief Default constructor for compilation unit-level optimization.

      @param[in] c A reference to a WIR_CompilationUnit to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit TC_OperationBitWidths( WIR_CompilationUnit & );

    /*!
      @brief Default constructor for function-level optimization.

      @param[in] f A reference to a WIR_Function to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit TC_OperationBitWidths( WIR_Function & );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~TC_OperationBitWidths( void );


  protected:

    /*!
      @brief runOptimization reduces the bit widths of machine operations in the
             given system.

      @param[in] s A reference to a WIR_System to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void runOptimization( WIR_System & );

    /*!
      @brief runOptimization reduces the bit widths of machine operations in the
             given compilation unit.

      @param[in] c A reference to a WIR_CompilationUnit to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void runOptimization( WIR_CompilationUnit & );

    /*!
      @brief runOptimization reduces the bit widths of machine operations in the
             given function.

      @param[in] f A reference to a WIR_Function to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void runOptimization( WIR_Function & );


  private:

    /*!
      @brief reduce reduces the bit width of the given TriCore machine
             operation.

      @param[in,out] i A reference to a WIR_Instruction to be optimized.
      @param[in] pos An iterator pointing to a WIR_Operation inside i to be
                     optimized.

      Bit widths of branches are reduced whenever possible and without proper
      consideration of jump displacements. The correction of branches with
      incorrect displacements thus has to be done elsewhere afterwards.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<std::reference_wrapper<WIR_Operation>>::iterator reduce( WIR_Instruction &,
                                                                       std::list<std::reference_wrapper<WIR_Operation>>::const_iterator ) const;

};

}       // namespace WIR

#endif  // _TC_OPERATIONBITWIDTHS_H
