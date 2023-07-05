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
  @file wiroptimization.h
  @brief This file provides the interface of generic %WIR code optimizations.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_OPTIMIZATION_H
#define _WIR_OPTIMIZATION_H


//
// Include section
//

// Include WIR headers
#include <wir/wirtypes.h>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_BasicBlock;
class WIR_CompilationUnit;
class WIR_Function;
class WIR_ID_API;
class WIR_Instruction;
class WIR_Operation;
class WIR_System;


/*!
  @brief Class WIR_Optimization provides the basic functionality that each %WIR
         code optimization must have.

  Optimizations can be carried out at various abstraction levels, from %WIR
  systems down to individual operations, depending on which constructor is
  actually used.

  This class serves as virtual base class from which actual optimizations are
  derived. By default, optimizations are enabled to generate 16 bits wide
  operations.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_Optimization
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor for system-level optimization.

      @param[in,out] s A reference to a WIR_System to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_Optimization( WIR_System & );

    /*!
      @brief Default constructor for compilation unit-level optimization.

      @param[in,out] c A reference to a WIR_CompilationUnit to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_Optimization( WIR_CompilationUnit & );

    /*!
      @brief Default constructor for function-level optimization.

      @param[in,out] f A reference to a WIR_Function to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_Optimization( WIR_Function & );

    /*!
      @brief Default constructor for basic block-level optimization.

      @param[in,out] b A reference to a WIR_BasicBlock to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_Optimization( WIR_BasicBlock & );

    /*!
      @brief Default constructor for instruction-level optimization.

      @param[in,out] i A reference to a WIR_Instruction to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_Optimization( WIR_Instruction & );

    /*!
      @brief Default constructor for operation-level optimization.

      @param[in,out] o A reference to a WIR_Operation to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_Optimization( WIR_Operation & );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_Optimization( void );


    //
    // Optimization management.
    //

    /*!
      @brief optimize performs an optimization.

      Depending on the value of mLevel, the corresponding runOptimization method
      is called.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void optimize( void );

    /*!
      @brief setGenerate16BitOperations sets whether an optimization shall
             generate 16 bits wide operations or not.

      @param[in] f A Boolean flag defaulting to true that denotes whether 16
                   bits wide operations shall be generated or not.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setGenerate16BitOperations( bool = true );

    /*!
      @brief getGenerate16BitOperations returns whether an optimization shall
             generate 16 bits wide operations or not.

      @return true if 16 bits wide operations shall be generated, false
              otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool getGenerate16BitOperations( void ) const;


  protected:

    /*!
      @brief runOptimization performs a system-level optimization.

      @param[in] s A reference to a WIR_System to be optimized.

      For actual system-level optimizations, this method needs to be overloaded
      in derived classes.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void runOptimization( WIR_System & );

    /*!
      @brief runOptimization performs a compilation unit-level optimization.

      @param[in] c A reference to a WIR_CompilationUnit to be optimized.

      For actual compilation unit-level optimizations, this method needs to be
      overloaded in derived classes.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void runOptimization( WIR_CompilationUnit & );

    /*!
      @brief runOptimization performs a function-level optimization.

      @param[in] f A reference to a WIR_Function to be optimized.

      For actual function-level optimizations, this method needs to be
      overloaded in derived classes.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void runOptimization( WIR_Function & );

    /*!
      @brief runOptimization performs a basic block-level optimization.

      @param[in] b A reference to a WIR_BasicBlock to be optimized.

      For actual basic block-level optimizations, this method needs to be
      overloaded in derived classes.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void runOptimization( WIR_BasicBlock & );

    /*!
      @brief runOptimization performs an instruction-level optimization.

      @param[in] i A reference to a WIR_Instruction to be optimized.

      For actual instruction-level optimizations, this method needs to be
      overloaded in derived classes.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void runOptimization( WIR_Instruction & );

    /*!
      @brief runOptimization performs an operation-level optimization.

      @param[in] o A reference to a WIR_Operation to be optimized.

      For actual operation-level optimizations, this method needs to be
      overloaded in derived classes.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void runOptimization( WIR_Operation & );

    /*!
      @brief This enum represents the abstraction level of a %WIR optimization.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    enum class WIR_OptimizationLevel : char
    {
      //! System-level optimization.
      sys,

      //! Compilation unit-level optimization.
      cu,

      //! Function-level optimization.
      fct,

      //! Basic block-level optimization.
      bb,

      //! Instruction-level optimization.
      ins,

      //! Operation-level optimization.
      op
    };


    //
    // Handling of containers
    //

    /*!
      @brief copyContainers copies any kind of %WIR containers from an
             original instruction to a new instruction stemming from some
             optimization.

      @param[in,out] i A reference to a new instruction.
      @param[in] iOrig A const reference to an original instruction.
      @param[in] op A Boolean flag defaulting to true that specifies whether
                    recursively all containers attached to the operations inside
                    iOrig shall also be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void copyContainers( WIR_Instruction &, const WIR_Instruction &,
                         bool = true ) const;

    /*!
      @brief copyContainers copies any kind of %WIR containers from an
             original operation to a new operation stemming from some
             optimization.

      @param[in,out] o A reference to a new operation.
      @param[in] oOrig A const reference to an original operation.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void copyContainers( WIR_Operation &, const WIR_Operation & ) const;

    //! mLevel stores an optimization's abstraction level.
    WIR_OptimizationLevel mLevel;

    /*!
      @brief m16BitOperations stores whether an optimization shall generate 16
             bits wide operations or not.
    */
    bool m16BitOperations;


  private:

    /*!
      @brief No standard construction allowed, users must use one of the above
             constructors instead.
    */
    WIR_Optimization( void ) = delete;

    /*!
      @brief No copy construction allowed, users must use one of the above
             constructors instead.
    */
    WIR_Optimization( const WIR_Optimization & ) = delete;

    /*!
      @brief No move construction allowed, users must use one of the above
             constructors instead.
    */
    WIR_Optimization( WIR_Optimization && ) = delete;

    //! mID refers to the %WIR object to be analyzed.
    WIR_ID_API &mID;

};

}       // namespace WIR

#endif  // _WIR_OPTIMIZATION_H
