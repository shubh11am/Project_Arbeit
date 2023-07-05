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
  @file wiranalysis.h
  @brief This file provides the interface of generic %WIR code analyses.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_ANALYSIS_H
#define _WIR_ANALYSIS_H


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
  @brief Class WIR_Analysis provides the basic functionality that each %WIR
         code analysis must have.

  Analyses can be carried out at various abstraction levels, from %WIR systems
  down to individual operations, depending on which constructor is actually
  used. Analysis results are attached to %WIR objects using %WIR containers so
  that %WIR analyses do not provide explicit getter methods for retrieving
  analysis data.

  This class serves as virtual base class from which actual analyses are
  derived.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_Analysis
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor for system-level analysis.

      @param[in] o A reference to a WIR_System to be analyzed.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_Analysis( WIR_System & );

    /*!
      @brief Default constructor for compilation unit-level analysis.

      @param[in] o A reference to a WIR_CompilationUnit to be analyzed.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_Analysis( WIR_CompilationUnit & );

    /*!
      @brief Default constructor for function-level analysis.

      @param[in] o A reference to a WIR_Function to be analyzed.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_Analysis( WIR_Function & );

    /*!
      @brief Default constructor for basic block-level analysis.

      @param[in] o A reference to a WIR_BasicBlock to be analyzed.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_Analysis( WIR_BasicBlock & );

    /*!
      @brief Default constructor for instruction-level analysis.

      @param[in] o A reference to a WIR_Instruction to be analyzed.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_Analysis( WIR_Instruction & );

    /*!
      @brief Default constructor for operation-level analysis.

      @param[in] o A reference to a WIR_Operation to be analyzed.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_Analysis( WIR_Operation & );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_Analysis( void );


    //
    // Analysis management.
    //

    /*!
      @brief analyze performs an analysis.

      Depending on the value of mLevel, the corresponding runAnalysis method is
      called.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void analyze( void );


  protected:

    /*!
      @brief runAnalysis performs a system-level analysis.

      @param[in] o A reference to a WIR_System to be analyzed.

      For actual system-level analyses, this method needs to be overloaded in
      derived classes.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void runAnalysis( WIR_System & );

    /*!
      @brief runAnalysis performs a compilation unit-level analysis.

      @param[in] o A reference to a WIR_CompilationUnit to be analyzed.

      For actual compilation unit-level analyses, this method needs to be
      overloaded in derived classes.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void runAnalysis( WIR_CompilationUnit & );

    /*!
      @brief runAnalysis performs a function-level analysis.

      @param[in] o A reference to a WIR_Function to be analyzed.

      For actual function-level analyses, this method needs to be overloaded in
      derived classes.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void runAnalysis( WIR_Function & );

    /*!
      @brief runAnalysis performs a basic block-level analysis.

      @param[in] o A reference to a WIR_BasicBlock to be analyzed.

      For actual basic block-level analyses, this method needs to be overloaded
      in derived classes.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void runAnalysis( WIR_BasicBlock & );

    /*!
      @brief runAnalysis performs an instruction-level analysis.

      @param[in] o A reference to a WIR_Instruction to be analyzed.

      For actual instruction-level analyses, this method needs to be overloaded
      in derived classes.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void runAnalysis( WIR_Instruction & );

    /*!
      @brief runAnalysis performs an operation-level analysis.

      @param[in] o A reference to a WIR_Operation to be analyzed.

      For actual operation-level analyses, this method needs to be overloaded in
      derived classes.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void runAnalysis( WIR_Operation & );


  private:

    /*!
      @brief No standard construction allowed, users must use one of the above
             constructors instead.
    */
    WIR_Analysis( void ) = delete;

    /*!
      @brief No copy construction allowed, users must use one of the above
             constructors instead.
    */
    WIR_Analysis( const WIR_Analysis & ) = delete;

    /*!
      @brief No move construction allowed, users must use one of the above
             constructors instead.
    */
    WIR_Analysis( WIR_Analysis && ) = delete;

    /*!
      @brief This enum represents the abstraction level of a %WIR analysis.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    enum class WIR_AnalysisLevel : char
    {
      //! System-level analysis.
      sys,

      //! Compilation unit-level analysis.
      cu,

      //! Function-level analysis.
      fct,

      //! Basic block-level analysis.
      bb,

      //! Instruction-level analysis.
      ins,

      //! Operation-level analysis.
      op
    };

    //! mLevel stores an analysis' abstraction level.
    WIR_AnalysisLevel mLevel;

    //! mID refers to the %WIR object to be analyzed.
    WIR_ID_API &mID;

};

}       // namespace WIR

#endif  // _WIR_ANALYSIS_H
