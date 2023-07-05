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
  @file tcasmparser.h
  @brief This file provides the interface of a TriCore assembly code parser.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _TC_ASMPARSER_H
#define _TC_ASMPARSER_H


//
// Include section
//

// Include standard headers
#include <functional>
#include <istream>
#include <list>
#include <memory>
#include <string>
#include <vector>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_BasicBlock;

class TC_AsmArgument;
class TC_AsmContext;
class TC_AsmLex;


/*!
  @brief Class TC_AsmParser is a TriCore assembly code parser that creates %WIR
         code from a given stream or string.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class TC_AsmParser
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor creating a TriCore assembly code parser.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC_AsmParser( void );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ~TC_AsmParser( void );


    //
    // Debug output management.
    //

    /*!
      @brief setDebugScanner (de-) activates debug output of the scanner.

      @param[in] d A Boolean specifying whether debug output of the scanner
                   shall be activated or not.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setDebugScanner( bool = true );

    /*!
      @brief setDebugParser (de-) activates debug output of the parser.

      @param[in] d A Boolean specifying whether debug output of the parser shall
                   be activated or not.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setDebugParser( bool = true );


    //
    // Parser management.
    //

    /*!
      @brief run parses the assembly code given in a string.

      @param[in] s A const reference to a string containing the assembly code to
                   be parsed as GNU inline assembly-compatible manner.
      @param[in] tArgs A const reference to a vector of pointers to template
                       assembly arguments according to the GNU inline assembly
                       semantics.
      @param[in,out] b A reference to the basic block to which the generated
                       TriCore code will be appended. If necessary, more basic
                       blocks will be created by the parser and will be appended
                       after b.
      @param[in] sName A const reference to a string holding the input stream's
                       name for error messages.
      @return A list of basic blocks carrying the generated TriCore code with b
              being the list head.

      The parser requires that b is already be inserted into a %WIR function and
      that this %WIR function in turn is properly inserted into a compilation
      unit and a %WIR system.

      If the parser is supposed not to generate 16 bits wide operations, but the
      input to be parsed actually includes such an operation, the parser emits a
      corresponding warning.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const std::list<std::reference_wrapper<WIR_BasicBlock>> &run( const std::string &,
                                                                  const std::vector<std::unique_ptr<TC_AsmArgument>> &,
                                                                  WIR_BasicBlock &,
                                                                  const std::string & );

    /*!
      @brief run parses the assembly code given in an input stream.

      @param[in] s A reference to an input stream containing the assembly code
                   to be parsed as GNU inline assembly-compatible manner.
      @param[in] tArgs A const reference to a vector of pointers to template
                       assembly arguments according to the GNU inline assembly
                       semantics.
      @param[in,out] b A reference to the basic block to which the generated
                       TriCore code will be appended. If necessary, more basic
                       blocks will be created by the parser and will be appended
                       after b.
      @param[in] sName A const reference to a string holding the input stream's
                       name for error messages.
      @return A list of basic blocks carrying the generated TriCore code with b
              being the list head.

      The parser requires that b is already be inserted into a %WIR function and
      that this %WIR function in turn is properly inserted into a compilation
      unit and a %WIR system.

      If the parser is supposed not to generate 16 bits wide operations, but the
      input to be parsed actually includes such an operation, the parser emits a
      corresponding warning.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const std::list<std::reference_wrapper<WIR_BasicBlock>> &run( std::istream &,
                                                                  const std::vector<std::unique_ptr<TC_AsmArgument>> &,
                                                                  WIR_BasicBlock &,
                                                                  const std::string & );

    /*!
      @brief setGenerate16BitOperations sets whether the parser shall generate
             16 bits wide operations or not.

      @param[in] f A Boolean flag defaulting to true that denotes whether 16
                   bits wide operations shall be generated or not.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setGenerate16BitOperations( bool = true );

    /*!
      @brief getGenerate16BitOperations returns whether the parser shall
             generate 16 bits wide operations or not.

      @return true if 16 bits wide operations shall be generated, false
              otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool getGenerate16BitOperations( void ) const;


  private:

    friend class TC_AsmYacc;

    /*!
      @brief No copy construction allowed, users must use one of the above
             constructors instead.
    */
    TC_AsmParser( const TC_AsmParser & ) = delete;

    /*!
      @brief No move construction allowed, users must use one of the above
             constructors instead.
    */
    TC_AsmParser( TC_AsmParser && ) = delete;

    /*!
      @brief resolveStringLabels resolves all string label parameters attached
             to %WIR operations by proper label parameters.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void resolveStringLabels( void );

    //! mContext holds a (smart) pointer to the parser's internal context.
    std::unique_ptr<TC_AsmContext> mContext;

    /*!
      @brief mDebugScanner stores whether debug output of the scanner shall be
             enabled.
    */
    bool mDebugScanner;

    /*!
      @brief mDebugParser stores whether debug output of the parser shall be
             enabled.
    */
    bool mDebugParser;

    /*!
      @brief mScanner points to the current scanner instance.

      This is required to connect both parser and scanner, cf. declaration of
      macro yylex in the parser's grammar.
    */
    TC_AsmLex *mScanner;

    //! mStreamName holds the input stream's name for error messages.
    std::string mStreamName;

    /*!
      @brief m16BitOperations stores whether the parser shall generate 16 bits
             wide operations or not.
    */
    bool m16BitOperations;

};

}       // namespace WIR

#endif  // _TC_ASMPARSER_H
