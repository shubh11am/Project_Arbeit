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
  @file tcasmcontext.h
  @brief This file provides the interface of a container storing data that is
         internally used by the TriCore assembly code parser.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _TC_ASMCONTEXT_H
#define _TC_ASMCONTEXT_H


//
// Include section
//

// Include standard headers
#include <functional>
#include <list>
#include <map>
#include <memory>
#include <set>
#include <string>
#include <vector>

// Include WIR headers
#include <wir/wirbaseprocessor.h>
#include <wir/wirtypes.h>

// Include local headers
#include "tcasmargument.h"
#include <arch/tricore/asmparser/location.hh>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_BasicBlock;
class WIR_Function;
class WIR_Operation;

class TC13;
class TC_ARegP;
class TC_DRegP;
class TC_ERegP;
class TC_AsmParser;
class TC_AsmTemplateRegister;
struct MnemonicData;


/*!
  @brief Class TC_AsmContext is a container for relevant data used internally by
         the TriCore assembly code parser.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class TC_AsmContext
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ~TC_AsmContext( void );


    //
    // Parser context management.
    //

    /*!
      @brief getTemplateArgument returns a copy of the ith template assembly
             argument.

      @param[in] i The index of the template argument to be looked up.
      @return A pointer to the clone of the specified template assembly
              argument.

      The caller of getTemplateArgument is responsible for the proper deletion
      of the returned object.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC_AsmArgument *getTemplateArgument( unsigned int ) const;

    /*!
      @brief hasTemplateArguments returns whether template assembly arguments
             have to be considered while parsing.

      @return true if there are template arguments, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool hasTemplateArguments( void ) const;

    /*!
      @brief pushBackOperation appends a new operation to be generated.

      @param[in] m A const reference to a struct describing opcode and bit width
                   of the new operation.
      @param[in] a A const reference to a vector of operation arguments.
      @return true if the creation of a %WIR operation was successful, false
              otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool pushBackOperation( const MnemonicData &,
                            const std::vector<TC_AsmArgument *> & );

    /*!
      @brief insertBasicBlock inserts a new basic block and associates it with
             the given label name.

      @param[in] n A const reference to a string holding the label's name.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertBasicBlock( const std::string & );

    /*!
      @brief getProcessor returns the current TriCore processor.

      @return A reference to the TriCore processor currently manipulated by the
              parser.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC13 &getProcessor( void ) const;

    /*!
      @brief getBasicBlocks returns the list of basic blocks modified/created by
             the parser.

      @return A const reference to a list of (wrapped) references to %WIR
              basic blocks.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const std::list<std::reference_wrapper<WIR_BasicBlock>> &getBasicBlocks( void ) const;

    /*!
      @brief getARegP determines the physical address register with the
             specified number.

      @param[in] n An integer in the interval [0, 15] denoting the index of the
                   required address register.
      @return A const reference to the n-th physical address register.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const TC_ARegP &getARegP( int ) const;

    /*!
      @brief getDRegP determines the physical data register with the specified
             number.

      @param[in] n An integer in the interval [0, 15] denoting the index of the
                   required data register.
      @return A const reference to the n-th physical data register.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const TC_DRegP &getDRegP( int ) const;

    /*!
      @brief getERegP determines the physical extended data register with the
             specified number.

      @param[in] n An even integer in the interval [0, 15] denoting the index of
                   the required extended data register.
      @return A const reference to the n-th physical extended data register.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const TC_ERegP &getERegP( int ) const;

    /*!
      @brief getPRegP determines the physical extended address register with the
             specified number.

      @param[in] n An even integer in the interval [0, 15] denoting the index of
                   the required extended address register.
      @return A const reference to the n-th physical extended address register.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const TC_PRegP &getPRegP( int ) const;

    /*!
      @brief getPSW_C determines the physical carry bit of the Processor Status
             word.

      @return A const reference to the TriCore's physical PSW.C bit.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const TC_PSWBit &getPSW_C( void ) const;

    /*!
      @brief setError sets a new error message along with its location.

      @param[in] l A const reference to the error's location.
      @param[in] m A const reference to the error message's string.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setError( const location &, const std::string & );

    /*!
      @brief getErrMessage returns the current error message.

      @return A string containing the error message.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::string getErrMessage( void ) const;

    /*!
      @brief getErrLocation returns the current error location.

      @return An error location.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    location getErrLocation( void ) const;

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

    friend class TC_AsmParser;

    /*!
      @brief No standard construction allowed, users must use
             TC_AsmContext( const list<unique_ptr<TC_AsmArgument>> &,
                            WIR_BasicBlock & )
             instead.
    */
    TC_AsmContext( void ) = delete;

    /*!
      @brief Default constructor creating a container for the specified assembly
             template arguments and basic block.

      @param[in] tArgs A const reference to a vector of pointers to assembly
                       template arguments according to the GNU inline assembly
                       semantics.
      @param[in,out] b A reference to the basic block to which the generated
                       TriCore code will be appended.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC_AsmContext( const std::vector<std::unique_ptr<TC_AsmArgument>> &,
                   WIR_BasicBlock & );

    /*!
      @brief check16BitOperation checks whether an operation is 16 bits wide and
             whether such operations shall actually be generated.

      @param[in] o A const reference to a %WIR operation to be checked.

      If the parser is about to actually generate a 16 bits wide operation while
      this is not wanted, a warning is emitted.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void check16BitOperation( const WIR_Operation & ) const;

    /*!
      @brief mTemplateArguments holds a reference to template assembly arguments
             according to the GNU inline assembly semantics.
    */
    const std::vector<std::unique_ptr<TC_AsmArgument>> &mTemplateArguments;

    //! mIt denotes the position before which new basic blocks will be inserted.
    std::list<std::reference_wrapper<WIR_BasicBlock>>::const_iterator mIt;

    //! mBasicBlocks stores all basic blocks modified/created by the parser.
    std::list<std::reference_wrapper<WIR_BasicBlock>> mBasicBlocks;

    //! mBlockOfLabel maps a label name to its associated %WIR basic block.
    std::map<std::string, std::reference_wrapper<WIR_BasicBlock>> mBlockOfLabel;

    /*!
      @brief mFunction refers to the %WIR function holding all the basic blocks
             modified/created by the parser.
    */
    WIR_Function &mFunction;

    /*!
      @brief mProcessor refers to the TriCore processor for which %WIR code is
             generated by the parser.
    */
    TC13 &mProcessor;

    //! mErrMessage stores a generated error message.
    std::string mErrMessage;

    //! mErrLocation stores a locations of an error message.
    location mErrLocation;

    /*!
      @brief mOperationsToBeResolved contains all newly generated %WIR
             operations where string labels need to be resolved by actual label
             parameters.
    */
    WIR_OperationSet mOperationsToBeResolved;

    /*!
      @brief m16BitOperations stores whether the parser shall generate 16 bits
             wide operations or not.
    */
    bool m16BitOperations;

};

}       // namespace WIR

#endif  // _TC_ASMCONTEXT_H
