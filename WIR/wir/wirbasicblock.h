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
  @file wirbasicblock.h
  @brief This file provides the interface of %WIR basic blocks.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_BASICBLOCK_H
#define _WIR_BASICBLOCK_H


//
// Include section
//

// Include standard headers
#include <string>
#include <utility>

// Include WIR headers
#include <wir/API/wircontainerapi.h>
#include <wir/API/wiridapi.h>
#include <wir/API/wirinsertionapi.h>
#include <wir/API/wirlistapi.h>
#include <wir/wirtypes.h>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_Function;
class WIR_Instruction;


/*!
  @brief Class WIR_BasicBlock is the generic representation of basic code
         blocks.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_BasicBlock : public WIR_ID_API,
                       public WIR_Container_API
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor creating an empty basic block.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_BasicBlock( void );

    /*!
      @brief Default constructor creating a basic block containing the given
             instructions.

      @tparam Args R-value references of class WIR_Instruction.
      @param[in] i An R-value reference to a %WIR instruction.
      @param[in] __args A variadic number of R-value references to %WIR
                        instructions to be inserted in this basic block.

      This constructor uses variadic templates so that argument lists of
      arbitrary lengths can be passed to it.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    template<typename... Args>
    WIR_BasicBlock( WIR_Instruction &&i, Args&&... __args ) :
      WIR_ID_API {},
      WIR_Container_API {},
      mFunctionPointer { nullptr },
      mDontOptimize { false },
      mDirectSuccessor { nullptr }
    {
      DSTART( "WIR_BasicBlock::WIR_BasicBlock(WIR_Instruction&&, Args&& ...)" );

      pushBackInstruction( std::move( i ) );
      addInstructions( std::forward<Args>( __args )... );
    };

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      When copying a basic block that is inserted in some %WIR function, the
      resulting copy will not be inserted in a function.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_BasicBlock( const WIR_BasicBlock & );

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      Trying to move a basic block that is inserted in some %WIR function
      results in an assertion, since you are not allowed to move a basic block
      whose ownership is managed by a function.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_BasicBlock( WIR_BasicBlock && );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ~WIR_BasicBlock( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      When copying a basic block that is inserted in some %WIR function, the
      resulting copy will not be inserted in a function.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_BasicBlock & operator = ( const WIR_BasicBlock & );

    /*!
      @brief Move-assignment operator.

      @param[in] __o An R-value reference to another object to be moved.

      Trying to move a basic block that is inserted in some %WIR function
      results in an assertion, since you are not allowed to move a basic block
      whose ownership is managed by a function.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_BasicBlock & operator = ( WIR_BasicBlock && );


    //
    // Various lists.
    //

    // A list API for all instructions managed by a %WIR basic block.
    WIR_LIST_DECL( WIR_Instruction, Instruction, public );

    /*!
      @brief moveInstruction moves the specified %WIR instruction from its
             previous basic block to this one.

      @param[in] i A reference to a %WIR_Instruction to be moved.
      @return A reference to the moved instruction within its new basic block.

      moveInstruction fails with an assertion if the instruction to be moved
      refers to virtual registers belonging to a function that is different than
      that owning this basic block.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Instruction &moveInstruction( WIR_Instruction & );

    /*!
      @brief begin returns an iterator to the first instruction of a basic
             block.

      @return A const iterator pointing to the first instruction.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<std::reference_wrapper<WIR_Instruction>>::const_iterator begin( void ) const;

    /*!
      @brief end returns an iterator to the end of a basic block's instruction
             list.

      @return A const iterator pointing to the position after the last
              instruction of a basic block.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<std::reference_wrapper<WIR_Instruction>>::const_iterator end( void ) const;

    /*!
      @brief rbegin returns an iterator to the reverse-first instruction of a
             basic block.

      @return A const iterator pointing to the last instruction.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<std::reference_wrapper<WIR_Instruction>>::const_reverse_iterator rbegin( void ) const;

    /*!
      @brief rend returns an iterator to the reverse-end of a basic block's
             instruction list.

      @return A const iterator pointing to the position before the first
              instruction of a basic block.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<std::reference_wrapper<WIR_Instruction>>::const_reverse_iterator rend( void ) const;


    //
    // Function handling.
    //

    // Realize the API to manage a basic block's parent function.
    WIR_INSERTION_DECL( WIR_Function, Function );


    //
    // Name handling.
    //

    /*!
      @brief getName returns a basic block's specific name as constructed by
             function dumpWIRBlockLabel.

      @return A string that holds the block's name.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::string getName( void ) const;


    //
    // Size computation.
    //

    /*!
      @brief getSize returns a basic block's size in bytes.

      @return The basic block's byte size.

      getSize rounds upwards. I.e., if a basic block occupies 10 bytes and 4
      bits, getSize returns a size of 11 bytes.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    unsigned long long getSize( void ) const;


    //
    // Control flow properties.
    //

    /*!
      @brief getSuccessors returns a set of %WIR basic blocks that could be
             executed immediately after this block according to the control flow
             structure.

      @return The set of successor basic blocks.

      The set of successor blocks is determined as follows:
      -# If the last operation of this block is a jump, the target blocks of
         that jump are returned.
      -# Otherwise, the immediate successor of this block within its %WIR
         function is returned.
      Note that interprocedural control flow stemming from function calls is not
      considered here.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_BasicBlockSet getSuccessors( void ) const;

    /*!
      @brief getPredecessors returns a set of %WIR basic blocks that could be
             executed immediately before this block according to the control
             flow structure.

      @return The set of predecessor basic blocks.

      The set of predecessor blocks is given by all those basic blocks of this
      block's function where this block appears as successor.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_BasicBlockSet getPredecessors( void ) const;


    //
    // Register handling
    //

    /*!
      @brief getVREGs determines all virtual registers that occur in this basic
             block's instructions.

      @return The set of all virtual registers.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_VirtualRegisterSet getVREGs( void ) const;


    //
    // Modification prevention.
    //

    /*!
      @brief setDontOptimize sets whether a basic block can be modified or must
             not be changed by some optimization or transformation.

      @param[in] f A Boolean flag denoting whether the basic block must not be
                   modified.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setDontOptimize( bool = true );

    /*!
      @brief getDontOptimize returns whether a basic block can be modified or
             must not be changed by some optimization or transformation.

      @return true if the basic block must not be modified, false otherwise.

      A basic block must not be modified if the block by itself has been marked
      as such using setDontOptimize, or if it is inserted into a %WIR function
      that in turn must not be modified.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool getDontOptimize( void ) const;


    //
    // Stream I/O.
    //

    /*!
      @brief The << operator dumps a %WIR basic block to an output stream.

      @param[in] os A reference to an output stream.
      @param[in] b A const reference to the %WIR basic block to be dumped.
      @return A reference to the same output stream.

      By applying processor-specific I/O manipulators to the output stream
      beforehand, this << operator can flexibly emit valid assembly output for
      arbitrary processor architectures.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    friend std::ostream & operator << ( std::ostream &,
                                        const WIR_BasicBlock & );


  private:

    friend class WIR_Symbol;

    /*!
      @brief addInstructions adds the given list of instructions to this basic
             block.

      @tparam Args R-value references of class WIR_Instruction.
      @param[in] i An R-value reference to a %WIR instruction.
      @param[in] args A variadic number of R-value references to %WIR
                      instructions.

      addInstructions uses variadic templates so that argument lists of
      arbitrary lengths can be passed to it.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    template<typename... Args>
    void addInstructions( WIR_Instruction &&i, Args&&... args )
    {
      DSTART(
        "void WIR_BasicBlock::addInstructions(WIR_Instruction&&, Args&& ...)" );

      pushBackInstruction( std::move( i ) );
      addInstructions( std::forward<Args>( args )... );
    };

    /*!
      @brief Dummy function for adding instructions which does nothing.

      It only serves to terminate the recursion of the variadic method
      addInstructions.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void addInstructions( void ) const;

    /*!
      @brief copyBasicBlock performs actions common to the copy constructor and
             copy assignment operator of %WIR basic blocks.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void copyBasicBlock( const WIR_BasicBlock & );

    /*!
      @brief checkDontOptimize checks whether a basic block must not be
             modified.

      If this basic block must not be modified, checkDontOptimize asserts.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void checkDontOptimize( void ) const;

    /*!
      @brief Upon insertion of an instruction into a basic block, checkVREGs
             verifies that no virtual register occuring in the instruction
             belongs to a function different than that owning this basic block.

      @param[in] i A const reference to the instruction to be inserted.

      If a virtual register belongs to a different function, checkVREGs asserts.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void checkVREGs( const WIR_Instruction & ) const;


    //
    // Symbol handling.
    //

    /*!
      @brief invalidateSymbols marks the information of a system's global symbol
             table as invalid.

      invalidateSymbols only invalidates the symbol table if the current basic
      block is actually inserted into some %WIR system.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void invalidateSymbols( void ) const;


    //
    // Miscellaneous.
    //

    /*!
      @brief mDontOptimize stores whether a basic block can be modified or must
             not be changed by some optimization or transformation.
    */
    bool mDontOptimize;

    /*!
      @brief mDirectSuccessor points to the next basic block directly after this
             one in a function's sequential list of blocks.
    */
    WIR_BasicBlock *mDirectSuccessor;

};

}       // namespace WIR

#endif  // _WIR_BASICBLOCK_H
