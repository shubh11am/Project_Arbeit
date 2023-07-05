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
  @file wirinstruction.h
  @brief This file provides the interface of %WIR instructions.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_INSTRUCTION_H
#define _WIR_INSTRUCTION_H


//
// Include section
//

// Include standard headers
#include <utility>

// Include boost headers
#include <boost/current_function.hpp>

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

class WIR_BasicBlock;
class WIR_Operation;


/*!
  @brief Class WIR_Instruction is the generic representation of code
  instructions.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_Instruction final : public WIR_ID_API,
                              public WIR_Container_API
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor creating an empty instruction.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Instruction( void );

    /*!
      @brief Default constructor creating an instruction containing the given
             operations.

      @tparam Args R-value references of class WIR_Operation.
      @param[in] a An R-value reference to a %WIR operation.
      @param[in] __args A variadic number of R-value references to %WIR
                        operations to be inserted in this instruction.

      This constructor uses variadic templates so that argument lists of
      arbitrary lengths can be passed to it.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    template<typename... Args>
    WIR_Instruction( WIR_Operation &&a, Args&&... __args ) :
      WIR_ID_API {},
      WIR_Container_API {},
      mBasicBlockPointer { nullptr },
      mDontOptimize { false }
    {
      DSTART( "WIR_Instruction::WIR_Instruction(WIR_Operation&&, Args&& ...)" );

      pushBackOperation( std::move( a ) );
      addOperations( std::forward<Args>( __args )... );
    };

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      When copying an instruction that is inserted in some %WIR basic block, the
      resulting copy will not be inserted in a basic block.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Instruction( const WIR_Instruction & );

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      Trying to move an instruction that is inserted in some %WIR basic block
      results in an assertion, since you are not allowed to move a register
      whose ownership is managed by a basic block.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Instruction( WIR_Instruction && );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ~WIR_Instruction( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      When copying an instruction that is inserted in some %WIR basic block, the
      resulting copy will not be inserted in a basic block.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Instruction & operator = ( const WIR_Instruction & );

    /*!
      @brief Move-assignment operator.

      @param[in] __o An R-value reference to another object to be moved.

      Trying to move an instruction that is inserted in some %WIR basic block
      results in an assertion, since you are not allowed to move a register
      whose ownership is managed by a basic block.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Instruction & operator = ( WIR_Instruction && );


    //
    // Various lists.
    //

    // A list API for all operations managed by a %WIR instruction.
    WIR_LIST_DECL( WIR_Operation, Operation, public );

    /*!
      @brief moveOperation moves the specified %WIR operation from its current
             instruction to this one.

      @param[in] o A reference to a %WIR_Operation to be moved.
      @return A reference to the moved operatoin within its new instruction.

      The moved operation is added at the tail of this instruction's list of
      operations, i.e., a pushBackOperation is performed internally.

      moveOperation fails with an assertion if the operation to be moved refers
      to virtual registers belonging to a function that is different than that
      owning this instruction.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Operation &moveOperation( WIR_Operation & );

    /*!
      @brief begin returns an iterator to the first operation of an instruction.

      @return A const iterator pointing to the first operation.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<std::reference_wrapper<WIR_Operation>>::const_iterator begin( void ) const;

    /*!
      @brief end returns an iterator to the end of an instruction's operation
             list.

      @return A const iterator pointing to the position after the last operation
              of an instruction.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<std::reference_wrapper<WIR_Operation>>::const_iterator end( void ) const;

    /*!
      @brief rbegin returns an iterator to the reverse-first operation of an
             instruction.

      @return A const iterator pointing to the last operation.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<std::reference_wrapper<WIR_Operation>>::const_reverse_iterator rbegin( void ) const;

    /*!
      @brief rend returns an iterator to the reverse-end of an instruction's
             operation list.

      @return A const iterator pointing to the position before the first
              operation of an instruction.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<std::reference_wrapper<WIR_Operation>>::const_reverse_iterator rend( void ) const;


    //
    // Basic block handling.
    //

    // Realize the API to manage a instruction's parent basic block.
    WIR_INSERTION_DECL( WIR_BasicBlock, BasicBlock );


    //
    // Size computation.
    //

    /*!
      @brief getBitWidth returns an instruction's bit width.

      @return The instruction's bit width.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    unsigned int getBitWidth( void ) const;

    /*!
      @brief getSize returns an instruction's size in bytes.

      @return The instruction's byte size.

      getSize rounds upwards. I.e., if an instruction occupies 20 bits, getSize
      returns a size of 3 bytes.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    unsigned long long getSize( void ) const;


    //
    // Register handling
    //

    /*!
      @brief getVREGs determines all virtual registers that occur in this
             instruction's operations.

      @return The set of all virtual registers.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_VirtualRegisterSet getVREGs( void ) const;


    //
    // Modification prevention.
    //

    /*!
      @brief setDontOptimize sets whether an instruction can be modified or must
             not be changed by some optimization or transformation.

      @param[in] f A Boolean flag denoting whether the instruction must not be
                   modified.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setDontOptimize( bool = true );

    /*!
      @brief getDontOptimize returns whether an instruction can be modified or
             must not be changed by some optimization or transformation.

      @return true if the instruction must not be modified, false otherwise.

      An instruction must not be modified if the instruction by itself has been
      marked as such using setDontOptimize, or if it is inserted into a %WIR
      basic block that in turn must not be modified.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool getDontOptimize( void ) const;


    //
    // Stream I/O.
    //

    /*!
      @brief The << operator dumps a %WIR instruction to an output stream.

      @param[in] os A reference to an output stream.
      @param[in] i A const reference to the %WIR instruction to be dumped.
      @return A reference to the same output stream.

      By applying processor-specific I/O manipulators to the output stream
      beforehand, this << operator can flexibly emit valid assembly output for
      arbitrary processor architectures.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    friend std::ostream & operator << ( std::ostream &,
                                        const WIR_Instruction & );


  private:

    /*!
      @brief addOperations adds the given list of operations to this
             instruction.

      @tparam Args R-value references of class WIR_Operation.
      @param[in] a An R-value reference to a %WIR operation.
      @param[in] args A variadic number of R-value references to %WIR
                      operations.

      addOperations uses variadic templates so that argument lists of arbitrary
      lengths can be passed to it.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    template<typename... Args>
    void addOperations( WIR_Operation &&a, Args&&... args )
    {
      DSTART(
        "void WIR_Instruction::addOperations(WIR_Operation&&, Args&& ...)" );

      pushBackOperation( std::move( a ) );
      addOperations( std::forward<Args>( args )... );
    };

    /*!
      @brief Dummy function for adding operations which does nothing.

      It only serves to terminate the recursion of the variadic method
      addOperations.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void addOperations( void ) const;

    /*!
      @brief copyInstruction performs actions common to the copy constructor and
             copy assignment operator of %WIR instructions.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void copyInstruction( const WIR_Instruction & );

    /*!
      @brief checkDontOptimize checks whether an instruction must not be
             modified.

      If this instruction must not be modified, checkDontOptimize asserts.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void checkDontOptimize( void ) const;

    /*!
      @brief Upon insertion of an operation into an instruction, checkVREGs
             verifies that no virtual register occuring in the operation belongs
             to a function different than that owning this instruction.

      @param[in] o A const reference to the operation to be inserted.

      If a virtual register belongs to a different function, checkVREGs asserts.
      Furthermore, checkVREGs also directly invalidates the symbol table if the
      current basic block is actually inserted into some %WIR system. Although
      this symbol table invalidation is also explicitly done by method
      invalidateSymbols below, we also do it in checkVREGs for efficiency
      reasons.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void checkVREGs( const WIR_Operation & ) const;


    //
    // Symbol handling.
    //

    /*!
      @brief invalidateSymbols marks the information of a system's global symbol
             table as invalid.

      invalidateSymbols only invalidates the symbol table if the current
      instruction is actually inserted into some %WIR system.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void invalidateSymbols( void ) const;


    //
    // Attributes.
    //

    /*!
      @brief mDontOptimize stores whether an instruction can be modified or must
             not be changed by some optimization or transformation.
    */
    bool mDontOptimize;

};

}       // namespace WIR

#endif  // _WIR_INSTRUCTION_H
