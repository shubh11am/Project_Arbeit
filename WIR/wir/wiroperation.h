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
  @file wiroperation.h
  @brief This file provides the interface of %WIR operations.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_OPERATION_H
#define _WIR_OPERATION_H


//
// Include section
//

// Include standard headers
#include <utility>

// Include libuseful headers
#include <libuseful/debugmacros.h>

// Include WIR headers
#include <wir/API/wircontainerapi.h>
#include <wir/API/wiridapi.h>
#include <wir/API/wirinsertionapi.h>
#include <wir/API/wirlistapi.h>
#include <wir/API/wirpointerlistapi.h>
#include <wir/wirbaseprocessor.h>
#include <wir/wirtypes.h>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_BasicBlock;
class WIR_Instruction;
class WIR_Parameter;


/*!
  @brief Class WIR_Operation is the generic representation of machine code
         operations.

  Several operations can be included in one single %WIR instruction. All
  operations of one instruction are assumed to be executed in parallel. This
  enables the modelling of code for VLIW architectures.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_Operation final : public WIR_ID_API,
                            public WIR_Container_API
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor for %WIR operations.

      @tparam Args R-value references of class WIR_Parameter or derived classes.
      @param[in] __o A const reference to the opcode of this operation.
      @param[in] __f A const reference to the format of this operation.
      @param[in] __args A variadic number of R-value references to
                        WIR_Parameters for this operation.

      This constructor uses variadic templates so that argument lists of
      arbitrary lengths can be passed to it.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    template<typename... Args>
    WIR_Operation( const WIR_BaseProcessor::OpCode &__o,
                   const WIR_BaseProcessor::OperationFormat &__f,
                   Args&&... __args ) :
      WIR_ID_API {},
      WIR_Container_API {},
      mInstructionPointer { nullptr },
      mOpCode( const_cast<WIR_BaseProcessor::OpCode *>( &__o ) ),
      mOperationFormat( const_cast<WIR_BaseProcessor::OperationFormat *>( &__f ) ),
      mCheckParameters { true },
      mDontOptimize { false }
    {
      DSTART(
        "WIR_Operation::WIR_Operation(const WIR_BaseProcessor::OpCode&, const WIR_BaseProcessor::OperationFormat&, Args&& ...)" );

      checkOperationFormat();
      mCheckParameters = false;
      addParameters( std::forward<Args>( __args )... );
      mCheckParameters = true;
      checkParameters();
    };

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      When copying an operation that is inserted in some %WIR instruction, the
      resulting copy will not be inserted in an instruction.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Operation( const WIR_Operation & );

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      Trying to move an operation that is inserted in some %WIR instruction
      results in an assertion, since you are not allowed to move an operation
      whose ownership is managed by an instruction.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Operation( WIR_Operation && );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ~WIR_Operation( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      When copying an operation that is inserted in some %WIR instruction, the
      resulting copy will not be inserted in an instruction.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Operation & operator = ( const WIR_Operation & );

    /*!
      @brief Move-assignment operator.

      @param[in] __o An R-value reference to another object to be moved.

      Trying to move an operation that is inserted in some %WIR instruction
      results in an assertion, since you are not allowed to move an operation
      whose ownership is managed by an instruction.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Operation & operator = ( WIR_Operation && );


    //
    // Opcode handling.
    //

    /*!
      @brief getOpCode returns an operation's opcode.

      @return A const reference to the opcode.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const WIR_BaseProcessor::OpCode &getOpCode( void ) const;

    /*!
      @brief getOperationFormat returns an operation's format.

      @return A const reference to the operation format.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const WIR_BaseProcessor::OperationFormat &getOperationFormat( void ) const;


    //
    // Various lists.
    //

    // A list API for all parameters managed by a %WIR operation.
    WIR_POINTERLIST_DECL( WIR_Parameter, Parameter, public );

    /*!
      @brief pushBackParameter adds a new WIR_Parameter at the end of lists
             mParameterPointers and mParameterReferences, after its current last
             element.

      @param[in] p A pointer to the newly inserted element.
      @return A reference to the newly inserted element.

      @note Class WIR_Operation takes over full control over the ownership of
            the given pointer! In particular, WIR_Operation automatically
            destroys the object pointed to. Users of this variant of
            pushBackParameter are strongly discouraged of continuing to use this
            pointer afterwards. This variant of pushBackParameter is more
            efficient than the previous ones since it completely avoids any
            (polymorphic) copy operations. Thus, it should only be used if large
            amounts of parameters shall be created/added highly efficiently as,
            e.g., in operation factories of a compiler's code selector.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Parameter & pushBackParameter( WIR_Parameter * );

    /*!
      @brief getExplicitParameters returns the list of all explicit parameters.

      @return A list containing references to all explicit parameters.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const std::list<std::reference_wrapper<WIR_Parameter>> &getExplicitParameters( void ) const;

    /*!
      @brief getExplicitParameter returns the nth explicit parameter of an
             operation.

      @param[in] n The number of the explicit parameter to be returned.
                   Parameter numbers start with the value 1.
      @return A reference to the nth explicit parameter.

      getExplicitParameter asserts if n is larger than the actual number of
      explicit parameters of an operation.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Parameter &getExplicitParameter( unsigned int ) const;

    /*!
      @brief begin returns an iterator to the first parameter of an operation.

      @return A const iterator pointing to the first parameter.

      The returned iterator points to the list returned by getParameters(), not
      to the list returned by getExplicitParameters()!

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<std::reference_wrapper<WIR_Parameter>>::const_iterator begin( void ) const;

    /*!
      @brief end returns an iterator to the end of an operation's parameter
             list.

      @return A const iterator pointing to the position after the last parameter
              of an operation.

      The returned iterator points to the list returned by getParameters(), not
      to the list returned by getExplicitParameters()!

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<std::reference_wrapper<WIR_Parameter>>::const_iterator end( void ) const;

    /*!
      @brief rbegin returns an iterator to the reverse-first parameter of an
             operation.

      @return A const iterator pointing to the last parameter.

      The returned iterator points to the list returned by getParameters(), not
      to the list returned by getExplicitParameters()!

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<std::reference_wrapper<WIR_Parameter>>::const_reverse_iterator rbegin( void ) const;

    /*!
      @brief rend returns an iterator to the reverse-end of an operation's
             parameter list.

      @return A const iterator pointing to the position before the first
              parameter of an operation.

      The returned iterator points to the list returned by getParameters(), not
      to the list returned by getExplicitParameters()!

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<std::reference_wrapper<WIR_Parameter>>::const_reverse_iterator rend( void ) const;


    //
    // Instruction handling.
    //

    // Realize the API to manage an operation's parent instruction.
    WIR_INSERTION_DECL( WIR_Instruction, Instruction );


    //
    // Parameter handling.
    //

    /*!
      @brief getNumberOfExplicitParameters returns the number of explicit
             parameters of an operation.

      @return The number of explicit parameters.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    unsigned int getNumberOfExplicitParameters( void ) const;

    /*!
      @brief getNumberOfImplicitParameters returns the number of implicit
             parameters of an operation.

      @return The number of implicit parameters.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    unsigned int getNumberOfImplicitParameters( void ) const;

    /*!
      @brief enforceParameterChecking enables parameter checks by method
             checkParameters, even if %WIR is not built in failsafe mode.

      Use this method with care as it has a performance impact. It should be
      used, e.g., within an assembly code parser in order to ensure that all
      operations created by the parser actually have a good operation format.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    static void enforceParameterChecking( void );

    /*!
      @brief resetParameterChecking deactivates the effect of method
             enforceParameterChecking and returns to %WIR's default behavior,
             depending on whether the failsafe mode is used or not.
    */
    static void resetParameterChecking( void );


    //
    // Size computation.
    //

    /*!
      @brief getBitWidth returns an operation's bit width.

      @return The operation's bit width.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    unsigned int getBitWidth( void ) const;

    /*!
      @brief getSize returns an operation's size in bytes.

      @return The operation's byte size.

      getSize rounds upwards. I.e., if an operation occupies 20 bits, getSize
      returns a size of 3 bytes.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    unsigned long long getSize( void ) const;


    //
    // Operation properties.
    //

    /*!
      @brief isMemoryAccess returns whether an operation accesses memory
             without being an explicit load or store operation.

      @return true if the operation accesses memory, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isMemoryAccess( void ) const;

    /*!
      @brief isMemoryStore returns whether an operation writes to memory.

      @return true if the operation writes to memory, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isMemoryStore( void ) const;

    /*!
      @brief isMemoryLoad returns whether an operation reads from memory.

      @return true if the operation reads from memory, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isMemoryLoad( void ) const;

    /*!
      @brief isMove returns whether an operation is a register-register move.

      @return true if the operation is a move, false otherwise.

      This information is of particular interest for register allocation and
      coalescing of moves. The exact semantics of this method's return value is:
      true if the operation is a move where exactly one (virtual,
      non-precolored) register is defined and one (virtual, non-precolored)
      register is used.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isMove( void ) const;

    /*!
      @brief isCall returns whether an operation calls a function.

      @return true if the operation calls a function, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isCall( void ) const;

    /*!
      @brief isIndirectCall returns whether an operation indirectly calls a
             function.

      @return true if the operation is an indirect function call, false
              otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isIndirectCall( void ) const;

    /*!
      @brief isReturn returns whether an operation returns from a function call.

      @return true if the operation is a function return, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isReturn( void ) const;

    /*!
      @brief isJump returns whether an operation performs a jump.

      @return true if the operation is a jump, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isJump( void ) const;

    /*!
      @brief isConditionalJump returns whether an operation performs a
             conditional jump.

      @return true if the operation is a conditional jump, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isConditionalJump( void ) const;

    /*!
      @brief isUnconditionalJump returns whether an operation performs an
             unconditional jump.

      @return true if the operation is an unconditional jump, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isUnconditionalJump( void ) const;

    /*!
      @brief isIndirectJump returns whether an operation indirectly performs a
             jump.

      @return true if the operation is an indirect jump, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isIndirectJump( void ) const;

    /*!
      @brief isAsmDataDirective returns whether an opcode is an assembly data
             directive.

      @return true if the opcode is an assembly data directive, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isAsmDataDirective( void ) const;

    /*!
      @brief hasSideEffects returns whether an opcode has side effects.

      @return true if the opcode has side effects, false otherwise.

      Side effects could be non-obvious changes of a processor's internal status
      like, e.g., disabling interrupts, changing into supervisor mode or
      invalidating cache lines etc.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool hasSideEffects( void ) const;


    //
    // Control flow properties.
    //

    /*!
      @brief If this WIR_Operation is a jump, addJumpTarget adds the specified
             %WIR basic block to the set of explicit jump targets.

      @param[in] b A const reference to the basic block to be added as jump
                   target.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void addJumpTarget( const WIR_BasicBlock & );

    /*!
      @brief If this WIR_Operation is a jump, eraseJumpTarget removes the
             specified %WIR basic block from the set of explicit jump targets.

      @param[in] b A const reference to the basic block that is no longer a jump
                   target.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void eraseJumpTarget( const WIR_BasicBlock & );

    /*!
      @brief jumpTargetsAdded returns whether explicit jump targets have been
             specified for this WIR_Operation.

      @return true iff explicit jump targets were annotated using addJumpTarget,
              false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool jumpTargetsAdded( void ) const;

    /*!
      @brief If this WIR_Operation is a jump, getJumpTargets returns a set of
             %WIR basic blocks to which this operation jumps.

      @return The set of basic blocks to which this operation jumps.

      The set of jump targets is determined as follows:
      -# If this operation is not a jump, the empty set is returned.
      -# If jump targets were explicitly specified using addJumpTarget, then
         the set of these explicit jump targets is returned.
      -# If no explicit jump targets were specified, it is checked whether this
         jump contains label parameters, and the set of all basic blocks that
         are refered by these labels is returned.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_BasicBlockSet getJumpTargets( void ) const;


    //
    // Register handling
    //

    /*!
      @brief getVREGs determines all virtual registers that occur in this
             operation's parameters.

      @return The set of all virtual registers.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_VirtualRegisterSet getVREGs( void ) const;


    //
    // Modification prevention.
    //

    /*!
      @brief setDontOptimize sets whether an operation can be modified or must
             not be changed by some optimization or transformation.

      @param[in] f A Boolean flag denoting whether the operation must not be
                   modified.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setDontOptimize( bool = true );

    /*!
      @brief getDontOptimize returns whether an operation can be modified or
             must not be changed by some optimization or transformation.

      @return true if the operation must not be modified, false otherwise.

      An operation must not be modified if the operation by itself has been
      marked as such using setDontOptimize, or if it is inserted into a %WIR
      instruction that in turn must not be modified.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool getDontOptimize( void ) const;


    //
    // Stream I/O.
    //

    /*!
      @brief The << operator dumps a %WIR operation to an output stream.

      @param[in] os A reference to an output stream.
      @param[in] o A const reference to the %WIR operation to be dumped.
      @return A reference to the same output stream.

      By applying processor-specific I/O manipulators to the output stream
      beforehand, this << operator can flexibly emit valid assembly output for
      arbitrary processor architectures.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    friend std::ostream & operator << ( std::ostream &, const WIR_Operation & );


  private:

    friend class WIR_Parameter;

    /*!
      @brief No standard construction allowed, users must use
             WIR_Operation( const WIR_BaseProcessor::OpCode &,
                            const WIR_BaseProcessor::OperationFormat &, Args&&... )
             instead.
    */
    WIR_Operation( void ) = delete;

    /*!
      @brief checkOperationFormat ensures that the operation format stored in
             mOperationFormat is legal for the opcode from mOpCode.

      If this operation contains an illegal opcode/operation format combination,
      checkOperationFormat asserts.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void checkOperationFormat( void ) const;

    /*!
      @brief checkParameters ensures that the parameters stored in list
             mParameterReferences adhere to the operation format from
             mOperationFormat.

      If this operation contains a parameter sequence that does not match the
      specified operation format, checkParameters asserts.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void checkParameters( void ) const;

    /*!
      @brief copyOperation performs actions common to the copy constructor and
             copy assignment operator of %WIR operations.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void copyOperation( const WIR_Operation & );

    /*!
      @brief checkDontOptimize checks whether an operation must not be modified.

      If this operation must not be modified, checkDontOptimize asserts.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void checkDontOptimize( void ) const;

    /*!
      @brief Upon insertion of a parameter into an operation, checkVREGs
             verifies that no virtual register occuring in the parameter belongs
             to a function different than that owning this operation.

      @param[in] p A const reference to the parameter to be inserted.

      If a virtual register belongs to a different function, checkVREGs asserts.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void checkVREGs( const WIR_Parameter & ) const;

    /*!
      @brief addParameters adds the given list of parameters to this operation.

      @tparam T One R-value reference of class WIR_Parameter or derived classes.
      @tparam Args R-value references of class WIR_Parameter or derived classes.
      @param[in] a An R-value reference to a %WIR parameter.
      @param[in] args A variadic number of R-value references to %WIR
                      parameters.

      addParameters uses variadic templates so that argument lists of arbitrary
      lengths can be passed to it.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    template<typename T, typename... Args>
    void addParameters( T&& a, Args&&... args )
    {
      DSTART( "void WIR_Operation::addParameters(T&&, Args&& ...)" );

      pushBackParameter( std::forward<T>( a ) );
      addParameters( std::forward<Args>( args )... );
    };

    /*!
      @brief Dummy function for adding parameters which does nothing.

      It only serves to terminate the recursion of the variadic method
      addParameters.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void addParameters( void ) const;

    /*!
      @brief mExplicitParameterReferences holds (wrapped) references to all
             stored explicit parameters.
    */
    std::list<std::reference_wrapper<WIR_Parameter>> mExplicitParameterReferences;

    //! mOpCode points to an operation's opcode.
    WIR_BaseProcessor::OpCode *mOpCode;

    //! mOperationFormat points to an operation's parameter format.
    WIR_BaseProcessor::OperationFormat *mOperationFormat;

    /*!
      @brief mCheckParameters stores whether checkParameters should be invoked
             when inserting parameters, which is not desired during the creation
             of %WIR operations.
    */
    bool mCheckParameters;

    /*!
      @brief mDontOptimize stores whether an operation can be modified or must
             not be changed by some optimization or transformation.
    */
    bool mDontOptimize;

    /*!
      @brief mJumpTargets holds all operations specified as explicit jump
             targets.
    */
    WIR_BasicBlockSet mJumpTargets;

    /*!
      @brief Depending on whether %WIR is compiled in failsafe mode or not,
             constant mDefaultParameterChecking stores whether method
             checkParameters will be enabled or not by default.
    */
    static const bool mDefaultParameterChecking;

    /*!
      @brief mEnforceParameterChecking stores whether parameter checks by method
             checkParameters shall be enfored or not, irrespective of whether
             %WIR is built in failsame mode or not.
    */
    static bool mEnforceParameterChecking;

};

}       // namespace WIR

#endif  // _WIR_OPERATION_H
