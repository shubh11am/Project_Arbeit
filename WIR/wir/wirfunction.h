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
  @file wirfunction.h
  @brief This file provides the interface of %WIR functions.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_FUNCTION_H
#define _WIR_FUNCTION_H


//
// Include section
//

// Include standard headers
#include <fstream>
#include <map>
#include <string>
#include <utility>

// Include boost headers
#include <boost/current_function.hpp>

// Include WIR headers
#include <wir/API/wircontainerapi.h>
#include <wir/API/wiridapi.h>
#include <wir/API/wirinsertionapi.h>
#include <wir/API/wirlistapi.h>
#include <wir/API/wirpointerlistapi.h>
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
class WIR_PhysicalRegister;
class WIR_VirtualRegister;


/*!
  @brief Class WIR_Function is the generic representation of code functions.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_Function final : public WIR_ID_API,
                           public WIR_Container_API
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor creating an empty function.

      @param[in] s A const reference to a string to be copied that holds the
                   function's name.

      This constructor asserts if it is passed an empty string.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_Function( const std::string & );

    /*!
      @brief Default constructor creating an empty function.

      @param[in] s An R-value reference to a string to be moved that holds the
                   function's name.

      This constructor asserts if it is passed an empty string.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_Function( std::string && );

    /*!
      @brief Default constructor creating a function containing the given basic
             blocks.

      @tparam Args R-value references of class WIR_BasicBlock.
      @param[in] s A const reference to a string to be copied that holds the
                   function's name.
      @param[in] b An R-value reference to a %WIR basic block.
      @param[in] __args A variadic number of R-value references to %WIR
                        basic blocks to be inserted in this function.

      This constructor uses variadic templates so that argument lists of
      arbitrary lengths can be passed to it.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    template<typename... Args>
    WIR_Function( const std::string &s, WIR_BasicBlock &&b, Args&&... __args ) :
      WIR_ID_API {},
      WIR_Container_API {},
      mCompilationUnitPointer { nullptr },
      mDontOptimize { false },
      mCheckVREGs { true },
      mName { s },
      mFrameSize { 0 }
    {
      std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      ufAssert( !s.empty() );

      pushBackBasicBlock( std::move( b ) );
      addBasicBlocks( std::forward<Args>( __args )... );
    };

    /*!
      @brief Default constructor creating a function containing the given basic
             blocks.

      @tparam Args R-value references of class WIR_BasicBlock.
      @param[in] s An R-value reference to a string to be moved that holds the
                   function's name.
      @param[in] b An R-value reference to a %WIR basic block.
      @param[in] __args A variadic number of R-value references to %WIR
                        basic blocks to be inserted in this function.

      This constructor uses variadic templates so that argument lists of
      arbitrary lengths can be passed to it.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    template<typename... Args>
    WIR_Function( std::string &&s, WIR_BasicBlock &&b, Args&&... __args ) :
      WIR_ID_API {},
      WIR_Container_API {},
      mCompilationUnitPointer { nullptr },
      mDontOptimize { false },
      mCheckVREGs { true },
      mName { std::move( s ) },
      mFrameSize { 0 }
    {
      std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      ufAssert( !s.empty() );
      s.clear();

      pushBackBasicBlock( std::move( b ) );
      addBasicBlocks( std::forward<Args>( __args )... );
    };

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      When copying a function that is inserted in some %WIR compilation unit,
      the resulting copy will not be inserted in a compilation unit. Copying a
      function implies that the newly created function and all its basic blocks,
      instructions, operations etc. are all set as getDontOptimize() == false.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Function( const WIR_Function & );

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      Trying to move a function that is inserted in some %WIR compilation unit
      results in an assertion, since you are not allowed to move a function
      whose ownership is managed by a compilation unit.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Function( WIR_Function && );

    /*!
      @brief Destructor.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ~WIR_Function( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      When copying a function that is inserted in some %WIR compilation unit,
      the resulting copy will not be inserted in a compilation unit.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Function & operator = ( const WIR_Function & );

    /*!
      @brief Move-assignment operator.

      @param[in] __o An R-value reference to another object to be moved.

      Trying to move a function that is inserted in some %WIR compilation unit
      results in an assertion, since you are not allowed to move a function
      whose ownership is managed by a compilation unit.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Function & operator = ( WIR_Function && );


    //
    // Various lists.
    //

    // A list API for all basic blocks managed by a %WIR function.
    WIR_LIST_DECL( WIR_BasicBlock, BasicBlock, public );

    /*!
      @brief begin returns an iterator to the first basic block of a function.

      @return A const iterator pointing to the first basic block.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<std::reference_wrapper<WIR_BasicBlock>>::const_iterator begin( void ) const;

    /*!
      @brief end returns an iterator to the end of a function's basic block
             list.

      @return A const iterator pointing to the position after the last basic
              block of a function.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<std::reference_wrapper<WIR_BasicBlock>>::const_iterator end( void ) const;

    /*!
      @brief rbegin returns an iterator to the reverse-first basic block of a
             function.

      @return A const iterator pointing to the last basic block.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<std::reference_wrapper<WIR_BasicBlock>>::const_reverse_iterator rbegin( void ) const;

    /*!
      @brief rend returns an iterator to the reverse-end of a function's basic
             block list.

      @return A const iterator pointing to the position before the first basic
              block of a function.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<std::reference_wrapper<WIR_BasicBlock>>::const_reverse_iterator rend( void ) const;

    // A list API for all virtual registers managed by a %WIR function.
    WIR_POINTERLIST_DECL( WIR_VirtualRegister, VirtualRegister, public );

    /*!
      @brief pushBackVirtualRegister adds a new WIR_VirtualRegister at the end
             of lists mVirtualRegisterPointers and mVirtualRegisterReferences,
             after its current last element.

      @param[in] r A pointer to the newly inserted element.
      @return A reference to the newly inserted element.

      @note Class WIR_Function takes over full control over the ownership of
            the given pointer! In particular, WIR_Function automatically
            destroys the object pointed to. Users of this variant of
            pushBackVirtualRegister are strongly discouraged of continuing to
            use this pointer afterwards. This variant of pushBackVirtualRegister
            is more efficient than the previous ones since it completely avoids
            any (polymorphic) copy operations. Thus, it should only be used if
            large amounts of virtual registers shall be created/added highly
            efficiently as, e.g., in a compiler's code selector.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_VirtualRegister & pushBackVirtualRegister( WIR_VirtualRegister * );


    //
    // Compilation unit handling.
    //

    // Realize the API to manage a function's parent compilation unit.
    WIR_INSERTION_DECL( WIR_CompilationUnit, CompilationUnit );


    //
    // Name handling.
    //

    /*!
      @brief setName sets a function's specific name.

      @param[in] s A const reference to a string to be copied that holds the
                   function's name.

      setName asserts if it is passed an empty string.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setName( const std::string & );

    /*!
      @brief setName sets a function's specific name.

      @param[in] s An R-value reference to a string to be moved that holds the
                   function's name.

      setName asserts if it is passed an empty string.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setName( std::string && );

    /*!
      @brief getName returns a function's specific name.

      @return A string that holds the function's name.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::string getName( void ) const;


    //
    // Size computation.
    //

    /*!
      @brief getSize returns a function's code size in bytes.

      @return The function's byte size.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    unsigned long long getSize( void ) const;


    //
    // Precolor and interference handling.
    //

    /*!
      @brief insertPrecolor precolors the specified virtual register with a
             given physical register for register allocation.

      @param[in] v A const reference to a virtual register to be precolored.
      @param[in] p A const reference to a physical register to use during
                   register allocation.

      If v is part of a register hierarchy, the entire hierarchy of virtual
      registers is precolored with their corresponding hierarchical physical
      registers. If insertPrecolor is applied to a virtual register that is
      already precolored, the new precolor information silently overrides the
      old one.

      insertPrecolor asserts if it is called with a virtual register that does
      not belong to this %WIR function. It furthermore asserts if both the
      virtual and physical register have different register types, i.e., if it
      is attempted to precolor a TriCore virtual register with an ARM's physical
      register. It finally asserts if the two registers that shall be precolored
      have already been marked as interfering before.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertPrecolor( const WIR_VirtualRegister &,
                         const WIR_PhysicalRegister & );

    /*!
      @brief erasePrecolor removes any precoloring information related to the
             specified virtual register.

      @param[in] v A const reference to a virtual register to be precolored.

      If the given virtual register is not precolored at all or if it does not
      belong to this %WIR function, erasePrecolor silently ignores these issues.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void erasePrecolor( const WIR_VirtualRegister & );

    /*!
      @brief clearPrecolors removes all precoloring information from this %WIR
             function.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void clearPrecolors( void );

    /*!
      @brief containsPrecolor returns whether the specified virtual register is
             precolored or not.

      @param[in] v A const reference to a virtual register.
      @return true iff v belongs to this %WIR function and is precolored, false
              otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool containsPrecolor( const WIR_VirtualRegister & ) const;

    /*!
      @brief findPrecolor returns the physical register associated with the
             specified virtual register for register allocation.

      @param[in] v A const reference to a precolored virtual register.
      @return A reference to the physical register with which v is precolored.

      findPrecolor asserts if the specified virtual register does not belong to
      this %WIR function or if it is not precolored.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_PhysicalRegister &findPrecolor( const WIR_VirtualRegister & ) const;

    /*!
      @brief insertInterference marks the two specified registers as interfering
             for register allocation.

      @param[in] v A const reference to the first interfering virtual register.
      @param[in] p A const reference to the second interfering physical
                   register.

      If v is part of a register hierarchy, interferences are inserted for the
      entire hierarchies of both argument registers.

      insertInterference asserts if it is called with a virtual register that
      does not belong to this %WIR function. It furthermore asserts if both
      registers have different register types, i.e., if it is attempted to
      let a data register interfere with an address register. It finally asserts
      if the two registers that shall interfere have already been precolored
      with each other.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
   */
    void insertInterference( const WIR_VirtualRegister &,
                             const WIR_PhysicalRegister & );

    /*!
      @brief eraseInterference erases an interference between the two specified
             registers.

      @param[in] v A const reference to the first interfering virtual register.
      @param[in] p A const reference to the second interfering physical
                   register.

      If the two given registers do not interfere or if the specified virtual
      register does not belong to this %WIR function, eraseInterference silently
      ignores these issues.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void eraseInterference( const WIR_VirtualRegister &,
                            const WIR_PhysicalRegister & );

    /*!
      @brief clearInterferences removes all interference information from this
             %WIR function.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void clearInterferences( void );

    /*!
      @brief interfere returns whether the two specified registers interfere.

      @param[in] v A const reference to the first interfering virtual register.
      @param[in] p A const reference to the second interfering physical
                   register.
      @return true if the two registers interfere, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool interfere( const WIR_VirtualRegister &,
                    const WIR_PhysicalRegister & ) const;

    /*!
      @brief findInterferences returns the set of all physical registers that a
             virtual register of this %WIR function interferes with.

      @param[in] v A const reference to a virtual register.
      @return A set containing (wrapped) references to all physical registers
              interfering with v.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_PhysicalRegisterSet findInterferences( const WIR_VirtualRegister & ) const;


    //
    // Stack handling.
    // NOTE: An API for automatic stack layout management would be nice where
    //       one can specify whether the stack grows up-/downwards, where
    //       symbols can be put on the stack frame and start/end addresses of
    //       symbols relative to the stack pointer can be queried.
    //

    /*!
      @brief setFrameSize sets a function's processor-specific stack frame size.

      @param[in] f An integer denoting the function's new stack frame size.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setFrameSize( int );

    /*!
      @brief getFrameSize returns a function's processor-specific stack frame
             size.

      @return The function's stack frame size.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    int getFrameSize( void ) const;

    /*!
      @brief addFunctionInput adds a physical register used for argument passing
             to the set of function inputs.

      @param[in] p A const reference to a physical register being function
                   input.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void addFunctionInput( const WIR_PhysicalRegister & );

    /*!
      @brief getFunctionInputs returns the set of physical registers providing
             external inputs to a function.

      @return A const reference to set mFunctionInputs.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const WIR_PhysicalRegisterSet &getFunctionInputs( void ) const;


    //
    // Register handling
    //

    /*!
      @brief getVREGs determines all virtual registers that occur in this
             function's basic blocks.

      @return The set of all virtual registers.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_VirtualRegisterSet getVREGs( void ) const;


    //
    // Modification prevention.
    //

    /*!
      @brief setDontOptimize sets whether a function can be modified or must
             not be changed by some optimization or transformation.

      @param[in] f A Boolean flag denoting whether the function must not be
                   modified.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setDontOptimize( bool = true );

    /*!
      @brief getDontOptimize returns whether a function can be modified or
             must not be changed by some optimization or transformation.

      @return true if the function must not be modified, false otherwise.

      A function must not be modified if the function by itself has been marked
      as such using setDontOptimize, or if it is inserted into a %WIR
      compilation unit that in turn must not be modified.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool getDontOptimize( void ) const;


    //
    // Stream I/O.
    //

    /*!
      @brief The << operator dumps a %WIR function to an output stream.

      @param[in] os A reference to an output stream.
      @param[in] f A const reference to the %WIR function to be dumped.
      @return A reference to the same output stream.

      By applying processor-specific I/O manipulators to the output stream
      beforehand, this << operator can flexibly emit valid assembly output for
      arbitrary processor architectures.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    friend std::ostream & operator << ( std::ostream &,
                                        const WIR_Function & );


  private:

    friend class WIR_Symbol;
    friend void dumpWIRFunction( std::ostream &os, const WIR_Function &f );

    /*!
      @brief No standard construction allowed, users must use
             WIR_Function( std::string )
             instead.
    */
    WIR_Function( void ) = delete;

    /*!
      @brief addBasicBlocks adds the given list of basic blocks to this
             function.

      @tparam Args R-value references of class WIR_BasicBlock.
      @param[in] b An R-value reference to a %WIR basic block.
      @param[in] args A variadic number of R-value references to %WIR basic
                      blocks.

      addBasicBlocks uses variadic templates so that argument lists of arbitrary
      lengths can be passed to it.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    template<typename... Args>
    void addBasicBlocks( WIR_BasicBlock &&b, Args&&... args )
    {
      std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      pushBackBasicBlock( std::move( b ) );
      addBasicBlocks( std::forward<Args>( args )... );
    };

    /*!
      @brief Dummy function for adding basic blocks which does nothing.

      It only serves to terminate the recursion of the variadic method
      addBasicBlocks.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void addBasicBlocks( void ) const;

    /*!
      @brief copyFunction performs actions common to the copy constructor and
             copy assignment operator of %WIR functions.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void copyFunction( const WIR_Function & );

    /*!
      @brief checkDontOptimize checks whether a function must not be modified.

      If this function must not be modified, checkDontOptimize asserts.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void checkDontOptimize( void ) const;

    /*!
      @brief Upon insertion of a basic block into a function, checkVREGs
             verifies that no virtual register occuring in the basic block
             belongs to a different function.

      @param[in] b A const reference to the basic block to be inserted.

      If a virtual register belongs to a different function, checkVREGs asserts.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void checkVREGs( const WIR_BasicBlock & ) const;


    //
    // Symbol handling.
    //

    /*!
      @brief For the specified basic block, insertSymbol adds a corresponding
             symbol to the system's symbol table.

      @param[in] b A const reference to a basic block for which a symbol shall
                   be added.

      insertSymbol only creates a new symbol if the current function is actually
      inserted into some %WIR system.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertSymbol( const WIR_BasicBlock & );

    /*!
      @brief mPrecolors maps the ID of a virtual register to a physical register
             in order to provide pre-assignments for register allocation.
    */
    std::map<std::reference_wrapper<WIR_VirtualRegister>, WIR_PhysicalRegister *, WIR_Compare<WIR_VirtualRegister>> mPrecolors;

    /*!
      @brief mInterferences stores interferences between virtual registers and
             sets of physical registers in order to provide constraints for
             register allocation.
    */
    std::map<WIR_id_t, WIR_PhysicalRegisterSet> mInterferences;

    /*!
      @brief mDontOptimize stores whether a function can be modified or must not
             be changed by some optimization or transformation.
    */
    bool mDontOptimize;

    /*!
      @brief mCheckVREGs steers whether checkVREGs() is applied while inserting
             a basic block or not.
    */
    bool mCheckVREGs;

    //! mName holds a function's name.
    std::string mName;

    //! mFrameSize holds a function's processor-specific stack frame size.
    int mFrameSize;

    /*!
      @brief mFunctionInputs stores physical registers that are used for passing
             arguments to the function. These physical registers thus denote
             inputs to a function that are defined outside the function itself,
             and this needs to be considered properly during data flow analysis.
    */
    WIR_PhysicalRegisterSet mFunctionInputs;

};

}       // namespace WIR

#endif  // _WIR_FUNCTION_H
