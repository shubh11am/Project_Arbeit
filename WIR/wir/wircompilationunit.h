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
  @file wircompilationunit.h
  @brief This file provides the interface of %WIR compilation units (i.e.,
         source code file).

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_COMPILATION_UNIT_H
#define _WIR_COMPILATION_UNIT_H


//
// Include section
//

// Include standard headers
#include <functional>
#include <list>
#include <ostream>
#include <utility>

// Include boost headers
#include <boost/current_function.hpp>

// Include WIR headers
#include <wir/wirmisc.h>
#include <wir/API/wircontainerapi.h>
#include <wir/API/wiridapi.h>
#include <wir/API/wirinsertionapi.h>
#include <wir/API/wirlistapi.h>
#include <wir/API/wirnameapi.h>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_Data;
class WIR_Function;
class WIR_System;


/*!
  @brief Class WIR_CompilationUnit is the generic representation of code files.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_CompilationUnit final : public WIR_ID_API,
                                  public WIR_Container_API,
                                  public WIR_Name_API
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor creating an empty compilation unit.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_CompilationUnit( void );

    /*!
      @brief Default constructor creating a compilation unit containing the
             given functions.

      @tparam Args R-value references of class WIR_Function.
      @param[in] f An R-value reference to a %WIR function.
      @param[in] __args A variadic number of R-value references to %WIR
                        functions to be inserted in this compilation unit.

      This constructor uses variadic templates so that argument lists of
      arbitrary lengths can be passed to it.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    template<typename... Args>
    WIR_CompilationUnit( WIR_Function &&f, Args&&... __args ) :
      WIR_ID_API {},
      WIR_Container_API {},
      WIR_Name_API {},
      mSystemPointer { nullptr },
      mDontOptimize { false }
    {
      std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      pushBackFunction( std::move( f ) );
      addFunctions( std::forward<Args>( __args )... );
    };

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      When copying a compilation unit that is inserted in some %WIR system, the
      resulting copy will not be inserted in a system.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_CompilationUnit( const WIR_CompilationUnit & );

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      Trying to move a compilation unit that is inserted in some %WIR system
      results in an assertion, since you are not allowed to move a compilation
      unit whose ownership is managed by a system.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_CompilationUnit( WIR_CompilationUnit && );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ~WIR_CompilationUnit( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      When copying a compilation unit that is inserted in some %WIR system, the
      resulting copy will not be inserted in a system.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_CompilationUnit & operator = ( const WIR_CompilationUnit & );

    /*!
      @brief Move-assignment operator.

      @param[in] __o An R-value reference to another object to be moved.

      Trying to move a compilation unit that is inserted in some %WIR system
      results in an assertion, since you are not allowed to move a compilation
      unit whose ownership is managed by a system.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_CompilationUnit & operator = ( WIR_CompilationUnit && );


    //
    // Various lists.
    //

    // A list API for all functions managed by a %WIR compilation unit.
    WIR_LIST_DECL( WIR_Function, Function, public );

    /*!
      @brief begin returns an iterator to the first function of a compilation
             unit.

      @return A const iterator pointing to the first function.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<std::reference_wrapper<WIR_Function>>::const_iterator begin( void ) const;

    /*!
      @brief end returns an iterator to the end of a compilation unit's function
             list.

      @return A const iterator pointing to the position after the last function
              of a compilation unit.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<std::reference_wrapper<WIR_Function>>::const_iterator end( void ) const;

    /*!
      @brief pushBackData adds a new WIR_Data at the end of list mData, after
             its current last element.

      @param[in] o A const reference to the WIR_Data to be copy-added.
      @return A reference to the newly inserted element.

      @details The content of o is copied to the new list element.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Data &pushBackData( const WIR_Data & );

    /*!
      @brief pushBackData adds a new WIR_Data at the end of list mData, after
             its current last element.

      @param[in] o An R-value reference to the WIR_Data to be move-added.
      @return A reference to the newly inserted element.

      @details The content of o is moved to the new list element.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Data &pushBackData( WIR_Data && );

    /*!
      @brief pushFrontData adds a new WIR_Data at the beginning of list mData,
             right before its current first element.

      @param[in] o A const reference to the WIR_Data to be copy-added.
      @return A reference to the newly inserted element.

      @details The content of o is copied to the new list element.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Data &pushFrontData( const WIR_Data & );

    /*!
      @brief pushFrontData adds a new WIR_Data at the beginning of list mData,
             right before its current first element.

      @param[in] o An R-value reference to the WIR_Data to be move-added.
      @return A reference to the newly inserted element.

      @details The content of o is moved to the new list element.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Data &pushFrontData( WIR_Data && );

    /*!
      @brief insertData inserts a new WIR_Data before the element at the
             specified position.

      @param[in] pos An iterator denoting the position where the new element is
                     inserted.
      @param[in] o A const reference to the WIR_Data to be copy-added.
      @return An iterator pointing to the newly inserted element.

      @details The content of o is copied to the new list element.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<std::reference_wrapper<WIR_Data>>::iterator insertData( std::list<std::reference_wrapper<WIR_Data>>::const_iterator,
                                                                      const WIR_Data & );

    /*!
      @brief insertData inserts a new WIR_Data before the element at the
             specified position.

      @param[in] pos An iterator denoting the position where the new element is
                     inserted.
      @param[in] o An R-value reference to the WIR_Data to be move-added.
      @return An iterator pointing to the newly inserted element.

      @details The content of o is moved to the new list element.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<std::reference_wrapper<WIR_Data>>::iterator insertData( std::list<std::reference_wrapper<WIR_Data>>::const_iterator,
                                                                      WIR_Data && );

    /*!
      @brief replaceData replaces the list element at the specified position by
             a new WIR_Data.

      @param[in] pos An iterator pointing to the position to be replaced.
      @param[in] o A const reference to the WIR_Data to be copy-inserted.
      @return An iterator pointing to the newly inserted element.

      @details The content of o is copied into the list. The replaced element is
               immediately destroyed.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<std::reference_wrapper<WIR_Data>>::iterator replaceData( std::list<std::reference_wrapper<WIR_Data>>::const_iterator,
                                                                       const WIR_Data & );

    /*!
      @brief replaceData replaces the list element at the specified position by
             a new WIR_Data.

      @param[in] pos An iterator pointing to the position to be replaced.
      @param[in] o An R-value reference to the WIR_Data to be move-inserted.
      @return An iterator pointing to the newly inserted element.

      @details The content of o is moved into the list. The replaced element is
               immediately destroyed.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<std::reference_wrapper<WIR_Data>>::iterator replaceData( std::list<std::reference_wrapper<WIR_Data>>::const_iterator,
                                                                       WIR_Data && );

    /*!
      @brief popBackData removes the last WIR_Data from list mData.

      @details This destroys the removed element.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void popBackData( void );

    /*!
      @brief popFrontData removes the first WIR_Data from list mData.

      @details This destroys the removed element.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void popFrontData( void );

    /*!
      @brief eraseData removes a single WIR_Data from the specified position.

      @param[in] pos An iterator denoting the position where the element is
                     removed.
      @return An iterator pointing to the element following the erased element.

      @details This destroys the removed element.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<std::reference_wrapper<WIR_Data>>::iterator eraseData( std::list<std::reference_wrapper<WIR_Data>>::const_iterator );

    /*!
      @brief clearData removes all elements from list mData.

      @details This destroys all removed elements.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void clearData( void );

    /*!
      @brief getData returns the list mDataReferences.

      @return A const reference to the list mData.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const std::list<std::reference_wrapper<WIR_Data>> &getData( void ) const;

    /*!
      @brief containsData returns whether list mData contains a WIR_Data with
             the specified ID.

      @param[in] id An object's ID to be found.
      @return true if mData contains an object with the given ID, false
              otherwise.

      @note This function is rather inefficient: Its complexity is linear in the
            list's length. Thus, this method should be used with care.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool containsData( WIR_id_t ) const;

    /*!
      @brief containsData returns whether list mData contains the specified
             WIR_Data.

      @param[in] o A const reference to the WIR_Data to be found.
      @return true if mData contains the specified object, false otherwise.

      @note This function is rather inefficient: Its complexity is linear in the
            list's length. Thus, this method should be used with care.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool containsData( const WIR_Data & ) const;

    /*!
      @brief findData finds a WIR_Data with the specified ID in list mData.

      @param[in] id An object's ID to be found.
      @return An iterator pointing to the found element with the specified ID,
              or the end() iterator otherwise.

      @note This function is rather inefficient: Its complexity is linear in the
            list's length. Thus, this method should be used with care.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<std::reference_wrapper<WIR_Data>>::const_iterator findData( WIR_id_t ) const;

    /*!
      @brief findData finds the specified WIR_Data in list mData.

      @param[in] o A const reference to the WIR_Data to be found.
      @return An iterator pointing to the found element, or the end() iterator
              otherwise.

      @note This function is rather inefficient: Its complexity is linear in the
            list's length. Thus, this method should be used with care.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<std::reference_wrapper<WIR_Data>>::const_iterator findData( const WIR_Data & ) const;


    //
    // System handling.
    //

    // Realize the API to manage a compilation unit's parent system.
    WIR_INSERTION_DECL( WIR_System, System );


    //
    // Register handling.
    //

    /*!
      @brief getVREGs determines all virtual registers that occur in this
             compilation unit's functions.

      @return The set of all virtual registers.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_VirtualRegisterSet getVREGs( void ) const;


    //
    // Modification prevention.
    //

    /*!
      @brief setDontOptimize sets whether a compilation unit can be modified or
             must not be changed by some optimization or transformation.

      @param[in] f A Boolean flag denoting whether the compilation unit must not
                   be modified.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setDontOptimize( bool = true );

    /*!
      @brief getDontOptimize returns whether a compilation unit can be modified
             or must not be changed by some optimization or transformation.

      @return true if the compilation unit must not be modified, false
              otherwise.

      A compilation unit must not be modified if the compilation unit by itself
      has been marked as such using setDontOptimize, or if it is inserted into a
      %WIR system that in turn must not be modified.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool getDontOptimize( void ) const;


    //
    // Stream I/O.
    //

    /*!
      @brief The << operator dumps a %WIR compilation unit to an output stream.

      @param[in] os A reference to an output stream.
      @param[in] c A const reference to the %WIR compilation unit to be dumped.
      @return A reference to the same output stream.

      By applying processor-specific I/O manipulators to the output stream
      beforehand, this << operator can flexibly emit valid assembly output for
      arbitrary processor architectures.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    friend std::ostream & operator << ( std::ostream &,
                                        const WIR_CompilationUnit & );


  private:

    /*!
      @brief addFunctions adds the given list of functions to this compilation
             unit.

      @tparam Args R-value references of class WIR_Function.
      @param[in] f An R-value reference to a %WIR function.
      @param[in] args A variadic number of R-value references to %WIR functions.

      addFunctions uses variadic templates so that argument lists of arbitrary
      lengths can be passed to it.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    template<typename... Args>
    void addFunctions( WIR_Function &&f, Args&&... args )
    {
      std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      pushBackFunction( std::move( f ) );
      addFunctions( std::forward<Args>( args )... );
    };

    /*!
      @brief Dummy function for adding functions which does nothing.

      It only serves to terminate the recursion of the variadic method
      addFunctions.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void addFunctions( void ) const;

    /*!
      @brief copyCompilationUnit performs actions common to the copy constructor
             and copy assignment operator of %WIR compilation units.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void copyCompilationUnit( const WIR_CompilationUnit & );

    /*!
      @brief checkDontOptimize checks whether a compilation unit must not be
             modified.

      If this compilation unit must not be modified, checkDontOptimize asserts.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void checkDontOptimize( void ) const;


    //
    // Symbol handling.
    //

    /*!
      @brief For all basic blocks of the specified function and for the function
             itself, insertSymbols adds corresponding symbols to the system's
             symbol table.

      @param[in] f A const reference to a function for which symbols shall be
                   added.

      insertSymbols only creates new symbols if the current compilation unit is
      actually inserted into some %WIR system.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertSymbols( const WIR_Function & );

    /*!
      @brief For all the specified data object, insertSymbol adds a symbol to
             the system's symbol table.

      @param[in] d A const reference to a data object for which symbols shall be
                   added.

      insertSymbol only creates new symbols if the current compilation unit is
      actually inserted into some %WIR system.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertSymbol( const WIR_Data & );

    //! mData holds all stored %WIR data objects.
    std::list<WIR_Data> mData;

    /*!
      @brief mDataReferences holds (wrapped) references to all stored %WIR data
             objects.
    */
    std::list<std::reference_wrapper<WIR_Data>> mDataReferences;

    /*!
      @brief mDontOptimize stores whether a compilation unit can be modified or
             must not be changed by some optimization or transformation.
    */
    bool mDontOptimize;

};

}       // namespace WIR

#endif  // _WIR_COMPILATION_UNIT_H
