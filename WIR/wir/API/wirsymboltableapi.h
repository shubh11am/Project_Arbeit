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
  @file wirsymboltableapi.h
  @brief This file provides the interface of a base class for managing symbol
         tables in derived classes.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_SYMBOLTABLE_API_H
#define _WIR_SYMBOLTABLE_API_H


//
// Include section
//

// Include standard headers
#include <functional>
#include <map>
#include <set>
#include <string>

// Include WIR headers
#include <wir/wirsymbol.h>
#include <wir/wirtypes.h>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_BasicBlock;
class WIR_Data;
class WIR_Function;
class WIR_Symbol;


/*!
  @brief Class WIR_SymbolTable_API provides a simple API to retrieve symbol
         tables from %WIR objects.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_SymbolTable_API
{

  public:

    //
    // Symbol handling.
    //

    /*!
      @brief getSymbols returns all currently managed symbols.

      @return A const reference to set mSymbols.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const WIR_SymbolSet &getSymbols( void ) const;

    /*!
      @brief containsSymbol returns whether the symbol table contains a
             WIR_Symbol for some basic block, data or function object having the
             specified ID.

      @param[in] id A basic block/data/function's ID to be found.
      @return true if mSymbolReferences contains the specified symbol, false
              otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool containsSymbol( WIR_id_t ) const;

    /*!
      @brief containsSymbol returns whether set mSymbolReferences contains the
             specified WIR_Symbol.

      @param[in] s A const reference to the symbol to be found.
      @return true if mSymbolReferences contains the specified symbol, false
              otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool containsSymbol( const WIR_Symbol & ) const;

    /*!
      @brief containsSymbol returns whether this symbol table contains a symbol
             for the specified %WIR basic block.

      @param[in] b A const reference to the basic block to be found.
      @return true if the symbol table contains the specified basic block, false
              otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool containsSymbol( const WIR_BasicBlock & ) const;

    /*!
      @brief containsSymbol returns whether this symbol table contains a symbol
             for the specified %WIR data object.

      @param[in] d A const reference to the data object to be found.
      @return true if the symbol table contains the specified data object, false
              otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool containsSymbol( const WIR_Data & ) const;

    /*!
      @brief containsSymbol returns whether this symbol table contains a symbol
             for the specified %WIR function.

      @param[in] f A const reference to the function to be found.
      @return true if the symbol table contains the specified function, false
              otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool containsSymbol( const WIR_Function & ) const;

    /*!
      @brief containsSymbol returns whether this symbol table contains a symbol
             with the given name.

      @param[in] s A const reference to a string with the symbol's name to be
                   looked up.
      @return true if the symbol table contains a symbol with the specified
              name, false otherwise.

      @note This function is rather inefficient: Its complexity is linear in the
            list's length. Thus, this method should be used with care.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool containsSymbol( const std::string & ) const;

    /*!
      @brief findSymbol finds a WIR_Symbol for some basic block, data or
             function object having the specified ID.

      @param[in] id A basic block/data/function's ID to be found.
      @return A reference to the found symbol.

      findSymbol asserts if the symbol table does not contain an object with the
      specified ID.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Symbol &findSymbol( WIR_id_t ) const;

    /*!
      @brief findSymbol finds the symbol for the specified %WIR basic block.

      @param[in] b A const reference to the basic block to be found.
      @return A reference to the found symbol.

      findSymbol asserts if the symbol table does not contain the specified
      basic block.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Symbol &findSymbol( const WIR_BasicBlock & ) const;

    /*!
      @brief findSymbol finds the symbol for the specified %WIR data object.

      @param[in] d A const reference to the data object to be found.
      @return A reference to the found symbol.

      findSymbol asserts if the symbol table does not contain the specified data
      object.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Symbol &findSymbol( const WIR_Data & ) const;

    /*!
      @brief findSymbol finds the symbol for the specified %WIR function.

      @param[in] f A const reference to the function to be found.
      @return A reference to the found symbol.

      findSymbol asserts if the symbol table does not contain the specified
      function.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Symbol &findSymbol( const WIR_Function & ) const;

    /*!
      @brief findSymbol finds the symbol with the given name.

      @param[in] s A const reference to a string with the symbol's name to be
                   found.
      @return A reference to the found symbol.

      findSymbol asserts if the symbol table does not contain a symbol with
      the specified name.

      @note This function is rather inefficient: Its complexity is linear in the
            list's length. Thus, this method should be used with care.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Symbol &findSymbol( const std::string & ) const;

    /*!
      @brief findSymbol finds the symbol with the given address.

      @param[in] a A const reference to a memory address to be found.
      @return A reference to a symbol covering the given memory address, i.e.,
              the given address lies within the interval between the symbol's
              base address and its size.

      If the given memory address is covered by both a function and a basic
      block symbol, findSymbol returns the basic block's symbol. findSymbol
      asserts if the symbol table does not contain a symbol covering the given
      memory address.

      @note This function is rather inefficient: Its complexity is linear in the
            list's length. Thus, this method should be used with care.
    */
    WIR_Symbol &findSymbol( const WIR_MemoryAddress & ) const;


  protected:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor assigning an empty symbol table.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_SymbolTable_API( void );

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_SymbolTable_API( const WIR_SymbolTable_API & );

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_SymbolTable_API( WIR_SymbolTable_API && );

    /*!
      @brief Destructor.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_SymbolTable_API( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_SymbolTable_API & operator = ( const WIR_SymbolTable_API & );

    /*!
      @brief Move-assignment operator.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_SymbolTable_API & operator = ( WIR_SymbolTable_API && );


    //
    // Symbol handling.
    //

    /*!
      @brief insertSymbol inserts a new WIR_Symbol into the symbol table.

      @param[in] s A const reference to the symbol to be copy-added.
      @return A reference to the newly inserted element.

      insertSymbol asserts if a symbol for an already contained function or
      basic block is inserted. The content of s is copied to the new set
      element.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Symbol &insertSymbol( const WIR_Symbol & );

    /*!
      @brief insertSymbol inserts a new WIR_Symbol into the symbol table.

      @param[in] s An R-value reference to the symbol to be move-added.
      @return A reference to the newly inserted element.

      insertSymbol asserts if a symbol for an already contained function or
      basic block is inserted. The content of s is copied to the new set
      element.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Symbol &insertSymbol( WIR_Symbol && );

    /*!
      @brief insertSymbol inserts a new WIR_Symbol for the specified basic block
             into the symbol table.

      @param[in] b A const reference to the basic block for which a symbol is
                   inserted.
      @return A reference to the newly inserted symbol.

      insertSymbol asserts if a symbol for an already contained basic block is
      inserted.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Symbol &insertSymbol( const WIR_BasicBlock & );

    /*!
      @brief insertSymbol inserts a new WIR_Symbol for the specified data object
             into the symbol table.

      @param[in] d A const reference to the data object for which a symbol is
                   inserted.
      @return A reference to the newly inserted symbol.

      insertSymbol asserts if a symbol for an already contained data object is
      inserted.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Symbol &insertSymbol( const WIR_Data & );

    /*!
      @brief insertSymbol inserts a new WIR_Symbol for the specified function
             into the symbol table.

      @param[in] f A const reference to the function for which a symbol is
                   inserted.
      @return A reference to the newly inserted symbol.

      insertSymbol asserts if a symbol for an already contained function is
      inserted.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Symbol &insertSymbol( const WIR_Function & );

    /*!
      @brief eraseSymbol removes the symbol for the specified %WIR basic block.

      @param[in] b A const reference to the basic block whose symbol shall be
                   removed.
      @return An iterator pointing to the element following the erased element.

      This destroys the removed element.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_SymbolSet::iterator eraseSymbol( const WIR_BasicBlock & );

    /*!
      @brief eraseSymbol removes the symbol for the specified %WIR data object.

      @param[in] d A const reference to the data object whose symbol shall be
                   removed.
      @return An iterator pointing to the element following the erased element.

      This destroys the removed element.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_SymbolSet::iterator eraseSymbol( const WIR_Data & );

    /*!
      @brief eraseSymbol removes the symbol for the specified %WIR function.

      @param[in] f A const reference to the function whose symbol shall be
                   removed.
      @return An iterator pointing to the element following the erased element.

      This destroys the removed element.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_SymbolSet::iterator eraseSymbol( const WIR_Function & );

    /*!
      @brief clearSymbols removes all elements from set mSymbols.

      This destroys all removed elements.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void clearSymbols( void );

    /*!
      @brief invalidateSymbols marks the information computed for all symbols as
             invalid.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void invalidateSymbols( void );

    /*!
      @brief mSymbolsDirty stores whether the information stored in the table's
             symbols are dirty or valid.
    */
    bool mSymbolsDirty;


  private:

    friend class WIR_BasicBlock;
    friend class WIR_CompilationUnit;
    friend class WIR_Data;
    friend class WIR_Function;
    friend class WIR_Instruction;

    /*!
      @brief getSymbolID returns the ID of the basic block, function or data
             object that is represented by the specified symbol.

      @param[in] s A const reference to a symbol.
      @return nullid if the given symbol is not part of this symbol table, the
              ID of the object represented by the given symbol otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_id_t getSymbolID( const WIR_Symbol & ) const;

    /*!
      @brief checkDuplicates verifies that the object represented by the
             specified symbol is not yet part of this symbol table.

      @param[in] s A const reference to a symbol to be checked for duplicates.
      @return The ID of the object represented by the given symbol.

      If the specified symbol is indeed a duplicate, checkDuplicates terminates
      with an assertion.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_id_t checkDuplicates( const WIR_Symbol & ) const;

    /*!
      @brief copySymbolTable performs actions common to the copy constructor and
             copy assignment operator of %WIR symbol tables.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void copySymbolTable( const WIR_SymbolTable_API & );

    //! mSymbols holds all stored %WIR symbols.
    std::set<WIR_Symbol> mSymbols;

    //! mSymbolReferences holds (wrapped) references to all stored %WIR symbols.
    WIR_SymbolSet mSymbolReferences;

    /*!
      @brief mID2Symbol maps the IDs of a basic block, data or a function to its
             corresponding symbol.
    */
    std::map<WIR_id_t, std::reference_wrapper<WIR_Symbol>> mID2Symbol;

};

}       // namespace WIR

#endif  // _WIR_SYMBOLTABLE_API_H
