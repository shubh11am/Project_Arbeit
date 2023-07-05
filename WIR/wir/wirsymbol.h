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
  @file wirsymbol.h
  @brief This file provides the interface of %WIR symbols.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_SYMBOL_H
#define _WIR_SYMBOL_H


//
// Include section
//

// Include standard headers
#include <cstddef>
#include <string>

// Include WIR headers
#include <wir/API/wiridapi.h>
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
class WIR_ID_API;
class WIR_Section;


/*!
  @brief Class WIR_Symbol models %WIR symbols, i.e., (global) variables,
         functions or basic blocks.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_Symbol : public WIR_ID_API
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor creating a basic block symbol.

      @param[in] b A const reference to a %WIR basic block for which a symbol is
                   created.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_Symbol( const WIR_BasicBlock & );

    /*!
      @brief Default constructor creating a data object symbol.

      @param[in] d A const reference to a %WIR data object for which a symbol is
                   created.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_Symbol( const WIR_Data & );

    /*!
      @brief Default constructor creating a function symbol.

      @param[in] f A const reference to a %WIR function for which a symbol is
                   created.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_Symbol( const WIR_Function & );

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Symbol( const WIR_Symbol & );

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Symbol( WIR_Symbol && );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_Symbol( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Symbol & operator = ( const WIR_Symbol & );

    /*!
      @brief Move-assignment operator.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Symbol & operator = ( WIR_Symbol && );


    //
    // Type handling.
    //

    /*!
      @brief getType returns to which kind of entity a symbol refers, i.e.,
             whether a symbol represents a %WIR basic block, data or a function
             etc.

      @return The kind of a symbol as expressed by the enumeration class
              WIR_SymbolType.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_SymbolType getType( void ) const;

    /*!
      @brief In case the symbol refers to a %WIR basic block, getBasicBlock
             returns the represented block.

      @return A reference to the represented basic block.

      If the symbol does not refer to a basic block, getBasicBlock fails with an
      assertion.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_BasicBlock &getBasicBlock( void ) const;

    /*!
      @brief In case the symbol refers to a %WIR data object, getData returns
             the represented data.

      @return A reference to the represented data object.

      If the symbol does not refer to a data object, getData fails with an
      assertion.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Data &getData( void ) const;

    /*!
      @brief In case the symbol refers to a %WIR function, getFunction returns
             the represented function.

      @return A reference to the represented function.

      If the symbol does not refer to a function, getFunction fails with an
      assertion.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Function &getFunction( void ) const;


    //
    // Name handling.
    //

    /*!
      @brief getName returns a symbol's specific name.

      @return A string that holds the name of the basic block/data/function that
              the symbol refers to.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::string getName( void ) const;


    //
    // Symbol properties.
    //

    /*!
      @brief setConst sets whether a symbol is constant/read-only or not.

      @param[in] b A Boolean denoting whether the symbol is constant or not.

      If the object refered by the symbol must not be modified, setConst
      asserts.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setConst( bool = true );

    /*!
      @brief isConst returns whether a symbol is constant/read-only or not.

      @return true iff the symbol is constant, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isConst( void ) const;

    /*!
      @brief setExtern sets whether a symbol is external or not.

      @param[in] b A Boolean denoting whether the symbol is external or not.

      If the object refered by the symbol must not be modified, setExtern
      asserts.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setExtern( bool = true );

    /*!
      @brief isExtern returns whether a symbol is external or not.

      @return true iff the symbol is external, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isExtern( void ) const;

    /*!
      @brief setGlobal sets whether a symbol is global or not.

      @param[in] b A Boolean denoting whether the symbol is global or not.

      If the object refered by the symbol must not be modified, setGlobal
      asserts.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setGlobal( bool = true );

    /*!
      @brief isGlobal returns whether a symbol is global or not.

      @return true iff the symbol is global, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isGlobal( void ) const;

    /*!
      @brief setVolatile sets whether a symbol is volatile or not.

      @param[in] b A Boolean denoting whether the symbol is volatile or not.

      If the object refered by the symbol must not be modified, setVolatile
      asserts.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setVolatile( bool = true );

    /*!
      @brief isVolatile returns whether a symbol is volatile or not.

      @return true iff the symbol is volatile, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isVolatile( void ) const;


    //
    // Memory layout properties.
    //

    /*!
      @brief setSection sets the ELF executable file section into which the
             symbol is assembled.

      @param[in] s A const reference to a %WIR section to which this symbol is
                   bound.

      The assignment of a symbol to a section is done according to the following
      rules:
      -# If the symbol refers to a function, the section assignment of all basic
         blocks of that function is unset.
      -# If the symbol refers to a basic block, the section assignment of that
         block's function is unset. Section assignments of all other basic
         blocks of that very same function remain unchanged.
      -# Finally, the current symbol itself is assigned to the specified
         section.
      This way, section assignments for code only exist either for a function or
      for a function's basic blocks or for none of them, but never for both.

      If the object refered by the symbol must not be modified, setSection
      asserts.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setSection( const WIR_Section & );

    /*!
      @brief unsetSection removes any prior explicit assignment of a symbol to
             an ELF executable file section.

      If the object refered by the symbol must not be modified, unsetSection
      asserts.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void unsetSection( void );

    /*!
      @brief getSection returns the ELF executable file section into which the
             symbol is assembled.

      @return A reference to the %WIR section to which this symbol is bound.

      If the symbol has been assigned to a section explicitly before using
      setSection, this explicitly specified section is returned.

      If the symbol refers to a basic block and the block's function symbol has
      been assigned to a section, the function's section is returned.

      If the symbol refers to a function and that function's basic blocks have
      been assigned to sections, the section of the very first basic block of
      the function is returned.

      If no explicit section assignment exists, a default section is determined
      and returned by getSection according to the following rules:
      -# If the symbol refers to a function or a basic block, the default
         section ".text" of the first processor core of the %WIR system is
         returned.
      -# Otherwise, the symbol refers to a data object. In this case,
         -# section ".rodata" of the first processor core of the %WIR system is
            returned if the symbol is constant,
         -# else section ".data" of the first processor core of the %WIR system
            is returned if the data object is initialized,
         -# else section ".bss" of the first processor core of the %WIR system
            is returned.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const WIR_Section &getSection( void ) const;

    /*!
      @brief getBaseAddress returns a symbol's base address.

      @return The symbol's physical start address.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_MemoryAddress getBaseAddress( void ) const;


  private:

    friend class WIR_System;

    /*!
      @brief setBaseAddress sets a symbol's base address.

      @param[in] s A const reference to the symbol's physical start address.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setBaseAddress( const WIR_MemoryAddress & );

    /*!
      @brief checkDontOptimize checks whether the object to which a symbol
             refers must not be modified.

      If the refered object must not be modified, checkDontOptimize asserts.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void checkDontOptimize( void ) const;

    /*!
      @brief mSymbolType stores to which kind of entity a symbol referes, i.e.,
             whether a symbol represents a %WIR basic block, data or function.
    */
    WIR_SymbolType mSymbolType;

    /*!
      @brief mReferedID points the %WIR object that a symbol represents.
    */
    WIR_ID_API *mReferedID;

    //! mConst stores whether a symbol is constant/read-only.
    bool mConst;

    //! mExtern stores whether a symbol is external.
    bool mExtern;

    //! mGlobal stores whether a symbol is global.
    bool mGlobal;

    //! mVolatile stores whether a symbol is volatile.
    bool mVolatile;

    /*!
      @brief mSection points to the %WIR ELF executable file section into which
             the symbol is assembled.
    */
    WIR_Section *mSection;

    /*!
      @brief mBaseAddress stores a symbol's base address.

      The base address denotes the physical start address of a symbol.
    */
    WIR_MemoryAddress mBaseAddress;

};

}       // namespace WIR

#endif  // _WIR_SYMBOL_H
