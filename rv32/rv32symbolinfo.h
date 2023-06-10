/*

   This header file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Compiler Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2021 - 2022

   Hamburg University of Technology (TUHH)
   Institute of Embedded Systems
   21071 Hamburg
   Germany

   http://www.tuhh.de/es/esd/research/wcc

*/

/*!
  @file rv32symbolinfo.h
  @brief This file provides the interface of the RISC-V RV32 class modeling
         symbols and their storage properties.
*/


#ifndef _RV32_SYMBOLINFO_H
#define _RV32_SYMBOLINFO_H


//
// Class forward declarations
//

namespace WIR {
class WIR_VirtualRegister;
class RV_RegV;
}


//
// Header section
//

namespace RV32 {

/*!
  @brief Class SymbolInfo represents a symbol (variable).

  It comprises various information used when dealing with function arguments,
  structs/unions and arrays.
*/
class RV32_SymbolInfo
{

  public:

    //
    // Local type definitions.
    //

    //! Enumeration for symbol type classification.
    enum class Type : char
    {
      //! Represents a function argument that might be passed via a REG.
      R_ARGUMENT,

      //! Represents a local variable stored as virtual register.
      LOCAL_VAR,

      //! Represents a local variable stored explicitly on the stack.
      LOCAL_STACK_VAR
    };


    //
    // Constructors and destructors.
    //

    /*!
      @brief Constructor creating a default symbol.
    */
    RV32_SymbolInfo( void );

    /*!
      @brief Constructor creating a symbol of specific type.

      @param[in] symbolType A specifier denoting a symbol's type.
      @param[in] argPos An integer denoting the actual position of a function
                        argument.
      @param[in] offset An integer denoting a symbol's position on the stack.
      @param[in] addressTaken A Boolean denoting whether the address of a symbol
                              is taken or not.
      @param[in] stInsGenerated A Boolean denoting whether store instructions
                                are already generated at the beginning of a
                                function, for function arguments passed via
                                registers whose address is taken.
    */
    RV32_SymbolInfo( Type symbolType, int, int, bool, bool );

    /*!
      @brief Copy constructor.

      @param[in] st A const reference to another object to be copied.
    */
    RV32_SymbolInfo( const RV32_SymbolInfo & );

    /*!
      @brief Destructor.
    */
    ~RV32_SymbolInfo( void );

    /*!
      @brief Default copy-assignment operator.
    */
    RV32_SymbolInfo & operator = ( const RV32_SymbolInfo & ) = default;


    //
    // Symbol handling.
    //

    /*!
      @brief setSymbolType sets the type of the symbol.

      @param[in] type A specifier for a symbol's type.
    */
    void setSymbolType( enum Type );

    /*!
      @brief getSymbolType returns the type of the symbol.

      @return The symbol's type.
    */
    enum Type getSymbolType( void ) const;

    /*!
      @brief setArgumentPos sets the position of a symbol if it is used as
             function argument.

      @param[in] argPos An integer denoting the actual position of a function
                        argument.
    */
    void setSymbolArgumentPos( int );

    /*!
      @brief getSymbolArgumentPos returns the position of a symbol if it is used
             as function argument.

      @return An integer denoting the symbol's position if it is a function
              argument, or -1 otherwise.
    */
    int getSymbolArgumentPos( void ) const;

    /*!
      @brief setSymbolOffset sets a symbol's position on the stack.

      @param[in] offset An integer denoting a symbol's position on the stack.
    */
    void setSymbolOffset( int );

    /*!
      @brief getSymbolOffset returns a symbol's position on the stack.

      @return An integer denoting the symbol's stack offset.
    */
    int getSymbolOffset( void ) const;

    /*!
      @brief setSymbolReg associates the given virtual %WIR register with a
             symbol.

      @param[in] r A const reference to a virtual %WIR register.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setSymbolReg( const WIR::WIR_VirtualRegister & );

    /*!
      @brief getSymbolReg returns the virtual %WIR register associated with a
             symbol.

      @return A reference to the associated virtual %WIR register.

      getSymbolReg fails with an assertion if no %WIR register has previously
      been associated with a symbol.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR::WIR_VirtualRegister &getSymbolReg( void ) const;

    /*!
      @brief isSymbolRegSet returns whether a %WIR register has previously been
             associated with a symbol.

      @return true if a register is associated, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isSymbolRegSet( void ) const;

    /*!
      @brief setInternalVReg associates the given virtual %WIR register as
             function-internal register of a symbol.

      @param[in] r A const reference to a virtual %WIR register.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setInternalVReg( const WIR::WIR_VirtualRegister & );

    /*!
      @brief getInternalVReg returns the function-internal virtual %WIR register
             associated with a symbol.

      @return A reference to the associated virtual %WIR register.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR::WIR_VirtualRegister &getInternalVReg( void ) const;

    /*!
      @brief isInternalVRegSet returns whether a function-internal virtual %WIR
             register has previously been associated with a symbol.

      @return true if a register is associated, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isInternalVRegSet( void ) const;

    /*!
      @brief setAddrReg associates the given virtual RISC-V register as address
             register of a symbol.

      @param[in] r A const reference to a virtual RISC-V register.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setAddrReg( const WIR::RV_RegV & );

    /*!
      @brief getAddrReg returns the virtual RISC-V address register associated
             with a symbol.

      @return A reference to the associated virtual address register.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR::RV_RegV &getAddrReg( void ) const;

    /*!
      @brief isAddrRegSet returns whether a RISC-V address register has
             previously been associated with a symbol.

      @return true if a register is associated, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isAddrRegSet( void ) const;

    /*!
      @brief setAddressTaken sets whether the address of the current symbol was
             taken.

      @param[in] addressTaken A Boolean denoting whether the address of a symbol
                              is taken or not.

      This information is valid only for symbols being function arguments.
    */
    void setAddressTaken( bool );

    /*!
      @brief isAddressTaken returns whether the address of a symbol was taken.

      @return true if a symbol's address was taken, false otherwise.

      This information is valid only for symbols being function arguments.
    */
    bool isAddressTaken( void ) const;

    /*!
      @brief setStoreInstructionsGenerated sets whether store instructions are
             already generated for a symbol at the beginning of a function.

      @param[in] stInsGenerated A Boolean denoting whether store instructions
                                are already generated at the beginning of a
                                function, for function arguments passed via
                                registers whose address is taken.

      This information is valid only for symbols being function arguments, whose
      address have been taken.
    */
    void setStoreInstructionsGenerated( bool );

    /*!
      @brief areStoreInstructionsGenerated returns whether store instructions
             are already generated for a symbol at the beginning of a function.

      @return true if store instructions are already generated at the beginning
              of a function, false otherwise.

      This information is valid only for symbols being function arguments, whose
      address have been taken.
    */
    bool areStoreInstructionsGenerated( void ) const;

    /*!
      @brief print prints out the contents of this class to stdout.
    */
    void print( void ) const;

  private:

    //! mSymbolType holds the type of the symbol.
    enum Type mSymbolType;

    //! mArgPos holds the position of a symbol if it is a function argument.
    int mArgPos;

    //! mOffset holds the current position of the symbol on the stack.
    int mOffset;

    //! mReg points to the %WIR register assigned to this symbol.
    WIR::WIR_VirtualRegister *mReg;

    /*!
      @brief mVReg points to the virtual %WIR register assigned to a function
             argument.
    */
    WIR::WIR_VirtualRegister *mVReg;

    /*!
      @brief mAReg points to the virtual %WIR address register assigned to this
             symbol.
    */
    WIR::RV_RegV *mAReg;

    //! mAddressTaken stores whether the address of the current symbol is taken.
    bool mAddressTaken;

    /*!
      @brief mStoreInstructionsGenerated indicates whether required ST
             instructions are already generated during code selection at the
             very beginning of a WIR function for function arguments passed via
             registers, whose address is taken.
    */
    bool mStoreInstructionsGenerated;

};

}

#endif  // _RV32_SYMBOLINFO_H
