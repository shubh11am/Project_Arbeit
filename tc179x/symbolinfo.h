/*

   This header file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Compiler Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2005 - 2022

   Hamburg University of Technology (TUHH)
   Institute of Embedded Systems
   21071 Hamburg
   Germany

   http://www.tuhh.de/es/esd/research/wcc

*/


#ifndef _SYMBOLINFO_TC179X_H
#define _SYMBOLINFO_TC179X_H


// Include standard headers
#include <string>


//
// Class forward declarations
//

namespace WIR {
class WIR_VirtualRegister;
class TC_ARegV;
}


/*!
  @brief Class SymbolInfo represents a symbol (variable).

  It comprises various information used when dealing with function arguments,
  structs/unions and arrays.
*/
class SymbolInfo
{
  public:

    //! Enumeration for symbol type classification.
    enum Type
    {
      //! Represents a function argument that might be passed via a DREG.
      D_ARGUMENT,

      //! Represents a function argument that might be passed via an AREG.
      A_ARGUMENT,

      //! Represents a local variable stored as virtual register.
      LOCAL_VAR,

      //! Represents a local variable stored explicitly on the stack.
      LOCAL_STACK_VAR,

      //! Represents a local composed type passed as function argument
      LOCAL_COMPOSED_ARGUMENT
    };

    //! Constructor creating a default symbol.
    SymbolInfo( void );

    //! Constructor with parameters.
    SymbolInfo( Type symbolType, int, int, const std::string &,
                const std::string &, const std::string &, bool, bool );

    //! Copy constructor.
    SymbolInfo( const SymbolInfo & );

    //! Destructor.
    ~SymbolInfo( void );

    //! Default copy-assignment operator.
    SymbolInfo & operator = ( const SymbolInfo & ) = default;

    //! getSymbolType returns the type of the symbol.
    enum Type getSymbolType( void ) const;

    //! setSymbolType sets the type of the symbol.
    void setSymbolType( enum Type );

    //! getSymbolOffset returns the offset.
    int getSymbolOffset( void ) const;

    //! setSymbolOffset( int ) sets the attribute mOffset.
    void setSymbolOffset( int );

    //! getArgumentPos returns the position of the argument if the argument is a function argument, otherwise -1.
    int getSymbolArgumentPos( void ) const;

    //! setArgumentPos sets the attribute mArgNum.
    void setSymbolArgumentPos( int );

    //! getSymReg returns the LLIR register name.
    std::string getSymReg( void ) const;

    //! setSymbolReg sets the LLIR register name.
    void setSymbolReg( const std::string & );

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

    //! getIntVReg returns the attribute mInternalVReg.
    std::string getIntVReg( void ) const;

    //! setInternalVReg sets the attribute mInternalVReg.
    void setInternalVReg( const std::string & );

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

      getInternalVReg fails with an assertion if no %WIR register has previously
      been associated with a symbol.

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

    //! getAdrReg returns the LLIR address register name.
    std::string getAdrReg( void ) const;

    //! setAddrReg sets the LLIR address register name.
    void setAddrReg( const std::string & );

    /*!
      @brief setAddrReg associates the given virtual TriCore register as address
             register of a symbol.

      @param[in] r A const reference to a virtual TriCore address register.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setAddrReg( const WIR::TC_ARegV & );

    /*!
      @brief getAddrReg returns the virtual TriCore address register associated
             with a symbol.

      @return A reference to the associated virtual address register.

      getAddrReg fails with an assertion if no %WIR address register has
      previously been associated with a symbol.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR::TC_ARegV &getAddrReg( void ) const;

    /*!
      @brief isAddrRegSet returns whether a TriCore address register has
             previously been associated with a symbol.

      @return true if a register is associated, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isAddrRegSet( void ) const;

    //! getAddressTaken returns whether the address of the current symbol was taken.
    /*!
      This information is valid only for symbols being function arguments.
    */
    bool getAddressTaken( void ) const;

    //! setAddressTaken sets whether the address of the current symbol was taken.
    /*!
      This information is valid only for symbols being function arguments.
    */
    void setAddressTaken( bool );

    //! getStoreInstructionsGenerated returns the attribute mStoreInstructionsGenerated.
    bool getStoreInstructionsGenerated( void ) const;

    //! setStoreInstructionsGenerated sets the attribute mStoreInstructionsGenerated.
    void setStoreInstructionsGenerated( bool );

    //! print prints out the contents of this class to stdout.
    void print( void ) const;


  private:

    //! mSymbolType represents the type of the symbol.
    enum Type mSymbolType;

    //! When dealing with function arguments, the actual position is required.
    int mArgNum;

    //! mOffset represents the current position of the symbol on the stack.
    int mOffset;

    //! mLlirReg indicates the LLIR register name assigned to this symbol.
    std::string mLlirReg;

    //! mInternalVReg represents the LLIR register name assigned to a function argument within a function.
    std::string mInternalVReg;

    //! mAddrReg indicates the virtual address register name assigned to this symbol.
    std::string mAddrReg;

    //! mReg points to the %WIR data register assigned to this symbol.
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
    WIR::TC_ARegV *mAReg;

    //! mAddressTaken indicates whether the address of the current symbol is taken.
    bool mAddressTaken;

    //! mStoreInstructionsGenerated indicates whether required ST instructions are already generated during code selection at the very beginning of a LLIR function for function arguments passed via registers, whose address is taken.
    bool mStoreInstructionsGenerated;

};


#endif  // _SYMBOLINFO_TC179X_H
