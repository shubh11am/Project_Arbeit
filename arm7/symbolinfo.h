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


#ifndef _SYMBOLINFO_ARM7_H
#define _SYMBOLINFO_ARM7_H


// Include standard headers
#include <cstring>
#include <map>
#include <iostream>
#include <string>


//!  The class SymbolInfo represents a symbol (variable).
/*!
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
    SymbolInfo();

    //! Constructor with parameters.
    SymbolInfo( Type symbolType, int, int, const std::string&,
                const std::string&, const std::string&, bool, bool );

    //! Copy constructor.
    SymbolInfo( const SymbolInfo & );

    //! Default Destructor.
    ~SymbolInfo();

    //! Default copy-assignment operator.
    SymbolInfo & operator = ( const SymbolInfo & ) = default;

    //! getSymbolType returns the type of the symbol.
    enum Type getSymbolType() const;

    //! setSymbolType sets the type of the symbol.
    void setSymbolType( enum Type );

    //! getSymbolOffset returns the offset.
    int getSymbolOffset() const;

    //! setSymbolOffset( int ) sets the attribute mOffset.
    void setSymbolOffset( int );

    //! getArgumentPos returns the position of the argument if the argument is a function argument, otherwise -1.
    int getSymbolArgumentPos() const;

    //! setArgumentPos sets the attribute mArgNum.
    void setSymbolArgumentPos( int );

    //! getSymbolReg returns the LLIR register name.
    std::string getSymbolReg() const;

    //! setSymbolReg sets the LLIR register name.
    void setSymbolReg( const std::string & );

    //! getInternalVReg returns the attribute mInternalVReg.
    std::string getInternalVReg() const;

    //! setInternalVReg sets the attribute mInternalVReg.
    void setInternalVReg( const std::string & );

    //! getAddrReg returns the LLIR address register name.
    std::string getAddrReg() const;

    //! setAddrReg sets the LLIR address register name.
    void setAddrReg( const std::string & );

    //! getAddressTaken returns whether the address of the current symbol was taken.
    /*!
      This information is valid only for symbols being function arguments.
    */
    bool getAddressTaken() const;

    //! setAddressTaken sets whether the address of the current symbol was taken.
    /*!
      This information is valid only for symbols being function arguments.
    */
    void setAddressTaken( bool );

    //! getStoreInstructionsGenerated returns the attribute mStoreInstructionsGenerated.
    bool getStoreInstructionsGenerated() const;

    //! setStoreInstructionsGenerated sets the attribute mStoreInstructionsGenerated.
    void setStoreInstructionsGenerated( bool );

    //! print prints out the contents of this class to stdout.
    void print() const;


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

    //! mAddressTaken indicates whether the address of the current symbol is taken.
    bool mAddressTaken;

    //! mStoreInstructionsGenerated indicates whether required ST instructions are already generated during code selection at the very beginning of a LLIR function for function arguments passed via registers, whose address is taken.
    bool mStoreInstructionsGenerated;

};


#endif  // _SYMBOLINFO_ARM7_H
