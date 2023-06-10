/*

   This source file belongs to the

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
  @file rv32symbolinfo.cc
  @brief This file implements the RISC-V RV32 class modeling symbols and their
         storage properties.
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wcc.h>
#endif

// Include standard headers
#include <iostream>

// Include boost headers
#include <boost/current_function.hpp>

// Include WIR headers
#include <wir/wir.h>
#include <arch/riscv/rv32imc.h>

// Include libuseful headers
#include <libuseful/debugmacros.h>

// Include local headers
#include <rv32/rv32symbolinfo.h>


//
// Code section
//

using namespace std;
using namespace WIR;


namespace RV32 {


//
// Public class methods
//

/*
  Constructor creating a default symbol.
*/
RV32_SymbolInfo::RV32_SymbolInfo( void ) :
  mSymbolType { Type::LOCAL_VAR },
  mArgPos { 0 },
  mOffset { 0 },
  mReg { nullptr },
  mVReg { nullptr },
  mAReg { nullptr },
  mAddressTaken( false ),
  mStoreInstructionsGenerated( false )
{
  DSTART( "RV32_SymbolInfo::RV32_SymbolInfo()" );
};


/*
  Constructor creating a symbol of specific type.
*/
RV32_SymbolInfo::RV32_SymbolInfo( Type symbolType, int argPos, int offset,
                                  bool addressTaken, bool stInsGenerated ) :
  mSymbolType { symbolType },
  mArgPos { argPos },
  mOffset { offset },
  mReg { nullptr },
  mVReg { nullptr },
  mAReg { nullptr },
  mAddressTaken { addressTaken },
  mStoreInstructionsGenerated { stInsGenerated }
{
  DSTART(
    "RV32_SymbolInfo::RV32_SymbolInfo(RV32_SymbolInfo::Type, int, int, bool, "
    "bool)" );
};


/*
  Copy constructor.
*/
RV32_SymbolInfo::RV32_SymbolInfo( const RV32_SymbolInfo &st ) :
  mSymbolType { st.mSymbolType },
  mArgPos { st.mArgPos },
  mOffset { st.mOffset },
  mReg { st.mReg },
  mVReg { st.mVReg },
  mAReg { st.mAReg },
  mAddressTaken { st.mAddressTaken },
  mStoreInstructionsGenerated { st.mStoreInstructionsGenerated }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Destructor.
*/
RV32_SymbolInfo::~RV32_SymbolInfo( void )
{
  DSTART( "RV32_SymbolInfo::~RV32_SymbolInfo()" );
};

/*
  setSymbolType sets the type of the symbol.
*/
void RV32_SymbolInfo::setSymbolType( enum Type type )
{
  DSTART( "void RV32_SymbolInfo::setSymbolType(RV32_SymbolInfo::Type)" );

  mSymbolType = type;
};


/*
  getSymbolType returns the type of the symbol.
*/
enum RV32_SymbolInfo::Type RV32_SymbolInfo::getSymbolType( void ) const
{
  DSTART( "RV32_SymbolInfo::Type RV32_SymbolInfo::getSymbolType() const" );

  return( mSymbolType );
};


/*
  setArgumentPos sets the position of a symbol if it is used as function
  argument.
*/
void RV32_SymbolInfo::setSymbolArgumentPos( int argPos )
{
  DSTART( "void RV32_SymbolInfo::setSymbolArgumentPos(int)" );

  mArgPos = argPos;
};


/*
  getSymbolArgumentPos returns the position of a symbol if it is used as
  function argument.
*/
int RV32_SymbolInfo::getSymbolArgumentPos( void ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  if ( ( mSymbolType == Type::R_ARGUMENT ) ||
       // For argument symbols whose address was taken.
       ( mSymbolType == Type::LOCAL_STACK_VAR ) )
    return( mArgPos );
  else
    return( -1 );
};


/*
  setSymbolOffset sets a symbol's position on the stack.
*/
void RV32_SymbolInfo::setSymbolOffset( int offset )
{
  DSTART( "void RV32_SymbolInfo::setSymbolOffset(int)" );

  mOffset = offset;
};


/*
  getSymbolOffset returns a symbol's position on the stack.
*/
int RV32_SymbolInfo::getSymbolOffset( void ) const
{
  DSTART( "int RV32_SymbolInfo::getSymbolOffset() const" );

  return( mOffset );
};


/*
  setSymbolReg associates the given virtual WIR register with a symbol.
*/
void RV32_SymbolInfo::setSymbolReg( const WIR::WIR_VirtualRegister &r )
{
  DSTART( "void RV32_SymbolInfo::setSymbolReg(const WIR_VirtualRegister&)" );

  mReg = const_cast<WIR_VirtualRegister *>( &r );
};


/*
  getSymbolReg returns the virtual WIR register associated with a symbol.

  getSymbolReg fails with an assertion if no WIR register has previously been
  associated with a symbol.
*/
WIR::WIR_VirtualRegister &RV32_SymbolInfo::getSymbolReg( void ) const
{
  DSTART( "WIR_VirtualRegister& RV32::RV32_SymbolInfo::getSymbolReg() const" );

  return( *mReg );
};


/*
  isSymbolRegSet returns whether a WIR register has previously been associated
  with a symbol.
*/
bool RV32_SymbolInfo::isSymbolRegSet( void ) const
{
  DSTART( "bool RV32_SymbolInfo::isSymbolRegSet() const" );

  return( mReg != nullptr );
};


/*
  setInternalVReg associates the given virtual WIR register as function-internal
  register of a symbol.
*/
void RV32_SymbolInfo::setInternalVReg( const WIR::WIR_VirtualRegister &r )
{
  DSTART( "void RV32_SymbolInfo::setInternalVReg(const WIR_VirtualRegister&)" );

  mVReg = const_cast<WIR_VirtualRegister *>( &r );
};


/*
  getInternalVReg returns the function-internal virtual WIR register associated
  with a symbol.
*/
WIR::WIR_VirtualRegister &RV32_SymbolInfo::getInternalVReg( void ) const
{
  DSTART( "WIR_VirtualRegister& RV32_SymbolInfo::getInternalVReg() const" );

  return( *mVReg );
};


/*
  isInternalVRegSet returns whether a function-internal virtual WIR register has
  previously been associated with a symbol.
*/
bool RV32_SymbolInfo::isInternalVRegSet( void ) const
{
  DSTART( "bool RV32_SymbolInfo::isInternalVRegSet() const" );

  return( mVReg != nullptr );
};


/*
  setAddrReg associates the given virtual RISC-V register as address register of
  a symbol.
*/
void RV32_SymbolInfo::setAddrReg( const WIR::RV_RegV &r )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  mAReg = const_cast<RV_RegV *>( &r );
};


/*
  getAddrReg returns the virtual RISC-V address register associated with a
  symbol.
*/
WIR::RV_RegV &RV32_SymbolInfo::getAddrReg( void ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  return( *mAReg );
};


/*
  isAddrRegSet returns whether a RISC-V address register has previously been
  associated with a symbol.
*/
bool RV32_SymbolInfo::isAddrRegSet( void ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  return( mAReg != nullptr );
};


/*
  setAddressTaken sets whether the address of the current symbol was taken.

  This information is valid only for symbols being function arguments.
*/
void RV32_SymbolInfo::setAddressTaken( bool addressTaken )
{
  DSTART( "void RV32_SymbolInfo::setAddressTaken(bool)" );

  mAddressTaken = addressTaken;
};


/*
  isAddressTaken returns whether the address of a symbol was taken.

  This information is valid only for symbols being function arguments.
*/
bool RV32_SymbolInfo::isAddressTaken( void ) const
{
  DSTART( "bool RV32_SymbolInfo::isAddressTaken() const" );

  return( mAddressTaken );
};


/*
  setStoreInstructionsGenerated sets whether store instructions are already
  generated for a symbol at the beginning of a function.

  This information is valid only for symbols being function arguments, whose
  address have been taken.
*/
void RV32_SymbolInfo::setStoreInstructionsGenerated( bool stInsGenerated )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  mStoreInstructionsGenerated = stInsGenerated;
};


/*
  areStoreInstructionsGenerated returns whether store instructions are already
  generated for a symbol at the beginning of a function.

  This information is valid only for symbols being function arguments, whose
  address have been taken.
*/
bool RV32_SymbolInfo::areStoreInstructionsGenerated( void ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  return( mStoreInstructionsGenerated );
};


/*
  print prints out the contents of this class to stdout.
*/
void RV32_SymbolInfo::print( void ) const
{
  cout << "\tmSymbolType: ";
  if ( mSymbolType == Type::R_ARGUMENT )
    cout << "R_ARGUMENT" << endl;
  if ( mSymbolType == Type::LOCAL_VAR )
    cout << "LOCAL_VAR" << endl;
  if ( mSymbolType == Type::LOCAL_STACK_VAR )
    cout << "LOCAL_STACK_VAR" << endl;

  cout << "\tmArgNum: " << mArgPos << endl;
  cout << "\tmOffset: " << mOffset << endl;
  cout << "\tmReg: "
       << string( mReg != nullptr ? mReg->getName() : "<not set>" )
       << " / " << WIR_id_t( mReg != nullptr ? mReg->getID() : 0 ) << endl;
  cout << "\tmVReg: "
       << string( mVReg != nullptr ? mVReg->getName() : "<not set>" )
       << " / " << WIR_id_t( mVReg != nullptr ? mVReg->getID() : 0 ) << endl;
  cout << "\tmAReg: "
       << string( mAReg != nullptr ? mAReg->getName() : "<not set>" )
       << " / " << WIR_id_t( mAReg != nullptr ? mAReg->getID() : 0 ) << endl;
  cout << "\tmAddressTaken: "
       << ( mAddressTaken == true ?
            string( "true" ) : string( "false" ) )
       << endl;
  cout << "\tmStoreInstructionsGenerated: "
       << ( mStoreInstructionsGenerated == true ?
            string( "true" ) : string( "false" ) )
       << endl;
  cout << endl;
};

}       // namespace RV32
