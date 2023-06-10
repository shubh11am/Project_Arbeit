/*

   This source file belongs to the

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
#include <arch/tricore/tc13.h>

// Include private headers
#include "symbolinfo.h"


//
// Code section
//

using namespace std;
using namespace WIR;


//
// Public class methods
//

SymbolInfo::SymbolInfo( void ) :
  mSymbolType { LOCAL_VAR },
  mArgNum { 0 },
  mOffset { 0 },
  mLlirReg( "" ),
  mInternalVReg( "" ),
  mAddrReg( "" ),
  mReg { nullptr },
  mVReg { nullptr },
  mAReg { nullptr },
  mAddressTaken( false ),
  mStoreInstructionsGenerated( false )
{
};


SymbolInfo::SymbolInfo( Type symbolType, int argNum, int offset,
                        const std::string &llirReg,
                        const std::string &internalVReg,
                        const std::string &addrReg, bool addressTaken,
                        bool stInsGenerated ) :
  mSymbolType( symbolType ),
  mArgNum( argNum ),
  mOffset( offset ),
  mLlirReg( llirReg ),
  mInternalVReg( internalVReg ),
  mAddrReg( addrReg ),
  mReg { nullptr },
  mVReg { nullptr },
  mAReg { nullptr },
  mAddressTaken( addressTaken ),
  mStoreInstructionsGenerated( stInsGenerated )
{
};


SymbolInfo::SymbolInfo( const SymbolInfo &st ) :
  mSymbolType { st.mSymbolType },
  mArgNum { st.mArgNum },
  mOffset { st.mOffset },
  mLlirReg { st.mLlirReg },
  mInternalVReg { st.mInternalVReg },
  mAddrReg { st.mAddrReg },
  mReg { st.mReg },
  mVReg { st.mVReg },
  mAReg { st.mAReg },
  mAddressTaken { st.mAddressTaken },
  mStoreInstructionsGenerated { st.mStoreInstructionsGenerated }
{
};


SymbolInfo::~SymbolInfo()
{
};


enum SymbolInfo::Type SymbolInfo::getSymbolType( void ) const
{
  return( mSymbolType );
};


void SymbolInfo::setSymbolType( enum Type type )
{
  mSymbolType = type;
};


int SymbolInfo::getSymbolOffset( void ) const
{
  return( mOffset );
};


void SymbolInfo::setSymbolOffset( int offset )
{
  mOffset = offset;
};


int SymbolInfo::getSymbolArgumentPos( void ) const
{
  if ( ( mSymbolType == D_ARGUMENT ) ||
       ( mSymbolType == A_ARGUMENT ) ||
       // For argument symbols whose address was taken.
       ( mSymbolType == LOCAL_STACK_VAR ) )
    return( mArgNum );
  else
    return( -1 );
};


void SymbolInfo::setSymbolArgumentPos( int argNum )
{
  mArgNum = argNum;
};


std::string SymbolInfo::getSymReg( void ) const
{
  return( mLlirReg );
};


void SymbolInfo::setSymbolReg( const std::string &reg )
{
  mLlirReg = reg;
};


/*
  setSymbolReg associates the given virtual WIR register with a symbol.
*/
void SymbolInfo::setSymbolReg( const WIR::WIR_VirtualRegister &r )
{
  DSTART( "void SymbolInfo::setSymbolReg(const WIR_VirtualRegister&)" );

  mReg = const_cast<WIR_VirtualRegister *>( &r );
};


/*
  getSymbolReg returns the virtual WIR register associated with a symbol.

  getSymbolReg fails with an assertion if no WIR register has previously been
  associated with a symbol.
*/
WIR::WIR_VirtualRegister &SymbolInfo::getSymbolReg( void ) const
{
  DSTART( "WIR_VirtualRegister& SymbolInfo::getSymbolReg() const" );

  return( *mReg );
};


/*
  isSymbolRegSet returns whether a WIR register has previously been associated
  with a symbol.
*/
bool SymbolInfo::isSymbolRegSet( void ) const
{
  DSTART( "bool SymbolInfo::isSymbolRegSet() const" );

  return( mReg != nullptr );
};


std::string SymbolInfo::getIntVReg( void ) const
{
  return( mInternalVReg );
};


void SymbolInfo::setInternalVReg( const std::string &reg )
{
  mInternalVReg = reg;
};


/*
  setInternalVReg associates the given virtual WIR register as function-internal
  register of a symbol.
*/
void SymbolInfo::setInternalVReg( const WIR::WIR_VirtualRegister &r )
{
  DSTART( "void SymbolInfo::setInternalVReg(const WIR_VirtualRegister&)" );

  mVReg = const_cast<WIR_VirtualRegister *>( &r );
};


/*
  getInternalVReg returns the function-internal virtual WIR register associated
  with a symbol.

  getInternalVReg fails with an assertion if no WIR register has previously been
  associated with a symbol.
*/
WIR::WIR_VirtualRegister &SymbolInfo::getInternalVReg( void ) const
{
  DSTART( "WIR_VirtualRegister& SymbolInfo::getInternalVReg() const" );

  return( *mVReg );
};


/*
  isInternalVRegSet returns whether a function-internal virtual WIR register has
  previously been associated with a symbol.
*/
bool SymbolInfo::isInternalVRegSet( void ) const
{
  DSTART( "bool SymbolInfo::isInternalVRegSet() const" );

  return( mVReg != nullptr );
};


std::string SymbolInfo::getAdrReg( void ) const
{
  return( mAddrReg );
};


void SymbolInfo::setAddrReg( const std::string &reg )
{
  mAddrReg = reg;
};


/*
  setAddrReg associates the given virtual TriCore register as address register
  of a symbol.
*/
void SymbolInfo::setAddrReg( const WIR::TC_ARegV &r )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  mAReg = const_cast<TC_ARegV *>( &r );
};


/*
  getAddrReg returns the virtual TriCore address register associated with a
  symbol.

  getAddrReg fails with an assertion if no WIR register has previously been
  associated with a symbol.
*/
WIR::TC_ARegV &SymbolInfo::getAddrReg( void ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  return( *mAReg );
};


/*
  isAddrRegSet returns whether a TriCore address register has previously been
  associated with a symbol.
*/
bool SymbolInfo::isAddrRegSet( void ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  return( mAReg != nullptr );
};


/*
  getAddressTaken returns whether the address of the current symbol was taken.
  This information is valid only for symbols being function arguments.
*/
bool SymbolInfo::getAddressTaken( void ) const
{
  return( mAddressTaken );
};


/*
  setAddressTaken sets whether the address of the current symbol was taken.
  This information is valid only for symbols being function arguments.
*/
void SymbolInfo::setAddressTaken( bool addressTaken )
{
  mAddressTaken = addressTaken;
};


/*
  getStoreInstructionsGenerated returns the attribute
  mStoreInstructionsGenerated.
*/
bool SymbolInfo::getStoreInstructionsGenerated( void ) const
{
  return( mStoreInstructionsGenerated );
};


/*
  setStoreInstructionsGenerated sets the attribute mStoreInstructionsGenerated.
*/
void SymbolInfo::setStoreInstructionsGenerated( bool set )
{
  mStoreInstructionsGenerated = set;
};


void SymbolInfo::print( void ) const
{
  cout << "\tmSymbolType: ";
  if ( mSymbolType == D_ARGUMENT )
    cout << "D_ARGUMENT" << endl;
  if ( mSymbolType == A_ARGUMENT )
    cout << "A_ARGUMENT" << endl;
  if ( mSymbolType == LOCAL_VAR )
    cout << "LOCAL_VAR" << endl;
  if ( mSymbolType == LOCAL_STACK_VAR )
    cout << "LOCAL_STACK_VAR" << endl;
  if ( mSymbolType == LOCAL_COMPOSED_ARGUMENT )
    cout << "LOCAL_COMPOSED_ARGUMENT" << endl;

  cout << "\tmArgNum: " << mArgNum << endl;
  cout << "\tmOffset: " << mOffset << endl;
  cout << "\tmLlirReg: " << mLlirReg << endl;
  cout << "\tmInternalVReg: " << mInternalVReg << endl;
  cout << "\tmAddrReg: " << mAddrReg << endl;
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
