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

// Include private headers
#include "symbolinfo.h"

using namespace std;

//
// Class section
//

SymbolInfo::SymbolInfo() :
  mSymbolType { LOCAL_VAR },
  mArgNum { 0 },
  mOffset { 0 },
  mLlirReg( "" ),
  mInternalVReg( "" ),
  mAddrReg( "" ),
  mAddressTaken( false ),
  mStoreInstructionsGenerated( false )
{
}


SymbolInfo::SymbolInfo( Type symbolType, int argNum, int offset,
                        const std::string& llirReg,
                        const std::string& internalVReg,
                        const std::string& addrReg, bool addressTaken,
                        bool stInsGenerated ) :
  mSymbolType( symbolType ), mArgNum( argNum ), mOffset( offset ),
  mLlirReg( llirReg ), mInternalVReg( internalVReg ), mAddrReg( addrReg ),
  mAddressTaken( addressTaken ), mStoreInstructionsGenerated( stInsGenerated )
{
}


SymbolInfo::SymbolInfo( const SymbolInfo &st ) :
  mSymbolType { st.mSymbolType },
  mArgNum { st.mArgNum },
  mOffset { st.mOffset },
  mLlirReg { st.mLlirReg },
  mInternalVReg { st.mInternalVReg },
  mAddrReg { st.mAddrReg },
  mAddressTaken { st.mAddressTaken },
  mStoreInstructionsGenerated { st.mStoreInstructionsGenerated }
{
}


SymbolInfo::~SymbolInfo()
{
}


enum SymbolInfo::Type SymbolInfo::getSymbolType() const
{
  return mSymbolType;
}


void SymbolInfo::setSymbolType( enum Type type )
{
  mSymbolType = type;
}


int SymbolInfo::getSymbolOffset() const
{
  return mOffset;
}


void SymbolInfo::setSymbolOffset( int offset )
{
  mOffset = offset;
}


int SymbolInfo::getSymbolArgumentPos() const
{
  if ( mSymbolType == D_ARGUMENT ||
       mSymbolType == A_ARGUMENT ||
       // For argument symbols whose address was taken
       mSymbolType == LOCAL_STACK_VAR )
    return mArgNum;
  else
    return -1;
}


void SymbolInfo::setSymbolArgumentPos( int argNum )
{
  mArgNum = argNum;
}


std::string SymbolInfo::getSymbolReg() const
{
  return mLlirReg;
}


void SymbolInfo::setSymbolReg( const std::string &reg )
{
  mLlirReg = reg;
}


std::string SymbolInfo::getInternalVReg() const
{
  return mInternalVReg;
}


void SymbolInfo::setInternalVReg( const std::string &reg )
{
  mInternalVReg = reg;
}


std::string SymbolInfo::getAddrReg() const
{
  return mAddrReg;
}


void SymbolInfo::setAddrReg( const std::string &reg )
{
  mAddrReg = reg;
}


/*
  getAddressTaken returns whether the address of the current symbol was taken.
  This information is valid only for symbols being function arguments.
*/
bool SymbolInfo::getAddressTaken() const
{
  return mAddressTaken;
}


/*
  setAddressTaken sets whether the address of the current symbol was taken.
  This information is valid only for symbols being function arguments.
*/
void SymbolInfo::setAddressTaken( bool addressTaken )
{
  mAddressTaken = addressTaken;
}


/*
  getStoreInstructionsGenerated returns the attribute
  mStoreInstructionsGenerated.
*/
bool SymbolInfo::getStoreInstructionsGenerated() const
{
  return mStoreInstructionsGenerated;
}


/*
  setStoreInstructionsGenerated sets the attribute mStoreInstructionsGenerated.
*/
void SymbolInfo::setStoreInstructionsGenerated( bool set )
{
  mStoreInstructionsGenerated = set;
}


void SymbolInfo::print() const
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
  cout << "\tmAddressTaken: "
       << ( mAddressTaken == true ?
            string( "true" ) : string( "false" ) )
       << endl;
  cout << "\tmStoreInstructionsGenerated: "
       << ( mStoreInstructionsGenerated == true ?
            string( "true" ) : string( "false" ) )
       << endl;
  cout << endl;
}
