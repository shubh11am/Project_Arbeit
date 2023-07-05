/*

   This source file belongs to the

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
  @file wirsymboltableapi.cc
  @brief This file implements a base class for managing symbol tables in derived
         classes.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include standard headers
#include <iostream>
#include <sstream>
#include <utility>

// Include boost headers
#include <boost/current_function.hpp>

// Include libuseful headers
#include <libuseful/debugmacros.h>
#include <libuseful/io.h>

// Include WIR headers
#include <wir/wir.h>

// Include private headers
#include "wirsymboltableapi.h"


//
// Code section
//

namespace WIR {


using namespace std;


//
// Public class methods
//

/*
  getSymbols returns all currently managed symbols.
*/
const WIR_SymbolSet &WIR_SymbolTable_API::getSymbols( void ) const
{
  DSTART( "const WIR_SymbolSet& WIR_SymbolTable_API::getSymbols() const" );

  return( mSymbolReferences );
};


/*
  containsSymbol returns whether the symbol table contains a WIR_Symbol for some
  basic block, data or function object having the specified ID.
*/
bool WIR_SymbolTable_API::containsSymbol( WIR_id_t id ) const
{
  DSTART( "bool WIR_SymbolTable_API::containsSymbol(WIR_id_t) const" );

  return( mID2Symbol.count( id ) == 1 );
};


/*
  containsSymbol returns whether set mSymbolReferences contains the specified
  WIR_Symbol.
*/
bool WIR_SymbolTable_API::containsSymbol( const WIR_Symbol &s ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  return( mSymbols.find( s ) != mSymbols.end() );
};


/*
  containsSymbol returns whether this symbol table contains a symbol for the
  specified WIR basic block.
*/
bool WIR_SymbolTable_API::containsSymbol( const WIR_BasicBlock &b ) const
{
  DSTART(
    "bool WIR_SymbolTable_API::containsSymbol(const WIR_BasicBlock&) const" );

  return( mID2Symbol.count( b.getID() ) == 1 );
};


/*
  containsSymbol returns whether this symbol table contains a symbol for the
  specified WIR data object.
*/
bool WIR_SymbolTable_API::containsSymbol( const WIR_Data &d ) const
{
  DSTART( "bool WIR_SymbolTable_API::containsSymbol(const WIR_Data&) const" );

  return( mID2Symbol.count( d.getID() ) == 1 );
};


/*
  containsSymbol returns whether this symbol table contains a symbol for the
  specified WIR function.
*/
bool WIR_SymbolTable_API::containsSymbol( const WIR_Function &f ) const
{
  DSTART(
    "bool WIR_SymbolTable_API::containsSymbol(const WIR_Function&) const" );

  return( mID2Symbol.count( f.getID() ) == 1 );
};


/*
  containsSymbol returns whether this symbol table contains a symbol with the
  given name.
*/
bool WIR_SymbolTable_API::containsSymbol( const std::string &s ) const
{
  DSTART( "bool WIR_SymbolTable_API::containsSymbol(const string&) const" );

  for ( WIR_Symbol &sym : mSymbolReferences )
    if ( sym.getName() == s )
      return( true );

  return( false );
};


/*
  findSymbol finds a WIR_Symbol for some basic block, data or function object
  having the specified ID.

  findSymbol asserts if the symbol table does not contain an object with the
  specified ID.
*/
WIR_Symbol &WIR_SymbolTable_API::findSymbol( WIR_id_t id ) const
{
  DSTART( "WIR_Symbol& WIR_SymbolTable_API::findSymbol(WIR_id_t) const" );

  auto it = mID2Symbol.find( id );
  ufAssert( it != mID2Symbol.end() );
  return( it->second.get() );
};


/*
  findSymbol finds the symbol for the specified WIR basic block.

  findSymbol asserts if the symbol table does not contain the specified basic
  block.
*/
WIR_Symbol &WIR_SymbolTable_API::findSymbol( const WIR_BasicBlock &b ) const
{
  DSTART(
    "WIR_Symbol& WIR_SymbolTable_API::findSymbol(const WIR_BasicBlock&) const" );

  return( findSymbol( b.getID() ) );
};


/*
  findSymbol finds the symbol for the specified WIR data object.

  findSymbol asserts if the symbol table does not contain the specified data
  object.
*/
WIR_Symbol &WIR_SymbolTable_API::findSymbol( const WIR_Data &d ) const
{
  DSTART(
    "WIR_Symbol& WIR_SymbolTable_API::findSymbol(const WIR_Data&) const" );

  return( findSymbol( d.getID() ) );
};


/*
  findSymbol finds the symbol for the specified WIR function.

  findSymbol asserts if the symbol table does not contain the specified
  function.
*/
WIR_Symbol &WIR_SymbolTable_API::findSymbol( const WIR_Function &f ) const
{
  DSTART(
    "WIR_Symbol& WIR_SymbolTable_API::findSymbol(const WIR_Function&) const" );

  return( findSymbol( f.getID() ) );
};


/*
  findSymbol finds the symbol with the given name.

  findSymbol asserts if the symbol table does not contain a symbol with the
  specified name.
*/
WIR_Symbol &WIR_SymbolTable_API::findSymbol( const std::string &s ) const
{
  DSTART( "WIR_Symbol& WIR_SymbolTable_API::findSymbol(const string&) const" );

  for ( WIR_Symbol &sym : mSymbolReferences )
    if ( sym.getName() == s )
      return( sym );

  ufAssertT( false, string( "Failed to find symbol with name '" ) + s + "'." );
};


/*
  findSymbol finds the symbol with the given address.

  If the given memory address is covered by both a function and a basic block
  symbol, findSymbol returns the basic block's symbol. findSymbol asserts if the
  symbol table does not contain a symbol covering the given memory address.
*/
WIR_Symbol &WIR_SymbolTable_API::findSymbol( const WIR_MemoryAddress &a ) const
{
  DSTART(
    "WIR_Symbol& WIR_SymbolTable_API::findSymbol(const WIR_MemoryAddress&) "
    "const" );

  WIR_Symbol *res = nullptr;

  for ( WIR_Symbol &sym : mSymbolReferences ) {
    auto t = sym.getType();
    auto size =
      ( t == WIR_SymbolType::block ) ? sym.getBasicBlock().getSize() :
        ( t == WIR_SymbolType::data ) ? sym.getData().getSize() :
          sym.getFunction().getSize();

    WIR_AddressRange symRange {
      sym.getBaseAddress(), sym.getBaseAddress() + size - 1 };
    WIR_AddressRange adrRange { a, a };

    // Check whether the address ranges of the current symbol and of the given
    // address overlap.
    if ( !symRange.hasEmptyIntersection( adrRange ) )
      if ( t == WIR_SymbolType::function )
        // If we find a function symbol first, let's continue and check whether
        // there is a "smaller" matching basic block symbol. If so, findSymbol
        // returns the basic block symbol instead of the whole function.
        res = &sym;
      else
        return( sym );
  }

  if ( res != nullptr )
    // If we only found a matching function symbol but no "smaller" basic block
    // symbol, we now return the function.
    return( *res );

  stringstream str;
  str << "Failed to find symbol with address 0x" << hex << a << ".";
  ufAssertT( false, str.str() );
};


//
// Protected class methods
//

/*
  Default constructor assigning an empty symbol table.
*/
WIR_SymbolTable_API::WIR_SymbolTable_API( void ) :
  mSymbolsDirty { false }
{
  DSTART( "WIR_SymbolTable_API::WIR_SymbolTable_API()" );
};


/*
  Copy constructor.
*/
WIR_SymbolTable_API::WIR_SymbolTable_API( const WIR_SymbolTable_API &__o )
{
  DSTART(
    "WIR_SymbolTable_API::WIR_SymbolTable_API(const WIR_SymbolTable_API&)" );

  copySymbolTable( __o );
};


/*
  Move constructor.
*/
WIR_SymbolTable_API::WIR_SymbolTable_API( WIR_SymbolTable_API &&__o ) :
  mSymbolsDirty { __o.mSymbolsDirty },
  mSymbols { move( __o.mSymbols ) },
  mSymbolReferences { move( __o.mSymbolReferences ) },
  mID2Symbol { __o.mID2Symbol }
{
  DSTART( "WIR_SymbolTable_API::WIR_SymbolTable_API(WIR_SymbolTable_API&&)" );

  __o.mSymbolsDirty = true;
  __o.mSymbolReferences.clear();
  __o.mSymbols.clear();
  __o.mID2Symbol.clear();
};


/*
  Destructor.
*/
WIR_SymbolTable_API::~WIR_SymbolTable_API( void )
{
  DSTART( "virtual WIR_SymbolTable_API::~WIR_SymbolTable_API()" );
};


/*
  Copy-assignment operator.
*/
WIR_SymbolTable_API & WIR_SymbolTable_API::operator = ( const WIR_SymbolTable_API &__o )
{
  DSTART(
    "WIR_SymbolTable_API& WIR_SymbolTable_API::operator=(const WIR_SymbolTable_API&)" );

  copySymbolTable( __o );

  return( *this );
};


/*
  Move-assignment operator.
*/
WIR_SymbolTable_API & WIR_SymbolTable_API::operator = ( WIR_SymbolTable_API &&__o )
{
  DSTART(
    "WIR_SymbolTable_API& WIR_SymbolTable_API::operator=(WIR_SymbolTable_API&&)" );

  mSymbolsDirty = __o.mSymbolsDirty;
  __o.mSymbolsDirty = true;

  mSymbolReferences = move( __o.mSymbolReferences );
  __o.mSymbolReferences.clear();

  mSymbols = move( __o.mSymbols );
  __o.mSymbols.clear();

  mID2Symbol = move( __o.mID2Symbol );
  __o.mID2Symbol.clear();

  return( *this );
};


/*
  insertSymbol inserts a new WIR_Symbol into the symbol table.

  insertSymbol asserts if a symbol for an already contained function or basic
  block is inserted. The content of s is copied to the new set element.
*/
WIR_Symbol &WIR_SymbolTable_API::insertSymbol( const WIR_Symbol &s )
{
  DSTART( "WIR_Symbol& WIR_SymbolTable_API::insertSymbol(const WIR_Symbol&)" );

  WIR_id_t id = checkDuplicates( s );

  auto it = mSymbols.insert( s ).first;
  mSymbolReferences.insert( const_cast<WIR_Symbol &>( *it ) );
  mSymbolsDirty = true;
  mID2Symbol.insert(
    make_pair<WIR_id_t, reference_wrapper<WIR_Symbol>>(
      move( id ),
      reference_wrapper<WIR_Symbol>( const_cast<WIR_Symbol &>( *it ) ) ) );

  return( const_cast<WIR_Symbol &>( *it ) );
};


/*
  insertSymbol inserts a new WIR_Symbol into the symbol table.

  insertSymbol asserts if a symbol for an already contained function or basic
  block is inserted. The content of s is copied to the new set element.
*/
WIR_Symbol &WIR_SymbolTable_API::insertSymbol( WIR_Symbol &&s )
{
  DSTART( "WIR_Symbol& WIR_SymbolTable_API::insertSymbol(WIR_Symbol&&)" );

  auto id = checkDuplicates( s );

  auto it = mSymbols.insert( s ).first;
  mSymbolReferences.insert( const_cast<WIR_Symbol &>( *it ) );
  mSymbolsDirty = true;
  mID2Symbol.insert(
    make_pair<WIR_id_t, reference_wrapper<WIR_Symbol>>(
      move( id ),
      reference_wrapper<WIR_Symbol>( const_cast<WIR_Symbol &>( *it ) ) ) );

  return( const_cast<WIR_Symbol &>( *it ) );
};


/*
  insertSymbol inserts a new WIR_Symbol for the specified basic block into the
  symbol table.

  insertSymbol asserts if a symbol for an already contained basic block is
  inserted.
*/
WIR_Symbol &WIR_SymbolTable_API::insertSymbol( const WIR_BasicBlock &b )
{
  DSTART(
    "WIR_Symbol& WIR_SymbolTable_API::insertSymbol(const WIR_BasicBlock&)" );

  ufAssertT(
    mID2Symbol.count( b.getID() ) == 0,
    "Illegal attempt to insert basic block '" << b.getName() <<
    "' twice into symbol table." << endl );

  auto it = mSymbols.insert( WIR_Symbol( b ) ).first;
  mSymbolReferences.insert( const_cast<WIR_Symbol &>( *it ) );
  mSymbolsDirty = true;
  mID2Symbol.insert(
    make_pair<WIR_id_t, reference_wrapper<WIR_Symbol>>(
      move( b.getID() ),
      reference_wrapper<WIR_Symbol>( const_cast<WIR_Symbol &>( *it ) ) ) );

  return( const_cast<WIR_Symbol &>( *it ) );
};


/*
  insertSymbol inserts a new WIR_Symbol for the specified data object into the
  symbol table.

  insertSymbol asserts if a symbol for an already contained data object is
  inserted.
*/
WIR_Symbol &WIR_SymbolTable_API::insertSymbol( const WIR_Data &d )
{
  DSTART( "WIR_Symbol& WIR_SymbolTable_API::insertSymbol(const WIR_Data&)" );

  ufAssertT(
    mID2Symbol.count( d.getID() ) == 0,
    "Illegal attempt to insert data object '" << d.getName() <<
    "' twice into symbol table." << endl );

  auto it = mSymbols.insert( WIR_Symbol( d ) ).first;
  mSymbolReferences.insert( const_cast<WIR_Symbol &>( *it ) );
  mSymbolsDirty = true;
  mID2Symbol.insert(
    make_pair<WIR_id_t, reference_wrapper<WIR_Symbol>>(
      move( d.getID() ),
      reference_wrapper<WIR_Symbol>( const_cast<WIR_Symbol &>( *it ) ) ) );

  return( const_cast<WIR_Symbol &>( *it ) );
};


/*
  insertSymbol inserts a new WIR_Symbol for the specified function into the
  symbol table.

  insertSymbol asserts if a symbol for an already contained function is
  inserted.
*/
WIR_Symbol &WIR_SymbolTable_API::insertSymbol( const WIR_Function &f )
{
  DSTART(
    "WIR_Symbol& WIR_SymbolTable_API::insertSymbol(const WIR_Function&)" );

  ufAssertT(
    mID2Symbol.count( f.getID() ) == 0,
    "Illegal attempt to insert function '" << f.getName() <<
    "' twice into symbol table." << endl );

  auto it = mSymbols.insert( WIR_Symbol( f ) ).first;
  mSymbolReferences.insert( const_cast<WIR_Symbol &>( *it ) );
  mSymbolsDirty = true;
  mID2Symbol.insert(
    make_pair<WIR_id_t, reference_wrapper<WIR_Symbol>>(
      move( f.getID() ),
      reference_wrapper<WIR_Symbol>( const_cast<WIR_Symbol &>( *it ) ) ) );

  return( const_cast<WIR_Symbol &>( *it ) );
};


/*
  eraseSymbol removes the symbol for the specified WIR basic block.

  This destroys the removed element.
*/
WIR_SymbolSet::iterator WIR_SymbolTable_API::eraseSymbol( const WIR_BasicBlock &b )
{
  DSTART(
    "WIR_SymbolSet::iterator WIR_SymbolTable_API::eraseSymbol(const WIR_BasicBlock&)" );

  WIR_Symbol &s = findSymbol( b );
  auto id = b.getID();

  mID2Symbol.erase( id );
  auto it = mSymbolReferences.find( s );
  auto it1 = mSymbolReferences.erase( it );
  mSymbols.erase( s );
  mSymbolsDirty = true;

  return( it1 );
};


/*
  eraseSymbol removes the symbol for the specified WIR data object.

  This destroys the removed element.
*/
WIR_SymbolSet::iterator WIR_SymbolTable_API::eraseSymbol( const WIR_Data &d )
{
  DSTART(
    "WIR_SymbolSet::iterator WIR_SymbolTable_API::eraseSymbol(const WIR_Data&)" );

  WIR_Symbol &s = findSymbol( d );
  auto id = d.getID();

  mID2Symbol.erase( id );
  auto it = mSymbolReferences.find( s );
  auto it1 = mSymbolReferences.erase( it );
  mSymbols.erase( s );
  mSymbolsDirty = true;

  return( it1 );
};


/*
  eraseSymbol removes the symbol for the specified WIR function.

  This destroys the removed element.
*/
WIR_SymbolSet::iterator WIR_SymbolTable_API::eraseSymbol( const WIR_Function &f )
{
  DSTART(
    "WIR_SymbolSet::iterator WIR_SymbolTable_API::eraseSymbol(const WIR_Function&)" );

  WIR_Symbol &s = findSymbol( f );
  auto id = f.getID();

  mID2Symbol.erase( id );
  auto it = mSymbolReferences.find( s );
  auto it1 = mSymbolReferences.erase( it );
  mSymbols.erase( s );
  mSymbolsDirty = true;

  return( it1 );
};


/*
  clearSymbols removes all elements from set mSymbols.

  This destroys all removed elements.
*/
void WIR_SymbolTable_API::clearSymbols( void )
{
  DSTART( "void WIR_SymbolTable_API::clearSymbols()" );

  mID2Symbol.clear();
  mSymbolReferences.clear();
  mSymbols.clear();

  mSymbolsDirty = false;
};


/*
  invalidateSymbols marks the information computed for all symbols as invalid.
*/
void WIR_SymbolTable_API::invalidateSymbols( void )
{
  DSTART( "void WIR_SymbolTable_API::invalidateSymbols()" );

  mSymbolsDirty = true;
};


//
// Private class methods
//

/*
  getSymbolID returns the ID of the basic block, function or data object that is
  represented by the specified symbol.
*/
WIR_id_t WIR_SymbolTable_API::getSymbolID( const WIR_Symbol &s ) const
{
  DSTART(
    "WIR_id_t WIR_SymbolTable_API::getSymbolID(const WIR_Symbol&) const" );

  WIR_id_t id = nullid;

  switch ( s.getType() ) {

    case WIR_SymbolType::block: {
      id = s.getBasicBlock().getID();
      break;
    }

    case WIR_SymbolType::data: {
      id = s.getData().getID();
      break;
    }

    case WIR_SymbolType::function: {
      id = s.getFunction().getID();
      break;
    }
  }

  return( id );
};


/*
  checkDuplicates verifies that the object represented by the specified symbol
  is not yet part of this symbol table.

  If the specified symbol is indeed a duplicate, checkDuplicates terminates with
  an assertion.
*/
WIR_id_t WIR_SymbolTable_API::checkDuplicates( const WIR_Symbol &s ) const
{
  DSTART(
    "WIR_id_t WIR_SymbolTable_API::checkDuplicates(const WIR_Symbol&) const" );

  WIR_id_t id = getSymbolID( s );

  ufAssertT(
    id != nullid && mID2Symbol.count( id ) == 0,
    "Illegal attempt to insert a duplicate entry into symbol table." << endl );

  return( id );
};


/*
  copySymbolTable performs actions common to the copy constructor and copy
  assignment operator of WIR symbol tables.
*/
void WIR_SymbolTable_API::copySymbolTable( const WIR_SymbolTable_API &__o )
{
  DSTART(
    "void WIR_SymbolTable_API::copySymbolTable(const WIR_SymbolTable_API&)" );

  // Copy symbols according to the sorted order.
  clearSymbols();
  for ( const WIR_Symbol &s : __o.mSymbolReferences )
    insertSymbol( s );
};

}       // namespace WIR
