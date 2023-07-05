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
  @file wirtaskmanager.cc
  @brief This file implements %WIR task managers.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include boost headers
#include <boost/current_function.hpp>

// Include libuseful headers
#include <libuseful/debugmacros.h>
#include <libuseful/io.h>

// Include WIR headers
#include <wir/wir.h>


//
// Code section
//

namespace WIR {


using namespace std;


//
// Public class methods
//

/*
  Default constructor creating a basic block symbol.
*/
WIR_Symbol::WIR_Symbol( const WIR_BasicBlock &b ) :
  WIR_ID_API {},
  mSymbolType { WIR_SymbolType::block },
  mReferedID { const_cast<WIR_BasicBlock *>( &b ) },
  mConst { false },
  mExtern { false },
  mGlobal { false },
  mVolatile { false },
  mSection { nullptr }
{
  DSTART( "WIR_Symbol::WIR_Symbol(const WIR_BasicBlock&)" );
};


/*
  Default constructor creating a data object symbol.
*/
WIR_Symbol::WIR_Symbol( const WIR_Data &d ) :
  WIR_ID_API {},
  mSymbolType { WIR_SymbolType::data },
  mReferedID { const_cast<WIR_Data *>( &d ) },
  mConst { false },
  mExtern { false },
  mGlobal { false },
  mVolatile { false },
  mSection { nullptr }
{
  DSTART( "WIR_Symbol::WIR_Symbol(const WIR_Data&)" );
};


/*
  Default constructor creating a function symbol.
*/
WIR_Symbol::WIR_Symbol( const WIR_Function &f ) :
  WIR_ID_API {},
  mSymbolType { WIR_SymbolType::function },
  mReferedID { const_cast<WIR_Function *>( &f ) },
  mConst { false },
  mExtern { false },
  mGlobal { false },
  mVolatile { false },
  mSection { nullptr }
{
  DSTART( "WIR_Symbol::WIR_Symbol(const WIR_Function&)" );
};


/*
  Copy constructor.
*/
WIR_Symbol::WIR_Symbol( const WIR_Symbol &__o ) :
  WIR_ID_API { __o },
  mSymbolType { __o.mSymbolType },
  mReferedID { __o.mReferedID },
  mConst { __o.mConst },
  mExtern { __o.mExtern },
  mGlobal { __o.mGlobal },
  mVolatile { __o.mVolatile },
  mSection { nullptr },
  mBaseAddress { 0 }
{
  DSTART( "WIR_Symbol::WIR_Symbol(const WIR_Symbol&)" );
};


/*
  Move constructor.
*/
WIR_Symbol::WIR_Symbol( WIR_Symbol &&__o ) :
  mSymbolType { move( __o.mSymbolType ) },
  mReferedID { __o.mReferedID },
  mConst { move( __o.mConst ) },
  mExtern { move( __o.mExtern ) },
  mGlobal { move( __o.mGlobal ) },
  mVolatile { move( __o.mVolatile ) },
  mSection { __o.mSection },
  mBaseAddress { move( __o.mBaseAddress ) }
{
  DSTART( "WIR_Symbol::WIR_Symbol(WIR_Symbol&&)" );

  __o.mReferedID = nullptr;
  __o.mConst = false;
  __o.mExtern = false;
  __o.mGlobal = false;
  __o.mVolatile = false;
  __o.mSection = nullptr;
  __o.mBaseAddress = 0;
};


/*
  Destructor.
*/
WIR_Symbol::~WIR_Symbol( void )
{
  DSTART( "virtual WIR_Symbol::~WIR_Symbol()" );
};


/*
  Copy-assignment operator.
*/
WIR_Symbol & WIR_Symbol::operator = ( const WIR_Symbol &__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  mSymbolType = __o.mSymbolType;
  mReferedID = __o.mReferedID;
  mConst = __o.mConst;
  mExtern = __o.mExtern;
  mGlobal = __o.mGlobal;
  mVolatile = __o.mVolatile;
  mSection = nullptr;
  mBaseAddress = 0;

  return( *this );
};


/*
  Move-assignment operator.
*/
WIR_Symbol & WIR_Symbol::operator = ( WIR_Symbol &&__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  mSymbolType = __o.mSymbolType;
  mReferedID = __o.mReferedID;
  __o.mReferedID = nullptr;
  mConst = __o.mConst;
  __o.mConst = false;
  mExtern = __o.mExtern;
  __o.mExtern = false;
  mGlobal = __o.mGlobal;
  __o.mGlobal = false;
  mVolatile = __o.mVolatile;
  __o.mVolatile = false;
  mSection = __o.mSection;
  __o.mSection = nullptr;
  mBaseAddress = 0;
  __o.mBaseAddress = 0;

  return( *this );
};


/*
  getType returns to which kind of entity a symbol refers, i.e., whether a
  symbol represents a WIR basic block, data or a function etc.
*/
WIR_SymbolType WIR_Symbol::getType( void ) const
{
  DSTART( "WIR_SymbolType WIR_Symbol::getType() const" );

  return( mSymbolType );
};


/*
  In case the symbol refers to a WIR basic block, getBasicBlock returns the
  represented block.

  If the symbol does not refer to a basic block, getBasicBlock fails with an
  assertion.
*/
WIR_BasicBlock &WIR_Symbol::getBasicBlock( void ) const
{
  DSTART( "WIR_BasicBlock& WIR_Symbol::getBasicBlock() const" );

  return( dynamic_cast<WIR_BasicBlock &>( *mReferedID ) );
};


/*
  In case the symbol refers to a WIR data object, getData returns the
  represented data.

  If the symbol does not refer to a data object, getData fails with an
  assertion.
*/
WIR_Data &WIR_Symbol::getData( void ) const
{
  DSTART( "WIR_Data& WIR_Symbol::getData() const" );

  return( dynamic_cast<WIR_Data &>( *mReferedID ) );
};


/*
  In case the symbol refers to a WIR function, getFunction returns the
  represented function.

  If the symbol does not refer to a function, getFunction fails with an
  assertion.
*/
WIR_Function &WIR_Symbol::getFunction( void ) const
{
  DSTART( "WIR_Function& WIR_Symbol::getFunction() const" );

  return( dynamic_cast<WIR_Function &>( *mReferedID ) );
};


/*
  getName returns a symbol's specific name.
*/
string WIR_Symbol::getName( void ) const
{
  DSTART( "string WIR_Symbol::getName() const" );

  switch ( mSymbolType ) {
    case WIR_SymbolType::block:
      return( getBasicBlock().getName() );

    case WIR_SymbolType::data:
      return( getData().getName() );

    case WIR_SymbolType::function:
      return( getFunction().getName() );
  }

  return( "" );
};


/*
  setConst sets whether a symbol is constant/read-only or not.

  If the object refered by the symbol must not be modified, setConst asserts.
*/
void WIR_Symbol::setConst( bool b )
{
  DSTART( "void WIR_Symbol::setConst(bool)" );

  checkDontOptimize();
  mConst = b;
};


/*
  isConst returns whether a symbol is constant/read-only or not.
*/
bool WIR_Symbol::isConst( void ) const
{
  DSTART( "bool WIR_Symbol::isConst() const" );

  return( mConst );
};


/*
  setExtern sets whether a symbol is external or not.

  If the object refered by the symbol must not be modified, setExtern asserts.
*/
void WIR_Symbol::setExtern( bool b )
{
  DSTART( "void WIR_Symbol::setExtern(bool)" );

  checkDontOptimize();
  mExtern = b;
};


/*
  isExtern returns whether a symbol is external or not.
*/
bool WIR_Symbol::isExtern( void ) const
{
  DSTART( "bool WIR_Symbol::isExtern() const" );

  return( mExtern );
};


/*
  setGlobal sets whether a symbol is global or not.

  If the object refered by the symbol must not be modified, setGlobal asserts.
*/
void WIR_Symbol::setGlobal( bool b )
{
  DSTART( "void WIR_Symbol::setGlobal(bool)" );

  checkDontOptimize();
  mGlobal = b;
};


/*
  isGlobal returns whether a symbol is global or not.
*/
bool WIR_Symbol::isGlobal( void ) const
{
  DSTART( "bool WIR_Symbol::isGlobal() const" );

  return( mGlobal );
};


/*
  setVolatile sets whether a symbol is volatile or not.

  If the object refered by the symbol must not be modified, setVolatile asserts.
*/
void WIR_Symbol::setVolatile( bool b )
{
  DSTART( "void WIR_Symbol::setVolatile(bool)" );

  checkDontOptimize();
  mVolatile = b;
};


/*
  isVolatile returns whether a symbol is volatile or not.
*/
bool WIR_Symbol::isVolatile( void ) const
{
  DSTART( "bool WIR_Symbol::isVolatile() const" );

  return( mVolatile );
};


/*
  setSection sets the ELF executable file section into which the symbol is
  assembled.

  The assignment of a symbol to a section is done according to the following
  rules:
  -# If the symbol refers to a function, the section assignment of all basic
     blocks of that function is unset.
  -# If the symbol refers to a basic block, the section assignment of that
     block's function is unset. Section assignments of all other basic blocks of
     that very same function remain unchanged.
  -# Finally, the current symbol itself is assigned to the specified section.
  This way, section assignments for code only exist either for a function or for
  a function's basic blocks or for none of them, but never for both.

  If the object refered by the symbol must not be modified, setSection asserts.
*/
void WIR_Symbol::setSection( const WIR_Section &s )
{
  DSTART( "void WIR_Symbol::setSection(const WIR_Section&)" );

  // Before assigning a function to a section, unassign all included basic
  // blocks.
  if ( mSymbolType == WIR_SymbolType::function ) {
    auto &f = getFunction();
    auto &sys = f.getCompilationUnit().getSystem();

    for ( WIR_BasicBlock &b : f ) {
      auto &sym = sys.findSymbol( b );
      b.checkDontOptimize();
      sym.mSection = nullptr;
    }
  } else

  // Before assigning a basic block to a section, unassign its function.
  if ( mSymbolType == WIR_SymbolType::block ) {
    auto &f = getBasicBlock().getFunction();

    f.checkDontOptimize();
    auto &sym = f.getCompilationUnit().getSystem().findSymbol( f );
    sym.mSection = nullptr;
  }

  // Finally, assign the current symbol to the desired section.
  checkDontOptimize();
  mSection = const_cast<WIR_Section *>( &s );

  WIR_System &sys =
    ( mSymbolType == WIR_SymbolType::block ?
        getBasicBlock().getFunction().getCompilationUnit().getSystem() :
        ( mSymbolType == WIR_SymbolType::data ?
            getData().getCompilationUnit().getSystem() :
            getFunction().getCompilationUnit().getSystem() ) );
  sys.invalidateSymbols();
};


/*
  unsetSection removes any prior explicit assignment of a symbol to an ELF
  executable file section.

  If the object refered by the symbol must not be modified, unsetSection
  asserts.
*/
void WIR_Symbol::unsetSection( void )
{
  DSTART( "void WIR_Symbol::unsetSection()" );

  checkDontOptimize();
  mSection = nullptr;

  WIR_System &sys =
    ( mSymbolType == WIR_SymbolType::block ?
        getBasicBlock().getFunction().getCompilationUnit().getSystem() :
        ( mSymbolType == WIR_SymbolType::data ?
            getData().getCompilationUnit().getSystem() :
            getFunction().getCompilationUnit().getSystem() ) );
  sys.invalidateSymbols();
};


/*
  getSection returns the ELF executable file section into which the symbol is
  assembled.

  If a symbol has been assigned to a section explicitly before using setSection,
  this explicitly specified section is returned.

  If the symbol refers to a basic block and the block's function symbol has been
  assigned to a section, the function's section is returned.

  If the symbol refers to a function and that function's basic blocks have been
  assigned to sections, the section of the very first basic block of the
  function is returned.

  If no explicit section assignment exists, a default section is determined and
  returned by getSection according to the following rules:
  -# If the symbol refers to a function or a basic block, the default section
      ".text" of the first processor core of the WIR system is returned.
  -# Otherwise, the symbol refers to a data object. In this case,
     -# section ".rodata" of the first processor core of the WIR system is
        returned if the symbol is constant,
     -# else section ".data" of the first processor core of the WIR system is
        returned if the data object is initialized,
     -# else section ".bss" of the first processor core of the WIR system is
        returned.
*/
const WIR_Section &WIR_Symbol::getSection( void ) const
{
  DSTART( "const WIR_Section& WIR_Symbol::getSection() const" );

  // Is the symbol itself explicitly assigned to a section?
  if ( mSection != nullptr )
    return( *mSection );

  // Is it a basic block symbol whose function has been explicitly assigned?
  if ( mSymbolType == WIR_SymbolType::block ) {
    auto &f = getBasicBlock().getFunction();
    auto &sym = f.getCompilationUnit().getSystem().findSymbol( f );

    if ( sym.mSection != nullptr )
      return( *(sym.mSection) );
  }

  // Determine the WIR system.
  WIR_System &s =
    ( mSymbolType == WIR_SymbolType::block ?
        getBasicBlock().getFunction().getCompilationUnit().getSystem() :
        ( mSymbolType == WIR_SymbolType::data ?
          getData().getCompilationUnit().getSystem() :
          getFunction().getCompilationUnit().getSystem() ) );

  // Get the first processor core of the system.
  auto &p = s.getComponents<WIR_BaseProcessor>().begin()->get();

  // Is it a function symbol without explicit assignment, but the function's
  // blocks are assigned?
  if ( mSymbolType == WIR_SymbolType::function ) {
    bool blockLevelAssignment = false;
    WIR_Function &f = getFunction();

    // Are some basic blocks of the function assigned?
    for ( WIR_BasicBlock &b : f )
      if ( s.findSymbol( b ).mSection != nullptr ) {
        blockLevelAssignment = true;
        break;
      }

    if ( blockLevelAssignment ) {
      // Yes, so return the section assigned to the very first basic block.
      WIR_BasicBlock &b = f.getBasicBlocks().front().get();
      WIR_Symbol &sym = s.findSymbol( b );

      if ( sym.mSection != nullptr )
        return( *(sym.mSection) );
      else
        return( p.getTextSection() );
    }
  }

  // No explicit assignment exists at all, determine a default section.
  if ( mSymbolType != WIR_SymbolType::data )
    return( p.getTextSection() );

  if ( isConst() )
    return( p.getRODataSection() );

  if ( getData().isInitialized() )
    return( p.getDataSection() );

  return( p.getBssSection() );
};


/*
  getBaseAddress returns a symbol's base address.
*/
WIR_MemoryAddress WIR_Symbol::getBaseAddress( void ) const
{
  DSTART( "WIR_MemoryAddress WIR_Symbol::getBaseAddress() const" );

  WIR_System &s =
    ( mSymbolType == WIR_SymbolType::block ?
        getBasicBlock().getFunction().getCompilationUnit().getSystem() :
        ( mSymbolType == WIR_SymbolType::data ?
            getData().getCompilationUnit().getSystem() :
            getFunction().getCompilationUnit().getSystem() ) );
  s.computeMemoryLayout();

  return( mBaseAddress );
};


//
// Private class methods
//

/*
  setBaseAddress sets a symbol's base address.
*/
void WIR_Symbol::setBaseAddress( const WIR_MemoryAddress &s )
{
  DSTART( "void WIR_Symbol::setBaseAddress(const WIR_MemoryAddress&)" );

  mBaseAddress = s;
};


/*
  checkDontOptimize checks whether the object to which a symbol refers must not
  be modified.

  If the refered object must not be modified, checkDontOptimize asserts.
*/
void WIR_Symbol::checkDontOptimize( void ) const
{
  DSTART( "void WIR_Symbol::checkDontOptimize() const" );

  switch ( mSymbolType ) {
    case WIR_SymbolType::block: {
      ufAssertT(
        !getBasicBlock().getDontOptimize(),
        "Illegal attempt to modify a symbol whose refered basic block is " <<
        "set as 'don't optimize'!" );
      break;
    }

    case WIR_SymbolType::data: {
      ufAssertT(
        !getData().getDontOptimize(),
        "Illegal attempt to modify a symbol whose refered data object is " <<
        "set as 'don't optimize'!" );
      break;
    }

    case WIR_SymbolType::function: {
      ufAssertT(
        !getFunction().getDontOptimize(),
        "Illegal attempt to modify a symbol whose refered function is set " <<
        "as 'don't optimize'!" );
      break;
    }
  }
};

}       // namespace WIR
