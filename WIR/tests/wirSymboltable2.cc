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


//
// Include section
//

// Include standard headers
#include <set>

// Include libuseful headers
#include <libuseful/io.h>

// Include WIR headers
#include <wir/wir.h>
#include <arch/generic/mips.h>


using namespace std;
using namespace WIR;


void checkST( const WIR_System &sys )
{
  set<WIR_id_t> ids;

  for ( WIR_CompilationUnit &c : sys )
    for ( WIR_Function &f : c ) {
      ids.insert( f.getID() );
      for ( WIR_BasicBlock &b : f )
        ids.insert( b.getID() );
    }

  ufAssert( sys.getSymbols().size() == ids.size() );
  for ( WIR_Symbol &s : sys.getSymbols() ) {
    WIR_id_t id = nullid;

    if ( s.getType() == WIR_SymbolType::function )
      id = s.getFunction().getID();
    else
      id = s.getBasicBlock().getID();

    ufAssert( ids.count( id ) == 1 );
  }

  for ( WIR_CompilationUnit &c : sys )
    for ( WIR_Function &f : c.getFunctions() ) {
      ufAssert( sys.containsSymbol( f ) );
      ufAssert( sys.containsSymbol( f.getID() ) );
      ufAssert( sys.findSymbol( f ).getType() == WIR_SymbolType::function );
      ufAssert( sys.findSymbol( f ).getFunction() == f );
      ufAssert( sys.findSymbol( f.getID() ).getFunction() == f );

      for ( WIR_BasicBlock &b : f ) {
        ufAssert( sys.containsSymbol( b ) );
        ufAssert( sys.containsSymbol( b.getID() ) );
        ufAssert( sys.findSymbol( b ).getType() == WIR_SymbolType::block );
        ufAssert( sys.findSymbol( b ).getBasicBlock() == b );
        ufAssert( sys.findSymbol( b.getID() ).getBasicBlock() == b );
      }
    }
};


int main( void )
{
  WIR_Init();

  WIR_Function f( "foo" );
  WIR_BasicBlock b;
  WIR_CompilationUnit c1;
  WIR_TaskManager t;
  WIR_System sys( "genericmips.sys", t );

  ufAssert( sys.getSymbols().empty() );

  f.pushBackBasicBlock( b );
  f.pushBackBasicBlock( WIR_BasicBlock() );
  c1.pushBackFunction( f );
  c1.pushBackFunction( WIR_Function( "bar" ) );

  // Check validity of symbol table across insertion/removal/replacement of
  // functions into compilation units of systems.
  sys.pushBackCompilationUnit( c1 );
  sys.pushFrontCompilationUnit( WIR_CompilationUnit( c1 ) );
  sys.pushFrontCompilationUnit( WIR_CompilationUnit( c1 ) );
  sys.pushBackCompilationUnit( WIR_CompilationUnit( c1 ) );
  checkST( sys );

  auto it = sys.getCompilationUnits().begin();
  ++it;
  ++it;
  WIR_CompilationUnit &c = it->get();

  c.pushBackFunction( WIR_Function( "a" ) );
  c.pushFrontFunction( WIR_Function( "b" ) );
  c.pushFrontFunction( WIR_Function( "c" ) );
  c.pushBackFunction( WIR_Function( "d" ) );
  checkST( sys );

  c.popBackFunction();
  c.popFrontFunction();
  checkST( sys );

  auto it1 = c.getFunctions().begin();
  ++it1;
  it1 = c.insertFunction( it1, WIR_Function( f ) );
  it1 = c.insertFunction( it1, WIR_Function( f ) );
  it1 = c.insertFunction( it1, WIR_Function( f ) );
  it1 = c.getFunctions().begin();
  ++it1;
  ++it1;
  ++it1;
  c.replaceFunction( it1, WIR_Function( f ) );
  checkST( sys );

  it1 = c.getFunctions().begin();
  ++it1;
  ++it1;
  c.eraseFunction( it1 );
  checkST( sys );

  c.clearFunctions();
  checkST( sys );

  return( 0 );
}
