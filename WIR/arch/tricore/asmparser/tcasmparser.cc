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
  @file tcasmparser.cc
  @brief This file implements a TriCore assembly code parser.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include standard headers
#include <sstream>

// Include libuseful headers
#include <libuseful/debugmacros.h>
#include <libuseful/io.h>

// Include WIR headers
#include <wir/wir.h>

// Include local headers
#include <arch/tricore/asmparser/tcasmyacc.hh>
#include "tcasmcontext.h"
#include "tcasmlex.h"
#include "tcasmparser.h"


//
// Code section
//

namespace WIR {


using namespace std;


//
// Public class methods
//

/*
  Default constructor creating a TriCore assembly code parser.
*/
TC_AsmParser::TC_AsmParser( void ) :
  mContext {},
  mDebugScanner { false },
  mDebugParser { false },
  mScanner { nullptr },
  m16BitOperations { true }
{
  DSTART( "TC_AsmParser::TC_AsmParser()" );
};


/*
  Destructor.
*/
TC_AsmParser::~TC_AsmParser( void )
{
  DSTART( "TC_AsmParser::~TC_AsmParser()" );
};


/*
  setDebugScanner (de-) activates debug output of the scanner.
*/
void TC_AsmParser::setDebugScanner( bool d )
{
  DSTART( "void TC_AsmParser::setDebugScanner(bool)" );

  mDebugScanner = d;
};


/*
  setDebugParser (de-) activates debug output of the parser.
*/
void TC_AsmParser::setDebugParser( bool d )
{
  DSTART( "void TC_AsmParser::setDebugParser(bool)" );

  mDebugParser = d;
};


/*
  run parses the assembly code given in a string.

  The parser requires that b is already be inserted into a WIR function and that
  this WIR function in turn is properly inserted into a compilation unit and a
  WIR system.

  If the parser is supposed not to generate 16 bits wide operations, but the
  input to be parsed actually includes such an operation, the parser emits a
  corresponding warning.
*/
const std::list<std::reference_wrapper<WIR_BasicBlock>> &TC_AsmParser::run( const std::string &s,
                                                                            const std::vector<std::unique_ptr<TC_AsmArgument>> &tArgs,
                                                                            WIR_BasicBlock &b,
                                                                            const std::string &sName )
{
  DSTART( "const list<reference_wrapper<WIR_BasicBlock> >& TC_AsmParser::run(const string&, const vector<unique_ptr<TC_AsmArgument> >&, WIR_BasicBlock&, const string&)" );

  istringstream iss( s );
  return( run( iss, tArgs, b, sName ) );
};


/*
  run parses the assembly code given in an input stream.

  The parser requires that b is already be inserted into a WIR function and that
  this WIR function in turn is properly inserted into a compilation unit and a
  WIR system.

  If the parser is supposed not to generate 16 bits wide operations, but the
  input to be parsed actually includes such an operation, the parser emits a
  corresponding warning.
*/
const std::list<std::reference_wrapper<WIR_BasicBlock>> &TC_AsmParser::run( std::istream &s,
                                                                            const std::vector<std::unique_ptr<TC_AsmArgument>> &tArgs,
                                                                            WIR_BasicBlock &b,
                                                                            const std::string &sName )
{
  DSTART( "const list<reference_wrapper<WIR_BasicBlock> >& TC_AsmParser::run(istream&, const vector<unique_ptr<TC_AsmArgument> >&, WIR_BasicBlock&, const string&)" );

  ufAssert( b.isInserted() );

  // Create new parser context.
  mContext.reset( new TC_AsmContext( tArgs, b ) );
  mContext->setGenerate16BitOperations( m16BitOperations );
  mStreamName = sName;

  // Create new scanner.
  TC_AsmLex scanner( *mContext, &s, mContext->hasTemplateArguments() );
  scanner.setDebug( mDebugScanner );
  mScanner = &scanner;

  // Create new parser.
  TC_AsmYacc parser( *this );
  parser.set_debug_level( mDebugParser );

  if ( parser.parse() != 0 ) {
    // Do error handling.
    string err = mContext->getErrMessage();
    auto loc = mContext->getErrLocation();

    if ( err.substr( 0, 12 ) == "syntax error" )
      err = err.substr( 13, string::npos );
    if ( err.substr( 0, 2 ) == " u" )
      err = "U" + err.substr( 2, string::npos );

    ufErrMsg << ufFile( *(loc.begin.filename), loc.begin.line )
             << "Syntax error in inline assembly: " << err + "." << endl;

    exit( 1 );
  }

  // Resolve all temporary string label parameters attached to WIR operations.
  resolveStringLabels();

  // Cleanup.
  mScanner = nullptr;
  mStreamName.clear();

  // Return basic block list.
  return( mContext->getBasicBlocks() );
};


/*
  setGenerate16BitOperations sets whether an optimization shall generate 16 bits
  wide operations or not.
*/
void TC_AsmParser::setGenerate16BitOperations( bool f )
{
  DSTART( "void TC_AsmParser::setGenerate16BitOperations(bool)" );

  m16BitOperations = f;
};


/*
  getGenerate16BitOperations returns whether an optimization shall generate 16
  bits wide operations or not.
*/
bool TC_AsmParser::getGenerate16BitOperations( void ) const
{
  DSTART( "bool TC_AsmParser::getGenerate16BitOperations() const" );

  return( m16BitOperations );
};


//
// Private class methods
//

/*
  resolveStringLabels resolves all string label parameters attached to WIR
  operations by proper label parameters.
*/
void TC_AsmParser::resolveStringLabels( void )
{
  DSTART( "void TC_AsmParser::resolveStringLabels()" );

  // Iterate over all operations where a label still needs to be resolved.
  for ( WIR_Operation &o : mContext->mOperationsToBeResolved ) {
    auto pos1 = o.begin();
    auto pos2 = o.begin();

    string label;

    // Determine the positions of the label to be resolved and of the parameter
    // containing the string to be resolved.
    for ( auto it = o.begin(); it != o.end(); ++it ) {
      WIR_Parameter &p = it->get();

      if ( ( p.getType() == WIR_ParameterType::label ) && p.isExplicit() )
        pos1 = it;
      else

      if ( ( p.getType() == WIR_ParameterType::str ) && p.isImplicit() ) {
        pos2 = it;
        label = dynamic_cast<WIR_StringParameter &>( p ).getString();
      }
    }

    if ( mContext->mBlockOfLabel.count( label ) ) {
      // Replace the old label parameter by a new one with the correct block
      // label.
      o.replaceParameter(
        pos1, WIR_LabelParameter( mContext->mBlockOfLabel.at( label ) ) );

      // Remove the dummy string parameter.
      o.eraseParameter( pos2 );
    } else {
      // Label resolution failed, emit an error message.
      ufErrMsg << ufFile( mStreamName )
               << "Syntax error in inline assembly: Failed to resolve label "
               << "'" + label + "'." << endl;

      exit( 1 );
    }
  }
};

}       // namespace WIR
