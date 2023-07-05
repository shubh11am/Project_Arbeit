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
  @file wirsystemconfig.cc
  @brief This file implements a simple parser for %WIR system configuration
         files.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include standard headers
#include <fstream>

// Include libuseful headers
#include <libuseful/debugmacros.h>
#include <libuseful/exceptions.h>
#include <libuseful/io.h>
#include <libuseful/stringtools.h>

// Include WIR headers
#include <wir/wirsystemconfig.h>
#include <wir/wirmisc.h>


//
// Code section
//

namespace WIR {


using namespace std;


//
// Public class methods
//


/*
  Default constructor reading a system configuration file.

  This constructor asserts if the specified filename cannot be opened or if the
  configuration file contains syntax errors.
*/
WIR_SystemConfig::WIR_SystemConfig( const std::string &f )
{
  DSTART( "WIR_SystemConfig::WIR_SystemConfig(const string&)" );

  // Open configuration file.
  ifstream ifs( f );
  if ( ifs.fail() ) {
    ifs.close();
    ufAssertT( false, "Failed to open file '" << f << "'." );
  }

  // Read configuration file.
  enum PARSER_STATE {
    ERROR,
    START,
    EXPECT_PARAGRAPH,
    PARAGRAPH,
    PROPERTY
  } state = START;
  string line;
  unsigned int lineno = 1;

  if ( !getline( ifs, line ) )
    return;

  while ( 1 ) {
    string::size_type n;

    // Handle comments.
    if ( ( n = line.find( '#', 0 ) ) != string::npos ) {
      DOUT(
        "Skipping comment '" << line.substr( n, line.size() ) << "'" << endl );
      line.erase( n );
    }
    line = trim( line );

    if ( line.size() ) {

      switch( state ) {

        case START: {
          state = EXPECT_PARAGRAPH;
          continue;
        }

        case EXPECT_PARAGRAPH: {
          if ( line.at( 0 ) == '[' ) {
            // Paragraph start detected.
            n = line.find_first_of( ']' );
            if ( ( n != string::npos ) && ( n > 1 ) &&
                 ( n == line.size() - 1 ) ) {
              string name = trim( line.substr( 1, line.size() - 2 ) );

              // Create new config file paragraph.
              mParagraphs.emplace_back( Paragraph( name ) );
              mParagraphs.back().setLine( lineno );
              DOUT( "Creating new paragraph '" << name << "'" << endl );
              state = PARAGRAPH;
              break;
            }
          }

          state = ERROR;
          continue;
        }

        case PARAGRAPH: {
          if ( line[ 0 ] == '[' )
            state = EXPECT_PARAGRAPH;
          else
            state = PROPERTY;

          continue;
        }

        case PROPERTY: {
          if ( ( ( n = line.find( '=' ) ) != string::npos ) && ( n > 0 ) &&
               ( line.find( '=' ) == line.rfind( '=' ) ) ) {
            string key, value;

            key.assign( line, 0, n );
            value.assign( line, n + 1, line.size() );

            key = trim( key );
            value = trim( value );

            // Keys must not contain white spaces.
            if ( key.find( ' ' ) != string::npos ) {
              state = ERROR;
              continue;
            }

            if ( key == "type" ) {
              // Paragraph types are handeled separately here, they are not
              // managed as key/value pairs.
              if ( value == "bus" )
                mParagraphs.back().setType( WIR_SystemComponentType::bus );
              else

              if ( value == "cache" )
                mParagraphs.back().setType( WIR_SystemComponentType::cache );
              else

              if ( value == "core" )
                mParagraphs.back().setType( WIR_SystemComponentType::core );
              else

              if ( value == "memory" )
                mParagraphs.back().setType( WIR_SystemComponentType::memory );
              else

              if ( value == "section" )
                mParagraphs.back().setType( WIR_SystemComponentType::section );
              else

              if ( value == "system" )
                mParagraphs.back().setType( WIR_SystemComponentType::system );
              else {
                state = ERROR;
                continue;
              }
            } else {
              mParagraphs.back().addProperty( key, value, lineno );
              DOUT(
                "  Adding property '" << key << "' / '" << value << "' / " <<
                lineno << " to paragraph '" << mParagraphs.back().getName() <<
                "'" << endl );
            }

            break;
          }

          if ( line[ 0 ] == '[' )
            state = EXPECT_PARAGRAPH;
          else
            state = ERROR;

          continue;
        }

        case ERROR:
        default: {
          throw ufFatalError( f, lineno, "Syntax error.", false );
        }

      }
    }

    if ( !getline( ifs, line ) )
      return;
    ++lineno;
  }

  ifs.close();
};


/*
  Destructor.
*/
WIR_SystemConfig::~WIR_SystemConfig( void )
{
  DSTART( "WIR_SystemConfig::~WIR_SystemConfig()" );
};


/*
  Default constructor creating a named paragraph
*/
WIR_SystemConfig::Paragraph::Paragraph( const std::string &n ) :
  mName { n },
  mLine { 0 },
  mType { WIR_SystemComponentType::system },
  mTypeSet { false }
{
  DSTART( "WIR_SystemConfig::Paragraph::Paragraph(const string&)" );
};


/*
  getName returns a paragraph's specific name.
*/
string WIR_SystemConfig::Paragraph::getName( void ) const
{
  DSTART( "string WIR_SystemConfig::Paragraph::getName() const" );

  return( mName );
};


/*
  setLine sets the line number where a config file paragraph starts.
*/
void WIR_SystemConfig::Paragraph::setLine( unsigned int l )
{
  DSTART( "void WIR_SystemConfig::Paragraph::setLine(unsigned int)" );

  mLine = l;
};


/*
  getLine returns the line number where a config file paragraph starts.
*/
unsigned int WIR_SystemConfig::Paragraph::getLine( void ) const
{
  DSTART( "unsigned int WIR_SystemConfig::Paragraph::getLine() const" );

  return( mLine );
};


/*
  setType sets a paragraph's specific type.
*/
void WIR_SystemConfig::Paragraph::setType( const WIR_SystemComponentType t )
{
  DSTART(
    "void WIR_SystemConfig::Paragraph::setType(WIR_SystemComponentType)" );

  mType = t;
  mTypeSet = true;
};


/*
  getType returns a paragraph's specific type.
*/
WIR_SystemComponentType WIR_SystemConfig::Paragraph::getType( void ) const
{
  DSTART(
    "WIR_SystemComponentType WIR_SystemConfig::Paragraph::getType() const" );

  return( mType );
};


/*
  isTypeSet returns whether a paragraph's type has been specified.
*/
bool WIR_SystemConfig::Paragraph::isTypeSet( void ) const
{
  DSTART( "bool WIR_SystemConfig::Paragraph::isTypeSet() const" );

  return( mTypeSet );
};


/*
  addProperty adds the given key-value pair to a paragraph.
*/
void WIR_SystemConfig::Paragraph::addProperty( const std::string &k,
                                               const std::string &v,
                                               unsigned int l )
{
  DSTART(
    "void WIR_SystemConfig::Paragraph::addProperty(const string&, const string&, unsigned int)" );

  struct value val;
  val.value = v;
  val.line = l;

  mProperties.push_back( make_pair( k, val ) );
};


/*
  getProperties returns all properties associated with a paragraph.
*/
const std::list<std::pair<std::string, WIR_SystemConfig::Paragraph::value>> &WIR_SystemConfig::Paragraph::getProperties( void ) const
{
  DSTART(
    "const list<pair<string, WIR_SystemConfig::Paragraph::value> >& WIR_SystemConfig::Paragraph::getProperties() const" );

  return( mProperties );
};


/*
  getParagraphs returns all paragraphs read from a configuration file.
*/
const std::list<WIR_SystemConfig::Paragraph> &WIR_SystemConfig::getParagraphs( void ) const
{
  DSTART(
    "const list<WIR_SystemConfig::Paragraph>& WIR_SystemConfig::getParagraphs() const" );

  return( mParagraphs );
};

}       // namespace WIR
