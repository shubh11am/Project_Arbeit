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
  @file wirdatainit.cc
  @brief This file implements initialization data for %WIR data objects.

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

// Include boost headers
#include <boost/current_function.hpp>

// Include libuseful headers
#include <libuseful/debugmacros.h>
#include <libuseful/io.h>
#include <libuseful/stringtools.h>

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
  Default constructor for initialization spaces.

  This constructor sets the type of the WIR_DataInit object to
  WIR_DataInitType::ispace.
*/
WIR_DataInit::WIR_DataInit( std::size_t s ) :
  mType { WIR_DataInitType::ispace },
  mValues { "0" },
  mSpaces { s }
{
  DSTART( "WIR_DataInit::WIR_DataInit(size_t)" );
};


/*
  Copy constructor.
*/
WIR_DataInit::WIR_DataInit( const WIR_DataInit &__o ) :
  mType { __o.mType },
  mValues { __o.mValues },
  mSpaces { __o.mSpaces }
{
  DSTART( "WIR_DataInit::WIR_DataInit(const WIR_DataInit&)" );
};


/*
  Move constructor.
*/
WIR_DataInit::WIR_DataInit( WIR_DataInit &&__o ) :
  mType { move( __o.mType ) },
  mValues { move( __o.mValues ) },
  mSpaces { move( __o.mSpaces ) }
{
  DSTART( "WIR_DataInit::WIR_DataInit(WIR_DataInit&&)" );

  __o.mType = WIR_DataInitType::ibyte;
  __o.mValues.clear();
  __o.mSpaces = 0;
};


/*
  Destructor.
*/
WIR_DataInit::~WIR_DataInit( void )
{
  DSTART( "WIR_DataInit::~WIR_DataInit()" );
};


/*
  Copy-assignment operator.
*/
WIR_DataInit & WIR_DataInit::operator = ( const WIR_DataInit &__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  mType = __o.mType;
  mValues = __o.mValues;
  mSpaces = __o.mSpaces;

  return( *this );
};


/*
  Move-assignment operator.
*/
WIR_DataInit & WIR_DataInit::operator = ( WIR_DataInit &&__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  mType = move( __o.mType );
  __o.mType = WIR_DataInitType::ibyte;
  mValues = move( __o.mValues );
  __o.mValues.clear();
  mSpaces = move( __o.mSpaces );
  __o.mSpaces = 0;

  return( *this );
};


/*
  getType returns the type of a %WIR initialization literal.
*/
WIR_DataInitType WIR_DataInit::getType( void ) const
{
  DSTART( "WIR_DataInitType WIR_DataInit::getType() const" );

  return( mType );
};


/*
  getValues returns an initializer's init literals.
*/
const list<string> &WIR_DataInit::getValues( void ) const
{
  DSTART( "const list<string>& WIR_DataInit::getValues() const" );

  return( mValues );
};


/*
  For initializers of type WIR_DataInitType::ispace, getSpace returns the number
  of skipped space bytes.
*/
size_t WIR_DataInit::getSpace( void ) const
{
  DSTART( "size_t WIR_DataInit::getSpace() const" );

  return( mSpaces );
};


/*
  escapeString converts special characters (e.g., line feeds or tabs) into an
  escaped version (e.g., \\n or \\t).
*/
std::string WIR_DataInit::escapeString( const std::string &s )
{
  DSTART( "static string WIR_DataInit::escapeString(const string&)" );

  string res = "";

  for ( auto c : s ) {
    switch ( c ) {
      case 8: {                           // Backspace
        res += "\\b";
        break;
      }

      case 9: {                           // Tab
        res += "\\t";
        break;
      }

      case 10: {                          // New Line
        res += "\\n";
        break;
      }

      case 12: {                          // Form Feed
        res += "\\f";
        break;
      }

      case 13: {                          // Carriage Return
        res += "\\r";
        break;
      }

      case 34: {                          // Double Quotes
        res += "\\\"";
        break;
      }

      case 92: {                          // Backslash
        res += "\\\\";
        break;
      }

      default: {
        if ( ( c >= 32 ) && ( c <= 126 ) )
          res += c;
        else
          res += charToOctal( c );

        break;
      }
    }
  }

  return( res );
};


/*
  unescapeString converts escape characters (e.g., \\n or \\t) into an unescaped
  version with special characters (e.g., \\n or \\t).
*/
string WIR_DataInit::unescapeString( const std::string &s )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  stringstream res;

  for ( auto it = s.begin(); it != s.end(); ++it ) {
    char c = *it;

    if ( c == '\\' ) {
      ++it;

      if ( it == s.end() )
        return( res.str() );

      c = *it;

      switch ( c ) {
        case 'b': {                       // Backspace
          res << "\b";
          break;
        }

        case 't': {                       // Tab
          res << "\t";
          break;
        }

        case 'n': {                       // New Line
          res << "\n";
          break;
        }

        case 'f': {                       // Form Feed
          res << "\f";
          break;
        }

        case 'r': {                       // Carriage Return
          res << "\r";
          break;
        }

        case '\"': {                       // Double Quotes
          res << "\"";
          break;
        }

        case '\\': {                       // Backslash
          res << "\\";
          break;
        }

        default: {
          int value = ( c - 48 ) * 64;

          if ( ++it == s.end() )
            return( res.str() );
          c = *it;

          value += ( c - 48 ) * 8;

          if ( ++it == s.end() )
            return( res.str() );
          c = *it;

          value += ( c - 48 );
          res << (char) value;

          break;
        }
      }
    } else
      res << c;
  }

  return( res.str() );
};


//
// Private class methods
//

/*
  Dummy function for adding initializer literals which does nothing.

  It only serves to terminate the recursion of the variadic method
  addInitializers.
*/
void WIR_DataInit::addInitializers( void ) const
{
  DSTART( "void WIR_DataInit::addInitializers() const" );
};

}       // namespace WIR
