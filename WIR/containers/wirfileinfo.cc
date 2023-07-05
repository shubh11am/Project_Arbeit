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
  @file wirfileinfo.cc
  @brief This file implements %WIR containers representing file locations.

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
  Default constructor.

  Any newline characters in the file name are removed, since this class models
  single-line names only.
*/
WIR_FileInfo::WIR_FileInfo( const std::string &s, unsigned long long l ) :
  WIR_Container<WIR_FileInfo> {},
  mFileName { s },
  mLineNumber { l }
{
  DSTART( "WIR_FileInfo::WIR_FileInfo(const string&, long long unsigned int)" );

  mFileName.erase(
    remove( mFileName.begin(), mFileName.end(), '\n'), mFileName.end() );
};


/*
  Default constructor.

  Any newline characters in the file name are removed, since this class models
  single-line names only.
*/
WIR_FileInfo::WIR_FileInfo( std::string &&s, unsigned long long l ) :
  WIR_Container<WIR_FileInfo> {},
  mFileName { move( s ) },
  mLineNumber { l }
{
  DSTART( "WIR_FileInfo::WIR_FileInfo(string&&, long long unsigned int)" );

  mFileName.erase(
    remove( mFileName.begin(), mFileName.end(), '\n'), mFileName.end() );

  s.clear();
};


/*
  Copy constructor.
*/
WIR_FileInfo::WIR_FileInfo( const WIR_FileInfo &__o ) :
  WIR_Container<WIR_FileInfo> { __o },
  mFileName { __o.mFileName },
  mLineNumber { __o.mLineNumber }
{
  DSTART( "WIR_FileInfo::WIR_FileInfo(const WIR_FileInfo&)" );
};


/*
  Move constructor.
*/
WIR_FileInfo::WIR_FileInfo( WIR_FileInfo &&__o ) :
  WIR_Container<WIR_FileInfo> { move( __o ) },
  mFileName { move( __o.mFileName ) },
  mLineNumber { __o.mLineNumber }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  __o.mFileName.clear();
};


/*
  Destructor.
*/
WIR_FileInfo::~WIR_FileInfo( void )
{
  DSTART( "virtual WIR_FileInfo::~WIR_FileInfo()" );
};


/*
  Copy-assignment operator.
*/
WIR_FileInfo & WIR_FileInfo::operator = ( const WIR_FileInfo &__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  WIR_Container<WIR_FileInfo>::operator = ( __o );

  mFileName = __o.mFileName;
  mLineNumber = __o.mLineNumber;

  return( *this );
};


/*
  Move-assignment operator.
*/
WIR_FileInfo & WIR_FileInfo::operator = ( WIR_FileInfo &&__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  WIR_Container<WIR_FileInfo>::operator = ( move( __o ) );

  mFileName = move( __o.mFileName);
  __o.mFileName.clear();
  mLineNumber = move( __o.mLineNumber );
  __o.mLineNumber = 0;

  return( *this );
};


/*
  isUnique returns whether file information is unique, i.e., whether at most one
  instance of this container type can be attached to a WIR class.
*/
bool WIR_FileInfo::isUnique( void ) const
{
  DSTART( "virtual bool WIR_FileInfo::isUnique() const" );

  return( true );
};


/*
  setFileName sets a file name.

  Any newline characters in the file name are removed, since this class models
  single-line names only.
*/
void WIR_FileInfo::setFileName( const std::string &s )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  mFileName = s;
  mFileName.erase(
    remove( mFileName.begin(), mFileName.end(), '\n'), mFileName.end() );
};


/*
  setFileName sets a file name.

  Any newline characters in the file name are removed, since this class models
  single-line names only.
*/
void WIR_FileInfo::setFileName( std::string &&s )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  mFileName = move( s );
  mFileName.erase(
    remove( mFileName.begin(), mFileName.end(), '\n'), mFileName.end() );
  s.clear();
};


/*
  getFileName returns the file name.
*/
std::string WIR_FileInfo::getFileName( void ) const
{
  DSTART( "string WIR_FileInfo::getFileName() const" );

  return( mFileName );
};


/*
  setLineNumber sets a line number.
*/
void WIR_FileInfo::setLineNumber( unsigned long long l )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  mLineNumber = l;
};


/*
  getLineNumber returns the line number.
*/
unsigned long long WIR_FileInfo::getLineNumber( void ) const
{
  DSTART( "long long unsigned int WIR_FileInfo::getLineNumber() const" );

  return( mLineNumber );
};


/*
  The << operator dumps WIR file information to an output stream.

  By applying processor-specific I/O manipulators to the output stream
  beforehand, this << operator can flexibly emit valid assembly output for
  arbitrary processor architectures.
*/
std::ostream & operator << ( std::ostream &os, const WIR_FileInfo &o )
{
  DSTART( "ostream& operator<<(ostream&, const WIR_FileInfo&)" );

  WIR_Registry::getFileInfoDumper( os.iword( WIR_ProcessorIO() ) )( os, o );

  return( os );
};

}       // namespace WIR
