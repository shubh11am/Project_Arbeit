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
  @file wirdataaccess.cc
  @brief This file implements %WIR containers storing data access information.

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
*/
WIR_DataAccess::WIR_DataAccess( WIR_DataSet &&s ) :
  WIR_Container<WIR_DataAccess> {},
  mData { move( s ) }
{
  DSTART( "WIR_DataAccess::WIR_DataAccess(WIR_DataSet&&)" );
};


/*
  Destructor.
*/
WIR_DataAccess::~WIR_DataAccess( void )
{
  DSTART( "virtual WIR_DataAccess::~WIR_DataAccess()" );
};


/*
  isUnique returns whether data access containers are unique, i.e., whether at
  most one instance of this container type can be attached to a WIR class.
*/
bool WIR_DataAccess::isUnique( void ) const
{
  DSTART( "virtual bool WIR_DataAccess::isUnique() const" );

  return( true );
};


/*
  addData adds a WIR data object to the set of accessed data objects.
*/
void WIR_DataAccess::addData( const WIR_Data &d )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  mData.insert( const_cast<WIR_Data &>( d ) );
};


/*
  getData returns all data objects of a data access container.
*/
const WIR_DataSet &WIR_DataAccess::getData( void ) const
{
  DSTART( "const WIR_DataSet& WIR_DataAccess::getData() const" );

  return( mData );
};


/*
  The << operator dumps a data access container to an output stream.
*/
std::ostream & operator << ( std::ostream &os, const WIR_DataAccess &o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  // Build a comment string.
  string c = "Data Access:";
  for ( WIR_Data &d : o.mData )
    c += string( " " ) + d.getName();

  // Dump the data access container as WIR comment.
  WIR_Registry::getCommentDumper( os.iword( WIR_ProcessorIO() ) )(
    os, WIR_Comment { c } );

  return( os );
};

}       // namespace WIR
