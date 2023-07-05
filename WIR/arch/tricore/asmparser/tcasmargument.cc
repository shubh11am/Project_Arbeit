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
  @file tcasmargument.cc
  @brief This file implements assembly operation arguments.

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

// Include local headers
#include "tcasmargument.h"


//
// Code section
//

namespace WIR {


using namespace std;


//
// Public class methods
//

/*
  Default constructor creating an argument of a given type.
*/
TC_AsmArgument::TC_AsmArgument( Type t )  :
  mType { t }
{
  DSTART( "TC_AsmArgument::TC_AsmArgument(TC_AsmArgument::Type)" );
};


/*
  Copy constructor.
*/
TC_AsmArgument::TC_AsmArgument( const TC_AsmArgument &a ) :
  mType { a.mType }
{
  DSTART( "TC_AsmArgument::TC_AsmArgument(const TC_AsmArgument&)" );
};


/*
  Destructor.
*/
TC_AsmArgument::~TC_AsmArgument( void )
{
  DSTART( "virtual TC_AsmArgument::~TC_AsmArgument()" );
};


/*
  Copy-assignment operator.
*/
TC_AsmArgument & TC_AsmArgument::operator = ( const TC_AsmArgument &__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  mType = __o.mType;

  return( *this );
};


/*
  setType sets an argument's specific type.
*/
void TC_AsmArgument::setType( Type t )
{
  DSTART( "void TC_AsmArgument::setType(TC_AsmArgument::Type)" );

  mType = t;
};


/*
  getType returns the type of an assembly operation argument.
*/
TC_AsmArgument::Type TC_AsmArgument::getType( void ) const
{
  DSTART( "TC_AsmArgument::Type TC_AsmArgument::getType() const" );

  return( mType );
};

}       // namespace WIR
