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
  @file tcasmlabel.cc
  @brief This file implements label arguments.

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
#include <libuseful/stringtools.h>

// Include local headers
#include "tcasmlabel.h"


//
// Code section
//

namespace WIR {


using namespace std;


//
// Public class methods
//

/*
  Default constructor creating a label argument with a given name.
*/
TC_AsmLabel::TC_AsmLabel( const std::string &l ) :
  TC_AsmArgument { Type::DISP },
  mName { trim( l ) }
{
  DSTART( "TC_AsmLabel::TC_AsmLabel(const string&)" );
};


/*
  Copy constructor.
*/
TC_AsmLabel::TC_AsmLabel( const TC_AsmLabel &l ) :
  TC_AsmArgument { l },
  mName { trim( l.mName ) }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Destructor.
*/
TC_AsmLabel::~TC_AsmLabel( void )
{
  DSTART( "virtual TC_AsmLabel::~TC_AsmLabel()" );
};


/*
  Copy-assignment operator.
*/
TC_AsmLabel & TC_AsmLabel::operator = ( const TC_AsmLabel &__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  TC_AsmArgument::operator = ( __o );

  mName = __o.mName;

  return( *this );
};


/*
  isCompatible returns whether a label is compatible with a given argument type.
*/
bool TC_AsmLabel::isCompatible( Type t ) const
{
  DSTART(
    "virtual bool TC_AsmLabel::isCompatible(TC_AsmArgument::Type) const" );

  return(
    static_cast<unsigned long long>( t ) &
    static_cast<unsigned long long>( Type::DISP ) );
};


/*
  getName returns a label argument's name.
*/
std::string TC_AsmLabel::getName( void ) const
{
  DSTART( "string TC_AsmLabel::getName() const" );

  return( mName );
};


//
// Protected class methods
//

/*
  clone creates a copy of a label argument.
*/
TC_AsmLabel *TC_AsmLabel::clone( void ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  return( new TC_AsmLabel( *this ) );
};

}       // namespace WIR
