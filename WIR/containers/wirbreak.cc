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
  @file wirbreak.cc
  @brief This file implements a %WIR container marking branches resulting from
         ANSI-C break statements.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

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
WIR_Break::WIR_Break( void ) :
  WIR_Container<WIR_Break> {}
{
  DSTART( "WIR_Break::WIR_Break()" );
};


/*
  Destructor.
*/
WIR_Break::~WIR_Break( void )
{
  DSTART( "virtual WIR_Break::~WIR_Break()" );
};


/*
  isUnique returns whether break containers are unique, i.e., whether at most
  one instance of this container type can be attached to a WIR class.
*/
bool WIR_Break::isUnique( void ) const
{
  DSTART( "virtual bool WIR_Break::isUnique() const" );

  return( true );
};

}       // namespace WIR
