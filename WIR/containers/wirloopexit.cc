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
  @file wirloopexit.cc
  @brief This file implements a %WIR container marking branches that are regular
         exits of ANSI-C for and while-do loops.

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
WIR_LoopExit::WIR_LoopExit( bool b ) :
  WIR_Container<WIR_LoopExit> {},
  mExplicitSuccIsExit { b }
{
  DSTART( "WIR_LoopExit::WIR_LoopExit(bool)" );
};


/*
  Destructor.
*/
WIR_LoopExit::~WIR_LoopExit( void )
{
  DSTART( "virtual WIR_LoopExit::~WIR_LoopExit()" );
};


/*
  isUnique returns whether loop exit containers are unique, i.e., whether at
  most one instance of this container type can be attached to a WIR class.
*/
bool WIR_LoopExit::isUnique( void ) const
{
  DSTART( "virtual bool WIR_LoopExit::isUnique() const" );

  return( true );
};


/*
  brief explicitSuccessorIsExit returns whether the explicit or the implicit
  successor of a branch is the loop exit.
*/
bool WIR_LoopExit::explicitSuccessorIsExit( void ) const
{
  DSTART( "bool WIR_LoopExit::explicitSuccessorIsExit() const" );

  return( mExplicitSuccIsExit );
};

}       // namespace WIR
