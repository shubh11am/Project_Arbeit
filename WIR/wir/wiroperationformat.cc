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
  @file wiroperationformat.cc
  @brief This file implements %WIR operation formats.

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
  Destructor.
*/
WIR_OperationFormat::~WIR_OperationFormat( void )
{
  DSTART( "WIR_OperationFormat::~WIR_OperationFormat()" );
};


/*
  getParameters returns the list mParameterReferences.
*/
const std::list<std::reference_wrapper<WIR_Parameter>> &WIR_OperationFormat::getParameters( void ) const
{
  DSTART(
    "const list<reference_wrapper<WIR_Parameter> >& WIR_OperationFormat::getParameters() const" );

  return( mParameterReferences );
};


/*
  begin returns an iterator to the first parameter of an operation format.
*/
std::list<std::reference_wrapper<WIR_Parameter>>::const_iterator WIR_OperationFormat::begin( void ) const
{
  DSTART(
    "list<reference_wrapper<WIR_Parameter> >::const_iterator WIR_OperationFormat::begin() const" );

  return( mParameterReferences.begin() );
};


/*
  end returns an iterator to the end of an operation format's parameter list.
*/
std::list<std::reference_wrapper<WIR_Parameter>>::const_iterator WIR_OperationFormat::end( void ) const
{
  DSTART(
    "list<reference_wrapper<WIR_Parameter> >::const_iterator WIR_OperationFormat::end() const" );

  return( mParameterReferences.end() );
};


//
// Private class methods
//

/*
  Move constructor.
*/
WIR_OperationFormat::WIR_OperationFormat( WIR_OperationFormat &&__o ) :
  mParameterPointers { move( __o.mParameterPointers ) },
  mParameterReferences { move( __o.mParameterReferences) }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  __o.mParameterPointers.clear();
  __o.mParameterReferences.clear();
};


/*
  Move-assignment operator.
*/
WIR_OperationFormat & WIR_OperationFormat::operator = ( WIR_OperationFormat &&__o )
{
  DSTART(
    "WIR_OperationFormat& WIR_OperationFormat::operator=(WIR_OperationFormat&&)" );

  mParameterPointers = move( __o.mParameterPointers );
  __o.mParameterPointers.clear();

  mParameterReferences = move( __o.mParameterReferences );
  __o.mParameterReferences.clear();

  return( *this );
};


/*
  Dummy function for adding parameters which does nothing.

  It only serves to terminate the recursion of the variadic method
  addParameters.
*/
void WIR_OperationFormat::addParameters( void ) const
{
  DSTART( "void WIR_OperationFormat::addParameters() const" );
};

}       // namespace WIR
