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
  @file wirbaseimmediateparameter.cc
  @brief This file implements basic generic %WIR immediate parameters.

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


WIR_id_t WIR_BaseImmediateParameter::mTypeID = 0;


//
// Public class methods
//

/*
  Default constructor creating an immediate parameter for the specified bit
  width.
*/
WIR_BaseImmediateParameter::WIR_BaseImmediateParameter( unsigned int __w ) :
  WIR_Parameter {},
  mBitWidth { __w }
{
  DSTART(
    "WIR_BaseImmediateParameter::WIR_BaseImmediateParameter(unsigned int)" );
};


/*
  Copy constructor.
*/
WIR_BaseImmediateParameter::WIR_BaseImmediateParameter( const WIR_BaseImmediateParameter &__o ) :
  WIR_Parameter { __o },
  mBitWidth { __o.mBitWidth }
{
  DSTART(
    "WIR_BaseImmediateParameter::WIR_BaseImmediateParameter(const WIR_BaseImmediateParameter&)" );
};


/*
  Move constructor.
*/
WIR_BaseImmediateParameter::WIR_BaseImmediateParameter( WIR_BaseImmediateParameter &&__o ) :
  WIR_Parameter { move( __o ) },
  mBitWidth { move( __o.mBitWidth ) }
{
  DSTART(
    "WIR_BaseImmediateParameter::WIR_BaseImmediateParameter(WIR_BaseImmediateParameter&&)" );

  __o.mBitWidth = 0;
};


/*
  Destructor.
*/
WIR_BaseImmediateParameter::~WIR_BaseImmediateParameter( void )
{
  DSTART( "virtual WIR_BaseImmediateParameter::~WIR_BaseImmediateParameter()" );
};


/*
  Copy-assignment operator.
*/
WIR_BaseImmediateParameter & WIR_BaseImmediateParameter::operator = ( const WIR_BaseImmediateParameter &__o )
{
  DSTART(
    "WIR_BaseImmediateParameter& WIR_BaseImmediateParameter::operator=(const WIR_BaseImmediateParameter&)" );

  WIR_Parameter::operator = ( __o );

  mBitWidth = __o.mBitWidth;

  return( *this );
};


/*
  Move-assignment operator.
*/
WIR_BaseImmediateParameter & WIR_BaseImmediateParameter::operator = ( WIR_BaseImmediateParameter &&__o )
{
  DSTART(
    "WIR_BaseImmediateParameter& WIR_BaseImmediateParameter::operator=(WIR_BaseImmediateParameter&&)" );

  WIR_Parameter::operator = ( move( __o ) );

  mBitWidth = move( __o.mBitWidth );
  __o.mBitWidth = 0;

  return( *this );
};


/*
  getType returns the type of a %WIR parameter, i.e., that it is an immediate
  parameter.
*/
WIR_ParameterType WIR_BaseImmediateParameter::getType( void ) const
{
  DSTART(
    "virtual WIR_ParameterType WIR_BaseImmediateParameter::getType() const" );

  return( WIR_ParameterType::imm );
};


/*
  getBitWidth returns immediate parameter's bit width.
*/
unsigned int WIR_BaseImmediateParameter::getBitWidth( void ) const
{
  DSTART( "unsigned int WIR_BaseImmediateParameter::getBitWidth() const" );

  return( mBitWidth );
};


/*
  isUnsigned returns whether an immediate parameter is unsigned or not.
*/
bool WIR_BaseImmediateParameter::isUnsigned( void ) const
{
  DSTART( "bool WIR_BaseImmediateParameter::isUnsigned() const" );

  return( !isSigned() );
};


/*
  The << operator dumps a WIR immediate parameter to an output stream.
*/
std::ostream & operator << ( std::ostream &os, const WIR_BaseImmediateParameter &p )
{
  DSTART( "ostream& operator<<(ostream&, const WIR_BaseImmediateParameter&)" );

  WIR_Registry::getImmediateParameterDumper(
    os.iword( WIR_ProcessorIO() ) )( os, p );

  return( os );
};


//
// Protected class methods
//

/*
  Default constructor registering a new container type.
*/
WIR_BaseImmediateParameter::Registrator::Registrator( WIR_id_t &id )
{
  DSTART(
    "WIR_BaseImmediateParameter::Registrator::Registrator(WIR_id_t&)" );

  registerNewImmediateType( id );
};


/*
  touch is a dummy method that just serves to activate the initialization of
  static data members.

  Objects of class 'Registrator' are used as static initializers for class
  WIR_ImmediateParameter. Since WIR_ImmediateParameter is a templated class,
  instantiation of its static data members does not occur until these are
  explicitly referenced. For this purpose, this method is provided:
  WIR_ImmediateParameter can 'touch' its static data member so that it will get
  initialized.
*/
void WIR_BaseImmediateParameter::Registrator::touch( void )
{
  DSTART( "void WIR_BaseImmediateParameter::Registrator::touch()" );
};


//
// Private class methods
//

/*
  registerNewImmediateType registers a new immediate parameter type.
*/
void WIR_BaseImmediateParameter::registerNewImmediateType( WIR_id_t &id )
{
  DSTART(
    "static void WIR_BaseImmediateParameter::registerNewImmediateType(WIR_id_t&)" );

  id = mTypeID++;
};

}       // namespace WIR
