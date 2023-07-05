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
  @file wiraddressingmodeparameter.cc
  @brief This file implements parameters representing addressing modes.

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
#include <libuseful/io.h>

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
  Default constructor for addressing mode parameters.
*/
WIR_AddressingModeParameter::WIR_AddressingModeParameter( const WIR_BaseProcessor::AddressingMode &__r ) :
  WIR_Parameter {},
  mMode { const_cast<WIR_BaseProcessor::AddressingMode *>( &__r ) }
{
  DSTART(
    "WIR_AddressingModeParameter::WIR_AddressingModeParameter(const WIR_BaseProcessor::AddressingMode&)" );
};


/*
  Copy constructor.
*/
WIR_AddressingModeParameter::WIR_AddressingModeParameter( const WIR_AddressingModeParameter &__o ) :
  WIR_Parameter { __o },
  mMode { __o.mMode }
{
  DSTART(
    "WIR_AddressingModeParameter::WIR_AddressingModeParameter(const WIR_AddressingModeParameter&)" );
};



/*
  Move constructor.
*/
WIR_AddressingModeParameter::WIR_AddressingModeParameter( WIR_AddressingModeParameter &&__o ) :
  WIR_Parameter { move( __o ) },
  mMode { move( __o.mMode ) }
{
  DSTART(
    "WIR_AddressingModeParameter::WIR_AddressingModeParameter(WIR_AddressingModeParameter&&)" );

  __o.mMode = nullptr;
};


/*
  Destructor.
*/
WIR_AddressingModeParameter::~WIR_AddressingModeParameter( void )
{
  DSTART(
    "virtual WIR_AddressingModeParameter::~WIR_AddressingModeParameter()" );
};


/*
  Copy-assignment operator.
*/
WIR_AddressingModeParameter & WIR_AddressingModeParameter::operator = ( const WIR_AddressingModeParameter &__o )
{
  DSTART(
    "WIR_AddressingModeParameter& WIR_AddressingModeParameter::operator=(const WIR_AddressingModeParameter&)" );

  WIR_Parameter::operator = ( __o );

  mMode = __o.mMode;

  return( *this );
};


/*
  Move-assignment operator.
*/
WIR_AddressingModeParameter & WIR_AddressingModeParameter::operator = ( WIR_AddressingModeParameter &&__o )
{
  DSTART(
    "WIR_AddressingModeParameter& WIR_AddressingModeParameter::operator=(WIR_AddressingModeParameter&&)" );

  WIR_Parameter::operator = ( move( __o ) );

  mMode = move( __o.mMode );
  __o.mMode = nullptr;

  return( *this );
};


/*
  getType returns the type of a WIR parameter, i.e., that it is an addressing
  mode parameter.
*/
WIR_ParameterType WIR_AddressingModeParameter::getType( void ) const
{
  DSTART(
    "virtual WIR_ParameterType WIR_AddressingModeParameter::getType() const" );

  return( WIR_ParameterType::addr );
};


/*
  setAddressingMode sets a parameter's actual addressing mode.
*/
void WIR_AddressingModeParameter::setAddressingMode( const WIR_BaseProcessor::AddressingMode &m )
{
  DSTART(
    "void WIR_AddressingModeParameter::setAddressingMode(const WIR_BaseProcessor::AddressingMode&)" );

  checkDontOptimize();

  mMode = const_cast<WIR_BaseProcessor::AddressingMode *>( &m );
};


/*
  getAddressingMode gets a parameter's addressing mode.
*/
WIR_BaseProcessor::AddressingMode &WIR_AddressingModeParameter::getAddressingMode( void ) const
{
  DSTART(
    "WIR_BaseProcessor::AddressingMode& WIR_AddressingModeParameter::getAddressingMode() const" );

  ufAssertT(
    mMode != nullptr,
    "Attempt to get addressing mode from a parameter that has previously " <<
    "been moved." );

  return( *mMode );
};


/*
  The << operator dumps a WIR addressing mode parameter to an output stream.
*/
std::ostream & operator << ( std::ostream &os,
                             const WIR_AddressingModeParameter &p )
{
  DSTART( "ostream& operator<<(ostream&, const WIR_AddressingModeParameter&)" );

  WIR_Registry::getAddressingModeParameterDumper(
    os.iword( WIR_ProcessorIO() ) )( os, p );

  return( os );
};


//
// Protected class methods
//

/*
  clone creates a copy of an addressing mode parameter.

  Clone just calls the corresponding copy constructor.
*/
WIR_Parameter *WIR_AddressingModeParameter::clone( void ) const
{
  DSTART( "virtual WIR_Parameter* WIR_AddressingModeParameter::clone() const" );

  return( new WIR_AddressingModeParameter( *this ) );
};

}       // namespace WIR
