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
  @file wirconditionfieldparameter.cc
  @brief This file implements parameters representing condition fields for
         predicated execution of operations.

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
  Default constructor for condition field parameters.
*/
WIR_ConditionFieldParameter::WIR_ConditionFieldParameter( const WIR_BaseProcessor::Condition &__c ) :
  WIR_Parameter {},
  mCondition { const_cast<WIR_BaseProcessor::Condition *>( &__c ) }
{
  DSTART(
    "WIR_ConditionFieldParameter::WIR_ConditionFieldParameter(const WIR_BaseProcessor::Condition&)" );
};


/*
  Copy constructor.
*/
WIR_ConditionFieldParameter::WIR_ConditionFieldParameter( const WIR_ConditionFieldParameter &__o ) :
  WIR_Parameter { __o },
  mCondition { __o.mCondition }
{
  DSTART(
    "WIR_ConditionFieldParameter::WIR_ConditionFieldParameter(const WIR_ConditionFieldParameter&)" );
};



/*
  Move constructor.
*/
WIR_ConditionFieldParameter::WIR_ConditionFieldParameter( WIR_ConditionFieldParameter &&__o ) :
  WIR_Parameter { move( __o ) },
  mCondition { move( __o.mCondition ) }
{
  DSTART(
    "WIR_ConditionFieldParameter::WIR_ConditionFieldParameter(WIR_ConditionFieldParameter&&)" );

  __o.mCondition = nullptr;
};


/*
  Destructor.
*/
WIR_ConditionFieldParameter::~WIR_ConditionFieldParameter( void )
{
  DSTART(
    "virtual WIR_ConditionFieldParameter::~WIR_ConditionFieldParameter()" );
};


/*
  Copy-assignment operator.
*/
WIR_ConditionFieldParameter & WIR_ConditionFieldParameter::operator = ( const WIR_ConditionFieldParameter &__o )
{
  DSTART(
    "WIR_ConditionFieldParameter& WIR_ConditionFieldParameter::operator=(const WIR_ConditionFieldParameter&)" );

  WIR_Parameter::operator = ( __o );

  mCondition = __o.mCondition;

  return( *this );
};


/*
  Move-assignment operator.
*/
WIR_ConditionFieldParameter & WIR_ConditionFieldParameter::operator = ( WIR_ConditionFieldParameter &&__o )
{
  DSTART(
    "WIR_ConditionFieldParameter& WIR_ConditionFieldParameter::operator=(WIR_ConditionFieldParameter&&)" );

  WIR_Parameter::operator = ( move( __o ) );

  mCondition = move( __o.mCondition );
  __o.mCondition = nullptr;

  return( *this );
};


/*
  getType returns the type of a WIR parameter, i.e., that it is a condition
  field parameter.
*/
WIR_ParameterType WIR_ConditionFieldParameter::getType( void ) const
{
  DSTART(
    "virtual WIR_ParameterType WIR_ConditionFieldParameter::getType() const" );

  return( WIR_ParameterType::cond );
};


/*
  setCondition sets a parameter's actual condition.
*/
void WIR_ConditionFieldParameter::setCondition( const WIR_BaseProcessor::Condition &c )
{
  DSTART(
    "void WIR_ConditionFieldParameter::setCondition(const WIR_BaseProcessor::Condition&)" );

  checkDontOptimize();

  mCondition = const_cast<WIR_BaseProcessor::Condition *>( &c );
};


/*
  getCondition gets a parameter's condition.
*/
WIR_BaseProcessor::Condition &WIR_ConditionFieldParameter::getCondition( void ) const
{
  DSTART(
    "WIR_BaseProcessor::Condition& WIR_ConditionFieldParameter::getCondition() const" );

  ufAssertT(
    mCondition != nullptr,
    "Attempt to get condition from a parameter that has previously " <<
    "been moved." );

  return( *mCondition );
};


/*
  The << operator dumps a WIR condition field parameter to an output stream.
*/
std::ostream & operator << ( std::ostream &os,
                             const WIR_ConditionFieldParameter &p )
{
  DSTART( "ostream& operator<<(ostream&, const WIR_ConditionFieldParameter&)" );

  WIR_Registry::getConditionFieldParameterDumper(
    os.iword( WIR_ProcessorIO() ) )( os, p );

  return( os );
};


//
// Protected class methods
//

/*
  clone creates a copy of a condition field parameter.

  Clone just calls the corresponding copy constructor.
*/
WIR_Parameter *WIR_ConditionFieldParameter::clone( void ) const
{
  DSTART( "virtual WIR_Parameter* WIR_ConditionFieldParameter::clone() const" );

  return( new WIR_ConditionFieldParameter( *this ) );
};

}       // namespace WIR
