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
  @file wirparameter.cc
  @brief This file implements parameters of %WIR operations.

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
  Default constructor creating an empty parameter.
*/
WIR_Parameter::WIR_Parameter( void ) :
  WIR_ID_API {},
  WIR_Container_API {},
  mOperationPointer { nullptr },
  mImplicit { false },
  mDontOptimize { false }
{
  DSTART( "WIR_Parameter::WIR_Parameter()" );
};


/*
  Copy constructor.

  When copying a parameter that is inserted in some %WIR operation, the
  resulting copy will not be inserted in an operation.
*/
WIR_Parameter::WIR_Parameter( const WIR_Parameter &__o ) :
  WIR_ID_API { __o },
  WIR_Container_API { __o },
  mOperationPointer { nullptr },
  mImplicit { __o.mImplicit },
  mDontOptimize { __o.mDontOptimize }
{
  DSTART( "WIR_Parameter::WIR_Parameter(const WIR_Parameter&)" );
};


/*
  Move constructor.

  Trying to move a parameter that is inserted in some %WIR operation results in
  an assertion, since you are not allowed to move a parameter whose ownership is
  managed by an operation.
*/
WIR_Parameter::WIR_Parameter( WIR_Parameter &&__o ) :
  WIR_ID_API { move( __o ) },
  WIR_Container_API { move( __o ) },
  mOperationPointer { nullptr },
  mImplicit { __o.mImplicit },
  mDontOptimize { __o.mDontOptimize }
{
  DSTART( "WIR_Parameter::WIR_Parameter(WIR_Parameter&&)" );

  ufAssertT(
    __o.mOperationPointer == nullptr,
    "Invalid attempt to move a parameter out of its owning operation." );

  __o.mImplicit = false;
};


/*
  Destructor.
*/
WIR_Parameter::~WIR_Parameter( void )
{
  DSTART( "virtual WIR_Parameter::~WIR_Parameter()" );

  checkDontOptimize();
};


/*
  Copy-assignment operator.

  When copying a parameter that is inserted in some %WIR operation, the
  resulting copy will not be inserted in an operation.
*/
WIR_Parameter & WIR_Parameter::operator = ( const WIR_Parameter &__o )
{
  DSTART( "WIR_Parameter& WIR_Parameter::operator=(const WIR_Parameter&)" );

  WIR_Container_API::operator = ( __o );

  mOperationPointer = nullptr;
  mImplicit = __o.mImplicit;
  mDontOptimize = __o.mDontOptimize;

  return( *this );
};


/*
  Move-assignment operator.

  Trying to move a parameter that is inserted in some %WIR operation results in
  an assertion, since you are not allowed to move a parameter whose ownership is
  managed by an operation.
*/
WIR_Parameter & WIR_Parameter::operator = ( WIR_Parameter &&__o )
{
  DSTART( "WIR_Parameter& WIR_Parameter::operator=(WIR_Parameter&&)" );

  ufAssertT(
    __o.mOperationPointer == nullptr,
    "Invalid attempt to move a parameter out of its owning operation." );

  WIR_Container_API::operator = ( move( __o ) );

  mOperationPointer = nullptr;
  mImplicit = __o.mImplicit;
  __o.mImplicit = false;
  mDontOptimize = __o.mDontOptimize;

  return( *this );
};


//
// API implementations.
//

WIR_INSERTION_IMPL( WIR_Operation, Operation, WIR_Parameter );


/*
  setImplicit specifies a parameter as implicit or not.

  An implicit parameter will not be made explicit in a %WIR assembly code dump.
  Instead, such implicit parameters are hidden ones that internally reflect data
  dependencies. Using implicit parameters, e.g., additional def/use
  relationships can be modeled.
*/
void WIR_Parameter::setImplicit( bool i )
{
  DSTART( "void WIR_Parameter::setImplicit(bool)" );

  checkDontOptimize();

  mImplicit = i;

  // Verify that this parameter's operation still adheres to its correct format.
  if ( isInserted() )
    getOperation().checkOperationFormat();
};


/*
  isExplicit returns whether a parameter is explicit or not.
*/
bool WIR_Parameter::isExplicit( void ) const
{
  DSTART( "bool WIR_Parameter::isExplicit() const" );

  return( !mImplicit );
};


/*
  isImplicit returns whether a parameter is implicit or not.
*/
bool WIR_Parameter::isImplicit( void ) const
{
  DSTART( "bool WIR_Parameter::isImplicit() const" );

  return( mImplicit );
};


/*
  setDontOptimize sets whether a parameter can be modified or must not be
  changed by some optimization or transformation.
*/
void WIR_Parameter::setDontOptimize( bool f )
{
  DSTART( "void WIR_Parameter::setDontOptimize(bool)" );

  mDontOptimize = f;
};


/*
  getDontOptimize returns whether a parameter can be modified or must not be
  changed by some optimization or transformation.

  A parameter must not be modified if the parameter by itself has been marked
  as such using setDontOptimize, or if it is inserted into a %WIR operation that
  in turn must not be modified.
*/
bool WIR_Parameter::getDontOptimize( void ) const
{
  DSTART( "bool WIR_Parameter::getDontOptimize() const" );

  return(
    mDontOptimize || ( isInserted() && getOperation().getDontOptimize() ) );
};


/*
  The << operator dumps a WIR parameter to an output stream.
*/
std::ostream & operator << ( std::ostream &os, const WIR_Parameter &p )
{
  DSTART( "ostream& operator<<(ostream&, const WIR_Parameter&)" );

  switch ( p.getType() ) {

    case WIR_ParameterType::addr: {
      os << dynamic_cast<const WIR_AddressingModeParameter &>( p );

      break;
    }

    case WIR_ParameterType::cond: {
      os << dynamic_cast<const WIR_ConditionFieldParameter &>( p );

      break;
    }

    case WIR_ParameterType::imm: {
      os << dynamic_cast<const WIR_BaseImmediateParameter &>( p );

      break;
    }

    case WIR_ParameterType::label: {
      os << dynamic_cast<const WIR_LabelParameter &>( p );

      break;
    }

    case WIR_ParameterType::reg: {
      os << dynamic_cast<const WIR_RegisterParameter &>( p );

      break;
    }

    case WIR_ParameterType::str: {
      os << dynamic_cast<const WIR_StringParameter &>( p );

      break;
    }

    default: {
      ufAssertT( false, "Untreated parameter type detected!" );
      break;
    }
  }

  return( os );
};


//
// Protected class methods
//

/*
  checkDontOptimize checks whether a parameter must not be modified.

  If this parameter must not be modified, checkDontOptimize asserts.
*/
void WIR_Parameter::checkDontOptimize( void ) const
{
  DSTART( "void WIR_Parameter::checkDontOptimize() const" );

  ufAssertT(
    !getDontOptimize(),
    "Illegal attempt to modify a parameter that is set as 'don't optimize'!" );
};

}       // namespace WIR
