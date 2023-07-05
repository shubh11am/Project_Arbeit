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
  @file wirlabelparameter.cc
  @brief This file implements parameters representing labels.

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
  Default constructor for label parameters refering to basic blocks.
*/
WIR_LabelParameter::WIR_LabelParameter( const WIR_BasicBlock &__o ) :
  WIR_Parameter {},
  mLabelType { WIR_SymbolType::block },
  mReferedID { const_cast<WIR_BasicBlock *>( &__o ) }
{
  DSTART( "WIR_LabelParameter::WIR_LabelParameter(const WIR_BasicBlock&)" );
};


/*
  Default constructor for label parameters refering to data objects.
*/
WIR_LabelParameter::WIR_LabelParameter( const WIR_Data &__o ) :
  WIR_Parameter {},
  mLabelType { WIR_SymbolType::data },
  mReferedID { const_cast<WIR_Data *>( &__o ) }
{
  DSTART( "WIR_LabelParameter::WIR_LabelParameter(const WIR_Data&)" );
};


/*
  Default constructor for label parameters refering to functions.
*/
WIR_LabelParameter::WIR_LabelParameter( const WIR_Function &__o ) :
  WIR_Parameter {},
  mLabelType { WIR_SymbolType::function },
  mReferedID { const_cast<WIR_Function *>( &__o ) }
{
  DSTART( "WIR_LabelParameter::WIR_LabelParameter(const WIR_Function&)" );
};


/*
  Copy constructor.
*/
WIR_LabelParameter::WIR_LabelParameter( const WIR_LabelParameter &__o ) :
  WIR_Parameter { __o },
  mLabelType { __o.mLabelType },
  mReferedID { __o.mReferedID }
{
  DSTART( "WIR_LabelParameter::WIR_LabelParameter(const WIR_LabelParameter&)" );
};


/*
  Move constructor.
*/
WIR_LabelParameter::WIR_LabelParameter( WIR_LabelParameter &&__o ) :
  WIR_Parameter { move( __o ) },
  mLabelType { move( __o.mLabelType ) },
  mReferedID { __o.mReferedID }
{
  DSTART( "WIR_LabelParameter::WIR_LabelParameter(WIR_LabelParameter&&)" );
};


/*
  Destructor.
*/
WIR_LabelParameter::~WIR_LabelParameter( void )
{
  DSTART( "virtual WIR_LabelParameter::~WIR_LabelParameter()" );
};


/*
  Copy-assignment operator.
*/
WIR_LabelParameter & WIR_LabelParameter::operator = ( const WIR_LabelParameter &__o )
{
  DSTART(
    "WIR_LabelParameter& WIR_LabelParameter::operator=(const WIR_LabelParameter&)" );

  WIR_Parameter::operator = ( __o );

  mLabelType = __o.mLabelType;
  mReferedID = __o.mReferedID;

  return( *this );
};


/*
  Move-assignment operator.
*/
WIR_LabelParameter & WIR_LabelParameter::operator = ( WIR_LabelParameter &&__o )
{
  DSTART(
    "WIR_LabelParameter& WIR_LabelParameter::operator=(WIR_LabelParameter&&)" );

  WIR_Parameter::operator = ( move( __o ) );

  mLabelType = __o.mLabelType;
  mReferedID = __o.mReferedID;
  __o.mReferedID = nullptr;

  return( *this );
};


/*
  getType returns the type of a WIR parameter, i.e., that it is a label
  parameter.
*/
WIR_ParameterType WIR_LabelParameter::getType( void ) const
{
  DSTART(
    "virtual WIR_ParameterType WIR_LabelParameter::getType() const" );

  return( WIR_ParameterType::label );
};


/*
  getLabelType returns to which kind of entity a label parameter refers, i.e.,
  whether a label points to a WIR basic block or to a function etc.
*/
WIR_SymbolType WIR_LabelParameter::getLabelType( void ) const
{
  DSTART( "WIR_LabelType WIR_LabelParameter::getLabelType() const" );

  return( mLabelType );
};


/*
  In case the label refers to a WIR basic block, getBasicBlock returns the
  refered block.

  If the label does not refer to a basic block, getBasicBlock fails with an
  assertion.
*/
WIR_BasicBlock &WIR_LabelParameter::getBasicBlock( void ) const
{
  DSTART( "WIR_BasicBlock& WIR_LabelParameter::getBasicBlock() const" );

  ufAssertT(
    mReferedID != nullptr,
    "Invalid attempt to access a label that has been moved previously." );

  return( dynamic_cast<WIR_BasicBlock &>( *mReferedID ) );
};


/*
  In case the label refers to a WIR data object, getData returns the refered
  data object.

  If the label does not refer to a data object, getData fails with an assertion.
*/
WIR_Data &WIR_LabelParameter::getData( void ) const
{
  DSTART( "WIR_Data& WIR_LabelParameter::getData() const" );

  ufAssertT(
    mReferedID != nullptr,
    "Invalid attempt to access a label that has been moved previously." );

  return( dynamic_cast<WIR_Data &>( *mReferedID ) );
};


/*
  In case the label refers to a WIR function, getFunction returns the refered
  function.

  If the label does not refer to a function, getFunction fails with an
  assertion.
*/
WIR_Function &WIR_LabelParameter::getFunction( void ) const
{
  DSTART( "WIR_Function& WIR_LabelParameter::getFunction() const" );

  ufAssertT(
    mReferedID != nullptr,
    "Invalid attempt to access a label that has been moved previously." );

  return( dynamic_cast<WIR_Function &>( *mReferedID ) );
};


/*
  getName returns a label's specific name.
*/
string WIR_LabelParameter::getName( void ) const
{
  DSTART( "string WIR_LabelParameter::getName() const" );

  string res;

  switch ( getLabelType() ) {

    case WIR_SymbolType::block: {
      res = getBasicBlock().getName();
      break;
    }

    case WIR_SymbolType::data: {
      res = getData().getName();
      break;
    }

    case WIR_SymbolType::function: {
      res = getFunction().getName();
      break;
    }

    default: {
      ufAssertT( false, "Currently unsupported label type." );
      break;
    }

  }

  return( res );
};


/*
  The << operator dumps a WIR label parameter to an output stream.
*/
std::ostream & operator << ( std::ostream &os, const WIR_LabelParameter &p )
{
  DSTART( "ostream& operator<<(ostream&, const WIR_LabelParameter&)" );

  WIR_Registry::getLabelParameterDumper(
    os.iword( WIR_ProcessorIO() ) )( os, p );

  return( os );
};


//
// Protected class methods
//

/*
  clone creates a copy of a label parameter.

  Clone just calls the corresponding copy constructor.
*/
WIR_Parameter *WIR_LabelParameter::clone( void ) const
{
  DSTART( "virtual WIR_Parameter* WIR_LabelParameter::clone() const" );

  return( new WIR_LabelParameter( *this ) );
};

}       // namespace WIR
