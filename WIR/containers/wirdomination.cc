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
  @file wirdomination.cc
  @brief This file implements WIR containers representing sets of dominating
         basic blocks or instructions that are available at other blocks or
         instructions, resp.

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
  Default constructor.
*/
WIR_Domination::WIR_Domination( void ) :
  WIR_Container<WIR_Domination> {}
{
  DSTART( "WIR_Domination::WIR_Domination()" );
};


/*
  Destructor.
*/
WIR_Domination::~WIR_Domination( void )
{
  DSTART( "virtual WIR_Domination::~WIR_Domination()" );
};


/*
  isUnique returns whether domination sets are unique, i.e., whether at most one
  instance of this container type can be attached to a WIR class.
*/
bool WIR_Domination::isUnique( void ) const
{
  DSTART( "virtual bool WIR_Domination::isUnique() const" );

  return( true );
};


/*
  insertDominator adds a new dominator block to set mDominatorBlocks.
*/
void WIR_Domination::insertDominator( const WIR_BasicBlock &b )
{
  DSTART( "void WIR_Domination::insertDominator(const WIR_BasicBlock&)" );

  mDominatorBlocks.insert( const_cast<WIR_BasicBlock &>( b ) );
};


/*
  insertDominator adds a new dominator instruction to set
  mDominatorInstructions.
*/
void WIR_Domination::insertDominator( const WIR_Instruction &i )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  mDominatorInstructions.insert( const_cast<WIR_Instruction &>( i ) );
};


/*
  getDominatorBlocks returns the set mDominatorBlocks.
*/
const WIR_BasicBlockSet &WIR_Domination::getDominatorBlocks( void ) const
{
  DSTART(
    "const WIR_BasicBlockSet& WIR_Domination::getDominatorBlocks() const" );

  return( mDominatorBlocks );
};


/*
  getDominatorInstructions returns the set mDominatorInstructions.
*/
const WIR_InstructionSet &WIR_Domination::getDominatorInstructions( void ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  return( mDominatorInstructions );
};

}       // namespace WIR
