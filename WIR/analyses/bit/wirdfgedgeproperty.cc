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
  @file wirdfgedgeproperty.cc
  @brief This file implements the basic properties of data flow graph edges.

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
#include <analyses/bit/wirdfg.h>


//
// Code section
//

namespace WIR {


using namespace std;


//
// Public class methods
//

/*
  Default constructor for data flow graph edges between two source and target
  register parameters.

  Upon creation of a DFG edge, the data flow from source to target is assumed to
  be unsigned, since the semantics of the source and target WIR operations are
  unknown here. The signedness of the up/down values attached to this edge thus
  has to be corrected later during up or down analysis where processor-specific
  handlers of the operations are involved.

  An edge's source parameter must be a defined or def-used register parameter.
  An edge's target parameter must be a used or def-used register parameter. Both
  register parameters have to refer to the same register. This constructor
  asserts if these preconditions are not met.
*/
WIR_DFGEdgeProperty::WIR_DFGEdgeProperty( const WIR_RegisterParameter &s,
                                          const WIR_RegisterParameter &t,
                                          const WIR_DFG &d ) :
  mSource { s },
  mTarget { t },
  mType { WIR_DFGNodeType::op },
  mDownValue { WIR_L4::bU, s.getRegister().getBitWidth(), false },
  mUpValue { WIR_L4::bU, s.getRegister().getBitWidth(), false },
  mIsFix { false },
  mDFG { &d }
{
  DSTART(
    "WIR_DFGEdgeProperty::WIR_DFGEdgeProperty(const WIR_RegisterParameter&, "
    "const WIR_RegisterParameter&, const WIR_DFG&)" );

  #ifdef FAILSAFEMODE
  ufAssert( s.isDefined() || s.isDefUsed() );
  ufAssert( t.isUsed() || t.isDefUsed() );
  ufAssert(
    ( s.getRegister() == t.getRegister() ) ||
    s.getRegister().isChildOf( t.getRegister() ) ||
    t.getRegister().isChildOf( s.getRegister() ) ||

    ( s.getRegister().isVirtual() &&
      dynamic_cast<WIR_VirtualRegister &>( s.getRegister() ).isPrecolored() &&
      dynamic_cast<WIR_VirtualRegister &>( s.getRegister() ).getPrecolor() ==
        t.getRegister() ) ||
    ( s.getRegister().isVirtual() &&
      dynamic_cast<WIR_VirtualRegister &>( s.getRegister() ).isPrecolored() &&
      dynamic_cast<WIR_VirtualRegister &>(
        s.getRegister() ).getPrecolor().isChildOf( t.getRegister() ) ) ||
    ( s.getRegister().isVirtual() &&
      dynamic_cast<WIR_VirtualRegister &>( s.getRegister() ).isPrecolored() &&
      t.getRegister().isChildOf(
        dynamic_cast<WIR_VirtualRegister &>(
          s.getRegister() ).getPrecolor() ) ) ||

    ( t.getRegister().isVirtual() &&
      dynamic_cast<WIR_VirtualRegister &>( t.getRegister() ).isPrecolored() &&
      dynamic_cast<WIR_VirtualRegister &>( t.getRegister() ).getPrecolor() ==
        s.getRegister() ) ||
    ( t.getRegister().isVirtual() &&
      dynamic_cast<WIR_VirtualRegister &>( t.getRegister() ).isPrecolored() &&
      dynamic_cast<WIR_VirtualRegister &>(
        t.getRegister() ).getPrecolor().isChildOf( s.getRegister() ) ) ||
    ( t.getRegister().isVirtual() &&
      dynamic_cast<WIR_VirtualRegister &>( t.getRegister() ).isPrecolored() &&
      s.getRegister().isChildOf(
        dynamic_cast<WIR_VirtualRegister &>( t.getRegister() ).getPrecolor() ) )
    );
  #endif

  // Check for back edges. Reachability analysis must have been done beforehand.
  auto &srcIns = s.getOperation().getInstruction();
  auto &tgtIns = t.getOperation().getInstruction();

  DOUT(
    "Checking edge '" << s << "' -> '" << t << "':" << endl <<
    "  srcInc = " << srcIns <<
    "  tgtIns = " << tgtIns <<
    "  isReachable( srcIns, tgtIns ) = " <<
    string(
      WIR_Reachability::isReachable( srcIns, tgtIns ) ? "true": "false" ) <<
    endl <<
    "  isReachable( srcIns, tgtIns, false ) = " <<
    string(
      WIR_Reachability::isReachable( srcIns, tgtIns, false ) ? "true": "false" ) <<
    endl );

  mIsBackEdge =
    ( WIR_Reachability::isReachable( srcIns, tgtIns ) &&
      !WIR_Reachability::isReachable( srcIns, tgtIns, false ) );
};


/*
  Default constructor for data flow graph edges denoting an input from an
  immediate value.
*/
WIR_DFGEdgeProperty::WIR_DFGEdgeProperty( const WIR_BaseImmediateParameter &p,
                                          const WIR_DFG &d ) :
  mSource { p },
  mTarget { p },
  mType { WIR_DFGNodeType::imm },
  mDownValue { p },
  mUpValue { p },
  mIsBackEdge { false },
  mIsFix { true },
  mDFG { &d }
{
  DSTART(
    "WIR_DFGEdgeProperty::WIR_DFGEdgeProperty(const "
    "WIR_BaseImmediateParameter&, const WIR_DFG&)" );
};


/*
  Default constructor for data flow graph edges denoting an input from a
  function-external register.

  Upon creation of a DFG edge, the data flow via a function-external register is
  assumed to be unsigned, since the semantics of the involved WIR operation are
  unknown here. The signedness of the up/down values attached to this edge thus
  has to be corrected later during up or down analysis where processor-specific
  handlers of the operations are involved.
*/
WIR_DFGEdgeProperty::WIR_DFGEdgeProperty( const WIR_RegisterParameter &p,
                                          const WIR_DFG &d ) :
  mSource { p },
  mTarget { p },
  mType { WIR_DFGNodeType::reg },
  mDownValue { p, false },
  mUpValue { p, false },
  mIsBackEdge { false },
  mIsFix { true },
  mDFG { &d }
{
  DSTART(
    "WIR_DFGEdgeProperty::WIR_DFGEdgeProperty(const WIR_RegisterParameter&, "
    "const WIR_DFG&)" );
};


/*
  Destructor.
*/
WIR_DFGEdgeProperty::~WIR_DFGEdgeProperty( void )
{
  DSTART( "WIR_DFGEdgeProperty::~WIR_DFGEdgeProperty()" );
};


/*
  getType returns the type of a WIR data flow graph edge, i.e., whether it is an
  operation or a immediate, label or register parameter.
*/
WIR_DFGNodeType WIR_DFGEdgeProperty::getType( void ) const
{
  DSTART( "WIR_DFGNodeType WIR_DFGEdgeProperty::getType() const" );

  return( mType );
};


/*
  If an edge represents data flow via a register, getSourceRegisterParameter
  returns the edge's source register parameter.

  An edge represents data flow via a register if the edge type is either
  'op' or 'reg'. getSourceRegisterParameter asserts if it is called for
  different edge types.
*/
const WIR_RegisterParameter &WIR_DFGEdgeProperty::getSourceRegisterParameter( void ) const
{
  DSTART(
    "const WIR_RegisterParameter& WIR_DFGEdgeProperty::getSourceRegisterParameter() const" );

  #ifdef FAILSAFEMODE
  ufAssert(
    ( mType == WIR_DFGNodeType::op ) || ( mType == WIR_DFGNodeType::reg ) );
  #endif

  return( dynamic_cast<const WIR_RegisterParameter &>( mSource ) );
};


/*
  If an edge represents data flow via a register, getTargetRegisterParameter
  returns the edge's target register parameter.

  An edge represents data flow via a register if the edge type is either
  'op' or 'reg'. getTargetRegisterParameter asserts if it is called for
  different edge types.
*/
const WIR_RegisterParameter &WIR_DFGEdgeProperty::getTargetRegisterParameter( void ) const
{
  DSTART(
    "const WIR_RegisterParameter& WIR_DFGEdgeProperty::getTargetRegisterParameter() const" );

  #ifdef FAILSAFEMODE
  ufAssert(
    ( mType == WIR_DFGNodeType::op ) || ( mType == WIR_DFGNodeType::reg ) );
  #endif

  return( dynamic_cast<const WIR_RegisterParameter &>( mTarget ) );
};


/*
  If an edge represents data flow via an immediate, getImmediateParameter
  returns the edge's immediate parameter.

  An edge represents data flow via an immediate if the edge type is 'imm'.
  getImmediateParameter asserts if it is called for different edge types.
*/
const WIR_BaseImmediateParameter &WIR_DFGEdgeProperty::getImmediateParameter( void ) const
{
  DSTART(
    "const WIR_BaseImmediateParameter& WIR_DFGEdgeProperty::getImmediateParameter() const" );

  #ifdef FAILSAFEMODE
  ufAssert( mType == WIR_DFGNodeType::imm );
  #endif

  return( dynamic_cast<const WIR_BaseImmediateParameter &>( mSource ) );
};


/*
  setDownValue sets an edge's bit-true down value.

  The edge's down value is only updated if v is bitwise higher in half-order L4
  than the edge's previous down value. If the edge's down value actually remains
  unmodified, the edge is marked to have reached a fixed point.
*/
void WIR_DFGEdgeProperty::setDownValue( WIR_UpDownValue &&v )
{
  DSTART( "void WIR_DFGEdgeProperty::setDownValue(WIR_UpDownValue&&)" );

  DOUT(
    "Current mDonwValue: " << mDownValue << endl <<
    "New down value: " << v << endl );

  // If nothing needs to be changed, the edge reached a fixed point.
  if ( mDownValue.isEqual( v ) ) {
    mIsFix = true;
    DOUT( "Resulting down value (1): " << mDownValue << endl );
    return;
  }

  // If the new down value is better than the actual one, update it.
  if ( v > mDownValue ) {
    mDownValue = move( v );
    mIsFix = false;
    DOUT( "Resulting down value (2): " << mDownValue << endl );
    return;
  }

  // If the new down value is worse than the actual one, simply discard it.
  if ( mDownValue > v ) {
    mIsFix = true;
    DOUT( "Resulting down value (3): " << mDownValue << endl );
    return;
  }

  // Otherwise, the new and the actual down values carry the same L4
  // information, but with different locations in some bit positions.
  auto tmpDownValue = move( v );

  for ( unsigned int i = 0; i < mDownValue.getBitWidth(); ++i ) {
    WIR_L4 b1 = mDownValue[ i ], b2 = tmpDownValue[ i ];

    if ( ( ( b1 == WIR_L4::bL ) && ( b2 == WIR_L4::bL ) ) ||
         ( ( b1 == WIR_L4::bN ) && ( b2 == WIR_L4::bN ) ) ) {
      auto &l1 = mDownValue.getLocation( i );
      auto &l2 = tmpDownValue.getLocation( i );

      if ( l1.isRegisterParameter() && l2.isRegisterParameter() &&
           ( mType == WIR_DFGNodeType::op ) ) {
        auto &rp1 = l1.getRegisterParameter();
        auto &rp2 = l2.getRegisterParameter();

        if ( ( rp1 == mSource ) && ( rp2 != mSource ) ) {
          // Location l1 refers to the edge's source parameter, so let's replace
          // l1 by l2.
          mDownValue.setBit( i, b2, l2 );
          mIsFix = false;
        } else {
          // Determine distances from rp1 and rp2 to o, resp. and choose the
          // location with larger distance.
          auto d1 =
            mDFG->getDistance( rp1.getOperation(), mSource.getOperation() );
          auto d2 =
            mDFG->getDistance( rp2.getOperation(), mSource.getOperation() );

          if ( d2 > d1 ) {
            mDownValue.setBit( i, b2, l2 );
            mIsFix = false;
          }
        }
      }
    }
  }

  DOUT( "Resulting down value (4): " << mDownValue << endl );
};


/*
  getDownValue provides an edge's bit-true down value.
*/
WIR_UpDownValue &WIR_DFGEdgeProperty::getDownValue( void )
{
  DSTART( "WIR_UpDownValue& WIR_DFGEdgeProperty::getDownValue()" );

  return( mDownValue );
};


/*
  setUpValue sets an edge's bit-true up value.

  The edge's up value is only updated if v is bitwise higher in half-order L4
  than the edge's previous up value. If the edge's up value actually remains
  unmodified, the edge is marked to have reached a fixed point.
*/
void WIR_DFGEdgeProperty::setUpValue( WIR_UpDownValue &&v )
{
  DSTART( "void WIR_DFGEdgeProperty::setUpValue(WIR_UpDownValue&&)" );

  // If nothing needs to be changed, the edge reached a fixed point.
  if ( mUpValue.isEqual( v ) ) {
    mIsFix = true;
    return;
  }

  // If the new up value is better than the actual one, update it.
  if ( v > mUpValue ) {
    mUpValue = move( v );
    mIsFix = false;
  } else
    // Otherwise, the new up value is worse than the actual one. Simply discard.
    mIsFix = true;
};


/*
  getUpValue provides an edge's bit-true up value.
*/
WIR_UpDownValue &WIR_DFGEdgeProperty::getUpValue( void )
{
  DSTART( "WIR_UpDownValue& WIR_DFGEdgeProperty::getUpValue()" );

  return( mUpValue );
};


/*
  initUpValue initializes an edge's up value with the current content of the
  edge's down value.
*/
void WIR_DFGEdgeProperty::initUpValue( void )
{
  DSTART( "void WIR_DFGEdgeProperty::initUpValue()" );

  mUpValue = mDownValue;
  mIsFix = false;
};


/*
  isBackEdge returns whether a DFG edge is a back edge.
*/
bool WIR_DFGEdgeProperty::isBackEdge( void ) const
{
  DSTART( "bool WIR_DFGEdgeProperty::isBackEdge() const" );

  return( mIsBackEdge );
};


/*
  isFix returns whether a DFG edge reached a fixed point or not.
*/
bool WIR_DFGEdgeProperty::isFix( void ) const
{
  DSTART( "bool WIR_DFGEdgeProperty::isFix() const" );

  return( mIsFix );
};


/*
  setFix sets whether a DFG edge reached a fixed point during analysis or not.
*/
void WIR_DFGEdgeProperty::setFix( bool f )
{
  DSTART( "void WIR_DFGEdgeProperty::setFix(bool)" );

  mIsFix = f;
};

}       // namespace WIR
