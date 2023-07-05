/*

   This source file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Intermediate Representation Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2021 - 2022

   Hamburg University of Technology (TUHH)
   Institute of Embedded Systems
   21071 Hamburg
   Germany

   http://www.tuhh.de/es/esd/research/wcc

*/

/*!
  @file wirloopbound.cc
  @brief This file implements %WIR loop bounds.

  @author Til Mauersberger <Til.Mauersberger@tuhh.de>
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
  Default constructor.
*/
WIR_LoopBound::WIR_LoopBound( void ) :
  WIR_FlowFact {},
  mMin { -1 },
  mMax { -1 },
  mLoop { nullptr },
  mLoopControlType { LoopControlType::unknown }
{
  DSTART( "WIR_LoopBound::WIR_LoopBound()" );
};


/*
  Constructor creating a non-empty loop bound.

  Ideally, the basic block specified by the third parameter is the one
  containing the condition of the loop. The user is responsible for not
  annotating one loop by more than one loop bound!
*/
WIR_LoopBound::WIR_LoopBound( int _min, int _max, const WIR_BasicBlock &_loop,
                              LoopControlType _ctrl ) :
  WIR_FlowFact {}
{
  DSTART(
    "WIR_LoopBound::WIR_LoopBound(int, int, const WIR_BasicBlock&,"
    " WIR_LoopBound::LoopControlType)" );

  if ( ( _min < -1 ) || ( _max < _min ) ) {
    ufErrMsg << ufFile() << "WIR_LoopBound::WIR_LoopBound(...): "
             << "Data not valid, min: " << _min << ", max: " << _max << "."
             << endl;
    return;
  }

  if ( ( _min == -1 ) && ( _max != -1 ) ) {
    ufErrMsg << ufFile() << "WIR_LoopBound::WIR_LoopBound(...): "
             << "Data not valid, _min == -1, but _max is not." << endl;
    return;
  }

  ufAssertT( doStructuralAnalysis( _loop ), "Structural analysis failed." );

  mMin = _min;
  mMax = _max;
  mLoop = &_loop;
  mLoopControlType = _ctrl;
};


/*
  Copy constructor.

  The copy will not be inserted in any WIR system or referenced by any
  WIR_FlowFactRefs.
*/
WIR_LoopBound::WIR_LoopBound( const WIR_LoopBound &__o ) :
  WIR_FlowFact { __o },
  mMin { __o.mMin },
  mMax { __o.mMax },
  mLoop { __o.mLoop },
  mLoopControlType { __o.mLoopControlType }
{
  DSTART( "WIR_LoopBound::WIR_LoopBound(const WIR_LoopBound&)" );
};


/*
  Destructor.
*/
WIR_LoopBound::~WIR_LoopBound( void )
{
  DSTART( "virtual WIR_LoopBound::~WIR_LoopBound()" );

  // Remove this flow fact from the relevant FlowFactRef.
  if ( !isInserted() || ( mLoop == nullptr ) )
    return;

  eraseReference( *mLoop );
};


/*
  Copy-assignment operator.

  The copy will not be inserted in any WIR system or referenced by any
  WIR_FlowFactRefs.
*/
WIR_LoopBound & WIR_LoopBound::operator = ( const WIR_LoopBound &__o )
{
  DSTART( "WIR_LoopBound& WIR_LoopBound::operator=(const WIR_LoopBound&)" );

  WIR_FlowFact::operator = ( __o );

  mMin = __o.mMin;
  mMax = __o.mMax;
  mLoop = __o.mLoop;
  mLoopControlType = __o.mLoopControlType;

  return( *this );
};


/*
  getType returns the type of a %WIR flow fact, i.e., whether it is an entry
  point, flow restriction or loop bound.
*/
WIR_FlowFactType WIR_LoopBound::getType( void ) const
{
  DSTART( "virtual WIR_FlowFactType WIR_LoopBound::getType() const" );

  return( WIR_FlowFactType::loopbound );
};


/*
  The << operator dumps a WIR loop bound to an output stream.
*/
std::ostream & operator << ( std::ostream &os, const WIR_LoopBound &l )
{
  DSTART( "ostream& operator<<(ostream&, const WIR_LoopBound&)" );

  os << "Loop bound min: " << l.mMin << " max: " << l.mMax << " for: "
     << l.mLoop->getName() << endl;

  return( os );
};


/*
  setMin sets the minimum number of iterations.

  The new minimum has to be lower than the current maximum. If the minimum is
  set to -1 (no loop bound available), the maximum is also updated.
*/
void WIR_LoopBound::setMin( int min )
{
  DSTART( "void WIR_LoopBound::setMin(int)" );

  // Check data.
  if ( ( min > mMax ) || ( min  < -1 ) ) {
    ufWarnMsg << ufFile() << "Invalid lower loop bound" << min
              << " found, ignoring." << endl;
    return;
  }

  // In case of -1, both values have to be changed.
  if ( min == -1 )
    mMax = -1;

  mMin = min;
};


/*
  setMax sets the maximum number of iterations.

  The new maximum has to be larger than the current minimum. If the minimum is
  -1, it is set to zero, because a loop bound is now specified.
*/
void WIR_LoopBound::setMax( int max )
{
  DSTART( "void WIR_LoopBound::setMax(int)" );

  // Check data.
  if ( ( max < mMin ) || ( max < 0 ) ) {
    ufWarnMsg << ufFile() << "Invalid upper loop bound" << max
              << " found, ignoring." << endl;
    return;
  }

  // Giving an upper bound means a bound is known and the minimum number of
  // iterations is zero.
  if ( mMin == -1 )
    mMin = 0;

  mMax = max;
};


/*
  setLoop sets the loop this loop bound is referring to.

  Note: When using this method, the user is responsible not to annotate one loop
        with more than one loop bound (a loop can consist of more than one basic
        block!).

  This method invokes the update of FlowFactRefs.
*/
void WIR_LoopBound::setLoop( const WIR_BasicBlock &l, LoopControlType ctrl )
{
  DSTART(
    "void WIR_LoopBound::setLoop(const WIR_BasicBlock&, "
    "WIR_LoopBound::LoopControlType)" );

  if ( mLoop != nullptr )
    eraseReference( *mLoop );

  ufAssertT( doStructuralAnalysis( l ), "Structural analysis failed." );

  mLoop = &l;
  mLoopControlType = ctrl;

  addReference( *mLoop );
};


/*
  getMin returns the minimum number of iterations specified in this loop bound.
*/
int WIR_LoopBound::getMin( void ) const
{
  DSTART( "int WIR_LoopBound::getMin() const" );

  return( mMin );
};


/*
  getMax returns the maximum number of iterations specified in this loop bound.
*/
int WIR_LoopBound::getMax( void ) const
{
  DSTART( "int WIR_LoopBound::getMax() const" );

  return( mMax );
};


/*
  getLoop returns one basic block of the loop this loop bound annotates.
*/
const WIR_BasicBlock &WIR_LoopBound::getLoop( void ) const
{
  DSTART( "const WIR_BasicBlock& WIR_LoopBound::getLoop() const" );

  ufAssertT( mLoop != nullptr, "Loop must be set before accessing it." );

  return( *mLoop );
};


/*
  getLoopControlType returns the control type of the loop (e.g.,
  head-controlled).

  For some WCET analyzers, it may be required to add one additional iteration
  for head-controlled loops for the last back-edge invocation before the loop
  terminates (e.g., aiT needs this).
*/
WIR_LoopBound::LoopControlType WIR_LoopBound::getLoopControlType( void ) const
{
  DSTART(
    "WIR_LoopBound::LoopControlType WIR_LoopBound::getLoopControlType() "
    "const" );

  return( mLoopControlType );
};


/*
  isBoundSpecified returns whether actual loop bounds are specified.

  This method does not consider whether this loop bound refers to an actual loop
  or not.
*/
bool WIR_LoopBound::isBoundSpecified( void ) const
{
  DSTART( "bool WIR_LoopBound::isBoundSpecified() const" );

  if ( ( mMin >= 0 ) && ( mMax >= 0 ) && ( mMax >= mMin ) )
    return( true );

  return( false );
};


/*
  isSignificant returns whether a loop bound is significant for WCET calculation
  or not.
*/
bool WIR_LoopBound::isSignificant( void ) const
{
  DSTART( "virtual bool WIR_LoopBound::isSignificant() const" );

  return( isBoundSpecified() );
};


/*
  reorganize adjusts all references to WIR basic blocks stored by a flow fact
  after a deep copy of flow facts.
*/
void WIR_LoopBound::reorganize( const std::map<WIR_id_t, WIR_BasicBlock *> &blockIDMap )
{
  DSTART(
    "virtual void WIR_LoopBound::reorganize(const map<long long unsigned int, "
    "WIR_BasicBlock*>&)" );

  if ( mLoop == nullptr )
    return;

  // Skip this flow fact if no counterpart in the copied WIR is found.
  auto block_it = blockIDMap.find( mLoop->getID() );
  if ( block_it == blockIDMap.end() )
    return;

  // Assign the copied WIR basic block this loop bound should annotate.
  mLoop = block_it->second;
};


//
// Protected class methods
//

/*
  onInsert is called whenever this loop bound is added to a WIR_System.

  This method inserts a reference of itself into the FlowFactRef of the basic
  block this loop bound is specified for.
*/
void WIR_LoopBound::onInsert( WIR_System *s )
{
  DSTART( "virtual void WIR_LoopBound::onInsert(WIR_System*)" );

  // Call base class implementation to set mSystemPointer.
  WIR_FlowFact::onInsert( s );

  if ( mLoop )
    addReference( *mLoop );
};


/*
  clone creates a copy of a WIR loop bound.

  This method only calls the copy constructor and allocates a new WIR loop bound
  on the heap.
*/
WIR_FlowFact *WIR_LoopBound::clone( void ) const
{
  DSTART( "virtual WIR_FlowFact* WIR_LoopBound::clone() const" );

  return( new WIR_LoopBound( *this ) );
};


//
// Private class methods
//

/*
  doStructuralAnalysis applies WIR's structural analysis to the given basic
  block to determine if it in fact refers to a loop.
*/
bool WIR_LoopBound::doStructuralAnalysis( const WIR_BasicBlock &b ) const
{
  DSTART(
    "bool WIR_LoopBound::doStructuralAnalysis(const WIR_BasicBlock&) const" );

  // Fail if the basic block is not inserted.
    ufAssertT(
      b.isInserted(),
      "Invalid attempt to attach a loop bound to basic block '" + b.getName() +
      "' which is not inserted into a function." );
  if ( !b.isInserted() ) {
    ufWarnMsg << ufFile() << "Invalid attempt to attach a loop bound to basic "
              << "block '" << b.getName() << "' which is not inserted into a "
              << "function." << endl;
    return( false );
  }

  // Cleanup recursively clears all attached WIR_ControlTree objects. To be
  // called before returning from this function.
  auto cleanup = []( WIR_Container_API &c_api ) {
    c_api.eraseContainers( WIR_ControlTree::getContainerTypeID(), true );
  };

  // Perform structural analysis.
  WIR_StructuralAnalysis { b.getFunction() }.analyze();

  auto &controlTree = b.getContainers<WIR_ControlTree>().begin()->get();
  auto &leaf = controlTree.getBasicBlockTreeNode();
  auto &parentNode = leaf.getParent();

  reference_wrapper<WIR_ControlTreeNode> loopNodeRef { parentNode };
  while ( loopNodeRef.get().isAcyclic() ) {
    if ( loopNodeRef.get() == loopNodeRef.get().getParent() )
      break;
    loopNodeRef = loopNodeRef.get().getParent();
  }

  auto &loopNode = loopNodeRef.get();

  // Fail if the given basic block does not refer to a loop.
  ufAssertT(
    ( loopNode.getType() == WIR_CTNodeType::whileloop ) ||
    ( loopNode.getType() == WIR_CTNodeType::naturalloop ) ||
    ( loopNode.getType() == WIR_CTNodeType::selfloop ) ||
    ( loopNode.getType() == WIR_CTNodeType::improper ),
    "Invalid attempt to attach a loop bound to basic block '" + b.getName() +
    "' which is not part of a loop." );
  if ( ( loopNode.getType() != WIR_CTNodeType::whileloop ) &&
       ( loopNode.getType() != WIR_CTNodeType::naturalloop ) &&
       ( loopNode.getType() != WIR_CTNodeType::selfloop ) &&
       ( loopNode.getType() != WIR_CTNodeType::improper ) ) {
    cleanup( b.getFunction() );
    ufWarnMsg << ufFile() << "Invalid attempt to attach a loop bound to basic "
              << " block '" << b.getName() << "' which is not part of a loop."
              << endl;
    return( false );
  }

  DACTION(
    string type;

    switch ( loopNode.getType() ) {
      case WIR_CTNodeType::whileloop:
        type = "a while/for";
        break;

      case WIR_CTNodeType::naturalloop:
        type = "a natural";
        break;

      case WIR_CTNodeType::selfloop:
        type = "a self";
        break;

      default:
        type = "not a";
        break;
    }
    DOUT ( "The encountered loop is " << type << " loop." << endl ); );

  // Check if the given basic block is the loop's entrypoint.
  auto entryNodeRef = cref( loopNode.getEntry() );
  while ( entryNodeRef.get().getType() != WIR_CTNodeType::bb ) {
    if ( entryNodeRef.get() == entryNodeRef.get().getEntry() )
      break;
    entryNodeRef = entryNodeRef.get().getEntry();
  }

  if ( entryNodeRef.get().getEntry().getType() == WIR_CTNodeType::bb ) {
    auto &entryNode =
      static_cast<const WIR_BasicBlockTreeNode &>( entryNodeRef.get() );

    if ( entryNode.getBasicBlock() == b ) {
      cleanup( b.getFunction() );
      return( true );
    }
  }

  cleanup( b.getFunction() );
  ufAssertT(
    false,
    "Invalid attempt to attach a loop bound to basic block '" + b.getName() +
    "' which is not a loop exit." );
  ufWarnMsg << ufFile() << "Invalid attempt to attach a loop bound to basic "
            << "block '" << b.getName() << "' which is not a loop exit."
            << endl;

  return( false );
};

}       // namespace WIR
