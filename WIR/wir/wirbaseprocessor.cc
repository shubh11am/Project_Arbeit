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
  @file wirbaseprocessor.cc
  @brief This file implements basic generic %WIR processor architectures.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include standard headers
#include <map>

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


WIR_id_t WIR_BaseProcessor::mTypeID = 1;


//
// Public class methods
//

/*
  Default constructor creating an empty processor.
*/
WIR_BaseProcessor::WIR_BaseProcessor( void ) :
  WIR_SystemComponent { "nonempty" },
  mClockFrequency { 0 },
  mVoltage { 0.0f }
{
  DSTART( "WIR_BaseProcessor::WIR_BaseProcessor()" );
};


/*
  Copy constructor.
*/
WIR_BaseProcessor::WIR_BaseProcessor( const WIR_BaseProcessor &__o ) :
  WIR_SystemComponent { __o }
{
  DSTART( "WIR_BaseProcessor::WIR_BaseProcessor(const WIR_BaseProcessor&)" );

  copyProcessor( __o );
};


/*
  Move constructor.
*/
WIR_BaseProcessor::WIR_BaseProcessor( WIR_BaseProcessor &&__o ) :
  WIR_SystemComponent { move( __o ) },
  mPhRegPointers { move( __o.mPhRegPointers ) },
  mPhRegReferences { move( __o.mPhRegReferences ) },
  mProcessorName { move( __o.mProcessorName ) },
  mISAName { move( __o.mISAName ) },
  mClockFrequency { move( __o.mClockFrequency ) },
  mVoltage { move( __o.mVoltage ) },
  mSections { move( __o.mSections ) },
  mSectionReferences { move( __o.mSectionReferences ) }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  __o.mPhRegReferences.clear();
  __o.mPhRegPointers.clear();

  // Adjust the parent IDs of the physical registers.
  for ( WIR_PhysicalRegister &r : mPhRegReferences )
    r.onInsert( this );

  __o.mProcessorName.clear();
  __o.mISAName.clear();
  __o.mClockFrequency = 0;
  __o.mVoltage = 0.0f;

  __o.mSections.clear();
  __o.mSectionReferences.clear();

  // Adjust the parent IDs of the core's sections.
  for ( WIR_Section &s : mSectionReferences )
    s.onInsert( this );
};


/*
  Destructor.
*/
WIR_BaseProcessor::~WIR_BaseProcessor( void )
{
  DSTART( "virtual WIR_BaseProcessor::~WIR_BaseProcessor()" );
};


/*
  Copy-assignment operator.
*/
WIR_BaseProcessor & WIR_BaseProcessor::operator = ( const WIR_BaseProcessor &__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  WIR_SystemComponent::operator = ( __o );

  copyProcessor( __o );

  return( *this );
};


/*
  Move-assignment operator.
*/
WIR_BaseProcessor & WIR_BaseProcessor::operator = ( WIR_BaseProcessor &&__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  WIR_SystemComponent::operator = ( move( __o ) );

  mPhRegPointers = move( __o.mPhRegPointers );
  __o.mPhRegPointers.clear();
  mPhRegReferences = move( __o.mPhRegReferences );
  __o.mPhRegReferences.clear();

  // Adjust the parent IDs of the physical registers.
  for ( WIR_PhysicalRegister &r : mPhRegReferences )
    r.onInsert( this );

  mProcessorName = move( __o.mProcessorName );
  __o.mProcessorName.clear();
  mISAName = move( __o.mISAName );
  __o.mISAName.clear();
  mClockFrequency = move( __o.mClockFrequency );
  __o.mClockFrequency = 0;
  mVoltage = move( __o.mVoltage );
  __o.mVoltage = 0.0f;

  mSections = move( __o.mSections );
  __o.mSections.clear();
  mSectionReferences = move( __o.mSectionReferences );
  __o.mSectionReferences.clear();

  // Adjust the parent IDs of the core's sections.
  for ( WIR_Section &s : mSectionReferences )
    s.onInsert( this );

  return( *this );
};


/*
  getType returns the type of a system component, i.e., that it is a processor
  core.
*/
WIR_SystemComponentType WIR_BaseProcessor::getType( void ) const
{
  DSTART(
    "virtual WIR_SystemComponentType WIR_BaseProcessor::getType() const" );

  return( WIR_SystemComponentType::core );
};


/*
  getProcessorName returns a processor's name as given by its manufacturer.
*/
string WIR_BaseProcessor::getProcessorName( void ) const
{
  DSTART( "string WIR_BaseProcessor::getProcessorName() const" );

  return( mProcessorName );
};


/*
  getISAName returns a processor's ISA description.
*/
string WIR_BaseProcessor::getISAName( void ) const
{
  DSTART( "string WIR_BaseProcessor::getISAName() const" );

  return( mISAName );
};


/*
  getClockFrequency returns a processor's clock frequency.
*/
unsigned long long WIR_BaseProcessor::getClockFrequency( void ) const
{
  DSTART(
    "long long unsigned int WIR_BaseProcessor::getClockFrequency() const" );

  return( mClockFrequency );
};


/*
  getVoltage returns a processor core's voltage.
*/
float WIR_BaseProcessor::getVoltage( void ) const
{
  DSTART( "float WIR_BaseProcessor::getVoltage() const" );

  return( mVoltage );
};


/*
  Default constructor for addressing modes.
*/
WIR_BaseProcessor::AddressingMode::AddressingMode( const std::string &__s ) :
  WIR_InheritableEnum {},
  mName { __s }
{
  DSTART( "WIR_BaseProcessor::AddressingMode::AddressingMode(const string&)" );
};


/*
  Destructor.
*/
WIR_BaseProcessor::AddressingMode::~AddressingMode( void )
{
  DSTART( "virtual WIR_BaseProcessor::AddressingMode::~AddressingMode()" );
};


/*
  getName returns an addressing mode's specific name.
*/
std::string WIR_BaseProcessor::AddressingMode::getName( void ) const
{
  DSTART( "string WIR_BaseProcessor::AddressingMode::getName() const" );

  return( mName );
};


/*
  isCompatible returns whether one addressing mode is compatible with another.

  This compatibility check is used by WIR_Operation::checkParameters in order to
  verify that addressing mode parameters inserted into some %WIR operation
  actually match with the operation's format.

  By overloading this method in derived classes, specific compatibility checks
  for individual processor architectures can be realized.
*/
bool WIR_BaseProcessor::AddressingMode::isCompatible( const AddressingMode &t,
                                                      const OperationFormat &f,
                                                      const OpCode &o ) const
{
  DSTART(
    "virtual bool WIR_BaseProcessor::AddressingMode::isCompatible(const WIR_BaseProcessor::AddressingMode&, const WIR_BaseProcessor::OperationFormat&, const WIR_BaseProcessor::OpCode&) const" );

  // Avoid unused parameter warning.
  (void) f;
  (void) o;

  // In general, we simply check that the provided addressing mode also belongs
  // to the correct processor architecture.
  return( this->getProcessorTypeName() == t.getProcessorTypeName() );
};


/*
  Default constructor for conditions.
*/
WIR_BaseProcessor::Condition::Condition( const std::string &__s ) :
  WIR_InheritableEnum {},
  mName { __s }
{
  DSTART( "WIR_BaseProcessor::Condition::Condition(const string&)" );
};


/*
  getName returns a condition's specific name.
*/
std::string WIR_BaseProcessor::Condition::getName( void ) const
{
  DSTART( "string WIR_BaseProcessor::Condition::getName() const" );

  return( mName );
};


/*
  Default constructor for opcodes.
*/
WIR_BaseProcessor::OpCode::OpCode( const std::string &__s, bool __ma, bool __st,
                                   bool __ld, bool __mv, bool __cl, bool __ic,
                                   bool __rt, bool __cj, bool __uj, bool __ij,
                                   bool __ad, bool __se ) :
  WIR_InheritableEnum {},
  mName { __s },
  mIsMemoryAccess{ __ma },
  mIsMemoryStore { __st },
  mIsMemoryLoad { __ld },
  mIsMove { __mv },
  mIsCall { __cl },
  mIsIndirectCall { __ic },
  mIsReturn { __rt },
  mIsConditionalJump { __cj },
  mIsUnconditionalJump { __uj },
  mIsIndirectJump { __ij },
  mIsAsmDataDirective { __ad },
  mHasSideEffects { __se }
{
  DSTART(
    "WIR_BaseProcessor::OpCode::OpCode(const string&, bool, bool, bool, bool, bool, bool, bool, bool, bool, bool, bool, bool)" );
};


/*
  Destructor.
*/
WIR_BaseProcessor::OpCode::~OpCode( void )
{
  DSTART( "virtual WIR_BaseProcessor::OpCode::~OpCode()" );
};


/*
  getName returns an opcode's specific mnemonic.
*/
std::string WIR_BaseProcessor::OpCode::getName( void ) const
{
  DSTART( "string WIR_BaseProcessor::OpCode::getName() const" );

  return( mName );
};


/*
  isMemoryAccess returns whether an opcode accesses memory without being an
  explicit load or store operation.

  By overloading this method in derived classes, specific tests for individual
  processor architectures can be realized.
*/
bool WIR_BaseProcessor::OpCode::isMemoryAccess( const WIR_Operation &o ) const
{
  DSTART(
    "virtual bool WIR_BaseProcessor::OpCode::isMemoryAccess(const WIR_Operation&) const" );

  (void) o;

  return( mIsMemoryAccess );
};


/*
  isMemoryStore returns whether an opcode writes to memory.

  By overloading this method in derived classes, specific tests for individual
  processor architectures can be realized.
*/
bool WIR_BaseProcessor::OpCode::isMemoryStore( const WIR_Operation &o ) const
{
  DSTART(
    "virtual bool WIR_BaseProcessor::OpCode::isMemoryStore(const WIR_Operation&) const" );

  (void) o;

  return( mIsMemoryStore );
};


/*
  isMemoryLoad returns whether an opcode reads from memory.

  By overloading this method in derived classes, specific tests for individual
  processor architectures can be realized.
*/
bool WIR_BaseProcessor::OpCode::isMemoryLoad( const WIR_Operation &o ) const
{
  DSTART(
    "virtual bool WIR_BaseProcessor::OpCode::isMemoryLoad(const WIR_Operation&) const" );

  (void) o;

  return( mIsMemoryLoad );
};


/*
  isMove returns whether an opcode is a register-register move.

  By overloading this method in derived classes, specific tests for individual
  processor architectures can be realized.
*/
bool WIR_BaseProcessor::OpCode::isMove( const WIR_Operation &o ) const
{
  DSTART(
    "virtual bool WIR_BaseProcessor::OpCode::isMove(const WIR_Operation&) const" );

  (void) o;

  return( mIsMove );
};


/*
  isCall returns whether an opcode calls a function.

  By overloading this method in derived classes, specific tests for individual
  processor architectures can be realized.
*/
bool WIR_BaseProcessor::OpCode::isCall( const WIR_Operation &o ) const
{
  DSTART(
    "virtual bool WIR_BaseProcessor::OpCode::isCall(const WIR_Operation&) const" );

  (void) o;

  return( mIsCall );
};


/*
  isIndirectCall returns whether an opcode indirectly calls a function.

  By overloading this method in derived classes, specific tests for individual
  processor architectures can be realized.
*/
bool WIR_BaseProcessor::OpCode::isIndirectCall( const WIR_Operation &o ) const
{
  DSTART(
    "virtual bool WIR_BaseProcessor::OpCode::isIndirectCall(const WIR_Operation&) const" );

  (void) o;

  return( mIsIndirectCall );
};


/*
  isReturn returns whether an opcode returns from a function call.

  By overloading this method in derived classes, specific tests for individual
  processor architectures can be realized.
*/
bool WIR_BaseProcessor::OpCode::isReturn( const WIR_Operation &o ) const
{
  DSTART(
    "virtual bool WIR_BaseProcessor::OpCode::isReturn(const WIR_Operation&) const" );

  (void) o;

  return( mIsReturn );
};


/*
  isConditionalJump returns whether an opcode performs a conditional jump.

  By overloading this method in derived classes, specific tests for individual
  processor architectures can be realized.
*/
bool WIR_BaseProcessor::OpCode::isConditionalJump( const WIR_Operation &o ) const
{
  DSTART(
    "virtual bool WIR_BaseProcessor::OpCode::isConditionalJump(const WIR_Operation&) const" );

  (void) o;

  return( mIsConditionalJump );
};


/*
  isUnconditionalJump returns whether an opcode performs an unconditional jump.

  By overloading this method in derived classes, specific tests for individual
  processor architectures can be realized.
*/
bool WIR_BaseProcessor::OpCode::isUnconditionalJump( const WIR_Operation &o ) const
{
  DSTART(
    "virtual bool WIR_BaseProcessor::OpCode::isUnconditionalJump(const WIR_Operation&) const" );

  (void) o;

  return( mIsUnconditionalJump );
};


/*
  isIndirectJump returns whether an opcode indirectly performs a jump.

  By overloading this method in derived classes, specific tests for individual
  processor architectures can be realized.
*/
bool WIR_BaseProcessor::OpCode::isIndirectJump( const WIR_Operation &o ) const
{
  DSTART(
    "virtual bool WIR_BaseProcessor::OpCode::isIndirectJump(const WIR_Operation&) const" );

  (void) o;

  return( mIsIndirectJump );
};


/*
  isAsmDataDirective returns whether an opcode is an assembly data directive.

  By overloading this method in derived classes, specific tests for individual
  processor architectures can be realized.
*/
bool WIR_BaseProcessor::OpCode::isAsmDataDirective( const WIR_Operation &o ) const
{
  DSTART(
    "virtual bool WIR_BaseProcessor::OpCode::isAsmDataDirective(const WIR_Operation&) const" );

  (void) o;

  return( mIsAsmDataDirective );
};


/*
  hasSideEffects returns whether an opcode has side effects.

  Side effects could be non-obvious changes of a processor's internal status
  like, e.g., disabling interrupts, changing into supervisor mode or
  invalidating cache lines etc.

  By overloading this method in derived classes, specific tests for individual
  processor architectures can be realized.
*/
bool WIR_BaseProcessor::OpCode::hasSideEffects( const WIR_Operation &o ) const
{
  DSTART(
    "virtual bool WIR_BaseProcessor::OpCode::hasSideEffects(const WIR_Operation&) const" );

  (void) o;

  return( mHasSideEffects );
};


/*
  Default constructor for register types.
*/
WIR_BaseProcessor::RegisterType::RegisterType( const std::string &__pp,
                                               const std::string &__pv,
                                               const std::string &__sp,
                                               const std::string &__sv,
                                               unsigned int __w ) :
  WIR_InheritableEnum {},
  mPrefix { __pp, __pv },
  mSuffix { __sp, __sv },
  mBitWidth { __w }
{
  DSTART(
    "WIR_BaseProcessor::RegisterType::RegisterType(const string&, const string&, const string&, const string&, unsigned int)" );
};


/*
  getPrefixes returns a register type's specific prefixes.
*/
const std::string ( & WIR_BaseProcessor::RegisterType::getPrefixes( void ) const )[ 2 ]
{
  DSTART(
    "const string (& WIR_BaseProcessor::RegisterType::getPrefixes() const)[2]" );

  return( mPrefix );
};


/*
  getSuffixes returns a register type's specific suffixes.
*/
const std::string ( & WIR_BaseProcessor::RegisterType::getSuffixes( void ) const )[ 2 ]
{
  DSTART(
    "const string (& WIR_BaseProcessor::RegisterType::getSuffixes() const)[2]" );

  return( mSuffix );
};


/*
  getBitWidth returns a register type's bit width.
*/
unsigned int WIR_BaseProcessor::RegisterType::getBitWidth( void ) const
{
  DSTART( "unsigned int WIR_BaseProcessor::RegisterType::getBitWidth() const" );

  return( mBitWidth );
};


/*
  isCompatible returns whether one register type is compatible with another.

  This compatibility check is used by WIR_Operation::checkParameters in order to
  verify that register parameters inserted into some WIR operation actually
  match with the operation's format.

  By overloading this method in derived classes, specific compatibility checks
  for individual processor architectures can be realized.
*/
bool WIR_BaseProcessor::RegisterType::isCompatible( const RegisterType &t,
                                                    const WIR_BaseRegister &ra,
                                                    const WIR_BaseRegister &rf ) const
{
  DSTART(
    "virtual bool WIR_BaseProcessor::RegisterType::isCompatible(const WIR_BaseProcessor::RegisterType&, const WIR_BaseRegister&, const WIR_BaseRegister&) const" );

  if ( rf.isPhysical() ) {
    // Special handling of physical registers required by an operation format.
    // We need to make sure that ra matches exactly the given physical register,
    // or that is precolored accordingly.
    if ( ra.isPhysical() ) {
      if ( ( ra.getName() == rf.getName() ) && ( *this == t ) )
        return( true );
      else
        return( false );
    }

    auto &vr = dynamic_cast<const WIR_VirtualRegister &>( ra );
    if ( vr.isPrecolored() && ( vr.getPrecolor().getName() == "r14" ) )
      return( true );
    else
      return( false );
  }

  return( *this == t );
};


/*
  Default constructor for operation formats.
*/
WIR_BaseProcessor::OperationFormat::OperationFormat( unsigned int __w,
                                                     const std::string &__s ) :
  WIR_InheritableEnum {},
  mBitWidth { __w },
  mSize { __w % 8 != 0 ? __w / 8 + 1 : __w / 8 },
  mName { __s }
{
  DSTART(
    "WIR_BaseProcessor::OperationFormat::OperationFormat(unsigned int, const string&)" );
};


/*
  getBitWidth returns an operation format's bit width.
*/
unsigned int WIR_BaseProcessor::OperationFormat::getBitWidth( void ) const
{
  DSTART(
    "unsigned int WIR_BaseProcessor::OperationFormat::getBitWidth() const" );

  return( mBitWidth );
};


/*
  getSize returns an operation format's size in bytes.

  getSize rounds upwards. I.e., if an operation format occupies 20 bits, getSize
  returns a size of 3 bytes.
*/
unsigned int WIR_BaseProcessor::OperationFormat::getSize( void ) const
{
  DSTART( "unsigned int WIR_BaseProcessor::OperationFormat::getSize() const" );

  return( mSize );
};


/*
  getName returns an operation format's specific name.
*/
std::string WIR_BaseProcessor::OperationFormat::getName( void ) const
{
  DSTART( "string WIR_BaseProcessor::OperationFormat::getName() const" );

  return( mName );
};


/*
  getPhRegs returns a vector containing all physical registers of a processor
  architecture.
*/
const std::vector<std::reference_wrapper<const WIR_PhysicalRegister>> WIR_BaseProcessor::getPhRegs( void ) const
{
  DSTART(
    "const vector<reference_wrapper<const WIR_PhysicalRegister> > WIR_BaseProcessor::getPhRegs() const" );

  vector<reference_wrapper<const WIR_PhysicalRegister>> res;

  for ( const WIR_PhysicalRegister &r : mPhRegReferences )
    res.push_back( r );

  return( res );
};


/*
  getPhRegs returns a vector containing all physical registers of a processor
  architecture of the specified type.
*/
const std::vector<std::reference_wrapper<const WIR_PhysicalRegister>> WIR_BaseProcessor::getPhRegs( const RegisterType &t ) const
{
  DSTART(
    "const vector<reference_wrapper<const WIR_PhysicalRegister> > WIR_BaseProcessor::getPhRegs(const WIR_BaseProcessor::RegisterType&) const" );

  vector<reference_wrapper<const WIR_PhysicalRegister>> res;

  for ( const WIR_PhysicalRegister &r : mPhRegReferences )
    if ( r.getType() == t )
      res.push_back( r );

  return( res );
};


/*
  getSections returns the set mSectionReferences.
*/
const WIR_SectionSet &WIR_BaseProcessor::getSections( void ) const
{
  DSTART( "const WIR_SectionSet& WIR_BaseProcessor::getSections() const" );

  return( mSectionReferences );
};


/*
  begin returns an iterator to the first basic block of a function.
*/
WIR_SectionSet::const_iterator WIR_BaseProcessor::begin( void ) const
{
  DSTART( "WIR_SectionSet::const_iterator WIR_BaseProcessor::begin() const" );

  return( mSectionReferences.begin() );
};


/*
  end returns an iterator to the end of a function's basic block list.
*/
WIR_SectionSet::const_iterator WIR_BaseProcessor::end( void ) const
{
  DSTART( "WIR_SectionSet::const_iterator WIR_BaseProcessor::end() const" );

  return( mSectionReferences.end() );
};


/*
  findSection finds a WIR_Section with the specified ID in set
  mSectionReferences.
*/
WIR_SectionSet::const_iterator WIR_BaseProcessor::findSection( WIR_id_t id ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  for ( auto it = mSectionReferences.begin();
        it != mSectionReferences.end(); ++it )
    if ( it->get().getID() == id )
      return( it );

  return( mSectionReferences.end() );
};


/*
  findSection finds a WIR_Section with the specified name in set
  mSectionReferences.
*/
WIR_SectionSet::const_iterator WIR_BaseProcessor::findSection( const std::string &n ) const
{
  DSTART(
    "WIR_SectionSet::const_iterator WIR_BaseProcessor::findSection(const string&) const" );

  for ( auto it = mSectionReferences.begin();
        it != mSectionReferences.end(); ++it )
    if ( it->get().getName() == n )
      return( it );

  return( mSectionReferences.end() );
};


/*
  findSectionByAt finds the WIR_Section with the specified Loaded Memory Address
  (LMA).
*/
WIR_SectionSet::const_iterator WIR_BaseProcessor::findSectionByAt( const WIR_MemoryAddress &a ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  for ( auto it = mSectionReferences.begin();
        it != mSectionReferences.end(); ++it )
    if ( it->get().getAt() == a )
      return( it );

  return( mSectionReferences.end() );
};


/*
  findSectionByStart finds the WIR_Section with the specified Virtual Memory
  Address (VMA).
*/
WIR_SectionSet::const_iterator WIR_BaseProcessor::findSectionByStart( const WIR_MemoryAddress &a ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  for ( auto it = mSectionReferences.begin();
        it != mSectionReferences.end(); ++it )
    if ( it->get().getStart() == a )
      return( it );

  return( mSectionReferences.end() );
};


/*
  getTextSection returns the default text section ".text".
*/
WIR_Section &WIR_BaseProcessor::getTextSection( void ) const
{
  DSTART( "WIR_Section& WIR_BaseProcessor::getTextSection() const" );

  return( findSection( ".text" )->get() );
};


/*
  getDataSection returns the default initialized data section ".data".
*/
WIR_Section &WIR_BaseProcessor::getDataSection( void ) const
{
  DSTART( "WIR_Section& WIR_BaseProcessor::getDataSection() const" );

  return( findSection( ".data" )->get() );
};


/*
  getBssSection returns the default uninitialized data section ".bss".
*/
WIR_Section &WIR_BaseProcessor::getBssSection( void ) const
{
  DSTART( "WIR_Section& WIR_BaseProcessor::getBssSection() const" );

  return( findSection( ".bss" )->get() );
};


/*
  getRODataSection returns the default read-only data section ".rodata".
*/
WIR_Section &WIR_BaseProcessor::getRODataSection( void ) const
{
  DSTART( "WIR_Section& WIR_BaseProcessor::getRODataSection() const" );

  return( findSection( ".rodata" )->get() );
};


/*
  containsSection returns whether set mSectionReferences contains a WIR_Section
  with the specified ID.
*/
bool WIR_BaseProcessor::containsSection( WIR_id_t id ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  for ( WIR_Section &item : mSectionReferences )
    if ( item.getID() == id )
      return( true );

  return( false );
};


/*
  containsSection returns whether set mSectionReferences contains a WIR_Section
  with the specified name.
*/
bool WIR_BaseProcessor::containsSection( const std::string &n ) const
{
  DSTART( "bool WIR_BaseProcessor::containsSection(const string&) const" );

  for ( WIR_Section &item : mSectionReferences )
    if ( item.getName() == n )
      return( true );

  return( false );
};


/*
  containsSection returns whether set mSectionReferences contains a WIR_Section
  with the specified Loaded Memory Address (LMA).
*/
bool WIR_BaseProcessor::containsSectionAt( const WIR_MemoryAddress &a ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  for ( WIR_Section &item : mSectionReferences )
    if ( item.getAt() == a )
      return( true );

  return( false );
};


/*
  containsSection returns whether set mSectionReferences contains a WIR_Section
  with the specified Virtual Memory Address (VMA).
*/
bool WIR_BaseProcessor::containsSectionStart( const WIR_MemoryAddress &a ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  for ( WIR_Section &item : mSectionReferences )
    if ( item.getStart() == a )
      return( true );

  return( false );
};


//
// Protected class methods
//

/*
  Default constructor registering a new processor type.
*/
WIR_BaseProcessor::Registrator::Registrator( WIR_id_t &id )
{
  DSTART( "WIR_BaseProcessor::Registrator::Registrator(WIR_id_t&)" );

  registerNewProcessorType( id );
};


/*
  touch is a dummy method that just serves to activate the initialization of
  static data members.

  Objects of class 'Registrator' are used as static initializers for class
  WIR_Processor. Since WIR_Processor is a templated class, instantiation of its
  static data members does not occur until these are explicitly referenced. For
  this purpose, this method is provided: WIR_Processor can 'touch' its static
  data member so that it will get initialized.
*/
void WIR_BaseProcessor::Registrator::touch( void )
{
  DSTART( "void WIR_BaseProcessor::Registrator::touch()" );
};


/*
  setProcessorName sets a processor's name as given by its manufacturer.
*/
void WIR_BaseProcessor::setProcessorName( const std::string &s )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  mProcessorName = s;
};


/*
  setProcessorName sets a processor's name as given by its manufacturer.
*/
void WIR_BaseProcessor::setProcessorName( std::string &&s )
{
  DSTART( "void WIR_BaseProcessor::setProcessorName(string&&)" );

  mProcessorName = move( s );
};


/*
  setISAName sets a processor's description of its instruction set architecture
  (ISA).
*/
void WIR_BaseProcessor::setISAName( const std::string &s )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  mISAName = s;
};


/*
  setISAName sets a processor's description of its instruction set architecture
  (ISA).
*/
void WIR_BaseProcessor::setISAName( std::string &&s )
{
  DSTART( "void WIR_BaseProcessor::setISAName(string&&)" );

  mISAName = move( s );
};


/*
  setClockFrequency sets a processor's clock frequency.
*/
void WIR_BaseProcessor::setClockFrequency( unsigned long long f )
{
  DSTART( "void WIR_BaseProcessor::setClockFrequency(long long unsigned int)" );

  mClockFrequency = f;
};


/*
  setVoltage sets a processor core's voltage.
*/
void WIR_BaseProcessor::setVoltage( float u )
{
  DSTART( "void WIR_BaseProcessor::setVoltage(float)" );

  mVoltage = u;
};


//
// Private class methods
//

/*
  getMaxDelay returns the maximum number of clock cycles for which a system
  component can delay a memory access.

  This function does not make real sense for processor cores but is inherited
  from WIR_SystemComponent. Thus, its implementation here is private and only a
  dummy.
*/
unsigned int WIR_BaseProcessor::getMaxDelay( const unsigned int d ) const
{
  DSTART(
    "virtual unsigned int WIR_BaseProcessor::getMaxDelay(unsigned int) const" );

  (void) d;
  return( 0 );
};


/*
  registerNewProcessorType registers a new processor type.
*/
void WIR_BaseProcessor::registerNewProcessorType( WIR_id_t &id )
{
  DSTART(
    "static void WIR_BaseProcessor::registerNewProcessorType(WIR_id_t&)" );

  id = mTypeID++;
};


/*
  copyProcessor performs actions common to the copy constructor and copy
  assignment operator of WIR processors.
*/
void WIR_BaseProcessor::copyProcessor( const WIR_BaseProcessor &__o )
{
  DSTART( "void WIR_BaseProcessor::copyProcessor(const WIR_BaseProcessor&)" );

  // Copy physical registers.
  map<WIR_id_t, WIR_PhysicalRegister *> registerIDMap;

  mPhRegReferences.clear();
  mPhRegPointers.clear();
  for ( WIR_PhysicalRegister &p : __o.mPhRegReferences ) {
    WIR_PhysicalRegister *ptr =
      static_cast<WIR_PhysicalRegister *>( p.clone() );
    mPhRegPointers.push_back( unique_ptr<WIR_PhysicalRegister>( ptr ) );

    mPhRegReferences.push_back( *ptr );

    registerIDMap[ p.getID() ] = ptr;
  }

  // Adjust parent/child relationship of hierarchical registers.
  for ( WIR_PhysicalRegister &p : __o.mPhRegReferences ) {
    WIR_PhysicalRegister &copy = *(registerIDMap[ p.getID() ] );

    for ( const WIR_PhysicalRegister &c : p )
      copy.pushBackChild( *(registerIDMap[ c.getID() ]) );

    if ( p.getParent() == p )
      copy.onInsert( this );
  }

  // Copy other attributes.
  mProcessorName = __o.mProcessorName;
  mISAName = __o.mISAName;
  mClockFrequency = __o.mClockFrequency;
  mVoltage = __o.mVoltage;

  // Copy sections.
  clearSections();
  for ( const WIR_Section &s : __o.mSections )
    insertSection( WIR_Section( s ) );
};


/*
  insertSection adds a new WIR_Section to the sets mSections and
  mSectionReferences.

  insertSection asserts if a section with the same name as an already existing
  section is to be inserted. The content of o is moved to the new set element.
*/
WIR_Section &WIR_BaseProcessor::insertSection( WIR_Section &&o )
{
  DSTART( "WIR_Section& WIR_BaseProcessor::insertSection(WIR_Section&&)" );

  ufAssertT(
    !containsSection( o.getName() ),
    "Illegal attempt to insert a section with already existing name '" <<
    o.getName() << "'." );

  auto it = mSections.emplace( o ).first;
  WIR_Section &s = const_cast<WIR_Section &>( *it );
  s.onInsert( this );
  mSectionReferences.insert( s );

  return( s );
};


/*
  ersaseSection removes a single WIR_Section from the specified position.

  eraseSection asserts if one of the default sections .text, .data, .bss or
  .data is removed. This destroys the removed element. This destroys the removed
  element.
*/
WIR_SectionSet::iterator WIR_BaseProcessor::eraseSection( WIR_SectionSet::const_iterator pos )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  ufAssertT(
    ( ( pos->get().getName() != ".text" ) &&
      ( pos->get().getName() != ".data" ) &&
      ( pos->get().getName() != ".bss" ) &&
      ( pos->get().getName() != ".rodata" ) ),
    "Illegal attempt to erase default section '" << pos->get().getName() <<
    "'." );

  auto it1 = mSections.find( *pos );

  if ( it1 != mSections.end() ) {
    auto it = mSectionReferences.erase( pos );
    mSections.erase( it1 );
    return( it );
  } else
    return( mSectionReferences.end() );
};


/*
  brief clearSections removes all elements from sets mSections and
  mSectionReferences.

  This destroys all removed elements.
*/
void WIR_BaseProcessor::clearSections( void )
{
  DSTART( "void WIR_BaseProcessor::clearSections()" );

  mSectionReferences.clear();
  mSections.clear();
};

}       // namespace WIR
