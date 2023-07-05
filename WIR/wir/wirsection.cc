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
  @file wirsection.cc
  @brief This file implements %WIR object file sections.

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
#include <wir/API/wirinsertionapi.h>
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
  Default constructor creating a named section.

  Default alignment and block values (10 and 3, resp.) are taken over from WCC's
  old memorylayout, for both ARM and TriCore. This constructor asserts if it is
  passed an empty string.
*/
WIR_Section::WIR_Section( const std::string &s, const WIR_MemoryRegion &r ) :
  WIR_ID_API {},
  mProcessorPointer { nullptr },
  mAlignment { 10 },
  mBlock { 3 },
  mFill { 0 },
  mRegion { const_cast<WIR_MemoryRegion *>( &r ) },
  mLoadRegion { nullptr }
{
  DSTART( "WIR_Section::WIR_Section(const string&, const WIR_MemoryRegion&)" );

  ufAssert( !s.empty() );
  mName = s;
};


/*
  Default constructor creating a named section.

  Default alignment and block values (10 and 3, resp.) are taken over from WCC's
  old memorylayout, for both ARM and TriCore. This constructor asserts if it is
  passed an empty string.
*/
WIR_Section::WIR_Section( std::string &&s, const WIR_MemoryRegion &r ) :
  WIR_ID_API {},
  mProcessorPointer { nullptr },
  mAlignment { 10 },
  mBlock { 3 },
  mFill { 0 },
  mRegion { const_cast<WIR_MemoryRegion *>( &r ) },
  mLoadRegion { nullptr }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  ufAssert( !s.empty() );
  mName = move( s );
  s.clear();
};


/*
  Copy constructor.
*/
WIR_Section::WIR_Section( const WIR_Section &__o ) :
  WIR_ID_API { __o },
  mProcessorPointer { nullptr },
  mName { __o.mName },
  mAlignment { __o.mAlignment },
  mBlock { __o.mBlock },
  mStart { __o.mStart },
  mAt { __o.mAt },
  mLength { __o.mLength },
  mFill { __o.mFill },
  mRegion { __o.mRegion },
  mLoadRegion { __o.mLoadRegion }
{
  DSTART( "WIR_Section::WIR_Section(const WIR_Section&)" );
};


/*
  Move constructor.
*/
WIR_Section::WIR_Section( WIR_Section &&__o ) :
  WIR_ID_API { move( __o ) },
  mProcessorPointer { nullptr },
  mName { move( __o.mName ) },
  mAlignment { move( __o.mAlignment ) },
  mBlock { move( __o.mBlock ) },
  mStart { std::move( __o.mStart ) },
  mAt { std::move( __o.mAt ) },
  mLength { move( __o.mLength ) },
  mFill { move( __o.mFill ) },
  mRegion { __o.mRegion },
  mLoadRegion { __o.mLoadRegion }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  ufAssertT(
    __o.mProcessorPointer == nullptr,
    "Invalid attempt to move a section out of its owning processor '" <<
    __o.getProcessor().getName() << "'." );

  __o.mName.clear();
  __o.mAlignment = 0;
  __o.mBlock = 0;
  __o.mStart = boost::optional<WIR_MemoryAddress>();
  __o.mAt = boost::optional<WIR_MemoryAddress>();
  __o.mLength = 0;
  __o.mFill = 0;
  __o.mRegion = nullptr;
  __o.mLoadRegion = nullptr;
};


/*
  Destructor.
*/
WIR_Section::~WIR_Section( void )
{
  DSTART( "virtual WIR_Section::~WIR_Section()" );
};


//
// API implementations.
//

WIR_INSERTION_IMPL( WIR_BaseProcessor, Processor, WIR_Section );


/*
  getName returns a section's specific name.
*/
string WIR_Section::getName( void ) const
{
  DSTART( "string WIR_Section::getName() const" );

  return( mName );
};


/*
  setAlignment sets the section's external alignment, i.e., the alignment of the
  section's start/end boundaries.

  The alignment denotes the number of least-significant zero bits that start/end
  addresses must have. An alignment of 3 means that the least-significant 3 bits
  are '0', i.e., that the resulting start/end addresses are multiples of 8.
*/
void WIR_Section::setAlignment( std::size_t a )
{
  DSTART( "void WIR_Section::setAlignment(size_t)" );

  mAlignment = a;

  WIR_BaseProcessor &p = getProcessor();
  if ( p.isInserted() )
    p.getSystem().invalidateSymbols();
};


/*
  getAlignment returns the section's external alignment, i.e., the alignment of
  the section's start/end boundaries.

  The alignment denotes the number of least-significant zero bits that start/end
  addresses must have. An alignment of 3 means that the least-significant 3 bits
  are '0', i.e., that the resulting start/end addresses are multiples of 8.
*/
size_t WIR_Section::getAlignment( void ) const
{
  DSTART( "size_t WIR_Section::getAlignment() const" );

  return( mAlignment );
};


/*
  setBlock sets the section's internal alignment, i.e., the alignment of the
  section's internal relocation counter after an advancement.

  The alignment denotes the number of least-significant zero bits that the
  relocation counter must have. An alignment of 3 means that the
  least-significant 3 bits are '0', i.e., that the resulting relocation counter
  is a multiple of 8.
*/
void WIR_Section::setBlock( std::size_t a )
{
  DSTART( "void WIR_Section::setBlock(size_t)" );

  mBlock = a;

  WIR_BaseProcessor &p = getProcessor();
  if ( p.isInserted() )
    p.getSystem().invalidateSymbols();
};


/*
  getBlock returns the section's internal alignment, i.e., the alignment of the
  section's internal relocation counter after an advancement.

  The alignment denotes the number of least-significant zero bits that the
  relocation counter must have. An alignment of 3 means that the
  least-significant 3 bits are '0', i.e., that the resulting relocation
  counter is a multiple of 8.
*/
size_t WIR_Section::getBlock( void ) const
{
  DSTART( "size_t WIR_Section::getBlock() const" );

  return( mBlock );
};


/*
  getStart returns a section's Virtual Memory Address (VMA).

  In most cases, LMA and VMA are the same. However, some ROM-based systems might
  need to copy a section from ROM into RAM during startup time. For this
  particular situation, the LMA denotes the ROM address to which a section is
  loaded, and the VMA denotes the RAM address to which the section is copied.
*/
WIR_MemoryAddress WIR_Section::getStart( void ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  return( mStart.get_value_or( WIR_SaturatingInt<unsigned int>( 0 ) ) );
};


/*
  isStartSet returns whether the section's VMA has been set previously.
*/
bool WIR_Section::isStartSet( void ) const
{
  DSTART( "bool WIR_Section::isStartSet() const" );

  return( mStart.is_initialized() );
};


/*
  getAt returns a section's Loaded Memory Address (LMA).

  In most cases, LMA and VMA are the same. However, some ROM-based systems might
  need to copy a section from ROM into RAM during startup time. For this
  particular situation, the LMA denotes the ROM address to which a section is
  loaded, and the VMA denotes the RAM address to which the section is copied.
*/
WIR_MemoryAddress WIR_Section::getAt( void ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  return( mAt.get_value_or( WIR_SaturatingInt<unsigned int>( 0 ) ) );
};


/*
  isAtSet returns whether the section's LMA has been set previously.
*/
bool WIR_Section::isAtSet( void ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  return( mAt.is_initialized() );
};


/*
  getLength returns a section's length in bytes.
*/
size_t WIR_Section::getLength( void )
{
  DSTART( "size_t WIR_Section::getLength()" );

  WIR_BaseProcessor &p = getProcessor();
  if ( p.isInserted() )
    p.getSystem().computeMemoryLayout();

  return( mLength );
};


/*
  getFill returns the fill pattern for uninitialized section intervals.
*/
unsigned short WIR_Section::getFill( void ) const
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  return( mFill );
};


/*
  getProcessor returns the WIR_MemoryRegion to which this section is attached.
*/
WIR_MemoryRegion &WIR_Section::getRegion( void ) const
{
  DSTART( "WIR_MemoryRegion& WIR_Section::getRegion() const" );

  return( *mRegion );
};


/*
  getLoadRegion returns a region from which this section is loaded during a
  system's boot time.

  If no load region was set previously, getLoadRegion fails with an assertion.
*/
WIR_MemoryRegion &WIR_Section::getLoadRegion( void ) const
{
  DSTART( "WIR_MemoryRegion& WIR_Section::getLoadRegion() const" );

  ufAssert( mLoadRegion != nullptr );

  return( *mLoadRegion );
};


/*
  isLoadRegionSet returns whether a load region has been set for this section
  previously.
*/
bool WIR_Section::isLoadRegionSet( void ) const
{
  DSTART( "bool WIR_Section::isLoadRegionSet() const" );

  return( mLoadRegion != nullptr );
};


//
// Private class methods
//

/*
  Copy-assignment operator.
*/
WIR_Section & WIR_Section::operator = ( const WIR_Section &__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  mName = __o.mName;
  mAlignment = __o.mAlignment;
  mBlock = __o.mBlock;
  mStart = __o.mStart;
  mAt = __o.mAt;
  mLength = __o.mLength;
  mFill = __o.mFill;
  mProcessorPointer = nullptr;
  mRegion = __o.mRegion;
  mLoadRegion = __o.mLoadRegion;

  return( *this );
};


/*
  Move-assignment operator.
*/
WIR_Section & WIR_Section::operator = ( WIR_Section &&__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  ufAssertT(
    __o.mProcessorPointer == nullptr,
    "Invalid attempt to move a section out of its owning processor '" <<
    __o.getProcessor().getName() << "'." );

  mName = move( __o.mName );
  __o.mName.clear();

  mAlignment = __o.mAlignment;
  __o.mAlignment = 0;

  mBlock = __o.mBlock;
  __o.mBlock = 0;

  mStart = std::move( __o.mStart );
  __o.mStart = boost::optional<WIR_MemoryAddress>();

  mAt = std::move( __o.mAt );
  __o.mAt = boost::optional<WIR_MemoryAddress>();

  mLength = move( __o.mLength );
  __o.mLength = 0;

  mFill = __o.mFill;
  __o.mFill = 0;

  mProcessorPointer = nullptr;

  mRegion = __o.mRegion;
  __o.mRegion = nullptr;

  mLoadRegion = __o.mLoadRegion;
  __o.mLoadRegion = nullptr;

  return( *this );
};


/*
  setStart sets a section's Virtual Memory Address (VMA).

  In most cases, LMA and VMA are the same. However, some ROM-based systems might
  need to copy a section from ROM into RAM during startup time. For this
  particular situation, the LMA denotes the ROM address to which a section is
  loaded, and the VMA denotes the RAM address to which the section is copied.
*/
void WIR_Section::setStart( const WIR_MemoryAddress &a )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  mStart = a;
};


/*
  setAt sets a section's Loaded Memory Address (LMA).

  In most cases, LMA and VMA are the same. However, some ROM-based systems might
  need to copy a section from ROM into RAM during startup time. For this
  particular situation, the LMA denotes the ROM address to which a section is
  loaded, and the VMA denotes the RAM address to which the section is copied.
*/
void WIR_Section::setAt( const WIR_MemoryAddress &a )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  mAt = a;
};


/*
  setLength sets a section's length in bytes.
*/
void WIR_Section::setLength( std::size_t l )
{
  DSTART( "void WIR_Section::setLength(size_t)" );

  mLength = l;
};


/*
  setFill sets the fill pattern for uninitialized section intervals.
*/
void WIR_Section::setFill( unsigned short f )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  mFill = f;
};


/*
  setLoadRegion sets a region from which this section is loaded during a
  system's boot time.
*/
void WIR_Section::setLoadRegion( WIR_MemoryRegion &r )
{
  DSTART( "void WIR_Section::setLoadRegion(WIR_MemoryRegion&)" );

  mLoadRegion = &r;
};

}       // namespace WIR
