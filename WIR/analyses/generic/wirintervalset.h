/*

   This header file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Intermediate Representation Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2015 - 2022, Heiko Falk, Timon Kelter.

   Hamburg University of Technology (TUHH)
   Institute of Embedded Systems
   21071 Hamburg
   Germany

   http://www.tuhh.de/es/esd/research/wcc

*/

/*!
  @file wirintervalset.h
  @brief This file provides the basic interface of generic sets of intervals.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_INTERVAL_SET_H
#define _WIR_INTERVAL_SET_H


//
// Include section
//

// Include standard headers
#include <initializer_list>
#include <limits>
#include <list>
#include <sstream>
#include <stdexcept>
#include <type_traits>

// Include boost headers
#include <boost/current_function.hpp>
#include <boost/iterator/iterator_facade.hpp>

// Include libuseful headers
#include <libuseful/debugmacros.h>

// Include WIR headers
#include <analyses/generic/wirinterval.h>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

template<typename BaseType> class WIR_Interval;


/*!
  @brief This enum represents different compression options for an interval set
         type.

  @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
*/
enum class WIR_IntervalCompression
{
  /*!
    @brief An interval set will only be compressed when its content is
           explicitly requested.

    An "explicit request" is a call to any getter or iterator function.
  */
  ondemand,

  /*!
    @brief An interval set will be stored in compressed form at all times.

    This means that compression will be carried out after each operation on the
    interval set.
  */
  permanent,

  /*!
    @brief Compression of an interval set will not be carried out at all.

    This means that neighbor intervals are never joined together.
  */
  none
};


/*!
  @brief Class WIR_IntervalSet represents a set of intervals.

  @tparam IntegerType The type of the integers to be used for the underlying
                      intervals.
  @tparam CompType The way in which the compression of the set is done.

  Within the set of intervals, adjacent intervals are joined together depending
  on the selected compression type. Upon removal of intervals from the set, the
  contained intervals may be split up.

  @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
*/
template<typename IntegerType, WIR_IntervalCompression CompType>
class WIR_IntervalSet
{

  public:

    //
    // Local type definitions.
    //

    //! The instantiated type of this class.
    using type = WIR_IntervalSet<IntegerType, CompType>;

    //! The integer base type of the intervals.
    using intType = IntegerType;

    //! The type of the contained intervals (as visible internally).
    using _intervalType = WIR_Interval<intType>;

    //! The type of the contained intervals (as visible to the outside).
    using intervalType = const _intervalType;

    //! The container type used to store the chunks (as visible internally).
    using _chunkSetType = std::list<_intervalType>;

    //! The container type used to store the chunks (as visible to the outside).
    using chunkSetType = const std::list<_intervalType>;

    //! The iterator type to iterate over the chunks.
    using iterator = typename chunkSetType::iterator;

    //! The iterator type to iterate over the chunks.
    using const_iterator = typename chunkSetType::const_iterator;


    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor for empty interval sets.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    WIR_IntervalSet( void )
    {
      DSTART( "WIR_IntervalSet<IntegerType, CompType>::WIR_IntervalSet()" );
    };

    /*!
      @brief Constructor creating the interval set [constant, constant].

      @param[in] constant The value to be used as interval.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    explicit WIR_IntervalSet( intType constant )
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      mChunks.emplace_back( constant );
      validate<WIR_IntervalCompression::permanent>();
    };

    /*!
      @brief Constructor creating an interval set with the given interval i.

      @param[in] i A const reference to the interval to be used.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    explicit WIR_IntervalSet( const intervalType &i )
    {
      DSTART(
        "WIR_IntervalSet<IntegerType, CompType>::WIR_IntervalSet(WIR_IntervalSet<IntegerType, CompType>::intervalType&)" );

      if ( !i.isValid() ) {
        throw std::invalid_argument( "The given interval must be valid!" );
      }
      mChunks.push_back( i );
      validate<WIR_IntervalCompression::permanent>();
    };

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    // cppcheck-suppress noExplicitConstructor
    WIR_IntervalSet( const type &__o ) :
      mChunks { __o.mChunks }
    {
      DSTART(
        "WIR_IntervalSet<IntegerType, CompType>::WIR_IntervalSet(const type&)" );
    };

    /*!
      @brief Copy constructor with different compression type.

      @tparam otherComp The compression type of the interval set to be copied.
      @param[in] __o A const reference to another object to be copied.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    template<WIR_IntervalCompression otherComp>
    WIR_IntervalSet( const WIR_IntervalSet<intType, otherComp> &__o ) :
      mChunks { __o.mChunks }
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      validate<WIR_IntervalCompression::permanent>();
    };

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    // cppcheck-suppress noExplicitConstructor
    WIR_IntervalSet( type &&__o ) :
      mChunks { std::move( __o.mChunks ) }
    {
      DSTART(
        "WIR_IntervalSet<IntegerType, CompType>::WIR_IntervalSet(WIR_IntervalSet<IntegerType, CompType>::type&&)" );

      __o.clear();
    };

    /*!
      @brief Move constructor with different compression type.

      @param[in] __o An R-value reference to another object to be moved.
      @tparam otherComp The compression type of the interval set to be copied.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    template<WIR_IntervalCompression otherComp>
    WIR_IntervalSet( WIR_IntervalSet<intType, otherComp> &&__o ) :
      mChunks { std::move( __o.mChunks ) }
    {
      DSTART(
        "WIR_IntervalSet<IntegerType, CompType>::WIR_IntervalSet(WIR_IntervalSet<IntegerType, otherComp>&&)" );

      __o.clear();

      validate<WIR_IntervalCompression::permanent>();
    };

    /*!
      @brief Destructor.
      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    virtual ~WIR_IntervalSet()
    {
      DSTART( "WIR_IntervalSet<IntegerType, CompType>::~WIR_IntervalSet()" );
    };

    // Interval sets with differing compression modes are effectively the same
    // type.
    friend class WIR_IntervalSet<intType, WIR_IntervalCompression::ondemand>;
    friend class WIR_IntervalSet<intType, WIR_IntervalCompression::permanent>;
    friend class WIR_IntervalSet<intType, WIR_IntervalCompression::none>;


    //
    // Getters / setters.
    //

    /*!
      @brief getIntervals returns the set of chunks.

      @return A const reference to the list mChunks of intervals.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    chunkSetType &getIntervals( void ) const
    {
      DSTART(
        "WIR_IntervalSet<IntegerType, CompType>::chunkSetType& WIR_IntervalSet<IntegerType, CompType>::getIntervals() const" );

      validate<WIR_IntervalCompression::ondemand>();
      return( mChunks );
    };

    /*!
      @brief getIntervalCount returns the number of intervals contained in this
             set.

      @return The size of the list mChunks of intervals.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    size_t getIntervalCount( void ) const
    {
      DSTART(
        "size_t WIR_IntervalSet<IntegerType, CompType>::getIntervalCount() const" );

      validate<WIR_IntervalCompression::ondemand>();
      return( mChunks.size() );
    };

    /*!
      @brief getWidth returns the accumulated width of all contained intervals.

      @return The accumulated width over all intervals.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    intType getWidth( void ) const
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      validate<WIR_IntervalCompression::ondemand>();

      intType res = 0;
      for ( _intervalType &i : mChunks )
        res += i.getWidth();

      return( res );
    };

    /*!
      @brief getHull returns the hull of the interval set
             ([minValue, maxValue]).

      @return The interval representing the hull over the entire set.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    intervalType getHull( void ) const
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      validate<WIR_IntervalCompression::ondemand>();

      return(
        intervalType( mChunks.front().getLower(), mChunks.back().getUpper() ) );
    };

    /*!
      @brief isEmpty returns whether the interval set is empty or not.

      @return True iff the interval set is empty, false otherwise.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    bool isEmpty( void ) const
    {
      DSTART( "bool WIR_IntervalSet<IntegerType, CompType>::isEmpty() const" );

      return( mChunks.empty() );
    };

    /*!
      @brief clear removes all chunkgs from this interval set.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    void clear( void )
    {
      DSTART( "void WIR_IntervalSet<IntegerType, CompType>::clear()" );

      mChunks.clear();
    };


    //
    // Iterators.
    //

    /*!
      @brief begin returns an iterator to the beginning of this interval set.

      @return A const iterator pointing to the first interval in the set.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    const_iterator begin( void ) const
    {
      DSTART(
        "WIR_IntervalSet<IntegerType, CompType>::const_iterator WIR_IntervalSet<IntegerType, CompType>::begin() const" );

      validate<WIR_IntervalCompression::ondemand>();
      return( mChunks.begin() );
    };

    /*!
      @brief begin returns an iterator to the beginning of this interval set.

      @return An iterator pointing to the first interval in the set.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    iterator begin( void )
    {
      DSTART(
        "WIR_IntervalSet<IntegerType, CompType>::iterator WIR_IntervalSet<IntegerType, CompType>::begin()" );

      validate<WIR_IntervalCompression::ondemand>();
      return( mChunks.begin() );
    };

    /*!
      @brief end returns an iterator to the end of this interval set.

      @return A const iterator pointing to the position after the last interval
              in this set.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    const_iterator end( void ) const
    {
      DSTART(
        "WIR_IntervalSet<IntegerType, CompType>::const_iterator WIR_IntervalSet<IntegerType, CompType>::end() const" );

      validate<WIR_IntervalCompression::ondemand>();
      return( mChunks.end() );
    };

    /*!
      @brief end returns an iterator to the end of this interval set.

      @return An iterator pointing to the position after the last interval in
              this set.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    iterator end( void )
    {
      DSTART(
        "WIR_IntervalSet<IntegerType, CompType>::iterator WIR_IntervalSet<IntegerType, CompType>::end()" );

      validate<WIR_IntervalCompression::ondemand>();
      return( mChunks.end() );
    };


    //
    // Comparison operators (== , !=, <, <=, >, >=).
    //

    /*!
      @brief This operator compares two WIR_IntervalSets for equality.

      @param[in] i A const reference to the operand to be compared with.
      @return true iff both operands are equal, false otherwise.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    bool operator == ( const type &i ) const
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      validate<WIR_IntervalCompression::ondemand>();
      i.validate<WIR_IntervalCompression::ondemand>();
      return( mChunks == i.mChunks );
    };

    /*!
      @brief This operator compares two WIR_IntervalSets for inequality.

      @param[in] i A const reference to the operand to be compared with.
      @return true iff both operands are inequal, false otherwise.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    bool operator != ( const type &i ) const
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      validate<WIR_IntervalCompression::ondemand>();
      i.validate<WIR_IntervalCompression::ondemand>();
      return( mChunks != i.mChunks );
    };

    /*!
      @brief This operator performs a less-than comparison of two
             WIR_IntervalSets.

      @param[in] i A const reference to the operand to be compared with.
      @return true iff the left operand is less than the right one, false
              otherwise.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    bool operator < ( const type &i ) const
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      return( ( *this <= i ) && ( *this != i ) );
    };

    /*!
      @brief This operator performs a less-equal comparison of two
             WIR_IntervalSets.

      @param[in] i A const reference to the operand to be compared with.
      @return true iff the left operand is less than or equal to the right one,
              false otherwise.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    bool operator <= ( const type &i ) const
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      validate<WIR_IntervalCompression::ondemand>();
      i.validate<WIR_IntervalCompression::ondemand>();
      return( mChunks.back().getUpper() <= i.mChunks.front().getLower() );
    };

    /*!
      @brief This operator performs a greater-than comparison of two
             WIR_IntervalSets.

      @param[in] i A const reference to the operand to be compared with.
      @return true iff the left operand is greater than the right one, false
              otherwise.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    bool operator > ( const type &i ) const
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      return( i < *this );
    };

    /*!
      @brief This operator performs a greater-equal comparison of two
             WIR_IntervalSets.

      @param[in] i A const reference to the operand to be compared with.
      @return true iff the left operand is greater than or equal to the right
              one, false otherwise.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    bool operator >= ( const type &i ) const
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      return( i <= *this );
    };


    //
    // Assignment operators.
    //

    /*!
      @brief Copy-assignment operator.

      @param[in] i A const reference to another object to be copied.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    type & operator = ( const type &i )
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      if ( this != &i )
        mChunks = i.mChunks;
      return( *this );
    };

    /*!
      @brief Move-assignment operator.

      @param[in] i An R-value reference to another object to be moved.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    type & operator = ( type &&i )
    {
      DSTART(
        "WIR_IntervalSet<IntegerType, CompType>::type& WIR_IntervalSet<IntegerType, CompType>::operator=(WIR_IntervalSet<IntegerType, CompType>::type&&)" );

      if ( this != &i ) {
        mChunks = std::move( i.mChunks );
        i.clear();
      }
      return( *this );
    };

    /*!
      @brief Copy-assignment operator with different compression type.

      @param[in] i A const reference to another object to be copied.
      @tparam otherComp The compression type of the interval set to be copied.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    template<WIR_IntervalCompression otherComp>
    type & operator = ( const WIR_IntervalSet<intType, otherComp> &i )
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      if ( this != &i ) {
        mChunks = i.mChunks;
        validate<WIR_IntervalCompression::permanent>();
      }
      return( *this );
    };

    /*!
      @brief Move-assignment operator with different compression type.

      @param[in] i An R-value reference to another object to be moved.
      @tparam otherComp The compression type of the interval set to be copied.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    template<WIR_IntervalCompression otherComp>
    type & operator = ( WIR_IntervalSet<intType, otherComp> &&i )
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      if ( this != &i ) {
        mChunks = std::move( i.mChunks );
        i.clear();
        validate<WIR_IntervalCompression::permanent>();
      }
      return( *this );
    };


    //
    // Addition/subtraction operators (+, +=, -, -=).
    //

    /*!
      @brief Addition operator.

      @param[in] i A const reference to another object to be added.
      @return An interval set containing the sum of the two interval sets.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    type operator + ( intervalType &i ) const
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      type clone( *this );
      return( clone += i );
    };

    /*!
      @brief Addition-assignment operator.

      @param[in] i A const reference to another object to be added.
      @return A reference to this interval set containing the sum.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    type & operator += ( intervalType &i )
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      for ( _intervalType &chunk : mChunks )
        chunk += i;
      validate<WIR_IntervalCompression::permanent>();
      return( *this );
    };

    /*!
      @brief Subtraction operator.

      @param[in] i A const reference to another object to be subtracted.
      @return An interval set containing the difference of the two interval
              sets.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    type operator - ( intervalType &i ) const
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      type clone( *this );
      return( clone -= i );
    };

    /*!
      @brief Subtraction-assignment operator.

      @param[in] i A const reference to another object to be subtracted.
      @return A reference to this interval set containing the difference.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    type & operator -= ( intervalType &i )
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      for ( _intervalType &chunk : mChunks )
        chunk -= i;
      validate<WIR_IntervalCompression::permanent>();
      return( *this );
    };


    //
    // Miscellaneous.
    //

    #define WRAPPER_BASE( type, name, body, constfunc ) \
      type name ( const intervalType &i ) constfunc \
      { \
        body; \
      }; \
      type name ( const intType &i ) constfunc \
      { \
        body; \
      };

    #define METHOD_RETURN_WRAPPERS( return_type, methodname, constfunc ) \
      WRAPPER_BASE( \
        return_type, methodname, return methodname( type( i ) ), constfunc )
    #define METHOD_NORETURN_WRAPPERS( methodname, constfunc ) \
      WRAPPER_BASE( \
        void, methodname, methodname( type( i ) ), constfunc )

    /*!
      @brief contains computes whether this interval set fully contains @a i

      @param[in] i A const reference to another object.
      @return True iff this interval set fully contains i, false otherwise.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    bool contains( const type &i ) const
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      validate<WIR_IntervalCompression::ondemand>();
      i.validate<WIR_IntervalCompression::ondemand>();

      static_assert(
        CompType != WIR_IntervalCompression::none,
        "The current implementation of 'contains' depends on the joining "
        "of adjacent intervals" );

      // All chunks that are contained in "i" must have a containing chunk
      // in "this" to make "this" contain "i". Chunks must be ordered!
      auto itSmallCand = i.begin(); // Candidate who is tested to be contained
      auto itBigCand = begin();     // Candidate who is tested to contain
      while ( ( itSmallCand != i.end() ) && ( itBigCand != end() ) ) {
        // Get parts to compare
        intervalType &vSmallCand = *itSmallCand;
        intervalType &vBigCand   = *itBigCand;
        // Skip part if it is an "additional" part compared to the smaller
        // candidate.
        if ( vSmallCand.getLower() > vBigCand.getUpper() )
          ++itBigCand;
        else {
          // Have differences -> Have disjoint parts
          // (Test intersection first for efficiency)
          if ( vSmallCand.hasEmptyIntersection( vBigCand ) ||
               !vSmallCand.difference( vBigCand ).isEmpty() )
            return( false );
          else
            // Only increment itSmallCand, vBigCand may also cover the
            // following chunks of the smaller interval set candidate.
            ++itSmallCand;
        }
      }

      // If there are more chunks in "i", this is okay. Else, have
      // superfluous, non-covered chunks in "this".
      return( itSmallCand == i.end() );
    };
    METHOD_RETURN_WRAPPERS( bool, contains, const );

    /*!
      @brief unite computes the union of two interval sets.

      @param[in] s A const reference to another object to be united.

      unite works in-place, i.e., it modifies "this".

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    void unite( const type &s )
    {
      DSTART(
        "void WIR_IntervalSet<IntegerType, CompType>::unite(const type&)" );

      if ( this != &s ) {
        mChunks.splice( mChunks.end(), _chunkSetType( s.mChunks ) );
        validate<WIR_IntervalCompression::permanent>();
      }
    };
    METHOD_NORETURN_WRAPPERS( unite, );

    /*!
      @brief unification computes the union of two interval sets.

      @param[in] s A const reference to another object.
      @return An interval set containing the union.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    type unification( const type &s ) const
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      type copy( *this );
      copy.unite( s );
      return( copy );
    };
    METHOD_RETURN_WRAPPERS( type, unification, const );

    /*!
      @brief intersect computes the intersection of two interval sets.

      @param[in] s A const reference to another object to be intersected.

      intersect works in-place, i.e., it modifies "this".

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    void intersect( const type &s )
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      if ( this != &s ) {
        type meWithoutS( *this );
        meWithoutS.remove( s );
        remove( meWithoutS );
        validate<WIR_IntervalCompression::permanent>();
      }
    };
    METHOD_NORETURN_WRAPPERS( intersect, );

    /*!
      @brief intersection computes the intersection of two interval sets.

      @param[in] s A const reference to another object.
      @return An interval set containing the intersection.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    type intersection( const type &s ) const
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      type copy( *this );
      copy.intersect( s );
      return( copy );
    };
    METHOD_RETURN_WRAPPERS( type, intersection, const );

    /*!
      @brief remove removes the given interval set's content from the current
             one's.

      @param[in] s A const reference to another object to be removed.

      remove works in-place, i.e., it modifies "this".

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    void remove( const type &s )
    {
      DSTART(
        "void WIR_IntervalSet<IntegerType, CompType>::remove(const type&)" );

      for ( const _intervalType& chunk : s ) {
        _chunkSetType newChunks;
        for ( iterator itThis( mChunks.begin() ), itEnd( mChunks.end() );
              itThis != itEnd; ) {
          if ( !chunk.hasEmptyIntersection( *itThis ) ) {
            type newDiff( itThis->difference( chunk ) );
            newChunks.splice( newChunks.end(), newDiff.getChunks() );
            mChunks.erase( itThis++ );
          } else
            ++itThis;
        }
        mChunks.splice( mChunks.end(), newChunks );
      }
      validate<WIR_IntervalCompression::permanent>();
    };
    METHOD_NORETURN_WRAPPERS( remove, );

    /*!
      @brief difference removes @a s from this interval set.

      @param[in] s A const reference to another object to be removed.
      @return An interval set containing the difference.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    type difference( const type &s ) const
    {
      DSTART(
        "WIR_IntervalSet<IntegerType, CompType>::type WIR_IntervalSet<IntegerType, CompType>::difference(const type&) const" );

      type copy( *this );
      copy.remove( s );
      return( copy );
    };
    METHOD_RETURN_WRAPPERS( type, difference, const );

    /*!
      @brief toString produces a human-readable string representation of the
             interval set.

      @return A string representing the interval set.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    std::string toString() const
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      std::stringstream out;
      out << std::hex << "[";
      for ( intervalType &ii : *this ) {
        if ( &ii != &*begin() )
          out << ",";
        out << ii.getLower() << "-" << ii.getUpper();
      }
      out << "]";
      return( out.str() );
    };

    #undef WRAPPER_BASE
    #undef METHOD_RETURN_WRAPPERS
    #undef METHOD_NORETURN_WRAPPERS


  protected:

    /*!
      @brief getChunks returns the intervals contained by an interval set.

      @return A const reference to the list of intervals.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    chunkSetType &getChunks( void ) const
    {
      DSTART(
        "WIR_IntervalSet<IntegerType, CompType>::chunkSetType& WIR_IntervalSet<IntegerType, CompType>::getChunks() const" );

      return( mChunks );
    };

    /*!
      @brief getChunks returns the intervals contained by an interval set.

      @return A reference to the list of intervals.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    _chunkSetType &getChunks( void )
    {
      DSTART(
        "WIR_IntervalSet<IntegerType, CompType>::_chunkSetType& WIR_IntervalSet<IntegerType, CompType>::getChunks()" );

      return( mChunks );
    };

    /*!
      @brief Struct IntervalComparator serves for interval sorting.

      Chunks are sorted
        - ascending by their lower bound and (to break ties)
        - ascending by their upper bound.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    struct IntervalComparator
    {
      bool operator() ( const _intervalType &lhs,
                        const _intervalType &rhs ) const
      {
        if ( lhs.getLower() != rhs.getLower() )
          return( lhs.getLower() < rhs.getLower() );
        else
          return( lhs.getUpper() < rhs.getUpper() );
      };
    };

    /*!
      @brief validate triggers the interval set compression.

      @tparam triggerType Specifies under which condition compression shall
                          actually take place.

      This method is marked as "const" to support the different compression
      types. Since the compression is logically not a change of the content of
      the interval set, we marked the chunk container as mutable.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    template<WIR_IntervalCompression triggerType> void validate( void ) const
    {
      DSTART(
        "void WIR_IntervalSet<IntegerType, CompType>::validate() const" );

      if ( triggerType == CompType )
        applyCompression();
    };

    /*!
      @brief applyCompression actually compresses the contained intervals.

      This method is marked as "const" to support the different compression
      types. Since the compression is logically not a change of the content of
      the interval set, we marked the chunk container as mutable.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    virtual void applyCompression( void ) const
    {
      DSTART(
        "void WIR_IntervalSet<IntegerType, CompType>::applyCompression() const" );

      DOUT( "Initial    " << *this << "\n" );

      // Sort the list.
      mChunks.sort( IntervalComparator() );
      DOUT( "Normalized " << *this << "\n" );

      // Compress overlapping chunks.
      if ( mChunks.size() > 1 ) {
        typename _chunkSetType::iterator itChunk( mChunks.begin() ),
                                         itChunkEnd( mChunks.end() );
        _intervalType *lastChunk = nullptr;

        while ( itChunk != itChunkEnd ) {
          _intervalType *chunk = &*itChunk;
          bool deleted = false;

          if ( lastChunk != nullptr ) {
            // If the two chunks are direct neighbors or have a nonempty
            // intersection, then we set the upper bound of the last chunk
            // to the upper bound of the current one and delete the current one.
            // (The chunks are ordered ascending by a) lower and b) upper bound)
            if ( lastChunk->getUpper() >= ( chunk->getLower() - 1 ) ) {
              // If the previous chunk completely contains this one, remove it.
              // Otherwise, also take over the upper bound from this chunk.
              if ( chunk->getUpper() > lastChunk->getUpper() )
                lastChunk->setUpper( chunk->getUpper() );
              // cppcheck-suppress eraseDereference
              itChunk = mChunks.erase( itChunk );
              deleted = true;
            }
          }

          if ( !deleted ) {
            lastChunk = chunk;
            ++itChunk;
          }
        }
      }

      DOUT( "Compressed " << *this << "\n" );
    };


  private:

    /*!
      @brief mChunks stores the list of all intervals contained by a set.

      It is declared mutable, since the ondemand compression would otherwise
      render all methods non-const.
    */
    mutable _chunkSetType mChunks;

};


//
// Stream I/O.
//

/*!
  @brief The << operator dumps a %WIR interval set to an output stream.

  @param[in] os A reference to an output stream.
  @param[in] i A const reference to the %WIR interval set to be dumped.
  @return A reference to the same output stream.

  @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
*/
template<typename BaseType, WIR_IntervalCompression CompType>
std::ostream & operator << ( std::ostream &os,
                             const WIR_IntervalSet<BaseType, CompType> &i )
{
  DSTART(
    "ostream& operator<<(ostream&, const WIR_IntervalSet<BaseType, CompType>&)" );

  os << "[";
  for ( typename WIR_IntervalSet<BaseType, CompType>::intervalType &ii : i ) {
    if ( &ii != &*i.begin() )
      os << ",";
    os << ii.getLower() << "-" << ii.getUpper();
  }
  os << "]";

  return( os );
};

}       // namespace WIR

#endif  // _WIR_INTERVAL_SET_H
