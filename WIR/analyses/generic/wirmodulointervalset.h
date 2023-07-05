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
  @file wirmodulointervalset.h
  @brief This file provides the basic interface of generic sets of modulo
         intervals.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_MODULO_INTERVAL_SET_H
#define _WIR_MODULO_INTERVAL_SET_H


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
#include <vector>

// Include boost headers
#include <boost/current_function.hpp>
#include <boost/iterator/iterator_facade.hpp>

// Include libuseful headers
#include <libuseful/debugmacros.h>

// Include WIR headers
#include <analyses/generic/wirintervalset.h>


//
// Header section
//

namespace WIR {

/*!
  @brief Class WIR_ModuloIntervalSet represents a set of modulo intervals.

  @tparam IntegerType The type of the integers to be used for the underlying
                      intervals.
  @tparam CompType The way in which the compression of the set is done.

  A modulo interval is an interval which can only contain values from a given
  range [0, ..., upper_bound]. It is useful in static analysis to represent
  cyclic integral values.

  To represent such an interval, one could compute the modulus after each change
  to the interval. When the result of the addition of two intervals "crosses"
  the upper_bound like in

    upper_bound = 10
    [8,9] + [1,2] = [9] and [0,1]

  then the result are multiple intervals. To represent such "split"
  results/values, this class directly represents a set of modulo intervals.

  Due to the internal realization of the subtraction operation, the upper limit
  of the underlying data type has to be at least twice as big as the modulo
  bound (otherwise, there may be overflows in the underlying integer
  computations).

  @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
*/
template<typename IntegerType, WIR_IntervalCompression CompType>
class WIR_ModuloIntervalSet : public WIR_IntervalSet<IntegerType, CompType>
{

  public:

    //
    // Local type definitions.
    //

    //! The instantiated type of this class.
    using type = WIR_ModuloIntervalSet<IntegerType, CompType>;

    //! The classes' base type from which it inherits.
    using baseType = WIR_IntervalSet<IntegerType, CompType>;

    //! The integer base type of the intervals.
    using intType = typename baseType::intType;

    //! The type of the contained intervals (as visible internally).
    using _intervalType = typename baseType::_intervalType;

    //! The type of the contained intervals (as visible to the outside).
    using intervalType = typename baseType::intervalType;

    //! The container type used to store the chunks (as visible internally).
    using _chunkSetType = typename baseType::_chunkSetType;

    //! The container type used to store the chunks (as visible to the outside).
    using chunkSetType = typename baseType::chunkSetType;


    //
    // Constructors.
    //

    /*!
      @brief Default constructor for an empty modulo interval set.

      @param[in] moduloBound The interval set's modulus.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    explicit WIR_ModuloIntervalSet( intType moduloBound ) :
      mModuloBound( moduloBound )
    {
      DSTART(
        "WIR_ModuloIntervalSet<IntegerType, CompType>::WIR_ModuloIntervalSet(WIR_ModuloIntervalSet<IntegerType, CompType>::intType)" );
    };

    /*!
      @brief Constructor creating the modulo interval set [constant, constant].

      @param[in] moduloBound The interval set's modulus.
      @param[in] constant The value to be used as interval.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    WIR_ModuloIntervalSet( intType moduloBound, intType constant ) :
      baseType( constant ), mModuloBound( moduloBound )
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      baseType::template validate<WIR_IntervalCompression::permanent>();
    };

    /*!
      @brief Constructor creating a modulo interval set with the given interval
             i.

      @param[in] moduloBound The interval set's modulus.
      @param[in] i A const reference to the interval to be used.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    WIR_ModuloIntervalSet( intType moduloBound, const intervalType &i ) :
      baseType( i ),
      mModuloBound( moduloBound )
    {
      DSTART(
        "WIR_ModuloIntervalSet<IntegerType, CompType>::WIR_ModuloIntervalSet(WIR_ModuloIntervalSet<IntegerType, CompType>::intType, const intervalType&)" );

      baseType::template validate<WIR_IntervalCompression::permanent>();
    };

    /*!
      @brief Constructor creating a modulo interval set with the given interval
             set i.

      @param[in] moduloBound The interval set's modulus.
      @param[in] i A const reference to the interval set to be used.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    WIR_ModuloIntervalSet( intType moduloBound, const baseType &i ) :
      baseType( i ),
      mModuloBound( moduloBound )
    {
      DSTART(
        "WIR_ModuloIntervalSet<IntegerType, CompType>::WIR_ModuloIntervalSet(WIR_ModuloIntervalSet<IntegerType, CompType>::intType, const baseType&)" );

      baseType::template validate<WIR_IntervalCompression::permanent>();
    };

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    // cppcheck-suppress noExplicitConstructor
    WIR_ModuloIntervalSet( const type &__o ) :
      baseType( __o ),
      mModuloBound( __o.mModuloBound )
    {
      DSTART(
        "WIR_ModuloIntervalSet<IntegerType, CompType>::WIR_ModuloIntervalSet(const type&)" );
    };

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    // cppcheck-suppress noExplicitConstructor
    WIR_ModuloIntervalSet( type &&__o ) :
      baseType( std::move( __o ) ),
      mModuloBound( std::move( __o.mModuloBound ) )
    {
      DSTART(
        "WIR_ModuloIntervalSet<IntegerType, CompType>::WIR_ModuloIntervalSet(WIR_ModuloIntervalSet<IntegerType, CompType>::type&&)" );

      __o.clear();
    };


    //
    // Getters / setters.
    //

    /*!
      @brief isSaturated returns whether this interval holds all possible values
             within its modulo range.

      @return True if the inverval covers the whole modulo range, false
              otherwise.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    bool isSaturated( void ) const
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      baseType::template validate<WIR_IntervalCompression::ondemand>();
      return(
        ( baseType::getChunks().size() == 1 ) &&
        ( baseType::getChunks().begin()->getWidth() >= mModuloBound ) );
    };

    /*!
      @brief getModuloBound returns this inveral set's modulus.

      @return The accumulated width over all intervals.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    intType getModuloBound( void ) const
    {
      DSTART(
        "WIR_ModuloIntervalSet<IntegerType, CompType>::intType WIR_ModuloIntervalSet<IntegerType, CompType>::getModuloBound() const" );

      return( mModuloBound );
    };


    //
    // Comparison operators (== , !=).
    //

    /*!
      @brief This operator compares two WIR_ModuloIntervalSets for equality.

      @param[in] i A const reference to the operand to be compared with.
      @return true iff both operands are equal, false otherwise.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    bool operator == ( const type &i ) const
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      return(
        baseType::operator == ( i ) && ( mModuloBound == i.mModuloBound ) );
    };

    /*!
      @brief This operator compares two WIR_ModuloIntervalSets for inequality.

      @param[in] i A const reference to the operand to be compared with.
      @return true iff both operands are inequal, false otherwise.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    bool operator != ( const type &i ) const
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      return(
        baseType::operator != ( i ) || ( mModuloBound != i.mModuloBound ) );
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

      if ( this != &i ) {
        baseType::operator = ( i );
        mModuloBound = i.mModuloBound;
      }
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
        "WIR_ModuloIntervalSet<IntegerType, CompType>::type& WIR_ModuloIntervalSet<IntegerType, CompType>::operator=(WIR_ModuloIntervalSet<IntegerType, CompType>::type&&)" );

      if ( this != &i ) {
        baseType::operator = ( std::move( i ) );
        mModuloBound = std::move( i.mModuloBound );
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
    type &operator+=( intervalType &i )
    {
std::cout << BOOST_CURRENT_FUNCTION << std::endl;
      DSTART( BOOST_CURRENT_FUNCTION );

      for ( const intervalType& iChunk : type( mModuloBound, i ) )
        baseType::operator += ( iChunk );

      return( *this );
    };

    /*!
      @brief Subtraction operator.

      @param[in] i A const reference to another object to be subtracted.
      @return An interval set containing the difference of the two interval
              sets.

      See also the detailed comment for operator -=.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    type operator - ( intervalType &i ) const
    {
      DSTART(
        "WIR_ModuloIntervalSet<IntegerType, CompType>::type WIR_ModuloIntervalSet<IntegerType, CompType>::operator-(WIR_ModuloIntervalSet<IntegerType, CompType>::intervalType&) const" );

      type clone( *this );
      return( clone -= i );
    };

    /*!
      @brief Subtraction-assignment operator.

      @param[in] i A const reference to another object to be subtracted.
      @return A reference to this interval set containing the difference.

      Since we may experience wrap-around behavior when subtracting from zero in
      an unsigned representation, we first shift the content by the modulo
      bound. We assume here that the upper limit of the underlying data type is
      at least twice the modulo bound.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    type &operator -= ( intervalType &i )
    {
      DSTART(
        "WIR_ModuloIntervalSet<IntegerType, CompType>::type& WIR_ModuloIntervalSet<IntegerType, CompType>::operator-=(WIR_ModuloIntervalSet<IntegerType, CompType>::intervalType&)" );

      for ( const intervalType &iChunk : type( mModuloBound, i ) )
        for ( _intervalType &chunk : baseType::getChunks() ) {
          chunk += mModuloBound;
          chunk -= iChunk;
        }

      baseType::template validate<WIR_IntervalCompression::permanent>();
      return( *this );
    };


  protected:

    /*!
      @brief applyCompression performs modulo-splitting of the interval and
             re-compression.

      This method is marked as "const" to support the different compression
      types. Since the compression is logically not a change of the content of
      the interval set, we marked the chunk container as mutable.

      @author Timon Kelter <Timon.Kelter@tu-dortmund.de>
    */
    virtual void applyCompression( void ) const
    {
      DSTART(
        "void WIR_ModuloIntervalSet<IntegerType, CompType>::applyCompression() const" );

      // Normalize all chunks.
      std::list<_intervalType> newChunks;
      _chunkSetType &intervals =
        const_cast<_chunkSetType &>( baseType::getChunks() );

      for ( _intervalType &i : intervals ) {
        if ( !i.isValid() )
          // Due to invalid values in constructor or internal error
          throw std::runtime_error( "Contained interval is invalid!" );

        // Bring lower boundary into the [0, mModuloBound) interval.
        if ( i.getLower() >= mModuloBound ) {
          intType oldLower = i.getLower();
          intType newLower = oldLower % mModuloBound;
          intType newUpper = i.getUpper() - (oldLower - newLower);
          i = { newLower, newUpper };
        }

        // If the chunk crosses the modulo boundary, split it.
        if ( i.getUpper() >= mModuloBound ) {
          const unsigned int containedModuloRanges =
            i.getUpper() / mModuloBound;

          // The chunk may fall into 3 parts:
          // a) [i.getLower(), mModuloBound - 1]
          // b) [mModuloBound, containedModuloRanges * mModuloBound - 1]
          // c) [containedModuloRanges * mModuloBound, i.getUpper()]
          // Parts a) and c) exist in any case, part b) only exists, when
          // containedModuloRanges is greater than or equal to 1.
          if ( containedModuloRanges > 1 )
            // b)
            newChunks.emplace_back( 0, mModuloBound - 1 );

          // c)
          newChunks.emplace_back( 0, i.getUpper() % mModuloBound );

          // a)
          i.setUpper( mModuloBound - 1 );
        }
      }

      // Copy the new chunks into the chunk list.
      intervals.insert( intervals.end(), newChunks.begin(), newChunks.end() );

      // Call the parent class' method to do the rest.
      baseType::applyCompression();
    };


  private:

    //! Ensure that IntegerType is always unsigned.
    static_assert(
      std::is_unsigned<IntegerType>::value,
      "Modulo interval sets are only defined for unsigned integer types!" );

    //! mModuloBound stores the interval set's modulus.
    intType mModuloBound;

};

}       // namespace WIR

#endif  // _WIR_MODULO_INTERVAL_SET_H
