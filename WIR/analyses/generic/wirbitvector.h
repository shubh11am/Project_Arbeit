/*

   This header file belongs to the

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
  @file wirbitvector.h
  @brief This file provides the interface of highly efficient %WIR bit vectors.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_BITVECTOR_H
#define _WIR_BITVECTOR_H


//
// Include section
//

// Include standard headers
#include <cstring>
#include <memory>

// Include libuseful headers
#include <libuseful/io.h>


//
// Header section
//

namespace WIR {

/*!
  @brief Class WIR_BitVector realizes bit vectors of arbitrary, but fixed, size.

  Such bit vectors are heavily used to represent sets within various %WIR data
  flow analyses.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_BitVector
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor.

      @param[in] n An unsigned integer denoting the number of bits to be
                   allocated in a bit vector.

      All bits of a newly allocated bit vector are initialized to 0.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    inline explicit WIR_BitVector( size_t n ) :
      mSize { n },
      mWords {
        n % ( sizeof( word_t ) * 8 ) == 0 ?
          n / ( sizeof( word_t ) * 8 ) : n / ( sizeof( word_t ) * 8 ) + 1 },
      mModified { true },
      mStorage { std::make_unique<word_t []>( mWords ) }
    {
      for ( unsigned int i = 0; i < mWords; ++i )
        mStorage[ i ] = 0;
    };

    /*!
      @brief Copy constructor.

      @param[in] v A const reference to another bit vector to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    inline WIR_BitVector( const WIR_BitVector &v ) :
      mSize { v.mSize },
      mWords { v.mWords },
      mModified { true },
      mStorage { std::make_unique<word_t []>( mWords ) }
    {
      // The following assertion is required to avoid warnings of the clang
      // static analyzer.
      ufAssert( mWords == v.mWords );

      for ( unsigned int i = 0; i < mWords; ++i )
        mStorage[ i ] = v.mStorage[ i ];
    };

    /*!
      @brief Move constructor.

      @param[in] v An R-value reference to another bit vector to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    inline WIR_BitVector( WIR_BitVector &&v ) :
      mSize { v.mSize },
      mWords { v.mWords },
      mModified { v.mModified },
      mStorage { std::move( v.mStorage ) }
    {
      v.mSize = 0;
      v.mWords = 0;
      v.mStorage.release();
    };

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    inline ~WIR_BitVector( void ) {};

    /*!
      @brief Copy-assignment operator.

      @param[in] v A const reference to another bit vector to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    inline WIR_BitVector & operator = ( const WIR_BitVector &v )
    {
      mSize = v.mSize;
      mWords = v.mWords;
      mModified = true;
      mStorage =
        std::unique_ptr<word_t []> { std::make_unique<word_t []>( mWords ) };
      std::memcpy(
        mStorage.get(), v.mStorage.get(), mWords * sizeof( word_t ) );

      return( *this );
    };

    /*!
      @brief Move-assignment operator.

      @param[in] v An R-value reference to another bit vector to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    inline WIR_BitVector & operator = ( WIR_BitVector &&v )
    {
      mSize = v.mSize;
      v.mSize = 0;

      mWords = v.mWords;
      v.mWords = 0;

      mModified = v.mModified;

      mStorage = std::move( v.mStorage );
      v.mStorage.release();

      return( *this );
    };


    //
    // Comparison operators.
    //

    /*!
      @brief The == operator checks for equality of bit vectors.

      @param[in] v A const reference to another bit vector to be compared.
      @return true iff both operands have the same bitwise contents, false
              otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    inline bool operator == ( const WIR_BitVector &v ) const
    {
      for ( unsigned int i = 0; i < mWords - 1; ++i )
        if ( mStorage[ i ] != v.mStorage[ i ] )
          return( false );

      for ( unsigned int i = 0;
            i < mSize - ( ( mWords - 1 ) * sizeof( word_t ) * 8 ); ++i )
        if ( ( mStorage[ mWords - 1 ] & ( 1 << i ) ) !=
             ( v.mStorage[ mWords - 1 ] & ( 1 << i ) ) )
          return( false );

      return( true );
    };

    /*!
      @brief The != operator checks for inequality of bit vectors.

      @param[in] v A const reference to another bit vector to be compared.
      @return true iff both operands have different bitwise contents, false
              otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    inline bool operator != ( const WIR_BitVector &v ) const
    {
      for ( unsigned int i = 0; i < mWords - 1; ++i )
        if ( mStorage[ i ] != v.mStorage[ i ] )
          return( true );

      for ( unsigned int i = 0;
            i < mSize - ( ( mWords - 1 ) * sizeof( word_t ) * 8 ); ++i )
        if ( ( mStorage[ mWords - 1 ] & ( 1 << i ) ) !=
             ( v.mStorage[ mWords - 1 ] & ( 1 << i ) ) )
          return( true );

      return( false );
    };


    //
    // Vector operations.
    //

    /*!
      @brief size returns the number of bits managed by a bit vector.

      @return An unsigned integer denoting the number of bits allocated in a bit
              vector.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    inline size_t size( void ) const
    {
      return( mSize );
    };

    /*!
      @brief isModified returns whether a bit vector has been modified since the
             last invocation of the very same method.

      @return A Boolen denoting whether the bit vector has been modified or not.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isModified( void )
    {
      bool res = mModified;
      mModified = false;

      return( res );
    };

    /*!
      @brief set_union computes the union of a bit vector with this vector.

      @param[in] v A const reference to another bit vector to be added.

      @note set_union assumes that both involved bit vectors have exactly the
            same size of physical storage. I.e., mWords == v.mWords must hold.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    inline void set_union( const WIR_BitVector &v )
    {
      // The following assertion is required to avoid warnings of the clang
      // static analyzer.
      ufAssert( mWords == v.mWords );

      if ( mModified )
        for ( unsigned int i = 0; i < mWords; ++i )
          mStorage[ i ] |= v.mStorage[ i ];
      else
        for ( unsigned int i = 0; i < mWords; ++i ) {
          auto oldWord = mStorage[ i ];

          mStorage[ i ] |= v.mStorage[ i ];

          if ( mStorage[ i ] != oldWord )
            mModified = true;
        }
    };

    /*!
      @brief set_intersection computes the intersection of a bit vector with
             this vector.

      @param[in] v A const reference to another bit vector to be intersected.

      @note set_intersection assumes that both involved bit vectors have exactly
            the same size of physical storage. I.e., mWords == v.mWords must
            hold.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    inline void set_intersection( const WIR_BitVector &v )
    {
      // The following assertion is required to avoid warnings of the clang
      // static analyzer.
      ufAssert( mWords == v.mWords );

      if ( mModified )
        for ( unsigned int i = 0; i < mWords; ++i )
          mStorage[ i ] &= v.mStorage[ i ];
      else
        for ( unsigned int i = 0; i < mWords; ++i ) {
          auto oldWord = mStorage[ i ];

          mStorage[ i ] &= v.mStorage[ i ];

          if ( mStorage[ i ] != oldWord )
            mModified = true;
        }
    };

    /*!
      @brief set_difference computes the difference between this bit vector and
             another vector.

      @param[in] v A const reference to another bit vector to be subtracted.

      @note set_difference assumes that both involved bit vectors have exactly
            the same size of physical storage. I.e., mWords == v.mWords must
            hold.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    inline void set_difference( const WIR_BitVector &v )
    {
      // The following assertion is required to avoid warnings of the clang
      // static analyzer.
      ufAssert( mWords == v.mWords );

      if ( mModified )
        for ( unsigned int i = 0; i < mWords; ++i )
          mStorage[ i ] &= ~(v.mStorage[ i ]);
      else
        for ( unsigned int i = 0; i < mWords; ++i ) {
          auto oldWord = mStorage[ i ];

          mStorage[ i ] &= ~(v.mStorage[ i ]);

          if ( mStorage[ i ] != oldWord )
            mModified = true;
        }
    };


    //
    // Bit operations.
    //

    /*!
      @brief Element access operator returning a vector's bit from the given bit
             position.

      @param[in] i An unsigned integer that denotes the bit position to be
                   returned, with 0 <= i < mSize.
      @return A Boolean denoting the vector's value at bit position i.

      @note This operator assumes that 0 <= i < mSize must hold. For efficiency
            reasons, absolutely no boundary checks are performed to verify this
            assumption!

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    inline bool operator [] ( size_t i ) const
    {
      size_t word = i / ( sizeof( word_t ) * 8 );
      size_t bit = i % ( sizeof( word_t ) * 8 );

      return( mStorage[ word ] & ( 1 << bit ) );
    };

    /*!
      @brief setBit sets a vector's bit at the given bit position.

      @param[in] i An unsigned integer that denotes the bit position to be set,
                   with 0 <= i < mSize.

      @note setBit assumes that 0 <= i < mSize must hold. For efficiency
            reasons, absolutely no boundary checks are performed to verify this
            assumption!

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    inline void setBit( size_t i )
    {
      size_t word = i / ( sizeof( word_t ) * 8 );
      size_t bit = i % ( sizeof( word_t ) * 8 );

      if ( mModified )
        mStorage[ word ] |= ( 1 << bit );
      else {
        auto oldWord = mStorage[ word ];

        mStorage[ word ] |= ( 1 << bit );

        if ( mStorage[ word ] != oldWord )
          mModified = true;
      }
    };

    /*!
      @brief clearBit clears a vector's bit at the given bit position.

      @param[in] i An unsigned integer that denotes the bit position to be
                   cleared, with 0 <= i < mSize.

      @note clearBit assumes that 0 <= i < mSize must hold. For efficiency
            reasons, absolutely no boundary checks are performed to verify this
            assumption!

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    inline void clearBit( size_t i )
    {
      size_t word = i / ( sizeof( word_t ) * 8 );
      size_t bit = i % ( sizeof( word_t ) * 8 );

      if ( mModified )
        mStorage[ word ] &= ~( 1 << bit );
      else {
        auto oldWord = mStorage[ word ];

        mStorage[ word ] &= ~( 1 << bit );

        if ( mStorage[ word ] != oldWord )
          mModified = true;
      }
    };


  private:

    //
    // Local type definitions.
    //

    /*!
      @brief word_t represents the fundamental data type of a bit vector's
             physical storage.
    */
    using word_t = unsigned int;

    /*!
      @brief No standard construction allowed, users must use one of the above
             constructors instead.
    */
    WIR_BitVector( void ) = delete;

    //! mSize stores the number of bits of a bit vector.
    size_t mSize;

    /*!
      @brief mWords stores the number of memory words required to store mSize
             many bits.
    */
    size_t mWords;

    //! mModified stores whether a bit vector has been modified or not.
    bool mModified;

    //! mStorage physically stores the bits of a bit vector.
    std::unique_ptr<word_t []> mStorage;

};

}       // namespace WIR

#endif  // _WIR_BITVECTOR_H
