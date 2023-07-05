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
  @file wiridapi.h
  @brief This file provides the interface of a base class for managing unique
         numerical IDs of derived classes.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_ID_API_H
#define _WIR_ID_API_H


//
// Header section
//

namespace WIR {


//
// Global data type declarations
//

//! This type is used to represent unique numerical IDs of %WIR objects.
using WIR_id_t = unsigned long long;


//! nullid holds the minimal ID that can be handeled with type WIR_id_t.
const WIR_id_t nullid { 0 };


/*!
  @brief Class WIR_ID_API provides a simple API to assign unique IDs to %WIR
         objects and to query such IDs.

  The assignment of IDs is solely done by the constructors. There is no way to
  manually assign IDs to objects.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_ID_API
{

  public:

    //
    // Comparison operators.
    //

    /*!
      @brief The == operator checks for equality of IDs.

      @param[in] __o A const reference to another object to be compared.
      @return true iff both operands have the same IDs, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    inline virtual bool operator == ( const WIR_ID_API &__o ) const
    {
      return( mID == __o.mID );
    };

    /*!
      @brief The != operator checks for inequality of IDs.

      @param[in] __o A const reference to another object to be compared.
      @return true iff both operands have different IDs, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    inline virtual bool operator != ( const WIR_ID_API &__o ) const
    {
      return( mID != __o.mID );
    };


    //
    // Methods to retrieve an ID.
    //

    /*!
      @brief getID returns a WIR object's unique ID.

      @return An object's ID.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    inline WIR_id_t getID( void ) const
    {
      return( mID );
    };


  protected:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor registering a fresh unique ID of a new object.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    inline WIR_ID_API( void )
    {
      mID = mIDCounter++;
    };

    /*!
      @brief This copy constructor simply behaves like the standard constructor.

      This way, fresh unique IDs are also assigned to a copy-created instance.
      Thus, the argument passed to this copy constructor is de facto irrelevant.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    inline WIR_ID_API( const WIR_ID_API & )
    {
      mID = mIDCounter++;
    };

    /*!
      @brief This move constructor simply re-registers the already used ID.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    inline WIR_ID_API( WIR_ID_API &&__o )
    {
      mID = __o.mID;
      __o.mID = nullid;
    };

    /*!
      @brief The destructor unregisters the object's ID.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    inline virtual ~WIR_ID_API( void ) {};

    /*!
      @brief This copy-assignment operator simply behaves like the standard
             constructor.

      This way, fresh unique IDs are also assigned to a copy-created instance.
      Thus, the argument passed to this assignment operator is de facto
      irrelevant.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    inline WIR_ID_API & operator = ( const WIR_ID_API & )
    {
      mID = mIDCounter++;
      return( *this );
    };


  private:

    //! mID holds the unique numerical ID of WIR objects.
    WIR_id_t mID;

    //! mIDCounter contains the next free numerial ID for WIR objects.
    static WIR_id_t mIDCounter;

};

}       // namespace WIR

#endif  // _WIR_ID_API_H
