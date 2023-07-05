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
  @file wircontainer.h
  @brief This file provides the interface of generic %WIR containers that can
         freely be attached to the core classes of the %WIR library.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_CONTAINER_H
#define _WIR_CONTAINER_H


//
// Include section
//

// Include standard headers
#include <iostream>

// Include libuseful headers
#include <libuseful/debugmacros.h>

// Include WIR headers
#include <wir/API/wiridapi.h>
#include <wir/wirbasecontainer.h>


//
// Header section
//

namespace WIR {

/*!
  @brief Class WIR_Container models a generic container for arbitrary meta-data.

  WIR_Container adds functionality for statically and uniquely assigning
  container type IDs. This way, meta-data of different types can unambiguously
  be distinguished.

  WIR_Container serves as virtual base class from which actual containers for
  dedicated meta-data are derived. For this purpose, every container type must
  inherit from WIR_Container in the following way:

  class MyContainer : public WIR_Container<MyContainer>
  {
    ...
  };

  This creates a template instantiation of WIR_Container that adds a static
  field for the container type ID of 'MyContainer'. Furthermore, static
  initializers are added to MyContainer that fill this field with a valid value
  allocated from class WIR_BaseContainer via a call to method
  registerNewContainerType.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
template<typename DerivedContainerClass>
class WIR_Container : public WIR_BaseContainer
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor creating an empty container and registering its
             container type.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Container( void ) :
      WIR_BaseContainer {}
    {
      DSTART( "WIR_Container<DerivedContainerClass>::WIR_Container()" );

      // The following call is important, see also comments in
      // WIR_BaseContainer::Registrator::touch.
      mRegistrator.touch();
    };

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Container( const WIR_Container &__o ) :
      WIR_BaseContainer { __o }
    {
      DSTART(
        "WIR_Container<DerivedContainerClass>::WIR_Container(const WIR_Container<DerivedContainerClass>&)" );
    };

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Container( WIR_Container &&__o ) :
      WIR_BaseContainer { std::move( __o ) }
    {
      DSTART(
        "WIR_Container<DerivedContainerClass>::WIR_Container(WIR_Container<DerivedContainerClass>&&)" );
    };

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_Container( void )
    {
      DSTART( "WIR_Container<DerivedContainerClass>::~WIR_Container()" );
    };

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Container & operator = ( const WIR_Container &__o )
    {
      DSTART(
        "WIR_Container<DerivedContainerClass>& WIR_Container<DerivedContainerClass>::operator=(const WIR_Container<DerivedContainerClass>&)" );

      WIR_BaseContainer::operator = ( __o );

      return( *this );
    };

    /*!
      @brief Move-assignment operator.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Container & operator = ( WIR_Container &&__o )
    {
      DSTART(
        "WIR_Container<DerivedContainerClass>& WIR_Container<DerivedContainerClass>::operator=(WIR_Container<DerivedContainerClass>&&)" );

      WIR_BaseContainer::operator = ( std::move( __o ) );

      return( *this );
    };


    //
    // Container type handling.
    //

    /*!
      @brief getContainerTypeID returns the ID of a container type.

      @return The container type's ID.

      Since getContainerTypeID is a static method, it can be used to query types
      for derived container classes without having actual objects of such a
      container class.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    static WIR_id_t getContainerTypeID( void )
    {
      DSTART(
        "static WIR_id_t WIR_Container<DerivedContainerClass>::getContainerTypeID()" );

      return( mContainerID );
    };

    /*!
      @brief getContainerType returns the ID of a container type.

      @return The container type's ID.

      getContainerType can be used to query types for actual container objects.
      Please note the difference to method getContainerTypeID above that serves
      the same purpose but that is a static method.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_id_t getContainerType( void ) const
    {
      DSTART(
        "WIR_id_t WIR_Container<DerivedContainerClass>::getContainerType() const" );

      return( mContainerID );
    };


  private:

    /*!
      @brief clone creates a copy of a %WIR container.

      @return A pointer to the newly created copy of this container.

      Using the template argument, clone just calls the correct copy constructor
      of the actual container class.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_BaseContainer *clone( void ) const
    {
      DSTART(
        "WIR_BaseContainer* WIR_Container<DerivedContainerClass>::clone()" );

      return(
        new DerivedContainerClass(
          *( static_cast<const DerivedContainerClass *>( this ) ) ) );
    };

    //! mContainerID holds the statically initialized container type ID.
    static WIR_id_t mContainerID;

    //! mRegistrator is used to initialize mContainerID via its constructor.
    static WIR_BaseContainer::Registrator mRegistrator;

};

template<typename DerivedContainerClass>
WIR_id_t WIR_Container<DerivedContainerClass>::mContainerID;

template<typename DerivedContainerClass>
WIR_BaseContainer::Registrator WIR_Container<DerivedContainerClass>::mRegistrator( DerivedContainerClass::mContainerID );

}       // namespace WIR

#endif  // _WIR_CONTAINER_H
