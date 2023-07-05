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
  @file wircontainerapi.h
  @brief This file provides the interface of a base class for managing %WIR
         containers in derived classes.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_CONTAINER_API_H
#define _WIR_CONTAINER_API_H


//
// Include section
//

// Include standard headers
#include <functional>
#include <map>
#include <memory>
#include <set>

// Include libuseful headers
#include <libuseful/debugmacros.h>

// Include WIR headers
#include <wir/wirbasecontainer.h>
#include <wir/wirtypes.h>


//
// Header section
//

namespace WIR {

/*!
  @brief Class WIR_Container_API provides a simple API to attach and retrieve
         generic %WIR containers to/from %WIR objects.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_Container_API
{

  public:

    //
    // Methods to attach and retrieve containers.
    //

    /*!
      @brief insertContainer attaches a new container.

      @param[in] c A const reference to the container to be copy-added.

      The content of c is copied into the WIR_Container_API class. If a unique
      container is inserted and there are already containers of the same type
      attached before, all the old containers are erased before.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertContainer( const WIR_BaseContainer & );

    /*!
      @brief insertContainer attaches a new container.

      @param[in] c An R-value reference to the container to be copy-added.

      The content of c is copied into the WIR_Container_API class. If a unique
      container is inserted and there are already containers of the same type
      attached before, all the old containers are erased before.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertContainer( WIR_BaseContainer && );

    /*!
      @brief insertContainer attaches a new container.

      @param[in] p A pointer to the container to be added.

      If a unique container is inserted and there are already containers of the
      same type attached before, all the old containers are erased before.

      @note Class WIR_Container_API takes over full control over the ownership
            of the given pointer! In particular, WIR_Container_API automatically
            destroys the object pointed to. Users of this variant of
            insertContainer are strongly discouraged of continuing to use this
            pointer afterwards. This variant of insertContainer is more
            efficient than the previous ones since it completely avoids any
            (polymorphic) copy operations. Thus, it should only be used if large
            amounts of containers shall be created/added highly efficiently.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertContainer( WIR_BaseContainer * );

    /*!
      @brief eraseContainers removes all containers of the specified container
             type.

      @param[in] id The ID of the container type to be removed.
      @param[in] recursive A Boolean that denotes whether the specified
                           container type shall recursively be erased also for
                           all objects below this one.

      This destroys all removed containers.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void eraseContainers( WIR_id_t, bool = false );

    /*!
      @brief eraseContainer removes the specified container.

      @param[in] c A reference to the container to be removed.

      This destroys the removed container.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void eraseContainer( WIR_BaseContainer & );

    /*!
      @brief clearContainers removes all containers.

      This destroys all removed containers.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void clearContainers( void );

    /*!
      @brief getContainerTypes determines all types of containers that are
             currently attached.

      @return A set containing all container type IDs.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::set<WIR_id_t> getContainerTypes( void ) const;

    /*!
      @brief This variant of getContainers returns a set of all containers that
             are currently attached at all.

      @return A set containing (wrapped) references to all attached containers.

      Since the containers can be of different type, the returned set contains
      generic references of type WIR_BaseContainer.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const WIR_ContainerSet getContainers( void ) const;

    /*!
      @brief This variant of getContainers returns a correctly casted set of
             attached containers of type DerivedContainerClass.

      @tparam DerivedContainerClass The name of a derived container class.
      @return A set containing (wrapped) references to all attached containers
              of template type DerivedContainerClass. The returned references
              are already correctly casted.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    template<typename DerivedContainerClass>
    const std::set<std::reference_wrapper<DerivedContainerClass>,
                   WIR_Compare<DerivedContainerClass>> getContainers( void ) const
    {
      DSTART( "const std::set<std::reference_wrapper<_Tp>, WIR::WIR_Compare<DerivedContainerClass> > WIR::WIR_Container_API::getContainers() const" );

      std::set<std::reference_wrapper<DerivedContainerClass>, WIR_Compare<DerivedContainerClass>> res;
      WIR_id_t t = DerivedContainerClass::getContainerTypeID();
      auto it = mContainerPointers.find( t );

      if ( it != mContainerPointers.end() ) {
        auto &containers = it->second;

        for ( auto &c : containers ) {
          DerivedContainerClass &ref =
            dynamic_cast<DerivedContainerClass &>( *( c.get() ) );
          res.insert( ref );
        }
      }

      return( res );
    };

    /*!
      @brief containsContainers returns whether some containers are attached at
             all.

      @return true if mContainerPointers contains a container at all, false
              otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool containsContainers( void ) const;

    /*!
      @brief containsContainers returns whether containers of the specified type
             are attached.

      @param[in] id A container type's ID to be found.
      @return true if mContainerPointers contains containers of type id, false
              otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool containsContainers( WIR_id_t ) const;

    /*!
      @brief containsContainer returns whether the specified container is
             attached.

      @param[in] c A const reference to the container to be found.
      @return true if mContainerPointers contains container c, false
              otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool containsContainer( const WIR_BaseContainer & ) const;


  protected:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Container_API( void );

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Container_API( const WIR_Container_API & );

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Container_API( WIR_Container_API && );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_Container_API( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Container_API & operator = ( const WIR_Container_API & );

    /*!
      @brief Move-assignment operator.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Container_API & operator = ( WIR_Container_API && );


  private:

    /*!
      @brief copyContainers performs actions common to the copy constructor and
             copy assignment operator of WIR_Container_API.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void copyContainers( const WIR_Container_API & );

    /*!
      @brief mContainerPointers maps IDs of container types to sets of managed
             pointers to all stored %WIR containers of that type.
    */
    std::map<WIR_id_t, std::set<std::unique_ptr<WIR_BaseContainer>>> mContainerPointers;

};

}       // namespace WIR

#endif  // _WIR_CONTAINER_API_H
