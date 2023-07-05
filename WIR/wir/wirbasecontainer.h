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
  @file wirbasecontainer.h
  @brief This file provides the basic interface of generic %WIR containers that
         can freely be attached to the core classes of the %WIR library.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_BASECONTAINER_H
#define _WIR_BASECONTAINER_H


//
// Include section
//

// Include WIR headers
#include <wir/API/wiridapi.h>


//
// Header section
//

namespace WIR {

/*!
  @brief Class WIR_BaseContainer models a generic container for arbitrary
         meta-data.

  Such containers can be attached to the %WIR core classes by using the
  pushBackContainer/pushFrontContainer/getContainers/... API of the
  corresponding classes.

  The actual meta-data to be attached to %WIR core classes is modeled by
  inheriting from this class and by adding appropriate members, setters and
  getters. However, generic mechanisms are required in order to distinguish
  different types of meta-data, e.g., to distinguish between containers for
  WCET and ACET data. This generic container type handling is introduced in
  class WIR_Container that inherits from WIR_BaseContainer. So, actual
  containers for, e.g., WCET or ACET data must inherit from WIR_Container and
  not from WIR_BaseContainer. End-user code shall never make use of and directly
  inherit from WIR_BaseContainer.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_BaseContainer : public WIR_ID_API
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor creating an empty container.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_BaseContainer( void );

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_BaseContainer( const WIR_BaseContainer & );

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_BaseContainer( WIR_BaseContainer && );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_BaseContainer( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_BaseContainer & operator = ( const WIR_BaseContainer & );

    /*!
      @brief Move-assignment operator.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_BaseContainer & operator = ( WIR_BaseContainer && );

    /*!
      @brief isUnique returns whether a container is unique, i.e., whether at
             most one instance of this container type can be attached to a %WIR
             class.

      @return true if the container is unique, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool isUnique( void ) const = 0;


    //
    // Container type handling.
    //

    /*!
      @brief getContainerType returns the ID of a container type.

      @return The container type's ID.

      getContainerType can be used to query types for actual container objects.
      Since actual containers are defined by inheriting from this class,
      getContainerType is purely virtual here.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_id_t getContainerType( void ) const = 0;


  protected:

    /*!
      @brief Class Registrator serves to initialize static members of %WIR
             containers and automatically registers a new unique ID per
             container type.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    class Registrator
    {

      public:

        // This class has almost no public interface so that almost all standard
        // constructors and operators are deleted.
        Registrator( void ) = delete;
        Registrator( const Registrator & ) = delete;
        Registrator( Registrator && ) = delete;
        Registrator & operator = ( const Registrator & ) = delete;
        Registrator & operator = ( Registrator && ) = delete;

        /*!
          @brief Default constructor registering a new container type.

          @param[out] id A reference that finally holds the new container type's
                         unique ID.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        explicit Registrator( WIR_id_t & );

        /*!
          @brief touch is a dummy method that just serves to activate the
                 initialization of static data members.

          Objects of class 'Registrator' are used as static initializers for
          class WIR_Container. Since WIR_Container is a templated class,
          instantiation of its static data members does not occur until these
          are explicitly referenced. For this purpose, this method is provided:
          WIR_Container can 'touch' its static data member so that it will get
          initialized.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        void touch( void );

    };


  private:

    friend class Registrator;
    friend class WIR_Container_API;

    /*!
      @brief registerNewContainerType registers a new container type.

      @param[out] id A reference that finally holds the unique ID for the new
                     container type.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    static void registerNewContainerType( WIR_id_t & );

    /*!
      @brief clone creates a copy of a %WIR container.

      @return A pointer to the newly created copy of this container.

      In the derived class WIR_Container<typename T>, clone shall just call the
      copy constructor so that by using the template argument, the correct copy
      constructor of the actual container class (which is unknown here) is
      called. For this purpose, clone is a purely virtual method here.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_BaseContainer *clone( void ) const = 0;

    //! mTypeID holds the next free ID for new container types.
    static WIR_id_t mTypeID;

};

}       // namespace WIR

#endif  // _WIR_BASECONTAINER_H
