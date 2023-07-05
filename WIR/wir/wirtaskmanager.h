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
  @file wirtaskmanager.h
  @brief This file provides the interface of a %WIR task manager.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_TASKMANAGER_H
#define _WIR_TASKMANAGER_H


//
// Include section
//

// Include standard headers
#include <list>
#include <utility>

// Include WIR headers
#include <wir/API/wiridapi.h>
#include <wir/API/wirinsertionapi.h>
#include <wir/wirtypes.h>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_System;
class WIR_EntryPoint;


/*!
  @brief Class WIR_TaskManager models all tasks executed on a %WIR system.

  WIR_TaskManager simply provides a list of %WIR functions that serve as entry
  points of tasks, plus the facility to specify communication and dependencies
  between pairs of tasks.

  This class serves as virtual base class. More sophisticated task properties
  and support for various scheduling policies can be added by inheriting from
  this class.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_TaskManager : public WIR_ID_API
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor creating an empty task manager.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_TaskManager( void );

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      When copying a task manager that is inserted in some %WIR system, the
      resulting copy will not be inserted in a system.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_TaskManager( const WIR_TaskManager & );

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      Trying to move a task manager that is inserted in some %WIR system results
      in an assertion, since you are not allowed to move a task manager whose
      ownership is managed by a system.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_TaskManager( WIR_TaskManager && );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_TaskManager( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      When copying a task manager that is inserted in some %WIR system, the
      resulting copy will not be inserted in a system.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_TaskManager & operator = ( const WIR_TaskManager & );

    /*!
      @brief Move-assignment operator.

      @param[in] __o An R-value reference to another object to be moved.

      Trying to move a task manager that is inserted in some %WIR system results
      in an assertion, since you are not allowed to move a task manager whose
      ownership is managed by a system.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_TaskManager & operator = ( WIR_TaskManager && );


    //
    // System handling.
    //

    // Realize the API to manage a task manager's parent system.
    WIR_INSERTION_DECL( WIR_System, System );


    //
    // Entry point handling.
    //

    /*!
      @brief getEntryPoints returns all entry point flow facts attached to a
             %WIR system.

      @return A list of references to all %WIR entry points.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    std::list<std::reference_wrapper<WIR_EntryPoint>> getEntryPoints( void ) const;

    /*!
      @brief getTaskEntries returns all basic blocks being task entries of a
             %WIR system.

      @return The set of those basic blocks being the first ones in all
              functions referenced by entry point flow facts.

      @author Til Mauersberger <Til.Mauersberger@tuhh.de>
    */
    WIR_BasicBlockSet getTaskEntries( void ) const;


  protected:

    /*!
      @brief clone creates a copy of a %WIR task manager.

      @return A pointer to the newly created copy of this parameter.

      Clone just calls the corresponding copy constructor.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_TaskManager *clone( void ) const;

};

}       // namespace WIR

#endif  // _WIR_TASKMANAGER_H
