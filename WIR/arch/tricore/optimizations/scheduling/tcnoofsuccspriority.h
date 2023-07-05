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
  @file tcnoofsuccspriority.h
  @brief This file provides the interface of an abstract base class computing
         the number of successor operations scheduling priority for TriCore
         operations.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _TC_NOOFSUCCSPRIORITY_H
#define _TC_NOOFSUCCSPRIORITY_H


//
// Include section
//

// Include WIR headers
#include <optimizations/scheduling/wirnoofsuccspriority.h>
#include <arch/tricore/optimizations/scheduling/tcschedulingpriority.h>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_Operation;


/*!
  @brief Class TC_NoOfSuccsPriority is a class used to determine scheduling
         priorities based on the number of successors of a TriCore operation
         in the scheduling dependence graph.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class TC_NoOfSuccsPriority : public WIR_NoOfSuccsPriority,
                             public TC_SchedulingPriority
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC_NoOfSuccsPriority( void );

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC_NoOfSuccsPriority( const TC_NoOfSuccsPriority & );

    /*!
      @brief Destructor.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~TC_NoOfSuccsPriority( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC_NoOfSuccsPriority & operator = ( const TC_NoOfSuccsPriority & );


  protected:

    /*!
      @brief clone creates a copy of a TriCore number of successors scheduling
             priority.

      @return A pointer to the newly created copy of this priority object.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_SchedulingPriority *clone( void ) const;

};

}       // namespace WIR

#endif  // _TC_NOOFSUCCSPRIORITY_H
