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
  @file tcmaxdelaypriority.h
  @brief This file provides the interface of a class computing the maximum delay
         scheduling priority for TriCore operations.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _TC_MAXDELAYPRIORITY_H
#define _TC_MAXDELAYPRIORITY_H


//
// Include section
//

// Include WIR headers
#include <optimizations/scheduling/wirmaxdelaypriority.h>
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
  @brief Class TC_MaxDelayPriority determines maximum delay scheduling
         priorities for TriCore operations.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class TC_MaxDelayPriority : public WIR_MaxDelayPriority,
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
    TC_MaxDelayPriority( void );

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC_MaxDelayPriority( const TC_MaxDelayPriority & );

    /*!
      @brief Destructor.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~TC_MaxDelayPriority( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC_MaxDelayPriority & operator = ( const TC_MaxDelayPriority & );


  protected:

    /*!
      @brief clone creates a copy of a TriCore maximum delay scheduling
             priority.

      @return A pointer to the newly created copy of this priority object.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_SchedulingPriority *clone( void ) const;

};

}       // namespace WIR

#endif  // _TC_MAXDELAYPRIORITY_H
