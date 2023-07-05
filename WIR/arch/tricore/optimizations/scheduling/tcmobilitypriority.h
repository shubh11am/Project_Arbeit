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
  @file tcmobilitypriority.h
  @brief This file provides the interface of a base class computing the mobility
         scheduling priority for TriCore operations.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _TC_MBOBILITYPRIORITY_H
#define _TC_MBOBILITYPRIORITY_H


//
// Include section
//

// Include WIR headers
#include <optimizations/scheduling/wirmobilitypriority.h>
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
  @brief Class TC_MobilityPriority is a class used to determine scheduling
         priorities based on a TriCore operation's mobility.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class TC_MobilityPriority : public WIR_MobilityPriority,
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
    TC_MobilityPriority( void );

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC_MobilityPriority( const TC_MobilityPriority & );

    /*!
      @brief Destructor.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~TC_MobilityPriority( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC_MobilityPriority & operator = ( const TC_MobilityPriority & );


  protected:

    /*!
      @brief clone creates a copy of a TriCore mobility scheduling priority.

      @return A pointer to the newly created copy of this priority object.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_SchedulingPriority *clone( void ) const;

};

}       // namespace WIR

#endif  // _TC_MBOBILITYPRIORITY_H
