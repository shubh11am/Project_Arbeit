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
  @file wirbreak.h
  @brief This file provides the interface of a %WIR container marking branches
         resulting from ANSI-C break statements.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_BREAK_H
#define _WIR_BREAK_H


//
// Include section
//

// Include WIR headers
#include <wir/wircontainer.h>


//
// Header section
//

namespace WIR {

/*!
  @brief Class WIR_Break marks branch operations that result from an ANSI-C
         break statement.

  WIR_Break containers are supposed to be attached to WIR_Operation objects
  during code selection and are exploited by %WIR's structural control flow
  analysis.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_Break : public WIR_Container<WIR_Break>
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Break( void );

    /*!
      @brief Destructor.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_Break( void );

    /*!
      @brief isUnique returns whether break containers are unique, i.e., whether
             at most one instance of this container type can be attached to a
             %WIR class.

      @return Always true, break containers are unique.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool isUnique( void ) const;

};

}       // namespace WIR

#endif  // _WIR_BREAK_H
