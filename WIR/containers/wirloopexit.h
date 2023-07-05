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
  @file wirloopexit.h
  @brief This file provides the interface of a %WIR container marking branches
         that are regular exits of ANSI-C for and while-do loops.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_LOOPEXIT_H
#define _WIR_LOOPEXIT_H


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
  @brief Class WIR_LoopExit marks branch operations that are regular exits of
         ANSI-C for, while-do or do-while loops.

  WIR_LoopExit containers are supposed to be attached to WIR_Operation objects
  during code selection and are exploited by %WIR's structural control flow
  analysis.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_LoopExit : public WIR_Container<WIR_LoopExit>
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor.

      @param[in] b A Boolean flag denoting whether the branches explicit jump
                   target or its implicit successor is the loop exit.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_LoopExit( bool );

    /*!
      @brief Destructor.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_LoopExit( void );

    /*!
      @brief isUnique returns whether loop exit containers are unique, i.e.,
             whether at most one instance of this container type can be attached
             to a %WIR class.

      @return Always true, loop exit containers are unique.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool isUnique( void ) const;

    /*!
      @brief explicitSuccessorIsExit returns whether the explicit or the
             implicit successor of a branch is the loop exit.

      @return true if the explicit successor of a branch is the loop exit, false
              otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool explicitSuccessorIsExit( void ) const;


  private:

    /*!
      @brief A Boolean denoting whether the explicit or the implicit successor
             of a branch is the loop exit.
    */
    bool mExplicitSuccIsExit;

};

}       // namespace WIR

#endif  // _WIR_LOOPEXIT_H
