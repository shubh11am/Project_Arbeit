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
  @file wirinheritableenum.h
  @brief This file provides an API for inheritable enumeration classes.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIRINHERITABLEENUM_H
#define _WIRINHERITABLEENUM_H


//
// Header section
//

namespace WIR {

/*!
  @brief Class WIR_InheritableEnum quasi models enumeration types that can be
         inherited.

  This class serves as base class from which actual enumerations are derived.
  This kind of enumerations is mostly used to model processor-specific features
  like, e.g., mnemonics or addressing modes.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_InheritableEnum
{

  public:

    /*!
      @brief The == operator checks for equality of enumerators.

      @param[in] __o A const reference to another object to be compared.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool operator == ( const WIR_InheritableEnum & ) const;

    /*!
      @brief The != operator checks for inequality of enumerators.

      @param[in] __o A const reference to another object to be compared.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool operator != ( const WIR_InheritableEnum & ) const;

    /*!
      @brief The < operator checks for less-than of enumerators.

      @param[in] __o A const reference to another object to be compared.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool operator < ( const WIR_InheritableEnum & ) const;


  protected:

    friend class WIR_Registry;

    /*!
      @brief Default constructor assigning a new unique ID to an enumerator.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_InheritableEnum( void );

    //! mID holds an enumerator's unique ID.
    const unsigned int mID;

    //! mMaxValue stores the next free ID.
    static unsigned int mMaxValue;

};

}       // namespace WIR

#endif  // _WIRINHERITABLEENUM_H
