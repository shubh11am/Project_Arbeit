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
  @file wirnameapi.h
  @brief This file provides the interface of a base class for managing names of
         named derived classes.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_NAME_API_H
#define _WIR_NAME_API_H


//
// Include section
//

// Include standard headers
#include <string>


//
// Header section
//

namespace WIR {

/*!
  @brief Class WIR_Name_API provides a simple API to assign and retrieve names
         to/from named %WIR objects.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_Name_API
{

  public:

    //
    // Methods to assign and retrieve names.
    //

    /*!
      @brief setName sets an object's specific name.

      @param[in] s A const reference to a string to be copied that holds the
                   object's name.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setName( const std::string & );

    /*!
      @brief setName sets an object's specific name.

      @param[in] s An R-value reference to a string to be moved that holds the
                   object's name.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setName( std::string && );

    /*!
      @brief getName returns an object's specific name.

      @return A string that holds the object's name.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::string getName( void ) const;


  protected:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor assigning an empty name.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Name_API( void );

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Name_API( const WIR_Name_API & );

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Name_API( WIR_Name_API && );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_Name_API( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Name_API & operator = ( const WIR_Name_API & );

    /*!
      @brief Move-assignment operator.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Name_API & operator = ( WIR_Name_API && );


  private:

    //! mName holds an object's name.
    std::string mName;

};

}       // namespace WIR

#endif  // _WIR_NAME_API_H
