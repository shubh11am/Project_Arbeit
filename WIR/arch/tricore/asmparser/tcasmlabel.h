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
  @file tcasmlabel.h
  @brief This file provides the interface of label arguments.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _TC_ASMLABEL_H
#define _TC_ASMLABEL_H


//
// Include section
//

// Include standard headers
#include <string>

// Include local headers
#include "tcasmargument.h"


//
// Header section
//

namespace WIR {

/*!
  @brief Class TC_AsmLabel represents label arguments for an assembly operation.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class TC_AsmLabel final : public TC_AsmArgument
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor creating a label argument with a given name.

      @param[in] l A const reference to a label's name.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit TC_AsmLabel( const std::string & );

    /*!
      @brief Copy constructor.

      @param[in] l A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC_AsmLabel( const TC_AsmLabel & );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~TC_AsmLabel( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC_AsmLabel & operator = ( const TC_AsmLabel & );


    //
    // Type management.
    //

    /*!
      @brief isCompatible returns whether a label is compatible with a given
             argument type.

      @param[in] t A specifier denoting the argument's type.
      @return true if the label is compatible with t's type, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool isCompatible( Type ) const;


    //
    // Value management.
    //

    /*!
      @brief getName returns a label argument's name.

      @return The label's name.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::string getName( void ) const;


  protected:

    /*!
      @brief clone creates a copy of a label argument.

      @return A pointer to the newly created copy of this label.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual TC_AsmLabel *clone( void ) const;


  private:

    /*!
      @brief No standard construction allowed, users must use
             TC_AsmLabel( string )
             instead.
    */
    TC_AsmLabel( void ) = delete;

    //! mName holds a label argument's name.
    std::string mName;

};

}       // namespace WIR

#endif  // _TC_ASMLABEL_H
