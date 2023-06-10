/*

   This header file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Compiler Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2020 - 2022

   Hamburg University of Technology (TUHH)
   Institute of Embedded Systems
   21071 Hamburg
   Germany

   http://www.tuhh.de/es/esd/research/wcc

*/


#ifndef _TC_ADDRESSWITHOFFSET_H
#define _TC_ADDRESSWITHOFFSET_H


//
// Include section
//

// Include local headers
#include <codesel/addresswithoffset.h>


//
// Class forward declarations
//

class LLIR_Register;

namespace WIR {
class TC_ARegV;
}


//
// Header section
//

/*!
  @brief Class TC_AddressWithOffset identifies TriCore memory accesses in short
         form.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class TC_AddressWithOffset : public AddressWithOffset
{

  public:

    /*!
      @brief Constructor initializing an empty address + offset.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC_AddressWithOffset( void );

    /*!
      @brief Constructor initializing an empty address + offset based memory
             access.

      @param[in] reg A pointer to the involved LLIR address register.
      @param[in] r A const reference to the involved %WIR address register.
      @param[in] o A long integer denoting the offset of the memory access.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC_AddressWithOffset( LLIR_Register *, const WIR::TC_ARegV &, long );

    /*!
      @brief getAReg returns the address register involved in an address
             modification.

      @return A reference to a an address register.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR::TC_ARegV &getAReg( void ) const;


  protected:

    //! mAReg points to the involved %WIR address register.
    WIR::TC_ARegV *mAReg;

};

#endif  // _TC_ADDRESSWITHOFFSET_H
