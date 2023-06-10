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


#ifndef _RV32_ADDRESSWITHOFFSET_H
#define _RV32_ADDRESSWITHOFFSET_H

//
// Include section
//

// Include local headers
#include <codesel/addresswithoffset.h>
#include <rv32/rv32codesel.h>

//
// Class forward declarations
//


namespace WIR {
class WIR_BaseRegister;
class WIR_Data;
class RV_RegV;
}


//
// Header section
//

/*!
  @brief Class RV32_AddressWithOffset identifies riscv memory accesses in short
         form.
*/
namespace RV32 {
class RV32_LValue;

class RV32_AddressWithOffset : public AddressWithOffset
{

  public:

    /*!
      @brief Constructor initializing an empty address + offset.
    */
    RV32_AddressWithOffset( void );

    /*!
      @brief Constructor initializing an empty address + offset based memory
             access.

      @param[in] r A const reference to the involved %WIR address register.
      @param[in] o A long integer denoting the offset of the memory access.
    */
    RV32_AddressWithOffset( WIR::WIR_BaseRegister &, long );

    /*!
      @brief getAReg returns the address register involved in an address
             modification.

      @return A reference to a an address register.
    */
    WIR::RV_RegV &getAReg( void ) const;


  protected:

    //! mAReg points to the involved %WIR address register.
    WIR::RV_RegV *mAReg;

};
}
#endif  // _RV32_ADDRESSWITHOFFSET_H
