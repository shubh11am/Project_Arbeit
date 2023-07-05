/*

   This source file belongs to the

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
  @file wiridapi.cc
  @brief This file implements a base class for managing unique numerical IDs of
         derived classes.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include standard headers
#include <iostream>

// Include libuseful headers
#include <libuseful/debugmacros.h>

// Include WIR headers
#include <wir/wirregistry.h>

// Include private headers
#include "wiridapi.h"


//
// Code section
//

namespace WIR {


using namespace std;


// mIDCounter contains the next free numerial ID for WIR objects.
WIR_id_t WIR_ID_API::mIDCounter = 1;

}       // namespace WIR
