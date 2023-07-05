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
  @file wircfganalysis.h
  @brief This file provides the interface of generic CFG-based control flow
         analyses.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_CFGANALYSIS_H
#define _WIR_CFGANALYSIS_H


//
// Include section
//

// Include WIR headers
#include <analyses/generic/wiranalysis.h>
#include <analyses/controlflow/wircfg.h>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_Function;


/*!
  @brief Class WIR_ControlFlowAnalysis is a generic base class for control flow
         analysis based on the %WIR control flow graph.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_ControlFlowAnalysis : public WIR_Analysis,
                                public WIR_CFG
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor for function-level analysis.

      @param[in] f A reference to a WIR_Function to be analyzed.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_ControlFlowAnalysis( WIR_Function & );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_ControlFlowAnalysis( void );

};

}       // namespace WIR

#endif  // _WIR_CFGANALYSIS_H
