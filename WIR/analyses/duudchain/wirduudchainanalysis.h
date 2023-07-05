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
  @file wirduudchainanalysis.h
  @brief This file provides the interface of the def-use/use-def chain analysis.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_DUUDCHAINANALYSIS_H
#define _WIR_DUUDCHAINANALYSIS_H


//
// Include section
//

// Include WIR headers
#include <wir/wirtypes.h>
#include <analyses/generic/wiranalysis.h>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_Function;


/*!
  @brief Class WIR_DUUDChainAnalysis is the %WIR def-use/use-def chain analysis.

  Analysis results are stored in WIR_DUUDChain containers that are attached to
  %WIR register parameters.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_DUUDChainAnalysis : public WIR_Analysis
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
    explicit WIR_DUUDChainAnalysis( WIR_Function & );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_DUUDChainAnalysis( void );


    //
    // Analysis of virtual and physical registers.
    //

    /*!
      @brief setVirtualRegistersOnly sets whether the analysis should consider
             only virtual registers or whether both virtual and physical
             registers are analyzed.

      @param[in] b A Boolean denoting whether a only virtual (true) or both
                   virtual and physical registers (false) are analyzed.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setVirtualRegistersOnly( bool = true );

    /*!
      @brief getVirtualRegistersOnly returns whether only virtual or both
             virtual and physical registers are analyzed.

      @return True if only virtual registers are analyzed, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool getVirtualRegistersOnly( void ) const;


  protected:

    /*!
      @brief runAnalysis performs def-use/use-def chain analysis of the given
             function.

      @param[in] f A reference to a WIR_Function to be analyzed.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void runAnalysis( WIR_Function & );


  private:

    /*!
      @brief init initializes data structures by attaching fresh containers and
             doing a reaching-definitions analysis.

      @param[in] f A reference to a WIR_Function to be analyzed.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void init( WIR_Function & );

    /*!
      @brief mVirtualRegistersOnly stores whether the analysis should consider
             only virtual registers or whether both virtual and physical
             registers are analyzed.

      By defaults, both virtual and physical registers are analyzed.
    */
    bool mVirtualRegistersOnly;

};

}       // namespace WIR

#endif  // _WIR_DUUDCHAINANALYSIS_H
