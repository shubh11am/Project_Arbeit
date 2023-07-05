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
  @file wir.h
  @brief This file provides the complete interface of the entire %WIR library.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_H
#define _WIR_H


//
// Include section
//

// Include generic WIR headers
#include <wir/wiraddressingmodeparameter.h>
#include <wir/wirbasecontainer.h>
#include <wir/wirbaseimmediateparameter.h>
#include <wir/wirbaseprocessor.h>
#include <wir/wirbaseregister.h>
#include <wir/wirbasicblock.h>
#include <wir/wirbus.h>
#include <wir/wirbusarbitration.h>
#include <wir/wircache.h>
#include <wir/wirconditionfieldparameter.h>
#include <wir/wircompilationunit.h>
#include <wir/wircontainer.h>
#include <wir/wirdata.h>
#include <wir/wirdatainit.h>
#include <wir/wirfpbusarbitration.h>
#include <wir/wirfunction.h>
#include <wir/wirimmediateparameter.h>
#include <wir/wirinstruction.h>
#include <wir/wirio.h>
#include <wir/wirlabelparameter.h>
#include <wir/wirmemoryregion.h>
#include <wir/wirmisc.h>
#include <wir/wiroperation.h>
#include <wir/wiroperationformat.h>
#include <wir/wirparameter.h>
#include <wir/wirpdbusarbitration.h>
#include <wir/wirphysicalregister.h>
#include <wir/wirprocessor.h>
#include <wir/wirregister.h>
#include <wir/wirregisterparameter.h>
#include <wir/wirregistry.h>
#include <wir/wirroundrobinbusarbitration.h>
#include <wir/wirsection.h>
#include <wir/wirsignedimmediateparameter.h>
#include <wir/wirstats.h>
#include <wir/wirstringparameter.h>
#include <wir/wirsymbol.h>
#include <wir/wirsystem.h>
#include <wir/wirtaskmanager.h>
#include <wir/wirtdmabusarbitration.h>
#include <wir/wirtypes.h>
#include <wir/wirunsignedimmediateparameter.h>
#include <wir/wirvirtualregister.h>

// Include WIR containers
#include <containers/wiravailabledefinitions.h>
#include <containers/wirbitvalues.h>
#include <containers/wirbreak.h>
#include <containers/wircomment.h>
#include <containers/wircontroltree.h>
#include <containers/wirdataaccess.h>
#include <containers/wirdomination.h>
#include <containers/wirduudchain.h>
#include <containers/wirfileinfo.h>
#include <containers/wirflowfactref.h>
#include <containers/wirimmediatedomination.h>
#include <containers/wirliveout.h>
#include <containers/wirloopexit.h>
#include <containers/wirreachability.h>
#include <containers/wirreachingdefinitions.h>
#include <containers/wirschedulingconstraint.h>

// Include WIR analyses
#include <analyses/availabledefinitions/wiravailabledefinitionsanalysis.h>
#include <analyses/bit/wirbitdfa.h>
#include <analyses/bit/wirupdownvalue.h>
#include <analyses/controlflow/wircfganalysis.h>
#include <analyses/domination/wirdominationanalysis.h>
#include <analyses/duudchain/wirduudchainanalysis.h>
#include <analyses/generic/wiranalysis.h>
#include <analyses/immediatedomination/wirimmediatedominationanalysis.h>
#include <analyses/lifeness/wirlifenessanalysis.h>
#include <analyses/reachability/wirreachabilityanalysis.h>
#include <analyses/reachingdefinitions/wirreachingdefinitionsanalysis.h>
#include <analyses/structuralcontrolflow/wirstructuralanalysis.h>

// Include WIR optimizations
#include <optimizations/deadcode/wirdeadcode.h>
#include <optimizations/emptyblocks/wiremptyblocks.h>
#include <optimizations/loopinvariantcm/wirloopinvariantcm.h>
#include <optimizations/generic/wiroptimization.h>
#include <optimizations/peephole/wirpeephole.h>
#include <optimizations/redundantblocks/wirredundantblocks.h>
#include <optimizations/redundantcode/wirredundantcode.h>
#include <optimizations/unreachableblocks/wirunreachableblocks.h>
#include <optimizations/unusedvregs/wirunusedvregs.h>
#include <optimizations/webs/wirwebs.h>

// Include WIR flow facts
#include <flowfacts/wirentrypoint.h>
#include <flowfacts/wirflowfactupdater.h>
#include <flowfacts/wirflowrestriction.h>
#include <flowfacts/wirloopbound.h>

#endif  // _WIR_H
