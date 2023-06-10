#
#
#  This source file belongs to the
#
#           Hamburg University of Technology (TUHH)
#             WCC Compiler Framework
#
#  and is property of its respective copyright holder. It must neither be used
#  nor published even in parts without explicit written permission.
#
#  Copyright 2010 - 2022
#
#  Hamburg University of Technology (TUHH)
#  Institute of Embedded Systems
#  21071 Hamburg
#  Germany
#
#  http://www.tuhh.de/es/esd/research/wcc
#
#


###############################################################################
#
#
# Helper rules for Assignment Expressions
#
#
###############################################################################


############################################################################
#
# Rules for getting and casting (if needed) the LHS of assignments
#
############################################################################

# The following casts may be needed:
#
# llereg; reg        -> ca_lhs_dreg (if reg holds a float)
# reg; ereg; llereg  -> ca_lhs_ereg
# reg; llereg        -> ca_lhs_llereg (if reg holds no float)
# areg               -> ca_lhs_areg
#
# Every conversion must also be done with the respective deref_... nonterminals

# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_lhs_ereg: ereg
{
  // Direct conversion "ereg" -> "ca_lhs_ereg"
  if ( isAssignmentLHS( *$1->getExp() ) &&
       isDoubleType( getComputationLevelType( *$1->getExp() ) ) )
    $cost[0] = $cost[1] + lhsConversionCost( *$1->getExp() );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "ca_lhs_ereg: ereg", $1 );

  auto p = $action[1]();

  WriteBackInfo wbInfo( p.first, p.second );
  doLhsConversion( *$1->getExp(), wbInfo );

  return( wbInfo );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_lhs_ereg: deref_ereg
{
  // Direct conversion "deref_ereg" -> "ca_lhs_ereg"
  if ( isAssignmentLHS( *$1->getExp() ) &&
       isDoubleType( getComputationLevelType( *$1->getExp() ) ) )
    $cost[0] = $cost[1] + lhsConversionCost( *$1->getExp() );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "ca_lhs_ereg: deref_ereg", $1 );

  WriteBackInfo wbInfo( $action[1]( loadResult ) );
  doLhsConversion( *$1->getExp(), wbInfo );

  return( wbInfo );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_lhs_ereg: dreg
{
  // Conversion from "dreg" to "ca_lhs_ereg"
  if ( isAssignmentLHS( *$1->getExp() ) &&
       isDoubleType( getComputationLevelType( *$1->getExp() ) ) )
    $cost[0] = $cost[1] + lhsConversionCost( *$1->getExp() );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "ca_lhs_ereg: dreg", $1 );

  auto p = $action[1]();

  WriteBackInfo wbInfo( p.first, p.second );
  doLhsConversion( *$1->getExp(), wbInfo );

  return( wbInfo );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_lhs_ereg: deref_dreg
{
  if ( isAssignmentLHS( *$1->getExp() ) &&
       isDoubleType( getComputationLevelType( *$1->getExp() ) ) )
    $cost[0] = $cost[1] + lhsConversionCost( *$1->getExp() );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "ca_lhs_ereg: deref_dreg", $1 );

  WriteBackInfo wbInfo( $action[1]( loadResult ) );
  doLhsConversion( *$1->getExp(), wbInfo );

  return( wbInfo );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_lhs_ereg: llereg
{
  // Conversion from "llereg" to "ca_lhs_ereg"
  if ( isAssignmentLHS( *$1->getExp() ) &&
       isDoubleType( getComputationLevelType( *$1->getExp() ) ) )
    $cost[0] = $cost[1] + lhsConversionCost( *$1->getExp() );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "ca_lhs_ereg: llereg", $1 );

  auto p = $action[1]();

  WriteBackInfo wbInfo( p.first, p.second );
  doLhsConversion( *$1->getExp(), wbInfo );

  return( wbInfo );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_lhs_ereg: deref_llereg
{
  if ( isAssignmentLHS( *$1->getExp() ) &&
       isDoubleType( getComputationLevelType( *$1->getExp() ) ) )
    $cost[0] = $cost[1] + lhsConversionCost( *$1->getExp() );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "ca_lhs_ereg: deref_llereg", $1 );

  WriteBackInfo wbInfo( $action[1]( loadResult ) );
  doLhsConversion( *$1->getExp(), wbInfo );

  return( wbInfo );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_lhs_areg: areg
{
  // This rule performs the direct conversion "areg" -> "ca_lhs_areg".
  if ( isAssignmentLHS( *$1->getExp() ) &&
       isARegType( getComputationLevelType( *$1->getExp() ) ) )
    $cost[0] = $cost[1] + lhsConversionCost( *$1->getExp() );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "ca_lhs_areg: areg", $1 );

  auto p = $action[1]();

  WriteBackInfo wbInfo( p.first, p.second );
  doLhsConversion( *$1->getExp(), wbInfo );

  return( wbInfo );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_lhs_areg: deref_areg
{
  // Direct conversion "deref_areg" -> "ca_lhs_areg".
  if ( isAssignmentLHS( *$1->getExp() ) &&
       isARegType( getComputationLevelType( *$1->getExp() ) ) )
    $cost[0] = $cost[1] + lhsConversionCost( *$1->getExp() );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "ca_lhs_areg: deref_areg", $1 );

  WriteBackInfo wbInfo( $action[1]( loadResult ) );
  doLhsConversion( *$1->getExp(), wbInfo );

  return( wbInfo );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_lhs_dreg: dreg
{
  // Direct conversion "reg" -> "ca_lhs_dreg"
  if ( isAssignmentLHS( *$1->getExp() ) &&
       isDRegType( getComputationLevelType( *$1->getExp() ) ) )
    // Casting is needed for different reg types (e.g., float/int)
    $cost[0] = $cost[1] + lhsConversionCost( *$1->getExp() );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "ca_lhs_dreg: dreg", $1 );

  auto p = $action[1]();

  WriteBackInfo wbInfo( p.first, p.second );
  doLhsConversion( *$1->getExp(), wbInfo );

  return( wbInfo );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_lhs_dreg: deref_dreg
{
  if ( isAssignmentLHS( *$1->getExp() ) &&
       isDRegType( getComputationLevelType( *$1->getExp() ) ) )
    // Casting is needed for different reg types (e.g., float/int).
    $cost[0] = $cost[1] + lhsConversionCost( *$1->getExp() );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "ca_lhs_dreg: deref_dreg", $1 );

  WriteBackInfo wbInfo( $action[1]( loadResult ) );
  doLhsConversion( *$1->getExp(), wbInfo );

  return( wbInfo );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_lhs_dreg: llereg
{
  // Conversion from "llereg" to "ca_lhs_dreg" is only valid if the RHS is a
  // float value (then long long must be casted to float).
  if ( isAssignmentLHS( *$1->getExp() ) &&
       isDRegType( getComputationLevelType( *$1->getExp() ) ) )
    $cost[0] = $cost[1] + lhsConversionCost( *$1->getExp() );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "ca_lhs_dreg: llereg", $1 );

  auto p = $action[1]();

  WriteBackInfo wbInfo( p.first, p.second );
  doLhsConversion( *$1->getExp(), wbInfo );

  return( wbInfo );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_lhs_dreg: deref_llereg
{
  // Conversion from "deref_llereg" to "ca_lhs_dreg" is only valid if the RHS is
  // a float value (then long long must be casted to float).
  if ( isAssignmentLHS( *$1->getExp() ) &&
       isDRegType( getComputationLevelType( *$1->getExp() ) ) )
    $cost[0] = $cost[1] + lhsConversionCost( *$1->getExp() );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "ca_lhs_dreg: deref_llereg", $1 );

  WriteBackInfo wbInfo( $action[1]( loadResult ) );
  doLhsConversion( *$1->getExp(), wbInfo );

  return( wbInfo );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_lhs_llereg: llereg
{
  if ( isAssignmentLHS( *$1->getExp() ) &&
       isLongLongType( getComputationLevelType( *$1->getExp() ) ) )
    $cost[0] = $cost[1] + lhsConversionCost( *$1->getExp() );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "ca_lhs_llereg: llereg", $1 );

  auto p = $action[1]();

  WriteBackInfo wbInfo( p.first, p.second );
  doLhsConversion( *$1->getExp(), wbInfo );

  return( wbInfo );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_lhs_llereg: deref_llereg
{
  if ( isAssignmentLHS( *$1->getExp() ) &&
       isLongLongType( getComputationLevelType( *$1->getExp() ) ) )
    $cost[0] = $cost[1] + lhsConversionCost( *$1->getExp() );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "ca_lhs_llereg: deref_llereg", $1 );

  WriteBackInfo wbInfo( $action[1]( loadResult ) );
  doLhsConversion( *$1->getExp(), wbInfo );

  return( wbInfo );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_lhs_llereg: dreg
{
  // Conversion from "dreg" to "ca_lhs_llereg"
  if ( isAssignmentLHS( *$1->getExp() ) &&
       isLongLongType( getComputationLevelType( *$1->getExp() ) ) )
    $cost[0] = $cost[1] + lhsConversionCost( *$1->getExp() );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "ca_lhs_llereg: dreg", $1 );

  auto p = $action[1]();

  WriteBackInfo wbInfo( p.first, p.second );
  doLhsConversion( *$1->getExp(), wbInfo );

  return( wbInfo );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_lhs_llereg: deref_dreg
{
  if ( isAssignmentLHS( *$1->getExp() ) &&
       isLongLongType( getComputationLevelType( *$1->getExp() ) ) )
    $cost[0] = $cost[1] + lhsConversionCost( *$1->getExp() );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "ca_lhs_llereg: deref_dreg", $1 );

  WriteBackInfo wbInfo( $action[1]( loadResult ) );
  doLhsConversion( *$1->getExp(), wbInfo );

  return( wbInfo );
};


############################################################################
#
# Rules for writing and casting (if needed) back the results of assignments
#
############################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
ereg: ca_result_ereg
{
  if ( isDoubleType( $1->getExp()->getType() ) )
    $cost[0] = $cost[1] + castBackAndStoreCost( *$1->getExp() );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "ereg: ca_result_ereg", $1 );

  auto wbInfo = $action[1]();

  auto p = castBackAndStore( *$1->getExp(), wbInfo );

  return(
    make_pair( p.first, ref( dynamic_cast<TC_ERegV &>( *(p.second) ) ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: ca_result_ereg
{
  if ( isDRegType( $1->getExp()->getType() ) )
    $cost[0] = $cost[1] + castBackAndStoreCost( *$1->getExp() );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: ca_result_ereg", $1 );

  auto wbInfo = $action[1]();

  auto p = castBackAndStore( *$1->getExp(), wbInfo );

  return(
    make_pair( p.first, ref( dynamic_cast<TC_DRegV &>( *(p.second) ) ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
llereg: ca_result_ereg
{
  if ( isLongLongType( $1->getExp()->getType() ) )
    $cost[0] = $cost[1] + castBackAndStoreCost( *$1->getExp() );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "llereg: ca_result_ereg", $1 );

  auto wbInfo = $action[1]();

  auto p = castBackAndStore( *$1->getExp(), wbInfo );

  return(
    make_pair( p.first, ref( dynamic_cast<TC_ERegV &>( *(p.second) ) ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
areg: ca_result_areg
{
  if ( isARegType( $1->getExp()->getType() ) )
    $cost[0] = $cost[1] + castBackAndStoreCost( *$1->getExp() );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "areg: ca_result_areg", $1 );

  auto wbInfo = $action[1]();

  auto p = castBackAndStore( *$1->getExp(), wbInfo );

  return(
    make_pair( p.first, ref( dynamic_cast<TC_ARegV &>( *(p.second) ) ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: ca_result_dreg
{
  if ( isDRegType( $1->getExp()->getType() ) )
    $cost[0] = $cost[1] + castBackAndStoreCost( *$1->getExp() );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: ca_result_dreg", $1 );

  auto wbInfo = $action[1]();

  auto p = castBackAndStore( *$1->getExp(), wbInfo );

  return(
    make_pair( p.first, ref( dynamic_cast<TC_DRegV &>( *(p.second) ) ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
llereg: ca_result_dreg
{
  if ( isLongLongType( $1->getExp()->getType() ) )
    $cost[0] = $cost[1] + castBackAndStoreCost( *$1->getExp() );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "llereg: ca_result_dreg", $1 );

  auto wbInfo = $action[1]();

  auto p = castBackAndStore( *$1->getExp(), wbInfo );

  return(
    make_pair( p.first, ref( dynamic_cast<TC_ERegV &>( *(p.second) ) ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
llereg: ca_result_llereg
{
  if ( isLongLongType( $1->getExp()->getType() ) )
    $cost[0] = $cost[1] + castBackAndStoreCost( *$1->getExp() );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "llereg: ca_result_llereg", $1 );

  auto wbInfo = $action[1]();

  auto p = castBackAndStore( *$1->getExp(), wbInfo );

  return(
    make_pair( p.first, ref( dynamic_cast<TC_ERegV &>( *(p.second) ) ) ) );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
dreg: ca_result_llereg
{
  if ( isDRegType( $1->getExp()->getType() ) )
    $cost[0] = $cost[1] + castBackAndStoreCost( *$1->getExp() );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "dreg: ca_result_llereg", $1 );

  auto wbInfo = $action[1]();

  auto p = castBackAndStore( *$1->getExp(), wbInfo );

  return(
    make_pair( p.first, ref( dynamic_cast<TC_DRegV &>( *(p.second) ) ) ) );
};


###############################################################################
#
#
# Simple Assignment Expressions
#
#
###############################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_result_ereg: tpm_AssignExpASSIGN( ca_lhs_ereg, ereg )
{
  $cost[0] = $cost[2] + $cost[3] + 2 * TC13::OperationFormat::SDD_1.getSize();
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_ereg: tpm_AssignExpASSIGN( ca_lhs_ereg, ereg )", $1 );

  // Evaluate RHS first to account for side effects.
  auto p = $action[3]();
  auto wbInfo = $action[2]( false );

  // LLIR
  TCINSTRUCTIONS.insertMOV( wbInfo.getTempReg(), p.first, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertMOV(
    dynamic_cast<TC_ERegV &>( wbInfo.mTmpReg.get() ), p.second, $1->getExp() );

  return( wbInfo );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_result_areg: tpm_AssignExpASSIGN( ca_lhs_areg, areg )
{
  if ( isArrayType( *$2->getExp() ) &&
       dynamic_cast<IR_StringConstExp *>( $3->getExp() ) &&
       !isFunctionArgument( *$2->getExp() ) )
    $cost[0] = COST_INFINITY;
  else
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::SAA_1.getSize();
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_areg: tpm_AssignExpASSIGN( ca_lhs_areg, areg )", $1 );

  // Evaluate RHS first to account for side effects.
  auto p = $action[3]();
  auto wbInfo = $action[2]( false );

  // LLIR
  TCINSTRUCTIONS.insertMOV_AA( wbInfo.getTempReg(), p.first, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertMOV_AA(
    dynamic_cast<TC_ARegV &>( wbInfo.mTmpReg.get() ), p.second, $1->getExp() );

  return( wbInfo );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
areg: tpm_AssignExpASSIGN( areg, areg )
{
  // This rule handles assignments of string constants to char arrays.
  auto *rhsStringConstExp = dynamic_cast<IR_StringConstExp *>( $3->getExp() );

  if ( isArrayType( *$2->getExp() ) && rhsStringConstExp &&
       !isFunctionArgument( *$2->getExp() ) ) {
    // Determine how many characters have to be copied (either the size of the
    // character string or the size of the lhs array if known).
    auto *str = dynamic_cast<IR_StringConstExp *>( $3->getExp() );
    auto *arrayt = dynamic_cast<IR_FixArrayType *>( &$2->getExp()->getType() );
    const int copyLength =
      arrayt != 0 ?
        min<int>( arrayt->getSize(), str->getValue().size() ) :
        str->getValue().size();
    int remaining = $0->getExp()->getType().sizeOf() - copyLength - 1;

    $cost[0] =
      $cost[2] + $cost[3] + TC13::OperationFormat::DAC10PIA.getSize() +
      TC13::OperationFormat::AC10DPIA_1.getSize() +
      TC13::OperationFormat::DC4L_3.getSize();

    if ( ( copyLength >= TC_Const4_Signed::getMinValue( 4 ) ) &&
         ( copyLength <= TC_Const4_Signed::getMaxValue( 4 ) ) )
      $cost[0] += TC13::OperationFormat::SDC4_1.getSize();
    else

    if ( ( copyLength >= TC_Const16_Signed::getMinValue( 16 ) ) &&
         ( copyLength <= TC_Const16_Signed::getMaxValue( 16 ) ) )
      $cost[0] += TC13::OperationFormat::DC16_1.getSize();
    else

    if ( ( copyLength >= 0 ) &&
         ( copyLength <= (int) TC_Const16_Unsigned::getMaxValue( 16 ) ) )
      $cost[0] += TC13::OperationFormat::DC16_2.getSize();
    else
      $cost[0] +=
        TC13::OperationFormat::DC16_2.getSize() +
        TC13::OperationFormat::DDC16_1.getSize();

    if ( remaining > 0 ) {
      $cost[0] +=
        TC13::OperationFormat::SDC4_1.getSize() +
        TC13::OperationFormat::AC10DPIA_1.getSize() +
        TC13::OperationFormat::DC4L_3.getSize();

      if ( ( remaining >= TC_Const4_Signed::getMinValue( 4 ) ) &&
           ( remaining <= TC_Const4_Signed::getMaxValue( 4 ) ) )
        $cost[0] += TC13::OperationFormat::SDC4_1.getSize();
      else

      if ( ( remaining >= TC_Const16_Signed::getMinValue( 16 ) ) &&
           ( remaining <= TC_Const16_Signed::getMaxValue( 16 ) ) )
        $cost[0] += TC13::OperationFormat::DC16_1.getSize();
      else

      if ( ( remaining >= 0 ) &&
           ( remaining <= (int) TC_Const16_Unsigned::getMaxValue( 16 ) ) )
        $cost[0] += TC13::OperationFormat::DC16_2.getSize();
      else
        $cost[0] +=
          TC13::OperationFormat::DC16_2.getSize() +
          TC13::OperationFormat::DDC16_1.getSize();
    }
  } else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION( "areg: tpm_AssignExpASSIGN( areg, areg )", $1 );

  // Evaluate RHS first to account for side effects.
  auto p2 = $action[3]();
  auto p1 = $action[2]();

  // Determine how many characters have to be copied (either the size of the
  // character string or the size of the lhs array if known).
  auto *str = dynamic_cast<IR_StringConstExp *>( $3->getExp() );
  auto *arrayt = dynamic_cast<IR_FixArrayType *>( &$2->getExp()->getType() );
  const int copyLength =
    arrayt != 0 ?
      min<int>( arrayt->getSize(), str->getValue().size() ) :
      str->getValue().size();

  // Determine whether we can use the LOOP instruction.
  bool useLOOP =
    !TCCODESEL->getConfig()->getEnableLOOPInstruction() ||
    ( TCCODESEL->getNumberOfEnclosingLOOPs( $1->getExp()->getStmt() ) <= 1 );

  // Generate new loop that stores the string constant from $3 in the array of
  // $2.
  // TODO: The following loop could be sped up if we wouldn't use ST.B but ST.D
  //       as in the initlist rules - this requires a function
  //       "getStackOffset( IR_Exp& ) to determine the total offset of the LHS
  //       from the stack pointer. The ST.D/LD.D operations need a 2-byte
  //       aligned address and we can't guarantee this as long as we have no
  //       information about the alignment of $2.

  // LLIR
  LLIR_Register *loopCounter = nullptr;
  if ( useLOOP ) {
    loopCounter = TCINSTRUCTIONS.CreateRegister( "", true );
    TCINSTRUCTIONS.insertLEA( loopCounter, copyLength, $1->getExp() );
  } else {
    loopCounter = TCINSTRUCTIONS.CreateRegister( "", false );
    TCINSTRUCTIONS.insertMOV( loopCounter, copyLength, $1->getExp() );
  }

  LLIR_BB &loopBB =
    beginNewLLIRBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  TCCODESEL->addLoopboundToLLIR(
    &loopBB, copyLength + 1, copyLength + 1, LLIR_Loopbound::TAIL_CONTROLLED );

  LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertLD_B( regTmp, OPER_POSTINC, p2.first, 1, $1->getExp() );
  TCINSTRUCTIONS.insertST_B( OPER_POSTINC, p1.first, 1, regTmp, $1->getExp() );

  if ( useLOOP ) {
    TCINSTRUCTIONS.insertLOOP( loopCounter, loopBB.GetLabel(), $1->getExp() );
    TCCODESEL->getLastLLIRBB()->AddPragma(
      new LLIR_Pragma( "Loop condition: LOOP", true ) );
  } else {
    TCINSTRUCTIONS.insertJNED( loopCounter, 0, loopBB.GetLabel(), $1->getExp() );
    TCCODESEL->getLastLLIRBB()->AddPragma(
      new LLIR_Pragma( "Loop condition: DOWHILE", true ) );
  }

  beginNewLLIRBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );

  // WIR
  auto &lc =
    useLOOP ?
      static_cast<WIR_VirtualRegister &>( TCINSTRUCTIONS.createAReg() ) :
      static_cast<WIR_VirtualRegister &>( TCINSTRUCTIONS.createDReg() );
  if ( useLOOP )
    TCINSTRUCTIONS.insertLEA(
      dynamic_cast<TC_ARegV &>( lc ), copyLength, $1->getExp() );
  else
    TCINSTRUCTIONS.insertMOVConstant(
      dynamic_cast<TC_DRegV &>( lc ), copyLength, $1->getExp() );

  auto &b1 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  // TODO: Add loop bound!

  auto &tmpReg = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertLD_B(
    tmpReg, TC13::AddressingMode::post, p2.second, 1, $1->getExp() );
  TCINSTRUCTIONS.insertST_B(
    TC13::AddressingMode::post, p1.second, 1, tmpReg, $1->getExp() );

  if ( useLOOP )
    TCINSTRUCTIONS.insertLOOP(
      dynamic_cast<TC_ARegV &>( lc ), b1, $1->getExp() );
  else
    TCINSTRUCTIONS.insertJNED(
      dynamic_cast<TC_DRegV &>( lc ), 0, b1, $1->getExp() );

  TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );

  // Generate a second loop to fill the remaining array positions with '0'.
  int remaining = $0->getExp()->getType().sizeOf() - copyLength - 1;
  if ( remaining > 0 ) {
    // LLIR
    TCINSTRUCTIONS.insertMOV( regTmp, 0, $1->getExp() );
    if ( useLOOP ) {
      loopCounter = TCINSTRUCTIONS.CreateRegister( "", true );
      TCINSTRUCTIONS.insertLEA( loopCounter, remaining, $1->getExp() );
    } else {
      loopCounter = TCINSTRUCTIONS.CreateRegister( "", false );
      TCINSTRUCTIONS.insertMOV( loopCounter, remaining, $1->getExp() );
    }

    LLIR_BB &secondLoopBB =
      beginNewLLIRBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
    TCCODESEL->addLoopboundToLLIR(
      &secondLoopBB, remaining + 1, remaining + 1,
      LLIR_Loopbound::TAIL_CONTROLLED );

    TCINSTRUCTIONS.insertST_B( OPER_POSTINC, p1.first, 1, regTmp, $1->getExp() );

    if ( useLOOP ) {
      TCINSTRUCTIONS.insertLOOP(
        loopCounter, secondLoopBB.GetLabel(), $1->getExp() );
      TCCODESEL->getLastLLIRBB()->AddPragma(
        new LLIR_Pragma( "Loop condition: LOOP", true ) );
    } else {
      TCINSTRUCTIONS.insertJNED(
        loopCounter, 0, secondLoopBB.GetLabel(), $1->getExp() );
      TCCODESEL->getLastLLIRBB()->AddPragma(
        new LLIR_Pragma( "Loop condition: DOWHILE", true ) );
    }

    beginNewLLIRBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );

    // WIR
    TCINSTRUCTIONS.insertMOV( tmpReg, 0, $1->getExp() );
    if ( useLOOP )
      TCINSTRUCTIONS.insertLEA(
        dynamic_cast<TC_ARegV &>( lc ), remaining, $1->getExp() );
    else
      TCINSTRUCTIONS.insertMOVConstant(
        dynamic_cast<TC_DRegV &>( lc ), remaining, $1->getExp() );

    auto &b2 =
      TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
    // TODO: Add loop bound!

    TCINSTRUCTIONS.insertST_B(
      TC13::AddressingMode::post, p1.second, 1, tmpReg, $1->getExp() );

    if ( useLOOP )
      TCINSTRUCTIONS.insertLOOP(
        dynamic_cast<TC_ARegV &>( lc ), b2, $1->getExp() );
    else
      TCINSTRUCTIONS.insertJNED(
        dynamic_cast<TC_DRegV &>( lc ), 0, b2, $1->getExp() );

    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  }

  return( p1 );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_result_dreg: tpm_AssignExpASSIGN( ca_lhs_dreg, dreg )
{
  $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::SDD_1.getSize();
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_dreg: tpm_AssignExpASSIGN( ca_lhs_dreg, dreg )", $1 );

  // Evaluate RHS first to account for side effects.
  auto p = $action[3]();
  auto wbInfo = $action[2]( false );

  // LLIR
  TCINSTRUCTIONS.insertMOV( wbInfo.getTempReg(), p.first, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertMOV(
    dynamic_cast<TC_DRegV &>( wbInfo.mTmpReg.get() ), p.second, $1->getExp() );

  return( wbInfo );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_result_dreg: tpm_AssignExpASSIGN( ca_lhs_dreg, areg )
{
  $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::SDA_1.getSize();
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_dreg: tpm_AssignExpASSIGN( ca_lhs_dreg, areg )", $1 );

  // Evaluate RHS first to account for side effects.
  auto p = $action[3]();
  auto wbInfo = $action[2]( false );

  // LLIR
  TCINSTRUCTIONS.insertMOV_D( wbInfo.getTempReg(), p.first, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertMOV_D(
    dynamic_cast<TC_DRegV &>( wbInfo.mTmpReg.get() ), p.second, $1->getExp() );

  return( wbInfo );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_result_llereg: tpm_AssignExpASSIGN( ca_lhs_llereg, llereg )
{
  $cost[0] = $cost[2] + $cost[3] + 2 * TC13::OperationFormat::SDD_1.getSize();
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_llereg: tpm_AssignExpASSIGN( ca_lhs_llereg, llereg )", $1 );

  // Evaluate RHS first to account for side effects.
  auto p = $action[3]();
  auto wbInfo = $action[2]( false );

  // LLIR
  TCINSTRUCTIONS.insertMOV( wbInfo.getTempReg(), p.first, $1->getExp() );

  // WIR
  TCINSTRUCTIONS.insertMOV(
    dynamic_cast<TC_ERegV &>( wbInfo.mTmpReg.get() ), p.second, $1->getExp() );

  return( wbInfo );
};


###############################################################################
#
#
# Compound Assignment Expressions
#
#
###############################################################################


###############################################################################
#
# Arithmetic Operation PLUS
#
###############################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_result_ereg: tpm_AssignExpPLUS( ca_lhs_ereg, ereg )
{
  $cost[0] =
    $cost[2] + $cost[3] + 6 * TC13::OperationFormat::SDD_1.getSize() +
    TC13::OperationFormat::L.getSize();
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_ereg: tpm_AssignExpPLUS( ca_lhs_ereg, ereg )", $1 );

  // Evaluate RHS first to account for side effects and lower register pressure.
  auto p = $action[3]();
  auto wbInfo = $action[2]( true );

  // LLIR
  auto *reg = wbInfo.getTempReg();
  TCINSTRUCTIONS.insertADD_D( reg, reg, p.first, $1->getExp() );

  // WIR
  auto &r = dynamic_cast<TC_ERegV &>( wbInfo.mTmpReg.get() );
  TCINSTRUCTIONS.insertADD_D( r, r, p.second, $1->getExp() );

  return( wbInfo );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_result_dreg: tpm_AssignExpPLUS( ca_lhs_dreg, const9 )
{
  if ( $0->getExp()->getType().isIntegralType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_dreg: tpm_AssignExpPLUS( ca_lhs_dreg, const9 )", $1 );

  auto wbInfo = $action[2]( true );
  auto v = $action[3]().getIntValue();

  // LLIR
  auto *reg = wbInfo.getTempReg();
  TCINSTRUCTIONS.insertADD( reg, reg, v, $1->getExp() );

  // WIR
  auto &r = dynamic_cast<TC_DRegV &>( wbInfo.mTmpReg.get() );
  TCINSTRUCTIONS.insertADD( r, r, v, $1->getExp() );

  return( wbInfo );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_result_dreg: tpm_AssignExpPLUS( ca_lhs_dreg, dreg )
{
  $cost[0] = $cost[2] + $cost[3];

  const IR_Type &t = getComputationLevelType( *$1->getExp() );
  if ( t.isIntegralType() )
    $cost[0] += TC13::OperationFormat::DDD_1.getSize();
  else
  if ( t.getType() == IR_Type::FLOAT )
    $cost[0] += FLOAT_COST( TC13::OperationFormat::DDD_1 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_dreg: tpm_AssignExpPLUS( ca_lhs_dreg, dreg )", $1 );

  // Evaluate RHS first to account for side effects and lower register pressure.
  auto p = $action[3]();
  auto wbInfo = $action[2]( true );

  const IR_Type &t = getComputationLevelType( *$1->getExp() );

  // LLIR
  auto *reg = wbInfo.getTempReg();
  if ( t.isIntegralType() )
    TCINSTRUCTIONS.insertADD( reg, reg, p.first, $1->getExp() );
  else
    TCINSTRUCTIONS.insertADD_F( reg, reg, p.first, $1->getExp() );

  // WIR
  auto &r = dynamic_cast<TC_DRegV &>( wbInfo.mTmpReg.get() );
  if ( t.isIntegralType() )
    TCINSTRUCTIONS.insertADD( r, r, p.second, $1->getExp() );
  else
    TCINSTRUCTIONS.insertADD_F( r, r, p.second, $1->getExp() );

  return( wbInfo );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_result_llereg: tpm_AssignExpPLUS( ca_lhs_llereg, llereg )
{
  $cost[0] = $cost[2] + $cost[3] + 2 * TC13::OperationFormat::DDD_1.getSize();
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_llereg: tpm_AssignExpPLUS( ca_lhs_llereg, llereg )", $1 );

  // Evaluate RHS first to account for side effects and lower register pressure.
  auto p = $action[3]();
  auto wbInfo = $action[2]( true );

  // LLIR
  auto *reg = wbInfo.getTempReg();
  TCINSTRUCTIONS.insertADDX(
    getLVLLChild( reg ), getLVLLChild( reg ), getLVLLChild( p.first ),
    $1->getExp() );
  TCINSTRUCTIONS.insertADDC(
    getHVLLChild( reg ), getHVLLChild( reg ), getHVLLChild( p.first ),
    $1->getExp() );

  // WIR
  auto &r = dynamic_cast<TC_ERegV &>( wbInfo.mTmpReg.get() );
  TCINSTRUCTIONS.insertADDX(
    dynamic_cast<TC_DRegV &>( r.begin()->get() ),
    dynamic_cast<TC_DRegV &>( r.begin()->get() ),
    dynamic_cast<TC_DRegV &>( p.second.get().begin()->get() ), $1->getExp() );
  TCINSTRUCTIONS.insertADDC(
    dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( p.second.get().rbegin()->get() ), $1->getExp() );

  return( wbInfo );
};


###############################################################################
#
# Arithmetic Operation MINUS
#
###############################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_result_dreg: tpm_AssignExpMINUS( ca_lhs_dreg, ereg )
{
  $cost[0] =
    $cost[2] + $cost[3] + 6 * TC13::OperationFormat::SDD_1.getSize() +
    TC13::OperationFormat::L.getSize();
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_dreg: tpm_AssignExpMINUS( ca_lhs_dreg, ereg )", $1 );

  // Evaluate RHS first to account for side effects and lower register pressure.
  auto p = $action[3]();
  auto wbInfo = $action[2]( true );

  // LLIR
  auto *reg = wbInfo.getTempReg();
  TCINSTRUCTIONS.insertSUB_D( reg, reg, p.first, $1->getExp() );

  // WIR
  auto &r = dynamic_cast<TC_ERegV &>( wbInfo.mTmpReg.get() );
  TCINSTRUCTIONS.insertSUB_D( r, r, p.second, $1->getExp() );

  return( wbInfo );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_result_ereg: tpm_AssignExpMINUS( ca_lhs_ereg, ereg )
{
  $cost[0] =
    $cost[2] + $cost[3] + 6 * TC13::OperationFormat::SDD_1.getSize() +
    TC13::OperationFormat::L.getSize();
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_ereg: tpm_AssignExpMINUS( ca_lhs_ereg, ereg )", $1 );

  // Evaluate RHS first to account for side effects and lower register pressure.
  auto p = $action[3]();
  auto wbInfo = $action[2]( true );

  // LLIR
  auto *reg = wbInfo.getTempReg();
  TCINSTRUCTIONS.insertSUB_D( reg, reg, p.first, $1->getExp() );

  // WIR
  auto &r = dynamic_cast<TC_ERegV &>( wbInfo.mTmpReg.get() );
  TCINSTRUCTIONS.insertSUB_D( r, r, p.second, $1->getExp() );

  return( wbInfo );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_result_dreg: tpm_AssignExpMINUS( ca_lhs_dreg, dreg )
{
  $cost[0] = $cost[2] + $cost[3];

  const IR_Type &t = getComputationLevelType( *$1->getExp() );
  if ( t.isIntegralType() )
    $cost[0] += TC13::OperationFormat::DDD_1.getSize();
  else
  if ( t.getType() == IR_Type::FLOAT )
    $cost[0] += FLOAT_COST( TC13::OperationFormat::DDD_1 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_dreg: tpm_AssignExpMINUS( ca_lhs_dreg, dreg )", $1 );

  // Evaluate RHS first to account for side effects and lower register pressure.
  auto p = $action[3]();
  auto wbInfo = $action[2]( true );

  const IR_Type &t = getComputationLevelType( *$1->getExp() );

  // LLIR
  auto *reg = wbInfo.getTempReg();
  if ( t.isIntegralType() )
    TCINSTRUCTIONS.insertSUB( reg, reg, p.first, $1->getExp() );
  else
    TCINSTRUCTIONS.insertSUB_F( reg, reg, p.first, $1->getExp() );

  // WIR
  auto &r = dynamic_cast<TC_DRegV &>( wbInfo.mTmpReg.get() );
  if ( t.isIntegralType() )
    TCINSTRUCTIONS.insertSUB( r, r, p.second, $1->getExp() );
  else
    TCINSTRUCTIONS.insertSUB_F( r, r, p.second, $1->getExp() );

  return( wbInfo );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_result_llereg: tpm_AssignExpMINUS( ca_lhs_llereg, llereg )
{
  $cost[0] = $cost[2] + $cost[3] + 2 * TC13::OperationFormat::DDD_1.getSize();
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_llereg: tpm_AssignExpMINUS( ca_lhs_llereg, llereg )", $1 );

  // Evaluate RHS first to account for side effects and lower register pressure.
  auto p = $action[3]();
  auto wbInfo = $action[2]( true );

  // LLIR
  auto *reg = wbInfo.getTempReg();
  TCINSTRUCTIONS.insertSUBX(
    getLVLLChild( reg ), getLVLLChild( reg ), getLVLLChild( p.first ),
    $1->getExp() );
  TCINSTRUCTIONS.insertSUBC(
    getHVLLChild( reg ), getHVLLChild( reg ), getHVLLChild( p.first ),
    $1->getExp() );

  // WIR
  auto &r = dynamic_cast<TC_ERegV &>( wbInfo.mTmpReg.get() );
  TCINSTRUCTIONS.insertSUBX(
    dynamic_cast<TC_DRegV &>( r.begin()->get() ),
    dynamic_cast<TC_DRegV &>( r.begin()->get() ),
    dynamic_cast<TC_DRegV &>( p.second.get().begin()->get() ), $1->getExp() );
  TCINSTRUCTIONS.insertSUBC(
    dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( p.second.get().rbegin()->get() ), $1->getExp() );

  return( wbInfo );
};


###############################################################################
#
# Arithmetic Operation MULT
#
###############################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_result_dreg: tpm_AssignExpMULT( ca_lhs_dreg, const9 )
{
  if ( effectiveType( *$2->getExp() ).isIntegralType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_dreg: tpm_AssignExpMULT( ca_lhs_dreg, const9 )", $1 );

  auto wbInfo = $action[2]( true );
  auto v = $action[3]().getIntValue();

  // LLIR
  auto *reg = wbInfo.getTempReg();
  TCINSTRUCTIONS.insertMUL( reg, reg, v, $1->getExp() );

  // WIR
  auto &r = dynamic_cast<TC_DRegV &>( wbInfo.mTmpReg.get() );
  TCINSTRUCTIONS.insertMUL( r, r, v, $1->getExp() );

  return( wbInfo );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_result_dreg: tpm_AssignExpMULT( ca_lhs_dreg, powerOfTwo )
{
  if ( effectiveType( *$2->getExp() ).isIntegralType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_dreg: tpm_AssignExpMULT( ca_lhs_dreg, powerOfTwo )", $1 );

  auto wbInfo = $action[2]( true );
  const int power = $action[3]().getIntValue();

  // LLIR
  auto *reg = wbInfo.getTempReg();
  TCINSTRUCTIONS.insertSHA( reg, reg, power, $1->getExp() );

  // WIR
  auto &r = dynamic_cast<TC_DRegV &>( wbInfo.mTmpReg.get() );
  TCINSTRUCTIONS.insertSHA( r, r, power, $1->getExp() );

  return( wbInfo );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_result_dreg: tpm_AssignExpMULT( ca_lhs_dreg, negPowerOfTwo )
{
  if ( effectiveType( *$2->getExp() ).isIntegralType() )
    $cost[0] =
      $cost[2] + $cost[3] + 2 * TC13::OperationFormat::DDC9_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_dreg: tpm_AssignExpMULT( ca_lhs_dreg, negPowerOfTwo )", $1 );

  auto wbInfo = $action[2]( true );
  const int power = $action[3]().getIntValue();

  // LLIR
  auto *reg = wbInfo.getTempReg();
  TCINSTRUCTIONS.insertSHA( reg, reg, power, $1->getExp() );
  TCINSTRUCTIONS.insertRSUB( reg, reg, 0, $1->getExp() );

  // WIR
  auto &r = dynamic_cast<TC_DRegV &>( wbInfo.mTmpReg.get() );
  TCINSTRUCTIONS.insertSHA( r, r, power, $1->getExp() );
  TCINSTRUCTIONS.insertRSUB( r, r, 0, $1->getExp() );

  return( wbInfo );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_result_ereg: tpm_AssignExpMULT( ca_lhs_ereg, ereg )
{
  $cost[0] =
    $cost[2] + $cost[3] + 6 * TC13::OperationFormat::SDD_1.getSize() +
    TC13::OperationFormat::L.getSize();
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_ereg: tpm_AssignExpMULT( ca_lhs_ereg, ereg )", $1 );

  // Evaluate RHS first to account for side effects and lower register pressure.
  auto p = $action[3]();
  auto wbInfo = $action[2]( true );

  // LLIR
  auto *reg = wbInfo.getTempReg();
  TCINSTRUCTIONS.insertMUL_D( reg, reg, p.first, $1->getExp() );

  // WIR
  auto &r = dynamic_cast<TC_ERegV &>( wbInfo.mTmpReg.get() );
  TCINSTRUCTIONS.insertMUL_D( r, r, p.second, $1->getExp() );

  return( wbInfo );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_result_dreg: tpm_AssignExpMULT( ca_lhs_dreg, dreg )
{
  $cost[0] = $cost[2] + $cost[3];

  const IR_Type &t = getComputationLevelType( *$1->getExp() );
  if ( t.isIntegralType() )
    $cost[0] += TC13::OperationFormat::DDD_1.getSize();
  else
  if ( t.getType() == IR_Type::FLOAT )
    $cost[0] += FLOAT_COST( TC13::OperationFormat::DDD_1 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_dreg: tpm_AssignExpMULT( ca_lhs_dreg, dreg )", $1 );

  // Evaluate RHS first to account for side effects and lower register pressure.
  auto p = $action[3]();
  auto wbInfo = $action[2]( true );

  const IR_Type &t = getComputationLevelType( *$1->getExp() );

  // LLIR
  auto *reg = wbInfo.getTempReg();
  if ( t.isIntegralType() )
    TCINSTRUCTIONS.insertMUL( reg, reg, p.first, $1->getExp() );
  else
    TCINSTRUCTIONS.insertMUL_F( reg, reg, p.first, $1->getExp() );

  // WIR
  auto &r = dynamic_cast<TC_DRegV &>( wbInfo.mTmpReg.get() );
  if ( t.isIntegralType() )
    TCINSTRUCTIONS.insertMUL( r, r, p.second, $1->getExp() );
  else
    TCINSTRUCTIONS.insertMUL_F( r, r, p.second, $1->getExp() );

  return( wbInfo );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_result_llereg: tpm_AssignExpMULT( ca_lhs_llereg, llereg )
{
  $cost[0] =
    $cost[2] + $cost[3] + TC13::OperationFormat::EDD.getSize() +
    2 * TC13::OperationFormat::DDDD.getSize() +
    2 * TC13::OperationFormat::SDD_1.getSize();
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_llereg: tpm_AssignExpMULT( ca_lhs_llereg, llereg )", $1 );

  // Evaluate RHS first to account for side effects and lower register pressure.
  auto p = $action[3]();
  auto wbInfo = $action[2]( true );

  // LLIR
  auto *reg = wbInfo.getTempReg();
  auto *regTmp = TCINSTRUCTIONS.CreateERegister( "" );

  TCINSTRUCTIONS.insertMUL_U(
    regTmp, getLVLLChild( reg ), getLVLLChild( p.first ), $1->getExp() );
  TCINSTRUCTIONS.insertMADD(
    getHVLLChild( regTmp ), getHVLLChild( regTmp ), getLVLLChild( reg ),
    getHVLLChild( p.first ), $1->getExp() );
  TCINSTRUCTIONS.insertMADD(
    getHVLLChild( regTmp ), getHVLLChild( regTmp ), getHVLLChild( reg ),
    getLVLLChild( p.first ), $1->getExp() );
  TCINSTRUCTIONS.insertMOV( reg, regTmp, $1->getExp() );

  // WIR
  auto &r = dynamic_cast<TC_ERegV &>( wbInfo.mTmpReg.get() );
  auto &tmpReg = TCINSTRUCTIONS.createEReg();

  TCINSTRUCTIONS.insertMUL_U(
    tmpReg,  dynamic_cast<TC_DRegV &>( r.begin()->get() ),
    dynamic_cast<TC_DRegV &>( p.second.get().begin()->get() ), $1->getExp() );
  TCINSTRUCTIONS.insertMADD(
    dynamic_cast<TC_DRegV &>( tmpReg.rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( tmpReg.rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( r.begin()->get() ),
    dynamic_cast<TC_DRegV &>( p.second.get().rbegin()->get() ), $1->getExp() );
  TCINSTRUCTIONS.insertMADD(
    dynamic_cast<TC_DRegV &>( tmpReg.rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( tmpReg.rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( p.second.get().begin()->get() ), $1->getExp() );
  TCINSTRUCTIONS.insertMOV( r, tmpReg, $1->getExp() );

  return( wbInfo );
};


###############################################################################
#
# Arithmetic Operation DIV
#
###############################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_result_dreg: tpm_AssignExpDIV( ca_lhs_dreg, powerOfTwo )
{
  if ( $2->getExp()->getType().isIntegralType() ) {
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_1.getSize();

    if ( !$2->getExp()->getType().isUnsignedType() )
      $cost[0] += 3 * TC13::OperationFormat::DDC9_1.getSize();
  } else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_dreg: tpm_AssignExpDIV( ca_lhs_dreg, powerOfTwo )", $1 );

  auto wbInfo = $action[2]( true );
  const int power = $action[3]().getIntValue();

  // LLIR
  auto *reg = wbInfo.getTempReg();
  if ( $2->getExp()->getType().isUnsignedType() )
    TCINSTRUCTIONS.insertSH( reg, reg, -power, $1->getExp() );
  else {
    LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );

    TCINSTRUCTIONS.insertSHA( regTmp, reg, -31, $1->getExp() );
    TCINSTRUCTIONS.insertSH( regTmp, regTmp, -(32 - power), $1->getExp() );
    TCINSTRUCTIONS.insertADD( reg, reg, regTmp, $1->getExp() );
    TCINSTRUCTIONS.insertSHA( reg, reg, -power, $1->getExp() );
  }

  // WIR
  auto &r = dynamic_cast<TC_DRegV &>( wbInfo.mTmpReg.get() );
  if ( $2->getExp()->getType().isUnsignedType() )
    TCINSTRUCTIONS.insertSH( r, r, -power, $1->getExp() );
  else {
    auto &tmpReg = TCINSTRUCTIONS.createDReg();

    TCINSTRUCTIONS.insertSHA( tmpReg, r, -31, $1->getExp() );
    TCINSTRUCTIONS.insertSH( tmpReg, tmpReg, -(32 - power), $1->getExp() );
    TCINSTRUCTIONS.insertADD( r, r, tmpReg, $1->getExp() );
    TCINSTRUCTIONS.insertSHA( r, r, -power, $1->getExp() );
  }

  return( wbInfo );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_result_dreg: tpm_AssignExpDIV( ca_lhs_dreg, negPowerOfTwo )
{
  if ( $2->getExp()->getType().isIntegralType() ) {
    $cost[0] = $cost[2] + $cost[3];

    if ( $2->getExp()->getType().isUnsignedType() )
      if ( getComputationLevelType( *$1->getExp() ).isUnsignedType() )
        $cost[0] +=
          TC13::OperationFormat::SDC4_1.getSize() +
          TC13::OperationFormat::DDD_1.getSize();
      else
        $cost[0] += 2 * TC13::OperationFormat::DDC9_1.getSize();
    else
      $cost[0] +=
        4 * TC13::OperationFormat::DDC9_1.getSize() +
        TC13::OperationFormat::DDD_1.getSize();
  } else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_dreg: tpm_AssignExpDIV( ca_lhs_dreg, negPowerOfTwo )", $1 );

  auto wbInfo = $action[2]( true );
  const int power = $action[3]().getIntValue();

  // LLIR
  auto *reg = wbInfo.getTempReg();

  if ( $2->getExp()->getType().isUnsignedType() ) {
    if ( getComputationLevelType( *$1->getExp() ).isUnsignedType() ) {
      LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );

      long long value = -2;
      for ( int i = 1; i < power; i++, value *= 2 ) ;

      if ( value >= -32768 )
        TCINSTRUCTIONS.insertMOV( regTmp, value, $1->getExp() );
      else {
        int low, high;

        low = value & 0x0000FFFF;

        if ( low > maxSignedShortValue )
          low = minSignedShortValue + ( low - maxSignedShortValue ) - 1;

        high = value - low;

        for ( int i = 0; i < 16; i++ )
          high /= 2;

        if ( high < 0 )
          high += maxUnsignedShortValue + 1;

        TCINSTRUCTIONS.insertMOVH( regTmp, high, $1->getExp() );
      }

      TCINSTRUCTIONS.insertGE_U( reg, reg, regTmp, $1->getExp() );
    } else {
      TCINSTRUCTIONS.insertSH( reg, reg, -power, $1->getExp() );
      TCINSTRUCTIONS.insertRSUB( reg, reg, 0, $1->getExp() );
    }
  } else {
    // Emit code to achieve a normalization of signed variables to 0 if the
    // dividend is larger than the divisor.
    LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );

    TCINSTRUCTIONS.insertSHA( regTmp, reg, -31, $1->getExp() );
    TCINSTRUCTIONS.insertSH( regTmp, regTmp, -(32 - power), $1->getExp() );
    TCINSTRUCTIONS.insertADD( reg, reg, regTmp, $1->getExp() );

    TCINSTRUCTIONS.insertSHA( reg, reg, -power, $1->getExp() );
    TCINSTRUCTIONS.insertRSUB( reg, reg, 0, $1->getExp() );
  }

  // WIR
  auto &r = dynamic_cast<TC_DRegV &>( wbInfo.mTmpReg.get() );

  if ( $2->getExp()->getType().isUnsignedType() ) {
    if ( getComputationLevelType( *$1->getExp() ).isUnsignedType() ) {
      auto &tmpReg = TCINSTRUCTIONS.createDReg();

      long long value = -2;
      for ( int i = 1; i < power; i++, value *= 2 ) ;

      if ( value >= -32768 )
        TCINSTRUCTIONS.insertMOV( tmpReg, value, $1->getExp() );
      else {
        int low, high;

        low = value & 0x0000FFFF;

        if ( low > maxSignedShortValue )
          low = minSignedShortValue + ( low - maxSignedShortValue ) - 1;

        high = value - low;

        for ( int i = 0; i < 16; i++ )
          high /= 2;

        if ( high < 0 )
          high += maxUnsignedShortValue + 1;

        TCINSTRUCTIONS.insertMOVH( tmpReg, high, $1->getExp() );
      }

      TCINSTRUCTIONS.insertGE_U( r, r, tmpReg, $1->getExp() );
    } else {
      TCINSTRUCTIONS.insertSH( r, r, -power, $1->getExp() );
      TCINSTRUCTIONS.insertRSUB( r, r, 0, $1->getExp() );
    }
  } else {
    // Emit code to achieve a normalization of signed variables to 0 if the
    // dividend is larger than the divisor.
    auto &tmpReg = TCINSTRUCTIONS.createDReg();

    TCINSTRUCTIONS.insertSHA( tmpReg, r, -31, $1->getExp() );
    TCINSTRUCTIONS.insertSH( tmpReg, tmpReg, -(32 - power), $1->getExp() );
    TCINSTRUCTIONS.insertADD( r, r, tmpReg, $1->getExp() );

    TCINSTRUCTIONS.insertSHA( r, r, -power, $1->getExp() );
    TCINSTRUCTIONS.insertRSUB( r, r, 0, $1->getExp() );
  }

  return( wbInfo );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_result_ereg: tpm_AssignExpDIV( ca_lhs_ereg, ereg )
{
  $cost[0] =
    $cost[2] + $cost[3] + 6 * TC13::OperationFormat::SDD_1.getSize() +
    TC13::OperationFormat::L.getSize();
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_ereg: tpm_AssignExpDIV( ca_lhs_ereg, ereg )", $1 );

  // Evaluate RHS first to account for side effects and lower register pressure.
  auto p = $action[3]();
  auto wbInfo = $action[2]( true );

  // LLIR
  auto *reg = wbInfo.getTempReg();
  TCINSTRUCTIONS.insertDIV_D( reg, reg, p.first, $1->getExp() );

  // WIR
  auto &r = dynamic_cast<TC_ERegV &>( wbInfo.mTmpReg.get() );
  TCINSTRUCTIONS.insertDIV_D( r, r, p.second, $1->getExp() );

  return( wbInfo );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_result_dreg: tpm_AssignExpDIV( ca_lhs_dreg, dreg )
{
  $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::SDD_1.getSize();

  const IR_Type &t = getComputationLevelType( *$1->getExp() );
  if ( t.isIntegralType() ) {
    $cost[0] += TC13::OperationFormat::EDD.getSize();

    switch ( $2->getExp()->getType().getType() ) {

      case IR_Type::UNSIGNED_CHAR:
      case IR_Type::BOOL: {
        $cost[0] += TC13::OperationFormat::EED.getSize();
        break;
      }

      case IR_Type::CHAR:
      case IR_Type::UNSIGNED_SHORT: {
        $cost[0] += 2 * TC13::OperationFormat::EED.getSize();
        break;
      }

      case IR_Type::SHORT: {
        $cost[0] += 3 * TC13::OperationFormat::EED.getSize();
        break;
      }

      case IR_Type::INT:
      case IR_Type::LONG: {
        $cost[0] += 5 * TC13::OperationFormat::EED.getSize();
        break;
      }

      case IR_Type::UNSIGNED_INT:
      case IR_Type::UNSIGNED_LONG: {
        $cost[0] += 4 * TC13::OperationFormat::EED.getSize();
        break;
      }

      default: {
        $cost[0] = COST_INFINITY;
        break;
      }

    }
  } else
  if ( t.getType() == IR_Type::FLOAT )
    $cost[0] += FLOAT_COST( TC13::OperationFormat::DDD_1 );
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_dreg: tpm_AssignExpDIV( ca_lhs_dreg, dreg )", $1 );

  // Evaluate RHS first to account for side effects and lower register pressure.
  auto p = $action[3]();
  auto wbInfo = $action[2]( true );

  const IR_Type &t = getComputationLevelType( *$1->getExp() );

  // LLIR
  auto *reg = wbInfo.getTempReg();
  if ( t.isIntegralType() ) {
    bool isUnsigned = t.isUnsignedType();

    switch ( $2->getExp()->getType().getType() ) {

      case IR_Type::BOOL:
      case IR_Type::CHAR:
      case IR_Type::UNSIGNED_CHAR: {
        TCINSTRUCTIONS.insertDIV_B(
          reg, p.first, reg, isUnsigned, $1->getExp() );
        break;
      }

      case IR_Type::SHORT:
      case IR_Type::UNSIGNED_SHORT: {
        TCINSTRUCTIONS.insertDIV_H(
          reg, p.first, reg, isUnsigned, $1->getExp() );
        break;
      }

      case IR_Type::INT:
      case IR_Type::UNSIGNED_INT:
      case IR_Type::LONG:
      case IR_Type::UNSIGNED_LONG: {
        TCINSTRUCTIONS.insertDIV_W(
          reg, p.first, reg, isUnsigned, $1->getExp() );
        break;
      }

      default:
        break;

    }
  } else
    TCINSTRUCTIONS.insertDIV_F( reg, reg, p.first, $1->getExp() );

  // WIR
  auto &r = dynamic_cast<TC_DRegV &>( wbInfo.mTmpReg.get() );
  if ( t.isIntegralType() ) {
    bool isUnsigned = t.isUnsignedType();

    switch ( $2->getExp()->getType().getType() ) {

      case IR_Type::BOOL:
      case IR_Type::CHAR:
      case IR_Type::UNSIGNED_CHAR: {
        TCINSTRUCTIONS.insertDIV_B( r, r, p.second, isUnsigned, $1->getExp() );
        break;
      }

      case IR_Type::SHORT:
      case IR_Type::UNSIGNED_SHORT: {
        TCINSTRUCTIONS.insertDIV_H( r, r, p.second, isUnsigned, $1->getExp() );
        break;
      }

      case IR_Type::INT:
      case IR_Type::UNSIGNED_INT:
      case IR_Type::LONG:
      case IR_Type::UNSIGNED_LONG: {
        TCINSTRUCTIONS.insertDIV_W( r, r, p.second, isUnsigned, $1->getExp() );
        break;
      }

      default:
        break;

    }
  } else
    TCINSTRUCTIONS.insertDIV_F( r, r, p.second, $1->getExp() );

  return( wbInfo );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_result_llereg: tpm_AssignExpDIV( ca_lhs_llereg, llereg )
{
  $cost[0] =
    $cost[2] + $cost[3] + 6 * TC13::OperationFormat::SDD_1.getSize() +
    TC13::OperationFormat::L.getSize();
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_llereg: tpm_AssignExpDIV( ca_lhs_llereg, llereg )", $1 );

  // Evaluate RHS first to account for side effects and lower register pressure
  auto p = $action[3]();
  auto wbInfo = $action[2]( true );

  // LLIR
  auto *reg = wbInfo.getTempReg();
  if ( $2->getExp()->getType().isUnsignedType() ||
       $3->getExp()->getType().isUnsignedType() )
    TCINSTRUCTIONS.insertDIV_ULL( reg, reg, p.first, $1->getExp() );
  else
    TCINSTRUCTIONS.insertDIV_LL( reg, reg, p.first, $1->getExp() );

  // WIR
  auto &r = dynamic_cast<TC_ERegV &>( wbInfo.mTmpReg.get() );
  if ( $2->getExp()->getType().isUnsignedType() ||
       $3->getExp()->getType().isUnsignedType() )
    TCINSTRUCTIONS.insertDIV_ULL( r, r, p.second, $1->getExp() );
  else
    TCINSTRUCTIONS.insertDIV_LL( r, r, p.second, $1->getExp() );

  return( wbInfo );
};


###############################################################################
#
# Arithmetic Operation MOD
#
###############################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_result_dreg: tpm_AssignExpMOD( ca_lhs_dreg, powerOfTwo )
{
  const IR_Type &t = getComputationLevelType( *$1->getExp() );
  if ( t.isIntegralType() ) {
    $cost[0] =
      $cost[2] + $cost[3] + TC13::OperationFormat::DDC4C5C5.getSize() +
      TC13::OperationFormat::DDD_1.getSize();

    if ( !t.isUnsignedType() )
      $cost[0] +=
        2 * TC13::OperationFormat::DDC9_1.getSize() +
        TC13::OperationFormat::DDD_1.getSize();
  } else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_dreg: tpm_AssignExpMOD( ca_lhs_dreg, powerOfTwo )", $1 );

  auto wbInfo = $action[2]( true );
  const int power = $action[3]().getIntValue();

  const IR_Type &t = getComputationLevelType( *$1->getExp() );

  // LLIR
  auto *reg = wbInfo.getTempReg();
  LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );
  if ( t.isUnsignedType() )
    // This one computes '( reg / powerOfTwo ) * powerOfTwo'.
    TCINSTRUCTIONS.insertINSERT( regTmp, reg, 0, 0, power, $1->getExp() );
  else {
    // Emit code to achieve a normalization of signed variables to 0 if the
    // dividend is larger than the divisor.
    TCINSTRUCTIONS.insertSHA( regTmp, reg, -31, $1->getExp() );
    TCINSTRUCTIONS.insertSH( regTmp, regTmp, -(32 - power), $1->getExp() );
    TCINSTRUCTIONS.insertADD( regTmp, reg, regTmp, $1->getExp() );

    // This one computes '( reg / powerOfTwo ) * powerOfTwo'.
    TCINSTRUCTIONS.insertINSERT( regTmp, regTmp, 0, 0, power, $1->getExp() );
  }

  TCINSTRUCTIONS.insertSUB( reg, reg, regTmp, $1->getExp() );

  // WIR
  auto &r = dynamic_cast<TC_DRegV &>( wbInfo.mTmpReg.get() );
  auto &tmpReg = TCINSTRUCTIONS.createDReg();
  if ( t.isUnsignedType() )
    // This one computes '( r / powerOfTwo ) * powerOfTwo'.
    TCINSTRUCTIONS.insertINSERT( tmpReg, r, 0, 0, power, $1->getExp() );
  else {
    // Emit code to achieve a normalization of signed variables to 0 if the
    // dividend is larger than the divisor.
    TCINSTRUCTIONS.insertSHA( tmpReg, r, -31, $1->getExp() );
    TCINSTRUCTIONS.insertSH( tmpReg, tmpReg, -(32 - power), $1->getExp() );
    TCINSTRUCTIONS.insertADD( tmpReg, r, tmpReg, $1->getExp() );

    // This one computes '( r / powerOfTwo ) * powerOfTwo'.
    TCINSTRUCTIONS.insertINSERT( tmpReg, tmpReg, 0, 0, power, $1->getExp() );
  }

  TCINSTRUCTIONS.insertSUB( r, r, tmpReg, $1->getExp() );

  return( wbInfo );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_result_dreg: tpm_AssignExpMOD( ca_lhs_dreg, negPowerOfTwo )
{
  const IR_Type &t = getComputationLevelType( *$1->getExp() );

  if ( t.isIntegralType() ) {
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DDD_1.getSize();

    if ( $2->getExp()->getType().isUnsignedType() )
      if ( t.isUnsignedType() )
        $cost[0] +=
          TC13::OperationFormat::SDC4_1.getSize() +
          2 * TC13::OperationFormat::DDD_1.getSize();
      else
        $cost[0] += TC13::OperationFormat::DDC4C5C5.getSize();
    else
      $cost[0] +=
        2 * TC13::OperationFormat::DDC9_1.getSize() +
        TC13::OperationFormat::DDD_1.getSize() +
        TC13::OperationFormat::DDC4C5C5.getSize();
  } else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_dreg: tpm_AssignExpMOD( ca_lhs_dreg, negPowerOfTwo )", $1 );

  auto wbInfo = $action[2]( true );
  const int power = $action[3]().getIntValue();

  const IR_Type &t = getComputationLevelType( *$1->getExp() );

  // LLIR
  auto *reg = wbInfo.getTempReg();
  LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );

  if ( $2->getExp()->getType().isUnsignedType() ) {
    if ( t.isUnsignedType() ) {
      LLIR_Register *regTmp1 = TCINSTRUCTIONS.CreateRegister( "" );

      long long value = -2;
      for ( int i = 1; i < power; i++, value *= 2 ) ;

      if ( value >= -32768 )
        TCINSTRUCTIONS.insertMOV( regTmp1, value, $1->getExp() );
      else {
        int low, high;

        low = value & 0x0000FFFF;

        if ( low > maxSignedShortValue )
          low = minSignedShortValue + ( low - maxSignedShortValue ) - 1;

        high = value - low;

        for ( int i = 0; i < 16; i++ )
          high /= 2;

        if ( high < 0 )
          high += maxUnsignedShortValue + 1;

        TCINSTRUCTIONS.insertMOVH( regTmp1, high, $1->getExp() );
      }

      TCINSTRUCTIONS.insertGE_U( regTmp, reg, regTmp1, $1->getExp() );
      TCINSTRUCTIONS.insertMUL( regTmp, regTmp, regTmp1, $1->getExp() );
    } else
      // This one computes '( reg / powerOfTwo ) * powerOfTwo'.
      TCINSTRUCTIONS.insertINSERT( regTmp, reg, 0, 0, power, $1->getExp() );
  } else {
    // Emit code to achieve a normalization of signed variables to 0 if
    // the dividend is larger than the divisor.
    TCINSTRUCTIONS.insertSHA( regTmp, reg, -31, $1->getExp() );
    TCINSTRUCTIONS.insertSH( regTmp, regTmp, -(32 - power ), $1->getExp() );
    TCINSTRUCTIONS.insertADD( regTmp, reg, regTmp, $1->getExp() );

    // This one computes '( reg / powerOfTwo ) * powerOfTwo'.
    TCINSTRUCTIONS.insertINSERT( regTmp, regTmp, 0, 0, power, $1->getExp() );
  }

  TCINSTRUCTIONS.insertSUB( reg, reg, regTmp, $1->getExp() );

  // WIR
  auto &r = dynamic_cast<TC_DRegV &>( wbInfo.mTmpReg.get() );
  auto &tmpReg = TCINSTRUCTIONS.createDReg();

  if ( $2->getExp()->getType().isUnsignedType() ) {
    if ( t.isUnsignedType() ) {
      auto &tmpReg1 = TCINSTRUCTIONS.createDReg();

      long long value = -2;
      for ( int i = 1; i < power; i++, value *= 2 ) ;

      if ( value >= -32768 )
        TCINSTRUCTIONS.insertMOV( tmpReg1, value, $1->getExp() );
      else {
        int low, high;

        low = value & 0x0000FFFF;

        if ( low > maxSignedShortValue )
          low = minSignedShortValue + ( low - maxSignedShortValue ) - 1;

        high = value - low;

        for ( int i = 0; i < 16; i++ )
          high /= 2;

        if ( high < 0 )
          high += maxUnsignedShortValue + 1;

        TCINSTRUCTIONS.insertMOVH( tmpReg1, high, $1->getExp() );
      }

      TCINSTRUCTIONS.insertGE_U( tmpReg, r, tmpReg1, $1->getExp() );
      TCINSTRUCTIONS.insertMUL( tmpReg, tmpReg, tmpReg1, $1->getExp() );
    } else
      // This one computes '( r / powerOfTwo ) * powerOfTwo'.
      TCINSTRUCTIONS.insertINSERT( tmpReg, r, 0, 0, power, $1->getExp() );
  } else {
    // Emit code to achieve a normalization of signed variables to 0 if
    // the dividend is larger than the divisor.
    TCINSTRUCTIONS.insertSHA( tmpReg, r, -31, $1->getExp() );
    TCINSTRUCTIONS.insertSH( tmpReg, tmpReg, -(32 - power ), $1->getExp() );
    TCINSTRUCTIONS.insertADD( tmpReg, r, tmpReg, $1->getExp() );

    // This one computes '( r / powerOfTwo ) * powerOfTwo'.
    TCINSTRUCTIONS.insertINSERT( tmpReg, tmpReg, 0, 0, power, $1->getExp() );
  }

  TCINSTRUCTIONS.insertSUB( r, r, tmpReg, $1->getExp() );

  return( wbInfo );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_result_dreg: tpm_AssignExpMOD( ca_lhs_dreg, dreg )
{
  $cost[0] =
    $cost[2] + $cost[3] + TC13::OperationFormat::SDD_1.getSize() *
    TC13::OperationFormat::EDD.getSize();

  const IR_Type &t = getComputationLevelType( *$1->getExp() );
  switch ( t.getType() ) {
    case IR_Type::CHAR:
    case IR_Type::UNSIGNED_SHORT: {
      $cost[0] += 2 * TC13::OperationFormat::EED.getSize();
      break;
    }

    case IR_Type::UNSIGNED_CHAR:
    case IR_Type::BOOL: {
      $cost[0] += TC13::OperationFormat::EED.getSize();
      break;
    }

    case IR_Type::SHORT: {
      $cost[0] += 3 * TC13::OperationFormat::EED.getSize();
      break;
    }

    default: {
      if ( t.isUnsignedType() )
        $cost[0] += 4 * TC13::OperationFormat::EED.getSize();
      else
        $cost[0] += 5 * TC13::OperationFormat::EED.getSize();
      break;
    }
  }
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_dreg: tpm_AssignExpMOD( ca_lhs_dreg, dreg )", $1 );

  // Evaluate RHS first to account for side effects and lower register pressure.
  auto p = $action[3]();
  auto wbInfo = $action[2]( true );

  const IR_Type &t = getComputationLevelType( *$1->getExp() );
  const bool isUnsigned = t.isUnsignedType();

  // LLIR
  auto *reg = wbInfo.getTempReg();
  switch ( t.getType() ) {
    case IR_Type::BOOL:
    case IR_Type::CHAR:
    case IR_Type::UNSIGNED_CHAR: {
      TCINSTRUCTIONS.insertMOD_B( reg, p.first, reg, isUnsigned, $1->getExp() );
      break;
    }

    case IR_Type::SHORT:
    case IR_Type::UNSIGNED_SHORT: {
      TCINSTRUCTIONS.insertMOD_H( reg, p.first, reg, isUnsigned, $1->getExp() );
      break;
    }

    default: {
      TCINSTRUCTIONS.insertMOD_W( reg, p.first, reg, isUnsigned, $1->getExp() );
      break;
    }
  }

  // WIR
  auto &r = dynamic_cast<TC_DRegV &>( wbInfo.mTmpReg.get() );
  switch ( t.getType() ) {
    case IR_Type::BOOL:
    case IR_Type::CHAR:
    case IR_Type::UNSIGNED_CHAR: {
      TCINSTRUCTIONS.insertMOD_B( r, r, p.second, isUnsigned, $1->getExp() );
      break;
    }

    case IR_Type::SHORT:
    case IR_Type::UNSIGNED_SHORT: {
      TCINSTRUCTIONS.insertMOD_H( r, r, p.second, isUnsigned, $1->getExp() );
      break;
    }

    default: {
      TCINSTRUCTIONS.insertMOD_W( r, r, p.second, isUnsigned, $1->getExp() );
      break;
    }
  }

  return( wbInfo );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_result_llereg: tpm_AssignExpMOD( ca_lhs_llereg, llereg )
{
  $cost[0] =
    $cost[2] + $cost[3] + 6 * TC13::OperationFormat::SDD_1.getSize() +
    TC13::OperationFormat::L.getSize();
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_llereg: tpm_AssignExpMOD( ca_lhs_llereg, llereg )", $1 );

  // Evaluate RHS first to account for side effects and lower register pressure.
  auto p = $action[3]();
  auto wbInfo = $action[2]( true );

  // LLIR
  auto *reg = wbInfo.getTempReg();
  if ( $2->getExp()->getType().isUnsignedType() ||
       $3->getExp()->getType().isUnsignedType() )
    TCINSTRUCTIONS.insertMOD_ULL( reg, reg, p.first, $1->getExp() );
  else
    TCINSTRUCTIONS.insertMOD_LL( reg, reg, p.first, $1->getExp() );

  // WIR
  auto &r = dynamic_cast<TC_ERegV &>( wbInfo.mTmpReg.get() );
  if ( $2->getExp()->getType().isUnsignedType() ||
       $3->getExp()->getType().isUnsignedType() )
    TCINSTRUCTIONS.insertMOD_ULL( r, r, p.second, $1->getExp() );
  else
    TCINSTRUCTIONS.insertMOD_LL( r, r, p.second, $1->getExp() );

  return( wbInfo );
};


###############################################################################
#
# Bitwise logical operators
#
###############################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_result_dreg: tpm_AssignExpAND( ca_lhs_dreg, uconst9 )
{
  $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_2.getSize();
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_dreg: tpm_AssignExpAND( ca_lhs_dreg, uconst9 )", $1 );

  auto wbInfo = $action[2]( true );
  auto v = $action[3]().getIntValue();

  // LLIR
  auto *reg = wbInfo.getTempReg();
  TCINSTRUCTIONS.insertAND( reg, reg, v, $1->getExp() );

  // WIR
  auto &r = dynamic_cast<TC_DRegV &>( wbInfo.mTmpReg.get() );
  TCINSTRUCTIONS.insertAND( r, r, v, $1->getExp() );

  return( wbInfo );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_result_dreg: tpm_AssignExpAND( ca_lhs_dreg, dreg )
{
  $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DDD_1.getSize();
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_dreg: tpm_AssignExpAND( ca_lhs_dreg, dreg )", $1 );

  // Evaluate RHS first to account for side effects and lower register pressure.
  auto p = $action[3]();
  auto wbInfo = $action[2]( true );

  // LLIR
  auto *reg = wbInfo.getTempReg();
  TCINSTRUCTIONS.insertAND( reg, reg, p.first, $1->getExp() );

  // WIR
  auto &r = dynamic_cast<TC_DRegV &>( wbInfo.mTmpReg.get() );
  TCINSTRUCTIONS.insertAND( r, r, p.second, $1->getExp() );

  return( wbInfo );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_result_dreg: tpm_AssignExpOR( ca_lhs_dreg, uconst9 )
{
  $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_2.getSize();
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_dreg: tpm_AssignExpOR( ca_lhs_dreg, uconst9 )", $1 );

  auto wbInfo = $action[2]( true );
  auto v = $action[3]().getIntValue();

  // LLIR
  auto *reg = wbInfo.getTempReg();
  TCINSTRUCTIONS.insertOR( reg, reg, v, $1->getExp() );

  // WIR
  auto &r = dynamic_cast<TC_DRegV &>( wbInfo.mTmpReg.get() );
  TCINSTRUCTIONS.insertOR( r, r, v, $1->getExp() );

  return( wbInfo );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_result_dreg: tpm_AssignExpOR( ca_lhs_dreg, dreg )
{
  $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DDD_1.getSize();
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_dreg: tpm_AssignExpOR( ca_lhs_dreg, dreg )", $1 );

  // Evaluate RHS first to account for side effects and lower register pressure.
  auto p = $action[3]();
  auto wbInfo = $action[2]( true );

  // LLIR
  auto *reg = wbInfo.getTempReg();
  TCINSTRUCTIONS.insertOR( reg, reg, p.first, $1->getExp() );

  // WIR
  auto &r = dynamic_cast<TC_DRegV &>( wbInfo.mTmpReg.get() );
  TCINSTRUCTIONS.insertOR( r, r, p.second, $1->getExp() );

  return( wbInfo );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_result_dreg: tpm_AssignExpXOR( ca_lhs_dreg, uconst9 )
{
  $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_2.getSize();
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_dreg: tpm_AssignExpXOR( ca_lhs_dreg, uconst9 )", $1 );

  auto wbInfo = $action[2]( true );
  auto v = $action[3]().getIntValue();

  // LLIR
  auto *reg = wbInfo.getTempReg();
  TCINSTRUCTIONS.insertXOR( reg, reg, v, $1->getExp() );

  // WIR
  auto &r = dynamic_cast<TC_DRegV &>( wbInfo.mTmpReg.get() );
  TCINSTRUCTIONS.insertXOR( r, r, v, $1->getExp() );

  return( wbInfo );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_result_dreg: tpm_AssignExpXOR( ca_lhs_dreg, dreg )
{
  $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DDD_1.getSize();
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_dreg: tpm_AssignExpXOR( ca_lhs_dreg, dreg )", $1 );

  // Evaluate RHS first to account for side effects and lower register pressure.
  auto p = $action[3]();
  auto wbInfo = $action[2]( true );

  // LLIR
  auto *reg = wbInfo.getTempReg();
  TCINSTRUCTIONS.insertXOR( reg, reg, p.first, $1->getExp() );

  // WIR
  auto &r = dynamic_cast<TC_DRegV &>( wbInfo.mTmpReg.get() );
  TCINSTRUCTIONS.insertXOR( r, r, p.second, $1->getExp() );

  return( wbInfo );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_result_llereg: tpm_AssignExpAND( ca_lhs_llereg, llereg )
{
  $cost[0] = $cost[2] + $cost[3] + 2 * TC13::OperationFormat::DDD_1.getSize();
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_llereg: tpm_AssignExpAND( ca_lhs_llereg, llereg )", $1 );

  // Evaluate RHS first to account for side effects and lower register pressure.
  auto p = $action[3]();
  auto wbInfo = $action[2]( true );

  // LLIR
  auto *reg = wbInfo.getTempReg();
  TCINSTRUCTIONS.insertAND(
    getLVLLChild( reg ), getLVLLChild( reg ), getLVLLChild( p.first ),
    $1->getExp() );
  TCINSTRUCTIONS.insertAND(
    getHVLLChild( reg ), getHVLLChild( reg ), getHVLLChild( p.first ),
    $1->getExp() );

  // WIR
  auto &r = dynamic_cast<TC_ERegV &>( wbInfo.mTmpReg.get() );
  TCINSTRUCTIONS.insertAND(
    dynamic_cast<TC_DRegV &>( r.begin()->get() ),
    dynamic_cast<TC_DRegV &>( r.begin()->get() ),
    dynamic_cast<TC_DRegV &>( p.second.get().begin()->get() ), $1->getExp() );
  TCINSTRUCTIONS.insertAND(
    dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( p.second.get().rbegin()->get() ), $1->getExp() );

  return( wbInfo );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_result_llereg: tpm_AssignExpOR( ca_lhs_llereg, llereg )
{
  $cost[0] = $cost[2] + $cost[3] + 2 * TC13::OperationFormat::DDD_1.getSize();
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_llereg: tpm_AssignExpOR( ca_lhs_llereg, llereg )", $1 );

  // Evaluate RHS first to account for side effects and lower register pressure.
  auto p = $action[3]();
  auto wbInfo = $action[2]( true );

  // LLIR
  auto *reg = wbInfo.getTempReg();
  TCINSTRUCTIONS.insertOR(
    getLVLLChild( reg  ), getLVLLChild( reg ), getLVLLChild( p.first ),
    $1->getExp() );
  TCINSTRUCTIONS.insertOR(
    getHVLLChild( reg ), getHVLLChild( reg ), getHVLLChild( p.first ),
    $1->getExp() );

  // WIR
  auto &r = dynamic_cast<TC_ERegV &>( wbInfo.mTmpReg.get() );
  TCINSTRUCTIONS.insertOR(
    dynamic_cast<TC_DRegV &>( r.begin()->get() ),
    dynamic_cast<TC_DRegV &>( r.begin()->get() ),
    dynamic_cast<TC_DRegV &>( p.second.get().begin()->get() ), $1->getExp() );
  TCINSTRUCTIONS.insertOR(
    dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( p.second.get().rbegin()->get() ), $1->getExp() );

  return( wbInfo );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_result_llereg: tpm_AssignExpXOR( ca_lhs_llereg, llereg )
{
  $cost[0] = $cost[2] + $cost[3] + 2 * TC13::OperationFormat::DDD_1.getSize();
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_llereg: tpm_AssignExpXOR( ca_lhs_llereg, llereg )", $1 );

  // Evaluate RHS first to account for side effects and lower register pressure.
  auto p = $action[3]();
  auto wbInfo = $action[2]( true );

  // LLIR
  auto *reg = wbInfo.getTempReg();
  TCINSTRUCTIONS.insertXOR(
    getLVLLChild( reg ), getLVLLChild( reg ), getLVLLChild( p.first ),
    $1->getExp() );
  TCINSTRUCTIONS.insertXOR(
    getHVLLChild( reg ), getHVLLChild( reg ), getHVLLChild( p.first ),
    $1->getExp() );

  // WIR
  auto &r = dynamic_cast<TC_ERegV &>( wbInfo.mTmpReg.get() );
  TCINSTRUCTIONS.insertXOR(
    dynamic_cast<TC_DRegV &>( r.begin()->get() ),
    dynamic_cast<TC_DRegV &>( r.begin()->get() ),
    dynamic_cast<TC_DRegV &>( p.second.get().begin()->get() ), $1->getExp() );
  TCINSTRUCTIONS.insertXOR(
    dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( p.second.get().rbegin()->get() ), $1->getExp() );

  return( wbInfo );
};


###############################################################################
#
# Shift operators
#
###############################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_result_dreg: tpm_AssignExpSHL( ca_lhs_dreg, const9 )
{
  IR_Type &r = effectiveType( *$2->getExp() );

  if ( r.isIntegralType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_dreg: tpm_AssignExpSHL( ca_lhs_dreg, const9 )", $1 );

  auto wbInfo = $action[2]( true );
  auto v = $action[3]().getIntValue();

  // LLIR
  auto *reg = wbInfo.getTempReg();
  if ( TCIR_CONFIGURATION->arithmeticSHR )
    TCINSTRUCTIONS.insertSHA( reg, reg, v, $1->getExp() );
  else
    TCINSTRUCTIONS.insertSH( reg, reg, v, $1->getExp() );

  // WIR
  auto &r = dynamic_cast<TC_DRegV &>( wbInfo.mTmpReg.get() );
  if ( TCIR_CONFIGURATION->arithmeticSHR )
    TCINSTRUCTIONS.insertSHA( r, r, v, $1->getExp() );
  else
    TCINSTRUCTIONS.insertSH( r, r, v, $1->getExp() );

  return( wbInfo );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_result_dreg: tpm_AssignExpSHL( ca_lhs_dreg, dreg )
{
  if ( $2->getExp()->getType().isIntegralType() )
    $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DDD_1.getSize();
  else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_dreg: tpm_AssignExpSHL( ca_lhs_dreg, dreg )", $1 );

  // Evaluate RHS first to account for side effects and lower register pressure.
  auto p = $action[3]();
  auto wbInfo = $action[2]( true );

  // LLIR
  auto *reg = wbInfo.getTempReg();
  if ( TCIR_CONFIGURATION->arithmeticSHR )
    TCINSTRUCTIONS.insertSHA( reg, reg, p.first, $1->getExp() );
  else
    TCINSTRUCTIONS.insertSH( reg, reg, p.first, $1->getExp() );

  // WIR
  auto &r = dynamic_cast<TC_DRegV &>( wbInfo.mTmpReg.get() );
  if ( TCIR_CONFIGURATION->arithmeticSHR )
    TCINSTRUCTIONS.insertSHA( r, r, p.second, $1->getExp() );
  else
    TCINSTRUCTIONS.insertSH( r, r, p.second, $1->getExp() );

  return( wbInfo );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_result_dreg: tpm_AssignExpSHR( ca_lhs_dreg, const9 )
{
  $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_1.getSize();
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_dreg: tpm_AssignExpSHR( ca_lhs_dreg, const9 )", $1 );

  auto wbInfo = $action[2]( true );
  auto v = $action[3]().getIntValue();

  auto t = effectiveType( *$2->getExp() ).getType();

  // LLIR
  auto *reg = wbInfo.getTempReg();
  if ( TCIR_CONFIGURATION->arithmeticSHR )
    if ( ( t == IR_Type::UNSIGNED_INT ) || ( t == IR_Type::UNSIGNED_LONG ) )
      TCINSTRUCTIONS.insertSH( reg, reg, -v, $1->getExp() );
    else
      TCINSTRUCTIONS.insertSHA( reg, reg, -v, $1->getExp() );
  else
    TCINSTRUCTIONS.insertSH( reg, reg, -v, $1->getExp() );

  // WIR
  auto &r = dynamic_cast<TC_DRegV &>( wbInfo.mTmpReg.get() );
  if ( TCIR_CONFIGURATION->arithmeticSHR )
    if ( ( t == IR_Type::UNSIGNED_INT ) || ( t == IR_Type::UNSIGNED_LONG ) )
      TCINSTRUCTIONS.insertSH( r, r, -v, $1->getExp() );
    else
      TCINSTRUCTIONS.insertSHA( r, r, -v, $1->getExp() );
  else
    TCINSTRUCTIONS.insertSH( r, r, -v, $1->getExp() );

  return( wbInfo );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_result_dreg: tpm_AssignExpSHR( ca_lhs_dreg, dreg )
{
  $cost[0] =
    $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_1.getSize() +
    TC13::OperationFormat::DDD_1.getSize();
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_dreg: tpm_AssignExpSHR( ca_lhs_dreg, dreg )", $1 );

  // Evaluate RHS first to account for side effects and lower register pressure.
  auto p = $action[3]();
  auto wbInfo = $action[2]( true );

  auto t = effectiveType( *$2->getExp() ).getType();

  // LLIR
  auto *reg = wbInfo.getTempReg();
  LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );

  // Generate RSUB instruction in order to negate the shift count.
  TCINSTRUCTIONS.insertRSUB( regTmp, p.first, 0, $1->getExp() );

  if ( TCIR_CONFIGURATION->arithmeticSHR )
    if ( ( t == IR_Type::UNSIGNED_INT ) || ( t == IR_Type::UNSIGNED_LONG ) )
      TCINSTRUCTIONS.insertSH( reg, reg, regTmp, $1->getExp() );
    else
      TCINSTRUCTIONS.insertSHA( reg, reg, regTmp, $1->getExp() );
  else
    TCINSTRUCTIONS.insertSH( reg, reg, regTmp, $1->getExp() );

  // WIR
  auto &r = dynamic_cast<TC_DRegV &>( wbInfo.mTmpReg.get() );
  auto &tmpReg = TCINSTRUCTIONS.createDReg();

  // Generate RSUB instruction in order to negate the shift count.
  TCINSTRUCTIONS.insertRSUB( tmpReg, p.second, 0, $1->getExp() );

  if ( TCIR_CONFIGURATION->arithmeticSHR )
    if ( ( t == IR_Type::UNSIGNED_INT ) || ( t == IR_Type::UNSIGNED_LONG ) )
      TCINSTRUCTIONS.insertSH( r, r, tmpReg, $1->getExp() );
    else
      TCINSTRUCTIONS.insertSHA( r, r, tmpReg, $1->getExp() );
  else
    TCINSTRUCTIONS.insertSH( r, r, tmpReg, $1->getExp() );

  return( wbInfo );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_result_llereg: tpm_AssignExpSHL( ca_lhs_llereg, llereg )
{
  $cost[0] =
    $cost[2] + $cost[3] + TC13::OperationFormat::DC5L.getSize() +
    TC13::OperationFormat::SDD_1.getSize() +
    TC13::OperationFormat::SDC4_1.getSize() +
    2 * TC13::OperationFormat::DDDD.getSize() +
    TC13::OperationFormat::L.getSize() + TC13::OperationFormat::DDD_1.getSize();
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_llereg: tpm_AssignExpSHL( ca_lhs_llereg, llereg )", $1 );

  bool useSHA =
    TCIR_CONFIGURATION->arithmeticSHR &&
    !$2->getExp()->getType().isUnsignedType();

  // Evaluate RHS first to account for side effects and lower register pressure.
  auto p = $action[3]();
  auto wbInfo = $action[2]( true );

  // LLIR
  const string lt32shamt = LLIR::getUniqueLabel();
  const string gt31shamt = LLIR::getUniqueLabel();
  const string endBlock = LLIR::getUniqueLabel();

  auto *reg = wbInfo.getTempReg();
  TCINSTRUCTIONS.insertJZ_T(
    getLVLLChild( p.first ), 5, lt32shamt, $1->getExp() );

  // Create block for shifting by more than 31 bits.
  beginNewLLIRBasicBlock( gt31shamt.c_str() );
  TCINSTRUCTIONS.insertMOV(
    getHVLLChild( reg ), getLVLLChild( reg ), $1->getExp() );
  TCINSTRUCTIONS.insertMOV( getLVLLChild( reg ), 0, $1->getExp() );
  TCINSTRUCTIONS.insertDEXTR(
    getHVLLChild( reg ), getHVLLChild( reg ), getLVLLChild( reg ),
    getLVLLChild( p.first ), $1->getExp() );
  TCINSTRUCTIONS.insertJ( endBlock.c_str(), $1->getExp() );

  // Create block for shifting by less than 32 bits.
  beginNewLLIRBasicBlock( lt32shamt.c_str() );
  TCINSTRUCTIONS.insertDEXTR(
    getHVLLChild( reg ), getHVLLChild( reg ), getLVLLChild( reg ),
    getLVLLChild( p.first ), $1->getExp() );
  if ( useSHA )
    TCINSTRUCTIONS.insertSHA(
      getLVLLChild( reg ), getLVLLChild( reg ), getLVLLChild( p.first ),
      $1->getExp() );
  else
    TCINSTRUCTIONS.insertSH(
      getLVLLChild( reg ), getLVLLChild( reg ), getLVLLChild( p.first ),
      $1->getExp() );

  // Create end block.
  beginNewLLIRBasicBlock( endBlock.c_str() );

  // WIR
  // Create basic block for shifts by more than 31 bits.
  auto *currentBB = TC179x_wirBB;
  auto &b1 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  // Create basic block for shifts by less than 32 bits.
  auto &b2 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  // Create end basic block.
  auto &b3 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  TC179x_wirBB = currentBB;

  auto &r = dynamic_cast<TC_ERegV &>( wbInfo.mTmpReg.get() );
  TCINSTRUCTIONS.insertJZ_T(
    dynamic_cast<TC_DRegV &>( p.second.get().begin()->get() ), 5, b2,
    $1->getExp() );

  // Generate code for shifts by more than 31 bits.
  TC179x_wirBB = &b1;
  TCINSTRUCTIONS.insertMOV(
    dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( r.begin()->get() ), $1->getExp() );
  TCINSTRUCTIONS.insertMOV(
    dynamic_cast<TC_DRegV &>( r.begin()->get() ), 0, $1->getExp() );
  TCINSTRUCTIONS.insertDEXTR(
    dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( r.begin()->get() ),
    dynamic_cast<TC_DRegV &>( p.second.get().begin()->get() ), $1->getExp() );
  TCINSTRUCTIONS.insertJ( b3, $1->getExp() );

  // Generate code for shifts by less than 32 bits.
  TC179x_wirBB = &b2;
  TCINSTRUCTIONS.insertDEXTR(
    dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( r.begin()->get() ),
    dynamic_cast<TC_DRegV &>( p.second.get().begin()->get() ), $1->getExp() );
  if ( useSHA )
    TCINSTRUCTIONS.insertSHA(
      dynamic_cast<TC_DRegV &>( r.begin()->get() ),
      dynamic_cast<TC_DRegV &>( r.begin()->get() ),
      dynamic_cast<TC_DRegV &>( p.second.get().begin()->get() ), $1->getExp() );
  else
    TCINSTRUCTIONS.insertSH(
      dynamic_cast<TC_DRegV &>( r.begin()->get() ),
      dynamic_cast<TC_DRegV &>( r.begin()->get() ),
      dynamic_cast<TC_DRegV &>( p.second.get().begin()->get() ), $1->getExp() );

  // Switch to end block.
  TC179x_wirBB = &b3;

  return( wbInfo );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_result_llereg: tpm_AssignExpSHL( ca_lhs_llereg, dreg )
{
  $cost[0] =
    $cost[2] + $cost[3] + TC13::OperationFormat::DC5L.getSize() +
    TC13::OperationFormat::SDD_1.getSize() +
    TC13::OperationFormat::SDC4_1.getSize() +
    2 * TC13::OperationFormat::DDDD.getSize() +
    TC13::OperationFormat::L.getSize() + TC13::OperationFormat::DDD_1.getSize();
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_llereg: tpm_AssignExpSHL( ca_lhs_llereg, dreg )", $1 );

  bool useSHA =
    TCIR_CONFIGURATION->arithmeticSHR &&
    !$2->getExp()->getType().isUnsignedType();

  // Evaluate RHS first to account for side effects and lower register pressure.
  auto p = $action[3]();
  auto wbInfo = $action[2]( true );

  // LLIR
  const string lt32shamt = LLIR::getUniqueLabel();
  const string gt31shamt = LLIR::getUniqueLabel();
  const string endBlock = LLIR::getUniqueLabel();

  auto *reg = wbInfo.getTempReg();
  TCINSTRUCTIONS.insertJZ_T( p.first, 5, lt32shamt, $1->getExp() );

  // Create block for shifting by more than 31 bits.
  beginNewLLIRBasicBlock( gt31shamt.c_str() );
  TCINSTRUCTIONS.insertMOV(
    getHVLLChild( reg ), getLVLLChild( reg ), $1->getExp() );
  TCINSTRUCTIONS.insertMOV( getLVLLChild( reg ), 0, $1->getExp() );
  TCINSTRUCTIONS.insertDEXTR(
    getHVLLChild( reg ), getHVLLChild( reg ), getLVLLChild( reg ), p.first,
    $1->getExp() );
  TCINSTRUCTIONS.insertJ( endBlock.c_str(), $1->getExp() );

  // Create block for shifting by less than 32 bits.
  beginNewLLIRBasicBlock( lt32shamt.c_str() );
  TCINSTRUCTIONS.insertDEXTR(
    getHVLLChild( reg ), getHVLLChild( reg ), getLVLLChild( reg ), p.first,
    $1->getExp() );
  if ( useSHA )
    TCINSTRUCTIONS.insertSHA(
      getLVLLChild( reg ), getLVLLChild( reg ), p.first, $1->getExp() );
  else
    TCINSTRUCTIONS.insertSH(
      getLVLLChild( reg ), getLVLLChild( reg ), p.first, $1->getExp() );

  // Create end block.
  beginNewLLIRBasicBlock( endBlock.c_str() );

  // WIR
  // Create basic block for shifts by more than 31 bits.
  auto *currentBB = TC179x_wirBB;
  auto &b1 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  // Create basic block for shifts by less than 32 bits.
  auto &b2 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  // Create end basic block.
  auto &b3 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  TC179x_wirBB = currentBB;

  auto &r = dynamic_cast<TC_ERegV &>( wbInfo.mTmpReg.get() );
  TCINSTRUCTIONS.insertJZ_T( p.second, 5, b2, $1->getExp() );

  // Generate code for shifts by more than 31 bits.
  TC179x_wirBB = &b1;
  TCINSTRUCTIONS.insertMOV(
    dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( r.begin()->get() ), $1->getExp() );
  TCINSTRUCTIONS.insertMOV(
    dynamic_cast<TC_DRegV &>( r.begin()->get() ), 0, $1->getExp() );
  TCINSTRUCTIONS.insertDEXTR(
    dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( r.begin()->get() ), p.second, $1->getExp() );
  TCINSTRUCTIONS.insertJ( b3, $1->getExp() );

  // Generate code for shifts by less than 32 bits.
  TC179x_wirBB = &b2;
  TCINSTRUCTIONS.insertDEXTR(
    dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( r.begin()->get() ), p.second, $1->getExp() );
  if ( useSHA )
    TCINSTRUCTIONS.insertSHA(
      dynamic_cast<TC_DRegV &>( r.begin()->get() ),
      dynamic_cast<TC_DRegV &>( r.begin()->get() ), p.second, $1->getExp() );
  else
    TCINSTRUCTIONS.insertSH(
      dynamic_cast<TC_DRegV &>( r.begin()->get() ),
      dynamic_cast<TC_DRegV &>( r.begin()->get() ), p.second, $1->getExp() );

  // Switch to end block.
  TC179x_wirBB = &b3;

  return( wbInfo );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_result_llereg: tpm_AssignExpSHL( ca_lhs_llereg, const9 )
{
  auto *c = dynamic_cast<IR_IntConstExp *>( $3->getExp() );

  if ( c ) {
    $cost[0] = $cost[2] + $cost[3];

    if ( c->getValue().getIntValue() > 31 )
      $cost[0] +=
        TC13::OperationFormat::SDD_1.getSize() +
        TC13::OperationFormat::SDC4_1.getSize() +
        TC13::OperationFormat::DDDC5.getSize();
    else

    if ( c->getValue().getIntValue() != 0 )
      $cost[0] +=
        TC13::OperationFormat::DDDC5.getSize() +
        TC13::OperationFormat::DDC9_1.getSize();
  } else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_llereg: tpm_AssignExpSHL( ca_lhs_llereg, const9 )", $1 );

  bool useSHA =
    TCIR_CONFIGURATION->arithmeticSHR &&
    !$2->getExp()->getType().isUnsignedType();

  auto wbInfo = $action[2]( true );
  auto v = $action[3]().getIntValue();

  // LLIR
  auto *reg = wbInfo.getTempReg();

  if ( v > 31 ) {
    TCINSTRUCTIONS.insertMOV(
      getHVLLChild( reg ), getLVLLChild( reg ), $1->getExp() );
    TCINSTRUCTIONS.insertMOV( getLVLLChild( reg ), 0, $1->getExp() );
    TCINSTRUCTIONS.insertDEXTR(
      getHVLLChild( reg ), getHVLLChild( reg ), getLVLLChild( reg ), v & 0x1F,
      $1->getExp() );
  } else

  if ( v != 0 ) {
    TCINSTRUCTIONS.insertDEXTR(
      getHVLLChild( reg ), getHVLLChild( reg ), getLVLLChild( reg ), v,
      $1->getExp() );
    if ( useSHA )
      TCINSTRUCTIONS.insertSHA(
        getLVLLChild( reg ), getLVLLChild( reg ), v, $1->getExp() );
    else
      TCINSTRUCTIONS.insertSH(
        getLVLLChild( reg ), getLVLLChild( reg ), v, $1->getExp() );
  }

  // WIR
  auto &r = dynamic_cast<TC_ERegV &>( wbInfo.mTmpReg.get() );

  if ( v > 31 ) {
    TCINSTRUCTIONS.insertMOV(
      dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
      dynamic_cast<TC_DRegV &>( r.begin()->get() ), $1->getExp() );
    TCINSTRUCTIONS.insertMOV(
      dynamic_cast<TC_DRegV &>( r.begin()->get() ), 0, $1->getExp() );
    TCINSTRUCTIONS.insertDEXTR(
      dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
      dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
      dynamic_cast<TC_DRegV &>( r.begin()->get() ), v & 0x1F, $1->getExp() );
  } else

  if ( v != 0 ) {
    TCINSTRUCTIONS.insertDEXTR(
      dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
      dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
      dynamic_cast<TC_DRegV &>( r.begin()->get() ), v, $1->getExp() );
    if ( useSHA )
      TCINSTRUCTIONS.insertSHA(
        dynamic_cast<TC_DRegV &>( r.begin()->get() ),
        dynamic_cast<TC_DRegV &>( r.begin()->get() ), v, $1->getExp() );
    else
      TCINSTRUCTIONS.insertSH(
        dynamic_cast<TC_DRegV &>( r.begin()->get() ),
        dynamic_cast<TC_DRegV &>( r.begin()->get() ), v, $1->getExp() );
  }

  return( wbInfo );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_result_llereg: tpm_AssignExpSHR( ca_lhs_llereg, llereg )
{
  $cost[0] =
    $cost[2] + $cost[3] + TC13::OperationFormat::DC4L_1.getSize() +
    2 * TC13::OperationFormat::DDC9_1.getSize() +
    TC13::OperationFormat::DC5L.getSize() +
    2 * TC13::OperationFormat::DDD_1.getSize() +
    2 * TC13::OperationFormat::L.getSize() +
    TC13::OperationFormat::DDDD.getSize() +
    TC13::OperationFormat::SDD_1.getSize();

  if ( $2->getExp()->getType().isSignedType() )
    $cost[0] += TC13::OperationFormat::DDC5C5.getSize();
  else
    $cost[0] += TC13::OperationFormat::SDC4_1.getSize();

}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_llereg: tpm_AssignExpSHR( ca_lhs_llereg, llereg )", $1 );

  bool useSHA =
    TCIR_CONFIGURATION->arithmeticSHR &&
    !$2->getExp()->getType().isUnsignedType();

  // Evaluate RHS first to account for side effects and lower register pressure.
  auto p = $action[3]();
  auto wbInfo = $action[2]( true );

  // LLIR
  const string initBlock = LLIR::getUniqueLabel();
  const string lt32shamt = LLIR::getUniqueLabel();
  const string gt31shamt = LLIR::getUniqueLabel();
  const string moveBlock = LLIR::getUniqueLabel();
  const string remBlock = LLIR::getUniqueLabel();
  const string endBlock = LLIR::getUniqueLabel();

  auto *reg = wbInfo.getTempReg();
  auto *regTmp = TCINSTRUCTIONS.CreateRegister( "" );

  TCINSTRUCTIONS.insertJEQ(
    getLVLLChild( p.first ), 0, moveBlock, $1->getExp() );

  // Create block for computing the shift amount.
  beginNewLLIRBasicBlock( initBlock.c_str() );
  TCINSTRUCTIONS.insertRSUB( regTmp, getLVLLChild( p.first ), 32, $1->getExp() );
  TCINSTRUCTIONS.insertJZ_T(
    getLVLLChild( p.first ), 5, lt32shamt, $1->getExp() );

  // Create block for shifting by more than 31 bits.
  beginNewLLIRBasicBlock( gt31shamt.c_str() );
  if ( useSHA )
    TCINSTRUCTIONS.insertSHA(
      getLVLLChild( reg ), getHVLLChild( reg ), regTmp, $1->getExp() );
  else
    TCINSTRUCTIONS.insertSH(
      getLVLLChild( reg ), getHVLLChild( reg ), regTmp, $1->getExp() );
  if ( $2->getExp()->getType().isSignedType() )
    TCINSTRUCTIONS.insertEXTR(
      getHVLLChild( reg ), getLVLLChild( reg ), 31, 1, $1->getExp() );
  else
    TCINSTRUCTIONS.insertMOV( getHVLLChild( reg ), 0, $1->getExp() );
  TCINSTRUCTIONS.insertJ( endBlock.c_str(), $1->getExp() );

  // Create block for shifting by less than 32 bits.
  beginNewLLIRBasicBlock( lt32shamt.c_str() );
  TCINSTRUCTIONS.insertDEXTR(
    getLVLLChild( reg ), getHVLLChild( reg ), getLVLLChild( reg ), regTmp,
    $1->getExp() );
  TCINSTRUCTIONS.insertJ( remBlock.c_str(), $1->getExp() );

  // Create block for moving if shift amount is zero.
  beginNewLLIRBasicBlock( moveBlock.c_str() );
  TCINSTRUCTIONS.insertMOV(
    getLVLLChild( reg ), getLVLLChild( reg ), $1->getExp() );

  // Create block for computing the high-value result word.
  beginNewLLIRBasicBlock( remBlock.c_str() );
  TCINSTRUCTIONS.insertRSUB( regTmp, getLVLLChild( p.first ), 0, $1->getExp() );
  if ( useSHA )
    TCINSTRUCTIONS.insertSHA(
      getHVLLChild( reg ), getHVLLChild( reg ), regTmp, $1->getExp() );
  else
    TCINSTRUCTIONS.insertSH(
      getHVLLChild( reg ), getHVLLChild( reg ), regTmp, $1->getExp() );

  // Create end block.
  beginNewLLIRBasicBlock( endBlock.c_str() );

  // WIR
  // Create basic block to compute the shift amount.
  auto *currentBB = TC179x_wirBB;
  auto &b1 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  // Create basic block for shifts by more than 31 bits.
  auto &b2 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  // Create basic block for shifts by less than 32 bits.
  auto &b3 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  // Create basic block for moves if shift amount is zero.
  auto &b4 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  // Create basic block to compute the high-value result word.
  auto &b5 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  // Create end basic block.
  auto &b6 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  TC179x_wirBB = currentBB;

  auto &r = dynamic_cast<TC_ERegV &>( wbInfo.mTmpReg.get() );
  auto &tmpReg = TCINSTRUCTIONS.createDReg();

  TCINSTRUCTIONS.insertJEQ(
    dynamic_cast<TC_DRegV &>( p.second.get().begin()->get() ), 0, b4,
    $1->getExp() );

  // Create block to compute the shift amount.
  TC179x_wirBB = &b1;
  TCINSTRUCTIONS.insertRSUB(
    tmpReg, dynamic_cast<TC_DRegV &>( p.second.get().begin()->get() ), 32,
    $1->getExp() );
  TCINSTRUCTIONS.insertJZ_T(
    dynamic_cast<TC_DRegV &>( p.second.get().begin()->get() ), 5, b3,
    $1->getExp() );

  // Generate code for shifts by more than 31 bits.
  TC179x_wirBB = &b2;
  if ( useSHA )
    TCINSTRUCTIONS.insertSHA(
      dynamic_cast<TC_DRegV &>( r.begin()->get() ),
      dynamic_cast<TC_DRegV &>( r.rbegin()->get() ), tmpReg, $1->getExp() );
  else
    TCINSTRUCTIONS.insertSH(
      dynamic_cast<TC_DRegV &>( r.begin()->get() ),
      dynamic_cast<TC_DRegV &>( r.rbegin()->get() ), tmpReg, $1->getExp() );
  if ( $2->getExp()->getType().isSignedType() )
    TCINSTRUCTIONS.insertEXTR(
      dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
      dynamic_cast<TC_DRegV &>( r.begin()->get() ), 31, 1, $1->getExp() );
  else
    TCINSTRUCTIONS.insertMOV(
      dynamic_cast<TC_DRegV &>( r.rbegin()->get() ), 0, $1->getExp() );
  TCINSTRUCTIONS.insertJ( b6, $1->getExp() );

  // Generate code for shifts by less than 32 bits.
  TC179x_wirBB = &b3;
  TCINSTRUCTIONS.insertDEXTR(
    dynamic_cast<TC_DRegV &>( r.begin()->get() ),
    dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( r.begin()->get() ), tmpReg, $1->getExp() );
  TCINSTRUCTIONS.insertJ( b5, $1->getExp() );

  // Generate move if shift amount is zero.
  TC179x_wirBB = &b4;
  TCINSTRUCTIONS.insertMOV(
    dynamic_cast<TC_DRegV &>( r.begin()->get() ),
    dynamic_cast<TC_DRegV &>( r.begin()->get() ), $1->getExp() );

  // Generate code to compute the high-value result word.
  TC179x_wirBB = &b5;
  TCINSTRUCTIONS.insertRSUB(
    tmpReg, dynamic_cast<TC_DRegV &>( p.second.get().begin()->get() ), 0,
    $1->getExp() );
  if ( useSHA )
    TCINSTRUCTIONS.insertSHA(
      dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
      dynamic_cast<TC_DRegV &>( r.rbegin()->get() ), tmpReg, $1->getExp() );
  else
    TCINSTRUCTIONS.insertSH(
      dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
      dynamic_cast<TC_DRegV &>( r.rbegin()->get() ), tmpReg, $1->getExp() );

  // Switch to end block.
  TC179x_wirBB = &b6;

  return( wbInfo );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_result_llereg: tpm_AssignExpSHR( ca_lhs_llereg, dreg )
{
  $cost[0] =
    $cost[2] + $cost[3] + TC13::OperationFormat::DC4L_1.getSize() +
    2 * TC13::OperationFormat::DDC9_1.getSize() +
    TC13::OperationFormat::DC5L.getSize() +
    2 * TC13::OperationFormat::DDD_1.getSize() +
    2 * TC13::OperationFormat::L.getSize() +
    TC13::OperationFormat::DDDD.getSize() +
    TC13::OperationFormat::SDD_1.getSize();

  if ( $2->getExp()->getType().isSignedType() )
    $cost[0] += TC13::OperationFormat::DDC5C5.getSize();
  else
    $cost[0] += TC13::OperationFormat::SDC4_1.getSize();
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_llereg: tpm_AssignExpSHR( ca_lhs_llereg, dreg )", $1 );

  bool useSHA =
    TCIR_CONFIGURATION->arithmeticSHR &&
    !$2->getExp()->getType().isUnsignedType();

  // Evaluate RHS first to account for side effects and lower register pressure.
  auto p = $action[3]();
  auto wbInfo = $action[2]( true );

  // LLIR
  const string initBlock = LLIR::getUniqueLabel();
  const string lt32shamt = LLIR::getUniqueLabel();
  const string gt31shamt = LLIR::getUniqueLabel();
  const string moveBlock = LLIR::getUniqueLabel();
  const string remBlock = LLIR::getUniqueLabel();
  const string endBlock = LLIR::getUniqueLabel();

  auto *reg = wbInfo.getTempReg();
  auto *regTmp = TCINSTRUCTIONS.CreateRegister( "" );

  TCINSTRUCTIONS.insertJEQ( p.first, 0, moveBlock, $1->getExp() );

  // Create block for computing the shift amount.
  beginNewLLIRBasicBlock( initBlock.c_str() );
  TCINSTRUCTIONS.insertRSUB( regTmp, p.first, 32, $1->getExp() );
  TCINSTRUCTIONS.insertJZ_T( p.first, 5, lt32shamt, $1->getExp() );

  // Create block for shifting by more than 31 bits.
  beginNewLLIRBasicBlock( gt31shamt.c_str() );
  if ( useSHA )
    TCINSTRUCTIONS.insertSHA(
      getLVLLChild( reg ), getHVLLChild( reg ), regTmp, $1->getExp() );
  else
    TCINSTRUCTIONS.insertSH(
      getLVLLChild( reg ), getHVLLChild( reg ), regTmp, $1->getExp() );
  if ( $2->getExp()->getType().isSignedType() )
    TCINSTRUCTIONS.insertEXTR(
      getHVLLChild( reg ), getLVLLChild( reg ), 31, 1, $1->getExp() );
  else
    TCINSTRUCTIONS.insertMOV( getHVLLChild( reg ), 0, $1->getExp() );
  TCINSTRUCTIONS.insertJ( endBlock.c_str(), $1->getExp() );

  // Create block for shifting by less than 32 bits.
  beginNewLLIRBasicBlock( lt32shamt.c_str() );
  TCINSTRUCTIONS.insertDEXTR(
    getLVLLChild( reg ), getHVLLChild( reg ), getLVLLChild( reg ), regTmp,
    $1->getExp() );
  TCINSTRUCTIONS.insertJ( remBlock.c_str(), $1->getExp() );

  // Create block for moving if shift amount is zero.
  beginNewLLIRBasicBlock( moveBlock.c_str() );
  TCINSTRUCTIONS.insertMOV(
    getLVLLChild( reg ), getLVLLChild( reg ), $1->getExp() );

  // Create block for computing the high-value result word.
  beginNewLLIRBasicBlock( remBlock.c_str() );
  TCINSTRUCTIONS.insertRSUB( regTmp, p.first, 0, $1->getExp() );
  if ( useSHA )
    TCINSTRUCTIONS.insertSHA(
      getHVLLChild( reg ), getHVLLChild( reg ), regTmp, $1->getExp() );
  else
    TCINSTRUCTIONS.insertSH(
      getHVLLChild( reg ), getHVLLChild( reg ), regTmp, $1->getExp() );

  // Create end block.
  beginNewLLIRBasicBlock( endBlock.c_str() );

  // WIR
  // Create basic block to compute the shift amount.
  auto *currentBB = TC179x_wirBB;
  auto &b1 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  // Create basic block for shifts by more than 31 bits.
  auto &b2 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  // Create basic block for shifts by less than 32 bits.
  auto &b3 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  // Create basic block for moves if shift amount is zero.
  auto &b4 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  // Create basic block to compute the high-value result word.
  auto &b5 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  // Create end basic block.
  auto &b6 =
    TCCODESEL->startNewBasicBlock( *$1->getExp()->getStmt().getBasicBlock() );
  TC179x_wirBB = currentBB;

  auto &r = dynamic_cast<TC_ERegV &>( wbInfo.mTmpReg.get() );
  auto &tmpReg = TCINSTRUCTIONS.createDReg();

  TCINSTRUCTIONS.insertJEQ( p.second, 0, b4, $1->getExp() );

  // Create block to compute the shift amount.
  TC179x_wirBB = &b1;
  TCINSTRUCTIONS.insertRSUB( tmpReg, p.second, 32, $1->getExp() );
  TCINSTRUCTIONS.insertJZ_T( p.second, 5, b3, $1->getExp() );

  // Generate code for shifts by more than 31 bits.
  TC179x_wirBB = &b2;
  if ( useSHA )
    TCINSTRUCTIONS.insertSHA(
      dynamic_cast<TC_DRegV &>( r.begin()->get() ),
      dynamic_cast<TC_DRegV &>( r.rbegin()->get() ), tmpReg, $1->getExp() );
  else
    TCINSTRUCTIONS.insertSH(
      dynamic_cast<TC_DRegV &>( r.begin()->get() ),
      dynamic_cast<TC_DRegV &>( r.rbegin()->get() ), tmpReg, $1->getExp() );
  if ( $2->getExp()->getType().isSignedType() )
    TCINSTRUCTIONS.insertEXTR(
      dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
      dynamic_cast<TC_DRegV &>( r.begin()->get() ), 31, 1, $1->getExp() );
  else
    TCINSTRUCTIONS.insertMOV(
      dynamic_cast<TC_DRegV &>( r.rbegin()->get() ), 0, $1->getExp() );
  TCINSTRUCTIONS.insertJ( b6, $1->getExp() );

  // Generate code for shifts by less than 32 bits.
  TC179x_wirBB = &b3;
  TCINSTRUCTIONS.insertDEXTR(
    dynamic_cast<TC_DRegV &>( r.begin()->get() ),
    dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
    dynamic_cast<TC_DRegV &>( r.begin()->get() ), tmpReg, $1->getExp() );
  TCINSTRUCTIONS.insertJ( b5, $1->getExp() );

  // Generate move if shift amount is zero.
  TC179x_wirBB = &b4;
  TCINSTRUCTIONS.insertMOV(
    dynamic_cast<TC_DRegV &>( r.begin()->get() ),
    dynamic_cast<TC_DRegV &>( r.begin()->get() ), $1->getExp() );

  // Generate code to compute the high-value result word.
  TC179x_wirBB = &b5;
  TCINSTRUCTIONS.insertRSUB( tmpReg, p.second, 0, $1->getExp() );
  if ( useSHA )
    TCINSTRUCTIONS.insertSHA(
      dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
      dynamic_cast<TC_DRegV &>( r.rbegin()->get() ), tmpReg, $1->getExp() );
  else
    TCINSTRUCTIONS.insertSH(
      dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
      dynamic_cast<TC_DRegV &>( r.rbegin()->get() ), tmpReg, $1->getExp() );

  // Switch to end block.
  TC179x_wirBB = &b6;

  return( wbInfo );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_result_llereg: tpm_AssignExpSHR( ca_lhs_llereg, const9 )
{
  auto *c = dynamic_cast<IR_IntConstExp *>( $3->getExp() );

  if ( c ) {
    $cost[0] = $cost[2] + $cost[3];

    if ( c->getValue().getIntValue() > 31 ) {
      $cost[0] += TC13::OperationFormat::DDC9_1.getSize();

      if ( $2->getExp()->getType().isSignedType() )
        $cost[0] += TC13::OperationFormat::DDC5C5.getSize();
      else
        $cost[0] += TC13::OperationFormat::SDC4_1.getSize();
    } else

    if ( c->getValue().getIntValue() != 0 )
      $cost[0] +=
        TC13::OperationFormat::DDDC5.getSize() +
        TC13::OperationFormat::DDC9_1.getSize();
  } else
    $cost[0] = COST_INFINITY;
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_llereg: tpm_AssignExpSHR( ca_lhs_llereg, const9 )", $1 );

  bool useSHA =
    TCIR_CONFIGURATION->arithmeticSHR &&
    !$2->getExp()->getType().isUnsignedType();

  auto wbInfo = $action[2]( true );
  auto v = $action[3]().getIntValue();

  // LLIR
  auto *reg = wbInfo.getTempReg();

  if ( v > 31 ) {
    if ( useSHA )
      TCINSTRUCTIONS.insertSHA(
        getLVLLChild( reg ), getHVLLChild( reg ), -( v & 0x1F ), $1->getExp() );
    else
      TCINSTRUCTIONS.insertSH(
        getLVLLChild( reg ), getHVLLChild( reg ), -( v & 0x1F ), $1->getExp() );
    if ( $2->getExp()->getType().isSignedType() )
      TCINSTRUCTIONS.insertEXTR(
        getHVLLChild( reg ), getLVLLChild( reg ), 31, 1, $1->getExp() );
    else
      TCINSTRUCTIONS.insertMOV( getHVLLChild( reg ), 0, $1->getExp() );
  } else

  if ( v != 0 ) {
    TCINSTRUCTIONS.insertDEXTR(
      getLVLLChild( reg ), getHVLLChild( reg ), getLVLLChild( reg ), 32 - v,
      $1->getExp() );
    if ( useSHA )
      TCINSTRUCTIONS.insertSHA(
        getHVLLChild( reg ), getHVLLChild( reg ), -v, $1->getExp() );
    else
      TCINSTRUCTIONS.insertSH(
        getHVLLChild( reg ), getHVLLChild( reg ), -v, $1->getExp() );
  }

  // WIR
  auto &r = dynamic_cast<TC_ERegV &>( wbInfo.mTmpReg.get() );

  if ( v > 31 ) {
    if ( useSHA )
      TCINSTRUCTIONS.insertSHA(
        dynamic_cast<TC_DRegV &>( r.begin()->get() ),
        dynamic_cast<TC_DRegV &>( r.rbegin()->get() ), -( v & 0x1F ),
        $1->getExp() );
    else
      TCINSTRUCTIONS.insertSH(
        dynamic_cast<TC_DRegV &>( r.begin()->get() ),
        dynamic_cast<TC_DRegV &>( r.rbegin()->get() ), -( v & 0x1F ),
        $1->getExp() );
    if ( $2->getExp()->getType().isSignedType() )
      TCINSTRUCTIONS.insertEXTR(
        dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
        dynamic_cast<TC_DRegV &>( r.begin()->get() ), 31, 1, $1->getExp() );
    else
      TCINSTRUCTIONS.insertMOV(
        dynamic_cast<TC_DRegV &>( r.rbegin()->get() ), 0, $1->getExp() );
  } else

  if ( v != 0 ) {
    TCINSTRUCTIONS.insertDEXTR(
      dynamic_cast<TC_DRegV &>( r.begin()->get() ),
      dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
      dynamic_cast<TC_DRegV &>( r.begin()->get() ), 32 - v, $1->getExp() );
    if ( useSHA )
      TCINSTRUCTIONS.insertSHA(
        dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
        dynamic_cast<TC_DRegV &>( r.rbegin()->get() ), -v, $1->getExp() );
    else
      TCINSTRUCTIONS.insertSH(
        dynamic_cast<TC_DRegV &>( r.rbegin()->get() ),
        dynamic_cast<TC_DRegV &>( r.rbegin()->get() ), -v, $1->getExp() );
  }

  return( wbInfo );
};


##############################################################################
#
# Expressions for Pointer Handling
#
###############################################################################

# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_result_areg: tpm_AssignExpPLUS( ca_lhs_areg, addrOffset )
{
  int off =
    getConstIntValue( $3 ) * computeSizeOf( getBaseType( *$2->getExp() ) );

  $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::AAC16BOA.getSize();

  if ( ( off < TC_Const16_Signed::getMinValue( 16 ) ) ||
       ( off > TC_Const16_Signed::getMaxValue( 16 ) ) )
    $cost[0] +=
      TC13::OperationFormat::SAA_1.getSize() +
      TC13::OperationFormat::AAC16.getSize();
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_areg: tpm_AssignExpPLUS( ca_lhs_areg, addrOffset )", $1 );

  auto wbInfo = $action[2]( true );
  int off =
    $action[3]().getIntValue() * computeSizeOf( getBaseType( *$2->getExp() ) );

  // LLIR
  auto *reg = wbInfo.getTempReg();
  TCINSTRUCTIONS.insertLEA( reg, OPER_BASE, reg, off, $1->getExp() );

  // WIR
  auto &r = dynamic_cast<TC_ARegV &>( wbInfo.mTmpReg.get() );
  TCINSTRUCTIONS.insertLEA( r, r, off, $1->getExp() );

  return( wbInfo );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_result_areg: tpm_AssignExpPLUS( ca_lhs_areg, dreg )
{
  const int byteSize = computeSizeOf( getBaseType( *$2->getExp() ) );

  $cost[0] =
    $cost[2] + $cost[3] + TC13::OperationFormat::SAA_1.getSize() +
    loadRegisterRelativeAddressCost( byteSize );
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_areg: tpm_AssignExpPLUS( ca_lhs_areg, dreg )", $1 );

  const int byteSize = computeSizeOf( getBaseType( *$2->getExp() ) );

  // Evaluate RHS first to account for side effects and lower register pressure.
  auto p = $action[3]();
  auto wbInfo = $action[2]( true );

  // LLIR
  auto *reg = wbInfo.getTempReg();
  LLIR_Register *regTmp =
    loadRegisterRelativeAddress( reg, p.first, byteSize, $1->getExp() );
  TCINSTRUCTIONS.insertMOV_AA( reg, regTmp, $1->getExp() );

  // WIR
  auto &r = dynamic_cast<TC_ARegV &>( wbInfo.mTmpReg.get() );
  auto &tmpReg = loadRegRelativeAddr( r, p.second, byteSize, $1->getExp() );
  TCINSTRUCTIONS.insertMOV_AA( r, tmpReg, $1->getExp() );

  return( wbInfo );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_result_areg: tpm_AssignExpMINUS( ca_lhs_areg, addrOffset )
{
  int off =
    -getConstIntValue( $3 ) * computeSizeOf( getBaseType( *$2->getExp() ) );

  $cost[0] = $cost[2] + $cost[3] + TC13::OperationFormat::AAC16BOA.getSize();

  if ( ( off < TC_Const16_Signed::getMinValue( 16 ) ) ||
       ( off > TC_Const16_Signed::getMaxValue( 16 ) ) )
    $cost[0] +=
      TC13::OperationFormat::SAA_1.getSize() +
      TC13::OperationFormat::AAC16.getSize();
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_areg: tpm_AssignExpMINUS( ca_lhs_areg, addrOffset )", $1 );

  auto wbInfo = $action[2]( true );
  int off =
    -$action[3]().getIntValue() * computeSizeOf( getBaseType( *$2->getExp() ) );

  // LLIR
  auto *reg = wbInfo.getTempReg();
  TCINSTRUCTIONS.insertLEA( reg, OPER_BASE, reg, off, $1->getExp() );

  // WIR
  auto &r = dynamic_cast<TC_ARegV &>( wbInfo.mTmpReg.get() );
  TCINSTRUCTIONS.insertLEA( r, r, off, $1->getExp() );

  return( wbInfo );
};


# @author Heiko Falk <Heiko.Falk@tuhh.de>
ca_result_areg: tpm_AssignExpMINUS( ca_lhs_areg, dreg )
{
  const int byteSize = computeSizeOf( getBaseType( *$2->getExp() ) );

  $cost[0] =
    $cost[2] + $cost[3] + TC13::OperationFormat::DDC9_1.getSize() +
    TC13::OperationFormat::SAA_1.getSize() +
    loadRegisterRelativeAddressCost( byteSize );
}
=
{
  DEBUG_RULE_ACTION(
    "ca_result_areg: tpm_AssignExpMINUS( ca_lhs_areg, dreg )", $1 );

  // Evaluate RHS first to account for side effects and lower register pressure.
  auto p = $action[3]();
  auto wbInfo = $action[2]( true );

  const int byteSize = computeSizeOf( getBaseType( *$2->getExp() ) );

  // LLIR
  auto *reg = wbInfo.getTempReg();
  LLIR_Register *regTmp = TCINSTRUCTIONS.CreateRegister( "" );
  TCINSTRUCTIONS.insertRSUB( regTmp, p.first, 0, $1->getExp() );
  LLIR_Register *regTmp1 =
    loadRegisterRelativeAddress( reg, regTmp, byteSize, $1->getExp() );
  TCINSTRUCTIONS.insertMOV_AA( reg, regTmp1, $1->getExp() );

  // WIR
  auto &r = dynamic_cast<TC_ARegV &>( wbInfo.mTmpReg.get() );
  auto &tmpReg = TCINSTRUCTIONS.createDReg();
  TCINSTRUCTIONS.insertRSUB( tmpReg, p.second, 0, $1->getExp() );
  auto &tmpReg1 = loadRegRelativeAddr( r, tmpReg, byteSize, $1->getExp() );
  TCINSTRUCTIONS.insertMOV_AA( r, tmpReg1, $1->getExp() );

  return( wbInfo );
};
