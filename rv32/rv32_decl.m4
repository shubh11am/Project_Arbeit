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
#  Copyright 2021 - 2022
#
#  Hamburg University of Technology (TUHH)
#  Institute of Embedded Systems
#  21071 Hamburg
#  Germany
#
#  http://www.tuhh.de/es/esd/research/wcc
#
#


# A Statement.
# @return Nothing is returned by a stmt non-terminal.
%declare<void> stmt;

# A Register
# @return A reference to the virtual register where a reg's result resides.
%declare<WIR::RV_RegV &><RV32::dummyRegV> reg;

# A modified register, always an rvalue.
# This nonterminal introduces a "delay" in the evaluation of pointer
# arithmetics. This allows for better performance, because the pointer
# arithmetics can be performed as part of a LD/ST instructions, if applicable.
%declare<RV32::RV32_AddressModification> modified_reg<void>;

# A register which may contain 'char', 'short', 'int' or 'float' together
# with the memory location from which it was read. This should only be used by
# LHS sides of assignments which must write back the result to memory. Parameter
# loadResult indicates whether the result should really be loaded from memory
# (true) or whether just the address should be computed (false).
%declare<RV32::RV32_LValue> deref_reg<bool loadResult>;

# Represents the called function:
# - args are the WIR registers that store the arguments that are passed by
#   register.
# - returnBehaviour specifies what kind of result is returned.
# Returned is the WIR register that contains the result.
%declare<WIR_BaseRegister *><nullptr> called_function<const RV32::argList &args,
                                                      enum RV32::RegType returnBehaviour>;

# A special nonterminal for handling function call arguments.
%declare<void> arg<bool dryrun, int index, int offset, IR_CallExp *theCall,
                   RV32::argList &args,RV32::argList &dryArgs>;

# An unsigned 5-bit constant.
# @return An IR integer storing the actual constant value.
%declare<IR_Integer> uconst5;

# A signed 6-bit constant.
# @return An IR integer storing the actual constant value.
%declare<IR_Integer> const6;

# An unsigned 6-bit constant.
# @return An IR integer storing the actual constant value.
%declare<IR_Integer> uconst6;

# An unsigned 8-bit constant.
# @return An IR integer storing the actual constant value.
%declare<IR_Integer> uconst8;

# A signed 12-bit constant.
# @return An IR integer storing the actual constant value.
%declare<IR_Integer> const12;

# An unsigned 20-bit constant.
# @return An IR integer storing the actual constant value.
%declare<IR_Integer> uconst20;
