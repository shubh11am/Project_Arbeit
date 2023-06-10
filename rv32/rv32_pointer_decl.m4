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
#  Copyright 2005 - 2022
#
#  Hamburg University of Technology (TUHH)
#  Institute of Embedded Systems
#  21071 Hamburg
#  Germany
#
#  http://www.tuhh.de/es/esd/research/wcc
#
#

# A register which contains an address (pointer or array).
%declare<WIR::RV_RegV &><RV32::dummyRegV> areg;

# An address register which contains pointers together with the memory location
# from which it was read. This should only be used by LHS sides of assignments
# which must write back the result to memory. Parameter loadResult indicates
# whether the result should really be loaded into memory (true)
# or whether just the address should be computed (false).
%declare<RV32::RV32_LValue> deref_areg<bool loadResult>;

# A modified address register, always an rvalue.
# This nonterminal introduces a "delay" in the evaluation of pointer
# arithmetics. This allows for better performance, because the pointer
# arithmetics can be performed as part of a LD/ST instructions, if applicable.
# The parameter indicates whether a 'dry run' should be performed, in which
# case the returned instance can only be used for cost calculation.
%declare<RV32::RV32_AddressModification> modified_areg<void>;

# This is only generated for constant addresses in memory.
%declare<IR_Integer> constAddress;

# The C standard dictates that the unary dereference operator immediately
# followed by the address operator shall not emit any code, i.e. &*e and e
# shall emit the same code. Note, however, that e could be an lvalue, whereas
# &*e is never an lvalue.
# This is realized with the zero_op_* nonterminals, for both the
# AdressModification and the Register* that could represent an address.
%declare<WIR::RV_RegV &><RV32::dummyRegV> zero_op_areg<void>;

# Non-terminals for constants that may appear as immediates in instructions.
%declare<IR_Integer> addrOffset;