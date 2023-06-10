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
%declare<LLIR_Register *><nullptr> areg<void>;

# This nonterminal combines the dreg, ereg and areg nonterminals since they are
# all represented by the LLIR_Register*.
# Use this if you do not care whether the input register is a value register or
# an address / array register.
# Do not produce this nonterminal as output, however. Produce a reg, an ereg or
# an areg depending on the underlying type so that other rules can
# differentiate the two in their input.
%declare<LLIR_Register *><nullptr> any_reg<void>;

# An address register which contains pointers together with the memory location
# from which it was read. This should only be used by LHS sides of assignments
# which must write back the result to memory. Parameter loadResult indicates
# whether the result should really be loaded into memory (true)
# or whether just the address should be computed (false).
# The second parameter indicates whether the action part should insert any
# instructions or perform a 'dry run', in which case the returned lvalue can
# be used for cost calculations.
%declare<ARM_LValue> deref_areg<bool loadResult, bool dryRun>;

# A modified address register, always an rvalue.
# This nonterminal introduces a "delay" in the evaluation of pointer
# arithmetics. This allows for better performance, because the pointer
# arithmetics can be performed as part of a LD/ST instructions, if applicable.
# The parameter indicates whether a 'dry run' should be performed, in which
# case the returned instance can only be used for cost calculation.
%declare<ARM_AddressModification> modified_areg<bool dryRun>;

# The C standard dictates that the unary dereference operator immediately
# followed by the address operator shall not emit any code, i.e. &*e and e
# shall emit the same code. Note, however, that e could be an lvalue, whereas
# &*e is never an lvalue.
# This is realized with the zero_op_* nonterminals, for both the
# AdressModification and the LLIR_Register* that could represent an address.
%declare<ARM_AddressModification> zero_op_modified_areg<bool dryRun>;
%declare<LLIR_Register *><nullptr> zero_op_areg<void>;
