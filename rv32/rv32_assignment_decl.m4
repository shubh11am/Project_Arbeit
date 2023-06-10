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
#  Copyright 2022
#
#  Hamburg University of Technology (TUHH)
#  Institute of Embedded Systems
#  21071 Hamburg
#  Germany
#
#  http://www.tuhh.de/es/esd/research/wcc
#
#


# All update-assignment operators (e.g., +=, -=, >>=, ...) have the inherent
# problem that in an assignment where the left-hand side should be casted, no
# tpm_ImplicitCast can be generated. Consider, e.g., a situation like
#
# int i; float f; i+= f;
#
# Here, first 'i' must be casted to 'float' for the operation, then the result
# must be casted back to 'int'. To model this, we introduce new nonterminals
# that do the first casting if neccessary. Then, we do all compound assignment
# operations (+, -, ...) in dedicated rules which can then be restricted to the
# the casted actual types, and don't need to care about casting. These dedicated
# rules return special 'ca_result' nonterminals which then cast the result
# back to the result type if that is neccessary.
#
# Using this framework, we only need to provide 6 rules per operation type
# (+, -, ...) instead of 18 rules per operation which would be needed to cover
# all combinations manually. (This DOES matter, because with 10 different
# compound assignment types, we save 120 rules with this framework - enough
# redundancy for plenty of annoying little bugs.)

# A (possibly casted) version of the 'reg' non-terminal. It is casted if it is
# the LHS of an update-assignment operator that requires some casting (see
# above).
# @param[in] loadResult A Boolean flag denoting whether subsequent rules should
#                       load the resulting value into a register from memory
#                       (needed for assignments to compounds resulting in memory
#                       accesses), or not (for simple assignments).
# @return A RV32_WriteBackInfo struct providing information about the potential
#         memory write-back.
%declare<RV32::RV32_WriteBackInfo> ca_lhs_reg<bool loadResult>;

# A back-cast of a 'reg' non-terminal to the final type of an update-assignment
# operator (see above).
# @return A RV32_WriteBackInfo struct providing information about the potential
#         memory write-back.
%declare<RV32::RV32_WriteBackInfo> ca_result_reg<void>;

%declare<RV32::RV32_WriteBackInfo> ca_result_areg<void>;
%declare<RV32::RV32_WriteBackInfo> ca_lhs_areg<bool loadResult>;