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


# All compound assignments have the problem that if in an assignment where
# the lefthand side should be casted, no tpm_ImplicitCast can be generated
# because f.e. in a situation like
#
# int i; float f; i+= f;
#
# first 'i' must be casted to 'float' for the operation and then the result
# must be casted back to 'int'. To model this we introduce new nonterminals
# that do the first casting if neccessary. Then we do all compound assignment
# operations (+,-,...) in dedicated rules which can then be restricted to the
# the casted actual types, and must not care about casting. These dedicated
# rules return special 'ca_result' nonterminals which then cast the result
# back to the result type if that is neccessary. Through this framework we
# must only generate 6 rules per operation type (+,-,...) instead of 18 rules
# per operation which would be needed to cover all combinations manually.
# (This DOES matter, because with 10 different compound assignment types
#  we save 120 rules with this framework - enough redundancy for plenty of
#  annoying little bugs)


# All the following nonterminals are the (possibly casted) versions of standard
# nonterminals. They are casted if they are the LHS of an assignment expression
# and this assignment is a compound one, that requires them to be casted (see
# above for an example). The parameter 'loadResult' expresses whether the
# underlying rules, if they access the memory, should load the resulting value
# into a register (needed for compound assignments) or just load the address
# (needed for simple assignments)

%declare<WriteBackInfo> ca_lhs_dreg<bool loadResult>;
%declare<WriteBackInfo> ca_lhs_areg<bool loadResult>;
%declare<WriteBackInfo> ca_lhs_ereg<bool loadResult>;
%declare<WriteBackInfo> ca_lhs_llereg<bool loadResult>;

# These nonterminals do the back-casting to the type of the assignment

%declare<WriteBackInfo> ca_result_dreg<void>;
%declare<WriteBackInfo> ca_result_areg<void>;
%declare<WriteBackInfo> ca_result_ereg<void>;
%declare<WriteBackInfo> ca_result_llereg<void>;
