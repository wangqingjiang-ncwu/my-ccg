# my-ccg

Copyright (c) 2019-2022 China University of Water Resources and Electric Power,
All rights reserved.

This is a Haskell implemented, Combinatory Categorial Grammar based Chinese syntatic and semantic parser.

From version 1.0, many changes were completed, not limited to:
(1) An interactive syntactic and semantic parser is implemented. Sentential parsing is considered as a process of computing transitive closure of syntax type combinations. During every time of combinatory transition, syntax type conversions are selected on demand by user, then syntax type combinations are finished by machine, finally syntactic ambiguities are resolved by user, and the resultant transitive closure includes only one parsing tree instead of a syntactic forest.
(2) Introducing Clause Theory, a sentence is considered as a sequence of clauses. When parsing a sentence including multiple clauses, user can select to start parsing from a certain clause, which avoids repeat of analyzing the formerly parsed clauses. By storing selections of non-classical syntactic types and removed phrases, every clause might be parsed again automatically.
(3) A marking system of categorial conversions has already been formed, which includes various non-classical syntactic types used by parts of speech and phrasal structures. By augmenting with categorial conversions, Combinatory Categorial Grammar can express inflectional-absent Chinese.

The current version is 2.6.
