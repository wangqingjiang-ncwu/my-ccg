# my-ccg
This is a Haskell implemented Chinese parser for Combinatory Categorial Grammar.

From version 1.0, many changes were completed, not limited to:
(1) By man-machine interactions, to designate category-converted rules such that class-changed phrase overlaps in Chinese can be  explained. 
(2) By pruning, namely removing unwanted phrases created from ambiguity of CCG category combinations, only one syntactic tree instead of a syntactic forest is obtained for every clause.
(3) By introducing Clause Theory in Chinese, to consider a sentence as a sequence of clauses.
(4) By storing rule selections and pruned phrases, every clause might be parsed again automatically.
(5) For complex sentences, which include multiple clauses, allow to start parsing from a certain clause, which avoids repeat of analyzing the former clauses.
