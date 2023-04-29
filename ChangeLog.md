# Changelog for my-ccg

## Unreleased changes

2023-4-28 Add a new table in database ccg4c, ambi_resol, which is used to store ambiguity resolution fragment.

According to parsing scripts of evert sentence, the process of constructive treebank becomes iterable. To study ambiguity resolution, several models for
ambiguity resolution fragments are designed successively. For every ambiguity resolution model, a same set of sentences should be parsed again.
