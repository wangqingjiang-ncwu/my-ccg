# Changelog for my-ccg

## Unreleased changes

2023-5-10 Add a new table in database ccg4c, ambi_resol1, which describe a new ambiguity resolution model as following.
+-------------+--------------+------+-----+---------+----------------+
| Field       | Type         | Null | Key | Default | Extra          |
+-------------+--------------+------+-----+---------+----------------+
| id          | int unsigned | NO   | PRI | NULL    | auto_increment |
| leftPhrase  | varchar(140) | YES  |     | NULL    |                |
| rightPhrase | varchar(140) | YES  |     | NULL    |                |
| context     | mediumtext   | YES  |     | NULL    |                |
| overType    | tinyint(1)   | YES  |     | NULL    |                |
| prior       | varchar(4)   | YES  |     | Noth    |                |
| hitCount    | int unsigned | YES  |     | 1       |                |
+-------------+--------------+------+-----+---------+----------------+
leftPhrase, rightPhrase and context are all defined by type PhraCate. context is the set of PhraCate created to now not
including the interoverlapped leftOver and rightOver. overType is the overlapping type between leftPhrase and rightPhrase.
prior dictates which one between leftOver and rightOver is selected to remain. hitCount records how many times the fragment
record has been hit to now.

Add module AmbiResol to include various types and functions about ambiguity resolution models and samples.
Model0: StruGene
Model1: AmbiResol1

Rename configuration file 'mysql_config' to 'Configuration', because some new configuration parameters are not related to database system MySQL.

According to parsing scripts of evert sentence, the process of constructive treebank becomes iterable. To study ambiguity resolution, several models for
ambiguity resolution fragments are designed successively. For every ambiguity resolution model, a same set of sentences should be parsed again.

2023-5-28 Add module clusering, which is used to analyze ambiguity resolution sampels.

2023-6-12 When ambiguity resolved results are not same as the anticipated, that is, some phrases bringing ambiguous overlaps are not removed because they are not banned phrases in previous parsing script, the manually resolving ambiguities are needed. This situation means the grammar used now is not same as the grammar used before. To keep different ambiguity resolved models are always for identical grammar, when grammar changes, ambiguity resolved fragment databases for different resolving models should be updated simultaneously.

In configuration file "Configuration", add the attribute "ambi_resol_models" to store all model names. For now, that is
ambi_resol_models:[ambi_resol0, ambi_resol1]
The model "ambi_resol0" is alias of original model "stru_gene".

2023-6-26 Add category conversion N/s for
    "s np\*np", where the phrase with category np\*np has structure XX.
The resultant category has still structure XX. That is not same with "np np\*np" with structure HnC.

2023-6-28 Make clear again that pronoun-quantifier phrase uses conversion ADJ/n to create.
    In statistical mudolue, functions 'countInTree', 'countInScript', 'SearchInTree', and 'searchInScript' read tree or script and perform various statistical tasks. In the past, these functions read from Table corpus. Now, the source of tree and script are set by the value of attribute 'tree_source' and 'script_source' in file Configuration. Thus, these statistical tasks can be performed on different treebanks and different scriptbanks.
