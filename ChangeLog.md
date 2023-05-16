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
