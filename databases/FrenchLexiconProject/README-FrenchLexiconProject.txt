French Lexicon Project
======================

The French Lexicon Project (FLP) provides lexical decision data for
38,840 French words and the same number of nonwords. The full data
represents 1942000 reactions times from 975 participants.

Tables: [FLP.words.csv](FLP.words.csv) and
[FLP.pseudowords.csv](FLP.pseudowords.csv)

Web site: <https://sites.google.com/site/frenchlexicon/>

Publication:

Ferrand, Ludovic, Boris New, Marc Brysbaert, Emmanuel Keuleers, Patrick
Bonin, Alain Méot, Maria Augustinova, and Christophe Pallier. 2010. The
French Lexicon Project: Lexical Decision Data for 38,840 French Words
and 38,840 Pseudowords. *Behavior Research Methods* 42 (2): 488--96.
https://doi.org/10.3758/BRM.42.2.488.
[pdf](Ferrand%20et%20al.%20-%202010%20-%20The%20French%20Lexicon%20Project%20Lexical%20decision%20data%20.pdf)

------------------------------------------------------------------------

Running `make` executes the commands in `Makefile`: the scripts
[process.words.R](process.words.R) and
[process.pseudowords.R](process.pseudowords.R) compute the by-item
averages from the [raw lexical decision time](results.utf8.csv) data and
produce the final tables.

Intermediate `*.csv` files contain the raw data with an additional
column ('keep') signaling the items that are within the subject'
mean+-3stddev range.

Christophe Pallier

LICENSE CC BY-SA 4.0
