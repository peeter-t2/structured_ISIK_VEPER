# MachineReadable-ISIK
A machine-readable dataset based on ISIK (Estonian biographical database, Eesti biograafiline andmebaas), main database is maintained by the Archival Library of the Literary Museum here: http://www2.kirmus.ee/biblioserver/isik/index.php

Files:
* ISIK processing.R - an R script to process the data
* ISIK main.csv - main data file of the corpus with one row per individual

I needed to collect metainformation for a linguistic corpus in my PhD research and I am working to make the ISIK biographical database machine-readable. 

Data is collected via the web-interface, with only the main biographical details kept. The longer biographical overview is split into pieces, and aligned with a few simple heuristics.

The dated biographical data will likely be converted into a long-form table that would allow individual life histories to be studied.


I figured other people may need this data too, so I thought I would do it publically. Tips & contributions welcome!
