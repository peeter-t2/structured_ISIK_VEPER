# Structured ISIK and VEPER

ISIK (http://www2.kirmus.ee/biblioserver/isik/) is a biographical database compiled by the Archival Library of the Estonian Literary Museum based on the records of bibliographic archives. The information is given in semi-structured format, with some text processing needed to export the data. VEPER (http://isik.tlulib.ee/) is a biographical database compiled by Tallinn University focussed on Estonian diaspora. The database is presented in the same format, although different fields are used there.

This repository offers the data from both sites in format that is easier to use in data analysis. The dataset has rich information and not all of it has been structured, with some information in wrong fields for a few entries and the structures of ISIK and VEPER have not been fully consolidated. Contributions are welcome here, and it is the aim to offer a starting point for iterative improvements here.

## Files

### Data

Data is given in a number of different files, with the most useful files probably being `all_combined_compact.tsv` and `all_combined_maximal.tsv` which include the main information in a structured format. Parts of the dataset are given in the elements folder, with more detailed data files made for ISIK_education and ISIK_biographies available there as well. `manual/` folder includes data files that have been manually edited to link and align data. A specific use case is made in the export folder `export_compact_data_to_ENB.tsv` to link this data with the biographical data in Estonian National Bibliography. The links between the datasets have been made manually based on the names of the individuals.

```
./data/export/export_compact_data_to_ENB.tsv
./data/elements/ISIK_VEPER_metasimple.tsv
./data/elements/ISIK_VEPER_births_gis.tsv
./data/elements/ISIK_VEPER_births.tsv
./data/elements/ISIK_metasimple.tsv
./data/elements/ISIK_education_summary.tsv
./data/elements/ISIK_education_detailed.tsv
./data/elements/ISIK_biographies_summary.tsv
./data/elements/ISIK_biographies_detailed.tsv
./data/manual/links_allcoauthors.csv
./data/manual/ISIK_merged_only_manually_aligned.tsv
./data/manual/ISIK_basic_sorted_conv_manual_fixed_safecopy_changed.csv
./data/raw/VEPER.zip
./data/raw/ISIK.zip
./data/raw/ISIK_VEPER_long_raw_v7_all_fromzip.tsv
./data/all_combined_compact.tsv
./data/all_combined_maximal.tsv
```

Code

The code used to build the structured dataset is given here. Note that it has not been cleaned and sometimes references files from outside this repository. The main reason to include them is to be able to check the routines performed later on. Reproducing exactly the same results may prove difficult.
```
./build/0_ISIK_standalone_files_to_df_done_in_2019.R
./build/1_isik_raw_to_formatted.R
./build/2_life_history_from_ISIK_&_meta_all_2.R
./build/3_education_and_domiciles.R
./build/4_plot_ISIK_migrations.R
./build/5_make_combined_data_link2enb.R
```

## Data collection

Data is collected via the web-interface, with only the main biographical details kept. The longer biographical overview is split into pieces, and aligned with a few simple heuristics. The raw data is given in ISIK.zip and VEPER.zip files.

## Extraction of birthplaces

Birthplaces are given in a sentence that goes from less to more granular. Each of the levels were extracted via regular expressions from the dataset and stored in a separate column.

### Adding geo-locations to birthplaces

The birthplace locations on all 4 levels (county, parish1, parish2, village) were matched with geonames entries and their coordinates. The location with the most granular response was used. The matches require sensibility checks to be done, however the general quality of the matches seemed enough for simple analyses.

## Extraction of biographical data

Biographical data are provided in one text paragraph in ISIK, containing details on education and employment in a succinct semi-structured format. The data was retrieved via regular expressions and distributed into episodes of education and employment. They were then structured to count the highest level of education attained and years of schooling. For employment, the location of employment was extracted where possible and considered as the primary home (domicile) for the person there. Data on this was collected to extract the main home in the adult life of the person, by taking the place where they spent most of their life from the age of 21.

### Adding geo-locations to biographical data

To the data on adult life homes (domiciles), geographical information was added based on EKI placename database, which includes also smaller and historical names.The matches require sensibility checks to be done, however the general quality of the matches seemed enough for simple analyses.


## Linking to ENB

The links to ENB general registry have been made manually based on matching full names and matching birthdates, if available. The hand-annotated file is given in "data/raw/linking_data/isik_manual_linking4.csv". The data columns to use there are coauthor, set and id, data can be added based on these links. The file that combines all the links is given in data/links_allcoauthors.csv, using columns coauthor (ENB name), all_isiks (ISIK and VEPER codes separated by space), all_viafs (all matched viaf codes, but just one for each here), localid (local id for the dataset).


## Places to improve the dataset

There are a number of places where the dataset could be improved.
- Some fields are poorly aligned. If someone is analysing marriages, family life or occupations, aligning the main data file a bit better can help.
- Geo-locations are currently done via simple mergers based on the name only, and based on two different databases Geonames for birthplaces and EKI Placenames for domiciles. Using just one database for this could improve the data. Best would be to seek to consolidate the two databases as well as checking the locations for accuracy on the map.
- Educational and employment episodes are automatically extracted and classified. Manual checking will improve their accuracy if exact journeys of individuals are important. Finding placenames from these episodes may be improved by e.g. locating particular institutions on the map.


## Citation and license

The structured dataset is meant for public use. It has been created for Tinits, Peeter 2023. Stratified Historical Corpus of Estonian 1800–1940. Eesti Rakenduslingvistika Ühingu Aastaraamat 19, 175-194. doi:10.5128/ERYa19.11, which can be cited if you appreciate this dataset.

The creative content here is licensed under the [Creative Commons Attribution 4.0 International](https://creativecommons.org/licenses/by/4.0/) license (CC-BY-4.0), and the source code is licensed under the [MIT](https://choosealicense.com/licenses/mit/) license. The contents of the ISIK and VEPER archives belong to their respective caretakes at the Literary Museum of Estonia and Tallinn University.
