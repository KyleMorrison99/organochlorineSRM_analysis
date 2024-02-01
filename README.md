# Sixty years since Silent Spring: a map of meta-analyses on organochlorine pesticides reveals urgent needs for improving methodological quality

The repository described below contains the data, bibliometric files, and code used for this study. A pre-compiled file is also provided:

Please find the detailed description of the folders and their content below. Should you wish to reuse any data or analysis for future studies, kindly contact Kyle Morrison at kyle.morrison@unsw.edu.au.


## literature_search 
This folder contains all the bibliometric files created during the literature search. This folder contains the following four sub-folders:
- `ocp_srm_literature_database_search`: Contains bibliographic files from literature database searches.
- `ocp_srm_grey_literature_search`: Contains bibliographic files from grey literature searches.
- `ocp_srm_backward_forward_search`: Contains bibliographic files from backward and forward citation searches.
- `ocp_srm_all_search_combined`: Contains all search files combined with an R code for duplicate removal.

### ocp_srm_literature_database_search
- `ocp_srm_search_scopus.bib`: Bibliographic file with results from the Scopus literature search.
- `ocp_srm_search_wos_1_to_1000.bib`: Bibliographic file with results 1 to 1000 from the Web of Science literature search.
- `ocp_srm_search_wos_1000_to_1075.bib`: Bibliographic file with results 1000 to 1075 from the Web of Science literature search.
- `ocp_srm_search_pubmed.bib`: Bibliographic file with results from the PubMed literature search.
- `ocp_srm_search_pubmed_mesh.bib`: Bibliographic file with results from the PubMed literature search using MeSH terms.
- `ocp_srm_search_sciencedirect.bib`: Bibliographic file with results from the ScienceDirect literature search.
- `ocp_srm_search_cochrane.bib`: Bibliographic file with results from the Cochrane literature search.

### ocp_srm_grey_literature_search
The grey literature search was conducted on the Bielefeld Academic Search Engine (BASE). Files in this directory contain:
- `ocp_srm_grey_search_base1.bib`: Bibliographic file containing the first 10 studies retrieved from BASE.
- `ocp_srm_grey_search_base2.bib`: Bibliographic file containing studies 11-20 retrieved from BASE.
- `ocp_srm_grey_search_base3.bib`: Bibliographic file containing studies 21-30 retrieved from BASE.
- `ocp_srm_grey_search_base4.bib`: Bibliographic file containing studies 31-40 retrieved from BASE.

### ocp_srm_backward_forward_search
This subdirectory contains bibliographic files with studies that cite specific works, including:
- `ocp_srm_backward_belou.bib`: Contains all studies citing Belou et al., 2016 at the time of search.
- `ocp_srm_backward_burns.bib`: Contains all studies citing Burns and Juberg, 2021 at the time of search.
- `ocp_srm_backward_iqbal.bib`: Contains all studies citing Iqbal et al., 2022 at the time of search.
- `ocp_srm_backward_mentis.bib`: Contains all studies citing Mentis et al., 2021 at the time of search.
- `ocp_srm_backward_onyije.bib`: Contains all studies citing Onyije et al., 2022 at the time of search.
- `ocp_srm_backward_rojas.bib`: Contains all studies citing Rojas et al., 2021  at the time of search.
- `ocp_srm_search_combined_forward.bib`: Contains all studies that the above-cited studies reference.

## literature_screen
This directory houses bibliographic files used for title, abstract, and keyword screening, along with full-text screening:
- `ocp_srm_abstract_screening.bib`: bibliographic file containing all studies for title, abstract and keyword screening.
- `ocp_srm_full_text_screening.bib`: bibliographic file containing all studies for full-text screening.
- `ocp_srm_included.bib`: bibliographic file containing all studies inlcuded in the systematic map. 

## data
This directory encompasses all the data procured during the data extraction phase. It comprises the following files:
- `ocp_srm_impact_details.csv`: This file contains all the extracted data on the impacts of organochlorine pesticides.
- `ocp_srm_ocp_details.csv`: This file contains all of the organochlorines pesticides synthesized in each meta-analysis.
- `ocp_srm_species_details.csv`: This file contains all the species included in each meta-analyses synthesising studies on wildlife.
- `ocp_srm_study_details.csv`: This file contains all the details of the meta-analyses methodology including the critical appraisal.  
- `ocp_srm_subject_details.csv`: This file contains all the extracted data on the subjects exposed to organochlorine pesticides.
- `bib_sco.bib`: This bibliometric file includes all studies incorporated into the map for bibliometric analysis.

## R
This directory includes R code, markdown, and associated files. It contains the following:
- `organochlorineSRM_analysis`: This is the cache produced from knitting the Rmarkdown.
- `organochlorineSRM_analysis`: These are files generated from knitting the Rmarkdown.
- `organochlorineSRM_analysis.html`: This is the knitted analysis code.
- `organochlorineSRM_analysis.rmd`: This is the raw analysis code.

## figures
This directory stores all the created figures in both PDF and JPEG formats.
