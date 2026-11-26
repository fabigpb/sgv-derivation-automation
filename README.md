Data and Derivation Method for ad hoc Soil Guideline Values (SGV)

Ad-hoc Soil Guideline Values are risk-based reference values, according to the recommended methodology developed within the AP-PPP project (see data package under https://doi.org/10.25678/000FXW and the publication under https://www.dora.lib4ri.ch/eawag/item/eawag:37541), and do not have a regulatory nature that goes beyond their intended use within the ConSoil project (https://www.ecotoxcentre.ch/projects/soil-ecotoxicology/monitoring-concept-for-plant-protection-products-in-soils).
To understand in which context the data tables and the calculator sheet (calculator_info_example.xlsx) are used, it is highly advised to read the publication  (https://doi.org/10.55408/eawag:37541) and the associated two guidance documents (one contained as PDF in the previous noted data package other available under https://www.ecotoxcentre.ch/media/czdbwdbi/2023_consoil_sgv_report_part_2.pdf). Due to reasons of data protection the collected toxicity data for SGV derivation are not included. 
Please see the R code itself or read the guidance documents to understand derivation process. The R code and any updates to it are made available here.
In the following, the content of various files are explained.
For more information consult the data package under https://doi.org/10.25678/000FXW.

R code

ad_hoc_sgv_pub.R:
This script derives the ad hoc sgv according to the guideline described in https://doi.org/10.25678/000FXW and makes use of the collected toxicity data.

eu_endpt_ad_hoc_sgv_pub.R:
This script derives the screened ad hoc SGV according to the guideline decribed in Chapter 9 in "Ad hoc SGV guideline.pdf" available under https://doi.org/10.25678/000FXW and makes use of the collected toxicity data.
