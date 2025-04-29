This readme file was generated on 2025-04-29 by Luis Posadas Martinez

# GENERAL INFORMATION

Title: Molecular Hybrid Screening With Biallelic SNP Genotype Information

DOI: 10.5281/zenodo.15306649


PI: Luis Posadas Martinez Ph.D.
Research Assistant Professor &  Graduate Faculty Associate
University of Nebraska-Lincoln
202 Keim Hall
Department of Agronomy & Horticulture
University of Nebraska
Lincoln, NE 68583-0915

Date of data collection: January 2025

Place of data collection: Lincoln, Nebraska, USA


# SHARING/ACCESS INFORMATION

MIT open source license
All data in these files is property of the University of Nebraska, Lincoln. Use of these files is meant for educational purposes. 
Citation: cff-version: 1.2.0
title: >-
  Molecular Hybrid Screening With Biallelic SNP Genotype
  Information
message: University of Nebraska-Lincoln
type: software
authors:
  - given-names: LUIS
    family-names: POSADAS
    email: LPOSADAS2@UNL.EDU
    affiliation: UNIVERSITY OF NEBRASKA-LINCOLN
    orcid: 'https://orcid.org/0000-0002-1726-9407'
  - name: UNIVERSITY OF NEBRASKA-LINCOLN
    address: '1400 R St, Lincoln, NE 68588'
    city: LINCOLN
    country: US
    post-code: '68588'
    region: NORTH AMERICA



Keywords: molecular screening; fingerprinting; hybrids; genotype data; SNPs; single nucleotide polymorphism; biallelic genotypes, F1 generation

Language: R

Funding: Nebraska Soybean Board

# DATA & FILE OVERVIEW

Analyzed_F1_data_single_crosses.csv - Output of script with counts of genotype calls and classification into "true hybrid" (H), "true self" (S) or inconclusive calls (?)

Hybrid_Genotypes.csv: Biallelic SNP genotype data from purported F1 hybrid plants obtained from genotyping with KASP molecular assays

Parental_Genotypes.csv: Biallelic SNP genotype data from parental lines used for artificial pollinations that gave rise to the purported F1 hybrids obtained from genotyping with KASP molecular assays

Polymorphic_SNPs.csv: File with information regarding SNP polymorphisms between two parents representing a hybrid

Script_for_analizing_F1_genotyping_results_V3.R: R script to run the analysis of genotype data and output "Analyzed_F1_data_Small_Set_For_ManuscriptSingle_crosses_(NEW_RUN).csv" above

All files were created in April 2025.



File relationship:

Hybrid_Genotypes.csv, Parental_Genotypes.csv and Polymorphic_SNPs.csv files are inputs for script (Script_for_analizing_F1_genotyping_results_V3.R).

Analyzed_F1_data_single_crosses.csv is the output of said script. 



# METHODOLOGICAL INFORMATION

The analysis was conducted in R version 3.6.3 (2020-02-29) -- "Holding the Windsock"
Copyright (C) 2020 The R Foundation for Statistical Computing
Platform: x86_64-apple-darwin15.6.0 (64-bit)

The GUI used was RStudio
2024.12.1 Build 563

No additional packages other than the base packages and dev tools were installed. 

Genotype data was collected using KASP reactions that included KASP-TF V4.0 2X Master Mix 96/384, Standard ROX (2.5 mL) and a QuantStudio™ 7 Flex Real-Time PCR System, 384-well, desktop instrument.


# DATA-SPECIFIC INFORMATION FOR: 

	Analyzed_F1_data_single_crosses.csv

	101 variables
	33 records

	Hybrid_Genotypes.csv

	22 variables
	33 records

	Parental_Genotypes.csv

	22 variables
	33 records


	Polymorphic_SNPs.csv

	28 variables
	33 records


	Missing data: #N/A


