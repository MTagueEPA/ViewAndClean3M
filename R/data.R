#' File IDs
#'
#' The first is the fileids dataset.  This is used by the document pulling
#' functionality, to map document slugs identified to Clowder ids, to allow for
#' pulling the pdf from Clowder.
#'
#' @name fileids
#' @keywords dataset
#' @docType data
#' @format A data frame containing 6 columns, including document name (and thus slug),
#' Clowder id, size, and more.
"fileids"

#' Filedf
#'
#' The other two datasets are the OCR outputs for the pdf files.  Both are data
#' frames containing document slugs and string outputs, to be used by the document
#' search function.  The filedf contains just this.
#'
#' @name filedf
#' @keywords dataset
#' @docType data
#' @format A data frame containing 2 columns - document slug, and string copy of
#' the OCR output for the document
"filedf"

#' Pagedf
#'
#' OCR outputs for the pdf files.  Contains a third column for page number, with
#' string outputs split by page.
#'
#' @name pagedf
#' @keywords dataset
#' @docType data
#' @format A data frame containing 3 columns - document slug, page number, and
#' string copy of the OCR output for the document
"pagedf"

#' Chems
#'
#' Chemical dictionary linking terms which define the same chemical.  For example,
#' PFOA, perfluorooctanoate, and C-8 are all listed in the same row
#'
#' @name chems
#' @keywords dataset
#' @docType data
#' @format A data frame containing 4 columns - a core abbreviation and full name,
#' then all other abbreviations used in the 3M docset, then all other full names.
"chems"

#' DCAPdf
#'
#' original data frame of requested docs pulled from toxval
#' @name DCAPdf
#' @keywords dataset
#' @docType data
#' @format Data frame containing 6 columns - dtxsid, parent dtxsid, source hash,
#' source, document name, and url
"DCAPdf"

#' DCAPfiledf
#'
#' @name DCAPfiledf
#' @keywords dataset
#' @docType data
"DCAPfiledf"

#' dtxsidf
#'
#' @name dtxsidf
#' @keywords dataset
#' @docType data
"dtxsidf"

#' zipdf
#'
#' @name zipdf
#' @keywords dataset
#' @docType data
"zipdf"

#' atsdr_mrls_extraction_20240101
#'
#' @name atsdr_mrls_extraction_20240101
#' @keywords dataset
#' @docType data
"atsdr_mrls_extraction_20240101"

#' Carcinogenicity_03272024
#'
#' @name Carcinogenicity_03272024
#' @keywords dataset
#' @docType data
"Carcinogenicity_03272024"

#' Chemical_Rev_History_20230509
#'
#' @name Chemical_Rev_History_20230509
#' @keywords dataset
#' @docType data
"Chemical_Rev_History_20230509"

#' chemicals_details_20230509
#'
#' @name chemicals_details_20230509
#' @keywords dataset
#' @docType data
"chemicals_details_20230509"

#' Copper_Data_Entry_Final
#'
#' @name Copper_Data_Entry_Final
#' @keywords dataset
#' @docType data
"Copper_Data_Entry_Final"

#' DevelopmentalToxicityTeratogenicity_03262024
#'
#' @name DevelopmentalToxicityTeratogenicity_03262024
#' @keywords dataset
#' @docType data
"DevelopmentalToxicityTeratogenicity_03262024"

#' ECOTOX_chemical_mapping_20220721
#'
#' @name ECOTOX_chemical_mapping_20220721
#' @keywords dataset
#' @docType data
"ECOTOX_chemical_mapping_20220721"

#' ECOTOX_dictionary_20240530
#'
#' @name ECOTOX_dictionary_20240530
#' @keywords dataset
#' @docType data
"ECOTOX_dictionary_20240530"

#' ECOTOX_terr_HH_export_30May2024
#'
#' @name ECOTOX_terr_HH_export_30May2024
#' @keywords dataset
#' @docType data
"ECOTOX_terr_HH_export_30May2024"

#' hawc_original_12_06_21
#'
#' @name hawc_original_12_06_21
#' @keywords dataset
#' @docType data
"hawc_original_12_06_21"

#' hawc_pfas_150_doses3
#'
#' @name hawc_pfas_150_doses3
#' @keywords dataset
#' @docType data
"hawc_pfas_150_doses3"

#' hawc_pfas_150_groups3
#'
#' @name hawc_pfas_150_groups3
#' @keywords dataset
#' @docType data
"hawc_pfas_150_groups3"

#' hawc_pfas_150_raw3
#'
#' @name hawc_pfas_150_raw3
#' @keywords dataset
#' @docType data
"hawc_pfas_150_raw3"

#' hawc_pfas_430_doses3
#'
#' @name hawc_pfas_430_doses3
#' @keywords dataset
#' @docType data
"hawc_pfas_430_doses3"

#' hawc_pfas_430_groups3
#'
#' @name hawc_pfas_430_groups3
#' @keywords dataset
#' @docType data
"hawc_pfas_430_groups3"

#' hawc_pfas_430_raw3
#'
#' @name hawc_pfas_430_raw3
#' @keywords dataset
#' @docType data
"hawc_pfas_430_raw3"

#' HealthCanada_TRVs_2010_AppendixA_v2
#'
#' @name HealthCanada_TRVs_2010_AppendixA_v2
#' @keywords dataset
#' @docType data
"HealthCanada_TRVs_2010_AppendixA_v2"

#' hess_6_16_21
#'
#' @name hess_6_16_21
#' @keywords dataset
#' @docType data
"hess_6_16_21"

#' HPVIS_Mammalian_Acute_20191220
#'
#' @name HPVIS_Mammalian_Acute_20191220
#' @keywords dataset
#' @docType data
"HPVIS_Mammalian_Acute_20191220"

#' HPVIS_Mammalian_Devtox_20191220
#'
#' @name HPVIS_Mammalian_Devtox_20191220
#' @keywords dataset
#' @docType data
"HPVIS_Mammalian_Devtox_20191220"

#' HPVIS_Mammalian_Reprotox_20191220
#'
#' @name HPVIS_Mammalian_Reprotox_20191220
#' @keywords dataset
#' @docType data
"HPVIS_Mammalian_Reprotox_20191220"

#' HPVIS_Mammalian_Repeat_Dose_20200305
#'
#' @name HPVIS_Mammalian_Repeat_Dose_20200305
#' @keywords dataset
#' @docType data
"HPVIS_Mammalian_Repeat_Dose_20200305"

#' Neurotoxicity_03272024
#'
#' @name Neurotoxicity_03272024
#' @keywords dataset
#' @docType data
"Neurotoxicity_03272024"

#' PFAS_150_SEM_chemicals
#'
#' @name PFAS_150_SEM_chemicals
#' @keywords dataset
#' @docType data
"PFAS_150_SEM_chemicals"

#' PFAS_150_SEM_HERO_ID_vs_citation
#'
#' @name PFAS_150_SEM_HERO_ID_vs_citation
#' @keywords dataset
#' @docType data
"PFAS_150_SEM_HERO_ID_vs_citation"

#' PFAS_150_SEM_results
#'
#' @name PFAS_150_SEM_results
#' @keywords dataset
#' @docType data
"PFAS_150_SEM_results"

#' pprtv_cphea_Estimate_of_Carcinogenic_Risk_from_Inhalation_Exposure
#'
#' @name pprtv_cphea_Estimate_of_Carcinogenic_Risk_from_Inhalation_Exposure
#' @keywords dataset
#' @docType data
"pprtv_cphea_Estimate_of_Carcinogenic_Risk_from_Inhalation_Exposure"

#' pprtv_cphea_Estimate_of_Carcinogenic_Risk_from_Oral_Exposure
#'
#' @name pprtv_cphea_Estimate_of_Carcinogenic_Risk_from_Oral_Exposure
#' @keywords dataset
#' @docType data
"pprtv_cphea_Estimate_of_Carcinogenic_Risk_from_Oral_Exposure"

#' pprtv_cphea_Estimate_of_Chronic_RFC_for_Inhalation_Exposure
#'
#' @name pprtv_cphea_Estimate_of_Chronic_RFC_for_Inhalation_Exposure
#' @keywords dataset
#' @docType data
"pprtv_cphea_Estimate_of_Chronic_RFC_for_Inhalation_Exposure"

#' pprtv_cphea_Estimate_of_Chronic_RFD_for_Oral_Exposure
#'
#' @name pprtv_cphea_Estimate_of_Chronic_RFD_for_Oral_Exposure
#' @keywords dataset
#' @docType data
"pprtv_cphea_Estimate_of_Chronic_RFD_for_Oral_Exposure"

#' pprtv_cphea_Weight_of_Evidence_for_Cancer
#'
#' @name pprtv_cphea_Weight_of_Evidence_for_Cancer
#' @keywords dataset
#' @docType data
"pprtv_cphea_Weight_of_Evidence_for_Cancer"

#' pprtv_cphea_chemicals_20230123
#'
#' @name pprtv_cphea_chemicals_20230123
#' @keywords dataset
#' @docType data
"pprtv_cphea_chemicals_20230123"

#' pprtv_cphea_full
#'
#' @name pprtv_cphea_full
#' @keywords dataset
#' @docType data
"pprtv_cphea_full"

#' RepeatedDoseToxicityOral_03262024
#'
#' @name RepeatedDoseToxicityOral_03262024
#' @keywords dataset
#' @docType data
"RepeatedDoseToxicityOral_03262024"

#' RfC_Toxicity_Values_20230509
#'
#' @name RfC_Toxicity_Values_20230509
#' @keywords dataset
#' @docType data
"RfC_Toxicity_Values_20230509"

#' RfD_Toxicity_Values_20230509
#'
#' @name RfD_Toxicity_Values_20230509
#' @keywords dataset
#' @docType data
"RfD_Toxicity_Values_20230509"

#' source_atsdr_mrls_manual_pod
#'
#' @name source_atsdr_mrls_manual_pod
#' @keywords dataset
#' @docType data
"source_atsdr_mrls_manual_pod"

#' source_iris_summary_curation
#'
#' @name source_iris_summary_curation
#' @keywords dataset
#' @docType data
"source_iris_summary_curation"

#' source_ntp_pfas_manual_TOX_96_0_awebb01_20230810_revised
#'
#' @name source_ntp_pfas_manual_TOX_96_0_awebb01_20230810_revised
#' @keywords dataset
#' @docType data
"source_ntp_pfas_manual_TOX_96_0_awebb01_20230810_revised"

#' source_ntp_pfas_manual_TOX_97_0_awebb01_20230810_revised
#'
#' @name source_ntp_pfas_manual_TOX_97_0_awebb01_20230810_revised
#' @keywords dataset
#' @docType data
"source_ntp_pfas_manual_TOX_97_0_awebb01_20230810_revised"

#' source_ntp_pfas_manual_TOX_97_1_awebb01_20230810_revised
#'
#' @name source_ntp_pfas_manual_TOX_97_1_awebb01_20230810_revised
#' @keywords dataset
#' @docType data
"source_ntp_pfas_manual_TOX_97_1_awebb01_20230810_revised"

#' source_pprtv_cphea_summary_curation
#'
#' @name source_pprtv_cphea_summary_curation
#' @keywords dataset
#' @docType data
"source_pprtv_cphea_summary_curation"

#' ToxicityReproduction_03272024
#'
#' @name ToxicityReproduction_03272024
#' @keywords dataset
#' @docType data
"ToxicityReproduction_03272024"

#' toxref_toxval_pod_11APR2024
#'
#' @name toxref_toxval_pod_11APR2024
#' @keywords dataset
#' @docType data
"toxref_toxval_pod_11APR2024"

#' toxref_toxval_pod_13AUG2024
#'
#' @name toxref_toxval_pod_13AUG2024
#' @keywords dataset
#' @docType data
"toxref_toxval_pod_13AUG2024"

#' WOE_Details_20230509
#'
#' @name WOE_Details_20230509
#' @keywords dataset
#' @docType data
"WOE_Details_20230509"

#' WOE_Tox_Vals_20230509
#'
#' @name WOE_Tox_Vals_20230509
#' @keywords dataset
#' @docType data
"WOE_Tox_Vals_20230509"
