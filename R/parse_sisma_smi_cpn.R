#' A specific helper function for parsing a cleaned CSV export from SISMA
#'
#' @param file Dataframe cleaned via reshape_sisma
#'
#' @return A tidy format of SISMA cpn dataframe
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- parse_sisma_smi_cpn()}


parse_sisma_smi_cpn <- function(file) {

  df <- file %>%

    dplyr::filter(!is.na(value)) %>%

    dplyr::mutate(

      indicator_temp = dplyr::case_when(

        # consultation feature engineering
        stringr::str_detect(indicator, "_anos") ~ "MG_CON1_MES",
        stringr::str_detect(indicator, "12_semanas") ~ "MG_CON1_12SEM_MES",
        stringr::str_detect(indicator, "total_da_coorte") ~ "MG_CON1_COORTE",
        stringr::str_detect(indicator, "4_ou_mais_consultas") ~ "MG_CON4_COORTE",

        # hiv feature engineering
        stringr::str_detect(indicator, "parceiros_presentes") ~ "PARCEIRO_PRESENTE",
        stringr::str_detect(indicator, "parceiros_testado") ~ "PARCEIRO_HIV_TESTADO",
        stringr::str_detect(indicator, "iniciaram_ctz|ctz_a_entrada") ~ "MG_HIV_CTZ",
        stringr::str_detect(indicator, "positiva_a_entrada|gravidas_testadas_hiv") ~ "MG_HIV_ESTADO",
        stringr::str_detect(indicator, "tarv_a_entrada|iniciaram_tarv|monoprofilaxia|biprofilaxia") ~ "MG_HIV_POS_ARV",

        # nutrition feature engineering
        stringr::str_detect(indicator, "ganho") ~ "MG_NUT_GANHO_2TRI",
        stringr::str_detect(indicator, "aguda_grave") ~ "MG_NUT_DAG",
        stringr::str_detect(indicator, "aguda_moderada") ~ "MG_NUT_DAM",
        stringr::str_detect(indicator, "suplementos") ~ "MG_NUT_SUPLEM",
        stringr::str_detect(indicator, "curadas") ~ "MG_NUT_DESNUT_CUR",
        stringr::str_detect(indicator, "abandonaram") ~ "MG_NUT_DESNUT_ABND",
        stringr::str_detect(indicator, "desparasitante") ~ "MG_NUT_DESPARA",
        stringr::str_detect(indicator, "ferroso") ~ "MG_NUT_SALFER_3D",

        # malaria feature engineering
        stringr::str_detect(indicator, "2_doses_de_tip") ~ "MG_MAL_TIP_2DOS",
        stringr::str_detect(indicator, "4_ou_mais_doses") ~ "MG_MAL_TIP_4DOS",
        stringr::str_detect(indicator, "remtil") ~ "MG_MAL_REMTIL",
        stringr::str_detect(indicator, "diagnostico_laboratorial") ~ "MG_MAL_DIAG",
        stringr::str_detect(indicator, "tratamento_para_malaria") ~ "MG_MAL_DIAG_TX",

        # its feature engineering
        stringr::str_detect(indicator, "diagnostico_sindromico") ~ "MG_ITS_DIAG", # ok
        stringr::str_detect(indicator, "tratamento_sindromico") ~ "MG_ITS_DIAG_TX", # ok
        stringr::str_detect(indicator, "3a_dose_de_tratamento_de_sifilis") ~ "MG_ITS_SIF_3DOS", # ok
        stringr::str_detect(indicator, "para_sifilis") ~ "MG_ITS_TESTADA_SIF", #
        stringr::str_detect(indicator, "receberam_tratamento_de_sifilis") ~ "PARCEIRO_SIF_TX",

        # tb feature engineering
        stringr::str_detect(indicator, "inh") ~ "MG_TB_INH",
        stringr::str_detect(indicator, "tuberculose") ~ "MG_TB_TX",

        # other feature engineering
        stringr::str_detect(indicator, "misoprostol") ~ "MG_HPP_MISOPROSTOL",
        stringr::str_detect(indicator, "1a_dose_vat") ~ "MG_VAT_1D",
        stringr::str_detect(indicator, "5a_dose_vat") ~ "MG_VAT_2_5D"),


      age = dplyr::case_when(
        stringr::str_detect(indicator, "10_14") ~ "10-14",
        stringr::str_detect(indicator, "15_19") ~ "15-19",
        stringr::str_detect(indicator, "20_24") ~ "20-24",
        stringr::str_detect(indicator, "_25_") ~ "25+"),


      disaggregate = dplyr::case_when(

        # hiv testing feature engineering
        stringr::str_detect(indicator, "hiv_positiva_a_entrada") ~ "Positivo a entrada",
        stringr::str_detect(indicator, "testadas_hiv_positivas") ~ "Positivo",
        stringr::str_detect(indicator, "testadas_hiv_negativas") ~ "Negativo",
        stringr::str_detect(indicator, "parceiros_testados_positivos") ~ "Positivo",
        stringr::str_detect(indicator, "parceiros_testados_negativos") ~ "Negativo",

        # pmtct arv feature engineering
        stringr::str_detect(indicator, "tarv_a_entrada") ~ "TARV a entrada",
        stringr::str_detect(indicator, "iniciaram_tarv") ~ "Inicio TARV",
        stringr::str_detect(indicator, "monoprofilaxia") ~ "Monoprofilaxia",
        stringr::str_detect(indicator, "biprofilaxia_nvp_azt") ~ "Biprofilaxia",

        # its feature engineering
        stringr::str_detect(indicator, "positivas_para_sifilis") ~ "Positivo",
        stringr::str_detect(indicator, "negativas_para_sifilis") ~ "Negativo",

        # ctz feature engineering
        stringr::str_detect(indicator, "iniciaram_ctz") ~ "Inicio CTZ",
        stringr::str_detect(indicator, "ctz_a_entrada") ~ "CTZ a entrada"),


      # calculate aligned period for 1st anc indicators
      period_cohort = dplyr::if_else(indicator %in% c("MG_1CON_MES", "MG_1CON_12SEM_MES"), period, period - months(6))

    ) %>%

    dplyr::select(starts_with("period"), snu, psnu, sitename, sisma_uid, indicator = indicator_temp, age, disaggregate, value)

}
