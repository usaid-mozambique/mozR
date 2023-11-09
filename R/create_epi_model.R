#create the complete set of data.
#' Creates a dataset for the desired indicator where all values are mapped by age group, sex and PSNU.
#' Any values assigned to the age group 15+ are divided in to the other age categories based on the proportion
#' in each district or province.
#'
#' @param mer_data Data genie file in rds format
#' @param indicator_value one indicator from the data genie file
#' @param standard_disag relevant standarddisaggregates for the indicator
#' @param num_dem set to numerator as standard - numeratordenom field in datagenie
#' @param label Text name of the indicator
#'
#' @return a tibble with the indicator broken down by PSNU, sex and age.
#' @export
#'
#' @examples
#'  \dontrun{
#'    create_data(mer_data, "TX_CURR", "Age/Sex/HIVStatus", "N", "TX_CURR" )
#'  }

create_data <- function(mer_data, indicator_value, standard_disag, num_dem, label){
  data <- clean_mer(mer_data, indicator_value, standard_disag, num_dem)
  data_1 <- scenario_1(data)
  data_2 <- scenario_2(data)
  data_3 <- scenario_3(data)

  data_complete <- data_1 %>%
    dplyr::union_all(data_2) %>%
    dplyr::union_all(data_3) %>%
    dplyr::group_by(snu1, snu1uid, psnu, psnuuid, sex, age) %>%
    dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(indicator = label)
}


#' A tibble with desired indicator grouped by PSNU and with age bands matching the age bands
#' in spectrum.
#'
#' @param mer_data Data genie file in rds format
#' @param indicator_value one indicator from the data genie file
#' @param standard_disag relevant standarddisaggregates for the indicator
#' @param num_dem set to numerator as standard - numeratordenom field in datagenie
#'
#' @return A tibble with desired indicator grouped by PSNU and with age bands matching the age bands
#' in spectrum.
#' @export
#'
#' @examples
#' \dontrun{
#'    clean_mer(mer_data, "TX_CURR", "Age/Sex/HIVStatus", "N")
#'  }


clean_mer <- function(mer_data, indicator_value, standard_disag, num_dem){

  mer <- mer_data %>%
    dplyr::filter(standardizeddisaggregate %in% standard_disag,
                  indicator %in% indicator_value,
                  numeratordenom == num_dem) %>%
    dplyr::group_by(snu1, snu1uid, psnu, psnuuid, sex, ageasentered, numeratordenom, age_group_type) %>%
    dplyr::rename(value = VAL_QUARTER, age = ageasentered)%>%
    dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = 'drop') %>%
    dplyr::select(-numeratordenom)
}


#' Creates a dataset for the indicator where the spectrum age bands are known. Any age bands without a value
#' are removed.
#'
#' @param indicator_data data for one indicator that has already been cleaned by clean_mer()
#'
#' @return A tibble with values for desired indicator for all ages except <15 and 15+
#' @export
#'
#' @examples
#'  \dontrun{
#'    scenario_1(data_1)
#'  }
scenario_1 <- function(indicator_data){
  #step 1:  create dataset with semi-detailed age groups
  indicator_model <- indicator_data %>%
    dplyr::filter(age != "<15", age !="15+",
                  value >0
    )

}

#' Creates an estimate per age band for districts that report values for <15 or 15+.  The estimate is created using
#' the total values for each age band per district. (Scenario 2)
#'
#' @param indicator_data data for one indicator that has already been cleaned by clean_mer()
#'
#' @return Tibble with an estimated value for any districts that use <15 or 15+ using PSNU proportion
#' @export
#'
#' @examples
#' \dontrun{
#'    scenario_2(data_2)
#'  }
scenario_2 <- function(indicator_data){

  # 15+ and <15 already excluded from scenario_1 to create proportion dataset
  proportion <- scenario_1(indicator_data)

  #create a table of totals per PSNU and sex
  psnu_temp <- proportion %>%
    dplyr::group_by(psnuuid, sex, age_group_type) %>%
    dplyr::summarise(psnu_total = sum(value, na.rm = TRUE), .groups = 'drop')

  #calculate the proportion based on the total table and individual results
  det_total <- proportion %>%
    dplyr::left_join(psnu_temp, by = c("psnuuid", "sex", "age_group_type")) %>%
    dplyr::mutate(psnu_percentage = value / psnu_total)

  #Create expected result for coarse age data using psnu proportion
  indicator_temp <- indicator_data %>%
    dplyr::filter(age %in% c("<15","15+"),
                  value > 0
    ) %>%
    dplyr::select( -c(age, snu1, snu1uid, psnu)) %>%
    dplyr::rename(coarse_value = value) %>%
    dplyr::left_join(det_total, by = c("psnuuid", "sex", "age_group_type")) %>%
    dplyr::mutate(est_value = coarse_value * psnu_percentage) %>%
    dplyr::filter(!is.na(est_value)) %>%
    dplyr::select(-c( value, psnu_total, psnu_percentage)) %>%
    dplyr::rename(value = est_value) %>%
    tidyr::drop_na() %>%
    dplyr::select(-coarse_value)

}



#' Creates an estimate per age band for districts that report values for 15+.  The estimate is created using
#' the total values for each age band per province. (Scenario 3)
#'
#' @param indicator_data data for one indicator that has already been cleaned by clean_mer()
#'
#' @return Tibble with an estimated value for any districts that use 15+ using SNU proportion
#' @export
#'
#' @examples
#'  \dontrun{
#'    scenario_3(data_3)
#'  }

scenario_3 <- function(indicator_data){

  #returns a list of a results based on semi-detailed age group
  proportion <- scenario_1(indicator_data)

  # create a temp table that includes the total value per SNU, Age and Sex
  temp_snu_age <- proportion %>%
    dplyr::group_by(snu1uid, sex, age, age_group_type) %>%
    dplyr::summarise(total_snu_age = sum(value, na_rm = TRUE), .groups = "drop")

  # create a temp table that includes the total value per SNU and Sex and age_group_label
  temp_snu <- proportion %>%
    dplyr::group_by(snu1uid, sex, age_group_type) %>%
    dplyr::summarise(total_snu = sum(value, na_rm = TRUE), .groups = "drop") %>%
    dplyr::distinct()

  #proportion at a SNU level for age and sex
  det_total <- temp_snu_age %>%
    dplyr::left_join(temp_snu, by = c("snu1uid", "sex", "age_group_type")) %>%
    dplyr::mutate(snu_percentage = total_snu_age / total_snu) %>%
    dplyr::select(-c("total_snu_age", "total_snu")) %>%
    dplyr::distinct()

  #List of unique PSNUs that are adults with detailed age groups
  psnu_list <- proportion %>%
    dplyr::select(psnuuid, age_group_type) %>%
    dplyr::distinct()

  #keep only relevant districts and create estimates
  indicator_temp <- indicator_data %>%
    dplyr::filter(age %in% c("<15","15+"),
                  value > 0
    ) %>%
    dplyr::select(-c("age")) %>%
    #remove rows where data exists at a PSNU level
    dplyr::anti_join(psnu_list, by = "psnuuid") %>%
    dplyr::rename(coarse_value = value) %>%

    # This join adds one line per age group.  SNU appears multiple times in both tables
    dplyr::left_join(det_total, by = c("snu1uid", "sex", "age_group_type"), relationship = "many-to-many")  %>%
    dplyr::mutate(est_value = round(coarse_value * snu_percentage, digits = 0 )) %>%
    dplyr::select(-c("snu_percentage", coarse_value)) %>%
    dplyr::rename(value = est_value)


  #only process age groups IF 15+ data exists
  if (nrow(indicator_temp) == 0){
    return(indicator_temp)
  }

  else{
    return(indicator_temp)
  }

}
