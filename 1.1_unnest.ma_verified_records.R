#' Unnest and Translate Mosquito Alert Verified Records
#'
#' This function takes a dataset of Mosquito Alert verified records (in sf or data.frame format),
#' filters it for Italy (country == "ITA"), and processes it by unnested responses and translated questions and answers.
#' The output is a cleaned and translated dataset, optionally saved to disk.
#'
#' @param records An df object or a data frame containing Mosquito Alert verified records, typically loaded from a JSON file.
#' @param out_rds Optional. Character string indicating the path to save the resulting dataset as an RDS file. If NULL, the file is not saved.
#'
#' @return A data frame or tibble containing the processed and translated records, ready for analysis.
#'
#' @details
#' This function:
#' \itemize{
#'   \item Converts input records to an sf object (if not already);
#'   \item Filters records for the country "ITA";
#'   \item Unnests nested response lists for each question;
#'   \item Expands multiple-answer fields into separate columns;
#'   \item Translates both question texts and answers into English (if applicable);
#'   \item Optionally saves the final dataset to disk as an RDS file.
#' }
#'
#' @examples
#' \dontrun{
#' # Load records from JSON (already flattened to a data.frame or sf)
#' records <- jsonlite::fromJSON("path/to/all_records2023.json", flatten = TRUE)
#' processed_data <- unnest_ma_verified_records(records, out_rds = "processed_data_2023.rds")
#' }
#'
#' @export

unnest_ma_verified_records <- function(
    records = NULL, 
    out_rds = NULL
    ) {
  
  ####LOADING PACKAGES USED####
  required_packages <- c("httr", "jsonlite", "dplyr", "tidyr", "sf", "purrr", "units", "lubridate")
  new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
  if(length(new_packages)) install.packages(new_packages)
  lapply(required_packages, library, character.only = TRUE)
  
  #Ensure records are in sf format
  if (!inherits(records, "sf")) {
    records <- records %>%
      sf::st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  }
  
  #Filter for Italy
  records <- records %>% dplyr::filter(country == "ITA")
  
  #Unnest responses
  resp_lists <- records$responses
  names(resp_lists) <- records$version_UUID
  responses <- purrr::imap_dfr(resp_lists, 
                               ~ .x %>% mutate(version_UUID = .y) %>% as_tibble()
  ) %>% dplyr::mutate(question_id = as.character(question_id))
  
  #Prepare records for joining
  records <- records %>% sf::st_drop_geometry() %>% dplyr::select(-responses)
  
  #Pivot and process responses
  responses <- responses %>%
    dplyr::group_by(version_UUID, question_id) %>%
    dplyr::summarise(
      translated_question = first(translated_question),
      answers = list(translated_answer),
      .groups = "drop"
    )
  
  responses_wide <- responses %>%
    tidyr::pivot_wider(
      id_cols = version_UUID,
      names_from = question_id,
      values_from = c(translated_question, answers),
      names_glue = "{.value}_{question_id}"
    ) %>%
    dplyr::rename_with(~ sub("^translated_question_(\\d+)$", "question_\\1", .x),
                tidyr::starts_with("translated_question_"))
  
  #Handle multiple answers (expand lists into separate columns)
  answer_cols <- grep("^answers_\\d+$", names(responses_wide), value = TRUE)
  for(col in answer_cols) {
    qnum <- sub("^answers_(\\d+)$", "\\1", col)
    temp <- responses_wide %>% tidyr::unnest_wider(all_of(col), names_sep = "_")
    new_cols <- dplyr::setdiff(names(temp), names(responses_wide))
    new_names <- paste0("answer_", qnum, "_", seq_along(new_cols))
    names(temp)[match(new_cols, names(temp))] <- new_names
    responses_wide <- temp
  }
  
  #Merge with records
  final <- records %>% dplyr::left_join(responses_wide, by = "version_UUID")
  
  #### TRANSLATION ####
  question_translations <- c(
    "question_1"  = "How many bites do you have and where are they on your body?",
    "question_2"  = "Show where you were bitten",
    "question_3"  = "What time was it?",
    "question_4"  = "Where were you when they bit you?",
    "question_5"  = "When were you bitten?",
    "question_6"  = "What type of mosquito do you think it was?",
    "question_7"  = "What does the mosquito look like?",
    "question_10" = "Is there water?",
    "question_12" = "What type of breeding site have you found in a public place?",
    "question_13" = "Where did you find the mosquito?",
    "question_17" = "Were there larvae in the breeding site?"
  )
  
  #Apply question translations only if columns exist
  for (col in names(question_translations)) {
    if (col %in% names(final)) {
      final[[col]] <- question_translations[[col]]
    }
  }
  
  #Translation dictionary for answers
  translations_en <- c(
    "Non saprei"       = "I don't know",
    "Zanzara comune"   = "Common mosquito",
    "Aedes aliena"     = "Invasive Aedes",
    "Mosquit comú"     = "Common mosquito",
    "In un edificio"   = "Inside a building",
    "In un veicolo"    = "Inside a vehicle",
    "In a building"    = "Inside a building",
    "Dins d'un edifici"= "Inside a building",
    "Torace 1"         = "Thorax 1",
    "Torace 4"         = "Thorax 4",
    "Addome 3"         = "Abdomen 3",
    "Zampa 4"          = "Leg 4",
    "Addome 1"         = "Abdomen 1",
    "Zampa 1"          = "Leg 1",
    "Zampa 3"          = "Leg 3",
    "Bal kar"                 = "Left arm",
    "Braç esquerre"           = "Left arm",
    "Braccio sinistro"        = "Left arm",
    "Braço esquerdo"          = "Left arm",
    "Left arm"                = "Left arm",
    "Δεξιά_χέρι"              = "Right arm",    
    "Braç dret"               = "Right arm",
    "Braccio destro"          = "Right arm",
    "Brazo derecho"           = "Right arm",
    "Jobb kar"                = "Right arm",
    "Right arm"               = "Right arm",
    "Bal láb"                 = "Left leg",
    "Gamba sinistra"          = "Left leg",
    "Piept"                   = "Chest",         
    "Brazo izquierdo"         = "Left arm",      
    "Gamba destra"            = "Right leg",
    "Jobb láb"                = "Right leg",
    "Left leg"                = "Left leg",
    "Right leg"               = "Right leg",
    "Brust/Bauch/Rücken"      = "Chest",
    "Chest"                   = "Chest",
    "Pecho"                   = "Chest",
    "Torace"                  = "Chest",
    "Poitrine"                = "Chest",
    "Гърди"                   = "Chest",
    "Head"                    = "Head",
    "Cap"                     = "Head",
    "Hoofd"                   = "Head",
    "Cabeza"                  = "Head",
    "Testa"                   = "Head",
    "Глава"                   = "Head",
    "Abends"                  = "Afternoon",
    "Délután"                 = "Afternoon",
    "Afternoon"               = "Afternoon",
    "Mañana"                  = "Morning",
    "Mattina"                 = "Morning",
    "Morning"                 = "Morning",
    "Mediodía"                = "Midday",
    "Mezzogiorno"             = "Midday",
    "Midday"                  = "Midday",
    "Nacht"                   = "Night",
    "Nachts"                  = "Night",
    "Night"                   = "Night",
    "All'aperto"              = "Outdoors",
    "A l'exterior"            = "Outdoors",
    "Outdoors"                = "Outdoors",
    "Dentro de un edificio"   = "Inside a building",
    "Inside a building"       = "Inside a building",
    "Dentro de un vehículo"   = "Inside a vehicle",
    "Inside a vehicle"        = "Inside a vehicle",
    "I don't know"            = "I don't know",
    "Ich weiß nicht"          = "I don't know",
    "Adesso"                  = "Just now",
    "Agora mesmo"             = "Just now",
    "Just now"                = "Just now",
    "Az elmúlt 24 órában"     = "In the last 24 hours",
    "In the last 24 hours"    = "In the last 24 hours",
    "Aedes invasor"           = "Invasive Aedes",
    "Invasive Aedes"          = "Invasive Aedes",
    "Common mosquito"         = "Common mosquito",
    "I don't know"            = "I don't know",
    "Abdomen 1"               = "Abdomen 1",
    "Thorax 1"                = "Thorax 1",
    "Leg 1"                   = "Leg 1",
    "Ja"                      = "Yes",
    "Sì"                      = "Yes",
    "Yes"                     = "Yes",
    "Gully"                   = "Gully",
    "Storm drain"             = "Storm drain",
    "Other"                   = "Other",
    "Outdoors"                = "Outdoors",
    "Inside a building"       = "Inside a building",
    "Inside a vehicle"        = "Inside a vehicle",
    "Nee"                     = "No",
    "Nein"                    = "No",
    "No"                      = "No"
  )
  
  #Apply answer translations only if columns exist
  all_answer_cols <- grep("^answer_\\d+_\\d+$", names(final), value = TRUE)
  for(col in all_answer_cols) {
    old <- as.character(final[[col]])
    if (!is.null(old)) {
      old_non_na <- old[!is.na(old)]
      miss <- setdiff(unique(old_non_na), names(translations_en))
      if (length(miss)) warning("Values not in dictionary: ", paste(miss, collapse = ", "))
      final[[col]] <- translations_en[old]
    }
  }
  
  ####SAVING####
  if (!is.null(out_rds)) saveRDS(final, out_rds)
  final
}

