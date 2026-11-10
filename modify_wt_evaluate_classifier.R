##modify the wildrtrax function to evaluate classifiers on a species by 
## species basis


library(wildrtrax)

##example project to test
## need to add code to be able to pull lots of projects and still ahve the 
## function work

data <- wt_download_report(project_id = 3749,
                           sensor_id = "ARU",
                           reports = c("main", "ai"))

## modifications start at line 105, at the filter to species of interest step
wt_evaluate_classifier_spp <- function(data, resolution = NULL, remove_species = TRUE,  species = NULL, thresholds = c(0.01, 0.99)){
  
  # Check if the data object is in the right format
  if (!inherits(data, "list") && !grepl("ai", names(data)[[2]]) && !grepl("main", names(data))[[1]]) {
    stop("The input should be the output of the `wt_download_report()` function with the argument `reports=c('main', 'ai')`")
  }
  
  # Check if the project has the correct transcription method for evaluation method chosen
  method <- data[[2]]$task_method[1]
  if(method=="NONE"){
    stop("The `wt_evaluate_classifier()` function only works on recordings processed with the '1SPT' or '1SPM' methods")
  }
  if(method=="1SPT" & resolution=="minute"){
    stop("You can only evaluate at the minute resolution for recordings that have been processed with the '1SPM' method")
  }
  
  # Get the classifier report and filter species as requested
  if(remove_species==TRUE){
    class <- data[[1]] |>
      filter(is_species_allowed_in_project==TRUE)
  } else {
    class <- data[[1]]
  }
  
  # Summarize the classifier report to the requested resolution
  if(resolution=="task"){
    
    class <- class |>
      inner_join(data[[2]] |> select(recording_id, task_id) |> distinct(), by = "recording_id")
    
    detections <- class |>
      rename(start_s = detection_time) |>
      inner_join(data[[2]] |> select(recording_id, task_id, task_duration) |> distinct(), by = c("recording_id" = "recording_id", "task_id" = "task_id")) |>
      filter(!start_s > task_duration) |>
      group_by(location_id, recording_id, task_id, version, species_common_name) |>
      summarise(confidence = max(confidence), .groups="keep") |>
      ungroup() |>
      mutate(classifier = 1)
  } else if(resolution=="minute"){
    detections <- class |>
      mutate(minute = ifelse(detection_time==0, 1, ceiling(detection_time/60))) |>
      group_by(location_id, recording_id, version, species_common_name, minute) |>
      summarise(confidence = max(confidence), .groups="keep") |>
      ungroup() |>
      mutate(classifier = 1)
  } else if(resolution=="recording"){
    detections <- class |>
      group_by(location_id, recording_id, version, species_common_name) |>
      summarise(confidence = max(confidence), .groups="keep") |>
      ungroup() |>
      mutate(classifier = 1)
  } else { stop("A resolution was not specified.")}
  
  # Tidy up the main report
  if(resolution=="task"){
    main <- wt_tidy_species(data[[2]], remove=c("mammal", "amphibian", "abiotic", "insect", "human", "unknown")) |>
      select(location_id, recording_id, task_id, species_common_name) |>
      unique() |>
      mutate(human = 1)
  } else if(resolution=="minute"){
    main <- wt_tidy_species(data[[2]], remove=c("mammal", "amphibian", "abiotic", "insect", "human", "unknown")) |>
      mutate(minute = ifelse(detection_time==0, 1, ceiling(detection_time/60))) |>
      select(location_id, recording_id, species_common_name, minute) |>
      unique() |>
      mutate(human = 1)
  } else if(resolution=="recording"){
    main <- wt_tidy_species(data[[2]], remove=c("mammal", "amphibian", "abiotic", "insect", "human", "unknown")) |>
      select(location_id, recording_id, species_common_name) |>
      unique() |>
      mutate(human = 1)
  } else {stop("A resolution was not specified.")}
  
  # Join together
  if (resolution == "minute") {
    both <- full_join(detections, main, by=c("location_id", "recording_id", "species_common_name", "minute")) |>
      mutate(human = ifelse(is.na(human), 0, 1),
             classifier = ifelse(is.na(classifier), 0, 1),
             tp = ifelse(classifier==1 & human==1, 1, 0),
             fp = ifelse(classifier==1 & human==0, 1, 0),
             fn = ifelse(classifier==0 & human==1, 1, 0))
  } else {
    both <- full_join(detections, main, by=c("location_id", "recording_id", "species_common_name")) |>
      mutate(human = ifelse(is.na(human), 0, 1),
             classifier = ifelse(is.na(classifier), 0, 1),
             tp = ifelse(classifier==1 & human==1, 1, 0),
             fp = ifelse(classifier==1 & human==0, 1, 0),
             fn = ifelse(classifier==0 & human==1, 1, 0))
  }
  
  # Filter to species of interest (default: every species in the main report)
  if(is.null(species)){
    species <- unique(main$species_common_name)
  }
  both <- filter(both, species_common_name %in% species)
  
  # Total number of human detections, now per classifier AND species
  human_totals <- both |>
    group_by(version, species_common_name) |>
    summarise(human_total = sum(human, na.rm = TRUE), .groups = "drop")
  
  thresholds_vec <- seq(thresholds[1], thresholds[2], 0.01)
  
  # Calculate metrics per classifier x species
  prf_combined <- both |>
    left_join(human_totals, by = c("version", "species_common_name")) |>
    filter(!is.na(version)) |>
    group_split(version, species_common_name) |>
    map_dfr(function(df) {
      map_dfr(thresholds_vec, ~ wildrtrax:::.wt_calculate_prf(threshold = .x, data = df,
                                                              human_total = unique(df$human_total))) |>
        mutate(classifier = unique(df$version),
               species_common_name = unique(df$species_common_name))
    }) |>
    distinct()
  
  return(prf_combined)
}


eval_sp <- wt_evaluate_classifier_spp(data, resolution = "task",
                                      thresholds = c(0.1, 0.99))

names(eval_sp)

eval_long <- eval_sp |> 
  pivot_longer(any_of(c("precision", "recall", "f_score", "fscore")),
               names_to = "metric", values_to = "value")

spp_list <-   group_split(eval_long, species_common_name)
names(spp_list) <- map_chr(spp_list, \(d) d$species_common_name[1])

##need to make the plots prettier and add smoother
plots <- map(spp_list, \(d) 
             ggplot(d, aes(threshold, value, colour = metric, linetype = classifier)) +
               geom_line() + ylim(0,1) + 
               labs(title = d$species_common_name[1]) + theme_bw()) 

plots$`Baird's Sparrow` ##yikes need more data :)