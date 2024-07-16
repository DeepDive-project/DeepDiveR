#' Build equal length time bins
#'
#' A function to create a table of time bins. The function is called within the
#' `time_bins()` function only in the case where `equal_bins` are specified by
#' the user. By default, 100 1My bins are generated.
#'
#' @param start \code{numeric}. Numerical age at which the oldest time bin
#'    should start. Default is `100`.
#' @param end \code{numeric}. Numerical age at which the youngest time bin
#'    should end. Default is `0`.
#' @param n \code{numeric}. Number of time bins to be made. Default is `100`.
#' @returns A `dataframe` containing the start, end, and midpoint ages of the
#'    generated time bins.
#'
#' @examples
#' # Generate 100 time bins between 100My and present
#' example1 <- build_bins()
#' # Generate 20 time bins between 10My and 5My
#' example2 <- build_bins(start = 10, end = 5, n = 20)
#' @export
build_bins <- function (start = 100, end = 0, n = 100) {

  # Handling errors
  if (!is.numeric(start) || !is.numeric(end)) {
    stop("`start` and/or `end` are not of numeric class")
  }

  if (end >= start) {
    stop(paste("Start value must be larger than end value"))
  }

  # Convert number of bins to bin length
  interval <- abs((start - end) / n)

  # Vector of bin boundary times
  bin_bound_seq <- seq(from = start, to = end, by = -interval)

  # Offset start and end times by 1 to build bins of required length
  bins <- data.frame(
    start = bin_bound_seq[-length(bin_bound_seq)],
    end = bin_bound_seq[-1]
  )

  # Add midpoints
  bins$midpoint <- rowMeans(bins)
  return(bins)
}


#' Build time bins using geological stages
#'
#' A function to create a table of time bins. The function is called within the
#' `time_bins()` function only in the case where `stages` are specified by the
#' user. By default, Cenozoic stages are returned.
#'
#' @param start \code{numeric}. Numerical age at which the oldest time bin
#'    should start. Default is `66`.
#' @param end \code{numeric}. Numerical age at which the youngest time bin
#'    should end. Default is `0`.
#' @returns A `dataframe` containing the start, end, and midpoint ages of the
#'    generated time bins.
#'
#' @examples
#' # Generate time bins for the Cenozoic
#' example1 <- build_stages()
#' # Generate time bins for the Mesozoic
#' example2 <- build_stages(start = 252, end = 66)
#' @export
build_stages <- function(start = 66, end = 0) {

  # Handling errors
  if (!is.numeric(start) || !is.numeric(end)) {
    stop("`start` and/or `end` are not of numeric class")
  }

  if (end >= start) {
    stop(paste("Start value must be larger than end value"))
  }

  # Load inbuilt table of geological stages
  bins <- geo_bins
  bins <- bins[3:6]

  # Trim to relevant stages
  bins <- bins[which(bins$start <= start & bins$end >= end), ]

  return(bins)
}


#' Build time bins using geological epochs
#'
#' A function to create a table of time bins. The function is called within the
#' `time_bins()` function only in the case where `epochs` are specified by the
#' user. By default, Cenozoic epochs are returned.
#'
#' @param start \code{numeric}. Numerical age at which the oldest time bin
#'    should start. Default is `66`.
#' @param end \code{numeric}. Numerical age at which the youngest time bin
#'    should end. Default is `0`.
#' @returns A `dataframe` containing the start, end, and midpoint ages of the
#'    generated time bins.
#'
#' @importFrom tidyr separate
#' @examples
#' # Generate time bins for the Cenozoic
#' example1 <- build_stages()
#' # Generate time bins for the Mesozoic
#' example2 <- build_stages(start = 252, end = 66)
#' @export
build_epochs <- function(start = 66, end = 0) {

  # Handling errors
  if (!is.numeric(start) || !is.numeric(end)) {
    stop("`start` and/or `end` are not of numeric class")
  }

  if (end >= start) {
    stop(paste("Start value must be larger than end value"))
  }

  # Load inbuilt table of geological stages
  bins <- geo_bins

  # Combine 'system' and 'series' names
  bins$epoch <- paste(bins$sys, bins$series)
  bins <- bins[c(10, 4:6)]

  # Trim to relevant epochs
  bins <- bins[which(bins$start <= start & bins$end >= end), ]

  # Consolidate epochs
  epoch_tables <- split(bins, f = bins$epoch)
  new_bins <- data.frame()
  for(i in 1:length(epoch_tables)){
    epoch <- unique(epoch_tables[[i]][,1])
    start <- max(epoch_tables[[i]][,2])
    end <- min(epoch_tables[[i]][,3])
    midpoint <- (start - end)/2 + end
    e <- cbind(epoch, start, end, midpoint)
    new_bins <- rbind(new_bins, e)
  }
  bins <- new_bins[order(as.numeric(new_bins$start), decreasing = TRUE), ]
  bins <- separate(bins, epoch, into = c("period", "epoch"), sep=" ")

  return(bins)
}


#' Build time bins using a custom vector
#'
#' A function to create a table of time bins. The function takes a vector of
#' time bin boundary ages (assumed to be millions of years, or years, in the
#' past) and makes a table of start, end, and midpoint ages.
#'
#' @param custom_bins \code{numeric}. A vector of time bin boundary ages.
#' @returns A `dataframe` containing the start, end, and midpoint ages of the
#'    generated time bins.
#'
#' @examples
#' # Create vector of bin boundary ages
#' bin_boundaries <- c(264.26, 224.54, 192.9)
#' # Generate time bins
#' example1 <- customise_bins(custom_bins = bin_boundaries)
#' @export
customise_bins <- function(custom_bins = NULL){

  # Handling errors
  if (!is.numeric(custom_bins)) {
    stop("`custom_bins` vector is not of numeric class")
  }

  # Sort bin boundaries
  custom_bins <- sort(custom_bins, decreasing = TRUE)

  # Create bins
  bins <- data.frame(
    start = custom_bins[-length(custom_bins)],
    end = custom_bins[-1])

  # Calculate midpoints
  bins$midpoint <- rowMeans(bins)

  return(bins)
}



#' Modify time bins
#'
#' A function to modify a table of time bin start, end, and midpoint ages,
#' depending on any settings provided.
#'
#' @param bins \code{dataframe}. A `dataframe` of time bin start, end, and
#'    midpoint ages.
#' @param finish \code{numeric}. The age at which the youngest time bin ends.
#' @param rmv_bins \code{character}. The names of time bins to be removed from
#'    the `dataframe`.
#' @param quat \code{logical}. When `quat = TRUE`, bins in the Quaternary are
#'    merged into one time bin. Default is `FALSE`.
#' @param holo \code{logical}. When `holo = TRUE`, bins in the Holocene are
#'    merged into one time bin. Default is `FALSE`.
#' @param holo_ple \code{logical}. When `holo_ple = TRUE`, bins in the Holocene
#'    are merged into one time bin along with the Upper Pleistocene. Default is
#'    `FALSE`.
#' @returns A `dataframe` containing the start, end, and midpoint ages of the
#'    generated time bins.
#'
#' @examples
#' # Remove Sinemurian from a set of Jurassic time bins
#' example1 <- build_stages(start = 202, end = 145)
#' example1 <- adjust_stages(example1, finish = 145, rmv_bins = "Sinemurian")
#' # Merge Quaternary time bin
#' example2 <- build_stages()
#' example2 <- adjust_stages(example2, finish = 0, quat = TRUE)
#' @export
adjust_stages <- function(bins = NULL, finish = NULL, rmv_bins = NULL,
                          quat = FALSE, holo = FALSE, holo_ple = FALSE){

  # Handling errors
  if (is.data.frame(bins) == FALSE) {
    stop("`bins` should be a dataframe.")
  }

  if (!is.numeric(finish)) {
    stop("`finish` is not of numeric class")
  }

  # Remove named stages
  if(!is.null(rmv_bins)){
    bins <- bins[which(bins$stage != rmv_bins), ]
  }

  # Merge bins in the Quaternary to one bin
  if(quat == T & finish < 2.58){
    n_q_bins <- length(which(bins$start <= 2.58))
    bins<-bins[1:((nrow(bins)) - n_q_bins), ]
    bins <- rbind(bins, c("Quaternary", 2.58, finish,
                          midpoint = (2.58 - finish) / 2))
  }

  # Merge bins in the Holocene to one bin
  if(holo == T & finish <= 0.0117){
    n_holo_bins_merge <- length(which(bins$start <= 0.0117))
    bins<-bins[1:((nrow(bins)) - n_holo_bins_merge), ]
    bins <- rbind(bins, c("Holocene", 0.0117, finish,
                          midpoint = (0.0117 - finish) / 2))
  }

  # Merge bins in the Holocene and Upper Pleistocene to one bin
  if(holo_ple == T & finish <= 0.0117){
    n_bins_merge <- length(which(bins$start <= 0.129))
    bins<-bins[1:((nrow(bins)) - n_bins_merge), ]
    bins <- rbind(bins, c("Upper", 0.129, finish,
                          midpoint = (0.129 - finish) / 2))
  }

  return(bins)
}


#' Adjust epochs time bins
#'
#' 'adjust_epochs()' takes a table of time bin start, end and midpoint ages and
#' makes adjustments depending on settings provided.
#' @param bins A table of time bin start, end and midpoint ages.
#' @param finish Ending age of the youngest time bin.
#' @param rmv_bins Named bins will be removed from the bins object.
#' @param quat When TRUE bins in the Quaternary are merged into one time bin.
#' @returns A table of start, end and midpoint ages
#' @examples
#' adjust_epochs(bins, finish=192.9)
#' @export
adjust_epochs <- function(bins, finish, rmv_bins=NULL, quat=F){
  if(!is.null(rmv_bins)){
    bins <- bins[which(bins$epoch!=rmv_bins), ]
  }
  if(quat == T & finish < 2.58){
    n_q_bins <- length(which(bins$start<=2.58))
    bins<-bins[1:((nrow(bins))-n_q_bins), ]
    bins <- rbind(bins, c("Quaternary", "Quaternary", 2.58, finish,
                          midpoint = (2.58-finish)/2))
  }
  return(bins)
}


#' Make time bins which line up with geological stages or epochs
#'
#' 'div_bins()' makes a table of time bin start, end and midpoint ages at
#' either the scale of epochs or stages, which are then broken down into smaller
#' time bins of specified average length with bin boundaries that align to those
#' of the geological stages or epochs.
#' @param res Return "high" or "low" resolution time bins
#' @param bin_type Determines how bins will be made (equal bins, epochs, or stages)
#' @param begin Starting age of the oldest time bin
#' @param finish Ending age of the youngest time bin
#' @param t_res Adjust the average length of time bins (t_res = 1 gives bins averaging 1 Ma in length)
#' @param n_bins Number of bins (used only when equal_bins are specified)
#' @param remove_bins Specify named time bins to be removed (quote directly or
#' use a list object)
#' @param use_q Merge time bins in the Quaternary together
#' @param merge_holo Merge time bins in the Holocene together
#' @param merge_holo_ple Merge time bins in the Upper Pleistocene and Holocene
#' @returns A table of time bins with start, end and midpoint ages. Where epochs
#' or stages specified names are stored as well.
#' @examples
#' div_bins(bin_type="stages", begin=264.28 , finish=192.9, t_res=NULL)
#' div_bins(bin_type="stages", begin=264.28, finish=192.9, t_res=1)
#' div_bins(bin_type="stages", begin=264.28 , finish=192.9, t_res=10)
#' @export
div_bins <- function(bin_type, begin, finish, t_res=NULL, n_bins=NA,
                  remove_bins=NULL, use_q=FALSE, merge_holo=FALSE,
                  merge_holo_ple= FALSE){

  g_bins <- time_bins(bin_type=bin_type, begin = begin, finish = finish,
                      n_bins=n_bins, remove_bins = remove_bins, use_q = use_q,
                      merge_holo=merge_holo, merge_holo_ple = merge_holo_ple)
  if(is.null(t_res)){
    return(g_bins)
  }

  if(!is.null(t_res)){
    g_start <- c(as.numeric(g_bins$start))
    g_end <- c(as.numeric(g_bins$end))
    n <- round((g_start-g_end)*t_res) ## ADJUST:AVOID WORDS TO DEFINE THE OPTION (NULL = LOW, IF NOT NULL A NUMBER THAT REFLECTS THE DESIRED RESOLUTION)
    start <- c()
    g_bin_indices <- c()
    for(i in 1:length(g_start)){
      high_res <- seq(g_start[i], g_end[i], length.out=n[i]+1)
      high_res <- high_res[-length(high_res)]
      start <- append(start, high_res)
      g_bin_indices <- append(g_bin_indices, rep(i, times=length(high_res)))
    }
    end <- append(start[-1], finish)
    midpoint <- (start-end)/2 + end
    bins <- data.frame(start, end, midpoint)
    avg_bin_length <- mean(start-end)
    range_bin_lengths <- range(start-end)
    g_bin_indices
    avg_bin_length <- mean(start-end)
    range_bin_lengths <- range(start-end)
    print(paste0("Average bin length is ", avg_bin_length))
    print(paste0("Range of bin lengths ", range_bin_lengths))

    return(bins)
  }
}


#' Make time bins all options
#'
#' 'time_bins_all_opts()' makes a table of time bin start, end and midpoint ages
#'
#' @param bin_type Determines how bins will be made (equal bins, epochs, or
#' stages) or whether "custom_bins" will be used.
#' @param begin Starting age of the oldest time bin
#' @param finish Ending age of the youngest time bin
#' @param n_bins Number of bins (used only when equal_bins are specified)
#' @param remove_bins Specify named time bins to be removed (quote directly or
#' use a list object)
#' @param use_q Merge time bins in the Quaternary together
#' @param merge_holo Merge time bins in the Holocene together
#' @param merge_holo_ple Merge time bins in the Upper Pleistocene and Holocene
#' @param lr_hr_bins Make time bins at a specified low or high resolution
#' @returns A table of time bins with start, end and midpoint ages. Where epochs
#' or stages specified names are stored as well.
#' @examples
#' time_bins_all_opts(bin_type="equal_bins", begin=100, finish=0, n_bins=10)
#' time_bins_all_opts(bin_type="stages", begin=251.902, finish=66)
#' time_bins_all_opts(bin_type="epochs", begin=251.902, finish=66)
#' time_bins_all_opts(bin_type="stages", begin=66, finish=0, remove_bins="Danian")
#' time_bins_all_opts(bin_type="stages", begin=66, finish=0, use_q = T)
#' time_bins_all_opts(bin_type="stages", begin=66, finish=0, merge_holo = T)
#' time_bins_all_opts(bin_type="stages", begin=66, finish=0, merge_holo_ple = T)
#' time_bins_all_opts(bin_type="epochs", begin=66, finish=0, use_q = T)
#' time_bins_all_opts(bin_type="epochs", begin=66, finish=0, remove_bins = "Holocene")
#' time_bins_all_opts(bin_type="custom_bins", custom_bins = -c(10:0))
#' time_bins_all_opts(bin_type="stages", begin=66, finish=0, lr_hr_bins="low")
#' time_bins_all_opts(bin_type="stages", begin=66, finish=0, lr_hr_bins="high")
#' @export
time_bins_all_opts <- function(bin_type, begin, finish, n_bins = 100,
                               custom_bins = NULL, lr_hr_bins = "",
                               remove_bins = NULL, use_q = FALSE,
                               merge_holo = FALSE, merge_holo_ple = FALSE){
  if(bin_type == "equal_bins"){
    bins <- build_bins(start = begin, end = finish, n = n_bins)
  }
  if(bin_type == "epochs"){
    bins <- build_epochs(begin = begin, finish = finish)
    bins <- adjust_epochs(bins, finish = finish, rmv_bins = remove_bins,
                          quat = use_q)
  }
  if(bin_type == "stages"){
    bins <- build_stages(begin = begin, finish = finish)
    bins <- adjust_stages(bins, finish = finish, rmv_bins = remove_bins,
                          quat = use_q, holo = merge_holo,
                          holo_ple = merge_holo_ple)
  }
  if(bin_type == "custom_bins"){
    bins <- customise_bins(custom_bins)
  }
  if(lr_hr_bins == "low"){
    bins <- lr_hr_bins(res = "low", bin_type = bin_type, begin = begin,
                       finish = finish)
  }
  if(lr_hr_bins == "high"){
    # Build high resolution time bins with boundaries that align to lr_bins with
    # length 1 My on average
    bins <- lr_hr_bins(res="high", bin_type = bin_type, begin = begin,
                       finish = finish)
  }
  return(bins)
}


#' Make time bins
#'
#' 'time_bins()' makes a table of time bin start, end and midpoint ages
#'
#' @param bin_type Determines how bins will be made (equal bins, epochs, or
#' stages) or whether "custom_bins" will be used.
#' @param begin Starting age of the oldest time bin
#' @param finish Ending age of the youngest time bin
#' @param n_bins Number of bins (used only when equal_bins are specified)
#' @param remove_bins Specify named time bins to be removed (quote directly or
#' use a list object)
#' @param use_q Merge time bins in the Quaternary together
#' @param merge_holo Merge time bins in the Holocene together
#' @param merge_holo_ple Merge time bins in the Upper Pleistocene and Holocene
#' @returns A table of time bins with start, end and midpoint ages. Where epochs
#' or stages specified names are stored as well.
#' @examples
#' time_bins(bin_type="equal_bins", begin=100, finish=0, n_bins=10)
#' time_bins(bin_type="stages", begin=251.902, finish=66)
#' time_bins(bin_type="epochs", begin=251.902, finish=66)
#' time_bins(bin_type="stages", begin=66, finish=0, remove_bins="Danian")
#' time_bins(bin_type="stages", begin=66, finish=0, use_q = T)
#' time_bins(bin_type="stages", begin=66, finish=0, merge_holo = T)
#' time_bins(bin_type="stages", begin=66, finish=0, merge_holo_ple = T)
#' time_bins(bin_type="epochs", begin=66, finish=0, use_q = T)
#' time_bins(bin_type="epochs", begin=66, finish=0, remove_bins = "Holocene")
#' time_bins(bin_type="custom_bins", custom_bins = -c(10:0))
#' @export
time_bins <- function(bin_type, begin, finish, n_bins = 100, custom_bins = NULL,
                      remove_bins = NULL, use_q = FALSE, merge_holo = FALSE,
                      merge_holo_ple = FALSE){
  if(bin_type == "equal_bins"){
    bins <- build_bins(start = begin, end = finish, n = n_bins)
  }
  if(bin_type == "epochs"){
    bins <- build_epochs(begin = begin, finish = finish)
    bins <- adjust_epochs(bins, finish = finish, rmv_bins = remove_bins,
                          quat = use_q)
  }
  if(bin_type == "stages"){
    bins <- build_stages(begin = begin, finish = finish)
    bins <- adjust_stages(bins, finish = finish, rmv_bins = remove_bins,
                          quat = use_q, holo = merge_holo,
                          holo_ple = merge_holo_ple)
  }
  if(bin_type == "custom_bins"){
    bins <- customise_bins(custom_bins)
  }
  return(bins)
}
