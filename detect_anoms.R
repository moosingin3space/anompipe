detect_anoms <- function(data, k = 0.49, alpha = 0.05, num_obs_per_period = NULL, 
                         use_decomp = TRUE, use_esd = FALSE, one_tail = TRUE, 
                         upper_tail = TRUE, verbose = FALSE) {
    # Detects anomalies in a time series using S-H-ESD.
    #
    # Args:
    #	 data: Time series to perform anomaly detection on.
    #	 k: Maximum number of anomalies that S-H-ESD will detect as a percentage of the data.
    #	 alpha: The level of statistical significance with which to accept or reject anomalies.
    #	 num_obs_per_period: Defines the number of observations in a single period, and used during seasonal decomposition.
    #	 use_decomp: Use seasonal decomposition during anomaly detection.
    #	 use_esd: Uses regular ESD instead of hybrid-ESD. Note hybrid-ESD is more statistically robust.
    #	 one_tail: If TRUE only positive or negative going anomalies are detected depending on if upper_tail is TRUE or FALSE.
    #	 upper_tail: If TRUE and one_tail is also TRUE, detect only positive going (right-tailed) anomalies. If FALSE and one_tail is TRUE, only detect negative (left-tailed) anomalies.
    #	 verbose: Additionally printing for debugging.
    # Returns:
    #   A list containing the anomalies (anoms) and decomposition components (stl).

    if(is.null(num_obs_per_period)) {
        stop("must supply period length for time series decomposition")
    }

    num_obs <- nrow(data)

    # Check to make sure we have at least two periods worth of data for anomaly context
    if(num_obs < num_obs_per_period * 2) {
        stop("Anom detection needs at least 2 periods worth of data")
    }

    # Check if our timestamps are posix
    posix_timestamp <- if (class(data[[1L]])[1L] == "POSIXlt") TRUE else FALSE

    # -- Step 1: Decompose data. This returns a univarite remainder which will be used for anomaly detection. Optionally, we might NOT decompose.
    data_decomp <- stl(ts(data[[2L]], frequency = num_obs_per_period), 
                       s.window = "periodic", robust = TRUE)

    data <- data.frame(timestamp = data[[1L]], count = (data[[2L]]-data_decomp$time.series[,"seasonal"]-median(data[[2L]])))
    data_decomp <- data.frame(timestamp=data[[1L]], count=(as.numeric(trunc(data_decomp$time.series[,"trend"]+data_decomp$time.series[,"seasonal"]))))

    if(posix_timestamp){
        data_decomp <- format_timestamp(data_decomp)
    }
    # Maximum number of outliers that S-H-ESD can detect (e.g. 49% of data)
    max_outliers <- trunc(num_obs*k)

    dataNAs <- sum(is.na(data[[2L]]))
    if (dataNAs > 0) {
        if (any(is.na(data[[2L]][-(1L:dataNAs)]))) 
            stop("Data contains non-leading NAs")
        else
            data[[2L]][1L:dataNAs] <- 1
    }

    func_ma <- match.fun(median)
    func_sigma <- match.fun(mad)

    ## Define values and vectors.
    n <- length(data[[2L]])
    if (posix_timestamp) {
        R_idx <- as.POSIXlt(data[[1L]][1L:max_outliers], tz = "UTC")
    } else {
        R_idx <- 1L:max_outliers
    }

    num_anoms <- 0L

    # Compute test statistic until r=max_outliers values have been
    # removed from the sample.
    for (i in 1L:max_outliers){
        if(verbose) print(paste(i,"/", max_outliers,"completed"))

        if(one_tail){
            if(upper_tail){
                ares <- data[[2L]] - func_ma(data[[2L]])
            } else {
                ares <- func_ma(data[[2L]]) - data[[2L]]
            }
        } else {
            ares = abs(data[[2L]] - func_ma(data[[2L]]))
        }

        ares <- ares/func_sigma(data[[2L]])
        R <- max(ares)

        temp_max_idx <- which(ares == R)[1L]

        R_idx[i] <- data[[1L]][temp_max_idx]

        data <- data[-which(data[[1L]] == R_idx[i]), ]   

        ## Compute critical value.
        if(one_tail){
            p <- 1 - alpha/(n-i+1)
        } else {
            p <- 1 - alpha/(2*(n-i+1))
        }

        t <- qt(p,(n-i-1L))
        lam <- t*(n-i) / sqrt((n-i-1+t**2)*(n-i+1))

        if(R > lam)
            num_anoms <- i
    }

    return(list(anoms = R_idx[1L:num_anoms], stl = data_decomp))
}

#' Anomaly Detection Using Seasonal Hybrid ESD Test 
#'
#' A technique for detecting anomalies in seasonal univariate time series where the input is a 
#' series of <timestamp, count> pairs.
#' @name AnomalyDetectionTs
#' @param x Time series as a two column data frame where the first column consists of the 
#' timestamps and the second column consists of the observations.
#' @param max_anoms Maximum number of anomalies that S-H-ESD will detect as a percentage of the
#' data.
#' @param direction Directionality of the anomalies to be detected. Options are: 
#' \code{'pos' | 'neg' | 'both'}.
#' @param alpha The level of statistical significance with which to accept or reject anomalies.  
#' @param only_last Find and report anomalies only within the last day or hr in the time series.
#' \code{NULL | 'day' | 'hr'}.
#' @param threshold Only report positive going anoms above the threshold specified. Options are: 
#' \code{'None' | 'med_max' | 'p95' | 'p99'}.
#' @param e_value Add an additional column to the anoms output containing the expected value.
#' @param longterm Increase anom detection efficacy for time series that are greater than a month.
#' See Details below.
#' @param plot A flag indicating if a plot with both the time series and the estimated anoms,
#' indicated by circles, should also be returned.
#' @param y_log Apply log scaling to the y-axis. This helps with viewing plots that have extremely
#' large positive anomalies relative to the rest of the data.
#' @param xlabel X-axis label to be added to the output plot.
#' @param ylabel Y-axis label to be added to the output plot.
#' @details
#' \code{longterm} This option should be set when the input time series is longer than a month.
#' The option enables the approach described in Vallis, Hochenbaum, and Kejariwal (2014).\cr\cr
#' \code{threshold} Filter all negative anomalies and those anomalies whose magnitude is smaller 
#' than one of the specified thresholds which include: the median 
#' of the daily max values (med_max), the 95th percentile of the daily max values (p95), and the 
#' 99th percentile of the daily max values (p99).
#' @param title Title for the output plot.
#' @return The returned value is a list with the following components.
#' @return \item{anoms}{Data frame containing timestamps, values, and optionally expected values.}
#' @return \item{plot}{A graphical object if plotting was requested by the user. The plot contains
#' the estimated anomalies annotated on the input time series.}
#' @return One can save \code{anoms} to a file in the following fashion: 
#' \code{write.csv(<return list name>[["anoms"]], file=<filename>)}
#' @return One can save \code{plot} to a file in the following fashion: 
#' \code{ggsave(<filename>, plot=<return list name>[["plot"]])}
#' @references Vallis, O., Hochenbaum, J. and Kejariwal, A., (2014) "A Novel Technique for 
#' Long-Term Anomaly Detection in the Cloud", 6th USENIX, Philadelphia, PA.
#' @references Rosner, B., (May 1983), "Percentage Points for a Generalized ESD Many-Outlier Procedure"
#' , Technometrics, 25(2), pp. 165-172.
#'
#' @docType data
#' @keywords datasets
#' @name raw_data
#' 
#' @examples
#' data(raw_data)
#' AnomalyDetectionTs(raw_data, max_anoms=0.02, direction='both', plot=TRUE)
#' # To detect only the anomalies on the last day, run the following:
#' AnomalyDetectionTs(raw_data, max_anoms=0.02, direction='both', only_last="day", plot=TRUE)
#' @seealso \code{\link{AnomalyDetectionVec}}
#' @export
#' 
AnomalyDetectionTs <- function(x, max_anoms = 0.10, alpha = 0.05, 
                               only_last = NULL, threshold = 'None', longterm = FALSE){

    # Check for supported inputs types
    if(!is.data.frame(x)){
        stop("data must be a single data frame.")
    } else {
        if(ncol(x) != 2 || !is.numeric(x[[2]])){
            stop("data must be a 2 column data.frame, with the first column being a set of timestamps, and the second coloumn being numeric values.")
        }
        # Format timestamps if necessary
        if (!(class(x[[1]])[1] == "POSIXlt")) {
            x <- format_timestamp(x)
        }
    }
    # Rename data frame columns if necessary
    if (any((names(x) == c("timestamp", "count")) == FALSE)) {
        colnames(x) <- c("timestamp", "count")
    }

    # Sanity check all input parameters
    #if(max_anoms > .49){
    #stop(paste("max_anoms must be less than 50% of the data points (max_anoms =", round(max_anoms*length(x[[2]]), 0), " data_points =", length(x[[2]]),")."))
    #}
    #if(!(0.01 <= alpha || alpha <= 0.1)){
    #print("Warning: alpha is the statistical signifigance, and is usually between 0.01 and 0.1")
    #}
    #if(!is.null(only_last) && !only_last %in% c('day','hr')){
    #stop("only_last must be either 'day' or 'hr'")
    #}
    #if(!threshold %in% c('None','med_max','p95','p99')){
    #stop("threshold options are: None | med_max | p95 | p99.") 
    #}
    #if(!is.logical(longterm)){
    #stop("longterm must be either TRUE (T) or FALSE (F)")
    #}

    # -- Main analysis: Perform S-H-ESD

    # Derive number of observations in a single day.
    # Although we derive this in S-H-ESD, we also need it to be minutley later on so we do it here first.
    gran <- get_gran(x, 1)

    if(gran == "day"){
        num_days_per_line <- 7
        if(is.character(only_last) &&  only_last == 'hr'){
            only_last <- 'day'
        }
    } else {
        num_days_per_line <- 1
    }

    # Aggregate data to minutely if secondly
    if(gran == "sec"){
        x <- format_timestamp(aggregate(x[2], format(x[1], "%Y-%m-%d %H:%M:00"), eval(parse(text="sum"))))
    }

    period = switch(gran,
                    min = 1440,
                    hr = 24,
                    # if the data is daily, then we need to bump the period to weekly to get multiple examples
                    day = 7)
    num_obs <- length(x[[2]])

    if(max_anoms < 1/num_obs){
        max_anoms <- 1/num_obs
    }

    max_anoms <- max_anoms/2

    # -- Setup for longterm time series

    # If longterm is enabled, break the data into subset data frames and store in all_data
    if(longterm){
        # Pre-allocate list with size equal to the number of two week chunks in x + any left over chunk
        # handle edge cases for daily and single column data period lengths
        if(gran == "day"){
            num_obs_two_week <- period*2
        } else {
            num_obs_two_week <- period*14
        }

        # Store last date in time series
        last_date <- x[[1]][num_obs] 

        all_data <- vector(mode="list", length=ceiling(length(x[[1]])/(num_obs_two_week)))
        # Subset x into two week chunks
        for(j in seq(1,length(x[[1]]), by=num_obs_two_week)){
            start_date <- x[[1]][j]
            end_date <- min(start_date + lubridate::weeks(2), x[[1]][length(x[[1]])])
            # if there is at least 14 days left, subset it, otherwise subset last_date - 14days
            if(difftime(end_date, start_date, units = "days") == as.difftime(14, units="days")){
                all_data[[ceiling(j/(num_obs_two_week))]] <- subset(x, x[[1]] >= start_date & x[[1]] < end_date)
            }else{
                all_data[[ceiling(j/(num_obs_two_week))]] <- subset(x, x[[1]] > (last_date-lubridate::weeks(2)) & x[[1]] <= last_date)
            }
        }
    }else{
        # If longterm is not enabled, then just overwrite all_data list with x as the only item
        all_data <- list(x)
    }

    # Create empty data frames to store all anoms and seasonal+trend component from decomposition
    all_anoms <- data.frame(timestamp=numeric(0), count=numeric(0))
    seasonal_plus_trend <- data.frame(timestamp=numeric(0), count=numeric(0))

    anoms <- data.frame(timestamp=numeric(0), count=numeric(0))

    # Detect anomalies on all data (either entire data in one-pass, or in 2 week blocks if longterm=TRUE)
    direction <- "both"
    for(i in 1:length(all_data)) {

        anomaly_direction = switch(direction,
                                   "pos" = data.frame(one_tail=TRUE, upper_tail=TRUE), # upper-tail only (positive going anomalies)
                                   "neg" = data.frame(one_tail=TRUE, upper_tail=FALSE), # lower-tail only (negative going anomalies)
                                   "both" = data.frame(one_tail=FALSE, upper_tail=TRUE)) # Both tails. Tail direction is not actually used.

        # detect_anoms actually performs the anomaly detection and returns the results in a list containing the anomalies
        # as well as the decomposed components of the time series for further analysis.
        s_h_esd_timestamps <- detect_anoms(all_data[[i]], k=max_anoms, alpha=alpha, num_obs_per_period=period, use_decomp=TRUE, use_esd=FALSE, 
                                           one_tail=anomaly_direction$one_tail, upper_tail=anomaly_direction$upper_tail, verbose=FALSE) 

        # store decomposed components in local variable and overwrite s_h_esd_timestamps to contain only the anom timestamps 
        data_decomp <- s_h_esd_timestamps$stl
        s_h_esd_timestamps <- s_h_esd_timestamps$anoms

        # -- Step 3: Use detected anomaly timestamps to extract the actual anomalies (timestamp and value) from the data
        if(length(s_h_esd_timestamps) > 0){
            anoms <- subset(all_data[[i]], (all_data[[i]][[1]] %in% s_h_esd_timestamps))
        } else {
            anoms <- data.frame(timestamp=numeric(0), count=numeric(0))
        }

        # Filter the anomalies using one of the thresholding functions if applicable
        if(threshold != "None"){
            # Calculate daily max values
            periodic_maxs <- tapply(x[[2]],as.Date(x[[1]]),FUN=max)     

            # Calculate the threshold set by the user
            if(threshold == 'med_max'){
                thresh <- median(periodic_maxs)
            }else if (threshold == 'p95'){
                thresh <- quantile(periodic_maxs, .95)
            }else if (threshold == 'p99'){
                thresh <- quantile(periodic_maxs, .99)
            }
            # Remove any anoms below the threshold
            anoms <- subset(anoms, anoms[[2]] >= thresh)
        }
        all_anoms <- rbind(all_anoms, anoms)
        seasonal_plus_trend <- rbind(seasonal_plus_trend, data_decomp)
    }

    # Cleanup potential duplicates
    all_anoms <- all_anoms[!duplicated(all_anoms[[1]]), ]
    seasonal_plus_trend <- seasonal_plus_trend[!duplicated(seasonal_plus_trend[[1]]), ]

    # -- If only_last was set by the user, create subset of the data that represent the most recent day
    if(!is.null(only_last)){
        start_date <- x[[1]][num_obs]-lubridate::days(7)
        start_anoms <- x[[1]][num_obs]-lubridate::days(1)
        if(gran == "day"){
            #TODO: This might be better set up top at the gran check
            breaks <- 3*12
            num_days_per_line <- 7
        } else {
            if(only_last == 'day'){
                breaks <- 12
            }else{
                # We need to change start_date and start_anoms for the hourly only_last option
                start_date <- lubridate::floor_date(x[[1]][num_obs]-lubridate::days(2), "day")
                start_anoms <- x[[1]][num_obs]-lubridate::hours(1)
                breaks <- 3
            }   
        }

        # subset the last days worth of data
        x_subset_single_day <- subset(x, (x[[1]] > start_anoms))
        # When plotting anoms for the last day only we only show the previous weeks data
        x_subset_week <- subset(x, ((x[[1]] <= start_anoms) & (x[[1]] > start_date)))
        all_anoms <- subset(all_anoms, all_anoms[[1]] >= x_subset_single_day[[1]][1])
        num_obs <- length(x_subset_single_day[[2]])
    }

    # Calculate number of anomalies as a percentage
    anom_pct <- (length(all_anoms[[2]]) / num_obs) * 100

    # If there are no anoms, then let's exit
    if(anom_pct == 0){
        return(NULL)
    }

    return(list(anoms=anoms, trendline=seasonal_plus_trend))
}
format_timestamp <- function(indf, index = 1) {
    if (class(indf[[index]])[1] == "POSIXlt") {
        return(indf)
    }
    if (stringr::str_detect(indf[[index]][1], "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2} \\+\\d{4}$")) {
        indf[[index]] <- strptime(indf[[index]], format="%Y-%m-%d %H:%M:%S", tz="UTC")
    }
    else if (stringr::str_detect(indf[[index]][1], "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$")) {
        indf[[index]] <- strptime(indf[[index]], format="%Y-%m-%d %H:%M:%S", tz="UTC")
    }
    else if (stringr::str_detect(indf[[index]][1], "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}$")) {
        indf[[index]] <- strptime(indf[[index]], format="%Y-%m-%d %H:%M", tz="UTC")
    }
    else if (stringr::str_detect(indf[[index]][1], "^\\d{2}/\\d{2}/\\d{2}$")) {
        indf[[index]] <- strptime(indf[[index]], format="%m/%d/%y", tz="UTC")
    }
    else if (stringr::str_detect(indf[[index]][1], "^\\d{2}/\\d{2}/\\d{4}$")) {
        indf[[index]] <- strptime(indf[[index]], format="%m/%d/%Y", tz="UTC")
    }
    else if (stringr::str_detect(indf[[index]][1], "^\\d{4}\\d{2}\\d{2}$")) {
        indf[[index]] <- strptime(indf[[index]], format="%Y%m%d", tz="UTC")
    }
    else if (stringr::str_detect(indf[[index]][1], "^\\d{4}/\\d{2}/\\d{2}/\\d{2}$")) {
        indf[[index]] <- strptime(indf[[index]], format="%Y/%m/%d/%H", tz="UTC")
    }
    else if (stringr::str_detect(indf[[index]][1], "^\\d{10}$")) {
        # Handle Unix seconds in milliseconds
        indf[[index]] <- as.POSIXlt(indf[[index]], origin="1970-01-01", tz="UTC")
    }

    return(indf)
}

get_gran = function(tsdf, index=1) {
    n = length(tsdf[[index]])
    # We calculate the granularity from the time difference between the last 2 entries (sorted)
    gran = round(difftime(max(tsdf[[index]]), sort(tsdf[[index]], partial = n-1)[n-1], 
                          units = "secs"))

    if (gran >= 86400) {
        return("day")
    }
    else if (gran >= 3600) {
        return("hr")
    }
    else if (gran >= 60) {
        return("min")
    }
    else if (gran >= 1) {
        return("sec")
    }
    else {
        return("ms")
    }
}
