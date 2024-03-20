SCAPES_media <- function(questionnaire, media_folder, force_overwrite = FALSE) {
  
  questionnaire_id <- questionnaire$uid
  
  SCAPES_user_agent <- function() {
    robotoolbox_version <- packageVersion("robotoolbox")
    os <- Sys.info()[["sysname"]]
    os_version <- paste(Sys.info()[["release"]],
                        Sys.info()[["version"]])
    r_version <- paste0(R.version$major, ".",
                        R.version$minor,
                        ifelse(R.version$status == "", "",
                               paste0("-", R.version$status)))
    header <- paste0("robotoolbox/",
                     robotoolbox_version, " (", os, "/",
                     os_version, "; ", "R/", r_version, ")")
    header
  }
  
  SCAPES_xget <- function(path, args = list(), n_retry = 3L, ...) {
    headers <- list(Authorization = paste("Token",
                                          Sys.getenv("KOBOTOOLBOX_TOKEN")),
                    `User-Agent` = SCAPES_user_agent())
    cli <- HttpClient$new(Sys.getenv("KOBOTOOLBOX_URL"),
                          headers = headers, opts = list(...))
    res <- cli$retry("get",
                     path = path,
                     query = args,
                     times = n_retry,
                     retry_only_on = c(500, 503),
                     terminate_on = 404)
    if (res$status_code >= 300)
      abort(error_msg(res$content),
            call = NULL)
    res$raise_for_ct_json()
    res$parse("UTF-8")
  }
  
  
  SCAPES_get_subs <- function(uid, args = list(), ...) {
    path <- paste0("api/v2/assets/", uid, "/data.json")
    res <- SCAPES_xget(path = path, args = args, ...)
    res <- fparse(res, max_simplify_lvl = "data_frame",
                  query = "/results")
    as_tibble(res)
  }
  
  attachments <- SCAPES_get_subs(questionnaire_id)[["_attachments"]]
  
  if (!dir.exists(media_folder))
    rlang::abort(paste(media_folder, "folder does not exist, create it first! Run dir.create(...)"),
                 call = NULL)
  
  no_img <- sapply(attachments, is.null)
  
  if(any(!no_img)) {
    
    attachments <- attachments[!no_img]
    urls <- lapply(attachments,
                   \(x) data.frame(url = x$download_url,
                                   id = x$instance,
                                   fname = x$filename))
    urls <- list_rbind(urls)
    urls <- unique(urls)
    urls$fname <- basename(urls$fname)
    
    headers <- list(Authorization = paste("Token",
                                          Sys.getenv("KOBOTOOLBOX_TOKEN")))
    
    
  }
  
  urls <- urls %>%
    group_by(id) %>%
    group_split()
  
  total_groups <- length(urls)
  group_count <- 0
  
  
  SCAPES_download_media <- function(url_info, folder, headers, force_overwrite) {
    # Extract filename and path
    fname <- paste0(url_info$id, "_", url_info$fname)
    path <- file.path(folder, fname)
    # Check if file exists and force overwrite option is not enabled
    if (!force_overwrite && any(file.exists(path))) {
      return(NULL)  # Skip downloading
    }
    # Create async connection
    cc <- Async$new(urls = url_info$url, headers = headers)
    # Download the file
    res <- cc$get(disk = path)
    # Check for HTTP status codes
    cond <- vapply(res, function(x) x$status_code, double(1)) >= 300L
    if (any(cond)) {
      msg <- res$content()[cond]
      abort(error_msg(msg[[1]]), call = NULL)
    }
    invisible(res)
    
  }
  
  # Initialize progress bar
  pb <- progress_bar$new(
    format = "[:bar] :current/:total surveys processed",
    total = total_groups,
    clear = TRUE
  )
  
  results <- list()
  
  for (i in 1:length(urls)) {
    group_count <- group_count + 1
    # Update progress bar
    pb$tick()
    
    # Append the results of the current iteration to the 'results' list
    results[[i]] <- lapply(urls[i], SCAPES_download_media, folder = media_folder, headers = headers, force_overwrite = force_overwrite)
    
  }
  
  # Close progress bar
  pb$terminate()
  
  # Count the number of successful downloads
  skipped_count <- sum(unlist(lapply(results, function(x) any(unlist(lapply(x, is.null))))))
  downloaded_count <- length(urls) - skipped_count
  
  message(paste("Downloaded media from", downloaded_count, "surveys. Skipped", skipped_count, "surveys with associated images already in the media folder."))
  
}

