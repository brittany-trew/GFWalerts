#' Request token.
#' 
#' Request an authorization token from GFW. Requires your username and password.
#' 
#' @param username character. The username for your Global Forest Watch account.
#' @param pw character. The password for your Global Forest Watch account.
#' @return Returns an authorization token from Global Forest Watch.
#' @examples
#' \dontrun{
#'   # Example to request a token.
#'   auth_token <- request.token(username = "your_username", pw = "your_password")
#' }

#' @export
request.token <- function(username, pw){
  url <- "https://data-api.globalforestwatch.org/auth/token"
  response <- POST(url, 
                   body = list(username = username, password = pw), 
                   encode = "form")
  parsed_response <- content(response, "parsed")
  auth.token <- parsed_response$data$access_token  # Replace with your actual token
  return(auth.token)
}

#' Obtain API key.
#' 
#' Request an API key for downloading data from Global Forest Watch. Requires an authorization token which can be obtained using the function \code{\link{request.token}}.
#' 
#' @param auth.token character. Authorization token from Global Forest Watch (see description)
#' @return Returns an API key from Global Forest Watch.
#' @examples
#' \dontrun{
#'   # Example to obtain an API key
#'   auth_token <- request.token(username = "your_username", pw = "your_password")
#'   api_key <- create.api(auth_token)
#' }
#' @export
create.api <- function(auth.token){
  url <- "https://data-api.globalforestwatch.org/auth/apikey"
  headers <- c(
    `Authorization` = paste("Bearer", auth.token),
    `Content-Type` = "application/json"
  )
  body <- list(
    alias = "api-key-for-new-app",
    email = "gfw.guides@yahoo.com",
    organization = "GFW",
    domains = list()
  )
  json_body <- jsonlite::toJSON(body, auto_unbox = TRUE)
  response <- POST(url, add_headers(.headers=headers), body = json_body, encode = "json")
  parsed_response <- content(response, "parsed")
  api.key <- parsed_response$data$api_key
  return(api.key)
}

#' Print Global Forest Watch Data.
#' 
#' Provides the names of all the datasets available from Global Forest Watch. Requires an API key from \code{\link{create.api}}.
#' 
#' @param api.key character. API key from Global Forest Watch (see description)
#' @return NULL. Prints the names of all available datasets from Global Forest Watch.
#' @export
find.dataset <- function(api.key){
  url <- "https://data-api.globalforestwatch.org/datasets"
  response <- GET(url, add_headers(`x-api-key` = api.key))
  parsed_response <- content(response, "parsed")
  for(i in 1:length(parsed_response$data)){
    print(i)
    print(parsed_response$data[[i]]$dataset)
  }
}

#' Define extent.
#' 
#' Define a geographical extent for requesting data from Global Forest Watch.
#' 
#' @param xmn numeric. xmin for bounding box.
#' @param xmx numeric. xmax for bounding box.
#' @param ymn numeric. ymin for bounding box.
#' @param ymx numeric. ymax for bounding box.
#' @return Returns a list with bounding coordinates.
#' @export
geom.define <- function(xmn, xmx, ymn, ymx) {
  list(
    type = "Polygon",
    coordinates = list(
      list(
        c(xmn, ymx),  
        c(xmn, ymn),  
        c(xmx, ymn),  
        c(xmx, ymx),  
        c(xmn, ymx)   
      )
    )
  )
}

#' Print data fields.
#' 
#' Print all fields associated with a Global Forest Watch dataset. Requires the data url which can be obtained from \code{\link{find.dataset}}.
#' 
#' @param url character. url for the dataset from Global Forest Watch (see description)
#' @return Returns a list of fields associated with the dataset defined by your url.
#' @export
print.fields <- function(url){
  response <- GET(url)
  df_response <- content(response, "parsed")
  for(i in 1:length(df_response$data)){
    print(i)
    print(df_response$data[[i]]$pixel_meaning)
  }
}

#' Download Global Forest Watch Integrated Alerts.
#' 
#' Download Global Forest Watch Integrated Alerts for deforestation across a defined geographical extent and time period. Requires an API key from \code{\link{create.api}} and a vector of boundary coordinates for the geographical extent.
#' 
#' @param api.key character. API key from Global Forest Watch (see description)
#' @param geom.ext numeric vector of length 4. Boundary coordinates for the geographical extent, given as (xmin, xmax, ymin, ymax).
#' @param start character string. The start date for the alerts (in "YYYY-MM-DD" format).
#' @param end character string. The end date for the alerts (in "YYYY-MM-DD" format).
#' @param confidence optional character. Define the confidence level for alerts (high or highest, default is highest)
#' @param max_retries optional single positive numeric value. Maximum number of times to resend the request if it fails (default is 3 tries).
#' @param retry_delay optional single positive numeric value. Number of seconds delay between retries so as to not overload the request system (default is 5 seconds).
#' 
#' @return Returns the Global Forest Watch integrated deforestation alerts as a point SpatVector object. If the download fails after retries, returns `NULL`.
#' @export
get.gfw.alerts <- function(api.key, geom.ext, start, end,
                           confidence = "highest",
                           max_retries = 3, retry_delay = 5){
  
  url = "https://data-api.globalforestwatch.org/dataset/gfw_integrated_alerts/latest/download/csv"
  
  for (attempt in 1:max_retries) {
    # Use geom.define to construct the geometry from geom.ext coordinates
    geometry <- geom.define(geom.ext[1], geom.ext[2], geom.ext[3], geom.ext[4])
    
    # Set SQL query
    sql_query <- paste0("SELECT longitude, latitude, gfw_integrated_alerts__date, gfw_integrated_alerts__confidence FROM data WHERE gfw_integrated_alerts__date >= '", start, "' AND gfw_integrated_alerts__date <= '", end, "'")
    
    # Prepare the POST request body
    body <- toJSON(list(geometry = geometry, sql = sql_query), auto_unbox = TRUE)
    
    # Send the POST request
    response <- POST(url, add_headers(`x-api-key` = api.key, `Content-Type` = "application/json"), body = body)
    
    if (status_code(response) == 200) {
      # Parse the response content as text
      csv_content <- content(response, "text")
      
      if (csv_content == "") {
        return(NULL)
      }
      
      # Convert the CSV text to a data frame
      data <- readr::read_csv(csv_content, show_col_types = FALSE)
      print(paste("Downloaded all alerts for: ", start, " to ", end))
      
      # Convert to spatial object and filter by confidence level
      points_sf <- st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326, agr = "constant")
      points_filtered <- dplyr::filter(points_sf, gfw_integrated_alerts__confidence == confidence)
      points_filtered <- terra::vect(points_filtered)
      
      print(paste("Filtered alerts for confidence: '", confidence, "' and converted to shapefile."))
      return(points_filtered)
      
    } else if (status_code(response) == 500) {
      print("Server error (500): The request could not be processed. Consider trying again later.")
      return(NULL)
      
    } else if (grepl("timed out", content(response, "text"))) {
      if (attempt < max_retries) {
        print(paste("Attempt", attempt, "failed due to timeout. Retrying after", retry_delay, "seconds..."))
        Sys.sleep(retry_delay) # Wait before retrying
      } else {
        print("Max retries reached due to timeout. Consider reducing the size of your geographical extent or time period.")
        return(NULL)
      }
    } else {
      # Handle other errors or unexpected status codes
      print(paste("Request failed with status code:", status_code(response)))
      print(content(response, "text"))
      return(NULL)
    }
  }
  
  # If retries fail, explicitly return NULL and inform the user
  print("Max retries reached. The request failed. Consider adjusting the request parameters.")
  return(NULL)
}

#' Monthly Global Forest Watch Integrated Alerts.
#' 
#' Download monthly Global Forest Watch Integrated Alerts for deforestation across a defined geographical extent. The function splits the target month into short time steps to minimize the risk of the request failing and combines the results. For requests that don't require results as monthly time steps, use \code{\link{get.gfw.alerts}}.
#' 
#' @param target character vector of length 2. The output from \code{\link{find.target.MY}}, where the first element is the target month ("MM") and the second element is the target year ("YYYY").
#' @param api.key character. API key from Global Forest Watch (see description)
#' @param geom.ext numeric vector of length 4. Boundary coordinates for the geographical extent (xmin, xmax, ymin, ymax). See \code{\link{geom.define}} for more details.
#' @param target.proj character. The EPSG code for the target projection of the spatial data (e.g., "EPSG:4326").
#' @param confidence optional character. Define the confidence level for alerts (high or highest, default is highest)
#' @param max_retries optional single positive numeric value. Maximum number of times to resend the request if it fails (default is 3 tries).
#' @param retry_delay optional single positive numeric value. Number of seconds delay between retries so as to not overload the request system (default is 5 seconds).
#' 
#' @return A list with a SpatVector point object consisting of the Global Forest Watch integrated deforestation alerts for the target month, plus a start and end date.
#' @export
run.download.month <- function(target, api.key, geom.ext,
                               target.proj = "EPSG:4326",
                               confidence = "highest",
                               max_retries = 3, retry_delay = 5){
  
  # Split the month into short time steps
  split.seq <- split.month(target)
  
  list_month <- list()
  itn <- 1
  
  for (i in 1:length(split.seq[[1]])) {
    # Run in parts to reduce request size
    start <- split.seq[[1]][i]
    end <- split.seq[[2]][i]
    
    # Get the alerts for the current time step
    result <- get.gfw.alerts(api.key = api.key, 
                             geom.ext = geom.ext, 
                             start = start, 
                             end = end)
    
    # Check if result is NULL or empty before projecting
    if (is.null(result) || nrow(result) == 0) {
      # Skip this iteration if no data was returned
      warning(paste("No data for time step from", start, "to", end))
      next()
    }
    
    # Project result (ensure target projection is applied correctly)
    result_proj <- terra::project(result, target.proj) # Use the specified projection
    
    list_month[[itn]] <- result_proj
    itn <- itn + 1
  }
  
  # Combine records if data was collected
  if (length(list_month) > 0) {
    month_df <- do.call(rbind, list_month)
    
    # Remove duplicates from merged data
    attr_data <- st_as_sf(month_df)
    attr_data <- attr_data %>% distinct()
    
    # Filter data for the target month
    filt_df <- attr_data[format(as.Date(attr_data$gfw_integrated_alerts__date), "%m") == sprintf("%02d", as.numeric(target[1])), ]
    vect_filt <- month_df[which(month_df$gfw_integrated_alerts__date %in% filt_df$gfw_integrated_alerts__date), ]
    
    return(list("alerts" = vect_filt, "start" = split.seq[[3]], "end" = split.seq[[4]]))
  } else {
    message("No data was collected for the specified period.")
  }
}


#' Split a month into small time steps.
#' 
#' Splits a target month into steps of 2 days.
#' @param target character vector of length 2. The output from \code{\link{find.target.MY}}, where the first element is the target month ("MM") and the second element is the target year ("YYYY").
#' 
#' @return A list consisting of a sequence of start and end dates to split the month, plus the first and last dates in the month.
#' @export
split.month <- function(target) {
  target_month <- target[1]
  target_year <- target[2]
  start_date <- as.Date(paste(target_year, target_month, "01", sep = "-"))
  if (as.numeric(target_month) < 12) {
    end_date <- as.Date(paste(target_year, as.numeric(target_month) + 1, "01", sep = "-")) - 1
  } else {
    end_date <- as.Date(paste(as.numeric(target_year) + 1, "01", "01", sep = "-")) - 1
  }
  start_seq <- seq(start_date, end_date, by = "2 days")
  end_seq <- pmin(start_seq + 1, end_date)
  start.end <- list(start_seq = start_seq, end_seq = end_seq, start_date = start_date, end_date = end_date)
  return(start.end)
}

#' Define target month based on current date.
#' 
#' Calculate the target month and year for downloading deforestation alerts based on the current date and a specified number of months back. This is useful for time-based data queries to Global Forest Watch.
#' @param current_date date. Date object representing the current date, typically obtained using `Sys.Date()`.
#' @param months_back integer. A positive integer representing the number of months to move backwards from the current date to find the target month. For example, the month prior to the current month is 1 back.
#' 
#' @return character vector of length 2. The output from \code{\link{find.target.MY}}, as c("MM", "YYYY"), where the first element is the target month ("MM") and the second element is the target year ("YYYY"). 
#' @export
find.target.MY <- function(current_date, months_back){
  dm <- as.Date(format(current_date, "%Y-%m-01"))
  dm <- seq(dm, length = 2, by = paste(-months_back, "months"))[2]
  target.month <- format(dm, "%m")
  target.year <- format(dm, "%Y")
  return(c(target.month, target.year))
}
