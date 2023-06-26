
#' Title
#'
#' @return
#' @export
#'
#' @examples
#' bucket()
bucket <- function() {
  MerkleLabGIS <- GET("https://devise.uwyo.edu/umbraco/api/MerkleGisDataApi/GetFiles")
  MerkleLabGIS <- fromJSON(rawToChar(MerkleLabGIS$content))

  # URL to be parsed
  MerkleLabGIS$url <- MerkleLabGIS$fileUrl

  MerkleLabGIS$url <- sub("\\?.*", "", MerkleLabGIS$url)
  MerkleLabGIS$urlmodified_url <- sub("https://devise\\.pathfinder\\.arcc\\.uwyo\\.edu/", "https://pathfinder.arcc.uwyo.edu/devise/", MerkleLabGIS$url)

  # Filter URLs that do not end with .tif, .shp, or .xml
  valid_extensions <- c(".tif")
  valid_urls <- grepl(paste0(paste0(valid_extensions, collapse = "|"), "$"), MerkleLabGIS$urlmodified_url)
  MerkleLabGIS <- MerkleLabGIS[valid_urls, ]

  # Parsing the URL
  parts <- strsplit(MerkleLabGIS$urlmodified_url, "/")
  max_parts <- max(sapply(parts, length))
  parsed_parts <- matrix("", nrow = length(parts), ncol = max_parts)

  for (i in 1:length(parts)) {
    parsed_parts[i, 1:length(parts[[i]])] <- parts[[i]]
  }

  parsed_parts_df <- data.frame(parsed_parts, stringsAsFactors = FALSE)
  MerkleLabGIS <- cbind(MerkleLabGIS, parsed_parts_df)
  MerkleLabGIS$Category <- MerkleLabGIS$X6
  MerkleLabGIS$SubCategory <- MerkleLabGIS$X7
  MerkleLabGIS$URL <- MerkleLabGIS$urlmodified_url
  MerkleLabGIS <- MerkleLabGIS[, c("fileName", "lastModified", "Category", "SubCategory", "URL")]


  return(MerkleLabGIS)
}
