
#' Title
#'
#' @return
#' @export
#'
#' @examples
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
#' @return
#' @export
#'
#' @examples
crop_cog_files <- function(cog_urls, shapefile_path, output_folder) {
  require(raster)

  # Load the shapefile as a spatial object
  shapefile <- shapefile(shapefile_path)

  # Initialize an empty list to store cropped rasters
  cropped_rasters <- list()

  # Iterate over the list of COG URLs
  for (cog_url in cog_urls) {
    # Prepend '/vsicurl/' to the COG URL
    cog_path <- paste0("/vsicurl/", cog_url)
    print(cog_path)

    # Load the COG as a raster
    cog_raster <- raster(cog_path)
    print(cog_raster)

    projected_shapefile <- spTransform(shapefile, projection(cog_raster))
    projection(projected_shapefile)

    # Crop the projected COG to the projected shapefile extent
    cropped_raster <- crop(cog_raster, projected_shapefile)
    plot(cropped_raster)
    # Extract the file name from the COG URL
    file_name <- basename(cog_url)

    # Construct the output file path
    output_path <- file.path(output_folder, file_name)
    print(output_path)
    # Write the cropped raster to the specified output path
    writeRaster(cropped_raster, output_path)

    # Add the cropped raster to the list
    cropped_rasters <- append(cropped_rasters, cropped_raster)

  }

  # Return the list of cropped rasters
  return(cropped_rasters)
}
