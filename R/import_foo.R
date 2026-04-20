# collection of import function

#' function to import a single ROI
#'
#' @param path file path of the ROI to import
#' @param pixel_fct the pixel size in microns
#' @param convert TRUE/FALSE to convert pixels values to microns
#' @return A sf object containing the coordinates defining the ROI
#' imported
importROI <- function(path, pixel_fct, convert = TRUE){
  roi_path = stringr::str_replace_all(path, "\\\\", "//")
  roi_data <- RImageJROI::read.ijroi(roi_path)
  # convert to sf package compatible
  roi_coords <- roi_data$coords
  # convert to units of micrometers
  if(convert){
    roi_coords <- roi_coords * pixel_fct
  }
  if (!all(roi_coords[1, ] == roi_coords[nrow(roi_coords), ])) {
    # Add the first coordinate at the end to close the polygon
    roi_coords <- rbind(roi_coords, roi_coords[1, ])
  }
  roi_polygon <- sf::st_polygon(list(roi_coords))
  # roi_sf <- sf::st_sfc(roi_polygon)
  return(roi_polygon)
}

#' Import all values from kernel extraction
#'
#' @param path file path of the grid coordinates to import
#' @return A numeric matrix with ROI means as rows and frames as columns.
#'   Row names are ROI labels (with "Mean" stripped), column names are
#'   sequential frame indices.
importGridVals <- function(path){
  path_fix = stringr::str_replace_all(path, "\\\\", "//")

  #read csv and transpose for more memory efficient handling
  matrix <- data.table::fread(path_fix) |> as.data.frame() |> t()

  #Formatting steps
  colnames(matrix) = seq(1:dim(matrix)[2])
  matrix = matrix[-1,]
  rownames(matrix) = stringr::str_remove(rownames(matrix), "Mean")
  return(matrix)
}

#' index_files
#'
#' @param paths file paths of the time series values to import
#'
#' @returns A tibble with one row per dataset and columns:
#'   \code{file_id} (filename without extension), \code{file_path}
#'   (full path to the original TIFF, or \code{NA} if not present), and
#'   \code{folder_path} (path to the corresponding results folder).
#'
#' @details
#' Discovery is attempted in three stages, stopping at the first success:
#' \enumerate{
#'   \item Scan for \code{*_results} subdirectories containing a
#'         \code{grid_centroids.csv} sentinel file.
#'   \item Read a \code{manifest.csv} in \code{paths$base_dir} with columns
#'         \code{file_id} and \code{folder_path}.
#'   \item Scan for TIFF files (original behaviour, requires images present).
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' index_files(paths = params$paths)
#' }
index_files <- function(paths) {

  # --- Stage 1: validated results folders ---
  result_dirs <- list.dirs(paths$base_dir, recursive = FALSE, full.names = TRUE)
  result_dirs <- result_dirs[grepl("_results$", basename(result_dirs))]
  valid_dirs  <- result_dirs[
    file.exists(file.path(result_dirs, "grid_centroids.csv"))
  ]

  if (length(valid_dirs) > 0) {
    folder_path <- valid_dirs
    file_id     <- stringr::str_remove(basename(folder_path), "_results$")
    tiff_path   <- file.path(paths$base_dir, paste0(file_id, ".tif"))
    return(tibble::tibble(
      file_id     = file_id,
      file_path   = ifelse(file.exists(tiff_path), tiff_path, NA_character_),
      folder_path = folder_path
    ))
  }

  # --- Stage 2: manifest.csv ---
  manifest_path <- file.path(paths$base_dir, "manifest.csv")
  if (file.exists(manifest_path)) {
    manifest <- utils::read.csv(manifest_path, stringsAsFactors = FALSE)
    if (!all(c("file_id", "folder_path") %in% names(manifest))) {
      stop("manifest.csv must contain columns 'file_id' and 'folder_path'")
    }
    tiff_path <- file.path(paths$base_dir, paste0(manifest$file_id, ".tif"))
    return(tibble::tibble(
      file_id     = manifest$file_id,
      file_path   = ifelse(file.exists(tiff_path), tiff_path, NA_character_),
      folder_path = manifest$folder_path
    ))
  }

  # --- Stage 3: TIFF scan (legacy fallback) ---
  files <- list.files(paths$base_dir, pattern = "(?i)\\.tif+$", full.names = TRUE)
  if (length(files) == 0) {
    stop(
      "No datasets found in ", paths$base_dir,
      ": no validated '_results' folders, no manifest.csv, and no TIFF files."
    )
  }
  file_id <- basename(files) |> stringr::str_remove("(?i)\\.tif+$")
  tibble::tibble(
    file_id     = file_id,
    file_path   = files,
    folder_path = file.path(paths$base_dir, paste0(file_id, "_results"))
  )
}

#' prepare_file
#'
#' @param file_row list of files to analyse, output of index_files function
#' @param params params object created by the make_params function
#'
#' @return a list containing the file_id, the ROI, the coordinates of each
#' grid element, the coordinates of each point as sf elements, the path to
#' the main folder, and the channels to be analysed
#' @export
prepare_file <- function(file_row, params) {

  scn_roi <- importROI(
    file.path(file_row$folder_path,
              paste0(file_row$file_id, "_SCN_nuclei.roi")),
    pixel_fct = params$pixel_fct
  )

  grid_coord <- read.csv(
    file.path(file_row$folder_path, "grid_centroids.csv")
  ) |> as.data.frame()

  grid_coord$X <- grid_coord$X*params$pixel_fct
  grid_coord$Y <- grid_coord$Y*params$pixel_fct

  grid_points_sf <- sf::st_as_sf(grid_coord, coords = c("X", "Y"))

  # Load all enabled channels

  channels <- purrr::imap(params$channels, function(ch, ch_id) {
    if (!ch$enabled) return(NULL)
    grid_vals_path <- file.path(file_row$folder_path, ch$grid_file)
    grid_vals <- importGridVals(grid_vals_path)
    list(
      grid_vals = grid_vals,
      grid_points_sf = grid_points_sf,
      scn_roi = scn_roi,
      grid_coord = grid_coord
      # channel-specific metadata can be added here
    )
  }) |> purrr::compact() # Remove NULLs for disabled channels

  list(
    file_id = file_row$file_id,
    scn_roi = scn_roi,
    grid_coord = grid_coord,
    grid_points_sf = grid_points_sf,
    folder_path = file_row$folder_path,
    channels = channels
  )
}

