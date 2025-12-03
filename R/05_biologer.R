#' @title Filter Spatial Points Falling Within a Polygon
#' @description This function filters observation data points (`data`) to return only
#' those points that are geographically contained within the provided spatial polygon
#' (`polygon`). It handles automatic conversion of raw data into an sf object and ensures
#' Coordinate Reference Systems (CRS) match before performing the spatial join.
#'
#' @param polygon An object of class \code{sf} (or coercible to \code{sf}) representing
#' the area of interest (the spatial filter). Must be provided.
#' @param data A \code{data.table} or \code{sf} object containing observation points.
#' If \code{NULL}, the full dataset is loaded via \code{open_data()}. The data must contain
#' columns named 'decimalLongitude' and 'decimalLatitude' if passed as a \code{data.table}.
#' @param verbose Logical. If \code{TRUE}, messages about automatic data loading, CRS
#' conversion, and data conversion are printed. Defaults to \code{FALSE}.
#'
#' @return An object of class \code{sf} containing only the points from the input
#' \code{data} that fall strictly within the \code{polygon}.
#'
#' @details The function uses \code{sf::st_within} with \code{left = FALSE} to
#' ensure only points entirely inside the polygon are returned, and only once.
#' If the CRS of the \code{polygon} and \code{points} do not match, the
#' \code{polygon} is transformed to match the \code{points}' CRS.
#'
#' @importFrom sf st_crs st_transform st_as_sf st_join st_within st_sf st_zm
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming 'my_polygon_sf' is a pre-loaded sf polygon and 'my_data_dt' is a data.table
#'
#' # 1. Filter a pre-loaded data.table
#' filtered_data <- points_in_polygon(polygon = my_polygon_sf, data = my_data_dt)
#'
#' # 2. Filter the full dataset (data = NULL)
#' filtered_full <- points_in_polygon(polygon = my_polygon_sf, verbose = TRUE)
#' }
points_in_polygon <- function(polygon = NULL, data = NULL, verbose = FALSE) {
  if (is.null(polygon)) {
    stop("You myst provide a spatial polygon to extract points from it.", call. = FALSE)
  }

  if (is.null(data)) {
    if (verbose == TRUE) {
      message("No data specified, RBiologer will process the full dataset...")
    }
    data <- open_data(verbose = verbose)
  }

  ########################################################
  # Preparing the data points
  ########################################################

  if (inherits(data, "sf")) {
    if (verbose == TRUE) {
      message("Poits are already given as sf object.")
    }
    points_data <- data
  } else {
    if (verbose == TRUE) {
      message("Points are given as data.frame or data.table > converting into sf points.")
    }
    points_data <- st_as_sf(
      data,
      coords = c("decimalLongitude", "decimalLatitude"),
      crs = 4326
    )
  }

  ########################################################
  # Preparing the polygons
  ########################################################

  if (inherits(polygon, "sfc") && !inherits(polygon, "sf")) {
    if (verbose == TRUE) message("Input polygon is a geometry set (sfc). Converting to sf object.")
    polygon <- st_sf(geometry = polygon)
  }

  if (!inherits(polygon, "sf")) {
    polygon <- st_as_sf(polygon)
  }

  polygon <- st_zm(polygon, drop = TRUE, what = "ZM")

  if (st_crs(points_data) != st_crs(polygon)) {
    if (verbose == TRUE) {
      message("CRS of the points and polygon does not match. Converting the polygon.")
    }
    polygon <- st_transform(polygon, st_crs(points_data))
  }

  if (verbose == TRUE) {
    message(paste0("Performing spatial join: ",
                   nrow(points_data), " points (CRS:",
                   st_crs(points_data)$epsg, ") vs. ",
                   length(polygon), " polygon (CRS:",
                   st_crs(polygon)$epsg, ")."))
  }

  st_join(x = points_data,
          y = polygon,
          join = st_within,
          left = FALSE)
}

#' @title Filter and Standardize Data Based on License Restrictions
#' @description
#' This function processes observation data, applying license-based filters and
#' coordinate coarsening to meet data access requirements. It removes fully closed data,
#' coarsens coordinates for geographically limited data, and manages a 3-year time embargo
#' by updating licenses or removing observations that are still restricted.
#'
#' @param data A \code{data.table} or \code{data.frame} containing observation records. If
#' \code{NULL}, the full dataset is loaded via \code{open_data()}. The data must contain 'year',
#' 'month', 'day', 'mgrs10k', 'decimalLongitude', 'decimalLatitude', 'dcterms:accessRights',
#' and 'dcterms:license' columns.
#' @param verbose Logical. If \code{TRUE}, messages about automatic data loading are printed.
#' Defaults to \code{FALSE}.
#'
#' @return A \code{data.table} containing only the records that are publicly available (open
#' access or embargo passed), with coordinates coarsened for geographically restricted data.
#'
#' @details The function performs three main actions:
#' \enumerate{
#'   \item **Removal:** Data with \code{dcterms:accessRights} set to "Closed" are permanently removed.
#'   \item **Coarsening:** Data with "CC BY-NC-SA 4.0 (limited coordinates)" are spatially
#'          protected by replacing 'decimalLongitude' and 'decimalLatitude' with the centroid of the
#'          MGRS 10x10 km grid cell, and setting \code{coordinateUncertaintyInMeters} to
#'          7071m. \strong{Requires the \code{mgrs} package.}
#'   \item **Embargo Management:** Data with "CC BY-SA 4.0 (closed for 3 years)" are checked against
#'          the current date. If 3 years have passed, the access rights and license are updated to "CC BY-SA 4.0".
#'          If the embargo is ongoing, the records are permanently removed.
#' }
#'
#' @import data.table
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming 'my_data' is a data.table loaded from a Biologer server
#' processed_data <- filter_data_by_license(data = my_data)
#'
#' # Process the full dataset with verbose output
#' full_processed_data <- filter_data_by_license(verbose = TRUE)
#' }
filter_data_by_license <- function(data = NULL, verbose = FALSE) {
  coarse_uncertainty <- 7000L # Precission of the UTM centroid
  current_date <- Sys.Date()
  embargo_years <- 3
  closed_access <- "Closed"
  geographically_limited_access <- "CC BY-NC-SA 4.0 (limited coordinates)"
  temporary_limited_access <- "CC BY-SA 4.0 (closed for 3 years)"
  new_access_rights <- "CC BY-SA 4.0"
  new_license_url <- "https://creativecommons.org/licenses/by-sa/4.0/"
  data_no <- nrow(data)

  if (is.null(data)) {
    if (verbose == TRUE) {
      message("No data specified, RBiologer will process the full dataset...")
    }
    data <- open_data(verbose = verbose)
  }

  if (inherits(data, "data.frame") && !inherits(data, "data.table")) {
    data.table::setDT(data)
  }

  # Part 1. Completely remove data with restricted license
  if (verbose == TRUE) {
    message(paste0("Removing ", sum(data$`dcterms:accessRights` == closed_access),
                     " data with closed license."))
  }
  data <- data[`dcterms:accessRights` != closed_access]

  # Part 2. Get the polygon centroids for restricred data
  idx_coarse <- which(data$`dcterms:accessRights` == geographically_limited_access)
  if (length(idx_coarse) > 0) {
    if (verbose == TRUE) {
      message(paste0("Processing ", length(idx_coarse),
                     " geographically restricted records."))
    }
    coarse_subset <- data[idx_coarse]
    if ("geometry" %in% names(coarse_subset)) {
      # A: Input is a data.table subset *from* an sf object (has geometry column)
      coarse_data_for_centroid <- sf::st_as_sf(
        coarse_subset,
        wkt = "geometry",
        crs = 4326,
        remove = FALSE
      )
    } else if ("decimalLongitude" %in% names(coarse_subset) && "decimalLatitude" %in% names(coarse_subset)) {
      # B: Input is a standard data.table with required coordinate columns
      coarse_data_for_centroid <- sf::st_as_sf(
        coarse_subset,
        coords = c("decimalLongitude", "decimalLatitude"),
        crs = 4326,
        remove = FALSE
      )
    } else {
      # Error handling if the required columns are missing
      stop("Could not find a 'geometry' column or 'decimalLatitude'/'decimalLongitude' columns for coarsening.",
           call. = FALSE)
    }
    coarsened_data <- add_utm_centroid(
      data = coarse_data_for_centroid,
      cell_size = 10000
    )
    data[idx_coarse, `:=`(
      decimalLongitude = coarsened_data$mgrs10k_centroid_lon,
      decimalLatitude  = coarsened_data$mgrs10k_centroid_lat,
      coordinateUncertaintyInMeters  = coarse_uncertainty
    )]
  } else if (verbose == TRUE) {
    message("No records found with geographically restricted license for coarsening.")
  }

  # Part 3. Make data restricted for 3 years open after 3 years
  message(paste0("Processing ", sum(data$`dcterms:accessRights` == temporary_limited_access),
                   " data closed for 3 years."))
  if (nrow(data[`dcterms:accessRights` == temporary_limited_access]) > 0) {
    data[`dcterms:accessRights` == temporary_limited_access,
         `:=`(
           # Default missing month and day to 1 for calculation safety
           obs_month = data.table::fifelse(is.na(month), 1L, month),
           obs_day = data.table::fifelse(is.na(day), 1L, day)
         )]
    # Calculate embargo end date based on the safest available date
    data[`dcterms:accessRights` == temporary_limited_access,
         `:=`(
           observation_date = as.Date(paste(year, obs_month, obs_day, sep = "-")),
           embargo_end_date = as.Date(
             format(as.Date(paste(year, obs_month, obs_day, sep = "-")), "%Y-%m-%d")
           ) + as.difftime(embargo_years * 365.25, units = "days")
         )]
    # Update the data (make open if embargo period is over)
    data[embargo_end_date < current_date,
         `:=`(
           `dcterms:accessRights` = new_access_rights,
           `dcterms:license` = new_license_url
         )]
    # Delete records where the data is still restricted
    data <- data[!(
      `dcterms:accessRights` == temporary_limited_access & embargo_end_date >= current_date
    )]
    data[, `:=`(observation_date = NULL, embargo_end_date = NULL, obs_month = NULL, obs_day = NULL)]
  }

  if (verbose == TRUE) {
    message(paste0("A total of ", nrow(data), " data remain after processing (of the ",
                   data_no, " data before)"))
  }

  data
}


#' Subset Biologer Data
#'
#' Subset a Biologer dataframe based on specified column-value conditions.
#'
#' @param df A dataframe to subset. Defaults to the result of `open.data()` if NULL.
#' @param ... Named arguments specifying column names and the values to subset by.
#'            For example: `sex = "value"`, `taxon.name = "value"`, or
#'            `taxon.id = c("value1", "value2")`.
#' @return A subsetted dataframe.
#' @export
subset.biologer <- function(df = NULL, ...) {
  # Load default dataset if df is not supplied
  if (is.null(df)) {
    df <- open_data()
  }

  # Convert variable conditions into a list
  conditions <- list(...)

  # Ensure all provided column names exist in the dataframe
  invalid_cols <- setdiff(names(conditions), names(df))
  if (length(invalid_cols) > 0) {
    stop("Invalid column names: ", paste(invalid_cols, collapse = ", "))
  }

  # Dynamically build filtering logic
  subset_df <- df
  for (col in names(conditions)) {
    values <- conditions[[col]]
    if (length(values) > 1) {
      # Use `%in%` for multiple values
      subset_df <- subset_df[subset_df[[col]] %in% values, ]
    } else {
      # Use `==` for single value
      subset_df <- subset_df[subset_df[[col]] == values, ]
    }
  }

  # Return the subsetted dataframe
  return(subset_df)
}


#############################################################

#' Plot RH_2018 Shapefile
#'
#' Loads and plots the RH_2018 shapefile included in the package.
#'
#' @return A ggplot object showing the RH_2018 map.
#' @export
plot.hr <- function() {
  library(sf)
  library(ggplot2)

  # Locate and load the shapefile
  shapefile_path <- system.file("extdata", "RH_2018.shp", package = "biologerR")
  if (shapefile_path == "") stop("Shapefile not found in the package.")

  rh_data <- sf::st_read(shapefile_path, quiet = TRUE)

  # Create the plot
  ggplot(data = rh_data) +
    geom_sf(fill = "lightblue", color = "black") +
    theme_minimal()
}


#' Plot Zupanije JSON
#'
#' Loads and plots the Zupanije.json file included in the package.
#'
#' @return A ggplot object showing the Zupanije map.
#' @export
plot.zupanija <- function() {
  library(sf)
  library(ggplot2)

  # Locate the GeoJSON file
  json_path <- system.file("extdata", "zupanije.json", package = "biologerR")
  if (json_path == "") stop("GeoJSON file not found in the package.")

  # Read the GeoJSON
  zupanije_sf <- sf::st_read(json_path, quiet = TRUE)

  # Add region centroids for labeling
  zupanije_sf$centroid <- sf::st_centroid(zupanije_sf$geometry)
  centroids <- sf::st_coordinates(zupanije_sf$centroid)
  zupanije_sf$lon <- centroids[, 1]
  zupanije_sf$lat <- centroids[, 2]

  # Plot with labels
  ggplot(data = zupanije_sf) +
    geom_sf(fill = "lightgreen", color = "darkgreen") +
    geom_text(aes(x = lon, y = lat, label = name), size = 2, color = "blue") +
    theme_minimal()
}


#' Plot Biologer Observations on a Map
#'
#' Plots latitude and longitude of observations from the Biologer dataframe (`df`) on
#' either the RH_2018 or Zupanije map.
#'
#' @param fill Column name to use for coloring points. Default is "taxon.name".
#' @param shape Column name to use for point shapes. Default is NULL (no shapes).
#' @param layer Map layer to use: "rh" for RH_2018.shp or "zupanije" for Zupanije.json.
#' @param df A dataframe containing Biologer data with latitude and longitude columns.
#' @param title Plot title. Default is NULL (no title).
#' @param x_axis_label X-axis label. Default is NULL (no label).
#' @param y_axis_label Y-axis label. Default is NULL (no label).
#' @param legend_fill_title Title for the fill legend. Default is NULL (no title).
#' @param legend_shape_title Title for the shape legend. Default is NULL (no title).
#' @param dot_size Size of the points. Default is 2.
#' @param layer_fill_color Fill color for the map layer. Default is "lightblue".
#' @param layer_border_color Border color for the map layer. Default is "black".
#' @param layer_border_width Border width for the map layer. Default is 0.5.
#' @param palette Color palette for points. Can be a predefined name or a custom named vector.
#'
#' @return A ggplot object showing observations on the selected map layer.
#' @export
plot.biologer <- function(fill = "taxon.name", shape = NULL, layer = "rh", df,
                          title = NULL, x_axis_label = NULL, y_axis_label = NULL,
                          legend_fill_title = NULL, legend_shape_title = NULL,
                          dot_size = 2,
                          layer_fill_color = "lightblue",
                          layer_border_color = "black",
                          layer_border_width = 0.5,
                          palette = "viridis") {
  library(sf)
  library(ggplot2)
  library(scales)

  # Remove rows with missing coordinates
  df <- df[!is.na(df$latitude) & !is.na(df$longitude), ]
  message(nrow(df), " rows remain after removing rows with missing latitude or longitude.")

  # Load the selected map layer
  map_layer <- NULL
  if (layer == "rh") {
    shapefile_path <- system.file("extdata", "RH_2018.shp", package = "biologerR")
    if (shapefile_path == "") stop("RH_2018 shapefile not found in the package.")
    map_layer <- sf::st_read(shapefile_path, quiet = TRUE)
  } else if (layer == "zupanije") {
    json_path <- system.file("extdata", "zupanije.json", package = "biologerR")
    if (json_path == "") stop("Zupanije GeoJSON file not found in the package.")
    map_layer <- sf::st_read(json_path, quiet = TRUE)
  } else {
    stop("Invalid layer specified. Use 'rh' for RH_2018 or 'zupanije'.")
  }

  # Convert the Biologer data to an sf object
  df_sf <- sf::st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

  # Transform Biologer data to match the CRS of the map layer
  df_sf <- sf::st_transform(df_sf, crs = sf::st_crs(map_layer))

  # Generate a dynamic palette if needed
  unique_fill_values <- unique(df_sf[[fill]])
  if (is.character(palette) && !is.null(names(palette))) {
    # Custom palette provided
    if (!all(unique_fill_values %in% names(palette))) {
      missing_values <- setdiff(unique_fill_values, names(palette))
      stop("The custom palette is missing colors for: ", paste(missing_values, collapse = ", "))
    }
    colors <- palette
  } else if (is.character(palette) && length(palette) == 1) {
    # Predefined palette
    color_count <- length(unique_fill_values)
    if (palette == "viridis") {
      colors <- viridis::viridis(color_count)
    } else if (palette == "plasma") {
      colors <- viridis::plasma(color_count)
    } else if (palette == "rainbow") {
      colors <- rainbow(color_count)
    } else {
      colors <- scales::hue_pal()(color_count)
    }
    names(colors) <- unique_fill_values
  } else {
    stop("Invalid palette. Use a predefined palette name or a named list of colors.")
  }

  # Build the ggplot
  p <- ggplot() +
    geom_sf(data = map_layer, fill = layer_fill_color, color = layer_border_color, size = layer_border_width) +
    geom_sf(
      data = df_sf,
      aes_string(color = fill, shape = shape),
      size = dot_size
    ) +
    scale_color_manual(values = colors) +
    theme_minimal()

  # Add custom title and axis labels
  if (!is.null(title)) {
    p <- p + ggtitle(title)
  }
  if (!is.null(x_axis_label)) {
    p <- p + xlab(x_axis_label)
  }
  if (!is.null(y_axis_label)) {
    p <- p + ylab(y_axis_label)
  }

  # Customize the legends if titles are provided
  if (!is.null(legend_fill_title)) {
    p <- p + scale_color_manual(name = legend_fill_title, values = colors)
  }
  if (!is.null(legend_shape_title)) {
    p <- p + scale_shape_discrete(name = legend_shape_title)
  }

  return(p)
}


#' Count Observations Per Zupanija
#'
#' Counts the number of observations per Zupanija based on the coordinates of rows in the dataframe.
#'
#' @param fill Column name to group and count rows by. Default is "taxon.name".
#' @param df A dataframe containing latitude and longitude columns.
#' @param selected_zupanije Optional vector of županija names to filter by. Default is NULL (all županije).
#'
#' @return A dataframe with counts for each Zupanija and a TOTAL row.
#' @export
zupanije.biologer <- function(fill = "taxon.name", df, selected_zupanije = NULL) {
  library(sf)
  library(dplyr)

  # Remove rows with missing coordinates
  df <- df[!is.na(df$latitude) & !is.na(df$longitude), ]
  message(nrow(df), " rows remain after removing rows with missing latitude or longitude.")

  # Load Zupanije map layer
  json_path <- system.file("extdata", "zupanije.json", package = "biologerR")
  if (json_path == "") stop("Zupanije GeoJSON file not found in the package.")
  zupanije_sf <- sf::st_read(json_path, quiet = TRUE)

  # Filter županije if specified
  if (!is.null(selected_zupanije)) {
    if (!all(selected_zupanije %in% zupanije_sf$name)) {
      invalid_zupanije <- selected_zupanije[!selected_zupanije %in% zupanije_sf$name]
      stop("Invalid županija names: ", paste(invalid_zupanije, collapse = ", "))
    }
    zupanije_sf <- zupanije_sf[zupanije_sf$name %in% selected_zupanije, ]
  }

  # Convert the Biologer data to an sf object
  df_sf <- sf::st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

  # Spatial join to find which Zupanija each point falls into
  joined_data <- sf::st_join(df_sf, zupanije_sf, join = sf::st_within)

  # Count rows per Zupanija and grouping variable
  if (fill == "none") {
    counts <- joined_data %>%
      group_by(name) %>%
      summarise(count = n(), .groups = "drop") %>%
      mutate(!!fill := "ALL") # Add a default "ALL" column when no grouping
  } else {
    counts <- joined_data %>%
      group_by(!!sym(fill), name) %>%
      summarise(count = n(), .groups = "drop")
  }

  # Add TOTAL row for each group and for the entire dataset
  total_per_group <- counts %>%
    group_by(!!sym(fill)) %>%
    summarise(name = "TOTAL", count = sum(count), .groups = "drop")

  total_overall <- counts %>%
    summarise(!!sym(fill) := "TOTAL", name = "TOTAL", count = sum(count), .groups = "drop")

  # Combine everything into a single dataframe
  final_counts <- bind_rows(counts, total_per_group, total_overall)

  # Arrange for better readability
  final_counts <- final_counts %>%
    arrange(!!sym(fill), name)

  # Print the result
  print(final_counts)
  return(final_counts)
}


#' Plot Biologer Observations for Specific Zupanije
#'
#' Plots observations within one or more Zupanije based on coordinates and user specifications.
#'
#' @param fill Column name to use for coloring points. Default is "taxon.name".
#' @param shape Column name to use for point shapes. Default is NULL (no shapes).
#' @param zupanija Vector of Zupanija names to filter and plot.
#' @param df A dataframe containing latitude and longitude columns.
#' @param title Plot title. Default is NULL (no title).
#' @param x_axis_label X-axis label. Default is NULL (no label).
#' @param y_axis_label Y-axis label. Default is NULL (no label).
#' @param legend_fill_title Title for the fill legend. Default is NULL (no title).
#' @param legend_shape_title Title for the shape legend. Default is NULL (no title).
#' @param dot_size Size of the points. Default is 2.
#' @param layer_fill_color Fill color for the map layer. Default is "lightblue".
#' @param layer_border_color Border color for the map layer. Default is "black".
#' @param layer_border_width Border width for the map layer. Default is 0.5.
#' @param palette Color palette for points. Can be a predefined name or a custom named vector.
#'
#' @return A ggplot object showing observations for the specified Zupanije.
#' @export
plot.zupanije <- function(fill = "taxon.name", shape = NULL, zupanija, df,
                          title = NULL, x_axis_label = NULL, y_axis_label = NULL,
                          legend_fill_title = NULL, legend_shape_title = NULL,
                          dot_size = 2,
                          layer_fill_color = "lightblue",
                          layer_border_color = "black",
                          layer_border_width = 0.5,
                          palette = "viridis") {
  library(sf)
  library(ggplot2)
  library(scales)

  # Remove rows with missing coordinates
  df <- df[!is.na(df$latitude) & !is.na(df$longitude), ]
  message(nrow(df), " rows remain after removing rows with missing latitude or longitude.")

  # Load Zupanije map layer
  json_path <- system.file("extdata", "zupanije.json", package = "biologerR")
  if (json_path == "") stop("Zupanije GeoJSON file not found in the package.")
  zupanije_sf <- sf::st_read(json_path, quiet = TRUE)

  # Filter for the selected Zupanije
  zupanije_filtered <- zupanije_sf[zupanije_sf$name %in% zupanija, ]
  if (nrow(zupanije_filtered) == 0) stop("None of the specified Zupanije were found.")

  # Convert the Biologer data to an sf object
  df_sf <- sf::st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

  # Transform Biologer data to match the CRS of the Zupanija map layer
  df_sf <- sf::st_transform(df_sf, crs = sf::st_crs(zupanije_sf))

  # Spatial join to filter points within the selected Zupanije
  points_in_zupanije <- sf::st_join(df_sf, zupanije_filtered, join = sf::st_within, left = FALSE)

  # Generate a dynamic palette if needed
  unique_fill_values <- unique(points_in_zupanije[[fill]])
  if (is.character(palette) && !is.null(names(palette))) {
    # Custom palette provided
    if (!all(unique_fill_values %in% names(palette))) {
      missing_values <- setdiff(unique_fill_values, names(palette))
      stop("The custom palette is missing colors for: ", paste(missing_values, collapse = ", "))
    }
    colors <- palette
  } else if (is.character(palette) && length(palette) == 1) {
    # Predefined palette
    color_count <- length(unique_fill_values)
    if (palette == "viridis") {
      colors <- viridis::viridis(color_count)
    } else if (palette == "plasma") {
      colors <- viridis::plasma(color_count)
    } else if (palette == "rainbow") {
      colors <- rainbow(color_count)
    } else {
      colors <- scales::hue_pal()(color_count)
    }
    names(colors) <- unique_fill_values
  } else {
    stop("Invalid palette. Use a predefined palette name or a named list of colors.")
  }

  # Build the ggplot
  p <- ggplot() +
    geom_sf(data = zupanije_filtered, fill = layer_fill_color, color = layer_border_color, size = layer_border_width) +
    geom_sf(
      data = points_in_zupanije,
      aes_string(color = fill, shape = shape),
      size = dot_size
    ) +
    scale_color_manual(values = colors) +
    theme_minimal()

  # Add custom title and axis labels
  if (!is.null(title)) {
    p <- p + ggtitle(title)
  }
  if (!is.null(x_axis_label)) {
    p <- p + xlab(x_axis_label)
  }
  if (!is.null(y_axis_label)) {
    p <- p + ylab(y_axis_label)
  }

  # Customize the legends if titles are provided
  if (!is.null(legend_fill_title)) {
    p <- p + scale_color_manual(name = legend_fill_title, values = colors)
  }
  if (!is.null(legend_shape_title)) {
    p <- p + scale_shape_discrete(name = legend_shape_title)
  }

  return(p)
}

#################################### 3
#' Plot Observations Within a Polygon
#'
#' This function plots observations within a user-defined polygon,
#' providing both a detailed plot with observations and a location overview.
#'
#' @param fill Column name for coloring points. Default is "taxon.name".
#' @param shape Column name for point shapes. Default is NULL (no shapes).
#' @param polygon_coords A data frame or list of data frames defining polygon(s) with longitude and latitude columns.
#' @param df A dataframe containing latitude and longitude columns, as well as any additional data for plotting.
#' @param layer Map layer to use for the background: "rh" or "zupanije". Default is "zupanije".
#' @param zupanija Vector of Zupanija names to filter and plot. Default is NULL (plots all).
#' @param padding Padding in meters around the polygon to adjust the map extent. Default is 0.
#' @param title Title for the plot with observations. Default is NULL.
#' @param x_axis_label Label for the X-axis. Default is NULL.
#' @param y_axis_label Label for the Y-axis. Default is NULL.
#' @param legend_fill_title Title for the fill legend. Default is NULL.
#' @param legend_shape_title Title for the shape legend. Default is NULL.
#' @param dot_size Size of the observation points. Default is 2.
#' @param layer_fill_color Fill color for the map layer. Default is "lightblue".
#' @param layer_border_color Border color for the map layer. Default is "black".
#' @param layer_border_width Border width for the map layer. Default is 0.5.
#' @param polygon_fill_color Fill color for the polygon. Default is "orange".
#' @param polygon_border_color Border color for the polygon. Default is "red".
#' @param polygon_border_width Border width for the polygon. Default is 1.
#' @param palette Color palette for points. Can be a predefined palette (e.g., "viridis")
#' or a custom named vector. Default is "viridis".
#'
#' @return A list containing two ggplot objects:
#'   - `plot_with_points`: Plot showing the observations within the polygon.
#'   - `polygon_location`: Plot showing only the polygon over the selected map layer.
#' @export
plot.polygon <- function(fill = "taxon.name",
                         shape = NULL,
                         polygon_coords,
                         df,
                         layer = "zupanije",
                         zupanija = NULL,
                         padding = 0,
                         title = NULL,
                         x_axis_label = NULL,
                         y_axis_label = NULL,
                         legend_fill_title = NULL,
                         legend_shape_title = NULL,
                         dot_size = 2,
                         layer_fill_color = "lightblue",
                         layer_border_color = "black",
                         layer_border_width = 0.5,
                         polygon_fill_color = "orange",
                         polygon_border_color = "red",
                         polygon_border_width = 1,
                         palette = "viridis") {
  library(sf)
  library(ggplot2)
  library(scales)

  # Handle single polygon or list of polygons
  if (inherits(polygon_coords, "data.frame")) {
    polygon_coords_list <- list(polygon_coords)
  } else if (is.list(polygon_coords) && all(sapply(polygon_coords, inherits, "data.frame"))) {
    polygon_coords_list <- polygon_coords
  } else {
    stop("polygon_coords must be a data frame or a list of data frames.")
  }

  # Create sf polygons with closing points
  create_polygon <- function(coords) {
    # Ensure the first and last points match to 'close' the polygon
    if (!all(coords[1, ] == coords[nrow(coords), ])) {
      coords <- rbind(coords, coords[1, ])
    }
    sf::st_polygon(list(as.matrix(coords)))
  }

  polygons_sfg <- lapply(polygon_coords_list, create_polygon)
  polygons_sfc <- sf::st_sfc(polygons_sfg, crs = 4326)
  polygons_sf <- sf::st_sf(geometry = polygons_sfc)

  # Calculate bounding box with optional padding
  if (padding > 0) {
    combined_poly <- sf::st_union(polygons_sf)
    centroid <- sf::st_centroid(combined_poly)
    coords <- sf::st_coordinates(centroid)
    lon <- coords[1, "X"]
    lat <- coords[1, "Y"]
    zone <- floor((lon + 180) / 6) + 1
    hemisphere <- ifelse(lat >= 0, "north", "south")
    crs_utm <- sprintf("+proj=utm +zone=%d +%s +datum=WGS84", zone, hemisphere)

    combined_poly_utm <- sf::st_transform(combined_poly, crs = crs_utm)
    buffered_utm <- sf::st_buffer(combined_poly_utm, dist = padding)
    buffered_poly <- sf::st_transform(buffered_utm, crs = 4326)
    bbox <- sf::st_bbox(buffered_poly)
  } else {
    bbox <- sf::st_bbox(polygons_sf)
  }
  xlim <- c(bbox["xmin"], bbox["xmax"])
  ylim <- c(bbox["ymin"], bbox["ymax"])

  # Remove rows with missing coordinates
  df <- df[!is.na(df$latitude) & !is.na(df$longitude), ]
  message(nrow(df), " rows remain after removing NA coordinates.")
  df_sf <- sf::st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

  # Load the requested map layer
  if (layer == "rh") {
    shapefile_path <- system.file("extdata", "RH_2018.shp", package = "biologerR")
    if (shapefile_path == "") stop("RH_2018 shapefile not found in the biologerR package.")
    map_layer <- sf::st_read(shapefile_path, quiet = TRUE)
  } else if (layer == "zupanije") {
    json_path <- system.file("extdata", "zupanije.json", package = "biologerR")
    if (json_path == "") stop("Zupanije GeoJSON not found in the biologerR package.")
    map_layer <- sf::st_read(json_path, quiet = TRUE)

    # Filter if specific zupanija is provided
    if (!is.null(zupanija)) {
      map_layer <- map_layer[map_layer$name %in% zupanija, ]
      if (nrow(map_layer) == 0) {
        stop("None of the specified Zupanija names were found in the map layer.")
      }
    }
  } else {
    stop("Invalid layer. Use 'rh' or 'zupanije'.")
  }

  # Ensure map layer has correct CRS
  map_layer <- sf::st_transform(map_layer, crs = 4326)

  # Filter points that fall within the polygons
  points_in_poly <- sf::st_join(df_sf, polygons_sf, join = sf::st_within, left = FALSE)

  # Prepare color palette
  unique_fill <- unique(points_in_poly[[fill]])
  if (is.null(unique_fill)) {
    warning("No points found within the polygon(s). No points will be plotted.")
    unique_fill <- character(0) # handle empty
  }

  if (is.character(palette) && !is.null(names(palette))) {
    # A custom named palette was provided
    if (!all(unique_fill %in% names(palette))) {
      missing <- setdiff(unique_fill, names(palette))
      stop("The provided palette is missing colors for: ", paste(missing, collapse = ", "))
    }
    colors <- palette
  } else if (is.character(palette) && length(palette) == 1) {
    # One of the predefined palettes
    n <- length(unique_fill)
    colors <- switch(palette,
      "viridis" = viridis::viridis(n),
      "plasma"  = viridis::plasma(n),
      "rainbow" = rainbow(n),
      # default fallback:
      scales::hue_pal()(n)
    )
    names(colors) <- unique_fill
  } else {
    stop("Invalid palette. Use a named vector of colors or a predefined palette (e.g., 'viridis').")
  }

  # Main plot (with points)
  plot_main <- ggplot() +
    geom_sf(
      data = map_layer,
      fill = layer_fill_color,
      color = layer_border_color,
      linewidth = layer_border_width
    ) +
    geom_sf(
      data = polygons_sf,
      fill = polygon_fill_color,
      color = polygon_border_color,
      linewidth = polygon_border_width
    ) +
    # Only add geom_sf for points if we have any
    {
      if (nrow(points_in_poly) > 0) {
        geom_sf(
          data = points_in_poly,
          aes_string(color = fill, shape = shape),
          size = dot_size
        )
      }
    } +
    {
      if (length(unique_fill) > 0) {
        scale_color_manual(values = colors)
      }
    } +
    coord_sf(xlim = xlim, ylim = ylim) +
    labs(
      title = title,
      x = x_axis_label,
      y = y_axis_label,
      color = legend_fill_title,
      shape = legend_shape_title
    ) +
    theme_minimal()

  # Location overview plot (just the polygons and the chosen layer)
  plot_location <- ggplot() +
    geom_sf(
      data = map_layer,
      fill = layer_fill_color,
      color = layer_border_color,
      linewidth = layer_border_width
    ) +
    geom_sf(
      data = polygons_sf,
      fill = polygon_fill_color,
      color = polygon_border_color,
      linewidth = polygon_border_width
    ) +
    coord_sf() +
    labs(title = "Polygon Location Overview") +
    theme_minimal()

  # Return both plots in a list
  return(list(
    plot_with_points = plot_main,
    polygon_location = plot_location
  ))
}


#' Count Observations Per Polygon
#'
#' Counts the number of observations that fall within each provided polygon.
#'
#' @param fill Column name to group and count rows by. Default is "taxon.name".
#' @param polygon_coords A data frame or list of data frames defining polygon(s) with longitude and latitude columns.
#' @param polygon_names Optional vector of names for the polygons. If not provided, polygons will be numbered.
#' @param df A dataframe containing latitude and longitude columns, as well as the column specified in fill.
#'
#' @return A dataframe with counts for each polygon and a TOTAL row.
#' @export
polygon.counts <- function(fill = "taxon.name", polygon_coords, polygon_names = NULL, df) {
  library(sf)
  library(dplyr)

  # Remove rows with missing coordinates
  df <- df[!is.na(df$latitude) & !is.na(df$longitude), ]
  message(nrow(df), " rows remain after removing rows with missing latitude or longitude.")

  # Handle single polygon or list of polygons
  if (inherits(polygon_coords, "data.frame")) {
    polygon_coords_list <- list(polygon_coords)
  } else if (is.list(polygon_coords) && all(sapply(polygon_coords, inherits, "data.frame"))) {
    polygon_coords_list <- polygon_coords
  } else {
    stop("polygon_coords must be a data frame or a list of data frames.")
  }

  # Create polygon names if not provided
  if (is.null(polygon_names)) {
    polygon_names <- paste("Polygon", seq_along(polygon_coords_list))
  }
  if (length(polygon_names) != length(polygon_coords_list)) {
    stop("Length of polygon_names must match the number of polygons.")
  }

  # Create sf polygons with closing points
  create_polygon <- function(coords) {
    if (!all(coords[1, ] == coords[nrow(coords), ])) {
      coords <- rbind(coords, coords[1, ])
    }
    sf::st_polygon(list(as.matrix(coords)))
  }

  polygons_sfg <- lapply(polygon_coords_list, create_polygon)
  polygons_sfc <- sf::st_sfc(polygons_sfg, crs = 4326)
  polygons_sf <- sf::st_sf(
    name = polygon_names,
    geometry = polygons_sfc
  )

  # Convert the data to an sf object
  df_sf <- sf::st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

  # Spatial join to find which polygon each point falls into
  joined_data <- sf::st_join(df_sf, polygons_sf, join = sf::st_within)

  # Count rows per polygon and grouping variable
  if (fill == "none") {
    counts <- joined_data %>%
      group_by(name) %>%
      summarise(count = n(), .groups = "drop") %>%
      mutate(!!fill := "ALL") # Add a default "ALL" column when no grouping
  } else {
    counts <- joined_data %>%
      group_by(!!sym(fill), name) %>%
      summarise(count = n(), .groups = "drop")
  }

  # Add TOTAL row for each group and for the entire dataset
  total_per_group <- counts %>%
    group_by(!!sym(fill)) %>%
    summarise(name = "TOTAL", count = sum(count), .groups = "drop")

  total_overall <- counts %>%
    summarise(!!sym(fill) := "TOTAL", name = "TOTAL", count = sum(count), .groups = "drop")

  # Combine everything into a single dataframe
  final_counts <- bind_rows(counts, total_per_group, total_overall)

  # Arrange for better readability
  final_counts <- final_counts %>%
    arrange(!!sym(fill), name)

  # Print the result
  print(final_counts)
  return(final_counts)
}


###############################################################
#' Plot Observations Within EEA Grid Square(s)
#'
#' This function plots observations within user-selected EEA grid square(s). It provides both
#' a detailed plot with observations (if any fall within the squares) and a location overview
#' of the selected squares.
#'
#' @param fill Column name for coloring points. Default is "taxon.name".
#' @param shape Column name for point shapes. Default is NULL (no shapes). If provided, this
#'   column must exist in \code{df}; otherwise shape aesthetics are omitted.
#' @param grid_type Which grid layer to use: "1km" or "10km". Default is "1km".
#' @param square_id A vector of square IDs to filter and plot (e.g., c("10kmE459N247", "10kmE460N244")).
#'   Default is NULL (plots all squares in the chosen grid).
#' @param df A data frame containing \strong{at least} `longitude` and `latitude` columns, plus
#'   any additional columns you want for \code{fill} or \code{shape}.
#' @param layer Map layer to use for the background: "rh" or "zupanije". Default is "zupanije".
#' @param zupanija Vector of Zupanija names to filter and plot. Default is NULL (plots all).
#' @param padding Padding in meters around the selected square(s) to adjust the map extent. Default is 0.
#' @param title Title for the plot with observations. Default is NULL.
#' @param x_axis_label Label for the X-axis. Default is NULL.
#' @param y_axis_label Label for the Y-axis. Default is NULL.
#' @param legend_fill_title Title for the fill legend. Default is NULL.
#' @param legend_shape_title Title for the shape legend. Default is NULL.
#' @param dot_size Size of the observation points. Default is 2.
#' @param layer_fill_color Fill color for the map layer. Default is "lightblue".
#' @param layer_border_color Border color for the map layer. Default is "black".
#' @param layer_border_width Border width for the map layer. Default is 0.5.
#' @param grid_fill_color Fill color for the selected grid squares. Default is "orange".
#' @param grid_border_color Border color for the selected grid squares. Default is "red".
#' @param grid_border_width Border width for the selected grid squares. Default is 1.
#' @param palette Color palette for points. Can be a predefined palette (e.g., "viridis", "plasma",
#'   "rainbow") or a custom named vector. Default is "viridis".
#'
#' @return A list containing two ggplot objects:
#'   \itemize{
#'     \item \code{plot_with_points}: Plot showing the observations (if any) within the selected square(s).
#'     \item \code{square_location}: Plot showing only the selected square(s) over the chosen map layer.
#'   }
#' @export
#'
#' @examples
#' \dontrun{
#' # Example data
#' my_data <- data.frame(
#'   taxon.name = c("Species A", "Species B"),
#'   longitude  = c(16.0, 15.5),
#'   latitude   = c(45.8, 45.9),
#'   shape_col  = c("Circle", "Triangle") # Example shape column
#' )
#'
#' # Plot single 10×10 km square with shape
#' single_10km <- plot.squares(
#'   fill               = "taxon.name",
#'   shape              = "shape_col",
#'   grid_type          = "10km",
#'   square_id          = c("10kmE459N248"),
#'   df                 = my_data,
#'   layer              = "zupanije",
#'   zupanija           = c("Zagreb"),
#'   padding            = 5000,
#'   title              = "Single 10×10 km Square",
#'   x_axis_label       = "Longitude",
#'   y_axis_label       = "Latitude",
#'   legend_fill_title  = "Taxon Legend",
#'   legend_shape_title = "Shape Legend",
#'   dot_size           = 3,
#'   layer_fill_color   = "grey90",
#'   layer_border_color = "blue",
#'   layer_border_width = 0.8,
#'   grid_fill_color    = "yellow",
#'   grid_border_color  = "red",
#'   grid_border_width  = 1.5,
#'   palette            = "plasma"
#' )
#'
#' # Access the plots
#' single_10km$plot_with_points
#' single_10km$square_location
#' }
plot.squares <- function(fill = "taxon.name",
                         shape = NULL,
                         grid_type = c("1km", "10km"),
                         square_id = NULL,
                         df,
                         layer = "zupanije",
                         zupanija = NULL,
                         padding = 0,
                         title = NULL,
                         x_axis_label = NULL,
                         y_axis_label = NULL,
                         legend_fill_title = NULL,
                         legend_shape_title = NULL,
                         dot_size = 2,
                         layer_fill_color = "lightblue",
                         layer_border_color = "black",
                         layer_border_width = 0.5,
                         grid_fill_color = "orange",
                         grid_border_color = "red",
                         grid_border_width = 1,
                         palette = "viridis") {
  # Load required packages
  library(sf)
  library(ggplot2)
  library(scales)

  grid_type <- match.arg(grid_type)

  #------------------------------------------------------------------------
  # 1) Load the EEA grid shapefile (with × in filenames) from your package
  #------------------------------------------------------------------------
  if (grid_type == "1km") {
    grid_path <- system.file("extdata", "EEA_mreza", "RH_EEA_1×1km.shp", package = "biologerR")
    if (grid_path == "") {
      stop("1×1 km EEA shapefile not found in the biologerR package. Please check your installation.")
    }
    grid_layer <- sf::st_read(grid_path, quiet = TRUE)
  } else {
    grid_path <- system.file("extdata", "EEA_mreza", "RH_EEA_10×10km.shp", package = "biologerR")
    if (grid_path == "") {
      stop("10×10 km EEA shapefile not found in the biologerR package. Please check your installation.")
    }
    grid_layer <- sf::st_read(grid_path, quiet = TRUE)

    # Rename CellCode -> cellcode if needed
    if ("CellCode" %in% names(grid_layer) && !"cellcode" %in% names(grid_layer)) {
      grid_layer <- dplyr::rename(grid_layer, cellcode = "CellCode")
    }
  }

  # Make sure grid layer is in WGS84
  grid_layer <- sf::st_transform(grid_layer, crs = 4326)

  #------------------------------------------------------------------------
  # 2) Filter squares if square_id provided; else all squares
  #------------------------------------------------------------------------
  if (!is.null(square_id)) {
    sel_idx <- which(grid_layer$cellcode %in% square_id)
    if (length(sel_idx) == 0) {
      stop("None of the specified square_id values were found in the chosen grid.")
    }
    grid_layer <- grid_layer[sel_idx, ]
  }

  #------------------------------------------------------------------------
  # 3) Compute bounding box with optional padding
  #------------------------------------------------------------------------
  combined_grid <- sf::st_union(grid_layer)
  if (padding > 0) {
    centroid <- sf::st_centroid(combined_grid)
    coords <- sf::st_coordinates(centroid)
    lon <- coords[1, "X"]
    lat <- coords[1, "Y"]
    zone <- floor((lon + 180) / 6) + 1
    hemisphere <- ifelse(lat >= 0, "north", "south")
    crs_utm <- sprintf("+proj=utm +zone=%d +%s +datum=WGS84", zone, hemisphere)

    combined_grid_utm <- sf::st_transform(combined_grid, crs = crs_utm)
    buffered_utm <- sf::st_buffer(combined_grid_utm, dist = padding)
    buffered_grid <- sf::st_transform(buffered_utm, crs = 4326)
    bbox <- sf::st_bbox(buffered_grid)
  } else {
    bbox <- sf::st_bbox(combined_grid)
  }
  xlim <- c(bbox["xmin"], bbox["xmax"])
  ylim <- c(bbox["ymin"], bbox["ymax"])

  #------------------------------------------------------------------------
  # 4) Prepare points data: remove NAs, convert to sf
  #------------------------------------------------------------------------
  df <- df[!is.na(df$latitude) & !is.na(df$longitude), ]
  message(nrow(df), " rows remain after removing NA coordinates.")

  df_sf <- sf::st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

  #------------------------------------------------------------------------
  # 5) Load the map layer (rh or zupanije), filter by zupanija if needed
  #------------------------------------------------------------------------
  if (layer == "rh") {
    shapefile_path <- system.file("extdata", "RH_2018.shp", package = "biologerR")
    if (shapefile_path == "") {
      stop("RH_2018 shapefile not found in the biologerR package. Please check your installation.")
    }
    map_layer <- sf::st_read(shapefile_path, quiet = TRUE)
  } else if (layer == "zupanije") {
    json_path <- system.file("extdata", "zupanije.json", package = "biologerR")
    if (json_path == "") {
      stop("Zupanije GeoJSON not found in the biologerR package. Please check your installation.")
    }
    map_layer <- sf::st_read(json_path, quiet = TRUE)

    if (!is.null(zupanija)) {
      map_layer <- map_layer[map_layer$name %in% zupanija, ]
      if (nrow(map_layer) == 0) {
        stop("None of the specified Zupanija names were found in the map layer.")
      }
    }
  } else {
    stop("Invalid layer. Use 'rh' or 'zupanije'.")
  }

  map_layer <- sf::st_transform(map_layer, crs = 4326)

  #------------------------------------------------------------------------
  # 6) Join points with the selected grid squares
  #------------------------------------------------------------------------
  points_in_grid <- sf::st_join(df_sf, grid_layer, join = sf::st_within, left = FALSE)

  #------------------------------------------------------------------------
  # 7) Prepare color palette
  #------------------------------------------------------------------------
  unique_fill <- unique(points_in_grid[[fill]])
  if (is.null(unique_fill) || length(unique_fill) == 0) {
    # No points found
    warning("No points found within the selected square(s). Plotting squares only.")
    unique_fill <- character(0)
  }

  if (is.character(palette) && !is.null(names(palette))) {
    # A custom named palette was provided
    if (!all(unique_fill %in% names(palette))) {
      missing <- setdiff(unique_fill, names(palette))
      stop("The provided palette is missing colors for: ", paste(missing, collapse = ", "))
    }
    colors <- palette
  } else if (is.character(palette) && length(palette) == 1) {
    # A known palette name
    n <- length(unique_fill)
    colors <- switch(palette,
      "viridis" = viridis::viridis(n),
      "plasma"  = viridis::plasma(n),
      "rainbow" = rainbow(n),
      scales::hue_pal()(n) # fallback
    )
    names(colors) <- unique_fill
  } else {
    stop("Invalid palette. Use a named vector of colors or a predefined palette (e.g., 'viridis').")
  }

  #------------------------------------------------------------------------
  # 8) Build the main plot
  #------------------------------------------------------------------------
  # We will conditionally include the points layer if we have any points.
  # Also we check if the shape column is valid in points_in_grid.

  have_points <- nrow(points_in_grid) > 0
  shape_is_valid <- have_points && !is.null(shape) && shape %in% names(points_in_grid)

  # Start the plot with the base layer (map + grid squares)
  plot_main <- ggplot() +
    geom_sf(
      data = map_layer,
      fill = layer_fill_color,
      color = layer_border_color,
      linewidth = layer_border_width
    ) +
    geom_sf(
      data = grid_layer,
      fill = grid_fill_color,
      color = grid_border_color,
      linewidth = grid_border_width
    )

  # If there are any points within the squares, add them
  if (have_points) {
    if (shape_is_valid) {
      plot_main <- plot_main +
        geom_sf(
          data = points_in_grid,
          aes_string(color = fill, shape = shape),
          size = dot_size
        )
    } else {
      # shape column doesn't exist => color only
      plot_main <- plot_main +
        geom_sf(
          data = points_in_grid,
          aes_string(color = fill),
          size = dot_size
        )
    }
    # scale_color_manual for fill
    if (length(unique_fill) > 0) {
      plot_main <- plot_main + scale_color_manual(values = colors)
    }
  }

  # Add the rest of the labels, coords, theme
  plot_main <- plot_main +
    coord_sf(xlim = xlim, ylim = ylim) +
    labs(
      title = title,
      x = x_axis_label,
      y = y_axis_label,
      color = legend_fill_title,
      shape = legend_shape_title
    ) +
    theme_minimal()

  #------------------------------------------------------------------------
  # 9) Build the location overview (no points, just squares & map)
  #------------------------------------------------------------------------
  plot_location <- ggplot() +
    geom_sf(
      data = map_layer,
      fill = layer_fill_color,
      color = layer_border_color,
      linewidth = layer_border_width
    ) +
    geom_sf(
      data = grid_layer,
      fill = grid_fill_color,
      color = grid_border_color,
      linewidth = grid_border_width
    ) +
    coord_sf() +
    labs(title = "Selected Square(s) Location Overview") +
    theme_minimal()

  #------------------------------------------------------------------------
  # 10) Return both plots in a list
  #------------------------------------------------------------------------
  return(list(
    plot_with_points = plot_main,
    square_location  = plot_location
  ))
}


########################################
#' Count Observations per EEA Grid Square
#'
#' Counts the number of observations that fall within each EEA grid square (1×1 km or 10×10 km).
#'
#' @param fill Character. Column name to group and count rows by. Default is `"taxon.name"`.
#'   If you set \code{fill = "none"}, it won't group by any column and will count total
#'   observations per square.
#' @param grid_type Which grid layer to use: `"1km"` or `"10km"`. Default is `"1km"`.
#' @param square_id Optional vector of square IDs (e.g., \code{"10kmE459N247"}) to filter.
#'   If not provided, all squares in the chosen grid layer will be included.
#' @param df A data frame containing \strong{at least} \code{longitude}, \code{latitude}, and
#'   any column specified by \code{fill}.
#'
#' @return A \code{data.frame} with counts for each square (identified by \code{cellcode})
#'   and a \strong{TOTAL} row. Columns:
#'   \itemize{
#'     \item The grouping column specified by \code{fill} (or "ALL" if \code{fill = "none"}).
#'     \item \code{cellcode} (the grid square ID or "TOTAL").
#'     \item \code{count} (the number of observations).
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example data with 2 observations:
#' obs <- data.frame(
#'   taxon.name = c("Species A", "Species B"),
#'   longitude  = c(16.0, 15.5),
#'   latitude   = c(45.8, 45.9)
#' )
#'
#' # Count how many observations of each taxon fall into each 10km square
#' # that you specify:
#' grid_counts <- squares.count(
#'   fill      = "taxon.name",
#'   grid_type = "10km",
#'   square_id = c("10kmE459N247", "10kmE460N244"),
#'   df        = obs
#' )
#'
#' # If you want to count all squares in the 1km grid, do:
#' all_1km_counts <- squares.count(
#'   fill      = "taxon.name",
#'   grid_type = "1km",
#'   square_id = NULL, # all squares
#'   df        = obs
#' )
#'
#' # Or if you just want total counts per square (no grouping),
#' # use fill = "none":
#' all_10km_nogroup <- squares.count(
#'   fill      = "none",
#'   grid_type = "10km",
#'   df        = obs
#' )
#' }
squares.count <- function(fill = "taxon.name",
                          grid_type = c("1km", "10km"),
                          square_id = NULL,
                          df) {
  # Required packages
  library(sf)
  library(dplyr)

  grid_type <- match.arg(grid_type)

  # 1) Remove rows with missing lat/long in the observation data
  df <- df[!is.na(df$latitude) & !is.na(df$longitude), ]
  message(nrow(df), " rows remain after removing rows with missing latitude or longitude.")

  # 2) Load the requested EEA grid (1km or 10km) from your package
  if (grid_type == "1km") {
    grid_path <- system.file("extdata", "EEA_mreza", "RH_EEA_1×1km.shp", package = "biologerR")
    if (grid_path == "") {
      stop("1×1 km EEA shapefile not found in the biologerR package. Please check your installation.")
    }
    squares_sf <- sf::st_read(grid_path, quiet = TRUE)
  } else {
    grid_path <- system.file("extdata", "EEA_mreza", "RH_EEA_10×10km.shp", package = "biologerR")
    if (grid_path == "") {
      stop("10×10 km EEA shapefile not found in the biologerR package. Please check your installation.")
    }
    squares_sf <- sf::st_read(grid_path, quiet = TRUE)
    # Rename CellCode -> cellcode if needed
    if ("CellCode" %in% names(squares_sf) && !"cellcode" %in% names(squares_sf)) {
      squares_sf <- dplyr::rename(squares_sf, cellcode = "CellCode")
    }
  }

  # Make sure squares are in WGS84
  squares_sf <- sf::st_transform(squares_sf, crs = 4326)

  # 3) Optionally filter squares by square_id
  if (!is.null(square_id)) {
    sel_idx <- which(squares_sf$cellcode %in% square_id)
    if (length(sel_idx) == 0) {
      stop("None of the specified square_id values were found in the chosen grid.")
    }
    squares_sf <- squares_sf[sel_idx, ]
  }

  # 4) Convert observation data to sf
  df_sf <- sf::st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

  # 5) Spatial join: which square does each observation fall into?
  joined_data <- sf::st_join(df_sf, squares_sf, join = sf::st_within)

  # 6) Summarize the counts per square
  if (fill == "none") {
    # No grouping => just total counts per square
    counts <- joined_data %>%
      group_by(cellcode) %>%
      summarise(count = n(), .groups = "drop") %>%
      mutate(!!fill := "ALL") # add a filler column for uniform structure
  } else {
    # Group by the specified fill column + the square cellcode
    counts <- joined_data %>%
      group_by(!!sym(fill), cellcode) %>%
      summarise(count = n(), .groups = "drop")
  }

  # 7) Create TOTAL row(s)
  #  - One total row per fill group
  #  - One overall TOTAL row
  total_per_group <- counts %>%
    group_by(!!sym(fill)) %>%
    summarise(cellcode = "TOTAL", count = sum(count), .groups = "drop")

  total_overall <- counts %>%
    summarise(!!sym(fill) := "TOTAL", cellcode = "TOTAL", count = sum(count))

  # Combine them into one data frame
  final_counts <- bind_rows(counts, total_per_group, total_overall)

  # 8) Arrange for better readability
  final_counts <- final_counts %>%
    arrange(!!sym(fill), cellcode)

  # 9) Print and return
  print(final_counts)
  return(final_counts)
}
