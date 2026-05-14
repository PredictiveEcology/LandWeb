library(dplyr)
library(furrr)
library(ggplot2)
library(ggpattern)
library(map)

## polygon type for which to build comparative boxplots
poly_types <- c("", "ANSR", "Caribou")

# LTHFC maps ---------------------------------------------------------------------------------------

## fmt: skip
studyArea <- sf::st_read(file.path("outputs", "NW_AB_2025", "LTHFC_NW_AB.gpkg")) |>
  sf::st_union()

lthfc_options <- c("0", "A", "B", "C")

lthfc_df <- purrr::map(
  .x = lthfc_options,
  .f = function(x) {
    ## fmt: skip
    lthfc <- file.path("outputs", glue::glue("NW_AB_LTHFC_Option{x}_aspenDispersal_logROS"), "rep01", "landweb_lthfc_clean.shp") |>
      sf::st_read()
    ## fmt: skip
    lthfc <- sf::st_intersection(lthfc, sf::st_transform(studyArea, sf::st_crs(lthfc))) |>
      dplyr::rename(LTHFC = frRtrnI) |>
      dplyr::mutate(option = x, .before = "area")
  }
) |>
  do.call(rbind, args = _) |>
  dplyr::mutate(LTHFC = as.factor(LTHFC)) |>
  dplyr::group_by(option)

## build a linear palette across all possible values (multiples of 5)
lthfc_range <- range(as.integer(as.character(lthfc_df$LTHFC)), na.rm = TRUE)
lthfc_all_vals <- seq(lthfc_range[1], lthfc_range[2], by = 5)
lthfc_all_cols <- setNames(
  colorRampPalette(c("red", "yellow"))(length(lthfc_all_vals)),
  as.character(lthfc_all_vals)
)
lthfc_cols <- lthfc_all_cols[levels(lthfc_df$LTHFC)] ## subset to only values present in data

gg_lthfc <- ggplot2::ggplot(lthfc_df, ggplot2::aes(fill = LTHFC)) +
  ggplot2::geom_sf() +
  ggplot2::geom_sf_text(ggplot2::aes(label = LTHFC)) +
  ggplot2::scale_fill_manual(values = lthfc_cols) +
  ggplot2::facet_wrap(~option, ncol = 2) +
  ggplot2::xlab("longitude") +
  ggplot2::ylab("latitude")

f_gg_lthfc <- file.path("outputs", "NW_AB_2025", "maps", "LTHFC_NW_AB.png")
ggplot2::ggsave(f_gg_lthfc, gg_lthfc, height = 8, width = 8)

# boxplots -----------------------------------------------------------------------------------------

## inputs are from each of the NW AB Landweb runs:
## - high dispersal: Options A, B, and C, plus one using original LTHFC layer;
## - aspen dispersal: Options A, B, and C, plus one using original LTHFC layer;

## get vegTypeMap of initial conditions for calculating forested areas
vtm <- file.path(
  "outputs",
  "NW_AB_LTHFC_Option0_highDispersal_logROS",
  "rep01",
  "vegTypeMap_year0000.grd"
) |>
  terra::rast()

## reporting polygons from ml object to calculate *forested* area to add to boxplots
## fmt: skip
ml <- readRDS(file.path("outputs", "NW_AB_LTHFC_Option0_highDispersal_logROS", "ml_preamble.rds"))
pixel_area <- prod(terra::res(vtm)) ## in map units (m^2)

purrr::walk(.x = poly_types, .f = function(type) {
  ._type <- ifelse(nzchar(type), paste0("_", type), type)
  ._type_name <- paste0("NW_AB", ._type) ## underscores in name
  type_name <- gsub("_", " ", ._type_name) ## underscores replaced with spaces

  ## fmt: skip
  cli::cli_alert_info(paste0("Building comparative boxplots for ", ._type_name, " ..."))

  ## fmt: skip
  csv_files <- list(
    "0_HD" = file.path("outputs", "NW_AB_LTHFC_Option0_highDispersal_logROS", "boxplots", paste0("leading_boxplots_NW_AB", ._type, ".csv")),
    "A_HD"   = file.path("outputs", "NW_AB_2025", "OptionA", "Boxplots", paste0("leading_boxplots_nw_ab", ._type, ".csv")),
    "B_HD"   = file.path("outputs", "NW_AB_2025", "OptionB", "Boxplots", paste0("leading_boxplots_nw_ab", ._type, ".csv")),
    "C_HD"   = file.path("outputs", "NW_AB_2025", "OptionC", "boxplots", paste0("leading_boxplots_nw_ab", ._type, ".csv")),

    "0_AD" = file.path("outputs", "NW_AB_LTHFC_Option0_aspenDispersal_logROS", "boxplots", paste0("leading_boxplots_NW_AB", ._type, ".csv")),
    "A_AD"   = file.path("outputs", "NW_AB_LTHFC_OptionA_aspenDispersal_logROS", "boxplots", paste0("leading_boxplots_NW_AB", ._type, ".csv")),
    "B_AD"   = file.path("outputs", "NW_AB_LTHFC_OptionB_aspenDispersal_logROS", "boxplots", paste0("leading_boxplots_NW_AB", ._type, ".csv")),
    "C_AD"   = file.path("outputs", "NW_AB_LTHFC_OptionC_aspenDispersal_logROS", "boxplots", paste0("leading_boxplots_NW_AB", ._type, ".csv"))
  )

  stopifnot(all(file.exists(unlist(csv_files))))

  ## fmt: skip
  output_dir <- file.path("outputs", "NW_AB_2025", "comparative_boxplots", ._type_name) |>
    fs::dir_create()

  polys <- ml[[type_name]] |>
    sf::st_as_sf()

  polys <- polys |>
    dplyr::rename(zone = Name) |>
    dplyr::mutate(
      zone = gsub(" LandWeb Study Area", "", zone),
      zone = sub("^(.+)\\s+\\1$", "\\1", zone)
    ) |>
    terra::vect() |>
    terra::project(vtm)

  poly_areas <- terra::extract(vtm, polys, cells = FALSE, na.rm = TRUE) |>
    dplyr::group_by(ID) |>
    dplyr::summarise(n_pixels = dplyr::n()) |>
    dplyr::mutate(
      zone = polys$zone[ID],
      zone_area = units::set_units(n_pixels * pixel_area, "m^2") |>
        units::set_units("ha")
    ) |>
    dplyr::group_by(zone) |>
    dplyr::summarise(zone_area = sum(zone_area))

  ## LTHFC option labels (plot order)
  option_labels <- c("Original", "Longest", "Intermediate", "Shortest")
  option_order <- c("0", "A", "B", "C")
  option_label_map <- setNames(option_labels, option_order)

  ## Read and combine data
  all_data <- dplyr::bind_rows(
    lapply(names(csv_files), function(option) {
      f_csv <- csv_files[[option]]
      df <- data.table::fread(file = f_csv, data.table = FALSE)
      df <- df |>
        dplyr::mutate(
          ## Remove " LandWeb Study Area" from zone
          zone = gsub(" LandWeb Study Area", "", zone),
          ageClass = dplyr::case_when(
            ageClass == "Young" ~ "Young (0-39 years)",
            ageClass == "Immature" ~ "Immature (40-79 years)",
            ageClass == "Mature" ~ "Mature (80-119 years)",
            ageClass == "Old" ~ "Old (≥120 years)",
          ),
          lthfc_option = strsplit(option, "_")[[1]][1],
          dispersal_type = if_else(
            grepl("aspenDispersal", f_csv),
            "Aspen",
            "High"
          ),
          .before = "proportionCC"
        ) |>
        dplyr::mutate(
          ## fix zone name repeats (e.g., "Yates (YAT) Yates (YAT)" should be "Yates (YAT)")
          zone = sub("^(.+)\\s+\\1$", "\\1", zone)
        )

      return(df)
    })
  )

  all_data <- all_data |>
    dplyr::mutate(
      ageClass = factor(
        ageClass,
        levels = c(
          "Young (0-39 years)",
          "Immature (40-79 years)",
          "Mature (80-119 years)",
          "Old (≥120 years)"
        )
      ),
      dispersal_type = factor(dispersal_type, levels = c("Aspen", "High")),
      lthfc_option = factor(
        option_label_map[as.character(lthfc_option)],
        levels = option_labels
      )
    )

  ## Plotting Function
  plot_boxflip <- function(
    subdf,
    zone_arg,
    species_arg,
    output_dir,
    poly_areas,
    option_labels
  ) {
    plotdf <- subdf |>
      dplyr::group_by(lthfc_option, dispersal_type, ageClass) |>
      dplyr::slice(1) |>
      dplyr::ungroup() |>
      tidyr::complete(
        lthfc_option = factor(option_labels, levels = option_labels),
        fill = list(
          MIN = NA,
          q25_0 = NA,
          MED = NA,
          q75_0 = NA,
          MAX = NA,
          proportionCC = NA
        )
      )

    p <- ggplot2::ggplot(plotdf) +
      ggpattern::geom_boxplot_pattern(
        ggplot2::aes(
          x = lthfc_option,
          fill = lthfc_option,
          pattern = dispersal_type,
          ymin = MIN,
          lower = q25_0,
          middle = MED,
          upper = q75_0,
          ymax = MAX,
        ),
        stat = "identity",
        alpha = 0.7,
        color = "black",
        pattern_fill = "black",
        pattern_angle = 45,
        pattern_density = 0.1,
        pattern_spacing = 0.025,
        pattern_key_scale_factor = 0.6,
        position = ggplot2::position_dodge2(padding = 0.3),
        width = 0.5,
        na.rm = TRUE
      ) +
      ggplot2::facet_wrap(~ageClass, nrow = 2) +
      ggplot2::ylim(0, 1) +
      ggplot2::scale_fill_manual(
        values = c(
          Original = "steelblue",
          Longest = "forestgreen",
          Intermediate = "darkorange",
          Shortest = "firebrick"
        ),
        guide = "none"
      ) +
      ggpattern::scale_pattern_manual(
        values = c(Aspen = "none", High = "stripe")
      ) +
      ggplot2::coord_flip() +
      ggplot2::geom_point(
        ggplot2::aes(
          x = lthfc_option,
          y = proportionCC,
          colour = "Current Condition"
        ),
        size = 3,
        na.rm = TRUE
      ) +
      ggplot2::scale_x_discrete(drop = FALSE) +
      ggplot2::scale_colour_discrete(type = "darkred") +
      ggplot2::labs(
        title = paste(zone_arg, "-", species_arg),
        caption = paste0(
          "Total Area of ",
          zone_arg,
          ": ",
          dplyr::filter(poly_areas, zone == zone_arg) |>
            dplyr::pull(zone_area) |>
            as.numeric() |>
            format(digits = 7, big.mark = ","),
          " ha"
        ),
        ## NOTE: "LTFC" is used instead of "LTHFC" for historical reasons
        x = "LTFC Option",
        y = paste0("Proportion of ", species_arg, "-Leading Area"),
        colour = "",
        pattern = "Dispersal Type"
      ) +
      ggplot2::theme_bw(base_size = 16) +
      ggplot2::theme(
        axis.title.x = ggplot2::element_text(face = "bold", size = 16),
        axis.title.y = ggplot2::element_text(face = "bold", size = 16),
        axis.text.y = ggplot2::element_text(face = "bold", size = 14),
        legend.position = "bottom",
        panel.grid.minor = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(
          hjust = 0.5,
          face = "bold",
          size = 20
        )
      )

    cleaned_zone <- gsub("[^a-zA-Z0-9]", "", zone_arg)
    cleaned_species <- gsub("[^a-zA-Z0-9]", "", species_arg)
    fname <- paste0("boxplot_", cleaned_zone, "_", cleaned_species, ".png")
    ggplot2::ggsave(
      file.path(output_dir, fname),
      p,
      width = 12,
      height = 9,
      dpi = 300
    )
  }

  ## Pre-split data so each worker receives only its subset
  data_splits <- split(
    all_data,
    list(all_data$zone, all_data$vegCover),
    drop = TRUE
  )

  ## Generate all plots sequentially
  ## NOTE: parallel execution (furrr::future_walk) is not faster here for unknown reasons
  ## fmt: skip
  cli::cli_alert_info(paste0("  Generating ", length(data_splits), " boxplots ..."))
  purrr::walk(
    data_splits,
    function(subdf) {
      z <- as.character(subdf$zone[1])
      s <- as.character(subdf$vegCover[1])
      plot_boxflip(subdf, z, s, output_dir, poly_areas, option_labels)
    }
  )
})

rm(ml)

# patch size histograms ----------------------------------------------------------------------------

future::plan(
  future::multisession,
  workers = parallelly::availableCores(constraints = "connections", omit = 1)
)

## inputs are from each of the NW AB Landweb runs:
## - high dispersal: Options A, B, and C, plus one using original LTHFC layer;
## - aspen dispersal: Options A, B, and C, plus one using original LTHFC layer;

purrr::walk(.x = poly_types, .f = function(type) {
  ._type <- ifelse(nzchar(type), paste0("_", type), type)
  ._type_name <- paste0("NW_AB", ._type) ## underscores in name

  patch_sizes <- c(100L, 500L, 1000L, 5000L)

  ## fmt: skip
  cli::cli_alert_info(paste0("Building comparative histograms for ", ._type_name, " ..."))

  ## fmt: skip
  output_dir <- file.path("outputs", "NW_AB_2025", "comparative_histograms", ._type_name) |>
    fs::dir_create()

  ## LTHFC option labels (plot order)
  option_labels <- c("Original", "Longest", "Intermediate", "Shortest")
  option_order <- c("0", "A", "B", "C")
  option_label_map <- setNames(option_labels, option_order)

  ## fmt: skip
  option_names <- c("0_HD", "A_HD", "B_HD", "C_HD", "0_AD", "A_AD", "B_AD", "C_AD")

  ## Read all CSVs for all patch sizes in one pass
  all_data <- dplyr::bind_rows(
    lapply(patch_sizes, function(psize) {
      ._psize <- paste0("_", psize)

      ## fmt: skip
      csv_files <- list(
        "0_HD" = file.path("outputs", "NW_AB_LTHFC_Option0_highDispersal_logROS", "histograms", paste0("largePatches_NW_AB", ._type, ._psize, ".csv")),
        "A_HD"   = file.path("outputs", "NW_AB_2025", "OptionA", "Histograms", paste0("largePatches_nw_ab", ._type, ._psize, ".csv")),
        "B_HD"   = file.path("outputs", "NW_AB_2025", "OptionB", "Histograms", paste0("largePatches_nw_ab", ._type, ._psize, ".csv")),
        "C_HD"   = file.path("outputs", "NW_AB_2025", "OptionC", "histograms", paste0("largePatches_nw_ab", ._type, ._psize, ".csv")),

        "0_AD" = file.path("outputs", "NW_AB_LTHFC_Option0_aspenDispersal_logROS", "histograms", paste0("largePatches_NW_AB", ._type, ._psize, ".csv")),
        "A_AD"   = file.path("outputs", "NW_AB_LTHFC_OptionA_aspenDispersal_logROS", "histograms", paste0("largePatches_NW_AB", ._type, ._psize, ".csv")),
        "B_AD"   = file.path("outputs", "NW_AB_LTHFC_OptionB_aspenDispersal_logROS", "histograms", paste0("largePatches_NW_AB", ._type, ._psize, ".csv")),
        "C_AD"   = file.path("outputs", "NW_AB_LTHFC_OptionC_aspenDispersal_logROS", "histograms", paste0("largePatches_NW_AB", ._type, ._psize, ".csv"))
      )

      stopifnot(all(file.exists(unlist(csv_files))))

      dplyr::bind_rows(
        lapply(option_names, function(option) {
          f_csv <- csv_files[[option]]
          df <- data.table::fread(file = f_csv, data.table = FALSE)
          df <- df |>
            dplyr::mutate(
              patch_size = psize,
              ## Remove " LandWeb Study Area" from polygonName
              polygonName = gsub(" LandWeb Study Area", "", polygonName),
              ageClass = dplyr::case_when(
                ageClass == "Young" ~ "Young (0-39 years)",
                ageClass == "Immature" ~ "Immature (40-79 years)",
                ageClass == "Mature" ~ "Mature (80-119 years)",
                ageClass == "Old" ~ "Old (≥120 years)",
              ),
              lthfc_option = strsplit(option, "_")[[1]][1],
              dispersal_type = if_else(
                grepl("aspenDispersal", f_csv),
                "Aspen",
                "High"
              ),
              .before = "N"
            ) |>
            dplyr::mutate(
              ## drop row numbers
              X = NULL,
              ## fix zone name repeats (e.g., "Yates (YAT) Yates (YAT)" should be "Yates (YAT)")
              polygonName = sub("^(.+)\\s+\\1$", "\\1", polygonName)
            )

          return(df)
        })
      )
    })
  )

  all_data <- all_data |>
    dplyr::mutate(
      ageClass = factor(
        ageClass,
        levels = c(
          "Young (0-39 years)",
          "Immature (40-79 years)",
          "Mature (80-119 years)",
          "Old (≥120 years)"
        )
      ),
      dispersal_type = factor(dispersal_type, levels = c("Aspen", "High")),
      lthfc_option = factor(
        option_label_map[as.character(lthfc_option)],
        levels = option_labels
      )
    )

  ## Plotting Function
  plot_hists <- function(subdf, zone_arg, species_arg, patch_size, output_dir) {
    p <- ggplot2::ggplot(
      subdf,
      ggplot2::aes(x = N, fill = lthfc_option, pattern = dispersal_type)
    ) +
      ggpattern::geom_histogram_pattern(
        position = "identity",
        alpha = 0.7,
        color = "black",
        pattern_fill = "black",
        pattern_angle = 45,
        pattern_density = 0.1,
        pattern_spacing = 0.025,
        pattern_key_scale_factor = 0.6,
        na.rm = TRUE
      ) +
      ggpattern::scale_pattern_manual(
        values = c(Aspen = "none", High = "stripe"),
        guide = "none"
      ) +
      ggplot2::facet_grid(
        dispersal_type ~ ageClass,
        labeller = ggplot2::labeller(
          dispersal_type = c(
            Aspen = "Aspen Dispersal",
            High = "High Dispersal"
          )
        )
      ) +
      ggplot2::scale_fill_manual(
        values = c(
          Original = "steelblue",
          Longest = "forestgreen",
          Intermediate = "darkorange",
          Shortest = "firebrick"
        )
      ) +
      ggplot2::geom_vline(
        ggplot2::aes(xintercept = NCC, colour = "Current Condition"),
        na.rm = TRUE
      ) +
      ggplot2::scale_colour_manual(
        values = c("Current Condition" = "darkred")
      ) +
      ggplot2::labs(
        title = paste0(zone_arg, " - ", species_arg),
        x = paste0("Number of patches greater than ", patch_size, " ha"),
        y = "Count",
        colour = "",
        ## NOTE: "LTFC" is used instead of "LTHFC" for historical reasons
        fill = "LTFC Option"
      ) +
      ggplot2::theme_bw(base_size = 16) +
      ggplot2::theme(
        axis.title.x = ggplot2::element_text(face = "bold", size = 16),
        axis.title.y = ggplot2::element_text(face = "bold", size = 16),
        axis.text.y = ggplot2::element_text(face = "bold", size = 14),
        legend.position = "bottom",
        panel.grid.minor = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(
          hjust = 0.5,
          face = "bold",
          size = 20
        )
      )

    cleaned_zone <- gsub("[^a-zA-Z0-9]", "", zone_arg)
    cleaned_species <- gsub("[^a-zA-Z0-9]", "", species_arg)
    ## fmt: skip
    fname <- paste0("histogram_", cleaned_zone, "_", cleaned_species, "_", patch_size, ".png")
    ggplot2::ggsave(
      file.path(output_dir, fname),
      p,
      width = 12,
      height = 9,
      dpi = 300
    )
  }

  ## Pre-split data so each worker receives only its subset
  data_splits <- split(
    all_data,
    list(all_data$patch_size, all_data$polygonName, all_data$vegCover),
    drop = TRUE
  )

  ## Generate all plots in a single parallel dispatch across all patch sizes
  ## fmt: skip
  cli::cli_alert_info(paste0("  Generating ", length(data_splits), " histograms ..."))
  furrr::future_walk(
    data_splits,
    function(subdf) {
      psize <- subdf$patch_size[1]
      z <- as.character(subdf$polygonName[1])
      s <- as.character(subdf$vegCover[1])
      plot_hists(subdf, z, s, psize, output_dir)
    },
    .options = furrr::furrr_options(
      seed = NULL,
      globals = c("plot_hists", "output_dir")
    )
  )
})

future::plan(future::sequential)

## upload to Google Drive -----------------------------------------------------

## fmt: skip
googledrive::drive_auth(path = fs::dir_ls(".", type = "file", regexp = "landweb.*[.]json$"))

googledrive::drive_put(
  f_gg_lthfc,
  path = googledrive::as_id("1icwggSLDnOqbAVuJCZiyIW5GrhZ8nXF0") ## maps
)

purrr::walk(.x = poly_types, .f = function(type) {
  ._type <- ifelse(nzchar(type), paste0("_", type), type)
  ._type_name <- paste0("NW_AB", ._type) ## underscores in name

  ## fmt: skip
  output_dir_boxplots <- file.path("outputs", "NW_AB_2025", "comparative_boxplots", ._type_name)
  stopifnot(dir.exists(output_dir_boxplots))

  ## upload boxplots
  purrr::walk(
    .x = fs::dir_ls(output_dir_boxplots, type = "file"),
    .f = googledrive::drive_put,
    path = googledrive::as_id("1KQTvV0fT4bUZaiGYAtEwByyW8p2TlbOn") |>
      googledrive::drive_ls() |>
      dplyr::filter(name == ._type_name) |>
      dplyr::pull(id)
  )

  ## fmt: skip
  output_dir_histograms <- file.path("outputs", "NW_AB_2025", "comparative_histograms", ._type_name)
  stopifnot(dir.exists(output_dir_histograms))

  ## upload histograms
  purrr::walk(
    .x = fs::dir_ls(output_dir_histograms, type = "file"),
    .f = googledrive::drive_put,
    path = googledrive::as_id("1-dUcUzB4P-2-WiJN7TSFZd6xjntnbUgL") |>
      googledrive::drive_ls() |>
      dplyr::filter(name == ._type_name) |>
      dplyr::pull(id)
  )
})
