library(dplyr)
library(ggplot2)

# LTHFC maps ---------------------------------------------------------------------------------------

studyArea <- sf::st_read(file.path("outputs", "NW_AB_2025", "LTHFC_NW_AB.gpkg")) |>
  sf::st_union()

lthfc_options <- c("0", "A", "B", "C")

lthfc_df <- purrr::map(
  .x = lthfc_options,
  .f = function(x) {
    lthfc <- file.path("outputs", glue::glue("NW_AB_LTHFC_Option{x}_aspenDispersal_logROS"), "rep01", "landweb_lthfc_clean.shp") |>
      sf::st_read()
    lthfc <- sf::st_intersection(lthfc, sf::st_transform(studyArea, sf::st_crs(lthfc))) |>
      dplyr::rename(LTHFC = frRtrnI) |>
      dplyr::mutate(option = x, .before = "area")
  }
) |> do.call(rbind, args = _) |>
  dplyr::mutate(LTHFC = as.factor(LTHFC)) |>
  dplyr::group_by(option)

gg_lthfc <- ggplot2::ggplot(lthfc_df, ggplot2::aes(fill = LTHFC)) +
  ggplot2::geom_sf() +
  ggplot2::geom_sf_text(aes(label = LTHFC)) +
  ggplot2::facet_wrap(~option, ncol = 2) +
  ggplot2::xlab("longitude") +
  ggplot2::ylab("latitude")

f_gg_lthfc <- file.path("outputs", "NW_AB_2025", "LTHFC_NW_AB.png")
ggplot2::ggsave(f_gg_lthfc, gg_lthfc, height = 8, width = 8)

# boxplots -----------------------------------------------------------------------------------------

## inputs are from each of the NW AB Landweb runs:
## - high dispersal: Options A, B, and C, plus one using original LTHFC layer;
## - aspen dispersal: Options A, B, and C, plus one using original LTHFC layer;

csv_files <- list(
  "0_HD" = file.path("outputs", "NW_AB_LTHFC_Option0_highDispersal_logROS", "boxplots", "leading_boxplots_NW_AB_ANSR.csv"),
  "A_HD"   = file.path("outputs", "NW_AB_2025", "OptionA", "Boxplots", "leading_boxplots_nw_ab_ANSR.csv"),
  "B_HD"   = file.path("outputs", "NW_AB_2025", "OptionB", "Boxplots", "leading_boxplots_nw_ab_ANSR.csv"),
  "C_HD"   = file.path("outputs", "NW_AB_2025", "OptionC", "boxplots", "leading_boxplots_nw_ab_ANSR.csv"),

  "0_AD" = file.path("outputs", "NW_AB_LTHFC_Option0_aspenDispersal_logROS", "boxplots", "leading_boxplots_NW_AB_ANSR.csv"),
  "A_AD"   = file.path("outputs", "NW_AB_LTHFC_OptionA_aspenDispersal_logROS", "boxplots", "leading_boxplots_NW_AB_ANSR.csv"),
  "B_AD"   = file.path("outputs", "NW_AB_LTHFC_OptionB_aspenDispersal_logROS", "boxplots", "leading_boxplots_NW_AB_ANSR.csv"),
  "C_AD"   = file.path("outputs", "NW_AB_LTHFC_OptionC_aspenDispersal_logROS", "boxplots", "leading_boxplots_NW_AB_ANSR.csv")
)

stopifnot(all(file.exists(unlist(csv_files))))

output_dir <- file.path("outputs", "NW_AB_2025", "comparative_boxplots") |> fs::dir_create()

## LTHFC option labels (plot order)
option_labels <- c("Original", "Longest", "Intermediate", "Shortest")
option_order <- c("0", "A", "B", "C")
option_label_map <- setNames(option_labels, option_order)

## Read and combine data
all_data <- bind_rows(
  lapply(names(csv_files), function(option) {
    f_name <- csv_files[[option]]
    df <- read.csv(file = f_name)
    ## Remove " LandWeb Study Area" from zone
    df <- df |>
      dplyr::mutate(
        zone = gsub(" LandWeb Study Area", "", zone),
        lthfc_option = strsplit(option, "_")[[1]][1],
        dispersal_type = if_else(grepl("aspenDispersal", f_name), "aspen", "high"),
        .before = "proportionCC"
      )

    return(df)
  })
)

all_data <- all_data |>
  dplyr::mutate(
    ageClass = factor(ageClass, levels = c("Young", "Immature", "Mature", "Old")),
    dispersal_type = factor(dispersal_type, levels = c("aspen", "high")),
    lthfc_option = factor(option_label_map[as.character(lthfc_option)], levels = option_labels)
  )

## Plotting Function
plot_boxflip <- function(subdf, zone_arg, species_arg, output_dir) {
  plotdf <- subdf |>
    dplyr::filter(
      as.character(zone)     == as.character(zone_arg),
      as.character(vegCover) == as.character(species_arg)
    ) |>
    dplyr::group_by(lthfc_option, dispersal_type, ageClass) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    tidyr::complete(
      lthfc_option = factor(option_labels, levels = option_labels),
      fill = list(MIN = NA, q25_0 = NA, MED = NA, q75_0 = NA, MAX = NA, proportionCC = NA)
    )

  p <- ggplot(plotdf) +
    geom_boxplot(
      aes(
        x = lthfc_option,
        fill = dispersal_type,
        ymin = MIN, lower = q25_0, middle = MED, upper = q75_0, ymax = MAX,
      ),
      stat = "identity",
      color = "black",
      alpha = 0.7,
      width = 0.5,
      na.rm = TRUE
    ) +
    coord_flip() +
    geom_point(
      aes(x = lthfc_option, y = proportionCC),
      color = "red",
      size = 3,
      na.rm = TRUE,
      show.legend = FALSE
    ) +
    facet_wrap(~ageClass, nrow = 2) +
    scale_x_discrete(drop = FALSE) +
    labs(
      title = paste(zone_arg, "-", species_arg),
      x = "LTHFC option",
      y = "Proportion of forest area",
      fill = "Dispersal type"
    ) +
    theme_bw(base_size = 16) +
    theme(
      axis.title.x = element_text(face = "bold", size = 16),
      axis.title.y = element_text(face = "bold", size = 16),
      axis.text.y = element_text(face = "bold", size = 14),
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 20)
    )

  cleaned_zone <- gsub("[^a-zA-Z0-9]", "", zone_arg)
  cleaned_species  <- gsub("[^a-zA-Z0-9]", "", species_arg)
  fname <- paste0("boxplot_", cleaned_zone, "_", cleaned_species, ".png")
  ggsave(file.path(output_dir, fname), p, width = 10, height = 6, dpi = 300)
}

## Generate all plots
zones <- unique(as.character(all_data$zone))
species <- unique(as.character(all_data$vegCover))
ages  <- unique(as.character(all_data$ageClass))

for(z in zones) {
  for(s in species) {
    plot_boxflip(all_data, z, s, output_dir)
  }
}
message("All comparative boxplots saved to: ", output_dir)
