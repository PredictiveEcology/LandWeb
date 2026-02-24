## plotting function to mass produce boxplots by NSR
## inputs are from each of the NW AB Landweb runs:
## - high dispersal: Options A, B, and C, plus one using original LTHFG layer;
## - aspen dispersal: Options A, B, and C plus the original 2020 runs;

library(data.table)
library(dplyr)
library(ggplot2)

csv_files <- list(
  Old_HD = file.path("outputs", "NW_AB_LTHFC_Option0_highDispersal_logROS", "boxplots", "leading_NW_AB_ANSR.csv"),
  A_HD   = file.path("outputs", "NW_AB_2025", "OptionA", "Boxplots", "leading_nw_ab_ANSR.csv"),
  B_HD   = file.path("outputs", "NW_AB_2025", "OptionB", "Boxplots", "leading_nw_ab_ANSR.csv"),
  C_HD   = file.path("outputs", "NW_AB_2025", "OptionC", "Boxplots", "leading_nw_ab_ANSR.csv"),

  Old_AD = file.path("outputs", "NW_AB_LTHFC_Option0_aspenDispersal_logROS", "boxplots", "leading_NW_AB_ANSR.csv"),
  A_AD   = file.path("outputs", "NW_AB_LTHFC_OptionA_aspenDispersal_logROS", "boxplots", "leading_NW_AB_ANSR.csv"),
  B_AD   = file.path("outputs", "NW_AB_LTHFC_OptionB_aspenDispersal_logROS", "boxplots", "leading_NW_AB_ANSR.csv"),
  C_AD   = file.path("outputs", "NW_AB_LTHFC_OptionC_aspenDispersal_logROS", "boxplots", "leading_NW_AB_ANSR.csv")
)

stopifnot(all(file.exists(unlist(csv_files)))) ## TODO: resume (HERE)

output_dir <- file.path("outputs", "NW_AB_2025", "comparative_boxplots") |> fs::dir_create()

## Option labels (plot order)
option_labels <- c(
  "Longest LTHFC (HD)", "Longest LTHFC (AD)",
  "Intermediate LTHFC (HD)", "Intermediate LTHFC (AD)",
  "Shortest LTHFC (HD)", "Shortest LTHFC (AD)",
  "Original LTHFC (HD)", "Original LTHFC (AD)"
)
option_order <- c("A_HD", "A_AD", "B_HD", "B_AD", "C_HD", "C_AD", "Old_HD", "Old_AD")
option_label_map <- setNames(option_labels, option_order)

## Read and combine data
all_data <- bind_rows(
  lapply(names(files), function(option) {
    df <- fread(file = files[[option]])
    ## Remove " LandWeb Study Area" from zone
    df$zone <- gsub(" LandWeb Study Area", "", df$zone)
    df$Option <- option
    df
  })
)

all_data <- all_data %>% filter(vegCover == "All species")

keep_zones <- all_data %>%
  filter(Option %in% c("A", "B", "C")) %>%
  pull(zone) %>%
  unique()

all_data <- all_data %>% filter(zone %in% keep_zones)

all_data$Option <- factor(all_data$Option, levels = option_order)
all_data$PlotOption <- option_label_map[as.character(all_data$Option)]
all_data$PlotOption <- factor(all_data$PlotOption, levels = option_labels)
all_data$ageClass <- factor(all_data$ageClass, levels = c("Young", "Immature", "Mature", "Old"))

## Plotting Function
plot_boxflip <- function(subdf, zone_arg, ageClass_arg, output_dir) {
  plotdf <- subdf %>%
    filter(
      as.character(zone)     == as.character(zone_arg),
      as.character(ageClass) == as.character(ageClass_arg)
    ) %>%
    group_by(PlotOption) %>%
    slice(1) %>%
    ungroup() %>%
    complete(PlotOption = factor(option_labels, levels = option_labels), fill = list(MIN = NA, q25_0 = NA, MED = NA, q75_0 = NA, MAX = NA, proportionCC = NA)) # ensure all labels appear

  ## Color: light green/grey for HD, dark green/grey for AD;
  box_colors <- c(
    "limegreen", "darkgreen", ## option A
    "limegreen", "darkgreen", ## option B
    "limegreen", "darkgreen", ## option C
    "gray",      "gray30"     ## original lthfc
  )
  names(box_colors) <- option_labels

  p <- ggplot(plotdf,
              aes(
                x = PlotOption,
                ymin = MIN, lower = q25_0, middle = MED, upper = q75_0, ymax = MAX
              )) +
    geom_boxplot(
      stat = "identity",
      aes(fill = PlotOption),
      color = "black",
      alpha = 0.7,
      width = 0.5,
      na.rm = TRUE
    ) +
    geom_point(aes(y = proportionCC), color = "red", size = 3, na.rm = TRUE) +
    scale_fill_manual(values = box_colors, guide = "none") +
    coord_flip() +
    scale_x_discrete(drop = FALSE) +
    labs(
      title = paste(zone_arg, "-", ageClass_arg),
      x = NULL,
      y = "Proportion of forest area"
    ) +
    theme_classic(base_size = 16) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
      axis.title.x = element_text(face = "bold", size = 16),
      axis.title.y = element_text(face = "bold", size = 16),
      axis.text.y = element_text(face = "bold", size = 14)
    )
  cleaned_zone <- gsub("[^a-zA-Z0-9]", "", zone_arg)
  cleaned_age  <- gsub("[^a-zA-Z0-9]", "", ageClass_arg)
  fname <- paste0("boxplot_", cleaned_zone, "_", cleaned_age, ".png")
  ggsave(file.path(output_dir, fname), p, width = 10, height = 6, dpi = 300)
}

## Generate all plots
zones <- unique(as.character(all_data$zone))
ages  <- unique(as.character(all_data$ageClass))
for(z in zones) {
  for(a in ages) {
    plot_boxflip(all_data, z, a, output_dir)
  }
}
message("All comparative boxplots saved to: ", output_dir)
