## WARNING: requires several packages that are NOT part of LandWeb v2
# renv::deactivate() ## ensure you reactivate it when done!
# install.packages(c("DiagrammeR", "DiagrammeRsvg", "rsvg"))

## adjust output figs_dir as needed
figs_dir <- file.path("outputs", "_figures") |>
  fs::dir_create()

DiagrammeR::grViz(
  "digraph model_workflow {

  graph [rankdir = TB, fontname = 'Helvetica', bgcolor = white, nodesep = 0.8, ranksep = 0.7, compound = true]
  node [fontname = 'Helvetica', fontsize = 11, style = filled, color = grey40, penwidth = 1.2]
  edge [color = grey40, arrowsize = 0.8, penwidth = 1.2]

  # --- Row 1: Input databases ---
  node [shape = cylinder, fillcolor = lightblue, fontcolor = grey10, width = 1.4, height = 0.8]
  D1 [label = 'forest\\ncover']
  D2 [label = 'vegetation\\n parameters']
  D3 [label = 'fire\\nhistory']
  D4 [label = 'study area &\\nreporting polygons']
  {rank = same; D1; D2; D3; D4}

  # --- Output nodes ---
  node [shape = tab, fillcolor = burlywood, fontcolor = grey10, width = 1.4, height = 0.8]
  D5 [label = 'Simulation Outputs']
  D6 [label = 'NRV Analyses Outputs']

  # --- Row 2: C1 Data Preparation ---
  subgraph cluster_C1 {
    label = 'Data Preparation'
    fontname = 'Helvetica-Bold'
    fontsize = 13
    style = 'filled,rounded'
    fillcolor = lemonchiffon
    color = goldenrod
    penwidth = 1.5

    node [shape = box, style = 'filled,rounded', fillcolor = lightsalmon, fontcolor = grey10, width = 1.8]
    M1 [label = 'LandWeb_preamble']
    M2 [label = 'Biomass_speciesData']
    M3 [label = 'Biomass_borealDataPrep']

    {rank = same; M1; M2; M3}
  }

  # --- Row 3: C2 Main Simulation ---
  subgraph cluster_C2 {
    label = 'Main Simulation'
    fontname = 'Helvetica-Bold'
    fontsize = 13
    style = 'filled,rounded'
    fillcolor = honeydew
    color = mediumseagreen
    penwidth = 1.5

    node [shape = box, style = 'filled,rounded', fillcolor = darkseagreen2, fontcolor = grey10, width = 1.8]
    M4 [label = 'Biomass_regeneration']
    M5 [label = 'Biomass_core']
    M6 [label = 'timeSinceFire']
    M7 [label = 'LandMine']
    M8 [label = 'LandWeb_output']

    # 2-col x 3-row grid
    {rank = same; M4; M5}
    {rank = same; M6; M7}
    {rank = same; M8}
    # Vertical alignment between rows
    M4 -> M6 [style = invis]
    M5 -> M7 [style = invis]
    M6 -> M8 [style = invis]
  }

  # --- Row 5: C3 NRV Analyses ---
  subgraph cluster_C3 {
    label = 'NRV Analyses'
    fontname = 'Helvetica-Bold'
    fontsize = 13
    style = 'filled,rounded'
    fillcolor = lavender
    color = mediumpurple
    penwidth = 1.5

    node [shape = box, style = 'filled,rounded', fillcolor = plum, fontcolor = grey10, width = 1.8]
    M9 [label = 'burnSummaries']
    M10 [label = 'LandWeb_summary']

    {rank = same; M9; M10}
  }

  # --- Spacer between C1 and C2 ---
  spacer1 [shape = point, width = 0, height = 0, label = '']

  # --- Edges ---
  D1 -> M1 [lhead = cluster_C1]
  D2 -> M2 [lhead = cluster_C1]
  D3 -> M3 [lhead = cluster_C1]
  D4 -> M3 [lhead = cluster_C1]
  D4 -> M9 [lhead = cluster_C3]
  M1 -> M2 -> M3
  M2 -> spacer1 [ltail = cluster_C1, arrowhead = none]
  spacer1 -> M4 [lhead = cluster_C2]
  M3 -> M9 [ltail = cluster_C1, lhead = cluster_C3]
  M8 -> D5 [ltail = cluster_C2]
  D5 -> M10 [lhead = cluster_C3]
  M10 -> D6 [ltail = cluster_C3]
}
"
) |>
  DiagrammeRsvg::export_svg() |>
  charToRaw() |>
  rsvg::rsvg_png(file.path(figs_dir, "modelling_workflow.png"))

# renv::activate()
