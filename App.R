# =====================================================================================
# 1. LOAD PACKAGES
# =====================================================================================
#options(repos = c(
#  INLA = "https://inla.r-inla-download.org/R/stable",
#  CRAN = "https://cran.rstudio.com/"
#))


library(shiny)
library(bslib)
library(shinycssloaders)
library(DT)

library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(INLA)
library(Matrix)
library(leaflet)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

library(mgcv)
library(scales)
library(raster)
library(jsonlite)

#rsconnect::writeManifest()

# =====================================================================================
# 2. CUSTOM CSS
# =====================================================================================

custom_css <- "

body {
  font-family: 'Segoe UI', sans-serif;
  background-color: #f5f7fa;
}

/* Navigation bar */

.navbar {
  background: linear-gradient(
    90deg,
    #123B5D 0%,
    #176B87 50%,
    #1F8A70 100%
  ) !important;
}

.navbar-brand {
  font-weight: 700;
  font-size: 21px;
}

/* Main hero */

.hero-banner {
  background: linear-gradient(
    120deg,
    #123B5D 0%,
    #176B87 50%,
    #1F8A70 100%
  );

  color: white;
  border-radius: 18px;
  padding: 35px;
  margin-bottom: 25px;

  box-shadow: 0 5px 18px rgba(0,0,0,0.12);
}

.hero-banner h1 {
  font-weight: 700;
  margin-bottom: 10px;
}

/* Cards */

.info-card {
  background: white;
  border-radius: 15px;
  padding: 22px;
  height: 100%;

  box-shadow: 0 4px 15px rgba(0,0,0,0.07);

  border-top: 5px solid #176B87;
}

.info-card-green {
  border-top-color: #1F8A70;
}

.info-card-orange {
  border-top-color: #DD6B20;
}

.info-card-purple {
  border-top-color: #805AD5;
}

/* General cards */

.card {
  border-radius: 15px !important;
  border: none !important;

  box-shadow:
    0 4px 15px rgba(0,0,0,0.07);
}

/* Section headings */

.section-head {
  color: #123B5D;
  font-weight: 700;

  margin-top: 10px;
  margin-bottom: 6px;
}

.sub-desc {
  color: #5A6772;
  font-size: 0.95rem;

  margin-bottom: 20px;
}

/* Buttons */

.btn-primary {
  background-color: #176B87 !important;
  border-color: #176B87 !important;
}

.btn-success {
  background-color: #1F8A70 !important;
  border-color: #1F8A70 !important;
}

/* Plot containers */

.plot-card {
  background: white;
  border-radius: 15px;
  padding: 15px;

  box-shadow:
    0 4px 15px rgba(0,0,0,0.06);

  margin-bottom: 20px;
}

/* Value boxes */

.value-box {
  background: white;
  border-radius: 15px;
  padding: 20px;

  text-align: center;

  box-shadow:
    0 4px 15px rgba(0,0,0,0.06);
}

.value-number {
  font-size: 28px;
  font-weight: 700;
  color: #176B87;
}

.value-label {
  color: #687580;
  font-size: 14px;
}

/* Sidebar */

.bslib-sidebar-layout > .sidebar {
  background-color: #ffffff;
}

/* Tables */

.dataTables_wrapper {
  font-size: 13px;
}
"


# =====================================================================================
# 3. APPLICATION THEME
# =====================================================================================

app_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#176B87",
  secondary = "#1F8A70"
)


# =====================================================================================
# 4. UI
# =====================================================================================

ui <- page_navbar(
  
  title = "GeoRisk Engine 🌧",
  
  id = "main_nav",
  
  theme = app_theme,
  
  header = tags$head(
    tags$style(
      HTML(custom_css)
    )
  ),
  
  fillable = FALSE,
  
  
  # ===================================================================================
  # TAB 1: OVERVIEW
  # ===================================================================================
  
  nav_panel(
    
    "Overview",
    
    div(
      class = "hero-banner",
      
      h1(
        "Spatial Modelling of Precipitation-Related Insurance Claims"
      ),
      
      h2(
        "Divan van der Heide."
      ),
      
      p(
        "Student Number: 23529815.",
      ),
      
      p(
        "Supervisor: Dr. Ansie Smit.",
      ),
      
      p(
        "Co-Supervisors: Prof. Janet van Niekerk, Prof. Inger Fabris-Rotelli.",
      )
    ),
    
    layout_column_wrap(
      
      width = 1/2,
      
      
      div(
        class = "info-card",
        
        h4("Research Overview"),
        
        p(
          "Due to climate change, there has been an increase in claims for insurance companies."
        ),
        
        p(
          "Accurate modelling of losses is very important for
              insurance companies, thus there is a need to consider
              models that handle large datasets effectively."
        )
      ),
      
      
      div(
        class = "info-card info-card-green",
        
        h4("Research Objectives"),
        
        p(
          "Modelling of Insurance claims."
        ),
        
        p(
          "Model the claim amounts associated with individual claims."
        ),
        
        p(
          "Predicting the Number and Amount of Insurance claims with INLA.",
        )
      )
      
    ),
    
    layout_column_wrap(
      
      width = 1/3,
      
      
      div(
        class = "info-card",
        
        h4("📍 Claim Frequency"),
        
        p(
          tags$b("Where are insurance claims more likely to occur?")
        ),
        
        p(
          "The spatial point process model estimates spatial variation ",
          "in claim intensity across South Africa."
        )
      ),
      
      
      div(
        class = "info-card info-card-green",
        
        h4("💰 Claim Severity"),
        
        p(
          tags$b("Where are claims expected to be more expensive?")
        ),
        
        p(
          "The Gamma marks model estimates the expected claim amount ",
          "at different spatial locations."
        )
      ),
      
      
      div(
        class = "info-card info-card-orange",
        
        h4("⚠️ Overall Risk"),
        
        p(
          tags$b("Where is insurance risk highest?")
        ),
        
        p(
          "Overall expected risk combines estimated claim frequency ",
          "and expected claim severity."
        )
      )
      
    ),
    
    br(),
    
    h3(
      "Research Framework",
      class = "section-head"
    ),
    
    p(
      "The analysis separates insurance risk into two components: ",
      "the frequency of precipitation-related claims and the severity ",
      "of those claims. These components are then combined to produce ",
      "an overall spatial risk surface.",
      class = "sub-desc"
    ),
    
    layout_column_wrap(
      
      width = 1/4,
      
      div(
        class = "value-box",
        div(class = "value-number", "1"),
        div(class = "value-label", "Prepare Claim Data")
      ),
      
      div(
        class = "value-box",
        div(class = "value-number", "2"),
        div(class = "value-label", "Model Claim Frequency")
      ),
      
      div(
        class = "value-box",
        div(class = "value-number", "3"),
        div(class = "value-label", "Model Claim Severity")
      ),
      
      div(
        class = "value-box",
        div(class = "value-number", "4"),
        div(class = "value-label", "Estimate Overall Risk")
      )
      
    ),
    
    br(),
    
    div(
      class = "plot-card",
      
      h4("Study Region"),
      
      withSpinner(
        plotOutput(
          "overview_map",
          height = "500px"
        )
      )
    )
    
  ),
  
  
  # ===================================================================================
  # TAB 2: BACKGROUND
  # ===================================================================================
  
  nav_panel(
    
    "Background",
    
    div(
      
      h3(
        "Background and Methodology",
        class = "section-head"
      ),
      
      p(
        "The modelling framework used to analyse precipitation-related
       insurance claims.",
        class = "sub-desc"
      )
      
    ),
    
    div(
      class = "plot-card",
      
      uiOutput("background_page")
      
    ),
    
    div(
      
      style = "text-align:center; margin-top:20px; margin-bottom:20px;",
      
      actionButton(
        "background_prev",
        "← Previous",
        class = "btn-primary"
      ),
      
      actionButton(
        "background_next",
        "Next →",
        class = "btn-primary"
      )
      
    )
    
  ),
  
  # ===================================================================================
  # TAB 3: DATA
  # ===================================================================================
  
  nav_panel(
    
    "Data",
    
    layout_sidebar(
      
      sidebar = sidebar(
        
        h4("Data Selection"),
        
        fileInput(
          "file",
          "Upload Excel Workbook",
          accept = c(".xlsx", ".xls")
        ),
        
        uiOutput(
          "sheet_selector"
        ),
        
        actionButton(
          "load_data",
          "Load Dataset",
          class = "btn-primary",
          width = "100%"
        ),
        
        br(),
        br(),
        
        helpText(
          "The workbook should contain longitude, latitude and claim amount variables."
        )
        
      ),
      
      div(
        
        h3(
          "Insurance Claims Dataset",
          class = "section-head"
        ),
        
        p(
          "Preview the data used by the spatial models.",
          class = "sub-desc"
        ),
        
        
        # ==========================================================
        # INLA SPATIAL MESH
        # ==========================================================
        
        div(
          class = "plot-card",
          
          h4(
            "INLA Spatial Mesh"
          ),
          
          p(
            "Triangular mesh used to represent the spatial random field
       across the study region.",
            class = "sub-desc"
          ),
          
          withSpinner(
            plotOutput(
              "mesh_plot",
              height = "600px"
            )
          )
          
        ),
        
        
        # ==========================================================
        # DATA TABLE
        # ==========================================================
        
        div(
          class = "plot-card",
          
          h4(
            "Claim Data"
          ),
          
          withSpinner(
            DTOutput(
              "data_table"
            )
          )
          
        )
        
      )
      
    )
    
  ),
  
  
  # ===================================================================================
  # TAB 4: EXPLORATION
  # ===================================================================================
  
  nav_panel(
    
    "Exploration",
    
    div(
      
      h3(
        "Exploratory Data Analysis",
        class = "section-head"
      ),
      
      p(
        "Explore the number and monetary value of precipitation-related claims over time.",
        class = "sub-desc"
      )
      
    ),
    
    div(
      class = "plot-card",
      
      h4("Number of Claims per Year"),
      
      withSpinner(
        plotOutput(
          "claims_per_year",
          height = "430px"
        )
      )
    ),
    
    div(
      class = "plot-card",
      
      h4("Claim Amount per Year"),
      
      withSpinner(
        plotOutput(
          "amount_per_year",
          height = "430px"
        )
      )
    )
    
  ),
  
  
  # ===================================================================================
  # TAB 5: SPATIAL ANALYSIS
  # ===================================================================================
  
  nav_panel(
    "Spatial Analysis",
    
    div(
      h3("Observed Insurance Claims", class = "section-head"),
      p(
        "Observed precipitation-related insurance claims plotted across South Africa.",
        class = "sub-desc"
      )
    ),
    
    div(
      class = "plot-card",
      h4("Observed Claim Locations"),
      withSpinner(
        leafletOutput("observed_map", height = "700px")
      )
    )
    
  ),
  
  
  # ===================================================================================
  # TAB 6: MODEL
  # ===================================================================================
  
  nav_panel(
    
    "Model",
    
    layout_sidebar(
      
      sidebar = sidebar(
        
        h4("Model Controls"),
        
        actionButton(
          "run_model",
          "Run Spatial Models",
          class = "btn-success",
          width = "100%"
        ),
        
        br(),
        br(),
        
        p(
          "The spatial models use the observed
   precipitation-related insurance claims
   to investigate how claim behaviour varies
   across South Africa."
        ),
        
        p(
          "First, the claim locations are modelled
   using a Poisson Point process.
   This models the spatial frequency of claims
   and identifies areas where claims are
   estimated to occur more frequently."
        ),
        
        p(
          "Second, the claim amounts are modelled
   using a Gamma distribution. This describes
   how the expected claim severity varies
   across different spatial locations."
        ),
        
        p(
          "Both models contain a latent spatial field
   represented using the INLA mesh. INLA is
   used to estimate these spatial effects and
   project them across the study region."
        ),
        
        p(
          "The maps on the right therefore show the
   estimated spatial effects obtained from
   the fitted models."
        )
        
      ),
      
      div(
        
        h3(
          "INLA Spatial Models",
          class = "section-head"
        ),
        
        p(
          "Visualisation of the fitted spatial models.",
          class = "sub-desc"
        ),
        
        div(
          class = "plot-card",
          
          h4("Projected Spatial Field: Claim Frequency"),
          
          withSpinner(
            plotOutput(
              "model_spatial_frequency",
              height = "650px"
            )
          )
          
        ),
        
        div(
          class = "plot-card",
          
          h4("Projected Spatial Field: Claim Severity"),
          
          withSpinner(
            plotOutput(
              "model_spatial_severity",
              height = "650px"
            )
          )
          
        )
        
      )
      
    )
    
  ),
  
  
  # ===================================================================================
  # TAB 7: STATS FOR NERDS
  # ===================================================================================
  
  nav_panel(
    
    "Stats for Nerds",
    
    div(
      
      h3(
        "Statistical Model Results",
        class = "section-head"
      ),
      
      p(
        "Detailed statistical output from the fitted INLA models.",
        class = "sub-desc"
      )
      
    ),
    
    div(
      class = "plot-card",
      
      h4("Point Process Model"),
      
      verbatimTextOutput(
        "pp_summary"
      )
      
    ),
    
    div(
      class = "plot-card",
      
      h4("Gamma Marks Model"),
      
      verbatimTextOutput(
        "gamma_summary"
      )
      
    ),
    
    div(
      class = "plot-card",
      
      h4("Model Diagnostics"),
      
      verbatimTextOutput(
        "diagnostics"
      )
      
    )
    
  ),
  
  
  # ===================================================================================
  # TAB 8: PREDICTIONS
  # ===================================================================================
  
  nav_panel(
    
    "Predictions",
    
    div(
      
      h3(
        "Predicted Spatial Insurance Risk",
        class = "section-head"
      ),
      
      p(
        "Predicted claim frequency, expected severity and combined insurance risk.",
        class = "sub-desc"
      )
      
    ),
    
    layout_column_wrap(
      
      width = 1/3,
      
      div(
        class = "value-box",
        
        div(
          class = "value-number",
          textOutput("min_intensity")
        ),
        
        div(
          class = "value-label",
          "Minimum Claim Intensity"
        )
      ),
      
      div(
        class = "value-box",
        
        div(
          class = "value-number",
          textOutput("max_intensity")
        ),
        
        div(
          class = "value-label",
          "Maximum Claim Intensity"
        )
      ),
      
      div(
        class = "value-box",
        
        div(
          class = "value-number",
          textOutput("mean_intensity")
        ),
        
        div(
          class = "value-label",
          "Mean Claim Intensity"
        )
      )
      
    ),
    
    br(),
    
    div(
      class = "plot-card",
      
      div(
        style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
        h4("Spatial Claim Frequency", style = "margin: 0;"),
        selectInput(
          "prediction_ci",
          "Credible Interval",
          choices = c(
            "80%" = 0.80,
            "90%" = 0.90,
            "95%" = 0.95,
            "99%" = 0.99
          ),
          selected = 0.95,
          width = "160px"
        )
      ),
      
      layout_column_wrap(
        width = 1/2,
        
        div(
          h5("Prediction Map"),
          withSpinner(
            plotOutput(
              "prediction_intensity",
              height = "600px"
            )
          )
        ),
        
        div(
          h5("Interactive Credible Interval"),
          withSpinner(
            plotOutput(
              "prediction_intensity_ci",
              height = "600px"
            )
          )
        )
      )
    ),
    
    div(
      class = "plot-card",
      
      h4("Spatial Claim Severity"),
      
      layout_column_wrap(
        width = 1/2,
        
        div(
          h5("Prediction Map"),
          withSpinner(
            plotOutput(
              "prediction_severity",
              height = "600px"
            )
          )
        ),
        
        div(
          h5("Interactive Credible Interval"),
          withSpinner(
            plotOutput(
              "prediction_severity_ci",
              height = "600px"
            )
          )
        )
      )
    ),
    
    div(
      class = "plot-card",
      
      h4("Overall Spatial Insurance Risk"),
      
      layout_column_wrap(
        width = 1/2,
        
        div(
          h5("Prediction Map"),
          withSpinner(
            plotOutput(
              "prediction_risk",
              height = "600px"
            )
          )
        ),
        
        div(
          h5("Interactive Credible Interval"),
          withSpinner(
            plotOutput(
              "prediction_risk_ci",
              height = "600px"
            )
          )
        )
      )
    )
    
  ),
  
  
  # ===================================================================================
  # TAB 9: CONCLUSION
  # ===================================================================================
  
  nav_panel(
    
    "Conclusion",
    
    div(
      
      class = "hero-banner",
      
      h1("Conclusion"),
      
      p(
        "A summary of the spatial modelling of precipitation-related ",
        "insurance claims using INLA."
      )
      
    ),
    
    br(),
    
    div(
      
      class = "plot-card",
      
      h3(
        "Research Summary",
        class = "section-head"
      ),
      
      p(
        "This study used 20,103 storm, hail and flood-related insurance ",
        "claims from Momentum Life to investigate the spatial behaviour ",
        "of precipitation-related insurance claims across South Africa."
      ),
      
      p(
        "A marked spatial point-process framework was used, with claim ",
        "locations representing the spatial points and claim amounts ",
        "representing the marks."
      ),
      
      p(
        "INLA was used to model claim frequency and severity while ",
        "accounting for spatial dependence through a latent Gaussian field."
      )
      
    ),
    
    br(),
    
    layout_column_wrap(
      
      width = 1/3,
      
      div(
        class = "info-card",
        
        h4("📍 Frequency"),
        
        p(
          "Claim frequency showed substantial spatial variation ",
          "across South Africa."
        ),
        
        p(
          "Higher estimated intensities were mainly observed in ",
          "the north-eastern and eastern regions."
        )
      ),
      
      div(
        class = "info-card info-card-green",
        
        h4("💰 Severity"),
        
        p(
          "Expected claim severity showed comparatively smaller ",
          "spatial variation."
        ),
        
        p(
          "Estimated expected claim amounts were approximately ",
          "R30,000 to R45,000."
        )
      ),
      
      div(
        class = "info-card info-card-orange",
        
        h4("⚠️ Risk"),
        
        p(
          "Frequency and severity were combined ",
          "to obtain a spatial ",
          "expected-loss measure."
        ),
        
        p(
          "Higher risk reflects the combined effect of claim ",
          "frequency and expected claim severity."
        )
      )
      
    ),
    
    br(),
    
    layout_column_wrap(
      
      width = 1/2,
      
      div(
        class = "plot-card",
        
        h3(
          "Research Limitations",
          class = "section-head"
        ),
        
        tags$ul(
          
          tags$li(
            "The application focused only on the spatial component."
          ),
          
          tags$li(
            "Storm, hail and flood claims were modelled jointly."
          ),
          
          tags$li(
            "Exposure information was not available."
          ),
          
          tags$li(
            "No direct MCMC comparison was performed."
          )
          
        )
        
      ),
      
      div(
        class = "plot-card",
        
        h3(
          "Future Research",
          class = "section-head"
        ),
        
        tags$ul(
          
          tags$li(
            "Incorporate the temporal component of the data."
          ),
          
          tags$li(
            "Model storm, hail and flood claims separately."
          ),
          
          tags$li(
            "Include meteorological and exposure information."
          ),
          
          tags$li(
            "Compare INLA with MCMC computationally."
          )
          
        )
        
      )
      
    ),
    
    br(),
    
    div(
      
      class = "plot-card",
      
      h3(
        "Overall Conclusion",
        class = "section-head"
      ),
      
      p(
        "The results demonstrate how marked spatial point processes, ",
        "latent Gaussian models and INLA can be combined to investigate ",
        "the spatial behaviour of precipitation-related insurance claims."
      ),
      
      p(
        "The framework provides a foundation for extending the analysis ",
        "to a more comprehensive spatial-temporal insurance risk model."
      )
      
    )
    
  )
  
)


# =====================================================================================
# 5. SERVER
# =====================================================================================

server <- function(input, output, session) {
  
  
  # ===================================================================================
  # DATA STORAGE
  # ===================================================================================
  
  data_store <- reactiveVal(NULL)
  
  model_data <- reactiveVal(NULL)
  
  pp_model <- reactiveVal(NULL)
  
  gamma_model <- reactiveVal(NULL)
  
  spatial_results <- reactiveVal(NULL)
  
  
  # ===================================================================================
  # SOUTH AFRICA MAP & PROVINCE BOUNDARIES
  # ===================================================================================
  
  sa_map <- ne_countries(
    scale = "medium",
    country = "South Africa",
    returnclass = "sf"
  )
  
  sa_provinces <- readRDS("sa_provinces.rds")
  
  
  # ===================================================================================
  # INLA SPATIAL MESH
  # ===================================================================================
  
  mesh_data <- reactive({
    
    req(model_data())
    
    modelData <- model_data()
    
    coords <- as.matrix(
      modelData[
        ,
        c("Longitude", "Latitude")
      ]
    )
    
    
    # South African boundary
    
    sa_sp <- as(
      sa_map,
      "Spatial"
    )
    
    sa_boundary <- inla.sp2segment(
      sa_sp
    )
    
    
    # Create INLA mesh
    
    mesh <- inla.mesh.2d(
      
      boundary = sa_boundary,
      
      max.edge = c(
        0.4,
        1.8
      ),
      
      cutoff = 0.15
      
    )
    
    
    # ----------------------------------------------------------
    # Create mesh triangle segments
    # ----------------------------------------------------------
    
    triangles <- mesh$graph$tv
    
    mesh_segments <- do.call(
      rbind,
      lapply(
        seq_len(nrow(triangles)),
        function(i) {
          
          triangle <- c(
            triangles[i, ],
            triangles[i, 1]
          )
          
          data.frame(
            Longitude = mesh$loc[
              triangle,
              1
            ],
            Latitude = mesh$loc[
              triangle,
              2
            ],
            Triangle = i
          )
          
        }
      )
    )
    
    
    list(
      mesh = mesh,
      segments = mesh_segments,
      modelData = modelData
    )
    
  })
  
  
  
  # ===================================================================================
  # MESH PLOT
  # ===================================================================================
  
  output$mesh_plot <- renderPlot({
    
    mesh_info <- mesh_data()
    
    mesh_segments <- mesh_info$segments
    
    modelData <- mesh_info$modelData
    
    
    ggplot() +
      
      # --------------------------------------------------------
    # INLA triangular mesh
    # --------------------------------------------------------
    
    geom_path(
      data = mesh_segments,
      aes(
        x = Longitude,
        y = Latitude,
        group = Triangle
      ),
      colour = "grey55",
      linewidth = 0.35
    ) +
      
      
      # --------------------------------------------------------
    # South Africa boundary
    # --------------------------------------------------------
    
    geom_sf(
      data = sa_map,
      fill = NA,
      colour = "black",
      linewidth = 0.8
    ) +
      
      
      # --------------------------------------------------------
    # Observed claim locations
    # --------------------------------------------------------
    
    geom_point(
      data = modelData,
      aes(
        x = Longitude,
        y = Latitude
      ),
      colour = "#DD6B20",
      size = 1.2,
      alpha = 0.45
    ) +
      
      
      # --------------------------------------------------------
    # Coordinate limits
    # --------------------------------------------------------
    
    coord_sf(
      xlim = c(
        16,
        33
      ),
      ylim = c(
        -35,
        -22
      ),
      expand = FALSE
    ) +
      
      
      # --------------------------------------------------------
    # Labels
    # --------------------------------------------------------
    
    labs(
      title = "INLA Spatial Mesh",
      subtitle = paste0(
        "Triangular mesh with ",
        mesh_info$mesh$n,
        " vertices and ",
        nrow(mesh_info$mesh$graph$tv),
        " triangles"
      ),
      x = "Longitude",
      y = "Latitude"
    ) +
      
      
      # --------------------------------------------------------
    # Theme
    # --------------------------------------------------------
    
    theme_minimal(
      base_size = 13
    ) +
      
      theme(
        plot.title = element_text(
          face = "bold",
          size = 16,
          hjust = 0.5
        ),
        
        plot.subtitle = element_text(
          size = 11,
          hjust = 0.5
        ),
        
        axis.title = element_text(
          face = "bold"
        ),
        
        panel.grid.major = element_line(
          colour = "grey85"
        ),
        
        panel.grid.minor = element_blank(),
        
        plot.margin = margin(
          10,
          10,
          10,
          10
        )
      )
    
  })
  # ===================================================================================
  # STANDARDIZED SPATIAL MAP HELPER FUNCTION
  # ===================================================================================
  
  create_spatial_map <- function(
    data,
    fill_var,
    title,
    subtitle = NULL,
    legend_title = "Value",
    palette_option = "magma",
    trans = "log10"
  ) {
    ggplot() +
      geom_raster(
        data = data,
        aes(x = Longitude, y = Latitude, fill = .data[[fill_var]]),
        interpolate = TRUE
      ) +
      geom_sf(
        data = sa_provinces,
        fill = NA,
        colour = "black",
        linewidth = 0.4
      ) + 
      scale_fill_viridis_c(
        option = palette_option,
        trans = trans,
        name = legend_title,
        na.value = "transparent",
        labels = label_comma()
      ) +
      coord_sf(
        xlim = c(16, 33),
        ylim = c(-35, -22),
        expand = FALSE
      ) +
      labs(
        title = title,
        subtitle = subtitle,
        x = "Longitude",
        y = "Latitude"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
        plot.subtitle = element_text(size = 10, colour = "grey30", hjust = 0.5),
        axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        panel.grid.major = element_line(colour = "grey85"),
        panel.grid.minor = element_blank(),
        plot.margin = margin(10, 10, 10, 10)
      )
  }
  
  # ===================================================================================
  # OVERVIEW MAP
  # ===================================================================================
  
  output$overview_map <- renderPlot({
    
    bbox <- st_bbox(sa_map)
    
    ggplot() +
      geom_sf(
        data = sa_map,
        fill = "grey85",
        colour = "black"
      ) +
      coord_sf(
        xlim = c(bbox["xmin"], bbox["xmax"]-5),
        ylim = c(bbox["ymin"]+10, bbox["ymax"]),
        expand = FALSE
      ) +
      labs(
        title = "South Africa",
        subtitle = "Study region for spatial insurance claim analysis",
        x = "Longitude",
        y = "Latitude"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(
          size = 16,
          face = "bold"
        ),
        plot.subtitle = element_text(
          size = 11
        ),
        plot.margin = margin(
          5, 5, 5, 5
        )
      )
  })
  
  # ===================================================================================
  # BACKGROUND / METHODOLOGY SLIDESHOW
  # ===================================================================================
  
  background_page <- reactiveVal(1)
  
  
  observeEvent(input$background_next, {
    
    background_page(
      min(background_page() + 1, 9)
    )
    
  })
  
  
  observeEvent(input$background_prev, {
    
    background_page(
      max(background_page() - 1, 1)
    )
    
  })
  
  
  output$background_page <- renderUI({
    
    page <- background_page()
    
    if(page == 1) {
      
      tagList(
        
        h3("1. Marked Spatial Point Process"),
        
        p(
          "Each insurance claim is represented by its geographical
         location together with the amount associated with that claim."
        ),
        
        withMathJax(
          helpText(
            "$$\\mathbf{X} = \\{x_i,m(x_i)\\}_{i=1}^{n}$$"
          )
        ),
        
        p(
          "The location represents where the claim occurred, while the
         mark represents the claim amount."
        ),
        
        
        
        h3("2. Claim Frequency"),
        
        p(
          "The point process component models where precipitation-related
         insurance claims are likely to occur."
        ),
        
        withMathJax(
          helpText(
            "$$N(A) \\sim Poisson\\left(\\int_A \\lambda(u)\\,du\\right)$$"
          )
        ),
        
        p(
          "The intensity function λ(u) describes the expected frequency
         of claims at a particular spatial location."
        ),
        
        withMathJax(
          helpText(
            "$$\\lambda(u)$$"
          )
        ),
        
        h3("3. Claim Severity"),
        
        p(
          "The amount associated with each claim is modelled separately
         from the claim frequency."
        ),
        
        withMathJax(
          helpText(
            "$$m(x_i) \\sim Gamma(\\mu_i,\\phi)$$"
          )
        ),
        
        p(
          "The Gamma distribution is used because insurance claim amounts
         are positive and typically right-skewed."
        ),
        
        
        h3("4. Linear Predictor"),
        
        p(
          "The latent spatial field is incorporated into the claim
         intensity through the linear predictor."
        ),
        
        withMathJax(
          helpText(
            "$$\\log(\\lambda(u)) = \\beta_0 + x(u)$$"
          )
        ),
        
        p(
          "The logarithm ensures that the predicted claim intensity
         remains positive."
        )
        
      )
      
    } else if(page == 2) {
      
      tagList(
        
        h3("5. Bayesian Model Structure"),
        
        p(
          "The model combines observed claim information with prior
         distributions for the unknown parameters and spatial effects."
        ),
        
        p(
          "The resulting model is a latent Gaussian model, which makes
         it suitable for inference using INLA."
        ),
        
        withMathJax(
          helpText(
            "$$x\\mid\\theta \\sim N(0,Q^{-1}(\\theta))$$"
          ),
          
          
          h3("6. Frequency and Severity"),
          
          p(
            "The analysis separates insurance risk into two components:"
          ),
          
          tags$ul(
            
            tags$li(
              tags$b("Frequency: "),
              "how often claims are expected to occur."
            ),
            
            tags$li(
              tags$b("Severity: "),
              "how expensive those claims are expected to be."
            )
            
          ),
          
          p(
            "These components can subsequently be combined to obtain
         an overall measure of spatial insurance risk."
          ),
          
          
          h3("7. Integrated Nested Laplace Approximation"),
          
          p(
            "INLA is used to perform Bayesian inference for the latent
         Gaussian model."
          ),
          
          p(
            "Rather than relying on computationally expensive simulation
         methods such as MCMC, INLA provides accurate approximations
         to the posterior distributions of the model parameters."
          ),
          
          p(
            "This makes INLA particularly useful for spatial models
         involving large datasets."
          ),
          
          
          
          h3("8. Posterior Inference"),
          
          p(
            "The final model produces posterior distributions for the
         parameters and spatial effects."
          ),
          
          withMathJax(
            helpText(
              "$$p(x,\\theta\\mid y)$$"
            )
          ),
          
          p(
            "These posterior results are used to obtain spatial predictions
         of claim frequency, claim severity and overall insurance risk."
          )
          
        )
        
      )
      
    }
    
  })
  
  
  # ===================================================================================
  # DYNAMIC SHEET SELECTOR
  # ===================================================================================
  
  output$sheet_selector <- renderUI({
    
    req(input$file)
    
    sheets <- excel_sheets(
      input$file$datapath
    )
    
    selectInput(
      "selected_sheet",
      "Select Worksheet",
      choices = sheets,
      selected = sheets[1]
    )
    
  })
  
  
  # ===================================================================================
  # LOAD DATA
  # ===================================================================================
  
  observeEvent(input$load_data, {
    
    req(input$file)
    req(input$selected_sheet)
    
    dat <- read_excel(
      input$file$datapath,
      sheet = input$selected_sheet
    )
    
    data_store(dat)
    
    showNotification(
      "Dataset loaded successfully.",
      type = "message"
    )
    
  })
  
  
  # ===================================================================================
  # DATA TABLE
  # ===================================================================================
  
  output$data_table <- renderDT({
    
    req(data_store())
    
    datatable(
      data_store(),
      options = list(
        pageLength = 10,
        scrollX = TRUE
      ),
      filter = "top"
    )
    
  })
  
  
  # ===================================================================================
  # PREPARE MODEL DATA
  # ===================================================================================
  
  observe({
    
    req(data_store())
    
    dat <- data_store()
    
    required_columns <- c(
      "DAY_X_CORD",
      "DAY_Y_CORD",
      "AMOUNT"
    )
    
    if (!all(required_columns %in% names(dat))) {
      
      model_data(NULL)
      
      return()
      
    }
    
    
    modelData <- dat %>%
      
      dplyr::select(
        Longitude = DAY_X_CORD,
        Latitude = DAY_Y_CORD,
        Amount = AMOUNT
      ) %>%
      
      dplyr::filter(
        !is.na(Longitude),
        !is.na(Latitude),
        !is.na(Amount),
        Amount > 0
      )
    
    
    model_data(modelData)
    
  })
  
  
  # ===================================================================================
  # EXPLORATION: CLAIMS PER YEAR
  # ===================================================================================
  
  output$claims_per_year <- renderPlot({
    
    req(data_store())
    
    dat <- data_store()
    
    req("CLM_LOSS_DT" %in% names(dat))
    
    dat$Year <- as.numeric(
      substr(
        as.character(dat$CLM_LOSS_DT),
        1,
        4
      )
    )
    
    dat <- dat %>%
      filter(!is.na(Year))
    
    # When the combined precipitation worksheet is selected,
    # show HAIL, FLOOD and STORM side-by-side in different colours.
    if (identical(input$selected_sheet, "PRECIPITATION DATA") &&
        "CAUSE" %in% names(dat)) {
      
      yearly_claims <- dat %>%
        mutate(CAUSE = as.character(CAUSE)) %>%
        group_by(Year, CAUSE) %>%
        summarise(
          Claims = n(),
          .groups = "drop"
        )
      
      ggplot(
        yearly_claims,
        aes(
          x = factor(Year),
          y = Claims,
          fill = CAUSE
        )
      ) +
        geom_col(position = "dodge") +
        scale_fill_manual(
          values = c(
            "HAIL" = "dodgerblue",
            "FLOOD" = "forestgreen",
            "STORM" = "red"
          )
        ) +
        labs(
          title = "Claims per year by precipitation type (2026: 4 months)",
          x = "Year",
          y = "Number of Claims",
          fill = "Claim Type"
        ) +
        theme_minimal(base_size = 13)
      
    } else {
      
      yearly_claims <- dat %>%
        count(Year)
      
      ggplot(
        yearly_claims,
        aes(
          x = factor(Year),
          y = n
        )
      ) +
        geom_col(fill = "#176B87") +
        labs(
          title = "Claims per year by selected precipitation type (2026: 4 months)",
          x = "Year",
          y = "Number of Claims"
        ) +
        theme_minimal(base_size = 13)
    }
    
  })
  
  
  # ===================================================================================
  # EXPLORATION: AMOUNT PER YEAR
  # ===================================================================================
  
  output$amount_per_year <- renderPlot({
    
    req(data_store())
    
    dat <- data_store()
    
    req("CLM_LOSS_DT" %in% names(dat))
    
    dat$Year <- as.numeric(
      substr(
        as.character(dat$CLM_LOSS_DT),
        1,
        4
      )
    )
    
    dat <- dat %>%
      filter(!is.na(Year))
    
    # When the combined precipitation worksheet is selected,
    # show HAIL, FLOOD and STORM side-by-side in different colours.
    if (identical(input$selected_sheet, "PRECIPITATION DATA") &&
        "CAUSE" %in% names(dat)) {
      
      yearly_amount <- dat %>%
        mutate(CAUSE = as.character(CAUSE)) %>%
        group_by(Year, CAUSE) %>%
        summarise(
          TotalAmount = sum(AMOUNT, na.rm = TRUE),
          .groups = "drop"
        )
      
      ggplot(
        yearly_amount,
        aes(
          x = factor(Year),
          y = TotalAmount,
          fill = CAUSE
        )
      ) +
        geom_col(position = "dodge") +
        scale_fill_manual(
          values = c(
            "HAIL" = "dodgerblue",
            "FLOOD" = "forestgreen",
            "STORM" = "red"
          )
        ) +
        scale_y_continuous(labels = label_comma()) +
        labs(
          title = "Claim amount per year by precipitation type (2026: 4 months)",
          x = "Year",
          y = "Total Claim Amount",
          fill = "Claim Type"
        ) +
        theme_minimal(base_size = 13)
      
    } else {
      
      yearly_amount <- dat %>%
        group_by(Year) %>%
        summarise(
          TotalAmount = sum(AMOUNT, na.rm = TRUE),
          .groups = "drop"
        )
      
      ggplot(
        yearly_amount,
        aes(
          x = factor(Year),
          y = TotalAmount
        )
      ) +
        geom_col(fill = "#1F8A70") +
        scale_y_continuous(labels = label_comma()) +
        labs(
          title = "Claim amount per year by selected precipitation type (2026: 4 months)",
          x = "Year",
          y = "Total Claim Amount"
        ) +
        theme_minimal(base_size = 13)
    }
    
  })
  
  # ===================================================================================
  # SPATIAL ANALYSIS: OBSERVED CLAIM MAP
  # ===================================================================================
  
  output$observed_map <- renderLeaflet({
    req(data_store())
    
    dat <- data_store() %>%
      filter(
        !is.na(DAY_X_CORD),
        !is.na(DAY_Y_CORD)
      )
    
    req(nrow(dat) > 0)
    
    if ("CAUSE" %in% names(dat) &&
        all(c("HAIL", "FLOOD", "STORM") %in% unique(as.character(dat$CAUSE)))) {
      
      dat$CAUSE <- as.character(dat$CAUSE)
      pal <- colorFactor(
        palette = c(
          "HAIL" = "dodgerblue",
          "FLOOD" = "forestgreen",
          "STORM" = "red"
        ),
        domain = c("HAIL", "FLOOD", "STORM")
      )
      
      popup_text <- if ("CLAIM_NUMBER" %in% names(dat)) {
        paste0(
          "<b>Claim:</b> ", dat$CLAIM_NUMBER,
          "<br><b>Cause:</b> ", dat$CAUSE,
          if ("AMOUNT" %in% names(dat)) paste0("<br><b>Amount:</b> ", dat$AMOUNT) else ""
        )
      } else {
        paste0(
          "<b>Cause:</b> ", dat$CAUSE,
          if ("AMOUNT" %in% names(dat)) paste0("<br><b>Amount:</b> ", dat$AMOUNT) else ""
        )
      }
      
      leaflet(dat) %>%
        addTiles() %>%
        addCircleMarkers(
          lng = ~DAY_X_CORD,
          lat = ~DAY_Y_CORD,
          color = ~pal(CAUSE),
          radius = 3,
          stroke = FALSE,
          fillOpacity = 0.8,
          popup = popup_text
        ) %>%
        addLegend(
          "bottomright",
          pal = pal,
          values = ~CAUSE,
          title = "Claim Type"
        )
      
    } else {
      
      popup_text <- if ("CLAIM_NUMBER" %in% names(dat)) {
        paste0(
          "<b>Claim:</b> ", dat$CLAIM_NUMBER,
          if ("AMOUNT" %in% names(dat)) paste0("<br><b>Amount:</b> ", dat$AMOUNT) else ""
        )
      } else {
        if ("AMOUNT" %in% names(dat)) paste0("<b>Amount:</b> ", dat$AMOUNT) else "Observed claim"
      }
      
      leaflet(dat) %>%
        addTiles() %>%
        addCircleMarkers(
          lng = ~DAY_X_CORD,
          lat = ~DAY_Y_CORD,
          color = "#176B87",
          radius = 3,
          stroke = FALSE,
          fillOpacity = 0.8,
          popup = popup_text
        )
    }
  })
  
  
  # ===================================================================================
  # RUN MODELS
  # ===================================================================================
  
  observeEvent(input$run_model, {
    
    req(model_data())
    
    modelData <- model_data()
    
    showNotification(
      "Running spatial models. This may take some time...",
      type = "message",
      duration = NULL,
      id = "model_running"
    )
    
    
    # ===============================================================================
    # COORDINATES
    # ===============================================================================
    
    coords <- as.matrix(
      modelData[
        ,
        c("Longitude", "Latitude")
      ]
    )
    
    
    # ===============================================================================
    # SOUTH AFRICAN BOUNDARY
    # ===============================================================================
    
    sa_sp <- as(
      sa_map,
      "Spatial"
    )
    
    sa_boundary <- inla.sp2segment(
      sa_sp
    )
    
    
    # ===============================================================================
    # SPATIAL MESH
    # ===============================================================================
    
    mesh <- inla.mesh.2d(
      
      boundary = sa_boundary,
      
      max.edge = c(
        0.4,
        1.8
      ),
      
      cutoff = 0.15
      
    )
    
    
    # ===============================================================================
    # SPDE MODEL
    # ===============================================================================
    
    spde <- inla.spde2.pcmatern(
      
      mesh = mesh,
      
      prior.range = c(
        8,
        0.5
      ),
      
      prior.sigma = c(
        0.5,
        0.01
      )
      
    )
    
    
    # ===============================================================================
    # FEM INTEGRATION WEIGHTS
    # ===============================================================================
    
    fem <- inla.mesh.fem(
      mesh,
      order = 1
    )
    
    weights <- as.numeric(
      Matrix::rowSums(
        fem$c0
      )
    )
    
    locations <- mesh$loc[
      ,
      1:2,
      drop = FALSE
    ]
    
    
    # ===============================================================================
    # POINT PROCESS DATA
    # ===============================================================================
    
    event_locations <- coords
    
    integration_locations <- locations
    
    n_events <- nrow(
      event_locations
    )
    
    n_integration <- nrow(
      integration_locations
    )
    
    
    y_pp <- c(
      rep(1, n_events),
      rep(0, n_integration)
    )
    
    
    E_pp <- c(
      rep(0, n_events),
      weights
    )
    
    
    # ===============================================================================
    # A MATRICES
    # ===============================================================================
    
    A_event <- inla.spde.make.A(
      mesh = mesh,
      loc = event_locations
    )
    
    A_integration <- inla.spde.make.A(
      mesh = mesh,
      loc = integration_locations
    )
    
    A_all <- rbind(
      A_event,
      A_integration
    )
    
    
    # ===============================================================================
    # SPATIAL INDEX
    # ===============================================================================
    
    spatial_index <- inla.spde.make.index(
      name = "spatial",
      n.spde = spde$n.spde
    )
    
    
    # ===============================================================================
    # POINT PROCESS STACK
    # ===============================================================================
    
    n_pp <- length(
      y_pp
    )
    
    
    stack_pp <- inla.stack(
      
      data = list(
        y = y_pp,
        E = E_pp
      ),
      
      A = list(
        1,
        A_all
      ),
      
      effects = list(
        
        list(
          Intercept = rep(
            1,
            n_pp
          )
        ),
        
        spatial_index
        
      ),
      
      tag = "point_process"
      
    )
    
    
    # ===============================================================================
    # POINT PROCESS MODEL
    # ===============================================================================
    
    formula_pp <- y ~
      -1 +
      Intercept +
      f(
        spatial,
        model = spde
      )
    
    
    point_process_model <- inla(
      
      formula_pp,
      
      family = "poisson",
      
      data = inla.stack.data(
        stack_pp
      ),
      
      E = inla.stack.data(
        stack_pp
      )$E,
      
      control.predictor = list(
        
        A = inla.stack.A(
          stack_pp
        ),
        
        compute = TRUE
        
      ),
      
      control.compute = list(
        
        dic = TRUE,
        waic = TRUE,
        cpo = TRUE,
        config = TRUE  # Enabled config = TRUE for posterior sampling
        
      ),
      
      control.inla = list(
        strategy = "adaptive"
      ),
      
      verbose = FALSE
      
    )
    
    
    # ===============================================================================
    # GAMMA MODEL
    # ===============================================================================
    
    n_gamma <- nrow(
      modelData
    )
    
    
    stack_gamma <- inla.stack(
      
      data = list(
        marks = modelData$Amount
      ),
      
      A = list(
        1,
        A_event
      ),
      
      effects = list(
        
        list(
          Intercept = rep(
            1,
            n_gamma
          )
        ),
        
        spatial_index
        
      ),
      
      tag = "gamma_marks"
      
    )
    
    
    formula_gamma <- marks ~
      -1 +
      Intercept +
      f(
        spatial,
        model = spde
      )
    
    
    gamma_model <- inla(
      
      formula_gamma,
      
      family = "gamma",
      
      data = inla.stack.data(
        stack_gamma
      ),
      
      control.predictor = list(
        
        A = inla.stack.A(
          stack_gamma
        ),
        
        compute = TRUE
        
      ),
      
      control.compute = list(
        
        dic = TRUE,
        waic = TRUE,
        cpo = TRUE,
        config = TRUE  # Enabled config = TRUE for posterior sampling
        
      ),
      
      control.inla = list(
        strategy = "adaptive"
      ),
      
      verbose = FALSE
      
    )
    
    
    # ===============================================================================
    # REGULAR PROJECTION GRID
    # ===============================================================================
    
    projector <- inla.mesh.projector(
      
      mesh,
      
      xlim = c(
        16,
        33
      ),
      
      ylim = c(
        -35,
        -22
      ),
      
      dims = c(
        300,
        300
      )
      
    )
    
    
    # ===============================================================================
    # GRID
    # ===============================================================================
    
    grid_df <- expand.grid(
      
      Longitude = projector$x,
      
      Latitude = projector$y
      
    )
    
    
    # ===============================================================================
    # RAW SPATIAL PROJECTIONS
    # ===============================================================================
    
    raw_pp <- inla.mesh.project(
      
      projector,
      
      point_process_model$
        summary.random$
        spatial$
        mean
      
    )
    
    
    raw_gamma <- inla.mesh.project(
      
      projector,
      
      gamma_model$
        summary.random$
        spatial$
        mean
      
    )
    
    
    grid_df$Raw_PP <- as.vector(
      raw_pp
    )
    
    grid_df$Raw_Gamma <- as.vector(
      raw_gamma
    )
    
    
    # ===============================================================================
    # SPLINE SMOOTHING
    # ===============================================================================
    
    valid_pp <- !is.na(
      grid_df$Raw_PP
    )
    
    valid_gamma <- !is.na(
      grid_df$Raw_Gamma
    )
    
    
    k_pp <- min(
      120,
      max(
        10,
        floor(
          sqrt(
            sum(valid_pp)
          )
        )
      )
    )
    
    k_gamma <- min(
      120,
      max(
        10,
        floor(
          sqrt(
            sum(valid_gamma)
          )
        )
      )
    )
    
    
    smooth_pp <- gam(
      
      Raw_PP ~
        s(
          Longitude,
          Latitude,
          k = k_pp
        ),
      
      data = grid_df[
        valid_pp,
      ]
      
    )
    
    
    smooth_gamma <- gam(
      
      Raw_Gamma ~
        s(
          Longitude,
          Latitude,
          k = k_gamma
        ),
      
      data = grid_df[
        valid_gamma,
      ]
      
    )
    
    
    grid_df$SpatialEffect_PP <- NA_real_
    
    grid_df$SpatialEffect_Gamma <- NA_real_
    
    
    grid_df$SpatialEffect_PP[
      valid_pp
    ] <- predict(
      
      smooth_pp,
      
      grid_df[
        valid_pp,
      ]
      
    )
    
    
    grid_df$SpatialEffect_Gamma[
      valid_gamma
    ] <- predict(
      
      smooth_gamma,
      
      grid_df[
        valid_gamma,
      ]
      
    )
    
    
    # ===============================================================================
    # FIXED EFFECTS
    # ===============================================================================
    
    beta0_pp <- point_process_model$
      summary.fixed[
        "Intercept",
        "mean"
      ]
    
    
    beta0_gamma <- gamma_model$
      summary.fixed[
        "Intercept",
        "mean"
      ]
    
    
    # ===============================================================================
    # PREDICTED INTENSITY / SEVERITY / RISK
    # ===============================================================================
    
    grid_df <- grid_df %>%
      
      mutate(
        
        Intensity =
          exp(
            beta0_pp +
              SpatialEffect_PP
          ),
        
        ExpectedSeverity =
          exp(
            beta0_gamma +
              SpatialEffect_Gamma
          ),
        
        ExpectedRisk =
          Intensity *
          ExpectedSeverity
        
      )
    
    
    # ===============================================================================
    # SOUTH AFRICA MASK
    # ===============================================================================
    
    grid_sf <- st_as_sf(
      
      grid_df,
      
      coords = c(
        "Longitude",
        "Latitude"
      ),
      
      crs = 4326,
      
      remove = FALSE
      
    )
    
    
    inside_sa <- as.vector(
      
      st_intersects(
        grid_sf,
        sa_map,
        sparse = FALSE
      )
      
    )
    
    
    grid_df$SpatialEffect_PP[
      !inside_sa
    ] <- NA
    
    grid_df$SpatialEffect_Gamma[
      !inside_sa
    ] <- NA
    
    grid_df$Intensity[
      !inside_sa
    ] <- NA
    
    grid_df$ExpectedSeverity[
      !inside_sa
    ] <- NA
    
    grid_df$ExpectedRisk[
      !inside_sa
    ] <- NA
    
    
    # ===============================================================================
    # SAVE EVERYTHING
    # ===============================================================================
    
    pp_model(
      point_process_model
    )
    
    gamma_model(
      gamma_model
    )
    
    
    spatial_results(
      
      list(
        
        grid = grid_df,
        
        mesh = mesh,
        
        modelData = modelData,
        
        projector = projector,
        
        spatial_pp =
          point_process_model$
          summary.random$
          spatial,
        
        spatial_gamma =
          gamma_model$
          summary.random$
          spatial
        
      )
      
    )
    
    
    removeNotification(
      "model_running"
    )
    
    showNotification(
      "Spatial models completed successfully.",
      type = "message"
    )
    
  })
  
  
  # ===================================================================================
  # MODEL SUMMARY
  # ===================================================================================
  
  output$pp_summary <- renderPrint({
    
    req(pp_model())
    
    model <- pp_model()
    
    cat(
      "POINT PROCESS MODEL\n",
      "===================\n\n"
    )
    
    cat(
      "Fixed Effects:\n\n"
    )
    
    print(
      model$summary.fixed
    )
    
    cat(
      "\n\nHyperparameters:\n\n"
    )
    
    print(
      model$summary.hyperpar
    )
    
    cat(
      "\n\nDIC:\n"
    )
    
    print(
      model$dic$dic
    )
    
    cat(
      "\n\nWAIC:\n"
    )
    
    print(
      model$waic$waic
    )
    
  })
  
  
  # ===================================================================================
  # GAMMA SUMMARY
  # ===================================================================================
  
  output$gamma_summary <- renderPrint({
    
    req(gamma_model())
    
    model <- gamma_model()
    
    cat(
      "GAMMA MARKS MODEL\n",
      "=================\n\n"
    )
    
    cat(
      "Fixed Effects:\n\n"
    )
    
    print(
      model$summary.fixed
    )
    
    cat(
      "\n\nHyperparameters:\n\n"
    )
    
    print(
      model$summary.hyperpar
    )
    
    cat(
      "\n\nDIC:\n"
    )
    
    print(
      model$dic$dic
    )
    
    cat(
      "\n\nWAIC:\n"
    )
    
    print(
      model$waic$waic
    )
    
  })
  
  
  # ===================================================================================
  # DIAGNOSTICS
  # ===================================================================================
  
  output$diagnostics <- renderPrint({
    
    req(
      pp_model(),
      gamma_model()
    )
    
    cat(
      "POINT PROCESS MODEL\n"
    )
    
    cat(
      "DIC:",
      pp_model()$dic$dic,
      "\n"
    )
    
    cat(
      "WAIC:",
      pp_model()$waic$waic,
      "\n\n"
    )
    
    
    cat(
      "GAMMA MARK MODEL\n"
    )
    
    cat(
      "DIC:",
      gamma_model()$dic$dic,
      "\n"
    )
    
    cat(
      "WAIC:",
      gamma_model()$waic$waic,
      "\n\n"
    )
    
    
    cat(
      "CPO failures - Point Process:\n"
    )
    
    print(
      summary(
        pp_model()$cpo$failure
      )
    )
    
    
    cat(
      "\nCPO failures - Gamma:\n"
    )
    
    print(
      summary(
        gamma_model()$cpo$failure
      )
    )
    
  })
  
  
  # ===================================================================================
  # MODEL TAB: PROJECTED CLAIM FREQUENCY SPATIAL FIELD
  # ===================================================================================
  
  output$model_spatial_frequency <- renderPlot({
    req(spatial_results())
    grid_df <- spatial_results()$grid
    
    create_spatial_map(
      data = grid_df,
      fill_var = "SpatialEffect_PP",
      title = "Projected Spatial Field: Claim Frequency",
      subtitle = "Spatial effect on frequency",
      legend_title = "Spatial\nEffect",
      palette_option = "magma",
      trans = "identity"
    )
  })
  
  # ===================================================================================
  # MODEL TAB: PROJECTED CLAIM SEVERITY SPATIAL FIELD
  # ===================================================================================
  
  output$model_spatial_severity <- renderPlot({
    req(spatial_results())
    grid_df <- spatial_results()$grid
    
    create_spatial_map(
      data = grid_df,
      fill_var = "SpatialEffect_Gamma",
      title = "Projected Spatial Field: Claim Severity",
      subtitle = "Spatial effect on severity",
      legend_title = "Spatial\nEffect",
      palette_option = "magma",
      trans = "identity"
    )
  })
  
  # ===================================================================================
  # PREDICTION VALUE BOXES
  # ===================================================================================
  
  output$min_intensity <- renderText({
    
    req(
      spatial_results()
    )
    
    grid <- spatial_results()$grid
    
    format(
      min(
        grid$Intensity,
        na.rm = TRUE
      ),
      big.mark = ",",
      digits = 3
    )
    
  })
  
  
  output$max_intensity <- renderText({
    
    req(
      spatial_results()
    )
    
    grid <- spatial_results()$grid
    
    format(
      max(
        grid$Intensity,
        na.rm = TRUE
      ),
      big.mark = ",",
      digits = 3
    )
    
  })
  
  
  output$mean_intensity <- renderText({
    
    req(
      spatial_results()
    )
    
    grid <- spatial_results()$grid
    
    format(
      mean(
        grid$Intensity,
        na.rm = TRUE
      ),
      big.mark = ",",
      digits = 3
    )
    
  })
  
  
  # ===================================================================================
  # POSTERIOR SAMPLES FOR INTERACTIVE CREDIBLE INTERVAL PLOTS
  # ===================================================================================
  
  prediction_posterior <- reactive({
    
    prediction_file <- "prediction_posterior_samples.rds"
    
    req(
      file.exists(prediction_file)
    )
    
    readRDS(
      prediction_file
    )
    
  })
  
  # ===================================================================================
  # CREDIBLE INTERVAL DATA
  # ===================================================================================
  
  prediction_ci_data <- reactive({
    req(prediction_posterior())
    
    posterior <- prediction_posterior()
    level_key <- as.character(as.numeric(input$prediction_ci))
    
    posterior$ci_data[[level_key]]
  })
  
  
  # ===================================================================================
  # PREDICTION PLOT & CI: INTENSITY (FREQUENCY)
  # ===================================================================================
  
  output$prediction_intensity <- renderPlot({
    req(spatial_results())
    grid_df <- spatial_results()$grid
    
    create_spatial_map(
      data = grid_df,
      fill_var = "Intensity",
      title = "Estimated Spatial Claim Frequency",
      subtitle = "Expected number of claims across South Africa",
      legend_title = "Expected\nFrequency\nin numbers",
      palette_option = "magma",
      trans = "log10"
    )
  })
  
  output$prediction_intensity_ci <- renderPlot({
    req(model_data())
    ci <- prediction_ci_data()
    level <- as.numeric(input$prediction_ci) * 100
    
    d <- ci$intensity %>%
      dplyr::filter(inside) %>%
      dplyr::mutate(uncertainty = pmax((upper - lower) / median, .Machine$double.eps))
    
    create_spatial_map(
      data = d,
      fill_var = "uncertainty",
      title = paste0("Frequency Uncertainty (", level, "% Credible Interval)"),
      subtitle = "Relative uncertainty ((Upper - Lower) / Median)",
      legend_title = "CI Width",
      palette_option = "magma",
      trans = "log10"
    )
  }, width = "auto", height = 600)
  
  # ===================================================================================
  # PREDICTION PLOT & CI: SEVERITY
  # ===================================================================================
  
  output$prediction_severity <- renderPlot({
    req(spatial_results())
    grid_df <- spatial_results()$grid
    
    create_spatial_map(
      data = grid_df,
      fill_var = "ExpectedSeverity",
      title = "Estimated Spatial Claim Severity",
      subtitle = "Expected claim amount across South Africa",
      legend_title = "Expected\nSeverity\nin Rands",
      palette_option = "magma",
      trans = "log10"
    )
  })
  
  output$prediction_severity_ci <- renderPlot({
    req(model_data())
    ci <- prediction_ci_data()
    level <- as.numeric(input$prediction_ci) * 100
    
    d <- ci$severity %>%
      dplyr::filter(inside) %>%
      dplyr::mutate(uncertainty = pmax((upper - lower) / median, .Machine$double.eps))
    
    create_spatial_map(
      data = d,
      fill_var = "uncertainty",
      title = paste0("Severity Uncertainty (", level, "% Credible Interval)"),
      subtitle = "Relative uncertainty ((Upper - Lower) / Median)",
      legend_title = "CI Width",
      palette_option = "magma",
      trans = "log10"
    )
  }, width = "auto", height = 600)
  
  # ===================================================================================
  # PREDICTION PLOT & CI: RISK
  # ===================================================================================
  
  output$prediction_risk <- renderPlot({
    req(spatial_results())
    grid_df <- spatial_results()$grid
    
    create_spatial_map(
      data = grid_df,
      fill_var = "ExpectedRisk",
      title = "Estimated Spatial Risk",
      subtitle = "Expected total risk cost across South Africa",
      legend_title = "Expected\nRisk\nin Rands",
      palette_option = "magma",
      trans = "log10"
    )
  })
  
  output$prediction_risk_ci <- renderPlot({
    req(model_data())
    ci <- prediction_ci_data()
    level <- as.numeric(input$prediction_ci) * 100
    
    d <- ci$risk %>%
      dplyr::filter(inside) %>%
      dplyr::mutate(uncertainty = pmax((upper - lower) / median, .Machine$double.eps))
    
    create_spatial_map(
      data = d,
      fill_var = "uncertainty",
      title = paste0("Risk Uncertainty (", level, "% Credible Interval)"),
      subtitle = "Relative uncertainty ((Upper - Lower) / Median)",
      legend_title = "CI Width",
      palette_option = "magma",
      trans = "log10"
    )
  }, width = "auto", height = 600)
  
}


# =====================================================================================
# 6. RUN APPLICATION
# =====================================================================================

shinyApp(
  ui = ui,
  server = server
)