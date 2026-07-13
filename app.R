library(shiny)
library(leaflet)
library(sf)
library(dplyr)

# Source the metadata definition
source("metadata.R")

options(shiny.maxRequestSize = 30*1024^2)

cat("Loading spatial datasets...\n")
deptos <- readRDS("data/departamentos.rds")
municipios_nuevos <- readRDS("data/municipios_nuevos.rds")
distritos <- readRDS("data/distritos.rds")
zonas_productivas <- readRDS("data/zonas_productivas.rds")
uso_suelo <- readRDS("data/uso_potencial_suelo.rds")
reg_plan_nacion <- readRDS("data/regiones_plan_nacion.rds")
reg_pnodt <- readRDS("data/regiones_pnodt.rds")
subreg_pnodt <- readRDS("data/subregiones_pnodt.rds")
corredor_seco <- readRDS("data/corredor_seco.rds")
corredor_outline <- readRDS("data/corredor_seco_outline.rds")

# Extra scraped layers
clase_suelos <- readRDS("data/clase_suelos.rds")
humedad_relativa <- readRDS("data/humedad_relativa.rds")
temperatura <- readRDS("data/temperatura.rds")

# Crop potentials and precipitation layers
potencial_banano <- readRDS("data/potencial_banano.rds")
potencial_cana <- readRDS("data/potencial_cana.rds")
potencial_sorgo <- readRDS("data/potencial_sorgo.rds")
potencial_frijol <- readRDS("data/potencial_frijol.rds")
potencial_maiz <- readRDS("data/potencial_maiz.rds")
precipitacion <- readRDS("data/precipitacion.rds")

cuerpos_agua <- readRDS("data/cuerpos_agua.rds")
rios_importantes <- readRDS("data/rios_importantes.rds")
calles_principales <- readRDS("data/calles_principales.rds")
puertos <- readRDS("data/puertos.rds")
aeropuertos <- readRDS("data/aeropuertos.rds")
lineas_ferreas <- readRDS("data/lineas_ferreas.rds")
fronteras <- readRDS("data/fronteras.rds")
areas_urbanas <- readRDS("data/areas_urbanas.rds")
areas_rurales <- readRDS("data/areas_rurales.rds")
segmentos <- readRDS("data/segmentos.rds")
sectores <- readRDS("data/sectores.rds")
zonas_censales <- readRDS("data/zonas_censales.rds")

# Load metadata
zones_meta <- get_zones_metadata()

# Join metadata to spatial zones for easier rendering
zonas_productivas <- zonas_productivas %>%
  left_join(zones_meta, by = "zone")

# --- HIGH PERFORMANCE PRE-CALCULATIONS AT STARTUP ---
cat("Pre-calculating tabular statistics for instant panel updates...\n")

# Drop geometry from segments and other layers to make queries instant
segmentos_df <- st_drop_geometry(segmentos)
uso_suelo_df <- st_drop_geometry(uso_suelo)
reg_plan_nacion_df <- st_drop_geometry(reg_plan_nacion)
reg_pnodt_df <- st_drop_geometry(reg_pnodt)
subreg_pnodt_df <- st_drop_geometry(subreg_pnodt)
corredor_seco_df <- st_drop_geometry(corredor_seco)

# Flat dataframes for scraped layers
clase_suelos_df <- st_drop_geometry(clase_suelos)
humedad_relativa_df <- st_drop_geometry(humedad_relativa)
temperatura_df <- st_drop_geometry(temperatura)

# Flat dataframes for dissolved crop potentials and precipitation layers
potencial_banano_df <- st_drop_geometry(potencial_banano)
potencial_cana_df <- st_drop_geometry(potencial_cana)
potencial_sorgo_df <- st_drop_geometry(potencial_sorgo)
potencial_frijol_df <- st_drop_geometry(potencial_frijol)
potencial_maiz_df <- st_drop_geometry(potencial_maiz)
precipitacion_df <- st_drop_geometry(precipitacion)

# Group segments by depto and district (mpio_norm) to get district-level stats
distrito_census_stats <- segmentos_df %>%
  group_by(
    norm_depto = Depto_Norm,
    norm_mpio = Mpio_Norm
  ) %>%
  summarize(
    agri_households = sum(Actividad_Hogar_Agricola, na.rm = TRUE),
    population = sum(Poblacion, na.rm = TRUE),
    .groups = "drop"
  )

# Join these census stats to our distritos dataset (as attributes, no geometry)
distritos_df <- distritos %>%
  st_drop_geometry() %>%
  left_join(distrito_census_stats, by = c("norm_depto", "norm_mpio"))

# Pre-calculate zone stats
zone_stats <- distritos_df %>%
  group_by(zone) %>%
  summarize(
    districts_count = n(),
    districts_list = paste(MPIO, collapse = ", "),
    viviendas = sum(viviendas, na.rm = TRUE),
    agri_households = sum(agri_households, na.rm = TRUE),
    population = sum(population, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(zone))

# Pre-calculate new municipality stats
mpio_nuevo_stats <- distritos_df %>%
  group_by(mpio) %>%
  summarize(
    depto = paste(unique(DEPTO), collapse = ", "),
    districts_count = n(),
    districts_list = paste(MPIO, collapse = ", "),
    viviendas = sum(viviendas, na.rm = TRUE),
    agri_households = sum(agri_households, na.rm = TRUE),
    population = sum(population, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(mpio))

# Pre-calculate Zonas Censales statistics by aggregating segment-level data
segmentos_df <- segmentos_df %>%
  mutate(
    zone_ce_key = substr(SEG_ID, 1, 6),
    sector_key = substr(SEG_ID, 1, 7)
  )

zonas_ce_stats <- segmentos_df %>%
  group_by(zone_ce_key) %>%
  summarize(
    agri_households = sum(Actividad_Hogar_Agricola, na.rm = TRUE),
    population = sum(Poblacion, na.rm = TRUE),
    viviendas = sum(Viviendas, na.rm = TRUE),
    .groups = "drop"
  )

zonas_censales <- zonas_censales %>%
  mutate(zone_ce_key = paste0(COD_MUN4, COD_ZON_CE)) %>%
  left_join(zonas_ce_stats, by = "zone_ce_key")

zonas_censales_df <- st_drop_geometry(zonas_censales)

# Pre-calculate Sectores Censales statistics by aggregating segment-level data
sectores_stats <- segmentos_df %>%
  group_by(sector_key) %>%
  summarize(
    agri_households = sum(Actividad_Hogar_Agricola, na.rm = TRUE),
    population = sum(Poblacion, na.rm = TRUE),
    viviendas = sum(Viviendas, na.rm = TRUE),
    producers = sum(CONTEO_PRODUCTOR, na.rm = TRUE),
    .groups = "drop"
  )

sectores <- sectores %>%
  mutate(sector_key = COD_SEC_2) %>%
  left_join(sectores_stats, by = "sector_key")

sectores_df <- st_drop_geometry(sectores)

# Color palette for Zonas Productivas
zone_colors <- c(
  "#10b981", "#34d399", "#059669", "#047857", "#064e3b",
  "#0284c7", "#38bdf8", "#0369a1", "#075985", "#0c4a6e",
  "#ea580c", "#f97316", "#fb923c", "#c2410c", "#9a3412",
  "#8b5cf6", "#a78bfa", "#7c3aed", "#6d28d9", "#5b21b6",
  "#ec4899", "#f472b6", "#db2777", "#be185d", "#9d174d",
  "#eab308", "#facc15", "#ca8a04", "#a16207", "#854d0e",
  "#06b6d4", "#22d3ee", "#0891b2", "#0e7490", "#155e75"
)
pal_zones <- colorFactor(palette = zone_colors, domain = zones_meta$zone)

# Color palette for segments (Heatmap)
pal_seg <- colorNumeric(
  palette = "YlOrRd",
  domain = segmentos$Actividad_Hogar_Agricola,
  na.color = "transparent"
)

# Soil potential land use capability classes & colors (CENTA)
soil_classes <- c(
  "Clase I", "Clase II", "Clase III", "Clase IV", 
  "Clase V", "Clase VI", "Clase VII", "Clase VIII", 
  "Agua", "Urbana", "Pantano"
)
soil_colors <- c(
  "#15803d", "#22c55e", "#86efac", "#d9f99d",
  "#eab308", "#f97316", "#ea580c", "#b91c1c",
  "#0ea5e9", "#64748b", "#0d9488"
)
pal_soil <- colorFactor(palette = soil_colors, domain = soil_classes, na.color = "#475569")

# Clase de suelos (Geological Great Groups)
soil_type_names <- c(
  "Agua", "ANDISOLES", "LATOSOLES ARCILLOSOS ACIDOS", "LITOSOLES", 
  "ALUVIALES", "LATOSOLES ARCILLO ROJIZOS", "GRUMOSOLES", 
  "REGOSOLES Y HALOMORFICOS", "AREA URBANA"
)
soil_type_colors <- c(
  "#0ea5e9", "#7c2d12", "#c2410c", "#78716c", 
  "#d97706", "#ea580c", "#44403c", "#a21caf", "#64748b"
)
pal_soil_type <- colorFactor(palette = soil_type_colors, domain = soil_type_names, na.color = "#475569")

# Humedad relativa
humidity_ranges <- c("60-70", "70-75", "75-80", ">80")
humidity_colors <- c("#bae6fd", "#7dd3fc", "#38bdf8", "#0284c7")
pal_humidity <- colorFactor(palette = humidity_colors, domain = humidity_ranges, na.color = "#cbd5e1")

# Temperatura
temp_ranges <- c(
  "10.0 - 12.5°C", "12.5 - 15.0°C", "15.0 - 17.5°C", "17.5 - 20.0°C", 
  "20.0 - 22.5°C", "22.5 - 25.0°C", "25.0 - 27.5°C", "27.5 - 30.0°C", "?"
)
temp_colors <- c(
  "#1e3a8a", "#2563eb", "#3b82f6", "#60a5fa", 
  "#fef08a", "#fde047", "#f97316", "#dc2626", "#64748b"
)
pal_temp <- colorFactor(palette = temp_colors, domain = temp_ranges, na.color = "#cbd5e1")

# --- PALETTES FOR NEW CROP POTENTIALS AND PRECIPITATION ---
# Banano
pal_banano <- colorFactor(palette = c("#16a34a", "#f97316", "#eab308"), 
                          domain = c("Potencial alto", "Potencial bajo", "Potencial medio"), na.color = "#cbd5e1")
# Cana
pal_cana <- colorFactor(palette = c("#15803d", "#facc15", "#f97316", "#dc2626"), 
                        domain = c("Aptitud alta", "Aptitud media", "Aptitud baja", "No apto"), na.color = "#cbd5e1")
# Sorgo
pal_sorgo <- colorFactor(palette = c("#047857", "#ea580c", "#84cc16"), 
                         domain = c(" Potencial  Alto", " Potencial Bajo", " Potencial Medio"), na.color = "#cbd5e1")
# Frijol
pal_frijol <- colorFactor(palette = c("#059669", "#d97706", "#c2410c"), 
                          domain = c("Potencial Alto", "Potencial Medio", "Potencial Bajo"), na.color = "#cbd5e1")
# Maiz
pal_maiz <- colorFactor(palette = c("#10b981", "#ef4444", "#eab308"), 
                        domain = c(" Potencial  Alto", " Potencial  Bajo", " Potencial  Medio"), na.color = "#cbd5e1")
# Precipitacion
precip_levels <- c("1100-1400", "1400-1500", "1500-1600", "1600-1700", "1700-1800", 
                   "1800-1900", "1900-2000", "2000-2100", "2100-2500", "Mas de 2500")
precip_colors <- c("#bae6fd", "#7dd3fc", "#38bdf8", "#0ea5e9", "#0284c7", 
                    "#0369a1", "#075985", "#1e3a8a", "#172554", "#030712")
pal_precip <- colorFactor(palette = precip_colors, domain = precip_levels, na.color = "#cbd5e1")

# Planning and Regional palettes
pal_plan <- colorFactor(palette = "Set2", domain = reg_plan_nacion$REGION_PLA, na.color = "transparent")
pal_pnodt <- colorFactor(palette = "Set3", domain = reg_pnodt$REGION_PND, na.color = "transparent")
pal_subpnodt <- colorFactor(palette = "Paired", domain = subreg_pnodt$SUB_REGION, na.color = "transparent")

# Region centroids for zooming
region_views <- list(
  "Todo El Salvador" = list(lng = -88.89653, lat = 13.79418, zoom = 9),
  "Occidental"       = list(lng = -89.75000, lat = 13.90000, zoom = 10),
  "Central"          = list(lng = -89.25000, lat = 13.80000, zoom = 10),
  "Paracentral"      = list(lng = -88.85000, lat = 13.70000, zoom = 10),
  "Oriental"         = list(lng = -88.15000, lat = 13.60000, zoom = 10)
)

# UI Definition
ui <- fluidPage(
  title = "Zonas Agroproductivas de El Salvador",
  theme = NULL,
  
  # Inject stylesheet and load FontAwesome
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css")
  ),
  
  div(class = "app-container",
      
      # Sidebar Panel (left)
      div(class = "sidebar-panel",
          
          # Header
          div(class = "app-header",
              div(class = "app-title-wrapper",
                  span(class = "app-badge", "SIG-Agro"),
                  h1(class = "app-title", "Zonas Agroproductivas")
              ),
              p(class = "app-subtitle", "Propuesta de Zonas Agroproductivas para la Inversión"),
              p(class = "app-institution", "MAG - Oficina de Planeación Estratégica y Seguimiento OPES")
          ),
          
          # Region Selector
          div(class = "control-group",
              h3(class = "control-group-title", tags$i(class = "fa-solid fa-earth-americas"), "Zoom Regional"),
              selectInput("region_filter", NULL, 
                           choices = names(region_views), 
                           selected = "Todo El Salvador")
          ),
          
          # Base Maps
          div(class = "control-group",
              h3(class = "control-group-title", tags$i(class = "fa-solid fa-map"), "Mapa Base"),
              radioButtons("basemap", NULL,
                           choices = c("CartoDB Claro" = "light", 
                                       "Satélite (ESRI)" = "satellite", 
                                       "CartoDB Oscuro" = "dark"),
                           selected = "light")
          ),
          
          # Political & Regional Planning Layers
          div(class = "control-group",
              h3(class = "control-group-title", tags$i(class = "fa-solid fa-landmark"), "Límites Políticos y Regiones"),
              checkboxGroupInput("politicos_layers", NULL,
                                 choices = c("Departamentos" = "deptos", 
                                             "Nuevos Municipios (44)" = "mpios_nuevos", 
                                             "Distritos (262)" = "distritos",
                                             "Regiones Plan Nación" = "reg_plan_nacion",
                                             "Regiones PNODT" = "reg_pnodt",
                                             "Subregiones PNODT" = "subreg_pnodt"),
                                 selected = c("deptos"))
          ),
          
          # Agroproductive, Soils & Climate Layers
          div(class = "control-group",
              h3(class = "control-group-title", tags$i(class = "fa-solid fa-wheat-awn"), "Propuestas y Agroclima"),
              checkboxGroupInput("agro_layers", NULL,
                                 choices = c("Zonas Agroproductivas (PDF)" = "zonas", 
                                             "Mapa de Calor (Segmentos Agrícolas)" = "heatmap",
                                             "Uso Potencial del Suelo (CENTA)" = "uso_suelo",
                                             "Clase de Suelos (CENTA)" = "clase_suelos",
                                             "Corredor Seco (104 Distritos)" = "corredor_seco",
                                             "Temperatura Promedio (CENTA)" = "temperatura",
                                             "Humedad Relativa (CENTA)" = "humedad_relativa",
                                             "Precipitación Anual (CENTA)" = "precipitacion_capa",
                                             "Potencial de Maíz (CENTA)" = "pot_maiz",
                                             "Potencial de Frijol (CENTA)" = "pot_frijol",
                                             "Potencial de Sorgo (CENTA)" = "pot_sorgo",
                                             "Potencial de Caña (CENTA)" = "pot_cana",
                                             "Potencial de Banano (CENTA)" = "pot_banano"),
                                 selected = c("zonas"))
          ),
          
          # Infrastructure & Support Layers
          div(class = "control-group",
              h3(class = "control-group-title", tags$i(class = "fa-solid fa-road"), "Infraestructura y Apoyo"),
              checkboxGroupInput("support_layers", NULL,
                                 choices = c("Cuerpos de Agua" = "agua",
                                             "Ríos Importantes" = "rios",
                                             "Calles Principales" = "calles",
                                             "Puertos y Aeropuertos" = "puertos_aero",
                                             "Líneas Férreas" = "ferreas",
                                             "Fronteras" = "fronteras",
                                             "Áreas Urbanas" = "urbanas",
                                             "Áreas Rurales" = "rurales",
                                             "Sectores Censales" = "sectores",
                                             "Zonas Censales" = "zonas_censales",
                                             "Segmentos Censales" = "segmentos_outline"),
                                 selected = character(0))
          )
      ),
      
      # Map & Floating Panel Area (right)
      div(class = "map-container",
          # Value Boxes Row
          uiOutput("value_boxes"),
          
          # Map wrapper
          div(class = "map-wrapper",
              leafletOutput("map", width = "100%", height = "100%"),
              
              # Floating Info Panel
              uiOutput("info_panel")
          )
      )
  )
)

# Server Logic
server <- function(input, output, session) {
  
  # Reactive value to store the clicked object information
  clicked_info <- reactiveVal(list(type = "empty"))
  
  # Reactive expression to compute counts for the value boxes based on active region
  stats_data <- reactive({
    region <- input$region_filter
    
    if (region == "Todo El Salvador") {
      zones_subset <- zones_meta
    } else {
      zones_subset <- zones_meta %>% filter(region == !!region)
    }
    
    # Districts belonging to these zones
    dists_subset <- distritos_df %>% filter(zone %in% zones_subset$zone)
    
    list(
      zones_count = nrow(zones_subset),
      mpios_count = n_distinct(dists_subset$mpio, na.rm = TRUE),
      dists_count = nrow(dists_subset)
    )
  })
  
  # Render the value boxes dynamically
  output$value_boxes <- renderUI({
    stats <- stats_data()
    
    div(class = "value-boxes-container",
        # Box 1: Zonas
        div(class = "value-box",
            div(class = "value-box-icon", tags$i(class = "fa-solid fa-wheat-awn")),
            div(class = "value-box-content",
                div(class = "value-box-number", stats$zones_count),
                div(class = "value-box-label", "Zonas Agroproductivas")
            )
        ),
        # Box 2: Nuevos Municipios
        div(class = "value-box",
            div(class = "value-box-icon blue", tags$i(class = "fa-solid fa-landmark")),
            div(class = "value-box-content",
                div(class = "value-box-number", stats$mpios_count),
                div(class = "value-box-label", "Nuevos Municipios")
            )
        ),
        # Box 3: Distritos
        div(class = "value-box",
            div(class = "value-box-icon orange", tags$i(class = "fa-solid fa-map-pin")),
            div(class = "value-box-content",
                div(class = "value-box-number", stats$dists_count),
                div(class = "value-box-label", "Distritos Integrados")
            )
        )
    )
  })
  
  # Initialize the base map
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
      setView(lng = -88.89653, lat = 13.79418, zoom = 9) %>%
      addProviderTiles(providers$CartoDB.Positron, layerId = "base_tile")
  })
  
  # Observe regional zoom updates
  observeEvent(input$region_filter, {
    view <- region_views[[input$region_filter]]
    leafletProxy("map") %>%
      setView(lng = view$lng, lat = view$lat, zoom = view$zoom)
  })
  
  # Observe basemap selection
  observeEvent(input$basemap, {
    proxy <- leafletProxy("map")
    proxy %>% removeTiles(layerId = "base_tile")
    
    if (input$basemap == "dark") {
      proxy %>% addProviderTiles(providers$CartoDB.DarkMatter, layerId = "base_tile")
    } else if (input$basemap == "satellite") {
      proxy %>% addProviderTiles(providers$Esri.WorldImagery, layerId = "base_tile")
    } else {
      proxy %>% addProviderTiles(providers$CartoDB.Positron, layerId = "base_tile")
    }
  })
  
  # --- SEPARATED OBSERVERS FOR OPTIMIZED LAYER RENDERINGS ---
  
  # 1. Observe Political & Regional Planning Layers
  observeEvent(input$politicos_layers, {
    layers <- input$politicos_layers
    proxy <- leafletProxy("map")
    
    # Departamentos
    if ("deptos" %in% layers) {
      proxy %>%
        addPolygons(data = deptos, color = "#475569", weight = 1.5, fillColor = "transparent",
                    label = ~DEPTO,
                    highlightOptions = highlightOptions(color = "#10b981", weight = 2.5, bringToFront = TRUE),
                    layerId = ~paste0("depto_", DEPTO),
                    group = "deptos")
    } else {
      proxy %>% clearGroup("deptos")
    }
    
    # Nuevos Municipios (44)
    if ("mpios_nuevos" %in% layers) {
      proxy %>%
        addPolygons(data = municipios_nuevos, color = "#64748b", weight = 1.2, fillColor = "transparent",
                    label = ~paste0(mpio, " (", depto, ")"),
                    highlightOptions = highlightOptions(color = "#10b981", weight = 2, bringToFront = TRUE),
                    layerId = ~paste0("mpionuevo_", mpio),
                    group = "mpios_nuevos")
    } else {
      proxy %>% clearGroup("mpios_nuevos")
    }
    
    # Distritos (262)
    if ("distritos" %in% layers) {
      proxy %>%
        addPolygons(data = distritos, color = "#94a3b8", weight = 0.8, fillColor = "transparent",
                    label = ~paste0(MPIO, " (", DEPTO, ")"),
                    highlightOptions = highlightOptions(color = "#10b981", weight = 2, bringToFront = TRUE),
                    layerId = ~paste0("distrito_", MPIO, "_", DEPTO),
                    group = "distritos")
    } else {
      proxy %>% clearGroup("distritos")
    }
    
    # Regiones Plan Nación
    if ("reg_plan_nacion" %in% layers) {
      # Add temporary human-readable label column
      reg_plan_nacion_lbl <- reg_plan_nacion %>%
        mutate(nom_reg = sapply(REGION_PLA, function(r) {
          if (is.na(r)) "Zona Protegida / Sin Definir"
          else switch(as.character(r),
                      "1" = "Región I - Occidental",
                      "2" = "Región II - Central / Metropolitana",
                      "3" = "Región III - Paracentral",
                      "4" = "Región IV - Central Norte",
                      "5" = "Región V - Oriental",
                      paste0("Región ", r))
        }))
      
      proxy %>%
        addPolygons(data = reg_plan_nacion_lbl,
                    fillColor = ~pal_plan(REGION_PLA),
                    fillOpacity = 0.2,
                    color = "#3b82f6",
                    weight = 1.8,
                    label = ~nom_reg,
                    highlightOptions = highlightOptions(fillOpacity = 0.35, color = "#ffffff", weight = 2.5, bringToFront = TRUE),
                    layerId = ~paste0("plan_", OBJECTID),
                    group = "reg_plan_nacion")
    } else {
      proxy %>% clearGroup("reg_plan_nacion")
    }
    
    # Regiones PNODT
    if ("reg_pnodt" %in% layers) {
      proxy %>%
        addPolygons(data = reg_pnodt,
                    fillColor = ~pal_pnodt(REGION_PND),
                    fillOpacity = 0.2,
                    color = "#8b5cf6",
                    weight = 1.8,
                    label = ~paste0("Región PNODT: ", REGION_PND),
                    highlightOptions = highlightOptions(fillOpacity = 0.35, color = "#ffffff", weight = 2.5, bringToFront = TRUE),
                    layerId = ~paste0("pnodt_", OBJECTID),
                    group = "reg_pnodt")
    } else {
      proxy %>% clearGroup("reg_pnodt")
    }
    
    # Subregiones PNODT
    if ("subreg_pnodt" %in% layers) {
      proxy %>%
        addPolygons(data = subreg_pnodt,
                    fillColor = ~pal_subpnodt(SUB_REGION),
                    fillOpacity = 0.15,
                    color = "#ec4899",
                    weight = 1.2,
                    label = ~paste0("Subregión PNODT: ", SUB_REGION),
                    highlightOptions = highlightOptions(fillOpacity = 0.3, color = "#ffffff", weight = 2, bringToFront = TRUE),
                    layerId = ~paste0("subpnodt_", OBJECTID),
                    group = "subreg_pnodt")
    } else {
      proxy %>% clearGroup("subreg_pnodt")
    }
  }, ignoreNULL = FALSE)
  
  # 2. Observe Agroproductive, Soils & Climate Layers
  observeEvent(input$agro_layers, {
    layers <- input$agro_layers
    proxy <- leafletProxy("map")
    
    # Propuesta Zonas Agroproductivas (PDF)
    if ("zonas" %in% layers) {
      proxy %>%
        addPolygons(data = zonas_productivas,
                    fillColor = ~pal_zones(zone),
                    fillOpacity = 0.5,
                    color = "#10b981",
                    weight = 1.5,
                    label = ~zone,
                    highlightOptions = highlightOptions(fillOpacity = 0.75, color = "#ffffff", weight = 2, bringToFront = TRUE),
                    layerId = ~paste0("zona_", zone),
                    group = "zonas")
    } else {
      proxy %>% clearGroup("zonas")
    }
    
    # Heatmap based on Segmentos (Choropleth)
    if ("heatmap" %in% layers) {
      proxy %>%
        addPolygons(data = segmentos,
                    fillColor = ~pal_seg(Actividad_Hogar_Agricola),
                    fillOpacity = 0.65,
                    stroke = FALSE,
                    smoothFactor = 1.0,
                    label = ~paste0("Cantón: ", CANTON, " (Hogares Agrícolas: ", Actividad_Hogar_Agricola, ")"),
                    layerId = ~paste0("seg_", SEG_ID),
                    group = "heatmap")
    } else {
      proxy %>% clearGroup("heatmap")
    }
    
    # Uso Potencial del Suelo (CENTA)
    if ("uso_suelo" %in% layers) {
      proxy %>%
        addPolygons(data = uso_suelo,
                    fillColor = ~pal_soil(Clase),
                    fillOpacity = 0.55,
                    color = "#1e293b",
                    weight = 0.8,
                    smoothFactor = 1.0,
                    label = ~paste0("Clase: ", Clase, " (", Municipio, ", ", Departamento, ")"),
                    highlightOptions = highlightOptions(fillOpacity = 0.75, color = "#ffffff", weight = 1.5, bringToFront = TRUE),
                    layerId = ~paste0("soil_", OBJECTID),
                    group = "uso_suelo")
    } else {
      proxy %>% clearGroup("uso_suelo")
    }
    
    # Clase de Suelos (CENTA - Gran Grupo)
    if ("clase_suelos" %in% layers) {
      proxy %>%
        addPolygons(data = clase_suelos,
                    fillColor = ~pal_soil_type(NOMBRE),
                    fillOpacity = 0.55,
                    color = "#1e293b",
                    weight = 0.8,
                    smoothFactor = 1.0,
                    label = ~paste0("Tipo: ", NOMBRE, " (", Municipio, ", ", Departamento, ")"),
                    highlightOptions = highlightOptions(fillOpacity = 0.75, color = "#ffffff", weight = 1.5, bringToFront = TRUE),
                    layerId = ~paste0("clasesuelo_", OBJECTID),
                    group = "clase_suelos")
    } else {
      proxy %>% clearGroup("clase_suelos")
    }
    
    # Corredor Seco (104 Distritos)
    if ("corredor_seco" %in% layers) {
      proxy %>%
        addPolygons(data = corredor_seco,
                    fillColor = "#d97706",
                    fillOpacity = 0.35,
                    color = "#ea580c",
                    weight = 1.2,
                    label = ~paste0("Distrito en Corredor Seco: ", MPIO, " (", DEPTO, ")"),
                    highlightOptions = highlightOptions(fillOpacity = 0.55, color = "#ffffff", weight = 2, bringToFront = TRUE),
                    layerId = ~paste0("corredor_", COD_MUN4),
                    group = "corredor_seco") %>%
        # Overlay outline around the dry corridor
        addPolylines(data = corredor_outline,
                     color = "#ea580c",
                     weight = 2.5,
                     dashArray = "4, 8",
                     group = "corredor_seco")
    } else {
      proxy %>% clearGroup("corredor_seco")
    }
    
    # Temperatura Promedio (CENTA)
    if ("temperatura" %in% layers) {
      proxy %>%
        addPolygons(data = temperatura,
                    fillColor = ~pal_temp(TEMPERATURA),
                    fillOpacity = 0.6,
                    color = "#b91c1c",
                    weight = 0.5,
                    smoothFactor = 1.0,
                    label = ~paste0("Temp: ", TEMPERATURA, " (", Municipio, ")"),
                    highlightOptions = highlightOptions(fillOpacity = 0.8, color = "#ffffff", weight = 1.5, bringToFront = TRUE),
                    layerId = ~paste0("temp_", OBJECTID),
                    group = "temperatura")
    } else {
      proxy %>% clearGroup("temperatura")
    }
    
    # Humedad Relativa (CENTA)
    if ("humedad_relativa" %in% layers) {
      proxy %>%
        addPolygons(data = humedad_relativa,
                    fillColor = ~pal_humidity(Valor),
                    fillOpacity = 0.6,
                    color = "#0284c7",
                    weight = 0.5,
                    smoothFactor = 1.0,
                    label = ~paste0("Humedad: ", Valor, "% (", Interpretacion, " - ", Municipio, ")"),
                    highlightOptions = highlightOptions(fillOpacity = 0.8, color = "#ffffff", weight = 1.5, bringToFront = TRUE),
                    layerId = ~paste0("humedad_", OBJECTID),
                    group = "humedad_relativa")
    } else {
      proxy %>% clearGroup("humedad_relativa")
    }
    
    # Precipitación Anual (CENTA)
    if ("precipitacion_capa" %in% layers) {
      proxy %>%
        addPolygons(data = precipitacion,
                    fillColor = ~pal_precip(ClassVal),
                    fillOpacity = 0.55,
                    color = "#1d4ed8",
                    weight = 0.8,
                    smoothFactor = 1.0,
                    label = ~paste0("Lluvia: ", ClassVal, " mm/año"),
                    highlightOptions = highlightOptions(fillOpacity = 0.75, color = "#ffffff", weight = 1.5, bringToFront = TRUE),
                    layerId = ~paste0("precip_", ClassVal),
                    group = "precipitacion_capa")
    } else {
      proxy %>% clearGroup("precipitacion_capa")
    }
    
    # Potencial Maíz (CENTA)
    if ("pot_maiz" %in% layers) {
      proxy %>%
        addPolygons(data = potencial_maiz,
                    fillColor = ~pal_maiz(ClassVal),
                    fillOpacity = 0.55,
                    color = "#1e293b",
                    weight = 0.8,
                    smoothFactor = 1.0,
                    label = ~paste0("Maíz: ", trimws(ClassVal)),
                    highlightOptions = highlightOptions(fillOpacity = 0.75, color = "#ffffff", weight = 1.5, bringToFront = TRUE),
                    layerId = ~paste0("potmaiz_", ClassVal),
                    group = "pot_maiz")
    } else {
      proxy %>% clearGroup("pot_maiz")
    }
    
    # Potencial Frijol (CENTA)
    if ("pot_frijol" %in% layers) {
      proxy %>%
        addPolygons(data = potencial_frijol,
                    fillColor = ~pal_frijol(ClassVal),
                    fillOpacity = 0.55,
                    color = "#1e293b",
                    weight = 0.8,
                    smoothFactor = 1.0,
                    label = ~paste0("Frijol: ", trimws(ClassVal)),
                    highlightOptions = highlightOptions(fillOpacity = 0.75, color = "#ffffff", weight = 1.5, bringToFront = TRUE),
                    layerId = ~paste0("potfrijol_", ClassVal),
                    group = "pot_frijol")
    } else {
      proxy %>% clearGroup("pot_frijol")
    }
    
    # Potencial Sorgo (CENTA)
    if ("pot_sorgo" %in% layers) {
      proxy %>%
        addPolygons(data = potencial_sorgo,
                    fillColor = ~pal_sorgo(ClassVal),
                    fillOpacity = 0.55,
                    color = "#1e293b",
                    weight = 0.8,
                    smoothFactor = 1.0,
                    label = ~paste0("Sorgo: ", trimws(ClassVal)),
                    highlightOptions = highlightOptions(fillOpacity = 0.75, color = "#ffffff", weight = 1.5, bringToFront = TRUE),
                    layerId = ~paste0("potsorgo_", ClassVal),
                    group = "pot_sorgo")
    } else {
      proxy %>% clearGroup("pot_sorgo")
    }
    
    # Potencial Caña (CENTA)
    if ("pot_cana" %in% layers) {
      proxy %>%
        addPolygons(data = potencial_cana,
                    fillColor = ~pal_cana(ClassVal),
                    fillOpacity = 0.55,
                    color = "#1e293b",
                    weight = 0.8,
                    smoothFactor = 1.0,
                    label = ~paste0("Caña: ", ClassVal),
                    highlightOptions = highlightOptions(fillOpacity = 0.75, color = "#ffffff", weight = 1.5, bringToFront = TRUE),
                    layerId = ~paste0("potcana_", ClassVal),
                    group = "pot_cana")
    } else {
      proxy %>% clearGroup("pot_cana")
    }
    
    # Potencial Banano (CENTA)
    if ("pot_banano" %in% layers) {
      proxy %>%
        addPolygons(data = potencial_banano,
                    fillColor = ~pal_banano(ClassVal),
                    fillOpacity = 0.55,
                    color = "#1e293b",
                    weight = 0.8,
                    smoothFactor = 1.0,
                    label = ~paste0("Banano/Plátano: ", ClassVal),
                    highlightOptions = highlightOptions(fillOpacity = 0.75, color = "#ffffff", weight = 1.5, bringToFront = TRUE),
                    layerId = ~paste0("potbanano_", ClassVal),
                    group = "pot_banano")
    } else {
      proxy %>% clearGroup("pot_banano")
    }
  }, ignoreNULL = FALSE)
  
  # 3. Observe Support & Infrastructure Layers
  observeEvent(input$support_layers, {
    layers <- input$support_layers
    proxy <- leafletProxy("map")
    
    # Cuerpos de agua
    if ("agua" %in% layers) {
      proxy %>%
        addPolygons(data = cuerpos_agua, color = "#0284c7", weight = 1, fillOpacity = 0.6,
                    popup = ~paste0("<strong>Cuerpo de Agua:</strong> ", ifelse(is.na(NOMBRE), "Sin nombre", NOMBRE)),
                    group = "agua")
    } else {
      proxy %>% clearGroup("agua")
    }
    
    # Ríos importantes
    if ("rios" %in% layers) {
      proxy %>%
        addPolylines(data = rios_importantes, color = "#0ea5e9", weight = 2, opacity = 0.8,
                     popup = ~paste0("<strong>Río Importante:</strong> ", NOMBRE),
                     group = "rios")
    } else {
      proxy %>% clearGroup("rios")
    }
    
    # Calles principales
    if ("calles" %in% layers) {
      proxy %>%
        addPolylines(data = calles_principales, color = "#475569", weight = 1.5, opacity = 0.7,
                     popup = ~paste0("<strong>Calle Principal:</strong> ", NOMBRE),
                     group = "calles")
    } else {
      proxy %>% clearGroup("calles")
    }
    
    # Líneas férreas
    if ("ferreas" %in% layers) {
      proxy %>%
        addPolylines(data = lineas_ferreas, color = "#d97706", weight = 1.5, opacity = 0.6,
                     dashArray = "5, 5",
                     popup = "<strong>Línea Férrea</strong>",
                     group = "ferreas")
    } else {
      proxy %>% clearGroup("ferreas")
    }
    
    # Áreas Urbanas
    if ("urbanas" %in% layers) {
      proxy %>%
        addPolygons(data = areas_urbanas, color = "#ef4444", weight = 1, fillColor = "#f87171", fillOpacity = 0.3,
                    group = "urbanas")
    } else {
      proxy %>% clearGroup("urbanas")
    }
    
    # Áreas Rurales
    if ("rurales" %in% layers) {
      proxy %>%
        addPolygons(data = areas_rurales, color = "#84cc16", weight = 0.5, fillColor = "#a3e635", fillOpacity = 0.1,
                    group = "rurales")
    } else {
      proxy %>% clearGroup("rurales")
    }
    
    # Sectores Censales
    if ("sectores" %in% layers) {
      proxy %>%
        addPolygons(data = sectores, color = "#3b82f6", weight = 0.8, fillColor = "#60a5fa", fillOpacity = 0.1,
                    group = "sectores")
    } else {
      proxy %>% clearGroup("sectores")
    }
    
    # Zonas Censales
    if ("zonas_censales" %in% layers) {
      proxy %>%
        addPolygons(data = zonas_censales, color = "#8b5cf6", weight = 1, fillColor = "#c084fc", fillOpacity = 0.15,
                    group = "zonas_censales")
    } else {
      proxy %>% clearGroup("zonas_censales")
    }
    
    # Segmentos Censales outline
    if ("segmentos_outline" %in% layers) {
      proxy %>%
        addPolygons(data = segmentos, color = "#10b981", weight = 0.5, fillColor = "transparent",
                    popup = ~paste0("<strong>Segmento:</strong> ", SEG_ID),
                    group = "segmentos_outline")
    } else {
      proxy %>% clearGroup("segmentos_outline")
    }
    
    # Puertos y Aeropuertos
    if ("puertos_aero" %in% layers) {
      proxy %>%
        addCircleMarkers(data = puertos, radius = 6, color = "#ec4899", fillColor = "#f472b6",
                         fillOpacity = 0.9, weight = 1.5,
                         popup = ~paste0("<strong>Puerto:</strong> ", NOMBRE),
                         group = "puertos_aero") %>%
        addCircleMarkers(data = aeropuertos, radius = 6, color = "#06b6d4", fillColor = "#22d3ee",
                         fillOpacity = 0.9, weight = 1.5,
                         popup = ~paste0("<strong>Aeropuerto:</strong> ", NOMBRE),
                         group = "puertos_aero")
    } else {
      proxy %>% clearGroup("puertos_aero")
    }
    
    # Fronteras
    if ("fronteras" %in% layers) {
      proxy %>%
        addCircleMarkers(data = fronteras, radius = 5, color = "#f43f5e", fillColor = "#fda4af",
                         fillOpacity = 0.9, weight = 1.5,
                         popup = ~paste0("<strong>Frontera Terrestre:</strong> ", NOMBRE),
                         group = "fronteras")
    } else {
      proxy %>% clearGroup("fronteras")
    }
  }, ignoreNULL = FALSE)
  
  # --- INSTANT CLICK HANDLER WITH PRE-CALCULATED LOOKUPS ---
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    click_id <- click$id
    
    if (is.null(click_id)) return()
    
    # Case 1: Clicked on a Zona Productiva
    if (startsWith(click_id, "zona_")) {
      zone_name <- sub("zona_", "", click_id)
      
      meta <- zones_meta %>% filter(zone == zone_name)
      stats <- zone_stats %>% filter(zone == zone_name)
      
      clicked_info(list(
        type = "zona",
        name = zone_name,
        region = meta$region,
        crops = meta$crops,
        competitiveness = meta$competitiveness,
        districts_count = stats$districts_count,
        districts_list = stats$districts_list,
        viviendas = stats$viviendas,
        agri_households = stats$agri_households,
        population = stats$population
      ))
    }
    # Case 2: Clicked on a New Municipality (44)
    else if (startsWith(click_id, "mpionuevo_")) {
      mpio_name <- sub("mpionuevo_", "", click_id)
      stats <- mpio_nuevo_stats %>% filter(mpio == mpio_name)
      
      clicked_info(list(
        type = "municipio_nuevo",
        name = mpio_name,
        depto = stats$depto,
        districts_count = stats$districts_count,
        districts_list = stats$districts_list,
        viviendas = stats$viviendas,
        agri_households = stats$agri_households,
        population = stats$population
      ))
    }
    # Case 3: Clicked on a District (262)
    else if (startsWith(click_id, "distrito_")) {
      id_parts <- strsplit(sub("distrito_", "", click_id), "_")[[1]]
      dist_name <- id_parts[1]
      depto_name <- id_parts[2]
      
      stats <- distritos_df %>% filter(MPIO == dist_name, DEPTO == depto_name)
      
      clicked_info(list(
        type = "distrito",
        name = dist_name,
        depto = depto_name,
        mpio_nuevo = stats$mpio,
        zone = ifelse(is.na(stats$zone), "Ninguna (Fuera de propuesta)", stats$zone),
        viviendas = stats$viviendas,
        agri_households = stats$agri_households,
        population = stats$population
      ))
    }
    # Case 4: Clicked on a Segment (Heatmap)
    else if (startsWith(click_id, "seg_")) {
      seg_id_val <- sub("seg_", "", click_id)
      seg_row <- segmentos_df %>% filter(SEG_ID == seg_id_val)
      
      clicked_info(list(
        type = "segmento",
        id = seg_id_val,
        canton = seg_row$CANTON,
        depto = seg_row$Depto_Norm,
        mpio = seg_row$Mpio_Norm,
        viviendas = seg_row$Viviendas,
        agri_households = seg_row$Actividad_Hogar_Agricola,
        population = seg_row$Poblacion,
        poverty = seg_row$Pobreza_Carencia,
        producers = seg_row$CONTEO_PRODUCTOR
      ))
    }
    # Case 5: Clicked on a Soil Potential land capability polygon (CENTA)
    else if (startsWith(click_id, "soil_")) {
      soil_id_val <- as.integer(sub("soil_", "", click_id))
      soil_row <- uso_suelo_df %>% filter(OBJECTID == soil_id_val)
      
      clicked_info(list(
        type = "uso_suelo",
        clase = soil_row$Clase,
        depto = soil_row$Departamento,
        mpio = soil_row$Municipio,
        km2 = soil_row$KM2,
        ha = soil_row$HA,
        mz = soil_row$MZ
      ))
    }
    # Case 6: Clicked on a Región Plan Nación
    else if (startsWith(click_id, "plan_")) {
      plan_id_val <- as.integer(sub("plan_", "", click_id))
      row <- reg_plan_nacion_df %>% filter(OBJECTID == plan_id_val)
      r <- row$REGION_PLA
      
      readable_name <- if (is.na(r)) "Zona Protegida / Sin Definir"
                      else switch(as.character(r),
                                  "1" = "Región I - Occidental",
                                  "2" = "Región II - Central / Metropolitana",
                                  "3" = "Región III - Paracentral",
                                  "4" = "Región IV - Central Norte",
                                  "5" = "Región V - Oriental",
                                  paste0("Región ", r))
      
      clicked_info(list(
        type = "plan_nacion",
        name = readable_name,
        viviendas = row$viviendas,
        personas = row$personas,
        hogares = row$hogares,
        serv_elect = row$serv_elect,
        serv_agua = row$serv_agua,
        serv_telef = row$serv_telef,
        computador = row$computador
      ))
    }
    # Case 7: Clicked on a Región PNODT
    else if (startsWith(click_id, "pnodt_")) {
      pnodt_id_val <- as.integer(sub("pnodt_", "", click_id))
      row <- reg_pnodt_df %>% filter(OBJECTID == pnodt_id_val)
      
      clicked_info(list(
        type = "pnodt",
        name = paste0("Región PNODT: ", row$REGION_PND),
        id = row$REGION_ID,
        viviendas = row$viviendas,
        personas = row$personas,
        hogares = row$hogares,
        serv_elect = row$serv_elect,
        serv_agua = row$serv_agua,
        serv_telef = row$serv_telef,
        computador = row$computador
      ))
    }
    # Case 8: Clicked on a Subregión PNODT
    else if (startsWith(click_id, "subpnodt_")) {
      subpnodt_id_val <- as.integer(sub("subpnodt_", "", click_id))
      row <- subreg_pnodt_df %>% filter(OBJECTID == subpnodt_id_val)
      
      clicked_info(list(
        type = "subpnodt",
        name = paste0("Subregión PNODT: ", row$SUB_REGION),
        viviendas = row$viviendas,
        personas = row$personas,
        hogares = row$hogares,
        serv_elect = row$serv_elect,
        serv_agua = row$serv_agua,
        serv_telef = row$serv_telef,
        computador = row$computador
      ))
    }
    # Case 9: Clicked on a Corredor Seco District
    else if (startsWith(click_id, "corredor_")) {
      mun_code <- sub("corredor_", "", click_id)
      stats <- distritos_df %>% filter(COD_MUN4 == mun_code)
      
      clicked_info(list(
        type = "corredor_seco_dist",
        name = stats$MPIO,
        depto = stats$DEPTO,
        mpio_nuevo = stats$mpio,
        zone = ifelse(is.na(stats$zone), "Ninguna (Fuera de propuesta)", stats$zone),
        viviendas = stats$viviendas,
        agri_households = stats$agri_households,
        population = stats$population
      ))
    }
    # Case 10: Clicked on a Clase de suelos polygon (CENTA)
    else if (startsWith(click_id, "clasesuelo_")) {
      soil_id_val <- as.integer(sub("clasesuelo_", "", click_id))
      soil_row <- clase_suelos_df %>% filter(OBJECTID == soil_id_val)
      
      clicked_info(list(
        type = "clase_suelo",
        nombre = soil_row$NOMBRE,
        codigo = soil_row$CODIGO,
        depto = soil_row$Departamento,
        mpio = soil_row$Municipio,
        km2 = soil_row$KM2,
        ha = soil_row$HA,
        mz = soil_row$MZ
      ))
    }
    # Case 11: Clicked on a Humedad Relativa polygon (CENTA)
    else if (startsWith(click_id, "humedad_")) {
      hum_id_val <- as.integer(sub("humedad_", "", click_id))
      hum_row <- humedad_relativa_df %>% filter(OBJECTID == hum_id_val)
      
      clicked_info(list(
        type = "humedad",
        valor = hum_row$Valor,
        interpretacion = hum_row$Interpretacion,
        depto = hum_row$Departamento,
        mpio = hum_row$Municipio,
        km2 = hum_row$KM2,
        ha = hum_row$HA,
        mz = hum_row$MZ
      ))
    }
    # Case 12: Clicked on a Temperatura polygon (CENTA)
    else if (startsWith(click_id, "temp_")) {
      temp_id_val <- as.integer(sub("temp_", "", click_id))
      temp_row <- temperatura_df %>% filter(OBJECTID == temp_id_val)
      
      clicked_info(list(
        type = "temperatura_clima",
        rango = temp_row$TEMPERATURA,
        depto = temp_row$Departamento,
        mpio = temp_row$Municipio,
        km2 = temp_row$KM2,
        ha = temp_row$HA,
        mz = temp_row$MZ
      ))
    }
    # Case 13: Clicked on a Precipitación polygon (CENTA)
    else if (startsWith(click_id, "precip_")) {
      class_val <- sub("precip_", "", click_id)
      row <- precipitacion_df %>% filter(ClassVal == class_val)
      
      clicked_info(list(
        type = "precipitacion_clima",
        valor = class_val,
        km2 = row$KM2,
        ha = row$HA,
        mz = row$MZ
      ))
    }
    # Case 14: Clicked on a Potencial Maíz polygon (CENTA)
    else if (startsWith(click_id, "potmaiz_")) {
      class_val <- sub("potmaiz_", "", click_id)
      row <- potencial_maiz_df %>% filter(ClassVal == class_val)
      
      clicked_info(list(
        type = "potencial_cultivo",
        crop = "Maíz",
        valor = trimws(class_val),
        km2 = row$KM2,
        ha = row$HA,
        mz = row$MZ
      ))
    }
    # Case 15: Clicked on a Potencial Frijol polygon (CENTA)
    else if (startsWith(click_id, "potfrijol_")) {
      class_val <- sub("potfrijol_", "", click_id)
      row <- potencial_frijol_df %>% filter(ClassVal == class_val)
      
      clicked_info(list(
        type = "potencial_cultivo",
        crop = "Frijol",
        valor = trimws(class_val),
        km2 = row$KM2,
        ha = row$HA,
        mz = row$MZ
      ))
    }
    # Case 16: Clicked on a Potencial Sorgo polygon (CENTA)
    else if (startsWith(click_id, "potsorgo_")) {
      class_val <- sub("potsorgo_", "", click_id)
      row <- potencial_sorgo_df %>% filter(ClassVal == class_val)
      
      clicked_info(list(
        type = "potencial_cultivo",
        crop = "Sorgo",
        valor = trimws(class_val),
        km2 = row$KM2,
        ha = row$HA,
        mz = row$MZ
      ))
    }
    # Case 17: Clicked on a Potencial Caña polygon (CENTA)
    else if (startsWith(click_id, "potcana_")) {
      class_val <- sub("potcana_", "", click_id)
      row <- potencial_cana_df %>% filter(ClassVal == class_val)
      
      clicked_info(list(
        type = "potencial_cultivo",
        crop = "Caña de Azúcar",
        valor = class_val,
        km2 = row$KM2,
        ha = row$HA,
        mz = row$MZ
      ))
    }
    # Case 18: Clicked on a Potencial Banano polygon (CENTA)
    else if (startsWith(click_id, "potbanano_")) {
      class_val <- sub("potbanano_", "", click_id)
      row <- potencial_banano_df %>% filter(ClassVal == class_val)
      
      clicked_info(list(
        type = "potencial_cultivo",
        crop = "Banano y Musáceas",
        valor = class_val,
        km2 = row$KM2,
        ha = row$HA,
        mz = row$MZ
      ))
    }
  })
  
  # Render the floating info panel dynamically
  output$info_panel <- renderUI({
    info <- clicked_info()
    
    if (info$type == "empty") {
      return(
        div(class = "floating-info-panel",
            div(class = "floating-panel-header",
                h4(class = "floating-panel-title", "Detalle de Selección")
            ),
            div(class = "info-empty-state",
                tags$i(class = "fa-solid fa-hand-pointer"),
                p("Haz clic en cualquier zona, municipio, distrito, región o polígono de suelo en el mapa para visualizar sus características productivas y estadísticas.")
            )
        )
      )
    }
    
    # If clicked a Zone
    if (info$type == "zona") {
      crop_tags <- unlist(strsplit(info$crops, ", "))
      tags_html <- lapply(crop_tags, function(t) {
        span(class = "detail-tag", t)
      })
      
      div(class = "floating-info-panel",
          div(class = "floating-panel-header",
              h4(class = "floating-panel-title", info$name),
              span(class = "app-badge", info$region)
          ),
          
          div(class = "detail-section",
              div(class = "detail-label", "Rubros Potenciales de Inversión"),
              div(class = "detail-tag-list", tags_html)
          ),
          
          div(class = "detail-section",
              div(class = "detail-label", "Factores de Competitividad"),
              div(class = "detail-value", info$competitiveness)
          ),
          
          div(class = "detail-section",
              div(class = "detail-label", "Territorios que abarca"),
              div(class = "detail-value", info$districts_list)
          ),
          
          div(class = "stats-grid",
              div(class = "stat-card",
                  div(class = "stat-value", format(info$districts_count, big.mark = ",")),
                  div(class = "stat-label", "Distritos")
              ),
              div(class = "stat-card",
                  div(class = "stat-value", format(info$agri_households, big.mark = ",")),
                  div(class = "stat-label", "Hogares Agrícolas")
              ),
              div(class = "stat-card",
                  div(class = "stat-value", format(info$viviendas, big.mark = ",")),
                  div(class = "stat-label", "Viviendas")
              ),
              div(class = "stat-card",
                  div(class = "stat-value", format(info$population, big.mark = ",")),
                  div(class = "stat-label", "Población")
              )
          )
      )
    }
    # If clicked a New Municipality (44)
    else if (info$type == "municipio_nuevo") {
      div(class = "floating-info-panel",
          div(class = "floating-panel-header",
              h4(class = "floating-panel-title", info$name),
              span(class = "app-badge", "Nuevo Municipio")
          ),
          
          div(class = "detail-section",
              div(class = "detail-label", "Departamento"),
              div(class = "detail-value", info$depto)
          ),
          
          div(class = "detail-section",
              div(class = "detail-label", "Distritos Integrados"),
              div(class = "detail-value", info$districts_list)
          ),
          
          div(class = "stats-grid",
              div(class = "stat-card",
                  div(class = "stat-value", format(info$districts_count, big.mark = ",")),
                  div(class = "stat-label", "Distritos")
              ),
              div(class = "stat-card",
                  div(class = "stat-value", format(info$agri_households, big.mark = ",")),
                  div(class = "stat-label", "Hogares Agrícolas")
              ),
              div(class = "stat-card",
                  div(class = "stat-value", format(info$viviendas, big.mark = ",")),
                  div(class = "stat-label", "Viviendas")
              ),
              div(class = "stat-card",
                  div(class = "stat-value", format(info$population, big.mark = ",")),
                  div(class = "stat-label", "Población")
              )
          )
      )
    }
    # If clicked a District (262)
    else if (info$type == "distrito") {
      div(class = "floating-info-panel",
          div(class = "floating-panel-header",
              h4(class = "floating-panel-title", info$name),
              span(class = "app-badge", "Distrito")
          ),
          
          div(class = "detail-section",
              div(class = "detail-label", "Departamento"),
              div(class = "detail-value", info$depto)
          ),
          
          div(class = "detail-section",
              div(class = "detail-label", "Nuevo Municipio (Reforma 2024)"),
              div(class = "detail-value", info$mpio_nuevo)
          ),
          
          div(class = "detail-section",
              div(class = "detail-label", "Zona Agroproductiva Propuesta"),
              div(class = "detail-value", info$zone)
          ),
          
          div(class = "stats-grid",
              div(class = "stat-card",
                  div(class = "stat-value", format(info$agri_households, big.mark = ",")),
                  div(class = "stat-label", "Hogares Agrícolas")
              ),
              div(class = "stat-card",
                  div(class = "stat-value", format(info$viviendas, big.mark = ",")),
                  div(class = "stat-label", "Viviendas")
              ),
              div(class = "stat-card",
                  div(class = "stat-value", format(info$population, big.mark = ",")),
                  div(class = "stat-label", "Población")
              )
          )
      )
    }
    # If clicked a Census Segment
    else if (info$type == "segmento") {
      div(class = "floating-info-panel",
          div(class = "floating-panel-header",
              h4(class = "floating-panel-title", paste0("Seg: ", info$id)),
              span(class = "app-badge", "Segmento Censal")
          ),
          
          div(class = "detail-section",
              div(class = "detail-label", "Ubicación"),
              div(class = "detail-value", paste0("Cantón ", info$canton, ", ", info$mpio, ", ", info$depto))
          ),
          
          div(class = "stats-grid",
              div(class = "stat-card",
                  div(class = "stat-value", format(info$agri_households, big.mark = ",")),
                  div(class = "stat-label", "Hogares Agrícolas")
              ),
              div(class = "stat-card",
                  div(class = "stat-value", format(info$viviendas, big.mark = ",")),
                  div(class = "stat-label", "Viviendas")
              ),
              div(class = "stat-card",
                  div(class = "stat-value", format(info$producers, big.mark = ",")),
                  div(class = "stat-label", "Productores")
              ),
              div(class = "stat-card",
                  div(class = "stat-value", format(info$population, big.mark = ",")),
                  div(class = "stat-label", "Población")
              )
          )
      )
    }
    # If clicked a Soil Potential land use capability polygon (CENTA)
    else if (info$type == "uso_suelo") {
      # Descriptive agricultural capability text
      class_desc <- switch(info$clase,
        "Clase I"   = "Suelos con muy pocas limitaciones que restrinjan su uso. Muy fértiles y planos, aptos para cultivos intensivos anuales.",
        "Clase II"  = "Suelos con limitaciones moderadas que reducen la elección de cultivos o requieren prácticas de conservación moderadas.",
        "Clase III" = "Suelos con limitaciones severas que requieren prácticas especiales de conservación y manejo. Aptos para varios cultivos.",
        "Clase IV"  = "Suelos con limitaciones muy severas. Adecuados para cultivos ocasionales con manejo intensivo o pastos/cultivos perennes.",
        "Clase V"   = "Suelos no aptos para cultivos anuales debido a inundaciones, pedregosidad o humedad. Aptos para pastos y silvicultura.",
        "Clase VI"  = "Suelos aptos principalmente para vegetación permanente (pastos, frutales, cacao, café) y silvicultura. Moderada pendiente.",
        "Clase VII" = "Suelos con limitaciones muy severas. Aptos únicamente para forestería de protección, café bajo sombra y conservación.",
        "Clase VIII" = "Suelos de protección extrema. No aptos para uso agrícola ni forestal comercial; exclusivos para conservación y fauna silvestre.",
        "Agua"      = "Embalse, lago, laguna o cuerpo de agua superficial registrado.",
        "Urbana"    = "Áreas urbanas consolidadas, cascos municipales o infraestructura civil.",
        "Pantano"   = "Zonas húmedas inundables o pantanosas con drenaje muy deficiente. Claves para la regulación hídrica.",
        "Suelo sin clasificación o zona protegida."
      )
      
      div(class = "floating-info-panel",
          div(class = "floating-panel-header",
              h4(class = "floating-panel-title", info$clase),
              span(class = "app-badge", "Aptitud de Uso (CENTA)")
          ),
          
          div(class = "detail-section",
              div(class = "detail-label", "Ubicación Geográfica"),
              div(class = "detail-value", paste0(info$mpio, ", ", info$depto))
          ),
          
          div(class = "detail-section",
              div(class = "detail-label", "Capacidad Agrológica del Suelo"),
              div(class = "detail-value", class_desc)
          ),
          
          div(class = "stats-grid",
              div(class = "stat-card",
                  div(class = "stat-value", format(round(info$km2, 2), big.mark = ",")),
                  div(class = "stat-label", "Área (Km²)")
              ),
              div(class = "stat-card",
                  div(class = "stat-value", format(round(info$ha, 2), big.mark = ",")),
                  div(class = "stat-label", "Área (Hectáreas)")
              ),
              div(class = "stat-card",
                  div(class = "stat-value", format(round(info$mz, 2), big.mark = ",")),
                  div(class = "stat-label", "Área (Manzanas)")
              )
          )
      )
    }
    # If clicked a Plan Nacion Region
    else if (info$type == "plan_nacion") {
      div(class = "floating-info-panel",
          div(class = "floating-panel-header",
              h4(class = "floating-panel-title", info$name),
              span(class = "app-badge", "Región Plan Nación")
          ),
          
          div(class = "detail-section",
              div(class = "detail-label", "Cobertura de Servicios Básicos"),
              div(class = "detail-value", paste0("Hogares con Agua Potable: ", format(info$serv_agua, big.mark = ","))),
              div(class = "detail-value", paste0("Hogares con Electricidad: ", format(info$serv_elect, big.mark = ","))),
              div(class = "detail-value", paste0("Hogares con Telefonía: ", format(info$serv_telef, big.mark = ","))),
              div(class = "detail-value", paste0("Hogares con Computadora: ", format(info$computador, big.mark = ",")))
          ),
          
          div(class = "stats-grid",
              div(class = "stat-card",
                  div(class = "stat-value", format(info$personas, big.mark = ",")),
                  div(class = "stat-label", "Población")
              ),
              div(class = "stat-card",
                  div(class = "stat-value", format(info$hogares, big.mark = ",")),
                  div(class = "stat-label", "Hogares")
              ),
              div(class = "stat-card",
                  div(class = "stat-value", format(info$viviendas, big.mark = ",")),
                  div(class = "stat-label", "Viviendas")
              )
          )
      )
    }
    # If clicked a PNODT Region
    else if (info$type == "pnodt") {
      div(class = "floating-info-panel",
          div(class = "floating-panel-header",
              h4(class = "floating-panel-title", info$name),
              span(class = "app-badge", paste0("ID: ", info$id))
          ),
          
          div(class = "detail-section",
              div(class = "detail-label", "Servicios Básicos (Hogares)"),
              div(class = "detail-value", paste0("Acceso a Agua: ", format(info$serv_agua, big.mark = ","))),
              div(class = "detail-value", paste0("Acceso a Electricidad: ", format(info$serv_elect, big.mark = ",")))
          ),
          
          div(class = "stats-grid",
              div(class = "stat-card",
                  div(class = "stat-value", format(info$personas, big.mark = ",")),
                  div(class = "stat-label", "Población")
              ),
              div(class = "stat-card",
                  div(class = "stat-value", format(info$hogares, big.mark = ",")),
                  div(class = "stat-label", "Hogares")
              ),
              div(class = "stat-card",
                  div(class = "stat-value", format(info$viviendas, big.mark = ",")),
                  div(class = "stat-label", "Viviendas")
              )
          )
      )
    }
    # If clicked a PNODT Subregion
    else if (info$type == "subpnodt") {
      div(class = "floating-info-panel",
          div(class = "floating-panel-header",
              h4(class = "floating-panel-title", info$name),
              span(class = "app-badge", "Subregión PNODT")
          ),
          
          div(class = "detail-section",
              div(class = "detail-label", "Servicios Básicos (Hogares)"),
              div(class = "detail-value", paste0("Acceso a Agua: ", format(info$serv_agua, big.mark = ","))),
              div(class = "detail-value", paste0("Acceso a Electricidad: ", format(info$serv_elect, big.mark = ",")))
          ),
          
          div(class = "stats-grid",
              div(class = "stat-card",
                  div(class = "stat-value", format(info$personas, big.mark = ",")),
                  div(class = "stat-label", "Población")
              ),
              div(class = "stat-card",
                  div(class = "stat-value", format(info$hogares, big.mark = ",")),
                  div(class = "stat-label", "Hogares")
              ),
              div(class = "stat-card",
                  div(class = "stat-value", format(info$viviendas, big.mark = ",")),
                  div(class = "stat-label", "Viviendas")
              )
          )
      )
    }
    # If clicked a Corredor Seco District
    else if (info$type == "corredor_seco_dist") {
      div(class = "floating-info-panel",
          div(class = "floating-panel-header",
              h4(class = "floating-panel-title", info$name),
              span(class = "app-badge", "Corredor Seco")
          ),
          
          div(class = "detail-section",
              div(class = "detail-label", "Ubicación Administrativa"),
              div(class = "detail-value", paste0("Departamento: ", info$depto)),
              div(class = "detail-value", paste0("Nuevo Municipio: ", info$mpio_nuevo)),
              div(class = "detail-value", paste0("Propuesta Agroproductiva: ", info$zone))
          ),
          
          div(class = "detail-section",
              div(class = "detail-label", "Aptitud de Cultivo en Corredor Seco"),
              div(class = "detail-value", "Este distrito forma parte de los 104 distritos del Corredor Seco de El Salvador, caracterizado por una alta susceptibilidad a sequías severas e irregularidad de lluvias. Se recomienda inversión en infraestructura de riego, cosechas de agua, y cultivos tolerantes al estrés hídrico (como sorgo, variedades resistentes de frijol y sistemas silvopastoriles).")
          ),
          
          div(class = "stats-grid",
              div(class = "stat-card",
                  div(class = "stat-value", format(info$agri_households, big.mark = ",")),
                  div(class = "stat-label", "Hogares Agrícolas")
              ),
              div(class = "stat-card",
                  div(class = "stat-value", format(info$viviendas, big.mark = ",")),
                  div(class = "stat-label", "Viviendas")
              ),
              div(class = "stat-card",
                  div(class = "stat-value", format(info$population, big.mark = ",")),
                  div(class = "stat-label", "Población")
              )
          )
      )
    }
    # If clicked a Clase de Suelo (Great Group)
    else if (info$type == "clase_suelo") {
      soil_desc <- switch(info$nombre,
        "Agua"                        = "Cuerpos de agua superficiales, lagos, lagunas o embalses.",
        "ANDISOLES"                  = "Suelos de origen volcánico desarrollados sobre cenizas y piroclastos. Poseen alta capacidad de retención de humedad, alta porosidad y son extremadamente fértiles para café, hortalizas y frutales de altura.",
        "LATOSOLES ARCILLOSOS ACIDOS" = "Suelos altamente meteorizados, arcillosos y de pH ácido. Requieren enmiendas de cal o fertilización adecuada. Típicos en zonas montañosas y laderas.",
        "LITOSOLES"                  = "Suelos muy delgados y pedregosos formados sobre roca consolidada en fuertes pendientes. Tienen aptitud forestal y de protección hídrica, alta susceptibilidad a la erosión.",
        "ALUVIALES"                   = "Suelos planos de valles fluviales depositados por corrientes. Sumamente fértiles, profundos y aptos para agricultura intensiva de riego (hortalizas, caña de azúcar, granos básicos).",
        "LATOSOLES ARCILLO ROJIZOS"  = "Suelos de color rojizo bien drenados y profundos, con buena saturación de bases. Altamente productivos para una gran variedad de cultivos tropicales.",
        "GRUMOSOLES"                  = "Suelos arcillosos pesados (arcillas expansivas tipo 2:1) que se agrietan fuertemente en época seca y se expanden en lluviosa. Aptos para caña de azúcar, arroz y pastos, requieren drenaje controlado.",
        "REGOSOLES Y HALOMORFICOS"   = "Suelos de texturas arenosas o costeros salinos. Aptos para coco, marañón o pastos tolerantes a la salinidad, con limitaciones por baja retención hídrica o exceso de sales.",
        "AREA URBANA"                = "Suelos sellados por infraestructura civil, edificaciones o cascos urbanos.",
        "Suelo sin clasificar."
      )
      
      div(class = "floating-info-panel",
          div(class = "floating-panel-header",
              h4(class = "floating-panel-title", info$nombre),
              span(class = "app-badge", paste0("Código: ", info$codigo))
          ),
          
          div(class = "detail-section",
              div(class = "detail-label", "Ubicación Geográfica"),
              div(class = "detail-value", paste0(info$mpio, ", ", info$depto))
          ),
          
          div(class = "detail-section",
              div(class = "detail-label", "Características del Grupo de Suelo"),
              div(class = "detail-value", soil_desc)
          ),
          
          div(class = "stats-grid",
              div(class = "stat-card",
                  div(class = "stat-value", format(round(info$km2, 2), big.mark = ",")),
                  div(class = "stat-label", "Área (Km²)")
              ),
              div(class = "stat-card",
                  div(class = "stat-value", format(round(info$ha, 2), big.mark = ",")),
                  div(class = "stat-label", "Área (Hectáreas)")
              ),
              div(class = "stat-card",
                  div(class = "stat-value", format(round(info$mz, 2), big.mark = ",")),
                  div(class = "stat-label", "Área (Manzanas)")
              )
          )
      )
    }
    # If clicked a Humedad Relativa
    else if (info$type == "humedad") {
      hum_desc <- switch(info$valor,
        "60-70" = "Zonas de menor humedad relativa promedio. Favorece la sanidad vegetal reduciendo la incidencia de hongos, ideal para granos básicos, melón y sandía en época seca con riego dirigido.",
        "70-75" = "Humedad relativa intermedia. Adecuada para la mayoría de cultivos anuales y frutales tropicales, balance óptimo entre transpiración y desarrollo vegetativo.",
        "75-80" = "Zonas de humedad relativa alta. Propicio para el desarrollo de hongos foliares si no se manejan distancias adecuadas; excelente para hortalizas de hoja, café, y cacao.",
        ">80"   = "Humedad relativa muy alta (usualmente zonas montañosas o boscosas). Excelente para helechos, musgos, orquídeas y café bajo sombra, con alta necesidad de monitoreo fitosanitario.",
        "Humedad sin caracterizar."
      )
      
      div(class = "floating-info-panel",
          div(class = "floating-panel-header",
              h4(class = "floating-panel-title", paste0("HR: ", info$valor, "%")),
              span(class = "app-badge", info$interpretacion)
          ),
          
          div(class = "detail-section",
              div(class = "detail-label", "Ubicación Geográfica"),
              div(class = "detail-value", paste0(info$mpio, ", ", info$depto))
          ),
          
          div(class = "detail-section",
              div(class = "detail-label", "Impacto y Manejo Agrícola"),
              div(class = "detail-value", hum_desc)
          ),
          
          div(class = "stats-grid",
              div(class = "stat-card",
                  div(class = "stat-value", format(round(info$km2, 2), big.mark = ",")),
                  div(class = "stat-label", "Área (Km²)")
              ),
              div(class = "stat-card",
                  div(class = "stat-value", format(round(info$ha, 2), big.mark = ",")),
                  div(class = "stat-label", "Área (Hectáreas)")
              ),
              div(class = "stat-card",
                  div(class = "stat-value", format(round(info$mz, 2), big.mark = ",")),
                  div(class = "stat-label", "Área (Manzanas)")
              )
          )
      )
    }
    # If clicked a Temperatura
    else if (info$type == "temperatura_clima") {
      temp_desc <- if (info$rango == "?") {
        "Zona sin medición climática registrada."
      } else {
        switch(info$rango,
          "10.0 - 12.5°C" = "Clima extremadamente frío de alta montaña. Apto para hortalizas de clima muy frío (papa, repollo) y frutales de hueso de altura (durazno, manzana).",
          "12.5 - 15.0°C" = "Clima frío de montaña. Excelente para flores de exportación, hortalizas especializadas (brócoli, lechuga) y frutales deciduos.",
          "15.0 - 17.5°C" = "Clima templado-frío. Ideal para café de estricta altura (SHG), fresas, moras y hortalizas de clima templado.",
          "17.5 - 20.0°C" = "Clima templado agradable. Altamente favorable para café de alta calidad, aguacate, cítricos y frutales diversos.",
          "20.0 - 22.5°C" = "Clima templado-cálido. Apto para café de media altura, maíz, frijol y sistemas agroforestales.",
          "22.5 - 25.0°C" = "Clima cálido-templado. Excelente para maíz, sorgo, ganadería y frutales como mango, aguacate de bajura y cítricos.",
          "25.0 - 27.5°C" = "Clima cálido tropical. Ideal para caña de azúcar, maíz, pastizales extensivos, coco y plátano. Requiere riego en época seca.",
          "27.5 - 30.0°C" = "Clima muy cálido (típico de llanuras costeras). Apto para caña de azúcar de alto rendimiento, pastos tolerantes al calor, melón, sandía y ajonjolí bajo riego.",
          paste0("Clima con rango térmico promedio de ", info$rango, ".")
        )
      }
      
      div(class = "floating-info-panel",
          div(class = "floating-panel-header",
              h4(class = "floating-panel-title", info$rango),
              span(class = "app-badge", "Temperatura Promedio")
          ),
          
          div(class = "detail-section",
              div(class = "detail-label", "Ubicación Geográfica"),
              div(class = "detail-value", paste0(info$mpio, ", ", info$depto))
          ),
          
          div(class = "detail-section",
              div(class = "detail-label", "Aptitud de Cultivos por Clima"),
              div(class = "detail-value", temp_desc)
          ),
          
          div(class = "stats-grid",
              div(class = "stat-card",
                  div(class = "stat-value", format(round(info$km2, 2), big.mark = ",")),
                  div(class = "stat-label", "Área (Km²)")
              ),
              div(class = "stat-card",
                  div(class = "stat-value", format(round(info$ha, 2), big.mark = ",")),
                  div(class = "stat-label", "Área (Hectáreas)")
              ),
              div(class = "stat-card",
                  div(class = "stat-value", format(round(info$mz, 2), big.mark = ",")),
                  div(class = "stat-label", "Área (Manzanas)")
              )
          )
      )
    }
    # If clicked a Precipitación
    else if (info$type == "precipitacion_clima") {
      precip_desc <- switch(info$valor,
        "1100-1400" = "Zonas de menor precipitación anual en el país. Susceptibles a sequías recurrentes; requieren variedades resistentes y sistemas de riego para cultivos comerciales.",
        "1400-1500" = "Rango de lluvia moderado-bajo. Adecuado para granos básicos de ciclo corto y ganadería, con necesidad de conservación de humedad en suelos.",
        "1500-1600" = "Rango de lluvia promedio del corredor seco alto. Favorable para sorgo y frijol en siembras de postrera.",
        "1600-1700" = "Lluvia intermedia. Rango común en valles interiores y meseta central, propicio para caña de azúcar y café de bajura.",
        "1700-1800" = "Lluvia óptima para la mayoría de cultivos agrícolas anuales de El Salvador sin requerir riego suplementario constante en época de lluvias.",
        "1800-1900" = "Precipitación favorable para frutales tropicales, cítricos, y café de media altura.",
        "1900-2000" = "Zonas de alta precipitación. Común en estribaciones montañosas, excelente para recarga de acuíferos y café de altura.",
        "2000-2100" = "Precipitación muy alta. Favorable para café de estricta altura (SHG) y silvicultura de montaña.",
        "2100-2500" = "Zonas lluviosas de cordillera. Alta recarga hídrica, propicio para café y hortalizas de clima templado.",
        "Mas de 2500" = "Las zonas más lluviosas del país (cumbres de montañas y macizos volcánicos). Exclusivas para conservación de bosques de neblina, café de alta calidad y recarga hídrica nacional.",
        "Precipitación sin clasificar."
      )
      
      div(class = "floating-info-panel",
          div(class = "floating-panel-header",
              h4(class = "floating-panel-title", paste0(info$valor, " mm")),
              span(class = "app-badge", "Precipitación Anual")
          ),
          
          div(class = "detail-section",
              div(class = "detail-label", "Zonificación Hídrica (CENTA)"),
              div(class = "detail-value", precip_desc)
          ),
          
          div(class = "detail-section",
              div(class = "detail-label", "Área Total en el País"),
              div(class = "detail-value", "Esta isoyeta de precipitación cubre un área agregada a nivel nacional de:")
          ),
          
          div(class = "stats-grid",
              div(class = "stat-card",
                  div(class = "stat-value", format(round(info$km2, 2), big.mark = ",")),
                  div(class = "stat-label", "Área (Km²)")
              ),
              div(class = "stat-card",
                  div(class = "stat-value", format(round(info$ha, 2), big.mark = ",")),
                  div(class = "stat-label", "Área (Hectáreas)")
              ),
              div(class = "stat-card",
                  div(class = "stat-value", format(round(info$mz, 2), big.mark = ",")),
                  div(class = "stat-label", "Área (Manzanas)")
              )
          )
      )
    }
    # If clicked a Potencial de cultivo
    else if (info$type == "potencial_cultivo") {
      crop_desc <- switch(info$crop,
        "Maíz" = switch(info$valor,
          "Potencial Alto"  = "Suelos y climas con aptitud óptima para maíz. Rendimientos excelentes con manejo tecnológico intermedio.",
          "Potencial Medio" = "Aptitud moderada. Limitaciones leves por pendiente, fertilidad o lluvia. Requiere fertilización equilibrada.",
          "Potencial Bajo"  = "Aptitud marginal. Limitaciones severas de suelo o pedregosidad. Se sugiere rotación o sistemas mixtos.",
          "Suelos con limitaciones severas para maíz o áreas protegidas."
        ),
        "Frijol" = switch(info$valor,
          "Potencial Alto"  = "Zonas ideales para frijol (especialmente postrera). Suelos francos bien drenados que evitan encharcamientos.",
          "Potencial Medio" = "Aptitud media. Riesgo de encharcamiento o sequía moderada. Se aconseja camas de siembra elevadas.",
          "Potencial Bajo"  = "Aptitud marginal. Suelos muy arcillosos o propensos a sequía extrema, alto riesgo de enfermedades radiculares.",
          "Suelos no aptos para frijol."
        ),
        "Sorgo" = switch(info$valor,
          "Potencial Alto"  = "Aptitud excelente. Zonas ideales para sorgo de grano o forrajero, con alta eficiencia fotosintética y tolerancia térmica.",
          "Potencial Medio" = "Aptitud moderada. Buena producción con manejo de drenaje y densidad de siembra adaptada.",
          "Potencial Bajo"  = "Aptitud marginal. Suelos extremadamente pedregosos o de pendiente muy pronunciada.",
          "Área no recomendada para sorgo comercial."
        ),
        "Caña de Azúcar" = switch(info$valor,
          "Aptitud alta"  = "Valles planos con suelos profundos y climas cálidos de excelente radiación solar. Rendimientos máximos de sacarosa.",
          "Aptitud media" = "Aptitud moderada. Limitada por disponibilidad de agua en época de sequía o pendientes suaves.",
          "Aptitud baja"  = "Aptitud marginal. Suelos arcillosos con drenaje deficiente o altitudes mayores a las recomendadas.",
          "No apto"       = "Terrenos de ladera, áreas protegidas o climas fríos montañosos donde la caña de azúcar no es económicamente viable.",
          "Sin recomendación."
        ),
        "Banano y Musáceas" = switch(info$valor,
          "Potencial alto"  = "Suelos aluviales ricos en potasio, profundos y climas cálidos-húmedos. Excelente potencial productivo con riego.",
          "Potencial medio" = "Aptitud moderada. Suelos con drenaje regular o limitación de humedad en época seca. Requiere riego constante.",
          "Potencial bajo"  = "Aptitud marginal. Suelos de baja fertilidad, delgados o con pendientes pronunciadas que dificultan el laboreo.",
          "Zonas no aptas para musáceas."
        ),
        "Clase de potencial sin descripción."
      )
      
      div(class = "floating-info-panel",
          div(class = "floating-panel-header",
              h4(class = "floating-panel-title", paste0("Aptitud: ", info$valor)),
              span(class = "app-badge", info$crop)
          ),
          
          div(class = "detail-section",
              div(class = "detail-label", "Evaluación Agrícola (CENTA)"),
              div(class = "detail-value", crop_desc)
          ),
          
          div(class = "detail-section",
              div(class = "detail-label", "Cobertura Nacional de esta Categoría"),
              div(class = "detail-value", paste0("La superficie total en El Salvador con esta categoría de potencial para ", info$crop, " es de:"))
          ),
          
          div(class = "stats-grid",
              div(class = "stat-card",
                  div(class = "stat-value", format(round(info$km2, 2), big.mark = ",")),
                  div(class = "stat-label", "Área (Km²)")
              ),
              div(class = "stat-card",
                  div(class = "stat-value", format(round(info$ha, 2), big.mark = ",")),
                  div(class = "stat-label", "Área (Hectáreas)")
              ),
              div(class = "stat-card",
                  div(class = "stat-value", format(round(info$mz, 2), big.mark = ",")),
                  div(class = "stat-label", "Área (Manzanas)")
              )
          )
      )
    }
  })
}

# Run Application
shinyApp(ui = ui, server = server)
