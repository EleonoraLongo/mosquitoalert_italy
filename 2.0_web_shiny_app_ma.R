rm(list = ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

library(shiny)
library(dplyr)
library(lubridate)
library(plotly)
library(sf)
library(leaflet)
library(MASS)
library(raster)


####DATA IN ENVIRONMENT####
grid <- sf::st_read("../external_data/grid_italy_4326.gpkg")
sf::st_crs(grid) <- 4326
password = "S5Hxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
bbox <- matrix(data = c(6.5, 18.55, 35.49, 47.1),
               ncol = 2, 
               nrow = 2, 
               byrow = TRUE, 
               dimnames = list(c("masked_lon","masked_lat"), 
                               c("min","max")))

italy <- sf::st_read("../external_data/gadm41_ITA_3.shp")

italy_regions <- italy %>%
  dplyr::group_by(NAME_1) %>%
  dplyr::summarise(geometry = sf::st_union(geometry))


#####uer locations#####
user_loc <- download.ma_private(
  grid = grid,
  file_zip_link = "https://drive.google.com/file/d/1Gdu9eym1saOeUfewnozXuIGoKGbL035R/view?usp=drive_link",
  file_inside_zip_name = "user_locations_small_cell.Rds",
  password = password,
  bbox = bbox
)

user_loc <- user_loc %>%
  dplyr::mutate(date = as.Date(fix_date))

user_loc <- user_loc %>% 
  dplyr::filter(fix_date > "2020-10-01")

#####raw records#####
raw_records <- download.ma_private(
  grid = grid,
  file_zip_link = "https://drive.google.com/file/d/1HCJ81d8XsiYSL276p9uT93dQ3w4w4qVU/view?usp=drive_link",
  file_inside_zip_name = "mosquito_alert_raw_reports.Rds",
  password = password,
  bbox = bbox
)

raw_records <- raw_records %>% sf::st_drop_geometry()

raw_records <- raw_records %>%
  dplyr::mutate(date = as.Date(creation_time))

raw_records <- raw_records %>% 
  dplyr::filter(date > "2020-10-01")


#####zenodo records#####
records <- download.ma_verified_records(
  years = 2020:2025,
  nation = "ITA",
  report.type = c("adult", "bite")
)

records <- records %>% 
  dplyr::select(-responses)

records <- records %>% 
  dplyr::filter(!movelab_annotation_euro.class_name == "Not sure")

records_sf <- sf::st_as_sf(records, coords = c("lon", "lat"), crs = 4326, remove = FALSE)

records_sf <- sf::st_join(records_sf, grid, left = TRUE)

records_sf_adult <- records_sf %>% dplyr::filter(type == "adult")

#####sampled cells grid#####
cells <- unique(records_sf_adult$TigacellID)

#unique sampled tigacells
grid$record <- 'no'

for (i in 1:nrow(grid)) {
  if (grid$TigacellID[i] %in% records_sf_adult$TigacellID) {
    grid$record[i] <- 'yes'
  }
}

grid$record <- as.factor(grid$record)

grid_sampled <- grid %>% 
  dplyr::filter(
    record == "yes"
  )


####SHINY APP####
#####ui#####
ui <- shiny::fluidPage(
  shiny::titlePanel("Visualizzazione Dati Mosquito Alert"),
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::selectInput(
        "plot_choice", "Seleziona il contenuto da visualizzare:",
        choices = c(
          "Nuovi Registrati vs Totali",
          "Media Record per Utente",
          "Tabella Frequenze",
          "Kernel Density Map",
          "Evenness (Pielou) Aedes albopictus vs Culex sp."
        )
      ),
      shiny::conditionalPanel(
        condition = "input.plot_choice == 'Nuovi Registrati vs Totali'",
        shiny::selectInput("aggregation", "Aggregazione per:",
                           choices = c(
                             "Giorno"      = "day",
                             "Settimana"   = "week",
                             "Bisettimana" = "biweek",
                             "Mese"        = "month",
                             "Anno"        = "year"
                           ), selected = "month"),
        selectInput("region",       "Regione:",   choices = NULL),
        selectInput("province",     "Provincia:", choices = NULL),
        selectInput("municipality", "Comune:",    choices = NULL)
      ),
      shiny::conditionalPanel(
        condition = "input.plot_choice == 'Media Record per Utente'",
        shiny::selectInput("aggregation_raw", "Aggregazione per:",
                           choices = c(
                             "Giorno"      = "day",
                             "Settimana"   = "week",
                             "Bisettimana" = "biweek",
                             "Mese"        = "month",
                             "Anno"        = "year"
                           ), selected = "month")
      ),
      shiny::conditionalPanel(
        condition = "input.plot_choice == 'Tabella Frequenze'",
        shiny::selectInput("region_tab",       "Regione:",   choices = NULL),
        shiny::selectInput("province_tab",     "Provincia:", choices = NULL),
        shiny::selectInput("municipality_tab", "Comune:",    choices = NULL)
      ),
      shiny::conditionalPanel(
        condition = "input.plot_choice == 'Kernel Density Map' || input.plot_choice == 'Evenness (Pielou) Aedes albopictus vs Culex sp.'",
        conditionalPanel(
          condition = "input.plot_choice == 'Kernel Density Map'",
          selectInput("species_k", "Specie:", choices = NULL)
        ),
        selectInput("region_k",       "Regione:",    choices = NULL),
        selectInput("province_k",     "Provincia:",  choices = NULL),
        selectInput("municipality_k", "Comune:",     choices = NULL)
      )
    ),
    mainPanel(
      uiOutput("dynamic_output"),
      br(),
      div(style = "margin-top: 10px;", htmlOutput("map_explanation"))
    )
  )
)

server <- function(input, output, session) {
  # Inizializzazione dropdown
  observe({
    updateSelectInput(session, "region",     choices = c("Tutte", sort(unique(user_loc$NAME_1))), selected = "Tutte")
    updateSelectInput(session, "region_tab", choices = c("Tutte", sort(unique(raw_records$NAME_1))), selected = "Tutte")
    updateSelectInput(session, "species_k",  choices = sort(unique(na.omit(records_sf_adult$movelab_annotation_euro.class_name))))
    updateSelectInput(session, "region_k",   choices = c("Tutte", sort(unique(records_sf_adult$NAME_1))), selected = "Tutte")
  })
  
  # Cascata regione→provincia→comune
  observeEvent(input$region, {
    prov <- if (input$region == "Tutte") user_loc$NAME_2 else user_loc$NAME_2[user_loc$NAME_1 == input$region]
    updateSelectInput(session, "province", choices = c("Tutte", sort(unique(prov))), selected = "Tutte")
    muni <- if (input$region == "Tutte") user_loc$NAME_3 else user_loc$NAME_3[user_loc$NAME_1 == input$region]
    updateSelectInput(session, "municipality", choices = c("Tutte", sort(unique(muni))), selected = "Tutte")
  })
  observeEvent(input$province, {
    muni <- if (input$province == "Tutte") user_loc$NAME_3 else user_loc$NAME_3[user_loc$NAME_2 == input$province]
    updateSelectInput(session, "municipality", choices = c("Tutte", sort(unique(muni))), selected = "Tutte")
  })
  observeEvent(input$region_tab, {
    prov <- if (input$region_tab == "Tutte") raw_records$NAME_2 else raw_records$NAME_2[raw_records$NAME_1 == input$region_tab]
    updateSelectInput(session, "province_tab", choices = c("Tutte", sort(unique(prov))), selected = "Tutte")
    muni <- if (input$region_tab == "Tutte") raw_records$NAME_3 else raw_records$NAME_3[raw_records$NAME_1 == input$region_tab]
    updateSelectInput(session, "municipality_tab", choices = c("Tutte", sort(unique(muni))), selected = "Tutte")
  })
  observeEvent(input$province_tab, {
    muni <- if (input$province_tab == "Tutte") raw_records$NAME_3 else raw_records$NAME_3[raw_records$NAME_2 == input$province_tab]
    updateSelectInput(session, "municipality_tab", choices = c("Tutte", sort(unique(muni))), selected = "Tutte")
  })
  observeEvent(input$region_k, {
    prov <- if (input$region_k == "Tutte") italy$NAME_2 else italy$NAME_2[italy$NAME_1 == input$region_k]
    updateSelectInput(session, "province_k", choices = c("Tutte", sort(unique(prov))), selected = "Tutte")
    muni <- if (input$region_k == "Tutte") italy$NAME_3 else italy$NAME_3[italy$NAME_1 == input$region_k]
    updateSelectInput(session, "municipality_k", choices = c("Tutte", sort(unique(muni))), selected = "Tutte")
  })
  observeEvent(input$province_k, {
    muni <- if (input$province_k == "Tutte") italy$NAME_3 else italy$NAME_3[italy$NAME_2 == input$province_k]
    updateSelectInput(session, "municipality_k", choices = c("Tutte", sort(unique(muni))), selected = "Tutte")
  })
  
  # Filtri reactives
  filtered_data <- reactive({
    df <- user_loc
    if (input$region      != "Tutte") df <- filter(df, NAME_1 == input$region)
    if (input$province    != "Tutte") df <- filter(df, NAME_2 == input$province)
    if (input$municipality!= "Tutte") df <- filter(df, NAME_3 == input$municipality)
    df
  })
  filtered_raw_table <- reactive({
    df <- raw_records
    if (input$region_tab      != "Tutte") df <- filter(df, NAME_1 == input$region_tab)
    if (input$province_tab    != "Tutte") df <- filter(df, NAME_2 == input$province_tab)
    if (input$municipality_tab!= "Tutte") df <- filter(df, NAME_3 == input$municipality_tab)
    df
  })
  
  # Aggregazione Media Record
  aggregated_raw_records <- reactive({
    filtered_raw_table() %>%
      mutate(
        date = as.Date(creation_time),
        time_group = case_when(
          input$aggregation_raw == "day"    ~ date,
          input$aggregation_raw == "week"   ~ floor_date(date, "week"),
          input$aggregation_raw == "biweek" ~ floor_date(date, "week") - weeks(week(date) %% 2),
          input$aggregation_raw == "month"  ~ floor_date(date, "month"),
          input$aggregation_raw == "year"   ~ floor_date(date, "year")
        )
      ) %>%
      group_by(time_group, user) %>% summarise(n_record = n(), .groups = "drop") %>%
      group_by(time_group) %>% summarise(
        media    = mean(n_record), n = n(), sd = sd(n_record),
        se       = ifelse(n>1, sd/sqrt(n), 0),
        ci_lower = pmax(media - qt(0.975, df=pmax(n-1,1))*se, 0),
        ci_upper = media + qt(0.975, df=pmax(n-1,1))*se,
        .groups = "drop"
      ) %>%
      arrange(time_group) %>%
      mutate(
        label = case_when(
          input$aggregation_raw == "day"    ~ format(time_group, "%d-%m-%Y"),
          input$aggregation_raw == "week"   ~ paste0("Settimana ", isoweek(time_group), " (", year(time_group), ")"),
          input$aggregation_raw == "biweek" ~ paste0("Bisettimana ", ceiling(isoweek(time_group)/2), " (", year(time_group), ")"),
          input$aggregation_raw == "month"  ~ format(time_group, "%B %Y"),
          input$aggregation_raw == "year"   ~ as.character(year(time_group))
        ),
        label = factor(label, levels = unique(label))
      )
  })
  
  # Aggregazione Registrazioni
  aggregated_data <- reactive({
    filtered_data() %>%
      mutate(
        date = as.Date(fix_date),
        time_group = case_when(
          input$aggregation == "day"    ~ date,
          input$aggregation == "week"   ~ floor_date(date, "week"),
          input$aggregation == "biweek" ~ floor_date(date, "week") - weeks(week(date) %% 2),
          input$aggregation == "month"  ~ floor_date(date, "month"),
          input$aggregation == "year"   ~ floor_date(date, "year")
        )
      ) %>%
      group_by(time_group) %>% summarise(
        nuovi_registrati = n_distinct(user_coverage_uuid[participation_days==0]),
        totali_registrati = n_distinct(user_coverage_uuid), .groups = "drop"
      ) %>%
      arrange(time_group) %>%
      mutate(
        label = case_when(
          input$aggregation == "day"    ~ format(time_group, "%d-%m-%Y"),
          input$aggregation == "week"   ~ paste0("Settimana ", isoweek(time_group), " (", year(time_group), ")"),
          input$aggregation == "biweek" ~ paste0("Bisettimana ", ceiling(isoweek(time_group)/2), " (", year(time_group), ")"),
          input$aggregation == "month"  ~ format(time_group, "%B %Y"),
          input$aggregation == "year"   ~ as.character(year(time_group))
        ),
        label = factor(label, levels = unique(label))
      )
  })
  
  filtered_k <- reactive({
    data <- records_sf_adult
    if (length(input$species_k) > 0) data <- data %>% dplyr::filter(movelab_annotation_euro.class_name %in% input$species_k)
    if (input$region_k != "Tutte") data <- data %>% dplyr::filter(NAME_1 == input$region_k)
    if (input$province_k != "Tutte") data <- data %>% dplyr::filter(NAME_2 == input$province_k)
    if (input$municipality_k != "Tutte") data <- data %>% dplyr::filter(NAME_3 == input$municipality_k)
    data
  })
  
  filtered_grid <- reactive({
    grid_filtered <- italy
    if (input$region_k != "Tutte") grid_filtered <- grid_filtered %>% filter(NAME_1 == input$region_k)
    if (input$province_k != "Tutte") grid_filtered <- grid_filtered %>% filter(NAME_2 == input$province_k)
    if (input$municipality_k != "Tutte") grid_filtered <- grid_filtered %>% filter(NAME_3 == input$municipality_k)
    grid_filtered
  })
  
  filtered_users <- reactive({
    data <- user_loc
    if (input$region_k != "Tutte") data <- data %>% filter(NAME_1 == input$region_k)
    if (input$province_k != "Tutte") data <- data %>% filter(NAME_2 == input$province_k)
    if (input$municipality_k != "Tutte") data <- data %>% filter(NAME_3 == input$municipality_k)
    data %>% distinct(user_coverage_uuid, .keep_all=TRUE)
  })
  
  # Render tabelle e grafici
  output$freq_table <- renderTable({
    df <- filtered_raw_table();
    if(nrow(df)==0) return(data.frame(Messaggio="Nessun dato disponibile"))
    counts <- df %>% count(user, name = "n")
    tot_u <- n_distinct(counts$user); tot_r <- sum(counts$n)
    tab <- counts %>% mutate(categoria = case_when(
      n==1~"1 record", n==2~"2 record", n==3~"3 record", n==4~"4 record",
      n>=5 & n<=10~"5-10 record", n>=11 & n<=20~"11-20 record",
      n>=21 & n<=50~"21-50 record", n>50~">50 record"
    )) %>% group_by(categoria) %>% summarise(
      `Numero di utenti nella categoria` = n(),
      `Totale report`=sum(n), .groups="drop"
    ) %>% mutate(
      `Frequenza relativa (%)`=round(`Numero di utenti nella categoria`/tot_u*100,2),
      `Frequenza relativa sui report (%)`=round(`Totale report`/tot_r*100,2)
    ) %>% arrange(factor(categoria, levels=c(
      "1 record","2 record","3 record","4 record",
      "5-10 record","11-20 record","21-50 record",">50 record"
    )))
    bind_rows(tab, tibble(
      categoria="Totale",
      `Numero di utenti nella categoria`=tot_u,
      `Totale report`=tot_r,
      `Frequenza relativa (%)`=100,
      `Frequenza relativa sui report (%)`=100
    ))
  })
  
  output$dynamic_plot <- renderPlotly({
    if(input$plot_choice=="Nuovi Registrati vs Totali"){
      plot_data <- aggregated_data(); if(nrow(plot_data)==0) return(NULL)
      plotly::plot_ly(plot_data, x=~label) %>%
        plotly::add_bars(y=~nuovi_registrati, name="Nuovi registrati",
                         hovertemplate="Periodo: %{x}<br>Nuovi: %{y}<extra></extra>") %>%
        plotly::add_bars(y=~totali_registrati, name="Totali registrati",
                         hovertemplate="Periodo: %{x}<br>Totali: %{y}<extra></extra>") %>%
        plotly::layout(
          title="Registrazioni",
          barmode="group",
          xaxis=list(title="Periodo", tickangle=-45),
          yaxis=list(title="Numero di registrazioni"),
          legend=list(x=0.1, y=1)
        )
    } else if(input$plot_choice=="Media Record per Utente"){
      plot_data <- aggregated_raw_records(); if(nrow(plot_data)==0) return(NULL)
      plotly::plot_ly(plot_data, x=~label) %>%
        plotly::add_trace(y=~media, mode="markers", type="scatter",
                          marker=list(size=8, color="steelblue"), name="Media") %>%
        plotly::add_trace(y=~ci_lower, mode="lines", type="scatter",
                          line=list(color="steelblue", width=2, dash="dot"), showlegend=FALSE) %>%
        plotly::add_trace(y=~ci_upper, mode="lines", type="scatter",
                          line=list(color="steelblue", width=2, dash="dot"), showlegend=FALSE) %>%
        plotly::layout(
          title="Media record per utente con intervallo di confidenza",
          xaxis=list(title="Periodo", tickangle=-45),
          yaxis=list(title="Media record per utente"),
          showlegend=FALSE
        )
    }
  })
  
  #––– Kernel density map
  output$kernel_map <- renderLeaflet({
    df  <- filtered_k()
    coords <- st_coordinates(df)
    req(nrow(coords) > 0)
    
    kde <- kde2d(coords[,1], coords[,2], n = 300)
    r   <- raster(
      xmn   = min(kde$x), xmx = max(kde$x),
      ymn   = min(kde$y), ymx = max(kde$y),
      nrows = nrow(kde$z), ncols = ncol(kde$z),
      crs   = st_crs(df)$wkt
    )
    values(r) <- as.vector(kde$z)
    
    gm <- grid_sampled %>% filter(record=="yes") %>% st_transform(st_crs(df))
    r_masked <- mask(r, as(gm, "Spatial"))
    pal <- colorNumeric("viridis", values(r_masked), na.color="transparent")
    
    leaflet() %>%
      addTiles() %>%
      addRasterImage(r_masked, colors = pal, opacity = 0.7) %>%
      addPolygons(data = italy_regions, fill = FALSE, color = "black", weight = 1.5) %>%
      addLegend(pal = pal, values = values(r_masked), title = "Densità")
  })
  
  #––– Evenness map
  output$user_evenness_map <- renderLeaflet({
    req(input$plot_choice == "Evenness (Pielou) Aedes albopictus vs Culex sp.")
    species_list <- c("Aedes albopictus", "Culex sp.")
    
    # 1) punti filtrati
    segs <- records_sf_adult %>%
      filter(movelab_annotation_euro.class_name %in% species_list) %>%
      { if(input$region_k!="Tutte")       filter(., NAME_1 == input$region_k)       else . } %>%
      { if(input$province_k!="Tutte")     filter(., NAME_2 == input$province_k)     else . } %>%
      { if(input$municipality_k!="Tutte") filter(., NAME_3 == input$municipality_k) else . } %>%
      st_transform(4326)
    req(nrow(segs) > 0)
    
    # 2) raster template
    bb   <- st_bbox(segs)
    tpl  <- raster(
      xmn = bb["xmin"], xmx = bb["xmax"],
      ymn = bb["ymin"], ymx = bb["ymax"],
      nrows = 300, ncols = 300,
      crs = st_crs(4326)$wkt
    )
    
    # 3) KDE per specie
    rasters <- lapply(species_list, function(sp) {
      pts <- st_coordinates(filter(segs, movelab_annotation_euro.class_name == sp))
      if (nrow(pts)==0) {
        r <- tpl; values(r) <- 0; return(r)
      }
      kd <- kde2d(pts[,1], pts[,2], n = 300, lims = c(bb["xmin"], bb["xmax"], bb["ymin"], bb["ymax"]))
      r  <- tpl; values(r) <- as.vector(kd$z); r
    })
    stk <- stack(rasters)
    
    # 4) Pielou’s J
    pj <- calc(stk, fun = function(v) {
      v[is.na(v)] <- 0
      tot <- sum(v); if (tot<=0) return(NA)
      p   <- v / tot
      H   <- -sum(p[p>0]*log(p[p>0]))
      S   <- sum(v>0)
      if (S<=1) return(0)
      H / log(S)
    })
    
    # 5) maschero dove non ci sono utenti
    us <- filtered_users() %>% st_transform(4326)
    uv <- kde2d(st_coordinates(us)[,1], st_coordinates(us)[,2], n = 300, lims = c(bb["xmin"], bb["xmax"], bb["ymin"], bb["ymax"]))
    ru <- tpl; values(ru) <- as.vector(uv$z)
    mask_u <- ru; values(mask_u) <- ifelse(values(ru)>0,1,NA)
    pj2 <- mask(pj, mask_u)
    
    # 6) maschero con griglia campionata
    gm <- grid_sampled %>% filter(record=="yes") %>% st_transform(st_crs(pj2))
    mg <- rasterize(as(gm, "Spatial"), pj2, field=1, background=NA)
    pj_m <- mask(pj2, mg)
    
    # 7) plot finale
    pal <- colorNumeric("viridis", values(pj_m), na.color="transparent")
    leaflet() %>%
      addTiles() %>%
      addRasterImage(pj_m, colors = pal, opacity = 0.7) %>%
      addPolygons(data = italy_regions, fill = FALSE, color = "black", weight = 1.5) %>%
      addLegend(pal = pal, values = values(pj_m), title = "Pielou’s evenness")
  })
  
  #––– Sceglie quale output UI mostrare
  output$dynamic_output <- renderUI({
    switch(input$plot_choice,
           "Nuovi Registrati vs Totali"          = plotlyOutput("dynamic_plot"),
           "Media Record per Utente"             = plotlyOutput("dynamic_plot"),
           "Tabella Frequenze"                   = tableOutput("freq_table"),
           "Kernel Density Map"                  = leafletOutput("kernel_map"),
           "Evenness (Pielou) Aedes albopictus vs Culex sp." = leafletOutput("user_evenness_map")
    )
  })
  
  output$map_explanation <- renderUI({
    switch(input$plot_choice,
           "Kernel Density Map" = HTML("
        <p><strong>Kernel Density Map</strong><br>
        Stima la densità spaziale delle segnalazioni tramite KDE su griglia 300×300.<br>
        Colori più intensi = aree con più segnalazioni; trasparenza = zero segnalazioni.</p>
      "),
           "Evenness (Pielou) Aedes albopictus vs Culex sp." = HTML("
        <p><strong>Pielou’s evenness</strong><br>
        Misura l'uniformità spaziale tra le specie selezionate.<br>
        Per ogni specie si calcola la KDE; in ogni cella si determina la proporzione p<sub>i</sub> e<br>
        l'entropia di Shannon H = –∑p<sub>i</sub>ln(p<sub>i</sub>), normalizzata da ln(S):<br>
        J = H / ln(S), S = numero di specie presenti.<br>
        Valori vicini a 1 = distribuzione uniforme; vicini a 0 = specie dominate.</p>
      "),
           NULL
    )
  })
  
}

shinyApp(ui, server)

