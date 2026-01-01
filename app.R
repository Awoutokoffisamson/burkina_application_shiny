################################################################################
# APP.R - Application Shiny Burkina Faso
# Design Professionnel - Bleu Foncé & Orange
################################################################################

library(shiny)
library(leaflet)
library(DT)
library(plotly)
library(sf)
library(dplyr)
library(scales)

# Charger les données
source("global.R")

# =============================================================================
# UI - Interface utilisateur
# =============================================================================

ui <- fluidPage(

    # CSS personnalisé
    tags$head(
        tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800&display=swap"),
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
        tags$style(HTML("
            .shiny-output-error { visibility: hidden; }
            .shiny-output-error:before { visibility: hidden; }
        "))
    ),

    # ===== HEADER =====
    div(
        class = "header-container",
        div(
            style = "display: flex; align-items: center; gap: 20px;",
            # Drapeau du Burkina Faso
            div(
                style = "width: 70px; height: 47px; border-radius: 5px; overflow: hidden; box-shadow: 0 3px 8px rgba(0,0,0,0.3); flex-shrink: 0;",
                div(style = "height: 50%; background-color: #EF2B2D;"),
                div(
                    style = "height: 50%; background-color: #009E49; position: relative;",
                    div(style = "position: absolute; top: -12px; left: 50%; transform: translateX(-50%); color: #FCD116; font-size: 24px; text-shadow: 0 1px 2px rgba(0,0,0,0.3);", "★")
                )
            ),
            div(
                h1(class = "header-title", "Burkina Faso — Subdivisions Administratives 2025"),
                p(class = "header-subtitle", "Nouvelle organisation territoriale : 17 Régions • 47 Provinces • 351 Communes")
            )
        )
    ),

    # ===== SOUS-HEADER ORANGE =====
    div(
        class = "sub-header",
        span(class = "sub-header-text", " Données du Recensement Général de la Population et de l'Habitation (RGPH) 2019"),
        span(class = "sub-header-text", " Réforme administrative de Juillet 2025")
    ),

    # ===== CONTENU PRINCIPAL =====
    fluidRow(
        style = "padding: 0 25px;",

        # Sidebar
        column(
            3,
            wellPanel(
                h4(" Navigation"),
                selectInput("niveau", NULL,
                    choices = c(
                        "️ Régions" = "regions",
                        " Provinces" = "provinces",
                        "️ Communes" = "communes"
                    ),
                    selected = "regions"
                ),
                conditionalPanel(
                    condition = "input.niveau == 'provinces' || input.niveau == 'communes'",
                    selectInput("region_filter", "Filtrer par région :",
                        choices = c("Toutes les régions" = "all"),
                        selected = "all"
                    )
                ),
                conditionalPanel(
                    condition = "input.niveau == 'communes'",
                    selectInput("province_filter", "Filtrer par province :",
                        choices = c("Toutes les provinces" = "all"),
                        selected = "all"
                    )
                )
            ),
            wellPanel(
                h4("📈 Statistiques nationales"),
                tableOutput("stats_nationales")
            ),
            wellPanel(
                h4(" Sources"),
                tags$div(
                    style = "font-size: 0.85rem; color: #4a5568;",
                    tags$p(style = "margin: 8px 0;", HTML(" <strong>Population</strong> : INSD - RGPH 2019")),
                    tags$p(style = "margin: 8px 0;", HTML(" <strong>Subdivision</strong> : Réforme Juillet 2025"))
                )
            ),

            # Section Auteur
            div(
                class = "author-section",
                h5("Réalisé par"),
                p("AWOUTO K. Samson, élève ingénieur statisticien économiste à l'ENSAE DAKAR")
            )
        ),

        # Panel principal
        column(
            9,

            # Breadcrumb
            uiOutput("breadcrumb"),

            # Cartes statistiques
            fluidRow(
                column(
                    3,
                    div(
                        class = "stat-card",
                        div(class = "stat-value", textOutput("stat_count", inline = TRUE)),
                        div(class = "stat-label", textOutput("stat_label", inline = TRUE))
                    )
                ),
                column(
                    3,
                    div(
                        class = "stat-card",
                        div(class = "stat-value", textOutput("stat_pop", inline = TRUE)),
                        div(class = "stat-label", "Population")
                    )
                ),
                column(
                    3,
                    div(
                        class = "stat-card",
                        div(class = "stat-value", textOutput("stat_area", inline = TRUE)),
                        div(class = "stat-label", "Superficie (km²)")
                    )
                ),
                column(
                    3,
                    div(
                        class = "stat-card",
                        div(class = "stat-value", textOutput("stat_density", inline = TRUE)),
                        div(class = "stat-label", "Densité (hab/km²)")
                    )
                )
            ),
            br(),

            # Tabs
            tabsetPanel(
                id = "main_tabs",
                tabPanel(
                    "🗺️ Carte Interactive",
                    br(),
                    div(
                        class = "map-container",
                        leafletOutput("map", height = "520px")
                    )
                ),
                tabPanel(
                    "📊 Tableau de données",
                    br(),
                    DTOutput("data_table")
                ),
                tabPanel(
                    "📈 Visualisations",
                    br(),
                    fluidRow(
                        column(6, plotlyOutput("chart_pop", height = "420px")),
                        column(6, plotlyOutput("chart_density", height = "420px"))
                    )
                )
            )
        )
    ),

    # ===== FOOTER =====
    div(
        class = "footer-container",
        div(class = "footer-author", "Travaux de AWOUTO K. Samson"),
        div(class = "footer-credits", "Application développée avec R Shiny | Données INSD 2019")
    )
)

# =============================================================================
# SERVER - Logique serveur
# =============================================================================

server <- function(input, output, session) {
    # Mise à jour des filtres
    observe({
        updateSelectInput(session, "region_filter",
            choices = c(
                "Toutes les régions" = "all",
                sort(unique(regions_data$region_new))
            )
        )
    })

    observe({
        if (input$region_filter != "all") {
            provinces_in_region <- provinces_data %>%
                filter(region_new == input$region_filter) %>%
                pull(province_new) %>%
                sort()
            updateSelectInput(session, "province_filter",
                choices = c("Toutes les provinces" = "all", provinces_in_region)
            )
        } else {
            updateSelectInput(session, "province_filter",
                choices = c("Toutes les provinces" = "all", sort(unique(provinces_data$province_new)))
            )
        }
    })

    # Données réactives
    current_data <- reactive({
        switch(input$niveau,
            "regions" = regions_data,
            "provinces" = {
                data <- provinces_data
                if (input$region_filter != "all") {
                    data <- data %>% filter(region_new == input$region_filter)
                }
                data
            },
            "communes" = {
                data <- communes_data
                if (input$region_filter != "all") {
                    data <- data %>% filter(region_new == input$region_filter)
                }
                if (input$province_filter != "all") {
                    data <- data %>% filter(province_new == input$province_filter)
                }
                data
            }
        )
    })

    # Statistiques nationales
    output$stats_nationales <- renderTable(
        {
            burkina_summary
        },
        colnames = FALSE,
        striped = TRUE,
        hover = TRUE,
        bordered = FALSE,
        width = "100%"
    )

    # Breadcrumb
    output$breadcrumb <- renderUI({
        # Initialize items with the base Burkina Faso link
        items <- list(
            actionLink("goto_burkina", "Burkina Faso", class = "breadcrumb-item")
        )

        if (input$niveau != "regions" && input$region_filter != "all") {
            items <- c(items, list(
                span(" › ", class = "breadcrumb-separator"),
                actionLink("goto_region", input$region_filter, class = "breadcrumb-item")
            ))
        }

        if (input$niveau == "communes" && input$province_filter != "all") {
            items <- c(items, list(
                span(" › ", class = "breadcrumb-separator"),
                span(input$province_filter, class = "breadcrumb-current")
            ))
        }

        div(class = "breadcrumb", items)
    })

    # Cartes statistiques
    output$stat_count <- renderText({
        format(nrow(current_data()), big.mark = " ")
    })

    output$stat_label <- renderText({
        switch(input$niveau,
            "regions" = "Régions",
            "provinces" = "Provinces",
            "communes" = "Communes"
        )
    })

    output$stat_pop <- renderText({
        total <- sum(current_data()$Population_2019, na.rm = TRUE)
        if (total >= 1e6) {
            paste0(round(total / 1e6, 1), " M")
        } else {
            format(total, big.mark = " ")
        }
    })

    output$stat_area <- renderText({
        total <- sum(current_data()$Superficie_km2, na.rm = TRUE)
        format(round(total), big.mark = " ")
    })

    output$stat_density <- renderText({
        pop <- sum(current_data()$Population_2019, na.rm = TRUE)
        area <- sum(current_data()$Superficie_km2, na.rm = TRUE)
        round(pop / area, 1)
    })

    # Carte Leaflet
    output$map <- renderLeaflet({
        data <- current_data()

        # Palette orange
        pal <- colorNumeric(
            palette = c("#fff7ec", "#fee8c8", "#fdd49e", "#fdbb84", "#fc8d59"),
            domain = data$Densite,
            na.color = "#cccccc"
        )

        name_col <- switch(input$niveau,
            "regions" = "region_new",
            "provinces" = "province_new",
            "communes" = "NAME_3"
        )

        labels <- sprintf(
            "<div style='font-family: Inter, sans-serif;'><strong style='font-size: 14px; color: #1e3a5f;'>%s</strong><br/><span style='color: #c65d07;'>Population:</span> %s<br/><span style='color: #c65d07;'>Superficie:</span> %s km²<br/><span style='color: #c65d07;'>Densité:</span> %s hab/km²</div>",
            data[[name_col]],
            format(data$Population_2019, big.mark = " "),
            format(round(data$Superficie_km2), big.mark = " "),
            data$Densite
        ) %>% lapply(htmltools::HTML)

        leaflet(data) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            addPolygons(
                fillColor = ~ pal(Densite),
                weight = 1.5,
                opacity = 1,
                color = "#1e3a5f",
                fillOpacity = 0.75,
                highlightOptions = highlightOptions(
                    weight = 3,
                    color = "#fc8d59",
                    fillOpacity = 0.9,
                    bringToFront = TRUE
                ),
                label = labels,
                labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "8px 12px", "border-radius" = "8px"),
                    textsize = "13px",
                    direction = "auto"
                )
            ) %>%
            addLegend(
                pal = pal,
                values = ~Densite,
                title = "Densité<br/>(hab/km²)",
                position = "bottomright",
                opacity = 0.9
            )
    })

    # Tableau de données
    output$data_table <- renderDT({
        data <- current_data() %>% st_drop_geometry()

        if (input$niveau == "regions") {
            data <- data %>%
                select(
                    Région = region_new, Provinces = nb_provinces, Communes = nb_communes,
                    `Population 2019` = Population_2019, `Superficie (km²)` = Superficie_km2,
                    `Densité` = Densite
                ) %>%
                arrange(Région)
        } else if (input$niveau == "provinces") {
            data <- data %>%
                select(
                    Province = province_new, Région = region_new, Communes = nb_communes,
                    `Population 2019` = Population_2019, `Superficie (km²)` = Superficie_km2,
                    `Densité` = Densite
                ) %>%
                arrange(Province)
        } else {
            data <- data %>%
                select(
                    Commune = NAME_3, Province = province_new, Région = region_new,
                    `Population 2019` = Population_2019, `Superficie (km²)` = Superficie_km2,
                    `Densité` = Densite
                ) %>%
                arrange(Commune)
        }

        datatable(data,
            options = list(
                pageLength = 15,
                language = list(
                    search = "🔍 Rechercher :",
                    lengthMenu = "Afficher _MENU_ entrées",
                    info = "Affichage de _START_ à _END_ sur _TOTAL_ entrées",
                    paginate = list(previous = "◀ Précédent", `next` = "Suivant ▶")
                ),
                dom = "lfrtip"
            ),
            rownames = FALSE,
            class = "cell-border stripe hover"
        ) %>%
            formatRound(
                columns = c("Population 2019", "Superficie (km²)", "Densité"),
                digits = 0, mark = " "
            )
    })

    # Graphique Population
    output$chart_pop <- renderPlotly({
        data <- current_data() %>%
            st_drop_geometry() %>%
            arrange(desc(Population_2019)) %>%
            head(15)

        name_col <- switch(input$niveau,
            "regions" = "region_new",
            "provinces" = "province_new",
            "communes" = "NAME_3"
        )

        plot_ly(data,
            x = ~ reorder(get(name_col), Population_2019),
            y = ~Population_2019,
            type = "bar",
            marker = list(
                color = "#1e3a5f",
                line = list(color = "#152a45", width = 1)
            )
        ) %>%
            layout(
                title = list(
                    text = paste("<b>Top 15 -", switch(input$niveau,
                        "regions" = "Régions",
                        "provinces" = "Provinces",
                        "communes" = "Communes"
                    ), "par population</b>"),
                    font = list(size = 14, color = "#1e3a5f")
                ),
                xaxis = list(title = "", tickangle = -45, tickfont = list(size = 10)),
                yaxis = list(title = "Population (2019)", tickfont = list(size = 10)),
                margin = list(b = 120),
                plot_bgcolor = "rgba(0,0,0,0)",
                paper_bgcolor = "rgba(0,0,0,0)"
            )
    })

    # Graphique Densité
    output$chart_density <- renderPlotly({
        data <- current_data() %>%
            st_drop_geometry() %>%
            arrange(desc(Densite)) %>%
            head(15)

        name_col <- switch(input$niveau,
            "regions" = "region_new",
            "provinces" = "province_new",
            "communes" = "NAME_3"
        )

        plot_ly(data,
            x = ~ reorder(get(name_col), Densite),
            y = ~Densite,
            type = "bar",
            marker = list(
                color = "#c65d07",
                line = list(color = "#a04d05", width = 1)
            )
        ) %>%
            layout(
                title = list(
                    text = paste("<b>Top 15 -", switch(input$niveau,
                        "regions" = "Régions",
                        "provinces" = "Provinces",
                        "communes" = "Communes"
                    ), "par densité</b>"),
                    font = list(size = 14, color = "#c65d07")
                ),
                xaxis = list(title = "", tickangle = -45, tickfont = list(size = 10)),
                yaxis = list(title = "Densité (hab/km²)", tickfont = list(size = 10)),
                margin = list(b = 120),
                plot_bgcolor = "rgba(0,0,0,0)",
                paper_bgcolor = "rgba(0,0,0,0)"
            )
    })

    # Navigation breadcrumb
    observeEvent(input$goto_burkina, {
        updateSelectInput(session, "niveau", selected = "regions")
        updateSelectInput(session, "region_filter", selected = "all")
        updateSelectInput(session, "province_filter", selected = "all")
    })

    observeEvent(input$goto_region, {
        updateSelectInput(session, "niveau", selected = "provinces")
        updateSelectInput(session, "province_filter", selected = "all")
    })
}

# =============================================================================
# Lancer l'application
# =============================================================================

shinyApp(ui = ui, server = server)
