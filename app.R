library(shiny)
library(dplyr)
library(stringr)
library(readr)
library(sortable)
library(DT)

# --- Données ---
df <- read.csv("data/affectation_formatee.csv", stringsAsFactors = FALSE)
avis <- read.csv("data/avis_de_stage_clean_v2.csv", stringsAsFactors = FALSE)

df <- df %>% mutate(stage_clean = str_to_lower(stage))

# --- UI ---
ui <- navbarPage("🩺 Stage UP",
                 
                 tabPanel("🔍 Trouver ses costagiaires",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("pole", "Pôle :", choices = sort(unique(df$pole))),
                              selectInput("trimestre", "Trimestre :", choices = sort(unique(df$trimestre))),
                              uiOutput("matiere_ui"),
                              uiOutput("stage_ui")
                            ),
                            mainPanel(
                              h4("👥 Co-stagiaires :"),
                              verbatimTextOutput("resultats")
                            )
                          )
                 ),
                 
                 tabPanel("🌟 Choisir son stage",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("pole_choix", "Choisir un pôle :", choices = sort(unique(avis$pole))),
                              uiOutput("matiere_rank"),
                              selectizeInput("hopitaux_pref", "Hôpitaux préférés :", choices = NULL, multiple = TRUE),
                              actionButton("lancer_match", "🌟 Voir les recommandations")
                            ),
                            mainPanel(
                              h4("🔹 Stages recommandés :"),
                              uiOutput("stages_recommandes")
                            )
                          )
                 ),
                 
                 tabPanel("🏆 Meilleurs stages par matière",
                          sidebarLayout(
                            sidebarPanel(
                              checkboxGroupInput("filtre_pole", "Filtrer par pôle :", choices = sort(unique(avis$pole)), selected = unique(avis$pole)),
                              sliderInput("nb_stages", "Nombre de stages par matière :", min = 1, max = 10, value = 3),
                              checkboxInput("filtre_garde", "Afficher uniquement les stages avec gardes", value = FALSE)
                            ),
                            mainPanel(uiOutput("best_stage_blocs"))
                          )
                 ),
                 
                 tabPanel("📋 Tous les stages",
                          fluidPage(
                            h4("📊 Liste de tous les stages"),
                            DT::dataTableOutput("table_stages")
                          )
                 )
)

# --- Serveur ---
server <- function(input, output, session) {
  
  output$matiere_ui <- renderUI({
    req(input$pole)
    selectInput("matiere", "Matière :", choices = sort(unique(df$matiere[df$pole == input$pole])))
  })
  
  output$stage_ui <- renderUI({
    req(input$pole, input$matiere)
    stages <- df %>%
      filter(pole == input$pole, matiere == input$matiere) %>%
      mutate(label = paste(hopital, "-", stage)) %>%
      pull(label) %>% unique() %>% sort()
    selectInput("stage", "Stage :", choices = stages)
  })
  
  output$resultats <- renderText({
    req(input$pole, input$matiere, input$stage, input$trimestre)
    hopital <- str_split(input$stage, " - ", simplify = TRUE)[1]
    stage_nom <- str_split(input$stage, " - ", simplify = TRUE)[2]
    noms <- df %>%
      filter(
        pole == input$pole, matiere == input$matiere,
        hopital == hopital, stage == stage_nom,
        trimestre == input$trimestre
      ) %>%
      arrange(etudiant) %>% pull(etudiant)
    if (length(noms) == 0) return("Aucun co-stagiaire trouvé.")
    paste(noms, collapse = "\n")
  })
  
  observe({
    updateSelectizeInput(session, "hopitaux_pref", choices = sort(unique(avis$hopital)), server = TRUE)
  })
  
  output$matiere_rank <- renderUI({
    matieres <- sort(unique(avis$matiere[avis$pole == input$pole_choix]))
    bucket_list("Classez vos matières préférées :", "matiere_rank", add_rank_list("Préférées", labels = matieres))
  })
  
  observeEvent(input$lancer_match, {
    req(input$pole_choix, input$hopitaux_pref, input$matiere_rank)
    ordre <- input$matiere_rank[[1]]
    
    resultats <- avis %>%
      filter(pole == input$pole_choix, hopital %in% input$hopitaux_pref, matiere %in% ordre) %>%
      group_by(service, hopital, chef) %>%
      summarise(
        moyenne = round(mean(note, na.rm = TRUE), 1),
        n = n(),
        garde = mean(garde, na.rm = TRUE),
        ecos = first(ECOS),
        avis_text = paste(avis, collapse = "\n\n---\n\n"),
        .groups = "drop"
      ) %>%
      mutate(prio = match(matiere, ordre)) %>%
      arrange(prio, desc(moyenne)) %>% head(10)
    
    output$stages_recommandes <- renderUI({
      tagList(lapply(1:nrow(resultats), function(i) {
        tags$div(
          tags$h5(paste0("📌 ", resultats$service[i], " — ", resultats$hopital[i])),
          tags$p(strong(toupper(resultats$chef[i]))),
          tags$p(paste0(resultats$moyenne[i], "/20 (n=", resultats$n[i], ")")),
          tags$p(paste0("Gardes : ", round(resultats$garde[i], 1))),
          tags$p(paste0("ECOS : ", resultats$ecos[i])),
          tags$details(tags$summary("📝 Lire les avis"), tags$pre(style="white-space:pre-wrap;", resultats$avis_text[i])),
          tags$hr()
        )
      }))
    })
  })
  
  output$best_stage_blocs <- renderUI({
    req(input$filtre_pole)
    data <- avis %>%
      filter(pole %in% input$filtre_pole) %>%
      group_by(matiere, pole, service, hopital, chef) %>%
      summarise(
        moy = round(mean(note, na.rm = TRUE), 1),
        n = n(),
        garde = mean(garde, na.rm = TRUE),
        ecos = first(ECOS),
        avis_txt = paste(avis, collapse = "\n\n---\n\n"),
        .groups = "drop"
      )
    
    if (input$filtre_garde) {
      data <- data %>% filter(garde > 0)
    }
    
    data <- data %>%
      arrange(matiere, desc(moy)) %>%
      group_by(matiere) %>%
      slice_head(n = input$nb_stages)
    
    palette <- c("Pole 1" = "rgba(255,0,0,0.15)", "Pole 2" = "rgba(0,128,0,0.15)", "Pole 3" = "rgba(255,165,0,0.15)")
    
    blocs <- lapply(unique(data$matiere), function(mat) {
      items <- data %>% filter(matiere == mat)
      cols <- lapply(1:nrow(items), function(i) {
        tags$div(
          style = paste0("background-color:", palette[items$pole[i]], "; padding: 10px; margin-bottom: 5px; border-radius: 10px;"),
          tags$strong(paste0(items$service[i], " — ", items$hopital[i])), tags$br(),
          toupper(items$chef[i]), tags$br(),
          paste0(items$moy[i], "/20 (n=", items$n[i], ")"), tags$br(),
          paste0("Gardes : ", round(items$garde[i], 1)), tags$br(),
          paste0("ECOS : ", items$ecos[i]), tags$br(),
          tags$details(tags$summary("📝 Avis"), tags$pre(style = "white-space:pre-wrap;", items$avis_txt[i]))
        )
      })
      tags$div(
        style = "margin: 20px; border-radius: 10px; border: 2px solid #ccc; padding: 10px;",
        tags$div(style = "background-color: #333; color: white; padding: 6px; border-radius: 10px; font-weight: bold;", mat),
        do.call(tagList, cols)
      )
    })
    
    rows <- split(blocs, ceiling(seq_along(blocs)/3))
    lapply(rows, function(row) fluidRow(lapply(row, function(col) column(width = 4, col))))
  })
  
  output$table_stages <- DT::renderDataTable({
    avis %>%
      select(pole, hopital, service, chef, note, garde, ECOS, matiere) %>%
      DT::datatable(
        options = list(pageLength = 20, autoWidth = TRUE),
        rownames = FALSE
      )
  })
}

shinylive::export("mon_app")
