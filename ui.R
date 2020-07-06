

shinyUI(
  dashboardPagePlus(
    enable_preloader = F,
    dashboardHeaderPlus(title = "DataViz",
                        titleWidth = 285,
                        enable_rightsidebar = FALSE,
                        left_menu = tagList(
                          tags$style(type='text/css', ".selectize-dropdown-content { max-height: 600px; }"),
                          # actionButton("home_button", label = NULL, icon("home")),
                          # selectizeInput(
                          #   'select_perim',
                          #   label = NULL,
                          #   width = "200px",
                          #   choices = perimetre_list,
                          #   options = list(
                          #     placeholder = 'Sélectionner un périmètre',
                          #     onInitialize = I('function() { this.setValue(""); }')
                          #   )
                          # ) ,
                       #    tags$head(tags$style("
                       # .jhr{
                       # display: inline;
                       # vertical-align: middle;
                       # padding-left: 10px;
                       # }")),
                       # tags$style(HTML(".selected {background-color:blue !important;}")),
                    
                          pickerInput("select_perim",
                                      label = NULL,
                                      choices = countries,
                                      width = 200,
                                      selected = NULL,
                                      multiple = FALSE,
                                      options = pickerOptions(
                                        maxOptions = 1,
                                                              # actionsBox = TRUE,
                                                              # deselectAllText = "Tout déselectionner",
                                                              # selectAllText = "Tout sélectionner",
                                                              title = 'Sélectionner un périmètre'),
                                      choicesOpt = list(content = icons_perims)
                          ),
                          # pickerInput("select_perim",
                          #             label = NULL,
                          #             choices = countries,
                          #             width = 200,
                          #             selected = NULL,
                          #             multiple = TRUE,
                          #             options = pickerOptions(maxOptions = 1,
                          #                                     title = 'Sélectionner un périmètre'),
                          #             choicesOpt = list(content = icons_perim)
                          # ),

                          # selectizeInput(
                          #   'select_variable',
                          #   label = NULL,
                          #   width = "220px",
                          #   choices = NULL,
                          #   options = list(
                          #     placeholder = 'Sélectionner une variable',
                          #     onInitialize = I('function() { this.setValue(""); }')
                          #   )
                          # ),
                          selectizeInput(
                            'select_title', 
                            size = 30,
                            label = NULL,
                            width = "600px",
                            choices = NULL,
                            options = list(
                              placeholder = 'Sélectionner un titre',
                              onInitialize = I('function() { this.setValue(""); }')
                            )
                          )
                          ,actionButton("MAJ_dico", label = "MAJ du dictionnaire", icon("refresh"))
                        )
                        ),
    
    dashboardSidebar(width=285,
                     sidebarMenu(id = "tabs_menu",
                                 menuItem("Graphiques", tabName = "8",
                                          icon = icon("fas fa-sitemap"),startExpanded = F),
                                 
                                 menuItem("Graphiques2", tabName = "9",
                                          icon = icon("fas fa-sitemap"),startExpanded = F),
                                 # fluidRow(
                                 # materialSwitch(
                                 #   inputId = "interactive_plot",
                                 #   label = "Primary", 
                                 #   value = FALSE,
                                 #   status = "primary"
                                 # )
                                 # ),
                               fluidRow(
                                 tags$style(type = "text/css", "#downloadData {color: black; margin-left:75px;}"),
                                 downloadButton("downloadData", label = "Télécharger les données")
                               ),
                               
                               materialSwitch(
                                 inputId = "interact_plot",
                                 label = "Graphique intéractif", 
                                 value = FALSE,
                                 status = "primary"
                               ),
                               # fluidRow(
                               #   tags$style(type = "text/css", "#downloadPlot {color: black; margin-left:75px;}"),
                               #   downloadButton("downloadPlot", label = "Télécharger le graphique")
                               # ),
                             
                               fluidRow(
                                 uiOutput("MAJ_plot"),
                                 # actionButton("MAJ_plot", label = "MAJ du graphique", icon("refresh")),
                                 tags$style(type = "text/css", "#MAJ_plot {color: black; margin-left:45px;}")
                               ),
                               fluidRow(
                                 tags$style(type = "text/css", "#cahier {display: inline-block;color: black; margin-left:45px;}"),
                                 selectizeInput(
                                   'cahier', 
                                   label = "Créer une présentation",
                                   size = 30,
                                   width = "300px",
                                   multiple = TRUE,
                                   choices = NULL,
                                   options = list(
                                     'plugins' = list('remove_button'),
                                     'create' = TRUE,
                                     'persist' = FALSE,
                                     placeholder = 'Ajouter un graphique',
                                     onInitialize = I('function() { this.setValue(""); }')
                                   )
                                 )
                               ),
                               fluidRow(
                                 tags$style(type = "text/css", "#downloadCahier {color: black; margin-left:75px;}"),
                                 downloadButton("downloadCahier", label = "Télécharger la présentation")
                               )
                     )
    ),
    ## Body content
    dashboardBody(
      # fluidRow()
      # useShinyjs(),
      
      # tags$head(tags$style(
      #   type="text/css",
      #   "#Image img {max-width: 100%; width: 100%; height: auto}"
      # )),
      
      tabItems(
        tabItem(tabName = "8",
             fluidRow(
               fillCol(
                 fillRow(uiOutput("list_tab")),
                 flag("fr", size = 0)
               )
               )
        )
      )
    )
  ))


