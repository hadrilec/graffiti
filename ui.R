

shinyUI(
  dashboardPagePlus(
    enable_preloader = F,
    dashboardHeaderPlus(title = "DataViz",
                        titleWidth = 285,
                        enable_rightsidebar = FALSE,
                        left_menu = tagList(
                       
                           
                          
                          tags$style(type='text/css', ".selectize-dropdown-content { max-height: 600px; }"),
                          
                          conditionalPanel(
                            condition = "input.tabs_menu != '9'",
                          pickerInput("select_perim",
                                      label = NULL,
                                      choices = countries,
                                      width = 200,
                                      selected = NULL,
                                      multiple = FALSE,
                                      options = pickerOptions(
                                        maxOptions = 1,
                                        title = 'Sélectionner un périmètre'),
                                      choicesOpt = list(content = icons_perims)
                          )),
                          conditionalPanel(
                            condition = "input.tabs_menu != '9'",
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
                          ))
                          , conditionalPanel(
                            condition = "input.tabs_menu != '9'",
                            actionButton("MAJ_dico", label = "MAJ du catalogue", icon("refresh")))
                          
                        # )#cond panel 8
                        )
                        ),
    
    dashboardSidebar(width=285,
                     sidebarMenu(id = "tabs_menu",
                                 menuItem("Graphiques International", tabName = "8",
                                          icon = icon("chart-area"),startExpanded = F),
                                 
                                 menuItem("Graphiques France", tabName = "7",
                                          icon = icon("chart-area"),startExpanded = F),
                                 
                                 menuItem("Tableau de bord", tabName = "9",
                                          icon = icon("desktop"),startExpanded = F),
                                
                                 conditionalPanel(
                                   condition = "input.tabs_menu != '9'",
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
                           
                               fluidRow(
                                 uiOutput("MAJ_plot"),
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
                                 ) #cond panel 8
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
        ),
        tabItem(tabName = "9",
                fluidRow(
                  fillCol(
                      fillRow(uiOutput("slide_show"))
                  )
                )
        )
      )
    )
  ))


