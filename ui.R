

shinyUI(
  dashboardPagePlus(
    enable_preloader = F,
    dashboardHeaderPlus(
      # titlePanel(title=div(img(src="graffiti.PNG"), "")),
      # title = "Graffiti",
      title = div(img(src="graffiti.PNG", width="70%"), ""),
                        titleWidth = 285,
                        enable_rightsidebar = FALSE,
                        left_menu = tagList(
                          tags$style(type='text/css', ".selectize-dropdown-content { max-height: 600px; }"),

                          conditionalPanel(
                            condition = "input.tabs_menu != '9' && input.tabs_menu != '10'",
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
                          )
                          ),
                          conditionalPanel(
                            condition = "input.tabs_menu != '9' && input.tabs_menu != '10'",
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
                            condition = "input.tabs_menu != '9' && input.tabs_menu != '10'",
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
                                 
                                 menuItem("Graphiques INSEE", tabName = "10",
                                          icon = icon("desktop"),startExpanded = F),
                                 

                                 conditionalPanel(
                                   condition = "input.tabs_menu != '10'",
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
                                 ), #cond panel 8
                               conditionalPanel(
                                 condition = "input.tabs_menu == '10'",
                                 fluidRow(
                                   tags$style(type = "text/css", "#downloadData2 {color: black;}"),
                                   column(12, align = "center", offset = 0,
                                          downloadButton("downloadData2",
                                                         label = "Donn\u00E9es du graphique")
                                   )
                                 ),
                                 # fluidRow(
                                 #   column(12, align = "center", offset = 0,
                                 #          div(style="display: inline-block;vertical-align:bottom; width: 250px;",
                                 #              uiOutput("slides"))
                                 #   )
                                 # )
                                 # ,fluidRow(
                                 #   tags$style(type = "text/css", "#downloadSlides {color: black;}"),
                                 #   column(12, align = "center", offset = 0,
                                 #          downloadButton("downloadSlides", label = "T\u00E9l\u00E9charger la pr\u00E9sentaion")
                                 #   )
                                 # ),
                                 fluidRow(
                                   column(12, align = "center", offset = 0,
                                          actionBttn(
                                            inputId = "new_plot",
                                            label = "Nouveau graphique!",
                                            style = "gradient",
                                            color = "succes",
                                            icon = icon("sync")
                                          )
                                   )),
                                 fluidRow(
                                   column(12, align = "center", offset = 0,
                                          actionButton(inputId = "deselect_idbank_in_list",
                                                       label = "D\u00E9s\u00E9lectionner toutes les s\u00E9ries")
                                   )
                                 ),
                                 fluidRow(
                                   column(12, align = "center", offset = 0,
                                          switchInput(
                                            inputId = "interactive_plot",
                                            label = "Graphique int\U00E9ractif",
                                            labelWidth = "150px",
                                            onStatus = "success",
                                            offStatus = "danger"
                                          )
                                   )
                                 ),
                                 fluidRow(
                                   column(12, align = "center", offset = 0,
                                          switchInput(
                                            inputId = "one_plot",
                                            label = "Toutes les courbes sur un seul graphique",
                                            labelWidth = "150px",
                                            onStatus = "success",
                                            offStatus = "danger"
                                          )
                                   )
                                 ),
                                 fluidRow(
                                   column(12, align = "center", offset = 0,
                                          uiOutput("slider_period")
                                   )
                                 ),
                                 fluidRow(
                                   column(12,align = "left", offset = 0,
                                          uiOutput("growth_button")
                                   )
                                 )
                                 
                               )
                     )
    ),
    ## Body content
    dashboardBody(
    #   tags$head(tags$style(HTML('
    #   .main-header .logo {
    #     font-family: "Georgia", Times, "Times New Roman", serif;
    #     font-weight: bold;
    #     font-size: 24px;
    #   }
    # '))),
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
        ),
        tabItem(tabName = "10",
                fluidRow(
                  column(9,
                         # tags$style(type = "text/css", ".dataset_picker {background-color:white}"),
                         uiOutput("dataset_picker")
                  )
                  ,column(3,
                          uiOutput("idbank_picker")
                  )
                ),
                fluidRow(
                  uiOutput("dims")),
                tags$style(
                  '#idbank_list {cursor: pointer}'
                ),
                fluidRow(
                  uiOutput("list_tab2"))
        )
      )
    )
  ))


