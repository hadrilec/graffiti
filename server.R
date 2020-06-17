shinyServer(function(input,output,session) {
  
  DB_variables_react <- reactive({
    DB_variables %>% 
      arrange(perim)
  })
  
  link_code_file <- reactiveVal()
  
  gg_react <- reactiveValues()
  values <- reactiveValues(df_data = NULL)
  
  var_name <- reactiveVal()
  
  file_gg  <- reactiveVal()
  update_plot  <- reactiveVal()
  update_plot_finished  <- reactiveVal()
  run_time_plot  <- reactiveVal()
  DB_plot_cahier <- reactiveVal()
  code_related_title <- reactiveVal()
  page_code = reactiveVal()
  
  observeEvent({var_name()},{
    
    Print(var_name())
    
    if(input$select_title != ""){
      if(!is.null(var_name())){
        updateSelectizeInput(session, "cahier", 
                             choices = c(var_name(), input$cahier), selected = input$cahier)
      }
    }
  })
  
  # 
  # ajout bouton MAJ graphique si disponible ####
  # 
  observe({

    render_update_button = FALSE

    if(!is.null(update_plot())){
      if(update_plot()){
        if(!is.null(link_code_file())){
          if(file.exists(link_code_file())){
            render_update_button = TRUE
          }
        }
      }
    }
    if(input$select_title == ""){
      render_update_button = FALSE
    }

    if(render_update_button){
      output$MAJ_plot <- renderUI({
        actionButton("MAJ_plot", label = "MAJ du graphique", icon("refresh"))
      })
    }else{
      output$MAJ_plot <- renderUI({NULL})
    }
    Print(render_update_button)
  })
  
  # 
  # declenchement MAJ du graphique ####
  # 
  
  observeEvent({input$MAJ_plot},{
    
    Print(input$MAJ_plot)
    Print(update_plot())
    Print(link_code_file())
    
    if(!is.null(update_plot())){
      if(update_plot()){
        if(!is.null(link_code_file())){
          
          print("plot update triggered ")
          Print(link_code_file())
          
          run_time_plot_final = 30
          
          if(!is.null(run_time_plot())){
            if(is.numeric(run_time_plot())){
              run_time_plot_final = round(run_time_plot()) + 1
            }
          }
          
          withProgress(message = 'MAJ du graphique', detail = "",{
                         # N = 60
                         for(i in 1:run_time_plot_final){
                           
                           # Long Running Task
                           Sys.sleep(1)
                           
                           # Update progress
                           incProgress(1/run_time_plot_final,
                                       detail = paste(round(i/run_time_plot_final*100), "%"))
                         }
                         
                         source_plot = try(source(link_code_file(), encoding = "utf-8"))
                       })
          
         if(class(source_plot) != 'try-error'){
           
           print('update completed')
           
           update_plot_finished(sample(1:100))  
           
  
         }
        }
      }
    }
    
  })
  
  
  # 
  # MAJ des variables et titres proposés en fonction du périmètre indiqué ####
  # 
  observeEvent({input$select_perim},{
    
    perim <- input$select_perim
    
    Print(input$select_perim)
    
    if(!is.null(perim)){
      if (perim != ""){
        
        title_select = 
          DB_variables_react() %>% 
          filter(perim %in% c(input$select_perim)) %>% 
          pull(title) %>% 
          unique()
        
        if(!input$select_title %in% title_select){
          title_selected = NULL
        }else{
          title_selected = input$select_title
        }
        
        updateSelectizeInput(session, "select_title", 
                             choices = title_select, 
                             selected = title_selected)
       
        
      }else{
        print("select_title null")
        updateSelectizeInput(session, "select_title", choices = "")
      }
    }else{
      print("select_title null")
      updateSelectizeInput(session, "select_title", choices = "")
    }
   
  })
  
  # 
  # MAJ du titre en fonction de la variable indiquée et inversement ####
  #
  dico_table_react <- reactiveVal()
  
  observe({
    
    dico_table = DB_variables_react() %>%
      select(perim, var, title) %>%
      dplyr::rename(`Périmètre` = perim,
                    Variable = var,
                    Titre = title) %>% 
      as.data.frame() 
    
      dico_table = dico_table %>% 
        DT::datatable(filter = "top", selection = "single") %>%
        DT::formatStyle(0, lineHeight = '10px', target= 'row') 
    
    
    dico_table_react(dico_table)
    
    output$dico_home_page <- DT::renderDT(dico_table_react()) 
  })
  
  observeEvent({
    input$dico_home_page_cell_clicked
  },{
    
    row_selected = input$dico_home_page_cell_clicked$row[1]
    
    Print(row_selected)

    if(!is.null(row_selected)){
  
        title_selected = 
          DB_variables_react() %>%
          slice(row_selected) %>%
          pull(title)

          perim_selected =
            DB_variables_react() %>%
            slice(row_selected) %>%
            pull(perim)

          title_choices =
            DB_variables_react() %>%
            filter(perim == perim_selected) %>%
            pull(title)

          updateSelectizeInput(session, "select_title",
                               choices = title_choices,
                               selected = title_selected)

          updatePickerInput(
            session,
            "select_perim",
            label = NULL,
            selected = perim_selected,
            choices = countries,
            choicesOpt = list(content = icons_perims)
          )
      

    }
  })
  
  observeEvent({
    input$select_title
    update_plot_finished()
  },
  ignoreNULL = FALSE, ignoreInit = FALSE,
  handlerExpr = {
    print(" ")
    print("start  ")
    
    Print(input$tabs)
    Print(input$select_perim)
    Print(input$select_title)
    
    perim <- input$select_perim
      
      list_tab2 = list()
      
  
      if(input$select_perim != ""){
      if(input$select_title != ""){
        
        var = DB_variables_react() %>% 
          dplyr::filter(title %in% c(input$select_title)) %>% 
          filter(perim %in% c(input$select_perim)) %>% 
          slice(1) %>% 
          pull(var) %>% 
          unique() 
      
        
        # 
        # chargement du graphique correspondant au tab selectionné ####
        # 
        Print(var)
        
        
        pattern_searched = paste0(c("gg_plot", "gg_html"), ".rds", collapse = "|")
        path_var = file.path(link_results, perim, var)
        
          list_file_var = file.path(path_var, list.files(path_var, pattern = pattern_searched))
          list_file_pdf = file.path(path_var, list.files(path_var, pattern = "pdf"))
        
          file_to_load = list_file_var[1]     
          
             # 
             # recherche du pdf correspondant au graphique ####
             # 
             if(length(list_file_pdf) > 0){
               file_gg(list_file_pdf[1])
             }else{
               file_gg(NULL)
             }
          
             # 
             # chargement du graphique ####
             # 
          
             Print(file_to_load)
           
              gg = readRDS(file_to_load)
              
              var_ly = paste0(var, "_ly")
              if(input$interact_plot == TRUE){
                if("ggplot" %in% class(gg)){
                  gg_react[[var_ly]] = plotly::ggplotly(gg)
                }else{
                  gg_react[[var_ly]] = gg
                }
              }
            
              Print(names(reactiveValuesToList(gg_react)))
              
              link_code_file_ = gg$link_code_file
              link_code_file_ = gsub(link_app, ".", link_code_file_)
              
              var_name(var)
              link_code_file(link_code_file_)
              update_plot(gg$update)
              run_time_plot(gg$run_time)
              # 
              # recherche des donnees dans les graphs
              # emplacement peut varier en fonction du graphique
              # 
              if(length(gg$data) != 0){
                values$df_data <- gg$data
              }else{
                if (length(gg$layers) >= 2) {
                  if (!is.null(gg$layers[[1]]$data) & !is.null(gg$layers[[2]]$data)) {
                    values$df_data <-
                      plyr::rbind.fill(gg$layers[[1]]$data, gg$layers[[2]]$data)
                    
                  }
                }
              }
              
              # 
              # mise a jour du graphique a l ecran 
              # 
              
              var_gg = paste0(var, "_gg")
              var_ly_gg = paste0(var_ly, "_gg")
              # output[[var_gg]] <- renderPlot({gg_react[[var]]})
              
              # 
              # creation d'un onglet/tab avec le graphique 
              # 
              Print(var)
              Print(class(gg))
              
              if("ggplot" %in% class(gg)){
                
                output[[var_gg]] <- renderPlot({gg_react[[var]]})
                
                output[[var_ly_gg]] <- renderPlotly({gg_react[[var_ly]]})
                
                if(input$interact_plot == FALSE){
                  list_tab2[[length(list_tab2)+1]] = tabPanel(title = "Graphique",
                                                              plotOutput(var_gg,
                                                                         width = "100%"
                                                                         , height = "80vh"
                                                              ))
                }else{
                  list_tab2[[length(list_tab2)+1]] = tabPanel(title = "Graphique",
                                                              plotlyOutput(var_ly_gg,
                                                                           width = "100%"
                                                                           , height = "80vh"
                                                              ))
                }
                
              }else if("highchart" %in% class(gg)){
                
                output[[var_gg]] <- renderHighchart({gg_react[[var]]})
                
                list_tab2[[length(list_tab2)+1]] = tabPanel(title = "Graphique",
                                                            highchartOutput(var_gg,
                                                                            width = "100%"
                                                                            , height = "80vh"
                                                            ))
              }
              
              
                
                list_file_code = file.path(path_var, list.files(path_var, pattern = "code.html"))
                page_code(list_file_code[1])
                
                Print(list_file_code)
                
                
                if(length(list_file_code) > 0){
                  
                  code_file = list_file_code[1]
                  list_tab2[[length(list_tab2)+1]] = tabPanel(title = "Code",
                                                              box(
                                                                width = "100%",
                                                                includeHTML(list_file_code[1])
                                                              ))

                }
                
      }
      }
      
      list_tab2[[length(list_tab2)+1]] =
        tabPanel(title = "Catalogue",
                 box(
                   width = "100%", height = "75vh",
                   
                   DT::dataTableOutput("dico_home_page",
                                       width = "100%"
                                       , height = "75vh"
                   ) 
                   
                   ))
        
        output$list_tab <- renderUI({
          do.call(tabsetPanel, c(list_tab2, id = 'tabs'))
        })
      
      

      print("end  ")

      })

  observe({
    
    # 
    # données pour le bouton téléchargement ####
    # 
    
    if(exists("values")){
      
      data_ <- values$df_data 
      if(!is.null(data_)){
        if(all(names(data_) %in% c("time", "value", "label"))){
          data_ = data_ %>% 
            tidyr::pivot_wider(names_from = "label", values_from = "value")
        }
      }
     
      
      if(exists("file_gg")){
        file_gg_ = file_gg()
      }else{
        file_gg_ = NULL
      }
      
      if(!is.null(data_)){
        if(any(class(data_) == "data.frame")){
          output$downloadData <- downloadHandler(
            filename = function() {
              if(!is.null(file_gg_)){
                file_gg_name = basename(file_gg_)
                paste("data_", gsub(".pdf","", file_gg_name),"_", gsub("-|:| |CET","", Sys.time()), ".xlsx", sep="")
              }else{
                paste("data_", gsub("-|:| |CET","", Sys.time()), ".xslx", sep="")
              }
            },
            content = function(file) {
              openxlsx::write.xlsx(data_, file)
            }
          )
        }
      }
    }
    
    # 
    # graphique pour le bouton téléchargement ####
    # 
    if(exists("file_gg")){
      if(!is.null(file_gg_)){
        file_gg_ = file_gg()
        
          output$downloadPlot <- downloadHandler(
            filename = function() {
              file_gg_name = basename(file_gg_)
              paste("plot_", gsub(".pdf","", file_gg_name),"_", gsub("-|:| |CET","", Sys.time()), ".pdf", sep="")
            },
            content = function(file) {
              file.copy(file_gg_, file)
            }
          )
        
      }
    }
  })
  
  # 
  # création du cahier ####
  # 
  
  observe({
    list_graph_cahier = lapply(input$cahier, function(x) gg_react[[x]])
    
    output$downloadCahier <- downloadHandler(
      filename = function() {
        paste("cahier_", gsub("-|:| |CET","", Sys.time()), ".pdf", sep="")
      },
      content = function(file) {
        rmarkdown::render(input = cahier_file,
                          output_file = file,
                          params = list(list_graph = list_graph_cahier))
      }
    )
    
  })

}
)