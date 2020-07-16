shinyServer(function(input, output, session) {
  
  observe({Print(input$tabs_menu)})
  
  
  output$slide_show <- renderUI({
    if("slickR" %in% pkg[,1]){
      
      gg1 = try(s3read_using(FUN = readRDS,
                            object = "dataviz/FR/pollution_idf/pollution_idf_gg_plot",
                            bucket = "groupe-1360",
                            opts = list("use_https" = F, "region" = "")), silent = T)
      
      gg2 = try(s3read_using(FUN = readRDS,
                            object = "dataviz/FI/epargne_fr/epargne_fr_gg_plot",
                            bucket = "groupe-1360",
                            opts = list("use_https" = F, "region" = "")), silent = T)
      
      slickR::slickR(obj = list(gg1, gg2), height = 100, width = "95%") + 
        settings(dots = TRUE, autoplay = TRUE)
    }
  })
  
  DB_variables_react <- reactive({
    Print(input$MAJ_dico)
    DB_variable = update_DB_variable()
    DB_variable %>% 
      arrange(perim)
  })
  
  DB_minio_all <- reactive({
    Print(input$MAJ_dico)
    DB_minio = unlist(get_minio_all())
  })
  
  perimeter_selected <- reactive({input$select_perim})
  
  link_code_file <- reactiveVal()
  
  gg_react <- reactiveValues()
  values <- reactiveValues(df_data = NULL)
  
  var_name <- reactiveVal()
  
  file_gg  <- reactiveVal()
  current_plot  <- reactiveVal()
  update_plot  <- reactiveVal()
  update_plot_finished  <- reactiveVal()
  run_time_plot  <- reactiveVal()
  DB_plot_cahier <- reactiveVal()
  code_related_title <- reactiveVal()
  page_code = reactiveVal()
  
  
  
  observeEvent({var_name()},{
     
    Print(var_name())
    
    gg = current_plot()
    
    if(input$select_title != ""){
      if(!is.null(var_name())){
        if("ggplot" %in% class(gg)){
          updateSelectizeInput(session, "cahier", 
                               choices = c(var_name(), input$cahier), selected = input$cahier)
        }else{
          updateSelectizeInput(session, "cahier", 
                               choices = c(input$cahier), selected = input$cahier)
        }
       
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

    Print(update_plot())
    Print(link_code_file())
    Print(render_update_button)
    
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
        # if(!is.null(link_code_file())){
          file_code = file.path("dataviz", perimeter_selected(), var_name(), paste0(var_name(), "_code"))
          Print(file_code)
          if(file_code %in% DB_minio_all()){
            print("plot update triggered ")
            # Print(link_code_file())
            
            # 
            # download code file
            # 
            
            file_dwn = tempfile()
            
            aws.s3::save_object(file_code,
                                file = file_dwn,
                                bucket = "groupe-1360", use_https = F, region = "")
            
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
              # 
              # execute code file
              # 
              
              source_plot = try(source(file_dwn, encoding = "utf-8"))
            })
            
            if(class(source_plot) != 'try-error'){
              
              print('update completed')
              
              update_plot_finished(sample(1:100))  
              
              
            }
          }
         
        # }
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
    input$interact_plot
    update_plot_finished()
  },
  ignoreNULL = FALSE, ignoreInit = FALSE,
  handlerExpr = {
    cat(" ", file = stderr())
    
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
        
        minio_file_path = 
          DB_variables_react() %>% 
          dplyr::filter(title %in% c(input$select_title)) %>% 
          dplyr::filter(perim %in% c(input$select_perim)) %>% 
          slice(1) %>% 
          pull(minio_path) %>% 
          unique() 
        
        # 
        # chargement du graphique correspondant au tab selectionné ####
        # 
        Print(var)
        var_name(var)
        
        # pattern_searched = paste0(c("gg_plot", "gg_html"), ".rds", collapse = "|")
        pattern_searched = paste0(c("gg_plot", "gg_html"), paste0(".", data_format), collapse = "|")
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
           
            # if(!is.na(file_to_load)){
            
              # gg = readRDS(file_to_load)
              
              # 
              # minio_file_path = file.path("dataviz", perim, var, paste0(var, "_gg_plot"))
         
              Print(minio_file_path)
              
              if(stringr::str_detect(minio_file_path, "_gg_plot$")){
                
                gg = try(s3read_using(FUN = readRDS,
                                      object = minio_file_path,
                                      bucket = "groupe-1360",
                                      opts = list("use_https" = F, "region" = "")), silent = T)
                
                if(!"try-error" %in% class(gg)){
                  read_message = "minio"
                }else{
                  load(file_to_load)
                  read_message = "app"
                }
                
                Print(read_message)
                # Print(class(gg))
                
                gg_react[[var]] = gg
                current_plot(gg)
                
                var_ly = paste0(var, "_ly")
                
                Print(input$interact_plot)
                
                get_interactive_plot = FALSE
                
                if(!is.null(input$interact_plot)){
                  if(input$interact_plot == TRUE){
                    if("ggplot" %in% class(gg)){
                      get_interactive_plot = TRUE
                    }
                  }
                }
                if(get_interactive_plot == TRUE){
                  
                  gg_ly =  plotly::ggplotly(gg) %>% 
                    layout(legend = list(orientation = "h",   # show entries horizontally
                                         xanchor = "center",  # use center of legend as anchor
                                         x = 0.5,
                                         yanchor = "bottom",
                                         y = -0.6
                    ))  
                  # subtitle
                  # if(!is.null(gg$labels$subtitle) & !is.null(gg$labels$title)){
                  # subtitle_ly = paste0(gg$labels$title,
                  #                   '<br>', '<sup>',
                  #                   gsub("\\\n", "", gg$labels$subtitle),
                  #                   '</sup>')
                  # subtitle_ly = paste0(gsub("\\\n", "", gg$labels$subtitle))
                  
                  # gg_ly = gg_ly %>% 
                  #   layout(
                  #     # title = list(text = title_ly),
                  #     annotations=list(text = subtitle_ly,
                  #                      xref="paper",
                  #                      x=0.5,
                  #                      yanchor = "bottom",
                  #                      y = -0.2,
                  #                      showarrow=FALSE)
                  #          )
                  # }
                  
                  gg_react[[var_ly]] = gg_ly
                }else{
                  gg_react[[var_ly]] = gg
                }
                
                link_code_file_ = gg$link_code_file
                link_code_file_ = gsub(link_app, ".", link_code_file_)
                
                
                link_code_file(link_code_file_)
                update_plot(gg$update)
                run_time_plot(gg$run_time)
                
                # 
                # mise a jour du graphique a l ecran 
                # 
                
                var_gg = paste0(var, "_gg")
                var_ly_gg = paste0(var_ly, "_gg")
               
                # 
                # creation d'un onglet/tab avec le graphique 
                # 
                Print(var)
                
                
                if("ggplot" %in% class(gg)){
                  
                  Print(class(gg))
                  # output[[var_gg]] <- renderPlot({gg_react[[var]]})
                  output[[var_gg]] <- renderPlot({gg})
                  
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
                  Print(class(gg))
                  
                  
                  
                  if("highcharter" %in% pkg[,1]){
                    
                    output[[var_gg]] <- renderHighchart({gg_react[[var]]})
                    
                    list_tab2[[length(list_tab2)+1]] = tabPanel(title = "Graphique",
                                                                highchartOutput(var_gg,
                                                                                width = "100%"
                                                                                , height = "80vh"
                                                                ))
                  }
                  
                  
                }
                # }file_to_load !is.na
                
              }
              
            
          # 
          # AJOUT DU GRAPHIQUE PNG EXISTANT
          # 
              if(stringr::str_detect(minio_file_path, "_png$|_jpg$")){
                # if(file.exists(var_file)){
                
              
                if(stringr::str_detect(minio_file_path, "_png$")){image_format = "png"}
                if(stringr::str_detect(minio_file_path, "_jpg$")){image_format = "jpg"}
                
                if("magick" %in% pkg[,1]){
                  output$Image <- renderImage({
                    
                    # filename <- normalizePath(var_file)
                    
                    tmpfile <- 
                      get_object(minio_file_path, 
                                 bucket = "groupe-1360",
                                 use_https = F, region = "") %>%
                      magick::image_read() %>%
                      magick::image_write(tempfile(fileext = paste0(".", image_format)),
                                          format = image_format)
                    
                    
                    # Return a list containing the filename and alt text
                    list(src = tmpfile,
                         alt = "test")
                    
                  }, deleteFile = FALSE)
                  
                  list_tab2[[length(list_tab2)+1]] = tabPanel(title = "Graphique",
                                                              box(
                                                                width = "100%",
                                                                imageOutput("Image")
                                                              ))
                }
               
              }
            
              
              list_file_code = file.path(path_var, list.files(path_var, pattern = "code.html"))
              page_code(list_file_code[1])
              
              Print(list_file_code)
              
              minio_file_path_html = file.path("dataviz", perim, var, paste0(var, "_html"))
              
              if(minio_file_path_html %in% DB_minio_all()){
                
                file_dwn = tempfile()
                
                dwn_html = 
                  try(aws.s3::save_object(minio_file_path_html, 
                                          file = file_dwn,
                                          bucket = "groupe-1360", use_https = F, region = ""))
                
                
                if(!"try-error" %in% class(dwn_html)){
                  cat("html from minio", file = stderr())
                  list_tab2[[length(list_tab2)+1]] = tabPanel(title = "Code",
                                                              box(
                                                                width = "100%",
                                                                includeHTML(file_dwn)
                                                              ))
                  
                }
                
              }else{
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
      }
      
      list_tab2[[length(list_tab2)+1]] =
        tabPanel(title = "Catalogue",
                 box(
                   width = "100%", 
                   
                   DT::dataTableOutput("dico_home_page",
                                       width = "100%"
                                       , height = "75vh"
                   ) 
                   
                   ))
        
        output$list_tab <- renderUI({
          do.call(tabsetPanel, c(list_tab2, id = 'tabs'))
        })

      })

  observe({
  
      Print(input$downloadData)
      
    # 
    # données pour le bouton téléchargement ####
    # 
    
      # 
      # recherche des donnees dans les graphs
      # emplacement peut varier en fonction du graphique
      # 
      
      gg = current_plot()
      
      if("ggplot" %in% class(gg)){
        if(length(gg$data) != 0){
          data_current_plot <- gg$data
        }else{
          if (length(gg$layers) >= 2) {
            if (!is.null(gg$layers[[1]]$data) & !is.null(gg$layers[[2]]$data)) {
              data_current_plot <-
                plyr::rbind.fill(gg$layers[[1]]$data, gg$layers[[2]]$data)
              
            }
          }
        }
      }
      if("highchart" %in% class(gg)){
        
        n_series = length(gg[["x"]][["hc_opts"]][["series"]])
        if(n_series > 0){
          serie_id = 1
          list_df = list()
          
          for(serie_id in 1:n_series){
            data_list = gg[["x"]][["hc_opts"]][["series"]][[serie_id]][["data"]]
            
            data_raw_hc = unlist(flatten(data_list))
            
            for(test_id in 2*1:(length(data_list)/2)){
              list_df[[length(list_df)+1]] = data.frame(time = data_raw_hc[test_id-1],
                                                        value = data_raw_hc[test_id],
                                                        series = serie_id)
            }
          }
          
          data_hc = bind_rows(list_df)
          
          data_hc = data_hc %>% 
            mutate(time2 = as.POSIXct(time/1000, origin  = "1970-01-01"))
          
        }else{
          data_hc = data.frame()
        }
        data_current_plot = data_hc
      }
      
      if(exists("data_current_plot")){
        data_ = data_current_plot
      }else{
        data_ = NULL
      }
     
    
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
        paste("presentation_", gsub("-|:| |CET","", Sys.time()), ".pdf", sep="")
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