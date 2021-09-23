shinyServer(function(input, output, session) {

  observe({Print(input$tabs_menu)})


  output$slide_show <- renderUI({
    if("slickR" %in% pkg[,1]){

      # gg1 = try(s3read_using(FUN = readRDS,
      #                       object = "dataviz/FR/pollution_idf/pollution_idf_gg_plot",
      #                       bucket = "groupe-1360",
      #                       opts = list("use_https" = F, "region" = "")), silent = T)
      # 
      # gg2 = try(s3read_using(FUN = readRDS,
      #                       object = "dataviz/FI/epargne_fr/epargne_fr_gg_plot",
      #                       bucket = "groupe-1360",
      #                       opts = list("use_https" = F, "region" = "")), silent = T)
      # 
      # slickR::slickR(obj = list(gg1, gg2), height = 100, width = "95%") +
      #   settings(dots = TRUE, autoplay = TRUE)
    }
  })

  DB_variables_react <- reactive({
    Print(input$MAJ_dico)
    Print(input$tabs_menu)

    DB_variable = update_DB_variable() %>%
      arrange(perim)

    if(input$tabs_menu == 7){
      DB_variable =
        DB_variable %>%
        filter(str_detect(perim, "^FR-"))
    }
    if(input$tabs_menu == 8){
      DB_variable =
        DB_variable %>%
        filter(!str_detect(perim, "^FR-"))
    }
    DB_variable
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
  idbank_list_react <- reactiveVal()
  idbank_list_view  <- reactiveVal()
  gg_current_name <- reactiveVal()
  data_plot <- reactiveVal()
  get_one_plot <- reactiveVal()
  list_plot_selected <- reactiveValues()
  get_interactive_plot <- reactiveVal()
  
  observe({
    get_interactive_plot(FALSE)
    if(!is.null(input$interactive_plot)){
      if(input$interactive_plot == TRUE){
        get_interactive_plot(TRUE)
      }
    }
  })
  
  observe({
    get_one_plot(FALSE)
    if(!is.null(input$one_plot)){
      if(input$one_plot == TRUE){
        get_one_plot(TRUE)
      }
    }
  })
  

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
        # if(!is.null(link_code_file())){
          # if(file.exists(link_code_file())){
        file_code = file.path("dataviz", perimeter_selected(), var_name(), paste0(var_name(), "_code"))

        if(length(file_code)>0){
          if(file_code %in% DB_minio_all()){
            render_update_button = TRUE
          }
        }
          # }
        # }
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
                                bucket = Sys.getenv("AWS_BUCKET"), use_https = T, region = "")

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

              var_update  = 'update completed'
              Print(var_update)
              update_plot_finished(sample(1:100))


            }
          }

        # }
      }
    }

  })

  observeEvent({
    input$tabs_menu
  },{
    if(input$tabs_menu == 8){

      countries2 = countries[!str_detect(countries, "^FR-")]
      icons_perims2 = icons_perims[!str_detect(icons_perims, "^FR-")]

      updatePickerInput(
        session = session,
        inputId = "select_perim",
        choices = countries2,
        choicesOpt = list(content = icons_perims2))


    }else if(input$tabs_menu == 7){

      other_perim = DB_variables_react() %>%
        filter(str_detect(perim, "^FR-")) %>%
        pull(perim) %>%
        as.character()

      updatePickerInput(
        session = session,
        inputId = "select_perim",
        choices = c(insee_perim$insee_dt_id, other_perim),
        choicesOpt = list(content = c(insee_perim$insee_dt, other_perim)))


    }
  })

  #
  # MAJ des variables et titres proposés en fonction du périmètre indiqué ####
  #
  observeEvent({
    input$select_perim
    },{

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
        DT::datatable(filter = "top", selection = "single", options = list(pageLength = 100)) %>%
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
    input$tabs_menu
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

              if(!is.null(minio_file_path)){
                if(length(minio_file_path) > 0){
                  if(stringr::str_detect(minio_file_path, "_gg_plot$")){

                    gg = try(s3read_using(FUN = readRDS,
                                          object = minio_file_path,
                                          bucket = Sys.getenv("AWS_BUCKET"),
                                          opts = list("use_https" = T, "region" = "")), silent = T)

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
                                     bucket = Sys.getenv("AWS_BUCKET"),
                                     use_https = T, region = "") %>%
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
                                              bucket = Sys.getenv("AWS_BUCKET"),
                                              use_https = T, region = ""))


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

    Print(cahier_file)
    Print(getwd())
    Print(file.exists(cahier_file))
    Print(Sys.getenv("PATH"))

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
  
  # 
  # Graphiques INSEE ####
  # 

  # 
  observe({

      dataset_list_selectize = dataset_list_selectize_fr
      dataset_placeholder = "Choix du jeu de donn\u00E9es"
      deselect_all_text = "Tout d\u00E9s\u00E9lectionner"
      select_all_text = "Tout s\u00E9lectionner"

      idbank_list_selected = id_fr
      idbank_list_all_label = idbank_list_all_label_fr
      idbank_placeholder = "Choix d'une s\u00E9rie"


    # Print(input$growth_button)
    if(!is.null(input$growth_button)){

      updatePrettyRadioButtons(
        session = session,
        inputId = "growth_button",
        selected = input$growth_button,
        label = "Choisir : ",
        choiceValues = c("raw", "yoy", "pop"),
        choiceNames = c("Donn\u00E9es brutes",
                        "Taux de croissance annuel",
                        "Taux de croissance trimestriel ou mensuel")
      )
    }


    # updateSliderInput(session = session,
    #                   inputId = "slider_period",
    #                   value = input$slider_period,
    #                   min = input$slider_period[1],
    #                   max = input$slider_period[2],
    #                   label = "Choix de la p\u00E9riode temporelle")

    output$dataset_picker <-
      renderUI(

        shinyWidgets::pickerInput(
          inputId = "dataset_picker",
          label = NULL,
          choices = dataset_list_id,
          width = "100%",
          multiple = TRUE,
          options =
            list(
              `actions-box` = TRUE,
              `deselect-all-text` = deselect_all_text,
              `select-all-text` = select_all_text,
              title = dataset_placeholder
            ),
          choicesOpt = list(content = dataset_list_selectize)
        )
      )

    # output$idbank_picker <-
    #   renderUI(
    #     selectizeInput(
    #       inputId = "idbank_picker",
    #       label = NULL,
    #       choices = NULL,
    #       width = "100%",
    #       multiple = TRUE,
    #       options = list(
    #         'plugins' = list('remove_button'),
    #         'create' = TRUE,
    #         'persist' = FALSE,
    #         placeholder = idbank_placeholder,
    #         onInitialize = I('function() { this.setValue(""); }')
    #       )
    #     )
    #   )


    # idbank_list_selected =
    #   idbank_list_selected %>%
    #   select(nomflow, idbank, title, cleFlow) %>%
    #   insee::clean_table() %>%
    #   as.data.frame()
    # 
    # idbank_list_shown = idbank_list_selected %>%
    #   DT::datatable(filter = "none", selection = c("multiple"),
    #                 options = list(pageLength = 100, scrollX = TRUE,
    #                                autoWidth = TRUE,
    #                                columnDefs = list(list(width = '170px', targets = c(0)),
    #                                                  list(width = '130px', targets = c(1)),
    #                                                  list(width = '500px', targets = c(2)))), rownames = FALSE) %>%
    #   DT::formatStyle(0, lineHeight = '15px', target= 'row')
    # 
    # idbank_list_react(idbank_list_selected)
    # idbank_list_view(idbank_list_selected)
    # 
    # output$idbank_list <- DT::renderDT(idbank_list_shown)
    # 
    # list_tab = list()
    # 
    # list_tab[[length(list_tab)+1]] =
    #   tabPanel(title = "Catalogue des s\u00E9ries - Cliquer sur une ou plusieurs s\U00E9ries pour afficher le graphique",
    #            box(
    #              width = "100%",
    #              DT::dataTableOutput("idbank_list", width = "100%", height = "75vh")
    #            ))
    # 
    # output$list_tab2 <- renderUI({
    #   do.call(tabsetPanel, c(list_tab, id = 'tabs2'))
    # })

    output$slides <- renderUI(
      selectizeInput(
        'slides',
        label = "Cr\u00E9er une pr\U00E9sentation",
        size = 30,
        width = "300px",
        multiple = TRUE,
        choices = NULL,
        options = list(
          'plugins' = list('remove_button'),
          'create' = TRUE,
          'persist' = FALSE,
          placeholder = "Ajouter le graphique à l'\u00E9cran",
          onInitialize = I('function() { this.setValue(""); }')
        )
      )
    )
  })
  
  
  
  observeEvent({
    input$dataset_picker
  },{
        
    #
    Print(input$dataset_picker)
    
    idbank_list_selected = id_fr
    dim_selectize_placeholder = "Filtrer"
    deselect_all_text = "Tout d\u00E9s\u00E9lectionner"
    select_all_text = "Tout s\u00E9lectionner"
    
    idbank_list_selected = idbank_list_selected %>%
      select(nomflow, idbank, title, cleFlow)
    
    if(!is.null(input$dataset_picker)){
      
      loc_first_space = stringr::str_locate(input$dataset_picker, "\\s")[1]
      
      if(is.na(loc_first_space)){
        loc_first_space = nchar(input$dataset_picker)
      }else{
        loc_first_space = loc_first_space - 1
      }
      
      dataset_selected_name = substr(input$dataset_picker, 1, loc_first_space)
      
      
      idbank_list_from_dataset =
        insee::get_idbank_list(dataset_selected_name) %>%
        pull(idbank)
      
      updateSelectizeInput(session, 'idbank_picker',
                           choices = idbank_list_from_dataset,
                           server = TRUE)
      
      
      idbank_list_selected =
        insee::get_idbank_list(dataset_selected_name) %>% 
        insee::clean_table()
      
      id_fr_short = id_fr %>%
        filter(nomflow %in% dataset_selected_name) %>% 
        select(idbank, title)
      
      idbank_list_selected = idbank_list_selected %>% left_join(id_fr_short, by = "idbank")
      
      first_col = c("nomflow", "idbank", 'title')
      other_col = names(idbank_list_selected)[!names(idbank_list_selected) %in% first_col]
      other_col = other_col[!stringr::str_detect(other_col, "^dim[:digit:]{1,}$")]
      idbank_list_selected = idbank_list_selected[,c(first_col, other_col)]
      
      # list_dim_column = names(idbank_list_selected)[grep("^dim", names(idbank_list_selected))]
      list_idbank_column = names(idbank_list_selected)
      list_dim_column = list_idbank_column[!stringr::str_detect(list_idbank_column, "^idbank|^cleFlow|^nomflow|^dim[:digit:]|^title")]
      list_dim_column_label = list_dim_column[stringr::str_detect(list_dim_column, "_label_fr$|_label_en$")]
      list_dim_column_short = list_dim_column[!list_dim_column %in% list_dim_column_label]
      list_dim_ui = list()
      
      for(idim in 1:length(list_dim_column_short)){
        
        dim = list_dim_column_short[idim]
        dim_label_fr = list_dim_column_label[list_dim_column_label %in% paste0(dim, c("_label_fr"))]
        dim_label_en = list_dim_column_label[list_dim_column_label %in% paste0(dim, c("_label_en"))]
        
        if(length(dim_label_fr) > 0 & length(dim_label_en) > 0){
          
          df_dim = idbank_list_selected %>%
            select(!!dim, !!dim_label_fr, !!dim_label_en) %>%
            distinct()
          
        }else{
          df_dim = idbank_list_selected %>%
            select(!!dim) %>%
            distinct()
          
          df_dim[,paste0(dim, "_label_fr")] = ""
          df_dim[,paste0(dim, "_label_en")] = ""
        }
        
        df_dim[, "value_label"] = unlist(lapply(1:nrow(df_dim),
                                                function(i){
                                                  if(is.na(df_dim[i,dim])){return(NA)}
                                                  if(df_dim[i, paste0(dim, sprintf("_label_%s", "fr"))] == ""){
                                                    return(df_dim[i,dim])
                                                  }else{
                                                    return(paste(df_dim[i,dim], "-", df_dim[i,paste0(dim, sprintf("_label_%s", "fr"))]))
                                                  }
                                                }))
        
        dim_placeholder = sprintf("%s", dim)
        
        value_dim = df_dim %>% pull(dim) 
        value_dim_label = df_dim %>% pull("value_label")
        
        value_dim = sort(value_dim[!is.na(value_dim)])
        value_dim_label = sort(value_dim_label[!is.na(value_dim_label)])
        
        list_dim_ui[[length(list_dim_ui)+1]] <-
          column(2,
                 shinyWidgets::pickerInput(
                   inputId = paste0("dim", idim),
                   label = NULL,
                   choices = value_dim,
                   width = "100%",
                   multiple = TRUE,
                   options =
                     pickerOptions(
                       actionsBox = TRUE,
                       selectAllText = select_all_text,
                       deselectAllText = deselect_all_text,
                       dropdownAlignRight = "auto",
                       title = dim_placeholder
                     ),
                   choicesOpt = list(content = value_dim_label)
                 )
          )
      }
      
      output$dims = renderUI({tagList(list_dim_ui)})
      
    }else{
      updateSelectizeInput(session, 'idbank_picker', choices = idbank_list_all, server = TRUE)
      output$dims = renderUI({NULL})
    }
    
    idbank_list_react(idbank_list_selected)
    idbank_list_view(idbank_list_selected)
    
    
    idbank_list_shown = idbank_list_selected %>%
      DT::datatable(filter = "none", selection = c("multiple"),
                    options = list(pageLength = 100, scrollX = TRUE,
                                   autoWidth = TRUE,
                                   columnDefs = list(list(width = '170px', targets = c(0)),
                                                     list(width = '130px', targets = c(1)),
                                                     list(width = '500px', targets = c(2)))), rownames = FALSE) %>%
      DT::formatStyle(0, lineHeight = '15px', target= 'row', textAlign = 'center')
    
    output$idbank_list <- DT::renderDT(idbank_list_shown)
    
    list_tab = list()
    
    list_tab[[length(list_tab)+1]] =
      tabPanel(title = "Catalogue des s\u00E9ries - Cliquer sur une ou plusieurs s\U00E9ries pour afficher le graphique",
               box(
                 width = "100%",
                 DT::dataTableOutput("idbank_list", width = "100%", height = "75vh")
               ))
    
    output$list_tab2 <- renderUI({
      do.call(tabsetPanel, c(list_tab, id = 'tabs2'))
    })
    
  }, ignoreNULL = FALSE)
  
  
  
  observeEvent({
    input$dim1
    input$dim2
    input$dim3
    input$dim4
    input$dim5
    input$dim6
    input$dim7
    input$dim8
    input$dim9
    input$dim10
    input$dim11
    input$dim12
    input$dim13
    input$dim14
    input$dim15
    input$dim16
    input$idbank_picker
  },{
    # Print(input$dim1)
    
    # if(is.null(lang())){lang_selected = "en"}else{lang_selected = lang()}
    
    
    idbank_list_selected = idbank_list_react()
    list_idbank_column = names(idbank_list_selected)
    list_dim_column = list_idbank_column[!stringr::str_detect(list_idbank_column, "^idbank|^cleFlow|^nomflow|^dim[:digit:]|^title")]
    list_dim_column_label = list_dim_column[stringr::str_detect(list_dim_column, "_label_fr$|_label_en$")]
    list_dim_column_short = list_dim_column[!list_dim_column %in% list_dim_column_label]
    
    any_dim_not_null = FALSE
    
    for(idim in 1:length(list_dim_column_short)){
      dim = list_dim_column_short[idim]
      
      if(length(dim) > 0) {
        if (!is.na(dim)) {
          any_dim_not_null = TRUE
          if (!is.null(input[[paste0('dim', idim)]])) {
            
            idbank_list_selected =
              idbank_list_selected %>%
              filter(!!sym(dim) %in% input[[paste0('dim', idim)]])
          }
        }
      }
      
    }
    
    if(!is.null(input$idbank_picker)){
      if(nrow(idbank_list_selected) > 0 ){
        
        any_dim_not_null = TRUE
        
        idbank_list_selected =
          idbank_list_selected %>%
          dplyr::filter(idbank %in% input$idbank_picker) %>%
          insee::clean_table()
      }
    }
    
    if(any_dim_not_null){
      
      idbank_list_view(idbank_list_selected)
      
      idbank_list_shown = idbank_list_selected %>%
        DT::datatable(filter = "none", selection = c("multiple"),
                      options = list(pageLength = 100, scrollX = TRUE,
                                     autoWidth = TRUE,
                                     columnDefs = list(list(width = '170px', targets = c(0)),
                                                       list(width = '130px', targets = c(1)),
                                                       list(width = '500px', targets = c(2)))), rownames = FALSE) %>%
        DT::formatStyle(0, lineHeight = '15px', target= 'row', textAlign = 'center')
      
      output$idbank_list <- DT::renderDT(idbank_list_shown)
      
      list_tab = list()
      
      list_tab[[length(list_tab)+1]] =
        tabPanel(title = "Catalogue des s\u00E9ries - Cliquer sur une ou plusieurs s\U00E9ries pour afficher le graphique",
                 box(
                   width = "100%",
                   DT::dataTableOutput("idbank_list", width = "100%", height = "75vh")
                 ))
      
      output$list_tab2 <- renderUI({
        do.call(tabsetPanel, c(list_tab, id = 'tabs2'))
      })
    }
    
  }
  , ignoreNULL = FALSE
  )
  
  observeEvent({
    input$deselect_idbank_in_list
  },{
    idbank_list_proxy <- DT::dataTableProxy("idbank_list", session = session)
    DT::selectRows(idbank_list_proxy, NULL)
  })
  
  
  observeEvent({
    input$new_plot
    input$idbank_list_rows_selected
  },{
    # if(is.null(lang())){lang_selected = "en"}else{lang_selected = lang()}
    
    row_selected = input$idbank_list_rows_selected
    
    # trigger_update
    if(TRUE){
      if(!is.null(row_selected)){
        
        idbank_selected =
          idbank_list_view() %>%
          slice(row_selected) %>%
          pull(idbank)
        
        data_raw = get_insee_idbank(idbank_selected)
        data = data_raw
        
        data = data %>%
          mutate(TITLE_EN = paste(TITLE_EN , "-", IDBANK)) %>%
          mutate(TITLE_FR = paste(TITLE_FR , "-", IDBANK))
        
        gg_name = paste0("plot_", gsub("-|:| |CET","", Sys.time()))
        gg_current_name(gg_name)
        
        output$growth_button <- renderUI({
          prettyRadioButtons(
            inputId = "growth_button",
            selected = input$growth_button,
            label = "Choisir : ",
            choiceNames = c("Donn\u00E9es brutes",
                            "Taux de croissance annuel",
                            "Taux de croissance trimestriel ou mensuel"),
            choiceValues = c("raw", "yoy", "pop")
          )
        })
        
        if(!is.null(input$growth_button)){
          
          if(input$growth_button != "raw"){
            data = data %>%
              mutate(year = lubridate::year(DATE)) %>%
              arrange(DATE) %>%
              group_by(IDBANK, year) %>%
              mutate(freq = n()) %>%
              group_by(IDBANK) %>%
              mutate(freq = max(freq))
          }
          
          if(input$growth_button == "yoy"){
            
            data = data %>%
              mutate(OBS_VALUE_raw = OBS_VALUE) %>%
              group_by(IDBANK) %>%
              mutate(OBS_VALUE = 100 * (OBS_VALUE / abs(dplyr::lag(OBS_VALUE, min(freq))) - 1))
            
            data = data %>%
              mutate(TITLE_EN = paste(TITLE_EN , "-", "Taux de croissance annuel"))
          }
          
          if(input$growth_button == "pop"){
            data = data %>%
              mutate(OBS_VALUE_raw = OBS_VALUE) %>%
              group_by(IDBANK) %>%
              mutate(OBS_VALUE = 100 * (OBS_VALUE / abs(dplyr::lag(OBS_VALUE, 1)) - 1))
            
            data = data %>%
              mutate(
                add_title_period = case_when(
                  freq == 12 ~ "Taux de croissance mensuel",
                  freq == 4 ~ "Taux de croissance trimestriel",
                  freq == 6 ~ "Taux de croissance bimensuel",
                  freq == 2 ~ "Taux de croissance semestriel",
                  freq == 1 ~ "Taux de croissance annuel"
                )
              )
            
            data = data %>%
              mutate(TITLE_FR = paste(TITLE_FR , "-", add_title_period)) %>%
              select(-add_title_period)
          }
          
          data = data %>% ungroup() %>% arrange(desc(DATE))
        }
        
        data_plot(data)
        
        if(is.null(input$slider_period)){
          min_slider_period =  min(data_raw$DATE)
          max_slider_period =  max(data_raw$DATE)
        }else{
          min_slider_period = input$slider_period[1]
          max_slider_period = input$slider_period[2]
        }
        
        output$slider_period <- renderUI({
          sliderInput("slider_period",
                      label = "Choix de la p\u00E9riode temporelle",
                      min = min(data_raw$DATE),  max = max(data_raw$DATE),
                      value = c(min_slider_period, max_slider_period))
        })
        
        data = data %>%
          filter(DATE >= min_slider_period & DATE <= max_slider_period)
        
        data$TITLE_EN = unlist(lapply(strwrap(data$TITLE_EN,
                                              width = 80, simplify=FALSE),
                                      paste, collapse="\n"))
        
        data$TITLE_FR = unlist(lapply(strwrap(data$TITLE_FR,
                                              width = 80, simplify=FALSE),
                                      paste, collapse="\n"))
        
        gg =
          ggplot(data, aes(x = DATE, y = OBS_VALUE))
        
        
        if(!get_one_plot()){
          gg = gg +
            facet_wrap(~TITLE_FR, scales = "free", dir = "v")
          gg = gg +
            geom_line() +
            geom_point(size = 1.5)
        }else{
          gg = gg +
            geom_line(aes(colour = TITLE_FR)) +
            geom_point(aes(colour = TITLE_FR), size = 1.5)
          gg = gg + guides(colour = guide_legend(ncol = 1))
        }
        
        gg = gg %>% add_style(lang = "fr")
        
        # gg_current(gg)
        list_plot_selected[[gg_name]] = gg
        
        output[[gg_name]] <- renderPlot({gg})
        
        if(!get_interactive_plot()){
          
          tab = tabPanel(title =  "Graphique",
                         box(
                           width = "100%",
                           plotOutput(gg_name, width = "100%", height = "80vh")
                         ))
          
        }else{
          
          output[["plotly_requested"]] <- plotly::renderPlotly({gg_plotly(data, lang = "fr")})
          
          tab =   tabPanel(title =  "Graphique",
                           box(
                             width = "100%",
                             plotlyOutput("plotly_requested", width = "100%", height = "80vh")
                           ))
          
        }
        
        removeTab(inputId = "tabs2",
                  target =  "Graphique")
        
        insertTab(inputId = "tabs2",
                  tab = tab,
                  position = "after", 
                  target = "Catalogue des s\u00E9ries - Cliquer sur une ou plusieurs s\U00E9ries pour afficher le graphique",
                  select = TRUE)
        
        data_selected = data %>% select(-TITLE_EN)
        
        data_shown =
          data_selected %>%
          arrange(desc(DATE)) %>%
          DT::datatable( filter = "none",
                         options = list(pageLength = 100, scrollX = TRUE,
                                        autoWidth = TRUE,
                                        columnDefs = list(list(width = '400px', targets = c(8)))
                         ),
                         rownames = FALSE) %>%
          DT::formatStyle(0, lineHeight = '15px', target = 'row', textAlign = 'center')
        
        output$data_table <- DT::renderDT(data_shown)
        
        
        tab_data =     tabPanel(title = "Donn\u00E9es",
                                box(
                                  width = "100%",
                                  DT::dataTableOutput("data_table", width = "100%", height = "75vh")
                                ))
        
        removeTab(inputId = "tabs2",
                  target = "Donn\u00E9es")
        
        insertTab(inputId = "tabs2",
                  tab = tab_data,
                  position = "after", 
                  target = "Graphique",
                  select = FALSE)
        
      }
    }
  })
  
  observeEvent({
    input$catalogue_cell_clicked
  },{
    
    row_selected = input$catalogue_cell_clicked$row[1]
    
    if(!is.null(row_selected)){
      
      gg_selected =
        plot_table_react() %>%
        slice(row_selected) %>%
        pull(id)
      
      plot_table_shown =
        plot_table %>%
        dplyr::select(id, title_fr) %>%
        dplyr::rename(Titre = title_fr, Identifiant = id)
      
      plot_table_shown2 = 
        plot_table_shown %>% 
        DT::datatable(
          filter = "none",
          options = list(pageLength = 100),
          rownames = FALSE
        ) %>%
        DT::formatStyle(0, lineHeight = '15px', target = 'row')
      
      output$catalogue <- DT::renderDT(plot_table_shown2)
      
      gg_statement = sprintf("%s(lang='%s')", gg_selected, "fr")
      
      gg = eval(parse(text = gg_statement))
      
      # gg_current(gg)
      gg_current_name(gg_selected)
      list_plot_selected[[gg_selected]] = gg
      
      output[[paste0(gg_selected, "_plot")]] <- renderPlot({gg})
      
      data_plot(gg[["data"]])
      
      list_tab = list()
      
      list_tab[[length(list_tab)+1]] =
        tabPanel(title = "Catalogue",
                 box(
                   width = "100%",
                   DT::dataTableOutput("catalogue", width = "100%", height = "75vh")
                 ))
      
      list_tab[[length(list_tab)+1]] =
        tabPanel(title = "Graphique",
                 box(
                   width = "100%",
                   plotOutput(paste0(gg_selected, "_plot"), width = "100%", height = "80vh")
                 ))
      
      list_tab[[length(list_tab)+1]] =
        tabPanel(title = "Code",
                 box(
                   width = "100%",
                   renderPrint({print(gg_selected);get(gg_selected)})
                 ),
                 box(
                   width = "100%",
                   renderPrint({print("add_style");add_style})
                 )
        )
      
      output$list_tab <- renderUI({
        do.call(tabsetPanel, c(list_tab, id = 'tabs',
                               selected = "Graphique"))
      })
      
    }
    
  })
  
  observe({
    
    input$downloadData2
    
    data = data_plot()
    
    if(!is.null(data)){
      if(any(class(data) == "data.frame")){
        
        output$downloadData2 <- downloadHandler(
          filename = function() {
            paste("data_", gsub("-|:| |CET","", Sys.time()), ".csv", sep="")
          },
          content = function(file) {
            utils::write.csv(data, file,  row.names = FALSE)
          }
        )
        
      }
    }
    
  })
  
  
  observeEvent({
    gg_current_name()
  },{
    updateSelectizeInput(session, "slides",
                         choices = c(gg_current_name(), input$slides), selected = input$slides)
  })
  
  #
  # DOWNLOAD SLIDES
  #
  observe({
    list_plot_slides = lapply(input$slides, function(x) list_plot_selected[[x]])
    
    output$downloadSlides <- downloadHandler(
      filename = function() {
        paste("frenchEconomy_slides_", gsub("-|:| |CET","", Sys.time()), ".pdf", sep="")
      },
      content = function(file) {
        rmarkdown::render(input = slides_rmd_file,
                          output_file = file,
                          params = list(list_graph = list_plot_slides))
      }
    )
    
  })
  
  
}
)
