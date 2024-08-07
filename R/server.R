server <- function(input, output, session){
  my_color_palette <- c("#00719A","#FCD0BE","#413647","#FF9A73","#063547",
                        "#992B00","#F3D9AE","#6991B3","#B8062F","#BDECFC",
                        "#D3D3D3")

  palette_xgfs <- c(
    # source: http://tsitsul.in/blog/coloropt/
    "#bdbdbd",
    "#00a76c",
    "#878500",
    "#00c6f8",
    "#5954d6",
    "#ff9287",
    "#b24502",
    "#d163e6",
    "#00bbad",
    "#006e00",
    "#008cf9",
    "#b80058",
    "#ebac23"
  )

  my_data_sidebar_fileinput_tables <- bslib::card_body(
    fillable = FALSE,
    fileInput("my_data_samples",
              label = span("Muestras",
                           bslib::tooltip(bsicons::bs_icon("info-circle"),
                                          "Carga la tabla 'samples.csv'",
                                          placement = "right")),
              #buttonLabel = "Buscar 'samples.csv'",
              buttonLabel = bsicons::bs_icon("upload"),
              accept = c(".csv"),
              width = "100%",
              placeholder = "Ningún archivo seleccionado"
    ),
    fileInput("my_data_abundance",
              label = span("Abundancias",
                           bslib::tooltip(bsicons::bs_icon("info-circle"),
                                          "Carga la tabla 'counts.csv' o 'abundances.csv'",
                                          placement = "right")),
              # buttonLabel = "Buscar 'counts.csv'",
              buttonLabel = bsicons::bs_icon("upload"),
              accept = c(".csv"),
              width = "100%",
              placeholder = "Ningún archivo seleccionado"
    ),
    fileInput("my_data_taxa",
              label = span("Taxonomia", bslib::tooltip(bsicons::bs_icon("info-circle"),
                                                       "Carga la tabla taxa.csv",
                                                       placement = "right")),
              # buttonLabel = "Buscar 'taxa.csv'",
              buttonLabel = bsicons::bs_icon("upload"),
              accept = c(".csv"),
              width = "100%",
              placeholder = "Ningún archivo seleccionado"
    )
  )


  my_data_sidebar_fileinput_list <- list(
    "tidytacos" = my_data_sidebar_fileinput_tables,
    "biom" = bslib::card_body(
      fillable = FALSE,
      fileInput("my_data_biom",
                label = "Cargar archivo .biom",
                # buttonLabel = "Archivo",
                buttonLabel = bsicons::bs_icon("upload"),
                accept = c(".biom"),
                width = "100%",
                placeholder = "Ningún archivo seleccionado"
      )
    )
  )

  #Reactive update of the opening of the sidebar
  observe({
    if(input$my_navbar != "start"){
      bslib::toggle_sidebar(
        id = "my_sidebar", open = TRUE, session
      )
    }else{
      bslib::toggle_sidebar(
        id = "my_sidebar", open = FALSE, session
      )
    }
    # toggle_sidebar(
    #   id = "my_sidebar", open = input$my_navbar != "start"
    #   )
  })

  output$load_file_options <- renderUI(
    {
      switch (input$input_format,
              "Tidytacos" = my_data_sidebar_fileinput_list$tidytacos,
              "Biom" = my_data_sidebar_fileinput_list$biom
      )

    }
  )

  # Reactive upload of data
  # adapted from: https://stackoverflow.com/questions/75675984/r-shiny-how-to-have-an-action-button-that-automatically-downloads-a-csv-file
  loaded_data <- reactiveValues(my_loaded_data = NULL,
                                my_data = NULL,
                                my_sample_col_clicked_ix = NULL,
                                my_taxa_col_clicked_ix = NULL,
                                from = FALSE)

  observeEvent(input$input_my_data, {
    if(input$input_format == "Tidytacos"){
      req(input$my_data_samples, input$my_data_abundance, input$my_data_taxa)
      withProgress(
        message = "Cargando tus datos ...", value = 0, {
          my_samples <- readr::read_csv(input$my_data_samples$datapath,
                                        col_types = readr::cols())
          incProgress(.2, message = "samples.csv imported!")

          my_taxa <- readr::read_csv(input$my_data_taxa$datapath,
                                     col_types = readr::cols())
          incProgress(.4, message = "taxa.csv imported!")

          my_counts <- readr::read_csv(input$my_data_abundance$datapath,
                                       col_types = readr::cols())
          incProgress(.6, message = "counts.csv imported!")

          expected_rank_names <- colnames(my_taxa)[!colnames(my_taxa) %in%
                                                     c("taxon","taxon_id","sequence")]
          incProgress(.7, message = "combining all files ...")
          my_vdata <- tidytacos:::make_tidytacos(samples = my_samples,
                                                 taxa = my_taxa,
                                                 counts = my_counts,
                                                 sample_name = sample_id,
                                                 taxon_name = taxon_id)
          if ( !all(my_vdata %>% tidytacos::rank_names() %in% expected_rank_names)) {
            warning(paste0(
              "Not all default rank names found. Replacing them with:\n c(\"",
              paste(expected_rank_names, collapse='","'),
              "\")\n\nIf these are not the rank names of your taxon table, \nplease set ",
              "them manually using 'set_rank_names()'"))
            my_vdata <- my_vdata %>% tidytacos::set_rank_names(expected_rank_names)
          }

          my_vdata <- my_vdata %>%
            tidytacos::add_mean_rel_abundance() %>%
            tidytacos::add_prevalence() %>%
            tidytacos::add_total_count() %>%
            my_add_top_genus(genus_aggregation = T)


          loaded_data$my_loaded_data <- loaded_data$my_data <- my_vdata
          incProgress(1, message = "Completado!")
        }
      )
      updateTabsetPanel(session, inputId="my_navbar", "my_data")
    }
  })

  observeEvent(input$input_example, {
    withProgress(
      message = "Cargaremos un dataset ejemplo ...", value = 0, {

        incProgress(.25, message = "Cargando dataset ...")
        #
        #load(my_example)
        loaded_data$my_loaded_data <- loaded_data$my_data <- vdata %>%
          tidytacos::set_rank_names(c("kingdom", "phylum", "class",
                                      "order", "family", "genus")) %>%
          tidytacos::add_total_count() %>%
          my_add_top_genus(genus_aggregation = T) %>%
          tidytacos::add_mean_rel_abundance() %>%
          tidytacos::add_prevalence()

        #loaded_data$my_dominant_taxa_counts <- loaded_data$my_loaded_data %>%
        #  tidytacos::samples() %>%
        #  dplyr::count(dominant_taxa, sort = TRUE)

        incProgress(1, message = "Completado!")
      }
    )
    updateTabsetPanel(session, inputId="my_navbar", "my_data")
  })

  observeEvent(input$input_apply_filter, {
    req(loaded_data$my_loaded_data)

    cols_to_recompute_samples <- c("total_count",
                                   "top_genus1_id", "top_genus1_rel_abundance", "top_genus1",
                                   "top_genus2_id", "top_genus2_rel_abundance", "top_genus2",
                                   "dominant_taxa")
    cols_to_recompute_taxa <- c("mean_rel_abundance")

    # Filtrar muestras #
    #
    my_filtered_data <- loaded_data$my_loaded_data
    # Minimal total count (aka library size)
    total_count_threshold <- input$sample_total_count
    if(total_count_threshold > 1){
      my_filtered_data <- my_filtered_data %>%
        #tidytacos::add_total_count() %>%
        tidytacos::filter_samples(total_count >= total_count_threshold)
    }
    # Provided sample IDs
    sample_ids_to_filter <- input$sampleIDs_to_filter
    if(!is.null(sample_ids_to_filter)){
      my_filtered_data <- my_filtered_data %>%
        tidytacos::filter_samples(!(sample_id %in% sample_ids_to_filter))
    }
    # Filtrar taxa #
    #
    # Minimal relative abundance (mean)
    ra_threshold_from_filter <- input$taxa_mean_ra
    if(ra_threshold_from_filter>0){
      my_filtered_data <- my_filtered_data %>%
        #tidytacos::add_mean_rel_abundance() %>%
        tidytacos::filter_taxa(mean_rel_abundance >= ra_threshold_from_filter) %>%
        tidytacos::select_taxa(-mean_rel_abundance)
    }
    # Minimal prevalence
    prev_threshold_from_filter <- input$taxa_prevalence
    if(prev_threshold_from_filter > 0){
      my_filtered_data <- my_filtered_data %>%
        tidytacos::filter_taxa(prevalence >= prev_threshold_from_filter) %>%
        tidytacos::select_taxa(-prevalence)
    }
    # Provided Taxa IDs
    taxon_ids_to_filter <- input$taxaIDs_to_filter
    if(!is.null(taxon_ids_to_filter)){
      my_filtered_data <- my_filtered_data %>%
        tidytacos::filter_taxa(!(taxon_id %in% taxon_ids_to_filter))
    }

    # Re-compute total counts and top genus
    my_filtered_data$samples <- my_filtered_data$samples %>%
      dplyr::select(-one_of(cols_to_recompute_samples))

    my_filtered_data <- my_filtered_data %>%
      tidytacos::add_total_count() %>%
      my_add_top_genus(genus_aggregation = T) %>%
      tidytacos::add_mean_rel_abundance() %>%
      tidytacos::add_prevalence()

    # Replace the current working dataset with the filtered one #
    loaded_data$my_data <- my_filtered_data
  })

  observeEvent(input$input_clean_filter, {
    req(loaded_data$my_loaded_data)
    loaded_data$my_data <- loaded_data$my_loaded_data
  })

  # Non-numeric columns with at most 15 unique values
  samples_columns_non_numeric <- reactive({
    req(loaded_data$my_data)
    cols_to_exclude <- c("sample","sample_id","top_genus1","top_genus2","dominant_taxa")
    my_nonnumeric_cols <- loaded_data$my_data$samples %>%
      dplyr::select_if(~!is.numeric(.x)) %>%
      apply(2, function(x){length(unique(x))})

    names(my_nonnumeric_cols)[(my_nonnumeric_cols <=15) &
                                !(names(my_nonnumeric_cols) %in% cols_to_exclude)]
  })

  # Taxonomic ranks
  my_taxonomic_ranks <- reactive({
    req(loaded_data$my_data)
    tidytacos::rank_names(loaded_data$my_data)
  })

  my_sample_ids <- reactive({
    req(loaded_data$my_data)
    loaded_data$my_data %>%
      tidytacos::samples() %>% dplyr::pull(sample_id)
  })

  # output$my_samples_id <- renderPrint({
  #   input$sampleIDs_to_filter
  # })

  my_taxon_ids <- reactive({
    req(loaded_data$my_data)
    loaded_data$my_data %>%
      tidytacos::taxa() %>% dplyr::pull(taxon_id)
  })

  observe({
    req(my_sample_ids())
    updateSelectizeInput(inputId = "sampleIDs_to_filter",
                         choices = my_sample_ids(),
                         selected = character(0),
                         server = TRUE)
  })

  observe({
    req(my_taxon_ids())
    updateSelectizeInput(inputId = "taxaIDs_to_filter",
                         choices = my_taxon_ids(),
                         selected = character(0),
                         server = TRUE)
  })

  observeEvent(loaded_data$my_data, {
    req(loaded_data$my_data)
    nr_samples <- as.numeric(tidytacos::tacosum(loaded_data$my_data)["n_samples"])
    updateSliderInput(inputId = "taxa_prevalence",
                      max = nr_samples)
  })

  #---------------------------------------------------------------------
  # Populate DATA page                                                 #
  #---------------------------------------------------------------------

  output$total_sample <- renderText(
    tidytacos::tacosum(loaded_data$my_data)["n_samples"])

  output$total_taxa <- renderText(
    tidytacos::tacosum(loaded_data$my_data)["n_taxa"])

  output$total_reads <- renderText(
    tidytacos::tacosum(loaded_data$my_data)["n_reads"])

  # Populate "Muestras y Metadata" accordion
  #-----------------------------------------
  create_datatable_w_buttons <- function(my_df, buttons_id_prefix){
    number_of_buttoms <- ncol(my_df)
    my_df_col_ix_onclick_name <- paste0("'",buttons_id_prefix,"'")
    my_buttons <- lapply(1:number_of_buttoms,
                         function(i){
                           actionButton(
                             paste0(buttons_id_prefix,i),
                             "plot",
                             class = "btn-primary btn-sm",
                             style = "border-radius: 50%;",
                             onclick = sprintf("Shiny.setInputValue(%s, %d, {priority:'event'});",
                                               my_df_col_ix_onclick_name, i )
                           )
                         })
    sketch <- tags$table(
      class = "row-border stripe hover compact",
      DT::tableHeader(c("", names(my_df))),
      DT::tableFooter(c("", my_buttons))
    )
    DT::datatable(
      my_df,
      #fillContainer = TRUE,
      container = sketch,
      filter = "top",
      extensions = 'FixedColumns',
      options =
        list(
          autoWidth = TRUE,
          scrollX=TRUE,
          #scrollY="200px",
          columnDefs = list(
            list(
              className = "dt-center",
              targets = "_all"
            )
          ),
          lengthMenu = c(5, 10, 15, 20),
          fixedColumns = list(leftColumns = 3)
        ),
      style = "bootstrap4", height = "100%"
    )
  }

  output$my_samples <- DT::renderDataTable({
    req(loaded_data$my_data)
    create_datatable_w_buttons(loaded_data$my_data$samples,
                               "my_sample_col_clicked_ix")

  })

  shiny::observeEvent(input$my_sample_col_clicked_ix, {
    loaded_data$my_sample_col_clicked_ix <- input$my_sample_col_clicked_ix
    bslib::sidebar_toggle(id="data_samples_table_sidebar", open = TRUE)
  })

  output$my_col_name <- renderText({
    req(loaded_data$my_sample_col_clicked_ix)
    names(loaded_data$my_data$samples)[loaded_data$my_sample_col_clicked_ix]
  }
  )

  create_menu_for_table_col <- function(my_df, my_col_ix, preffix_id){
    col_is_numeric <- is.numeric(my_df[[my_col_ix]])
    col_unique_values <- length(unique(my_df[[my_col_ix]]))
    if(col_is_numeric & col_unique_values > 19){
      bslib::card_body(
        p("La variable seleccionada es numérica."),
        radioButtons(
          paste0(preffix_id,"_col_plot_choice"),"Selecciona el tipo de gráfico:",
          c("density","histogram","boxplot"), inline = TRUE
        ),
        conditionalPanel(
          condition = sprintf("input.%s_col_plot_choice=='histogram'",preffix_id),
          sliderInput(paste0(preffix_id,"_col_plot_hist_bins"), "Número de intervalos",
                      min=5, max = 100, value = 30)
        )
      )
    }else{
      bslib::card_body(
        p("La variable seleccionada es categórica o discreta.")
      )
    }
  }

  output$my_sample_col_plot_options <- renderUI({
    req(loaded_data$my_data, loaded_data$my_sample_col_clicked_ix)
    create_menu_for_table_col(my_df = loaded_data$my_data$samples,
                              my_col_ix = loaded_data$my_sample_col_clicked_ix,
                              preffix_id = "samples")
  })

  output$my_samples_col_plots <- renderPlot({
    #
    req(loaded_data$my_data, loaded_data$my_sample_col_clicked_ix)
    #
    col_plot_type <- input$samples_col_plot_choice
    col_plot_hist_bins <- input$samples_col_plot_hist_bins
    col_name <- names(loaded_data$my_data$samples)[loaded_data$my_sample_col_clicked_ix]
    col_is_numeric <- is.numeric(loaded_data$my_data$samples[[loaded_data$my_sample_col_clicked_ix]])
    col_unique_values <- length(unique(loaded_data$my_data$samples[[loaded_data$my_sample_col_clicked_ix]]))
    #
    if(col_is_numeric & col_unique_values > 19){
      req(col_plot_type)
      plot_num_col_from_df(my_df = loaded_data$my_data$samples,
                           my_col_ix = loaded_data$my_sample_col_clicked_ix,
                           plot_type = col_plot_type,
                           n_bins = col_plot_hist_bins)
    }else{
      plot_disc_col_from_df(my_df = loaded_data$my_data$samples,
                            my_col_ix = loaded_data$my_sample_col_clicked_ix)
    }
  })

  output$my_samples_nas <- plotly::renderPlotly({
    req(loaded_data$my_data)
    my_data_to_plot <- tidytacos::samples(loaded_data$my_data)
    my_data_to_plot %>%
      heatmaply::heatmaply_na(labRow = my_data_to_plot$sample_id,
                              showticklabels = c(F, F),
                              xlab = "Variables",
                              ylab = "Muestras",
                              hide_colorbar = TRUE,
                              show_dendrogram = c(F, F),
                              k_row = 3, k_col = 3)
  })

  # Populate "Taxa y Metadata" accordion
  #-----------------------------------------
  output$my_taxa <- DT::renderDT({
    req(loaded_data$my_data)
    create_datatable_w_buttons(loaded_data$my_data$taxa, "my_taxa_col_clicked_ix")
  })

  shiny::observeEvent(input$my_taxa_col_clicked_ix, {
    loaded_data$my_taxa_col_clicked_ix <- input$my_taxa_col_clicked_ix
    bslib::sidebar_toggle(id="data_taxa_table_sidebar", open = TRUE)
  })

  output$my_taxa_col_name <- renderText({
    req(loaded_data$my_data, loaded_data$my_taxa_col_clicked_ix)
    names(loaded_data$my_data$taxa)[loaded_data$my_taxa_col_clicked_ix]}
  )

  output$my_taxa_col_plot_options <- renderUI({
    req(loaded_data$my_data, loaded_data$my_taxa_col_clicked_ix)
    create_menu_for_table_col(my_df = loaded_data$my_data$taxa,
                              my_col_ix = loaded_data$my_taxa_col_clicked_ix,
                              preffix_id = "taxa")
  })

  output$my_taxa_col_plots <- renderPlot({
    req(loaded_data$my_data, loaded_data$my_taxa_col_clicked_ix)

    col_plot_type <- input$taxa_col_plot_choice
    col_plot_hist_bins <- input$taxa_col_plot_hist_bins
    col_name <- names(loaded_data$my_data$taxa)[loaded_data$my_taxa_col_clicked_ix]
    col_is_numeric <- is.numeric(loaded_data$my_data$taxa[[loaded_data$my_taxa_col_clicked_ix]])
    col_unique_values <- length(unique(loaded_data$my_data$taxa[[loaded_data$my_taxa_col_clicked_ix]]))
    if(col_is_numeric & col_unique_values > 19){
      req(col_plot_type)
      plot_num_col_from_df(my_df = loaded_data$my_data$taxa,
                           my_col_ix = loaded_data$my_taxa_col_clicked_ix,
                           plot_type = col_plot_type,
                           n_bins = col_plot_hist_bins)
    }else{
      plot_disc_col_from_df(my_df = loaded_data$my_data$taxa,
                            my_col_ix = loaded_data$my_taxa_col_clicked_ix)
    }
  })

  # Populate "Abundancias" accordion
  # --------------------------------
  output$my_counts <- DT::renderDT({
    req(loaded_data$my_data)
    DT::datatable(loaded_data$my_data$counts,
              class = "row-border stripe hover compact",
              filter = "top",
              options =
                list(
                  autoWidth = TRUE,
                  scrollX=TRUE,
                  #scrollY="200px",
                  columnDefs = list(
                    list(
                      className = "dt-center",
                      targets = "_all"
                    )
                  ),
                  lengthMenu = c(5, 10, 15, 20)
                ))
  })

  #---------------------------------------------------------------------
  # Populate COMPOSITION page                                          #
  #---------------------------------------------------------------------
  #-----------------------------------
  #Populate "Taxa Dominante" accordion
  #------------------------------------

  # Dominant taxa counts, reactive to the status of the input dataset
  my_dominant_taxa_counts <- reactive({
    req(loaded_data$my_data)
    my_counts <- loaded_data$my_data %>%
      tidytacos::samples() %>%
      dplyr::count(dominant_taxa, sort = TRUE) %>%
      dplyr::mutate(taxa_perc = round((`n`/sum(`n`))*100,1),
                    labels = scales::percent(taxa_perc, scale=1))
    #count_total <- sum(my_counts$n)
    #my_counts %>%
    #  dplyr::mutate(taxa_perc = round((n/count_total)*100, 1))
    no_dominance_ix <- which(my_counts$dominant_taxa == "No dominance")
    my_levels <- c(my_counts$dominant_taxa[-no_dominance_ix], "No dominance")
    my_colors_ix <- which(seq(length(my_levels)) != no_dominance_ix & my_counts$taxa_perc > 1)
    my_colors <- rep(my_color_palette[11], length(my_levels))

    my_colors[no_dominance_ix] <- my_color_palette[10]
    if(length(my_colors_ix)<10){
      my_colors[my_colors_ix] <- my_color_palette[1:length(my_colors_ix)]
    }else{
      my_colors[my_colors_ix[1:9]] <- my_color_palette[1:9]
    }

    my_counts <- my_counts %>%
      dplyr::mutate(dominant_taxa = factor(dominant_taxa, levels=my_levels),
                    taxa_colors = my_colors) %>%
      dplyr::arrange(dominant_taxa)

    my_counts
  })

  # Updated selection list if bar plot by category of variable of choice

  # Update list of choices
  observeEvent(input$dom_taxa_barplot_stratify,{
    req(loaded_data$my_data, samples_columns_non_numeric)

    shinyWidgets::updatePickerInput(inputId = "dom_taxa_barplot_variable",
                                    choices = samples_columns_non_numeric())
  })

  # Selected Variable stored as reactive value
  my_dominant_taxa_stratify_var <- reactiveVal()

  observeEvent(input$dom_taxa_barplot_variable,{
    my_dominant_taxa_stratify_var(input$dom_taxa_barplot_variable)
  })

  # Selected association test stored as reactive value
  my_dominant_taxa_stratify_test <- reactiveVal()

  observeEvent(input$dom_taxa_barplot_stratify_test,{
    if(input$dom_taxa_barplot_stratify_test != "No"){
      my_dominant_taxa_stratify_test(input$dom_taxa_barplot_stratify_test)
    }
  })

  # Dominant taxa counts by variable
  my_dominant_taxa_counts_by_categories <- reactive({
    req(my_dominant_taxa_counts, input$dom_taxa_barplot_variable)
    my_selected_var <- input$dom_taxa_barplot_variable
    my_levels <- my_dominant_taxa_counts() %>%
      dplyr::pull(dominant_taxa) %>% levels()

    loaded_data$my_data %>%
      tidytacos::samples() %>%
      dplyr::mutate(dominant_taxa=factor(dominant_taxa, levels=my_levels),
                    !!my_selected_var:=factor(get(my_selected_var))) %>%
      dplyr::group_by(across(all_of(c("dominant_taxa", my_selected_var))), .drop=FALSE) %>%
      dplyr::summarise(n=dplyr::n(), .groups = "drop") %>%
      dplyr::group_by(across(all_of(my_selected_var))) %>%
      dplyr::mutate(category_total = sum(n),
             taxa_perc = (n/category_total)*100,
             labels = scales::percent(taxa_perc, scale=1)) %>%
      dplyr::left_join(my_dominant_taxa_counts()[,c("dominant_taxa","taxa_colors")],
                       by=dplyr::join_by(dominant_taxa))
  })

  my_dominant_taxa_by_variable_contingency <- reactive({
    req(my_dominant_taxa_counts_by_categories, input$dom_taxa_barplot_variable)
    if(input$dom_taxa_barplot_stratify_test != "No"){
      my_selected_var <- input$dom_taxa_barplot_variable
      #my_dominant_taxa_counts_by_categories() %>%
      #  dplyr::select(dominant_taxa, {{my_selected_var}}, n) %>%
      #  tidyr::spread({{my_selected_var}}, n)
      table(loaded_data$my_data$samples[["dominant_taxa"]],
            loaded_data$my_data$samples[[my_selected_var]])
    }
  })

  observeEvent(my_dominant_taxa_by_variable_contingency(),{
    #TODO
    bslib::sidebar_toggle(id="my_dominant_taxa_sidebar", open=TRUE)
  })

  # Generate output with contingency table
  output$my_dominant_taxa_by_variable_contingency <- renderTable({
    req(my_dominant_taxa_counts_by_categories, my_dominant_taxa_stratify_var)
    my_selected_var <- my_dominant_taxa_stratify_var()
    my_dominant_taxa_counts_by_categories() %>%
      dplyr::select(dominant_taxa, !!my_selected_var, n) %>%
      tidyr::spread(!!my_selected_var, n)
    #print(my_dominant_taxa_by_variable_contingency())
  })

  output$my_dominant_taxa_by_variable_test_out <- renderPrint({
    req(my_dominant_taxa_by_variable_contingency())
    if(input$dom_taxa_barplot_stratify_test == "Fisher"){
      fisher.test(my_dominant_taxa_by_variable_contingency())
    }else if(input$dom_taxa_barplot_stratify_test == "Chi-Square"){
      chisq.test(my_dominant_taxa_by_variable_contingency())
    }

  })

  output$my_dominant_taxa_stratify_var <- renderText(my_dominant_taxa_stratify_var())

  output$my_dominant_taxa_stratify_test <- renderText(my_dominant_taxa_stratify_test())

  # Plot bar plot of dominant taxa
  # -------------------------------
  output$my_dominant_taxa_barplot <- plotly::renderPlotly({
    req(my_dominant_taxa_counts)
    my_taxa_colors <- my_dominant_taxa_counts()[["taxa_colors"]]
    my_taxa_levels <- levels(my_dominant_taxa_counts()[["dominant_taxa"]])

    if(input$dom_taxa_barplot_perc){
      my_x_var <- "taxa_perc"
      my_x_title <- "Frecuencia porcentual (%)"
      my_text_var <- "labels"
    }else if(!input$dom_taxa_barplot_perc){
      my_x_var <- "n"
      my_x_title <- "Frecuencia"
      my_text_var <- "n"
    }

    if(input$dom_taxa_barplot_stratify){
      # Stratify data by variable
      req(my_dominant_taxa_counts_by_categories, input$dom_taxa_barplot_variable,
          cancelOutput = TRUE)

      if(input$dom_taxa_barplot_stratify_yaxis=="variable seleccionada"){
        my_y_var <- input$dom_taxa_barplot_variable
        my_y_var_order <- levels(my_dominant_taxa_counts_by_categories()[[my_y_var]])
        my_y_title <- my_y_var
        my_color_var <- "dominant_taxa"
        my_color_var_order <-
          my_colors <- setNames(my_taxa_colors, my_taxa_levels)
        my_legend_order_mode <- "reversed"
        my_data <- my_dominant_taxa_counts_by_categories()%>%
          dplyr::mutate(dominant_taxa = factor(dominant_taxa,
                                               levels=rev(my_taxa_levels)))
      }else{
        my_y_var <- "dominant_taxa"
        my_y_var_order <- rev(my_taxa_levels)
        my_y_title <- "Taxón"
        my_color_var <- input$dom_taxa_barplot_variable
        my_colors <- RColorBrewer::brewer.pal(n=12, name="Paired")
        my_legend_order_mode <- "reversed"
        my_data <- my_dominant_taxa_counts_by_categories()
      }

      my_data %>%
        plotly::plot_ly(source="dom_taxa_bars_plot",
                        #y=~get(input$dom_taxa_barplot_variable),
                        y=~get(my_y_var),
                        x=~get(my_x_var),
                        type="bar",
                        orientation = "h",
                        color=~get(my_color_var),
                        #marker = list(color = ~taxa_colors),
                        text=~get(my_text_var),
                        textposition="outside",
                        colors = my_colors
        ) %>%
        plotly::layout(barmode="group",
               uniformtext=list(minsize=8, mode='hide'),
               xaxis=list(title=my_x_title),
               yaxis=list(title=my_y_title,
                          category="array",
                          categoryarray=my_y_var_order),
               legend=list(orientation = "h",
                           xanchor = "center",
                           x = 0.5, y = -0.3,
                           traceorder=my_legend_order_mode)) %>%
        plotly::config(displaylogo = FALSE, locale = "es",
               edits = list(axisTitleText = TRUE, titleText = TRUE,
                            legendPosition = FALSE, legendText = FALSE),
               modeBarButtonsToRemove = c("lasso2d",
                                          "hoverCompareCartesian",
                                          "zoomOut2d","zoomIn2d")) %>%
        plotly::event_register('plotly_click') %>%
        plotly::toWebGL()
    }else{
      # Without stratifying
      my_colors <- setNames(my_taxa_colors, my_taxa_levels)
      dominant_taxa_counts <- my_dominant_taxa_counts()
      taxa_levels <- levels(dominant_taxa_counts$dominant_taxa)

      if(input$dom_taxa_barplot_stack){
        # Stacked bars
        my_y_var <- ""
        my_barmode <- "stack"
        legend_status <- TRUE
        my_barplot <- dominant_taxa_counts %>%
          plotly::plot_ly(source="dom_taxa_bars_plot",
                          y="", x=~get(my_x_var),
                          color = ~dominant_taxa,
                          text = ~get(my_x_var),
                          textposition="outside",
                          colors = my_colors,
                          type="bar",
                          orientation = "h")
      }else if (!input$dom_taxa_barplot_stack){
        # Separated bars
        my_y_var <- "dominant_taxa"
        my_barmode <- "group"
        legend_status <- FALSE
        my_barplot <- dominant_taxa_counts %>%
          plotly::plot_ly(source="dom_taxa_bars_plot",
                          y=~dominant_taxa, x=~get(my_x_var),
                          color = ~dominant_taxa,
                          text = ~get(my_x_var),
                          textposition="outside",
                          colors = my_colors,
                          type="bar",
                          orientation = "h")
      }
      my_barplot %>%
        plotly::layout(barmode=my_barmode,
               showlegend=legend_status,
               xaxis=list(title=my_x_title),
               yaxis=list(categoryorder="array",
                          categoryarray=rev(taxa_levels)),
               legend=list(orientation="h",
                           xanchor = "center",
                           x = 0.5, y = -0.3,
                           traceorder="normal"
               )) %>%
        plotly::config(displaylogo = FALSE, locale = "es",
               edits = list(axisTitleText = TRUE, titleText = TRUE,
                            legendPosition = FALSE, legendText = FALSE),
               modeBarButtonsToRemove = c("lasso2d",
                                          "hoverCompareCartesian",
                                          "zoomOut2d","zoomIn2d")) %>%
        plotly::event_register('plotly_click') %>%
        plotly::toWebGL()
    }

  })

  # Update switch bottoms in certain events
  observeEvent(
    {
      input$dom_taxa_barplot_stratify
      input$dom_taxa_barplot_variable
      input$dom_taxa_barplot_stratify_yaxis},
    {
      shinyWidgets::updateMaterialSwitch(session, inputId = "dom_taxa_barplot_stack",
                           value=FALSE)
      shinyWidgets::updateMaterialSwitch(session, inputId = "dom_taxa_barplot_perc",
                           value=FALSE)
    })

  observeEvent({input$dom_taxa_barplot_perc},{
    if(input$dom_taxa_barplot_stratify){
      shinyWidgets::updateMaterialSwitch(session, inputId = "dom_taxa_barplot_stack",
                           value=FALSE)
    }
  })

  # Update bar plot if stack switch activated
  observe({
    if(input$dom_taxa_barplot_stratify & input$dom_taxa_barplot_stack){
      plotly::plotlyProxy("my_dominant_taxa_barplot", session) %>%
        plotly::plotlyProxyInvoke(
          "relayout", list(barmode="stack",
                           uniformtext=list(minsize=8, mode='hide'))
        )
    } else if (input$dom_taxa_barplot_stratify & !input$dom_taxa_barplot_stack) {
      plotly::plotlyProxy("my_dominant_taxa_barplot", session) %>%
        plotly::plotlyProxyInvoke(
          "relayout", list(barmode="group",
                           uniformtext=list(minsize=8, mode='hide'))
        )
    }
  })

  # Plot pie chart of dominant taxa
  #---------------------------------

  output$my_dominant_taxa_pie <-  plotly::renderPlotly({
    req(my_dominant_taxa_counts)

    dominant_taxa_counts <- my_dominant_taxa_counts()

    dominant_taxa_counts <- dominant_taxa_counts %>%
      dplyr::mutate(dominant_taxa_grouped = replace(as.character(dominant_taxa),
                                                    (dominant_taxa!="No dominance" & taxa_perc<=1),
                                                    "Other")) %>%
      dplyr::group_by(dominant_taxa_grouped) %>%
      dplyr::summarise(n=sum(n)) %>%
      dplyr::arrange(desc(n))

    no_dominance_ix <- which(dominant_taxa_counts$dominant_taxa_grouped == "No dominance")
    other_ix <- which(dominant_taxa_counts$dominant_taxa_grouped == "Other")

    my_levels <- c(dominant_taxa_counts$dominant_taxa_grouped[-c(no_dominance_ix,other_ix)], "Other","No dominance")

    my_colors_ix <- which((seq(length(my_levels)) != no_dominance_ix & seq(length(my_levels)) != other_ix))

    my_colors <- rep(my_color_palette[11], length(my_levels))
    my_colors[no_dominance_ix] <- my_color_palette[10]
    #my_colors[other_ix] <- my_color_palette[11]
    if(length(my_colors_ix)<10){
      my_colors[my_colors_ix] <- my_color_palette[1:length(my_colors_ix)]
    }else{
      my_colors[my_colors_ix[1:9]] <- my_color_palette[1:9]
    }

    dominant_taxa_counts <- dominant_taxa_counts %>%
      dplyr::mutate(dominant_taxa_grouped = factor(dominant_taxa_grouped, levels=my_levels),
                    taxa_color = my_colors)

    my_pie_colors <- dominant_taxa_counts %>%
      dplyr::arrange(dominant_taxa_grouped) %>% dplyr::pull(taxa_color)

    my_pie <- dominant_taxa_counts %>%
      dplyr::arrange(dominant_taxa_grouped) %>%
      plotly::plot_ly(type="pie", labels=~dominant_taxa_grouped, values=~n,
              sort=FALSE, direction="clockwise",
              marker = list(colors = my_pie_colors),
              #domain = list(x=c(0.75, 0.75), y=c(0.2, 0.7)),
              textinfo="percent",
              insidetextorientation="horizontal") %>%
      plotly::layout(legend=list(orientation = "h", title = list(text = "Taxón")),
             showlegend = FALSE) %>%
      plotly::config(displaylogo = FALSE, locale = "es",
             edits = list(axisTitleText = TRUE, titleText = TRUE,
                          legendPosition = FALSE, legendText = FALSE),
             modeBarButtonsToRemove = c("lasso2d",
                                        "hoverCompareCartesian",
                                        "zoomOut2d","zoomIn2d"))
    my_pie %>% plotly::toWebGL()

  })

  my_dominant_taxa_onclick_data <- reactive({
    req(my_dominant_taxa_counts)
    plotly::event_data("plotly_click", source = "dom_taxa_bars_plot")
  })

  my_dominant_taxa_onclick_samples <- reactive({
    req(loaded_data$my_data, my_dominant_taxa_onclick_data())
    loaded_data$my_data$samples %>%
      dplyr::filter(dominant_taxa == my_dominant_taxa_onclick_data()$y) %>%
      dplyr::pull(sample_id)
  })

  observeEvent(my_dominant_taxa_onclick_data(), {
    req(my_dominant_taxa_counts, my_dominant_taxa_onclick_data())

    if(!input$dom_taxa_barplot_stratify){
      bslib::sidebar_toggle(id="my_dominant_taxa_sidebar", open=TRUE)

      if(length(my_dominant_taxa_onclick_samples()) > 19){
        updateTabsetPanel(inputId = "my_dominant_taxa_sidebar_plot_menu",
                          selected = "my_dominant_taxa_sidebar_plot_cont")
      }else{
        updateTabsetPanel(inputId = "my_dominant_taxa_sidebar_plot_menu",
                          selected = "my_dominant_taxa_sidebar_plot_bars")
      }
    }
  })

  output$my_dominant_taxa_name <- renderText({
    req(my_dominant_taxa_counts, my_dominant_taxa_onclick_data())
    my_dominant_taxa_onclick_data()$y
  })

  output$my_dominant_taxa_barplot_click <- renderText({
    req(my_dominant_taxa_counts, my_dominant_taxa_onclick_data())
    if(is.null(my_dominant_taxa_onclick_data())){
      return("Selecciona una barra del diagrama de barras")}

    #loaded_data$my_data$samples %>%
    #  dplyr::filter(dominant_taxa == my_dominant_taxa_onclick_data()$y) %>%
    #  dplyr::pull(sample_id)
    my_dominant_taxa_onclick_samples()
    #my_dominant_taxa_onclick_data()
  })

  output$my_dominant_taxa_barplot_click_plot <- renderPlot({
    req(loaded_data$my_data, my_dominant_taxa_counts,
        my_dominant_taxa_onclick_data(),
        my_dominant_taxa_onclick_samples())
    #
    col_plot_type <- tolower(input$dt_plot_choice)
    col_plot_hist_bins <- input$dt_plot_hist_bins
    col_name <- "top_genus1_rel_abundance"
    my_df_subset <- loaded_data$my_data$samples %>%
      dplyr::filter(sample_id %in% my_dominant_taxa_onclick_samples()) %>%
      dplyr::select(sample_id, top_genus1_rel_abundance)

    if(length(my_dominant_taxa_onclick_samples()) > 19){
      req(col_plot_type)
      plot_num_col_from_df(my_df = my_df_subset,
                           my_col_ix = 2,
                           plot_type = col_plot_type,
                           n_bins = col_plot_hist_bins) +
        xlim(0.0,1.0)
    }else{
      #plot_disc_col_from_df(my_df = my_df_subset,
      #                      my_col_ix = 2)
      my_df_subset %>%
        #dplyr::arrange(desc(top_genus1_rel_abundance)) %>%
        ggplot2::ggplot(aes(top_genus1_rel_abundance,
                   reorder(sample_id,top_genus1_rel_abundance))) +
        ggplot2::geom_col() +
        ggplot2::xlim(0.0, 1.0)
    }
  }

  )

  observe({
    if(input$dom_taxa_pie_legend){
      plotly::plotlyProxy("my_dominant_taxa_pie", session) %>%
        plotly::plotlyProxyInvoke(
          "relayout", list(showlegend=TRUE,
                           annotations = list(insidetextorientation="horizontal"))
        )
    } else {
      plotly::plotlyProxy("my_dominant_taxa_pie", session) %>%
        plotly::plotlyProxyInvoke(
          "relayout", list(showlegend=FALSE,
                           annotations = list(insidetextorientation="horizontal"))
        )
    }
  })

  #--------------------------------
  # Populate "Profiles" accordion
  #--------------------------------

  # Update list of choices for taxonomic ranks
  observeEvent(input$profiles_aggregate,{
    req(my_taxonomic_ranks)


    shinyWidgets::updatePickerInput(inputId = "profiles_select_rank",
                                    choices = my_taxonomic_ranks(),
                                    selected = my_taxonomic_ranks()[length(my_taxonomic_ranks())]
    )
  })

  # Update list of variables
  observeEvent(input$profiles_stratify,{
    req(samples_columns_non_numeric)

    shinyWidgets::updatePickerInput(inputId = "profiles_stratify_variable",
                                    choices = samples_columns_non_numeric())
  })


  my_profiles_variable_categories <- reactive({
    req(input$profiles_stratify_variable)
    my_selected_var <- input$profiles_stratify_variable
    levels(as.factor(loaded_data$my_data$samples[[my_selected_var]]))
  })

  # Update list of categories according to selected variable
  observeEvent(my_profiles_variable_categories(), {
    freezeReactiveValue(input, "profiles_stratify_variable_categories")
    shinyWidgets::updateCheckboxGroupButtons(
      inputId = "profiles_stratify_variable_categories",
      choices = my_profiles_variable_categories(),
      selected = my_profiles_variable_categories())
  })

  #
  my_profiles_variable_categories_selected <- reactive({
    input$profiles_stratify_variable_categories
  })

  #
  output$profiles_variable_select <- renderPrint({
    paste(length(my_profiles_variable_categories_selected()),
          length(my_profiles_variable_categories()))
  })

  #
  observeEvent(input$profiles_stratify_variable_mostrar_categories, {
    shinyWidgets::updateCheckboxGroupButtons(
      inputId = "profiles_stratify_variable_categories",
      selected = my_profiles_variable_categories())
  })

  observeEvent(input$profiles_stratify_variable_clear_categories, {
    shinyWidgets::updateCheckboxGroupButtons(
      inputId = "profiles_stratify_variable_categories",
      selected = character(0))
  })

  # Aggregate data by taxonomic rank
  my_data_aggregated_by_rank <-reactive({
    req(input$profiles_select_rank)
    loaded_data$my_data %>%
      tidytacos::aggregate_taxa(rank=input$profiles_select_rank)
  })

  # Update value of profiles to plot
  profiles_table_to_plot <- reactive({

    if(!input$profiles_aggregate & !input$profiles_stratify){
      req(loaded_data$my_data)
      my_data_to_plot <- loaded_data$my_data %>%
        tidytacos:::prepare_for_bp() %>%
        dplyr::select(taxon_id,
                      sample_id,
                      count,
                      rel_abundance,
                      sample_clustered,
                      taxon_name_color)
    } else if(!input$profiles_aggregate & input$profiles_stratify){
      req(loaded_data$my_data, input$profiles_stratify_variable)
      # The name of the variable to stratify profiles
      my_variable <- input$profiles_stratify_variable
      my_data_to_plot <- loaded_data$my_data %>%
        tidytacos:::prepare_for_bp() %>%
        dplyr::select(taxon_id,
                      sample_id,
                      count,
                      rel_abundance,
                      !!my_variable,
                      sample_clustered,
                      taxon_name_color)
    } else if (input$profiles_aggregate & !input$profiles_stratify){
      req(input$profiles_select_rank, my_data_aggregated_by_rank())
      my_data_to_plot <- my_data_aggregated_by_rank() %>%
        tidytacos:::prepare_for_bp() %>%
        dplyr::select(taxon_id,
                      sample_id,
                      count,
                      rel_abundance,
                      sample_clustered,
                      taxon_name_color)
    } else if (input$profiles_aggregate & input$profiles_stratify){
      req(my_data_aggregated_by_rank(), input$profiles_stratify_variable)
      my_variable <- input$profiles_stratify_variable
      my_data_to_plot <- my_data_aggregated_by_rank() %>%
        tidytacos:::prepare_for_bp() %>%
        dplyr::select(taxon_id,
                      sample_id,
                      count,
                      rel_abundance,
                      !!my_variable,
                      sample_clustered,
                      taxon_name_color)
    }
    #profiles_table_to_plot(my_data_to_plot)
    my_data_to_plot
  })

  # Levels of taxa, ordered according to mean relative abundance
  profiles_taxa_levels <- reactive({
    req(profiles_table_to_plot())
    profiles_table_to_plot() %>%
      dplyr::group_by(taxon_name_color, .drop=FALSE) %>%
      dplyr::summarise(mean_ra = mean(rel_abundance), .groups = "drop") %>%
      dplyr::filter(taxon_name_color != "Other taxa") %>%
      dplyr::arrange(desc(mean_ra)) %>%
      dplyr::pull(taxon_name_color) %>%
      as.character() %>%
      append("Other taxa") %>%
      rev()
  })

  # Function to plot profiles
  # TODO

  # List of stacked abundances plots, one per category
  profiles_plots_for_each_variable_category <- reactive({
    req(profiles_table_to_plot(), profiles_taxa_levels(),
        input$profiles_stratify_variable, my_profiles_variable_categories())

    my_variable <- input$profiles_stratify_variable

    profiles_table_to_plot_splitted <- profiles_table_to_plot() %>%
      dplyr::mutate(taxon_name_color = factor(taxon_name_color,
                                              levels = profiles_taxa_levels())) %>%
      split(~get(my_variable))


    nr_subplots <- length(profiles_table_to_plot_splitted)

    my_profiles_subplots <- seq(nr_subplots) %>%
      purrr::map(~{
        show_legend_status <- if(.x==1) TRUE else FALSE
        my_catg_name <- my_profiles_variable_categories()[.x]
        my_subplot_title <- glue::glue("<b>{my_catg_name}</b>")
        profiles_table_to_plot_splitted[[.x]] %>%
          dplyr::mutate(sample_clustered = droplevels(sample_clustered)) %>%
          plotly::plot_ly(
            #x = ~forcats::fct_reorder(sample_clustered, as.integer(sample_clustered)),
            x= ~sample_clustered,
            y = ~rel_abundance,
            color = ~taxon_name_color,
            colors = palette_xgfs,
            name = ~taxon_name_color,
            hovertemplate = paste("<b>%{x}</b>",
                                  "<br>%{y:.2%}<br>"),
            type = "bar",
            legendgroup=~taxon_name_color,
            showlegend = show_legend_status) %>%
          plotly::add_annotations(text=my_subplot_title, x=0, y=1.05,
                                  xref="paper", xanchor="left",
                                  showarrow=FALSE) %>%
          plotly::layout(barmode = "stack",
                         xaxis = list(title = "Muestras", showticklabels=FALSE),
                         yaxis = list(title = "Abundancia Relativa"),
                         legend = list(title = list(text = "Taxón")))
      })
    names(my_profiles_subplots) <- my_profiles_variable_categories()
    my_profiles_subplots
  })

  output$profiles_stacked <- plotly::renderPlotly({
    if(input$profiles_stratify && !is.null(input$profiles_stratify_variable)){
      req(input$profiles_stratify_variable,
          profiles_plots_for_each_variable_category(),
          my_profiles_variable_categories(),
          my_profiles_variable_categories_selected())

      my_selected_var <- input$profiles_stratify_variable
      my_selected_catg <- my_profiles_variable_categories_selected()
      nr_selected_catg <- length(my_selected_catg)
      # Start creating the plots
      if(nr_selected_catg == 1){
        my_plots <- profiles_plots_for_each_variable_category()[[my_selected_catg]]
        my_plots$x$attrs[[1]]$showlegend <- TRUE
        my_plots
      }else if(nr_selected_catg == length(my_profiles_variable_categories())){
        my_plots <- profiles_plots_for_each_variable_category() %>%
          plotly::subplot(margin = .05, nrows = 2, shareY = TRUE)
      }else{
        my_plots <- profiles_plots_for_each_variable_category()[my_selected_catg]
        my_plots[[1]]$x$attrs[[1]]$showlegend <- TRUE
        my_plots <- plotly::subplot(my_plots, margin = .05, nrows = 2, shareY = TRUE)
      }

      my_plots %>%
        plotly::layout(
          title = list(text = my_selected_var,
                       xref = "paper", x=0, xanchor = "left",
                       yref = "paper", yanchor = "top"
          ),
          legend = list(traceorder = "reversed",
                        title = list(text = "Taxón"))) %>%
        plotly::config(displaylogo = FALSE, locale = "es",
               edits = list(axisTitleText = TRUE, titleText = TRUE,
                            legendPosition = FALSE, legendText = FALSE),
               modeBarButtonsToRemove = c("lasso2d",
                                          "hoverCompareCartesian",
                                          "zoomOut2d","zoomIn2d"))

    } else {
      req(profiles_table_to_plot, profiles_taxa_levels)
      profiles_table_to_plot() %>%
        dplyr::mutate(taxon_name_color = factor(taxon_name_color,
                                                levels = profiles_taxa_levels())) %>%
        plotly::plot_ly(x = ~forcats::fct_reorder(sample_clustered, as.integer(sample_clustered)),
                        y = ~rel_abundance,
                        color = ~taxon_name_color,
                        colors = palette_xgfs,
                        name = ~taxon_name_color,
                        hovertemplate = paste("<b>%{x}</b>",
                                              "<br>%{y:.2%}<br>"),
                        type = "bar") %>%
        plotly::config(displaylogo = FALSE, locale = "es",
               edits = list(axisTitleText = TRUE, titleText = TRUE,
                            legendPosition = FALSE, legendText = FALSE),
               modeBarButtonsToRemove = c("lasso2d",
                                          "hoverCompareCartesian",
                                          "zoomOut2d","zoomIn2d")) %>%
        plotly::toWebGL() %>%
        plotly::layout(barmode = "stack",
                       xaxis = list(title = "Muestras", showticklabels=FALSE),
                       yaxis = list(title = "Abundancia Relativa"),
                       legend = list(title = list(text = "Taxón")))
    }
  })

  shiny::observeEvent({
    input$profiles_stratify_apply_test
    input$profiles_stratify},
    {
      req(input$profiles_stratify_apply_test, input$profiles_stratify)
      if(input$profiles_stratify_apply_test){
        bslib::sidebar_toggle(id="my_profiles_sidebar", open = TRUE)
      } else {
        bslib::sidebar_toggle(id="my_profiles_sidebar", open = CLOSE)
      }
    })

  # Selected Variable stored as reactive value
  my_profiles_stratify_var <- reactiveVal()

  observeEvent(input$profiles_stratify_variable,{
    my_profiles_stratify_var(input$profiles_stratify_variable)
  })

  output$my_profiles_stratify_var <- renderText(my_profiles_stratify_var())

  my_profiles_by_variable_test_out <- reactiveVal()

  observeEvent({
    input$profiles_aggregate
    input$profiles_select_rank
    input$profiles_stratify
    input$profiles_stratify_apply_test
    my_profiles_stratify_var()},{
      if(input$profiles_stratify && input$profiles_stratify_apply_test){
        req(my_profiles_stratify_var())
        my_var <- my_profiles_stratify_var()
        if(input$profiles_aggregate){
          req(my_profiles_stratify_var(), my_data_aggregated_by_rank())
          my_adonis2_results <- my_data_aggregated_by_rank() %>%
            tidytacos::perform_adonis(my_var)
        }else if(input$profiles_aggregate == FALSE){
          req(my_profiles_stratify_var(), loaded_data$my_data)
          my_adonis2_results <- loaded_data$my_data %>%
            tidytacos::perform_adonis(my_var)
        }
        my_profiles_by_variable_test_out(my_adonis2_results)
      } else {
        my_profiles_by_variable_test_out(NULL)
      }
    })

  output$my_profiles_by_variable_test_out <- renderPrint({
    req(my_profiles_stratify_var, my_profiles_by_variable_test_out())
    my_profiles_by_variable_test_out()
  })
}
