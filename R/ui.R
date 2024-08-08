#' The Application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'
#' @import shiny magrittr
#' @noRd
ui <- function(request){
  #shiny::addResourcePath('www', fs::path_package("app/www", package='VTOOL'))
  # Value boxes
  my_data_value_boxes <- list(
    bslib::value_box(
      title = "Nr. de Muestras",
      value = textOutput("total_sample"),
      showcase = icon("vial")#,
      #p("Some text"),
      #p("More text ...")
      #full_screen = FALSE,
      #theme = "success"
    ),
    bslib::value_box(
      title = "Nr. de Taxones",
      value = textOutput("total_taxa"),
      showcase = icon("bacteria")#,
      #p("Some text"),
      #p("More text ...")
    ),
    bslib::value_box(
      title = "Lecturas Totales",
      value = textOutput("total_reads"),
      showcase = icon("dna")#,
      #p("Some text"),
      #p("More text ...")
    )
  )

  bslib::page_navbar(
    window_title = "VTOOL",
    title = span(tags$img(src = "www/vtool_logo_2.svg", height = 100, width = 100),
                 class = "header-title"),
    id = "my_navbar",
    theme = bslib::bs_theme(version = 5, preset = "yeti", primary = "#00719a"),
    # The Main sidebar
    sidebar = bslib::sidebar(
      id = "my_sidebar",
      title = span(bsicons::bs_icon("funnel"), "Filtrar Datos", class = "sidebar-title"),
      open = FALSE,
      bg = "white",
      bslib::accordion(
        open = FALSE,
        bslib::accordion_panel(
          title ="Filtrar muestras",
          icon = icon("vial"),
          sliderInput(
            "sample_total_count",
            label = "Lecturas totales mínimo",
            min = 1,
            max = 10000,
            value = 1
          ),
          selectizeInput(inputId = "sampleIDs_to_filter",
                         label = "Muestras IDs",
                         choices = NULL,
                         selected = NULL,
                         multiple = TRUE,
                         options = list(delimiter = " ", create = T,
                                        placeholder = "Enter Sample_ID"))
        ),
        bslib::accordion_panel(
          title = "Filtrar taxones",
          icon = icon("bacteria"),
          sliderInput(
            inputId = "taxa_mean_ra",
            label = "Abundancia relativa mínima (media)",
            min = 0,
            max = 0.5,
            value = 0
          ),
          sliderInput(
            inputId = "taxa_prevalence",
            label = "Prevalencia en las muestras",
            min = 1,
            max = 100,
            value = 1
          ),
          selectizeInput(inputId = "taxaIDs_to_filter",
                         label = "Taxon IDs",
                         choices = NULL,
                         selected = NULL,
                         multiple = TRUE,
                         options = list(delimiter = " ", create = T,
                                        placeholder = "Enter ID(s) of Taxón"))
        )
      ),
      actionButton(
        "input_apply_filter",
        label = "Filtrar"
      ),
      actionButton(
        "input_clean_filter",
        label = "Remover filtros"
      )),
    # The tabs inside the navigation bar
    ####################################
    # First tab
    #----------
    bslib::nav_panel(
      title = "Inicio",
      value = "start",
      # The card with content
      bslib::card_body(fillable = FALSE,
                bslib::layout_columns(
                  col_widths = c(8,4),
                  # First element of the card
                  tags$div(class = "jumbotron text-left",
                           style = "margin-bottom:0px;margin-top:0px;margin-right:100px",
                           tags$img(src="www/vtool_logo_1.svg", width=150, height=150,
                                    style="margin-left:0px;padding-left:0px"),
                           tags$h3(class = 'jumbotron-heading',
                                   stye = 'margin-bottom:0px;margin-top:0px',
                                   'Un explorador de datos para estudiar el microbioma vaginal'),
                           #p(lorem::ipsum(paragraphs = 2, sentences = 5))
                           br(),
                           p("VTOOL es una herramienta basada en Shiny para el análisis
                         exploratorio de datos. A través de una interface
                           interactiva e intuitiva, esta herramienta busca facilitar
                           la exploración y visualización de datos asociados al estudio
                           del microbioma vaginal."),
                           br(),
                           p("VTOOL es un proyecto de código abierto (open source),
                           desarrollado en colaboración con investigadores y bioninformáticos,
                           quienes buscan fomentar el estudio del microbioma vaginal.
                           Si te interesa contribuir, reportar errores, o saber más de
                           este proyecto, por favor",
                             tags$a(href="https://github.com/mticlla/vtool", "visita nuestro
                           repositorio en GitHub"), ".", .noWS = "outside")
                  ),
                  # Second element
                  bslib::card_body(
                    fillable = FALSE,
                    fill = FALSE,
                    tags$h3(class = 'jumbotron-heading', 'Empezemos cargando tus datos'),
                    p("1. Selecciona el formato del archivo:"),
                    shinyWidgets::awesomeRadio(
                      "input_format",
                      label = "Formato:",
                      choices = c("Tidytacos", "Biom"),
                      selected = "Tidytacos", inline = T),
                    # tags$br(),
                    p("2. Encuentra tu(s) archivos:"),
                    uiOutput("load_file_options"),
                    tags$br(),
                    actionButton(
                      "input_my_data",
                      label = "Importar datos",
                      width = "100%",
                      disabled = FALSE
                    ) %>% bslib::tooltip("¡No olvides cargar tus archivos antes de presionar este botón!"),
                    tags$br(),
                    tags$br(),
                    tags$h3(class = 'jumbotron-heading', 'O carga un dataset ejemplo'),
                    actionButton(
                      "input_example",
                      label = "Cargar ejemplo",
                      width = "100%"
                    ),
                    tags$br(), tags$br()
                    #br(),
                    # card_body(
                    #   fillable = FALSE,
                    #   tags$br(),
                    #   tags$h3("O carga nuestro dataset ejemplo"),
                    #   actionButton(
                    #     "input_example",
                    #     label = "Cargar ejemplo",
                    #     width = "100%"
                    #   )
                    # )
                  )
                ))
    ),
    bslib::nav_panel(
      title = "Datos",
      value = "my_data",
      # the card with content
      bslib::layout_column_wrap(
        width = 1/3,
        fill = FALSE,
        height = "150px",
        #width = "250px",
        !!!my_data_value_boxes
      ),
      bslib::navset_pill(
        id="my_data_tabs",
        # Muestras y metadata
        bslib::nav_panel(
          title = "Muestras y metadata",
          bslib::navset_underline(id="my_samples_navbar",
                           bslib::nav_panel("Explorar Tabla",
                                     bslib::card(
                                       class="table-responsive",
                                       full_screen = TRUE,
                                       bslib::layout_sidebar(
                                         sidebar = bslib::sidebar(id="data_samples_table_sidebar",
                                                           position = "right",
                                                           width = 400,
                                                           open = FALSE,
                                                           tags$span(strong("Variable seleccionada: "),
                                                                     textOutput("my_col_name")),
                                                           #textOutput("my_col_name"),
                                                           uiOutput("my_sample_col_plot_options"),
                                                           bslib::card(fullscreen = TRUE,
                                                                plotOutput("my_samples_col_plots") %>%
                                                                  shinycssloaders::withSpinner(color="#00719a",
                                                                              caption = "Cargando gráfico"))

                                         ),
                                         p("Esta vista te permite explorar las muestras y su metadata."),
                                         DT::dataTableOutput(outputId = "my_samples", width = "100%") %>%
                                           shinycssloaders::withSpinner(color="#00719a", caption = "Cargando tabla")
                                       ))
                           ),
                           bslib::nav_panel("Data Faltante", value = "my_samples_na_tab",
                                     bslib::card(
                                       full_screen = TRUE,
                                       #card_header("Data Faltante"),
                                       tags$span(class = "text-left",
                                                 p("Esta vista permite explorar patrones
                                             de valores faltantes (NA) en la tabla de
                                             muestras. El gráfico que mostramos es
                                             una representación gráfica de la tabla,
                                             donde las celdas que contienen valores son
                                             de color ",
                                                   span("gris claro", style="color:#333333;"),
                                                   " y las celdas con valores faltantes son
                                             de color",
                                                   span("gris oscuro", style="color:#cccccc;"),"."),
                                                 p("Además, el orden de las filas y las columnas
                                             ha sido re-organizado de tal modo que muestras
                                             con valores faltantes en las mismas columnas son
                                             agrupadas. De igual manera, columnas con valores
                                             faltantes en las mismas muestras son agrupadas.")
                                       ),
                                       plotly::plotlyOutput("my_samples_nas") %>%
                                         shinycssloaders::withSpinner(color="#00719a",
                                                     caption = "Cargando gráfico")
                                     )
                           ),
                           bslib::nav_spacer())
        ),
        bslib::nav_panel(
          title = "Taxones y metadata",
          bslib::card(
            full_screen = TRUE,
            bslib::layout_sidebar(
              sidebar = bslib::sidebar(id="data_taxa_table_sidebar",
                                position = "right",
                                width = 400,
                                open = FALSE,
                                tags$span(strong("Variable seleccionada: "),
                                          textOutput("my_taxa_col_name")),
                                #textOutput("my_taxa_col_name"),
                                uiOutput("my_taxa_col_plot_options"),
                                plotOutput("my_taxa_col_plots") %>%
                                  shinycssloaders::withSpinner(color="#00719a",
                                              caption = "Cargando gráfico")
              ),
              p("Esta vista te permite explorar las taxa y su metadata."),
              DT::DTOutput(outputId = "my_taxa", width = "100%") %>%
                shinycssloaders::withSpinner(color="#00719a", caption = "Cargando tabla")
            ))
        ),
        bslib::nav_panel(
          title = "Abundancias",
          bslib::card(
            full_screen = TRUE,
            bslib::layout_sidebar(
              sidebar = bslib::sidebar(id = "data_counts_table_sidebar",
                                position = "right",
                                width = 400,
                                open = FALSE),
              p("Esta vista te permite explorar la abundancia de cada taxón en cada muestra."),
              DT::DTOutput(outputId = "my_counts", width = "100%") %>%
                shinycssloaders::withSpinner(color="#00719a", caption = "Cargando tabla")
            )
          )
        )
      )
    ),
    # 3rd tab
    #----------
    bslib::nav_panel(
      title = "Composición",
      value = "my_community",
      bslib::navset_pill(
        id="my_community_tabs",
        # Dominant taxa
        #--------------
        bslib::nav_panel(
          title = "Taxón dominante",
          bslib::layout_sidebar(
            border = FALSE,
            sidebar = bslib::sidebar(id="my_dominant_taxa_sidebar",
                              position = "right",
                              open = FALSE,
                              width = 400,
                              conditionalPanel(
                                condition = "input.dom_taxa_barplot_stratify==false",
                                tags$b("Taxón domiante seleccionado:"),
                                textOutput("my_dominant_taxa_name"),
                                verbatimTextOutput("my_dominant_taxa_barplot_click"),
                                #
                                tabsetPanel(
                                  id="my_dominant_taxa_sidebar_plot_menu",
                                  type="hidden",
                                  tabPanel("my_dominant_taxa_sidebar_plot_cont",
                                           radioButtons("dt_plot_choice","Selecciona el tipo de gráfico:",
                                                        c("Density","Histogram","Boxplot"), inline = TRUE
                                           ),
                                           conditionalPanel(
                                             condition = "input.dt_plot_choice=='Histogram'",
                                             sliderInput("dt_plot_hist_bins", "Número de intervalos",
                                                         min=5, max = 100, value = 30)
                                           )),
                                  tabPanel("my_dominant_taxa_sidebar_plot_bars")
                                ),
                                #
                                plotOutput("my_dominant_taxa_barplot_click_plot") %>%
                                  shinycssloaders::withSpinner(color="#00719a")
                              ),
                              conditionalPanel(
                                condition = "input.dom_taxa_barplot_stratify==true",
                                tags$b("Taxón dominante y variable seleccionada"),
                                tags$p(strong("Variable seleccionada: "),
                                       textOutput("my_dominant_taxa_stratify_var")),
                                tags$p(strong("Evaluación de asociación: "),br(),
                                       em("Prueba de asociación: "),
                                       textOutput("my_dominant_taxa_stratify_test")),
                                bslib::navset_underline(
                                  id="my_dom_taxa_barplot_stratify_test_results",
                                  bslib::nav_panel(title="Resultado",
                                            verbatimTextOutput("my_dominant_taxa_by_variable_test_out")),
                                  bslib::nav_panel(title="Tabla de contingencia",
                                            tableOutput("my_dominant_taxa_by_variable_contingency"))
                                )
                              )
            ),
            tags$b("Participantes según taxón dominante"),
            bslib::layout_column_wrap(
              #width = 1/2,
              fill = TRUE,
              #height = "200px",
              width = "300px",
              heights_equal = "row",
              # Pie chart
              bslib::card_body(
                checkboxInput("dom_taxa_pie_legend",
                              label="Mostrar Legenda",
                              value = FALSE),
                plotly::plotlyOutput("my_dominant_taxa_pie") %>%
                  shinycssloaders::withSpinner(color="#00719a")
              ),
              # Bar plot
              bslib::card_body(
                shinyWidgets::materialSwitch(inputId = "dom_taxa_barplot_stratify",
                                             label = "Agrupar participantes según variable",
                                             right = TRUE,
                                             value = FALSE,
                                             status = "primary"),
                conditionalPanel(
                  condition = "input.dom_taxa_barplot_stratify==true",
                  shinyWidgets::pickerInput(inputId = "dom_taxa_barplot_variable",
                                            label = "Seleccionar variable",
                                            choices = c("a","b"),
                                            inline = TRUE, width = "fit",
                                            options = list(title = "seleccionar",
                                                           `live-search` = TRUE))
                ),
                conditionalPanel(
                  condition = "input.dom_taxa_barplot_variable",
                  shinyWidgets::awesomeRadio(inputId = "dom_taxa_barplot_stratify_yaxis",
                                             label = "Agrupar categorias por:",
                                             choices = c("taxa domiante", "variable seleccionada"),
                                             selected = "taxa domiante",
                                             inline = TRUE),
                  shinyWidgets::awesomeRadio(inputId = "dom_taxa_barplot_stratify_test",
                                             label = "Evaluar asociación",
                                             choices = c("No","Fisher", "Chi-Square"),
                                             selected = "No",
                                             inline = TRUE)
                ),
                #
                plotly::plotlyOutput("my_dominant_taxa_barplot") %>%
                  shinycssloaders::withSpinner(color="#00719a",
                                               caption = "Generando figura ... no olvides seleccionar una variable"),
                # Switch for certain visualization options
                bslib::layout_column_wrap(
                  width = "100px",
                  shinyWidgets::materialSwitch(inputId = "dom_taxa_barplot_perc",
                                               label = "Usar frequencia porcentual",
                                               right = TRUE,
                                               value = FALSE,
                                               status = "primary"),
                  conditionalPanel(
                    condition = "input.dom_taxa_barplot_stratify==false || input.dom_taxa_barplot_stratify_yaxis=='variable seleccionada'",
                    shinyWidgets::materialSwitch(inputId = "dom_taxa_barplot_stack",
                                                 label = "Apilar barras",
                                                 right = TRUE,
                                                 value = FALSE,
                                                 status = "primary")
                  )
                )
              )
            )
          )
        ),
        # Profiles
        # --------
        bslib::nav_panel(
          title = "Perfiles",
          bslib::layout_sidebar(
            border = FALSE,
            sidebar = bslib::sidebar(
              id="my_profiles_sidebar",
              position = "right",
              open = FALSE,
              width = 400,
              #tags$b("Perfiles y variable seleccionada"),
              tags$b("Permutational Multivariate Analysis of Variance Using Distance Matrices"),
              tags$p(strong("Variable seleccionada: "),
                     textOutput("my_profiles_stratify_var")),
              tags$span(strong("Evaluación de asociación: "),br(),
                        #em("Prueba de asociación: "),
                        #p("Permutational Multivariate Analysis of Variance Using Distance Matrices"),
                        #textOutput("adonis2")
              ),
              bslib::navset_underline(
                id="my_profiles_stratify_test_results",
                bslib::nav_panel(title="Resultado",
                          verbatimTextOutput("my_profiles_by_variable_test_out")),
                bslib::nav_panel(title="Interpretación",
                          #tableOutput("my_profiles_by_variable_interpret")
                          tags$span(
                            p("PERMANOVA compara cómo varían los grupos entre sí
                        en comparación con cómo varían las personas dentro de cada grupo.
                        Utiliza una estadística llamada pseudo-F, que es parecida a la
                        F-estadística que se usa en ANOVA. Esta estadística puede ser
                        cero o positiva, y los valores más altos indican que el factor
                        de agrupamiento es más importante."),
                            p("PERMANOVA es una técnica muy poderosa y flexible.
                          Se puede usar con datos de cualquier tipo, ya sea simple
                          o complejo, y con cualquier medida de distancia. Además,
                          funciona bien con diseños complicados que incluyen múltiples
                          factores y covariables (predictores continuos).")
                          )
                )
              )
            ),
            bslib::card_body(
              bslib::layout_column_wrap(
                width = "300px",
                heights_equal = "row",
                bslib::card_body(
                  fill = TRUE,
                  shinyWidgets::materialSwitch(inputId = "profiles_aggregate",
                                               label = "Agrupar por categoría taxonómica",
                                               right = TRUE,
                                               value = FALSE,
                                               status = "primary"),
                  conditionalPanel(
                    condition = "input.profiles_aggregate==true",
                    shinyWidgets::pickerInput(inputId = "profiles_select_rank",
                                              label = "Categoría taxonómica:",
                                              choices = c("genus"),
                                              inline = TRUE, width = "fit",
                                              options = list(title = "seleccionar"))
                  )
                ),
                bslib::card_body(
                  shinyWidgets::materialSwitch(inputId = "profiles_stratify",
                                               label = "Estratificar por variable",
                                               right = TRUE,
                                               value = FALSE,
                                               status = "primary"),
                  conditionalPanel(
                    condition = "input.profiles_stratify==true",
                    shinyWidgets::pickerInput(inputId = "profiles_stratify_variable",
                                              label = "Variable seleccionada",
                                              choices = c("a","b"),
                                              inline = TRUE, width = "fit",
                                              options = list(title = "seleccionar",
                                                             `live-search` = TRUE))
                  ),
                  #
                  conditionalPanel(
                    condition = "input.profiles_stratify_variable",
                    shinyWidgets::checkboxGroupButtons(
                      inputId = "profiles_stratify_variable_categories",
                      label = "Categoría(s) de la variable a mostrar:",
                      choices = c("",""),
                      direction = "horizontal",
                      individual = TRUE,
                      checkIcon = list(
                        yes = tags$i(class = "fa fa-circle",
                                     style = "color: steelblue"),
                        no = tags$i(class = "fa fa-circle-o",
                                    style = "color: steelblue"))
                    ),
                    bslib::layout_column_wrap(
                      width = "100px",
                      fixed_width = TRUE,
                      fillable = TRUE,
                      shinyWidgets::actionBttn(
                        inputId = "profiles_stratify_variable_mostrar_categories",
                        label = "Todas",
                        color = "primary",
                        style = "bordered",size = "sm",block = FALSE
                      ),
                      shinyWidgets::actionBttn(
                        inputId = "profiles_stratify_variable_clear_categories",
                        label = "Ninguna",
                        color = "primary",
                        style = "bordered",size = "sm", block = FALSE
                      )
                    ),
                    checkboxInput("profiles_stratify_apply_test",
                                  label="Evaluar diferencia en composición",
                                  value = FALSE)
                  )
                ),
              )),
            bslib::card_body(
              plotly::plotlyOutput("profiles_stacked") %>%
                shinycssloaders::withSpinner(color="#00719a")
            )
          )
        )
      )
    )
  )
}
