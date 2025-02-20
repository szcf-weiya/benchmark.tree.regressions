#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)
#library(ggplot2) # (try to reduce loading time?)
library(plotly)

choices.data.model = c(Friedman = "sim_friedman",
                       Checkerboard = "sim_checkerboard",
                       Linear = "sim_linear",
                       Max = "sim_max",
                       SingleIndex = "sim_singleIndex")
choices.x.structure = c(Independent = "indep", `AR(1)` = "ar1", `AR(1)+` = "ar1+", Factor = "factor")
choices.real.data = c(CASP = "CASP",
                      Energy = "Energy",
                      AirQuality = "AirQuality",
                      BiasCorrection = "BiasCorrection",
                      ElectricalStability = "ElectricalStability",
                      GasTurbine = "GasTurbine",
                      ResidentialBuilding = "ResidentialBuilding",
                      LungCancerGenomic = "LungCancerGenomic")
setup_html = HTML("<p>All data-generating processes are homoscedastic additive error models: $$Y = f(X) + \\epsilon$$</p>
                          <p>Given $n$ samples and $p$ features, the input data is $$\\mathbf{X}\\in {\\mathrm{I\\!R}}^{n\\times p}, y\\in {\\mathrm{I\\!R}}^n$$</p>
                          <p>Here we consider $\\epsilon \\sim N(0, 1)$, and vary the <strong>covariance structure of $X$</strong> and the <strong>data model $f$</strong> (you can select from the below drop-down menu):</p>")
fluid_row1_github_action = fluidRow(
  column(12,
         selectInput("data.action.x",
                     "Structure of X:",
                     choices = choices.x.structure,
                     selected = "indep"),
         uiOutput("formula.action.x"),
         selectInput("data.action",
                     "Data Model:",
                     choices = choices.data.model,
                     selected = "sim_friedman"),
         uiOutput("formula.action")
  ))

fluid_row2_github_action = fluidRow(
  column(12,
         fluidRow(
           column(12,
                  card(
                    card_header("5-fold CV Error", container = htmltools::h3),
                    plotlyOutput("ly_errplot_action"))
           ),
           column(12,
                  card(
                    card_header("Running Time (seconds)", container = htmltools::h3),
                    plotlyOutput("ly_timeplot_action")
                  )
           )
         )
  )
)

fluid_row1_github_action_real = fluidRow(
  column(12,
         p("You can select the real dataset from the below drop-down menu:"),
         selectInput("real.data.action",
                     "Real Data",
                     choices = choices.real.data,
                     selected = "CASP")
  ))

fluid_row2_github_action_real = fluidRow(
  column(12,
         fluidRow(
           column(12,
                  card(
                    card_header("5-fold CV Error", container = htmltools::h3),
                    plotlyOutput("ly_errplot_action_real"))
           ),
           column(12,
                  card(
                    card_header("Running Time (seconds)", container = htmltools::h3),
                    plotlyOutput("ly_timeplot_action_real")
                  )
           )
         )
  )
)


fluid_row1_local = fluidRow(
  column(12,
         selectInput("data.x",
                     "Structure of X:",
                     choices = choices.x.structure,
                     selected = "indep"),
         uiOutput("formula.x"),
         selectInput("data",
                     "Data Model:",
                     choices = choices.data.model,
                     selected = "sim_friedman"),
         uiOutput("formula"),
         HTML("<p>Since we considered different combinations of $(n, p)$, you can select $n$ (or $p$) as the x-axis, and check the trend along $p$ (or $n$)</p>"),
         selectInput("x.axis",
                     "x-axis:",
                     choices = c(`Sample Size (n)` = "n", `Number of Features (p)` = "p"),
                     selected = "n"),
         uiOutput("n_or_p")
  ))

fluid_row2_local = fluidRow(
  column(12,
         fluidRow(
           column(12,
                  card(
                    card_header("5-fold CV Error", container = htmltools::h3),
                    plotlyOutput("ly_errplot"))
           ),
           column(12,
                  card(
                    card_header("Running Time (seconds)", container = htmltools::h3),
                    plotlyOutput("ly_timeplot")
                  )
           )
         )
  )
)

fluid_row1_local_real = fluidRow(
  column(12,
         p("You can select the real dataset from the below drop-down menu:"),
         selectInput("real.data",
                     "Real Data:",
                     choices = choices.real.data,
                     selected = "CASP")
  ))

fluid_row2_local_real = fluidRow(
  column(12,
         fluidRow(
           column(12,
                  card(
                    card_header("5-fold CV Error", container = htmltools::h3),
                    plotlyOutput("ly_errplot_real"))
           ),
           column(12,
                  card(
                    card_header("Running Time (seconds)", container = htmltools::h3),
                    plotlyOutput("ly_timeplot_real")
                  )
           )
         )
  )
)

link_github = tags$a(
  shiny::icon("github"), "GitHub",
  href = "https://github.com/szcf-weiya/benchmark.tree.regressions/",
  target = "_blank"
)
footer = tags$footer(div(
  style = "text-align: left; padding: 10px; position: fixed; bottom: 0; width: 100%; background-color: #f8f9fa; border-top: 1px solid #e9ecef;",
  HTML(paste0('&copy; 2025 <a href="https://hohoweiya.xyz/">Lijun Wang</a>. Last updated: ', Sys.getenv("GIT_COMMIT_DATE", "Unknown")))
))
header = tags$style(HTML("
    body {
      font-size: 18px; /* Default font size for body */
    }
  "))
tab_ga = tabPanel("GitHub Action",
                  uiOutput("action_md"),
                  fluid_row1_github_action,
                  fluid_row2_github_action
)
tab_local =   tabPanel("Local Results",
                       p(
                         "Due to limited computational resource (e.g., maximum running time
             is 6 hours and it does not support parallel computing with too many cores)
             by GitHub actions, here we present more results which is run locally on HPC.
             You can also reproduce the results on your own machines."
                       ),
                       fluid_row1_local,
                       fluid_row2_local
)
tips_figure = HTML("<p>&#10071; <strong>Tips:</strong> The figures are interactive, powered by Plotly. For example, you can hide or highlight methods by clicking on their names in the legend.
              </p>")
desc_realdata = HTML("
              <p>Here are real datasets we conducted benchmarking experiments. The input is $\\mathbf{X}\\in {\\mathrm{I\\!R}}^{n\\times p}, y\\in {\\mathrm{I\\!R}}^n$.</p>
              ")
ui = page_navbar(
  title = "Benchmarking Tree Regressions",
  header = header,
  theme = bs_theme(bootswatch = "cerulean"),
  sidebar = sidebar(
    width = "40%",
    wellPanel(
      #h2("Benchmarking Tree Regressions"),
      withMathJax(),
      tags$script(src = "https://cdn.plot.ly/plotly-2.11.1.min.js"),
      tags$head(
        tags$script(
          HTML(
            "
      MathJax.Hub.Config({
        tex2jax: { inlineMath: [['$', '$'], ['\\\\(', '\\\\)']] },
        'HTML-CSS': { scale: 90, linebreaks: { automatic: true } },
        SVG: { scale: 90, linebreaks: { automatic: true } }
      });
      "
          )
        )
      ),
      uiOutput("intro_md")
    )
  ),

  nav_panel(title = "Introduction",
            uiOutput("intro_panel_md"),
            footer),
  nav_panel(title = "GA Simulations",
            uiOutput("action_md"),
            setup_html,
            HTML("<p>Due to limited computational resources in Github Action, here we focus on $n = 100, p = 20$ situation. For other combinations of $(n, p)$, please check the <strong>Local Simulations</strong> part, in which more experiments has been conducted in HPC.</p>"),
            fluid_row1_github_action,
            tips_figure,
            uiOutput("if_exist_na_action"),
            fluid_row2_github_action,
            footer
  ),
  nav_panel(title = "Local Simulations",
            p(
              "Due to limited computational resource (e.g., maximum running time
           is 6 hours and it does not support parallel computing with too many cores)
           by GitHub actions, here we present more results which is run locally on HPC.
           You can also reproduce the results on your own machines."
            ),
            setup_html,
            fluid_row1_local,
            tips_figure,
            uiOutput("if_exist_na_local"),
            fluid_row2_local,
            footer
  ),
  nav_panel(title = "GA Real Data",
            desc_realdata,
            tableOutput("real_data_meta"),
            fluid_row1_github_action_real,
            tips_figure,
            uiOutput("if_exist_na_action_real"),
            fluid_row2_github_action_real,
            footer),
  nav_panel(title = "Local Real Data",
            desc_realdata,
            tableOutput("real_data_meta2"),
            fluid_row1_local_real,
            tips_figure,
            uiOutput("if_exist_na_local_real"),
            fluid_row2_local_real,
            footer),
  nav_spacer(),
  nav_item(link_github)
  # nav_menu(
  #   title = "Links",
  #   align = "right",
  #   nav_item(link_github)
  # ),
)
add_target_blank = function(txt) {
  html = xml2::read_html(txt)
  links = xml2::xml_find_all(html, "//a")
  xml2::xml_set_attr(links, "target", "_blank")
  as.character(html)
}
# Define server logic required to draw a histogram
server <- function(input, output) {
  intro_md = "

  This application benchmarks different tree-based regression models on various (simulated) datasets. The surveyed methods include:

  The method name are formatted as `{Method}_{para1}_{para2}_..._{para3}`, where `paraX` are tuning parameters.

  - **Bayesian Additive Regression Trees (BART)**:
    - [![](https://img.shields.io/badge/R-BART-blue)](https://cran.r-project.org/web/packages/BART/index.html): `BART_{ntree}`
      - `ntree`: number of trees in the forest. The default number of trees is 200 for continuous outcomes.
      - `ndpost`: the number of posterior draws returned. The default in 1000.
      - `nskip`: number of burnin iterations. The default is 100.
    - [![](https://img.shields.io/badge/R-dbarts-blue)](https://cran.r-project.org/web/packages/dbarts/index.html): `dbarts_{ntree}`
      - same parameters as above: `ntree`, `ndpost`, and `nskip`
  - **Accelerated BART (XBART)**:
    - [![](https://img.shields.io/badge/R-XBART-blue)](https://github.com/JingyuHe/XBART): `XBART_{num_trees}_{num_sweeps}`
      - `num_trees`: number of trees in the forest
      - `num_sweeps`: (similar to `npost` in `BART`) one sweep is one iteration of the XBART algorithm, sampling all trees at once. The paper [He & Hahn (2023)](https://doi.org/10.1080/01621459.2021.1942012) recommends the number of sweeps `num_sweeps` to be 40.
      - `burnin`: (similar to `nskip` in `BART`) number of burnin sweeps. The paper recommends it to be 15. Thus we fix it to be `15`.
      - `mtry`: number of features at each split of the tree. The default is `p`.
  - **Random Forests**:
    - [![](https://img.shields.io/badge/R-randomForest-blue)](https://cran.r-project.org/web/packages/randomForest/index.html): `randomForest_{ntree}`
      - `ntree`: number of trees.
      - `mtry`: number of features at each split of the tree. The default is `p / 3` for continuous outcomes, and `sqrt(p)` for categorical outcomes, which is also consistent with the ESL book.
    - [![](https://img.shields.io/badge/R-ranger-blue)](https://cran.r-project.org/web/packages/ranger/index.html): `ranger_{num.trees}`
      - `num.trees`: number of trees.
      - `mtry`: number of features at each split of the tree. The default is `sqrt(p)`. To be consistent with `randomForest`, we set it to be `p / 3`.
  - **XGBoost**:
    - [![](https://img.shields.io/badge/R-xgboost-blue)](https://cran.r-project.org/web/packages/xgboost/index.html): `XGBoost_{nrounds}_{early_stopping_rounds}`
      - `nrounds`: max number of boosting iterations
      - `early_stopping_rounds`: If `NULL`, the early stopping function is not triggered. If set to an integer `k`, training with a validation set will stop if the performance doesn't improve for `k` rounds.
  - **Multivariate Adaptive Regression Splines (MARS)**:
    - [![](https://img.shields.io/badge/R-earth-blue)](https://cran.r-project.org/web/packages/earth/index.html): `earth_{degree}`
      - `degree`: linear model (`degree` = 1) or interaction model (`degree` = 2)
    - [![](https://img.shields.io/badge/R-earth.dof.patch-blue)](https://github.com/szcf-weiya/earth.dof.patch): a patch for a modified degrees of freedom (DoF) on [![](https://img.shields.io/badge/R-earth-blue)](https://cran.r-project.org/web/packages/earth/index.html): `earth_{degree}_df`
      - by default, the `df` in `penalty` of [![](https://img.shields.io/badge/R-earth-blue)](https://cran.r-project.org/web/packages/earth/index.html) is `degree + 1`. [Wang, Zhao, & Fan (2024)](https://doi.org/10.1080/10618600.2024.2388545) suggests setting an adaptive `penalty` to fulfill the consistency of DoF.

  "
  # plotly_tips_md = "
  # > &#10071; **Tips:** The figures are interactive, powered by Plotly. For example, you can hide or highlight methods by clicking on their names in the legend.
  # "
  intro_panel_md = "
  The project is partially inspired by [benchopt](https://github.com/benchopt/benchopt), but aimed for a more **R-user-friendly, easily-hacking, more statistically** benchmarking.

  For the necessity and importance of benchmarking, I would like cite the following paragraph from [benchopt](https://github.com/benchopt/benchopt)'s paper:

  > We firmly believe that this critical task of **maintaining an up-to-date benchmark in a field cannot be solved without a collective effort**. We want to empower the community to take up this challenge and **build a living, reproducible and standardized state of the art that can serve as a foundation for future research**.

  - **GA Results:** For a latest, open, and reproducible benchmarking, we run the experiments on publicly accessible [GitHub Action (GA)](https://github.com/szcf-weiya/benchmark.tree.regressions/actions) platform.

  - **Local Results:** On the other hand, due to limited computational resource (e.g., maximum running time is 6 hours) on GitHub Action, we also run the same code for more repetitions locally on HPC.
  "
  action_md = "
  The results on this page is run on the publicly accessible [GitHub Action](https://github.com/szcf-weiya/benchmark.tree.regressions/actions) platform, so the benchmarking are completely latest, open, and reproducible.
  "
  real_data_md = "
  Here are real datsets we conducted benchmarking experiments.

  "
  output$intro_md = renderUI({
    HTML(add_target_blank(markdown::markdownToHTML(text = intro_md, fragment.only = TRUE)))
  })
  output$action_md = renderUI({
    HTML(add_target_blank(markdown::markdownToHTML(text = action_md, fragment.only = TRUE)))
  })
  # output$right_top_md = renderUI({
  #   HTML(add_target_blank(markdown::markdownToHTML(text = right_top_md, fragment.only = TRUE)))
  # })
  output$intro_panel_md = renderUI({
    HTML(add_target_blank(markdown::markdownToHTML(text = intro_panel_md, fragment.only = TRUE)))
  })
  # output$real_data_md = renderUI(
  #   HTML(add_target_blank(markdown::markdownToHTML(text = real_data_md, fragment.only = TRUE)))
  # )
  # output$real_data_md2 = renderUI(
  #   HTML(add_target_blank(markdown::markdownToHTML(text = real_data_md, fragment.only = TRUE)))
  # )
  real.data.meta = readRDS("real-data-meta.rds")
  output$real_data_meta = renderTable({
    real.data.meta
  }, sanitize.text.function = identity)
  output$real_data_meta2 = renderTable({
    real.data.meta
  }, sanitize.text.function = identity)
  if (file.exists("res-debug.rds")) {
    df.action = readRDS("res-debug.rds")
    df.action$group = sapply(as.character(df.action$method), function(x) strsplit(x, "_")[[1]][1])
    df.local = readRDS("res-debug.rds")
    df.action.real = readRDS("res-debug-real.rds")
    df.action.real$group = sapply(as.character(df.action.real$method), function(x) strsplit(x, "_")[[1]][1])
    df.local.real = readRDS("res-debug-real.rds") # dummy use
    df.local.real$group = sapply(as.character(df.local.real$method), function(x) strsplit(x, "_")[[1]][1])
  } else {
    if (file.exists("res-action.rds"))
      df.action = readRDS("res-action.rds")
    else
      df.action = readRDS("res-hpc.rds")
    df.action$group = sapply(as.character(df.action$method), function(x) strsplit(x, "_")[[1]][1])
    df.local = readRDS("res-hpc.rds")
    if (file.exists("res-action-real.rds"))
      df.action.real = readRDS("res-action-real.rds")
    else
      df.action.real = readRDS("res-hpc-real.rds")
    df.action.real$group = sapply(as.character(df.action.real$method), function(x) strsplit(x, "_")[[1]][1])
    df.local.real = readRDS("res-hpc-real.rds") # the first time is just copied from res-debug-real.rds
    df.local.real$group = sapply(as.character(df.local.real$method), function(x) strsplit(x, "_")[[1]][1])
  }
  df0 = eventReactive(c(input$data.action.x, input$data.action), {
    req(input$data.action)
    req(input$data.action.x)
    df.tmp = subset(df.action, data.model == input$data.action)
    subset(df.tmp, structure == input$data.action.x)
  })
  df1 = eventReactive(c(input$data, input$data.x), {
    req(input$data)
    req(input$data.x)
    df.tmp = subset(df.local, data.model == input$data)
    subset(df.tmp, structure == input$data.x)
  })
  df0.real = eventReactive(input$real.data.action, {
    req(input$real.data.action)
    subset(df.action.real, data.model == paste0("real_", input$real.data.action))
  })
  df1.real = eventReactive(input$real.data, {
    req(input$real.data)
    subset(df.local.real, data.model == paste0("real_", input$real.data))
  })
  fun_formula = function(x) {
    if (x == "sim_friedman") {
      withMathJax(HTML("$$y = 10\\sin(\\pi x_1x_2) + 20(x_3-0.5)^2 + 10x_4 + 5x_5 + N(0, 1)$$"))
    } else if (x == "sim_checkerboard") {
      withMathJax(HTML("$$y = 2x_{1}x_{2} + 2x_{3}x_{4} + N(0, 1)$$"))
    } else if (x == "sim_linear") {
      withMathJax(HTML("$$y = 2x_{1} + 2x_{2} + 4x_{3} + N(0, 1)$$"))
    } else if (x == "sim_max") {
      withMathJax(HTML("$$y = \\max(x_1, x_2, x_3) + N(0, 1)$$"))
    } else if (x == "sim_singleIndex") {
      withMathJax(HTML("$$y = 10\\sqrt{a} + \\sin(5a); a = \\sum_{j=1}^{10}(x_j - \\gamma_j)^2; \\gamma_j = -1.5 + \\frac{j-1}{3}$$"))
    }
  }
  fun_formula_x = function(x) {
    if (x == "indep") {
      withMathJax(HTML("$$X_{ij}\\sim_{i.i.d.} N(0, 1), i=1,\\ldots,n; j=1,\\ldots,p$$"))
    } else if (x == "ar1") {
      withMathJax(HTML("$$X\\sim N(0_p, \\Sigma)\\\\
                       \\Sigma_{jk} = 0.9^{\\vert j-k\\vert}$$"))
    } else if (x == "ar1+") {
      withMathJax(HTML("$$X\\sim N(0_p, \\Sigma)\\\\\
                       \\Sigma_{jk} = 0.5^{\\vert j-k\\vert} + 0.2I(i\\neq j)$$"))
    } else if (x == "factor") {
      withMathJax(HTML("$$\\mathbf{X = (BF)}^\\top+\\epsilon\\\\
                       \\mathbf{F_{k\\times n}}\\sim N(0, 1), k=p/5\\\\
                       \\sum_{j=1}^k \\mathbf{B}_{ij}=1, \\sum_{i=1}^p\\mathbf{B}_{ij}=5\\\\
                       \\epsilon_{n\\times p} \\sim N(0, 0.01k)
                       $$"))
    }
  }
  output$formula = renderUI({
    fun_formula(input$data)
  })
  output$formula.action = renderUI({
    fun_formula(input$data.action)
  })
  output$formula.x = renderUI({
    fun_formula_x(input$data.x)
  })
  output$formula.action.x = renderUI({
    fun_formula_x(input$data.action.x)
  })
  output$n_or_p = renderUI({
    if (input$x.axis == "n")
      selectInput("fix_p", "Number of Features (p):", choices = unique(df1()$p))
    else
      selectInput("fix_n", "Sample Size (n):", choices = unique(df1()$n))
  })
  df.n = eventReactive(c(input$fix_p, df1()), {
    req(input$fix_p)
    subset(df1(), p == input$fix_p)
  })
  df.p = eventReactive(c(input$fix_n, df1()), {
    req(input$fix_n)
    subset(df1(), n == input$fix_n)
  })
  plot_theme =
    theme(
      plot.title = element_text(size = 20, face = "bold"),  # Title size
      axis.title.x = element_text(size = 14),                # X-axis title size
      axis.title.y = element_text(size = 14),                # Y-axis title size
      axis.text.x = element_text(size = 12),                 # X-axis text size
      axis.text.y = element_text(size = 12),                 # Y-axis text size
      legend.title = element_text(size = 14),                 # Legend title size
      legend.text = element_text(size = 12)                   # Legend text size
    )

  output$if_exist_na_action = renderUI({
    na_methods = as.character(df0()$method[which(is.na(df0()$runtime))])
    if (length(na_methods) > 0)
      HTML(paste0("<p>&#10071; <strong>Warnings: </strong>", paste0(na_methods, collapse = " "), " failed.</p>"))
  })
  output$if_exist_na_action_real = renderUI({
    na_methods = as.character(df0.real()$method[which(is.na(df0.real()$runtime))])
    if (length(na_methods) > 0)
      HTML(paste0("<p>&#10071; <strong>Warnings: </strong>", paste0(na_methods, collapse = " "), " failed.</p>"))
  })
  output$if_exist_na_local = renderUI({
    if (input$x.axis == "n") {
      na_methods = unique(as.character(df.n()$method[which(is.na(df.n()$runtime))]))
    } else {
      na_methods = unique(as.character(df.p()$method[which(is.na(df.p()$runtime))]))
    }
    if (length(na_methods) > 0)
      HTML(paste0("<p>&#10071; <strong>Warnings: </strong>", paste0(na_methods, collapse = " "), " failed.</p>"))
  })
  output$if_exist_na_local_real = renderUI({
    na_methods = as.character(df1.real()$method[which(is.na(df1.real()$runtime))])
    if (length(na_methods) > 0)
      HTML(paste0("<p>&#10071; <strong>Warnings: </strong>", paste0(na_methods, collapse = " "), " failed.</p>"))
  })
  output$ly_errplot_action = renderPlotly({
    plot_ly(df0(), x = ~method, y=~cv.error, type = "bar", color = ~group) %>% layout(barmode = "stack")
  })
  output$ly_timeplot_action = renderPlotly({
    plot_ly(df0(), x = ~method, y=~runtime, type = "bar", color = ~group) %>% layout(barmode = "stack")
  })

  output$ly_errplot_action_real = renderPlotly({
    plot_ly(df0.real(), x = ~method, y=~cv.error, type = "bar", color = ~group) %>% layout(barmode = "stack")
  })
  output$ly_timeplot_action_real = renderPlotly({
    plot_ly(df0.real(), x = ~method, y=~runtime, type = "bar", color = ~group) %>% layout(barmode = "stack")
  })

  output$ly_errplot_real = renderPlotly({
    plot_ly(df1.real(), x = ~method, y=~cv.error, type = "bar", color = ~group) %>% layout(barmode = "stack")
  })
  output$ly_timeplot_real = renderPlotly({
    plot_ly(df1.real(), x = ~method, y=~runtime, type = "bar", color = ~group) %>% layout(barmode = "stack")
  })
  # output$errplot <- renderPlot({
  #   ggplot(df1(), aes(x = n, y = cv.error, shape = method, color = as.factor(p))) + geom_point(size = 3) + geom_line() + plot_theme
  # })
  available_symbols <- c("circle", "square", "diamond", "cross", "x", "triangle-up",
                         "triangle-down", "triangle-left", "triangle-right",
                         "star", "hexagram", "pentagon", "bowtie", "hourglass",
                         "arrow-up", "arrow-down", "asterisk", "hash", "y-up",
                         "y-down", "line-ew", "line-ns")

  output$ly_errplot = renderPlotly({
    if (input$x.axis == "n") {
      plot_ly(data = df.n(), x = ~n, y = ~cv.error, type = "scatter", mode = "markers+lines", marker = list(size = 10), symbol = ~method, symbols = available_symbols)
    } else {
      plot_ly(data = df.p(), x = ~p, y = ~cv.error, type = "scatter", mode = "markers+lines", marker = list(size = 10), symbol = ~method, symbols = available_symbols)
    }
  })

  # output$timeplot = renderPlot({
  #   ggplot(df1(), aes(x = n, y = runtime, shape = method, color = as.factor(p))) + geom_point(size = 3) + geom_line() + plot_theme
  # })
  output$ly_timeplot = renderPlotly({
    if (input$x.axis == "n") {
      plot_ly(data = df.n(), x = ~n, y = ~runtime, type = "scatter", mode = "markers+lines", marker = list(size = 10), symbol = ~method, symbols = available_symbols)
    } else {
      plot_ly(data = df.p(), x = ~p, y = ~runtime, type = "scatter", mode = "markers+lines", marker = list(size = 10), symbol = ~method, symbols = available_symbols)
    }
  })
  # output$errplot.p <- renderPlot({
  #   ggplot(df1(), aes(x = p, y = cv.error, shape = method, color = as.factor(n))) + geom_point(size = 3) + geom_line() + plot_theme
  # })
  # output$timeplot.p = renderPlot({
  #   ggplot(df1(), aes(x = p, y = runtime, shape = method, color = as.factor(n))) + geom_point(size = 3) + geom_line() + plot_theme
  # })
}

# Run the application
shinyApp(ui = ui, server = server)
