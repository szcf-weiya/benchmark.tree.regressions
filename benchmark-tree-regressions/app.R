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
library(ggplot2)
library(plotly)

ui <- navbarPage(
  title = "Benchmarking Tree Regressions",
  theme = bs_theme(version = 5),
  tabPanel("Introduction",
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
  ),
  tabPanel("GitHub Action",
           uiOutput("action_md"),
           fluidRow(
             column(3,
                    selectInput("data.action",
                                "Data:",
                                choices = c(Friedman = "sim_friedman", Checkerboard = "sim_checkerboard", Linear = "sim_linear"),
                                selected = "sim_friedman"),
                    withMathJax(HTML("$$x\\in {\\mathrm{I\\!R}}^{n\\times p}, y\\in {\\mathrm{I\\!R}}^n$$")),
                    uiOutput("formula.action")
             ),
             column(9,
                    fluidRow(
                      column(6,
                             card(
                               card_header("5-fold CV Error", container = htmltools::h3),
                               plotlyOutput("ly_errplot_action"))
                      ),
                      column(6,
                             card(
                               card_header("Running Time (seconds)", container = htmltools::h3),
                               plotlyOutput("ly_timeplot_action")
                             )
                      )
                    )
             )
           )
  ),
  tabPanel("Local Results",
           p(
             "Due to limited computational resource (e.g., maximum running time
             is 6 hours and it does not support parallel computing with too many cores)
             by GitHub actions, here we present more results which is run locally on HPC.
             You can also reproduce the results on your own machines."
           ),
           fluidRow(
             column(3,
                    selectInput("data",
                                "Data:",
                                choices = c(Friedman = "sim_friedman", Checkerboard = "sim_checkerboard", Linear = "sim_linear"),
                                selected = "sim_friedman"),
                    withMathJax(HTML("$$x\\in {\\mathrm{I\\!R}}^{n\\times p}, y\\in {\\mathrm{I\\!R}}^n$$")),
                    uiOutput("formula"),
                    selectInput("x.axis",
                                "x-axis:",
                                choices = c(`Sample Size (n)` = "n", `Number of Features (p)` = "p"),
                                selected = "n"),
                    uiOutput("n_or_p")
             ),
             column(9,
                    fluidRow(
                      column(6,
                             card(
                               card_header("5-fold CV Error", container = htmltools::h3),
                               plotlyOutput("ly_errplot"))
                      ),
                      column(6,
                             card(
                               card_header("Running Time (seconds)", container = htmltools::h3),
                               plotlyOutput("ly_timeplot")
                             )
                      )
                    )
             )
           )
  ),
  # Add a footer at the bottom
  tags$footer(
    div(
      style = "text-align: center; padding: 10px; position: fixed; bottom: 0; width: 100%; background-color: #f8f9fa; border-top: 1px solid #e9ecef;",
      HTML("&copy; 2024 "),
      tags$a(href = "https://github.com/szcf-weiya/", "szcf-weiya"),
      HTML(". All rights reserved.")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  intro_md = "
  This application benchmarks different tree-based regression models on various (simulated) datasets. The surveyed methods include:

  - **Bayesian Additive Regression Trees (BART)**: with R package [BART](https://cran.r-project.org/web/packages/BART/index.html)
  - **XBART**: with R package [XBART](https://github.com/JingyuHe/XBART)
  - **Random Forests**: with R package [ranger](https://cran.r-project.org/web/packages/ranger/index.html)
  - **XGBoost**: with R package [xgboost](https://cran.r-project.org/web/packages/xgboost/index.html)
  - **Multivariate Adaptive Regression Splines (MARS)**: with R package [earth](https://cran.r-project.org/web/packages/earth/index.html) and R package [earth.dof.patch](https://github.com/szcf-weiya/earth.dof.patch) with a modified degrees of freedom (df).

  The datasets with detailed generating model can be selected from the following drop-down menu.

  **Tips:** The figures are interactive, powered by Plotly. For example, you can hide or highlight methods by clicking on their names in the legend.
  "
  action_md = "
  The results on this page is completely run on the publicly accessible [GitHub Action](https://github.com/szcf-weiya/benchmark.tree.regressions/actions) platform. So the benchmarking are absolutely latest, open, and reproducible.
  "
  output$intro_md = renderUI({
    HTML(markdown::markdownToHTML(text = intro_md, fragment.only = TRUE))
  })
  output$action_md = renderUI({
    HTML(markdown::markdownToHTML(text = action_md, fragment.only = TRUE))
  })
  if (file.exists("res-action.rds"))
    df.action = readRDS("res-action.rds")
  else
    df.action = readRDS("res-hpc.rds")
  df.action$group = sapply(df.action$method, function(x) strsplit(x, "_")[[1]][1])
  df.local = readRDS("res-hpc.rds")
  df0 = eventReactive(input$data.action, {
    req(input$data.action)
    subset(df.action, data.model == input$data.action)
  })
  df1 = eventReactive(input$data, {
    req(input$data)
    subset(df.local, data.model == input$data)
  })
  fun_formula = function(x) {
    if (x == "sim_friedman") {
      withMathJax(HTML("$$y = 10\\sin(\\pi x_1x_2) + 20(x_3-0.5)^2 + 10x_4 + 5x_5 + N(0, 1)$$"))
    } else if (x == "sim_checkerboard") {
      withMathJax(HTML("$$y = 2x_{50}x_{100} + 2x_{150}x_{200} + N(0, 1)$$"))
    } else if (x == "sim_linear") {
      withMathJax(HTML("$$\\Sigma_{jk} = 0.5^{\\vert j-k\\vert} + 0.2I(i\\neq j)\\\\y = 2x_{50} + 2x_{100} + 4x_{150} + N(0, 1)$$"))
    }
  }
  output$formula = renderUI({
    fun_formula(input$data)
  })
  output$formula.action = renderUI({
    fun_formula(input$data.action)
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

  output$ly_errplot_action = renderPlotly({
    plot_ly(df0(), x = ~method, y=~cv.error, type = "bar", color = ~group) %>% layout(barmode = "stack")
  })
  output$ly_timeplot_action = renderPlotly({
    plot_ly(df0(), x = ~method, y=~runtime, type = "bar", color = ~group) %>% layout(barmode = "stack")
  })
  # output$errplot <- renderPlot({
  #   ggplot(df1(), aes(x = n, y = cv.error, shape = method, color = as.factor(p))) + geom_point(size = 3) + geom_line() + plot_theme
  # })
  output$ly_errplot = renderPlotly({
    if (input$x.axis == "n") {
      plot_ly(data = df.n(), x = ~n, y = ~cv.error, type = "scatter", mode = "markers+lines", marker = list(size = 10), symbol = ~method)
    } else {
      plot_ly(data = df.p(), x = ~p, y = ~cv.error, type = "scatter", mode = "markers+lines", marker = list(size = 10), symbol = ~method)
    }
  })

  # output$timeplot = renderPlot({
  #   ggplot(df1(), aes(x = n, y = runtime, shape = method, color = as.factor(p))) + geom_point(size = 3) + geom_line() + plot_theme
  # })
  output$ly_timeplot = renderPlotly({
    if (input$x.axis == "n") {
      plot_ly(data = df.n(), x = ~n, y = ~runtime, type = "scatter", mode = "markers+lines", marker = list(size = 10), symbol = ~method)
    } else {
      plot_ly(data = df.p(), x = ~p, y = ~runtime, type = "scatter", mode = "markers+lines", marker = list(size = 10), symbol = ~method)
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
