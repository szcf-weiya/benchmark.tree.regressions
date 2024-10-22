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
# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = bs_theme(version = 5),
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

    # Application title
    titlePanel("Benchmarking Tree Regressions"),
  # Add a descriptive paragraph with highlights and itemization
  p(
    "This application benchmarks different tree-based regression models on various (simulated) datasets. The surveyed methods include:"
  ),
  # Add an itemized list for key metrics
  tags$ul(
    tags$li(tags$strong("Bayesian Additive Regression Trees (BART): "),
            "with R package ",
            tags$a(href = "https://cran.r-project.org/web/packages/BART/index.html", "BART")),
    tags$li(tags$strong("XBART: "),
            "with R package ",
            tags$a(href = "https://github.com/JingyuHe/XBART", "XBART")),
    tags$li(tags$strong("Random Forests: "),
            "with R package ",
            tags$a(href = "https://cran.r-project.org/web/packages/ranger/index.html", "ranger")),
    tags$li(tags$strong("XGBoost: "),
            "with R package ",
            tags$a(href = "https://cran.r-project.org/web/packages/xgboost/index.html", "xgboost")),
    tags$li(tags$strong("Multivariate Adaptive Regression Splines (MARS): "),
            "with R package ",
            tags$a(href = "https://cran.r-project.org/web/packages/earth/index.html", "earth"),
            " and R package ",
            tags$a(href = "https://github.com/szcf-weiya/earth.dof.patch", "earth.dof.patch"),
            " with a modified degrees of freedom (df)."),
  ),

  p("The datasets with detailed generating model can be selected from the following drop-down menu."),

  #p("The benchmarking results are automatically generated through GitHub Actions."),

  p(tags$strong("This is an ongoing project, more methods and datasets will be added. We welcome and appreciate any comments.")),

  p(tags$strong("Tips:"), "The figures are interactive, powered by Plotly. For example, you can hide or highlight methods by clicking on their names in the legend."),

    fluidRow(
      column(3,
             selectInput("data",
                         "Data:",
                         choices = c(Friedman = "sim_friedman", Checkerboard = "sim_checkerboard"),
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
    ),
  # Add a footer at the bottom
  tags$footer(
    div(
      style = "text-align: center; padding: 10px; position: fixed; bottom: 0; width: 100%; background-color: #f8f9fa; border-top: 1px solid #e9ecef;",
      HTML("&copy; 2024 "),
      tags$a(href = "https://github.com/szcf-weiya/", "szcf-weiya"),
      HTML(". All rights reserved. Last compiled on: "),
      Sys.Date()
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    if (file.exists("res-action.rds"))
      df = readRDS("res-action.rds")
    else
      df = readRDS("res256.rds")
    df1 = eventReactive(input$data, {
      req(input$data)
      subset(df, data.model == input$data)
    })
    output$formula = renderUI({
      if (input$data == "sim_friedman") {
        withMathJax(HTML("$$y = 10\\sin(\\pi x_1x_2) + 20(x_3-0.5)^2 + 10x_4 + 5x_5 + N(0, 1)$$"))
      } else if (input$data == "sim_checkerboard") {
        withMathJax(HTML("$$y = 2x_{50}x_{100} + 2x_{150}x_{200} + N(0, 1)$$"))
      }
    })
    output$n_or_p = renderUI({
      if (input$x.axis == "n")
        selectInput("fix_p", "Number of Features (p):", choices = unique(df1()$p))
      else
        selectInput("fix_n", "Sample Size (n):", choices = unique(df1()$n))
    })
    # observeEvent(input$x.axis, {
    #   if (input$x.axis == "n") {
    #     output$n_or_p = renderUI({
    #       selectInput("fix_p", "Number of Features:",
    #                   choices = unique(df1()$p))
    #     })
    #   } else {
    #     output$n_or_p = renderUI({
    #       selectInput("fix_n", "Sample Size:",
    #                   choices = unique(df1()$n))
    #     })
    #   }
    # })
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
