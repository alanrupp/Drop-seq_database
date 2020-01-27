# App for exploring Drop-seq datasets we generate
library(shiny)
library(dplyr)
library(shinythemes)
library(DT)

# - UI ------------------------------------------------------------------------
ui <- 
    navbarPage(title = "Drop-seq data", theme = shinytheme("flatly"),
        # - Tab 1: Grab a dataset and view ------------------------------------
        tabPanel("Explore",
            fluidPage(
                # HTML tag info
                tags$head(
                    tags$style(HTML("hr {border-top: 1px solid #000000;}"))
                    ),
                # select dataset
                fluidRow(
                    column(width = 3, 
                        selectInput("dataset", "Dataset:", choices = data_names)
                        ),
                    column(width = 3, align = 'left', style = 'margin-top: 25px;',
                        actionButton("select_data", "Select", width = "100%")
                        ),
                    column(width = 3, 
                        selectInput("cluster", "Cluster:", choices = cluster_list)
                        )
                    ),
                tags$h4("Plots"),
                fluidRow(
                  column(width = 4, plotOutput("clusters")),
                  column(width = 4, plotOutput("umap")),
                  column(width = 4, plotOutput("violin"))
                  ),
                tags$hr(),
                tags$h4("Table"),
                fluidRow(
                    column(width = 12, 
                        checkboxGroupInput("classes", "", 
                                           choices = c("Receptor" = "receptor", 
                                                       "Secreted" = "secreted", 
                                                       "Transcription factor" = "transcription-factor"),
                                           inline = TRUE))
                ),
                fluidRow(
                  column(width = 12, DTOutput("gene_table"))
                )
                )
            ),
        tabPanel("About")
  )