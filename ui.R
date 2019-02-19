# App for exploring Drop-seq datasets we generate
library(shiny)
library(dplyr)
library(shinythemes)

# - UI ------------------------------------------------------------------------
ui <- 
  navbarPage(title = "Drop-seq data", 
             theme = shinytheme("flatly"),
             
             # - Tab 1: Grab a dataset and view ------------------------------------------
             tabPanel("Explore data",
                      fluidPage(
                        
                        # HTML tag info
                        tags$head(
                          tags$style(HTML("hr {border-top: 1px solid #000000;}"))
                          ),
                        
                        # select dataset
                        fluidRow(
                          column(width = 3, 
                                 selectInput("dataset", 
                                             "Dataset:", 
                                             choices = data_names_)
                                 ),
                          column(width = 3, align = 'left', 
                                 style = 'margin-top: 25px;',
                                 actionButton("select_data", "Select", 
                                              width = "100%")
                                 )
                          ),
                        fluidRow(
                          column(width = 6,
                                 plotOutput("umap")),
                          column(width = 6,
                                 plotOutput("heatmap"))
                          )
                      )
                      ),
             
             # - Tab 2 - plotting top markers per cluster -------------------- 
             tabPanel("Top markers",
                      fluidPage(
                        fluidRow(
                          column(width = 3, 
                                 selectizeInput("clusterID", 
                                                "Cluster ID",
                                                choices = NULL)),
                          column(width = 3,
                                 sliderInput("num_genes", "Number of genes",
                                             min = 1, max = 16, value = 4)),
                          column(width = 3, align = 'left', 
                                 style = 'margin-top: 25px;',
                                 actionButton("select_cluster", "Select", 
                                              width = "100%")
                        )
                      ),
                        
                        fluidRow(
                          width = 6,
                          plotOutput("markers_plot")
                        )
                        
                      )),
             
             tabPanel("Browse genes",
                      fluidPage(
                        fluidRow(
                          column(width = 3, 
                                 selectizeInput("genes", 
                                                "Genes",
                                                choices = genes,
                                                multiple = TRUE)),
                          column(width = 3, align = 'left', 
                                 style = 'margin-top: 25px;',
                                 actionButton("select_genes", "Select", 
                                              width = "100%")
                        )
                      ),
                      fluidRow(
                        plotOutput("gene_umap", 
                                   width = "100%")
                      ),
                      fluidRow(
                        plotOutput("gene_vln",
                                   width = "100%")
                      ))
             )
  )
