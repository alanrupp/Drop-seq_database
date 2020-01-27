library(dplyr)
library(ggplot2)
library(shiny)

source("global.R")

# - Server function ----------------------------------------------------------
server <- function(input, output, session) {
  session$onSessionEnded(stopApp)
  
  # read in data
  observeEvent(input$select_data, {
    
    progress <- Progress$new()
    progress$set(message = "Loading data", 
                  detail = "This may take a bit for large datasets", 
                  value = NULL)
    
    # grab new data
    load(file = data_files[data_names == input$dataset], .GlobalEnv)
    progress$close()
    
    # make genes and clusters lists for menus
    gene_list <- list(Genes = sort(unique(rownames(mtx))))
    cluster_list <- list(Clusters = sort(unique(clusters$cluster)))
    
    updateSelectizeInput(session, "genes", choices = gene_list)
    updateSelectizeInput(session, "cluster", choices = cluster_list)
    
    # select a gene from the table to plot
    row_id <- reactive({
      if (is.null(input$gene_table_rows_selected)) {
        1
      } else (
        input$gene_table_rows_selected
      )
    })
    
    markers_table <- reactive({
      
      keep_markers(input$cluster, input$classes)
      
    })
    
    # Plot outputs
    output$clusters <- renderPlot({ umap_clusters() })
    output$umap <- renderPlot({ umap_gene(markers_table()[row_id(), "gene"]) })
    output$violin <- renderPlot({ violin_gene(markers_table()[row_id(), "gene"]) })
    
    
    # Table output
    output$gene_table <- renderDT({ markers_table() },
                                  class = "display nowrap compact",
                                  filter = "top",
                                  server = TRUE,
                                  selection = "single", 
                                  escape = FALSE)
  })
}