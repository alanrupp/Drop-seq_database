library(dplyr)
library(ggplot2)
library(shiny)

# - Server function ----------------------------------------------------------
server <- function(input, output, session) {
  session$onSessionEnded(stopApp)
  
  # read in data
  observeEvent(input$select_data, {
    
    progress_ <- Progress$new()
    progress_$set(message = "Loading data", 
                  detail = "This may take a bit for large datasets", 
                  value = NULL)
    
    # grab new data
    load(file = data_files[data_names == input$dataset], .GlobalEnv)
    progress_$close()
    
    
    # make genes and clusters lists for menus
    gene_list <- list(Genes = sort(unique(rownames(mtx))))
    #gene_list[["Genes"]] <- gene_list[["Genes"]][gene_list[["Genes"]] %in% protein_coding]
    cluster_list <- list(Clusters = levels(clusters$cluster))
    
    updateSelectizeInput(session, "genes", 
                         choices = gene_list, 
                         server = TRUE)
    updateSelectizeInput(session, "clusterID", 
                         choices = cluster_list, 
                         server = TRUE)

    # UMAP plot of clusters
    output$umap <- renderPlot({
      
      umap_clusters(mtx, clusters)
      
    })
    
    
    # - Tab 2 - plot top markers for each cluster
    observeEvent(input$select_cluster, {
      
      marker_set <- grab_markers(markers, input$clusterID, input$num_genes)
      
      # Violin plot of top markers by group
      output$markers_plot <- renderPlot({
        
        gene_plot(mtx, marker_set, input$plottype_cluster)
        
      })
      
    })
    
    
    # - Tab 3 - Expression by gene of choice
    observeEvent(input$select_genes, {
      
      # Plots of selected genes
      output$gene_plot <- renderPlot({
        
        gene_plot(mtx, input$gene, input$plottype_gene)
        
      })
      
    })
  
  })
  
}