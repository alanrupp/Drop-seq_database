library(dplyr)
library(ggplot2)
library(shiny)
library(DT)
library(Seurat)

# - Server function ----------------------------------------------------------
server <- function(input, output, session) {
  session$onSessionEnded(stopApp)
  
  # read in data
  observeEvent(input$select_data, {
    
    progress_ <- Progress$new()
    progress_$set(message = "Loading data", 
                  detail = "This may take a few minutes", 
                  value = NULL)
    
    # remove old data and grab new
    rm(list = ls()[!str_detect(ls(), "_$")])
    load(file = data_files_[data_names_ == input$dataset], .GlobalEnv)
    
    progress_$close()
    
    # find Seurat object in environment
    find_class <- function(class_input) {
      obj <- ls(envir = .GlobalEnv)
      types <- map(obj, ~ class(get(.x)))
      hit <- obj[which(types == class_input)]
      return(hit)
    }
    
    seurat_name <- find_class("seurat")
    seurat <- get(seurat_name)
    
    # make genes and clusters lists for menus
    genes <- list(Genes = sort(unique(rownames(seurat@data))))
    genes[["Genes"]] <- genes[["Genes"]][genes[["Genes"]] %in% protein_coding_]
    clusters <- list(Clusters = sort(unique(markers$cluster)))
    
    updateSelectizeInput(session, "clusterID", choices = clusters, server = TRUE)
    
    # table of clusters
    #output$table1 <- renderTable({
    #    seurat@ident %>% table()
    #})
    
    # UMAP plot of clusters
    output$umap <- renderPlot({
      
      DimPlot(seurat, reduction.use = "umap", do.label = TRUE, no.legend = TRUE)
      
    })
    
    # Heatmap of top DE genes
    output$heatmap <- renderPlot({
      
      cells_use <- function(cluster, n = 100) {
        in_ident <- names(seurat@ident)[seurat@ident == cluster]
        if (length(in_ident) < n) {
          in_ident <- sample(in_ident)
        } else {
          in_ident <- sample(in_ident, n)
        }
        return(in_ident)
      }
      genes_use <- arrange(markers, p_val_adj)$gene[1:500]
      
      map(unique(seurat@ident),
          ~rowMeans(seurat@data[genes_use, cells_use(.x)])
          ) %>%
        as.data.frame() %>%
        set_names(unique(seurat@ident)) %>%
        t() %>%
        pheatmap::pheatmap(., show_colnames = FALSE,
                           color = RColorBrewer::brewer.pal("seq", "YlGnBu"),
                           legend = FALSE)
      
    })
    
    
    # - Tab 2 - plot top markers for each cluster
    observeEvent(input$select_cluster, {
      
      genes_cluster <- 
        markers %>%
        filter(cluster == input$clusterID) %>%
        filter(gene %in% protein_coding_) %>%
        slice(1:input$num_genes) %>%
        .$gene
      
      # Violin plot of top markers by group
      output$markers_plot <- renderPlot({
      
        VlnPlot(seurat, 
                features.plot = genes_cluster,
                point.size.use = -1)
        
      })
      
    })
    
    
    # - Tab 3 - Expression by gene of choice
    observeEvent(input$select_genes, {
      
      # Expression plot of selected genes by UMAP
      output$gene_umap <- renderPlot({
        
        FeaturePlot(seurat,
                    features.plot = input$genes,
                    reduction.use = "umap",
                    cols.use = c("gray90", "navyblue"),
                    nCol = 3)
        
      })
      
      # Violin plot of selected genes by cluster
      output$gene_vln <- renderPlot({
        
        VlnPlot(seurat, 
                features.plot = input$genes,
                point.size.use = -1,
                nCol = 3)
        
      })
      
    })
  
  })
  
}