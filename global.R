library(dplyr)
library(ggplot2)
library(stringr)
library(readr)
library(tidyr)
library(purrr)

# - Grab markers -------------------------------------------------------------
grab_markers <- function(markers_df, clst, n_genes) {
  mrkrs <- markers_df %>%
    filter(cluster == clst) %>%
    filter(p_val_adj < 0.05) %>%
    arrange(desc(pct.1 - pct.2)) %>%
    .$gene
  if (n_genes > length(mrkrs)) {
    mrkrs <- mrkrs
  } else {
    mrkrs <- mrkrs[1:n_genes]
  }
  return(mrkrs)
}

# - Plot clusters on UMAP ---------------------------------------------------
umap_clusters <- function(mtx, clusters) {
  df <- bind_cols(umap, clusters)
  cluster_centers <- df %>%
    group_by(cluster) %>%
    summarize("UMAP1" = mean(UMAP1), "UMAP2" = mean(UMAP2))
  plt <- ggplot(df, aes(x = UMAP1, y = UMAP2)) +
    geom_point(aes(color = cluster), show.legend = FALSE) +
    theme_bw() +
    theme(panel.grid = element_blank())
  plt <- plt + 
    geom_text(data = cluster_centers, aes(label = cluster), show.legend = FALSE)
  return(plt)
}

# - Plot gene by cluster -----------------------------------------------------
gene_plot <- function(mtx, genes, plot_type) {
  if (length(genes) == 1) {
    df <- mtx[genes, ] %>% as.data.frame() %>% set_names("counts")
  } else {
    df <- mtx[genes, ] %>% as.matrix() %>% t() %>% as.data.frame()
  }
  
  # grab data for plots
  if (plot_type == "umap") {
    df <- bind_cols(df, umap)
    if (length(genes) > 1) {
      df <- gather(df, -starts_with("UMAP"), key = "gene", value = "counts")
    }
  } else if (plot_type %in% c("violin", "boxplot")) {
    df$cluster <- clusters$cluster
     if (length(genes) > 1) {
       df <- gather(df, -cluster, key = "gene", value = "counts")
     }
  }
  
  # set up plots
  if (plot_type == "umap") {
    plt <- ggplot(df, aes(x = UMAP1, y = UMAP2)) +
      geom_point(aes(color = counts), show.legend = FALSE, stroke = 0) +
      scale_color_gradient(low = "gray90", high = "navyblue") +
      theme_bw() +
      theme(panel.grid = element_blank())
  } else if (plot_type %in% c("violin", "boxplot")) {
    plt <- ggplot(df, aes(x = cluster, y = counts, fill = cluster)) +
      scale_y_continuous(expand = c(0, 0)) +
      labs(y = "Counts", x = element_blank()) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
            panel.grid = element_blank())
    if (plot_type == "violin") {
      plt <- plt + 
        geom_violin(scale = "width", show.legend = FALSE) +
        geom_jitter(color = "black", stroke = 0, alpha = 0.5,
                    show.legend = FALSE, height = 0)
    } else if (plot_type == "boxplot") {
      plt <- plt + 
        geom_boxplot(show.legend = FALSE)
    }
  }
  
  if (length(genes) > 1) {
    plt <- plt + facet_wrap(~gene, scales = "free_y")
  }

  return(plt)
}


# - Read in data --------------------------------------------------------------
data_files <- list.files("datasets", ".Rdata", full.names = TRUE)
data_names <- str_extract(data_files, "(?<=\\/)(.+)(?=\\.Rdata)")
data_names <- str_replace_all(data_names, "_", " ")