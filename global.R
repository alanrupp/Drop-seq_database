library(dplyr)
library(ggplot2)
library(stringr)
library(readr)
library(tidyr)

# - Plot clusters on UMAP ---------------------------------------------------
umap_clusters <- function() {
  df <- bind_cols(umap, clusters)
  cluster_centers <- df %>%
    group_by(cluster) %>%
    summarize("UMAP1" = median(UMAP1), "UMAP2" = median(UMAP2))
  p <- ggplot(df, aes(x = UMAP1, y = UMAP2)) +
    geom_point(aes(color = cluster), show.legend = FALSE) +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    geom_text(data = cluster_centers, aes(label = cluster), show.legend = FALSE) +
    ggtitle("Clusters")
  return(p)
}

# - Plot gene on UMAP ---------------------------------------------------------
umap_gene <- function(gene) {
  df <- data.frame("counts" = as.numeric(mtx[gene, ])) %>% bind_cols(., umap)
  p <- ggplot(df, aes(x = UMAP1, y = UMAP2)) +
    geom_point(aes(color = counts), show.legend = FALSE, stroke = 0) +
    scale_color_gradient(low = "gray90", high = "navyblue") +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    ggtitle(gene)
  return(p)
}

# - Plot gene on violin -------------------------------------------------------
violin_gene <- function(gene) {
  df <- data.frame(counts = mtx[gene, ]) %>% bind_cols(., clusters)
  p <- ggplot(df, aes(x = cluster, y = counts, fill = cluster)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(y = "Normalized expression", x = element_blank()) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
          panel.grid = element_blank()) +
    geom_violin(scale = "width", show.legend = FALSE)
    ggtitle(gene)
  return(p)
}

# - Make markers table --------------------------------------------------------
keep_markers <- function(clstr, classes) {
  df <- filter(markers, cluster == clstr) %>% 
    select(gene, cluster, avg_logFC, pct.1, pct.2, p_val, p_val_adj) %>%
    mutate_at(vars(starts_with("pct")), ~ round(.x * 100), 0) %>%
    mutate(avg_logFC = round(avg_logFC, 2)) %>%
    arrange(p_val_adj) %>%
    mutate_at(vars(starts_with("p_")), ~ scales::scientific(.x, digits = 2)) %>%
    rename("Fold change" = avg_logFC, "Pct express" = pct.1,
           "Pct other" = pct.2, "P" = p_val, "FDR" = p_val_adj)
  if (!is.null(classes)) {
    df <- filter(df, gene %in% anno[rowSums(anno[, classes]) > 0, ]$gene)
  }
  return(df)
}

# - Read in data --------------------------------------------------------------
data_files <- list.files("datasets", ".Rdata", full.names = TRUE)
data_names <- str_extract(data_files, "(?<=\\/)(.+)(?=\\.Rdata)")
data_names <- str_replace_all(data_names, "_", " ")

anno <- read_csv("annotation.csv")
gene_info <- read_csv("biotypes.csv")

# default value
cluster_list <- "No data loaded"