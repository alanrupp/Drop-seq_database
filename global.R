library(tidyverse)

# - Read in data --------------------------------------------------------------
data_files_ <- list.files(path = "datasets", pattern = ".Rdata.gz", 
                         full.names = TRUE)
data_names_ <- 
  data_files_ %>%
  map(., ~ str_replace(.x, "datasets\\/", "")) %>%
  map(., ~ str_replace(.x, "\\.Rdata\\.gz", "")) %>%
  map(., ~ str_replace(.x, "_", " ")) %>%
  unlist()

# keep only protein_coding genes
protein_coding_ <- 
  read_csv("biotypes.csv") %>% 
  filter(gene_biotype == "protein_coding") %>%
  .$gene_name

# defaults
clusters <- list(Clusters = seq(from = 0, to = 10))
genes <- list(Gene = read_csv("biotypes.csv") %>% .$gene_name)


