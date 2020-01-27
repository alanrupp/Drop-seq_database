# Drop-seq database
Shiny app for browsing Drop-seq data

## Input data organization
In an R environment, create 4 objects:
* `mtx`: a matrix of normalized counts, column names are barcodes, row names are genes
* `clusters`: a data.frame with one column—`cluster`—and rownames that correspond to the column names of `mtx`
* `umap`: a data.frame of dimension reduction coordinates. 2 columns—`UMAP1` and `UMAP2`—and rownames that correspond to the column names of `mtx`
* `markers`: a data.frame of marker genes for each cluster from the output of `FindAllMarkers` in Seurat
  * If not using `FindAllMarkers`, the data.frame must have the columns `gene`, `cluster`, `avg_logFC`, `pct.1`, `pct.2`, `p_val`, and `p_val_adj`.

Save these 4 objects in an `Rdata` file in a folder called `datasets`.
