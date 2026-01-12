## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.width = 7, fig.height=5)

## ----eval=FALSE---------------------------------------------------------------
# remotes::install_github("alserglab/mascarade")

## -----------------------------------------------------------------------------
library(mascarade)
library(data.table)
library(ggplot2)
library(ggforce)

## -----------------------------------------------------------------------------
data("exampleMascarade")

## -----------------------------------------------------------------------------
head(exampleMascarade$dims)

## -----------------------------------------------------------------------------
head(exampleMascarade$clusters)

## -----------------------------------------------------------------------------
head(exampleMascarade$features)

## -----------------------------------------------------------------------------
data <- data.table(exampleMascarade$dims, 
                   cluster=exampleMascarade$clusters,
                   exampleMascarade$features)
ggplot(data, aes(x=UMAP_1, y=UMAP_2)) + 
    geom_point(aes(color=cluster)) + 
    coord_fixed() + 
    theme_classic()
                               

## -----------------------------------------------------------------------------
maskTable <- generateMask(dims=exampleMascarade$dims, 
                          clusters=exampleMascarade$clusters)

## -----------------------------------------------------------------------------
head(maskTable)

## -----------------------------------------------------------------------------
ggplot(data, aes(x=UMAP_1, y=UMAP_2)) + 
    geom_point(aes(color=cluster)) + 
    geom_path(data=maskTable, aes(group=group)) +
    coord_fixed() + 
    theme_classic()

## -----------------------------------------------------------------------------
ggplot(data, aes(x=UMAP_1, y=UMAP_2)) + 
    geom_point(color="grey") + 
    geom_path(data=maskTable, aes(group=group, color=cluster), linewidth=1) +
    coord_fixed() + 
    theme_classic()

## -----------------------------------------------------------------------------
ggplot(data, aes(x=UMAP_1, y=UMAP_2)) + 
    geom_point(color="grey") + 
    ggforce::geom_shape(data=maskTable, aes(group=group, color=cluster),
                        linewidth=1, fill=NA, expand=unit(-1, "pt")) +
    coord_fixed() + 
    theme_classic()

## -----------------------------------------------------------------------------
maskTable <- generateMask(dims=exampleMascarade$dims, 
                          clusters=exampleMascarade$clusters,
                          expand=0.02)
ggplot(data, aes(x=UMAP_1, y=UMAP_2)) + 
    geom_point(color="grey") + 
    ggforce::geom_shape(data=maskTable, aes(group=group, color=cluster),
                        linewidth=1, fill=NA, expand=unit(-1, "pt")) +
    coord_fixed() + 
    theme_classic()

## -----------------------------------------------------------------------------
myMask <- list(
    geom_mark_shape(data=maskTable, aes(group=cluster, color=cluster, label = cluster),
                    fill = NA,
                   linewidth=1, expand=unit(-1, "pt"),
                   con.cap=0, con.type = "straight",
                   label.fontsize = 10, label.buffer = unit(0, "cm"),
                   label.fontface = "plain",
                   label.minwidth = 0,
                   label.margin = margin(2, 2, 2, 2, "pt"),
                   label.lineheight = 0,
                   con.colour = "inherit",
                   show.legend = FALSE),
    # expanding to give a bit more space for labels
    scale_x_continuous(expand = expansion(mult = 0.1)),
    scale_y_continuous(expand = expansion(mult = 0.1))
)

ggplot(data, aes(x=UMAP_1, y=UMAP_2)) +
    geom_point(color="grey") +
    myMask +
    coord_fixed() +
    theme_classic()

## -----------------------------------------------------------------------------
ggplot(data, aes(x=UMAP_1, y=UMAP_2)) +
    geom_point(color="grey") +
    fancyMask(maskTable, ratio=1) +
    theme_classic()

## -----------------------------------------------------------------------------
ggplot(data, aes(x=UMAP_1, y=UMAP_2)) + 
    geom_point(aes(color=GNLY), size=0.5) +
    scale_color_gradient2(low = "#404040", high="red") + 
    fancyMask(maskTable, ratio=1) +
    theme_classic()

## -----------------------------------------------------------------------------
ggplot(data, aes(x=UMAP_1, y=UMAP_2)) + 
    geom_point(aes(color=GNLY), size=0.5) + 
    scale_color_gradient2(low = "#404040", high="red") + 
    geom_path(data=maskTable[cluster=="NK"], aes(group=group)) +
    coord_fixed() + 
    theme_classic()

## ----message=FALSE------------------------------------------------------------
library(Seurat)

## -----------------------------------------------------------------------------
pbmc3k <- readRDS(url("https://alserglab.wustl.edu/files/mascarade/examples/pbmc3k_seurat5.rds"))
pbmc3k <- NormalizeData(pbmc3k)
pbmc3k

## ----eval=FALSE, message=FALSE, warning=FALSE---------------------------------
# if (requireNamespace("SeuratData")) {
#     if (!AvailableData()["pbmc3k", "Installed"]) {
#         InstallData("pbmc3k")
#     }
#     LoadData("pbmc3k")
# 
#     pbmc3k <- UpdateSeuratObject(pbmc3k.final)
#     pbmc3k
# }

## -----------------------------------------------------------------------------
maskTable <- generateMaskSeurat(pbmc3k)

## ----seurat-dimplot-----------------------------------------------------------
DimPlot(pbmc3k) + NoLegend() +
    fancyMask(maskTable, ratio=1)

## ----seurat-dimplot-noborder--------------------------------------------------
DimPlot(pbmc3k) + NoLegend() +
    fancyMask(maskTable, linewidth = 0, ratio=1)

## ----seurat-gnly--------------------------------------------------------------
FeaturePlot(pbmc3k, "GNLY", cols=c("grey90", "red")) +
    fancyMask(maskTable, ratio=1)

## ----message=FALSE, warning=FALSE---------------------------------------------
featureList <- c("MS4A1", "GNLY", "CD3E", "CD14")
plots <- FeaturePlot(pbmc3k, features=featureList, cols=c("grey90", "red"), combine = FALSE)
plots <- lapply(plots, `+`, fancyMask(maskTable, ratio=1, linewidth=0.5, label=FALSE))
patchwork::wrap_plots(plots)

## ----message=FALSE, warning=FALSE---------------------------------------------
pbmc3k <- RunTSNE(pbmc3k)

maskTable <- generateMaskSeurat(pbmc3k, reduction = "tsne")

plots <- FeaturePlot(pbmc3k, 
                     features=featureList,
                     reduction = "tsne",
                     cols=c("grey90", "red"),
                     combine = FALSE)
plots <- lapply(plots, `+`, fancyMask(maskTable, ratio=1, linewidth=0.5, label=FALSE))

patchwork::wrap_plots(plots)

## -----------------------------------------------------------------------------
sessionInfo()

