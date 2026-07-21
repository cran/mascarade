## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.width = 7, fig.height=5)

## ----message=FALSE------------------------------------------------------------
library(mascarade)
library(data.table)
library(ggplot2)
library(colorrepel)

## -----------------------------------------------------------------------------
example <- readRDS(url("https://alserglab.wustl.edu/files/mascarade/examples/pbmc3k_umap.rds"))
data <- data.table(example$dims, 
                   cluster=example$clusters)

maskTable <- generateMask(dims=example$dims, 
                          clusters=example$clusters)

ggplot(data, aes(x=UMAP_1, y=UMAP_2)) + 
    geom_point(aes(color=cluster)) + 
    geom_path(data=maskTable, aes(group=group)) +
    coord_fixed() + 
    theme_classic()
                               

## -----------------------------------------------------------------------------
ggplot(data, aes(x=UMAP_1, y=UMAP_2)) + 
    geom_point(aes(color=cluster)) + 
    fancyMask(maskTable, ratio=1, linewidth = 0) +
    theme_classic() + theme(legend.position = "none")

## -----------------------------------------------------------------------------
example <- readRDS(url("https://alserglab.wustl.edu/files/mascarade/examples/pbmc3k_tsne.rds"))
data <- data.table(example$dims, 
                   cluster=example$clusters)

maskTable <- generateMask(dims=example$dims, 
                          clusters=example$clusters)

ggplot(data, aes(x=tSNE_1, y=tSNE_2)) + 
    geom_point(aes(color=cluster)) + 
    geom_path(data=maskTable, aes(group=group)) +
    coord_fixed() + 
    theme_classic()
                               

## -----------------------------------------------------------------------------
ggplot(data, aes(x=tSNE_1, y=tSNE_2)) + 
    geom_point(aes(color=cluster)) + 
    fancyMask(maskTable, ratio=1, linewidth = 0) +
    theme_classic() + theme(legend.position = "none")

## ----message=FALSE------------------------------------------------------------
example <- readRDS(url("https://alserglab.wustl.edu/files/mascarade/examples/aya.rds"))
data <- data.table(example$dims, 
                   cluster=example$clusters)

maskTable <- generateMask(dims=example$dims, 
                          clusters=example$clusters)

ggplot(data, aes(x=UMAP_1, y=UMAP_2)) + 
    geom_point(aes(color=cluster), size=0.5) + 
    scale_color_repel() +
    geom_path(data=maskTable, aes(group=group)) +
    coord_fixed() + 
    theme_classic()
                               

## ----message=FALSE------------------------------------------------------------
example <- readRDS(url("https://alserglab.wustl.edu/files/mascarade/examples/chiajung1.rds"))
data <- data.table(example$dims, 
                   cluster=example$clusters)

maskTable <- generateMask(dims=example$dims, 
                          clusters=example$clusters)

ggplot(data, aes(x=UMAP_1, y=UMAP_2)) + 
    geom_point(aes(color=cluster), size=0.1) + 
    scale_color_repel() +
    geom_path(data=maskTable, aes(group=group)) +
    coord_fixed() + 
    theme_classic()
                               

## ----message=FALSE------------------------------------------------------------
example <- readRDS(url("https://alserglab.wustl.edu/files/mascarade/examples/chiajung2.rds"))
data <- data.table(example$dims, 
                   cluster=example$clusters)

maskTable <- generateMask(dims=example$dims, 
                          clusters=example$clusters)

ggplot(data, aes(x=UMAP_1, y=UMAP_2)) + 
    geom_point(aes(color=cluster)) + 
    scale_color_repel() +
    geom_path(data=maskTable, aes(group=group)) +
    coord_fixed() + 
    theme_classic()
                               

## ----fig.height=7, fig.width=8, message=FALSE---------------------------------
example <- readRDS(url("https://alserglab.wustl.edu/files/mascarade/examples/vshitov.rds"))
data <- data.table(example$dims, 
                   cluster=example$clusters)

maskTable <- generateMask(dims=example$dims, 
                          clusters=example$clusters)

ggplot(data, aes(x=UMAP1, y=UMAP2)) + 
    geom_point(aes(color=cluster), size=0.1) + 
    scale_color_repel() +
    fancyMask(maskTable, 
              con.type = "line", # ledges clutter the image visually here
              ratio=1, 
              linewidth = 0, 
              limits.expand = c(0.2, 0.1), # more space to the left and right of the plot
              label.buffer = unit(1, "mm"), # smaller buffer zone around clusters
              label.width = unit(25, "mm"), # wrap long lines
              label.margin = margin(1, 1, 1, 1, "pt"), # tighter label margins
              label.fontsize = 10) +
    theme_classic() + theme(legend.position = "none")
                               

## -----------------------------------------------------------------------------
sessionInfo()

