#load relevant packages
library(tibble)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(packrat)

# #collect package dependencies
install.packages("packrat")
ggplot2<-packrat:::recursivePackageDependencies("ggplot2", ignore = "", lib.loc = .libPaths()[1])
survminer<-packrat:::recursivePackageDependencies("survminer", ignore = "", lib.loc = .libPaths()[1])
tidyverse<-packrat:::recursivePackageDependencies("tidyverse", ignore = "", lib.loc = .libPaths()[1])
haven<-packrat:::recursivePackageDependencies("haven", ignore = "", lib.loc = .libPaths()[1])
magrittr<-packrat:::recursivePackageDependencies("magrittr", ignore = "", lib.loc = .libPaths()[1])
DescTools<-packrat:::recursivePackageDependencies("DescTools", ignore = "", lib.loc = .libPaths()[1])
yardstick<-packrat:::recursivePackageDependencies("yardstick", ignore = "", lib.loc = .libPaths()[1])
patchwork<- packrat:::recursivePackageDependencies("patchwork", ignore = "", lib.loc = .libPaths()[1])
tidyr<- packrat:::recursivePackageDependencies("tidyr", ignore = "", lib.loc = .libPaths()[1])
survival<-packrat:::recursivePackageDependencies("survival", ignore = "", lib.loc = .libPaths()[1])
mice<- packrat:::recursivePackageDependencies("mice", ignore = "", lib.loc = .libPaths()[1])
cchsflow<-packrat:::recursivePackageDependencies("cchsflow", ignore = "", lib.loc = .libPaths()[1])
recodeflow<-packrat:::recursivePackageDependencies("recodeflow", ignore = "", lib.loc = .libPaths()[1])
dials<-packrat:::recursivePackageDependencies("dials", ignore = "", lib.loc = .libPaths()[1])
tune<-packrat:::recursivePackageDependencies("tune", ignore = "", lib.loc = .libPaths()[1])
purrr<- packrat:::recursivePackageDependencies("purrr", ignore = "", lib.loc = .libPaths()[1])
dplyr<-packrat:::recursivePackageDependencies("dplyr", ignore = "", lib.loc = .libPaths()[1])
tibble<-packrat:::recursivePackageDependencies("tibble", ignore = "", lib.loc = .libPaths()[1])
stringr<-packrat:::recursivePackageDependencies("stringr", ignore = "", lib.loc = .libPaths()[1])

#Create dependencies list
deps_list<- list(ggplot2=ggplot2, survminer=survminer, tidyverse=tidyverse, haven=haven, magrittr=magrittr, DescTools=DescTools, yardstick=yardstick, patchwork=patchwork, tidyr=tidyr, survival=survival, mice=mice, 
cchsflow=cchsflow, recodeflow=recodeflow, dials=dials, tune=tune, purrr=purrr, dplyr=dplyr, tibble=tibble, stringr=stringr)

#create dependency table in grou
dependency_table1 <- enframe(deps_list, name = "Package", value = "Dependencies") %>%
  mutate(Dependencies = map_chr(Dependencies, ~ paste(.x, collapse = ", ")))

dependency_table1

#create long form dependency table
dep_long <- enframe(deps_list, name = "Package", value = "Dependency") %>%
  unnest(Dependency) %>%
  mutate(Dependency = as.character(Dependency)) %>%
  filter(!is.na(Dependency), Dependency != "") %>%
  distinct(Package, Dependency)

# pivot the table to wide format
dependency_table <- dep_long %>%
  mutate(value = 1L) %>%
  pivot_wider(
    names_from  = Dependency,
    values_from = value,
    values_fill = 0L
  ) %>%
  arrange(Package)

#Order columns alphabetically (dependencies grouped)
dep_cols <- setdiff(names(dependency_table), "Package")
dependency_table <- dependency_table %>%
  select(Package, sort(dep_cols))


dependency_table



####create dependency network####
###----------######

# identify packages of interest in roots
roots <- c(
  "ggplot2","survminer","tidyverse","haven","magrittr",
  "DescTools","yardstick","patchwork","tidyr","survival", "tune", "mice", "cchsflow",
  "recodeflow", "dials", "purrr", "tibble", "stringr"
)

# Use your primary library (change if you want)
lib1 <- .libPaths()[1]

# We'll map dependencies against installed packages in lib1
db <- installed.packages(lib.loc = lib1)

# Identify base/recommended packages to optionally de-emphasize or drop
base_rec <- rownames(db[!is.na(db[, "Priority"]) & db[, "Priority"] %in% c("base","recommended"), , drop = FALSE])

# Helper to get direct deps for one package
direct_deps <- function(pkg, db_mat) {
  out <- tools::package_dependencies(
    packages = pkg,
    db       = db_mat,
    which    = c("Depends","Imports","LinkingTo"),
    recursive = FALSE
  )[[1]]
  # Drop bare "R" and duplicates
  out <- unique(setdiff(out, "R"))
  out
}

# Expand recursively but record only direct edges at each step
dependency_edges <- function(roots, db_mat, drop_base = FALSE) {
  seen   <- character(0)
  queue  <- unique(roots)
  edges  <- list()

  while (length(queue)) {
    pkg <- queue[1]
    queue <- queue[-1]
    if (pkg %in% seen) next
    seen <- c(seen, pkg)

    deps <- direct_deps(pkg, db_mat)
    if (drop_base) deps <- setdiff(deps, base_rec)

    if (length(deps)) {
      edges[[length(edges) + 1]] <- data.frame(from = pkg, to = deps, stringsAsFactors = FALSE)
      # keep exploring
      queue <- unique(c(queue, deps))
    }
  }

  if (length(edges)) {
    unique(do.call(rbind, edges))
  } else {
    data.frame(from = character(), to = character(), stringsAsFactors = FALSE)
  }
}

edges <- dependency_edges(roots, db, drop_base = FALSE)
head(edges)

# install.packages("igraph") # if needed
install.packages("igraph")
library(igraph)

g <- graph_from_data_frame(edges, directed = TRUE)

# Mark root packages vs. others
V(g)$group <- ifelse(V(g)$name %in% roots, "root", "dependency")

# Simple plot (base igraph)
set.seed(1)
plot(
  g,
  layout = layout_with_fr(g),
  vertex.label = V(g)$name,
  vertex.size  = ifelse(V(g)$group == "root", 18, 10),
  vertex.frame.color = NA,
  vertex.label.cex = ifelse(V(g)$group == "root", 0.9, 0.7),
  vertex.label.color = "black",
  edge.arrow.size = 0.4
)
