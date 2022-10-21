library(targets)
library(tarchetypes) # need version >= 0.6.0.9000

tar_option_set(packages = c('tidyverse',
                            'sf',
                            'scico',
                            'quarto',
                            'scales',
                            'ggspatial',
                            'rnaturalearthdata',
                            'rnaturalearth',
                            'ggsn',
                            'rmapshaper',
                            'maptiles',
                            'tidyterra'))

# Phase target makefiles
source("1_fetch.R")
source("2_process.R")
# source("3_visualize.R")

# Combined list of target outputs
c(p1_targets, p2_targets)