# NEPHU Population Profile - Aboriginal Health
# Author: Alana Little, NEPHU (alana.little@austin.org.au)
# Version 1.0, 20/02/2025
#
# Code for overlaying CTG target line on charts

# ------------------------------------------------------------------------------
# Closing the Gap target line ----
# ------------------------------------------------------------------------------
f_graph_ctg_target <- function(data, ctg_target) {
  
  figure <- data +
    geom_hline(aes(yintercept = ctg_target),
               col       = colour_table_mid,
               linetype  = "dashed",
               linewidth = 1)
  
  return(figure)
  
}

