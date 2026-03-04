raw <- readRDS("output/raw_nhanes.rds")

for (comp in c("TRIGLY", "HDL", "TCHOL", "GHB", "GLU")) {
  for (sfx in c("J", "L")) {
    key <- paste0(comp, "_", sfx)
    cat(sprintf("\n=== %s ===\n", key))
    print(names(raw[[key]]))
  }
}
