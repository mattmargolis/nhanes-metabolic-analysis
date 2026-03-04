raw <- readRDS("output/raw_nhanes.rds")

cat("=== BPQ_J columns ===\n")
print(grep("^BPQ", names(raw[["BPQ_J"]]), value=TRUE))

cat("\n=== BPQ_L columns ===\n")
print(grep("^BPQ", names(raw[["BPQ_L"]]), value=TRUE))

cat("\n=== DEMO_J columns ===\n")
print(grep("^(WTMEC|WTINT|SDMV|WTM)", names(raw[["DEMO_J"]]), value=TRUE))

cat("\n=== DEMO_L columns ===\n")
print(grep("^(WTMEC|WTINT|SDMV|WTM)", names(raw[["DEMO_L"]]), value=TRUE))

cat("\n=== DIQ_J BP-related ===\n")
print(grep("^DIQ", names(raw[["DIQ_J"]]), value=TRUE))

cat("\n=== DIQ_L columns ===\n")
print(grep("^DIQ", names(raw[["DIQ_L"]]), value=TRUE))
