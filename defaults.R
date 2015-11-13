
base.dir <- "~/.jobhunter/"
weights.path <- paste0(base.dir, "weights.csv")
listings.path <- paste0(base.dir, "listings.csv")
prettyfields.path <- paste0(base.dir, "pretty.csv")
test.listings.path <- paste0(base.dir, "testlists.csv")
if (!dir.exists(base.dir)) dir.create(base.dir)
