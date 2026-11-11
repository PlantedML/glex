# randomPlantedForest from version 0.6 returns a pruned model (class `rpf_pruned`) from `rpf()` by default, and
# renames `ntrees` to `n_families`. The tests of `glex.rpf()` are about the forest: `rpf_forest()` fits it with
# either version.
rpf_prunes <- function() {
  "prune_components" %in% getNamespaceExports("randomPlantedForest")
}

rpf_forest <- function(...) {
  args <- list(...)
  if (rpf_prunes()) {
    names(args)[names(args) == "ntrees"] <- "n_families"
    args$prune <- FALSE
  }
  do.call(randomPlantedForest::rpf, args)
}
