# Load the packages required to reproduce the figures.
local({
  packages <- c("ggplot2", "dplyr", "readxl", "cowplot", "gtable",
                "sf", "scales", "ggspatial")
  installed <- vapply(packages, function(package) {
    length(find.package(package, quiet = TRUE)) > 0L
  }, logical(1))
  missing <- packages[!installed]
  if (length(missing)) {
    command <- paste0("install.packages(c(",
                      paste(encodeString(missing, quote = '"'), collapse = ", "), "))")
    stop("Install the missing R packages, then rerun this script:\n", command,
         call. = FALSE)
  }

  for (package in packages) loadNamespace(package)
  message("All required R packages are ready.")
})
