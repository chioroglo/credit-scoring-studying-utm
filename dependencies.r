dependencies <- c("httpgd", "tibble", "dplyr", "stringr")


for (package in dependencies) {
    if (!requireNamespace(package, quietly = TRUE)) {
        install.packages(package)
    } else {
        cat(paste("Requirement for", package, "is already satisfied\n",sep = " "))
    }
}