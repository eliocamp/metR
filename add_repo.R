temp <- tempdir()
pkgbuild::build(dest_path = temp)

repo <- "docs"
unlink(repo, recursive = TRUE)
dir.create(repo)

miniCRAN::addLocalPackage("metR", pkgPath = temp, path = repo)
