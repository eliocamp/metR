temp <- tempdir()
pkgbuild::build(dest_path = temp)

repo <- "docs/repo"
unlink(repo, recursive = TRUE)
dir.create(repo)

miniCRAN::addLocalPackage("metR", pkgPath = temp, path = repo)
