# Hide this file from R build
usethis::use_build_ignore("dev_history.R")

# Create a vignette
usethis::use_vignette("aa-exploration")

# Document functions and dependencies
attachment::att_to_description()

# Check the package
devtools::check()

# Install package
devtools::install()


# Publish
chameleon::build_pkgdown()
