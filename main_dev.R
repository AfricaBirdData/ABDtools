# 12-5-2021

# Script to keep track of package development

# DO NOT RUN EVERYTHING AGAIN

# If you want to add a function copy and paste a function block
# at the end of the script (before the install part) and run
# that section that you just pasted.

rm(list = ls())

library(devtools)
library(testthat)


# Create a package structure
create_package("/mnt/DATA/Documentos/mis_trabajos/Academic/BIRDIE/ABDtools")

# Add license
use_mit_license("BIRDIE Development Team")

# Remember to edit the DESCRIPTION file

# Create RMarkdown README
usethis::use_readme_rmd()


# Imports -----------------------------------------------------------------

# Import pipe
use_pipe()

# Import packages
usethis::use_package("dplyr")
usethis::use_package("rgee")
usethis::use_package("lifecycle")

# Function listCwacSites --------------------------------------------------

# Add function
use_r("listCwacSites")

# test locally
load_all()

listCwacSites("North West")

# Add documentation
# Add ROxygen skeleton manually
document()

check()

# Add tests

use_testthat()

use_test()

test()





# Install -----------------------------------------------------------------

devtools::install()


