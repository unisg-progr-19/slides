

# convert all .R to .Rmd, also add link to R script
path <- normalizePath(here::here("bookdown/R"))
path_output <- normalizePath(here::here("docs"))

r.files <- list.files(path, pattern = "\\.[rR]$", full.names = TRUE)

spin_with_link <- function(r.file) {
  knitr::spin(r.file, knit = FALSE)
  rmd.file <- gsub("\\.[rR]$", ".Rmd", r.file)
  inp <- readLines(rmd.file)
  link.txt <- c("", paste0("[Download as R script](", basename(r.file),")"), "")
  out <- c(inp[1], link.txt, inp[-1])
  writeLines(out, rmd.file)
}

lapply(r.files, spin_with_link)

# render bookdown   output_dir = NULL

owd <- getwd()
setwd(path)
bookdown::render_book("index.Rmd", output_dir = path_output)
setwd(owd)



# clean up
# file.remove(gsub("\\.[rR]$", ".Rmd", r.files))


# copy to server
# system("aws s3 rm --recursive s3://www.cynkra.com/courses/day2")
# system("aws s3 cp _book s3://www.cynkra.com/ws/2019-10-unisg  --recursive")



