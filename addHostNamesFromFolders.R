path <- file.path("C:", "Users", "Merika", "Documents", "2015 Fall", "Rosie", 'DRM', "DRM Pilot A2", "Room Specific Subject Data")
hostnames <- list.dirs(path,full.names = TRUE, recursive = FALSE)

for (directory in hostnames) {
  dumb_files <- list.files(path = file.path(directory), pattern = "*Data.csv")
  smart_files <- sapply(dumb_files, function(x) file.path("data",x))
  x <- addIDvariable(smart_files, name = "Hostname", value = basename(directory), position = 3,
                     write = TRUE)
}
