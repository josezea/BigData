library(sparklyr)
library(data.table)
library(readr)
library(tictoc)
system("java -version")
spark_available_versions()
spark_install()
spark_installed_versions()

dir()
tic()
flights <- readr::read_csv("flights14.csv")
toc()
rm(flights)

tic()
flights <- data.table::fread("flights14.csv")
toc()

?fread
names(iris)
write.csv(iris, "iris.csv", row.names = F)
# Solo primera, tercera y quinta
clases <- c("numeric", "NULL", "numeric",
            "NULL", "character")
iris2 <- fread("iris.csv", colClasses=clases)
iris2
