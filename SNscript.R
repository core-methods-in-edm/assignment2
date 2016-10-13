library(stats)
library(base)
library(igraph)
library(dplyr)
library(tidry)

K1 <- read.table("hudk4050-classes.csv", sep = ",", header = TRUE)
K2<- tidyr::unite(K1, studentName,First.name, Last.name)


K2$studentName <- as.factor(K2$studentName)
K3 <- tidyr::gather(K2,studentName,courseN)
names(K3) <- c("studentName","courseN","courseName")

K4 <- dplyr::select(K3, courseName, studentName)
newEDGE <- dplyr::count(K4, courseName, studentName)
newEDGE = dplyr::filter(newEDGE, courseName!="")
names(newEDGE) <- c("from", "to", "count")

course <- dplyr::select(K4, courseName)
course <- unique(course)
course <- dplyr::arrange(course, courseName)
course <- dplyr::slice(course, 2:38)
course$group <- "course"

studentNew <- dplyr::select(K3, studentName)
studentNew <- unique(student)
studentNew$group <- "studentNew"

names(course) <- c("id",  "group")
names(studentNew) <- c("id", "group")


VERTEXNew <- dplyr::bind_rows(studentNew, course)
g <- graph.data.frame(newEDGE, directed=TRUE, vertices=VERTEXNew)
plot(g,layout=layout.fruchterman.reingold)

edge.arrow.size=10
vertex.label=NA
vertex.size=10
plot(g,layout=layout.fruchterman.reingold)

