Classes<-read.csv("C:/Users/Ben/Documents/Assignment 2/classes.csv")
              

Classes<-do.call<- paste, c(Classes[c( "First.name","Last.name")], sep = " "))

as.data.frame(Classes)

Classes

View(Classes)

Names <- merge(Classes$First.name, Classes$Last.name)



m <- cbind(Classes$First.name, Classes$First.name:Classes$Last.name)




Classes$First.name<- with(Classes, paste0(First.name, Last.name))

Classes$Last.name<-NULL

colnames(Classes$First.name)[2] <- "Name"

library(plyr)

rename(Classes, c("First.name"="Name"))

View(Classes)

names(Classes)[1] <- "Name"


library(igraph)

directed_graph<- graph.data.frame(Classes, directed = TRUE)

undirected_graph<- graph.data.frame(Classes, directed = FALSE)


plot(directed_graph)
plot(undirected_graph)


Classes <- dplyr::select(Classes, Course1, Course2, Course3, Course4, Course5)

EDGE <- dplyr::count(Classes, Course1, Course2, Course3, Course4 ,Course5)



Common.Classes <- dplyr::select(Classes, Course1, Course2, Course3, Course4, Course5)

Common.Classes <- unique(Common.Classes)
#Add a variable that describes that they are teachers
Common.Classes <- "Common Classes"

View(Common.Classes)



Uncommon.Classes <- dplyr::select(Classes, Course1, Course2, Course3, Course4, Course5)
Uncommon.Classes <- unique(Uncommon.Classes)
Uncommon.Classes$group <- "Uncommon Classes"

View(Uncommon.Classes)


names(Common.Classes) <- c("Course1", "Course2", "Course3", "Course4", "Course5")
names(Uncommon.Classes)<- c("Course1", "Course2", "Course3", "Course4", "Course5")

VERTEX <- dplyr::bind_rows(Common.Classes,Uncommon.Classes)



g <- graph.data.frame(EDGE, directed=TRUE, vertices=VERTEX)


