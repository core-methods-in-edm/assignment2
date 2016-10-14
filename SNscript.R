library(tidyr, dplyr)
library(igraph)

K1 <- read.table("hudk4050-classes.csv", sep = ",", header = TRUE)
K2<- tidyr::unite(K1, studentName,First.name, Last.name)


K2$studentName <- as.factor(K2$studentName)
K3 <- tidyr::gather(K2,studentName,courseN)
names(K3) <- c("studentName","courseN","courseName")

K4 <- dplyr::select(K3, courseName, studentName)
K5 <- dplyr::filter(K4, courseName!="HUDK4050")
newEDGE <- dplyr::count(K5, courseName, studentName)
newEDGE = dplyr::filter(newEDGE, courseName!="")
names(newEDGE) <- c("from", "to", "count")

course <- dplyr::select(K5, courseName)
course <- unique(course)
course <- dplyr::arrange(course, courseName)
course <- dplyr::filter(course, courseName!="")
course$group <- "course"

studentNew <- dplyr::select(K3, studentName)
studentNew <- unique(studentNew)
studentNew$group <- "studentNew"

names(course) <- c("id",  "group")
names(studentNew) <- c("id", "group")


VERTEXNew <- dplyr::bind_rows(studentNew, course)
g <- graph.data.frame(newEDGE, directed=TRUE, vertices=VERTEXNew)
plot(g,layout=layout.fruchterman.reingold)

edge.arrow.size=20
vertex.label=NA
vertex.size=5
plot(g,layout=layout.fruchterman.reingold)

#following the code in revere.R
K6 <- dplyr::filter(K5, courseName!="")
K7 <- table(K6)

course.net <- K7 %*% t(K7)
student.net <- t(K7) %*% K7

diag(course.net) <- NA
diag(student.net) <- NA

course.g <- graph.adjacency(course.net,mode="undirected", weighted=NULL, diag=FALSE)
student.g <- graph.adjacency(student.net, weighted=TRUE, mode="undirected", diag=FALSE)

la <- layout.fruchterman.reingold(student.g)
e.wt <- get.edge.attribute(student.g, "weight")

pdf(file="student-view.pdf", width=10, height=10)
plot(student.g, layout=la, vertex.size=15,edge.width=e.wt,vertex.label=V(student.g)$name)
dev.off()

png(file="student-view.png", width=1000, height=1000, res=150)
plot(student.g, layout=la, vertex.size=15,edge.width=e.wt,
     vertex.label=V(student.g)$name)
dev.off()

pdf(file="course-view.pdf", width=22, height=17,pointsize=8)

la <- layout.fruchterman.reingold(course.g)
e.wt <- get.edge.attribute(course.g, "weight")

plot(course.g, layout=la, vertex.size=3,edge.width=0.1,
     vertex.label=V(course.g)$name)
dev.off()

png(file="course-view.png", width=2200, height=1700, res=150)

la <- layout.fruchterman.reingold(course.g)
e.wt <- get.edge.attribute(course.g, "weight")

plot(course.g, layout=la, vertex.size=3,edge.width=0.1,
     vertex.label=V(course.g)$name)
dev.off()

#course.net <- K7 %*% t(K7) before = case 1. person.net <- data %*% t(data) 2.person2.net <- data %*% t(data)
K7.t <- t(K7)
course2.net <- K7 %*% t(K7)
student2.net <- t(K7) %*% K7

diag(course2.net) <- NA
diag(student2.net) <- NA

course2.g <- graph.adjacency(course2.net, mode="undirected", weighted=TRUE, diag=FALSE)
student2.g <- graph.adjacency(student2.net, weighted=NULL, mode="undirected", diag=FALSE)

la <- layout.fruchterman.reingold(student2.g)
e.wt <- get.edge.attribute(student2.g, "weight")

pdf(file="student-weighted-view.pdf", width=20, height=20)
plot(student2.g, layout=la, vertex.size=15,edge.width=e.wt,
     vertex.label=V(student2.g)$name)
dev.off()

png(file="student2-weighted-view.png", width=2000, height=2000, res=150)
plot(student2.g, layout=la, vertex.size=15,edge.width=e.wt,
     vertex.label=V(student2.g)$name)
dev.off()

### Centrality

## Betweenness
btwn.student <- betweenness(student.g)
names(btwn.student) <- V(student.g)$name
ind <- order(-btwn.student)
btwn.student[ind][1:10]

## Eigenvector
cent.eig <- evcent(student.g)
names(cent.eig$vector) <- V(student.g)$name

ind <- order(-cent.eig$vector)
cent.eig$vector[ind][1:10]

#cent.eig$vector[ind][1:10]
#David_Cody       Devan_Goto          Jie_Gao    Jingtong_Feng       Chuheng_Hu 
#1.0000000        1.0000000        1.0000000        0.9194821        0.8816590 
#Samantha_Pepe      Shreya_Goel Jonathan_Stelman   Joshua_Coleman     Zhuqian_Zhou 
#0.8816590        0.6375686        0.5797423        0.5797423        0.5797423 

cent.eig <- evcent(course.g)
names(cent.eig$vector) <- V(course.g)$name
ind <- order(-cent.eig$vector)
cent.eig$vector[ind][1:10]


#cent.eig$vector[ind][1:10]
#HUDK4054  HUDK4029  ITSF4010  HUDM5059  HUDK5059  HUDM5123  HUDK4049  HUDK5030  HUDM5122  MSTU4001 
#1.0000000 0.9173929 0.6149128 0.3270369 0.2049709 0.2049709 0.1418614 0.1196965 0.1196965 0.1196965 

## Kleinberg authority
cent.klein <- authority.score(student.g)
names(cent.klein$vector) <- V(student.g)$name
ind <- order(-cent.klein$vector)
cent.klein$vector[ind][1:10]

#> cent.klein$vector[ind][1:10]
#David_Cody       Devan_Goto          Jie_Gao    Jingtong_Feng       Chuheng_Hu 
#1.0000000        1.0000000        1.0000000        0.9193911        0.8816692 
#Samantha_Pepe      Shreya_Goel Jonathan_Stelman   Joshua_Coleman    Lauren_Romine 
#0.8816692        0.6375663        0.5797494        0.5797494        0.5797494 


pdf(file="student-network-reduced.pdf", width=22, height=17,pointsize=8)
student.g.copy <- student.g
la <- layout.fruchterman.reingold(student.g.copy)
plot(student.g.copy, layout=la, vertex.size=3,vertex.label=V(student.g.copy)$name)
dev.off()

png(file="student-network-reduced.png", width=2200, height=1700, res=140)
student.g.copy <- student.g
la <- layout.fruchterman.reingold(student.g.copy)
plot(student.g.copy, layout=la, vertex.size=3,vertex.label=V(student.g.copy)$name)
dev.off()

png(file="test.png", width=2000, height=1000)
lay <- layout.fruchterman.reingold(student.g)
pr.id <- 200
# Plot the eigevector and betweenness centrality
par(mfrow=c(1,2))
plot(bonpow(student.g, exponent=1), betweenness(student.g))

e.rank <- rank(-evcent(student.g)$vector)
b.rank <- rank(-betweenness(student.g))
c.rank <- rank(-bonpow(student.g, exponent=1))
s.top <- c.rank < 10 | b.rank < 10
text(bonpow(student.g)[s.top], betweenness(student.g)[s.top], cex=0.6, pos=4, labels=V(student.g)$name[s.top])
V(student.g)[pr.id]$color <- "yellow"
E(student.g)$color="grey95"
plot(student.g, layout=lay, vertex.size=2,vertex.label.cex=0.6, vertex.label=V(student.g)$name)
dev.off()
