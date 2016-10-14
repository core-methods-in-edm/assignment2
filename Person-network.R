###Assignment 2
###by jiaxi li

### Read file
library(igraph)
D1 <- read.csv("hudk4050-classes.csv",header = TRUE, sep = ",")

library(dplyr)
library(tidyr)

### Combine first&last name 
D1 <- tidyr::unite(D1,sNAME,First.name,Last.name,sep = "_")

### Reshaping dataset
D2 <- gather(D1,Course,sNAME)
names(D2) <- c("sName", "Courses", "cName")
D3 <- dplyr::select(D2,sName, cName)

### omit missing value
D3$cName <- ifelse(D3$cName=="",NA,D3$cName)
D3 <- na.omit(D3)

###Removing HUDK4050( for everyone is taking this class)
D4 <- subset(D3,cName != "HUDK4050")

### Trans into  matrix
table1 <- table(D4$sName,D4$cName)

### Trans into adjacency matrix
data <- as.matrix(table1, row.names=1)

### construct vertex and edge 
Classmates.net <- data %*% t(data)

### if interested in relationship b/w courses 
Courses.net <- t(data) %*% data 

### igraph 
diag(Classmates.net) <- NA

Classmates.g <- graph.adjacency(Classmates.net ,mode="undirected", weighted=NULL, diag=FALSE)
la <- layout.fruchterman.reingold(Classmates.g)
e.wt <- get.edge.attribute(Classmates.g, "weight")

### graph store in png file
png(file="figures/group-view.png", width=800, height=800, res=150)
plot(Classmates.g, layout=la, vertex.size=5,edge.width=e.wt,vertex.label=V(Classmates.g)$name)
dev.off()

### Centrality

## Betweenness
btwn.Classmates <- betweenness(Classmates.g)
names(btwn.Classmates) <- V(Classmates.g)$name
ind <- order(-btwn.Classmates)
btwn.Classmates[ind]

## Eigenvector
cent.eig <- evcent(Classmates.g)
names(cent.eig$vector) <- V(Classmates.g)$name
ind <- order(-cent.eig$vector)
cent.eig$vector[ind]

## Kleinberg authority
cent.klein <- authority.score(Classmates.g)
names(cent.klein$vector) <- V(Classmates.g)$name
ind <- order(-cent.klein$vector)
cent.klein$vector[ind]

png(file="figures/classmates-network-reduced.png", width=800, height=800, res=80)
Classmates.g.copy <- Classmates.g
la <- layout.fruchterman.reingold(Classmates.g.copy)
plot(Classmates.g.copy, layout=la, vertex.size=5,
     vertex.label=V(Classmates.g.copy)$name)
dev.off()
