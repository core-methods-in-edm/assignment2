PN <- read.csv("hudk4050-classes.csv")
View(PN)

#creating a full name for uniqueness
PN2 <- transform(PN, Full.name = paste(First.name, Last.name, sep=''))[-c(2:1)]
View(PN2)

#reshaping data vertically
PN3 <- tidyr::gather(PN2, "Full.name", "Course1", 1:5)
View(PN3)

#renaming the reshaped data
names(PN3) <- c("Full.name", "course_number", "course_name")

#Removing all blank classes
PN4 <- dplyr::filter(PN3, course_name > 0)

#Taking only the class_name and Full.name
PN5 <- dplyr::select(PN4, 1,3)

#
PN5$intersection <- "1"

#Spreading to show course at top and full name, 1's if in class, 0 If not
PN6 <- tidyr::spread(PN5, "course_name", "intersection")
PN6[is.na(PN6)] <- 0