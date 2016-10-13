D1 <- read.table("discipline-data.csv", sep = ",", header = TRUE)

D1$stid <- as.factor(D1$stid)

D2 <- dplyr::select(D1, tid, stid)

EDGE <- dplyr::count(D2, tid, stid)

names(EDGE) <- c("from", "to", "count")

V.TCH <- dplyr::select(D1, tid, t.gender, t.expertise)

V.TCH <- unique(V.TCH)

V.TCH$group <- "teacher"

V.STD <- dplyr::select(D1, stid, s.gender, s.major)
V.STD <- unique(V.STD)
V.STD$group <- "student"

names(V.TCH) <- c("id", "gender", "topic", "group")
names(V.STD) <- c("id", "gender", "topic", "group")

