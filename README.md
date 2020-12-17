---
 title: "Assignment 2"
 author: "Charles Lang"
 date: "September 24, 2020"
 author: "Yucheng Pan"
 date: "September 26, 2020"
 output: html_document
 ---
 #Part I
 # Part I

 ## Data Wrangling
 In the hackathon a project was proposed to collect data from student video watching, a sample of this data is available in the file video-data.csv.
 @@ -65,7 +65,7 @@ y <- c(2,4,2,3,2,4,3)
 table1 <- table(x,y)

 #Display the table as a Barplot
 barplot(table1)
 barplot(table1, beside = T)

 #Create a data frame of the average total key points for each year and plot the two against each other as a lines

 @@ -86,82 +86,98 @@ D5 <- D1[,c(2,5,6,7)]
 #Draw a matrix of plots for every combination of variables
 pairs(D5)
 ```
 ## Part II

 # Part II

 1. Create a simulated data set containing 100 students, each with a score from 1-100 representing performance in an educational game. The scores should tend to cluster around 75. Also, each student should be given a classification that reflects one of four interest groups: sport, music, nature, literature.

 ```{r}
 #rnorm(100, 75, 15) creates a random sample with a mean of 75 and standard deviation of 20
 #rnorm(100, 75, 15) creates a random sample with a mean of 75 and standard deviation of 15
 score <- rnorm(100, 75, 15)
 #pmax sets a maximum value, pmin sets a minimum value
 score <- pmin(score, 100)
 score <- pmax(score, 0)
 #round rounds numbers to whole number values
 score <- round(score)
 #sample draws a random samples from the groups vector according to a uniform distribution


 group <- sample(c('sport','music','nature','literature'), 100, replace = TRUE)
 df1 <- data.frame(score, group)
 ```

 2. Using base R commands, draw a histogram of the scores. Change the breaks in your histogram until you think they best represent your data.

 ```{r}

 hist(df1$score)
 hist(df1$score, breaks = 100)
 hist(df1$score, breaks = 100, ylim = c(0,3))
 hist(df1$score, breaks = c(40,60,70,80,90,100))
 ```


 3. Create a new variable that groups the scores according to the breaks in your histogram.

 ```{r}
 #cut() divides the range of scores into intervals and codes the values in scores according to which interval they fall. We use a vector called `letters` as the labels, `letters` is a vector made up of the letters of the alphabet.

 rank <- cut(score, c(40,60,70,80,90,100), labels = LETTERS[1:5])
 ```

 4. Now using the colorbrewer package (RColorBrewer; http://colorbrewer2.org/#type=sequential&scheme=BuGn&n=3) design a pallette and assign it to the groups in your data on the histogram.

 ```{r}
 library(RColorBrewer)
 #Let's look at the available palettes in RColorBrewer

 #The top section of palettes are sequential, the middle section are qualitative, and the lower section are diverging.
 #Make RColorBrewer palette available to R and assign to your bins

 #M display.brewer.all 
 display.brewer.all()
 #Use named palette in histogram

 hist(df1$score, breaks = c(0,60,70,80,90,100),col = brewer.pal(n = 5, name = "Blues"))
 ```


 5. Create a boxplot that visualizes the scores for each interest group and color each interest group a different color.

 ```{r}
 #Make a vector of the colors from RColorBrewer

 boxplot(df1$score~df1$group, xlab = "Score", ylab = "Group", col = brewer.pal(n = 4, name = "Set1"))
 ```


 6. Now simulate a new variable that describes the number of logins that students made to the educational game. They should vary from 1-25.

 ```{r}

 login.times <- rnorm(100, 15, 5)
 login.times <- pmax(login.times, 1)
 login.times <- pmin(login.times, 25)
 login.times <- round(login.times)
 df1$login.times <- login.times
 ```

 7. Plot the relationships between logins and scores. Give the plot a title and color the dots according to interest group.

 ```{r}


 library(ggplot2)
 ggplot(df1, aes(x=login.times, y=score, color=group)) + 
     geom_point() +
     scale_color_brewer(palette = "Set1") +
     labs(title = 'Relationships between logins and scores', x = 'logins', y = 'score')
 ```


 8. R contains several inbuilt data sets, one of these in called AirPassengers. Plot a line graph of the the airline passengers over time using this data set.

 ```{r}

 plot(AirPassengers)
 ```


 9. Using another inbuilt data set, iris, plot the relationships between all of the variables in the data set. Which of these relationships is it appropraiet to run a correlation on? 
 9. Using another inbuilt data set, iris, plot the relationships between all of the variables in the data set. Which of these relationships is it appropriate to run a correlation on? 

 ```{r}

 pairs(iris)
 ```

 We can run correlation on Sepal.Length ~ Speal.Length, Sepal.Length ~ Petal.Length, Sepal.Length ~ Petal.Width and Petal.Length ~ Petal.Length

 # Part III - Analyzing Swirl

 ## Data 
 @@ -183,21 +199,65 @@ The variables are:
 `skipped` - whether the student skipped the question  
 `datetime` - the date and time the student attempted the question  
 `hash` - anonymyzed student ID  
 ```{r}
 DF1 <- read.csv("swirl-data.csv", header = TRUE)
 head(DF1,5)
 ```

 3. Create a new data frame that only includes the variables `hash`, `lesson_name` and `attempt` called `DF2`
 ```{r}
 DF2 <- DF1[,c(8, 2, 5)]
 DF2 <- na.omit(DF2)
 head(DF2,5)
 ```


 4. Use the `group_by` function to create a data frame that sums all the attempts for each `hash` by each `lesson_name` called `DF3`
 ```{r}
 DF3 <- DF2 %>% group_by(hash,lesson_name) %>% summarise(count = n())
 head(DF3,5)
 ```

 5. On a scrap piece of paper draw what you think `DF3` would look like if all the lesson names were column names

 6. Convert `DF3` to this format  
 ```{r}
 DF3 <- DF3 %>% group_by(lesson_name) %>% summarise(count = sum(count))
 head(DF3,5)
 ```

 7. Create a new data frame from `DF1` called `DF4` that only includes the variables `hash`, `lesson_name` and `correct`
 ```{r}
 DF4 <- DF1[,c(8, 2, 4)]
 DF4 <- na.omit(DF4)
 head(DF4,5)
 ```

 8. Convert the `correct` variable so that `TRUE` is coded as the **number** `1` and `FALSE` is coded as `0`  
 ```{r}
 DF4[DF4 == 'TRUE'] <- 1
 DF4[DF4 == 'FALSE'] <- 0
 DF4$correct <- as.numeric(DF4$correct)
 head(DF4,5)
 ```

 9. Create a new data frame called `DF5` that provides a mean score for each student on each course
 ```{r}
 DF5 <- DF1[c(8, 1, 2, 4)]
 DF5 <- filter(DF5, correct == 'TRUE')
 DF5 <- DF5 %>% group_by(hash, course_name, lesson_name) %>% summarise(score = n())
 DF5 <- DF5 %>% group_by(hash, course_name) %>% summarise(mean.score = round(mean(score), 2))
 head(DF5,5)
 ```

 10. **Extra credit** Convert the `datetime` variable into month-day-year format and create a new data frame (`DF6`) that shows the average correct for each day
 ```{r}
 DF6 <- DF1[c(8, 7, 4)]
 DF6 <- filter(DF6, datetime > 15049775, correct == 'TRUE')
 DF6$datetime = as.Date(as.POSIXct(DF6$datetime, origin="1970-01-01"))
 DF6 <- DF6 %>% group_by(hash, datetime) %>% summarise(correct = n())
 DF6 <- DF6 %>% group_by(datetime) %>% summarise(average.correct = mean(correct))
 head(DF6,5)
 ```

 Finally use the knitr function to generate an html document from your work. Commit, Push and Pull Request your work back to the main branch of the repository. Make sure you include both the .Rmd file and the .html file. 