# Principal Component Analysis

I used data from the Assistments Intelligent Tutoring system. This system gives students hints based on how they perform on math problems. I wanted to see if I can build a decision tree to help teachers decide which students to follow up with, based on students' performance in Assignments. I create three groups ("teacher should intervene", "teacher should monitor student progress" and "no action") based on students' previous use of the system and how many hints they use. I will be building a decision tree using the "party" package. The party package builds decision trees based on a set of statistical stopping rules.

Examine the PrincipleComponentAnalysis.html for the results. 

# Codebook
id - student id
prior_prob_count - The number of problems a student has done in the system prior to the surrent session  
score - The score the student achieved in the current session  
hints - The number of hints the student requested in the current session  
hint.y - Whether or not the student asked for hints in the current session  
complete - Whether or not the student completed the cirrent session  
action - The action suggested by the system to a teacher about a given student based on their performance

- prior_percent_correct: The percentage of problems a student has answered correctly prior to this session
- problems_attempted: The number of problems the student has attempted in the current session
- mean_correct: The average number of correct answers a student made on their first attempt at problems in the current session
- mean_hint: The average number of hints a student asked for in the current session
- mean_attempt: The average number of attempts a student took to answer a problem in the current session
- mean_confidence: The average confidence each student has in their ability to answer the problems in the current session

