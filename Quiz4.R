#                    Mike Lennon Quiz 4         #

#####################    Question 1 #########################
#. Show an appropriate visualization that displays the total number of movies for each decade.

quiz4 <- read.table("C:\\Users\\mike\\Desktop\\Data Aquisition\\movies.tab", 
                    sep="\t", header=TRUE, quote="", comment="")
str(quiz4)
# Need to create a function to label the decade then do a count for each decade then bar chart
whatDecade<- function(inputVector){
    decade <- character()
  for(i in length(inputVector)){
    print(inputVector[1])
    if ((inputVector[i] >= 2010) && (inputVector[i] < 2020) ){
      decade[i]<- "2010s"
    }else if((inputVector[i] >= 2000) && (inputVector[i] < 2010) ){
      decade[i]<- "2000s"
    }else if((inputVector[i] >= 1990) && (inputVector[i] < 2000) ){
      decade[i]<- "90s"
    }else if((inputVector[i] >= 1980) && (inputVector[i] < 1990) ){
      decade[i]<- "80s"
    }else if((inputVector[i] >= 1970) && (inputVector[i] < 1980) ){
      decade[i]<- "70s"
    }   else if((inputVector[i] >= 1960) && (inputVector[i] < 1970) ){
      decade[i]<- "60s"
    } else if((inputVector[i] >= 1950) && (inputVector[i] < 1960) ){
      decade[i]<- "50s"
    } else if((inputVector[i] >= 1940) && (inputVector[i] < 1950) ){
      decade[i]<- "40s"
    }else if((inputVector[i] >= 1930) && (inputVector[i] < 1940) ){
      decade[i]<- "30s"
    }else if((inputVector[i] >= 1920) && (inputVector[i] < 1930) ){
      decade[i]<- "20s"
    }else if((inputVector[i] >= 1910) && (inputVector[i] < 1920) ){
      decade[i]<- "1910s"
    }
    else if((inputVector[i] >= 1900) && (inputVector[i] < 1910) ){
      decade[i]<- "1900s"
    }
    else{  decade[i]<- "unspecified"   }
    
    
  }
   decade
}

yearSubset <- quiz4[, c(2)]
#(is.numeric(yearSubset))

test <- whatDecade(yearSubset)
# was unable to debug this just gives na for all entries

barplot(table(test), col="darkblue", main="Movies per decade", ylim=c(0,150),
        xlab="Decade",ylab="Number of Movies")



#####  Question 2 #####
#. Show the average IMDB user rating for different genres of movies? Has this changed over time?
actionMovieRate <- subset(quiz4, Action == 1, select = c(rating))
summary( actionMovieRate )
#Mean   :5.292 
animationMovieRate <- subset(quiz4, Animation == 1, select = c(rating))
summary( animationMovieRate )
# Mean   :6.584
comedyMovieRate <- subset(quiz4, Comedy == 1, select = c(rating))
summary( comedyMovieRate )
#Mean   : 5.955
dramaMovieRate <- subset(quiz4, Drama == 1, select = c(rating))
summary( dramaMovieRate )
# Mean   : 6.154
documentaryMovieRate <- subset(quiz4, Documentary == 1, select = c(rating))
summary( documentaryMovieRate )
#Mean   :6.651
romanceMovieRate <- subset(quiz4, Romance == 1, select = c(rating))
summary( romanceMovieRate )
# Mean   :6.164
shortMovieRate <- subset(quiz4, Short == 1, select = c(rating))
summary( shortMovieRate )
# Mean   : 6.481

# to test for over time you would feed in movie vectors for each decade
# then see the average ratings of that genre for different decades




##### Question 3 #####
# Is there a relationship between length of movie and movie rating? 
regressCols = c("length", "rating")
q3 = quiz4[regressCols]

lmfit = lm( rating ~ length, data=q3 )
lmfit
summary(lmfit)
# The rating is decreasing slightly with increasing length of movie but 
# the relationship is very significant



#### Question 4  ######
#Is there a relationship between length of movie and genre?
q4 = quiz4
lmfit4 = lm( length ~ Action + Animation + Comedy + Drama + Documentary + Romance + 
               Short, data=q4 )
lmfit4
summary(lmfit4)

# There was a significant relationship for this - Animated movies were shorter  and Romance longer
#Coefficients:
 # (Intercept)       Action    Animation       Comedy        Drama  Documentary      Romance  
#      91.7455       6.8315      -8.3044      -0.6413       6.9231      -2.6293       6.1872

#### Question 5  ######
# Which other variable best predicts total number of votes that a movie received.
# There were too many observations missing for budget and for mpaa so I left them out
q5 = quiz4
#lmfit5 = lm( votes ~ year +  rating + length + Action + Animation + Comedy + Drama
#            + Documentary + Romance + r1 +r2 + r3 + r4 + r5 + r6 + r7 + r8 + r9 +
#              r10 + Short, data=q4 )

lmfit5 = lm( votes ~ r10, data=q4  )

lmfit5
summary(lmfit5)
# All of the variables except for r10 and short were highly significant
# I also did single regressions
# year p value <2e-16 ***
# rating p value <2e-16 ***
# length  p value < 2e-16 ***
# r1  1.3e-08 ***
# r2  6.59e-06 ***
# r3   not good     0.383
# r4  2.67e-06 ***
# r5   <2e-16 ***
# r6  3.95e-09 ***
# r7  9.17e-10 ***
# r8  <2e-16 ***
# r9  <2e-16 ***
# r10 0.767 
# Best p values were year, rating, length, r5,r8 and r9 







