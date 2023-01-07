```{r}

https://stackoverflow.com/questions/50315987/how-to-use-predict-function-with-text-categories  = misschien iets

str(Df)

#Name, blurb and both date attributes can't be included in the KNN model
Df_KNN <- subset(Df, select = -c(blurb,name,deadline,launched_at))

str(Df_KNN)

#state contains the outcomes i like to predict. It's type is character which needs to be transformed to factor
Df_KNN$state <- as.factor(Df_KNN$state)

# And Difftime to numeric
Df_KNN$DateDiff <- as.numeric(Df_KNN$DateDiff)

#check form if normalisation is neccesary
summary(Df_KNN[c("pledged", "goal", "DateDiff")])

# Ranges are very different. Goal will have a larger impact then datediff, so normalization is needed

#Normalize ~ From FIDM class
#Function
normalize <- function(x) { # Function takes in a vector
  return ((x - min(x)) / (max(x) - min(x))) # distance of item value - minimum vector value divided by the range of all vector values
}

#Put numeric columns in end of Df for normalize function
Df_KNN <- Df_KNN %>% relocate(goal, .after = last_col()) %>% 
  relocate(pledged, .after = last_col())


nCols <- dim(Df_KNN)[2]
Df_KNN_n <- sapply(5:nCols, function(x) {
  normalize(Df_KNN[,x])
}) %>% as.data.frame()

#check # Still a big difference
summary(Df_KNN_n[c("DateDiff", "pledged", "goal")])
#Df_KNN %>% mutate(across(where(is.numeric), scale))

#Add non numeric columns
Df_KNN_n <- cbind(Df_KNN_n, state = Df_KNN$state ,currency = Df_KNN$currency, country = Df_KNN$country, category = Df_KNN$category, DaysDiff = Df_KNN$DateDiff)


#Create dummy columns to have numeric values 
Df_KNN_n <- dummy_cols(Df_KNN_n, select_columns = c("country", "currency", "category"))

#Create test and train set 70%train 30%test
set.seed(123)
dt = sort(sample(nrow(Df_KNN_n),nrow(Df_KNN_n)*.7)) #https://www.listendata.com/2015/02/splitting-data-into-training-and-test.html

trainDf <- Df_KNN_n[dt,]
testDf <- Df_KNN_n[-dt,]

#Check for imbalance
prop.table(table(trainDf$state)) # No imbalance
# The upsample fucntion will increase the size of the minority class, without losing data. In this case the classes will have the same size
trainDf_up <- upSample( x = trainDf %>% 
                          select(-state),
                        y = trainDf$state,
                        list = F,
                        yname = "state"
)
# check upsample result
table(trainDf_up$state)

#Remove non numeric columns except state for test and train_up dataframes
trainDf_up <- select(trainDf_up, -c(currency, category, country))
testDf <- select(testDf, -c(currency, category, country))


```

# Modeling & Evaluation 
```{r}
#To use KNN the set needs scaling first, because KNN calculates the distance between the data therefor the range between the data must be the same.The scaling process is carried out using the z-score method and the scaling process only changes the scale of the data without changing the distribution of the initial data.
#Scaling train data
train_x <- trainDf_up %>%
  select(-state) %>%
  scale() 


#Save variable
train_y <- trainDf_up$state

#Scaling test data
test_x <- testDf %>%
  select(-state) %>%
  scale( center = attr(train_x, "scaled:center"),
         scale = attr(train_x, "scaled:scale")
  )

#Save variable
test_y <- testDf$state

#The actual modeling with knn3
pred_knn <- knn3Train(train = train_x,
                      test = test_x,
                      cl = train_y,
                      #k = sqrt(nrow(train_x)) %>%
                      k = 7 %>%
                        round()) %>%
  as.factor()


confusionMatrix(pred_knn, test_y, positive = "successful") 


CrossTable(pred_knn, test_y)


```




string = "Hólmer"

chartr(paste(names(unwanted_array), collapse=''),
       paste(unwanted_array, collapse=''),
       string)











