
##############################################################################
##############################################################################
###                                                                        ###
###                           POSITION PREDICTOR                           ###
###                                                                        ###
###            A CLASSIFIER USED TO PREDICT THE PLAYER POSITION.           ###
###                  PREDICT BOJO BOX PLAYERS FROM S1-S41.                 ###
###  TRUE LABELS FOR TRAINING COME FROM S42-S56, MANUALLY LABELED BY LUKE  ###
###                                                                        ###
###                            AUTHOR: CANADICE                            ###
###                          CREATED: 2020-12-31                           ###
###                          LAST EDIT: 2021-01-03                         ###
###                                                                        ###
##############################################################################
##############################################################################

##----------------------------------------------------------------
##                  Loading required libraries                   -
##----------------------------------------------------------------

require(dplyr)
require(rpart)
require(rpart.plot)
require(keras)
## Use install_keras() the first time and follow the instructions posted
# install_keras()

##----------------------------------------------------------------
##                  Setting the working directory                -
##----------------------------------------------------------------


## Not needed in some cases
# setwd("..")

##----------------------------------------------------------------
##                    Importing the data sets                    -
##      Data processing to aggregate for player and season       -
##----------------------------------------------------------------

data <- 
  
  ###  Import the player statistics data set
  
  read.csv(
    "data_sets/bojobox_all_skaters.csv", 
    sep = ",", 
    header = TRUE
  ) %>% 
  mutate(
    FHMID = as.numeric(FHMID)
  ) %>%   
  
  ###  Aggregating over player ID and season
  
  group_by(
    FHMID,
    Season
  ) %>% 
  summarize(
    across(
      GamesPlayed:FaceoffWins, 
      sum
    )
  ) %>% 
  
  ### Creating a relative statistic per game
  
  mutate(
    across(
      Goals:FaceoffWins,
      function(x) x/GamesPlayed
    )
  ) %>% 
  
  ###  Importing and joining the true positions                       
  
  left_join(
    read.csv(
      "data_sets/positions_42-56.csv",
      sep = ",",
      header = TRUE
      ),
    by = c("FHMID" = "FHMIDS")
  ) %>% 
  rename(
    truePosition = Posistion.Real
  ) %>% 
  mutate(
    truePosition = truePosition %>% factor()
  ) %>% 
  
  ### Filtering out the seasons where FHM was used, and the data available has changed
  
  filter(
    Season < 53
  )


##-------------------------------------------------------------------
##  Splitting the data to training and test based on known labels   -
##-------------------------------------------------------------------

trainData <- 
  data %>% 
  filter(
    !is.na(truePosition)
  )

testData <-
  data %>% 
  filter(
    is.na(truePosition)
  )

##----------------------------------------------------------------
##                Training a decision tree model                 -
##----------------------------------------------------------------

position_formula <- 
  as.formula(
    paste(
      "truePosition",
      paste(
        colnames(data)[3:37],
        collapse = " + "
      ),
      sep = " ~ "
    )
  )

model <- 
  rpart(
    formula = position_formula,
    data = trainData,
    control = 
      list(
        minsplit = 2,
        xval = 0,
        cp = 0,
        maxdepth = 100,
        maxcompete = 0,
        
        ### As missing values are present we need surrogates
        maxsurrogate = 5,
        usesurrogate = 2
      )
  )

rpart.plot(model)

model$cptable

##----------------------------------------------------------------
##              Setting up a neural network model                -
##----------------------------------------------------------------

### Converts the y to numeric

trainData$truePosition <- 
  trainData$truePosition %>% 
  as.numeric() %>% 
  as.character()

### Converts the training data to a format Tensorflow/Keras understands, matrix form and "one-hot" encoding

xTrain <- 
  as.matrix(
    trainData[, 3:37]
  ) 

xTest <- 
  as.matrix(
    testData[,3:37]
  )
  
### Normalizes all input variables for training and test sets

xTrainScaled <- scale(xTrain)

xTestScaled <- 
  scale(
    xTest,
    center = 
      attr(
        xTrainScaled, "scaled:center"
      ),
    scale =
      attr(
        xTrainScaled, "scaled:scale"
      )
  )

### One-hot encoding of the response variable

yTrain <-
  to_categorical(
    trainData[, colnames(trainData) == "truePosition"] %>% as.matrix(),
    num_classes = NULL
  )[,2:3]

### Defining the architecture of the neural network

nnModel <- 
  keras_model_sequential() %>% 
  layer_dense(
    units = 20, 
    activation = "relu", 
    input_shape = ncol(xTrainScaled),
    use_bias = TRUE, 
    name = "First"
  ) %>% 
  layer_dense(
    units = 40, 
    activation = "relu",
    use_bias = TRUE, 
    name = "Second"
  ) %>% 
  layer_dense(
    units = 20, 
    activation = "relu",
    use_bias = TRUE, 
    name = "Third"
  ) %>% 
  layer_dense(
    units = 2,
    activation = "softmax",
    name = "Output"
  ) %>% 
  
  ### Setting the structure of how the network should be trained
  
  compile(
    loss = "categorical_crossentropy",
    optimizer = 
      optimizer_sgd(
        lr = 0.01,
        momentum = 0.1
      ),
    metrics = c("accuracy")
  )


## Prints the summary of the model
# summary(nnModel)


##--------------------------------------------------------------------
##  Running a model fit to find the best model on the validation data  
##--------------------------------------------------------------------

history <- 
  nnModel %>% 
  fit(
    x = xTrainScaled, 
    y = yTrain, 
    epochs = 200, 
    batch_size = 50, 
    validation_split = 0.20,
    
    ### Allowing for the model to stop early given that the validation loss isn't improved
    callback = 
      callback_early_stopping(
        patience = 10,
        monitor = "val_loss"
      )
  )

### Fits the model to this epoch

prediction <- 
  yTrain %>% 
  cbind(
    predict(
      nnModel,
      xTrainScaled,
      verbose = 1,
      batch_size = 50
    ) %>% 
      as.data.frame() %>% 
      rowwise() %>% 
      mutate(
        prob = pmax(V1, V2)
      ) %>% 
      select(
        prob
      ),
    predict_classes(
      nnModel,
      xTrainScaled,
      verbose = 1,
      batch_size = 50
    ) %>% 
      as.data.frame() %>% 
      rename(
        
      )
  ) 





