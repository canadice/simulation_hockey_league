
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
###                          LAST EDIT: 2020-12-31                         ###
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
    true_position = Posistion.Real
  ) %>% 
  mutate(
    true_position = factor(true_position)
  )


##-------------------------------------------------------------------
##  Splitting the data to training and test based on known labels   -
##-------------------------------------------------------------------

train_data <- 
  data %>% 
  filter(
    !is.na(true_position)
  )

test_data <-
  data %>% 
  filter(
    is.na(true_position)
  )

##----------------------------------------------------------------
##                        Training a model                       -
##----------------------------------------------------------------

position_formula <- 
  as.formula(
    paste(
      "true_position",
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
    data = train_data,
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









