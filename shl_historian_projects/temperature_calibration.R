
###########################################################################
###########################################################################
###                                                                     ###
###                       TEMPERATURE CALIBRATION                       ###
###                                                                     ###
###########################################################################
###########################################################################

## Inspired by https://github.com/cerlymarco/MEDIUM_NoteBook/blob/master/NeuralNet_Calibration/NeuralNet_Calibration.ipynb
## and https://github.com/stellargraph/stellargraph/blob/develop/stellargraph/calibration.py

require(tensorflow)

temperatureCalibration <- 
  function(xTrain, yTrain, xValid = NULL, yValid = NULL, epochs = 100){
    
    T <- tensorflow::tf$Variable(tensorflow::tf$ones(shape=shape(1,1)))
    history <- list(NULL)
    early_stopping <- FALSE
    optimizer <- keras::optimizer_adam(lr = 0.001)
    
    cost <- function(T, x, y){
      
      scaled_logits = tensorflow::tf$multiply(x=x, y=1.0 / T)
      
      cost_value = tensorflow::tf$reduce_mean(
        tensorflow::tf$nn$softmax_cross_entropy_with_logits(logits=scaled_logits, labels=y)
      )
      
      return(cost_value)
      
    }
    
    grad <- function(T, x, y){
      
      tape <- tensorflow::tf$GradientTape()
      
      cost_value <- cost(T, x, y)
      
      return(cost_value, tape$gradient(cost_value, T))
    }
    
    if(!is.null(xValid)){
      early_stopping = TRUE  
    }
    
    for(epoch in 1:epochs){
      gra
      ### I am here
      
    }
    
    for epoch in range(epochs):
      train_cost, grads = grad(T, X_train, y_train)
    optimizer.apply_gradients(zip([grads], [T]))
    if early_stopping:
      val_cost = cost(T, X_valid, y_valid)
    if (len(history) > 0) and (val_cost > history[-1][1]):
      break
    else: 
      history.append([train_cost, val_cost, T.numpy()[0]])
    else:
      history.append([train_cost, T.numpy()[0]])
    
    history = np.asarray(history)
    temperature = history[-1, -1]
    
    return temperature
    
    
    def calibrated_proba(logits, temperature):
      
      scaled_prediction = logits / temperature
    
    return np.exp(scaled_prediction) / np.sum(np.exp(scaled_prediction), axis=-1, keepdims=True)
    
  }


















