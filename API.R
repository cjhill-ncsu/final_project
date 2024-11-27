
library(tidymodels)
library(ggplot2)


data <- readRDS("processed_data.rds")
final_rf_model <- readRDS("final_rf_model.rds")


#* Predict diabetes status
#* @param BMI Numeric. Body Mass Index. Default: 28
#* @param Sex Factor. Default: "Female"
#* @param Age Factor. Default: "70-74"
#* @param GenHlth Factor. Default: "Very Good"
#* @param HeartDiseaseorAttack Factor. Default: "No" 
#* @post /pred
function(BMI = 28, 
         Sex = "Female", 
         Age = "70-74", 
         GenHlth = "Very Good", 
         HeartDiseaseorAttack = "No") {
  
  input_data <- data.frame(
    BMI = as.numeric(BMI),
    Sex = factor(Sex, 
                 levels = levels(data$Sex)),
    Age = factor(Age, 
                 levels = levels(data$Age), 
                 ordered = is.ordered(data$Age)),
    GenHlth = factor(GenHlth, 
                     levels = levels(data$GenHlth),
                     ordered = is.ordered(data$GenHlth)),
    HeartDiseaseorAttack = factor(HeartDiseaseorAttack, 
                                  levels = levels(data$HeartDiseaseorAttack))
  )
  
  predict(final_rf_model, new_data = input_data, type = "prob")
}
#Query with: 


#* Plot the confusion matrix
#* @serializer png
#* @get /confusion
function() {

  predictions <- predict(final_rf_model, new_data = data, type = "class")
  
  cm_data <- data.frame(
    truth = factor(data$Diabetes_binary, 
                   levels = levels(data$Diabetes_binary)),
    prediction = factor(predictions$.pred_class, 
                        levels = levels(data$Diabetes_binary))
  )
  
  cm <- conf_mat(data = cm_data, 
                 truth = truth, 
                 estimate = prediction)
  
  p <- autoplot(cm, type = "heatmap")
  
  print(p)
}
#Query with: http://127.0.0.1:8000/confusion


#* Information about the API
#* @get /info
function() {
  list(
    name = "Chris Hill",
    github_pages_url = "https://cjhill-ncsu.github.io/final_project/"
  )
}
#Query with: http://127.0.0.1:8000/info