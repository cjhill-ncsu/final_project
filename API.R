
library(tidymodels)
library(ggplot2)


data <- readRDS("processed_data.rds")
final_rf_model <- readRDS("final_rf_model.rds")


#* Predict diabetes status
#* @param BMI Numeric. Body Mass Index. Default: 28
#* @param Sex Factor. Default: "Female"
#* @param Age Factor. Default: "70-74"
#* @param Income Factor. Default: "$10,000 to $15,000"
#* @param HeartDiseaseorAttack Factor. Default: "No" 
#* @post /pred
function(BMI = 28, 
         Sex = "Female", 
         Age = "70-74", 
         Income = "$10,000 to $15,000", 
         HeartDiseaseorAttack = "No") {
  
  input_data <- data.frame(
    BMI = as.numeric(BMI),
    Sex = factor(Sex, 
                 levels = levels(data$Sex)),
    Age = factor(Age, 
                 levels = levels(data$Age), 
                 ordered = is.ordered(data$Age)),  # Ensure ordered factor
    Income = factor(Income, 
                    levels = levels(data$Income),
                    ordered = is.ordered(data$Income)),
    HeartDiseaseorAttack = factor(HeartDiseaseorAttack, 
                                  levels = levels(data$HeartDiseaseorAttack))
  )
  
  predict(final_rf_model, new_data = input_data, type = "prob")
}
#Query with: http://127.0.0.1:8000/pred?BMI=28&Sex=Female&Age=70-74&Income=%2410%2C000%20to%20%2415%2C000&HeartDiseaseorAttack=No


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
  
  autoplot(cm, type = "heatmap")
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