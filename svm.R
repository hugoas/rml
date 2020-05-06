ds = read.csv('C:/Users/hugos/Desktop/FS Dataset/Parkinson/ReplicatedAcousticFeatures-ParkinsonDatabase2019.csv',
              header =TRUE)

#Remover colunas inutiliz?veis              
parkinson <- subset(ds, select = -c(ID,Recording,Gender))

#Separar dados treinamento e teste        
intrain <- createDataPartition(y = parkinson$Status, p= 0.8, list = FALSE)
training <- parkinson[intrain,]
testing <- parkinson[-intrain,]

anyNA(parkinson)

summary(parkinson)

#indicar vari?vel categoriz?vel
training[["Status"]] = factor(training[["Status"]])

#controle de treinamento
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

svm_Linear <- train(Status ~., data = training, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)

test_pred <- predict(svm_Linear, newdata = testing)
test_pred

confusionMatrix(table(test_pred, testing$Status))

#grid

grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
svm_Linear_Grid <- train(Status ~., data = training, method = "svmLinear",
                         trControl=trctrl,
                         preProcess = c("center", "scale"),
                         tuneGrid = grid,
                         tuneLength = 10)
svm_Linear_Grid
plot(svm_Linear_Grid)

#Treinamento dos dados teste

test_pred_grid <- predict(svm_Linear_Grid, newdata = testing)
test_pred_grid

confusionMatrix(table(test_pred_grid, testing$Status))