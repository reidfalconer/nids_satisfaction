# MODELLING H2O ====

library(h2o)
h2o.init(nthreads=-1,max_mem_size='6G')

h2o.mod = h2o.deeplearning(y = "satisfactions",
                           training_frame = as.h2o(train),
                           activation = "Rectifier", 
                           hidden = c(5,5),
                           epochs = 200,
                           train_samples_per_iteration = -2,
                           variable_importances=T,
                           nfolds = 5)
summary(h2o.mod)
h2o.mod.pred <- h2o.predict(h2o.mod, newdata = as.h2o(test))
accuracy(round(as.vector(h2o.mod.pred)), test$satisfactions)

rfHex <- h2o.randomForest(y = "satisfactions", 
                          ntrees = 20,
                          max_depth = 30,
                          training_frame = as.h2o(train),
                          nfolds = 5)

rfHex.pred <- h2o.predict(rfHex, newdata = as.h2o(test))
accuracy(round(as.vector(rfHex.pred)), test$satisfactions)


h2o.shutdown()