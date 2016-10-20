rm(list = ls())
train = read.csv("http://eeyore.ucdavis.edu/stat141/Data/digitsTrain.csv", header=TRUE) 
# Randomize dataset
set.seed(0) 
new_df = train[sample(nrow(train)),]

distance_df = new_df[-1]

distrance_matrix = as.matrix(dist(distance_df, method = "euclidean"))
View(distrance_matrix)

# CV Combine knn and CV together, notes from Nick's discussion
k = 3
groups = 5
groups_length = nrow(new_df) / groups

cross_validation = function(new_df, label, k, metric){
    predictions = sapply(1:groups, function(g){
    
    lower_bound = (g - 1) * groups_length + 1  
    upper_bound = groups_length * g
    
    test_block = lower_bound: upper_bound
    train_block = -test_block
    
    sub_dist_matrix = distrance_matrix[test_block, train_block]  
    label = new_df$label
    label = label[-test_block]
    predictors = apply(sub_dist_matrix, 1, function(row){
      label[order(row)][1:k]
    })
    predictors = t(predictors)
    return(predictors)
  })
  actual_label = as.data.frame(matrix(new_df$label, nrow = 1000, ncol = 5))
  actual_label = as.matrix(do.call("rbind", replicate(k, actual_label, simplify = FALSE)))
  final = table(Actual = actual_label, Prediction = predictions)
  AR = sum(diag(final))/ sum(final)
  ER = 1 - AR
  ER
}
cross_validation(new_df,label, 1, euclidean)
########################## CV for manhattan method
#dist_matrix2 = as.matrix(dist(distance_df, method = "manhattan"))
# CV
k = 3
groups = 5
groups_length = nrow(new_df) / groups

cross_validation_for_manhattan = function(new_df, label, k, metric){
  predictions = sapply(1:groups, function(g){
    
    lower_bound = (g - 1) * groups_length + 1  
    upper_bound = groups_length * g
    
    test_block = lower_bound: upper_bound
    train_block = -test_block
    
    sub_dist_matrix = dist_matrix2[test_block, train_block]  
    label = new_df$label
    label = label[-test_block]
    predictors = apply(sub_dist_matrix, 1, function(row){
      label[order(row)][1:k]
    })
    predictors = t(predictors)
    return(predictors)
  })
  actual_label = as.data.frame(matrix(new_df$label, nrow = 1000, ncol = 5))
  actual_label = as.matrix(do.call("rbind", replicate(k, actual_label, simplify = FALSE)))
  final = table(Actual = actual_label, Prediction = predictions)
  AR = sum(diag(final))/ sum(final)
  ER = 1 - AR
  ER
}
cross_validation_for_manhattan(new_df,label, 1, manhattan)
##########

k = 1  #Best k
####################
predictions = sapply(1:groups, function(g){
  
  lower_bound = (g - 1) * groups_length + 1  
  upper_bound = groups_length * g
  
  test_block = lower_bound: upper_bound
  train_block = -test_block
  
  sub_dist_matrix = distrance_matrix[test_block, train_block]  
  label = new_df$label
  label = label[-test_block]
  predictors = apply(sub_dist_matrix, 1, function(row){
    label[order(row)][1:k]
  })
  predictors = t(predictors)
  return(predictors)
})
########################
actual_label = as.data.frame(matrix(new_df$label, nrow = 1000, ncol = 5))
actual_label = as.matrix(do.call("rbind", replicate(k, actual_label, simplify = FALSE)))
# Confusion matrix for k 
final = table(Actual = actual_label, Prediction = predictions)
final
#Q1
#k = 1 by using the euclidean method is the best model for me.
#Q2
### Plot showing the overall cross-validation misclassification rate versus number of k = 20 (Euclidean)
num_of_k = 10
graph = sapply(1:num_of_k, function(b){
  cross_validation(new_df,label, b, euclidean)
})
plot(graph, xlab = "Number of k", ylab = "Error rate", main = "Euclidean method")
### Plot showing the overall cross-validation misclassification rate versus number of k = 20 (Manhattan)
graph_m = sapply(1:num_of_k, function(b){
  cross_validation_for_manhattan(new_df,label, b,manhattan)
})
plot(graph_m, xlab = "Number of k", ylab = "Error rate", main = "Manhattan method")
# Q3
final

# #Q4
classification = diag(final)/rowSums(final)
best = which(classification == max(classification))
best
worst = which(classification == min(classification))
worst

#Q5
# Confuse with what
# # for digit 0
# digit0 = final[1,]
# digit0[which(digit0 == (max( digit0[digit0!=max(digit0)] )))]
confused = lapply(1:10, function(c){
  digit = final[c,]
  digit[which(digit == (max(digit[digit != max(digit)])))]
})
confused

#Q6
misclassification = which(predictions != actual_label)
predictions = as.data.frame(predictions)
target = misclassification[1] # The first one that we predicted wrong
View(predictions)
View(actual_label)
label_for_bad_handwriting_actual = actual_label[target]
label_for_bad_handwriting_actual
label_for_bad_handwriting_pred = predictions[target,1]
label_for_bad_handwriting_pred
### Draw prediction
r = target
actual_num = new_df$label[r]
actual_num
draw(new_df[r, -1])
### topk for k = 1
i = 1:1000
sub_dm = distrance_matrix[i, -i]
# Find k nearest neighbor; k = 1
k = 1
topkneighbors = function(x) topk = order(x, decreasing = FALSE)[1:k] #sort
topk = apply(sub_dm, 1, topkneighbors)
predict_num = topk[target]
label = new_df$label
label[predict_num]
draw(new_df[predict_num, -1])
draw_actual = draw(distance_df[target, -1])
### done with 1 misclassification
target2 = misclassification[2]
draw_actual2 = draw(distance_df[target2, -1])
target3 = misclassification[3]
draw_actual3 = draw(distance_df[target3, -1])
target4 = misclassification[4]
draw_actual4 = draw(distance_df[target4, -1])
target5 = misclassification[5]
draw_actual5 = draw(distance_df[target5, -1])
#Part II
new_stuff = split(train, train$label)
# Test
digit1 = as.data.frame(new_stuff[2])
digit1 = digit1[-1]
digit1pixelaverage = colMeans(digit1[1:ncol(digit1)])
sort(digit1pixelaverage, decreasing = TRUE)
# End of testing
all_averages = lapply(new_stuff, colMeans)
everything = t(as.data.frame(all_averages))
everything = everything[,-1]
# distance_matrix_part2 = as.data.frame(as.matrix(dist(everything, method = "euclidean")))
###################
groups = 5
cross_validation_part2 = function(new_stuff, label, k, metric){
  predictions_part2 = sapply(1:groups, function(g){
    
    lower_bound = (g - 1) * groups_length + 1  
    upper_bound = groups_length * g
    
    test_block = lower_bound : upper_bound
    want = train[test_block,]
    want = want[,-1]
    fthat = everything
    train_block = -test_block
    
    orig_df = rbind(want, fthat)
    
    orig_dm = as.matrix(dist(orig_df, method = "euclidean"))
    
    sub_dm = orig_dm[c(1:1000), -c(1:1000)]  
    
    colnames(sub_dm)[] = c(0:9)
    label_part2 = colnames(sub_dm)[]
    
    predictors = apply(sub_dm, 1, function(row){
      label_part2[order(row)[1:k]]})
    predictors = as.integer(predictors)
    predictors = t(predictors)
    return(predictors)
  })
  actual_label = as.data.frame(matrix(train$label, nrow = 1000, ncol = 5))
  actual_label = as.matrix(do.call("rbind", replicate(k, actual_label, simplify = FALSE)))
  final2 = table(Actual = actual_label, Prediction = predictions_part2)
  print(final2)
  AR = sum(diag(final2))/ sum(final2)
  ER = 1 - AR
  ER
}
cross_validation_part2(new_stuff, label, 1, euclidean)
final2 = table(Actual = actual_label, Prediction = predictions_part2)
final2