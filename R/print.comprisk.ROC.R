print.comprisk.ROC <-
function(x, ...){
cat("$AUC\n")
print(x$AUC)
cat("\n")
cat("$ROC \n")
print(head(x$ROC, n=10))
cat(" \n ...(only first ten rows displayed)...\n")
}
