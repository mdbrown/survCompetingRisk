myauc <-
function(array,n0,n1) sum(rank(array)[1:n1]-rank(array[1:n1]))/n0/n1
