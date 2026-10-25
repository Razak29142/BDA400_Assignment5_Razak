function (arr1, arr2) 
{
    if (length(arr1) != length(arr2)) 
        stop("Both arrays should have the same length")
    n <- length(arr1)
    crossover_signals <- rep("None", n)
    if (n > 1) {
        for (i in 2:n) {
            if (arr1[i] > arr2[i] && arr1[i - 1] <= arr2[i - 1]) {
                crossover_signals[i] <- "Up"
            }
            else if (arr1[i] < arr2[i] && arr1[i - 1] >= arr2[i - 1]) {
                crossover_signals[i] <- "Down"
            }
        }
    }
    return(crossover_signals)
}
