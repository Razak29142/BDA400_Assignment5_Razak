function (data) 
{
    if (length(data) == 0) 
        stop("Data must not be empty")
    mean_value <- sum(data)/length(data)
    diff_values <- data - mean_value
    squared_diff <- diff_values^2
    variance <- sum(squared_diff)/length(data)
    standard_deviation <- sqrt(variance)
    return(standard_deviation)
}
