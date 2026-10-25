function (regressionSource, regressionLength, regressionOffset) 
{
    n <- length(regressionSource)
    if (regressionLength < 2 || regressionLength > n || regressionLength != floor(regressionLength)) {
        stop("Invalid regressionLength")
    }
    if (regressionOffset < 0 || regressionOffset >= regressionLength || regressionOffset != floor(regressionOffset) || regressionLength + regressionOffset > n) {
        stop("Invalid regressionOffset")
    }
    end_index <- n - regressionOffset
    start_index <- end_index - regressionLength + 1
    source_subset <- regressionSource[start_index:end_index]
    index_values <- seq_along(source_subset)
    mean_index <- mean(index_values)
    mean_source <- mean(source_subset)
    numerator <- sum((index_values - mean_index) * (source_subset - mean_source))
    denominator <- sum((index_values - mean_index)^2)
    slope <- numerator/denominator
    intercept <- mean_source - slope * mean_index
    predicted_values <- slope * index_values + intercept
    return(list(slope = slope, intercept = intercept, predicted_values = predicted_values))
}
