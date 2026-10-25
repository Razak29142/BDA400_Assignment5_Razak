function (data, period, k_period, d_period) 
{
    rsi_values <- rsi(data, period)
    if (length(k_period) != 1 || is.na(k_period) || k_period < 1 || k_period != floor(k_period) || length(d_period) != 1 || is.na(d_period) || d_period < 1 || d_period != floor(d_period)) 
        stop("Invalid smoothing periods")
    valid_rsi <- rsi_values[!is.na(rsi_values)]
    if (length(valid_rsi) == 0) 
        return(list(k_line = numeric(0), d_line = numeric(0)))
    min_rsi <- min(valid_rsi)
    max_rsi <- max(valid_rsi)
    if (max_rsi == min_rsi) {
        k_values <- rep(0, length(valid_rsi))
    }
    else {
        k_values <- (valid_rsi - min_rsi)/(max_rsi - min_rsi)
    }
    if (length(k_values) < k_period) 
        return(list(k_line = numeric(0), d_line = numeric(0)))
    k_line <- sma(k_values, k_period)
    d_line <- if (length(k_line) >= d_period) {
        sma(k_line, d_period)
    }
    else {
        numeric(0)
    }
    return(list(k_line = k_line, d_line = d_line))
}
