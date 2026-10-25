function (data, period) 
{
    if (length(period) != 1 || is.na(period) || period < 1 || period != floor(period)) 
        stop("Period must be a positive integer")
    n <- length(data)
    rsi_values <- rep(NA_real_, n)
    if (n <= period) 
        return(rsi_values)
    differences <- diff(data)
    gains <- pmax(differences, 0)
    losses <- pmax(-differences, 0)
    avg_gain <- sum(gains[1:period])/period
    avg_loss <- sum(losses[1:period])/period
    calculate_rsi <- function(gain, loss) {
        if (loss == 0 && gain == 0) 
            return(50)
        if (loss == 0) 
            return(100)
        100 - (100/(1 + gain/loss))
    }
    rsi_values[period + 1] <- calculate_rsi(avg_gain, avg_loss)
    if (n > period + 1) {
        for (i in (period + 2):n) {
            avg_gain <- (avg_gain * (period - 1) + gains[i - 1])/period
            avg_loss <- (avg_loss * (period - 1) + losses[i - 1])/period
            rsi_values[i] <- calculate_rsi(avg_gain, avg_loss)
        }
    }
    return(rsi_values)
}
