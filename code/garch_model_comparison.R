# Wrapping the function from the practical into a function so that it can be mapped onto
garch_model_comparison <- function(data, sector) {
    models <- 1:4
    model_list <- list()

    for (p in models) {
        garchfit <- ugarchspec(
            variance.model = list(model = c("sGARCH", "gjrGARCH", "eGARCH", "apARCH")[p], garchOrder = c(1, 1)),
            mean.model = list(armaOrder = c(1, 0), include.mean = TRUE),
            distribution.model = c("norm", "snorm", "std", "sstd", "ged", "sged", "nig", "ghyp", "jsu")[1]
        )

        garchfit1 <- ugarchfit(spec = garchfit, data = as.numeric(data[[sector]]))
        model_list[[p]] <- garchfit1
    }

    names(model_list) <- c("sGARCH", "gjrGARCH", "eGARCH", "apARCH")

    fit_mat <- sapply(model_list, infocriteria)
    rownames(fit_mat) <- rownames(infocriteria(model_list[[1]]))

    # Combine the results into a single data frame
    result_df <- data.frame(
        Sector = sector,
        fit_mat
    )

    result_df
}