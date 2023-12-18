box_test_foo <- function(data, sectors) {
    results_list <- list()

    for (sector in sectors) {
        if (sector %in% colnames(data)) {
            play_df <- data %>%
                select(date, !!sym(sector)) %>%
                tbl_xts()

            box_test_result <- Box.test(coredata(play_df^2), type = "Ljung-Box", lag = 12)

            results_list[[sector]] <- data.frame(
                TestStatistic = box_test_result$statistic,
                PValue = box_test_result$p.value,
                Lag = box_test_result$parameter
            )
        } else {
            warning(paste("Sector", sector, "not found in the data. Skipping."))
        }
    }
    # Combine all results into a single data frame
    result_df <- do.call(rbind, results_list)

    # Print the data frame as a nice table using kable
    kable(result_df, caption = "Ljung-Box Test Results")
}