return_persistence_plotter <- function(data, sector){
    if(sector == "Financials"){
        play_df<- data %>%
            select(date, Financials) %>%
            tbl_xts()
    }

    if(sector == "Resources"){
        play_df<- data %>%
            select(date, Resources) %>%
            tbl_xts()
    }

    if(sector == "Industrials"){
        play_df<- data %>%
            select(date, Industrials) %>%
            tbl_xts()
    }
    Plotdata = cbind(play_df, play_df^2, abs(play_df))

    colnames(Plotdata) = c("Returns", "Returns_Sqd", "Returns_Abs")

    Plotdata <-
        Plotdata %>% xts_tbl() %>%
        gather(ReturnType, Returns, -date)

    g<- ggplot(Plotdata) +
        geom_line(aes(x = date, y = Returns, colour = ReturnType, alpha = 0.5)) +

        ggtitle(glue("Return Type Persistence: SA {sector}")) +
        facet_wrap(~ReturnType, nrow = 3, ncol = 1, scales = "free") +

        guides(alpha = "none", colour = "none") +
        fmxdat::theme_fmx()

       finplot(g)
}