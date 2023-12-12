acf_plotter <- function(data, sector){
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


   p1<- ggAcf(play_df, main = glue("ACF: {sector} Returns"))+
       theme_fmx()+
       theme(axis.title.x = element_blank())
   p2<- ggAcf(abs(play_df), main = glue("ACF: {sector} Absolute Returns"))+
       theme_fmx()+
       theme(axis.title.x = element_blank())
   p3<-  ggAcf(play_df^2, main = glue("ACF: {sector} Squared Returns"))+
       theme_fmx()

   grid.arrange(finplot(p1), finplot(p2), finplot(p3), ncol = 1)
}