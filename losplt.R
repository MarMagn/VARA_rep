train <- final_data %>%
  rename(has_AE = rrr_30)
dd <- datadist(train)
options(datadist = "dd")
agePlot <- function(model, title){
  out <-
    contrast(model,
             b = list(los = 7),
             a = list(los = 0:25))
  tmp <- data.frame(Los = 0:25,
                    Hazard = exp(out$Contrast),
                    Lower = exp(out$Lower),
                    Upper = exp(out$Upper))
  ggplot(tmp, aes(x=Los, y=Hazard)) +
    scale_y_continuous(#trans=log2_trans(),
                       # breaks = trans_breaks("log2", function(x) 2^x),
                       # labels = trans_format("log2", math_format(2^.x)),
                       expand=c(0,0)) +
    scale_x_continuous(expand=c(0,0)) +
    geom_ribbon(aes(ymin=Lower, ymax=Upper),
                fill="#a6bddb", color="#a6bddb", alpha=.5) +
    geom_line() +
    theme_bw() +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=18),
          title=element_text(size=20)) +
    xlab("LOS (days)") +
    ylab("Odds ratio") +
    ggtitle(title)
}
agePlot(lrm(has_AE~rcs(los,4), data = train), title = "Risk associated with LOS")
        
