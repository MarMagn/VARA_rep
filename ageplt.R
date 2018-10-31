agePlot <- function(model, title){
  out <-
    contrast(model,
             b = list(age = 70),
             a = list(age = 40:90))
  tmp <- data.frame(Age = c(40:90),
                    Hazard = exp(out$Contrast),
                    Lower = exp(out$Lower),
                    Upper = exp(out$Upper))
  ggplot(tmp, aes(x=Age, y=Hazard)) +
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
    xlab("Age (years)") +
    ylab("Odds ratio") +
    ggtitle(title)
}

agePlot(lrm(has_AE~ rcs(age,4) , data = train), title = "Risk associated with age")

