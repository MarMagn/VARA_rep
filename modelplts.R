
x <- data.frame(tmp$`30 days`)
x$Type <- "No Codes"
x$Model <- rownames(x)
y <- data.frame(tmp$`30 with readmission codes`)
y$Type <- "With Codes"
y$Model <- rownames(x)
x <- rbind(x,y)
ggplot(data=x, aes(x=Model, y=AUC, fill=Type)) +
  geom_bar(stat="identity", position = position_dodge()) +
  geom_text(aes(label=sprintf("%0.2f", round(AUC, digits = 2))), position = position_dodge(0.9), vjust=1.3, size = 2.5) +
  coord_cartesian(ylim=c(0.62, .76))  + theme_minimal()+
  theme(axis.text.x = element_text(angle = 55, hjust = 1)) + labs(title = "AUC for different models at 90 days",x = "Type of model", y = "AUC")

z <- data.frame(tmp_nn)  
z <- z[,c(3, 7, 11, 15)]


z[, 'Model'] <- row.names(z)

z <- data.frame(matrix(unlist(z), nrow=3, ncol = 5, byrow=F))
names(z) <- c("30 days with codes", "30 days","90 days with codes", "90 days", "Model")
z <- z[,c(2,1,4,3,5)]


z$`30 days` <- as.numeric(as.character(z$`30 days`))
z$`30 days with codes` <- as.numeric(as.character(z$`30 days with codes`))
z$`90 days` <- as.numeric(as.character(z$`90 days`))
z$`90 days with codes` <- as.numeric(as.character(z$`90 days with codes`))


y <- melt(z, id = "Model")




x <- data.frame(tmp)
x <- x[,c(3, 7, 11, 15)]
names(x) <- c("30 days with codes", "30 days","90 days with codes", "90 days")
x <- x[,c(2,1,4,3)]
x[, 'Model'] <- row.names(x)

y <- melt(x, id = "Model")

ggplot(y, aes(variable, value)) +   
  geom_bar(aes(fill = Model), position = "dodge", stat="identity") +
  scale_fill_brewer(palette="Blues") +
  labs(title = "AUC for the different models",x = "Type of model and data", y = "AUC") +
  coord_cartesian(ylim=c(0.55, .76)) +
  facet_grid(.~variable, scales = "free_x") 
  theme(axis.text.x = none)

  theme(axis.text.x = element_text(angle = 55, hjust = 1) ) 
  
ggplot(y, aes(Model, value)) + 
  geom_bar(aes(fill = Model), position = "dodge", stat="identity", color = "steel blue") +
  facet_grid(.~variable) +
  coord_cartesian(ylim=c(0.55, .76)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), title = element_text(size = 18) ) +
  scale_fill_brewer(palette=1, direction = -1) +
  labs(title = "AUC for the different models",x = "Type of model", y = "AUC") +
  guides(fill=FALSE) 
  


x <- data.frame(tmp)
x <- do.call(rbind, tmp)
x <- tmp$`30 days`
x$AUC <- round(x$AUC, digits = 3)
p<-ggplot(data=x, aes(x= row.names(x), y=AUC)) +
  geom_bar(stat="identity", color="dark green", fill = "green", width = 0.5) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  geom_text(aes(label=sprintf("%0.2f", round(AUC, digits = 2))), vjust=1.3, size = 3) +
  coord_cartesian(ylim=c(0.6, 0.7))

p
geom_bar(stat="identity", color="blue", fill="white")

sprintf("%0.2f", round(a, digits = 2))