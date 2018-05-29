
input = read.csv("./data/NCHSData04.csv")

plot(x = c(1:224),y = input$Expected[209:432], col = "black", type="l", ylab='', xlab = '', ylim=range(4:12))
par(new = TRUE)
plot(x = c(1:224),y = input$Threshold[209:432], col = "black", type="l", ylab='', xlab = '', ylim=range(4:12))
par(new = TRUE)
plot(x = c(1:224),y = input$Percent.of.Deaths.Due.to.Pneumonia.and.Influenza[209:432], col = "red", type="l", ylab='', xlab = '', ylim=range(4:12))
#axis(1, at = seq(0, 300, by = 10), las=2)
