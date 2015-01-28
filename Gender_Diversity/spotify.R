# Name: Clay Gibson
# Class: Directed Reading
# Description: Graph the hiring funnel broken down by gender

require(ggplot2)
require(reshape2)

male <- c(1396, 271, 111, 40, 19, 15)
female <- c(486, 48, 11, 4, 3, 3)
mfcount <- rbind(male, female)
colnames(mfcount) <- c("Apply", "Phone", "Skype", "Person", "Offer", "Offer Accepted")

mfpercent <- as.data.frame(matrix(rep(0,8), nrow = 2))
for(i in 2:ncol(mfcount)) {
  mfpercent[1,i-1] <- mfcount[1,i]/mfcount[1,1]
  mfpercent[2,i-1] <- mfcount[2,i]/mfcount[2,1]
}
colnames(mfpercent) <- c("Phone", "Skype", "Person", "Offer", "Offer Accepted")
row.names(mfpercent) <- c("Male", "Female")

mfpercent <- melt(as.matrix(mfpercent))
colnames(mfpercent) <- c("Gender", "Stage", "Percent")
mfpercent$Percent <- round(mfpercent$Percent*100, digits = 2)
mfpercent$size <- c(271, 48, 111, 11, 40, 4, 19, 3, 15, 3)


ggplot(data = mfpercent, aes(x=Stage, y=Percent, group = Gender, color = Gender)) + 
  geom_line(size=1) + geom_point(aes(size = size)) + scale_size(range = c(2,40)) +
  ylim(0,23)
