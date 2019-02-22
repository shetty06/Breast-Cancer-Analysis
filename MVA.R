#ggplot2 is used to plot the bar plot
#install.packages("ggplot2")
library("ggplot2")
#corrplot is used to plot the correlation matrix
#install.packages("corrplot")
library("corrplot")
#It is used to reshape a one-dimensional array into a two-dimensional array with one column and multiple arrays.
#install.packages("reshape")
library("reshape")

#Reading the dataset
breast_cancer <- read.csv("C:\\Users\\APEKSHA\\Downloads\\wisc_bc_data.csv")

#Displaying the dataset using head function
head(breast_cancer)

#Displays structure of the dataset
str(breast_cancer)

#Displays the names of the columns
names(breast_cancer)

#Displays the summary of the dataset
summary(breast_cancer)

#To display the frequency table
diagnosis.table <- table(breast_cancer$diagnosis)

#Displays the table
#This shows how many patients are benign and malignant 
diagnosis.table

#Generate barplot
ggplot(data=breast_cancer, aes(x=diagnosis)) + geom_bar(stat = "count") 

#Generate Pie chart represented in frequency 
diagnosis.prop.table <- prop.table(diagnosis.table)*100
diagnosis.prop.df <- as.data.frame(diagnosis.prop.table)
pielabels <- sprintf("%s - %3.1f%s", diagnosis.prop.df[,1], diagnosis.prop.table, "%")
colors <- terrain.colors(2)
pie(diagnosis.prop.table,
    labels=pielabels,  
    clockwise=TRUE,
    col=colors,
    border="gainsboro",
    radius=0.8,
    cex=0.8, 
    main="frequency of cancer diagnosis")
legend(1, .4, legend=diagnosis.prop.df[,1], cex = 0.7, fill = colors)

#To Plot histograms of "mean" variables group by diagnosis
data_mean <- breast_cancer[ ,c("diagnosis", "radius_mean", "texture_mean","perimeter_mean", "area_mean", "smoothness_mean", "compactness_mean", "concavity_mean", "symmetry_mean" )]

#Plot histograms 
ggplot(data = melt(data_mean, id.var = "diagnosis"), mapping = aes(x = value)) + 
  geom_histogram(bins = 10, aes(fill=diagnosis), alpha=0.5) + facet_wrap(~variable, scales ='free_x')

#Generate a Scatter plot of two varaible ie. concavity against radius
data <- breast_cancer[,c('concavity_worst','radius_worst')]
plot(x = breast_cancer$concavity_worst,y = breast_cancer$radius_worst,
     xlab = "concavity_worst",
     ylab = "radius_worst",
     main = "Concavity_worst vs radius_worst", 
     pch=15,
     col = c("red","blue")
     )
rug(breast_cancer$concavity_worst, side = 1)
rug(breast_cancer$radius_worst, side = 2)

#Generate Corelation Matrix of columns
corMatMy <- cor(breast_cancer[,3:32])
corrplot(corMatMy, order = "hclust", tl.cex = 0.7)

#Generate Scatterplot Matrix
pairs(~radius_mean+perimeter_mean+area_mean+compactness_mean+concavity_mean,data = breast_cancer,main = "Scatterplot Matrix",col=c("red","blue","green","yellow"))

#Multivariate analysis
#t-tEST
with(data=breast_cancer,t.test(radius_mean[diagnosis=="B"],radius_mean[diagnosis=="M"],var.equal=TRUE))
with(data=breast_cancer,t.test(texture_mean[diagnosis=="B"],texture_mean[diagnosis=="M"],var.equal=TRUE))
with(data=breast_cancer,t.test(perimeter_mean[diagnosis=="B"],perimeter_mean[diagnosis=="M"],var.equal=TRUE))
with(data=breast_cancer,t.test(area_mean[diagnosis=="B"],area_mean[diagnosis=="M"],var.equal=TRUE))
with(data=breast_cancer,t.test(smoothness_mean[diagnosis=="B"],smoothness_mean[diagnosis=="M"],var.equal=TRUE))
with(data=breast_cancer,t.test(compactness_mean[diagnosis=="B"],compactness_mean[diagnosis=="M"],var.equal=TRUE))
with(data=breast_cancer,t.test(concavity_mean[diagnosis=="B"],concavity_mean[diagnosis=="M"],var.equal=TRUE))
with(data=breast_cancer,t.test(points_mean[diagnosis=="B"],points_mean[diagnosis=="M"],var.equal=TRUE))
with(data=breast_cancer,t.test(symmetry_mean[diagnosis=="B"],symmetry_mean[diagnosis=="M"],var.equal=TRUE))
with(data=breast_cancer,t.test(dimension_mean[diagnosis=="B"],dimension_mean[diagnosis=="M"],var.equal=TRUE))

#Hotelling's T2 test
#install.packages("Hotelling")
library(Hotelling)
t2testcan <- hotelling.test(radius_mean + texture_mean + perimeter_mean + area_mean + smoothness_mean + compactness_mean + concavity_mean + points_mean + symmetry_mean + dimension_mean ~ diagnosis, data=breast_cancer)
# Output of the function hotelling.test is given
cat("T2 statistic =",t2testcan$stat[[1]],"\n")
print(t2testcan)
#  T2 statistic is located in the first element of the list "stat"
#View(t2testcan)
#View(breast_cancer)


#Levene's tests based on absolute differences around means using t-tests. Standarizing the data set with scale()
matstand <- scale(breast_cancer[,3:10])
head(matstand)
matben <- matstand[breast_cancer$diagnosis =="B",]
head(matben)
matmalign <- matstand[breast_cancer$diagnosis == "M",]
vecmedianben <- apply(matben, 2, median)
# in the above 2 represents column. Hence, we are asking for column median
vecmedianben

vecmedianmalign <- apply(matmalign, 2, median)
matabsdevben <- abs(matben - matrix(rep(vecmedianben,nrow(matben)),nrow=nrow(matben), byrow=TRUE))

matabsdevmalign <- abs(matmalign - matrix(rep(vecmedianmalign,nrow(matmalign)),nrow=nrow(matmalign), byrow=TRUE))

head(matabsdevmalign)

matabsdev.all <- rbind(matabsdevben,matabsdevmalign)
matabsdev.all <- data.frame(breast_cancer$diagnosis, matabsdev.all)

t.test(matabsdev.all$radius_mean[breast_cancer$diagnosis == "B"],matabsdev.all$radius_mean[breast_cancer$diagnosis == "M"], alternative="less",var.equal = TRUE)
t.test(matabsdev.all$texture_mean[breast_cancer$diagnosis == "B"],matabsdev.all$texture_mean[breast_cancer$diagnosis == "M"], alternative="less",var.equal = TRUE)
t.test(matabsdev.all$perimeter_mean[breast_cancer$diagnosis == "B"],matabsdev.all$perimeter_mean[breast_cancer$diagnosis == "M"], alternative="less",var.equal = TRUE)
t.test(matabsdev.all$area_mean[breast_cancer$diagnosis == "B"],matabsdev.all$area_mean[breast_cancer$diagnosis == "M"], alternative="less",var.equal = TRUE)
t.test(matabsdev.all$smoothness_mean[breast_cancer$diagnosis == "B"],matabsdev.all$smoothness_mean[breast_cancer$diagnosis == "M"], alternative="less",var.equal = TRUE)
t.test(matabsdev.all$compactness_mean[breast_cancer$diagnosis == "B"],matabsdev.all$compactness_mean[breast_cancer$diagnosis == "M"], alternative="less",var.equal = TRUE)
t.test(matabsdev.all$concavity_mean[breast_cancer$diagnosis == "B"],matabsdev.all$concavity_mean[breast_cancer$diagnosis == "M"], alternative="less",var.equal = TRUE)
t.test(matabsdev.all$points_mean[breast_cancer$diagnosis == "B"],matabsdev.all$points_mean[breast_cancer$diagnosis == "M"], alternative="less",var.equal = TRUE)


head(matstand)
matstand.all <- data.frame(breast_cancer$diagnosis, matstand)
head(matstand.all)
colnames(matstand.all) <- colnames(breast_cancer[2:10])
t2testcan <- hotelling.test(radius_mean + texture_mean + perimeter_mean + area_mean + smoothness_mean + compactness_mean + concavity_mean + points_mean + symmetry_mean + dimension_mean ~ diagnosis, data=breast_cancer)
cat("T2 statistic =",t2testcan$stat[[1]],"\n")
print(t2testcan)

# In the above we standardized using scale function
head(matabsdev.all)

#install.packages("car")
library(car)
#leveneTest() produces a two-sided test
# Leverne test is used to verify Homoscedasticity. It tests if the variance of two samples are # #equal. Levene's test is an inferential statistic used to assess the equality of variances for a #variable calculated for two or more groups.[1] Some common statistical procedures assume that #variances of the populations from which different samples are drawn are equal. Levene's test #assesses this assumption.
leveneTest(radius_mean ~ diagnosis, data=breast_cancer)
leveneTest(texture_mean ~ diagnosis, data=breast_cancer)
leveneTest(perimeter_mean ~ diagnosis, data=breast_cancer)
leveneTest(area_mean ~ diagnosis, data=breast_cancer)
leveneTest(smoothness_mean ~ diagnosis, data=breast_cancer)
leveneTest(compactness_mean~ diagnosis, data=breast_cancer)
leveneTest(concavity_mean~ diagnosis, data=breast_cancer)
leveneTest(points_mean ~ diagnosis, data=breast_cancer)
leveneTest(symmetry_mean ~ diagnosis, data=breast_cancer)
leveneTest(dimension_mean ~ diagnosis, data=breast_cancer)
