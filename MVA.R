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


