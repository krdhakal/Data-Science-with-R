# get the working directory
getwd()
setwd('/Volumes/GoogleDrive/My Drive/ML_AI/Edureka/DS with R/Class 4')
getwd()
require(stats)
tem=read.csv('video_game.csv')
#tem <- na.omit(tem)  # Remove NA
library(stats)
library(dplyr)
library(ggplot2)
game <- tem %>% filter(Rating != "") %>% droplevels()  #remove empty rating observations
str(tem)
# By multiplying 1000000 we get the actual sale, 
# Adding 1 makes all sales positive which make log possible for all sales later
colnames(game) <- c("X","Name", "Platform", "Year.Release", "Genre", "Publisher", "NA.Sales", "EU.Sales", "JP.Sales", "Other.Sales", "Global.Sales", "Critic.Score", "Critic.Count", "User.Score", "User.Count", "Developer", "Rating")

game$Year.Release <- as.factor(as.character(game$Year.Release))
game$NA.Sales <-  as.numeric(game$NA.Sales)
game$NA.Sales <- game$NA.Sales *(1000000 +1)
game$EU.Sales <- game$EU.Sales * 1000000 + 1
game$JP.Sales <- game$JP.Sales * 1000000 + 1
game$Other.Sales <- game$Other.Sales * 1000000 + 1
game$Global.Sales <- game$Global.Sales * 1000000 + 1

# By being divided by 10 to make Critic Score the same decimal as User Score
game$Critic.Score <- as.numeric(as.character(game$Critic.Score)) / 10  
game$User.Score <- as.numeric(as.character(game$User.Score))
game$Critic.Count <- as.numeric(game$Critic.Count)
game$User.Count <- as.numeric(game$User.Count)
# lets see the sales 
NA.Sales.Log <- log(game$NA.Sales)   
EU.Sales.Log <- log(game$EU.Sales)  
JP.Sales.Log <- log(game$JP.Sales)   
Other.Sales.Log <- log(game$Other.Sales)  
Global.Sales.Log <- log(game$Global.Sales) 
Critic.Count.Log <- log(game$Critic.Count)
User.Count.Log <- log(game$User.Count)
#Then we combine the log variables with the original variables.
game.log <- cbind.data.frame(NA.Sales.Log, EU.Sales.Log, JP.Sales.Log, Other.Sales.Log,
                             Global.Sales.Log, Critic.Count.Log, User.Count.Log)
game <- cbind.data.frame(game, game.log)  # The 
#Now we plot histogram and QQ plot for the transformed data set.
name <- colnames(game)[c(11, 13, 17:23)] 
name <- as.factor(name)
# Pick up the numeric columns according to the names 
par(mfrow = c(5, 4))  # Layout in 5 rows and 4 columns
for (i in 1:length(name)){
  sub <- sample(game[name[i]][, 1], 5000)
  submean <- mean(sub)
  hist(sub, main = paste("Hist. of", name[i], sep = " "), xlab = name[i])
  abline(v = submean, col = "blue", lwd = 1)
  qqnorm(sub, main = paste("Q-Q Plot of", name[i], sep = " "))
  qqline(sub) 
  if (i == 1) {s.t <- shapiro.test(sub)
  } else {s.t <- rbind(s.t, shapiro.test(sub))
  }
}
s.t <- s.t[, 1:2]  # Take first two columns of shapiro.test result
s.t <- cbind(name, s.t) # Add variable name for the result
s.t
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Regroup platform as Platform.type 
pc <- c("PC")
xbox <- c("X360", "XB", "XOne")
nintendo <- c("Wii", "WiiU", "N64", "GC", "NES", "3DS", "DS") 
playstation <- c("PS", "PS2", "PS3", "PS4", "PSP", "PSV")
game <- game %>%
  mutate(Platform.type = ifelse(Platform %in% pc, "PC",
                                ifelse(Platform %in% xbox, "Xbox",
                                       ifelse(Platform %in% nintendo, "Nintendo", 
                                              ifelse(Platform %in% playstation, "Playstation", "Others"))))) 

library(ggplot2)
ggplot(game, aes(x = Platform.type)) + geom_bar(fill = "blue")

#As the bar plot shows here, Playstation is the biggest group, then Xbox and Nintendo. While “Others” is the smallest type.
dat <- data.frame(table(game$Genre))
dat$fraction <- dat$Freq / sum(dat$Freq)
dat <- dat[order(dat$fraction), ]
dat$ymax <- cumsum(dat$fraction)
dat$ymin <- c(0, head(dat$ymax, n = -1))
names(dat)[1] <- "Genre"
library(ggplot2)
ggplot(dat, aes(fill = Genre, ymax = ymax, ymin = ymin, xmax = 4, xmin = 3)) +
  geom_rect(colour = "grey30") +  # Background color
  coord_polar(theta = "y") +  # Coordinate system to polar
  xlim(c(0, 4)) +  
  labs(title = "Ring plot for Genre", fill = "Genre") +
  theme(plot.title = element_text(hjust = 0.5))
#Correlation plot
install.packages('ellipse')
library(ellipse)  
library(corrplot)
game[,c(13)]
st <- game[, c(12, 14, 18:24)]  # Take numeric variables as goal matrix
st <- as.data.frame(st)
st <- na.omit(st)
st <- st %>% filter(Rating != "") %>% droplevels()  #remove empty rating observations
library(ellipse)  
library(corrplot)
corMatrix <- cor(st,use="pairwise.complete.obs") # Correlation matrix
#corMatrix <- na.omit(corMatrix)
col <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "#7FFF7F",
                          "cyan", "#007FFF", "blue", "#00007F"))
corrplot.mixed(corMatrix, order = "AOE", lower = "number", lower.col = "black", 
               number.cex = .8, upper = "ellipse",  upper.col = col(10), 
               diag = "u", tl.pos = "lt", tl.col = "black")
#########################################################
install.packages(ggpmisc)
library(ggpmisc)  # Package for function stat_poly_eq
formula <- y ~ x
p1 <- ggplot(game, aes(x = User.Score, y = Critic.Score)) + 
  geom_point(aes(color = Platform), alpha = .8) + 
  geom_smooth(method = 'lm', se = FALSE, formula = formula) +  # Add regression line
  theme(legend.position = "none") +
  stat_poly_eq(formula = formula,   # Add regression equation and R square value
               eq.with.lhs = "italic(hat(y))~`=`~",  # Add ^ on y
               aes(label = paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~")),
               label.x.npc = "left", label.y.npc = 0.9,   # Position of the equation label
               parse = TRUE)  # Output.type as "expression" 
p2 <- ggplot() + 
  geom_density(data = game, aes(x = Critic.Score), color = "darkblue", fill = "lightblue") + 
  geom_density(data = game, aes(x = User.Score), color = "darkgreen", fill = "lightgreen",                                       alpha=.5) +
  labs(x = "Critic.Score-blue, User.Score-green") 

library(gridExtra)
grid.arrange(p1, p2, nrow = 1, ncol = 2)

p1 <- ggplot(game, aes(x = Critic.Score, y = Global.Sales.Log)) + 
  geom_point(aes(color = Genre)) + 
  geom_smooth()  # Regression line
p2 <- ggplot(game, aes(x = User.Score, y = Global.Sales.Log)) + 
  geom_point(aes(color = Rating)) + 
  geom_smooth()
grid.arrange(p1, p2, nrow = 1, ncol = 2)
#########################
game$Platform.type <- as.factor(game$Platform.type)
ggplot(game, aes(x = game$Platform.type, y = Global.Sales.Log, fill =Rating)) +
  geom_boxplot() 
ggplot(game, aes(Critic.Score, Global.Sales.Log, color = Platform.type)) + 
  geom_point() + 
  facet_wrap(~ Genre)
#Because there are too many levels in Publisher and Developer, and there is an apparent correlation between them, 
#we use only the top 12 levels of Publisher and classified the other publishers as “Others”;
#Because of the good correlation between Critic.Score and User.Score, we use only critic score;
#Also we use only log value of user score count because of its closer correlation to global sales log.
#We will not put other sales log variables in our model because of their apparent correlation with the global sales log."""
# Re-categorize publisher into 13 groups
Publisher. <- head(names(sort(table(game$Publisher), decreasing = TRUE)), 12)
game <- game %>%
  mutate(Publisher.type = ifelse(Publisher %in% Publisher., as.character(Publisher), "Others")) 
game.lm <- game[, c(3:4, 12, 21, 23:26)] # Take columns for linear model
model <- lm(Global.Sales.Log ~ ., data = game.lm)
summary(model)
model <- lm(Global.Sales.Log ~ I(Critic.Score^3) + I(Critic.Score^4),  data = game.lm)
summary(model)
model <- aov(Global.Sales.Log ~ ., data = game.lm)
summary(model)
ModelFunc <- function(x) {model$coefficients[1] + x^3*model$coefficients[2] +
    x^4*model$coefficients[3]}
ggplot(data = game.lm, aes(x = Critic.Score, y = Global.Sales.Log)) + 
  geom_point() + 
  stat_function(fun = ModelFunc, color = 'blue', size = 1)
        