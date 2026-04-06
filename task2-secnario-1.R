library(tidyverse)
library(readxl)
library(psych)
library(corrplot)
library(GGally)
concrete <- read_excel("Copy of concrete compressive strength.xlsx")
concrete <- read_excel("concrete compressive strength.xlsx")
str(concrete)
head(concrete)
colnames(concrete) <- c("Cement", "BlastFurnaceSlag", "FlyAsh", "Water",
"Superplasticizer", "CoarseAggregate", "FineAggregate",
"Age", "ConcreteCategory", "ContainsFlyAsh", "CompressiveStrength")
describe(concrete)
concrete %>%
pivot_longer(cols = where(is.numeric)) %>%
ggplot(aes(x = value)) +
geom_histogram(bins = 30, fill = "skyblue", color = "black") +
facet_wrap(~name, scales = "free") +
labs(title = "Distribution of Concrete Mix Variables") +
theme_minimal()
concrete %>%
pivot_longer(cols = where(is.numeric)) %>%
ggplot(aes(x = value)) +
geom_histogram(bins = 30, fill = "skyblue", color = "black") +
facet_wrap(~name, scales = "free") +
labs(title = "Distribution of Concrete Mix Variables") +
theme_minimal()
ggplot(concrete, aes(x = Cement, y = CompressiveStrength)) +
geom_point(alpha = 0.6, color = "darkorange") +
geom_smooth(method = "lm", color = "red") +
labs(title = "Cement vs Compressive Strength",
x = "Cement (kg/m³)", y = "Compressive Strength (MPa)") +
theme_minimal()
ggplot(concrete, aes(x = Age, y = CompressiveStrength)) +
geom_point(alpha = 0.6, color = "steelblue") +
geom_smooth(method = "lm", color = "red") +
labs(title = "Age vs Compressive Strength",
x = "Age (days)", y = "Compressive Strength (MPa)") +
theme_minimal()
corr_matrix <- cor(concrete %>% select_if(is.numeric))
corrplot(corr_matrix, method = "color", addCoef.col = "black",
tl.col = "black", number.cex = 0.7,
title = "Correlation Heatmap of Concrete Variables", mar = c(0,0,2,0))
numeric_vars <- concrete %>% select_if(is.numeric)
corr_matrix <- cor(numeric_vars, use = "complete.obs")
print(round(corr_matrix, 2))
library(corrplot)
corrplot(corr_matrix,
method = "color",
type = "upper",
addCoef.col = "black",
tl.col = "black",
tl.srt = 45,
col = colorRampPalette(c("blue", "white", "red"))(200),
title = "Correlation Heatmap of Concrete Variables",
mar = c(0,0,2,0))
GGally::ggpairs(numeric_vars, columns = 1:8,
title = "Pairwise Relationships among Concrete Variables")
colnames(concrete) <- c("Cement", "BlastFurnaceSlag", "FlyAsh", "Water",
"Superplasticizer", "CoarseAggregate", "FineAggregate",
"Age", "ConcreteCategory", "ContainsFlyAsh", "CompressiveStrength")
concrete$ConcreteCategory <- as.factor(concrete$ConcreteCategory)
concrete$ContainsFlyAsh <- as.logical(concrete$ContainsFlyAsh)
set.seed(123)
set.seed(123)
train_index <- createDataPartition(concrete$CompressiveStrength, p = 0.8, list = FALSE)
lm_model <- lm(CompressiveStrength ~ Cement + BlastFurnaceSlag + FlyAsh + Water +
Superplasticizer + CoarseAggregate + FineAggregate + Age,
data = train_data)
library(tidyverse)
library(caret)
library(car)
colnames(concrete) <- c("Cement", "BlastFurnaceSlag", "FlyAsh", "Water",
"Superplasticizer", "CoarseAggregate", "FineAggregate",
"Age", "ConcreteCategory", "ContainsFlyAsh", "CompressiveStrength")
library(car)
concrete$ConcreteCategory <- as.factor(concrete$ConcreteCategory)
concrete$ContainsFlyAsh <- as.logical(concrete$ContainsFlyAsh)
set.seed(123)
train_index <- createDataPartition(concrete$CompressiveStrength, p = 0.8, list = FALSE)
train_data <- concrete[train_index, ]
test_data <- concrete[-train_index, ]
lm_model <- lm(CompressiveStrength ~ Cement + BlastFurnaceSlag + FlyAsh + Water +
Superplasticizer + CoarseAggregate + FineAggregate + Age,
data = train_data)
summary(lm_model)
vif(lm_model)
Superplasticizer + CoarseAggregate + FineAggregate + Age,
predictions <- predict(lm_model, newdata = test_data)
rmse <- sqrt(mean((test_data$CompressiveStrength - predictions)^2))
r2 <- cor(test_data$CompressiveStrength, predictions)^2
cat("Root Mean Square Error (RMSE):", rmse, "\n")
cat("R-squared:", r2, "\n")
ggplot(data = NULL, aes(x = test_data$CompressiveStrength, y = predictions)) +
geom_point(color = "blue", alpha = 0.6) +
geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
labs(title = "Actual vs Predicted Compressive Strength",
x = "Actual Strength (MPa)", y = "Predicted Strength (MPa)") +
theme_minimal()
geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
poly_model <- lm(CompressiveStrength ~ poly(Cement, 2) + poly(Water, 2) + poly(Age, 2) +
Superplasticizer + CoarseAggregate + FineAggregate,
data = train_data)
summary(poly_model)
cor.test(concrete$Cement, concrete$CompressiveStrength, method = "pearson")
cor.test(concrete$Water, concrete$CompressiveStrength, method = "pearson")
t.test(CompressiveStrength ~ ContainsFlyAsh, data = concrete, var.equal = TRUE)
anova_model <- aov(CompressiveStrength ~ ConcreteCategory, data = concrete)
summary(anova_model)
TukeyHSD(anova_model)
age_model <- lm(CompressiveStrength ~ Age, data = concrete)
summary(age_model)
summary(concrete)
describe(concrete)
cor_matrix <- cor(concrete %>% select_if(is.numeric))
corrplot(corr_matrix, method = "color")
lm_model <- lm(CompressiveStrength ~ Cement + Water + Superplasticizer + Age +
BlastFurnaceSlag + FlyAsh + CoarseAggregate + FineAggregate, data = concrete)
summary(lm_model)
cor.test(concrete$Water, concrete$CompressiveStrength, method = "pearson")
library(ggplot2)
ggplot(concrete, aes(x = Water, y = CompressiveStrength)) +
BlastFurnaceSlag + FlyAsh + CoarseAggregate + FineAggregate, data = concrete)
geom_point(alpha = 0.6, color = "steelblue") +
geom_smooth(method = "lm", color = "red") +
labs(title = "Relationship Between Water Content and Compressive Strength",
x = "Water Content (kg/m³)",
y = "Compressive Strength (MPa)") +
theme_minimal()
shapiro.test(concrete$Water)
shapiro.test(concrete$CompressiveStrength)
plot(concrete$Water, concrete$CompressiveStrength,
main = "Scatter Plot: Water vs Compressive Strength",
xlab = "Water Content (kg/m³)", ylab = "Compressive Strength (MPa)",
plot(lm_model, which = 1)
plot(lm_model, which = 2)
shapiro.test(residuals(lm_model))
plot(lm_model, which = 3)
vif(lm_model)
durbinWatsonTest(lm_model)
cor_fine <- cor.test(concrete$FineAggregate, concrete$CompressiveStrength, method = "pearson")
cor_coarse <- cor.test(concrete$CoarseAggregate, concrete$CompressiveStrength, method = "pearson")
cor_flyash <- cor.test(concrete$FlyAsh, concrete$CompressiveStrength, method = "pearson")
cor_slag <- cor.test(concrete$BlastFurnaceSlag, concrete$CompressiveStrength, method = "pearson")
cor_super <- cor.test(concrete$Superplasticizer, concrete$CompressiveStrength, method = "pearson")
cor_super
cor_flyash
cor_fine
ggplot(concrete, aes(x = Superplasticizer, y = CompressiveStrength)) +
geom_point(alpha = 0.6, color = "royalblue") +
cor_flyash <- cor.test(concrete$FlyAsh, concrete$CompressiveStrength, method = "pearson")
geom_smooth(method = "lm", color = "red") +
labs(title = "Superplasticizer vs Compressive Strength",
x = "Superplasticizer (kg/m³)",
y = "Compressive Strength (MPa)") +
theme_minimal()
ggplot(concrete, aes(x = FlyAsh, y = CompressiveStrength)) +
geom_point(alpha = 0.6, color = "purple") +
geom_smooth(method = "lm", color = "red") +
labs(title = "Fly Ash vs Compressive Strength",
x = "Fly Ash (kg/m³)",
y = "Compressive Strength (MPa)") +
theme_minimal()
ggplot(concrete, aes(x = BlastFurnaceSlag, y = CompressiveStrength)) +
geom_point(alpha = 0.6, color = "darkgreen") +
geom_smooth(method = "lm", color = "red") +
labs(title = "Blast Furnace Slag vs Compressive Strength",
x = "Blast Furnace Slag (kg/m³)",
y = "Compressive Strength (MPa)") +
theme_minimal()
View(cor_fine)
View(train_data)
