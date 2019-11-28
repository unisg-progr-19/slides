#' # Random Forests
#'
#' [Intro Slides](09-random-forest.html)
#'
#' ## Slang
#'
#' Econometrics                                                 |ML
#' -------------------------------------------------------------|--------------------------
#' sample data, to estimate the model                           |training sample
#' estimating a model                                           |training a model
#' regression parameters                                        |weights
#' regressor, predictor, independent variable, RHS               |feature
#' estimating relationship between dependent var and regressors |supervised learning
#' clustering                                                   |unsupervised learning
#' discrete response problems                                   |classification problems
#'
#'
#' ## The Boston housing data set, again
#'
#' As in a previous exercise, we will work with Boston, which is part of the
#' [MASS](https://cran.r-project.org/web/packages/MASS/index.html) package. The
#' data set was used in an [1978
#' article](https://www.sciencedirect.com/science/article/abs/pii/0095069678900062)
#' on hedonic price estimation and the measurement for the demand for clean air.
#'
#' For an explanation of variable names, check out the documentation:
#'
# `?MASS::Boston`
#'
#' Again, we want to keep the data in a tibble, for convenience:

library(tidyverse)
boston <- as_tibble(MASS::Boston) %>%
  mutate(chas = as.logical(chas))

#' Most predictive modeling is subject to the danger of overfitting. We will
#' discuss the concept later on. This means that a model can perform well in
#' within the sample it was estimated, but terrible, if it is applied to new
#' data. A popular way to control the problem is to estimate (train) the model
#' on a part of the original data only (e.g. 70%), and use the rest to test the
#' model afterwards.

#' To separate the dataset, we first add an ID column:

boston_with_id <-
  boston %>%
  mutate(id = row_number())

#' The `sample_frac()` function allow as to pick a percentage of all rows from
#' the original dataset:

boston_train_with_id <- boston_with_id %>%
  sample_frac(0.7)

boston_train <- boston_train_with_id %>%
  select(-id)

#' This is the datset we will use for estimating our models. `anti_join()` can
#' be used to retrieve all rows that are not within `boston_train`, based on the
#' `id`:

boston_test <- boston_with_id %>%
  anti_join(boston_train_with_id, by = "id") %>%
  select(-id)

#' This is the datset we will use for evaluating our models.
#'
#' ## Prediction with OLS
#'
#' We can use the OLS model to perform predictions of the median house value.
#' The OLS curve covers the predicted values of the model. Let's recapitulate
#' and see, how the median value of owner-occupied homes, `medv`, can pe
#' predicted by the neighborhood, as measured by the percentage of lower status
#' population, `lstat`.

m_ols <- lm(medv ~ lstat, data = boston_train)
boston_test %>%
  mutate(predict = predict(m_ols, newdata = boston_test)) %>%
  ggplot(aes(x = lstat, y = medv)) +
  geom_point() +
  geom_point(mapping = aes(y = predict), shape = 21) +
  geom_segment(mapping = aes(xend = lstat, yend = predict))


#' ## Decision Trees
#'
#' A decision tree is tree-like structure of decisions and their possible
#' consequences. In a decision tree model, the prediction depends on particular
#' value of a variable. For example:

library(rpart)
m_tree_1 <- rpart(medv ~ lstat, data = boston_train, method = "anova", maxdepth = 1)
m_tree_1
plot(m_tree_1)
text(m_tree_1)

boston_test %>%
  mutate(predict = predict(m_tree_1, newdata = boston_test)) %>%
  ggplot(aes(x = lstat, y = medv)) +
  geom_point() +
  geom_point(mapping = aes(y = predict), shape = 21) +
  geom_segment(mapping = aes(xend = lstat, yend = predict), alpha = 0.2) +
  geom_vline(aes(xintercept = 9.95))

#' More interesting trees can be grown if we increase the *depth* of a tree.
#' Here, we let the tree grow to a depths of 2, which allows for four possible
#' prediction values:

m_tree_2 <- rpart(medv ~ lstat, data = boston_train, method = "anova", maxdepth = 2)
m_tree_2

plot(m_tree_2)
text(m_tree_2)

boston_test %>%
  mutate(predict = predict(m_tree_2, newdata = boston_test)) %>%
  ggplot(aes(x = lstat, y = medv)) +
  geom_point() +
  geom_point(mapping = aes(y = predict), shape = 21) +
  geom_segment(mapping = aes(xend = lstat, yend = predict), alpha = 0.2) +
  geom_vline(aes(xintercept = 4.295)) +
  geom_vline(aes(xintercept = 4.295)) +
  geom_vline(aes(xintercept = 15))

#' Analogous to multiple regression, we also can grow trees that depend on more
#' than variable. Here, our prediction depends both on the average number of
#' rooms per dwelling, `rm` and the percentage of lower status population
#' `lstat`.

m_tree_all <- rpart(medv ~ ., data = boston_train, method = "anova", maxdepth = 2)
m_tree_all

plot(m_tree_all)
text(m_tree_all)

boston_test %>%
  mutate(predict = predict(m_tree_all, newdata = boston_test)) %>%
  ggplot(aes(x = lstat, y = medv, color = rm)) +
  geom_point() +
  geom_point(mapping = aes(y = predict), shape = 21) +
  geom_segment(mapping = aes(xend = lstat, yend = predict), alpha = 0.2)

#' ## Random Forest
#'
#' Random forest takes random subsets of data and train trees on each subset.
#' That is the reason behind the name for this method: random (subsets) forest
#' (trees). When predicting new data from the test set each tree produces its
#' own prediction. Then the predictions from all the trees are combined together
#' (averaged). This is why random forest can approximate linear patterns, even
#' when the single trees, as we saw above, only produce mean values for
#' different ranges of values.
#'
#' The *randomForest* package offers basic random forest estimation in R:

library(randomForest)

#' Because random forests are based on random sampling, setting the *seed* is
#' needed to make the estimation reproducible:

set.seed(0)

#' Let's start with a simple example:

m_forest <- randomForest(medv ~ lstat, data = boston_train)
plot(m_forest)

boston_test %>%
  mutate(predict = predict(m_forest, newdata = boston_test)) %>%
  ggplot(aes(x = lstat, y = medv)) +
  geom_point() +
  geom_point(mapping = aes(y = predict), shape = 21) +
  geom_segment(mapping = aes(xend = lstat, yend = predict), alpha = 0.2)


#' We now have a bunch of possible forecasts. Which one should you use. The
#' *Root Mean Squared Error*, while not the only possible measure, is usually a
#' good starting point. It's main characteristics are the following:
#'
#' 1. Squared - so negative values become positive (deviation is deviation)
#' 2. Squared - so large deviations are extra-penalized
#' 3. Mean - influence of all errors are summarized with one number
#' 4. Root - to go back to original scale after squaring.

boston_test %>%
  summarize(
    rmse_forest = sqrt(mean((medv - predict(m_forest, newdata = boston_test))^2)),
    rmse_tree_1 = sqrt(mean((medv - predict(m_tree_1, newdata = boston_test))^2)),
    rmse_ols = sqrt(mean((medv - predict(m_ols, newdata = boston_test))^2))
  )

#' ### Using more than one variable

library(randomForest)
m_forest_all <- randomForest(medv ~ ., data = boston_train)

boston_test %>%
  mutate(predict = predict(m_forest_all, newdata = boston_test)) %>%
  ggplot(aes(x = lstat, y = medv)) +
  geom_point() +
  geom_point(mapping = aes(y = predict), shape = 21) +
  geom_segment(mapping = aes(xend = lstat, yend = predict), alpha = 0.2)

m_ols_all <- lm(medv ~ ., data = boston_train)
summary(m_ols_all)

boston_test %>%
  mutate(predict = predict(m_ols_all, newdata = boston_test)) %>%
  ggplot(aes(x = lstat, y = medv)) +
  geom_point() +
  geom_point(mapping = aes(y = predict), shape = 21) +
  geom_segment(mapping = aes(xend = lstat, yend = predict), alpha = 0.2)


boston_test %>%
  summarize(
    rmse_forest = sqrt(sum((medv - predict(m_forest, newdata = boston_test))^2)),
    rmse_tree_1 = sqrt(sum((medv - predict(m_tree_1, newdata = boston_test))^2)),
    rmse_ols = sqrt(sum((medv - predict(m_ols, newdata = boston_test))^2)),
    rmse_ols_all = sqrt(sum((medv - predict(m_ols_all, newdata = boston_test))^2)),
    rmse_forest_all = sqrt(sum((medv - predict(m_forest_all, newdata = boston_test))^2))
  )

#' If the interactions between features are mostly linear (as modeled by linear
#' regression) then random forest will not beat linear regression no matter
#' how much data it will have. However if there are interactions in the data the
#' linear model will require for you to list them in the formula (`y ~ x1 * x2`)
#' and random forest would be able to find them on its own.
#'
#' ### Variable importance
#'
#' So we have used OLS, decision trees and random forest to explain the values of
#' `medv`. But which independent variables are the most important? We have many
#' variables, and would like to rank them by importance. How can we do that?

#' For **OLS**, the importance can be obtained by the p-value. According to this,
#' `lstat` and `rm` are the most important variables:

broom::tidy(m_ols_all) %>%
  arrange(p.value)

#' For **single trees**, the values that split the data at the highest level
#' should be more important. Again, `lstat` and `rm` are the most important
#' variables:

m_tree_all

#' For **random forest**, there are several different measures of variable
#' importance. We will focus on one of them, the *decrease in accuracy*. It
#' measures the percentage of accuracy decrease if one variable is omitted.  To
#' calculate this measure of importance - we need to add `importance = TRUE` to
#' our model call:

m_forest_all <- randomForest(medv ~ ., data = boston_train, importance = TRUE)

#' `varImpPlot()` gives a convenient overview of variable importance, `type = 1`
#' selects the measure of importance, here, the mean decrease in accuracy after
#' permutation:

varImpPlot(m_forest_all, type = 1)

#' Again `lstat` and `rm` are the most important variables, but `rm` seems to be
#' substantially more important than `lstat`.
#'
#'
#' ### Exercises
#'
#' Back to the `CASchools` dataset on test performance, school characteristics
#' and student demographic backgrounds for school districts in California. As
#' before, we will enhance the dataset by defining two new variables,
#' `student_teacher_ratio`, the student-teacher ratio, and `test_score`, an
#' average of two underlying test scores:

library(AER)
data(CASchools)

caschools <-
  CASchools %>%
  as_tibble() %>%
  mutate(student_teacher_ratio = students / teachers) %>%
  mutate(test_score = (read + math) / 2) %>%
  select(-read, -math, -students, -teachers, -district, -school, -county, -grades)

caschools

#' 1. Separate the data set into a training and a test set. Make sure the
#' training set contains 75% of the available observations.
#'

#' 2. Build a decision tree to predict `student_teacher_ratio`, using all
#' variables in `caschools_train`. Use the defaults of `rpart`. Draw the
#' resulting decision tree. Store the model as `m_tree`.
#'

#' 3. From the documentation, `?rpart`, can you figure out how the depth of a
#' tree is determined? Which one is the most important variable?
#'

#' 4. Estimate an OLS model to predict `student_teacher_ratio`, using all
#' variables in `caschools_train`. Which one is the most important variable?
#' Store the model as `m_ols`.
#'

#' 5. grow a random forest to predict `student_teacher_ratio`, using all
#' variables in `caschools_train`. Use the defaults of `randomForest`. Store the
#' model as `m_forest`.
#'

#' 6. Plot the variable importance for `m_forest`. Which one is the most
#' imporant?
#'

#' 7. Using the test data, can you compute RMSE measures for `m_ols`, `m_tree`,
#' and `m_forest`? Which performs best?
#'

#' ## Classification
#'
#' This data set provides information on the fate of passengers on the fatal
#' maiden voyage of the ocean liner Titanic, summarized according to economic
#' status (class), sex, age and survival. Titanic dataset is classic example in
#' machine learning, and can be found in the *titanic* package.

# install.packages("titanic")  # make sure it is installed.
head(titanic::titanic_train)

#' The goal of the exercise it to predict survival. The datasets come divided
#' into training and testing set. However, the testing part does not have
#' survival information and is not useful that way. In the following, we will
#' limit ourself to the `titanic::titanic_train`.
#'
#' Categorical variables will be a problem in random forest, so we will convert
#' them into factors. We also clean up the data and remove missing values. The
#' cleaned dataset looks as follows:

titanic <-
  as_tibble(na.omit(titanic::titanic_train)) %>%
  select(-Name, -PassengerId, -Cabin, -Ticket, -Embarked) %>%
  mutate(Sex = as_factor(Sex)) %>%
  mutate(Survived = as_factor(Survived))


#' `Survived`
#' : Passenger Survival Indicator
#'
#' `Pclass`
#' : Passenger Class
#'
#' `Sex`
#' : Sex
#'
#' `Age`
#' : Age
#'
#' `SibSp`
#' : Number of Siblings/Spouses Aboard
#'
#' `Parch`
#' : Number of Parents/Children Aboard
#'
#'
#' Again, the usual separation in test and training data set.

set.seed(0)
titanic_with_id <-
  titanic %>%
  mutate(id = row_number())

titanic_train_with_id <-
  titanic_with_id %>%
  sample_frac(0.70)

titanic_train <-
  titanic_train_with_id %>%
  select(-id)

titanic_test <-
  titanic_with_id %>%
  anti_join(titanic_train_with_id, by = "id") %>%
  select(-id)


#' To start, let's build a decision tree:

m_titanic_tree <- rpart(Survived ~ ., data = titanic_train)
plot(m_titanic_tree)
text(m_titanic_tree, all = TRUE)

m_titanic_tree

#' This has a very simple interpretation: If you are on the Titanic, the best
#' guess is that you will die. If you are male, things look bad, unless you are
#' a child. If you are female, things look better, especially if you travel
#' first class. So it is not very hard to guess the end of the movie *Titanic*.
#'
#' Switching to random forest, the output now includes the a *confusion matrix*.
#' The confusion matrix shows, based on out-of-bag evaluation, which
#' classification has been done correctly and which has not. 270 deaths has been
#' predicted correctly, 151 survivals have been predicted correctly. On the
#' other hand, 27 dying passengers have been incorrectly predicted to survive,
#' while 52 surviving passengers have been incorrectly predicted to to die.

set.seed(0)
m_titanic_forest <- randomForest(Survived ~ ., data = titanic_train, importance = TRUE)
m_titanic_forest

#' The out-of-bag forecast error are usually a good way to quickly assess the
#' forecast accuracy of a random forest model. However, if we want to run the
#' model on our test data, compute the confusion table as follows:

tibble(
  predicted = predict(m_titanic_forest, newdata = titanic_test),
  actual = titanic_test$Survived
) %>%
  group_by(predicted, actual) %>%
  summarize(count = n())

#' The corresponding out-of-bag confusion table as is as follows:
tibble(
  predicted = predict(m_titanic_forest),
  actual = titanic_train$Survived
) %>%
  group_by(predicted, actual) %>%
  summarize(count = n())

#' Finally, which variables are the important one?

varImpPlot(m_titanic_forest, type = 1)
