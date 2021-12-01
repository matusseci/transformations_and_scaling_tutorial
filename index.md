---
theme: jekyll-theme-cayman
title: Data distributions, transformations and scaling
subtitle: Understand fundamental concepts of understanding and preparing data for modelling
date: 28/11/2021
author: Matus Seci
---

# Tutorial aims:

1. Be able to describe common types of distributions found in ecological and environmental data
2. Understand the purpose of transformations and scaling in statistical analysis and use appropriate syntax in R to apply both common and more advanced transformations and scaling
3. Learn how to reverse transformations and scaling to obtain estimates in the original units of measure
4. Learn how to apply these concepts to real problems involving data through a worked examples


# Steps:
1. [Introduction](#intro)
2. [**Part I: Data Distributions**](#base)
  - [Terminology](#Terminology)
  - [Gaussian (Normal) Distribution](#Gaussian)
  - [Binomial Distribution](#Binomial)
  - [Other types](#other)
3. [**Part II: Transformations**](#Transformations)
  - [`log` transformation](#log)
  - [Square root `sqrt` transformation](#sqrt)
  - [Box-Cox transformation using `boxcox()`](#bc)
  - [Linear modelling using transformed data](#trans_lin)
4. [**Part III: Scaling**](#Scaling)
  - [Standardization](#Standardization)
  - [Normalization](#Normalization)
  - [Using `caret` package for scaling and transforming data](#caret)
5. [Scaling for Data Visualization](#datavis_scaling)
4. [Summary](#Summary)
5. [Challenge](#Challenge)

# 1. Introduction
{: #intro}

Data come in a wide variety of shapes and sizes. We use data distributions to study and understand the data. Many models are build around assumptions that the data follows a certain distribution, most typically linear models always assume **normal** distribution of the data.
However, real world data rarely follow perfect normal distribution and therefore break this assumption. For dealing with these issues we can use **transformations** and **scaling** to momentarily alter the data in order to satisfy these assumptions. They are therefore powerful tools for allowing us to utilize a wide variety of data that would not be available for modelling otherwise.

This tutorial will show you how to work with data distributions and what types of distributions you are likely to encounter when exploring ecological and environmental data. Then, we will explore common transformations and scaling procedures you might use in order to modify distributions of the data through real world examples.

## Prerequisites
This tutorial is suitable for novices and intermediate learners in statistical analysis and depending on your level you should pick and choose which parts of tutorial are useful for you, for example a beginner might learn basic transformations such as logarithmic and square-root transformations while intermediate learner will extend these concepts by learning about Box-Cox transformation. However, to get most out of this tutorial you should have a basic knowledge of descriptive statistics and linear models. Knowledge of high school algebra (functions, equation manipulation) will enhance the understanding of the concepts.  

While we will use programming language `R` throughout the tutorial the concepts you will learn here are applicable in other programming languages as well! To fully appreciate the code in this tutorial you should have at least a basic knowledge of data manipulation using `dplyr`, `tidyr` and visualising data using `ggplot2`. If you are new to R or need to refresh your memory there are great resources available on the Coding Club website:
- [Intro to R](https://ourcodingclub.github.io/tutorials/intro-to-r/)   
- [Basic Data Manipulation](https://ourcodingclub.github.io/tutorials/data-manip-intro/)
- [Data Visualization](https://ourcodingclub.github.io/tutorials/datavis/)


Now we are ready to dive into the world of data distributions, transformations and scaling!

# I. Data Distributions

## Terminology
A data distribution shows all the possible values or intervals of the data. It helps us to determine which values are the most common in the dataset and how spread out the values are.

### Visualizing data distributions
We can use two types of plots to visualise data distributions (1) histograms and (2) density plots (also called kernel density functions). Below is a code showing how to plot each of these using an artificial dataset in R.

Make sure that you include the header at the beginning of your script with the description of the script, your name and contact details and the dates you worked on the script. If you would like to found out more about a proper coding etiquette you can have a look at a tutorial [here](https://ourcodingclub.github.io/tutorials/etiquette/).


As the `sample()` functions contains a random element (the numbers sampled are different each time we use it based on chance) we use `set.seed()`. This functions ensures that each time `set.seed()` with the same number (e.g. `set.seed(1)` but the number is arbitrary, you can use any number as long as you do not change it, 1 is my favourite number) is run before the sample generation the result will be the same.

```r
# Coding Club Tutorial - Data distributions, transformations and scaling
# Matus Seci, matusseci@gmail.com
# 29/11/2021

# I. Data distributions ----

# Visualizing data dsitributions

# Set seed
set.seed(1)

# Generate a random sample
rand_sample <- sample(x = seq(1, 25, 0.25), size = 100, replace = TRUE)

# (1) Plot a histogram
hist(rand_sample,
     breaks = 10,
     xlab = 'Values',
     ylab = 'Frequency',
     main = 'Data distribution of a random sample')

# (2) Plot a density function
plot(density(rand_sample),
     xlab = 'Values',
     ylab = 'Frequency',
     main = 'Data distribution of a random sample')
```

As we can see a histogram uses **bins** to group values together and provides a coarser description of the data distribution. On the other hand, a density plots smooths out the transitions between values and produces a continuous **curve**. This can be advantageous when we want a very precise distribution of continuous data, however, if our dataset contains few values the density curve can look slightly biased. Therefore, you should be careful when using density plots.

As an alternative to the base R code we can use `ggplot2` functions `geom_histogram()` and `geom_density()` to visualize data distributions.

```r
library(ggplot2)



```

### Skewness
Skewness of data distributions describes a situation when a data distribution has its mean value different from its median.

![alt text]('https://github.com/EdDataScienceEES/tutorial-matusseci/blob/master/figures/skewness.png')

Positive skew is also called **right-skewed** and negative skew is also called **left-skewed**. This terminology is a source of endless confusion as for some people it is rather counterintuitive.

With skewed data we can sometimes talk about **zero-inflation**. This term refers to the fact that our dataset contains an unusually high number of zero values. There are several ways to deal with this issue, for example using transformations as we will see later.


### Kurtosis
Kurtosis describes the 'tailedness' of the data distribution.

![alt text]('https://github.com/EdDataScienceEES/tutorial-matusseci/blob/master/figures/kurtosis.jpeg')

A **mesokurtic** distribution is a distribution which has data spread in the same way as a **normal** distribution. A **leptokurtic** distribution has 'fatter tails' meaning that the extreme values occur more frequently than in a normal distribution. A **platykurtic** distribution has 'thinner tails' and therefore produces fewer extreme values. It is very important to note that the extreme values mentioned above should be considered in reference to the rest of the distribution and not other distributions.

# II. Data Transformations

In the following parts we will work with the data from the Living Planet Index which is an open-source database containing population data of a large number of species from all around the globe. Let's load it into our script along with the libraries we will use in this part of the tutorial. If you do not have some of these packages installed, use `install.packages('package_name')` to install them now and then load them.

```r
library(tidyverse)  # contains dplyr (data manipulation) and other useful packages
library(cowplot)  # making effective plot grids
library(MASS)  # contains boxcox() function
library(ggeffects)  # model predictions

# Import Data
load('data/LPI_species.Rdata')
```
Now we can look at the basic structure of the dataframe to get some idea of the different variables it contains.

```r
str(LPI_species)
summary(LPI_species)
```
We can see that the dataset contains information about 31 species. In this part we will look at the population data of the white stork (__Ciconia ciconia__) sampled using the **direct counts** method. We use `dplyr` function `filter()` to extract these data and adjust the year variable to be a numeric variable using `mutate()` and `parse_number()`.

```r
# Extract the white stork data from the main dataset
stork <- LPI_species %>%
  filter(Common.Name == 'White stork' & Sampling.method == 'Direct counts')%>%
  mutate(year = parse_number(as.character(year))  # convert the year column to character and then parse the numeric part
```

In particular we will attempt to answer the following research question: Has the population of the white stork decreased over time?

Let's look at the distribution of the data to get some feeling of what the data look like and what model we could use to answer this question.

```r
# Look at the distribution of the data
(stork_hist <- ggplot(data = stork) +
    geom_histogram(aes(x = pop),
                   alpha = 0.9,
                   fill = 'blue') +
    labs(x = 'Value',
         y = 'Density') +
    plot_theme() +
    theme(legend.position = 'none'))
```
<center> <img src="{{ site.baseurl }}/stork_hist.png" alt="Img" style="width: 800px;"/> </center>!

We can see that our data are very skewed with most of the values occuring being small. This data distribution is far from normal and therefore we cannot use them directly if we want to use a linear model as it assumes normal distribution of data. This is where transformations come in!

**NOTE** Observant learners will notice that we are dealing here with **count data** and therefore we could model this dataset using **generalized linear model** with **Poisson distribution**. This would be a perfectly correct approach, however, for the sake if this tutorial we will stick with a linear model to demonstrate how we can use transformations to model even non-normally distributed data.

## Logarithmic transformation

As we can see our data are right-skewed. We can also plot a simple scatter plot to see that a distribution like this would not be very well described by a straight line but rather an **exponential function**.

```r
# Plot a scatter plot of the data
(stork_scatter <- ggplot(data = stork) +
    geom_point(aes(x = year, y = pop),  # change to geom_point() for scatter plot
                   alpha = 0.9,
                   color = '#18a1db') +
    labs(x = 'Year',
         y = 'Population Abundance',
         title = 'Population abundance of white stork') +
    plot_theme())  # apply the custom theme
```
<center> <img src="{{ site.baseurl }}/stork_scatter.png" alt="Img" style="width: 800px;"/> </center>!

This means that we need to apply a **logarithmic transformation** which will **linearize** the data and we will be able to fit linear model. Luckily, this procedure is very simple in R using a base R function `log()`. Together with `mutate()` function we can create a new column with the transformed data so that we do not overwrite the original data in case we want to use them later.

```r
# Log transform the data
stork <- stork %>%
  mutate(logpop = log(pop))

# Plot a scatter plot of the log transformed data
(stork_scatter <- ggplot(data = stork) +
    geom_point(aes(x = year, y = logpop),  # change pop -> logpop
                   alpha = 0.9,
                   color = '#18a1db') +
    labs(x = 'Year',
         y = 'Population Abundance',
         title = 'Population abundance of white stork') +
    plot_theme())  # apply the custom theme
```   

<center> <img src="{{ site.baseurl }}/stork_scatter_log.png" alt="Img" style="width: 800px;"/> </center>!

We can see that the data have been constrained to a much narrower range (y-axis) and while there is not a crystal clear linear pattern we could argue that a linear line would fit the best for this scatter plot. Let's have a look at how the data distribution changed by looking at a histogram.

```r
# Plot the histogram of log transformed data
(stork_log_hist <- ggplot(data = stork) +
    geom_histogram(aes(x = logpop),
                   alpha = 0.9,
                   fill = '#18a1db') +
    labs(x = 'Value',
         y = 'Density') +
    plot_theme())
```

<center> <img src="{{ site.baseurl }}/stork_log_hist.png" alt="Img" style="width: 800px;"/> </center>!

Now this look much closer to the normal distribution than the previous histogram! You can see the transformation in practice in the animation below.

**INSERT ANIMATION**

**NOTE** Log transformations are often used to transform right-skewed data, however, the transformation has a major shortcoming which is that it only works for **positive non-zero** data. This is due to the mathematical properties of the logarithmic function. If you find out that your data have a 0 values but you would still like to use log transformation you can use **add a constant** to the variable before performing the transformation, for example log(x + 1) where x is the variable. This way you can get rid of the negative or zero values. However, you should still use this method with caution as adding a constant changes the properties of the logartihm a bit.

Our data look quite normally distributed now but we might think that a **weaker** transformation could result in a data more centered than what we have now.

## Square-root transformation
**Square root transformation** looks works in a very similar way as logarithmic transformation and is used in similar situations (right-skewed data), however, it is a **weaker** transformation. What do we mean by weaker? Well, to answer this question it is a good idea to look at the graphs of these mathematical functions.

<center> <img src="{{ site.baseurl }}/log_sqrt_func.png" alt="Img" style="width: 800px;"/> </center>!

As you can see the logarithmic function levels off much more quickly which means that it squishes large values much more strongly than square-root. As a result, with log transformation extreme values in the dataset will not have as strong an effect. The plots also indicate that square-root transformation has the same disadvantage as log transformation - it can only be used on positive non-zero data.

Similar to the log transformation, we can use `sqrt()` function in base R to make this transformation.
```r
# Create a square-root transformed column
stork <- stork %>%
  mutate(sqrtpop = sqrt(pop))

# Plot the histogram of square root transformed data
(stork_hist_sqrt <- ggplot(data = stork) +
    geom_histogram(aes(x = sqrtpop),  # change pop -> sqrtpop
                   alpha = 0.9,
                   fill = '#18a1db') +
    labs(x = 'Value',
         y = 'Density') +
    plot_theme())
```   

<center> <img src="{{ site.baseurl }}/stork_hist_sqrt.png" alt="Img" style="width: 800px;"/> </center>!

This does not look bad but the data are still quite skewed. This probably means that out of the three options we have seen the most normal looking distribution would be achieved with the log transformation. While it would be completely alright to use log transformed data we will extend this a bit more and introduce a more advanced concept called **Box-Cox transformation**.

### Box-Cox Transformation
Box-Cox transformation is a stiatistical procedure developed by Geoerge Box and Sir David Roxbee Cox for transforming non-normally distributed data into a normal distribution. The transformation is not as straightforward as logarithmic or square-root transformations and requires a bit more explanation. We will start by giving out an equation that describes the transformation (do not worry if you do not understand the equation, it is not essential).

**INSERT equation**

We can see that the transformation is determined by a parameter **lambda** and **if lambda is 0 the transformation is simply log transformation**. Essentially, Box-Cox transformation uses the above equation and different lambda values to test different strengths of transformations and determines at which lambda value the distribution is most normal. It is therefore much more **precise** then just comparing sqrt and log transformations - it tries out many more options! The animation below demonstrates how the different lambda values change the results of transformation.

<center> <img src="{{ site.baseurl }}/animated_boxcox.gif" alt="Img" style="width: 800px;"/> </center>!

Now let's try to use Box-Cox transformation on our data. To do this we can use `boxcox()` function from `MASS` package which we have loaded earlier. `boxcox` function takes as an argument either a **model object** or a **model formula** so we will start with building a simple linear model from the original data using `lm()` function looking at how the abundance changed over time (year ~ pop). With default values it tests values for lambda in the range (-2, 2) with 0.1 steps so quite a few lambda values!

```r
# Build a model
stork.mod <- lm(pop ~ year, data = stork)

# Find the optimal lambda for Box-Cox
bc <- boxcox(stork.mod)
```
After you run the `boxcox` command a plot like the one below should show up in your plot console.

**insert boxcoxplot**

The plot shows the optimal value of the lambda parameter. We can see that for our data it is somewhere around 0.1. To extract the exact optimal value we can use the code below.

```r
# Extract the optimal lambda value
(lambda <- bc$x[which.max(bc$y)])
```
Now that we have the exact value, we can use it to transform our data by applying the formula from above and the lambda value.

```r
# Transform the data using this lambda value
stork <- stork %>%
  mutate(bcpop = ((pop^lambda-1)/lambda))

# Plot a histogram of the Box-Cox transformed data
(stork_hist_bc <- ggplot(data = stork) +
    geom_histogram(aes(x = bcpop),
                   alpha = 0.9,
                   fill = 'blue') +
    labs(x = 'Value',
         y = 'Density') +
    plot_theme())
```

<center> <img src="{{ site.baseurl }}/stork_hist_bc.png" alt="Img" style="width: 800px;"/> </center>!

We can see that the distribution is very similar to the one we got using the log transformation. This is not surprising since the lambda value we used was approximately 0.1 and lambda = 0 would result in log transformation. You can probably now see that in our situation using the log transformation would be a pretty good approximation to the Box-Cox optimal result.

Before proceeding to model the data we can visually appreciate the differences between the transformations we have learned and applied so far by plotting them in a panel together using `cowplot` package and `plot_grid()` function.

```r
# Panel of histograms for different transformations
(stork_dist <- plot_grid(stork_hist + labs(title = 'Original data'),  # original data  
                        stork_log_hist + labs(title = 'Log transformation'),  # logarithmic transformation
                        stork_hist_sqrt + labs(title = 'Square-root transformation'),  # square-root transformation
                        stork_hist_bc + labs(title = 'Box-Cox transformation'),  # Box-Cox transformation
                        nrow = 2,  # number of row in the panel
                        ncol = 2))  # number of columns in the panel
```
## Building models using transformed data and backtransformations

We will now continue to build a model using the transformed data and answer our research question. We will use the Box-Cox transformed data but feel free to use the log transformed data if you want to keep things more simple!

```r
# Fit new model using the Box-Cox transformed data
stork.bc.mod <- lm(bcpop ~ year, data = stork)

# Show the summary of the model outputs
summary(stork.bc.mod)
```
**insert the summary screenshot here**

We can see that the our results are highly significant with the effect size of 0.04 and standard error of 0.006. But what exactly does this mean? Since we have transformed our data we are getting the estimate (effect size) and standard error on the transformed scale, not on the original scale! This might be quite confusing when we present our results. We will therefore **back-transform** our data into the original scale but before that let's have a quick look at the model assumption of normality to see how well our transformed data did compared with the model that would use the original data. We will use so called Q-Q plots for this. If the data are normally distributed, the points in the Q-Q plot should lie on the line.

```r
# Tell R to display two plots next to each other
par(mfrow = c(1, 2))

# Q-Q plot for the original data model
qqnorm(stork.mod$residuals)
qqline(stork.mod$residuals)

# Q-Q plot for the Box-Cox transformed data model
qqnorm(stork.bc.mod$residuals)
qqline(stork.bc.mod$residuals)

# Reset the plot display settings
par(mfrow = c(1, 1))
```

**insert the qqplots**

We can see that while the transformed data are not perfectly aligned with the line, they deviate much less than the original data. We can therefore conclude that the transformed data are a good fit for the normality assumption. We can move to the backtransformations now.

Reversing transformations is essentially applying a function to the transformed data which is the inverse of the operation that was used to do the transformation. The reverse transformations for the procedures we used in this tutorial are listed in the table below together with their functions in R (given they exist).

**insert table here**

We can verify whether these reverse transformations work by simply applying them on the columns we created earlier and then comparing them with the original `pop` column.

```r
# Verify reverse transformations by creating new columns that should match the original
stork <- stork %>%
  mutate(back_log = exp(logpop),
         back_sqrt = sqrtpop^2,
         back_bc = (bcpop*lambda + 1)^(1/lambda)) %>%
  glimpse()  # displays a couple of observations from each column
```
We can see that the values in these columns and the `pop` column match which is great and we can use these transformations to obtain predictions of our results on a relevant scale.

We will use `ggpredict()` function from `ggeffects` package to get predictions and then convert them into a relevant scale by applying a relevant reverse transformation.

```r
# Get the predictions of our model
stork.pred <- ggpredict(stork.bc.mod, terms = c('year'))

# Apply the reverse transformation on the relevant columns
stork.pred$predicted <- (stork.pred$predicted*lambda + 1)^(1/lambda)
stork.pred$std.error <- (stork.pred$std.error*lambda + 1)^(1/lambda)

# slope
(0.040557*lambda + 1)^(1/lambda)

# std. error
(0.006177*lambda + 1)^(1/lambda)

# Plot the predictions
(stork_plot <- ggplot(stork.pred) +
   # plot the prediction line with ribbon in red
   geom_line(aes(x = x, y = predicted), color = '#db1818') +          
   geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error),
               fill = "#fc7777", alpha = 0.5) +  
   # plot the original data
   geom_point(data = stork,                      
              aes(y = pop, x = year)) +
   # add the slope and standard error
   annotate("text", x = 1975, y = 180, label = "Slope = 1.04\n Std. error = 1.01") +
   labs(x = '',
        y = 'Population Abundance',
        title = "Global Stork population increased between 1970-2012",
        caption = 'Data Source: Living Planet Index') +
   plot_theme() +
   xlim(c(1970, 2008))  # limit the x-axis to the year that we have in our dataset
)
```

<center> <img src="{{ site.baseurl }}/stork_plot.png" alt="Img" style="width: 800px;"/> </center>!

We can see that the prediction line is not straight but is more of a curve which reflects the fact that we have used transformed data. We have also corrected the slope and standard deviation from the model to correct values.

This is the end of the first part of the tutorial. You should now be comfortable using transformations to convert non-normal data into a normal shape, use them in a model and then reverse the transformation to present results in the original units. Next we will look at a different type of data manipulation - scaling.

# II. Scaling

Scaling describes a set of procedures used to adjust the distribution of the data, particularly its **range** through **linear transformation**. Linear transformation in this context means that it uses only basic arithmetic operations (addition, subtraction, multiplication, division) and not exponentiating or logarithms. You might now ask the question, in what situations we would not use transformations like log and sqrt but use scaling? Imagine that you have a dataset of species abundance measurements where some data were obtained by counts (units = individuals) and others using a population index (no units). The former might be in a range of 1000s while the other will have value from 0 - 1! Is it possible to directly compare the two? Of course not. This is where scaling comes in. It allows us to put two variables on **the same scale** and thus make them **comparable**.  In this tutorial we will cover the two most common types of scaling: **standardization** and **normalization**.   

<center> <img src="{{ site.baseurl }}/scaling_demo.png" alt="Img" style="width: 800px;"/> </center>!

## Standardization
As in the case of transformation we will work with a dataset from Living Planet Index. This time we will use population data on the atlantic salmon (__Salmo salar__) but unlike in the previous case we will keep all the observations. We will answer a similar question to the one in the previous example: Did the population of the atlantic salmon decrease over time? Let's extract the data from the main dataset and look at the `units` variable to see what units the population measurements are in.

```r
# Extract the Atlantic salmon data from the main dataset
salmon <- LPI_species %>%
  filter(Common.Name == 'Atlantic salmon') %>%
  mutate(year = parse_number(as.character(year)))

# Look at the units in the dataset
unique(salmon$Units)
```
That's a lot of different units! We deinitely cannot compare units like `Number of smolt` and `Individual counts`. Furthermore, our dataset contains populaton data from multiple studies and locations which will probably have different average populations and trends so the range of data will be different. Therefore, we need to scale the data in some way to be able to use it for answering our question but before that let's have a look at the distributions of the individual studies. To do this we will use `ggplot2` function `facet_wrap()` which allows us to create plots of the each population measured with one line instead of creating each plot separately. Our dataset a variable `id` which contains a unique identifier for each of the studies and we can use for plotting the distributions. Sometimes the plot viewer in RStudio can have trouble displaying large plots. A good workaround for this issue is simply saving the plot on your computer and viewing it then.

```r
# Look at the distribution of the data for each of the populations
(salmon_density_loc <- salmon %>%                            
    ggplot(aes(x = pop)) +
      geom_density() +  # we use geom_density() instead of geom_histogram in this case but they are interchangeable
      facet_wrap(~ id, scale = 'free') +  # create the grid based on the id, scale = 'free' allows different x and y scale for each population
      labs(y = 'Density',
           x = '\nValue\n',
           title = 'Distributions of individual Atlantic salmon populations\n',
           caption = 'Data Source: Living Planet Index') +
      plot_theme() +
      theme(axis.text.x = element_blank(),  # we remove the axis text to make the plots less clutered
            axis.text.y = element_blank()))

# Save the plot
ggsave(plot = salmon_density_loc,
       filename = 'figures/salmon_hist_loc.png',
       width = 10, height = 12, units = 'in')
```

<center> <img src="{{ site.baseurl }}/salmon_hist_loc.png" alt="Img" style="width: 800px;"/> </center>

We can see that the individual populations have different distributions but many of them are close to normal distribution on their own scale which is good. This means that we can use **standardization** to scale the data.

**Standardization** is a scaling procedure during which we **subtract the mean from the original data and divide them by standard deviation**. It is especially useful for data which are already normally distributed, in fact, the name of the procedure derives from a term **standard normal**. Normal distribution is defined by its **mean** and **standard deviation** which means that given these two parameters you can draw the exact curve describing the distribution (this fact alone makes it so popular, it is really easy to measure these two properties). **Standard normal** refers to a normal distribution with mean = 0 and standard deviation = 1. So when we apply the procedure to our data we should get a roughly normal distribution centered at 0 with standard looking tails.

You might ask why this procedure would not work for other distributions. Well, the main issue here is that other distributions such as Poisson, binomial or exponential are not well described by their mean and standard deviation. This is stemming from the **assymetry** of these distributions. Look at the animation below to see what would happen if we applied standardization to binomial data. As you can see we do not achieve the desired result which would be center on 0 and standard deviation of 1.


**insert animation**

Let's therefore move on to apply standardization to our data. We will use a combination of `group_by()` and `mutate()` to standardize data from each of the studies individually.

```r
# Standardize the data
salmon <- salmon %>%
  group_by(id) %>%  # group the data by the study id
  mutate(scalepop_standard = (pop-mean(pop))/(sd(pop))) %>%  # apply standardization
  ungroup()  # ungroup the data to avoid issue with grouping later on
```
Now let's have a look at how our data overall data distribution has changed by plotting histogram of the original data and the standardized data.

```r
# Histogram of the original, unscaled data
salmon_hist <- ggplot(data = salmon) +
    geom_histogram(aes(x = pop),
                   alpha = 0.9,
                   fill = '#319450') +
    labs(x = 'Value',
         y = 'Density') +
    plot_theme()

# Look at the distribution of the scaled data
salmon_hist_scaled <- ggplot(data = salmon) +
    geom_histogram(aes(x = scalepop_standard),
                       alpha = 0.9,
                       fill = '#319450') +
    labs(x = 'Value',
         y = 'Density') +
    plot_theme()

# Panel of the histograms
(salmon_dist <- plot_grid(salmon_hist + labs(title = 'Original data'),  # original data  
                          salmon_hist_scaled + labs(title = 'Standardized data'),  # standardized data
                          nrow = 1,  # number of row in the panel
                          ncol = 2))  # number of columns in the panel
```
<center> <img src="{{ site.baseurl }}/salmon_dist_panel.png" alt="Img" style="width: 800px;"/> </center>

This is a huge difference! We can clearly see that our data are now centered on 0 and the distribution look very close to normal, even if slightly skewed but compared to the original data this is an incredible improvement.
We would now proceed with modelling the data using the transformed variable but since the procedure would be essentially the same as in the transformation example above we will not fully repeat the explanation process here.

The only difference would be reverse transforming the data to show in the final predictions plot. Essentially, the procedure is the same as for other transformations - apply reverse mathematical operations. Since for standardization we **subtracted the mean and divided by standard deviation of the original data**  to reverse the transforamtion we need to **multiply by the standard deviation and add the mean of the original data**. Another thing to pay attention to is that we used this procedure on each individual study separately and thus the reversing has to do the same. This is demonstrated in the code below.

```r
# Reverse transformation test of the salmon data
salmon <- salmon %>%
  group_by(id) %>%  # we group by id again
  mutate(pop_scaled_rev = (scalepop_standard * sd(pop) + mean(pop))) %>%  # apply the reverse transformation
  ungroup() %>%
  glimpse()  # look at the result
```

The data in the `pop_scaled_rev` column should match the data in the `pop` column which they do and so we applied the reverse transformation correctly.

There is one imporatant issue to consider when working with scaled data but presenting the unscaled version. We can notice in our histograms above that in the original data we have most of the data with a very small value and then some outliers which have very large values. This can create a major issue when presenting the data, in particular it will make y-axis scale very high and squish all the small value data points on the x-axis. While this technically correct the visualization then would not correctly convey the message which is the trend that we have detected. In situations like this it is safet to simply present the scaled data instead of reversing the scaling and **explain in the text of your report the reason why you did this**. It might be more difficult to understand the meaning of the effect size/slope since it will not have any meaningful units but the prediction plot will be much more clear and interpretable.  

# Normalization

**Normalization** is another scaling procedure but unlike **standaridzation** it can be used for any distribution. In fact, it is important to understand that the goals of these two procedures are different. Standardization aims to convert any normal distribution into a standard normal but the goal of normalization is to **rescale the data into a set range of values**. It is defined as **subtracting minimum value and dividing by the range of the original variable**. Using this procedure on a set of data which contains only **non-negative values** will result in a **range of [0, 1]** and if there are **negative values** the range will be **[-1, 1]**. The most imporatnt property of this scaling procedure is that it **does not change the relative distances between individual data points and so should not significantly alter the data distribution**.

Now you might ask why you would want to scale data this way if it only changes the range but not data? There are several reasons why you might want to do this:
- **Using several variables with different ranges and units** - this is essentially the same reason as the one we had for standardization with the difference that normalization can be applied to any set of data to make them unitless.
- **Distance-based algorithms and dimensionality reduction** - building on the first point, if we want to use an algorithm for exploring our data which relies on calculating and comparing distances between points we need to have the variables in the same range, otherwise variables with the larger range will have disproportionate influence on the algorithm results. Such algorithms include mostly **machine learning algorithms** such as k-nearest neihbours and k-means clustering algorithm, and dimensionality reduction techniques such as principal component analysis (PCA). If you would like to learn more about these you can have a look at tutorials here and here.**************
- **Convergence issues and improving model performance** - when we use more complicated models such **hierarchical models** and **Bayesian models** whose underlying calculations are much more complicated than for a regular linear model we can encounter the issue of **convergence**. **Convergence** essentially means that the model has successfully finished calculating the result. Non-scaled data often cause complicated models to **diverge (i.e. not converge)** as the distances between the points become too complicated for the model to handle.


Since showing all of these options is beyond the scope of this tutorial, we will only learn how to apply the procedure in R and show its results through histograms.

For this part we will work with a very well known dataset called **Palmer Penguins**. It is available through a package in R so you just need to install it and you can access the data at any point thereafter.

```r
# Install the penguins package
install.packages("palmerpenguins")

# Load the library
library(palmerpenguins)

# Import the data
penguins <- palmerpenguins::penguins

# Look at the variables in the dataset
str(penguins)
summary(penguins)
```
As you can see, the dataset contains data for three different species of penguins and measurements of bill length and depth (in mm), flipper length (in mm) and body mass (in g) and some other variables such as sex but we will focus on the four 'measurement' variables. Each of these variables has a different range of values, i.e. flipper length in mm will be a much larger value than the beak depth in mm. In addition to this the body mass is in a completely different units. If we wanted to use this dataset for let's say classifying the penguin species based on these 4 measurements we would need to scale them. That is what we are going to do now. Let's therefore apply the normalization to the 4 variables. Before we do that we need to remove observations with NA values so that we do not get any unexpected error and we have only complete observations.  

```r
# Remove observations with NA for the variables we are considering
penguins <- penguins[complete.cases(penguins[ , 3:6]),]  # filter out only observations which have value in columns 3:6

# Scale the penguin data using normalization
penguins <- penguins %>%
  mutate(bill_length_mm_norm = (bill_length_mm - min(bill_length_mm))/(max(bill_length_mm)-min(bill_length_mm)),
         bill_depth_mm_norm = (bill_depth_mm - min(bill_depth_mm))/(max(bill_depth_mm)-min(bill_depth_mm)),
         flipper_length_mm_norm = (flipper_length_mm - min(flipper_length_mm))/(max(flipper_length_mm)-min(flipper_length_mm)),
         body_mass_g_norm = (body_mass_g - min(body_mass_g))/(max(body_mass_g)-min(body_mass_g)))
```
Ugh, this is a quite repetetive code. There surely has to be a better way to apply the same procedure to 4 variables at once? Indeed, there is. `caret` package contains a function `preProcess()` which we can use to apply many different scaling and transformation procedures including **normalization**, **standardization** and even **Box-Cox transformation**. I kept this function a secret up until this point of the tutorial since it is important to understand how the individual scaling procedures and transformations work which is best done through manually implementing them. However, at this point we can make our lives easier by utilizing `preProcess()` as shown below (this code should produce the same result as above but will overwrite the existing columns instead of creating new ones).

```r
# Load the library
library(caret)

# Using preProcess to scale the data
penguins_mapping <- preProcess(penguins[, 3:6], method = c('range'))  # preProcess creates a mapping for the chosen variables
penguins_norm <- predict(penguins_mapping, penguins)  # we transform the data using predict() and the mapping
```
This is much neater than the previous procedure. You can explore the other transformations available in `preProcess()` in the documentation by using the command `help(preProcess)` in your console. The code for histograms has also got quite repetetive at this point so we will not write the full code here but this is what the histograms would look like for each of the variables with original, unscaled data and normalized data.

<center> <img src="{{ site.baseurl }}/penguin_panel.png" alt="Img" style="width: 800px;"/> </center>

As you can see the shapes of the histograms have not changed which is what we would expect. However, if you look at the x-axis there is a clear change in the scale which is exactly what we wanted to achieve.

After this your data would be ready to be crunched through an algorithm of your choice. We will not follow through with that in this tutorial as it would require explaining a lot of concept not directly related to scaling and transformation. Instead we will move on to the last part which will explain how to effectively change the scale on your plots without the need to change the variables themselves.

**NOTE** There are many other scaling procedures which can be useful in different situations. You can explore these on this Wikipedia page or here.
