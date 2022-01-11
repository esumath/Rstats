## About this app

Funded by an FPDC (2021 - 2022) grant, this web app is built by [**Xuemao Zhang**](https://www.esu.edu/mathematics/faculty.cfm).  The goal of the app is to promote students' interest in [**R programming**](https://cran.r-project.org/) while learning **introductory statistics**.  Installation and configuration are not needed as a web-based application.  Students interested in coding can copy the **R code** in the app and paste to the R workspace installed on their PC or Mac to **reproduce** the data analysis results.  However, the functionality is point-and-click so those uninterested in coding will not be intimidated by the features. The app was intended for students in all PASSHE ([**PA State System of Higher Education**](https://www.passhe.edu)) schools to use.  It is expected to be adopted by more undergraduate universities.  Please contact me **<xzhang2@esu.edu>** if you have any suggestions or ideas to improve this web app.

DO NOT upload sensitive data to the app since security considerations have not been addressed yet.

Some R packages are used in this app for statistical data analyses.  If you are interested in R programming, please install these packages by pasting `install.packages(c("car","BSDA","datarium","dplyr","EnvStats","ggplot2", "rstatix"))` to the R console after [**R**](https://cran.r-project.org/) is installed on your computer.


### Structure of the app
The app has the following six menus:
&nbsp;
* **Distributions**
 + **Probability Calculation**     Probability calculations for some discrete and continuous distributions including Finite distribution, Binomial distribution, Poisson distribution, Normal distribution, Student-t distribution, Chi-square distribution and F distribution.
 + **Quantile Calculation**     Calculating quantiles/percentiles for Normal distribution, Student-t distribution, Chi-square distribution and F distribution. 
&nbsp;
* **Inferences**
 + **Statistics**   Confidence interval estimation and hypothesis testing when summary statistics of a sample data are given.
 + **Data**    Confidence interval estimation and hypothesis testing when sample data are is given.  The data must be manually entered.

&nbsp;
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;  The following four menus will allow users to perform basic data analyses and statistical routines after a dataset is uploaded.
* **Data Import**     
 + Users can upload or manually enter data (up to 5 variables), transform variables in the data, and download the data set.   Only .csv (comma-separated values) format files are accepted by the app when a data set is to be uploaded.  
&nbsp;
* **Univariate**
 + **Numerical Data Analysis**    Users can describe a single numerical variable with graphs and numerical summaries, and conduct statistical inferences about population means and variances.
 + **Categorical Data Analysis**    Users can describe a single categorical variable with graphs and numerical summaries, and conduct statistical inferences about population proportions.
&nbsp;
* **Multivariate**
 + **SLR Model**    Users use scatter plot and the simple linear regression model to describe the relationship between two numerical variables. 
 + **MLR Model**   Users study the relationship between a response variable and a set of explanatory variables using polynomial regression model, multiple linear regression model, or the logistic regression model. 
 + **Contingency Analysis** Users investigate the dependence between two categorical variables by contingency analysis.
&nbsp;
* **ANOVA**
 + **One-way ANOVA**   The one-way ANOVA (analysis of variance) is used to determine whether there are any statistically significant differences between the means of three or more independent (unrelated) groups.
 + **Two-way ANOVA**   The two-way ANOVA is used to understand if there is an interaction between the two independent factors on the numerical response variable, and compare the factor levels or combination of factor levels if no significant interaction exists.


### Main techs used in this app 

+ The [`{golem}`](https://github.com/ThinkR-open/golem) framework is used to build the Shiny App backend. 

+ The whole app is powered by [`{shiny}`](https://github.com/rstudio/shiny).

+ Most data visualisations are done with [`{ggplot2}`](https://github.com/tidyverse/ggplot2).

+ Reproducible R code is done with the function [`interpolate`](https://github.com/gammarama/intRo/tree/papers/arxiv-2016) and package [`{shinymeta}`](https://github.com/rstudio/shinymeta).

+  Ace text editor is provided by [`{shinyAce}`](https://github.com/trestletech/shinyAce), and [`{rclipboard}`](https://github.com/sbihorel/rclipboard) provides copy-to-clipboard functionality.

+  Excel-like data entry ability is provided by [`{rhandsontable}`](https://github.com/jrowen/rhandsontable).

+ Packages for data analysis include [`{car}`](https://github.com/cran/car), [`{BSDA}`](https://github.com/alanarnholt/BSDA), [`{datarium}`](https://github.com/kassambara/datarium),  [`{dplyr}`](https://github.com/tidyverse/dplyr), [`{EnvStats}`](https://github.com/cran/EnvStats) and [`{rstatix}`] (https://github.com/kassambara/rstatix).

+ Mathematical symbols and equations are rendered using [`MathJax`](https://github.com/mathjax/MathJax).

+ The app is deployed on [RStudio shinyapps.io](https://www.rstudio.com/products/shinyapps/).



