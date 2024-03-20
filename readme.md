----
title: Series of utility functions in R using formulas. 
author: Charles-Édouard Giguère
date: 2019-04-01

---

<!-- badges: start -->
[![cran version](http://www.r-pkg.org/badges/version/CUFF)](https://cran.r-project.org/package=CUFF)
[![Monthly Downloads](https://cranlogs.r-pkg.org/badges/CUFF)](https://cranlogs.r-pkg.org/badges/CUFF)
[![Total Downloads](https://cranlogs.r-pkg.org/badges/grand-total/CUFF)](https://cranlogs.r-pkg.org/badges/grand-total/CUFF)
<!-- badges: end -->

# CUFF: Charles utility functions using formulas 

1.  correlation: function to produce correlation matrix. The associated print method 
                 only prints the lower triangle of the matrix. 
    
2.  cont.desc: function to describe continuous variables
    
3.  cross: crosstabs builder with an optional latex and excel output. See ftab also 
           to print a table with frequencies and percentages in one table. 
    
4.  descr: function to describe a mixture of continuous and categorical variables
    
5.  freq: frequencies of a vector or a data.frame. 
    
6.  strutil: Series of string utility. For a more extensive set of
             functions use the stringi package.
    
7.  sum.n: sum weighted by the number of non missing values. 
    
8.  meansd: mean and sd displayed in a string.
    
9.  cf: Extract formated coefficient. 

10. clip: put a matrix-like object into clipboard.

11. ftab: add percentages to a table to get percentages and 
          frequencies in the same output.

12. meansd: function that combines mean and sd in the same output.
            To be used with xf. 

13. view: view matrix-like objects in a browser like in R-studio. 

14. xf: Function similar to xtabs but applied to a continuous variable. 
        This function applies a function to a variable across levels of 
		one or more categorical variable. 
