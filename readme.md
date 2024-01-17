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

1. correlation: function to produce correlation matrix. The associated
   print matrix only prints the lower triangle part of the matrix. 
   
2. cross: crosstabs builder with an optional latex and excel
   output. See ftab also to print a table with frequencies and
   percentages in one table. 
   
3. freq: frequencies of a vector or a data.frame. 

4. strutil: Series of string utility. For a more extensive set of
   functions use the stringi package.
   
5. sum.n: sum weighted by the number of non missing values. 

6. meansd: mean and sd displayed in a string.

7. cf: Extract formated coefficient. 

8. clip: put a matrix-like object into clipboard.

9. ftab: add percentages to a table to get percentages and 
         frequencies in the same output.

10. meansd: function that combines mean and sd in the same output.
            To be used with xf. 

11. view: view matrix-like objects in a browser like in R-studio. 

12. xf: Function similar to xtabs but applied to a continuous variable. 
        This function applies a function to a variable across levels of 
		one or more categorical variable. 
