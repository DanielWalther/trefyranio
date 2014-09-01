trefyranio
==========
In the files and folders here you can find the code used on the website *trefyranio.com*. The code for the full model as well as for some of the commonly used graphs is now available. I've also posted the code for my latest blog post and will continue to do so as I update the blog with anything other than the regular forecast. The goal is to make the code for all of my posts available online to ensure reproducability and transparency. 

To use the code you need to have the *R statistical software* (and preferably R Studio) installed. Make sure that you have **the latest version of R** installed (at least version 3.1.0) You also need to **install all the packages used in code**. The packages that are used are called in the beginning of the code through the "library" function, but you need to download them yourself before they can be called.

The *dataset* used for the forecasts can be found at https://github.com/MansMeg/SwedishPolls. This dataset is imported into R automatically when you run the forecast code.

# Changes and updates to the code

Since I started making my forecasts in May the code has been revised a couple of times. The two most important changes are:

* How the polls are weighted
* How the variance terms of the observation equation and the latent state are calculated
 
In the beginning I weighted and aggregated the polls on a monthly basis. Now I ignore time and instead focus on the number of people asked. As soon as *8000 people* have been polled those scores are aggregated and new polls that come in then form part of the next group of 8000 people. By aggregating based on number of people rather than time we ensure that all groupings are equally large and therefore simultaneously that all scores are equally reliable. 

Secondly, the variance is now calculated through the R time series command "StructTS" and then multiplied by 3 to take account of unobserved uncertainty stemming from potential bias in the polls. This seems to work well when applying the model to past elections but is also a bit of a "dirty fix" since we don't really know how large the unobserved bias is. 

In the future I will also employ different weights for different polling institutes but have yet to decide exactly how to calculate the weights.
