trefyranio
==========
In the files and folders here you can find the code used on the website *trefyranio.com*. The code for the full model as well as for some of the commonly used graphs is now available. I've also posted the code for my latest blog post and will continue to do so as I update the blog with anything other than the regular forecast. The goal is to make the code for all of my posts available online to ensure reproducability and transparency. 

To use the code you need to have the *R statistical software* (and preferably R Studio) installed. Make sure that you have **the latest version of R** installed (at least version 3.1.0) You also need to **install all the packages used in code**. The packages that are used are called in the beginning of the code through the "library" function, but you need to download them yourself before they can be called.

The *dataset* used for the forecasts can be found at https://github.com/MansMeg/SwedishPolls. This dataset is imported into R automatically when you run the forecast code.

# Changes and updates to the code

Since I started making my forecasts in May the code has been revised a couple of times. The two most important changes are:

* How the polls are weighted
* How the variance terms of the observation equation and the latent state are calculated
 
In the beginning I weighted and aggregated the polls either on a monthly basis or after a certain number of people had been polled. Now I avoid aggregation and instead have a completely continuous time trend. Each new poll is included as a new observation in the model and it is weighted by the square root of the number of people that have been polled.

Secondly, the variance of the observed state is assumed to have mean zero and a standard deviation corresponding to the size of the poll. The variance of the latent state is given a (constant) non-informative gamma prior.

Two polling houses are given less weight - Sentio and United Minds. These tend to stand out from other polls more than one would expect from random chance alone and United Minds in particular also performed poorly in the 2010 election. All polls from Sentio are weighted down by 20% and those from United Minds by 10%.
