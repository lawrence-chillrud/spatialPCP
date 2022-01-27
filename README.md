# spatialPCP

`spatialPCP` is a repository for the development of... Spatial PCP. And associated datasets, experiments, functions, etc.

Table of Contents:
1. [Software Requirements](#software-requirements)
2. [Repository Contents](#repository-contents)
3. [R and RStudio resources](#r-and-rstudio-resources)

## 1. Software requirements

Beyond many R packages that are easily found and installed via CRAN, two custom packages are needed for `spatialPCP`: `pcpr` and `PCPhelpers`. The original `pcpr` for older PCP functions can be found [here](https://github.com/Columbia-PRIME/pcpr). `PCPhelpers` can be found [here](https://github.com/Columbia-PRIME/PCPhelpers). Once cloning either repo, install in R with:

`install.packages("PATH_TO_FOLDER", repos = NULL, type="source")`

and load with:

`library(pcpr)` or `library(PCPhelpers)`

Other packages needed for `spatialPCP` can be installed and loaded by running [requirements.R](code/scripts/requirements.R). _Remember to update requirements.R as packages are added to scripts, please!_

## 2. Repository contents

* [code](code)
	* [functions](code/functions): for functions such as [make\_map.R](code/functions/make_map.R) or [make\_adjacency\_mat.R](code/functions/make_adjacency_mat.R)
	* [scripts](code/scripts): for experiments

* [data](data)
	* [censusNYC2010](data/censusNYC2010): contains [census data](data/censusNYC2010/nyc_census_2010.csv) as a `.csv` file along with an accompanying [README.txt](data/censusNYC2010/README.txt).
	* [shapefilesNYC2010](data/shapefilesNYC2010): contains necessary shapefiles for mapping 2010's NYC census tracts. Also contains accompanying adjacency matrix saved as a `.rds` file which can be loaded with: 

		```A <- readRDS(here::here("data", "shapefilesNYC2010", "adjacencyMatrix.rds"))```

* [figures](figures)
	* [jointMaps](figures/jointMaps): some maps made from the 2010 NYC census data (from [mapCensusNYC2010.R](code/scripts/mapCensusNYC2010.R)).
	* [pcaPlots](figures/pcaPlots): some summary PCA plots of the 2010 NYC census data (from [mapCensusNYC2010.R](code/scripts/mapCensusNYC2010.R)). 
	* _Note: please do not push many large figures into this directory, or the `.git` folder will blow up in size. In fact I've added the folder to the [.gitignore](.gitignore), so you need to force add files inside figures to the repo with `git add -f [filename]`._

## 3. R and RStudio resources

### Downloads

* [R](https://www.r-project.org)

* [RStudio](https://www.rstudio.com/products/rstudio/download/) IDE

### Free resources for learning R

* [R for Data Science](https://r4ds.had.co.nz/index.html) by Hadley Wickham & Garrett Grolemund

* [R Programming for Research](https://geanders.github.io/RProgrammingForResearch/) by Brooke Anderson, Rachel Severson, & Nicholas Good
	* [Accompanying Video Lectures](https://www.youtube.com/channel/UC73v_zCdNE2aZrFZsG22JBw/playlists)

* [Data Science I](https://www.p8105.com) by Jeff Goldsmith

* [Data Science: Foundations using R](https://www.coursera.org/specializations/data-science-foundations-r) course by Johns Hopkins via Coursera

* [Introduction to R](https://www.datacamp.com/courses/free-introduction-to-r?utm_source=adwords_ppc&utm_medium=cpc&utm_campaignid=15888888220&utm_adgroupid=140760953428&utm_device=c&utm_keyword=data%20camp%20r&utm_matchtype=p&utm_network=g&utm_adpostion=&utm_creative=575101531132&utm_targetid=aud-299261629654:kwd-1299889917519&utm_loc_interest_ms=&utm_loc_physical_ms=9073502&gclid=Cj0KCQiAraSPBhDuARIsAM3Js4pYTde7m10pSU44KwM6LY349sXa4cR5LtnIiBeMRiBQzFdU3KSWK2AaAnC3EALw_wcB) datacamp course