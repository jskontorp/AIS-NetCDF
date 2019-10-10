# AIS-NetCDF
A guide on merging AIS with NetCDF data in R.

Firstly the metorological data can be downloaded from [Copernicus](https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels?tab=form), either via filling out the form, or using the API (see [link]).

```
library(ncdf4)
library(dplyr)
library(lubridate)
library(tidyverse)
library(vroom)
```
