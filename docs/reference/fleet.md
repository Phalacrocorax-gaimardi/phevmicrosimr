<div class="container template-reference-topic">

<div>

<div class="navbar navbar-default navbar-fixed-top" role="navigation">

<div class="container">

<div class="navbar-header">

<span class="sr-only">Toggle navigation</span> <span
class="icon-bar"></span> <span class="icon-bar"></span> <span
class="icon-bar"></span>

<span class="navbar-brand">
<a href="../index.html" class="navbar-link">phevmicrosimr</a> <span
class="version label label-default" toggle="tooltip" placement="bottom"
title="Released version">0.1.0</span> </span>

</div>

<div id="navbar" class="navbar-collapse collapse">

-   [<span class="fas fa-home fa-lg"></span>](../index.html)
-   [Reference](../reference/index.html)

<!-- -->

</div>

</div>

</div>

</div>

<div class="row">

<div class="col-md-9 contents">

<div class="page-header">

# 2021 passenger car models

<div class="hidden name">

`fleet.Rd`

</div>

</div>

<div class="ref-description">

2021 new passenger cars listed in Ireland with technology costs. The
order follow CSO car model sales data 2014-2019 except where new models
have been introduced. The lowest cost trim is used except in cases where
vehicle type is available in a more expensive trim only. In some cases
this comparison is not possible e.g mini cooper BEV has in basic trim
while PHEV version exists as premium "Countryman" trim only. Prices are
from 2021 price list where possible, however e.g. Audi and BMW prices
are adjusted 2020 prices.

</div>

``` usage
fleet
```

## <a href="#format" class="anchor"></a>Format

A data frame with 244 rows and 16 variables:

make  
manufacturer

model  
model

segment  
car market segment A,B,C,D,E,F, crossovers (-J) and multi-purpose
vehicles (-M). Sports cars are S.

type  
powertrain or fuel type

2021_rrp  
price excluding subsidies

tech_cost_2021  
retail price before incentives (including VAT and VRT)

WLTP  
combined WLTP emissions

vrt  
vrt rate for 2021 based on WLTP

motor  
road tax (euros)

kWh  
Li-B capacity. Mild HEVs are assigned a value 0.4, HEVs are assigned 1
all others from manufacturer

2021_rrp\*  
list price (bevs and phevs only) including incentives

comment  
additional info

kWh/100km  
conversion efficiency if quoted

AER  
quoted electric ranges

model_start  
introduction model year

model_end  
last model year if applicable

## <a href="#source" class="anchor"></a>Source

[Multiples sources primarilty manufacturer price lists. Also
https://www.rte.ie/brainstorm/2020/1215/1184581-electric-cars-buyers-guide-2021-ireland/](Multiples%20sources%20primarilty%20manufacturer%20price%20lists.%20Also%20https://www.rte.ie/brainstorm/2020/1215/1184581-electric-cars-buyers-guide-2021-ireland/)

</div>

<div id="pkgdown-sidebar" class="col-md-3 hidden-xs hidden-sm">

## Contents

</div>

</div>

<div class="copyright">

Developed by The package maintainer.

</div>

<div class="pkgdown">

Site built with [pkgdown](https://pkgdown.r-lib.org/) 1.6.1.

</div>

</div>
