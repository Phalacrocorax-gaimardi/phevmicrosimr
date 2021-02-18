# phevmicrosimr
This is an agent-based microsimulator for ZEV uptake. 

## Installation

```
library(devtools)
install_github("Phalacrocorax-gaimardi/microsim")
install_github("Phalacrocorax-gaimardi/phevmicrosimr")
```

## Use
```
runABM(scenario=scenario_1,Nrun=8)
```
See
```
help(runABM)
```
for more details

## Data

## Background

Agent characteristics in the model correspond to a representative 2018 survey of 924 Irish households on attitudes to electric vehicles. These agents exercise purchase decisions on a passenger car fleet that is derived from new passenger car models available in Ireland in 2021. 235 car models are included, with 30 BEVs, 39 PHEVs, 15 HEVs, 87 petrol and 64 diesel vehicles. The dataset contains pricing and technical parameters as well as the year of introduction of new technology for HEVs, BEVs and PHEVs. Both new or used cars can be purchased depending on the agents’ preference.
Simulations are initialized in January 2015, with initial fuel types (petrol or diesel) and registration year taken from the survey where available. A market segment is also assigned probabilistically based on household characteristics. Randomization of the agents’ social network and preferred market segment is implemented at the beginning of every run. At each monthly time step, a fixed percentage (1.6%) of agents decide to replace their car. Their choice is based on the maximisation of financial utility calculated from a selection of vehicles in their segment, except in the case of BEVs where there are additional social, range anxiety and penalty (risk-aversion) terms. No penalty is included for switching from a PHEV to a BEV so that this decision is based on financial and range anxiety terms only. This treatment makes PHEVs a potential “gateway technology” to full electrification.

## Dependencies

R Version 4.0.0 or later.

Tested on Windows and Mac systems.

Social influence network builder
[microsim](https://github.com/Phalacrocorax-gaimardi/microsim)



