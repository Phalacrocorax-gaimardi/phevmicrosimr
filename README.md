# phevmicrosimr

<!-- badges: start -->
<!-- badges: end -->

This is an agent-based microsimulator for ZEV uptake. 

## Installation

```
library(devtools)
install_github("Phalacrocorax-gaimardi/microsim")
install_github("Phalacrocorax-gaimardi/phevmicrosimr")
```

## Usage
```
runABM(scenario=scenario_1,Nrun=8)
```
Use `help(runABM)` for more details.

## Data

```
#fleet data
data(fleet)
#agent characteristics
data(agents_init)
#techno-economic scenario
data(scenario_1)
```

## Dependencies

- R Version 4.0.0 or later.

- Tested on Windows and Mac systems.

- Social influence network builder [microsim](https://github.com/Phalacrocorax-gaimardi/microsim)



