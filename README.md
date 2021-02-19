# phevmicrosimr

<!-- badges: start -->
<!-- badges: end -->

Agent-based microsimulator for ZEV transition. 

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

- Tested on Windows 10 and macOS Mojave.

- Social influence network builder [microsim](https://github.com/Phalacrocorax-gaimardi/microsim)

