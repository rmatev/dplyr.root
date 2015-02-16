# dplyr.root
**dplyr.root** implements the inteface of the
[dplyr](https://github.com/hadley/dplyr) package
for ROOT files backend.
For more information on ROOT see https://root.cern.ch.


## Feature set
Currently, only the most basic functionallity is implemented:
`filter`, `mutate`, `select` and derivatives.
For any other operations, e.g. `group_by`, `summarise` or joins, the data has
to be first read in memory with `collect()`.
Selections can be cached internally (using TEntryList) with `collapse()`.

## Installation
You must have ROOT installed on the system before installing *RootTreeToR*.

You can install the latest development version from github with
```{r}
devtools::install_github("rmatev/RootTreeToR")
devtools::install_github("rmatev/dplyr.root")
```
