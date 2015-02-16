# dplyr.root
**dplyr.root** is a ROOT Backend for the package [dplyr](https://github.com/hadley/dplyr).

Currently, only the most basic functionallity is implemented:
`filter`, `mutate`, `select` and derivatives.
For any other operations, e.g. `group_by`, `summarise` or joins, the data has
to be first read in memory with `collect()`.
Selections can be cached internally (using TEntryList) with `collapse()`.

You can install the latest development version from github with
```{r}
devtools::install_github("rmatev/RootTreeToR")
devtools::install_github("rmatev/dplyr.root")
```
