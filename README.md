### Overview

The `smoove` package is a collection of functions to work with continuous time correlated velocity movement (CVM) models as described in Gurarie et al. (nearly in press is *Movement Ecology*).   There are functions that simulate, diagnose and estimate these movement models, as well as functions for facilitating a change point analysis.

To install in R:
```
require(devtools)
install_github("EliGurarie/smoove")
```

To see a detailed vignette:
```
vignette("smoove")
```

Or simply: https://github.com/EliGurarie/smoove/blob/master/vignettes/smoove.pdf.

### MacOS
If you get the following error while installing on the MacOS:

>ld: warning: directory not found for option '-L/usr/local/lib/gcc/x86_64-apple-darwin13.0.0/4.8.2'
ld: library not found for -lgfortran
clang: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [smoove.so] Error 1
ERROR: compilation failed for package ‘smoove’
* removing ‘/Users/rokays/Library/R/3.3/library/smoove’
Installation failed: Command failed (1)

Run the following command on your terminal to install `gfortran` first and then install using `devtools`:

```shell
curl -O http://r.research.att.com/libs/gfortran-4.8.2-darwin13.tar.bz2
sudo tar fvxz gfortran-4.8.2-darwin13.tar.bz2 -C /
```

### References

E. Gurarie, C. Fleming, W.F. Fagan, K. Laidre, J. Hernández-Pliego, O. Ovaskainen. Correlated velocity models as a fundamental unit of animal movement: synthesis and applications. [*Movement Ecology*](https://movementecologyjournal.biomedcentral.com/articles/10.1186/s40462-017-0103-3). 
