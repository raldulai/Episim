#!/bin/sh

cd ..
R --vanilla <<RSCRIPT
library(inlinedocs);
package.skeleton.dx("Episim", excludePattern="AllClasses|nonexports|sn")
RSCRIPT

R CMD build Episim

