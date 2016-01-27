Expanding the package
=====================

 1. Go to folder `/galaxy/dist/galaxy-pfem/tools/metabolomics/annotation`.
 2. Create a new folder named `lcmsmatching`.
 3. Enter the newly created folder `lcmsmatching`
 4. Expand package `mth-w4m-lcmsmatching-*.tar.gz`.

R requirements
==============

R version 3.1.0 or higher is required (because of package plyr).

Then you need to install some libraries for R:

For this you need to run R as root:
``` {.bash}
sudo R
```

First choose a CRAN mirror:
``` {.r}
chooseCRANmirror(graphics = FALSE)
```

And then issue the following R commands:
``` {.r}
install.packages('getopt')
install.packages('stringr')
install.packages('plyr')
install.packages('XML')
```

Adding the new tool to Galaxy
=============================

Edit `/galaxy/dist/galaxy-pfem/metabolomics_tool_conf.xml`, and add inside the section `6-Annotation` the following line:
``` {.xml}
<tool file="annotation/lcmsmatching/lcmsmatching.xml"/>
```

Restarting W4M Galaxy
=====================

``` {.bash}
service galaxy restart
```
