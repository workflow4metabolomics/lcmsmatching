Expanding the package
=====================

 1. Go to folder `/galaxy/dist/galaxy-pfem/tools/metabolomics/annotation`.
 2. Create a new folder named `lcmsmatching`.
 3. Enter the newly created folder `lcmsmatching`
 4. Expand package `mth-w4m-lcmsmatching-*.tar.gz`.

R requirements
==============

You need to install the following libraries for R:

 * stringr.
 * rJava.
 * xlsxjars.
 * xlsx.
 * getopt.

For this you need to run R as root:
``` {.bash}
sudo R
```

And then issue the following R commands:
``` {.r}
install.packages('stringr')
install.packages('rJava')
install.packages('xlsxjars')
install.packages('xlsx')
install.packages('getopt')
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
