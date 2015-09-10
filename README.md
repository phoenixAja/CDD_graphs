
>#**Elution Profile Graph Using R Scripts** 
   
>## **Phoenix Logan**
>###**2015**
   



###**Introduction:**  

This program was designed to run on R 3.2.0 ("_Full of Ingredients_"), however this script will work on newer versions of R as well, but it has not yet been tested on those architypes.
To successfully run this script you will need to execute it using terminal. If you are running this script from a windows platform I strongly recommend using [gitbash](https://git-for-windows.github.io/). If for some reason you want an alternative, you can use [cygwin](https://www.cygwin.com/). 
To get the script **CDD_new.R** you can fork it from this repository to your own github account, or you can ask me to email it to you. Feel free to submit a pull request to me if you want to edit this file yourself or want to add changes.

###**R Installation**###
* To install R, go [to their website to download the most recent version](https://cran.r-project.org/mirrors.html) and choose an appropriate CRAN mirror.
* No additional R packages are needed to run this specific script.
* Once R has been downloaded get the absolute path to Rscript.exe:
 1. For Windows this would most likely be located on the C Drive in Program files. 
  >Ex:  /c/Program\\ Files/R/R-3.2.0/bin/Rscript.exe
 
 2. For Linux and Mac This would be (most likely) in bin 
 


###**Running Script in Terminal:**
1. Open Terminal, and head to directory where Files are contained that need to be parsed 
2.  When in desired location run the following command:
>Ex: /c/Program\\Files/R/R-3.2.0/bin/Rscript.exe CDD\_new.R CDD_File.csv
 
###**Output:**
1. a graph of output data presented in landscape and overlayed fashion

 * A Few Important Notes:
  * Data to be parsed must have a .csv extension
  * Specify the full path to the R script (**CDD\_new.R**) In this case I am running the script from the same directory where the parsed files are located.
  * If you run into problems feel free to email me at phoenix@phoenixlogan.net
