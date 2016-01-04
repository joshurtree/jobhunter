## Before installation
This program has only been tested on a linux machine. Due to a lack of supporting packages it may not work on Windows.

## Installation 
The program uses the programming language R therefore a copy has to be downloaded from 

https://cran.r-project.org/mirrors.html

Then from within the R shell run the command 

install.packages('XML', 'parallel', 'shiny')

To run the program run the command

R -e "shiny::runApp('install_dir')"

or from within the R shell

shiny::runApp('install_dir')

Where install_dir is the directory where the Job Hunter files have been extracted 
