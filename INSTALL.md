## Before installation
This program has only been tested on a linux machine. Due to a lack of supporting packages it may not work on Windows.

## Installation 
The program uses the programming language R therefore a copy has to be downloaded from 

https://cran.r-project.org/mirrors.html

Then from the command line type

R -e "install-packages('parallel', 'shiny')

In the directory where the Job Hunter files have been extracted run the command

R -e "shiny::runApp('.')"
