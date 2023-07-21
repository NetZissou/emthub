library('devtools'); 
library('shiny');

args <- commandArgs(trailingOnly=TRUE);
options(shiny.port = as.numeric(args[1]));
options(shiny.host = '0.0.0.0');

load_all();
app();
