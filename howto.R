# Set current folder to where the data is
files <- list.files(pattern = "*.asc", full.names = T) # Get list of asc files
data <- read_asc(files) # read files into table (data.frame)
ssdata <- pre_process(data) # Subset the data for 1000 to 1500
fitted <- data.frame(fit_poly(ssdata)) # Fit polynomial and calculate reverse potential
get_p(fitted) # Get only the reverse potential for each sample
