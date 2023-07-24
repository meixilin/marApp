
#######################################################################
# Read tables
read_table <- function(mypath) {
    mysuffix = tools::file_ext(mypath)
    if (mysuffix == 'gz') {
        mypath = gzfile(mypath,'rt')
    }
    output = read.table(mypath, header = 1, sep = "\t", stringsAsFactors = F,
                        quote = "", comment.char = "")
    return(output)
}

