## this file contains the function print_results. This function gets the data from the Rdata files and prints it ##

print_results(p=10,graph="random",n=20,report="auc",round=2)
{
    load(file = "testdata.Rdata")

    if ( report = "auc")
    {
        cat("the area under the curve is: ",result $ auc_mpl_bd,file="output",append=FALSE)
    }
        


}