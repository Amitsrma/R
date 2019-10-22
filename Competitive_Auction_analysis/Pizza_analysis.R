order = read.csv("order_random_sampling.csv")

# merging two dataframes on a common column
# actually not necessary, checking 
merged_1 = merge(order, ctype, by='CheeseTypeKey')

head(merged_1)

revenue_cube <- 
    tapply(merged_1$ProfitKey, # can be run with `order` dataframe too, if merged isn't created
           merged_1[,c("Store.LocationKey", "Month", "Year", "PizzaSizeKey")], 
           FUN=function(x){return (sum(x))})
# Showing the cells of the cube
revenue_cube

# selecting NA's in cube and changing them to zero
revenue_cube[is.na(revenue_cube)] = 0

# show changed revenue_cube
revenue_cube
