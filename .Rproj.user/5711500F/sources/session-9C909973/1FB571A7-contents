#Loading libraries
library(purrr)#Find an equivalent of this here
library(dplyr)#Find data table equivalents of this
library(janitor)

library(ranger)
library(igraph)
library(Matrix)

library(correlation)
library(data.table)
library(fastDummies)


#Loading the example dataset
#df_tot <- read.csv("C:\\Users\\sunny\\Downloads\\housingPrices\\train.csv")

DataCleaning = function(df_tot)
{
  #Coerce to character
  df_tot <- as.data.table(df_tot)
  df_tot[is.na(df_tot), ] <- 0


  #Remove all spaces with underscore R
  names(df_tot) <- make.names(names(df_tot), unique=TRUE)
  df_tot<- janitor::clean_names(df_tot)


  # Replace all NAs by 0
  #Coerce to factor
  changeCols_char <- colnames(df_tot)[which(as.vector(df_tot[,lapply(.SD, class)]) == "character")]
  df_tot[,(changeCols_char):= lapply(.SD, as.factor), .SDcols = changeCols_char]

  #Coerce to numeric data type
  changeCols_num <- colnames(df_tot)[which(as.vector(df_tot[,lapply(.SD, class)]) == "integer")]
  df_tot[,(changeCols_num):= lapply(.SD, as.numeric), .SDcols = changeCols_num]

  #Clean column names


  #Cleaning constant columns
  #df_tot[sapply(df_tot, is.character)] <- lapply(df_tot[sapply(df_tot, is.character)], as.factor)
  #df_tot[sapply(df_tot, is.integer)] <- lapply(df_tot[sapply(df_tot, is.integer)], as.numeric)

  #numeric type creation of ordered factor columns
  #df_tot = as.numeric(df_tot[['The orderedcolumns here']])

  #Dummy creation uon columns that are unordered factors
  df_tot = fastDummies::dummy_cols(df_tot)
  df_tot<- janitor::clean_names(df_tot)


  #Dropping the previous columns
  df_tot[, (changeCols_char) := NULL]


  return(df_tot)
}

GeneralCor = function(df, cor1 = 'pearson', cor2 = 'PointBiserial', cor3 = 'kendall' , dummies = FALSE)
{
  cor_value <- NULL

  cor_fun <- function(pos_1, pos_2, cor1 = 'pearson', cor2 = 'biserial', cor3 = 'kendall', cor4 = 'polychloric')
  {

    #Same value , we return 1
    if(identical(df[[pos_1]], df[[pos_2]]) || pos_1 == pos_2)
    {

      cor_value = 1
      return(cor_value)
    }

    #If one factor and one numeric
    if(class(df[[pos_1]])[1] %in% c("numeric") && class(df[[pos_2]])[1] %in% c("numeric"))
    {
      if(cor1 == 'pearson')
      {
        cor_value  <- correlation::cor_test(df, x = names(df)[pos_1], y = names(df)[pos_2], method = 'pearson')$r
      }
    }


    #If both are factor check
    if(class(df[[pos_1]])[1] %in% c("factor") && class(df[[pos_2]])[1] %in% c("factor"))
    {
      if(cor2 == 'Polychloric') #binary
      {
        cor_value  <- correlation::cor_test(df, x = names(df)[pos_1], y = names(df)[pos_2] , method = 'Polychloric')$r
      }
    }

    #If one is factor and another is numeric
    if(class(df[[pos_1]])[1] %in% c("numeric") && class(df[[pos_2]])[1] %in% c("factor") || class(df[[pos_1]])[1] %in% c("factor") && class(df[[pos_2]])[1] %in% c("numeric"))
    {
      if(cor2 == 'polychoric') #binary
      {
        cor_value  <- correlation::cor_test(df, x = names(df)[pos_1], y = names(df)[pos_2] , method = 'polychoric')$r
      }
    }

    #
    if(class(df[[pos_1]])[1] %in% c("ordered") && class(df[[pos_2]])[1] %in% c("ordered"))
    {
      if(cor2 == 'polychoric') #binary
      {
        cor_value  <- correlation::cor_test(df, x = names(df)[pos_1], y = names(df)[pos_2] , method = 'polychoric')$r
      }
    }

    # #If both are numeric
    if((class(df[[pos_1]])[1] %in% c("numeric") && class(df[[pos_2]])[1] %in% c("ordered")) || class(df[[pos_1]])[1] %in% c("ordered") && class(df[[pos_2]])[1] %in% c("numeric"))
    {

      if(cor3 == 'kendall')
      {
        cor_value  <- correlation::cor_test(df, x = names(df)[pos_1], y = names(df)[pos_2] , method = 'kendall')$r
      }

    }

    return(cor_value)
  }

  cor_fun <- Vectorize(cor_fun)

  #Computing the matrix
  corrmat <- outer(1:ncol(df)
                   ,1:ncol(df)
                   ,function(x, y) cor_fun(pos_1 = x, pos_2 = y,  cor1 = 'pearson', cor2 = 'PointBiserial', cor3 = 'kendall'))

  rownames(corrmat) <- colnames(df)
  colnames(corrmat) <- colnames(df)

  return(corrmat)
}

Y_var = 'sale_price'
CorrelatedFeatures = function(Data, Y_var, Focus_variables = list(), corr_cutoff = 0.7, RF_coverage = 0.95, num_features = 5,  plot = FALSE, fast_calculation = FALSE, cor1 = 'pearson', cor2 = 'PointBiserial', cor3 = 'cramersV')
{
  #ToDo
  #Perform all subletting and initalizations here
  #Creating clusters based upon graph theory
  list1 = list()

  #Data Cleaning
  Data <- DataCleaning(Data)

  #note to add all data checks that are needed in the system

  #If any NA values drop it
  Data[is.na(Data), ] <- 0

  #If any value is not a numeric or factor , we drop it
  class_check <- as.data.table(sapply(Data, class))
  if(any(class_check$V1 %in% 'character'))
  {
    print('Please convert all chracter variables to factors or dummies')
    break
  }

  Data <- as.data.table(Data)

  ###Correlation matrix creation
  #Examine this further of course
  cor_matrix <- GeneralCor(Data[, -Y_var, with = FALSE])
  cor_matrix[cor_matrix == 'NULL'] <- 0

  ut <- upper.tri(cor_matrix)
  pairs_mat <- data.frame(
    var1 = rownames(cor_matrix)[row(cor_matrix)[ut]],
    var2 = colnames(cor_matrix)[col(cor_matrix)[ut]],
    value = unlist((cor_matrix)[ut])
  )

  #Sub-setting values only above threshold values
  pairs_mat = pairs_mat[which(abs(pairs_mat$value) >= corr_cutoff & (pairs_mat$var1 != pairs_mat$var2)),]
  rownames(pairs_mat) <- NULL


  ##Start of Random Forest iterations

  list1 = list()

  if(dim(pairs_mat)[1] != 0)
  {
    for(j in 1:nrow(pairs_mat))
    {
      list1[[j]] = c(pairs_mat[j,1], pairs_mat[j,2])
      list1[[j]] = sort(list1[[j]])
    }

    i = rep(1:length(list1), lengths(list1))
    j = factor(unlist(list1))
    tab = sparseMatrix(i = i , j = as.integer(j), x = TRUE, dimnames= list(NULL, levels(j)))
    connects = tcrossprod(tab, boolArith = TRUE)
    group = clusters(graph_from_adjacency_matrix(as(connects, "lsCMatrix")))$membership
    var_groups <- tapply(list1, group, function(x) sort(unique(unlist(x))))
    var_groups <- as.list(var_groups)

    #Condition to extract the focus variables
    for(i in 1:length(var_groups))
    {
      if(any(var_groups[[i]] %in% Focus_variables))
      {
        if(sum(which(var_groups[[i]] %in% Focus_variables >= 2)))
        {
          Intersection <- dplyr::intersect(Focus_variables, var_groups[[i]])
          var_groups[[i]] <- Intersection[1]

          next
        }

        var_groups[[i]] <- dplyr::intersect(Focus_variables, var_groups[[i]])
      }
    }

    #Getting every combination of variables possible
    if(fast_calculation == TRUE)
    {
      result <- purrr::map(var_groups, 1)
      result <- as.data.frame(t(unlist(result)))
    }else
    {
      result <- expand.grid(var_groups)
    }

    RF_list <- list()

    noncor_columns = colnames(Data)[! colnames(Data) %in% unlist(var_groups)]
    Data_nocor <- Data[, ..noncor_columns]

    ##Start of the RF, note we need to add multiprocessing here
    for(i in 1:nrow(result))
    {
      Data_temp <- cbind(Data_nocor, Data[, unlist(result[i,]), with = FALSE])

      Rf <- ranger(as.formula(paste(paste(Y_var, '~'), paste(colnames(Data_temp), collapse = "+"))), data = Data_temp, mtry = ncol(Data_temp/3), importance = 'permutation')
      Rf_2 <- data.frame(Rf$variable.importance)
      RF_list[[i]] <- Rf_2

      print(i)
    }

  }else
  {
    Rf <- ranger(as.formula(paste(paste(Y_var, '~'), paste(colnames(Data_temp), collapse = "+"))), data = Data_temp, mtry = ncol(Data_temp/3), importance = 'permutation')
    Rf_2 <- data.frame(Rf$variable.importance)
    RF_list[[i]] <- Rf_2
  }


  #Fast Aggregation across multiple frames
  l <- lapply(RF_list, function(x) {x$RowName <- row.names(x) ; x})
  Res <- Reduce(function(...) merge(..., by = "RowName", all = TRUE), l)

  Rf_2 <- dcast(melt(setDT(Res), "RowName"),
          RowName ~ sub("\\..*", "", variable),
          mean,
          na.rm = TRUE,
          value.var = "value")


  #Taking the best variable from each group
  if(dim(pairs_mat)[1] != 0)
  {
    for(bv in 1:nrow(var_groups))
    {
      comp <- var_groups[bv]
      comp <- unlist(comp[[1]])
      temp <- Rf_2[which(Rf_2$RowName %in% comp)]
      keep_var <- Rf_2$RowName[Rf_2$Rf == max(temp$Rf)]
      rem_var <- comp[which(comp != keep_var)]

      #Dropping all values not needed
      Rf_2 <- Rf_2[!(Rf_2$RowName %in% unlist(rem_var))]
    }

    #Fitting the RF without the correlated variables
    temp_var <- c(Rf_2$RowName, Y_var)
    Data_temp <- Data[,c(..temp_var)]

    Rf_list = list()

    #Here let us add RFE in order to run it

    Rf <- ranger(as.formula(paste(paste(Y_var, '~'), paste(colnames(Data_temp), collapse = "+"))), data = Data_temp, mtry = ncol(Data_temp)/3, importance = 'permutation')
    Rf_2 <- data.frame(Rf$variable.importance)
    Rf_2$Var <- rownames(Rf_2)



    ##Simple 95%optimization




  # #Final RF based on RFE , based on num_features or coverage methods
  # if(#Method is based on coverage)

    Rf_2 <- na.omit(Rf_2)
    Rf_2 <- Rf_2[which(Rf_2$Rf.variable.importance  >= 0),]
    Rf_2 <- Rf_2[order(-Rf_2$Rf.variable.importance),]

    Rf_2$Rf.variable.importance <- Rf_2$Rf.variable.importance/sum(Rf_2$Rf.variable.importance)
    x1 <- cumsum(Rf_2$Rf.variable.importance)
    temp <- which(x1 > RF_coverage)[1]

    final_variables <- Rf_2[0:temp,]$Var


  # if(#method is based on num_columns)
  # {
  #   Rf_2 <- na.omit(Rf_2)
  #   Rf_2 <- Rf_2[which(Rf_2$Rf >= 0)]
  #   Rf_2 <- Rf_2[order(-Rf)]
  #
  #   Rf_2 <- Rf_2$Rf/sum(Rf_2$Rf)
  #   Rf_2 <- cumsum(Rf_2$Rf)
  #   temp <- which(Rf_2$Rf > Rf_info_cutoff)[1]
  # }

  }

  ##Plotting function for correlation methods
  if(plot == TRUE)
  {
    print('lo')
  }

  return(final_variables)

}





