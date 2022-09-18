#Loading libraries
library(purrr) #Find an equivalent of this here
library(dplyr)#Find data table equivlents of this

library(ranger)
library(igraph)
library(Matrix)

library(correlation)
library(data.table)
library(dummy)

#Loading the example dataset
#df_tot <- read.csv("C:\\Users\\sunny\\Downloads\\housingPrices\\train.csv")




#To do , build and test this for a more generalize approach
GeneralCor = function(df, cor1 = 'pearson', cor2 = 'PointBiserial', cor3 = 'kendall')
{

  cor_fun <- function(pos_1, pos_2, cor1, cor2,  cor3)
  {
    #Same value , we return 1
    if(all(df[[pos_1]] == df[[pos_2]]))
    {
      cor_value = 1
      return(cor_value)

    }

    #If both are numeric
    if(class(df[[pos_1]]) %in% c("integer", "numeric") && class(df[[pos_2]]) %in% c("integer", "numeric"))
    {

      if(cor1 == 'pearson')
      {
        cor_value  <- correlation::cor_test(df, x = names(df)[pos_1], y = names(df)[pos_2] , method = 'pearson')$r
      }

      if(cor1 == 'Biweight midcorrelation')
      {
        cor_value  <- correlation::cor_test(df, x = names(df)[pos_1], y = names(df)[pos_2] , method = 'Biweight')$r
      }

      if(cor1 == 'Distance')
      {
        cor_value  <- correlation::cor_test(df, x = names(df)[pos_1], y = names(df)[pos_2] , method = 'Distance')$r
      }

      if(cor1 == 'percentage')
      {
        cor_value  <- correlation::cor_test(df, x = names(df)[pos_1], y = names(df)[pos_2] , method = 'percentage')$r
      }

    }

    #If one factor and one numeric
    if(class(df[[pos_1]]) %in% c("character", "factor") && class(df[[pos_2]]) %in% c("integer", "numeric"))
    {
      if(cor2 == 'PointBiserial') #binary
      {
        cor_value  <- correlation::cor_test(df, x = names(df)[pos_1], y = names(df)[pos_2] , method = 'PointBiserial')$r
      }
    }

    #If one numeric and one factor
    if(class(df[[pos_1]]) %in% c("integer", "numeric") && class(df[[pos_2]]) %in% c("character", "factor"))
    {
      if(cor2 == 'PointBiserial')
      {
        cor_value  <- correlation::cor_test(df, x = names(df)[pos_1], y = names(df)[pos_2] , method = 'PointBiserial')$r
      }
    }

    #If both are factors
    if(class(df[[pos_1]]) %in% c("character", "factor") && class(df[[pos_2]]) %in% c("character", "factor"))
    {
      if(cor3 == 'spearman')
      {
        cor_value  <- correlation::cor_test(df, x = names(df)[pos_1], y = names(df)[pos_2] , method = 'spearman')$rho
      }

      if(cor3 == 'kendall')
      {
        cor_value  <- correlation::cor_test(df, x = names(df)[pos_1], y = names(df)[pos_2] , method = 'kendall')$tau
      }

      if(cor3 == 'ShepherdsPi')
      {
        cor_value  <- correlation::cor_test(df, x = names(df)[pos_1], y = names(df)[pos_2] , method = 'shepherd')$r
      }

      if(cor3 == 'Polychoric')
      {
        cor_value  <- correlation::cor_test(df, x = names(df)[pos_1], y = names(df)[pos_2] , method = 'polychoric')$r
      }

      if(cor3 == 'Tetrachoric')
      {
        cor_value  <- correlation::cor_test(df, x = names(df)[pos_1], y = names(df)[pos_2] , method = 'tetrachoric')$r
      }
    }

    return(cor_value)
  }

  cor_fun <- Vectorize(cor_fun)

  #Computing the matrix
  corrmat <- outer(1:ncol(df)
                   ,1:ncol(df)
                   ,function(x, y) cor_fun(x, y, cor1,  cor2, cor3)
  )

  rownames(corrmat) <- colnames(df)
  colnames(corrmat) <- colnames(df)

  return(corrmat)
}



#Testing cor1 values

# sapply(df_tot, class)
# c$OverallQual
#
# df_tot_numeric <- df_tot$
#
#
# df_tot <- as.data.table(df_tot)
#
# columns <- c('MSSubClass', 'LotArea', 'OpenPorchSF', 'Alley')
# df_tot_num <- df_tot[, ..columns]
#
# df_tot_num[is.na(Alley), Alley := "Grass"]
#
# df_tot_num <- as.data.table(cbind(dummy::dummy(df_tot_num), df_tot_num))
# df_tot_num[,Alley:=NULL]
#
# #Note that we should two flags , cor_auto = FALSE (Automatically determines the correct correlation measure between two variables)
# #Dummies = FALSE , indicating that automatic dummy creation will be done
#
#
# GeneralCor(df_tot_num, cor1 = 'Biweight midcorrelation', cor3 = 'tetrachloric')

#RandomForest needs to determine if its a classification or a regression

CorrelatedFeatures = function(Data, Y_var, Focus_variables = list(), corr_cutoff = 0.9, RF_coverage = 0.95, num_features = 5,  plot = FALSE, fast_calculation = FALSE, cor1 = 'pearson', cor2 = 'biserialcorrekation', cor3 = 'cramersV')
{
  #ToDo
  #Perform all subletting and initalizations here
  #Creating clusters based upon graph theory
  list1 = list()


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

  ut <- upper.tri(cor_matrix)
  pairs_mat <- data.frame(
    var1 = rownames(cor_matrix)[row(cor_matrix)[ut]],
    var2 = rownames(cor_matrix)[col(cor_matrix)[ut]],
    value  =(cor_matrix)[ut]
  )

  #Sub-setting values only above threshold values
  pairs_mat = pairs_mat[which(abs(pairs_mat$value) >= corr_cutoff & (pairs_mat$var1 != pairs_mat$var2)),]


  ##Start of Random Forest iterations


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
    Data_nocor <- Data[, dplyr::union(unlist(var_groups), unlist(Focus_variables)), with = FALSE] #Do I need a union in dplyr?

    ##Start of the RF
    for(i in 1:nrow(result))
    {
      Data_temp <- cbind(Data_nocor, Data[, unlist(result[i,]), with = FALSE])

      Rf <- ranger(as.formula(paste(paste(Y_var, '~'), paste(colnames(Data_temp), collapse = "+"))), data = Data_temp, mtry = ncol(Data_temp/3), importance = 'permutation')
      Rf_2 <- data.frame(Rf$variable.importance)
      RF_list[[i]] <- Rf_2
    }

  }else
  {
    Rf <- ranger(as.formula(paste(paste(Y_var, '~'), paste(colnames(Data_temp), collapse = "+"))), data = Data_temp, mtry = ncol(Data_temp/3), importance = 'permutation')
    Rf_2 <- data.frame(Rf$variable.importance)
    RF_list[[i]] <- Rf_2
  }

  #Fast Aggregation across multiple frames
  l <- lapply(RF_list, function(x) {x$Rowname <- row.names(x) ; x})
  Res <- Reduce(function(...) merge(..., by = 'RowName', all = TRUE), l)

  Rf_2 <- dcast(melt(setDT(Res), "RowName"),
                RowName ~ sub("\\..*","",variable),
                mean,
                na.rm = TRUE,
                value.var = "value")

  #Taking the best variable from each group
  if(dim(pairs_mat)[1] != 0)
  {
    for(bv in 1:nrow(var_groups))
    {
      comp <- var_groups[bv]
      comp <- unlist(strsplit(comp[[1]]), '"')# Change is needed?
      temp <- Rf_2[which(Rf_2$Rowname %in% comp)]
      keep_var <- Rf_2$RowName[Rf_2$Rf == max(temp$Rf)]
      rem_var <- comp[which(comp != keep_var)]

      #Dropping all values not needed
      Rf_2 <- Rf_2[!(Rf_2$RowName %in% unlist(rem_var))]
    }

    #Fitting the RF without the correlated variables
    temp_var <- c(Rf_2$RowName, 'Y_var')
    Data_temp <- Data[,c(...temp_var)]

    Rf_list = list()

    Rf <- ranger(as.formula(paste(paste(Y_var, '~'), paste(colnames(Data_temp), collapse = "+"))), data = Data_temp, mtry = ncol(Data_temp/3), importance = 'permutation')
    Rf_2 <- data.frame(Rf$variable.importance)
    RF_list[[1]] <- Rf_2

    l <- lapply(RF_list, function(x) {x$Rowname <- row.names(x) ; x})
    Res <- Reduce(function(...) merge(..., by = 'RowName', all = TRUE), l)

    Rf_2 <- dcast(melt(setDT(Res), "RowName"),
                  RowName ~ sub("\\..*","",variable),
                  mean,
                  na.rm = TRUE,
                  value.var = "value")
  }


  # #Final RF based on RFE , based on num_features or coverage methods
  # if(#Method is based on coverage)
  # {
  #     Rf_2 <- na.omit(Rf_2)
  #     Rf_2 <- Rf_2[which(Rf_2$Rf >= 0)]
  #     Rf_2 <- Rf_2[order(-Rf)]
  #
  #     Rf_2 <- Rf_2$Rf/sum(Rf_2$Rf)
  #     Rf_2 <- cumsum(Rf_2$Rf)
  #     temp <- which(Rf_2$Rf > Rf_info_cutoff)[1]
  #   }
  #
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



  ##Plotting function for correlation methods
  if(plot == TRUE)
  {
    print('lo')
  }

}


#After the variables are built without the correlated features , should we do RFE to remove the variables ?
#Let us do RFE , cumsum removal ,removing the lowest variable until we obtain the required coverage ?
#Add gini and other things?
#RFE based on the number of featurs or the coverage




#Things to do
#2. Finish and test the actual package , can it handle both classification and regression equations ?
#1. Embedd that dataset
