#Loading the example dataset

#' Housing prices dataset
#' @name Housing_Prices_dataset
#' @keywords datasets
NULL

#' Advertisement dataset
#' @name Advertisement
#' @keywords datasets
NULL

#' Automatic Data Cleaning
#'
#' @param Data The imported Data Frame
#' @param Y_var The X variable
#'
#' @return The cleaned data.
#' @examples
#' DataCleaning(Data = TangledFeatures::Housing_Prices_dataset,  Y_var = 'SalePrice')

#' @importFrom data.table :=
#' @importFrom data.table .SD
#' @export
#'

DataCleaning <- function(Data, Y_var)
{
  #Ordinal Encoding function
  encode_ordinal <- function(x, order = unique(x)) {
    x <- as.numeric(factor(x, levels = order, exclude = NULL))
    x
  }

  if(!is.data.frame(Data)) stop("Value needs to be a data frame or data table")

  #Coerce to data table drop NA
  Data <- data.table::as.data.table(Data)
  Data[is.na(Data), ] <- 0

  #Storing column position for the dependent variable to see if it has changed
  Y_var_pos <- which(colnames(Data) == Y_var)

  #Remove unclean names
  names(Data) <- make.names(names(Data), unique=TRUE)
  Data <- janitor::clean_names(Data)

  #Storing the new column dependent value
  New_Yvar <- colnames(Data)[Y_var_pos]

  #Make all characters factors
  changeCols_char <- colnames(Data)[which(as.vector(Data[,lapply(.SD, class)]) == "character")]
  if(length(changeCols_char) != 0)
  {
    Data[,(changeCols_char):= lapply(.SD, as.factor), .SDcols = changeCols_char]
  }

  #If there is a order to it, use ordinal encoding, note please change this
  for(i in seq(ncol(Data)))
  {
    if(is.ordered(Data[[i]]))
    {
      Data[,i] <- encode_ordinal(Data[[i]])
    }
  }

  #Dummy creation of columns that are unordered factors
  if(length(changeCols_char) != 0)
  {
    Data <- fastDummies::dummy_cols(Data)
  }

  Data <- janitor::clean_names(Data)

  #Coerce to numeric data type
  changeCols_num <- colnames(Data)[which(as.vector(Data[,lapply(.SD, class)]) == "integer")]
  if(length(changeCols_num) != 0)
  {
    Data[,(changeCols_num):= lapply(.SD, as.numeric), .SDcols = changeCols_num]
  }

  #Dropping the previous columns
  Data[, (changeCols_char) := NULL]

  #numeric type creation of ordered factor columns
  Data_Results <- list('Cleaned_Data' = Data, 'New_Dependent' = New_Yvar)

  return(Data_Results)
}

#' Generalized Correlation function
#'
#' @param df The imported Data Frame
#' @param cor1 The correlation metric between two continuous features. Defaults to pearson
#' @param cor2 The correlation metric between one categorical feature and one cont feature. Defaults to biserial
#' @param cor3 The correlation metric between two categorical features. Defaults to Cramers-V
#' @return Returns a correlation matrix containing the correlation values between the features

#' @examples
#' GeneralCor(df = TangledFeatures::Advertisement)
#' @export
#'

GeneralCor <- function(df, cor1 = 'pearson', cor2 = 'polychoric', cor3 = 'spearman')
{
  cor_value <- NULL

  cor_fun <- function(pos_1, pos_2)
  {

    #Same value , we return 1
    if(identical(df[[pos_1]], df[[pos_2]]) || pos_1 == pos_2)
    {

      cor_value <- 1
      return(cor_value)
    }

    #If both numeric
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
      if(cor3 == 'spearman') #binary
      {
        cor_value  <- correlation::cor_test(df, x = names(df)[pos_1], y = names(df)[pos_2] , method = 'spearman')$r
      }
    }

    #If one is factor and another is numeric
    if(class(df[[pos_1]])[1] %in% c("numeric") && class(df[[pos_2]])[1] %in% c("factor") || class(df[[pos_1]])[1] %in% c("factor") && class(df[[pos_2]])[1] %in% c("numeric"))
    {
      if(cor2 == 'polychoric') #binary
      {
        cor_value  <- correlation::cor_test(df, x = names(df)[pos_1], y = names(df)[pos_2] , method = 'polychoric')$r
      }

      if(cor2 == 'PointBiserial') #binary
      {
        cor_value  <- correlation::cor_test(df, x = names(df)[pos_1], y = names(df)[pos_2] , method = 'biserial')$r
      }
    }

    #for ordered factors
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
  corrmat <- outer(seq(ncol(df))
                   ,seq(ncol(df))
                   ,function(x, y) cor_fun(pos_1 = x, pos_2 = y))


  rownames(corrmat) <- colnames(df)
  colnames(corrmat) <- colnames(df)

  return(corrmat)
}

#' The main TangledFeatures function
#'
#' @param Data The imported Data Frame
#' @param Y_var The dependent variable
#' @param Focus_variables The list of variables that you wish to give a certain bias to in the correlation matrix
#' @param corr_cutoff The correlation cutoff variable. Defaults to 0.8
#' @param RF_coverage The Random Forest coverage of explainable. Defaults to 95 percent
#' @param plot Return if plotting is to be done. Binary True or False
#' @param fast_calculation Returns variable list without many Random Forest iterations by simply picking a variable from a correlated group
#' @param cor1 The correlation metric between two continuous features. Defaults to pearson correlation
#' @param cor2 The correlation metric between one categorical feature and one continuous feature. Defaults to bi serial correlation correlation
#' @param cor3 The correlation metric between two categorical features. Defaults to CramersV correlation
#'
#' @return Returns a list of variables that are ready for future modelling, along with other metrics
#' @examples
#' TangledFeatures(Data = TangledFeatures::Advertisement, Y_var = 'Sales')

#' @importFrom methods as
#' @importFrom stats as.formula
#' @importFrom stats na.omit
#' @importFrom data.table as.data.table
#' @importFrom data.table dcast
#' @importFrom data.table melt
#' @importFrom data.table setDT
#' @importFrom ranger ranger
#' @importFrom igraph graph_from_adjacency_matrix
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_tile
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 scale_fill_gradient2
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_blank

#' @export
#'
TangledFeatures <- function(Data, Y_var, Focus_variables = list(), corr_cutoff = 0.7, RF_coverage = 0.95, plot = FALSE, fast_calculation = FALSE, cor1 = 'pearson', cor2 = 'polychoric', cor3 = 'spearman')
{
  #ToDo
  #Perform all subletting and initialization here
  #Creating clusters based upon graph theory

  list1 <- list()
  Results <- list()
  var1 <- NULL
  var2 <- NULL
  temp_var <- NULL
  value <- NULL
  connects <- c()

  #Data Cleaning
  DataCleanRes <- DataCleaning(Data, Y_var = Y_var)

  #Updating the values after cleaning
  Y_var <- DataCleanRes$New_Dependent
  Data <- DataCleanRes$Cleaned_Data

  #Note to add all data checks that are needed in the system

  #If any NA values drop it
  Data[is.na(Data), ] <- 0

  #If any value is not a numeric or factor , we drop it
  class_check <- data.table::as.data.table(vapply(Data, class, 'character'))
  if(any(class_check$V1 %in% 'character'))
  {
    warning('Please convert all chracter variables to factors or dummies')
    return(Results)
  }

  Data <- as.data.table(Data) #Redundant steps?

  ###Correlation matrix creation
  #Examine this further of course
  cor_matrix <- GeneralCor(Data[, -Y_var, with = FALSE])
  cor_matrix[cor_matrix == 'NULL'] <- 0

  ut <- upper.tri(cor_matrix)
  pairs_mat_total <- data.frame(
    var1 = rownames(cor_matrix)[row(cor_matrix)[ut]],
    var2 = colnames(cor_matrix)[col(cor_matrix)[ut]],
    value = unlist((cor_matrix)[ut])
  )

  #Sub-setting values only above threshold values
  pairs_mat <- pairs_mat_total[which(abs(pairs_mat_total$value) >= corr_cutoff & (pairs_mat_total$var1 != pairs_mat_total$var2)),]
  rownames(pairs_mat) <- NULL


  ##Start of Random Forest iterations

  list1 <- list()

  if(dim(pairs_mat)[1] != 0)
  {
    for(j in seq(nrow(pairs_mat)))
    {
      list1[[j]] <- c(pairs_mat[j,1], pairs_mat[j,2])
      list1[[j]] <- sort(list1[[j]])
    }

    i <- rep(seq(length(list1)), lengths(list1))
    j <- factor(unlist(list1))
    tab <- Matrix::sparseMatrix(i = i , j = as.integer(j), x = TRUE, dimnames= list(NULL, levels(j)))
    connects <- Matrix::tcrossprod(tab, boolArith = TRUE)
    group <- igraph::clusters(igraph::graph_from_adjacency_matrix(as(connects, "lMatrix")))$membership
    var_groups <- tapply(list1, group, function(x) sort(unique(unlist(x))))
    var_groups <- as.list(var_groups)

    #Condition to extract the focus variables
    for(i in seq(length(var_groups)))
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
    if(fast_calculation == TRUE || prod(vapply(var_groups, length, length(var_groups))) > 100)
    {
      result <- purrr::map(var_groups, 1)
      result <- as.data.frame(t(unlist(result)))
    }else
    {
      result <- expand.grid(var_groups)
    }

    RF_list <- list()

    noncor_columns <- colnames(Data)[! colnames(Data) %in% unlist(var_groups)]
    Data_nocor <- Data[,noncor_columns, with = FALSE]

    ##Start of the RF, note we need to add multiprocessing here
    for(i in seq(nrow(result)))
    {
      Data_temp <- cbind(Data_nocor, Data[, unlist(result[i,]), with = FALSE])

      Rf <- ranger::ranger(as.formula(paste(paste(Y_var, '~'), paste(colnames(Data_temp), collapse = "+"))), data = Data_temp, mtry = ncol(Data_temp/3), importance = 'permutation')
      Rf_2 <- data.frame(Rf$variable.importance)
      RF_list[[i]] <- Rf_2
    }

  }else
  {
    RF_list <- list()

    Rf <- ranger(as.formula(paste(paste(Y_var, '~'), paste(colnames(Data[, -Y_var, with = FALSE]), collapse = "+"))), data = Data, mtry = ncol(Data[, -Y_var, with = FALSE]/3), importance = 'permutation')
    Rf_2 <- data.frame(Rf$variable.importance)
    RF_list[[1]] <- Rf_2
  }


  # #Fast Aggregation across multiple frames
  l <- lapply(RF_list, function(x) {x$RowName <- row.names(x) ; x})
  Res <- Reduce(function(...) merge(..., by = "RowName", all = TRUE), l)

  Rf_2 <- dcast(melt(setDT(Res), "RowName"),
                RowName ~ sub("\\..*", "", variable),
                mean,
                na.rm = TRUE,
                value.var = "value")


  # #Taking the best variable from each group
  if(dim(pairs_mat)[1] != 0)
  {
    for(bv in seq(nrow(var_groups)))
    {
      comp <- var_groups[bv]
      comp <- unlist(comp[[1]])
      temp <- Rf_2[which(Rf_2$RowName %in% comp)]

      keep_var <- Rf_2$RowName[Rf_2$Rf == max(temp$Rf)][1]
      rem_var <- comp[which(comp != keep_var)]

      #Dropping all values not needed
      Rf_2 <- Rf_2[!(Rf_2$RowName %in% unlist(rem_var))]
    }

    #Fitting the RF without the correlated variables
    temp_var <- c(Rf_2$RowName, Y_var)
    Data_temp <- Data[,temp_var, with = FALSE]

    Rf_list <- list()

    #Here let us add RFE in order to run it
    Rf <- ranger(as.formula(paste(paste(Y_var, '~'), paste(colnames(Data_temp), collapse = "+"))), data = Data_temp, mtry = ncol(Data_temp)/3, importance = 'permutation')
    Rf_2 <- data.frame(Rf$variable.importance)
    Rf_2$Var <- rownames(Rf_2)



    ##Simple 95%optimization

    #To ADD: RFE with cv based methods


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

  }else
  {
    var_groups <- c()
    Rf <- ranger(as.formula(paste(paste(Y_var, '~'), paste(colnames(Data[, -Y_var, with = FALSE]), collapse = "+"))), data = Data, mtry = ncol(Data[, -Y_var, with = FALSE]/3), importance = 'permutation')
    Rf_2 <- data.frame(Rf$variable.importance)
    Rf_2$Var <- rownames(Rf_2)

    ##Simple 95%optimization

    #To ADD: RFE with cv based methods


    # #Final RF based on RFE , based on num_features or coverage methods
    # if(#Method is based on coverage)

    Rf_2 <- na.omit(Rf_2)
    Rf_2 <- Rf_2[which(Rf_2$Rf.variable.importance  >= 0),]
    Rf_2 <- Rf_2[order(-Rf_2$Rf.variable.importance),]

    Rf_2$Rf.variable.importance <- Rf_2$Rf.variable.importance/sum(Rf_2$Rf.variable.importance)
    x1 <- cumsum(Rf_2$Rf.variable.importance)
    temp <- which(x1 > RF_coverage)[1]

    final_variables <- Rf_2[0:temp,]$Var

  }

  ##Plotting function for correlation methods
  if(plot == TRUE)
  {
    if(length(connects) != 0)
    {
      #Heat map of the correlation
      heatmap <- ggplot2::ggplot(data = pairs_mat_total, aes(x=var1, y=var2, fill=value)) +
        geom_tile() +
        scale_fill_gradient2(low = "blue", high = "green",
                             limit = c(-1,1), name = "Correlation") +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              panel.background = element_blank())

      #Correlation information matrix generation
      heatmap <- ggplot2::ggplot(data = pairs_mat_total, aes(x=var1, y=var2, fill=value)) +
        geom_tile() +
        scale_fill_gradient2(low = "blue", high = "green",
                             limit = c(-1,1), name = "Correlation") +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              panel.background = element_blank())


      #Graph object of the correlation
      igraph_plot <- graph_from_adjacency_matrix(as(connects, "lMatrix"))
    }else
    {
      heatmap <- c()
      igraph_plot <- c()
    }


  }else
  {
    heatmap <- c()
    igraph_plot <- c()
  }

  Results <- list('Final_Variables' = final_variables, 'Variable_groups' = var_groups,
                  'Correlation_heatmap' = heatmap,'Graph_plot' = igraph_plot)

  return(Results)

}
