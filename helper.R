# load packages
setwd("~/Dropbox/MSc BDS/Thesis/Code/Janine_Thesis")

# load packages
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("BART")) install.packages("BART")
if (!require("randomForestSRC")) install.packages("randomForestSRC")
if (!require("randomForest")) install.packages("randomForest")



# helper variables

remove_from_df = c('XUnnamed: 0',  'XUnnamed: 0.1',  'Xlevel_0',  'Xindex',
                      'X5_achterstandswijk',  'X6_dataend',  'X6_datastart',
                      'X1_datum',  'X1_einddatum',  'X2_epiid',  'X2_epistart',
                      'X2_epistartjaar',  'X2_epistop', 'X1_peildatum_DT', 
                      'X1_presid', 'X2_prevmed', 'X6_jaardeel', 'X4_geboortejaar',
                      'X0_id_client_ozps', 'X6_maxkwartaal', 'X6_minkwartaal', 'X1_nastart', 
                      'X1_seizoen', 'X1_dag', 'X1_maandag', 'X1_dinsdag', 'X1_woensdag', 'X1_donderdag',
                      'X1_vrijdag', 'X1_zaterdag', 'X1_zondag', 'X1_januari', 'X1_februari', 
                      'X1_maart', 'X1_april', 'X1_mei', 'X1_juni', 'X1_juli', 'X1_augustus', 'X1_september', 
                      'X1_oktober', 'X1_november', 'X1_december', 'X6_medicom', 'X6_geen_door_late_pres', 'X2_jaar',
                      'X5_percentiel_laagink', 'X5_percentiel_nwalloch', 'X5_percentiel_ses', 'X5_praktijk_ozps',
                      'X5_stedelijkheid', 'X0_laatste')


# stratified sampling

stratified <- function(df, group, size, select = NULL, 
                       replace = FALSE, bothSets = FALSE) {
  if (is.null(select)) {
    df <- df
  } else {
    if (is.null(names(select))) stop("'select' must be a named list")
    if (!all(names(select) %in% names(df)))
      stop("Please verify your 'select' argument")
    temp <- sapply(names(select),
                   function(x) df[[x]] %in% select[[x]])
    df <- df[rowSums(temp) == length(select), ]
  }
  df.interaction <- interaction(df[group], drop = TRUE)
  df.table <- table(df.interaction)
  df.split <- split(df, df.interaction)
  if (length(size) > 1) {
    if (length(size) != length(df.split))
      stop("Number of groups is ", length(df.split),
           " but number of sizes supplied is ", length(size))
    if (is.null(names(size))) {
      n <- setNames(size, names(df.split))
      message(sQuote("size"), " vector entered as:\n\nsize = structure(c(",
              paste(n, collapse = ", "), "),\n.Names = c(",
              paste(shQuote(names(n)), collapse = ", "), ")) \n\n")
    } else {
      ifelse(all(names(size) %in% names(df.split)),
             n <- size[names(df.split)],
             stop("Named vector supplied with names ",
                  paste(names(size), collapse = ", "),
                  "\n but the names for the group levels are ",
                  paste(names(df.split), collapse = ", ")))
    }
  } else if (size < 1) {
    n <- round(df.table * size, digits = 0)
  } else if (size >= 1) {
    if (all(df.table >= size) || isTRUE(replace)) {
      n <- setNames(rep(size, length.out = length(df.split)),
                    names(df.split))
    } else {
      message(
        "Some groups\n---",
        paste(names(df.table[df.table < size]), collapse = ", "),
        "---\ncontain fewer observations",
        " than desired number of samples.\n",
        "All observations have been returned from those groups.")
      n <- c(sapply(df.table[df.table >= size], function(x) x = size),
             df.table[df.table < size])
    }
  }
  temp <- lapply(
    names(df.split),
    function(x) df.split[[x]][sample(df.table[x],
                                     n[x], replace = replace), ])
  set1 <- do.call("rbind", temp)
  
  if (isTRUE(bothSets)) {
    set2 <- df[!rownames(df) %in% rownames(set1), ]
    list(SET1 = set1, SET2 = set2)
  } else {
    set1
  }
}


