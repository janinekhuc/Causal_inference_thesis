# load packages
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("BART")) install.packages("BART")
if (!require("randomForestSRC")) install.packages("randomForestSRC")
if (!require("randomForest")) install.packages("randomForest")
if (!require("splitstackshape")) install.packages("splitstackshape")


# helper variables & preprocessing

remove_from_df = c('X',  'Unnamed..0',  'level_0',  'index',
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
remove_from_df = remove_from_df %>% str_replace("X", "")

# coefficients
cipro_coef <- read.csv("cipro_coefficients_2way.csv", 
                       check.names = FALSE)
nitro_coef <- read.csv("nitro_coefficients_2way.csv", 
                       check.names = FALSE)
nitro_coef$`0` = round(nitro_coef$`0`, 3)
cipro_coef$`0` = round(cipro_coef$`0`, 3)


nitro_coef$stand_agresti = round(nitro_coef$stand_agresti, 3)
cipro_coef$stand_agresti = round(cipro_coef$stand_agresti, 3)

cipro_top = cipro_coef[order(cipro_coef$`0`, decreasing = TRUE)[1:10],]
cipro_bot = cipro_coef[order(cipro_coef$`0`, decreasing = FALSE)[1:10],]
cipro_coef_most = rbind(cipro_top, cipro_bot)

nitro_top = nitro_coef[order(nitro_coef$`0`, decreasing = TRUE)[1:10],]
nitro_bot = nitro_coef[order(nitro_coef$`0`, decreasing = FALSE)[1:10],]
nitro_coef_most = rbind(nitro_top, nitro_bot)


# create outcoome functions
c_2_col = apply(nitro_coef_most[,c("0", "index")], 1, paste, collapse="*data$`")
two_col = gsub('\\s', '+', c_2_col)
paste(two_col, collapse = '` ')


