#STOCHASTIC CONTROL ---------------------------------------------------------------------------------

#Clear Memory
rm(list = ls())

#Library Download
library(data.table)     #Data Manipulation
library(dummies)        #Data Manipulation - dummy representation
library(tibble)         #Data Manipulation - row_to_column operator
library(dplyr)		      #Data Manipulation - Pipe Operator %>%
library(readxl)		      #Connection with Excel
library(pracma)         #Calculate Hypervolume
library(e1071)          #Calculate Skewness 
library(rpart)          #Regression Tree - Train
library(rpart.plot)     #Regression Tree - Plot
library(gramEvol)       #Symbolic - Generate random expression

#FUNCTION -------------------------------------------------------------------------------------------

#Support
F_Z_01_FROM_VEC_TO_LIST<-function(T_NN_WEIGHT, LR, TP){
  
  #Reference
  R<-ifelse(TP == "BIAS", 1, T_NN_WEIGHT[NN_LAYER == LR &  TYPE == TP, max(NN_ROW)])  #Layer number of rows
  C<-ifelse(TP == "BIAS", 1, T_NN_WEIGHT[NN_LAYER == LR &  TYPE == TP, max(NN_COL)])  #Layer number of cols
  
  #Transformation
  out<-T_NN_WEIGHT[
                   #Filter column to process
                   NN_LAYER == LR, c("NETWORK", "TYPE","VALUE")][, 
                  
                   #Normalize network weight/bias                                              
                   MN := mean(VALUE), keyby = .(NETWORK)][,                           #Layer mean
                   SD := sd(VALUE),   keyby = .(NETWORK)][,                           #Layer standard deviation
                   WB := (VALUE - MN) / SD][                                          #Normalized weight and bias
                  
                   #Keep only network index and normalized weight/bias
                   TYPE == TP, c("NETWORK", "WB")] %>%
                  
                   #Convert table into list of matrices
                   group_split(NETWORK) %>%                                           #Create list for each network
                   lapply(., function(x) x[,"WB"]) %>%                                #Select column value
                   lapply(., function(x) unlist(x)) %>%                               #Convert into single vector
                   lapply(., function(x) matrix(data = x, nrow = R, ncol = C))        #Reshape into matrix format
            
  #Output
  return(out)
  
}
F_Z_02_FROM_REF_TO_EXPR<-function(r, c, T_SETTING_RUN, T_SETTING_ENVIRONMENT, T_SR_STRUCTURE, T_NN_LIBRARY, T_SR_WEIGHT){
  
  #Reference
  RUN_SEED<-T_SETTING_RUN[RUN_INDEX == r, RUN_SEED]                                                               #Random number generator seed  
  N<-T_SR_WEIGHT[CLASS == c, max(NETWORK)]                                                                        #Number of networks
  L<-T_SETTING_ENVIRONMENT[RUN_INDEX == r, max(ENV_LOB)]                                                          #Number of LoBs
  E<-T_SR_STRUCTURE[RUN_INDEX == r, RULE_COMPONENT_NUMBER]                                                        #Number of expressions to aggregate
  P<-T_SR_STRUCTURE[RUN_INDEX == r, RULE_FLOAT_PRECISION]                                                         #Float precision
  VARIABLE<-c("ACT_SURPLUS", "ACT_RISK_APPETITE", "ACT_PREMIUM", "ACT_RI", "ACT_DIVIDEND")                        #Control variables
  
  #Data Container
  T_W<-copy(T_SR_WEIGHT[CLASS == c])                                                                              #Network weights
  out<-CJ(CLASS = c, NETWORK = 1:N, VARIABLE = VARIABLE, EXPRESSION = "", FUNCTION = "")                          #Create reference
  
  #Transformation
  for(n in 1:N){                                                                                                  #For each network
    
    for(v in VARIABLE){                                                                                           #For each control variable
      
      #Generate Control Rule - Bias  
      B_CR<-"0"                                                                                                   #Initialize rule text - BIAS
      
      for(l in 1:L){                                                                                              #For each lob - BIAS
        B_CF<-T_W[NETWORK == n & VARIABLE == v & TYPE == "BIAS" & INDEX == l, VALUE] %>% round(., P)              #Retrieve coefficient - BIAS
        B_CR<-paste(B_CR, " + NN_LOB_", l, " * ", B_CF, sep = "")                                                 #Combine elements - BIAS
      }
      
      B_CR<-substr(x = B_CR, start = 5, stop = nchar(B_CR))                                                       #Eliminate initialization root - BIAS
      B_CR<-paste("(", B_CR, ")", sep = "")                                                                       #Generate rule - BIAS
      
      #Generate Control Rule - Expression
      E_CR<-"0"                                                                                                   #Initialize rule text - EXPRESSION
      
      for(e in 1:E){                                                                                              #For each expressions to aggregate - EXPRESSION
        E_CF<-T_W[NETWORK == n & VARIABLE == v & TYPE == "COEFFICIENT" & INDEX == e, VALUE] %>% round(., P)       #Retrieve coefficient - EXPRESSION
        E_NR<-T_W[NETWORK == n & VARIABLE == v & TYPE == "EXPRESSION"  & INDEX == e, VALUE]                       #Retrieve symbolic expression - reference - EXPRESSION
        E_TX<-T_NN_LIBRARY[VARIABLE == v & TYPE == "EXPRESSION" & INDEX == E_NR, RULE]                            #Retrieve symbolic expression - symbolic text - EXPRESSION
        E_CR<-paste(E_CR, " + ", E_CF, " * ", " ( ", E_TX, " ) ", sep = "")                                       #Combine elements - EXPRESSION
      }
      
      E_CR<-substr(x = E_CR, start = 5, stop = nchar(E_CR))                                                       #Eliminate initialization root - EXPRESSION
      E_CR<-paste("(", E_CR, ")", sep = "")                                                                       #Generate rule - EXPRESSION
      
      #Generate Control Rule - Full Expression
      RULE_E<-paste(B_CR, " + ", E_CR, sep = "")
      
      #Generate Control Rule - Function
      FUNC_N<-T_W[NETWORK == n & VARIABLE == v & TYPE == "FUNCTION"  & INDEX == 1, VALUE]                         #Retrieve symbolic expression - reference - FUNCTION
      FUNC_T<-T_NN_LIBRARY[VARIABLE == v & TYPE == "FUNCTION" & INDEX == 1, RULE]                                 #Retrieve symbolic expression - symbolic text - FUNCTION
      RULE_F<-gsub("INPUT", paste("TEMP_", v, sep = ""), as.character(FUNC_T))                                    #Generate rule - FUNCTION
      
      #Store results
      out[NETWORK == n & VARIABLE == v, EXPRESSION := RULE_E]                 
      out[NETWORK == n & VARIABLE == v, FUNCTION   := RULE_F]
      
    }
    
  }
  
  out<-setkey(out, NETWORK)                                                                                       #Set primary key
  
  #Output
  return(out)
  
}
F_Z_03_NON_LINEARITY<-function(x){
  
  #Transformation
  ( 1 + exp( -x ) ) ^ -1

}
F_Z_04_MM<-function (...) Reduce("%*%", list(...))
F_Z_05_GRAPH_1<-function(CASE, r, g, RUN_TYPE, OPT_TYPE, COL, L_NN_POPULATION){
  
  #Graph - F_C_06_RUN | OPTIMIZATION SEARCH
  if(CASE == 1){
    G_NAME<-paste("RUN_", r, " - ", RUN_TYPE, " - ", OPT_TYPE, " - GENERATION_", g, sep = "")
    jpeg(paste(G_NAME, ".jpg", sep = ""))
    L_NN_POPULATION$T_NN_SCORE[, .(RISK, RETURN)] %>% plot(., main = G_NAME, xlim = c(0, 1), ylim = c(0, 2.5))
    L_NN_POPULATION$T_NN_SCORE[CLASS == "ARCHIVE"][order(RISK,RETURN), .(RISK, RETURN)] %>% lines(., type = "s", col = COL)
    dev.off()
  }
  
  #Graph - F_C_06_RUN | ARCHIVE
  if(CASE == 2){
    G_NAME<-paste("RUN_", r, " - ", RUN_TYPE, " - ", OPT_TYPE, " - ARCHIVE", sep = "")
    jpeg(paste(G_NAME, ".jpg", sep = ""))
    L_NN_POPULATION$T_NN_SCORE[, .(RISK, RETURN)] %>% plot(., main = G_NAME, xlim = c(0,1), ylim = c(0, 2.5))
    L_NN_POPULATION$T_NN_SCORE[order(RISK,RETURN), .(RISK, RETURN)] %>% lines(., type = "s", col = COL)
    dev.off()
  }  

}
F_Z_06_GRAPH_2<-function(CASE, r, RUN_TYPE, T_A_SCORE, T_B_SCORE){

  #Graph - F_E_01_RUN | COMPARISON: OPT_TYPE
  if(CASE == 1){
    G_NAME<-paste("RUN_", r, " - ", RUN_TYPE, " - FRONTIER COMPARISON - OPTIMIZATION", sep = "")
    jpeg(paste(G_NAME, ".jpg", sep = ""))
    T_A_SCORE[order(RISK, RETURN), c("RISK","RETURN")] %>% plot(xlim = c(0,1),  ylim = c(0, 2.5), col = "blue", type = "s", main = G_NAME)
    T_B_SCORE[order(RISK, RETURN), c("RISK","RETURN")] %>% lines(xlim = c(0,1), ylim = c(0, 2.5), col = "red",  type = "s")
    legend('topright', c("ADEMO", "UMCS"), lty = 1, col = c('blue', 'red'), bty = 'n', cex = .75)
    dev.off()
  }

  #Graph - F_E_01_RUN | COMPARISON: RUN_TYPE
  if(CASE == 2){
    G_NAME<-paste("RUN_", r, " - FRONTIER COMPARISON - APPROACH", sep = "")
    jpeg(paste(G_NAME, ".jpg", sep = ""))
    T_A_SCORE[order(RISK, RETURN), c("RISK","RETURN")] %>% plot(xlim = c(0,1),  ylim = c(0, 2.5), col = "blue", type = "s", main = G_NAME)
    T_B_SCORE[order(RISK, RETURN), c("RISK","RETURN")] %>% lines(xlim = c(0,1), ylim = c(0, 2.5), col = "red",  type = "s")
    legend('topright', c("NUMERIC", "SYMBOLIC"), lty = 1, col = c('blue', 'red'), bty = 'n', cex = .75)
    dev.off()
  }

}
F_Z_07_GRAPH_3<-function(r, n, j, V_O, L_DT_SINGLE){
  
  #Graph - F_D_05_IMITATE | REGRESSION TREE
  G_NAME<-paste("RUN_", r, " - AGENT_", n, " - VARIABLE_", j, " - ", V_O[j], sep = "")
  jpeg(paste(G_NAME, ".jpg", sep = ""))
  rpart.plot(L_DT_SINGLE[[j]], main = G_NAME)
  dev.off()
  
}
F_Z_08_STORE<-function(r, T_RUN_RESULT, L_NN_ES_POPULATION, L_NN_MS_POPULATION, L_SR_ES_POPULATION, L_SR_MS_POPULATION, T_NN_COMPARE, T_NN_ANALYZE_WEIGHT, T_NN_ANALYZE_POLICY, L_DT_ES_STATISTICS){
  
  #Reference
  COL_NAME<-c("RUN_INDEX", "TABLE", "RUN_TYPE", "OPT_TYPE", "CLASS", "NETWORK", "ENV_LOB", "VARIABLE", "TYPE", "INDEX", 
              "RETURN", "RISK", "PARETO", "CROWD", 
              "NN_LAYER", "NN_ROW", "NN_COL",  
              "EXPRESSION", "FUNCTION", "VALUE",  
              "HYPERVOLUME", "CARDINALITY", "SPACING", "DOMINANCE", "RANGE", 
              "MEAN", "SD", "SKEWNESS", "MIN", "MAX", "Q25", "Q50", "Q75", "SUM", "COV", 
              "VARIABLE_OUT", "VARIABLE_IN", "N_SPLIT", "ERROR_REAL", "ERROR_X", "X_STD", "IMPORTANCE")
  
  #Transformation - Create master table
  T_TEMP<-rbindlist( list(
    
                          #T_NN_SCORE
                          copy(L_NN_ES_POPULATION$T_NN_SCORE)  %>% .[, RUN_INDEX := r] %>% .[, RUN_TYPE := "NN"] %>% .[, OPT_TYPE := "ADEMO"] %>% .[, TABLE := "T_NN_SCORE"],
                          copy(L_NN_MS_POPULATION$T_NN_SCORE)  %>% .[, RUN_INDEX := r] %>% .[, RUN_TYPE := "NN"] %>% .[, OPT_TYPE := "UMCS"]  %>% .[, TABLE := "T_NN_SCORE"],
                          copy(L_SR_ES_POPULATION$T_NN_SCORE)  %>% .[, RUN_INDEX := r] %>% .[, RUN_TYPE := "SR"] %>% .[, OPT_TYPE := "ADEMO"] %>% .[, TABLE := "T_NN_SCORE"],
                          copy(L_SR_MS_POPULATION$T_NN_SCORE)  %>% .[, RUN_INDEX := r] %>% .[, RUN_TYPE := "SR"] %>% .[, OPT_TYPE := "UMCS"]  %>% .[, TABLE := "T_NN_SCORE"],
                          
                          #T_XX_WEIGHT
                          copy(L_NN_ES_POPULATION$T_NN_WEIGHT) %>% .[, RUN_INDEX := r] %>% .[, RUN_TYPE := "NN"] %>% .[, OPT_TYPE := "ADEMO"] %>% .[, TABLE := "T_NN_WEIGHT"],
                          copy(L_NN_MS_POPULATION$T_NN_WEIGHT) %>% .[, RUN_INDEX := r] %>% .[, RUN_TYPE := "NN"] %>% .[, OPT_TYPE := "UMCS"]  %>% .[, TABLE := "T_NN_WEIGHT"],
                          copy(L_SR_ES_POPULATION$T_SR_WEIGHT) %>% .[, RUN_INDEX := r] %>% .[, RUN_TYPE := "SR"] %>% .[, OPT_TYPE := "ADEMO"] %>% .[, TABLE := "T_SR_WEIGHT"],
                          copy(L_SR_MS_POPULATION$T_SR_WEIGHT) %>% .[, RUN_INDEX := r] %>% .[, RUN_TYPE := "SR"] %>% .[, OPT_TYPE := "UMCS"]  %>% .[, TABLE := "T_SR_WEIGHT"],
                          
                          #T_SR_EXPRESSION
                          copy(L_SR_ES_POPULATION$T_NN_EXPRESSION) %>% .[, RUN_INDEX := r] %>% .[, RUN_TYPE := "SR"] %>% .[, OPT_TYPE := "ADEMO"] %>% .[, TABLE := "T_NN_EXPRESSION"],
                          copy(L_SR_MS_POPULATION$T_NN_EXPRESSION) %>% .[, RUN_INDEX := r] %>% .[, RUN_TYPE := "SR"] %>% .[, OPT_TYPE := "UMCS"]  %>% .[, TABLE := "T_NN_EXPRESSION"],
                          
                          #T_NN_COMPARE
                          copy(T_NN_COMPARE) %>% .[, TABLE := "T_NN_COMPARE"],
                          
                          #T_NN_ANALYZE_WEIGHT
                          copy(T_NN_ANALYZE_WEIGHT) %>% .[, RUN_TYPE := "NN"] %>% .[, OPT_TYPE := "ADEMO"] %>% .[, TABLE := "T_NN_ANALYZE_WEIGHT"],
                          
                          #T_NN_ANALYZE_POLICY
                          copy(T_NN_ANALYZE_POLICY) %>% .[, RUN_TYPE := "NN"] %>% .[, OPT_TYPE := "ADEMO"] %>% .[, TABLE := "T_NN_ANALYZE_POLICY"],
                          
                          #T_DT_ERROR
                          copy(L_DT_ES_STATISTICS$T_DT_ERROR) %>% as.data.table %>% .[, RUN_TYPE := "DT"] %>% .[, OPT_TYPE := "ADEMO"] %>% .[, TABLE := "T_DT_ERROR"],
                          
                          #T_DT_IMPORTANCE
                          copy(L_DT_ES_STATISTICS$T_DT_IMPORTANCE) %>% as.data.table %>% .[, RUN_TYPE := "DT"] %>% .[, OPT_TYPE := "ADEMO"] %>% .[, TABLE := "T_DT_IMPORTANCE"] ), 
                        
                     fill = TRUE) %>%
                          
          .[, ..COL_NAME]
  
  #Transformation - Create output table
  out<-if(r == 1){T_TEMP}else{rbind(T_RUN_RESULT, T_TEMP)} %>% as.data.table(.)
  
  #Transformation - Download result
  write.table(out, file = paste("RUN_", r, " - T_RUN_RESULT.csv", sep = ""), row.names = FALSE, dec = ".", sep = ";", quote = FALSE)
  
  #Output
  return(out)
  
}

#Initialization
F_A_01_LIBRARY<-function(r, T_SETTING_RUN, T_SETTING_ENVIRONMENT, T_SR_STRUCTURE){
  
  #Reference
  RUN_SEED<-T_SETTING_RUN[RUN_INDEX == r, RUN_SEED]                                      #Random number generator seed  
  L<-T_SETTING_ENVIRONMENT[RUN_INDEX == r, max(ENV_LOB)]                                 #Number of LoBs
  E<-T_SR_STRUCTURE[RUN_INDEX == r, RULE_COMPONENT_NUMBER]                               #Number of expressions to aggregate
  D<-T_SR_STRUCTURE[RUN_INDEX == r, RULE_COMPONENT_DEPTH]                                #Single expression max depth
  R<-T_SR_STRUCTURE[RUN_INDEX == r, RULE_RANDOM_NUMBER]                                  #Number of random expressions to generate
  
  #Data Container
  LIBRARY_E_B  <-list()                                                                  #Basic expression
  LIBRARY_E_EXP<-list()                                                                  #Advanced expression: exp
  
  #Reproducibility
  set.seed(RUN_SEED + r)
  
  #Transformation - Create Bias Library
  LIBRARY_B<-CJ(TYPE = "BIAS",        INDEX = 1:L, RULE = "NULL", MEAN = 0, SD = 1)
  
  #Transformation - Create Coefficient Library
  LIBRARY_C<-CJ(TYPE = "COEFFICIENT", INDEX = 1:E, RULE = "NULL", MEAN = 0, SD = 1)
  
  #Transformation - Create Expression Library
  RULE<-list(expr = grule(op(expr, expr), var),                                                                                       #Define expression structure
             op   = grule(`+`, `-`, `*`),                                                                                             #Define basic operator
             var  = grule(NN_A_EXP, NN_A_RESULT, NN_A_SOLVENCY, NN_A_SURPLUS, NN_L_EXP, NN_L_RESULT, NN_L_SOLVENCY, NN_L_SURPLUS))    #Define variables name
  
  GRAMMAR<-CreateGrammar(RULE)                                                           #Generate grammar
  LIBRARY_E_B<-GrammarRandomExpression(GRAMMAR, numExpr = R, max.depth = D)              #Generate random basic expressions
  
  for(i in 1:R){                                                                         #For each basic expression
    LIBRARY_E_EXP[[i]]<-as.expression( paste( "exp(", (LIBRARY_E_B[[i]]), ")" ) )        #Generate the exp transformation
  }
  
  LIBRARY_E<-c(LIBRARY_E_B, LIBRARY_E_EXP) %>%                                           #Combine all expression lists
             .[!duplicated(as.character(.))] %>%                                         #Drop any duplication
             lapply(., as.character)         %>%                                         #Convert to text format
             lapply(., as.data.table)        %>%                                         #Convert from list to table
             rbindlist(.)                    %>%                                         #Combine into single table
             .[, INDEX := 1:(.N)]            %>%                                         #Create index
             .[, TYPE  := "EXPRESSION"]      %>%                                         #Define type
             .[, MEAN  := 0]                 %>%                                         #Assign initial mean  (only for BIAS & COEFFICIENT)
             .[, SD    := 0]                 %>%                                         #Assign initial sd    (only for BIAS & COEFFICIENT)
             setnames(., which(grepl("V",  names(.))), "RULE")                           #Rename text column
  
  #Transformation - Create Function Library
  LIBRARY_F_B<-list(expression( ( 1 + exp( -INPUT ) ) ^ -1 ),
                    expression( min(max(INPUT, 0), 1) ))
  
  LIBRARY_F<-c(LIBRARY_F_B)                  %>%
             .[!duplicated(as.character(.))] %>%                                         #Drop any duplication
             lapply(., as.character)         %>%                                         #Convert to text format
             lapply(., as.data.table)        %>%                                         #Convert from list to table
             rbindlist(.)                    %>%                                         #Combine into single table
             .[, INDEX := 1:(.N)]            %>%                                         #Create index
             .[, TYPE  := "FUNCTION"]        %>%                                         #Define type
             .[, MEAN  := 0]                 %>%                                         #Assign initial mean  (only for BIAS & COEFFICIENT)
             .[, SD    := 0]                 %>%                                         #Assign initial sd    (only for BIAS & COEFFICIENT)
             setnames(., which(grepl("V",  names(.))), "RULE")                           #Rename text column
  
  #Transformation - Create output table
  out<-rbind(rbind(LIBRARY_B, LIBRARY_C, LIBRARY_E, LIBRARY_F) %>% .[, VARIABLE := "ACT_SURPLUS"],
             rbind(LIBRARY_B, LIBRARY_C, LIBRARY_E, LIBRARY_F) %>% .[, VARIABLE := "ACT_RISK_APPETITE"],
             rbind(LIBRARY_B, LIBRARY_C, LIBRARY_E, LIBRARY_F) %>% .[, VARIABLE := "ACT_PREMIUM"],
             rbind(LIBRARY_B, LIBRARY_C, LIBRARY_E, LIBRARY_F) %>% .[, VARIABLE := "ACT_RI"],
             rbind(LIBRARY_B, LIBRARY_C, LIBRARY_E, LIBRARY_F) %>% .[, VARIABLE := "ACT_DIVIDEND"]) %>%
       setcolorder(., c("VARIABLE", "TYPE", "INDEX", "RULE", "MEAN", "SD")) %>%
       setkey(., VARIABLE, TYPE, INDEX)
  
  #Output
  return(out)
  
}
F_A_02_NN_WEIGHT<-function(r, g, c, T_SETTING_RUN, T_NN_STRUCTURE){
  
  #Reference
  RUN_SEED<-T_SETTING_RUN[RUN_INDEX == r, RUN_SEED]                                                                   #Random number generator seed  
  N<-T_SETTING_RUN[RUN_INDEX == r, OPT_POPULATION]                                                                    #Number of networks
  NN_ROW<-T_NN_STRUCTURE[RUN_INDEX == r, NN_ROW]                                                                      #Number of rows per layer 
  NN_COL<-T_NN_STRUCTURE[RUN_INDEX == r, NN_COL]                                                                      #Number of cols per layer

  #Reproducibility
  set.seed(RUN_SEED + r + g)
  
  #Transformation
  out<-rbind(CJ(NETWORK = 1:N, NN_LAYER = 1, NN_ROW = 0:NN_ROW[1], NN_COL = 0:NN_COL[1]),                             #Create reference on layer = 1
             CJ(NETWORK = 1:N, NN_LAYER = 2, NN_ROW = 0:NN_ROW[2], NN_COL = 0:NN_COL[2]),                             #Create reference on layer = 2
             CJ(NETWORK = 1:N, NN_LAYER = 3, NN_ROW = 0:NN_ROW[3], NN_COL = 0:NN_COL[3]))[                            #Create reference on layer = 3
       !(NN_ROW == 0 & NN_COL != 0)][                                                                                 #Fix reference for bias step 1
       !(NN_ROW != 0 & NN_COL == 0)] %>%                                                                              #Fix reference for bias step 2
       merge(., T_NN_STRUCTURE[RUN_INDEX == r, .(NN_LAYER, FAN_IN = NN_ROW, FAN_OUT = NN_COL)], by = "NN_LAYER") %>%  #Retrieve network structure information
       .[, TYPE  := ifelse(NN_ROW == 0 & NN_COL == 0, "BIAS", "WEIGHT")] %>%                                          #Assign type of parameter
       .[, RANGE := 4 * (6 / (FAN_IN + FAN_OUT))^0.5] %>%                                                             #Glorot & Bengio uniform initialization range
       .[, VALUE := runif(n = .N, min = - RANGE, max = + RANGE)] %>%                                                  #Sample initial parameter
       .[, CLASS := c] %>%                                                                                            #Assign network class                                           
       .[, .(CLASS, NETWORK, NN_LAYER, NN_ROW, NN_COL, TYPE, VALUE)]   %>%                                            #Filter columns to keep
       setkey(., CLASS, NETWORK, NN_LAYER, NN_ROW, NN_COL, TYPE)                                                      #Set primary key

  #Output
  return(out)  
  
}
F_A_03_SR_WEIGHT<-function(r, g, c, T_SETTING_RUN, T_SETTING_ENVIRONMENT, T_SR_STRUCTURE, T_NN_LIBRARY){
  
  #Reference
  RUN_SEED<-T_SETTING_RUN[RUN_INDEX == r, RUN_SEED]                                                              #Random number generator seed  
  L<-T_SETTING_ENVIRONMENT[RUN_INDEX == r, max(ENV_LOB)]                                                         #Number of LoBs
  N<-T_SETTING_RUN[RUN_INDEX == r, OPT_POPULATION]                                                               #Number of networks
  E<-T_SR_STRUCTURE[RUN_INDEX == r, RULE_COMPONENT_NUMBER]                                                       #Number of expressions to aggregate
  VARIABLE<-c("ACT_SURPLUS", "ACT_RISK_APPETITE", "ACT_PREMIUM", "ACT_RI", "ACT_DIVIDEND")                       #Control variables
  TYPE<-c("BIAS", "COEFFICIENT", "EXPRESSION", "FUNCTION")                                                       #Control rule components
  
  #Data container
  out<-data.table()
  
  #Reproducibility
  set.seed(RUN_SEED + r + g)
  
  #Transformation
  for(v in VARIABLE){                                                                                            #For each control variable

    LN_E<-T_NN_LIBRARY[VARIABLE == v & TYPE == "EXPRESSION", .N]                                                 #Number of unique expressions in library
    LN_F<-T_NN_LIBRARY[VARIABLE == v & TYPE == "FUNCTION",   .N]                                                 #Number of unique functions   in library

    T_TEMP<-CJ(CLASS = c, NETWORK = 1:N, VARIABLE = v, TYPE = TYPE, INDEX = 1:max(L, E)) %>%                     #Create reference
            .[!(TYPE == "BIAS"        & INDEX > L)] %>%                                                          #Fix reference for BIAS
            .[!(TYPE == "COEFFICIENT" & INDEX > E)] %>%                                                          #Fix reference for COEFFICIENT
            .[!(TYPE == "EXPRESSION"  & INDEX > E)] %>%                                                          #Fix reference for EXPRESSION
            .[!(TYPE == "FUNCTION"    & INDEX > 1)] %>%                                                          #Fix reference for FUNCTION
            merge(x  = .,                                                                                        #Retrieve sampling parameters - BIAS & COEFFICIENT
                  y  = T_NN_LIBRARY[VARIABLE == v & (TYPE == "BIAS" | TYPE == "COEFFICIENT"), .(TYPE, INDEX, MEAN, SD)], 
                  by = c(TYPE = "TYPE", INDEX ="INDEX"), all.x = TRUE) %>%
            .[, MEAN := ifelse(is.na(MEAN), 0, MEAN)] %>%                                                        #Fix NAs in non-float reference - MEAN
            .[, SD   := ifelse(is.na(SD),   0, SD)]   %>%                                                        #Fix NAs in non-float reference - SD
            .[, TEMP_EXPRESSION := sample(x = 1:LN_E, size = .N, replace = TRUE)] %>%                            #Sample initial parameter - EXPRESSION
            .[, TEMP_FUNCTION   := sample(x = 1:LN_F, size = .N, replace = TRUE)] %>%                            #Sample initial parameter - FUNCTION
            .[, TEMP_FLOAT      := rnorm(n = .N, mean = MEAN, sd = SD)] %>%                                      #Sample initial parameter - BIAS & COEFFICIENT
            .[, VALUE           := ifelse(TYPE == "EXPRESSION", TEMP_EXPRESSION,                                 #Assign sampled values
                                   ifelse(TYPE == "FUNCTION",   TEMP_FUNCTION, TEMP_FLOAT) )] %>%
            .[,  .(CLASS, NETWORK, VARIABLE, TYPE, INDEX, VALUE)]                                                #Filter columns to keep
    
    #Store results
    out<-rbind(out, T_TEMP) %>%                                                                                  #Store results
         setkey(., CLASS, NETWORK, VARIABLE, TYPE, INDEX)                                                        #Set primary key
  }
  
  #Output
  return(out)  
  
}
F_A_04_MATRIX<-function(c, T_NN_WEIGHT){
  
  #Reference
  L<-T_NN_WEIGHT[, max(NN_LAYER)]                                                   #Number of LoBs

  #Data Container
  BIAS<-list()                                                                      #Empty list for bias
  WEIGHT<-list()                                                                    #Empty list for weight
  
  #Transformation
  for(l in 1:L){                                                                    #For each layer
    BIAS[[l]]<-F_Z_01_FROM_VEC_TO_LIST(T_NN_WEIGHT[CLASS == c], l, "BIAS")          #Create bias lists
    WEIGHT[[l]]<-F_Z_01_FROM_VEC_TO_LIST(T_NN_WEIGHT[CLASS == c], l, "WEIGHT")      #Create weight lists
  }
  
  out<-list(BIAS = BIAS, WEIGHT = WEIGHT)                                           #Create final list 
  
  #Output
  return(out)
  
}
F_A_05_REWARD<-function(r, T_SETTING_RUN){
  
  #Reference
  N<-T_SETTING_RUN[RUN_INDEX == r, OPT_POPULATION]                         #Number of networks
  S<-T_SETTING_RUN[RUN_INDEX == r, OPT_SIMULATION]                         #Number of simulations
  RUN_INITIAL_SURPLUS<-T_SETTING_RUN[RUN_INDEX == r, RUN_INITIAL_SURPLUS]  #Initial surplus
  
  #Transformation
  out<-CJ(NETWORK = 1:N, SIMULATION = 1:S)[,                               #Create reference
       ':=' (TIME = 0,                                                     #Initialize time reference
             SURPLUS  = RUN_INITIAL_SURPLUS,                               #Initialize surplus
             SCR = 0,                                                      #Initialize SCR
             DIVIDEND = 0,                                                 #Initialize dividend
             SURVIVE = 1)] %>%                                             #Initialize survive
       setkey(., NETWORK, SIMULATION, TIME)                                #Set primary key

  #Output
  return(out)  
  
}
F_A_06_CONTROL<-function(r, T_SETTING_MACRO, T_SETTING_RUN, T_SETTING_ENVIRONMENT){

  #Reference
  N<-T_SETTING_RUN[RUN_INDEX == r, OPT_POPULATION]                                                #Number of networks
  S<-T_SETTING_RUN[RUN_INDEX == r, OPT_SIMULATION]                                                #Number of simulations
  L<-T_SETTING_ENVIRONMENT[RUN_INDEX == r, max(ENV_LOB)]                                          #Number of LoBs
  RUN_INITIAL_SURPLUS<-T_SETTING_RUN[RUN_INDEX == r, RUN_INITIAL_SURPLUS]                         #Initial surplus
  NN_SCALE_SURPLUS<-T_SETTING_RUN[RUN_INDEX == r, NN_SCALE_SURPLUS]                               #Surplus scaling factor for network
  COL_NAME<-setdiff(T_SETTING_MACRO[TABLE == "T_RL_CONTROL", VARIABLE],                           #Retrieve columns names
                    c("NETWORK", "SIMULATION", "ENV_LOB", "START_SURPLUS", "START_SURVIVE",
                      T_SETTING_MACRO[TABLE == "T_SETTING_ENVIRONMENT", VARIABLE]))
  
  #Transformation - Create base set of info
  out<-CJ(NETWORK = 1:N, SIMULATION = 1:S, ENV_LOB = 1:L) %>%                                     #Create reference      
       .[T_SETTING_ENVIRONMENT[RUN_INDEX == r, ], on = "ENV_LOB"] %>%                             #Retrieve environment parameter    
       .[, (COL_NAME)   := 0] %>%                                                                 #Create empty cols
       .[, SURPLUS      := RUN_INITIAL_SURPLUS / L] %>%                                           #Initialize surplus per lob
       .[, SURVIVE      := 1] %>%                                                                 #Initialize survive
       .[, NN_A_SURPLUS := RUN_INITIAL_SURPLUS / NN_SCALE_SURPLUS] %>%                            #Network input - agent specific - surplus
       .[, NN_L_SURPLUS := SURPLUS / NN_SCALE_SURPLUS] %>%                                        #Network input - loB   specific - surplus
    setkey(., NETWORK, SIMULATION, ENV_LOB)                                                       #Set primary key

  #Transformation - Create lob dummy representation
  options(warn = -1)                                                                              #Turn off warning message
  ENV_LOB_DUMMY<-dummy(out$ENV_LOB, sep = "_", drop = TRUE, fun = as.numeric, verbose = FALSE)    #Create ENV_LOB dummy representation
  colnames(ENV_LOB_DUMMY)<-paste("NN_LOB_", 1:L, sep = "")                                        #Assign standard naming
  options(warn = 1)                                                                               #Turn on warning message
  
  #Transformation - Create output table
  out<-cbind(out, ENV_LOB_DUMMY)                                                                 

  #Output
  return(out)
  
}
F_A_07_SCORE<-function(){
  
  #Data Container
  data.table(CLASS   = character(), 
             NETWORK = integer(), 
             RETURN  = numeric(), 
             RISK    = numeric(),
             PARETO  = integer(),
             CROWD   = numeric()) %>%
    setkey(., CLASS, NETWORK)

}
F_A_08_COMPARE<-function(r){
  
  #Data Container
  out<-CJ(RUN_INDEX = r, RUN_TYPE = c("NN", "SR"), OPT_TYPE = c("ADEMO", "UMCS"), HYPERVOLUME = 0, CARDINALITY = 0, SPACING = 0, RANGE = 0, DOMINANCE = 0) %>% 
       setkey(., RUN_INDEX, RUN_TYPE, OPT_TYPE)
  
}
F_A_09_GRID<-function(r, T_SETTING_RUN, T_SETTING_ENVIRONMENT, L_NN_POPULATION){
  
  #Reference
  RUN_SEED<-T_SETTING_RUN[RUN_INDEX == r, RUN_SEED]                            #Random number generator seed  
  N<-L_NN_POPULATION$T_NN_SCORE[, max(NETWORK)]                                #Number of networks
  S<-T_SETTING_RUN[RUN_INDEX == r, OPT_SIMULATION]                             #Number of simulations
  L<-T_SETTING_ENVIRONMENT[RUN_INDEX == r, max(ENV_LOB)]                       #Number of LoBs
  
  #Reproducibility
  set.seed(RUN_SEED + r)
  
  #Transformation
  out<-CJ(NETWORK = 1:N, SIMULATION = 1:S, ENV_LOB = 1:L)[,                    #Create reference
       ':=' (NN_L_EXP      = runif(n = .N, min = 0, max = 1),                  #Simulate Network Input: NN_EXPOSURE
             NN_L_RESULT   = runif(n = .N, min = 0, max = 1),                  #Simulate Network Input: NN_RESULT
             NN_L_SOLVENCY = runif(n = .N, min = 0, max = 1),                  #Simulate Network Input: NN_SOLVENCY
             NN_L_SURPLUS  = runif(n = .N, min = 0, max = 1))] %>%             #Simulate Network Input: NN_SURPLUS
       setkey(., NETWORK, SIMULATION, ENV_LOB)                                 #Set primary key

  #Output
  return(out)  
  
}

#Stochastic Control
F_B_01_MOVE<-function(r, RUN_TYPE, T_SETTING_RUN, T_SETTING_ENVIRONMENT, T_NN_MATRIX, T_NN_EXPRESSION, T_RL_CONTROL){
  
  #Reference
  S<-T_SETTING_RUN[RUN_INDEX == r, OPT_SIMULATION]                                            #Number of simulations
  N<-T_SETTING_RUN[RUN_INDEX == r, OPT_POPULATION]                                            #Number of networks
  L<-T_SETTING_ENVIRONMENT[RUN_INDEX == r, max(ENV_LOB)]                                      #Number of LoBs
  COL_NAME_1<-CJ(ENV_LOB = 1:L, VARIABLE = c("TEMP_ACT_DIVIDEND", "TEMP_ACT_PREMIUM", "TEMP_ACT_RI", "TEMP_ACT_RISK_APPETITE", "TEMP_ACT_SURPLUS"))[, VAR_NAME := paste(VARIABLE, ENV_LOB, sep = ".")][,VAR_NAME] %>% sort
  COL_NAME_2<-c("NETWORK", "SIMULATION", "ENV_LOB", "ACT_DIVIDEND", "ACT_PREMIUM", "ACT_RI", "ACT_RISK_APPETITE", "ACT_SURPLUS")
  COL_NAME_3<-names(T_RL_CONTROL)[!grepl("ACT_", names(T_RL_CONTROL))]
  COL_NAME_4<-names(T_RL_CONTROL)                                                             #Columns to keep - SYMBOLIC
  
  #Data container
  T_E<-{ if (RUN_TYPE == "SR") copy(T_NN_EXPRESSION) else NULL }                              #Copy expression table
  T_C<-copy(T_RL_CONTROL)                                                                     #Copy control table
  
  #Transformation
  { if (RUN_TYPE == "NN") 
    
    #NUMERIC
    out<-
      
          #Prepare Input Data
          dcast(T_C,                                                                          #Reshape table from long to wide      
                NETWORK + SIMULATION ~ ENV_LOB, 
                value.var = c("NN_L_EXP", "NN_L_RESULT", "NN_L_SOLVENCY", "NN_L_SURPLUS")) %>%
          split(., .$NETWORK) %>%                                                             #Create list for each network
          lapply(., function(x) x[,c("NETWORK" ,"SIMULATION") := NULL]) %>%                   #Remove index columns
          lapply(., as.matrix) %>%                                                            #Convert table to matrix format
          
          #Feed Forward: Layer 1
          mapply(F_Z_04_MM, ., T_NN_MATRIX$WEIGHT[[1]], SIMPLIFY = FALSE) %>%                 #Matrix multiplication
          mapply("+", ., as.numeric(T_NN_MATRIX$BIAS[[1]]), SIMPLIFY = FALSE) %>%             #Add bias
          lapply(., F_Z_03_NON_LINEARITY)  %>%                                                #Non Linearity
          
          #Feed Forward: Layer 2
          mapply(F_Z_04_MM, ., T_NN_MATRIX$WEIGHT[[2]], SIMPLIFY = FALSE) %>%                 #Matrix multiplication 
          mapply("+", ., as.numeric(T_NN_MATRIX$BIAS[[2]]), SIMPLIFY = FALSE) %>%             #Add bias     
          lapply(., F_Z_03_NON_LINEARITY) %>%                                                 #Non Linearity
          
          #Feed Forward: Layer 3
          mapply(F_Z_04_MM, ., T_NN_MATRIX$WEIGHT[[3]], SIMPLIFY = FALSE) %>%                 #Matrix multiplication  
          mapply("+", ., as.numeric(T_NN_MATRIX$BIAS[[3]]), SIMPLIFY = FALSE) %>%             #Add bias 
          lapply(., F_Z_03_NON_LINEARITY) %>%                                                 #Non Linearity 
          
          #Adjust table format
          lapply(., as.data.table) %>%                                                        #Reconvert to data table
          lapply(., function(x) x[, SIMULATION := 1:S]) %>%                                   #Add back simulation index
          rbindlist(., idcol = 'NETWORK') %>%                                                 #Combine list into single table
          setnames(., which(grepl("V",  names(.))), COL_NAME_1) %>%                           #Rename variables
          melt(.,                                                                             #Reshape table from wide to long
               id.vars = c("NETWORK", "SIMULATION"), 
               measure.vars = list(TEMP_ACT_DIVIDEND = paste("TEMP_ACT_DIVIDEND", 1:L, sep = "."), 
                                   TEMP_ACT_PREMIUM = paste("TEMP_ACT_PREMIUM", 1:L, sep = "."), 
                                   TEMP_ACT_RI = paste("TEMP_ACT_RI", 1:L, sep = "."), 
                                   TEMP_ACT_RISK_APPETITE = paste("TEMP_ACT_RISK_APPETITE", 1:L, sep = "."), 
                                   TEMP_ACT_SURPLUS = paste("TEMP_ACT_SURPLUS", 1:L, sep = ".")), 
               variable.name = "ENV_LOB") %>%
          
          #Convert network signal into action
          .[, NETWORK := as.integer(NETWORK)] %>%                                             #Adjust network index format
          .[, ENV_LOB := as.integer(ENV_LOB)] %>%                                             #Adjust lob index format
          .[, ACT_RISK_APPETITE := 1 + TEMP_ACT_RISK_APPETITE] %>%                            #Calculate risk appetite action
          .[, ACT_PREMIUM := TEMP_ACT_PREMIUM] %>%                                            #Calculate premium action
          .[, ACT_RI := pmin(TEMP_ACT_RI, 0.99)] %>%                                          #Calculate reinsurance action
          .[, ACT_DIVIDEND := TEMP_ACT_DIVIDEND] %>%                                          #Calculate dividend action
          .[, TEMP_ACT_SURPLUS_SCALE := sum(TEMP_ACT_SURPLUS), by = c("NETWORK", "SIMULATION")] %>%
          .[, ACT_SURPLUS := ifelse(TEMP_ACT_SURPLUS_SCALE == 0, 1 / L, TEMP_ACT_SURPLUS / TEMP_ACT_SURPLUS_SCALE)] %>%    
          .[, ..COL_NAME_2] %>%                                                               #Select final columns
          setkey(., NETWORK, SIMULATION, ENV_LOB) %>%                                         #Set primary key
          
          #Join with T_RL_CONTROL table
          T_C[,..COL_NAME_3][., on = c("NETWORK", "SIMULATION", "ENV_LOB")]
    
    else                      
  
    #SYMBOLIC
    for(i in 1:N){                                                                            #For each network
      
      T_N<-copy(T_C[NETWORK == i]) %>%                                                        #Calculate action
        
        #Evaluate expression
        .[, TEMP_ACT_SURPLUS       := EvalExpressions(expressions = T_E[NETWORK == i & VARIABLE == "ACT_SURPLUS",       EXPRESSION], envir = .)] %>%
        .[, TEMP_ACT_RISK_APPETITE := EvalExpressions(expressions = T_E[NETWORK == i & VARIABLE == "ACT_RISK_APPETITE", EXPRESSION], envir = .)] %>%
        .[, TEMP_ACT_PREMIUM       := EvalExpressions(expressions = T_E[NETWORK == i & VARIABLE == "ACT_PREMIUM",       EXPRESSION], envir = .)] %>%
        .[, TEMP_ACT_RI            := EvalExpressions(expressions = T_E[NETWORK == i & VARIABLE == "ACT_RI",            EXPRESSION], envir = .)] %>%
        .[, TEMP_ACT_DIVIDEND      := EvalExpressions(expressions = T_E[NETWORK == i & VARIABLE == "ACT_DIVIDEND",      EXPRESSION], envir = .)] %>%
        
        #Evaluate rule signal
        .[, SIGN_ACT_SURPLUS       := EvalExpressions(expressions = T_E[NETWORK == i & VARIABLE == "ACT_SURPLUS",       FUNCTION], envir = .)] %>%
        .[, SIGN_ACT_RISK_APPETITE := EvalExpressions(expressions = T_E[NETWORK == i & VARIABLE == "ACT_RISK_APPETITE", FUNCTION], envir = .)] %>%
        .[, SIGN_ACT_PREMIUM       := EvalExpressions(expressions = T_E[NETWORK == i & VARIABLE == "ACT_PREMIUM",       FUNCTION], envir = .)] %>%
        .[, SIGN_ACT_RI            := EvalExpressions(expressions = T_E[NETWORK == i & VARIABLE == "ACT_RI",            FUNCTION], envir = .)] %>%
        .[, SIGN_ACT_DIVIDEND      := EvalExpressions(expressions = T_E[NETWORK == i & VARIABLE == "ACT_DIVIDEND",      FUNCTION], envir = .)] %>%
        
        #Clear any abnormal signals
        .[, SIGN_ACT_SURPLUS       := ifelse(is.infinite(SIGN_ACT_SURPLUS)       | is.na(SIGN_ACT_SURPLUS),       1, SIGN_ACT_SURPLUS)]       %>%
        .[, SIGN_ACT_RISK_APPETITE := ifelse(is.infinite(SIGN_ACT_RISK_APPETITE) | is.na(SIGN_ACT_RISK_APPETITE), 0, SIGN_ACT_RISK_APPETITE)] %>%
        .[, SIGN_ACT_PREMIUM       := ifelse(is.infinite(SIGN_ACT_PREMIUM)       | is.na(SIGN_ACT_PREMIUM),       0, SIGN_ACT_PREMIUM)]       %>%
        .[, SIGN_ACT_RI            := ifelse(is.infinite(SIGN_ACT_RI)            | is.na(SIGN_ACT_RI),            0, SIGN_ACT_RI)]            %>%
        .[, SIGN_ACT_DIVIDEND      := ifelse(is.infinite(SIGN_ACT_DIVIDEND)      | is.na(SIGN_ACT_DIVIDEND),      0, SIGN_ACT_DIVIDEND)]      %>%
        
        #Convert rule signal into action 
        .[, ACT_RISK_APPETITE := 1 + SIGN_ACT_RISK_APPETITE] %>%                              
        .[, ACT_PREMIUM       := SIGN_ACT_PREMIUM]           %>%                                              
        .[, ACT_RI            := pmin(SIGN_ACT_RI, 0.99)]    %>%                                            
        .[, ACT_DIVIDEND      := SIGN_ACT_DIVIDEND]          %>%                                            
        .[, ACT_SURPLUS_SCALE := sum(SIGN_ACT_SURPLUS), by = c("NETWORK", "SIMULATION")] %>%
        .[, ACT_SURPLUS       := ifelse(ACT_SURPLUS_SCALE == 0, 1 / L, SIGN_ACT_SURPLUS / ACT_SURPLUS_SCALE)] %>%    
        
        #Prepare final table
        .[, ..COL_NAME_4] %>%                                                                 
        setkey(., NETWORK, SIMULATION, ENV_LOB)               
      
      out<-if(i == 1){T_N}else{rbind(out, T_N)} %>%                                           #Create aggregated output table
           as.data.table(.) %>%                                                               #Adjust table format
           setkey(., NETWORK, SIMULATION, ENV_LOB)                                            #Set primary key
    }
    
  }
  
  #Output
  return(out)   
  
}
F_B_02_ENVIRONMENT<-function(r, g, i, T_SETTING_RUN, T_RL_REWARD, T_RL_CONTROL){
  
  #Reference
  RUN_SEED<-T_SETTING_RUN[RUN_INDEX == r, RUN_SEED]                  #Random number generator seed  
  NN_SCALE_SR<-T_SETTING_RUN[RUN_INDEX == r, NN_SCALE_SR]            #Network input: scale factor on solvency ratio
  NN_SCALE_SURPLUS<-T_SETTING_RUN[RUN_INDEX == r, NN_SCALE_SURPLUS]  #Network input: scale factor on surplus
  COL_NAME<-names(T_RL_CONTROL)                                      #Columns to keep
  
  #Reproducibility
  set.seed(RUN_SEED + g + i)
  
  #Transformation
  out<-T_RL_CONTROL[
    
       #Retrieve initial Surplus and Survive
       T_RL_REWARD[,.(NETWORK, SIMULATION, START_SURPLUS = SURPLUS, START_SURVIVE = SURVIVE)], 
       on = .(NETWORK, SIMULATION)][,

       #Update Time Reference
       TIME := i][,
                                            
       #Convert survive status to flag (0/1)
       START_SURVIVE := ifelse(TIME == START_SURVIVE, 1, 0)][,
                                                                                                  
       #Pricing
       PREMIUM_GROSS := ENV_FRQ_M * ENV_SEV_M * (1 + ACT_PREMIUM) * START_SURVIVE][,
       PREMIUM_RI    := ENV_FRQ_M * ENV_SEV_M * (1 + ACT_PREMIUM * ENV_RI_LOADING) * (1 - ACT_RI) * START_SURVIVE][,
                                                                                                                                                                                                                                                                                          
       #SCR ex-ante: Exposure
       EXP_SCR_UW_P := (PREMIUM_GROSS - PREMIUM_RI) * ENV_SCR_FACTOR_UW_P * START_SURVIVE][,
       EXP_SCR_UW_R := ENV_FRQ_M * ENV_SEV_M * ENV_RES_FACTOR * ACT_RI * ENV_SCR_FACTOR_UW_R * START_SURVIVE][,
       EXP_SCR_UW   := (EXP_SCR_UW_P ^ 2 + EXP_SCR_UW_R ^ 2 + 2 * ENV_SCR_RHO_UW_PR * EXP_SCR_UW_P * EXP_SCR_UW_R) ^ 0.5 * START_SURVIVE][,
       EXP_SCR_CR   := ENV_FRQ_M * ENV_SEV_M * (1 - ACT_RI) * ENV_SCR_FACTOR_CR * START_SURVIVE][,
       EXP_SCR      := (EXP_SCR_UW ^ 2 + EXP_SCR_CR ^ 2 + 2 * ENV_SCR_RHO_UW_CR * EXP_SCR_UW * EXP_SCR_CR) ^ 0.5 * START_SURVIVE][,
       EXP_TARGET   := ifelse(START_SURVIVE == 1, ((START_SURPLUS * ACT_SURPLUS) / ACT_RISK_APPETITE) / EXP_SCR, 0)][,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
       #Market reaction
       PH_PROBABILITY  := ifelse(START_SURVIVE == 1, 1 - exp(PREMIUM_GROSS * ENV_PH_FACTOR) / (1 + exp(PREMIUM_GROSS * ENV_PH_FACTOR)), 0)][,
       PH_RESPONSE	   := ifelse(START_SURVIVE == 1, rpois(n = .N, lambda = PH_PROBABILITY * ENV_EXP_TOTAL), 0)][,
       EXP_NUMBER      := ifelse(START_SURVIVE == 1, pmin(EXP_TARGET, PH_RESPONSE), 0)][,
       CLAIMS_NUMBER	 := ifelse(START_SURVIVE == 1, rpois(n = .N, lambda = ENV_FRQ_M * EXP_NUMBER), 0)][,
       CLAIMS_AVG_COST := ifelse(START_SURVIVE == 1, rlnorm(n = .N, meanlog = ENV_SEV_P1, sdlog = ENV_SEV_P2), 0)][,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
       #Result pre-dividend
       RESULT_INFLOW        := EXP_NUMBER * PREMIUM_GROSS][,
       RESULT_OUTFLOW       := CLAIMS_NUMBER * CLAIMS_AVG_COST][,
       RESULT_PRE_DIVIDEND  := RESULT_INFLOW - RESULT_OUTFLOW][,
       SURPLUS_PRE_DIVIDEND := START_SURPLUS * ACT_SURPLUS + RESULT_PRE_DIVIDEND][,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
       #SCR ex-post
       SCR_UW_P       := RESULT_INFLOW * ENV_SCR_FACTOR_UW_P][,
       SCR_UW_R       := RESULT_OUTFLOW * ENV_RES_FACTOR * ENV_SCR_FACTOR_UW_R][,
       SCR_UW         := (SCR_UW_P ^ 2 + SCR_UW_R ^ 2 + 2 * ENV_SCR_RHO_UW_PR * SCR_UW_P * SCR_UW_R) ^ 0.5][,
       SCR_CR         := RESULT_OUTFLOW * ENV_SCR_FACTOR_CR][,
       SCR            := (SCR_UW ^ 2 + SCR_CR ^ 2 + 2 * ENV_SCR_RHO_UW_CR * SCR_UW * SCR_CR) ^ 0.5][,
       SOLVENCY_RATIO := ifelse(SCR != 0, SURPLUS_PRE_DIVIDEND / SCR, 0)][,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
       #Agent survival
       SCR_TOT            := sum(SCR),                  keyby = .(NETWORK, SIMULATION)][,
       SURPLUS_TOT        := sum(SURPLUS_PRE_DIVIDEND), keyby = .(NETWORK, SIMULATION)][,
       SOLVENCY_RATIO_TOT := ifelse(SCR_TOT != 0, SURPLUS_TOT / SCR_TOT, 0)][,
       SURVIVE            := ifelse(SURPLUS_TOT > SCR_TOT, 1, 0)][,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
       #Dividend
       MAX_DIVIDEND := ifelse(SURVIVE == 1, pmax( pmin(SURPLUS_TOT - SCR_TOT, RESULT_PRE_DIVIDEND), 0), 0)][,
       DIVIDEND     := MAX_DIVIDEND * ACT_DIVIDEND][,
       RESULT       := ifelse(SURVIVE == 1, RESULT_PRE_DIVIDEND  - DIVIDEND, 0)][,
       SURPLUS      := ifelse(SURVIVE == 1, SURPLUS_PRE_DIVIDEND - DIVIDEND, 0)][,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
       #Aggregated results
       EXP_NUMBER_TOT    := sum(EXP_NUMBER),    keyby = .(NETWORK, SIMULATION)][,
       EXP_TARGET_TOT    := sum(EXP_TARGET),    keyby = .(NETWORK, SIMULATION)][,
       RESULT_TOT        := sum(RESULT),        keyby = .(NETWORK, SIMULATION)][,
       RESULT_INFLOW_TOT := sum(RESULT_INFLOW), keyby = .(NETWORK, SIMULATION)][,                                                                          
       
       #Neural network inputs
       NN_A_EXP      := ifelse(EXP_TARGET_TOT != 0,    EXP_NUMBER_TOT / EXP_TARGET_TOT,  0) * SURVIVE][,
       NN_A_RESULT   := ifelse(RESULT_INFLOW_TOT != 0, RESULT_TOT / RESULT_INFLOW_TOT,   0)][,
       NN_A_SOLVENCY := ifelse(SURVIVE == 1,           SOLVENCY_RATIO_TOT / NN_SCALE_SR, 0)][,
       NN_A_SURPLUS  := ifelse(SURVIVE == 1,           SURPLUS_TOT / NN_SCALE_SURPLUS,   0)][,
       
       NN_L_EXP      := ifelse(EXP_TARGET != 0,        EXP_NUMBER / EXP_TARGET,          0) * SURVIVE][,
       NN_L_RESULT   := ifelse(RESULT_INFLOW != 0,     RESULT / RESULT_INFLOW,           0)][,
       NN_L_SOLVENCY := ifelse(SURVIVE == 1,           SOLVENCY_RATIO / NN_SCALE_SR,     0)][,
       NN_L_SURPLUS  := ifelse(SURVIVE == 1,           SURPLUS / NN_SCALE_SURPLUS,       0)][,
       
       #Filter columns to keep
       ..COL_NAME]
  
  #Output
  return(out)      
  
}
F_B_03_REWARD<-function(r, i, T_SETTING_RUN, T_RL_REWARD, T_RL_CONTROL){

  #Reference
  RUN_DISCOUNT_RATE<-T_SETTING_RUN[RUN_INDEX == r, RUN_DISCOUNT_RATE]     #Initial surplus

  #Transformation
  out<-T_RL_REWARD[
                    #Calculate agent aggregated status
                    T_RL_CONTROL[,
                                  .(TEMP_SCR      = sum(SCR), 
                                    TEMP_SURPLUS  = sum(SURPLUS), 
                                    TEMP_DIVIDEND = sum(DIVIDEND) * (1 + RUN_DISCOUNT_RATE) ^ - i), 
                                  keyby = .(NETWORK, SIMULATION)],
    
                    #Retrieve reward table
                    on = .(NETWORK, SIMULATION)][,
                    
                    #Update cumulative reward                                 
                    TIME           := i][,                                                  
                    FINAL_DIVIDEND := TEMP_DIVIDEND + DIVIDEND][,
                    FINAL_SURVIVE  := ifelse(TEMP_SURPLUS > TEMP_SCR, SURVIVE + 1, SURVIVE)][,
                    FINAL_SCR      := ifelse(TEMP_SURPLUS > TEMP_SCR, TEMP_SCR, 0)][,
                    FINAL_SURPLUS  := ifelse(TEMP_SURPLUS > TEMP_SCR, TEMP_SURPLUS, 0)][,
                    
                    #Filter and rename
                    .(NETWORK, SIMULATION, TIME, SURPLUS = FINAL_SURPLUS, SCR = FINAL_SCR, DIVIDEND = FINAL_DIVIDEND, SURVIVE = FINAL_SURVIVE)]

  #Output
  return(out)  

}
F_B_04_SCORE<-function(r, c, T_SETTING_RUN, T_RL_REWARD, T_NN_SCORE){
  
  #Reference
  TH<-T_SETTING_RUN[RUN_INDEX == r, RUN_TIME_HORIZON]                       #Time horizon
  RUN_INITIAL_SURPLUS<-T_SETTING_RUN[RUN_INDEX == r, RUN_INITIAL_SURPLUS]   #Initial surplus
  
  #Transformation
  out<-T_RL_REWARD[, 
                   
       #Calculate risk measure             
       SURVIVE_TEMP := TH + 1 - SURVIVE][,
       
       #Calculate average performance                                    
       .(RETURN = mean(DIVIDEND) / RUN_INITIAL_SURPLUS,
         RISK = mean(SURVIVE_TEMP) / TH),
       by = c("NETWORK")][,
                          
       #Add optimization reference
       CLASS  := c][,                                                       #Assign network class
       PARETO := 0][,                                                       #Assign Pareto efficiency level
       CROWD  := 0] %>%                                                     #Assign crowd distance
       
       #Retrieve old generations score
       rbind(., T_NN_SCORE[CLASS != c]) %>%
    
       #Set primary key
       setkey(., CLASS, NETWORK)
  
  #Output
  return(out)  
  
}
F_B_05_RUN<-function(r, g, c, RUN_TYPE, T_SETTING_MACRO, T_SETTING_RUN, T_SETTING_ENVIRONMENT, T_NN_LIBRARY, T_NN_WEIGHT, T_SR_WEIGHT, T_NN_SCORE){

  #Reference
  TH<-T_SETTING_RUN[RUN_INDEX == r, RUN_TIME_HORIZON]  #Time Horizon
  
  #Data Container
  T_RL_CONTROL<-F_A_06_CONTROL(r, T_SETTING_MACRO, T_SETTING_RUN, T_SETTING_ENVIRONMENT)
  T_RL_REWARD<-F_A_05_REWARD(r, T_SETTING_RUN)
  
  { if (RUN_TYPE == "NN") T_NN_MATRIX<-F_A_04_MATRIX(c, T_NN_WEIGHT) 
    else                  T_NN_EXPRESSION<-F_Z_02_FROM_REF_TO_EXPR(r, c, T_SETTING_RUN, T_SETTING_ENVIRONMENT, T_SR_STRUCTURE, T_NN_LIBRARY, T_SR_WEIGHT) }

  #Transformation - Simulate agent / environment interactions
  for(i in 1:TH){
    
    #Action
    T_RL_CONTROL<-F_B_01_MOVE(r, RUN_TYPE, T_SETTING_RUN, T_SETTING_ENVIRONMENT, T_NN_MATRIX, T_NN_EXPRESSION, T_RL_CONTROL)
    
    #Environment
    T_RL_CONTROL<-F_B_02_ENVIRONMENT(r, g, i, T_SETTING_RUN, T_RL_REWARD, T_RL_CONTROL)
    
    #Reward
    T_RL_REWARD<-F_B_03_REWARD(r, i, T_SETTING_RUN, T_RL_REWARD, T_RL_CONTROL)
    
    #Message
    message(RUN_TYPE, " - Run: ", r, " - Generation: ", g, " - Time: ", i, sep = "")

  }
  
  #Transformation - Evaluate agents' score
  T_NN_SCORE<-F_B_04_SCORE(r, c, T_SETTING_RUN, T_RL_REWARD, T_NN_SCORE)

  #Transformation - Create output list
  out<-list(T_NN_SCORE  = T_NN_SCORE, T_NN_WEIGHT = T_NN_WEIGHT, T_SR_WEIGHT = T_SR_WEIGHT)
  
  #Output
  return(out)

}

#Optimization
F_C_01_RANK<-function(T_NN_SCORE){
  
  #Reference
  j<-0                                                                                    #Initialize Pareto Sorting counter
  EMPTY<-data.table(NETWORK=0, RETURN=0, RISK=0, CLASS="EMPTY", PARETO=0, CROWD=0)        #Empty row to fill lagged data tables (up / down)
  COL_NAME<-colnames(T_NN_SCORE)                                                          #Columns to keep
  
  #Data Container
  out<-copy(T_NN_SCORE)
  
  #Transformation - PARETO SORT
  out<-out[order(RISK, -rank(RETURN)), ][, ':=' (PARETO = 0, CROWD = 0)]                  #Sort and clear previous ranking
  
  while(out[, sum(PARETO == 0)] > 0){                                                     #Until non-ordered networks exist
    
    j<-j + 1                                                                              #Update efficiency level
    
    out<-out[, RETURN_TEMP := ifelse(PARETO == 0, RETURN, 0)][,                           #Adjust return to consider only non-ranked networks
               CUM_MAX     := cummax(RETURN_TEMP)][,                                      #Evaluate cumulative max return
               PARETO      := ifelse(RETURN_TEMP == CUM_MAX & PARETO == 0, j, PARETO)][,  #Flag Pareto rank
            ..COL_NAME]                                                                   #Drop temporary columns
  }
  
  #Transformation - CROWDING DISTANCE
  out<-
    
       #Sort networks by Pareto Rank, Risk and Return
       out[order(PARETO, RISK, RETURN), ] %>%
    
       #Combine original observation with lagged data tables (up / down)
       cbind(.,
             rbind(EMPTY, .[1:(.N-1),]) %>% setnames(., paste(COL_NAME, "UP",   sep = "_")),
             rbind(.[2:.N,],     EMPTY) %>% setnames(., paste(COL_NAME, "DOWN", sep = "_"))) %>%
    
       #Find boundaries observation within frontier
       .[, CHECK := ifelse(PARETO == PARETO_UP, ifelse(PARETO == PARETO_DOWN, 1, 0), 0)] %>%
    
       #Calculate crowding distance
       .[, CROWD := ifelse(CHECK == 0, 
                           Inf,
                           ( abs(RETURN - RETURN_DOWN) + abs(RETURN - RETURN_UP) ) / 2 +
                           ( abs(RISK   - RISK_DOWN)   + abs(RISK   - RISK_UP)   ) / 2)] %>%
    
       #Filter columns to keep
       .[, ..COL_NAME] %>%
    
       #Restore index order 
       .[order(CLASS, NETWORK)]
  
  #Output
  return(out)
  
}
F_C_02_ARCHIVE<-function(RUN_TYPE, L_NN_POPULATION){
  
  #Reference
  COL_NAME<-{ if (RUN_TYPE == "NN") colnames(L_NN_POPULATION$T_NN_WEIGHT)     #Columns to keep
              else                  colnames(L_NN_POPULATION$T_SR_WEIGHT) }

  #Data Container
  T_S<-copy(L_NN_POPULATION$T_NN_SCORE)                                       #Filter old archive - score
  T_W<-{ if (RUN_TYPE == "NN") copy(L_NN_POPULATION$T_NN_WEIGHT)              #Filter old archive - weight/bias
         else                  copy(L_NN_POPULATION$T_SR_WEIGHT) }            

  #Transformation
  T_S[, NETWORK := ifelse(CLASS == "ARCHIVE", - NETWORK, NETWORK)]            #Adjust old archive network reference - score
  T_W[, NETWORK := ifelse(CLASS == "ARCHIVE", - NETWORK, NETWORK)]            #Adjust old archive network reference - weight/bias
  
  T_S<-unique(T_S, by = c("RETURN", "RISK"))                                  #Drop any duplicates solutions
  
  T_S<-F_C_01_RANK(T_S)                                                       #Rank current population

  A_S<-T_S[PARETO == 1] %>%                                                   #Filter efficient frontier
       .[order(PARETO, -rank(CROWD))] %>%                                     #Sort by crowding distance
       .[CROWD > 0]                                                           #Drop any overlapping solutions
  
  A_W<-T_W[                                                                   #Retrieve archive weight/bias
           A_S[, c("NETWORK", "CLASS", "PARETO","CROWD")], on = c("NETWORK", "CLASS")][
           PARETO == 1][
           order(PARETO, -rank(CROWD))][,
           ..COL_NAME]
  
  A_S[, ':=' (NETWORK = match(NETWORK, unique(NETWORK)), CLASS = "ARCHIVE")]  #Restore network references - score
  A_W[, ':=' (NETWORK = match(NETWORK, unique(NETWORK)), CLASS = "ARCHIVE")]  #Restore network references - weight/bias                      
  
  out<-{ if (RUN_TYPE == "NN") list(T_NN_SCORE  = rbind(A_S, L_NN_POPULATION$T_NN_SCORE[CLASS != "ARCHIVE"]),   #Create output list . score
                                    T_NN_WEIGHT = rbind(A_W, L_NN_POPULATION$T_NN_WEIGHT[CLASS != "ARCHIVE"]),  #Create output list - weight/bias - NUMERIC
                                    T_SR_WEIGHT = L_NN_POPULATION$T_SR_WEIGHT[CLASS != "ARCHIVE"])              #Create output list - weight/bias - SYMBOLIC
    
         else                  list(T_NN_SCORE  = rbind(A_S, L_NN_POPULATION$T_NN_SCORE[CLASS != "ARCHIVE"]),   #Create output list . score
                                    T_NN_WEIGHT = L_NN_POPULATION$T_NN_WEIGHT[CLASS != "ARCHIVE"],              #Create output list - weight/bias - NUMERIC
                                    T_SR_WEIGHT = rbind(A_W, L_NN_POPULATION$T_SR_WEIGHT[CLASS != "ARCHIVE"]))} #Create output list - weight/bias - SYMBOLIC

  #Output
  return(out)
  
}
F_C_03_REPRODUCE<-function(r, g, RUN_TYPE, T_SETTING_RUN, T_NN_LIBRARY, L_NN_POPULATION){
  
  #Reference
  RUN_SEED<-T_SETTING_RUN[RUN_INDEX == r, RUN_SEED]                           #Random number generator seed  
  OPT_MUTATION_FRQ<-T_SETTING_RUN[RUN_INDEX == r, OPT_MUTATION_FRQ]           #Optimization parameter: mutation frequency
  OPT_MUTATION_SEV<-T_SETTING_RUN[RUN_INDEX == r, OPT_MUTATION_SEV]           #Optimization parameter: mutation severity
  A<-L_NN_POPULATION$T_NN_SCORE[CLASS == "ARCHIVE", max(NETWORK)]             #Number of networks in archive
  P<-L_NN_POPULATION$T_NN_SCORE[CLASS == "PARENT",  max(NETWORK)]             #Number of parent networks
  E_MAX<-T_NN_LIBRARY[TYPE == "EXPRESSION", max(INDEX)]                       #Max expression reference
  F_MAX<-T_NN_LIBRARY[TYPE == "FUNCTION",   max(INDEX)]                       #Max function   reference
  
  #Data Container
  T_S<-copy(L_NN_POPULATION$T_NN_SCORE)                                       #Networks score
  T_W<-{ if (RUN_TYPE == "NN") copy(L_NN_POPULATION$T_NN_WEIGHT)              #Networks weight/bias
         else                  copy(L_NN_POPULATION$T_SR_WEIGHT) }            
  
  #Reproducibility
  set.seed(RUN_SEED + r + g)   
  
  #Transformation - Find network to combine
  F_S<-T_S[!duplicated(T_S, by = c("RISK", "RETURN")), ]                      #Drop all duplicated networks
  INDEX_P<-F_S[CLASS == "PARENT",  NETWORK]                                   #Network index to sample - Parent
  INDEX_A<-F_S[CLASS == "ARCHIVE", NETWORK][1:min(P, A)]                      #Network index to sample - Archive

  #Transformation - Create children network - weight/bias
  { if (RUN_TYPE == "NN") 
    
               #NUMERIC
               T_NN_WEIGHT<-copy(T_W)[
    
               #Filter parent networks
               CLASS == "PARENT",  .(NETWORK, NN_LAYER, NN_ROW, NN_COL, TYPE, VALUE_O = VALUE)] %>%
              
               #Sample networks to combine 
               .[data.table(NETWORK = 1:P,
                            INDEX_A = sample(x = INDEX_A, size = P, replace = TRUE),
                            INDEX_1 = sample(x = INDEX_P, size = P, replace = TRUE),
                            INDEX_2 = sample(x = INDEX_P, size = P, replace = TRUE),
                            INDEX_3 = sample(x = INDEX_P, size = P, replace = TRUE),
                            INDEX_4 = sample(x = INDEX_P, size = P, replace = TRUE)), on = "NETWORK"] %>%
              
               #Retrieve networks to combine
               .[ T_W[CLASS == "ARCHIVE" & NETWORK %in% .$INDEX_A, .(NETWORK, NN_LAYER, NN_ROW, NN_COL, TYPE, VALUE_A = VALUE)], 
                  on = c(INDEX_A = "NETWORK", NN_LAYER = "NN_LAYER", NN_ROW = "NN_ROW", NN_COL = "NN_COL", TYPE = "TYPE")] %>%
              
               .[ T_W[CLASS == "PARENT"  & NETWORK %in% .$INDEX_1, .(NETWORK, NN_LAYER, NN_ROW, NN_COL, TYPE, VALUE_1 = VALUE)], 
                  on = c(INDEX_1 = "NETWORK", NN_LAYER = "NN_LAYER", NN_ROW = "NN_ROW", NN_COL = "NN_COL", TYPE = "TYPE")] %>%
              
               .[ T_W[CLASS == "PARENT"  & NETWORK %in% .$INDEX_2, .(NETWORK, NN_LAYER, NN_ROW, NN_COL, TYPE, VALUE_2 = VALUE)], 
                  on = c(INDEX_2 = "NETWORK", NN_LAYER = "NN_LAYER", NN_ROW = "NN_ROW", NN_COL = "NN_COL", TYPE = "TYPE")] %>%
              
               .[ T_W[CLASS == "PARENT"  & NETWORK %in% .$INDEX_3, .(NETWORK, NN_LAYER, NN_ROW, NN_COL, TYPE, VALUE_3 = VALUE)], 
                  on = c(INDEX_3 = "NETWORK", NN_LAYER = "NN_LAYER", NN_ROW = "NN_ROW", NN_COL = "NN_COL", TYPE = "TYPE")] %>%
              
               .[ T_W[CLASS == "PARENT"  & NETWORK %in% .$INDEX_4, .(NETWORK, NN_LAYER, NN_ROW, NN_COL, TYPE, VALUE_4 = VALUE)], 
                  on = c(INDEX_4 = "NETWORK", NN_LAYER = "NN_LAYER", NN_ROW = "NN_ROW", NN_COL = "NN_COL", TYPE = "TYPE")] %>%
            
               #Generate children networks 
               .[, SELECTION_CROSS_OVER := sample(x = 0:1, size = .N, replace = TRUE)] %>%
               .[, SELECTION_MUTATION   := sample(x = 0:1, size = .N, replace = TRUE, prob = c(1 - OPT_MUTATION_FRQ, OPT_MUTATION_FRQ))] %>%
               .[, MUTATION             := rnorm(n = .N, mean = 0, sd = OPT_MUTATION_SEV)] %>%
               .[, VALUE_CANDIDATE      := VALUE_A + (VALUE_1 - VALUE_2) + (VALUE_3 - VALUE_4)] %>%
               .[, VALUE                := VALUE_O * SELECTION_CROSS_OVER + VALUE_CANDIDATE * (1 - SELECTION_CROSS_OVER) + MUTATION * SELECTION_MUTATION] %>%
               .[, CLASS                := "CHILDREN"] %>%
              
               #Select columns to keep
               .[, .(NETWORK, NN_LAYER, NN_ROW, NN_COL, TYPE, VALUE, CLASS)] %>%
              
               #Sort by decreasing network index
               .[order(NETWORK, NN_LAYER, NN_ROW, NN_COL, TYPE)] %>%
              
               #Retrieve archive and parent networks
               rbind(copy(T_W), .) %>%
              
               #Set primary key
               setkey(., CLASS, NETWORK, NN_LAYER, NN_ROW, NN_COL, TYPE)
  
    else                  
  
               #SYMBOLIC
               T_SR_WEIGHT<-copy(T_W)[
    
               #Filter parent networks
               CLASS == "PARENT",  .(NETWORK, VARIABLE, TYPE, INDEX, VALUE_O = VALUE)] %>%
              
               #Sample networks to combine 
               .[data.table(NETWORK = 1:P,
                            INDEX_A = sample(x = INDEX_A, size = P, replace = TRUE),
                            INDEX_1 = sample(x = INDEX_P, size = P, replace = TRUE),
                            INDEX_2 = sample(x = INDEX_P, size = P, replace = TRUE),
                            INDEX_3 = sample(x = INDEX_P, size = P, replace = TRUE),
                            INDEX_4 = sample(x = INDEX_P, size = P, replace = TRUE)), on = "NETWORK"] %>%
              
               #Retrieve networks to combine
               .[ T_W[CLASS == "ARCHIVE" & NETWORK %in% .$INDEX_A, .(NETWORK, VARIABLE, TYPE, INDEX, VALUE_A = VALUE)], 
                  on = c(INDEX_A = "NETWORK", VARIABLE ="VARIABLE", TYPE = "TYPE", INDEX = "INDEX")] %>%
              
               .[ T_W[CLASS == "PARENT"  & NETWORK %in% .$INDEX_1, .(NETWORK, VARIABLE, TYPE, INDEX, VALUE_1 = VALUE)], 
                  on = c(INDEX_1 = "NETWORK", VARIABLE ="VARIABLE", TYPE = "TYPE", INDEX = "INDEX")] %>%
              
               .[ T_W[CLASS == "PARENT"  & NETWORK %in% .$INDEX_2, .(NETWORK, VARIABLE, TYPE, INDEX, VALUE_2 = VALUE)], 
                  on = c(INDEX_2 = "NETWORK", VARIABLE ="VARIABLE", TYPE = "TYPE", INDEX = "INDEX")] %>%
              
               .[ T_W[CLASS == "PARENT"  & NETWORK %in% .$INDEX_3, .(NETWORK, VARIABLE, TYPE, INDEX, VALUE_3 = VALUE)], 
                  on = c(INDEX_3 = "NETWORK", VARIABLE ="VARIABLE", TYPE = "TYPE", INDEX = "INDEX")] %>%
              
               .[ T_W[CLASS == "PARENT"  & NETWORK %in% .$INDEX_4, .(NETWORK, VARIABLE, TYPE, INDEX, VALUE_4 = VALUE)], 
                  on = c(INDEX_4 = "NETWORK", VARIABLE ="VARIABLE", TYPE = "TYPE", INDEX = "INDEX")] %>%
              
               #Generate children networks 
               .[, SELECTION_CROSS_OVER := sample(x = 0:1, size = .N, replace = TRUE)] %>%
               .[, SELECTION_MUTATION   := sample(x = 0:1, size = .N, replace = TRUE, prob = c(1 - OPT_MUTATION_FRQ, OPT_MUTATION_FRQ))] %>%
               
               .[, MUTATION_EXPRESSION  := sample(x = 1:E_MAX, size = .N, replace = TRUE)] %>%
               .[, MUTATION_FUNCTION    := sample(x = 1:F_MAX, size = .N, replace = TRUE)] %>%  
               .[, MUTATION_FLOAT       := rnorm(n = .N, mean = 0, sd = OPT_MUTATION_SEV)] %>%
              
               .[, VALUE_TEMP_EXPRESSION := sample(x = c(0, 1, 2, 3, 4), size = .N, replace = TRUE)] %>%
               .[, VALUE_TEMP_FUNCTION   := sample(x = c(0, 1, 2, 3, 4), size = .N, replace = TRUE)] %>%
               .[, VALUE_TEMP_FLOAT      := VALUE_A + (VALUE_1 - VALUE_2) + (VALUE_3 - VALUE_4)] %>%
              
               .[, VALUE_CANDIDATE_EXPRESSION := ifelse(SELECTION_CROSS_OVER  == 1, VALUE_O,
                                                 ifelse(SELECTION_MUTATION    == 1, MUTATION_EXPRESSION,       
                                                 ifelse(VALUE_TEMP_EXPRESSION == 0, VALUE_A,
                                                 ifelse(VALUE_TEMP_EXPRESSION == 1, VALUE_1,
                                                 ifelse(VALUE_TEMP_EXPRESSION == 2, VALUE_2, 
                                                 ifelse(VALUE_TEMP_EXPRESSION == 3, VALUE_3,
                                                 ifelse(VALUE_TEMP_EXPRESSION == 4, VALUE_4, 0)))))))] %>%
              
               .[, VALUE_CANDIDATE_FUNCTION   := ifelse(SELECTION_CROSS_OVER  == 1, VALUE_O,
                                                 ifelse(SELECTION_MUTATION    == 1, MUTATION_FUNCTION,       
                                                 ifelse(VALUE_TEMP_FUNCTION   == 0, VALUE_A,
                                                 ifelse(VALUE_TEMP_FUNCTION   == 1, VALUE_1,
                                                 ifelse(VALUE_TEMP_FUNCTION   == 2, VALUE_2, 
                                                 ifelse(VALUE_TEMP_FUNCTION   == 3, VALUE_3,
                                                 ifelse(VALUE_TEMP_FUNCTION   == 4, VALUE_4, 0)))))))] %>%  
              
               #--- Test effect of rescaling numerical elements (bias, coefficient) -> to return in original code, just keep the VALUE_CANDIDATE_FLOAT line now in comment      
               #.[, VALUE_CANDIDATE_FLOAT      := VALUE_O * SELECTION_CROSS_OVER + VALUE_TEMP_FLOAT * (1 - SELECTION_CROSS_OVER) + MUTATION_FLOAT * SELECTION_MUTATION] %>%
               .[, VALUE_CANDIDATE_FLOAT_TEMP   := VALUE_O * SELECTION_CROSS_OVER + VALUE_TEMP_FLOAT * (1 - SELECTION_CROSS_OVER) + MUTATION_FLOAT * SELECTION_MUTATION] %>%      

               .[, FLOAT_FLAG                   := ifelse(TYPE == "BIAS", "FLOAT", ifelse(TYPE == "COEFFICIENT", "FLOAT", TYPE))] %>%
               .[, VALUE_CANDIDATE_FLOAT_SCALE  := sum(VALUE_CANDIDATE_FLOAT_TEMP), keyby = .(NETWORK, VARIABLE, FLOAT_FLAG)]     %>%      
               .[, VALUE_CANDIDATE_FLOAT        := VALUE_CANDIDATE_FLOAT_TEMP / VALUE_CANDIDATE_FLOAT_SCALE]                      %>%
               #---    
      
               .[, VALUE := ifelse(TYPE == "EXPRESSION", VALUE_CANDIDATE_EXPRESSION,
                            ifelse(TYPE == "FUNCTION",   VALUE_CANDIDATE_FUNCTION, 
                                                         VALUE_CANDIDATE_FLOAT))]  %>%

               .[, CLASS := "CHILDREN"] %>%
              
               #Select columns to keep
               .[, .(NETWORK, VARIABLE, TYPE, INDEX, VALUE, CLASS)] %>%
              
               #Sort by decreasing network index
               .[order(NETWORK, VARIABLE, TYPE, INDEX)] %>%
              
               #Retrieve archive and parent networks
               rbind(copy(T_W), .) %>%
              
               #Set primary key
               setkey(., CLASS, NETWORK, VARIABLE, TYPE, INDEX)
    
    }

  #Transformation - Create children network - score
  T_NN_SCORE<-copy(T_S)[
    
              #Filter parent networks
              CLASS == "PARENT"][, 
                                 
              #Prepare score table for children networks
              ':=' (RETURN  = 0, RISK = 0, PARETO = 0, CROWD = 0, CLASS = "CHILDREN")] %>%
              
              #Retrieve archive and parent networks
              rbind(copy(T_S), .) %>%
              
              #Set primary key
              setkey(., CLASS, NETWORK)
  
  #Transformation - Create output list
  out<-{ if (RUN_TYPE == "NN") list(T_NN_SCORE  = T_NN_SCORE, T_NN_WEIGHT = T_NN_WEIGHT,                 T_SR_WEIGHT = L_NN_POPULATION$T_SR_WEIGHT)
         else                  list(T_NN_SCORE  = T_NN_SCORE, T_NN_WEIGHT = L_NN_POPULATION$T_NN_WEIGHT, T_SR_WEIGHT = T_SR_WEIGHT )} 
  
  #Output
  return(out)
  
}
F_C_04_SAMPLE<-function(r, g, RUN_TYPE, T_SETTING_RUN, T_SETTING_ENVIRONMENT, T_SR_STRUCTURE, T_NN_STRUCTURE, T_NN_LIBRARY, L_NN_POPULATION){
  
  #Data Container
  T_S<-copy(L_NN_POPULATION$T_NN_SCORE)                                       #Networks score
  T_W<-{ if (RUN_TYPE == "NN") copy(L_NN_POPULATION$T_NN_WEIGHT)              #Networks weight/bias
         else                  copy(L_NN_POPULATION$T_SR_WEIGHT) }            
  
  #Transformation - Weight/Bias
  T_W<-{ if (RUN_TYPE == "NN") rbind(T_W[CLASS == "ARCHIVE"], 
                                     F_A_02_NN_WEIGHT(r, g, "CHILDREN", T_SETTING_RUN, T_NN_STRUCTURE))
  
         else                  rbind(T_W[CLASS == "ARCHIVE"], 
                                     F_A_03_SR_WEIGHT(r, g, "CHILDREN", T_SETTING_RUN, T_SETTING_ENVIRONMENT, T_SR_STRUCTURE, T_NN_LIBRARY))}

  #Transformation - Score
  T_S<-T_S[, 
           
           #Clear score on non-optimal solutions
           RETURN := ifelse(CLASS == "ARCHIVE", RETURN, 0)][,
           RISK   := ifelse(CLASS == "ARCHIVE", RISK, 0)][,
                                                                                                           
           #Update class flag                                                                            
           CLASS := ifelse(CLASS == "ARCHIVE", CLASS, "CHILDREN")]
  
  #Transformation - Create output list
  out<-{ if (RUN_TYPE == "NN") list(T_NN_SCORE  = T_S, T_NN_WEIGHT = T_W,                         T_SR_WEIGHT = L_NN_POPULATION$T_SR_WEIGHT)
         else                  list(T_NN_SCORE  = T_S, T_NN_WEIGHT = L_NN_POPULATION$T_NN_WEIGHT, T_SR_WEIGHT = T_W )} 

  #Output
  return(out)
  
}
F_C_05_SELECT<-function(r, g, RUN_TYPE, T_SETTING_RUN, L_NN_POPULATION){
  
  #Reference
  RUN_SEED<-T_SETTING_RUN[RUN_INDEX == r, RUN_SEED]                                                                    #Random number generator seed  
  G<-T_SETTING_RUN[RUN_INDEX == r, OPT_GENERATION]                                                                     #Number of generation
  P<-L_NN_POPULATION$T_NN_SCORE[CLASS == "PARENT",  max(NETWORK)]                                                      #Number of parent networks
  N<-{ if (RUN_TYPE == "NN") L_NN_POPULATION$T_NN_WEIGHT[NETWORK == 1 & CLASS == "PARENT",  .N]                        #Number of parameter per network
       else                  L_NN_POPULATION$T_SR_WEIGHT[NETWORK == 1 & CLASS == "PARENT",  .N] }

  #Data Container
  T_S<-copy(L_NN_POPULATION$T_NN_SCORE)                                                                                #Networks score
  T_W<-{ if (RUN_TYPE == "NN") copy(L_NN_POPULATION$T_NN_WEIGHT)                                                       #Networks weight/bias
         else                  copy(L_NN_POPULATION$T_SR_WEIGHT) }            

  #Transformation - Rank network
  T_S<-F_C_01_RANK(T_S[CLASS != "ARCHIVE"]) 
  
  #Transformation - Truncate Population - score
  T_S<-
       #Combine parent and children networks
       merge(T_S[CLASS == "PARENT",   .(NETWORK, RETURN_P = RETURN, RISK_P = RISK)],
             T_S[CLASS == "CHILDREN", .(NETWORK, RETURN_C = RETURN, RISK_C = RISK)],
             by = "NETWORK") %>%
      
       #Compare parent and children score
       .[, KEEP := ifelse( (RISK_C <= RISK_P & RETURN_C > RETURN_P) | (RISK_C < RISK_P & RETURN_C >= RETURN_P), "CHILDREN",
                   ifelse( (RISK_P <= RISK_C & RETURN_P > RETURN_C) | (RISK_P < RISK_C & RETURN_P >= RETURN_C), "PARENT",
                                                                                                                "TIE"))] %>%
       .[, .(NETWORK, KEEP)] %>%
      
       #Retrieve network score
       merge(T_S[CLASS != "ARCHIVE"], ., by = "NETWORK") %>%
      
       #Drop dominated networks
       .[, SELECT := ifelse( CLASS == KEEP | KEEP == "TIE", 1, 0)] %>%
       .[SELECT == 1,] %>%
      
       #Create temporary key
       .[, KEY_TEMP := paste(CLASS, NETWORK, sep = "_")] %>%
      
       #Sort by Pareto & Crow
       .[order(PARETO, -rank(CROWD)), .(NETWORK, RETURN, RISK, CLASS, PARETO, CROWD, KEY_TEMP)] %>%

       #Truncate population to original dimension
       .[1:P] %>%
      
       #Sort by temporary key
       .[order(KEY_TEMP)]
  
  #Transformation - Truncate Population - weight/bias
  T_W<-
       #Select parent/children networks
       T_W[CLASS != "ARCHIVE"][, 
                            
       #Create temporary key
       KEY_TEMP := paste(CLASS, NETWORK, sep = "_")][
                              
       #Drop dominated networks  
       KEY_TEMP %in% T_S$KEY_TEMP][
                                
       #Sort by temporary key 
       order(KEY_TEMP)]
  
  #Transformation - Create output list
  T_W[, ':=' (CLASS = "PARENT", KEY_TEMP = NULL, NETWORK = sort(rep(1:P, N)))]                                         #Restore original network references - weight/bias
  T_S[, ':=' (CLASS = "PARENT", KEY_TEMP = NULL, NETWORK = 1:P)]                                                       #Restore original network references - score
  
  out<-{ if (RUN_TYPE == "NN") list(T_NN_SCORE  = rbind(copy(L_NN_POPULATION$T_NN_SCORE)[CLASS == "ARCHIVE",],  T_S),  #Create output list - score
                                    T_NN_WEIGHT = rbind(copy(L_NN_POPULATION$T_NN_WEIGHT)[CLASS == "ARCHIVE",], T_W),  #Create output list - weight/bias - NUMERIC
                                    T_SR_WEIGHT = copy(L_NN_POPULATION$T_SR_WEIGHT))                                   #Create output list - weight/bias - SYMBOLIC

         else                  list(T_NN_SCORE  = rbind(copy(L_NN_POPULATION$T_NN_SCORE)[CLASS == "ARCHIVE",],  T_S),  #Create output list - score
                                    T_NN_WEIGHT = copy(L_NN_POPULATION$T_NN_WEIGHT),                                   #Create output list - weight/bias - NUMERIC
                                    T_SR_WEIGHT = rbind(copy(L_NN_POPULATION$T_SR_WEIGHT)[CLASS == "ARCHIVE",], T_W))} #Create output list - weight/bias - SYMBOLIC
    
  #Output
  return(out)
  
}
F_C_06_RUN<-function(r, RUN_TYPE, OPT_TYPE, T_SETTING_MACRO, T_SETTING_RUN, T_SETTING_ENVIRONMENT, T_NN_STRUCTURE, T_SR_STRUCTURE){
  
  #Reference
  G<-T_SETTING_RUN[RUN_INDEX == r, OPT_GENERATION]          #Number of generation
  g<-0                                                      #Generation index
  COL<-ifelse(OPT_TYPE == "ADEMO", "blue", "red")           #Efficient frontier color (graph)
  
  #Generate library of random expression
  T_NN_LIBRARY<-F_A_01_LIBRARY(r, T_SETTING_RUN, T_SETTING_ENVIRONMENT, T_SR_STRUCTURE)

  #Initialize population
  L_NN_POPULATION<-list(T_NN_SCORE  = F_A_07_SCORE(),
                        T_NN_WEIGHT = F_A_02_NN_WEIGHT(r, g, "PARENT", T_SETTING_RUN, T_NN_STRUCTURE), 
                        T_SR_WEIGHT = F_A_03_SR_WEIGHT(r, g, "PARENT", T_SETTING_RUN, T_SETTING_ENVIRONMENT, T_SR_STRUCTURE, T_NN_LIBRARY))

  #Evaluate initial generation
  L_NN_POPULATION<-F_B_05_RUN(r, g, "PARENT", RUN_TYPE, T_SETTING_MACRO, T_SETTING_RUN, T_SETTING_ENVIRONMENT, T_NN_LIBRARY, L_NN_POPULATION$T_NN_WEIGHT, L_NN_POPULATION$T_SR_WEIGHT, L_NN_POPULATION$T_NN_SCORE)

  #Update archive
  L_NN_POPULATION<-F_C_02_ARCHIVE(RUN_TYPE, L_NN_POPULATION)
  
  #Plot
  F_Z_05_GRAPH_1(1, r, g, RUN_TYPE, OPT_TYPE, COL, L_NN_POPULATION)

  #Optimization Search
  for(g in 1:G){
    
    #Reproduction
    L_NN_POPULATION<-{ if (OPT_TYPE == "ADEMO") F_C_03_REPRODUCE(r, g, RUN_TYPE, T_SETTING_RUN, T_NN_LIBRARY, L_NN_POPULATION)
                       else                     F_C_04_SAMPLE(r, g, RUN_TYPE, T_SETTING_RUN, T_SETTING_ENVIRONMENT, T_SR_STRUCTURE, T_NN_STRUCTURE, T_NN_LIBRARY, L_NN_POPULATION) }
    
    #Evaluation
    L_NN_POPULATION<-F_B_05_RUN(r, g, "CHILDREN", RUN_TYPE, T_SETTING_MACRO, T_SETTING_RUN, T_SETTING_ENVIRONMENT, T_NN_LIBRARY, L_NN_POPULATION$T_NN_WEIGHT, L_NN_POPULATION$T_SR_WEIGHT, L_NN_POPULATION$T_NN_SCORE)

    #Selection
    L_NN_POPULATION<-{ if (OPT_TYPE == "ADEMO") F_C_05_SELECT(r, g, RUN_TYPE, T_SETTING_RUN, L_NN_POPULATION)
                       else                     L_NN_POPULATION }
    
    #Update archive
    L_NN_POPULATION<-F_C_02_ARCHIVE(RUN_TYPE, L_NN_POPULATION)
    
    #Plot
    F_Z_05_GRAPH_1(1, r, g, RUN_TYPE, OPT_TYPE, COL, L_NN_POPULATION)
    
  }
  
  #Filter efficient networks
  L_NN_POPULATION$T_NN_SCORE<-L_NN_POPULATION$T_NN_SCORE[CLASS == "ARCHIVE"]
  L_NN_POPULATION$T_NN_WEIGHT<-if (RUN_TYPE == "NN") L_NN_POPULATION$T_NN_WEIGHT[CLASS == "ARCHIVE"] else NULL
  L_NN_POPULATION$T_SR_WEIGHT<-if (RUN_TYPE == "SR") L_NN_POPULATION$T_SR_WEIGHT[CLASS == "ARCHIVE"] else NULL

  #Extract expressions
  L_NN_POPULATION$T_NN_EXPRESSION<-if (RUN_TYPE == "SR") F_Z_02_FROM_REF_TO_EXPR(r, "ARCHIVE", T_SETTING_RUN, T_SETTING_ENVIRONMENT, T_SR_STRUCTURE, T_NN_LIBRARY, L_NN_POPULATION$T_SR_WEIGHT) else NULL

  #Plot
  F_Z_05_GRAPH_1(2, r, g, RUN_TYPE, OPT_TYPE, COL, L_NN_POPULATION)
  
  #Output
  return(L_NN_POPULATION)
  
}

#Analysis & Plot
F_D_01_COMPARE<-function(r, RUN_TYPE, T_SETTING_RUN, T_NN_COMPARE, L_XX_ES_POPULATION, L_XX_MS_POPULATION){
  
  #Reference
  R_TYPE<-RUN_TYPE
  OPT_REF_RISK<-T_SETTING_RUN[RUN_INDEX == r, OPT_REF_RISK]                                #Hypervolume reference point - risk
  OPT_REF_RETURN<-T_SETTING_RUN[RUN_INDEX == r, OPT_REF_RETURN]                            #Hypervolume reference point - return
  
  #Data Container
  T_XX_ES_SCORE<-copy(L_XX_ES_POPULATION$T_NN_SCORE)                                       #Efficient solution - ADEMO
  T_XX_MS_SCORE<-copy(L_XX_MS_POPULATION$T_NN_SCORE)                                       #Efficient solution - UMCS
  T_XX_CB_SCORE<-rbind(T_XX_ES_SCORE, copy(T_XX_MS_SCORE)[, NETWORK := - NETWORK])         #Combined list of solutions 
  
  #Transformation - Rank solutions
  T_XX_ES_SCORE<-F_C_01_RANK(T_XX_ES_SCORE)
  T_XX_MS_SCORE<-F_C_01_RANK(T_XX_MS_SCORE)
  T_XX_CB_SCORE<-F_C_01_RANK(T_XX_CB_SCORE)
  
  #Transformation - Hypervolume
  TEMP_ES_SCORE<-rbind(data.table(RETURN = 0, RISK = 0), copy(T_XX_ES_SCORE[order(RISK, RETURN)][,.(RETURN, RISK)]), data.table(RETURN = OPT_REF_RETURN, RISK = OPT_REF_RISK))
  TEMP_MS_SCORE<-rbind(data.table(RETURN = 0, RISK = 0), copy(T_XX_MS_SCORE[order(RISK, RETURN)][,.(RETURN, RISK)]), data.table(RETURN = OPT_REF_RETURN, RISK = OPT_REF_RISK))

  T_NN_COMPARE[RUN_INDEX == r & RUN_TYPE == R_TYPE & OPT_TYPE == "ADEMO", "HYPERVOLUME"]<-trapz(TEMP_ES_SCORE[,RISK], TEMP_ES_SCORE[,RETURN]) / (OPT_REF_RETURN * OPT_REF_RISK)
  T_NN_COMPARE[RUN_INDEX == r & RUN_TYPE == R_TYPE & OPT_TYPE == "UMCS",  "HYPERVOLUME"]<-trapz(TEMP_MS_SCORE[,RISK], TEMP_MS_SCORE[,RETURN]) / (OPT_REF_RETURN * OPT_REF_RISK)
  
  #Transformation - Spacing
  T_NN_COMPARE[RUN_INDEX == r & RUN_TYPE == R_TYPE & OPT_TYPE == "ADEMO", "SPACING"]<-mean(T_XX_ES_SCORE$CROWD[T_XX_ES_SCORE$CROWD!=Inf])
  T_NN_COMPARE[RUN_INDEX == r & RUN_TYPE == R_TYPE & OPT_TYPE == "UMCS",  "SPACING"]<-mean(T_XX_MS_SCORE$CROWD[T_XX_MS_SCORE$CROWD!=Inf])
  
  #Transformation - Cardinality
  T_NN_COMPARE[RUN_INDEX == r & RUN_TYPE == R_TYPE & OPT_TYPE == "ADEMO", "CARDINALITY"]<-nrow(T_XX_ES_SCORE)
  T_NN_COMPARE[RUN_INDEX == r & RUN_TYPE == R_TYPE & OPT_TYPE == "UMCS",  "CARDINALITY"]<-nrow(T_XX_MS_SCORE)
  
  #Transformation - Range
  T_NN_COMPARE[RUN_INDEX == r & RUN_TYPE == R_TYPE & OPT_TYPE == "ADEMO", "RANGE"]<-(max(T_XX_ES_SCORE$RISK) - min(T_XX_ES_SCORE$RISK)) + (max(T_XX_ES_SCORE$RETURN) - min(T_XX_ES_SCORE$RETURN)) / OPT_REF_RETURN
  T_NN_COMPARE[RUN_INDEX == r & RUN_TYPE == R_TYPE & OPT_TYPE == "UMCS",  "RANGE"]<-(max(T_XX_MS_SCORE$RISK) - min(T_XX_MS_SCORE$RISK)) + (max(T_XX_MS_SCORE$RETURN) - min(T_XX_MS_SCORE$RETURN)) / OPT_REF_RETURN
  
  #Transformation - Dominance
  T_NN_COMPARE[RUN_INDEX == r & RUN_TYPE == R_TYPE & OPT_TYPE == "ADEMO", "DOMINANCE"]<-nrow(T_XX_CB_SCORE[PARETO == 1 & NETWORK > 0]) / nrow(T_XX_CB_SCORE[PARETO == 1])
  T_NN_COMPARE[RUN_INDEX == r & RUN_TYPE == R_TYPE & OPT_TYPE == "UMCS",  "DOMINANCE"]<-nrow(T_XX_CB_SCORE[PARETO == 1 & NETWORK < 0]) / nrow(T_XX_CB_SCORE[PARETO == 1])
  
  #Output
  return(T_NN_COMPARE)
  
}
F_D_02_PREDICT<-function(r, T_SETTING_RUN, T_SETTING_ENVIRONMENT, L_NN_POPULATION){
  
  #Reference
  COL_NAME<-c("NN_L_EXP", "NN_L_RESULT", "NN_L_SOLVENCY", "NN_L_SURPLUS", "ACT_DIVIDEND", "ACT_PREMIUM", "ACT_RI", "ACT_RISK_APPETITE", "ACT_SURPLUS")
  
  #Data Container
  T_NN_MATRIX<-F_A_04_MATRIX("ARCHIVE", L_NN_POPULATION$T_NN_WEIGHT)                                                 #Convert optimal network  weight/bias into matrix format
  T_RL_CONTROL<-F_A_09_GRID(r, T_SETTING_RUN, T_SETTING_ENVIRONMENT, L_NN_POPULATION)                                #Sample environment states
  
  #Transformation
  out<-F_B_01_MOVE(r, "NN", T_SETTING_RUN, T_SETTING_ENVIRONMENT, T_NN_MATRIX, T_NN_EXPRESSION, T_RL_CONTROL) %>%    #Assign action to environment states
       dcast(., NETWORK + SIMULATION ~ ENV_LOB, value.var = COL_NAME)                                                #Reshape table from long to wide      

  #Output
  return(out)
  
}
F_D_03_ANALYZE_WEIGHT<-function(r, L_NN_POPULATION){
  
  #Data Container
  T_NN_WEIGHT<-copy(L_NN_POPULATION$T_NN_WEIGHT)
  
  #Transformation
  out<-T_NN_WEIGHT[, RUN_INDEX := r][, 
                     .(RUN_INDEX, NETWORK, NN_LAYER, VALUE)] %>%
       group_by(RUN_INDEX, NETWORK, NN_LAYER) %>% 
       summarise_all(funs(mean, sd, skewness, min, max, quantile(., probs = 0.25), quantile(., probs = 0.50), quantile(., probs = 0.75), sum)) %>%
       mutate(range = max - min, CoV   = sd / mean) %>%
       as.data.table %>%
       setkey(RUN_INDEX, NETWORK)
  
  names(out)<-c("RUN_INDEX", "NETWORK", "NN_LAYER","MEAN", "SD", "SKEWNESS", "MIN", "MAX", "Q25", "Q50", "Q75", "SUM", "RANGE", "COV")
  
  #Output
  return(out)
  
}
F_D_04_ANALYZE_POLICY<-function(r, T_SETTING_ENVIRONMENT, T_NN_PREDICT){
  
  #Reference
  L<-T_SETTING_ENVIRONMENT[RUN_INDEX == r, max(ENV_LOB)]
  COL_NAME<-list(ACT_DIVIDEND = paste("ACT_DIVIDEND", 1:L, sep = "_"), 
                 ACT_PREMIUM = paste("ACT_PREMIUM", 1:L, sep = "_"), 
                 ACT_RI = paste("ACT_RI", 1:L, sep = "_"), 
                 ACT_RISK_APPETITE = paste("ACT_RISK_APPETITE", 1:L, sep = "_"), 
                 ACT_SURPLUS = paste("ACT_SURPLUS", 1:L, sep = "_"))
  
  #Data Container
  T_PREDICT<-copy(T_NN_PREDICT) %>%
             .[, RUN_INDEX := r] %>%
             melt(., id.vars = c("NETWORK", "SIMULATION", "RUN_INDEX"), measure.vars = COL_NAME, variable.name = "ENV_LOB")
  
  #Transformation
  out<-rbind(T_PREDICT[, .(RUN_INDEX, NETWORK, ENV_LOB, VALUE = ACT_DIVIDEND)][,      VARIABLE := "ACT_DIVIDEND"],
             T_PREDICT[, .(RUN_INDEX, NETWORK, ENV_LOB, VALUE = ACT_PREMIUM)][,       VARIABLE := "ACT_PREMIUM"],
             T_PREDICT[, .(RUN_INDEX, NETWORK, ENV_LOB, VALUE = ACT_RI)][,            VARIABLE := "ACT_RI"],
             T_PREDICT[, .(RUN_INDEX, NETWORK, ENV_LOB, VALUE = ACT_RISK_APPETITE)][, VARIABLE := "ACT_RISK_APPETITE"],
             T_PREDICT[, .(RUN_INDEX, NETWORK, ENV_LOB, VALUE = ACT_SURPLUS)][,       VARIABLE := "ACT_SURPLUS"]) %>%
    
       group_by(RUN_INDEX, NETWORK, ENV_LOB, VARIABLE) %>% 
       summarise_all(funs(mean, sd, skewness, min, max, quantile(., probs = 0.25), quantile(., probs = 0.50), quantile(., probs = 0.75))) %>%
       mutate(range = max - min, CoV   = sd / mean) %>%
       as.data.table %>%
       setkey(RUN_INDEX, NETWORK, ENV_LOB, VARIABLE)
  
  names(out)<-c("RUN_INDEX", "NETWORK", "ENV_LOB", "VARIABLE", "MEAN", "SD", "SKEWNESS", "MIN", "MAX", "Q25", "Q50", "Q75", "RANGE", "COV")
  
  #Output
  return(out)
  
}
F_D_05_IMITATE<-function(r, T_SETTING_ENVIRONMENT, L_NN_POPULATION, T_NN_PREDICT){
  
  #Reference
  N<-L_NN_POPULATION$T_NN_SCORE[, max(NETWORK)]                                         #Number of networks
  L<-T_SETTING_ENVIRONMENT[RUN_INDEX == r, max(ENV_LOB)]                                #Number of LoBs
  V_I<-names(T_NN_PREDICT)[grepl("NN_",  names(T_NN_PREDICT))]                          #Variable names: input
  V_O<-names(T_NN_PREDICT)[grepl("ACT_", names(T_NN_PREDICT))]                          #Variable names: output
  A_LOW<-L_NN_POPULATION$T_NN_SCORE[order(RISK, RETURN), NETWORK][1]                    #Agent reference: low risk low return
  A_HGH<-L_NN_POPULATION$T_NN_SCORE[order(RISK, RETURN), NETWORK][N]                    #Agent reference: high risk high return
  
  #Data Container
  L_DT_POPULATION<-list()                                                               #Imitation agents container - Population
  L_DT_SINGLE<-list()                                                                   #Imitation agents container - Single
  
  #Transformation
  for(n in 1:N){                                                                        #For each black box agent
    
    for(j in 1:length(V_O)){                                                            #For each control variable
      
      VR<-c(V_O[j], V_I)                                                                #Define regression variables
      FM<-as.formula(paste0(V_O[j], "~ ."))                                             #Define regression formula
      L_DT_SINGLE[[j]]<-rpart(formula = FM, data = T_NN_PREDICT[NETWORK == n, ..VR])    #Fit regression tree
      names(L_DT_SINGLE)[[j]]<-V_O[j]                                                   #Define regression name
      if(n == A_LOW | n == A_HGH){
        options(warn = -1)                                                              #Turn off warning message
        F_Z_07_GRAPH_3(r, n, j, V_O, L_DT_SINGLE)}                                      #Plot
        options(warn = 1)                                                               #Turn on  warning message
    }
    
    L_DT_POPULATION[[n]]<-L_DT_SINGLE                                                    #Store white box agent
    message("Run: " , r, " - Imitation Agent: ", n, sep = "")                            #Message
    
  }
  
  #Output
  return(L_DT_POPULATION)
  
}
F_D_06_EXTRACT<-function(r, L_NN_POPULATION, L_DT_POPULATION, T_NN_PREDICT){
  
  #Reference
  N<-L_NN_POPULATION$T_NN_SCORE[, max(NETWORK)]                                         #Number of networks
  J<-length(names(T_NN_PREDICT)[grepl("ACT_", names(T_NN_PREDICT))])                    #Number of regression variables
  
  #Transformation
  for(n in 1:N){                                                                        #For each white box agent
    
    for(j in 1:J){                                                                      #For each control variable
      
      VAR_TMP<-names(L_DT_POPULATION[[n]])[j]                                           #Regression variable
      
      VAR<-substr(VAR_TMP, 1, nchar(VAR_TMP)-2)                                         #Extract lob
      ENV_LOB<-substr(VAR_TMP, nchar(VAR_TMP), nchar(VAR_TMP)) %>% as.numeric           #Extract variable name
      
      CHECK<-data.frame(IMPORTANCE = L_DT_POPULATION[[n]][[j]]$variable.importance) %>% nrow  #Check if tree is available
      
      if(CHECK != 0){
        
        #Extract error statistics
        T_DT_ERROR<-data.frame(L_DT_POPULATION[[n]][[j]]$cptable) %>%                   
                    rename("N_SPLIT"    = nsplit,
                           "ERROR_REAL" = rel.error,
                           "ERROR_X"    = xerror,
                           "X_STD"      = xstd) %>%
                    mutate(RUN_INDEX = r,
                           NETWORK = n,
                           ENV_LOB = ENV_LOB,
                           VARIABLE_OUT = VAR) %>%
                    select(one_of(c("RUN_INDEX", "NETWORK", "ENV_LOB", "VARIABLE_OUT", "N_SPLIT", "ERROR_REAL", "ERROR_X", "X_STD")))      
        
        #Extract variable importance
        T_DT_IMPORTANCE<-data.frame(IMPORTANCE = L_DT_POPULATION[[n]][[j]]$variable.importance) %>%
                         rownames_to_column() %>% 
                         rename("VARIABLE_TEMP" = rowname) %>%
                         rename("IMPORTANCE_TEMP" = IMPORTANCE) %>%
                         mutate(RUN_INDEX = r,
                                NETWORK = n,
                                ENV_LOB = ENV_LOB,
                                VARIABLE_OUT = VAR,
                                VARIABLE_IN  = VARIABLE_TEMP,
                                IMPORTANCE = IMPORTANCE_TEMP / sum(IMPORTANCE_TEMP)) %>%
                         select(one_of(c("RUN_INDEX", "NETWORK", "ENV_LOB", "VARIABLE_OUT", "VARIABLE_IN", "IMPORTANCE")))      
        
        #Store results
        OUT_ERROR<-if(n == 1 & j == 1){T_DT_ERROR}else{rbind(OUT_ERROR, T_DT_ERROR)}
        OUT_IMPORTANCE<-if(n == 1 & j == 1){T_DT_IMPORTANCE}else{rbind(OUT_IMPORTANCE, T_DT_IMPORTANCE)}
      }
      
    }
    
  }
  
  out<-list(T_DT_IMPORTANCE = OUT_IMPORTANCE, T_DT_ERROR = OUT_ERROR)
  
  #Output
  return(out)
  
}

#Master
F_E_01_RUN<-function(T_SETTING_MACRO, T_SETTING_RUN, T_SETTING_ENVIRONMENT, T_NN_STRUCTURE, T_SR_STRUCTURE){
  
  #Reference
  R<-T_SETTING_RUN[, max(RUN_INDEX)]               #Number of run

  #Transformation
  for (r in 1:R){
    
    #Search optimal strategies
    L_NN_ES_POPULATION<-F_C_06_RUN(r, "NN", "ADEMO", T_SETTING_MACRO, T_SETTING_RUN, T_SETTING_ENVIRONMENT, T_NN_STRUCTURE, T_SR_STRUCTURE) #NUMERIC  - ADEMO
    L_NN_MS_POPULATION<-F_C_06_RUN(r, "NN", "UMCS",  T_SETTING_MACRO, T_SETTING_RUN, T_SETTING_ENVIRONMENT, T_NN_STRUCTURE, T_SR_STRUCTURE) #NUMERIC  - UMCS
    L_SR_ES_POPULATION<-F_C_06_RUN(r, "SR", "ADEMO", T_SETTING_MACRO, T_SETTING_RUN, T_SETTING_ENVIRONMENT, T_NN_STRUCTURE, T_SR_STRUCTURE) #SYMBOLIC - ADEMO
    L_SR_MS_POPULATION<-F_C_06_RUN(r, "SR", "UMCS",  T_SETTING_MACRO, T_SETTING_RUN, T_SETTING_ENVIRONMENT, T_NN_STRUCTURE, T_SR_STRUCTURE) #SYMBOLIC - UMCS
    
    #Compare optimization algorithms
    T_NN_COMPARE<-F_A_08_COMPARE(r)                                                                                                         #Frontier comparison table
    T_NN_COMPARE<-F_D_01_COMPARE(r, "NN", T_SETTING_RUN, T_NN_COMPARE, L_NN_ES_POPULATION, L_NN_MS_POPULATION)                              #NUMERIC  - ADEMO vs UMCS
    T_NN_COMPARE<-F_D_01_COMPARE(r, "SR", T_SETTING_RUN, T_NN_COMPARE, L_SR_ES_POPULATION, L_SR_MS_POPULATION)                              #SYMBOLIC - ADEMO vs UMCS
    
    F_Z_06_GRAPH_2(1, r, "NN", L_NN_ES_POPULATION$T_NN_SCORE, L_NN_MS_POPULATION$T_NN_SCORE)                                                #NUMERIC  - ADEMO vs UMCS
    F_Z_06_GRAPH_2(1, r, "SR", L_SR_ES_POPULATION$T_NN_SCORE, L_SR_MS_POPULATION$T_NN_SCORE)                                                #SYMBOLIC - ADEMO vs UMCS
    F_Z_06_GRAPH_2(2, r, "SR", L_NN_ES_POPULATION$T_NN_SCORE, L_SR_ES_POPULATION$T_NN_SCORE)                                                #NUMERIC vs SYMBOLIC

    #Analyze black-box strategies
    T_NN_PREDICT<-F_D_02_PREDICT(r, T_SETTING_RUN, T_SETTING_ENVIRONMENT, L_NN_ES_POPULATION)                                               #Run black box agents on simulated input
    T_NN_ANALYZE_WEIGHT<-F_D_03_ANALYZE_WEIGHT(r, L_NN_ES_POPULATION)                                                                       #Calculate statistical properties of weights
    T_NN_ANALYZE_POLICY<-F_D_04_ANALYZE_POLICY(r, T_SETTING_ENVIRONMENT, T_NN_PREDICT)                                                      #Calculate statistical properties of actions
    
    #Fit imitation white-box strategies
    L_DT_ES_POPULATION<-F_D_05_IMITATE(r, T_SETTING_ENVIRONMENT, L_NN_ES_POPULATION, T_NN_PREDICT)                                          #Fit a regression tree on the black box agents
    L_DT_ES_STATISTICS<-F_D_06_EXTRACT(r, L_NN_ES_POPULATION, L_DT_ES_POPULATION, T_NN_PREDICT)                                             #Calculate statistical properties of imitation agents
    
    #Output
    T_RUN_RESULT<-F_Z_08_STORE(r, T_RUN_RESULT, L_NN_ES_POPULATION, L_NN_MS_POPULATION, L_SR_ES_POPULATION, L_SR_MS_POPULATION, T_NN_COMPARE, T_NN_ANALYZE_WEIGHT, T_NN_ANALYZE_POLICY, L_DT_ES_STATISTICS)
    
  }

}

#RUN ------------------------------------------------------------------------------------------------

#Setting
IN_FILE<-"Prototype.xlsx"
IN_DIR<-"C:/Users/andre/OneDrive/Desktop/Documenti/3.Universita/2.Progetto/DYNAMIC FINANCIAL MODELLING"
OUT_DIR<-"C:/Users/andre/OneDrive/Desktop/Documenti/3.Universita/2.Progetto/DYNAMIC FINANCIAL MODELLING/02.OUTPUT"

#Upload Tables
setwd(OUT_DIR)
IN_REF<-paste(IN_DIR, "/", IN_FILE, sep = "")

T_SETTING_MACRO<-read_excel(path = IN_REF,       sheet = "T_SETTING_MACRO")       %>% as.data.table
T_SETTING_RUN<-read_excel(path = IN_REF,         sheet = "T_SETTING_RUN")         %>% as.data.table
T_SETTING_ENVIRONMENT<-read_excel(path = IN_REF, sheet = "T_SETTING_ENVIRONMENT") %>% as.data.table
T_NN_STRUCTURE<-read_excel(path = IN_REF,        sheet = "T_NN_STRUCTURE")        %>% as.data.table
T_SR_STRUCTURE<-read_excel(path = IN_REF,        sheet = "T_SR_STRUCTURE")        %>% as.data.table

#Execute
#F_E_01_RUN(T_SETTING_MACRO, T_SETTING_RUN, T_SETTING_ENVIRONMENT, T_NN_STRUCTURE, T_SR_STRUCTURE)

#HELPER ---------------------------------------------------------------------------------------------

r<-1                  #Index: Run
i<-1                  #Index: Simulation
g<-0                  #Index: Evolutionary Search
c<-"PARENT"           #Network class
RUN_TYPE<-"SR"        #Agent type (NN = neural network, SR = symbolic regression)
OPT_TYPE<-"UMCS"      #Optimization strategy

T_NN_LIBRARY    = F_A_01_LIBRARY(1, T_SETTING_RUN, T_SETTING_ENVIRONMENT, T_SR_STRUCTURE)
T_NN_WEIGHT     = F_A_02_NN_WEIGHT(1, 1, "PARENT", T_SETTING_RUN, T_NN_STRUCTURE)
T_SR_WEIGHT     = F_A_03_SR_WEIGHT(1, 1, "PARENT", T_SETTING_RUN, T_SETTING_ENVIRONMENT, T_SR_STRUCTURE, T_NN_LIBRARY)
T_RL_REWARD     = F_A_05_REWARD(1, T_SETTING_RUN)
T_RL_CONTROL    = F_A_06_CONTROL(1, T_SETTING_MACRO, T_SETTING_RUN, T_SETTING_ENVIRONMENT)
T_NN_SCORE      = F_A_07_SCORE()
T_NN_COMPARE    = F_A_08_COMPARE(1)
T_NN_MATRIX     = F_A_04_MATRIX("PARENT", T_NN_WEIGHT)
T_NN_EXPRESSION = F_Z_02_FROM_REF_TO_EXPR(1, "PARENT", T_SETTING_RUN, T_SETTING_ENVIRONMENT, T_SR_STRUCTURE, T_NN_LIBRARY, T_SR_WEIGHT)

L_NN_POPULATION<-F_B_05_RUN(1, 0, "PARENT", "NN", T_SETTING_MACRO, T_SETTING_RUN, T_SETTING_ENVIRONMENT, T_NN_LIBRARY, T_NN_WEIGHT, T_SR_WEIGHT, T_NN_SCORE)
L_SR_POPULATION<-F_B_05_RUN(1, 0, "PARENT", "SR", T_SETTING_MACRO, T_SETTING_RUN, T_SETTING_ENVIRONMENT, T_NN_LIBRARY, T_NN_WEIGHT, T_SR_WEIGHT, T_NN_SCORE)

L_NN_ES_POPULATION_R1<-F_C_06_RUN(1, "NN", "ADEMO", T_SETTING_MACRO, T_SETTING_RUN, T_SETTING_ENVIRONMENT, T_NN_STRUCTURE, T_SR_STRUCTURE)
L_NN_ES_POPULATION_R2<-F_C_06_RUN(2, "NN", "ADEMO", T_SETTING_MACRO, T_SETTING_RUN, T_SETTING_ENVIRONMENT, T_NN_STRUCTURE, T_SR_STRUCTURE)
L_NN_ES_POPULATION_R3<-F_C_06_RUN(3, "NN", "ADEMO", T_SETTING_MACRO, T_SETTING_RUN, T_SETTING_ENVIRONMENT, T_NN_STRUCTURE, T_SR_STRUCTURE)
L_NN_MS_POPULATION_R1<-F_C_06_RUN(1, "NN", "UMCS",  T_SETTING_MACRO, T_SETTING_RUN, T_SETTING_ENVIRONMENT, T_NN_STRUCTURE, T_SR_STRUCTURE)
L_NN_MS_POPULATION_R2<-F_C_06_RUN(2, "NN", "UMCS",  T_SETTING_MACRO, T_SETTING_RUN, T_SETTING_ENVIRONMENT, T_NN_STRUCTURE, T_SR_STRUCTURE)
L_NN_MS_POPULATION_R3<-F_C_06_RUN(3, "NN", "UMCS",  T_SETTING_MACRO, T_SETTING_RUN, T_SETTING_ENVIRONMENT, T_NN_STRUCTURE, T_SR_STRUCTURE)

L_SR_ES_POPULATION_R1<-F_C_06_RUN(1, "SR", "ADEMO", T_SETTING_MACRO, T_SETTING_RUN, T_SETTING_ENVIRONMENT, T_NN_STRUCTURE, T_SR_STRUCTURE)
L_SR_ES_POPULATION_R2<-F_C_06_RUN(2, "SR", "ADEMO", T_SETTING_MACRO, T_SETTING_RUN, T_SETTING_ENVIRONMENT, T_NN_STRUCTURE, T_SR_STRUCTURE)
L_SR_ES_POPULATION_R3<-F_C_06_RUN(3, "SR", "ADEMO", T_SETTING_MACRO, T_SETTING_RUN, T_SETTING_ENVIRONMENT, T_NN_STRUCTURE, T_SR_STRUCTURE)
L_SR_MS_POPULATION_R1<-F_C_06_RUN(1, "SR", "UMCS",  T_SETTING_MACRO, T_SETTING_RUN, T_SETTING_ENVIRONMENT, T_NN_STRUCTURE, T_SR_STRUCTURE)
L_SR_MS_POPULATION_R2<-F_C_06_RUN(2, "SR", "UMCS",  T_SETTING_MACRO, T_SETTING_RUN, T_SETTING_ENVIRONMENT, T_NN_STRUCTURE, T_SR_STRUCTURE)
L_SR_MS_POPULATION_R3<-F_C_06_RUN(3, "SR", "UMCS",  T_SETTING_MACRO, T_SETTING_RUN, T_SETTING_ENVIRONMENT, T_NN_STRUCTURE, T_SR_STRUCTURE)
