## load up the packages we will need: 
library(tidyr)
library(readxl)
library(dplyr)
library(table1)
library(tidyverse)
library(gt)
library(patchwork)

## 1. Stat Analysis function -----
stat_analysis <- function(data = data_clean, output, model){
  
  g.res <- model_list[[output]][[model]] %>%
    summary() %>%
    coef() %>%
    as.data.frame()
  
  g.res.clean <- data.frame()
  g.res.clean_biv <- data.frame()
  
  var_list <- c("Age", "Gender", "Activity", "Food_Intake", model)
  var_list2 <- c("Age", "Gender", "Activity", "Food Intake", model)
  
  for(i in 1:length(var_list)){
    name_i <- (var_list)[i]
    name_display_i <- var_list2[i]   
    
    out_i <- g.res[grepl(name_i, rownames(g.res)),]
    
    or <- exp(out_i[,1])
    lci <- exp(out_i[,1] - 1.96*out_i[,2])
    uci <- exp(out_i[,1] + 1.96*out_i[,2])
    p.value <- out_i[,4]
    
    if(class(data[[name_i]]) == class(1)){
      g.res.clean0 <- data.frame(type = c("Subhead", "Value"),
                                 model = c(name_display_i, "Value"),
                                 log.estimate = c(NA, log(or)),
                                 log.conf.low = c(NA, log(lci)),
                                 log.conf.high = c(NA, log(lci)),
                                 estimate = c(NA, or),
                                 conf.low = c(NA, lci),
                                 conf.high = c(NA, uci),
                                 p.value = c(NA, p.value)) %>%
        mutate(id = paste0(model, name_i))
      
      g.res.clean <- bind_rows(g.res.clean, g.res.clean0)
      
    } else{
      
      g.res.clean0 <- data.frame(type = c("Subhead", "Ref", rep("Value", data[[name_i]] %>% levels() %>% length() - 1)),
                                 model = c(name_display_i, levels(data[[name_i]])),
                                 log.estimate = c(NA, NA, log(or)),
                                 log.conf.low = c(NA, NA, log(lci)),
                                 log.conf.high = c(NA, NA, log(lci)),
                                 estimate = c(NA, NA, or),
                                 conf.low = c(NA, NA, lci),
                                 conf.high = c(NA, NA, uci),
                                 p.value = c(NA, NA, p.value)) %>%
        mutate(id = paste0(model, name_i))
      
      g.res.clean <- bind_rows(g.res.clean, g.res.clean0)
      
    }
    
    ## bivariate
    g.res.biv_i <- glm(Central_Obesity ~ data[[name_i]] , data = data, family = "binomial") %>%
      summary() %>%
      coef() %>%
      as.data.frame()
    
    if(tolower(model) == "obesity"){
      g.res.biv_i <- glm(Obesity ~ data[[name_i]] , data = data, family = "binomial") %>%
        summary() %>%
        coef() %>%
        as.data.frame()
    }
    
    out_i <- g.res.biv_i[-1,]
    
    or <- exp(out_i[,1])
    lci <- exp(out_i[,1] - 1.96*out_i[,2])
    uci <- exp(out_i[,1] + 1.96*out_i[,2])
    p.value <- out_i[,4]
    
    if(class(data[[name_i]]) == class(1)){
      g.res.clean_biv0 <- data.frame(type = c("Subhead", "Value"),
                                     model = c(name_display_i, "Value"),
                                     log.estimate = c(NA, log(or)),
                                     log.conf.low = c(NA, log(lci)),
                                     log.conf.high = c(NA, log(lci)),
                                     estimate = c(NA, or),
                                     conf.low = c(NA, lci),
                                     conf.high = c(NA, uci),
                                     p.value = c(NA, p.value)) %>%
        mutate(id = paste0(model, name_i))
      
      g.res.clean_biv <- bind_rows(g.res.clean_biv, g.res.clean_biv0)
    } else{
      g.res.clean_biv0 <- data.frame(type = c("Subhead", "Ref", rep("Value", data[[name_i]] %>% levels() %>% length() - 1)),
                                     model = c(name_display_i, levels(data[[name_i]])),
                                     log.estimate = c(NA, NA, log(or)),
                                     log.conf.low = c(NA, NA, log(lci)),
                                     log.conf.high = c(NA, NA, log(lci)),
                                     estimate = c(NA, NA, or),
                                     conf.low = c(NA, NA, lci),
                                     conf.high = c(NA, NA, uci),
                                     p.value = c(NA, NA, p.value)) %>%
        mutate(id = paste0(model, name_i))
      
      g.res.clean_biv <- bind_rows(g.res.clean_biv, g.res.clean_biv0)
    }
    
  }
  
  res <- list(adjusted = g.res.clean,
              unadjusted = g.res.clean_biv)
  return(res)
}

## 2. Forest plot function -----
generate_plot <- function(data = data_clean, stats = "adjusted", model = "Codominant", output = "Central_Obesity"){
  {
    res = stat_analysis(data = data, output = "Central_Obesity", model = "Codominant")[[stats]]
    
    n_data <- nrow(res)
    
    res$id <- factor(res$id, levels = res$id)
    
    ## plotting
    ## ---------------------------
    # create forest plot on log scale (middle section of figure)
    library(scales)  # for label_number()
    
    custom_log_labels <- function(x) {
      sapply(x, function(val) {
        if (val < 1) {
          format(val, digits = 3, nsmall = 3, trim = TRUE)
        } else {
          format(val, big.mark = ",", scientific = FALSE, trim = TRUE)
        }
      })
    }
    
    risk_label = paste0("Risk Factors of ", sub("_", " ", output))
    protective_label = paste0("Protective Factors of ", sub("_", " ", output))
    
    p_mid <-
      res |>
      ggplot(aes(y = fct_rev(id))) +
      theme_classic() +
      geom_point(aes(x=estimate), shape=15, size=3) +
      geom_linerange(aes(xmin=conf.low, xmax=conf.high)) +
      labs(x= ifelse(tolower(stats) == "adjusted",
                     "Adjusted Odds Ratio",
                     "Crude Odds Ratio")) +
      coord_cartesian(ylim=c(1, n_data+1), xlim=c(0.1, 10))+
      scale_x_log10(
        limits = c(0.1, 10),
        breaks = c(0.1, 1, 10),
        labels = custom_log_labels
      )+
      geom_vline(xintercept = 1, linetype="dashed") +
      annotate("text", x = ifelse(tolower(output) == "obesity", 3,3.7), y = n_data+1, label = paste0("Risk Factors of ", sub("_", " ", output))) +
      annotate("text", x = 1/(ifelse(tolower(output) == "obesity", 3,3.7)), y = n_data+1, label = paste0("Protective Factors of ", sub("_", " ", output))) +
      theme(axis.line.y = element_blank(),
            axis.ticks.y= element_blank(),
            axis.text.y= element_blank(),
            axis.title.y= element_blank())
    # wrangle results into pre-plotting table form
    res_plot <- res |>
      mutate(across(c(estimate, conf.low, conf.high), ~formatC(.x, format = "f", digits = 2)),
             estimate_lab = paste0(ifelse(grepl("NA",estimate), ifelse(type == "Ref", "Ref", ""), paste0(estimate)),
                                   ifelse(grepl("NA",conf.low), "", paste0(" (", conf.low, "-")),
                                   ifelse(grepl("NA",conf.high), "", paste0(conf.high, ")"))),
             color = c(rep(c("gray","white"), floor((n_data - n_data %% 2)/2) ), rep("gray", n_data %% 2))) |>
      mutate(p.value = case_when(p.value < .01 ~ "<0.01", TRUE ~ str_pad(as.character(round(p.value, 2)),width=4,pad="0",side="right")))
    
    head.df <- res_plot[1,] %>%
      mutate(model = "Model",
             id = "Heading",
             estimate_lab = ifelse(tolower(stats) == "adjusted",
                                   "aOR (95%CI)",
                                   "cOR (95%CI)"),
             conf.low = "",
             conf.high = "",
             p.value = "p-value")
    
    res_plot <- rbind(head.df, res_plot)
    
    res_plot$id <- factor(res_plot$id, levels = res_plot$id) %>% fct_rev()
    
    # left side of plot - hazard ratios
    p_left <-
      res_plot  |>
      ggplot(aes(y = id)) + 
      geom_text(aes(x=ifelse(type != "Subhead", 0.2, 0), label=model), hjust=0, fontface = ifelse(res_plot$type == "Subhead", "bold", "plain")) +
      geom_text(aes(x=2, label=estimate_lab), hjust=0, fontface = ifelse(res_plot$estimate_lab == ifelse(tolower(stats) == "adjusted",
                                                                                                         "aOR (95%CI)",
                                                                                                         "cOR (95%CI)"), "bold", "plain")) +
      theme_void() +
      coord_cartesian(xlim=c(0,4))
    # right side of plot - pvalues
    p_right <-
      res_plot  |>
      ggplot() +
      geom_text(aes(x=0, y=id, label=p.value), hjust=0, fontface = ifelse(res_plot$p.value == "p-value", "bold", "plain")) +
      theme_void() 
    # layout design (top, left, bottom, right)
    
    (p_left | p_mid | p_right) + plot_layout(widths = c(0.9, 0.9, 0.25))
  }
  
}