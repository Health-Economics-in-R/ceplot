context("test-s_analysis_to_tornado_plot_data.R")

library(ggplot2)

test_that("", {

   s_analysis <- data.frame(output = c(10,1,11,5,3),
                            sens = c(2,2,3,0,2),
                            spec = c(1,4,2,2,2))

   s_analysis <- model.frame(formula = output ~ sens + spec,
                             data = s_analysis)

   s_analysis_to_tornado_plot_data(s_analysis) %>%
     ggplot_tornado(baseline_output = 6)

})

test_that("", {

   s_analysis <- data.frame(output = c(10,1,11,5),
                            sens = c(NA,NA,3,0),
                            spec = c(1,4,NA,NA))

   s_analysis <- model.frame(formula = output ~ sens + spec,
                             data = s_analysis,
                             na.action = 'na.pass')

   s_analysis_to_tornado_plot_data(s_analysis) %>%
     ggplot_tornado(baseline_output = 6)

})
