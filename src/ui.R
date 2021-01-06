library(markdown)
library(shiny)
# ui <- 
shinyUI(
  navbarPage("PPoS",
           tabPanel("Continuous",
                    fluidRow(
                      column(width = 2,
                             wellPanel(
                               h4(id="cont_interim_result", "Interim Results"),
                               tags$style(HTML("#cont_interim_result{color: blue;font-size: 20px;font-style: bold;}")),
                               
                               numericInput("continuous_interim_sample_size", "Sample Size:", value = 45, min = 1, max = Inf, width = "350px"),
                               conditionalPanel(
                                 condition = "input.continuous_number_of_samples == 'cont_two_samples'", 
                                 numericInput("continuous_mean_diff", "Mean Difference:",value = 0, min = -Inf, max = Inf, width = "350px"),
                                 numericInput("continuous_allocation_ratio", "Allocation Ratio:",value = 1, min = 1, max = Inf, width = "350px")
                               ),
                               conditionalPanel(
                                 condition = "input.continuous_number_of_samples == 'cont_one_sample'", 
                                 numericInput("continuous_mean_interim", "Mean:",value = 0, min = -Inf, max = Inf, width = "350px")
                               ),
                               
                               radioButtons("continuous_measure_dispersion_flag", "Measure Dispersion",
                                            c("Standard Error:"="continuous_se", "Standard Deviation:"="continuous_sd")
                               ),
                               conditionalPanel(
                                 condition = "input.continuous_measure_dispersion_flag == 'continuous_se'", 
                                 numericInput("continuous_measure_dispersion", "Standard Error:",value = 1, min = 0.00001, max = Inf, width = "350px")
                               ),
                               conditionalPanel(
                                 condition = "input.continuous_measure_dispersion_flag == 'continuous_sd'", 
                                 numericInput("continuous_measure_dispersion", "Standard Deviation:",value = 1, min = 0.00001, max = Inf, width = "350px")
                               ),
                             ),
                             wellPanel(
                               h4(id="cont_post_interim_trend", "Post-Interim Trend"),
                               tags$style(HTML("#cont_post_interim_trend{color: blue;font-size: 20px;font-style: bold;}")),
                               radioButtons("continuous_post_interim_trend_flag", "Different from Interim?", 
                                            c("No"="no", "Yes"="yes")
                               ),
                               conditionalPanel(
                                 condition = "input.continuous_post_interim_trend_flag == 'yes'", 
                                 numericInput("continuous_post_interim_trend_mean_diff", "Mean Difference:",value = 0, min = -Inf, max = Inf, width = "350px")#textOutput("default_post_interim_mean_diff")
                               ),
                             ),
                             wellPanel(
                               h4(id="cont_prior_distribution", "Prior Distribution (optional)"),
                               tags$style(HTML("#cont_prior_distribution{color: blue;font-size: 20px;font-style: bold;}")),
                               radioButtons("continuous_prior_distribution_flag", "Have prior information?", 
                                            c("No"="no", "Yes"="yes")
                               ),
                               conditionalPanel(
                                 condition = "input.continuous_prior_distribution_flag == 'yes'", 
                                 fluidRow(
                                   column(5,
                                          numericInput("continuous_prior_mean", "Mean", value = 0, min = -Inf, max = Inf, width = "100px")
                                   ),
                                   column(5, ofset = 9,
                                          numericInput("continuous_prior_sd", "Std Dev", value = 1, min = 0, max = Inf, width = "100px")
                                   )
                                 )
                               )
                             ),
                             wellPanel(
                               h4(id="cont_success_criteria", "Success Type"),
                               tags$style(HTML("#cont_success_criteria{color: blue;font-size: 20px;font-style: bold;}")),
                               selectInput("continuous_success_criteria", "Success Criteria",
                                           c(Clinical="cont_clinical_success", Trial="cont_trial_success")
                               ),
                               conditionalPanel(
                                 condition = "input.continuous_success_criteria == 'cont_trial_success'", 
                                 radioButtons("continuous_success_criteria_alpha", "Scale Value",
                                              c("Z Scale"="cont_z_scale", "1-alpha Scale"="cont_alpha_Scale")
                                 ),
                                 numericInput("continuous_success_criteria_value", 
                                              "Critical Value at Final Analysis:",
                                              value = 1.96, min = -Inf, max = Inf, width = "350px")
                               ),
                               conditionalPanel(
                                 condition = "input.continuous_success_criteria == 'cont_clinical_success'", 
                                 numericInput("continuous_success_criteria_value_clinical", 
                                              "Clinically Meaningful Value:",
                                              value = 0, min = -Inf, max = Inf, width = "350px")
                               )
                             )
                      ),
                      
                      column(10,
                             fluidRow(
                               column(3, 
                                      wellPanel (
                                        selectInput("continuous_number_of_samples", "Number of Samples",
                                                    c(`One-Sample`="cont_one_sample", `Two-Sample`="cont_two_samples"))
                                      )
                               ),
                               column(3,
                                      wellPanel (
                                        selectInput("continuous_hypothesis", "Alternative Hypothesis",
                                                    c(Greater="cont_greater", Smaller="cont_smaller"))
                                      )
                               ),
                               column(3,
                                      wellPanel (
                                        numericInput("continuous_total_sample_size", "Total Sample Size:", value = 225, min = 1, max = Inf,width = "350px")#as.numeric(as.character(div(textOutput("min_total_sample_size"))))
                                      )
                                      
                               ), 
                               column(3,
                                      wellPanel (
                                        numericInput("continuous_parameter", "Parameter:", value = 0, min = -Inf, max = Inf,width = "350px")
                                      )
                                      
                               )
                             ),
                             fluidRow(
                               column(12,
                                      wellPanel(
                                        h4(id="cont_Summary", "Summary"),
                                        tags$style(HTML("#cont_Summary{color: blue;font-size: 20px;font-style: bold;text-align:center}")),
                                        h5(id="cont_text", "Welcome to PPos: Free to use for One-Sample/Two Samples Continuous Endpoint")
                                      )
                              )
                             ),
                             fluidRow(
                               column(6,
                                      wellPanel(
                                            h4(id="cont_conditional_power", "Conditional Power (CP)"),
                                            tags$style(HTML("#cont_conditional_power{color: blue;font-size: 20px;font-style: bold;}")),
                                            div(textOutput("cont_conditional_power_interim_trend"),style = "font-size:100%")
                                            )
                                        ),
                                      
                              column(6,
                                     wellPanel(
                                       h4(id="cont_predictive_power_prior", "Predictive Power of Success (PPoS)"),
                                       tags$style(HTML("#cont_predictive_power_prior{color: blue;font-size: 20px;font-style: bold;}")),
                                       div(textOutput("cont_conditional_power_ppos_interim"),style = "font-size:100%")
                                     )
                                    )     
                              )
                      )
                    )
           ),
           tabPanel("Binary",
                    fluidRow(
                      column(width = 2,
                             wellPanel(
                               h4(id="bin_interim_result", "Interim Results"),
                               tags$style(HTML("#bin_interim_result{color: blue;font-size: 20px;font-style: bold;}")),
                               conditionalPanel(
                                 condition = "input.binary_number_of_samples == 'bin_two_samples'", 
                                 
                                 h5(id = "bin_trt_grp","Treatment Group:"),
                                 tags$style(HTML("#bin_trt_grp{color: brown;font-style: bold;}")),
                                 
                                 fluidRow(
                                   column(5,
                                          numericInput("binary_interim_sample_size_1", "Sample Size", value = 0, min = 1, max = Inf, width = "100px")
                                   ),
                                   column(5, ofset = 9,
                                          numericInput("binary_prop_sample_1", "Proportion", value = 0, min = 0, max = 1, width = "100px")
                                   )
                                 ), 
                                 
                                 h5(id = "bin_cnt_grp","Control Group:"),
                                 tags$style(HTML("#bin_cnt_grp{color: brown;font-style: bold;}")),
                                 
                                 fluidRow(
                                   column(5,
                                          numericInput("binary_interim_sample_size_2", "Sample Size", value = 0, min = 1, max = Inf, width = "100px")
                                   ),
                                   column(5, ofset = 9,
                                          numericInput("binary_prop_sample_2", "Proportion", value = 0, min = 0, max = 1, width = "100px")
                                   )
                                 ), 
                                
                               ),
                               conditionalPanel(
                                 condition = "input.binary_number_of_samples == 'bin_one_sample'", 
                                 numericInput("binary_interim_sample_size", "Sample Size:", value = 105, min = 1, max = Inf, width = "350px"),
                                 numericInput("binary_prop_one_sample", "Proportion:",value = 0.379, min = 0, max = 1, width = "350px")
                               ),
                               
                             ),
                             wellPanel(
                               h4(id="bin_post_interim_trend", "Post-Interim Trend"),
                               tags$style(HTML("#bin_post_interim_trend{color: blue;font-size: 20px;font-style: bold;}")),
                               radioButtons("binary_post_interim_trend_flag", "Different from Interim?", 
                                            c("No"="no", "Yes"="yes")
                               ),
                               conditionalPanel(
                                 condition = "input.binary_post_interim_trend_flag == 'yes'", 
                                 numericInput("binary_post_interim_trend_mean_diff", "Prop Difference:",value = 0, min = -Inf, max = Inf, width = "350px")
                               ),
                             ),
                             wellPanel(
                               h4(id="bin_prior_distribution", "Prior Distribution (optional)"),
                               tags$style(HTML("#bin_prior_distribution{color: blue;font-size: 20px;font-style: bold;}")),
                               radioButtons("binary_prior_distribution_flag", "Have prior information?", 
                                            c("No"="no", "Yes"="yes")
                               ),
                               conditionalPanel(
                                 condition = "input.binary_prior_distribution_flag == 'yes'", 
                                 radioButtons("binary_prior_distribution_type_flag", "Distribution", 
                                              c("Normal"="normal", "Beta(a,b)"="beta")
                                 ),
                                 conditionalPanel(
                                   condition = "input.binary_prior_distribution_type_flag == 'normal'",
                                   fluidRow(
                                     column(5,
                                            numericInput("binary_prior_mean", "Mean", value = 0, min = -Inf, max = Inf, width = "100px")
                                     ),
                                     column(5, ofset = 9,
                                            numericInput("binary_prior_sd", "Std Dev", value = 1, min = 0, max = Inf, width = "100px")
                                     )
                                   )
                                 ),
                                 conditionalPanel(
                                   condition = "input.binary_prior_distribution_type_flag == 'beta'",
                                   fluidRow(
                                     column(5,
                                            numericInput("binary_prior_a", "a", value = 0, min = 0, max = Inf, width = "100px")
                                     ),
                                     column(5, ofset = 9,
                                            numericInput("binary_prior_b", "b", value = 0, min = 0, max = Inf, width = "100px")
                                     )
                                   )
                                 )
                               )
                             ),
                             wellPanel(
                               h4(id="bin_success_criteria", "Success Type"),
                               tags$style(HTML("#bin_success_criteria{color: blue;font-size: 20px;font-style: bold;}")),
                               selectInput("binary_success_criteria", "Success Criteria",
                                           c(Clinical="bin_clinical_success", Trial="bin_trial_success")
                               ),
                               conditionalPanel(
                                 condition = "input.binary_success_criteria == 'bin_trial_success'", 
                                 radioButtons("binary_success_criteria_alpha", "Scale Value",
                                              c("Z Scale"="bin_z_scale", "1-alpha Scale"="bin_alpha_Scale")
                                 ),
                                 numericInput("binary_success_criteria_value", 
                                              "Critical Value at Final Analysis:",
                                              value = 1.96, min = -Inf, max = Inf, width = "350px")
                               ),
                               conditionalPanel(
                                 condition = "input.binary_success_criteria == 'bin_clinical_success'", 
                                 numericInput("binary_success_criteria_value_clinical", 
                                              "Clinically Meaningful Value:",
                                              value = 0, min = -Inf, max = Inf, width = "350px")
                               )
                             )
                      ),
                      
                      column(10,
                             fluidRow(
                               column(3, 
                                      wellPanel (
                                        selectInput("binary_number_of_samples", "Number of Samples",
                                                    c(`One-Sample`="bin_one_sample", `Two-Sample`="bin_two_samples"))
                                      )
                               ),
                               column(3,
                                      wellPanel (
                                        selectInput("binary_hypothesis", "Alternative Hypothesis",
                                                    c(Greater="bin_greater", Smaller="bin_smaller"))
                                      )
                               ),
                               column(3,
                                      wellPanel (
                                        numericInput("binary_total_sample_size", "Total Sample Size:", value = 210, min = 1, max = Inf,width = "350px")
                                      )
                                      
                               ), 
                               column(3,
                                      wellPanel (
                                        numericInput("binary_parameter", "Parameter:", value = 0.3, min = 0, max = 1,width = "350px")
                                      )
                                      
                               )
                             ),
                             fluidRow(
                               column(12,
                                      wellPanel(
                                        h4(id="binary_Summary", "Summary"),
                                        tags$style(HTML("#binary_Summary{color: blue;font-size: 20px;font-style: bold;text-align:center}")),
                                        h5(id="binary_text", "Welcome to PPos: Free to use for One-Sample/Two Samples Binary Endpoint")
                                      )
                               )
                             ),
                             fluidRow(
                               column(6,
                                      wellPanel(
                                        h4(id="bin_conditional_power", "Conditional Power (CP)"),
                                        tags$style(HTML("#bin_conditional_power{color: blue;font-size: 20px;font-style: bold;}")),
                                        div(textOutput("bin_conditional_power_interim_trend"),style = "font-size:100%")
                                      )
                               ),
                               
                               column(6,
                                      wellPanel(
                                        h4(id="bin_predictive_power_prior", "Predictive Power of Success (PPoS)"),
                                        tags$style(HTML("#bin_predictive_power_prior{color: blue;font-size: 20px;font-style: bold;}")),
                                        div(textOutput("bin_conditional_power_ppos_interim"),style = "font-size:100%")
                                      )
                               )     
                             )
                      )
                    )
           ),
           tabPanel("Survival",
                    fluidRow(
                      column(width = 2,
                             wellPanel(
                               h4(id="sur_interim_result", "Interim Results"),
                               tags$style(HTML("#sur_interim_result{color: blue;font-size: 20px;font-style: bold;}")),
                               numericInput("survival_interim_sample_size", "Sample Size:", value = 332, min = 1, max = Inf, width = "350px"),
                               numericInput("survival_hazard_ratio", "Hazard Ratio(HR):",value = 0.82, min = 0, max = Inf, width = "350px"),
                               numericInput("survival_allocation_ratio", "Allocation Ratio:",value = 1, min = 1, max = Inf, width = "350px")
                             ),
                             wellPanel(
                               h4(id="sur_post_interim_trend", "Post-Interim Trend"),
                               tags$style(HTML("#sur_post_interim_trend{color: blue;font-size: 20px;font-style: bold;}")),
                               radioButtons("survival_post_interim_trend_flag", "Different from Interim?", 
                                            c("No"="no", "Yes"="yes")
                               ),
                               conditionalPanel(
                                 condition = "input.survival_post_interim_trend_flag == 'yes'", 
                                 numericInput("survival_post_interim_trend_hr_diff", "HR Difference:",value = 0, min = -Inf, max = Inf, width = "350px")
                               ),
                             ),
                             wellPanel(
                               h4(id="sur_prior_distribution", "Prior Distribution (optional)"),
                               tags$style(HTML("#sur_prior_distribution{color: blue;font-size: 20px;font-style: bold;}")),
                               radioButtons("survival_prior_distribution_flag", "Have prior information?", 
                                            c("No"="no", "Yes"="yes")
                               ),
                               conditionalPanel(
                                 condition = "input.survival_prior_distribution_flag == 'yes'", 
                                 # radioButtons("survival_prior_distribution_type_flag", "Distribution", 
                                 #              c("Normal"="normal", "Beta"="beta")
                                 fluidRow(
                                   column(5,
                                          numericInput("survival_prior_mean", "HR(Prior)", value = 0, min = -Inf, max = Inf, width = "100px")
                                   ),
                                   column(5, ofset = 9,
                                          numericInput("survival_prior_sd", "# Events", value = 1, min = 0.00001, max = Inf, width = "100px")
                                   )
                                 )
                               )
                             ),
                             wellPanel(
                               h4(id="sur_success_criteria", "Success Type"),
                               tags$style(HTML("#sur_success_criteria{color: blue;font-size: 20px;font-style: bold;}")),
                               selectInput("survival_success_criteria", "Success Criteria",
                                           c(Clinical="sur_clinical_success", Trial="sur_trial_success")
                               ),
                               conditionalPanel(
                                 condition = "input.survival_success_criteria == 'sur_trial_success'", 
                                 radioButtons("survival_success_criteria_alpha", "Scale Value",
                                              c("Z Scale"="sur_z_scale", "1-alpha Scale"="sur_alpha_Scale")
                                 ),
                                 numericInput("survival_success_criteria_value", 
                                              "Critical Value at Final Analysis:",
                                              value = 1.96, min = -Inf, max = Inf, width = "350px")
                               ),
                               conditionalPanel(
                                 condition = "input.survival_success_criteria == 'sur_clinical_success'", 
                                 numericInput("survival_success_criteria_value_clinical", 
                                              "Clinically Meaningful Value:",
                                              value = 0.80, min = -Inf, max = Inf, width = "350px")
                               )
                             )
                      ),
                      
                      column(10,
                             fluidRow(
                               column(3, 
                                      wellPanel (
                                        selectInput("survival_number_of_samples", "Number of Samples",
                                                    c(`Two-Sample`="sur_two_samples"))
                                      )
                               ),
                               column(3,
                                      wellPanel (
                                        selectInput("survival_hypothesis", "Alternative Hypothesis",
                                                    c(Greater="surv_greater", Smaller="surv_smaller"))
                                      )
                               ),
                               column(3,
                                      wellPanel (
                                        numericInput("survival_total_sample_size", "Total Sample Size:", value = 441, min = 1, max = Inf,width = "350px")
                                      )
                                      
                               ), 
                               column(3,
                                      wellPanel (
                                        numericInput("survival_parameter", "Parameter:", value = 1, min = 1, max = Inf,width = "350px")
                                      )
                                      
                               )
                             ),
                             fluidRow(
                               column(12,
                                      wellPanel(
                                        h4(id="surv_Summary", "Summary"),
                                        tags$style(HTML("#surv_Summary{color: blue;font-size: 20px;font-style: bold;text-align:center}")),
                                        h5(id="surv_text", "Welcome to PPos: Free to use for Two Samples Survival Endpoint")
                                      )
                               )
                             ),
                             fluidRow(
                               column(6,
                                      wellPanel(
                                        h4(id="sur_conditional_power", "Conditional Power (CP)"),
                                        tags$style(HTML("#sur_conditional_power{color: blue;font-size: 20px;font-style: bold;}")),
                                        div(textOutput("surv_conditional_power_interim_trend"),style = "font-size:100%")
                                      )
                               ),
                               
                               column(6,
                                      wellPanel(
                                        h4(id="sur_predictive_power_prior", "Predictive Power of Success (PPoS)"),
                                        tags$style(HTML("#sur_predictive_power_prior{color: blue;font-size: 20px;font-style: bold;}")),
                                        div(textOutput("surv_conditional_power_ppos_interim"),style = "font-size:100%")
                                      )
                               )     
                             )
                      )
                    )
           ),
           tabPanel("Synopsis",
                    fluidRow(
                      column(1,
                             tags$small(
                               " "
                             )
                      ),
                      column(10,
                             wellPanel(
                               h4(id="synopsis", "Overview"),
                               tags$style(HTML("#synopsis{color: blue;font-size: 20px;font-style: bold;text-align:center}")),
                               includeHTML("../doc/synopsis.html")
                               )
                      )
                    )
           ),
           tabPanel("Mathematical Derivation",
                    tags$iframe(style="height:800px; width:100%; scrolling=yes", src="https://arxiv.org/pdf/2006.15282.pdf")
           ),
           tabPanel("Contact Us",
                    fluidRow(
                      column(1,
                             tags$small(
                               " "
                             )
                      ),
                      column(10,
                             wellPanel(
                               includeHTML("../doc/about_us.html")
                               )
                             
                      )
                  )

           ),
           tabPanel("Licence",
                    fluidRow(
                      column(1,
                             tags$small(
                               " "
                             )
                      ),
                      column(10,
                              wellPanel(
                              h4(id="licence", "LICENCE"),
                              tags$style(HTML("#licence{color: blue;font-size: 20px;font-style: bold;text-align:center}")),
                              includeHTML("../doc/licence.html")
                            )
                      )
                    )

           )

  )
)