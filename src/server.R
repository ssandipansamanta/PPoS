library(shiny)
source("all_results.R")

shinyServer(
  function(input, output, session) {
 
    ppos_output_cont <- reactive(
      if(input$continuous_post_interim_trend_flag=='no'){
        if(input$continuous_prior_distribution_flag =='no'){
          ppos_output_cont_no_post_interim(input)
        }else{
          ppos_output_cont_no_post_interim_with_prior(input)
        }
      }else{
        if(input$continuous_prior_distribution_flag =='no'){
          ppos_output_cont_with_post_interim(input)
        }else{
          ppos_output_cont_with_post_interim_with_prior(input)
        }
      }  
    )
    
    
    output$min_cont_total_sample_size <- renderText({eventReactive(input$continuous_interim_sample_size)})
    output$default_cont_post_interim_mean_diff <- renderText({eventReactive(input$continuous_mean_diff)})
    output$cont_conditional_power_interim_trend <- renderText({ifelse(input$continuous_post_interim_trend_flag=='no',
                                                                      paste0(ppos_output_cont()$result_interim_trend,"."),
                                                                      paste0(ppos_output_cont()$result_interim_trend,ppos_output_cont()$result_sp_trend))})
    output$cont_conditional_power_ppos_interim <- renderText({ifelse(input$continuous_prior_distribution_flag =='no',
                                                                     paste0(ppos_output_cont()$result_ppos_interim_data,"."),
                                                                     paste0(ppos_output_cont()$result_ppos_interim_data,ppos_output_cont()$result_ppos_interim_with_prior))})

    
    ppos_output_bin <- reactive(
      if(input$binary_post_interim_trend_flag=='no'){
        if(input$binary_prior_distribution_flag =='no'){
          ppos_output_bin_no_post_interim(input)
        }else{
          ppos_output_bin_no_post_interim_with_prior(input)
        }
      }else{
        if(input$binary_prior_distribution_flag =='no'){
          ppos_output_bin_with_post_interim(input)
        }else{
          ppos_output_bin_with_post_interim_with_prior(input)
        }
      }  
    )
    
    output$min_bin_total_sample_size <- renderText({eventReactive(input$binary_interim_sample_size)})
    output$default_bin_post_interim_mean_diff <- renderText({eventReactive(input$binary_post_interim_trend_mean_diff)})
    output$bin_conditional_power_interim_trend <- renderText({ifelse(input$binary_post_interim_trend_flag=='no',
                                                                     paste0(ppos_output_bin()$result_interim_trend,"."),
                                                                     paste0(ppos_output_bin()$result_interim_trend,ppos_output_bin()$result_sp_trend))})
    output$bin_conditional_power_ppos_interim <- renderText({ifelse(input$binary_prior_distribution_flag =='no', 
                                                                    paste0(ppos_output_bin()$result_ppos_interim_data,"."),
                                                                    paste0(ppos_output_bin()$result_ppos_interim_data,ppos_output_bin()$result_ppos_interim_with_prior))})
    
    
    ppos_output_surv <- reactive(
      if(input$survival_post_interim_trend_flag=='no'){
        if(input$survival_prior_distribution_flag =='no'){
          ppos_output_surv_no_post_interim(input)
        }else{
          ppos_output_surv_no_post_interim_with_prior(input)
        }
      }else{
        if(input$survival_prior_distribution_flag =='no'){
          ppos_output_surv_with_post_interim(input)
        }else{
          ppos_output_surv_with_post_interim_with_prior(input)
        }
      }
    )

    output$min_surv_total_sample_size <- renderText({eventReactive(input$survival_interim_sample_size)})
    output$default_surv_post_interim_hr_diff <- renderText({eventReactive(input$survival_post_interim_trend_hr_diff)})
    output$surv_conditional_power_interim_trend <- renderText({ifelse(input$survival_post_interim_trend_flag=='no', 
                                                                      paste0(ppos_output_surv()$result_interim_trend,"."),
                                                                      paste0(ppos_output_surv()$result_interim_trend,ppos_output_surv()$result_sp_trend))})
    output$surv_conditional_power_ppos_interim <- renderText({ifelse(input$survival_prior_distribution_flag =='no',
                                                                     paste0(ppos_output_surv()$result_ppos_interim_data,"."),
                                                                     paste0(ppos_output_surv()$result_ppos_interim_data,ppos_output_surv()$result_ppos_interim_with_prior))})
    
    
  
    # output$plot <- renderPlot({
    #   # plot(cars, type=input$plotType)
    #   cat(input$number_of_samples)
    # })
    # output$text <- renderText({paste("you have selected:",input$number_of_samples)})
    # output$continuous_success_criteria_selection <- renderText({
    #   if(input$continuous_success_criteria == "cont_trial_success"){
    #     "Critical value at final analysis :"
    #   } else{
    #     "Clinically meaningful value  :"
    #     }
    #   
    #   })
    # output$continuous_verb <- renderText({paste("you have selected:",input$continuous_number_of_samples)})
    # output$binary_verb <- renderText({paste("you have selected:",input$binary_number_of_samples)})
    # output$survival_verb <- renderText({paste("you have selected:",input$survival_number_of_samples)})
    
  
  
  }
)