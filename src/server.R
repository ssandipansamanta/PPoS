library(shiny)
library(ggplot2)
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

    
    con_plot_data <- reactive(
      if(input$continuous_prior_distribution_flag =='no'){
        pps_plot_output_cont(input,ppos_output_cont()$mean_pred_wo_prior,ppos_output_cont()$sd_pred_wo_prior)
      }else{
        pps_plot_output_cont(input,ppos_output_cont()$mean_pred_wo_prior,ppos_output_cont()$sd_pred_wo_prior,
                    ppos_output_cont()$mean_pred_w_prior,ppos_output_cont()$sd_pred_w_prior)
      }
    )
    
    output$cont_distribution <- renderPlot({
      ggplot(data = con_plot_data(), aes(x = value, colour=type)) + 
        geom_line(aes(y = Density), size = 1.5) +
        xlab('value') +  ylab('Density')  + theme_classic() + theme(legend.position="bottom", legend.title = element_blank()) 
        
      })
    
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
    
    
    bin_plot_data <- reactive(
      if(input$binary_prior_distribution_flag =='no'){
        pps_plot_output_bin(input,ppos_output_bin()$mean_pred_wo_prior,ppos_output_bin()$sd_pred_wo_prior)
      }else{
        pps_plot_output_bin(input,ppos_output_bin()$mean_pred_wo_prior,ppos_output_bin()$sd_pred_wo_prior,
                        ppos_output_bin()$mean_pred_w_prior,ppos_output_bin()$sd_pred_w_prior)
      }
    )
    
    output$bin_distribution <- renderPlot({
      ggplot(data = bin_plot_data(), aes(x = value, colour=type)) + 
        geom_line(aes(y = Density), size = 1.5) +
        xlab('value') +  ylab('Density')  + theme_classic() + theme(legend.position="bottom", legend.title = element_blank()) 
      
    })
    
    
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
    
    surv_plot_data <- reactive(
      if(input$survival_prior_distribution_flag =='no'){
        pps_plot_output_surv(input,ppos_output_surv()$mean_pred_wo_prior,ppos_output_surv()$sd_pred_wo_prior)
      }else{
        pps_plot_output_surv(input,ppos_output_surv()$mean_pred_wo_prior,ppos_output_surv()$sd_pred_wo_prior,
                        ppos_output_surv()$mean_pred_w_prior,ppos_output_surv()$sd_pred_w_prior)
      }
    )
    
    output$surv_distribution <- renderPlot({
      ggplot(data = surv_plot_data(), aes(x = value, colour=type)) + 
        geom_line(aes(y = Density), size = 1.5) +
        xlab('value') +  ylab('Density')  + theme_classic() + theme(legend.position="bottom", legend.title = element_blank()) 
      
    })
    
  
  
  }
)