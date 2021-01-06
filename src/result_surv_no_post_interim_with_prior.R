source("ppos_function.R")
ppos_output_surv_no_post_interim_with_prior <- function(input){
  if(input$survival_success_criteria == 'sur_trial_success'){
    if(input$survival_success_criteria_alpha == 'sur_z_scale'){
      succ_ia(type="surv", 
              nsamples=2, 
              null.value=input$survival_parameter, 
              alternate=ifelse(input$survival_hypothesis=='surv_smaller',"smaller","greater"), 
              D=input$survival_total_sample_size, 
              d=input$survival_interim_sample_size, 
              a=input$survival_allocation_ratio,   
              hr.ia=input$survival_hazard_ratio,        
              succ.crit="trial", 
              Z.crit.final=input$survival_success_criteria_value,
              # hr.exp=input$survival_post_interim_trend_hr_diff,
              hr.prior=input$survival_prior_mean, D.prior=input$survival_prior_sd
      )
    }else{
      succ_ia(type="surv", 
              nsamples=2, 
              null.value=input$survival_parameter, 
              alternate=ifelse(input$survival_hypothesis=='surv_smaller',"smaller","greater"), 
              D=input$survival_total_sample_size, 
              d=input$survival_interim_sample_size, 
              a=input$survival_allocation_ratio,   
              hr.ia=input$survival_hazard_ratio,        
              succ.crit="trial", 
              alpha.final=input$survival_success_criteria_value,
              # hr.exp=input$survival_post_interim_trend_hr_diff,
              hr.prior=input$survival_prior_mean, D.prior=input$survival_prior_sd
      )
    }
    
  }else{
    succ_ia(type="surv", 
            nsamples=2, 
            null.value=input$survival_parameter, 
            alternate=ifelse(input$survival_hypothesis=='surv_smaller',"smaller","greater"),
            D=input$survival_total_sample_size, 
            d=input$survival_interim_sample_size, 
            a=input$survival_allocation_ratio,   
            hr.ia=input$survival_hazard_ratio,        
            succ.crit="clinical", 
            clin.succ.threshold=input$survival_success_criteria_value_clinical,            
            # hr.exp=input$survival_post_interim_trend_hr_diff,
            hr.prior=input$survival_prior_mean, D.prior=input$survival_prior_sd
    )
  }
}
