source("ppos_function.R")
ppos_output_bin_with_post_interim <- function(input){
  if(input$binary_success_criteria == 'bin_trial_success'){
    if(input$binary_success_criteria_alpha == 'bin_z_scale'){
      if(input$binary_number_of_samples == 'bin_one_sample'){
        succ_ia(type="bin", 
                nsamples=1, 
                null.value=input$binary_parameter, 
                alternate=ifelse(input$binary_hypothesis=='bin_smaller',"smaller","greater"),
                N=input$binary_total_sample_size, 
                n=input$binary_interim_sample_size,  
                prop.ia=input$binary_prop_one_sample,
                prop.exp=input$binary_post_interim_trend_mean_diff,
                stderr.ia=sqrt(input$binary_prop_one_sample*(1-input$binary_prop_one_sample)/input$binary_interim_sample_size), 
                succ.crit="trial", 
                Z.crit.final=input$binary_success_criteria_value)        
      }else{
        succ_ia(type="bin", 
                nsamples=2, 
                null.value=input$binary_parameter, 
                alternate=ifelse(input$binary_hypothesis=='bin_smaller',"smaller","greater"),
                N=input$binary_total_sample_size, 
                n=input$binary_interim_sample_size_1 + input$binary_interim_sample_size_2,
                a=ifelse(input$binary_interim_sample_size_1 > input$binary_interim_sample_size_2,round(input$binary_interim_sample_size_1/input$binary_interim_sample_size_2),round(input$binary_interim_sample_size_2/input$binary_interim_sample_size_1)),
                propdiff.ia=input$binary_prop_sample_1-input$binary_prop_sample_2,
                propdiff.exp=input$binary_post_interim_trend_mean_diff,
                stderr.ia=sqrt(input$binary_prop_sample_1*(1-input$binary_prop_sample_1)/input$binary_interim_sample_size_1 + input$binary_prop_sample_2*(1-input$binary_prop_sample_2)/input$binary_interim_sample_size_2), 
                succ.crit="trial", 
                Z.crit.final=input$binary_success_criteria_value)  
      }
    }else{
      if(input$binary_number_of_samples == 'bin_one_sample'){
        succ_ia(type="bin", 
                nsamples=1, 
                null.value=input$binary_parameter, 
                alternate=ifelse(input$binary_hypothesis=='bin_smaller',"smaller","greater"),
                N=input$binary_total_sample_size, 
                n=input$binary_interim_sample_size,  
                prop.ia=input$binary_prop_one_sample,
                prop.exp=input$binary_post_interim_trend_mean_diff,
                stderr.ia=sqrt(input$binary_prop_one_sample*(1-input$binary_prop_one_sample)/input$binary_interim_sample_size), 
                succ.crit="trial", 
                alpha.final=input$binary_success_criteria_value)        
      }else{
        succ_ia(type="bin", 
                nsamples=2, 
                null.value=input$binary_parameter, 
                alternate=ifelse(input$binary_hypothesis=='bin_smaller',"smaller","greater"),
                N=input$binary_total_sample_size, 
                n=input$binary_interim_sample_size_1 + input$binary_interim_sample_size_2,
                a=ifelse(input$binary_interim_sample_size_1 > input$binary_interim_sample_size_2,round(input$binary_interim_sample_size_1/input$binary_interim_sample_size_2),round(input$binary_interim_sample_size_2/input$binary_interim_sample_size_1)),
                propdiff.ia=input$binary_prop_sample_1-input$binary_prop_sample_2,
                propdiff.exp=input$binary_post_interim_trend_mean_diff,
                stderr.ia=sqrt(input$binary_prop_sample_1*(1-input$binary_prop_sample_1)/input$binary_interim_sample_size_1 + input$binary_prop_sample_2*(1-input$binary_prop_sample_2)/input$binary_interim_sample_size_2), 
                succ.crit="trial", 
                alpha.final=input$binary_success_criteria_value)  
      }
    }
  }else{
    if(input$binary_number_of_samples == 'bin_one_sample'){
      succ_ia(type="bin", 
              nsamples=1, 
              null.value=input$binary_parameter, 
              alternate=ifelse(input$binary_hypothesis=='bin_smaller',"smaller","greater"),
              N=input$binary_total_sample_size, 
              n=input$binary_interim_sample_size,  
              prop.ia=input$binary_prop_one_sample,
              prop.exp=input$binary_post_interim_trend_mean_diff,
              stderr.ia=sqrt(input$binary_prop_one_sample*(1-input$binary_prop_one_sample)/input$binary_interim_sample_size), 
              succ.crit="clinical", 
              clin.succ.threshold=input$binary_success_criteria_value_clinical)        
    }else{
      succ_ia(type="bin", 
              nsamples=2, 
              null.value=input$binary_parameter, 
              alternate=ifelse(input$binary_hypothesis=='bin_smaller',"smaller","greater"),
              N=input$binary_total_sample_size, 
              n=input$binary_interim_sample_size_1 + input$binary_interim_sample_size_2,
              a=ifelse(input$binary_interim_sample_size_1 > input$binary_interim_sample_size_2,round(input$binary_interim_sample_size_1/input$binary_interim_sample_size_2),round(input$binary_interim_sample_size_2/input$binary_interim_sample_size_1)),
              propdiff.ia=input$binary_prop_sample_1-input$binary_prop_sample_2,
              propdiff.exp=input$binary_post_interim_trend_mean_diff,
              stderr.ia=sqrt(input$binary_prop_sample_1*(1-input$binary_prop_sample_1)/input$binary_interim_sample_size_1 + input$binary_prop_sample_2*(1-input$binary_prop_sample_2)/input$binary_interim_sample_size_2), 
              succ.crit="clinical", 
              clin.succ.threshold=input$binary_success_criteria_value_clinical)  
    } 
  }
}