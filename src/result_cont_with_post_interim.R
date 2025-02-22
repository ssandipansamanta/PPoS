source("ppos_function.R")
ppos_output_cont_with_post_interim <- function(input){
  if(input$continuous_measure_dispersion_flag == 'continuous_se'){
    if(input$continuous_success_criteria=='cont_trial_success'){
      if(input$continuous_success_criteria_alpha =='cont_z_scale'){
        if(input$continuous_number_of_samples=='cont_one_sample'){
          succ_ia(type="cont",
                  nsamples=1,
                  null.value=input$continuous_parameter,
                  alternate=ifelse(input$continuous_hypothesis=='cont_smaller',"smaller","greater"),
                  N=input$continuous_total_sample_size,
                  n=input$continuous_interim_sample_size,
                  mean.ia=input$continuous_mean_interim,
                  stderr.ia=input$continuous_measure_dispersion,
                  mean.exp = input$continuous_post_interim_trend_mean_diff,
                  succ.crit="trial",
                  Z.crit.final=input$continuous_success_criteria_value)
        }else{
          succ_ia(type="cont",
                  nsamples=2,
                  null.value=input$continuous_parameter,
                  alternate=ifelse(input$continuous_hypothesis=='cont_smaller',"smaller","greater"),
                  N=input$continuous_total_sample_size,
                  n=input$continuous_interim_sample_size,
                  meandiff.ia=input$continuous_mean_diff,
                  a = input$continuous_allocation_ratio,
                  stderr.ia=input$continuous_measure_dispersion,
                  meandiff.exp = input$continuous_post_interim_trend_mean_diff,
                  succ.crit="trial",
                  Z.crit.final=input$continuous_success_criteria_value)
          
        }
      }else{
        if(input$continuous_number_of_samples=='cont_one_sample'){
          succ_ia(type="cont",
                  nsamples=1,
                  null.value=input$continuous_parameter,
                  alternate=ifelse(input$continuous_hypothesis=='cont_smaller',"smaller","greater"),
                  N=input$continuous_total_sample_size,
                  n=input$continuous_interim_sample_size,
                  mean.ia=input$continuous_mean_interim,
                  stderr.ia=input$continuous_measure_dispersion,
                  mean.exp = input$continuous_post_interim_trend_mean_diff,
                  succ.crit="trial",
                  alpha.final=input$continuous_success_criteria_value)
        }else{
          succ_ia(type="cont",
                  nsamples=2,
                  null.value=input$continuous_parameter,
                  alternate=ifelse(input$continuous_hypothesis=='cont_smaller',"smaller","greater"),
                  N=input$continuous_total_sample_size,
                  n=input$continuous_interim_sample_size,
                  meandiff.ia=input$continuous_mean_diff,
                  a = input$continuous_allocation_ratio,
                  stderr.ia=input$continuous_measure_dispersion,
                  meandiff.exp = input$continuous_post_interim_trend_mean_diff,
                  succ.crit="trial",
                  alpha.final=input$continuous_success_criteria_value)
        }
      }
    }else{
      if(input$continuous_number_of_samples=='cont_one_sample'){
        succ_ia(type="cont",
                nsamples=1,
                null.value=input$continuous_parameter,
                alternate=ifelse(input$continuous_hypothesis=='cont_smaller',"smaller","greater"),
                N=input$continuous_total_sample_size,
                n=input$continuous_interim_sample_size,
                mean.ia=input$continuous_mean_interim,
                stderr.ia=input$continuous_measure_dispersion,
                mean.exp = input$continuous_post_interim_trend_mean_diff,
                succ.crit="clinical",
                clin.succ.threshold=input$continuous_success_criteria_value_clinical)  
      }else{
        succ_ia(type="cont",
                nsamples=2,
                null.value=input$continuous_parameter,
                alternate=ifelse(input$continuous_hypothesis=='cont_smaller',"smaller","greater"),
                N=input$continuous_total_sample_size,
                n=input$continuous_interim_sample_size,
                meandiff.ia=input$continuous_mean_diff,
                a = input$continuous_allocation_ratio,
                stderr.ia=input$continuous_measure_dispersion,
                meandiff.exp = input$continuous_post_interim_trend_mean_diff,
                succ.crit="clinical",
                clin.succ.threshold=input$continuous_success_criteria_value_clinical)
      }
    }
  }else{
    if(input$continuous_success_criteria=='cont_trial_success'){
      if(input$continuous_success_criteria_alpha =='cont_z_scale'){
        if(input$continuous_number_of_samples=='cont_one_sample'){
          succ_ia(type="cont",
                  nsamples=1,
                  null.value=input$continuous_parameter,
                  alternate=ifelse(input$continuous_hypothesis=='cont_smaller',"smaller","greater"),
                  N=input$continuous_total_sample_size,
                  n=input$continuous_interim_sample_size,
                  mean.ia=input$continuous_mean_interim,
                  sd.ia = input$continuous_measure_dispersion,
                  mean.exp = input$continuous_post_interim_trend_mean_diff,
                  succ.crit="trial",
                  Z.crit.final=input$continuous_success_criteria_value)
        }else{
          succ_ia(type="cont",
                  nsamples=2,
                  null.value=input$continuous_parameter,
                  alternate=ifelse(input$continuous_hypothesis=='cont_smaller',"smaller","greater"),
                  N=input$continuous_total_sample_size,
                  n=input$continuous_interim_sample_size,
                  meandiff.ia=input$continuous_mean_diff,
                  a = input$continuous_allocation_ratio,
                  sd.ia = input$continuous_measure_dispersion,
                  meandiff.exp = input$continuous_post_interim_trend_mean_diff,
                  succ.crit="trial",
                  Z.crit.final=input$continuous_success_criteria_value)
        }
      }else{
        if(input$continuous_number_of_samples=='cont_one_sample'){
          succ_ia(type="cont",
                  nsamples=1,
                  null.value=input$continuous_parameter,
                  alternate=ifelse(input$continuous_hypothesis=='cont_smaller',"smaller","greater"),
                  N=input$continuous_total_sample_size,
                  n=input$continuous_interim_sample_size,
                  mean.ia=input$continuous_mean_interim,
                  sd.ia = input$continuous_measure_dispersion,
                  mean.exp = input$continuous_post_interim_trend_mean_diff,
                  succ.crit="trial",
                  alpha.final=input$continuous_success_criteria_value)
        }else{
          succ_ia(type="cont",
                  nsamples=2,
                  null.value=input$continuous_parameter,
                  alternate=ifelse(input$continuous_hypothesis=='cont_smaller',"smaller","greater"),
                  N=input$continuous_total_sample_size,
                  n=input$continuous_interim_sample_size,
                  meandiff.ia=input$continuous_mean_diff,
                  a = input$continuous_allocation_ratio,
                  sd.ia = input$continuous_measure_dispersion,
                  meandiff.exp = input$continuous_post_interim_trend_mean_diff,
                  succ.crit="trial",
                  alpha.final=input$continuous_success_criteria_value)
        }
      }
    }else{
      if(input$continuous_number_of_samples=='cont_one_sample'){
        succ_ia(type="cont",
                nsamples=1,
                null.value=input$continuous_parameter,
                alternate=ifelse(input$continuous_hypothesis=='cont_smaller',"smaller","greater"),
                N=input$continuous_total_sample_size,
                n=input$continuous_interim_sample_size,
                mean.ia=input$continuous_mean_interim,
                sd.ia = input$continuous_measure_dispersion,
                mean.exp = input$continuous_post_interim_trend_mean_diff,
                succ.crit="clinical",
                clin.succ.threshold=input$continuous_success_criteria_value_clinical)
      }else{
        succ_ia(type="cont",
                nsamples=2,
                null.value=input$continuous_parameter,
                alternate=ifelse(input$continuous_hypothesis=='cont_smaller',"smaller","greater"),
                N=input$continuous_total_sample_size,
                n=input$continuous_interim_sample_size,
                meandiff.ia=input$continuous_mean_diff,
                a = input$continuous_allocation_ratio,
                sd.ia = input$continuous_measure_dispersion,
                meandiff.exp = input$continuous_post_interim_trend_mean_diff,
                succ.crit="clinical",
                clin.succ.threshold=input$continuous_success_criteria_value_clinical)
      } 
    }
    
  }
}
