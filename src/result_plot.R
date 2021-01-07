plot_function <- function(mean,sd,no_data_points){
  x <- seq(from=mean-4*sd, to=mean+4*sd, length.out=no_data_points)
  hx <- dnorm(x, mean=mean, sd=sd)
  return(list("x" = x, "hx" = hx))
}

pps_plot_output_cont <- function(input,mean_pred_wo_prior=NULL,sd_pred_wo_prior=NULL,mean_pred_w_prior=NULL,sd_pred_w_prior=NULL){
  if(input$continuous_prior_distribution_flag =='no'){
    plot_output <- plot_function(mean = mean_pred_wo_prior,sd=sd_pred_wo_prior,no_data_points = 1000)
    plot_data <- data.frame(type = "without Prior",value=plot_output$x,Density=plot_output$hx)  
  }else{
    plot_output <- plot_function(mean = mean_pred_wo_prior,sd=sd_pred_wo_prior,no_data_points = 1000)
    data_wo_prior <- data.frame(type = "without Prior",value=plot_output$x,Density=plot_output$hx)
    
    plot_output <- plot_function(mean = mean_pred_w_prior,sd=sd_pred_w_prior,no_data_points = 1000)
    data_w_prior <- data.frame(type = "with Prior",value=plot_output$x,Density=plot_output$hx)
    plot_data <- rbind(data_w_prior,data_wo_prior)
  }
  return(plot_data)
}

pps_plot_output_bin <- function(input,mean_pred_wo_prior=NULL,sd_pred_wo_prior=NULL,mean_pred_w_prior=NULL,sd_pred_w_prior=NULL){
  if(input$binary_prior_distribution_flag =='no'){
    plot_output <- plot_function(mean = mean_pred_wo_prior,sd=sd_pred_wo_prior,no_data_points = 1000)
    plot_data <- data.frame(type = "without Prior",value=plot_output$x,Density=plot_output$hx)  
  }else{
    plot_output <- plot_function(mean = mean_pred_wo_prior,sd=sd_pred_wo_prior,no_data_points = 1000)
    data_wo_prior <- data.frame(type = "without Prior",value=plot_output$x,Density=plot_output$hx)
    
    plot_output <- plot_function(mean = mean_pred_w_prior,sd=sd_pred_w_prior,no_data_points = 1000)
    data_w_prior <- data.frame(type = "with Prior",value=plot_output$x,Density=plot_output$hx)
    plot_data <- rbind(data_w_prior,data_wo_prior)
  }
  return(plot_data)
}

pps_plot_output_surv <- function(input,mean_pred_wo_prior=NULL,sd_pred_wo_prior=NULL,mean_pred_w_prior=NULL,sd_pred_w_prior=NULL){
  if(input$survival_prior_distribution_flag =='no'){
    plot_output <- plot_function(mean = mean_pred_wo_prior,sd=sd_pred_wo_prior,no_data_points = 1000)
    plot_data <- data.frame(type = "without Prior",value=plot_output$x,Density=plot_output$hx)  
  }else{
    plot_output <- plot_function(mean = mean_pred_wo_prior,sd=sd_pred_wo_prior,no_data_points = 1000)
    data_wo_prior <- data.frame(type = "without Prior",value=plot_output$x,Density=plot_output$hx)
    
    plot_output <- plot_function(mean = mean_pred_w_prior,sd=sd_pred_w_prior,no_data_points = 1000)
    data_w_prior <- data.frame(type = "with Prior",value=plot_output$x,Density=plot_output$hx)
    plot_data <- rbind(data_w_prior,data_wo_prior)
  }
  return(plot_data)
}
