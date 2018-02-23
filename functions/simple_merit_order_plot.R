## Merit order plot ## 

simple_merit_order_plot = function(data, mytitle, set_demand, add_demand)
{
  if (set_demand == TRUE){    
      plot <- ggplot2::ggplot() +
      ggplot2::geom_step(data=data, mapping=ggplot2::aes(x=data[,1], y=data[,2])) +
      ggplot2::geom_step(data=data, mapping=ggplot2::aes(x=data[,1], y=data[,2]), direction="vh", linetype=0) +
      ggplot2::geom_point(data=data, mapping=ggplot2::aes(x=data[,1], y=data[,2]), color="red") + 
      ggplot2::labs(x = "Quantity (MWh)") +
      ggplot2::labs(y = "Price (Eur/MWh)") +
      ggplot2::ggtitle(paste0(mytitle)) +
      ggplot2::geom_vline(xintercept = add_demand, color = "green")
  }else{
      plot <- ggplot2::ggplot() +
      ggplot2::geom_step(data=data, mapping=ggplot2::aes(x=data[,1], y=data[,2])) +
      ggplot2::geom_step(data=data, mapping=ggplot2::aes(x=data[,1], y=data[,2]), direction="vh", linetype=0) +
      ggplot2::geom_point(data=data, mapping=ggplot2::aes(x=data[,1], y=data[,2]), color="red") + 
      ggplot2::labs(x = "Quantity (MWh)") +
      ggplot2::labs(y = "Price (Eur/MWh)") +
      ggplot2::ggtitle(paste0(mytitle))
  }
  print(plot)
}
