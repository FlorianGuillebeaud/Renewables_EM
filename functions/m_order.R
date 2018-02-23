## return the vector of quantities in "merit order"

m_order = function(quantities)
{
  if (length(quantities)==1) return(quantities)
  else{
    new_quantities = vector()
    new_quantities[1] = 0 
    for (i in 2:length(quantities)){
      quantities_sorted = sort(quantities) 
      new_quantities[i] = new_quantities[i-1]+quantities[i-1] 
    }
    return(new_quantities)
  }
}

