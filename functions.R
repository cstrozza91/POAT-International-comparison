### PROSPECTIVE AGE FUNCTION ###
prosp.age <- 
  function(age, new_age, ex, ref_ex){
    
    spline_param <-
      smooth.spline(age, ex)
    
    ex_smooth <- 
      predict(spline_param, x = new_age) %>% 
      as.data.frame() %>% 
      mutate(diff = abs(y - ref_ex))
    
    min_diff <- 
      ex_smooth %>% 
      mutate(diff = abs(y-ref_ex)) %>% 
      summarise(prosp = min(diff)) %>% 
      as.numeric()
    
    prosp_age <- 
      ex_smooth %>% 
      filter(diff == min_diff) %>% 
      select(x) %>% 
      as.numeric()
    
    return(prosp_age)
  }

### EQUIVALENT AGE FUNCTION ###
equi.age <- 
  function(age, new_age, qx, ref_qx){
    
    spline_param <-
      smooth.spline(age, qx)
    
    qx_smooth <- 
      predict(spline_param, x = new_age) %>% 
      as.data.frame() %>% 
      mutate(diff = abs(y - ref_qx))
    
    min_diff <- 
      qx_smooth %>% 
      mutate(diff = abs(y-ref_qx)) %>% 
      summarise(prosp = min(diff)) %>% 
      as.numeric()
    
    equi_age <- 
      qx_smooth %>% 
      filter(diff == min_diff) %>% 
      select(x) %>% 
      as.numeric()
    
    return(equi_age)
  }

### S.AGE FUNCTION ###
s.age <- 
  function(age, new_age, lx, ref_lx){
    
    spline_param <-
      smooth.spline(age, lx)
    
    lx_smooth <- 
      predict(spline_param, x = new_age) %>% 
      as.data.frame() %>% 
      mutate(diff = abs(y - ref_lx))
    
    min_diff <- 
      lx_smooth %>% 
      mutate(diff = abs(y-ref_lx)) %>% 
      summarise(prosp = min(diff)) %>% 
      as.numeric()
    
    s_age <- 
      lx_smooth %>% 
      filter(diff == min_diff) %>% 
      select(x) %>% 
      as.numeric()
    
    return(s_age)
  }

### MODAL AGE AT DEATH (SPLINE) ###
mode.spline <- 
  function(age, new_age, dx){
    
    spline_param <-
      smooth.spline(age, dx)
    
    dx_smooth <- 
      predict(spline_param, x = new_age) %>% 
      as.data.frame()
    
    max_dx <-
      dx_smooth %>% 
      summarise(mode_spline = max(dx_smooth)) %>% 
      as.numeric()
    
    mode_spline <- 
      dx_smooth %>% 
      filter(y == max_dx) %>% 
      select(x) %>% 
      as.numeric()
    
    return(mode_spline)
  }