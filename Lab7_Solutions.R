hypothesisTest_z = function(test, alpha, mu0, n, xbar, sigma){
  z_test = (xbar-mu0)/(sigma/sqrt(n))
  if(test=="upper"){
    p_value = 1-pnorm(z_test)
    z_critical = qnorm(1-alpha)
    cat("h0 : mu=", z_test, "z_critical =", mu0, "\n")
    cat("z_test=", z_test, "z_critical =", z_critical, "\n")
    cat("p_value = ", p_value, "alpha=",alpha,"\n")
    if(z_test > z_critical){
      cat("Reject h0 since z_test > z_critical (or p_value < alpha")
    }else{
      cat("Fail to reject H0 since z_test > z_critical (or p_value > alpha)")
    }
  }else if(test=="lower"){
    p_value = 1-pnorm(z_test)
    z_critical = qnorm(1-alpha)
    cat("h0 : mu=", z_test, "z_critical =", mu0, "\n")
    cat("z_test=", z_test, "z_critical =", z_critical, "\n")
    cat("p_value = ", p_value, "alpha=",alpha,"\n")
    if(z_test > z_critical){
      cat("Reject h0 since z_test > z_critical (or p_value < alpha")
    }else{
      cat("Fail to reject H0 since z_test > z_critical (or p_value > alpha)")
    }
    #Lower-tailed test
  }else if(test=="twotailed"){
    #Two-tailed test
  }else{
    print("Error!")
  }
}

hypothesisTest_z("upper",0.05,8,50,8.3,2.0)