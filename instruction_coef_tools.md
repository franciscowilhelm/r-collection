next,  modify coef_wrapper such that bayes no longer just doubles the p-values (pval) because thats not a permissible operation. 

Bayes is set to true manually right now. Instead we can just check the model (e.g., mod$input$analysis$estimator == "BAYES") if its bayes or not. 

If bayes, we should use the 95% CI to assign significance instead of using p-values. 
We can retrieve Mplus output for significance using "sig" column in model$parameters$unstandardized. So we need to bypass coef() and look directly at the mplus model's parameters output.



