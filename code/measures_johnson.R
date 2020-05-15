# measures defined in Ren et al. 2008
# with equidistant grid 

integrate_fun <- function(X, 
                          n = nrow(X),
                          nxgrid = ncol(X), 
                          xind = matrix(as.vector(1:ncol(X)), 
                                        nrow=nrow(X), 
                                        ncol=nxgrid, 
                                        byrow=T),
                          integration = "simpson")
{
  
  # copied from refund:::pffr
  # credits to Fabian Scheipl
  L <- switch(integration,
              "simpson" = {
                # \int^b_a f(t) dt = (b-a)/gridlength/3 * [f(a) + 4*f(t_1) + 2*f(t_2) + 4*f(t_3) +
                # 2*f(t_3) +...+ f(b)]
                ((xind[,nxgrid]-xind[,1])/nxgrid)/3 *
                  matrix(c(1, rep(c(4, 2), length=nxgrid-2), 1), nrow=n, ncol=nxgrid, byrow=T)
              },
              "trapezoidal" = {
                # \int^b_a f(t) dt = .5* sum_i (t_i - t_{i-1}) f(t_i) + f(t_{i-1}) =
                #	(t_2 - t_1)/2 * f(a=t_1) + sum^{nx-1}_{i=2} ((t_i - t_i-1)/2 + (t_i+1 - t_i)/2) * f(t_i) + 
                # ... +
                #			+ (t_nx - t_{nx-1})/2 * f(b=t_n)
                diffs <- t(apply(xind, 1, diff))
                .5 * cbind(diffs[,1],
                           t(apply(diffs, 1, filter, filter=c(1,1)))[,-(nxgrid-1)],
                           diffs[,(nxgrid-1)])
              },
              "riemann" = {
                # simple quadrature rule:
                # \int^b_a f(t) dt = sum_i (t_i-t_{i-1})*(f(t_i))
                diffs <- t(apply(xind, 1, diff))
                #assume delta(t_0=a, t_1) = avg. delta
                cbind(rep(mean(diffs),n), diffs)
              }
  )
  
  apply(L*X,1,sum)
  
}

RMSE <- function(actual_mat, pred_mat, time_diff = ncol(actual_mat)-1, ...)
{
  
  sqrt(integrate_fun((actual_mat - pred_mat)^2, ...)/time_diff)
  
}

relRMSE <- function(actual_mat, pred_mat, ...)
{
  
  nom <- RMSE(actual_mat, pred_mat, ...)
  denom <- 0.5 * (apply(actual_mat, 1, function(x) diff(range(x))) + 
                    apply(pred_mat, 1, function(x) diff(range(x))))
  return(nom/denom)
  
}

cor_fun <- function(actual_mat, pred_mat)
{
  
  sapply(1:nrow(actual_mat), function(i) cor(actual_mat[i,], pred_mat[i,])) 
  
}

all_measures <- function(actual_mat, pred_mat, ...)
{
  
  data.frame(RMSE = RMSE(actual_mat, pred_mat, ...),
             relRMSE = relRMSE(actual_mat, pred_mat, ...),
             cor = cor_fun(actual_mat, pred_mat))
  
}