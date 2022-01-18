## When estimating random effects, if group sizes are unbalanced, does this bias the estimates? 

library(tidyverse)
library(lme4)
library(broom)
library(broom.mixed)
library(tictoc)

## Define simulation functions ----

#' Draw a sample of group effects (mus)
#' 
#' @param N Number of mus to draw
#' @param dist Distribution function for mus
#' @param M Mean of the distribution function
#' @param S Standard deviation of the distribution function
#' @param ... Other parameters passed to distribution function
#' @return Numeric vector of length N
draw_mus = function(N = 20, dist = rnorm, M = 0, S = 1, ...) {
    dist(n = N, mean = M, sd = S, ...)
}

#' Generate observations
#' 
#' @param N Number of groups
#' @param large_N How many groups are large (vs. small)
#' @param large_n Number of observations to collect from large groups
#' @param small_n Number of observations to collect from small groups
#' @param S Standard deviation of distribution of mu
#' @param const Constant value in generating y
#' @param noise_sd Standard deviation of (Gaussian) noise
#' 
#' @details Each group has a mean, mu, drawn using `draw_mus()`.  
#' 
#' @return Dataframe with one row per observation and columns `group_idx` (1 to N), group `mu`, `group_size`, `n` for the group, and the individual observation `obs`
draw_obs = function(N = 20, large_N = 2, large_n = 500, small_n = 25, 
                    S = 2, const = 1, noise_sd = 1) {
    tibble(group_idx = 1:N, 
           mu = draw_mus(N, S = S), 
           group_size = c(rep('large', large_N), 
                          rep('small', N - large_N)), 
           n = case_when(group_size == 'large' ~ large_n, 
                         group_size == 'small' ~ small_n), 
           noise = map2(n, noise_sd, ~ rnorm(.x, mean = 0, sd = .y))) |> 
        unnest(noise) |> 
        mutate(y = const + mu + noise)
}

## Visual check that draw_obs() is working correctly
# draw_obs() |>
#     ggplot(aes(group_idx, y, group = group_idx)) +
#     geom_violin() +
#     geom_point(position = 'jitter', alpha = .1)

#' Run the simulation (one time)
#' 
#' Runs one round of the simulation
#' @param ... Parameters passed to `draw_obs()`
#' @details After drawing the observations, the simulation uses `lme4:lmer()` to fit a random effects model with the formula `y ~ 1 + (1|group_idx)`.  It then extracts the random effects estimates, joins them with the true values, and calculates the residual. 
#' @return Dataframe with one row per group and columns
#'     - `group_idx` Group index
#'     - `estimate` Estimated group effect from the random effects model
#'     - `mu` True group effect
#'     - `group_size` Is the group large or small? 
#'     - `n` Sample size for the group
#'     - `data` Nested dataframe of observations (observed `y` and true `noise`)
#'     - `residual` Difference between true and estimated group effect
one_sim = function(...) {
    data = draw_obs(...)
    
    model = lmer(y ~ 1 + (1|group_idx), data = data)
    
    estimates = tidy(model, effects = 'ran_vals') |> 
        select(group_idx = level, 
               estimate) |> 
        mutate(group_idx = as.integer(group_idx)) |> 
        full_join(nest(data, data = c(y, noise)), 
                  by = 'group_idx') |> 
        mutate(residual = estimate - mu)
    return(estimates)
}
# one_sim()

#' Run the simulation many times
#' 
#' @param NN Number of iterations of the simulation
#' @param ... Other parameters passed to `draw_obs()`
#' @note `map()` doesn't seem to play nicely with simply using ... to handle all of the passed parameters.  The map index gets passed as the first element of ... and ultimately is interpreted as the number of large groups. 
#' @return Dataframe, same format as from `one_sim()`, with a simulation run index `sim_idx` column. 
many_sims = function(NN = 5, ...) {
    map_dfr(1:NN, \(x) one_sim(...), .id = 'sim_idx')
}

## Run the simulation ----
#' | Default parameter values:  
#' | 1k iterations
#' | 20 groups, 2 large
#' | Group effects (mu) drawn from N(0, 2)
#' | DV values (y) constructed as 1 + mu + N(0, 1)
#' | 500 observations from large groups, 25 from small groups
set.seed(2022-01-16)
# ~40 sec
tic()
sim_data = many_sims(NN = 1000)
toc()

## Analyze results ----
#' The point of random effects is to reduce overfitting by biasing estimates towards the mean across all groups (0 in this case).  So estimate-mu regression line will be flatter than identity line. 
ggplot(sim_data, aes(mu, estimate, color = group_size)) +
    geom_point(alpha = .01) +
    stat_function(fun = identity, color = 'black') +
    geom_smooth(method = 'lm') +
    scale_color_brewer(palette = 'Set1') +
    theme_minimal()

#' And residuals will be negatively correlated with true value
ggplot(sim_data, aes(mu, residual, color = group_size)) +
    geom_point(alpha = .01) +
    geom_hline(yintercept = 0, color = 'black') +
    geom_smooth(method = 'lm') +
    scale_color_brewer(palette = 'Set1') +
    theme_minimal()

#' The overall bias is small and similar for both large and small groups; 
#' recall group effects sd is 2 and noise sd is 1
sim_data |> 
    group_by(group_size) |> 
    summarize(est_bias = mean(residual))
