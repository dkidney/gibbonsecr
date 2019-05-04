
# prep "two_occasion_sim" data

rm(list = ls())

suppressPackageStartupMessages({
    library(magrittr)
    library(tidyverse)
    library(forcats)
    library(stringr)
    library(gibbonsecr)
})

devtools::load_all(".")

par(mar = c(0,0,0,0), oma = c(0,0,0,0))

R = 4^2
K = 3
S = 1

D = 1
g0 = 1
sigma = 1
pcall = 1

samp = function(n, x, prob = NULL, replace = TRUE){
    sample(x, n, replace, prob)
}


# posts -----

# square grid
array_centres = expand.grid(
    x = seq(-5000, 5000, length.out = sqrt(R)),
    y = seq(-5000, 5000, length.out = sqrt(R))
) %>%
    as_data_frame %>%
    arrange(x, y) %>%
    rownames_to_column("array") %>%
    mutate(array = array %>% as.integer) %T>%
    print

# add/subtract adjustment to make diagonal listening post arrangement
adj = sqrt(500^2 / 2)
posts = bind_rows(
    array_centres %>%
        mutate(post = "A"),
    array_centres %>%
        mutate(post = "B", x = x - adj, y = y - adj),
    array_centres %>%
        mutate(post = "C", x = x + adj, y = y + adj)
) %>%
    select(array, post, x, y) %>%
    arrange(array, post) %>%
    mutate(post = paste(array, post, sep = "_")) %>%
    mutate(array = "1") %T>%
    print

plot(posts %>% select(x, y), asp = 1, ann = FALSE, axes = FALSE, pch = 19)


# usage -----

posts_long = 1:S %>%
    map(~posts) %>%
    bind_rows(.id = "occasion") %>%
    mutate(occasion = occasion %>% as.integer) %>%
    arrange(array, post, occasion) %>%
    select(array, post, occasion, everything()) %>%
    mutate(usage = as.integer(0.9 > runif(nrow(.)))) %T>% # 90 % usage probability
    print

posts = posts_long %>%
    select(-occasion) %>%
    nest(usage, .key = "usage") %>%
    mutate(
        usage = usage %>%
            map(unlist) %>%
            map_chr(paste, collapse = "")
    ) %T>%
    print


# covariates -----

covariates = posts_long %>%
    select(-x, -y, -usage)

post_occasion_level = covariates %>%
    mutate(
        observer = nrow(.) %>%
            samp(c("inexperienced", "experienced"), prob = c(0.4, 0.6))
    ) %T>%
    print

# post_level = covariates %>%
#     select(array, post) %>%
#     distinct %>%
#     mutate(
#         habitat_density = nrow(.) %>%
#             samp(c("dense", "open"), prob = c(0.4, 0.6))
#     ) %T>%
#     print

occasion_level = covariates %>%
    select(array, occasion) %>%
    distinct %>%
    mutate(
        weather = nrow(.) %>%
            samp(c("overcast", "clear"), prob = c(0.4, 0.6))
    ) %T>%
    print

covariates %<>%
    # left_join(post_level, by = c("array", "post")) %>%
    left_join(occasion_level, by = c("array", "occasion")) %>%
    left_join(post_occasion_level, by = c("array", "post", "occasion")) %>%
    print


# population -----

xlim = range(posts$x) + c(-1, 1) * 5000
ylim = range(posts$y) + c(-1, 1) * 5000

A = diff(range(xlim)) * diff(range(ylim)) / 1000^2 ; A
N = D * A ; N

population = data_frame(
    x = runif(N, xlim[1], xlim[2]),
    y = runif(N, ylim[1], ylim[2])
) %>%
    rownames_to_column("group") %T>%
    print

population %>%
    select(x, y) %>%
    plot(asp = TRUE, ann = FALSE, axes = FALSE, pch = 19, cex = 0.5, col = "grey")
posts %>%
    select(x, y) %>%
    points(pch = 19)


# distances -----

distances = calc_distances(
    posts %>% select(x, y),
    population %>% select(x, y)
) %>%
    `/`(1000) %>%
    as_data_frame %>%
    set_colnames(posts$post) %>%
    rownames_to_column("group") %>%
    left_join(population, by = "group") %>%
    gather(post, distance, -group, -x, -y) %T>%
    print


# detection probabiliities -----

detprobs = distances %>%
    left_join(posts_long %>% select(-x, -y), "post") %>%
    select(array, post, occasion, group, x, y, distance, usage) %>%
    mutate(detprob = distance %>% halfnormal(list(g0, sigma))) %T>%
    print


# calling prob -----

calling_probs = detprobs %>%
    select(array, occasion, group) %>%
    distinct %>%
    mutate(called = as.integer(pcall > runif(nrow(.)))) %T>%
    print


# detections -----

detections = detprobs %>%
    left_join(calling_probs, c("array", "occasion", "group")) %>%
    mutate(detected = as.integer(detprob > runif(nrow(.)))) %>%
    mutate(detected = detected * usage * called) %T>%
    print

detections %>%
    filter(detected == 1) %>%
    select(x, y) %>%
    points(col = 2, cex = 0.75)


# bearings -----

bearings = calc_bearings(
    posts %>% select(x, y),
    population %>% select(x, y)
) %>%
    as_data_frame %>%
    set_colnames(posts$post) %>%
    rownames_to_column("group") %>%
    left_join(population, by = "group") %>%
    gather(post, bearing, -group, -x, -y) %T>%
    print

detections %<>%
    left_join(bearings %>% select(-x, -y), by = c("group", "post")) %T>%
    print


# estimated bearings and distances -----

detections %<>%
    mutate(true_bearing = bearing) %>%
    mutate(true_distance = distance) %>%
    mutate(bearing = nrow(.) %>% rvonmises(list(50), bearing)) %>%
    mutate(distance = nrow(.) %>% rgamma(list(10), distance))


# data -----

data = import_data(
    detections = detections %>% filter(detected == 1),
    posts = posts,
    covariates = covariates,
    bearing_units = "radians",
    distance_units = "km"
) %T>%
    print


# mask -----

mask = data %>% make_mask %T>% print
mask$points %<>%
    mutate(habitat = if_else(y > 1500 - 6000 * sin(x / 5000), "open", "dense"))
mask


# plot -----

mask$points %$%
    plot(x, y, asp = 1, pch = 15, cex = 0.5, ann = FALSE, axes = FALSE,
         col = c("darkgreen", "lightgreen")[as.factor(habitat)])
population %>%
    select(x, y) %>%
    points(pch = 19, cex = 0.5, col = "red")
posts %>%
    select(x, y) %>%
    points(pch = 19)
detections %>%
    filter(detected == 1) %>%
    select(x, y) %>%
    points(pch = 19, col = "blue", cex = 0.75)


# initial fit -----

message("initial fit")

args = list(
    data = data,
    mask = mask,

    detfunc = "half normal",
    # detfunc = "hazard rate",

    # bearing_dist = "none",
    bearing_dist = "von mises",

    distance_dist = "none",
    # distance_dist = "gamma",
    # distance_dist = "log normal",

    # density = ~1,
    density = ~habitat,

    detfunc_intercept = 1,
    # detfunc_intercept = ~1,

    detfunc_scale = ~1,
    # detfunc_scale = ~habitat,

    detfunc_shape = ~1,
    # detfunc_shape = ~habitat,

    bearing_scale = ~1,
    # bearing_scale = ~observer,

    distance_scale = ~1,
    # distance_scale = ~observer,

    calling_prob = 1,
    # calling_prob = ~1
    # calling_prob = ~weather

    verbose = TRUE
)

stop("initial fit")

fit = do.call(fit_model, args) %T>% print
fit %>% coef %>% cbind
fit %>% plot("densurf")


# simulated data -----

stop("simulated data")

args$verbose = FALSE
args$start = c(

    "density.(Intercept)" = log(1),
    "density.habitatopen" = log(0.5 / 1),

    # "detfunc_intercept.(Intercept)" = logit(0.75),

    "detfunc_scale.(Intercept)" = log(1),

    "bearing_scale.(Intercept)"            = log(70)
    # "bearing_scale.observerinexperienced"  = log(10 / 70),

    # "distance_scale.(Intercept)"           = log(10),
    # "distance_scale.observerinexperienced" = log(10 / 100)

    # "calling_prob.(Intercept)"             = logit(0.5)
    # "calling_prob.weatherovercast"         = 1.0986125
)

if(0){
    sim_data = fit %>% simulate(coef = args$start)
    sim_data

    mask$points %$%
        plot(x, y, asp = 1, pch = 15, cex = 0.5, ann = FALSE, axes = FALSE,
             col = c("darkgreen", "lightgreen")[as.factor(habitat)])
    sim_data$detections %>% {
        filter(., array == array[1]) %$%
            points(x, y, pch = 19, cex = 0.5)
        filter(., detected == 1) %>%
            filter(array == array[1]) %$%
            points(x, y, pch = 19, cex = 1, col = 4)
    }

    plot(mask, "habitat")

    args$data = sim_data
    sim_fit = do.call(fit_model, args) %T>% print
    sim_fit %>% coef %>% cbind
    sim_fit %>% plot("densurf")

}

n_sims = 3

message("simulating data")
pb = txtProgressBar(0, n_sims, 0, "-", getOption("width") - 10, style = 3)
sim_data = 1:n_sims %>%
    map(function(i){
        sim_data = try(simulate(fit, coef = args$start), TRUE)
        setTxtProgressBar(pb, pb$getVal() + 1)
        if(inherits(sim_data, "try-error")) NULL else sim_data
    })
close(pb)

message("fitting models")
pb = txtProgressBar(0, n_sims, 0, "-", getOption("width") - 10, style = 3)
sim_fits = 1:n_sims %>%
    map(function(i){
        args$data = sim_data[[i]]
        sim_fit = try(do.call(fit_model, args), TRUE)
        setTxtProgressBar(pb, pb$getVal() + 1)
        if(inherits(sim_fit, "try-error") ||
           sim_fit$nlm$code > 2 ||
           sim_fit$nlm$iterations < 2) NULL else sim_fit
    })
close(pb)

df = sim_fits %>%
    compact %>%
    lapply(coef) %>%
    map(enframe, name = "coef") %>%
    bind_rows(.id = "sim") %T>%
    print

plot = df %>%
    ggplot(aes(x = value)) +
    geom_density() +
    geom_histogram(aes(y = ..density..), bins = 10) +
    facet_wrap(~ coef, scales = "free") +
    geom_vline(aes(xintercept = value), args$start %>% enframe("coef"))
plot %>% print



# simulation data -----

stop()

args$trace = FALSE

args$bearing_dist = "none"
args$distance_dist = "none"
args$rcpp = TRUE
fit = do.call(fit_model, args) ; fit
# args$rcpp = FALSE
# fit = do.call(fit_model, args) ; fit


args$bearing_dist = "von mises"
args$distance_dist = "none"
args$rcpp = TRUE
fit = do.call(fit_model, args) ; fit
# args$rcpp = FALSE
# fit = do.call(fit_model, args) ; fit


args$bearing_dist = "none"
args$distance_dist = "gamma"
args$rcpp = TRUE
fit = do.call(fit_model, args) ; fit
# args$rcpp = FALSE
# fit = do.call(fit_model, args) ; fit


args$bearing_dist = "von mises"
args$distance_dist = "gamma"
args$rcpp = TRUE
fit = do.call(fit_model, args) ; fit
# args$rcpp = FALSE
# fit = do.call(fit_model, args) ; fit



args$bearing_dist = "none"
args$distance_dist = "none"
args$debug = TRUE
args$trace = TRUE
args$loglik_only = TRUE
args$fit = TRUE

args$rcpp = FALSE
fit = do.call(fit_model, args)
fit

args$rcpp = TRUE
fit = do.call(fit_model, args)
fit



args$bearing_dist = "von mises"
args$distance_dist = "none"
args$debug = TRUE
args$trace = TRUE
args$loglik_only = TRUE
args$fit = TRUE

args$rcpp = FALSE
fit = do.call(fit_model, args)
fit

args$rcpp = TRUE
fit = do.call(fit_model, args)
fit



args$bearing_dist = "none"
args$distance_dist = "gamma"
args$debug = TRUE
args$trace = TRUE
args$loglik_only = TRUE
args$fit = FALSE

args$rcpp = FALSE
fit = do.call(fit_model, args)
fit

args$rcpp = TRUE
fit = do.call(fit_model, args)
fit









