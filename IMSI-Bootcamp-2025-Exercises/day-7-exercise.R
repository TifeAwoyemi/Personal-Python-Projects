# Exercise 3: Randomizer ----

## (a) Randomness by Hand ----
a = 22695477
c = 1
m = 2^32
seed = 5040

X0 = seed

X1 = (a * X0 + c) %% m
X1

X2 = (a * X1 + c) %% m
X2

X3 = (a * X2 + c) %% m
X3

X4 = (a * X3 + c) %% m
X4

## (b) Crafting a linear number generator ----
my_lcg = function(n, a, c, m, seed) {

  my_lcg_draw = rep(0, n)

  my_lcg_draw[1] = seed

  for (i in 2:n) {

    my_lcg_draw[i] = (a * my_lcg_draw[i - 1] + c) %% m

  }

  return(my_lcg_draw)

}

### Test (b) ----
my_lcg(n = 4, a, c, m, seed)


## (c) Creating your own Uniform Distribution ----

my_runif = function(n, start_val, end_val, lcg_a, lcg_c, lcg_m, lcg_seed) {

  X = my_lcg(n, lcg_a, lcg_c, lcg_m, lcg_seed)

  X_max = lcg_m - 1

  my_runif_draw = start_val + (X / X_max) * (end_val - start_val)

  return(my_runif_draw)

}

### Test (c) ----

lcg_a = 22695477
lcg_c = 1
lcg_m = 2^32
lcg_seed = 5040

my_runif(n = 4, start_val = 1, end_val = 10, lcg_a, lcg_c, lcg_m, lcg_seed)


## (d) Sampling from a Triangular Distribution ----

my_rtri = function(n, a, b, c, lcg_a, lcg_c, lcg_m, lcg_seed) {

  U = my_runif(n, start_val = 0, end_val = 1, lcg_a, lcg_c, lcg_m, lcg_seed)

  T_vec = ifelse((U < (c - a) / (c - b)),
                 a + sqrt(U * (b - a) * (c - a)),
                 b - sqrt((1 - U) * (b - a) * (c - a))
  )

  return(T_vec)

}

### Test (d) ----

my_rtri(n = 4, 2, 6, 4, lcg_a, lcg_c, lcg_m, lcg_seed)

## (e) Verifying Number Generation ----

n = 250

# Use the LCG values specified in (a)
lcg_a = 22695477
lcg_c = 1
lcg_m = 2^32

# Use your favorite 4 digits
lcg_seed = 5040

# Compute and save the different RNG Values
rng_df = data.frame(
  i = seq_len(n),
  LCG = my_lcg(n, lcg_a, lcg_c, lcg_m, lcg_seed),
  Uniform = my_runif(n, 0, 1, lcg_a, lcg_c, lcg_m, lcg_seed),
  Triangular = my_rtri(n, 2, 6, 4, lcg_a, lcg_c, lcg_m, lcg_seed)
)

head(rng_df)

# Convert data to long form.
tidy_rng_df = tidyr::gather(rng_df,
                            key = "RNG Type",
                            value = "RNG",
                            -i)

# Reorder factor levels for facet display
tidy_rng_df$`RNG Type` = factor(tidy_rng_df$`RNG Type`,
                                c("LCG", "Uniform", "Triangular"))

ggplot(tidy_rng_df) +
  aes(x = RNG, fill = `RNG Type`) +
  geom_histogram(color = "black") +
  facet_wrap(~`RNG Type`, scales = "free") +
  labs(title = "Comparison of RNG Values Generated",
       subtitle = "RNG Types: LCG (Integer), Uniform (Real), and Triangular (Real)",
       y = "Frequency",
       x = "RNG Value",
       subcaption = "Data Science Summer Workshop Series @ IMSI") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Exercise 4: What kind of grade distribution does ... have? ----

## (a) Installing a developmental R Package from GitHub ----

remotes::install_github("illinois-r/uiucdata")

# eval:FALSE
uiucdata = uiucdata::grade_dist
head(uiucdata)

## (b) Removing a variable ----

col_ind = match(c('course_section', 'sched_type', 'term'), names(uiucdata))
col_ind

grade_dist_reduced = uiucdata[, -col_ind]

head(grade_dist_reduced)

## (c) Identifying Complete Records ----

grade_dist_complete = na.omit(grade_dist_reduced)

head(grade_dist_complete)

## (d) Dynamically obtaining data information ----

No_missing_vals = nrow(grade_dist_reduced) - nrow(grade_dist_complete)

paste("The number of observation with missing values in the original data =",
      No_missing_vals)

## (e) Retrieving only STAT course subjects ----

grade_dist_stat = subset(grade_dist_complete, subject == 'STAT')

head(grade_dist_stat)

## (f) Visualizing the data ----

ggplot(data = grade_dist_stat) +
  aes(x = a_plus) +
  geom_histogram() +
  facet_wrap(~year) +
  labs(
    title = "Distributions of A+ Grades Given in STAT Courses Over Time",
    subtitle = "Data from Summer 2005 to Summer 2017 based on FOIA Requests",
    y = "Frequency",
    x = "Number of A+ Grades Issued",
    caption = "Data Science Summer Workshop Series @ IMSI"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 75, hjust = 1))


# Exercise 5: Awake in a Nightmare ----

## (a) Making It Legibile ----

d = function(x) {
  length(x)
}

s = function(x) {
  x*x
}

u = function(x, ...) {
  m = 0
  k = 1

  while (k <= d(x)) {
    m = m + x[k]
    k = k + 1
  }

  m / d(x)
}

w = function(b, v, q = T) {
  n = d(b)
  o = 0

  for (i in seq_len(n)) {
    o = o + s(b[i] - v)
  }

  (1 / (n - 1 * (q == T)) * o)^(0.5)

}

n = function(p, i, ...) {
  m = u(i, ...)
  r = w(i, m)
  (p - m) / r
}

z = function(p, i, h = T, ...) {
  g = seq(-3, 3, length = 1000)
  gg = dnorm(g)
  plot(g, gg, type = "l", lwd = 1, ...)
  k = n(p = p,i = i, ...)

  if(h) {
    tt = c(g[g <= k], k, -3)
    ll = c(gg[g <= k], 0, 0)
  } else {
    tt = c(k, g[g >= k], 3)
    ll = c(0, gg[g >= k], 0)
  }

  polygon(tt, ll, col="red")
}

# Main function runs well
i = c(-1.21, 0.28, 1.08, -2.35, 0.43, 0.51, -0.57, -0.55, -0.56, -0.89)

z(1.57, i)

z(-2.33, i, h = F)

## (b) Making a hypothesis ----

# Main function runs well
i = c(-1.21, 0.28, 1.08, -2.35, 0.43, 0.51, -0.57, -0.55, -0.56, -0.89)

z(1.57, i)

z(-2.33, i, h = F)

# The output looks like a plot of a normal distribution of a given population/sample.
# So the functions calculate mean and standard deviation of a given data,
# standardize a given value based on the data distribution and plots this against
# generated normal distribution with mean = 0 and standard deviation = 1.

## (c) Document the existing functions ----

## d: Gets the length of a data
##      Args:
##        x = a numeric data
##      Returns:
##        Numeric value equivalent to the length of the data (aka total number
##        of observations or size of the population/sample)

d = function(x) {
  length(x)
}

## s: Squares each element of a given data
##      Args:
##        x = a numeric data
##      Returns:
##        Numeric vector equivalent to the square of the data

s = function(x) {
  x*x
}

## u: Calculates the mean of a given data
##      Args:
##        x = a numeric data
##        ... = optional arguments that can be listed later
##      Returns:
##        Numeric value equivalent to the mean of the data

u = function(x, ...) {
  m = 0
  # m is predefined with the intent to update in a loop

  k = 1
  # k is a counter

  while (k <= d(x)) {
    # while k <= the length of x

    m = m + x[k]
    # update m by adding each element of x

    k = k + 1
    # loop runs until k becomes greater than the length of x
    # m is now the cumulative sum of all elements in x
  }

  m / d(x)
  # Divide the cumulative sum by the length of x to get the mean of x

}

## w: Calculates the standard deviation of a given data
##      Args:
##        b = a numeric data
##        v = a scalar value which is the mean of the data
##        q = a Boolean value with default definition "T" denoted that the
##        data is a sample. If q = F is given, then the data is a population
##      Returns:
##        Numeric value equivalent to the standard deviation of the data

w = function(b, v, q = T) {
  n = d(b)
  # n is the length of the data b

  o = 0
  # o is predefined with the intent to update in a loop

  for (i in seq_len(n)) {
    # i is between 1 and n, the length of the data b

    o = o + s(b[i] - v)
    # v is subtracted from each element of i and this new value is squared and
    # then added to the current "o" value
  }
  # "o" is therefore the sum of squared error of the data

  (1 / (n - 1 * (q == T)) * o)^(0.5)
  # This expression gives the standard deviation of the data of a sample or
  # population depending on if q = T or q = F respectively.
}

## n: Calculates a value that is standardized from a data distribution
##      Args:
##        p = a scalar value
##        i = a numeric data
##      Returns:
##        Numeric value that is standardized from a distribution

n = function(p, i, ...) {
  m = u(i, ...)
  # m is a scalar value which is calculated from the mean function

  r = w(i, m)
  # r is a scalar value which is calculated from the standard deviation function

  (p - m) / r

  # A value that represents comparison of p to the distribution of data i
}

## z: Calculates the standard deviation of a given data
##      Args:
##        p = a scalar value
##        i = a numeric data
##        h = a Boolean value with default definition
##      Returns:
##        A plot of a normal distribution ...

z = function(p, i, h = T, ...) {
  g = seq(-3, 3, length = 1000)
  # A vector of length 1000 with a values evenly spaced between -3 and 3.

  gg = dnorm(g)
  # g is transformed to a normal distribution with mean 0 and standard
  # deviation 1.

  plot(g, gg, type = "l", lwd = 1, ...)
  # Plots the bare normal distribution

  k = n(p = p,i = i, ...)
  # Standardizes a value p in reference to a given data i

  if(h) {
    # If hypothesis == TRUE

    tt = c(g[g <= k], k, -3)
    # Make a vector of values with -3 and values in g such that g is less than
    # or equal to k

    ll = c(gg[g <= k], 0, 0)
    # Make another vector of values with c(0, 0) and values in gg such that g is
    # less than or equal to k

  } else {
    # If hypothesis == F

    tt = c(k, g[g >= k], 3)
    # Make a vector of values with 3 and values in g such that g is greater than
    # or equal to k

    ll = c(0, gg[g >= k], 0)
    # Make another vector of values with c(0, 0) and values in gg such that g is
    # greater than or equal to k
  }

  polygon(tt, ll, col="red")
  # Layer the plot above by shading the area (polygon) bounded by tt and ll.
}

## (d) Re-writing code ----


my_data_dist = function(dat, p, q = T, h = T) {

  n = length(dat)

  dat_mean = tail(cumsum(dat), 1) / n

  ss_err = tail(cumsum((dat - dat_mean) ^ 2), 1)

  dat_st_dev = sqrt(ss_err / (n - 1 * as.numeric(q)))

  dat_standardized_val = (p - dat_mean) / dat_st_dev

  evenly_spaced_seq = seq(-3, 3, length = 1000)

  norm_dist_seq = dnorm(evenly_spaced_seq)



  if(h) {

    x_shaded = c(evenly_spaced_seq[evenly_spaced_seq <= dat_standardized_val],
                 dat_standardized_val, -3)
    y_shaded = c(norm_dist_seq[evenly_spaced_seq <= dat_standardized_val],
                 0, 0)

  } else {

    x_shaded = c(dat_standardized_val,
                 evenly_spaced_seq[evenly_spaced_seq >= dat_standardized_val],
                 3)
    y_shaded = c(0,
                 norm_dist_seq[evenly_spaced_seq >= dat_standardized_val],
                 0)
  }

  plot(evenly_spaced_seq, norm_dist_seq, type = "l", lwd = 1)

  polygon(x_shaded, y_shaded, col="red")

}


## (e) Verifying correctness between routines ----

dat = c(-1.21, 0.28, 1.08, -2.35, 0.43, 0.51, -0.57, -0.55, -0.56, -0.89)

my_data_dist(dat, 1.57)

my_data_dist(dat, -2.33, h = F)




# Rough work ----

# for (i in 1:n) {
#
#   condition_1 = U[i] > 0 &  U[i] < (c - a) / (c - b)
#
#   condition_2 = U[i] >= (c - a) / (c - b) & U[i]< 1
#
#   if (condition_1) {
#
#   }
#
# }
# which(colnames(uiucdata) == 'course_section')
