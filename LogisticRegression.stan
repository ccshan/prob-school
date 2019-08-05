data {
  int<lower=0> n;
  vector[n] score;
  int<lower=0,upper=1> survival[n];
}
parameters {
  real coeff0;
  real coeff1;
}
model {
  coeff0 ~ normal(0, 10);
  coeff1 ~ normal(0, 10);
  survival ~ bernoulli_logit(coeff0 + coeff1 * score);
}
