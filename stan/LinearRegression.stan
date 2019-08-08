data {
  int<lower=0> n;
  vector[n] x;
  vector[n] y;
}
parameters {
  real coeff0;
  real coeff1;
}
model {
  coeff0 ~ normal(0, 10);
  coeff1 ~ normal(0, 10);
  y ~ normal(coeff0 + coeff1 * x, 3);
}
