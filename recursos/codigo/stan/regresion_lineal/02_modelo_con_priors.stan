data {
  int<lower=0> N;  // Cantidad de observaciones
  vector[N] x;     // Valores del predictor
  vector[N] y;     // Valores de la respuesta
  real beta0_mu;   // Media del prior del intercepto
}
parameters {
  real beta0;
  real beta1;
  real<lower=0> sigma;
}
transformed parameters {
  vector[N] mu;
  mu = beta0 + beta1 * x;
}
model {
  beta0 ~ normal(beta0_mu, 10);
  beta1 ~ normal(0, 0.5);
  sigma ~ normal(0, 5);
  y ~ normal(mu, sigma);
}
generated quantities {
  // Obtención de muestras de la distribución predictiva a posteriori
  vector[N] y_rep;
  for (i in 1:N) {
    y_rep[i] = normal_rng(mu[i], sigma);
  }
}
