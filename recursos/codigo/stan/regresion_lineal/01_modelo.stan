data {
  int<lower=0> N;  // Cantidad de observaciones
  vector[N] x;     // Valores de la variable predictora
  vector[N] y;     // Valores de la variable respuesta
}
parameters {
  real beta0;           // Intercepto
  real beta1;           // Pendiente
  real<lower=0> sigma;  // Desvio estándar del error
}
transformed parameters {
  vector[N] mu;
  mu = beta0 + beta1 * x;
}
model {
  y ~ normal(mu, sigma);
}
generated quantities {
  // Obtención de muestras de la distribución predictiva a posteriori
  vector[N] y_rep;
  for (i in 1:N) {
    y_rep[i] = normal_rng(mu[i], sigma);
  }
}
