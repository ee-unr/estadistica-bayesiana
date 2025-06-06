data {
  int<lower=0> N;             // Cantidad de observaciones
  array[N] int location_idx;  // Índice de la ciudad
  vector[N] temp9am;          // Temperatura a las 9 am (predictor)
  vector[N] temp3pm;          // Temperatura a las 3 pm (respuesta)
}
parameters {
  vector[2] beta0;      // Interceptos
  real beta1;           // Pendiente
  real<lower=0> sigma;  // Desvío estándar del error
}
transformed parameters {
  vector[N] mu;         // Predictor lineal
  mu = beta0[location_idx] + beta1 * temp9am;
}
model {
  beta0 ~ normal(17.5, 6.25);
  beta1 ~ normal(0, 10);
  sigma ~ normal(0, 15);
  temp3pm ~ normal(mu, sigma);
}
generated quantities {
  vector[N] y_rep;
  vector[N] log_likelihood;

  for (i in 1:N) {
    // Obtención de muestras de la distribución predictiva a posteriori
    y_rep[i] = normal_rng(mu[i], sigma);

    // Cálculo de la log-verosimilitud
    log_likelihood[i] = normal_lpdf(temp3pm[i] | mu[i], sigma);
  }
}
