data {
  int<lower=1> N;             // Cantidad de observaciones
  int<lower=0, upper=1> y[N]; // Vector de respuesta (0 y 1)
  vector[N] x;
}
parameters {
  real a;
  real b;
}
model {
  // Notar que 'a' y 'b' reciben priors uniformes
  y ~ bernoulli_logit(a + b * x);
}

