data {
  int<lower=1> N;             // Cantidad de observaciones
  array[N] int<lower=0, upper=1> y; // Vector de respuesta (0 y 1)
  array[N] int partido_idx;         // Índice del partido
  vector[N] x;
}
parameters {
  vector[3] a;
  vector[3] b;
}
model {
  y ~ bernoulli_logit(a[partido_idx] + b[partido_idx] .* x);
}
