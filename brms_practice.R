library(brms)
library(tidyverse)
library(lme4)

##################################
####Binary
##################################
data('VerbAgg', package = 'lme4')


#-------------------1PL Models----------------------------
##hierarchical item parameters

#formula
formula_va_1pl <- bf(r2 ~ 1 + (1|item) + (1|id))

#prior
prior_va_1pl <- 
  prior('normal(0,3)', class = 'sd', group = 'id') +
  prior('normal(0,3)', class = 'sd', group = 'item')

#stancode
make_stancode(
  formula = formula_va_1pl,
  data = VerbAgg,
  family = brmsfamily('bernoulli', 'logit'),
  prior = prior_va_1pl
)

#fitting
fit_va_1pl <- brm(
  formula = formula_va_1pl,
  data = VerbAgg,
  family = brmsfamily('bernoulli', 'logit'),
  prior = prior_va_1pl
)

#summary
summary(fit_va_1pl)

#plot
plot(fit_va_1pl)

# extract person parameters
ranef_va_1pl <- ranef(fit_va_1pl)
person_pars_va_1pl <- ranef_va_1pl$id

# extract item parameters
item_pars_va_1pl <- coef(fit_va_1pl)$item

# plot item parameters
item_pars_va_1pl[, , "Intercept"] %>%
  as_tibble() %>%
  rownames_to_column() %>%
  rename(item = "rowname") %>%
  mutate(item = as.numeric(item)) %>%
  ggplot(aes(item, Estimate, ymin = Q2.5, ymax = Q97.5)) +
  geom_pointrange() +
  coord_flip() +
  labs(x = "Item Number")

# plot person parameters
person_pars_va_1pl[, , "Intercept"] %>%
  as_tibble() %>%
  rownames_to_column() %>%
  arrange(Estimate) %>%
  mutate(id = seq_len(n())) %>%
  ggplot(aes(id, Estimate, ymin = Q2.5, ymax = Q97.5)) +
  geom_pointrange(alpha = 0.7) +
  coord_flip() +
  labs(x = "Person Number (Sorted)")

#comparison to lme4 model
lme4_va_1pl <- glmer(
  r2 ~ 1 + (1 | item) + (1 | id),
  data = VerbAgg,
  family = binomial()
)
summary(lme4_va_1pl)


# person and item parameters are similar to those obtained by brms
coef(lme4_va_1pl)$item
ranef(lme4_va_1pl)$id

#-------------------2PL Models----------------------------
#formula
formula_va_2pl <- bf(
  r2 ~ exp(logalpha) * eta,
  eta ~ 1 + (1 |i| item) + (1 | id),
  logalpha ~ 1 + (1 |i| item),
  nl = TRUE
)

# specify some weakly informative priors
prior_va_2pl <- 
  prior("normal(0, 5)", class = "b", nlpar = "eta") +
  prior("normal(0, 1)", class = "b", nlpar = "logalpha") +
  prior("constant(1)", class = "sd", group = "id", nlpar = "eta") + 
  prior("normal(0, 3)", class = "sd", group = "item", nlpar = "eta") +
  prior("normal(0, 1)", class = "sd", group = "item", nlpar = "logalpha")

# fit the 2PL model
# this models throws some convergence warnings which are false
# positives and can be safely ignored
fit_va_2pl <- brm(
  formula = formula_va_2pl,
  data = VerbAgg,
  family = brmsfamily("bernoulli", "logit"),
  prior = prior_va_2pl,
)

# obtain some basic summaries
summary(fit_va_2pl)
plot(fit_va_2pl, ask = FALSE)

# extract item parameters
item_pars_va_2pl <- coef(fit_va_2pl)$item

# plot item parameters
# difficulties
eta <- item_pars_va_2pl[, , "eta_Intercept"] %>%
  as_tibble() %>%
  rownames_to_column()

# discriminations
alpha <- item_pars_va_2pl[, , "logalpha_Intercept"] %>%
  exp() %>%
  as_tibble() %>%
  rownames_to_column()

# plot difficulties and discrimination next to each other
bind_rows(eta, alpha, .id = "nlpar") %>%
  rename(item = "rowname") %>%
  mutate(item = as.numeric(item)) %>%
  mutate(nlpar = factor(nlpar, labels = c("Easiness", "Discrimination"))) %>%
  ggplot(aes(item, Estimate, ymin = Q2.5, ymax = Q97.5)) +
  facet_wrap("nlpar", scales = "free_x") +
  geom_pointrange() +
  coord_flip() +
  labs(x = "Item Number")

# extract person parameters
ranef_va_2pl <- ranef(fit_va_2pl)
person_pars_va_2pl <- ranef_va_2pl$id

# plot person parameters
person_pars_va_2pl[, , "eta_Intercept"] %>%
  as_tibble() %>%
  rownames_to_column() %>%
  select(-Est.Error) %>%
  arrange(Estimate) %>%
  mutate(id = seq_len(n())) %>%
  ggplot(aes(id, Estimate, ymin = Q2.5, ymax = Q97.5)) +
  geom_pointrange(alpha = 0.7) +
  coord_flip() +
  labs(x = "Person Number (Sorted)")


#-------------------GRM Models----------------------------
#formula
formula_va_ord_1pl <- bf(resp ~ 1 + (1|item) + (1|id))
fit_va_ord_1pl <- brm(
  formula = formula_va_ord_1pl,
  data = VerbAgg,
  family = brmsfamily('cumulative', 'logit'),
  prior = prior_va_1pl
)

#summary
summary(fit_va_ord_1pl)
plot(fit_va_ord_1pl, ask = FALSE)

# extract item and person parameters
ranef_va_ord_1pl <- ranef(fit_va_ord_1pl)

# plot person parameters
ranef_va_ord_1pl$id[, , "Intercept"] %>%
  as_tibble() %>%
  rownames_to_column() %>%
  arrange(Estimate) %>%
  mutate(id = seq_len(n())) %>%
  ggplot(aes(id, Estimate, ymin = Q2.5, ymax = Q97.5)) +
  geom_pointrange(alpha = 0.7) +
  coord_flip() +
  labs(x = "Person Number (Sorted)")
