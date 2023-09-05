# Title: Elite norms and mass polarization study 3 design diagnosis ----

# Notes: ----
    #* Description: script for diagnosing experimental design used in study 3 of Elite norms and mass polarization project ----
    #* Updated: 2022 - 02 - 16 ----
    #* Updated by: dcr ----

# Setup ----
    #* Set seed
set.seed(90210)
    #* modularly load packages ----
#install.packages('box')#install box package, if not already installed, to modularly load functions for packages
box::use(
    haven = haven[read_dta],
    dplyr = dplyr[mutate, case_when, select, filter, bind_rows],
    DeclareDesign = DeclareDesign[...],
    DesignLibrary = DesignLibrary[two_by_two_designer, two_arm_designer],
    fabricatr = fabricatr[fabricate, draw_binary, potential_outcomes, reveal_outcomes],
    randomizr = randomizr[complete_rs, complete_ra, conduct_ra, block_ra],
    estimatr = estimatr[lm_robust, difference_in_means],
    ggplot2 = ggplot2[ggplot, aes, geom_line, geom_point, theme_bw, labs, theme, ggsave, element_blank, element_text, guides, guide_legend],
    patchwork = patchwork[...]
)
    #* Load ANES Dataset
anes = read_dta('C:/Users/damon/Dropbox/datasets/anes_2020_timeseries/anes_2020_timeseries.dta')
# Declare population ----
knowledge = anes |>
    select(V201642, V201644, V201645, V201646, V201647) |>
    mutate(know1 = ifelse(V201642 == 1896, 1, 0),
            know2 = ifelse(V201644 == 6, 1, 0),
            know3 = ifelse(V201645 == 1, 1, 0),
            know4 = ifelse(V201646 == 1, 1, 0),
            know5 = ifelse(V201647 == 2, 1, 0),
            knowledge = ((know1 + know2 + know3 + know4 + know5)/5)
            )
attention = anes |>
    select(V201005) |>
    mutate(attention = ifelse(V201005 <= 0, NA, V201005))

pid = anes |>
    select(V201231x) |>
    mutate(pid = ifelse(V201231x <= 0, NA, V201231x))

white = anes |>
    select(V201549x) |>
    mutate(white = ifelse(V201549x <= 0, NA, V201549x))

# Simulate dataset
    #* Get means and std. dev from anes data for attention, knowledge, pid, and white ----
knowledgeMean = mean(knowledge$knowledge, na.rm = TRUE)
knowledgeSD = sd(knowledge$knowledge, na.rm = TRUE)
attentionMean = mean(attention$attention, na.rm = TRUE)
attentionSD = sd(attention$attention, na.rm = TRUE)
pidMean = mean(pid$pid, na.rm = TRUE)
pidSD = sd(pid$pid, na.rm = TRUE)
whiteMean = mean(white$white, na.rm = TRUE)
whiteSD = sd(white$white, na.rm = TRUE)
    #* Simulate vectors with these covariates ----
knowledgeSample = rnorm(mean = knowledgeMean, sd = knowledgeSD, n = 2000)
attentionSample = rnorm(mean = attentionMean, sd = attentionSD, n = 2000)
pidSample = rnorm(mean = pidMean, sd = pidSD, n = 2000)
whiteSample = rnorm(mean = whiteMean, sd = whiteSD, n = 2000)


# Declare the design ----
#` Description of true DGP
#` Make a sample with an N of 2000
#` Errors should be normally distributed and centered around zero
#` The outcome variable is linearly related to the treatment, which is randomly assigned and is between a control and treatment condition
#` The treatment variable is moderated by whether or not it is congruent with their political attitudes 
#` #` The effect sizes can vary anywhere between -0.9 and 0.9. However, my expectation is that i will represent the moderating effect size, the main effect of the treatment will be i/3, the main effect of attention will be i/6, knowledge will be i/6, gender will be i/10, and white will be i/15.

effect = c(-0.9, -0.8, -0.7,-0.6, -0.5, -0.4, -0.3, -0.2, -0.1, 0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
sample = c(500, 1000, 1500)
final_df = data.frame()
for(i in effect){
    for(n in sample){
        #** DGP
    study_dgp = declare_model(N = n, U = rnorm(N, sd = 0))
    #** 2x2 factorial design
        potential_outcomes = declare_potential_outcomes(Y_Z_0_P_0 = (0 + U), Y_Z_1_P_0 = (i/3 + U), Y_Z_0_P_1 = (i/3 + U), Y_Z_1_P_1 = (i + U))

        estimand_1 = declare_inquiry(ATE_Z = 0 * mean(Y_Z_1_P_1 - Y_Z_0_P_1) + (1 - 0) * mean(Y_Z_1_P_0 - Y_Z_0_P_0))

        estimand_2 = declare_inquiry(ATE_P = 0 * mean(Y_Z_1_P_1 - Y_Z_1_P_0) + (1- 0) * mean(Y_Z_0_P_1 - Y_Z_0_P_0))
        
        estimand_3 = declare_inquiry(ATE_ZP = mean((Y_Z_1_P_1 - Y_Z_1_P_0) - (Y_Z_0_P_1 - Y_Z_0_P_0)))

        assign_Z = declare_assignment(Z = complete_ra(N, prob = 0.5))

        assign_P = declare_assignment(P = block_ra(prob = 0.5, blocks = Z))

        reveal_Y = declare_reveal(Y_variables = Y, assignment_variables = c(Z, P))

        estimator_1 = declare_estimator(Y ~ Z + P, model = lm_robust, term = c('Z', 'P'), inquiry = c('ATE_Z', 'ATE_P'), label = paste('2x2 Main Effect', 'N =', n))

        estimator_2 = declare_estimator(Y ~ Z + P + Z:P, model = lm_robust, term = 'Z:P', inquiry = 'ATE_ZP', label = paste('2x2 Interaction', 'N =', n))

        factorial = study_dgp + potential_outcomes + estimand_1 + estimand_2 + estimand_3 + assign_Z + assign_P + reveal_Y + estimator_1 + estimator_2

    # Combine all of it
    design_diagnoses = diagnose_design(factorial, sims = 1000)
    tidy = broom::tidy(design_diagnoses) |>
        mutate(effect_size = i)
    final_df = rbind(final_df, tidy)
    print('Finished iteration')
}}
saveRDS(final_df, 'study_simulation.rds')

    #* Extract diagnosands
rmse = final_df |>
    filter(diagnosand == 'rmse')

bias = final_df |>
    filter(diagnosand == 'bias')

power = final_df |>
    filter(diagnosand == 'power')

rmsePlot = ggplot(data = rmse) +
    geom_line(aes(x = effect_size, y = estimate, group = estimator, linetype = estimator)) +
    geom_point(aes(x = effect_size, y = estimate, group = estimator, shape = estimator)) +
    theme_bw() +
    theme(legend.position = 'none', legend.title = element_blank(),text=element_text(size=16, family = 'sans')) + 
    labs(x = 'Effect Size', y = '', title = 'RMSE')
biasPlot = ggplot(data = bias) +
    geom_line(aes(x = effect_size, y = estimate, group = estimator, linetype = estimator)) +
    geom_point(aes(x = effect_size, y = estimate, group = estimator, shape = estimator)) +
    theme_bw() +
    theme(legend.position = 'none', text=element_text(size=16, family = 'sans')) +
    labs(x = '', y = '', title = 'Bias')
powerPlot = ggplot(data = power) +
    geom_line(aes(x = effect_size, y = estimate, group = estimator, linetype = estimator)) +
    geom_point(aes(x = effect_size, y = estimate, group = estimator, shape = estimator)) +
    theme_bw() +
    theme(legend.position = 'bottom', text=element_text(size=16, family = 'sans')) +
    labs(x = '', y = '', title = 'Power', caption = 'Data source: 1000 simulated designs using OLS with robust standard errors.', linetype = 'Estimator', shape = 'Estimator') +
    guides(linetype = guide_legend(nrow = 3))

diagnosesPlot = biasPlot / rmsePlot / powerPlot + theme(legend.position = 'bottom', ) + guides(linetype = guide_legend(nrow = 3)) + plot_annotation(caption = 'Data Source: 1000 simulated designs using OLS with robust standard errors') + plot_layout(heights = 5)
ggsave('figures/study_1_diagnoses.jpeg', dpi = 300)







# Declare the Design ----
    #* Study 1 ----
        #** DGP ----
#` Description of true DGP
#` Make a sample with an N of 2000.
#` Errors should be normally distributed
#` The outcome variable is linearly related to the treatment, which is randomly assigned and is between a control condition and a treatment condition.
#` The treatment variable is moderated by the degree to which a respondent reports that they pay attention to the news
#` The degree to which a respondent pays attention to the news is not random and is dependent on a number of factors such as knowledge, gender, and race.
#` The effect sizes can vary anywhere between -0.9 and 0.9. However, my expectation is that i will represent the moderating effect size, the main effect of the treatment will be i/3, the main effect of partisan congruence will be i/6, knowledge will be i/6, attention will be i/6, gender will be i/10, and white will be i/15.
effect = c(-0.9, -0.8, -0.7,-0.6, -0.5, -0.4, -0.3, -0.2, -0.1, 0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
finalDF = data.frame()
for(i in effect){
    study1DGP = declare_model(N = 2000, U = rnorm(2000), knowledge = knowledgeSample, attention = attentionSample, pid = pidSample, white = whiteSample, Z = draw_binary(0.5, N = 2000), female = draw_binary(0.5, N = 2000), potential_outcomes(Y ~ (0.6) * Z + (0.3) * attention + 1.8 * Z * attention + (0.3) * knowledge  + (.18) * female + (.09) * white + U))
           #** Two-arm design with interaction
study1TwoArm = study1DGP + 
    declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
    declare_assignment(Z = Z) +
    declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
            #*** Interaction and Main Effect
    declare_estimator(Y ~ Z + attention + knowledge + female + white, model = lm_robust, label = 'Main Effect') +
    declare_estimator(Y ~ Z + attention + Z:attention + knowledge + female + white, model = lm_robust, label = 'Interaction')
        #** 2-Arm Block
study1Block = study1DGP + 
    declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
    declare_assignment(Z = block_ra(blocks = attention, prob = 0.2)) +
    declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
        #** Interaction and Main Effect
    declare_estimator(Y ~ Z + attention + knowledge + female + white, model = lm_robust, label = 'Block Main Effect') +
    declare_estimator(Y ~ Z + attention + Z:attention + knowledge + female + white, model = lm_robust, label = 'Block Interaction')
        #** Compare Designs
design_diagnoses = diagnose_design(study1Block, study1TwoArm, sims = 1000)
tidy = broom::tidy(design_diagnoses) |>
    mutate(effect_size = i)
finalDF = rbind(finalDF, tidy)
print('Finished iteration')
}
saveRDS(finalDF, 'data/study_1_simulation.RDS')
    #* Extract diagnosands
rmse = finalDF |>
    filter(diagnosand == 'rmse')

bias = finalDF |>
    filter(diagnosand == 'bias')

power = finalDF |>
    filter(diagnosand == 'power' & effect_size <= 0.5 & effect_size >= -0.5)

rmsePlot = ggplot(data = rmse) +
    geom_line(aes(x = effect_size, y = estimate, group = estimator, linetype = estimator)) +
    geom_point(aes(x = effect_size, y = estimate, group = estimator, shape = estimator)) +
    theme_bw() +
    theme(legend.position = 'bottom', legend.title = element_blank(),text=element_text(size=16, family = 'sans')) + 
    labs(x = 'Effect Size', y = '', title = 'RMSE')
biasPlot = ggplot(data = bias) +
    geom_line(aes(x = effect_size, y = estimate, group = estimator, linetype = estimator)) +
    geom_point(aes(x = effect_size, y = estimate, group = estimator, shape = estimator)) +
    theme_bw() +
    theme(legend.position = 'none', text=element_text(size=16, family = 'sans')) +
    labs(x = '', y = '', title = 'Bias')
powerPlot = ggplot(data = power) +
    geom_line(aes(x = effect_size, y = estimate, group = estimator, linetype = estimator)) +
    geom_point(aes(x = effect_size, y = estimate, group = estimator, shape = estimator)) +
    theme_bw() +
    theme(legend.position = 'none', text=element_text(size=16, family = 'sans')) +
    labs(x = '', y = '', title = 'Power', caption = 'N = 2000')

diagnosesPlot = biasPlot + plot_spacer() + rmsePlot + plot_spacer() + powerPlot + plot_layout(ncol = 5, widths = c(2,0.1, 2, 0.1, 2))
ggsave('figures/study_1_diagnoses.jpeg', height = 10, width = 20, dpi = 300)
