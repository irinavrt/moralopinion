---
title: "Supplementary Information"
output:
    html_document:
      keep_md: TRUE
    pdf_document:
      fig_caption: yes
      keep_tex: yes
header-includes:
  - \usepackage[labelfont=bf]{caption}
  - \usepackage{float}
---

\captionsetup[figure]{name=Supplementary Figure}



# Supplementary methods

## 1. Simulation model

We here describe simulations carried out to investigate the robustness of the results of the mathematical model when (1) the probability that an agent is swayed by an argument is allowed to decline if the agent was not moved by that argument previously, and additionally (2) the population is not fixed but includes agents dying and being replaced by naïve agents who have yet to encounter any arguments. 

The simulation model includes 1 000 agents with an equal number of liberals and conservatives. As in the main model, liberals and conservatives differ in the kinds of arguments by which they may be swayed. Conservatives can be swayed by arguments based on any moral foundation, while liberals only accept arguments based on the harm or fairness foundation. The model assumes that there are a total of 10 arguments for each position. The ‘for’ position is always supported by 10 harm-fairness-based arguments. To vary the strength of the harm-fairness connection advantage, we let the number of harm-fairness-based arguments ‘against’ vary between 0 and 9 (with the remaining arguments being based on binding foundations). The population is assumed initially to have only 2% of liberals and 2% of conservatives holding the ‘for’ position. At every time step, each agent discusses an issue with another randomly assigned agent. Influence may occur only if the two agents hold opposite positions, in which case each agent is exposed to a randomly drawn argument for the position they are not currently holding. Given that an agent accepts the argument, the probability that the agent is swayed by it is initially set to 0.5. This probability decreases by a depreciation factor r (0 < *r* < 1) every time the agent is exposed to the argument but not swayed by it.

Running simulations of this model reveals the resulting dynamics to exhibit key similarities with the original model: the position with a harm-fairness connection advantage spreads both among liberals and conservatives, and at a rate that increases with the size of the advantage. However, a new phenomenon may occur if the deterioration in arguments is sufficiently strong compared to the harm-fairness connection advantage; namely, public opinion may then get stuck in a mixed equilibrium, that is, stop moving before the advantaged position has completely taken over, see Supplementary Fig. 1.

We now introduce a birth and death process by which agents are sometimes replaced by naïve individuals. Specifically, at each time point, a fixed proportion of randomly drawn agents is replaced with new agents that are of the same type (liberal or conservative) and hold the same position (for or against) as the agent they replace. As illustrated in Supplementary Fig. 2, the resulting dynamics from simulating this model has the same key features of the model in the main text: the position with a harm-fairness connection advantage spreads both among liberals and conservatives and at a speed that increases with the size of the advantage. Moreover, note that the inclusion of births and deaths typically makes public opinion continue to move until the advantaged position has taken over the whole population. Even with the most extreme deterioration rate of zero, which is equivalent to agents immediately disregarding any future argument they are not convinced by the first time, we find that even a 5% death rate can be enough for the advantaged position to take over completely.

# Supplementary Results 

## 1. The effect of opinion and beliefs about opinion change on the connection between position and arguments

In this section, we will verify that argument support for a moral position is not based on personal opinion of respondents nor on perceived opinion change. Supplementary Fig. 4 shows estimates for opinion, political leaning and beliefs about opinion change from the mixed-effect models. Both having an opinion in favour of a position and believing that the position has become more popular increases the probability of choosing universal arguments for the position. However, these effects together explain about 2% of the variation or less for all types of arguments.

We also control for our main result being enforced by the small but significant effects of individual opinions and beliefs about opinion change. We constructed an alternative measure of harm-fairness connection advantage in which, rather than using average harm and fairness support for each position, we averaged the predicted values from the mixed-effect models for the harm and fairness foundations, controlling for individual opinion, political leaning, and belief about opinion change. The main correlation between the opinion change rate and the alternative measure remained robust, *r*(72) = .69, p < 0.001, 95% CI [0.54, 0.79].

## 2. Harm-fairness connection advantage reliability

Here we verify that the chosen sample size is sufficient for reliable estimation of the harm-fairness connection advantage. Our end goal was to correlate the harm-fairness connection advantage to change rate estimates. Inaccuracy in the harm-fairness advantage estimates will bias the correlation. We approximated a possible bias with random draws of subsamples from the Mturk responses. The results are presented in Supplementary Fig. 5, which shows how the estimated correlation between the harm-fairness connection advantage and the rate of change reaches 0.7 already at n = 40.

We also found that the results are the same in the first and the second half of the data collection, *r*(72) = 0.97, p < 0.001, 95% CI [0.96, 0.98].


# Supplementary figures

<div class="figure" style="text-align: center">
<img src="supplement_files/figure-html/sfig1-1.png" alt="**Support of the advantaged position when the simulation model with argument deterioration has reached equilibrium.** Public opinion equilibria generated by 100 simulations with an argument deterioration rate of *r* = 0.5. The harm-fairness connection advantage is either large (model A: 10 harm-fairness arguments &quot;for&quot; and 2 &quot;against&quot;) or small (model B: 10 harm-fairness arguments &quot;for&quot; and 6 &quot;against&quot;). In the latter case, the plot shows that public opinion stops moving before the advantaged position has taken over completely."  />
<p class="caption">**Support of the advantaged position when the simulation model with argument deterioration has reached equilibrium.** Public opinion equilibria generated by 100 simulations with an argument deterioration rate of *r* = 0.5. The harm-fairness connection advantage is either large (model A: 10 harm-fairness arguments "for" and 2 "against") or small (model B: 10 harm-fairness arguments "for" and 6 "against"). In the latter case, the plot shows that public opinion stops moving before the advantaged position has taken over completely.</p>
</div>

![**How liberal and conservative opinions change when births and deaths are included in the simulation model with deteriorating arguments.** Opinion movement generated by the simulation model, averaged over 100 simulations, when the argument deterioration is set to the extreme value of *r* = 0 (immediate deterioration). The harm-fairness connection advantage is either large (model A: 10 harm-fairness arguments "for" and 2 "against") or small (model B: 10 harm-fairness arguments "for" and 6 "against"). As the death rate increases from 1% to 5%, we see that, even in the small advantage condition, the advantaged position eventually takes over the whole population.](supplement_files/figure-html/sfig2-1.png)

![**Proportion of liberal opinions within different ideological subcategories.** The average trend between 37 GSS items with absolute harm-fairness advantage larger than 0.2. All groups have become more liberal in their average opinions over the last 40 years. Trajectories of moderate ideological subcategories lie in between the trajectories of the extremes. The sample size varies between years with median 37 extremely liberal, 178 liberal, 208 moderately liberal, 575 moderate, 258 moderately conservative, 212 conservative, and 42 extremely conservative respondents.](supplement_files/figure-html/sfig3-1.png)


![**Effect of respondents’ moral opinion, political ideology and beliefs about public opinion change on applicability of arguments for different moral positions.** Having an opinion in favour of a position increases the probability of choosing universal arguments, and reduces the probability of choosing binding arguments. Believing that the position has become more popular has a similar but much smaller effect. Political leaning has no effect. Note that all these effects combined explain less than 3% of the variance in which argument was selected for each position (see Fig. 1). The sample size is 24 465 argument evaluations in the baseline model (black) and 14 492 in the extended model (grey). The group sizes are 74 GSS issues, 148 moral positions, 409 individuals in the baseline model and 219 in the full  model.](supplement_files/figure-html/sfig4-1.png)



![**Accuracy of the effect of harm-fairness connection advantage on rate of change as a function of sample size of argument measures.** The figure shows average regression coefficient, its standard error, and resulting correlation based on 200 random draws of n responses. The shade area shows 95% CI. Small sample sizes lead to an underestimation of the main effect, but not its precision. Sample sizes above 40 are sufficient to get an accurate estimate of the main result of the paper. Note that the sample used has on average 176 responses per issue.](supplement_files/figure-html/sfig5-1.png)

