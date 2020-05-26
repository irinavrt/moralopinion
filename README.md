# The connection between moral position and moral arguments drives opinion change

by P. Strimling, I. Vartanova, F. Jansson, and K. Eriksson (2019)

This study investigates how moral positions and moral arguments drive opinion change.  Based on a formal model of opinion dynamics as well as moral foundations theory, we studied what makes certain issue positions more popular among liberals than among conservatives, and what makes certain issue positions become steadily more popular over time while public opinion remains stable on other issues. An online survey examined such connections between moral positions and moral arguments. Our key findings comprise that i) certain moral positions are more common among liberals than among conservatives due to a larger amount of harm- and fairness-based arguments that speak for them; ii) both conservatives and liberals gradually shift opinions in the same (liberal) direction and at the same rate, with conservatives lagging behind; and iii) public opinion on a given issue tends to move at a rate related to how much better harm–fairness arguments connect with one position than with the opposing position.

This repository contains data and R code to reproduce the results of the paper. Read the paper here: https://rdcu.be/bRXqZ

## About the data

We analyzed the data with R via RStudio, using the tidyverse package. The data comprises the following variables stemming from the Mturk dataset:

- question = Moral issue from GSS questionnaire presented to participants.
- order = Random order of presented question on Mturk.
- items = Different moral issues based on the GSS questionnaire that were displayed to participants.  
- answer = position = Participants’ answer to moral issues ```[question yes no]```
- arg = Participants’ argument for being for or against a moral issue ```[Now consider why you chose that answer. Which of the following arguments apply?]```
	- Harm
      - ```otherwise someone suffers emotionally```
      - ```then someone cares for someone weak or vulnerable```
      - ```otherwise someone is cruel```
  - Fairness
      - ```otherwise some people are treated differently from others```
      - ```otherwise someone acts unfairly```
      - ```otherwise someone is denied his or her rights```
  - Loyalty
      - ```then someone’s action shows love for his or her country```
      - ```otherwise someone does something to betray his or her group```
      - ```otherwise someone shows a lack of loyalty```
  - Authority
      - ```otherwise someone shows a lack of respect for authority```
      - ```then someone conforms to the traditions of society```
      - ```otherwise an action causes chaos or disorder```
  - Purity
      - ```otherwise someone violates standards of purity and decency```
      - ```otherwise someone does something disgusting```
      - ```then someone acts in a way that God would approve of```
  - Other
       - ```then everyone is free to do as they wanted```
       - ```otherwise someone’s freedom of choice is restricted```
       - ```then everyone is free to decide what group norms or traditions they want to follow```
       
- counter_arg = Participants’ beliefs on counterarguments ```[Now we would also like you to consider which of the arguments you would expect from someone who had answered no/yes to the question. They would answer no/yes because ...]```
	- Harm
      - ```otherwise someone suffers emotionally```
      - ```then someone cares for someone weak or vulnerable```
      - ```otherwise someone is cruel```
  - Fairness
      - ```otherwise some people are treated differently from others```
      - ```otherwise someone acts unfairly```
      - ```otherwise someone is denied his or her rights```
  - Loyalty
      - ```then someone’s action shows love for his or her country```
      - ```otherwise someone does something to betray his or her group```
      - ```otherwise someone shows a lack of loyalty```
  - Authority
      - ```otherwise someone shows a lack of respect for authority```
      - ```then someone conforms to the traditions of society```
      - ```otherwise an action causes chaos or disorder```
  - Purity
      - ```otherwise someone violates standards of purity and decency```
      - ```otherwise someone does something disgusting```
      - ```then someone acts in a way that God would approve of```
  - Other
       - ```then everyone is free to do as they wanted```
       - ```otherwise someone’s freedom of choice is restricted```
       - ```then everyone is free to decide what group norms or traditions they want to follow```
       
- workerId = Participants’ identification number (anonymized).
- change_belief = Publicopinion = Participants’ beliefs on public opinion change during the last 40 years on a 5-point scale ranging from ```support has gone down by 15 percentage points or more``` to ```support has gone up by 15 percentage points or more```.
- hf_advantage = Harm-fairness advantage.
- lib_hf_ad = Harm-fairness advantage of liberals.
- cons_hf_adv = Harm-fairness advantage of conservatives.
- opposite_hf_adv = Harm-fairness advantage estimated separately among those who hold the opposite position.
- polviews = Participant's party affiliation ```[liberal conservative]```
- age = Participant's reported age.
- gender = Participant's reported gender.
- politics =  Participants’ political orientation.
- party = Participants’ political party affiliation.
- ethnic = Participant's reported race.
- origin  = Participants’ country of origin. 

## About the repository

[Guiding description of the depository.]
