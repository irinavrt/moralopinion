# The connection between moral position and moral arguments drives opinion change

by P. Strimling, I. Vartanova, F. Jansson, and K. Eriksson (2019)

This study investigates how moral arguments drive opinion change. Based on a formal model of opinion dynamics as well as moral foundations theory, we studied what makes certain issue positions more popular among liberals than among conservatives, and what makes certain issue positions become steadily more popular over time while public opinion remains stable on other issues. An online survey examined the connection between moral positions and moral arguments. Our key findings comprise that i) certain moral positions are more common among liberals than among conservatives due to a larger amount of harm- and fairness-based arguments that speak for them; ii) both conservatives and liberals gradually shift opinions in the same (liberal) direction and at the same rate, with conservatives lagging behind; and iii) public opinion on a given issue tends to move at a rate related to how much better harm–fairness arguments connect with one position than with the opposing position.

This repository contains data and R code to reproduce the results of the paper. Read the paper here: https://rdcu.be/bRXqZ

## About the data

### GSS Data

The raw dataset and the cumulative codebook of the General Social Survey (1972-2018) can be found under the following [GSS website](http://gss.norc.org/documents/codebook/gss_codebook.pdf).

### Mturk Data

The raw data (data/mturk-responses.csv) comprises the following variables stemming from the Mturk dataset:

- issue = Moral issue code corresponding to the GSS variable names.
- question = Moral issue from GSS questionnaire presented to participants ```[E.g. Do you think it should be possible for a pregnant woman to obtain a legal abortion if the woman wants it for any reason?]```
- opinion = Participants’ answer to moral issues ```[yes no]```
- arg = Participants’ selected arguments for being for or against a moral issue ```[Now consider why you chose that answer. Which of the following arguments apply?]```
	0 = Harm
  1 = Fairness
  2 = Loyalty
  3 = Authority
  4 = Purity
  5 = Liberty
  6 = Other (asked to specify in a separate field)
  
- counter_arg = Participants’ beliefs on counterarguments ```[Now we would also like you to consider which of the arguments you would expect from someone who had answered no/yes to the question. They would answer no/yes because ...]``` The codes are the same as in arg.
- workerid = Participants’ identification number (anonymized).
- belief_publicopinion = Participants’ beliefs on public opinion change during the last 40 years on a 5-point scale ranging from ```support has gone down by 15 percentage points or more``` to ```support has gone up by 15 percentage points or more```. This question was only asked in the second data collection. 
- collection = Identifies two diffeerent periods of data collection: 1 = collected between 2015.05.22 and 2015.05.27; 2 = collected between 2016.12.23 and 2017.01.04. 
- age = Participant's reported age.
- gender = Participant's reported gender.
- politics =  Participants’ political orientation.
  1 = Very Liberal
  2 = Liberal
  3 = Slightly Liberal
  4 = Moderate
  5 = Slightly Conservative
  6 = Conservative
  7 = Very Conservative
  0 = Libertarian
  
- party = Participants’ political party affiliation.
- ethnic = Participant's ethno-racial identity.

### License

CC BY-SA 4.0 license applies to the R code and Mturk data published in the repository.