# The connection between moral position and moral arguments drives opinion change

by P. Strimling, I. Vartanova, F. Jansson, and K. Eriksson (2019)

This study investigates how moral arguments drive opinion change. Based on a formal model of opinion dynamics as well as moral foundations theory, we studied what makes certain issue positions more popular among liberals than among conservatives, and what makes certain issue positions become steadily more popular over time while public opinion remains stable on other issues. An online survey examined the connection between moral positions and moral arguments. Our key findings comprise that i) certain moral positions are more common among liberals than among conservatives due to a larger amount of harm- and fairness-based arguments that speak for them; ii) both conservatives and liberals gradually shift opinions in the same (liberal) direction and at the same rate, with conservatives lagging behind; and iii) public opinion on a given issue tends to move at a rate related to how much better harm–fairness arguments connect with one position than with the opposing position.

This repository contains data and R code to reproduce the results of the paper. Read the paper here: https://rdcu.be/bRXqZ

## About the repository

The current subsection describes each folder and file to navigate easily through the repository moralopinion. 

- data [relative link] includes the GSS, Mturk and simulation data files.
	- arguments-agreement-rpt-models.rds
	- cleaned-arguments.rds
	- cleaned-gss.rds
	- formal-modal.rds
	- gss-items.csv
	- mf-measures.rds
	- mturk-argument-data.csv
	- mtruk-participants.csv
	- mturk-responses.rds
	- sim-argument-deterioration.rds
	- sim-death-process.rds
- figures [relative link]
	- Figure 1 [relative link] depicts the main factor in the applicability of arguments.
	- Figure 2 [relative link] displays the proportion of Liberals and Conservatives across time and Model A and B.
	- Figure 3 [relative link] depicts the proportions of harm-fairness advantage, Liberals vs. Conservatives, Education as well as Birth cohort across time. 
	- Figure 4 [relative link] displays the plot of public opinion change on the harm-fairness connection advantage.
	
- syntax [relativ link]
	- arguments-agreement.R
	- auxiliary_functions.R
	- figures.R
	- moralopinion.Rproj
	- prepare-data.R
	- results-in-text.R
	- results-in-text.md
	
- supplementary findings [relative link]
	- simulations.R
	- supplement.Rmd
	- supplement.pdf

## About the data

### GSS Data

The raw dataset and the cumulative codebook of the General Social Survey (1972-2018) can be found under the following [GSS website](http://gss.norc.org/documents/codebook/gss_codebook.pdf).

### Mturk Data

The raw and processed data comprises the following variables stemming from the Mturk dataset:

#### Variables of the [processed dataset] (data/mturk-agrument-data.csv)

- question = Moral issue from GSS questionnaire presented to participants ```[E.g. Do you think it should be possible for a pregnant woman to obtain a legal abortion if the woman wants it for any reason?]```
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
- party = Participants’ political party affiliation ```[Democrat Independent Republican]```
- ethnic = Participant's reported race ```[European other]```
- origin  = Participants’ country of origin. 

#### Variables of the raw datasets

The following variables in the raw datasets were not listed in the processed dataset and are therefore complementary. Note that variable names of the Mturk raw data mostly begin with "answer.", which is dropped in the processed data for simlicity. Variables that are otherwise identical in name (expect "answer.") are the same variables with different names (e.g. Answer.age=age).

##### Mturk responses [relative link]

- issue
- collection

##### Mturk participants [relative link]

- X1
- HitID
- HITTypeId
- Title: Title of the study.
- Description: Description of the HIT.
- Keywords: HIT keywords.
- Reward: Participants' monetary reward in Dollar.
- CreationTime: Date of data collection.
- MaxAssignments: Selection of Mturkers with maximum of 500 assignemnts. (?)
- RequesterAnnotation: Identification number of the launched batch.
- AssignmentDurationInSeconds: Duration of the HIT.
- AutoApprovalDelayInSeconds: Delay if approval in seconds.
- Expiration: Expiration date of the HIT.
- NumberOfSimilarHITs: Number of similar HITs.
- LifetimeInSeconds: ?
- AssignmentId: Unique ID generated for each worker taking the HIT. (Melis: Perhaps use instead of WorkerID?)
- AssignmentStatus: Status of the assignment (e.g. sumbitted or approved).
- AcceptTime: acceptance date and time of the assignment. 
- SubmitTime:
- AutoApprovalTime
- ApprovalTime
- RejectionTime
- RequesterFeedback:
- WorkTimeInSeconds:
- LifetimeApprovalRate
- Last30DaysApprovalRate
- Last7DaysApprovalRate
- Answer.comments
- Approve
- reject
- invite


## License