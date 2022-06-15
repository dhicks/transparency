---
title: "Something something values disclosure replication"
author: "Daniel J. Hicks and Emilio Lobato""
date: Typeset \today
abstract: |
	abstract

<!--bibliography: ../bibfile.bib-->
---

# Introduction #

Over the past 15 years, many philosophers of science have rejected the ideal of value-free science and related, traditional understandings of objectivity and political neutrality of science. *[Douglas and Elliott]*  According to the value-free ideal, social and political values — such as feminism, environmentalism, or the protection of human health — have no legitimate role to play in the evaluation of scientific hypotheses.  The value-free ideal is compatible with allowing social and political values to play important roles earlier and later in inquiry.  Specifically, these values may legitimately shape the content and framing of research questions — researchers might decide to investigate whether chemical X causes cancer out of a concern to protect human health — and will be essential when scientific findings are used to inform public policy — say, banning the use of chemical X.  But, according to the value-free ideal, these values must not influence the collection and analysis of data and the process of reaching an overall conclusion about whether or not chemical X causes cancer.  

Challenges to the value-free ideal argue that at least some social and political values may, or even should, play a role in the evaluation of scientific hypotheses.  Keeping with the example, the question of whether chemical X causes cancer has much more significant social and political implications than the question of whether chemical X fluoresces under ultraviolet light.  So, in the interest of protecting human health, it would be appropriate to *scientifically* accept the hypothesis that chemical X is carcinogenic based on relatively provisional evidence *[cite]*.  

While this kind of argument indicates that at least some social and political values may or should play at least some role in the evaluation of at least some scientific hypotheses, it doesn't provide us with much positive guidance:  which values, playing which roles, in the evaluation of which hypotheses?  *[New Direction]*  One (partial) answer to this set of questions appeals to the ideal of transparency:  scientists should disclose to the public the social and political values that have influenced their research *[Elliott]*.  But some philosophers have objected to this transparency proposal, arguing that it might actually undermine trust in science.  According to this objection, members of the public generally accept the ideal of value-free science; values disclosures would violate this ideal, making scientists (incorrectly) appear biased and untrustworthy *[John, Kovaka]*.  

This objection is an empirical prediction: if scientists disclose their values, they will be perceived as less trustworthy.  @ElliottValuesEnvironmentalResearch2017 conducted an online survey study to evaluate this prediction.  However, these authors collected data from a somewhat small sample using Amazon Mechanical Turk, and adopted an analytical approach that significantly diluted their sample across different conditions.  

In this paper, we report the results of a replication of @ElliottValuesEnvironmentalResearch2017 study 1, using a larger, more reliable sample and a more statistically efficient analytical approach.  



# Methods and Materials #

## Experimental design ##

We use the same experimental stimulus and design as @ElliottValuesEnvironmentalResearch2017, embedded within a larger survey with other results not reported here.  In the replication component, participants are randomly assigned to one of $3 \times 2$ (values \times harm) conditions.  In each condition, participants are first shown a single presentation slide and the following explanatory text: 

> For several decades, a scientist named Dr. Riley Spence has been doing research on chemicals used in consumer products. One chemical—Bisphenol A, popularly known as BPA—is found in a large number of consumer products and is suspected of posing risks to human health. Yet, scientists do not agree about these possible health risks. Dr. Spence recently gave a public talk in Washington, D.C., about BPA research. Here is the final slide from Dr. Spence's presentation. 

| No other information about the (fictional) Dr. Spence is provided.  The content of the slide varies across the conditions.  Each slide has the header "My conclusion," followed by a list of 2 or 3 bullets.  The first bullet, if present, makes a values disclosure, stating that either economic growth or public health should be a top national priority.  This first bullet is not present in the "no disclosure" values condition.  The second bullet, identical across conditions, states "I examined the scientific evidence on potential health risks of BPA."  The third bullet concludes that BPA either does or does not cause harm to people, depending on whether the subject is in the harm or *[not-harm]* condition. 

After viewing the slide, the participant is then asked to rate Dr. Spence's trustworthiness using a semantic differential scale.  @ElliottValuesEnvironmentalResearch2017 did not cite a source for their scale; we used the Muenster Epistemic Trustworthiness Inventory [METI\; @ElliottValuesEnvironmentalResearch2017], which substantially overlaps but is not identical to the @ElliottValuesEnvironmentalResearch2017 scale.  On the METI, participants rate the target scientist (the fictional Dr. Spence) on a 1-7 scale for 14 items, where each item is anchored at the ends by a pair of words, such as competent-incompetent or responsible-irresponsible.  In both @ElliottValuesEnvironmentalResearch2017 and our replication analysis, all items were averaged together to form a 1-7 composite measure of trustworthiness.  To aid interpretation, in the analysis the direction of the scale has been set so that increased values correspond to increasing perceived trustworthiness.  

Ethicists make a key distinction between trust and trustworthiness *[Baier]*.  Trust is a verb: placing one's trust in someone else, with respect to some activity or domain.  Trustworthiness is an assessment, of whether or not that trust is appropriate.  In the experiment, participants might trust Dr. Spence's conclusion even if they judge Spence to be untrustworthy, or vice versa; though we would typically expect these two to go together.  As its name indicates, METI assesses perceived trustworthiness, not trust (such as accepting Dr. Spencer's conclusion about BPA).  

@ElliottValuesEnvironmentalResearch2017 explain that they selected BPA as a complex, ongoing, public scientific controversy.  For our replication, we chose to keep BPA, rather than switching to a different public scientific controversy with a higher profile in 2021, such as climate change, police violence, voter fraud, or any of numerous aspects of the Covid-19 pandemic.  All of these controversies are highly politically charged, with prominent public experts and counterexperts *[cite]*.  While we anticipated some effects of political partisanship in the BPA case, we felt it would be less likely to swamp the values disclosure that was our primary interest.  

After filling in the METI, participants provided demographic information and other sections of the survey that are not examined here.  Due to researcher error, a question about the participants' values (whether they prioritize economic growth or public health) was omitted in the first wave of data collection; this question was asked in a followup wave.  

## Replication hypotheses and analytical approach ##

We identified 5 major findings from @ElliottValuesEnvironmentalResearch2017 for our replication attempt:  

*[consistent labeling of hypotheses]*

a. Modest correlation between values and ideology:  
	(i) Political liberals are more likely to prioritize public health over economic growth, compared to political conservatives; but (ii) a majority of political conservatives prioritize public health.  
b. Consumer risk sensitivity:  Scientists who find that a chemical harms human health are perceived as more trustworthy than scientists who find that a chemical does not cause harm. 
c. Transparency penalty: Scientists who disclose values are perceived as less trustworthy than scientists who do not.  
d. Shared values:  Given that the scientist discloses values, if the participant and the scientist share the same values, the scientist is perceived as more trustworthy than if the participant and scientist have discordant values.  
e. Variation in effects:  The magnitude of the effects above vary depending on whether the participant prioritizes public health or economic growth.  

Hypothesis c, the transparency penalty, corresponds to the objection to transparency:  disclosing values undermines trust in science.  However, the shared values effect works in the opposite direction, counteracting the transparency penalty.  

*[hypothesis a]*

Hypotheses b-e were analyzed using linear regression models as a common framework, with a direct acyclic graph (DAG) constructed a priori to identify appropriate adjustments (covariates) for hypotheses d and e.  


## Participants ##

Participants were recruited using the online survey platform Prolific, and the survey was administered in a web browser using Qualtrics.  Prolific has an option to draw samples that are balanced to be representative by age, binary gender, and a 5-category race variable (taking values Asian, Black, Mixed, Other, and White) for US adults *[https://researcher-help.prolific.co/hc/en-gb/articles/360019238413-Representative-samples-FAQ]*.  A recent analysis finds that Prolific produces substantially higher quality data than Amazon Mechanical Turk for online survey studies, though three of the five authors are affiliated with Prolific [@PeerDataQualityPlatforms2021].  Preliminary power analysis recommended a sample of approximately 1,000 participants to reliably detect non-interaction effects (hypotheses a-d).  

The study was approved by the UC Merced IRB on August 17, 2021, and data collection ran October 18-20, 2021.  As mentioned above, due to researcher error a question about participants' values was not included in the original survey.  A followup survey asking this question of the same group of participants was conducted December 8, 2021 through March 5, 2022.  








# Results #

After excluding participants who declined consent after opening the survey or did not complete the survey, we had 988 participants.  660 participants (67%) were randomly assigned to a values disclosure condition (scientist prioritizes either economic growth or public health); 844 participants (85%) responded to the followup question about their own values (participant prioritizes economic growth or public health).  Consequently, subsamples for hypotheses d and e were substantially smaller than the full analysis sample.  





We tested the hypothesis of a modest correlation between values and ideology in two ways. First, to test (H1a) whether political liberals are more likely to prioritize public health over economic growth compared to conservatives, we conducted a Spearman's rank order correlation. Results revealed a significant correlation in line with the hypothesis, Spearman's rho = -.47, p < .001. Political liberals were more likely than conservatives to value public health over economic growth. To test (H1b) that a majority of political conservatives priortize public health over economic growth we cross-tabulated the data. Results revealed that, contrary to the hypothesis, slightly more than half of the self-reported political conservatives in our sample reported valuing economic growth (51.7%) over public health (48.3%).

Next, we tested the hypotheses that (H2) scientists who find a chemical harms human health are perceived as more trustworthy than scientist who find that a chemical does not cause harm and (H3) a scientist who discloses values are perceived as less trustworthy than a scientist who does not. For this analysis, we regressed participants' METI ratings onto both the Conclusions and Disclosure experimental conditions. The full model was significant, adj. R^2 = .148, F(2, 985) = 86.59, p < .001 (see Table #). Specifically, results revealed that the conclusions reported by the scientist predicted participants' perceived trustworthiness in line with our hypothesis. Participants rated the scientist who reported that BPA causes harm as more trustworthy than the scientist who reported that BPA does not cause harm. By contrast, the results do not provide evidence in favor of our hypothesis that a scientist disclosing their values are perceived as less trustworthy than a scientist who does not disclose values. 

Next, we tested the hypothesis that (H4) if the participant and scientist share the same values, the scientist is perceived as more trustworthy than if the participant and scientist do not share the same values. For this and related analyses, we only included data from participants who were assigned to either of the Disclosure conditions and self-reported their own values, reducing the sample to 567. Following this, we created a new Shared Values variable as a composite of the participants' reported values and the scientist's values. For this analysis, because the Shared Values is a composite, we wanted to adjust the model to account for any direct contributions to participants' METI rating by either the scientist's values or the participants' values. Thus, we regression participant METI scores onto the participants' reported values, the scientist's values condition, and the composite Shared Values variable. The resulting model was significant, adj. R^2 = .065, F(3, 563) = 14.15, p < .001. However, contrary to expectations, Shared Values did not emerge as a significant predictor of participants' perceptions of the trustworthiness of the scientist. Rather, only the scientist's stated values significantly predicted how trustworthy participants rated the scientist such that a scientist who disclosed valuing public health was rated as more trustworthy than a scientist who disclosed valuing economic growth. This was true even for participants who valued economic growth over public health.

## Variation in Effect Sizes ## 

We ran the following analyses to test our hypothesis that (H5) the magnitude of the effects found for the tests of H2-H4 vary depending on whether the participant prioritizes public health or economic growth. Results are presented in Table #.

H5a - consumer risk sensitivity
To test whether the findings regarding consumer risk sensitivity vary as a function of participants' values, we regressed participants' METI ratings of the scientist in the stimuli onto the Conclusions condition, Participants' Values variable, and the Conclusions by Participants' values interaction term. The full model was significant, adj. R^2 = .173, F(3, 840) = 59.95, p < .001. As with the earlier analysis, results showed a main effect of the Conclusions condition, such that participants rated the scientist who reported that BPA causes harm as more trustworthy than the scientist who reported that BPA does not cause harm. However, this effect was qualified by a significant interaction with participants' values. Participants who prioritized public health over economic growth and read about a scientist who concluded that BPA causes harm rated the scientist as more trustworthy than participants who read about a scientist who concluded that BPA does not cause harm, regardless of those participants' values.


H5b - transparency penalty
To test whether the findings above about a hypothesized transparency penalty may vary based on participants' values, we regressed participants' METI ratings of the scientist onto the Disclosure condition variable, Participants' Values variable, and the Disclosure by Participants' Values interaction term. The full model was not significant, adj. R^2 < .001, F(3, 840) = 1.19, p = .31. As with the earlier analysis, our results do not provide evidence for a transparency penalty to the perceived trustworthiness of a scientist, either in general or interacting with participants' own reported values. 


H5c - shared values
To test whether the findings regarding Shared values vary as a function of participants' values, we regressed participants' METI ratings of the scientist in the stimuli onto the Shared Values variable, Participants' Values variable, and the Shared Values by Participants' values interaction term. The full model was significant, adj. R^2 = .065, F(3, 563) = 14.15, p < .001. Results from this analysis suggests that Shared Values does predict the perceived trustworthiness of a scientist, such that if participants share the same values as the scientist, the scientist is perceived as more trustworthy. The model revealed a significant interaction between Shared Values and Participants' Values such that the highest perceived trustworthiness ratings were from participants who prioritized public health over economic growth reading about a scientist who disclosed valuing public health. However, as revealed in our analysis for H4, the evidence that shared values between the participant and scientist are what predicts the perceived trustworthiness of a scientist should be interpreted with caution. Because of the inclusion of Participants' values and the Scientist's values in the analysis for H4, the set of predictor variables for that model and the set of predictor variables for this model are different linear combinations of each other. Hence why the resulting full model parameters are identical between the two models. In spite of this, we report the results of this analysis for the sake of completeness, as this was an a priori planned statistical analysis.

Because of the findings from the analysis run to test H4, we conducted an unplanned post hoc analysis to further test the hypothesis that the effects found in the earlier analysis vary as a function of participants' values. For this test, we regression METI scores onto the Scientist's Values experimental condition, the Shared Values composite variable, and their interaction term. This is, as with the previous model and the model reported to test H4, a different linear combination of the same predictor variables and therefore the full model parameters are identical. However, presentation of the model this way renders a clearer picture of what variables we tested seem to be driving perceptions of the trustworthiness of a scientist. The evidence here suggests that if a scientist chooses to disclose their values alongside their presentation of scientific research, what values they share predicts their perceived trustworthiness, and that the scientist in our study who disclosed valuing public health was perceived a significantly more trustworthy than the scientist who disclosed valuing economic growth.


# Conclusion #

