<br>  
* This app lets you simulate experimental treatment assignment. It illustrates how in expectation, treatment groups are equal on all known and unknown background variables. 
<br>  
* Pick a number of treatments and click "Assign". You will then see
    + The means/proportions of six variables for one assignment, under the tab "One sample";
    + The distributions of treatment differences or other test statistics (see below) for a hundred assignments, under the tab "100 samples".
<br>  
* The dataset is the British Election Study 2017 that can be found at https://www.britishelectionstudy.com/data-object/2017-face-to-face/. It has been reduced to include the following six variables only: Age, Gender, Ethnicity (reduced to five categories), Education (reduced to four categories), Ideology (11-point left-right scale), Partner presence during interview
<br>  
* The variable "partner present during interview" is a variable that one would not usually include in analysis. In many designs such as online surveys, one could not even measure it. It is here included to illustrate the key strength of experimental randomization: Apart from *known* background variables, which we can control for statistically, randomization also stochastically equalizes groups on *unknown* variables. 
<br>  
* The histograms ("One hundred assignments") depict the distribution of test statistics when random assignment is repeated a hundred times. These statistics are different depending on the nature of the variable and the number of treatments:
    + For two treatments and continuous or binary variables (age, gender, ideology, partner), the histogram shows differences between the treatments for each assignment.
    + There is no such intuitive simple statistic for other cases, therefore the histograms show either chi-squared or F statistics.

