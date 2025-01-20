## How polarized are the old and the young in Germany?
*A descriptive analysis of age group-related polarization in public opinion  between 1990 and 2018. A new method from Traber et al. (2023) is used for capturing polarization, which uses the overlapping of the group distributions instead of mean value differences. For mapping the public opinion distributions for the older and younger group over time Zhou's Hierarchical Item Response Model (2019) was chosen. In this approach latent variable is not at the individual level, but in a hierarchically structured manner in relation to selected covariates. In contrast to classic test theory, the mean values and standard deviations are not simply calculated from the items measuring political attitudes, but the latent variable is estimated using logit models.Taken together, this is an adequate approach to adequately investigate polarization between age groups.*

**Research Question:** How did the polarization of public opinion within and between generations in Germany change between 1990 and 2018?

**Data:** ALLBUS data from 1980-2018 
Download the date [here](https://search.gesis.org/research_data/ZA5284) and name it "ALLBUS1980-2018.dta" or skip this the data set preparation steps and start with the R script "Estimate with hgmr_Step3.R", where the uploaded "Data_Allbus,Zeit.Rdata" is used.  
For more information see: GESIS-Leibniz-Institut Für Sozialwissenschaften. (2021). ALLBUS/GGSS 1980-2018 (Kumulierte Allgemeine Bevölkerungsumfrage der Sozialwissenschaften/Cumulated German General Social Survey 1980-2018)Allgemeine Bevölkerungsumfrage der Sozialwissenschaften ALLBUS - Kumulation 1980-2018 (1.1.0) [Data set]. GESIS Data Archive. [https://doi.org/10.4232/1.13748](https://doi.org/10.4232/1.13748)

**Method**:  Hierarchical Item Response Model. Method based on the following papers: 
*  Traber, D., Stoetzer, L. F., & Burri, T. (2023). Group-based Public Opinion Polarization in Multi-Party Systems. West European Politics, 2023, 46(4), 652-677. [https://doi.org/10.1080/01402382.2022.2110376]https://doi.org/10.1080/01402382.2022.2110376)
*  Zhou, X. (2019). Hierarchical Item Response Models for Analyzing Public Opinion. Political Analysis, 27(4), 481–502. [https://doi.org/10.1017/pan.2018.63](https://doi.org/10.1017/pan.2018.63)

**Abstract**  
In recent years, the extent to which the political values of younger and older people differ has been increasingly discussed in the media and in academic circles. The question of whether there is an intensifying generational conflict is becoming increasingly relevant against the backdrop of an ageing population. The indirect, mediating effect of generational affiliation and age on political preferences is examined by micro- and macro-sociological explanatory approaches. The current state of research provides a diffuse picture of whether and to what extent the polarization between old and young in Germany has changed over time. The aim of this research paper is therefore to use a descriptive analysis to describe the polarization in political attitudes between younger and older generations in Germany and thus provide starting points for further research.  
A hierarchical item response model is calculated on the basis of data from the General Population Surveys from 1991 to 2018, which estimates the mean values and variance for an older (“post-war and affluent generation”, born between  1939 and 1964) and a younger cohort (“Generation X, Y and Z”, born between 1965 and 1995 ). This makes it possible to identify patterns in the direction of change and convergence within the two groups in their political attitudes. Political attitudes are measured via four topics: Family image, morality, redistribution and migration. In a second step, Bhattacharyya's distance measurement is used to analyse how the polarization between the groups has changed over time.  
In a nutshell, it can be seen that the younger cohort is more left-liberal in their political attitudes than the older cohort. Furthermore, it can be seen that both generation groups have become more left-liberal and heterogeneous in their political attitudes over the years. The gap in political attitudes has remained relatively stable over time, meaning that there is no clear polarization or depolarization. If the individual topics are examined individually, it becomes clear that the differences in political attitudes between old and young are different and change to varying degrees: While old and young are very similar when it comes to socio-political attitudes, the difference is greatest when it comes to the image of the family. However, there is no strong polarization or depolarization in any of the topics.

**key figures**:

*The interpreted and visualized results can be found in the research paper "Forschungspapier_Brandes_Polarisierung zwischen Generationen.pdf" (German).* 

![geschätzter Mittelwert und Varianz](https://github.com/user-attachments/assets/bc692e1e-1769-46e7-bb8a-31361ceae748){width=50%}

-  this figure shows the estimated average and the distribution around the average of the older generation on the left and the younger generation of the right over the time. Higher values on the y-axis indicates a more left-liberal political attitude while lower values represent more right-conservative political attitudes
-  overall, both generations get more left-liberal over the time.
-  the younger generation is in each time point more left-liberal than the older one
-  looking at the vertical lines, which ilustrate the 10%, 30% and 50% around the average, more variance can be found in the older generation than in the younger. That indicates, the older generation is more heterogeneous

![Cohesion_over_time](https://github.com/user-attachments/assets/7950ff03-1e08-4413-b38d-c8b594791ec0)

-  in this figure the ideological cohesion is visualized. It is measured by the estimated variance of the distribution and the upper and lower bandwidth of the variance. Blue is the older, orange the younger generation.
-  Comparing 1991 to 2018 in both generations much more heterogeneity can be found. This could be an indicator for an period effect, meaning that globalization and digitalization as mega trends reducing the effect of socio-demographic factors on individual political attitudes. In times of gobalization and digitalization individuals are more and more part of several groups, that offer multiple identifications. Thus, political attitudes are less determined by a particular group affiliation like a generation or sex. A high intra- and intergenerational mobility damps the effect.

![Verteilung über die Zeit](https://github.com/user-attachments/assets/f9f43a10-871b-479b-a7ff-825f69bd17e2)

-  this figure shows the distribution of political attitudes over time in comparison for four time points.
-  As you can see is the younger generation more homgeneous and more left-liberal. For both groups the curves elongated over time, what can be seen as an indicator for deploralization over time. Generational groups became less homogeneous and conclusion poliarization declines.

![Polarisierung](https://github.com/user-attachments/assets/3a917fe6-875d-4c08-888c-c8e8b7b0e035)

-  Finally, this figure shows the polarization between the groups, using Bhattacharyyas (1946) distance measurement.
-  We can see that the distance between the averages of the political attitudes stays nearly the same and only a minimal decrease of the distance is perceptible. All in all, the analysis gives no indicator for an increase in polarization, that could back up the mass an newspaper articles proclaiming the danger of polarization in Germany. 




**Infos concerning the code**  
Scripts were coded under R 4.3.2.  
Method was developed by Traber, Stoetzer and Burri (2023). Starting point of the data analysis was code written by [Lukas Stoetzer.](https://www.lukas-stoetzer.org/)
Used libraries are listed at the beginning of the script.





