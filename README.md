This repository contains the tasks for the UNICEF Data and Analytics P3 technical evaluation.

-   Please clone this repository to your local computer. Once complete, push to your own GitHub repository and share the link. To preserve your anonymity, **do not fork** the repository. Additionally, ensure you do not include your name in the assessment. Assessments will be graded anonymously.

-   Please respect the confidential nature of this test by not discussing or sharing its contents with anyone.

-   Do not copy the contents or text from other documents or AI systems without crediting the respective sources.

-   Code and results should be uploaded to your own repository on GitHub. The code must be well-documented and ready to be executed as automatically as possible following best GitHub and coding practices. The code can be written in R, Python, or Stata.

-   The estimated duration of the assessment is 4 hours. You have **48 hours** to complete the assessment. Please do not make further commits after 48 hours, as they will not be considered for grading.

# What is the exercise?

Imagine you have joined the Strategic Data Analytics Team, a fast and flexible analytic unit typically required to provide perspectives and explorations within a few hours. The purpose of the Strategic Data Analytics Team is to:

-   Provide fast, concise, sound, engaging, and insightful data analytics on key topics for high-level technical management, helping them understand and communicate these topics.
-   Conduct quick explorations applying cutting-edge and emerging methodologies to offer new perspectives and insightful narratives with the data.

## **Task 1**

Your task is to calculate population-weighted coverage of health services (antenatal care and skilled birth attendance) for countries categorized as on-track and off-track in achieving under-5 mortality targets as of 2023.

### **Data**

-   Retrieve from the UNICEF Global Data Repository the following indicators at the country level from 2018 to 2022. [[LINK](https://data.unicef.org/resources/data_explorer/unicef_f/?ag=UNICEF&df=GLOBAL_DATAFLOW&ver=1.0&dq=.MNCH_ANC4+MNCH_SAB.&startPeriod=2018&endPeriod=2022)]

    -   Antenatal care 4+ visits (ANC4) - the percentage of women (aged 15-49 years) attended at least four times during pregnancy by any provider

    -   Skilled birth attendant (SAB) - the percentage of deliveries attended by skilled health personnel

-   Population data: United Nations World Population Prospects population estimates. Data can be found in the "01_rawdata" folder. [FILE: WPP2022_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT_REV1.xlsx]

-   Under-five mortality on-track and off-track classifications: Provided by the United Nations Inter-agency Group for Child Mortality Estimation (UN IGME), this classification indicates countries that are on-track or off-track to achieve the Sustainable Development Goal (SDG) target for under-five mortality (on-track if Status.U5MR is “achieved” or “on-track”; off-track if status is “acceleration needed”) to achieve the SDG target for under-five mortality in 2030. [FILE: On-track and off-track countries.xlsx]

### Step

1.  **Data preparation**

-   Clean and merge datasets based on common identifiers.

-   Filter for countries with a coverage estimate between 2018 to 2022. Use only the most recent coverage estimate for the country during these five years for the weighted average.

2.  **Calculate weighted averages for on-track and off-track countries**

-   Calculate the population-weighted coverage for on-track and off-track countries for ANC4 and SAB use the formula:

    Weighted Coverage = $$\frac{\sum_{i=1}^{n} x_i w_i}{\sum_{i=1}^{n} w_i}$$

    where x~i~ = country coverage estimate and w~i~ = projected births for 2022.

-   Create a visualization of your choice comparing population-weighted coverage estimates for on-track and off-track countries for each indicator, with a short paragraph on interpretation and caveats.

# Task 2

Your task is to produce a Data Perspective on the evolution of education for 4- to 5-year-old children. Particularly interesting for the Perspective is understanding how educational performance evolves month by month at these critical ages, considering both general education and specific subjects (e.g., literature and math, physical education). Methods should be sound and well-argued if necessary.

**Data Perspective**

"Data Perspective" is a document that provides a concise, clear, and technically sound analysis of the most relevant aspects of an issue. Data Perspectives usually include 1-2 clean and engaging images or panels to visually support the most important message, offer a high-level perspective, or highlight interesting findings. The narrative is clear and results-oriented, aiming to provide a comprehensive narrative of the topic with the analysis, while offering enough technical detail to justify and follow the methods used for analysis, evaluation, significance, or summary statistics.

This Data Perspective should not be longer than 2 pages. Code (along with the report in PDF or HTML) should be uploaded to the repository.

### Data

The data come from the 2019 Zimbabwe MICS6 survey. For more details, see <https://mics.unicef.org/surveys>. The MICS survey in Zimbabwe interviewed over 11,000 households, with more than 6,000 mothers and caregivers of children under 5 interviewed.

The data relevant to this task come from the Mother/Caregiver interview for children under 5. The data can be found in the "01_rawdata" folder. Only children aged 3 or 4 are included. [FILE: Zimbabwe_children_under5_interview.csv]

**Codebook**

**interview_date**: "Date of Interview"\
**child_age_years**: "Child age in years"\
**child_birthday**: "Child date of birth"\
**EC6**: "Can (name) identify or name at least ten letters of the alphabet?" "Yes=1/No=2/DK=8"\
**EC7**: "Can (name) read at least four simple, popular words?" "Yes=1/No=2/DK=8"\
**EC8**: "Does (name) know the name and recognize the symbol of all numbers from 1 to 10?" "Yes=1/No=2/DK=8"\
**EC9**: "Can (name) pick up a small object with two fingers, like a stick or a rock from the ground?" "Yes=1/No=2/DK=8"\
**EC10**: "Is (name) sometimes too sick to play?" "Yes=1/No=2/DK=8"\
**EC11**: "Does (name) follow simple directions on how to do something correctly?" "Yes=1/No=2/DK=8"\
**EC12**: "When given something to do, is (name) able to do it independently?" "Yes=1/No=2/DK=8"\
**EC13**: "Does (name) get along well with other children?" "Yes=1/No=2/DK=8"\
**EC14**: "Does (name) kick, bite, or hit other children or adults?" "Yes=1/No=2/DK=8"\
**EC15**: "Does (name) get distracted easily?" "Yes=1/No=2/DK=8"

The following educational areas (and related variables) can be considered:\
**Literacy + Math**: EC6, EC7, EC8\
**Physical**: EC9, EC10\
**Learning**: EC11, EC12\
**Socio-emotional**: EC13, EC14, EC15
