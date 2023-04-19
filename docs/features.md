# Features Description and Background Knowledge


- Age: age of the patient (years)
- Sex: sex of the patient (M: Male, F: Female)
- ChestPainType: chest pain type (TA: Typical Angina, ATA: Atypical Angina, NAP: Non-Anginal Pain, ASY: Asymptomatic)
- RestingBP: resting blood pressure (mm Hg)
- Cholesterol: serum cholesterol (mm/dl)
- FastingBS: fasting blood sugar (1: if FastingBS > 120 mg/dl, 0: otherwise)
- RestingECG: resting electrocardiogram results (Normal: Normal, ST: having ST-T wave abnormality (T wave inversions and/or ST elevation or depression of > 0.05 mV), LVH: showing probable or definite left ventricular hypertrophy by Estes' criteria)
- MaxHR: maximum heart rate achieved (Numeric value between 60 and 202)
- ExerciseAngina: exercise-induced angina (Y: Yes, N: No)
- Oldpeak: oldpeak = ST (Numeric value measured in depression)
- ST_Slope: the slope of the peak exercise ST segment (Up: upsloping, Flat: flat, Down: downsloping)
- HeartDisease: output class (1: heart disease, 0: Normal)

## Age

Type: int

Values: positive integers

Description: age of the patient in years

## Sex

Type: char 

Values: (M: Male, F: Female) 

Description: sex of the patient

## Chest Pain Type

Type: string 

Values: (TA: Typical Angina, ATA: Atypical Angina, NAP: Non-Anginal Pain, ASY: Asymptomatic)

### Description

Angina pectoris is a common presentation of myocardial ischemia in patients with obstructive 
coronary artery disease (CAD). Classically, **typical angina pectoris (TA)** is defined as 
substernal chest discomfort with a characteristic quality and duration, provoked by exertion 
or emotional stress, and relieved by rest or nitroglycerin. In contrast, **nontypical angina 
(NTA or ATA, for ATypical Angina)** may be defined as symptoms ascribed as angina that do not meet criteria for TA. 
Older studies have shown that patients with TA have a high pretest probability of 
obstructive CAD, especially in males and older patients; however, current study shows no 
association between TA and obstructive CAD or inducible myocardial ischemia.Patients who 
present with NTA can be misdiagnosed and have worse outcomes than TA patients. 
Atypical symptoms are commonly observed in females, the elderly, and among those with a 
history of diabetes mellitus and/or congestive heart failure, and no obstructive CAD. 
Prior contemporary work indicates that patients with signs and symptoms of ischemia but no 
obstructive CAD have a relatively high prevalence of coronary microvascular dysfunction (CMD), 
an elevated adverse cardiac events rate, and increased healthcare resource utilization.




