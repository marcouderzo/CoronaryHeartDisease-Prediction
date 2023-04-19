# Features Description and Background Knowledge


- **Age**: age of the patient (years)
- **Sex**: sex of the patient (M: Male, F: Female)
- **ChestPainType**: chest pain type (TA: Typical Angina, ATA: Atypical Angina, NAP: Non-Anginal Pain, ASY: Asymptomatic)
- **RestingBP**: resting blood pressure (mm Hg)
- **Cholesterol**: serum cholesterol (mm/dl)
- **FastingBS**: fasting blood sugar (1: if FastingBS > 120 mg/dl, 0: otherwise)
- **RestingECG**: resting electrocardiogram results (Normal: Normal, ST: having ST-T wave abnormality (T wave inversions and/or ST elevation or depression of > 0.05 mV), LVH: showing probable or definite left ventricular hypertrophy by Estes' criteria)
- **MaxHR**: maximum heart rate achieved (Numeric value between 60 and 202)
- **ExerciseAngina**: exercise-induced angina (Y: Yes, N: No)
- **Oldpeak**: oldpeak = ST (Numeric value measured in depression)
- **ST_Slope**: the slope of the peak exercise ST segment (Up: upsloping, Flat: flat, Down: downsloping)
- **HeartDisease**: output class (1: heart disease, 0: Normal)

## Age

Type: int

Values: positive integers

Description: age of the patient in years.

## Sex

Type: char 

Values: (M: Male, F: Female) 

Description: sex of the patient.

## ChestPainType

Type: string 

Values: (TA: Typical Angina, ATA: Atypical Angina, NAP: Non-Anginal Pain, ASY: Asymptomatic)

Description: type of chest pain.

### Background Knowledge

_Angina_ is a type of chest pain caused by reduced blood flow to the heart. Angina is a symptom of coronary artery disease. Angina is also called angina pectoris. Angina pain is often described as squeezing, pressure, heaviness, tightness or pain in the chest.

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

A chest pain is very likely **non-anginal (NAP)**, meaning non-cardiac related, if its duration is over 30 minutes or less than 5 seconds, it increases with inspiration, can be brought on with one movement of the trunk or arm, can be brought on by local fingers pressure, or bending forward, or it can be relieved immediately on lying down.


## RestingBP

Type: int

Values: positive integers

Description: resting blood pressure, expressed in mm Hg

### Background Knowledge

Blood pressure is a measure of the force that your heart uses to pump blood around your body, and is measured in millimetres of mercury (mmHg) and is given as 2 figures:

- systolic pressure: the pressure when your heart pushes blood out.
- diastolic pressure: the pressure when your heart rests between beats.

In this dataset, it seems that RestingBP only shows systolic pressure, as values are in average well over 130 mm Hg, so it is unlikely to be diastolic.


### Cholesterol

Type: int

Values: positive integers

Description: A person's serum cholesterol level represents the amount of total cholesterol in their blood, and is measured in mm/dl.

### Background Knowledge

Cholesterol is any of a class of certain organic molecules called lipids. It is a sterol (or modified steroid), a type of lipid. Cholesterol is biosynthesized by all animal cells and is an essential structural component of animal cell membranes. 
Cholesterol also serves as a precursor for the biosynthesis of steroid hormones, bile acid and vitamin D. Cholesterol is the principal sterol synthesized by all animals. In vertebrates, hepatic cells typically produce the greatest amounts.

A person's serum cholesterol level represents the amount of total cholesterol in their blood. A person's serum cholesterol level comprises the amount of high-density lipoprotein (HDL), low-density lipoprotein (LDL), and triglycerides in the blood. Triglycerides are a type of fat bundled with cholesterol.


## FastingBS

Type: int

Values: (1: fasted, 0: not fasted)

Description: blood sugar level when the patient is fasted, measured in mg/dl. Value is 1 if FastingBS > 120 mg/dl, 0 otherwise

### Background Knowledge

This measures your blood sugar after an overnight fast (not eating). A fasting blood sugar level of 99 mg/dL or lower is normal, 100 to 125 mg/dL indicates you have prediabetes, and 126 mg/dL or higher indicates you have diabetes.




