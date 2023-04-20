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


## RestingECG

Type: string

Values: (Normal, ST, LVH)

Description: Results of resting electrocardiogram. ST: having **ST-T** wave abnormality (T wave inversions and/or ST elevation or depression of > 0.05 mV). **LVH**: showing probable or definite left ventricular hypertrophy by Estes' criteria.

### Background Knowledge


An electrocardiogram (ECG or EKG) is a test that records the electrical activity of the heart. It consists of a series of waveforms that correspond to the different phases of the cardiac cycle.

<img src="https://upload.wikimedia.org/wikipedia/commons/thumb/9/9e/SinusRhythmLabels.svg/800px-SinusRhythmLabels.svg.png" width="600" height="600" >

The ECG trace consists of several waves and intervals, including:

- P wave: represents the depolarization (contraction) of the atria.
- QRS complex: represents the depolarization (contraction) of the ventricles.
- T wave: represents the repolarization (relaxation) of the ventricles.
- The ST segment is the part of the ECG that connects the QRS complex and the T wave. It represents the time when the ventricles are depolarized but have not yet fully repolarized.

ST-T wave abnormality refers to a change in the normal shape or position of the ST segment and T wave on an ECG. T wave inversions and/or ST elevation or depression of > 0.05 mV are the most common types of ST-T wave abnormalities.

T wave inversions are when the T wave is inverted, meaning that it is pointing downward instead of upward. This can be a sign of myocardial ischemia or infarction, which means that there is reduced blood flow or damage to the heart muscle.

ST elevation is when the ST segment is elevated above the baseline. This can be a sign of acute myocardial infarction (heart attack) or pericarditis (inflammation of the lining around the heart).

ST depression is when the ST segment is depressed below the baseline. This can be a sign of myocardial ischemia, which means that there is reduced blood flow to the heart muscle.



## MaxHR

Type: int

Values: positive integers between 60 and 202.

Description: maximum heart rate achieved by the patient.


## ExerciseAngina

Type: char

Values: (Y: Yes, N: No)

Description: whether or not the patient experiences exercise-induced angina (heart-related chest pain)

### Background Knowledge

Angina is pain in the chest that comes on with exercise, stress, or other things that make the heart work harder. It is an extremely common symptom of coronary artery disease, which is caused by cholesterol-clogged coronary arteries. Angina may feel like pressure in the chest, jaw or arm. It often occurs with exercise or stress. As the heart pumps harder to keep up with what you are doing, it needs more oxygen-rich blood. If this demand is not met, you may feel pain or discomfort in your chest.


