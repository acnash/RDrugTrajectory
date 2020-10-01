<img align="center" src="https://github.com/acnash/RDrugTrajectory/blob/master/images/mamagnolia_acresunderconstruction.gif" alt="Under Construction"/>    

# RDrugTrajectory
An R package designed for the analysis of CPRD prescription electronic healthcare record (EHR) data.

RDrugTrajctory provides an API for analysing raw CPRD EHRs. Functions available include:
- loading & saving CPRD raw text files as R data.frames
- works over the medical.txt and product.txt data files (part of the CPRD data dictionary - not supplied with RDrugTrajectory)
- retrieving patient IDs, gender, age and index of multiple 
- retrieve all prescriptions with a matching disease date
- retrieve the first prescription event for a patient cohort
- drug prescription incidence rate
- survival analysis prescription to first event timeline generator 
- drug prescription demographic data anlysis
- mean cumulative function plots of drug prescriptions
- many more function....

## Motivation
CPRD electronic healthcare records are delivered as flat text files. Researchers have two choices, pay a lot of money to use existing tools or write tools of their own. Having faced this problem myself I started developing my own tools three years ago.

This R package came about as the result of supervising a very talented post-graduate Clinical Neuroscience student (please see Credits). The student's project, titled *"A Longitudinal Cohort Study of Migraine Preventative Medication Usage in UK CPRD"* required immediate access to primary care clinical and prescription records of patients suffering with headache disorders. The CPRD data can be very overwhelming, especially to those with little experience in electronic healthcare records or the manipulation of large data sets. To mitigate this concern I decided to develop a drug-prescription themed R API. Whilst I was writing the R package my student was putting it to use, performing the analysis necessary to answer the research aims whilst reporting bugs and suggesting additional features. My ethos is to build an R package that can interrogate CPRD records whilst requiring minimal R language experience (although some experience goes a long way).  

## Ethics
The data attached to the R package, images and examples presented here, and the images and examples in the accompanying publication (expected soon), **is fabricated and does not in any part represent real patient data.** Our accompanying researech articles have ISAC (Independent Scientific Advisory Committee - UK Gov) approval.

## Build status
Under pre-alpha stage until initial release - expected, early October 2020. Learn more about software life-cycles <a href="https://en.wikipedia.org/wiki/Software_release_life_cycle#Pre-alpha">here</a>. Whilst in active development I cannot gaurentee the results and all responsibility lies with the user. 

## Example Screenshots
Example results using **fabricated electronic healthcare records.**

Plotting the number of patients by their first drug prescription (Figure 1) is achieved by simply accessing the frequency data.frame in the FirstDrugObject list-type. The first element holds a named-list (by the drug ID) of patient id vectors, the second entry reflects the first entry in structure but holds the event dates, and the third element is a data.frame of drug by number of first prescriptions on record. With all of this information, one is able to analysis each drug type by social-demographic factors (functions provided). </br>

<center>
<img src="https://github.com/acnash/RDrugTrajectory/blob/master/images/prescription_frequency.png" width="450" />
    
**Figure 1.** First drug prescription (matched to a disease event e.g., headache) frequency. </br> </br>
</center>

RDrugTrajectory can produce the data structure necessary to perform a mean cumulative function (MCF) over several groups. Having first filtered the cohort for only medication of interest, and MCF plot reveals prescription burden by social-demographic factors, for example by social deprivation scores (Figure 2), or whole cohort populations. </br>

<center>
<img src="https://github.com/acnash/RDrugTrajectory/blob/master/images/IMD_prescriptions.png" width="450" />
    
**Figure 2.** Cumulative drug prescriptions stratified by patient IMD score.</br></br>
</center>

The first codedraft of RDrugTrajectory is able to plot the change in a patients medication starting from their first prescription (Figure 3). Later releases will factor in time.</br>

<center>
<img src="https://github.com/acnash/RDrugTrajectory/blob/master/images/drug_switch.png" width="450" />

**Figure 3.** Prescription drug changes. From first drug prescription matched with disease. The example does not take into account time. </br>
</center>

## Installation
Whilst in pre-alpha and alpha development phase please download a release and install locally:

    install.packages("path/to/tar/file", source = TRUE, repos=NULL) 
    library(rCPRD) 

Ideally you should also have a copy of the medical.txt and product.txt files that can be found inside the Windows installation of the CPRD data dictionary. These files are used to name products (e.g., drugs) and medical terms. In their absence the R package will still work, however, the *medcode* and *prodcode* entries will remain as codes.

## Code Example and Tutorial
Please see the R vignette, API and forth coming publication.  

## Code style
The conventional R code style with original Java/C++ code blocks (showing my age). I do not use the R dot-notation for naming variables and I have avoided the %>% infix notation where possible. The initial release 

## API Reference
Please see the available R vignette and forth coming publication. An API will be added here in due course.

## Tests
There are basic unit tests one can run. Instruction will be added here in due course.

## Problems, bugs, suggested features
Please raise an issue for bug fixes or suggested features/improvements. **Please note:** the code is only maintained by myself (Anthony Nash). Unfortunately, issues won't be resolved overnight! I have students to supervise, fellowships and papers to write and my own research to be getting on with. Please be patient. 

## Credits
**Dr Anthony Nash PhD**, University of Oxford, Nuffield Department of Clinical Neurosciences - Design, development, test, research, supervisor.

**Tingyee Chang MSc**, University of Oxford, Nuffield Department of Clinical Neurosciences - Test and research.

**Dr Zameel Cader DPhil**, University of Oxford, Nuffield Department of Clinical Neurosciences - Group lead.


## Funding
We are grateful to the Oxford Science Innovation, NIHR Oxford Biomedical Research Centre and NIHR Oxford Health Biomedical Research Centre (Informatics and Digital Health theme, grant BRC-1215-20005) for funding. The views expressed are those of the authors and not necessarily those of the UK National Health Service, the NIHR, or the UK Department of Health. 

## License
MIT © Anthony Nash
