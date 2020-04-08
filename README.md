# The social and linguistic dynamics of an online community



# Objectives
There is a hypothesis in social science, that people adopt similar patterns of language use when they communicate. 
This project will investigate this concept in an online forum, where participants communicate with each other via conversations online. 
Using the metadata and linguistic summary from a real online forum this project investigates whether the language used changes over time and furthermore whether members who interact directly with each other pick up the similar language which may be different from other members of the forum. For instance, is the proportion of language expressing optimism different between groups, or does it change over time?



# Dataset
The data is contained in the file 'webforum.csv' which can be found via the provided link. 
The dataset consists of the metadata and linguistic analysis of 20,000 made 
by various users in multiple threads on an online forum over the years 2002 to 2011.
The linguistic analysis was conducted using Linguistic
Inquiry and Word Count (LIWC), which assesses the prevalence of certain thoughts, 
feelings and motivations by calculating the proportion of keywords used in communication. 
link for the data-frame:  http://liwc.wpengine.com



# Code Iformation
- Data Cleaning
The dataset was checked for any duplicate PostIDs which would mean that the same record is present
more than once in the dataset but none were found.

- Choosing Subsets for Analysis
The goal here is to find subsets of manageable size from the original dataset as the original dataset is
quite large (20000 posts, 500 threads).

- Data Analysis
Data analysis process is broken down as below:
. Analysis of attributes ‘Authentic’ & ‘Tone’ across all threads in all chosen datasets
. Analysis of threads in largest dataset
. Analysis of threads in small dataset
. Analysis of attributes ‘anx’, ‘anger’, ‘work’, ‘money’ across all threads in all chosen datasets
. Author Analysis
. Time Series Analysis Using Attributes ‘Clout’ & ‘Analytic’
. Statistical Test Using t-test




