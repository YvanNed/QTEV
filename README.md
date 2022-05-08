# QTEV

Analysis of the questionnaire duration distance and speed across a train journey:

- Formulaires_csv is the participants' data
- GPS_data is the train data from each journey. They were obtained in an xlsx format (from the sncf) and were saved as csv file (to read them more easily on R) on the CSV directory. CSV_CLEAN contains the train data after preprocessing (OpenAllFill_and_ExctractTrainData) + after removing the part of the journey not needed (from Lyon Part-Dieu to Lyon Perrache (or reversed))  
- OpenAllFill_and_ExctractTrainData.R allows to preprocessed train data: it will interpolate the missing timing informations form distance and speed data
- Identify_speed_episodes.R allows to plot each journey to identify manually the speed episodes
- format_survey.R is for now the way to reformat the data   
