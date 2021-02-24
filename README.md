# Shaping_Translation
UPDATE 24.02.21: I am working on many things at the same time, so I have decided to upload my code in batches in order not to delay it too much. For now, roughly half of my code for the Revista Azul is available. The plots relative to the signature type, its combination with the other variables, and the explorations of aspects not mentioned in my article will follow shortly. The code is the same for the Revista Moderna (except plot numbers), so feel free to use it with the dataRM dataset, but bear in mind that you will have the wrong labels. Thanks for your patience!


Welcome to the repo containing the complementary material for my article titled "Shaping translation in two Mexican cultural magazines:  a case study in the use of quantitative methods for the analysis of translation in periodical publications."


Here, you will find supplementary information about the datasets, in-depth comments on some specific points and issues, and a more comprehensive exploration of the variables retained for the article. 
This includes the graphs that did not make it into the article due to space limitations. If you are unfamiliar with R or unable to run the code for whatever reasons, Figures.pdf contains all the figures, as well as the figures index and an explanation of the numbering system.




Datasets information:

dataRA and dataRM contain the data that is necessary to the analysis carried out in the article. My whole database will be published at a later stage. Please note that these datasets might change slightly in time as new translations are identified. As most translations are not indicated in the magazines, they can be challenging to spot. I surely got the vast majority of them, but it is not impossible that I will find a few more next time I go through this material. This should however be a matter of details and not change the overall resutls of my reseach.


The translations that were included in the datasets correspond to texts where it can be affirmed with a fair degree of certainty that they were originally written in a different language. The translation itself does not necessarily have to have been done especially for this publication, sometimes it is simply reprinted from another periodical.
Dubious cases (for instance when the original author is unclear) have been left out for the time being. 

Issue 49 (II. 23) of the *Revista Azul* is problematic as 4 pages are missing from the digitised version on the Hemeroteca Nacional Digital website (pp. 359-361 of the second volume). Being unable to access a hard copy at the moment, I have roughly recretaed the contents from Díaz Alejo and Prado Velazquez's *Indice de la Revista Azul* and have determined that those pages contain three transaltions (Tolstoy, Byron and de Amicis). I have included those in the dataset, however, the corresponding data is incomplete, as I could not see the actual contents of those pages. The starting page is indicated in the *Indice*, but the length had to be estimated (where two texts share a page, they have been considered as two equal halves of the page for now; the last 1.25 page of de Amicis's text is visible, so I added it to its first hypothetical 0.5 page (the end of the translation is on pp. 363-364). The signature type was left blank as there is no way to infer it (although I would expect both Byron's and Tolstoy's to be anonymous unidentified, as is the case of all other translations from those two authors). This data will be added in as soon as I can consult a copy of the *Revista Azul*.

