# Automatic Verb Classifier for Abui (AVC-abz)
This repository contains the R code used in the Automatic Verb Classifier for Abui. 
It is a clustering tool allowing to explore the verbal inflectional classes in Abui (Glottolog code: abui1241; Glottolog profile: https://glottolog.org/resource/languoid/id/abui1241). The system uses a manually created learning set with inflectional profiles Abui verbs as data for a k-means clustering algorithm. The clustering is subsequently compared with the corpus data, harvested using the Matrix ODIN Morphology (MOM) tool, as described in Zamaraeva et al. 2017. 

The implemented system is available at: http://gogo.utia.cas.cz/abui/.

References:

Frantisek Kratochvil, George Saad, Jiří Vomlel, and Václav Kratochvíl. 2022. Automatic Verb Classifier for Abui (AVC-abz). In Proceedings of the Workshop on Resources and Technologies for Indigenous, Endangered and Lesser-resourced Languages in Eurasia within the 13th Language Resources and Evaluation Conference, pages 42–50, Marseille, France. European Language Resources Association. URL: https://aclanthology.org/2022.eurali-1.7/

Olga Zamaraeva, František Kratochvíl, Emily M. Bender, Fei Xia, and Kristen Howell. 2017. Computational Support for Finding Word Classes: A Case Study of Abui. In Proceedings of ComputEL-2: 2nd Workshop on Computational Methods for Endangered Languages, Honolulu, Hawaii, March 6-7, 2017. Association for Computational Linguistics (ACL).
