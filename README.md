# Auto-STEED

Systematic reviews, i.e., research summaries that address focused questions in a structured and reproducible manner, are a cornerstone of evidence-based medicine and research. However, certain systematic review steps such as data extraction are labour-intensive which hampers their applicability, not least with the rapidly expanding body of biomedical literature.

To bridge this gap, we developped a data mining tool in R environment to automate data extraction from neuroscience in vivo publications. The function was
trained on a literature corpus (n=45 publications) of animal motor neuron disease studies and tested in two validation corpora (motor neuron diseases, n=31 publications; multiple sclerosis, n=244 publications).

Our data mining tool Auto-STEED (Automated and STructured Extraction of Experimental Data) was able to extract key experimental parameters such as animal models and species as well as risk of bias items such as randomization or blinding from in vivo studies. Sensitivity and specificity were over 85 and 80%, respectively, for most items in both validation corpora. Accuracy and F-scores were above 90% and 0.9 for most items in the validation corpora. Time savings were above 99%.

Our developed text mining tool Auto-STEED is able to extract key experimental parameters and risk of bias items from the neuroscience in vivo literature. With this, our tool can be deployed to probe certain field in a research improvement context or to replace one human reader during data extraction resulting in substantial time-savings. Our tool can contribute towards automation of systematic reviews.

The preprint of the manuscript is available at: https://osf.io/preprints/metaarxiv/xkrpv/
