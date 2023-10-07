# public_transport_COVID_Lombardy

Code developed for the paper **The Impact of Public Transport on the Diffusion of the COVID-19 Pandemic in Lombardy during 2020**, authored by Greta Galliani, Piercesare Secchi, and Francesca Ieva, published in Medical Research Archives in September 2023.

## Overview

This repository contains the code and resources associated with the research paper titled **The Impact of Public Transport on the Diffusion of the COVID-19 Pandemic in Lombardy during 2020** authored by Greta Galliani, Piercesare Secchi, and Francesca Ieva, published in Medical Research Archives in September 2023.

## Data and Resources

Due to the substantial size of the datasets required for the analyses, some datasets must be downloaded and placed in specific locations as indicated in the following table:

| Description | Link | Location |
|-------------|------|----------|
| 2020 OD matrix released by Regione Lombardia | [Link](https://www.dati.lombardia.it/Mobilit-e-trasporti/Matrice-OD2020-Passeggeri/hyqr-mpe2/data) | Data/RegioneLombardia/matriceOD_lombardia_2020.csv |
| Data about the population of every Italian municipality as of Jan 1, 2020 | [Link](http://dati.istat.it/Index.aspx?QueryId=19101) | Data/ISTAT/Population/comuni_popolazione_2020.csv |
| Data about daily deaths from Jan 1, 2011 to Mar 31, 2023, for every age class and Italian municipality | [Link](https://www.istat.it/it/archivio/240401) | Data/ISTAT/Mortality/comuni_giornaliero_31marzo23.csv |
| Data about road distances from every municipality in Lombardia to every other Italian municipality | [Link](https://www.istat.it/storage/cartografia/matrici_distanze/Lombardia.zip) | Data/ISTAT/Distances_comuni/Lombardia_distances.txt |
| Data about road distances from every municipality in Veneto to every other Italian municipality | [Link](https://www.istat.it/storage/cartografia/matrici_distanze/Veneto.zip) | Data/ISTAT/Distances_comuni/Veneto_distances.txt |

The Trenord data in the folder `Data/Trenord` are simulated and do not represent actual train movements that occurred in 2020 due to confidentiality constraints. They serve as examples to demonstrate how the code produces spatial analysis results.

## Code Details

The code included in this repository explores two age classes: 70+ (as presented in the paper) and 50-69 (not discussed in the paper).

## Citation

If you use this code or data in your work, please consider citing the original paper authored by Greta Galliani, Piercesare Secchi, and Francesca Ieva as: 

Ieva, Francesca; Galliani, Greta; Secchi, Piercesare. The impact of public transport on the diffusion of COVID-19 pandemic in Lombardy during 2020. Medical Research Archives, [S.l.], v. 11, n. 9, sep. 2023. ISSN 2375-1924. Available at: <https://esmed.org/MRA/mra/article/view/4356>. doi: https://doi.org/10.18103/mra.v11i9.4356

For further details about the research and its findings, please refer to the associated paper.



