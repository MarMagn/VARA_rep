Presentation Socialstyrelsen
========================================================
author: Martin Magnéli
date: 
autosize: true

VARA-studien
========================================================
Målet med VARA-studien var att validera SKLs Vården i siffror och Svenska Höftprotesregistrets instrument för att mäta vårdskador efter höftprotesoperation. Dessa två är baseras på utvalda ICD-koder som registreras i patientregistret under återinläggningar. Vi genomförde retrospektiv journalgranskning på 1998 akuta och elektiva patienter. Vi granskade alla sjukhusvårdtillfällen 90 dagar efter operationen och registrerade alla skador och vårdskador vi hittade med Markörbaserad journalgranskning.
- Vi hittade 2116 skador hos 1171 patienter. 
- 1604 skador hos 975 patienter bedömdes som vårdskador (undvikbara).
- Endast 54% av skadorna hade en korrekt ICD-10 kod.
Metod
==========
- Ett viktat urval användes för att maximera antalet skador
- Kumulativ incidens 28% på 30 dagar och 30% på 90 dagar.
- Justerad incidens rate var 0,43 skador per personmånad.
- Justerad sensitivitet var 6% på 30 dagar och 15% på 90 dagar.
- Justerad specificiter var 95% på 30 dagar och 92% på 90 dagar.


Sensitivitet (90 dagar) för instrumenten baserat på ICD-koder
======================

```r
x <- table(AE_frame$cat_90)
x['TP']/(x['TP']+x['FN'])
```

```
       TP 
0.4798629 
```
**Specificitet**

```r
x['TN']/(x['TN']+x['FP'])
```

```
       TN 
0.8062575 
```

























































```
Error in file(file, "rt") : cannot open the connection
```
