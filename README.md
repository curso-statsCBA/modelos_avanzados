# Modelos Estadísticos Avanzados
Curso de posgrado para el doctorado en Cs. Biológicas de la Universidad Nacional de Córdoba.   

**PÁGINA EN CONSTRUCCIÓN**   
*Disculpe las molestias*   

Este curso se dictará del 11 al 22 de setiembre de 2023 en forma **presencial** el aula del Doctorado en Ciencias Biológicas (FCEFyN, Universidad Nacional de Córdoba), Vélez Sársfield esq. Duarte Quiros, Ciudad de Córdoba.    

El horario de cursado es de 8:30 a 13:30. Se debe asistir con su propia computadora personal y los softwares R y RStudio instalados y en su última versión. Se sugiere descargar previamente los siguientes paquetes: *ape, blmeco, car, caret, class, cluster, ClusterR, emmeans, e1071, fpc, factoextra, gamm4, geodata, ggeffects, ggmap, ggplot2, glmnet, gratia, gstat, ILRS2, lme4, lmtest, lmerTest, MASS, mclust, mgcv, nlme, OpenStreetMap, pbkrtest, pscl, randomForest, RLRsim, ROCR, sp, tree, tidygam, vegan* y *viridis*.   

## Links útiles
### Ayuda general GLMM
https://bbolker.github.io/mixedmodels-misc/glmmFAQ 
### Introducción a modelos mixtos
https://peerj.com/articles/4794/
### Modelos mixtos con correlaciones
https://bbolker.github.io/mixedmodels-misc/notes/corr_braindump.html   
### An Introduction to Statistical Learning (gratis)
https://www.statlearning.com/
### The Elements of Statistical Learning (gratis)
https://hastie.su.domains/ElemStatLearn/   
### Introduction to Generalized Additive Models with R and mgcv
Video de Gavin Simpson
https://www.youtube.com/watch?v=sgw4cu8hrZM&t=8997s


## Cronograma simplificado.   

El curso consta de clases teóricas, ejercicios y laboratorio de análisis de datos grupal.   

### Día 1.   
* Teórico: Contenido de la materia. Los modelos estadísticos como hipótesis estadísticas y el test o selección simultánea de múltiples hipótesis estadísticas empleando la Teoría de la Información. [Link](teoricos/Teor1.pdf)     
* Práctico: Ejercicios de LM y nivelación en R. [Link](TP1/TP1.html)   

### Día 2.   
* Teórico: Componentes de los GLM: predictor lineal de las variables explicativas, función de enlace y errores aleatorios. Modelos lineales generalizados con estructura de errores binomial.[Link 1](teoricos/Teor2.1.pdf) - [Link 2](teoricos/Teor2.2.pdf)      
* Práctico: Ejercicios de distribución binomial. [Link](TP2/practico02.html)     

### Día 3.   
* Teórico: Datos de conteos (Poisson y Binomial Negativa). Sobredispersión de GLM para datos discretos: diagnóstico y soluciones. Modelos de mezcla (mixture models) para datos con exceso de ceros. [Link](teoricos/Teor3.pdf)     
* Práctico: Ejercicios para datos de conteo. [Link](TP3/practico03.html)     

### Día 4.
* Teórico: Definición de efectos fijos y efectos aleatorios y su interpretación biológica. Uso de los Modelos Lineares Generalizados Mixtos (GLMM) para modelar restricciones de aleatorización de unidades muestrales y el diseño experimental. El ajuste de los GLMM por Máximo de Verosimilitud Restringido (REML). Test de efectos y selección de modelos en los GLMM. [Link](teoricos/Teor4.pdf)   
* Práctico: Ejercicios de GLMM. [Link](TP4/practico04.html)    

### Día 5.
* Teórico: Autocorrelación espacial y temporal. Como detectarla y que tipo de modelos aplicar para incluir la correlación en los modelos. [Link](teoricos/Teor5.pdf)  
* Práctico: Ejercicios para datos con autocorrelación. [Link](TP5/Práctico-05.html)    

### Día 6.
* Teórico: Los Modelos Aditivos Generalizados (GAM) como generalizaciones no paramétricas y no lineales de los GLM. Componentes de un GAM: función lineal, estructura de los errores, función de enlace y función de suavizado. Utilidad y limitaciones de los GAM ilustrada con ejemplos. Validación y evaluación de la calidad del ajuste en GAM y GAMM. [Link](teoricos/Teor6.pdf)   
* Práctico: Ejercicios de GAM. [Link](TP6/practico06.html)    

### Día 7.   
* Teórico: Introducción al Machine Learning. Datos de entrenamiento, prueba y validación. Ejemplos en ciencia de la aplicación de este tipo de modelos. Regresión logística/SVM/Random forest. Análisis discriminante. ¿Qué son las redes neuronales? Concepto. [Link](teoricos/Teor7-8.pdf)   
* Práctico: Presentación de un problema para resolver aplicando un modelo de aprendizaje supervisado. [Link](TP7/practico07.html)   

### Día 8.   
* Práctico: Lectura e interpretación de publicaciones científicas.   
* Práctico: Continúa el desarrollo de la parte práctica y presentaciones a cargo de los estudiantes. [Link](TP8/tp8.html)   

### Día 9.  
* Teórico: Aprendizaje No Supervisado. Datos etiquetados vs. no etiquetados. Gaussian admixture, K-medias, Clustering, Dbscan. [Link](teoricos/Teor9.pdf)   
* Práctico: Ejercicios con modelos de aprendizaje no supervisado. [Link](TP4/tp9.html)   

### Día 10.  
Evaluación: presentaciones orales con datos propios, habiendo aplicado alguno de los modelos que vimos durante el desarrollo del curso.   
