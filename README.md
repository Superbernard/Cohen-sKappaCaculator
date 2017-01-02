# Inter-rater-reliability-calculator
A GUI to automatically calculate Inter-rater reliability 

Interrater reliability (IRR) has been widely used to evaluate the degree of agreement between raters in social science research. To date, there has been no single statistical program or package that can automatically calculate interrater reliability starting from data preparation, data analysis to data reporting and visualization. A major challenge of computing IRR is the difficulty in arranging the raw data into the desired format and layout to fit the entry of statistical tools. The data preparation work has been realized to be tedious and time-consuming. Many researchers lamented over this issue. In particular, if a qualitative study involves intensive content analysis or if a longitudinal study requires ongoing data collection, the task would become even more burdensome. 

For the sake of social science researches and practitioners, we developed a Graphical User Interface (GUI) with R to resolve the aforementioned issues. Despite various forms of measurement in computing interrater reliability, we focused primarily on Cohen’s kappa (Cohen, 1960), a well-established measure for capturing raters’ agreement with respect to categorical items or outcomes. Having tested, debugging, and polishing our program, we managed to compute IRR Cohen’s Kappa of complex dataset (i.e., 15 categories and over 10,000 records) in just 2 minutes, which would otherwise be done in hours using traditional database software. The massive amount of time saved is attributable to the unique capability of data arrangement, and simultaneous assessment of multiple categories. And yet, all of the packages available in the market can simply compute the IRR without the capability of arranging the raw data into the desired layout or of handling multiple categories across multiple raters. Admittedly, the present version of the calculator GUI has some limitations, like it requires certain structure for data inputs and can only recognize CSV files, but it might be a promising start for both academia and industry. 

Second, the problems of program breakdown can be caused by a small failure or a tiny fault. Unlike other IRR calculating packages, our IRR Calculator can tolerate some faults. This fault-tolerant design allows out program to work smoothly without the interruption of the faults (e.g., blank rows, extra no code in Knowledge category etc.).  In additional, the flexibility of the IRR Calculator GUI allows us to tweak the coding scheme and to apply our program to any other contexts. 


Cohen’s Kappa: 

Kappa = (Po - Pe) / (1 - Pe) = 1 - (1 - P0 ) / (1 - Pe)

where Po is the relative observed agreement among raters, and Pe is the hypothetical probability of chance agreement, using the observed data to calculate the probabilities of each observer randomly saying each category.




References
Cohen, J. (1960). A Coefficient of Agreement for Nominal Scales. Educational and Psychological Measurement, 20(1).

