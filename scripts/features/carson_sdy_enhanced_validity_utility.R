# Carson et al. (1998) - SDy-Enhanced Revised Validity + Utility Condition
# Source: Carson, K. P., Becker, J. S., & Henderson, J. A. (1998). Is utility really futile? A failure to replicate and an extension. Journal of Applied Psychology, 83(1), 84–96.

# Word Count: ~800+ words (includes additional SDy explanation)
# Study: Study 2 only (new condition)

sdy_enhanced_validity_utility <- "
Imagine that you are a vice president of a large American based multi-national corporation. The company has over 10,000 employees, and last year's sales exceeded $4 billion dollars. The company is highly regarded for the quality of it's products. Lately, however, some people within the organization have raised concerns about the quality of the clerical/administrative personnel that the firm has been hiring.

Potential new hires are interviewed before a decision is made to extend an offer, but at present no systematic procedures exist by which clerical/administrative employees are selected. Although you are skeptical that the methods by which your firm selects such personnel can be improved upon, an organizational consultant has been retained to investigate the issue.

This consultant specializes in the development and validation of selection practices that attempt to enable companies to select high performers in a legally defensible manner. She is also a member of the American Psychological Association, and graduated ten years ago with a Ph.D. from a prestigious university.

This particular consultant was contacted because she has done research showing that employee performance can be predicted through the use of selection testing. After a discussion of the issues, the consultant submits a preliminary proposal. This is a summary of her proposal:

Interviews like the ones your company uses have traditionally had lower levels of validity than test based selection systems. Validity is simply an expression of the relationship between something used to predict job performance and actual job performance. A validity coefficient of zero means there is no relationship; a coefficient of 1.00 means there is a perfect relationship. It is very rare for validity coefficients to exceed .50 in a selection context. The higher the validity coefficient, the more able we are to accurately predict who will be successful on the job. The average validity for the type of interview your company is using is approximately .20, whereas a test designed for another client yielded a coefficient of .40, which is consistent with research evidence for these types of tests.

For your company, a validity study consisting of the following steps will be performed:

1. Develop a test to measure employee performance that is tailor-made to your firm;
2. Administer this test across all relevant categories of clerical/administrative personnel;
3. Correlate the results of the test with performance on the job to see how well the test predicts performance;
4. Use the results of this test to alter existing selection practices so as to improve employee performance in the clerical/administrative category.

The cost of designing and validating the selection test will be $6,100. Because you expect to hire about 470 new clerical/administrative employees this year from a large pool of applicants we estimated that the total implementation cost of the selection program will be $423,000. This amount reflects recruiting costs, test administrators' salary, computer test scoring, and outlays for test booklets. Total costs are approximately $429,100.

These costs should be evaluated in the context of the return the company can expect to receive. If the validity study which we are proposing does in fact indicate that the new test has a validity coefficient of about .40 (which is expected based on past experience and research), this would mean that more productive employees are being selected than is currently the case. More productive employees are obviously beneficial since they produce more for the company for the same labor costs as their less productive counterparts. Utility analysis is the process of estimating the benefit to the company of selecting better, more productive, employees.

A utility analysis was conducted based on the expected validity of the new test, the number of applicants for the jobs, the cost estimates of administering the new selection procedure, and the information gained from supervisors in your company about the value of more productive employees in this particular job category. The value of more productive employees was obtained utilizing the Schmidt-Hunter global estimation procedure. This method is based on the following reasoning: If job performance in dollar terms is distributed normally, then the difference between the value to the organization of the products and services produced by the average (50th percentile) employee and those produced by an employee at the 85th percentile in performance is equal to SDy. To facilitate these judgments, raters are told to imagine how much the goods and services would cost if provided by an outside consulting firm. The magnitude of SDy then is inferred from the difference between these two estimates.

This analysis indicated that the average clerical/administrative employee will be \"worth\" $7,117 more to the organization than the average employee selected under the current system because the better employees hired based on the new procedure will learn their jobs more quickly and continue to improve as new policies and technology are put in place. Since the company expects to hire 470 employees in this job category over the course of the year, this savings equals $3,344,933 (470*7117) per year to the company. Further, since employees in this job category stay with the company for an average of 18 years, the total benefit to the company over the life span of the employees selected this year is estimated to be $60,208,786 (18*3,344,933). In sum, the utility analysis shows that improved selection has an impressive economic benefit to the company.

As vice president of the company, it is up to you to decide whether to implement the consultant's recommendations.
"

# Key characteristics:
# - Identical to Revised Validity + Utility condition PLUS:
# - Additional 12-line explanation of SDy calculation method
# - Uses Schmidt-Hunter global estimation procedure
# - Explains the reasoning behind SDy estimation
# - ~800+ words (slightly longer than revised condition)
# - Used in Study 2 only
# - No significant difference from Revised Validity + Utility condition

cat("SDy-Enhanced Revised Validity + Utility Condition loaded.\n")
cat("Word count:", length(strsplit(sdy_enhanced_validity_utility, "\\s+")[[1]]), "\n") 