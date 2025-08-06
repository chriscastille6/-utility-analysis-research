# Carson et al. (1998) - Revised Validity-Only Condition
# Source: Carson, K. P., Becker, J. S., & Henderson, J. A. (1998). Is utility really futile? A failure to replicate and an extension. Journal of Applied Psychology, 83(1), 84–96.

# Word Count: 485 words
# Study: Study 1 only (new condition)

revised_validity_only <- "
Imagine you are the vice president of human resources in a large American based multi-national company. This corporation employs over 10,000 people and had sales of over $4 billion dollars last year. The corporation has a reputation for high quality products. Recently concerns have been raised about the quality of clerical/administrative personnel.

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

As vice president of the company, it is up to you to decide whether to implement the consultant's recommendations.
"

# Key characteristics:
# - Includes explicit validity coefficients (.40 expected, .20 for current interviews)
# - Clearer language and better structure
# - No utility analysis
# - 485 words (120 words longer than original)
# - Used in Study 1 only

cat("Revised Validity-Only Condition loaded.\n")
cat("Word count:", length(strsplit(revised_validity_only, "\\s+")[[1]]), "\n") 