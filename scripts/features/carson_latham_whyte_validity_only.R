# Carson et al. (1998) - Latham and Whyte's Validity-Only Condition
# Source: Carson, K. P., Becker, J. S., & Henderson, J. A. (1998). Is utility really futile? A failure to replicate and an extension. Journal of Applied Psychology, 83(1), 84–96.

# Word Count: 365 words
# Study: Replication of Latham & Whyte (1994) original condition

latham_whyte_validity_only <- "
Imagine that you are a vice president of a large American based multi-national corporation. The company has over 10,000 employees, and last year's sales exceeded $4 billion dollars. The company is highly regarded for the quality of it's products. Lately, however, some people within the organization have raised concerns about the quality of the clerical/administrative personnel that the firm has been hiring.

Potential new hires are interviewed before a decision is made to extend an offer, but at present no systematic procedures exist by which clerical/administrative employees are selected. Although you are skeptical that the methods by which your firm selects such personnel can be improved upon, an organizational consultant has been retained to investigate the issue.

This consultant specializes in the development and validation of selection practices that attempt to enable companies to select high performers in a legally defensible manner. She is also a member of the American Psychological Association, and graduated ten years ago with a Ph.D. from a prestigious university.

This particular consultant was contacted because she has done research showing that employee performance can be predicted through the use of selection testing. After a discussion of the issues, the consultant has recommended that the following steps be taken:

1. Develop a test to measure employee performance that is tailor-made to your firm;
2. Administer this test across all relevant categories of clerical/administrative personnel;
3. Correlate the results of the test with performance on the job to see how well the test predicts performance;
4. Use the results of this test to alter existing selection practices so as to improve employee performance in the clerical/administrative category.

The consultant has stated that the cost of designing and validating the selection test will be $6,100. Because you expect to hire about 470 new clerical/administrative employees this year from a large pool of applicants it is estimated that the total implementation cost of the selection program will be $423,000. This amount reflects recruiting costs, test administrators' salary, computer test scoring, and outlays for test booklets. Total costs of the consultant's recommendations, if followed, are therefore approximately $429,100.

As vice president of the company, it is up to you to decide whether to implement the consultant's recommendations.
"

# Key characteristics:
# - No quantitative validity information
# - No utility analysis
# - Basic scenario with costs only
# - 365 words
# - Used in both Study 1 and Study 2 as replication condition

cat("Latham and Whyte's Validity-Only Condition loaded.\n")
cat("Word count:", length(strsplit(latham_whyte_validity_only, "\\s+")[[1]]), "\n") 