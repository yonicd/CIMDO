# CIMDO
## Background
A set of banking stability measures which take account of distress dependence among the banks in a system, thereby providing a set of tools to analyze stability from complementary perspectives by allowing the measurement of 
  - common distress of the banks in a system
  - distress between specific banks, 
  - distress in the system associated with a specific bank

The approach defines the banking system as a portfolio of banks and infers the system's multivariate density (BSMD) from which the proposed measures are estimated. The BSMD embeds the banks' default inter-dependence structure that captures linear and non-linear distress dependencies among the banks in the system, and its changes at different times of the economic cycle. The BSMD is recovered using the CIMDO-approach, that in the presence of restricted data, improves density specification without explicitly imposing parametric forms that, under restricted data sets, are difficult to model. Thus, the proposed measures can be constructed from a very limited set of publicly available data and can be provided for a wide range of both developing and developed countries.

This methodology was dervied by [Segoviano and Goodhart 2009](https://www.imf.org/external/pubs/ft/wp/2009/wp0904.pdf).

## Dashboard
The dashboard reads all outputs by the Financial Stability Measures system and creates an interactive experience to study the model results. The graphs can be in static mode (ggplot) or interactive mode (plotly) which is still in beta. 

The measures currently available to view are:


|                 Stability Measures                  |
|:-------------------------------------:|
|                 Joint Probability of Distress (JPOD)                  |
|                 Banking Stability Index (BSI)                  |
|             Pr(Group 1 \| Group 2 in Default)             |
|         User Definded Groups          |
| Pr(At least 1 defaults\|Security Default) |
| Pr(Exactly 1 defaults\|Security Default)  |
|         Spillover Coefficient         |
|     Spillover Coefficient Change      |
|    Spillover Coefficient Relative     |
|    Spillover Coefficient Absolute     |
|           Pr(system\|security)            |
|           Pr(security\|system)            |
|             Emperical PoD             |
|       Pr(row default\|col default)        |
|          Pr(row and col default)          |




