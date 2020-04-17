# > boruta_stock_train <- Boruta(Class~., data =read_file[-c(1,2,227)], doTrace = 3)
# 1. run of importance source...
# Computing permutation importance.. Progress: 36%. Estimated remaining time: 54 seconds.
# Computing permutation importance.. Progress: 70%. Estimated remaining time: 26 seconds.
# Assigned hit to 151 attributes out of 223 undecided.
# 2. run of importance source...
# Computing permutation importance.. Progress: 41%. Estimated remaining time: 43 seconds.
# Computing permutation importance.. Progress: 80%. Estimated remaining time: 15 seconds.
# Assigned hit to 78 attributes out of 223 undecided.
# 3. run of importance source...
# Computing permutation importance.. Progress: 41%. Estimated remaining time: 45 seconds.
# Computing permutation importance.. Progress: 80%. Estimated remaining time: 15 seconds.
# Assigned hit to 83 attributes out of 223 undecided.
# 4. run of importance source...
# Computing permutation importance.. Progress: 38%. Estimated remaining time: 50 seconds.
# Computing permutation importance.. Progress: 77%. Estimated remaining time: 18 seconds.
# Assigned hit to 132 attributes out of 223 undecided.
# 5. run of importance source...
# Computing permutation importance.. Progress: 40%. Estimated remaining time: 46 seconds.
# Computing permutation importance.. Progress: 80%. Estimated remaining time: 15 seconds.
# Assigned hit to 89 attributes out of 223 undecided.
# 6. run of importance source...
# Computing permutation importance.. Progress: 39%. Estimated remaining time: 48 seconds.
# Computing permutation importance.. Progress: 77%. Estimated remaining time: 18 seconds.
# Assigned hit to 105 attributes out of 223 undecided.
# 7. run of importance source...
# Computing permutation importance.. Progress: 38%. Estimated remaining time: 49 seconds.
# Computing permutation importance.. Progress: 77%. Estimated remaining time: 18 seconds.
# Assigned hit to 105 attributes out of 223 undecided.
# 8. run of importance source...
# Computing permutation importance.. Progress: 41%. Estimated remaining time: 45 seconds.
# Computing permutation importance.. Progress: 78%. Estimated remaining time: 17 seconds.
# Assigned hit to 122 attributes out of 223 undecided.
# 9. run of importance source...
# Computing permutation importance.. Progress: 39%. Estimated remaining time: 48 seconds.
# Computing permutation importance.. Progress: 80%. Estimated remaining time: 15 seconds.
# Assigned hit to 70 attributes out of 223 undecided.
# 10. run of importance source...
# Computing permutation importance.. Progress: 38%. Estimated remaining time: 50 seconds.
# Computing permutation importance.. Progress: 77%. Estimated remaining time: 18 seconds.
# Assigned hit to 115 attributes out of 223 undecided.
# 11. run of importance source...
# Computing permutation importance.. Progress: 39%. Estimated remaining time: 48 seconds.
# Computing permutation importance.. Progress: 79%. Estimated remaining time: 16 seconds.
# Assigned hit to 64 attributes out of 223 undecided.
# 12. run of importance source...
# Computing permutation importance.. Progress: 38%. Estimated remaining time: 50 seconds.
# Computing permutation importance.. Progress: 78%. Estimated remaining time: 17 seconds.
# Assigned hit to 146 attributes out of 223 undecided.
# 13. run of importance source...
# Computing permutation importance.. Progress: 38%. Estimated remaining time: 50 seconds.
# Computing permutation importance.. Progress: 77%. Estimated remaining time: 18 seconds.
# Assigned hit to 114 attributes out of 223 undecided.
# 14. run of importance source...
# Computing permutation importance.. Progress: 38%. Estimated remaining time: 50 seconds.
# Computing permutation importance.. Progress: 76%. Estimated remaining time: 19 seconds.
# Assigned hit to 115 attributes out of 223 undecided.
# 15. run of importance source...
# Computing permutation importance.. Progress: 37%. Estimated remaining time: 52 seconds.
# Computing permutation importance.. Progress: 76%. Estimated remaining time: 19 seconds.
# Assigned hit to 69 attributes out of 223 undecided.
# After 15 iterations, +26 mins: 
#   confirmed 11 attributes: Earnings_Yield, Effect_of_forex_changes_on_cash, EV_to_Sales, Free_Cash_Flow_per_Share, Gross_Profit_Growth and 6 more;
# rejected 10 attributes: cashConversionCycle, Net_Cash.Marketcap, Net_Income_._Discontinued_ops, Net_Income_._Non.Controlling_int, Preferred_Dividends and 5 more;
# still have 202 attributes left.
# 
# 16. run of importance source...
# Computing permutation importance.. Progress: 39%. Estimated remaining time: 48 seconds.
# Computing permutation importance.. Progress: 78%. Estimated remaining time: 17 seconds.
# Assigned hit to 108 attributes out of 202 undecided.
# 17. run of importance source...
# Computing permutation importance.. Progress: 41%. Estimated remaining time: 44 seconds.
# Computing permutation importance.. Progress: 81%. Estimated remaining time: 14 seconds.
# Assigned hit to 130 attributes out of 202 undecided.
# 18. run of importance source...
# Computing permutation importance.. Progress: 40%. Estimated remaining time: 47 seconds.
# Computing permutation importance.. Progress: 83%. Estimated remaining time: 13 seconds.
# Assigned hit to 104 attributes out of 202 undecided.
# 19. run of importance source...
# Computing permutation importance.. Progress: 40%. Estimated remaining time: 45 seconds.
# Computing permutation importance.. Progress: 81%. Estimated remaining time: 14 seconds.
# Assigned hit to 39 attributes out of 202 undecided.
# After 19 iterations, +32 mins: 
#   confirmed 4 attributes: assetTurnover, ebtperEBIT, Revenue_Growth, Weighted_Average_Shares_Growth;
# rejected 5 attributes: Investing_Cash_flow, Issuance_.repayment._of_debt, operatingCycle, operatingProfitMargin, X10Y_Net_Income_Growth_.per_Share.;
# still have 193 attributes left.
# 
# 20. run of importance source...
# Computing permutation importance.. Progress: 42%. Estimated remaining time: 42 seconds.
# Computing permutation importance.. Progress: 85%. Estimated remaining time: 10 seconds.
# Assigned hit to 75 attributes out of 193 undecided.
# 21. run of importance source...
# Computing permutation importance.. Progress: 44%. Estimated remaining time: 39 seconds.
# Computing permutation importance.. Progress: 86%. Estimated remaining time: 10 seconds.
# Assigned hit to 116 attributes out of 193 undecided.
# 22. run of importance source...
# Computing permutation importance.. Progress: 43%. Estimated remaining time: 40 seconds.
# Computing permutation importance.. Progress: 88%. Estimated remaining time: 8 seconds.
# Assigned hit to 102 attributes out of 193 undecided.
# 23. run of importance source...
# Computing permutation importance.. Progress: 44%. Estimated remaining time: 39 seconds.
# Computing permutation importance.. Progress: 88%. Estimated remaining time: 8 seconds.
# Assigned hit to 95 attributes out of 193 undecided.
# After 23 iterations, +39 mins: 
#   confirmed 8 attributes: effectiveTaxRate, enterpriseValueMultiple, EV_to_Free_cash_flow, EV_to_Operating_cash_flow, Free_Cash_Flow_Yield and 3 more;
# rejected 10 attributes: Acquisitions_and_disposals, Book_Value_per_Share, Capital_Expenditure, Long.term_debt, Net_cash_flow_._Change_in_cash and 5 more;
# still have 175 attributes left.
# 
# 24. run of importance source...
# Computing permutation importance.. Progress: 46%. Estimated remaining time: 36 seconds.
# Computing permutation importance.. Progress: 90%. Estimated remaining time: 7 seconds.
# Assigned hit to 84 attributes out of 175 undecided.
# 25. run of importance source...
# Computing permutation importance.. Progress: 46%. Estimated remaining time: 36 seconds.
# Computing permutation importance.. Progress: 93%. Estimated remaining time: 4 seconds.
# Assigned hit to 97 attributes out of 175 undecided.
# 26. run of importance source...
# Computing permutation importance.. Progress: 46%. Estimated remaining time: 36 seconds.
# Computing permutation importance.. Progress: 91%. Estimated remaining time: 5 seconds.
# Assigned hit to 63 attributes out of 175 undecided.
# After 26 iterations, +43 mins: 
#   confirmed 6 attributes: Consolidated_Income, Enterprise_Value_over_EBITDA, grossProfitMargin, priceToOperatingCashFlowsRatio, Total_non.current_assets and 1 more;
# rejected 6 attributes: debtEquityRatio, Goodwill_and_Intangible_Assets, Property._Plant_._Equipment_Net, Revenue, Stock.based_compensation and 1 more;
# still have 163 attributes left.
# 
# 27. run of importance source...
# Computing permutation importance.. Progress: 41%. Estimated remaining time: 43 seconds.
# Computing permutation importance.. Progress: 86%. Estimated remaining time: 10 seconds.
# Assigned hit to 95 attributes out of 163 undecided.
# 28. run of importance source...
# Computing permutation importance.. Progress: 45%. Estimated remaining time: 37 seconds.
# Computing permutation importance.. Progress: 91%. Estimated remaining time: 6 seconds.
# Assigned hit to 93 attributes out of 163 undecided.
# 29. run of importance source...
# Computing permutation importance.. Progress: 49%. Estimated remaining time: 32 seconds.
# Computing permutation importance.. Progress: 95%. Estimated remaining time: 3 seconds.
# Assigned hit to 77 attributes out of 163 undecided.
# 30. run of importance source...
# Computing permutation importance.. Progress: 47%. Estimated remaining time: 34 seconds.
# Computing permutation importance.. Progress: 98%. Estimated remaining time: 1 seconds.
# Assigned hit to 87 attributes out of 163 undecided.
# After 30 iterations, +49 mins: 
#   confirmed 4 attributes: Current_ratio, eBTperEBIT, freeCashFlowPerShare, Receivables_Turnover;
# rejected 5 attributes: Dividends_per_Share_Growth, Other_comprehensive_income, Shareholders_Equity_per_Share, X10Y_Shareholders_Equity_Growth_.per_Share., X5Y_Dividend_per_Share_Growth_.per_Share.;
# still have 154 attributes left.
# 
# 31. run of importance source...
# Computing permutation importance.. Progress: 49%. Estimated remaining time: 32 seconds.
# Computing permutation importance.. Progress: 100%. Estimated remaining time: 0 seconds.
# Assigned hit to 70 attributes out of 154 undecided.
# 32. run of importance source...
# Computing permutation importance.. Progress: 52%. Estimated remaining time: 29 seconds.
# Computing permutation importance.. Progress: 100%. Estimated remaining time: 0 seconds.
# Assigned hit to 66 attributes out of 154 undecided.
# 33. run of importance source...
# Computing permutation importance.. Progress: 51%. Estimated remaining time: 30 seconds.
# Assigned hit to 67 attributes out of 154 undecided.
# After 33 iterations, +53 mins: 
#   confirmed 4 attributes: Graham_Number, Gross_Margin, Net_Income, Sector;
# rejected 7 attributes: daysOfSalesOutstanding, Debt_Growth, Interest_Debt_per_Share, Net_Current_Asset_Value, Net_Debt and 2 more;
# still have 143 attributes left.
# 
# 34. run of importance source...
# Computing permutation importance.. Progress: 51%. Estimated remaining time: 30 seconds.
# Computing permutation importance.. Progress: 100%. Estimated remaining time: 0 seconds.
# Assigned hit to 71 attributes out of 143 undecided.
# 35. run of importance source...
# Computing permutation importance.. Progress: 51%. Estimated remaining time: 29 seconds.
# Assigned hit to 63 attributes out of 143 undecided.
# 36. run of importance source...
# Computing permutation importance.. Progress: 52%. Estimated remaining time: 28 seconds.
# Assigned hit to 107 attributes out of 143 undecided.
# After 36 iterations, +57 mins: 
#   confirmed 3 attributes: EPS_Diluted_Growth, PB_ratio, priceBookValueRatio;
# rejected 5 attributes: Deferred_revenue, Investment_purchases_and_sales, Payout_Ratio, R.D_to_Revenue, Total_shareholders_equity;
# still have 135 attributes left.
# 
# 37. run of importance source...
# Computing permutation importance.. Progress: 54%. Estimated remaining time: 25 seconds.
# Assigned hit to 88 attributes out of 135 undecided.
# 38. run of importance source...
# Computing permutation importance.. Progress: 57%. Estimated remaining time: 23 seconds.
# Assigned hit to 86 attributes out of 135 undecided.
# 39. run of importance source...
# Computing permutation importance.. Progress: 54%. Estimated remaining time: 26 seconds.
# Assigned hit to 58 attributes out of 135 undecided.
# After 39 iterations, +1 hours: 
#   confirmed 1 attribute: EPS_Growth;
# rejected 2 attributes: Days_of_Inventory_on_Hand, Long.term_investments;
# still have 132 attributes left.
# 
# 40. run of importance source...
# Computing permutation importance.. Progress: 55%. Estimated remaining time: 25 seconds.
# Assigned hit to 88 attributes out of 132 undecided.
# 41. run of importance source...
# Computing permutation importance.. Progress: 56%. Estimated remaining time: 24 seconds.
# Assigned hit to 79 attributes out of 132 undecided.
# 42. run of importance source...
# Computing permutation importance.. Progress: 52%. Estimated remaining time: 28 seconds.
# Assigned hit to 79 attributes out of 132 undecided.
# After 42 iterations, +1.1 hours: 
#   confirmed 3 attributes: dividendpaidAndCapexCoverageRatios, Net_Income_per_Share, operatingCashFlowPerShare;
# rejected 1 attribute: Weighted_Average_Shs_Out;
# still have 128 attributes left.
# 
# 43. run of importance source...
# Computing permutation importance.. Progress: 56%. Estimated remaining time: 24 seconds.
# Assigned hit to 82 attributes out of 128 undecided.
# 44. run of importance source...
# Computing permutation importance.. Progress: 54%. Estimated remaining time: 26 seconds.
# Assigned hit to 61 attributes out of 128 undecided.
# 45. run of importance source...
# Computing permutation importance.. Progress: 57%. Estimated remaining time: 23 seconds.
# Assigned hit to 56 attributes out of 128 undecided.
# After 45 iterations, +1.1 hours: 
#   confirmed 3 attributes: Earnings_before_Tax, Net_Profit_Margin, priceToBookRatio;
# rejected 1 attribute: Depreciation_._Amortization;
# still have 124 attributes left.
# 
# 46. run of importance source...
# Computing permutation importance.. Progress: 58%. Estimated remaining time: 22 seconds.
# Assigned hit to 87 attributes out of 124 undecided.
# 47. run of importance source...
# Computing permutation importance.. Progress: 59%. Estimated remaining time: 21 seconds.
# Assigned hit to 75 attributes out of 124 undecided.
# 48. run of importance source...
# Computing permutation importance.. Progress: 55%. Estimated remaining time: 25 seconds.
# Assigned hit to 88 attributes out of 124 undecided.
# After 48 iterations, +1.2 hours: 
#   confirmed 6 attributes: daysOfInventoryOutstanding, Dividend_payments, EBIT_Margin, EBITDA, netProfitMargin and 1 more;
# still have 118 attributes left.
# 
# 49. run of importance source...
# Computing permutation importance.. Progress: 58%. Estimated remaining time: 22 seconds.
# Assigned hit to 65 attributes out of 118 undecided.
# 50. run of importance source...
# Computing permutation importance.. Progress: 57%. Estimated remaining time: 23 seconds.
# Assigned hit to 71 attributes out of 118 undecided.
# 51. run of importance source...
# Computing permutation importance.. Progress: 57%. Estimated remaining time: 23 seconds.
# Assigned hit to 79 attributes out of 118 undecided.
# After 51 iterations, +1.3 hours: 
#   confirmed 4 attributes: Earnings_Before_Tax_Margin, EBIT, operatingCashFlowSalesRatio, Profit_Margin;
# rejected 2 attributes: interestCoverage, X3Y_Operating_CF_Growth_.per_Share.;
# still have 112 attributes left.
# 
# 52. run of importance source...
# Computing permutation importance.. Progress: 58%. Estimated remaining time: 22 seconds.
# Assigned hit to 69 attributes out of 112 undecided.
# 53. run of importance source...
# Computing permutation importance.. Progress: 58%. Estimated remaining time: 22 seconds.
# Assigned hit to 82 attributes out of 112 undecided.
# After 53 iterations, +1.3 hours: 
#   confirmed 3 attributes: capitalExpenditureCoverageRatios, Free_Cash_Flow, PTB_ratio;
# rejected 1 attribute: Receivables;
# still have 108 attributes left.
# 
# 54. run of importance source...
# Computing permutation importance.. Progress: 56%. Estimated remaining time: 24 seconds.
# Assigned hit to 32 attributes out of 108 undecided.
# 55. run of importance source...
# Computing permutation importance.. Progress: 56%. Estimated remaining time: 24 seconds.
# Assigned hit to 80 attributes out of 108 undecided.
# 56. run of importance source...
# Computing permutation importance.. Progress: 58%. Estimated remaining time: 22 seconds.
# Assigned hit to 25 attributes out of 108 undecided.
# After 56 iterations, +1.4 hours: 
#   confirmed 3 attributes: Days_Sales_Outstanding, Operating_Income, pretaxProfitMargin;
# rejected 2 attributes: Free_Cash_Flow_growth, Invested_Capital;
# still have 103 attributes left.
# 
# 57. run of importance source...
# Computing permutation importance.. Progress: 59%. Estimated remaining time: 21 seconds.
# Assigned hit to 36 attributes out of 103 undecided.
# 58. run of importance source...
# Computing permutation importance.. Progress: 54%. Estimated remaining time: 26 seconds.
# Assigned hit to 28 attributes out of 103 undecided.
# 59. run of importance source...
# Computing permutation importance.. Progress: 55%. Estimated remaining time: 25 seconds.
# Assigned hit to 20 attributes out of 103 undecided.
# After 59 iterations, +1.4 hours: 
#   confirmed 1 attribute: Retained_earnings_.deficit.;
# rejected 2 attributes: dividendPayoutRatio, Tangible_Asset_Value;
# still have 100 attributes left.
# 
# 60. run of importance source...
# Computing permutation importance.. Progress: 61%. Estimated remaining time: 19 seconds.
# Assigned hit to 34 attributes out of 100 undecided.
# 61. run of importance source...
# Computing permutation importance.. Progress: 60%. Estimated remaining time: 20 seconds.
# Assigned hit to 56 attributes out of 100 undecided.
# After 61 iterations, +1.5 hours: 
#   rejected 3 attributes: Debt_to_Equity, payoutRatio, Tax_Liabilities;
# still have 97 attributes left.
# 
# 62. run of importance source...
# Computing permutation importance.. Progress: 63%. Estimated remaining time: 17 seconds.
# Assigned hit to 30 attributes out of 97 undecided.
# 63. run of importance source...
# Computing permutation importance.. Progress: 60%. Estimated remaining time: 20 seconds.
# Assigned hit to 66 attributes out of 97 undecided.
# 64. run of importance source...
# Computing permutation importance.. Progress: 64%. Estimated remaining time: 17 seconds.
# Assigned hit to 49 attributes out of 97 undecided.
# After 64 iterations, +1.5 hours: 
#   confirmed 3 attributes: cashRatio, eBITperRevenue, Free_Cash_Flow_margin;
# rejected 3 attributes: Interest_Coverage, Net_Debt_to_EBITDA, Tangible_Book_Value_per_Share;
# still have 91 attributes left.
# 
# 65. run of importance source...
# Computing permutation importance.. Progress: 62%. Estimated remaining time: 19 seconds.
# Assigned hit to 62 attributes out of 91 undecided.
# 66. run of importance source...
# Computing permutation importance.. Progress: 63%. Estimated remaining time: 18 seconds.
# Assigned hit to 49 attributes out of 91 undecided.
# 67. run of importance source...
# Computing permutation importance.. Progress: 64%. Estimated remaining time: 17 seconds.
# Assigned hit to 48 attributes out of 91 undecided.
# After 67 iterations, +1.6 hours: 
#   rejected 2 attributes: Stock.based_compensation_to_Revenue, X5Y_Revenue_Growth_.per_Share.;
# still have 89 attributes left.
# 
# 68. run of importance source...
# Computing permutation importance.. Progress: 64%. Estimated remaining time: 17 seconds.
# Assigned hit to 63 attributes out of 89 undecided.
# 69. run of importance source...
# Computing permutation importance.. Progress: 64%. Estimated remaining time: 17 seconds.
# Assigned hit to 66 attributes out of 89 undecided.
# After 69 iterations, +1.6 hours: 
#   confirmed 3 attributes: Income_Quality, Price_to_Sales_Ratio, SG.A_Expenses_Growth;
# still have 86 attributes left.
# 
# 70. run of importance source...
# Computing permutation importance.. Progress: 65%. Estimated remaining time: 16 seconds.
# Assigned hit to 45 attributes out of 86 undecided.
# 71. run of importance source...
# Computing permutation importance.. Progress: 62%. Estimated remaining time: 19 seconds.
# Assigned hit to 61 attributes out of 86 undecided.
# 72. run of importance source...
# Computing permutation importance.. Progress: 66%. Estimated remaining time: 16 seconds.
# Assigned hit to 34 attributes out of 86 undecided.
# After 72 iterations, +1.7 hours: 
#   confirmed 1 attribute: Operating_Cash_Flow;
# still have 85 attributes left.
# 
# 73. run of importance source...
# Computing permutation importance.. Progress: 65%. Estimated remaining time: 16 seconds.
# Assigned hit to 36 attributes out of 85 undecided.
# 74. run of importance source...
# Computing permutation importance.. Progress: 64%. Estimated remaining time: 17 seconds.
# Assigned hit to 58 attributes out of 85 undecided.
# 75. run of importance source...
# Computing permutation importance.. Progress: 64%. Estimated remaining time: 17 seconds.
# Assigned hit to 57 attributes out of 85 undecided.
# After 75 iterations, +1.7 hours: 
#   confirmed 3 attributes: debtRatio, dividendYield, priceToSalesRatio;
# rejected 1 attribute: Weighted_Average_Shs_Out_.Dil.;
# still have 81 attributes left.
# 
# 76. run of importance source...
# Computing permutation importance.. Progress: 60%. Estimated remaining time: 20 seconds.
# Assigned hit to 62 attributes out of 81 undecided.
# 77. run of importance source...
# Computing permutation importance.. Progress: 63%. Estimated remaining time: 18 seconds.
# Assigned hit to 59 attributes out of 81 undecided.
# After 77 iterations, +1.8 hours: 
#   confirmed 2 attributes: Net_Income_Growth, returnOnEquity;
# rejected 1 attribute: Investments;
# still have 78 attributes left.
# 
# 78. run of importance source...
# Computing permutation importance.. Progress: 65%. Estimated remaining time: 16 seconds.
# Assigned hit to 52 attributes out of 78 undecided.
# 79. run of importance source...
# Computing permutation importance.. Progress: 65%. Estimated remaining time: 16 seconds.
# Assigned hit to 59 attributes out of 78 undecided.
# 80. run of importance source...
# Computing permutation importance.. Progress: 66%. Estimated remaining time: 16 seconds.
# Assigned hit to 32 attributes out of 78 undecided.
# After 80 iterations, +1.8 hours: 
#   confirmed 1 attribute: ROE;
# still have 77 attributes left.
# 
# 81. run of importance source...
# Computing permutation importance.. Progress: 66%. Estimated remaining time: 15 seconds.
# Assigned hit to 21 attributes out of 77 undecided.
# 82. run of importance source...
# Computing permutation importance.. Progress: 62%. Estimated remaining time: 19 seconds.
# Assigned hit to 42 attributes out of 77 undecided.
# After 82 iterations, +1.9 hours: 
#   confirmed 1 attribute: Inventory_Growth;
# rejected 2 attributes: Average_Inventory, X10Y_Revenue_Growth_.per_Share.;
# still have 74 attributes left.
# 
# 83. run of importance source...
# Computing permutation importance.. Progress: 70%. Estimated remaining time: 13 seconds.
# Assigned hit to 17 attributes out of 74 undecided.
# 84. run of importance source...
# Computing permutation importance.. Progress: 60%. Estimated remaining time: 20 seconds.
# Assigned hit to 51 attributes out of 74 undecided.
# 85. run of importance source...
# Computing permutation importance.. Progress: 69%. Estimated remaining time: 14 seconds.
# Assigned hit to 55 attributes out of 74 undecided.
# After 85 iterations, +1.9 hours: 
#   confirmed 1 attribute: freeCashFlowOperatingCashFlowRatio;
# still have 73 attributes left.
# 
# 86. run of importance source...
# Computing permutation importance.. Progress: 67%. Estimated remaining time: 15 seconds.
# Assigned hit to 51 attributes out of 73 undecided.
# 87. run of importance source...
# Computing permutation importance.. Progress: 67%. Estimated remaining time: 15 seconds.
# Assigned hit to 45 attributes out of 73 undecided.
# 88. run of importance source...
# Computing permutation importance.. Progress: 69%. Estimated remaining time: 13 seconds.
# Assigned hit to 51 attributes out of 73 undecided.
# After 88 iterations, +2 hours: 
#   confirmed 2 attributes: Cost_of_Revenue, ebitperRevenue;
# still have 71 attributes left.
# 
# 89. run of importance source...
# Computing permutation importance.. Progress: 69%. Estimated remaining time: 14 seconds.
# Assigned hit to 38 attributes out of 71 undecided.
# 90. run of importance source...
# Computing permutation importance.. Progress: 68%. Estimated remaining time: 14 seconds.
# Assigned hit to 36 attributes out of 71 undecided.
# After 90 iterations, +2 hours: 
#   confirmed 2 attributes: Debt_to_Assets, EBITDA_Margin;
# still have 69 attributes left.
# 
# 91. run of importance source...
# Computing permutation importance.. Progress: 68%. Estimated remaining time: 14 seconds.
# Assigned hit to 16 attributes out of 69 undecided.
# 92. run of importance source...
# Computing permutation importance.. Progress: 71%. Estimated remaining time: 12 seconds.
# Assigned hit to 15 attributes out of 69 undecided.
# 93. run of importance source...
# Computing permutation importance.. Progress: 68%. Estimated remaining time: 14 seconds.
# Assigned hit to 44 attributes out of 69 undecided.
# After 93 iterations, +2.1 hours: 
#   rejected 2 attributes: inventoryTurnover, shortTermCoverageRatios;
# still have 67 attributes left.
# 
# 94. run of importance source...
# Computing permutation importance.. Progress: 59%. Estimated remaining time: 21 seconds.
# Assigned hit to 24 attributes out of 67 undecided.
# 95. run of importance source...
# Computing permutation importance.. Progress: 67%. Estimated remaining time: 15 seconds.
# Assigned hit to 43 attributes out of 67 undecided.
# After 95 iterations, +2.1 hours: 
#   confirmed 1 attribute: EPS_Diluted;
# still have 66 attributes left.
# 
# 96. run of importance source...
# Computing permutation importance.. Progress: 69%. Estimated remaining time: 13 seconds.
# Assigned hit to 40 attributes out of 66 undecided.
# 97. run of importance source...
# Computing permutation importance.. Progress: 68%. Estimated remaining time: 14 seconds.
# Assigned hit to 29 attributes out of 66 undecided.
# 98. run of importance source...
# Computing permutation importance.. Progress: 70%. Estimated remaining time: 13 seconds.
# Assigned hit to 41 attributes out of 66 undecided.
# After 98 iterations, +2.1 hours: 
#   confirmed 2 attributes: Capex_to_Operating_Cash_Flow, quickRatio;
# still have 64 attributes left.
# 
# 99. run of importance source...
# Computing permutation importance.. Progress: 71%. Estimated remaining time: 12 seconds.
# Assigned hit to 48 attributes out of 64 undecided.