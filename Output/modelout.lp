/* Objective function */
min: +6543333.33334 USA_Location_Low +9563333.33333 USA_Location_High +5013200 Germany_Location_Low +7318466.66666 Germany_Location_High
 +6271533.33334 Japan_Location_Low +9160666.66666 Japan_Location_High +3251533.33333 Brazil_Location_Low
 +4761533.33333 Brazil_Location_High +2124066.66667 India_Location_Low +6201066.66666 India_Location_High
 +12 USA_Production_USA +26.335 Germany_Production_USA +25.4 Japan_Production_USA +24.45 Brazil_Production_USA
 +18.65 India_Production_USA +24.25 USA_Production_Germany +13 Germany_Production_Germany +32.75 Japan_Production_Germany
 +30.05 Brazil_Production_Germany +20.4 India_Production_Germany +13.1 USA_Production_Japan +21.617 Germany_Production_Japan
 +10 Japan_Production_Japan +36 Brazil_Production_Japan +29.5 India_Production_Japan +28.1 USA_Production_Brazil
 +33.244 Germany_Production_Brazil +53.61 Japan_Production_Brazil +8 Brazil_Production_Brazil +34.4 India_Production_Brazil
 +20.778 USA_Production_India +23.073 Germany_Production_India +24.35 Japan_Production_India +37.75 Brazil_Production_India
 +5 India_Production_India;

/* Constraints */
Location constraint for USA: -500000 USA_Location_Low -1000000 USA_Location_High +USA_Production_USA +USA_Production_Germany +USA_Production_Japan
 +USA_Production_Brazil +USA_Production_India <= 0;
Location constraint for Germany: -500000 Germany_Location_Low -1000000 Germany_Location_High +Germany_Production_USA +Germany_Production_Germany
 +Germany_Production_Japan +Germany_Production_Brazil +Germany_Production_India <= 0;
Location constraint for Japan: -500000 Japan_Location_Low -1000000 Japan_Location_High +Japan_Production_USA +Japan_Production_Germany
 +Japan_Production_Japan +Japan_Production_Brazil +Japan_Production_India <= 0;
Location constraint for Brazil: -500000 Brazil_Location_Low -1000000 Brazil_Location_High +Brazil_Production_USA +Brazil_Production_Germany
 +Brazil_Production_Japan +Brazil_Production_Brazil +Brazil_Production_India <= 0;
Location constraint for India: -500000 India_Location_Low -1000000 India_Location_High +India_Production_USA +India_Production_Germany
 +India_Production_Japan +India_Production_Brazil +India_Production_India <= 0;
Production constraint for USA: +USA_Production_USA +Germany_Production_USA +Japan_Production_USA +Brazil_Production_USA +India_Production_USA >= 2800000;
Production constraint for Germany: +USA_Production_Germany +Germany_Production_Germany +Japan_Production_Germany +Brazil_Production_Germany
 +India_Production_Germany >= 90000;
Production constraint for Japan: +USA_Production_Japan +Germany_Production_Japan +Japan_Production_Japan +Brazil_Production_Japan +India_Production_Japan >= 1700000;
Production constraint for Brazil: +USA_Production_Brazil +Germany_Production_Brazil +Japan_Production_Brazil +Brazil_Production_Brazil
 +India_Production_Brazil >= 145000;
Production constraint for India: +USA_Production_India +Germany_Production_India +Japan_Production_India +Brazil_Production_India +India_Production_India >= 160000;
CO2 constraint for USA: +84.0620948 Germany_Production_USA +299.31716568 Japan_Production_USA +125.5220626 Brazil_Production_USA
 +181.06915104 India_Production_USA <= 10000000000;
CO2 constraint for Germany: +84.0620948 USA_Production_Germany +267.65434 Japan_Production_Germany +140.79148464 Brazil_Production_Germany
 +149.30790664 India_Production_Germany <= 10000000000;
CO2 constraint for Japan: +299.31716568 USA_Production_Japan +267.65434 Germany_Production_Japan +282.62497856 Brazil_Production_Japan
 +127.72629136 India_Production_Japan <= 10000000000;
CO2 constraint for Brazil: +125.5220626 USA_Production_Brazil +140.79148464 Germany_Production_Brazil +282.62497856 Japan_Production_Brazil
 +191.40908928 India_Production_Brazil <= 10000000000;
CO2 constraint for India: +181.06915104 USA_Production_India +149.30790664 Germany_Production_India +127.72629136 Japan_Production_India
 +191.40908928 Brazil_Production_India <= 10000000000;
