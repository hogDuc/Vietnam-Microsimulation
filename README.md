# Vietnam-Microsimulation

### Introduction
A static Microsimulation model for Vietnam.
The simulation utilizes the Vietnam Household Living Standard Survey as the input. It allows users to modify different tax policies, subsidy policies, etc... to see how said policies impact Vietnam's microeconomy, which includes Government income / expenditure, households' income, expenditures, as well as Vietnam GINI index and poverty rate.

### Disclaimer
**Since the VHLSS data is confidential, it cannot be uploaded to Github. However, past VHLSS data are available for free online, and with some adjustments to the variable names, the simulation should still run normally.**

### Instruction
Once the appropriate VHLSS data is saved in the working directory, run vnmod_shiny.R
You can adjust different taxes, subsidy policies on the left panel, press submit to see the results.
**Caution:** Do not leave input fields empty as it will return NA.
To customize new policies, the code must be modified to target appropriate subjects to apply the policy.
