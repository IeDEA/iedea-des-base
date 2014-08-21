# IeDEA-DES-Base Package

This repository is intended to the become the basic code you will need to
interact with the any data that are stored using the **IeDEA Data Exchange
Standard (IeDEA-DES)**

It will allow the following functionality:

* Reading the `IeDEA-DES` table specs from machine readable form
* Loading data that is formatted according to the `IeDEA-DES`
* Compute basic virtual variables: inferred values such as data of HAART
initiation or CD4 at baseline. These are useful and commonly used values that
are associated with individual records but their direct storage should be
avoided.
* Compute basic metrics on populations of patients
* Basic data visualizations and graphics, including association with maps via
the tblCENTER records
* Run basic quality assurance tests. (Currently on hold)
