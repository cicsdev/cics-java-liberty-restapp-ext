Running the Link to Liberty examples
====================================

## LINK to Liberty

* The ability to LINK to a program defined as a POJO in Liberty is available when using CICS TS V5.3 with
[APAR PI63005](http://www-01.ibm.com/support/docview.wss?uid=swg1PI63005). Add the `cicsts:link-1.0` feature to server.xml to enable the automatic
creation of a CICS PROGRAM definition.
* Annotations must be enabled in the development environment for the LINKable programs (GETSUPPI,L2LSEC,L2LTRAN) to be defined automatically when the application is deployed.
* At a CICS terminal, start transaction JL2L with (optionally) a numeric Supplier ID as a parameter to the transaction; JL2S to invoke the Security sample; or JL2T with (optionally) one of the LinkToTransaction action verbs (see above) to perform the associated transaction test.

## Reference

* For further details on the Link to Liberty functionality refer to this
[developer center article](https://developer.ibm.com/cics/2016/11/14/link-to-liberty-now-available-in-cics-ts-v5-3/)
