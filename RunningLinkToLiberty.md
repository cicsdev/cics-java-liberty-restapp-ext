Running the Link to Liberty examples
====================================

The Link to Liberty samples are provided as COBOL programs, which `EXEC CICS LINK` to a Java method which
has the `@CICSProgram` annotation. Note that the ability to LINK to a program defined as a POJO in Liberty is available when using:

* CICS TS V5.3 with [APAR PI63005](http://www-01.ibm.com/support/docview.wss?uid=swg1PI63005)
* CICS TS V5.4

Annotations must be enabled in the development environment for the LINKable programs to be defined automatically when the application
is deployed. Add the `cicsts:link-1.0` feature to `server.xml` to enable the automatic creation of a CICS PROGRAM definition.


## LINK to Liberty

Each of the examples are initiated as a transaction from a CICS terminal, and invoke the method in the indicated Java class.

* `JL2L [ supplierID ]` - starts program [`LINK2SUP`](src/Cobol/LINK2SUP.cbl) which invokes the `GETSUPPI` program, defined in the
[LinkToLiberty](src/Java/com/ibm/cicsdev/restappext/LinkToLiberty.java) class. The parameter supplierID is optional and should
be numeric if specified.
* `JL2S ` - starts program [`LINK2SEC`](src/Cobol/LINK2SEC.cbl) which invokes the `L2LSEC` program, defined in the
[LinkToSecurity](src/Java/com/ibm/cicsdev/restappext/LinkToSecurity.java) class.
* `JL2T [ action ]` - starts program  [`LINK2TXN`](src/Cobol/LINK2TXN.cbl) which invokes the `L2LTRAN` program, defined in the
[LinkToTransaction](src/Java/com/ibm/cicsdev/restappext/LinkToTransaction.java) class. The parameter action is optional should
be one of the following values:
  * `COMMIT`
  * `ROLLBACK`
  * `ABEND`
  * `THROW`
  * `CATCH`
  * `PERCOLATE`

## Reference

* For further details on the Link to Liberty functionality refer to this
[developer center article](https://developer.ibm.com/cics/2016/11/14/link-to-liberty-now-available-in-cics-ts-v5-3/)
