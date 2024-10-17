Running the Link to Liberty examples
====================================

The Link to Liberty samples are provided as COBOL programs with associated Java classes, which demonstrate `EXEC CICS LINK` to
a Java method which has the `@CICSProgram` annotation. Note that the ability to link to a program defined as a POJO in Liberty
is available when using CICS TS V5.4 or later.

Annotations must be enabled in the development environment, and the `cicsts:link-1.0` feature added to `server.xml` for the
linkable programs to be made available when the application is deployed.


## Link to Liberty

Each of the examples are initiated as a transaction from a CICS terminal, and invoke the method in the indicated Java class.

* `JL2L [ supplierID ]` - starts program [`LINK2SUP`](src/cobol/LINK2SUP.cbl) which invokes the `GETSUPPI` program, defined in the
[LinkToLiberty](src/main/java/com/ibm/cicsdev/restappext/LinkToLiberty.java) class. The parameter supplierID is optional and should
be numeric if specified.
* `JL2S ` - starts program [`LINK2SEC`](src/cobol/LINK2SEC.cbl) which invokes the `L2LSEC` program, defined in the
[LinkToSecurity](src/main/java/com/ibm/cicsdev/restappext/LinkToSecurity.java) class.
* `JL2T [ action ]` - starts program  [`LINK2TXN`](src/cobol/LINK2TXN.cbl) which invokes the `L2LTRAN` program, defined in the
[LinkToTransaction](src/main/java/com/ibm/cicsdev/restappext/LinkToTransaction.java) class. The parameter action is optional should
be one of the following values:
  * `COMMIT`
  * `ROLLBACK`
  * `ABEND`
  * `THROW`
  * `CATCH`
  * `PERCOLATE`

## Reference

See the following sites for further details on the Link to Liberty functionality:

* [Linking to a Java EE application from a CICS program](https://www.ibm.com/support/knowledgecenter/SSGMCP_5.4.0/applications/developing/java/link_2_liberty.html)
in the IBM Knowledge Center
