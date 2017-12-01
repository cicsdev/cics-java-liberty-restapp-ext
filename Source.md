Source code
===========

## Java files

### [com.ibm.cicsdev.restappext](src/Java/com/ibm/cicsdev/restappext)

* [`CICSApplication`](src/Java/com/ibm/cicsdev/restappext/CICSApplication.java) - class used to specify the path for this application.
* [`LinkChannelResource`](src/Java/com/ibm/cicsdev/restappext/LinkChannelResource.java) - provides a method which demonstrates the use of channels and containers when using the LINK command in Java.
* [`LinkCommareaResource`](src/Java/com/ibm/cicsdev/restappext/LinkCommareaResource.java) - contains several methods demonstrating the options available for using the LINK command in Java with a commarea.
* [`LinkToLiberty`](src/Java/com/ibm/cicsdev/restappext/LinkToLiberty.java) - simple POJO to demonstrate how the `CICSProgram` annotation can be used to allow non-Java programs to issue an `EXEC CICS LINK` command and execute code in a Liberty JVM server. The `CICSProgram` annotation requires CICS Explorer or CICS Build Toolkit V5.3.0.8 or later.
* [`LinkToSecurity`](src/Java/com/ibm/cicsdev/restappext/LinkToSecurity.java) - Link to Liberty method to demonstrate some aspects of the security behaviour of Link to Liberty.  Returns the CICS userID and the Java Subject name in container `CONT-IDENTITY`.
* [`LinkToTransaction`](src/Java/com/ibm/cicsdev/restappext/LinkToTransaction.java) - Link to Liberty method to demonstrate aspects of transaction and Exception behaviour of Link to Liberty.  Expects input container "ACTION" containing one of { "COMMIT", "ROLLBACK", "ABEND", "THROW", "CATCH", "PERCOLATE" } and will act accordingly.
* [`TaskResource`](src/Java/com/ibm/cicsdev/restappext/TaskResource.java) - provides a simple REST service for retrieving basic task information.
* [`TemporaryStorageResource`](src/Java/com/ibm/cicsdev/restappext/TemporaryStorageResource.java) - several methods used to manipulate TSQs.
* [`VsamKsdsFileResource`](src/Java/com/ibm/cicsdev/restappext/VsamKsdsFileResource.java) - demonstrates use of the JCICS API to access a VSAM KSDS file.

### [com.ibm.cicsdev.restappext.bean](src/Java/com/ibm/cicsdev/restappext/bean)

* [`StatusBean`](src/Java/com/ibm/cicsdev/restappext/bean/StatusBean.java) - simple JAX-RS bean for returning a status message back to the RESTful client.
* [`StockPartBean`](src/Java/com/ibm/cicsdev/restappext/bean/StockPartBean.java) - simple JAX-RS bean for returning the information held in a `StockPart` instance.
* [`StockPartCollection`](src/Java/com/ibm/cicsdev/restappext/bean/StockPartCollection.java) - simple JAX-RS bean for returning a collection of `StockPartBean` instances.
* [`SupplierBean`](src/Java/com/ibm/cicsdev/restappext/bean/SupplierBean.java) - simple JAX-RS bean for returning the information held in a `Supplier` instance.
* [`TaskBean`](src/Java/com/ibm/cicsdev/restappext/bean/TaskBean.java) - simple JAX-RS bean for storing task information retrieved from CICS.

### [com.ibm.cicsdev.restappext.helper](src/Java/com/ibm/cicsdev/restappext/helper)

* [`StockPartHelper`](src/Java/com/ibm/cicsdev/restappext/helper/StockPartHelper.java) - class used to provide methods used when creating sample `StockPart` objects.

## COBOL files

### COBOL copybooks

* [`STOKPART.cpy`](src/Cobol/STOKPART.cpy) - copybook used to generate the `StockPart` class.
* [`SUPPLIER.cpy`](src/Cobol/SUPPLIER.cpy) - copybook used to generate the `Supplier` class.

### COBOL source

* [`ADDPART.cbl`](src/Cobol/ADDPART.cbl) - write a `STOCK-PART` commarea to a file.
* [`ADDPARTC.cbl`](src/Cobol/ADDPARTC.cbl) - write a `STOCK-PART` container to a file.
* [`GETPART.cbl`](src/Cobol/GETPART.cbl) - receive a part ID in a commarea and return a complete `StockPart` record.
* [`GETSUPPL.cbl`](src/Cobol/GETSUPPL.cbl) - receive a `StockPart` record in the commarea and return the relevant `Supplier` record.
* [`LINK2SEC.cbl`](src/Cobol/LINK2SEC.cbl) - run from terminal to invoke L2LSEC ([`LinkToSecurity.java`](src/Java/com/ibm/cicsdev/restappext/LinkToSecurity.java)) via Link to Liberty and display results to the user.
* [`LINK2SUP.cbl`](src/Cobol/LINK2SUP.cbl) - run from terminal to invoke GETSUPPI ([`LinkToLiberty.java`](src/Java/com/ibm/cicsdev/restappext/LinkToLiberty.java)) via Link to Liberty.
* [`LINK2TXN.cbl`](src/Cobol/LINK2TXN.cbl) - run from terminal to invoke L2LTRAN ([`LinkToTransaction.java`](src/Java/com/ibm/cicsdev/restappext/LinkToTransaction.java)) via Link to Liberty, accepting input ACTION from the terminal and passing to Java in container "ACTION".
* [`PROG1.cbl`](src/Cobol/PROG1.cbl) - receive no commarea and write a message using COBOL DISPLAY.

## Supporting files

* [`DFHCSD.txt`](etc/DFHCSD.txt) - output from a DFHCSDUP EXTRACT command for the sample resources used.
* [`DEFVSAM.jcl`](etc/DEFVSAM.jcl) - a sample job to define the dataset required to run the VSAM KSDS examples.
* [`com.ibm.cicsdev.restappext.generated.jar`](lib/com.ibm.cicsdev.restappext.generated.jar) - contains the Java
source and class files generated using the JZOS record generator.

## Reference

* For further details on the Link to Liberty functionality refer to this
[developer center article](https://developer.ibm.com/cics/2016/11/14/link-to-liberty-now-available-in-cics-ts-v5-3/)
