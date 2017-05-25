cics-java-liberty-restapp-ext
=============================

This repository provides sample materials for use with the IBM Redbooks video course
"[Extending a CICS Web application using JCICS](https://www.redbooks.ibm.com/redbooks.nsf/redbookabstracts/crse0302.html?Open)". The
application provided is a simple, RESTful web application providing several code examples for accessing CICS resources from Java. 

For further examples, see the [cics-java-jcics-samples](https://github.com/cicsdev/cics-java-jcics-samples) repository.

## Source code

### Java package com.ibm.cicsdev.restappext
* [`CICSApplication`](src-java/com/ibm/cicsdev/restappext/CICSApplication.java) - class used to specify the path for this application.
* [`LinkChannelResource`](src-java/com/ibm/cicsdev/restappext/LinkChannelResource.java) - provides a method which demonstrates the use
of channels and containers when using the LINK command in Java.
* [`LinkCommareaResource`](src-java/com/ibm/cicsdev/restappext/LinkCommareaResource.java) - contains several methods demonstrating
the options available for using the LINK command in Java with a commarea.
* [`LinkToLiberty`](src-java/com/ibm/cicsdev/restappext/LinkToLiberty.java) - simple POJO to demonstrate how the `CICSProgram` annotation
can be used to allow non-Java programs to issue an `EXEC CICS LINK` command and execute code in a Liberty JVM server. The `CICSProgram`
annotation requires CICS Explorer or CICS Build Toolkit V5.3.0.8 or later.
* [`TemporaryStorageResource`](src-java/com/ibm/cicsdev/restappext/TemporaryStorageResource.java) - several methods used to manipulate TSQs.
* [`TransientDataResource`](src-java/com/ibm/cicsdev/restappext/TransientDataResource.java)
* [`VsamKsdsFileResource`](src-java/com/ibm/cicsdev/restappext/VsamKsdsFileResource.java) - demonstrates use of the JCICS API to
access a VSAM KSDS file.

### Java package com.ibm.cicsdev.restappext.bean
* [`StatusBean`](src-java/com/ibm/cicsdev/restappext/bean/StatusBean.java) - simple JAX-RS bean for returning a status message back to the RESTful client.
* [`StockPartBean`](src-java/com/ibm/cicsdev/restappext/bean/StockPartBean.java) - simple JAX-RS bean for returning the information held in a StockPart instance.
* [`StockPartCollection`](src-java/com/ibm/cicsdev/restappext/bean/StockPartCollection.java) - simple JAX-RS bean for returning a collection of StockPartBean instances.
* [`SupplierBean`](src-java/com/ibm/cicsdev/restappext/bean/SupplierBean.java) - simple JAX-RS bean for returning the information held in a Supplier instance.

### Java package com.ibm.cicsdev.restappext.helper
* [`StockPartHelper`](src-java/com/ibm/cicsdev/restappext/helper/StockPartHelper.java) - class used to provide methods used when creating sample StockPart objects.

### COBOL copybooks
* [`src-cobol/STOKPART.cpy`](src-cobol/STOKPART.cpy) - copybook used to generate the `StockPart` class.
* [`src-cobol/SUPPLIER.cpy`](src-cobol/SUPPLIER.cpy) - copybook used to generate the `Supplier` class.

### COBOL source files
* [`src-cobol/ADDPART.cbl`](src-cobol/ADDPART.cbl) - write a STOCK-PART commarea to a file.
* [`src-cobol/ADDPARTC.cbl`](src-cobol/ADDPARTC.cbl) - write a STOCK-PART container to a file.
* [`src-cobol/GETPART.cbl`](src-cobol/GETPART.cbl) - receive a part ID in a commarea and return a complete StockPart record.
* [`src-cobol/GETSUPPL.cbl`](src-cobol/GETSUPPL.cbl) - receive a StockPart record in the commarea and return the relevant Supplier record.
* [`src-cobol/PROG1.cbl`](src-cobol/PROG1.cbl) - receive no commarea and write a message using COBOL DISPLAY.



## Supporting files
* [`etc/DFHCSD.txt`](etc/DFHCSD.txt) - output from a DFHCSDUP EXTRACT command for the sample resources used.
* [`etc/DEFVSAM.jcl`](etc/DEFVSAM.jcl) - a sample job to define the dataset required to run the VSAM KSDS examples.
* [`lib/com.ibm.cicsdev.restappext.generated.jar`](lib/com.ibm.cicsdev.restappext.generated.jar) - contains the Java
source and class files generated using the JZOS record generator.


## Pre-requisites

* CICS TS V5.1 or later, due to the usage of the `getString()` methods.
* Java SE 1.7 or later on the z/OS system
* Java SE 1.7 or later on the workstation
* Eclipse with WebSphere Developer Tools and CICS Explorer SDK V5.3.0.8 or later installed


## Configuration

The sample Java classes are designed to be added to a dynamic web project and deployed into a Liberty JVM server as a WAR,
either using the dropins directory or using a CICS bundle project. 



The VSAM examples use the sample file `SMPLXMPL`. For a sample CICS FILE definition, see the file [`etc/DFHCSD.txt`](etc/DFHCSD.txt).


### To add the resources to Eclipse:
1. Using an Eclipse development environment create a dynamic web project called `com.ibm.cicsdev.restapp-ext` and add the Java samples to the `src` folder.
1. Copy the `com.ibm.cicsdev.restappext.generated.jar` file to the folder `/WebContent/WEB-INF/lib` relative to the root of your WAR project.
1. Add the CICS Liberty JVM server libraries to the build path of your project. 
1. Add the `com.ibm.cicsdev.restappext.generated.jar` file to the project build path.
1. Ensure the web project is targeted to compile at a level that is compatible with the Java level being used on CICS. This can be achieved by editing the Java Project Facet in the project properties.
1. [Optional] Create a CICS bundle project called com.ibm.cicsdev.restapp-ext.cicsbundle and add a dynamic web project include for the project created in step 1.

### To start a JVM server in CICS:
1. Enable Java support in the CICS region by adding the `SDFJAUTH` library to the STEPLIB concatenation and setting `USSHOME` and the `JVMPROFILEDIR` SIT parameters.
1. Define a Liberty JVM server called `DFHWLP` using the supplied sample definition `DFHWLP` in the CSD group `DFH$WLP`.
1. Copy the CICS sample `DFHWLP.jvmprofile` zFS file to the `JVMPROFILEDIR` directory specified above and ensure the `JAVA_HOME` variable is set correctly.
1. Add the `jaxrs-1.1` Liberty feature to `server.xml`.
1. Install the `DFHWLP` resource defined in step 2 and ensure it becomes enabled.
1. [CICS TS V5.3 with APAR PI63005 only] Add the `cicsts:link-1.0` feature to `server.xml`.

*Note:* in CICS TS V5.1, the file suffix `.jvmprofile` is not used.


## Running the examples

All JAX-RS methods can be invoked using a web browser with the root URI of `http://host:port/com.ibm.cicsdev.restapp.ext/`.

### LINK without data

Invoke the COBOL program `PROG1` with no commarea using the `rest/commarea/empty` URI. See the [`PROG1.cbl`](src-cobol.cbl) program
which may be used with this sample.


### LINK with a commarea
* Invoke the COBOL program `ADDPART` using the `rest/commarea/addPart` URI. See the [`ADDPART.cbl`](src-cobol/ADDPART.cbl) program which
may be used with this sample.
* Invoke the COBOL program `GETSUPPL` using the `rest/commarea/getSupplier` URI. See the [`GETSUPPL.cbl`](src-cobol/GETSUPPL.cbl) program which
may be used with this sample.
* Invoke the COBOL program `GETPART` using the `rest/commarea/getPart` URI. See the [`GETPART.cbl`](src-cobol/GETPART.cbl) program which
may be used with this sample.

### LINK with channels and containers
* Invoke the COBOL program `ADDPARTC` using the `rest/channel/addPart` URI. See the [`ADDPARTC.cbl`](src-cobol/ADDPARTC.cbl) program which
may be used with this sample.

### LINK to Liberty
* The ability to LINK to a program defined as a POJO in Liberty is available when using CICS TS V5.3 with APAR PI63005. Add the `cicsts:link-1.0`
feature to server.xml to enable the automatic creation of a CICS PROGRAM definition.

### Temporary storage queues
* Write an item to a TSQ using the `rest/tsq/write` URI.
* Update the first item in a TSQ using the `rest/tsq/update` URI.
* Delete a TSQ using the `rest/tsq/delete` URI.


### VSAM KSDS
* Write a new record to the file using the `rest/ksds/write` URI. This invokes the `VsamKsdsFileResource.writeNewRecord()` method.
* Delete the first record in the file using the `rest/ksds/delete` URI. This invokes the `VsamKsdsFileResource.deleteRecord()` method.
* Update the first record in the file using the `rest/ksds/update` URI. This invokes the `VsamKsdsFileResource.updateRecord()` method.

In all cases, the JSON returned will be the current contents of the file. Pressing refresh in the browser will repeat the GET request,
causing a record to be written, updated, or deleted accordingly.

## License
This project is licensed under [Apache License Version 2.0](LICENSE).


## Reference

* For further details on the Link to Liberty functionality refer to this
[developer center article](https://developer.ibm.com/cics/2016/11/14/link-to-liberty-now-available-in-cics-ts-v5-3/)
