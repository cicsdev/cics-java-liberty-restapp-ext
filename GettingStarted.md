Getting started
===============

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
1. Using an Eclipse development environment create a dynamic web project called `com.ibm.cicsdev.restappext` and add the Java samples to the `src` folder.
1. Copy the `com.ibm.cicsdev.restappext.generated.jar` file to the folder `/WebContent/WEB-INF/lib` relative to the root of your WAR project.
1. Add the CICS Liberty JVM server libraries to the build path of your project. 
1. Add the `com.ibm.cicsdev.restappext.generated.jar` file to the project build path.
1. Ensure the web project is targeted to compile at a level that is compatible with the Java level being used on CICS. This can be achieved by editing the Java Project Facet in the project properties.
1. [Optional] Create a CICS bundle project called com.ibm.cicsdev.restappext.cicsbundle and add a dynamic web project include for the project created in step 1.

### To start a JVM server in CICS:
1. Enable Java support in the CICS region by adding the `SDFJAUTH` library to the STEPLIB concatenation and setting `USSHOME` and the `JVMPROFILEDIR` SIT parameters.
1. Define a Liberty JVM server called `DFHWLP` using the supplied sample definition `DFHWLP` in the CSD group `DFH$WLP`.
1. Copy the CICS sample `DFHWLP.jvmprofile` zFS file to the `JVMPROFILEDIR` directory specified above and ensure the `JAVA_HOME` variable is set correctly.
1. Add the `jaxrs-1.1` Liberty feature to `server.xml`.
1. Install the `DFHWLP` resource defined in step 2 and ensure it becomes enabled.
1. [CICS TS V5.3 with APAR PI63005 only] Add the `cicsts:link-1.0` feature to `server.xml`.

*Note:* in CICS TS V5.1, the file suffix `.jvmprofile` is not used.

### To add sample resources to CICS:
1. Compile the supplied sample COBOL programs into a load library included in the CICS DFHRPL concatenation.
1. Run a DFHCSDUP job using the definitions for the sample resources (etc/DFHCSD.txt).
