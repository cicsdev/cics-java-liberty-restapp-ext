Getting started
===============

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
1. [Optional] Create a CICS bundle project called `com.ibm.cicsdev.restappext.cicsbundle` and add a dynamic web project include for the project created in step 1.



### Building the Example

The sample can be built using the supplied Gradle or Maven build files to produce a WAR file and optionally a CICS Bundle archive.

#### Gradle (command line)

Run the following in a local command prompt:

`gradle clean build`

This creates a WAR file inside the `build/libs` directory and a CICS bundle ZIP file inside the `build/distributions` directory.

If using the CICS bundle ZIP, the default CICS JVM server name `DFHWLP` should be modified using the `jvmserver` property in the gradle build [file](build.gradle) to match the required CICS JVMSERVER resource name, or alternatively can be set on the command line as follows.


`gradle clean build -Pjvmserver=MYJVM`


#### Maven (command line)

First install the generated JAR file into the local Maven repository by running the following Maven command in a local command prompt

`mvn org.apache.maven.plugins:maven-install-plugin:2.5.2:install-file -Dfile=lib/cics-java-liberty-restapp-ext-generated.jar     -DgroupId=com.ibm.cicsdev -DartifactId=cics-java-liberty-restapp-ext-generated -Dversion=1.0 -Dpackaging=jar -DlocalRepositoryPath=local-repo -DcreateChecksum=true`

Next run the following in a local command prompt which will create a WAR file.

`mvn clean verify`

This creates a WAR file in the `target` directory. 

If building a CICS bundle ZIP the CICS bundle plugin bundle-war goal is driven using the maven verify phase. The CICS JVM server name should be modified in the <jvmserver> property in the [`pom.xml`](pom.xml) to match the required CICS JVMSERVER resource name, or alternatively can be set on the command line as follows. 

`mvn clean verify -Djvmserver=MYJVM`




### To start a JVM server in CICS:
1. Enable Java support in the CICS region by adding the `SDFJAUTH` library to the STEPLIB concatenation and setting `USSHOME` and the `JVMPROFILEDIR` SIT parameters.
    * Adding ``SDFJAUTH`` is **not required** if using CICS 5.5 or later.
3. Define a Liberty JVM server called `DFHWLP` using the supplied sample definition `DFHWLP` in the CSD group `DFH$WLP`.
4. Copy the CICS sample `DFHWLP.jvmprofile` zFS file to the `JVMPROFILEDIR` directory specified above and ensure the `JAVA_HOME` variable is set correctly.
5. Add the `jaxrs-1.1` Liberty feature to `server.xml`.
6. Install the `DFHWLP` resource defined in step 2 and ensure it becomes enabled.
7. [CICS TS V5.4, or V5.3 with APAR PI63005 only] Add the `cicsts:link-1.0` feature to `server.xml`.

*Note:* in CICS TS V5.1, the file suffix `.jvmprofile` is not used.

### To add sample resources to CICS:
1. Compile the supplied sample COBOL programs into a load library included in the CICS DFHRPL concatenation.
1. Run a DFHCSDUP job using the definitions for the [sample resources](etc/DFHCSD.txt).
