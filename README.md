cics-java-liberty-restappext
=============================

This repository provides sample materials for use with the IBM Redbooks video course
"[Extending a CICS Web application using JCICS](https://www.redbooks.ibm.com/redbooks.nsf/redbookabstracts/crse0302.html?Open)". The
application provided is a simple, RESTful web application providing several code examples for accessing CICS resources from Java using
the JCICS API.

This repository builds on the application constructed in the IBM Redbooks video course
"[Developing a RESTful Web application for Liberty in CICS](https://www.redbooks.ibm.com/redbooks.nsf/redbookabstracts/crse0300.html?Open)",
which uses the [cics-java-liberty-restapp](https://github.com/cicsdev/cics-java-liberty-restapp) repository.

For further examples, see the [cics-java-jcics-samples](https://github.com/cicsdev/cics-java-jcics-samples) repository.

## Repository contents

Full details on the contents of this repository can be found on the [Source code](Source.md) page.

## Pre-reqs

* CICS TS V5.4 or later
* Java SE 1.8 or later on the workstation
* Eclipse with the IBM CICS SDK for Java EE, Jakarta EE and Liberty, or any IDE that supports usage of the Maven Central artifact [com.ibm.cics:com.ibm.cics.server.](https://search.maven.org/artifact/com.ibm.cics/com.ibm.cics.server)
* Maven or Gradle build tools (optional)

## Getting started

A guide to deploying these samples into CICS can be found on the [Getting started](GettingStarted.md) page.

## Running the code samples

See the dedicated pages for executing the [JAX-RS](RunningJAXRS.md) and [Link to Liberty](RunningLinkToLiberty.md) sample applications inside CICS.

## License
This project is licensed under [Apache License Version 2.0](LICENSE).

