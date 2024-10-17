Running the JAX-RS examples
===========================

All JAX-RS methods can be invoked using a web browser with the following root URI:

`http://host:port/com.ibm.cicsdev.restappext/`

## LINK without data

This sample uses the [`LinkCommareaResource`](src/main/java/com/ibm/cicsdev/restappext/LinkCommareaResource.java) JAX-RS resource to invoke a
COBOL program.

* `rest/commarea/empty` - invokes the COBOL program [`PROG1`](src/cobol/PROG1.cbl) with no commarea supplied or returned.

## LINK with a commarea

These samples uses the [`LinkCommareaResource`](src/main/java/com/ibm/cicsdev/restappext/LinkCommareaResource.java) JAX-RS resource to invoke
COBOL programs.

* `rest/commarea/addPart` - invokes the COBOL program [`ADDPART`](src/cobol/ADDPART.cbl), passing and receiving data using the same
structure in the commarea.
* `rest/commarea/getSupplier` - invokes the COBOL program [`GETSUPPL`](src/cobol/GETSUPPL.cbl), passing data in the commarea using one
structure, and receiving data using a different structure.
* `rest/commarea/getPart` - invokes the COBOL program [`GETPART`](src/cobol/GETPART.cbl), passing a small amount of data in the commarea,
and receiving a full record in the response.

## LINK with channels and containers

This sample uses the [`LinkChannelResource`](src/main/java/com/ibm/cicsdev/restappext/LinkChannelResource.java) JAX-RS resource to invoke a
COBOL program.

* `rest/channel/addPart` - invokes the COBOL program [`ADDPARTC`](src/cobol/ADDPARTC.cbl), passing data in a container and receiving data
in the updated container.

## Temporary storage queues

These samples uses the [`TemporaryStorageResource`](src/main/java/com/ibm/cicsdev/restappext/TemporaryStorageResource.java) JAX-RS resource to
manipulate CICS Temporary Storage Queues (TSQs) using the JCICS API.

* `rest/tsq/write` - writes an item to a TSQ.
* `rest/tsq/update` - updates the first item in a TSQ.
* `rest/tsq/delete` - delete a TSQ.


## VSAM KSDS

These samples uses the [`VsamKsdsFileResource`](src/main/java/com/ibm/cicsdev/restappext/VsamKsdsFileResource.java) JAX-RS resource to
manipulate a VSAM Key-Sequenced DataSet (KSDS) using the JCICS API.

* `rest/ksds/write` - writes a new record to the file.
* `rest/ksds/delete` - deletes the first record in the file.
* `rest/ksds/update` - updates the first record in the file.

In all cases, the JSON returned will be the current contents of the file. Pressing refresh in the browser will repeat the GET request,
causing a record to be written, updated, or deleted accordingly.
