Running the JAX-RS examples
===========================

All JAX-RS methods can be invoked using a web browser with the following root URI:

`http://host:port/com.ibm.cicsdev.restappext/`

## LINK without data

This sample uses the [`LinkCommareaResource`](src/Java/com/ibm/cicsdev/restappext/LinkCommareaResource.java) JAX-RS resource to invoke a
COBOL program.

* `rest/commarea/empty` - invokes the COBOL program [`PROG1`](src/Cobol/PROG1.cbl) with no commarea supplied or returned.

## LINK with a commarea

These samples uses the [`LinkCommareaResource`](src/Java/com/ibm/cicsdev/restappext/LinkCommareaResource.java) JAX-RS resource to invoke
COBOL programs.

* `rest/commarea/addPart` - invokes the COBOL program [`ADDPART`](src/Cobol/ADDPART.cbl), passing and receiving data using the same
structure in the commarea.
* `rest/commarea/getSupplier` - invokes the COBOL program [`GETSUPPL`](src/Cobol/GETSUPPL.cbl), passing data in the commarea using one
structure, and receiving data using a different structure.
* `rest/commarea/getPart` - invokes the COBOL program [`GETPART`](src/Cobol/GETPART.cbl), passing a small amount of data in the commarea,
and receiving a full record in the response.

## LINK with channels and containers

This sample uses the [`LinkChannelResource`](src/Java/com/ibm/cicsdev/restappext/LinkChannelResource.java) JAX-RS resource to invoke a
COBOL program.

* `rest/channel/addPart` - invokes the COBOL program [`ADDPARTC`](src/Cobol/ADDPARTC.cbl), passing data in a container and receiving data
in the updated container.

## Temporary storage queues

* Write an item to a TSQ using the `rest/tsq/write` URI.
* Update the first item in a TSQ using the `rest/tsq/update` URI.
* Delete a TSQ using the `rest/tsq/delete` URI.


## VSAM KSDS

* Write a new record to the file using the `rest/ksds/write` URI. This invokes the `VsamKsdsFileResource.writeNewRecord()` method.
* Delete the first record in the file using the `rest/ksds/delete` URI. This invokes the `VsamKsdsFileResource.deleteRecord()` method.
* Update the first record in the file using the `rest/ksds/update` URI. This invokes the `VsamKsdsFileResource.updateRecord()` method.

In all cases, the JSON returned will be the current contents of the file. Pressing refresh in the browser will repeat the GET request,
causing a record to be written, updated, or deleted accordingly.
