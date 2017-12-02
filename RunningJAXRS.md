Running the JAX-RS examples
===========================

All JAX-RS methods can be invoked using a web browser with the following root URI:

`http://host:port/com.ibm.cicsdev.restappext/`

## LINK without data

* `rest/commarea/empty` - invokes the COBOL program [`PROG1`](src/Cobol/PROG1.cbl) with no commarea.

## LINK with a commarea

* `rest/commarea/addPart` - invokes the COBOL program [`ADDPART.cbl`](src/Cobol/ADDPART.cbl).
* `rest/commarea/getSupplier` - invokes the COBOL program [`GETSUPPL.cbl`](src/Cobol/GETSUPPL.cbl).
* `rest/commarea/getPart` - invokes the COBOL program [`GETPART.cbl`](src/Cobol/GETPART.cbl).

## LINK with channels and containers

* Invoke the COBOL program `ADDPARTC` using the `rest/channel/addPart` URI. See the [`ADDPARTC.cbl`](src/Cobol/ADDPARTC.cbl) program which
may be used with this sample.

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
