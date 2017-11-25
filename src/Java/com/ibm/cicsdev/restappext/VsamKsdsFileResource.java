/* Licensed Materials - Property of IBM                                   */
/*                                                                        */
/* SAMPLE                                                                 */
/*                                                                        */
/* (c) Copyright IBM Corp. 2017 All Rights Reserved                       */
/*                                                                        */
/* US Government Users Restricted Rights - Use, duplication or disclosure */
/* restricted by GSA ADP Schedule Contract with IBM Corp                  */
/*                                                                        */

package com.ibm.cicsdev.restappext;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import com.ibm.cics.server.CicsConditionException;
import com.ibm.cics.server.DuplicateRecordException;
import com.ibm.cics.server.EndOfFileException;
import com.ibm.cics.server.KSDS;
import com.ibm.cics.server.KeyHolder;
import com.ibm.cics.server.KeyedFileBrowse;
import com.ibm.cics.server.RecordHolder;
import com.ibm.cics.server.RecordNotFoundException;
import com.ibm.cics.server.SearchType;
import com.ibm.cics.server.Task;
import com.ibm.cicsdev.restappext.bean.StockPartCollection;
import com.ibm.cicsdev.restappext.generated.StockPart;
import com.ibm.cicsdev.restappext.helper.StockPartHelper;

/**
 * Provides a simple, RESTful-style resource to demonstrate parts of the CICS
 * KSDS file support using Java.
 */
@Path("ksds")
@Produces(MediaType.APPLICATION_JSON)
public class VsamKsdsFileResource
{
    /**
     * Example to demonstrate writing a new record to a VSAM KSDS file.
     * 
     * @return A StockPartCollection instance representing the updated VSAM file.
     */
    @GET
    @Path("write")
    public StockPartCollection writeNewRecord() {
        
        // Create a new random record
        StockPart sp = StockPartHelper.generate();
        
        // Get the flat byte structure from the JZOS object
        byte[] record = sp.getByteBuffer();
        
        // Get a byte array containing the record key
        byte[] key = StockPartHelper.getKey(sp);
        
        // Create a reference to the CICS file definition
        KSDS ksds = new KSDS();
        ksds.setName("SMPLXMPL");
       
        try {
            // Write the record into the file at the specified key
            ksds.write(key, record);
                        
            // Only want this write in the current UoW - issue a CICS syncpoint here.
            // This will release any locks we hold.
            Task.getTask().commit();
        }
        catch (DuplicateRecordException dre) {
            
            // Collision on the generated key - log, but don't add anything new
            String strMsg = "Tried to insert duplicate key %d"; 
            System.out.println( String.format(strMsg, sp.getPartId()) );
        }
        catch (CicsConditionException cce) {
            
            // An unexpected CICS error occurred - build an error message
            String err = String.format("Error writing record : %s", cce.getMessage());

            // Create a response
            Response r = Response.serverError().entity(err).build();

            // Pass the error back up the handler chain (JAX-RS 1.0)
            throw new WebApplicationException(cce, r);
        }
        
        // Return a collection to show the contents of the file
        return queryFile(ksds);
    }


    /**
     * Example demonstrating how to delete a record in a VSAM KSDS file.
     * 
     * @return A StockPartCollection instance representing the updated VSAM file.
     */
    @GET
    @Path("delete")
    public StockPartCollection deleteRecord() {

        // Create a reference to the CICS file definition
        KSDS ksds = new KSDS();
        ksds.setName("SMPLXMPL");

        // Holder object to receive the data
        RecordHolder rh = new RecordHolder();

        // Read from the first possible key
        byte[] keyZero = StockPartHelper.getKeyZero();

        try {            
            // Read the first record afer key zero
            ksds.readForUpdate(keyZero, SearchType.GTEQ, rh);
            
            // Delete the record we have just read
            ksds.delete();
            
            // Only want this delete operation in the current UoW - issue a CICS syncpoint here.
            // This will release any locks we hold.
            Task.getTask().commit();
        }
        catch (RecordNotFoundException rnfe) {
            // Initial browse failed - no records in file - ignore this condition
        }
        catch (CicsConditionException cce) {
            
            // Some other CICS failure - build an error message
            String err = String.format("Error deleting record : %s", cce.getMessage());

            // Create a response
            Response r = Response.serverError().entity(err).build();

            // Pass the error back up the handler chain (JAX-RS 1.0)
            throw new WebApplicationException(cce, r);
            
            /*
             * Alternative if JAX-RS 2.0 is available.
             * throw new InternalServerErrorException(cce);
             */
        }
        
        // Return a collection to show the contents of the file
        return queryFile(ksds);
    }


    /**
     * Example showing how to update a record in a VSAM KSDS file.
     *
     * @return A StockPartCollection instance representing the updated VSAM file.
     */
    @GET
    @Path("update")
    public StockPartCollection updateRecord() {
        
        // Create a reference to the CICS file definition
        KSDS ksds = new KSDS();
        ksds.setName("SMPLXMPL");

        // Holder object to receive the data
        RecordHolder rh = new RecordHolder();

        // Read from the first possible key
        byte[] keyZero = StockPartHelper.getKeyZero();

        try {            
            // Read the first record afer key zero
            ksds.readForUpdate(keyZero, SearchType.GTEQ, rh);

            // Create a StockPart instance from the record
            StockPart sp = new StockPart( rh.getValue() );
            
            // Create a new StockPart instance containing random data
            StockPart spRandom = StockPartHelper.generate();
            
            // Copy the key from the read record into the new record
            spRandom.setPartId( sp.getPartId() );
            
            // Rewrite the recently-read record with the new data
            ksds.rewrite( spRandom.getByteBuffer() );
            
            // Only want this delete operation in the current UoW - issue a CICS syncpoint here.
            // This will release any locks we hold.
            Task.getTask().commit();
        }
        catch (RecordNotFoundException rnfe) {
            // Initial read failed - no records in file
        }
        catch (CicsConditionException cce) {
            
            // Some other CICS failure - build an error message
            String err = String.format("Error updating record : %s", cce.getMessage());

            // Create a response
            Response r = Response.serverError().entity(err).build();

            // Pass the error back up the handler chain (JAX-RS 1.0)
            throw new WebApplicationException(cce, r);
        }
        
        // Return a collection to show the contents of the file
        return queryFile(ksds);
    }

    /**
     * Creates a {@link StockPartCollection} instance to represent the contents
     * of the CICS VSAM file.
     * 
     * Note this pattern is for demonstration purposes only: it is not good
     * practice to browse through an entire file.
     * 
     * @param ksds an instance of the JCICS {@link KSDS} class representing
     * the file to be browsed.
     * 
     * @return A StockPartCollection instance representing the contents of
     * the supplied KSDS file. This method assumes the file contains records
     * which match the copybook used to generate the StockPart class. 
     */
    private StockPartCollection queryFile(KSDS ksds) {
        
        // Result to return
        StockPartCollection coll = new StockPartCollection();
        
        // Configure the result object
        coll.setResourceName(ksds.getName());
        
        // Holder object to receive the data
        RecordHolder recordHolder = new RecordHolder();
        KeyHolder keyHolder = new KeyHolder();
        
        // Start a browse of the file at the first possible key
        byte[] keyZero = StockPartHelper.getKeyZero();
        
        try {
            
            // Start the browse of the file
            KeyedFileBrowse kfb = ksds.startBrowse(keyZero, SearchType.GTEQ);
            
            // Loop until we break out
            while ( true ) {
                
                // Read a record from the file
                kfb.next(recordHolder, keyHolder);
                
                // Get the record as a sequence of bytes
                byte[] record = recordHolder.getValue();
                
                // Convert this byte array to a new object and add to the result
                coll.add( new StockPart(record) );
            }
        }
        catch (RecordNotFoundException rnfe) {
            // Initial browse failed - no records in file
        }
        catch (EndOfFileException eof) {
            // Normal termination of loop - no further records            
        }
        catch (CicsConditionException cce) {            
            
            // Some other CICS failure - build an error message
            String err = String.format("Error querying file : %s", cce.getMessage());

            // Create a response
            Response r = Response.serverError().entity(err).build();

            // Pass the error back up the handler chain (JAX-RS 1.0)
            throw new WebApplicationException(cce, r);
        }
        
        // Return the constructed object
        return coll;
    }
}
