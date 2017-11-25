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

import java.util.Date;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;
import javax.ws.rs.DELETE;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import com.ibm.cics.server.CicsConditionException;
import com.ibm.cics.server.InvalidQueueIdException;
import com.ibm.cics.server.ItemErrorException;
import com.ibm.cics.server.ItemHolder;
import com.ibm.cics.server.TSQ;
import com.ibm.cics.server.TSQType;
import com.ibm.cicsdev.restappext.bean.StockPartCollection;
import com.ibm.cicsdev.restappext.generated.StockPart;
import com.ibm.cicsdev.restappext.helper.StockPartHelper;


/**
 * Provides a simple, RESTful-style resource to demonstrate parts of the CICS
 * temporary storage queue support using Java.
 */
@Path("tsq")
@Produces(MediaType.APPLICATION_JSON)
public class TemporaryStorageResource
{
    /**
     * Key used in the HTTPSession object to store the name of the TSQ used
     * by a specific user.
     */
    private static final String SESSION_TSQ_NAME_KEY = "TSQ.NAME";

    /**
     * An automatically-injected field that contains request details.
     */
    @Context
    private HttpServletRequest request;

    /**
     * Writes a new record to the queue associated with this HTTP session.
     * 
     * @return A StockPartCollection instance representing the updated TSQ.
     */
    @GET
    @Path("write")
    public StockPartCollection writeNewRecord() {
        
        // Find the TSQ queue for the current HTTP session
        TSQ tsq = getQueue();

        try {
            // Create a random record to write to the queue
            StockPart sp = StockPartHelper.generate();
            
            // Get the created record as a sequence of bytes
            byte[] record = sp.getByteBuffer();
            
            // Call the JCICS API to add this record as an entry to the temporary storage queue
            tsq.writeItem(record);
        }
        catch (CicsConditionException cce) {
            
            // A CICS failure - build an error message
            String err = String.format("Error writing to TSQ: %s", cce.getMessage());

            // Create a response
            Response r = Response.serverError().entity(err).build();

            // Pass the error back up the handler chain (JAX-RS 1.0)
            throw new WebApplicationException(cce, r);
        }
        
        // Query the new contents of the queue and return
        return queryQueue(tsq);        
    }

    
    /**
     * Updates the first item in the TSQ associated with this HTTP session.
     * 
     * @return A StockPartCollection instance representing the updated TSQ.
     */
    @GET
    @Path("update")
    public StockPartCollection updateRecord() {

        // Find the TSQ queue for the current HTTP session
        TSQ tsq = getQueue();

        try {
            // Create a random record to write to the queue
            StockPart sp = StockPartHelper.generate();

            // Get the created record as a sequence of bytes
            byte[] record = sp.getByteBuffer();
            
            // Call the JCICS API to update the first record
            tsq.rewriteItem(1, record);
        }
        catch (InvalidQueueIdException iqe) {
            // This means the queue was not found - acceptable in this case
        }
        catch (CicsConditionException cce) {
            
            // Some other CICS failure - build an error message
            String err = String.format("Error updating record : %s", cce.getMessage());

            // Create a response
            Response r = Response.serverError().entity(err).build();

            // Pass the error back up the handler chain (JAX-RS 1.0)
            throw new WebApplicationException(cce, r);
        }
        
        // Query the new contents of the queue and return
        return queryQueue(tsq);
    }

    
    /**
     * Provides a means of clearing the TSQ for the current HTTP session.
     * 
     * In a true RESTful application, deletion of a resource would map to the DELETE
     * HTTP verb, using the {@link DELETE} annotation.
     * 
     * @return A StockPartCollection instance representing the updated (i.e. empty) TSQ.
     */
    @GET
    @Path("delete")
    public StockPartCollection deleteQueue() {
        
        // Find the required queue
        TSQ tsq = getQueue();

        try {
            // Delete the queue
            tsq.delete();
        }
        catch (InvalidQueueIdException iq) {
            // This means the queue was not found - acceptable in this case
        }
        catch (CicsConditionException cce) {
            
            // Some other CICS failure - build an error message
            String err = String.format("Error deleting queue : %s", cce.getMessage());

            // Create a response
            Response r = Response.serverError().entity(err).build();

            // Pass the error back up the handler chain (JAX-RS 1.0)
            throw new WebApplicationException(cce, r);
        }
        
        // queryQueue will return an empty response
        return queryQueue(tsq);
    }
    
    
    /**
     * Retrieves the TSQ associated with the current HTTP session.
     * 
     * If no queue is associated with the current HTTP session, then a new
     * queue name is created.
     * 
     * @return a TSQ instance associated with the current HTTP session.
     */
    private TSQ getQueue() {

        // Get the current HTTP session, creating a new one if required
        HttpSession session = this.request.getSession(true);

        // Get the existing queue name
        String strQueueName = (String) session.getAttribute(SESSION_TSQ_NAME_KEY);
        
        // Is this empty?
        if ( strQueueName == null ) {
            
            // Yes - generate a new queue name
            strQueueName = generateQueueName();
            
            // Store this new queue name in the session
            session.setAttribute(SESSION_TSQ_NAME_KEY, strQueueName);
            
            // This new session will timeout in 30 minutes
            session.setMaxInactiveInterval(1800);
        }
        
        // Create and configure the CICS queue object
        TSQ tsq = new TSQ();
        tsq.setName(strQueueName);
        tsq.setType(TSQType.MAIN);

        // Return the queue to the caller
        return tsq;
    }
    
    /**
     * Takes the supplied TSQ and returns all of the items in the queue as
     * StockPart instances within a StockPartCollection.
     *  
     * @param tsq The queue to query.
     * 
     * @return A StockPartCollection instance representing the contents of the
     * supplied TSQ.
     */    
    private StockPartCollection queryQueue(TSQ tsq) {
        
        // Result to return
        StockPartCollection coll = new StockPartCollection();
        
        // Configure the result object
        coll.setResourceName(tsq.getName());
        
        // Holder object to receive the data
        ItemHolder holder = new ItemHolder();
        
        try {
            // Loop until we break out - elements in a queue are 1-based
            for ( int i = 1; true; i++ ) {
                
                // Read item from the queue
                tsq.readItem(i, holder);
                
                // Get the record as a sequence of bytes
                byte[] record = holder.getValue();
                
                // Convert this byte array to a new object and add to the result
                coll.add( new StockPart(record) );
            }
        }
        catch (ItemErrorException iee) {
            // Equivalent to a CICS ITEMERROR
            // Normal termination of loop - no further elements            
        }
        catch (InvalidQueueIdException iqe) {
            // Equivalent to a CICSQIDERR 
            // No such queue - return an empty object
        }
        catch (CicsConditionException cce) {
            
            // A CICS failure - build an error message
            String err = String.format("Error querying queue : %s", cce.getMessage());

            // Create a response
            Response r = Response.serverError().entity(err).build();

            // Pass the error back up the handler chain (JAX-RS 1.0)
            throw new WebApplicationException(cce, r);
        }
        
        // Return the constructed object
        return coll;
    }
    
    
    /**
     * Generate a new, unique TS queue name.
     * 
     * @return A 16-character string that is suitable for use as a TS queue name.
     */
    private static synchronized String generateQueueName() {
        
        // Create a new queue name using the current time
        long lNow = new Date().getTime();
        
        // Convert to hex characters
        String strQueueName = Long.toHexString(lNow).toUpperCase();
        
        // Pad with leading zeros and prefix with "QN"
        strQueueName = "000" + strQueueName;
        strQueueName = strQueueName.substring(strQueueName.length() - 14);
        strQueueName = "QN" + strQueueName;
        
        // Return the new queue name
        return strQueueName;
    }        
}
