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

import com.ibm.cics.server.Channel;
import com.ibm.cics.server.CicsConditionException;
import com.ibm.cics.server.Container;
import com.ibm.cics.server.Program;
import com.ibm.cics.server.Task;
import com.ibm.cicsdev.restappext.bean.StockPartBean;
import com.ibm.cicsdev.restappext.generated.StockPart;
import com.ibm.cicsdev.restappext.helper.StockPartHelper;

/**
 * Provides an example of using the CICS LINK command to send a
 * channel to a CICS program, receive the updated data area, and then
 * return the data as a JAX-RS service in a CICS Liberty environment.
 * 
 * This class is equivalent to the {@link LinkCommareaResource} class,
 * but uses channels and containers, rather than commareas. 
 */
@Path("channel")
@Produces(MediaType.APPLICATION_JSON)
public class LinkChannelResource
{
    /**
     * Provides a simple example where a new StockPart instance is created
     * and added to a VSAM file by another CICS program. 
     * 
     * <p>The method performs the following steps:</p>
     * <ul>
     * <li>Creates an instance of the StockPart class with random data</li>
     * <li>Serializes the object to a byte array</li>
     * <li>Creates a channel and container to store the byte array data</li>
     * <li>Links to the CICS program ADDPART, passing the constructed channel</li>
     * <li>The target program is expected to return an updated container, to demonstrate
     * we can receive data back from the target program</li>
     * </ul>
     * 
     * @return a StockPartBean instance representing the newly-created part. 
     */
    @GET
    @Path("addPart")
    public StockPartBean addPart() {

        // The StockPartBean instance we will return
        StockPartBean bean;
        
        // Create a new instance of our JZOS generated class using our helper class
        StockPart sp = StockPartHelper.generate();
        
        try {
            // Get the created record as a sequence of bytes
            byte[] buf = sp.getByteBuffer();
            
            // Create a channel
            Channel channel = Task.getTask().createChannel("MY-CHANNEL");
            
            // Create container
            Container container = channel.createContainer("STOKPART");
            
            // Initialise with the data
            container.put(buf);
            
            // Create a new instance of the CICS Program class
            Program p = new Program();
            
            // Specify the target program name
            p.setName("ADDPARTC");

            // Perform the link using the channel created above
            p.link(channel);
            
            // Get the updated container
            container = channel.getContainer("STOKPART");
            buf = container.get();
            
            // Build output object from updated container
            StockPart retSP = new StockPart(buf);
            
            // Use this to initialise a JAX-RS bean
            bean = new StockPartBean(retSP);
        }
        catch (CicsConditionException cce) {            
            
            // A CICS failure - build an error message
            String err = String.format("Error linking to program: %s", cce.getMessage());

            // Create a response
            Response r = Response.serverError().entity(err).build();

            // Pass the error back up the handler chain (JAX-RS 1.0)
            throw new WebApplicationException(cce, r);
        }

        // Return the new object converted to a JAX-RS bean 
        return bean;
    }    
}
