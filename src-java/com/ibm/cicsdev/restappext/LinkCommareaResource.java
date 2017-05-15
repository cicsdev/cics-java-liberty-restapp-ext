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
import com.ibm.cics.server.Program;
import com.ibm.cicsdev.restappext.bean.StatusBean;
import com.ibm.cicsdev.restappext.bean.StockPartBean;
import com.ibm.cicsdev.restappext.bean.SupplierBean;
import com.ibm.cicsdev.restappext.generated.StockPart;
import com.ibm.cicsdev.restappext.generated.Supplier;
import com.ibm.cicsdev.restappext.helper.StockPartHelper;

/**
 * Provides various examples of using the CICS LINK command.
 */
@Path("commarea")
@Produces(MediaType.APPLICATION_JSON)
public class LinkCommareaResource
{
    /**
     * Very simple method to invoke the CICS program PROG1.
     * This program takes no input commarea and returns no output commarea. 
     * 
     * @return a single String to confirm success.
     */
    @GET
    @Path("empty")
    public StatusBean linkNoCommarea() {
        
        try {
            // Create a new instance of the CICS Program class
            Program p = new Program();
            
            // Specify the target program name
            p.setName("PROG1");

            // Perform the link with no commarea
            p.link();
        }
        catch (CicsConditionException cce) {            
            
            // A CICS failure - build an error message
            String err = String.format("Error linking to program: %s", cce.getMessage());

            // Create a response
            Response r = Response.serverError().entity(err).build();

            // Pass the error back up the handler chain (JAX-RS 1.0)
            throw new WebApplicationException(cce, r);
        }

        // Rudimentary success message        
        return new StatusBean("Program returned successfully");
    }
    
    /**
     * Provides a simple example where a new StockPart instance is created
     * and added to a VSAM file by another CICS program. 
     * 
     * <p>The method performs the following steps:</p>
     * <ul>
     * <li>Creates an instance of the StockPart class with random data</li>
     * <li>Serializes the object to a byte array</li>
     * <li>Links to the CICS program ADDPART, passing the byte array as a COMMAREA</li>
     * <li>The target program is expected to return an updated COMMAREA, to demonstrate
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
            
            // Create a new instance of the CICS Program class
            Program p = new Program();
            
            // Specify the target program name
            p.setName("ADDPART");

            // Perform the link using the commarea created above
            p.link(buf);
            
            // Build output object from updated commarea
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
    
    /**
     * Simple example where a commarea uses one data format to send data to
     * a target program, and then retrieves response data using a different
     * format.
     *  
     * @return a SupplierBean instance representing the retrieved supplier information.
     */
    @GET
    @Path("getSupplier")
    public SupplierBean retrieveSupplier() {

        // The SupplierBean instance we will return
        SupplierBean bean;
        
        // Create a new instance of our JZOS generated class using our helper class
        StockPart sp = StockPartHelper.generate();
        
        try {
            // Get the created record as a sequence of bytes
            byte[] buf = sp.getByteBuffer();
            
            // Create a new instance of the CICS Program class
            Program p = new Program();
            
            // Specify the target program name
            p.setName("GETSUPPL");

            // Perform the link using the commarea created above
            p.link(buf);
            
            // Build output object from updated commarea
            Supplier supplier = new Supplier(buf);
            
            // Use this to initialise a JAX-RS bean
            bean = new SupplierBean(supplier);
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
    
    
    /**
     * Simple example to show how the JCICS API can be used to provide the
     * same functionality as the DATALENGTH attribute in the EXEC CICS API.
     * 
     * @return a StockPartBean instance containing the information the
     * GETPART program has created for this LINK operation.
     */
    @GET
    @Path("getPart")
    public StockPartBean getPart() {

        // The StockPartBean instance we will return
        StockPartBean bean;
        
        // Create a new instance of our StockPart class
        StockPart sp = new StockPart();
        
        // Specify just the part id
        sp.setPartId(123456);
        
        try {
            // getByteBuffer returns length 80
            byte[] buf = sp.getByteBuffer();
            
            // Create a new instance of the CICS Program class
            Program p = new Program();
            
            // Specify the target program name
            p.setName("GETPART");

            // Perform the link using the commarea created above
            p.link(buf, 8);
            
            // Build output object from updated commarea
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
