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

import com.ibm.cics.server.Channel;
import com.ibm.cics.server.CicsConditionException;
import com.ibm.cics.server.Container;
import com.ibm.cics.server.Task;
import com.ibm.cics.server.invocation.CICSProgram;
import com.ibm.cicsdev.restappext.generated.StockPart;
import com.ibm.cicsdev.restappext.generated.Supplier;

/**
 * Provides an example of deploying a POJO into Liberty to allow the
 * use of CICS LINK into a Java EE environment.
 *
 * Link to Liberty is not supported in environments earlier than
 * CICS V5.3 with APAR PI63005.
 *
 * To successfully compile this source file, you will need to upgrade your
 * CICS Explorer or CICS Build Toolkit environment to V5.3.0.8 or later.
 */
public class LinkToLiberty
{
    
    /**
     * Provides an example of using a Java annotation to define a program
     * that can be deployed inside Liberty and LINKed to using the standard
     * CICS mechanism. Data is passed using channels and containers.
     */
    
    @CICSProgram("GETSUPPI")
    public void getSupplierInfo() throws CicsConditionException
    {
        // Retrieve the current channel
        Channel ch = Task.getTask().getCurrentChannel();
        
        // Get the correct container
        Container contStockPart = ch.getContainer("STOKPART");
        
        // Convert to a StockPart instance
        StockPart sp = new StockPart( contStockPart.get() );
        
        // Extract the supplier ID
        int iSupplierId = sp.getSupplier();
        
        // Create a new Supplier instance
        Supplier supplier = new Supplier();
        
        // Initialise using the supplier ID above
        supplier.setSupplierId(iSupplierId);
        
        // Create a new generated supplier name
        String name = "Supplier #" + Task.getTask().getTaskNumber();
        supplier.setSupplierName(name);
        
        // Store in a return container
        Container contSupplier = ch.createContainer("SUPPLIER");
        contSupplier.put( supplier.getByteBuffer() );
    }
}
