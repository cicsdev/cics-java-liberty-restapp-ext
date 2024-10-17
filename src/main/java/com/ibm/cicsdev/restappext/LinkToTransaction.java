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

import java.io.UnsupportedEncodingException;

import com.ibm.cics.server.Channel;
import com.ibm.cics.server.CicsConditionException;
import com.ibm.cics.server.Container;
import com.ibm.cics.server.InvalidQueueIdException;
import com.ibm.cics.server.ItemHolder;
import com.ibm.cics.server.TSQ;
import com.ibm.cics.server.Task;
import com.ibm.cics.server.invocation.CICSProgram;

/**
 * Class containing Link to Liberty method performAction().
 * See LinkToLiberty.java for more information.
 */
public class LinkToTransaction
{
    private static final String CICSPROG = "L2LTRAN";

    /**
     * performAction() - method to demonstrate the transactional behaviour of
     * Link to Liberty in various scenarios such as abends and Exceptions. 
     * This method is linked to by the LINK2TXN COBOL program, passing the ACTION container.
     * 
     * @throws CicsConditionException
     * @throws UnsupportedEncodingException
     */

    // @CICSProgram annotation argument may be a constant String or literal.
    @CICSProgram(CICSPROG)  
    public void performAction() throws CicsConditionException, UnsupportedEncodingException
    {
        // Write an item to a TSQ to demonstrate backout in the case of rollback. 
        TSQ tsq = new TSQ(); 
        
        // TSQ has same name as the L2L program. This will match the TSMODEL in DFHCSD.txt
        tsq.setName(CICSPROG);  
        
        // Form the item string and write it to the TSQ; convert using EBCDIC encoding.
        String itemStr = "Written from " + CICSPROG + " by Task " + Task.getTask().getTaskNumber();
        int item = tsq.writeItem(itemStr.getBytes("IBM037"));  

        // Get the Channel passed on LINK, to access containers.
        Channel ch = Task.getTask().getCurrentChannel();    
        
        // Get a reference to the ACTION container.
        Container actionCont = ch.getContainer("ACTION");   // Null if no ACTION container
        
        // Read ACTION container to determine action.  Content already uppercased by caller.
        String action = actionCont.getString().trim();      // NPE if no ACTION container. 
        
        // switch on a String variable is supported in Java 7 and above.
        switch(action) {
        case "":       
            // No action - NOOP.
            break;
        case "ABEND":  
            // Abend task "manually" with specific abend code.
            Task.getTask().abend("AL2L");
            break;
        case "ROLLBACK":  
            // Not permitted in L2L method - will abend.
            Task.getTask().rollback(); 
            break;  
        case "COMMIT":
            // Not permitted in L2L method - will abend.
            Task.getTask().commit();   
            break;  
        case "THROW": 
            // Throw an explicit NullPointerException to see what happens.
            throw new NullPointerException();  
        case "CATCH":
        case "PERCOLATE": 
            // Do something likely to result in a CicsConditionException
            try {
                ItemHolder holder = new ItemHolder();
                tsq.readItem(item + 1, holder);        // ITEMERR highly likely!
            }
            // Catch CicsConditionException, handle "expected" instances.
            catch(CicsConditionException cce) {
                if (cce instanceof InvalidQueueIdException) {
                    // QIDERR condition is an "expected" condition.
                    // Do something sensible, end normally.  In this case NOOP.
                }
                // If action is PERCOLATE, rethrow "unexpected" conditions. 
                if (action.equals("PERCOLATE")) {
                    throw cce;           // CICS will abend task with AExx code. 
                }
            }
            break;
        default:
            // Unexpected action, throw an IllegalArgumentException.
            throw new IllegalArgumentException("Invalid action: " + action);
        }
    }
}
