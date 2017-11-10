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
 * Provides an example of deploying a POJO into Liberty to allow the
 * use of CICS LINK into a Java EE environment.
 *
 * Link to Liberty is not supported in environments earlier than
 * CICS V5.3 with APAR PI63005.
 *
 * To successfully compile this source file, you will need to upgrade your
 * CICS Explorer or CICS Build Toolkit environment to V5.3.0.8 or later.
 */
public class LinkToTransaction
{
	private static final String CICSPROG = "L2LTRAN";
    
    @CICSProgram(CICSPROG)  // Note annotation argument may be a constant String or literal.
    public void performAction() throws CicsConditionException, UnsupportedEncodingException
    {
    	// First write a record to the TSQ with this program name and Task number. 
        TSQ tsq = new TSQ(); tsq.setName(CICSPROG);  // TSQ has same name as the L2L program.
    	String itemStr = "Written from "+CICSPROG+" by Task "+Task.getTask().getTaskNumber();
        int item = tsq.writeItem(itemStr.getBytes("IBM037"));  // Write converted String to TSQ. 
    	
        // Read container to determine action.  Content already uppercased by caller.
        Channel ch = Task.getTask().getCurrentChannel();    // Channel passed on LINK 
        Container actionCont = ch.getContainer("ACTION");   // Null if no ACTION container
        String action = actionCont.getString().trim();      // NPE if no ACTION container. 
        
        // switch on String variable supported from Java 7 and above.
        switch(action) {  
        case "ABEND":  // Abend task "manually" with specific abend code.
        	Task.getTask().abend("AL2L");
        	break;
        case "ROLLBACK":  // These actions are done by caller after return. NOOP here.
        case "COMMIT":
        	break;  
        case "THROW": throw new NullPointerException();  // Just to see what happens...
        case "CATCH":
        case "PERCOLATE": // Do something which could result in CicsConditionException
        	try {
        		ItemHolder holder = new ItemHolder();
        		tsq.readItem(item + 1, holder);        // ITEMERR highly likely!
        	}
        	catch(CicsConditionException cce) {  // Catch it, handle expected conditions.
        		if (cce instanceof InvalidQueueIdException) {  // QIDERR condition "expected"
        			// Do something sensible, end normally.
        		}
        		// Rethrow "unexpected" conditions, CICS will abend task with AExx code.
        		if (action.equals("PERCOLATE")) throw cce;  
        	}
        	break;
        default: throw new IllegalArgumentException("Invalid action: "+action);
        
        } // switch(action)
    } //performAction()
}
